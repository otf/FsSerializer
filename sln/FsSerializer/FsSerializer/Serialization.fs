namespace FsSerializer

open System
open System.Collections
open System.Xml.Linq
open System.Xml.Serialization
open System.Linq
open System.Reflection
open Microsoft.FSharp.Reflection

module ListUtil = 
  let (<!>) f m  = List.map f m

module MaybeUtil =
  let maybe n f = 
    function
    | Some x -> f x
    | None -> n

  let (>>?) m f =
    match m with
    | Some x -> f x
    | None -> None

module ReflectionHelper =
  open MaybeUtil
  let optionType = typeof<Option<_>>.GetGenericTypeDefinition ()

  let listType = typeof<List<_>>.GetGenericTypeDefinition ()

  let optionBaseType (typ:Type) = (typ.GetGenericArguments ()).[0]
  
  let listBaseType (typ:Type) = (typ.GetGenericArguments ()).[0]

  let typeNameType (typ:Type) = typ.Name
  let typeName (value:obj) = value.GetType() |> typeNameType

  let propName (prop:PropertyInfo) = prop.Name

  let fieldsOfType (typ:Type) = FSharpType.GetRecordFields typ |> List.ofArray

  let fields (value:obj) = FSharpType.GetRecordFields (value.GetType ()) |> List.ofArray

  let getField (value:obj) (prop:PropertyInfo) : obj = FSharpValue.GetRecordField (value, prop)

  let createRecord (typ:Type) (args:obj list) = FSharpValue.PreComputeRecordConstructor (typ) (List.toArray args)

  let unionCases (typ:Type) = FSharpType.GetUnionCases (typ) |> List.ofArray

  let typeOfUnionCase (case:UnionCaseInfo) = ((case.GetFields ()).Single ()).PropertyType
    
  let createUnion (case:UnionCaseInfo) (arg:obj) = FSharpValue.PreComputeUnionConstructor case [| arg |]

  let castOption typ (opt:obj option) : obj = 
    let someCase = (FSharpType.GetUnionCases (optionType.MakeGenericType ([|typ|]) )).Single (fun x-> x.Name = "Some")
    let newOpt = FSharpValue.PreComputeUnionConstructor (someCase) 
    maybe (None :> obj) (fun x -> newOpt [|x|]) opt

  let castList typ (list:obj list) : obj = 
    let listType = listType.MakeGenericType ([|typ|])
    let emptyCase = (FSharpType.GetUnionCases (listType)).First ()
    let cons = listType.GetMethod ("Cons")
    let mutable result = FSharpValue.PreComputeUnionConstructor emptyCase [||]
    for e in list |> List.rev do
      result <- cons.Invoke (null, [| e ; result |])
    result

module AttributeHelper =
  open MaybeUtil
  open ReflectionHelper

  let private att<'att> (typ:MemberInfo) = 
    let atts = typ.GetCustomAttributes (typeof<'att>, false) 
    if atts.Any () then Some (atts.Single () :?> 'att)
    else None

  let isAtt<'att> (prop:MemberInfo) = 
    let atts = prop.GetCustomAttributes (typeof<'att>, false) 
    atts.Any ()

  let maybeName name = if name <> "" then Some name else None
  
  let isOption (typ:Type) = typ.IsGenericType && (typ.GetGenericTypeDefinition () ) = optionType

  let unsafeGet (value:obj) = 
    let typ = value.GetType ()
    (typ.GetProperty ("Value")).GetValue (value, null)

  let rootNameType (typ:Type) = att<XmlRootAttribute> typ >>? (fun att -> Some att.ElementName) >>? maybeName

  let rootName (value:obj) = value.GetType () |> rootNameType

  let elementName (typ:MemberInfo) = att<XmlElementAttribute> (typ) >>? (fun att -> Some att.ElementName) >>? maybeName

  let attributeName (typ:MemberInfo) = att<XmlAttributeAttribute> (typ) >>? (fun att -> Some att.AttributeName) >>? maybeName

  let arrayName (typ:MemberInfo) = att<XmlArrayAttribute> (typ) >>? (fun att -> Some att.ElementName) >>? maybeName


[<AutoOpen>]
module Serialization =
  open ReflectionHelper
  open AttributeHelper
  open MaybeUtil
  open ListUtil

  let (|PrimitiveType|_|) (typ:Type) = 
    if typ.IsPrimitive then Some PrimitiveType 
    else None

  let (|StringType|_|) (typ:Type) = 
    if typ = typeof<string> then Some StringType
    else None

  let (|XElementType|_|) (typ:Type) = 
    if typ = typeof<XElement> then Some XElementType 
    else None

  let (|RecordType|_|) (typ:Type) = 
    if FSharpType.IsRecord typ then Some RecordType 
    else None

  let (|UnionType|_|) (typ:Type) = 
    if FSharpType.IsUnion typ then Some UnionType 
    else None

  let (|OptionType|_|) (typ:Type) = 
    if typ.IsGenericType && (typ.GetGenericTypeDefinition ()) = optionType then
      Some OptionType 
    else None

  let (|ListType|_|) (typ:Type) = 
    if typ.IsGenericType && (typ.GetGenericTypeDefinition ()) = listType then
      Some ListType 
    else None


  let (|AttributeProperty|_|) (prop:PropertyInfo) =
    if isAtt<XmlAttributeAttribute> prop then Some AttributeProperty
    else None

  let (|ElementProperty|_|) (prop:PropertyInfo) =
    if isAtt<XmlElementAttribute> prop then Some ElementProperty
    else None

  let (|ArrayProperty|_|) (prop:PropertyInfo) =
    if isAtt<XmlArrayAttribute> prop then Some ArrayProperty
    else None

  let private serializePrimitive (value:obj) =
    value.ToString () :> obj

  let getXName attGetter altGetter vallue = maybe (XName.Get <| altGetter vallue) XName.Get (attGetter vallue)

  let rec private serializeRecord (value:obj) =
    let name = getXName rootName typeName value
    let children = (serializeProperty value) <!> fields value
    XElement (name , children) :> obj

  and private serializeProperty (value:obj) (prop:PropertyInfo) : obj list =
    match prop with
    | ElementProperty ->
        let name = getXName elementName propName prop
        let elementValue = getField value prop |> serializeSupportedType
        [XElement (name, elementValue ) :> obj]

    | AttributeProperty -> 
        let name = getXName attributeName propName prop
        match getField value prop |> serializeSupportedType with
        | [x] -> [XAttribute (name, x ) :> obj]
        | [] -> []
        | _ -> failwith "属性値が複数の値を返しました。"

    | ArrayProperty ->
        let name = getXName arrayName propName prop
        let baseType = listBaseType prop.PropertyType
        let list = getField value prop
        let values = list :?> IEnumerable
        seq [ for v in values -> XElement(name, serializeSupportedType v) :> obj ]
        |> List.ofSeq

    | _ -> getField value prop |> serializeSupportedType

  and private serializeUnion (value:obj) =
    let (_, field) = FSharpValue.GetUnionFields (value, value.GetType ())
    serializeSupportedType field.[0]

  and private serializeList (value:obj) =
    let values = value :?> IEnumerable
    seq [ for v in values -> serializeSupportedType v ] |> Seq.concat |> List.ofSeq

  and private serializeSupportedType (value:obj) : obj list =
    if value = null then // Option は objだと null だよねー
      []
    else
      match value.GetType () with
      | PrimitiveType | StringType -> [serializePrimitive value]
      | XElementType -> [value]
      | OptionType -> serializeSupportedType (unsafeGet value)
      | ListType -> serializeList value
      | RecordType -> [serializeRecord value]
      | UnionType -> serializeUnion value
      | _ -> failwith "サポートされていない型が指定されました。"

  let serialize<'a> (value:'a) =  (serializeSupportedType value).Single () :?> XElement

  let deserializePrimitive (typ:Type) (text:string) = (typ.GetMethod ("Parse")).Invoke (null, [|text|])

  let rec deserializeProperty (x:XElement) (prop:PropertyInfo) : obj = 
    let propType = prop.PropertyType
    match (prop, propType) with
    | (ElementProperty, OptionType) ->
        let name = getXName elementName propName prop 
        let baseType = optionBaseType propType
        let element = x.Element (name)
        if element = null then None :> obj
        else deserializeSupportedType baseType element.Value |> Some |> castOption baseType

    | (ElementProperty, _) ->         
        let name = getXName elementName propName prop
        let element = x.Element (name)
        deserializeSupportedType propType element.Value

    | (AttributeProperty, OptionType) ->
        let name = getXName attributeName propName prop
        let baseType = optionBaseType propType
        let att = x.Attribute (name)
        if att = null then None :> obj
        else deserializeSupportedType baseType att.Value |> Some |> castOption baseType

    | (AttributeProperty, _) ->
        let name = getXName attributeName propName prop
        let att = x.Attribute (name)
        deserializeSupportedType propType att.Value

    | (ArrayProperty, _) -> 
        let name = getXName arrayName propName prop
        let baseType = listBaseType propType
        let elements = x.Elements name
        seq [ for e in elements -> deserializeSupportedType baseType e.Value ] 
        |> List.ofSeq |> castList baseType
        
    | (_, XElementType) -> deserializeSupportedType propType x
    | (_, ListType) -> 
      let baseType = listBaseType propType
      let elements = x.Elements () |> List.ofSeq
      (deserializeSupportedType baseType <!> elements) |> (castList baseType)

    | (_, _) -> deserializeSupportedType propType x.Value

  and deserializeRecord (typ:Type) (x:XElement) = 
    let fieldValues = (deserializeProperty x) <!> fieldsOfType typ
    createRecord typ fieldValues

  and deserializeUnion (typ:Type) (x:XElement) =
    let matchCase = unionCases typ |> List.find (fun c -> x.Name = getXName rootNameType typeNameType (typeOfUnionCase c))
    let fieldType = typeOfUnionCase matchCase
    createUnion matchCase (deserializeSupportedType fieldType x)
    
  and deserializeSupportedType (typ:Type) (x:obj) : obj = 
    match typ with
      | RecordType -> deserializeRecord typ (x :?> XElement)
      | UnionType -> deserializeUnion typ (x :?> XElement)
      | PrimitiveType -> deserializePrimitive typ (x :?> string)
      | XElementType -> ((x :?> XElement).Elements ()).Single () :> obj
      | StringType -> x
      | _ -> failwith "サポートされていない型が指定されました。"

  let deserialize<'a> (xml:XElement) = deserializeSupportedType typeof<'a> xml :?> 'a
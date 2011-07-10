namespace FsSerializer

open System
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

module AttributeHelper =
  let private att<'att> (typ:MemberInfo) = 
    let atts = typ.GetCustomAttributes (typeof<'att>, false) 
    let att = atts.Single ()
    att :?> 'att

  let isAtt<'att> (prop:MemberInfo) = 
    let atts = prop.GetCustomAttributes (typeof<'att>, false) 
    atts.Any ()

  let maybeName name = if name <> "" then Some name else None

  let typeName (value:obj) = value.GetType().Name

  let propName (prop:PropertyInfo) = prop.Name

  let fields (value:obj) = FSharpType.GetRecordFields (value.GetType ()) |> List.ofArray

  let getField (value:obj) (prop:PropertyInfo) : obj = FSharpValue.GetRecordField (value, prop)
  
  let isOption (typ:Type) = typ.IsGenericType && (typ.GetGenericTypeDefinition () ).GUID = typeof<Option<_>>.GUID 

  let unsafeGet (value:obj) = 
    let typ = value.GetType ()
    (typ.GetProperty ("Value")).GetValue (value, null)

  let rootName (value:obj) = maybeName (att<XmlRootAttribute> (value.GetType ())).ElementName 

  let elementName (typ:MemberInfo) = maybeName (att<XmlElementAttribute> typ).ElementName

  let attributeName (typ:MemberInfo) = maybeName  (att<XmlAttributeAttribute> typ).AttributeName

  let arrayName (typ:Type) = maybeName (att<XmlArrayAttribute> typ).ElementName

[<AutoOpen>]
module Serialization =
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
    if typ.IsGenericType && (typ.GetGenericTypeDefinition ()).GUID = typeof<Option<_>>.GUID then
      Some OptionType 
    else None
//
//  let (|ListType|_|) (prop:PropertyInfo) = 
//    if prop.PropertyType.IsGenericType && (prop.PropertyType.GetGenericTypeDefinition ()) = typeof<List<_>> then
//      Some ListProperty 
//    else None

//

  let (|AttributeProperty|_|) (prop:PropertyInfo) =
    if isAtt<XmlAttributeAttribute> prop then Some AttributeProperty
    else None

  let (|ElementProperty|_|) (prop:PropertyInfo) =
    if isAtt<XmlElementAttribute> prop then Some ElementProperty
    else None

  let (|ArrayProperty|_|) (prop:PropertyInfo) =
    if isAtt<XmlElementAttribute> prop then Some ArrayProperty
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
        | _ -> failwith ""

    | ArrayProperty -> failwith ""
    | _ -> getField value prop |> serializeSupportedType

  and private serializeSupportedType (value:obj) : obj list =
    if value = null then // Option は objだと null だよねー
      []
    else
      match value.GetType () with
      | PrimitiveType | StringType -> [serializePrimitive value]
      | XElementType -> [value]
      | OptionType -> serializeSupportedType (unsafeGet value)
      | RecordType -> [serializeRecord value]
      | UnionType -> 
        let (_, field) = FSharpValue.GetUnionFields (value, value.GetType ())
        serializeSupportedType field.[0]
      | _ -> failwith "サポートされていない型が指定されました。"

  let serialize<'a> (value:'a) =  (serializeSupportedType value).Single () :?> XElement

  let deserialize<'a> (xml:XElement) = failwith<'a> "undefined"
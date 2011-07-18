namespace FsSerializer

open System
open System.Collections
open System.Xml.Linq
open System.Xml.Serialization
open System.Linq
open System.Reflection
open Microsoft.FSharp.Reflection
open ReflectionHelper
open AttributeHelper
open MaybeUtil
open ListUtil
open XUtil

[<AutoOpen>]
module Serialization =
  let obj2Text (value:obj) = value.ToString () :> obj
  let parseText (typ:Type) (text:string) = (typ.GetMethod ("Parse")).Invoke (null, [|text|])
  let getXName attGetter altGetter vallue = maybe (XName.Get <| altGetter vallue) XName.Get (attGetter vallue)
  let getXName2 attGetter altGetter = maybe (XName.Get <| altGetter) XName.Get (attGetter)

  let rec serializeRecord (value:obj) =
    let name = getXName rootName typeName value
    let children = (serializeProperty value) <!> fields value
    XElement (name , children) :> obj

  and serializeProperty (value:obj) (prop:PropertyInfo) : obj list =
    match prop with
    | ElementProperty name -> serializerElementProperty value prop name
    | AttributeProperty name -> serializerAttributeProperty value prop name
    | ArrayProperty name -> serializerArrayProperty value prop name
    | _ -> serializeUnknowProperty value prop

  and serializerElementProperty (value:obj) (prop:PropertyInfo) (name:string option) =   
    let xname = getXName2 name (propName prop)
    let elementValue = getField value prop |> serializeSupportedType
    [XElement (xname, elementValue ) :> obj]

  and serializerAttributeProperty (value:obj) (prop:PropertyInfo) (name:string option) =  
    let xname = getXName2 name (propName prop)
    match getField value prop |> serializeSupportedType with
    | [x] -> [XAttribute (xname, x ) :> obj]
    | [] -> []
    | _ -> failwith "属性値が複数の値を返しました。"

  and serializerArrayProperty (value:obj) (prop:PropertyInfo) (name:string option) =  
    let xname = getXName2 name (propName prop)
    let baseType = listBaseType prop.PropertyType
    let list = getField value prop
    let values = list :?> IEnumerable
    [ for v in values -> XElement(xname, serializeSupportedType v) :> obj ]

  and serializeUnknowProperty (value:obj) (prop:PropertyInfo) =
    getField value prop |> serializeSupportedType

  and serializeUnion (value:obj) =
    let (_, field) = FSharpValue.GetUnionFields (value, value.GetType ())
    serializeSupportedType field.[0]

  and serializeList (value:obj) =
    let values = value :?> IEnumerable
    [ for v in values do yield! serializeSupportedType v ]

  and serializeSupportedType (value:obj) : obj list =
    if value = null then // Option は objだと null だよねー
      []
    else
      match value.GetType () with
      | PrimitiveType | StringType -> [obj2Text value]
      | XElementType -> [value]
      | OptionType -> serializeSupportedType (unsafeGet value)
      | ListType -> serializeList value
      | RecordType -> [serializeRecord value]
      | UnionType -> serializeUnion value
      | _ -> failwith "サポートされていない型が指定されました。"

  let serialize<'a> (value:'a) =  (serializeSupportedType value).Single () :?> XElement

  let rec deserializeProperty (x:XElement) (prop:PropertyInfo) : obj = 
    match prop with
    | ElementProperty name -> deserializeElementProperty x prop name
    | AttributeProperty name -> deserializeAttributeProperty x prop name
    | ArrayProperty name -> deserializeArrayProperty x prop name
    | _ -> deserializeUnknownProperty x prop
    
  and deserializeFromNode (nodeGetter:XName->'a) (conv: Type->'a->obj) (prop:PropertyInfo) (name:string option)   =
    let propName = propName prop
    let propType= prop.PropertyType
    let xname = getXName2 name propName
    let node = nodeGetter xname

    if isOption propType then
      let baseType = optionBaseType propType
      if node = null then None :> obj
      else conv baseType node |> Some |> castOption baseType
    else
      conv propType node

  and deserializeElementProperty x = deserializeFromNode (element x) xElement2Obj

  and deserializeAttributeProperty x = deserializeFromNode (attribute x) xAttribute2Obj

  and deserializeArrayProperty (x:XElement) (prop:PropertyInfo) (name:string option) =
    let propName = propName prop
    let propType= prop.PropertyType
    let xname = getXName2 name propName
    let baseType = listBaseType propType
    let elements = x.Elements xname
    [ for e in elements -> xElement2Obj baseType e ] |> castList baseType

  and deserializeUnknownProperty (x:XElement) (prop:PropertyInfo) =
    let propName = propName prop
    let propType= prop.PropertyType
    match propType with
        | XElementType ->  xElement2Obj propType x
        | ListType -> 
            let baseType = listBaseType propType
            let elements = x.Elements () |> List.ofSeq
            (xElement2Obj baseType <!> elements) |> (castList baseType)
        | _ -> xElement2Obj propType x

  and xElement2Record typ x = 
    let fieldValues = (deserializeProperty x) <!> fieldsOfType typ
    createRecord typ fieldValues

  and xElement2Union (typ:Type) (x:XElement) =
    let matchCase = unionCases typ |> List.find (fun c -> x.Name = getXName rootNameType typeNameType (typeOfUnionCase c))
    let fieldType = typeOfUnionCase matchCase
    createUnion matchCase (xElement2Obj fieldType x)
    
  and xAttribute2Obj (typ:Type) (x:XAttribute) : obj =
    match typ with
      | PrimitiveType -> parseText typ x.Value
      | StringType -> x.Value :> obj
      | _ -> failwith "デシリアライズできない属性です。"

  and xElement2Obj (typ:Type) (x:XElement) : obj = 
    match typ with
      | RecordType -> xElement2Record typ x
      | UnionType -> xElement2Union typ x
      | PrimitiveType -> parseText typ x.Value
      | XElementType -> (x.Elements ()).Single () :> obj
      | StringType -> x.Value :> obj
      | _ -> failwith "サポートされていない型が指定されました。"

  let deserialize<'a> (xml:XElement) = xElement2Obj typeof<'a> xml :?> 'a
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

  let mplus m1 m2 = 
    match (m1, m2) with
    | (Some x, _) -> Some x
    | (_, Some x) -> Some x
    | (_, _ ) -> None

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

  let props (value:obj) = (value.GetType ()).GetProperties () |> List.ofArray

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

//  let (|DateTimeType|_|) (prop:PropertyInfo) = 
//    if prop.PropertyType = typeof<DateTime> then Some DateTimeProperty 
//    else None
//
//  let (|GuidType|_|) (prop:PropertyInfo) = 
//    if prop.PropertyType = typeof<Guid> then Some GuidProperty 
//    else None
//
//  let (|XElementType|_|) (prop:PropertyInfo) = 
//    if prop.PropertyType = typeof<XElement> then Some XElementProperty 
//    else None
//
  let (|RecordType|_|) (typ:Type) = 
    if FSharpType.IsRecord typ then Some RecordType 
    else None
//
//  let (|UnionType|_|) (prop:PropertyInfo) = 
//    if FSharpType.IsUnion prop.PropertyType then Some UnionProperty 
//    else None
//
//  let (|OptionType|_|) (prop:PropertyInfo) = 
//    if prop.PropertyType.IsGenericType && (prop.PropertyType.GetGenericTypeDefinition ()) = typeof<Option<_>> then
//      Some OptionProperty 
//    else None
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

  let private serializeProperty (value:obj) (prop:PropertyInfo) =
    match prop with
    | ElementProperty -> failwith ""
    | AttributeProperty -> 
        let name = getXName attributeName propName prop
        let attValue = prop.GetValue (value, null) |> serializePrimitive
        XAttribute (name, attValue )
    | ArrayProperty -> failwith ""
    | _ -> failwith ""

  let private serializeRecord (value:obj) =
    let name = getXName rootName typeName value
    let children = (serializeProperty value) <!> props value
    XElement (name , children) :> obj

  let private serializeSupportedType (value:obj) : obj =
    match value.GetType () with
    | PrimitiveType | StringType -> serializePrimitive value
    | RecordType -> serializeRecord value
    | _ -> failwith "サポートされていない型が指定されました。"

  let serialize<'a> (value:'a) =  (serializeSupportedType value) :?> XElement

  let deserialize<'a> (xml:XElement) = failwith<'a> "undefined"
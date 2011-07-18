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

  let ifbe b f = if b then Some f else None

  let (>>?) m f =
    match m with
    | Some x -> f x
    | None -> None

  let (<?>) (m1:option<_>) (m2:option<_>) = if m1.IsSome then m1 else m2

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
  
  let isOption (typ:Type) = typ.IsGenericType && (typ.GetGenericTypeDefinition () ) = optionType
  
  let isList (typ:Type) = typ.IsGenericType && (typ.GetGenericTypeDefinition () ) = listType

  let (|PrimitiveType|_|) (typ:Type) = ifbe typ.IsPrimitive PrimitiveType

  let (|StringType|_|) (typ:Type) = ifbe (typ = typeof<string>) StringType

  let (|XElementType|_|) (typ:Type) = ifbe (typ = typeof<XElement>) XElementType

  let (|RecordType|_|) (typ:Type) = ifbe (FSharpType.IsRecord typ) RecordType

  let (|UnionType|_|) (typ:Type) = ifbe (FSharpType.IsUnion typ) UnionType

  let (|OptionType|_|) (typ:Type) = ifbe (isOption typ) OptionType

  let (|ListType|_|) (typ:Type) = ifbe (isList typ) ListType

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

  let unsafeGet (value:obj) = 
    let typ = value.GetType ()
    (typ.GetProperty ("Value")).GetValue (value, null)

  let rootNameType (typ:Type) = att<XmlRootAttribute> typ >>? (fun att -> Some att.ElementName) >>? maybeName

  let rootName (value:obj) = value.GetType () |> rootNameType

  let elementName (typ:MemberInfo) = att<XmlElementAttribute> (typ) >>? (fun att -> Some att.ElementName) >>? maybeName

  let attributeName (typ:MemberInfo) = att<XmlAttributeAttribute> (typ) >>? (fun att -> Some att.AttributeName) >>? maybeName

  let arrayName (typ:MemberInfo) = att<XmlArrayAttribute> (typ) >>? (fun att -> Some att.ElementName) >>? maybeName

  let (|AttributeProperty|_|) (prop:PropertyInfo) = ifbe (isAtt<XmlAttributeAttribute> prop) (attributeName prop)

  let (|ElementProperty|_|) (prop:PropertyInfo) = ifbe (isAtt<XmlElementAttribute> prop) (elementName prop)

  let (|ArrayProperty|_|) (prop:PropertyInfo) = ifbe (isAtt<XmlArrayAttribute> prop) (arrayName prop)

module XUtil =
  let element (x:XElement) = x.Element
  let attribute (x:XElement) = x.Attribute
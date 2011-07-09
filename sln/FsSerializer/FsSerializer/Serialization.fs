namespace FsSerializer

open System.Xml.Linq

module Serialization =

  let serialize<'a> (record:'a) = failwith<XElement> "undefined"

  let deserialize<'a> (xml:XElement) = failwith<'a> "undefined"
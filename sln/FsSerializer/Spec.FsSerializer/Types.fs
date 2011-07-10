module Types

open System
open System.Xml.Linq
open System.Xml.Serialization

// tuple は無理ぽ

[<XmlRoot>]
type FullName = { 
  [<XmlAttribute>] FirstName : string
  [<XmlAttribute>] LastName : string
  [<XmlAttribute>] MiddleName : string option
  }

[<XmlRoot>]
type Employee = {
  [<XmlAttribute>] CorporateName : string
  Profile : XElement
  }

[<XmlRoot>]
type Student = {
  [<XmlElement("SN")>] SchoolName : string
}

[<XmlRoot>]
type Student2 = {
  SchoolName : string
}

type StudentUnion = 
| StudentOne of Student
| StudentTwo of Student2

type Attribute = 
| EmyloyeeAttribute of Employee 
| StudentAttribute of StudentUnion
| OtherAttribute of string

type Person = {
  FullName : FullName
  TwitterAccounts : string list
  Attributes : Attribute list
  Age : int
  Profile : XElement option
  Website : string option
  GirlFriend : Person option
}
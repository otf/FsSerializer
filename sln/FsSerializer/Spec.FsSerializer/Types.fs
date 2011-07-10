module Types

open System
open System.Xml.Linq
open System.Xml.Serialization

// tuple は無理ぽ

[<XmlRoot>]
type FullName = { 
  [<XmlAttribute>] FirstName : string
  [<XmlAttribute>] LastName : string
  }

type Employee = {
  EmployeeId : Guid
  CorporateName : string
  HireDate : DateTime
  Prifile : XElement
  }

type Student = {
  SchoolName : string
}

type Attribute = 
| EmyloyeeAttribute of Employee 
| StudentAttribute of Student
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
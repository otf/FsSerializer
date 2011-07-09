module Types

open System
open System.Xml.Linq

// tuple は無理ぽ

type FullName = { 
  FirstName : string
  LastName : string
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
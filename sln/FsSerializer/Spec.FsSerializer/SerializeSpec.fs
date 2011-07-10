module SerializeSpec

open System
open NaturalSpec
open System.Xml.Linq
open FsSerializer
open Types

[<Scenario>]
let ``NaturalSpecちゃんと動くよね？`` () = 
  Given 100
  |> It should equal 100
  |> Verify 

let xmlEqual (left:XElement) right = XElement.DeepEquals (left, right)
let xml = XElement.Parse

[<Scenario>]
let ``プリミティブ型のみから構成されるレコードのシリアライズ`` () =
  let fullname = { FirstName = "Yusuke" ; LastName = "Sato"; MiddleName=None } : FullName
  let expected = xml "<FullName FirstName=\"Yusuke\" LastName=\"Sato\" />"
  Given fullname
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

  Given serialize fullname
  |> When deserialize<FullName>
  |> Verify

[<Scenario>]
let ``Someプリミティブ型のみから構成されるレコードのシリアライズ`` () =
  let fullname = { FirstName = "Yusuke" ; LastName = "Sato"; MiddleName=(Some "W") } : FullName
  let expected = xml "<FullName FirstName=\"Yusuke\" LastName=\"Sato\" MiddleName=\"W\" />"

  Given fullname
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

  Given serialize fullname
  |> When deserialize<FullName>
  |> Verify

[<Scenario>]
let ``XML要素を含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student
  let expected = xml "<Student><SN>Nagoya University</SN></Student>"

  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

  Given serialize student
  |> When deserialize<Student>
  |> Verify

[<Scenario>]
let ``XMLコンテントを含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student2
  let expected = xml "<Student2>Nagoya University</Student2>"

  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

[<Scenario>]
let ``XElementを含んだレコードのシリアライズ`` () =
  let employee = { 
      CorporateName="Sato Shoji"
      Profile = xml "<xml>im otf</xml>" } : Employee

  let expected = xml "<Employee CorporateName=\"Sato Shoji\"><xml>im otf</xml></Employee>"

  Given employee
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify


[<Scenario>]
let ``Unionを含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student2

  let expected = xml "<Student2>Nagoya University</Student2>"

  Given (StudentTwo student)
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

[<Scenario>]
let ``Lisdt含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student2
  let student2 = { SchoolName = "Tokyo University"  } : Student2
  let studentList = { Students = [ student ; student2 ] } : StudentList
  let expected = xml "<StudentList><Student2>Nagoya University</Student2><Student2>Tokyo University</Student2></StudentList>"

  Given studentList
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify
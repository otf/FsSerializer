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
  |> When deserialize<FullName>
  |> It should equal fullname
  |> Verify

[<Scenario>]
let ``Someプリミティブ型のみから構成されるレコードのシリアライズ`` () =
  let fullname = { FirstName = "Yusuke" ; LastName = "Sato"; MiddleName=(Some "W") } : FullName
  let expected = xml "<FullName FirstName=\"Yusuke\" LastName=\"Sato\" MiddleName=\"W\" />"

  Given fullname
  |> When serialize
  |> It should have (xmlEqual expected)
  |> When deserialize<FullName>
  |> It should equal fullname
  |> Verify

[<Scenario>]
let ``XML要素を含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student
  let expected = xml "<Student><SN>Nagoya University</SN></Student>"

  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> When deserialize<Student>
  |> It should equal student
  |> Verify

[<Scenario>]
let ``XMLコンテントを含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student2
  let expected = xml "<Student2>Nagoya University</Student2>"

  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> When deserialize<Student2>
  |> It should equal student
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
  |> When deserialize<Employee>
  |> It should equal employee
  |> Verify


[<Scenario>]
let ``Unionを含んだレコードのシリアライズ`` () =
  let student = StudentTwo ( { SchoolName = "Nagoya University"  } : Student2)

  let expected = xml "<Student2>Nagoya University</Student2>"

  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> When deserialize<StudentUnion>
  |> It should equal student
  |> Verify


[<Scenario>]
let ``List含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student2
  let student2 = { SchoolName = "Tokyo University"  } : Student2
  let studentList = { Students = [ student ; student2 ] } : StudentList
  let expected = xml "<StudentList><Student2>Nagoya University</Student2><Student2>Tokyo University</Student2></StudentList>"

  Given studentList
  |> When serialize
  |> It should have (xmlEqual expected)
  |> When deserialize<StudentList>
  |> It should equal studentList
  |> Verify

[<Scenario>]
let ``StringList含んだレコードのシリアライズ`` () =
  let addressList = { Address = [ "000-000-000" ; "111-111-111" ] } : AddressList
  let expected = xml "<AddressList><Address>000-000-000</Address><Address>111-111-111</Address></AddressList>"

  Given addressList
  |> When serialize
  |> It should have (xmlEqual expected)
  |> When deserialize<AddressList>
  |> It should equal addressList
  |> Verify

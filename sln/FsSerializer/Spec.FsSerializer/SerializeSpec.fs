module SerializeSpec

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

[<Scenario>]
let ``Someプリミティブ型のみから構成されるレコードのシリアライズ`` () =
  let fullname = { FirstName = "Yusuke" ; LastName = "Sato"; MiddleName=(Some "W") } : FullName
  let expected = xml "<FullName FirstName=\"Yusuke\" LastName=\"Sato\" MiddleName=\"W\" />"
  Given fullname
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

[<Scenario>]
let ``XML要素を含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student
  let expected = xml "<Student><SN>Nagoya University</SN></Student>"
  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify

[<Scenario>]
let ``XMLコンテントを含んだレコードのシリアライズ`` () =
  let student = { SchoolName = "Nagoya University"  } : Student2
  let expected = xml "<Student2>Nagoya University</Student2>"
  Given student
  |> When serialize
  |> It should have (xmlEqual expected)
  |> Verify
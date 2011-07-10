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


[<Scenario>]
let ``プリミティブ型のみから構成されるレコードのシリアライズ`` () =
  let fullname = { FirstName = "Yusuke" ; LastName = "Sato" } : FullName
  Given fullname
  |> When (serialize >> string)
  |> It should equal "<FullName FirstName=\"Yusuke\" LastName=\"Sato\" />"
  |> Verify
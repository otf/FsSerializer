module SerializeSpec

open NaturalSpec

[<Scenario>]
let SchenarioTest () = 
  Given 100
  |> It should equal 100
  |> Verify 
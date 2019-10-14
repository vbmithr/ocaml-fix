open Fixtypes

let date () =
  ignore (Date.print (0, 0, 0))

let time () =
  ignore (TZTimeOnly.print ((0, 0, 0), 0))

let types = [
  "date", `Quick, date ;
  "time", `Quick, time ;
]

let () =
  Alcotest.run "fix" [
    "types", types ;
  ]


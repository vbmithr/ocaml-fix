open Fixtypes

let date () =
  ignore (Date.to_string (0, 0, 0))

let time () =
  ignore (TZTimeOnly.to_string ((0, 0, 0), 0))

let types = [
  "date", `Quick, date ;
  "time", `Quick, time ;
]

let () =
  Alcotest.run "fix" [
    "types", types ;
  ]


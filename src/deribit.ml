open Sexplib.Std
open Fix
open Fixtypes
open Field

type _ typ += CancelOnDisconnect : bool typ
module CancelOnDisconnect = Make(struct
    type t = bool [@@deriving sexp]
    let t = CancelOnDisconnect
    let pp = YesOrNo.pp
    let parse = YesOrNo.parse
    let tag = 9001
    let name = "CancelOnDisconnect"
  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | CancelOnDisconnect, CancelOnDisconnect -> Some Eq
    | _ -> None
end)
let () = register_field (module CancelOnDisconnect)

let sid = "ocaml-fix"
let tid = "DERIBITSERVER"

let logon_fields
    ?(cancel_on_disconnect=true)
    ~username
    ~secret
    ~ts =
  let b64rand =
    B64.encode (Monocypher.Rand.gen 32 |> Bigstring.to_string) in
  let rawdata =
    Format.asprintf "%.0f.%s" (Ptime.to_float_s ts *. 1e3) b64rand in
  let password =
    B64.encode Digestif.SHA256.(digest_string (rawdata ^ secret) |> to_raw_string) in
  [ RawData.create rawdata ;
    Username.create username ;
    Password.create password ;
    CancelOnDisconnect.create cancel_on_disconnect ;
  ]

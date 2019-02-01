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

let senderCompID = "ocaml-fix"
let targetCompID = "DERIBITSERVER"

let base_fields =
  [ SenderCompID.create senderCompID ;
    TargetCompID.create targetCompID ]

let logon
    ?(cancel_on_disconnect=true)
    ?(heartbeat=Ptime.Span.of_int_s 30)
    ~username
    ~secret
    ~ts
    seqnum =
  let b64rand =
    B64.encode (Monocypher.Rand.gen 32 |> Bigstring.to_string) in
  let rawdata =
    Format.asprintf "%.0f.%s" (Ptime.to_float_s ts *. 1e3) b64rand in
  let password =
    B64.encode Digestif.SHA256.(digest_string (rawdata ^ secret) |> to_raw_string) in
  let heartbeat_s =
    match Ptime.Span.to_int_s heartbeat with
    | None -> invalid_arg "logon: invalid heartbeat"
    | Some hb -> hb in
  let fields =
    MsgSeqNum.create seqnum ::
    RawData.create rawdata ::
    HeartBtInt.create heartbeat_s ::
    Username.create username ::
    Password.create password ::
    CancelOnDisconnect.create cancel_on_disconnect ::
    base_fields in
  Fix.create ~typ:Fixtypes.MsgType.Logon ~fields

let logout seqnum =
  let fields =
    MsgSeqNum.create seqnum ::
    base_fields in
  Fix.create ~typ:Fixtypes.MsgType.Logout ~fields

let heartbeat testReqID seqnum =
  Fix.heartbeat ~senderCompID ~targetCompID ~testReqID seqnum

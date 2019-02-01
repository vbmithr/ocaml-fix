open Core
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
    ?(heartbeat=Time_ns.Span.of_int_sec 30)
    ~secret
    () =
  let ts =
    let open Time_ns in
    now () |>
    to_int63_ns_since_epoch |>
    fun v -> Int63.(v / of_int 1_000_000) in
  let b64rand =
    B64.encode (Monocypher.Rand.gen 32 |> Bigstring.to_string) in
  let rawdata =
    Format.asprintf "%a.%s" Int63.pp ts b64rand in
  let password =
    B64.encode Digestif.SHA256.(digest_string (rawdata ^ secret) |> to_raw_string) in
  let fields =
    MsgSeqNum.create 1 ::
    RawData.create rawdata ::
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

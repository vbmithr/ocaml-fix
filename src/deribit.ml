open Core
open Fix
open Fixtypes
open Field

type _ typ += CancelOnDisconnect : bool typ

module CancelOnDisconnect = struct
  module T = struct
    type t = bool [@@deriving sexp]
    let pp = YesOrNo.pp
    let tag = 9001
    let name = "CancelOnDisconnect"
  end

  include T

  let create v = create CancelOnDisconnect (module T) v

  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | CancelOnDisconnect, CancelOnDisconnect -> Some Eq
    | _ -> None

  let find :
    type a. a typ -> field -> a option = fun typ (Field.F (typ', _, v)) ->
    match eq typ typ' with
    | None -> None
    | Some Eq -> Some v

  let parse tag' v =
    if tag = tag' then
      Some (F (CancelOnDisconnect, (module T), YesOrNo.parse_exn v))
    else None
end

let () = register_field (module CancelOnDisconnect)

let base_fields =
  [ SenderCompID.create "ocaml-fix" ;
    TargetCompID.create "DERIBITSERVER" ]

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
    base_fields in
  Fix.create ~typ:Fixtypes.MsgType.Logon ~fields

open Core
open Fix

let base_fields =
  let open Field in
  [ sendercompid "ocaml-fix" ; targetcompid "DERIBITSERVER" ]

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
    Field.msgseqnum 1 ::
    Field.rawdata rawdata ::
    Field.password password ::
    base_fields in
  create ~typ:MsgType.Logon ~fields

open Fix
open Field

module CancelOnDisconnect = struct
  module T = struct
    type t =
      | All
      | Session [@@deriving sexp]

    let parse = function
      | "Y" -> Some All
      | "S" -> Some Session
      | _ -> None

    let print = function
      | All -> "Y"
      | Session -> "S"
  end
  include T
  include Fixtypes.Make(T)
end

type _ typ += CancelOnDisconnect : CancelOnDisconnect.t typ
module CancelOrdersOnDisconnect = Make(struct
    type t = CancelOnDisconnect.t [@@deriving sexp]
    let t = CancelOnDisconnect
    let pp = CancelOnDisconnect.pp
    let parse = CancelOnDisconnect.parse
    let tag = 8013
    let name = "CancelOnDisconnect"
  let eq :
    type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
    match a, b with
    | CancelOnDisconnect, CancelOnDisconnect -> Some Eq
    | _ -> None
end)
let () = register_field (module CancelOrdersOnDisconnect)

let tid = "Coinbase"
let url = Uri.make ~scheme:"https"
    ~host:"fix.pro.coinbase.com" ~port:4198 ()
let sandbox_url = Uri.make ~scheme:"https"
    ~host:"fix-public.sandbox.pro.coinbase.com" ~port:4198 ()

let logon_fields
    ?cancel_on_disconnect
    ~key
    ~secret
    ~passphrase
    ~logon_ts =
  let prehash = String.concat "\x01" [
    Format.asprintf "%a" Fixtypes.UTCTimestamp.pp logon_ts ;
    Format.asprintf "%a" MsgType.pp Logon ;
    "1" ; key ; tid ; passphrase ;
  ] in
  let rawdata =
    Base64.encode_exn
      Digestif.SHA256.(hmac_string ~key:secret prehash |> to_raw_string) in
  List.rev_append [
    EncryptMethod.create Other ;
    RawData.create rawdata ;
    Password.create passphrase ;
  ]
    begin
      match cancel_on_disconnect with
      | None -> []
      | Some `All -> [CancelOrdersOnDisconnect.create All] ;
      | Some `Session -> [CancelOrdersOnDisconnect.create Session] ;
    end

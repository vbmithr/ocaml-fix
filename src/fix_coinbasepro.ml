open Rresult
open Sexplib.Std
open Fix
open Fixtypes
open Field

module SelfTradePrevention = struct
  module T = struct
    type t =
      | DecrementAndCancel
      | CancelRestingOrder
      | CancelIncomingOrder
      | CancelBothOrders
    [@@deriving sexp,yojson,show]

    let parse = function
      | "D" -> Ok DecrementAndCancel
      | "O" -> Ok CancelRestingOrder
      | "N" -> Ok CancelIncomingOrder
      | "B" -> Ok CancelBothOrders
      | _ -> R.error_msg "invalid argument"

    let to_string = function
      | DecrementAndCancel -> "D"
      | CancelRestingOrder -> "O"
      | CancelIncomingOrder -> "N"
      | CancelBothOrders -> "B"
  end
  include T
  include Fixtypes.Make(T)
end

type _ typ += SelfTradePrevention : SelfTradePrevention.t typ
module STP = Make(struct
    type t = SelfTradePrevention.t [@@deriving sexp,yojson]
    let t = SelfTradePrevention
    let pp = SelfTradePrevention.pp_fix
    let parse = SelfTradePrevention.parse
    let tag = 7928
    let name = "SelfTradePrevention"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | SelfTradePrevention, SelfTradePrevention -> Some Eq
      | _ -> None
  end)
let () = register_field (module STP)

type _ typ += BatchID : string typ
module BatchID = Make(struct
    type t = string [@@deriving sexp,yojson]
    let t = BatchID
    let pp = Format.pp_print_string
    let parse s = Ok s
    let tag = 8014
    let name = "BatchID"
    let eq :
      type a b. a typ -> b typ -> (a, b) eq option = fun a b ->
      match a, b with
      | BatchID, BatchID -> Some Eq
      | _ -> None
  end)
let () = register_field (module BatchID)

let tid = "Coinbase"
let version = Version.v42
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

let testreq ~testreqid =
  let fields = [Field.TestReqID.create testreqid] in
  Fix.create ~fields Fixtypes.MsgType.TestRequest

let order_status_request orderID =
  let fields = [
    Field.OrderID.create (Uuidm.to_string orderID) ;
  ] in
  Fix.create ~fields Fixtypes.MsgType.OrderStatusRequest

let supported_timeInForces = [
  Fixtypes.TimeInForce.GoodTillCancel ;
  ImmediateOrCancel ;
  FillOrKill ;
  PostOnly ;
]

let supported_ordTypes = [
  Fixtypes.OrdType.Market ;
  Limit ;
  StopLimit ;
]


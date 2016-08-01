open Fix_intf
open Fix
open Nocrypto

open Bs_devkit.Core

let make_msg = ref @@ msg_maker ~targetcompid:"Coinbase" ~sendercompid:"" ()
let init apikey = make_msg := msg_maker ~targetcompid:"Coinbase" ~sendercompid:apikey ()

let logon ?(heartbeat=30) ~secret ~passphrase () =
  let fields = [
    Fix.Tag.S EncryptMethod, "0";
    S HeartBtInt, string_of_int heartbeat;
    S Password, passphrase;
  ]
  in
  let seqnum, msg = !make_msg Logon fields in
  (* Now adding signature *)
  (* See https://docs.exchange.coinbase.com/?javascript#logon *)
  let prehash_tags = Tag.[
      S SendingTime;
      S MsgType;
      S MsgSeqNum;
      S SenderCompID;
      S TargetCompID;
      S Password
    ]
  in
  let fields_with_msgtype = Tag.Map.add (Tag.S MsgType) (MsgType.to_string msg.typ) msg.fields in
  let prehash_tags = List.map (fun tag -> Tag.Map.find tag fields_with_msgtype) prehash_tags in
  let prehash_str = String.concat "\001" prehash_tags in
  match Base64.decode Cstruct.(of_string secret) with
  | None -> invalid_arg "Base64.decode"
  | Some secret_decoded ->
  let signature = Hash.SHA256.hmac ~key:secret_decoded Cstruct.(of_string prehash_str) |> Base64.encode in
  let msg = add_field msg (S RawData) Cstruct.(to_string signature) in
  seqnum, msg

let logout () = !make_msg Logout []
let heartbeat ?testreqid () = !make_msg Heartbeat (match testreqid with None -> [] | Some id -> [S TestReqID, id])
let testreq id = !make_msg TestRequest [S TestReqID, id]

let coinbase_of_symbol = function
  | "XBTUSD" -> "BTC-USD"
  | "XBTEUR" -> "BTC-EUR"
  | _ -> invalid_arg "coinbase_of_symbol"

let new_order ?(order_type="2") ?(tif="1") ~uuid ~symbol ~side ~p ~v () =
  let side = match side with BuyOrSell.Buy -> "1" | Sell -> "2" in
  !make_msg NewOrderSingle [
    S HandlInst, "1";
    S ClOrdID, uuid;
    S Symbol, coinbase_of_symbol symbol;
    S Side, side;
    S Price, Printf.sprintf "%f" p;
    S OrderQty, Printf.sprintf "%f" v;
    S OrdType, order_type;
    S TimeInForce, tif;
  ]

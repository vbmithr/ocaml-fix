open Fix_intf
open Fix
open Nocrypto

let make_msg = ref @@
  msg_maker ~minor:2 ~targetcompid:"Coinbase" ~sendercompid:"" ()

let init apikey =
  make_msg :=
    msg_maker ~minor:2 ~targetcompid:"Coinbase" ~sendercompid:apikey ()

let logon ?(heartbeat=30) ~apisecret ~passphrase () =
  let fields =
    [
     98, "0"; (* encryption *)
     108, string_of_int heartbeat;
     554, passphrase;
    ] in
  let seqnum, msg = !make_msg (string_of_msgname Logon) fields in
  (* Now adding signature *)
  (* See https://docs.exchange.coinbase.com/?javascript#logon *)
  let prehash_tags = [SendingTime; MsgType; MsgSeqNum;
                      SenderCompId; TargetCompId; Password] in
  let prehash_tags =
    List.map (fun tag ->
        match Msg.find msg (tag_to_enum tag) with
        | None -> invalid_arg "find_field"
        | Some field -> field
      ) prehash_tags in
  let prehash_str = String.concat "\001" prehash_tags in
  let secret_decoded = Base64.decode Cstruct.(of_string apisecret) in
  let signature = Hash.SHA256.hmac ~key:secret_decoded
      Cstruct.(of_string prehash_str) |> Base64.encode in
  let msg = add_field msg 96 Cstruct.(to_string signature) in
  seqnum, msg

let logout () = !make_msg (string_of_msgname Logout) []

let heartbeat ?testreqid () =
  !make_msg (string_of_msgname Heartbeat)
    (match testreqid with None -> [] | Some id -> [112, id])

let testreq id =
  !make_msg (string_of_msgname TestRequest) [112, id]

let new_order ?(order_type="2") ?(tif="1")
    ~uuid ~symbol ~direction ~p ~v () =
  let string_of_symbol = function
    | `XBTUSD -> "BTC-USD"
    | `XBTEUR -> "BTC-EUR"
    | _ -> invalid_arg "string_of_symbol"
  in
  let string_of_direction = function `Buy -> "1" | `Sell -> "2" in
  !make_msg (string_of_msgname NewOrderSingle)
    [
      21, "1";
      11, uuid;
      55, string_of_symbol symbol;
      54, string_of_direction direction;
      44, Printf.sprintf "%f" p;
      38, Printf.sprintf "%f" v;
      40, order_type;
      59, tif;
    ]

open Fix_intf
open Fix
open Nocrypto

let make_msg = msg_maker ~minor:2 ~targetcompid:"Coinbase"

let logon ?(heartbeat=30) ~apikey ~apisecret () =
  let make_msg = make_msg ~sendercompid:apikey () in
  let fields =
    [
     98, "0"; (* encryption *)
     108, string_of_int heartbeat;
     554, apisecret;
    ] in
  let seqnum, msg = make_msg "A" fields in
  (* Now adding signature *)
  (* See https://docs.exchange.coinbase.com/?javascript#logon *)
  let prehash_tags = [SendingTime; MsgType; MsgSeqNum;
                      SenderCompId; TargetCompId; Password] in
  let prehash_tags = List.map (fun tag -> IntMap.find (tag_to_enum tag) msg
                              ) prehash_tags in
  let prehash_str = String.concat "\001" prehash_tags in
  let secret_decoded = Base64.decode Cstruct.(of_string apisecret) in
  let signature = Hash.SHA256.hmac ~key:secret_decoded
      Cstruct.(of_string prehash_str) in
  let signature = Base64.encode signature in
  let msg = IntMap.add 96 Cstruct.(to_string signature) msg in
  seqnum, msg

let logout apikey =
  let make_msg = make_msg ~sendercompid:apikey () in
  make_msg "5" []

let heartbeat ?testreqid apikey =
  let make_msg = make_msg ~sendercompid:apikey () in
  make_msg "5" (match testreqid with None -> [] | Some id -> [112, id])

let testreq ~testreqid apikey =
  let make_msg = make_msg ~sendercompid:apikey () in
  make_msg "1" [112, testreqid]

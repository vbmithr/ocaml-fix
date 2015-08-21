open Fix_intf
open Fix

let make_msg =
  msg_maker
    ~sendercompid:"CLIENT1"
    ~targetcompid:"EXECUTOR" ()

let logon ?(heartbeat=30) () =
  let fields =
    [
     98, "0"; (* encryption *)
     108, string_of_int heartbeat;
     (* 553, username; *)
     (* 554, passwd; *)
    ] in
  make_msg (string_of_msgname Logon) fields

let logout ?(response=false) () =
  make_msg (string_of_msgname Logout)
    (if response then [8500, "0"] else [])

let heartbeat ?testreqid () =
  let fields =
    (match testreqid with
     | None -> []
     | Some value -> [112, value]) in
  make_msg (string_of_msgname Heartbeat) fields

let testreq reqid = make_msg "1" [112, reqid]

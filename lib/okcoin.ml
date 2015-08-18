open Fix

let make_msg = msg_maker
    ~sendercompid:"de5bbcf9-a6fc-4c32-b569-7d4d1d619525"
    ~targetcompid:"OKSERVER" ()

let logon ?(heartbeat=30) ~username ~passwd () =
  let fields =
    [
     create_field ~tag:98 ~value:"0" (); (* encryption *)
     create_field ~tag:108 ~value:(string_of_int heartbeat) ();
     create_field ~tag:553 ~value:username ();
     create_field ~tag:554 ~value:passwd ();
    ] in
  make_msg "A" fields

let logout () =
  make_msg "5" [create_field ~tag:8500 ~value:"0" ()]

let heartbeat ?testreqid ~username ~passwd () =
  let fields =
    create_field ~tag:553 ~value:username () ::
    create_field ~tag:554 ~value:passwd () ::
    (match testreqid with
     | None -> []
     | Some value -> [create_field ~tag:112 ~value ()]) in
  make_msg "0" fields

let testreq reqid =
  make_msg "1" [create_field ~tag:112 ~value:(string_of_int reqid) ()]

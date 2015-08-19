type msgname =
  | Heartbeat
  | Logon
  | Logout

let msgtype_of_msgname = function
  | Heartbeat -> "0"
  | Logon -> "A"
  | Logout -> "5"

let msgname_of_msgtype = function
  | "0" -> Some Heartbeat
  | "A" -> Some Logon
  | "5" -> Some Logout
  | _ -> None

type tag =
  | BeginString [@value 8]
  | BodyLength [@value 9]
  | CheckSum [@value 10]
  | MsgType [@value 35]
  | SenderCompId [@value 49]
  | TargetCompId [@value 56]
  | Text [@value 58]
  | HeartBtInt [@value 108]
  | ResetSeqNumFlag [@value 141]
  | Username [@value 553]
  | Password [@value 554]
      [@@deriving show, enum]

type msg = (int, string) Hashtbl.t

let show_msg msg =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\n";
  Hashtbl.iter (fun tag value ->
      let tag_str = tag_of_enum tag |> function
        | Some tag -> show_tag tag
        | None -> string_of_int tag in
      Buffer.add_string buf @@ tag_str ^ "=" ^ value ^ "\n")
    msg;
  Buffer.contents buf

let field_of_string s =
  try
    let i = String.index s '=' in
    int_of_string @@ String.sub s 0 i,
    String.sub s (i+1) (String.length s - i - 1)
  with _ -> invalid_arg ("field_of_string: " ^ s)

let string_of_field tag value =
  let buf = Buffer.create 128 in
  Buffer.add_string buf @@ string_of_int tag;
  Buffer.add_char buf '=';
  Buffer.add_string buf value;
  Buffer.add_char buf '\001';
  Buffer.contents buf

let string_of_msg msg =
  let buf = Buffer.create 128 in
  List.iter
    (fun tag -> Buffer.add_string buf @@ string_of_field tag @@ Hashtbl.find msg tag)
    [8; 9; 35];
  Hashtbl.iter
    (fun tag field ->
       if tag <> 8 && tag <> 9 && tag <> 35 then
         Buffer.add_string buf @@ string_of_field tag field)
    msg;
  let res = Buffer.contents buf in
  let v = ref 0 in
  String.iter (fun c -> v := !v + Char.code c) res;
  Buffer.add_string buf ("10=" ^ string_of_int (!v mod 256) ^ "\001");
  Buffer.contents buf

let body_length msg =
  Hashtbl.fold
    (fun tag value a ->
       a + 2 + String.length value +
       String.length (string_of_int tag)
    )
    msg 0

let msg_maker ?(major=4) ?(minor=4) ~sendercompid ~targetcompid () =
  let seqnum = ref 1 in
  fun msgtype fields ->
    let verstring = Printf.sprintf "FIX.%d.%d" major minor in
    let timestring =
      let open Unix in
      let timeofday = gettimeofday () in
      let ms, _ = modf timeofday in
      let tm = timeofday |> gmtime in
      Printf.sprintf "%d%02d%02d-%02d:%02d:%02d.%3.0f"
        (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour
        tm.tm_min tm.tm_sec (ms *. 1000.) in
    let msg = Hashtbl.create 13 in
    Hashtbl.add msg 34 (string_of_int !seqnum);
    Hashtbl.add msg 35 msgtype;
    Hashtbl.add msg 49 sendercompid;
    Hashtbl.add msg 56 targetcompid;
    Hashtbl.add msg 52 timestring;
    List.iter (fun (tag, value) -> Hashtbl.add msg tag value) fields;
    let msglen = body_length msg in
    Hashtbl.add msg 8 verstring;
    Hashtbl.add msg 9 (string_of_int msglen);
    incr seqnum;
    pred !seqnum, msg

let read_msg s ~pos ~len =
  let msg = Hashtbl.create 13 in
  let rec inner pos =
    try
      let i = String.index_from s pos '\001' in
      if i > pos + len then raise Not_found
      else
        let sub = String.sub s pos (i - pos) in
        let tag, value = field_of_string sub in
        Hashtbl.add msg tag value;
        inner (succ i)
    with Not_found -> msg
  in inner pos

let write_msg buf ~pos msg =
  let msg = string_of_msg msg in
  let msg_len = String.length msg in
  Bytes.blit_string msg 0 buf pos msg_len

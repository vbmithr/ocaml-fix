include Fix_intf

let msgname_of_string = function
  | "0" -> Some Heartbeat
  | "1" -> Some TestRequest
  | "2" -> Some ResendRequest
  | "3" -> Some Reject
  | "4" -> Some SequenceReset
  | "5" -> Some Logout
  | "A" -> Some Logon
  | _ -> None

module IntMap = Map.Make(struct type t = int let compare = compare end)

type msg = string IntMap.t

let show_msg msg =
  let buf = Buffer.create 128 in
  Buffer.add_string buf "\n";
  IntMap.iter (fun tag value ->
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
    (fun tag -> Buffer.add_string buf @@ string_of_field tag @@ IntMap.find tag msg)
    [8; 9; 35];
  IntMap.iter
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
  IntMap.fold
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
    let msg = IntMap.empty in
    let msg = IntMap.add 34 (string_of_int !seqnum) msg in
    let msg = IntMap.add 35 msgtype msg in
    let msg = IntMap.add 49 sendercompid msg in
    let msg = IntMap.add 56 targetcompid msg in
    let msg = IntMap.add 52 timestring msg in
    let msg = List.fold_left (fun a (tag, value) ->
        IntMap.add tag value a) msg fields in
    let msglen = body_length msg in
    let msg = IntMap.add 8 verstring msg in
    let msg = IntMap.add 9 (string_of_int msglen) msg in
    incr seqnum;
    pred !seqnum, msg

let read_msg s ~pos ~len =
  let rec inner acc pos =
    try
      let i = String.index_from s pos '\001' in
      if i > pos + len then raise Not_found
      else
        let sub = String.sub s pos (i - pos) in
        let tag, value = field_of_string sub in
        inner (IntMap.add tag value acc) @@ succ i
    with Not_found -> acc
  in inner IntMap.empty pos

let write_msg buf ~pos msg =
  let msg = string_of_msg msg in
  let msg_len = String.length msg in
  Bytes.blit_string msg 0 buf pos msg_len

open Astring
open Sexplib.Std

include Fix_intf

type t = {
  major: int;
  minor: int;
  len: int;
  typ: MsgType.t;
  fields: string Tag.Map.t
} [@@deriving create, sexp]

let extract_opt ~msg = function
  | None -> invalid_arg @@ "extract_opt: " ^ msg
  | Some v -> v

let size_of_int = function
  | i when i < 10 -> 1
  | i when i < 100 -> 2
  | i when i < 1000 -> 3
  | i when i < 10000 -> 4
  | _ -> invalid_arg "size_of_int"

let length_of_field tag value =
  2 + (Tag.to_enum tag |> size_of_int) + String.length value

let field_of_sub ?expected s =
  let s_str = String.Sub.to_string s in
  let tag, value = String.Sub.cut ~sep:(String.Sub.of_char '=') s |> extract_opt ~msg:("field_of_sub: " ^ s_str) in
  let tag = String.Sub.to_int tag |> extract_opt ~msg:("field_of_sub" ^ s_str) in
  let tag = Tag.of_enum tag in
  match expected with
  | None -> tag, value
  | Some expected ->
    if expected <> tag then invalid_arg "field_of_sub: unexpected tag"
    else tag, value

let parse_version s = Scanf.sscanf s "FIX.%d.%d" (fun major minor -> major, minor)
let print_version ~major ~minor = Printf.sprintf "FIX.%d.%d" major minor

let create ?check_len ?(major=4) ?(minor=2) ~typ ~fields =
  let len = Tag.Map.fold (fun tag value a -> a + length_of_field tag value) fields (length_of_field (S MsgType) (MsgType.to_string typ)) in
  match check_len with
  | None -> create ~major ~minor ~len ~typ ~fields
  | Some len' ->
    if len <> len' then invalid_arg @@ Printf.sprintf "create: check_len failure: given %d, computed %d" len' len 
   else create ~major ~minor ~len ~typ ~fields

let to_bytes { major; minor; typ; len; fields } =
  let add_field buf tag value =
    Buffer.add_string buf (Tag.to_enum tag |> String.of_int);
    Buffer.add_char buf '=';
    Buffer.add_string buf value;
    Buffer.add_char buf '\001'
  in
  let buf = Buffer.create 128 in
  add_field buf (S BeginString) @@ print_version ~major ~minor;
  add_field buf (S BodyLength) @@ String.of_int len;
  add_field buf (S MsgType) @@ MsgType.to_string typ;
  Tag.Map.iter (fun tag value -> add_field buf tag value) fields;
  let sum = String.fold_left (fun a c -> a + Char.to_int c) 0 @@ Buffer.contents buf in
  add_field buf (S CheckSum) @@ Printf.sprintf "%03d" (sum mod 256);
  Buffer.contents buf

let timestring ts_float =
  let open Unix in
  let ms, _ = modf ts_float in
  let tm = ts_float |> gmtime in
  Printf.sprintf "%d%02d%02d-%02d:%02d:%02d.%03.0f"
    (1900 + tm.tm_year) (tm.tm_mon + 1) tm.tm_mday tm.tm_hour
    tm.tm_min tm.tm_sec (ms *. 1000.)

let make_create ?major ?minor ~sendercompid ~targetcompid () =
  let seqnum = ref 1 in
  let fields = Tag.Map.(add (S TargetCompID) targetcompid (add (S SenderCompID) sendercompid empty)) in
  fun typ fields' ->
    let fields = Tag.Map.add (S MsgSeqNum) (String.of_int !seqnum) fields in
    let fields = Tag.Map.add (S SendingTime) (timestring @@ Unix.gettimeofday ()) fields in
    let fields = ListLabels.fold_left fields' ~f:(fun a (tag, value) -> Tag.Map.add tag value a) ~init:fields in
    let msg = create ?major ?minor ~typ ~fields () in
    incr seqnum;
    pred !seqnum, msg

let add_field msg tag value =
  { msg with
    len = msg.len + length_of_field tag value;
    fields = Tag.Map.add tag value msg.fields
  }

let compute_chksum fields =
  ListLabels.fold_left fields ~init:0 ~f:(fun a sub ->
      String.Sub.fold_left (fun a c -> a + Char.to_int c) (a+1) sub
    ) mod 256

let read ?pos ?len buf =
  let buf = String.sub_with_range ?first:pos ?len buf in
  match String.Sub.cuts ~sep:(String.Sub.of_char '\001') buf with
  | version :: len :: typ :: fields ->
    let fields = List.(tl @@ rev fields) in
    let chksum' = compute_chksum @@ version :: len :: typ :: List.(tl fields) in
    let _, version = field_of_sub version ~expected:(S BeginString) in
    let _, len = field_of_sub len ~expected:(S BodyLength) in
    let _, typ = field_of_sub typ ~expected:(S MsgType) in
    let major, minor = parse_version @@ String.Sub.to_string version in
    let len = String.Sub.to_string len |> int_of_string in
    let typ = String.Sub.to_string typ |> MsgType.of_string in
    let fields = ListLabels.fold_left fields ~init:Tag.Map.empty ~f:(fun a sub ->
        let tag, field = field_of_sub sub in
        Tag.Map.add tag (String.Sub.to_string field) a
      )
    in
    let chksum = Tag.Map.find (S CheckSum) fields |> int_of_string in
    if chksum <> chksum' then invalid_arg "read_msg: bad checksum";
    let fields = Tag.Map.remove (S CheckSum) fields in
    create ~check_len:len ~major ~minor ~typ ~fields ()
  | _ -> invalid_arg "read_msg"

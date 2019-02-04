open Rresult
open Astring
open Sexplib.Std

module Field = Field
module Fixtypes = Fixtypes

open Fixtypes

(* let src = Logs.Src.create "fix.core" *)

type t = {
  version : Version.t ;
  typ : MsgType.t ;
  sid : string ;
  tid : string ;
  seqnum : int ;
  ts : Ptime.t option ;
  fields : Field.Set.t ;
} [@@deriving sexp]

let create
    ?(version=Version.FIXT (1,1))
    ?ts ?(sid="") ?(tid="") ?(seqnum=0) ?(fields=[])  typ =
  { version ; typ ; sid ; tid ; seqnum ; ts ; fields = Field.Set.of_list fields }

let create_set
    ?(version=Version.FIXT (1,1))
    ?ts ?(sid="") ?(tid="") ?(seqnum=0) ?(fields=Field.Set.empty) typ =
  { version ; typ ; sid ; tid ; seqnum ; ts ; fields }

let heartbeat
    ?(version=Version.FIXT (1,1)) ?ts ?sid ?tid ?seqnum ?(testReqID="") () =
  let fields = match testReqID with
    | "" -> Field.Set.empty
    | s -> Field.(Set.singleton (TestReqID.create s)) in
  create_set ?ts ?sid ?tid ?seqnum ~fields ~version Fixtypes.MsgType.Heartbeat

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let to_bytes ?(buf = Buffer.create 128) { version ; typ ; sid ; tid ; seqnum ; ts ; fields } =
  let add_field buf tag value =
    Buffer.add_string buf tag;
    Buffer.add_char buf '=';
    Buffer.add_string buf value;
    Buffer.add_char buf '\x01'
  in
  let fields =
    match ts with
    | None -> fields
    | Some ts -> Field.Set.add (Field.SendingTime.create ts) fields in
  add_field buf "35" @@ MsgType.print typ ;
  add_field buf "49" sid ;
  add_field buf "56" tid ;
  add_field buf "34" @@ string_of_int seqnum ;
  Field.Set.iter begin fun f ->
    Field.add_to_buffer buf f ;
    Buffer.add_char buf '\x01'
  end fields ;
  let fieldslen = Buffer.length buf in
  let fields = Buffer.contents buf in
  Buffer.clear buf ;
  add_field buf "8" @@ Version.print version ;
  add_field buf "9" @@ String.of_int fieldslen ;
  Buffer.add_string buf fields ;
  let sum = String.fold_left (fun a c -> a + Char.to_int c) 0 @@ Buffer.contents buf in
  add_field buf "10" @@ Printf.sprintf "%03d" (sum mod 256);
  Buffer.contents buf

(* Compute checksum on all but the last field *)
let compute_chksum fields =
  let global, local =
    ListLabels.fold_left fields ~init:(0, 0) ~f:begin fun (global, _local) sub ->
      let count = String.Sub.fold_left (fun a c -> a + Char.to_int c) 1 sub in
      global + count, count
    end in
  (global - local) mod 256

let of_fields fields =
  let open R.Infix in
  match fields with
  | beginString :: bodyLength :: msgType :: fields ->
    R.of_option
      ~none:(fun () -> R.error_msg "missing BeginString")
      (Field.find Field.BeginString beginString) >>= fun version ->
    R.of_option
      ~none:(fun () -> R.error_msg "missing BodyLength")
      (Field.find Field.BodyLength bodyLength) >>= fun _ ->
    R.of_option
      ~none:(fun () -> R.error_msg "missing MsgType")
      (Field.find Field.MsgType msgType) >>= fun typ ->
    let fields = Field.Set.of_list fields in
    R.of_option
      ~none:(fun () -> R.error_msg "missing MsgSeqNum")
      (Field.find_set Field.MsgSeqNum fields) >>= fun seqnum ->
    R.of_option
      ~none:(fun () -> R.error_msg "missing SenderCompID")
      (Field.find_set Field.SenderCompID fields) >>= fun sid ->
    R.of_option
      ~none:(fun () -> R.error_msg "missing TargetCompID")
      (Field.find_set Field.TargetCompID fields) >>= fun tid ->
    let ts = Field.find_set Field.SendingTime fields in
    let fields = Field.(remove_set BeginString fields) in
    let fields = Field.(remove_set BodyLength fields) in
    let fields = Field.(remove_set MsgType fields) in
    let fields = Field.(remove_set MsgSeqNum fields) in
    let fields = Field.(remove_set SenderCompID fields) in
    let fields = Field.(remove_set TargetCompID fields) in
    let fields = Field.(remove_set SendingTime fields) in
    R.ok (create_set ?ts ~fields ~seqnum ~sid ~tid ~version typ)
  | _ ->
    R.error_msg "missing standard header"

let read ?pos ?len buf =
  let buf = String.sub_with_range ?first:pos ?len buf in
  let fields = String.Sub.cuts ~empty:false ~sep:(String.Sub.of_char '\x01') buf in
  let computed_chksum = compute_chksum fields in
  begin
    try
      ListLabels.fold_left fields ~init:[] ~f:begin fun a f ->
        match Field.parse (String.Sub.to_string f) with
        | Error _ as e -> R.failwith_error_msg e
        | Ok v -> v :: a
      end |> R.ok
    with Failure msg -> R.error_msg msg
  end |> function
  | Error _ as e -> e
  | Ok [] -> R.error_msg "empty message"
  | Ok (chksum :: fields) ->
    let open R.Infix in
    R.of_option
      ~none:(fun () -> R.error_msg "missing CheckSum")
      (Field.CheckSum.find Field.CheckSum chksum) >>= fun ck ->
    begin if computed_chksum <> (int_of_string ck)
      then R.error_msg "bad checksum"
      else R.ok ()
    end >>= fun () ->
    of_fields fields

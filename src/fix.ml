open Rresult
open Astring
open Sexplib.Std

module Field = Field
module Fixtypes = Fixtypes

open Fixtypes

let src = Logs.Src.create "fix.core"

type t = {
  typ : MsgType.t ;
  sid : string ;
  tid : string ;
  seqnum : int ;
  ts : Ptime.t option ;
  fields : Field.field list ;
} [@@deriving sexp]

let create ?ts ?(sid="") ?(tid="") ?(seqnum=0) ?(fields=[]) typ =
  { typ ; sid ; tid ; seqnum ; ts ; fields }

let heartbeat ?ts ?sid ?tid ?seqnum ?(testReqID="") () =
  let fields = match testReqID with
    | "" -> []
    | s -> [Field.TestReqID.create s] in
  create ?ts ?sid ?tid ?seqnum ~fields Fixtypes.MsgType.Heartbeat

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let to_bytes ?(buf = Buffer.create 128) ~version { typ ; fields } =
  let add_field buf tag value =
    Buffer.add_string buf tag;
    Buffer.add_char buf '=';
    Buffer.add_string buf value;
    Buffer.add_char buf '\x01'
  in
  let fields = Field.MsgType.create typ :: fields in
  ListLabels.iter fields ~f:begin fun f ->
    Field.add_to_buffer buf f ;
    Buffer.add_char buf '\x01'
  end ;
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

let read ?pos ?len buf =
  let buf = String.sub_with_range ?first:pos ?len buf in
  let fields = String.Sub.cuts ~empty:false ~sep:(String.Sub.of_char '\x01') buf in
  Logs.debug ~src (fun m -> m "Read %a" String.Sub.pp buf) ;
  let computed_chksum = compute_chksum fields in
  begin
    try
      ListLabels.fold_left fields ~init:[] ~f:begin fun a f ->
        match Field.parse (String.Sub.to_string f) with
        | Error _ as e -> R.failwith_error_msg e
        | Ok None -> a
        | Ok (Some v) ->
          Logs.debug ~src (fun m -> m "Parsed %a" Field.pp v) ;
          v :: a
      end |> R.ok
    with Failure msg -> R.error_msg msg
  end |> function
  | Error _ as e -> e
  | Ok [] -> R.error_msg "empty message"
  | Ok (chksum :: fields) ->
    match Field.(CheckSum.find CheckSum chksum),
          Field.find_list Field.MsgType fields,
          Field.find_list Field.MsgSeqNum fields,
          Field.find_list Field.SenderCompID fields,
          Field.find_list Field.TargetCompID fields
    with
    | None, _, _, _, _ -> R.error_msg "missing checksum"
    | Some _, None, _, _, _ -> R.error_msg "missing MsgType"
    | Some _, Some _, None, _, _ -> R.error_msg "missing MsgSeqNum"
    | Some _, Some _, Some _, None, _ -> R.error_msg "missing SenderCompID"
    | Some _, Some _, Some _, Some _, None -> R.error_msg "missing targetCompID"
    | Some ck, Some typ, Some seqnum, Some sid, Some tid ->
      if computed_chksum <> (int_of_string ck) then R.error_msg "bad checksum"
      else
        let ts = Field.find_list Field.SendingTime fields in
        R.ok (create ?ts ~fields ~seqnum ~sid ~tid typ)

open Rresult
open Astring
open Sexplib.Std

module Field = Field
module Fixtypes = Fixtypes

open Fixtypes

let src = Logs.Src.create "fix.core"

type t = {
  typ : MsgType.t ;
  fields : Field.field list ;
} [@@deriving sexp]

let create ~typ ~fields = { typ ; fields }

let heartbeat ~senderCompID ~targetCompID ~testReqID seqnum =
  let fields = Field.[
      SenderCompID.create senderCompID ;
      TargetCompID.create targetCompID ;
      TestReqID.create testReqID ;
      MsgSeqNum.create seqnum ;
    ] in
  create ~typ:Fixtypes.MsgType.Heartbeat ~fields

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
  Logs.debug ~src (fun m -> m "Found %d fields" (List.length fields)) ;
  let computed_chksum = compute_chksum fields in
  begin
    try
      ListLabels.fold_left fields ~init:[] ~f:begin fun a f ->
        Logs.debug ~src (fun m -> m "Parsing %a" String.Sub.pp f) ;
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
          Field.find_list Field.MsgType fields with
    | None, _ -> R.error_msg "missing checksum"
    | Some _, None -> R.error_msg "missing MsgType"
    | Some chksum, Some typ ->
      if computed_chksum <> (int_of_string chksum) then
        R.error_msg "bad checksum"
      else R.ok (create ~typ ~fields)

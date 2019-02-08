(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

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
  groups : (Field.field * Field.field list list) option ;
} [@@deriving sexp]

let create
    ?(version=Version.FIXT (1,1))
    ?ts ?(sid="") ?(tid="") ?(seqnum=0) ?(fields=[]) ?groups typ =
  { version ; typ ; sid ; tid ; seqnum ; ts ;
    fields = Field.Set.of_list fields ; groups }

let create_set
    ?(version=Version.FIXT (1,1))
    ?ts ?(sid="") ?(tid="") ?(seqnum=0) ?(fields=Field.Set.empty) ?groups typ =
  { version ; typ ; sid ; tid ; seqnum ; ts ; fields ; groups }

let heartbeat
    ?(version=Version.FIXT (1,1)) ?ts ?sid ?tid ?seqnum ?(testReqID="") () =
  let fields = match testReqID with
    | "" -> Field.Set.empty
    | s -> Field.(Set.singleton (TestReqID.create s)) in
  create_set ?ts ?sid ?tid ?seqnum ~fields ~version Fixtypes.MsgType.Heartbeat

let pp ppf t =
  Format.fprintf ppf "%a" Sexplib.Sexp.pp (sexp_of_t t)

let serialize t
    { version ; typ ; sid ; tid ;
      seqnum ; ts ; fields ; groups } =
  let open Faraday in
  let effects = ref [] in
  let len = ref 0 in
  let sum = ref 0 in
  let k a b c =
    effects := a :: !effects ;
    len := !len + b ;
    sum := !sum + c in
  let write_kchar c =
    k (fun () -> write_char t c) 1 (Char.to_int c) in
  let add_field k tag value =
    let open Faraday in
    k begin fun () ->
      write_string t tag ;
      write_char t '=' ;
      write_string t value ;
      write_char t '\x01'
    end
      (String.length tag + String.length value + 2)
      (Field.sum_string tag + Field.sum_string value + Char.to_int '=' + 1)
  in
  let inner () =
    let fields =
      match ts with
      | None -> fields
      | Some ts -> Field.Set.add (Field.SendingTime.create ts) fields in
    add_field k "35" @@ MsgType.print typ ;
    add_field k "49" sid ;
    add_field k "56" tid ;
    add_field k "34" (string_of_int seqnum) ;
    Field.Set.iter begin fun f ->
      Field.serialize k t f ;
      write_kchar '\x01'
    end fields ;
    begin match groups with
      | None -> ()
      | Some (sep, groups) ->
        Field.serialize k t sep ;
        write_kchar '\x01' ;
        List.iter begin fun group ->
          List.iter begin fun f ->
            Field.serialize k t f ;
            write_kchar '\x01' ;
          end group
        end groups
    end
  in
  inner () ;
  add_field (fun e _ s -> sum := !sum + s ; e ()) "8" (Version.print version) ;
  add_field (fun e _ s -> sum := !sum + s ; e ()) "9" (String.of_int !len) ;
  add_field k "10" (Printf.sprintf "%03d" (!sum mod 256)) ;
  List.fold_right (fun e () -> e ()) !effects ()

let fields_groups fields =
  let s, sep, _, groups =
    List.fold_left begin fun (s, sep, first_in_group, groups) f ->
      match
        Field.find Field.NoRelatedSym f = None &&
        Field.find Field.NoMDEntries f = None &&
        Field.find Field.NoPositions f = None &&
        Field.find Field.NoFills f = None &&
        Field.find Field.TotNumReports f = None &&
        Field.find Field.NoMiscFees f = None &&
        true,
        first_in_group,
        groups
      with
      | true, None, _ -> Field.Set.add f s, sep, first_in_group, groups
      | true, Some f', _ when Field.same_kind f f' -> s, sep, first_in_group, [f] :: groups
      | true, Some _, [] -> s, sep, Some f, [[f]]
      | true, Some _, h :: t -> s, sep, first_in_group, (f :: h) :: t
      | false, _, _ -> s, Some f, Some f, groups
    end (Field.Set.empty, None, None, []) fields in
  match sep with
  | None -> s, None
  | Some sep -> s, Some (sep, List.rev_map List.rev groups)

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
    let fields, groups = fields_groups fields in
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
    R.ok (create_set ?ts ~fields ?groups ~seqnum ~sid ~tid ~version typ)
  | _ ->
    R.error_msg "missing standard header"

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

(*---------------------------------------------------------------------------
   Copyright (c) 2019 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult

module Field : module type of Field

type t = {
  version : Fixtypes.Version.t ;
  typ : Fixtypes.MsgType.t ;
  sid : string ;
  tid : string ;
  seqnum : int ;
  ts : Ptime.t option ;
  fields : Field.Set.t ;
  groups : (Field.t * Field.field list list) option ;
} [@@deriving sexp,yojson]

val pp : Format.formatter -> t -> unit
val create :
  ?version:Fixtypes.Version.t ->
  ?ts:Ptime.t ->
  ?sid:string ->
  ?tid:string ->
  ?seqnum:int ->
  ?fields:Field.t list ->
  ?groups:(Field.t * Field.t list list) ->
  Fixtypes.MsgType.t -> t

val heartbeat :
  ?version:Fixtypes.Version.t ->
  ?ts:Ptime.t ->
  ?sid:string ->
  ?tid:string ->
  ?seqnum:int ->
  ?testReqID:string -> unit -> t

val serialize : Faraday.t -> t -> unit
val of_fields : Field.t list -> (t, R.msg) result

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

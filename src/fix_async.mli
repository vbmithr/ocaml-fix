open Core_kernel
open Async
open Fix

val with_connection :
  ?tmpbuf:Bytes.t ->
  version:Fixtypes.Version.t ->
  Uri.t ->
  (Fix.t Pipe.Reader.t * Fix.t Pipe.Writer.t) Deferred.t

val with_connection_ez :
  ?tmpbuf:Bytes.t ->
  ?history_size:int ->
  ?heartbeat:Time_ns.Span.t ->
  ?logon_fields:Field.t list ->
  sid:string ->
  tid:string ->
  version:Fixtypes.Version.t ->
  Uri.t ->
  (Fix.t Pipe.Reader.t * Fix.t Pipe.Writer.t) Deferred.t

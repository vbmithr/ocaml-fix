open Core_kernel
open Async
open Fix

val with_connection :
  ?tmpbuf:Bytes.t ->
  version:Version.t ->
  Uri.t ->
  (t Pipe.Reader.t * t Pipe.Writer.t) Deferred.t

open Core_kernel
open Async
open Fix

val with_connection :
  ?tmpbuf:Bytes.t ->
  version:Fixtypes.Version.t ->
  Uri.t ->
  (Fix.t Pipe.Reader.t * Fix.t Pipe.Writer.t) Deferred.t

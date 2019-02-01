open Core
open Async

val with_connection :
  Uri.t -> (Fix.t Pipe.Reader.t * Fix.t Pipe.Writer.t) Deferred.t

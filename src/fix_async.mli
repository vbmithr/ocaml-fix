open Core.Std
open Async.Std

val with_connection :
  ?log:Log.t ->
  ?timeout:Time.Span.t ->
  ?tmpbuf:string ->
  ?tls:[`Noconf | `CAFile of string] ->
  host:string ->
  port:int ->
  unit ->
  (Fix.t Pipe.Reader.t * Fix.t Pipe.Writer.t) Deferred.t

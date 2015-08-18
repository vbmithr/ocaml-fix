open Core.Std
open Async.Std

val with_connection :
  ?timeout:Time.Span.t ->
  ?max_msg_size:int ->
  ?ssl:bool -> host:string -> port:int ->
  username:string -> passwd:string ->
  (Fix.msg Pipe.Reader.t * Fix.msg Pipe.Writer.t) Deferred.t

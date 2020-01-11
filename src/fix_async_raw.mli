open Async
open Fix

module Log_async : Logs_async.LOG

val connect :
  ?stream:Faraday.t -> Uri.t ->
  (t Pipe.Reader.t * t Pipe.Writer.t) Deferred.Or_error.t

val with_connection :
  ?stream:Faraday.t -> Uri.t ->
  f:(t Pipe.Reader.t -> t Pipe.Writer.t -> 'a Deferred.Or_error.t) ->
  'a Deferred.Or_error.t

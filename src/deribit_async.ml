open Core
open Async

open Fixtypes

let with_connection url =
  Fix_async.with_connection ~version:Version.v44 url >>= fun (r, w) ->
  return (r, w)

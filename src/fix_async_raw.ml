open Rresult
open Core
open Async
open Fix

let src = Logs.Src.create "fix.async"
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let write_iovec w iovec =
  List.fold_left iovec ~init:0 ~f:begin fun a { Faraday.buffer ; off ; len } ->
    Writer.write_bigstring w buffer ~pos:off ~len ;
    a+len
  end

let rec flush stream w =
  match Faraday.operation stream with
  | `Close -> raise Exit
  | `Yield -> Deferred.unit
  | `Writev iovecs ->
    let nb_written = write_iovec w iovecs  in
    Faraday.shift stream nb_written ;
    flush stream w

let parse_f fields msg_write = function
  | Error e -> Error.raise e
  | Ok ((field, _) as e) ->
    match Field.(find CheckSum field) with
    | None ->
      fields := e :: !fields ;
      Deferred.unit
    | Some chk ->
      match int_of_string_opt chk with
      | None -> failwith "checksum is not an integer"
      | Some chk ->
        let chk' =
          List.fold_left !fields ~init:0 ~f:(fun a (_, c) -> a + c) in
        if chk <> (chk' mod 256) then failwith "checksum failed"
        else
          match Fix.of_fields (List.rev_map !fields ~f:fst) with
          | Error _ as e -> R.failwith_error_msg e
          | Ok msg ->
            fields := [] ;
            Log_async.debug (fun m -> m "<- %a" pp msg) >>= fun () ->
            Pipe.write msg_write msg

let run stream msg_read msg_write r w =
  don't_wait_for begin
    Pipe.iter msg_read ~f:begin fun msg ->
      Fix.serialize stream msg ;
      flush stream w >>= fun () ->
      Log_async.debug (fun m -> m "-> %a" Fix.pp msg)
    end
  end ;
  let fields = ref [] in
  let prsr = Angstrom.lift
      (R.reword_error (function `Msg m -> Error.of_string m)) Field.parser in
  Deferred.Result.map_error ~f:Error.of_string
    (Angstrom_async.parse_many prsr (parse_f fields msg_write) r)

let connect ?(stream=Faraday.create 4096) uri =
  let client_read, msg_write = Pipe.create () in
  let msg_read, client_write = Pipe.create () in
  let cleanup r w _ =
    Log_async.warn (fun m -> m "connect: cleaning up") >>= fun () ->
    Pipe.close_read msg_read ;
    Pipe.close msg_write ;
    Deferred.any_unit [ Reader.close r ; Writer.close w ] in
  Monitor.try_with_or_error
    (fun () -> Async_uri.connect uri) >>|? fun (_, _, r, w) ->
  don't_wait_for begin
    Deferred.any [
      (Pipe.closed client_write >>= fun () -> Deferred.Or_error.ok_unit) ;
      run stream msg_read msg_write r w ;
    ] >>= cleanup r w
  end ;
  client_read, client_write

let with_connection ?stream url ~f =
  connect ?stream url >>=? fun (r, w) ->
  Monitor.protect (fun () -> f r w)
    ~finally:begin fun () ->
      Pipe.close w ; Pipe.close_read r ; Deferred.unit
    end

open Rresult
open Core
open Async
open Fix

let src = Logs.Src.create "fix.async"
module Log = (val Logs.src_log src : Logs.LOG)
module Log_async = (val Logs_async.src_log src : Logs_async.LOG)

let write_iovecs w iovecs =
  let nbWritten =
    List.fold_left iovecs ~init:0 ~f:begin fun a ({ Faraday.len; _ } as iovec) ->
      Writer.schedule_iovec w (Obj.magic iovec) ;
      a+len
    end in
  `Ok nbWritten

let flush stream w =
  Faraday_async.serialize stream
    ~yield:(fun _ -> Scheduler.yield ())
    ~writev:(fun iovecs -> return (write_iovecs w iovecs))

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

let mk_client_write ~monitor w =
  Pipe.create_writer begin fun r ->
    Scheduler.within' ~monitor begin fun () ->
      Pipe.iter r ~f:begin fun msg ->
        let stream = Faraday.create 256 in
        Fix.serialize stream msg ;
        Faraday.close stream ;
        flush stream w >>= fun () ->
        Log_async.debug (fun m -> m "-> %a" Fix.pp msg)
      end
    end
  end

let mk_client_read ~monitor r =
  Pipe.create_reader ~close_on_exception:false begin fun w ->
    Scheduler.within' ~monitor begin fun () ->
      let fields = ref [] in
      let prsr = Angstrom.lift
          (R.reword_error (function `Msg m -> Error.of_string m)) Field.parser in
      Deferred.Or_error.ok_exn @@
      Deferred.Result.map_error ~f:Error.of_string
        (Angstrom_async.parse_many prsr (parse_f fields w) r)
    end
  end

let connect uri =
  Async_uri.connect uri >>= fun { r; w; _ } ->
  let monitor = Monitor.create () in
  let client_read = mk_client_read ~monitor r in
  let client_write = mk_client_write ~monitor w in
  let log_exn exn = Log.err (fun m -> m "%a" Exn.pp exn) in
  Monitor.detach_and_iter_errors monitor ~f:log_exn ;
  Monitor.detach_and_iter_errors (Writer.monitor w) ~f:log_exn ;
  don't_wait_for (Deferred.all_unit Pipe.[closed client_read;
                                          closed client_write] >>= fun () ->
                  Writer.close w >>= fun () ->
                  Reader.close r) ;
  return (client_read, client_write)

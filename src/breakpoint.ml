open! Core
open Async

(* Keep in sync with magic_breakpoint_next_stub *)
module Hit = struct
  type t =
    { timestamp : Time_ns.Span.t
    ; passed_timestamp : Time_ns.Span.t
    ; passed_val : int
    ; tid : Pid.t
    ; ip : Int64.Hex.t
    }
  [@@deriving sexp]
end

module Perf_bp = struct
  type t

  external create
    :  tid:int
    -> cpu:int
    -> addr:int64
    -> single_hit:bool
    -> (t, int) result
    = "magic_breakpoint_create_stub"

  let create follow ~addr ~single_hit =
    let tid, cpu =
      match follow with
      | `Tid tid -> Pid.to_int tid, -1
      | `Cpu cpu ->  -1, cpu
    in
    match create ~tid ~cpu ~addr ~single_hit with
    | Ok t -> Ok t
    | Error errno -> Errno.to_error errno
  ;;

  external get_fd : t -> int = "magic_breakpoint_fd_stub"

  let fd t = get_fd t |> Core_unix.File_descr.of_int |> Core_unix.dup

  external destroy : t -> unit = "magic_breakpoint_destroy_stub"
  external next_hit' : t -> require_pid:int -> Hit.t option = "magic_breakpoint_next_stub"

  let next_hit t = next_hit' t ~require_pid:0
end

(* let get_tids pid : Pid.Set.t =
 *   let pid = Pid.to_int pid in
 *   let tasks = Core_unix.opendir [%string "/proc/%{pid#Int}/task"] in
 *   let rec collect tid_accum =
 *     match Core_unix.readdir_opt tasks with
 *     | None -> tid_accum
 *     | Some ("." | "..") -> collect tid_accum
 *     | Some tid -> collect (Set.add tid_accum (Int.of_string tid |> Pid.of_int))
 *   in
 *   collect Pid.Set.empty
 * ;; *)

module All_cpus = struct
  type t =
    { pid : Pid.t
    ; fds : Perf_bp.t list (** Indexed by CPU *)
    ; single_hit : bool
    ; mutable async_cycle_time_of_last_delivered_hit : Time_ns.t
    (** There's no reason to take more than one snapshot in quick succession, so we only
        want to deliver a hit once per async cycle, even though the FDs can generate
        multiple hits per async cycle. *)
    }

  let destroy t = List.iter t.fds ~f:Perf_bp.destroy

  let with_process_filtering__resolve_when_done
    pid
    ~addr
    ~single_hit
    ~interrupt
    ~on_async_cycle_with_hit
    =
    let cpus = (Or_error.ok_exn Linux_ext.cores) () |> List.init ~f:Fn.id in
    let%map.Or_error fds =
      List.fold_right cpus ~init:(Ok []) ~f:(fun cpu acc ->
        let%bind.Or_error bp_list = acc in
        (* We can't use the typical single-hit implementation, because it would mean that
           a breakpoint hit in another process would cause us to stop recording. *)
        let%map.Or_error bp = Perf_bp.create (`Cpu cpu) ~addr ~single_hit:false in
        bp :: bp_list)
    in
    let t =
      { pid; fds; single_hit; async_cycle_time_of_last_delivered_hit = Time_ns.epoch }
    in
    (* Since other processes can generate events that fill up the ring buffer, we need to
       make sure they are always cleared out. Therefore handle FD polling in this module. *)
    List.mapi t.fds ~f:(fun cpu bp ->
      let async_fd =
        Async_unix.Fd.create
          Async_unix.Fd.Kind.File
          (Perf_bp.fd bp)
          (Info.of_string [%string "perf breakpoint cpu %{cpu#Int}"])
      in
      let%map res =
        Async_unix.Fd.interruptible_every_ready_to
          async_fd
          `Read
          ~interrupt
          (fun () ->
            let this_cycle_start = Async_unix.Scheduler.cycle_start_ns () in
            print_endline "Got a wakeup!";
            let rec handle_next_hit () =
              match Perf_bp.next_hit' bp ~require_pid:(Pid.to_int pid) with
              | None -> ()
              | Some hit ->
                (* Handle user callback first to minimize gap between bp hit and snapshot *)
                if Time_ns.( < ) t.async_cycle_time_of_last_delivered_hit this_cycle_start
                then (
                  t.async_cycle_time_of_last_delivered_hit <- this_cycle_start;
                  print_endline "invoking callback!";
                  on_async_cycle_with_hit hit;
                  if t.single_hit
                  then destroy t (* [destroy] makes all future [next_hit] calls None. *));
                handle_next_hit ()
            in
            handle_next_hit ())
          ()
      in
      match res with
      | `Interrupted -> Perf_bp.destroy bp
      | `Bad_fd | `Closed | `Unsupported -> failwith "failed to wait on breakpoint")
    |> Deferred.all_unit
  ;;
end

open! Core

(* For the signal FD to work, all of the following must true:
   - Signal delivery setup (ie setting the signal mask) happens on some thread A
   - This signal does not get unmasked by the runtime or any library (fingers crossed)
   - The breakpoint is configured to deliver to the same thread A
   - The signal FD is polled on thread A.

   The only real way to ensure this is to ensure that all ocaml code is running on one
   thread: the main thread. Hence, we need Notify_the_scheduler.
*)
let assert_async_when_finished_behavior () =
  match !Async_unix.In_thread.When_finished.default with
  | Notify_the_scheduler -> ()
  | Take_the_async_lock | Try_to_take_the_async_lock ->
    failwith
      "Async scheduler needs to\n be set to notify for async breakpoint delivery to work!"
;;

module Signal_delivery = struct
  type t = Core_unix.Thread_id.t (* The TID that we set up on *)

  let marker_signal = 16
  (* SIGSTKFLT, unused on linux. Hopefully no one will miss this
     when we mask it and use a signalfd *)

  module Fd = struct
    type outer = t
    type t = Core_unix.File_descr.t

    external create : outer -> t = "magic_breakpoint_make_signal_fd_stub"

    let inner t =
      (* The thread we are on matters at poll/read time, not at signalfd
         creation time. So this check makes more sense here. *)
      assert_async_when_finished_behavior ();
      t
    ;;

    external which_breakpoint_fd_triggered
      :  t
      -> int
      = "magic_breakpoint_signal_fd_which_triggered_stub"

    let[@inline] which_breakpoint_fd_triggered t =
      assert_async_when_finished_behavior ();
      which_breakpoint_fd_triggered t
    ;;

    external destroy : t -> unit = "magic_breakpoint_signal_fd_destroy_stub"
  end

  external setup_on_this_thread
    :  signal:int
    -> unit
    = "magic_breakpoint_signal_delivery_setup_stub"

  let setup_on_this_thread () =
    (* CR-someday ibrooks: It would be great if we coud check that we were on the main
       thread already. We might even set this variable here. But that's also global state,
       so it kind of makes sense to require it to have been set near the start of the
       application. *)
    assert_async_when_finished_behavior ();
    let tid = (Or_error.ok_exn Core_unix.gettid) () in
    setup_on_this_thread ~signal:marker_signal;
    tid
  ;;

  let configure_async_runtime () =
    Async_unix.In_thread.When_finished.default := Notify_the_scheduler
  ;;
end

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
    :  pid:Pid.t
    -> addr:int64
    -> single_hit:bool
    -> inherit_and_set_signal_delivery:bool
    -> signal_tid:int
    -> which_signal:int
    -> (t, int) result
    = "magic_breakpoint_create_stub_bytecode" "magic_breakpoint_create_stub_native"
  (* CR-someday ibrooks: perf has a flag to limit inherits to threads. I think it
     makes sense to keep this off in case a process forks and executes in the same
     address space, but fork+exec means we could theoretically get erronious
     breakpoint hits. *)

  let create
    pid
    ~addr
    ~single_hit
    ~(inherit_behavior :
       [ `Do_not_inherit | `Inherit_and_signal_this_thread of Signal_delivery.t ])
    =
    let inherit_and_set_signal_delivery, signal_tid, which_signal =
      match inherit_behavior with
      | `Do_not_inherit -> false, 0, 0
      | `Inherit_and_signal_this_thread tid ->
        true, Core_unix.Thread_id.to_int tid, Signal_delivery.marker_signal
    in
    match
      create
        ~pid
        ~addr
        ~single_hit
        ~inherit_and_set_signal_delivery
        ~signal_tid
        ~which_signal
    with
    | Ok t -> Ok t
    | Error errno -> Errno.to_error errno
  ;;

  external get_fd : t -> int = "magic_breakpoint_fd_stub"

  let fd t = get_fd t |> Core_unix.File_descr.of_int |> Core_unix.dup

  external destroy : t -> unit = "magic_breakpoint_destroy_stub"
  external next_hit : t -> Hit.t option = "magic_breakpoint_next_stub"
end

let get_tids pid : Pid.Set.t =
  let pid = Pid.to_int pid in
  let tasks = Core_unix.opendir [%string "/proc/%{pid#Int}/task"] in
  let rec collect tid_accum =
    match Core_unix.readdir_opt tasks with
    | None -> tid_accum
    | Some ("." | "..") -> collect tid_accum
    | Some tid -> collect (Set.add tid_accum (Int.of_string tid |> Pid.of_int))
  in
  collect Pid.Set.empty
;;

module All_threads = struct
  type t =
    { signal_fd : Signal_delivery.Fd.t
    ; fd_to_breakpoint : Perf_bp.t Int.Map.t
    }

  let of_process pid ~addr ~single_hit =
    (* ibrooks: When we are attaching to a running a process, installing breakpoints that
       inherit on thread creation is racy with thread creation itself. Without stopping
       the process first, we can't be sure that our breakpoint was installed before the
       new thread was created.

       There is a theoretically robust way to stop all threads in a process so we can
       guarantee no races. However, it's slow and complex. Complex code is sad. Slowly
       stopping an actively-running performance-sensitive process is also sad. It's rare
       that a running program actually spawns new threads. So, we do the simple thing
       here: check if we might have failed, tell the user, and give up.

       For future reference, I've written the theoretical stop procedure here. The problem
       is that we want to stop the tracee such that if magic trace is killed or crashes,
       the tracee resumes like nothing happened. This means we want to use ptrace rather
       than a raw SIGSTOP. So, in a C stub, we would:

       1. Open a pipe. Fork. The parent waits on the pipe.
       2. The child robustly uses ptrace to stop all threads in the tracee.
       3. The parent collects tracee TIDs and installs breakpoints.
       4. The parent kills the child. The child has been setup so that the tracee resumes
       upon process death.

       The child stops all threads using this procedure:
       1. Initialize an empty list of stopped TIDs
       2. Collect the TIDs of the tracee
       3. For each thread that is not stopped:
       3.1. PTRACE_SEIZE the thread.
       3.2. PTRACE_INTERRUPT the thread.
       3.3. At this point, the thread is stopped. Either the stop is induced by
       PTRACE_INTERRUPT, or its an unrelated signal delivery stop, syscall stop, or group
       stop. All of these stops should resume with correct behavior when the ptracing
       process dies.
       3.4. Call waitpid to see if the thread died before entering a ptrace stop. Handle
       this case appropriately.
       3.5. Otherwise, add this thread to the list of stopped threads.
       4. Collect the TIDs of the tracee again and see if we've stopped all of them. If we
       haven't, loop.
    *)
    let open Or_error.Let_syntax in
    let signal_delivery = Signal_delivery.setup_on_this_thread () in
    let signal_fd = Signal_delivery.Fd.create signal_delivery in
    let tids_before_breakpoint_install = get_tids pid in
    let%map fd_to_breakpoint =
      Set.fold tids_before_breakpoint_install ~init:(Ok Int.Map.empty) ~f:(fun acc tid ->
        let%bind map = acc in
        let%map bkpt_fd =
          Perf_bp.create
            tid
            ~addr
            ~single_hit
            ~inherit_behavior:(`Inherit_and_signal_this_thread signal_delivery)
        in
        let fd = Perf_bp.get_fd bkpt_fd in
        Map.add_exn map ~key:fd ~data:bkpt_fd)
    in
    let tids_after_breakpoint_install = get_tids pid in
    if not (Set.equal tids_after_breakpoint_install tids_before_breakpoint_install)
    then
      print_s
        [%message
          "Warning: Breakpoint installation raced with tracee thread creation. We can't \
           guarantee triggers will be caught in all threads. If you ever see this, \
           please contact magic-trace developers; we want to know if we should support \
           this."
            (tids_before_breakpoint_install : Pid.Set.t)
            (tids_after_breakpoint_install : Pid.Set.t)];
    { signal_fd; fd_to_breakpoint }
  ;;

  (* CR ibrooks: do we need to dup here? *)
  let fd t = Signal_delivery.Fd.inner t.signal_fd

  let next_hit t =
    (* CR-someday ibrooks: if we get multiple breakpoint hits faster than we can process
       them, the second signal will be dropped, meaning we won't know to look for the
       second hit. We can handle this by iterating over breakpoint FDs and polling for
       breakpoint hits. *)
    let triggered_fd = Signal_delivery.Fd.which_breakpoint_fd_triggered t.signal_fd in
    if triggered_fd > 0
    then (
      let triggered_bp =
        Map.find t.fd_to_breakpoint triggered_fd
        |> Option.value_exn
             ~message:"Signal had an fd value that wasn't in the breakpoint map."
      in
      match Perf_bp.next_hit triggered_bp with
      | Some hit -> Some hit
      | None ->
        failwith "Expected breakpoint that originated signal to have a hit to collect.")
    else
      (* If there's no signal, check if there are any other hits on other FDs. This is
         slow if we have a lot of fds, but we'll only hit this after we've already acted
         on the first breakpoint hit using the signal. *)
      Map.fold_until
        t.fd_to_breakpoint
        ~init:None
        ~f:(fun ~key:_ ~data:perf_fd _acc ->
          match Perf_bp.next_hit perf_fd with
          | None -> Continue None
          | Some hit -> Stop (Some hit))
        ~finish:Fn.id
  ;;

  let destroy t =
    Signal_delivery.Fd.destroy t.signal_fd;
    Map.iter t.fd_to_breakpoint ~f:Perf_bp.destroy
  ;;
end

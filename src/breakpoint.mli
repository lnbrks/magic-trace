open! Core

module Signal_delivery : sig
  type t

  module Fd : sig
    type outer := t
    type t

    val create : outer -> t

    (* CR-someday ibrooks: Look into making an API that slots into Async such that we can
       do a runtime check for signal mask and TID correctness just before polling. *)

    (** This must be polled from the same OS thread that called [Signal_delivery.create] *)
    val inner : t -> Core_unix.File_descr.t

    (** If multiple breakpoint perf fds are open, you must disambiguate which one fired to
        find the [Hit.t]. Before calling, ensure that a poll on the [inner] fd has
        returned, indicating there is a pending signal to be consumed. If there is no
        pending signal, returns -1. *)
    val which_breakpoint_fd_triggered : t -> int
  end

  (** Setup preconditions for signal delivery to the calling thread. In practice, this
      means adding a marker signal to the signal mask. *)
  val setup_on_this_thread : unit -> t

  (** Set the global behavior of the async runtime so that signal delivery will work. This
      is meant to ensure Async cycles are always run on the main thread, so this should be
      called early in the program whenever signal delivery is needed. *)
  val configure_async_runtime : unit -> unit
end

module Hit : sig
  type t =
    { timestamp : Time_ns.Span.t
    ; passed_timestamp : Time_ns.Span.t
    ; passed_val : int
    ; tid : Pid.t
    ; ip : int64
    }
  [@@deriving sexp]
end

module Perf_bp : sig
  type t

  (** Uses [perf_event_open] to set a hardware breakpoint at a given address in a process.
      When that breakpoint is hit the resulting file descriptor will poll as readable.

      If [single_hit] is set the breakpoint will disable itself after being hit once.

      If [inherit_behavior] is [`Inherit_and_signal_this_thread], then the event will be
      created with the inherit attribute set, which means it follows thread creation and
      forking. Further, in this case, polls to the resulting fd will not behave as expected
      due to a kernel bug.

      Instead, the fd is setup to notify via signal delivery. Use the [Signal_delivery]
      module to get notified of this using a signalfd. *)
  val create
    :  Pid.t
    -> addr:int64
    -> single_hit:bool
    -> inherit_behavior:
         [ `Do_not_inherit | `Inherit_and_signal_this_thread of Signal_delivery.t ]
    -> t Or_error.t

  val destroy : t -> unit
  val next_hit : t -> Hit.t option

  (** Returns a waitable fd valid only until [t] is destroyed or GCd *)
  val fd : t -> Core_unix.File_descr.t
end

module All_threads : sig
  type t

  val of_process : Pid.t -> addr:int64 -> single_hit:bool -> t Or_error.t
  val fd : t -> Core_unix.File_descr.t
  val next_hit : t -> Hit.t option
  val destroy : t -> unit
end

module Schedulable : sig
  type ('a, 'prio) t = private
    | Prune
    | Now of 'a
    | Yield of 'prio * (unit -> ('a, 'prio) t)
    | Choice of ('a, 'prio) t * ('a, 'prio) t
end

module State : sig
  type ('a, 'prio, 'state) t = 'state -> ('a * 'state, 'prio) Schedulable.t
end

type ('a, 'err, 'prio, 'state) t = (('a, 'err) result, 'prio, 'state) State.t

(* Monadic boilerplate *)

val return : 'a -> ('a, 'err, 'prio, 'state) t

val bind :
     ('a, 'err, 'prio, 'state) t
  -> ('a -> ('b, 'err, 'prio, 'state) t)
  -> ('b, 'err, 'prio, 'state) t

val ( let* ) :
     ('a, 'err, 'prio, 'state) t
  -> ('a -> ('b, 'err, 'prio, 'state) t)
  -> ('b, 'err, 'prio, 'state) t

val map :
  ('a -> 'b) -> ('a, 'err, 'prio, 'state) t -> ('b, 'err, 'prio, 'state) t

val ( let+ ) :
  ('a, 'err, 'prio, 'state) t -> ('a -> 'b) -> ('b, 'err, 'prio, 'state) t

(* State *)

val modify_state : ('state -> 'state) -> (unit, 'err, 'prio, 'state) t

val set_state : 'state -> (unit, 'err, 'prio, 'state) t

val state : unit -> ('state, 'err, 'prio, 'state) t

(* Symbolic execution *)

val choose :
     ('a, 'err, 'prio, 'state) t
  -> ('a, 'err, 'prio, 'state) t
  -> ('a, 'err, 'prio, 'state) t

val fail : 'err -> ('a, 'err, 'prio, 'state) t

val fork :
     parent:('a, 'err, 'prio, 'state) t
  -> child:'prio * ('a, 'err, 'prio, 'state) t
  -> ('a, 'err, 'prio, 'state) t

val prune : unit -> ('a, 'err, 'prio, 'state) t

val yield : 'prio -> (unit, 'err, 'prio, 'state) t

val run :
     ('a, 'err, 'prio, 'state) t
  -> 'state
  -> (('a, 'err) result * 'state, 'prio) Schedulable.t

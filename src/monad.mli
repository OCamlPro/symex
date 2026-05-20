module Schedulable : sig
  type ('a, 'err, 'prio) t = private
    | Prune
    | Ok of 'a
    | Error of 'err
    | Yield of 'prio * (unit -> ('a, 'err, 'prio) t)
    | Choice of ('a, 'err, 'prio) t * ('a, 'err, 'prio) t
end

type ('a, 'err, 'prio, 'state) t

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

val map_state : ('state -> 'state) -> (unit, 'err, 'prio, 'state) t

val fold_state : ('state -> 'a) -> ('a, 'err, 'prio, 'state) t

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
  -> ('a * 'state, 'err, 'prio) Schedulable.t

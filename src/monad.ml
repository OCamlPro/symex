(** A monad representing computation that can be cooperatively scheduled.
    Computations can yield, and fork (Choice). *)
module Schedulable = struct
  type ('a, 'prio) t =
    | Prune
    | Now of 'a
    | Yield of 'prio * (unit -> ('a, 'prio) t)
    | Choice of ('a, 'prio) t * ('a, 'prio) t

  let[@inline] return (x : 'a) : ('a, _) t = Now x

  let rec bind (x : ('a, 'prio) t) (f : 'a -> ('b, 'prio) t) : ('b, 'prio) t =
    match x with
    | Prune -> Prune
    | Now v -> f v
    | Yield (prio, step) -> Yield (prio, fun () -> bind (step ()) f)
    | Choice (a, b) -> Choice (bind a f, bind b f)

  let[@inline] map (f : 'a -> 'b) (x : ('a, 'prio) t) : ('b, 'prio) t =
    bind x (fun x -> return (f x))

  let[@inline] yield (prio : 'prio) : (unit, 'prio) t =
    Yield (prio, Fun.const (Now ()))

  let[@inline] choose (x : ('a, 'prio) t) (y : ('a, 'prio) t) : ('a, 'prio) t =
    Choice (x, y)

  let prune : (_, _) t = Prune
end

(* Add a notion of State to the Schedulable monad. "Transformer without module functor" style. *)
module State = struct
  type ('a, 'prio, 'state) t = 'state -> ('a * 'state, 'prio) Schedulable.t

  let[@inline] return (x : 'a) : ('a, 'prio, 'state) t =
   fun (state : 'state) -> Schedulable.return (x, state)

  let[@inline] bind (x : ('a, 'prio, 'state) t) (f : 'a -> ('b, 'prio, 'state) t)
    : ('b, 'prio, 'state) t =
   fun (state : 'state) ->
    Schedulable.bind (x state) (fun (x, state) -> f x state)

  let[@inline] map (f : 'a -> 'b) (x : ('a, 'prio, 'state) t) :
    ('b, 'prio, 'state) t =
    bind x (fun x -> return (f x))

  let[@inline] lift (x : ('a, 'prio) Schedulable.t) : ('a, 'prio', 'state) t =
   fun (state : 'state) -> Schedulable.map (fun x -> (x, state)) x

  let[@inline] lift2 (f : 'x -> 'y -> ('a * 'state, 'prio) Schedulable.t)
    (x : 'state -> 'x) (y : 'state -> 'y) : ('a, 'prio, 'state) t =
   fun state -> f (x state) (y state)

  let[@inline] with_state (f : 'state -> 'a * 'state) : ('a, 'prio, 'state) t =
   fun state -> Schedulable.return (f state)

  let[@inline] modify_state (f : 'state -> 'state) : (unit, 'prio, 'state) t =
   fun state -> Schedulable.return ((), f state)
end

(* Add a notion of faillibility to the evaluation. "Transformer without module functor" style. *)
type ('a, 'err, 'prio, 'state) t = (('a, 'err) result, 'prio, 'state) State.t

let[@inline] return (x : 'a) : ('a, 'err, 'prio, 'state) t = State.return (Ok x)

let[@inline] lift (x : ('a, 'prio, 'state) State.t) :
  ('a, 'err, 'prio, 'state) t =
  State.map Result.ok x

let[@inline] bind (x : ('a, 'err, 'prio, 'state) t)
  (f : 'a -> ('b, 'err, 'prio, 'state) t) : ('b, 'err, 'prio, 'state) t =
  State.bind x (function Ok x -> f x | Error _ as e -> State.return e)

let[@inline] ( let* ) (x : ('a, 'err, 'prio, 'state) t) f : _ t = bind x f

let[@inline] map (f : 'a -> 'b) (x : ('a, 'err, 'prio, 'state) t) :
  ('b, 'err, 'prio, 'state) t =
  State.map (fun x -> Result.map f x) x

let[@inline] ( let+ ) (x : ('a, 'err, 'prio, 'state) t) (f : 'a -> 'b) :
  ('b, 'err, 'prio, 'state) t =
  map f x

let[@inline] lift_schedulable (v : ('a, 'prio) Schedulable.t) :
  ('a, 'err, 'prio, 'state) t =
  let v = State.lift v in
  lift v

let[@inline] with_state (f : 'state -> 'a) : ('a, 'err, 'prio, 'state) t =
  let x = State.with_state (fun state -> (f state, state)) in
  lift x

let[@inline] state () : ('state, 'err, 'prio, 'state) t = with_state Fun.id

let[@inline] modify_state (f : 'state -> 'state) : (unit, 'err, 'prio, 'state) t
    =
  lift (State.modify_state f)

let[@inline] set_state (state : 'state) : (unit, 'err, 'prio, 'state) t =
  modify_state (Fun.const state)

(* Create two new branches, they do not yield so the yield should be created manually! *)
let[@inline] choose (x : ('a, 'err, 'prio, 'state) t)
  (y : ('a, 'err, 'prio, 'state) t) : ('a, 'err, 'prio, 'state) t =
  State.lift2 Schedulable.choose x y

(* Yield the current branch (i.e. add it to the work queue so that it gets executed later. )*)
let[@inline] yield (prio : 'prio) : (unit, 'err, 'prio, 'state) t =
  lift_schedulable @@ Schedulable.yield prio

(* Child will be a new branch that immediately yields, and parent will execute directly without yielding. *)
let[@inline] fork ~(parent : ('a, 'err, 'prio, 'state) t)
  ~(child : 'prio * ('a, 'err, 'prio, 'state) t) : ('a, 'err, 'prio, 'state) t =
  let prio, child = child in
  let child = bind (yield prio) (fun () -> child) in
  choose parent child

let[@inline] prune () : ('a, 'err, 'prio, 'state) t =
  lift_schedulable Schedulable.prune

let[@inline] fail (err : 'err) : ('a, 'err, 'prio, 'state) t =
  State.return (Error err)

let run (f : ('a, 'err, 'prio, 'state) t) (state : 'state) :
  (('a, 'err) result * 'state, 'prio) Schedulable.t =
  f state

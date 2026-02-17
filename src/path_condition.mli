(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val pp : t Fmt.t

val empty : t

(* Add an expression to the PC. *)
val add : Smtml.Typed.Bool.t -> t -> t

(* You should only use this after checking the boolean expression makes the PC SAT. If you add it without checking, it may lead to bad results. The recommended way to go is : 1. call add on old_pc and get the new_pc 2. check that new_pc is SAT 3. if it is, call add_already_checked on old_pc and keep this version which is more optimized. *)
val add_already_checked : Smtml.Typed.Bool.t -> t -> t

(* CAUTION: this must only be called after the symbol has been added to the path condition *)
val slice_on_symbol : Smtml.Symbol.t -> t -> Smtml.Expr.Set.t

(* CAUTION: this must only be called after the condition added to the path condition with `add` *)
val slice_on_condition : Smtml.Typed.Bool.t -> t -> Smtml.Expr.Set.t

val slice : t -> Smtml.Expr.Set.t list

(* This is only a way to shortcut the SMT call, as sometimes we can detect the PC is unsat. If this function returns false, it does not mean it is SAT or UNSAT. *)
val is_unsat : t -> bool

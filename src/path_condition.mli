(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

type t

val pp : t Fmt.t

val empty : t

(* You should only use this after checking the condition is SAT in the context of the path condition. If you add it without checking, it may lead to wrong results. *)
val add_checked_sat_condition : Smtml.Typed.Bool.t -> t -> t

(* Get all the slices that are related to the condition given in argument. The condition is not added to the result. *)
val slice_on_new_condition : Smtml.Typed.Bool.t -> t -> Smtml.Expr.Set.t

(* Get the slice for a symbol. Return an empty set if the symbol is not part of the path condition. *)
val slice_on_symbol : Smtml.Symbol.t -> t -> Smtml.Expr.Set.t

(* Get all slices of the path condition as a list. *)
val to_list : t -> Smtml.Expr.Set.t list

(* Return a map of known equalities from symbols to values. *)
val get_known_equalities : t -> Smtml.Value.t Smtml.Symbol.Map.t

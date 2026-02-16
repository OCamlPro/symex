(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = struct
  include Union_find.Make (Smtml.Symbol)

  type nonrec t = Smtml.Expr.Set.t t

  let pp : t Fmt.t = fun ppf union_find -> pp Smtml.Expr.Set.pp ppf union_find
end

module Equalities : sig
  type t

  val empty : t

  val pp : t Fmt.t
end = struct
  type t = Smtml.Value.t Smtml.Symbol.Map.t

  let empty = Smtml.Symbol.Map.empty

  let pp ppf v =
    Fmt.pf ppf "@[<hov 1>{%a}@]"
      (Fmt.iter_bindings
         ~sep:(fun ppf () -> Fmt.pf ppf ",@ ")
         Smtml.Symbol.Map.iter
         (fun ppf (k, v) ->
           Fmt.pf ppf "%a = %a" Smtml.Symbol.pp k Smtml.Value.pp v ) )
      v
end

type t =
  { union_find : Union_find.t
  ; equalities : Equalities.t
  }

let pp : t Fmt.t =
 fun ppf { union_find; equalities } ->
  Fmt.pf ppf "union find:@\n  @[<v>%a@]@\nequalities:@\n  @[<v>%a@]"
    Union_find.pp union_find Equalities.pp equalities

let empty : t =
  let union_find = Union_find.empty in
  let equalities = Equalities.empty in
  { union_find; equalities }

let add_one (condition : Smtml.Expr.t) (pc : t) : t =
  match Smtml.Expr.get_symbols [ condition ] with
  | hd :: tl ->
    (* We add the first symbol to the UF *)
    let union_find =
      let c = Smtml.Expr.Set.singleton condition in
      Union_find.add ~merge:Smtml.Expr.Set.union hd c pc.union_find
    in
    (* We union-ize all symbols together, starting with the first one that has already been added *)
    let union_find, _last_sym =
      List.fold_left
        (fun (union_find, last_sym) sym ->
          ( Union_find.union ~merge:Smtml.Expr.Set.union last_sym sym union_find
          , sym ) )
        (union_find, hd) tl
    in
    let equalities = pc.equalities in
    { union_find; equalities }
  | [] ->
    (* It means smtml did not properly simplified an expression! *)
    assert false

let add (condition : Smtml.Typed.Bool.t) (pc : t) : t =
  (* we start by splitting the condition ((P & Q) & R) into a set {P; Q; R} before adding each of P, Q and R into the UF data structure, this way we maximize the independence of the PC *)
  let splitted_condition = Smtml.Typed.Bool.split_conjunctions condition in
  Smtml.Expr.Set.fold add_one splitted_condition pc

(* Get all sub conditions of the path condition as a list of independent sets of constraints. *)
let slice (pc : t) = Union_find.explode pc.union_find

(* Return the set of constraints from [pc] that are relevant for [sym]. *)
let slice_on_symbol (sym : Smtml.Symbol.t) (pc : t) : Smtml.Expr.Set.t =
  match Union_find.find_opt sym pc.union_find with
  | Some s -> s
  | None ->
    (* if there is a symbol, it should have been added to the union-find structure before, otherwise it means `add` has not been called properly before *)
    assert false

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice_on_condition (c : Smtml.Typed.Bool.t) (pc : t) : Smtml.Expr.Set.t =
  match Smtml.Typed.get_symbols [ c ] with
  | sym0 :: _tl ->
    (* we need only the first symbol as all the others should have been merged with it *)
    slice_on_symbol sym0 pc
  | [] ->
    (* It means smtml did not properly simplified a expression! *)
    assert false

(* SPDX-License-Identifier: AGPL-3.0-or-later *)
(* Copyright © 2021-2024 OCamlPro *)
(* Written by the Owi programmers *)

module Union_find = struct
  include Union_find.Make (Smtml.Symbol)

  type nonrec t = Smtml.Expr.Set.t t

  let pp : t Fmt.t = fun ppf union_find -> pp Smtml.Expr.Set.pp ppf union_find
end

module Equalities : sig
  type t = Smtml.Value.t Smtml.Symbol.Map.t

  val pp : t Fmt.t
end = struct
  type t = Smtml.Value.t Smtml.Symbol.Map.t

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
  Fmt.pf ppf "union find:@\n  @[<v>%a@]@\nequalities:@\n  @[<v>%a@]@\n"
    Union_find.pp union_find Equalities.pp equalities

let empty : t =
  let union_find = Union_find.empty in
  let equalities = Smtml.Symbol.Map.empty in
  { union_find; equalities }

let rec add_one_equality symbol value (pc : t) : t =
  let equalities = Smtml.Symbol.Map.add symbol value pc.equalities in
  match Union_find.find_and_remove_opt symbol pc.union_find with
  | Some (set, union_find) ->
    (* propagate back the equality in the union find *)
    let pc = { union_find; equalities } in
    Smtml.Expr.Set.fold add_one_constraint set pc
  | None -> { pc with equalities }

and add_one_constraint (condition : Smtml.Expr.t) (pc : t) : t =
  (* we start by simplifying the constraint by substituting already known equalities *)
  let condition = Smtml.Expr.inline_symbol_values pc.equalities condition in
  let condition = Smtml.Expr.simplify condition in

  let pc, shortcut =
    match Smtml.Expr.view condition with
    (* if the condition is of the form e1 = e2 *)
    | Relop (_, Smtml.Ty.Relop.Eq, e1, e2) -> begin
      match (Smtml.Expr.view e1, Smtml.Expr.view e2) with
      (* it has the form: symbol = value *)
      | Smtml.Expr.Symbol symbol, Val value | Val value, Symbol symbol -> begin
        match Smtml.Symbol.Map.find_opt symbol pc.equalities with
        | None ->
          (* we don't have an equality for s=v so we add it *)
          (add_one_equality symbol value pc, true)
        | Some value' ->
          (* that would mean the PC is unsat which is illegal! *)
          assert (Smtml.Eval.relop symbol.ty Eq value value');
          (* we discovered an already known equality, nothing to do *)
          (pc, true)
      end
      | _ -> (pc, false)
    end
    | _ -> (pc, false)
  in

  if shortcut then pc
  else
    match Smtml.Expr.view condition with
    | Val True ->
      (* no need to change anything, the condition is a tautology *)
      pc
    | Val False ->
      (* the PC is unsat *)
      assert false
    | _ -> begin
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
              ( Union_find.union ~merge:Smtml.Expr.Set.union last_sym sym
                  union_find
              , sym ) )
            (union_find, hd) tl
        in
        { pc with union_find }
      | [] ->
        (* either it is a boolean value (because it is a condition) and was matched before, either it's not a value and should have at least one symbol!  *)
        assert false
    end

let add_checked_sat_condition (condition : Smtml.Typed.Bool.t) (pc : t) : t =
  (* we start by splitting the condition ((P & Q) & R) into a set {P; Q; R} before adding each of P, Q and R into the UF data structure, this way we maximize the independence of the PC *)
  let splitted_condition = Smtml.Typed.Bool.split_conjunctions condition in
  Smtml.Expr.Set.fold add_one_constraint splitted_condition pc

(* Get all sub conditions of the path condition as a list of independent sets of constraints. *)
let to_list (pc : t) =
  let uf = Union_find.explode pc.union_find in
  (* add equalities too *)
  Smtml.Symbol.Map.fold
    (fun symbol value acc ->
      let eq =
        Smtml.Expr.Bool.equal (Smtml.Expr.symbol symbol)
          (Smtml.Expr.value value)
        |> Smtml.Expr.Set.singleton
      in
      eq :: acc )
    pc.equalities uf

(* Return the set of constraints from [pc] that are relevant for [sym]. *)
let slice_on_symbol (sym : Smtml.Symbol.t) (pc : t) : Smtml.Expr.Set.t =
  match Union_find.find_opt sym pc.union_find with
  | Some s -> s
  | None -> (
    match Smtml.Symbol.Map.find_opt sym pc.equalities with
    | Some value ->
      let eq =
        Smtml.Expr.Bool.equal (Smtml.Expr.symbol sym) (Smtml.Expr.value value)
      in
      Smtml.Expr.Set.singleton eq
    | None -> Smtml.Expr.Set.empty )

(* Return the set of constraints from [pc] that are relevant for [c]. *)
let slice_on_new_condition (c : Smtml.Typed.Bool.t) (pc : t) : Smtml.Expr.Set.t
    =
  let symbols = Smtml.Typed.get_symbols [ c ] in
  List.fold_left
    (fun set symbol ->
      let slice = slice_on_symbol symbol pc in
      Smtml.Expr.Set.union set slice )
    Smtml.Expr.Set.empty symbols

let get_known_equalities (pc : t) : Smtml.Value.t Smtml.Symbol.Map.t =
  pc.equalities

(* utility functions *)

let i32_const n = Smtml.Typed.Bitv32.of_int32 n

let sym_i32 name = Smtml.Symbol.make (Smtml.Ty.Ty_bitv 32) name

let i32_sym name = Smtml.Typed.Bitv32.symbol @@ sym_i32 name

let eq e1 e2 = Smtml.Typed.Bool.eq e1 e2

let print pc = Fmt.str "%a" Symex.Path_condition.pp pc

(* basic tests *)

let test_print_pc_empty () : unit =
  let pc = Symex.Path_condition.empty |> print in
  Alcotest.(check string)
    "same string"
    "union find:\n\
    \  ((aliases_of_canonicals {}) (payload_of_canonicals {}))\n\
     equalities:\n\
    \  {}\n\
     is_unsat: false"
    pc

let test_print_pc_one () : unit =
  let pc =
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (i32_sym "x0") (i32_const 42l))
    |> print
  in
  Alcotest.(check string)
    "same string"
    "union find:\n\
    \  ((aliases_of_canonicals {}) (payload_of_canonicals {}))\n\
     equalities:\n\
    \  {x0 = 42}\n\
     is_unsat: false"
    pc

let test_print_pc_two () : unit =
  let pc =
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (i32_sym "x0") (i32_const 42l))
    |> Symex.Path_condition.add (eq (i32_sym "y0") (i32_const 56l))
    |> print
  in
  Alcotest.(check string)
    "same string"
    "union find:\n\
    \  ((aliases_of_canonicals {}) (payload_of_canonicals {}))\n\
     equalities:\n\
    \  {x0 = 42, y0 = 56}\n\
     is_unsat: false"
    pc

let test_print_pc_one_two_sym () : unit =
  let pc =
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (i32_sym "x0") (i32_sym "y0"))
    |> print
  in
  Alcotest.(check string)
    "same string"
    "union find:\n\
    \  ((aliases_of_canonicals {(x0 {y0})})\n\
    \   (payload_of_canonicals {(x0 (bool.eq x0 y0))}))\n\
     equalities:\n\
    \  {}\n\
     is_unsat: false"
    pc

let basics_suite =
  let open Alcotest in
  ( "basics"
  , [ test_case "pc_empty" `Quick test_print_pc_empty
    ; test_case "pc_one" `Quick test_print_pc_one
    ; test_case "pc_two" `Quick test_print_pc_two
    ; test_case "pc_one_two_sym" `Quick test_print_pc_one_two_sym
    ] )

(* `slice_on_symbol` *)

let test_slice_on_symbol_single () =
  let slice =
    let x0 = sym_i32 "x0" in
    Symex.Path_condition.empty
    |> Symex.Path_condition.add
         (eq (Smtml.Typed.Bitv32.symbol x0) (i32_const 42l))
    |> Symex.Path_condition.slice_on_symbol x0
  in
  Alcotest.(check int)
    "one constraint in slice" 1
    (Smtml.Expr.Set.cardinal slice)

let test_slice_on_symbol_irrelevant () =
  let slice =
    let y0 = sym_i32 "y0" in
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (i32_sym "x0") (i32_const 42l))
    |> Symex.Path_condition.add
         (eq (Smtml.Typed.Bitv32.symbol y0) (i32_const 56l))
    |> Symex.Path_condition.slice_on_symbol y0
  in
  Alcotest.(check int) "no constraints for y0" 1 (Smtml.Expr.Set.cardinal slice)

let test_slice_on_symbol_transitive () =
  let slice =
    let x0 = sym_i32 "x0" in
    let y0 = i32_sym "y0" in
    let z0 = i32_sym "z0" in
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (Smtml.Typed.Bitv32.symbol x0) y0)
    |> Symex.Path_condition.add (eq y0 z0)
    |> Symex.Path_condition.slice_on_symbol x0
  in
  Alcotest.(check int)
    "transitive constraints included" 2
    (Smtml.Expr.Set.cardinal slice)

let slice_on_symbol_suite =
  let open Alcotest in
  ( "slice_on_symbol"
  , [ test_case "single" `Quick test_slice_on_symbol_single
    ; test_case "irrelevant" `Quick test_slice_on_symbol_irrelevant
    ; test_case "transitive" `Quick test_slice_on_symbol_transitive
    ] )

(* `slice_on_condition` *)

let test_slice_on_condition_basic () =
  let slice =
    let c1 = eq (i32_sym "x0") (i32_const 10l) in
    Symex.Path_condition.empty
    |> Symex.Path_condition.add c1
    |> Symex.Path_condition.slice_on_condition c1
  in
  Alcotest.(check int) "condition slice size" 1 (Smtml.Expr.Set.cardinal slice)

let test_slice_on_condition_transitive () =
  let slice =
    let x0 = i32_sym "x0" in
    let y0 = i32_sym "y0" in
    let z0 = i32_sym "z0" in
    let c1 = eq x0 y0 in
    let c2 = eq y0 z0 in
    Symex.Path_condition.empty
    |> Symex.Path_condition.add c1
    |> Symex.Path_condition.add c2
    |> Symex.Path_condition.slice_on_condition c1
  in
  Alcotest.(check int) "transitive slice size" 2 (Smtml.Expr.Set.cardinal slice)

let slice_on_condition_suite =
  let open Alcotest in
  ( "slice_on_condition"
  , [ test_case "basic" `Quick test_slice_on_condition_basic
    ; test_case "transitive" `Quick test_slice_on_condition_transitive
    ] )

(* `slice` *)

let test_slice_empty () =
  let slices = Symex.Path_condition.empty |> Symex.Path_condition.slice in
  Alcotest.(check int) "no slices" 0 (List.length slices)

let test_slice_independent () =
  let slices =
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (i32_sym "x0") (i32_const 1l))
    |> Symex.Path_condition.add (eq (i32_sym "y0") (i32_const 2l))
    |> Symex.Path_condition.slice
  in
  Alcotest.(check int) "two independent slices" 2 (List.length slices)

let test_slice_connected () =
  let slices =
    Symex.Path_condition.empty
    |> Symex.Path_condition.add (eq (i32_sym "x0") (i32_sym "y0"))
    |> Symex.Path_condition.add (eq (i32_sym "y0") (i32_sym "z0"))
    |> Symex.Path_condition.slice
  in
  Alcotest.(check int) "one connected slice" 1 (List.length slices)

let slice_suite =
  let open Alcotest in
  ( "slice"
  , [ test_case "empty" `Quick test_slice_empty
    ; test_case "independent" `Quick test_slice_independent
    ; test_case "connected" `Quick test_slice_connected
    ] )

(* run tests *)
let () =
  let open Alcotest in
  run "path-condition"
    [ basics_suite
    ; slice_on_symbol_suite
    ; slice_on_condition_suite
    ; slice_suite
    ]

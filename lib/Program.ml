(* These code forms a very basic top-level forms *)
exception TODO

module S = Syntax
module V = Value
module Eq = Equality
module E = Eval

type definition =
  (* let x : T = t *)
  | Let of string * S.term * S.term
  (* assume x : T *)
  | Assume of string * S.term
  (* check term : T *)
  | Check of S.term * S.term

type program = definition list

let rec depth : V.env -> int =
 fun env -> match env with Emp -> 0 | Extend (env, _) -> depth env

let rec check_def : V.env -> definition -> V.env =
 fun env d ->
  match d with
  (* infer type from term *)
  (* equate_ty ty and ty' *)
  (* bind toplevel context with x *)
  | Let (_, _, _) -> raise TODO
  | Assume (_, ty) ->
      let ty' = E.eval env ty in
      V.Extend (env, V.Stuck (V.Var (Lvl (depth env)), ty'))
  | Check (tm, ty) ->
      let ty' = infer_ty (E.eval env tm) in
      print_string (V.v_to_str ty');
      print_newline ();
      print_string (V.v_to_str (E.eval env ty));
      print_newline ();
      Eq.equate_ty 0 (E.eval env ty) ty';
      env

and infer_ty : V.value -> V.value =
 fun v ->
  match v with
  | V.Stuck (_, ty) -> ty
  | V.Type i -> V.Type (i + 1)
  | _ -> raise TODO

let check_program : program -> bool =
 fun p ->
  Printexc.register_printer (function
    | Eq.Unequal (t1, t2) ->
        Some
          (Printf.sprintf "TYPE `%s` not equals to `%s`" (V.v_to_str t1)
             (V.v_to_str t2))
    | Eq.UnequalV (t1, t2) ->
        Some
          (Printf.sprintf "VALUE `%s` not equals to `%s`" (V.v_to_str t1)
             (V.v_to_str t2))
    | Eq.UnequalS (t1, t2) ->
        Some
          (Printf.sprintf "STUCK `%s` not equals to `%s`" (V.stuck_to_str t1)
             (V.stuck_to_str t2))
    | _ -> None);
  let _ = List.fold_left check_def V.Emp p in
  true

let%test _ =
  check_program
    [
      (* Nat : Type 0 *)
      Assume ("Nat", S.Type 0);
      (* zero : Nat *)
      Assume ("zero", S.Var (Idx 0));
      Check (S.Var (Idx 1), S.Type 0);
      Check (S.Var (Idx 0), S.Var (Idx 1));
    ]

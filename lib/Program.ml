(* These code forms a very basic top-level forms *)
exception TODO
exception InferLambda
open List
module S = Syntax
module V = Value
module Eq = Equality
module E = Eval

type top_def =
  (* let x : T = t *)
  | Let of string * S.term * S.term
  (* assume x : T *)
  | Assume of string * S.term
  (* check term : T *)
  | Check of S.term * S.term

type program = top_def list

let rec check_program : V.env -> V.ctx -> top_def list -> unit =
 fun env ctx d ->
  match d with
  | [] -> ()
  (* infer type from term *)
  (* equate_ty ty and ty' *)
  (* bind toplevel context with x *)
  | Let (_x, a, t) :: rest ->
    let (t, a') = infer env ctx t in
    Eq.equate_ty (length ctx) (E.eval env a) a';
    check_program (E.eval env t :: env) (a' :: ctx) rest
  | Assume (_x, ty) :: rest ->
    let t = E.eval env ty in
    check_program (V.Stuck (V.Var (Lvl (length env)), t) :: env) (t :: ctx) rest
  | Check (tm, ty) :: rest ->
      let (_tm, ty') = infer env ctx tm in
      let ty = E.eval env ty in
      print_string (V.v_to_str ty');
      print_newline ();
      print_string (V.v_to_str ty);
      print_newline ();
      Eq.equate_ty 0 ty ty';
      check_program env ctx rest

and infer : V.env -> V.ctx -> S.term -> (S.term * V.value) =
 fun env ctx tm ->
  match tm with 
  | Var (Idx x) -> (Var (Idx x), E.proj ctx x)
  | Pi (_, _) -> raise TODO
  | Sg (a, B fam) ->
    let a = check env ctx a (V.Type 0) in 
    let a' = E.eval env a in
    let _fiber = E.eval (V.Stuck (V.Var (Lvl (length env)), a') :: env) fam in
    (Sg (a, B fam), V.Type 0)
  (* cannot infer a lambda *)
  | Lam _ -> raise InferLambda
  | App (_, _) -> raise TODO
  | Pair (_, _) -> raise TODO
  | Fst _ -> raise TODO
  | Snd _ -> raise TODO
  (* universe of type *)
  | Type i -> (Type i, V.Type (i+1))

and check : V.env -> V.ctx -> S.term -> V.value -> S.term =
  fun env ctx tm ty ->
    match (tm, ty) with
    | t, expected -> 
      let (t', inferred) = infer env ctx t in
      Eq.equate_ty (length ctx) inferred expected;
       t'
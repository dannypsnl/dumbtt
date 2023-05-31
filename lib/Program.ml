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
and context = { env : V.env; ctx : (string * V.vty) list; lvl : V.var }

let bind : context -> string -> V.vty -> context =
 fun ctx name typ ->
  let curLvl = V.unLvl ctx.lvl in
  let v = V.Stuck (V.Var (V.Lvl curLvl), typ) in
  let env = v :: ctx.env in
  let c = (name, typ) :: ctx.ctx in
  { env; ctx = c; lvl = V.Lvl (curLvl + 1) }

and define : context -> string -> V.vty -> V.value -> context =
 fun ctx name typ value ->
  let curLvl = V.unLvl ctx.lvl in
  let env = value :: ctx.env in
  let c = (name, typ) :: ctx.ctx in
  { env; ctx = c; lvl = V.Lvl (curLvl + 1) }

let rec check_program : context -> top_def list -> unit =
 fun ctx d ->
  match d with
  | [] -> ()
  (* infer type from term *)
  (* equate_ty ty and ty' *)
  (* bind toplevel context with x *)
  | Let (x, a, t) :: rest ->
      let a = check ctx a (V.Type 0) in
      let a = E.eval ctx.env a in
      let t = check ctx t a in
      let t = E.eval ctx.env t in
      let ctx = define ctx x a t in
      check_program ctx rest
  | Assume (x, ty) :: rest ->
      let ty = E.eval ctx.env ty in
      let ctx = bind ctx x ty in
      check_program ctx rest
  | Check (tm, ty) :: rest ->
      let _tm, ty' = infer ctx tm in
      let ty = E.eval ctx.env ty in
      print_string (V.v_to_str ty');
      print_newline ();
      print_string (V.v_to_str ty);
      print_newline ();
      Eq.equate_ty 0 ty ty';
      check_program ctx rest

and infer : context -> S.term -> S.term * V.vty =
 fun ctx tm ->
  match tm with
  | Var (Idx x) -> (Var (Idx x), E.proj ctx.env x)
  | Pi (_, _) -> raise TODO
  | Sg (a, B fam) ->
      let a = check ctx a (V.Type 0) in
      let a' = E.eval ctx.env a in
      let _fiber = E.eval (a' :: ctx.env) fam in
      (Sg (a, B fam), V.Type 0)
  (* cannot infer a lambda *)
  | Lam _ -> raise InferLambda
  | App (_, _) -> raise TODO
  | Pair (_, _) -> raise TODO
  | Fst _ -> raise TODO
  | Snd _ -> raise TODO
  (* universe of type *)
  | Type i -> (Type i, V.Type (i + 1))

and check : context -> S.term -> V.value -> S.term =
 fun ctx tm ty ->
  match (tm, ty) with
  | t, expected ->
      let t', inferred = infer ctx t in
      Eq.equate_ty (V.unLvl ctx.lvl) inferred expected;
      t'

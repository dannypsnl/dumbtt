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
and ctx = (string * V.vty) list
and context = { env : V.env; ctx : ctx; lvl : V.var }

let rec ctx_proj : ctx -> int -> V.vty =
 fun c i ->
  match c with
  | [] -> failwith "cannot find variable"
  | (_, typ) :: c' ->
      if i == 0 then typ
      else if i > 0 then ctx_proj c' (i - 1)
      else failwith "bug: the indices shouldn't smaller than 0"

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
  (* cannot infer a lambda *)
  | Lam _ -> raise InferLambda
  (* inferrable *)
  | Var (Idx x) -> (Var (Idx x), ctx_proj ctx.ctx x)
  | Pi (_, _) -> raise TODO
  | Sg (a, B fam) ->
      let a = check ctx a (V.Type 0) in
      let a' = E.eval ctx.env a in
      let _fiber = E.eval (a' :: ctx.env) fam in
      (Sg (a, B fam), V.Type 0)
  | App (t, u) -> (
      let t, tty = infer ctx t in
      match tty with
      | Pi (a, clos) ->
          let u = check ctx u a in
          let u' = E.eval ctx.env u in
          let b = E.clos_app clos u' in
          (App (t, u), b)
      | _ -> raise TODO)
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

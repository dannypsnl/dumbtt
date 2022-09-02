(* These code forms a very basic top-level forms *)
exception TODO

module S = Syntax
module V = Value
module Eq = Equality
module E = Eval

type definition = (* let x : T = t *)
  | Let of string * S.term * S.term

type program =
  | Empty
  | T of S.term * program
  (* definition *)
  | D of definition * program

let rec def : V.env -> definition -> V.env =
 fun env d ->
  match d with
  (* infer type from term *)
  (* equate_ty ty and ty' *)
  (* bind toplevel context with x *)
  | Let (_, ty, tm) ->
      let ty' = infer_ty env tm in
      Eq.equate_ty 0 (E.eval env ty) ty';
      V.Extend (env, E.eval env tm)

and infer_ty : V.env -> S.term -> V.value =
 fun env tm ->
  match tm with
  | S.Type i -> V.Type (i + 1)
  (* Pi x:a.b *)
  | S.Pi (a, B b) ->
      let (V.Type k1) = infer_ty env a in
      let (V.Type k2) = infer_ty (V.Extend (env, a)) b in
      V.Type (max k1 k2)
  | S.Lam (B body) ->
      let param_ty = V.Type 0 in
      V.Pi (param_ty, C { binder = B body; env })
  | _ -> raise TODO

let%test _ = T (Var (Idx 1), Empty) = Empty

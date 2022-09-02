type var = Lvl of int

type value =
  | Pi of value * closure
  | Sg of value * closure
  | Lam of closure
  | Pair of value * value
  | Type of int
  | Stuck of stuck * value (* the `value` here is the type of `stuck` *)

and stuck =
  (* or known as neutral *)
  | Var of var
  | Fst of stuck
  | Snd of stuck
  | App of { fn : stuck; arg : value; base : value }

and closure =
  (* closure bind a term up to value place *)
  (* and with a captured environment *)
  | C of { binder : Syntax.term Syntax.binder; env : env }

(* Gamma, x : A |- binder : B *)
(* ------------------------------------- *)
(* env : foreach z : C in Gamma, a value *)
and env =
  | Emp
  (* empty *)
  | Extend of env * value

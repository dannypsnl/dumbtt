module S = Syntax

type var = Lvl of int

type value =
  | Pi of value * closure
  | Sg of value * closure
  | Lam of closure
  | Pair of value * value
  | Type of int
  | Stuck of stuck * value (* the `value` here is the type of `stuck` *)

and vty = value

and stuck =
  (* or known as neutral *)
  | Var of var
  | Fst of stuck
  | Snd of stuck
  | App of { fn : stuck; arg : value; base : value }

and closure =
  (* closure bind a term up to value place *)
  (* and with a captured environment *)
  | Clos of { binder : Syntax.term Syntax.binder; env : env }

and env = value list

let rec unLvl : var -> int = fun v -> match v with Lvl l -> l

and lvl2Ix : var -> var -> S.var =
 fun cur prev -> Idx (unLvl cur - unLvl prev - 1)

let rec v_to_str : value -> string =
 fun v ->
  match v with
  | Pi (_, _) -> "Pi"
  | Sg (_, _) -> "Sigma"
  | Lam _ -> "Lambda"
  | Pair (l, r) -> v_to_str l ^ "x" ^ v_to_str r
  | Type i -> "Type_" ^ string_of_int i
  | Stuck (s, ty) -> stuck_to_str s ^ " : " ^ v_to_str ty

and stuck_to_str : stuck -> string =
 fun s ->
  match s with
  | Var (Lvl i) -> "#" ^ string_of_int i
  | Fst s -> stuck_to_str s ^ ".1"
  | Snd s -> stuck_to_str s ^ ".2"
  | App { fn; arg; base } ->
      stuck_to_str fn ^ "(" ^ v_to_str arg ^ ")" ^ " : " ^ v_to_str base

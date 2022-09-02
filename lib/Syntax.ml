type var = Idx of int
type 'a binder = B of 'a

type term =
  | Var of var
  | Pi of term * term binder
  | Sg of term * term binder
  | Lam of term binder
  | App of term * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  (* universe of type *)
  | Type of int

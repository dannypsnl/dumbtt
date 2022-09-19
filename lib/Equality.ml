module V = Value
module S = Syntax

exception Unequal of V.value * V.value
exception UnequalV of V.value * V.value
exception UnequalS of V.stuck * V.stuck

(* G |- A == B type *)
let rec equate_ty : int -> V.value -> V.value -> unit =
 (* len of Context *)
 fun len tp0 tp1 ->
  match (tp0, tp1) with
  | ( V.Pi (base0, V.C { binder = B fam0; env = env0 }),
      V.Pi (base1, V.C { binder = B fam1; env = env1 }) ) ->
      equate_ty len base0 base1;
      let var : V.value = V.Stuck (V.Var (V.Lvl len), base0) in
      let fiber0 = Eval.eval (V.Extend (env0, var)) fam0 in
      let fiber1 = Eval.eval (V.Extend (env1, var)) fam1 in
      equate_ty (len + 1) fiber0 fiber1
  | ( V.Sg (base0, V.C { binder = B fam0; env = env0 }),
      V.Sg (base1, V.C { binder = B fam1; env = env1 }) ) ->
      equate_ty len base0 base1;
      let var : V.value = V.Stuck (V.Var (V.Lvl len), base0) in
      let fiber0 = Eval.eval (V.Extend (env0, var)) fam0 in
      let fiber1 = Eval.eval (V.Extend (env1, var)) fam1 in
      equate_ty (len + 1) fiber0 fiber1
  | V.Type l0, V.Type l1 -> if l0 = l1 then () else raise (Unequal (tp0, tp1))
  | V.Stuck (s0, t0), V.Stuck (s1, t1) ->
      equate_ty 0 t0 t1;
      equate_stuck 0 s0 s1;
      ()
  | _ -> raise (Unequal (tp0, tp1))

and equate : int -> V.value -> V.value -> V.value -> unit =
 fun len tp val0 val1 ->
  match tp with
  | V.Pi (base, V.C { binder = B fam; env }) ->
      let var = V.Stuck (V.Var (V.Lvl len), base) in
      let result0 = Eval.app val0 var in
      let result1 = Eval.app val1 var in
      let fiber = Eval.eval (V.Extend (env, var)) fam in
      equate (len + 1) fiber result0 result1
  | V.Sg (base, V.C { binder = B fam; env }) ->
      let fst0 = Eval.fst val0 in
      let fst1 = Eval.fst val1 in
      equate len base fst0 fst1;
      let snd0 = Eval.snd val0 in
      let snd1 = Eval.snd val1 in
      let fiber = Eval.eval (V.Extend (env, fst1)) fam in
      equate len fiber snd0 snd1
  | _ -> (
      match (val0, val1) with
      | V.Stuck (stuck0, tp0), V.Stuck (stuck1, tp1) ->
          equate_ty len tp0 tp1;
          equate_stuck len stuck0 stuck1
      | _ -> raise (UnequalV (val0, val1)))

and equate_stuck : int -> V.stuck -> V.stuck -> unit =
 fun len s0 s1 ->
  match (s0, s1) with
  | V.Var l0, V.Var l1 -> if l0 = l1 then () else raise (UnequalS (s0, s1))
  | V.Fst s0, V.Fst s1 -> equate_stuck len s0 s1
  | V.Snd s0, V.Fst s1 -> equate_stuck len s0 s1
  | ( V.App { fn = fn0; arg = arg0; base = base0 },
      V.App { fn = fn1; arg = arg1; base = base1 } ) ->
      equate_stuck len fn0 fn1;
      equate_ty len base0 base1;
      equate len base0 arg0 arg1
  | _ -> raise (UnequalS (s0, s1))

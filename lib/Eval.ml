exception TODO

module V = Value
module S = Syntax

let rec proj : V.env -> int -> V.value =
 fun env i ->
  match env with
  | Emp -> failwith "cannot find variable"
  | Extend (env', v) ->
      if i == 0 then v
      else if i > 0 then proj env' (i - 1)
      else failwith "bug: the indices shouldn't smaller than 0"

let rec eval : V.env -> S.term -> V.value =
 fun env term ->
  match term with
  | S.Var (S.Idx i) -> proj env i
  | S.Pi (base, fam) ->
      let vbase = eval env base in
      let cfam = V.C { binder = fam; env } in
      V.Pi (vbase, cfam)
  | S.Sg (base, fam) ->
      let vbase = eval env base in
      let cfam = V.C { binder = fam; env } in
      V.Sg (vbase, cfam)
  | S.Lam binder -> V.Lam (V.C { binder; env })
  | S.App (fn, arg) ->
      let vfn = eval env fn in
      let varg = eval env arg in
      app vfn varg
  | S.Pair (l, r) ->
      let vl = eval env l in
      let vr = eval env r in
      V.Pair (vl, vr)
  | S.Fst pair -> fst (eval env pair)
  | S.Snd pair -> snd (eval env pair)

and fst : V.value -> V.value =
 fun vpair ->
  match vpair with
  | V.Pair (l, _) -> l
  | V.Stuck (stuck, tp) -> (
      match tp with
      | V.Sg (base, _) -> V.Stuck (V.Fst stuck, base)
      | _ -> failwith "you cannot do projection on non-sigma")
  | _ -> failwith "you cannot do projection on non-pair"

and snd : V.value -> V.value =
 fun vpair ->
  match vpair with
  | V.Pair (_, r) -> r
  | V.Stuck (stuck, tp) -> (
      match tp with
      | V.Sg (_, V.C { binder = B fam; env }) ->
          let u = fst vpair in
          let fiber = eval (V.Extend (env, u)) fam in
          V.Stuck (V.Snd stuck, fiber)
      | _ -> failwith "you cannot do projection on non-sigma")
  | _ -> failwith "you cannot do projection on non-pair"

and app : V.value -> V.value -> V.value =
 fun vfn varg ->
  match vfn with
  | V.Lam (V.C { binder = B term; env }) ->
      let env' = V.Extend (env, varg) in
      eval env' term
  | V.Stuck (stuck, tp) -> (
      match tp with
      | V.Pi (base, V.C { binder = B fam; env }) ->
          (* M : Pi(x : A).B[x] *)
          (* ------------------ *)
          (* M(N) : B[N] *)
          let stuck = V.App { fn = stuck; arg = varg; base } in
          let fiber = eval (V.Extend (env, varg)) fam in
          V.Stuck (stuck, fiber)
      | _ -> failwith "cannot apply on non-pi")
  | _ -> failwith "cannot apply on non-lambda"

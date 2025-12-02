(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
  | TyTuple of ty list
  | TyList of ty
  | TyRcd of (string * ty) list
;;

type term =
    TmTrue
  | TmFalse
  | TmIf of term * term * term
  | TmZero
  | TmSucc of term
  | TmPred of term
  | TmIsZero of term
  | TmVar of string
  | TmAbs of string * ty * term
  | TmApp of term * term
  | TmLetIn of string * term * term
  | TmFix of term
  | TmString of string
  | TmConcat of term * term
  | TmTuple of term list
  | TmRcd of (string * term) list
  | TmProj of term * int
  | TmProjRcd of term * string
  | TmNil of ty
  | TmCons of term * term
  | TmHead of term
  | TmTail of term
  | TmIsNil of term
;;

type command =
    Eval of term
  | Bind of string * term
  | Quit
;;

type binding =
  TyBind of ty
| TyTmBind of (ty * term)
;;

type context = (string * binding) list;;

(* CONTEXT MANAGEMENT *)

let emptyctx = [];;

let addtbinding ctx s ty = (s, TyBind ty) :: ctx;;
let addvbinding ctx s ty tm = (s, TyTmBind (ty, tm)) :: ctx;;

let gettbinding ctx s =
  match List.assoc s ctx with
  | TyBind ty -> ty
  | _ -> raise Not_found
;;

let getvbinding ctx s =
  match List.assoc s ctx with
  | TyTmBind (_, tm) -> tm
  | _ -> raise Not_found
;;

(* TYPE MANAGEMENT (TYPING) *)

let rec string_of_ty ty = match ty with
   | TyBool -> "Bool"
   | TyNat -> "Nat"
   | TyString -> "String"
   | TyArr (t1,t2) -> "(" ^ string_of_ty t1 ^ "->" ^ string_of_ty t2 ^ ")"
   | TyTuple tys -> "{" ^ String.concat ", " (List.map string_of_ty tys) ^ "}"
   | TyList t -> "[" ^ string_of_ty t ^ "]"
   | TyRcd fields -> "{" ^ String.concat ", " (List.map (fun (l, t) -> l ^ ":" ^ string_of_ty t) fields) ^ "}"
;;

exception Type_error of string;;

let rec typeof ctx tm = match tm with
    (* T-True *)
    TmTrue ->
      TyBool

    (* T-False *)
  | TmFalse ->
      TyBool

    (* T-If *)
  | TmIf (t1, t2, t3) ->
      if typeof ctx t1 = TyBool then
        let tyT2 = typeof ctx t2 in
        if typeof ctx t3 = tyT2 then tyT2
        else raise (Type_error "arms of conditional have different types")
      else
        raise (Type_error "guard of conditional not a boolean")

    (* T-Zero *)
  | TmZero ->
      TyNat

    (* T-Succ *)
  | TmSucc t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of succ is not a number")

    (* T-Pred *)
  | TmPred t1 ->
      if typeof ctx t1 = TyNat then TyNat
      else raise (Type_error "argument of pred is not a number")

    (* T-Iszero *)
  | TmIsZero t1 ->
      if typeof ctx t1 = TyNat then TyBool
      else raise (Type_error "argument of iszero is not a number")

    (* T-Var *)
  | TmVar x ->
      (try gettbinding ctx x with
       _ -> raise (Type_error ("no binding type for variable " ^ x)))

    (* T-Abs *)
  | TmAbs (x, tyT1, t2) ->
      let ctx' = addtbinding ctx x tyT1 in
      let tyT2 = typeof ctx' t2 in
      TyArr (tyT1, tyT2)

    (* T-App *)
  | TmApp (t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let tyT2 = typeof ctx t2 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT2 = tyT11 then tyT12
             else raise (Type_error "parameter type mismatch")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-Let *)
  | TmLetIn (x, t1, t2) ->
      let tyT1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x tyT1 in
      typeof ctx' t2

    (* T-Fix *)
  | TmFix t1 ->
      let tyT1 = typeof ctx t1 in
      (match tyT1 with
           TyArr (tyT11, tyT12) ->
             if tyT11 = tyT12 then tyT12
             else raise (Type_error "result of body not compatible with domain")
         | _ -> raise (Type_error "arrow type expected"))

    (* T-String *)
  | TmString _ ->
      TyString

    (* T-Concat *)
  | TmConcat (t1,t2) ->
      let ty1 = typeof ctx t1 in
      let ty2 = typeof ctx t2 in
      if ty1 = TyString && ty2 = TyString then TyString
      else raise (Type_error "++ operands must be String")

    (* T-Tuple *)
  | TmTuple ts ->
      TyTuple (List.map (typeof ctx) ts)

    (* T-Rcd *)
  | TmRcd fields ->
      let field_types = List.map (fun (l, t) -> (l, typeof ctx t)) fields in
      TyRcd field_types

    (* T-Proj *)
  | TmProj (t, i) ->
      let tyT = typeof ctx t in
      (match tyT with
         TyTuple tys ->
           if i >= 1 && i <= List.length tys then List.nth tys (i-1)
           else raise (Type_error ("projection index " ^ string_of_int i ^ " out of bounds"))
       | _ -> raise (Type_error "projection of non-tuple"))

    (* T-ProjRcd *)
  | TmProjRcd (t, l) ->
      let tyT = typeof ctx t in
      (match tyT with
         TyRcd fields ->
           (try List.assoc l fields with
            Not_found -> raise (Type_error ("field " ^ l ^ " not found in record")))
       | _ -> raise (Type_error "projection of non-record"))

  
  (* T-Cons: construct a list by consing head and tail *)
  | TmCons (h, t) ->
      let tyH = typeof ctx h in
      (match t with
         TmNil ty -> TyList tyH
       | _ -> let tyT = typeof ctx t in
              (match tyT with
                 TyList ty when ty = tyH -> TyList ty
               | _ -> raise (Type_error "type mismatch in cons")))

  | TmHead t ->
      (match typeof ctx t with
         TyList ty -> ty
       | _ -> raise (Type_error "attempted head operation applied to non-list"))

  | TmTail t ->
      (match typeof ctx t with
         TyList ty -> TyList ty
       | _ -> raise (Type_error "attempted tail operation applied to non-list"))
  (* T-Nil: empty list literal (cannot infer on its own) *)
  | TmNil ty ->
      TyList ty

  | TmIsNil t ->
      (match typeof ctx t with
         TyList _ -> TyBool
       | _ -> raise (Type_error "attempted isnil operation applied to non-list"))

;;

;;


(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
    TmTrue ->
      "true"
  | TmFalse ->
      "false"
  | TmIf (t1,t2,t3) ->
      "if " ^ "(" ^ string_of_term t1 ^ ")" ^
      " then " ^ "(" ^ string_of_term t2 ^ ")" ^
      " else " ^ "(" ^ string_of_term t3 ^ ")"
  | TmZero ->
      "0"
  | TmSucc t ->
     let rec f n t' = match t' with
          TmZero -> string_of_int n
        | TmSucc s -> f (n+1) s
        | _ -> "succ " ^ "(" ^ string_of_term t ^ ")"
      in f 1 t
  | TmPred t ->
      "pred " ^ "(" ^ string_of_term t ^ ")"
  | TmIsZero t ->
      "iszero " ^ "(" ^ string_of_term t ^ ")"
  | TmVar s ->
      s
  | TmAbs (s, tyS, t) ->
      "(lambda " ^ s ^ ":" ^ string_of_ty tyS ^ ". " ^ string_of_term t ^ ")"
  | TmApp (t1, t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (s, t1, t2) ->
      "let " ^ s ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t ->
      "(fix " ^ string_of_term t ^ ")"
  | TmString s ->
      "\"" ^ s ^ "\""
  | TmConcat (t1,t2) ->
      "(" ^ string_of_term t1 ^ " ^ " ^ string_of_term t2 ^ ")"
  | TmTuple ts ->
      "{" ^ String.concat ", " (List.map string_of_term ts) ^ "}"
  | TmRcd fields ->
      "{" ^ String.concat ", " (List.map (fun (l, t) -> l ^ " = " ^ string_of_term t) fields) ^ "}"
  | TmProj (t, i) ->
      string_of_term t ^ "." ^ string_of_int i
  | TmProjRcd (t, l) ->
    string_of_term t ^ "." ^ l
  | TmNil _ ->
    "[]"
  | TmCons (t1, t2) ->
    let rec collect acc = function
      TmCons (h, tl) -> collect (acc @ [h]) tl
    | TmNil _ -> acc
    | other -> acc @ [other]
    in
    "[" ^ String.concat "; " (List.map string_of_term (collect [] (TmCons (t1, t2)))) ^ "]"
  | TmHead t ->
    "head " ^ string_of_term t
  | TmTail t ->
    "tail " ^ string_of_term t
  | TmIsNil t ->
    "isnil " ^ string_of_term t
;;

let rec ldif l1 l2 = match l1 with
    [] -> []
  | h::t -> if List.mem h l2 then ldif t l2 else h::(ldif t l2)
;;

let rec lunion l1 l2 = match l1 with
    [] -> l2
  | h::t -> if List.mem h l2 then lunion t l2 else h::(lunion t l2)
;;

let rec free_vars tm = match tm with
    TmTrue ->
      []
  | TmFalse ->
      []
  | TmIf (t1, t2, t3) ->
      lunion (lunion (free_vars t1) (free_vars t2)) (free_vars t3)
  | TmZero ->
      []
  | TmSucc t ->
      free_vars t
  | TmPred t ->
      free_vars t
  | TmIsZero t ->
      free_vars t
  | TmVar s ->
      [s]
  | TmAbs (s, _, t) ->
      ldif (free_vars t) [s]
  | TmApp (t1, t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmLetIn (s, t1, t2) ->
      lunion (ldif (free_vars t2) [s]) (free_vars t1)
  | TmFix t ->
      free_vars t
  | TmString _ ->
      []
  | TmConcat (t1,t2) ->
      lunion (free_vars t1) (free_vars t2)
  | TmTuple ts ->
      List.fold_left lunion [] (List.map free_vars ts)
  | TmRcd fields ->
      List.fold_left lunion [] (List.map (fun (_, t) -> free_vars t) fields)
  | TmProj (t, _) ->
      free_vars t
  | TmProjRcd (t, _) ->
      free_vars t
  | TmNil _ ->
    []
  | TmCons (h, t) ->
    lunion (free_vars h) (free_vars t)
  | TmHead t ->
    free_vars t
  | TmTail t ->
    free_vars t
  | TmIsNil t ->
    free_vars t
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm = match tm with
    TmTrue ->
      TmTrue
  | TmFalse ->
      TmFalse
  | TmIf (t1, t2, t3) ->
      TmIf (subst x s t1, subst x s t2, subst x s t3)
  | TmZero ->
      TmZero
  | TmSucc t ->
      TmSucc (subst x s t)
  | TmPred t ->
      TmPred (subst x s t)
  | TmIsZero t ->
      TmIsZero (subst x s t)
  | TmVar y ->
      if y = x then s else tm
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmAbs (y, tyY, subst x s t)
           else let z = fresh_name y (free_vars t @ fvs) in
                TmAbs (z, tyY, subst x s (subst y (TmVar z) t))
  | TmApp (t1, t2) ->
      TmApp (subst x s t1, subst x s t2)
  | TmLetIn (y, t1, t2) ->
      if y = x then TmLetIn (y, subst x s t1, t2)
      else let fvs = free_vars s in
           if not (List.mem y fvs)
           then TmLetIn (y, subst x s t1, subst x s t2)
           else let z = fresh_name y (free_vars t2 @ fvs) in
                TmLetIn (z, subst x s t1, subst x s (subst y (TmVar z) t2))
  | TmFix t1 ->
      TmFix (subst x s t1)
  | TmString tm ->
      TmString tm
  | TmConcat (t1,t2) ->
      TmConcat (subst x s t1, subst x s t2)
  | TmTuple ts ->
      TmTuple (List.map (subst x s) ts)
  | TmRcd fields ->
      TmRcd (List.map (fun (l, t) -> (l, subst x s t)) fields)
  | TmProj (t, i) ->
      TmProj (subst x s t, i)
  | TmProjRcd (t, l) ->
      TmProjRcd (subst x s t, l)
  | TmNil ty ->
    TmNil ty
  | TmCons (h, t') ->
    TmCons (subst x s h, subst x s t')
  | TmHead t' ->
    TmHead (subst x s t')
  | TmTail t' ->
    TmTail (subst x s t')
  | TmIsNil t' ->
    TmIsNil (subst x s t')
;;

let rec isnumericval tm = match tm with
    TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
    TmTrue  -> true
  | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | TmTuple ts -> List.for_all isval ts
  | TmRcd fields -> List.for_all (fun (_, t) -> isval t) fields
  | TmNil _ -> true
  | TmCons (h, t) -> isval h && isval t
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies
;;

let rec eval1 ctx tm = match tm with
    (* E-IfTrue *)
    TmIf (TmTrue, t2, _) ->
      t2

    (* E-IfFalse *)
  | TmIf (TmFalse, _, t3) ->
      t3

    (* E-If *)
  | TmIf (t1, t2, t3) ->
      let t1' = eval1 ctx t1 in
      TmIf (t1', t2, t3)

    (* E-Succ *)
  | TmSucc t1 ->
      let t1' = eval1 ctx t1 in
      TmSucc t1'

    (* E-PredZero *)
  | TmPred TmZero ->
      TmZero

    (* E-PredSucc *)
  | TmPred (TmSucc nv1) when isnumericval nv1 ->
      nv1

    (* E-Pred *)
  | TmPred t1 ->
      let t1' = eval1 ctx t1 in
      TmPred t1'

    (* E-IszeroZero *)
  | TmIsZero TmZero ->
      TmTrue

    (* E-IszeroSucc *)
  | TmIsZero (TmSucc nv1) when isnumericval nv1 ->
      TmFalse

    (* E-Iszero *)
  | TmIsZero t1 ->
      let t1' = eval1 ctx t1 in
      TmIsZero t1'

    (* E-AppAbs *)
  | TmApp (TmAbs(x, _, t12), v2) when isval v2 ->
      subst x v2 t12

    (* E-App2: evaluate argument before applying function *)
  | TmApp (v1, t2) when isval v1 ->
      let t2' = eval1 ctx t2 in
      TmApp (v1, t2')

    (* E-App1: evaluate function before argument *)
  | TmApp (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmApp (t1', t2)

    (* E-LetV *)
  | TmLetIn (x, v1, t2) when isval v1 ->
      subst x v1 t2

    (* E-Let *)
  | TmLetIn(x, t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmLetIn (x, t1', t2)

    (* E-FixBeta *)
  | TmFix (TmAbs (x, _, t2)) ->
      subst x tm t2

    (* E-Fix *)
  | TmFix t1 ->
      let t1' = eval1 ctx t1 in
      TmFix t1'

    (* E-ConcatString *)
  | TmConcat (TmString s1, TmString s2) ->
      TmString (s1 ^ s2)

  | TmConcat (TmString s1, t2) ->
      let t2' = eval1 ctx t2 in
      TmConcat (TmString s1, t2')

  | TmConcat (t1, t2) ->
      let t1' = eval1 ctx t1 in
      TmConcat (t1', t2)

    (* E-Tuple *)
  | TmTuple ts ->
      let rec eval_tuple = function
          [] -> []
        | t::ts -> (eval1 ctx t) :: ts
      in TmTuple (eval_tuple ts)

      (*E-Rcd*)
  | TmRcd fields ->
      let rec eval_fields = function
          [] -> []
        | (l, t)::fields -> (l, eval1 ctx t) :: fields
      in TmRcd (eval_fields fields)

    (* E-ProjTuple *)
  | TmProj (TmTuple ts, i) when List.for_all isval ts ->
      if i >= 1 && i <= List.length ts then List.nth ts (i-1)
      else raise NoRuleApplies  (* or error, but since typed, shouldn't happen *)

  | TmProj (t, i) ->
      let t' = eval1 ctx t in
      TmProj (t', i)

    (* E-ProjRcd *)
  | TmProjRcd (TmRcd fields, l) when List.for_all (fun (_, t) -> isval t) fields ->
      (try List.assoc l fields with Not_found -> raise NoRuleApplies)

  | TmProjRcd (t, l) ->
      let t' = eval1 ctx t in
      TmProjRcd (t', l)

  | TmCons (t1, t2) ->
    let t1' = eval1 ctx t1 in
    TmCons (t1', t2)

  | TmNil _ ->
    raise NoRuleApplies

  | TmHead (TmCons (v1, v2)) when isval v1 && isval v2 ->
    v1
  | TmHead t ->
    let t' = eval1 ctx t in
    TmHead t'

  | TmTail (TmCons (v1, v2)) when isval v1 && isval v2 ->
    v2
  | TmTail t ->
    let t' = eval1 ctx t in
    TmTail t'

  | TmIsNil (TmNil _) ->
    TmTrue
  | TmIsNil (TmCons (_, _)) ->
    TmFalse
  | TmIsNil t ->
    let t' = eval1 ctx t in
    TmIsNil t'

  | TmVar s ->
      getvbinding ctx s

  | _ ->
      raise NoRuleApplies
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try
    let tm' = eval1 ctx tm in
    eval ctx tm'
  with
    NoRuleApplies -> apply_ctx ctx tm
;;

let execute ctx = function
    Eval tm ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline ("- : " ^ string_of_term tm' ^ " : " ^ string_of_ty tyTm);
      ctx

  | Bind (s, tm) ->
      let tyTm = typeof ctx tm in
      let tm' = eval ctx tm in
      print_endline (s ^ " : " ^ string_of_ty tyTm ^ " = " ^ string_of_term tm');
      addvbinding ctx s tyTm tm'

  | Quit ->
      raise End_of_file

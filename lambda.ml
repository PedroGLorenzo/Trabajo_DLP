(* TYPE DEFINITIONS *)

type ty =
     TyBool
   | TyNat
   | TyArr of ty * ty
   | TyString
   | TyTuple of ty list
   | TyList of ty
   | TyRcd of (string * ty) list
   | TyVariant of (string * ty) list
   | TyName of string
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
  | TmLength of term
  | TmAppend of term * term
  | TmMap of term * term
  | TmVariant of string * term
  | TmAs of term * ty
  | TmCase of term * (string * string * term) list
;;

type command =
     Eval of term
   | Bind of string * term
   | TypeBind of string * ty
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
  | TyTmBind (ty, _) -> ty
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
   | TyVariant fields -> "<" ^ String.concat ", " (List.map (fun (l, t) -> l ^ ":" ^ string_of_ty t) fields) ^ ">"
   | TyName s -> s
;;

exception Type_error of string;;

(*gets the actual type from a type name*)
let rec resolve_ty ctx = function
  | TyName s -> gettbinding ctx s
  | ty -> ty
;;

let rec resolve_ty_full ctx ty =
  match resolve_ty ctx ty with
    TyArr (ty1, ty2) -> TyArr (resolve_ty_full ctx ty1, resolve_ty_full ctx ty2)
  | TyTuple tys -> TyTuple (List.map (resolve_ty_full ctx) tys)
  | TyList ty -> TyList (resolve_ty_full ctx ty)
  (* dont know if the record and variant are properly named*)
  | TyRcd fields -> TyRcd (List.map (fun (l, t) -> (l, resolve_ty_full ctx t)) fields)
  | TyVariant fields -> TyVariant (List.map (fun (l, t) -> (l, resolve_ty_full ctx t)) fields)
  | ty -> ty
;;

let is_subtype ctx ty1 ty2 =
  let rtn1 = resolve_ty_full ctx ty1 in
  let rtn2 = resolve_ty_full ctx ty2 in
  match rtn1, rtn2 with
    (*Exact matching & explicit nat,bool, str*)
    _, _ when rtn1 = rtn2 -> 
      true
    (*Array*)
  | TyArr (t1, rt1), TyArr (t2, rt2) ->
      is_subtype ctx t2 t1 && is_subtype ctx rt1 rt2 (*check later*)
    (*Tuple*)
  | TyTuple tys1, TyTuple tys2 ->
      List.length tys1 = List.length tys2 &&
      List.for_all2 (is_subtype ctx) tys1 tys2 (*check subtype on all elem*)
    (*List*)
  | TyList t1, TyList t2 ->
      is_subtype ctx t1 t2
    (*Variant*)
  | TyVariant fields1, TyVariant fields2 ->
      List.for_all (fun (lbl2, t2) ->
        match List.assoc_opt lbl2 fields1 with
        | Some t1 -> is_subtype ctx t1 t2 
        | None -> false
      ) fields2
    (*Record*)
  | TyRcd fields1, TyRcd fields2 ->
      List.for_all (fun (lbl2, t2) ->
        match List.assoc_opt lbl2 fields1 with
        | Some t1 -> is_subtype ctx t1 t2 
        | None -> false
      ) fields2
    (*Return false if not a subtype*)
  | _ -> 
    false
;;


(* can also be resolved by checking if is subtype ty1 ty2 and then ty2 ty1*)
let ty_equal ctx ty1 ty2 =
  let rty1 = resolve_ty_full ctx ty1 in
  let rty2 = resolve_ty_full ctx ty2 in
  rty1 = rty2
;;

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
         let tyT3 = typeof ctx t3 in
         if resolve_ty ctx tyT3 = resolve_ty ctx tyT2 then resolve_ty ctx tyT2
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
              if resolve_ty ctx tyT2 = resolve_ty ctx tyT11 then tyT12
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
              resolve_ty ctx tyT12
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

     (* T-Variant *)
   | TmVariant (c, t) ->
       let tyT = typeof ctx t in
       TyVariant [(c, tyT)]

   | TmAs (t, ty) ->
       let tyT = typeof ctx t in
       let ty' = resolve_ty ctx ty in
       (match tyT, ty' with
          TyVariant [(c, tyV)], TyVariant fields ->
            if List.mem_assoc c fields && List.assoc c fields = tyV then ty'
            else raise (Type_error ("variant " ^ c ^ " not compatible with type"))
        | _ -> raise (Type_error "as expects variant and variant type"))

     (* T-Case *)
   | TmCase (t, branches) ->
       let tyT = resolve_ty ctx (typeof ctx t) in
       (match tyT with
          TyVariant fields ->
            let rec check_branches = function
                [] -> raise (Type_error "case must have branches")
              | [(c, x, body)] ->
                  (try let tyC = List.assoc c fields in
                       let ctx' = addtbinding ctx x tyC in
                       resolve_ty ctx (typeof ctx' body)
                   with Not_found -> raise (Type_error ("unknown constructor " ^ c)))
              | (c, x, body)::rest ->
                  (try let tyC = List.assoc c fields in
                       let ctx' = addtbinding ctx x tyC in
                       let tyBody = resolve_ty ctx (typeof ctx' body) in
                       let tyRest = check_branches rest in
                       if tyBody = tyRest then tyBody
                       else raise (Type_error "case branches have different types")
                   with Not_found -> raise (Type_error ("unknown constructor " ^ c)))
            in check_branches branches
        | _ -> raise (Type_error "case of non-variant"))

  
  (* T-Cons *)
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
  (* T-Nil *)
  | TmNil ty ->
      TyList ty

  | TmIsNil t ->
      (match typeof ctx t with
         TyList _ -> TyBool
       | _ -> raise (Type_error "attempted isnil operation applied to non-list"))
  | TmLength t ->
      (match typeof ctx t with
         TyList _ -> TyNat
       | _ -> raise (Type_error "attempted length operation applied to non-list"))
  | TmAppend (t1, t2) ->
      (match typeof ctx t1, typeof ctx t2 with
         TyList ty1, TyList ty2 when ty1 = ty2 -> TyList ty1
       | _ -> raise (Type_error "type mismatch in append"))
  | TmMap (t1, t2) ->
      (match typeof ctx t1, typeof ctx t2 with
         TyArr (tyA, tyB), TyList tyL when tyA = tyL -> TyList tyB
       | _ -> raise (Type_error "type mismatch in map"))

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
   | TmVariant (c, t) ->
       "<" ^ c ^ " = " ^ string_of_term t ^ ">"
   | TmAs (t, ty) ->
       string_of_term t ^ " as " ^ string_of_ty ty
   | TmCase (t, branches) ->
       "case " ^ string_of_term t ^ " of" ^
       String.concat "| " (List.map (fun (c, x, body) -> "<" ^ c ^ "=" ^ x ^ "> => " ^ string_of_term body) branches)
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
  | TmLength t ->
    "length " ^ string_of_term t
  | TmAppend (t1, t2) ->
    "append " ^ string_of_term t1 ^ " " ^ string_of_term t2
  | TmMap (t1, t2) ->
    "map " ^ string_of_term t1 ^ " " ^ string_of_term t2
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
  | TmLength t ->
    free_vars t
  | TmAppend (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmMap (t1, t2) ->
    lunion (free_vars t1) (free_vars t2)
  | TmVariant (_, t) ->
    free_vars t
  | TmAs (t, _) ->
    free_vars t
  | TmCase (t, branches) ->
    let branch_vars = List.fold_left lunion [] (List.map (fun (_, x, body) -> ldif (free_vars body) [x]) branches) in
    lunion (free_vars t) branch_vars
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
   | TmVariant (c, t) ->
       TmVariant (c, subst x s t)
   | TmAs (t, ty) ->
       TmAs (subst x s t, ty)
   | TmCase (t, branches) ->
       TmCase (subst x s t, List.map (fun (c, y, body) -> (c, y, if y = x then body else subst x s body)) branches)
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
  | TmLength t' ->
      TmLength (subst x s t')
  | TmAppend (t1, t2) ->
      TmAppend (subst x s t1, subst x s t2)
  | TmMap (t1, t2) ->
      TmMap (subst x s t1, subst x s t2)
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
  | TmVariant (_, t) -> isval t
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
   |    TmProj (TmTuple ts, i) when List.for_all isval ts ->
       if i >= 1 && i <= List.length ts then List.nth ts (i-1)
       else raise NoRuleApplies  (* or error, but since typed, shouldn't happen *)

    |    TmProj (t, i) ->
       let t' = eval1 ctx t in
       TmProj (t', i)

     (* E-ProjRcd *)
    | TmProjRcd (TmRcd fields, l) when List.for_all (fun (_, t) -> isval t) fields ->
       (try List.assoc l fields with Not_found -> raise NoRuleApplies)

    | TmProjRcd (t, l) ->
       let t' = eval1 ctx t in
       TmProjRcd (t', l)

     (* E-Variant *)
    | TmVariant (c, t) ->
       let t' = eval1 ctx t in
       TmVariant (c, t')

    | TmAs (TmVariant (c, v), ty) when isval v ->
       TmVariant (c, v)

    | TmAs (t, ty) ->
       let t' = eval1 ctx t in
       TmAs (t', ty)

     (* E-CaseVariant *)
    | TmCase (TmVariant (c, v), branches) when isval v ->
       (try let (_, x, body) = List.find (fun (c', _, _) -> c' = c) branches in
            subst x v body
        with Not_found -> raise NoRuleApplies)

       (*E-Case*)
   | TmCase (t, branches) ->
       let t' = eval1 ctx t in
       TmCase (t', branches)

    (* E-Cons *)
    | TmCons (v1, v2) when isval v1 ->
        let t2' = eval1 ctx v2 in
        TmCons (v1, t2')
    | TmCons (t1, t2) ->
        let t1' = eval1 ctx t1 in
        TmCons (t1', t2)

    (*E-Nil*)
    | TmNil _ ->
        raise NoRuleApplies
    (* E-Head *)
    | TmHead (TmCons (v1, v2)) when isval v1 && isval v2 ->
        v1
    | TmHead t ->
        let t' = eval1 ctx t in
        TmHead t'

    (* E-Tail *)
    | TmTail (TmCons (v1, v2)) when isval v1 && isval v2 ->
        v2
    | TmTail t ->
        let t' = eval1 ctx t in
        TmTail t'

    (* E-IsNil *)
    | TmIsNil (TmNil _) ->
        TmTrue
    | TmIsNil (TmCons (_, _)) ->
        TmFalse
    | TmIsNil t ->
        let t' = eval1 ctx t in
        TmIsNil t'

    (* E-Length *)
    | TmLength (TmNil _) ->
        TmZero
    | TmLength (TmCons (h, t)) when isval h && isval t ->
        TmSucc (TmLength t) (*loop applying successor rec*)
    | TmLength t ->
        let t' = eval1 ctx t in
        TmLength t'

    (* E-Append *)
    | TmAppend (TmNil _, v2) when isval v2 ->
        v2
    | TmAppend (TmCons (h, t), v2) when isval h && isval t && isval v2 ->
        TmCons (h, TmAppend (t, v2)) (*append loop with h of t and v2 as new tail*)
    | TmAppend (t1, t2) ->
        let t1' = eval1 ctx t1 in
        TmAppend (t1', t2)
    
    (* E-Map *)
    | TmMap (TmAbs (x, tyx, tBody), TmNil ty) ->
        TmNil ty
    | TmMap (TmAbs (x, tyx, tBody), TmCons (h, t)) when isval h && isval t ->
        TmCons (subst x h tBody, TmMap (TmAbs (x, tyx, tBody), t))
    | TmMap (v1, t2) when isval v1 ->
        let t2' = eval1 ctx t2 in
        TmMap (v1, t2')
    | TmMap (t1, t2) ->
        let t1' = eval1 ctx t1 in
        TmMap (t1', t2)
      
    (* E-Var *)
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

   | TypeBind (s, ty) ->
       print_endline (s ^ " = " ^ string_of_ty ty);
       addtbinding ctx s ty

   | Quit ->
       raise End_of_file
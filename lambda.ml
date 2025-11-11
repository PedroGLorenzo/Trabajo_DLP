(* TYPE DEFINITIONS *)

type ty =
    TyBool
  | TyNat
  | TyArr of ty * ty
  | TyString
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
;;

exception Type_error of string;;

let rec typeof ctx tm = match tm with
  | TmTrue -> TyBool
  | TmFalse -> TyBool
  | TmIf (t1,t2,t3) ->
      (match typeof ctx t1 with
       | TyBool ->
           let ty2 = typeof ctx t2 in
           let ty3 = typeof ctx t3 in
           if ty2 = ty3 then ty2 else raise (Type_error "branches differ")
       | _ -> raise (Type_error "if condition not Bool"))
  | TmZero -> TyNat
  | TmSucc t | TmPred t ->
      (match typeof ctx t with
       | TyNat -> TyNat
       | _ -> raise (Type_error "succ/pred arg not Nat"))
  | TmIsZero t ->
      (match typeof ctx t with
       | TyNat -> TyBool
       | _ -> raise (Type_error "iszero arg not Nat"))
  | TmVar x ->
      gettbinding ctx x
  | TmAbs (x, tyX, t) ->
      let ctx' = addtbinding ctx x tyX in
      let tyT = typeof ctx' t in
      TyArr (tyX, tyT)
  | TmApp (t1,t2) ->
      (match typeof ctx t1 with
       | TyArr (tyA, tyB) ->
           let ty2 = typeof ctx t2 in
           if ty2 = tyA then tyB else raise (Type_error "arg type mismatch")
       | _ -> raise (Type_error "apply non-function"))
  | TmLetIn (x, t1, t2) ->
      let ty1 = typeof ctx t1 in
      let ctx' = addtbinding ctx x ty1 in
      typeof ctx' t2
  | TmFix t ->
      (match typeof ctx t with
       | TyArr (tyA, tyB) when tyA = tyB -> tyA
       | _ -> raise (Type_error "fix expects ty->ty"))
  | TmString _ -> TyString
  | TmConcat (t1,t2) ->
      let ty1 = typeof ctx t1 in
      let ty2 = typeof ctx t2 in
      if ty1 = TyString && ty2 = TyString then TyString
      else raise (Type_error "++ operands must be String")
;;

(* TERMS MANAGEMENT (EVALUATION) *)

let rec string_of_term = function
  | TmTrue -> "true"
  | TmFalse -> "false"
  | TmIf (a,b,c) ->
      "if " ^ string_of_term a ^ " then " ^ string_of_term b ^
      " else " ^ string_of_term c
  | TmZero -> "0"
  | TmSucc t ->
      let rec count n = function
        | TmZero -> string_of_int n
        | TmSucc t' -> count (n+1) t'
        | _ -> "succ(" ^ string_of_term t ^ ")"
      in count 1 t
  | TmPred t -> "pred(" ^ string_of_term t ^ ")"
  | TmIsZero t -> "iszero(" ^ string_of_term t ^ ")"
  | TmVar x -> x
  | TmAbs (x, tyX, t) ->
      "lambda " ^ x ^ ":" ^ string_of_ty tyX ^ ". " ^ string_of_term t
  | TmApp (t1,t2) ->
      "(" ^ string_of_term t1 ^ " " ^ string_of_term t2 ^ ")"
  | TmLetIn (x,t1,t2) ->
      "let " ^ x ^ " = " ^ string_of_term t1 ^ " in " ^ string_of_term t2
  | TmFix t -> "fix(" ^ string_of_term t ^ ")"
  | TmString s -> "\"" ^ s ^ "\""
  | TmConcat (t1,t2) ->
      "(" ^ string_of_term t1 ^ " ^ " ^ string_of_term t2 ^ ")"
;;

let rec ldif l1 l2 = match l1 with
  | h::t -> if List.mem h l2 then ldif t l2 else h :: ldif t l2
  | [] -> []
;;

let rec lunion l1 l2 = match l1 with
  | h::t -> if List.mem h l2 then lunion t l2 else h :: lunion t l2
  | [] -> l2
;;

let rec free_vars tm = match tm with
  | TmTrue | TmFalse | TmZero | TmString _ -> []
  | TmSucc t | TmPred t | TmIsZero t | TmFix t -> free_vars t
  | TmIf (a,b,c) -> lunion (free_vars a) (lunion (free_vars b) (free_vars c))
  | TmVar x -> [x]
  | TmAbs (x, _, t) -> ldif (free_vars t) [x]
  | TmApp (t1,t2) -> lunion (free_vars t1) (free_vars t2)
  | TmLetIn (x,t1,t2) -> lunion (free_vars t1) (ldif (free_vars t2) [x])
  | TmConcat (t1,t2) -> lunion (free_vars t1) (free_vars t2)
;;

let rec fresh_name x l =
  if not (List.mem x l) then x else fresh_name (x ^ "'") l
;;

let rec subst x s tm =
  let r = subst x s in
  match tm with
  | TmTrue | TmFalse | TmZero | TmString _ -> tm
  | TmVar y -> if y = x then s else tm
  | TmSucc t -> TmSucc (r t)
  | TmPred t -> TmPred (r t)
  | TmIsZero t -> TmIsZero (r t)
  | TmIf (a,b,c) -> TmIf (r a, r b, r c)
  | TmAbs (y, tyY, t) ->
      if y = x then tm
      else if not (List.mem y (free_vars s)) then TmAbs (y, tyY, r t)
      else
        let y' = fresh_name y (lunion (free_vars t) (free_vars s)) in
        TmAbs (y', tyY, r (subst y (TmVar y') t))
  | TmApp (t1,t2) -> TmApp (r t1, r t2)
  | TmLetIn (y,t1,t2) ->
      let t1' = r t1 in
      if y = x then TmLetIn (y, t1', t2)
      else if not (List.mem y (free_vars s)) then TmLetIn (y, t1', r t2)
      else
        let y' = fresh_name y (lunion (free_vars t2) (free_vars s)) in
        let t2' = subst y (TmVar y') t2 in
        TmLetIn (y', t1', r t2')
  | TmFix t -> TmFix (r t)
  | TmConcat (t1,t2) -> TmConcat (r t1, r t2)
;;

let rec isnumericval tm = match tm with
  | TmZero -> true
  | TmSucc t -> isnumericval t
  | _ -> false
;;

let rec isval tm = match tm with
  | TmTrue | TmFalse -> true
  | TmAbs _ -> true
  | TmString _ -> true
  | t when isnumericval t -> true
  | _ -> false
;;

exception NoRuleApplies;;

let rec eval1 ctx tm = match tm with
  | TmIf (TmTrue, t2, _) -> t2
  | TmIf (TmFalse, _, t3) -> t3
  | TmIf (t1,t2,t3) -> TmIf (eval1 ctx t1, t2, t3)
  | TmSucc t when not (isval t) -> TmSucc (eval1 ctx t)
  | TmPred TmZero -> TmZero
  | TmPred (TmSucc nv) when isnumericval nv -> nv
  | TmPred t -> TmPred (eval1 ctx t)
  | TmIsZero TmZero -> TmTrue
  | TmIsZero (TmSucc nv) when isnumericval nv -> TmFalse
  | TmIsZero t -> TmIsZero (eval1 ctx t)
  | TmApp (TmAbs (x,_,t12), v2) when isval v2 -> subst x v2 t12
  | TmApp (v1, t2) when isval v1 -> TmApp (v1, eval1 ctx t2)
  | TmApp (t1, t2) -> TmApp (eval1 ctx t1, t2)
  | TmLetIn (x, v1, t2) when isval v1 -> subst x v1 t2
  | TmLetIn (x, t1, t2) -> TmLetIn (x, eval1 ctx t1, t2)
  | TmFix (TmAbs (x,ty,t)) -> subst x (TmFix (TmAbs (x,ty,t))) t
  | TmFix t -> TmFix (eval1 ctx t)
  | TmConcat (t1,t2) when not (isval t1) -> TmConcat (eval1 ctx t1, t2)
  | TmConcat (v1,t2) when isval v1 && not (isval t2) -> TmConcat (v1, eval1 ctx t2)
  | TmConcat (TmString s1, TmString s2) -> TmString (s1 ^ s2)
  | _ -> raise NoRuleApplies
;;

let apply_ctx ctx tm =
  List.fold_left (fun t x -> subst x (getvbinding ctx x) t) tm (free_vars tm)
;;

let rec eval ctx tm =
  try let tm' = eval1 ctx tm in eval ctx tm'
  with NoRuleApplies -> tm
;;

let execute ctx = function
  | Eval tm ->
      let ty = typeof ctx tm in
      let v = eval ctx tm in
      Printf.printf "%s : %s\n" (string_of_term v) (string_of_ty ty);
      ctx
  | Bind (x, tm) ->
      let ty = typeof ctx tm in
      let v = eval ctx tm in
      Printf.printf "%s = %s : %s\n" x (string_of_term v) (string_of_ty ty);
      addvbinding (addtbinding ctx x ty) x ty v
  | Quit ->
      Printf.printf "Bye.\n"; ctx
;;
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
   | Bind of string * ty option * term
   | TypeBind of string * ty
   | Quit
;;

type binding =
   TyBind of ty
 | TyTmBind of (ty * term)
;;

type context =
   (string * binding) list
;;

val emptyctx : context;;
val addtbinding : context -> string -> ty -> context;;
val addvbinding : context -> string -> ty -> term -> context;;
val gettbinding : context -> string -> ty;;
val getvbinding : context -> string -> term;;

val string_of_ty : ty -> string;;
exception Type_error of string;;

(* resolve simple TyName*)
val resolve_ty : context -> ty -> ty;;
(* resolve full recursively all nested TyNames*)
val resolve_ty_full : context -> ty -> ty;;

val is_subtype : context -> ty -> ty -> bool;;
val ty_equal : context -> ty -> ty -> bool;;

val ty_join : context -> ty -> ty -> ty;;

val typeof : context -> term -> ty;;

val string_of_term : term -> string;;
exception NoRuleApplies;;
val eval : context -> term -> term;;

val execute : context -> command -> context;;
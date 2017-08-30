open Map

(* Context Initialization *)
module Context = Map.Make(String)
module Func_index = Map.Make(struct type t = int let compare : int -> int -> int = compare end)
module Global_Ctx = Map.Make(String)

type ty = 
        | I 
        | F of ty*ty
        | Tref of ty
        | Unit

type specifier = 
        | Private
        | Public

type term =
        | Var of string  
        | Abs of string*string*ty*terms   (* name, type, body *)
        | App of term*term
        | Ref of term 
        | Deref of term
        | Assign of term*term
        | Unit
        | Val of int
        | Decl of specifier*string*term

        and terms = 
        | Term of term
        | Decs of terms*term


let rec pr_type = function 
        | I -> "I"
        | F (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"
        | Tref t -> "Ref "^pr_type t
        | Unit -> "Unit"


let rec term_to_string = function 
        | Var name -> name
        | Abs (func_name, name, tp, b) -> "/" ^ name ^ ".(" ^ dec_to_string b ^ ")"
        | App (t1, t2) -> term_to_string t1 ^ "( "^term_to_string t2^" )"
        | Ref t -> "Ref ("^term_to_string t^")"
        | Deref t -> "! ("^term_to_string t^")"
        | Assign (t1, t2) -> term_to_string t1^" := "^term_to_string t2
        | Unit -> "Unit"
        | Val x -> string_of_int x
        | Decl (s, n, t) -> n ^ " = " ^ term_to_string t 
        and dec_to_string = function
        | Term t -> term_to_string t
        | Decs (d, t) -> (dec_to_string d)^"; "^(term_to_string t)
        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Eval_error of string 



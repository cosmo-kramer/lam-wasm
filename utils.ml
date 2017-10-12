open Map

(* Context Initialization *)
module Context = Map.Make(String)
module Func_index = Map.Make(struct type t = int let compare : int -> int -> int = compare end)
module Global_Ctx = Map.Make(String)

type ty = 
        | I 
        | F of ty*ty
        | Tref of ty
        | Un
        | Unit

type direction =
        | Synthesizes
        | Checks


type specifier = 
        | Private
        | Public

type term =
        | Var of string  
        | Abs of string*string*ty*terms   (* name, type, body *)
        | App of term*term
        | Ref of term
        | Unref of term 
        | Deref of term
        | Assign of term*term
        | Unit
        | Val of int

        and terms = 
        | Term of term
        | Terms of terms*term

type decl = specifier*string*term         

type global_decls =
        | Decl of decl 
        | Decls of global_decls*decl

let rec pr_type = function 
        | I -> "I"
        | F (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"
        | Tref t -> "Ref "^pr_type t
        | Unit -> "Unit"


let rec term_to_string = function 
        | Var name -> name
        | Abs (func_name, name, tp, b) -> "/" ^ name ^ ".(" ^ terms_to_string b ^ ")"
        | App (t1, t2) -> term_to_string t1 ^ "( "^term_to_string t2^" )"
        | Ref t -> "Ref ("^term_to_string t^")"
        | Unref t -> "Unref ("^ term_to_string t^")"
        | Deref t -> "! ("^term_to_string t^")"
        | Assign (t1, t2) -> term_to_string t1^" := "^term_to_string t2
        | Unit -> "Unit"
        | Val x -> string_of_int x
        and terms_to_string = function
        | Term t -> term_to_string t
        | Terms (d, t) -> (terms_to_string d)^"; "^(term_to_string t)


let rec decl_to_string (d: global_decls) =  
        match d with
        | Decl (s, n, t) -> n ^ " = " ^ term_to_string t
        | Decls (s, d) -> decl_to_string s ^ "\n" ^ decl_to_string (Decl d) 

        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Eval_error of string 

let func_num = ref 0
let globals = ref ""
let get_func_num () = (func_num := !func_num + 1; !func_num) 


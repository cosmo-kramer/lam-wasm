open Map
(* Context Initialization *)
module Context = Map.Make(String)
module Func_index = Map.Make(struct type t = int let compare : int -> int -> int = compare end)
module Global_Ctx = Map.Make(String)
module BoundVars = Set.Make(String)
type ty = 
        | Tint 
        | Tfun of ty*ty
        | Tref of ty
        | Tun
        | Tunit

type term =
        | Var of string  
        | Abs of string*string*term   (* func_name, name, body *)
        | App of term*term
        | Ref of term
        | Deref of term
        | Assign of term*term
        | Unit
        | Val of int
        | Let of string*term*term
        | Asc of term*ty

type decl = string*term         

type global_decls =
        | Decl of decl 
        | Decls of global_decls*decl

let rec pr_type = function 
        | Tint -> "I"
        | Tfun (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"
        | Tref t -> "Ref "^pr_type t
        | Tunit -> "Unit"
        | Tun -> "Un"


let rec term_to_string = function 
        | Var name -> name
        | Abs (func_name, name, b) -> "/" ^ name ^ ".(" ^ term_to_string b ^ ")"
        | App (t1, t2) -> term_to_string t1 ^ "( "^term_to_string t2^" )"
        | Ref t -> "Ref ("^term_to_string t^")"
        | Deref t -> "! ("^term_to_string t^")"
        | Assign (t1, t2) -> term_to_string t1^" := "^term_to_string t2
        | Unit -> "Unit"
        | Val x -> string_of_int x
        | Let (s, t1, t2) -> "let "^s^" "^(term_to_string t1)^" in "^(term_to_string t2)
        | Asc (e, t) -> (term_to_string e)^(" : ")^(pr_type t) 


let rec decl_to_string (d: global_decls) =  
        match d with
        | Decl (n, t) -> n ^ " = " ^ term_to_string t
        | Decls (s, d) -> decl_to_string s ^ "\n" ^ decl_to_string (Decl d) 


 


type state = {
  funcs_code : string list;
  init_global : string list;
  exports : string list;
  globals : string list;
}


        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Eval_error of string 

let func_num = ref 0
let get_func_num () = (func_num := !func_num + 1; !func_num) 
let empty_state = {funcs_code = []; init_global = []; exports = []; globals = []}

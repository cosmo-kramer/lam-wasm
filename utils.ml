open Map
open List
(* Context Initialization *)
module Context = Map.Make(String)
module Func_index = Map.Make(String)(*struct type t = int let compare : int -> int -> int = compare end)*)
module Global_ctx = Map.Make(String)
module BoundVars = Set.Make(String)
module Funcs_code = Map.Make(String)


type term =
        | Var of string  
        | Abs of string*string*term   (* func_name (name of wasm func), name(arg name), body *)
        | App of term*term
        | Ref of term
        | Deref of term
        | Assign of term*term
        | Unit
        | Val of int
        | Let of string*term*term
        | Asc of term*ty

        and refinement = 
                | Eq of (term option)*(term option)   (*Nones will be filled by the term (x) *) 
                | Leq of ((term option)*(term option))
                | Un of term option 
                | Tr 
        and baseT = 
        | Tint 
        | Tunit

        and ty =
        | R of baseT*refinement
        | Tfun of ty*ty
        | Tref of baseT*refinement
        | Tun

let apply phi t = match phi with
| Eq (None, None) -> Eq (t, t)
| Eq (None, x) -> Eq (t, x)
| Eq (x, None) -> Eq (x, t)
| Un None -> Un t
| x -> x
 
let erase ctx = Context.fold (fun k v l ->
                                match v with
                                | R (_, phi) -> (apply phi (Some (Var k)))::l
                                | _ -> l
                             ) ctx []
(* Compilation units -> let string = t1 in compUnit *)
type tcompUnit = 
        | Lcomp of string*term*tcompUnit
        | Lterm of string*term*term

type texports = 
        | All
        | Exp of (string*ty) list

type decl = string*term         

type global_decls =
        | Decl of decl 
        | Decls of global_decls*decl

let rec pr_bType = function
        | Tint -> "Int"
        | Tunit -> "Unit"


let rec pr_phi phi = "phi" 
        and pr_type t = match t with
        | R (b, phi) -> "{x: "^pr_bType b^" | "^ pr_phi phi^" }"   
        | Tfun (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"
        | Tref (t, phi) -> "Ref (x."^pr_phi phi^")"^pr_bType t
        | Tun -> "Un"

and term_to_string = function 
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


let rec compUnit_to_string = function
        | Lcomp (name, t1, l1) -> "let "^name^" = "^(term_to_string t1)^" in \n"^compUnit_to_string l1
        | Lterm (name, t1, t2) -> "let "^name^" = "^(term_to_string t1)^" in \n"^term_to_string t2
let rec decl_to_string (d: global_decls) =  
        match d with
        | Decl (n, t) -> n ^ " = " ^ term_to_string t
        | Decls (s, d) -> decl_to_string s ^ "\n" ^ decl_to_string (Decl d) 

type state = {
  funcs_code : (string list);
  globals : (ty Global_ctx.t);
}


        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Eval_error of string 
exception Export_error of string
let func_num = ref 0
let get_func_num () = (func_num := !func_num + 1; !func_num) 
let low_integrity = ref 0
let module_name = ref ""
let func_index : (int Func_index.t ref) = ref Func_index.empty
let empty_state = {funcs_code = []; globals = Global_ctx.empty;}

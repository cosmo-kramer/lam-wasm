open Map
open List
(* Context Initialization *)
module Context = Map.Make(String)
module Func_index = Map.Make(String)(*struct type t = int let compare : int -> int -> int = compare end)*)
module Global_ctx = Map.Make(String)
module BoundVars = Set.Make(String)
module Funcs_code = Map.Make(String)
module Base_types = Map.Make(String)


        (* Exceptions *)
exception Type_resolution_failed of string
exception Application_failed of string
exception Unbound_Var of string 
exception Error of string 
exception Export_error of string

type term =
        | Var of string  
        | Abs of string*string*term   (* func_name (name of wasm func), name(arg name), body *)
        | App of term*term
        | Ref of term
        | Deref of term
        | Assign of term*term
        | Unit
        | Val of int
        | Plus of term*term
        | Let of string*term*term
        | Asc of term*ty
        | Fix of term 
        | Constructor of string*(term list)
        | Pair of term*term
        and refinement = 
                | Eq of term*term   (*Nones will be filled by the term (x) *) 
                | Leq of term*term
                | Un of term
                | Tr 
        and baseT = 
        | Base of string*((string*(ty list)) list)
      (*  | Dummy of string  Having this dummy type helps in parsing recursive base types, this has no rules for type checking so any type where this doesn't get erased to a Base constructor will fail type checking *)
        and ty =
              (* x    baseT    phi  *)  
        | R of string*string*refinement
        | Rty of string*ty*refinement
        | Tfun of ty*ty
        | Tref of ty
        | Tpair of ty*ty
        | Tun

let isPure = function 
        | Ref _ | Deref _ | Assign _ -> raise (Error "Impure phi!")
        | _ -> ()

let rec subst x t t1 = match t1 with
| Val n -> Val n
| Var y -> if y = x then t else t1
| Abs (s, n, bd) -> Abs (s, n, (subst x t bd))
| App (t1, t2) -> App (subst x t t1, subst x t t2)
| Plus (t1, t2) -> Plus (subst x t t1, subst x t t2)
| Unit -> Unit
| Let (x, t1, t2) -> Let (x, subst x t t1, subst x t t2)
| Asc (t1, tyy) -> Asc (subst x t t1, tyy)
| Fix f -> Fix (subst x t f)
| Constructor (s, l) -> Constructor (s, map (subst x t) l)
| Pair (t1, t2) -> Pair (subst x t t1, subst x t t2)
| _ -> raise (Error "Impure phi!"); Unit

let rec apply phi x t = match phi with
| Eq (t1, t2) -> Eq (subst x t t1, subst x t t2)
| Leq (t1, t2) -> Leq (subst x t t1, subst x t t2)
| Un t1 -> Un (subst x t t1)
| Tr -> Tr

let erase ctx = Context.fold (fun k v l ->
                                match v with
                                | R (x, _, phi) -> (apply phi x (Var k))::l
                                | _ -> l
                             ) ctx []
(* Compilation units -> let string = t1 in compUnit *)
type tcompUnit = 
        | Lcomp of string*term*tcompUnit
        | Lterm of term

type texports = 
        | All
        | Exp of (string*ty) list

type decl = string*term         

type global_decls =
        | Decl of decl 
        | Decls of global_decls*decl

let rec pr_bType b = "b_type" 
let rec pr_phi = function
        | Eq (t1, t2) -> (term_to_string t1)^"  ==  "^(term_to_string t2)
        | Leq (t1, t2) -> (term_to_string t1)^"  <=  "^(term_to_string t2)
        | Un t -> "Un ("^(term_to_string t)^")"
        | Tr -> "True"
 
        and pr_type t = match t with
        | R (x, b, phi)  ->  "{"^x^": "^pr_bType b^" | "^ pr_phi phi^" }"   
        | Rty (x, b, phi)-> "{"^x^": "^pr_bType b^" | "^ pr_phi phi^" }"   
        | Tfun (a, b) -> "("^pr_type a^") -> ("^pr_type b^")"
        | Tref ty -> "Ref ("^pr_type ty^")"
        | Tpair (t1, t2) -> "("^pr_type t1^" * "^pr_type t2^")"
        | Tun -> "Un"

and term_to_string = function 
        | Var name -> name
        | Abs (func_name, name, b) -> "/" ^ name ^ ".(" ^ term_to_string b ^ ")"
        | App (t1, t2) -> term_to_string t1 ^ "( "^term_to_string t2^" )"
        | Pair (t1, t2) -> "("^term_to_string t1 ^ "," ^ term_to_string t2^")"
        | Plus (t1, t2) -> term_to_string t1 ^ "- "^term_to_string t2
        | Ref t -> "Ref ("^term_to_string t^")"
        | Deref t -> "! ("^term_to_string t^")"
        | Assign (t1, t2) -> term_to_string t1^" := "^term_to_string t2
        | Unit -> "Unit"
        | Val x -> string_of_int x
        | Let (s, t1, t2) -> "let "^s^" "^(term_to_string t1)^" in "^(term_to_string t2)
        | Asc (e, t) -> (term_to_string e)^(" : ")^(pr_type t) 
        | Fix f -> "fix ("^(term_to_string f)^")"
        | Constructor (s, l) -> "Constructor "^s^" .. terms .. "
let rec compUnit_to_string = function
        | Lcomp (name, t1, l1) -> "let "^name^" = "^(term_to_string t1)^" in \n"^compUnit_to_string l1
        | Lterm t2 -> term_to_string t2
let rec decl_to_string (d: global_decls) =  
        match d with
        | Decl (n, t) -> n ^ " = " ^ term_to_string t
        | Decls (s, d) -> decl_to_string s ^ "\n" ^ decl_to_string (Decl d) 

type state = {
  funcs_code : (string list);
  globals : (ty Global_ctx.t);
}

(* Base types  string*((string*(baseT list)) list)   *)

let intB = Base ("int", [])
let unitB = Base ("unit", [])
let intT = R ("__NONE__", "int", Tr)
let unitT = R ("__NONE__", "unit", Tr)


let base_types = ref Base_types.empty

let add_base_type bT: unit = let Base (x, _) = bT in (base_types := Base_types.add x bT !base_types)

let dummy = (add_base_type intB); add_base_type unitB 

let debug = ref false  
let func_num = ref 0
let get_func_num () = (func_num := !func_num + 1; !func_num) 
let low_integrity = ref 0
let module_name = ref ""
let func_index : (int Func_index.t ref) = ref Func_index.empty
let find_opt x l = try
                        Some (List.find x l)
                   with Not_found -> None

let empty_state = {funcs_code = []; globals = Global_ctx.empty;}
let int_of_constructor (s: string) :int = let res = ref 0 in 
                                          String.iter (fun c -> res := (!res)*50+(int_of_char c - int_of_char 'a')) s;
                                          !res

let get_type s l = 
                  let pred = ( fun bT -> let Base (_, cs) = bT in 
                  let ll = ref None in
                  (List.iter (fun x -> let (nm, l') = x in if nm = s then (ll := Some l') else ()) cs); !ll) in
                  let ty = ref "__NONE__" in
                  let tList = ref [] in
                  (Base_types.iter (fun k v ->
                          if pred v != None then 
                                          let Some ll = pred v in 
                                          ty := k;tList := ll else ()
                            ) !base_types);
                            let tL = List.combine l !tList in
                            (!ty, tL)

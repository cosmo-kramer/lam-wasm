open Utils
open Map
open Printf
open Type_check

(*
                        Resolved
        PDS: It would be somewhat cleaner if this stuff were collected into a

		type state = {
			funcs_code : string;	(* or string list with an eventual concatenation *)
			init_global : string;
			exports : string;
		}

	and the translation consumed a state (and produced a new
	state). As things are written, you have to remember to clear
	the references if you decide to translate two modules in one
	run of the program.

	All of these could accumulate string list, rather than string,
	leaving the concatenation to the end. (You can concatenate n
	characters in time O(n), but the way you do it is O(n^2).)
*)

(*let funcs_code = ref ""
  let init_global = ref ""
  let exports = ref "(export \"main\" (func $mm))\n" 
*)


(*
                Resolved
        PDS: Sequential composition need not be part of the language.
	In the call-by-value STLC we can encode let expressions:

		let x = e1 in e2 := (λx. e2) e1

	and then we can encode sequential composition:

		e1; e2 := let x = e1 in e2	(* where x not free in e2; see ./toy_typechecker/var.mli:/fresh *)

	The parser can handle e1; e2 without cluttering the language.
*)
let rec gen_webAsm (t:term) ctx clsr st = match t with
  | Var x -> 
    if Global_Ctx.mem x !global_ctx then
(*
	PDS: This appears to be dead code. I think I see what you're
	doing here and I wonder if there is a cleaner way. Not all
	source variables need correspond to target local variables. In
	the source language, we have one class of variables (bound in
	declarations, let expressions, and lambdas). The variables
	bound in declarations compile to stuff outside any WA function
	body. Rather than resort to yet more state, a cleaner way
	might be to enrich our notion of variables with a bit (bound
	by a declaration or not). I used a record in
	toy_typechecker/var.ml:/var_info to support this sort of
	compile-time distinction.
*) 
      (
        ("(call_indirect $GG (i32.const 0) (get_global $"^x^") (i32.load (get_global $"^x^")))\n", clsr, st)
      ) else  
      (try  
         (BoundVars.find x ctx);
         ("(get_local $"^x^")", clsr, st)
       with Not_found -> raise (Unbound_Var x))
  | Abs (func_name, name, func_body) -> (let f_num: int = get_func_num () in
                                                   let f_name = if func_name = "" then "FF"^(string_of_int f_num) else func_name in 

                                                   func_index := Func_index.add f_num f_name !func_index;
                                                   let header = "(func $"^f_name^
                                                                "(param $"^name^" i32) (param $cl_add i32) (result i32) \n(local $this i32)\n" in
(*	
 *	Also also, you have not accounted for low integrity closures
	(as far as I can tell). The way to do that is to first
	allocate a high-integrity closure (like you're doing) and then
	to use dynamic sealing. If that's unclear, ask Deepak or me to
	explain.
*)
                                                   let new_clsr = clsr + 1 in
                                                   let cl_init = ref ("(set_local $this (get_global $dynamic_heap_ctr))\n"^
                                                                      "call $store_high ("^string_of_int (f_num-1)^"))\n") in

                                                   (* Store all values in ctx *) 
(*
        PDS: Closing over the entire context is a mistake. You should
	implement a function FV to compute the variables occurring
	free in an expression and then close over FV(func_body) \ DV
	where DV is the set of variables bound by top-level
	declarations. (The vars in DV are addressible at runtime
	without help from closures.)
*)
                                                   let fv = get_fv func_body (BoundVars.add name (BoundVars.empty)) in
                                                   List.iter (fun k ->
                                                       cl_init := (!cl_init)^
                                                                  "(call $store_high (get_local $"^k^"))\n"
                                                     ) fv; 
                                                   let get_cl = ref "" in
                                                   List.iter (fun k  ->
                                                       get_cl := !get_cl^("(local $"^k^" i32)\n")
                                                     ) fv;

                                                   (* Retrieve vars from closure *) 
                                                   BoundVars.iter (fun k -> 
                                                       get_cl := !get_cl^(
                                                           (*    "(set_local $cl_add (i32.add (get_local $cl_add) (i32.const 4)))\n"^*)
                                                           "(set_local $cl_add (i32.add (get_local $cl_add) "^
                                                           "(i32.const 4)))\n(i32.load (get_local $cl_add)\n)\n"^
                                                           "(set_local $"^k^")\n")
                                                     ) ctx; 
                                                   let new_ctx = BoundVars.add name ctx in 
                                                   let (bd, new_clsr, st') = gen_webAsm func_body new_ctx new_clsr st in 
                                                   st' = {st with funcs_code = ("\n\n"^header^(!get_cl)^bd^")\n")::st.funcs_code};
                                                   try 
                                                     (!cl_init ^ "\n(get_local $this)", new_clsr, st')
                                                   with Not_found -> raise (Unbound_Var (string_of_int f_num))
                                                  )  



  | App (t1, t2) -> let (code1, new_clsr, st) = gen_webAsm t1 ctx clsr st in
                    let (code2, new_clsr, st) = gen_webAsm t2 ctx new_clsr st in
   (code1^"(set_global $_this)\n"^code2^"(get_global $_this\n)\n(get_global $_this)\n(i32.load)\n(call_indirect $GG)\n", new_clsr, st)

  | Val x -> ("(i32.const "^string_of_int x^")\n", clsr, st)
  | Ref t ->  if infer_exp Context.empty t  == Tun then 
                  let res = "(set_global $dynamic_heap_ctr (i32.add (get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
                       "(get_global $dynamic_heap_ctr)\n" in
    let (cd, new_clsr, st) = gen_webAsm t ctx clsr st in
    (res ^ cd ^ "\n (i32.store) \n" 
     ^ "(get_global $dynamic_heap_ctr)\n", new_clsr, st)
  else (
          let res = "(set_global $dynamic_heap_ctr (i32.add (get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
                                        "(get_global $dynamic_heap_ctr)\n" in
                                       let (cd, new_clsr, st) = gen_webAsm t ctx clsr st in
                                        (res ^ cd ^ "\n (call $store_low) \n" 
                                        ^ "(get_global $dynamic_heap_ctr)\n", new_clsr, st)
  )

(*
	Doubt: I have allocated a big chunk of memory whilie initializing memory. I am right now not checking if my $dynamic_heap_ctr goes beyond
        that size. Should I implement that? So, if I understand your question correctly, there is no concept of allocating memory right now.


        PDS: This is surprising! Alongside store_low, you should also
	have imported a function alloc_low : i32 -> i32 for allocating
	low memory. You don't want to allocate low reference cells
	from the same memory as high reference cells.
*)
 | Deref t -> let (cd, new_clsr, st) = gen_webAsm t ctx clsr st in
    (match infer_exp Context.empty t with  
     | Tref _ -> (cd ^ "\n(i32.load)\n", new_clsr, st)
     | Tun _ -> (cd ^ "\n(call $load_low)\n", new_clsr, st)
     | _ -> raise (Eval_error "Derefing non-ref term")
    )
  | Assign (t1, t2) -> let (code1, new_clsr, st) = gen_webAsm t1 ctx clsr st in
           let (code2, new_clsr, st) = gen_webAsm t2 ctx new_clsr st in
           (code1^code2^"(i32.store)\n", new_clsr, st)
  | Let (name,t1,t2) -> let (code1, new_clsr, st) = gen_webAsm t1 ctx clsr st in
                        let (code2, new_clsr, st) = gen_webAsm t2 ctx clsr st in
                        (code1^"\n (set_global "^name^")\n"^code2, new_clsr, st)
  | Asc (e, t) -> gen_webAsm e ctx clsr st 
  | Unit -> ("", clsr, st)
(*
 *      Doubt: I do not understand this, can you ellaborate?
	PDS: You actually want to pass around a dummy value of type
	i32 (0 will do) in the target language. This is the only way
	to achieve a uniform closure representation.
*)




(*
	PDS: I will read the rest once you've updated to the treatment
	of declarations sketched in ./toy_typechecker/.
*)
let rec gen_webAsm_decls (d: global_decls) clsr st =     
  match d with
  | Decl (name, t) -> let (cd, new_clsr, st) = gen_webAsm (Abs (name, "dummy_x", t)) BoundVars.empty clsr st in
  let exp =  match infer_exp Context.empty t  with
      | Tun -> ("(export \""^name^"\" (func $"^name^"))\n")::st.exports
      | _ -> st.exports in 
  let st = {  st with 
          globals = ("(global $"^name^" (mut i32) (i32.const 0))\n")::st.globals;
          init_global = ("(set_global $" ^ name ^ ")\n")::(cd::st.init_global);
          exports = exp 
        } in
        ("", new_clsr, st)
  | Decls (s, d) -> let (_, cl, st') = gen_webAsm_decls s clsr st in gen_webAsm_decls (Decl d) cl st'


let create_code d t st = let (cd, cl, st') = (match d with
    | Some d' -> gen_webAsm_decls d' 0 st
    | None -> ("", 0, st)
  ) in
  let (cd, clsr, st') = (gen_webAsm t BoundVars.empty cl st') in
  let tab_ins = ref "" in
  for k=1 to clsr do
    tab_ins := (!tab_ins)^
               "(elem (i32.const "^string_of_int (k-1)^
               ") $"^Func_index.find k !func_index^")\n"
  done;
  ("(module \n"^
   "(func $load_low (import \"low\" \"load\") (param i32) (result i32))\n"^
   "(func $store_low (import \"low\" \"store\") (param i32) (param i32))\n"^     
   "(type $GG (func (param i32) (param i32) (result i32)))\n"^
   "(table " ^ string_of_int clsr ^ " anyfunc)\n"^(!tab_ins)^
   "(memory $0 100)\n"^
   "(global $dynamic_heap_ctr (mut i32) (i32.const 0))\n"^
   "(global $_this (mut i32) (i32.const 0))\n"^
   "(func $store_high (param $0 i32)
     (      
             (i32.store (get_global $dynamic_heap_ctr) (get_local $0))\n
             set_global $dynamic_heap_ctr (i32.add (get_global $dynamic_heap_ctr) (i32.const 4))\n
     ) )"^
   String.concat ""  (List.rev st.globals)^"\n"^
   "(export \"memory\" (memory $0))\n(export \"main\" (func $mm))\n"^
   String.concat ""  (List.rev st.exports)^"\n"^  
   String.concat ""  (List.rev st.funcs_code)^ "\n (func $mm (result i32)\n(local $this i32)\n"^
   String.concat ""  (List.rev st.init_global)^"\n"^cd^"\n)\n)"
  )  

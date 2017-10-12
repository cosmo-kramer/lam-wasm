open Utils
open Map
open Printf
open Type_check

(*
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
let funcs_code = ref ""
let init_global = ref ""
let exports = ref "(export \"main\" (func $mm))\n" 

(*
	PDS: Sequential composition need not be part of the language.
	In the call-by-value STLC we can encode let expressions:

		let x = e1 in e2 := (λx. e2) e1

	and then we can encode sequential composition:

		e1; e2 := let x = e1 in e2	(* where x not free in e2; see ./toy_typechecker/var.mli:/fresh *)

	The parser can handle e1; e2 without cluttering the language.
*)
let rec gen_webAsm (d: terms) ctx clsr = match d with
        | Term t -> gen_webAsm_term t ctx clsr
        | Terms (d1, t) -> let (f, new_clsr) = (gen_webAsm d1 ctx clsr) in 
                           let (s, n_clsr) = (gen_webAsm_term t ctx new_clsr) in 
                           (f^"\n\n" ^s, n_clsr) 

and gen_webAsm_term (t:term) ctx clsr = match t with
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
                                                        ("(call_indirect $GG (i32.const 0) (get_global $"^x^") (i32.load (get_global $"^x^")))\n", clsr)
                                                ) else  
                                                       (try  
                                                (Context.find x ctx);
                                        ("(get_local $"^x^")", clsr)
                                      with Not_found -> raise (Unbound_Var x))
                           | Abs (func_name, name, arg_type, func_body) -> (let f_num: int = get_func_num () in
                                                                            let f_name = if func_name = "" then "FF"^(string_of_int f_num) else func_name in 
                                                                            
                                                                            func_index := Func_index.add f_num f_name !func_index;
                                                                            let header = "(func $"^f_name^
                                                                             "(param $"^name^" i32) (param $cl_add i32) (result i32) \n(local $this i32)\n" in
(*
	PDS: It's a bit ugly to grow the heap N times rather than to
	grow it once by 4N bytes. Also, consider calling a WA function
	high_alloc : i32 -> i32 rather than inline its body. The
	reason is you can then rewrite high_alloc (if you choose) to
	dynamically grow the linear memory when it is full.

	Also, what's the invariant on dynamic_heap_ctr?
	It looks like you're doing the equivalent of
		p += 4; mem[p] = closure
	rather than
		mem[p] = closure; p += 4
	both here and your code for references. Even if your code is
	correct, it is surprising and you should add a comment
	explaining why it's not wrong (e.g., a top of file comment
	describing run-time invariants and allocation).

	Also also, you have not accounted for low integrity closures
	(as far as I can tell). The way to do that is to first
	allocate a high-integrity closure (like you're doing) and then
	to use dynamic sealing. If that's unclear, ask Deepak or me to
	explain.
*)
                                                                let new_clsr = clsr + 1 in
                                                                let cl_init = ref ("(set_global $dynamic_heap_ctr (i32.add  "^
                                                                                   "(get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
                                                                                   "(set_local $this (get_global $dynamic_heap_ctr))\n"^
                                                                                   "(i32.store \n(get_global $dynamic_heap_ctr)\n(i32.const "^
                                                                                  string_of_int (f_num-1)^")\n)\n") in 
                                                               
                                                                                (* Store all values in ctx *) 
(*
	PDS: Closing over the entire context is a mistake. You should
	implement a function FV to compute the variables occurring
	free in an expression and then close over FV(func_body) \ DV
	where DV is the set of variables bound by top-level
	declarations. (The vars in DV are addressible at runtime
	without help from closures.)
*)
                                                                Context.iter (fun k v ->
                                                                                   cl_init := (!cl_init)^
                                                                                   "(set_global $dynamic_heap_ctr (i32.add  "^
                                                                                   "(get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
                                                                                   "(i32.store \n(get_global $dynamic_heap_ctr)\n"^
                                                                                   "(get_local $"^k^"))\n"
                                                                              ) ctx; 
                                                                let get_cl = ref "" in
                                                                Context.iter (fun k v ->
                                                                                get_cl := !get_cl^("(local $"^k^" i32)\n")
                                                                )       ctx;
                                                                
                                                                                (* Retrieve vars from closure *) 
                                                                Context.iter (fun k v -> 
                                                                                get_cl := !get_cl^(
                                                                            (*    "(set_local $cl_add (i32.add (get_local $cl_add) (i32.const 4)))\n"^*)
                                                                                "(set_local $cl_add (i32.add (get_local $cl_add) "^
                                                                                "(i32.const 4)))\n(i32.load (get_local $cl_add)\n)\n"^
                                                                                "(set_local $"^k^")\n")
                                                                        ) ctx; 
                                                                let new_ctx = Context.add name arg_type ctx in 
                                                                let (bd, new_clsr) = gen_webAsm func_body new_ctx new_clsr in 
                                                                funcs_code := !funcs_code^"\n\n"^header^(!get_cl)^bd^")\n";
                                                                try 
                                                                (!cl_init ^ "\n(get_local $this)", new_clsr)
                                                                with Not_found -> raise (Unbound_Var (string_of_int f_num))
                                                                )  
                           
                          | App (t1, t2) -> (match (typeOf ctx t1) with
(*
	PDS: Please implement left-to-right evaluation semantics,
	rather than right-to-left.
*)
                                            | F _ ->  let (code2, new_clsr) = gen_webAsm_term t2 ctx clsr in
                                                      let (code1, n_clsr) = gen_webAsm_term t1 ctx new_clsr in
         
                                                      (code2^code1^"(set_global $_this)\n(get_global $_this\n)\n(get_global $_this)\n(i32.load)\n(call_indirect $GG)\n", n_clsr)
         
                                            | _ -> raise (Application_failed "LHS not a func!")) 
                                          
                                                                                           
                          | Val x -> ("(i32.const "^string_of_int x^")\n", clsr)
                          | Ref t -> let res = "(set_global $dynamic_heap_ctr (i32.add (get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
                                               "(get_global $dynamic_heap_ctr)\n" in
                                     let (cd, new_clsr) = gen_webAsm_term t ctx clsr in
                                     (res ^ cd ^ "\n (i32.store) \n" 
                                     ^ "(get_global $dynamic_heap_ctr)\n", new_clsr)

                          | Unref t -> let res = "(set_global $dynamic_heap_ctr (i32.add (get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
(*
	PDS: This is surprising! Alongside store_low, you should also
	have imported a function alloc_low : i32 -> i32 for allocating
	low memory. You don't want to allocate low reference cells
	from the same memory as high reference cells.
*)
                                               "(get_global $dynamic_heap_ctr)\n" in
                                       let (cd, new_clsr) = gen_webAsm_term t ctx clsr in
                                        (res ^ cd ^ "\n (call $store_low) \n" 
                                        ^ "(get_global $dynamic_heap_ctr)\n", new_clsr)

                          | Deref t -> let (cd, new_clsr) = gen_webAsm_term t ctx clsr in
                                       (match typeOf ctx t with
                                       | Tref _ -> (cd ^ "\n(i32.load)\n", new_clsr)
                                       | Un _ -> (cd ^ "\n(call $load_low)\n", new_clsr)
                                       | _ -> raise (Eval_error "Derefing non-ref term")
                                       )
                          | Assign (t1, t2) -> (match typeOf ctx t1 with
                                                | Tref ty1 -> if ty1 != typeOf ctx t2 
                                                              then (raise (Eval_error "assignment type mismatch")) 
                                                              else
                                                                (let (code1, new_clsr) = gen_webAsm_term t1 ctx clsr in
                                                                 let (code2, new_clsr) = gen_webAsm_term t2 ctx new_clsr in
                                                                 (code1^code2^"(i32.store)\n", new_clsr)
                                                                )
                                                | _ -> raise (Eval_error "LHS in assignment not a Ref!"))
                          | Unit -> ("", clsr)
(*
	PDS: You actually want to pass around a dummy value of type
	i32 (0 will do) in the target language. This is the only way
	to achieve a uniform closure representation.
*)
                       



(*
	PDS: I will read the rest once you've updated to the treatment
	of declarations sketched in ./toy_typechecker/.
*)
let rec gen_webAsm_decls (d: global_decls) clsr =     
        match d with
        | Decl (sp, name, t) -> let (cd, new_clsr) = gen_webAsm_term (Abs (name, "dummy_x", I, Term t)) Context.empty clsr in
                                                  globals := !globals ^ "(global $"^name^" (mut i32) (i32.const 0))\n";
                                                  init_global := !init_global ^ cd ^ "(set_global $" ^ name ^ ")\n";
                                                  (
                                                          match sp with
                                                          | Public -> exports := !exports ^ "(export \""^name^"\" (func $"^name^"))\n"
                                                          | Private -> exports := !exports ^ ""; 
                                                  );
                                                  ("", new_clsr)
        | Decls (s, d) -> let (_, cl) = gen_webAsm_decls s clsr in gen_webAsm_decls (Decl d) cl 
                                                  

let create_code d (t: terms) = let (cd, cl) = (match d with
                                               | Some d' -> gen_webAsm_decls d' 0
                                               | None -> ("", 0)
                                              ) in
                                               let (cd, clsr) = (gen_webAsm t Context.empty cl) in
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
                                                !globals^
                                                "(export \"memory\" (memory $0))\n"^
                                                !exports ^  
                                                !funcs_code ^ "\n (func $mm (result i32)\n(local $this i32)\n"^
                                                !init_global^"\n"^cd^"\n)\n)"
                                               )  

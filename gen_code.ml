open Utils
open Map
open Printf
open Type_check

let rec gen_webAsm_term (t:term) ctx bv st = match t with
  | Var x -> 
         if BoundVars.mem x bv then ("(get_local $"^x^")", st) else if Global_ctx.mem x st.globals then ("(get_global $"^x^")", st) else raise (Unbound_Var x)
  | Abs (func_name, name, func_body) -> let f_num: int = get_func_num () in
                                                  let f_name = !module_name^"_"^(if func_name = "" then "FF"^(string_of_int f_num) else func_name) in 
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
                                                   
                                                   let bv = BoundVars.add name bv in 
                                                   let cl_init = ref ("(set_local $this (get_global $dynamic_heap_ctr))\n"^
                                                                      "(call $store_high (i32.const "^string_of_int (f_num-1)^"))\n") in

                                                   (* Store all values in ctx *) 
                                                   let fv = get_fv func_body (BoundVars.add name BoundVars.empty) st in
                                                   List.iter (fun k ->
                                                       cl_init := (!cl_init)^
                                                                  "(call $store_high (get_local $"^k^"))\n"
                                                     ) fv; 
                                                   let get_cl = ref "" in
                                                   List.iter (fun k  ->
                                                       get_cl := !get_cl^("(local $"^k^" i32)\n")
                                                     ) fv;

                                                   (* Retrieve vars from closure *) 
                                                   List.iter (fun k -> 
                                                       get_cl := !get_cl^(
                                                           (*    "(set_local $cl_add (i32.add (get_local $cl_add) (i32.const 4)))\n"^*)
                                                           "(set_local $cl_add (i32.add (get_local $cl_add) "^
                                                           "(i32.const 4)))\n(i32.load (get_local $cl_add)\n)\n"^
                                                           "(set_local $"^k^")\n")
                                                     ) fv; 
                                                   let (bd, st') = gen_webAsm_term func_body ctx bv st in 
                                                (*   Printf.printf "++  %s  ++" (List.hd st'.funcs_code);  *)
                                                   (!cl_init ^ "\n(get_local $this)", st')
  | App (t1, t2) -> let (code1, st) = gen_webAsm_term t1 ctx bv st in
                    let (code2, st) = gen_webAsm_term t2 ctx bv st in
   (code1^"(set_global $_this)\n"^code2^"(get_global $_this\n)\n(get_global $_this)\n(i32.load)\n(call_indirect $GG)\n", st)

  | Val x -> ("(i32.const "^string_of_int x^")\n", st)
  | Ref t ->  let (cd, st) = gen_webAsm_term t ctx bv st in
                  if infer_exp ctx t  == Tun then  
                      (cd ^ "\n (call $store_high) \n", st) 
              else 
                      (cd ^ "\n (call $store_low) \n", st) 

 | Deref t -> let (cd, st) = gen_webAsm_term t ctx bv st in
    (match infer_exp ctx t with  
     | Tref _ -> (cd ^ "\n(i32.load)\n", st)
     | Tun _ -> (cd ^ "\n(call $load_low)\n", st)
     | _ -> raise (Eval_error "Derefing non-ref term")
    )
  | Assign (t1, t2) -> let (code1, st) = gen_webAsm_term t1 ctx bv st in
           let (code2, st) = gen_webAsm_term t2 ctx bv st in
           (code1^code2^"(i32.store)\n", st)
  | Let (name,t1,t2) -> let (code1, st) = gen_webAsm_term t1 ctx bv st in
                        let bv = BoundVars.add name bv in 
                        let st = { st with globals = Global_ctx.add name (infer_exp ctx t1) st.globals } in 
                        let ctx = Context.add name (infer_exp ctx t1) ctx in
                        let (code2, st) = gen_webAsm_term t2 ctx bv st in
                        (code1^"\n (set_global $"^name^")\n"^code2, st)
  | Asc (e, t) -> gen_webAsm_term e ctx bv st 
  | Unit -> ("(i32.const 0)", st)


let rec gen_webAsm (t: tcompUnit) ctx st = match t with
  | Lcomp (name, t1, t2) -> let st = { st with globals = Global_ctx.add name (infer_exp ctx t1) st.globals } in 
                            let (cd1, st) = gen_webAsm_term t1 ctx BoundVars.empty st in
                            let (cd2, st) = gen_webAsm t2 ctx st in
                            (cd1^"\n (set_global $"^name^")\n"^cd2, st)

  | Lterm (name, t1, t2) -> let st = { st with globals = Global_ctx.add name (infer_exp ctx t1) st.globals } in 
                            let (cd1, st) = gen_webAsm_term t1 ctx BoundVars.empty st in
                            let (cd2, st) = gen_webAsm_term t2 ctx BoundVars.empty st in
                            (cd1^"\n (set_global $"^name^")\n"^cd2, st)

let rec check_export_types l gl = match l with
| [] -> ()
| (name, ty)::t -> if Global_ctx.find name gl = Tun then (check_export_types t gl) else raise (Export_error "Type mismatch!")


let compile_module (t:tcompUnit) exp = 
  let (cd, st) = (gen_webAsm t Context.empty empty_state) in
  match exp with
  | Exp l -> check_export_types l st.globals; (cd, st)
  | All -> Global_ctx.iter (fun k v -> if (v = Tun) then () else raise (Export_error "Val not Un!")) st.globals; (cd, st) 
     

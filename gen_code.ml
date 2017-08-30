open Utils
open Map
open Printf

let func_num = ref 0
let globals = ref ""
let get_func_num () = (func_num := !func_num + 1; !func_num) 

let global_ctx = ref Global_Ctx.empty
let func_index = ref Func_index.empty 

       
let rec type_check ctx x = match x with
        | Term t -> typeOf ctx t
        | Decs (d, t) -> if sanity ctx d then typeOf ctx t else raise (Type_resolution_failed "Non-unit expression")  
and sanity ctx = function 
        | Term t -> if typeOf ctx t == Unit then true else false
        | Decs (d, t) -> if sanity ctx d then (typeOf ctx t = Unit) else false
and typeOf ctx t :ty= match t with 
        | Var name -> if Global_Ctx.mem name !global_ctx then
               ( 
                
                       Global_Ctx.find name !global_ctx
               )
                        else  
                        ( try 
                Context.find name ctx
                        with Not_found -> raise (Type_resolution_failed ("Var "^name^" not found!")))
        | Abs (func_name, name, tp, b) -> let new_ctx = Context.add name tp ctx in 
                                                F (tp, type_check new_ctx b)
        | App (t1, t2) -> (match typeOf ctx t1 with
                                | F (a, b) -> if (typeOf ctx t2) = a then b
                                              else raise (Type_resolution_failed (
                                                      "Arg type mismatch!"^pr_type (typeOf ctx t1)^" and  "^pr_type (typeOf ctx t2)
                                                      )
                                              )
                                | _ -> raise (Type_resolution_failed "Exp not of func type!") 
                           )
        | Ref t1 -> Tref (typeOf ctx t1)
        | Deref t1 -> (match typeOf ctx t1 with
                     | Tref y -> y
                     | _ -> raise (Type_resolution_failed "Can not deref a non ref value!"))
        | Assign (t1, t2) -> if (typeOf ctx t1) = Tref (typeOf ctx t2) then Unit else raise (Type_resolution_failed "Assign!")
        | Unit -> Unit 
        | Val _ -> I
        | Decl (s, n, t) -> global_ctx := Global_Ctx.add n (typeOf ctx t) !global_ctx; Unit

let funcs_code = ref ""
let init_global = ref ""
let exports = ref "(export \"main\" (func $mm))\n" 


let rec gen_webAsm d ctx clsr = match d with
        | Term t -> gen_webAsm_term t ctx clsr
        | Decs (d1, t) -> let (f, new_clsr) = (gen_webAsm d1 ctx clsr) in let (s, n_clsr) = (gen_webAsm_term t ctx new_clsr) in (f^"\n\n" ^s, n_clsr) 
and gen_webAsm_term (t:term) ctx clsr = match t with
                           | Var x -> 
                                        if Global_Ctx.mem x !global_ctx then 
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
                                                                let new_clsr = clsr + 1 in
                                                                let cl_init = ref ("(set_global $dynamic_heap_ctr (i32.add  "^
                                                                                   "(get_global $dynamic_heap_ctr) (i32.const 4)))\n"^
                                                                                   "(set_local $this (get_global $dynamic_heap_ctr))\n"^
                                                                                   "(i32.store \n(get_global $dynamic_heap_ctr)\n(i32.const "^
                                                                                  string_of_int (f_num-1)^")\n)\n") in 
                                                               
                                                                                (* Store all values in ctx *) 
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

                          | Deref t -> let (cd, new_clsr) = gen_webAsm_term t ctx clsr in
                                       (cd ^ "\n(i32.load)\n", new_clsr)
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
                          | Decl (sp, name, t) -> (if Context.cardinal ctx != 0 then raise (Eval_error "Non global decl!\n") else
                                                  let (cd, new_clsr) = gen_webAsm_term (Abs (name, "dummy_x", I, Term t)) ctx clsr in
                                                  globals := !globals ^ "(global $"^name^" (mut i32) (i32.const 0))\n";
                                                  init_global := !init_global ^ cd ^ "(set_global $" ^ name ^ ")\n";
                                                  (
                                                          match sp with
                                                          | Public -> exports := !exports ^ "(export \""^name^"\" (func $"^name^"))\n"
                                                          | Private -> exports := !exports ^ ""; 
                                                  );
                                                  ("", new_clsr))
                      
                                                  

let create_code t = let (cd, clsr) = (gen_webAsm t Context.empty 0) in
                                       let tab_ins = ref "" in
                                       for k=1 to clsr do
                                               tab_ins := (!tab_ins)^
                                               "(elem (i32.const "^string_of_int (k-1)^
                                               ") $"^Func_index.find k !func_index^")\n"
                                       done;
                                       ("(module \n"^
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
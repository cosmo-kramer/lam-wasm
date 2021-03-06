open Utils

let rec link (modules: (string*state) list) = 
  let tab_ins = ref "" in
  Func_index.iter (fun k v ->  
               tab_ins := (!tab_ins)^
               "(elem (i32.const "^string_of_int (v-1)^
               ") $"^k^")\n") !func_index;
  let module_funcs_appended = ref "" in
  let globals_init = ref "" in
  let start_func = ref "" in
  let call_low_funcs = ref [] in
  let call_low_offset = ref 0 in
  List.iter (fun (cd, st) -> 
          start_func := !start_func ^ cd ^ "\n";
          module_funcs_appended := !module_funcs_appended ^ "\n" ^ 
                                    (String.concat "" st.funcs_code);      
          Global_ctx.iter (
                  fun k v -> globals_init := !globals_init ^ 
                                             "(global $"^k^" (mut i32) (i32.const 0))\n"
                ) st.globals;
          ) modules;

  
("(module \n"^
   "(func $load_low (import \"low\" \"load\") (param i32) (result i32))\n"^
   "(func $store_low (import \"low\" \"store\") (param i32) (param i32))\n"^     
   "(type $GG (func (param i32) (param i32) (result i32)))\n"^
   "(table " ^ string_of_int !func_num ^ " anyfunc)\n"^(!tab_ins)^
   "(memory $0 100)\n"^
   "(global $dynamic_heap_ctr (mut i32) (i32.const "^string_of_int !low_integrity^"))\n"^
   "(global $_this (mut i32) (i32.const 0))\n"^
   "(func $store_high (param $0 i32)
           
             (i32.store (get_global $dynamic_heap_ctr) (get_local $0))\n
             (set_global $dynamic_heap_ctr (i32.add (get_global $dynamic_heap_ctr) (i32.const 4)))\n
      )
   (func $call_low (param $0 i32) (result i32)
          (select
                (i32.load (get_local $0))
                (i32.const -1)
                (i32.lt_s
                        (get_local $0)
                        (i32.const "^string_of_int !low_integrity^")
                )
          )    

   )"^
      !globals_init^
"(export \"memory\" (memory $0))\n(export (func $start))\n"^
   !module_funcs_appended^ "\n (func \"mm\" $start (result i32)\n(local $this i32)\n"^
   !start_func^"\n)\n)"
)



open Why3
open Format
open Utils
(* reads the config file *)
let config : Whyconf.config = Whyconf.read_config None
(* the [main] section of the config file *)
let main : Whyconf.main = Whyconf.get_main config
(* all the provers detected, from the config file *)
let provers : Whyconf.config_prover Whyconf.Mprover.t =
  Whyconf.get_provers config
let env : Env.env =
  Env.create_env (Whyconf.loadpath main)
(* loading the Alt-Ergo driver *)

let int_theory : Theory.theory =
  Env.read_theory env ["int"] "Int"
let why3_true = Term.t_true

let plus_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.Theory.th_export ["infix +"] 
let le_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.Theory.th_export ["infix <="] 
let eq_symbol : Term.lsymbol = Theory.ns_find_ls int_theory.Theory.th_export ["infix ="] 

let rec convert phi x var = match phi with
        | Val n ->  Term.t_const (Number.ConstInt (Number.int_const_dec (string_of_int n))) Ty.ty_int
        | Var y ->  let var_x = if x == y then var else Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int in Term.t_var var_x 
        | Plus (e1, e2) -> Term.t_app_infer plus_symbol [convert e1 x var; convert e2 x var]
        | _ -> raise (Error "Converting term to why3 expression failed!\n"); Term.t_true

let rec get_fml phis x var_x = match phis with 
       | [] -> Term.t_true
       | h::t -> Term.t_and (match h with
                  | Eq (t1, t2) -> Term.ps_app eq_symbol [convert t1 x var_x;convert t2 x var_x]
                  | Leq (t1, t2) -> Term.ps_app le_symbol [convert t1 x var_x;convert t2 x var_x]
                  | Tr -> Term.t_true
                  | _ -> raise (Error "Unrecognized refinement for why3!\n") 
                 ) (get_fml t x var_x)

let forall_close x phi1 phi2 =
        let var_x = Term.create_vsymbol (Ident.id_fresh x) Ty.ty_int in 
        let fml1 = get_fml [phi1] x var_x in
        let fml2 = get_fml [phi2] x var_x in
        Term.t_forall_close [var_x] [] (Term.t_implies fml1 fml2) 
        (*        Term.t_forall_close [idX] [] t *)



let check_alt_ergo fml: unit = 
if !debug then (printf "@[formula is:@ %a@]@." Pretty.print_term fml) else ();
let task1 : Task.task = None (* empty task *) in

let task1 = Task.use_export task1 int_theory in
let goal_id1 : Decl.prsymbol =
  Decl.create_prsymbol (Ident.id_fresh "goal1") in

let task1 : Task.task =
  Task.add_prop_decl task1 Decl.Pgoal goal_id1 fml in
let alt_ergo : Whyconf.config_prover =
let prover = {Whyconf.prover_name = "Alt-Ergo";
                  prover_version = "1.30";
                  prover_altern = ""} in
    Whyconf.Mprover.find prover provers in
let alt_ergo_driver : Driver.driver =
          Whyconf.load_driver main env alt_ergo.Whyconf.driver [] in
let result2 : Call_provers.prover_result =
   Call_provers.wait_on_call
    (Driver.prove_task ~command:alt_ergo.Whyconf.command
    ~limit:Call_provers.empty_limit
    alt_ergo_driver task1) in
(match result2.pr_answer with
| Valid -> ()
| Invalid -> raise (Error "Invalid why3 formulae!");
| _ -> raise (Error "Why3 failed!\n");
) 

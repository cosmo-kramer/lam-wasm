open Lexer
open Parser
open Printf 
open Utils
open Linker

let parse_from_string s =
  let lex = Lexing.from_string s in
      Lexing.(lex.lex_curr_p <- {lex.lex_curr_p with pos_cnum = 0});
      Parser.program Lexer.identifier lex


let read_to_list fname = let inp = open_in fname in 
                           let s = ref [] in 
                           try 
                                   while true; 
                                        do s := (input_line inp)::(!s) 
                                   done; 
                                    [] with End_of_file -> !s;;


let () = let modules = ref [] in
         for i = 1 to Array.length Sys.argv - 1 do
                if Sys.argv.(i) = "--v" then debug := true else
                (let code = String.concat ""  (List.rev (read_to_list Sys.argv.(i))) in
                 module_name := Sys.argv.(i);
                 modules := (parse_from_string code)::!modules;
                ) 
          done; 
          let fl = open_out "test.wast" in
          Printf.fprintf fl "%s" (link !modules);
          Printf.printf "Compiled Successfully\n";
                

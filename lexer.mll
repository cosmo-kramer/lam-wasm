{
       open Parser
  open Printf
      module L = Lexing 
          module B = Buffer

   let set_filename (fname:string) (lexbuf:L.lexbuf)  =
           ( lexbuf.L.lex_curr_p <-  
                   { lexbuf.L.lex_curr_p with L.pos_fname = fname }
                   ; lexbuf
   )


(*
   type token = 
           | IDENTIFIER of string
           | EOF
           | ERROR 
           | LAMBDA
           | REF
           | DEREF
           | ASSIGN 
*)        
     let line_num = ref 0

   exception Eof
                   }
        
   let space = [' ' '\t']
   let new_line = ['\n']
   let digit = ['0'-'9']
   let alph = ['a'-'z']
   let lam = ['?'] 
   rule identifier = parse
   | space+       {identifier lexbuf}
   | new_line+         {Lexing.new_line lexbuf; identifier lexbuf }
   | alph+(alph*digit*)* as name      { IDENTIFIER(name)      }
   | eof        {raise Eof}
   | lam { LAMBDA }
   | ':' { COL }
   | '(' { OP_BR }
   | ')' { CL_BR }
   | '.' { DOT }
   | "->" { ARROW }
   | _  {printf "Found wrong token";ERROR}
  
  {
   let main () = 
           
           try
           let lexbuf = set_filename "stdin" @@ L.from_channel stdin in
           (while true do
                   Parser.program  identifier lexbuf 
           done)
           with Eof -> exit 0
(*           let rec loop acc =  function
                   | EOF   ->  to_string EOF :: acc |> List.rev
                   | ERROR -> printf "Bad Token"; acc |> List.rev 
                        | x     ->  loop (to_string x :: acc) (identifier lexbuf)
           in
                loop [] (identifier lexbuf) 
                        |> String.concat " " 
                        |> print_endline                
*)
   let () = main ()        
  }








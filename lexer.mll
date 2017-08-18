{
       open Parser
  open Printf

   let load_buf (fname:string) (lexbuf:Lexing.lexbuf)  =
           ( lexbuf.Lexing.lex_curr_p <-  
                   { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
                   ; lexbuf
   )


(*
   type token = 
           | IDENTIFIER of string
           | EOF
           | ERROR 
           | AMBDA
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
   | "Ref" { REF }
   | "|" { DEREF }
   | "=" { ASSIGN }
   | "0" { ZERO }
   | _  {printf "Found wrong token";ERROR}
  
  {
   let main () = 
           
           try
                   let lexbuf = load_buf "stdin" @@ Lexing.from_channel stdin in
                   (while true do
                           Parser.program  identifier lexbuf 
                   done)
           with Eof -> exit 0
   let () = main ()        
  }








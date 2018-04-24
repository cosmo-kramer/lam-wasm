{
       open Parser
  open Printf

   let load_buf (fname:string) (lexbuf:Lexing.lexbuf)  =
           ( lexbuf.Lexing.lex_curr_p <-  
                   { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = fname }
                   ; lexbuf
   )

     let line_num = ref 0

   exception Eof
                   }
        
   let space = [' ' '\t']
   let new_line = ['\n']
   let digit = ['0'-'9']
   let alph = ['a'-'z']
   let cap = ['A'-'Z']
   let lam = ['?'] 
   rule identifier = parse
   | space+       {identifier lexbuf}
   | new_line+         {Lexing.new_line lexbuf; identifier lexbuf }
   | eof        { EOF }
   | lam { LAMBDA }
   | "let" { Let }
   | "in" { IN }
   | ':' { COL }
   | '(' { OP_BR }
   | ')' { CL_BR }
   | '.' { DOT }
   | '<' { LEQ }
   | "->" { ARROW }
   | "Un" { Un }
   | "Ref" { REF }
   | "Uref" { UNREF }
   | "True" { True }
   | "Unit" { Unit }
   | "," { COMMA }
   | "+" { PLUS }
   | "|" { DEREF }
   | "=" {EQ }
   | '_' {UNS}
   | ';' { SCOL }
   | '{' { OP_BRACES }
   | '}' { CL_BRACES }
   | ['0'-'9']+ as v { VAL v}
   | cap+(alph*digit*cap*)*  as name { Cons(name)  }
   | alph+(alph*digit*)* as name      { IDENTIFIER(name)      }
   | _  {printf "Found wrong token";ERROR}

   
(*   {
   let main () = 
           
           try
                   let lexbuf = load_buf "stdin" @@ Lexing.from_channel stdin in
                   (while true do
                           Parser.program  identifier lexbuf 
                   done)
           with Eof -> exit 0
   let () = main ()        
  }
*)







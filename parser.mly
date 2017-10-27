%{
open Printf 
open Pervasives
open Lexing
open Utils
open Type_check
open Gen_code
%}

%token <string> IDENTIFIER
%token EOF 
%token LAMBDA 
%token REF
%token DEREF
%token EQ
%token ERROR
%token COL
%token DOT
%token OP_BR
%token CL_BR
%token ARROW
%token SCOL 
%token PRIVATE
%token PUBLIC
%token EOF
%token UNREF
%token Let
%token IN
%token <string> VAL  
%start program
%type <unit> program
%type <term> term
%type <ty> type_identifier 
%type <global_decls> global_decls
%%

program: global_decls term EOF {
        printf "%s" (term_to_string $2);
        
        let fl = open_out "test.wast" in
        Printf.fprintf fl "%s" (create_code (Some $1) $2 empty_state);
        exit 0;
}
| term EOF {
        let fl = open_out "test.wast" in
        Printf.fprintf fl "%s" (create_code None $1 empty_state);
        exit 0; 
} 


type_identifier : type_identifier2 ARROW type_identifier2 {
        Tfun ($1, $3)
}
| IDENTIFIER {
        Tint
}
| REF type_identifier{
        Tref $2
}
type_identifier2 : OP_BR type_identifier CL_BR {
       $2 
}
| IDENTIFIER {
        Tint
}

term : IDENTIFIER  {
        Var $1        
}
| LAMBDA IDENTIFIER DOT term {
        Abs ("", $2, $4) 
} 
| OP_BR term term CL_BR {
        App ($2, $3)
}
| REF term{
        Ref $2
}
| DEREF term{
        Deref $2
}
| EQ term term {
        Assign ($2, $3)
}
| VAL {
        Val (int_of_string $1)
}
| Let IDENTIFIER EQ term IN term {
        Let ($2, $4, $6)
}
| OP_BR term COL type_identifier CL_BR {
        $2
}

decl : IDENTIFIER EQ OP_BR term CL_BR SCOL {
        ($1, $4)
}

global_decls : global_decls decl {
        Decls ($1, $2)
}
| decl {
        Decl $1 
}

%%

%{
open Printf 
open Pervasives
open Lexing
open Utils
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

%token <string> VAL  
%start program
%type <specifier> specifier 
%type <unit> program
%type <term> term
%type <terms> terms
%type <ty> type_identifier 
%type <global_decls> global_decls
%%

program: global_decls terms EOF {
        type_check_decl $1 Context.empty;
        type_check Context.empty $2;
        printf "%s" (terms_to_string $2);
        
        let fl = open_out "test.wast" in
        Printf.fprintf fl "%s" (create_code (Some $1) $2);
        exit 0;
}
| terms EOF {
        type_check Context.empty $1;
        let fl = open_out "test.wast" in
        Printf.fprintf fl "%s" (create_code None $1);
        exit 0; 
} 


terms:  terms SCOL term {
        (*        printf "Pretty print:    %s      \n"  (to_string $1) ; 
                printf "\n %s \n" (pr_type (typeOf Context.empty $1));*)
        
        Terms ($1, $3)

} | term  {
        Term $1
} | OP_BR terms CL_BR {
        $2;
}  

type_identifier : type_identifier2 ARROW type_identifier2 {
        F ($1, $3)
}
| IDENTIFIER {
        I
}
| REF type_identifier{
        Tref $2
}
type_identifier2 : OP_BR type_identifier CL_BR {
       $2 
}
| IDENTIFIER {
        I
}

specifier : PRIVATE {
        Private
}
| PUBLIC {
        Public
}

term : IDENTIFIER  {
        Var $1        
}
| LAMBDA IDENTIFIER COL type_identifier DOT OP_BR terms CL_BR {
        Abs ("", $2, $4, $7) 
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
| OP_BR term CL_BR {
        $2
}

decl : specifier IDENTIFIER EQ OP_BR term CL_BR SCOL {
        ($1, $2, $5)
}

global_decls : global_decls decl {
        Decls ($1, $2)
}
| decl {
        Decl $1 
}

%%

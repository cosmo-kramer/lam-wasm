%{
open Printf 
open Pervasives
open Lexing
open Utils
%}

%token <string> IDENTIFIER
%token EOF 
%token LAMBDA 
%token REF
%token DEREF
%token ASSIGN
%token ERROR
%token COL
%token DOT
%token OP_BR
%token CL_BR
%token ARROW


%start program
%type <unit> program
%type <term> term
%type <ty> type_identifier 
%%

program:  term {
        
        printf "Pretty print:    %s      \n"  (to_string $1) ; 
                printf "\n %s \n" (pr_type (typeOf global_context $1));
                printf "Reduced ->   %s \n" (to_string (eval global_context $1));

}

type_identifier : type_identifier2 ARROW type_identifier2 {
        F ($1, $3)
}
| IDENTIFIER ARROW IDENTIFIER{
        F (I, I)
}
| IDENTIFIER {
        I
}

type_identifier2 : OP_BR type_identifier CL_BR {
       $2 
}

term : IDENTIFIER  {
        Var $1        
}
| LAMBDA IDENTIFIER COL type_identifier DOT term {
        Abs ($2, $4, $6) 
} 
| OP_BR term term CL_BR {
        App ($2, $3)
}


%%

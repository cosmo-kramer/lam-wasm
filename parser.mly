%{
open Printf 
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

%start program
%type <unit> program
%type <term> term

%%

program:  term {
        
        printf "Pretty print:    %s      \n"  (to_string $1) ; 
                printf "\n\n";
                printf "Reduced ->   %s \n" (to_string (eval global_context $1));

}


term : IDENTIFIER  {
        Var $1        
}
| LAMBDA IDENTIFIER COL IDENTIFIER DOT term {
        Abs ($2, I, $6) 
} 
| OP_BR term term CL_BR {
        App ($2, $3)
}


%%

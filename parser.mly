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
%token COMMA 
%token PRIVATE
%token PUBLIC
%token EOF

%token <string> VAL  
%start program
%type <unit> program
%type <term> term
%type <decs> terms
%type <ty> type_identifier 
%%

program: terms EOF {
        printf "CCCCCCCCCCCCCCC\n";
        type_check Context.empty $1;
        printf "%s" (dec_to_string $1);
        
        
        let fl = open_out "test.wast" in
        Printf.fprintf fl "%s" (create_code $1);
        exit 0;
}


terms:  terms COMMA term {
        printf "BBBBBBBBB\n";
        (*        printf "Pretty print:    %s      \n"  (to_string $1) ; 
                printf "\n %s \n" (pr_type (typeOf Context.empty $1));*)
        
        Decs($1, $3)

} | term  {
        printf "AAAAAAAA\n";
        Term $1
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


term : IDENTIFIER  {
        Var $1        
}
| LAMBDA IDENTIFIER COL type_identifier DOT OP_BR terms CL_BR {
        printf "LAMM!\n";
        Abs ($2, $4, $7) 
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
| ASSIGN term term {
        Assign ($2, $3)
}
| VAL {
        Val (int_of_string $1)
}
| OP_BR term CL_BR {
        $2
}

%%

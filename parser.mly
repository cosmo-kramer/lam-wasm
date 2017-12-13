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
%token COMMA 
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
%token Un
%token PRIVATE
%token PUBLIC
%token EOF
%token UNREF
%token Let
%token IN
%token <string> VAL  
%start program
%type <(string*Utils.state)> program
%type <texports> exports
%type <term> term
%type <ty> type_identifier
%type <tcompUnit> compUnit 
%%

program:  exports compUnit {
        let fl = open_out "test.wast" in
        Printf.printf "\n%s\n" (compUnit_to_string $2);
        compile_module $2 $1 
} 

exports : {
        All
}
| OP_BR list_of_exp CL_BR {
        Exp $2
}

list_of_exp : {
        []
}
| IDENTIFIER COL type_identifier COMMA list_of_exp {
        ($1, $3)::$5
}
| IDENTIFIER COL type_identifier {
        [($1, $3)]
}


compUnit :  Let IDENTIFIER EQ term IN compUnit {
        Lcomp ($2, $4, $6)
}
| Let IDENTIFIER EQ term IN term {
        Lterm ($2, $4, $6)
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
| Un {
        Tun
}

type_identifier2 : OP_BR type_identifier CL_BR {
       $2 
}
| IDENTIFIER {
        Tint
}
| Un {
        Tun
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
| term COL type_identifier {
        Asc ($1, $3)
}
| OP_BR term CL_BR {
        $2
}
| Let IDENTIFIER EQ term IN term {
        Let ($2, $4, $6)
}

%%

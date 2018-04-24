%{
open Printf 
open Pervasives
open Lexing
open Utils
open Type_check
open Gen_code
%}

%token <string> IDENTIFIER
%token <string> Cons
%token EOF
%token COMMA 
%token Unit
%token LAMBDA 
%token REF
%token DEREF
%token CL_BRACES
%token OP_BRACES
%token True
%token LEQ
%token EQ
%token ERROR
%token UNS
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
%token X
%token PLUS
%token <string> VAL  
%start program
%type <refinement> phi
%type <(string*Utils.state)> program
%type <string> BaseT
%type <texports> exports
%type <term> term
%type <ty> type_identifier
%type <tcompUnit> compUnit 
%%

program:  exports type_decls compUnit {
        compile_module $3 $1
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

type_decls : IDENTIFIER EQ ConsList type_decls {
        add_base_type (Base ($1, $3))
}
| {
        
}

ConsList : Cons param_list {
        [($1, $2)]
} 
| Cons param_list DEREF ConsList {
        ($1, $2)::$4
}

param_list : type_identifier param_list {
       $1::$2
}
| {
        []
}

compUnit :  Let IDENTIFIER EQ term IN compUnit {
        Lcomp ($2, $4, $6)
}
| term {
        Lterm $1
}

BaseT : IDENTIFIER {
        $1
}

phi : term EQ term {
        isPure $1;isPure $3;
        Eq ($1, $3)
}
| Un OP_BR term CL_BR {
          isPure $3;
          Un $3
} 
| term LEQ term {
         isPure $1;isPure $3;
         Leq ($1, $3)
}
| True {
        Tr
}

type_identifier : type_identifier2 ARROW type_identifier2 {
        Tfun ($1, $3)
}
| OP_BRACES IDENTIFIER COL BaseT DEREF phi CL_BRACES {
        R ($2, $4, $6)
}
| OP_BRACES IDENTIFIER COL type_identifier DEREF phi CL_BRACES {
        Rty ($2, $4, $6)
}
| OP_BR type_identifier COMMA type_identifier CL_BR
{
        Tpair ($2, $4)
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
| OP_BRACES IDENTIFIER COL BaseT DEREF phi CL_BRACES {
      R ($2, $4, $6)
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
| term PLUS term {
        Plus ($1, $3)
}
| OP_BR term CL_BR {
        $2
}
| Let IDENTIFIER EQ term IN term {
        Let ($2, $4, $6)
}
| Cons para_list  {
        Constructor ($1, $2)
}
| OP_BR term COMMA term CL_BR {
        Pair ($2, $4)
}
| Unit {
        Unit
}
para_list : term COMMA para_list {
        $1::$3
}
| {
        []
}

%%

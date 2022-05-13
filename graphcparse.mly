/* Ocamlyacc parser for GraphC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LSQUARE RSQUARE PLUS MINUS MULTI DIVIDE MODULUS ASSIGN
%token EQ NEQ LT GT LTE GTE AND OR 
%token IF ELSE WHILE FOR INT BOOL STRING VOID NODE EDGE LIST NODELIST GRAPH EDGELIST 
/* return, COMMA token */
%token RETURN COMMA PRINT
%token <int> LITERAL
%token <string> STRINGLIT
%token <bool> BLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%nonassoc NOELSE 
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left EQ NEQ
%left LT GT LTE GTE
%left PLUS MINUS
%left MULTI DIVIDE MODULUS

%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl decls { (($1 :: fst $2), snd $2) } /*(($1 :: fst $3), snd $3)*/
 | fdecl decls { (fst $2, ($1 :: snd $2)) } /*raise (Failure "hrer")*/

/* int x */
vdecl:
  typ ID SEMI { ($1, $2) }

typ:
    INT    { Int   }
  | BOOL   { Bool  }
  | STRING { String }
  | VOID   { Void }
  | NODE   { Node }
  | EDGE  {Edge}
  | GRAPH {Graph}
  | NODELIST { NList }
  | EDGELIST {EList}

/* fdecl */
fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  {
    {
      rtyp=$1;
      fname=$2;
      formals=$4;
      body=$7
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }

formals_list:
  typ ID { [($1,$2)] }
  | typ ID COMMA formals_list { ($1,$2)::$4 }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1     }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt %prec NOELSE            { If($3, $5, Block([]))  }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  | FOR LPAREN expr SEMI expr SEMI expr RPAREN stmt    { For ($3, $5, $7, $9)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }
  | typ ID ASSIGN expr SEMI                 { Dec($1, $2, Assign($2,$4))}
  | typ ID SEMI                             { Dec($1, $2, Noexpr)}



expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | STRINGLIT        { StringLit($1)          }
  | ID               { Id($1)                } 
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr MULTI  expr { Binop($1, Multi,   $3) }
  | expr DIVIDE  expr { Binop($1, Divide,   $3) }
  | expr MODULUS  expr { Binop($1, Modulus,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Gt,  $3)   }
  | expr LTE     expr { Binop($1, Lte,  $3)   }
  | expr GTE     expr { Binop($1, Gte,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID ASSIGN expr   { Assign($1, $3)         }
  | PRINT LPAREN expr RPAREN { Print($3) }
  | LPAREN expr RPAREN { $2                   }
  /* call */
  | ID LPAREN args_opt RPAREN { Call ($1, $3) } /*Call ($1, $3)*/

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }

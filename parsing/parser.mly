%{
  open Ast

  let mk_binop nm l r = let op = Var (nm) in
                          mk_app op [l; r]

%}

%token ARROW
%token AT
%token BACKSLASH
%token BACKTICK
%token BANG
%token BAR
%token BAR_GT
%token BEGIN
%token COLON
%token COMMA
%token DIV
%token DOT
%token DOUBLE_SEMI
%token <string> ID
%token <float> FLOAT
%token <string> COMMENT
%token ELSE
%token EOF
%token END
%token EQ
%token EQUAL
%token EXTERN
%token FUNCTION
%token GE
%token GT
%token IF
%token IN
%token <string> INFIXOP0
%token <string> INFIXOP1
%token <string> INFIXOP2
%token <string> INFIXOP3
%token <string> INFIXOP4
%token <int> INT
%token LBRACE
%token LE
%token LET
%token LPAREN
%token LT
%token LSQ_BRACKET
%token LT_BAR
%token MINUS
%token OPEN
%token <string> PREFIXOP
%token PLUS
%token RANGE_OP
%token RPAREN
%token RBRACE
%token RSQ_BRACKET
%token SEMI
%token <string> STRING
%token THEN
%token TIMES
%token UNIT

/* Precedences and associativities.*/
%left SEMI
%left PLUS MINUS
%left TIMES DIV BACKTICK
%left RANGE_OP
%nonassoc UMINUS

%start toplevel
%type <Ast.toplevel> toplevel

%start term_type
%type <Ast.t> term_type
%%

toplevel :
  | expr DOUBLE_SEMI              { Expr ($1) }
  | OPEN STRING                   { Open ($2) }
  | EXTERN ID COLON term_type DOUBLE_SEMI    { Extern ($2, $4) }
;


term_type :
  | ID                        { TApp ($1, []) }
  | term_type ARROW term_type { TApp ("->", $1::[$3]) }

params :
  | ID                        { [ $1 ] }
  | ID params                 { $1 :: $2 }


/* function arguments */
fargs :
  | expr               { [ $1 ] }
  | expr fargs         { $1::$2 }


pat :
  | ID                 { $1}
;

expr :
  | INT                         { IntLit ($1) }
  | FLOAT                       { FloatLit ($1) }
  | STRING                      { StrLit ($1) }
  | ID                          { Var ($1) }
  | BACKSLASH params ARROW expr { Lam ($2, $4) }
  | BACKSLASH params DOT expr   { Lam ($2, $4) }
  | FUNCTION params ARROW expr  { Lam ($2, $4) }
  | let_expr                    { $1 }
  | infix_expr                  { $1 }
  | expr fargs                  { mk_app $1 $2 }
  | LPAREN expr RPAREN          { $2 }
  | IF expr THEN expr ELSE expr { If ($2, $4, $6) }
  | expr BACKTICK ID BACKTICK expr { mk_app (Var ($3)) [$1; $5] }
  | op_expr                      { $1 }
  | expr SEMI expr               { 
      let nm = Utils.get_new_name "var" in
	Let (nm, $1, $3)
    }
  | LPAREN tuple_expr RPAREN     { Tuple $2 }
  | UNIT                         { Tuple [] }
;

infix_expr : 
  | expr INFIXOP0 expr          { mk_binop $2 $1 $3 }
  | expr INFIXOP1 expr          { mk_binop $2 $1 $3 }
  | expr INFIXOP2 expr          { mk_binop $2 $1 $3 }
  | expr INFIXOP3 expr          { mk_binop $2 $1 $3 }
  | expr INFIXOP4 expr          { mk_binop $2 $1 $3 }

/*FIXME : need to simplify rules */
let_expr :
  | LET ID EQUAL expr IN expr   { Let ($2, $4, $6)  }
  | LET ID EQUAL expr { 
      let nm = $2 in
      let var = Var (nm) in
	Let (nm, $4, var)
    }
  | LET LPAREN operator RPAREN EQUAL expr {
      let e1 = $6 in
      let nm = $3 in
      let var = Var (nm) in
	Let (nm, e1, var)
    }
  | LET ID params EQUAL expr    { 
      let func = Lam ($3, $5) in
      let var  = Var ($2) in
	Let ($2, func, var)
    }
  | LET LPAREN operator RPAREN  params EQUAL expr {
      let func = Lam ($5, $7) in
      let nm = $3 in
      let var = Var (nm) in
	Let (nm, func, var)
    }
;

operator :
  | PLUS       { "+" }
  | MINUS      { "-" }
  | TIMES      { "*" }
  | DIV        { "/" }
  | GT         { ">" }
  | LT         { "<" }
  | EQ         { "==" }
  | RANGE_OP   { ".." }
  | INFIXOP0   { $1 }
  | INFIXOP1   { $1 }
  | INFIXOP2   { $1 }
  | INFIXOP3   { $1 }
  | INFIXOP4   { $1 }
;

op_expr :
  | expr PLUS expr              { mk_binop "+" $1 $3 }
  | expr MINUS expr             { mk_binop "-" $1 $3 }
  | expr TIMES expr             { mk_binop "*" $1 $3 }
  | expr DIV expr               { mk_binop "/" $1 $3 }
  | expr GT expr                { mk_binop ">" $1 $3 }
  | expr LT expr                { mk_binop "<" $1 $3 }
  | expr EQ expr                { mk_binop "==" $1 $3 }
  | expr GE expr                { mk_binop ">=" $1 $3 }
  | expr LE expr                { mk_binop "<=" $1 $3 }
  | expr RANGE_OP expr          { mk_binop ".." $1 $3 }
  | expr LT_BAR expr            { mk_binop "<|" $1 $3 }
  | expr BAR_GT expr            { mk_binop "|>" $1 $3 }
  | expr BAR expr               { mk_binop "|" $1 $3 }
  | expr BANG expr BAR_GT expr  { mk_app (mk_binop "!|>" $1 $3) [$5] }
  | expr LT_BAR expr BANG expr  { mk_app (mk_binop "<|!" $1 $3) [$5] }
  | expr LSQ_BRACKET INT RSQ_BRACKET {
      Select ($1, $3)
    }
  | ID COLON expr BAR expr      { 
      let nm = $1 in
      let var = Var (nm) in
      let binop = mk_binop ":|" $3 $5 in
	Let (nm, binop, var)
    }
;

tuple_expr :
  | expr                        { [$1] }
  | expr COMMA tuple_expr       { $1 :: $3 }
  | expr COMMA expr             { $1 ::[$3] }
;


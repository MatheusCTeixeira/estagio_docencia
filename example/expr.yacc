%%

%name Expr
%term NUM of int | ADD  | SUB  | MUL  | DIV | EOF | EOP
%nonterm EXPR of int | START of int
%pos int
%eop EOP

%left ADD SUB
%left MUL DIV

%start START
%noshift EOF

%%

START   : EXPR EOP      (print(EXPR))

EXPR    : NUM           (NUM)
        | EXPR ADD EXPR (EXPR1 + EXPR2)
        | EXPR SUB EXPR (EXPR1 - EXPR2)
        | EXPR MUL EXPR (EXPR1 * EXPR2)
        | EXPR DIV EXPR (EXPR1 div EXPR2)

structure Tokens = Tokens

fun toInt s =
    case Int.fromString s of
         SOME i => i
       | NONE => 0;

type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token

val pos = ref 0
val eof = fn () => Tokens.EOF(!pos,!pos)
    
%%
%header (functor ExprLexFun(structure Tokens: Expr_TOKENS));
digit=[0-9];
ws = [\ \t];
%%
\n       => (pos := (!pos) + 1; lex());
{ws}+    => (lex());
{digit}+ => (Tokens.NUM(toInt(yytext), !pos,!pos));
"+"      => (Tokens.ADD(!pos,!pos));
"*"      => (Tokens.MUL(!pos,!pos));
";"      => (Tokens.EOP(!pos,!pos));
"-"      => (Tokens.SUB(!pos,!pos));
"/"      => (Tokens.DIV(!pos,!pos));
"."      => (lex());

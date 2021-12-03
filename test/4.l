%s MAGIC
magic magic
%%

{magic} { BEGIN MAGIC; }
<MAGIC>.+ { BEGIN 0; printf("magic: "); ECHO; }


%%

int yywrap() { return 1; }

int main() {
    yylex();
    return 0;
}

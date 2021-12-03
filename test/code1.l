%%

[\t\n ]	;

-?(([0-9]+)|([0-9]*\.[0-9]+)([eE][-+]?[0-9]+)?) { printf("number\n"); }

. ECHO;

%%

int yywrap() {
    return 0;
}

int main() {
    yylex();
    return 0;
}

%{
%}                        
%%
                        
[\t ]+ /* ignore white space */;

is |
am |
were |
was |
be |
being |
been |
do |
does |
did |
will |
would |
should |
can |
could |
has |
have |
had |
go {printf("%s: is a verb\n", yytext);}

[a-zA-Z]+ {printf("%s: is not a verb\n", yytext);}

.|\n {ECHO; /**/}
%%

int yywrap() {
    return 0;
}

int main() {
    yylex();
}

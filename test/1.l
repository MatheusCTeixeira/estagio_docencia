%{
%}                        
%%
                        
[\t ]+ /* ignore white space */;

is |
am {print("%s", yytext);}



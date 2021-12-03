
%{
    enum {
        LOOKUP=0,
        VERB,
        ADJ,
        ADV,
        NOUN,
        PREP,
        PRON,
        CONJ
        };

    int state;

    int add_word(int type, char *word);
    int lookup_word(char *word);
%}

%%

\n { state = LOOKUP; }
^verb { state = VERB; }
^adj { state = ADJ; }
^adv {state = ADV; }
^noun { state = NOUN; }
^prep { state = PREP; }
^pron { state = PRON; }
^conj { state = CONJ; }

[a-zA-Z]+ {
	if (state != LOOKUP) {
		add_word(state, yytext);
	} else {
        switch (lookup_word(yytext)) {
        case VERB: printf("%s: is a verb\n", yytext); break;
        case ADJ: printf("%s: is an adjective\n", yytext); break;
        case ADV: printf("%s: is an adverb\n", yytext); break;
        case NOUN: printf("%s: is a noun", yytext); break;
        case PREP: printf("%s: is a preposition", yytext); break;
        case PRON: printf("%s: is a pronoun", yytext); break;
        case CONJ: printf("%s: is a conjunction", yytext); break;
        default:
            printf("%s: don't recognize\n", yytext);
            break;    
        }
    }
}

. /* ignore anything else */

%%

int yywrap() { return 0; }

int main() {
    yylex();
    return 0;
}

struct word {
    char *word_name;
    int word_type;
    struct word *next;
};

struct word *head;

extern void* malloc();


int add_word(int type, char *word) {
    struct word *wp;

    if (lookup_word(word) != LOOKUP) {
        printf("!!! warning: word %s already defined \n", word);
        return 0;
    }

    wp = (struct word *) malloc(sizeof(struct word));

    wp->next = head;

    wp->word_name = (char*)malloc(strlen(word)+1);
    strcpy(wp->word_name, word);
    wp->word_type = type;
    head = wp;
    return 1;
}

int lookup_word(char *word) {
    struct word *wp = head;

    for (; wp; wp = wp->next) {
        if (strcmp(wp->word_name, word) == 0)
            return wp->word_type;
    }

    return LOOKUP;
}

    

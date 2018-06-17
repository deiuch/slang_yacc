%{
#include <string.h>
#include "y.tab.h"

extern YYSTYPE yylval;

void skip_block_comment();
char *get_char_literal();
char *get_string_literal();

// TODO: Unicode
// following could be helpful (not in LEX)
/*
D                   \p{Nd}
L                   \p{L}\p{M}*+
WS                  \p{Z}
*/
%}

B                   [01]
O                   {B}|[2-7]
D                   {O}|[8-9]
H                   {D}|[A-Fa-f]

BIN                 0b{B}(_|{B})*
OCT                 0o{O}(_|{O})*
DEC                 {D}(_|{D})*
HEX                 0x{H}(_|{H})*

EXP                 (e|E)(-|\+)?{DEC}

L                   [A-Za-z]

WS                  [ \f\n\r\t\v]

%%

"//"[^\n]*\n        { /* ignore inline comment */ }
"/*"                { skip_block_comment(); }
"*/"                { return BLOCK_COMMENT_END; }
"\\\n"              { /* ignore escaped new line */ }

"is abstract"       { return IS_ABSTRACT; }
"and then"          { return AND_THEN; }
"or else"           { return OR_ELSE; }
"if"                { return IF; }
"then"              { return THEN; }
"elsif"             { return ELSIF; }
"else"              { return ELSE; }
"while"             { return WHILE; }
"in"                { return IN; }
"loop"              { return LOOP; }
"break"             { return BREAK; }
"try"               { return TRY; }
"catch"             { return CATCH; }
"raise"             { return RAISE; }
"unit"              { return UNIT; }
"is"                { return IS; }
"end"               { return END; }
"this"              { return THIS; }
"super"             { return SUPER; }
"routine"           { return ROUTINE; }
"return"            { return RETURN; }
"use"               { return USE; }
"as"                { return AS; }
"const"             { return CONST; }
"ref"               { return REF; }
"val"               { return VAL; }
"concurrent"        { return CONCURRENT; }
"abstract"          { return ABSTRACT; }
"extend"            { return EXTEND; }
"hidden"            { return HIDDEN; }
"final"             { return FINAL; }
"new"               { return NEW; }
"init"              { return INIT; }
"pure"              { return PURE; }
"safe"              { return SAFE; }
"override"          { return OVERRIDE; }
"alias"             { return ALIAS; }
"external"          { return EXTERNAL; }
"none"              { return NONE; }
"check"             { return CHECK; }
"require"           { return REQUIRE; }
"ensure"            { return ENSURE; }
"invariant"         { return INVARIANT; }
"variant"           { return VARIANT; }
"old"               { return OLD; }
"not"               { return NOT; }
"xor"               { return XOR; }
"and"               { return AND; }
"or"                { return OR; }
"true"              { return TRUE; }
"false"             { return FALSE; }

{L}({L}|{D}|_|$)* {
    yylval.text = (char *) malloc(strlen(yytext));
    strcpy(yylval.text, yytext);
    return IDENTIFIER;  // TODO: TYPE_IDENTIFIER
}

{BIN}|{OCT}|{DEC}|{HEX} {
    yylval.text = (char *) malloc(strlen(yytext));
    strcpy(yylval.text, yytext);
    return INTEGER_LITERAL;
}

{DEC}({EXP}|\.{DEC}{EXP}?) {
    yylval.text = (char *) malloc(strlen(yytext));
    strcpy(yylval.text, yytext);
    return REAL_LITERAL;
}

'(\\.|[^\\'])+' {
    yylval.text = get_char_literal();
    return CHAR_LITERAL;
}

\"(\\.|[^\\"])*\" {
    yylval.text = get_string_literal();
    return STRING_LITERAL;
}

";"|"\n"            { return SEPARATOR; }

"++"                { return PLUS_PLUS; }
"--"                { return MINUS_MINUS; }
"->"                { return MINUS_GREATER; }
".."                { return DOT_DOT; }
"/="                { return SLASH_EQUALS; }
":="                { return COLON_EQUALS; }
"<<"                { return LESS_LESS; }
"<="                { return LESS_EQUALS; }
"=>"                { return EQUALS_GREATER; }
">="                { return GREATER_EQUALS; }
">>"                { return GREATER_GREATER; }
"&"                 { return AMPERSAND; }
"("                 { return LPAREN; }
")"                 { return RPAREN; }
"*"                 { return ASTERISK; }
"+"                 { return PLUS; }
","                 { return COMMA; }
"-"                 { return MINUS; }
"."                 { return DOT; }
"/"                 { return SLASH; }
":"                 { return COLON; }
"<"                 { return LESS; }
"="                 { return EQUALS; }
">"                 { return GREATER; }
"?"                 { return QUESTION; }
"["                 { return LBRACKET; }
"\\"                { return BACKSLASH; }
"]"                 { return RBRACKET; }
"^"                 { return CARET; }
"|"                 { return VERTICAL; }
"~"                 { return TILDE; }

{WS}                { /* ignore whitespaces */ }
.                   { return *yytext; }

%%

void skip_block_comment()
{
    while (yylex() != BLOCK_COMMENT_END);
}

char *get_char_literal()
{
    char *res = get_string_literal();
    if (strlen(res) > 1)  // TODO: Unicode
    {
        fprintf(stderr, "error: not a char '%s'\n", res);
        exit(EXIT_FAILURE);
    }
    return res;
}

char *get_string_literal()
{
    size_t i = 1, j = 0;
    size_t len = strlen(yytext);
    char to_put;
    char *res = (char *) malloc(len - 2);
    while (i < len - 1)
    {
        if (yytext[i] == '\\')
        {
            ++i;
            switch (yytext[i])
            {
                case 'b':  to_put = '\b'; break;
                case 'f':  to_put = '\f'; break;
                case 'n':  to_put = '\n'; break;
                case 'r':  to_put = '\r'; break;
                case 't':  to_put = '\t'; break;
                case 'v':  to_put = '\v'; break;
                case '\\': to_put = '\\'; break;
                case '\"': to_put = '\"'; break;
                case '\'': to_put = '\''; break;
                case 'x':
                    // TODO: 2 hex
                    break;
                case 'u':
                    // TODO: 4 hex
                    break;
                default:
                    fprintf(stderr, "error: unknown escape '\\%c'\n", yytext[i]);
                    exit(EXIT_FAILURE);
            }
        }
        else
        {
            to_put = yytext[i];
        }
        res[j] = to_put;
        ++i;
        ++j;
    }
    res[j] = '\0';
    return res;
}

int yywrap()
{
    return 1;
}
/*
int main()
{
    while (1)
    {
        int token = yylex();
        switch (token) {
            case 0:
                return 0;

            case IS_ABSTRACT:
                printf("IS_ABSTRACT");
                break;
            case AND_THEN:
                printf("AND_THEN");
                break;
            case OR_ELSE:
                printf("OR_ELSE");
                break;
            case IF:
                printf("IF");
                break;
            case THEN:
                printf("THEN");
                break;
            case ELSIF:
                printf("ELSIF");
                break;
            case ELSE:
                printf("ELSE");
                break;
            case WHILE:
                printf("WHILE");
                break;
            case IN:
                printf("IN");
                break;
            case LOOP:
                printf("LOOP");
                break;
            case BREAK:
                printf("BREAK");
                break;
            case TRY:
                printf("TRY");
                break;
            case CATCH:
                printf("CATCH");
                break;
            case RAISE:
                printf("RAISE");
                break;
            case UNIT:
                printf("UNIT");
                break;
            case IS:
                printf("IS");
                break;
            case END:
                printf("END");
                break;
            case THIS:
                printf("THIS");
                break;
            case SUPER:
                printf("SUPER");
                break;
            case ROUTINE:
                printf("ROUTINE");
                break;
            case RETURN:
                printf("RETURN");
                break;
            case USE:
                printf("USE");
                break;
            case AS:
                printf("AS");
                break;
            case CONST:
                printf("CONST");
                break;
            case REF:
                printf("REF");
                break;
            case VAL:
                printf("VAL");
                break;
            case CONCURRENT:
                printf("CONCURRENT");
                break;
            case ABSTRACT:
                printf("ABSTRACT");
                break;
            case EXTEND:
                printf("EXTEND");
                break;
            case HIDDEN:
                printf("HIDDEN");
                break;
            case FINAL:
                printf("FINAL");
                break;
            case NEW:
                printf("NEW");
                break;
            case INIT:
                printf("INIT");
                break;
            case PURE:
                printf("PURE");
                break;
            case SAFE:
                printf("SAFE");
                break;
            case OVERRIDE:
                printf("OVERRIDE");
                break;
            case ALIAS:
                printf("ALIAS");
                break;
            case EXTERNAL:
                printf("EXTERNAL");
                break;
            case NONE:
                printf("NONE");
                break;
            case CHECK:
                printf("CHECK");
                break;
            case REQUIRE:
                printf("REQUIRE");
                break;
            case ENSURE:
                printf("ENSURE");
                break;
            case INVARIANT:
                printf("INVARIANT");
                break;
            case VARIANT:
                printf("VARIANT");
                break;
            case OLD:
                printf("OLD");
                break;
            case NOT:
                printf("NOT");
                break;
            case XOR:
                printf("XOR");
                break;
            case AND:
                printf("AND");
                break;
            case OR:
                printf("OR");
                break;
            case TRUE:
                printf("TRUE");
                break;
            case FALSE:
                printf("FALSE");
                break;

            case SEPARATOR:
                printf("SEPARATOR");
                break;

            case IDENTIFIER:
                printf("IDENTIFIER:%s", yytext);
                break;
            case TYPE_IDENTIFIER:
                printf("TYPE_IDENTIFIER:%s", yytext);
                break;
            case INTEGER_LITERAL:
                printf("INT:%s", yytext);
                break;
            case REAL_LITERAL:
                printf("REAL:%s", yytext);
                break;
            case CHAR_LITERAL:
                printf("CHAR:%s", yytext);
                break;
            case STRING_LITERAL:
                printf("STR:%s", yytext);
                break;

            case PLUS_PLUS:
                printf("PLUSPLUS");
                break;
            case MINUS_MINUS:
                printf("MINUS_MINUS");
                break;
            case MINUS_GREATER:
                printf("MINUS_GREATER");
                break;
            case DOT_DOT:
                printf("DOT_DOT");
                break;
            case SLASH_EQUALS:
                printf("SLASH_EQUALS");
                break;
            case COLON_EQUALS:
                printf("COLON_EQUALS");
                break;
            case LESS_LESS:
                printf("LESS_LESS");
                break;
            case LESS_EQUALS:
                printf("LESS_EQUALS");
                break;
            case EQUALS_GREATER:
                printf("EQUALS_GREATER");
                break;
            case GREATER_EQUALS:
                printf("GREATER_EQUALS");
                break;
            case GREATER_GREATER:
                printf("GREATER_GREATER");
                break;
            case AMPERSAND:
                printf("AMPERSAND");
                break;
            case ASTERISK:
                printf("ASTERISK");
                break;
            case PLUS:
                printf("PLUS");
                break;
            case LPAREN:
                printf("LPAREN");
                break;
            case RPAREN:
                printf("RPAREN");
                break;
            case COMMA:
                printf("COMMA");
                break;
            case MINUS:
                printf("MINUS");
                break;
            case DOT:
                printf("DOT");
                break;
            case SLASH:
                printf("SLASH");
                break;
            case COLON:
                printf("COLON");
                break;
            case LESS:
                printf("LESS");
                break;
            case EQUALS:
                printf("EQUALS");
                break;
            case GREATER:
                printf("GREATER");
                break;
            case QUESTION:
                printf("QUESTION");
                break;
            case LBRACKET:
                printf("LBRACKET");
                break;
            case BACKSLASH:
                printf("BACKSLASH");
                break;
            case RBRACKET:
                printf("RBRACKET");
                break;
            case CARET:
                printf("CARET");
                break;
            case VERTICAL:
                printf("VERTICAL");
                break;
            case TILDE:
                printf("TILDE");
                break;

            default:
                fprintf(stderr, "~UNKNOWN:%s", yytext);
        }
        printf("\n");
    }
    return 1;
}*/
Bison\Bin\bison.exe -dy -v yacc_syntax.y
Lex\Bin\lex.exe lex_tokens.l
gcc lex.yy.c y.tab.c -o SLang_parser.exe
SLang_parser.exe
pause
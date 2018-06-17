cmake_minimum_required(VERSION 3.9)
project(slang_yacc C)

set(CMAKE_C_STANDARD 99)

add_executable(slang_yacc lex.yy.c y.tab.c)
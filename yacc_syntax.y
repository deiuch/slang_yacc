%{
#include <stdio.h>
#include <string.h>

extern int yylex();
char const *yyerror(const char *str);
%}

%union {
    char *text;
}

/*
    Short major problems history:
    1) shift/reduce with list of AnonymousRoutine (list inside list)
    2) shift/reduce of routine name with anything that ends with optional IDENTIFIER
    3) reduce/reduce with abstract routine in one scope with abstract unit
    4) reduce/reduce of INIT keyword
    5) shift/reduce of ObjectDeclaration beginning (ConstOpt)
    6) shift/reduce of Routine beginning (PureOrSafeOpt and OverrideOpt)
    7) shift/reduce of coincident Routine and Object
    8) reduce/reduce of the rule Type:IDENTIFIER // TODO: analyze other solutions
    9) reduce/reduce of Tuple declaration (if defined like sequence of declarations and expressions)
    10) shift/reduce of routine-operator with ObjectDeclaration ending with Expression
    11) TODO: shift/reduce of CaseStatement (absence of the beginning of this statement)
    12) shift/reduce of "++", "--", "+", "-" and "(" as operations with a set of use cases
        (lambda as Expression removed)
    13) shift/reduce of LPAREN after the Expression
*/

%expect 0  // For expected amount of conflicts

/*
    This nonterminal was introduced in order to solve
    reduce/reduce conflict when in one scope abstract
    function may be declared before Unit with optional
    keyword ABSTRACT at the beginning.
*/
%token IS_ABSTRACT

%token AND_THEN  // Compound for ease
%token OR_ELSE   // of precedence setting
%token IF
%token THEN
%token ELSIF
%token ELSE
%token WHILE
%token IN
%token LOOP
%token BREAK
%token TRY
%token CATCH
%token RAISE
%token UNIT
%token IS
%token END
%token THIS
%token SUPER
%token ROUTINE
%token RETURN
%token USE
%token AS
%token CONST
%token REF
%token VAL
%token CONCURRENT
%token ABSTRACT
%token EXTEND
%token HIDDEN
%token FINAL
%token NEW
%token INIT
%token PURE
%token SAFE
%token OVERRIDE
%token ALIAS
%token EXTERNAL
%token NONE
%token CHECK
%token REQUIRE
%token ENSURE
%token INVARIANT
%token VARIANT
%token OLD
%token NOT
%token XOR
%token AND
%token OR
%token TRUE
%token FALSE

%token IDENTIFIER
%token TYPE_IDENTIFIER  // Different from ID for type defining (improve typename identifying)
%token INTEGER_LITERAL
%token REAL_LITERAL
%token CHAR_LITERAL
%token STRING_LITERAL

%token SEPARATOR  // Includes semicolon (;) and new line (\n)

%token PLUS_PLUS
%token MINUS_MINUS
%token MINUS_GREATER
%token DOT_DOT
%token SLASH_EQUALS
%token COLON_EQUALS
%token LESS_LESS
%token LESS_EQUALS
%token EQUALS_GREATER
%token GREATER_EQUALS
%token GREATER_GREATER
%token AMPERSAND
%token ASTERISK
%token PLUS
%token LPAREN
%token RPAREN
%token COMMA
%token MINUS
%token DOT
%token SLASH
%token COLON
%token LESS
%token EQUALS
%token GREATER
%token QUESTION
%token LBRACKET
%token BACKSLASH
%token RBRACKET
%token CARET
%token VERTICAL
%token TILDE

// Lower priority
%left DOT_DOT                                             // Value generator
%left OR_ELSE                                             // Logical OR
%left AND_THEN                                            // Logical AND
%left OR        VERTICAL                                  // Bitwise OR
%left XOR       CARET                                     // Bitwise Exclusive OR
%left AND       AMPERSAND                                 // Bitwise AND
%left EQUALS    SLASH_EQUALS                              // Object equality
%left LESS      LESS_EQUALS     GREATER   GREATER_EQUALS  // Value comparison
%left LESS_LESS GREATER_GREATER                           // Bitwise shift
%left PLUS      MINUS                                     // Additive operations
%left ASTERISK  SLASH           BACKSLASH                 // Multiplicative operations
%left DOT                                                 // TODO: comment
%nonassoc LOWER_THAN_LPAREN  // Pseudo-token for prioritizing the routine call in PostfixExpression
%right LPAREN
// Higher priority

%start CompilationUnit

%%

CompilationUnit     : UseDirectiveSeqOpt ProgramEntitySeqOpt
                    ;

ProgramEntitySeqOpt : /* empty */
                    | ProgramEntitySeqOpt ProgramEntity
                    ;

ProgramEntity       : Statement
                    | Declaration
                    | UnitWithCompoundName  // Allowed only on the highest level
                    ;

Declaration         : UnitDeclaration
                    | RoutineDeclaration
                    | ObjectDeclaration
                    ;

// Use directive ***

UseDirectiveSeqOpt  : /* empty */
                    | UseDirectiveSeqOpt UseDirective
                    ;

UseDirective        : USE       UseItemSeq
                    | USE CONST UseItemSeq
                    ;

UseItemSeq          :                      UseItem
                    | UseItemSeq COMMA     UseItem
                //  | UseItemSeq SEPARATOR UseItem  // shift/reduce, SEPARATOR is Operator also
                    ;

UseItem             : CompoundName
                    | CompoundName AS IDENTIFIER
                    ;

// Formal generics ***

FormalGenericsOpt   : /* empty */
                    | FormalGenerics
                    ;

FormalGenerics      : LBRACKET GeneralizedParamSeq RBRACKET
                    ;

GeneralizedParamSeq :                               GeneralizedParameter
                    | GeneralizedParamSeq COMMA     GeneralizedParameter
                    | GeneralizedParamSeq SEPARATOR GeneralizedParameter
                    ;

GeneralizedParameter: IDENTIFIER
                    | IDENTIFIER MINUS_GREATER Type
                    | IDENTIFIER MINUS_GREATER Type INIT
                    | IDENTIFIER MINUS_GREATER Type INIT LPAREN         RPAREN
                    | IDENTIFIER MINUS_GREATER Type INIT LPAREN TypeSeq RPAREN
                    | IDENTIFIER COLON Type  // Generic parameter as value of some type
                    ;

// Contracts ***

PreconditionOpt     : /* empty */
                    | REQUIRE      PredicateSeq
                    | REQUIRE ELSE PredicateSeq
                    ;

PostconditionOpt    : /* empty */
                    | ENSURE      PredicateSeq
                    | ENSURE THEN PredicateSeq
                    ;

InvariantOpt        : /* empty */
                    | INVARIANT PredicateSeq
                    ;

VariantOpt          : /* empty */
                    | VARIANT PredicateSeq
                    ;

PredicateSeq        :                        Predicate
                    | PredicateSeq COMMA     Predicate
                    | PredicateSeq SEPARATOR Predicate
                    ;

Predicate           :             Expression
                    | Label COLON Expression
                    ;

// Unit ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

/*
 *  All the endings (duplicating identifiers)
 *  of Units/Routines were discarded
 *  because of lots of conflicts with them and
 *  ability to express them as comments instead.
 */

UnitWithCompoundName: UnitSpecifierOpt UNIT IDENTIFIER DOT CompoundName UnitBody
                    ;  // Just CompoundName makes reduce/reduce with usual UnitDeclaration

UnitDeclaration     : UnitSpecifierOpt UNIT IDENTIFIER UnitBody
                    ;

UnitBody            : FormalGenericsOpt UnitDirectiveSeqOpt IS                InvariantOpt END
                    | FormalGenericsOpt UnitDirectiveSeqOpt IS NONE           InvariantOpt END
                    | FormalGenericsOpt UnitDirectiveSeqOpt IS UnitContentSeq InvariantOpt END
                    ;

UnitSpecifierOpt    : /* empty */
                    | REF
                    | VAL
                    | CONCURRENT
                    | ABSTRACT
                    ;

UnitDirectiveSeqOpt : /* empty */
                    | UnitDirectiveSeqOpt UnitDirective
                    ;

UnitDirective       : InheritanceDirective
                    | UseDirective
                    ;

InheritanceDirective: EXTEND BaseUnitSeq
                    ;

BaseUnitSeq         :                       BaseUnitName
                    | BaseUnitSeq COMMA     BaseUnitName
                    | BaseUnitSeq SEPARATOR BaseUnitName
                    ;

BaseUnitName        :       Type
                    | TILDE Type
                    ;

UnitContentSeq      :                HiddenFinalOpt UnitContent
                    | UnitContentSeq HiddenFinalOpt UnitContent
                    ;

HiddenFinalOpt      : /* empty */
                    | HIDDEN
                    | HIDDEN FINAL
                    ;

UnitContent         : SEPARATOR  // Statements are not allowed here, but SEPARATOR is
                //  | Statement
                    | Declaration
                    | InitRoutineDecl  // Allowed only in the Unit
                    | OperatorRoutineDecl  // Allowed only in the Unit
                    | ConstObjectsBlock
                    ;

ConstObjectsBlock   : CONST IS ConstObjectSeq END
                    ;

ConstObjectSeq      :                          ConstObject
                    | ConstObjectSeq COMMA     ConstObject
                    | ConstObjectSeq SEPARATOR ConstObject
                    ;

ConstObject         : IDENTIFIER
                    | IDENTIFIER DOT INIT
                    | IDENTIFIER DOT INIT LPAREN               RPAREN
                    | IDENTIFIER DOT INIT LPAREN ExpressionSeq RPAREN
                    ;

// Routine /////////////////////////////////////////////////////////////////////////////////////////////////////////////

RoutineDeclaration  :                  IDENTIFIER                RoutineParameters RoutineSpecs RoutineBody
                    | PureSafeOverride IDENTIFIER                RoutineParameters RoutineSpecs RoutineBody
                    |                  IDENTIFIER FormalGenerics RoutineParameters RoutineSpecs RoutineBody
                    | PureSafeOverride IDENTIFIER FormalGenerics RoutineParameters RoutineSpecs RoutineBody
                    ;  // No optional fields allowed because of matching with ObjectDeclaration

InitRoutineDecl     : INIT FormalGenericsOpt RoutineParameters RoutineSpecs InitRoutineBody
                    ;  // Separated because of shift/reduce with Expression: INIT

OperatorRoutineDecl : SEPARATOR        OpRoutineIdentifier FormalGenericsOpt RoutineParameters RoutineSpecs RoutineBody
                    | PureSafeOverride OpRoutineIdentifier FormalGenericsOpt RoutineParameters RoutineSpecs RoutineBody
                    ;  // TODO: lower priority than the PostfixExpression

OpRoutineIdentifier : OverridableOperator
                    | OverridableOperator ALIAS IDENTIFIER
                    ;

PureSafeOverride    : PURE
                    | SAFE
                    |      OVERRIDE
                    | PURE OVERRIDE
                    | SAFE OVERRIDE
                    ;

RoutineParameters   : LPAREN RPAREN
                    | TypeTuple
                    ;

RoutineSpecs        : ReturnTypeOpt UseDirectiveSeqOpt PreconditionOpt
                    ;

ReturnTypeOpt       : /* empty */
                    | ReturnType
                    ;

ReturnType          : COLON Type
                    | AS CompoundName
                    ;

RoutineBody         : IS                   PostconditionOpt END
                    | IS NONE              PostconditionOpt END
                    | IS RoutineContentSeq PostconditionOpt END
                    | EQUALS_GREATER Statement
                //  | EQUALS_GREATER Expression  // shift/reduce with Expression, harder to make RoutineCall
                    | IS_ABSTRACT  // reduce/reduce with unit specifier ABSTRACT
                    | IS EXTERNAL
                    ;

InitRoutineBody     : IS                   PostconditionOpt END
                    | IS NONE              PostconditionOpt END
                    | IS RoutineContentSeq PostconditionOpt END
                    ;

RoutineContentSeq   :                   RoutineContent
                    | RoutineContentSeq RoutineContent
                    ;

RoutineContent      : Statement
                    | Declaration
                    ;

// Object //////////////////////////////////////////////////////////////////////////////////////////////////////////////

ObjectDeclaration   :       IdentifierSeq ObjectTypeSpec
                    |       IdentifierSeq ObjectTypeSpecOpt IS Expression
                    | CONST IdentifierSeq ObjectTypeSpec
                    | CONST IdentifierSeq ObjectTypeSpecOpt IS Expression
                    ;  // No optional fields allowed because of matching with RoutineDeclaration

ObjectTypeSpecOpt   : /* empty */
                    | ObjectTypeSpec
                    ;

ObjectTypeSpec      : COLON ConcurrentOpt QuestionOpt RefOrValOpt Type
                    | COLON AS CompoundName
                    ;

ConcurrentOpt       : /* empty */
                    | CONCURRENT
                    ;

QuestionOpt         : /* empty */
                    | QUESTION
                    ;

RefOrValOpt         : /* empty */
                    | REF
                    | VAL
                    ;

// Type ////////////////////////////////////////////////////////////////////////////////////////////////////////////////

TypeSeq             :                   Type
                    | TypeSeq COMMA     Type
                    | TypeSeq SEPARATOR Type
                    ;

/*
 *  This rule is the only place where the
 *  conflicting IDENTIFIER is replaced by
 *  TYPE_IDENTIFIER. The solution for the
 *  lexical analyzer might be triggering
 *  when some special token found (COLON,
 *  MINUS_GREATER etc.) and waiting till
 *  Type non-terminal will be found.
 */
Type                : TypeTuple
                //  | IDENTIFIER  // reduce/reduce with rule "IdentifierSeq: IDENTIFIER".
                //  | IDENTIFIER LBRACKET TypeOrExpressionSeq RBRACKET  // No problem, just to be consistent
                    | TYPE_IDENTIFIER
                    | TYPE_IDENTIFIER LBRACKET TypeOrExpressionSeq RBRACKET
                    | ROUTINE RoutineParameters ReturnTypeOpt
                    |         RoutineParameters ReturnType  // reduce/reduce if just TypeSeq
                    ;

TypeTuple           : LPAREN TypeOrDeclarationSeq RPAREN
                    ;

TypeOrDeclarationSeq:                                TypeOrDeclaration
                    | TypeOrDeclarationSeq COMMA     TypeOrDeclaration
                    | TypeOrDeclarationSeq SEPARATOR TypeOrDeclaration
                    ;

// TODO: discuss if TYPE_IDENTIFIER required instead of IDENTIFIER
TypeOrDeclaration   : Type
                    | IDENTIFIER COLON RefOrValOpt Type
                    | IDENTIFIER                        IS Expression
                    | IDENTIFIER COLON RefOrValOpt Type IS Expression
                    ;  // Usual ObjectDeclaration causes lots of conflicts, we took only part of it

TypeOrExpressionSeq :                               Type
                    |                               Expression
                    | TypeOrExpressionSeq COMMA     Type
                    | TypeOrExpressionSeq COMMA     Expression
                    | TypeOrExpressionSeq SEPARATOR Type
                    | TypeOrExpressionSeq SEPARATOR Expression
                    ;

// Statement ///////////////////////////////////////////////////////////////////////////////////////////////////////////

Statement           : RoutineCall
                    | AssignmentStatement
                    | Deassignment
                    | IfCaseStatement
                    | LoopStatement
                    | BreakStatement
                    | RaiseStatement
                    | ControlStatement
                    | CheckStatement
                    | ReturnStatement
                    | SEPARATOR

                    | error { fprintf(stderr, "Error in the statement has been found!\n"); }  // TODO
                    ;

RoutineCall         : PostfixExpression %prec LOWER_THAN_LPAREN
                    ;  // Has lower precedence than PostfixExpression "routine call"

AssignmentStatement : PostfixExpression COLON_EQUALS Expression
                    ;

Deassignment        : QUESTION IDENTIFIER
                    ;

IfCaseStatement     : IF Expression THEN AlternativeSeq                          END
                    | IF Expression THEN AlternativeSeq   ELSE RoutineContentSeq END
                //  | IF Expression IS   CaseStatementSeq                        END
                //  | IF Expression IS   CaseStatementSeq ELSE RoutineContentSeq END
                    ;  // TODO: CaseStatement

AlternativeSeq      :                                      RoutineContentSeq
                    | AlternativeSeq ELSIF Expression THEN RoutineContentSeq
                    ;
/*
CaseStatementSeq    :                  CaseStatement
                    | CaseStatementSeq CaseStatement
                    ;

CaseStatement       : ExpressionSeq COLON RoutineContentSeq
                    ;  // TODO: shift/reduce (add keyword CASE?)
*/
LoopStatement       : WHILE               Expression InvariantOpt LOOP RoutineContentSeq VariantOpt END
                    | WHILE IDENTIFIER IN Expression InvariantOpt LOOP RoutineContentSeq VariantOpt END
                    | InvariantOpt LOOP RoutineContentSeq WHILE Expression VariantOpt END
                    ;  // Invariant and Variant are added, but are not agreed

BreakStatement      : BREAK SEPARATOR
                    | BREAK Label
                    ;  // SEPARATOR because of shift/reduce with next statements and declarations

RaiseStatement      : RAISE Expression
                    ;

ControlStatement    : TRY RoutineContentSeq CatchStatementSeq                        END
                    | TRY RoutineContentSeq CatchStatementSeq ELSE RoutineContentSeq END
                    ;

CatchStatementSeq   :                   CatchStatement
                    | CatchStatementSeq CatchStatement
                    ;

CatchStatement      : CATCH LPAREN                  Type RPAREN
                    | CATCH LPAREN IDENTIFIER COLON Type RPAREN
                    | CATCH LPAREN                  Type RPAREN RoutineContentSeq
                    | CATCH LPAREN IDENTIFIER COLON Type RPAREN RoutineContentSeq
                    ;

CheckStatement      : CHECK PredicateSeq END
                    ;

ReturnStatement     : RETURN SEPARATOR
                    | RETURN Expression
                    ;  // SEPARATOR because of shift/reduce with next statements and declarations

// Expression //////////////////////////////////////////////////////////////////////////////////////////////////////////

ExpressionSeq       :                         Expression
                    | ExpressionSeq COMMA     Expression
                    | ExpressionSeq SEPARATOR Expression
                    ;

Expression          : UnaryExpression
                    | Expression DOT             Expression
                    | Expression ASTERISK        Expression
                    | Expression SLASH           Expression
                    | Expression BACKSLASH       Expression
                    | Expression PLUS            Expression
                    | Expression MINUS           Expression
                    | Expression LESS_LESS       Expression
                    | Expression GREATER_GREATER Expression
                    | Expression LESS            Expression
                    | Expression LESS_EQUALS     Expression
                    | Expression GREATER         Expression
                    | Expression GREATER_EQUALS  Expression
                //  | Expression IS              Expression  // shift/reduce when used in precondition
                    | Expression EQUALS          Expression
                    | Expression SLASH_EQUALS    Expression
                    | Expression AND             Expression
                    | Expression AMPERSAND       Expression
                    | Expression XOR             Expression
                    | Expression CARET           Expression
                    | Expression OR              Expression
                    | Expression VERTICAL        Expression
                    | Expression AND_THEN        Expression
                    | Expression OR_ELSE         Expression
                    | Expression DOT_DOT         Expression
                    ;  // Adding an operator here - fill the precedence table at the top

UnaryExpression     : PostfixExpression %prec LPAREN
                    | NOT         UnaryExpression
                    | TILDE       UnaryExpression
                    | PLUS        UnaryExpression
                    | MINUS       UnaryExpression
                    | PLUS_PLUS   UnaryExpression
                    | MINUS_MINUS UnaryExpression
                    ;  // Just PostfixExpression has lower precedence than itself because of possible "routine call"

PostfixExpression   : PrimaryExpression
                    | PostfixExpression LPAREN               RPAREN %prec LPAREN
                    | PostfixExpression LPAREN ExpressionSeq RPAREN %prec LPAREN
                    | PostfixExpression PLUS_PLUS
                    | PostfixExpression MINUS_MINUS
                    ;

PrimaryExpression   : LPAREN ExpressionSeq RPAREN %prec LOWER_THAN_LPAREN  // Tuple or parenthesized expression
                //  | LPAREN Expression    RPAREN  // reduce/reduce with tuple of one element
                    | Literal
                      /*
                       * This rule has lower precedence comparing
                       * to the RoutineDeclaration rule when
                       * the LPAREN is found next.
                       */
                    | IDENTIFIER %prec LOWER_THAN_LPAREN
                    | NEW IDENTIFIER
                    | OLD IDENTIFIER
                    | INIT
                    | THIS
                    | SUPER
                    ;

// Primitives //////////////////////////////////////////////////////////////////////////////////////////////////////////

Literal             : BooleanLiteral
                    | INTEGER_LITERAL
                    | REAL_LITERAL
                    | CHAR_LITERAL
                    | STRING_LITERAL
                    ;

BooleanLiteral      : TRUE
                    | FALSE
                    ;

CompoundName        :                  IDENTIFIER
                    | CompoundName DOT IDENTIFIER
                    ;

IdentifierSeq       :                         IDENTIFIER
                    | IdentifierSeq COMMA     IDENTIFIER
                //  | IdentifierSeq SEPARATOR IDENTIFIER  // reduce/reduce with ExpressionSeq
                    ;

Label               : IDENTIFIER
                    ;

OverridableOperator : COLON_EQUALS
                    | AND_THEN
                    | OR_ELSE
                    | NOT
                    | XOR
                    | AND
                    | OR
                    | PLUS_PLUS
                    | MINUS_MINUS
                    | DOT_DOT
                    | SLASH_EQUALS
                    | LESS_LESS
                    | LESS_EQUALS
                    | GREATER_EQUALS
                    | GREATER_GREATER
                    | AMPERSAND
                    | ASTERISK
                    | PLUS
                    | MINUS
                    | SLASH
                    | LESS
                    | EQUALS
                    | GREATER
                    | BACKSLASH
                    | CARET
                    | VERTICAL
                    | TILDE
                    ;  // If any operator becomes overridable - add it here  // TODO: check the list

%%

char const *yyerror(const char *str)
{
    fprintf(stderr, "yyerror: %s\n", str);
}

int main()
{
    yylval.text = (char *) malloc(0);
    return yyparse();
}
/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static void create_table();
    static void insert_symbol(char*, bool, char*);  /* insert_symbol(id, isArray, typeName) */
    static char *lookup_symbol(char*);              /* Return the type name of the symbol */
    static void dump_symbol();

    Table *firstTable = NULL;
    Table *currentTable = NULL;

    int address = 0;
    int scope = 0;
%}

%error-verbose

/* Use variable or self-defined structure to represent
 * nonterminal and token type
 */
%union {
    int i_val;
    float f_val;
    char *s_val;
    char *type;
    char *op_type;
}

/* Token without return */
%token VAR
%token INT FLOAT BOOL STRING
%token INC DEC
%token GEQ LEQ EQL NEQ
%token ADD_ASSIGN SUB_ASSIGN MUL_ASSIGN QUO_ASSIGN REM_ASSIGN
%token LAND LOR
%token NEWLINE
%token PRINT PRINTLN IF ELSE FOR

/* Token with return, which need to specify type */
%token <s_val> IDENT
%token <i_val> INT_LIT
%token <f_val> FLOAT_LIT
%token <s_val> BOOL_LIT STRING_LIT

/* Nonterminal with return, which need to specify type */
%type <type> expr type_name primaryExpr operand
%type <op_type> assign_op

/* Precedence from low to high */
%right '='
%left LOR
%left LAND
%left '<' '>' GEQ LEQ EQL NEQ
%left '+' '-'
%left '*' '/' '%'
%left '!'   // FIXME: POS & NEG missing
%left ']'
%left ')'

/* Yacc will start at this nonterminal */
%start program

/* Grammar section */
%%

program
    : stmts
;

stmts
    : stmts stmt
    | stmt
    ;

stmt
    : dcl NEWLINE
    | simpleStmt NEWLINE
    | block NEWLINE
    | printStmt NEWLINE
    | NEWLINE
    ;

dcl
    : VAR IDENT type_name '=' expr  { bool isArray = false; insert_symbol($2, isArray, $3); }
    | VAR IDENT type_name           { bool isArray = false; insert_symbol($2, isArray, $3); }
    | VAR IDENT indexExpr type_name { bool isArray = true; insert_symbol($2, isArray, $4); }
    ;

type_name
    : INT               { $$ = "int32"; }
    | FLOAT             { $$ = "float32"; }
    | BOOL              { $$ = "bool"; }
    | STRING            { $$ = "string"; }
    ;

indexExpr
    : '[' expr ']'
    | primaryExpr '[' expr ']'
    ;

simpleStmt
    : assignmentStmt
    | exprStmt
    | incDecStmt
    ;

block
    : left_brace stmts right_brace
    ;

left_brace
    : '{'           { scope++; create_table(); }
    ;

right_brace
    : '}'           { dump_symbol(); scope--; }
    ;

assignmentStmt
    : expr assign_op expr   { printf("%s\n", $2); }
    ;

exprStmt
    : expr
    ;

expr
    : expr LOR expr     { $$ = "bool"; printf("LOR\n"); }
    | expr LAND expr    { $$ = "bool"; printf("LAND\n"); }
    | expr '<' expr     { $$ = "bool"; printf("LSS\n"); }
    | expr '>' expr     { $$ = "bool"; printf("GTR\n"); }
    | expr GEQ expr     { $$ = "bool"; printf("GEQ\n"); }
    | expr LEQ expr     { $$ = "bool"; printf("LEQ\n"); }
    | expr EQL expr     { $$ = "bool"; printf("EQL\n"); }
    | expr NEQ expr     { $$ = "bool"; printf("NEQ\n"); }
    | expr '+' expr     { $$ = $1; printf("ADD\n"); }
    | expr '-' expr     { $$ = $1; printf("SUB\n"); }
    | expr '*' expr     { $$ = $1; printf("MUL\n"); }
    | expr '/' expr     { $$ = $1; printf("QUO\n"); }
    | expr '%' expr     { $$ = $1; printf("REM\n"); }
    | unaryExpr
    | literal           
    | IDENT             { $$ = lookup_symbol($1); }
    ;

unaryExpr
    : primaryExpr
    | '+' unaryExpr     { printf("POS\n"); }
    | '-' unaryExpr     { printf("NEG\n"); }
    | '!' unaryExpr     { printf("NOT\n"); }
    ;

primaryExpr
    : operand           { $$ = $1; }
    | indexExpr
    | conversionExpr
    ;

operand
    : literal
    | '(' expr ')'
    | IDENT             { $$ = lookup_symbol($1); }
    ;

conversionExpr
    : type_name '(' expr ')'
    ;

literal
    : INT_LIT               { printf("INT_LIT %d\n", $1); }
    | FLOAT_LIT             { printf("FLOAT_LIT %f\n", $1); }
    | BOOL_LIT              { printf("%s\n", $1); }
    | '"' STRING_LIT '"'    { printf("STRING_LIT %s\n", $2); }
    ;

assign_op
    : '='                   { $$ = "ASSIGN"; }
    | ADD_ASSIGN            { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN            { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN            { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN            { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN            { $$ = "REM_ASSIGN"; }
    ;

incDecStmt
    : expr INC      { printf("INC\n"); }
    | expr DEC      { printf("DEC\n"); }
    ;

printStmt
    : PRINT '(' expr ')'    { printf("PRINT %s\n", $3); }
    | PRINTLN '(' expr ')'  { printf("PRINTLN %s\n", $3); }
    ;

%%

/* C code section */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        yyin = fopen(argv[1], "r");
    } else {
        yyin = stdin;
    }

    yylineno = 0;
    create_table(); /* create the first table */
    yyparse();
    dump_symbol();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

/* Creates a symbol table when entering a new scope */
static void create_table() {
    Table *newTable = malloc(sizeof(Table));
    
    /* Initialize the new table */
    newTable->scope = scope;
    newTable->firstSymbol = NULL;
    newTable->nextTable = NULL;
    
    if(firstTable == NULL) {
        firstTable = newTable;
        firstTable->prevTable = NULL;
    } else {
        newTable->prevTable = currentTable;
        currentTable->nextTable = newTable;
    }
    currentTable = newTable;
}

/* Inserts an entry for a variable declaration */
static void insert_symbol(char *name, bool isArray, char *type) {
    printf("> Insert {%s} into symbol table (scope level: %d)\n", name, scope);
    // printf("> Type: %s\n", type);
    // printf("> Array: %s\n", isArray ? "yes" : "no");

    Symbol *newSymbol = malloc(sizeof(Symbol));

    /* Initialize newSymbol */
    newSymbol->name = strdup(name);
    newSymbol->address = address++;
    newSymbol->lineno = yylineno;
    newSymbol->nextSymbol = NULL;
    if (isArray) {
        newSymbol->type = "array";
        newSymbol->elementType = strdup(type);
    } else {
        newSymbol->type = strdup(type);
        newSymbol->elementType = "-";
    }

    if (currentTable->firstSymbol == NULL) {
        currentTable->firstSymbol = newSymbol;
    } else {
        Symbol *currentSymbol;
        /* Get the last symbol of the current table */
        for (currentSymbol = currentTable->firstSymbol; currentSymbol->nextSymbol != NULL; currentSymbol = currentSymbol->nextSymbol)
            ;
        currentSymbol->nextSymbol = newSymbol;
    }
}

/* Looks up an entry in the symbol table */
static char* lookup_symbol(char *symbol) {
    Table *table;

    for (table = currentTable; table != NULL; table = table->prevTable) {
        Symbol *currentSymbol;
        for (currentSymbol = table->firstSymbol; currentSymbol != NULL; currentSymbol = currentSymbol->nextSymbol) {
            char *name = currentSymbol->name;
            if (strcmp(name, symbol) == 0) {
                printf("IDENT (name=%s, address=%d)\n", symbol, currentSymbol->address);
                
                char *type = currentSymbol->type;
                if (strcmp(type, "array") == 0) {
                    return currentSymbol->elementType;
                }
                return currentSymbol->type;
            }
        }
    }

    /* error: not declared */
}

/* Dumps all contents in the symbol table of current scope and its entries when exiting a scope */
static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    
    int index = 0;
    Symbol *currentSymbol;
    for (currentSymbol = currentTable->firstSymbol; currentSymbol != NULL; currentSymbol = currentSymbol->nextSymbol) {
        printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            index++, currentSymbol->name, currentSymbol->type, currentSymbol->address, currentSymbol->lineno, currentSymbol->elementType);
    }

    Table *prevTable = currentTable->prevTable;
    free(currentTable);
    currentTable = prevTable;

    if (prevTable != NULL) {
        currentTable->nextTable = NULL;
    }
}

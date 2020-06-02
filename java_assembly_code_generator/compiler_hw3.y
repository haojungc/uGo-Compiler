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
    static void insert_symbol(char*, bool, char*);      /* insert_symbol(id, isArray, typeName) */
    static char *lookup_symbol(char*);                  /* Returns the type name of the symbol */
    static void dump_symbol();
    static char abbr(char*);                            /* Gets the abbreviation of type name */
    static char *get_type(char*);                       /* Converts literal type to type name */
    static void check_operation(char*, char*, char*);   /* check_operation(type of LHS, type of RHS, operator) */
    static void check_assignment(char*, char*, char*);  /* check_assignment(type of LHS, type of RHS, operator) */
    static void check_condition(char*);                 /* Checks if the type is boolean */

    Table *firstTable = NULL;
    Table *currentTable = NULL;

    int address = 0;
    int scope = 0;
%}

%error-verbose

/* Uses variable or self-defined structure to represent
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
%type <type> expr type_name unaryExpr primaryExpr conversionExpr indexExpr operand literal
%type <op_type> assign_op

/* Precedence from low to high */
%right '='
%left LOR
%left LAND
%left '<' '>' GEQ LEQ EQL NEQ
%left '+' '-'
%left '*' '/' '%'

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
    | ifStmt NEWLINE
    | forStmt NEWLINE
    | printStmt NEWLINE
    | NEWLINE
    ;

expr
    : expr LOR expr     { $$ = "bool"; check_operation(get_type($1), get_type($3), "LOR"); printf("LOR\n"); }
    | expr LAND expr    { $$ = "bool"; check_operation(get_type($1), get_type($3), "LAND"); printf("LAND\n"); }
    | expr '<' expr     { $$ = "bool"; printf("LSS\n"); }
    | expr '>' expr     { $$ = "bool"; printf("GTR\n"); }
    | expr GEQ expr     { $$ = "bool"; printf("GEQ\n"); }
    | expr LEQ expr     { $$ = "bool"; printf("LEQ\n"); }
    | expr EQL expr     { $$ = "bool"; printf("EQL\n"); }
    | expr NEQ expr     { $$ = "bool"; printf("NEQ\n"); }
    | expr '+' expr     { $$ = $1; check_operation(get_type($1), get_type($3), "ADD"); printf("ADD\n"); }
    | expr '-' expr     { $$ = $1; check_operation(get_type($1), get_type($3), "SUB"); printf("SUB\n"); }
    | expr '*' expr     { $$ = $1; check_operation(get_type($1), get_type($3), "MUL"); printf("MUL\n"); }
    | expr '/' expr     { $$ = $1; check_operation(get_type($1), get_type($3), "QUO"); printf("QUO\n"); }
    | expr '%' expr     { $$ = $1; check_operation(get_type($1), get_type($3), "REM"); printf("REM\n"); }
    | unaryExpr         { $$ = $1; }
    ;

dcl
    : VAR IDENT type_name '=' expr      { bool isArray = false; insert_symbol($2, isArray, $3); }
    | VAR IDENT type_name               { bool isArray = false; insert_symbol($2, isArray, $3); }
    | VAR IDENT indexExpr type_name     { bool isArray = true; insert_symbol($2, isArray, $4); }
    ;

type_name
    : INT               { $$ = "int32"; }
    | FLOAT             { $$ = "float32"; }
    | BOOL              { $$ = "bool"; }
    | STRING            { $$ = "string"; }
    ;

indexExpr
    : '[' expr ']'
    | primaryExpr '[' expr ']'      { $$ = $1; }
    ;

simpleStmt
    : assignmentStmt
    | expr
    | incDecStmt
    ;

block
    : left_brace stmts right_brace
    ;

left_brace
    : '{'       { scope++; create_table(); }
    ;

right_brace
    : '}'       { dump_symbol(); scope--; }
    ;

ifStmt
    : IF condition block
    | IF condition block ELSE ifStmt
    | IF condition block ELSE block
    ;

condition
    : expr      { check_condition(get_type($1)); }
    ;

forStmt
    : FOR condition block
    | FOR forClause block
    ;

forClause
    : initStmt ';' condition ';' postStmt
    ;

initStmt
    : simpleStmt
    ;

postStmt
    : simpleStmt
    ;

assignmentStmt
    : expr assign_op expr   { check_assignment($1, get_type($3), $2); printf("%s\n", $2); }
    | expr '=' expr         { check_operation(get_type($1), get_type($3), "ASSIGN"); printf("ASSIGN\n"); }
    ;

unaryExpr
    : primaryExpr       { $$ = $1; }
    | '+' unaryExpr     { $$ = $2; printf("POS\n"); }
    | '-' unaryExpr     { $$ = $2; printf("NEG\n"); }
    | '!' unaryExpr     { $$ = $2; printf("NOT\n"); }
    ;

primaryExpr
    : operand           { $$ = $1; }
    | indexExpr         { $$ = $1; }
    | conversionExpr    { $$ = $1; }
    ;

operand
    : literal           { $$ = $1; }
    | '(' expr ')'      { $$ = $2; }
    | IDENT             { $$ = lookup_symbol($1); }
    ;

conversionExpr
    : type_name '(' expr ')'    { $$ = $1; printf("%c to %c\n", abbr($3), abbr($1)); }
    ;

literal
    : INT_LIT               { $$ = "int32_lit"; printf("INT_LIT %d\n", $1); }
    | FLOAT_LIT             { $$ = "float32_lit"; printf("FLOAT_LIT %f\n", $1); }
    | BOOL_LIT              { $$ = "bool_lit"; printf("%s\n", $1); }
    | '"' STRING_LIT '"'    { $$ = "string_lit"; printf("STRING_LIT %s\n", $2); free($2); }
    ;

assign_op
    : ADD_ASSIGN            { $$ = "ADD_ASSIGN"; }
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
    : PRINT '(' expr ')'    { printf("PRINT %s\n", get_type($3)); }
    | PRINTLN '(' expr ')'  { printf("PRINTLN %s\n", get_type($3)); }
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
    create_table(); /* Creates the first table */
    yyparse();
    dump_symbol();

	printf("Total lines: %d\n", yylineno);
    fclose(yyin);
    return 0;
}

/* Creates a symbol table when entering a new scope */
static void create_table() {
    Table *newTable = malloc(sizeof(Table));
    
    /* Initializes the new table */
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
    /* Checks if the symbol is already declared in the same scope */
    Symbol *currentSymbol;
    for (currentSymbol = currentTable->firstSymbol; currentSymbol != NULL; currentSymbol = currentSymbol->nextSymbol) {
        char *_name = currentSymbol->name;
        if (strcmp(_name, name) == 0) {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, _name, currentSymbol->lineno);
            return;
        }
    }
    
    /* Creates a new symbol */
    Symbol *newSymbol = malloc(sizeof(Symbol));
    newSymbol->name = name;
    newSymbol->address = address++;
    newSymbol->nextSymbol = NULL;
    if (isArray) {
        newSymbol->lineno = yylineno + 1;
        newSymbol->type = strdup("array");
        newSymbol->elementType = strdup(type);
    } else {
        newSymbol->lineno = yylineno;
        newSymbol->type = strdup(type);
        newSymbol->elementType = strdup("-");
    }

    if (currentTable->firstSymbol == NULL) {
        currentTable->firstSymbol = newSymbol;
    } else {
        /* Gets the last symbol of the current table */
        Symbol *currentSymbol;
        for (currentSymbol = currentTable->firstSymbol; currentSymbol->nextSymbol != NULL; currentSymbol = currentSymbol->nextSymbol)
            ;
        currentSymbol->nextSymbol = newSymbol;
    }

    printf("> Insert {%s} into symbol table (scope level: %d)\n", name, scope);
}

/* Looks up an entry in the symbol table */
static char *lookup_symbol(char *symbol) {
    /* Finds the matched symbol in the symbol tables */
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

    /* Undefined symbol */
    printf("error:%d: undefined: %s\n", yylineno + 1, symbol);
    return "undefined";
}

/* Dumps all contents in the symbol table of current scope and its entries when exiting a scope */
static void dump_symbol() {
    printf("> Dump symbol table (scope level: %d)\n", scope);
    printf("%-10s%-10s%-10s%-10s%-10s%s\n",
           "Index", "Name", "Type", "Address", "Lineno", "Element type");
    
    /* Prints all the symbols in the current scope */
    Symbol *currentSymbol, *dumpedSymbol;
    int index = 0;
    for (currentSymbol = currentTable->firstSymbol; currentSymbol != NULL;) {
        printf("%-10d%-10s%-10s%-10d%-10d%s\n",
            index++, currentSymbol->name, currentSymbol->type, currentSymbol->address, currentSymbol->lineno, currentSymbol->elementType);
        
        /* Shifts to the next symbol and releases memory */
        dumpedSymbol = currentSymbol;
        currentSymbol = currentSymbol->nextSymbol;
        free(dumpedSymbol->name);
        free(dumpedSymbol->type);
        free(dumpedSymbol->elementType);
        free(dumpedSymbol);
    }

    /* Deletes the current table and releases the memory */
    Table *prevTable = currentTable->prevTable;
    free(currentTable);
    currentTable = prevTable;

    if (prevTable != NULL) {
        currentTable->nextTable = NULL;
    }
}

static char abbr(char *type) {
    if (strcmp(type, "int32") == 0 || strcmp(type, "int32_lit") == 0)      return 'I';
    if (strcmp(type, "float32") == 0 || strcmp(type, "float32_lit") == 0)    return 'F';

    /* error: cannot be converted */
}

static char *get_type(char *type) {
    if (strcmp(type, "int32") == 0 || strcmp(type, "int32_lit") == 0)
        return "int32";
    if (strcmp(type, "float32") == 0 || strcmp(type, "float32_lit") == 0)
        return "float32";
    if (strcmp(type, "bool") == 0 || strcmp(type, "bool_lit") == 0)
        return "bool";
    if (strcmp(type, "string") == 0 || strcmp(type, "string_lit") == 0)
        return "string";
    
    return "undefined";
}

static void check_operation(char *left_type, char *right_type, char *op) {
    /* Undefined symbol */
    bool undefined = (strcmp(left_type, "undefined") == 0) || (strcmp(right_type, "undefined") == 0);
    if (undefined) return;

    /* Invalid LOR & LAND operation */
    bool left_isBool = (strcmp(left_type, "bool") == 0),
         right_isBool = (strcmp(right_type, "bool") == 0);
    bool invalid_LOR_LAND = (strcmp(op, "LOR") == 0 || strcmp(op, "LAND") == 0) && (!left_isBool || !right_isBool);
    if (invalid_LOR_LAND) {
        char *invalid_type = (left_isBool) ? right_type : left_type;
        printf("error:%d: invalid operation: (operator %s not defined on %s)\n", 
                yylineno, op, invalid_type);
        return;
    }

    /* Invalid REM operation */
    bool left_isInt32 = (strcmp(left_type, "int32") == 0),
         right_isInt32 = (strcmp(right_type, "int32") == 0);
    bool invalid_REM = (strcmp(op, "REM") == 0) && (!left_isInt32 || !right_isInt32);
    if (invalid_REM) {
        char *invalid_type = (left_isInt32) ? right_type : left_type;
        printf("error:%d: invalid operation: (operator REM not defined on %s)\n", 
                yylineno, invalid_type);
        return;
    }

    /* Other invalid operations */
    if (strcmp(left_type, right_type) != 0) {
        printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", 
                yylineno, op, left_type, right_type);
        return;
    }
}

static void check_assignment(char *left_type, char *right_type, char *op) {
    /* Assigning value to literal */
    bool isLiteral = strcmp(left_type, "int32_lit") == 0 ||
                     strcmp(left_type, "float32_lit") == 0 ||
                     strcmp(left_type, "bool_lit") == 0 ||
                     strcmp(left_type, "string_lit") == 0;
    if (isLiteral) {
        printf("error:%d: cannot assign to %s\n", yylineno, get_type(left_type));
        return;
    }

    /* Assigning value to different type of symbol */
    if (strcmp(left_type, right_type) != 0) {
        printf("error:%d: cannot assign to %s\n", yylineno, get_type(left_type));
        return;
    }
}

/* Checks if the type is boolean */
static void check_condition(char *type) {
    if (strcmp(type, "bool") != 0)
        printf("error:%d: non-bool (type %s) used as for condition\n", yylineno + 1, type);
}

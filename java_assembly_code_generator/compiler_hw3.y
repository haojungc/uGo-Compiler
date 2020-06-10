/*	Definition section */
%{
    #include "common.h" //Extern variables that communicate with lex
    // #define YYDEBUG 1
    // int yydebug = 1;
    #define MAX_SCOPE 10
    #define MAX_IF 50

    extern int yylineno;
    extern int yylex();
    extern FILE *yyin;

    void yyerror (char const *s)
    {
        printf("error:%d: %s\n", yylineno, s);
    }

    /* Symbol table function - you can add new function if needed. */
    static void create_table();
    static int insert_symbol(char*, bool, char*);       /* insert_symbol(id, isArray, typeName): Returns the address of the symbol */
    static char *lookup_symbol(char*, int*, bool*);     /* lookup_symbol(symbol, symbol_address, isArray): Returns the type name of the symbol */
    static void dump_symbol();
    static char abbr(char*);                            /* Gets the abbreviation of type name */
    static char *get_type(char*);                       /* Converts literal type to type name */
    static void check_operation(char*, char*, char*);   /* check_operation(type of LHS, type of RHS, operator) */
    static void check_assignment(char*, char*, char*);  /* check_assignment(type of LHS, type of RHS, operator) */
    static void check_condition(char*);                 /* Checks if the type is boolean */

    Table *firstTable = NULL;
    Table *currentTable = NULL;

    int address = 0;    /* Global symbol address */
    int symbol_address; /* Stores the global address of a symbol */
    int scope = 0;

    FILE *assembly_file;
    char *file_name = "hw3.j";
    bool error = false;
    int cmp_count = 0;
    int if_count[MAX_SCOPE] = {0};          /* if_count[scope] */
    int else_count[MAX_SCOPE][MAX_IF];      /* else_count[scope][if_count[scope]]: Stores the number of else of an complete if statement */
    int for_count[MAX_SCOPE] = {0};
    int block_count[MAX_SCOPE] = {0};
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
    : dcl NEWLINE           { fprintf(assembly_file, "\n"); }
    | simpleStmt NEWLINE    { fprintf(assembly_file, "\n"); }
    | block NEWLINE         { fprintf(assembly_file, "\n"); }
    | ifStmt NEWLINE        { fprintf(assembly_file, "\n"); }
    | forStmt NEWLINE       { fprintf(assembly_file, "\n"); }
    | printStmt NEWLINE     { fprintf(assembly_file, "\n"); }
    | NEWLINE               { fprintf(assembly_file, "\n"); }
    ;

expr
    : expr LOR expr     { $$ = "bool"; check_operation(get_type($1), get_type($3), "LOR"); printf("LOR\n"); fprintf(assembly_file, "\tior\n"); }
    | expr LAND expr    { $$ = "bool"; check_operation(get_type($1), get_type($3), "LAND"); printf("LAND\n"); fprintf(assembly_file, "\tiand\n"); }
    | expr '<' expr     { 
        $$ = "bool";
        printf("LSS\n"); 
        
        char type = abbr($1);
        bool isInt32 = (type == 'I');
        fprintf(assembly_file, "\t%s\n\t%s%d\n\t%s\n\t%s%d\n%s%d%c\n\t%s\n%s%d%c\n", 
                isInt32 ? "isub" : "fsub\n\tf2i",
                "iflt L_cmp_", cmp_count,
                "iconst_0",
                "goto L_cmp_", cmp_count + 1,
                "L_cmp_", cmp_count, ':',
                "iconst_1",
                "L_cmp_", cmp_count + 1, ':');
        cmp_count += 2;
    }
    | expr '>' expr     { 
        $$ = "bool"; 
        printf("GTR\n"); 
        
        char type = abbr($1);
        bool isInt32 = (type == 'I');
        fprintf(assembly_file, "\t%s\n\t%s%d\n\t%s\n\t%s%d\n%s%d%c\n\t%s\n%s%d%c\n", 
                isInt32 ? "isub" : "fsub\n\tf2i",
                "ifgt L_cmp_", cmp_count,
                "iconst_0",
                "goto L_cmp_", cmp_count + 1,
                "L_cmp_", cmp_count, ':',
                "iconst_1",
                "L_cmp_", cmp_count + 1, ':');
        cmp_count += 2;
    }
    | expr GEQ expr     { 
        $$ = "bool"; 
        printf("GEQ\n"); 
        
        char type = abbr($1);
        bool isInt32 = (type == 'I');
        fprintf(assembly_file, "\t%s\n\t%s%d\n\t%s\n\t%s%d\n%s%d%c\n\t%s\n%s%d%c\n", 
                isInt32 ? "isub" : "fsub\n\tf2i",
                "ifge L_cmp_", cmp_count,
                "iconst_0",
                "goto L_cmp_", cmp_count + 1,
                "L_cmp_", cmp_count, ':',
                "iconst_1",
                "L_cmp_", cmp_count + 1, ':');
        cmp_count += 2;
    }
    | expr LEQ expr     { 
        $$ = "bool"; 
        printf("LEQ\n"); 
        
        char type = abbr($1);
        bool isInt32 = (type == 'I');
        fprintf(assembly_file, "\t%s\n\t%s%d\n\t%s\n\t%s%d\n%s%d%c\n\t%s\n%s%d%c\n", 
                isInt32 ? "isub" : "fsub\n\tf2i",
                "ifle L_cmp_", cmp_count,
                "iconst_0",
                "goto L_cmp_", cmp_count + 1,
                "L_cmp_", cmp_count, ':',
                "iconst_1",
                "L_cmp_", cmp_count + 1, ':');
        cmp_count += 2;
    }
    | expr EQL expr     { 
        $$ = "bool"; 
        printf("EQL\n"); 
        
        char type = abbr($1);
        bool isInt32 = (type == 'I');
        fprintf(assembly_file, "\t%s\n\t%s%d\n\t%s\n\t%s%d\n%s%d%c\n\t%s\n%s%d%c\n", 
                isInt32 ? "isub" : "fsub\n\tf2i",
                "ifeq L_cmp_", cmp_count,
                "iconst_0",
                "goto L_cmp_", cmp_count + 1,
                "L_cmp_", cmp_count, ':',
                "iconst_1",
                "L_cmp_", cmp_count + 1, ':');
        cmp_count += 2;
    }
    | expr NEQ expr     { 
        $$ = "bool"; 
        printf("NEQ\n"); 
        
        char type = abbr($1);
        bool isInt32 = (type == 'I');
        fprintf(assembly_file, "\t%s\n\t%s%d\n\t%s\n\t%s%d\n%s%d%c\n\t%s\n%s%d%c\n", 
                isInt32 ? "isub" : "fsub\n\tf2i",
                "ifne L_cmp_", cmp_count,
                "iconst_0",
                "goto L_cmp_", cmp_count + 1,
                "L_cmp_", cmp_count, ':',
                "iconst_1",
                "L_cmp_", cmp_count + 1, ':');
        cmp_count += 2;
    }
    | expr '+' expr     { 
        $$ = $1; 
        check_operation(get_type($1), get_type($3), "ADD"); 
        printf("ADD\n"); 
        char *op = (strcmp(get_type($1), "int32") == 0) ? "iadd" : "fadd";
        fprintf(assembly_file, "\t%s\n", op); 
    }
    | expr '-' expr     { 
        $$ = $1; 
        check_operation(get_type($1), get_type($3), "SUB"); 
        printf("SUB\n");
        char *op = (strcmp(get_type($1), "int32") == 0) ? "isub" : "fsub";
        fprintf(assembly_file, "\t%s\n", op); 
    }
    | expr '*' expr     { 
        $$ = $1; 
        check_operation(get_type($1), get_type($3), "MUL"); 
        printf("MUL\n"); 
        char *op = (strcmp(get_type($1), "int32") == 0) ? "imul" : "fmul";
        fprintf(assembly_file, "\t%s\n", op);
    }
    | expr '/' expr     { 
        $$ = $1; 
        check_operation(get_type($1), get_type($3), "QUO"); 
        printf("QUO\n"); 
        char *op = (strcmp(get_type($1), "int32") == 0) ? "idiv" : "fdiv";
        fprintf(assembly_file, "\t%s\n", op);
    }
    | expr '%' expr     { $$ = $1; check_operation(get_type($1), get_type($3), "REM"); printf("REM\n"); fprintf(assembly_file, "\t%s\n", "irem"); }
    | unaryExpr         { $$ = $1; }
    ;

dcl
    : VAR IDENT type_name '=' expr      { 
        char *type = $3,
             *store_type;
        bool isArray = false;
        int address = insert_symbol($2, isArray, type);

        switch (abbr(type)) {
            case 'I':
                store_type = "istore";
            break;
            case 'F':
                store_type = "fstore";
            break;
            case 'B':
                store_type = "istore";
            break;
            case 'S':
                store_type = "astore";
            break;
        }
        fprintf(assembly_file, "\t%s %d\n", store_type, address);
    }
    | VAR IDENT type_name               {
        char *type = $3;
        bool isArray = false;
        int address = insert_symbol($2, isArray, type); 

        switch (abbr(type)) {
            case 'I':
                fprintf(assembly_file, "\t%s\n\t%s %d\n",
                    "ldc 0",
                    "istore", address);
            break;
            case 'S':
                fprintf(assembly_file, "\t%s\n\t%s %d\n",
                    "ldc \"\"",
                    "astore", address);
            break;
        }
    }
    | VAR IDENT indexExpr type_name     { 
        bool isArray = true,
             isInt32 = (abbr($4) == 'I');
        int address = insert_symbol($2, isArray, $4); 
        
        fprintf(assembly_file, "\t%s %s\n\t%s %d\n",
                "newarray", isInt32 ? "int" : "float",
                "astore", address);
    }
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
    : '{'   { 
        scope++;
        create_table();
        fprintf(assembly_file, "%s%d_%d :\n", 
                "L_block_begin_", scope - 1, block_count[scope - 1]);
    }
    ;

right_brace
    : '}'   { 
        dump_symbol();
        scope--;
        fprintf(assembly_file, "%s%d_%d :\n", 
                "L_block_end_", scope, block_count[scope]++);
    }
    ;

ifStmt
    : if_and_condition block {
        fprintf(assembly_file, "%s%d_%d_%d :\n%s%d_%d :\n",
                "L_if_false_", scope, if_count[scope], else_count[scope][if_count[scope]],
                "L_if_exit_", scope, if_count[scope]);
        if_count[scope]++;
    }
    | if_and_condition block_and_else ifStmt
    | if_and_condition block_and_else block {
        fprintf(assembly_file, "%s%d_%d :\n",
                "L_if_exit_", scope, if_count[scope]);
        if_count[scope]++;
    }
    ;

if_and_condition
    : IF condition  {
        fprintf(assembly_file, "\t%s %s%d_%d_%d\n",
                "ifeq", "L_if_false_", scope, if_count[scope], else_count[scope][if_count[scope]]);
    }
    ;

block_and_else
    : block ELSE    {
        fprintf(assembly_file, "\t%s %s%d_%d\n%s%d_%d_%d :\n",
                "goto", "L_if_exit_", scope, if_count[scope],
                "L_if_false_", scope, if_count[scope], else_count[scope][if_count[scope]]);
        else_count[scope][if_count[scope]]++;
    }
    ;

condition
    : expr      { check_condition(get_type($1)); }
    ;

forStmt
    : for_and_condition block   {
        fprintf(assembly_file, "\t%s %s_%d_%d\n%s_%d_%d :\n",
                "goto", "L_for_begin", scope, for_count[scope],
                "L_for_exit", scope, for_count[scope]);
        for_count[scope]++;
    }
    | for_and_forClause block   {
        fprintf(assembly_file, "\t%s %s_%d_%d\n%s_%d_%d :\n",
                "goto", "L_for_post", scope, for_count[scope],
                "L_for_exit", scope, for_count[scope]);
        for_count[scope]++;
    }
    ;

for
    : FOR   {
        fprintf(assembly_file, "%s_%d_%d :\n",
                "L_for_begin", scope, for_count[scope]);
    }
    ;

for_and_condition
    : for condition {
        fprintf(assembly_file, "\t%s %s_%d_%d\n",
                "ifeq", "L_for_exit", scope, for_count[scope]);
    }
    ;

for_and_forClause
    : for_and_initStmt condition_2 ';' postStmt  {
        fprintf(assembly_file, "\t%s %s_%d_%d\n%s_%d_%d :\n",
                "goto", "L_for_condition", scope, for_count[scope],
                "L_for_do", scope, for_count[scope]);
    }
    ;

for_and_initStmt
    : for initStmt ';'  {
        fprintf(assembly_file, "%s_%d_%d :\n",
                "L_for_condition", scope, for_count[scope]);
    }
    ;

condition_2
    : condition {
        fprintf(assembly_file, "\t%s %s_%d_%d\n\t%s %s_%d_%d\n%s_%d_%d :\n",
                "ifeq", "L_for_exit", scope, for_count[scope],
                "goto", "L_for_do", scope, for_count[scope],
                "L_for_post", scope, for_count[scope]);
    }
    ;

initStmt
    : simpleStmt
    ;

postStmt
    : simpleStmt
    ;

assignmentStmt
    : IDENT assign_op expr   { 
        int address;
        char *assign_op = $2,
             *type = lookup_symbol($1, &address, NULL);
        check_assignment(get_type(type), get_type($3), assign_op); 
        printf("%s\n", assign_op); 

        if (address != -1) {
            char *load_type,
                 *store_type,
                 *op;

            if (strcmp(assign_op, "ADD_ASSIGN") == 0) {
                switch (abbr(type)) {
                    case 'I':
                        load_type = "iload";
                        store_type = "istore";
                        op = "iadd";
                    break;
                    case 'F':
                        load_type = "fload";
                        store_type = "fstore";
                        op = "fadd";
                    break;
                }
            } else if (strcmp(assign_op, "SUB_ASSIGN") == 0) {
                switch (abbr(type)) {
                    case 'I':
                        load_type = "iload";
                        store_type = "istore";
                        op = "swap\n\tisub";
                    break;
                    case 'F':
                        load_type = "fload";
                        store_type = "fstore";
                        op = "swap\n\tfsub";
                    break;
                }
            } else if (strcmp(assign_op, "MUL_ASSIGN") == 0) {
                switch (abbr(type)) {
                    case 'I':
                        load_type = "iload";
                        store_type = "istore";
                        op = "imul";
                    break;
                    case 'F':
                        load_type = "fload";
                        store_type = "fstore";
                        op = "fmul";
                    break;
                }
            } else if (strcmp(assign_op, "QUO_ASSIGN") == 0) {
                switch (abbr(type)) {
                    case 'I':
                        load_type = "iload";
                        store_type = "istore";
                        op = "swap\n\tidiv";
                    break;
                    case 'F':
                        load_type = "fload";
                        store_type = "fstore";
                        op = "swap\n\tfdiv";
                    break;
                }
            } else if (strcmp(assign_op, "REM_ASSIGN") == 0) {
                load_type = "iload";
                store_type = "istore";
                op = "swap\n\tirem";
            }

            fprintf(assembly_file, "\t%s %d\n\t%s\n\t%s %d\n", 
                    load_type, address,
                    op,
                    store_type,
                    address);
        }
    }
    | IDENT '=' expr         {
        int address;
        char *type = lookup_symbol($1, &address, NULL),
             *store_type;
        check_operation(get_type(type), get_type($3), "ASSIGN"); 
        printf("ASSIGN\n"); 

        if (address != -1) {
            switch (abbr(type)) {
                case 'I':
                case 'B':
                    store_type = "istore";
                break;
                case 'F':
                    store_type = "fstore";
                break;
                case 'S':
                    store_type = "astore";
                break;
            }
            fprintf(assembly_file, "\t%s %d\n", store_type, address);
        }
    }
    | IDENT '[' INT_LIT ']' '=' expr   {
        int address,
            index = $3;
        char *type = lookup_symbol($1, &address, NULL),
             *store_type;
        check_operation(get_type(type), get_type($6), "ASSIGN"); 
        printf("ASSIGN\n"); 

        if (address != -1) {
            switch (abbr(type)) {
                case 'I':
                case 'B':
                    store_type = "iastore";
                break;
                case 'F':
                    store_type = "fastore";
                break;
            }
            fprintf(assembly_file, "\t%s %d\n\t%s\n\t%s %d\n\t%s\n\t%s\n", 
                    "aload", address,
                    "swap",
                    "ldc", index,
                    "swap",
                    store_type);
        }
    }
    | IDENT '[' expr ']' '=' expr    {

    }
    ;

unaryExpr
    : primaryExpr       { $$ = $1; }
    | '+' unaryExpr     { $$ = $2; printf("POS\n"); }
    | '-' unaryExpr     { 
        char *type = $2;
        $$ = type;
        printf("NEG\n");

        bool isInt32 = (abbr(type) == 'I');
        fprintf(assembly_file, "\t%s\n", isInt32 ? "ineg" : "fneg"); 
    }
    | '!' unaryExpr     { $$ = $2; printf("NOT\n"); fprintf(assembly_file, "\t%s\n\t%s\n", "iconst_1", "ixor"); }
    ;

primaryExpr
    : operand           { $$ = $1; }
    | indexExpr         { $$ = $1; }
    | conversionExpr    { $$ = $1; }
    | operand indexExpr { 
        char *type = $1;
        $$ = type;

        bool isInt32 = (abbr(type) == 'I');
        fprintf(assembly_file, "\t%s\n", isInt32 ? "iaload" : "faload"); 
    }
    ;

operand
    : literal           { $$ = $1; }
    | '(' expr ')'      { $$ = $2; }
    | IDENT             { 
        int address;
        bool isArray;
        char *type = lookup_symbol($1, &address, &isArray),
             *load_type;
        $$ = type;
        
        if (address != -1) {
            if (isArray) load_type = "aload";
            else {
                switch (abbr(type)) {
                    case 'I':
                    case 'B':
                        load_type = "iload";
                    break;
                    case 'F':
                        load_type = "fload";
                    break;
                    case 'S':
                        load_type = "aload";
                    break;
                }
            }
            fprintf(assembly_file, "\t%s %d\n", load_type, address);
        }
            
    }
    ;

conversionExpr
    : type_name '(' expr ')'    { 
        char *type_from = $3,
             *type_to = $1;
        char type_from_abbr = abbr(type_from),
             type_to_abbr = abbr(type_to);
        $$ = type_to;
        printf("%c to %c\n", type_from_abbr, type_to_abbr); 

        if (type_from_abbr != type_to_abbr) {
            char *conversion_type;
            if (type_from_abbr == 'I' && type_to_abbr == 'F')
                conversion_type = "i2f";
            else if (type_from_abbr == 'F' && type_to_abbr == 'I')
                conversion_type = "f2i";
            
            fprintf(assembly_file, "\t%s\n", conversion_type);
        }
    }
    ;

literal
    : INT_LIT               { $$ = "int32_lit"; printf("INT_LIT %d\n", $1); fprintf(assembly_file, "\tldc %d\n", $1); }
    | FLOAT_LIT             { $$ = "float32_lit"; printf("FLOAT_LIT %f\n", $1); fprintf(assembly_file, "\tldc %f\n", $1); }
    | BOOL_LIT              { 
        $$ = "bool_lit"; 
        printf("%s\n", $1); 
        char *s = (strcmp($1, "TRUE") == 0) ? "iconst_1" : "iconst_0";
        fprintf(assembly_file, "\t%s\n", s);
    }
    | '"' STRING_LIT '"'    { $$ = "string_lit"; printf("STRING_LIT %s\n", $2); fprintf(assembly_file, "\tldc \"%s\"\n", $2); free($2); }
    ;

assign_op
    : ADD_ASSIGN            { $$ = "ADD_ASSIGN"; }
    | SUB_ASSIGN            { $$ = "SUB_ASSIGN"; }
    | MUL_ASSIGN            { $$ = "MUL_ASSIGN"; }
    | QUO_ASSIGN            { $$ = "QUO_ASSIGN"; }
    | REM_ASSIGN            { $$ = "REM_ASSIGN"; }
    ;

incDecStmt
    : expr INC      { 
        printf("INC\n");
        bool isInt32 = (strcmp($1, "int32") == 0);
        char *op = isInt32 ? "iadd" : "i2f\n\tfadd",
             *store_type = isInt32 ? "istore" : "fstore";
        fprintf(assembly_file, "\t%s\n\t%s\n\t%s %d\n", "ldc 1", op, store_type, symbol_address); 
    }
    | expr DEC      { 
        printf("DEC\n"); 
        bool isInt32 = (strcmp($1, "int32") == 0);
        char *op = isInt32 ? "isub" : "i2f\n\tfsub",
             *store_type = isInt32 ? "istore" : "fstore";
        fprintf(assembly_file, "\t%s\n\t%s\n\t%s %d\n", "ldc 1", op, store_type, symbol_address); 
    }
    ;

printStmt
    : PRINT '(' expr ')'    { 
        char *type = get_type($3),
             *print_param;
        printf("PRINT %s\n", type);

        switch (abbr(type)) {
            case 'B':
                fprintf(assembly_file, "\t%s%d\n\t%s\n\t%s%d\n",
                    "ifne L_cmp_", cmp_count,
                    "ldc \"false\"",
                    "goto L_cmp_", cmp_count + 1);
                fprintf(assembly_file, "%s%d%c\n\t%s\n", "L_cmp_", cmp_count, ':', "ldc \"true\"");
                fprintf(assembly_file, "%s%d%c\n", "L_cmp_", cmp_count + 1, ':');
                cmp_count += 2;
            case 'S':
                print_param = "Ljava/lang/String;";
            break;
            case 'I':
                print_param = "I";
            break;
            case 'F':
                print_param = "F";
            break;
        }
        fprintf(assembly_file, "\t%s\n\t%s\n\t%s%s%s\n",
                "getstatic java/lang/System/out Ljava/io/PrintStream;",
                "swap",
                "invokevirtual java/io/PrintStream/print(", print_param, ")V");
    }
    | PRINTLN '(' expr ')'  { 
        char *type = get_type($3),
             *print_param;
        printf("PRINTLN %s\n", type); 
        
        switch (abbr(type)) {
            case 'B':
                fprintf(assembly_file, "\t%s%d\n\t%s\n\t%s%d\n",
                    "ifne L_cmp_", cmp_count,
                    "ldc \"false\"",
                    "goto L_cmp_", cmp_count + 1);
                fprintf(assembly_file, "%s%d%c\n\t%s\n", "L_cmp_", cmp_count, ':', "ldc \"true\"");
                fprintf(assembly_file, "%s%d%c\n", "L_cmp_", cmp_count + 1, ':');
                cmp_count += 2;
            case 'S':
                print_param = "Ljava/lang/String;";
            break;
            case 'I':
                print_param = "I";
            break;
            case 'F':
                print_param = "F";
            break;
        }
        fprintf(assembly_file, "\t%s\n\t%s\n\t%s%s%s\n",
                "getstatic java/lang/System/out Ljava/io/PrintStream;",
                "swap",
                "invokevirtual java/io/PrintStream/println(", print_param, ")V");
    }
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

    /* Setup Jasmin program */
    assembly_file = fopen(file_name, "w");
    fprintf(assembly_file, "%s\n%s\n%s\n%s\n%s\n%s\n\n",
            ".source hw3.j",
            ".class public Main",
            ".super java/lang/Object",
            ".method public static main([Ljava/lang/String;)V",
            ".limit stack 100 ; Define your storage size.",
            ".limit locals 100 ; Define your local space number.");       

    for (int i = 0; i < MAX_SCOPE; i++)
        for (int j = 0; j < MAX_IF; j++)
            else_count[i][j] = 0;
    
    yylineno = 0;
    create_table(); /* Creates the first table */
    yyparse();
    dump_symbol();

	printf("Total lines: %d\n", yylineno);

    if (error) remove(file_name);
    else fprintf(assembly_file, "\t%s\n%s", "return", ".end method");
    fclose(assembly_file);
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
static int insert_symbol(char *name, bool isArray, char *type) {
    /* Checks if the symbol is already declared in the same scope */
    Symbol *currentSymbol;
    for (currentSymbol = currentTable->firstSymbol; currentSymbol != NULL; currentSymbol = currentSymbol->nextSymbol) {
        char *_name = currentSymbol->name;
        if (strcmp(_name, name) == 0) {
            printf("error:%d: %s redeclared in this block. previous declaration at line %d\n", yylineno, _name, currentSymbol->lineno);
            error = true;
            return -1;
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

    return newSymbol->address;
}

/* Looks up an entry in the symbol table */
static char *lookup_symbol(char *symbol, int *address, bool *isArray) {
    /* Finds the matched symbol in the symbol tables */
    Table *table;
    for (table = currentTable; table != NULL; table = table->prevTable) {
        Symbol *currentSymbol;
        for (currentSymbol = table->firstSymbol; currentSymbol != NULL; currentSymbol = currentSymbol->nextSymbol) {
            char *name = currentSymbol->name;
            if (strcmp(name, symbol) == 0) {
                if (address != NULL)
                    *address = currentSymbol->address;
                symbol_address = currentSymbol->address;
                printf("IDENT (name=%s, address=%d)\n", symbol, currentSymbol->address);
                
                char *type = currentSymbol->type;
                if (strcmp(type, "array") == 0) {
                    if (isArray != NULL) *isArray = true;
                    return currentSymbol->elementType;
                }
                if (isArray != NULL) *isArray = false;
                return currentSymbol->type;
            }
        }
    }

    /* Undefined symbol */
    if (address != NULL) *address = -1;
    symbol_address = -1;
    printf("error:%d: undefined: %s\n", yylineno + 1, symbol);
    error = true;

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
    if (strcmp(type, "int32") == 0 || strcmp(type, "int32_lit") == 0)       return 'I';
    if (strcmp(type, "float32") == 0 || strcmp(type, "float32_lit") == 0)   return 'F';
    if (strcmp(type, "bool") == 0 || strcmp(type, "bool_lit") == 0)         return 'B';
    if (strcmp(type, "string") == 0 || strcmp(type, "string_lit") == 0)     return 'S';

    return 'U'; // undefined
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
        error = true;
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
        error = true;
        return;
    }

    /* Other invalid operations */
    if (strcmp(left_type, right_type) != 0) {
        printf("error:%d: invalid operation: %s (mismatched types %s and %s)\n", 
                yylineno, op, left_type, right_type);
        error = true;
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
        error = true;
        return;
    }

    /* Assigning value to different type of symbol */
    if (strcmp(left_type, right_type) != 0) {
        printf("error:%d: cannot assign to %s\n", yylineno, get_type(left_type));
        error = true;
        return;
    }
}

/* Checks if the type is boolean */
static void check_condition(char *type) {
    if (strcmp(type, "bool") != 0) {
        printf("error:%d: non-bool (type %s) used as for condition\n", yylineno + 1, type);
        error = true;
    }
}

%{
    int tot_line = 1;
    int tot_comment = 0;
%}

integer         -?[0-9]+
real            -?[0-9]+"."[0-9]+
id              [A-Za-z_][A-Za-z0-9_]*
string          \".*\"
c_comment       \/\*.*\*\/
cpp_comment     \/\/[^\n]*\n

%%

"+"             {printf("%-9s\t %s\n", yytext, "ADD");}
"-"             {printf("%-9s\t %s\n", yytext, "SUB");}
"*"             {printf("%-9s\t %s\n", yytext, "MUL");}
"/"             {printf("%-9s\t %s\n", yytext, "QUO");}
"%"             {printf("%-9s\t %s\n", yytext, "REM");}
"++"            {printf("%-9s\t %s\n", yytext, "INC");}
"--"            {printf("%-9s\t %s\n", yytext, "DEC");}

">"             {printf("%-9s\t %s\n", yytext, "GTR");}
"<"             {printf("%-9s\t %s\n", yytext, "LSS");}
">="            {printf("%-9s\t %s\n", yytext, "GEQ");}
"<="            {printf("%-9s\t %s\n", yytext, "LEQ");}
"=="            {printf("%-9s\t %s\n", yytext, "EQL");}
"!="            {printf("%-9s\t %s\n", yytext, "NEQ");}

"="             {printf("%-9s\t %s\n", yytext, "ASSIGN");}
"+="            {printf("%-9s\t %s\n", yytext, "ADD_ASSIGN");}
"-="            {printf("%-9s\t %s\n", yytext, "SUB_ASSIGN");}
"*="            {printf("%-9s\t %s\n", yytext, "MUL_ASSIGN");}
"/="            {printf("%-9s\t %s\n", yytext, "QUO_ASSIGN");}
"%="            {printf("%-9s\t %s\n", yytext, "REM_ASSIGN");}

"&&"            {printf("%-9s\t %s\n", yytext, "LAND");}
"||"            {printf("%-9s\t %s\n", yytext, "LOR");}
"!"             {printf("%-9s\t %s\n", yytext, "NOT");}

"("             {printf("%-9s\t %s\n", yytext, "LPAREN");}
")"             {printf("%-9s\t %s\n", yytext, "RPAREN");}
"["             {printf("%-9s\t %s\n", yytext, "LBRACK");}
"]"             {printf("%-9s\t %s\n", yytext, "RBRACK");}
"{"             {printf("%-9s\t %s\n", yytext, "LBRACE");}
"}"             {printf("%-9s\t %s\n", yytext, "RBRACE");}
";"             {printf("%-9s\t %s\n", yytext, "SEMICOLON");}
","             {printf("%-9s\t %s\n", yytext, "COMMA");}
\"              {printf("%-9s\t %s\n", yytext, "QUOTA");}

"print"         {printf("%-9s\t %s\n", yytext, "PRINT");}
"println"       {printf("%-9s\t %s\n", yytext, "PRINTLN");}

"if"            {printf("%-9s\t %s\n", yytext, "IF");}
"else"          {printf("%-9s\t %s\n", yytext, "ELSE");}
"for"           {printf("%-9s\t %s\n", yytext, "FOR");}

"int32"         {printf("%-9s\t %s\n", yytext, "INT");}
"float32"       {printf("%-9s\t %s\n", yytext, "FLOAT");}
"string"        {printf("%-9s\t %s\n", yytext, "STRING");}
"bool"          {printf("%-9s\t %s\n", yytext, "BOOL");}
"true"          {printf("%-9s\t %s\n", yytext, "TRUE");}
"false"         {printf("%-9s\t %s\n", yytext, "FALSE");}
"var"           {printf("%-9s\t %s\n", yytext, "VAR");}

{integer}       {printf("%-9s\t %s\n", yytext, "INT_LIT");}
{real}          {printf("%-9s\t %s\n", yytext, "FLOAT_LIT");}
{id}            {printf("%-9s\t %s\n", yytext, "IDENT");}
{string}        {printf("%-9s\t %s\n", yytext, "STRING_LIT");}
{c_comment}     {tot_comment++; printf("%-9s\t %s\n", yytext, "C Comment");}
{cpp_comment}   {tot_comment++; printf("%-9s\t %s\n", yytext, "C++ Comment");}
\n              {tot_line++; printf("%-9s\t %s\n", "", "NEWLINE");}
[ \t]           ;

%%

int yywrap() {return 1;}

int main()
{
    yylex();

    printf("\nFinish scanning,\n");
    printf("total line: %d\ncomment line: %d\n", tot_line, tot_comment);

    return 0;
}
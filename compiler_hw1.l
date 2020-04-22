%{
    int tot_line = 1;
    int tot_comment = 0;
%}

integer         [0-9]+
real            [0-9]+"."[0-9]+
id              [A-Za-z_][A-Za-z0-9_]*
string          \".*\"
c_comment       \/\*(.|\n)*\*\/
cpp_comment     \/\/.*\n

%%

"+"             {printf("%-8s \t %s\n", yytext, "ADD");}
"-"             {printf("%-8s \t %s\n", yytext, "SUB");}
"*"             {printf("%-8s \t %s\n", yytext, "MUL");}
"/"             {printf("%-8s \t %s\n", yytext, "QUO");}
"%"             {printf("%-8s \t %s\n", yytext, "REM");}
"++"            {printf("%-8s \t %s\n", yytext, "INC");}
"--"            {printf("%-8s \t %s\n", yytext, "DEC");}

">"             {printf("%-8s \t %s\n", yytext, "GTR");}
"<"             {printf("%-8s \t %s\n", yytext, "LSS");}
">="            {printf("%-8s \t %s\n", yytext, "GEQ");}
"<="            {printf("%-8s \t %s\n", yytext, "LEQ");}
"=="            {printf("%-8s \t %s\n", yytext, "EQL");}
"!="            {printf("%-8s \t %s\n", yytext, "NEQ");}

"="             {printf("%-8s \t %s\n", yytext, "ASSIGN");}
"+="            {printf("%-8s \t %s\n", yytext, "ADD_ASSIGN");}
"-="            {printf("%-8s \t %s\n", yytext, "SUB_ASSIGN");}
"*="            {printf("%-8s \t %s\n", yytext, "MUL_ASSIGN");}
"/="            {printf("%-8s \t %s\n", yytext, "QUO_ASSIGN");}
"%="            {printf("%-8s \t %s\n", yytext, "REM_ASSIGN");}

"&&"            {printf("%-8s \t %s\n", yytext, "LAND");}
"||"            {printf("%-8s \t %s\n", yytext, "LOR");}
"!"             {printf("%-8s \t %s\n", yytext, "NOT");}

"("             {printf("%-8s \t %s\n", yytext, "LPAREN");}
")"             {printf("%-8s \t %s\n", yytext, "RPAREN");}
"["             {printf("%-8s \t %s\n", yytext, "LBRACK");}
"]"             {printf("%-8s \t %s\n", yytext, "RBRACK");}
"{"             {printf("%-8s \t %s\n", yytext, "LBRACE");}
"}"             {printf("%-8s \t %s\n", yytext, "RBRACE");}
";"             {printf("%-8s \t %s\n", yytext, "SEMICOLON");}
","             {printf("%-8s \t %s\n", yytext, "COMMA");}
\"              {printf("%-8s \t %s\n", yytext, "QUOTA");}

"print"         {printf("%-8s \t %s\n", yytext, "PRINT");}
"println"       {printf("%-8s \t %s\n", yytext, "PRINTLN");}

"if"            {printf("%-8s \t %s\n", yytext, "IF");}
"else"          {printf("%-8s \t %s\n", yytext, "ELSE");}
"for"           {printf("%-8s \t %s\n", yytext, "FOR");}

"int32"         {printf("%-8s \t %s\n", yytext, "INT");}
"float32"       {printf("%-8s \t %s\n", yytext, "FLOAT");}
"string"        {printf("%-8s \t %s\n", yytext, "STRING");}
"bool"          {printf("%-8s \t %s\n", yytext, "BOOL");}
"true"          {printf("%-8s \t %s\n", yytext, "TRUE");}
"false"         {printf("%-8s \t %s\n", yytext, "FALSE");}
"var"           {printf("%-8s \t %s\n", yytext, "VAR");}

{integer}       {printf("%-8s \t %s\n", yytext, "INT_LIT");}
{real}          {printf("%-8s \t %s\n", yytext, "FLOAT_LIT");}
{id}            {printf("%-8s \t %s\n", yytext, "IDENT");}
{string}        {
                    int i;
                    char *s = malloc(sizeof(char) * (yyleng - 2));
                    
                    // Copy string literal to s
                    for(i = 0; i < yyleng - 2; i++)
                        s[i] = yytext[i + 1];

                    printf("%-8c \t %s\n", '\"', "QUOTA");
                    printf("%-8s \t %s\n", s, "STRING");
                    printf("%-8c \t %s\n", '\"', "QUOTA");

                    free(s);
                }
{c_comment}     {
                    int i, j;
                    char *s = malloc(sizeof(char) * yyleng);

                    tot_comment++;
                    
                    // print comments and count the total line
                    for(i = 0, j = 0; i < yyleng; i++) {
                        s[j++] = yytext[i];
                        if(yytext[i] == '\n') {
                            tot_line++;
                            printf("%-8s \t %s\n", s, "C Comment");

                            // reset s
                            memset(s, '\0', yyleng);
                            j = 0;
                        }
                    }

                    free(s);
                }
{cpp_comment}   {
                    tot_comment++;
                    printf("%-8s \t %s\n", yytext, "C++ Comment");
                    printf("%-8s \t %s\n", "", "NEWLINE");
                }
\n              {tot_line++; printf("%-8s \t %s\n", "", "NEWLINE");}
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
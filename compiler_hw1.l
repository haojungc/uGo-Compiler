%{
    #define true 1
    #define false 0

    int tot_line = 0;
    int tot_comment = 0;
    int newline = true;
%}

integer         [0-9]+
real            [0-9]+"."[0-9]+
id              [A-Za-z_][A-Za-z0-9_]*
cpp_comment     \/\/.*

%x STRING C_COMMENT CPP_COMMENT

%%

\"                  {printf("%-8s \t %s\n", "\"", "QUOTA"); BEGIN STRING;}
<STRING>\"          {printf("%-8s \t %s\n", "\"", "QUOTA"); BEGIN 0;}
<STRING>[^\"]*      {printf("%-8s \t %s\n", yytext, "STRING");}

"/*"                {
                        ECHO;
                        if(newline == true)
                            tot_comment++;
                        BEGIN C_COMMENT;
                    }
<C_COMMENT>"*/"     {newline = false; printf("%-8s \t %s\n", "*/", "C Comment"); BEGIN 0;}
<C_COMMENT>.        {ECHO;}
<C_COMMENT>\*[^\/]  {ECHO;}
<C_COMMENT>\n       {ECHO; printf("%-8s\t %s\n", "", "C Comment"); tot_line++; tot_comment++;}

{cpp_comment}       {
                        if(newline == true)
                            tot_comment++;
                        printf("%-8s \t %s\n", yytext, "C++ Comment");
                        BEGIN CPP_COMMENT;
                    }
<CPP_COMMENT>\n     {tot_line++; printf("%-8s \t %s\n", "", "NEWLINE"); BEGIN 0;}

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
\n              {newline == true; tot_line++; printf("%-8s \t %s\n", "", "NEWLINE");}
[ \t]           ;

<<EOF>>         {tot_line++; yyterminate();}

%%

int yywrap() {return 1;}

int main()
{
    yylex();

    printf("\nFinish scanning,\n");
    printf("total line: %d\ncomment line: %d\n", tot_line, tot_comment);

    return 0;
}
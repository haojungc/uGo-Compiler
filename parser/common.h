#ifndef COMMON_H
#define COMMON_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>

/* struct ... */
typedef struct symbol {
    char *name;
    char *type;
    int address;
    int lineno;
    char *elementType;
    struct symbol *nextSymbol;
} Symbol;

typedef struct table {
    int scope;
    struct symbol *firstSymbol;
    struct table *prevTable;
    struct table *nextTable;
} Table;

#endif /* COMMON_H */
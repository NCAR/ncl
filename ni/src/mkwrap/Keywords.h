

#include "y.tab.h"

typedef struct key{
	char *uppercase;
	char *lowercase;
	int token;
} KEY;

static KEY fkeytab [] = {
	"PRECISION","precision",PRECISION,
	"DIMENSION","dimension",DIMENSION,
	"DOUBLE","double",DOUBLE,
	"INTEGER", "integer", INTEGER,
	"LOGICAL","logical",LOGICAL,
	"CHARACTER","character", CHARACTER,
	"BYTE","byte",BYTE,
	"SUBROUTINE","subroutine",SUBROUTINE,
	"DIMSIZES","dimsizes",DIMSIZES,
	"DATA","data",DATA,
	"SAVE","save",SAVE,
	"COMMON","common",COMMON,
	"FUNCTION","function",FUNCTION,
	"REAL","real",REAL,
	"IMPLICIT","implicit",IMPLICIT,
	NULL,NULL,0
};
static KEY wkeytab [] = {
	NULL,"double",DOUBLE,
	NULL,"float",FLOAT,
	NULL, "integer", INTEGER,
	NULL,"short", SHORT,
	NULL,"long", LONG,
	NULL,"logical",LOGICAL,
	NULL,"string",STRNG,
	NULL,"char", CHARACTER,
	NULL,"byte",BYTE,
	"RETURN","return",RETURN,
	"PROCEDURE","procedure",PROCEDURE,
	"NEW","new",NEW,
	"NOMISSING","nomissing",NOMISSING,
	"DIMSIZES","dimsizes",DIMSIZES,
	"FUNCTION","function",FUNCTION,
	"MISSING","missing",MISSING,
	"IN","in",IN,
	"OUT","out",OUT,
	"INOUT","inout",INOUT,
	"TYPE","type",TYPE,
	NULL,NULL,0
};

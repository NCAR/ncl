

static char *initial_fmt = "\n\n\n#include <stdio.h>\n"
"/*\n"
"* The following are the required NCAR Graphics include files.\n"
"* They should be located in ${NCARG_ROOT}/include\n"
"*/\n"
"#include <ncarg/hlu/hlu.h>\n"
"#include <ncarg/hlu/NresDB.h>\n"
"#include <ncarg/ncl/defs.h>\n"
"#include <ncarg/ncl/NclDataDefs.h>\n"
"#include <ncarg/ncl/NclBuiltInSupport.h>\n"
"#include <ncarg/gks.h>\n"
"#include <ncarg/ncl/NclBuiltIns.h>\n\n\n\n";

static char *init_fmt= "\n\n\nvoid Init(void)"
"{\n"
"\tvoid *args;\n"
"\tlong dimsizes[NCL_MAX_DIMENSIONS];\n"
"\tint nargs;\n\n\n";

static char *endf_fmt = "}\n";

static char *pdef_fmt = "\n\nNhlErrorTypes %s_W( void ) {\n\tint i;\n";
static char *fdef_fmt = "\n\nNhlErrorTypes %s_W( void ) {\n\tint i;\n";

static char *argval_fmt = "\n\t%s = (%s*) NclGetArgValue(\n\t\t%d,\n\t\t%d,\n\t\t%s,\n\t\t%s,\n\t\t%s,\n\t\t%s,\n\t\t%s,\n\t\t%d);\n\n";


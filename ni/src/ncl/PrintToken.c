#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "parser.h"

#ifdef DEBUGER

void printtoken
#if	NhlNeedProto
(int type,char* name)
#else
(type,name)
	int type;
	char *name;
#endif
{
	if(name != NULL)
		fprintf(stdout,"%s ",name);
	switch(type) {
		case EOLN:
			fprintf(stdout,"EOLN\n");
			break;
		case KEYFUNC:
			fprintf(stdout,"KEYFUNC\n");
			break;
		case KEYPROC:
			fprintf(stdout,"KEYPROC\n");
			break;
		case VSBLKGET:
			fprintf(stdout,"VSBLKGET\n");
			break;
		case VSBLKCREATE:
			fprintf(stdout,"VSBLKCREATE\n");
			break;
		case VSBLKSET:
			fprintf(stdout,"VSBLKSET\n");
			break;
		case ELSE:
			fprintf(stdout,"ELSE\n");
			break;
		case RETURN:
			fprintf(stdout,"RETURN\n");
			break;
		case NCLEXTERNAL:
			fprintf(stdout,"NCLEXTERNAL\n");
			break;
		case EXTERNAL:
			fprintf(stdout,"EXTERNAL\n");
			break;
		case RP:
			fprintf(stdout,"RP\n");
			break;
		case LP:
			fprintf(stdout,"LP\n");
			break;
		case RBC:
			fprintf(stdout,"RBC\n");
			break;
		case LBC:
			fprintf(stdout,"LBC\n");
			break;
		case RBK:
			fprintf(stdout,"RBK\n");
			break;
		case LBK:
			fprintf(stdout,"LBK\n");
			break;
		case COLON:
			fprintf(stdout,"COLON\n");
			break;
		case SEMI:
			fprintf(stdout,"SEMI\n");
			break;
		case INT:
			fprintf(stdout,"INT\n");
			break;
		case DIMNUM:
			fprintf(stdout,"DIMNUM\n");
			break;
		case REAL:
			fprintf(stdout,"REAL\n");
			break;
		case STRING:
			fprintf(stdout,"STRING\n");
			break;
		case DIM:
			fprintf(stdout,"DIM\n");
			break;
		case DIMNAME:
			fprintf(stdout,"DIMNAME\n");
			break;
		case ATTNAME:
			fprintf(stdout,"ATTNAME\n");
			break;
		case INTEGER:
			fprintf(stdout,"INTEGER\n");
			break;
		case UINT:
			fprintf(stdout,"UINT\n");
			break;
		case FLOAT:
			fprintf(stdout,"FLOAT\n");
			break;
		case LONG:
			fprintf(stdout,"LONG\n");
			break;
		case ULONG:
			fprintf(stdout,"ULONG\n");
			break;
		case INT64:
			fprintf(stdout,"INT64\n");
			break;
		case UINT64:
			fprintf(stdout,"UINT64\n");
			break;
		case DOUBLE:
			fprintf(stdout,"DOUBLE\n");
			break;
		case BYTE:
			fprintf(stdout,"BYTE\n");
			break;
		case CHARACTER:
			fprintf(stdout,"CHARACTER\n");
			break;
		case GROUP:
			fprintf(stdout,"GROUP\n");
			break;
		case NUMERIC:
			fprintf(stdout,"NUMERIC\n");
			break;
		case ENUMERIC:
			fprintf(stdout,"ENUMERIC\n");
			break;
		case SNUMERIC:
			fprintf(stdout,"SNUMERIC\n");
			break;
		case FILETYPE:
			fprintf(stdout,"FILETYPE\n");
			break;
		case SHORT:
			fprintf(stdout,"SHORT\n");
			break;
		case USHORT:
			fprintf(stdout,"USHORT\n");
			break;
		case UNDEF:
			fprintf(stdout,"UNDEF\n");
			break;
		case VAR:
			fprintf(stdout,"VAR\n");
			break;
		case FVAR:
			fprintf(stdout,"FVAR\n");
			break;
		case WHILE:
			fprintf(stdout,"WHILE\n");
			break;
		case DO:
			fprintf(stdout,"DO\n");
			break;
		case QUIT:
			fprintf(stdout,"QUIT\n");
			break;
		case NPROC:
			fprintf(stdout,"NPROC\n");
			break;
		case BGIN:
			fprintf(stdout,"BGIN\n");
			break;
		case END:
			fprintf(stdout,"END\n");
			break;
		case NFUNC:
			fprintf(stdout,"NFUNC\n");
			break;
		case FDIM:
			fprintf(stdout,"FDIM\n");
			break;
		case IF:
			fprintf(stdout,"IF\n");
			break;
		case THEN:
			fprintf(stdout,"THEN\n");
			break;
		case VBLKNAME:
			fprintf(stdout,"VBLKNAME\n");
			break;
		case DFILE:
			fprintf(stdout,"DFILE\n");
			break;
		case OR:
			fprintf(stdout,"OR\n");
			break;
		case XOR:
			fprintf(stdout,"XOR\n");
			break;
		case AND:
			fprintf(stdout,"AND\n");
			break;
		case GT:
			fprintf(stdout,"GT\n");
			break;
		case GE:
			fprintf(stdout,"GE\n");
			break;
		case LT:
			fprintf(stdout,"LT\n");
			break;
		case LE:
			fprintf(stdout,"LE\n");
			break;
		case EQ:
			fprintf(stdout,"EQ\n");
			break;
		case NE:
			fprintf(stdout,"NE\n");
			break;
		case UNOP:
			fprintf(stdout,"UNOP\n");
			break;
		case NOT:
			fprintf(stdout,"NOT\n");
			break;
		default:
			fprintf(stdout,"No match for (%c)(%d)\n",(char)type,(int)type);
	}
}
#endif
#ifdef __cplusplus
}
#endif 

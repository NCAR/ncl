
/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 30 10:49:27 MDT 1993
 *
 *	Description:	
 */
#ifndef _NCKeywords_h
#define _NCKeywords_h

#ifdef __cplusplus
extern "C" {
#endif

#include "parser.h"

static struct {
        char *keyword;
        int  token;
}keytab[] = {
	{"while",        WHILE},
{"do",           DO},
{"if",           IF},
{"else",         ELSE},
{"then",         THEN},
{"begin",        BGIN},
{"end",          END},
{"procedure",    KEYPROC},
{"function",     KEYFUNC},
{"quit",         QUIT},
{"Quit",         QUIT},
{"QUIT",         QUIT},
{"integer",      INTEGER},
{"uint",         UINT},
{"float",        FLOAT},
{"long",         LONG},
{"ulong",        ULONG},
{"short",        SHORT},
{"ushort",       USHORT},
{"double",       DOUBLE},
{"character",    CHARACTER},
{"string",    	STRNG},
{"byte",         BYTE},
{"logical",      LOGICAL},
{"file",         FILETYPE},
{"group",        GROUP},
{"numeric",      NUMERIC},
{"enumeric",     ENUMERIC},
{"snumeric",     SNUMERIC},
{"graphic",      GRAPHIC},
{"int64",     	INT64},
{"uint64",	UINT64},
{"ubyte",        UBYTE},
{"return",       RETURN},
{"external",     EXTERNAL},
{"record",	RECORD},
{"create",       VSBLKCREATE},
{"setvalues",       VSBLKSET},
{"getvalues",       VSBLKGET},
{"local",	LOCAL},
{"stop",		STOP},
{"break",	BREAK},
{"continue",	CONTINUE},
{"noparent",	NOPARENT},
{"defaultapp",	NOPARENT},
{"new",		NEW},
{"True",		NCLTRUE},
{"False",	NCLFALSE},
{"list",	LIST},
{"nclexternal",	NCLEXTERNAL},
{"null",	NCLNULL},
{"_Missing",      NCLMISSING},
{"Missing",      NCLMISSING},
/*
{"objdata",	RKEY},
*/
{(char*)NULL,(int)0}
};

#ifdef __cplusplus
}
#endif
#endif /*_NCKeywords_h*/

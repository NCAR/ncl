
/*
 *      $Id: Keywords.h,v 1.3 1993-12-21 19:17:37 ethan Exp $
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

#include <y.tab.h>

static struct {
        char *keyword;
        int  token;
}keytab[] = {
"while",        WHILE,
"do",           DO,
"if",           IF,
"else",         ELSE,
"then",         THEN,
"begin",        BGIN,
"end",          END,
"procedure",    KEYPROC,
"function",     KEYFUNC,
"quit",         QUIT,
"integer",      INTEGER,
"float",        FLOAT,
"long",         LONG,
"short",        SHORT,
"double",       DOUBLE,
"character",    CHARACTER,
"byte",         BYTE,
"file",         FILETYPE,
"numeric",      NUMERIC,
"return",       RETURN,
"external",     EXTERNAL,
"record",	RECORD,
"create",       VSBLKCREATE,
"setvalues",       VSBLKSET,
"getvalues",       VSBLKGET,
"load",		LOAD,
"local",	LOCAL,
"stop",		STOP,
"break",	BREAK,
"continue",	CONTINUE,
/*
"objdata",	RKEY,
*/
"FooLayerClass", OBJTYPE,
"testfunc",	FUNC,
"testvar",	VAR,
"testproc",	PROC,
"testfile",	DFILE,
(char*)NULL,(int)NULL
};

#ifdef __cplusplus
}
#endif
#endif /*_NCKeywords_h*/

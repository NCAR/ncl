
/*
 *      $Id: Keywords.h,v 1.8 1994-10-29 00:57:22 ethan Exp $
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
"Quit",         QUIT,
"QUIT",         QUIT,
"integer",      INTEGER,
"float",        FLOAT,
"long",         LONG,
"short",        SHORT,
"double",       DOUBLE,
"character",    CHARACTER,
"string",    	STRNG,
"byte",         BYTE,
"logical",      LOGICAL,
"file",         FILETYPE,
"numeric",      NUMERIC,
"graphic",      GRAPHIC,
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
"noparent",	NOPARENT,
"new",		NEW,
/*
"objdata",	RKEY,
*/
(char*)NULL,(int)NULL
};

#ifdef __cplusplus
}
#endif
#endif /*_NCKeywords_h*/

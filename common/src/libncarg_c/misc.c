/*
 *	$Id: misc.c,v 1.9 2008-07-27 12:23:45 haley Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  2000                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*LINTLIBRARY*/

#include	<ctype.h>
#include 	<stdlib.h>
#include 	<string.h>
#include 	<errno.h>
#include	"c.h"

/*
 * isint
 *
 *	is a ascii string an integer?
 * on entry 
 *	*s	: a string
 * on exit
 *	return	: 1 => yes, 0 => no
 */
boolean	IsAsciiInt( s )
	const char	*s;
{

	if (*s == '-' || *s == '+') s++;

	while (*s && isdigit(*s)) s++;

	if (*s == '\0') return (TRUE);

	return (FALSE);
}

/*
 * Function:		NmuStrdup(str)
 *
 * Description:		malloc()'s space for new string,
 *			copies it in, and returns the pointer.
 *
 * In Args:		char *str
 *
 * Out Args:		None.
 *
 * Return Values:	char * pointer to new string.
 *
 * Side Effects:	malloc()'s memory.
 */

char *
NmuStrdup(str)
	const char	*str;
{
	char	*p;

	if (str == (char *) NULL) {
		(void) ESprintf(E_UNKNOWN, "NmuStrdup(NULL)");
		return((char *) NULL);
	}

	p = malloc(strlen(str)+1);
	if (p == (char *) NULL) {
		(void) ESprintf(errno, "NmuStrdup(\"%s\")", str);
		return((char *) NULL);
	}

	(void) strcpy(p, str);
	return(p);
}

/*
 *	$Id: misc.c,v 1.5 1992-10-02 16:30:04 don Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                          NCAR View V3.00alpha                        *
*                                                                      *
***********************************************************************/
/*LINTLIBRARY*/

#include	<ctype.h>
#include 	<stdlib.h>
#include	"c.h"

extern int	errno;


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

/*
 *	$Id: misc.c,v 1.4 1992-09-01 23:47:16 clyne Exp $
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


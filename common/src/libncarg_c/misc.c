/*
 *	$Id: misc.c,v 1.3 1992-03-26 18:22:47 clyne Exp $
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


/*
 * isint
 *
 *	is a string an integer?
 * on entry 
 *	*s	: a string
 * on exit
 *	return	: 1 => yes, 0 => no
 */
isint( s )
	char	*s;
{

	if (*s == '-' || *s == '+') s++;

	while (*s && isdigit(*s)) s++;

	if (*s == '\0') return (1);

	return (0);
}


/*
 *	$Id: misc.c,v 1.6 2000-07-11 21:58:07 haley Exp $
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
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this library; if not, write to the Free Software   *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

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

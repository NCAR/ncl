/*
 *	$Id: spooler.c,v 1.7 2000-07-12 18:13:28 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
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
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *	spooler.c
 *	
 *	Author		John Clyne
 *
 *	Date		Fri Oct 19 13:27:31 MDT 1990
 *
 *	This module manages the list of spoolers do be displayed in 
 *	the "print" menu
 */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <ncarg/c.h>

/*
 * 	SpoolerList
 *	[exported]
 *
 *	create a list of spooler names by parsing a ictrans' alias list
 *	The memory for the list returned is valid until the next invocation
 *	of SpoolerList
 * on entry
 *	*alias_list	: a list of spooler alias as created by ictrans
 * on exit
 *	return		: a null terminated list of spooler names
 */
char	**SpoolerList(alias_list)
	const char	*alias_list;
{
	int		i, 
			count;	/* num newline separated spoolers in list */
	const char	*s, *t;
	char		**ptr;

	static	char	**spooler_list = NULL;

	/*
	 *	free memory from previous invocation
	 */
	if (spooler_list) {
		for (ptr = spooler_list; *ptr; ptr++) {
			free((Voidptr) *ptr);
		}
		free((Voidptr) spooler_list);
	}

	/*
	 * count the number of items in the list to make life easier
	 */
	for (count = 0, s = alias_list; *s; s++) {
		if (*s == '\n') count++;
	}   
	if (s != alias_list) count++;

	spooler_list = (char **) 
		malloc((unsigned) (sizeof (char *) * (count + 1)));

	if (! spooler_list) {
		perror("malloc()");
		return(NULL);
	}

	/*
	 * parse the list of spooler names. The list is a newline seperated
	 * string of spooler descriptions of the form:
	 *
	 * 	spooler_name : stuff : more stuff
 	 *
	 */
	for (i = 0, s = alias_list, ptr = spooler_list; i < count; ptr++,i++) {
		while(isspace(*s)) s++;

		t = s;
		while(isprint(*s) && *s != ':') s++;

		if ( !(*ptr = malloc((unsigned) (s - t + 1)))) {
			perror("malloc()");
			return(NULL);
		}
		(void) strncpy (*ptr, t,s-t);
		(*ptr)[s-t] = '\0';

		while(isspace(*s)) s++;

		if (*s != ':') {
			(void) fprintf(stderr, "Error in spool config file\n");
			return(NULL);
		}

		/*
		 * skip ": stuff : more stuff"
		 */
		while (*s != '\n' && *s) s++;
	}
	*ptr = NULL;
		
	return(spooler_list);
}

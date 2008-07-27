/*
 *	$Id: spooler.c,v 1.9 2008-07-27 03:18:38 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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

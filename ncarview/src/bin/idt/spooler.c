/*
 *	$Id: spooler.c,v 1.4 1992-08-12 21:41:59 clyne Exp $
 */
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
#include <ctype.h>
#include <string.h>
#include <ncarv.h>

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
	char	*alias_list;
{
	int	i, 
		count;	/* number of newline separated spoolers in list */
	char	*s, *t;
	char	**ptr;

	static	char	**spooler_list = NULL;

	/*
	 *	free memory from previous invocation
	 */
	if (spooler_list) {
		for (ptr = spooler_list; *ptr; ptr++) {
			cfree((char *) *ptr);
		}
		cfree((char *) spooler_list);
	}

	/*
	 * count the number of items in the list to make life easier
	 */
	for (count = 0, s = alias_list; *s; s++) {
		if (*s == '\n') count++;
	}   
	if (s != alias_list) count++;
	*s = '\n';	/* terminate last item with a newline	*/

	spooler_list = (char **) 
		icMalloc((unsigned) (sizeof (char *) * (count + 1)));

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
		while(isprint(*s) && *s != ':')
			s++;
		if (*s != ':') {
			(void) fprintf(stderr, "Error in spool config file\n");
			return(NULL);
		}
		*s = '\0';
		*ptr = icMalloc((unsigned) (strlen(t) + 1));
		(void) strcpy (*ptr, t);

		while(*s != '\n')
			s++;
		s++;
	}
	*ptr = NULL;
		
	return(spooler_list);
}

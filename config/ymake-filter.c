/*
 *	$Id: ymake-filter.c,v 1.6 1994-03-16 01:35:09 boote Exp $
 */
/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	ymake-filter.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	Originally written 1988
 *		12/91 Modified to deal with Saber-C Makefiles
 *		5/93 Modified to Insert tabs for lines that have \= in them
 *			if they are not a target,macro definition or already
 *			have a leading tab.
 *
 *	Description:
 *		* Removes cpp comments.
 *		* Removes leading blank lines.
 *		* Compresses consecutive blank lines to one.
 *		* Inserts tabs on all lines except for targets,
 *		  macro definitions, and lines that already have
 *		  a leading tab..
 *		
 */

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>

#define TRUE	1
#define FALSE	0

/* These chars indicate no tab at beginning of line, unless escaped */
static char	tab_chars[] = ":=";

main()
{
	char	*line, *getcppline(), *index(), *tchar, *tchar2;
	int	len, lastlen, strlen();
	int	isacppcomment();

	lastlen = 0;

	while ( (line = getcppline()) != NULL )
	{

		if ( isacppcomment(line) ) continue;

		if ( (len = strlen(line)) > 0){
			int	do_tab = TRUE;
			int	i;

			/* look for escapable chars */

			for(i=0; i < sizeof(tab_chars);i++){
				char	*tchar;

				tchar = index(line,tab_chars[i]);
				if(tchar != NULL){
					if((tchar != line) &&
							(*(tchar-1) == '\\')){
						/* eat escape char */
						while(*tchar != '\0'){
							*(tchar-1) = *tchar;
							tchar++;
						}
						*(tchar-1) = *tchar;
					}
					else
						do_tab = FALSE;
				}
			}

			/* don't tab if Make comment or tab'd already */
			if((line[0] == '#') || (line[0] == '\t'))
				do_tab = FALSE;

			if(do_tab)
				(void) printf("\t");

			(void) printf("%s\n", line);
		}
		else{
			if (lastlen > 0) {
				(void) printf("\n");
			}
		}
		lastlen = len;
	}

	(void) exit(0);
}

char *
getcppline()
{
	int		c;
	static char	buf[2048];
	char 		*p;

	p = buf;

	do
	{
		switch(c = getchar())
		{
			/*
			Blanks *and* tabs were originally
			in the first case statement. This
			was changed for use with Saber-C.
			*/

			case ' ':
				if ( p != buf ) *p++ = c;
				break;

			case EOF:
				if ( p == buf ) return (char *) NULL;
				/* fall through */
			case '\n':
				*p = '\0';
				break;
			case ']':
				if ((p != buf) && (*(p-1) == '\\')){
					*(p-1) = c;
					c = ' ';
				}
				else
					*p = '\0';
				break;

			default:
				*p++ = c;
		}
	} while (c != EOF && c != '\n' && c != ']');

	/* remove trailing white */
	while ( --p >= buf && isspace(*p) ) *p = '\0';

	return buf;
}

isacppcomment(line)
char	*line;
{
	if (line[0] == '#' && (line[1] == ' ' || line[1] == '\t') &&
		(isdigit(line[2]) || (line[2] == 'l' && line[3] == 'i' &&
				      line[4] == 'n' && line[5] == 'e' &&
				      line[6] == ' ' && isdigit(line[7]) )))
		return TRUE;
	else
		return FALSE;
}

/*
 *	This function is included in order to avoid Sysv/BSD problems 
 *	on bootstrap installation.
 */
char *
index(s, c)
	char	*s;
	int	c;
{
	while(*s != '\0')
	{
		if (*s == c)
			return s;
		else
			s++;
	}
	return(NULL);
}

/*
 *	$Id: ymake-filter.c,v 1.5 1994-03-14 13:52:05 boote Exp $
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

main()
{
	char	*line, *getcppline(), *index(), *tchar, *tchar2;
	int	len, lastlen, strlen();
	int	isacppcomment();

	lastlen = 0;

	while ( (line = getcppline()) != NULL )
	{
		tchar = NULL;
		tchar2 = NULL;

		if ( isacppcomment(line) ) continue;

		if ( (len = strlen(line)) > 0)
		{
			if ((tchar2 = index(line,':')) == NULL &&
			    line[0] != '#' && line[0] != '\t' &&
			    ((tchar = index(line,'=')) == NULL)) {
				(void) printf("\t");
			}
			/*
			 * Need to add the tab if the '=' is escaped
			 * but be sure the = is not the first charactor in
			 * the line before looking at the char before it.
			 */
			else if(tchar != (char*)NULL && (tchar != line) &&
				(*(tchar-1) == '\\')) {
				(void) printf("\t");
				/* eat escape char */
				while(*tchar != '\0'){
					*(tchar-1) = *tchar;
					tchar++;
				}
				*(tchar-1) = *tchar;
			}
			/*
			 * Need to add the tab if the ':' is escaped
			 * but be sure the : is not the first charactor in
			 * the line before looking at the char before it.
			 */
			else if(tchar2 != (char*)NULL && (tchar2 != line) &&
				(*(tchar2-1) == '\\')) {
				(void) printf("\t");
				/* eat escape char */
				while(*tchar2 != '\0'){
					*(tchar2-1) = *tchar;
					tchar++;
				}
				*(tchar2-1) = *tchar2;
			}

			(void) printf("%s\n", line);
		}
		else
		{
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

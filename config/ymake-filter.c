/*
 *	$Id: ymake-filter.c,v 1.1.1.1 1992-04-17 22:30:09 ncargd Exp $
 */
/*
 *	$Id: ymake-filter.c,v 1.1.1.1 1992-04-17 22:30:09 ncargd Exp $
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

#include <stdio.h>
#include <ctype.h>

#define TRUE	1
#define FALSE	0

main()
{
	char	*line, *getcppline(), *index();
	int	len, lastlen, strlen();
	int	isacppcomment();

	lastlen = 0;

	while ( (line = getcppline()) != NULL )
	{
		if ( isacppcomment(line) ) continue;

		if ( (len = strlen(line)) > 0)
		{
			if (index(line,':') == NULL &&
			    index(line,'=') == NULL &&
			    line[0] != '#' && line[0] != '\t') {
				(void) printf("\t");
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
			case ']':
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

/*
 *	$Id: ymake-filter.c,v 1.12 2003-05-22 15:28:20 haley Exp $
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
#include <string.h>
#include <ctype.h>

#define TRUE	1
#define FALSE	0

/* These chars indicate no tab at beginning of line, unless escaped */
static char	tab_chars[] = {':','='};

int main()
{
	char	*line, *getcppline();
	int	len, lastlen;
	int	isacppcomment();

	lastlen = 0;

	while ( (line = getcppline()) != NULL )
	{

		if ( isacppcomment(line) ) continue;

		if ( (len = strlen(line)) > 0){
			int	do_tab = TRUE;
			int	i;
			char	*tchar;

			/* look for escapable chars */

			for(i=0; i < sizeof(tab_chars);i++){

				tchar = strchr(line,tab_chars[i]);
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

			if(line[0] == '\t'){
				do_tab = FALSE;
				/* Eat any extra tabs... */
				while(line[1] == '\t'){
					tchar = &line[1];
					while(*tchar != '\0'){
						*tchar = *(tchar+1);
						tchar++;
					}
				}
			}
			/* don't tab if Make comment or tab'd already */
			if(line[0] == '#')
				do_tab = FALSE;

			/*
			 * Impliment ## concat for bsd cpp's.
			 */
			if(line[0] != '#'){

				while((tchar = strstr(line,"##")) != NULL){
					/*
					 * if concat is escaped, eat escape
					 * and continue.
					 */
					if((tchar != line) &&
							(*(tchar-1) == '\\')){
						/* eat escape char */
						while(*tchar != '\0'){
							*(tchar-1) = *tchar;
							tchar++;
						}
						*(tchar-1) = *tchar;
					}
					/*
					 * Otherwise, eat concat.
					 */
					else{
						tchar++;tchar++;
						while(*tchar != '\0'){
							*(tchar-2) = *tchar;
							tchar++;
						}
						*(tchar-2) = *tchar;
					}
				}
			}

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

	fflush(stdout);
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

int isacppcomment(line)
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

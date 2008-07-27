/*
 *      File:		input.c
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Mon Jul 22 15:22:02 MDT 1991
 *
 *      Description:	This file contains utilities for getting input from
 *			a character buffer instead of a stream.
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


#include <string.h>
/*
 *	maximum number of characters in the buffer
 */
#define	BUFSIZE	1024



/*
 * pointer to the input buffer
 */
char	*inBufPtr;

/*
 * flag indicating input comes from memory and not from a file stream
 */
int	stringIO	= 0;

/*
 *	AppendString
 *	[exported]
 *
 *	This function allows strings to be concatenated to the input buffer
 *	for future read access using STRING_GETC macro. This macro behaves
 *	exactly like getc(). Each string that is appended also has a 
 *	newline character appended as well.
 *
 * on entry
 *	*s		: string to append.
 * on exit
 *	return		: 0 => ok, -1 => input buffer overflow.
 */
AppendString(s)
	char	*s;
{
	static	char	inBuf[BUFSIZE];	/* the text buffer	*/
	static	short	first = 1;	/* first time called	*/
	static	int	count = 0;	/* num chars in inBuf	*/

	if (first) {
		(void) strcpy(inBuf, s);
		count += strlen(s);
		(void) strcat(inBuf, "\n");
		count += 1;
		
		inBufPtr = inBuf;
		stringIO = 1;
		first = 0;
	}
	else {
		(void) strncat(inBuf, s, BUFSIZE - count - 1);
		count += strlen(s);
		(void) strncat(inBuf, "\n", BUFSIZE - count - 1);
		count += 1;
	}

	/*
	 * are we full yet
	 */
	if ((BUFSIZE - count - 1) < 0) {
		return(-1);
	}

	return(0);
}

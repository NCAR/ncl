/*
 *	$Id: buffer.c,v 1.10 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/
/*
 *	Author:	Tinsley Galyean (tag@boulder.colorado.edu)
 *
 *	Date:	Thu Mar 10 15:15:44 MST 1988
 *
 *	Output buffer management functions
 */

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <errno.h>
#include <ncarg/c.h>
#include "ctrandef.h"

static	int	outFD = -1;		/* fd for output init to stdout */

/* 
 * 	The output buffer
 */
#define		OUTPUT_BUF_SIZE 		1024
static char 	outbuf[OUTPUT_BUF_SIZE];
static int	outbufnum  = 0;

int	GcapOpenBuffer(file)
	char	*file;
{
#ifdef	cray
	int     w_mask = O_TRUNC | O_CREAT | O_WRONLY | O_RAW;
#else
	int     w_mask = O_TRUNC | O_CREAT | O_WRONLY;
#endif
	if (strcmp(file, "stdout") == 0) {
		outFD = fileno(stdout);
		return (1);
	}

	if ((outFD = open(file, w_mask, 0666)) < 0) {
		ESprintf(errno, "open(%s, %d, 0666)", file, w_mask);
		return(-1);
	}
	return(1);
}

/*
 *	Flushes out the output buffer. Called at the end of a picture.
 */
flush()
{
	(void)write(outFD,outbuf,outbufnum);
	outbufnum = 0;
}

/*
 *	Copies the "count" number of chars from "str" into the output buffer,
 *	flushing the buffer if need be.
 */
buffer(str,count)
	SignedChar 	*str;
	int  		count;
{
	register int	tmp;

	while ((tmp = OUTPUT_BUF_SIZE - outbufnum) < count) {

		memmove(&outbuf[outbufnum],str,tmp);
		(void)write(outFD,outbuf,OUTPUT_BUF_SIZE);
		outbufnum = 0; 

		count -= tmp;
		str += tmp;
	}

	memmove(&outbuf[outbufnum],str,count);
	outbufnum += count;
}


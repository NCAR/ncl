/*
 *	$Id: buffer.c,v 1.4 1992-04-16 17:29:55 clyne Exp $
 */
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
#include "ctrandef.h"

static	int	out = 1;		/* fd for output init to stdout */

/* 
 * 	The output buffer
 */
#define		OUTPUT_BUF_SIZE 		1024
static char 	outbuf[OUTPUT_BUF_SIZE];
static int	outbufnum  = 0;

/*
 *	Flushes out the output buffer. Called at the end of a picture.
 */
flush()
{
	(void)write(out,outbuf,outbufnum);
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

		bcopy(str,&outbuf[outbufnum],tmp);
		(void)write(out,outbuf,OUTPUT_BUF_SIZE);
		outbufnum = 0; 

		count -= tmp;
		str += tmp;
	}

	bcopy(str,&outbuf[outbufnum],count);
	outbufnum += count;
}


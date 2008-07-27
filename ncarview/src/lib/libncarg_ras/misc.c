/*
 *	$Id: misc.c,v 1.12 2008-07-27 03:18:46 haley Exp $
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

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	misc.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	1/31/91
 *
 *	Description:
 *		This file contains miscellaneous functions that
 *		are used by other members of the "libraster"
 *		package.
 */
#include <ncarg/c.h>
#include "ncarg_ras.h"

static unsigned long	swaptest = 1;

int
read_swap(fp, nb, buf, swapflag)
	FILE	*fp;
	int	nb;
	char	*buf;
	int	swapflag;
{
	int	status;

	status = fread( (char *) buf, 1, nb, fp);
	if (status != nb) return(RAS_EOF);

	if (swapflag && *(char *) &swaptest)
		_swaplong((char *) buf, (unsigned) nb);
	
	return(RAS_OK);
}

/* Swiped from John Clyne */

_swapshort (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;

    while (bp < ep) {
	c = *bp;
	*bp = *(bp + 1);
	bp++;
	*bp++ = c;
    }
}

_swaplong (bp, n)
    register char *bp;
    register unsigned n;
{
    register char c;
    register char *ep = bp + n;
    register char *sp;

    while (bp < ep) {
	sp = bp + 3;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	sp = bp + 1;
	c = *sp;
	*sp = *bp;
	*bp++ = c;
	bp += 2;
    }
}

/*
 *	ImageCount_()
 *
 *	Return the number of images in a file. Return -1 on error
 */
ImageCount_(name, format)
	char	*name;
	char	*format;
{
	Raster	*ras, *RasterOpen();
	int	count;
	int	rc;

	if ((ras = RasterOpen(name, format)) == (Raster *) NULL) {
		if (ras == (Raster *) NULL) {
			return(RAS_ERROR);
		}
	}

	count = 0;
	while ((rc = RasterRead(ras)) == RAS_OK) {
		count++;
	}
	if (rc == RAS_ERROR) {
		return(rc);
	}

	(void) RasterClose(ras);
	return(count);
}

/*
 *      $Id: memory.c,v 1.4 2008-07-27 03:18:46 haley Exp $
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

/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Don Middleton
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sat Jan 16 23:28:39 MST 1993
 *
 *	Description:	This file contains the functions ras_malloc(),
 *			ras_calloc(), and ras_free(), which duplicate
 *			the function of the standard library routines.
 *			The reason these exist is so that *all* memory
 *			allocation and freeing function for the raster
 *			library can be monitored from a single point.
 *
 *			An environment variable, NCARG_RAS_MEMDEBUG,
 *			can be set and memory debugging information
 *			will be output.
 *			
 */
#include <stdio.h>
#include <stdlib.h>
#include "ncarg_ras.h"

static int	mem_init		= False;
static char	*ncarg_ras_memdebug	= (char *) NULL;

Voidptr
ras_malloc(len)
	unsigned int	len;
{
	Voidptr		p;

	if (!mem_init) {
		ncarg_ras_memdebug = getenv("NCARG_RAS_MEMDEBUG");
	}

	p = (Voidptr) malloc(len);

	if (ncarg_ras_memdebug) {
		(void) fprintf(stderr,
			"ras_malloc(%10d): %12x   %8d bytes\n",
			len, (char *) p, len);
	}

	return(p);
}

Voidptr
ras_calloc(nelem, elsize)
	unsigned int	nelem;
	unsigned int	elsize;
{
	Voidptr		p;

	if (!mem_init) {
		ncarg_ras_memdebug = getenv("NCARG_RAS_MEMDEBUG");
	}

	/* Catch calloc's that might have been coded wrong accidentally.  */

	if (elsize != 1) {
		(void) fprintf(stderr, "ras_calloc(elsize !!!!==== 1)\n");
		return( (Voidptr) NULL);
	}

	p = (Voidptr) calloc(nelem, elsize);

	if (ncarg_ras_memdebug) {
		(void) fprintf(stderr,
			"ras_calloc(%10d): %12x   %8d bytes\n",
			nelem, (char *) p, (nelem*elsize) );
	}

	return(p);
}

void
ras_free(p)
	Voidptr		p;
{
	(void) free(p);
	if (ncarg_ras_memdebug) {
		(void) fprintf(stderr, "ras_free:            %12x\n", p);
	}
}

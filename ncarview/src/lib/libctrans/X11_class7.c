/*
 *	$Id: X11_class7.c,v 1.2 1991-01-09 11:07:41 clyne Exp $
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
/*	X11_class7.c
 *
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *
 *	This file contain the functions that implement class 7 
 *	There are no supported elements at this time.
 */
/*LINTLIBRARY*/



#include <stdio.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	<cterror.h>
#include	"cgmc.h"

/* Class 7 */

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_Message(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Message\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_ApplData(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_ApplData\n");
#endif DEBUG

	return (OK);
}



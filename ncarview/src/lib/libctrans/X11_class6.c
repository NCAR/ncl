/*
 *	$Id: X11_class6.c,v 1.2 1991-01-09 11:07:34 clyne Exp $
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
/*	X11_class6.c
 *
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *
 *	This file contain the functions that implement class 2 
 *	CGM elements. The supported elements are ESCAPE. 
 */
/*LINTLIBRARY*/



#include <stdio.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	<cterror.h>
#include	"cgmc.h"


/* Class 6 */
/*ARGSUSED*/
Ct_err	X11_Escape(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_Escape\n");
#endif DEBUG

	return (OK);
}



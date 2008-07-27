/*
 *	$Id: X11_class6.c,v 1.8 2008-07-27 03:18:42 haley Exp $
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
#include	<ncarg/c.h>
#include	"cgmc.h"


/* Class 6 */
/*ARGSUSED*/
int	X11_Escape(c)
	CGMC *c;
{
	return (0);
}



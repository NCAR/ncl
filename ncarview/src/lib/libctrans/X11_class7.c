/*
 *	$Id: X11_class7.c,v 1.4 1992-07-16 18:06:57 clyne Exp $
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
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ncarv.h>
#include "cgmc.h"

/* Class 7 */

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_Message(c)
	CGMC *c;
{

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_ApplData(c)
	CGMC *c;
{

	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}



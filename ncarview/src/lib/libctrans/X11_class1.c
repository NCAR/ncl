/*
 *	$Id: X11_class1.c,v 1.4 1992-07-16 18:06:41 clyne Exp $
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
/*	X11_class1.c:
 *
 *
 *		Author		John Clyne	(clyne@bierstadt.ucar.edu)
 *				8/19/88
 *
 *	This file contains functions that carry out the necessary 
 *	necessary for class 2 CGM elements. The *supported* elements are: 
 *	METAFILE VERSION, METAFILE DESCRIPTION, METAFILE ELEMENT LIST 
 *	and  METAFILE DEFAULTS REPLACEMENT. These elements are primarily
 *	concerned with information about the format of the CGM.
 */
/*LINTLIBRARY*/


#include 	<stdio.h>
#include 	<errno.h>
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	"cgmc.h"

/* Class 1 */
/*ARGSUSED*/
int	X11_MFVersion(c)
CGMC *c;
{
	return (0);
}

/*ARGSUSED*/
int	X11_MFDesc(c)
CGMC *c;
{
	return (0);
}


/*ARGSUSED*/
int	X11_MFElemList(c)
CGMC *c;
{
	return (0);
}

/*ARGSUSED*/
int	X11_MFDefaults(c)
CGMC *c;
{

	/*
	 *	this function is implemented by the code in X11_BegMF
	 *	and the default table in "default.c"
	 */

	return (0);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
int	X11_CharSetList(c)
CGMC *c;
{
	ESprintf(ENOSYS, "Unsupported CGM element");
	return (-1);
}

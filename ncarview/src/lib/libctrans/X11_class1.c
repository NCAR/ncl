/*
 *	$Id: X11_class1.c,v 1.2 1991-01-09 11:06:58 clyne Exp $
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
#include	<X11/Xlib.h>
#include	<X11/Xutil.h>
#include	<ncarv.h>
#include	<cterror.h>
#include	"cgmc.h"

/* Class 1 */
/*ARGSUSED*/
Ct_err	X11_MFVersion(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_MFVersion\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	X11_MFDesc(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_MFDesc\n");
#endif DEBUG

	return (OK);
}


/*ARGSUSED*/
Ct_err	X11_MFElemList(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_MFElemList\n");
#endif DEBUG

	return (OK);
}

/*ARGSUSED*/
Ct_err	X11_MFDefaults(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_MFDefaults\n");
#endif DEBUG

	/*
	 *	this function is implemented by the code in X11_BegMF
	 *	and the default table in "default.c"
	 */

	return (OK);
}

/*ARGSUSED*/
/* Currently unsupported by NCAR Graphics */
Ct_err	X11_CharSetList(c)
CGMC *c;
{
#ifdef DEBUG
	(void) fprintf(stderr,"X11_CharSetList\n");
#endif DEBUG

	return (OK);
}

/*
 *	$Id: X11_class1.c,v 1.8 2000-08-22 15:11:39 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software         *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
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
#include	<ncarg/c.h>
#include	"cgmc.h"

/* Class 1 */

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

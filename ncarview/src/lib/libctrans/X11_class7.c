/*
 *	$Id: X11_class7.c,v 1.6 2000-07-12 18:00:40 haley Exp $
 */
/************************************************************************
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
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
#include <ncarg/c.h>
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



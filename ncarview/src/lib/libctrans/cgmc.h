/*
 *	$Id: cgmc.h,v 1.7 2000-08-22 03:30:28 haley Exp $
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
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include <ncarg/c.h>

#ifndef _cgmc_
#define _cgmc_

typedef	long	int	CItype;		/* Color Index */

typedef struct	{
	long	int	red,green,blue;
} 			CDtype;		/* Color Direct */

typedef	struct	{
	CItype	index;
	CDtype	direct;
	}		COtype;

typedef short Etype;		/* Enumerated */

typedef long Itype;		/* Integer */

typedef long IXtype;		/* Index */

typedef long VDCtype;	/* VDC space value */

typedef struct {
	VDCtype	x,y;
}			Ptype;		/* point */

typedef double		Rtype;		/* real */

typedef struct {
	char	**string;
	int	*string_space;
} 			Stype;		/* strings */

typedef unsigned char	Dtype;

typedef	unsigned char	Ctype;		/*packed mode encoding of cell array*/

typedef	struct	{
	int	cgmclass;
	int	command;

	unsigned int	CInum,CIspace;
	CItype*		ci;

	unsigned int	CDnum,CDspace;
	CDtype*		cd;

	unsigned int	Enum,Espace;
	Etype*		e;	

	unsigned int	Inum,Ispace;
	Itype*		i;

	unsigned int	IXnum,IXspace;
	IXtype*		ix;

	unsigned int	Pnum,Pspace;
	Ptype*		p;

	unsigned int	Rnum,Rspace;
	Rtype*		r;

	unsigned int	Snum,Sspace;
	Stype*		s;

	unsigned int	VDCnum,VDCspace;
	VDCtype*	vdc;

	unsigned int	Cnum, Cspace;
	Ctype*		c;
	
	boolean 	more;

	int	Dnum,Dspace;
	Dtype*		d;
}	CGMC;

#endif

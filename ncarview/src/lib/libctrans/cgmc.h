/*
 *	$Id: cgmc.h,v 1.8 2008-07-27 03:22:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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

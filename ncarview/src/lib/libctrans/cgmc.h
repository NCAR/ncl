/***********************************************************************
*                                                                      *
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/
#include <ncarv.h>

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

typedef short int	Etype;		/* Enumerated */

typedef long int	Itype;		/* Integer */

typedef long	int	IXtype;		/* Index */

typedef long int	VDCtype;	/* VDC space value */

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
	int	class;
	int	command;

	unsigned short int	CInum,CIspace;
	CItype*		ci;

	unsigned short int	CDnum,CDspace;
	CDtype*		cd;

	unsigned short int	Enum,Espace;
	Etype*		e;	

	unsigned short int	Inum,Ispace;
	Itype*		i;

	unsigned short int	IXnum,IXspace;
	IXtype*		ix;

	unsigned short int	Pnum,Pspace;
	Ptype*		p;

	unsigned short int	Rnum,Rspace;
	Rtype*		r;

	unsigned short int	Snum,Sspace;
	Stype*		s;

	unsigned short int	VDCnum,VDCspace;
	VDCtype*	vdc;

	unsigned short int	Cnum, Cspace;
	Ctype*		c;
	
	boolean 	more;

	short int	Dnum,Dspace;
	Dtype*		d;
}	CGMC;

#endif

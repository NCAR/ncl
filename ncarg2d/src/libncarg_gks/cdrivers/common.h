/*
 *	$Id: common.h,v 1.3 2000-08-22 03:23:31 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
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

/*
 *      File:		common.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Fri May 17 13:17:10 MDT 1991
 *
 *      Description:	These are definitions common to all gks device drivers
 *
 */
#ifndef	_common_
#define	_common_

#include <math.h>

/*
 *	normalized device coordinate space that driver expects all coords
 *	to be in. 
 */
#define	NDC_LLX		0.0	/* lower left x coord		*/
#define	NDC_LLY		0.0	/* lower left y coord		*/
#define	NDC_URX		1.0	/* upper right x coord		*/
#define	NDC_URY		1.0	/* upper right x coord		*/

#define	RINT(A)	((A) > 0 ? (int) ((A) + 0.5) : -(int) (0.5 - (A)))


typedef	unsigned short	Boolean;

#ifndef	FALSE
#define	FALSE	0
#endif

#ifndef	TRUE
#define	TRUE	!FALSE
#endif

#define	GETBITS(TARG,POSS,N) \
	((N)<32 ? (((TARG) >> ((POSS)+1-(N))) & ~(~0 << (N))) : \
	((TARG) >> ((POSS)+1-(N))) )

/* 
 * magnitude of a vector
 */
#define MAG(X1,Y1)      ((float) (sqrt((double) (((X1)*(X1))+((Y1)*(Y1))))))

/*
 * absolute value
 */
#ifndef	ABS
#define	ABS(X)	((X) < 0 ? (-(X)) : (X))
#endif

#ifndef MAX
#define MAX(A,B)	(((A) > (B)) ? (A) : (B))
#endif

#ifndef MIN
#define MIN(A,B)	(((A) < (B)) ? (A) : (B))
#endif


extern	int	Verbose;


#endif	/* _common_	*/

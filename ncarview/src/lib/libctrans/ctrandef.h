/*
 *	$Id: ctrandef.h,v 1.13 2000-07-12 18:00:43 haley Exp $
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
*                          Copyright (C)  1989                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.00 - UNIX Release                  *
*                                                                      *
***********************************************************************/

#ifndef	_ctrandef_
#define	_ctrandef_

#if	defined(__STDC__) || defined(sgi) || defined(RS6000)
typedef	signed char	SignedChar;
#else
typedef	char	SignedChar;
#endif

/*	a macro to determine if two colors are the same	*/
#define COLOR_EQUAL(A,B)	((CSM == INDEXED) ? (A.index == B.index) : ((A.direct.red == B.direct.red)&& (A.direct.green == B.direct.green) && (A.direct.blue == B.direct.blue)))


#define	SQR(X)		((X) * (X))


#ifndef	M_PI
#define M_PI	3.14159265358979323846
#define M_PI_2	1.57079632679489661923
#endif

#ifndef	MAX
#define	MAX(A,B)	(((A) > (B)) ? (A) : (B))
#endif

#ifndef	MIN
#define	MIN(A,B)	(((A) < (B)) ? (A) : (B))
#endif

#ifndef	ABS
#define	ABS(X)          (((X) < (0)) ? -(X) : (X))
#endif

#ifndef	SIGN
#define	SIGN(X)		(((X) < (0)) ? (-1) : (1))
#endif

#ifndef	MIN3
#define	MIN3(A,B,C)	(((A)<(B)) ? ((A)<(C)?(A):(C)) : ((B)<(C)?(B):(C)))
#endif

#ifndef	MAX3
#define	MAX3(A,B,C)	(((A)>(B)) ? ((A)>(C)?(A):(C)) : ((B)>(C)?(B):(C)))
#endif

#ifndef	ROUND
#define	ROUND(A)	((A) > 0 ? (int) ((A) + 0.5) : -(int) (0.5 - (A)))
#endif

#ifndef	ODD
#define	ODD(A)		((A) & 1L)
#endif

typedef	double	Matrix2d[3][3];

/*
 *	This manifest constant is used to index the ClearDevice() function
 *	call
 */
#define	CLEAR_DEVICE	6

#endif	/* _ctrandef_	*/

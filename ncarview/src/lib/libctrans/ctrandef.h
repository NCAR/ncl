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

/*	a macro to determine if two colors are the same	*/
#define COLOR_EQUAL(A,B)	((CSM == INDEXED) ? (A.index == B.index) : ((A.direct.red == B.direct.red)&& (A.direct.green == B.direct.green) && (A.direct.blue == B.direct.blue)))


#define	SQR(X)		((X) * (X))

/*
 *	There is not a absolute value function for longs in in this C
 *	but there will be in Ansii C.  Therefore this should be removed then.
 */
#define	labs(x)	abs((int)(x))



#ifndef	M_PI
#define M_PI	3.14159265358979323846
#define M_PI_2	1.57079632679489661923
#endif

#ifndef	MAX
#define	MAX(A,B)	((A) > (B) ? (A) : (B))
#endif

#ifndef	MIN
#define	MIN(A,B)	((A) < (B) ? (A) : (B))
#endif

#ifndef	ABS
#define	ABS(X)          ((X) < 0 ? -(X) : (X))
#endif

#endif	_ctrandef_

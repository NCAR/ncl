/*
 *	$Id: common.h,v 1.4 2008-07-23 17:29:42 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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

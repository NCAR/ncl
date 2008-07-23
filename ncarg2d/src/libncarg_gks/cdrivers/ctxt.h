/*
 *	$Id: ctxt.h,v 1.4 2008-07-23 17:29:43 haley Exp $
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
 *      File:		ctxt.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Wed May  1 17:49:30 MDT 1991
 *
 *      Description:	clear text device driver header file
 */
#ifndef	_ctxt_driver_
#define	_ctxt_driver_

/*
 *	a point structure
 */
typedef	struct	CTXTPoint_	{
	float	x,y;
	} CTXTPoint;

/*
 *	a color structure
 */
typedef	struct	CTXTColor_	{
	float	r,g,b;
	} CTXTColor;

#endif	/* _ctxt_driver_	*/

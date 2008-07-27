/*
 *	$Id: hppcl.h,v 1.5 2008-07-27 03:22:41 haley Exp $
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

#ifndef	_hppcl_
#define	_hppcl_

#define	HPPCL_RESET	"E"
#define	HPPCL_PORTRAIT	"&l0O"
#define	HPPCL_LANDSCAPE	"&l1O"
#define	HPPCL_ENCODING	""
#define	HPPCL_START	"1A"
#define	HPPCL_END	"B"
#define	HPPCL_EJECT	"&l0H"
#define	HPPCL_POSITION	"%dx%dY"
#define	HPPCL_TRANSFER	"%dW"
#define	HPPCL_RESOLUTION	"%dR"


#ifdef	DEAD
#define	HPPCL_PAPER_WIDTH	8.5
#define	HPPCL_PAPER_HEIGHT	11.0
#endif

#define	HPPCL_PAPER_WIDTH	8.0
#define	HPPCL_PAPER_HEIGHT	10.0

#define	HPPCL_MAX_RES		300	/* default resolution 300dpi	*/

typedef	struct	HPPCL_Info	{
	int		do_compress;
	int		orientation;		
	int		dpi;
	char		*trans_data;
	int		row_size;
	int		image_size;
	int		start_x;
	int		start_y;
	} HPPCL_Info;

#endif	/* _hppcl_ */

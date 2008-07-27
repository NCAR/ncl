/*
 *	$Id: Xcrm.h,v 1.7 2008-07-27 03:22:38 haley Exp $
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

#ifndef	_xcrm_
#define	_xcrm_

#define	RGB_SEARCH	0
#define	PIXEL_SEARCH	1

extern	void	X11_initColorTable();
extern	int	X11_UpdateColorTable_();
extern int	init_color();

typedef struct X11_ColorStatus_{
	int		ref_count;
	unsigned short	red,green,blue;
	Pixeltype	xpixnum;
} X11_ColorStatus;

#endif

/*
 *	$Id: translate.h,v 1.8 2000-07-12 18:00:52 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
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

#ifndef	_translate_
#define	_translate_

#include <stdlib.h>
#include <ncarg/c.h>

/*
 */
#define	DEVWIN_LLX	0
#define	DEVWIN_LLY	0
#define	DEVWIN_URX	32767
#define	DEVWIN_URY	32767

/*
 *	macros to convert from virtual coordinates to device and device
 *	raster coordinates
 */
#define	XConvert(x)	(long) (((x) * (X_Scale)) + X_Off)
#define	YConvert(y)	(long) (((y) * (Y_Scale)) + Y_Off)
#define	R_XConvert(x)	(long) (((x) * (R_X_Scale)) + R_X_Off)
#define	R_YConvert(y)	(long) (((y) * (R_Y_Scale)) + R_Y_Off)

/*
 *	macros to convert back from device coordinates to virtual device coords 
 */
#define	XConvert_(x_)	(long) (((x_) - (X_Off)) * X_Scale)
#define	YConvert_(y_)	(long) (((y_) - (Y_Off)) * Y_Scale)

/*
 * Macros to convert VDC x and y's into device x and y's
 */
#define	XScale(x)	(long) (labs((long) (X_Scale * (x))))
#define	YScale(y)	(long) (labs((long) (Y_Scale * (y))))

/*
 * Macros to scale a device dimension to VDC dimension
 */
#define	XScale_(x_)	(long) (labs((long) (((double) (x_)) / X_Scale )))
#define	YScale_(y_)	(long) (labs((long) (((double) (y_)) / Y_Scale )))

/*
 *	structure to define a coordinate system
 */
typedef	struct	{
	long	llx,	/* lower left x		*/
		lly;	/* lower left y		*/
	long	urx,	/* upper right x	*/
		ury;	/* upper right y	*/
	} CoordRect;

/*
 *	additional translation and scaling to perform on coordinate 
 *	translation
 */
typedef	struct	{
	long	x_off,		/* offset to be added to an X coordinate*/
		y_off;		/* offset to be added to an Y coordinate*/
	double	x_scale,	/* additional X scaling			*/
		y_scale;	/* additional Y scaling			*/
	} CoordModifier;

extern	long	X_Off;
extern	long	Y_Off;
extern	double	X_Scale;
extern	double	Y_Scale;

extern	long	R_X_Off;
extern	long	R_Y_Off;
extern	double	R_X_Scale;
extern	double	R_Y_Scale;



extern	void	transinit(
#ifdef	NeedFuncProto
	CoordRect	*dev_extent,
	CoordModifier	dev_coord_modifier,
	int		device
#endif
);

extern	void	SetDevWin(
#ifdef	NeedFuncProto
	long	llx, 
	long	lly, 
	long	urx, 
	long	ury
#endif
);


extern	void	GetDevWin(
#ifdef	NeedFuncProto
	CoordRect	*dev_win_coord
#endif
);


extern	int	DevWinChanged(
#ifdef	NeedFuncProto
#endif
);


extern	void	SetDevViewport(
#ifdef	NeedFuncProto
	long	llx,
	long	lly,
	long	urx,
	long	ury
#endif
);

extern	CoordRect	PackCoordRect(
#ifdef	NeedFuncProto
	long	llx,
	long	lly,
	long	urx,
	long	ury
#endif
);

#endif

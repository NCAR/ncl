/*
 *	$Id: transform.h,v 1.4 2000-08-22 03:23:30 haley Exp $
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

/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		transform.h
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Nov 18 16:48:45 MST 1992
 *
 *	Description:	
 */

#ifndef	_transform_
#define	_transform_
/*
 *	A linear system which defines a mapping between 2D coordinate
 *	systems.
 */ 
typedef	struct {
	double	x_scale,
		x_trans,
		y_scale,
		y_trans;
	} Transform2D;
	
/*
 *	A coordinate space given by the lower-left and upper-right
 *	corners
 */
typedef	struct	{
	double	llx,
		lly,
		urx,
		ury;
	} CoordSpace;

/*
 *	A complete description of the coord transformation system for
 *	a given screen, window, and viewport
 */
typedef	struct	{
	CoordSpace	screen,
			window,
			viewport,
			nd_screen;	
	} TransSystem;



extern	void	TransformSetScreenSpace(
#ifdef	NeedFuncProto
	TransSystem	*tsystem,
	double		llx,
	double		lly,
	double		urx,
	double		ury
#endif
);

extern	void	TransformSetWindow(
#ifdef	NeedFuncProto
	TransSystem	*tsystem,
	double		llx,
	double		lly,
	double		urx,
	double		ury
#endif
);

extern	void	TransformSetViewport(
#ifdef	NeedFuncProto
	TransSystem	*tsystem,
	double		llx,
	double		lly,
	double		urx,
	double		ury
#endif
);

extern	void	TransformSetNDScreenSpace(
#ifdef	NeedFuncProto
	TransSystem	*tsystem,
	double		llx,
	double		lly,
	double		urx,
	double		ury
#endif
);

extern	Transform2D	TransformGetTransform(
#ifdef	NeedFuncProto
	TransSystem	*tsystem
#endif
);

extern	CoordSpace	ComputeLargestSquare(
#ifdef	NeedFuncProto
	double	llx,
	double	lly,
	double	urx,
	double	ury
#endif
);

#endif	/*	_transform_	*/

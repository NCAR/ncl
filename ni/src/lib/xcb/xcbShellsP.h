/*
 *      $Id: xcbShellsP.h,v 1.1 1997-06-11 20:49:24 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xcbShellsP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu May 29 15:42:32 MDT 1997
 *
 *	Description:	
 */
#ifndef	XCB_SHELLSP_H
#define	XCB_SHELLSP_H

#include <Xcb/xcbShells.h>
#include <X11/IntrinsicP.h>
#include <Xm/XmP.h>
#include <X11/ShellP.h>
#include <Xcb/xcbP.h>

typedef struct XcbShellClassPart XcbShellClassPart;
struct XcbShellClassPart{
	XmOffsetPtr	oset;
};

typedef struct XcbShellPart XcbShellPart;
struct XcbShellPart{
	/*
	 * Over-ride resources.
	 */
	String		background_string;
	Pixel		background_pixel;
	String		border_string;
	Pixel		border_pixel;
	Cardinal	depth;
	Colormap	colormap;
	Visual		*visual;

	Xcb		color_broker;
	Xcb		parent_broker;
	XcbMode		color_mode;
	int		max_color_cells;
	int		min_color_cells;
	int		red_levels;
	int		green_levels;
	int		blue_levels;
	int		rgb_error;

	/* private fields */
	Widget		self;
	Boolean		my_broker;
	_NhlCB		broker_destroyCB;
	_NhlCB		broker_cfaultCB;
};

#endif	/* XCB_SHELLSP_H */

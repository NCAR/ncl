/*
 *      $Id: xinteract.h,v 1.4 1999-02-23 03:56:56 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xinteract.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 28 17:50:54 MDT 1998
 *
 *	Description:	This file has all the declarations for the xinteract.c
 *			source file.
 */

#ifndef	_XINTERACT_H
#define	_XINTERACT_H

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/graphic.h>
#include <ncarg/ngo/xwk.h>

typedef struct _NgXBBox{
        XPoint  p0;
        XPoint  p1;
} NgXBBox;


typedef struct _NgWksObjRec {
	int		parent_id;
	int		wks_wrap_id;
	Const char	*name;
	NgCBWP		cccb;
	NhlBoolean	auto_refresh;
} NgWksObjRec, *NgWksObj;

typedef char NgViewStatus;

#define _ngBASIC_VIEW		0
#define _ngSIMPLE_TRANSFORM	1
#define _ngBASE_PLOT 		2
#define _ngOVERLAY		3
#define _ngANNOTATION		4
#define _ngBASE_PLOT_ANNOTATION	5 /*anno that may have its own overlays/annos*/

typedef struct _NgViewObjRec {
	int			parent_id;
	Const char		*name;
	_NhlCB			ovcb;
	_NhlCB			ancb;
	_NhlCB			svcb;
	NgViewStatus		vstatus;
	NgXBBox			xvp;	  /* X coordinates of viewport */
	NgXBBox			xbbox;    /* X coordinates of BB */
	NhlBoolean		visible;
} NgViewObjRec, *NgViewObj;


extern void NgNDCToXCoord(
	int		xwkid,
	NgXBBox		*xbbox,
	float		x,	
	float		y,	
	float		width,	
	float		height
);

extern void NgXCoordToNDC(
	int		xwkid,
	NgXBBox		*xbbox,
	float		*x,	
	float		*y,	
	float		*width,	
	float		*height
);

extern void NgDrawXwkView(
	int		xwkid,
	int		view_id,
	NhlBoolean	force_clear
);

extern void NgClearXwkView(
	int		xwkid,
	int		view_id
);

extern void NgSetSelectedXwkView(
	int		xwkid,
	int		view_id
);

#endif	/* _XINTERACT_H	*/

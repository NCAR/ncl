/*
 *	$Id: Xdefs.h,v 1.9 2000-07-12 18:00:41 haley Exp $
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
#define	MAX_COLOR_SIZE	256

typedef	unsigned long	Pixeltype;

#define POINTS_ALLOCED  1024
typedef	struct  {
        XPoint  *P;
        int     size;   /* size of array P      */
        } Point_buf;	/* the point buffer     */


#define	X_MAX_RGB	65535
/* sqrt(X_MAX_RGB^2+X_MAX_RGB^2+X_MAX_RGB^2)	*/
#define	X_MAX_INTEN_DIST	113510

#define CM_SHARED	(0)
#define CM_PRIVATE	(1)
#define CM_MIXED	(2)

#ifdef	X11_class0

Display		*dpy;		/* X server connection		*/
Drawable	win = 0;	/* Window ID 			*/
Drawable	drawable;	/* the drawable			*/
Visual		*bestVisual = NULL;
int		DspDepth;	/* depth in pixels of display	*/
Colormap	Cmap;		/* current colormap for $win	*/
int		ColorModel;	/* color model			*/
boolean		MyCmap;		/* I created Cmap		*/
boolean		RoCmap;		/* Read Only Cmap		*/
float		ColorErr;	/* % color error allowed	*/
/*
 *      If true the driver will not attempt to set the background
 *      color or clear the window.
 */
boolean ignoreBGChanges = FALSE;


GC	lineGC;			/* line GC 	See section 5.3	*/
GC	markerGC;		/* polymarker GC		*/
GC	polygonGC;		/* polygon GC			*/
GC	cellGC;			/* cell array GC		*/

Point_buf	Points = {NULL, 0};

XGCValues	gcv;		/* structure for manipulating a GC	*/
#else

extern	Display		*dpy;		
extern	Drawable	win;	
extern	Drawable	drawable;	
extern	Visual		*bestVisual;
extern	int		DspDepth;
extern	Colormap	Cmap;
extern	int		ColorModel;
extern	boolean		MyCmap;
extern	boolean		RoCmap;
extern	int		ColorErr;
extern	boolean 	ignoreBGChanges;

extern	GC	lineGC;	
extern	GC	markerGC;
extern	GC	polygonGC;
extern	GC	cellGC;

extern	Point_buf	Points;

extern	XGCValues	gcv;

#endif	/*	X11_class0	*/


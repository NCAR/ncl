/*
 *      $Id: xcb.h,v 1.3 1998-11-06 00:51:19 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xcb.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar 4 14:09:05 MST 1997
 *
 *	Description:	
 */
#ifndef	XCB_H
#define	XCB_H
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Intrinsic.h>
#include <X11/xpm.h>

#include <ncarg/hlu/hlu.h>

typedef enum XcbMode{
	XcbSHAREDCMAP,
	XcbMIXEDCMAP,
	XcbPRIVATECMAP
} XcbMode;

typedef enum XcbColCmp{
	XcbCmpRGB_DIST,
	XcbCmpFUNC
} XcbColCmp;

/*
 * This function is used to compare a color to a "requested" color.
 * It should return a value indicating how "close" the color is.  If
 * it returns 0.0, that should be considered an exact match.  The smaller
 * the number, the closer the match.  If it is called with "cmpcol == NULL",
 * this function should return the maximum value that is considered
 * a valid value (Or 0.0 if it doesn't consider any difference to be in error).
 * (If this function returns a value greater then that
 * for any compare, xcb shouldn't use that color unless it can't allocate
 * any more colors.)
 * This function is guarenteed to be called with "cmpcol == NULL" the first
 * time Xcb calls it, so if there is initialization to happen it can
 * happen there.  (It will call it with "cmpcol == NULL" again, but hopefully
 * this function will cache the results of any calculation, and save it
 * using the "cmp_data", so it can just return the value for the additional
 * times.)  Each time xcb finds it needs to compute the "closest" color
 * it currently has to a requested color, it will call this function with
 * req = to the requested color and "cmpcol == NULL", so that if the color
 * space comparisons are done it is irregular, they can at least be normalized
 * around the req color.  i.e. if some areas of the colorspace are more
 * sensitive to differences, that can be delt with.
 */
typedef float (*XcbColCmpFunc)(
	XColor	*req,		/* requested color - only use r,g,b fields */
	XColor	*cmpcol,	/* how close is req to this?		*/
	void	*cmp_data	/* passed in cmp_data of XcbAttr	*/
);

typedef struct _XcbRec XcbRec, *Xcb;
typedef struct XcbAttrRec XcbAttrRec, *XcbAttr;

/*
 * ColSetAttr masks
 */
#define	XcbMODE		0x001
#define XcbPARENT	0x002
#define XcbSCREEN	0x004
#define	XcbVIS		0x008
#define	XcbCMAP		0x010	/* MUST set VIS too */
#define XcbMAXNCOLS	0x020
#define XcbRWCOLS	0x040
#define XcbRGBLEVELS	0x080
#define XcbGRAYLEVELS	0x100
#define	XcbCOLCMP	0x200
#define	XcbRGBERR	0x400
#define	XcbINITMOTIF	0x800
#define	XcbMINNCOLS	0x1000

struct XcbAttrRec{
	XcbMode		mode;	/* SHARED,MIXED,PRIVATE			*/

	Xcb		parent;	/* does this Xcb have a parent Xcb	*/
	int		scr;
	Visual		*vis;	/* Which vis should I use		*/
	Colormap	cmap;	/* Which cmap should I use - must set
					visual if you set this.		*/

	int		max_ncols; /* from DefaultColormap		*/
			/*
			 * min_ncols is used to indicate the number of
			 * cols a broker should minimally be able to
			 * provide.  If it can't provide this many, the
			 * broker should switch to a private colormap
			 * (in mixed mode), even if the request comes
			 * from a child broker.  If this broker has
			 * already provided min_ncols, then the child
			 * broker will be denied a color from this
			 * level, and be forced to install a private
			 * colormap itself.  (This should probably only
			 * be set for brokers that could potentially
			 * have a number of broker children.)
			 */
	int		min_ncols;
	int		rw;	/* 0 default				*/

			/*
			 * These indicate the size of the color cube that
			 * the application would "like" to have.  It defaults
			 * to a 3/3/2 cube. (8 is the max because displays
			 * that can actually handle more colors then that
			 * will probably have a TrueColor/DirectColor visual
			 * available, and the cube isn't useful in that
			 * context.)
			 */
	int		rlevels;	/* 8 default/ 8 max	*/
	int		glevels;	/* 8 default/ 8 max	*/
	int		blevels;	/* 4 default/ 8 max	*/
	int		graylevels;	/* 64 default/ 256 max	*/
	XcbColCmp	cmp;		/* XcbCmpRGB_DIST default	*/

			/*
			 * These two are only used if cmp is XcbCmpFUNC
			 */
	XcbColCmpFunc	cmpfunc;
	void		*cmp_data;

			/*
			 * Only used if cmp is XcbCmpRGB_DIST
			 * 0 indicates use closest color in cube - don't worry
			 * about error.  If this error is larger then the
			 * distance between nodes in the RGB cube it will
			 * automatically be ignored.
			 */
	int		rgberr;		/*	10 default	*/
	char		*app_class_name;
};

/*
 * Public API...
 */

extern Visual *
XcbBestDepthVisual(
	Display		*dpy,
	int		screen
);

extern Xcb
XcbCreate(
	Display		*dpy,
	XcbAttr		attr,
	unsigned long	attr_mask
);

extern void
XcbFreeColors(
	Xcb		xcb,
	unsigned long	*pixels,
	int		npixels
);

extern void
XcbFreeColor(
	Xcb		xcb,
	unsigned long	pix
);

extern NhlBoolean
XcbAllocRWColor(
	Xcb		xcb,
	unsigned long	*pix	/* return */
);

extern void
XcbAllocROColor(
	Xcb		xcb,
	XColor		*col
);

/*
 * The "Close" versions of the functions will only return a color that
 * is "on" the internal color cube, instead of trying to allocate the
 * actual color you request.  This can be usefull to minimize color
 * allocations, on the other hand, these "close" colors are not always
 * that "close" to the color you request.
 */
extern void
XcbAllocCloseROColor(
	Xcb		xcb,
	XColor		*col
);

extern Boolean
XcbAllocNamedColor(
	Xcb		xcb,
	String		color_name,
	XColor		*col
);

extern Boolean
XcbAllocCloseNamedColor(
	Xcb		xcb,
	String		color_name,
	XColor		*col
);

extern Visual *
XcbGetVisual(
	Xcb	xcb
);

extern Colormap
XcbGetColormap(
	Xcb	xcb
);

extern unsigned int
XcbGetDepth(
	Xcb	xcb
);

extern void
XcbDestroy(
	Xcb	xcb
);

/*
 * XcbXpm funtions return integers that correspond to the ErrorStatus
 * codes in the Xpm library.  (These are just wrapper functions that
 * make sure the colors are allocated correctly, and allow the colors
 * to be free'd when the pixmap is free'd.
 *
 * The visual, colormap, depth, alloc_color, free_color, and color_closure
 * portions of the XpmAttributes will be over-written by the Xcb, so don't
 * try and use them.
 */
extern int
XcbXpmCreatePixmapFromData(
	Xcb		xcb,
	Drawable	d,
	char		**data,
	Pixmap		*pixmap_return,
	Pixmap		*shapemask_return,
	XpmAttributes	*attributes
);

extern int
XcbXpmReadFileToPixmap(
	Xcb		xcb,
	Drawable	d,
	char		*filename,
	Pixmap		*pixmap_return,
	Pixmap		*shapemask_return,
	XpmAttributes	*attributes
);

extern int
XcbXpmCreatePixmapFromXpmImage(
	Xcb		xcb,
	Drawable	d,
	XpmImage	*image,
	Pixmap		*pixmap_return,
	Pixmap		*shapemask_return,
	XpmAttributes	*attributes
);

/*
 * Free's colors associated with pixmap as well as pixmap.
 */
extern void
XcbFreePixmap(
	Xcb	xcb,
	Pixmap	pixmap
);

/*
 * TODO:
 *
 * The cfaultCBL and destroyCBL callback lists should be publicly
 * accessible, but they aren't yet.  (For now, include the xcbP.h
 * header file, and call _NhlCBAdd directly.)
 */
#endif	/* XCB_COLOR_H */

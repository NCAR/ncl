/*
 *      $Id: xcbP.h,v 1.2 1997-07-02 15:31:11 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1997			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xcbP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar 4 14:08:46 MST 1997
 *
 *	Description:	
 */
#ifndef	XCBP_H
#define	XCBP_H
#include <Xcb/xcb.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/Callbacks.h>
#include <X11/Xutil.h>

/*
 * Max distance in RGB cube.
 */
#define _XcbDIST_BW	(0x1bb65)	/* SQRT(MAXRI^2+MAXGI^2+MAXBI^2) */

#define _XcbMAX_RWRES	8

typedef enum XcbAllocT{
	XcbNONE = 0,
	XcbSTDCMAP,	/* NO allocation - RO usage */
	XcbMYCMAP,	/* RW allocation - ANY usage */
	XcbRO,		/* RO allocation - RO usage */
	XcbRW		/* RW allocation - RW usage */
} XcbAllocT;

typedef enum XcbVisT{
	XcbBW,
	XcbGRAY,
	XcbTRUE,
	XcbCOLOR
} XcbVisT;

typedef enum XcbSortKind{
	XcbNOSORT,
	XcbREFSORT,
	XcbPIXSORT,
	XcbVALSORT
} XcbSortKind;


typedef struct _XcbCStatRec _XcbCStatRec, *_XcbCStat;
struct _XcbCStatRec{
	XColor		xcol;
	XcbAllocT	alloc;
	int		ref;
	NhlBoolean	cube;
	_XcbCStat	next;
};

typedef struct _XcbDMStatRec _XcbDMStatRec, *_XcbDMStat;
struct _XcbDMStatRec{
	unsigned long	index;
	unsigned short	val;
	int		ref;
};

struct _XcbRec{
	Xcb			parent;
	Display			*dpy;
	int			scr;
	Visual			*vis;
	XVisualInfo		*visinfo;

	NhlBoolean		my_cmap;
	Colormap		cmap;

	NhlBoolean		do_stdcmap;
	XStandardColormap	stdcmap;

	XcbMode			mode;
	XcbVisT			vtype;

	int			ncols;

	int			max_ncols;
	int			min_ncols;
	int			rlevels;
	int			glevels;
	int			blevels;
	int			graylevels;

				/*
				 * indexes used for latest _XcbFindCubeNode
				 */
	unsigned long		ired;
	unsigned long		igreen;
	unsigned long		iblue;
	unsigned long		icube;

	double			rinc;
	double			ginc;
	double			binc;

	unsigned long		rval;
	unsigned long		gval;
	unsigned long		bval;

	_XcbDMStat		rmap;
	_XcbDMStat		gmap;
	_XcbDMStat		bmap;
	_XcbDMStat		*rsort;
	_XcbDMStat		*gsort;
	_XcbDMStat		*bsort;

	XcbColCmpFunc		cmpfunc;
	void			*cmp_data;
	float			max_err;
	int			percent_rgb_err;
	float			rgb_err_sqr;

	NhlBoolean		gray;
	NhlBoolean		rw;

	XcbSortKind		sort_kind;

	XStandardColormap	mycube;
	int			scube;	/* size of "cube" */
	int			nrwcells;
	int			srwcells;
	int			ncsort;	/* number colors currently set */
	int			scsort;
	int			scstat;	/* size of cstat		*/
	_XcbCStat		*cube;	/* pointers into cstat - in rgb order */
	_XcbCStat		*rwcells;
	_XcbCStat		*csort;
	_XcbCStat		cstat;	/* array holding info for each pixel */

	struct{
		int		red;
		int		green;
		int		blue;
	} shifts;

	struct{
		unsigned long	red;
		unsigned long	green;
		unsigned long	blue;
	} masks;

	struct{
		int		red;
		int		green;
		int		blue;
	} bits;

	unsigned long		black;
	unsigned long		white;

	/*
	 * Install to parent.
	 */
	_NhlCB			allocCB;
	_NhlCB			cfaultCB;
	_NhlCB			destroyCB;

	/*
	 * children Col records will use these to find out about changes
	 * in the parent.
	 */
	_NhlCBList		allocCBL;
	_NhlCBList		cfaultCBL;
	_NhlCBList		destroyCBL;
};

#ifndef MAX
#define MAX(a,b)	(((a)>(b))?(a):(b))
#endif
#ifndef MIN
#define MIN(a,b)	(((a)<(b))?(a):(b))
#endif

#define _XcbStAlloc(size,stack)\
		(((size)>sizeof(stack))\
		? Xmalloc((unsigned)(size))\
		: stack)
#define _XcbStFree(ptr,stack)\
		if((ptr)!=((void*)(stack))) XFree(ptr)

#endif	/* XCBP_H */

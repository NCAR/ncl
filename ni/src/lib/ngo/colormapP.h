/*
 *      $Id: colormapP.h,v 1.1 1998-10-19 20:25:54 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1998			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		colormapP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, ColorMapado
 *
 *	Date:		Mon Sep 28 10:34:50 MDT 1998
 *
 *	Description:	
 */
#ifndef	_NG_COLORMAPP_H_
#define	_NG_COLORMAPP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/colormap.h>

#define _NgCM_MAX_CVAL	(65535)

typedef struct _NgColorMapClassRec *NgColorMapClass;
typedef struct _NgColorMapRec *NgColorMap;

typedef enum {
	_NgCM_NOCHANGE = 0,
	_NgCM_EDITED = 1,
	_NgCM_FREED = 2
} _NgColorStatus;

typedef struct _NgColorMapPart {
	/* resource fields	*/
	int			work;
	/* private fields */
	NhlBoolean		focus;
	NhlBoolean		free_colors;
	unsigned long		white;
	unsigned long		black;
	GC			wgc;
	GC			bgc;
	XColor			cdef;
	NhlBoolean		cdef_allocated;
	NhlBoolean		changed;
	int			cmap_len;
	int			edit_cmap_len;
	NhlBoolean		allocated[NhlwkMAX_COLORS];
	_NgColorStatus		cmap_status[NhlwkMAX_COLORS];
	XColor			cmap[NhlwkMAX_COLORS];
	XColor			edit_cmap[NhlwkMAX_COLORS];

	int			num_indx_mapped;
	int			sel_indx;
	Dimension		w_width;
	Dimension		w_height;
	Widget			indx[NhlwkMAX_COLORS];
	Widget			csize;
	Widget			indxt;
	Widget			redt;
	Widget			greent;
	Widget			bluet;
	Widget			elabel;
	Widget			cur_def;
	Widget			rscale;
	Widget			gscale;
	Widget			bscale;
} NgColorMapPart;

typedef struct _NgColorMapRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgColorMapPart	colormap;
} NgColorMapRec;

typedef struct _NgColorMapClassPart {
	int		foo;
} NgColorMapClassPart;

typedef struct _NgColorMapClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgColorMapClassPart	colormap_class;
} NgColorMapClassRec;

extern NgColorMapClassRec	NgcolorMapClassRec;

#endif	/* _NG_COLORMAPP_H_ */

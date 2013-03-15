/*
 *      $Id: WorkstationP.h,v 1.29 2003-11-25 22:41:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		WorkstationP.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 9 10:44:17 MDT 1992
 *
 *	Description:	Private header file for workstation class.
 */
#ifndef _NWorkstationP_h
#define	_NWorkstationP_h

#include	<ncarg/c.h>
#include	<ncarg/hlu/BaseP.h>
#include	<ncarg/hlu/TextItem.h>
#include 	<ncarg/hlu/WorkstationI.h>
#include	<ncarg/hlu/GraphicStyleP.h>
#include        <ncarg/gksP.h>

#define _NhlMAX_COLOR_MAP	256

typedef enum _NhlCStatType{
	_NhlCOLUNSET = 0,
	_NhlCOLSET,
	_NhlCOLREMOVE,
	_NhlCOLNEW,
	_NhlCOLCHANGE
} _NhlCStat;

/*
 * this is currently only used to set the char buffer length for the Fortran
 * and to eliminate the possibility of buffer overflows.
 */

#define	_NhlMAXMARKERLEN	(80)
#define _NhlMAXDASHLEN          (128)

typedef struct _NhlPrivateColor {
	_NhlCStat	cstat;
	int		ci;
	float		red;
	float		green;
	float		blue;
} NhlPrivateColor;

typedef NhlErrorTypes (*NhlWorkstationProc)(
#if	NhlNeedProto
	NhlLayer	wl
#endif
);

typedef	NhlErrorTypes	(*NhlWorkstationAllocColorsProc)(
#if	NhlNeedProto
	NhlWorkstationLayer	wl,
	NhlPrivateColor		*new,
	NhlPrivateColor		*old
#endif
);

typedef NhlErrorTypes (*NhlWorkstationLineTo)(
#if	NhlNeedProto
	NhlLayer	wl,
	float  		x,
	float  		y,	
	int    		upordown
#endif
);

typedef NhlErrorTypes (*NhlWorkstationFill)(
#if	NhlNeedProto
	NhlLayer	wl,
	float  		*x,
	float  		*y,	
	int    		num_points
#endif
);

typedef NhlErrorTypes (*NhlWorkstationMarker)(
#if	NhlNeedProto
	NhlLayer	wl,
	float  		*x,
	float   	*y,	
	int     	num_points
#endif
);

#define _NhlwkLLUActivate   0
#define _NhlwkLLUDeactivate 1
#define _NhlwkLLUClear      2
#define _NhlwkLLUClose      3

typedef void (*NhlWorkstationNotify)(
#if	NhlNeedProto
	NhlLayer	wl,
        int		action
#endif
);

typedef NhlErrorTypes (*NhlWorkstationUpdateDrawBB)(
#if	NhlNeedProto
	NhlLayer	wl,
	NhlBoundingBox *bbox
#endif
);


#define NhlWK_ALLOC_UNIT 16

/*
 * Number of simultaneous Workstation instances allowed. This is one less
 * than the number of possible concurrent GKS workstations since the GKS
 * Segment workstation is opened in the View class initialize method
 * w/o any interaction with the Workstation class interface.
 */

#define MAX_OPEN_WKS (15)

/*
 * This is used as the Inheritance constant
 */
#define NhlInheritCurrentWksCount NULL
#define NhlInheritGksWksRecs NULL
#define NhlInheritHluWksFlag NULL
#define NhlInheritPalette ((int)-1)
#define NhlInheritOpen ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritClose ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritActivate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritDeactivate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritAllocateColors ((NhlWorkstationAllocColorsProc)_NhlInherit)
#define NhlInheritUpdate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritClear ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritLineTo ((NhlWorkstationLineTo)_NhlInherit)
#define NhlInheritFill   ((NhlWorkstationFill)_NhlInherit)
#define NhlInheritMarker ((NhlWorkstationMarker)_NhlInherit)
#define NhlInheritNotify ((NhlWorkstationNotify)_NhlInherit)
#define NhlInheritGetDrawBB ((NhlWorkstationGetDrawBB)_NhlInherit)

typedef struct _NhlDashSpec {
	NhlString dpat;
	NhlBoolean dynamic;
} NhlDashSpec;

typedef NhlDashSpec **NhlDashTable;

typedef struct _NhlFillSpec {
	int angle;
	float spacing;
	int dots_on;
	int *dots_p;
	int glyph;
	int type;
	int ici;
	NhlBoolean dynamic;
} NhlFillSpec;

typedef NhlFillSpec **NhlFillTable;

#define wkDEF_MARKER_ASPECT 1.3125

typedef struct _NhlMarkerSpec {
	char *marker;
	int font;
	float x_off;
	float y_off;
	float aspect_adj;
	float size_adj;
	float angle;
	NhlBoolean dynamic;
} NhlMarkerSpec;

typedef NhlMarkerSpec **NhlMarkerTable;
        
typedef struct _NhlWorkstationLayerPart{
	/* User visible resource fields */

	NhlGenArray	color_map;
	int		color_map_len;
	int		vswidth_dev_units;  /* width of the viewspace in
                                               device units */
	NhlGenArray	bkgnd_color;
        float           bkgnd_opacity;
	NhlGenArray	foregnd_color;
	int		dash_table_len;
	int		fill_table_len;
	int		marker_table_len;
	int		def_graphic_style_id;
	int		*views;
	int		*top_level_views;
        NhlwkAntiAlias  antialias;
        int             curr_antialias_state;

	/* Private resource fields */

	NhlBoolean		reset;
	NhlBoolean		set_public;
	int			graphic_style_id;
	_NhlCB			def_gs_destroy_cb;

	_NhlLineStyleInfo	default_lineinfo;
	_NhlLineStyleInfo	public_lineinfo;
	_NhlLineStyleInfo	private_lineinfo;
	_NhlLineStyleInfo	gs_lineinfo;
	_NhlLineStyleInfo	*lip;

	_NhlMarkerStyleInfo	default_markinfo;
	_NhlMarkerStyleInfo	public_markinfo;
	_NhlMarkerStyleInfo	private_markinfo;
	_NhlMarkerStyleInfo	gs_markinfo;
	_NhlMarkerStyleInfo	*mip;

	_NhlFillStyleInfo	default_fillinfo;
	_NhlFillStyleInfo	private_fillinfo;
	_NhlFillStyleInfo	gs_fillinfo;
	_NhlFillStyleInfo	*fip;

	NhlGenArray	dash_table_strings;

	NhlGenArray	marker_table_strings;
	NhlGenArray	marker_table_params;

	/* Private internal fields */

        int		def_plot_id;
	NhlPrivateColor	private_color_map[_NhlMAX_COLOR_MAP];
	NhlBoolean	cmap_changed;
        NhlBoolean	cleared;

	_NhlCB		color_index_cb;
	_NhlCB		colormap_cb;

	NhlDashTable dash_table;
	int	       dash_table_alloc_len;
	NhlMarkerTable marker_table;
	int	       marker_table_alloc_len;

	/* Export Values */

	int		gkswksid;

	/* Import Values */
	NhlBoolean	open;
	int		gkswkstype;
	int		gkswksconid;
} NhlWorkstationLayerPart;

typedef struct _NhlWorkstationLayerRec{
	NhlBaseLayerPart	base;
	NhlWorkstationLayerPart	work;
} NhlWorkstationLayerRec;

typedef struct _wkGksWksRec 
{
        int gks_id;
        int hlu_id;
        int gks_type;
} wkGksWksRec;

typedef struct _NhlWorkstationClassPart{
        int				*current_wks_count;
	wkGksWksRec			*gks_wks_recs;
        NhlBoolean			*hlu_wks_flag;
	NhlColor			def_background;
	NGDBM				*rgb_dbm;
	int				pal;
	NhlWorkstationProc		open_work;
	NhlWorkstationProc		close_work;
	NhlWorkstationProc		activate_work;
	NhlWorkstationProc		deactivate_work;
	NhlWorkstationAllocColorsProc	alloc_colors;
	NhlWorkstationProc		update_work;
	NhlWorkstationProc		clear_work;
	NhlWorkstationLineTo		lineto_work;
	NhlWorkstationFill		fill_work;
	NhlWorkstationMarker		marker_work;
        NhlWorkstationNotify		notify_work;
        NhlWorkstationUpdateDrawBB	update_drawbb;
} NhlWorkstationClassPart;

typedef struct _NhlWorkstationClassRec{
	NhlBaseClassPart		base_class;
	NhlWorkstationClassPart		work_class;
} NhlWorkstationClassRec;
	
extern NhlWorkstationClassRec NhlworkstationClassRec;	

/*
 * Private API functions for sub-classes.  (In reality these functions
 * probably won't be called by sub-classes, although sub-classes will
 * re-define the actual method function that gets called by these.)
 */

extern	NhlErrorTypes _NhlUpdateGksWksRecs(
#if	NhlNeedProto
	NhlLayer	wl,
        NhlBoolean	add,   /* add workstation if True, delete if False */
        int		*gks_id /* id to use for next open */
#endif
);

extern	NhlErrorTypes _NhlAllocateColors(
#if	NhlNeedProto
	NhlWorkstationLayer	wl
#endif
);

extern NhlGenArray _NhlStringToColorDefStringGenArray(
#if	NhlNeedProto
	NhlWorkstationClass	wc,
	Const char		*str,
	NhlBoolean		doerror
#endif
);

extern NhlBoolean _NhlLookupColor(
#if	NhlNeedProto
	NhlWorkstationClass	wc,
	Const char		*name,
	NGRGB			*rgb
#endif
);

/*
 * Palette obj declarations.  I am putting all the palette declarations
 * in the Workstation files because the palette object is essentially
 * just a worker object for the Workstation class.
 */

typedef struct _NhlPalCmapRec{
	NhlString	name;
	NhlColor	*cmap;
	int		cmap_size;
} _NhlPalCmap;

typedef struct _NhlPalListRec NhlPalListRec, *NhlPalList;
struct _NhlPalListRec{
	NrmQuark	quark;
	NhlGenArray	gen;
	NhlPalList	next;
};

typedef struct _NhlPaletteLayerPart{
	/* User settable resource fields */

	/* Private settable resource fields */
	NhlClass	work_class;

	/* Private internal fields */
	NhlPalList	cmaps;

} NhlPaletteLayerPart;

typedef struct _NhlPaletteLayerRec{
	NhlObjLayerPart		base;
	NhlPaletteLayerPart	pal;
} NhlPaletteLayerRec, *NhlPaletteLayer;

typedef struct _NhlPaletteClassPart{
	_NhlPalCmap		*default_maps;
} NhlPaletteClassPart;

typedef struct _NhlPaletteClassRec{
	NhlObjClassPart			base_class;
	NhlPaletteClassPart		pal_class;
} NhlPaletteClassRec, *NhlPaletteClass;
	
extern NhlPaletteClassRec NhlpaletteClassRec;	

/* Resource Names */

#define	_NhlNpalWorkClass	"pal.WorkClass"
#define	_NhlCpalWorkClass	"Pal.WorkClass"

extern NhlClass NhlpaletteClass;


#endif	/* _NWorkstationP_h */

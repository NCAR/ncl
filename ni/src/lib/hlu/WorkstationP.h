/*
 *      $Id: WorkstationP.h,v 1.7 1995-02-19 08:19:22 boote Exp $
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

#include	<ncarg/hlu/BaseP.h>
#include 	<ncarg/hlu/WorkstationI.h>

#define MAX_COLOR_MAP	256
/*
* Background is a special index in the color map and should either be
* first or last and should only be set ONCE per workstation at create
* time
*/
#define BACKGROUND	0
/*
* All ci fields in the private_color_map field get set to this originally
*/

#define REMOVE		-4
#define UNSET		-3
#define SETALMOST	-2

/*
 * this is currently only used to set the char buffer length for the FortranI
 */
#define	_NhlMAXMARKERLEN	(80)

typedef struct _NhlPrivateColor {
	int ci;
	float red;
	float green;
	float blue;
} NhlPrivateColor;

typedef NhlErrorTypes (*NhlWorkstationProc)(
#if	NhlNeedProto
	NhlLayer	l	/* layer to operate on	*/
#endif
);

typedef NhlErrorTypes (*NhlWorkstationLineTo)(
#if	NhlNeedProto
	NhlLayer	l	/* layer to operate on	*/,
	float   x,
	float   y,	
	int     upordown
#endif
);

typedef NhlErrorTypes (*NhlWorkstationFill)(
#if	NhlNeedProto
	NhlLayer	l	/* layer to operate on	*/,
	float   *x,
	float   *y,	
	int     num_points
#endif
);

typedef NhlErrorTypes (*NhlWorkstationMarker)(
#if	NhlNeedProto
	NhlLayer	l	/* layer to operate on	*/,
	float   *x,
	float   *y,	
	int     num_points
#endif
);

#define NhlWK_ALLOC_UNIT 16

/*
 * This is used as the Inheritance constant
 */
#define NhlInheritOpen ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritClose ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritActivate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritDeactivate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritUpdate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritClear ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritLineTo ((NhlWorkstationLineTo)_NhlInherit)
#define NhlInheritFill   ((NhlWorkstationFill)_NhlInherit)
#define NhlInheritMarker ((NhlWorkstationMarker)_NhlInherit)

typedef struct _NhlWorkLineInfo{
        NhlDashIndex	dash_pattern;
        float		line_dash_seglen;
        NhlColorIndex	line_color;
        float		line_thickness;
        NhlString	line_label;
	NhlFont		line_label_font;
	NhlColorIndex	line_label_color;
        float		line_label_font_height;
} _NhlWorkLineInfo;

typedef struct _NhlMarkerInfo{
	NhlMarkerIndex	marker_index;
	NhlColorIndex	marker_color;
	float		marker_size;
	float		marker_x_off;
	float		marker_y_off;
	float		marker_thickness;
} _NhlMarkerInfo;

typedef struct _NhlWorkstationLayerPart {
	/* User setable resource fields */

	NhlGenArray	color_map;
	int		color_map_len;
	NhlGenArray	bkgnd_color;
	NhlGenArray	foregnd_color;
	int		dash_table_len;
	int		fill_table_len;
	int		marker_table_len;

	/* Private setable resource fields */

	NhlBoolean		reset;
	NhlBoolean		set_public;

	_NhlWorkLineInfo	default_lineinfo;
	_NhlWorkLineInfo	public_lineinfo;
	_NhlWorkLineInfo	private_lineinfo;

	_NhlMarkerInfo		default_markinfo;
	_NhlMarkerInfo		public_markinfo;
	_NhlMarkerInfo		private_markinfo;

/*
 * The Marker Lines are disabled for now...
 */
	int	marker_lines_on;
	int	marker_line_dash_pattern;
	float	marker_line_thickness;
	float	marker_line_dash_seglen;
	int	marker_line_color;

	NhlGenArray	dash_table;

	int	fill_index;
	int	fill_color;
	int	fill_background;
	float	fill_scale_factor;
	float	fill_line_thickness;

	int	edges_on;
	int	edge_dash_pattern;
	float	edge_thickness;
	float	edge_dash_seglen;
	int	edge_color;

	NhlGenArray	marker_table_strings;
	NhlGenArray	marker_table_params;



	/* Private internal fields */

	NhlPrivateColor	private_color_map[MAX_COLOR_MAP];
	int		num_private_colors;

	int edge_char_size;
	int edge_dash_dollar_size;
	int marker_table_alloc_len;
	NhlMarkerSpec	*markers_p;
	int marker_line_char_size;
	int marker_line_dash_dollar_size;

	/* Export Values */

	int		gkswksid;

	/* Import Values */
	int		gkswkstype;
	int		gkswksconid;
} NhlWorkstationLayerPart;

typedef struct _NhlWorkstationLayerRec {
	NhlBaseLayerPart	base;
	NhlWorkstationLayerPart	work;
} NhlWorkstationLayerRec;

typedef struct _NhlWorkstationLayerClassPart {
	NhlWorkstationProc	open_work;
	NhlWorkstationProc	close_work;
	NhlWorkstationProc	activate_work;
	NhlWorkstationProc	deactivate_work;
	NhlWorkstationProc	update_work;
	NhlWorkstationProc	clear_work;
	NhlWorkstationLineTo	lineto_work;
	NhlWorkstationFill      fill_work;
	NhlWorkstationMarker    marker_work;
} NhlWorkstationLayerClassPart;

typedef struct _NhlWorkstationLayerClassRec {
	NhlBaseLayerClassPart		base_class;
	NhlWorkstationLayerClassPart	work_class;
} NhlWorkstationLayerClassRec;
	

typedef struct _NhlWorkstationLayerRec *NhlWorkstationLayer;
typedef struct _NhlWorkstationLayerClassRec *NhlWorkstationLayerClass;

extern NhlWorkstationLayerClassRec NhlworkstationLayerClassRec;	

#endif	/* _NWorkstationP_h */

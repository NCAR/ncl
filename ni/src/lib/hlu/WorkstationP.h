/*
 *      $Id: WorkstationP.h,v 1.3 1993-10-23 00:35:07 dbrown Exp $
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
#include 	<ncarg/hlu/Workstation.h>

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


typedef struct _NhlPrivateColor {
	int ci;
	float red;
	float green;
	float blue;
} NhlPrivateColor;

typedef NhlErrorTypes (*NhlWorkstationProc)(
#if	NhlNeedProto
	Layer	l	/* layer to operate on	*/
#endif
);

typedef NhlErrorTypes (*NhlWorkstationLineTo)(
#if	NhlNeedProto
	Layer	l	/* layer to operate on	*/,
	float   x,
	float   y,	
	int     upordown
#endif
);

typedef NhlErrorTypes (*NhlWorkstationFill)(
#if	NhlNeedProto
	Layer	l	/* layer to operate on	*/,
	float   *x,
	float   *y,	
	int     num_points
#endif
);

typedef NhlErrorTypes (*NhlWorkstationMarker)(
#if	NhlNeedProto
	Layer	l	/* layer to operate on	*/,
	float   *x,
	float   *y,	
	int     num_points
#endif
);

#define NhlWK_ALLOC_UNIT 16

/*
 * This is used as the Inheritance constant
 */
#define NhlInheritUpdate ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritClear ((NhlWorkstationProc)_NhlInherit)
#define NhlInheritFill   ((NhlWorkstationFill)_NhlInherit)
#define NhlInheritMarker ((NhlWorkstationMarker)_NhlInherit)


typedef struct _WorkstationLayerPart {
	/* User setable resource fields */

	NhlGenArray	color_map;
	int		color_map_len;
	NhlGenArray	bkgnd_color;
	NhlGenArray	foregnd_color;
        int dash_pattern;
        char *line_label;
        float line_thickness;
        int line_color;
        float line_label_font_height;
        float line_dash_seglen;
	int	dash_table_len;

	int	fill_table_len;
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

	int		marker_table_len;
	NhlGenArray	marker_table_strings;
	NhlGenArray	marker_table_params;

	char	*marker_string;
	int	marker_index;
	int	marker_color;
	float	marker_size;
	float	marker_x_off;
	float	marker_y_off;
	float	marker_thickness;

	int	marker_lines_on;
	int	marker_line_dash_pattern;
	float	marker_line_thickness;
	float	marker_line_dash_seglen;
	int	marker_line_color;

	/* Private internal fields */

	LayerList	children;
	int		num_children;
	NhlPrivateColor	private_color_map[MAX_COLOR_MAP];
	int		num_private_colors;

        int char_size;
        int dash_dollar_size;
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
} WorkstationLayerPart;

typedef struct _WorkstationLayerRec {
	BaseLayerPart		base;
	WorkstationLayerPart	work;
} WorkstationLayerRec;

typedef struct _WorkstationLayerClassPart {
	NhlWorkstationProc	open_work;
	NhlWorkstationProc	close_work;
	NhlWorkstationProc	activate_work;
	NhlWorkstationProc	deactivate_work;
	NhlWorkstationProc	update_work;
	NhlWorkstationProc	clear_work;
	NhlWorkstationLineTo	lineto_work;
	NhlWorkstationFill      fill_work;
	NhlWorkstationMarker    marker_work;

} WorkstationLayerClassPart;

typedef struct _WorkstationLayerClassRec {
	BaseLayerClassPart		base_class;
	WorkstationLayerClassPart	work_class;
} WorkstationLayerClassRec;
	

extern WorkstationLayerClassRec workstationLayerClassRec;	

extern  NhlErrorTypes _NhlAddWorkChildLayer(
#ifdef NhlNeedProto
        Layer   /* parent */,
        Layer   /* child */
#endif
);

extern  NhlErrorTypes _NhlDeleteWorkChildLayer(
#ifdef NhlNeedProto
        Layer   /* parent */,
        Layer   /* child */
#endif
);



#endif	/* _NWorkstationP_h */


/*
 *      $Id: Workstation.c,v 1.4 1993-11-04 19:51:10 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Workstation.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Sep 9 11:30:43 MDT 1992
 *
 *	Description:	The workstation class can be thought of as a base
 *			for all gks workstation types. It contains methods
 *			for opening, closing, activating and deactivating
 *			the workstation. Since these functions function 
 *			differently than any of the Base class methods. For
 *			one thing all of these methods are "up-chained" meaning
 *			the subclass versions are called before the superclass
 *			versions. This enables subclasses to do something before
 *			the actual open or close occurs. For CGM workstations
 *			this means calling GESC and setting the name of the
 *			metafile. For X workstations it could mean calling 
 *			GESC and setting window ids, geometry and display
 *			information. 
 *			
 *			There are two globally callable private functions
 *				NhlErrorTypes	_NhlAddWorkChildLayer
 *				NhlErrorTypes	_NhlDeleteWorkChildLayer
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/hluutil.h>

/*
* ------------> NEED TO set up default colormap to place in default resource 
*		field <-------------------
*/
static NhlColor def_color[] = {
{1.000000, 1.000000, 1.000000},
{1.000000, 1.000000, 0.968627},
{1.000000, 1.000000, 0.905882},
{1.000000, 1.000000, 0.843137},
{1.000000, 1.000000, 0.780392},
{1.000000, 1.000000, 0.717647},
{1.000000, 1.000000, 0.654902},
{1.000000, 1.000000, 0.592157},
{1.000000, 1.000000, 0.529412},
{1.000000, 1.000000, 0.470588},
{1.000000, 1.000000, 0.407843},
{1.000000, 1.000000, 0.345098},
{1.000000, 1.000000, 0.282353},
{1.000000, 1.000000, 0.219608},
{1.000000, 1.000000, 0.156863},
{1.000000, 1.000000, 0.094118},
{1.000000, 1.000000, 0.031373},
{1.000000, 0.968627, 0.031373},
{1.000000, 0.905882, 0.094118},
{1.000000, 0.843137, 0.156863},
{1.000000, 0.780392, 0.219608},
{1.000000, 0.717647, 0.282353},
{1.000000, 0.654902, 0.345098},
{1.000000, 0.592157, 0.407843},
{1.000000, 0.529412, 0.470588},
{1.000000, 0.470588, 0.529412},
{1.000000, 0.407843, 0.592157},
{1.000000, 0.345098, 0.654902},
{1.000000, 0.282353, 0.717647},
{1.000000, 0.219608, 0.780392},
{1.000000, 0.156863, 0.843137},
{1.000000, 0.094118, 0.905882},
{1.000000, 0.031373, 0.968627},
{1.000000, 0.000000, 0.968627},
{1.000000, 0.000000, 0.905882},
{1.000000, 0.000000, 0.843137},
{1.000000, 0.000000, 0.780392},
{1.000000, 0.000000, 0.717647},
{1.000000, 0.000000, 0.654902},
{1.000000, 0.000000, 0.592157},
{1.000000, 0.000000, 0.529412},
{1.000000, 0.000000, 0.470588},
{1.000000, 0.000000, 0.407843},
{1.000000, 0.000000, 0.345098},
{1.000000, 0.000000, 0.282353},
{1.000000, 0.000000, 0.219608},
{1.000000, 0.000000, 0.156863},
{1.000000, 0.000000, 0.094118},
{1.000000, 0.000000, 0.031373},
{0.968627, 0.031373, 0.031373},
{0.905882, 0.094118, 0.094118},
{0.843137, 0.156863, 0.156863},
{0.780392, 0.219608, 0.219608},
{0.717647, 0.282353, 0.282353},
{0.654902, 0.345098, 0.345098},
{0.592157, 0.407843, 0.407843},
{0.529412, 0.470588, 0.470588},
{0.470588, 0.529412, 0.529412},
{0.407843, 0.592157, 0.592157},
{0.345098, 0.654902, 0.654902},
{0.282353, 0.717647, 0.717647},
{0.219608, 0.780392, 0.780392},
{0.156863, 0.843137, 0.843137},
{0.094118, 0.905882, 0.905882},
{0.031373, 0.968627, 0.968627},
{0.000000, 1.000000, 0.968627},
{0.000000, 1.000000, 0.937255},
{0.000000, 1.000000, 0.874510},
{0.000000, 1.000000, 0.811765},
{0.000000, 1.000000, 0.780392},
{0.000000, 1.000000, 0.717647},
{0.000000, 1.000000, 0.654902},
{0.000000, 1.000000, 0.592157},
{0.000000, 1.000000, 0.529412},
{0.000000, 1.000000, 0.470588},
{0.000000, 1.000000, 0.407843},
{0.000000, 1.000000, 0.345098},
{0.000000, 1.000000, 0.282353},
{0.000000, 1.000000, 0.219608},
{0.000000, 1.000000, 0.156863},
{0.000000, 1.000000, 0.094118},
{0.000000, 1.000000, 0.031373},
{0.000000, 0.968627, 0.031373},
{0.000000, 0.905882, 0.094118},
{0.000000, 0.843137, 0.156863},
{0.000000, 0.780392, 0.219608},
{0.000000, 0.717647, 0.282353},
{0.000000, 0.654902, 0.345098},
{0.000000, 0.592157, 0.407843},
{0.000000, 0.529412, 0.470588},
{0.000000, 0.470588, 0.529412},
{0.000000, 0.407843, 0.592157},
{0.000000, 0.345098, 0.654902},
{0.000000, 0.282353, 0.717647},
{0.000000, 0.219608, 0.780392},
{0.000000, 0.156863, 0.843137},
{0.000000, 0.094118, 0.905882},
{0.000000, 0.031373, 0.968627},
{0.000000, 0.000000, 0.968627},
{0.000000, 0.000000, 0.905882},
{0.000000, 0.000000, 0.843137},
{0.000000, 0.000000, 0.780392},
{0.000000, 0.000000, 0.717647},
{0.000000, 0.000000, 0.654902},
{0.000000, 0.000000, 0.592157},
{0.000000, 0.000000, 0.529412},
{0.000000, 0.000000, 0.470588},
{0.000000, 0.000000, 0.407843},
{0.000000, 0.000000, 0.345098},
{0.000000, 0.000000, 0.282353},
{0.000000, 0.000000, 0.219608},
{0.000000, 0.000000, 0.156863},
{0.000000, 0.000000, 0.094118},
{0.000000, 0.000000, 0.031373}
};

/* 
 * There are currently 15 pre-defined dash patterns provided plus solid
 * (index 0, or NhlSOLIDLINE). The solid line is element 0 of the dash
 * pattern table but is not counted in the number of dash patterns 
 * returned to the user. Therefore the user can use fill pattern indexes 
 * 0 through (and including) NhlNwkDashTableLength as valid indexes.
 */

char *dash_patterns[] = { 
		 "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
                 "$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'$'",
                 "$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'",
                 "$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'",
                 "$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'$$$$'",
                 "$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'",
                 "$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'",
                 "$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'",
                 "$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'",
                 "$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''",
                 "$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''",
                 "$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''",
                 "$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''",
                 "$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''",
                 "$$$$$'$'$$$$$'$'$$$$$'$'$$$$$'$$$$$'$'$$$$$'$'",
                 "$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'$$$$$'$'$'",
                 "$$$$$'''''$$$$$'''''$$$$$'''''$$$$$'''''$$$$$'''''",
};

/* 
 * There are 16 pre-defined fill patterns, plus solid (index 0, 
 * or NhlSOLIDFILL ) and hollow (index -1, or NhlNhlHOLLOWFILL). 
 * Solid is element 0 of the fill specification table, but
 * is not counted in the number of fill patterns returned to the user.
 * Therefore the user can use fill indexes -1 through NhlNwkFillTableLength
 * as valid fill pattern indexes. The fill pattern table is global to
 * all workstations.
 */

static NhlFillSpec fill_specs[] = {

{ 0,   0.0,  0, NULL, 0, 0, 0 },
{ 0,   0.01, 0, NULL, 0, 1, 0 },
{ 90,  0.01, 0, NULL, 0, 1, 0 },
{ 45,  0.01, 0, NULL, 0, 1, 0 },
{ 135, 0.01, 0, NULL, 0, 1, 0 },
{ 0,   0.01, 0, NULL, 0, 2, 0 },
{ 45,  0.01, 0, NULL, 0, 2, 0 },
{ 22,  0.01, 0, NULL, 0, 1, 0 },
{ 68,  0.01, 0, NULL, 0, 1, 0 },
{ 112, 0.01, 0, NULL, 0, 1, 0 },
{ 158, 0.01, 0, NULL, 0, 1, 0 },
{ 22,  0.01, 0, NULL, 0, 2, 0 },
{ 68,  0.01, 0, NULL, 0, 2, 0 },
{ 0,   0.0003125, 0, NULL, 0, -3, 2 },
{ 0,   0.0003125, 0, NULL, 0, -3, 3 },
{ 0,   0.0003125, 0, NULL, 0, -4, 3 },
{ 0,   0.0003125, 0, NULL, 0, -4, 4 }

};

/* Note: the specs for the user-defined marker should be set to the same
 * values as the default marker - currently the asterisk
 * spec-values: marker string, x_off, y_off, aspect ratio (h/w), size factor,
 * dynamic allocation flag.
 */

static NhlMarkerSpec marker_specs[] = {
{"",       0.0, 0.0, 1.3125, 1.0, False},    /* user-defined */
{":F37:Z", 0.0, 0.0, 1.3125, 0.175, False}, /* 1 - dot (small filled circle)*/
{":F18:+", 0.0, 0.075, 1.3125, 0.95, False}, /* 2 - plus sign */
{":F1:*",  0.0, 0.0, 1.3125, 1.0, False},    /* 3 - asterisk */
{":F19:x", 0.0, 0.075, 1.3125, 1.2, False},  /* 4 - hollow circle */
{":F18:U", 0.0, 0.075, 1.3125, 1.1, False},  /* 5 - cross (x) */
{":F19:Z", 0.0, 0.083, 1.3125, 1.45, False}, /* 6 - hollow square */
{":F19:[", 0.0, -0.03, 1.5, 1.25, False},    /* 7 - up pointing triangle */
{":F19:X", 0.0, 0.87, 2.15, 0.67, False},    /* 8 - down pointing triangle */
{":F19:\\", 0.0, 0.075, 1.0, 1.15, False},   /* 9 - diamond */
{":F19:`", 0.0, 0.08, 1.5, 1.55, False}, /* 10-left pointing filled triangle */
{":F19:b", 0.0, 0.08, 1.5, 1.55, False},/* 11-right pointing filled triangle */
{":F19:]", 0.0, 0.0625, 1.3125, 1.1, False}, /* 12 - five-pointed star */
{":F19:m", 0.0, 0.0725, 1.3125, 1.1, False}, /* 13 - six-pointed star */
{":F18:Z", 0.0, 0.0, 1.3125, 0.8, False},    /* 14 - circle with center dot */
{":F37:[", 0.0, 0.0, 1.3125, 0.8, False},    /* 15 - circle with cross */
{":F37:Z", 0.0, 0.0, 1.3125, 0.8, False}     /* 16 - filled circle */

};

/* 
 * The marker table is global to all workstations. It consists of a
 * dynamically allocated array of pointers to NhlMarkerSpec structs.
 * Since the marker table may be reallocated when a marker is added, each
 * workstation is handed not a pointer to the table, but a pointer to
 * the table pointer.
 * The marker table length kept here is the actual length of the table
 * including the 0 element, which does not represent a real marker. The
 * NhlNwkMarkerTableLength read-only resource equals marker_table_len - 1; 
 * Allocation for new markers is in chunks, so the amount currently
 * allocated, and a pointer to the unused elements is also stored.
 */


static NhlMarkerTable marker_table;
static int marker_table_len;
static int marker_table_alloc_len;

/*
 * Likewise the fill table and dash table are global to all workstations
 * Therefore their length is keep statically by the Workstation Class
 */
static int fill_table_len;
static int dash_table_len;

static NrmQuark colormap_name;
static NrmQuark colormaplen_name;
static NrmQuark bkgnd_name;
static NrmQuark foregnd_name;
static NrmQuark	marker_tbl_strings_name;
static NrmQuark marker_tbl_params_name;

#define Oset(field) NhlOffset(WorkstationLayerRec,work.field)
static NhlResource resources[] = {
	{ NhlNwkColorMap, NhlCwkColorMap, NhlTGenArray , sizeof(NhlPointer),
		Oset(color_map), NhlTImmediate, NULL},
	{ NhlNwkColorMapLen, NhlCwkColorMapLen, NhlTInteger, sizeof(int),
		Oset(color_map_len),NhlTImmediate, 
					(NhlPointer)NhlNumber(def_color)},
	{ NhlNwkBackgroundColor, NhlCwkBackgroundColor, NhlTGenArray, 
		  sizeof(NhlPointer),
		  Oset(bkgnd_color), NhlTImmediate, NULL},
	{ NhlNwkForegroundColor, NhlCwkForegroundColor, NhlTGenArray, 
		  sizeof(NhlPointer),
		  Oset(foregnd_color), NhlTImmediate, NULL},
        { NhlNwkDashPattern, NhlCwkDashPattern, NhlTInteger, sizeof(int),
		Oset(dash_pattern),NhlTImmediate,(NhlPointer)0},
        { NhlNwkLineLabel, NhlCwkLineLabel, NhlTString, sizeof(char*),
                Oset(line_label), NhlTImmediate,(NhlPointer)NULL},
        { NhlNwkLineThicknessF, NhlCwkLineThicknessF, NhlTFloat, sizeof(float),
                Oset(line_thickness), NhlTString,"1.0"},
        { NhlNwkLineLabelFontHeightF, NhlCwkLineLabelFontHeightF, NhlTFloat,
                sizeof(float),
                Oset(line_label_font_height), NhlTString,"0.0125" },
        { NhlNwkLineDashSegLenF, NhlCwkLineDashSegLenF,NhlTFloat, 
		  sizeof(float),
		  Oset(line_dash_seglen),NhlTString, ".15" },
        { NhlNwkLineColor, NhlCwkLineColor,NhlTInteger, sizeof(int),
                Oset(line_color),NhlTImmediate,(NhlPointer)1},
	{ NhlNwkDashTableLength, NhlCwkDashTableLength, NhlTInteger, 
		sizeof(int),Oset(dash_table_len),NhlTImmediate,
					(NhlPointer)0},
	{ NhlNwkFillIndex, NhlCwkFillIndex, NhlTInteger, sizeof(int),
		Oset(fill_index), NhlTImmediate,(NhlPointer)0},
	{ NhlNwkFillColor, NhlCwkFillColor, NhlTInteger, sizeof(int),
		Oset(fill_color),NhlTImmediate,(NhlPointer)NhlFOREGROUND},
	{ NhlNwkFillBackground, NhlCwkFillBackground, NhlTInteger, sizeof(int),
		  Oset(fill_background), NhlTImmediate,
		  (NhlPointer)NhlTRANSPARENT},
	{ NhlNwkFillScaleFactorF,NhlCwkFillScaleFactorF,
		  NhlTFloat,sizeof(float),
		Oset(fill_scale_factor),NhlTString,"1.0"},
	{ NhlNwkFillLineThicknessF, NhlCwkFillLineThicknessF, NhlTFloat,
		sizeof(float),Oset(fill_line_thickness),NhlTString,"1.0"},
	{ NhlNwkFillTableLength, NhlCwkFillTableLength, NhlTInteger, 
		sizeof(int),Oset(fill_table_len),NhlTImmediate,
					(NhlPointer)0},
	{ NhlNwkDrawEdges, NhlCwkDrawEdges, NhlTInteger, sizeof(int),
		Oset(edges_on),NhlTString,"0"},
        { NhlNwkEdgeDashPattern, NhlCwkEdgeDashPattern, NhlTInteger,sizeof(int),
		Oset(edge_dash_pattern),NhlTString,"0"},
        { NhlNwkEdgeThicknessF, NhlCwkEdgeThicknessF, NhlTFloat,sizeof(float),
		Oset(edge_thickness),NhlTString,"1.0"},
        { NhlNwkEdgeDashSegLenF, NhlCwkEdgeDashSegLenF,NhlTFloat,sizeof(float),
		Oset(edge_dash_seglen),NhlTString,".15" },
        { NhlNwkEdgeColor, NhlCwkEdgeColor,NhlTInteger, sizeof(int),
		Oset(edge_color),NhlTString,"1"},

	{ NhlNwkMarkerTableLength, NhlCwkMarkerTableLength, NhlTInteger, 
		  sizeof(int),Oset(marker_table_len),NhlTImmediate,
		  (NhlPointer)0},
	{ NhlNwkMarkerTableStrings, NhlCwkMarkerTableStrings, NhlTGenArray,
		  sizeof(NhlPointer),Oset(marker_table_strings),NhlTImmediate,
		  (NhlPointer)NULL},
	{ NhlNwkMarkerTableParams, NhlCwkMarkerTableParams, 
		  NhlTGenArray, sizeof(NhlPointer),
		  Oset(marker_table_params), NhlTImmediate,
		  (NhlPointer)NULL},
	{ NhlNwkMarkerIndex, NhlCwkMarkerIndex, NhlTInteger, sizeof(int),
		  Oset(marker_index), NhlTImmediate,(NhlPointer)3},
        { NhlNwkMarkerString, NhlCwkMarkerString, NhlTString, sizeof(char*),
                Oset(marker_string), NhlTImmediate,(NhlPointer)NULL},
	{ NhlNwkMarkerColor, NhlCwkMarkerColor, NhlTInteger, sizeof(int),
		  Oset(marker_color),NhlTImmediate,(NhlPointer)1},
	{ NhlNwkMarkerSizeF,NhlCwkMarkerSizeF,
		  NhlTFloat,sizeof(float),
		  Oset(marker_size),NhlTString,"0.007"},
	{ NhlNwkMarkerXOffsetF,NhlCwkMarkerXOffsetF,
		  NhlTFloat,sizeof(float),
		  Oset(marker_x_off),NhlTString,"0.0"},
	{ NhlNwkMarkerYOffsetF,NhlCwkMarkerYOffsetF,
		  NhlTFloat,sizeof(float),
		  Oset(marker_y_off),NhlTString,"0.0"},
	{ NhlNwkMarkerThicknessF, NhlCwkMarkerThicknessF, NhlTFloat,
		  sizeof(float),Oset(marker_thickness),NhlTString,"1.0"},
	{ NhlNwkDrawMarkerLines, NhlCwkDrawMarkerLines, 
		  NhlTInteger, sizeof(int),
		  Oset(marker_lines_on),NhlTString,"0"},
        { NhlNwkMarkerLineDashPattern, NhlCwkMarkerLineDashPattern, 
		  NhlTInteger,sizeof(int),
		  Oset(marker_line_dash_pattern),NhlTString,"0"},
        { NhlNwkMarkerLineThicknessF, NhlCwkMarkerLineThicknessF, 
		  NhlTFloat,sizeof(float),
		  Oset(marker_line_thickness),NhlTString,"1.0"},
	{ NhlNwkMarkerLineDashSegLenF, NhlCwkMarkerLineDashSegLenF,
		  NhlTFloat,sizeof(float),
		  Oset(marker_line_dash_seglen),NhlTString,".15" },
        { NhlNwkMarkerLineColor, NhlCwkMarkerLineColor,NhlTInteger, 
		  sizeof(int),
		  Oset(marker_line_color),NhlTString,"1"},
};

/*
* Base class method declarations
*/

static NhlErrorTypes WorkstationClassPartInitialize(
#if	NhlNeedProto
	LayerClass	layerclass	/* layerclass to init	*/
#endif
);

static NhlErrorTypes WorkstationInitialize(
#ifdef NhlNeedProto
        LayerClass,	/* class */
        Layer,		/* req */
        Layer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args */
#endif
);

static NhlErrorTypes WorkstationClassInitialize();


static NhlErrorTypes WorkstationDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes    WorkstationSetValues(
#ifdef NhlNeedProto
        Layer,		/* old */
        Layer,		/* reference */
        Layer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes 	WorkstationGetValues(
#ifdef NhlNeedProto
	Layer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

/*
* WorkStation class method declarations
*/


static NhlErrorTypes WorkstationOpen(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationClose(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationActivate(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationDeactivate(
#ifdef NhlNeedProto
	Layer	/* instance */
#endif
);

static NhlErrorTypes WorkstationUpdate(
#if	NhlNeedProto
	Layer	l	/* instance	*/
#endif
);

static NhlErrorTypes WorkstationClear(
#if	NhlNeedProto
	Layer	l	/* instance	*/
#endif
);

static NhlErrorTypes WorkstationLineTo(
#if 	NhlNeedProto
	Layer	l,
	float	x,
	float 	y,
	int	upordown
#endif
);

static NhlErrorTypes WorkstationFill(
#if 	NhlNeedProto
	Layer	l,
	float	*x,
	float 	*y,
	int	num_points
#endif
);

static NhlErrorTypes WorkstationMarker(
#if 	NhlNeedProto
	Layer	l,
	float	*x,
	float 	*y,
	int	num_points
#endif
);

/*
* Private functions
*/
static NhlErrorTypes AllocateColors(
#ifdef NhlNeedProto
Layer	/* instance */
#endif
); 

static NhlErrorTypes DeallocateColors(
#ifdef NhlNeedProto
Layer	/* instance */
#endif
); 



/*
* Default color map
*/


WorkstationLayerClassRec workstationLayerClassRec = {
        {
/* class_name			*/	"Workstation",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(WorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&baseLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	WorkstationClassPartInitialize,
/* class_initialize		*/	WorkstationClassInitialize,
/* layer_initialize		*/	WorkstationInitialize,
/* layer_set_values		*/	WorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	WorkstationGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	WorkstationDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
        },
	{
/* open_work		*/	WorkstationOpen,
/* close_work		*/	WorkstationClose,
/* activate_work	*/	WorkstationActivate,
/* deactivate_work	*/	WorkstationDeactivate,
/* update_work		*/	WorkstationUpdate,
/* clear_work		*/	WorkstationClear,
/* lineto_work 		*/	WorkstationLineTo,
/* fill_work		*/	WorkstationFill,
/* marker_work		*/	WorkstationMarker
	}
};

LayerClass workstationLayerClass = (LayerClass)&workstationLayerClassRec;

/*
 * Function:	WorkstationClassInitialize
 *
 * Description:	Just needed to call StringToQuark for the color types and
 *		then check to see if GKS is open. If its not then GKS is opened
 *		here.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationClassInitialize()
{
	Gop_st status;
	int status1,dummy = 6;
	int i;

	(void)NrmStringToQuark(NhlTColorPtr);
	(void)NrmStringToQuark(NhlTColor);
	colormap_name = NrmStringToQuark(NhlNwkColorMap);
	colormaplen_name = NrmStringToQuark(NhlNwkColorMapLen);
	bkgnd_name = NrmStringToQuark(NhlNwkBackgroundColor);
	foregnd_name = NrmStringToQuark(NhlNwkForegroundColor);
	marker_tbl_strings_name = NrmStringToQuark(NhlNwkMarkerTableStrings);
	marker_tbl_params_name = NrmStringToQuark(NhlNwkMarkerTableParams);

	ginq_op_st(&status);

	if(status == GST_GKCL) {
/*
* Going to want to change what the logical unit errors will go out to
* which is the first parameter of the gopks call.
*/
		status1 = 0;
/* FORTRAN */ _NHLCALLF(gopks,GOPKS)(&dummy,&status1);
	}
	
/*
 * The static variables that hold the lengths of the marker, fill and
 * dash tables are the actual length of the arrays. In each case the
 * length of the table returned to the user is one less than the actual
 * array length. Solid fill and solid lines are not included as fill
 * or dash patterns, and the marker table includes a 0 dummy element.
 */

	marker_table_len = sizeof(marker_specs)/sizeof(NhlMarkerSpec);
	marker_table_alloc_len = marker_table_len;
	fill_table_len = sizeof(fill_specs)/sizeof(NhlFillSpec); 
	dash_table_len = sizeof(dash_patterns)/sizeof(char *);
	
/*
 * Allocate the marker table
 */
	marker_table = (NhlMarkerTable) NhlMalloc(marker_table_len * 
						  sizeof(NhlMarkerSpec *));
	if (marker_table == NULL) {
		NhlPError(FATAL,E_UNKNOWN,
			  "WorkstationClassInitialize: NhlMalloc failed");
		return FATAL;
	}
	for (i = 0; i < marker_table_len; i++) {
		marker_table[i] = &marker_specs[i];
	}
	
	return(NOERROR);
}

/*
 * Function:	WorkstationClassPartInitialize
 *
 * Description:	This function initializes the workstationclass part of
 *		this class and any subclasses of this class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationClassPartInitialize
#if	__STDC__
(
	LayerClass	layerclass	/* layerclass to init	*/
)
#else
(layerclass)
	LayerClass	layerclass;	/* layerclass to init	*/
#endif
{
	WorkstationLayerClass	lc = (WorkstationLayerClass)layerclass;
	WorkstationLayerClass	sc = (WorkstationLayerClass)
						lc->base_class.superclass;

	if(lc->work_class.update_work == NhlInheritUpdate){
		lc->work_class.update_work = sc->work_class.update_work;
	}

	if(lc->work_class.clear_work == NhlInheritClear){
		lc->work_class.clear_work = sc->work_class.clear_work;
	}

	if(lc->work_class.fill_work == NhlInheritFill){
		lc->work_class.fill_work = sc->work_class.fill_work;
	}

	if(lc->work_class.marker_work == NhlInheritMarker){
		lc->work_class.marker_work = sc->work_class.marker_work;
	}

	return NOERROR;
}

/*
 * Function:	WorkstationInitialize
 *
 * Description:	 DOES NOT OPEN THE WORKSTATION! Simply converts any colormap
 *		info into an internal form. The workstation will be opened by
 *		Create function after all of the initializations have occured.
 *
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes WorkstationInitialize
#if  __STDC__
( LayerClass class,  Layer req, Layer new, _NhlArgList args , int num_args  )
#else
( class,  req, new, args , num_args  )
	LayerClass 	class;
	Layer		req;
	Layer		new;
	_NhlArgList		args;
	int		num_args;
#endif
{
	WorkstationLayer newl = (WorkstationLayer) new;
	NhlColor* tcolor = NULL;
	int i;
	NhlErrorTypes retcode = NOERROR, subret;
	NhlGenArray ga;
	char *e_text;
	char *entry_name = "WorkstationInitialize";
	int count[2];
	int len1, len2;
	NhlMarkerTableParams *mparams;
	NhlString *mstrings;

/* 
 * In case someone tries to set the value of the read-only fill table size
 * the actual size is maintained in a private variable. If the resource
 * has been set, issue a warning, then replace with the real value.
 */
	if (_NhlArgIsSet(args,num_args,NhlNwkFillTableLength)) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
	}
	newl->work.fill_table_len = fill_table_len - 1;

/*
 * Same for marker table (note marker table element 0 is not reported
 * to the user)
 */

	if (_NhlArgIsSet(args,num_args,NhlNwkMarkerTableLength)) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
	}
	newl->work.marker_table_len = marker_table_len - 1;
/*
 * Since the marker specs are stored privately it is only necessary to
 * create a template GenArray initially. The data is not copied. 
 * If either of the
 * marker table resources (marker strings or marker params) is set, 
 * the default marker table is modifed to fit the largest of the 
 * two resources. If one resource is smaller than the other, or NULL, then
 * default values are supplied for each missing item.
 */

	len1 = 0;
	mparams = NULL;
	count[0] = 0;
	count[1] = 4;
	if ((ga = NhlCreateGenArray((NhlPointer)mparams,NhlTFloat,
				    sizeof(float),2,count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNwkMarkerTableParams);
		return FATAL;
	}
	ga->my_data = False;

	if (newl->work.marker_table_params != NULL) {		
		
		subret = _NhlValidatedGenArrayCopy(&ga,
						newl->work.marker_table_params,
						   4*8096,False,False,
						   NhlNwkMarkerTableParams, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		if (subret > WARNING) {
			len1 = 
			   newl->work.marker_table_params->len_dimensions[0];
		}
	}
	newl->work.marker_table_params = ga;
	mparams = (NhlMarkerTableParams *)newl->work.marker_table_params->data;

	len2 = 0;
	mstrings = NULL;
	count[0] = 0;
	if ((ga = NhlCreateGenArray((NhlPointer)mstrings,NhlTString,
				    sizeof(NhlString),1,count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNwkMarkerTableParams);
		return FATAL;
	}
	ga->my_data = False;

	if (newl->work.marker_table_strings != NULL) {		
		subret = _NhlValidatedGenArrayCopy(&ga,
					  newl->work.marker_table_strings,
						   4*8096,False,False,
						   NhlNwkMarkerTableStrings, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		if (subret > WARNING) {
			len2 = 
			    newl->work.marker_table_strings->len_dimensions[0];
		}
	}
	newl->work.marker_table_strings = ga;
	mstrings = (NhlString *) newl->work.marker_table_strings->data;

	for (i=0; i<MAX(len1,len2); i++) {
		float x, y, asp, size;
		char *mstr;
		if (i >= len1)
			x = y = asp = size = -100.0; 
		else {
			x = mparams[i][0];
			y = mparams[i][1];
			asp = mparams[i][2];
			size = mparams[i][3];
		}
		if (i >= len2)
			mstr = NULL;
		else
			mstr = mstrings[i];

		if (i < newl->work.marker_table_len) {
			subret = NhlSetMarker(new,i+1,mstr,x,y,asp,size);
			if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		}
		else {
			subret = NhlNewMarker(new,mstr,x,y,asp,size);
			if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		}
	}
	
/*
 * Same for dash pattern table
 */
	if (_NhlArgIsSet(args,num_args,NhlNwkDashTableLength)) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
	}
	newl->work.dash_table_len = dash_table_len - 1;

	newl->work.gkswksid = (int)FATAL;
	newl->work.gkswkstype = (int)FATAL;
	newl->work.gkswksconid = (int)FATAL;
	newl->work.children = NULL;
	newl->work.num_children = 0;

	for(i = 0; i < MAX_COLOR_MAP; i++) {
		newl->work.private_color_map[i].ci = UNSET;
		newl->work.private_color_map[i].red= 0.0;
		newl->work.private_color_map[i].green= 0.0;
		newl->work.private_color_map[i].blue= 0.0;
	}

/*
 * Process the background resource: default to black. 
 * The background color can only be set at create time.
 */
	if ((tcolor = (NhlColor*)NhlMalloc(sizeof(NhlColor))) == NULL) {
		e_text = "%s: error creating %s array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
		return FATAL;
	}
	for (i=0; i<3; i++)
		(*tcolor)[i] = 0.0;
	count[0] = 3;
	if ((ga = NhlCreateGenArray((NhlPointer)tcolor,NhlTFloat,
				    sizeof(float),1,count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
		return FATAL;
	}
	ga->my_data = True;

	if (newl->work.bkgnd_color != NULL) {
		subret = _NhlValidatedGenArrayCopy(&ga,newl->work.bkgnd_color,
						   3,True,False,
						   NhlNwkBackgroundColor, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;

	}
	newl->work.bkgnd_color = ga;

	tcolor = (NhlColor *) newl->work.bkgnd_color->data;
	for (i=0; i<3; i++) {
		if ((*tcolor)[i] < 0.0 || (*tcolor)[i] > 1.0) {
			int j;
			e_text =
			   "%s: %s holds an invalid color value: defaulting";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNwkBackgroundColor);
			retcode = MIN(retcode, WARNING);
			for (j=0; j<3; j++)
				(*tcolor)[i] = 0.0;
			break;
		}
	}
	newl->work.private_color_map[NhlBACKGROUND].ci = 0;
	newl->work.private_color_map[NhlBACKGROUND].red =  (*tcolor)[0];
	newl->work.private_color_map[NhlBACKGROUND].green = (*tcolor)[1];
	newl->work.private_color_map[NhlBACKGROUND].blue = (*tcolor)[2];

/* 
 * If the colormap generic array resource is not set, create one using
 * the static color entries defined in this module. Note that since the
 * values are stored in a private map, it is not necessary to keep a
 * copy of the actual colormap resource data. The color map length is
 * a read-only parameter. When the user supplies a colormap, the length
 * of the map is determined from the size of the supplied GenArray. Note
 * that background color is not counted in the number of colors in the
 * colormap.
 */
	newl->work.num_private_colors = NhlNumber(def_color) + 1;

	if (_NhlArgIsSet(args,num_args,NhlNwkColorMapLen)) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
	}
	newl->work.color_map_len = newl->work.num_private_colors - 1;

/*
 * Create the gen_array initially using the static default color map data.
 * Note the my_data field is set False. (it's the default, but just to
 * be certain...)
 */
	count[0] = newl->work.color_map_len;
	count[1] = 3;
	if ((ga = NhlCreateGenArray((NhlPointer)def_color,NhlTFloat,
			       sizeof(float),2,count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNwkColorMap);
		return FATAL;
	}
	ga->my_data = False;

	if (newl->work.color_map != NULL) {		
		subret = _NhlValidatedGenArrayCopy(&ga,newl->work.color_map,
						   3*MAX_COLOR_MAP,False,False,
						   NhlNwkColorMap, entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		if (subret > WARNING) {
			newl->work.color_map_len = 
				newl->work.color_map->len_dimensions[0];
		}
	}
	newl->work.color_map = ga;
	newl->work.num_private_colors = newl->work.color_map_len + 1;

/*
 * SETALMOST is changed to a GKS color index when the workstation is opened
 * for now the ci will be the same as the array index but this may change
 * and hence the need for the ci field. Should values be checked?
 */
	tcolor = newl->work.color_map->data;
	for (i=1; i<newl->work.num_private_colors; i++)  {
		newl->work.private_color_map[i].ci = SETALMOST;
		newl->work.private_color_map[i].red = tcolor[i-1][0];
		newl->work.private_color_map[i].green = tcolor[i-1][1];
		newl->work.private_color_map[i].blue =  tcolor[i-1][2];
	}

/*
 * Process the foreground color resource: the foreground color is always
 * color index #1. It is set automatically when the colormap is loaded
 * but an explicitly set foreground color overrides the value loaded using
 * the colormap resource.
 */

	tcolor = NULL;
	count[0] = 0;
	if ((ga = NhlCreateGenArray((NhlPointer)tcolor,NhlTFloat,
				    sizeof(float),1,count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
			  NhlNwkForegroundColor);
		return FATAL;
	}
	ga->my_data = False;

	if (newl->work.foregnd_color != NULL) {
		subret = _NhlValidatedGenArrayCopy(&ga,
						   newl->work.foregnd_color,
						   3,True,False,
						   NhlNwkForegroundColor, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		
		if (subret > WARNING) {
			tcolor = (NhlColor *) ga->data;
			for (i=0; i<3; i++) {
				if ((*tcolor)[i] < 0.0 || (*tcolor)[i] > 1.0) {
					e_text =
			    "%s: %s holds an invalid color value: not set";
					NhlPError(WARNING,E_UNKNOWN,
						  e_text,entry_name,
						  NhlNwkForegroundColor);
					subret = WARNING;
					retcode = MIN(retcode, subret);
					break;
				}
			}
		}
		if (subret > WARNING) {
			newl->work.private_color_map[NhlFOREGROUND].ci = 
				SETALMOST;
			newl->work.private_color_map[NhlFOREGROUND].red = 
				(*tcolor)[0];
			newl->work.private_color_map[NhlFOREGROUND].green =
				(*tcolor)[1];
			newl->work.private_color_map[NhlFOREGROUND].blue =
				(*tcolor)[2];
		}

	}
	newl->work.foregnd_color = ga;

/*
 * Set the line label resource
 */
        if(newl->work.line_label != NULL) {
		char *tmp;
                tmp = (char*)
			NhlMalloc((unsigned)strlen(newl->work.line_label)+1);
                strcpy(tmp,newl->work.line_label);
                newl->work.line_label = tmp;
        }

	return(retcode);
}


/*
 * Function:	WorkstationDestroy
 *
 * Description:	DOES NOT CLOSE WORKSTATION! Simply frees dynamically allocated
 *		storage. The workstation has been closed by the destroy 
 *		function closes the workstation before calling this destroy 
 *		method.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationDestroy
#if	__STDC__
( Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	WorkstationLayer	winst = (WorkstationLayer) inst;
	LayerList	step,tmp;
	NhlErrorTypes	retcode = NOERROR;

	NhlFreeGenArray(winst->work.bkgnd_color);
	NhlFreeGenArray(winst->work.foregnd_color);
	NhlFreeGenArray(winst->work.color_map);
	NhlFreeGenArray(winst->work.marker_table_strings);
	NhlFreeGenArray(winst->work.marker_table_params);

/*
* Questionable whether the destruction of a workstation should result in the
* destruction of all of the children. Here only the LayerList if freed
*/
	step = winst->work.children;
	while(step != NULL) {
		tmp = step->next;
		NhlFree(step);
		step = tmp;
	}
	return(retcode);
}

/*
 * Function:	WorkstationSetValues
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes    WorkstationSetValues
#if  __STDC__
( Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        Layer  old; 
        Layer  reference; 
        Layer  new;
        _NhlArgList args;
        int num_args;
#endif
{
	WorkstationLayer	newl = (WorkstationLayer) new;
	int i;
	WorkstationLayer	oldl = (WorkstationLayer) old;
	NhlErrorTypes	retcode = NOERROR,subret = NOERROR;
	char *tmp;
	int count;
	char *entry_name = "WorkstationSetValues";
	NhlColor *tcolor;
	int len1,len2;
	NhlMarkerTableParams *mparams;
	NhlString *mstrings;
	char *e_text;

/*
 * Check to ensure that no one has messed with the read only fill table
 * size.
 */
	if (newl->work.fill_table_len != fill_table_len - 1) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
		newl->work.fill_table_len = fill_table_len - 1;
	}

/*
 * Check to ensure that no one has messed with the read only marker table
 * size.
 */
	if (newl->work.marker_table_len != marker_table_len - 1) { 
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
		newl->work.marker_table_len = 
			marker_table_len - 1;
	}

	len1 = 0;
	if (newl->work.marker_table_params != oldl->work.marker_table_params) {
		subret = _NhlValidatedGenArrayCopy(
					   &(oldl->work.marker_table_params),
					   newl->work.marker_table_params,
						   4*8096,False,False,
						   NhlNwkMarkerTableParams, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		if (subret > WARNING) {
			len1 = 
			    newl->work.marker_table_params->len_dimensions[0];
		}
		newl->work.marker_table_params = 
			oldl->work.marker_table_params;
	}
	
	len2 = 0;	
	if (newl->work.marker_table_strings != 
	    oldl->work.marker_table_strings) {
		subret=_NhlValidatedGenArrayCopy(
					   &(oldl->work.marker_table_strings),
					     newl->work.marker_table_strings,
						 4*8096,False,False,
						 NhlNwkMarkerTableStrings, 
						 entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		if (subret > WARNING) {
			len2 = 
			   newl->work.marker_table_strings->len_dimensions[0];
		}
		newl->work.marker_table_strings =
			oldl->work.marker_table_strings;
	}

	mparams = (NhlMarkerTableParams *)newl->work.marker_table_params->data;
	mstrings = (NhlString *) newl->work.marker_table_strings->data;
	for (i=0; i<MAX(len1,len2); i++) {
		float x, y, asp, size;
		char *mstr;
		if (i >= len1)
			x = y = asp = size = -100.0; 
		else {
			x = mparams[i][0];
			y = mparams[i][1];
			asp = mparams[i][2];
			size = mparams[i][3];
		}
		if (i >= len2)
			mstr = NULL;
		else
			mstr = mstrings[i];

		if (i < newl->work.marker_table_len) {
			subret = NhlSetMarker(new,i+1,mstr,x,y,asp,size);
			if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		}
		else {
			subret = NhlNewMarker(new,mstr,x,y,asp,size);
			if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		}
	}
	
/*
 * Likewise for the dash table
 */

	if (newl->work.dash_table_len != dash_table_len - 1) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
		newl->work.dash_table_len = dash_table_len - 1;
	}

/*
 * The background color cannot change once the workstation is initialized
 */
	if(newl->work.bkgnd_color != oldl->work.bkgnd_color ) {
		NhlPError(WARNING,E_UNKNOWN,"Illegal Background color change");
		retcode = MIN(WARNING, retcode);
	}

/*
 * The color map len resource is read only also. 
 */
	if (newl->work.color_map_len != newl->work.num_private_colors - 1) {
		NhlPError(WARNING,E_UNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(WARNING, retcode);
		newl->work.color_map_len = newl->work.num_private_colors - 1;
	}

	if (newl->work.color_map != oldl->work.color_map) {
		count = MIN(MAX_COLOR_MAP,
			    newl->work.color_map->len_dimensions[0]);
		subret = _NhlValidatedGenArrayCopy(&(oldl->work.color_map),
					       newl->work.color_map,
					       3*MAX_COLOR_MAP,False,False,
					       NhlNwkColorMap, entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		newl->work.color_map = oldl->work.color_map;

		if (subret > WARNING) {
			newl->work.color_map_len = count;
			newl->work.num_private_colors = 
				newl->work.color_map_len + 1;
			tcolor = newl->work.color_map->data;
			for (i=1; i<newl->work.num_private_colors; i++)  {
				newl->work.private_color_map[i].ci = SETALMOST;
				newl->work.private_color_map[i].red = 
					tcolor[i-1][0];
				newl->work.private_color_map[i].green = 
					tcolor[i-1][1];
				newl->work.private_color_map[i].blue =  
					tcolor[i-1][2];
			}
		}
	}
	

/*
 * Process the foreground color resource: the foreground color is always
 * color index #1. It is set automatically when the colormap is loaded
 * but an explicitly set foreground color overrides the value loaded using
 * the colormap resource.
 */
	if (newl->work.foregnd_color != oldl->work.foregnd_color) {
		subret = _NhlValidatedGenArrayCopy(&(oldl->work.foregnd_color),
					       newl->work.foregnd_color,
					       3,True,False,
					       NhlNwkForegroundColor, 
					       entry_name);
		
		if ((retcode = MIN(retcode,subret)) < WARNING) 
				return retcode;
		newl->work.foregnd_color = oldl->work.foregnd_color;
		tcolor = (NhlColor *) newl->work.foregnd_color->data;

		if (subret > WARNING) {
			for (i=0; i<3; i++) {
				if ((*tcolor)[i] < 0.0 || (*tcolor)[i] > 1.0) {
					e_text =
			    "%s: %s holds an invalid color value: not set";
					NhlPError(WARNING,E_UNKNOWN,
						  e_text,entry_name,
						  NhlNwkForegroundColor);
					subret = WARNING;
					retcode = MIN(retcode, subret);
					break;
				}
			}
		}
		if (subret > WARNING) {
			newl->work.private_color_map[NhlFOREGROUND].ci = 
				SETALMOST;
			newl->work.private_color_map[NhlFOREGROUND].red = 
				(*tcolor)[0];
			newl->work.private_color_map[NhlFOREGROUND].green =
				(*tcolor)[1];
			newl->work.private_color_map[NhlFOREGROUND].blue =
				(*tcolor)[2];
		}

	}
		
/*
 * Allocate the colors now that both the color table and the foreground have
 * been set.
 */
	subret = AllocateColors((Layer)newl);
	retcode = MIN(retcode,subret);

/*
 * Set the line label
 */

        if( (oldl->work.line_label != newl->work.line_label)) {

                if(oldl->work.line_label != NULL){
                        NhlFree(oldl->work.line_label);
                        oldl->work.line_label = NULL;
                }
                if(newl->work.line_label != NULL) {
                        tmp = (char*)NhlMalloc((unsigned)
					  strlen(newl->work.line_label)+1);
                        strcpy(tmp,newl->work.line_label);
                        newl->work.line_label = tmp;
                }
        }

	return(retcode);
}

/*
 * Function:	WorkstationActivate
 *
 * Description:	WorkstationActivate activates the workdstation associated with
 *		this instance. If the workstation hasn't been initialize, which
 *		is next to impossible since NhlOpenWork is called from create,
 *		an error status is returned. Other wise the workstation is
 *		activated.
 *
 * In Args:	Takes just the instance
 *
 * Out Args:	Changes fields of the instance
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationActivate
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{
	WorkstationLayer  thework = (WorkstationLayer) instance;
	NhlErrorTypes retcode = NOERROR;	

	if(wksisopn(thework->work.gkswksid)) {
		if(!wksisact(thework->work.gkswksid)) {
			gactivate_ws(thework->work.gkswksid);
		} else {
/*
* WORKSTATION IS ALREADY ACTIVE
*/
			NhlPError(INFO,E_UNKNOWN,"WorkstationActivate called on already active workstation");
			retcode = INFO; 
		}
	} else {
/*
* ERROR WORKSTATION IS NOT OPEN INITILIZATION FAILED
*/
		NhlPError(FATAL,E_UNKNOWN, "WorkstationActivate can't activate an unopened workstation");
		retcode = FATAL;
	}
	return(retcode);
}

/*
 * Function:	WorkstationDeactivate
 *
 * Description:	Deactivates workstation. if not open FATAL error if not active
 *		informational message.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationDeactivate
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{
	WorkstationLayer  thework = (WorkstationLayer) instance;
	NhlErrorTypes	retcode = NOERROR;

	if(wksisopn(thework->work.gkswksid)&&wksisact(thework->work.gkswksid)){
		gdeactivate_ws(thework->work.gkswksid);
	} else {
/*
* ERROR WORKSTATION NOT ACTIVE OR NOT INITIALIZED
*/
		NhlPError(WARNING,E_UNKNOWN,"WorkstationDeactivate: workstation not active or not openned");
		retcode = WARNING;
	}

	return(retcode);
}

/*
 * Function:	WorkstationOpen
 *
 * Description: Checks and makes sure GKS is open and then procedes to try
 *		to find an available workstation id. Also WorkstationOpen checks
 *		the workstation type and conection id to see if they are valid
 *		if they are then it procedes to open the workstation. 
 *		Workstation open is part of an "up-chained" method so these
 *		values are set by the subclasses' WorkstationOpen before getting
 *		to this. This is done so escape elements that need to be called,
 *		by different workstation types, before the GOPWK call can be
 *		called.
 *
 * In Args:	Just take the workstation instance.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationOpen
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{	
	WorkstationLayer thework = (WorkstationLayer) instance;
	NhlErrorTypes retcode = NOERROR;
	int i = 2;

	if(thework->work.gkswkstype == FATAL) {
		NhlPError(FATAL,E_UNKNOWN,"Unknown workstation type");
		return(FATAL);
		
	} 
	if(thework->work.gkswksconid == FATAL) {
		NhlPError(FATAL,E_UNKNOWN,"Unknown workstation connection id");
		return(FATAL);
	}
	while(wksisopn(i)) {
		i++;
	}
	thework->work.gkswksid = i;

/* FORTRAN */ _NHLCALLF(gopwk,GOPWK)(&(thework->work.gkswksid),&(thework->work.gkswksconid),&(thework->work.gkswkstype));
	gset_clip_ind(GIND_NO_CLIP);

	retcode = AllocateColors((Layer)thework);

	return(retcode);
	
		
}

/*
 * Function:	WorkstationClose
 *
 * Description:	Called before workstation destroy. This like Open is an "up-
 *		chained method it is intended to allow subclasses to do things
 *		before the actual close. If workstation is not open or is 
 *		currently active an error message is provided.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes WorkstationClose
#if __STDC__
(Layer	instance )
#else
(instance)
	Layer	instance;
#endif
{
	WorkstationLayer	thework = (WorkstationLayer) instance;
	NhlErrorTypes retcode = NOERROR;

	if(wksisact(thework->work.gkswksid)){
		NhlPError(INFO,E_UNKNOWN,"WorkstationClose: workstation must be deactivated before closed, deactivating workstation now");
		_NhlDeactivateWorkstation(instance);
	} 
	if(!wksisopn(thework->work.gkswksid)) {
		NhlPError(INFO,E_UNKNOWN,"WorkstationClose: workstation already closed");
		retcode = INFO;
	} else {
		gclose_ws(thework->work.gkswksid);

	}
	return(retcode);
}

/*
 * Function:	WorkstationUpdate
 *
 * Description:	This function is used to update the workstation
 *
 * In Args:	
 *		Layer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationUpdate
#if	__STDC__
(
	Layer	l	/* workstation layer to update	*/
)
#else
(l)
	Layer	l;	/* workstation layer to update	*/
#endif
{
	gupd_ws(_NhlWorkstationId(l),GFLAG_PERFORM);

	return NOERROR;
}

/*
 * Function:	WorkstationClear
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:	
 *		Layer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationClear
#if	__STDC__
(
	Layer	l	/* workstation layer to update	*/
)
#else
(l)
	Layer	l;	/* workstation layer to update	*/
#endif
{
	gclear_ws(_NhlWorkstationId(l),GFLAG_ALWAYS);

	return NOERROR;
}

/*
 * Function:	NhlSetColor
 *
 * Description:	Convienience function for setting one color at a time.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes	NhlSetColor
#if	__STDC__
(int pid, int ci, float red, float green, float blue)
#else
(pid,ci,red,green,blue)
	int     pid;
	int	ci;
	float	red;
	float	green;
	float	blue;
#endif
{
	return(_NhlSetColor(_NhlGetLayer(pid),ci,red,green,blue));
}
NhlErrorTypes	_NhlSetColor
#if	__STDC__
(Layer inst, int ci, float red, float green, float blue)
#else
(inst,ci,red,green,blue)
	Layer	inst;
	int	ci;
	float	red;
	float	green;
	float	blue;
#endif
{
	WorkstationLayer	thework = (WorkstationLayer)inst;
	
	if(ci > MAX_COLOR_MAP) {
/*
* COLOR INDEX EXCEEDS MAX_COLOR_MAP
*/
		NhlPError(WARNING,E_UNKNOWN,"_NhlSetColor: color index exceeds MAX_COLOR_MAP");
		return(WARNING);
	}

	thework->work.private_color_map[ci - 1].ci = SETALMOST;
	thework->work.private_color_map[ci - 1].red = red;
	thework->work.private_color_map[ci - 1].green = green;
	thework->work.private_color_map[ci - 1].blue = blue;

	return(AllocateColors((Layer)thework));
}

/*
 * Function:	NhlFreeColor
 *
 * Description:	removes a color index from the workstation color map
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes	NhlFreeColor
#if	__STDC__
(int pid, int ci)
#else
(pid ,ci)
	int 	pid;
	int	ci;
#endif
{
	return(_NhlFreeColor(_NhlGetLayer(pid),ci));
}
NhlErrorTypes	_NhlFreeColor
#if	__STDC__
(Layer inst, int ci)
#else
(inst,ci)
	Layer inst;
	int	ci;
#endif
{
	WorkstationLayer	thework = (WorkstationLayer)inst;

	if(ci > MAX_COLOR_MAP) {
		NhlPError(WARNING,E_UNKNOWN,"_NhlFreeColor: color index exceeds MAX_COLOR_MAP");
		return(WARNING);
	}

	thework->work.private_color_map[ci - 1].ci =REMOVE;

	return(DeallocateColors((Layer)thework));
}


/*
 * Function:	AllocateColors
 *
 * Description: Used to allocate colors.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes AllocateColors
#if  __STDC__
(Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	WorkstationLayer thework = (WorkstationLayer) inst;
	Gcolr_rep tmpcolrrep;
	int i;
/*
* Temporary allocation routine until some color management scheme is put in 
* place. In fact this may turn in to a method
*/
	for( i = 0; i < MAX_COLOR_MAP; i++) {
		if(thework->work.private_color_map[i].ci == SETALMOST) {
			tmpcolrrep.rgb.red = 
				thework->work.private_color_map[i].red;
			tmpcolrrep.rgb.green = 
				thework->work.private_color_map[i].green;
			tmpcolrrep.rgb.blue= 
				thework->work.private_color_map[i].blue;
			gset_colr_rep(thework->work.gkswksid,i,&tmpcolrrep);
			thework->work.private_color_map[i].ci = i;
		}
	}
	return(NOERROR);
}
/*
 * Function:	DeallocateColors
 *
 * Description: Used to deallocate colors.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes DeallocateColors
#if  __STDC__
(Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	WorkstationLayer thework = (WorkstationLayer) inst;
	int i;

	for( i = 0; i < MAX_COLOR_MAP; i++) {
		if(thework->work.private_color_map[i].ci == REMOVE) {
			thework->work.private_color_map[i].ci = UNSET;
		}
	}
	return(NOERROR);
}

/*
 * Function:	NhlNewColor
 *
 * Description: Does not require the user to provide a color index and returns
 *		either an error status or a color index(not a GKS color index
 *		though, just an index into the workstatoins color map .
 * 
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int NhlNewColor
#if  __STDC__
(int pid, float red, float green, float blue)
#else
(pid,red,green,blue)
	int pid;
	float red;
	float green;
	float blue;
#endif
{
	return(_NhlNewColor(_NhlGetLayer(pid),red,green,blue));
}

int _NhlNewColor
#if   __STDC__
(Layer inst,float red,float green,float blue)
#else
(inst,red,green,blue)
        Layer   inst;
        float   red;
        float   green;
        float   blue;
#endif
{
	WorkstationLayer  thework = (WorkstationLayer) inst;
	int i = 1;
	NhlErrorTypes retcode = NOERROR;

	while( thework->work.private_color_map[i].ci != UNSET ) {
		i++;
		if(i == MAX_COLOR_MAP) {
/*
* ERROR : no available colors
*/		
			NhlPError(FATAL,E_UNKNOWN,"_NhlNewColor: no available colors");
			return(FATAL);
		}
	}
	thework->work.private_color_map[i].ci = SETALMOST;
	thework->work.private_color_map[i].red = red;
	thework->work.private_color_map[i].green = green;
	thework->work.private_color_map[i].blue = blue;
	retcode = AllocateColors((Layer)thework);

	return((retcode < INFO)? (int)retcode : i+1);
	 
}

/*
 * Function:	WorkstationGetValues
 *
 * Description:	
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects: 
 *	Memory is allocated when the following resources are retrieved:
 *		NhlNwkColorMap
 *		NhlNwkBackgroundColor
 *		NhlNwkMarkerTableStrings
 *		NhlNwkMarkerTableParams
 *	The user is responsible for freeing this memory.
 */

static NhlErrorTypes	WorkstationGetValues
#if __STDC__
(Layer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	Layer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	WorkstationLayer wl = (WorkstationLayer)l;
	int i,j;
	NhlColor* tmp;
	NhlPrivateColor *private;
	NhlGenArray ga;
	int count[2];
	NhlMarkerTableParams *mtp_p;
	char **s_p = NULL;
	char *e_text;
	char *entry_name = "WorkstationGetValues";

	for( i = 0; i< num_args; i++ ) {

		if(args[i].quark == colormap_name) {
			private = wl->work.private_color_map;
			tmp = (NhlColor*)
				NhlMalloc(wl->work.color_map_len
					  *sizeof(NhlColor));
			for(j = 0; j< wl->work.num_private_colors -1; j++) {
				tmp[j][0] = private[j + 1].red;
				tmp[j][1] = private[j + 1].green;
				tmp[j][2] = private[j + 1].blue;
			}
			count[0] = wl->work.color_map_len;
			count[1] = 3;
			if ((ga = NhlCreateGenArray((NhlPointer)tmp,
						    NhlTFloat,sizeof(float),
						    2,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkColorMap);
				return FATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;
		} else if (args[i].quark == bkgnd_name) {

			tmp = (NhlColor*) NhlMalloc(sizeof(NhlColor));
			(*tmp)[0] = 
				wl->work.private_color_map[NhlBACKGROUND].red;
			(*tmp)[1] =
			      wl->work.private_color_map[NhlBACKGROUND].green;
			(*tmp)[2] = 
				wl->work.private_color_map[NhlBACKGROUND].blue;
			count[0] = 3;
			if ((ga = NhlCreateGenArray((NhlPointer)tmp,
						    NhlTFloat,sizeof(float),
						    1,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkBackgroundColor);
				return FATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;

		} else if (args[i].quark == foregnd_name) {

			tmp = (NhlColor*) NhlMalloc(sizeof(NhlColor));
			(*tmp)[0] = 
			      wl->work.private_color_map[NhlFOREGROUND].red;
			(*tmp)[1] =
			    wl->work.private_color_map[NhlFOREGROUND].green;
			(*tmp)[2] = 
			     wl->work.private_color_map[NhlFOREGROUND].blue;
			count[0] = 3;
			if ((ga = NhlCreateGenArray((NhlPointer)tmp,
						    NhlTFloat,sizeof(float),
						    1,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkForegroundColor);
				return FATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;

		} else if (args[i].quark == marker_tbl_strings_name) {
			if ((s_p = (NhlString *) 
			     NhlMalloc(wl->work.marker_table_len *
				       sizeof(NhlString))) == NULL) {
				e_text = "%s: error allocating %s data";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkMarkerTableStrings);
				return FATAL;
			}
			for (j=0; j<wl->work.marker_table_len; j++) {
				if ((s_p[j] = (char *) NhlMalloc(strlen(
				   marker_table[j+1]->marker) + 1)) == NULL) {
				       e_text = "%s: error allocating %s data";
				       NhlPError(FATAL,E_UNKNOWN,e_text,
						 entry_name,
						 NhlNwkMarkerTableStrings);
				       return FATAL;
			        }
				strcpy(s_p[j], marker_table[j+1]->marker);
			}
			count[0] = wl->work.marker_table_len;
			if ((ga = NhlCreateGenArray((NhlPointer)s_p,
						    NhlTString,
						    sizeof(NhlString),
						    1,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkMarkerTableStrings);
				return FATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;

		} else if (args[i].quark == marker_tbl_params_name) {
			if ((mtp_p = (NhlMarkerTableParams *)
			     NhlMalloc(wl->work.marker_table_len *
				    sizeof(NhlMarkerTableParams))) == NULL) {
				e_text = "%s: error allocating %s data";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkMarkerTableParams);
				return FATAL;
			}
			for (j=0; j<wl->work.marker_table_len; j++) {
				mtp_p[j][0] = marker_table[j+1]->x_off;
				mtp_p[j][1] = marker_table[j+1]->y_off;
				mtp_p[j][2] = 
					marker_table[j+1]->aspect_adj;
				mtp_p[j][3] = 
					marker_table[j+1]->size_adj;
			}
			count[0] = wl->work.marker_table_len;
			count[1] = 4;
			if ((ga = NhlCreateGenArray((NhlPointer)mtp_p,
						    NhlTFloat,
						    sizeof(float),
						    2,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNwkMarkerTableParams);
				return FATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;

		}
	}
	return(NOERROR);
}

/*
* EVERYTHING BELOW HERE IS AND SHOULD BE PRIVATE BUT GLOBALLY CALLABLE
* FUNCTIONS AND SHOULD BE DECLARED IN "hluP.h"
*/

/*
 * Function:	_NhlAddWorkChildLayer
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes   _NhlAddWorkChildLayer
#if __STDC__
(Layer parent, Layer child)
#else
(parent,child)
	Layer parent;
	Layer child;
#endif
{
	WorkstationLayer wparent = (WorkstationLayer) parent;
	LayerList	tmp;
	

	if(wparent->work.children == NULL){
		wparent->work.children = (LayerList)
			NhlMalloc(sizeof(LayerListNode));
		wparent->work.children->next = NULL;
		wparent->work.children->layer = child;
		wparent->work.num_children = 1;
	} else {
		tmp = (LayerList) NhlMalloc(sizeof(LayerListNode));
		tmp->next = wparent->work.children;
		tmp->layer = child;
		wparent->work.children = tmp;
		++wparent->work.num_children;
	}
	return(NOERROR);
}

/*
 * Function:	_NhlDeleteWorkChildLayer
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes   _NhlDeleteWorkChildLayer
#if __STDC__
(Layer parent, Layer child)
#else 
(parent,child)
	Layer	parent;
	Layer 	child;
#endif
{
	WorkstationLayer	wparent = (WorkstationLayer) parent;
	LayerList 	step = wparent->work.children;
	LayerList	tmp;

	if(step != NULL ) {
		if(step->layer->base.id == child->base.id) {
			tmp = step;
			wparent->work.children = step->next;
			--wparent->work.num_children;
			NhlFree(step);
		}
		while(step->next != NULL){
			if(step->next->layer->base.id == child->base.id) {
				tmp = step->next;
				step->next = step->next->next; 
				NhlFree(tmp);
				--wparent->work.num_children;
				break;
			}
			step = step->next;
		}
		return(NOERROR);
	} else {
/*
*ERROR: No child in list
*/
		NhlPError(INFO,E_UNKNOWN,"_NhlDeleteWorkChildLayer: Delete requested on empty list");
		return(INFO);
	}
}

/*
 * Function:	_NhlWorkstationId
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int	_NhlWorkstationId
#if __STDC__
(Layer instance)
#else
(instance)
	Layer instance;
#endif
{
	WorkstationLayer wl = (WorkstationLayer) instance;

	return(wl->work.gkswksid);
}

/*
 * Function:	_NhlActivateWorkstation and CallActivateWorkstation
 *
 * Description: _NhlActivateWorkstation checks to see if the layer wks is
 *		a workstation and then calls CallActivate which recursively
 *		calls the activate_work methods.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes CallActivateWorkstation
#if __STDC__
	(Layer layer, LayerClass lc)
#else
	(layer,lc)
		Layer layer;
		LayerClass lc;
#endif
{
	WorkstationLayerClass wc = (WorkstationLayerClass) lc;
	NhlErrorTypes ancestor = NOERROR, thistime = NOERROR;

	if( wc->work_class.activate_work != NULL ) {
		thistime = (*(wc->work_class.activate_work))(layer);
		if( thistime < WARNING)
			return(thistime);
	}

	if( wc->base_class.superclass != baseLayerClass ) { 
		ancestor = CallActivateWorkstation(layer, wc->base_class.superclass);
	}
	return(MIN(ancestor,thistime));
}
NhlErrorTypes _NhlActivateWorkstation
#if __STDC__
(Layer wks)
#else
(wks)
	Layer wks;
#endif
{

	if(_NhlIsWorkstation(wks)) {
		return(CallActivateWorkstation(wks,wks->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform activate on nonworkstation");
		return(WARNING);
	}
}

/*
 * Function:	_NhlDeactivateWorkstation and CallDeativateWorkstation(
 *
 * Description:	performs same task as _NhlWorkstationActivate except
 *		the deactivate_work method is called
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes CallDeactivateWorkstation
#if __STDC__
	(Layer layer, LayerClass lc)
#else
	(layer,lc)
		Layer layer;
		LayerClass lc;
#endif
{
	WorkstationLayerClass wc = (WorkstationLayerClass) lc;
	NhlErrorTypes ancestor = NOERROR, thistime = NOERROR;

	if( wc->work_class.deactivate_work != NULL ) {
		thistime = (*(wc->work_class.deactivate_work))(layer);
		if( thistime < WARNING)
			return(thistime);
	}

	if( wc->base_class.superclass != baseLayerClass ) { 
		ancestor = CallDeactivateWorkstation(layer, wc->base_class.superclass);
	}
	return(MIN(ancestor,thistime));
}
NhlErrorTypes _NhlDeactivateWorkstation
#if __STDC__
(Layer wks)
#else
(wks)
	Layer wks;
#endif
{

	if(_NhlIsWorkstation(wks)) {
		return(CallDeactivateWorkstation(wks,wks->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform deactivate on nonworkstation");
		return(WARNING);
	}
}


/*
 * Function:	_NhlCloseWorkstation and CallCloseWorkstation
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes CallCloseWorkstation
#if __STDC__
(Layer instance,LayerClass lc)
#else
(instance,lc)
        Layer instance;
        LayerClass lc;
#endif
{
        WorkstationLayerClass   wc =(WorkstationLayerClass)lc ;
        NhlErrorTypes ancestorerr = NOERROR, thisclass = NOERROR;

        if(wc->work_class.close_work != NULL) {
                thisclass = (*wc->work_class.close_work)(instance);
                if(thisclass < WARNING)
                        return(thisclass);
        }

        if(lc->base_class.superclass != baseLayerClass)
                ancestorerr = CallCloseWorkstation(instance,lc->base_class.superclass);


        return(MIN(ancestorerr,thisclass));
}

NhlErrorTypes _NhlCloseWorkstation
#if __STDC__
(Layer layer)
#else
(layer)
	Layer	layer;
#endif
{
	if(_NhlIsWorkstation(layer)) {
		return(CallCloseWorkstation(layer,layer->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform close on nonworkstation");
		return(WARNING);
	}
}

/*
 * Function:    _NhlOpenWorkstation and CallOpenWorkstatation
 *
 * Description: Recursively calls open_work methods from the lowest subclass
 *              up.
 *
 * In Args:     New instance: layer
 *
 * Out Args:    NONE
 *
 * Return Values:       NONE
 *
 * Side Effects:        Workstation opened up and a workstation id assigned
 *
 */
static NhlErrorTypes CallOpenWorkstation
#if __STDC__
(Layer instance,LayerClass lc)
#else
(instance,lc)
        Layer instance;
        LayerClass lc;
#endif
{
        WorkstationLayerClass   wc =(WorkstationLayerClass)lc ;
        NhlErrorTypes ancestorerr = NOERROR, thisclass = NOERROR;

        if(wc->work_class.open_work != NULL) {
                thisclass = (*wc->work_class.open_work)(instance);
                if(thisclass < WARNING)
                        return(thisclass);
        }

        if(lc->base_class.superclass != baseLayerClass)
                ancestorerr = CallOpenWorkstation(instance,lc->base_class.superclass);


        return(MIN(ancestorerr,thisclass));
}

NhlErrorTypes _NhlOpenWorkstation
#if __STDC__
(Layer layer)
#else
(layer)
	Layer	layer;
#endif
{
	if(_NhlIsWorkstation(layer)) {
		return(CallOpenWorkstation(layer,layer->base.layer_class));
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlActivateWorkstation: attempt to perform open on nonworkstation");
		return(WARNING);
	}
}

/*
 * Function:	NhlGetWorkId
 *
 * Description:	Returns the GKS workstation id used by the workstation object
 *
 * In Args:	HLU Workstation object id
 *
 * Out Args:	NONE
 *
 * Returns:	integer gks workstation id
 */
int NhlGetGksWorkId
#if  __STDC__
(int workid)
#else
(workid)
int	workid;
#endif
{
	Layer l = _NhlGetLayer(workid);

	if(_NhlIsWorkstation(l)) {
		return(((WorkstationLayer)l)->work.gkswksid);
	} else {
		NhlPError(WARNING,E_UNKNOWN,"NhlGetGksWorkId: An incorrect type of object was passed");
		return(-1);
	}
}

/*
 * Function:	NhlGetGksCi
 *
 * Description: Returns a color index for GKS given a workstation color index.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int NhlGetGksCi
#if __STDC__
(int pid, int ci)
#else
(pid,ci)
	int pid;
	int ci;
#endif
{

 	return(_NhlGetGksCi(_NhlGetLayer(pid),ci));	
}
int _NhlGetGksCi
#if __STDC__
( Layer workstation, int  ci)
#else
(workstation, ci)
	Layer	workstation;
	int	ci;
#endif
{
	
	WorkstationLayer  wk = (WorkstationLayer) workstation;
	if(_NhlIsWorkstation(workstation)){
		if(wk->work.private_color_map[ci+1].ci >= 0) {
			return(wk->work.private_color_map[ci+1].ci);
		} else {
			NhlPError(WARNING,E_UNKNOWN,"_NhlGetGksCi: Color index requested is not allocated");
			return((int)WARNING);
		}
	} else {
		NhlPError(WARNING,E_UNKNOWN,"_NhlGetGksCi: attempt to return color from none workstation");
		return((int)WARNING);
	}
}

/*
 * Function:	NhlUpdateWorkstation
 *
 * Description:	This function is used to flush the workstation.
 *
 * In Args:	
 *		int	workid	id of workstation class object
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlUpdateWorkstation
#if	__STDC__
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	Layer			l = _NhlGetLayer(workid);
	WorkstationLayerClass	lc;

	if(l == (Layer)NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to update Workstation with PID#%d",workid);
		return FATAL;
	}

	if(!_NhlIsWorkstation(l)){
		NhlPError(FATAL,E_UNKNOWN,
			"PID#%d is not a Workstation Class object",workid);
		return FATAL;
	}

	lc = (WorkstationLayerClass)l->base.layer_class;

	return (*(lc->work_class.update_work))(l);
}

/*
 * Function:	NhlClearWorkstation
 *
 * Description:	This function is used to clear the workstation.
 *
 * In Args:	
 *		int	workid	id of workstation class object
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlClearWorkstation
#if	__STDC__
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	Layer			l = _NhlGetLayer(workid);
	WorkstationLayerClass	lc;

	if(l == (Layer)NULL){
		NhlPError(FATAL,E_UNKNOWN,
			"Unable to clear Workstation with PID#%d",workid);
		return FATAL;
	}

	if(!_NhlIsWorkstation(l)){
		NhlPError(FATAL,E_UNKNOWN,
			"PID#%d is not a Workstation Class object",workid);
		return FATAL;
	}

	lc = (WorkstationLayerClass)l->base.layer_class;

	return (*(lc->work_class.clear_work))(l);
}

NhlErrorTypes	NhlFrame
#if	__STDC__
(int wid)
#else
(wid)
	int wid;
#endif
{
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

	ret = NhlUpdateWorkstation(wid);
	ret1 = NhlClearWorkstation(wid);
	return(MIN(ret,ret1));
}

/*ARGSUSED*/
void _NhlSetLineInfo
#if  __STDC__
(Layer instance,Layer plot)
#else
(instance,plot)
        Layer instance;
        Layer plot;
#endif
{
        WorkstationLayer tinst = (WorkstationLayer)instance;
        float   fl,fr,fb,ft,ul,ur,ub,ut;
        float  y0,y1,x0,x1;
        int ll;
        char buffer[80];
	int ix;

	memset((void *) buffer, (char) 0, 80 * sizeof(char));

        c_sflush();

        c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

        y0 = fb;
        y1 = fb + tinst->work.line_label_font_height;
        y0 = (float)c_kfpy(y0);
        y1 = (float)c_kfpy(y1);

        tinst->work.char_size = (int) (y1 - y0);
        if(tinst->work.char_size < 4) {
                tinst->work.char_size = 4;
        }
        x0 = fl;
        x1 = fl + tinst->work.line_dash_seglen;
        x0 = (float)c_kfpy(x0);
        x1 = (float)c_kfpy(x1);

	if ((ix = tinst->work.dash_pattern) < 0) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
			  "_NhlSetLineInfo: invalid dash pattern index");
		ix = tinst->work.dash_pattern = NhlSOLIDLINE;
	}
	else if (ix > tinst->work.dash_table_len) {
		/* INFO - but it's a void function right now */
		NhlPError(INFO,E_UNKNOWN,
	"_NhlSetLineInfo: using mod function on dash pattern index: %d", ix);

		ix = 1 + (ix - 1) % tinst->work.dash_table_len; 
	}

        tinst->work.dash_dollar_size = (int)((x1-x0)/
                strlen(dash_patterns[ix])+.5);
        if(tinst->work.dash_dollar_size < 1)
                        tinst->work.dash_dollar_size = 1;

        strcpy(buffer,dash_patterns[ix]);
        if(tinst->work.line_label != NULL) {
                  strcpy(&(buffer[strlen(dash_patterns[ix])
				  - strlen(tinst->work.line_label)]),
			 tinst->work.line_label);
        }
        gset_line_colr_ind((Gint)_NhlGetGksCi(
			    plot->base.wkptr,tinst->work.line_color));

        gset_linewidth(tinst->work.line_thickness);
        c_dashdc(buffer,tinst->work.dash_dollar_size,tinst->work.char_size);
        return;
}


/*ARGSUSED*/
static NhlErrorTypes WorkstationLineTo
#if  __STDC__
(Layer l,float x,float y,int upordown)
#else
(l,x,y,upordown)
	Layer l;
	float x;
	float y;
	int upordown;
#endif
{
	static float lastx,lasty;
	static first = 0;
	int ix0,iy0,ix1,iy1;

	if(upordown == 1) {
		lastx = x;
		lasty = y;
/* FORTRAN*/    _NHLCALLF(lastd,LASTD)();
		first = 1;
		return(NOERROR);
	} else {
		if(first == 1) {
			ix0 = c_kfmx(lastx);
			iy0 = c_kfmy(lasty);
/* FORTRAN */		_NHLCALLF(cfvld,CFVLD)(&first,&ix0,&iy0);
			first = 2;
		}
		ix1 = c_kfmx(x);
		iy1 = c_kfmy(y);
/* FORTRAN */   _NHLCALLF(cfvld,CFVLD)(&first,&ix1,&iy1);
		lastx = x;
		lasty = y;
		return(NOERROR);
	}
}

NhlErrorTypes CallWorkLineTo
#if  __STDC__
(LayerClass lc, Layer instance,  float x, float y, int upordown)
#else
(lc, instance,  x, y, upordown)
LayerClass lc;
Layer instance;
float x;
float y;
int upordown;
#endif
{
        WorkstationLayerClass tlc = (WorkstationLayerClass)lc;

        if(tlc->work_class.lineto_work == NULL){
                if(tlc->base_class.superclass != NULL) {
                        return(CallWorkLineTo(lc->base_class.superclass,instance,x,y,upordown));
                } else {
                        NhlPError(WARNING,E_UNKNOWN,"_NhlWorkstationLineTo: Transformation object of type (%s) does not have lineto_work function",tlc->base_class.class_name);
                        return(WARNING);
                }
        } else {
                return((*tlc->work_class.lineto_work)(instance,x,y,upordown));
        }
}

NhlErrorTypes _NhlWorkstationLineTo
#if __STDC__
(Layer instance, float x, float y, int upordown)
#else
(instance,x,y,upordown)
Layer instance;
float   x;
float y;
int upordown;
#endif
{
        return(CallWorkLineTo(instance->base.layer_class,instance,x,y,upordown));
}


/*ARGSUSED*/
void _NhlSetFillInfo
#if  __STDC__
(Layer instance,Layer plot)
#else
(instance,plot)
        Layer instance;
        Layer plot;
#endif
{
        WorkstationLayer tinst = (WorkstationLayer)instance;
	WorkstationLayerPart *wk_p = &tinst->work;
        float   fl,fr,fb,ft,ul,ur,ub,ut;
        float  x0,x1;
        int ll,ix;
        char buffer[80];
	

	if (wk_p->edges_on && wk_p->edge_dash_pattern < 0) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
			  "_NhlSetFillInfo: invalid edge dash pattern index");
		wk_p->edge_dash_pattern = NhlSOLIDLINE;
	}
	else if (wk_p->edges_on && wk_p->edge_dash_pattern > 0) {
		memset((void *) buffer, (char) 0, 80 * sizeof(char));

		c_sflush();

		c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

		x0 = fl;
		x1 = fl + wk_p->edge_dash_seglen;
		x0 = (float)c_kfpy(x0);
		x1 = (float)c_kfpy(x1);
	
		if ((ix = wk_p->edge_dash_pattern) > wk_p->dash_table_len) {
			/* INFO - but it's a void function right now */
			NhlPError(INFO,E_UNKNOWN,
	"_NhlSetLineInfo: using mod function on dash pattern index: %d", ix);

			ix = 1 + (ix - 1) % wk_p->dash_table_len;
		}
		
		wk_p->edge_dash_dollar_size = (x1 - x0) /
			strlen(dash_patterns[ix]) + 0.5;
		if(wk_p->edge_dash_dollar_size < 1)
                        wk_p->edge_dash_dollar_size = 1;
		
		strcpy(buffer,dash_patterns[ix]);
		
		c_dashdc(buffer,wk_p->edge_dash_dollar_size,1);
	}
		
/*
 * Make sure the scale factor is okay
 */
	if (wk_p->fill_scale_factor <= 0.0) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
		"_NhlSetFillInfo: fill scale factor must be greater than 0.0");
		wk_p->fill_scale_factor = 1.0;
	}
		
/*
 * An out-of-bounds fill index should have been caught at a higher
 * level. 
 */

	if ((ix = wk_p->fill_index) < NhlHOLLOWFILL) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
			   "_NhlSetFillInfo: invalid fill index");
		wk_p->fill_index = NhlHOLLOWFILL;
		
	}
	else if (ix > wk_p->fill_table_len) {
		/* INFO - but it's a void function right now */
		NhlPError(INFO,E_UNKNOWN,
	 "_NhlSetLineInfo: using mod function on fill index: %d", ix);

		ix = 1 + (ix - 1) % wk_p->fill_table_len;
	}

	if (ix != NhlHOLLOWFILL) {
		c_sfseti("AN", fill_specs[ix].angle);
		c_sfsetr("SP", fill_specs[ix].spacing * 
			 wk_p->fill_scale_factor);
	}

        return;
}


static NhlErrorTypes WorkstationFill
#if  __STDC__
(Layer l,float *x,float *y,int num_points)
#else
(l,x,y,num_points)
	Layer l;
	float *x;
	float *y;
	int num_points;
#endif
{
        WorkstationLayer inst = (WorkstationLayer)l;
	WorkstationLayerPart *wk_p = &inst->work;
	static int first = 1;
	static float *dst;
	static int *ind;
	static int msize;
	static int nst, nnd;
        float   fl,fr,fb,ft,ul,ur,ub,ut;
	int ll, ix;
	Gfill_int_style save_fillstyle;
	Gint save_linecolor;
	Gint save_linetype;
	Gdouble save_linewidth;
	Gint err_ind;
	Gint fill_color;
	Gint fill_background;
	
	/* 
 * Create or enlarge the workspace arrays as required
 */

	if (first) {
		msize = MAX(num_points,NhlWK_INITIAL_FILL_BUFSIZE);
		nst = 2 * msize;
		nnd = 3 * msize;
		dst = (float *)NhlMalloc(nst * sizeof(float));
		ind = (int *)NhlMalloc(nnd * sizeof(int));
		first = 0;
		if (dst == NULL || ind == NULL) {
			NhlPError(FATAL,E_UNKNOWN,
			   "WorkstationFill: workspace allocation failed");
			return(FATAL);
		}
	}
	else if (msize < num_points) {
		msize = num_points;
		nst = 2 * msize;
		nnd = 3 * msize;
		dst = (float *)NhlRealloc(dst, nst * sizeof(float));
		ind = (int *)NhlRealloc(ind, nnd * sizeof(int));
		if (dst == NULL || ind == NULL) {
			NhlPError(FATAL,E_UNKNOWN,
			    "WorkstationFill: workspace allocation failed");
			return(FATAL);
		}
	}

/*
 * Make the user space coincide with the NDC space for the
 * duration of the routine
 */
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	c_set(fl,fr,fb,ft,fl,fr,fb,ft,1);
/*
 * Save attributes that may be modified
 */
	ginq_line_colr_ind(&err_ind, &save_linecolor);
	ginq_linewidth(&err_ind, &save_linewidth);
	ginq_fill_int_style(&err_ind, &save_fillstyle);
	ginq_linetype(&err_ind, &save_linetype);
	fill_color = _NhlGetGksCi(inst->base.wkptr, wk_p->fill_color);
	fill_background = (wk_p->fill_background < 0) ?
		wk_p->fill_background :
			_NhlGetGksCi(inst->base.wkptr, wk_p->fill_background);

/*
 * Draw the fill, unless a negative fill index is specified
 * (implying no fill)
 */
	if ((ix = wk_p->fill_index) == NhlSOLIDFILL) {
		/* fill_specs[ix].type  must be 0 */
		gset_fill_int_style(1);
		gset_linewidth(wk_p->fill_line_thickness);
		c_sfseti("type of fill", 0);
		c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,fill_color);
	}
	else if (ix > 0) {
		/* fill_specs[ix].type must not be 0 */
		ix = 1 + (ix - 1) % wk_p->fill_table_len;
		if (fill_background >= 0) {
			gset_linewidth(1.0);
			gset_fill_int_style(1);
			c_sfseti("type of fill", 0);
			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,
				 fill_background);
		}
		gset_linewidth(wk_p->fill_line_thickness);
		c_sfseti("TY", fill_specs[ix].type);
		if (fill_specs[ix].type > 0) { 
 			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,fill_color);
		}
		else {
			gset_line_colr_ind(fill_color);
 			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,
				 fill_specs[ix].ici);
		}
	}

/*
 * Draw the edges
 */
	if (wk_p->edges_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(inst->base.wkptr,
						      wk_p->edge_color));
		
		gset_linewidth(wk_p->edge_thickness);
		if (wk_p->edge_dash_pattern > 0) {
			c_curved(x,y,num_points);
		}
		else {
			c_curve(x,y,num_points);
		}
	}

/*
 * Restore state
 */

	gset_line_colr_ind(save_linecolor);
	gset_linewidth(save_linewidth);
	gset_fill_int_style(save_fillstyle);
	gset_linetype(save_linetype);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	return(NOERROR);

}

NhlErrorTypes CallWorkstationFill
#if  __STDC__
(LayerClass lc, Layer instance,  float *x, float *y, int num_points)
#else
(lc, instance,  x, y, num_points)
LayerClass lc;
Layer instance;
float *x;
float *y;
int num_points;
#endif
{
        WorkstationLayerClass tlc = (WorkstationLayerClass)lc;

        if(tlc->work_class.fill_work == NULL){
                if(tlc->base_class.superclass != NULL) {
                        return(CallWorkstationFill(lc->base_class.superclass,
						   instance,x,y,num_points));
                } else {
                        NhlPError(WARNING,E_UNKNOWN,"_NhlWorkstationFill: Transformation object of type (%s) does not have fill_work function",
				  tlc->base_class.class_name);
                        return(WARNING);
                }
        } else {
                return((*tlc->work_class.fill_work)(instance,x,y,num_points));
        }
}

NhlErrorTypes _NhlWorkstationFill
#if __STDC__
(Layer instance, float *x, float *y, int num_points)
#else
(instance,x,y,num_points)
Layer instance;
float   *x;
float *y;
int num_points;
#endif
{
        return(CallWorkstationFill(instance->base.layer_class,
				   instance,x,y,num_points));
}


/*
 * Adds a marker definition to the marker table and returns an index to
 * this marker. 
 */
/*ARGSUSED*/
int NhlNewMarker
#if  __STDC__
(Layer instance, 
 char *marker_string, 
 float x_off, 
 float y_off,
 float aspect_adj,
 float size_adj)
#else
(instance,marker_string,x_off,y_off,aspect_adj,size_adj)
        Layer instance;
	char *marker_string; 
	float x_off; 
	float y_off;
	float aspect_adj;
	float size_adj;
#endif
{
        WorkstationLayer tinst = (WorkstationLayer)instance;
	WorkstationLayerPart *wk_p = &tinst->work;
	NhlMarkerSpec *m_p;
	int i;

	if (marker_table_len == marker_table_alloc_len) {
		marker_table_alloc_len += NhlWK_ALLOC_UNIT;
		marker_table = (NhlMarkerTable) 
			NhlRealloc(marker_table, 
				   marker_table_alloc_len *
				   sizeof(NhlMarkerSpec *));
		if (marker_table == NULL) {
			NhlPError(FATAL,E_UNKNOWN,
			     "_NhlNewMarker: marker table realloc failed");
			return((int)FATAL);
		}
		m_p = (NhlMarkerSpec *)
			NhlMalloc(NhlWK_ALLOC_UNIT * sizeof(NhlMarkerSpec));
		if (m_p == NULL) {
			NhlPError(FATAL,E_UNKNOWN,
			     "_NhlNewMarker: marker specs alloc failed");
			return((int)FATAL);
		}
		for (i=marker_table_len; i<marker_table_alloc_len; i++) {
			marker_table[i] = &m_p[i - marker_table_len];
		}
	}
/*
 * If the marker string is NULL use the default marker string
 */
		
	marker_table_len += 1;
	wk_p->marker_table_len = marker_table_len - 1;
	m_p = marker_table[marker_table_len - 1];

	if (marker_string == NULL) {
		marker_string = marker_table[NhlWK_DEF_MARKER]->marker;
        }
	if ((m_p->marker = NhlMalloc(strlen(marker_string) + 1)) == NULL) {
		NhlPError(FATAL,E_UNKNOWN,
			  "_NhlNewMarker: marker string alloc failed");
		return((int)FATAL);
	}
	strcpy(m_p->marker, marker_string);
	
	if (x_off < 1.0 && x_off > -1.0)  
		m_p->x_off = x_off;
	else
		m_p->x_off = marker_specs[0].x_off;

	if (y_off < 1.0 && y_off > -1.0)
		m_p->y_off = y_off;
	else
		m_p->y_off = marker_specs[0].y_off;

	if (aspect_adj > 0.0)
		m_p->aspect_adj = aspect_adj;
	else
		m_p->aspect_adj = marker_specs[0].aspect_adj;

	if (size_adj > 0.0)
		m_p->size_adj = size_adj;
	else
		m_p->size_adj = marker_specs[0].size_adj;

	/* added markers are always dynamic */
	m_p->dynamic = True;

	return (wk_p->marker_table_len); 
	
}


/*
 * Allows modification of the characteristics of an existing marker, 
 * whether pre-defined or added using NhlNewMarker.
 */
/*ARGSUSED*/
NhlErrorTypes NhlSetMarker
#if  __STDC__
(Layer instance, 
 int	index,
 char	*marker_string, 
 float	x_off, 
 float	y_off,
 float	aspect_adj,
 float	size_adj)
#else
(instance,index,marker_string,x_off,y_off,aspect_adj,size_adj)
        Layer instance;
	int   index;
	char *marker_string; 
	float x_off; 
	float y_off;
	float aspect_adj;
	float size_adj;
#endif
{
	NhlMarkerSpec *m_p;
	char *c_p;

	if (index <= 0 || index > marker_table_len) {
		NhlPError(WARNING,E_UNKNOWN,
			  "_NhlEditMarker: invalid marker index");
		return(WARNING);
	}

	m_p = marker_table[index];

/* 
 * If the marker is one of the initial statically defined markers, make
 * a copy. This will allow the default markers to be restored if a method
 * of restoring defaults is implemented. All marker entries will have the
 * dynamic flag set true by the time this routine exits.
 */
	if (! m_p->dynamic) {
		if ((m_p = (NhlMarkerSpec *) 
		    NhlMalloc(sizeof(NhlMarkerSpec))) == NULL) {
			NhlPError(FATAL,E_UNKNOWN,
			      "_NhlEditMarker: marker alloc failed");
			return(FATAL);
		}
		memcpy((void *) m_p, (const void *) marker_table[index],
			sizeof(NhlMarkerSpec));
		marker_table[index] = m_p;
	}
		
	if (marker_string != NULL && 
	    strcmp(marker_string, m_p->marker)) {
		    if ((c_p = NhlMalloc(strlen(marker_string)+ 1 )) == NULL) {
			    NhlPError(FATAL,E_UNKNOWN,
				 "_NhlEditMarker: marker string alloc failed");
			    return(FATAL);
		    }
		    strcpy(c_p, marker_string);
		    if (m_p->dynamic) 
			    NhlFree(m_p->marker);
		    m_p->marker = c_p;
	}
	m_p->dynamic = True;

	if (x_off < 1.0 && x_off > -1.0)  
		m_p->x_off = x_off;
	else
		m_p->x_off = marker_specs[0].x_off;

	if (y_off < 1.0 && y_off > -1.0)
		m_p->y_off = y_off;
	else
		m_p->y_off = marker_specs[0].y_off;

	if (aspect_adj > 0.0)
		m_p->aspect_adj = aspect_adj;
	else
		m_p->aspect_adj = marker_specs[0].aspect_adj;

	if (size_adj > 0.0)
		m_p->size_adj = size_adj;
	else
		m_p->size_adj = marker_specs[0].size_adj;

	return (NOERROR); 
	
}

/*ARGSUSED*/
void _NhlSetMarkerInfo
#if  __STDC__
(Layer instance,Layer plot)
#else
(instance,plot)
        Layer instance;
        Layer plot;
#endif
{
        WorkstationLayer tinst = (WorkstationLayer)instance;
	WorkstationLayerPart *wk_p = &tinst->work;
        float   fl,fr,fb,ft,ul,ur,ub,ut;
        float  x0,x1;
        int ll,ix;
        char buffer[80];


	if (wk_p->marker_lines_on && wk_p->marker_line_dash_pattern < 0) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
		  "_NhlSetMarkerInfo: invalid marker dash pattern index");
		wk_p->marker_line_dash_pattern = NhlSOLIDLINE;
	}
	else if (wk_p->edges_on && wk_p->marker_line_dash_pattern > 0) {
		memset((void *) buffer, 0, 80 * sizeof(char));

		c_sflush();

		c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);

		x0 = fl;
		x1 = fl + wk_p->marker_line_dash_seglen;
		x0 = (float)c_kfpy(x0);
		x1 = (float)c_kfpy(x1);
	
		if ((ix = wk_p->marker_line_dash_pattern) > 
		    wk_p->dash_table_len) {
			/* INFO - but it's a void function right now */
			NhlPError(INFO,E_UNKNOWN,
       "_NhlSetMarkerInfo: using mod function on dash pattern index: %d", ix);

			ix = 1 + (ix - 1) % wk_p->dash_table_len;
		}
		wk_p->marker_line_dash_dollar_size = (x1 - x0) /
			strlen(dash_patterns[ix]) + 0.5;
		if(wk_p->marker_line_dash_dollar_size < 1)
                        wk_p->marker_line_dash_dollar_size = 1;
		
		strcpy(buffer,dash_patterns[ix]);
		
		c_dashdc(buffer,wk_p->marker_line_dash_dollar_size,1);
	}

/*
 * Make sure the marker size is okay
 */
	if (wk_p->marker_size <= 0.0) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
		"_NhlSetMarkerInfo: marker size must be greater than 0.0");
		wk_p->marker_size = 0.007;
	}
/*
 * An out-of-bounds marker index should have been caught at a higher
 * level. Error and set to default marker.
 */

	if ((ix = wk_p->marker_index) < 0) {
		/* WARNING - but it's a void function right now */
		NhlPError(WARNING,E_UNKNOWN,
			   "_NhlSetMarkerInfo: invalid marker index");
		wk_p->marker_index = NhlWK_DEF_MARKER;
	}
	else if (ix >wk_p->fill_table_len) {
		/* INFO - but it's a void function right now */
		NhlPError(INFO,E_UNKNOWN,
	 "_NhlSetLineInfo: using mod function on marker index: %d", ix);
	}

        return;
}


static NhlErrorTypes WorkstationMarker
#if  __STDC__
(Layer l,float *x,float *y,int num_points)
#else
(l,x,y,num_points)
	Layer l;
	float *x;
	float *y;
	int num_points;
#endif
{
        WorkstationLayer inst = (WorkstationLayer)l;
	WorkstationLayerPart *wk_p = &inst->work;
        float   fl,fr,fb,ft,ul,ur,ub,ut;
	int ll, i, index;
	int save_font;
	Gint save_linecolor;
	Gint save_linetype;
	Gdouble save_linewidth;
	Gint err_ind;
	float marker_size, x_off, y_off;
	int ret = NOERROR;
	char *string;
	int marker_color,line_color;
	float p_height, p_width;

/*
 * Make the user space coincide with the NDC space for the
 * duration of the routine
 */
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	c_set(fl,fr,fb,ft,fl,fr,fb,ft,1);
/*
 * Save attributes that may be modified
 */
	ginq_line_colr_ind(&err_ind, &save_linecolor);
	ginq_linewidth(&err_ind, &save_linewidth);
	ginq_linetype(&err_ind, &save_linetype);
	c_pcgeti("FN",&save_font);
	marker_color = _NhlGetGksCi(inst->base.wkptr, wk_p->marker_color);

/*
 * If marker lines are on, draw lines connecting the marker points
 */
	if (wk_p->marker_lines_on && num_points > 1) {
		gset_linewidth(wk_p->marker_line_thickness);
		line_color = _NhlGetGksCi(inst->base.wkptr, 
					  wk_p->marker_line_color);
		gset_line_colr_ind((Gint) line_color);
		if (wk_p->marker_line_dash_pattern > 0) {
			c_curved(x,y,num_points);
		}
		else {
			c_curve(x,y,num_points);
		}
	}
		
/*
 * Draw the markers; markers that do not define their own font using
 * a function code are drawn using the default font (font #1).
 */
	c_pcseti("FN", 1);
	gset_linewidth(wk_p->marker_thickness);
	c_pcseti("OC",marker_color);
	c_pcseti("CC",marker_color);
	if ((index = wk_p->marker_index) <= 0) {
		/* the marker string is used to define the marker */
		x_off = wk_p->marker_size * wk_p->marker_x_off;
		y_off = wk_p->marker_size * wk_p->marker_y_off;
		marker_size = wk_p->marker_size;
		if ((string = wk_p->marker_string) == NULL ||
		    string[0] == '\0') {
			/* WARNING*/
			NhlPError(WARNING,E_UNKNOWN,
			      "_NhlWorkstationMarker: invalid marker string");
			ret = WARNING;
			marker_size = 
				marker_table[NhlWK_DEF_MARKER]->size_adj *
					wk_p->marker_size;
			x_off = marker_size * 
				(marker_table[NhlWK_DEF_MARKER]->x_off + 
				 wk_p->marker_x_off);
			y_off = marker_size * 
				(marker_table[NhlWK_DEF_MARKER]->y_off +
				 wk_p->marker_y_off);
			string = 
				marker_table[NhlWK_DEF_MARKER]->marker;
		}
			
		for (i=0; i<num_points; i++) {
			c_plchhq(x[i]+x_off,y[i]+y_off,string,
				 marker_size,0.0,0.0);
		}

	}
	else if (index > 0) {
		index = 1 + (index - 1) % wk_p->marker_table_len;
		for (i=0; i<num_points; i++) {
			if (marker_table[index]->aspect_adj <= 1.0) {
				p_height = 21.0 * 
					marker_table[index]->aspect_adj;
				p_width = 21.0;
			} else {
				p_width = 21.0 / 
					marker_table[index]->aspect_adj;
				p_height = 21.0;
			}
			
			c_pcsetr("PH",p_height);
			c_pcsetr("PW",p_width);
			marker_size = marker_table[index]->size_adj *
				wk_p->marker_size;
			x_off = marker_size * 
				(marker_table[index]->x_off + 
				 wk_p->marker_x_off);
			y_off = marker_size * 
				(marker_table[index]->y_off + 
				 wk_p->marker_y_off);
			c_plchhq(x[i]+x_off,y[i]+y_off,
				 marker_table[index]->marker,
				 marker_size,0.0,0.0);
		}
	}

/*
 * Restore state
 */

	gset_line_colr_ind(save_linecolor);
	gset_linewidth(save_linewidth);
	gset_linetype(save_linetype);
	c_pcseti("FN",save_font);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);

	return(ret);

}

NhlErrorTypes CallWorkstationMarker
#if  __STDC__
(LayerClass lc, Layer instance,  float *x, float *y, int num_points)
#else
(lc, instance,  x, y, num_points)
LayerClass lc;
Layer instance;
float *x;
float *y;
int num_points;
#endif
{
        WorkstationLayerClass tlc = (WorkstationLayerClass)lc;

        if(tlc->work_class.marker_work == NULL){
                if(tlc->base_class.superclass != NULL) {
                        return(CallWorkstationMarker(
					      lc->base_class.superclass,
					      instance,x,y,num_points));
                } else {
                        NhlPError(WARNING,E_UNKNOWN,"_NhlWorkstationMarker: Transformation object of type (%s) does not have marker_work function",
				  tlc->base_class.class_name);
                        return(WARNING);
                }
        } else {
                return((*tlc->work_class.marker_work)(instance,
						      x,y,num_points));
        }
}

NhlErrorTypes _NhlWorkstationMarker
#if __STDC__
(Layer instance, float *x, float *y, int num_points)
#else
(instance,x,y,num_points)
Layer instance;
float   *x;
float *y;
int num_points;
#endif
{
        return(CallWorkstationMarker(instance->base.layer_class,
				     instance,x,y,num_points));
}

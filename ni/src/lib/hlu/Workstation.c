/*
 *      $Id: Workstation.c,v 1.70 1997-07-29 15:56:04 ethan Exp $
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
 *				NhlErrorTypes	_NhlAddWorkChildNhlLayer
 *				NhlErrorTypes	_NhlDeleteWorkChildNhlLayer
 */

#include <stdio.h>
#include <string.h>
#include <limits.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/hlu/TransformI.h>

#define DEBUG_NCGM 0
/* 
 * There are currently 16 pre-defined dash patterns provided plus solid
 * (index 0, or NhlSOLIDLINE). The solid line is element 0 of the dash
 * pattern table but is not counted in the number of dash patterns 
 * returned to the user. Therefore the user can use dash pattern indexes 
 * 0 through (and including) NhlNwkDashTableLength as valid indexes.
 */

static char *dash_patterns[] = { 
		 "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",
                 "$$$$''$$$$''$$$$''$$$$''$$$$''$$$$''$$$$''$$$$''",
                 "$''$''$''$''$''$''$''$''$''$''$''$''$''$''$''$''",
                 "$$$$''$''$$$$''$''$$$$''$''$$$$''$''$$$$''$''",
                 "$$$$''$'$''$$$$''$'$''$$$$''$'$''$$$$''$'$''",
                 "$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'$$'",
                 "$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'$$$'",
                 "$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'$'$$'",
                 "$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'$'$$$'",
                 "$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'$$'$$$$'",
                 "$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'$$$$'$$'$'$$'",
                 "$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''$$''",
                 "$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''$$$$$$''",
                 "$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''$$$'$$$''",
                 "$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''$$'''",
                 "$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''$'$'''",
                 "$$$$$'''''$$$$$'''''$$$$$'''''$$$$$'''''$$$$$'''''",
};

/* 
 * There are 16 pre-defined fill patterns, plus solid (index 0, 
 * or NhlSOLIDFILL ) and hollow (index -1, or NhlHOLLOWFILL). 
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
{ 0,   0.0003125, 0, NULL, 0, -4, 4 },
{ 45,  0.0075, 1, NULL, 0, 1, 0 }

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

static NrmQuark intQ;
static NrmQuark intgenQ;
static NrmQuark colormap_name;
static NrmQuark bkgnd_name;
static NrmQuark foregnd_name;
static NrmQuark	marker_tbl_strings_name;
static NrmQuark marker_tbl_params_name;
static NrmQuark dash_table_name;

#define Oset(field) NhlOffset(NhlWorkstationLayerRec,work.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNwkColorMap,NhlCwkColorMap,NhlTColorMap,sizeof(NhlGenArray),
		Oset(color_map),NhlTString,_NhlUSET("default"),
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkColorMapLen,NhlCwkColorMapLen,NhlTInteger,sizeof(int),
		Oset(color_map_len),NhlTImmediate,
		_NhlUSET(0),_NhlRES_GONLY,NULL},
	{NhlNwkBackgroundColor,NhlCwkBackgroundColor,NhlTFloatGenArray,
		sizeof(NhlPointer),Oset(bkgnd_color),NhlTImmediate,
		_NhlUSET(NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkForegroundColor,NhlCwkForegroundColor,NhlTFloatGenArray,
		sizeof(NhlPointer),Oset(foregnd_color),NhlTImmediate,
		_NhlUSET(NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkGksWorkId,NhlCwkGksWorkId,NhlTInteger,sizeof(int),
		 Oset(gkswksid),
		 NhlTImmediate,_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},
	{NhlNwkVSWidthDevUnits,NhlCwkVSWidthDevUnits,NhlTInteger,sizeof(int),
         	Oset(vswidth_dev_units),NhlTImmediate,
         	_NhlUSET((NhlPointer)1024),_NhlRES_GONLY,NULL},
	{NhlNwkDashTableLength,NhlCwkDashTableLength,NhlTInteger,sizeof(int),
		Oset(dash_table_len),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_GONLY,NULL},
	{NhlNwkFillTableLength,NhlCwkFillTableLength,NhlTInteger,
		sizeof(int),Oset(fill_table_len),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},
	{NhlNwkMarkerTableLength,NhlCwkMarkerTableLength,NhlTInteger,
		sizeof(int),Oset(marker_table_len),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},
	{NhlNwkDefGraphicStyleId,NhlCwkDefGraphicStyleId,
		 NhlTObjId,sizeof(int),Oset(def_graphic_style_id),
		 NhlTImmediate,_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},

#define POset(field) Oset(public_lineinfo.field)
	{NhlNwkDashPattern,NhlCwkDashPattern,NhlTDashIndex,
		 sizeof(NhlDashIndex),POset(dash_pattern),
		 NhlTImmediate,_NhlUSET((NhlPointer)0),
         	_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineDashSegLenF,NhlCwkLineDashSegLenF,NhlTFloat,sizeof(float),
		POset(line_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineColor,NhlCwkLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
		POset(line_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineThicknessF,NhlCwkLineThicknessF,NhlTFloat,sizeof(float),
		POset(line_thickness),NhlTString,_NhlUSET("1.0"),
		_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabel,NhlCwkLineLabel,NhlTString,sizeof(NhlString),
		POset(line_label_string),NhlTImmediate,
		 _NhlUSET((NhlPointer)NULL),
		 _NhlRES_DEFAULT|_NhlRES_PRIVATE,(NhlFreeFunc)NhlFree},
	{NhlNwkLineLabelFont,NhlCwkLineLabelFont,NhlTFont,sizeof(NhlFont),
		POset(line_label_font),NhlTImmediate,
		 _NhlUSET((NhlPointer)0),
		_NhlRES_DEFAULT|_NhlRES_PRIVATE,(NhlFreeFunc)NhlFree},
	{NhlNwkLineLabelFontColor,NhlCwkLineLabelFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(line_label_font_color),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),
		_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabelFontHeightF,NhlCwkLineLabelFontHeightF,NhlTFloat,
		sizeof(float),POset(line_label_font_height),NhlTString,
		_NhlUSET("0.0125"),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabelFontAspectF,NhlCwkLineLabelFontAspectF,NhlTFloat,
		sizeof(float),POset(line_label_font_aspect),NhlTString,
		_NhlUSET("1.3125"),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabelFontThicknessF,NhlCwkLineLabelFontThicknessF,NhlTFloat,
		sizeof(float),POset(line_label_font_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabelFontQuality,NhlCwkLineLabelFontQuality,NhlTFontQuality,
		sizeof(NhlFontQuality),POset(line_label_font_quality),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlHIGH),
         	_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabelConstantSpacingF,NhlCwkLineLabelConstantSpacingF,
		NhlTFloat,sizeof(float),POset(line_label_const_spacing),
		NhlTString,_NhlUSET("0.0"),
         	_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkLineLabelFuncCode,NhlCwkLineLabelFuncCode,NhlTCharacter,
		sizeof(char),POset(line_label_func_code),NhlTString,
		_NhlUSET(":"),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
#undef POset
#define POset(field) Oset(public_markinfo.field)
	{NhlNwkMarkerIndex,NhlCwkMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),POset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkMarkerColor,NhlCwkMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkMarkerSizeF,NhlCwkMarkerSizeF,NhlTFloat,sizeof(float),
		POset(marker_size),NhlTString,
		 _NhlUSET("0.007"),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkMarkerThicknessF,NhlCwkMarkerThicknessF,NhlTFloat,sizeof(float),
		POset(marker_thickness),NhlTString,_NhlUSET("1.0"),
		_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
#undef POset


/* End-documented-resources */

	{_NhlNwkReset,_NhlCwkReset,NhlTBoolean,sizeof(NhlBoolean),Oset(reset),
		NhlTImmediate,_NhlUSET((NhlPointer)0),
         	_NhlRES_SONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkSetPublic,_NhlCwkSetPublic,NhlTBoolean,sizeof(NhlBoolean),
		Oset(set_public),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_SONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkGraphicStyle,_NhlCwkGraphicStyle,NhlTObjId,
		sizeof(int),Oset(graphic_style_id),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlNULLOBJID),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},

#define POset(field) Oset(private_lineinfo.field)
	{_NhlNwkDashPattern,_NhlCwkDashPattern,NhlTDashIndex,
		sizeof(NhlDashIndex),POset(dash_pattern),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineDashSegLenF,_NhlCwkLineDashSegLenF,NhlTFloat,sizeof(float),
		POset(line_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineColor,_NhlCwkLineColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),POset(line_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineThicknessF,_NhlCwkLineThicknessF,NhlTFloat,sizeof(float),
		POset(line_thickness),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabel,_NhlCwkLineLabel,NhlTString,sizeof(NhlString),
		 POset(line_label_string),NhlTImmediate,
		 _NhlUSET((NhlPointer)NULL),
		_NhlRES_SGONLY|_NhlRES_PRIVATE,(NhlFreeFunc)NhlFree},
	{_NhlNwkLineLabelFont,_NhlCwkLineLabelFont,NhlTFont,sizeof(NhlFont),
		POset(line_label_font),NhlTImmediate,
		 _NhlUSET((NhlPointer)0),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelFontColor,_NhlCwkLineLabelFontColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(line_label_font_color),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlFOREGROUND),
		_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelFontHeightF,_NhlCwkLineLabelFontHeightF,NhlTFloat,
		sizeof(float),POset(line_label_font_height),NhlTString,
		_NhlUSET("0.0125"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelFontAspectF,_NhlCwkLineLabelFontAspectF,NhlTFloat,
		sizeof(float),POset(line_label_font_aspect),NhlTString,
		_NhlUSET("1.3125"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelFontThicknessF,_NhlCwkLineLabelFontThicknessF,
		NhlTFloat,sizeof(float),POset(line_label_font_thickness),
		NhlTString,_NhlUSET("1.0"),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelFontQuality,_NhlCwkLineLabelFontQuality,
		NhlTFontQuality,sizeof(NhlFontQuality),
		POset(line_label_font_quality),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlHIGH),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelConstantSpacingF,_NhlCwkLineLabelConstantSpacingF,
		NhlTFloat,sizeof(float),POset(line_label_const_spacing),
		NhlTString,_NhlUSET("0.0"),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkLineLabelFuncCode,_NhlCwkLineLabelFuncCode,NhlTCharacter,
		sizeof(char),POset(line_label_func_code),NhlTString,
		_NhlUSET(":"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
#undef POset

#define POset(field) Oset(private_markinfo.field)
	{_NhlNwkMarkerIndex,_NhlCwkMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),POset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkMarkerColor,_NhlCwkMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkMarkerSizeF,_NhlCwkMarkerSizeF,NhlTFloat,sizeof(float),
		POset(marker_size),NhlTString,_NhlUSET("0.007"),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkMarkerThicknessF,_NhlCwkMarkerThicknessF,NhlTFloat,
		sizeof(float),POset(marker_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
#undef POset

#define POset(field) Oset(private_fillinfo.field)

	{_NhlNwkFillIndex,_NhlCwkFillIndex,NhlTFillIndex,sizeof(NhlFillIndex),
		POset(fill_index),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkFillColor,_NhlCwkFillColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),POset(fill_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkFillBackground,_NhlCwkFillBackground,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(fill_background),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlTRANSPARENT),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkFillScaleFactorF,_NhlCwkFillScaleFactorF,NhlTFloat,
		sizeof(float),POset(fill_scale_factor),NhlTString,
		_NhlUSET("1.0"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkFillLineThicknessF,_NhlCwkFillLineThicknessF,NhlTFloat,
		sizeof(float),POset(fill_line_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkEdgesOn,_NhlCwkEdgesOn,NhlTBoolean,sizeof(NhlBoolean),
		POset(edges_on),NhlTImmediate,_NhlUSET(False),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkEdgeDashPattern,_NhlCwkEdgeDashPattern,NhlTDashIndex,
		sizeof(NhlDashIndex),POset(edge_dash_pattern),NhlTImmediate,
		_NhlUSET(0),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkEdgeThicknessF,_NhlCwkEdgeThicknessF,NhlTFloat,sizeof(float),
		POset(edge_thickness),NhlTString,
		 _NhlUSET("1.0"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkEdgeDashSegLenF,_NhlCwkEdgeDashSegLenF,NhlTFloat,sizeof(float),
		POset(edge_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkEdgeColor,_NhlCwkEdgeColor,NhlTColorIndex,
		 sizeof(NhlColorIndex),POset(edge_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
#undef POset


	{_NhlNwkDashTable,_NhlCwkDashTable,NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(dash_table),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_SGONLY|_NhlRES_PRIVATE,
		(NhlFreeFunc)NhlFreeGenArray},
	{_NhlNwkMarkerTableStrings,_NhlCwkMarkerTableStrings,
         	NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(marker_table_strings),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_SGONLY|_NhlRES_PRIVATE,
		(NhlFreeFunc)NhlFreeGenArray},
	{_NhlNwkMarkerTableParams,_NhlCwkMarkerTableParams,NhlTGenArray,
		sizeof(NhlGenArray),Oset(marker_table_params),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL}
};

static _NhlRawClassCB workc_callbacks[] = {
	{_NhlCBworkPreOpen,NULL,0,NULL,NULL,NULL},
};

/*
* Base class method declarations
*/

static NhlErrorTypes WorkstationClassInitialize();

static NhlErrorTypes WorkstationClassPartInitialize(
#if	NhlNeedProto
	NhlClass	layerclass	/* layerclass to init	*/
#endif
);

static NhlErrorTypes WorkstationInitialize(
#if	NhlNeedProto
        NhlClass,	/* class */
        NhlLayer,	/* req */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args */
#endif
);

static NhlErrorTypes    WorkstationSetValues(
#if	NhlNeedProto
        NhlLayer,	/* old */
        NhlLayer,	/* reference */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes 	WorkstationGetValues(
#if	NhlNeedProto
	NhlLayer,	/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes WorkstationDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes WorkstationDraw(
#if	NhlNeedProto
	NhlLayer
#endif
);

/*
* WorkStation class method declarations
*/


static NhlErrorTypes WorkstationOpen(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes WorkstationClose(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes WorkstationActivate(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes WorkstationDeactivate(
#if	NhlNeedProto
	NhlLayer	/* instance */
#endif
);

static NhlErrorTypes WorkstationAllocateColors(
#if	NhlNeedProto
	NhlLayer	wl
#endif
);

static NhlErrorTypes WorkstationUpdate(
#if	NhlNeedProto
	NhlLayer	l	/* instance	*/
#endif
);

static NhlErrorTypes WorkstationClear(
#if	NhlNeedProto
	NhlLayer	l	/* instance	*/
#endif
);

static NhlErrorTypes WorkstationLineTo(
#if 	NhlNeedProto
	NhlLayer	l,
	float		x,
	float 		y,
	int		upordown
#endif
);

static NhlErrorTypes WorkstationFill(
#if 	NhlNeedProto
	NhlLayer	l,
	float		*x,
	float 		*y,
	int		num_points
#endif
);

static NhlErrorTypes WorkstationMarker(
#if 	NhlNeedProto
	NhlLayer	l,
	float		*x,
	float 		*y,
	int		num_points
#endif
);

/*
 * Only MAX_OPEN_WKS Workstation instances are allowed to be open at one time.
 * The GKS Segment workstation is opened by the View class initialize
 * without using the Workstation class interface. Therefore, from the
 * Workstation class's point of view, one GKS workstation is unavailable.
 */
 
static int	CurrentWksCount = 0;

NhlWorkstationClassRec NhlworkstationClassRec = {
        {
/* class_name			*/	"workstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlbaseClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	workc_callbacks,
/* num_class_callbacks		*/	NhlNumber(workc_callbacks),

/* class_part_initialize	*/	WorkstationClassPartInitialize,
/* class_initialize		*/	WorkstationClassInitialize,
/* layer_initialize		*/	WorkstationInitialize,
/* layer_set_values		*/	WorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	WorkstationGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	WorkstationDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	WorkstationDraw,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
        },
	{
/* current_wks_count	*/	&CurrentWksCount,               
/* def_background	*/	{0.0,0.0,0.0},
/* pal			*/	NhlDEFAULT_APP,
/* open_work		*/	WorkstationOpen,
/* close_work		*/	WorkstationClose,
/* activate_work	*/	WorkstationActivate,
/* deactivate_work	*/	WorkstationDeactivate,
/* alloc_colors		*/	WorkstationAllocateColors,
/* update_work		*/	WorkstationUpdate,
/* clear_work		*/	WorkstationClear,
/* lineto_work 		*/	WorkstationLineTo,
/* fill_work		*/	WorkstationFill,
/* marker_work		*/	WorkstationMarker
	}
};

NhlClass NhlworkstationClass = (NhlClass)&NhlworkstationClassRec;

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
static NhlErrorTypes
WorkstationClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	NhlErrorTypes	ret = NhlNOERROR;
	Gop_st status;
	int status1,dummy = 6;
	int i;
	_NhlEnumVals	dashvals[] = {
		{NhlSOLIDLINE,	"solidline"}
	};
	NhlConvertArg	dashargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMINMAX)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)0)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)16)}
	};
	_NhlEnumVals	colorvals[] = {
		{NhlTRANSPARENT,	"transparent"},
		{NhlNULLCOLOR,		"nullcolor"},
		{NhlBACKGROUND,		"background"},
		{NhlFOREGROUND,		"foreground"}
	};
	NhlConvertArg	colorargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)}
	};
	_NhlEnumVals	fillvals[] = {
		{NhlSOLIDFILL,	"solidfill"},
		{NhlHOLLOWFILL,	"hollowfill"},
		{NhlNULLFILL,	"nullfill"}
	};
	NhlConvertArg	fillargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMINMAX)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)17)}
	};
	_NhlEnumVals	markervals[] = {
		{0,	"default"},
		{1,	"dot"},
		{2,	"+"},
		{2,	"plus"},
		{3,	"*"},
		{3,	"asterisk"},
		{4,	"hollow_circle"},
		{5,	"cross"},
		{5,	"x"},
		{6,	"hollow_square"},
		{7,	"up_triangle"},
		{8,	"down_triangle"},
		{9,	"neil"},
		{9,	"diamond"},
		{10,	"left_triangle_filled"},
		{11,	"right_triangle_filled"},
		{12,	"star_5point"},
		{13,	"star_6point"},
		{14,	"circle_w_dot"},
		{15,	"circle_w_cross"},
		{16,	"circle_filled"}
	};

	_NhlEnumVals	mrkline[] = {
		{NhlLINES,	"lines"},
		{NhlMARKERS,	"markers"},
		{NhlMARKLINES,	"marklines"}
	};

	(void)_NhlRegisterEnumType(NhlobjClass,NhlTDashIndex,dashvals,
		NhlNumber(dashvals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTColorIndex,colorvals,
		NhlNumber(colorvals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTFillIndex,fillvals,
		NhlNumber(fillvals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTMarkerIndex,markervals,
		NhlNumber(markervals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTMarkLineMode,mrkline,
		NhlNumber(mrkline));

	(void)NhlRegisterConverter(NhlobjClass,NhlTScalar,NhlTDashIndex,
		_NhlCvtScalarToIndex,dashargs,NhlNumber(dashargs),False,NULL);
	(void)NhlRegisterConverter(NhlobjClass,NhlTScalar,NhlTColorIndex,
		_NhlCvtScalarToIndex,colorargs,NhlNumber(colorargs),False,NULL);
	(void)NhlRegisterConverter(NhlobjClass,NhlTScalar,NhlTFillIndex,
		_NhlCvtScalarToIndex,fillargs,NhlNumber(fillargs),False,NULL);

	(void)NhlRegisterConverter(NhlobjClass,NhlTGenArray,
		NhlTDashIndexGenArray,_NhlCvtGenArrayToIndexGenArray,dashargs,
		NhlNumber(dashargs),False,NULL);
	(void)NhlRegisterConverter(NhlobjClass,NhlTGenArray,
		NhlTColorIndexGenArray,_NhlCvtGenArrayToIndexGenArray,colorargs,
		NhlNumber(colorargs),False,NULL);
	(void)NhlRegisterConverter(NhlobjClass,NhlTGenArray,
		NhlTFillIndexGenArray,_NhlCvtGenArrayToIndexGenArray,fillargs,
		NhlNumber(fillargs),False,NULL);

	intQ = NrmStringToQuark(NhlTInteger);
	intgenQ = NrmStringToQuark(NhlTIntegerGenArray);
	colormap_name = NrmStringToQuark(NhlNwkColorMap);
	bkgnd_name = NrmStringToQuark(NhlNwkBackgroundColor);
	foregnd_name = NrmStringToQuark(NhlNwkForegroundColor);
	marker_tbl_strings_name = NrmStringToQuark(_NhlNwkMarkerTableStrings);
	marker_tbl_params_name = NrmStringToQuark(_NhlNwkMarkerTableParams);
	dash_table_name = NrmStringToQuark(_NhlNwkDashTable);

	ginq_op_st(&status);

	if(status == GST_GKCL) {
/*
* Going to want to change what the logical unit errors will go out to
* which is the first parameter of the gopks call.
*/
		status1 = 0;
/* FORTRAN */ _NHLCALLF(gopks,GOPKS)(&dummy,&status1);
		/*
		 * Check for a gks Error.
		 */
		if(_NhlLLErrCheckPrnt(NhlFATAL,"WorkstationClassInitialize")){
			/*
			 * Bummer dude!
			 */
			NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Can't Open GKS",
						"WorkstationClassInitialize");
			return NhlFATAL;
		}
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "WorkstationClassInitialize: NhlMalloc failed");
		return NhlFATAL;
	}
	for (i = 0; i < marker_table_len; i++) {
		marker_table[i] = &marker_specs[i];
	}

	ret = NhlVACreate(&NhlworkstationClassRec.work_class.pal,"pal",
					NhlpaletteClass,_NhlGetDefaultApp(),
		_NhlNpalWorkClass,	NhlworkstationClass,
		NULL);
	return ret;
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
#if	NhlNeedProto
(
	NhlClass	layerclass	/* layerclass to init	*/
)
#else
(layerclass)
	NhlClass	layerclass;	/* layerclass to init	*/
#endif
{
	NhlWorkstationClass	lc =
					(NhlWorkstationClass)layerclass;
	NhlWorkstationClass	sc = (NhlWorkstationClass)
						lc->base_class.superclass;

        if(lc->work_class.current_wks_count == NhlInheritCurrentWksCount)
		lc->work_class.current_wks_count =
                        sc->work_class.current_wks_count;

	if(lc->work_class.pal == NhlInheritPalette)
		lc->work_class.pal = sc->work_class.pal;

	if(lc->work_class.open_work == NhlInheritOpen)
		lc->work_class.open_work = sc->work_class.open_work;

	if(lc->work_class.close_work == NhlInheritClose)
		lc->work_class.close_work = sc->work_class.close_work;

	if(lc->work_class.activate_work == NhlInheritActivate)
		lc->work_class.activate_work = sc->work_class.activate_work;

	if(lc->work_class.deactivate_work == NhlInheritDeactivate)
		lc->work_class.deactivate_work = sc->work_class.deactivate_work;

	if(lc->work_class.alloc_colors == NhlInheritAllocateColors)
		lc->work_class.alloc_colors = sc->work_class.alloc_colors;

	if(lc->work_class.update_work == NhlInheritUpdate)
		lc->work_class.update_work = sc->work_class.update_work;

	if(lc->work_class.clear_work == NhlInheritClear)
		lc->work_class.clear_work = sc->work_class.clear_work;

	if(lc->work_class.lineto_work == NhlInheritLineTo)
		lc->work_class.lineto_work = sc->work_class.lineto_work;

	if(lc->work_class.fill_work == NhlInheritFill)
		lc->work_class.fill_work = sc->work_class.fill_work;

	if(lc->work_class.marker_work == NhlInheritMarker)
		lc->work_class.marker_work = sc->work_class.marker_work;

	return NhlNOERROR;
}

/*
 * Function:	DoCmap
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlErrorTypes
DoCmap
#if	NhlNeedProto
(
	NhlWorkstationLayer	wl,
	NhlString		entry_name
)
#else
(wl,entry_name)
	NhlWorkstationLayer	wl;
	NhlString		entry_name;
#endif
{
	NhlWorkstationClass	wc = (NhlWorkstationClass)wl->base.layer_class;
	NhlWorkstationClassPart	*wcp = &wc->work_class;
	NhlWorkstationLayerPart	*wp = &wl->work;
	int			i;
	NhlColor		tc;
	NhlColor		*tcp = NULL;
	NhlPrivateColor		*pcmap = wp->private_color_map;
	NhlString		e_text;
	NhlErrorTypes 		ret = NhlNOERROR;

	/*
	 * If cmap is set, use it.
	 */
	if(wp->color_map){
		tcp = wp->color_map->data;
		wp->color_map_len = wp->color_map->len_dimensions[0];
		if(wp->color_map_len > _NhlMAX_COLOR_MAP) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DoCmap: Maximum color map length exceeded. Limit is (%d). Requested length is (%d), using only first (%d) elements.",_NhlMAX_COLOR_MAP,wp->color_map_len,_NhlMAX_COLOR_MAP);
			wp->color_map_len = _NhlMAX_COLOR_MAP;
			ret = NhlWARNING;
		}
		wp->color_map = NULL;
		wp->cmap_changed = True;

		for(i=0;i < wp->color_map_len;i++){
			if (tcp[i][0] < 0.0)
				continue;
			pcmap[i].red = tcp[i][0];
			pcmap[i].green = tcp[i][1];
			pcmap[i].blue = tcp[i][2];
			if(pcmap[i].cstat == _NhlCOLUNSET)
				pcmap[i].cstat = _NhlCOLNEW;
			else
				pcmap[i].cstat = _NhlCOLCHANGE;
		}
		for(i=wp->color_map_len; i < _NhlMAX_COLOR_MAP;i++){
			if(pcmap[i].cstat == _NhlCOLSET)
				pcmap[i].cstat = _NhlCOLREMOVE;
		}
	}

	/*
	 * Do background - If the user sets the background resource it is
	 * used.  If they didn't, but they set the cmap resource, it is set
	 * from the cmap.  If they didn't set either one, it defaults to
	 * the value of the def_background class field(This is done in
	 * "WorkstationAllocateColors"). A user-set value for the
	 * background is accepted only if it has exactly 3 elements.
	 */
	tcp = NULL;
	if(wp->bkgnd_color){
		if (wp->bkgnd_color->num_elements != 3) {
			e_text = 
	      "%s: ignoring %s; 3 color elements required (red, green, blue)";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
			tcp = NULL;
		}
		else {
			tcp = wp->bkgnd_color->data;
			if((*tcp)[0] == -1.0){
				tcp = NULL;
			}
			else if((*tcp)[0] < 0.0 || (*tcp)[0] > 1.0 ||
			   (*tcp)[1] < 0.0 || (*tcp)[1] > 1.0 ||
			   (*tcp)[2] < 0.0 || (*tcp)[2] > 1.0) {
				e_text = 
			     "%s: ignoring %s; out of range color elements";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNwkBackgroundColor);
				tcp = NULL;
			}
		}
		wp->bkgnd_color = NULL;
	}
	if((pcmap[NhlBACKGROUND].cstat == _NhlCOLUNSET) && !tcp){
		tcp = &wcp->def_background;
	}
	if(tcp){
		wp->cmap_changed = True;
		pcmap[NhlBACKGROUND].cstat =
			(pcmap[NhlBACKGROUND].cstat == _NhlCOLUNSET)?
			_NhlCOLNEW:_NhlCOLCHANGE;
		pcmap[NhlBACKGROUND].red = (*tcp)[0];
		pcmap[NhlBACKGROUND].green = (*tcp)[1];
		pcmap[NhlBACKGROUND].blue = (*tcp)[2];
	}

	/*
	 * Do foreground - If the user sets the foreground resource it is
	 * used.  If they didn't, but they set the cmap resource, it is set
	 * from the cmap.  If they didn't set either one, it is set to
	 * either white or black, depending upon which is further in
	 * geometric space.
	 */
	tcp = NULL;
	if(wp->foregnd_color){
		if (wp->foregnd_color->num_elements != 3) {
			e_text = 
	      "%s: ignoring %s; 3 color elements required (red, green, blue)";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
			tcp = NULL;
		}
		else {
			tcp = wp->foregnd_color->data;
			if((*tcp)[0] == -1.0){
				tcp = NULL;
			}
			else if((*tcp)[0] < 0.0 || (*tcp)[0] > 1.0 ||
			   (*tcp)[1] < 0.0 || (*tcp)[1] > 1.0 ||
			   (*tcp)[2] < 0.0 || (*tcp)[2] > 1.0) {
				e_text = 
			     "%s: ignoring %s; out of range color elements";
				NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,
					  entry_name,NhlNwkBackgroundColor);
				tcp = NULL;
			}
		}
		wp->foregnd_color = NULL;
	}
	if((pcmap[NhlFOREGROUND].cstat == _NhlCOLUNSET) && !tcp){
		if(pcmap[NhlBACKGROUND].red * pcmap[NhlBACKGROUND].red +
		pcmap[NhlBACKGROUND].green * pcmap[NhlBACKGROUND].green +
		pcmap[NhlBACKGROUND].blue * pcmap[NhlBACKGROUND].blue < .75){
			tc[0] = 1.0;
			tc[1] = 1.0;
			tc[2] = 1.0;
		}
		else {
			tc[0] = 0.0;
			tc[1] = 0.0;
			tc[2] = 0.0;
		}
		tcp = &tc;
	}
	if(tcp){
		wp->cmap_changed = True;
		pcmap[NhlFOREGROUND].cstat =
			(pcmap[NhlFOREGROUND].cstat == _NhlCOLUNSET)?
			_NhlCOLNEW:_NhlCOLCHANGE;
		pcmap[NhlFOREGROUND].red = (*tcp)[0];
		pcmap[NhlFOREGROUND].green = (*tcp)[1];
		pcmap[NhlFOREGROUND].blue = (*tcp)[2];
	}

	return (ret);
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
#if  NhlNeedProto
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(lc,req,new,args,num_args)
	NhlClass 	lc;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlWorkstationLayer	newl = (NhlWorkstationLayer)new;
	NhlWorkstationLayerPart	*wp = &newl->work;
	NhlWorkstationClassPart	*wcp =
				&((NhlWorkstationClass)lc)->work_class;
	int			i;
	NhlErrorTypes		retcode=NhlNOERROR,subret=NhlNOERROR;
	NhlGenArray		ga;
	char			*e_text;
	char			*entry_name = "WorkstationInitialize";
	int			count[2];
	int			len1, len2;
	NhlMarkerTableParams	*mparams;
	NhlString		*mstrings;

	if(*wcp->current_wks_count >= MAX_OPEN_WKS){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Only %d %s objects may exist simultaneously",
			entry_name,MAX_OPEN_WKS,lc->base_class.class_name);
		return NhlFATAL;
	}
        (*wcp->current_wks_count)++;
        
	wp->gkswksid = (int)NhlFATAL;
	wp->open = False;
	wp->gkswkstype = (int)NhlFATAL;
	wp->gkswksconid = (int)NhlFATAL;

	/*
	 * Initialize colormap with _NhlCOLUNSET, then call DoCmap to fill cmap
	 * with appropriate values.
	 */
	for(i=0;i < _NhlMAX_COLOR_MAP;i++)
		wp->private_color_map[i].cstat = _NhlCOLUNSET;

	wp->cmap_changed = True;
	retcode = DoCmap(newl,entry_name);

	newl->work.fill_table_len = fill_table_len - 1;
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  _NhlNwkMarkerTableParams);
		return NhlFATAL;
	}
	ga->my_data = False;

	if (newl->work.marker_table_params != NULL) {		
		
		subret = _NhlValidatedGenArrayCopy(&ga,
						newl->work.marker_table_params,
						   4*8096,False,False,
						   _NhlNwkMarkerTableParams, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		if (subret > NhlWARNING) {
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  _NhlNwkMarkerTableParams);
		return NhlFATAL;
	}
	ga->my_data = False;

	if (newl->work.marker_table_strings != NULL) {		
		subret = _NhlValidatedGenArrayCopy(&ga,
					  newl->work.marker_table_strings,
						   4*8096,False,False,
						   _NhlNwkMarkerTableStrings, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		if (subret > NhlWARNING) {
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
			subret = NhlSetMarker(new->base.id,i+1,mstr,x,y,asp,
									size);
			if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		}
		else {
			i = NhlNewMarker(new->base.id,mstr,x,y,asp,size);
			retcode = (NhlErrorTypes)MIN(retcode,i);
			if(retcode < NhlWARNING)
				return retcode;
		}
	}
	
	newl->work.dash_table_len = dash_table_len - 1;
/*
 * Initialize the "default" graphics primatives - used privately only.
 */
	newl->work.default_lineinfo = newl->work.private_lineinfo;
	newl->work.default_markinfo = newl->work.private_markinfo;
	newl->work.default_fillinfo = newl->work.private_fillinfo;
	newl->work.lip = &newl->work.private_lineinfo;
	newl->work.mip = &newl->work.private_markinfo;
	newl->work.fip = &newl->work.private_fillinfo;

/*
 * Create the default GraphicStyle object
 */
	{
		int	gsid;
		char	buffer[_NhlMAXRESNAMLEN];

		sprintf(buffer,"%s",new->base.name);
		strcat(buffer,".GraphicStyle");

		subret = NhlVACreate(&gsid,buffer,NhlgraphicStyleClass,
				  new->base.id,NULL);
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
			return retcode;
		newl->work.def_graphic_style_id = gsid;
	}

/*
 * The "wk" line and marker attributes are now obsolete but for now
 * if they are set they will affect the default graphic style.
 */
	{
		_NhlLineStyleInfo *lsp = &newl->work.public_lineinfo;
		_NhlMarkerStyleInfo *msp = &newl->work.public_markinfo; 
		NhlSArg		sargs[128];
		int		nargs = 0;

		
		if (lsp->dash_pattern != 0) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineDashPattern,lsp->dash_pattern);
		}
		
		if ((int)(100*lsp->line_dash_seglen) != 15) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineDashSegLenF,
				   lsp->line_dash_seglen);
		}
	
		if (lsp->line_color != NhlFOREGROUND) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineColor,lsp->line_color);
		}
		if ((int)lsp->line_thickness != 1) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineThicknessF,lsp->line_thickness);
		}
		if (lsp->line_label_string != NULL) {
			NhlSetSArg(&sargs[nargs++],
				NhlNgsLineLabelString,lsp->line_label_string);
		}
		if (lsp->line_label_font != 0) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFont,lsp->line_label_font);
		}

		if (lsp->line_label_font_color != NhlFOREGROUND) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontColor,
				   lsp->line_label_font_color);
		}
		if ((int)(10000*lsp->line_label_font_height) != 125) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontHeightF,
				   lsp->line_label_font_height);
		}
		if ((int)(10000*lsp->line_label_font_aspect) != 13125) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontAspectF,
				   lsp->line_label_font_aspect);
		}
		if ((int)lsp->line_label_font_thickness != 1) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontThicknessF,
				   lsp->line_label_font_thickness);
		}
		if (lsp->line_label_font_quality != NhlHIGH) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontQuality,
				   lsp->line_label_font_quality);
		}
		if ((int)lsp->line_label_const_spacing != 0) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelConstantSpacingF,
				   lsp->line_label_const_spacing);
		}
		if (lsp->line_label_func_code != ':') {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFuncCode,
				   lsp->line_label_func_code);
		}
		if (msp->marker_index != 3) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerIndex,msp->marker_index);
		}
		if (msp->marker_color != NhlFOREGROUND) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerColor,msp->marker_color);
		}
		if ((int)(1000*msp->marker_size) != 7) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerSizeF,msp->marker_size);
		}
		if ((int)msp->marker_thickness != 1) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerThicknessF,
				   msp->marker_thickness);
		}
		if (nargs > 0) {
			subret = NhlALSetValues(
					   newl->work.def_graphic_style_id,
					   sargs,nargs);

			if ((retcode = MIN(subret,retcode)) < NhlWARNING) {
				e_text = 
				    "%s: error setting default GraphicStyle";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			e_text = 
"%s: Obsolete Workstation line or marker resources set: use GraphicStyle resources instead";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			retcode = MIN(retcode,NhlWARNING);
		}
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
static NhlErrorTypes
WorkstationSetValues
#if  NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	reference,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlWorkstationLayer	newl = (NhlWorkstationLayer) new;
	int i;
	NhlWorkstationLayer	oldl = (NhlWorkstationLayer) old;
	NhlErrorTypes	retcode = NhlNOERROR,subret = NhlNOERROR;
	char *tmp;
	char *entry_name = "WorkstationSetValues";
	int len1,len2;
	NhlMarkerTableParams *mparams;
	NhlString *mstrings;
	char *e_text;

	/*
	 * Reset the Graphics Attributes...
	 */
	if(_NhlArgIsSet(args,num_args,_NhlNwkReset)){

		/*
		 * The _NhlNwkReset Private Resource MUST be set individually.
		 */
		if(num_args > 1){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:%s must be set alone!",entry_name,
					_NhlNwkReset);
			return NhlFATAL;
		}

		if (newl->work.private_lineinfo.line_label_string != NULL)
			NhlFree(newl->work.private_lineinfo.line_label_string);

		newl->work.private_lineinfo = newl->work.default_lineinfo;
		newl->work.private_markinfo = newl->work.default_markinfo;
		newl->work.private_fillinfo = newl->work.default_fillinfo;
		newl->work.lip = &newl->work.private_lineinfo;
		newl->work.mip = &newl->work.private_markinfo;
		newl->work.fip = &newl->work.private_fillinfo;

		return NhlNOERROR;
	}
	if(_NhlArgIsSet(args,num_args,_NhlNwkGraphicStyle)){
		NhlLayer gsl;
		_NhlLineStyleInfo	*lsp;
		_NhlMarkerStyleInfo	*msp;
		_NhlFillStyleInfo	*fsp;

		/*
		 * The _NhlNwkGraphicStyle Private Resource MUST be set
		 * individually. As long as this stays private - and is
		 * set at each entry, it should be okay to point to 
		 * the line label string in the GraphicStyle object.
		 */
		if (num_args > 1){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:%s must be set alone!",entry_name,
					_NhlNwkGraphicStyle);
			return NhlFATAL;
		}
		if (newl->work.graphic_style_id == NhlNULLOBJID) {
			gsl = _NhlGetLayer(newl->work.def_graphic_style_id);
		}
		else {
			gsl = _NhlGetLayer(newl->work.graphic_style_id);
		}
		if (gsl == NULL || ! _NhlIsStyle(gsl)) {
			e_text = "%s: invalid GraphicStyle object id";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}

		NhlVAGetValues(gsl->base.id,
			       _NhlNgsLineStyleInfo,&lsp,
			       _NhlNgsFillStyleInfo,&fsp,
			       _NhlNgsMarkerStyleInfo,&msp,
			       NULL);

		newl->work.lip = lsp;
		newl->work.fip = fsp;
		newl->work.mip = msp;
		return NhlNOERROR;
	}
		    
	/*
	 * This function sets the private colormap based on the public
	 * resources.
	 */
	subret = DoCmap(newl,entry_name);
	retcode = MIN(retcode,subret);

	/*
	 * This function actually allocates the colors contained in the
	 * private colormap.
	 */
	subret = _NhlAllocateColors(new);
	retcode = MIN(retcode,subret);

	len1 = 0;
	if (newl->work.marker_table_params != oldl->work.marker_table_params) {
		subret = _NhlValidatedGenArrayCopy(
					   &(oldl->work.marker_table_params),
					   newl->work.marker_table_params,
						   4*8096,False,False,
						   _NhlNwkMarkerTableParams, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		if (subret > NhlWARNING) {
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
						 _NhlNwkMarkerTableStrings, 
						 entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		if (subret > NhlWARNING) {
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
			subret = NhlSetMarker(new->base.id,i+1,mstr,x,y,asp,
									size);
			if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		}
		else {
			i = NhlNewMarker(new->base.id,mstr,x,y,asp,size);
			retcode = (NhlErrorTypes)MIN(retcode,i);
			if(retcode < NhlWARNING)
				return retcode;
		}
	}
	
/*
 * Set the private line label resources
 */

        if((oldl->work.private_lineinfo.line_label_string !=
	    newl->work.private_lineinfo.line_label_string)) {

		if (oldl->work.private_lineinfo.line_label_string != NULL)
			NhlFree(oldl->work.private_lineinfo.line_label_string);

                if(newl->work.private_lineinfo.line_label_string != NULL) {
                        tmp = (char*)NhlMalloc((unsigned)
		      strlen(newl->work.private_lineinfo.line_label_string)+1);
                        strcpy(tmp,
			       newl->work.private_lineinfo.line_label_string);
                        newl->work.private_lineinfo.line_label_string = tmp;
                }
        }

	if(newl->work.private_lineinfo.line_label_const_spacing < 0.0)
		newl->work.private_lineinfo.line_label_const_spacing = 0.0;

/*
 * The "wk" line and marker attributes are now obsolete but for now
 * if they are set they will affect the default graphic style.
 */
	{
		_NhlLineStyleInfo *lsp = &newl->work.public_lineinfo;
		_NhlLineStyleInfo *olsp = &oldl->work.public_lineinfo;
		_NhlMarkerStyleInfo *msp = &newl->work.public_markinfo; 
		_NhlMarkerStyleInfo *omsp = &newl->work.public_markinfo; 
		NhlSArg		sargs[128];
		int		nargs = 0;
		
		if (lsp->dash_pattern != olsp->dash_pattern) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineDashPattern,lsp->dash_pattern);
		}
		
		if (lsp->line_dash_seglen != olsp->line_dash_seglen) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineDashSegLenF,
				   lsp->line_dash_seglen);
		}
	
		if (lsp->line_color != olsp->line_color) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineColor,lsp->line_color);
		}
		if (lsp->line_thickness != olsp->line_thickness) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineThicknessF,lsp->line_thickness);
		}
		if (lsp->line_label_string != olsp->line_label_string) {
			NhlSetSArg(&sargs[nargs++],
				NhlNgsLineLabelString,lsp->line_label_string);
		}
		if (lsp->line_label_font != olsp->line_label_font) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFont,lsp->line_label_font);
		}

		if (lsp->line_label_font_color != 
		    olsp->line_label_font_color) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontColor,
				   lsp->line_label_font_color);
		}
		if (lsp->line_label_font_height != 
		    olsp->line_label_font_height) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontHeightF,
				   lsp->line_label_font_height);
		}
		if (lsp->line_label_font_aspect != 
		    olsp->line_label_font_aspect) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontAspectF,
				   lsp->line_label_font_aspect);
		}
		if (lsp->line_label_font_thickness != 
		    olsp->line_label_font_thickness) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontThicknessF,
				   lsp->line_label_font_thickness);
		}
		if (lsp->line_label_font_quality != 
		    olsp->line_label_font_quality) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFontQuality,
				   lsp->line_label_font_quality);
		}
		if (lsp->line_label_const_spacing != 
		    olsp->line_label_const_spacing) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelConstantSpacingF,
				   lsp->line_label_const_spacing);
		}
		if (lsp->line_label_func_code != olsp->line_label_func_code) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsLineLabelFuncCode,
				   lsp->line_label_func_code);
		}
		if (msp->marker_index != omsp->marker_index) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerIndex,msp->marker_index);
		}
		if (msp->marker_color != omsp->marker_color) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerColor,msp->marker_color);
		}
		if (msp->marker_size != omsp->marker_size) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerSizeF,msp->marker_size);
		}
		if (msp->marker_thickness != omsp->marker_thickness) {
			NhlSetSArg(&sargs[nargs++],
				   NhlNgsMarkerThicknessF,
				   msp->marker_thickness);
		}
		if (nargs > 0) {
			subret = NhlALSetValues(
					   newl->work.def_graphic_style_id,
					   sargs,nargs);

			if ((retcode = MIN(subret,retcode)) < NhlWARNING) {
				e_text = 
				    "%S: error setting default GraphicStyle";
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  e_text,entry_name);
				return NhlFATAL;
			}
			e_text = 
"%s: Obsolete Workstation line or marker resources set: use GraphicStyle resources instead";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			retcode = MIN(retcode,NhlWARNING);
		}
	}


	return(retcode);
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
 *		NhlNwkForegroundColor
 *		_NhlNwkMarkerTableStrings
 *		_NhlNwkMarkerTableParams
 *		_NhlNwkDashTable
 *	The user is responsible for freeing this memory.
 */
static NhlErrorTypes
WorkstationGetValues
#if NhlNeedProto
(
	NhlLayer	l,
	_NhlArgList	args,
	int		num_args
)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlWorkstationLayer wl = (NhlWorkstationLayer)l;
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
			for(j = 0; j< wl->work.color_map_len; j++) {
				tmp[j][0] = private[j].red;
				tmp[j][1] = private[j].green;
				tmp[j][2] = private[j].blue;
			}
			count[0] = wl->work.color_map_len;
			count[1] = 3;
			if ((ga = NhlCreateGenArray((NhlPointer)tmp,
						    NhlTFloat,sizeof(float),
						    2,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNwkColorMap);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
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
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNwkBackgroundColor);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;

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
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,NhlNwkForegroundColor);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;

		} else if (args[i].quark == marker_tbl_strings_name) {
			if ((s_p = (NhlString *) 
			     NhlMalloc(wl->work.marker_table_len *
				       sizeof(NhlString))) == NULL) {
				e_text = "%s: error allocating %s data";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,
					  _NhlNwkMarkerTableStrings);
				return NhlFATAL;
			}
			for (j=0; j<wl->work.marker_table_len; j++) {
				if ((s_p[j] = (char *) NhlMalloc(strlen(
				   marker_table[j+1]->marker) + 1)) == NULL) {
				       e_text = "%s: error allocating %s data";
				       NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						 entry_name,
						 _NhlNwkMarkerTableStrings);
				       return NhlFATAL;
			        }
				strcpy(s_p[j], marker_table[j+1]->marker);
			}
			count[0] = wl->work.marker_table_len;
			if ((ga = NhlCreateGenArray((NhlPointer)s_p,
						    NhlTString,
						    sizeof(NhlString),
						    1,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,
					  _NhlNwkMarkerTableStrings);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;

		} else if (args[i].quark == marker_tbl_params_name) {
			if ((mtp_p = (NhlMarkerTableParams *)
			     NhlMalloc(wl->work.marker_table_len *
				    sizeof(NhlMarkerTableParams))) == NULL) {
				e_text = "%s: error allocating %s data";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,_NhlNwkMarkerTableParams);
				return NhlFATAL;
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
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,_NhlNwkMarkerTableParams);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;

		} else if (args[i].quark == dash_table_name) {
			if ((s_p = (NhlString *) 
			     NhlMalloc(dash_table_len *
				       sizeof(NhlString))) == NULL) {
				e_text = "%s: error allocating %s data";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,_NhlNwkDashTable);
				return NhlFATAL;
			}
			for (j=0; j<dash_table_len; j++) {
				if ((s_p[j] = (char *) NhlMalloc(strlen(
				   dash_patterns[j])+1)) == NULL) {
				       e_text = "%s: error allocating %s data";
				       NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						 entry_name,_NhlNwkDashTable);
				       return NhlFATAL;
			        }
				strcpy(s_p[j], dash_patterns[j]);
			}
			count[0] = dash_table_len;
			if ((ga = NhlCreateGenArray((NhlPointer)s_p,
						    NhlTString,
						    sizeof(NhlString),
						    1,count)) == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,_NhlNwkDashTable);
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}

	}
	return(ret);
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
#if	NhlNeedProto
(
	NhlLayer	inst
)
#else
(inst)
	NhlLayer	inst;
#endif
{
	NhlWorkstationLayerPart	*wp = &((NhlWorkstationLayer)inst)->work;
	NhlWorkstationClassPart	*wcp =
                &((NhlWorkstationClass)inst->base.layer_class)->work_class;
	NhlErrorTypes	retcode = NhlNOERROR;

        (*wcp->current_wks_count)--;

	NhlFreeGenArray(wp->marker_table_strings);
	NhlFreeGenArray(wp->marker_table_params);

	if(wp->private_lineinfo.line_label_string != NULL)
		NhlFree(wp->private_lineinfo.line_label_string);

	if (_NhlGetLayer(wp->def_graphic_style_id) != NULL) {
		NhlDestroy(wp->def_graphic_style_id);
	}

	return(retcode);
}

/*
 * Function:	DrawChildren
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
DrawChildren
#if	NhlErrorTypes
(
	_NhlAllChildList	children
)
#else
(children)
	_NhlAllChildList	children;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR,ret1 = NhlNOERROR;

	if(!children)
		return NhlNOERROR;

	if(!_NhlIsPlotMember(children->pid) && NhlIsView(children->pid))
		ret = NhlDraw(children->pid);

	ret1 = DrawChildren(children->next);

	return MIN(ret,ret1);
}

/*
 * Function:	WorkstationDraw
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationDraw
#if	NhlNeedProto
(
	NhlLayer	layer
)
#else
(layer)
	NhlLayer	layer;
#endif
{
	/*
	 * Call draw on all children...
	 */
	return DrawChildren(layer->base.all_children);
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
static NhlErrorTypes
WorkstationOpen
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{	
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "WorkstationOpen";
	int			i = 3; /* default segment wks is 2 */

	if(wl->work.gkswkstype == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unknown workstation type");
		return(NhlFATAL);
		
	} 
	if(wl->work.gkswksconid == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unknown workstation connection id");
		return(NhlFATAL);
	}
	while(wksisopn(i)) {
		i++;
	}
	wl->work.gkswksid = i;

	_NHLCALLF(gopwk,GOPWK)(&(wl->work.gkswksid),&(wl->work.gkswksconid),
		&(wl->work.gkswkstype));
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
	gset_clip_ind(GIND_NO_CLIP);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func)){
		return NhlFATAL;
	}

	return _NhlAllocateColors(l);
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
static NhlErrorTypes
WorkstationClose
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "WorkstationClose";
	NhlErrorTypes retcode = NhlNOERROR;

	if(wksisact(wl->work.gkswksid)){
		NhlPError(NhlINFO,NhlEUNKNOWN,
"%s:workstation should be deactivated, deactivating workstation now",func);
		_NhlDeactivateWorkstation(l);
	} 
	if(!wksisopn(wl->work.gkswksid)) {
		NhlPError(NhlINFO,NhlEUNKNOWN,
			"%s workstation already closed",func);
		retcode = NhlINFO;
	} else {
		gclose_ws(wl->work.gkswksid);
		if(_NhlLLErrCheckPrnt(NhlINFO,func))
			retcode = NhlINFO;
	}

	return	retcode;
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
static NhlErrorTypes
WorkstationActivate
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "WorkstationActivate";

	if(!wksisopn(wl->work.gkswksid)){
		/*
		 * ERROR WORKSTATION IS NOT OPEN INITILIZATION FAILED
		 */
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:can't activate an unopened workstation",func);
		return NhlFATAL;
	}

	if(wksisact(wl->work.gkswksid)){
		/*
		 * WORKSTATION IS ALREADY ACTIVE
		 */
		NhlPError(NhlINFO,NhlEUNKNOWN,
				"%s:called on already active workstation",func);
		return NhlINFO; 
	}

#if DEBUG_NCGM
	fprintf(stderr,"calling gacwk\n");
#endif
	c_ngseti("cl",1);
	gactivate_ws(wl->work.gkswksid);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;

	return NhlNOERROR;
}

/*
 * Function:	WorkstationDeactivate
 *
 * Description:	Deactivates workstation. if not open NhlFATAL error if not
 *		active informational message.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes
WorkstationDeactivate
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	char			func[] = "WorkstationDeactivate";
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	NhlErrorTypes		retcode = NhlNOERROR;

	if(wksisopn(wl->work.gkswksid)&&wksisact(wl->work.gkswksid)){
#if DEBUG_NCGM
	fprintf(stderr,"calling gdawk\n");
#endif
		gdeactivate_ws(wl->work.gkswksid);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func))
			retcode = NhlWARNING;
	}
	else{
/*
* ERROR WORKSTATION NOT ACTIVE OR NOT INITIALIZED
*/
		NhlPError(NhlWARNING,NhlEUNKNOWN,"WorkstationDeactivate: workstation not active or not opened");
		retcode = NhlWARNING;
	}

	return retcode;
}

/*
 * Function:	WorkstationAllocateColors
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
static NhlErrorTypes
WorkstationAllocateColors
#if  NhlNeedProto
(
	NhlLayer l
)
#else
(l)
	NhlLayer l;
#endif
{
	char			func[] = "WorkstationAllocateColors";
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	NhlWorkstationClassPart	*wcp =
		&((NhlWorkstationClass)wl->base.layer_class)->work_class;
	Gcolr_rep		tcrep;
	int			i, max_col = 0;
	NhlPrivateColor		*pcmap;
	NhlErrorTypes		ret = NhlNOERROR;

	pcmap = wl->work.private_color_map;
	if (! wl->work.cmap_changed)
		return NhlNOERROR;

	for ( i = 0; i < _NhlMAX_COLOR_MAP; i++) {
		switch(pcmap[i].cstat){
			case _NhlCOLNEW:
			case _NhlCOLCHANGE:
				tcrep.rgb.red = pcmap[i].red;
				tcrep.rgb.green = pcmap[i].green;
				tcrep.rgb.blue= pcmap[i].blue;
				gset_colr_rep(wl->work.gkswksid,i,&tcrep);
				if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
					ret = NhlWARNING;
					pcmap[i].cstat = _NhlCOLUNSET;
				}
				else {
					pcmap[i].cstat = _NhlCOLSET;
					pcmap[i].ci = i;
					max_col = i;
				}
				break;

			case _NhlCOLREMOVE:
				pcmap[i].cstat = _NhlCOLUNSET;
			case _NhlCOLUNSET:
				pcmap[i].red = -1.0;
				pcmap[i].green = -1.0;
				pcmap[i].blue = -1.0;
				break;

			case _NhlCOLSET:
				max_col = i;
				break;
		}
	}

	/*
	 * The background and the foreground MUST be defined.
	 */
	if(pcmap[NhlBACKGROUND].cstat == _NhlCOLUNSET){
		tcrep.rgb.red = pcmap[NhlBACKGROUND].red =
							wcp->def_background[0];
		tcrep.rgb.green = pcmap[NhlBACKGROUND].green =
							wcp->def_background[1];
		tcrep.rgb.blue = pcmap[NhlBACKGROUND].blue =
							wcp->def_background[2];
		gset_colr_rep(wl->work.gkswksid,NhlBACKGROUND,&tcrep);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
			ret = NhlWARNING;
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Problems setting Background:Undefined results",
				func);
		}
		pcmap[NhlBACKGROUND].cstat = _NhlCOLSET;
	}

	if(pcmap[NhlFOREGROUND].cstat == _NhlCOLUNSET){
		if (pcmap[NhlBACKGROUND].red * pcmap[NhlBACKGROUND].red +
		    pcmap[NhlBACKGROUND].green * pcmap[NhlBACKGROUND].green +
		    pcmap[NhlBACKGROUND].blue * pcmap[NhlBACKGROUND].blue
		    < .75) {
			tcrep.rgb.red = pcmap[NhlFOREGROUND].red =
			tcrep.rgb.green = pcmap[NhlFOREGROUND].green =
			tcrep.rgb.blue = pcmap[NhlFOREGROUND].blue = 1.0;
		}
		else {
			tcrep.rgb.red = pcmap[NhlFOREGROUND].red =
			tcrep.rgb.green = pcmap[NhlFOREGROUND].green =
			tcrep.rgb.blue = pcmap[NhlFOREGROUND].blue = 0.0;
		}
		gset_colr_rep(wl->work.gkswksid,NhlFOREGROUND,&tcrep);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
			ret = NhlWARNING;
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Problems setting Foreground:Undefined results",
				func);
		}
		pcmap[NhlFOREGROUND].cstat = _NhlCOLSET;
	}

	max_col = MAX(max_col,1);
	wl->work.color_map_len = max_col + 1;
	wl->work.cmap_changed = False;
	
	return ret;
}

/*
 * Function:	WorkstationUpdate
 *
 * Description:	This function is used to update the workstation
 *
 * In Args:	
 *		NhlLayer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationUpdate
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	char	func[] = "WorkstationUpdate";

#if DEBUG_NCGM
	fprintf(stderr,"calling guwk\n");
#endif
	gupd_ws(_NhlWorkstationId(l),GFLAG_PERFORM);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;

	return NhlNOERROR;
}

/*
 * Function:	WorkstationClear
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:	
 *		NhlLayer	l	workstation layer to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
WorkstationClear
#if	NhlNeedProto
(
	NhlLayer	l	/* workstation layer to update	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to update	*/
#endif
{
	char	func[] = "WorkstationClear";

#if DEBUG_NCGM
	fprintf(stderr,"calling gclrwk\n");
#endif

	gclear_ws(_NhlWorkstationId(l),GFLAG_ALWAYS);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;

	return NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes
WorkstationLineTo
#if  NhlNeedProto
(
	NhlLayer	wl,
	float		x,
	float		y,
	int		upordown
)
#else
(l,x,y,upordown)
	NhlLayer	wl;
	float		x;
	float		y;
	int		upordown;
#endif
{
	char		func[] = "WorkstationLineTo";
	static float	lastx,lasty;
	static int	first = 0;

	if(upordown == 1) {
		lastx = x;
		lasty = y;
		c_dpdraw(0.0,0.0,2);
		first = 1;
	} else {
		if(first) {
			c_dpdraw(lastx,lasty,0);
			first = 0;
		}
		c_dpdraw(x,y,1);
		lastx = x;
		lasty = y;
	}
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;
	return NhlNOERROR;
}


static NhlErrorTypes
WorkstationFill
#if  NhlNeedProto
(
	NhlLayer	l,
	float		*x,
	float		*y,
	int		num_points
)
#else
(l,x,y,num_points)
	NhlLayer	l;
	float		*x;
	float		*y;
	int		num_points;
#endif
{
	char			func[] = "WorkstationFill";
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	NhlWorkstationLayerPart	*wk_p = &wl->work;
	_NhlFillStyleInfo	*wkfp = wk_p->fip;
	static int		first = 1;
	static float		*dst;
	static int		*ind;
	static int		msize;
	static int		nst, nnd;
        float			fl,fr,fb,ft,ul,ur,ub,ut;
	int			ll, ix;
	Gfill_int_style		save_fillstyle;
	Gint			save_linecolor;
	Gint			save_linetype;
	Gdouble			save_linewidth;
	Gint			err_ind;
	Gint			fill_color;
	Gint			fill_background;
	
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			   "WorkstationFill: workspace allocation failed");
			return(NhlFATAL);
		}
	}
	else if (msize < num_points) {
		msize = num_points;
		nst = 2 * msize;
		nnd = 3 * msize;
		dst = (float *)NhlRealloc(dst, nst * sizeof(float));
		ind = (int *)NhlRealloc(ind, nnd * sizeof(int));
		if (dst == NULL || ind == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			    "WorkstationFill: workspace allocation failed");
			return(NhlFATAL);
		}
	}

/*
 * Make the user space coincide with the NDC space for the
 * duration of the routine
 */
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
	c_set(fl,fr,fb,ft,fl,fr,fb,ft,1);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
/*
 * Save attributes that may be modified
 */
	ginq_line_colr_ind(&err_ind, &save_linecolor);
	ginq_linewidth(&err_ind, &save_linewidth);
	ginq_fill_int_style(&err_ind, &save_fillstyle);
	ginq_linetype(&err_ind, &save_linetype);
	fill_color = (wkfp->fill_color == NhlTRANSPARENT) ? NhlTRANSPARENT : 
		_NhlGetGksCi(l,wkfp->fill_color);
	fill_background = (wkfp->fill_background < 0) ?
		wkfp->fill_background : _NhlGetGksCi(l,wkfp->fill_background);

/*
 * Draw the fill, unless a negative fill index or Transparent fill color
 * is specified (implying no fill)
 */
	if (fill_color == NhlTRANSPARENT)
	/*SUPPRESS570*/
		;
	else if ((ix = wkfp->fill_index) == NhlSOLIDFILL) {
		/* fill_specs[ix].type  must be 0 */
		gset_fill_int_style(GSTYLE_SOLID);
		gset_linewidth(wkfp->fill_line_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_sfseti("type of fill", 0);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,fill_color);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}
	else if (ix > 0) {
		/* fill_specs[ix].type must not be 0 */
		ix = 1 + (ix - 1) % wk_p->fill_table_len;
		if (fill_background >= 0) {
			gset_linewidth(1.0);
			gset_fill_int_style(GSTYLE_SOLID);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			c_sfseti("type of fill", 0);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,
							fill_background);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		gset_linewidth(wkfp->fill_line_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_sfseti("TY", fill_specs[ix].type);
		c_sfseti("DO", fill_specs[ix].dots_on);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		if (fill_specs[ix].dots_on) {
			gset_marker_colr_ind(fill_color);
			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,-1);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		if (fill_specs[ix].type > 0) { 
 			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,fill_color);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		else {
			gset_line_colr_ind(fill_color);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
 			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,
				 fill_specs[ix].ici);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
	}

/*
 * Draw the edges
 */
	if (wkfp->edges_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(l,wkfp->edge_color));
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		gset_linewidth(wkfp->edge_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		if (wkfp->edge_dash_pattern > 0) {
			c_curved(x,y,num_points);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		else {
			c_curve(x,y,num_points);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
	}

/*
 * Restore state
 */

	gset_line_colr_ind(save_linecolor);
	gset_linewidth(save_linewidth);
	gset_fill_int_style(save_fillstyle);
	gset_linetype(save_linetype);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	return(NhlNOERROR);

}

/********************************
* Workstation Defined Functions	*
********************************/

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
int
_NhlWorkstationId
#if NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;

	return wl->work.gkswksid;
}

NhlErrorTypes
_NhlSetColor
#if	NhlNeedProto
(
	NhlLayer	l,
	int		ci,
	float		red,
	float		green,
	float		blue
)
#else
(l,ci,red,green,blue)
	NhlLayer	l;
	int		ci;
	float		red;
	float		green;
	float		blue;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "_NhlSetColor";

	if(ci == NhlTRANSPARENT){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:color index cannot be set: no allocation",func);
		return(NhlWARNING);
	}

	if (ci >= _NhlMAX_COLOR_MAP || ci < NhlBACKGROUND) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:invalid color index: no allocation",func);
		return(NhlWARNING);
	}

	if (red < 0.0 || red > 1.0 || green < 0.0 || green > 1.0 ||
		 blue < 0.0 || blue > 1.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		      "%s:a color component was invalid: no allocation",func);
		return(NhlWARNING);
	}

	wl->work.private_color_map[ci].red = red;
	wl->work.private_color_map[ci].green = green;
	wl->work.private_color_map[ci].blue = blue;
	if(wl->work.private_color_map[ci].cstat == _NhlCOLUNSET)
		wl->work.private_color_map[ci].cstat = _NhlCOLNEW;
	else
		wl->work.private_color_map[ci].cstat = _NhlCOLCHANGE;

	wl->work.cmap_changed = True;
	return _NhlAllocateColors(l);
}

NhlErrorTypes
NhlSetColor
#if	NhlNeedProto
(
	int	pid,
	int	ci,
	float	red,
	float	green,
	float	blue
)
#else
(pid,ci,red,green,blue)
	int     pid;
	int	ci;
	float	red;
	float	green;
	float	blue;
#endif
{
	char		func[]="NhlSetColor";
	NhlLayer	wl = _NhlGetLayer(pid);

	if(wl && _NhlIsWorkstation(wl))
		return _NhlSetColor(wl,ci,red,green,blue);

	NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation id=%d",func,pid);
	return NhlFATAL;
}

/*
 * Function:	nhlpfsetcolor
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfsetcolor,NHLPFSETCOLOR)
#if	NhlNeedProto
(
	int	*pid,
	int	*indx,
	float	*red,
	float	*green,
	float	*blue,
	int	*err
)
#else
(pid,indx,red,green,blue,err)
	int	*pid;
	int	*indx;
	float	*red;
	float	*green;
	float	*blue;
	int	*err;
#endif
{
	*err = NhlSetColor(*pid,*indx,*red,*green,*blue);

	return;
}

NhlErrorTypes
_NhlFreeColor
#if	NhlNeedProto
(
	NhlLayer	l,
	int		ci
)
#else
(l,ci)
	NhlLayer	l;
	int		ci;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char	func[] = "_NhlFreeColor";

	if((ci == NhlTRANSPARENT) || (ci >= _NhlMAX_COLOR_MAP) ||
		(ci < NhlBACKGROUND)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s:color index can not be freed",func);
		return NhlFATAL;
	}

	wl->work.private_color_map[ci].cstat = _NhlCOLREMOVE;

	wl->work.cmap_changed = True;
	return _NhlAllocateColors(l);
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
#if	NhlNeedProto
(
	int	pid,
	int	ci
)
#else
(pid ,ci)
	int 	pid;
	int	ci;
#endif
{
	NhlLayer	wl = _NhlGetLayer(pid);

	if(wl && _NhlIsWorkstation(wl))
		return _NhlFreeColor(wl,ci);

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"NhlFreeColor:Called with invalid Workstation id");
	return NhlFATAL;
}


/*
 * Function:	nhlpffreecolor
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpffreecolor,NHLPFFREECOLOR)
#if	NhlNeedProto
(
	int	*wid,
	int	*indx,
	int	*err
)
#else
(wid,indx,err)
	int	*wid;
	int	*indx;
	int	*err;
#endif
{
	*err = NhlFreeColor(*wid,*indx);

	return;
}

int
_NhlGetColor
#if	NhlNeedProto
(
        NhlLayer	l,
	int		ci,
        float		*red,
        float		*green,
        float		*blue
)
#else
(l,ci,red,green,blue)
        NhlLayer	l;
	int		ci;
        float		*red;
        float		*green;
        float		*blue;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "_NhlGetColor";
	NhlPrivateColor		*pcmap = wl->work.private_color_map;
	int			maxi = wl->work.color_map_len - 1;

	if(ci < 0){
		ci = NhlFOREGROUND;
	}

	if(ci > maxi){
		ci = ci % maxi;
		if(!ci)
			ci = maxi;
	}

	if(pcmap[ci].cstat == _NhlCOLUNSET){
		ci = NhlFOREGROUND;
	}

	*red = pcmap[ci].red;
	*green = pcmap[ci].green;
	*blue = pcmap[ci].blue;

	return ci;
}

int
NhlGetColor
#if	NhlNeedProto
(
        int		pid,
	int		ci,
        float		*red,
        float		*green,
        float		*blue
)
#else
(pid,ci,red,green,blue)
        int		pid;
	int		ci;
        float		*red;
        float		*green;
        float		*blue;
#endif
{
	char		func[]="NhlGetColor";
	NhlLayer	wl = _NhlGetLayer(pid);

	if(wl && _NhlIsWorkstation(wl))
	 	return _NhlGetColor(wl,ci,red,green,blue);	

	NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation id=%d",func,pid);
	return NhlFATAL;
}

int _NhlNewColor
#if   NhlNeedProto
(
	NhlLayer	l,
	float		red,
	float		green,
	float		blue
)
#else
(l,red,green,blue)
	NhlLayer	l,
	float		red,
	float		green,
	float		blue
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char		func[]="_NhlNewColor";
	int		i;
	NhlErrorTypes	ret;

	if (red < 0.0 || red > 1.0 || green < 0.0 || green > 1.0 ||
		 blue < 0.0 || blue > 1.0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:A color component was invalid; no allocation",func);
		return NhlFATAL;
	}

	for(i=2;i < _NhlMAX_COLOR_MAP;i++){
		if(wl->work.private_color_map[i].cstat == _NhlCOLUNSET){
			wl->work.private_color_map[i].cstat = _NhlCOLNEW;
			wl->work.private_color_map[i].red = red;
			wl->work.private_color_map[i].green = green;
			wl->work.private_color_map[i].blue = blue;
			wl->work.cmap_changed = True;
			ret = _NhlAllocateColors(l);

			if(ret < NhlINFO){
				NhlPError(ret,NhlEUNKNOWN,
					"%s:Problem allocating color",func);
				return ret;
			}
			return i;
		}
	}

	/*
	 * ERROR : no available colors
	 */
	NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:no available colors",func);
	return NhlFATAL ;
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
#if  NhlNeedProto
(
	int	pid,
	float	red,
	float	green,
	float	blue
)
#else
(pid,red,green,blue)
	int	pid,
	float	red,
	float	green,
	float	blue
#endif
{
	char		func[]="NhlNewColor";
	NhlLayer	wl = _NhlGetLayer(pid);

	if(wl && _NhlIsWorkstation(wl))
		return _NhlNewColor(wl,red,green,blue);

	NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation id=%d",func,pid);
	return NhlFATAL;
}

/*
 * Function:	nhlpfnewcolor
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfnewcolor,NHLPFNEWCOLOR)
#if	NhlNeedProto
(
	int	*wid,
	float	*red,
	float	*green,
	float	*blue,
	int	*indx
)
#else
(wid,red,green,blue,indx)
	int	*wid;
	float	*red;
	float	*green;
	float	*blue;
	int	*indx;
#endif
{
	*indx = NhlNewColor(*wid,*red,*green,*blue);

	return;
}

int _NhlGetGksCi
#if NhlNeedProto
(
	NhlLayer	l,
	int		ci
)
#else
(l, ci)
	NhlLayer	l;
	int		ci;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "_NhlGetGksCi";
	NhlPrivateColor		*pcmap = wl->work.private_color_map;
	int			maxi = wl->work.color_map_len - 1;

	if(ci < 0){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"%s:Invalid Color index=%d",
			func,ci);
		return pcmap[NhlFOREGROUND].ci;
	}

	if(ci > maxi){
		ci = ci % maxi;
		if(!ci)
			ci = maxi;
	}

	if(pcmap[ci].cstat == _NhlCOLUNSET)
		return pcmap[NhlFOREGROUND].ci;

	return pcmap[ci].ci;
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
int
NhlGetGksCi
#if NhlNeedProto
(
	int	pid,
	int	ci
)
#else
(pid,ci)
	int	pid;
	int	ci;
#endif
{
	char		func[]="NhlGetGksCi";
	NhlLayer	wl = _NhlGetLayer(pid);

	if(wl && _NhlIsWorkstation(wl))
	 	return _NhlGetGksCi(wl,ci);	

	NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation id=%d",func,pid);
	return NhlFATAL;
}

/*
 * Function:	nhlpfgetgksci
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfgetgksci,NHLPFGETGKSCI)
#if	NhlNeedProto
(
	int	*wid,
	int	*hluci,
	int	*gksci
)
#else
(wid,hluci,gksci)
	int	*wid;
	int	*hluci;
	int	*gksci;
#endif
{
	*gksci = NhlGetGksCi(*wid,*hluci);

	return;
}

/*
 * Function:	_NhlIsAllocatedColor
 *
 * Description: returns a Boolean value depending on whether a color is 
 *		currently allocated
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
int _NhlIsAllocatedColor
#if NhlNeedProto
( 
	NhlLayer	l,
	int		ci
)
#else
(l,ci)
	NhlLayer	l;
	int		ci;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[]="NhlIsAllocatedColor";

	if((ci < NhlTRANSPARENT) || (ci >= wl->work.color_map_len))
		return False;
	if (ci == NhlTRANSPARENT)
		return True;
	return (wl->work.private_color_map[ci].cstat == _NhlCOLSET);
}

/*
 * Function:	NhlIsAllocatedColor
 *
 * Description: returns a Boolean value depending on whether a color is 
 *		currently allocated
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlBoolean
NhlIsAllocatedColor
#if NhlNeedProto
(
	int	pid,
	int	ci
)
#else
(pid,ci)
	int	pid;
	int	ci;
#endif
{
	char		func[]="NhlIsAllocatedColor";
	NhlLayer	wl = _NhlGetLayer(pid);

	if(wl && _NhlIsWorkstation(wl))
		return _NhlIsAllocatedColor(wl,ci);
	
	NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation id=%d",func,pid);
	return False;
}

/*
 * Function:	nhlpfisallocatedcolor
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfisallocatedcolor,NHLPFISALLOCATEDCOLOR)
#if	NhlNeedProto
(
	int	*wid,
	int	*indx,
	int	*ret
)
#else
(wid,indx,ret)
	int	*wid;
	int	*indx;
	int	*ret;
#endif
{
	*ret = NhlIsAllocatedColor(*wid,*indx);

	return;
}

/*
 * Workstation Method Access Functions
 */
NhlErrorTypes
_NhlOpenWorkstation
#if NhlNeedProto
(
	NhlLayer	wks
)
#else
(wks)
	NhlLayer	wks;
#endif
{
	char				func[] = "_NhlOpenWorkstation";
	NhlErrorTypes			ret = NhlNOERROR;
	NhlWorkstationClassPart		*wc =
		&((NhlWorkstationClass)wks->base.layer_class)->work_class;
	NhlArgVal			sel,cbdata;

	if(!_NhlIsWorkstation(wks)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s: attempt to perform open on nonworkstation",func);
		return NhlFATAL;
	}

	NhlINITVAR(sel);
	NhlINITVAR(cbdata);
	cbdata.ptrval = wks;
	_NhlCallClassCallbacks(wks->base.layer_class,_NhlCBworkPreOpen,
								sel,cbdata);

	ret = (*(wc->open_work))(wks);
	if(ret != NhlFATAL){
		((NhlWorkstationLayer)wks)->work.open = True;
		(void)NhlUpdateWorkstation(wks->base.id);
	}

	return ret;
}

NhlErrorTypes
_NhlCloseWorkstation
#if NhlNeedProto
(
	NhlLayer	wks
)
#else
(wks)
	NhlLayer	wks;
#endif
{
	char				func[] = "_NhlCloseWorkstation";
	NhlWorkstationClassPart	*wc =
		&((NhlWorkstationClass)wks->base.layer_class)->work_class;

	if(!_NhlIsWorkstation(wks)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s: attempt to perform close on nonworkstation",func);
		return NhlFATAL;
	}

	if(!((NhlWorkstationLayer)wks)->work.open)
		return NhlNOERROR;

	return (*(wc->close_work))(wks);
}

/*
 * Function:	_NhlActivateWorkstation
 *
 * Description: _NhlActivateWorkstation calls the activate_work method.
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
NhlErrorTypes
_NhlActivateWorkstation
#if NhlNeedProto
(
	NhlLayer	wks
)
#else
(wks)
	NhlLayer	wks;
#endif
{
	char				func[] = "_NhlActivateWorkstation";
	NhlWorkstationClassPart	*wc =
		&((NhlWorkstationClass)wks->base.layer_class)->work_class;

	if(_NhlIsWorkstation(wks))
		return (*(wc->activate_work))(wks);

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: attempt to perform activate on nonworkstation",func);
	return NhlFATAL;
}

/*
 * Function:	_NhlDeactivateWorkstation
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
NhlErrorTypes
_NhlDeactivateWorkstation
#if NhlNeedProto
(
	NhlLayer	wks
)
#else
(wks)
	NhlLayer	wks;
#endif
{
	char				func[] = "_NhlDeactivateWorkstation";
	NhlWorkstationClassPart	*wc =
		&((NhlWorkstationClass)wks->base.layer_class)->work_class;

	if(_NhlIsWorkstation(wks))
		return (*(wc->deactivate_work))(wks);

	NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: attempt to perform deactivate on nonworkstation",func);
	return NhlFATAL;
}

/*
 * Function:	_NhlAllocateColors
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlErrorTypes
_NhlAllocateColors
#if	NhlNeedProto
(
	NhlLayer	wl
)
#else
(wl)
	NhlLayer	wl;
#endif
{
	NhlWorkstationClassPart	*wc =
		&((NhlWorkstationClass)wl->base.layer_class)->work_class;

	return (*(wc->alloc_colors))(wl);
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
#if	NhlNeedProto
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	NhlLayer		l = _NhlGetLayer(workid);
	NhlWorkstationClass	lc;

	if(l == (NhlLayer)NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to update Workstation with PID#%d",workid);
		return NhlFATAL;
	}

	if(!_NhlIsWorkstation(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"PID#%d is not a Workstation Class object",workid);
		return NhlFATAL;
	}

	lc = (NhlWorkstationClass)l->base.layer_class;

	return (*(lc->work_class.update_work))(l);
}

/*
 * Function:	nhlpfupdateworkstation
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfupdateworkstation,NHLPFUPDATEWORKSTATION)
#if	NhlNeedProto
(
	int	*wid,
	int	*err
)
#else
(wid,err)
	int	*wid;
	int	*err;
#endif
{
	*err = NhlUpdateWorkstation(*wid);

	return;
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
#if	NhlNeedProto
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	NhlLayer		l = _NhlGetLayer(workid);
	NhlWorkstationClass	lc;

	if(l == (NhlLayer)NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unable to clear Workstation with PID#%d",workid);
		return NhlFATAL;
	}

	if(!_NhlIsWorkstation(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"PID#%d is not a Workstation Class object",workid);
		return NhlFATAL;
	}

	lc = (NhlWorkstationClass)l->base.layer_class;

	return (*(lc->work_class.clear_work))(l);
}

/*
 * Function:	nhlpfclearworkstation
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfclearworkstation,NHLPFCLEARWORKSTATION)
#if	NhlNeedProto
(
	int	*wid,
	int	*err
)
#else
(wid,err)
	int	*wid;
	int	*err;
#endif
{
	*err = NhlClearWorkstation(*wid);

	return;
}

/*
 * Function:	NhlFrame
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlErrorTypes	NhlFrame
#if	NhlNeedProto
(
	int	wid
)
#else
(wid)
	int	wid;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes ret1 = NhlNOERROR;

	ret = NhlUpdateWorkstation(wid);
	ret1 = NhlClearWorkstation(wid);
	return(MIN(ret,ret1));
}

/*
 * Function:	nhlpfframe
 *
 * Description:	Fortran referencable Frame call
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	private Fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlpfframe,NHLPFFRAME)
#if	NhlNeedProto
(
	int	*wid,
	int	*err
)
#else
(wid,err)
	int	*wid;
	int	*err;
#endif
{
	*err = NhlFrame(*wid);

	return;
}

/*ARGSUSED*/
void
_NhlSetLineInfo
#if  NhlNeedProto
(
	NhlLayer	l,
	NhlLayer	plot
)
#else
(l,plot)
	NhlLayer	l;
	NhlLayer	plot;
#endif
{
	char			func[] = "_NhlSetLineInfo";
	char			*e_text;
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
        NhlWorkstationLayerPart	*wkp = &(wl)->work;
	_NhlLineStyleInfo	*wklp = wkp->lip;
	float			tf;
        char			buffer[80];
	int			buff_size = sizeof(buffer) - 1;
	int			i,j,ix;
	char			*tchar;

 
/*
 * Flush plotif buffer...
 */

	c_sflush();
#if 0
	c_plotif(0.0,0.0,2);
#endif

/*
 * Since Ezmap does not currently use dashpack, map data polylines and 
 * polygon edges both are controlled using dashchar routines. For now,
 * therefore in order to ensure that map polylines do not end up with edge
 * attributes it is necessarry to call both the dashchar and dashpack
 * routines. Hopefully the dashchar stuff (between the curly braces
 * below) can be eliminated when Ezmap gets updated.
 */
	{
		float fl,fr,fb,ft,ul,ur,ub,ut;
		float x0,x1;
		int ix,ll;
		int dollar_size;

		memset((void *) buffer, (char) 0, 80 * sizeof(char));

		c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
		if(_NhlLLErrCheckPrnt(NhlFATAL,func))
			return;

		x0 = fl;
		x1 = fl + wklp->line_dash_seglen;
		x0 = (float)c_kfpy(x0);
		x1 = (float)c_kfpy(x1);
	
		if ((ix = wklp->dash_pattern) > wkp->dash_table_len) {
			/* NhlINFO - but it's a void function right now */
			NhlPError(NhlINFO,NhlEUNKNOWN,
	"_NhlSetLineInfo: using mod function on dash pattern index: %d", ix);

			ix = 1 + (ix - 1) % wkp->dash_table_len;
		}
		
		dollar_size = (x1 - x0) /
			strlen(dash_patterns[ix]) + 0.5;
		if(dollar_size < 1) dollar_size = 1;
		
		strcpy(buffer,dash_patterns[ix]);
		
		c_dashdc(buffer,dollar_size,1);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

	memset((void *) buffer,'\0', sizeof(buffer)*sizeof(char));

	c_pcseti("FN",0);
	c_pcseti("CL",1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	/*
	 * Reset DashPack so we know what state we are starting from.
	 */
	_NHLCALLF(dprset,DPRSET)();
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	c_dpsetc("CRG","'");
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if ((ix = wklp->dash_pattern) < 0) {
		/* NhlWARNING - but it's a void function right now */
		e_text = "%s: invalid dash pattern index";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,func);
		ix = wklp->dash_pattern = NhlSOLIDLINE;
	}
	else if (ix > wkp->dash_table_len) {
		/* NhlINFO - but it's a void function right now */
		e_text = "%s: using mod function on dash pattern index: %d";
		NhlPError(NhlINFO,NhlEUNKNOWN,e_text,func,ix);
		ix = 1 + (ix - 1) % wkp->dash_table_len; 
	}
        strncpy(buffer,dash_patterns[ix],buff_size);

	tf = wklp->line_dash_seglen / (strlen(buffer)+.5);
	c_dpsetr("WOG",tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOS",tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if(wklp->line_color == NhlTRANSPARENT){
		i=0;
		while(buffer[i] != '\0'){
			buffer[i] = '\'';
			i++;
		}
	}
	else{
	        gset_line_colr_ind((Gint)_NhlGetGksCi(
			    plot->base.wkptr,wklp->line_color));
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

        if (wklp->line_label_string != NULL) {
		float	pwidth, pheight, height;
		char	fcode[2];

		buff_size = sizeof(buffer) - strlen(buffer) - 1;
		tchar = &buffer[strlen(buffer)];
		if(wklp->line_label_font_color == NhlTRANSPARENT){
			/*
			 * Put spaces in for label.
			 */
			j = MIN(strlen(wklp->line_label_string) * 2 + 1,buff_size);
			for(i=0;i < j-1;i+=2){
				tchar[i] = ' ';
				tchar[i+1] = '|';
			}
		}
		else{
			/*
			 * Add breaks in at each space of the label.
			 */
			i=0;
			j=0;
			while(i < buff_size && wklp->line_label_string[j] != '\0'){
				if(wklp->line_label_string[j] == ' ')
					tchar[i++] = '|';
				tchar[i++] = wklp->line_label_string[j++];
			}
			
			c_pcseti("CC",_NhlGetGksCi(plot->base.wkptr,
						wklp->line_label_font_color));
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

		}

		if (wklp->line_label_font_aspect <= 0.0 ) {
			/* NhlWARNING - but it's a void function right now */
			wklp->line_label_font_aspect = 1.3125;
			e_text = "%s: Aspect ratio cannot be zero or negative";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,func);
		}
		if(wklp->line_label_font_aspect <= 1.0) {
			pheight = 21.0 * wklp->line_label_font_aspect;
			pwidth = 21.0;
		} else {
			pwidth = 21.0 * 1.0 / wklp->line_label_font_aspect;
			pheight = 21.0;
		}
		/*
		 * The 1.125 factor compensates for the PLOTCHAR 'SA' parameter
		 */
		height = 1.125 * wklp->line_label_font_height * 
			(1.0 / wklp->line_label_font_aspect);

		c_dpsetr("WOC",height);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_pcsetr("PH",pheight);
		c_pcsetr("PW",pwidth);
		c_pcseti("CS",wklp->line_label_const_spacing);
		c_pcseti("FN",wklp->line_label_font);
		c_pcseti("QU",wklp->line_label_font_quality);
		/*
		 * Make sure GKS is using font 1. - Since plotchar uses
		 * GTX when quality is NhlLOW.
		 */
		if(wklp->line_label_font_quality == NhlLOW){
			Gtext_font_prec	gtfp;
			gtfp.font = 1;
			gtfp.prec = GPREC_STROKE;
			gset_text_font_prec(&gtfp);
		}
		fcode[0] = wklp->line_label_func_code;
		fcode[1] = '\0';
		c_pcsetc("FC",fcode);
		c_pcsetr("CL",wklp->line_label_font_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
        }

	c_dpsetc("DPT",buffer);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

        gset_linewidth(wklp->line_thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);


        return;
}

NhlErrorTypes
_NhlWorkstationLineTo
#if NhlNeedProto
(
	NhlLayer	l,
	float		x,
	float		y,
	int		upordown
)
#else
(l,x,y,upordown)
	NhlLayer	l;
	float		x;
	float		y;
	int		upordown;
#endif
{
	NhlWorkstationClassPart *wcp =
			&((NhlWorkstationClass)l->base.layer_class)->work_class;

	return (*wcp->lineto_work)(l,x,y,upordown);
}


/*ARGSUSED*/
void
_NhlSetFillInfo
#if  NhlNeedProto
(
	NhlLayer	l,
	NhlLayer	plot
)
#else
(l,plot)
	NhlLayer	l;
	NhlLayer	plot;
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;
	char			func[] = "_NhlSetFillInfo";
	NhlWorkstationLayerPart	*wk_p = &wl->work;
	_NhlFillStyleInfo	*wkfp = wk_p->fip;
        float			fl,fr,fb,ft,ul,ur,ub,ut;
        float			x0,x1;
        int			ll,ix;
        char			buffer[80];
	

	if (wkfp->edges_on && wkfp->edge_dash_pattern < 0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "_NhlSetFillInfo: invalid edge dash pattern index");
		wkfp->edge_dash_pattern = NhlSOLIDLINE;
	}
	else if (wkfp->edges_on && wkfp->edge_dash_pattern > 0) {
		memset((void *) buffer, (char) 0, 80 * sizeof(char));

#if 0
		c_sflush();
#endif
		c_plotif(0.0,0.0,2);

		c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
		if(_NhlLLErrCheckPrnt(NhlFATAL,func))
			return;

		x0 = fl;
		x1 = fl + wkfp->edge_dash_seglen;
		x0 = (float)c_kfpy(x0);
		x1 = (float)c_kfpy(x1);
	
		if ((ix = wkfp->edge_dash_pattern) > wk_p->dash_table_len) {
			/* NhlINFO - but it's a void function right now */
			NhlPError(NhlINFO,NhlEUNKNOWN,
	"_NhlSetFillInfo: using mod function on dash pattern index: %d", ix);

			ix = 1 + (ix - 1) % wk_p->dash_table_len;
		}
		
		wk_p->edge_dash_dollar_size = (x1 - x0) /
			strlen(dash_patterns[ix]) + 0.5;
		if(wk_p->edge_dash_dollar_size < 1)
                        wk_p->edge_dash_dollar_size = 1;
		
		strcpy(buffer,dash_patterns[ix]);
		
		c_dashdc(buffer,wk_p->edge_dash_dollar_size,1);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}
		
/*
 * Make sure the scale factor is okay
 */
	if (wkfp->fill_scale_factor <= 0.0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"_NhlSetFillInfo: fill scale factor must be greater than 0.0");
		wkfp->fill_scale_factor = 1.0;
	}
		
/*
 * An out-of-bounds fill index should have been caught at a higher
 * level. 
 */

	if ((ix = wkfp->fill_index) < NhlHOLLOWFILL) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			   "_NhlSetFillInfo: invalid fill index");
		wkfp->fill_index = NhlHOLLOWFILL;
		
	}
	else if (ix > wk_p->fill_table_len) {
		/* NhlINFO - but it's a void function right now */
		NhlPError(NhlINFO,NhlEUNKNOWN,
	 "_NhlSetLineInfo: using mod function on fill index: %d", ix);

		ix = 1 + (ix - 1) % wk_p->fill_table_len;
	}

	if (ix != NhlHOLLOWFILL) {
		c_sfseti("AN", fill_specs[ix].angle);
		(void)_NhlLLErrCheckPrnt(NhlINFO,func);
		c_sfsetr("SP", fill_specs[ix].spacing * 
			 wkfp->fill_scale_factor);
		(void)_NhlLLErrCheckPrnt(NhlINFO,func);
	}

        return;
}

NhlErrorTypes
_NhlWorkstationFill
#if NhlNeedProto
(
	NhlLayer	instance,
	float		*x,
	float		*y,
	int		num_points
)
#else
(instance,x,y,num_points)
	NhlLayer	instance;
	float		*x;
	float		*y;
	int		num_points;
#endif
{
	NhlWorkstationClassPart *wcp =
	&((NhlWorkstationClass)instance->base.layer_class)->work_class;

	return (*wcp->fill_work)(instance,x,y,num_points);
}

/*
 * Adds a marker definition to the marker table and returns an index to
 * this marker. 
 */
/*ARGSUSED*/
int NhlNewMarker
#if  NhlNeedProto
(
	int	wid, 
	char	*mark_string, 
	float	x_off, 
	float	y_off,
	float	aspect_adj,
	float	size_adj
)
#else
(wid,mark_string,x_off,y_off,aspect_adj,size_adj)
	int	wid;
	char	*mark_string;
	float	x_off;
	float	y_off;
	float	aspect_adj;
	float	size_adj;
#endif
{
        NhlWorkstationLayer tinst = (NhlWorkstationLayer)_NhlGetLayer(wid);
	NhlWorkstationLayerPart *wk_p;
	NhlMarkerSpec *m_p;
	int i;

	if((tinst == NULL) || !_NhlIsWorkstation(tinst)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlNewMarker:Invalid workstation id = %d",wid);
		return ((int)NhlFATAL);
	}
	
	wk_p = &tinst->work;

	if (marker_table_len == marker_table_alloc_len) {
		marker_table_alloc_len += NhlWK_ALLOC_UNIT;
		marker_table = (NhlMarkerTable) 
			NhlRealloc(marker_table, 
				   marker_table_alloc_len *
				   sizeof(NhlMarkerSpec *));
		if (marker_table == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			     "_NhlNewMarker: marker table realloc failed");
			return((int)NhlFATAL);
		}
		m_p = (NhlMarkerSpec *)
			NhlMalloc(NhlWK_ALLOC_UNIT * sizeof(NhlMarkerSpec));
		if (m_p == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			     "_NhlNewMarker: marker specs alloc failed");
			return((int)NhlFATAL);
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

	if (mark_string == NULL) {
		mark_string = marker_table[NhlWK_DEF_MARKER]->marker;
        }
	if ((m_p->marker = NhlMalloc(strlen(mark_string) + 1)) == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "_NhlNewMarker: marker string alloc failed");
		return((int)NhlFATAL);
	}
	strcpy(m_p->marker, mark_string);
	
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
 * Function:	nhlpfnewmarker
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfnewmarker,NHLPFNEWMARKER)
#if	NhlNeedProto
(
	int		*wid,
	_NhlFString	fmark,
	int		*fmark_len,
	float		*xoff,
	float		*yoff,
	float		*aspadj,
	float		*sizeadj,
	int		*indx_ret
)
#else
(wid,fmark,fmark_len,xoff,yoff,aspadj,sizeadj,indx_ret)
	int		*wid;
	_NhlFString	fmark;
	int		*fmark_len;
	float		*xoff;
	float		*yoff;
	float		*aspadj;
	float		*sizeadj;
	int		*indx_ret;
#endif
{
	char	tstr[_NhlMAXMARKERLEN];

	if(!_NhlFstrToCstr(tstr,NhlNumber(tstr),fmark,*fmark_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Can't convert Fortran string to C string");
		*indx_ret = NhlFATAL;
		return;
	}

	*indx_ret = NhlNewMarker(*wid,tstr,*xoff,*yoff,*aspadj,*sizeadj);

	return;
}

/*
 * Allows modification of the characteristics of an existing marker, 
 * whether pre-defined or added using NhlNewMarker.
 */
/*ARGSUSED*/
NhlErrorTypes
NhlSetMarker
#if  NhlNeedProto
(int instance, 
 int	index,
 char	*mark_string, 
 float	x_off, 
 float	y_off,
 float	aspect_adj,
 float	size_adj)
#else
(instance,index,mark_string,x_off,y_off,aspect_adj,size_adj)
        int instance;
	int   index;
	char *mark_string; 
	float x_off; 
	float y_off;
	float aspect_adj;
	float size_adj;
#endif
{
	NhlMarkerSpec *m_p;
	char *c_p;

	if (index <= 0 || index > marker_table_len) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "_NhlEditMarker: invalid marker index");
		return(NhlWARNING);
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			      "_NhlEditMarker: marker alloc failed");
			return(NhlFATAL);
		}
		memcpy((char *) m_p, (char *) marker_table[index],
			sizeof(NhlMarkerSpec));
		marker_table[index] = m_p;
	}
		
	if (mark_string != NULL && 
	    strcmp(mark_string, m_p->marker)) {
		    if ((c_p = NhlMalloc(strlen(mark_string)+ 1 )) == NULL) {
			    NhlPError(NhlFATAL,NhlEUNKNOWN,
				 "_NhlEditMarker: marker string alloc failed");
			    return(NhlFATAL);
		    }
		    strcpy(c_p, mark_string);
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

	return (NhlNOERROR); 
	
}

/*
 * Function:	nhlpfsetmarker
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfsetmarker,NHLPFSETMARKER)
#if	NhlNeedProto
(
	int		*wid,
	int		*indx,
	_NhlFString	fmark,
	int		*fmark_len,
	float		*xoff,
	float		*yoff,
	float		*aspadj,
	float		*sizeadj,
	int		*err
)
#else
(wid,indx,fmark,fmark_len,xoff,yoff,aspadj,sizeadj,err)
	int		*wid;
	int		*indx;
	_NhlFString	fmark;
	int		*fmark_len;
	float		*xoff;
	float		*yoff;
	float		*aspadj;
	float		*sizeadj;
	int		*err;
#endif
{
	char	tstr[_NhlMAXMARKERLEN];

	if(!_NhlFstrToCstr(tstr,NhlNumber(tstr),fmark,*fmark_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Can't convert Fortran string to C string");
		*err = NhlFATAL;
		return;
	}

	*err = NhlSetMarker(*wid,*indx,tstr,*xoff,*yoff,*aspadj,*sizeadj);

	return;
}

static struct{
	float	xoff;
	float	yoff;
	float	size;
	char	*string;
} minfo;

/*ARGSUSED*/
void
_NhlSetMarkerInfo
#if  NhlNeedProto
(
	NhlLayer	instance,
	NhlLayer	plot
)
#else
(instance,plot)
        NhlLayer	instance;
        NhlLayer	plot;
#endif
{
	char			func[] = "_NhlSetMarkerInfo";
        NhlWorkstationLayer	tinst = (NhlWorkstationLayer)instance;
	NhlWorkstationLayerPart	*wk_p = &tinst->work;
	_NhlMarkerStyleInfo	*mkp = wk_p->mip;
	int			index;
	int			marker_color;
	float			p_height, p_width;

/*
 * Flush plotif buffer...
 */
	c_plotif(0.,0.,2);
/*
 * Make sure the marker size is okay
 */
	if (mkp->marker_size <= 0.0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s: marker size must be greater than 0.0",func);
		mkp->marker_size = 0.007;
	}
/*
 * An out-of-bounds marker index should have been caught at a higher
 * level. Error and set to default marker.
 */

	if((index = mkp->marker_index) < 0){
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s: invalid marker index:%d",func,index);
		index = NhlWK_DEF_MARKER;
	}

	if(index == 0){
		p_width = 16.0;
		p_height = 21.0;
		minfo.size = marker_table[NhlWK_DEF_MARKER]->size_adj *
					mkp->marker_size;
		minfo.xoff = minfo.size *
			marker_table[NhlWK_DEF_MARKER]->x_off;
		minfo.yoff = minfo.size *
			marker_table[NhlWK_DEF_MARKER]->y_off;
		minfo.string = marker_table[NhlWK_DEF_MARKER]->marker;
	}
	else if (index > 0) {
		index = 1 + (index - 1) % wk_p->marker_table_len;
		if (marker_table[index]->aspect_adj <= 1.0) {
			p_width = 21.0;
			p_height = 21.0 * marker_table[index]->aspect_adj;
		} else {
			p_width = 21.0 / marker_table[index]->aspect_adj;
			p_height = 21.0;
		}

		minfo.size = marker_table[index]->size_adj * mkp->marker_size;
		minfo.xoff = minfo.size * marker_table[index]->x_off;
		minfo.yoff = minfo.size * marker_table[index]->y_off;
		minfo.string = marker_table[index]->marker;

	}
	c_pcsetr("PH",p_height);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcsetr("PW",p_width);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	c_pcseti("FN", 1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcsetr("CL",mkp->marker_thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	marker_color = _NhlGetGksCi(tinst->base.wkptr, mkp->marker_color);
	c_pcseti("OC",marker_color);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("CC",marker_color);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

        return;
}


static NhlErrorTypes
WorkstationMarker
#if  NhlNeedProto
(
	NhlLayer	l,
	float		*x,
	float		*y,
	int		num_points
)
#else
(l,x,y,num_points)
	NhlLayer	l;
	float		*x;
	float		*y;
	int		num_points;
#endif
{
	char			func[] = "WorkstationMarker";
        float			fl,fr,fb,ft,ul,ur,ub,ut;
	int			ll, i;
	NhlErrorTypes		ret = NhlNOERROR;

/*
 * Make the user space coincide with the NDC space for the
 * duration of the routine
 */
	c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
	c_set(fl,fr,fb,ft,fl,fr,fb,ft,1);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;

	for (i=0; i<num_points; i++) {
		/*
		 * marker size is multiplied by 1.125 to account
		 * for 'SA' parameter of PlotChar.
		 */
		c_plchhq(x[i]+minfo.xoff,y[i]+minfo.yoff,minfo.string,
			minfo.size*1.125,0.0,0.0);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func))
			ret = NhlWARNING;
	}

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		ret = NhlWARNING;

	return ret;
}

NhlErrorTypes
_NhlWorkstationMarker
#if NhlNeedProto
(
	NhlLayer	l,
	float		*x,
	float		*y,
	int		num_points
)
#else
(l,x,y,num_points)
	NhlLayer	l;
	float		*x;
	float		*y;
	int		num_points;
#endif
{
	NhlWorkstationClassPart *wcp =
	&((NhlWorkstationClass)l->base.layer_class)->work_class;

	return (*wcp->marker_work)(l,x,y,num_points);
}

/*
 * Function:	NhlIsWorkstation
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlBoolean
NhlIsWorkstation
#if	NhlNeedProto
(
	int	pid
)
#else
(pid)
	int	pid;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l && _NhlIsWorkstation(l))
		return True;

	return False;
}

/*
 * Function:	nhlpfisworkstation
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlpfisworkstation,NHLPFISWORKSTATION)
#if	NhlNeedProto
(
	int	*id,
	int	*status
)
#else
(id,status)
	int	*id;
	int	*status;
#endif
{
	*status = NhlIsWorkstation(*id);

	return;
}

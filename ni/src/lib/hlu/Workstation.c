/*
 *      $Id: Workstation.c,v 1.29 1995-03-06 06:03:32 boote Exp $
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
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/hlu/TransformI.h>

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

static NrmQuark intQ;
static NrmQuark intgenQ;
static NrmQuark colormap_name;
static NrmQuark colormaplen_name;
static NrmQuark bkgnd_name;
static NrmQuark foregnd_name;
static NrmQuark	marker_tbl_strings_name;
static NrmQuark marker_tbl_params_name;
static NrmQuark dash_table_name;

#define Oset(field) NhlOffset(NhlWorkstationLayerRec,work.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNwkColorMap,NhlCwkColorMap,NhlTFloatGenArray,sizeof(NhlGenArray),
		Oset(color_map),NhlTImmediate,_NhlUSET(NULL),0,
		(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkColorMapLen, NhlCwkColorMapLen, NhlTInteger, sizeof(int),
		Oset(color_map_len),NhlTImmediate, 
		_NhlUSET((NhlPointer)NhlNumber(def_color)),_NhlRES_GONLY,NULL},
	{NhlNwkBackgroundColor, NhlCwkBackgroundColor, NhlTFloatGenArray, 
		sizeof(NhlPointer),
		Oset(bkgnd_color), NhlTImmediate,_NhlUSET( NULL),0,
		(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkForegroundColor, NhlCwkForegroundColor, NhlTFloatGenArray, 
		sizeof(NhlPointer),
		Oset(foregnd_color), NhlTImmediate,_NhlUSET( NULL),0,
						(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkGksWorkId,NhlCwkGksWorkId,NhlTInteger,sizeof(int),
		Oset(gkswksid),NhlTImmediate,_NhlUSET((NhlPointer)0),
							_NhlRES_GONLY,NULL},
	{NhlNwkDashTableLength, NhlCwkDashTableLength, NhlTInteger, 
		sizeof(int),Oset(dash_table_len),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},
	{NhlNwkFillTableLength, NhlCwkFillTableLength, NhlTInteger, 
		sizeof(int),Oset(fill_table_len),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},
	{ NhlNwkMarkerTableLength, NhlCwkMarkerTableLength, NhlTInteger, 
		sizeof(int),Oset(marker_table_len),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_GONLY,NULL},

#define POset(field) Oset(public_lineinfo.field)
	{NhlNwkDashPattern,NhlCwkDashPattern,NhlTDashIndex,
		sizeof(NhlDashIndex),POset(dash_pattern),NhlTImmediate,
		_NhlUSET((NhlPointer)0),0,NULL},
        {NhlNwkLineDashSegLenF, NhlCwkLineDashSegLenF,NhlTFloat,sizeof(float),
		POset(line_dash_seglen),NhlTString,_NhlUSET(".15"),0,NULL},
        {NhlNwkLineColor,NhlCwkLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
                POset(line_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
        {NhlNwkLineThicknessF, NhlCwkLineThicknessF, NhlTFloat, sizeof(float),
                POset(line_thickness), NhlTString,_NhlUSET("1.0"),0,NULL},
        {NhlNwkLineLabel,NhlCwkLineLabel,NhlTString,sizeof(NhlString),
                POset(line_label),NhlTImmediate,_NhlUSET((NhlPointer)NULL),0,
		(NhlFreeFunc)NhlFree},
        {NhlNwkLineLabelFont,NhlCwkLineLabelFont,NhlTFont,sizeof(NhlFont),
                POset(line_label_font),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),0,(NhlFreeFunc)NhlFree},
	{NhlNwkLineLabelColor,NhlCwkLineLabelColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(line_label_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
        {NhlNwkLineLabelFontHeightF,NhlCwkLineLabelFontHeightF,NhlTFloat,
                sizeof(float),POset(line_label_font_height),NhlTString,
		_NhlUSET("0.0125"),0,NULL},
#undef POset
#define POset(field) Oset(public_markinfo.field)
	{NhlNwkMarkerIndex,NhlCwkMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),POset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),0,NULL},
	{NhlNwkMarkerColor,NhlCwkMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),0,NULL},
	{NhlNwkMarkerSizeF,NhlCwkMarkerSizeF,NhlTFloat,sizeof(float),
		  POset(marker_size),NhlTString,_NhlUSET("0.007"),0,NULL},
	{NhlNwkMarkerXOffsetF,NhlCwkMarkerXOffsetF,NhlTFloat,sizeof(float),
		  POset(marker_x_off),NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNwkMarkerYOffsetF,NhlCwkMarkerYOffsetF,NhlTFloat,sizeof(float),
		  POset(marker_y_off),NhlTString,_NhlUSET("0.0"),0,NULL},
	{NhlNwkMarkerThicknessF, NhlCwkMarkerThicknessF, NhlTFloat,
		  sizeof(float),POset(marker_thickness),NhlTString,
		  _NhlUSET("1.0"),0,NULL},
#undef POset

/* End-documented-resources */

	{_NhlNwkReset,_NhlCwkReset,NhlTBoolean,sizeof(NhlBoolean),Oset(reset),
		NhlTImmediate,_NhlUSET((NhlPointer)0),_NhlRES_SONLY,NULL},
	{_NhlNwkSetPublic,_NhlCwkSetPublic,NhlTBoolean,sizeof(NhlBoolean),
		Oset(set_public),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_SONLY,NULL},

#define POset(field) Oset(private_lineinfo.field)
	{_NhlNwkDashPattern,_NhlCwkDashPattern,NhlTDashIndex,
		sizeof(NhlDashIndex),POset(dash_pattern),NhlTImmediate,
		_NhlUSET((NhlPointer)0),_NhlRES_GSONLY,NULL},
        {_NhlNwkLineDashSegLenF, _NhlCwkLineDashSegLenF,NhlTFloat,sizeof(float),
		POset(line_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_GSONLY,NULL},
        {_NhlNwkLineColor,_NhlCwkLineColor,NhlTColorIndex,sizeof(NhlColorIndex),
                POset(line_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_GSONLY,NULL},
        {_NhlNwkLineThicknessF, _NhlCwkLineThicknessF, NhlTFloat, sizeof(float),
                POset(line_thickness), NhlTString,_NhlUSET("1.0"),
		_NhlRES_GSONLY,NULL},
        {_NhlNwkLineLabel,_NhlCwkLineLabel,NhlTString,sizeof(NhlString),
                POset(line_label), NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_GSONLY,(NhlFreeFunc)NhlFree},
        {_NhlNwkLineLabelFont, _NhlCwkLineLabelFont, NhlTFont, sizeof(NhlFont),
                POset(line_label_font), NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_GSONLY,(NhlFreeFunc)NhlFree},
	{_NhlNwkLineLabelColor,_NhlCwkLineLabelColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(line_label_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_GSONLY,NULL},
        {_NhlNwkLineLabelFontHeightF,_NhlCwkLineLabelFontHeightF,NhlTFloat,
                sizeof(float),POset(line_label_font_height),NhlTString,
		_NhlUSET("0.0125"),_NhlRES_GSONLY,NULL},
#undef POset

#define POset(field) Oset(private_markinfo.field)
	{_NhlNwkMarkerIndex,_NhlCwkMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),POset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),_NhlRES_GSONLY,NULL},
	{_NhlNwkMarkerColor,_NhlCwkMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_GSONLY,NULL},
	{_NhlNwkMarkerSizeF,_NhlCwkMarkerSizeF,NhlTFloat,sizeof(float),
		  POset(marker_size),NhlTString,_NhlUSET("0.007"),
		  _NhlRES_GSONLY,NULL},
	{_NhlNwkMarkerXOffsetF,_NhlCwkMarkerXOffsetF,NhlTFloat,sizeof(float),
		  POset(marker_x_off),NhlTString,_NhlUSET("0.0"),_NhlRES_GSONLY,
		  NULL},
	{_NhlNwkMarkerYOffsetF,_NhlCwkMarkerYOffsetF,NhlTFloat,sizeof(float),
		  POset(marker_y_off),NhlTString,_NhlUSET("0.0"),_NhlRES_GSONLY,
		  NULL},
	{_NhlNwkMarkerThicknessF, _NhlCwkMarkerThicknessF, NhlTFloat,
		  sizeof(float),POset(marker_thickness),NhlTString,
		  _NhlUSET("1.0"),_NhlRES_GSONLY,NULL},
#undef POset

	{_NhlNwkDashTable,_NhlCwkDashTable,NhlTStringGenArray,
		sizeof(NhlGenArray),Oset(dash_table),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_GSONLY,
		(NhlFreeFunc)NhlFreeGenArray},
	{_NhlNwkFillIndex,_NhlCwkFillIndex,NhlTFillIndex,sizeof(NhlFillIndex),
		Oset(fill_index),NhlTImmediate,_NhlUSET((NhlPointer)0),
		_NhlRES_GSONLY,NULL},
	{_NhlNwkFillColor,_NhlCwkFillColor,NhlTColorIndex,sizeof(NhlColorIndex),
		Oset(fill_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_GSONLY,NULL},
	{_NhlNwkFillBackground,_NhlCwkFillBackground,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(fill_background),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlTRANSPARENT),_NhlRES_GSONLY,NULL},
	{_NhlNwkFillScaleFactorF,_NhlCwkFillScaleFactorF,NhlTFloat,
		sizeof(float),Oset(fill_scale_factor),NhlTString,
		_NhlUSET("1.0"),_NhlRES_GSONLY,NULL},
	{_NhlNwkFillLineThicknessF,_NhlCwkFillLineThicknessF,NhlTFloat,
		sizeof(float),Oset(fill_line_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_GSONLY,NULL},
	{_NhlNwkDrawEdges,_NhlCwkDrawEdges,NhlTBoolean,sizeof(NhlBoolean),
		Oset(edges_on),NhlTImmediate,_NhlUSET(False),_NhlRES_GSONLY,
		NULL},
        {_NhlNwkEdgeDashPattern,_NhlCwkEdgeDashPattern,NhlTDashIndex,
		sizeof(NhlDashIndex),Oset(edge_dash_pattern),NhlTImmediate,
		_NhlUSET(0),_NhlRES_GSONLY,NULL},
	{_NhlNwkEdgeThicknessF,_NhlCwkEdgeThicknessF,NhlTFloat,sizeof(float),
		Oset(edge_thickness),NhlTString,_NhlUSET("1.0"),_NhlRES_GSONLY,
		NULL},
	{_NhlNwkEdgeDashSegLenF,_NhlCwkEdgeDashSegLenF,NhlTFloat,sizeof(float),
		Oset(edge_dash_seglen),NhlTString,_NhlUSET(".15"),
		_NhlRES_GSONLY,NULL},
	{_NhlNwkEdgeColor,_NhlCwkEdgeColor,NhlTColorIndex,sizeof(NhlColorIndex),
		Oset(edge_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_GSONLY,NULL},

	{_NhlNwkMarkerTableStrings,_NhlCwkMarkerTableStrings,NhlTStringGenArray,
		  sizeof(NhlGenArray),Oset(marker_table_strings),NhlTImmediate,
		  _NhlUSET((NhlPointer)NULL),_NhlRES_GSONLY,
		  (NhlFreeFunc)NhlFreeGenArray},
	{_NhlNwkMarkerTableParams,_NhlCwkMarkerTableParams,NhlTGenArray,
		sizeof(NhlGenArray),Oset(marker_table_params),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_GSONLY,NULL},
/*
 * Marker Lines are being disabled for 4.0.
 * "no.res" makes it impossible to change the marker_lines_on variable.
 *
 */
 	{_NhlNwkDrawMarkerLines,_NhlCwkDrawMarkerLines,NhlTBoolean,
		sizeof(NhlBoolean),Oset(marker_lines_on),NhlTImmediate,
		_NhlUSET(False),_NhlRES_NOACCESS,NULL},
        {_NhlNwkMarkerLineDashPattern,_NhlCwkMarkerLineDashPattern, 
		  NhlTDashIndex,sizeof(NhlDashIndex),
		  Oset(marker_line_dash_pattern),NhlTImmediate,_NhlUSET(0),
		  _NhlRES_NOACCESS,NULL},
	{_NhlNwkMarkerLineThicknessF,_NhlCwkMarkerLineThicknessF,NhlTFloat,
		sizeof(float),Oset(marker_line_thickness),NhlTString,
		_NhlUSET("1.0"),_NhlRES_NOACCESS,NULL},
	{_NhlNwkMarkerLineDashSegLenF,_NhlCwkMarkerLineDashSegLenF,NhlTFloat,
		sizeof(float),Oset(marker_line_dash_seglen),NhlTString,
		_NhlUSET(".15"),_NhlRES_NOACCESS,NULL},
	{_NhlNwkMarkerLineColor,_NhlCwkMarkerLineColor,NhlTColorIndex,
		sizeof(NhlColorIndex),Oset(marker_line_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),_NhlRES_NOACCESS,NULL},
};

/*
* Base class method declarations
*/

static NhlErrorTypes WorkstationClassPartInitialize(
#if	NhlNeedProto
	NhlLayerClass	layerclass	/* layerclass to init	*/
#endif
);

static NhlErrorTypes WorkstationInitialize(
#if	NhlNeedProto
        NhlLayerClass,	/* class */
        NhlLayer,	/* req */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args */
#endif
);

static NhlErrorTypes WorkstationClassInitialize();


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
	float	x,
	float 	y,
	int	upordown
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
* Private functions
*/
static NhlErrorTypes AllocateColors(
#if	NhlNeedProto
NhlLayer	/* instance */
#endif
); 

static NhlErrorTypes DeallocateColors(
#if	NhlNeedProto
NhlLayer	/* instance */
#endif
); 



/*
* Default color map
*/


NhlWorkstationLayerClassRec NhlworkstationLayerClassRec = {
        {
/* class_name			*/	"workstationLayerClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlbaseLayerClassRec,

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

/* layer_draw			*/	WorkstationDraw,

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

NhlLayerClass NhlworkstationLayerClass = (NhlLayerClass)&NhlworkstationLayerClassRec;

NhlErrorTypes
NhlCvtScalarToIndex
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char		func[] = "NhlCvtScalarToIndex";
	int		tint;
	NrmValue	ival;

	if(nargs != 2){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	ival.size = sizeof(int);
	ival.data.ptrval = &tint;
	if(_NhlReConvertData(from->typeQ,intQ,from,&ival) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert from %s to %s",func,
				NrmQuarkToString(from->typeQ),NhlTInteger);
		return NhlFATAL;
	}

	if(tint < args[0].data.intval || tint > args[1].data.intval){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is not within index range %d - %d",func,
			tint,args[0].data.intval,args[1].data.intval);
		return NhlFATAL;
	}

	if((to->size > 0) && (to->data.ptrval != NULL)){
		/* caller provided space */

		if(to->size < sizeof(int)){
			/* not large enough */
			to->size = sizeof(int);
			return NhlFATAL;
		}

		to->size = sizeof(int);
		*((int *)(to->data.ptrval)) = tint;

		return NhlNOERROR;
	}
	else{
		static int val;

		to->size = sizeof(int);
		val = tint;
		to->data.ptrval = &val;
		return NhlNOERROR;
	}
}

NhlErrorTypes
NhlCvtGenArrayToIndexGenArray
#if	NhlNeedProto
(
	NrmValue		*from,
	NrmValue		*to,
	NhlConvertArgList	args,
	int			nargs
)
#else
(from,to,args,nargs)
	NrmValue		*from;
	NrmValue		*to;
	NhlConvertArgList	args;
	int			nargs;
#endif
{
	char		func[] = "NhlCvtGenArrayToIndexGenArray";
	char		buff[_NhlMAXRESNAMLEN];
	char		*indxgen_name;
	NhlGenArray	tgen;
	int		*tint,i;
	NrmValue	ival;

	if(nargs != 2){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	ival.size = sizeof(NhlGenArray);
	ival.data.ptrval = &tgen;
	if(_NhlReConvertData(from->typeQ,intgenQ,from,&ival) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Unable to convert from %s to %s",func,
				NrmQuarkToString(from->typeQ),
				NhlTIntegerGenArray);
		return NhlFATAL;
	}

	tint = (int*)tgen->data;

	for(i=0;i < tgen->num_elements;i++){
		if(tint[i] < args[0].data.intval ||
						tint[i] > args[1].data.intval){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Value %d is not within index range %d - %d",func,
			tint[i],args[0].data.intval,args[1].data.intval);
			return NhlFATAL;
		}
	}

	indxgen_name = NrmQuarkToString(to->typeQ);
	strcpy(buff,indxgen_name);
	indxgen_name = strstr(buff,NhlTGenArray);
	if(!indxgen_name){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid \"to\" type %s ???",
			func,NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}
	*indxgen_name = '\0';
	tgen->typeQ = NrmStringToQuark(buff);

	if((to->size > 0) && (to->data.ptrval != NULL)){
		/* caller provided space */

		if(to->size < sizeof(NhlGenArray)){
			/* not large enough */
			to->size = sizeof(NhlGenArray);
			return NhlFATAL;
		}

		to->size = sizeof(NhlGenArray);
		*((NhlGenArray *)(to->data.ptrval)) = tgen;

		return NhlNOERROR;
	}
	else{
		static NhlGenArray val;

		to->size = sizeof(NhlGenArray);
		val = tgen;
		to->data.ptrval = &val;
		return NhlNOERROR;
	}
}

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
	Gop_st status;
	int status1,dummy = 6;
	int i;
	_NhlEnumVals	dashvals[] = {
		{NhlSOLIDLINE,	"solidline"}
	};
	NhlConvertArg	dashargs[] = {
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
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)},
		{NhlIMMEDIATE,sizeof(int),
					_NhlUSET((NhlPointer)(MAX_COLOR_MAP-1))}
	};
	_NhlEnumVals	fillvals[] = {
		{NhlSOLIDFILL,	"solidfill"},
		{NhlHOLLOWFILL,	"hollowfill"},
		{NhlNULLFILL,	"nullfill"}
	};
	NhlConvertArg	fillargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)16)}
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

	(void)_NhlRegisterEnumType(NhlTDashIndex,dashvals,NhlNumber(dashvals));
	(void)_NhlRegisterEnumType(NhlTColorIndex,colorvals,
		NhlNumber(colorvals));
	(void)_NhlRegisterEnumType(NhlTFillIndex,fillvals,NhlNumber(fillvals));
	(void)_NhlRegisterEnumType(NhlTMarkerIndex,markervals,
		NhlNumber(markervals));
	(void)_NhlRegisterEnumType(NhlTMarkLineMode,mrkline,NhlNumber(mrkline));

	(void)NhlRegisterConverter(NhlTScalar,NhlTDashIndex,NhlCvtScalarToIndex,
		dashargs,NhlNumber(dashargs),False,NULL);
	(void)NhlRegisterConverter(NhlTScalar,NhlTColorIndex,
		NhlCvtScalarToIndex,colorargs,NhlNumber(colorargs),False,NULL);
	(void)NhlRegisterConverter(NhlTScalar,NhlTFillIndex,NhlCvtScalarToIndex,
		fillargs,NhlNumber(fillargs),False,NULL);

	(void)NhlRegisterConverter(NhlTGenArray,NhlTDashIndexGenArray,
		NhlCvtGenArrayToIndexGenArray,dashargs,NhlNumber(dashargs),
		False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlTColorIndexGenArray,
		NhlCvtGenArrayToIndexGenArray,colorargs,NhlNumber(colorargs),
		False,NULL);
	(void)NhlRegisterConverter(NhlTGenArray,NhlTFillIndexGenArray,
		NhlCvtGenArrayToIndexGenArray,fillargs,NhlNumber(fillargs),
		False,NULL);

	intQ = NrmStringToQuark(NhlTInteger);
	intgenQ = NrmStringToQuark(NhlTIntegerGenArray);
	colormap_name = NrmStringToQuark(NhlNwkColorMap);
	colormaplen_name = NrmStringToQuark(NhlNwkColorMapLen);
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
	
	return(NhlNOERROR);
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
	NhlLayerClass	layerclass	/* layerclass to init	*/
)
#else
(layerclass)
	NhlLayerClass	layerclass;	/* layerclass to init	*/
#endif
{
	NhlWorkstationLayerClass	lc =
					(NhlWorkstationLayerClass)layerclass;
	NhlWorkstationLayerClass	sc = (NhlWorkstationLayerClass)
						lc->base_class.superclass;

	if(lc->work_class.open_work == NhlInheritOpen)
		lc->work_class.open_work = sc->work_class.open_work;

	if(lc->work_class.close_work == NhlInheritClose)
		lc->work_class.close_work = sc->work_class.close_work;

	if(lc->work_class.activate_work == NhlInheritActivate)
		lc->work_class.activate_work = sc->work_class.activate_work;

	if(lc->work_class.deactivate_work == NhlInheritDeactivate)
		lc->work_class.deactivate_work = sc->work_class.deactivate_work;

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
( NhlLayerClass class,  NhlLayer req, NhlLayer new, _NhlArgList args , int num_args  )
#else
( class,  req, new, args , num_args  )
	NhlLayerClass 	class;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList		args;
	int		num_args;
#endif
{
	NhlWorkstationLayer newl = (NhlWorkstationLayer) new;
	NhlColor* tcolor = NULL;
	int i;
	NhlErrorTypes retcode = NhlNOERROR, subret;
	NhlGenArray ga;
	char *e_text;
	char *entry_name = "WorkstationInitialize";
	int count[2];
	int len1, len2;
	NhlMarkerTableParams *mparams;
	NhlString *mstrings;
	NhlBoolean	explicit_cmap = False;

/* 
 * In case someone tries to set the value of the read-only fill table size
 * the actual size is maintained in a private variable. If the resource
 * has been set, issue a warning, then replace with the real value.
 */
	if (_NhlArgIsSet(args,num_args,NhlNwkFillTableLength)) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
	}
	newl->work.fill_table_len = fill_table_len - 1;

/*
 * Same for marker table (note marker table element 0 is not reported
 * to the user)
 */

	if (_NhlArgIsSet(args,num_args,NhlNwkMarkerTableLength)) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
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
			subret = NhlNewMarker(new->base.id,mstr,x,y,asp,size);
			if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		}
	}
	
/*
 * The dash pattern table is read-only
 */
	if (_NhlArgIsSet(args,num_args,NhlNwkDashTableLength)) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
	}
	if (_NhlArgIsSet(args,num_args,_NhlNwkDashTable)) {
		newl->work.dash_table  = NULL;
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
	}

	newl->work.dash_table_len = dash_table_len - 1;

	newl->work.gkswksid = (int)NhlFATAL;
	newl->work.gkswkstype = (int)NhlFATAL;
	newl->work.gkswksconid = (int)NhlFATAL;

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
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
		return NhlFATAL;
	}
	for (i=0; i<3; i++)
		(*tcolor)[i] = 0.0;
	count[0] = 3;
	if ((ga = NhlCreateGenArray((NhlPointer)tcolor,NhlTFloat,
				    sizeof(float),1,count)) == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
		return NhlFATAL;
	}
	ga->my_data = True;

	if (newl->work.bkgnd_color != NULL) {
		subret = _NhlValidatedGenArrayCopy(&ga,newl->work.bkgnd_color,
						   3,True,False,
						   NhlNwkBackgroundColor, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;

	}
	newl->work.bkgnd_color = ga;

	tcolor = (NhlColor *) newl->work.bkgnd_color->data;
	for (i=0; i<3; i++) {
		if ((*tcolor)[i] < 0.0 || (*tcolor)[i] > 1.0) {
			int j;
			e_text =
			   "%s: %s holds an invalid color value: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
				  NhlNwkBackgroundColor);
			retcode = MIN(retcode, NhlWARNING);
			for (j=0; j<3; j++)
				(*tcolor)[i] = 0.0;
			break;
		}
	}
	newl->work.private_color_map[NhlBACKGROUND].ci = SETALMOST;
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
 * that the user color map length is one less than the private color map
 * length since the user map length does not include the background color.
 */
	newl->work.num_private_colors = NhlNumber(def_color) + 1;

	if (_NhlArgIsSet(args,num_args,NhlNwkColorMapLen)) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkColorMap);
		return NhlFATAL;
	}
	ga->my_data = False;

	if (newl->work.color_map != NULL) {		
		subret = _NhlValidatedGenArrayCopy(&ga,newl->work.color_map,
						   3*MAX_COLOR_MAP,False,False,
						   NhlNwkColorMap, entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		if (subret > NhlWARNING) {
			newl->work.color_map_len = 
				newl->work.color_map->len_dimensions[0];
		}
		newl->work.num_private_colors = newl->work.color_map_len + 1;
		explicit_cmap = True;
	}
	newl->work.color_map = ga;

/*
 * SETALMOST is changed to a GKS color index when the workstation is opened
 * for now the ci will be the same as the array index but this may change
 * and hence the need for the ci field. Should values be checked? Yes.
 * Values less than 0.0 are considered "missing values" indicating that
 * the color should be considered unset. Checking for values greater than
 * 1.0 takes place in AllocateColors.
 * Start at index 1 since the background color has already been set.
 */
	tcolor = newl->work.color_map->data;
	for (i=1; i<newl->work.num_private_colors; i++)  {
		newl->work.private_color_map[i].ci = SETALMOST;
		newl->work.private_color_map[i].red = tcolor[i-1][0];
		newl->work.private_color_map[i].green = tcolor[i-1][1];
		newl->work.private_color_map[i].blue =  tcolor[i-1][2];
		if (tcolor[i-1][0] < 0.0 || tcolor[i-1][1] < 0.0 ||
		    tcolor[i-1][2] < 0.0) {
			newl->work.private_color_map[i].ci = UNSET;
		}
	}

/*
 * Process the foreground color resource: the foreground color is always
 * color index #1 (NhlFOREGROUND). It is set automatically when the 
 * colormap is loaded but an explicitly set foreground color overrides 
 * the colormap resource.
 */

	if (newl->work.foregnd_color != NULL) {
		tcolor = NULL;
		count[0] = 0;
		if ((ga = NhlCreateGenArray((NhlPointer)tcolor,NhlTFloat,
				    sizeof(float),1,count)) == NULL) {
			e_text = "%s: error creating %s GenArray";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name,
				  NhlNwkForegroundColor);
			return NhlFATAL;
		}
		ga->my_data = False;

		subret = _NhlValidatedGenArrayCopy(&ga,
						   newl->work.foregnd_color,
						   3,True,False,
						   NhlNwkForegroundColor, 
						   entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		
		tcolor = (NhlColor *) ga->data;
		for (i=0; i<3; i++) {
			if ((*tcolor)[i] < 0.0 || (*tcolor)[i] > 1.0) {
				e_text =
		    "%s: %s holds an invalid color value: not set";
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  e_text,entry_name,
					  NhlNwkForegroundColor);
				subret = NhlWARNING;
				retcode = MIN(retcode, subret);
				break;
			}
		}
		if (subret > NhlWARNING) {
			newl->work.private_color_map[NhlFOREGROUND].ci = 
				SETALMOST;
			newl->work.private_color_map[NhlFOREGROUND].red = 
				(*tcolor)[0];
			newl->work.private_color_map[NhlFOREGROUND].green =
				(*tcolor)[1];
			newl->work.private_color_map[NhlFOREGROUND].blue =
				(*tcolor)[2];
		}
		/*
		 * Foreground resource is created on the fly in GetValues
		 * if it is retrieved.
		 */
		(void)NhlFreeGenArray(ga);
		newl->work.foregnd_color = NULL;
	}
	else if(!explicit_cmap){
		/*
		 * If the cmap is defaulted - then the foreground depends upon
		 * the background.  It is black or white - which ever is better.
		 */
		newl->work.private_color_map[NhlFOREGROUND].ci = SETALMOST;
		if (newl->work.private_color_map[NhlBACKGROUND].red *
			newl->work.private_color_map[NhlBACKGROUND].red +
		    newl->work.private_color_map[NhlBACKGROUND].green *
			newl->work.private_color_map[NhlBACKGROUND].green +
		    newl->work.private_color_map[NhlBACKGROUND].blue *
			newl->work.private_color_map[NhlBACKGROUND].blue
		    < .75) {
			newl->work.private_color_map[NhlFOREGROUND].red = 1.0;
			newl->work.private_color_map[NhlFOREGROUND].green = 1.0;
			newl->work.private_color_map[NhlFOREGROUND].blue = 1.0;
		}
		else {
			newl->work.private_color_map[NhlFOREGROUND].red = 0.0;
			newl->work.private_color_map[NhlFOREGROUND].green = 0.0;
			newl->work.private_color_map[NhlFOREGROUND].blue = 0.0;
		}
	}

/*
 * Initialize the "default" graphics primatives.
 */
	newl->work.default_lineinfo = newl->work.private_lineinfo;
	newl->work.default_markinfo = newl->work.private_markinfo;

/*
 * Set the line label resource
 */
        if(newl->work.public_lineinfo.line_label != NULL) {
		char *tmp;
                tmp = (char*)NhlMalloc(
			strlen(newl->work.public_lineinfo.line_label)+1);
                strcpy(tmp,newl->work.public_lineinfo.line_label);
                newl->work.public_lineinfo.line_label = tmp;
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
#if	NhlNeedProto
( NhlLayer inst )
#else
(inst)
	NhlLayer inst;
#endif
{
	NhlWorkstationLayerPart	*wp = &((NhlWorkstationLayer)inst)->work;
	NhlErrorTypes	retcode = NhlNOERROR;

	NhlFreeGenArray(wp->bkgnd_color);
	NhlFreeGenArray(wp->foregnd_color);
	NhlFreeGenArray(wp->color_map);
	NhlFreeGenArray(wp->marker_table_strings);
	NhlFreeGenArray(wp->marker_table_params);

	if(wp->public_lineinfo.line_label != wp->private_lineinfo.line_label)
		NhlFree(wp->private_lineinfo.line_label);
	NhlFree(wp->public_lineinfo.line_label);

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
	NhlErrorTypes	ret,ret1;

	if(!children)
		return NhlNOERROR;

	if(!_NhlIsOverlayMember(children->pid))
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
#if  NhlNeedProto
( NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        NhlLayer  old; 
        NhlLayer  reference; 
        NhlLayer  new;
        _NhlArgList args;
        int num_args;
#endif
{
	NhlWorkstationLayer	newl = (NhlWorkstationLayer) new;
	int i;
	NhlWorkstationLayer	oldl = (NhlWorkstationLayer) old;
	NhlErrorTypes	retcode = NhlNOERROR,subret = NhlNOERROR;
	char *tmp;
	int count;
	char *entry_name = "WorkstationSetValues";
	NhlColor *tcolor;
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

		if(newl->work.private_lineinfo.line_label !=
					newl->work.public_lineinfo.line_label)
			NhlFree(newl->work.private_lineinfo.line_label);

		newl->work.private_lineinfo = newl->work.default_lineinfo;
		newl->work.private_markinfo = newl->work.default_markinfo;

		return NhlNOERROR;
	}
	if(_NhlArgIsSet(args,num_args,_NhlNwkSetPublic)){

		/*
		 * The _NhlNwkSetPublic Private Resource MUST be set
		 * individually.
		 */
		if(num_args > 1){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:%s must be set alone!",entry_name,
					_NhlNwkSetPublic);
			return NhlFATAL;
		}

		if(newl->work.private_lineinfo.line_label !=
					newl->work.public_lineinfo.line_label)
			NhlFree(newl->work.private_lineinfo.line_label);

		newl->work.private_lineinfo = newl->work.public_lineinfo;
		newl->work.private_markinfo = newl->work.public_markinfo;

		return NhlNOERROR;
	}

/*
 * Check to ensure that no one has messed with the read only fill table
 * size.
 */
	if (newl->work.fill_table_len != fill_table_len - 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
		newl->work.fill_table_len = fill_table_len - 1;
	}

/*
 * Check to ensure that no one has messed with the read only marker table
 * size.
 */
	if (newl->work.marker_table_len != marker_table_len - 1) { 
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
		newl->work.marker_table_len = 
			marker_table_len - 1;
	}

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
			subret = NhlNewMarker(new->base.id,mstr,x,y,asp,size);
			if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		}
	}
	
/*
 * The dash table is read-only
 */

	if (newl->work.dash_table_len != dash_table_len - 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
		newl->work.dash_table_len = dash_table_len - 1;
	}
	if (newl->work.dash_table != oldl->work.dash_table) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
		newl->work.dash_table = oldl->work.dash_table;
	}

/*
 * The background color cannot change once the workstation is initialized
 */
	if(newl->work.bkgnd_color != oldl->work.bkgnd_color ) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Illegal Background color change");
		retcode = MIN(NhlWARNING, retcode);
		newl->work.bkgnd_color = oldl->work.bkgnd_color;
	}

/*
 * The color map len resource is read only also. 
 */
	if (newl->work.color_map_len != newl->work.num_private_colors - 1) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "Attempt to set read-only resource ignored");
		retcode = MIN(NhlWARNING, retcode);
		newl->work.color_map_len = newl->work.num_private_colors - 1;
	}

	if (newl->work.color_map != oldl->work.color_map) {
		count = MIN(MAX_COLOR_MAP,
			    newl->work.color_map->len_dimensions[0]);
		subret = _NhlValidatedGenArrayCopy(&(oldl->work.color_map),
					       newl->work.color_map,
					       3*MAX_COLOR_MAP,False,False,
					       NhlNwkColorMap, entry_name);
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		newl->work.color_map = oldl->work.color_map;

		if (subret > NhlWARNING) {
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
				if (tcolor[i-1][0] < 0.0 || 
				    tcolor[i-1][1] < 0.0 ||
				    tcolor[i-1][2] < 0.0) {
					newl->work.private_color_map[i].ci 
						= UNSET;
				}
			}

			/* since passing in a colormap is a total reset of
			 * of the colormap; it is necessary to 'UNSET' any
			 * colors that were previously allocated */
			   
			for (i=newl->work.num_private_colors; 
			     i < MAX_COLOR_MAP; i++)  {
				newl->work.private_color_map[i].ci = UNSET;
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
		
		if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
				return retcode;
		newl->work.foregnd_color = oldl->work.foregnd_color;
		tcolor = (NhlColor *) newl->work.foregnd_color->data;

		if (subret > NhlWARNING) {
			for (i=0; i<3; i++) {
				if ((*tcolor)[i] < 0.0 || (*tcolor)[i] > 1.0) {
					e_text =
			    "%s: %s holds an invalid color value: not set";
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  e_text,entry_name,
						  NhlNwkForegroundColor);
					subret = NhlWARNING;
					retcode = MIN(retcode, subret);
					break;
				}
			}
		}
		if (subret > NhlWARNING) {
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
	subret = AllocateColors((NhlLayer)newl);
	retcode = MIN(retcode,subret);

/*
 * Set the line label
 */

        if((oldl->work.public_lineinfo.line_label !=
				newl->work.public_lineinfo.line_label)) {

		NhlFree(oldl->work.public_lineinfo.line_label);
                if(newl->work.public_lineinfo.line_label != NULL) {
                        tmp = (char*)NhlMalloc((unsigned)
			  strlen(newl->work.public_lineinfo.line_label)+1);
                        strcpy(tmp,newl->work.public_lineinfo.line_label);
                        newl->work.public_lineinfo.line_label = tmp;
                }
        }

        if((oldl->work.private_lineinfo.line_label !=
				newl->work.private_lineinfo.line_label)) {

		if(oldl->work.private_lineinfo.line_label !=
					oldl->work.public_lineinfo.line_label)
			NhlFree(oldl->work.private_lineinfo.line_label);
                if(newl->work.private_lineinfo.line_label != NULL) {
                        tmp = (char*)NhlMalloc((unsigned)
			  strlen(newl->work.private_lineinfo.line_label)+1);
                        strcpy(tmp,newl->work.private_lineinfo.line_label);
                        newl->work.private_lineinfo.line_label = tmp;
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
#if NhlNeedProto
(NhlLayer	instance )
#else
(instance)
	NhlLayer	instance;
#endif
{
	char			func[] = "WorkstationActivate";
	NhlWorkstationLayer	thework = (NhlWorkstationLayer) instance;
	NhlErrorTypes		retcode = NhlNOERROR;	

	if(wksisopn(thework->work.gkswksid)) {
		if(!wksisact(thework->work.gkswksid)) {
			gactivate_ws(thework->work.gkswksid);
			if(_NhlLLErrCheckPrnt(NhlWARNING,func))
				retcode = NhlWARNING;
		} else {
/*
* WORKSTATION IS ALREADY ACTIVE
*/
			NhlPError(NhlINFO,NhlEUNKNOWN,"WorkstationActivate called on already active workstation");
			retcode = NhlINFO; 
		}
	} else {
/*
* ERROR WORKSTATION IS NOT OPEN INITILIZATION FAILED
*/
		NhlPError(NhlFATAL,NhlEUNKNOWN, "WorkstationActivate can't activate an unopened workstation");
		retcode = NhlFATAL;
	}
	return(retcode);
}

/*
 * Function:	WorkstationDeactivate
 *
 * Description:	Deactivates workstation. if not open NhlFATAL error if not active
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
#if NhlNeedProto
(NhlLayer	instance )
#else
(instance)
	NhlLayer	instance;
#endif
{
	char			func[] = "WorkstationDeactivate";
	NhlWorkstationLayer	thework = (NhlWorkstationLayer) instance;
	NhlErrorTypes		retcode = NhlNOERROR;

	if(wksisopn(thework->work.gkswksid)&&wksisact(thework->work.gkswksid)){
		gdeactivate_ws(thework->work.gkswksid);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func))
			retcode = NhlWARNING;
	}
	else{
/*
* ERROR WORKSTATION NOT ACTIVE OR NOT INITIALIZED
*/
		NhlPError(NhlWARNING,NhlEUNKNOWN,"WorkstationDeactivate: workstation not active or not openned");
		retcode = NhlWARNING;
	}

	return retcode;
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
#if NhlNeedProto
(NhlLayer	instance )
#else
(instance)
	NhlLayer	instance;
#endif
{	
	char			func[] = "OpenWorkstation";
	NhlWorkstationLayer	thework = (NhlWorkstationLayer) instance;
	NhlErrorTypes		retcode = NhlNOERROR;
	int			i = 2;

	if(thework->work.gkswkstype == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unknown workstation type");
		return(NhlFATAL);
		
	} 
	if(thework->work.gkswksconid == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unknown workstation connection id");
		return(NhlFATAL);
	}
	while(wksisopn(i)) {
		i++;
	}
	thework->work.gkswksid = i;

/* FORTRAN */ _NHLCALLF(gopwk,GOPWK)(&(thework->work.gkswksid),&(thework->work.gkswksconid),&(thework->work.gkswkstype));
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
	gset_clip_ind(GIND_NO_CLIP);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func)){
		return NhlFATAL;
	}

	retcode = AllocateColors((NhlLayer)thework);

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
#if NhlNeedProto
(NhlLayer	instance )
#else
(instance)
	NhlLayer	instance;
#endif
{
	char			func[] = "WorkstationClose";
	NhlWorkstationLayer	thework = (NhlWorkstationLayer) instance;
	NhlErrorTypes retcode = NhlNOERROR;

	if(wksisact(thework->work.gkswksid)){
		NhlPError(NhlINFO,NhlEUNKNOWN,"WorkstationClose: workstation must be deactivated before closed, deactivating workstation now");
		_NhlDeactivateWorkstation(instance);
	} 
	if(!wksisopn(thework->work.gkswksid)) {
		NhlPError(NhlINFO,NhlEUNKNOWN,"WorkstationClose: workstation already closed");
		retcode = NhlINFO;
	} else {
		gclose_ws(thework->work.gkswksid);
		if(_NhlLLErrCheckPrnt(NhlINFO,func))
			retcode = NhlINFO;
	}
	return(retcode);
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
	NhlLayer	l	/* workstation layer to update	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to update	*/
#endif
{
	char	func[] = "WorkstationUpdate";

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

	gclear_ws(_NhlWorkstationId(l),GFLAG_ALWAYS);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;

	return NhlNOERROR;
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
#if	NhlNeedProto
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

/*
 * Function:	nhl_fsetcolor
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
void _NHLCALLF(nhl_fsetcolor,NHL_FSETCOLOR)
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

NhlErrorTypes	_NhlSetColor
#if	NhlNeedProto
(NhlLayer inst, int ci, float red, float green, float blue)
#else
(inst,ci,red,green,blue)
	NhlLayer	inst;
	int	ci;
	float	red;
	float	green;
	float	blue;
#endif
{
	NhlWorkstationLayer	thework = (NhlWorkstationLayer)inst;
	
	if (ci == NhlBACKGROUND || ci == NhlTRANSPARENT) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		     "NhlSetColor: color index cannot be set: no allocation");
		return(NhlWARNING);
	}
	else if (ci >= MAX_COLOR_MAP || ci < NhlTRANSPARENT) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "NhlSetColor: invalid color index: no allocation");
		return(NhlWARNING);
	}
	else if (red < 0.0 || red > 1.0 || green < 0.0 || green > 1.0 ||
		 blue < 0.0 || blue > 1.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
	         "NhlSetColor: a color component was invalid: no allocation");
		return(NhlWARNING);
	}

	thework->work.private_color_map[ci].ci = SETALMOST;
	thework->work.private_color_map[ci].red = red;
	thework->work.private_color_map[ci].green = green;
	thework->work.private_color_map[ci].blue = blue;

	return(AllocateColors((NhlLayer)thework));
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
#if	NhlNeedProto
(NhlLayer inst, int ci)
#else
(inst,ci)
	NhlLayer inst;
	int	ci;
#endif
{
	NhlWorkstationLayer	thework = (NhlWorkstationLayer)inst;

	if (ci == NhlBACKGROUND || ci == NhlTRANSPARENT) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "NhlSetColor: color index cannot be freed");
		return(NhlWARNING);
	}
	else if (ci >= MAX_COLOR_MAP || ci < NhlTRANSPARENT) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "NhlSetColor: invalid color index: cannot be freed");
		return(NhlWARNING);
	}

	thework->work.private_color_map[ci].ci = REMOVE;

	return(DeallocateColors((NhlLayer)thework));
}

/*
 * Function:	nhl_ffreecolor
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
void _NHLCALLF(nhl_ffreecolor,NHL_FFREECOLOR)
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
#if  NhlNeedProto
(NhlLayer inst )
#else
(inst)
	NhlLayer inst;
#endif
{
	char			func[] = "AllocateColors";
	NhlWorkstationLayer	thework = (NhlWorkstationLayer) inst;
	Gcolr_rep		tmpcolrrep;
	int			i, max_col = 0;
	NhlPrivateColor		*pcmap;
	NhlErrorTypes		ret = NhlNOERROR;
/*
* Temporary allocation routine until some color management scheme is put in 
* place. In fact this may turn in to a method
*/
	pcmap = thework->work.private_color_map;

	for ( i = 0; i < MAX_COLOR_MAP; i++) {
		if(pcmap[i].ci == SETALMOST) {
			tmpcolrrep.rgb.red = pcmap[i].red;
			tmpcolrrep.rgb.green = pcmap[i].green;
			tmpcolrrep.rgb.blue= pcmap[i].blue;
			gset_colr_rep(thework->work.gkswksid,i,&tmpcolrrep);
			if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
				ret = NhlWARNING;
				pcmap[i].ci = UNSET;
			}
			else {
				pcmap[i].ci = i;
				max_col = i;
			}
		}
		else if (pcmap[i].ci > NhlTRANSPARENT) {
			max_col = i;
		}
		else {
			pcmap[i].red = -1.0;
			pcmap[i].green = -1.0;
			pcmap[i].blue = -1.0;
		}
	}
	thework->work.color_map_len = max_col;
	thework->work.num_private_colors = max_col + 1;
	
	return ret;
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
#if  NhlNeedProto
(NhlLayer inst )
#else
(inst)
	NhlLayer inst;
#endif
{
	char			func[] = "DeallocateColors";
	NhlWorkstationLayer	thework = (NhlWorkstationLayer) inst;
	NhlPrivateColor		*pcmap = thework->work.private_color_map;
	Gcolr_rep		tmpcolrrep;
	int			i, max_col = 0;
	NhlErrorTypes		ret = NhlNOERROR;

/*
 * If the Foreground is removed set a new foreground of white or black 
 * depending on whether the Background color is closer to white or black.
 */

	if (pcmap[NhlFOREGROUND].ci == REMOVE) {
		pcmap[NhlFOREGROUND].ci = NhlFOREGROUND;
		if (pcmap[NhlBACKGROUND].red * pcmap[NhlBACKGROUND].red +
		    pcmap[NhlBACKGROUND].green * pcmap[NhlBACKGROUND].green +
		    pcmap[NhlBACKGROUND].blue * pcmap[NhlBACKGROUND].blue 
		    < .75) {
			pcmap[NhlFOREGROUND].red = 1.0;
			pcmap[NhlFOREGROUND].green = 1.0;
			pcmap[NhlFOREGROUND].blue = 1.0;
		}
		else {
			pcmap[NhlFOREGROUND].red = 0.0;
			pcmap[NhlFOREGROUND].green = 0.0;
			pcmap[NhlFOREGROUND].blue = 0.0;
		}
		tmpcolrrep.rgb.red = pcmap[NhlFOREGROUND].red;
		tmpcolrrep.rgb.green = pcmap[NhlFOREGROUND].green;
		tmpcolrrep.rgb.blue= pcmap[NhlFOREGROUND].blue;
		gset_colr_rep(thework->work.gkswksid,
			      NhlFOREGROUND,&tmpcolrrep);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func))
			ret = NhlWARNING;
	}
		
	for( i = 1; i < MAX_COLOR_MAP; i++) {
		if (pcmap[i].ci == REMOVE) {
			pcmap[i].ci = UNSET;
			pcmap[i].red = -1.0;
			pcmap[i].green = -1.0;
			pcmap[i].blue = -1.0;
		}
		else if (pcmap[i].ci > NhlTRANSPARENT) max_col = i;
	}
	thework->work.color_map_len = max_col;
	thework->work.num_private_colors = max_col + 1;
	return ret;
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

/*
 * Function:	nhl_fnewcolor
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
void _NHLCALLF(nhl_fnewcolor,NHL_FNEWCOLOR)
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

int _NhlNewColor
#if   NhlNeedProto
(NhlLayer inst,float red,float green,float blue)
#else
(inst,red,green,blue)
        NhlLayer   inst;
        float   red;
        float   green;
        float   blue;
#endif
{
	NhlWorkstationLayer  thework = (NhlWorkstationLayer) inst;
	int i = 2;
	NhlErrorTypes retcode = NhlNOERROR;

	if (red < 0.0 || red > 1.0 || green < 0.0 || green > 1.0 ||
		 blue < 0.0 || blue > 1.0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		 "NhlNewColor: a color component was invalid; no allocation");
		return(NhlWARNING);
	}
	while( thework->work.private_color_map[i].ci != UNSET ) {
		i++;
		if(i == MAX_COLOR_MAP) {
/*
* ERROR : no available colors
*/		
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "NhlNewColor: no available colors");
			return(NhlFATAL);
		}
	}
	thework->work.private_color_map[i].ci = SETALMOST;
	thework->work.private_color_map[i].red = red;
	thework->work.private_color_map[i].green = green;
	thework->work.private_color_map[i].blue = blue;
	retcode = AllocateColors((NhlLayer)thework);

	return((retcode <= NhlINFO)? (int)retcode : i);
	 
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
 *		_NhlNwkMarkerTableStrings
 *		_NhlNwkMarkerTableParams
 *		_NhlNwkDashTable
 *	The user is responsible for freeing this memory.
 */

static NhlErrorTypes	WorkstationGetValues
#if NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	NhlLayer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
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
					  entry_name,_NhlNwkMarkerTableStrings);
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
					  entry_name,_NhlNwkMarkerTableStrings);
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
	return(NhlNOERROR);
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
#if NhlNeedProto
(NhlLayer instance)
#else
(instance)
	NhlLayer instance;
#endif
{
	NhlWorkstationLayer wl = (NhlWorkstationLayer) instance;

	return(wl->work.gkswksid);
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
#if NhlNeedProto
(int pid, int ci)
#else
(pid,ci)
	int pid;
	int ci;
#endif
{
 	return(_NhlGetGksCi(_NhlGetLayer(pid),ci));	
}

/*
 * Function:	nhl_fgetgksci
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
void _NHLCALLF(nhl_fgetgksci,NHL_FGETGKSCI)
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

int _NhlGetGksCi
#if NhlNeedProto
(
	NhlLayer	workstation,
	int		ci
)
#else
(workstation, ci)
	NhlLayer	workstation;
	int		ci;
#endif
{
	char			func[] = "_NhlGetGksCi";
	NhlWorkstationLayer	wk = (NhlWorkstationLayer)workstation;
	if(wk && _NhlIsWorkstation(workstation)){
		if((ci < 0) || (ci >= MAX_COLOR_MAP)){
			return 1;
		}
		if(wk->work.private_color_map[ci].ci >= 0) {
			return(wk->work.private_color_map[ci].ci);
		} else {
			return(1);
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s: attempt to return color from non-workstation",func);
		return((int)1);
	}
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
NhlBoolean NhlIsAllocatedColor
#if NhlNeedProto
(int pid, int ci)
#else
(pid,ci)
	int pid;
	int ci;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l && _NhlIsAllocatedColor(l,ci))
		return True;
	return False;
}

/*
 * Function:	nhl_fisallocatedcolor
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
void _NHLCALLF(nhl_fisallocatedcolor,NHL_FISALLOCATEDCOLOR)
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
	NhlLayer workstation, 
	int  ci
)
#else
(workstation, ci)
	NhlLayer	workstation;
	int		ci;
#endif
{
	
	NhlWorkstationLayer  wk = (NhlWorkstationLayer) workstation;
	char *entry_name = "NhlIsAllocatedColor";
	char *e_text;

	if (_NhlIsWorkstation(wk)) {
		if (ci < NhlTRANSPARENT || ci >= MAX_COLOR_MAP) return False;
		if (ci == NhlTRANSPARENT) return True;
		return wk->work.private_color_map[ci].ci >= 0 ? True : False;
	}

	e_text = "%s: invalid workstation identifier";
	NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	return((int)NhlWARNING);
}

/*
 * Workstation Methods
 */
NhlErrorTypes _NhlOpenWorkstation
#if NhlNeedProto
(NhlLayer wks)
#else
(wks)
	NhlLayer wks;
#endif
{
	char				func[] = "_NhlOpenWorkstation";
	NhlWorkstationLayerClassPart	*wc =
		&((NhlWorkstationLayerClass)wks->base.layer_class)->work_class;

	if(_NhlIsWorkstation(wks)) {
		return (*(wc->open_work))(wks);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: attempt to perform open on nonworkstation",func);
		return NhlFATAL;
	}
}

NhlErrorTypes _NhlCloseWorkstation
#if NhlNeedProto
(NhlLayer wks)
#else
(wks)
	NhlLayer wks;
#endif
{
	char				func[] = "_NhlCloseWorkstation";
	NhlWorkstationLayerClassPart	*wc =
		&((NhlWorkstationLayerClass)wks->base.layer_class)->work_class;

	if(_NhlIsWorkstation(wks)) {
		return (*(wc->close_work))(wks);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: attempt to perform close on nonworkstation",func);
		return NhlFATAL;
	}
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
NhlErrorTypes _NhlActivateWorkstation
#if NhlNeedProto
(NhlLayer wks)
#else
(wks)
	NhlLayer wks;
#endif
{
	char				func[] = "_NhlActivateWorkstation";
	NhlWorkstationLayerClassPart	*wc =
		&((NhlWorkstationLayerClass)wks->base.layer_class)->work_class;

	if(_NhlIsWorkstation(wks)) {
		return (*(wc->activate_work))(wks);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: attempt to perform activate on nonworkstation",func);
		return NhlFATAL;
	}
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
NhlErrorTypes _NhlDeactivateWorkstation
#if NhlNeedProto
(NhlLayer wks)
#else
(wks)
	NhlLayer wks;
#endif
{
	char				func[] = "_NhlDeactivateWorkstation";
	NhlWorkstationLayerClassPart	*wc =
		&((NhlWorkstationLayerClass)wks->base.layer_class)->work_class;

	if(_NhlIsWorkstation(wks)) {
		return (*(wc->deactivate_work))(wks);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
		"%s: attempt to perform deactivate on nonworkstation",func);
		return NhlFATAL;
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
#if	NhlNeedProto
(
	int	workid	/* id of workstation class object	*/
)
#else
(workid)
	int	workid;	/* id of workstation class object	*/
#endif
{
	NhlLayer			l = _NhlGetLayer(workid);
	NhlWorkstationLayerClass	lc;

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

	lc = (NhlWorkstationLayerClass)l->base.layer_class;

	return (*(lc->work_class.update_work))(l);
}

/*
 * Function:	nhl_fupdateworkstation
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
_NHLCALLF(nhl_fupdateworkstation,NHL_FUPDATEWORKSTATION)
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
	NhlLayer			l = _NhlGetLayer(workid);
	NhlWorkstationLayerClass	lc;

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

	lc = (NhlWorkstationLayerClass)l->base.layer_class;

	return (*(lc->work_class.clear_work))(l);
}

/*
 * Function:	nhl_fclearworkstation
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
_NHLCALLF(nhl_fclearworkstation,NHL_FCLEARWORKSTATION)
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
 * Function:	nhl_fframe
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
_NHLCALLF(nhl_fframe,NHL_FFRAME)
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
void _NhlSetLineInfo
#if  NhlNeedProto
(NhlLayer instance,NhlLayer plot)
#else
(instance,plot)
        NhlLayer instance;
        NhlLayer plot;
#endif
{
	char			func[] = "_NhlSetLineInfo";
        NhlWorkstationLayerPart	*wkp = &((NhlWorkstationLayer)instance)->work;
	_NhlWorkLineInfo	*wklp = &wkp->private_lineinfo;
	float			tf;
        char			buffer[80];
	int			buff_size = sizeof(buffer) - 1;
	int			i,j,ix;
	char			*tchar;

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
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "_NhlSetLineInfo: invalid dash pattern index");
		ix = wklp->dash_pattern = NhlSOLIDLINE;
	}
	else if (ix > wkp->dash_table_len) {
		/* NhlINFO - but it's a void function right now */
		NhlPError(NhlINFO,NhlEUNKNOWN,
	"_NhlSetLineInfo: using mod function on dash pattern index: %d", ix);

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

        if(wklp->line_label != NULL){
		buff_size = sizeof(buffer) - strlen(buffer) - 1;
		tchar = &buffer[strlen(buffer)];
		if(wklp->line_label_color == NhlTRANSPARENT){
			/*
			 * Put spaces in for label.
			 */
			j = MIN(strlen(wklp->line_label) * 2 + 1,buff_size);
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
			while(i < buff_size && wklp->line_label[j] != '\0'){
				if(wklp->line_label[j] == ' ')
					tchar[i++] = '|';
				tchar[i++] = wklp->line_label[j++];
			}
			
			c_pcseti("CC",_NhlGetGksCi(plot->base.wkptr,
							wklp->line_label_color));
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
        }

	c_dpsetc("DPT",buffer);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	/*
	 * Use .8571 as multiplier to determine WOC from Font Height.
	 * This comes from 16(PW)/21(PH)*1/.8888(SA) Principal Width and Height
	 * and SIZA multiplyer in PlotChar.  Therefore, we need to make
	 * sure plotchar is using these default values.  This means it is
	 * not possible to change the font aspect in Line Labels -
	 * ya gotta draw the line somewhere.
	 */
	c_pcseti("PW",16);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("PH",21);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOC",wklp->line_label_font_height * 0.8571);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

        gset_linewidth(wklp->line_thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
        return;
}


/*ARGSUSED*/
static NhlErrorTypes WorkstationLineTo
#if  NhlNeedProto
(NhlLayer l,float x,float y,int upordown)
#else
(l,x,y,upordown)
	NhlLayer l;
	float x;
	float y;
	int upordown;
#endif
{
	char		func[] = "WorkstationLineTo";
	static float	lastx,lasty;
	static int	first = 0;
	int		ix0,iy0,ix1,iy1;

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

NhlErrorTypes _NhlWorkstationLineTo
#if NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance,x,y,upordown)
NhlLayer instance;
float   x;
float y;
int upordown;
#endif
{
	NhlWorkstationLayerClassPart *wcp =
	&((NhlWorkstationLayerClass)instance->base.layer_class)->work_class;

	return (*wcp->lineto_work)(instance,x,y,upordown);
}


/*ARGSUSED*/
void _NhlSetFillInfo
#if  NhlNeedProto
(NhlLayer instance,NhlLayer plot)
#else
(instance,plot)
        NhlLayer instance;
        NhlLayer plot;
#endif
{
	char			func[] = "_NhlSetFillInfo";
        NhlWorkstationLayer	tinst = (NhlWorkstationLayer)instance;
	NhlWorkstationLayerPart	*wk_p = &tinst->work;
        float			fl,fr,fb,ft,ul,ur,ub,ut;
        float			x0,x1;
        int			ll,ix;
        char			buffer[80];
	

	if (wk_p->edges_on && wk_p->edge_dash_pattern < 0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "_NhlSetFillInfo: invalid edge dash pattern index");
		wk_p->edge_dash_pattern = NhlSOLIDLINE;
	}
	else if (wk_p->edges_on && wk_p->edge_dash_pattern > 0) {
		memset((void *) buffer, (char) 0, 80 * sizeof(char));

		c_sflush();

		c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
		if(_NhlLLErrCheckPrnt(NhlFATAL,func))
			return;

		x0 = fl;
		x1 = fl + wk_p->edge_dash_seglen;
		x0 = (float)c_kfpy(x0);
		x1 = (float)c_kfpy(x1);
	
		if ((ix = wk_p->edge_dash_pattern) > wk_p->dash_table_len) {
			/* NhlINFO - but it's a void function right now */
			NhlPError(NhlINFO,NhlEUNKNOWN,
	"_NhlSetLineInfo: using mod function on dash pattern index: %d", ix);

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
	if (wk_p->fill_scale_factor <= 0.0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"_NhlSetFillInfo: fill scale factor must be greater than 0.0");
		wk_p->fill_scale_factor = 1.0;
	}
		
/*
 * An out-of-bounds fill index should have been caught at a higher
 * level. 
 */

	if ((ix = wk_p->fill_index) < NhlHOLLOWFILL) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			   "_NhlSetFillInfo: invalid fill index");
		wk_p->fill_index = NhlHOLLOWFILL;
		
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
			 wk_p->fill_scale_factor);
		(void)_NhlLLErrCheckPrnt(NhlINFO,func);
	}

        return;
}


static NhlErrorTypes WorkstationFill
#if  NhlNeedProto
(NhlLayer l,float *x,float *y,int num_points)
#else
(l,x,y,num_points)
	NhlLayer l;
	float *x;
	float *y;
	int num_points;
#endif
{
	char			func[] = "WorkstationFill";
        NhlWorkstationLayer	inst = (NhlWorkstationLayer)l;
	NhlWorkstationLayerPart	*wk_p = &inst->work;
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
	fill_color = (wk_p->fill_color == NhlTRANSPARENT) ? NhlTRANSPARENT : 
		_NhlGetGksCi(inst->base.wkptr, wk_p->fill_color);
	fill_background = (wk_p->fill_background < 0) ?
		wk_p->fill_background :
			_NhlGetGksCi(inst->base.wkptr, wk_p->fill_background);

/*
 * Draw the fill, unless a negative fill index or Transparent fill color
 * is specified (implying no fill)
 */
	if (fill_color == NhlTRANSPARENT)
	/*SUPPRESS570*/
		;
	else if ((ix = wk_p->fill_index) == NhlSOLIDFILL) {
		/* fill_specs[ix].type  must be 0 */
		gset_fill_int_style(1);
		gset_linewidth(wk_p->fill_line_thickness);
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
			gset_fill_int_style(1);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			c_sfseti("type of fill", 0);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,
							fill_background);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		gset_linewidth(wk_p->fill_line_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_sfseti("TY", fill_specs[ix].type);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
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
	if (wk_p->edges_on) {
		gset_line_colr_ind((Gint)_NhlGetGksCi(inst->base.wkptr,
						      wk_p->edge_color));
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		gset_linewidth(wk_p->edge_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		if (wk_p->edge_dash_pattern > 0) {
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

NhlErrorTypes CallWorkstationFill
#if  NhlNeedProto
(NhlLayerClass lc, NhlLayer instance,  float *x, float *y, int num_points)
#else
(lc, instance,  x, y, num_points)
NhlLayerClass lc;
NhlLayer instance;
float *x;
float *y;
int num_points;
#endif
{
        NhlWorkstationLayerClass tlc = (NhlWorkstationLayerClass)lc;

        if(tlc->work_class.fill_work == NULL){
                if(tlc->base_class.superclass != NULL) {
                        return(CallWorkstationFill(lc->base_class.superclass,
						   instance,x,y,num_points));
                } else {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlWorkstationFill: Transformation object of type (%s) does not have fill_work function",
				  tlc->base_class.class_name);
                        return(NhlWARNING);
                }
        } else {
                return((*tlc->work_class.fill_work)(instance,x,y,num_points));
        }
}

NhlErrorTypes _NhlWorkstationFill
#if NhlNeedProto
(NhlLayer instance, float *x, float *y, int num_points)
#else
(instance,x,y,num_points)
NhlLayer instance;
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
#if  NhlNeedProto
(int instance, 
 char *mark_string, 
 float x_off, 
 float y_off,
 float aspect_adj,
 float size_adj)
#else
(instance,mark_string,x_off,y_off,aspect_adj,size_adj)
        int instance;
	char *mark_string; 
	float x_off; 
	float y_off;
	float aspect_adj;
	float size_adj;
#endif
{
        NhlWorkstationLayer tinst = 
		(NhlWorkstationLayer)_NhlGetLayer(instance);
	NhlWorkstationLayerPart *wk_p;
	NhlMarkerSpec *m_p;
	int i;

	if((tinst == NULL) || !_NhlIsWorkstation(tinst)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlNewMarker:Invalid workstation id = %d",instance);
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
 * Function:	nhl_fnewmarker
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
void _NHLCALLF(nhl_fnewmarker,NHL_FNEWMARKER)
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
NhlErrorTypes NhlSetMarker
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
 * Function:	nhl_fsetmarker
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
void _NHLCALLF(nhl_fsetmarker,NHL_FSETMARKER)
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

/*ARGSUSED*/
void _NhlSetMarkerInfo
#if  NhlNeedProto
(NhlLayer instance,NhlLayer plot)
#else
(instance,plot)
        NhlLayer instance;
        NhlLayer plot;
#endif
{
	char			func[] = "_NhlSetMarkerInfo";
        NhlWorkstationLayer	tinst = (NhlWorkstationLayer)instance;
	NhlWorkstationLayerPart	*wk_p = &tinst->work;
	_NhlMarkerInfo		*mkp = &wk_p->private_markinfo;
        float			fl,fr,fb,ft,ul,ur,ub,ut;
        float			x0,x1;
        int			ll,ix;
        char			buffer[80];


	if (wk_p->marker_lines_on && wk_p->marker_line_dash_pattern < 0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		  "_NhlSetMarkerInfo: invalid marker dash pattern index");
		wk_p->marker_line_dash_pattern = NhlSOLIDLINE;
	}
	else if (wk_p->edges_on && wk_p->marker_line_dash_pattern > 0) {
		memset((void *) buffer, 0, 80 * sizeof(char));

		c_sflush();

		c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
		if(_NhlLLErrCheckPrnt(NhlFATAL,func))
			return;

		x0 = fl;
		x1 = fl + wk_p->marker_line_dash_seglen;
		x0 = (float)c_kfpy(x0);
		x1 = (float)c_kfpy(x1);
	
		if ((ix = wk_p->marker_line_dash_pattern) > 
		    wk_p->dash_table_len) {
			/* NhlINFO - but it's a void function right now */
			NhlPError(NhlINFO,NhlEUNKNOWN,
       "_NhlSetMarkerInfo: using mod function on dash pattern index: %d", ix);

			ix = 1 + (ix - 1) % wk_p->dash_table_len;
		}
		wk_p->marker_line_dash_dollar_size = (x1 - x0) /
			strlen(dash_patterns[ix]) + 0.5;
		if(wk_p->marker_line_dash_dollar_size < 1)
                        wk_p->marker_line_dash_dollar_size = 1;
		
		strcpy(buffer,dash_patterns[ix]);
		
		c_dashdc(buffer,wk_p->marker_line_dash_dollar_size,1);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

/*
 * Make sure the marker size is okay
 */
	if (mkp->marker_size <= 0.0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"_NhlSetMarkerInfo: marker size must be greater than 0.0");
		mkp->marker_size = 0.007;
	}
/*
 * An out-of-bounds marker index should have been caught at a higher
 * level. Error and set to default marker.
 */

	if ((ix = mkp->marker_index) < 0) {
		/* NhlWARNING - but it's a void function right now */
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			   "_NhlSetMarkerInfo: invalid marker index");
		mkp->marker_index = NhlWK_DEF_MARKER;
	}
	else if (ix >wk_p->marker_table_len) {
		/* NhlINFO - but it's a void function right now */
		NhlPError(NhlINFO,NhlEUNKNOWN,
	 "_NhlSetLineInfo: using mod function on marker index: %d", ix);
	}

        return;
}


static NhlErrorTypes WorkstationMarker
#if  NhlNeedProto
(NhlLayer l,float *x,float *y,int num_points)
#else
(l,x,y,num_points)
	NhlLayer l;
	float *x;
	float *y;
	int num_points;
#endif
{
	char			func[] = "WorkstationMarker";
        NhlWorkstationLayer	inst = (NhlWorkstationLayer)l;
	NhlWorkstationLayerPart	*wk_p = &inst->work;
	_NhlMarkerInfo		*mkp = &wk_p->private_markinfo;
        float			fl,fr,fb,ft,ul,ur,ub,ut;
	int			ll, i, index;
	int			save_font;
	Gint			save_linecolor;
	Gint			save_linetype;
	Gdouble			save_linewidth;
	Gint			err_ind;
	float			marker_size, x_off, y_off;
	NhlErrorTypes		ret = NhlNOERROR;
	char			*string;
	int			marker_color,line_color;
	float			p_height, p_width;

/*
 * Flush plotif buffer...
 */
	c_plotif(0.,0.,2);
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
	ginq_linetype(&err_ind, &save_linetype);
	c_pcgeti("FN",&save_font);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	marker_color = _NhlGetGksCi(inst->base.wkptr, mkp->marker_color);

/*
 * If marker lines are on, draw lines connecting the marker points
 */
	if (wk_p->marker_lines_on && num_points > 1) {
		gset_linewidth(wk_p->marker_line_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		line_color = _NhlGetGksCi(inst->base.wkptr, 
					  wk_p->marker_line_color);
		gset_line_colr_ind((Gint) line_color);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		if (wk_p->marker_line_dash_pattern > 0) {
			c_curved(x,y,num_points);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		else {
			c_curve(x,y,num_points);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
	}
		
/*
 * Draw the markers; markers that do not define their own font using
 * a function code are drawn using the default font (font #1).
 */
	c_pcseti("FN", 1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	gset_linewidth(mkp->marker_thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("OC",marker_color);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("CC",marker_color);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	if ((index = mkp->marker_index) <= 0) {
		/* the marker string is used to define the marker */
		x_off = mkp->marker_size * mkp->marker_x_off;
		y_off = mkp->marker_size * mkp->marker_y_off;
		marker_size = marker_table[NhlWK_DEF_MARKER]->size_adj *
					mkp->marker_size;
		x_off = marker_size * (marker_table[NhlWK_DEF_MARKER]->x_off + 
				 mkp->marker_x_off);
		y_off = marker_size * (marker_table[NhlWK_DEF_MARKER]->y_off +
				 mkp->marker_y_off);
		string = marker_table[NhlWK_DEF_MARKER]->marker;
			
		for (i=0; i<num_points; i++) {
			/*
			 * marker size is multiplied by 1.125 to account
			 * for 'SA' parameter of PlotChar.
			 */
			c_plchhq(x[i]+x_off,y[i]+y_off,string,marker_size*1.125,
								0.0,0.0);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
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
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			c_pcsetr("PW",p_width);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			marker_size = marker_table[index]->size_adj *
				mkp->marker_size;
			x_off = marker_size * 
				(marker_table[index]->x_off + 
				 mkp->marker_x_off);
			y_off = marker_size * 
				(marker_table[index]->y_off + 
				 mkp->marker_y_off);
			/*
			 * marker size is multiplied by 1.125 to account
			 * for 'SA' parameter of PlotChar.
			 */
			c_plchhq(x[i]+x_off,y[i]+y_off,
				 marker_table[index]->marker,
				 marker_size*1.125,0.0,0.0);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
	}

/*
 * Restore state
 */

	gset_line_colr_ind(save_linecolor);
	gset_linewidth(save_linewidth);
	gset_linetype(save_linetype);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("FN",save_font);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

/*
 * Flush plotif buffer...
 */
	c_plotif(0.,0.,2);

	return(ret);
}

NhlErrorTypes CallWorkstationMarker
#if  NhlNeedProto
(NhlLayerClass lc, NhlLayer instance,  float *x, float *y, int num_points)
#else
(lc, instance,  x, y, num_points)
NhlLayerClass lc;
NhlLayer instance;
float *x;
float *y;
int num_points;
#endif
{
        NhlWorkstationLayerClass tlc = (NhlWorkstationLayerClass)lc;

        if(tlc->work_class.marker_work == NULL){
                if(tlc->base_class.superclass != NULL) {
                        return(CallWorkstationMarker(
					      lc->base_class.superclass,
					      instance,x,y,num_points));
                } else {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"_NhlWorkstationMarker: Transformation object of type (%s) does not have marker_work function",
				  tlc->base_class.class_name);
                        return(NhlWARNING);
                }
        } else {
                return((*tlc->work_class.marker_work)(instance,
						      x,y,num_points));
        }
}

NhlErrorTypes _NhlWorkstationMarker
#if NhlNeedProto
(NhlLayer instance, float *x, float *y, int num_points)
#else
(instance,x,y,num_points)
NhlLayer instance;
float   *x;
float *y;
int num_points;
#endif
{
        return(CallWorkstationMarker(instance->base.layer_class,
				     instance,x,y,num_points));
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
 * Function:	nhl_fisworkstation
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
void _NHLCALLF(nhl_fisworkstation,NHL_FISWORKSTATION)
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

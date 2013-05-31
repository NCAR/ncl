/*
 *      $Id: Workstation.c,v 1.109 2010-03-27 18:58:25 dbrown Exp $
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
#include <ctype.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/hlu/TransformI.h>
#include <ncarg/hlu/LogLinPlot.h>
#include <ncarg/hlu/color.h>

#define DEBUG_NCGM 0
/* 
 * There are currently 16 pre-defined dash patterns provided plus solid
 * (index 0, or NhlSOLIDLINE). The solid line is element 0 of the dash
 * pattern table but is not counted in the number of dash patterns 
 * returned to the user. Therefore the user can use dash pattern indexes 
 * 0 through (and including) NhlNwkDashTableLength as valid indexes.
 */

static NhlDashSpec Dash_Specs[] = { 
  { "$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$",False },
  { "$$$$__$$$$__$$$$__$$$$__$$$$__$$$$__$$$$__$$$$__",False },
  { "$__$__$__$__$__$__$__$__$__$__$__$__$__$__$__$__",False },
  { "$$$$__$__$$$$__$__$$$$__$__$$$$__$__$$$$__$__",False },
  { "$$$$__$_$__$$$$__$_$__$$$$__$_$__$$$$__$_$__",False },
  { "$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_$$_",False },
  { "$$$_$$$_$$$_$$$_$$$_$$$_$$$_$$$_$$$_$$$_$$$_$$$_",False },
  { "$_$$_$_$$_$_$$_$_$$_$_$$_$_$$_$_$$_$_$$_$_$$_$_$$_",False },
  { "$_$$$_$_$$$_$_$$$_$_$$$_$_$$$_$_$$$_$_$$$_$_$$$_",False },
  { "$$_$$$$_$$_$$$$_$$_$$$$_$$_$$$$_$$_$$$$_$$_$$$$_",False },
  { "$$$$_$$_$_$$_$$$$_$$_$_$$_$$$$_$$_$_$$_$$$$_$$_$_$$_",False },
  { "$$__$$__$$__$$__$$__$$__$$__$$__$$__$$__$$__$$__",False },
  { "$$$$$$__$$$$$$__$$$$$$__$$$$$$__$$$$$$__$$$$$$__",False },
  { "$$$_$$$__$$$_$$$__$$$_$$$__$$$_$$$__$$$_$$$__",False },
  { "$$___$$___$$___$$___$$___$$___$$___$$___$$___$$___",False },
  { "$_$___$_$___$_$___$_$___$_$___$_$___$_$___$_$___",False },
  { "$$$$$_____$$$$$_____$$$$$_____$$$$$_____$$$$$_____",False }
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

static NhlFillSpec Fill_Specs[] = {

{ 0,   0.0,  0, NULL, 0, 0, 0, False},
{ 0,   0.01, 0, NULL, 0, 1, 0, False},
{ 90,  0.01, 0, NULL, 0, 1, 0, False},
{ 45,  0.01, 0, NULL, 0, 1, 0, False},
{ 135, 0.01, 0, NULL, 0, 1, 0, False},
{ 0,   0.01, 0, NULL, 0, 2, 0, False},
{ 45,  0.01, 0, NULL, 0, 2, 0, False},
{ 22,  0.01, 0, NULL, 0, 1, 0, False},
{ 68,  0.01, 0, NULL, 0, 1, 0, False},
{ 112, 0.01, 0, NULL, 0, 1, 0, False},
{ 158, 0.01, 0, NULL, 0, 1, 0, False},
{ 22,  0.01, 0, NULL, 0, 2, 0, False},
{ 68,  0.01, 0, NULL, 0, 2, 0, False},
{ 0,   0.0003125, 0, NULL, 0, -3, 2, False},
{ 0,   0.0003125, 0, NULL, 0, -3, 3, False},
{ 0,   0.0003125, 0, NULL, 0, -4, 3, False},
{ 0,   0.0003125, 0, NULL, 0, -4, 4, False},
{ 45,  0.0075, 1, NULL, 0, 1, 0, False}
};

/* Note: the specs for the user-defined marker should be set to the same
 * values as the default marker - currently the asterisk
 * spec-values: marker string, x_off, y_off, aspect ratio (h/w), size factor,
 * dynamic allocation flag.
 */

static NhlMarkerSpec Marker_Specs[] = {
{"",  1,0.0, 0.0, 1.0, 1.0, 0.0, False},    /* user-defined */
{"Z", 37,0.0, 0.0, 1.0, 0.175, 0.0, False}, 
					/* 1 - dot (small filled circle)*/
{"+", 18,0.0, 0.0, 1.0, 0.8955, 0.0, False}, /* 2 - plus sign */
{"*",  1,0.0, 0.0, 0.765, 1.2868, 0.0, False},    /* 3 - asterisk */
{"x", 19,0.0, 0.0, 1.0, 1.1428, 0.0, False},  /* 4 - hollow circle */
{"U", 18,0.0, 0.0, 1.0, 1.1428, 0.0, False},  /* 5 - cross (x) */
{"Z", 19,0.0, 0.0, 1.0, 1.33333, 0.0, False}, /* 6 - hollow square */
{"[", 19,0.0, 0.0, 1.166667, 1.1428, 0.0, False},    
					/* 7 - up pointing triangle */
{"X", 19,0.0, 0.0, 1.75, 0.5714, 0.0, False},    
					/* 8 - down pointing triangle */
{"\\", 19,0.0, 0.0, 0.6, 1.33333, 0.0, False},   /* 9 - diamond */
{"`", 19,0.0, 0.0, 0.88, 1.8182, 0.0, False}, 
					/* 10-left pointing filled triangle */
{"b", 19,0.0, 0.0, 0.88, 1.8182, 0.0, False},
					/* 11-right pointing filled triangle */
{"]", 19,0.0, 0.0, 1.0, 1.0, 0.0, False}, /* 12 - five-pointed star */
{"m", 19,0.0, 0.0, 0.875, 1.14286, 0.0, False},/* 13 - six-pointed star */
{"Z", 18,0.0, 0.0, 1.0, 0.7620, 0.0, False},    
					/* 14 - circle with center dot */
{"[", 37,0.0, 0.0, 1.0, 0.7620, 0.0, False},    /* 15 - circle with cross */
{"Z", 37,0.0, 0.0, 1.0, 0.7620, 0.0, False}     /* 16 - filled circle */

};

/* 
 * The marker table is global to all workstations. It consists of a
 * dynamically allocated array of pointers to NhlMarkerSpec structs.
 * Since the marker table may be reallocated when a marker is added, each
 * workstation is handed not a pointer to the table, but a pointer to
 * the table pointer.
 * The marker table length kept here is the actual length of the table
 * including the 0 element, which does not represent a real marker. The
 * NhlNwkMarkerTableLength read-only resource equals Marker_Table_Len - 1; 
 * Allocation for new markers is in chunks, so the amount currently
 * allocated, and a pointer to the unused elements is also stored.
 */


static int Marker_Table_Len;

/*
 * Likewise the fill table and dash table are global to all workstations
 * Therefore their length is keep statically by the Workstation Class
 */
static int Fill_Table_Len;
static NhlFillTable Fill_Table;
static int Dash_Table_Len;

static NrmQuark intQ;
static NrmQuark scalarQ;
static NrmQuark intgenQ;
static NrmQuark strgenQ;
static NrmQuark colormap_name;
static NrmQuark bkgnd_name;
static NrmQuark foregnd_name;
static NrmQuark	marker_tbl_strings_name;
static NrmQuark marker_tbl_params_name;
static NrmQuark dash_table_name;
static NrmQuark def_graphic_style_id_name;
static NrmQuark views_name;
static NrmQuark top_level_views_name;

#define Oset(field) NhlOffset(NhlWorkstationLayerRec,work.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{NhlNwkColorMap,NhlCwkColorMap,NhlTColorMap,sizeof(NhlGenArray),
		Oset(color_map),NhlTString,_NhlUSET("ncl_default"),
		_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkColorMapLen,NhlCwkColorMapLen,NhlTInteger,sizeof(int),
		Oset(color_map_len),NhlTImmediate,
		_NhlUSET(0),_NhlRES_GONLY,NULL},
        {NhlNwkBackgroundOpacityF,NhlCBackgroundOpacityF,NhlTFloat,
                sizeof(float),Oset(bkgnd_opacity),NhlTString,
                _NhlUSET("1.0"), _NhlRES_DEFAULT,NULL},	
	{NhlNwkBackgroundColor,NhlCBackgroundColor,
		NhlTColorDefinitionGenArray,
		sizeof(NhlPointer),Oset(bkgnd_color),NhlTImmediate,
		_NhlUSET(NULL),_NhlRES_DEFAULT,(NhlFreeFunc)NhlFreeGenArray},
        {NhlNwkForegroundColor,NhlCForegroundColor,
		NhlTColorDefinitionGenArray,
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
	{NhlNwkViews,NhlCwkViews,NhlTObjIdGenArray,
		sizeof(NhlGenArray),Oset(views),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_GONLY,
         	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkTopLevelViews,NhlCwkTopLevelViews,NhlTObjIdGenArray,
		sizeof(NhlGenArray),Oset(top_level_views),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_GONLY,
         	(NhlFreeFunc)NhlFreeGenArray},
	{NhlNwkAntiAlias,NhlCwkAntiAlias,NhlTAntiAlias,
		sizeof(NhlwkAntiAlias),Oset(antialias),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlANTIALIAS_ON),_NhlRES_DEFAULT,
         	NULL},
                                
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
    {NhlNwkLineOpacityF,NhlCwkLineOpacityF,NhlTFloat,
    	sizeof(float),POset(line_opacity),NhlTString,
    	_NhlUSET("1.0"),
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
		 _NhlUSET((NhlPointer)21),
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
		_NhlUSET("~"),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
#undef POset
#define POset(field) Oset(public_markinfo.field)
	{NhlNwkMarkerIndex,NhlCwkMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),POset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
	{NhlNwkMarkerColor,NhlCwkMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_DEFAULT|_NhlRES_PRIVATE,NULL},
    {NhlNwkMarkerOpacityF,NhlCwkMarkerOpacityF,NhlTFloat,
    	sizeof(float),POset(marker_opacity),NhlTString,
    	_NhlUSET("1.0"),
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
   	{_NhlNwkLineOpacityF,_NhlCwkLineOpacityF,NhlTFloat,
   		sizeof(float),POset(line_opacity),NhlTString,
   		_NhlUSET("1.0"),
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
		 _NhlUSET((NhlPointer)21),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
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
		_NhlUSET("~"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
#undef POset

#define POset(field) Oset(private_markinfo.field)
	{_NhlNwkMarkerIndex,_NhlCwkMarkerIndex,NhlTMarkerIndex,
		sizeof(NhlMarkerIndex),POset(marker_index),NhlTImmediate,
		_NhlUSET((NhlPointer)3),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNwkMarkerColor,_NhlCwkMarkerColor,NhlTColorIndex,
		sizeof(NhlColorIndex),POset(marker_color),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlFOREGROUND),
         	_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
    {_NhlNwkMarkerOpacityF,_NhlCwkMarkerOpacityF,NhlTFloat,
    	sizeof(float),POset(marker_opacity),NhlTString,
    	_NhlUSET("1.0"),
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
	{_NhlNwkFillOpacityF,_NhlCwkFillOpacityF,NhlTFloat,
		sizeof(float),POset(fill_opacity),NhlTString,
		_NhlUSET("1.0"),
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
	{_NhlNwkFillDotSizeF,_NhlCwkFillDotSizeF,NhlTFloat,
		sizeof(float),POset(fill_dot_size),NhlTString,
		_NhlUSET("0.0"),_NhlRES_SGONLY|_NhlRES_PRIVATE,NULL},
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
		sizeof(NhlGenArray),Oset(dash_table_strings),NhlTImmediate,
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

static _NhlRawObjCB callbacks[] = {
	{_NhlCBworkColorIndexChange,
			NhlOffset(NhlWorkstationLayerRec,work.color_index_cb),
		8,NULL,NULL,NULL},
	{_NhlCBworkColorMapChange,
			NhlOffset(NhlWorkstationLayerRec,work.colormap_cb),
		0,NULL,NULL,NULL},
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
	NhlWorkstationLayer	wl,
	NhlPrivateColor		*new,
	NhlPrivateColor		*old
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

extern void _NHLCALLF(gopwk,GOPWK) (
#if	NhlNeedProto
	int	*wkid,
        int	*conid,
        int	*wtype
#endif
);

static void StoreGksWksType (
#if	NhlNeedProto
        int	gks_id,
        int	wtype
#endif
);


/*
 * Only MAX_OPEN_WKS Workstation instances are allowed to be open at one time.
 * The GKS Segment workstation is opened by the View class initialize
 * without using the Workstation class interface. Therefore, from the
 * Workstation class's point of view, one GKS workstation is unavailable.
 */
 
static int	CurrentWksCount = 0;
static wkGksWksRec Gks_Wks_Recs[MAX_OPEN_WKS] =
	{ {-1},{-1},{-1} }; /* uninitialized */

static NhlBoolean Hlu_Wks_Flag = False;

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
/* callbacks			*/	callbacks,
/* num_callbacks		*/	NhlNumber(callbacks),
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
/* gks_wks_recs		*/	Gks_Wks_Recs,
/* hlu_wks_flag		*/	&Hlu_Wks_Flag,                                
/* def_background	*/	{0.0,0.0,0.0},
/* rgb_dbm		*/	NULL,
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
/* marker_work		*/	WorkstationMarker,
/* notify_work		*/	NULL,
/* update_drawbb        */      NULL
	}
};

NhlClass NhlworkstationClass = (NhlClass)&NhlworkstationClassRec;

/*
 * Function:	_NhlLookupColor
 *
 * Description:	This function is taken largely from the X11R5 distribution
 *		from MIT.  I combined the client and server sides of the
 *		named color codes in X to create this.  The MIT copyright
 *		follows:
 *
****************************************************************************
	Copyright    Massachusetts Institute of Technology    1985
Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation, and that the name of M.I.T. not be used in advertising or
publicity pertaining to distribution of the software without specific,
written prior permission.  M.I.T. makes no representations about the
suitability of this software for any purpose.  It is provided "as is"
without express or implied warranty.
****************************************************************************
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
_NhlLookupColor
#if	NhlNeedProto
(
	NhlWorkstationClass	wc,
	Const char		*name,
	NGRGB			*rgb
)
#else
(wc,name,rgb)
	NhlWorkstationClass	wc;
	Const char		*name;
	NGRGB			*rgb;
#endif
{
	register int	len,i;
	char		c;
	char		lname_buff[64];
	char		*lname;
	char		*s;
	NGdatum		rgbdata;
	unsigned short	r,g,b;
	float		rf,gf,bf;

	if(!name)
		return False;

	len = strlen(name);

	if(sscanf(name," (/ %f , %f , %f /) ",&rf,&gf,&bf) == 3){
		rgb->red = rf * 65535;
		rgb->green = gf * 65535;
		rgb->blue = bf * 65535;
		return True;
	}
	/*
	 * look for a colorspec string
	 */
	else if(*name == '#'){
		/*
		 * Hex RGB's
		 */
		name++;
		len--;
		if((len != 3) && (len != 6) && (len != 9) && (len != 12))
			return False;
		len /= 3;
		g = b = 0;
		do {
			r = g;
			g = b;
			b = 0;
			for(i = len; --i >= 0;){
				c = tolower(*name++);
				b <<= 4;
				if(c >= '0' && c <= '9')
					b |= c - '0';
				else if(c >= 'a' && c <= 'f')
					b |= c - ('a' - 10);
				else
					return False;
			}
		} while(*name != '\0');
		len <<=2;
		len = 16 - len;
		rgb->red = r << len;
		rgb->green = g << len;
		rgb->blue = b << len;
	}
	else{

		/*
		 * Look in database
		 */
		if(!wc->work_class.rgb_dbm)
			return False;

		lname = (len >= sizeof(lname_buff))?
					NhlMalloc((unsigned)len+1):lname_buff;
		s = lname;
		strcpy(lname,name);
		while(*s){
			if(isupper(*s))
				*s = tolower(*s);
			s++;
		}

		rgbdata.dptr = lname;
		rgbdata.dsize = len;	/* don't include \0 */

		rgbdata = NGdbm_fetch(wc->work_class.rgb_dbm,rgbdata);

		if(lname != lname_buff)
			NhlFree(lname);

		if(!rgbdata.dptr)
			return False;

		memcpy(rgb,rgbdata.dptr,sizeof(NGRGB));
	}

	return True;
}

static NhlBoolean
FindCIMatch
#if	NhlNeedProto
(
	NhlWorkstationLayer	wl,
	NGRGB			rgb,
	int			*ci
)
#else
(wl,rgb,ci)
	NhlWorkstationLayer	wl;
	NGRGB			rgb;
	int			*ci;
#endif
{
	char		func[]="FindCIMatch";
	int		i;
	float		t,cerr,err;
	NhlPrivateColor	*pcmap = wl->work.private_color_map;

	if(pcmap[0].cstat != _NhlCOLSET){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	"%s:Color Matching cannot happen before colormap initialization",func);
		return False;
	}

	*ci = 0;
	t = (pcmap[0].red*65535) - rgb.red;
	cerr = t*t;
	t = (pcmap[0].green*65535) - rgb.green;
	cerr += (t*t);
	t = (pcmap[0].blue*65535) - rgb.blue;
	cerr += (t*t);

	for ( i = 1; i < _NhlMAX_COLOR_MAP; i++) {
		if(pcmap[i].cstat != _NhlCOLSET) continue;
		t = (pcmap[i].red*65535) - rgb.red;
		err = t*t;
		t = (pcmap[i].green*65535) - rgb.green;
		err += (t*t);
		t = (pcmap[i].blue*65535) - rgb.blue;
		err += (t*t);
		if(err < cerr){
			cerr = err;
			*ci = i;
		}
	}

	return True;
}

int
NhlGetNamedColorIndex
#if	NhlNeedProto
(
	int		pid,
	Const char	*name
)
#else
(pid,name)
	int		pid;
	Const char	*name;
#endif
{
	char			func[]="NhlGetNamedColorIndex";
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)_NhlGetLayer(pid);
	NGRGB			rgb;
	int			index;

	if(!wl || !_NhlIsWorkstation(wl)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation",func));
		return NhlFATAL;
	}

	if(_NhlLookupColor((NhlWorkstationClass)wl->base.layer_class,
				name,&rgb) && FindCIMatch(wl,rgb,&index))
		return index;
	return NhlFATAL;
}

/*
 * Function:	CvtStringToColorIndex
 *
 * Description:	This is a type converter to convert string's to enumerations
 *		defined by the args. If the string is not found in the args,
 *		the string is checked to see if it is a valid colorname, and
 *		the rgb database is checked.
 *
 * In Args:	NrmValue		*from	ptr to from data
 *		NhlConvertArgList	args	args for conversion
 *		int			nargs	number of args
 *		
 *
 * Out Args:	NrmValue		*to	ptr to to data
 *
 * Scope:	Global public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CvtStringToColorIndex
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char			func[] = "CvtStringToColorIndex";
	NhlLayer		l;
	NhlWorkstationLayer	wl;
	int			i, tmp=0;
	NhlBoolean		set = False;
	NhlString		s1 = from->data.strval;
	NhlString		t2 = NULL;
	NrmValue		val;
	NhlGenArray		sgen;
	NGRGB			rgb;
	NhlErrorTypes		ret = NhlNOERROR;

	if(nargs < 2){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	l = *(NhlLayer*)args[0].data.ptrval;
	if(_NhlIsObj(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:%s resources are not valid for Obj sub-classes",
			func,NrmQuarkToString(to->typeQ));
		to->size = 0;
		return NhlFATAL;
	}
	wl = (NhlWorkstationLayer)l->base.wkptr;

	if(isdigit((int)*s1) || (*s1 == '-')){
		tmp = (int)strtol(s1,&t2,10);
		if(!tmp && (s1 == t2)){
			NhlPError(NhlINFO,NhlEUNKNOWN,
				"%s:Can't Convert \"%s\"",func,s1);
			to->size = 0;
			return NhlFATAL;
		}
		val.size = sizeof(int);
		val.data.intval = tmp;

		return _NhlReConvertData(intQ,to->typeQ,&val,to);
	}

/*
	if (strcasecmp(s1, "transparent") == 0) {
		tmp = NhlTRANSPARENT;
		set = True;
	}
*/

	for(i=1;i<nargs;i++){
		if(_NhlCmpString(args[i].data.strval,s1) == 0){
			tmp = args[i].size;
			set = True;
			break;
		}
	}

	/***** RLB
	if(!set &&
		_NhlLookupColor((NhlWorkstationClass)wl->base.layer_class,
								s1,&rgb) &&
		FindCIMatch(wl,rgb,&tmp))
			set = True;
******* NOT SURE WHAT CASE THIS WAS INTENDED TO HANDLE ?????
	if(!set){
		sgen = _NhlStringToStringGenArray(s1);
		if(sgen){
			val.size = sizeof(NhlGenArray);
			val.data.ptrval = sgen;

			return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
		}

		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s: Unable to convert string \"%s\" to requested type",
								func,s1);
		to->size = 0;
		return NhlFATAL;
	}
********/

	if (!set) {
		if (!_NhlLookupColor((NhlWorkstationClass)wl->base.layer_class, s1, &rgb)) {
			NhlPError(NhlFATAL, NhlEUNKNOWN,
					"%s: Unable to convert string \"%s\" to requested type",
					func, s1);
			to->size = 0;
			return NhlFATAL;
		}

		tmp = ALPHA_OPAQUE |
				((rgb.red   / 256) << 16) |
				((rgb.green / 256) <<  8) |
				((rgb.blue  / 256));
	}

	_NhlSetVal(int,sizeof(int),tmp);
}

/*
 * Function:	CvtStringGenArrayToColorIndexGenArray
 *
 * Description:
 *
 * In Args:
 *		
 *
 * Out Args:
 *
 * Scope:
 * Returns:
 * Side Effect:	
 */
static NhlErrorTypes
CvtStringGenArrayToColorIndexGenArray
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char		func[] = "CvtStringGenArrayToColorIndexGenArray";
	NhlLayer		l;
	NhlWorkstationLayer	wl;
	int			i,j,tmp=0;
	NhlBoolean		set = False;
	NhlString		*sdata;
	NhlString		s1,s2;
	NhlGenArray		sgen = from->data.ptrval;
	NhlGenArray		tgen;
	int			*tint;
	NhlErrorTypes		ret = NhlNOERROR,lret = NhlNOERROR;
	char			enum_name[_NhlMAXRESNAMLEN];
	NrmQuark		enumQ;
	NrmValue		ival,eval;

	if(nargs < 2){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}
	l = *(NhlLayer*)args[0].data.ptrval;
	if(_NhlIsObj(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:%s resources are not valid for Obj sub-classes",
			func,NrmQuarkToString(to->typeQ));
		to->size = 0;
		return NhlFATAL;
	}
	wl = (NhlWorkstationLayer)l->base.wkptr;

	if(!sgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),sgen);
	}
	sdata = sgen->data;

	/*
	 * What is the name for a single element of the to GenArray?
	 */
	s1 = NrmQuarkToString(to->typeQ);
	strcpy(enum_name,s1);
	s1 = strstr(enum_name,NhlTGenArray);
	if(!s1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid \"to\" type %s ???",
			func,NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}
	*s1 = '\0';
	enumQ = NrmStringToQuark(enum_name);

	tgen = _NhlConvertCreateGenArray(NULL,enum_name,sizeof(int),
				sgen->num_dimensions,sgen->len_dimensions);
	if(!tgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}
	tint = NhlConvertMalloc(sizeof(int) * sgen->num_elements);
	if(!tint){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}

	tgen->data = tint;

	for(i=0;i < sgen->num_elements;i++){
		s1 = sdata[i];
		set = False;

		if(isdigit((int)*s1) || (*s1 == '-')){
			tmp = (int)strtol(s1,&s2,10);
			ival.size = sizeof(int);
			ival.data.intval = tmp;
			eval.size = sizeof(int);
			eval.data.ptrval = &tint[i];

			if(tmp || (s1 != s2)){
				lret =_NhlReConvertData(intQ,enumQ,&ival,&eval);
				if(lret == NhlNOERROR)
					set = True;
			}
		}
		else{
			NGRGB	rgb;

			for(j=0;j<nargs;j++){
				if(_NhlCmpString(args[j].data.strval,s1) == 0){
					tint[i] = args[j].size;
					set = True;
					break;
				}
			}
			if(!set && _NhlLookupColor((NhlWorkstationClass)wl->base.layer_class,s1,&rgb)) {
			/* recall that these are 16-bit color-components, whereas we want 8-bit components */
				tint[i] = ALPHA_OPAQUE |
						((rgb.red   / 256) << 16) |
						((rgb.green / 256) << 8) |
						((rgb.blue  / 256));
				set = True;
			}
		}

		if(!set){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s: Unable to convert string \"%s\" to requested type",
								func,s1);
			to->size = 0;
			return NhlFATAL;
		}
	}

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
}

typedef enum {LP,RP,SPACE,CHAR,COMMA,ENDOFSTRING} _NhlCITokens;

static _NhlCITokens
NextCIToken
#if	NhlNeedProto
(
	Const char	*str,
	int		*index
)
#else
(str,index)
	Const char	*str;
	int		*index;
#endif
{
	(*index)++;
	switch(str[*index-1]){
	case '(':
		if(str[*index] == '/') {
			(*index)++;
			return(LP);
		} else {
			return(CHAR);
		}
	case '/':
		if(str[*index] == ')') {
			(*index)++;
			return(RP);
		} else {
			return(CHAR);
		}
	case ' ':
	case '\t':
		return(SPACE);
	case ',':
		return(COMMA);
	case '\0':
		return(ENDOFSTRING);
	default:
		return(CHAR);
	}
}

NhlGenArray
_NhlStringToColorDefStringGenArray
#if	NhlNeedProto
(
	NhlWorkstationClass	wc,
	Const char*		str,
	NhlBoolean		doerror
)
#else
(wc,str,doerror)
	NhlWorkstationClass	wc;
	Const char*		str;
	NhlBoolean		doerror;
#endif
{
	int		done = 0;
	int		state = 1;
	_NhlCITokens	token;
	char		**ptr;
	char		*data;
	ng_size_t  ptri = 0;
	int		datai=0;
	int		stri = 0;
	

	if(str == NULL) return(NULL);	
	while(!done) {
		token = NextCIToken(str,&stri);
		switch(state) {
		case 0:
			done = 1;
			break;
		case 1:
			switch(token) {
			case LP:
				data = NhlConvertMalloc(strlen(str) + 1);
				ptr = NhlConvertMalloc(sizeof(char*) *
								strlen(str));
				memset((char*)ptr,0,sizeof(char*)*strlen(str));
				state = 2;
				break;
			case SPACE:
				break;
			default:
				return NULL;
			}
			break;
		case 2:
			switch(token) {
			case SPACE:
				break;
			case CHAR:
				ptr[ptri++] = &(data[datai]);
				data[datai++] = str[stri-1];
				state = 11;
				break;
			case LP:
				ptr[ptri++] = &(data[datai]);
				data[datai++] = str[stri-2];
				data[datai++] = str[stri-1];
				state = 3;
				break;
			default:
				state = 0;
				break;
			}
			break;
		case 3:
			switch(token) {
			case SPACE:
				break;
			case CHAR:
				data[datai++] = str[stri-1];
				state = 13;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 4:
			switch(token) {
			case SPACE:
				break;
			case COMMA:
				data[datai++] = str[stri-1];
				state = 5;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 5:
			switch(token) {
			case SPACE:
				break;
			case CHAR:
				data[datai++] = str[stri-1];
				state = 14;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 6:
			switch(token) {
			case SPACE:
				break;
			case COMMA:
				data[datai++] = str[stri-1];
				state = 7;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 7:
			switch(token) {
			case SPACE:
				break;
			case CHAR:
				data[datai++] = str[stri-1];
				state = 8;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 8:
			switch(token) {
			case RP:
				data[datai++] = str[stri-2];
				data[datai++] = str[stri-1];
				state=10;
				break;
			case CHAR:
				data[datai++] = str[stri-1];
				break;
			case SPACE:
				state=9;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 9:
			switch(token) {
			case RP:
				data[datai++] = str[stri-2];
				data[datai++] = str[stri-1];
				state=10;
				break;
			case SPACE:
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 10:
			switch(token) {
			case RP:
				data[datai++] = '\0';
				state=12;
				done=1;
				break;
			case COMMA:
				data[datai++] = '\0';
				state=2;
				break;
			case SPACE:
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 11:
			switch(token) {
			case RP:
			case COMMA:
				/*
				 * remove trailing whitespace before
				 * terminating string.
				 */
				datai--;
				while(isspace(data[datai]))
					datai--;
				datai++;
				data[datai++] = '\0';
				if(token == RP){
					state=12;
					done=1;
				}
				else
					state=2;
				break;
			case CHAR:
			case SPACE:
				data[datai++] = str[stri-1];
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 12:
			done = 1;
			break;
		case 13:
			switch(token) {
			case CHAR:
				data[datai++] = str[stri-1];
				break;
			case COMMA:
				data[datai++] = str[stri-1];
				state=5;
				break;
			case SPACE:
				state=4;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		case 14:
			switch(token) {
			case CHAR:
				data[datai++] = str[stri-1];
				break;
			case COMMA:
				data[datai++] = str[stri-1];
				state=7;
				break;
			case SPACE:
				state=6;
				break;
			default:
				state = 0;
                                break;
			}
			break;
		default:
			state = 0;
			done = 1;
			break;
		}
	}

	if(state != 12){
		if(doerror)
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Syntax error parsing resource file array");
		return NULL;
	}

	return _NhlConvertCreateGenArray(ptr,NhlTString,sizeof(char*),1,&ptri);
}

/*
 * Function:	CvtStringToColorIndexGenArray
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
CvtStringToColorIndexGenArray
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char			func[] = "CvtStringToColorIndexGenArray";
	NhlLayer		l;
	NhlWorkstationLayer	wl;
	NhlString		s1 = from->data.strval;
	NrmValue		val;
	NhlGenArray		sgen;
	NhlPointer		data;

	if(nargs != 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	l = *(NhlLayer*)args[0].data.ptrval;
	if(_NhlIsObj(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:%s resources are not valid for Obj sub-classes",
			func,NrmQuarkToString(to->typeQ));
		to->size = 0;
		return NhlFATAL;
	}
	wl = (NhlWorkstationLayer)l->base.wkptr;

	/*
	 * First try and create a color Def String Array.
	 */
	sgen = _NhlStringToColorDefStringGenArray(
			(NhlWorkstationClass)wl->base.layer_class,s1,True);
	/*
	 * Then, try and create an old fashioned string Array
	 */
	if(!sgen)
		sgen = _NhlStringToStringGenArray(s1);

	/*
	 * If all that failed, then treat the string as a single element
	 * of a string array.
	 */
	if(!sgen){
		data = NhlConvertMalloc(from->size);
		if(data == NULL){
			NhlPError(NhlFATAL,ENOMEM,"%s",func);
			return NhlFATAL;
		}

		if(s1){
			NhlString	*sptr = data;
			*sptr = NhlConvertMalloc(strlen(s1)+1);
			if(!*sptr){
				NhlPError(NhlFATAL,ENOMEM,"%s",func);
				return NhlFATAL;
			}
			strcpy(*sptr,s1);
		}
		else
			*(NhlString *)data = NULL;

		sgen = _NhlConvertCreateGenArray(data,NhlTString,
						sizeof(NhlString),1,NULL);
	}
	/*
	 * Now, if we don't have an sgen, we had a memory problem.
	 */
	if(!sgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}

	val.size = sizeof(NhlGenArray);
	val.data.ptrval = sgen;

	return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
}

/*
 * Function:	CvtColorDefinitionGenArrayToColorIndexGenArray
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
CvtColorDefinitionGenArrayToColorIndexGenArray
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char	func[] = "CvtColorDefinitionGenArrayToColorIndexGenArray";
	NhlLayer		l;
	NhlWorkstationLayer	wl;
	NrmValue		val;
	char			enum_name[_NhlMAXRESNAMLEN];
	NhlString		s1;
	NrmQuark		enumQ;
	NhlGenArray		fgen = from->data.ptrval;
	NhlGenArray		tgen;
	NhlErrorTypes		ret = NhlNOERROR;
	float			*fptr;
	int			*iptr;
	int			i;

	if(nargs != 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	l = *(NhlLayer*)args[0].data.ptrval;
	if(_NhlIsObj(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:%s resources are not valid for Obj sub-classes",
			func,NrmQuarkToString(to->typeQ));
		to->size = 0;
		return NhlFATAL;
	}
	wl = (NhlWorkstationLayer)l->base.wkptr;

	if(!fgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fgen);
	}

	if((fgen->num_dimensions != 2) || (fgen->len_dimensions[1] != 3 && fgen->len_dimensions[1] != 4)){
		if((fgen->num_elements == 1) &&
			(fgen->size <= sizeof(NhlArgVal)) && (fgen->size > 0)){

			memcpy((char*)&val.data,(char*)fgen->data,fgen->size);
			val.size = fgen->size;

			return _NhlReConvertData(fgen->typeQ,to->typeQ,&val,to);
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:array has wrong dimensionality",func);
		return NhlFATAL;
	}

	val.size = sizeof(NhlGenArray);
	val.data.ptrval = &fgen;
	if(_NhlReConvertData(from->typeQ,NrmStringToQuark(NhlTFloatGenArray),
						from,&val) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to convert from %s to %s",func,
			NrmQuarkToString(from->typeQ),NhlTFloatGenArray);
		return NhlFATAL;
	}

	/*
	 * We now have a 2 dimensional float array - need to turn it into
	 * a 1-d array of ColorIndex Type - need to generate the actual
	 * name for the ColorIndex Type since there are multiples.
	 */
	s1 = NrmQuarkToString(to->typeQ);
	strcpy(enum_name,s1);
	s1 = strstr(enum_name,NhlTGenArray);
	if(!s1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid \"to\" type %s ???",
			func,NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}
	*s1 = '\0';
	enumQ = NrmStringToQuark(enum_name);
	tgen = _NhlConvertCreateGenArray(NULL,enum_name,sizeof(int),
			1,fgen->len_dimensions);
	if(!tgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}
	iptr = NhlConvertMalloc(sizeof(int) * fgen->len_dimensions[0]);
	if(!iptr){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}
	tgen->data = iptr;
	fptr = fgen->data;

	int colorStride = fgen->len_dimensions[1];
	for(i=0;i < fgen->len_dimensions[0];i++){
		unsigned int red = *(fptr+(colorStride*i)) * 255;
		unsigned int green = *(fptr+(colorStride*i)+1) * 255;
		unsigned int blue = *(fptr+(colorStride*i)+2) * 255;
		unsigned int alpha = (fgen->len_dimensions[1] == 4)
				? (int)(*(fptr+(colorStride*i)+3) * 63) << 24 | ALPHA_MASK
				: ALPHA_OPAQUE;
		iptr[i] = alpha |
				(red   << 16) |
				(green << 8) |
				blue;
	}

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
}

/*
 * Function:	CvtColorDefinitionGenArrayToColorIndex
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
CvtColorDefinitionGenArrayToColorIndex
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char	func[] = "CvtColorDefinitionGenArrayToColorIndex";
	NhlLayer		l;
	NhlWorkstationLayer	wl;
	NrmValue		val;
	NhlGenArray		fgen = from->data.ptrval;
	NhlErrorTypes		ret = NhlNOERROR;
	float			*fptr;
	int			tint;
	int			r, g, b, a;

	if(nargs != 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Called with improper number of args",func);
		to->size = 0;
		return NhlFATAL;
	}

	l = *(NhlLayer*)args[0].data.ptrval;
	if(_NhlIsObj(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:%s resources are not valid for Obj sub-classes",
			func,NrmQuarkToString(to->typeQ));
		to->size = 0;
		return NhlFATAL;
	}
	wl = (NhlWorkstationLayer)l->base.wkptr;

	if(!fgen){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:%s to %s with a NULL array",
			func,NrmQuarkToString(from->typeQ),
			NrmQuarkToString(to->typeQ));
		return NhlFATAL;
	}

	if((fgen->num_dimensions != 1) ||
			!(fgen->len_dimensions[0] == 3 || fgen->len_dimensions[0] == 4)) {
		if((fgen->num_elements == 1) &&
			(fgen->size <= sizeof(NhlArgVal)) && (fgen->size > 0)){

			memcpy((char*)&val.data,(char*)fgen->data,fgen->size);
			val.size = fgen->size;

			return _NhlReConvertData(fgen->typeQ,to->typeQ,&val,to);
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:array has wrong dimensionality",func);
		return NhlFATAL;
	}

	val.size = sizeof(NhlGenArray);
	val.data.ptrval = &fgen;
	if(_NhlReConvertData(from->typeQ,NrmStringToQuark(NhlTFloatGenArray),
						from,&val) < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to convert from %s to %s",func,
			NrmQuarkToString(from->typeQ),NhlTFloatGenArray);
		return NhlFATAL;
	}

	/*
	 * We now have a 2 dimensional float array - need to turn it into
	 * a ColorIndex Type
	 */
	fptr = fgen->data;
	r = (int)(*(fptr) * 255) << 16;
	g = (int)(*(fptr+1) * 255) << 8;
	b = (int)(*(fptr+2) * 255);
	a = (fgen->len_dimensions[0] == 4) ?
			(int)(*(fptr+3) * 63) << 24 | ALPHA_MASK :
			ALPHA_OPAQUE;
	tint = a | r | g | b;

	_NhlSetVal(int,sizeof(int),tint);
}

/*
 * Function:	CvtStringToColorDefinitionGenArray
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
CvtStringToColorDefinitionGenArray
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char	func[] = "CvtStringToColorDefinitionGenArray";
	NhlWorkstationClass	wc;
	NhlString		s1 = from->data.strval;
	NhlErrorTypes		ret = NhlNOERROR;
	NGRGB			rgb;

	if((nargs != 1) && (args[0].addressmode != NhlLAYEROFFSET)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid args?",func);
		return NhlFATAL;
	}
	wc = *(NhlWorkstationClass*)args[0].data.ptrval;

	if(_NhlLookupColor(wc,s1,&rgb)){
		NhlGenArray	tgen;
		ng_size_t  	tint = 3;
		float		*fptr;

		tgen = _NhlConvertCreateGenArray(NULL,NhlTFloat,sizeof(float),
								1,&tint);
		fptr = NhlConvertMalloc(sizeof(float) * 3);
		if(!tgen || !fptr){
			NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",
									func);
			return NhlFATAL;
		}
		tgen->data = fptr;

		if(rgb.red == 65535)
			fptr[0] = 1.0;
		else
			fptr[0] = (float)rgb.red / 65535.0;
		if(rgb.green == 65535)
			fptr[1] = 1.0;
		else
			fptr[1] = (float)rgb.green / 65535.0;
		if(rgb.blue == 65535)
			fptr[2] = 1.0;
		else
			fptr[2] = (float)rgb.blue / 65535.0;

		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
	}

	return _NhlReConvertData(from->typeQ,
			NrmStringToQuark(NhlTFloatGenArray),from,to);
}

/*
 * Function:	CvtStringGenArrayToColorDefinitionGenArray
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
CvtStringGenArrayToColorDefinitionGenArray
#if	NhlNeedProto
(
	NrmValue		*from,	/* ptr to from data	*/
	NrmValue		*to,	/* ptr to to data	*/
	NhlConvertArgList	args,	/* add'n args for conv	*/
	int			nargs	/* number of args	*/
)
#else
(from,to,args,nargs)
	NrmValue		*from;	/* ptr to from data	*/
	NrmValue		*to;	/* ptr to to data	*/
	NhlConvertArgList	args;	/* add'n args for conv	*/
	int			nargs;	/* number of args	*/
#endif
{
	char	func[] = "CvtStringGenArrayToColorDefinitionGenArray";
	NhlGenArray	fgen = from->data.ptrval;
	NhlString	*sptr;

	if(nargs != 0){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid args?",func);
		return NhlFATAL;
	}

	sptr = fgen->data;

	if((fgen->num_elements == 1) && (sptr[0] != NULL)){
		NrmValue	val;

		val.data.strval = sptr[0];
		val.size = strlen(sptr[0]);

		return _NhlReConvertData(NrmStringToQuark(NhlTString),to->typeQ,
								&val,to);
	}

	return _NhlReConvertData(from->typeQ,
			NrmStringToQuark(NhlTFloatGenArray),from,to);
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
	NhlErrorTypes	ret = NhlNOERROR;
	char		rgbfile[_NhlMAXFNAMELEN];
	Const char	*dbdir;
	Gop_st status;
	int status1,dummy = 6;
	int i;
	_NhlEnumVals	dashvals[] = {
                {NhlUNSPECIFIEDLINE, "Unspecified"},
                {NhlUNSPECIFIEDLINE, "UnspecifiedLine"},
		{NhlSOLIDLINE,	"SolidLine"},
		{NhlSOLIDLINE,	"Solid"},
                {1,		"Dash"},
		{1,             "D4U2"},
                {2,		"Dot"},
		{2,             "DU2"},
                {3,		"DashDot"},
		{3,             "D4U2DU2"},
                {4,		"DashDotDot"},
		{4,             "D4U2DUDU2"},
                {5,		"D2U"},
                {6,		"D3U"},
                {7,		"DUD2U"},
                {8,		"DUD3U"},
                {9,		"D2UD4U"},
                {10,		"D4UD2UDUD2U"},
                {11,		"D2U2"},
                {12,		"D6U2"},
                {13,		"D3UD3U2"},
                {14,		"D2U3"},
                {15,		"DUDU3"},
                {16,		"D5U5"},
	};
	NhlConvertArg	dashargsfullenum[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)}
	};
	NhlConvertArg	dashargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)0)}
	};
        
	_NhlEnumVals	colorvals[] = {
                {NhlUNSPECIFIEDCOLOR,   "Unspecified"},
                {NhlUNSPECIFIEDCOLOR,   "UnspecifiedColor"},
		{NhlTRANSPARENT,	"Transparent"},
		{NhlNULLCOLOR,		"NullColor"},
		{NhlBACKGROUND,		"Background"},
		{NhlFOREGROUND,		"Foreground"}
	};
	NhlConvertArg	namedcolorargs[NhlNumber(colorvals)+1];
	NhlConvertArg	colorargsfullenum[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-2)}
	};
	NhlConvertArg	colorargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)}
	};
        
	_NhlEnumVals	fillvals[] = {
                {NhlUNSPECIFIEDFILL,	"Unspecified"},
                {NhlUNSPECIFIEDFILL,	"UnspecifiedFill"},
		{NhlHOLLOWFILL,	"HollowFill"},
		{NhlHOLLOWFILL,	"Hollow"},
		{NhlNULLFILL,	"NullFill"},
		{NhlSOLIDFILL,	"SolidFill"},
		{NhlSOLIDFILL,	"Solid"},
		{1,		"HatchHorizontal"},
		{2,		"HatchVertical"},
		{3,		"Hatch_45"},
		{4,		"Hatch_135"},
		{5,		"CrossHatch"},
		{6,		"CrossHatch_45"},
		{7,		"Hatch_22"},
		{8,		"Hatch_68"},
		{9,		"Hatch_112"},
		{10,		"Hatch_158"},
		{11,		"CrossHatch_22"},
		{12,		"CrossHatch_68"},
		{13,		"CrossHatch_0_60"},
		{14,		"CrossHatch_0_60_120"},
		{15,		"CrossHatch_0_45_90"},
		{16,		"CrossHatch_0_45_90_135"},
		{17,		"Stippled"}
	};
	NhlConvertArg	fillargsfullenum[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMINMAX)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-2)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)17)}
	};
	NhlConvertArg	fillargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMINMAX)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)17)}
	};
	_NhlEnumVals	markervals[] = {
		{0,	"default"},
		{1,	"dot"},
		{2,	"plus"},
		{2,	"+"},
		{3,	"asterisk"},
		{3,	"*"},
		{4,	"hollow_circle"},
		{5,	"cross"},
		{5,	"x"},
		{6,	"hollow_square"},
		{7,	"up_triangle"},
		{8,	"down_triangle"},
		{9,	"diamond"},
		{9,	"neil"},
		{10,	"left_triangle_filled"},
		{11,	"right_triangle_filled"},
		{12,	"star_5point"},
		{13,	"star_6point"},
		{14,	"circle_w_dot"},
		{15,	"circle_w_cross"},
		{16,	"circle_filled"}
	};
	NhlConvertArg	markerargs[] = {
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)_NhlRngMIN)},
		{NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)-1)},
	};

	_NhlEnumVals	mrkline[] = {
		{NhlLINES,	"Lines"},
		{NhlMARKERS,	"Markers"},
		{NhlMARKLINES,	"MarkLines"}
	};

	_NhlEnumVals	fontqlist[] = {
		{NhlHIGH,	"High"},
		{NhlMEDIUM,	"Medium"},
		{NhlLOW,	"Low"},
		{NhlWORKSTATION, "Workstation"}
	};

	_NhlEnumVals	textdirlist[] = {
		{NhlDOWN,	"Down"},
		{NhlACROSS,	"Across"}
	};
        
        _NhlEnumVals   antialiaslist[] = {
                {NhlANTIALIAS_OFF,      "Off"},
                {NhlANTIALIAS_ON,       "On"},
                {NhlANTIALIAS_TEXTONLY, "TextOnly"}
        };


	_NhlRegisterEnumType(NhlbaseClass,NhlTFontQuality,fontqlist,
			     NhlNumber(fontqlist));
	_NhlRegisterEnumType(NhlbaseClass,NhlTTextDirection,textdirlist,
			     NhlNumber(textdirlist));
	_NhlRegisterEnumType(NhlbaseClass,NhlTAntiAlias,antialiaslist,
			     NhlNumber(antialiaslist));

	(void)_NhlRegisterEnumType(NhlobjClass,NhlTDashIndexFullEnum,
                                   dashvals,NhlNumber(dashvals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTColorIndexFullEnum,
                                   colorvals,NhlNumber(colorvals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTFillIndexFullEnum,
                                   fillvals,NhlNumber(fillvals));

	(void)_NhlRegisterEnumSubtype(NhlobjClass,NhlTDashIndex,
                                      NhlTDashIndexFullEnum,&dashvals[2],
                                      NhlNumber(dashvals)-2);
	(void)_NhlRegisterEnumSubtype(NhlobjClass,NhlTColorIndex,
                                   NhlTColorIndexFullEnum,&colorvals[2],
                                   NhlNumber(colorvals)-2);
	(void)_NhlRegisterEnumSubtype(NhlobjClass,NhlTFillIndex,
                                   NhlTFillIndexFullEnum,&fillvals[2],
                                   NhlNumber(fillvals)-2);
        
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTMarkerIndex,markervals,
		NhlNumber(markervals));
	(void)_NhlRegisterEnumType(NhlobjClass,NhlTMarkLineMode,mrkline,
		NhlNumber(mrkline));

	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTMarkerIndex,_NhlCvtScalarToIndex,
                 markerargs,NhlNumber(markerargs),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTMarkerIndexGenArray,
                 _NhlCvtGenArrayToIndexGenArray,markerargs,
                 NhlNumber(markerargs),False,NULL);

	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTDashIndexFullEnum,
		_NhlCvtScalarToIndex,
                 dashargsfullenum,NhlNumber(dashargsfullenum),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTFillIndexFullEnum,
		_NhlCvtScalarToIndex,
                 fillargsfullenum,NhlNumber(fillargsfullenum),False,NULL);

	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTDashIndex,_NhlCvtScalarToIndex,
                 dashargs,NhlNumber(dashargs),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTFillIndex,_NhlCvtScalarToIndex,
                 fillargs,NhlNumber(fillargs),False,NULL);
        
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTDashIndexFullEnumGenArray,
                 _NhlCvtGenArrayToIndexGenArray,dashargsfullenum,
                 NhlNumber(dashargsfullenum),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTFillIndexFullEnumGenArray,
                 _NhlCvtGenArrayToIndexGenArray,fillargsfullenum,
                 NhlNumber(fillargsfullenum),False,NULL);
        
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTDashIndexGenArray,
                 _NhlCvtGenArrayToIndexGenArray,dashargs,
                 NhlNumber(dashargs),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTFillIndexGenArray,
                 _NhlCvtGenArrayToIndexGenArray,fillargs,
                 NhlNumber(fillargs),False,NULL);

	/*
	 * LOTS of color stuff....
	 */
	(void)_NhlRegisterTypes(NhlTFloatGenArray,
					NhlTColorDefinitionGenArray,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTColorIndexFullEnum,
		_NhlCvtScalarToIndex,
                 colorargsfullenum,NhlNumber(colorargsfullenum),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTScalar,NhlTColorIndex,_NhlCvtScalarToIndex,
                 colorargs,NhlNumber(colorargs),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTColorIndexFullEnumGenArray,
                 _NhlCvtGenArrayToIndexGenArray,colorargsfullenum,
                 NhlNumber(colorargsfullenum),False,NULL);
	(void)NhlRegisterConverter
                (NhlobjClass,NhlTGenArray,NhlTColorIndexGenArray,
                 _NhlCvtGenArrayToIndexGenArray,colorargs,
                 NhlNumber(colorargs),False,NULL);

	namedcolorargs[0].addressmode = NhlLAYEROFFSET;
	namedcolorargs[0].size = sizeof(NhlPointer);
	namedcolorargs[0].data.ptrval = (NhlPointer)
					NhlOffset(NhlLayerRec,base.self);
	for(i=1;i<NhlNumber(namedcolorargs);i++){
		namedcolorargs[i].addressmode = NhlSTRENUM;
		namedcolorargs[i].size = colorvals[i-1].value;
		namedcolorargs[i].data.strval = colorvals[i-1].name;
	}

	(void)NhlRegisterConverter
		(NhlobjClass,NhlTString,NhlTColorIndexFullEnum,
		CvtStringToColorIndex,namedcolorargs,NhlNumber(namedcolorargs),
		False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTStringGenArray,NhlTColorIndexFullEnumGenArray,
		CvtStringGenArrayToColorIndexGenArray,
		namedcolorargs,NhlNumber(namedcolorargs),
		False,NULL);

	for(i=1;i<NhlNumber(namedcolorargs)-2;i++){
		namedcolorargs[i].addressmode = NhlSTRENUM;
		namedcolorargs[i].size = colorvals[i+1].value;
		namedcolorargs[i].data.strval = colorvals[i+1].name;
	}
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTString,NhlTColorIndex,
		CvtStringToColorIndex,namedcolorargs,
		NhlNumber(namedcolorargs)-2,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTStringGenArray,NhlTColorIndexGenArray,
		CvtStringGenArrayToColorIndexGenArray,namedcolorargs,
		NhlNumber(namedcolorargs)-2,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTString,NhlTColorIndexFullEnumGenArray,
		CvtStringToColorIndexGenArray,namedcolorargs,1,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTString,NhlTColorIndexGenArray,
		CvtStringToColorIndexGenArray,namedcolorargs,1,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTColorDefinitionGenArray,
		NhlTColorIndexFullEnumGenArray,
		CvtColorDefinitionGenArrayToColorIndexGenArray,
		namedcolorargs,1,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTColorDefinitionGenArray,NhlTColorIndexGenArray,
		CvtColorDefinitionGenArrayToColorIndexGenArray,
		namedcolorargs,1,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTColorDefinitionGenArray,NhlTColorIndexFullEnum,
		CvtColorDefinitionGenArrayToColorIndex,
		namedcolorargs,1,False,NULL);
	(void)NhlRegisterConverter
		(NhlobjClass,NhlTColorDefinitionGenArray,NhlTColorIndex,
		CvtColorDefinitionGenArrayToColorIndex,
		namedcolorargs,1,False,NULL);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTFloatGenArray,NhlTColorIndexFullEnumGenArray,
		NhlTColorDefinitionGenArray,NhlTColorIndexFullEnumGenArray);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTDoubleGenArray,NhlTColorIndexFullEnumGenArray,
		NhlTColorDefinitionGenArray,NhlTColorIndexFullEnumGenArray);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTFloatGenArray,NhlTColorIndexGenArray,
		NhlTColorDefinitionGenArray,NhlTColorIndexGenArray);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTDoubleGenArray,NhlTColorIndexGenArray,
		NhlTColorDefinitionGenArray,NhlTColorIndexGenArray);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTFloatGenArray,NhlTColorIndexFullEnum,
		NhlTColorDefinitionGenArray,NhlTColorIndexFullEnum);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTDoubleGenArray,NhlTColorIndexFullEnum,
		NhlTColorDefinitionGenArray,NhlTColorIndexFullEnum);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTFloatGenArray,NhlTColorIndex,
		NhlTColorDefinitionGenArray,NhlTColorIndex);
	(void)_NhlRegSymConv(NhlobjClass,
		NhlTDoubleGenArray,NhlTColorIndex,
		NhlTColorDefinitionGenArray,NhlTColorIndex);
        (void)_NhlRegSymConv(NhlobjClass,
		NhlTIntegerGenArray,NhlTColorIndex,
		NhlTColorDefinitionGenArray,NhlTColorIndex);


	/*
	 * These converters are used to allow "named" colors to set
	 * foreground/background resources.
	 */
	namedcolorargs[0].addressmode = NhlLAYEROFFSET;
	namedcolorargs[0].size = sizeof(NhlPointer);
	namedcolorargs[0].data.ptrval = (NhlPointer)
					NhlOffset(NhlLayerRec,base.layer_class);
	(void)NhlRegisterConverter
		(NhlworkstationClass,NhlTString,NhlTColorDefinitionGenArray,
		CvtStringToColorDefinitionGenArray,
		namedcolorargs,1,False,NULL);
	(void)_NhlRegSymConv(NhlworkstationClass,
		NhlTScalar,NhlTColorDefinitionGenArray,
		NhlTScalar,NhlTFloatGenArray);
	(void)NhlRegisterConverter
		(NhlworkstationClass,NhlTStringGenArray,
		NhlTColorDefinitionGenArray,
		CvtStringGenArrayToColorDefinitionGenArray,
		NULL,0,False,NULL);
	(void)_NhlRegSymConv(NhlworkstationClass,
		NhlTFloatGenArray,NhlTColorDefinitionGenArray,
		NhlTFloatGenArray,NhlTFloatGenArray);
	(void)_NhlRegSymConv(NhlworkstationClass,
		NhlTDoubleGenArray,NhlTColorDefinitionGenArray,
		NhlTDoubleGenArray,NhlTFloatGenArray);
	(void)_NhlRegSymConv(NhlworkstationClass,
		NhlTGenArray,NhlTColorDefinitionGenArray,
		NhlTGenArray,NhlTFloatGenArray);
	(void)_NhlRegSymConv(NhlworkstationClass,
		NhlTQuarkGenArray,NhlTColorDefinitionGenArray,
		NhlTQuarkGenArray,NhlTGenArray);

	intQ = NrmStringToQuark(NhlTInteger);
	scalarQ = NrmStringToQuark(NhlTScalar);
	intgenQ = NrmStringToQuark(NhlTIntegerGenArray);
	strgenQ = NrmStringToQuark(NhlTStringGenArray);
	colormap_name = NrmStringToQuark(NhlNwkColorMap);
	bkgnd_name = NrmStringToQuark(NhlNwkBackgroundColor);
	foregnd_name = NrmStringToQuark(NhlNwkForegroundColor);
	marker_tbl_strings_name = NrmStringToQuark(_NhlNwkMarkerTableStrings);
	marker_tbl_params_name = NrmStringToQuark(_NhlNwkMarkerTableParams);
	dash_table_name = NrmStringToQuark(_NhlNwkDashTable);
	def_graphic_style_id_name = NrmStringToQuark(NhlNwkDefGraphicStyleId);
 	views_name = NrmStringToQuark(NhlNwkViews);
 	top_level_views_name = NrmStringToQuark(NhlNwkTopLevelViews);

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

	Marker_Table_Len = sizeof(Marker_Specs)/sizeof(NhlMarkerSpec);
	Fill_Table_Len = sizeof(Fill_Specs)/sizeof(NhlFillSpec); 
	Dash_Table_Len = sizeof(Dash_Specs)/sizeof(NhlDashSpec);
	
/*
 * Allocate the marker table, the fill table and the dash pattern table.
 * The static "Spec" tables retain the default values, which can be 
 * restored because only the Tables are visible to the Workstation instances.
 */

	Fill_Table = (NhlFillTable) NhlMalloc(Fill_Table_Len * 
						  sizeof(NhlFillSpec *));
	if (Fill_Table == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "WorkstationClassInitialize: NhlMalloc failed");
		return NhlFATAL;
	}
	for (i = 0; i < Fill_Table_Len; i++) {
		Fill_Table[i] = &Fill_Specs[i];
	}



	dbdir = _NGGetNCARGEnv("database");
	if(dbdir){
		strcpy(rgbfile,dbdir);
		strcat(rgbfile,_NhlPATHDELIMITER);
		strcat(rgbfile,"rgb");
		NhlworkstationClassRec.work_class.rgb_dbm =
							NGdbm_open(rgbfile,0,0);
	}
	if(!NhlworkstationClassRec.work_class.rgb_dbm){
		NhlPError(NhlWARNING,errno,
			"WorkstationClassInitialize:Unable to access rgb color database - named colors unsupported");
}
	ret = NhlVACreate(&NhlworkstationClassRec.work_class.pal,"pal",
					NhlpaletteClass,_NhlGetDefaultApp(),
		_NhlNpalWorkClass,	NhlworkstationClass,
		NULL);
	return ret;
}

static void InitializeGksWksRecs
#if	NhlNeedProto
(
	NhlWorkstationClassPart *wcp
)
#else
(wcp)
	NhlWorkstationClassPart *wcp;
#endif
{
        Gint_list		wks_list;
        Gint			list[MAX_OPEN_WKS],gerror,wks_list_len;
        wkGksWksRec		*gksp = wcp->gks_wks_recs;
        int errind,iconn_id,ws_type;
        int i,j;

/*
 * The Gks workstation records are kept in ascending order by gks_id. This
 * simplifies lookup. The Hlu open list differs from the Gks open list in
 * that temporarily closed ncgm workstations appear in the HLU list. The
 * reason a separate list is required is to prevent the ids of temporarily
 * closed ncgms to be reassigned, and also to provide the information
 * required to notify the NcgmWorkstation workstation if it needs to reopen
 * a workstation because a user is doing a lowlevel activate on it. This
 * routine zeros out the records initially and then adds (in order) any
 * gks workstations that may already open.
 */
        for (i = 0; i < MAX_OPEN_WKS; i++) {
                gksp[i].hlu_id = NhlNULLOBJID;
                gksp[i].gks_id = 0;
        }
        wks_list.ints = list;
        wks_list.num_ints = MAX_OPEN_WKS;
        ginq_set_open_wss(MAX_OPEN_WKS,0,&gerror,&wks_list,&wks_list_len);
        
            /* sort list */
        
        for (i = 0; i< wks_list_len; i++) {
                int tmp,minix = i;
                for (j = i+1; j < wks_list_len; j++)
                        if (wks_list.ints[j] < wks_list.ints[minix])
                                minix = j;
                if (minix != i) {
                        tmp = wks_list.ints[i];
                        wks_list.ints[i] = wks_list.ints[minix];
                        wks_list.ints[minix] = tmp;
                }
        }
            /* add info to gks wks recs */
        
        for (i = 0; i < wks_list_len; i++) {
                gksp[i].gks_id = wks_list.ints[i];
                NGCALLF(gqwkc,GQWKC)
                        (&gksp[i].gks_id,&errind,&iconn_id,&ws_type);
                if (gksp[i].gks_type == 1) {
                        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
 "Non-HLU GKS ncgm workstation cannot be used until NhlClose is called"));
                }
                gksp[i].gks_type = ws_type;
        }
        *wcp->current_wks_count = wks_list_len;

        return;
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

        if(lc->work_class.gks_wks_recs == NhlInheritGksWksRecs)
		lc->work_class.gks_wks_recs =
                        sc->work_class.gks_wks_recs;
        
        if(lc->work_class.hlu_wks_flag == NhlInheritHluWksFlag)
		lc->work_class.hlu_wks_flag =
                        sc->work_class.hlu_wks_flag;

	if(lc->work_class.pal == NhlInheritPalette)
		lc->work_class.pal = sc->work_class.pal;

	if(lc->work_class.open_work == NhlInheritOpen)
		lc->work_class.open_work = sc->work_class.open_work;

	if(lc->work_class.close_work == NhlInheritClose)
		lc->work_class.close_work = sc->work_class.close_work;

	if(lc->work_class.activate_work == NhlInheritActivate)
		lc->work_class.activate_work = sc->work_class.activate_work;

	if(lc->work_class.deactivate_work == NhlInheritDeactivate)
		lc->work_class.deactivate_work =
                        sc->work_class.deactivate_work;

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

        if (lc->work_class.gks_wks_recs[0].gks_id == -1) 
                InitializeGksWksRecs(&lc->work_class);

	if(lc->work_class.rgb_dbm == (NGDBM*)NULL)
		lc->work_class.rgb_dbm = sc->work_class.rgb_dbm;

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
	NhlWorkstationLayer	nwl,
	NhlWorkstationLayer	owl,
	NhlString		entry_name
)
#else
(nwl,owl,entry_name)
	NhlWorkstationLayer	nwl;
	NhlWorkstationLayer	owl;
	NhlString		entry_name;
#endif
{
	NhlWorkstationClass	wc=(NhlWorkstationClass)nwl->base.layer_class;
	NhlWorkstationClassPart	*wcp = &wc->work_class;
	NhlWorkstationLayerPart	*owp, *nwp = &nwl->work;
	int			i;
	NhlColor		tc;
	NhlColor		*tcp = NULL;
	NhlPrivateColor		*pcmap = nwp->private_color_map;
	NhlString		e_text;
	NhlErrorTypes 		ret = NhlNOERROR;
	NhlBoolean		fg_changed = False, bg_changed = False;

	if (owl)
		owp = &owl->work;
	/*
	 * If cmap is set, use it.
	 */
	if(nwp->color_map) {
		tcp = nwp->color_map->data;
		nwp->color_map_len = nwp->color_map->len_dimensions[0];
		if(nwp->color_map_len > _NhlMAX_COLOR_MAP) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"DoCmap: Maximum color map length exceeded. Limit is (%d). Requested length is (%d), using only first (%d) elements.",_NhlMAX_COLOR_MAP,nwp->color_map_len,_NhlMAX_COLOR_MAP);
			nwp->color_map_len = _NhlMAX_COLOR_MAP;
			ret = NhlWARNING;
		}
		
		nwp->cmap_changed = False;
		if (tcp[0][0] >= 0.0 &&
		    ! (tcp[0][0] == pcmap[0].red &&
		       tcp[0][1] == pcmap[0].green &&
		       tcp[0][2] == pcmap[0].blue)) {
			bg_changed = True; 
			nwp->cmap_changed = True;
		}
				
		if (tcp[1][0] >= 0.0 &&
		    ! (tcp[1][0] == pcmap[1].red &&
		       tcp[1][1] == pcmap[1].green &&
		       tcp[1][2] == pcmap[1].blue)) {
			fg_changed = True; 
			nwp->cmap_changed = True;
		}

		if (owl && owp->color_map_len == nwp->color_map_len) {
			NhlPrivateColor	*old_pcmap = owp->private_color_map;
			for(i=0;i < nwp->color_map_len;i++){
				if (! memcmp(tcp[i],&old_pcmap[i].red,
					     3 * sizeof(float))) {
					pcmap[i].cstat = old_pcmap[i].cstat;
					pcmap[i].ci = old_pcmap[i].ci;
					continue;
				}
				nwp->cmap_changed = True;
				if (tcp[i][0] < 0.0) {
					continue;
				}
				pcmap[i].red = tcp[i][0];
				pcmap[i].green = tcp[i][1];
				pcmap[i].blue = tcp[i][2];
				if(pcmap[i].cstat == _NhlCOLUNSET)
					pcmap[i].cstat = _NhlCOLNEW;
				else
					pcmap[i].cstat = _NhlCOLCHANGE;
			}
		}
		else {
			nwp->cmap_changed = True;
			for(i=0;i < nwp->color_map_len;i++){
				if (tcp[i][0] < 0.0) {
					continue;
				}
				pcmap[i].red = tcp[i][0];
				pcmap[i].green = tcp[i][1];
				pcmap[i].blue = tcp[i][2];
				if(pcmap[i].cstat == _NhlCOLUNSET)
					pcmap[i].cstat = _NhlCOLNEW;
				else
					pcmap[i].cstat = _NhlCOLCHANGE;
			}
			for(i=nwp->color_map_len; i < _NhlMAX_COLOR_MAP;i++){
				if(pcmap[i].cstat == _NhlCOLSET)
					pcmap[i].cstat = _NhlCOLREMOVE;
			}
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
	if(nwp->bkgnd_color){
		if (nwp->bkgnd_color->num_elements != 3) {
			e_text = 
	      "%s: ignoring %s; 3 color elements required (red, green, blue)";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
			tcp = NULL;
		}
		else {
			tcp = nwp->bkgnd_color->data;
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
		/* copy the pointer to the old work so the setvalues cb will
		   work (new and old must be different) */
		if (owl) 
			owp->bkgnd_color = nwp->bkgnd_color;
		nwp->bkgnd_color = NULL;
	}
	else if (owl && bg_changed)
		owp->bkgnd_color = (NhlGenArray) -1;

	if((pcmap[NhlBACKGROUND].cstat == _NhlCOLUNSET) && !tcp){
		tcp = &wcp->def_background;
	}
	if(tcp){
		nwp->cmap_changed = True;
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
	if(nwp->foregnd_color){
		if (nwp->foregnd_color->num_elements != 3) {
			e_text = 
	      "%s: ignoring %s; 3 color elements required (red, green, blue)";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name,
			  NhlNwkBackgroundColor);
			tcp = NULL;
		}
		else {
			tcp = nwp->foregnd_color->data;
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
		/* copy the pointer to the old work so the setvalues cb will
		   work (new and old must be different) */
		if (owl) 
			owp->foregnd_color = nwp->foregnd_color;
		nwp->foregnd_color = NULL;
	}
	else if (owl && fg_changed)
		owp->foregnd_color = (NhlGenArray) -1;
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
		nwp->cmap_changed = True;
		pcmap[NhlFOREGROUND].cstat =
			(pcmap[NhlFOREGROUND].cstat == _NhlCOLUNSET)?
			_NhlCOLNEW:_NhlCOLCHANGE;
		pcmap[NhlFOREGROUND].red = (*tcp)[0];
		pcmap[NhlFOREGROUND].green = (*tcp)[1];
		pcmap[NhlFOREGROUND].blue = (*tcp)[2];
	}
	if (nwp->cmap_changed) {
 		/* copy the pointer to the old work so the setvalues cb will
		   work (new and old must be different) */
		if (owl)
			owp->color_map = nwp->color_map; 
	}

	nwp->color_map = NULL;
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
	NhlErrorTypes		retcode=NhlNOERROR;
	char			*entry_name = "WorkstationInitialize";
        int 			num_allowed = MAX_OPEN_WKS;

            /* if the segment workstation is not yet open,
               one less workstation allowed */
        
        for (i = 0; i < *wcp->current_wks_count; i++) {
                if (wcp->gks_wks_recs[i].gks_type == 3)
                        break;
        }
        if (i == *wcp->current_wks_count) num_allowed--;
	if(*wcp->current_wks_count >= num_allowed){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
              "%s: Limit reached for number of simultaneous GKS Workstations",
			entry_name);
		return NhlFATAL;
	}
        
	wp->gkswksid = (int)NhlFATAL;
	wp->open = False;
        wp->cleared = True;
	wp->gkswkstype = (int)NhlFATAL;
	wp->gkswksconid = (int)NhlFATAL;
	wp->def_plot_id = NhlNULLOBJID;
	wp->def_graphic_style_id = NhlNULLOBJID;
	wp->def_gs_destroy_cb = NULL;
        wp->curr_antialias_state = True;

	/*
	 * Initialize colormap with _NhlCOLUNSET, then call DoCmap to fill cmap
	 * with appropriate values. (To avoid purify errors set the whole
	 * struct to 0, _NhlCOLUNSET is the 0 enum value.)
	 */
	memset(wp->private_color_map,(char)0,
	       sizeof(NhlPrivateColor)* _NhlMAX_COLOR_MAP);
#if 0
	for(i=0;i < _NhlMAX_COLOR_MAP;i++)
		wp->private_color_map[i].cstat = _NhlCOLUNSET;
#endif
	wp->cmap_changed = True;
	retcode = DoCmap(newl,NULL,entry_name);

	newl->work.fill_table_len = Fill_Table_Len - 1;
/*
 * Since the marker specs are stored privately it is only necessary to
 * create a template GenArray initially. The data is not copied. 
 * If either of the
 * marker table resources (marker strings or marker params) is set, 
 * the default marker table is modifed to fit the largest of the 
 * two resources. If one resource is smaller than the other, or NULL, then
 * default values are supplied for each missing item.
 */
/*
  The following seems totally useless; I am removing it

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
 */
	newl->work.marker_table = (NhlMarkerTable) 
		NhlMalloc(Marker_Table_Len * sizeof(NhlMarkerSpec *));
	if (newl->work.marker_table == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	for (i = 0; i < Marker_Table_Len; i++) {
		newl->work.marker_table[i] = &Marker_Specs[i];
	}
	newl->work.marker_table_alloc_len = Marker_Table_Len;
	newl->work.marker_table_len = Marker_Table_Len - 1;

	newl->work.dash_table = (NhlDashTable) NhlMalloc(Dash_Table_Len * 
						  sizeof(NhlDashSpec *));
	if (newl->work.dash_table == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	for (i = 0; i < Dash_Table_Len; i++) {
		newl->work.dash_table[i] = &Dash_Specs[i];
	}
	newl->work.dash_table_alloc_len = Dash_Table_Len;
	newl->work.dash_table_len = Dash_Table_Len - 1;


/*
 * Initialize the "default" graphics primatives - used privately only.
 */
	newl->work.default_lineinfo = newl->work.private_lineinfo;
	newl->work.default_markinfo = newl->work.private_markinfo;
	newl->work.default_fillinfo = newl->work.private_fillinfo;
	newl->work.lip = &newl->work.private_lineinfo;
	newl->work.mip = &newl->work.private_markinfo;
	newl->work.fip = &newl->work.private_fillinfo;

	return(retcode);
}

/*
 * This function is called if the user destroys the default GraphicsStyle obj.
 */
static void
GSDestroyCB
(
        NhlArgVal       cbdata,
        NhlArgVal       udata
)
{
	NhlWorkstationLayer  wl = (NhlWorkstationLayer) udata.ptrval;

	wl->work.def_graphic_style_id = NhlNULLOBJID;
	wl->work.def_gs_destroy_cb = NULL;

	return;
}
/*
 * Function:	CreateDefGraphicsStyle
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
CreateDefGraphicsStyle
#if  NhlNeedProto
(
	NhlWorkstationLayer  wl,
	NhlString	     entry_name
)
#else
(wl,entry_name)
	NhlWorkstationLayer  wl;
	NhlString	     entry_name;
#endif
{
	NhlErrorTypes		retcode = NhlNOERROR,subret = NhlNOERROR;
	NhlWorkstationLayerPart *wlp = &wl->work;
	char 			*e_text;
	int			gsid;
	char			buffer[_NhlMAXRESNAMLEN];
	NhlSArg			sargs[128];
	int			nargs = 0;
	_NhlLineStyleInfo 	*lsp;
	_NhlMarkerStyleInfo 	*msp;
        NhlArgVal           	sel,udata;

	sprintf(buffer,"%s",wl->base.name);
	strcat(buffer, ".GraphicStyle");

	subret = NhlVACreate(&gsid,buffer,NhlgraphicStyleClass,
			     wl->base.id,NULL);
	if ((retcode = MIN(retcode,subret)) < NhlWARNING) 
		return retcode;
	wlp->def_graphic_style_id = gsid;

        NhlINITVAR(sel);
        NhlINITVAR(udata);
        udata.ptrval = wl;
	wlp->def_gs_destroy_cb = 
		_NhlAddObjCallback(_NhlGetLayer(wlp->def_graphic_style_id),
				   _NhlCBobjDestroy,sel,GSDestroyCB,udata);
/*
 * The "wk" line and marker attributes are now obsolete but for now
 * if they are set they will affect the default graphic style.
 */
	lsp = &wlp->public_lineinfo;
	msp = &wlp->public_markinfo; 

	if (lsp->dash_pattern != 0) {
		NhlSetSArg(&sargs[nargs++],
			   NhlNgsLineDashPattern,lsp->dash_pattern);
	}
		
	if ((int)(100*lsp->line_dash_seglen) != 15) {
		NhlSetSArg(&sargs[nargs++],
			   NhlNgsLineDashSegLenF,lsp->line_dash_seglen);
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
	if (lsp->line_label_font != 21) {
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
	if (lsp->line_label_func_code != '~') {
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
		subret = NhlALSetValues(wlp->def_graphic_style_id,sargs,nargs);

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
	NhlWorkstationLayer	oldl = (NhlWorkstationLayer) old;
	NhlErrorTypes	retcode = NhlNOERROR,subret = NhlNOERROR;
	char *tmp;
	char *entry_name = "WorkstationSetValues";
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
			if (newl->work.def_graphic_style_id == NhlNULLOBJID) {
				subret = CreateDefGraphicsStyle
					(newl,entry_name);
				retcode = MIN(subret,retcode);
				if (retcode < NhlWARNING)
					return retcode;
			}
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
	subret = DoCmap(newl,oldl,entry_name);
	retcode = MIN(retcode,subret);

	/*
	 * This function actually allocates the colors contained in the
	 * private colormap.
	 */
	subret = _NhlAllocateColors(newl);
	retcode = MIN(retcode,subret);
	
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
 * if they are set they will affect the default graphic style (if it exists).
 */
	if (newl->work.def_graphic_style_id != NhlNULLOBJID) {
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
	ng_size_t  count[2];
	NhlMarkerTableParams *mtp_p;
	char **s_p = NULL;
	char *e_text;
	char *entry_name = "WorkstationGetValues";

	for( i = 0; i< num_args; i++ ) {

		if(args[i].quark == def_graphic_style_id_name) {
			if (wl->work.def_graphic_style_id == NhlNULLOBJID) {
				ret = CreateDefGraphicsStyle(wl,entry_name);
				if (ret < NhlWARNING)
					return ret;
			}
			*((int *)(args[i].value.ptrval)) = 
				wl->work.def_graphic_style_id;
		}
		else if(args[i].quark == colormap_name) {
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
				   wl->work.marker_table[j+1]->marker) + 1)) == NULL) {
				       e_text = "%s: error allocating %s data";
				       NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						 entry_name,
						 _NhlNwkMarkerTableStrings);
				       return NhlFATAL;
			        }
				strcpy(s_p[j], wl->work.marker_table[j+1]->marker);
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
				mtp_p[j][0] = wl->work.marker_table[j+1]->x_off;
				mtp_p[j][1] = wl->work.marker_table[j+1]->y_off;
				mtp_p[j][2] = 
					wl->work.marker_table[j+1]->aspect_adj;
				mtp_p[j][3] = 
					wl->work.marker_table[j+1]->size_adj;
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
			     NhlMalloc((wl->work.dash_table_len+1) *
				       sizeof(NhlString))) == NULL) {
				e_text = "%s: error allocating %s data";
				NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
					  entry_name,_NhlNwkDashTable);
				return NhlFATAL;
			}
			for (j=0; j<=wl->work.dash_table_len; j++) {
				if ((s_p[j] = (char *) NhlMalloc(strlen(
				   wl->work.dash_table[j]->dpat)+1)) == NULL) {
				       e_text = "%s: error allocating %s data";
				       NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
						 entry_name,_NhlNwkDashTable);
				       return NhlFATAL;
			        }
				strcpy(s_p[j], wl->work.dash_table[j]->dpat);
			}
			count[0] = wl->work.dash_table_len+1;
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
		else if (args[i].quark == views_name) {
			_NhlAllChildList ch = l->base.all_children;
			int *views;

			count[0] = 0;
			while (ch) {
				if (NhlIsView(ch->pid))
					count[0]++;
				ch = ch->next;
			}
			if (! count[0]) {
				continue;
			}
			if (! (views = NhlMalloc(count[0] * sizeof(int)))) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			ch = l->base.all_children;
			j = 0;
			while (ch) {
				if (NhlIsView(ch->pid))
					views[j++] = ch->pid;
				ch = ch->next;
			}
			if ((ga = NhlCreateGenArray((NhlPointer)views,
						    NhlTObjId,
						    sizeof(int),
						    1,count)) == NULL) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value.ptrval)) = ga;
		}
		else if (args[i].quark == top_level_views_name) {
			_NhlAllChildList ch = l->base.all_children;
			int *views;

			count[0] = 0;
			while (ch) {
				if (NhlIsView(ch->pid) && 
				    !_NhlIsPlotMember(ch->pid))
					count[0]++;
				ch = ch->next;
			}
			if (! count[0]) {
				continue;
			}
			if (! (views = NhlMalloc(count[0] * sizeof(int)))) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			ch = l->base.all_children;
			j = 0;
			while (ch) {
				if (NhlIsView(ch->pid) && 
				    !_NhlIsPlotMember(ch->pid))
					views[j++] = ch->pid;
				ch = ch->next;
			}
			if ((ga = NhlCreateGenArray((NhlPointer)views,
						    NhlTObjId,
						    sizeof(int),
						    1,count)) == NULL) {
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
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
	NhlErrorTypes	retcode = NhlNOERROR;
	int i;

	if (wp->marker_table_strings)
		NhlFreeGenArray(wp->marker_table_strings);
	if (wp->marker_table_params)
		NhlFreeGenArray(wp->marker_table_params);


	for (i = 0; i <= wp->dash_table_len; i++) {
		if (wp->dash_table[i]->dynamic) {
			NhlFree(wp->dash_table[i]);
		}
	}
	NhlFree(wp->dash_table);

	/*
	 * remember wp->marker_table_len is 1 less than the real number
	 * of markers.
	 */
	for (i = 0; i <= wp->marker_table_len; i++) {
		if (wp->marker_table[i]->dynamic) {
			NhlFree(wp->marker_table[i]);
		}
	}
	NhlFree(wp->marker_table);

	if(wp->private_lineinfo.line_label_string != NULL)
		NhlFree(wp->private_lineinfo.line_label_string);

	if (wp->def_gs_destroy_cb) {
		_NhlCBDelete(wp->def_gs_destroy_cb);
		wp->def_gs_destroy_cb = NULL;
	}
	if (_NhlGetLayer(wp->def_graphic_style_id) != NULL) {
		NhlDestroy(wp->def_graphic_style_id);
	}
	if (_NhlGetLayer(wp->def_plot_id) != NULL) {
		NhlDestroy(wp->def_plot_id);
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

	if(wl->work.gkswkstype == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unknown workstation type");
		return(NhlFATAL);
		
	} 
	if(wl->work.gkswksconid == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unknown workstation connection id");
		return(NhlFATAL);
	}
        
        _NhlUpdateGksWksRecs(l,True,&wl->work.gkswksid);
        Hlu_Wks_Flag = True;
	(void)_NHLCALLF(gopwk,GOPWK)
                (&(wl->work.gkswksid),&(wl->work.gkswksconid),
		&(wl->work.gkswkstype));
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;

/*
 * HACK -- even though the default for the HLU library is that clipping
 * is turned off, initially it must be turned on in order force GKS
 * to write it into the Ncgm workstation. It is turned off at every
 * workstation activate call. I'm putting this into Workstation rather
 * than NcgmWorkstation so that this condition is an invariant for
 * all workstations. View classes that need to clip must set clipping
 * after calling workstation activate.
 */
        gset_clip_ind(GIND_CLIP);

	if(_NhlLLErrCheckPrnt(NhlWARNING,func)){
		return NhlFATAL;
	}

        _NhlSetBackgroundOpacity(l, wl->work.bkgnd_opacity);
        _NhlSetAntialiasingMode(l, NhlNON_TEXT_ANTIALIAS_MODE);
        
	return _NhlAllocateColors(wl);
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
        
        if (_NhlUpdateGksWksRecs(l,False,NULL)) {
                Hlu_Wks_Flag = True;
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
        
        wl->work.cleared = False;
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
        Hlu_Wks_Flag = True;
	gactivate_ws(wl->work.gkswksid);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;

#if DEBUG_NCGM
	fprintf(stderr,"calling gset_clip_ind\n");
#endif
        gset_clip_ind(GIND_NO_CLIP);

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
                Hlu_Wks_Flag = True;
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
	NhlWorkstationLayer	wl,
	NhlPrivateColor		*old,
	NhlPrivateColor		*new
)
#else
(wl,old,new)
	NhlWorkstationLayer	wl;
	NhlPrivateColor		*old;
	NhlPrivateColor		*new;
#endif
{
	char			func[] = "WorkstationAllocateColors";
	NhlWorkstationClassPart	*wcp =
		&((NhlWorkstationClass)wl->base.layer_class)->work_class;
	Gcolr_rep		tcrep;
	int			i;
	NhlPrivateColor		*pcmap = new;
	NhlErrorTypes		ret = NhlNOERROR;

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
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)l;

#if 0
        if (wl->work.cleared)
                return NhlNOERROR;
#endif

#if DEBUG_NCGM
	fprintf(stderr,"calling gclrwk\n");
#endif

        Hlu_Wks_Flag = True;
	gclear_ws(_NhlWorkstationId(l),GFLAG_ALWAYS);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func))
		return NhlWARNING;

        wl->work.cleared = True;
        
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
	Gfloat                  fill_opacity;
	
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
        fill_opacity = _NhlGetFillOpacity(l);
        _NhlSetFillOpacity(l, wkfp->fill_opacity);
        
        switch (wkfp->fill_color) {
            case NhlTRANSPARENT:
                    fill_color = NhlTRANSPARENT;
                    break;
            case NhlUNSPECIFIEDCOLOR:
                    fill_color = _NhlGetGksCi(l,NhlFOREGROUND);
                    break;
            default:
                    fill_color = _NhlGetGksCi(l,wkfp->fill_color);
                    break;
        }
	fill_background = (wkfp->fill_background < 0) ?
		wkfp->fill_background : _NhlGetGksCi(l,wkfp->fill_background);

/*
 * Draw the fill, unless a negative fill index or Transparent fill color
 * is specified (implying no fill)
 */
	if (fill_color == NhlTRANSPARENT)
	/*SUPPRESS570*/
		;
	else if ((ix = wkfp->fill_index) == NhlSOLIDFILL ||
                 ix == NhlUNSPECIFIEDFILL) {
		/* Fill_Specs[ix].type  must be 0 */
		gset_fill_int_style(GSTYLE_SOLID);
		gset_linewidth(wkfp->fill_line_thickness);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_sfseti("type of fill", 0);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,fill_color);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}
	else if (ix > 0) {
		/* Fill_Specs[ix].type must not be 0 */
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
		c_sfseti("TY", Fill_Specs[ix].type);
		c_sfseti("DO", Fill_Specs[ix].dots_on);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		if (Fill_Specs[ix].dots_on) {
			if (wkfp->fill_dot_size > 0.0) {
				c_sfseti("DO", -1);
				c_sfsetr("DS",wkfp->fill_dot_size);
				c_sfseti("DC",fill_color);
			}
			else {
				gset_marker_colr_ind(fill_color);
			}
			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,-1);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		else if (Fill_Specs[ix].type > 0) { 
 			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,fill_color);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
		else {
			gset_line_colr_ind(fill_color);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
 			c_sfsgfa(x,y,num_points,dst,nst,ind,nnd,
				 Fill_Specs[ix].ici);
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
		c_plotif(0.0,0.0,2);
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

       _NhlSetFillOpacity(l, fill_opacity);

       return(NhlNOERROR);

}

NhlErrorTypes
_NhlWorkstationCellFill
#if  NhlNeedProto
(
	NhlLayer	l,
	float		xp1,
	float		yp1,
        float           xp2,
	float           yp2,
	int             nx,
	int             ny,
	int             *clrixs
)
#else
(l,xp1,yp1,xp2,yp2,nx,ny,*clrixs)
	NhlLayer	l;
	float		xp1;
	float		yp1;
        float           xp2;
	float           yp2;
	int             nx;
	int             ny;
        int             *clrixs;
#endif
{
	char			func[] = "WorkstationCellFill";
        float			fl,fr,fb,ft,ul,ur,ub,ut;
	int			ll;
	int                     xs = 1;
	int                     ys = 1;
	

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

	NGCALLF(gca,GCA)(&xp1,&yp1,&xp2,&yp2,&nx,&ny,&xs,&ys,&nx,&ny,clrixs);

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	return(NhlNOERROR);

}

NhlBoolean LLUGksWorkIsOpen
#if     NhlNeedProto
(
	NhlLayer l,
        int     gks_id
)
#else
(l,gks_id)
	NhlLayer l;
        int     gks_id;
#endif
{
        int i,wkid,numopen,errind,tmp = 1;
	NhlWorkstationLayer wl = (NhlWorkstationLayer) l;

	/*
	 * can't use this check for Ncgm workstations because it could
	 * be temporarily closed, and then it doesn't show up.
	 */
	if (wl->work.gkswkstype == 1)
		return True;
/* FORTRAN */ _NHLCALLF(gqopwk,GQOPWK)(&tmp,&errind,&numopen,&wkid);
        if(wkid == gks_id)
                return(True);

        for(i  = 2; i <= numopen; i++ ) {
/* FORTRAN */ _NHLCALLF(gqopwk,GQOPWK)(&i,&errind,&tmp,&wkid);
                if(wkid == gks_id)
                        return(True);
        }
        return(False);
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
	return _NhlAllocateColors(wl);
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
	return _NhlAllocateColors(wl);
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
			ret = _NhlAllocateColors(wl);

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

	/* do nothing if "ci" is an ARGB color */
	if (( ci & ALPHA_MASK) > 0)
		return ci;

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
	if((ret != NhlFATAL)&&(((NhlWorkstationLayer)wks)->work.gkswkstype!=-9999)){
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

        NhlUpdateWorkstation(wks->base.id);
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
	NhlWorkstationLayer	wl
)
#else
(wl)
	NhlWorkstationLayer	wl;
#endif
{
	NhlWorkstationClassPart	*wc =
		&((NhlWorkstationClass)wl->base.layer_class)->work_class;
	NhlPrivateColor			old[_NhlMAX_COLOR_MAP];
	NhlPrivateColor			*new = wl->work.private_color_map;
	NhlErrorTypes			ret = NhlNOERROR;
	int				i,maxi=0;
	NhlArgVal			cbdata,sel;
	_NhlworkColorChangeDataRec	ccdata;

	NhlINITVAR(sel);
	NhlINITVAR(cbdata);
	NhlINITVAR(old);
	cbdata.ptrval = &ccdata;
	ccdata.pid = wl->base.id;

	if(!wl->work.cmap_changed)
		return ret;

	if (!LLUGksWorkIsOpen((NhlLayer)wl,wl->work.gkswksid)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Workstation with PID#%d is not open",wl->base.id);
		return NhlFATAL;
	}

	memcpy(old,new,sizeof(NhlPrivateColor)*_NhlMAX_COLOR_MAP);

	ret = (*(wc->alloc_colors))(wl,old,new);

	for(i=0;i<_NhlMAX_COLOR_MAP;i++){
		if(new[i].cstat == _NhlCOLSET)
			maxi = i;
		else{
			new[i].cstat = _NhlCOLUNSET;
			new[i].red = new[i].green = new[i].blue = -1.0;
		}
	}

	maxi = MAX(maxi,1);
	wl->work.color_map_len = maxi + 1;
	wl->work.cmap_changed = False;

	/*
	 * call callbacks.
	 */
	for(i=0;i<_NhlMAX_COLOR_MAP;i++){
		if(memcmp(&new[i],&old[i],sizeof(NhlPrivateColor)) == 0)
			continue;

		ccdata.ci = sel.lngval = i;
		ccdata.red = new[i].red;
		ccdata.green = new[i].green;
		ccdata.blue = new[i].blue;
		_NhlCallObjCallbacks((NhlLayer)wl,_NhlCBworkColorIndexChange,
								sel,cbdata);
	}
	sel.lngval = 0;
	ccdata.ci = -1;
	ccdata.red = ccdata.green = ccdata.blue = -1.0;
	_NhlCallObjCallbacks((NhlLayer)wl,_NhlCBworkColorMapChange,
			     sel,cbdata);


	return ret;
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
	NhlWorkstationLayer	wl = (NhlWorkstationLayer) l;
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
	if (!LLUGksWorkIsOpen(l,wl->work.gkswksid)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Workstation with PID#%d is not open",workid);
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

	c_plotif(0.0,0.0,2);


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
			strlen(wkp->dash_table[ix]->dpat) + 0.5;
		if(dollar_size < 1) dollar_size = 1;
		
		strcpy(buffer,wkp->dash_table[ix]->dpat);

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
#if 0
	c_dpsetc("CRG","'");
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
#endif
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
        strncpy(buffer,wkp->dash_table[ix]->dpat,buff_size);

	tf = wklp->line_dash_seglen / (strlen(buffer)+.5);
	c_dpsetr("WOG",tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_dpsetr("WOS",tf);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	if(wklp->line_color == NhlTRANSPARENT){
		i=0;
		while(buffer[i] != '\0'){
			buffer[i] = '_';
			i++;
		}
	}
	else{
	        gset_line_colr_ind((Gint)_NhlGetGksCi(
			    plot->base.wkptr,wklp->line_color));
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	}

	_NhlSetLineOpacity(wl, wklp->line_opacity);

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
	int			edge_dash_dollar_size;
	

	if (wkfp->edges_on) {
		c_plotif(0.0,0.0,2); /* flush the buffer before any changes to line attributes */

		if (wkfp->edge_dash_pattern < 0) {
			/* NhlWARNING - but it's a void function right now */
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "_NhlSetFillInfo: invalid edge dash pattern index");
			wkfp->edge_dash_pattern = NhlSOLIDLINE;
		}
		else if (wkfp->edges_on && wkfp->edge_dash_pattern > 0) {
			memset((void *) buffer, (char) 0, 80 * sizeof(char));


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
		
			edge_dash_dollar_size = (x1 - x0) /
				strlen(wk_p->dash_table[ix]->dpat) + 0.5;
			if(edge_dash_dollar_size < 1)
				edge_dash_dollar_size = 1;
		
			strcpy(buffer,wk_p->dash_table[ix]->dpat);

			c_dashdc(buffer,edge_dash_dollar_size,1);
			(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		}
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
	 "_NhlSetFillInfo: using mod function on fill index: %d", ix);

		ix = 1 + (ix - 1) % wk_p->fill_table_len;
	}

	if (ix != NhlHOLLOWFILL) {
		c_sfseti("AN", Fill_Specs[ix].angle);
		(void)_NhlLLErrCheckPrnt(NhlINFO,func);
		c_sfsetr("SP", Fill_Specs[ix].spacing * 
			 wkfp->fill_scale_factor);
		(void)_NhlLLErrCheckPrnt(NhlINFO,func);
	}

        return;
}


NhlErrorTypes
_NhlWorkstationRasterFill
#if  NhlNeedProto
(
	NhlLayer	l,
	float		xp1,
	float		yp1,
        float           xp2,
	float           yp2,
	int             nx,
	int             ny,
	int             *clrixs
)
#else
(l,xp1,yp1,xp2,yp2,nx,ny,*clrixs)
	NhlLayer	l;
	float		xp1;
	float		yp1;
        float           xp2;
	float           yp2;
	int             nx;
	int             ny;
        int             *clrixs;
#endif
{
	char			func[] = "WorkstationRasterFill";
        float			fl,fr,fb,ft,ul,ur,ub,ut;
	int			ll;
	int                     xs = 1;
	int                     ys = 1;
	

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

	NGCALLF(gca,GCA)(&xp1,&yp1,&xp2,&yp2,&nx,&ny,&xs,&ys,&nx,&ny,clrixs);
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;

	c_set(fl,fr,fb,ft,ul,ur,ub,ut,ll);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	return(NhlNOERROR);

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
	int	font,
	float	x_off, 
	float	y_off,
	float	aspect_adj,
	float	size_adj,
	float   angle
)
#else
(wid,mark_string,font,x_off,y_off,aspect_adj,size_adj,angle)
	int	wid;
	char	*mark_string;
	int     font;
	float	x_off;
	float	y_off;
	float	aspect_adj;
	float	size_adj;
	float   angle;
#endif
{
	char func[] = "NhlNewMarker";
        NhlWorkstationLayer tinst = (NhlWorkstationLayer)_NhlGetLayer(wid);
	NhlWorkstationLayerPart *wkp;
	NhlMarkerSpec *m_p;
	int i;

	if((tinst == NULL) || !_NhlIsWorkstation(tinst)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Invalid workstation id = %d",func,wid);
		return ((int)NhlFATAL);
	}
	
	wkp = &tinst->work;

	/*
	 * remember wp->marker_table_len is 1 less than the real number
	 * of markers.
	 */
	if (wkp->marker_table_len == wkp->marker_table_alloc_len - 1) {
		wkp->marker_table_alloc_len += NhlWK_ALLOC_UNIT;
		wkp->marker_table = (NhlMarkerTable) 
			NhlRealloc(wkp->marker_table, 
				   wkp->marker_table_alloc_len *
				   sizeof(NhlMarkerSpec *));
		if (wkp->marker_table == NULL) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return((int)NhlFATAL);
		}
		for (i =wkp->marker_table_len+1; 
		     i < wkp->marker_table_alloc_len; i++) {
                        wkp->marker_table[i] = NULL;
                }

	}
	if (mark_string == NULL || strlen(mark_string) == 0) {
		wkp->marker_table_len++;
		wkp->marker_table[wkp->marker_table_len] = 
			&Marker_Specs[NhlWK_DEF_MARKER];
		return (wkp->marker_table_len); 
        }
	else if (strlen(mark_string) > _NhlMAXMARKERLEN) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: marker string exceeds max length",func);
		return(NhlWARNING);
	}
		
	m_p = (NhlMarkerSpec *) NhlMalloc(sizeof(NhlMarkerSpec));
	if (m_p == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return((int)NhlFATAL);
	}
	wkp->marker_table[wkp->marker_table_len+1] = m_p;

	if ((m_p->marker = NhlMalloc(strlen(mark_string) + 1)) == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return((int)NhlFATAL);
	}
	strcpy(m_p->marker, mark_string);

	m_p->font = font;
	m_p->x_off = x_off;
	m_p->y_off = y_off;

	if (aspect_adj > 0.0)
		m_p->aspect_adj = aspect_adj;
	else
		m_p->aspect_adj = Marker_Specs[0].aspect_adj;

	if (size_adj > 0.0)
		m_p->size_adj = size_adj;
	else
		m_p->size_adj = Marker_Specs[0].size_adj;

	m_p->angle = angle;
	m_p->dynamic = True;

	wkp->marker_table_len++;
	return (wkp->marker_table_len); 
	
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
	int             *font,
	float		*xoff,
	float		*yoff,
	float		*aspadj,
	float		*sizeadj,
	float		*angle,
	int		*indx_ret
	
)
#else
(wid,fmark,fmark_len,font,xoff,yoff,aspadj,sizeadj,angle,indx_ret)
	int		*wid;
	_NhlFString	fmark;
	int		*fmark_len;
	int             *font;
	float		*xoff;
	float		*yoff;
	float		*aspadj;
	float		*sizeadj;
	float           *angle;
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

	*indx_ret = NhlNewMarker(*wid,tstr,*font,
				 *xoff,*yoff,*aspadj,*sizeadj,*angle);

	return;
}



/*
 * Allows modification of the characteristics of an existing marker, 
 * whether pre-defined or added using NhlNewMarker. If NULL, causes default
 * value to be restored. For added markers the default is index 1;
 */
/*ARGSUSED*/
NhlErrorTypes
NhlSetMarker
#if  NhlNeedProto
(int instance, 
 int	index,
 char	*mark_string, 
 int    font,
 float	x_off, 
 float	y_off,
 float	aspect_adj,
 float	size_adj,
 float  angle)
#else
(instance,index,mark_string,font,x_off,y_off,aspect_adj,size_adj,angle)
        int instance;
	int   index;
	char *mark_string; 
	int   font;
	float x_off; 
	float y_off;
	float aspect_adj;
	float size_adj;
	float angle;
#endif
{
	char func[] = "NhlSetMarker";
        NhlWorkstationLayer wl = (NhlWorkstationLayer)_NhlGetLayer(instance);
	NhlWorkstationLayerPart *wkp = &wl->work;
	NhlMarkerSpec *m_p;
	char *c_p;

	if (index <= 0 || index > wkp->marker_table_len) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: invalid marker index",func);
		return(NhlWARNING);
	}

	m_p = wkp->marker_table[index];

/*
 * An empty string is a signal to restore the default value. If the index
 * is in range of the original marker table, the original values are used.
 * If the index indicates that it is an added marker, then the value for 
 * index 1 are used.
 */
	if (mark_string == NULL || strlen(mark_string) == 0) {
		if (m_p->dynamic) {
			NhlFree(m_p->marker);
			NhlFree(m_p);
		}
		if (index < sizeof(Marker_Specs)/sizeof(NhlMarkerSpec)) {
			wkp->marker_table[index] = &Marker_Specs[index];
		}
		else {
			wkp->marker_table[index] = 
				&Marker_Specs[NhlWK_DEF_MARKER];
		}
		return (NhlNOERROR); 
	}
	else if (strlen(mark_string) > _NhlMAXMARKERLEN) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "%s: marker string exceeds max length",func);
		return(NhlWARNING);
	}

/* 
 * If the marker is one of the initial statically defined markers, make
 * a copy and mark it dynamic. The initial set of markers should
 * be considered read-only for the WorkstationClass. Changes to the table
 * are instance-specific.
 */
	if (! m_p->dynamic) {
		if ((m_p = (NhlMarkerSpec *) 
		    NhlMalloc(sizeof(NhlMarkerSpec))) == NULL) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return(NhlFATAL);
		}
		memcpy((char *) m_p, (char *) wkp->marker_table[index],
			sizeof(NhlMarkerSpec));
		wkp->marker_table[index] = m_p;
		/* make a copy is made of the string, or there
		   a chance that subsequently the static string might
		   get freed */
		m_p->marker = NULL;
	}
		
	if (m_p->marker == NULL || 
	    strcmp(mark_string, m_p->marker)) {
		    if ((c_p = NhlMalloc(strlen(mark_string)+ 1 )) == NULL) {
			    NHLPERROR((NhlFATAL,ENOMEM,NULL));
			    return(NhlFATAL);
		    }
		    strcpy(c_p, mark_string);
		    if (m_p->dynamic) 
			    NhlFree(m_p->marker);
		    m_p->marker = c_p;
	}
	m_p->dynamic = True;

	m_p->font = font;
	m_p->x_off = x_off;
	m_p->y_off = y_off;

	if (aspect_adj > 0.0)
		m_p->aspect_adj = aspect_adj;
	else
		m_p->aspect_adj = 1.0;

	if (size_adj > 0.0)
		m_p->size_adj = size_adj;
	else
		m_p->size_adj = 1.0;

	m_p->angle = angle;

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
	int             *font,
	float		*xoff,
	float		*yoff,
	float		*aspadj,
	float		*sizeadj,
	float		*angle,
	int		*err
)
#else
(wid,indx,fmark,fmark_len,font,xoff,yoff,aspadj,sizeadj,angle,err)
	int		*wid;
	int		*indx;
	_NhlFString	fmark;
	int		*fmark_len;
	int             *font;
	float		*xoff;
	float		*yoff;
	float		*aspadj;
	float		*sizeadj;
	float		*angle;
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

	*err = NhlSetMarker(*wid,*indx,tstr,*font,
			    *xoff,*yoff,*aspadj,*sizeadj,*angle);

	return;
}


/*
 * Adds a marker definition to the marker table and returns an index to
 * this marker. 
 */
/*ARGSUSED*/
int NhlNewDashPattern
#if  NhlNeedProto
(
	int	wid, 
	char	*dash_string
)
#else
(wid,dash_string)
	int	wid;
	char	*dash_string;
#endif
{
	char			func[] = "NhlNewDashPattern";
        NhlWorkstationLayer tinst = (NhlWorkstationLayer)_NhlGetLayer(wid);
	NhlWorkstationLayerPart *wkp;
	NhlDashSpec *d_p;
	int i;

	if((tinst == NULL) || !_NhlIsWorkstation(tinst)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Invalid workstation id = %d",func,wid);
		return ((int)NhlFATAL);
	}
	
	wkp = &tinst->work;

	/*
	 * remember wp->dash_table_len is 1 less than the real number
	 * of dashs.
	 */
	if (wkp->dash_table_len == wkp->dash_table_alloc_len - 1) {
		wkp->dash_table_alloc_len += NhlWK_ALLOC_UNIT;
		wkp->dash_table = (NhlDashTable) 
			NhlRealloc(wkp->dash_table, 
				   wkp->dash_table_alloc_len *
				   sizeof(NhlDashSpec *));
		if (wkp->dash_table == NULL) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return((int)NhlFATAL);
		}
		for (i =wkp->dash_table_len+1; 
		     i < wkp->dash_table_alloc_len; i++) {
                        wkp->dash_table[i] = NULL;
                }

	}
	if (dash_string == NULL || strlen(dash_string) == 0) {
		wkp->dash_table_len++;
		wkp->dash_table[wkp->dash_table_len] = &Dash_Specs[0];
		return (wkp->dash_table_len); 
	}
	else {
		if (strlen(dash_string) > _NhlMAXDASHLEN) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "%s: dash pattern exceeds max length",func);
			return(NhlWARNING);
		}
		for (i = 0; i < strlen(dash_string); i++) {
			if (! (*(dash_string + i) == '$' ||
			       *(dash_string + i) == '_')) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			 "%s: invalid dash pattern specifier",func);
				return((int)NhlWARNING);
			}
		}    
	}	
		
	d_p = (NhlDashSpec *) NhlMalloc(sizeof(NhlDashSpec));
	if (d_p == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return((int)NhlFATAL);
	}
	wkp->dash_table[wkp->dash_table_len+1] = d_p;
	if ((d_p->dpat = NhlMalloc(strlen(dash_string) + 1)) == NULL) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return((int)NhlFATAL);
	}
	strcpy(d_p->dpat, dash_string);
	
	d_p->dynamic = True;

	wkp->dash_table_len++;
	return (wkp->dash_table_len); 
	
}

/*
 * Function:	nhlpfnewdashpattern
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
void _NHLCALLF(nhlpfnewdashpattern,NHLPFNEWDASHPATTERN)
#if	NhlNeedProto
(
	int		*wid,
	_NhlFString	fdash,
	int		*fdash_len,
	int		*indx_ret
)
#else
(wid,fmark,fmark_len,xoff,yoff,aspadj,sizeadj,indx_ret)
	int		*wid;
	_NhlFString	fdash;
	int		*fdash_len;
	int		*indx_ret;
#endif
{
	char	tstr[_NhlMAXDASHLEN];

	if(!_NhlFstrToCstr(tstr,NhlNumber(tstr),fdash,*fdash_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Can't convert Fortran string to C string");
		*indx_ret = NhlFATAL;
		return;
	}

	*indx_ret = NhlNewDashPattern(*wid,tstr);

	return;
}


/*
 * Allows modification of the characteristics of an existing dash pattern, 
 */
/*ARGSUSED*/
NhlErrorTypes
NhlSetDashPattern
#if  NhlNeedProto
(int instance, 
 int	index,
 char	*dash_string)
#else
(instance,index,dash_string)
        int instance;
	int   index;
	char *dash_string; 
#endif
{
	NhlWorkstationLayer	wl = (NhlWorkstationLayer)_NhlGetLayer(instance);
	char			func[] = "NhlSetDashPattern";
	NhlWorkstationLayerPart	*wkp = &wl->work;
	NhlDashSpec *d_p;
	char *c_p;
	int i;

	if (index <= 0 || index > wkp->dash_table_len) {
		char *emsg = (index == 0) ?
			"%s: dash index 0 cannot be modified" :
			"%s: invalid dash index";
		NhlPError(NhlWARNING,NhlEUNKNOWN,emsg,func);
		return(NhlWARNING);
	}

	d_p = wkp->dash_table[index];

/*
 * An empty string is a signal to restore the default value. If the index
 * is in range of the original dash table, the original values are used.
 * If the index indicates that it is an added dash, then the value for 
 * index 0 (NhlSOLIDLINE) are used.
 */
	if (dash_string == NULL || strlen(dash_string) == 0) {
		if (d_p->dynamic) {
			NhlFree(d_p->dpat);
			NhlFree(d_p);
		}
		if (index < sizeof(Dash_Specs)/sizeof(NhlDashSpec)) {
			wkp->dash_table[index] = &Dash_Specs[index];
		}
		else {
			wkp->dash_table[index] = &Dash_Specs[0];
		}
		return (NhlNOERROR); 
	}
	else {
		if (strlen(dash_string) > _NhlMAXDASHLEN) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "%s: dash pattern exceeds max length",func);
			return(NhlWARNING);
		}
		for (i = 0; i < strlen(dash_string); i++) {
			if (! (*(dash_string + i) == '$' ||
			       *(dash_string + i) == '_')) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			 "%s: invalid dash pattern specifier",func);
				return(NhlWARNING);
			}
		}    
	}	
		
/* 
 * this routine is coded to allow expansion of the table.
 * If the dash is one of the initial statically defined dashs, make
 * a copy and mark it dynamic. The initial set of dash patterns should
 * be considered read-only for the WorkstationClass. Changes to the table
 * are instance-specific.
 */
	if (! d_p->dynamic) {
		if ((d_p = (NhlDashSpec *) 
		    NhlMalloc(sizeof(NhlDashSpec))) == NULL) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return(NhlFATAL);
		}
		memcpy(d_p, wkp->dash_table[index],
			sizeof(NhlDashSpec));
		wkp->dash_table[index] = d_p;
		d_p->dpat = NULL;
	}
		
	if (d_p->dpat == NULL || strcmp(dash_string, d_p->dpat)) {
		    if ((c_p = NhlMalloc(strlen(dash_string)+ 1 )) == NULL) {
			    NHLPERROR((NhlFATAL,ENOMEM,NULL));
			    return(NhlFATAL);
		    }
		    strcpy(c_p, dash_string);
		    if (d_p->dynamic) 
			    NhlFree(d_p->dpat);
		    d_p->dpat = c_p;
	}
	d_p->dynamic = True;

	return (NhlNOERROR); 
	
}

/*
 * Function:	nhlpfsetdashpattern
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
void _NHLCALLF(nhlpfsetdashpattern,NHLPFSETDASHPATTERN)
#if	NhlNeedProto
(
	int		*wid,
	int		*indx,
	_NhlFString	fdash_string,
	int		*fdash_string_len,
	int		*err
)
#else
(wid,indx,fdash_string,fdash_string_len,err)
	int		*wid;
	int		*indx;
	_NhlFString	fdash_string;
	int		*fdash_string_len;
#endif
{
	char	tstr[_NhlMAXDASHLEN];

	if(!_NhlFstrToCstr(tstr,NhlNumber(tstr),fdash_string,
			   *fdash_string_len)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"Can't convert Fortran string to C string");
		*err = NhlFATAL;
		return;
	}

	*err = NhlSetDashPattern(*wid,*indx,tstr);

	return;
}

static struct{
	float	xoff;
	float	yoff;
	float	size;
	float   angle;
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
	NhlWorkstationLayerPart	*wkp = &tinst->work;
	_NhlMarkerStyleInfo	*mkp = wkp->mip;
	int			index;
	int			marker_color;
	float			p_height, p_width;
	float			aspect;

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
		minfo.size = wkp->marker_table[NhlWK_DEF_MARKER]->size_adj *
					mkp->marker_size;
		minfo.xoff = mkp->marker_size *
			wkp->marker_table[NhlWK_DEF_MARKER]->x_off;
		minfo.yoff = mkp->marker_size *
			wkp->marker_table[NhlWK_DEF_MARKER]->y_off;
		minfo.string = wkp->marker_table[NhlWK_DEF_MARKER]->marker;
		aspect = wkDEF_MARKER_ASPECT * 
			wkp->marker_table[NhlWK_DEF_MARKER]->aspect_adj;
	}
	else if (index > 0) {
		index = 1 + (index - 1) % wkp->marker_table_len;
		aspect = wkDEF_MARKER_ASPECT * 
			wkp->marker_table[index]->aspect_adj;

		minfo.size = wkp->marker_table[index]->size_adj 
			* mkp->marker_size;
		minfo.xoff = mkp->marker_size 
			* wkp->marker_table[index]->x_off;
		minfo.yoff = mkp->marker_size 
			* wkp->marker_table[index]->y_off;
		minfo.angle = wkp->marker_table[index]->angle;
		minfo.string = wkp->marker_table[index]->marker;
	}
	
	if (aspect <= 1.0) {
		p_width = 21.0;
		p_height = 21.0 * aspect;
	} else {
		p_width = 21.0 / aspect;
		p_height = 21.0;
	}
	
	c_pcsetr("PH",p_height);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcsetr("PW",p_width);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
#if 0
	/* 
	 * This puts a boundary around the marker character box:
	 * useful for debugging
	 */
	c_pcseti("BF",1);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("BC",2);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcsetr("BM",0.0);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
#endif

	c_pcseti("FN", wkp->marker_table[index]->font);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcsetr("CL",mkp->marker_thickness);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	marker_color = _NhlGetGksCi(tinst->base.wkptr, mkp->marker_color);
	c_pcseti("OC",marker_color);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
	c_pcseti("CC",marker_color);
	(void)_NhlLLErrCheckPrnt(NhlWARNING,func);

	_NhlSetMarkerOpacity(tinst, mkp->marker_opacity);

#if 0
/*
 * figure the offsets needed to center the markers and add to the
 * user-specified offsets. Not needed when you use the 'CE' parameter.
 */

	{
		float db,dt,dr,dl;
		float x_off, y_off;

		c_pcseti("TE",1);
		c_plchhq(0.5,0.5,minfo.string,minfo.size*1.125,360.0,0.0);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		c_pcgetr("DL",&dl);
		c_pcgetr("DR",&dr);
		c_pcgetr("DT",&dt);
		c_pcgetr("DB",&db);
		(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
		if (fabs(dl) > 10.0) dl = 0.0001;
		if (fabs(dr) > 10.0) dr = 0.0001;
		if (fabs(db) > 10.0) db = 0.0001;
		if (fabs(dt) > 10.0) dt = 0.0001;
		x_off = 0.5 - (1.0 + dr - dl) / 2.0;
		y_off = 0.5 - (1.0 + dt - db) / 2.0;
		x_off = (dl - dr) / 2.0;
		y_off = (db - dt) / 2.0;
		minfo.xoff += x_off;
		minfo.yoff += y_off;
		c_pcseti("TE",1);
	}
#endif
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
        char			func_code[8];
	int                     ce;
        NhlWorkstationLayer	layer = (NhlWorkstationLayer)l;
	NhlWorkstationLayerPart	*wkp = &layer->work;
	_NhlMarkerStyleInfo	*mkp = wkp->mip;
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

        /* Because markers are implemented in terms of line/fill primitives, we save and set the opacity
         * state here accordingly, and then restore before returning to keep from undermining line/fill opacities
         * that may be in effect.
         */
        float lineOpacity = _NhlGetLineOpacity(layer);
	_NhlSetLineOpacity(layer, mkp->marker_opacity);
        float fillOpacity = _NhlGetFillOpacity(layer);
        _NhlSetFillOpacity(layer, mkp->marker_opacity);

        c_pcgetc("FC",func_code,8);
        c_pcsetc("FC",":");
	c_pcgeti("CE",&ce);
	c_pcseti("CE",1);

	for (i=0; i<num_points; i++) {
		/*
		 * marker size is multiplied by 1.125 to account
		 * for 'SA' parameter of PlotChar.
		 */
		c_plchhq(x[i]+minfo.xoff,y[i]+minfo.yoff,minfo.string,
			minfo.size*1.125,minfo.angle,0.0);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func))
			ret = NhlWARNING;
	}
	c_pcseti("CE",ce);
        c_pcsetc("FC",func_code);

        _NhlSetLineOpacity(layer, lineOpacity);
        _NhlSetFillOpacity(layer, fillOpacity);

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
 * Function:	_NhlDefaultPlot
 *
 * Description:	returns the default plot associated with a Workstation.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlLayer
_NhlDefaultPlot
#if	NhlNeedProto
(
	NhlLayer l
)
#else
(l)
	NhlLayer l;
#endif
{
	NhlWorkstationLayer wl;
	NhlErrorTypes subret = NhlNOERROR;

	if(! (l && _NhlIsWorkstation(l)))
		return NULL;

	wl = (NhlWorkstationLayer) l;

	if (wl->work.def_plot_id == NhlNULLOBJID) {
		int plot_id;
		char	buffer[_NhlMAXRESNAMLEN];

		sprintf(buffer,"%s",wl->base.name);
		strcat(buffer,".Plot");
		subret = NhlVACreate(&plot_id,buffer,NhllogLinPlotClass,
				     wl->base.id,
				     NhlNtfPlotManagerOn,False,
				     NhlNvpXF,0.0,
				     NhlNvpYF,1.0,
				     NhlNvpWidthF,1.0,
				     NhlNvpHeightF,1.0,
				     NULL);
		if (subret < NhlWARNING) 
			return NULL;
		wl->work.def_plot_id = plot_id;
	}
	
	return _NhlGetLayer(wl->work.def_plot_id);
}

/*
 * Function: _NhlUpdateDrawBB	
 *
 * Description:	updates the workstation bounding box with most recently
 *              drawn objects 
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */

extern NhlErrorTypes _NhlUpdateDrawBB
#if	NhlNeedProto
(
	NhlLayer	vl
)
#else
(wkid,vid)
	NhlLayer	vl;
#endif

{
	NhlLayer wkl = vl->base.wkptr;
	NhlWorkstationClassPart *wcp =
	&((NhlWorkstationClass)wkl->base.layer_class)->work_class;
	NhlBoundingBox bbox;
	NhlErrorTypes ret;

	if (!wcp->update_drawbb)
		return NhlNOERROR;

	ret = NhlGetBB(vl->base.id,&bbox); 

	if (ret < NhlWARNING)
		return ret;

	return MIN(ret,(*wcp->update_drawbb)(wkl,&bbox));
	
	
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

        
void _NhlSetAntialiasingMode(NhlLayer layer, NhlAntiAliasMode mode)
{
        NhlWorkstationLayer wLayer = (NhlWorkstationLayer) layer;
        
        int aaState = wLayer->work.antialias;
        if (mode == NhlTEXT_ANTIALIAS_MODE) {
            if (wLayer->work.antialias != NhlANTIALIAS_OFF)
                aaState = True;
        }
        else if (mode == NhlNON_TEXT_ANTIALIAS_MODE) {
            if (wLayer->work.antialias != NhlANTIALIAS_ON)
                aaState = False;
        }
        
        if (aaState != wLayer->work.curr_antialias_state) {
           _NGCAntiAlias antiAliasRec;
           Gescape_in_data gesc;
           antiAliasRec.type = NGC_ANTIALIAS;
           antiAliasRec.work_id = wLayer->work.gkswksid;
           antiAliasRec.antialias_boolean = aaState;
           gesc.escape_r1.data = &antiAliasRec;
           gesc.escape_r1.size = sizeof(antiAliasRec);
           gescape(NGESC_CNATIVE, &gesc, NULL, NULL);                    
            
            wLayer->work.curr_antialias_state = aaState;
        }
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

/*
 * Function:	CheckAndNotify
 *
 * Description: finds the GksId in the Gks Wks record. If the workstation
 *              class has a notify function, calls it.
 *
 * In Args:  
 *
 * Out Args: 
 *
 * Scope:	
 * Returns: ErrorTypes
 *
 * Side Effect:	
 */
static NhlErrorTypes CheckAndNotify
#if	NhlNeedProto
(
        int gks_id,
        int action,
        NhlBoolean *notified
)
#else
(gks_id,action,notified)
	int gks_id;
        int action;
        NhlBoolean *notified;
#endif
{
        wkGksWksRec	*gksp = Gks_Wks_Recs;
        NhlLayer l;
        NhlWorkstationClassPart *wcp;
        int i,j;

        *notified = False;
        for (i = 0; i < CurrentWksCount; i++) {
                if (gksp[i].gks_id == gks_id)
                        break;
        }
        if (i == CurrentWksCount) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "Gks Workstation %d is not open",gks_id));
                return NhlFATAL;
        }
        if (gksp[i].hlu_id == NhlNULLOBJID) {
                if (gksp[i].gks_type == 1) {
                        for (j=0;j<CurrentWksCount;j++) {
                                if (gksp[j].gks_type == 1 && j != i) {
                                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
   "Cannot access non-HLU GKS ncgm workstation while NcgmWorkstation exists"));
                                        return NhlFATAL;
                                }
                        }
                }
                return NhlNOERROR;
        }
        l = _NhlGetLayer(gksp[i].hlu_id);
	if(! (l && _NhlIsWorkstation(l))) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "Hlu Workstation invalid for Gks Id %d",gks_id));
                return NhlFATAL;
        }
	wcp = &((NhlWorkstationClass)l->base.layer_class)->work_class;

        if (wcp->notify_work) {
                (*wcp->notify_work)(l,action);
                *notified = True;
        }
        
        return NhlNOERROR;
}

/*
 * Function:	LLUpdateGksWksRecs
 *
 * Description:	adds to or removes a non-HLU workstation from the gks open list
 *
 * In Args:  if add is True adds it; if False removes it.
 *
 * Out Args: 
 *
 * Scope:	
 * Returns: if add is True:
 *			True if gks_id not in use, False otherwise.
 *          if add is False:
 *			True if removed from list, False if not found.
 * Side Effect:	
 */

static NhlBoolean LLUpdateGksWksRecs
#if	NhlNeedProto
(
        int gks_id,
        NhlBoolean add
)
#else
(gks_id,add)
	int gks_id;
        NhlBoolean add;
#endif
{
        wkGksWksRec	*gksp = Gks_Wks_Recs;
        int i,j;

        if (! add) {
                for (i = 0; i < CurrentWksCount; i++) {
                        if (gksp[i].gks_id == gks_id) {
                                for (j = i; j<CurrentWksCount-1; j++) {
                                        gksp[j] = gksp[j+1];
                                }
                                CurrentWksCount--;
                                return True;
                        }
                }
                return False;
        }
        for (i = 0; i < CurrentWksCount; i++) {
                if (gksp[i].gks_id == gks_id)
                        return False;
                else if (gksp[i].gks_id > gks_id) {
                        for (j = CurrentWksCount; j > i; j--) {
                                gksp[j] = gksp[j-1];
                        }
                        break;
                }
        }
        gksp[i].gks_id = gks_id;
        gksp[i].hlu_id = NhlNULLOBJID;
        CurrentWksCount++;
        
        return True;
}

/*
 * Function:	_NhlUpdateGksWksRecs
 *
 * Description:	adds to or removes an HLU workstation from the gks open list
 *
 * In Args:  if add is True adds it; if False removes it.
 *
 * Out Args: gks_id if add is True,
 *		this is the gks id to use for the gopwk call
 *                      
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */

NhlErrorTypes _NhlUpdateGksWksRecs
#if	NhlNeedProto
(
	NhlLayer l,
        NhlBoolean add,
        int	*gks_id
)
#else
(l,add,gks_id)
	NhlLayer l;
        NhlBoolean add;
        int	*gks_id;
#endif
{
	NhlWorkstationLayer wl;
        NhlWorkstationClassPart *wcp;
        char func[] = "_NhlUpdateGksWksRecs";
        wkGksWksRec		*gksp;
        int i,j;

	if(! (l && _NhlIsWorkstation(l)))
		return NhlFATAL;

	wl = (NhlWorkstationLayer) l;
	wcp = &((NhlWorkstationClass)l->base.layer_class)->work_class;
        gksp = wcp->gks_wks_recs;
        if (! add) {
                for (i = 0; i < *wcp->current_wks_count; i++) {
                        if (gksp[i].hlu_id == l->base.id) {
                                for (j = i;
                                     j<(*wcp->current_wks_count)-1; j++) {
                                        gksp[j] = gksp[j+1];
                                }
                                (*wcp->current_wks_count)--;
                                return NhlNOERROR;
                        }
                }
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                           "%s: no entry found for Workstation %d",
                           func,l->base.id));
                return NhlWARNING;
        }
        if (! gks_id) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "%s: internal error: null Gks id pointer",func));
                return NhlFATAL;
        }
        
            /* start with 3, so that segment workstation can always get 2 */
        i = 3;
        while(wksisopn(i)) {
		i++;
	}
	*gks_id = i;
        
        for (i = 0; i < *wcp->current_wks_count; i++) {
                if (gksp[i].gks_id > *gks_id) {
                        for (j = *wcp->current_wks_count; j>i; j--) {
                                gksp[j] = gksp[j-1];
                        }
                        break;
                }
        }
        gksp[i].gks_id = *gks_id;
        gksp[i].hlu_id = l->base.id;
        (*wcp->current_wks_count)++;
        
        return NhlNOERROR;
        
}

/*
 * Function:	StoreGksWksType
 *
 * Description:	Stores the workstation type of a newly opened workstation.
 *
 * In Args:  
 *
 * Out Args: 
 *                      
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */

static void StoreGksWksType
#if	NhlNeedProto
(
        int	gks_id,
        int	wtype
)
#else
(gks_id,wtype)
        int	gks_id;
        int	wtype;
#endif
{
        wkGksWksRec	*gksp = Gks_Wks_Recs;
        int i;

        for (i = 0; i < CurrentWksCount; i++) {
                if (gksp[i].gks_id == gks_id)
                        gksp[i].gks_type = wtype;
        }
}

/*
 * Function:	gopwk
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
void _NHLCALLF(gopwk,GOPWK)
#if	NhlNeedProto
(
	int	*wkid,
        int	*conid,
        int	*wtype
)
#else
(wkid,conid,wtype)
	int	*wkid;
        int	*conid;
        int	*wtype;
#endif
{
        char func[] = "GOPWK";
        
#if DEBUG_NCGM
	fprintf(stderr,"in hlu gopwk\n");
#endif

        if (! Hlu_Wks_Flag) {
                wkGksWksRec	*gksp = Gks_Wks_Recs;
                if (gksp[0].gks_id != -1 && *wtype == 1) {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
"%s: cannot open non-HLU GKS metafile workstation after initializing NhlworkstationClass",
                                   func));
                        return;
                }
                LLUpdateGksWksRecs(*wkid,True);
        }
	_NHLCALLF(gzopwk,GZOPWK)(wkid,conid,wtype);
	_NhlLLErrCheckPrnt(NhlFATAL,func);

        StoreGksWksType(*wkid,*wtype);
        
        Hlu_Wks_Flag = False;
        
	return;
}

/*
 * Function:	gclwk
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Sccle:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(gclwk,GCLWK)
#if	NhlNeedProto
(
	int	*wkid
)
#else
(wkid)
	int	*wkid;
#endif
{
        char func[] = "GCLWK";
#if DEBUG_NCGM
	fprintf(stderr,"in hlu gclwk\n");
#endif
        if (! Hlu_Wks_Flag) {
                NhlBoolean notified;
                if (CheckAndNotify(*wkid,_NhlwkLLUClose,&notified)
                        < NhlWARNING)
                        return;
                else if (notified)
                        return;
                LLUpdateGksWksRecs(*wkid,False);
        }

	_NHLCALLF(gzclwk,GZCLWK)(wkid);
	_NhlLLErrCheckPrnt(NhlFATAL,func);

        Hlu_Wks_Flag = False;
        
	return;
}


/*
 * Function:	gacwk
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
void _NHLCALLF(gacwk,GACWK)
#if	NhlNeedProto
(
	int	*wkid
)
#else
(wkid)
	int	*wkid;
#endif
{
        char func[] = "GACWK";
#if DEBUG_NCGM
	fprintf(stderr,"in hlu gacwk\n");
#endif
        if (! Hlu_Wks_Flag) {
                NhlBoolean notified;
                if (CheckAndNotify(*wkid,_NhlwkLLUActivate,&notified)
                        < NhlWARNING)
                        return;
                else if (notified)
                        return;
        }
        
	_NHLCALLF(gzacwk,GZACWK)(wkid);
	_NhlLLErrCheckPrnt(NhlFATAL,func);

        Hlu_Wks_Flag = False;
        
	return;
}


/*
 * Function:	gdacwk
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
void _NHLCALLF(gdawk,GDAWK)
#if	NhlNeedProto
(
	int	*wkid
)
#else
(wkid)
	int	*wkid;
#endif
{
        char func[] = "GDAWK";
#if DEBUG_NCGM
	fprintf(stderr,"in hlu gdawk\n");
#endif
        if (! Hlu_Wks_Flag) {
                NhlBoolean notified;
                if (CheckAndNotify(*wkid,_NhlwkLLUDeactivate,&notified)
                        < NhlWARNING)
                        return;
                else if (notified)
                        return;
        }
        
	_NHLCALLF(gzdawk,GZDAWK)(wkid);
	_NhlLLErrCheckPrnt(NhlFATAL,func);
        
        Hlu_Wks_Flag = False;

	return;
}

/*
 * Function:	gclrwk
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
void _NHLCALLF(gclrwk,GCLRWK)
#if	NhlNeedProto
(
	int	*wkid,
	int     *cofl
)
#else
(wkid,cofl)
	int	*wkid;
	int     *cofl;
#endif
{
        char func[] = "GDCLRWK";
        
#if DEBUG_NCGM
	fprintf(stderr,"in hlu gclrwk\n");
#endif
        

        if (! Hlu_Wks_Flag) {
                NhlBoolean notified;
                if (CheckAndNotify(*wkid,_NhlwkLLUClear,&notified)
                        < NhlWARNING)
                        return;
                else if (notified)
                        return;
        }
        
	_NHLCALLF(gzclrwk,GZCLRWK)(wkid,cofl);
	_NhlLLErrCheckPrnt(NhlFATAL,func);
        
        Hlu_Wks_Flag = False;

	return;
}


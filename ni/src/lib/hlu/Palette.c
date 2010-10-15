/*
 *      $Id: Palette.c,v 1.10.4.1 2008-03-28 20:37:36 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Palette.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Apr 18 18:34:21 MDT 1995
 *
 *	Description:	
 */

#include <stdlib.h>
#if defined(Darwin) 
#include <machine/types.h>
#endif
#include <dirent.h>
#include <ctype.h>

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/WorkstationP.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/hlu/ConvertersP.h>

#define NDV_COLORMAP_PATH  	"NDV_COLORMAP_PATH"
#define NCARG_COLORMAP_PATH  	"NCARG_COLORMAP_PATH"
#define DEFAULT_COLORMAP_PATH 	".:$NCARG_ROOT/lib/ncarg/colormaps"

#define Oset(field)	NhlOffset(NhlPaletteLayerRec,pal.field)
static NhlResource resources[] = {
	{_NhlNpalWorkClass,_NhlCpalWorkClass,NhlTPointer,sizeof(NhlPointer),
		 Oset(work_class),NhlTImmediate,_NhlUSET(NULL),_NhlRES_CONLY,NULL}
};
#undef Oset

static NhlColor	cyclic[] = {
	{-1.0,0.0,0.0},	/* white/black */
	{-1.0,0.0,0.0},	/* white/black */
	{1.0,0.0,0.0},	/* red */
	{0.0,1.0,0.0},	/* green */
	{0.0,0.0,1.0},	/* blue */
	{1.0,1.0,0.0},	/* yellow */
	{0.0,1.0,1.0},	/* cyan */
	{1.0,0.0,1.0}	/* magenta */
};

static NhlColor	def[] = {
	{-1.0,0.0,0.0},	/* white/black */
	{-1.0,0.0,0.0},	/* white/black */
	{1.0,0.0,0.0},	/* red */
	{0.0,1.0,0.0},	/* green */
	{0.0,0.0,1.0},	/* blue */
	{1.0,1.0,0.0},	/* yellow */
	{0.0,1.0,1.0},	/* cyan */
	{1.0,0.0,1.0},	/* magenta */
	{0.5,0.0,0.0},
	{0.5,1.0,1.0},
	{0.0,0.0,0.5},
	{1.0,1.0,0.5},
	{0.5,0.0,1.0},
	{1.0,0.5,0.0},
	{0.0,0.5,1.0},
	{0.5,1.0,0.0},
	{0.5,0.0,0.5},
	{0.5,1.0,0.5},
	{1.0,0.5,1.0},
	{0.0,0.5,0.0},
	{0.5,0.5,1.0},
	{1.0,0.0,0.5},
	{0.5,0.5,0.0},
	{0.0,0.5,0.5},
	{1.0,0.5,0.5},
	{0.0,1.0,0.5},
	{0.5,0.5,0.5},
	{0.125,0.125,0.125},
	{0.75,0.75,0.75},
	{0.25,0.25,0.25},
	{0.625,0.625,0.625},
	{0.375,0.375,0.375}
};

static NhlColor	gscyclic[] = {
	{-1.0,0.0,0.0},	/* white/black */
	{-1.0,0.0,0.0},	/* white/black */
	{0.5,0.5,0.5},
	{0.125,0.125,0.125},
	{0.75,0.75,0.75},
	{0.25,0.25,0.25},
	{0.625,0.625,0.625},
	{0.375,0.375,0.375}
};

static NhlColor gsld[] = {
	{0.00000,0.00000,0.00000},
	{1.00000,1.00000,1.00000},
	{0.96875,0.96875,0.96875},
	{0.93750,0.93750,0.93750},
	{0.90625,0.90625,0.90625},
	{0.87500,0.87500,0.87500},
	{0.84375,0.84375,0.84375},
	{0.81250,0.81250,0.81250},
	{0.78125,0.78125,0.78125},
	{0.75000,0.75000,0.75000},
	{0.71875,0.71875,0.71875},
	{0.68750,0.68750,0.68750},
	{0.65625,0.65625,0.65625},
	{0.62500,0.62500,0.62500},
	{0.59375,0.59375,0.59375},
	{0.56250,0.56250,0.56250},
	{0.53125,0.53125,0.53125},
	{0.50000,0.50000,0.50000},
	{0.46875,0.46875,0.46875},
	{0.43750,0.43750,0.43750},
	{0.40625,0.40625,0.40625},
	{0.37500,0.37500,0.37500},
	{0.34375,0.34375,0.34375},
	{0.31250,0.31250,0.31250},
	{0.28125,0.28125,0.28125},
	{0.25000,0.25000,0.25000},
	{0.21875,0.21875,0.21875},
	{0.18750,0.18750,0.18750},
	{0.15625,0.15625,0.15625},
	{0.12500,0.12500,0.12500},
	{0.09375,0.09375,0.09375},
	{0.06250,0.06250,0.06250},
	{0.03125,0.03125,0.03125}
};

static NhlColor gsdl[] = {
	{1.00000,1.00000,1.00000},
	{0.00000,0.00000,0.00000},
	{0.03125,0.03125,0.03125},
	{0.06250,0.06250,0.06250},
	{0.09375,0.09375,0.09375},
	{0.12500,0.12500,0.12500},
	{0.15625,0.15625,0.15625},
	{0.18750,0.18750,0.18750},
	{0.21875,0.21875,0.21875},
	{0.25000,0.25000,0.25000},
	{0.28125,0.28125,0.28125},
	{0.31250,0.31250,0.31250},
	{0.34375,0.34375,0.34375},
	{0.37500,0.37500,0.37500},
	{0.40625,0.40625,0.40625},
	{0.43750,0.43750,0.43750},
	{0.46875,0.46875,0.46875},
	{0.50000,0.50000,0.50000},
	{0.53125,0.53125,0.53125},
	{0.56250,0.56250,0.56250},
	{0.59375,0.59375,0.59375},
	{0.62500,0.62500,0.62500},
	{0.65625,0.65625,0.65625},
	{0.68750,0.68750,0.68750},
	{0.71875,0.71875,0.71875},
	{0.75000,0.75000,0.75000},
	{0.78125,0.78125,0.78125},
	{0.81250,0.81250,0.81250},
	{0.84375,0.84375,0.84375},
	{0.87500,0.87500,0.87500},
	{0.90625,0.90625,0.90625},
	{0.93750,0.93750,0.93750},
	{0.96875,0.96875,0.96875}
};

static NhlColor uniform[] = {
	{-1.0,0.0,0.0},	/* white/black */
	{-1.0,0.0,0.0},	/* white/black */
	{0.7500,1.0000,1.0000},
	{0.5000,1.0000,1.0000},
	{0.2500,1.0000,1.0000},
	{0.0000,1.0000,1.0000},
	{1.0000,0.8333,1.0000},
	{0.7500,0.8333,1.0000},
	{0.5000,0.8333,1.0000},
	{0.2500,0.8333,1.0000},
	{0.0000,0.8333,1.0000},
	{1.0000,0.6667,1.0000},
	{0.7500,0.6667,1.0000},
	{0.5000,0.6667,1.0000},
	{0.2500,0.6667,1.0000},
	{0.0000,0.6667,1.0000},
	{1.0000,0.5000,1.0000},
	{0.7500,0.5000,1.0000},
	{0.5000,0.5000,1.0000},
	{0.2500,0.5000,1.0000},
	{0.0000,0.5000,1.0000},
	{1.0000,0.3333,1.0000},
	{0.7500,0.3333,1.0000},
	{0.5000,0.3333,1.0000},
	{0.2500,0.3333,1.0000},
	{0.0000,0.3333,1.0000},
	{1.0000,0.1667,1.0000},
	{0.7500,0.1667,1.0000},
	{0.5000,0.1667,1.0000},
	{0.2500,0.1667,1.0000},
	{0.0000,0.1667,1.0000},
	{1.0000,0.0000,1.0000},
	{0.7500,0.0000,1.0000},
	{0.5000,0.0000,1.0000},
	{0.2500,0.0000,1.0000},
	{0.0000,0.0000,1.0000},
	{1.0000,1.0000,0.2500},
	{0.7500,1.0000,0.2500},
	{0.5000,1.0000,0.2500},
	{0.2500,1.0000,0.2500},
	{0.0000,1.0000,0.2500},
	{1.0000,0.8333,0.2500},
	{0.7500,0.8333,0.2500},
	{0.5000,0.8333,0.2500},
	{0.2500,0.8333,0.2500},
	{0.0000,0.8333,0.2500},
	{1.0000,0.6667,0.2500},
	{0.7500,0.6667,0.2500},
	{0.5000,0.6667,0.2500},
	{0.2500,0.6667,0.2500},
	{0.0000,0.6667,0.2500},
	{1.0000,0.5000,0.2500},
	{0.7500,0.5000,0.2500},
	{0.5000,0.5000,0.2500},
	{0.2500,0.5000,0.2500},
	{0.0000,0.5000,0.2500},
	{1.0000,0.3333,0.2500},
	{0.7500,0.3333,0.2500},
	{0.5000,0.3333,0.2500},
	{0.2500,0.3333,0.2500},
	{0.0000,0.3333,0.2500},
	{1.0000,0.1667,0.2500},
	{0.7500,0.1667,0.2500},
	{0.5000,0.1667,0.2500},
	{0.2500,0.1667,0.2500},
	{0.0000,0.1667,0.2500},
	{1.0000,0.0000,0.2500},
	{0.7500,0.0000,0.2500},
	{0.5000,0.0000,0.2500},
	{0.2500,0.0000,0.2500},
	{0.0000,0.0000,0.2500},
	{1.0000,1.0000,0.5000},
	{0.7500,1.0000,0.5000},
	{0.5000,1.0000,0.5000},
	{0.2500,1.0000,0.5000},
	{0.0000,1.0000,0.5000},
	{1.0000,0.8333,0.5000},
	{0.7500,0.8333,0.5000},
	{0.5000,0.8333,0.5000},
	{0.2500,0.8333,0.5000},
	{0.0000,0.8333,0.5000},
	{1.0000,0.6667,0.5000},
	{0.7500,0.6667,0.5000},
	{0.5000,0.6667,0.5000},
	{0.2500,0.6667,0.5000},
	{0.0000,0.6667,0.5000},
	{1.0000,0.5000,0.5000},
	{0.7500,0.5000,0.5000},
	{0.5000,0.5000,0.5000},
	{0.2500,0.5000,0.5000},
	{0.0000,0.5000,0.5000},
	{1.0000,0.3333,0.5000},
	{0.7500,0.3333,0.5000},
	{0.5000,0.3333,0.5000},
	{0.2500,0.3333,0.5000},
	{0.0000,0.3333,0.5000},
	{1.0000,0.1667,0.5000},
	{0.7500,0.1667,0.5000},
	{0.5000,0.1667,0.5000},
	{0.2500,0.1667,0.5000},
	{0.0000,0.1667,0.5000},
	{1.0000,0.0000,0.5000},
	{0.7500,0.0000,0.5000},
	{0.5000,0.0000,0.5000},
	{0.2500,0.0000,0.5000},
	{0.0000,0.0000,0.5000},
	{1.0000,1.0000,0.7500},
	{0.7500,1.0000,0.7500},
	{0.5000,1.0000,0.7500},
	{0.2500,1.0000,0.7500},
	{0.0000,1.0000,0.7500},
	{1.0000,0.8333,0.7500},
	{0.7500,0.8333,0.7500},
	{0.5000,0.8333,0.7500},
	{0.2500,0.8333,0.7500},
	{0.0000,0.8333,0.7500},
	{1.0000,0.6667,0.7500},
	{0.7500,0.6667,0.7500},
	{0.5000,0.6667,0.7500},
	{0.2500,0.6667,0.7500},
	{0.0000,0.6667,0.7500},
	{1.0000,0.5000,0.7500},
	{0.7500,0.5000,0.7500},
	{0.5000,0.5000,0.7500},
	{0.2500,0.5000,0.7500},
	{0.0000,0.5000,0.7500},
	{1.0000,0.3333,0.7500},
	{0.7500,0.3333,0.7500},
	{0.5000,0.3333,0.7500},
	{0.2500,0.3333,0.7500},
	{0.0000,0.3333,0.7500},
	{1.0000,0.1667,0.7500},
	{0.7500,0.1667,0.7500},
	{0.5000,0.1667,0.7500},
	{0.2500,0.1667,0.7500},
	{0.0000,0.1667,0.7500},
	{1.0000,0.0000,0.7500},
	{0.7500,0.0000,0.7500},
	{0.5000,0.0000,0.7500},
	{0.2500,0.0000,0.7500},
	{0.0000,0.0000,0.7500},
	{1.0000,1.0000,0.0000},
	{0.7500,1.0000,0.0000},
	{0.5000,1.0000,0.0000},
	{0.2500,1.0000,0.0000},
	{0.0000,1.0000,0.0000},
	{1.0000,0.8333,0.0000},
	{0.7500,0.8333,0.0000},
	{0.5000,0.8333,0.0000},
	{0.2500,0.8333,0.0000},
	{0.0000,0.8333,0.0000},
	{1.0000,0.6667,0.0000},
	{0.7500,0.6667,0.0000},
	{0.5000,0.6667,0.0000},
	{0.2500,0.6667,0.0000},
	{0.0000,0.6667,0.0000},
	{1.0000,0.5000,0.0000},
	{0.7500,0.5000,0.0000},
	{0.5000,0.5000,0.0000},
	{0.2500,0.5000,0.0000},
	{0.0000,0.5000,0.0000},
	{1.0000,0.3333,0.0000},
	{0.7500,0.3333,0.0000},
	{0.5000,0.3333,0.0000},
	{0.2500,0.3333,0.0000},
	{0.0000,0.3333,0.0000},
	{1.0000,0.1667,0.0000},
	{0.7500,0.1667,0.0000},
	{0.5000,0.1667,0.0000},
	{0.2500,0.1667,0.0000},
	{0.0000,0.1667,0.0000},
	{1.0000,0.0000,0.0000},
	{0.7500,0.0000,0.0000},
	{0.5000,0.0000,0.0000},
	{0.2500,0.0000,0.0000}
};

static NhlColor temp1[] = {
	{-1.0,0.0,0.0},	/* white/black */
	{-1.0,0.0,0.0},	/* white/black */
        {0.700,0.700,0.700},
        {0.650,0.650,0.700},
        {0.610,0.600,0.700},
        {0.550,0.550,0.700},
        {0.560,0.500,0.700},
        {0.450,0.450,0.700},
        {0.420,0.400,0.700},
        {0.350,0.350,0.700},
        {0.300,0.300,0.700},
        {0.250,0.250,0.700},
        {0.200,0.200,0.700},
        {0.150,0.150,0.700},
        {0.100,0.100,0.700},
        {0.050,0.050,0.700},
        {0.000,0.000,0.700},
        {0.000,0.050,0.700},
        {0.000,0.100,0.700},
        {0.000,0.150,0.700},
        {0.000,0.200,0.700},
        {0.000,0.250,0.700},
        {0.000,0.300,0.700},
        {0.000,0.350,0.700},
        {0.000,0.400,0.700},
        {0.000,0.450,0.600},
        {0.000,0.500,0.500},
        {0.000,0.550,0.400},
        {0.000,0.600,0.300},
        {0.000,0.650,0.200},
        {0.000,0.700,0.100},
        {0.000,0.725,0.000},
        {0.000,0.690,0.000},
        {0.030,0.685,0.000},
        {0.060,0.680,0.000},
        {0.100,0.575,0.000},
        {0.130,0.570,0.000},
        {0.160,0.565,0.000},
        {0.550,0.550,0.000},
        {0.555,0.545,0.000},
        {0.560,0.530,0.000},
        {0.565,0.485,0.000},
        {0.570,0.420,0.000},
        {0.675,0.375,0.000},
        {0.680,0.330,0.000},
        {0.690,0.300,0.000},
        {0.700,0.285,0.000},
        {0.700,0.270,0.000},
        {0.700,0.260,0.000},
        {0.700,0.240,0.000},
        {0.700,0.180,0.000},
        {0.700,0.130,0.000},
        {0.700,0.120,0.000},
        {0.700,0.100,0.000},
        {0.700,0.090,0.000},
        {0.750,0.090,0.000},
        {0.800,0.090,0.000},
        {0.830,0.070,0.000},
        {0.870,0.050,0.000},
        {0.900,0.030,0.000},
        {0.950,0.010,0.000},
        {0.990,0.000,0.000},
        {1.000,0.000,0.000}
};

static NhlColor psgcap[] = {
	{-1.00,0.00,0.00},
	{-1.00,0.00,0.00},
	{1.00,0.00,0.00},
	{0.00,1.00,0.00},
	{0.00,0.00,1.00},
	{0.00,1.00,1.00},
	{1.00,0.00,0.83},
	{1.00,1.00,0.00},
	{1.00,0.50,0.00},
	{0.60,0.83,0.00},
	{0.00,1.00,0.60},
	{0.00,0.50,1.00},
	{0.55,0.00,0.83},
	{1.00,0.00,0.55},
	{0.33,0.33,0.33},
	{0.67,0.67,0.67},
	{1.00,1.00,0.33},
	{0.75,1.00,0.45},
	{0.45,1.00,0.60},
	{0.17,1.00,0.75},
	{0.25,0.83,0.83},
	{0.50,0.67,0.83},
	{0.75,0.55,0.83},
	{1.00,0.33,0.90},
	{0.67,0.90,0.45},
	{0.40,0.90,0.55},
	{0.17,0.90,0.67},
	{0.17,0.67,0.90},
	{0.17,0.50,1.00},
	{0.45,0.33,1.00},
	{0.75,0.17,1.00},
	{0.90,0.09,1.00},
	{0.83,1.00,0.17},
	{0.67,1.00,0.25},
	{0.45,1.00,0.33},
	{0.17,1.00,0.50},
	{0.17,0.83,0.60},
	{0.17,0.67,0.75},
	{0.17,0.55,0.83},
	{0.25,0.45,0.90},
	{0.40,0.33,0.90},
	{0.67,0.17,0.90},
	{0.83,0.17,0.83},
	{0.90,0.33,0.67},
	{0.83,0.45,0.60},
	{0.83,0.60,0.50},
	{0.90,0.67,0.40},
	{0.90,0.67,0.25},
	{1.00,0.90,0.09},
	{0.83,1.00,0.09},
	{0.60,1.00,0.17},
	{0.45,1.00,0.25},
	{0.17,0.90,0.45},
	{0.17,0.83,0.55},
	{0.17,0.67,0.67},
	{0.17,0.55,0.75},
	{0.17,0.40,0.90},
	{0.40,0.25,0.90},
	{0.55,0.17,0.90},
	{0.83,0.17,0.75},
	{1.00,0.17,0.67},
	{1.00,0.25,0.60},
	{1.00,0.40,0.50},
	{1.00,0.50,0.40},
	{1.00,0.83,0.09},
	{0.75,1.00,0.00},
	{0.60,1.00,0.13},
	{0.40,1.00,0.17},
	{0.13,1.00,0.33},
	{0.13,0.83,0.45},
	{0.17,0.75,0.50},
	{0.13,0.60,0.67},
	{0.13,0.50,0.75},
	{0.13,0.40,0.83},
	{0.17,0.25,0.90},
	{0.17,0.17,1.00},
	{0.33,0.09,1.00},
	{0.55,0.09,0.90},
	{0.75,0.00,0.83},
	{0.90,0.00,0.75},
	{0.90,0.75,0.09},
	{0.67,0.90,0.09},
	{0.55,0.90,0.13},
	{0.25,0.90,0.25},
	{0.13,0.90,0.33},
	{0.17,0.75,0.40},
	{0.13,0.67,0.50},
	{0.13,0.55,0.60},
	{0.13,0.45,0.67},
	{0.17,0.33,0.75},
	{0.17,0.25,0.83},
	{0.33,0.17,0.83},
	{0.55,0.13,0.75},
	{0.75,0.13,0.67},
	{0.90,0.17,0.55},
	{0.83,0.25,0.50},
	{0.83,0.75,0.00},
	{0.60,0.83,0.09},
	{0.45,0.90,0.09},
	{0.33,0.90,0.13},
	{0.13,0.90,0.25},
	{0.17,0.83,0.25},
	{0.17,0.75,0.33},
	{0.17,0.60,0.45},
	{0.13,0.50,0.55},
	{0.13,0.45,0.60},
	{0.17,0.33,0.67},
	{0.33,0.25,0.67},
	{0.45,0.25,0.60},
	{0.67,0.25,0.50},
	{0.83,0.25,0.40},
	{1.00,0.25,0.33},
	{0.75,0.67,0.00},
	{0.55,0.75,0.09},
	{0.40,0.83,0.09},
	{0.25,0.90,0.09},
	{0.17,0.90,0.13},
	{0.17,0.83,0.17},
	{0.13,0.75,0.25},
	{0.13,0.67,0.33},
	{0.13,0.60,0.40},
	{0.13,0.45,0.50},
	{0.13,0.40,0.55},
	{0.25,0.33,0.55},
	{0.45,0.33,0.45},
	{0.55,0.33,0.40},
	{0.67,0.33,0.33},
	{0.83,0.33,0.25},
	{0.67,0.60,0.00},
	{0.50,0.67,0.09},
	{0.40,0.67,0.13},
	{0.25,0.67,0.17},
	{0.13,0.67,0.25},
	{0.09,0.60,0.33},
	{0.09,0.50,0.40},
	{0.13,0.40,0.45},
	{0.13,0.33,0.50},
	{0.09,0.17,0.67},
	{0.17,0.13,0.67},
	{0.40,0.13,0.55},
	{0.50,0.13,0.50},
	{0.60,0.13,0.45},
	{0.75,0.13,0.40},
	{0.90,0.13,0.33},
	{0.55,0.50,0.09},
	{0.45,0.60,0.09},
	{0.33,0.60,0.13},
	{0.17,0.60,0.17},
	{0.09,0.55,0.25},
	{0.13,0.45,0.33},
	{0.09,0.40,0.40},
	{0.17,0.33,0.40},
	{0.17,0.25,0.45},
	{0.25,0.17,0.50},
	{0.25,0.13,0.55},
	{0.33,0.13,0.50},
	{0.45,0.13,0.45},
	{0.55,0.13,0.40},
	{0.67,0.13,0.33},
	{0.83,0.13,0.25},
	{0.50,0.45,0.09},
	{0.40,0.50,0.09},
	{0.33,0.60,0.00},
	{0.17,0.60,0.09},
	{0.13,0.67,0.09},
	{0.13,0.60,0.13},
	{0.13,0.50,0.17},
	{0.09,0.45,0.25},
	{0.13,0.40,0.25},
	{0.13,0.33,0.33},
	{0.09,0.25,0.40},
	{0.09,0.17,0.50},
	{0.09,0.13,0.55},
	{0.13,0.00,0.60},
	{0.33,0.00,0.50},
	{0.50,0.00,0.40},
	{0.75,0.33,0.00},
	{0.67,0.40,0.00},
	{0.55,0.40,0.09},
	{0.33,0.40,0.17},
	{0.25,0.33,0.25},
	{0.25,0.25,0.33},
	{0.25,0.17,0.40},
	{0.33,0.13,0.40},
	{0.50,0.13,0.33},
	{0.60,0.13,0.25},
	{0.75,0.09,0.25},
	{0.83,0.00,0.25},
	{1.00,0.00,0.17},
	{1.00,0.13,0.13},
	{0.90,0.17,0.09},
	{0.83,0.25,0.09},
	{0.60,0.25,0.00},
	{0.45,0.40,0.00},
	{0.25,0.40,0.13},
	{0.25,0.33,0.17},
	{0.17,0.25,0.25},
	{0.17,0.17,0.33},
	{0.25,0.13,0.33},
	{0.33,0.09,0.33},
	{0.50,0.09,0.25},
	{0.67,0.09,0.17},
	{0.83,0.00,0.17},
	{0.90,0.00,0.13},
	{1.00,0.00,0.09},
	{1.00,0.13,0.09},
	{0.83,0.17,0.00},
	{0.75,0.25,0.00},
	{0.50,0.17,0.00},
	{0.40,0.33,0.00},
	{0.13,0.45,0.09},
	{0.09,0.40,0.13},
	{0.09,0.33,0.17},
	{0.09,0.25,0.25},
	{0.13,0.17,0.33},
	{0.17,0.09,0.40},
	{0.25,0.00,0.33},
	{0.45,0.00,0.25},
	{0.60,0.00,0.17},
	{0.75,0.00,0.13},
	{0.83,0.00,0.09},
	{0.75,0.09,0.09},
	{0.67,0.13,0.09},
	{0.60,0.17,0.00},
	{0.40,0.17,0.00},
	{0.17,0.33,0.00},
	{0.09,0.33,0.00},
	{0.09,0.25,0.09},
	{0.09,0.17,0.13},
	{0.09,0.13,0.25},
	{0.09,0.13,0.33},
	{0.09,0.00,0.33},
	{0.13,0.00,0.25},
	{0.17,0.00,0.17},
	{0.33,0.00,0.17},
	{0.33,0.00,0.13},
	{0.40,0.00,0.09},
	{0.45,0.09,0.09},
	{0.45,0.13,0.00},
	{0.17,0.09,0.00},
};

static NhlColor example[] = {
	{-1.000000,0.000000,0.000000},
	{-1.000000,0.000000,0.000000},
	{1.000000,1.000000,0.968627},
	{1.000000,1.000000,0.905882},
	{1.000000,1.000000,0.843137},
	{1.000000,1.000000,0.780392},
	{1.000000,1.000000,0.717647},
	{1.000000,1.000000,0.654902},
	{1.000000,1.000000,0.592157},
	{1.000000,1.000000,0.529412},
	{1.000000,1.000000,0.470588},
	{1.000000,1.000000,0.407843},
	{1.000000,1.000000,0.345098},
	{1.000000,1.000000,0.282353},
	{1.000000,1.000000,0.219608},
	{1.000000,1.000000,0.156863},
	{1.000000,1.000000,0.094118},
	{1.000000,1.000000,0.031373},
	{1.000000,0.968627,0.031373},
	{1.000000,0.905882,0.094118},
	{1.000000,0.843137,0.156863},
	{1.000000,0.780392,0.219608},
	{1.000000,0.717647,0.282353},
	{1.000000,0.654902,0.345098},
	{1.000000,0.592157,0.407843},
	{1.000000,0.529412,0.470588},
	{1.000000,0.470588,0.529412},
	{1.000000,0.407843,0.592157},
	{1.000000,0.345098,0.654902},
	{1.000000,0.282353,0.717647},
	{1.000000,0.219608,0.780392},
	{1.000000,0.156863,0.843137},
	{1.000000,0.094118,0.905882},
	{1.000000,0.031373,0.968627},
	{1.000000,0.000000,0.968627},
	{1.000000,0.000000,0.905882},
	{1.000000,0.000000,0.843137},
	{1.000000,0.000000,0.780392},
	{1.000000,0.000000,0.717647},
	{1.000000,0.000000,0.654902},
	{1.000000,0.000000,0.592157},
	{1.000000,0.000000,0.529412},
	{1.000000,0.000000,0.470588},
	{1.000000,0.000000,0.407843},
	{1.000000,0.000000,0.345098},
	{1.000000,0.000000,0.282353},
	{1.000000,0.000000,0.219608},
	{1.000000,0.000000,0.156863},
	{1.000000,0.000000,0.094118},
	{1.000000,0.000000,0.031373},
	{0.968627,0.031373,0.031373},
	{0.905882,0.094118,0.094118},
	{0.843137,0.156863,0.156863},
	{0.780392,0.219608,0.219608},
	{0.717647,0.282353,0.282353},
	{0.654902,0.345098,0.345098},
	{0.592157,0.407843,0.407843},
	{0.529412,0.470588,0.470588},
	{0.470588,0.529412,0.529412},
	{0.407843,0.592157,0.592157},
	{0.345098,0.654902,0.654902},
	{0.282353,0.717647,0.717647},
	{0.219608,0.780392,0.780392},
	{0.156863,0.843137,0.843137},
	{0.094118,0.905882,0.905882},
	{0.031373,0.968627,0.968627},
	{0.000000,1.000000,0.968627},
	{0.000000,1.000000,0.937255},
	{0.000000,1.000000,0.874510},
	{0.000000,1.000000,0.811765},
	{0.000000,1.000000,0.780392},
	{0.000000,1.000000,0.717647},
	{0.000000,1.000000,0.654902},
	{0.000000,1.000000,0.592157},
	{0.000000,1.000000,0.529412},
	{0.000000,1.000000,0.470588},
	{0.000000,1.000000,0.407843},
	{0.000000,1.000000,0.345098},
	{0.000000,1.000000,0.282353},
	{0.000000,1.000000,0.219608},
	{0.000000,1.000000,0.156863},
	{0.000000,1.000000,0.094118},
	{0.000000,1.000000,0.031373},
	{0.000000,0.968627,0.031373},
	{0.000000,0.905882,0.094118},
	{0.000000,0.843137,0.156863},
	{0.000000,0.780392,0.219608},
	{0.000000,0.717647,0.282353},
	{0.000000,0.654902,0.345098},
	{0.000000,0.592157,0.407843},
	{0.000000,0.529412,0.470588},
	{0.000000,0.470588,0.529412},
	{0.000000,0.407843,0.592157},
	{0.000000,0.345098,0.654902},
	{0.000000,0.282353,0.717647},
	{0.000000,0.219608,0.780392},
	{0.000000,0.156863,0.843137},
	{0.000000,0.094118,0.905882},
	{0.000000,0.031373,0.968627},
	{0.000000,0.000000,0.968627},
	{0.000000,0.000000,0.905882},
	{0.000000,0.000000,0.843137},
	{0.000000,0.000000,0.780392},
	{0.000000,0.000000,0.717647},
	{0.000000,0.000000,0.654902},
	{0.000000,0.000000,0.592157},
	{0.000000,0.000000,0.529412},
	{0.000000,0.000000,0.470588},
	{0.000000,0.000000,0.407843},
	{0.000000,0.000000,0.345098},
	{0.000000,0.000000,0.282353},
	{0.000000,0.000000,0.219608},
	{0.000000,0.000000,0.156863},
	{0.000000,0.000000,0.094118},
	{0.000000,0.000000,0.031373}
};

/*
 * List of all pre-defined colormaps.  Eventually, I would like to add
 * a dynamic list, so the user can add thier own cmap's.
 */
static _NhlPalCmap cmaps[] = {
	{"default",	def,		NhlNumber(def)},
	{"cyclic",	cyclic,		NhlNumber(cyclic)},
	{"gscyclic",	gscyclic,	NhlNumber(gscyclic)},
	{"gsltod",	gsld,		NhlNumber(gsld)},
	{"gsdtol",	gsdl,		NhlNumber(gsdl)},
	{"uniform",	uniform,	NhlNumber(uniform)},
	{"temp1",	temp1,		NhlNumber(temp1)},
	{"psgcap",	psgcap,		NhlNumber(psgcap)},
	{"example",	example,	NhlNumber(example)},
	{NULL,		NULL,		0}
};

static NhlErrorTypes PaletteClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes PaletteInitialize(
#if	NhlNeedProto
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes PaletteDestroy(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

NhlPaletteClassRec NhlpaletteClassRec = {
	/* BaseClassPart */
	{
/* class_name			*/	"paletteClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlPaletteLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	PaletteClassInitialize,
/* layer_initialize		*/	PaletteInitialize,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	PaletteDestroy
	},
	/* PaletteClassPart */
	{
/* default_maps			*/	cmaps
	}
};

NhlClass NhlpaletteClass = (NhlClass)&NhlpaletteClassRec;

static NrmQuark	strgenQ = NrmNULLQUARK;
static NrmQuark	fltgenQ = NrmNULLQUARK;
static NrmQuark	fltQ = NrmNULLQUARK;
static NrmQuark	scalarQ = NrmNULLQUARK;

/*
 * Function:	PaletteClassInitialize
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
PaletteClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	(void)_NhlRegisterTypes(NhlTFloatGenArray,NhlTColorMap,NULL);

	strgenQ = NrmStringToQuark(NhlTStringGenArray);
	fltgenQ = NrmStringToQuark(NhlTFloatGenArray);
	fltQ = NrmStringToQuark(NhlTFloat);
	scalarQ = NrmStringToQuark(NhlTScalar);

	return NhlNOERROR;
}

/*
 * Function:	CvtStringToCmap
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
CvtStringToCmap
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
	char			func[]="CvtStringToCmap";
	NhlString		s1 = from->data.strval;
	NhlGenArray		gen = NULL;
	NrmValue		val;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlWorkstationClass	wc;
	NhlPaletteLayer		pl;
	NhlPalList		cmaps;

	if(nargs != 1 && args[0].addressmode != NhlLAYEROFFSET){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid args?",func);
		return NhlFATAL;
	}
	wc = *((NhlWorkstationClass*)args[0].data.ptrval);
	pl = (NhlPaletteLayer)_NhlGetLayer(wc->work_class.pal);
	cmaps = pl->pal.cmaps;

	gen = _NhlStringToColorDefStringGenArray(wc,s1,False);
	if(!gen)
		gen = _NhlStringToStringGenArray(s1);
	if(gen){
		val.size = sizeof(NhlGenArray);
		val.data.ptrval = gen;

		return _NhlReConvertData(strgenQ,to->typeQ,&val,to);
	}

	while(cmaps){
		if(_NhlCmpString(NrmQuarkToString(cmaps->quark),s1) == 0){
			gen = cmaps->gen;
			break;
		}
		cmaps = cmaps->next;
	}

	if(!gen){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to convert string \"%s\" to %s",func,s1,
			NhlTColorMap);
		return NhlFATAL;
	}

	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
}

/*
 * Function:	CvtGenArrayToCmap
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
CvtGenArrayToCmap
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
	char		func[]="CvtGenArrayToCmap";
	NhlGenArray	gen = from->data.ptrval;
	NhlErrorTypes	ret = NhlNOERROR;

	if(!gen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),gen);
	}

	if((gen->num_dimensions != 2) || (gen->len_dimensions[1] != 3)){
		if((gen->num_elements == 1) &&
			(gen->size <= sizeof(NhlArgVal)) && (gen->size > 0)){
			NrmValue	val;

			memcpy((char*)&val.data,(char*)gen->data,gen->size);
			val.size = gen->size;

			return _NhlReConvertData(gen->typeQ,to->typeQ,&val,to);
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:From array has wrong dimensionality",func);
		return NhlFATAL;
	}

	return _NhlReConvertData(from->typeQ,fltgenQ,from,to);
}

/*
 * Function:	CvtStringGenArrayToCmap
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
CvtStringGenArrayToCmap
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
	char			func[]="CvtStringGenArrayToCmap";
	NhlWorkstationClass	wc;
	NhlGenArray		fgen = from->data.ptrval;
	NhlGenArray		tgen;
	NhlString		*sptr;
	float			*fptr;
	ng_size_t		dimlen[2] = {0,3};
	int			i;
	NhlErrorTypes		ret = NhlNOERROR;

	if(nargs != 1 && args[0].addressmode != NhlLAYEROFFSET){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid args?",func);
		return NhlFATAL;
	}
	wc = *((NhlWorkstationClass*)args[0].data.ptrval);

	if(!fgen){
		_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),fgen);
	}

	if(fgen->num_elements == 1){
		if((fgen->size <= sizeof(NhlArgVal)) && (fgen->size > 0)){
			NrmValue	val;

			memcpy((char*)&val.data,(char*)fgen->data,fgen->size);
			val.size = fgen->size;

			return _NhlReConvertData(fgen->typeQ,to->typeQ,&val,to);
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:From array has wrong dimensionality",func);
		return NhlFATAL;
	}

	if((fgen->num_dimensions == 2) || (fgen->len_dimensions[1] == 3)){
		return _NhlReConvertData(from->typeQ,fltgenQ,from,to);
	}

	if(fgen->num_dimensions != 1){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:From array has wrong dimensionality",func);
		return NhlFATAL;
	}

	dimlen[0] = fgen->num_elements;
	tgen = _NhlConvertCreateGenArray
		(NULL,NhlTFloat,sizeof(float),2,dimlen);
	if(!tgen){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}
	fptr = NhlConvertMalloc(sizeof(float) * fgen->num_elements * 3);
	if(!fptr){
		NhlPError(NhlFATAL,ENOMEM,"%s:unable to create array",func);
		return NhlFATAL;
	}
	tgen->data = fptr;
	sptr = fgen->data;
	for(i=0;i<fgen->num_elements;i++){
		NGRGB	rgb;
		if(!_NhlLookupColor(wc,sptr[i],&rgb)){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Unable to find rgb of \"%s\", using white",
				func,sptr[i]);
			/* set to white */
			rgb.red = rgb.green = rgb.blue = 65535;
		}
		if(rgb.red == 65535)
			*(fptr+(3*i)) = 1.0;
		else
			*(fptr+(3*i)) = (float)rgb.red / 65535.0;
		if(rgb.green == 65535)
			*(fptr+(3*i)+1) = 1.0;
		else
			*(fptr+(3*i)+1) = (float)rgb.green / 65535.0;
		if(rgb.blue == 65535)
			*(fptr+(3*i)+2) = 1.0;
		else
			*(fptr+(3*i)+2) = (float)rgb.blue / 65535.0;
	}
	_NhlSetVal(NhlGenArray,sizeof(NhlGenArray),tgen);
}

/*
 * Function:	PaletteInitialize
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
PaletteInitialize
#if	NhlNeedProto
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
#else
(lc,req,new,args,nargs)
	NhlClass	lc;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		nargs;
#endif
{
	NhlPaletteLayerPart	*pp = &((NhlPaletteLayer)new)->pal;
	NhlPaletteClass		pc = (NhlPaletteClass)lc;
	_NhlPalCmap		*cmptr = pc->pal_class.default_maps;
	NhlPalList		list,*tmp;
	ng_size_t		dimlen[2] = {0,3};
	NhlConvertArg		arg;
	
	arg.addressmode = NhlLAYEROFFSET;
	arg.size = sizeof(NhlPointer);
	arg.data.ptrval = (NhlPointer)NhlOffset(NhlWorkstationLayerRec,
							base.layer_class);

	pp->cmaps = NULL;
	tmp = &pp->cmaps;

	while(cmptr->name){
		list = NhlMalloc(sizeof(NhlPalListRec));
		if(!list){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		list->quark = NrmStringToQuark(cmptr->name);
		dimlen[0] = cmptr->cmap_size;
		list->gen = _NhlCreateGenArray(cmptr->cmap,NhlTFloat,
						sizeof(float),2,dimlen,False);
		if(!list->gen){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}

		list->next = NULL;
		*tmp = list;
		tmp = &list->next;
		cmptr++;
	}


	(void)_NhlRegSymConv(pp->work_class,NhlTQuarkGenArray,NhlTColorMap,
						NhlTQuarkGenArray,NhlTGenArray);
	(void)NhlRegisterConverter(pp->work_class,NhlTString,NhlTColorMap,
					CvtStringToCmap,&arg,1,False,NULL);
	(void)_NhlRegSymConv(pp->work_class,NhlTQuark,NhlTColorMap,
							NhlTQuark,NhlTScalar);
	(void)NhlRegisterConverter(pp->work_class,NhlTGenArray,
			NhlTColorMap,CvtGenArrayToCmap,NULL,0,False,NULL);
	(void)NhlRegisterConverter(pp->work_class,NhlTStringGenArray,
			NhlTColorMap,CvtStringGenArrayToCmap,&arg,1,False,NULL);

	return NhlNOERROR;
}

/*
 * Function:	PaletteDestroy
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
PaletteDestroy
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlPaletteLayer	pl = (NhlPaletteLayer)l;
	NhlPalList	t1,t2;

	t1 = pl->pal.cmaps;

	while(t1){
		t2 = t1;
		t1 = t1->next;
		NhlFreeGenArray(t2->gen);
		NhlFree(t2);
	}
	pl->pal.cmaps = NULL;

	return NhlNOERROR;
}

/*
 * Public API
 */

/*
 * Function:	NhlPalGetDefined
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
int
NhlPalGetDefined
#if	NhlNeedProto
(
	NhlClass	lc,
	NhlString	**names
)
#else
(lc,names)
	NhlClass	lc;
	NhlString	**names;
#endif
{
	char			func[] = "NhlPalGetDefined";
	NhlWorkstationClass	wc;
	int			i,num;
	NhlPaletteLayer		pl;
	NhlPalList		list;

	_NhlInitializeClass(lc);
	if(!(lc->base_class.class_inited & _NhlWorkstationClassFlag)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation Class",
									func);
		return 0;
	}
	wc = (NhlWorkstationClass)lc;
	pl = (NhlPaletteLayer)_NhlGetLayer(wc->work_class.pal);

	list = pl->pal.cmaps;

	num = 0;
	while(list){
		num++;
		list = list->next;
	}

	if(num==0)
		return num;

	*names = NhlMalloc(sizeof(NhlString)*num);
	if(!*names){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return 0;
	}

	for(list = pl->pal.cmaps,i=0;i<num;list = list->next,i++){
		NhlString	tstr;

		tstr = NrmQuarkToString(list->quark);
		if(!tstr)
			(*names)[i] = NULL;
		else{
			(*names)[i] = (NhlString)NhlMalloc(sizeof(char)*(strlen(tstr)+1));
			if(!(*names)[i]){
				/* Leak on error...*/
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				*names = NULL;
				return 0;
			}
			strcpy((*names)[i],tstr);
		}
	}

	return num;
}

/*
 * Function:	NhlPalGetColormap
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
NhlPalGetColormap
#if	NhlNeedProto
(
	NhlClass	lc,
	NhlString	name,
	NhlColor	**cmap,
	int		*cmap_len
)
#else
(lc,name,cmap,cmap_len)
	NhlClass	lc;
	NhlString	name;
	NhlColor	**cmap;
	int		*cmap_len;
#endif
{
	char			func[] = "NhlPalGetColormap";
	NhlWorkstationClass	wc;
	NhlPaletteLayer		pl;
	NhlPalList		list;

	_NhlInitializeClass(lc);
	if(!(lc->base_class.class_inited & _NhlWorkstationClassFlag)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation Class",
									func);
		return NhlFATAL;
	}
	wc = (NhlWorkstationClass)lc;
	pl = (NhlPaletteLayer)_NhlGetLayer(wc->work_class.pal);

	*cmap = NULL;
	*cmap_len = 0;

	list = pl->pal.cmaps;
	while(list){
		if(_NhlCmpString(NrmQuarkToString(list->quark),name) == 0)
			break;
		list = list->next;
	}

	if(!list){
		return NhlNOERROR;
	}

	*cmap = (NhlColor*)NhlMalloc(list->gen->size*list->gen->num_elements);
	if(!*cmap){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	memcpy(*cmap,list->gen->data,list->gen->size*list->gen->num_elements);
	*cmap_len = list->gen->len_dimensions[0];
	
	return NhlNOERROR;
}

/*
 * Function:	NhlPalSetColormap
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
NhlPalSetColormap
#if	NhlNeedProto
(
	NhlClass	lc,
	NhlString	name,
	NhlColor	*cmap,
	int		cmap_len
)
#else
(lc,name,cmap,cmap_len)
	NhlClass	lc;
	NhlString	name;
	NhlColor	*cmap;
	int		cmap_len;
#endif
{
	ng_size_t		dimlen[2] = {0,3};
	char			func[] = "NhlPalSetColormap";
	NhlWorkstationClass	wc;
	NhlPaletteLayer		pl;
	NhlPalList		list,*tmp;
	NhlGenArray		gen;

	_NhlInitializeClass(lc);
	if(!(lc->base_class.class_inited & _NhlWorkstationClassFlag)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation Class",
									func);
		return NhlFATAL;
	}
	wc = (NhlWorkstationClass)lc;
	pl = (NhlPaletteLayer)_NhlGetLayer(wc->work_class.pal);

	dimlen[0] = cmap_len;
	gen = _NhlCreateGenArray(cmap,NhlTFloat,sizeof(float),2,dimlen,True);
	if(!gen){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	list = pl->pal.cmaps;
	while(list){
		if(_NhlCmpString(NrmQuarkToString(list->quark),name) == 0)
			break;
		list = list->next;
	}

	if(!list){
		list = NhlMalloc(sizeof(NhlPalListRec));
		if(!list){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			NhlFreeGenArray(gen);
			return NhlFATAL;
		}
		list->quark = NrmStringToQuark(name);
		list->next = NULL;
		tmp = &pl->pal.cmaps;
		while(*tmp){
			tmp = &(*tmp)->next;
		}
		*tmp = list;

	}
	else{
		NhlFreeGenArray(list->gen);
	}

	list->gen = gen;

	return NhlNOERROR;
}


static NhlErrorTypes GetColormapsInPath
(
	NhlClass	lc,
	const char	*path,
	char		*func
)
{
	NhlErrorTypes	subret,ret = NhlNOERROR;
	struct dirent	*dirp;  
	DIR		*dp;
	int		i;
	int		count;
	char		fullpath[1024];
	char		*endp;
	float   	colormap[768] = { -1.0,-1.0,-1.0,-1.0,-1.0,-1.0 };
	int		min_ix = 6;
	float		max_cval;

/*
 * These colormaps do not overwrite the background and foreground
 * colors. Initially we set foreground to black and background to white.
 * but when the user actually loads one of these color maps these are
 * replaced with the workstation's current background and foreground.
 * Eventually there will be an option for specifying a range of indexes
 * into which the colormap should fit.
 */
	
	if (! path) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
	   "%s: Invalid directory encountered in colormap path specification",
			   func));
		return NhlWARNING;
	}

	if ((dp = opendir(path)) == NULL) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: Invalid colormap directory: %s",func,path));
		return NhlWARNING;
	}

	strcpy(fullpath,path);
	endp = fullpath + strlen(fullpath);
	*endp++ = '/';
	*endp = '\0';

	while ( (dirp = readdir(dp)) != NULL) {
		char *cp;
		FILE *fp;
		char buf[256];
		int dot_pos;

		if (! strcmp(dirp->d_name, ".")  ||
		    ! strcmp(dirp->d_name, ".."))
			continue;	
		if (! (cp = strrchr(dirp->d_name,'.')))
			continue;
		dot_pos = cp - dirp->d_name;
		cp++;
		if (! cp || 
		    (strcmp(cp,"rgb") && 
		     strcmp(cp,"ncmap") &&
		     strcmp(cp,"gp"))) 
			continue;
		
		strcpy(endp,dirp->d_name);
		fp = fopen(fullpath,"r");
		if (! fp) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: Unable to open colormap file %s: ignoring",
				   func,dirp->d_name));
			ret = MIN(NhlWARNING,ret);
			continue;
		}
		i = min_ix;
		max_cval = 0.0;
		while ((cp = fgets(buf,255,fp))) {
			char *next,*tcp = cp;
			float f;
			while (isspace(*tcp))
				tcp++;
			if (! (isdigit(*tcp) || *tcp == '.'))
				continue;
			while (1) {
				if (i > 767)
					break;
				f = strtod(tcp,&next);
				if (next == tcp)
					break;
				tcp = next;
				while (isspace(*tcp) || *tcp == ',')
					tcp++;
				colormap[i] = f;
				max_cval = MAX(max_cval,colormap[i]);
				i++;
			}
		}
		fclose(fp);

		count = i;
		if (max_cval > 1.0) {
			if (max_cval < 256.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 255.0;
				}
			}
			else if (max_cval <= 256.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 256.0;
				}
			}
			else if (max_cval < 65536.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 65535.0;
				}
			}
			else if (max_cval <= 65536.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 65536.0;
				}
			}
			else {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= max_cval;
				}
			}
		}
		
		*(endp + dot_pos) = '\0';
		subret = NhlPalSetColormap(lc,endp,
					   (NhlColor *)colormap,count / 3);
		ret = MIN(ret,subret);
		if (ret < NhlWARNING)
			return ret;

	}

	closedir(dp);

	return ret;
}

static NhlErrorTypes ReadUserColormaps
(
	NhlClass 	lc,
	NhlBoolean	suppress_path_message,
	char	 	*func
)
{
	NhlErrorTypes subret,ret = NhlNOERROR;
	const char *path;
	char buf[1024];
	char *cp;

	path = getenv(NDV_COLORMAP_PATH);
	if (! path)
		path = getenv(NCARG_COLORMAP_PATH);
	if (! path)
		path = _NGGetNCARGEnv("colormaps");
	if (! path) {
		if (! suppress_path_message) {
			fprintf(stderr,
		     "%s environment variable not set:\n\tdefaulting to %s\n",
				NDV_COLORMAP_PATH,DEFAULT_COLORMAP_PATH);
		}
		path = DEFAULT_COLORMAP_PATH;
	}
	if (! path) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		   "%s: Path to colormap files not found; no colomaps loaded",
			  func);
		return NhlWARNING;
	}
	strcpy(buf,path);
/*
 * Search for directories in the path from the end of the path string. 
 * That way, when duplicate names are found the files in directories at the
 * front of the path will replace the ones at the end.
 */
	cp = strrchr(buf,':');
	if (! cp) {
		return GetColormapsInPath(lc,_NGResolvePath(buf),func);
	}
	while (cp) {
		if (*(cp+1)) {
			subret = GetColormapsInPath
				(lc,_NGResolvePath(cp+1),func);
			ret = MIN(subret,ret);
		}
		if (ret < NhlWARNING)
			return ret;
		*cp = '\0';
		if (cp > buf) {
			cp = strrchr(buf, ':');
			if (! cp) {
				subret = GetColormapsInPath
					(lc,_NGResolvePath(buf),func);
				ret = MIN(subret,ret);
			}
		}
	}
	return ret;
}

/*
 * Function:	NhlPalLoadColormapFiles
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
NhlPalLoadColormapFiles
#if	NhlNeedProto
(
	NhlClass	lc,
	NhlBoolean	suppress_path_message
)
#else
(lc,suppress_path_message)
	NhlClass	lc;
	NhlBoolean	suppress_path_message;

#endif
{
	char		func[] = "NhlLoadColormapFiles";

	_NhlInitializeClass(lc);
	if(!(lc->base_class.class_inited & _NhlWorkstationClassFlag)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid Workstation Class",
									func);
		return NhlFATAL;
	}

	return ReadUserColormaps(lc,suppress_path_message,func);
}

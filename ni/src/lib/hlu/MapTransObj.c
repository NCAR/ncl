/*
*      $Id: MapTransObj.c,v 1.24 1996-02-26 21:46:01 dbrown Exp $
*/
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
*	File:		
*
*	Author:		Ethan Alpert
*			National Center for Atmospheric Research
*			PO 3000, Boulder, Colorado
*
*	Date:		Wed Oct 28 15:09:23 MST 1992
*
*	Description:	
*/
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/MapTransObjP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

{NhlNtrOutOfRangeF,NhlCtrOutOfRangeF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,trobj.out_of_range),
	 NhlTString,_NhlUSET("1.0e12"),0,NULL},
{NhlNmpProjection,NhlCmpProjection,NhlTProjection,sizeof(NhlProjection),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.projection),NhlTImmediate,
	 _NhlUSET((NhlPointer)NhlCYLINDRICALEQUIDISTANT),0,NULL},
{NhlNmpCenterLatF,NhlCmpCenterLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.center_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpCenterLonF,NhlCmpCenterLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.center_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpCenterRotF,NhlCmpCenterRotF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.center_rot),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRelativeCenterLat,NhlCmpRelativeCenterLat,NhlTBoolean,
	 sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.rel_center_lat),
	 NhlTImmediate,_NhlUSET((NhlPointer)False) ,0,NULL},
{NhlNmpRelativeCenterLon,NhlCmpRelativeCenterLon,NhlTBoolean,
	 sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.rel_center_lon),
	 NhlTImmediate,_NhlUSET((NhlPointer)False) ,0,NULL},
{NhlNmpPreserveAspectRatio,NhlCmpPreserveAspectRatio,NhlTBoolean,
	 sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.preserve_aspect),
	 NhlTImmediate,_NhlUSET((NhlPointer)True) ,0,NULL},


{NhlNmpLimitMode,NhlCmpLimitMode,NhlTMapLimitMode,
	 sizeof(NhlMapLimitMode),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_limit_mode),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlMAXIMALAREA),0,NULL},

{NhlNmpMinLatF,NhlCmpMinLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.min_lat),
	 NhlTString,_NhlUSET("-90.0"),0,NULL},
{NhlNmpMaxLatF,NhlCmpMaxLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.max_lat),
	 NhlTString,_NhlUSET("90.0"),0,NULL},
{NhlNmpMinLonF,NhlCmpMinLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.min_lon),
	 NhlTString,_NhlUSET("-180.0"),0,NULL},
{NhlNmpMaxLonF,NhlCmpMaxLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.max_lon),
	 NhlTString,_NhlUSET("180.0"),0,NULL},

{NhlNmpLeftAngleF,NhlCmpLeftAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_angle),
	 NhlTString,_NhlUSET("80.0"),0,NULL},
{NhlNmpRightAngleF,NhlCmpRightAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_angle),
	 NhlTString,_NhlUSET("80.0"),0,NULL},
{NhlNmpBottomAngleF,NhlCmpBottomAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_angle),
	 NhlTString,_NhlUSET("80.0"),0,NULL},
{NhlNmpTopAngleF,NhlCmpTopAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_angle),
	 NhlTString,_NhlUSET("80.0"),0,NULL},

{NhlNmpLeftNPCF,NhlCmpLeftNPCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_npc),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightNPCF,NhlCmpRightNPCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_npc),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNmpBottomNPCF,NhlCmpBottomNPCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_npc),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopNPCF,NhlCmpTopNPCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_npc),
	 NhlTString,_NhlUSET("1.0"),0,NULL},

{NhlNmpLeftNDCF,NhlCmpLeftNDCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_ndc),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightNDCF,NhlCmpRightNDCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_ndc),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNmpBottomNDCF,NhlCmpBottomNDCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_ndc),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopNDCF,NhlCmpTopNDCF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_ndc),
	 NhlTString,_NhlUSET("1.0"),0,NULL},

{NhlNmpLeftMapPosF,NhlCmpLeftMapPosF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_l),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightMapPosF,NhlCmpRightMapPosF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_r),
	 NhlTString,_NhlUSET("1.0"),0,NULL},
{NhlNmpBottomMapPosF,NhlCmpBottomMapPosF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_b),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopMapPosF,NhlCmpTopMapPosF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_t),
	 NhlTString,_NhlUSET("1.0"),0,NULL},


{NhlNmpActualMinLatF,NhlCmpActualMinLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.actual_min_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpActualMaxLatF,NhlCmpActualMaxLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.actual_max_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpActualMinLonF,NhlCmpActualMinLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.actual_min_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpActualMaxLonF,NhlCmpActualMaxLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.actual_max_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},


{NhlNmpLeftCornerLatF,NhlCmpLeftCornerLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_corner_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpLeftCornerLonF,NhlCmpLeftCornerLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_corner_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightCornerLatF,NhlCmpRightCornerLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_corner_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightCornerLonF,NhlCmpRightCornerLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_corner_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},

{NhlNmpLeftPointLatF,NhlCmpLeftPointLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_point_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpLeftPointLonF,NhlCmpLeftPointLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_point_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightPointLatF,NhlCmpRightPointLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_point_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightPointLonF,NhlCmpRightPointLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_point_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpBottomPointLatF,NhlCmpBottomPointLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_point_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpBottomPointLonF,NhlCmpBottomPointLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_point_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopPointLatF,NhlCmpTopPointLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_point_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopPointLonF,NhlCmpTopPointLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_point_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},


{NhlNmpLeftWindowF,NhlCmpLeftWindowF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_window),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightWindowF,NhlCmpRightWindowF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_window),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpBottomWindowF,NhlCmpBottomWindowF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_window),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopWindowF,NhlCmpTopWindowF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_window),
	 NhlTString,_NhlUSET("0.0"),0,NULL},

{NhlNmpLambertParallel1F,NhlCmpLambertParallel1F,NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.lambert_parallel_1),
	 NhlTString,_NhlUSET(".001"),0,NULL},
{NhlNmpLambertParallel2F,NhlCmpLambertParallel2F,NhlTFloat, 
	 sizeof(float), 
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.lambert_parallel_2),
	 NhlTString,_NhlUSET("89.999"),0,NULL},
{NhlNmpLambertMeridianF,NhlCmpLambertMeridianF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.lambert_meridian),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpSatelliteDistF,NhlCmpSatelliteDistF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_dist),
	 NhlTString,_NhlUSET("1"),0,NULL},
{NhlNmpSatelliteAngle1F,NhlCmpSatelliteAngle1F,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_angle_1),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpSatelliteAngle2F,NhlCmpSatelliteAngle2F,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_angle_2),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpEllipticalBoundary,NhlCmpEllipticalBoundary,NhlTBoolean,
	 sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.elliptical_boundary),
	 NhlTImmediate,_NhlUSET((NhlPointer)False) ,0,NULL},


/* End-documented-resources */

{NhlNmpTransChanged,NhlNmpTransChanged,NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.trans_changed),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),0,NULL}

};

/*
* Base Methods defined here
*/

static NhlErrorTypes MapTransInitialize(
#if	NhlNeedProto
NhlClass,     /* class */
NhlLayer,          /* req */
NhlLayer,          /* new */
_NhlArgList,    /* args */
int             /* num_args */
#endif
);

static NhlErrorTypes    MapTransClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes  MapTransSetValues(
#if	NhlNeedProto
NhlLayer,          /* old */
NhlLayer,          /* reference */
NhlLayer,          /* new */
_NhlArgList,    /* args */
int             /* num_args*/
#endif
);

/*
* TransObjClass Methods defined
*/

static NhlErrorTypes MapSetTrans(
#if	NhlNeedProto
NhlLayer   /*instance*/,
NhlLayer   /*parent */
#endif
);

static NhlErrorTypes MapWinToNDC(
#if	NhlNeedProto
NhlLayer   /*instance*/,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*  /*xmissing*/,
float*  /*ymissing*/,
int* 	/*status*/
#endif
);
static NhlErrorTypes MapNDCToWin(
#if	NhlNeedProto
NhlLayer   /*instance*/,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*  /*xmissing*/,
float*  /*ymissing*/,
int*	/*status*/
#endif
);


static NhlErrorTypes MapDataToWin(
#if	NhlNeedProto
NhlLayer   /*instance */, 
float*  /*x*/, 
float*   /*y*/, 
int     /* n*/, 
float*  /*xout*/, 
float*  /*yout*/ ,
float*  /*xmissing*/,
float*  /*ymissing*/,
int*	/*status*/
#endif 
); 

static NhlErrorTypes MapWinToData(
#if	NhlNeedProto
NhlLayer   /*instance */, 
float*  /*x*/, 
float*   /*y*/, 
int     /* n*/, 
float*  /*xout*/, 
float*  /*yout*/ ,
float*  /*xmissing*/,
float*  /*ymissing*/,
int*	/*status*/
#endif 
); 

static NhlErrorTypes MapDataToCompc(
#if	NhlNeedProto
NhlLayer   /*instance */, 
float*  /*x*/, 
float*   /*y*/, 
int     /* n*/, 
float*  /*xout*/, 
float*  /*yout*/ ,
float*  /*xmissing*/,
float*  /*ymissing*/,
int*	/*status*/
#endif
);

static NhlErrorTypes MapNDCLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes MapDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes MapWinLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes MapDataPolygon(
#if     NhlNeedProto
NhlLayer   /* instance */,
float*   /* x */,
float*   /* y */,
int     /* n */
#endif
);

/* internal functions */

static NhlErrorTypes GetWindowLimits(
#if     NhlNeedProto
	NhlMapTransObjLayer	mnew,
	float			*wl,
	float			*wr,
	float			*wb,
	float			*wt,
	NhlString		entry_name
#endif
);


static NhlErrorTypes CheckMapLimits(
#if     NhlNeedProto
	NhlMapTransObjLayer	mnew,
	NhlString		entry_name
#endif
);

NhlMapTransObjClassRec NhlmapTransObjClassRec = {
{
/* class_name			*/	"mapTransObjClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlMapTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhltransObjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	MapTransClassInitialize,
/* layer_initialize		*/	MapTransInitialize,
/* layer_set_values		*/	MapTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
},
{
/*
* For Maps Data and Compc are the same hence the following definitions.
*/
/* set_trans		*/	MapSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	MapWinToNDC,
/* ndc_to_win		*/	MapNDCToWin,
/* data_to_win		*/	MapDataToWin, 
/* win_to_data		*/	MapWinToData,
/* data_to_compc	*/	MapDataToCompc, /* compc and data are ident */
/* compc_to_data	*/	MapDataToCompc, /* compc and data are ident */
/* win_to_compc		*/	MapWinToData, /* compc and data are ident */
/* compc_to_win		*/	MapDataToWin, /* compc and data are ident */
/* data_lineto */       MapDataLineTo,
/* compc_lineto */      MapDataLineTo,
/* win_lineto */        MapWinLineTo,
/* NDC_lineto */        MapNDCLineTo,
/* data_polygon */      MapDataPolygon 
}
};

NhlClass NhlmapTransObjClass = (NhlClass) &NhlmapTransObjClassRec;

typedef struct _mpWinLimits {
	float u_min;
	float u_range;
	float v_min;
	float v_range;
} mpWinLimits;

#define mpPI 3.14159265358979323846

static mpWinLimits Win_Limits[] = {
	{ -1.0, 2.0, -1.0, 2.0 },
	{ -2.0, 4.0, -2.0, 4.0 },
	{ -2.0, 4.0, -2.0, 4.0 },
	{ -2.0, 4.0, -2.0, 4.0 },
	{ -mpPI, 2.0 * mpPI, -mpPI, 2.0 * mpPI },
	{ -2.0, 4.0, -1.0, 2.0 },
	{ -mpPI, 2.0 * mpPI, -mpPI, 2.0 * mpPI },
	{ -180.0, 360.0, -90.0, 180.0 },
	{ -1.0, 2.0, -1.0, 2.0 },
	{ -1.0, 2.0, -1.0, 2.0 }
};

/*
* Function:	MapSetTrans
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
static NhlErrorTypes MapSetTrans
#if	NhlNeedProto
( NhlLayer instance,NhlLayer parent )
#else
(instance,parent)
NhlLayer instance;
NhlLayer parent;
#endif
{
	NhlErrorTypes	ret = NhlNOERROR, subret = NhlNOERROR;
	char		*entry_name = "MapSetTrans";
	char		*e_text;
	float	xl;
	float	yt;
	float	width,xr;
	float	height,yb;
	int 	ix,irold,nerr,loglin;
	NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlMapTransObjLayerPart	*mtp = &(minstance->mptrans);
	char *cproj, *climit;
	float v_angle_lim, h_angle_lim, center_lat, center_lon;
	float rl1[2],rl2[2],rl3[2],rl4[2];

	ret =(*NhltransObjClassRec.trobj_class.set_trans)(instance,parent);

	xl = minstance->trobj.x;
	yt = minstance->trobj.y;
	width = minstance->trobj.width;
	height = minstance->trobj.height;
	xr = MIN(1.0,xl + width);
	yb = MAX(0.0,yt - height);
	xl = MAX(0.0,xl);
	yt = MIN(1.0,yt);
	width = xr - xl;
	height = yt -yb;
	c_mappos(xl,xr,yb,yt);
	c_mapsti("EL",mtp->elliptical_boundary);

	switch (mtp->projection) {
	case NhlORTHOGRAPHIC:
		cproj = "OR";
		h_angle_lim = v_angle_lim = 90;
		ix = 0;
		break;
	case NhlSTEREOGRAPHIC:
		cproj = "ST";
		h_angle_lim = v_angle_lim = 180;
		ix = 1;
		break;
	case NhlLAMBERTEQUALAREA:
		cproj = "LE";
		h_angle_lim = v_angle_lim = 180;
		ix = 2;
		break;
	case NhlGNOMONIC:
		cproj = "GN";
		h_angle_lim = v_angle_lim = 85;
		ix = 3;
		break;
	case NhlAZIMUTHALEQUIDISTANT:
		cproj = "AE";
		h_angle_lim = v_angle_lim = 180;
		ix = 4;
		break;
	case NhlMOLLWEIDE:
		cproj = "MO";
		h_angle_lim = v_angle_lim = 180;
		ix = 5;
		break;
	case NhlMERCATOR:
		cproj = "ME";
		h_angle_lim = 180;
		v_angle_lim = 85;
		ix = 6;
		break;
	case NhlCYLINDRICALEQUIDISTANT:
		cproj = "CE";
		h_angle_lim = 90;
		v_angle_lim = 180;
		ix = 7;
		break;
	case NhlLAMBERTCONFORMAL:
		if (mtp->map_limit_mode == NhlANGLES) {
			e_text = "%s: internal check error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cproj = "LC";
		ix = 8;
		break;
	case NhlSATELLITE:
		c_mapstr("SA",mtp->satellite_dist);
		c_mapstr("S1",mtp->satellite_angle_1);
		c_mapstr("S2",mtp->satellite_angle_2);
		h_angle_lim = v_angle_lim = 90;
		cproj = "SV";
		ix = 9;
		break;
	default:
		e_text = "%s: internal enumeration error - projection";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	if (mtp->rel_center_lat && mtp->map_limit_mode == NhlLATLON)
		center_lat = (mtp->max_lat + mtp->min_lat) / 2.0 
			+ mtp->center_lat;
	else
		center_lat = mtp->center_lat;

	center_lon = mtp->projection == NhlLAMBERTCONFORMAL ?
		mtp->lambert_meridian : mtp->center_lon;
	if (mtp->rel_center_lon && mtp->map_limit_mode == NhlLATLON)
		center_lon = (mtp->max_lon + mtp->min_lon) / 2.0
			+ center_lon;

	if (mtp->projection == NhlLAMBERTCONFORMAL) {
		c_maproj(cproj,mtp->lambert_parallel_1,center_lon,
			 mtp->lambert_parallel_2);
	}
	else {
		c_maproj(cproj,center_lat,center_lon,
			 mtp->center_rot);
	}

	switch (mtp->map_limit_mode) {
	case NhlMAXIMALAREA:
		rl1[0] = rl2[0] = rl3[0] = rl4[0] = 0.0;
		climit = "MA";
		break;
	case NhlLATLON:
		if (! mtp->trans_changed) {
			rl1[0] = mtp->ul;
			rl2[0] = mtp->ur;
			rl3[0] = mtp->ub;
			rl4[0] = mtp->ut;
		}
		else {
			subret = GetWindowLimits(minstance,
						 rl1,rl2,rl3,rl4,entry_name);
			if ((ret = MIN(ret,subret)) < NhlWARNING)
				return ret;
		}
		climit = "LI";
		break;
	case NhlANGLES:
		climit = "AN";
		if (fabs(mtp->left_angle) > h_angle_lim || 
		    fabs(mtp->right_angle) > h_angle_lim ||
		    fabs(mtp->bottom_angle) > v_angle_lim ||
		    fabs(mtp->top_angle) > v_angle_lim) {
			e_text = "%s: internal check error - angles";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		rl1[0] = mtp->left_angle;
		rl2[0] = mtp->right_angle;
		rl3[0] = mtp->bottom_angle;
		rl4[0] = mtp->top_angle;
		break;
	case NhlNPC:
	case NhlNDC:

		/* set window limits */

		climit = "LI";
		rl1[0] = Win_Limits[ix].u_min + 
			Win_Limits[ix].u_range * mtp->left_npc;
		rl2[0] = Win_Limits[ix].u_min + 
			Win_Limits[ix].u_range * mtp->right_npc;
		rl3[0] = Win_Limits[ix].v_min + 
			Win_Limits[ix].v_range * mtp->bottom_npc;
		rl4[0] = Win_Limits[ix].v_min + 
			Win_Limits[ix].v_range * mtp->top_npc;
		break;

	case NhlPOINTS:
		climit = "PO";
		rl1[0] = mtp->left_point_lat;
		rl1[1] = mtp->left_point_lon;
		rl2[0] = mtp->right_point_lat;
		rl2[1] = mtp->right_point_lon;
		rl3[0] = mtp->bottom_point_lat;
		rl3[1] = mtp->bottom_point_lon;
		rl4[0] = mtp->top_point_lat;
		rl4[1] = mtp->top_point_lon;
		break;

	case NhlCORNERS:
		climit = "CO";
		rl1[0] = mtp->left_corner_lat;
		rl2[0] = mtp->left_corner_lon;
		rl3[0] = mtp->right_corner_lat;
		rl4[0] = mtp->right_corner_lon;
		break;
	case NhlWINDOW:
		climit = "LI";
		if (mtp->left_window >= mtp->right_window ||
		    mtp->bottom_window >= mtp->top_window) {
			e_text = "%s: internal check error - window limits";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		rl1[0] = mtp->left_window;
		rl2[0] = mtp->right_window;
		rl3[0] = mtp->bottom_window;
		rl4[0] = mtp->top_window;
		break;
	default:
		e_text = "%s: internal enumeration error - map limit mode";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	c_mapset(climit,rl1,rl2,rl3,rl4);

	c_entsr(&irold,1);
	c_mapint();
	c_nerro(&nerr);

	if (nerr > 0) {	
		NhlBoolean recovered = False;
		char *e_msg = c_semess(0);

		c_errof();
		if (strstr(e_msg,"MAP HAS ZERO AREA") ||
		    strstr(e_msg,"MAP LIMITS INAPPROPRIATE")) {
			e_text = "%s: map limits invalid - using maximal area";
			mtp->map_limit_mode = NhlMAXIMALAREA;
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			c_mapset("MA",rl1,rl2,rl3,rl4);
			c_mapint();
			c_mapint();
			c_nerro(&nerr);
			if (nerr > 0) {	
				e_msg = c_semess(0);
				c_errof();
			}
			else {
				recovered = True;
			}
		}
		if (! recovered) {
			e_text = "%s: error initializing map: %s";
			ret = NhlFATAL;
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,e_msg);
			return ret;
		}
	}

	c_getset(&mtp->map_pos_l,&mtp->map_pos_r,&mtp->map_pos_b,
		 &mtp->map_pos_t,&mtp->ul,&mtp->ur,&mtp->ub,&mtp->ut,&loglin);

	mtp->aspect = ((mtp->map_pos_r - mtp->map_pos_l)/
		       (mtp->map_pos_t - mtp->map_pos_b));

	mtp->left_window = mtp->ul;
	mtp->right_window = mtp->ur;
	mtp->bottom_window = mtp->ub;
	mtp->top_window = mtp->ut;

	if (! mtp->preserve_aspect) {

		c_set (xl,xr,yb,yt,mtp->ul,mtp->ur,mtp->ub,mtp->ut,loglin);

		mtp->map_pos_l = xl;
		mtp->map_pos_r = xr;
		mtp->map_pos_t = yt;
		mtp->map_pos_b = yb;
	}

	mtp->left_ndc = mtp->map_pos_l;
	mtp->right_ndc = mtp->map_pos_r;
	mtp->bottom_ndc = mtp->map_pos_b;
	mtp->top_ndc = mtp->map_pos_t;

	if (mtp->map_limit_mode != NhlNDC && mtp->map_limit_mode != NhlNPC) {
		mtp->left_npc = (mtp->left_window - Win_Limits[ix].u_min) /
			Win_Limits[ix].u_range;
		mtp->right_npc = (mtp->right_window - Win_Limits[ix].u_min) /
			Win_Limits[ix].u_range;
		mtp->bottom_npc = (mtp->bottom_window - Win_Limits[ix].v_min) /
			Win_Limits[ix].v_range;
		mtp->top_npc = (mtp->top_window - Win_Limits[ix].v_min) /
			Win_Limits[ix].v_range;
	}
		

	mtp->trans_changed = False;

	return (ret);

}
/*
 * Function:	MapWinToNDC
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
static NhlErrorTypes MapWinToNDC
#if	NhlNeedProto
( NhlLayer instance,float* x,float* y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int* status)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
NhlLayer instance;
float *x;
float *y;
int n;
float *xout;
float *yout;
float *xmissing;
float *ymissing;
int* status;
#endif
{
        NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlMapTransObjLayerPart	*mtp = &(minstance->mptrans);
        int i;
	NhlErrorTypes ret = NhlNOERROR;
	float xmin,ymin,xmax,ymax;



	*status = 0;

	xmin = MIN(mtp->ul,mtp->ur);
	xmax = MAX(mtp->ul,mtp->ur);
	ymin = MIN(mtp->ut,mtp->ub);
	ymax = MAX(mtp->ut,mtp->ub);

	
        for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < xmin)
			||(x[i] > xmax)
			||(y[i] < ymin)
			||(y[i] > ymax)) {
			
			xout[i] = yout[i] = minstance->trobj.out_of_range;
			*status = 1;
		} else {
               		strans(mtp->ul,mtp->ur,
                       		mtp->ub,mtp->ut,
                       		mtp->map_pos_l,
				mtp->map_pos_r,
				mtp->map_pos_b,
				mtp->map_pos_t,
				x[i],y[i],
                       		&(xout[i]),&(yout[i]));
		}
	}

        return(ret);

}

/*
 * Function:	MapNDCToWin
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
static NhlErrorTypes MapNDCToWin
#if	NhlNeedProto
( NhlLayer instance,float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int *status;
#endif
{
        int i;
        NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlMapTransObjLayerPart	*mtp = &(minstance->mptrans);
	NhlErrorTypes ret = NhlNOERROR;


	*status = 0;
        for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i])) 
			||((ymissing != NULL)&&(*ymissing == y[i])) 
			||(x[i] < mtp->map_pos_l)
			||(x[i] > mtp->map_pos_r)
			||(y[i] < mtp->map_pos_b)
			||(y[i] > mtp->map_pos_t)) {
	
			*status = 1;
			xout[i]=yout[i]=minstance->trobj.out_of_range;

		} else {
               	strans( mtp->map_pos_l,
			mtp->map_pos_r,
			mtp->map_pos_b,
			mtp->map_pos_t,
			mtp->ul,mtp->ur,
                       	mtp->ub,mtp->ut,
			x[i],y[i],
                       	&(xout[i]),&(yout[i]));
		}
	}

        return(ret);
}

/*ARGSUSED*/
static NhlErrorTypes MapDataToCompc
#if	NhlNeedProto
( NhlLayer instance,float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int* status;
#endif
{
	int i;
        NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlErrorTypes ret = NhlNOERROR;
	float tmpx,tmpy;

	*status = 0;
	for( i = 0; i< n; i++) {
		if(((xmissing != NULL) &&(*xmissing == x[i]))
			||((ymissing != NULL) &&(*ymissing == y[i]))) {	
			*status = 1;
			xout[i] = yout[i] = minstance->trobj.out_of_range;
		} else {

			c_maptra(y[i],x[i],&tmpx,&tmpy);
/*
* A problem could develop here if 1e12 is not represented identically in
* FORTRAN and C because of arithmetic error
*/
			if((tmpx == 1e12) ||(tmpy == 1e12)) {
				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
			}
		}
	}
	return(ret);
}
/*
 * Function:	MapDataToWin
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
static NhlErrorTypes MapDataToWin
#if	NhlNeedProto
( NhlLayer instance,float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int* status;
#endif
{
	int i;
        NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlErrorTypes ret = NhlNOERROR;

	*status = 0;
	for( i = 0; i< n; i++) {
		if(((xmissing != NULL) &&(*xmissing == x[i]))
			||((ymissing != NULL) &&(*ymissing == y[i]))) {	
			*status = 1;
			xout[i] = yout[i] = minstance->trobj.out_of_range;
		} else {

			c_maptra(y[i],x[i],&(xout[i]),&(yout[i]));
/*
* A problem could develop here if 1e12 is not represented identically in
* FORTRAN and C because of arithmentic error
*/
			if((xout[i] == 1e12)
				||(yout[i] == 1e12)) {
				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
			}
		}
	}
	return(ret);
}

/*
 * Function:	
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
static NhlErrorTypes MapWinToData
#if	NhlNeedProto
( NhlLayer instance,float* x,float* y,int n,float* xout,float* yout,float *xmissing, float *ymissing, int* status)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
	int *status;
#endif
{
        NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlMapTransObjLayerPart	*mtp = &(minstance->mptrans);
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	float xmin,ymin,xmax,ymax;


        xmin = MIN(mtp->ul,mtp->ur);
        xmax = MAX(mtp->ul,mtp->ur);
        ymin = MIN(mtp->ut,mtp->ub);
        ymax = MAX(mtp->ut,mtp->ub);


	*status = 0;
	for(i=0; i< n; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			||((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < xmin)
			||(x[i] > xmax)
			||(y[i] < ymin)
			||(y[i] > ymax)) {

			*status = 1;
			xout[i]=yout[i]=minstance->trobj.out_of_range;
	

		} else {
			c_maptri(x[i],y[i],&(yout[i]),&(xout[i]));
			if((xout[i] == 1e12) || (yout[i] == 1e12)) {
				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
			}
			else if (xout[i] < mtp->min_lon) 
				xout[i] += 360.0;
			else if (xout[i] > mtp->max_lon)
				xout[i] -= 360.0;
		}
	}
	return(ret);
}

/*
 * Function:	MapTransSetValues
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
static NhlErrorTypes  MapTransSetValues
#if	NhlNeedProto
(NhlLayer old,NhlLayer reference,NhlLayer new,_NhlArgList args,int num_args)
#else
(old,reference,new,args,num_args)
	NhlLayer	old;
	NhlLayer	reference;
	NhlLayer	new;
	_NhlArgList args;
	int	num_args;
#endif
{
	NhlMapTransObjLayer mnew = (NhlMapTransObjLayer) new;
	NhlMapTransObjLayer mold = (NhlMapTransObjLayer) old;
	NhlMapTransObjLayerPart	*mtp = &(mnew->mptrans);
	NhlMapTransObjLayerPart	*omtp = &(mold->mptrans);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text, *entry_name = "MapTransSetValues";
	
	mtp->trans_changed = True;
	mnew->trobj.change_count++;

/*
 * Ignore any attempts to set map pos directly
 */
	mtp->map_pos_l = omtp->map_pos_l;
	mtp->map_pos_r = omtp->map_pos_r;
	mtp->map_pos_b = omtp->map_pos_b;
	mtp->map_pos_t = omtp->map_pos_t;

	if (_NhlCmpFAny(mnew->trobj.out_of_range,1e12,6) != 0) {
		e_text = 
		"%s: %s must always equal 1e12 for map projections: resetting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNtrOutOfRangeF);
		mnew->trobj.out_of_range = 1e12;
		ret = MIN(ret,NhlWARNING);
	} 

	subret = CheckMapLimits(mnew,entry_name);
	ret = MIN(ret,subret);

	if (mtp->map_limit_mode == NhlNDC &&
	    (mtp->left_ndc != omtp->left_ndc ||
	     mtp->right_ndc != omtp->right_ndc ||
	     mtp->bottom_ndc != omtp->bottom_ndc ||
	     mtp->top_ndc != omtp->top_ndc) ) {	     

		/* convert to fraction of map projection area */

		float width  = mtp->map_pos_r - mtp->map_pos_l;
		float height = mtp->map_pos_t - mtp->map_pos_b;
		float fl = (mtp->left_ndc - mtp->map_pos_l) / width;
		float fr = (mtp->right_ndc - mtp->map_pos_l) / width;
		float fb = (mtp->bottom_ndc - mtp->map_pos_b) / height;
		float ft = (mtp->top_ndc - mtp->map_pos_b) / height;

		/* now convert to normalized projection coordinates 
		   (using old NPC values in case they were set)   */

		width = omtp->right_npc - omtp->left_npc;
		height = omtp->top_npc - omtp->bottom_npc;
		mtp->right_npc = MIN(1.0,mtp->left_npc + fr * width);
		mtp->left_npc = MAX(0.0,mtp->left_npc + fl * width);
		mtp->top_npc = MIN(1.0,mtp->bottom_npc + ft * height);
		mtp->bottom_npc = MAX(0.0,mtp->bottom_npc + fb * height);
	}

	subret = _NhlSetTrans(new,new->base.parent);
	ret = MIN(ret,subret);

	return(ret);
	
}

/*
 * Function:	MapTransInitialize
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
static NhlErrorTypes MapTransInitialize
#if	NhlNeedProto
( NhlClass class,NhlLayer req,NhlLayer new,_NhlArgList args,int num_args)
#else
(class,req,new,args,num_args)
	NhlClass class;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList args;
	int 	num_args;
#endif
{
	NhlMapTransObjLayer mnew = (NhlMapTransObjLayer) new;
	NhlMapTransObjLayerPart	*mtp = &(mnew->mptrans);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text, *entry_name = "MapTransInitialize";

	mnew->trobj.change_count++;
	if (_NhlCmpFAny(mnew->trobj.out_of_range,1e12,6) != 0) {
		e_text = 
		"%s: %s must always equal 1e12 for map projections: resetting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNtrOutOfRangeF);
		mnew->trobj.out_of_range = 1e12;
	} 
	
	subret = CheckMapLimits(mnew,entry_name);
	ret = MIN(ret,subret);

	subret = _NhlSetTrans(new,new->base.parent);
	ret = MIN(ret,subret);

	return(ret);
}

/*
 * Function:	GetWindowLimits
 *
 * Description: Given the lat/lon limits returns the window limits
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
static NhlErrorTypes GetWindowLimits
#if	NhlNeedProto
(
	NhlMapTransObjLayer	mnew,
	float			*wl,
	float			*wr,
	float			*wb,
	float			*wt,
	NhlString		entry_name
)
#else
(mnew,wl,wr,wb,wt,entry_name)
	NhlMapTransObjLayer	mnew;
	float			*wl;
	float			*wr;
	float			*wb;
	float			*wt;
	NhlString		entry_name;
#endif
{
	NhlMapTransObjLayerPart	*mtp = &(mnew->mptrans);
        float tlat,tlon,uval,vval;
        float umin=1E12,umax=-1E12,vmin=1E12,vmax=-1E12;
        float latmin=1E12,latmax=-1E12,lonmin=1E12,lonmax=-1E12;
	float fl,fr,fb,ft,ul,ur,ub,ut;
	int ll;

        *wl=*wr=*wb=*wt=0.0;
        c_mapset("MA",wl,wr,wb,wt);
        c_mapint();
        c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
#if 0
        printf("vp - %f,%f,%f,%f user - %f,%f,%f,%f\n",
               fl,fr,fb,ft,ul,ur,ub,ut);
#endif
        for (tlat = mtp->min_lat; tlat <= mtp->max_lat; tlat += 1.0) {
                for (tlon = mtp->min_lon; tlon <= mtp->max_lon; tlon += 1.0) {
                        c_maptra(tlat,tlon,&uval,&vval);
                        if (uval >= 1E9 || vval >= 1E9)
                                continue;
                        if (uval < umin) umin = uval;
                        if (uval > umax) umax = uval;
                        if (vval < vmin) vmin = vval;
			if (vval > vmax) vmax = vval;
                        if (tlat < latmin) latmin = tlat;
                        if (tlat > latmax) latmax = tlat;
                        if (tlon < lonmin) lonmin = tlon;
                        if (tlon > lonmax) lonmax = tlon;
                }
        }
        *wl = umin;
        *wr = umax;
        *wb = vmin;
        *wt = vmax;
#if 0
        printf("limits - %f,%f,%f,%f\n",umin,umax,vmin,vmax);
        printf("lat,lon min,max - %f,%f,%f,%f\n",latmin,latmax,lonmin,lonmax);
#endif

        return NhlNOERROR;

}
/*
 * Function:	CheckMapLimits
 *
 * Description: Performs some elementary checks on the map limit resources.
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
static NhlErrorTypes CheckMapLimits
#if	NhlNeedProto
(
	NhlMapTransObjLayer	mnew,
	NhlString		entry_name
)
#else
(mnew,entry_name)
	NhlMapTransObjLayer	mnew;
	NhlString		entry_name;
#endif
{
	NhlMapTransObjLayerPart	*mtp = &(mnew->mptrans);
	NhlErrorTypes ret = NhlNOERROR;
	char *e_text;
	float v_angle_lim, h_angle_lim;
	float ftmp;

	if (mtp->min_lon < -540.0 || mtp->min_lon > 540.0) {
		e_text ="%s: %s out of range: resetting";
		ret = MIN(ret,NhlWARNING);
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,NhlNmpMinLonF);
		mtp->min_lon = -180.0;
	}
	if (mtp->max_lon < -540.0 || mtp->max_lon > 540.0) {
		e_text ="%s: %s out of range: resetting";
		ret = MIN(ret,NhlWARNING);
		NhlPError(ret,NhlEUNKNOWN,e_text,entry_name,NhlNmpMaxLonF);
		mtp->max_lon = 180.0;
	}
 
	switch (mtp->projection) {
	case NhlORTHOGRAPHIC:
		h_angle_lim = v_angle_lim = 90;
		break;
	case NhlSTEREOGRAPHIC:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlLAMBERTEQUALAREA:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlGNOMONIC:
		h_angle_lim = v_angle_lim = 85;
		break;
	case NhlAZIMUTHALEQUIDISTANT:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMOLLWEIDE:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMERCATOR:
		h_angle_lim = 180;
		v_angle_lim = 85;
		break;
	case NhlCYLINDRICALEQUIDISTANT:
		h_angle_lim = 90;
		v_angle_lim = 180;
		break;
	case NhlLAMBERTCONFORMAL:
		if (mtp->map_limit_mode == NhlANGLES) {
			e_text = 
"%s: Angle map limit mode invalid for Lambert Conformal projection: resetting";
			ret = MIN(ret,NhlWARNING);
			NhlPError(ret,NhlEUNKNOWN,e_text,entry_name);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		break;
	case NhlSATELLITE:
		h_angle_lim = v_angle_lim = 90;
		break;
	default:
		e_text = "%s: internal enumeration error - projection";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}

	switch (mtp->map_limit_mode) {
	case NhlMAXIMALAREA:
		break;
	case NhlLATLON:
		ftmp = _NhlCmpFAny(mtp->min_lat,mtp->max_lat,6);
		if (ftmp == 0) {
			e_text = 
	         "%s: min/max latitude limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: latitude min exceeds max: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->min_lat;
			mtp->min_lat = mtp->max_lat;
			mtp->max_lat = ftmp;
		}
		ftmp = _NhlCmpFAny(mtp->min_lon,mtp->max_lon,6);
		if (ftmp == 0) {
			e_text = 
		"%s: min/max longitude limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: longitude min exceeds max: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->min_lon;
			mtp->min_lon = mtp->max_lon;
			mtp->max_lon = ftmp;
		}
		break;
	case NhlANGLES:
		if (fabs(mtp->left_angle) > h_angle_lim) {
			e_text = "%s: left angle out of range: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->left_angle = mtp->left_angle < 0.0 ?
				- h_angle_lim : h_angle_lim;
		}
		if (fabs(mtp->right_angle) > h_angle_lim) {
			e_text = "%s: left angle out of range: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->right_angle = mtp->right_angle < 0.0 ?
				- h_angle_lim : h_angle_lim;
		}
		if (_NhlCmpFAny(mtp->left_angle + 
				mtp->right_angle,0.0,6) == 0) {
			e_text = 
		 "%s: left/right angle limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		if (fabs(mtp->bottom_angle) > v_angle_lim) {
			e_text = "%s: left angle out of range: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->bottom_angle = mtp->bottom_angle < 0.0 ?
				- v_angle_lim : v_angle_lim;
		}
		if (fabs(mtp->top_angle) > v_angle_lim) {
			e_text = "%s: left angle out of range: resetting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->top_angle = mtp->top_angle < 0.0 ?
				- v_angle_lim : v_angle_lim;
		}
		if (_NhlCmpFAny(mtp->left_angle + 
				mtp->right_angle,0.0,6) == 0) {
			e_text = 
		 "%s: bottom/top angle limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		break;
	case NhlNPC:
		ftmp = _NhlCmpFAny(mtp->left_npc,
				   mtp->right_npc,6);
		if (ftmp == 0) {
			e_text = 
	         "%s: min/max latitude limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: latitude min exceeds max: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->left_npc;
			mtp->left_npc = mtp->right_npc;
			mtp->right_npc = ftmp;
		}
		ftmp = _NhlCmpFAny(mtp->bottom_npc,
				   mtp->top_npc,6);
		if (ftmp == 0) {
			e_text = 
		"%s: min/max longitude limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: longitude min exceeds max: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->bottom_npc;
			mtp->bottom_npc = mtp->top_npc;
			mtp->top_npc = ftmp;
		}
		break;
	case NhlNDC:
		ftmp = _NhlCmpFAny(mtp->left_ndc,
				   mtp->right_ndc,6);
		if (ftmp == 0) {
			e_text = 
	         "%s: min/max latitude limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: latitude min exceeds max: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->left_ndc;
			mtp->left_ndc = mtp->right_ndc;
			mtp->right_ndc = ftmp;
		}
		ftmp = _NhlCmpFAny(mtp->bottom_ndc,
				   mtp->top_ndc,6);
		if (ftmp == 0) {
			e_text = 
		"%s: min/max longitude limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: longitude min exceeds max: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->bottom_ndc;
			mtp->bottom_ndc = mtp->top_ndc;
			mtp->top_ndc = ftmp;
		}
		break;
	case NhlCORNERS:
	case NhlPOINTS:
		break;
	case NhlWINDOW:
		ftmp = _NhlCmpFAny(mtp->left_window,mtp->right_window,6);
		if (ftmp == 0) {
			e_text = 
	        "%s: left/right window limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: window left exceeds right: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->left_window;
			mtp->left_window = mtp->right_window;
			mtp->right_window = ftmp;
		}
		ftmp = _NhlCmpFAny(mtp->bottom_window,mtp->top_window,6);
		if (ftmp == 0) {
			e_text = 
	        "%s: bottom/top window limits result in zero area: defaulting";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			mtp->map_limit_mode = NhlMAXIMALAREA;
		}
		else if (ftmp > 0) {
			e_text = 
			"%s: window bottom exceeds top: exchanging values";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			ftmp = mtp->bottom_window;
			mtp->bottom_window = mtp->top_window;
			mtp->top_window = ftmp;
		}
		break;
	default:
		e_text = "%s: internal enumeration error - map limit mode";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}
	return(ret);
}

/*
 * Function:    MapTransClassInitialize
 *
 * Description: Just calls StringToQuark to register new types
 *
 * In Args:     NONE
 *
 * Out Args:    NONE
 *
 * Return Values:       Error condition
 *
 * Side Effects:        NONE
 */
static NhlErrorTypes    MapTransClassInitialize
#if	NhlNeedProto
(void)
#else
()
#endif
{
        _NhlEnumVals   limitmodelist[] = {
        {NhlMAXIMALAREA,	"maximalarea"},
        {NhlLATLON,		"latlon"},
        {NhlANGLES,		"angles"},
	{NhlNPC,		"npc"},
	{NhlNDC,		"ndc"},
        {NhlCORNERS,		"corners"},
        {NhlPOINTS,		"points"},
        {NhlWINDOW,		"window"},
        };

        _NhlEnumVals   projectionlist[] = {
        {NhlORTHOGRAPHIC,		"orthographic"},
        {NhlSTEREOGRAPHIC,		"stereographic"},
        {NhlLAMBERTEQUALAREA,		"lambertequalarea"},
        {NhlGNOMONIC,			"gnomonic"},
        {NhlAZIMUTHALEQUIDISTANT,	"azimuthalequidistant"},
        {NhlSATELLITE,      		"satellite"},
        {NhlMOLLWEIDE,			"mollweide"},
        {NhlMERCATOR,			"mercator"},
        {NhlCYLINDRICALEQUIDISTANT,	"cylindricalequidistant"},
        {NhlLAMBERTCONFORMAL,		"lambertconformal"}
        };

        _NhlRegisterEnumType(NhlmapTransObjClass,NhlTMapLimitMode,limitmodelist,
						NhlNumber(limitmodelist));
        _NhlRegisterEnumType(NhlmapTransObjClass,NhlTProjection,projectionlist,
						NhlNumber(projectionlist));

	return NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes MapDataLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance, x, y, upordown)
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
	if(upordown) {
		c_mapiq();
		c_mapit(y,x,0);
	} else {
		c_mapit(y,x,2);
	}
	return(NhlNOERROR);
}
static NhlErrorTypes MapWinLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance, x, y, upordown)
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
	NhlMapTransObjLayer minst = (NhlMapTransObjLayer)instance;
        static float lastx,lasty;
        static call_frstd = 1;
        float currentx,currenty;
        float holdx,holdy;
	float xmin,ymin,xmax,ymax;

	xmin = MIN(minst->mptrans.ul,minst->mptrans.ur);
	xmax = MAX(minst->mptrans.ul,minst->mptrans.ur);
	ymin = MIN(minst->mptrans.ut,minst->mptrans.ub);
	ymax = MAX(minst->mptrans.ut,minst->mptrans.ub);

/*
* if true the moveto is being performed
*/
        if(upordown) {
                lastx = x;
                lasty = y;
                call_frstd =1;
                return(NhlNOERROR);
        } else {
                currentx = x;
                currenty = y;
                holdx = lastx;
                holdy = lasty;
		_NhlTransClipLine(xmin,xmax,ymin,ymax,
			&lastx,
			&lasty,
			&currentx,
			&currenty,
			minst->trobj.out_of_range);
		if((lastx == minst->trobj.out_of_range)
                        ||(lasty == minst->trobj.out_of_range)
                        ||(currentx == minst->trobj.out_of_range)
                        ||(currenty == minst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
                        lastx = x;
                        lasty = y;
                        call_frstd = 1;
                        return(_NhlWorkstationLineTo(minst->trobj.wkptr,c_cufx(x),c_cufy(y),1));
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
                                _NhlWorkstationLineTo(minst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);
                                call_frstd = 2;
                        }
                        _NhlWorkstationLineTo(minst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);
                        lastx = x;
                        return(NhlNOERROR);
                }
        }
}
static NhlErrorTypes MapNDCLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown)
#else
(instance, x, y, upordown)
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
        NhlMapTransObjLayer	mpinst = (NhlMapTransObjLayer)instance;
	NhlTransObjLayerPart	*tp = &mpinst->trobj;
        static float lastx,lasty;
        static call_frstd = 1;
        float currentx,currenty;
        NhlErrorTypes ret = NhlNOERROR,ret1 = NhlNOERROR;
        float holdx,holdy;

/*
* if true the moveto is being performed
*/
        if(upordown) {
                lastx = x;
                lasty = y;
                call_frstd = 1;
                return(NhlNOERROR);
        } else {
                currentx = x;
                currenty = y;
                holdx = lastx;
                holdy = lasty;
                _NhlTransClipLine(tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
                        &lastx, &lasty, &currentx, &currenty,
                        mpinst->trobj.out_of_range);
                if((lastx == mpinst->trobj.out_of_range)
                        ||(lasty == mpinst->trobj.out_of_range)
                        ||(currentx == mpinst->trobj.out_of_range)
                        ||(currenty == mpinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
                        lastx  = x;
                        lasty  = y;
                        call_frstd = 1;
                        return(_NhlWorkstationLineTo(mpinst->trobj.wkptr,x,y,1));
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
                                ret1 = _NhlWorkstationLineTo(mpinst->trobj.wkptr,lastx,lasty,1);
                                call_frstd = 2;
			}
                        ret = _NhlWorkstationLineTo(mpinst->trobj.wkptr,currentx,currenty,0);
                        lastx = x;
                        lasty = y;
                        return(MIN(ret1,ret));
                }
        }
}


/*ARGSUSED*/
static NhlErrorTypes MapDataPolygon
#if	NhlNeedProto
(NhlLayer instance, float *x, float *y, int n )
#else
(instance, x, y, n )
NhlLayer instance;
float *x;
float *y;
int n;
#endif
{
	NhlString e_text;
	NhlString entry_name = "MapDataPolygon";

	e_text = "%s: not yet implemented";
	NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
	return NhlWARNING;
	
}

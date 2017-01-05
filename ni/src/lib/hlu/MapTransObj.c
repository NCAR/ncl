/*
*      $Id: MapTransObj.c,v 1.62 2009-01-22 21:04:07 dbrown Exp $
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
#include <float.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/MapTransObjP.h>
#include <ncarg/hlu/SphericalGeometryP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/WorkspaceI.h>
#include <ncarg/hlu/Transform.h>
#include <ncarg/hlu/color.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

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
{NhlNmpGreatCircleLinesOn,NhlCmpGreatCircleLinesOn,NhlTBoolean,
	 sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.great_circle_lines_on),
	 NhlTImmediate,_NhlUSET((NhlPointer)False) ,0,NULL},
{NhlNmpPolyMode,NhlCmpPolyMode,NhlTMapPolyMode,
	 sizeof(NhlMapPolyMode),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_poly_mode),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlAUTOPOLY),0,NULL},

{NhlNmpDataMinLonF,NhlCmpDataMinLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.data_xmin),
	 NhlTString,_NhlUSET("-180.0"),0,NULL},
{NhlNmpDataMaxLonF,NhlCmpDataMaxLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.data_xmax),
	 NhlTString,_NhlUSET("180.0"),0,NULL},

/* End-documented-resources */
        
{NhlNmpPreserveAspectRatio,NhlCmpPreserveAspectRatio,NhlTBoolean,
	 sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.preserve_aspect),
	 NhlTImmediate,_NhlUSET((NhlPointer)True) ,_NhlRES_PRIVATE,NULL},

{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
          NhlOffset(NhlMapTransObjLayerRec,trobj.x_min),
          NhlTString,_NhlUSET("-180."),_NhlRES_GONLY,NULL},
{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
          NhlOffset(NhlMapTransObjLayerRec,trobj.x_max),
          NhlTString,_NhlUSET("180.0"),_NhlRES_GONLY,NULL},
{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
          NhlOffset(NhlMapTransObjLayerRec,trobj.y_min),
          NhlTString,_NhlUSET("-90.0"),_NhlRES_GONLY,NULL},
{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
          NhlOffset(NhlMapTransObjLayerRec,trobj.y_max),
          NhlTString,_NhlUSET("90.0"),_NhlRES_GONLY,NULL},
        
{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
          NhlOffset(NhlMapTransObjLayerRec,trobj.x_reverse),
          NhlTImmediate,_NhlUSET(False),_NhlRES_PRIVATE,NULL},
{ "no.res","No.res",NhlTBoolean,sizeof(NhlBoolean),
          NhlOffset(NhlMapTransObjLayerRec,trobj.y_reverse),
          NhlTImmediate,_NhlUSET(False),_NhlRES_PRIVATE,NULL},

{NhlNmpTransChanged,NhlNmpTransChanged,NhlTBoolean,sizeof(NhlBoolean),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.trans_changed),
	 NhlTImmediate,_NhlUSET((NhlPointer) True),_NhlRES_PRIVATE,NULL},
{NhlNmpDumpPolygonAreaMap, NhlCmpDumpPolygonAreaMap,NhlTBoolean,
	 sizeof(NhlBoolean),NhlOffset(NhlMapTransObjLayerRec,
	 mptrans.dump_polygon_area_map),
	 NhlTImmediate,_NhlUSET((NhlPointer) False),_NhlRES_PRIVATE,NULL},

{NhlNtrOutOfRangeF, NhlCtrOutOfRangeF, NhlTFloat, sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,trobj.out_of_range),
		NhlTString, _NhlUSET("1.0e12"),_NhlRES_PRIVATE,NULL },

{NhlNtrLineInterpolationOn,NhlCtrLineInterpolationOn,
	NhlTBoolean,sizeof(NhlBoolean),
        NhlOffset(NhlTransObjLayerRec,trobj.line_interpolation_on),
	NhlTImmediate,
  	_NhlUSET((NhlPointer)True),0,NULL},

#if 0

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
#endif
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

static NhlErrorTypes    MapTransGetValues(
#if	NhlNeedProto
	NhlLayer,       /* l */
	_NhlArgList,    /* args */
	int             /* num_args */
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

static NhlErrorTypes StandardMapDataPolygon(
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

extern int (_NHLCALLF(hlumappolygon,HLUMAPPOLYGON))(
#if	NhlNeedProto
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
#endif
);

extern int (_NHLCALLF(mapiqd,MAPIQD))(
#if	NhlNeedProto
	void
#endif
);

extern int (_NHLCALLF(mapitd,MAPITD))(
#if	NhlNeedProto
	float	*y,
	float	*x,
	int	*ifst
#endif
);

NhlMapTransObjClassRec NhlmapTransObjClassRec = {
{
/* class_name			*/	"mapTransformationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlMapTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhltransObjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	MapTransClassInitialize,
/* layer_initialize		*/	MapTransInitialize,
/* layer_set_values		*/	MapTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	MapTransGetValues,
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
/* data_lineto		*/      MapDataLineTo,
/* compc_lineto		*/      MapDataLineTo,
/* win_lineto		*/      MapWinLineTo,
/* NDC_lineto		*/      MapNDCLineTo,
/* data_polygon		*/      MapDataPolygon 
},
{
/* aws_id		*/	-1
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
#define DEG2RAD 0.017453292519943 
#define mpDATAEPS 0.00036

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
	{ -1.0, 2.0, -1.0, 2.0 },
        { -1.0, 2.0, -0.5072, 2 * 0.5072 },
	{ -mpPI, 2.0 * mpPI, -4.0/3.0, 2.0 * (4.0/3.0) },
	{ 0.0,0.0,0.0,0.0 }, /* depends on rotation angle */
	{ -mpPI, 2.0*mpPI, -mpPI/2.0, mpPI }, 
	{ 0.0,0.0,0.0,0.0 }, /* needs square root of 2 */ 
	{ 0.0,0.0,0.0,0.0 }, /* needs square root of 2 */ 
	{ 0.0,0.0,0.0,0.0 }  /* requires cos,acos functions */

};

static NhlLayer Wkptr = NULL;
static NrmQuark	Qdataxstart = NrmNULLQUARK;
static NrmQuark	Qdataxend =  NrmNULLQUARK;
static NrmQuark	Qxmin =  NrmNULLQUARK;
static NrmQuark	Qxmax =  NrmNULLQUARK;
static NrmQuark	Qymin =  NrmNULLQUARK;
static NrmQuark	Qymax =  NrmNULLQUARK;

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
	float rl1[2] = {0,0},rl2[2] = {0,0},rl3[2] = { 0, 0},rl4[2] = {0, 0};
        NhlBoolean outside_viewspace = False;
        float pxl,pxr,pyb,pyt,pw,ph;
	double denom;

	ret =(*NhltransObjClassRec.trobj_class.set_trans)(instance,parent);

	c_mpsetr("OT",0.0);

	xl = minstance->trobj.x;
	yt = minstance->trobj.y;
	width = minstance->trobj.width;
	height = minstance->trobj.height;
        xr = xl + width;
        yb = yt - height;
        
        if (xl < 0.0 || xr > 1.0 || yb < 0.0 || yt > 1.0) {
                outside_viewspace = True;
        }
        
        if (! outside_viewspace) {
                c_mappos(xl,xr,yb,yt);
        }
        else {
		/* temporarily resize and reposition map so that it lies
		   completely within the viewspace */
		/* well it turns out that floating point error can sometimes
		 * turn 0.0 into small negative numbers somewhere in Ezmap
		 * so now I will resort to a slightly more difficult 
		 * calculation and a space inside the viewport.
		 */
                if (width > height) {
			pw = 0.9;
			ph = 0.9 * (height / width);
                }
                else {
			ph = 0.9;
                        pw = 0.9  * (width / height);
                }
                pxl = 0.05;
                pyb = 0.05;
		pxr = pxl + pw;
		pyt = pyb + ph;
		
                c_mappos(pxl,pxr,pyb,pyt);
                
        }
        
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
	case NhlPSEUDOMOLLWEIDE:
		cproj = "MT";
		h_angle_lim = 180;
		v_angle_lim = 90;
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
		h_angle_lim = 180;
		v_angle_lim = 90;
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
	case NhlROBINSON:
		cproj = "RO";
		h_angle_lim = 180;
		v_angle_lim = 90;
		ix = 10;
		break;
	case NhlCYLINDRICALEQUALAREA:
		cproj = "EA";
		h_angle_lim = 180;
		v_angle_lim = 90;
		ix = 11;
		break;
	case NhlROTATEDMERCATOR:
		cproj = "RM";
		h_angle_lim = 180;
		v_angle_lim = 85;
		ix = 12;
		denom = fabs(sin(mtp->center_rot * DEG2RAD)) 
			+ fabs(cos(mtp->center_rot * DEG2RAD));
		Win_Limits[ix].u_min = -mpPI / denom;
		Win_Limits[ix].u_range = 2.0 * mpPI / denom;
		Win_Limits[ix].v_min = -mpPI / denom;
		Win_Limits[ix].v_range = 2.0 * mpPI / denom;
		break;
	case NhlAITOFF:
		cproj = "AI";
		h_angle_lim = 180;
		v_angle_lim = 90;
		ix = 13;
		break;
	case NhlHAMMER:
		cproj = "HA";
		h_angle_lim = 180;
		v_angle_lim = 90;
		ix = 14;
		Win_Limits[ix].u_min = -2.0 * sqrt(2.0);
		Win_Limits[ix].u_range = 4.0 * sqrt(2.0);
		Win_Limits[ix].v_min = -sqrt(2.0);
		Win_Limits[ix].v_range = 2.0 * sqrt(2.0);
		break;
	case NhlMOLLWEIDE:
		cproj = "MO";
		h_angle_lim = 180;
		v_angle_lim = 90;
		ix = 15;
		Win_Limits[ix].u_min = -2.0 * sqrt(2.0);
		Win_Limits[ix].u_range = 4.0 * sqrt(2.0);
		Win_Limits[ix].v_min = -sqrt(2.0);
		Win_Limits[ix].v_range = 2.0 * sqrt(2.0);
		break;
	case NhlWINKELTRIPEL:
		cproj = "WT";
		h_angle_lim = 180;
		v_angle_lim = 90;
		ix = 16;
		Win_Limits[ix].u_min = -mpPI / 2.0 * (1 + 2.0 / mpPI);
		Win_Limits[ix].u_range = mpPI * (1 + 2.0 / mpPI);
		Win_Limits[ix].v_min = -mpPI / 2.0;
		Win_Limits[ix].v_range = mpPI;
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
                    /* this works whether or not the map is entirely within
                       the viewspace */ 
                mtp->map_pos_l = xl;
                mtp->map_pos_r = xr;
                mtp->map_pos_b = yb;
                mtp->map_pos_t = yt;
                _NhlTransLLUSet(mtp->map_pos_l,mtp->map_pos_r,
                                mtp->map_pos_b,mtp->map_pos_t,
                                mtp->ul,mtp->ur,mtp->ub,mtp->ut,
                                loglin,&minstance->trobj.off_screen,
                                entry_name);
        }
        else if (outside_viewspace) {
                float npl,npr,npb,npt;

                    /* reposition to the real location */
                npl = xl + width * ((mtp->map_pos_l - pxl)/pw);
                npr = xl + width * ((mtp->map_pos_r - pxl)/pw);
                npb = yb + height * ((mtp->map_pos_b - pyb)/ph);
                npt = yb + height * ((mtp->map_pos_t - pyb)/ph);
                mtp->map_pos_l = npl;
                mtp->map_pos_r = npr;
                mtp->map_pos_b = npb;
                mtp->map_pos_t = npt;
                _NhlTransLLUSet(mtp->map_pos_l,mtp->map_pos_r,
                                mtp->map_pos_b,mtp->map_pos_t,
                                mtp->ul,mtp->ur,mtp->ub,mtp->ut,
                                loglin,&minstance->trobj.off_screen,
                                entry_name);
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
        mtp->data_xmin = MIN(minstance->trobj.data_xstart,
                             minstance->trobj.data_xend);
        mtp->data_xmax = MAX(minstance->trobj.data_xstart,
                             minstance->trobj.data_xend);

	mtp->trans_changed = False;

	return (ret);

}

/*
 * Function:	win_compare_check
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
static NhlBoolean win_compare_check
#if	NhlNeedProto
(
	NhlMapTransObjLayerPart *llp,
	float	*x,
 	float	*y,
	float   xmin,
	float   xmax,
	float   ymin,
	float   ymax
)
#else
(mtp,x,y,xmin,xmax,ymin,ymax)
	NhlMapTransObjLayerPart *mtp;
	float	*x;
	float	*y;
	float   xmin;
	float   xmax;
	float   ymin;
	float   ymax;
#endif
{
        int xmndif,xmxdif,ymndif,ymxdif;

	if ((xmndif = _NhlCmpFAny2(*x,xmin,5,0.0001)) < 0 ||
	    (xmxdif = _NhlCmpFAny2(*x,xmax,5,0.0001)) > 0 ||
	    (ymndif = _NhlCmpFAny2(*y,ymin,5,0.0001)) < 0 ||
	    (ymxdif = _NhlCmpFAny2(*y,ymax,5,0.0001)) > 0) {
		return False;
	}
	return True;
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
			
			if (! win_compare_check(mtp,&x[i],&y[i],
						xmin,xmax,ymin,ymax)) {
				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
				continue;
			}
			strans(mtp->ul,mtp->ur,
			       mtp->ub,mtp->ut,
			       mtp->map_pos_l,
			       mtp->map_pos_r,
			       mtp->map_pos_b,
			       mtp->map_pos_t,
			       x[i],y[i],
			       &(xout[i]),&(yout[i]));
			
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

	*status = 0;
	for( i = 0; i< n; i++) {
		if(((xmissing != NULL) &&(*xmissing == x[i]))
			||((ymissing != NULL) &&(*ymissing == y[i]))) {	
			*status = 1;
			xout[i] = yout[i] = minstance->trobj.out_of_range;
		} else {

/*			c_maptra(y[i],x[i],&tmpx,&tmpy);
 */

			xout[i] = x[i];
			yout[i] = y[i];

/*
* A problem could develop here if 1e12 is not represented identically in
* FORTRAN and C because of arithmetic error

			if(tmpx == minstance->trobj.out_of_range) {
				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
			}
*/
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
			if (xout[i] == minstance->trobj.out_of_range) {
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
			if (yout[i] == minstance->trobj.out_of_range) {
				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
			}
			else if (xout[i] < mtp->data_xmin) {
				if (xout[i] + 360 <= mtp->data_xmax + mpDATAEPS) {
					xout[i] += 360.0;
				}
			}
			else if (xout[i] > mtp->data_xmax) {
				if (xout[i] - 360.0 >= mtp->data_xmin - mpDATAEPS) {
					xout[i] -= 360.0;
				}
			}
		}
	}
	return(ret);
}

/*
 * Function:	GetMinMaxLatLon
 *
 * Description: Returns the min and max lat and lon given the current
 *              map limits and projection
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
static NhlErrorTypes GetMinMaxLatLon
#if	NhlNeedProto
(
	NhlMapTransObjLayer	mpl,
	float			*minlon,
	float			*maxlon,
	float			*minlat,
	float			*maxlat
)
#else
(mpl,minlon,maxlon,minlat,maxlat)
	NhlMapTransObjLayer	mpl;
	float			*minlon;
	float			*maxlon;
	float			*minlat;
	float			*maxlat
#endif
{
	NhlMapTransObjLayerPart	*mtp = &(mpl->mptrans);
	float xloc[2],yloc[2],xout[2],yout[2],xt[2],yt[2];
	int proj;
	float clon,crot,clat;
	NhlBoolean rel_lon,rel_lat;
	NhlBoolean northpole = False, southpole = False;
	float xdist,ydist,xinc,yinc;
	float xmin = 999,ymin = 999,xmax = -999,ymax = -999;
	int status;
	float cycle_point;
	float lat,lon,uval,vval;
	int i,j, nsteps;
	float ymin_u,ymin_v,ymax_u,ymax_v;
	float xmin_u,xmin_v,xmax_u,xmax_v;
	float xinc2,yinc2,xstart,ystart;
	*minlon = -180; 
	*maxlon = 180;
	*minlat = -90;
	*maxlat = 90;
	
	proj = mtp->projection;
	rel_lon = mtp->rel_center_lon;
	clon = proj ==  NhlLAMBERTCONFORMAL ? 
		mtp->lambert_meridian : mtp->center_lon;
	if (rel_lon && mtp->map_limit_mode == NhlLATLON)
		clon = (mtp->max_lon + mtp->min_lon) / 2.0 + clon;

	crot = mtp->center_rot;

	rel_lat = mtp->rel_center_lat;
	clat = mtp->center_lat;
	if (rel_lat && mtp->map_limit_mode == NhlLATLON)
		clat = (mtp->max_lat + mtp->min_lat) / 2.0 + clat;

	xloc[0] = mtp->left_window;
	yloc[0] = mtp->bottom_window;
	xloc[1] = mtp->right_window;
	yloc[1] = mtp->top_window;
	cycle_point = clon - 180;

	/* if it's a straight cyleq or mercator, and the data boundary 
	   line is not crossed then the data locs need no further processing */

	if ((proj == NhlCYLINDRICALEQUIDISTANT || 
	     proj == NhlMERCATOR) && clat == 0.0 && crot == 0.0) {
		c_maptri(xloc[0],yloc[0],&(yout[0]),&(xout[0]));
		c_maptri(xloc[1],yloc[1],&(yout[1]),&(xout[1]));

		if (xout[0] > 1e10 || xout[1] > 1e10) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
	       "GetMinMaxLatLon: unable to determine min/max lat/lon values");
			return NhlWARNING;;
		}
		/* no problem */
		if (_NhlCmpFAny2(xout[0],xout[1],6,1e-10)>= 0.0)
			xout[0] -= 360;
		while (xout[0] < cycle_point) {
			xout[0] += 360;
			xout[1] += 360;
		}
		while (xout[0] > cycle_point + 360) {
			xout[0] -= 360;
			xout[1] -= 360;
		}
		*minlon = xout[0];
		*minlat = yout[0];
		*maxlon = xout[1];
		*maxlat = yout[1];
		return NhlNOERROR;
	}

	/*
	 * see if the north or south pole is inside the area
	 */
	xt[0] = xt[1] = clon;
	yt[0] = -90;
	yt[1] = 90;

	
	MapDataToWin((NhlLayer) mpl,xt,yt,2,xt,yt,NULL,NULL,&status);

	if (yt[0] >= yloc[0] && yt[0] <= yloc[1] &&
	    xt[0] >= xloc[0] && xt[0] <= xloc[1])
		southpole = True;
	if (yt[1] >= yloc[0] && yt[1] <= yloc[1] &&
	    xt[1] >= xloc[0] && xt[1] <= xloc[1])
		northpole = True;

	/*
	 * sample points along each edge and determine the min and max
	 * data values
	 */

	xdist = xloc[1] - xloc[0];
	ydist = yloc[1] - yloc[0];
	nsteps = 100;
	xinc = xdist / nsteps;
	yinc = ydist / nsteps;
	for (i = 0; i <= nsteps; i++) {
		for (j = 0; j <= nsteps; j++) {
			uval = xloc[0] + xinc * j;
			vval = yloc[0] + yinc * i;
			c_maptri(uval,vval,&lat,&lon);
			if (lon > 1e9) 
				continue;
			while (lon < cycle_point)
				lon += 360;
			while (lon > cycle_point + 360)
				lon -= 360;
			if (xmin > lon) {
				xmin = lon;
				xmin_u = uval;
				xmin_v = vval;
			}
			if (xmax < lon) {
				xmax = lon;
				xmax_u = uval;
				xmax_v = vval;
			}
			if (ymin > lat) {
				ymin = lat;
				ymin_u = uval;
				ymin_v = vval;
			}
			if (ymax < lat) {
				ymax = lat;
				ymax_u = uval;
				ymax_v = vval;
			}
		}
	}
	xstart = MAX(xloc[0],xmin_u - xinc);
	ystart = MAX(yloc[0],xmin_v - yinc);
	xdist = MIN(xloc[1],xmin_u + xinc) - xstart;
	ydist = MIN(yloc[1],xmin_v + yinc) - ystart;
	nsteps = 20;
	xinc2 = xdist / nsteps;
	yinc2 = ydist / nsteps;

	for (i = 1; i < nsteps; i++) {
		for (j = 1; j < nsteps; j++) {
			uval = xstart + xinc2 * j;
			vval = ystart + yinc2 * i;
			c_maptri(uval,vval,&lat,&lon);
			if (lon > 1e9) 
				continue;
			while (lon < cycle_point)
				lon += 360;
			while (lon > cycle_point + 360)
				lon -= 360;
			if (xmin > lon) {
				xmin = lon;
			}
		}
	}
	xstart = MAX(xloc[0],xmax_u - xinc);
	ystart = MAX(yloc[0],xmax_v - yinc);
	xdist = MIN(xloc[1],xmax_u + xinc) - xstart;
	ydist = MIN(yloc[1],xmax_v + yinc) - ystart;
	xinc2 = xdist / nsteps;
	yinc2 = ydist / nsteps;
	for (i = 1; i < nsteps; i++) {
		for (j = 1; j < nsteps; j++) {
			uval = xstart + xinc2 * j;
			vval = ystart + yinc2 * i;
			c_maptri(uval,vval,&lat,&lon);
			if (lon > 1e9) 
				continue;
			while (lon < cycle_point)
				lon += 360;
			while (lon > cycle_point + 360)
				lon -= 360;
			if (xmax < lon) {
				xmax = lon;
			}
		}
	}
	if (northpole || southpole) {
		if (_NhlCmpFAny2(xmin,cycle_point,3,1e-6) == 0.0)
			xmin = cycle_point;
		if (_NhlCmpFAny2(xmax,cycle_point+360,3,1e-6) == 0.0)
			xmax = cycle_point+360;
	}
	if (_NhlCmpFAny2(xmin,0.0,6,1e-6) == 0.0)
		xmin = 0.0;
	if (_NhlCmpFAny2(xmax,0.0,6,1e-6) == 0.0)
		xmax = 0.0;
	*minlon = xmin;
	*maxlon = xmax;


	if (southpole) 
		*minlat = -90;
	else {
		xstart = MAX(xloc[0],ymin_u - xinc);
		ystart = MAX(yloc[0],ymin_v - yinc);
		xdist = MIN(xloc[1],ymin_u + xinc) - xstart;
		ydist = MIN(yloc[1],ymin_v + yinc) - ystart;
		xinc2 = xdist / nsteps;
		yinc2 = ydist / nsteps;
		for (i = 1; i < nsteps; i++) {
			for (j = 1; j < nsteps; j++) {
				uval = xstart + xinc2 * j;
				vval = ystart + yinc2 * i;
				c_maptri(uval,vval,&lat,&lon);
				if (lat > 1e9) 
					continue;
				if (ymin > lat) {
					ymin = lat;
				}
			}
		}
		if (_NhlCmpFAny2(ymin,0.0,6,1e-6) == 0.0)
			ymin = 0.0;
		*minlat = ymin;
	}
	if (northpole) {
		*maxlat = 90;
	}
	else {
		xstart = MAX(xloc[0],ymax_u - xinc);
		ystart = MAX(yloc[0],ymax_v - yinc);
		xdist = MIN(xloc[1],ymax_u + xinc) - xstart;
		ydist = MIN(yloc[1],ymax_v + yinc) - ystart;
		xinc2 = xdist / nsteps;
		yinc2 = ydist / nsteps;
		for (i = 1; i < nsteps; i++) {
			for (j = 1; j < nsteps; j++) {
				uval = xstart + xinc2 * j;
				vval = ystart + yinc2 * i;
				c_maptri(uval,vval,&lat,&lon);
				if (lat > 1e9) 
					continue;
				if (ymax < lat) {
					ymax = lat;
				}
			}
		}
		if (_NhlCmpFAny2(ymax,0.0,6,1e-6) == 0.0)
			ymax = 0.0;
		*maxlat = ymax;
	}
			    
        return NhlNOERROR;
	
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
	int data_arg_count = 0;

	if (num_args == 2 &&
	    (args[0].quark == Qdataxstart || args[0].quark == Qdataxend)) {
		mtp->data_xmin = MIN(mnew->trobj.data_xstart,
				     mnew->trobj.data_xend);
		mtp->data_xmax = MAX(mnew->trobj.data_xstart,
				     mnew->trobj.data_xend);
		return NhlNOERROR;
        }
	if (_NhlArgIsSet(args,num_args,NhlNmpDataMinLonF))
		data_arg_count++;
	if (_NhlArgIsSet(args,num_args,NhlNmpDataMaxLonF))
		data_arg_count++;
	if (data_arg_count) {
		if (mtp->data_xmin > mtp->data_xmax) {
			float tmp;
			char *e_text = 
		   "%s: mpDataMinLonF greater than  mpDataMaxLonF: exchanging";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			tmp = mtp->data_xmin;
			mtp->data_xmin = mtp->data_xmax;
			mtp->data_xmin = tmp;
		}
		if (mtp->data_xmin == mtp->data_xmax ||
		    mtp->data_xmin < -540 || mtp->data_xmax > 540) {
			char *e_text = 
   "%s: mpDataMinLonF and/or mpDataMaxLonF out of range: resetting to default";
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			mtp->data_xmin = -180;
			mtp->data_xmax = 180;
		}
		mnew->trobj.data_xstart = mtp->data_xmin;
		mnew->trobj.data_xend = mtp->data_xmax;
	}
	if (num_args == data_arg_count)
		return NhlNOERROR;
			 
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

	/*
	 * if switching from NhlLATLON mode, convert relative center lat/lon
	 * values to normal center lat/lon values and turn off the relative
	 * center switches. This implies that you must always set the relative
	 * switches on explicitly when you go to NhlLATLON mode, but it seems
	 * necessary to ensure consistent operation.
	 */
	if (omtp->map_limit_mode == NhlLATLON && 
	    mtp->map_limit_mode != NhlLATLON) {
		if (mtp->rel_center_lon) {
			if (! _NhlArgIsSet(args,num_args,NhlNmpCenterLonF))
				mtp->center_lon += 
					(mtp->max_lon + mtp->min_lon) / 2.0;
			mtp->rel_center_lon = False;
		}
		if (mtp->rel_center_lat) {
			if (! _NhlArgIsSet(args,num_args,NhlNmpCenterLatF))
				mtp->center_lat += 
					(mtp->max_lat + mtp->min_lat) / 2.0;
			mtp->rel_center_lat = False;
		}
	}
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
 * Function:    MapTransGetValues
 *
 * Description: 
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


static NhlErrorTypes    MapTransGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
        NhlLayer        l;
        _NhlArgList     args;
        int     	num_args;
#endif
{
        NhlMapTransObjLayer mtl = (NhlMapTransObjLayer)l;
        NhlBoolean gotminmax = False;
        float minlat,maxlat,minlon,maxlon;
        int i;
        
        for( i = 0; i< num_args; i++ ) {
                if (! gotminmax && (args[i].quark == Qxmin ||
                                    args[i].quark == Qxmax ||
                                    args[i].quark == Qymin ||
                                    args[i].quark == Qymax)) {
                        GetMinMaxLatLon(mtl,
                                        &minlon,&maxlon,
                                        &minlat,&maxlat);
                        gotminmax = True;
                }
                if (args[i].quark == Qxmin) {
                        *((float *)(args[i].value.ptrval)) = minlon;
                }
                else if (args[i].quark == Qxmax) {
                        *((float *)(args[i].value.ptrval)) = maxlon;
                }
                if (args[i].quark == Qymin) {
                        *((float *)(args[i].value.ptrval)) = minlat;
                }
                else if (args[i].quark == Qymax) {
                        *((float *)(args[i].value.ptrval)) = maxlat;
                }
        }
        return NhlNOERROR;
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
        float lonmin,lonmax,lonmax2;
        float latinc,loninc;
	float clon;
	NhlBoolean two_step = False;
	NhlBoolean lon_done,lat_done;

        *wl=*wr=*wb=*wt=0.0;
        c_mapset("MA",wl,wr,wb,wt);
        c_mapint();
#if 0
        c_getset(&fl,&fr,&fb,&ft,&ul,&ur,&ub,&ut,&ll);
        printf("vp - %f,%f,%f,%f user - %f,%f,%f,%f\n",
               fl,fr,fb,ft,ul,ur,ub,ut);
#endif
	clon = mtp->projection ==  NhlLAMBERTCONFORMAL ? 
		mtp->lambert_meridian : mtp->center_lon;
	if (mtp->rel_center_lon && mtp->map_limit_mode == NhlLATLON)
		clon = (mtp->max_lon + mtp->min_lon) / 2.0 + clon;
	
        latinc = MIN((mtp->max_lat - mtp->min_lat) / 91.0,1.0);
        loninc = MIN((mtp->max_lon - mtp->min_lon) / 91.0,1.0);
	lonmin = mtp->min_lon;
	lonmax = mtp->max_lon;

	while (lonmax > lonmin + 360)
		lonmax -=360;

	while (lonmin < clon - 180) {
		lonmin += 360;
		lonmax += 360;
	}
	while (lonmin > clon + 180) {
		lonmin -= 360;
		lonmax -= 360;
	}
	if (lonmax > clon + 180) {
		two_step = True;
		lonmax2 = lonmax - 360;
		lonmax = clon + 180;
	}
        
	lat_done = False;
        for (tlat = mtp->min_lat; ; tlat += latinc) {
		if (tlat > mtp->max_lat) {
			tlat = mtp->max_lat;
			lat_done = True;
		}
		lon_done = False;
                for (tlon = lonmin; ; tlon += loninc) {
			if (tlon > lonmax) {
				tlon = lonmax;
				lon_done = True;
			}
                        c_maptra(tlat,tlon,&uval,&vval);
                        if (uval > 1E9) {
				if (lon_done)
					break;
                                continue;
			}
                        if (uval < umin) umin = uval;
                        if (uval > umax) umax = uval;
                        if (vval < vmin) vmin = vval;
			if (vval > vmax) vmax = vval;
			if (lon_done)
				break;
                }
		lon_done = False;
		if (two_step) {
			for (tlon = clon - 180; ; tlon += loninc) {
				if (tlon > lonmax2) {
					tlon = lonmax2;
					lon_done = True;
				}
				c_maptra(tlat,tlon,&uval,&vval);
				if (uval > 1E9) {
					if (lon_done)
						break;
					continue;
				}
				if (uval < umin) umin = uval;
				if (uval > umax) umax = uval;
				if (vval < vmin) vmin = vval;
				if (vval > vmax) vmax = vval;
				if (lon_done)
					break;
			}
		}
		if (lat_done)
			break;
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
	case NhlPSEUDOMOLLWEIDE:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMERCATOR:
		h_angle_lim = 180;
		v_angle_lim = 85;
		break;
	case NhlCYLINDRICALEQUIDISTANT:
		h_angle_lim = 180;
		v_angle_lim = 90;
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
	case NhlROBINSON:
		h_angle_lim = 180;
		v_angle_lim = 90;
		break;
	case NhlCYLINDRICALEQUALAREA:
		h_angle_lim = 180;
		v_angle_lim = 90;
		break;
	case NhlROTATEDMERCATOR:
		h_angle_lim = 180;
		v_angle_lim = 85;
		break;
	case NhlAITOFF:
	case NhlHAMMER:
	case NhlMOLLWEIDE:
	case NhlWINKELTRIPEL:
		h_angle_lim = 180;
		v_angle_lim = 90;
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
        {NhlMAXIMALAREA,	"MaximalArea"},
        {NhlLATLON,		"LatLon"},
        {NhlANGLES,		"Angles"},
	{NhlNPC,		"NPC"},
	{NhlNDC,		"NDC"},
        {NhlCORNERS,		"Corners"},
        {NhlPOINTS,		"Points"},
        {NhlWINDOW,		"Window"},
        };

        _NhlEnumVals   projectionlist[] = {
        {NhlORTHOGRAPHIC,		"Orthographic"},
        {NhlSTEREOGRAPHIC,		"Stereographic"},
        {NhlLAMBERTEQUALAREA,		"LambertEqualArea"},
        {NhlGNOMONIC,			"Gnomonic"},
        {NhlAZIMUTHALEQUIDISTANT,	"AzimuthalEquidistant"},
        {NhlSATELLITE,      		"Satellite"},
        {NhlPSEUDOMOLLWEIDE,		"PseudoMollweide"},
        {NhlMERCATOR,			"Mercator"},
        {NhlCYLINDRICALEQUIDISTANT,	"CylindricalEquidistant"},
        {NhlLAMBERTCONFORMAL,		"LambertConformal"},
	{NhlROBINSON,			"Robinson"},
	{NhlCYLINDRICALEQUALAREA,       "CylindricalEqualArea"},
	{NhlROTATEDMERCATOR,            "RotatedMercator"},
	{NhlAITOFF,            		"Aitoff"},
	{NhlHAMMER,            		"Hammer"},
	{NhlMOLLWEIDE,                  "Mollweide"},
	{NhlWINKELTRIPEL,               "WinkelTripel"},
        };

        _NhlEnumVals   polymodelist[] = {
	{NhlAUTOPOLY,		"AutoPoly"},
        {NhlFASTPOLY,		"FastPoly"},
        {NhlSTANDARDPOLY,	"StandardPoly"}
        };

        _NhlRegisterEnumType(NhlmapTransObjClass,NhlTMapLimitMode,limitmodelist,
						NhlNumber(limitmodelist));
        _NhlRegisterEnumType(NhlmapTransObjClass,NhlTProjection,projectionlist,
						NhlNumber(projectionlist));
        _NhlRegisterEnumType(NhlmapTransObjClass,NhlTMapPolyMode,polymodelist,
						NhlNumber(polymodelist));

	Qdataxstart = NrmStringToQuark(NhlNtrDataXStartF);
	Qdataxend = NrmStringToQuark(NhlNtrDataXEndF);
 	Qxmin = NrmStringToQuark(NhlNtrXMinF);
	Qxmax = NrmStringToQuark(NhlNtrXMaxF);
 	Qymin = NrmStringToQuark(NhlNtrYMinF);
	Qymax = NrmStringToQuark(NhlNtrYMaxF);

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
	
	NhlMapTransObjLayer	mpl = (NhlMapTransObjLayer) instance;
	static float x_last,y_last,xw_last,yw_last,usize,vsize,uv_cut;
	float *xbuf, *ybuf;
	float xdist, ydist,xw,yw,uv_dist;
	int npoints;
	int i;
#if 0
	if (save_change_count != mpl->trobj.change_count) {
		GetMinMaxLatLon(mpl,
				&minlon,&maxlon,&minlat,&maxlat,entry_name);
		dlon = maxlon - minlon;
		dlat = maxlat - minlat;
		save_change_count = mpl->trobj.change_count;
	}
#endif
	if(upordown) {
		c_mapiqd();
		c_mapitd(y,x,0);
		x_last = x;
		y_last = y;
		c_maptrn(y,x,&xw_last,&yw_last);
		usize = mpl->mptrans.ur - mpl->mptrans.ul;
		vsize = mpl->mptrans.ut - mpl->mptrans.ub;
		uv_cut = sqrt(usize * usize + vsize * vsize) / 200;
		return NhlNOERROR;
	}
	xdist = x - x_last;
	if (xdist > 180.0)
		x -= 360.0;
	else if (xdist < -180)
		x += 360.0;
       
	xdist = x - x_last;			
	ydist = y - y_last;
	c_maptrn(y,x,&xw,&yw);
	
	if (! mpl->trobj.line_interpolation_on) {
		c_mapitd(y,x,2);
		x_last = fmod(x + 180, 360) - 180;
		y_last = y;
		xw_last = xw;
		yw_last = yw;
		return(NhlNOERROR);
	}

	uv_dist = sqrt((xw_last - xw) *(xw_last - xw) + (yw_last - yw) * (yw_last - yw));

	if (xw_last > 1e10 || xw > 1e10) 
		npoints = MAX(1.0,(int)(fabs(xdist) + fabs(ydist)));
	else 
		npoints = MAX(1.0,(int) (uv_dist / uv_cut));
	
	if (mpl->mptrans.great_circle_lines_on && npoints > 1) {
		xbuf = NhlMalloc(npoints*sizeof(float));
		ybuf = NhlMalloc(npoints*sizeof(float));
		c_mapgci(y_last,x_last,y,x,npoints,ybuf,xbuf);
		for (i = 0; i < npoints; i++)
			c_mapitd(ybuf[i],xbuf[i],1);
		c_mapitd(y,x,2);
		x_last = fmod(x + 180, 360) - 180;
		y_last = y;
		xw_last = xw;
		yw_last = yw;
		NhlFree(xbuf);
		NhlFree(ybuf);
		return NhlNOERROR;
	}

	for (i = 0; i < npoints-1; i++) {
		float yc = y_last + ydist *(i+1)/ (float)npoints;
		float xc = x_last + xdist *(i+1)/ (float)npoints;
		c_mapitd(yc,xc,1);
	}
	c_mapitd(y,x,2);
	x_last = fmod(x + 180, 360) - 180;
	y_last = y;
	xw_last = xw;
	yw_last = yw;
	
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
        static int call_frstd = 1;
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
        static int call_frstd = 1;
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
static NhlBoolean Clockwise
#if	NhlNeedProto
(float *x, float *y, int n, NhlBoolean closed,NhlBoolean *out_of_range)
#else
(x, y, n, closed, out_of_range)
float *x;
float *y;
int n;
NhlBoolean closed;
NhlBoolean *out_of_range;
#endif
{
	int i;
	NhlBoolean clockwise = False;
	double area = 0.0;
	double lat0,lon0,lat1,lon1;
	double x0,y0,x1,y1;
	double min_lon,max_lon;
	
/*
 * From comp.graphics.algorithms FAQ: orientation is clockwise if 
 * the signed area is less than 0.0; counter-clockwise otherwise.
 * (adapted for spherical surface - project to flat surface first)
 */
	min_lon = max_lon = x[0];
	*out_of_range = False;
	if (y[0] > 90.0) {
		lat0 = 90.0;
		*out_of_range = True;
	}
	else if (y[0] < -90.0) {
		lat0 = -90.0;
		*out_of_range = True;
	}
	else {
		lat0 = y[0];
	}
	if (x[0] > 540.0) {
		lon0 = 540.0;
		*out_of_range = True;
	}
	else if (x[0] < -540.0) {
		lon0 = -540.0;
		*out_of_range = True;
	}
	else {
		lon0 = x[0];
	}
	x0 = 0.0;
        y0 = 0.0;
	for (i=0; i < n; i++) {
		int nexti = i + 1;
		if (i == n - 1) {
			if (closed)
				break;
			else
				nexti = 0;
		}
		if (y[nexti] > 90.0) {
			lat1 = 90.0;
			*out_of_range = True;
		}
		else if (y[nexti] < -90.0) {
			lat1 = -90.0;
			*out_of_range = True;
		}
		else {
			lat1 = y[nexti];
		}
		if (x[nexti] > 540.0) {
			lon1 = 540.0;
			*out_of_range = True;
		}
		else if (x[nexti] < -540.0) {
			lon1 = -540.0;
			*out_of_range = True;
		}
		else {
			lon1 = x[nexti];
		}
		if (lon1 > max_lon) max_lon = lon1;
		if (lon1 < min_lon) min_lon = lon1;
		NGCALLF(hlugproj,HLUGPROJ)(&lat0,&lon0,&lat1,&lon1,&x1,&y1);
		area += x0 * y1 - x1 * y0;
		x0 = x1;
		y0 = y1;
	}
	if (max_lon - min_lon < 180) {
		clockwise = area < 0.0;
		return clockwise;
	}
	/* can't use gproj -- so just try pretending its not spherical */
	area = 0.0;
        for (i=0; i < n; i++) {
                int nexti = i + 1;
                if (i == n - 1) {
                        if (closed)
                                break;
                        else
                                nexti = 0;
                }
                if (y[nexti] > 90.0) {
                        lat1 = 90.0;
                        *out_of_range = True;
                }
                else if (y[nexti] < -90.0) {
                        lat1 = -90.0;
                        *out_of_range = True;
                }
                else {
                        lat1 = y[nexti];
                }
                if (x[nexti] > 540.0) {
                        lon1 = 540.0;
                        *out_of_range = True;
                }
                else if (x[nexti] < -540.0) {
                        lon1 = -540.0;
                        *out_of_range = True;
                }
                else {
                        lon1 = x[nexti];
                }
                area += lon0 * lat1 - lon1 * lat0;
                lon0 = lon1;
                lat0 = lat1;
        }
	clockwise = area < 0.0;
	return clockwise;
}

static double great_circle_distance(float lat1, float lon1, float lat2, float lon2)
{
	double a1[4], a2[4], retval;
	double dtor = atan(1.0)/ 45.0;
	a1[0] = cos(dtor * lat1);
	a1[1] = sin(dtor * lat1);
	a1[2] = cos(dtor * lon1);
	a1[3] = sin(dtor * lon1);

	a2[0] = cos(dtor * lat2);
	a2[1] = sin(dtor * lat2);
	a2[2] = cos(dtor * lon2);
	a2[3] = sin(dtor * lon2);
	retval = (double)adgcdp(a1,a2);
	return retval;
}
	
#define INIT -1
#define IN 0
#define OUT 1

static int CyclicClipPolyPoints(float *x,float *y,float *xw,float *yw,float *x_ret, float *y_ret, float *xw_ret, float *yw_ret, int n, double *pval, int fix, int right)
{
	int i;
	int init, status;
	int state;
	double xd0,xd1,xd_ret,yd0,yd1,yd_ret,pt0,pt1,pt_ret,xu0,xu1,xu_ret,yu0,yu1,yu_ret;
	double xu_last = 1e12, yu_last = 1e12;
	int last_ix;
	double pnew;
	double plast;
	int ix_save;
	int istate;
	static int call_count = 0;

	call_count++;
#if 0
	printf("fix index %d, call # %d\n",fix,call_count); /* fix should never come in as zero to this routine */
#endif
	if (right) {
		/* set state to condition before current index (fix) */
		if (pval[fix] > 0) {
			state = OUT;
		}
		else {
			state = IN;
		}
	}
	else {
		if (pval[fix] < 0) {
			state = OUT;
		}
		else {
			state = IN;
		}
	}
	istate = state;
	ix_save = 0;
	init = fix == 0 ? 1 : 0;
	last_ix = state == IN ? fix : 0;
	xu_last = xw[fix-1];
	yu_last = yw[fix-1];
	plast = pval[fix-1];
	pnew = pval[fix];
	status = state == IN ? 1 : 0; /* transition from state at fix -1 to fix */
	for (i = 0; i < fix; i++) {
		xw_ret[i] = xw[i];
		yw_ret[i] = yw[i];
		x_ret[i] = x[i];
		y_ret[i] = y[i];
	}

	for (i = fix; i <= n; i++) {
		int icomp = i - 1;
		int icur = (i == n) ? ix_save : i;

		if (state == OUT) {
			if (status) {
				plast = pval[icur];
				xu_last = xw[icur];
				yu_last = yw[icur];
			}
			else {
				init = 0;
				xd0 = x[icur];
				yd0 = y[icur];
				xd1 = x[icomp];
				yd1 = y[icomp];
				pt0 = pval[icur];
				pt1 = pval[icomp];
				xu0 = xw[icur];
				yu0 = yw[icur];
				xu1 = xu_last;
				yu1 = yu_last;
				NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
						       &yd1,&xd1,&pt1,&xu1,&yu1,
						       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
				xw_ret[last_ix] = xu_ret;
				yw_ret[last_ix] = yu_ret;
				x_ret[last_ix] = xd_ret;
				y_ret[last_ix] = yd_ret;
				xu_last = xu0;
				yu_last = yu0;
				last_ix++;
				xw_ret[last_ix] = xu0;
				yw_ret[last_ix] = yu0;
				x_ret[last_ix] = xd0;
				y_ret[last_ix] = yd0;
				last_ix++;
				plast = pval[icur];
				state = IN;
				if (i == n - 1 && istate == IN)
					break;
			}
		}
		else {
			init = 0;
			if (! status) {
				xu_last = xw_ret[last_ix] = xw[icur];
				yu_last = yw_ret[last_ix] = yw[icur];
				plast = pval[icur];
				last_ix++;
			}
			else {
				xd0 = x[icomp];
				yd0 = y[icomp];
				xd1 = x[icur];
				yd1 = y[icur];
				pt0 = plast;
				pt1 = pval[icur];
				xu0 = xu_last;
				yu0 = yu_last;
				xu1 = xw[icur];
				yu1 = yw[icur];
				NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
						       &yd1,&xd1,&pt1,&xu1,&yu1,
						       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
				plast = pval[icur];
				xw_ret[last_ix] = xu_ret;
				yw_ret[last_ix] = yu_ret;
				x_ret[last_ix] = xd_ret;
				y_ret[last_ix] = yd_ret;
				xu_last = xu1;
				yu_last = yu1;
				last_ix++;
				state = OUT;
				if (i == n - 1 && istate == OUT)
					break;
			}
		}
		pnew = pval[i >= n - 1 ? ix_save : i + 1];
		if (right) {
			status = pnew > 0 ? 0 : 1;
		}
		else {
			status = pnew < 0 ? 0 : 1;
		}
		/*status = (fabs(pnew - plast) > 270) ? 1 : 0;*/
	}
	
	return last_ix; /* this is the new n */
}
static int ClipPolyPoints(float *x,float *y,float *xw,float *yw,int n, double *pval, int fix)
{
	int i;
	int init, status;
	int state = OUT;
	double xd0,xd1,xd_ret,yd0,yd1,yd_ret,pt0,pt1,pt_ret,xu0,xu1,xu_ret,yu0,yu1,yu_ret;
	double xu_last = 1e12, yu_last = 1e12;
	int last_ix = 0;
	float xtmp,ytmp;
	double pnew;
	double plast;

	if (fix > 0) {
		xd0 = x[fix-1];
		yd0 = y[fix-1];
		xd1 = x[fix];
		yd1 = y[fix];
		pt0 = pval[fix-1];
		pt1 = pval[fix];
		xu0 = xw[fix-1];
		yu0 = yw[fix-1];
		xu1 = xw[fix];
		yu1 = yw[fix];
		NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
				       &yd1,&xd1,&pt1,&xu1,&yu1,
				       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
		last_ix = fix;
		xu_last = xu1;
		yu_last = yu1;
		xw[last_ix] = xu_ret;
		yw[last_ix] = yu_ret;
		last_ix++;
	}
	init = fix == 0 ? 1 : 0;
	plast = pval[fix];
	for (i = fix + 1; i < n; i++) {
		NGCALLF(map_next_point,MAP_NEXT_POINT)(y + i,x + i, &xtmp,&ytmp,&pnew,&status,&init);
		if (state == OUT) {
			if (status) {
				plast = pnew;
				xu_last = xtmp;
				yu_last = ytmp;
				continue;
			}
			init = 0;
			xd0 = x[i];
			yd0 = y[i];
			xd1 = x[i-1];
			yd1 = y[i-1];
			pt0 = pnew;
			pt1 = plast;
			xu0 = xtmp;
			yu0 = ytmp;
			xu1 = xu_last;
			yu1 = yu_last;
			NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
					       &yd1,&xd1,&pt1,&xu1,&yu1,
					       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
			xw[last_ix] = xu_ret;
			yw[last_ix] = yu_ret;
			xu_last = xu0;
			yu_last = yu0;
			last_ix++;
			xw[last_ix] = xu0;
			yw[last_ix] = yu0;
			last_ix++;
			plast = pnew;
			state = IN;
		}
		else {
			init = 0;
			if (! status) {
				xu_last = xw[last_ix] = xtmp;
				yu_last = yw[last_ix] = ytmp;
				last_ix++;
				continue;
			}
			xd0 = x[i-1];
			yd0 = y[i-1];
			xd1 = x[i];
			yd1 = y[i];
			pt0 = plast;
			pt1 = pnew;
			xu0 = xu_last;
			yu0 = yu_last;
			xu1 = xtmp;
			yu1 = ytmp;
			NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
					       &yd1,&xd1,&pt1,&xu1,&yu1,
					       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
			plast = pt1;
			xw[last_ix] = xu_ret;
			yw[last_ix] = yu_ret;
			xu_last = xu1;
			yu_last = yu1;
			last_ix++;
			state = OUT;
		}
	}
	if (state == OUT && fix > 0) {
		/* need to interpolate from last point to first point */
		NGCALLF(map_next_point,MAP_NEXT_POINT)(y,x, &xtmp,&ytmp,&pnew,&status,&init);
		xd0 = x[0];
		yd0 = y[0];
		xd1 = x[n-1];
		yd1 = y[n-1];
		pt0 = pnew;
		pt1 = plast;
		xu0 = xtmp;
		yu0 = ytmp;
		xu1 = xu_last;
		yu1 = yu_last;
		NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
				       &yd1,&xd1,&pt1,&xu1,&yu1,
				       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
		xw[last_ix] = xu_ret;
		yw[last_ix] = yu_ret;
		last_ix++;
	}
	else if (state == IN && fix == 0) {
		/* need to interpolate from last point to first point */
		NGCALLF(map_next_point,MAP_NEXT_POINT)(&(y[n-1]),&(x[n-1]), &xtmp,&ytmp,&pnew,&status,&init);
		xd0 = x[n-1];
		yd0 = y[n-1];
		xd1 = x[0];
		yd1 = y[0];
		pt0 = pnew;
		pt1 = plast;
		xu0 = xtmp;
		yu0 = ytmp;
		xu1 = 1e12;
		yu1 = 1e12;
		NGCALLF(mditve,MDITVE)(&yd0,&xd0,&pt0,&xu0,&yu0,
				       &yd1,&xd1,&pt1,&xu1,&yu1,
				       &yd_ret,&xd_ret,&pt_ret,&xu_ret,&yu_ret);
		xw[last_ix] = xu_ret;
		yw[last_ix] = yu_ret;
		last_ix++;
	}
#if 0
	max_d = 0;
	for (i = 0; i < last_ix; i++) {
		int lix;
		lix = i == 0 ? last_ix - 1 : i -1;
		dist = sqrt((xw[i] - xw[lix]) * (xw[i] - xw[lix]) + (yw[i] - yw[lix]) * (yw[i] - yw[lix]));
		printf("( %f %f ) ", xw[i],yw[i]);
		if (max_d < dist)  max_d = dist;
	}
	printf("\n");
	printf("========== %f\n",max_d);
	if (max_d > cum_maxd) {
		printf("!!!!!!!!!!!!!!!%f\n",cum_maxd);
		cum_maxd = max_d;
	}
#endif	
	return last_ix; /* this is the new n */
	
}

static NhlErrorTypes GreatCircleRenderPolygon
(NhlLayer instance, float *x, float *y, float *xw, float *yw, int n, int closed )
{
	NhlMapTransObjLayer mptrans = (NhlMapTransObjLayer) instance;
	int nl;
	int gsid;
	int fill_color;
	float xmin,xmax,ymin,ymax,xt,yt;
	float uv_cut, uv_area;
	float len;
	int i;
	float *xbuf,*ybuf;
	ng_size_t current_size = 256;
	ng_size_t tcount, npoints;
	float usize,vsize;
	int malloced = 0;
	static int tdebug = 0;
	static int call_count = 0;
	static float  tx = 0 , ty = 0;

	call_count++;
	usize = mptrans->mptrans.ur - mptrans->mptrans.ul;
	vsize = mptrans->mptrans.ut - mptrans->mptrans.ub;
	uv_cut = sqrt(usize * usize + vsize * vsize) / 100;
	uv_area = usize  * vsize;
	

	if (closed)
		nl = n;
	else {
		xw[n] = xw[0];
		yw[n] = yw[0];
		nl = n + 1;
	}
	/* first test bounding box of polygon to see if extra points are needed */
	xmin = xmax = xw[0];
	ymin = ymax = yw[0];
	for (i = 1; i < nl; i++) {
		if (xmin > xw[i]) 
			xmin = xw[i];
		if (xmax < xw[i])
			xmax = xw[i];
		if (ymin > yw[i]) 
			ymin = yw[i];
		if (ymax < yw[i])
			ymax = yw[i];
	}
	xt = xmax - xmin;
	yt = ymax - ymin;
	if (xt * yt > .5 * uv_area) {
		if (tdebug) 
			printf("skipping %d -- x %f y %f\n", call_count, xt, yt);
		return NhlNOERROR;
	}

	len = sqrt(xt * xt + yt * yt);
#if 0
	if (len / uv_cut > 20)
		return NhlNOERROR; 
#endif
			 
	tcount = 0;
	if (len < uv_cut) {
		xbuf = xw;
		ybuf = yw;
		tcount = nl;
	}
	else {
		xbuf = NhlMalloc(current_size * sizeof(float));
		ybuf = NhlMalloc(current_size * sizeof(float));
		malloced = 1;
		for (i = 0; i < nl-1; i++) {
			float xds, xde, yds, yde;
			int inext = i+1;
			int j;
			xt = xw[inext] - xw[i];
			yt = yw[inext] - yw[i];
			len = sqrt(xt * xt + yt * yt);
			xds = x[i];
			yds = y[i];
			xde = x[inext % n];
			yde = y[inext % n];
			xbuf[tcount] = xw[i];
			ybuf[tcount] = yw[i];
			tcount++;
			if (xt == 0 && (xw[i] == mptrans->mptrans.ul || xw[i] == mptrans->mptrans.ur))
				npoints = 0;
			else if (len > uv_cut) {
				npoints = (int) (len / uv_cut + .5);
			}
			else {
				npoints = 0;
			}
			if (tcount + npoints > current_size) {
				while (current_size < tcount + npoints + 100) {
					current_size *= 2;
				}
				xbuf = NhlRealloc(xbuf,current_size *sizeof(float));
				ybuf = NhlRealloc(ybuf,current_size *sizeof(float));
			}
			if (npoints > 0) {
				c_mapgci(yds,xds,yde,xde,npoints,ybuf + tcount,xbuf + tcount);
				for (j = tcount; j < tcount + npoints; j++) {
					c_maptrn(ybuf[j],xbuf[j],&(xbuf[j]),&(ybuf[j]));
					if (xbuf[j] > 1e8) 
						return NhlNOERROR;
				}
			}
			tcount += npoints;
		}
		xbuf[tcount] = xbuf[0];
		ybuf[tcount] = ybuf[0];
		tcount++;
	}

	if (tdebug) {
		int doprint = 0;
		float sx, sy;
		xmin = xmax = xbuf[0];
		ymin = ymax = ybuf[0];
		for (i = 1; i < tcount; i++) {
			if (xbuf[i] < 1e8 && xmin > xbuf[i]) 
				xmin = xbuf[i];
			if (xbuf[i] < 1e8 && xmax < xbuf[i])
				xmax = xbuf[i];
			if (xbuf[i] < 1e8 && ymin > ybuf[i]) 
				ymin = ybuf[i];
			if (xbuf[i] < 1e8 && ymax < ybuf[i])
				ymax = ybuf[i];
		}


		/*for (i = 0; i < tcount; i++)
			printf( "( %f %f ) ",xbuf[i],ybuf[i]);
			printf("\n");*/
		if (xmax - xmin > tx) {
			tx = xmax - xmin;
			doprint = 1;
		}
		if (ymax - ymin > ty) {
			ty = ymax - ymin;
			doprint = 1;
		}
		sx = xmax - xmin;
		sy = ymax - ymin;
		if (doprint) {
			printf("call # %d -- x %f y %f\n", call_count, tx, ty);
		}
		if (tx > 1e8 ||  ty > 1e8) return NhlNOERROR;
		if (sx * sx + sy * sy > 20 * uv_cut) 
			return NhlNOERROR;

		printf("max-min x  %f y  %f call #%d\n",sx,sy,call_count);

	}
	NhlVAGetValues(Wkptr->base.id,
		       _NhlNwkGraphicStyle, &gsid,
		       NULL);
	NhlVAGetValues(gsid,
		       NhlNgsFillColor, &fill_color,
		       NULL);
	gset_fill_colr_ind(fill_color);


	NGCALLF(gfa,GFA)(&tcount,xbuf,ybuf);
	if (malloced) {
		NhlFree(xbuf);
		NhlFree(ybuf);
	}
	return NhlNOERROR;
}

static NhlErrorTypes RenderPolygon
#if	NhlNeedProto
(float *xw, float *yw, int n, int closed )
#else
(xw, yw, n )
float *xw;
float *yw;
int n;
int closed;
#endif
{
	int nl;
	int gsid;
	int fill_color;

	if (closed)
		nl = n;
	else {
		xw[n] = xw[0];
		yw[n] = yw[0];
		nl = n + 1;
	}

	NhlVAGetValues(Wkptr->base.id,
		       _NhlNwkGraphicStyle, &gsid,
		       NULL);
	NhlVAGetValues(gsid,
		       NhlNgsFillColor, &fill_color,
		       NULL);

	gset_fill_colr_ind(fill_color);
#if 0
	for (i = 0; i < nl; i++) {
		if (fabs(xw[i]) < 0.05 && fabs(yw[i]) < 0.05)
			printf("here's a culprit maybe %f %f %d of %d\n", xw[i],yw[i],i,nl);
	}
#endif
	NGCALLF(gfa,GFA)(&nl,xw,yw);
	return NhlNOERROR;
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
	NhlErrorTypes ret = NhlNOERROR;
	int i;
	NhlMapTransObjLayer mptrans = (NhlMapTransObjLayer) instance;
	NhlBoolean	closed;
	int		gsid;
	float		*xw,*yw;
	int		nt;
	int             status;
	int             fill_color, fill_index;
	int 		init;
	double          *pval;
	int             is_cyclic;
	int             unprojectable;
	int             fail_ix;
	int             use_great_circle = 0;
	static int      count = 0;

	count++;
	Wkptr = mptrans->trobj.wkptr;
	NhlVAGetValues(Wkptr->base.id,
		       _NhlNwkGraphicStyle, &gsid,
		       NULL);
	NhlVAGetValues(gsid,
		       NhlNgsFillColor, &fill_color,
		       NhlNgsFillIndex, &fill_index,
		       NULL);

	if (fill_color < 0) 
		return (NhlNOERROR);
	gset_fill_colr_ind(fill_color);

	if (mptrans->mptrans.map_poly_mode == NhlSTANDARDPOLY ||
	    (mptrans->mptrans.map_poly_mode == NhlAUTOPOLY && fill_index != NhlSOLIDFILL)) {
		return StandardMapDataPolygon(instance,x,y,n);
	}
	if (mptrans->mptrans.projection == NhlLAMBERTEQUALAREA || 
	     mptrans->mptrans.projection== NhlAZIMUTHALEQUIDISTANT ||
	     (mptrans->trobj.line_interpolation_on && mptrans->mptrans.great_circle_lines_on))
		use_great_circle = 1;

	closed = x[0] == x[n-1] && y[0] == y[n-1] ? True : False;
	if (closed) {
		xw = (float*)NhlMalloc(sizeof(float) * (n+ 10));
		yw = (float*)NhlMalloc(sizeof(float) * (n+10));
		pval = (double*)NhlMalloc(sizeof(double) * (n+10));
	}
	else {
		xw = (float*)NhlMalloc(sizeof(float) * (n+10));
		yw = (float*)NhlMalloc(sizeof(float) * (n+10));
		pval = (double*)NhlMalloc(sizeof(double) * (n+10));
	}


	init = 1;
	is_cyclic = 0;
	unprojectable = 0;
	fail_ix = -1;
	/*printf("MapDataPolygon %d:",count);*/
	for (i = 0; i <= n; i++) {
		/*
		printf(" (%f %f)",x[i%n]-360,y[i%n]); 
		if (i > 0) {
			float gcd = great_circle_distance(y[i%n],x[i%n],y[i-1], x[i-1]);
			printf("gcd %f\n",gcd);
		}
		*/
		if (mptrans->mptrans.map_poly_mode == NhlAUTOPOLY && ! init) {
			if (great_circle_distance(y[i%n],x[i%n],y[i-1], x[i-1]) > 10) {
				NhlFree(xw);
				NhlFree(yw);
				NhlFree(pval);
				return StandardMapDataPolygon(instance,x,y,n);
			}
		}
		NGCALLF(map_next_point,MAP_NEXT_POINT)(y + (i % n),x + (i % n), xw + i, yw + i,pval + i,&status,&init);
		if (status == 3) {
			if (fail_ix < 0) fail_ix = i;
			is_cyclic = 1;
		}
		else if (status != 0) {
			if (fail_ix < 0) fail_ix = i;
			unprojectable = 1;
		}
		init = 0;
        }
	/*printf("\n");*/
 


	if (is_cyclic) {
		float *xw_ret, *yw_ret;
		float *x_ret, *y_ret;
		xw_ret = (float*)NhlMalloc(sizeof(float) * (n+10));
		yw_ret  = (float*)NhlMalloc(sizeof(float) * (n+10));
		x_ret = (float*)NhlMalloc(sizeof(float) * (n+10));
		y_ret  = (float*)NhlMalloc(sizeof(float) * (n+10));
                
		/*
		printf("Data locations: ");
		for (i = 0; i < n; i++) {
			printf("( %f %f ) ", x[i],y[i]);
		}
		printf("\n");
		printf("Win locations, Pval: ");
		for (i = 0; i < n; i++) {
			printf("( %f %f %f ) ", xw[i],yw[i],pval[i]);
		}
		printf("\n");
		*/

		nt = CyclicClipPolyPoints(x,y,xw,yw,x_ret,y_ret,xw_ret,yw_ret,n,pval,fail_ix,0);
		if (nt > 2)
			ret = use_great_circle ? GreatCircleRenderPolygon(instance,x_ret,y_ret,xw_ret,yw_ret,nt,closed) : RenderPolygon(xw_ret,yw_ret,nt,closed);
		
		/*
		printf("Win locations (after cyclic left): ");
		for (i = 0; i < nt; i++) {
			printf("( %f %f) ", xw_ret[i],yw_ret[i]);
		}
		printf("\n");
		*/
			
		nt = CyclicClipPolyPoints(x,y,xw,yw,x_ret,y_ret,xw_ret,yw_ret,n,pval,fail_ix,1);
		if (nt > 2) 
			ret = use_great_circle ? GreatCircleRenderPolygon(instance,x_ret,y_ret,xw_ret,yw_ret,nt,closed) : RenderPolygon(xw_ret,yw_ret,nt,closed);
		/*
		printf("Win locations (after cyclic right): ");
		for (i = 0; i < nt; i++) {
			printf("( %f %f) ", xw_ret[i],yw_ret[i]);
		}
		printf("\n");
		*/

		NhlFree(xw_ret);
		NhlFree(yw_ret);
		NhlFree(x_ret);
		NhlFree(y_ret);
	}
	else if (unprojectable) {
		nt = ClipPolyPoints(x,y,xw,yw,n,pval,fail_ix);
		if (nt > 2) 
			ret = use_great_circle ? GreatCircleRenderPolygon(instance,x,y,xw,yw,n,closed) : RenderPolygon(xw,yw,nt,closed);
	}
	else {
		ret = use_great_circle ? GreatCircleRenderPolygon(instance,x,y,xw,yw,n,closed) : RenderPolygon(xw,yw,n,closed);
	}       
	NhlFree(xw);
	NhlFree(yw);
	NhlFree(pval);
	Wkptr = NULL;

	return ret;
}

/*ARGSUSED*/
static NhlErrorTypes StandardMapDataPolygon
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
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	NhlMapTransObjClass mptransclass;
	int aws_id;
	NhlWorkspace *aws;
	float xdist, ydist;
	int size,i,j;
	NhlMapTransObjLayer mptrans = (NhlMapTransObjLayer) instance;
	char		cval[4];
	NhlBoolean	clockwise,closed,out_of_range;
	int		left_id, right_id;
	int		nexti;
	float		*xl,*yl;
	int		nl;

	Wkptr = mptrans->trobj.wkptr;
	closed = x[0] == x[n-1] && y[0] == y[n-1] ? True : False;

	clockwise = Clockwise(x,y,n,closed,&out_of_range);

	if (! out_of_range) {
		nl = n;
		xl = x;
		yl = y;
	}
	else {
		e_text = 
			"%s: out of range lat/lon coordinates encountered: constraining";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
		ret = MIN(ret,NhlWARNING);

		nl = closed ? n : n + 1;
		xl = NhlMalloc(nl * sizeof(float));
		yl = NhlMalloc(nl * sizeof(float));
		for (i = 0; i < n; i++) {
			if (y[i] > 90.0) 
				yl[i] = 90.0;
			else if (y[i] < -90.0)
				yl[i] = -90.0;
			else
				yl[i] = y[i];
			if (x[i] > 540.0)
				xl[i] = 540.0;
			else if (x[i] < -540.0)
				xl[i] = -540.0;
			else 
				xl[i] = x[i];
		}
		if (! closed) {
			yl[n] = yl[0];
			xl[n] = xl[0];
		}
		closed = True;
	}
			
	if (clockwise) {
		right_id = 5;
		left_id = 2;
	}
	else {
		right_id = 2;
		left_id = 5;
	}

	mptransclass = (NhlMapTransObjClass) _NhlClass(instance);
	aws_id = mptransclass->mptrans_class.aws_id;


	if (aws_id < 1) {
		aws_id = _NhlNewWorkspace(NhlwsAREAMAP,NhlwsNONE,
					  1000*sizeof(int));
		if (aws_id < 1) 
			return MIN(ret,(NhlErrorTypes)aws_id);
		mptransclass->mptrans_class.aws_id = aws_id;
	}
	if ((aws = _NhlUseWorkspace(aws_id)) == NULL) {
		e_text = "%s: error reserving area map workspace";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return(NhlFATAL);
	}
	c_arseti("RC",1);
	/* "VS" has to be set to 0; there was an obscure bug otherwise although I forget what it was */
	c_mpseti("VS",0);
	_NhlLLErrCheckPrnt(NhlWARNING,entry_name);
	subret = _NhlArinam(aws,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) goto err_ret;

	c_mpgetc("OU",cval,3);
	c_mpsetc("OU","NO");
	c_mpseti("G1",3);
	_NhlMapbla(aws,entry_name);
	c_mpsetc("OU",cval);
	
	subret = _NhlMapita(aws,yl[0],xl[0],0,3,left_id,right_id,entry_name); 
	if ((ret = MIN(subret,ret)) < NhlWARNING) goto err_ret;


	if (mptrans->trobj.line_interpolation_on) {
		if (mptrans->mptrans.great_circle_lines_on) {
			float	*xbuf,*ybuf;
			int	lastsize = 0;
			for (i = 0; i < n; i++) {
				nexti = i + 1;
				if (i == n - 1) {
					if (closed)
						break;
					else
						nexti = 0;
				}
				xdist = xl[nexti] - xl[i];
				ydist = yl[nexti] - yl[i];
				size =  MAX(1.0,(int)(fabs(xdist) + fabs(ydist)));
				if (lastsize == 0) {
					xbuf = NhlMalloc(size*sizeof(float));
					ybuf = NhlMalloc(size*sizeof(float));
					lastsize = size;
				}
				else if (lastsize < size) {
					xbuf = NhlRealloc(xbuf,size*sizeof(float));
					ybuf = NhlRealloc(ybuf,size*sizeof(float));
					lastsize = size;
				}
				if (! xbuf || !ybuf) {
					e_text = "%s: dynamic memory allocation error";
					NhlPError(NhlFATAL,
						  NhlEUNKNOWN,e_text,entry_name);
					ret = NhlFATAL;
					goto err_ret;
				}
				c_mapgci(yl[i],xl[i],
					 yl[nexti],xl[nexti],size,ybuf,xbuf);
				for (j = 0; j < size; j++) {
					subret = _NhlMapita(aws,ybuf[j],xbuf[j],
							    2,3,left_id,right_id,entry_name);
					if ((ret = MIN(subret,ret)) < NhlWARNING)
						goto err_ret;
				}
				subret = _NhlMapita(aws,yl[nexti],xl[nexti],
						    2,3,left_id,right_id,entry_name);
				if ((ret = MIN(subret,ret)) < NhlWARNING)
					goto err_ret;
			}
			NhlFree(xbuf);
			NhlFree(ybuf);
		}
		else {
			for (i = 0; i < n; i++) {
				nexti = i + 1;
				if (i == n - 1) {
					if (closed)
						break;
					else
						nexti = 0;
				}
				xdist = xl[nexti] - xl[i];
				ydist = yl[nexti] - yl[i];
				size =  MAX(1.0,(int)(fabs(xdist) + fabs(ydist)));
				for (j = 0; j < size; j++) {
					subret = _NhlMapita(aws,
							    yl[i] + ydist *(j+1)/ (float)size,
							    xl[i] + xdist *(j+1)/ (float)size,2,
							    3,left_id,right_id,entry_name);
					if ((ret = MIN(subret,ret)) < NhlWARNING)
						goto err_ret;
				}
			}
		}
	}
	else {
		for (i = 1; i <= n ; i++) {
			if (i == n && closed)
				break;
			subret = _NhlMapita(aws,yl[i % n],xl[i % n],2,3,left_id,right_id,entry_name);
			if ((ret = MIN(subret,ret)) < NhlWARNING)
				goto err_ret;
		}
	}
	
	_NhlMapiqa(aws,3,left_id,right_id,entry_name);

	if (mptrans->mptrans.dump_polygon_area_map) 
		_NhlDumpAreaMap(aws,entry_name);

	subret = _NhlArpram(aws,0,0,0,entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) goto err_ret;

	subret = _NhlArscam(aws,(_NHLCALLF(hlumappolygon,HLUMAPPOLYGON)),
			    entry_name);
	if ((ret = MIN(subret,ret)) < NhlWARNING) goto err_ret;

err_ret:
	subret = _NhlIdleWorkspace(aws);
	if ((ret = MIN(subret,ret)) < NhlWARNING) return ret;

	if (out_of_range) {
		NhlFree(xl);
		NhlFree(yl);
	}

	Wkptr = NULL;

	return ret;
}

/*
 * Function:  hlumappolygon
 *
 * Description: C version of APR user routine called from within ARSCAM 
 *		to fill the data polygon
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
int (_NHLCALLF(hlumappolygon,HLUMAPPOLYGON))
#if	NhlNeedProto
(
	float *xcs, 
	float *ycs, 
	int *ncs, 
	int *iai, 
	int *iag, 
	int *nai
)
#else
(xcs,ycs,ncs,iai,iag,nai)
	float *xcs; 
	float *ycs; 
	int *ncs; 
	int *iai; 
	int *iag; 
	int *nai;
#endif
{

	if (*iai < 1) return 0;
#if 0
	printf("iai %d iag %d nai %d\n", *iai,*iag,*nai);
#endif
	if (*iai == 5) {
		_NhlWorkstationFill(Wkptr,xcs,ycs,*ncs);
	}

	return 0;
}

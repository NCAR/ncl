/*
*      $Id: MapTransObj.c,v 1.8 1994-06-24 00:39:40 dbrown Exp $
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
#include <strings.h>
#include <math.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/MapTransObjP.h>
#include <ncarg/hlu/View.h>
#include <ncarg/hlu/Converters.h>
#include <ncarg/hlu/FortranP.h>

static NhlResource resources[] = {
{NhlNtrOutOfRangeF,NhlCtrOutOfRangeF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,trobj.out_of_range),
	 NhlTString,_NhlUSET("1.0e12"),0,NULL},
{NhlNmpProjection,NhlCmpProjection,NhlTProjection,sizeof(NhlProjection),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.projection),NhlTImmediate,
	 _NhlUSET((NhlPointer)NhlCYLINDRICAL_EQUIDISTANT),0,NULL},
{NhlNmpCenterLatF,NhlCmpCenterLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.center_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpCenterLonF,NhlCmpCenterLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.center_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpCenterRotF,NhlCmpCenterRotF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.center_rot),
	 NhlTString,_NhlUSET("0.0"),0,NULL},

{NhlNmpMapLimitMode,NhlCmpMapLimitMode,NhlTMapLimitMode,
	 sizeof(NhlMapLimitMode),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.map_limit_mode),
	 NhlTImmediate,_NhlUSET((NhlPointer)NhlMAXIMALAREA),0,NULL},
{NhlNmpMinLatF,NhlCmpMinLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.min_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpMaxLatF,NhlCmpMaxLatF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.max_lat),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpMinLonF,NhlCmpMinLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.min_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpMaxLonF,NhlCmpMaxLonF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.max_lon),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpLeftAngleF,NhlCmpLeftAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.left_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpRightAngleF,NhlCmpRightAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.right_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpBottomAngleF,NhlCmpBottomAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.bottom_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
{NhlNmpTopAngleF,NhlCmpTopAngleF,NhlTFloat,sizeof(float),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.top_angle),
	 NhlTString,_NhlUSET("0.0"),0,NULL},
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
{NhlNmpEllipticalBoundary,NhlCmpEllipticalBoundary,NhlTInteger,
	 sizeof(int),
	 NhlOffset(NhlMapTransObjLayerRec,mptrans.elliptical_boundary),
	 NhlTString,_NhlUSET("0") ,0,NULL},

/* not sure these are needed,
{ NhlNmpMapPosRF, NhlCmpMapPosRF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_r),
	NhlTString,_NhlUSET("0.95") ,0,NULL},
{ NhlNmpMapPosTF, NhlCmpMapPosTF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_t),
	NhlTString,_NhlUSET("0.95") ,0,NULL},
{ NhlNmpMapPosTF, NhlCmpMapPosTF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_t),
	NhlTString,_NhlUSET("0.0") ,0,NULL}
*/
{ NhlNmpRectLimitType, NhlCmpRectLimitType, NhlTString, sizeof(char*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_type),
	NhlTString,_NhlUSET("MA"),0,(NhlFreeFunc)NhlFree},
{ NhlNmpRectLimit1, NhlCmpRectLimit1, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_1),
	NhlTFloatPtr,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},
{ NhlNmpRectLimit2, NhlCmpRectLimit2, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_2),
	NhlTFloatPtr,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},
{ NhlNmpRectLimit3, NhlCmpRectLimit3, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_3),
	NhlTFloatPtr,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree},
{ NhlNmpRectLimit4, NhlCmpRectLimit4, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_4),
	NhlTFloatPtr,_NhlUSET(NULL),0,(NhlFreeFunc)NhlFree}
};

/*
* Base Methods defined here
*/

static NhlErrorTypes MapTransInitialize(
#ifdef NhlNeedProto
NhlLayerClass,     /* class */
NhlLayer,          /* req */
NhlLayer,          /* new */
_NhlArgList,    /* args */
int             /* num_args */
#endif
);

static NhlErrorTypes    MapTransClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes  MapTransSetValues(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
NhlLayer   /*instance*/,
NhlLayer   /*parent */
#endif
);

static NhlErrorTypes MapWinToNDC(
#ifdef NhlNeedProto
NhlLayer   /*instance*/,
NhlLayer   /* parent */,
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
#ifdef NhlNeedProto
NhlLayer   /*instance*/,
NhlLayer   /*parent */,
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
#ifdef NhlNeedProto 
NhlLayer   /*instance */, 
NhlLayer   /*parent */, 
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
#ifdef NhlNeedProto 
NhlLayer   /*instance */, 
NhlLayer   /*parent */, 
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
#ifdef NhlNeedProto
NhlLayer   /*instance */, 
NhlLayer   /*parent */, 
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
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes MapDataLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes MapWinLineTo(
#if     NhlNeedProto
NhlLayer   /* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
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

NhlMapTransObjLayerClassRec NhlmapTransObjLayerClassRec = {
{
/* class_name			*/	"MapTransObj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlMapTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhltransObjLayerClassRec,

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
/* NDC_lineto */        MapNDCLineTo
}
};

NhlLayerClass NhlmapTransObjLayerClass = (NhlLayerClass) &NhlmapTransObjLayerClassRec;

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
#if __STDC__
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
	float xl;
	float yt;
	float width,xr;
	float height,yb;
	int irold,nerr,loglin;
	NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;
	NhlMapTransObjLayerPart	*mtp = &(minstance->mptrans);
	char *cproj, *climit;
	float v_angle_lim, h_angle_lim;
	float rl1[2],rl2[2],rl3[2],rl4[2];

	NhlVAGetValues(parent->base.id,
		       NhlNvpXF,&xl,
		       NhlNvpYF,&yt,
		       NhlNvpWidthF,&width,
		       NhlNvpHeightF,&height,NULL);
	xr = xl + width;
	yb = yt - height;
	c_mappos(xl,xr,yb,yt);
	c_mapsti("EL",mtp->elliptical_boundary);

	switch (mtp->projection) {
	case NhlORTHOGRAPHIC:
		cproj = "OR";
		h_angle_lim = v_angle_lim = 90;
		break;
	case NhlSTEREOGRAPHIC:
		cproj = "ST";
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlLAMBERT_EQUALAREA:
		cproj = "LE";
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlGNOMONIC:
		cproj = "GN";
		h_angle_lim = v_angle_lim = 85;
		break;
	case NhlAZIMUTHAL_EQUIDISTANT:
		cproj = "AE";
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMOLLWEIDE:
		cproj = "MO";
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMERCATOR:
		cproj = "ME";
		h_angle_lim = 180;
		v_angle_lim = 85;
		break;
	case NhlCYLINDRICAL_EQUIDISTANT:
		cproj = "CE";
		h_angle_lim = 90;
		v_angle_lim = 180;
		break;
	case NhlLAMBERT_CONFORMAL:
		if (mtp->map_limit_mode == NhlANGLES) {
			e_text = "%s: internal check error";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
			return NhlFATAL;
		}
		cproj = "LC";
		break;
	case NhlSATELLITE:
		c_mapstr("SA",mtp->satellite_dist);
		c_mapstr("S1",mtp->satellite_angle_1);
		c_mapstr("S2",mtp->satellite_angle_2);
		h_angle_lim = v_angle_lim = 90;
		cproj = "SV";
		break;
	default:
		e_text = "%s: internal enumeration error - projection";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,entry_name);
		return NhlFATAL;
	}


	if (mtp->projection == NhlLAMBERT_CONFORMAL) {
		c_maproj(cproj,mtp->lambert_parallel_1,mtp->lambert_meridian,
			 mtp->lambert_parallel_2);
	}
	else {
		c_maproj(cproj,mtp->center_lat,mtp->center_lon,
			 mtp->center_rot);
	}

	switch (mtp->map_limit_mode) {
	case NhlMAXIMALAREA:
		rl1[0] = rl2[0] = rl3[0] = rl4[0] = 0.0;
		climit = "MA";
		break;
	case NhlLATLON:
		if (mtp->updated) {
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
			NhlPError(NhlWARNING,NhlEUNKNOWN,e_text,entry_name);
			ret = MIN(ret,NhlWARNING);
			c_mapset("MA",rl1,rl2,rl3,rl4);
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
/*
 * This destroys the nicely selected aspect ratio but is the only way
 * to guarantee WYSIWYG display and Point and Click after 
 * a plot has been transformed.
 */
	mtp->map_pos_l = xl;
	mtp->map_pos_r = xr;
	mtp->map_pos_t = yt;
	mtp->map_pos_b = yb;

	c_set (xl,xr,yb,yt,mtp->ul,mtp->ur,mtp->ub,mtp->ut,loglin);

	mtp->updated = True;

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
#if __STDC__
( NhlLayer instance,NhlLayer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int* status)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing,status)
NhlLayer instance;
NhlLayer parent;
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
#if __STDC__
( NhlLayer instance,NhlLayer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	NhlLayer parent;
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
			||(y[i] < mtp->map_pos_t)) {
	
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
#if __STDC__
( NhlLayer instance,NhlLayer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	NhlLayer parent;
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
* FORTRAN and C because of arithmentic error
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
#if __STDC__
( NhlLayer instance,NhlLayer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	NhlLayer parent;
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
#if __STDC__
( NhlLayer instance,NhlLayer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing, float *ymissing, int* status)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer instance;
	NhlLayer parent;
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
			if((xout[i] == 1e12)||
				(yout[i] == 1e12)) {

				*status = 1;
				xout[i]=yout[i]=minstance->trobj.out_of_range;
			}
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
#if __STDC__
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
	NhlMapTransObjLayerPart	*mtp = &(mnew->mptrans);
	NhlErrorTypes ret = NhlNOERROR, subret = NhlNOERROR;
	char *e_text, *entry_name = "MapTransSetValues";

	mtp->updated = False;
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
#if  __STDC__
( NhlLayerClass class,NhlLayer req,NhlLayer new,_NhlArgList args,int num_args)
#else
(class,req,new,args,num_args)
	NhlLayerClass class;
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

	mtp->updated = False;
	if (_NhlCmpFAny(mnew->trobj.out_of_range,1e12,6) != 0) {
		e_text = 
		"%s: %s must always equal 1e12 for map projections: resetting";
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  e_text,entry_name,NhlNtrOutOfRangeF);
		mnew->trobj.out_of_range = 1e12;
	} 
	
	subret = CheckMapLimits(mnew,entry_name);
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
#if  __STDC__
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
#if  __STDC__
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

	switch (mtp->projection) {
	case NhlORTHOGRAPHIC:
		h_angle_lim = v_angle_lim = 90;
		break;
	case NhlSTEREOGRAPHIC:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlLAMBERT_EQUALAREA:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlGNOMONIC:
		h_angle_lim = v_angle_lim = 85;
		break;
	case NhlAZIMUTHAL_EQUIDISTANT:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMOLLWEIDE:
		h_angle_lim = v_angle_lim = 180;
		break;
	case NhlMERCATOR:
		h_angle_lim = 180;
		v_angle_lim = 85;
		break;
	case NhlCYLINDRICAL_EQUIDISTANT:
		h_angle_lim = 90;
		v_angle_lim = 180;
		break;
	case NhlLAMBERT_CONFORMAL:
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
			mtp->map_limit_mode = NhlMAXIMALAREA;
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
			mtp->map_limit_mode = NhlMAXIMALAREA;
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
	case NhlCORNERS:
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
			mtp->map_limit_mode = NhlMAXIMALAREA;
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
			mtp->map_limit_mode = NhlMAXIMALAREA;
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
#if  __STDC__
(void)
#else
()
#endif
{
        NhlConvertArg   limitmodelist[] = {
        {NhlSTRENUM,    NhlMAXIMALAREA, _NhlUSET("maximalarea")},
        {NhlSTRENUM,    NhlLATLON, 	_NhlUSET("latlon")},
        {NhlSTRENUM,    NhlANGLES,      _NhlUSET("angles")},
        {NhlSTRENUM,    NhlCORNERS,     _NhlUSET("corners")},
        {NhlSTRENUM,    NhlWINDOW,      _NhlUSET("window")}
        };

        NhlConvertArg   intlimitmodelist[] = {
        {NhlIMMEDIATE,  sizeof(int),    _NhlUSET((NhlPointer)NhlMAXIMALAREA)},
        {NhlIMMEDIATE,  sizeof(int),    _NhlUSET((NhlPointer)NhlLATLON)},
        {NhlIMMEDIATE,  sizeof(int),    _NhlUSET((NhlPointer)NhlANGLES)},
        {NhlIMMEDIATE,  sizeof(int),    _NhlUSET((NhlPointer)NhlCORNERS)},
        {NhlIMMEDIATE,  sizeof(int),    _NhlUSET((NhlPointer)NhlWINDOW)}
	};

        NhlConvertArg   projectionlist[] = {
        {NhlSTRENUM,NhlORTHOGRAPHIC,	_NhlUSET("orthographic")},
        {NhlSTRENUM,NhlSTEREOGRAPHIC,	_NhlUSET("stereographic")},
        {NhlSTRENUM,NhlLAMBERT_EQUALAREA,_NhlUSET("lambert_equalarea")},
        {NhlSTRENUM,NhlGNOMONIC,	_NhlUSET("gnomonic")},
        {NhlSTRENUM,NhlAZIMUTHAL_EQUIDISTANT,
		 			_NhlUSET("azimuthal_equidistant")},
        {NhlSTRENUM,NhlSATELLITE,      	_NhlUSET("satellite")},
        {NhlSTRENUM,NhlMOLLWEIDE,	_NhlUSET("mollweide")},
        {NhlSTRENUM,NhlMERCATOR,	_NhlUSET("mercator")},
        {NhlSTRENUM,NhlCYLINDRICAL_EQUIDISTANT,
		 _NhlUSET("cylindrical_equidistant")},
        {NhlSTRENUM,NhlLAMBERT_CONFORMAL,_NhlUSET("lambert_conformal")}
        };

        NhlConvertArg   intprojectionlist[] = {
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlORTHOGRAPHIC)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlSTEREOGRAPHIC)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlLAMBERT_EQUALAREA)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlGNOMONIC)},
        {NhlIMMEDIATE,sizeof(int),
		 _NhlUSET((NhlPointer)NhlAZIMUTHAL_EQUIDISTANT)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlSATELLITE)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlMOLLWEIDE)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlMERCATOR)},
        {NhlIMMEDIATE,sizeof(int),
		 _NhlUSET((NhlPointer)NhlCYLINDRICAL_EQUIDISTANT)},
        {NhlIMMEDIATE,sizeof(int),_NhlUSET((NhlPointer)NhlLAMBERT_CONFORMAL)}
	};

        NhlConvertArg   limitmodegentoenumdat[] = {
        {NhlIMMEDIATE,  sizeof(char*),_NhlUSET((NhlPointer)NhlTMapLimitMode)},
        };
        NhlConvertArg   projectiongentoenumdat[] = {
        {NhlIMMEDIATE,  sizeof(char*),_NhlUSET((NhlPointer)NhlTProjection)},
        };

        NhlRegisterConverter(NhlTGenArray,NhlTMapLimitMode,NhlCvtGenToEnum,
			     limitmodegentoenumdat,1,False,NULL);

        NhlRegisterConverter(NhlTString,NhlTMapLimitMode,NhlCvtStringToEnum,
			     limitmodelist,
			     NhlNumber(limitmodelist),False,NULL);
        NhlRegisterConverter(NhlTInteger,NhlTMapLimitMode,NhlCvtIntToEnum,
			     intlimitmodelist,
			     NhlNumber(intlimitmodelist),False,NULL);
        NhlRegisterConverter(NhlTFloat,NhlTMapLimitMode,NhlCvtFloatToEnum,
			     intlimitmodelist,
			     NhlNumber(intlimitmodelist),False,NULL);
        NhlRegisterConverter(NhlTMapLimitMode,NhlTString,NhlCvtEnumToString,
			     limitmodelist,
			     NhlNumber(limitmodelist),False,NULL);
        NhlRegisterConverter(NhlTMapLimitMode,_NhlTFExpString,NhlCvtEnumToFStr,
			     limitmodelist,
			     NhlNumber(limitmodelist),False,NULL);

        NhlRegisterConverter(NhlTGenArray,NhlTProjection,NhlCvtGenToEnum,
			     projectiongentoenumdat,1,False,NULL);

        NhlRegisterConverter(NhlTString,NhlTProjection,NhlCvtStringToEnum,
			     projectionlist,
			     NhlNumber(projectionlist),False,NULL);
        NhlRegisterConverter(NhlTInteger,NhlTProjection,NhlCvtIntToEnum,
			     intprojectionlist,
			     NhlNumber(intprojectionlist),False,NULL);
        NhlRegisterConverter(NhlTFloat,NhlTProjection,NhlCvtFloatToEnum,
			     intprojectionlist,
			     NhlNumber(intprojectionlist),False,NULL);
        NhlRegisterConverter(NhlTProjection,NhlTString,NhlCvtEnumToString,
			     projectionlist,
			     NhlNumber(projectionlist),False,NULL);
        NhlRegisterConverter(NhlTProjection,_NhlTFExpString,NhlCvtEnumToFStr,
			     projectionlist,
			     NhlNumber(projectionlist),False,NULL);
	return NhlNOERROR;

}
/*ARGSUSED*/
static NhlErrorTypes MapDataLineTo
#if __STDC__
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance, parent, x, y, upordown)
NhlLayer instance;
NhlLayer parent;
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
#if __STDC__
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance, parent, x, y, upordown)
NhlLayer instance;
NhlLayer parent;
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
                        return(_NhlWorkstationLineTo(parent->base.wkptr,c_cufx(x),c_cufy(y),1));
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
                                _NhlWorkstationLineTo(parent->base.wkptr,c_cufx(lastx),c_cufy(lasty),1);
                                call_frstd = 2;
                        }
                        _NhlWorkstationLineTo(parent->base.wkptr,c_cufx(currentx),c_cufy(currenty),0);
                        lastx = x;
                        return(NhlNOERROR);
                }
        }
}
static NhlErrorTypes MapNDCLineTo
#if __STDC__
(NhlLayer instance, NhlLayer parent, float x, float y, int upordown)
#else
(instance, parent, x, y, upordown)
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
        NhlMapTransObjLayer mpinst = (NhlMapTransObjLayer)instance;
        static float lastx,lasty;
        static call_frstd = 1;
        float currentx,currenty;
        float xvp,yvp,widthvp,heightvp;
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
                NhlVAGetValues(parent->base.id,
                        NhlNvpXF,&xvp,
                        NhlNvpYF,&yvp,
                        NhlNvpWidthF,&widthvp,
                        NhlNvpHeightF,&heightvp,NULL);
                _NhlTransClipLine( xvp, xvp+widthvp, yvp-heightvp, yvp,
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
                        return(_NhlWorkstationLineTo(parent->base.wkptr,x,y,1));
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
                                ret1 = _NhlWorkstationLineTo(parent->base.wkptr,lastx,lasty,1);
                                call_frstd = 2;
			}
                        ret = _NhlWorkstationLineTo(parent->base.wkptr,currentx,currenty,0);
                        lastx = x;
                        lasty = y;
                        return(MIN(ret1,ret));
                }


        }

}


/*
*      $Id: MapTransObj.c,v 1.6 1994-04-29 21:31:18 dbrown Exp $
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
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/MapTransObjP.h>
#include <ncarg/hlu/View.h>

#define DEFAULT_PROJECTION "PS"
#define DEFAULT_OUTLINE_TYPE "PS"
#define DEFAULT_RECT_LIMIT_TYPE "MA"

static NhlResource resources[] = {
{ NhlNmpProjection, NhlCmpProjection, NhlTString, sizeof(char*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.projection),
	NhlTString,"CE" },
{ NhlNmpOutlineType, NhlCmpOutlineType, NhlTString, sizeof(char*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.outline_type),
	NhlTString,"PS" },
{ NhlNmpCenterLatF, NhlCmpCenterLatF, NhlTFloat, sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.center_lat),
	NhlTString,"0.0"},
{ NhlNmpCenterLonF, NhlCmpCenterLonF, NhlTFloat, sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.center_lon),
	NhlTString,"0.0"},
{ NhlNmpCenterRotF, NhlCmpCenterRotF, NhlTFloat, sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.center_rot),
	NhlTString,"0.0"},
{ NhlNmpRectLimitType, NhlCmpRectLimitType, NhlTString, sizeof(char*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_type),
	NhlTString,"MA"},
{ NhlNmpRectLimit1, NhlCmpRectLimit1, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_1),
	NhlTFloatPtr,NULL},
{ NhlNmpRectLimit2, NhlCmpRectLimit2, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_2),
	NhlTFloatPtr,NULL},
{ NhlNmpRectLimit3, NhlCmpRectLimit3, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_3),
	NhlTFloatPtr,NULL},
{ NhlNmpRectLimit4, NhlCmpRectLimit4, NhlTFloatPtr, sizeof(float*),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.rect_limit_4),
	NhlTFloatPtr,NULL},
{ NhlNmpLambertParallel1F, NhlCmpLambertParallel1F, NhlTFloat, 
	sizeof(float), 
	NhlOffset(NhlMapTransObjLayerRec,mptrans.lambert_parallel_1),
	NhlTString,".001"},
{ NhlNmpLambertParallel2F, NhlCmpLambertParallel2F, NhlTFloat, 
	sizeof(float), 
	NhlOffset(NhlMapTransObjLayerRec,mptrans.lambert_parallel_2),
	NhlTString,"89.999"},
{ NhlNmpLambertMeridianF,NhlCmpLambertMeridianF,NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.lambert_meridian),
	NhlTString,"0.0"},
{ NhlNmpSatelliteDistF,NhlCmpSatelliteDistF,NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_dist),
	NhlTString,"1"},
{ NhlNmpSatelliteAngle1F,NhlCmpSatelliteAngle1F,NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_angle_1),
	NhlTString,"0.0"},
{ NhlNmpSatelliteAngle2F,NhlCmpSatelliteAngle2F,NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_angle_2),
	NhlTString,"0.0"},
{ NhlNmpSatelliteAngle2F,NhlCmpSatelliteAngle2F,NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.satellite_angle_2),
	NhlTString,"0.0"},
{ NhlNmpEllipticalBoundary,NhlCmpEllipticalBoundary,NhlTInteger,
	sizeof(int),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.elliptical_boundary),
	NhlTString,"0" }
/* not sure these are needed,
{ NhlNmpMapPosLF, NhlCmpMapPosLF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_l),
	NhlTString,"0.05" },
{ NhlNmpMapPosRF, NhlCmpMapPosRF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_r),
	NhlTString,"0.95" },
{ NhlNmpMapPosTF, NhlCmpMapPosTF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_t),
	NhlTString,"0.95" },
{ NhlNmpMapPosTF, NhlCmpMapPosTF, NhlTFloat,sizeof(float),
	NhlOffset(NhlMapTransObjLayerRec,mptrans.map_pos_t),
	NhlTString,"0.0" }
*/
};

/*
* Base Methods defined here
*/

static NhlErrorTypes  MapTransSetValues(
#ifdef NhlNeedProto
NhlLayer,          /* old */
NhlLayer,          /* reference */
NhlLayer,          /* new */
_NhlArgList,    /* args */
int             /* num_args*/
#endif
);

static NhlErrorTypes MapTransInitialize(
#ifdef NhlNeedProto
NhlLayerClass,     /* class */
NhlLayer,          /* req */
NhlLayer,          /* new */
_NhlArgList,    /* args */
int             /* num_args */
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
/* class_initialize		*/	NULL,
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
float xl;
float yt;
float width,xr;
float height,yb;
int irold,nerr,loglin;
NhlMapTransObjLayer minstance = (NhlMapTransObjLayer)instance;

/*
* C bindings don't work for maps yet
*/
NhlVAGetValues(parent->base.id,
	NhlNvpXF,&xl,
	NhlNvpYF,&yt,
	NhlNvpWidthF,&width,
	NhlNvpHeightF,&height,NULL);
xr = xl + width;
yb = yt - height;
c_mappos(xl,xr,yb,yt);
c_mapstc("OU",minstance->mptrans.outline_type);
c_mapsti("EL",minstance->mptrans.elliptical_boundary);

if(strncmp(minstance->mptrans.projection,"SV",2) == 0) {
	c_mapstr("SA",minstance->mptrans.satellite_dist);
	c_mapstr("S1",minstance->mptrans.satellite_angle_1);
	c_mapstr("S2",minstance->mptrans.satellite_angle_2);
	c_maproj(minstance->mptrans.projection,
		minstance->mptrans.center_lat,
		minstance->mptrans.center_lon,
		minstance->mptrans.center_rot);
} else if(strncmp(minstance->mptrans.projection,"LC",2) == 0) {	
	c_maproj(minstance->mptrans.projection,
		minstance->mptrans.lambert_parallel_1,
		minstance->mptrans.lambert_meridian,
		minstance->mptrans.lambert_parallel_2);
} else {
	c_maproj(minstance->mptrans.projection,
		minstance->mptrans.center_lat,
		minstance->mptrans.center_lon,
		minstance->mptrans.center_rot);
}
c_mapset(minstance->mptrans.rect_limit_type,
		minstance->mptrans.rect_limit_1,
		minstance->mptrans.rect_limit_2,
		minstance->mptrans.rect_limit_3,
		minstance->mptrans.rect_limit_4);
	c_entsr(&irold,1);
	c_mapint();
	c_nerro(&nerr);
	if(nerr > 0){	
/*
* ERROR:
*/
/*
* Need code to determine which error occured
*/
		NhlPError(NhlFATAL,NhlEUNKNOWN,"MapSetTrans: An internal EZMAP error has occured");
		return(NhlFATAL);
	} else {
		c_getset(&minstance->mptrans.map_pos_l,
			&minstance->mptrans.map_pos_r,
			&minstance->mptrans.map_pos_b,
			&minstance->mptrans.map_pos_t,
			&minstance->mptrans.ul,
			&minstance->mptrans.ur,
			&minstance->mptrans.ub,
			&minstance->mptrans.ut,
			&loglin);

		minstance->mptrans.aspect = ((minstance->mptrans.map_pos_r -
					minstance->mptrans.map_pos_l)/
					(minstance->mptrans.map_pos_t - 
					minstance->mptrans.map_pos_b));
/*
* -------->This destroys the nicely selected aspect ratio but is the only way
* to guarenty WYSIWYG display and Point and Click after aplot has been
* transformed.<-----------
*/
		minstance->mptrans.map_pos_l = xl;
		minstance->mptrans.map_pos_r = xr;
		minstance->mptrans.map_pos_t = yt;
		minstance->mptrans.map_pos_b = yb;
		c_set(xl,
			xr,
			yb,
			yt,
			minstance->mptrans.ul,
			minstance->mptrans.ur,
			minstance->mptrans.ub,
			minstance->mptrans.ut,
			loglin);
		return(NhlNOERROR);
	}
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
        int i;
	NhlErrorTypes ret = NhlNOERROR;
	float xmin,ymin,xmax,ymax;



	*status = 0;

	xmin = MIN(minstance->mptrans.ul,minstance->mptrans.ur);
	xmax = MAX(minstance->mptrans.ul,minstance->mptrans.ur);
	ymin = MIN(minstance->mptrans.ut,minstance->mptrans.ub);
	ymax = MAX(minstance->mptrans.ut,minstance->mptrans.ub);

	
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
               		strans(minstance->mptrans.ul,minstance->mptrans.ur,
                       		minstance->mptrans.ub,minstance->mptrans.ut,
                       		minstance->mptrans.map_pos_l,
				minstance->mptrans.map_pos_r,
				minstance->mptrans.map_pos_b,
				minstance->mptrans.map_pos_t,
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
	NhlErrorTypes ret = NhlNOERROR;


	*status = 0;
        for(i = 0; i< n ; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i])) 
			||((ymissing != NULL)&&(*ymissing == y[i])) 
			||(x[i] < minstance->mptrans.map_pos_l)
			||(x[i] > minstance->mptrans.map_pos_r)
			||(y[i] < minstance->mptrans.map_pos_b)
			||(y[i] < minstance->mptrans.map_pos_t)) {
	
			*status = 1;
			xout[i]=yout[i]=minstance->trobj.out_of_range;

		} else {
               	strans( minstance->mptrans.map_pos_l,
			minstance->mptrans.map_pos_r,
			minstance->mptrans.map_pos_b,
			minstance->mptrans.map_pos_t,
			minstance->mptrans.ul,minstance->mptrans.ur,
                       	minstance->mptrans.ub,minstance->mptrans.ut,
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
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	float xmin,ymin,xmax,ymax;


        xmin = MIN(minstance->mptrans.ul,minstance->mptrans.ur);
        xmax = MAX(minstance->mptrans.ul,minstance->mptrans.ur);
        ymin = MIN(minstance->mptrans.ut,minstance->mptrans.ub);
        ymax = MAX(minstance->mptrans.ut,minstance->mptrans.ub);


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
	NhlMapTransObjLayer mold = (NhlMapTransObjLayer) old;
	NhlErrorTypes ret = NhlNOERROR;
	char *tmp;

	if(_NhlArgIsSet(args,num_args,NhlNtrOutOfRangeF)) {
		NhlPError(NhlINFO,NhlEUNKNOWN,"MapTransObj: NhlOutOfRangeF should not be set, must always remain 1e12");
		mnew->trobj.out_of_range = 1e12;
	} else {	
		mnew->trobj.out_of_range = 1e12;
	}

	if(mnew->mptrans.projection != mold->mptrans.projection) {
		NhlFree(mold->mptrans.projection);
		tmp = mnew->mptrans.projection;
		mnew->mptrans.projection = (char*)NhlMalloc((unsigned)
					strlen(tmp) +1);
		strcpy(mnew->mptrans.projection,tmp);
	}
	if(mnew->mptrans.outline_type != mold->mptrans.outline_type) {
		NhlFree(mold->mptrans.outline_type);
		tmp = mnew->mptrans.outline_type;
		mnew->mptrans.outline_type = (char*)NhlMalloc((unsigned)
					strlen(tmp) +1);
		strcpy(mnew->mptrans.outline_type,tmp);
	}
	if(mnew->mptrans.rect_limit_type != mold->mptrans.rect_limit_type) {
		NhlFree(mold->mptrans.rect_limit_type);
		tmp = mnew->mptrans.rect_limit_type;
		mnew->mptrans.rect_limit_type = (char*)NhlMalloc((unsigned)
					strlen(tmp) +1);
		strcpy(mnew->mptrans.rect_limit_type,tmp);
		

		if((mnew->mptrans.rect_limit_1 == mold->mptrans.rect_limit_1)||
		(mnew->mptrans.rect_limit_1 == mold->mptrans.rect_limit_1)||
		(mnew->mptrans.rect_limit_1 == mold->mptrans.rect_limit_1)||
		(mnew->mptrans.rect_limit_1 == mold->mptrans.rect_limit_1)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Change in NhlNmpRectLimitType but no change in values");
			ret = NhlWARNING;
		} 
	}
	if(strncmp(mnew->mptrans.rect_limit_type,"PO",2) == 0) {
		mold->mptrans.rect_limit_1[0] = mnew->mptrans.rect_limit_1[0];
		mold->mptrans.rect_limit_1[1] = mnew->mptrans.rect_limit_1[1];
		mold->mptrans.rect_limit_2[0] = mnew->mptrans.rect_limit_2[0];
		mold->mptrans.rect_limit_2[1] = mnew->mptrans.rect_limit_2[1];
		mold->mptrans.rect_limit_3[0] = mnew->mptrans.rect_limit_3[0];
		mold->mptrans.rect_limit_3[1] = mnew->mptrans.rect_limit_3[1];
		mold->mptrans.rect_limit_4[0] = mnew->mptrans.rect_limit_4[0];
		mold->mptrans.rect_limit_4[1] = mnew->mptrans.rect_limit_4[1];
		mnew->mptrans.rect_limit_1 = mold->mptrans.rect_limit_1;
		mnew->mptrans.rect_limit_2 = mold->mptrans.rect_limit_2;
		mnew->mptrans.rect_limit_3 = mold->mptrans.rect_limit_3;
		mnew->mptrans.rect_limit_4 = mold->mptrans.rect_limit_4;
	} else if(strncmp(mnew->mptrans.rect_limit_type,"MA",2)!=0){
		mold->mptrans.rect_limit_1[0] = mnew->mptrans.rect_limit_1[0];
		mold->mptrans.rect_limit_2[0] = mnew->mptrans.rect_limit_2[0];
		mold->mptrans.rect_limit_3[0] = mnew->mptrans.rect_limit_3[0];
		mold->mptrans.rect_limit_4[0] = mnew->mptrans.rect_limit_4[0];
		mnew->mptrans.rect_limit_1 = mold->mptrans.rect_limit_1;
		mnew->mptrans.rect_limit_2 = mold->mptrans.rect_limit_2;
		mnew->mptrans.rect_limit_3 = mold->mptrans.rect_limit_3;
		mnew->mptrans.rect_limit_4 = mold->mptrans.rect_limit_4;
	}
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
	NhlMapTransObjLayer mreq = (NhlMapTransObjLayer) req;
	NhlErrorTypes ret = NhlNOERROR;
	float *tmp;

	if(_NhlArgIsSet(args,num_args,NhlNtrOutOfRangeF)){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"MapTransObj: NhlOutOfRangeF should not be set, must always remain 1e12");
		mnew->trobj.out_of_range = 1e12;
	} else {	
		mnew->trobj.out_of_range = 1e12;
	}

	mnew->mptrans.projection = (char*)NhlMalloc((unsigned)
					strlen(mreq->mptrans.projection)+1);
	strcpy(mnew->mptrans.projection,mreq->mptrans.projection);
	
	mnew->mptrans.outline_type = (char*)NhlMalloc((unsigned)
					strlen(mreq->mptrans.outline_type)+1);
	strcpy(mnew->mptrans.outline_type,mreq->mptrans.outline_type);

	mnew->mptrans.rect_limit_type = (char*)NhlMalloc((unsigned)
				       strlen(mreq->mptrans.rect_limit_type)+1);
	strcpy(mnew->mptrans.rect_limit_type,mreq->mptrans.rect_limit_type);

	
	if(strncmp(mnew->mptrans.rect_limit_type,"MA",2)==0) {
		mnew->mptrans.rect_limit_1 = (float*)NhlMalloc((unsigned)	
				sizeof(float)*2);
		mnew->mptrans.rect_limit_2 = (float*)NhlMalloc((unsigned)	
				sizeof(float)*2);
		mnew->mptrans.rect_limit_3 = (float*)NhlMalloc((unsigned)	
				sizeof(float)*2);
		mnew->mptrans.rect_limit_4 = (float*)NhlMalloc((unsigned)	
				sizeof(float)*2);
	} else
	if(strncmp(mnew->mptrans.rect_limit_type,"PO",2) == 0) {
/*
* Only want to allocate these once. so I should make sure SetValues doesn't
*/
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_1[0];
		tmp[1] = mnew->mptrans.rect_limit_1[1];
		mnew->mptrans.rect_limit_1 = tmp;
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_2[0];
		tmp[1] = mnew->mptrans.rect_limit_2[1];
		mnew->mptrans.rect_limit_2 = tmp;
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_3[0];
		tmp[1] = mnew->mptrans.rect_limit_3[1];
		mnew->mptrans.rect_limit_3 = tmp;
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_4[0];
		tmp[1] = mnew->mptrans.rect_limit_4[1];
		mnew->mptrans.rect_limit_4 = tmp;
	} else {
/*
* Allocate 2 spaces even though now only one is being used
* may need both later on.
*/
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_1[0];
		mnew->mptrans.rect_limit_1 = tmp;
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_2[0];
		mnew->mptrans.rect_limit_2 = tmp;
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_3[0];
		mnew->mptrans.rect_limit_3 = tmp;
		tmp = (float*)NhlMalloc((unsigned) sizeof(float)*2);
		tmp[0] = mnew->mptrans.rect_limit_4[0];
		mnew->mptrans.rect_limit_4 = tmp;
	}

	return(ret);
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


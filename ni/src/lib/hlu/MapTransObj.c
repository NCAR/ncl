
/*
 *      $Id: MapTransObj.c,v 1.1 1993-04-30 17:22:52 boote Exp $
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
		NhlOffset(MapTransObjLayerRec,mptrans.projection),
		NhlTString,"CE" },
	{ NhlNmpOutlineType, NhlCmpOutlineType, NhlTString, sizeof(char*),
		NhlOffset(MapTransObjLayerRec,mptrans.outline_type),
		NhlTString,"PS" },
	{ NhlNmpCenterLatF, NhlCmpCenterLatF, NhlTFloat, sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.center_lat),
		NhlTString,"0.0"},
	{ NhlNmpCenterLonF, NhlCmpCenterLonF, NhlTFloat, sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.center_lon),
		NhlTString,"0.0"},
	{ NhlNmpCenterRotF, NhlCmpCenterRotF, NhlTFloat, sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.center_rot),
		NhlTString,"0.0"},
	{ NhlNmpRectLimitType, NhlCmpRectLimitType, NhlTString, sizeof(char*),
		NhlOffset(MapTransObjLayerRec,mptrans.rect_limit_type),
		NhlTString,"MA"},
	{ NhlNmpRectLimit1, NhlCmpRectLimit1, NhlTFloatPtr, sizeof(float*),
		NhlOffset(MapTransObjLayerRec,mptrans.rect_limit_1),
		NhlTFloatPtr,NULL},
	{ NhlNmpRectLimit2, NhlCmpRectLimit2, NhlTFloatPtr, sizeof(float*),
		NhlOffset(MapTransObjLayerRec,mptrans.rect_limit_2),
		NhlTFloatPtr,NULL},
	{ NhlNmpRectLimit3, NhlCmpRectLimit3, NhlTFloatPtr, sizeof(float*),
		NhlOffset(MapTransObjLayerRec,mptrans.rect_limit_3),
		NhlTFloatPtr,NULL},
	{ NhlNmpRectLimit4, NhlCmpRectLimit4, NhlTFloatPtr, sizeof(float*),
		NhlOffset(MapTransObjLayerRec,mptrans.rect_limit_4),
		NhlTFloatPtr,NULL},
	{ NhlNmpLambertParallel1F, NhlCmpLambertParallel1F, NhlTFloat, 
		sizeof(float), 
		NhlOffset(MapTransObjLayerRec,mptrans.lambert_parallel_1),
		NhlTString,".001"},
	{ NhlNmpLambertParallel2F, NhlCmpLambertParallel2F, NhlTFloat, 
		sizeof(float), 
		NhlOffset(MapTransObjLayerRec,mptrans.lambert_parallel_2),
		NhlTString,"89.999"},
	{ NhlNmpLambertMeridianF,NhlCmpLambertMeridianF,NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.lambert_meridian),
		NhlTString,"0.0"},
	{ NhlNmpSatelliteDistF,NhlCmpSatelliteDistF,NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.satellite_dist),
		NhlTString,"1"},
	{ NhlNmpSatelliteAngle1F,NhlCmpSatelliteAngle1F,NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.satellite_angle_1),
		NhlTString,"0.0"},
	{ NhlNmpSatelliteAngle2F,NhlCmpSatelliteAngle2F,NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.satellite_angle_2),
		NhlTString,"0.0"},
	{ NhlNmpSatelliteAngle2F,NhlCmpSatelliteAngle2F,NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.satellite_angle_2),
		NhlTString,"0.0"},
	{ NhlNmpEllipticalBoundary,NhlCmpEllipticalBoundary,NhlTInteger,
		sizeof(int),
		NhlOffset(MapTransObjLayerRec,mptrans.elliptical_boundary),
		NhlTString,"0" }
/* not sure these are needed,
	{ NhlNmpMapPosLF, NhlCmpMapPosLF, NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.map_pos_l),
		NhlTString,"0.05" },
	{ NhlNmpMapPosRF, NhlCmpMapPosRF, NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.map_pos_r),
		NhlTString,"0.95" },
	{ NhlNmpMapPosTF, NhlCmpMapPosTF, NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.map_pos_t),
		NhlTString,"0.95" },
	{ NhlNmpMapPosTF, NhlCmpMapPosTF, NhlTFloat,sizeof(float),
		NhlOffset(MapTransObjLayerRec,mptrans.map_pos_t),
		NhlTString,"0.0" }
*/
};

/*
* Base Methods defined here
*/

static NhlErrorTypes  MapTransSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes MapTransInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

/*
* TransObjClass Methods defined
*/

static NhlErrorTypes MapSetTrans(
#ifdef NhlNeedProto
Layer   /*instance*/,
Layer   /*parent */
#endif
);

static NhlErrorTypes MapWinToNDC(
#ifdef NhlNeedProto
Layer   /*instance*/,
Layer   /* parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*  /*xmissing*/,
float*  /*ymissing*/
#endif
);
static NhlErrorTypes MapNDCToWin(
#ifdef NhlNeedProto
Layer   /*instance*/,
Layer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*  /*xmissing*/,
float*  /*ymissing*/
#endif
);


static NhlErrorTypes MapDataToWin(
#ifdef NhlNeedProto 
Layer   /*instance */, 
Layer   /*parent */, 
float*  /*x*/, 
float*   /*y*/, 
int     /* n*/, 
float*  /*xout*/, 
float*  /*yout*/ ,
float*  /*xmissing*/,
float*  /*ymissing*/
#endif 
); 

static NhlErrorTypes MapWinToData(
#ifdef NhlNeedProto 
Layer   /*instance */, 
Layer   /*parent */, 
float*  /*x*/, 
float*   /*y*/, 
int     /* n*/, 
float*  /*xout*/, 
float*  /*yout*/ ,
float*  /*xmissing*/,
float*  /*ymissing*/
#endif 
); 


MapTransObjLayerClassRec mapTransObjLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&transObjLayerClassRec,
/* class_name			*/	"MapTransObj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(MapTransObjLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	MapTransInitialize,
/* layer_set_values		*/	MapTransSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	NULL
        },
        {
/*
* For Maps Compc and Data are the same hence the following definitions.
*/
/* set_trans		*/	MapSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	MapWinToNDC,
/* ndc_to_win		*/	MapNDCToWin,
/* data_to_win		*/	MapDataToWin, 
/* win_to_data		*/	MapWinToData,
/* data_to_compc	*/	NULL,
/* compc_to_data	*/	NULL,
/* win_to_compc		*/	MapWinToData,
/* compc_to_win		*/	MapDataToWin
        }
};

LayerClass mapTransObjLayerClass = (LayerClass) &mapTransObjLayerClassRec;

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
( Layer instance,Layer parent )
#else
(instance,parent)
	Layer instance;
	Layer parent;
#endif
{
	float xl;
	float yt;
	float width,xr;
	float height,yb;
	int irold,nerr,loglin;
	MapTransObjLayer minstance = (MapTransObjLayer)instance;
	
/*
* C bindings don't work for maps yet
*/
	NhlGetValues(parent->base.id,
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
		NhlPError(FATAL,E_UNKNOWN,"MapSetTrans: An internal EZMAP error has occured");
		return(FATAL);
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
		return(NOERROR);
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
( Layer instance,Layer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing)
	Layer instance;
	Layer parent;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
#endif
{
        MapTransObjLayer minstance = (MapTransObjLayer)instance;
        int i;
	NhlErrorTypes ret = NOERROR;
	int xmis=0; int ymis=0;

/*
	ret = MapSetTrans(instance,parent);
	if(ret < WARNING)
		return(ret);
*/

	if((xmissing ==NULL)&&(ymissing == NULL)) {
        	for(i = 0; i< n ; i++) {
                	strans(minstance->mptrans.ul,minstance->mptrans.ur,
                        	minstance->mptrans.ub,minstance->mptrans.ut,
                        	minstance->mptrans.map_pos_l,
				minstance->mptrans.map_pos_r,
				minstance->mptrans.map_pos_b,
				minstance->mptrans.map_pos_t,
				x[i],y[i],
                        	&(xout[i]),&(yout[i]));
	        }
	} else {
        	for(i = 0; i< n ; i++) {
			if((xmissing != NULL)&&(*xmissing == x[i]))
				xmis = 1;
			if((ymissing != NULL)&&(*ymissing == y[i]))
				ymis = 1;
                	strans(minstance->mptrans.ul,minstance->mptrans.ur,
                        	minstance->mptrans.ub,minstance->mptrans.ut,
                        	minstance->mptrans.map_pos_l,
				minstance->mptrans.map_pos_r,
				minstance->mptrans.map_pos_b,
				minstance->mptrans.map_pos_t,
				x[i],y[i],
                        	&(xout[i]),&(yout[i]));
			if(xmis) {
				xmis = 0;
				xout[i] = *xmissing;
			}
			if(ymis) {
				ymis = 0;
				yout[i] = *ymissing;
			}

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
( Layer instance,Layer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing)
	Layer instance;
	Layer parent;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
#endif
{
        int i;
        MapTransObjLayer minstance = (MapTransObjLayer)instance;
	NhlErrorTypes ret = NOERROR;
	int xmis=0;int ymis= 0;


/*
	ret = MapSetTrans(instance,parent);
	if(ret < WARNING)
		return(ret);
*/

	if((xmissing == NULL)&&(ymissing == NULL)) {
        	for(i = 0; i< n ; i++) {
                	strans( minstance->mptrans.map_pos_l,
				minstance->mptrans.map_pos_r,
				minstance->mptrans.map_pos_b,
				minstance->mptrans.map_pos_t,
				minstance->mptrans.ul,minstance->mptrans.ur,
                        	minstance->mptrans.ub,minstance->mptrans.ut,
				x[i],y[i],
                        	&(xout[i]),&(yout[i]));
        	}
	} else {
        	for(i = 0; i< n ; i++) {
			if((xmissing != NULL)&&(*xmissing == x[i])) 
				xmis = 1;
			if((ymissing != NULL)&&(*ymissing == y[i])) 
				ymis = 1;
                	strans( minstance->mptrans.map_pos_l,
				minstance->mptrans.map_pos_r,
				minstance->mptrans.map_pos_b,
				minstance->mptrans.map_pos_t,
				minstance->mptrans.ul,minstance->mptrans.ur,
                        	minstance->mptrans.ub,minstance->mptrans.ut,
				x[i],y[i],
                        	&(xout[i]),&(yout[i]));
			if(xmis) {
				xmis = 0;
				xout[i] = *xmissing;
			}
			if(ymis) {
				ymis = 0;
				yout[i] = *ymissing;
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
( Layer instance,Layer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing)
	Layer instance;
	Layer parent;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
#endif
{
	int i;
	NhlErrorTypes ret = NOERROR;
	int xmis=0; int ymis = 0;
/*
	ret = MapSetTrans(instance,parent);
	if(ret < WARNING)
		return(ret);
*/
	

	if((xmissing == NULL)&&(ymissing == NULL)) {
		for( i = 0; i< n; i++) {
/*
* Lat is y and Lon is X
*/
			c_maptra(y[i],x[i],&(xout[i]),&(yout[i]));
		}
	}else {
		for( i = 0; i< n; i++) {
			if((xmissing != NULL) &&(*xmissing == x[i]))
				xmis = 1;
			if((ymissing != NULL) &&(*ymissing == y[i]))
				ymis = 1;
			c_maptra(y[i],x[i],&(xout[i]),&(yout[i]));
			if(xmis) {
				xmis = 0;
				xout[i] = *xmissing;
			}
			if(ymis) {
				ymis = 0;
				yout[i] = *ymissing;
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
( Layer instance,Layer parent, float* x,float* y,int n,float* xout,float* yout,float *xmissing, float *ymissing)
#else
(instance,parent,x,y,n,xout,yout,xmissing,ymissing)
	Layer instance;
	Layer parent;
	float *x;
	float *y;
	int n;
	float *xout;
	float *yout;
	float *xmissing;
	float *ymissing;
#endif
{
	int i;
	NhlErrorTypes ret = NOERROR;
	int xmis = 0; int ymis=0;
/*
	ret = MapSetTrans(instance,parent);
	if(ret < WARNING)
		return(ret);
*/
	if((xmissing == NULL)&&(ymissing == NULL)) {	
		for(i=0; i< n; i++) {
			c_maptri(x[i],y[i],&(yout[i]),&(xout[i]));
		}
	} else {
		for(i=0; i< n; i++) {
			if((xmissing != NULL)&&(*xmissing == x[i]))
				xmis = 1; 
			if((ymissing != NULL)&&(*ymissing == y[i]))
				ymis = 1; 
			c_maptri(x[i],y[i],&(yout[i]),&(xout[i]));
			if(xmis){
				xmis =0;
				xout[i] = *xmissing;
			}
			if(ymis){
				ymis =0;
				yout[i] = *ymissing;
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
(Layer old,Layer reference,Layer new,_NhlArgList args,int num_args)
#else
(old,reference,new,args,num_args)
	Layer	old;
	Layer	reference;
	Layer	new;
	_NhlArgList args;
	int	num_args;
#endif
{
	MapTransObjLayer mnew = (MapTransObjLayer) new;
	MapTransObjLayer mold = (MapTransObjLayer) old;
	NhlErrorTypes ret = NOERROR;
	char *tmp;

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
			NhlPError(WARNING,E_UNKNOWN,"Change in NhlNmpRectLimitType but no change in values");
			ret = WARNING;
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
( LayerClass class,Layer req,Layer new,_NhlArgList args,int num_args)
#else
(class,req,new,args,num_args)
	LayerClass class;
	Layer	req;
	Layer	new;
	_NhlArgList args;
	int 	num_args;
#endif
{
	MapTransObjLayer mnew = (MapTransObjLayer) new;
	MapTransObjLayer mreq = (MapTransObjLayer) req;
	NhlErrorTypes ret = NOERROR;
	float *tmp;

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

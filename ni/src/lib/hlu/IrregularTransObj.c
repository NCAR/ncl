/*
 *      $Id: IrregularTransObj.c,v 1.6 1994-01-27 21:23:15 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularTransObj.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 19 09:33:44 MDT 1992
 *
 *	Description:	Takes one coordinate system and maps it into another
 *			Both coordinate systems must be monotonic. This will
 *			be used by contour, etc to map integer grid point
 *			indexes to irregularly spaced points so they can then
 *			be drawn on the linear viewport. Traditionally a
 *			funtion CPSPS2 has been used to interpolate the 
 *			irregular grids onto regular grids. This is not needed 
 *			since conpack can contour using the integer grids and 
 *			use this irregular transformation object to map from the
 *			integer coordinate system to the irregular one.
 *
 *			Eventually the methods here will be called from within
 *			the CPMPXY function used by CONPACK and eventually by
 *			the rest of the 2D utilities.
 *
 *NOTE--------->	Transformation object that is similar to this
 *			one is one that will truely provide an irregular 
 *			mapping transformation. Conpack will graph its contours
 *			as uniformly spaced points but the tick marks will 
 *			be irregular. Hence the appearance of a irregular 
 *			DATA==>VIEWPORT transformation.
 *
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/IrregularTransObjP.h>
#include <ncarg/hlu/View.h>

static NhlResource resources[] = {
	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_coord_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrXInterPoints,NhlCtrXInterPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_inter_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrXMaxF, NhlCtrXMaxF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_max),
                NhlTString, "0.0" },
        { NhlNtrXMinF, NhlCtrXMinF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_min),
                NhlTString, "0.0" },
	{ NhlNtrXNumPoints,NhlCtrXNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_num_points),
		NhlTString,"0" },
	{ NhlNtrXReverse, NhlCtrXReverse, NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_reverse),
		NhlTString,"0" },
	{ NhlNtrXTensionF, NhlCtrXTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_tension),
		NhlTString,"2.0" },
	{ NhlNtrXSamples, NhlCtrXSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_samples),
		NhlTString,"9" },
	{ NhlNtrXUseLog, NhlCtrXUseLog, NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.x_use_log),
		NhlTString,"0" },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_coord_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrYInterPoints,NhlCtrYInterPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_inter_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrYMaxF, NhlCtrYMaxF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_max),
                NhlTString, "0.0" },
        { NhlNtrYMinF, NhlCtrYMinF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_min),
                NhlTString, "0.0" },
	{ NhlNtrYNumPoints,NhlCtrYNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_num_points),
		NhlTString,"0" },
	{ NhlNtrYReverse, NhlCtrYReverse, NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_reverse),
		NhlTString,"0" },
	{ NhlNtrYTensionF, NhlCtrYTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_tension),
		NhlTString,"2.0" },
	{ NhlNtrYSamples, NhlCtrYSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_samples),
		NhlTString,"9" },
	{ NhlNtrYUseLog, NhlCtrYUseLog, NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularTransObjLayerRec,irtrans.y_use_log),
		NhlTString,"0" }
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  IrTransSetValues(
#ifdef NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes IrTransInitialize(
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

static NhlErrorTypes IrSetTrans(
#ifdef NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);

static NhlErrorTypes IrWinToNDC(
#ifdef NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer	/* parent */,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float* /*xmissing*/,
float* /*ymissing*/,
int*	/* status */
#endif
);


static NhlErrorTypes IrNDCToWin(
#ifdef NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer	/*parent */,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float* /*xmissing*/,
float* /*ymissing*/,
int*  /* status */
#endif
);


static NhlErrorTypes IrDataToCompc(
#ifdef NhlNeedProto
NhlLayer   /*instance */,
NhlLayer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float* /*xmissing*/,
float* /*ymissing*/,
int*  /* status */
#endif
);

static NhlErrorTypes IrCompcToData(
#ifdef NhlNeedProto
NhlLayer   /*instance */,
NhlLayer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float* /*xmissing*/,
float* /*ymissing*/,
int*  /* status */
#endif
);

static NhlErrorTypes IrDataToWin(
#ifdef NhlNeedProto
NhlLayer   /*instance */,
NhlLayer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float* /*xmissing*/,
float* /*ymissing*/,
int*  /* status */
#endif
);

static NhlErrorTypes IrNDCLineTo(
#if 	NhlNeedProto
NhlLayer	/* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrDataLineTo(
#if 	NhlNeedProto
NhlLayer	/* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrCompcLineTo(
#if 	NhlNeedProto
NhlLayer	/* instance */,
NhlLayer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

#define CREATE  1
#define SET 0

NhlIrregularTransObjLayerClassRec NhlirregularTransObjLayerClassRec = {
        {
/* class_name			*/	"IrregularTransObj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlIrregularTransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)
						&NhltransObjLayerClassRec,
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	IrTransInitialize,
/* layer_set_values		*/	IrTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL
        },
        {
/* set_trans		*/	IrSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	IrWinToNDC,
/* ndc_to_win		*/	IrNDCToWin,
/* data_to_win		*/	IrDataToWin, 
/* win_to_data		*/	IrDataToWin,  /* data and window are ident */
/* data_to_compc	*/	IrDataToCompc,
/* compc_to_data	*/	IrCompcToData,
/* win_to_compc		*/	IrDataToCompc,/* data and window are ident*/
/* compc_to_win		*/	IrCompcToData, /* data and windo are ident */
/* data_lineto		*/	IrDataLineTo,
/* compc_lineto		*/	IrCompcLineTo,
/* win_lineto		*/	IrDataLineTo,
/* NDC_lineto		*/	IrNDCLineTo
        }
};

NhlLayerClass NhlirregularTransObjLayerClass =
			(NhlLayerClass)&NhlirregularTransObjLayerClassRec;

static SetUpTrans
#if __STDC__
(NhlLayer new, NhlLayer old, int c_or_s,_NhlArgList args, int nargs)
#else
(new,old,c_or_s,args, nargs)
        NhlLayer   new;
        NhlLayer   old;
        int c_or_s;
        _NhlArgList args;
        int nargs;
#endif
{
	NhlIrregularTransObjLayer inew = (NhlIrregularTransObjLayer)new;
	NhlIrregularTransObjLayer iold = (NhlIrregularTransObjLayer)old;
	NhlErrorTypes ret = NhlNOERROR;
	float tmp;
	float *tmpptr;
	NhlStatus xstatus,ystatus;
	char buffer[80];
	char *error_lead;
	int call_spline_create;
	NhlErrorTypes ret1 = NhlNOERROR;

	if(c_or_s == SET) {
		error_lead = "IrTransSetValues";
		call_spline_create = 0;
	} else {
		error_lead = "IrTransInitialize";
		call_spline_create = 1;
	}


	if((inew->irtrans.x_coord_points == NULL)||
			(inew->irtrans.y_coord_points==NULL)||
			(inew->irtrans.x_num_points < 2)||
			(inew->irtrans.y_num_points < 2)) {
		if(c_or_s == CREATE ) {
		sprintf(buffer, "%s: Not enough information to set up transformations",error_lead);
		NhlPError(NhlFATAL,NhlEUNKNOWN,buffer);
			
		return(NhlFATAL);
		} else {
			memcpy((char*)&inew->irtrans,(char*)&iold->irtrans,
                                sizeof(NhlIrregularTransObjLayerPart));
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Not enough information to set up transformations, reseting to previous values",error_lead);
                        return(NhlWARNING);
		}
	} else {
		if(c_or_s == SET) {
			if(_NhlArgIsSet(args,nargs,NhlNtrXCoordPoints)) {
				call_spline_create = 1;
				if(iold->irtrans.x_coord_points != NULL) {
					NhlFree(iold->irtrans.x_coord_points);
				}
				tmpptr = inew->irtrans.x_coord_points;
				inew->irtrans.x_coord_points 
					= (float*)NhlMalloc((unsigned)
					sizeof(float) * (inew->irtrans.x_num_points));
				memcpy((char*)inew->irtrans.x_coord_points,
					(char*)tmpptr,sizeof(float)*inew->irtrans.x_num_points);
			}
			if(_NhlArgIsSet(args,nargs,NhlNtrYCoordPoints)) {
				call_spline_create = 1;
				if(iold->irtrans.y_coord_points != NULL) {
					NhlFree(iold->irtrans.y_coord_points);
				}
				tmpptr = inew->irtrans.y_coord_points;
				inew->irtrans.y_coord_points 
					= (float*)NhlMalloc((unsigned)
					sizeof(float) * (inew->irtrans.y_num_points));
				memcpy((char*)inew->irtrans.y_coord_points,
					(char*)tmpptr,sizeof(float)*inew->irtrans.y_num_points);
			}
		} else {
			tmpptr = inew->irtrans.x_coord_points;
			inew->irtrans.x_coord_points = (float*)NhlMalloc((unsigned)
					sizeof(float) *(inew->irtrans.x_num_points));
			memcpy((char*)inew->irtrans.x_coord_points,
				(char*)tmpptr,
				sizeof(float)*inew->irtrans.x_num_points);

			tmpptr = inew->irtrans.y_coord_points;
			inew->irtrans.y_coord_points = (float*)NhlMalloc((unsigned)
					sizeof(float) *(inew->irtrans.y_num_points));
			memcpy((char*)inew->irtrans.y_coord_points,
				(char*)tmpptr,
				sizeof(float)*inew->irtrans.y_num_points);

		}
		if((inew->irtrans.x_min == 0.0)&&
                        (inew->irtrans.x_max == 0.0)) {
                        inew->irtrans.x_min = MIN(inew->irtrans.x_coord_points[0],inew->irtrans.x_coord_points[inew->irtrans.x_num_points-1]);
                        inew->irtrans.x_max = MAX(inew->irtrans.x_coord_points[0],inew->irtrans.x_coord_points[inew->irtrans.x_num_points-1]);
                } else if(inew->irtrans.x_min > inew->irtrans.x_max) {
                        tmp = inew->irtrans.x_min;
                        inew->irtrans.x_min = inew->irtrans.x_max;
                        inew->irtrans.x_max = tmp;
                }
                if(inew->irtrans.x_min < MIN(inew->irtrans.x_coord_points[0],inew->irtrans.x_coord_points[inew->irtrans.x_num_points-1])) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Minimum value is less than minimum value of coordinate points array, resetting to minimum",error_lead);
                        inew->irtrans.x_min = MIN(inew->irtrans.x_coord_points[0],inew->irtrans.x_coord_points[inew->irtrans.x_num_points-1]);
                }
                if(inew->irtrans.x_max > MAX(inew->irtrans.x_coord_points[0],inew->irtrans.x_coord_points[inew->irtrans.x_num_points-1])) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Maximum value is greater than maximum value of coordinate points array, resetting to maximum",error_lead);
                        inew->irtrans.x_max = MAX(inew->irtrans.x_coord_points[0],inew->irtrans.x_coord_points[inew->irtrans.x_num_points-1]);
                }
                if((inew->irtrans.y_min == 0.0)&&
                        (inew->irtrans.y_max == 0.0)) {
                        inew->irtrans.y_min = MIN(inew->irtrans.y_coord_points[0],inew->irtrans.y_coord_points[inew->irtrans.y_num_points-1]);
                        inew->irtrans.y_max = MAX(inew->irtrans.y_coord_points[0],inew->irtrans.y_coord_points[inew->irtrans.y_num_points-1]);
                } else if(inew->irtrans.y_min > inew->irtrans.y_max) {
                        tmp = inew->irtrans.y_min;
                        inew->irtrans.y_min = inew->irtrans.y_max;
                        inew->irtrans.y_max = tmp;
                }
                if(inew->irtrans.y_min < MIN(inew->irtrans.y_coord_points[0],inew->irtrans.y_coord_points[inew->irtrans.y_num_points-1])) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Minimum value is less than minimum value of coordinate points array, resetting to minimum",error_lead);
                        inew->irtrans.y_min = MIN(inew->irtrans.y_coord_points[0],inew->irtrans.y_coord_points[inew->irtrans.y_num_points-1]);
                }
                if(inew->irtrans.y_max > MAX(inew->irtrans.y_coord_points[0],inew->irtrans.y_coord_points[inew->irtrans.y_num_points-1])) {
                        NhlPError(NhlWARNING,NhlEUNKNOWN,"%s: Maximum value is greater than maximum value of coordinate points array, resetting to maximum",error_lead);
                        inew->irtrans.y_max = MAX(inew->irtrans.y_coord_points[0],inew->irtrans.y_coord_points[inew->irtrans.y_num_points-1]);
		}
	}
	if(_NhlArgIsSet(args,nargs,NhlNtrXInterPoints)) {
		if((c_or_s==SET) &&(iold->irtrans.x_inter_points != NULL)) {
			NhlFree(iold->irtrans.x_inter_points);
		}
		tmpptr = inew->irtrans.x_inter_points;
		inew->irtrans.x_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (inew->irtrans.x_num_points));
		memcpy((char*)inew->irtrans.x_inter_points,
			(char*)tmpptr,
			sizeof(float)*inew->irtrans.x_num_points);
		call_spline_create = 1;
	} 
	if((inew->irtrans.y_inter_points != NULL)) { 
		if((c_or_s==SET) &&(iold->irtrans.x_inter_points != NULL)) {
			NhlFree(iold->irtrans.x_inter_points);
		}
		tmpptr = inew->irtrans.y_inter_points;
		inew->irtrans.y_inter_points = 
			(float*)NhlMalloc((unsigned) sizeof(float) 
			* (inew->irtrans.y_num_points)); 
		memcpy((char*)inew->irtrans.y_inter_points, 
			(char*)inew->irtrans.y_inter_points,
			sizeof(float)*inew->irtrans.y_num_points);
		call_spline_create = 1;
	} 

	inew->irtrans.ul = inew->irtrans.x_min;
	inew->irtrans.ur = inew->irtrans.x_max;
	inew->irtrans.ub = inew->irtrans.y_min;
	inew->irtrans.ut = inew->irtrans.y_max;

	if(inew->irtrans.x_reverse) {
		tmp = inew->irtrans.ur;
		inew->irtrans.ur = inew->irtrans.ul;
		inew->irtrans.ul = tmp;
	}
	if(inew->irtrans.y_reverse) {
		tmp = inew->irtrans.ut;
		inew->irtrans.ut = inew->irtrans.ub;
		inew->irtrans.ub = tmp;
	}
	if((c_or_s == SET)&&(inew->irtrans.y_tension !=
                iold->irtrans.y_tension)) {
                call_spline_create = 1;
        }
        if((c_or_s == SET)&&(inew->irtrans.x_tension !=
                iold->irtrans.x_tension)) {
                call_spline_create = 1;
        }

	if(call_spline_create ) {
		ret = _NhlCreateSplineCoordApprox(&(inew->irtrans.thecoord),
			inew->irtrans.x_use_log,
			inew->irtrans.x_coord_points,
			inew->irtrans.x_inter_points,
			inew->irtrans.x_num_points,
			inew->irtrans.y_use_log,
			inew->irtrans.y_coord_points,
			inew->irtrans.y_inter_points,
			inew->irtrans.y_num_points,
			inew->irtrans.x_tension,inew->irtrans.y_tension,
			inew->irtrans.x_samples,inew->irtrans.y_samples,
			&xstatus,&ystatus);
	}
	ret1 = _NhlEvalSplineCoordForward(&(inew->irtrans.thecoord),
                        inew->irtrans.x_min,inew->irtrans.y_min,
			&inew->irtrans.compc_x_min,
			&inew->irtrans.compc_y_min,NULL,NULL);
	(void) _NhlEvalSplineCoordForward(&(inew->irtrans.thecoord),
                        inew->irtrans.x_max,inew->irtrans.y_max,
			&inew->irtrans.compc_x_max,
			&inew->irtrans.compc_y_max,NULL,NULL);

	if(inew->irtrans.compc_x_max < inew->irtrans.compc_x_min) {
		tmp = inew->irtrans.compc_x_max;
		inew->irtrans.compc_x_max = inew->irtrans.compc_x_min;
		inew->irtrans.compc_x_min = tmp;
	}
	if(inew->irtrans.compc_y_max < inew->irtrans.compc_y_min) {
		tmp = inew->irtrans.compc_y_max;
		inew->irtrans.compc_y_max = inew->irtrans.compc_y_min;
		inew->irtrans.compc_y_min = tmp;
	}

	return(MIN(ret,ret1));
}

/*
 * Function:	IrTransSetValues
 *
 * Description:	SetValues method for IrregularTrans Objects
 *
 * In Args: 	All standard ones for set_values method.
 *
 * Out Args:	Same as all set_values methods;
 *
 * Return Values: Error status
 *
 * Side Effects: Allocates and copies space for array resources
 */
/*ARGSUSED*/
static NhlErrorTypes IrTransSetValues
#if __STDC__
(NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args, int num_args)
#else
(old,reference, new,args,num_args)
	NhlLayer old;
	NhlLayer reference;
	NhlLayer new;
	_NhlArgList args;
	int	num_args;
#endif
{
	return(SetUpTrans(new,old,SET,args,num_args));
}



/*
 * Function:	IrTransInitialize
 *
 * Description: Initialize function for IrregularTransObjs. Performs same
 *		operations as set_values for copying array resources
 *
 * In Args:  	Standard layer_initialize arguments.
 *
 * Out Args:	Standard layer_initialize output.
 *
 * Return Values: Error Status
 *
 * Side Effects: allocates space and copies valus of array resources.
 */
/*ARGSUSED*/
static NhlErrorTypes IrTransInitialize
#if __STDC__
( NhlLayerClass class, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        NhlLayerClass	class;
        NhlLayer		req;
        NhlLayer		new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	return(SetUpTrans(new,NULL,CREATE,args,num_args));
}


/*
 * Function:	IrSetTrans
 *
 * Description: set_trans method for IrregularTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the IrregularTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes IrSetTrans
#if __STDC__
(NhlLayer instance, NhlLayer parent) 
#else
(instance, parent)
NhlLayer   instance;
NhlLayer   parent;
#endif
{
	float x;
	float y;
	float width;
	float height;
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	NhlErrorTypes ret;
	

	ret = NhlVAGetValues(parent->base.id,
		NhlNvpXF,&x,
		NhlNvpYF,&y,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if(ret < NhlWARNING) {
		return(ret);
	}
	
		
		

	c_set(x,x+width,y-height,y,iinstance->irtrans.ul,iinstance->irtrans.ur,
		iinstance->irtrans.ub,iinstance->irtrans.ut,1);

	
	return(NhlNOERROR);
	
}


/*
 * Function:	IrWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *		Also replaces any input values that are out of the window with
 *		the value of its out_of_range fields and sets status to true.
 *
 * In Args:	instance is the IrregularTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes IrWinToNDC
#if  __STDC__
(NhlLayer instance,NhlLayer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int * status)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer   instance;
	NhlLayer   parent;
	float   *x;
	float   *y;
	int	n;
	float*  xout;
	float*  yout;
	float*  xmissing;	
	float*  ymissing;
	int*	status;
#endif
{
	float x0;
	float y0;
	float width;
	float height;
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	int i;
	NhlErrorTypes ret;

	*status = 0;	
	
	ret= NhlVAGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if(ret < NhlWARNING) {
		return(ret);
	}
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n ; i++) {
/*
* Ok to use min/max fields since Window and Data are the same
*/
			if((x[i] > iinstance->irtrans.x_max)
				||(x[i] < iinstance->irtrans.x_min)
				||(y[i] > iinstance->irtrans.y_max)
				||(y[i] < iinstance->irtrans.y_min)) {

				*status = 1;
				xout[i]=yout[i]=iinstance->trobj.out_of_range;
			} else {
				strans(iinstance->irtrans.ul,
					iinstance->irtrans.ur,
					iinstance->irtrans.ub,
					iinstance->irtrans.ut,
					 x0,x0+width,y0-height,y0,x[i],y[i],
					&(xout[i]),&(yout[i]));

			}
		}
	} else {	
		for(i = 0; i< n ; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
				||((ymissing != NULL)&&(*ymissing == y[i]))
				||(x[i] > iinstance->irtrans.x_max)
				||(x[i] < iinstance->irtrans.x_min)
				||(y[i] > iinstance->irtrans.y_max)
				||(y[i] < iinstance->irtrans.y_min)) {

				*status = 1;
                                xout[i]=yout[i]=iinstance->trobj.out_of_range;
			} else {

				strans(iinstance->irtrans.ul,
					iinstance->irtrans.ur,
					iinstance->irtrans.ub,
					iinstance->irtrans.ut,
					 x0,x0+width,y0-height,y0,x[i],y[i],
					&(xout[i]),&(yout[i]));
			}
		}
	}

	return(NhlNOERROR);
}


/*
 * Function:	IrNDCToWin
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
static NhlErrorTypes IrNDCToWin
#if  __STDC__
(NhlLayer instance,NhlLayer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer   instance;
	NhlLayer   parent;
	float   *x;
	float   *y;
	int	n;
	float*  xout;
	float*  yout;
	float*  xmissing;
	float*  ymissing;
	int * status;
#endif
{
	float x0;
	float y0;
	float width;
	int i;
	float height;
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	NhlErrorTypes ret;
	

	*status = 0;	
	ret = NhlVAGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if( ret < NhlWARNING)
		return(ret);

	if((xmissing == NULL) &&(ymissing == NULL)) {	
		for(i = 0; i< n; i++) {
			if((x[i] < x0)
				||(x[i] > x0 + width)
				||(y[i] < y0 - height)
				||(y[i] > y0)) {
			
				*status = 1;	
				xout[i]=yout[i]=iinstance->trobj.out_of_range;
			} else {
	
			strans(x0,x0+width,y0-height,y0,iinstance->irtrans.ul,
				iinstance->irtrans.ur, iinstance->irtrans.ub,
				iinstance->irtrans.ut, x[i],y[i],
				&(xout[i]),&(yout[i]));

			}
		}
	} else {
		for(i = 0; i< n; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
				|| ((ymissing != NULL)&&(*ymissing == y[i]))
				||(x[i] < x0)
                                ||(x[i] > x0 + width)
                                ||(y[i] < y0 - height)
                                ||(y[i] > y0)) {

				*status = 1;	
				xout[i]=yout[i]=iinstance->trobj.out_of_range;
			} else {
			strans(x0,x0+width,y0-height,y0,iinstance->irtrans.ul,
				iinstance->irtrans.ur, iinstance->irtrans.ub,
				iinstance->irtrans.ut, x[i],y[i],
				&(xout[i]),&(yout[i]));
			}
		}
	}
	return(NhlNOERROR);
}


/*
 * Function:	IrDataToCompc
 *
 * Description: Transforms data coordinates into the computation coordinate
 *		system
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
static NhlErrorTypes IrDataToCompc
#if  __STDC__
(NhlLayer instance,NhlLayer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int *status)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
        NhlLayer   parent;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*	xmissing;
	float*	ymissing;
	int * status;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	int i;

	*status = 0;

	for(i = 0; i< n; i++) {	
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			|| ((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < iinstance->irtrans.x_min)
                        ||(x[i] > iinstance->irtrans.x_max)
                        ||(y[i] < iinstance->irtrans.y_min)
                        ||(y[i] > iinstance->irtrans.y_max)) {

			xout[i] = yout[i] = iinstance->trobj.out_of_range;
			*status = 1;
		} else {
		ret = _NhlEvalSplineCoordForward(&(iinstance->irtrans.thecoord),
			x[i],y[i],&(xout[i]),&(yout[i]),NULL,NULL);
		}
	}
	return(ret);
}


/*
 * Function:	IrCompcToData
 *
 * Description:	transforms computational coordinates into data coordinates.
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
static NhlErrorTypes IrCompcToData
#if  __STDC__
(NhlLayer instance,NhlLayer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int *status)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
        NhlLayer   parent;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*	xmissing;
	float*	ymissing;
	int *status;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	int i;
	

	*status = 0;
	for(i = 0; i< n; i++ ) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			|| ((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < iinstance->irtrans.compc_x_min)
                        ||(x[i] > iinstance->irtrans.compc_x_max)
                        ||(y[i] < iinstance->irtrans.compc_y_min)
                        ||(y[i] > iinstance->irtrans.compc_y_max)) {

			yout[i]=xout[i]=iinstance->trobj.out_of_range;
			*status = 1;

		} else {

		ret = _NhlEvalSplineCoordInverse(&(iinstance->irtrans.thecoord),
			x[i],y[i],&(xout[i]),&(yout[i]),NULL,NULL);

		}
	}
	return(ret);
}

/*
 * Function:	IrDataToWin
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
/*ARGSUSED*/
static NhlErrorTypes IrDataToWin
#if  __STDC__
(NhlLayer instance,NhlLayer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int *status)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
        NhlLayer   parent;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*	xmissing;
	float*	ymissing;
	int *status;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NhlIrregularTransObjLayer iinstance = (NhlIrregularTransObjLayer)instance;
	int i;

	*status = 0;
	for(i = 0 ; i< n; i++) {
		if(((xmissing != NULL)&&(*xmissing == x[i]))
			|| ((ymissing != NULL)&&(*ymissing == y[i]))
			||(x[i] < iinstance->irtrans.compc_x_min)
                        ||(x[i] > iinstance->irtrans.compc_x_max)
                        ||(y[i] < iinstance->irtrans.compc_y_min)
                        ||(y[i] > iinstance->irtrans.compc_y_max)) {

			yout[i]=xout[i]=iinstance->trobj.out_of_range;

			*status = 1;
		} else {
			yout[i] = y[i];	
			xout[i] = x[i];
		}
	}
	return(ret);
}

static NhlErrorTypes IrCompcLineTo
#if __STDC__
(NhlLayer instance, NhlLayer parent,float x, float y, int upordown )
#else
(instance, parent,x, y, upordown )
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
        NhlIrregularTransObjLayer iinst = (NhlIrregularTransObjLayer)instance;
        static float lastx,lasty;
        static call_frstd = 1;
        float currentx,currenty;
        float holdx,holdy;
	float tmpx, tmpy;
	int status;

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
                _NhlTransClipLine(iinst->irtrans.compc_x_min,
                        iinst->irtrans.compc_x_max,
                        iinst->irtrans.compc_y_min,
                        iinst->irtrans.compc_y_max,
                        &lastx,
                        &lasty,
                        &currentx,
                        &currenty,
                        iinst->trobj.out_of_range);
                if((lastx == iinst->trobj.out_of_range)
			||(lasty == iinst->trobj.out_of_range)
			||(currentx == iinst->trobj.out_of_range)
			||(currenty == iinst->trobj.out_of_range)){
/*
* Line has gone completely out of window
*/
                        lastx = x;
                        lasty = y;
                        call_frstd = 1;
                        return(NhlNOERROR);
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
				IrCompcToData(instance,parent,&lastx,&lasty,1,&tmpx,&tmpy,NULL,NULL,&status);
                                _NhlWorkstationLineTo(parent->base.wkptr,c_cufx(tmpx),c_cufy(tmpy),1);
                                call_frstd = 2;
                        }
			IrCompcToData(instance,parent,&currentx,&currenty,1,&tmpx,&tmpy,NULL,NULL,&status);
                        _NhlWorkstationLineTo(parent->base.wkptr,c_cufx(currentx),c_cufy(currenty),0);
                        lastx = x;
                        lasty = y;
                        return(NhlNOERROR);
                }
        }
}



/*ARGSUSED*/
static NhlErrorTypes IrDataLineTo
#if __STDC__
(NhlLayer instance, NhlLayer parent,float x, float y, int upordown )
#else
(instance, parent,x, y, upordown )
NhlLayer instance;
NhlLayer parent;
float x;
float y;
int upordown;
#endif
{
        NhlIrregularTransObjLayer iinst = (NhlIrregularTransObjLayer)instance;
        static float lastx,lasty;
        static call_frstd = 1;
        float currentx,currenty;
        float holdx,holdy;

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
                _NhlTransClipLine(iinst->irtrans.x_min,
                        iinst->irtrans.x_max,
                        iinst->irtrans.y_min,
                        iinst->irtrans.y_max,
                        &lastx,
                        &lasty,
                        &currentx,
                        &currenty,
                        iinst->trobj.out_of_range);
                if((lastx == iinst->trobj.out_of_range)
			||(lasty == iinst->trobj.out_of_range)
			||(currentx == iinst->trobj.out_of_range)
			||(currenty == iinst->trobj.out_of_range)){
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
                        lasty = y;
                        return(NhlNOERROR);
                }
        }
}


static NhlErrorTypes IrNDCLineTo
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
        static float lastx,lasty;
        static call_frstd = 1;
        float currentx,currenty;
        float xvp,yvp,widthvp,heightvp;
        NhlErrorTypes ret = NhlNOERROR,ret1 = NhlNOERROR;
        float holdx,holdy;
	NhlTransObjLayer trinst = (NhlTransObjLayer)instance;

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
                        trinst->trobj.out_of_range);
                if((lastx == trinst->trobj.out_of_range)
			||(lasty == trinst->trobj.out_of_range)
			||(currentx == trinst->trobj.out_of_range)
			||(currenty == trinst->trobj.out_of_range)){
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
                                ret1 = _NhlWorkstationLineTo(parent->base.wkptr, lastx,lasty,1);
                                call_frstd = 2;
                        }
                        ret = _NhlWorkstationLineTo(parent->base.wkptr,currentx, currenty,0);
                        lastx = x;
                        lasty = y;
                        return(MIN(ret1,ret));
                }


        }

}

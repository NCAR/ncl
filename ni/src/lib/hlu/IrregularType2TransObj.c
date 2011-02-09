/*
 *      $Id: IrregularType2TransObj.c,v 1.23 1997-02-24 22:12:25 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		IrregularType2TransObj.c
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
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/IrregularType2TransObjP.h>
#include <ncarg/hlu/View.h>

static NhlResource resources[] = {

/* Begin-documented-resources */


	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTPointer,sizeof(float*),
		NhlOffset(NhlIrregularType2TransObjLayerRec,
			  irtrans.x_coord_points),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,(NhlFreeFunc)NhlFree},
	{ NhlNtrXInterPoints,NhlCtrXInterPoints,NhlTPointer,sizeof(float*),
		NhlOffset(NhlIrregularType2TransObjLayerRec,
			  irtrans.x_inter_points),
		NhlTImmediate,_NhlUSET(NULL),_NhlRES_NORACCESS,(NhlFreeFunc)NhlFree },
	{ NhlNtrXMaxF, NhlCtrXMaxF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.x_max),
		  NhlTString, _NhlUSET("0.0") ,0,NULL},
        { NhlNtrXMinF, NhlCtrXMinF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.x_min),
		  NhlTString,_NhlUSET("0.0") ,0,NULL},
	{ NhlNtrXNumPoints,NhlCtrXNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularType2TransObjLayerRec,
			  irtrans.x_num_points),
		  NhlTImmediate,_NhlUSET(0) ,_NhlRES_NORACCESS,NULL},
	{ NhlNtrXReverse, NhlCtrXReverse, NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.x_reverse),
		  NhlTImmediate,_NhlUSET(False),0,NULL},
	{ NhlNtrXTensionF, NhlCtrXTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.x_tension),
		  NhlTString,_NhlUSET("2.0") ,0,NULL},
	{ NhlNtrXSamples, NhlCtrXSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.x_samples),
		  NhlTImmediate,_NhlUSET((NhlPointer)9) ,0,NULL},
	{ NhlNtrXUseLog, NhlCtrXUseLog, NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.x_use_log),
		  NhlTImmediate,_NhlUSET(False),0,NULL},
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTPointer,sizeof(float*),
		NhlOffset(NhlIrregularType2TransObjLayerRec,
			  irtrans.y_coord_points),
		  NhlTImmediate,_NhlUSET(NULL) ,_NhlRES_NORACCESS,(NhlFreeFunc)NhlFree},
	{ NhlNtrYInterPoints,NhlCtrYInterPoints,NhlTPointer,sizeof(float*),
		NhlOffset(NhlIrregularType2TransObjLayerRec,
			  irtrans.y_inter_points),
		NhlTImmediate,_NhlUSET(NULL) ,_NhlRES_NORACCESS,(NhlFreeFunc)NhlFree},
	{ NhlNtrYMaxF, NhlCtrYMaxF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.y_max),
		  NhlTString, _NhlUSET("0.0") ,0,NULL},
        { NhlNtrYMinF, NhlCtrYMinF, NhlTFloat, sizeof(float),
                NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.y_min),
		  NhlTString, _NhlUSET("0.0"),0,NULL },
	{ NhlNtrYNumPoints,NhlCtrYNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(NhlIrregularType2TransObjLayerRec,
			  irtrans.y_num_points),
		  NhlTImmediate,_NhlUSET(0),_NhlRES_NORACCESS,NULL},
	{ NhlNtrYReverse, NhlCtrYReverse, NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.y_reverse),
		  NhlTImmediate,_NhlUSET(False),0,NULL },
	{ NhlNtrYTensionF, NhlCtrYTensionF, NhlTFloat, sizeof(float),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.y_tension),
		  NhlTString,_NhlUSET("2.0"),0,NULL },
	{ NhlNtrYSamples, NhlCtrYSamples, NhlTInteger, sizeof(int),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.y_samples),
		  NhlTImmediate,_NhlUSET((NhlPointer)9),0,NULL },
	{ NhlNtrYUseLog, NhlCtrYUseLog, NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(NhlIrregularType2TransObjLayerRec,irtrans.y_use_log),
		  NhlTImmediate,_NhlUSET(False),0,NULL}

/* End-documented-resources */

};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  IrTransSetValues(
#if	NhlNeedProto
        NhlLayer,          /* old */
        NhlLayer,          /* reference */
        NhlLayer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes IrTransInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
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
#if	NhlNeedProto
NhlLayer	/*instance*/,
NhlLayer  /*parent*/
#endif
);

static NhlErrorTypes IrWinToNDC(
#if	NhlNeedProto
NhlLayer	/*instance*/,
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
#if	NhlNeedProto
NhlLayer	/*instance*/,
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
#if	NhlNeedProto
NhlLayer   /*instance */,
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
#if	NhlNeedProto
NhlLayer   /*instance */,
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
#if	NhlNeedProto
NhlLayer   /*instance */,
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
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrDataLineTo(
#if 	NhlNeedProto
NhlLayer	/* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrTransGetValues(
#if  NhlNeedProto
	NhlLayer /* l */,
	_NhlArgList /*args */,
	int /* nargs */
#endif 
);

static NhlErrorTypes IrCompcLineTo(
#if 	NhlNeedProto
NhlLayer	/* instance */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

#define CREATE  1
#define SET 0

NhlIrregularType2TransObjClassRec NhlirregularType2TransObjClassRec = {
        {
/* class_name			*/	"irregularType2TransObjClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/  sizeof(NhlIrregularType2TransObjLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhltransObjClassRec,
/* cvt_table			*/	NULL,
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	IrTransInitialize,
/* layer_set_values		*/	IrTransSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	IrTransGetValues,
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

NhlClass NhlirregularType2TransObjClass =
			(NhlClass)&NhlirregularType2TransObjClassRec;

static NhlErrorTypes SetUpTrans
#if	NhlNeedProto
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
	NhlIrregularType2TransObjLayer inew = 
		(NhlIrregularType2TransObjLayer)new;
	NhlIrregularType2TransObjLayer iold = 
		(NhlIrregularType2TransObjLayer)old;
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
                                sizeof(NhlIrregularType2TransObjLayerPart));
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
 * Description:	SetValues method for IrregularType2Trans Objects
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
#if	NhlNeedProto
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
 * Description: Initialize function for IrregularType2TransObjs. Performs same
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
#if	NhlNeedProto
( NhlClass class, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        NhlClass	class;
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
 * Description: set_trans method for IrregularType2TransObjs. The current 
 *		instance and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the IrregularType2TransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes IrSetTrans
#if	NhlNeedProto
(
	NhlLayer	tobj,
	NhlLayer	vobj
) 
#else
(tobj,vobj)
	NhlLayer	tobj;
	NhlLayer	vobj;
#endif
{
	NhlIrregularType2TransObjLayer	to = 
		(NhlIrregularType2TransObjLayer)tobj;
	NhlTransObjLayerPart		*top = &to->trobj;
	NhlIrregularType2TransObjLayerPart	*tp = &to->irtrans;
	NhlErrorTypes ret;

	ret = (*NhltransObjClassRec.trobj_class.set_trans)(tobj,vobj);
	if(ret < NhlWARNING)
		return ret;

	c_set(top->x,top->x+top->width,top->y-top->height,top->y,
		tp->ul,tp->ur,tp->ub,tp->ut,1);

	
	return ret;
	
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
 * In Args:	instance is the IrregularType2TransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized 
 *			device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes IrWinToNDC
#if	NhlNeedProto
(
	NhlLayer	instance,
	float		*x,
	float		*y,
	int		n,
	float		*xout,
	float		*yout,
	float		*xmissing,
	float		*ymissing,
	int		*status
)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer	instance;
	float		*x;
	float		*y;
	int		n;
	float		*xout;
	float		*yout;
	float		*xmissing;
	float		*ymissing;
	int		*status;
#endif
{
	NhlIrregularType2TransObjLayer	iinstance =
		(NhlIrregularType2TransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;
	int i;

	*status = 0;	
	
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
					tp->x,tp->x+tp->width,
					tp->y-tp->height,tp->y,x[i],y[i],
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
					tp->x,tp->x+tp->width,
					tp->y-tp->height,tp->y,x[i],y[i],
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
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int* status)
#else
(instance,x,y,n,xout,yout,xmissing,ymissing,status)
	NhlLayer   instance;
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
	int i;
	NhlIrregularType2TransObjLayer	iinstance =
		(NhlIrregularType2TransObjLayer)instance;
	NhlTransObjLayerPart		*tp = &iinstance->trobj;

	*status = 0;	
	if((xmissing == NULL) &&(ymissing == NULL)) {	
		for(i = 0; i< n; i++) {
			if((x[i] < tp->x)
				||(x[i] > tp->x + tp->width)
				||(y[i] < tp->y - tp->height)
				||(y[i] > tp->y)) {
			
				*status = 1;	
				xout[i]=yout[i]=iinstance->trobj.out_of_range;
			} else {
	
			strans(tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
				iinstance->irtrans.ul, iinstance->irtrans.ur,
				iinstance->irtrans.ub, iinstance->irtrans.ut,
				x[i],y[i],&(xout[i]),&(yout[i]));

			}
		}
	} else {
		for(i = 0; i< n; i++) {
			if(((xmissing != NULL)&&(*xmissing == x[i]))
				|| ((ymissing != NULL)&&(*ymissing == y[i]))
				||(x[i] < tp->x)
                                ||(x[i] > tp->x + tp->width)
                                ||(y[i] < tp->y - tp->height)
                                ||(y[i] > tp->y)) {

				*status = 1;	
				xout[i]=yout[i]=iinstance->trobj.out_of_range;
			} else {
			strans(tp->x,tp->x+tp->width,tp->y-tp->height,tp->y,
				iinstance->irtrans.ul,iinstance->irtrans.ur,
				iinstance->irtrans.ub,iinstance->irtrans.ut,
				x[i],y[i],&(xout[i]),&(yout[i]));
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
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing,int *status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
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
	NhlIrregularType2TransObjLayer iinstance = 
		(NhlIrregularType2TransObjLayer)instance;
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
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int *status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
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
	NhlIrregularType2TransObjLayer iinstance = 
		(NhlIrregularType2TransObjLayer)instance;
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
#if	NhlNeedProto
(NhlLayer instance,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing,int *status)
#else
(instance, x,y,n,xout,yout,xmissing,ymissing,status)
        NhlLayer   instance;
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
	NhlIrregularType2TransObjLayer iinstance = 
		(NhlIrregularType2TransObjLayer)instance;
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
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown )
#else
(instance, x, y, upordown )
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
        NhlIrregularType2TransObjLayer iinst = 
		(NhlIrregularType2TransObjLayer)instance;
        static float lastx,lasty;
        static int call_frstd = 1;
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
				IrCompcToData(instance,&lastx,&lasty,1,&tmpx,&tmpy,NULL,NULL,&status);
                                _NhlWorkstationLineTo(iinst->trobj.wkptr,c_cufx(tmpx),c_cufy(tmpy),1);
                                call_frstd = 2;
                        }
			IrCompcToData(instance,&currentx,&currenty,1,&tmpx,&tmpy,NULL,NULL,&status);
                        _NhlWorkstationLineTo(iinst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);
                        lastx = x;
                        lasty = y;
                        return(NhlNOERROR);
                }
        }
}



/*ARGSUSED*/
static NhlErrorTypes IrDataLineTo
#if	NhlNeedProto
(NhlLayer instance, float x, float y, int upordown )
#else
(instance, x, y, upordown )
NhlLayer instance;
float x;
float y;
int upordown;
#endif
{
        NhlIrregularType2TransObjLayer iinst = 
		(NhlIrregularType2TransObjLayer)instance;
        static float lastx,lasty;
        static int call_frstd = 1;
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
                        return(_NhlWorkstationLineTo(iinst->trobj.wkptr,c_cufx(x),c_cufy(y),1));
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
                                _NhlWorkstationLineTo(iinst->trobj.wkptr,c_cufx(lastx),c_cufy(lasty),1);
                                call_frstd = 2;
                        }
                        _NhlWorkstationLineTo(iinst->trobj.wkptr,c_cufx(currentx),c_cufy(currenty),0);
                        lastx = x;
                        lasty = y;
                        return(NhlNOERROR);
                }
        }
}


static NhlErrorTypes IrNDCLineTo
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
        static float lastx,lasty;
        static int call_frstd = 1;
        float currentx,currenty;
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
                _NhlTransClipLine(trinst->trobj.x,
			trinst->trobj.x+trinst->trobj.width,
			trinst->trobj.y-trinst->trobj.height, trinst->trobj.y,
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
                        return(_NhlWorkstationLineTo(trinst->trobj.wkptr,x,y,1));
                } else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
                        }
                        if(call_frstd == 1) {
                                ret1 = _NhlWorkstationLineTo(trinst->trobj.wkptr, lastx,lasty,1);
                                call_frstd = 2;
                        }
                        ret = _NhlWorkstationLineTo(trinst->trobj.wkptr,currentx, currenty,0);
                        lastx = x;
                        lasty = y;
                        return(MIN(ret1,ret));
                }


        }

}
static NhlErrorTypes IrTransGetValues
#if	NhlNeedProto
(NhlLayer l, _NhlArgList args, int nargs)
#else
(l, args, nargs)
NhlLayer l;
_NhlArgList args;
int nargs;
#endif 
{
	NhlIrregularType2TransObjLayerPart *irp = 
		(&((NhlIrregularType2TransObjLayer)l)->irtrans);
	int i;
	NrmQuark QtrXCoordPoints = NrmStringToQuark(NhlNtrXCoordPoints);
	NrmQuark QtrXInterPoints = NrmStringToQuark(NhlNtrXInterPoints);
	NrmQuark QtrYCoordPoints = NrmStringToQuark(NhlNtrYCoordPoints);
	NrmQuark QtrYInterPoints = NrmStringToQuark(NhlNtrYInterPoints);

	for(i = 0; i< nargs; i++) {
		if(args[i].quark == QtrXCoordPoints ) {
			if((irp->x_coord_points != NULL)&&(irp->x_num_points > 0)) {
				*((float**)args[i].value.ptrval) = (float*) NhlMalloc(sizeof(float) * irp->x_num_points);
				memcpy((void*)*((float**)args[i].value.ptrval),
					(void*)irp->x_coord_points,
					irp->x_num_points * sizeof(float));
			} else {
				*((float**)args[i].value.ptrval) = NULL;
			}
		} 
		if(args[i].quark == QtrXInterPoints) {
			if((irp->x_inter_points != NULL)&&(irp->x_num_points > 0)) {
				*((float**)args[i].value.ptrval) = (float*) NhlMalloc(sizeof(float) * irp->x_num_points);
				memcpy((void*)*((float**)args[i].value.ptrval),
					(void*)irp->x_inter_points,
					irp->x_num_points * sizeof(float));
			} else {
				*((float**)args[i].value.ptrval) = NULL;
			}
		} 
		if(args[i].quark == QtrYCoordPoints ) {
			if((irp->y_coord_points != NULL)&&(irp->y_num_points > 0)) {
				*((float**)args[i].value.ptrval) = (float*) NhlMalloc(sizeof(float) * irp->y_num_points);
				memcpy((void*)*((float**)args[i].value.ptrval),
					(void*)irp->y_coord_points,
					irp->y_num_points * sizeof(float));
			} else {
				*((float**)args[i].value.ptrval) = NULL;
			}
		} 
		if(args[i].quark == QtrYInterPoints) {
			if((irp->y_inter_points != NULL)&&(irp->y_num_points > 0)) {
				*((float**)args[i].value.ptrval) = (float*) NhlMalloc(sizeof(float) * irp->y_num_points);
				memcpy((void*)*((float**)args[i].value.ptrval),
					(void*)irp->y_inter_points,
					irp->y_num_points * sizeof(float));
			} else {
				*((float**)args[i].value.ptrval) = NULL;
			}
		} 
	}
	return(NhlNOERROR);
	
}

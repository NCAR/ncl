
/*
 *      $Id: IrregularType2TransObj.c,v 1.2 1993-05-27 19:11:14 ethan Exp $
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
 *			The type 2 version of irregular grid transformations
 *			differs from the original in that it sets the window
 *			to be identical to the computational coordinates. This
 *			has the effect of placing the irregularly spaced 
 *			coordinates onto the frame and even spacings, thus 
 *			streching or deforming the grid so that all data can
 *			be view. The original returns points back to their
 *			data values before rendering. For some grids this has
 *			the effect of "crowding" several grid points in to one
 *			small screen location.
 *
 *
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/IrregularType2TransObjP.h>
#include <ncarg/hlu/View.h>


static NhlResource resources[] = {
	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_coord_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrXInterPoints,NhlCtrXInterPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_inter_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrXMaxF, NhlCtrXMaxF, NhlTFloat, sizeof(float),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_max),
		NhlTString, "0.0" },
	{ NhlNtrXMinF, NhlCtrXMinF, NhlTFloat, sizeof(float),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_min),
		NhlTString, "0.0" },
	{ NhlNtrXNumPoints,NhlCtrXNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_num_points),
		NhlTString,"0" },
	{ NhlNtrXReverse, NhlCtrXReverse, NhlTInteger,sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_reverse),
		NhlTString,"0" },
	{ NhlNtrXTension, NhlCtrXTension, NhlTFloat, sizeof(float),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_tension),
		NhlTString,"2.0" },
	{ NhlNtrXSamples, NhlCtrXSamples, NhlTInteger, sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_samples),
		NhlTString,"9" },
	{ NhlNtrXUseLog, NhlCtrXUseLog, NhlTInteger, sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.x_use_log),
		NhlTString,"0" },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_coord_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrYInterPoints,NhlCtrYInterPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_inter_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrYMaxF, NhlCtrYMaxF, NhlTFloat, sizeof(float),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_max),
		NhlTString, "0.0" },
	{ NhlNtrYMinF, NhlCtrYMinF, NhlTFloat, sizeof(float),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_min),
		NhlTString, "0.0" },
	{ NhlNtrYNumPoints,NhlCtrYNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_num_points),
		NhlTString,"0" },
	{ NhlNtrYReverse, NhlCtrYReverse, NhlTInteger,sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_reverse),
		NhlTString,"0" },
	{ NhlNtrYTension, NhlCtrYTension, NhlTFloat, sizeof(float),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_tension),
		NhlTString,"2.0" },
	{ NhlNtrYSamples, NhlCtrYSamples, NhlTInteger, sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_samples),
		NhlTString,"9" },
	{ NhlNtrYUseLog, NhlCtrYUseLog, NhlTInteger, sizeof(int),
		NhlOffset(IrregularType2TransObjLayerRec,ir2trans.y_use_log),
		NhlTString,"0" }
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  IrTransSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes IrTransInitialize(
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

static NhlErrorTypes IrSetTrans(
#ifdef NhlNeedProto
Layer	/*instance*/,
Layer  /*parent*/
#endif
);

static NhlErrorTypes IrWinToNDC(
#ifdef NhlNeedProto
Layer	/*instance*/,
Layer	/* parent */,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/
#endif
);


static NhlErrorTypes IrNDCToWin(
#ifdef NhlNeedProto
Layer	/*instance*/,
Layer	/*parent */,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/
#endif
);


static NhlErrorTypes IrDataToCompc(
#ifdef NhlNeedProto
Layer   /*instance */,
Layer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/
#endif
);

static NhlErrorTypes IrCompcToData(
#ifdef NhlNeedProto
Layer   /*instance */,
Layer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/
#endif
);

#ifdef	NOTYET
static NhlErrorTypes IrWinToCompc(
#ifdef NhlNeedProto
Layer   /*instance */,
Layer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/
#endif
);

static NhlErrorTypes IrCompcToWin(
#ifdef NhlNeedProto
Layer   /*instance */,
Layer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float*	/*xmissing*/,
float*	/*ymissing*/
#endif
);
#endif /*NOTYET*/


static NhlErrorTypes IrNDCLineTo(
#if     NhlNeedProto
Layer   /* instance */,
Layer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes IrDataLineTo(
#if     NhlNeedProto
Layer   /* instance */,
Layer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);

static NhlErrorTypes IrWinLineTo(
#if     NhlNeedProto
Layer   /* instance */,
Layer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);



IrregularType2TransObjLayerClassRec irregularType2TransObjLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&transObjLayerClassRec,
/* class_name			*/	"IrregularType2TransObj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(IrregularType2TransObjLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	IrTransInitialize,
/* layer_set_values		*/	IrTransSetValues,
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
/* set_trans		*/	IrSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	IrWinToNDC,
/* ndc_to_win		*/	IrNDCToWin,
/* data_to_win		*/	IrDataToCompc, 
/* win_to_data		*/	IrCompcToData, 
/* data_to_compc	*/	IrDataToCompc,
/* compc_to_data	*/	IrCompcToData,
/* win_to_compc		*/	NULL,
/* compc_to_win		*/	NULL,
/* data_lineto 		*/      IrDataLineTo,
/* compc_lineto		*/      IrWinLineTo,
/* win_lineto 		*/      IrWinLineTo,
/* NDC_lineto 		*/      IrNDCLineTo
        }
};

LayerClass irregularType2TransObjLayerClass = (LayerClass)&irregularType2TransObjLayerClassRec;




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
#if __STDC__
(Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference, new,args,num_args)
	Layer old;
	Layer reference;
	Layer new;
	_NhlArgList args;
	int	num_args;
#endif
{
	IrregularType2TransObjLayer inew = (IrregularType2TransObjLayer) new;
	IrregularType2TransObjLayer iold = (IrregularType2TransObjLayer) old;
	float tmp;
	NhlErrorTypes ret = NOERROR;
	char buffer[80];
/*
* Only type of change allowed by this object
*/
	
	_NhlEvalSplineCoordForward(&inew->ir2trans.thecoord,
		inew->ir2trans.x_min,
		inew->ir2trans.y_min,
		&(inew->ir2trans.ul),
		&(inew->ir2trans.ub),
		NULL,NULL);
	_NhlEvalSplineCoordForward(&inew->ir2trans.thecoord,
		inew->ir2trans.x_max,
		inew->ir2trans.y_max,
		&(inew->ir2trans.ur),
		&(inew->ir2trans.ut),
		NULL,NULL);
	if(inew->ir2trans.x_reverse) {
		tmp = inew->ir2trans.ur;
		inew->ir2trans.ur = inew->ir2trans.ul;
		inew->ir2trans.ul = tmp;
	}
	if(inew->ir2trans.y_reverse) {
		tmp = inew->ir2trans.ut;
		inew->ir2trans.ut = inew->ir2trans.ub;
		inew->ir2trans.ub = tmp;
	}
/*
* All the rest of the fields can't be changed
*/
	
	if(inew->ir2trans.x_coord_points != iold->ir2trans.x_coord_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.x_coord_points = iold->ir2trans.x_coord_points;
		ret = WARNING;
	}
	if(inew->ir2trans.x_inter_points != iold->ir2trans.x_inter_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.x_inter_points = iold->ir2trans.x_inter_points;
		ret = WARNING;
	}
	if(inew->ir2trans.x_num_points != iold->ir2trans.x_num_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.x_num_points = iold->ir2trans.x_num_points;
		ret = WARNING;
	}
	if(inew->ir2trans.x_tension != iold->ir2trans.x_tension) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.x_tension = iold->ir2trans.x_tension;
		ret = WARNING;
	}
	if(inew->ir2trans.x_samples != iold->ir2trans.x_samples) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.x_samples = iold->ir2trans.x_samples;
		ret = WARNING;
	}
	if(inew->ir2trans.y_coord_points != iold->ir2trans.y_coord_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.y_coord_points = iold->ir2trans.y_coord_points;
		ret = WARNING;
	}
	if(inew->ir2trans.y_inter_points != iold->ir2trans.y_inter_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.y_inter_points = iold->ir2trans.y_inter_points;
		ret = WARNING;
	}
	if(inew->ir2trans.y_num_points != iold->ir2trans.y_num_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.y_num_points = iold->ir2trans.y_num_points;
		ret = WARNING;
	}
	if(inew->ir2trans.y_tension != iold->ir2trans.y_tension) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.y_tension = iold->ir2trans.y_tension;
		ret = WARNING;
	}
	if(inew->ir2trans.y_samples != iold->ir2trans.y_samples) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->ir2trans.y_samples = iold->ir2trans.y_samples;
		ret = WARNING;
	}
	return(ret);
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
#if __STDC__
( LayerClass class, Layer req, Layer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        LayerClass	class;
        Layer		req;
        Layer		new;
        _NhlArgList	args;
        int		num_args;
#endif
{
	IrregularType2TransObjLayer inew = (IrregularType2TransObjLayer)new;
	IrregularType2TransObjLayer ireq = (IrregularType2TransObjLayer)req;
	NhlErrorTypes ret = NOERROR;
	float tmp;
	Status xstatus,ystatus;
	char buffer[80];


	if((ireq->ir2trans.x_coord_points == NULL)||
			(ireq->ir2trans.y_coord_points==NULL)||
			(ireq->ir2trans.x_num_points < 2)||
			(ireq->ir2trans.y_num_points < 2)) {
		sprintf(buffer, "IrTransInitialize: Not enough information to set up transformations");
		NhlPError(FATAL,E_UNKNOWN,buffer);
			
		return(FATAL);
	} else {
		inew->ir2trans.x_coord_points = (float*)NhlMalloc((unsigned)
				sizeof(float) *(ireq->ir2trans.x_num_points));
		inew->ir2trans.y_coord_points = (float*)NhlMalloc((unsigned)
				sizeof(float) *(ireq->ir2trans.y_num_points));
		bcopy(ireq->ir2trans.x_coord_points,
			inew->ir2trans.x_coord_points,
			sizeof(float)*ireq->ir2trans.x_num_points);
		bcopy(ireq->ir2trans.y_coord_points,
			inew->ir2trans.y_coord_points,
			sizeof(float)*ireq->ir2trans.y_num_points);
		if((inew->ir2trans.x_min == 0.0)&&
			(inew->ir2trans.x_max == 0.0)) {
			inew->ir2trans.x_min = MIN(inew->ir2trans.x_coord_points[0],inew->ir2trans.x_coord_points[ireq->ir2trans.x_num_points-1]);
			inew->ir2trans.x_max = MAX(inew->ir2trans.x_coord_points[0],inew->ir2trans.x_coord_points[ireq->ir2trans.x_num_points-1]);
		} else if(inew->ir2trans.x_min > inew->ir2trans.x_max) {
			tmp = inew->ir2trans.x_min;
			inew->ir2trans.x_min = inew->ir2trans.x_max;
			inew->ir2trans.x_max = tmp;
		}
		if((inew->ir2trans.y_min == 0.0)&&
			(inew->ir2trans.y_max == 0.0)) {
			inew->ir2trans.y_min = MIN(inew->ir2trans.y_coord_points[0],inew->ir2trans.y_coord_points[ireq->ir2trans.y_num_points-1]);
			inew->ir2trans.y_max = MAX(inew->ir2trans.y_coord_points[0],inew->ir2trans.y_coord_points[ireq->ir2trans.y_num_points-1]);
		} else if(inew->ir2trans.y_min > inew->ir2trans.y_max) {
			tmp = inew->ir2trans.y_min;
			inew->ir2trans.y_min = inew->ir2trans.y_max;
			inew->ir2trans.y_max = tmp;
		}
		
	}
	if((ireq->ir2trans.x_inter_points != NULL)) {
		inew->ir2trans.x_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (ireq->ir2trans.x_num_points));
		bcopy(ireq->ir2trans.x_inter_points,inew->ir2trans.x_inter_points,
			sizeof(float)*ireq->ir2trans.x_num_points);
	}
	
	if((ireq->ir2trans.y_inter_points != NULL)) {
		inew->ir2trans.y_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (ireq->ir2trans.y_num_points));
		bcopy(ireq->ir2trans.y_inter_points,inew->ir2trans.y_inter_points,
			sizeof(float)*ireq->ir2trans.y_num_points);
	}

	ret = _NhlCreateSplineCoordApprox(&(inew->ir2trans.thecoord),
		inew->ir2trans.x_use_log,
		inew->ir2trans.x_coord_points,
		inew->ir2trans.x_inter_points,
		inew->ir2trans.x_num_points,
		inew->ir2trans.y_use_log,
		inew->ir2trans.y_coord_points,
		inew->ir2trans.y_inter_points,
		inew->ir2trans.y_num_points,
		inew->ir2trans.x_tension,inew->ir2trans.y_tension,
		inew->ir2trans.x_samples,inew->ir2trans.y_samples,
		&xstatus,&ystatus);	
	_NhlEvalSplineCoordForward(&inew->ir2trans.thecoord,
		inew->ir2trans.x_min,
		inew->ir2trans.y_min,
		&(inew->ir2trans.ul),
		&(inew->ir2trans.ub),
		NULL,NULL);
	_NhlEvalSplineCoordForward(&inew->ir2trans.thecoord,
		inew->ir2trans.x_max,
		inew->ir2trans.y_max,
		&(inew->ir2trans.ur),
		&(inew->ir2trans.ut),
		NULL,NULL);

	if(ireq->ir2trans.x_reverse) {
		tmp = inew->ir2trans.ur;
		inew->ir2trans.ur = inew->ir2trans.ul;
		inew->ir2trans.ul = tmp;
	}
	if(ireq->ir2trans.y_reverse) {
		tmp = inew->ir2trans.ut;
		inew->ir2trans.ut = inew->ir2trans.ub;
		inew->ir2trans.ub = tmp;
	}
	return(ret);
}


/*
 * Function:	IrSetTrans
 *
 * Description: set_trans method for IrregularType2TransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
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
#if __STDC__
(Layer instance, Layer parent) 
#else
(instance, parent)
Layer   instance;
Layer   parent;
#endif
{
	float x;
	float y;
	float width;
	float height;
	IrregularType2TransObjLayer iinstance = (IrregularType2TransObjLayer)instance;
	NhlErrorTypes ret;
	

	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x,
		NhlNvpYF,&y,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if(ret < WARNING) {
		return(ret);
	}
	
		
		

	c_set(x,x+width,y-height,y,iinstance->ir2trans.ul,iinstance->ir2trans.ur,
		iinstance->ir2trans.ub,iinstance->ir2trans.ut,1);

	
	return(NOERROR);
	
}


/*
 * Function:	IrWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the IrregularType2TransObj and parent is the plot.
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float* xmissing,float* ymissing)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing)
	Layer   instance;
	Layer   parent;
	float   *x;
	float   *y;
	int	n;
	float*  xout;
	float*  yout;
	float*  xmissing;
	float*	ymissing;
#endif
{
	float x0;
	float y0;
	float width;
	float height;
	IrregularType2TransObjLayer iinstance = (IrregularType2TransObjLayer)instance;
	int i;
	NhlErrorTypes ret;
	int xmis = 0; int ymis = 0;
	
	
	ret= NhlGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if(ret < WARNING) {
		return(ret);
	}
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n ; i++) {
			strans(iinstance->ir2trans.ul,iinstance->ir2trans.ur,
				iinstance->ir2trans.ub,iinstance->ir2trans.ut,
				 x0,x0+width,y0-height,y0,x[i],y[i],
				&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n ; i++) {
			if((xmissing != NULL)&&(*xmissing == x[i])) 
				xmis = 1;
			if((ymissing != NULL)&&(*ymissing == y[i])) 
				ymis = 1;
			strans(iinstance->ir2trans.ul,iinstance->ir2trans.ur,
				iinstance->ir2trans.ub,iinstance->ir2trans.ut,
				 x0,x0+width,y0-height,y0,x[i],y[i],
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

	return(NOERROR);
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float * xmissing, float *ymissing)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing)
	Layer   instance;
	Layer   parent;
	float   *x;
	float   *y;
	int	n;
	float*  xout;
	float*  yout;
	float*	xmissing;
	float*	ymissing;
#endif
{
	float x0;
	float y0;
	float width;
	int i;
	float height;
	IrregularType2TransObjLayer iinstance = (IrregularType2TransObjLayer)instance;
	NhlErrorTypes ret;
	int xmis = 0; int ymis = 0;
	
	
	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if( ret < WARNING)
		return(ret);
	if((xmissing == NULL)&&(ymissing == NULL)) {
		for(i = 0; i< n; i++) {
			strans(x0,x0+width,y0-height,y0,iinstance->ir2trans.ul,
				iinstance->ir2trans.ur, iinstance->ir2trans.ub,
				iinstance->ir2trans.ut, x[i],y[i],
				&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n; i++) {
			if((xmissing != NULL)&&(*xmissing == x[i]))
				xmis = 1;
			if((ymissing != NULL)&&(*ymissing == y[i]))
				ymis = 1;
			strans(x0,x0+width,y0-height,y0,iinstance->ir2trans.ul,
				iinstance->ir2trans.ur, iinstance->ir2trans.ub,
				iinstance->ir2trans.ut, x[i],y[i],
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
	return(NOERROR);
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing)
        Layer   instance;
        Layer   parent;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*  xmissing;
	float*	ymissing;
#endif
{
	NhlErrorTypes ret = NOERROR;
	IrregularType2TransObjLayer iinstance = (IrregularType2TransObjLayer)instance;

	if(n == 1) {
		ret = _NhlEvalSplineCoordForward(&(iinstance->ir2trans.thecoord),
			*x,*y,xout,yout,xmissing,ymissing);
	} else {
		ret = _NhlMultiEvalSplineCoordForward(	
		      &(iinstance->ir2trans.thecoord),x,y,xout,yout,n,n,xmissing,ymissing);
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
#else
(instance, parent,x,y,n,xout,yout,xmissing,ymissing)
        Layer   instance;
        Layer   parent;
        float   *x;
        float   *y;
        int     n;
        float*  xout;
        float*  yout;
	float*	xmissing;
	float*	ymissing;
#endif
{
	NhlErrorTypes ret = NOERROR;
	IrregularType2TransObjLayer iinstance = (IrregularType2TransObjLayer)instance;

	if(n == 1) {
		ret = _NhlEvalSplineCoordInverse(&(iinstance->ir2trans.thecoord),
			*x,*y,xout,yout,xmissing,ymissing);
	} else {
		ret = _NhlMultiEvalSplineCoordInverse(	
		      &(iinstance->ir2trans.thecoord),x,y,xout,yout,n,n,xmissing,ymissing);
	}
	return(ret);
}


static NhlErrorTypes IrDataLineTo
#if __STDC__
(Layer instance, Layer parent,float x, float y, int upordown )
#else
(instance, parent,x, y, upordown )
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	IrregularType2TransObjLayer ir2inst = (IrregularType2TransObjLayer)instance;
	static float lastx,lasty;
	static call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	float holdx,holdy;
	int ix0,ix1,iy0,iy1;

/*
* if true the moveto is being performed
*/
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd = 1;
/* FORTRAN */		lastd_();
		return(NOERROR);
	} else {
		currentx = x;
		currenty = y;
		holdx = lastx;
		holdy = lasty;
		_NhlTransClipLine(ir2inst->ir2trans.x_min,
			ir2inst->ir2trans.x_max,
			ir2inst->ir2trans.y_min,
			ir2inst->ir2trans.y_max,
			&lastx,
			&lasty,
			&currentx,
			&currenty,
			-9999.0);
		if((lastx == -9999.0)||(lasty == -9999)||(currentx == -9999.0)||(currenty == -9999.0)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
/* FORTRAN */		lastd_();
			return(NOERROR);
		} else {
			xpoints[0] = lastx;
			xpoints[1] = currentx;
			ypoints[0] = lasty;
			ypoints[1] = currenty;
			IrDataToCompc(instance,parent,xpoints,ypoints,2,xpoints,ypoints,NULL,NULL);
			if((lastx != holdx)||(lasty!= holdy)) {
				call_frstd = 1;
/* FORTRAN */			lastd_();
			}
			if(call_frstd == 1) {
				ix0 = c_kumx(xpoints[0]);
				iy0 = c_kumy(ypoints[0]);
/* FORTRAN */			cfvld_(&call_frstd,&ix0,&iy0);
				call_frstd = 2;
			}
			ix1 = c_kumx(xpoints[1]);
			iy1 = c_kumy(ypoints[1]);
/* FORTRAN */		cfvld_(&call_frstd,&ix1,&iy1);
			lastx = x;
			lasty = y;
			return(NOERROR);
		}
			
			
	}
	
}

static NhlErrorTypes IrWinLineTo
#if __STDC__
(Layer instance, Layer parent, float x, float y, int upordown)
#else
(instance, parent, x, y, upordown)
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	IrregularType2TransObjLayer ir2inst = (IrregularType2TransObjLayer)instance;
	static float lastx,lasty;
	static call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	int ix0,ix1,iy0,iy1;
	float xmin,ymin,xmax,ymax;
	float holdx, holdy;

/*
* if true the moveto is being performed
*/
	xmin = MIN(ir2inst->ir2trans.ur,ir2inst->ir2trans.ul);
	xmax = MAX(ir2inst->ir2trans.ur,ir2inst->ir2trans.ul);
	ymin = MIN(ir2inst->ir2trans.ub,ir2inst->ir2trans.ut);
	ymax = MAX(ir2inst->ir2trans.ub,ir2inst->ir2trans.ut);
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd = 1;
		return(NOERROR);
	} else {
		currentx = x;
		currenty = y;
		holdx = lastx;
		holdy = lasty;
		_NhlTransClipLine(
			xmin, xmax, ymin, ymax,
			&lastx, &lasty, &currentx, &currenty,
			-9999.0);
		if((lastx == -9999.0)||(lasty == -9999)||(currentx == -9999.0)||(currenty == -9999.0)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
/* FORTRAN */		lastd_();
			return(NOERROR);
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
/* FORTRAN */                   lastd_();
                        }

			if(call_frstd == 1) {
				ix0 = c_kumx(lastx);
				iy0 = c_kumy(lasty);
/* FORTRAN */			cfvld_(&call_frstd,&ix0,&iy0);
				call_frstd = 2;
			}
			ix1 = c_kumx(currentx);
			iy1 = c_kumy(currenty);
/* FORTRAN */		cfvld_(&call_frstd,&ix1,&iy1);
			lastx = x;
			lasty = y;
			return(NOERROR);
		}
			
			
	}
	
}


static NhlErrorTypes IrNDCLineTo
#if __STDC__
(Layer instance, Layer parent,float x, float y, int upordown )
#else
(instance, parent,x, y, upordown )
Layer instance;
Layer parent;
float x;
float y;
int upordown;
#endif
{
	IrregularType2TransObjLayer ir2inst = (IrregularType2TransObjLayer)instance;
	static float lastx,lasty;
	static call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	int ix0,ix1,iy0,iy1;
	float xvp,yvp,widthvp,heightvp;
	float holdx,holdy;

/*
* if true the moveto is being performed
*/
	if(upordown) {
		lastx = x;
		lasty = y;
		call_frstd = 1;
/* FORTRAN */		lastd_();
		return(NOERROR);
	} else {
		currentx = x;
		currenty = y;
		holdx = lastx;
		holdy = lasty;
                NhlGetValues(parent->base.id,
                        NhlNvpXF,&xvp,
                        NhlNvpYF,&yvp,
                        NhlNvpWidthF,&widthvp,
                        NhlNvpHeightF,&heightvp,NULL);

		_NhlTransClipLine(
			xvp, xvp+widthvp, yvp-heightvp, yvp,
			&lastx, &lasty, &currentx, &currenty,
			-9999.0);
		if((lastx == -9999.0)||(lasty == -9999)||(currentx == -9999.0)||(currenty == -9999.0)){
/*
* Line has gone completely out of window
*/
			lastx = x;	
			lasty = y;
			call_frstd = 1;
/* FORTRAN */		lastd_();
			return(NOERROR);
		} else {
                        if((lastx != holdx)||(lasty!= holdy)) {
                                call_frstd = 1;
/* FORTRAN */                   lastd_();
                        }

			if(call_frstd == 1) {
				ix0 = c_kfmx(lastx);
				iy0 = c_kfmy(lasty);
/* FORTRAN */			cfvld_(&call_frstd,&ix0,&iy0);
				call_frstd = 2;
			}
			ix1 = c_kfmx(currentx);
			iy1 = c_kfmy(currenty);
/* FORTRAN */		cfvld_(&call_frstd,&ix1,&iy1);
			lastx = x;
			lasty = y;
			return(NOERROR);
		}
			
			
	}
	
}

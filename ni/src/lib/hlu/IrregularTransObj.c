
/*
 *      $Id: IrregularTransObj.c,v 1.2 1993-05-27 19:11:11 ethan Exp $
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

/*
* I added this comment to test CVS
*/


static NhlResource resources[] = {
	{ NhlNtrXCoordPoints,NhlCtrXCoordPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_coord_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrXInterPoints,NhlCtrXInterPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_inter_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrXNumPoints,NhlCtrXNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_num_points),
		NhlTString,"0" },
	{ NhlNtrXReverse, NhlCtrXReverse, NhlTInteger,sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_reverse),
		NhlTString,"0" },
	{ NhlNtrXTension, NhlCtrXTension, NhlTFloat, sizeof(float),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_tension),
		NhlTString,"2.0" },
	{ NhlNtrXSamples, NhlCtrXSamples, NhlTInteger, sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_samples),
		NhlTString,"9" },
	{ NhlNtrXUseLog, NhlCtrXUseLog, NhlTInteger,sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.x_use_log),
		NhlTString,"0" },
	{ NhlNtrYCoordPoints,NhlCtrYCoordPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_coord_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrYInterPoints,NhlCtrYInterPoints,NhlTFloatPtr,sizeof(float*),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_inter_points),
		NhlTFloatPtr,NULL },
	{ NhlNtrYNumPoints,NhlCtrYNumPoints,NhlTInteger,sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_num_points),
		NhlTString,"0" },
	{ NhlNtrYReverse, NhlCtrYReverse, NhlTInteger,sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_reverse),
		NhlTString,"0" },
	{ NhlNtrYTension, NhlCtrYTension, NhlTFloat, sizeof(float),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_tension),
		NhlTString,"2.0" },
	{ NhlNtrYSamples, NhlCtrYSamples, NhlTInteger, sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_samples),
		NhlTString,"9" },
	{ NhlNtrYUseLog, NhlCtrYUseLog, NhlTInteger,sizeof(int),
		NhlOffset(IrregularTransObjLayerRec,irtrans.y_use_log),
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
float* /*xmissing*/,
float* /*ymissing*/
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
float* /*xmissing*/,
float* /*ymissing*/
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
float* /*xmissing*/,
float* /*ymissing*/
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
float* /*xmissing*/,
float* /*ymissing*/
#endif
);

static NhlErrorTypes IrWinToCompc(
#ifdef NhlNeedProto
Layer   /*instance */,
Layer   /*parent */,
float*  /*x*/,
float*   /*y*/,
int     /* n*/,
float*  /*xout*/,
float*  /*yout*/,
float* /*xmissing*/,
float* /*ymissing*/
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
float* /*xmissing*/,
float* /*ymissing*/
#endif
);



IrregularTransObjLayerClassRec irregularTransObjLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&transObjLayerClassRec,
/* class_name			*/	"IrregularTransObj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(IrregularTransObjLayerRec),
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
/* data_to_win		*/	NULL, /* One To One for this Transformation */
/* win_to_data		*/	NULL, /* One To One for this Transformation */
/* data_to_compc	*/	IrDataToCompc,
/* compc_to_data	*/	IrCompcToData,
/* win_to_compc		*/	IrWinToCompc,
/* compc_to_win		*/	IrCompcToWin,
/* data_lineto */       NULL,
/* compc_lineto */      NULL,
/* win_lineto */        NULL,
/* NDC_lineto */        NULL
        }
};

LayerClass irregularTransObjLayerClass = (LayerClass)&irregularTransObjLayerClassRec;




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
	IrregularTransObjLayer inew = (IrregularTransObjLayer) new;
	IrregularTransObjLayer iold = (IrregularTransObjLayer) old;
	float tmp;
	NhlErrorTypes ret = NOERROR;
	char buffer[80];
/*
* Only type of change allowed by this object
*/
	if(inew->irtrans.x_reverse != iold->irtrans.x_reverse) {
		tmp = inew->irtrans.ur;
		inew->irtrans.ur = inew->irtrans.ul;
		inew->irtrans.ul = tmp;
	}
	if(inew->irtrans.y_reverse != iold->irtrans.y_reverse) {
		tmp = inew->irtrans.ut;
		inew->irtrans.ut = inew->irtrans.ub;
		inew->irtrans.ub = tmp;
	}
/*
* All the rest of the fields can't be changed
*/
	
	if(inew->irtrans.x_coord_points != iold->irtrans.x_coord_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.x_coord_points = iold->irtrans.x_coord_points;
		ret = WARNING;
	}
	if(inew->irtrans.x_inter_points != iold->irtrans.x_inter_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.x_inter_points = iold->irtrans.x_inter_points;
		ret = WARNING;
	}
	if(inew->irtrans.x_num_points != iold->irtrans.x_num_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.x_num_points = iold->irtrans.x_num_points;
		ret = WARNING;
	}
	if(inew->irtrans.x_tension != iold->irtrans.x_tension) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.x_tension = iold->irtrans.x_tension;
		ret = WARNING;
	}
	if(inew->irtrans.x_samples != iold->irtrans.x_samples) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.x_samples = iold->irtrans.x_samples;
		ret = WARNING;
	}
	if(inew->irtrans.y_coord_points != iold->irtrans.y_coord_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.y_coord_points = iold->irtrans.y_coord_points;
		ret = WARNING;
	}
	if(inew->irtrans.y_inter_points != iold->irtrans.y_inter_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.y_inter_points = iold->irtrans.y_inter_points;
		ret = WARNING;
	}
	if(inew->irtrans.y_num_points != iold->irtrans.y_num_points) {
		sprintf(buffer,"Coordinate transformation points can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.y_num_points = iold->irtrans.y_num_points;
		ret = WARNING;
	}
	if(inew->irtrans.y_tension != iold->irtrans.y_tension) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.y_tension = iold->irtrans.y_tension;
		ret = WARNING;
	}
	if(inew->irtrans.y_samples != iold->irtrans.y_samples) {
		sprintf(buffer,"Coordinate transformation information can only be set on Create");
		NhlPError(WARNING,E_UNKNOWN,buffer);
		inew->irtrans.y_samples = iold->irtrans.y_samples;
		ret = WARNING;
	}
	return(ret);
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
	IrregularTransObjLayer inew = (IrregularTransObjLayer)new;
	IrregularTransObjLayer ireq = (IrregularTransObjLayer)req;
	NhlErrorTypes ret = NOERROR;
	float tmp;
	Status xstatus,ystatus;
	char buffer[80];


	if((ireq->irtrans.x_coord_points == NULL)||
			(ireq->irtrans.y_coord_points==NULL)||
			(ireq->irtrans.x_num_points < 2)||
			(ireq->irtrans.y_num_points < 2)) {
		sprintf(buffer, "IrTransInitialize: Not enough information to set up transformations");
		NhlPError(FATAL,E_UNKNOWN,buffer);
			
		return(FATAL);
	} else {
		inew->irtrans.x_coord_points = (float*)NhlMalloc((unsigned)
				sizeof(float) *(ireq->irtrans.x_num_points));
		inew->irtrans.y_coord_points = (float*)NhlMalloc((unsigned)
				sizeof(float) *(ireq->irtrans.y_num_points));
		bcopy(ireq->irtrans.x_coord_points,
			inew->irtrans.x_coord_points,
			sizeof(float)*ireq->irtrans.x_num_points);
		bcopy(ireq->irtrans.y_coord_points,
			inew->irtrans.y_coord_points,
			sizeof(float)*ireq->irtrans.y_num_points);
		inew->irtrans.ul = inew->irtrans.x_coord_points[0];
		inew->irtrans.ur = inew->irtrans.x_coord_points[ireq->irtrans.x_num_points-1];
		inew->irtrans.ub = inew->irtrans.y_coord_points[0];
		inew->irtrans.ut = inew->irtrans.y_coord_points[ireq->irtrans.y_num_points-1];
	}
	if((ireq->irtrans.x_inter_points != NULL)) {
		inew->irtrans.x_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (ireq->irtrans.x_num_points));
		bcopy(ireq->irtrans.x_inter_points,inew->irtrans.x_inter_points,
			sizeof(float)*ireq->irtrans.x_num_points);
	} 
	if((ireq->irtrans.y_inter_points != NULL)) {
		inew->irtrans.y_inter_points = (float*)NhlMalloc((unsigned)
				sizeof(float) * (ireq->irtrans.y_num_points));
		bcopy(ireq->irtrans.y_inter_points,inew->irtrans.y_inter_points,
			sizeof(float)*ireq->irtrans.y_num_points);
	} 

	if(ireq->irtrans.x_reverse) {
		tmp = inew->irtrans.ur;
		inew->irtrans.ur = inew->irtrans.ul;
		inew->irtrans.ul = tmp;
	}
	if(ireq->irtrans.y_reverse) {
		tmp = inew->irtrans.ut;
		inew->irtrans.ut = inew->irtrans.ub;
		inew->irtrans.ub = tmp;
	}
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
	return(ret);
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
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;
	NhlErrorTypes ret;
	

	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x,
		NhlNvpYF,&y,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if(ret < WARNING) {
		return(ret);
	}
	
		
		

	c_set(x,x+width,y-height,y,iinstance->irtrans.ul,iinstance->irtrans.ur,
		iinstance->irtrans.ub,iinstance->irtrans.ut,1);

	
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
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
	float*  ymissing;
#endif
{
	float x0;
	float y0;
	float width;
	float height;
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;
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
			strans(iinstance->irtrans.ul,iinstance->irtrans.ur,
				iinstance->irtrans.ub,iinstance->irtrans.ut,
				 x0,x0+width,y0-height,y0,x[i],y[i],
				&(xout[i]),&(yout[i]));
		}
	} else {	
		for(i = 0; i< n ; i++) {
			if((xmissing != NULL) &&(*xmissing == x[i])) 
				xmis = 1;
			if((ymissing != NULL) &&(*ymissing == y[i])) 
				ymis = 1;
			strans(iinstance->irtrans.ul,iinstance->irtrans.ur,
				iinstance->irtrans.ub,iinstance->irtrans.ut,
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
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
	float*  ymissing;
#endif
{
	float x0;
	float y0;
	float width;
	int i;
	float height;
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;
	NhlErrorTypes ret;
	int xmis = 1; int ymis = 1;
	
	
	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if( ret < WARNING)
		return(ret);

	if((xmissing == NULL) &&(ymissing == NULL)) {	
		for(i = 0; i< n; i++) {
			strans(x0,x0+width,y0-height,y0,iinstance->irtrans.ul,
				iinstance->irtrans.ur, iinstance->irtrans.ub,
				iinstance->irtrans.ut, x[i],y[i],
				&(xout[i]),&(yout[i]));
		}
	} else {
		for(i = 0; i< n; i++) {
			if((xmissing != NULL)&&(*xmissing == x[i]))	
				xmis = 1;
			if((ymissing != NULL)&&(*ymissing == y[i]))	
				ymis = 1;
			strans(x0,x0+width,y0-height,y0,iinstance->irtrans.ul,
				iinstance->irtrans.ur, iinstance->irtrans.ub,
				iinstance->irtrans.ut, x[i],y[i],
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
	float*	xmissing;
	float*	ymissing;
#endif
{
	NhlErrorTypes ret = NOERROR;
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;

	if(n == 1) {
		ret = _NhlEvalSplineCoordForward(&(iinstance->irtrans.thecoord),
			*x,*y,xout,yout,xmissing,ymissing);
	} else {
		ret = _NhlMultiEvalSplineCoordForward(	
		      &(iinstance->irtrans.thecoord),x,y,xout,yout,n,n,xmissing,ymissing);
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
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing)
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
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;

	if(n == 1) {
		ret = _NhlEvalSplineCoordInverse(&(iinstance->irtrans.thecoord),
			*x,*y,xout,yout,xmissing,ymissing);
	} else {
		ret = _NhlMultiEvalSplineCoordInverse(	
		      &(iinstance->irtrans.thecoord),x,y,xout,yout,n,n,xmissing,ymissing);
	}
	return(ret);
}


/*
 * Function:	IrCompcToWin
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
static NhlErrorTypes IrCompcToWin
#if  __STDC__
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing,float* ymissing)
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
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;

	if(n == 1) {
		ret = _NhlEvalSplineCoordInverse(&(iinstance->irtrans.thecoord),
			*x,*y,xout,yout,xmissing,ymissing);
	} else {
		ret = _NhlMultiEvalSplineCoordInverse(	
		      &(iinstance->irtrans.thecoord),x,y,xout,yout,n,n,xmissing,ymissing);
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
static NhlErrorTypes IrWinToCompc
#if  __STDC__
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing)
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
	float*  ymissing;
#endif
{
	NhlErrorTypes ret = NOERROR;
	IrregularTransObjLayer iinstance = (IrregularTransObjLayer)instance;

	if(n == 1) {
		ret = _NhlEvalSplineCoordForward(&(iinstance->irtrans.thecoord),
			*x,*y,xout,yout,xmissing,ymissing);
	} else {
		ret = _NhlMultiEvalSplineCoordForward(	
		      &(iinstance->irtrans.thecoord),x,y,xout,yout,n,n,xmissing,ymissing);
	}
	return(ret);
}

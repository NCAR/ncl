
/*
 *      $Id: LogLinTransObj.c,v 1.2 1993-05-27 19:11:17 ethan Exp $
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
 *	Date:		Wed Nov 4 16:38:57 MST 1992
 *
 *	Description:	
 */
#include <stdio.h>
#include <ncarg/hlu/hluutil.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/LogLinTransObjP.h>
#include <ncarg/hlu/View.h>
#include <math.h>


static NhlResource resources[] = {
	{ NhlNtrXMinF,NhlCtrXMinF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinTransObjLayerRec,lltrans.x_min),
		NhlTString,"0.0"},
	{ NhlNtrXMaxF,NhlCtrXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinTransObjLayerRec,lltrans.x_max),
		NhlTString,"0.0"},
	{ NhlNtrXLog,NhlCtrXLog,NhlTInteger,sizeof(int),
		NhlOffset(LogLinTransObjLayerRec,lltrans.x_log),
		NhlTString,"0" },
	{ NhlNtrXReverse,NhlCtrXReverse,NhlTInteger,sizeof(int),
		NhlOffset(LogLinTransObjLayerRec,lltrans.x_reverse),
		NhlTString,"0"},
	{ NhlNtrYMinF,NhlCtrYMinF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinTransObjLayerRec,lltrans.y_min),
		NhlTString,"0.0"},
	{ NhlNtrYMaxF,NhlCtrYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinTransObjLayerRec,lltrans.y_max),
		NhlTString,"0.0"},
	{ NhlNtrYLog,NhlCtrYLog,NhlTInteger,sizeof(int),
		NhlOffset(LogLinTransObjLayerRec,lltrans.y_log),
		NhlTString,"0" },
	{ NhlNtrYReverse,NhlCtrYReverse,NhlTInteger,sizeof(int),
		NhlOffset(LogLinTransObjLayerRec,lltrans.y_reverse),
		NhlTString,"0"}
};

/*
* BaseClass Methods defined
*/

static NhlErrorTypes  LlTransSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes LlTransInitialize(
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

static NhlErrorTypes LlNDCLineTo(
#if     NhlNeedProto
Layer   /* instance */,
Layer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);
static NhlErrorTypes LlDataLineTo(
#if     NhlNeedProto
Layer   /* instance */,
Layer   /* parent */,
float   /* x */,
float   /* y */,
int     /* upordown */
#endif
);






static NhlErrorTypes LlSetTrans(
#ifdef NhlNeedProto
Layer	/*instance*/,
Layer  /*parent*/
#endif
);

static NhlErrorTypes LlWinToNDC(
#ifdef NhlNeedProto
Layer	/*instance*/,
Layer	/* parent */,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*  /*ymissing*/
#endif
);


static NhlErrorTypes LlNDCToWin(
#ifdef NhlNeedProto
Layer	/*instance*/,
Layer	/*parent */,
float*	/*x*/,
float*   /*y*/,
int	/* n*/,
float*	/*xout*/,
float*	/*yout*/,
float*	/*xmissing*/,
float*  /*ymissing*/
#endif
);


LogLinTransObjLayerClassRec logLinTransObjLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&transObjLayerClassRec,
/* class_name			*/	"LogLinTransObj",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(LogLinTransObjLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	NULL,
/* class_inited			*/	False,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	LlTransInitialize,
/* layer_set_values		*/	LlTransSetValues,
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
/* set_trans		*/	LlSetTrans,
/* trans_type		*/	NULL,
/* win_to_ndc		*/	LlWinToNDC,
/* ndc_to_win		*/	LlNDCToWin,
/* data_to_win		*/	NULL, /* One To One for this Transformation */
/* win_to_data		*/	NULL, /* One To One for this Transformation */
/* data_to_compc	*/	NULL,
/* compc_to_data	*/	NULL,
/* win_to_compc		*/	NULL,
/* compc_to_win		*/	NULL,
/* data_lineto 		*/      LlDataLineTo,
/* compc_lineto 	*/      LlDataLineTo,
/* win_lineto 		*/      LlDataLineTo,
/* NDC_lineto 		*/      LlNDCLineTo
        },
	{
		NULL
	}
};

LayerClass logLinTransObjLayerClass = (LayerClass)&logLinTransObjLayerClassRec;




/*
 * Function:	LlTransSetValues
 *
 * Description:	SetValues method for LogLinTrans Objects
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
static NhlErrorTypes LlTransSetValues
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
	LogLinTransObjLayer lnew = (LogLinTransObjLayer) new;
	float tmp;

	lnew->lltrans.ul = lnew->lltrans.x_min;
	lnew->lltrans.ur = lnew->lltrans.x_max;
	lnew->lltrans.ut = lnew->lltrans.y_max;
	lnew->lltrans.ub = lnew->lltrans.y_min;
	if(lnew->lltrans.x_reverse) {
		tmp = lnew->lltrans.ul;
		lnew->lltrans.ul = lnew->lltrans.ur;
		lnew->lltrans.ur = tmp;
	}
	if(lnew->lltrans.y_reverse) {
		tmp = lnew->lltrans.ut;
		lnew->lltrans.ut = lnew->lltrans.ub;
		lnew->lltrans.ub = tmp;
	}
	if((lnew->lltrans.y_log)&&(lnew->lltrans.x_log)) {
		lnew->lltrans.log_lin_value = 4;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrXMax or NhlNtrXMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrYMax or NhlNtrYMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
	} else if(lnew->lltrans.x_log) {
		lnew->lltrans.log_lin_value = 3;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrXMax or NhlNtrXMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
	} else if(lnew->lltrans.y_log) {
		lnew->lltrans.log_lin_value = 2;
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrYMax or NhlNtrYMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
	} else {
		lnew->lltrans.log_lin_value = 1;
	}
	return(NOERROR);

}

/*
 * Function:	LlTransInitialize
 *
 * Description: Initialize function for LogLinTransObjs. Performs same
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
static NhlErrorTypes LlTransInitialize
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
	LogLinTransObjLayer lnew = (LogLinTransObjLayer) new;
	float tmp;

	lnew->lltrans.ul = lnew->lltrans.x_min;
	lnew->lltrans.ur = lnew->lltrans.x_max;
	lnew->lltrans.ut = lnew->lltrans.y_max;
	lnew->lltrans.ub = lnew->lltrans.y_min;
	if(lnew->lltrans.x_reverse) {
		tmp = lnew->lltrans.ul;
		lnew->lltrans.ul = lnew->lltrans.ur;
		lnew->lltrans.ur = tmp;
	}
	if(lnew->lltrans.y_reverse) {
		tmp = lnew->lltrans.ut;
		lnew->lltrans.ut = lnew->lltrans.ub;
		lnew->lltrans.ub = tmp;
	}
	if((lnew->lltrans.x_log)&&(lnew->lltrans.y_log)) {
		lnew->lltrans.log_lin_value = 4;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrXMax or NhlNtrXMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrYMax or NhlNtrYMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
	} else if(lnew->lltrans.x_log) {
		lnew->lltrans.log_lin_value = 3;
		if((lnew->lltrans.x_min <= 0.0)||(lnew->lltrans.x_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrXMax or NhlNtrXMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
	} else if(lnew->lltrans.y_log) {
		lnew->lltrans.log_lin_value = 2;
		if((lnew->lltrans.y_min <= 0.0)||(lnew->lltrans.y_max<=0.0)){	
			NhlPError(FATAL,E_UNKNOWN,"LlSetValues: Either NhlNtrYMax or NhlNtrYMin has been set to <= 0 for a log transformation");
			return(FATAL);
		}
	} else {
		lnew->lltrans.log_lin_value = 1;
	}
	return(NOERROR);

}
/*
 * Function:	LlSetTrans
 *
 * Description: set_trans method for LogLinTransObjs. The current instance
 *		and the parent of the instance are needed. The parent 
 *		provides current screen position information (x,y,width,height)
 *		these are not set through resources because one transformation
 *		needs to possibly be shared by multiple plots.
 *
 * In Args:	instance    is the instance of the LogLinTransObj 
 *		parent	    is the parent of the transform
 *
 * Out Args:	NONE
 *
 * Return Values: Error Status
 *
 * Side Effects:  GKS state altered.
 */

static NhlErrorTypes LlSetTrans
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
	LogLinTransObjLayer linstance = (LogLinTransObjLayer)instance;
	NhlErrorTypes ret;
	

	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x,
		NhlNvpYF,&y,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if(ret < WARNING) {
		return(ret);
	}

	c_set(x,x+width,y-height,y,linstance->lltrans.ul,linstance->lltrans.ur,
		linstance->lltrans.ub,linstance->lltrans.ut,linstance->lltrans.log_lin_value);

	
	return(NOERROR);
	
}


/*
 * Function:	LlWinToNDC
 *
 * Description: Computes the current forward tranformation of the points x and
 *		y to NDC based on the current viewport of the parent. It is
 *		important that this routine not depend on a static screen 
 *		orientation because the parents view may have been transformed.
 *
 * In Args:	instance is the LogLinTransObj and parent is the plot.
 *		(x,y) are the coordinates in data space.
 *		(xout,yout) are the coordinate in Normalized device coordinates.
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes LlWinToNDC
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
	float*	xmissing;
	float*	ymissing;
#endif
{
	float x0;
	float y0;
	float width;
	int i;
	float height;
	LogLinTransObjLayer linstance = (LogLinTransObjLayer)instance;
	NhlErrorTypes ret;
	float urtmp,ultmp,uttmp,ubtmp;
	int xmis = 0;int ymis = 0;
	
	
	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if( ret < WARNING)
		return(ret);
	switch(linstance->lltrans.log_lin_value) {
		case 4:
/*
*XLogYLog case
*/
			urtmp = (float)log10(linstance->lltrans.ur);
			ultmp = (float)log10(linstance->lltrans.ul);
			uttmp = (float)log10(linstance->lltrans.ut);
			ubtmp = (float)log10(linstance->lltrans.ub);
	
			if((xmissing == NULL)&&(ymissing==NULL)) {	
				for(i = 0; i< n; i++) {
					strans( ultmp, urtmp, ubtmp, uttmp, 
						x0,x0+width,y0-height,y0,
						(float)log10(x[i]),
						(float)log10(y[i]), 
						&(xout[i]),&(yout[i]));
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)
							&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)
							&&(*ymissing == y[i]))
						ymis = 1;
					strans( ultmp, urtmp, ubtmp, uttmp, 
						x0,x0+width,y0-height,y0,
						(float)log10(x[i]),
						(float)log10(y[i]), 
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
			break;
		case 3:
/*
*XLogYLin case
*/
			urtmp = (float)log10(linstance->lltrans.ur);
			ultmp = (float)log10(linstance->lltrans.ul);
		
			if((xmissing == NULL)&&(ymissing==NULL)) {	
				for(i = 0; i< n; i++) {
				strans( ultmp, urtmp, linstance->lltrans.ub, 
					linstance->lltrans.ut, x0,x0+width,
					y0-height,y0,(float)log10(x[i]),y[i], 
					&(xout[i]),&(yout[i]));
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)
							&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)
							&&(*ymissing == y[i]))
						ymis = 1;
				strans( ultmp, urtmp, linstance->lltrans.ub, 
					linstance->lltrans.ut, x0,x0+width,
					y0-height,y0,(float)log10(x[i]),y[i], 
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
			break;
		case 2:
/*
*XLinYLog case
*/
			uttmp = (float)log10(linstance->lltrans.ut);
			ubtmp = (float)log10(linstance->lltrans.ub);
			if((xmissing == NULL)&&(ymissing==NULL)) {	
				for(i = 0; i< n; i++) {
				strans( linstance->lltrans.ul, 
					linstance->lltrans.ur, ubtmp,uttmp, 
					x0,x0+width,y0-height,y0,
					x[i],(float)log10(y[i]),
					&(xout[i]),&(yout[i]));
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)
							&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)
							&&(*ymissing == y[i]))
						ymis = 1;
				strans( linstance->lltrans.ul, 
					linstance->lltrans.ur, ubtmp,uttmp, 
					x0,x0+width,y0-height,y0,
					x[i],(float)log10(y[i]),
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
			break;
		case 1:
/*
*XLinYLin
*/
			if((xmissing == NULL)&&(ymissing==NULL)) {	
				for(i = 0; i< n; i++) {
				strans( linstance->lltrans.ul, 
					linstance->lltrans.ur, 
					linstance->lltrans.ub, 
					linstance->lltrans.ut, 
					x0,x0+width,y0-height,y0,
					x[i],y[i], &(xout[i]),&(yout[i]));
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)
							&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)
							&&(*ymissing == y[i]))
						ymis = 1;
				strans( linstance->lltrans.ul, 
					linstance->lltrans.ur, 
					linstance->lltrans.ub, 
					linstance->lltrans.ut, 
					x0,x0+width,y0-height,y0,
					x[i],y[i], &(xout[i]),&(yout[i]));
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
			break;
		default:
			NhlPError(FATAL,E_UNKNOWN,"Internal Error in LlNDCToWin");
			return(FATAL);
	}
}


/*
 * Function:	LlNDCToWin
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
static NhlErrorTypes LlNDCToWin
#if  __STDC__
(Layer instance,Layer parent ,float *x,float *y,int n,float* xout,float* yout,float *xmissing, float *ymissing)
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
	LogLinTransObjLayer linstance = (LogLinTransObjLayer)instance;
	NhlErrorTypes ret;
	float urtmp,ultmp,uttmp,ubtmp;
	int xmis = 0; int ymis = 0;
	
	
	ret = NhlGetValues(parent->base.id,
		NhlNvpXF,&x0,
		NhlNvpYF,&y0,
		NhlNvpWidthF,&width,
		NhlNvpHeightF,&height,NULL);
	if( ret < WARNING)
		return(ret);
	switch(linstance->lltrans.log_lin_value) {
		case 4:
/*
*XLogYLog case
*/
			urtmp = (float)log10(linstance->lltrans.ur);
			ultmp = (float)log10(linstance->lltrans.ul);
			uttmp = (float)log10(linstance->lltrans.ut);
			ubtmp = (float)log10(linstance->lltrans.ub);

			if((xmissing == NULL)&&(ymissing == NULL)) {	
				for(i = 0; i< n; i++) {
					strans(x0,x0+width,y0-height,y0,ultmp, 
						urtmp, ubtmp, uttmp, x[i],y[i], 
						&(xout[i]),&(yout[i]));
					xout[i]=(float)pow((double)10.0,
						(double)xout[i]);
					yout[i]=(float) pow((double)10.0,
						(double)yout[i]);
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)&&(*ymissing == y[i]))
						ymis = 1;
		
					strans(x0,x0+width,y0-height,y0,ultmp, urtmp, 
						ubtmp, uttmp, x[i],y[i], 
						&(xout[i]),&(yout[i]));
					xout[i]=(float)pow((double)10.0,
							(double)xout[i]);
					yout[i]=(float) pow((double)10.0,
							(double)yout[i]);
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
		
			break;
		case 3:
/*
*XLogYLin case
*/
			urtmp = (float)log10(linstance->lltrans.ur);
			ultmp = (float)log10(linstance->lltrans.ul);
		
			if((xmissing == NULL)&&(ymissing == NULL)) {	
				for(i = 0; i< n; i++) {
					strans(x0,x0+width,y0-height,y0,ultmp,
						urtmp, linstance->lltrans.ub,
						linstance->lltrans.ut, x[i],y[i],
						&(xout[i]),&(yout[i]));
					xout[i] = (float) pow((double)10.0,
						(double) xout[i]);
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)&&(*ymissing == y[i]))
						ymis = 1;
					strans(x0,x0+width,y0-height,y0,ultmp,
						urtmp, linstance->lltrans.ub,
						linstance->lltrans.ut, x[i],y[i],
						&(xout[i]),&(yout[i]));
					xout[i] = (float) pow((double)10.0,
							(double) xout[i]);
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
			break;
		case 2:
/*
*XLinYLog case
*/
			uttmp = (float)log10(linstance->lltrans.ut);
			ubtmp = (float)log10(linstance->lltrans.ub);
			if((xmissing == NULL)&&(ymissing == NULL)) {	
				for(i = 0; i< n; i++) {
					strans(x0,x0+width,y0-height,y0,
						linstance->lltrans.ul,
						linstance->lltrans.ur, ubtmp,uttmp, 
						x[i],y[i],
						&(xout[i]),&(yout[i]));
					yout[i]=(float) pow((double)10.0,
							(double)yout[i]);
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)&&(*ymissing == y[i]))
						ymis = 1;
					strans(x0,x0+width,y0-height,y0,
						linstance->lltrans.ul,
						linstance->lltrans.ur, 
						ubtmp,uttmp, 
						x[i],y[i],
						&(xout[i]),&(yout[i]));
					yout[i]=(float) pow((double)10.0,
							(double)yout[i]);
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
			break;
		case 1:
/*
*XLinYLin
*/
			if((xmissing == NULL)&&(ymissing == NULL)) {	
				for(i = 0; i< n; i++) {
					strans(x0,x0+width,y0-height,y0,
						linstance->lltrans.ul,
						linstance->lltrans.ur, 
						linstance->lltrans.ub,
						linstance->lltrans.ut, 
						x[i],y[i],
						&(xout[i]),&(yout[i]));
				}
			} else {
				for(i = 0; i< n; i++) {
					if((xmissing != NULL)&&(*xmissing == x[i]))
						xmis = 1;
					if((ymissing != NULL)&&(*ymissing == y[i]))
						ymis = 1;
					strans(x0,x0+width,y0-height,y0,
						linstance->lltrans.ul,
						linstance->lltrans.ur, 
						linstance->lltrans.ub,
						linstance->lltrans.ut, 
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
			break;
		default:
			NhlPError(FATAL,E_UNKNOWN,"Internal Error in LlNDCToWin");
			return(FATAL);
	}
}


static NhlErrorTypes LlDataLineTo
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
	LogLinTransObjLayer llinst = (LogLinTransObjLayer)instance;
	static float lastx,lasty;
	static call_frstd = 1;
	float currentx,currenty;
	float xpoints[2];
	float ypoints[2];
	int ix0,ix1,iy0,iy1;
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
		_NhlTransClipLine(llinst->lltrans.x_min,
			llinst->lltrans.x_max,
			llinst->lltrans.y_min,
			llinst->lltrans.y_max,
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

static NhlErrorTypes LlNDCLineTo
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
	LogLinTransObjLayer llinst = (LogLinTransObjLayer)instance;
	static float lastx,lasty;
	static call_frstd = 1;
	float currentx,currenty;
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
		_NhlTransClipLine( xvp, xvp+widthvp, yvp-heightvp, yvp,
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

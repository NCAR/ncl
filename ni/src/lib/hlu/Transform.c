/*
 *      $Id: Transform.c,v 1.22 1995-12-19 20:39:32 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Transform.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 2 16:40:50 MDT 1992
 *
 *	Description:	Provides all subclasses of this class a generic hook
 *			into which are placed functions for the forward and
 *			reverse transformations to support point-n-click.
 *			
 *			LevelOne implies a linear transformation NDC<==>WINDOW
 *			LevelTwo implies a transformation  from  WINDOW<==>DATA
 *			
 *			Level two is used when maps or odd transformations are
 *			used.
 */

#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/TransformP.h>
#include <ncarg/hlu/TransObjP.h>

static NhlResource resources[] = {

/* Begin-documented-resources */

	{ NhlNtfPlotManagerOn,NhlCtfPlotManagerOn,
		  NhlTBoolean,sizeof(NhlBoolean),
		  NhlOffset(NhlTransformLayerRec,trans.plot_manager_on),
		  NhlTImmediate,(NhlPointer)True,0,NULL},

/* End-documented-resources */

	{ NhlNtfOverlayObject,NhlCtfOverlayObject,
		  NhlTPointer,sizeof(NhlPointer),
		  NhlOffset(NhlTransformLayerRec,trans.overlay_object),
		  NhlTImmediate,(NhlPointer)NULL,0,NULL},
	{ NhlNtfOverlayTrans,NhlCtfOverlayTrans,
		  NhlTPointer,sizeof(NhlPointer),
		  NhlOffset(NhlTransformLayerRec,trans.overlay_trans_obj),
		  NhlTImmediate,(NhlPointer)NULL,0,NULL},
	{ NhlNtfOverlayStatus,NhlCtfOverlayStatus,
		  NhlTInteger,sizeof(int),
		  NhlOffset(NhlTransformLayerRec,trans.overlay_status),
		  NhlTImmediate,(NhlPointer)_tfNotInOverlay,0,NULL}
};

static NhlErrorTypes TransformClassPartInit(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

/*
* Transform Methods
*/

static NhlErrorTypes TransformDataToNDC(
#if	NhlNeedProto
	NhlLayer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */,
	float*		/* xout */,
	float*		/* yout */,
	float*		/*xmissing*/,
	float*		/*ymissing*/,
	int*		/*status*/,
	float*		/*out_of_range*/
#endif
);

static NhlErrorTypes TransformNDCToData(
#if	NhlNeedProto
	NhlLayer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */,
	float*		/* xout */,
	float*		/* yout */,
	float*		/*xmissing*/,
	float*		/*ymissing*/,
	int*		/*status*/,
	float*		/*out_of_range*/
#endif
);

static NhlErrorTypes TransformDataPolyline(
#if	NhlNeedProto
	NhlLayer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */
#endif
);

static NhlErrorTypes TransformNDCPolyline(
#if	NhlNeedProto
	NhlLayer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */
#endif
);

NhlTransformClassRec NhltransformClassRec = {
        {
/* class_name			*/      "transformClass",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(NhlTransformLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (NhlClass)&NhlviewClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	TransformClassPartInit,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	NULL,
/* layer_set_values		*/	NULL,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,

/* layer_draw			*/      NULL,

/* layer_pre_draw		*/      NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      NULL,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	{
/* overlay_capability 		*/	_tfNotOverlayCapable,
/* data_to_ndc			*/	TransformDataToNDC,
/* ndc_to_data			*/	TransformNDCToData,
/* data_polyline		*/	TransformDataPolyline,
/* ndc_polyline			*/	TransformNDCPolyline
	}
};
	
NhlClass NhltransformClass = (NhlClass)&NhltransformClassRec;

/*
 * Function:	TransformClassPartInit
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
static NhlErrorTypes
TransformClassPartInit
#if	NhlNeedProto
(
	NhlClass	lc
)
#else
(lc)
	NhlClass	lc;
#endif
{
	NhlTransformClass	tlc = (NhlTransformClass)lc;
	NhlTransformClass	sc = (NhlTransformClass)
						lc->base_class.superclass;

	if(tlc->trans_class.data_to_ndc == NhlInheritTransFunc)
		tlc->trans_class.data_to_ndc = sc->trans_class.data_to_ndc;
	if(tlc->trans_class.ndc_to_data == NhlInheritTransFunc)
		tlc->trans_class.ndc_to_data = sc->trans_class.ndc_to_data;

	if(tlc->trans_class.data_polyline == NhlInheritPolyTransFunc)
		tlc->trans_class.data_polyline = sc->trans_class.data_polyline;
	if(tlc->trans_class.ndc_polyline == NhlInheritPolyTransFunc)
		tlc->trans_class.ndc_polyline = sc->trans_class.ndc_polyline;
	return NhlNOERROR;
}

/*
 * Function:	TransformDataToNDC
 *
 * Description: Transform Data space to NDC space method for the Transform. 
 *		Takes one or more x,y pairs of data points and converts them
 *		into their respective NDC values.
 *
 * In Args:	plot	instance record
 *		x	array of x axis values in data coordinates
 * 		y	array of y axis values in data coordinates
 *		n	number of elements
 *		xout	storage provided for output x ndc vals
 *		yout	storage provided for output y ndc vals
 *		xmissing	holds value of missing value in x if NULL
 *				no missing value
 *		ymissing	holds value of missing value in y if NULL
 *				no missing value
 *
 * Out Args:	xout	does not allocate storage
 *		yout	does not allocate storage
 *		status  1 if an out of range condition occurs; else 0;
 *		out_of_range	the out of range indicator value that
 *				replaces all out of range values.
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes TransformDataToNDC
#if	NhlNeedProto
(NhlLayer plot,float* x,float* y,int n,
 float* xout,float* yout,float* xmissing,float* ymissing, 
 int *status, float * out_of_range)
#else
(plot,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	NhlLayer		plot;
	float		*x;
	float		*y;
	int		n;
	float		*xout;
	float		*yout;
	float		*xmissing;
	float		*ymissing;
	int		*status;
	float		*out_of_range;
#endif
{
	char			*entry_name = "TransformNDCToData";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlTransformLayerPart	*tfp = &(((NhlTransformLayer) plot)->trans);
	NhlLayer		top;
	int			mystatus = 0;

	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {
		top = tfp->overlay_trans_obj;
	}
	else {
		if ((top = tfp->trans_obj) == NULL) {
			e_text = 
			   "%s: no transformation object recorded for pid %d";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
				  entry_name,plot->base.id);
			return(ret);
	        }
	}

	subret = _NhlSetTrans(top, plot);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	NhlVAGetValues(top->base.id,
		     NhlNtrOutOfRangeF,out_of_range,NULL);

	subret = _NhlDataToWin(top,x,y,n,xout,yout,
			       &mystatus,xmissing,ymissing);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error transforming from NDC to window";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return ret;
	}
	*status = mystatus ? 1 : 0;

	subret = _NhlWinToNDC(top,xout,yout,n,xout,yout,
			      &mystatus,out_of_range,out_of_range);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error transforming from window to data";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return ret;
	}
	*status = mystatus ? 1 : *status;

	return(ret);

}

/*
 * Function:	TransformNDCToData
 *
 * Description:	Transform objects NDC to Data method for the Transform. 
 *		Takes one or more x,y pairs of NDC points and converts them
 *		into their respective data values.
 *
 * In Args:	plot 	instance record pointer
 *		x	array of x ndc vals to convert
 *		y	array of y ndc vals to convert
 *		n	number of elements in x,y,xout and yout
 *	 	xout	storage provided by user for conversion output
 *		yout	storage provided by user for conversion output
 *		xmissing  missing values in x input
 *		ymissing  missing values in y input
 *
 * Out Args:	xout	but does not allocate storage
 *		yout	but does not allocate storage
 *		status  1 if an out of range condition occurs; else 0;
 *		out_of_range	the out of range indicator value that
 *				replaces all out of range values.
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	 NONE
 */
static NhlErrorTypes TransformNDCToData
#if	NhlNeedProto
(NhlLayer plot,float* x,float* y,int n,
 float* xout,float* yout,float *xmissing,float *ymissing,
 int *status, float * out_of_range)
#else
(plot,x,y,n,xout,yout,xmissing,ymissing,status,out_of_range)
	NhlLayer		plot;
	float*		x;
	float*		y;
	int		n;
	float*		xout;
	float*		yout;
	float*		xmissing;
	float*		ymissing;
	int		*status;
	float		*out_of_range;
#endif
{
	char			*entry_name = "TransformNDCToData";
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	char			*e_text;
	NhlTransformLayerPart	*tfp = &(((NhlTransformLayer) plot)->trans);
	NhlLayer		top;
	int			mystatus = 0;

	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {
		top = tfp->overlay_trans_obj;
	}
	else {
		if ((top = tfp->trans_obj) == NULL) {
			e_text = 
			   "%s: no transformation object recorded for pid %d";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
				  entry_name,plot->base.id);
			return(ret);
	        }
	}

	subret = _NhlSetTrans(top, plot);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	NhlVAGetValues(top->base.id,
		     NhlNtrOutOfRangeF,out_of_range,NULL);

	subret = _NhlNDCToWin(top,x,y,n,xout,yout,
			      &mystatus,xmissing,ymissing);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error transforming from NDC to window";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return ret;
	}
	*status = mystatus ? 1 : 0;

	subret = _NhlWinToData(top,xout,yout,n,xout,yout,
			       &mystatus,out_of_range,out_of_range);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error transforming from window to data";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return ret;
	}
	*status = mystatus ? 1 : *status;

	return(ret);

}

/*
 * Function:	TransformDataPolyline
 *
 * Description:	Immediate mode drawing of a polyline whose points are
 *		given in data coordinate space.
 *
 * In Args:	plot 	instance record pointer
 *		x	array of x data coordinate values
 *		y	array of y data coordinate values
 *		n	number of elements in x and y
 *
 * Out Args:	none
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	 NONE
 */
static NhlErrorTypes TransformDataPolyline
#if	NhlNeedProto
(NhlLayer plot,float* x,float* y,int n)
#else
(plot,x,y,n)
	NhlLayer		plot;
	float*		x;
	float*		y;
	int		n;
#endif
{
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlTransformLayer		tl = (NhlTransformLayer) plot;
	NhlTransformLayerPart	*tfp = &(tl->trans);
	NhlTransObjLayer		top;
 	NhlTransObjClass 	tocp;
	char			*e_text;
	char			*entry_name = "TransformDataPolyline";
	int			i;

	if (n < 2) {
		e_text = "%s, not enough points for a line";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		return NhlWARNING;
	}

/* 
 * Set up the transformation based on whether the plot is part of an
 * overlay.
 */
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {
		top = (NhlTransObjLayer) tfp->overlay_trans_obj;
	}
	else {
		if ((top = (NhlTransObjLayer) tfp->trans_obj) == NULL) {
			e_text = 
			   "%s: no transformation object recorded for pid %d";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
				  entry_name,plot->base.id);
			return(ret);
	        }
	}

	tocp = (NhlTransObjClass) (top->base.layer_class);

	subret = _NhlActivateWorkstation(tl->base.wkptr);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	_NhlSetLineInfo(tl->base.wkptr, plot);

/* Set the transformation */

	subret = _NhlSetTrans((NhlLayer) top, plot);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}


/* Do a pen up to the first point */

	subret = _NhlDataLineTo((NhlLayer)top,*x++,*y++,1);

	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Pen down for the remaining lines */

	for (i = 1; i < n; i++) { 
		subret = _NhlDataLineTo((NhlLayer)top,*x++,*y++,0);

		if ((ret = MIN(ret,subret)) < NhlWARNING) 
			return ret;

	}

/*
 * This call ensures a NCAR LASTD call
 */
	subret = _NhlWorkstationLineTo(tl->base.wkptr,0.0,0.0,1);

        subret = _NhlDeactivateWorkstation(tl->base.wkptr);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error deactivating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	return MIN(ret,subret);
}

/*
 * Function:	TransformNDCPolyline
 *
 * Description:	Immediate mode drawing of a polyline whose points are
 *		given in NDC space.
 *
 * In Args:	plot 	instance record pointer
 *		x	array of x NDC values
 *		y	array of y NDC values
 *		n	number of elements in x and y
 *
 * Out Args:	none
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	 NONE
 */
static NhlErrorTypes TransformNDCPolyline
#if	NhlNeedProto
(NhlLayer plot,float* x,float* y,int n)
#else
(plot,x,y,n)
	NhlLayer		plot;
	float*		x;
	float*		y;
	int		n;
#endif
{
	char			*entry_name = "TransformNDCPolyline";
	char			*e_text;
	NhlErrorTypes		ret = NhlNOERROR, subret = NhlNOERROR;
	NhlTransformLayer		tl = (NhlTransformLayer) plot;
	NhlTransformLayerPart	*tfp = &(tl->trans);
	NhlTransObjLayer		top;
 	NhlTransObjClass 	tocp;
	int			i;

	if (n < 2) {
		e_text = "%s, not enough points for a line";
		NhlPError(NhlWARNING,NhlEUNKNOWN,e_text, entry_name);
		return NhlWARNING;
	}

/* 
 * Set up the transformation based on whether the plot is part of an
 * overlay.
 */
	if (tfp->overlay_status == _tfCurrentOverlayMember && 
	    tfp->overlay_trans_obj != NULL) {
		top = (NhlTransObjLayer) tfp->overlay_trans_obj;
	}
	else {
		if ((top = (NhlTransObjLayer) tfp->trans_obj) == NULL) {
			e_text = 
			   "%s: no transformation object recorded for pid %d";
			NhlPError(NhlFATAL,NhlEUNKNOWN,e_text,
				  entry_name,plot->base.id);
			return(ret);
	        }
	}
	tocp = (NhlTransObjClass) (top->base.layer_class);

	subret = _NhlActivateWorkstation(tl->base.wkptr);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error activating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	_NhlSetLineInfo(tl->base.wkptr, plot);

/* Not sure if a set trans is required */

	subret = _NhlSetTrans((NhlLayer) top, plot);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error setting transformation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

/* Do a pen up to the first point */

	subret = _NhlNDCLineTo((NhlLayer)top,*x++,*y++,1);

	if ((ret = MIN(ret,subret)) < NhlWARNING) 
		return ret;

/* Pen down for the remaining lines */

	for (i = 1; i < n; i++) { 
		subret = _NhlNDCLineTo((NhlLayer)top,*x++,*y++,0);

		if ((ret = MIN(ret,subret)) < NhlWARNING) 
			return ret;

	}

/*
 * This call ensures a NCAR LASTD call
 */
	subret = _NhlWorkstationLineTo(tl->base.wkptr,0.0,0.0,1);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error drawing polyline";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

        subret = _NhlDeactivateWorkstation(tl->base.wkptr);

	if ((ret = MIN(ret,subret)) < NhlWARNING) {
		e_text = "%s: error deactivating workstation";
		NhlPError(NhlFATAL,NhlEUNKNOWN,e_text, entry_name);
		return(ret);
	}

	return MIN(ret,subret);
}

/*
 * Function:	_NhlIsOverlayMember
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
NhlBoolean
_NhlIsOverlayMember
#if	NhlNeedProto
(
	int	pid
)
#else
(pid)
	int	pid;
#endif
{
	NhlLayer		l = _NhlGetLayer(pid);
	NhlTransformLayer	tl = NULL;

	if(!l)
		return False;

	if(!_NhlIsTransform(l))
		return False;

	tl = (NhlTransformLayer)l;

	if (tl->trans.overlay_status == _tfCurrentOverlayMember ||
	    tl->view.annomanager_id)
		return True;

	return False;
}

/*
 * Function:	NhlIsTransform
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
NhlBoolean
NhlIsTransform
#if	NhlNeedProto
(
	int	pid
)
#else
(pid)
	int	pid;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l && _NhlIsTransform(l))
		return True;

	return False;
}

/*
 * Function:	nhl_fistransform
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
void _NHLCALLF(nhl_fistransform,NHL_FISTRANSFORM)
#if	NhlNeedProto
(
	int	*id,
	int	*status
)
#else
(id,status)
	int	*id;
	int	*status;
#endif
{
	*status = NhlIsTransform(*id);

	return;
}

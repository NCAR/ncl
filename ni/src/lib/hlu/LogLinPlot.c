/*
 *      $Id: LogLinPlot.c,v 1.1 1993-11-20 01:06:05 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		LogLinPlot.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Creates and manages a generic Log-Linear plot that
 *			can serve as an overlay base plot or be used for
 *			plots that do not fit into any of the other hlu
 *			plot object catagories.
 */

#include <stdio.h>
#include <math.h>
#include <ncarg/hlu/LogLinPlotP.h>
#include <ncarg/hlu/Workstation.h>

static NhlResource resources[] = {
	{ NhlNllOverlayPlotBase,NhlCllOverlayPlotBase,
		NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(LogLinPlotLayerRec,llplot.overlay_plot_base),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNllXMinF,NhlCllXMinF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinPlotLayerRec,llplot.x_min),
		NhlTString,"0.0"},
	{ NhlNllXMaxF,NhlCllXMaxF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinPlotLayerRec,llplot.x_max),
		NhlTString,"1.0"},
	{ NhlNllXLog,NhlCllXLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(LogLinPlotLayerRec,llplot.x_log),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNllXReverse,NhlCllXReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(LogLinPlotLayerRec,llplot.x_reverse),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNllYMinF,NhlCllYMinF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinPlotLayerRec,llplot.y_min),
		NhlTString,"0.0"},
	{ NhlNllYMaxF,NhlCllYMaxF,NhlTFloat,sizeof(float),
		NhlOffset(LogLinPlotLayerRec,llplot.y_max),
		NhlTString,"1.0"},
	{ NhlNllYLog,NhlCllYLog,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(LogLinPlotLayerRec,llplot.y_log),
		NhlTImmediate,(NhlPointer)False},
	{ NhlNllYReverse,NhlCllYReverse,NhlTBoolean,sizeof(NhlBoolean),
		NhlOffset(LogLinPlotLayerRec,llplot.y_reverse),
		NhlTImmediate,(NhlPointer)False}
};

/* base methods */


static NhlErrorTypes LogLinPlotClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes LogLinPlotClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes LogLinPlotInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes LogLinPlotSetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes LogLinPlotDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes LogLinPlotDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

/*
* Transform Methods
*/

static NhlErrorTypes LogLinPlotDataToNDC(
#ifdef NhlNeedProto
	Layer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */,
	float*		/* xout */,
	float*		/* yout */,
	float*		/*xmissing*/,
	float*		/*ymissing*/
#endif
);

static NhlErrorTypes LogLinPlotNDCToData(
#ifdef NhlNeedProto
	Layer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */,
	float*		/* xout */,
	float*		/* yout */,
	float*		/*xmissing*/,
	float*		/*ymissing*/
#endif
);

static NhlErrorTypes LogLinPlotDataPolyline(
#ifdef NhlNeedProto
	Layer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */
#endif
);

static NhlErrorTypes LogLinPlotNDCPolyline(
#ifdef NhlNeedProto
	Layer		/* plot */,
	float*		/* x */,
	float*		/* y */,
	int		/* n */
#endif
);

static NhlErrorTypes SetUpTransObj(
#ifdef NhlNeedProto
	LogLinPlotLayer	xnew,
	LogLinPlotLayer	xold,
	NhlBoolean	init
#endif
);

LogLinPlotLayerClassRec logLinPlotLayerClassRec = {
        {
/* class_name			*/      "LogLinPlot",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(LogLinPlotLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	NULL,
/* num_resources		*/	0,
/* all_resources		*/	NULL,

/* class_part_initialize	*/	LogLinPlotClassPartInitialize,
/* class_initialize		*/	LogLinPlotClassInitialize,
/* layer_initialize		*/	LogLinPlotInitialize,
/* layer_set_values		*/	LogLinPlotSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	LogLinPlotDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      LogLinPlotDraw,

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
/* handles_overlays 		*/	True,
/* data_to_ndc			*/	LogLinPlotDataToNDC,
/* ndc_to_data			*/	LogLinPlotNDCToData,
/* data_polyline		*/	LogLinPlotDataPolyline,
/* ndc_polyline			*/	LogLinPlotNDCPolyline
	},
	{
/* foo				*/	NULL
	}
};
	
LayerClass logLinPlotLayerClass = (LayerClass)&logLinPlotLayerClassRec;


/*
 * Function:	LogLinPlotClassInitialize
 *
 * Description:
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
LogLinPlotClassInitialize
#if __STDC__
(
	void
)
#else
()
#endif
{

	return NOERROR;
}

/*
 * Function:	LogLinPlotClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		LogLinPlotLayerClassPart that cannot be initialized statically.
 *		Calls _NhlRegisterChildClass for the overlay manager object.
 *
 * In Args:	
 *		LayerClass	lc	Layer Class to init
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */

static NhlErrorTypes
LogLinPlotClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	LayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes lret = NOERROR;

	/*
	 * Register children objects
	 */
	lret = _NhlRegisterChildClass(lc,overlayLayerClass,False,False,
				      NULL);
	_NhlInitializeLayerClass(logLinTransObjLayerClass);

	return MIN(lret,ret);
}


/*
 * Function:	LogLinPlotInitialize
 *
 * Description: 
 *
 * In Args: 	class	objects layer_class
 *		req	instance record of requested values
 *		new	instance record of new object
 *		args	list of resources and values for reference
 *		num_args 	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	state change in GKS due to mapping transformations.
 */
/*ARGSUSED*/
static NhlErrorTypes
LogLinPlotInitialize
#if     __STDC__
(
	LayerClass	class,
	Layer		req,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        LayerClass      class;
        Layer           req;
        Layer           new;
        _NhlArgList     args;
        int             num_args;
#endif
{
	LogLinPlotLayer		lnew = (LogLinPlotLayer) new;
	LogLinPlotLayerPart	*lp = &(lnew->llplot);
	TransformLayerPart	*tp = &(lnew->trans);
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			buffer[MAXFNAMELEN];
	int			tmpid = -1;
	char			*e_text;

/* Initialize fields in the transform layer */

	tp->overlay_trans = NULL;
	tp->plot_trans = NULL;
	tp->title = NULL;
	tp->tic_marks = NULL;
	tp->legend = NULL;
	tp->labelbar = NULL;

/* Initialize fields in the loglinplot layer */

	lp->trans = NULL;
	lp->overlay = NULL;
	
/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (LogLinPlotLayer) req, True);
	ret = MIN(ret,subret);

/* Set up the overlay if required */

	if (lp->overlay_plot_base == True) {
		strcpy(buffer,lnew->base.name);
		strcat(buffer,".Overlay");
		subret = _NhlCreateChild(&tmpid,buffer,overlayLayerClass,
					 new,NULL);
		ret = MIN(ret,subret);
	}

	if(tmpid > -1 && ret >= WARNING) {
		lp->overlay = _NhlGetLayer(tmpid);
	} else {
		e_text = "%s: initialization failure";
		NhlPError(FATAL,E_UNKNOWN,e_text,"LogLinPlotInitialize");
		return(FATAL);
	} 

	return ret;
}

/*
 * Function:	LogLinPlotSetValues
 *
 * Description: 
 *
 * In Args:	old	copy of old instance record
 *		reference	requested instance record
 *		new	new instance record	
 *		args 	list of resources and values for reference
 *		num_args	number of elements in args.
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes LogLinPlotSetValues
#if  __STDC__
(
	Layer		old,
	Layer		reference,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	Layer		old;
	Layer		reference;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	LogLinPlotLayer		lnew = (LogLinPlotLayer) new;
	LogLinPlotLayerPart	*lp = &(lnew->llplot);
	TransformLayerPart	*tp = &(lnew->trans);
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;

/* Set a warning if user has tried to modify overlay status */
	
	if (lp->overlay_plot_base && lp->overlay == NULL) {
		e_text = "%s: Attempt to set create only resource";
		NhlPError(WARNING,E_UNKNOWN,e_text,LogLinPlotSetValues);
		ret = MIN(ret,WARNING);
	}

/* Set up the loglin transformation */

	subret = SetUpTransObj(lnew, (LogLinPlotLayer) old, True);
	ret = MIN(ret,subret);

	return ret;
}

/*
 * Function:	LogLinPlotDestroy
 *
 * Description:
 *
 * In Args:	inst		instance record pointer
 *
 * Out Args:	NONE
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes LogLinPlotDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	LogLinPlotLayerPart	*lp = &(((LogLinPlotLayer) inst)->llplot);
	NhlErrorTypes		ret = NOERROR;

	if (lp->overlay != NULL) {
		(void) _NhlDestroyChild(lp->overlay->base.id,inst);
	}
	if (lp->trans != NULL) {
		(void) NhlDestroy(lp->trans->base.id);
	}
	
	return(ret);
}

/*
 * Function:	LogLinPlotDraw
 *
 * Description:	Draw method for the LogLinPlot object.
 *
 * In Args:	layer	LogLinPlot instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes LogLinPlotDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	LogLinPlotLayer		l = (LogLinPlotLayer) layer;
	NhlErrorTypes		ret = NOERROR;

	return ret;
}

/*
 * Function:	LogLinPlotDataToNDC
 *
 * Description: This is the Data to NDC method of the transform class. It
 *		maps data to normalized device coordinates using the LogLinPlot
 *		object's TransObj which is referenced through 
 *		xplot->llplot.thetrans . The tranformation is set using 
 *		_NhlSetTrans and then mapped using the TranObjs method entry
 *		points _NhlDataToWin and then _NhlWinToNDC. This is the 
 *		standard way in which plot objects will present their 
 *		data transformation funtions to the user. Having these
 *		functions call the TransObj instead of haveing the user call
 *		it directly is to facilitate the eventual support of overlays
 * 		These will require the plot object to intercede in the 
 *		data tranformation progress.
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
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes LogLinPlotDataToNDC
#if __STDC__
(Layer plot,float* x,float* y,int n,float* xout,float* yout,float* xmissing,float* ymissing)
#else
(plot,x,y,n,xout,yout,xmissing,ymissing)
	Layer		plot;
	float*		x;
	float*		y;
	int		n;
	float*		xout;
	float*		yout;
	float*		xmissing;
	float*		ymissing;
#endif
{
	LogLinPlotLayer xplot = (LogLinPlotLayer)plot;
	int istrans = 0;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

#if 0
	 ret = _NhlSetTrans(xplot->llplot.thetrans,plot);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"XyPlotDataToNDC: A FATAL error occured while setting the tranformation of XyPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1 )
		ret1 = ret; 
		

	ret = _NhlDataToWin(xplot->llplot.thetrans,plot,x,y,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"LogLinPlotNDCToData: A FATAL error occured while transforming input to window, LogLinPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1)
		ret1 = ret;

	if(!istrans) {
		if(x != xout)
		memcpy((char*)xout,(char*)x,sizeof(float)*n);
		if(y != yout)
		memcpy((char*)yout,(char*)y,sizeof(float)*n);
	}

	istrans = 0;
	ret = _NhlWinToNDC(xplot->llplot.thetrans,plot,xout,yout,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"LogLinPlotNDCToData: A FATAL error occured while transforming from window to NDC, LogLinPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if( ret < ret1)
		ret1 = ret;
#endif

	return(ret1);

}
/*
 * Function:	LogLinPlotNDCToData
 *
 * Description:	Transform objects NDC to Data method for the LogLinPlot. 
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
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	 NONE
 */
static NhlErrorTypes LogLinPlotNDCToData
#if __STDC__
(Layer plot,float* x,float* y,int n,float* xout,float* yout,float *xmissing,float *ymissing)
#else
(plot,x,y,n,xout,yout,xmissing,ymissing)
	Layer		plot;
	float*		x;
	float*		y;
	int		n;
	float*		xout;
	float*		yout;
	float*		xmissing;
	float*		ymissing;
#endif
{
	LogLinPlotLayer xplot = (LogLinPlotLayer)plot;
	int istrans = 0;
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes ret1 = NOERROR;

#if 0
	ret = _NhlSetTrans(xplot->llplot.thetrans,plot);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"LogLinPlotNDCToData: A FATAL error occured while setting the tranformation of LogLinPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	ret = _NhlNDCToWin(xplot->llplot.thetrans,plot,x,y,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING){
		NhlPError(FATAL,E_UNKNOWN,"LogLinPlotNDCToData: A FATAL error occured while transforming input to window, LogLinPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;


	if(!istrans) {
		if(x != xout)
		memcpy((char*)xout,(char*)x,sizeof(float)*n);
		if(y != yout)
		memcpy((char*)yout,(char*)y,sizeof(float)*n);
	}


	istrans = 0;
	ret = _NhlWinToData(xplot->llplot.thetrans,plot,xout,yout,n,xout,yout,
		&istrans,xmissing,ymissing);
	if(ret < WARNING) {
		NhlPError(FATAL,E_UNKNOWN,"LogLinPlotNDCToData: A FATAL error occured while transforming from window to data, LogLinPlot object: %s , cannot continue",plot->base.name);
		return(ret);
	} else if(ret < ret1)
		ret1 = ret;

#endif

	return(ret1);

}

/*
 * Function:	LogLinPlotDataPolyline
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
static NhlErrorTypes LogLinPlotDataPolyline
#if __STDC__
(Layer plot,float* x,float* y,int n)
#else
(plot,x,y,n)
	Layer		plot;
	float*		x;
	float*		y;
	int		n;
#endif
{
	NhlErrorTypes ret = NOERROR;

	return ret;
}

/*
 * Function:	LogLinPlotNDCPolyline
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
static NhlErrorTypes LogLinPlotNDCPolyline
#if __STDC__
(Layer plot,float* x,float* y,int n)
#else
(plot,x,y,n)
	Layer		plot;
	float*		x;
	float*		y;
	int		n;
#endif
{
	NhlErrorTypes ret = NOERROR;

	return ret;
}

/*
 * Function:	SetUpTransObjs
 *
 * Description: Creates, Sets and Destroys the main tranformation object
 *		for the LogLinPlot. For log and linear plots the tranformation
 *		object is not destroyed when changes in rsources affecting
 *		the tranformation are changed (i.e. data extents). However,
 *		IrregularType2TransObjs have to be freed whenever the 
 *		data extent increases but not when it decreases.  The 
 *		LogLinTransObjs are only destroyed when the style is switched
 *		from log or linear to irregular.  This function uses two 
 *		switch statements to switch through the 25 possible combinations
 *		of (XStyle, YStyle). This is needed since one tranformation 
 *		object handles both x and y axis. The only real tricks here
 *		happen when either XStyle or YStyle is IRREGULAR and the other
 *		is not. When this happens an IrregularTransObj is created 
 *		and one of the IrregularTranObj is "fooled" into a linear
 *		or log tranformation. This is facilitated for log axis by 
 *		a resource that instructs the IrragularTransObj to take
 *		the logs of the input values and create an approximation of
 *		the logs of the data values.
 *
 * In Args:	xnew	new instance record
 *		xold	old instance record if calledfrom == SET
 *		init	true if initialization
 *
 * Out Args:	NONE
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
static NhlErrorTypes SetUpTransObj
#if  __STDC__
(
	LogLinPlotLayer	xnew,
	LogLinPlotLayer	xold,
	NhlBoolean	init
)
#else 
(xnew,xold,init)
	LogLinPlotLayer	xnew;
	LogLinPlotLayer	xold;
	NhlBoolean	init;
#endif
{
#if 0
	NhlSArg		sargs[30];
	int		nargs = 0;
	char		buffer[MAXRESNAMLEN];
	int		tmpid;
	float		tmpcoords[3];
	char		*error_lead=NULL;
	LayerClass	trans_class = NULL;
	NhlGenArray	gen;
	LogLinPlotLayerPart	*newxy = &xnew->llplot;
	LogLinPlotLayerPart	*oldxy=NULL;

/*
 * Now create main transformation object
 */	
	if(calledfrom == CREATE){
		error_lead = "LogLinPlotInitialize";
	}
	else{
		oldxy = &xold->llplot;

		if(calledfrom == SET){
			error_lead = "LogLinPlotSetValues";
		}
		else if (calledfrom == DATACHANGE){
		/*
		 * If we are coming from UpdateData - The only resources that
		 * could have changed are min and max - if they haven't changed
		 * return immediately.
		 */
			if((newxy->x_min == oldxy->x_min) &&
				(newxy->x_max == oldxy->x_max) &&
				(newxy->y_min == oldxy->y_min) &&
				(newxy->y_max == oldxy->y_max)){
				return NOERROR;
			}
			error_lead = "LogLinPlotUpdateData";
		}
	}

	/*
	 * If a new trans object needs to be created, do this.
	 */
	if(	(newxy->thetrans == NULL)
		||
		(calledfrom == CREATE)
		||
		(	(	(newxy->x_style == IRREGULAR) ||
				(newxy->y_style == IRREGULAR)
			) &&
			!oldxy->have_irreg_trans
		)
		||
		(	(newxy->x_style != IRREGULAR) &&
			(newxy->y_style != IRREGULAR) &&
			oldxy->have_irreg_trans
		)
									){

		if(newxy->thetrans != NULL){
			(void)NhlDestroy(newxy->thetrans->base.id);
			newxy->thetrans = NULL;
		}

		sprintf(buffer,"%s",xnew->base.name);
		strcat(buffer,".Trans");

		newxy->fake_x = newxy->fake_y = False;

		if(newxy->y_style == IRREGULAR){

			trans_class = irregularType2TransObjLayerClass;
			newxy->have_irreg_trans = True;

			gen = newxy->y_irregular_points;
			NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								gen->data);
			NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
							gen->len_dimensions[0]);
			NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newxy->y_tension);

			if(newxy->x_style == IRREGULAR){

				gen = newxy->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
							gen->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);
			}
			else{

				newxy->fake_x = True;
				newxy->fake_x_min = tmpcoords[0] = newxy->x_min;
				newxy->fake_x_max = tmpcoords[2] = newxy->x_max;

				if(newxy->x_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->x_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
					NhlSetSArg(&sargs[nargs++],
							NhlNtrXUseLog,True);
				}

				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);

			}
		}
		/*
		 * Y is not IRREG
		 */
		else{
			if(newxy->x_style == IRREGULAR){

				trans_class = irregularType2TransObjLayerClass;
				newxy->have_irreg_trans = True;

				gen = newxy->x_irregular_points;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								gen->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
							gen->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);

				newxy->fake_y = True;
				newxy->fake_y_min = tmpcoords[0] = newxy->y_min;
				newxy->fake_y_max = tmpcoords[2] = newxy->y_max;

				if(newxy->y_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->y_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
					NhlSetSArg(&sargs[nargs++],
							NhlNtrYUseLog,True);
				}

				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);

			}
			/*
			 * X is not IRREG
			 */
			else{
				trans_class = logLinTransObjLayerClass;
				newxy->have_irreg_trans = False;

				if(newxy->x_style == LOG)
					NhlSetSArg(&sargs[nargs++],NhlNtrXLog,
									True);
				if(newxy->y_style == LOG)
					NhlSetSArg(&sargs[nargs++],NhlNtrYLog,
									True);

			}
		}
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,newxy->x_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,newxy->x_max);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,newxy->y_min);
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,newxy->y_max);

		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,newxy->x_reverse);
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,newxy->y_reverse);

		(void)NhlALCreate(&tmpid,buffer,trans_class,xnew->base.id,
								sargs,nargs);

		newxy->thetrans = _NhlGetLayer(tmpid);
		if(newxy->thetrans == NULL){
			NhlPError(FATAL,E_UNKNOWN,
				"%s:Unable to continue without transformation",
								error_lead);
			return FATAL;
		}

		return NOERROR;
	}

	/*
	 * SetValues/UpdateData in existing trans object
	 */

	/*
	 * if we are tricking an irreg object into being a log or lin - take
	 * care of setting the transformation.
	 */
	if(newxy->have_irreg_trans){
		if(newxy->fake_x){
			if(newxy->x_style == IRREGULAR){
				newxy->fake_x = False;
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
					newxy->x_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,
				newxy->x_irregular_points->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrXTensionF,
							newxy->x_tension);
			}
			else if((newxy->x_style != oldxy->x_style) ||
				(newxy->x_min < newxy->fake_x_min) ||
				(newxy->x_max > newxy->fake_x_max)){

				newxy->fake_x_min = tmpcoords[0] = newxy->x_min;
				newxy->fake_x_max = tmpcoords[2] = newxy->x_max;
				if(newxy->x_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->x_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);
			}
		}
		else {
			if(newxy->x_style != IRREGULAR){
				newxy->fake_x = True;
				newxy->fake_x_min = tmpcoords[0] = newxy->x_min;
				newxy->fake_x_max = tmpcoords[2] = newxy->x_max;
				if(newxy->x_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->x_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrXCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrXNumPoints,3);

			}
		}
		if(newxy->fake_y){
			if(newxy->y_style == IRREGULAR){
				newxy->fake_y = False;
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
					newxy->y_irregular_points->data);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,
				newxy->y_irregular_points->len_dimensions[0]);
				NhlSetSArg(&sargs[nargs++],NhlNtrYTensionF,
							newxy->y_tension);
			}
			else if((newxy->y_style != oldxy->y_style) ||
				(newxy->y_min < newxy->fake_y_min) ||
				(newxy->y_max > newxy->fake_y_max)){

				newxy->fake_y_min = tmpcoords[0] = newxy->y_min;
				newxy->fake_y_max = tmpcoords[2] = newxy->y_max;
				if(newxy->y_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->y_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);
			}
		}
		else {
			if(newxy->y_style != IRREGULAR){
				newxy->fake_y = True;
				newxy->fake_y_min = tmpcoords[0] = newxy->y_min;
				newxy->fake_y_max = tmpcoords[2] = newxy->y_max;
				if(newxy->y_style == LINEAR){
					tmpcoords[1] =
						(tmpcoords[0]+tmpcoords[2])/2.0;
				}
				else if(newxy->y_style == LOG){
					tmpcoords[1] =
					(float)pow(10.0,(log10(tmpcoords[0] +
						log10(tmpcoords[2])) / 2.0));
				}
				NhlSetSArg(&sargs[nargs++],NhlNtrYCoordPoints,
								tmpcoords);
				NhlSetSArg(&sargs[nargs++],NhlNtrYNumPoints,3);

			}
		}
	}
		
	if(newxy->x_min != oldxy->x_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMinF,newxy->x_min);
	if(newxy->x_max != oldxy->x_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrXMaxF,newxy->x_max);
	if(newxy->y_min != oldxy->y_min)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMinF,newxy->y_min);
	if(newxy->y_max != oldxy->y_max)
		NhlSetSArg(&sargs[nargs++],NhlNtrYMaxF,newxy->y_max);

	if(newxy->x_reverse != oldxy->x_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrXReverse,newxy->x_reverse);
	if(newxy->y_reverse != oldxy->y_reverse)
		NhlSetSArg(&sargs[nargs++],NhlNtrYReverse,newxy->y_reverse);

	if(newxy->x_style != oldxy->x_style){
		if(newxy->have_irreg_trans)
			NhlSetSArg(&sargs[nargs++],NhlNtrXUseLog,
						(newxy->x_style == LOG));
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrXLog,
						(newxy->x_style == LOG));
	}

	if(newxy->y_style != oldxy->y_style){
		if(newxy->have_irreg_trans)
			NhlSetSArg(&sargs[nargs++],NhlNtrYUseLog,
						(newxy->y_style == LOG));
		else
			NhlSetSArg(&sargs[nargs++],NhlNtrYLog,
						(newxy->y_style == LOG));
	}

	return NhlALSetValues(newxy->thetrans->base.id,sargs,nargs);
#endif
	return NOERROR;
}

/*
 *      $Id: Overlay.c,v 1.3 1994-01-12 00:34:50 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Overlay.c
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 16 15:18:58 MST 1993
 *
 *	Description:	Manages the drawing for plot objects that must
 *			share a common overlay transformation
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/OverlayP.h>


static NhlResource resources[] = {

	{ NhlNovOverlayIds,NhlCovOverlayIds,NhlTGenArray,sizeof(NhlPointer),
		  NhlOffset(OverlayLayerRec,overlay.overlay_ids),
		  NhlTImmediate,NULL},
	{ NhlNovPreDrawOrder,NhlCovPreDrawOrder,NhlTGenArray,
		  sizeof(NhlPointer),
		  NhlOffset(OverlayLayerRec,overlay.pre_draw_order),
		  NhlTImmediate,NULL},
	{ NhlNovPostDrawOrder,NhlCovPostDrawOrder,NhlTGenArray,
		  sizeof(NhlPointer),
		  NhlOffset(OverlayLayerRec,overlay.post_draw_order),
		  NhlTImmediate,NULL},
	{ NhlNovMasterOverlay,NhlCovMasterOverlay,NhlTBoolean,
		  sizeof(NhlBoolean),
		  NhlOffset(OverlayLayerRec,overlay.master_overlay),
		  NhlTImmediate,(NhlPointer) True},
	{ NhlNovOverlayRecs,NhlCovOverlayRecs,NhlTGenArray,
		  sizeof(NhlPointer),
		  NhlOffset(OverlayLayerRec,overlay.sub_ov_recs),
		  NhlTImmediate,NULL},
/*
 * Annotation resources
 */

	{ NhlNovDisplayTitles,NhlCovDisplayTitles,
		  NhlTInteger,sizeof(int),
		  NhlOffset(OverlayLayerRec,overlay.display_titles),
		  NhlTImmediate,(NhlPointer) Nhl_ovNever},
	{ NhlNovDisplayTickMarks,NhlCovDisplayTickMarks,
		  NhlTInteger,sizeof(int),
		  NhlOffset(OverlayLayerRec,overlay.display_tickmarks),
		  NhlTImmediate,(NhlPointer) Nhl_ovNever},
	{ NhlNovDisplayLabelBar,NhlCovDisplayLabelBar,NhlTInteger,sizeof(int),
		  NhlOffset(OverlayLayerRec,overlay.display_labelbar),
		  NhlTImmediate,(NhlPointer) Nhl_ovNever},
	{ NhlNovDisplayLegend,NhlCovDisplayLegend,NhlTInteger,sizeof(int),
		  NhlOffset(OverlayLayerRec,overlay.display_legend),
		  NhlTImmediate,(NhlPointer) Nhl_ovNever},
/*
 * Intercepted tick mark resources
 */
	{ NhlNtmXBDataLeftF, NhlCtmXBDataLeftF,NhlTFloat, sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.x_b_data_left),
		  NhlTString,"0.0" },
	{ NhlNtmXBDataRightF, NhlCtmXBDataRightF,NhlTFloat, sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.x_b_data_right),
		  NhlTString,"1.0" },
	{ NhlNtmYLDataBottomF, NhlCtmYLDataBottomF,NhlTFloat, sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.y_l_data_bottom),
		  NhlTString,"0.0" },
	{ NhlNtmYLDataTopF, NhlCtmYLDataTopF,NhlTFloat, sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.y_l_data_top),
		  NhlTString,"1.0" },
		
/*
 * Intercepted title resources
 */

	{NhlNtiMainOffsetXF,NhlCtiMainOffsetXF,NhlTFloat,sizeof(float),
		 NhlOffset(OverlayLayerRec,overlay.ti_main_offset_x),
		 NhlTString,"0.0"},
	{NhlNtiXAxisOffsetXF,NhlCtiXAxisOffsetXF,NhlTFloat,sizeof(float),
		 NhlOffset(OverlayLayerRec,overlay.ti_x_axis_offset_x),
		 NhlTString,"0.0"},
	{NhlNtiYAxisOffsetYF,NhlCtiYAxisOffsetYF,NhlTFloat,sizeof(float),
		 NhlOffset(OverlayLayerRec,overlay.ti_y_axis_offset_y),
		 NhlTString,"0.0"},
	{NhlNtiXAxisPosition,NhlCtiXAxisPosition,NhlTTitlePositions,
		 sizeof(TitlePositions),
		 NhlOffset(OverlayLayerRec,overlay.ti_x_axis_position),
		 NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiYAxisPosition,NhlCtiYAxisPosition,NhlTTitlePositions,
		 sizeof(TitlePositions),
		 NhlOffset(OverlayLayerRec,overlay.ti_y_axis_position),
		 NhlTImmediate,(NhlPointer)CENTER},
	{NhlNtiMainPosition,NhlCtiMainPosition,NhlTTitlePositions,
		 sizeof(TitlePositions),
		 NhlOffset(OverlayLayerRec,overlay.ti_main_position),
		 NhlTImmediate,(NhlPointer)CENTER},

/* LabelBar resources */

	{ NhlNovLabelBarWidthF, NhlCovLabelBarWidthF,NhlTFloat, sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lbar_width),
		  NhlTString,"0.2" },
	{ NhlNovLabelBarHeightF, NhlCovLabelBarHeightF,NhlTFloat, 
		  sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lbar_height),
		  NhlTString,"0.5" },
	{ NhlNovLabelBarXOffsetF, NhlCovLabelBarXOffsetF,NhlTFloat, 
		  sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lbar_x_off),
		  NhlTString,"0.02" },
	{ NhlNovLabelBarYOffsetF, NhlCovLabelBarYOffsetF,NhlTFloat, 
		  sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lbar_y_off),
		  NhlTString,"0.00" },
	{NhlNovLabelBarSide, NhlCovLabelBarSide, NhlTPosition, 
		 sizeof(NhlJustification),
		 NhlOffset(OverlayLayerRec,overlay.lbar_side),
		 NhlTImmediate,(NhlPointer)NhlRIGHT},
	{NhlNovLabelBarPosition, NhlCovLabelBarPosition, NhlTPosition, 
		 sizeof(NhlJustification),
		 NhlOffset(OverlayLayerRec,overlay.lbar_pos),
		 NhlTImmediate,(NhlPointer)NhlCENTER},

/* intercepted LabelBar resources */

	{NhlNlbLabelBar, NhlClbLabelBar, NhlTBoolean, 
		 sizeof(NhlBoolean),
		 NhlOffset(OverlayLayerRec,overlay.lbar_on),
		 NhlTImmediate,(NhlPointer)False},
	{NhlNlbJustification, NhlClbJustification, NhlTJustification, 
		 sizeof(NhlJustification),
		 NhlOffset(OverlayLayerRec,overlay.lbar_just),
		 NhlTImmediate,(NhlPointer)NhlCENTERLEFT},
	{NhlNlbOrientation, NhlClbOrientation, NhlTOrientation, 
		 sizeof(NhlOrientation),
		 NhlOffset(OverlayLayerRec,overlay.lbar_orient),
		 NhlTImmediate,(NhlPointer)NhlVERTICAL},

/* Legend resources */

	{ NhlNovLegendWidthF, NhlCovLegendWidthF,NhlTFloat, sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lgnd_width),
		  NhlTString,"0.4" },
	{ NhlNovLegendHeightF, NhlCovLegendHeightF,NhlTFloat, 
		  sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lgnd_height),
		  NhlTString,"0.2" },
	{ NhlNovLegendXOffsetF, NhlCovLegendXOffsetF,NhlTFloat, 
		  sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lgnd_x_off),
		  NhlTString,"0.00" },
	{ NhlNovLegendYOffsetF, NhlCovLegendYOffsetF,NhlTFloat, 
		  sizeof(float),
		  NhlOffset(OverlayLayerRec,overlay.lgnd_y_off),
		  NhlTString,"0.02" },
	{NhlNovLegendSide, NhlCovLegendSide, NhlTPosition, 
		 sizeof(NhlPosition),
		 NhlOffset(OverlayLayerRec,overlay.lgnd_side),
		 NhlTImmediate,(NhlPointer)NhlBOTTOM},
	{NhlNovLegendPosition, NhlCovLegendPosition, NhlTPosition, 
		 sizeof(NhlPosition),
		 NhlOffset(OverlayLayerRec,overlay.lgnd_pos),
		 NhlTImmediate,(NhlPointer)NhlCENTER},

/* intercepted Legend resources */

	{NhlNlgLegend, NhlClgLegend, NhlTBoolean, 
		 sizeof(NhlBoolean),
		 NhlOffset(OverlayLayerRec,overlay.lgnd_on),
		 NhlTImmediate,(NhlPointer)False},
	{NhlNlgJustification, NhlClgJustification, NhlTJustification, 
		 sizeof(NhlJustification),
		 NhlOffset(OverlayLayerRec,overlay.lgnd_just),
		 NhlTImmediate,(NhlPointer)NhlCENTERLEFT},
	{NhlNlgOrientation, NhlClgOrientation, NhlTOrientation, 
		 sizeof(NhlOrientation),
		 NhlOffset(OverlayLayerRec,overlay.lgnd_orient),
		 NhlTImmediate,(NhlPointer)NhlVERTICAL},
};

/* base methods */


static NhlErrorTypes OverlayClassInitialize(
#ifdef NhlNeedProto
	void
#endif
);

static NhlErrorTypes OverlayClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	lc
#endif
);

static NhlErrorTypes OverlayInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes OverlaySetValues(
#ifdef NhlNeedProto
        Layer,          /* old */
        Layer,          /* reference */
        Layer,          /* new */
        _NhlArgList,    /* args */
        int             /* num_args*/
#endif
);

static NhlErrorTypes 	OverlayGetValues(
#ifdef NhlNeedProto
	Layer,		/* l */
	_NhlArgList, 	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes OverlayDestroy(
#ifdef NhlNeedProto
        Layer           /* inst */
#endif
);

static NhlErrorTypes OverlayDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes OverlayPreDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

static NhlErrorTypes OverlayPostDraw(
#ifdef NhlNeedProto
        Layer   /* layer */
#endif
);

/* internal static functions */

static NhlErrorTypes ManageAnnotations(
#ifdef NhlNeedProto
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,				       
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes ManageTitles(
#ifdef NhlNeedProto
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init				       
#endif
);

static NhlErrorTypes ManageTickMarks(
#ifdef NhlNeedProto
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init				       
#endif
);

static NhlErrorTypes ManageLabelBar(
#ifdef NhlNeedProto
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,		       
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes ManageLegend(
#ifdef NhlNeedProto
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,			       
	_NhlArgList	args,
	int		num_args
#endif
);
static NhlErrorTypes ManageLegend(
#ifdef NhlNeedProto
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,			       
	_NhlArgList	args,
	int		num_args
#endif
);

static NhlErrorTypes RestoreOverlayBase(
#ifdef NhlNeedProto
	OverlayLayerPart	*ovp,
	int			plot_number
#endif
);

static NhlErrorTypes RemoveOverlayBase(
#ifdef NhlNeedProto
	OverlayLayerPart	*ovp,
	int			plot_number
#endif
);

static NhlErrorTypes DissolveOverlay(
#ifdef NhlNeedProto
	Layer		overlay_object
#endif
);

OverlayLayerClassRec overlayLayerClassRec = {
        {
/* class_name			*/      "Overlay",
/* nrm_class			*/      NrmNULLQUARK,
/* layer_size			*/      sizeof(OverlayLayerRec),
/* class_inited			*/      False,
/* superclass			*/      (LayerClass)&transformLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	OverlayClassPartInitialize,
/* class_initialize		*/	OverlayClassInitialize,
/* layer_initialize		*/	OverlayInitialize,
/* layer_set_values		*/	OverlaySetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	OverlayGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	OverlayDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/      OverlayDraw,

/* layer_pre_draw		*/      OverlayPreDraw,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/      OverlayPostDraw,
/* layer_clear			*/      NULL

        },
	{
/* segment_wkid			*/	0,
/* get_bb			*/	NULL
	},
	{
/* overlay_capability 		*/	_tfNotOverlayCapable,
/* data_to_ndc			*/	NULL,
/* ndc_to_data			*/	NULL,
/* data_polyline		*/	NULL,
/* ndc_polyline			*/	NULL
	},
	{
/* foo				*/	NULL
	}
};

LayerClass overlayLayerClass = (LayerClass)&overlayLayerClassRec;

static NrmQuark Overlay_Ids;
static NrmQuark Overlay_Recs;
static NrmQuark Pre_Draw_Order;
static NrmQuark Post_Draw_Order;

/* static variables referenced by the low-level library mapping functions */

static Layer Trans_Obj = NULL;
static Layer Plot = NULL;
static Layer Overlay_Trans_Obj = NULL;
static Layer Overlay_Plot = NULL;

/*
 * Function:	OverlayClassInitialize
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
OverlayClassInitialize
#if __STDC__
(
	void
)
#else
()
#endif
{
	Overlay_Ids = NrmStringToQuark(NhlNovOverlayIds);
	Overlay_Recs = NrmStringToQuark(NhlNovOverlayRecs);
	Pre_Draw_Order = NrmStringToQuark(NhlNovPreDrawOrder);
	Post_Draw_Order = NrmStringToQuark(NhlNovPostDrawOrder);

	return NOERROR;
}

/*
 * Function:	OverlayClassPartInitialize
 *
 * Description:	This function initializes fields in the 
 *		OverlayLayerClassPart that cannot be initialized statically.
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
OverlayClassPartInitialize
#if	__STDC__
(
	LayerClass	lc	/* Layer Class to init	*/
)
#else
(lc)
	LayerClass	lc;	/* Layer Class to init	*/
#endif
{
	NhlErrorTypes ret = NOERROR, subret = NOERROR;

	subret = _NhlRegisterChildClass(lc,tickMarkLayerClass,
					False,False,
					NhlNtmXBDataLeftF,
					NhlNtmXBDataRightF,
					NhlNtmYLDataBottomF,
					NhlNtmYLDataTopF,
					NULL);

	if ((ret = MIN(subret,ret)) < WARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,titleLayerClass,False,False,
					NhlNtiMainOffsetXF,
					NhlNtiXAxisOffsetXF, 
					NhlNtiYAxisOffsetYF, 
					NhlNtiXAxisPosition, 
					NhlNtiYAxisPosition, 
					NhlNtiMainPosition,
					NULL);

	if ((ret = MIN(subret,ret)) < WARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,labelBarLayerClass,
					False,False,
					NhlNlbLabelBar,
					NhlNlbJustification,
					NhlNlbOrientation,
					NULL);

	if ((ret = MIN(subret,ret)) < WARNING)
		return ret;

	subret = _NhlRegisterChildClass(lc,legendLayerClass,
					False,False,
					NhlNlgLegend,
					NhlNlgJustification,
					NhlNlgOrientation,
					NULL);

	return MIN(subret,ret);
}


/*
 * Function:	OverlayInitialize
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
 * Side Effects:
 */
/*ARGSUSED*/
static NhlErrorTypes
OverlayInitialize
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
	NhlErrorTypes		ret = NOERROR;
	char			*e_text;
	char			*entry_name = "OverlayInitialize";
	OverlayLayer		ovnew = (OverlayLayer) new;
	OverlayLayerPart	*ovp = &(ovnew->overlay);
	TransformLayer		parent = (TransformLayer)ovnew->base.parent;
	ovRec			*ov_rec;
	int			i;

/*
 * Make sure the transformation supplied is valid
 */
	if (parent->trans.trans_obj == NULL ||
	    ! _NhlIsTransObj(parent->trans.trans_obj)) {
		e_text = "%s: invalid transformation supplied";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return(FATAL);
	}
	ovp->overlay_trans_obj = parent->trans.trans_obj;

/*
 * Allocate an array to store pointers to the overlay records. 
 * Then allocate an array for the first member element.
 */

	if ((ovp->ov_recs = (ovRec **) 
	     NhlMalloc(NhlOV_ALLOC_UNIT * sizeof(ovRec *))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	if ((ov_rec = (ovRec *) 
	     NhlMalloc(NhlOV_ALLOC_UNIT * sizeof(ovRec))) == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	ovp->overlay_alloc = NhlOV_ALLOC_UNIT;
	ovp->overlay_count = 1;
	ov_rec->plot = parent;
	ov_rec->ov_obj = new;
	ovp->ov_recs[0] = ov_rec;
       
	for (i = ovp->overlay_count; i < ovp->overlay_alloc; i++) 
		ovp->ov_recs[i] = NULL;

	ret = ManageAnnotations(ovnew,(OverlayLayer)req,True,args,num_args);
 
	return ret;
}

/*
 * Function:	OverlaySetValues
 *
 * Description: If the parent creates a new overlay trans object, or
 *		separates its own trans object from the overlay trans
 *		object it must notify the overlay object via a SetValues.
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
static NhlErrorTypes OverlaySetValues
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
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "OverlaySetValues";
	OverlayLayer		ovnew = (OverlayLayer) new;
	OverlayLayer		ovold = (OverlayLayer) old;
	OverlayLayerPart	*ovp = &(ovnew->overlay);
        NhlSArg			sargs[16];
        int			nargs = 0;
	int			i;

/*
 * If the Overlay records resource is set replace the old overlay record
 * pointer array with the new one.
 */
	if (_NhlArgIsSet(args,num_args,NhlNovOverlayRecs)) {

		ovRec	**ov_recs = (ovRec **) ovp->sub_ov_recs->data;
		int	new_count = ovp->sub_ov_recs->num_elements;
		
		if (ov_recs == NULL || ! _NhlIsTransform(ov_recs[0]->plot)) {
		     e_text = "%s: internally invalid overlay record resource";
		     NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		     return FATAL;
		}
		if (new_count > ovp->overlay_alloc) {
			ovp->ov_recs = (ovRec **)
				NhlRealloc(ovp->ov_recs, sizeof(ovRec *) *
					   MAX(new_count,ovp->overlay_alloc + 
					       NhlOV_ALLOC_UNIT));
			if (ovp->ov_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
				return FATAL;
			}
			ovp->overlay_alloc = MAX(new_count,ovp->overlay_alloc +
						 NhlOV_ALLOC_UNIT);
		}
		for (i = 0; i < MIN(new_count,ovp->overlay_count); i++) {
			memcpy(ovp->ov_recs[i], ov_recs[i],sizeof(ovRec));
		}
			
		for (i=MIN(new_count,ovp->overlay_count);i<new_count;i++) {
			if ((ovp->ov_recs[i] = (ovRec *) 
			     NhlMalloc(sizeof(ovRec))) == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
				return FATAL;
			}
			memcpy(ovp->ov_recs[i], ov_recs[i], sizeof(ovRec));
		}
		for (i = new_count; i < ovp->overlay_count; i++) {
			NhlFree(ovp->ov_recs[i]);
			ovp->ov_recs[i] = NULL;
		}
		ovp->overlay_count = new_count;
	}

	subret = ManageAnnotations(ovnew,ovold,False,args,num_args);
	if ((ret = MIN(ret,subret)) < WARNING) 
		return ret;
/*
 * Only a master overlay with member plots needs to execute the remaining code
 */

	if (! ovp->master_overlay || ovp->overlay_count < 2)
		return ret;

/*
 * If the base view has changed, modify the view of each overlay to 
 * match it.
 */
	if (ovnew->view.x != ovold->view.x)
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovnew->view.x);
	if (ovnew->view.y != ovold->view.y)
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovnew->view.y);
	if (ovnew->view.width != ovold->view.width)
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovnew->view.width);
	if (ovnew->view.height != ovold->view.height)
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovnew->view.height);
	
/* Start at 1 here because 0 is the base plot */

	for (i = 1; i < ovp->overlay_count; i++) {

		subret = NhlALSetValues(ovp->ov_recs[i]->plot->base.id,
					sargs,nargs);
		if ((ret = MIN(subret, ret)) < WARNING) {
			e_text = "%s: error setting overlay plot view";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
	}
		
	return ret;
}

/*
 * Function:	OverlayGetValues
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
 *	Memory is allocated when the following resources are retrieved:
 *		NhlNovOverlayIds
 *	The user is responsible for freeing this memory.
 */

static NhlErrorTypes	OverlayGetValues
#if __STDC__
(Layer l, _NhlArgList args, int num_args)
#else
(l,args,num_args)
	Layer	l;
	_NhlArgList	args;
	int	num_args;
#endif
{
	char			*entry_name = "OverlayGetValues";
	char			*e_text;
	OverlayLayerPart	*ovp = &((OverlayLayer) l)->overlay;
	int 			i,j;
	int			*ids;
	NhlGenArray		ga;
	ovRec			**ov_recs;

	for ( i = 0; i< num_args; i++ ) {

		if (args[i].quark == Overlay_Ids) {

			if ((ids = (int *) NhlMalloc(ovp->overlay_count * 
						     sizeof(int))) == NULL) {
				
				e_text = "%s: dynamic memory allocation error";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
				return FATAL;
			}

			for (j = 0; j < ovp->overlay_count; j++) {
				ids[j] = ovp->ov_recs[j]->plot->base.id; 
			}
			
			if ((ga = NhlCreateGenArray((NhlPointer)ids,
						    NhlTInteger,sizeof(int),
						    1,&ovp->overlay_count)) 
			    == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNovOverlayIds);
				return FATAL;
			}
			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;
		}
		else if (args[i].quark == Overlay_Recs) {
				
			ov_recs = (ovRec **) 
			      NhlMalloc(ovp->overlay_count * sizeof(ovRec *));
			if (ov_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
				return FATAL;
			}

			for (j = 0; j < ovp->overlay_count; j++) {

				ov_recs[j] = (ovRec *)
					NhlMalloc(sizeof(ovRec));
				if (ov_recs[j] == NULL) {
					e_text = 
					 "%s: dynamic memory allocation error";
					NhlPError(FATAL,E_UNKNOWN,
						  e_text,entry_name);
					return FATAL;
				}

				memcpy(ov_recs[j], 
				       ovp->ov_recs[j],sizeof(ovRec));
			}
			
			ga = NhlCreateGenArray((NhlPointer)ov_recs,
					       NhlTPointer,sizeof(ovRec *),
					       1,&ovp->overlay_count);
			if (ga == NULL) {
				e_text = "%s: error creating %s GenArray";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,
					  NhlNovOverlayRecs);
				return FATAL;
			}

			ga->my_data = True;
			*((NhlGenArray *)(args[i].value)) = ga;
		}
			
	}
	return(NOERROR);
}

/*
 * Function:	OverlayDestroy
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
static NhlErrorTypes OverlayDestroy
#if __STDC__
(Layer inst)
#else
(inst)
Layer inst;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name = "OverlayDestroy";
	char			*e_text;
	OverlayLayerPart	*ovp = &((OverlayLayer) inst)->overlay;
	int			i;

/*
 * If there are overlay members release them
 */
	if (ovp->overlay_count > 1) {
		subret = DissolveOverlay(inst);
		if ((ret = MIN(subret,ret)) < WARNING)
			return ret;
	}

	if (ovp->overlay_count > 1) {
		e_text = "%s: inconsistency in overlay count";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
	}
		
/*
 * Free the overlay base record and the overlay record pointer array
 */
	for (i=0; i < ovp->overlay_count; i++)
		NhlFree(ovp->ov_recs[i]);

	NhlFree(ovp->ov_recs);

	return(ret);
}

/*
 * Function:	OverlayPreDraw
 *
 * Description: Performs the "background" draw for an overlay plot
 *
 * In Args:	layer	Overlay instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes OverlayPreDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "OverlayPreDraw";
	OverlayLayer		ovl = (OverlayLayer) layer;
	OverlayLayerPart	*ovp = &(ovl->overlay);
	int			i;

/*
 * Set the overlay trans.
 */

	Overlay_Trans_Obj = ovp->ov_recs[0]->plot->trans.trans_obj;
	Overlay_Plot = (Layer) ovp->ov_recs[0]->plot;
	if (! _NhlIsTransObj(Overlay_Trans_Obj) || 
	    ! _NhlIsTransform(Overlay_Plot)) {
		e_text = "%s: invalid overlay trans object or plot class";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	subret = _NhlSetTrans(Overlay_Trans_Obj,Overlay_Plot);
	
        if ((ret = MIN(ret,subret)) < WARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the pre-draw methods for each plot object in turn.
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (Layer) ovp->ov_recs[i]->plot;

		subret = _NhlPreDraw(Plot);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: error in plot pre-draw";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(ret);
		}
	}
	return ret;
}


/*
 * Function:	OverlayDraw
 *
 * Description:
 *
 * In Args:	layer	Overlay instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes OverlayDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "OverlayDraw";
	OverlayLayer		ovl = (OverlayLayer) layer;
	OverlayLayerPart	*ovp = &(ovl->overlay);
	int			i;

/*
 * Set the overlay trans.
 */

	Overlay_Trans_Obj = ovp->ov_recs[0]->plot->trans.trans_obj;
	Overlay_Plot = (Layer) ovp->ov_recs[0]->plot;
	if (! _NhlIsTransObj(Overlay_Trans_Obj) || 
	    ! _NhlIsTransform(Overlay_Plot)) {
		e_text = "%s: invalid overlay trans object or plot class";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	subret = _NhlSetTrans(Overlay_Trans_Obj,Overlay_Plot);
	
        if ((ret = MIN(ret,subret)) < WARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the draw methods for each plot object in turn. 
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (Layer) ovp->ov_recs[i]->plot;

		subret = _NhlDraw(Plot);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: error in plot draw";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

	return ret;
}


/*
 * Function:	OverlayPostDraw
 *
 * Description:
 *
 * In Args:	layer	Overlay instance
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

static NhlErrorTypes OverlayPostDraw
#if  __STDC__
(Layer layer)
#else
(layer)
        Layer layer;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "OverlayPostDraw";
	OverlayLayer		ovl = (OverlayLayer) layer;
	OverlayLayerPart	*ovp = &(ovl->overlay);
	int			i;
	NhlBoolean		tickmarks_done = False,
				titles_done = False,
				labelbar_done = False,
				legend_done = False;

/*
 * Set the overlay trans.
 */

	Overlay_Trans_Obj = ovp->ov_recs[0]->plot->trans.trans_obj;
	Overlay_Plot = (Layer) ovp->ov_recs[0]->plot;
	if (! _NhlIsTransObj(Overlay_Trans_Obj) || 
	    ! _NhlIsTransform(Overlay_Plot)) {
		e_text = "%s: invalid overlay trans object or plot class";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	subret = _NhlSetTrans(Overlay_Trans_Obj,Overlay_Plot);
	
        if ((ret = MIN(ret,subret)) < WARNING) {
                e_text = "%s: error setting transformation";
                NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
                return(ret);
        }

/*
 * Call the post-draw methods for each plot object in turn. 
 * The annotation items for now are part of the post draw.
 */

	for (i = 0; i < ovp->overlay_count; i++) {

		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (Layer) ovp->ov_recs[i]->plot;

		subret = _NhlPostDraw(Plot);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: error in plot post-draw";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(ret);
		}

		if (ovp->ov_recs[i]->ov_obj != NULL) {
			OverlayLayerPart *opi = 
			  &(((OverlayLayer)ovp->ov_recs[i]->ov_obj)->overlay);

			if ((opi->display_tickmarks == Nhl_ovAlways) ||
			    (! tickmarks_done && 
			     opi->display_tickmarks == Nhl_ovConditionally)) {

				subret = NhlDraw(opi->tickmarks->base.id);
				if ((ret = MIN(subret,ret)) < WARNING) {
					e_text = "%s: error drawing tickmarks";
					NhlPError(FATAL,E_UNKNOWN,
						  e_text, entry_name);
					return(ret);
				}
				tickmarks_done = True;
			}

			if ((opi->display_titles == Nhl_ovAlways) ||
			    (! titles_done && 
			     opi->display_titles == Nhl_ovConditionally)) {

				subret = NhlDraw(opi->titles->base.id);
				if ((ret = MIN(subret,ret)) < WARNING) {
					e_text = "%s: error drawing titles";
					NhlPError(FATAL,E_UNKNOWN,
						  e_text, entry_name);
					return(ret);
				}
				titles_done = True;
			}

			if ((opi->display_labelbar == Nhl_ovAlways) ||
			    (! labelbar_done && 
			     opi->display_labelbar == Nhl_ovConditionally)) {

				subret = NhlDraw(opi->labelbar->base.id);
				if ((ret = MIN(subret,ret)) < WARNING) {
					e_text = "%s: error drawing labelbar";
					NhlPError(FATAL,E_UNKNOWN,
						  e_text, entry_name);
					return(ret);
				}
				labelbar_done = True;
			}

			if ((opi->display_legend == Nhl_ovAlways) ||
			    (! legend_done && 
			     opi->display_legend == Nhl_ovConditionally)) {

				subret = NhlDraw(opi->legend->base.id);
				if ((ret = MIN(subret,ret)) < WARNING) {
					e_text = "%s: error drawing legend";
					NhlPError(FATAL,E_UNKNOWN,
						  e_text, entry_name);
					return(ret);
				}
				legend_done = True;
			}
		}
	}
	return ret;
}

/*
 * Function:	ManageAnnotations
 *
 * Description: Internal function that manages the title, tickmark, labelbar
 *		and legend objects for both the Initialize and SetValues
 *		routines.
 *
 * In Args:	OverlayLayer	ovnew - The new overlay layer
 *		OverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageAnnotations
#if __STDC__
(
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,args,num_args)
	OverlayLayer	ovnew;
	OverlayLayer	ovold;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	OverlayLayerPart	*ovp = &ovnew->overlay;
	OverlayLayerPart	*oovp = &ovold->overlay;

	if (init) {
		ovp->tickmarks = NULL;
		ovp->titles = NULL;
		ovp->labelbar = NULL;
		ovp->legend = NULL;
	}

/*
 * Manage the individual annotation items. Even if the display value is
 * currently set to never, the item still must be managed if it has ever
 * been created.
 */
 
	if (ovp->display_tickmarks != Nhl_ovNever
	    || ovp->tickmarks != NULL) {
		subret = ManageTickMarks(ovnew,ovold,init);
		if ((ret = MIN(subret,ret)) < WARNING)
			return ret;
	}
	if (ovp->display_titles != Nhl_ovNever
	    || ovp->titles != NULL) {
		subret = ManageTitles(ovnew,ovold,init);
		if ((ret = MIN(subret,ret)) < WARNING)
			return ret;
	}
		
	if (ovp->display_labelbar != Nhl_ovNever
	    || ovp->labelbar != NULL) {
		subret = ManageLabelBar(ovnew,ovold,init,args,num_args);
		if ((ret = MIN(subret,ret)) < WARNING)
			return ret;
	}

	if (ovp->display_legend != Nhl_ovNever
	    || ovp->legend != NULL) {
		subret = ManageLegend(ovnew,ovold,init,args,num_args);
	}

	return MIN(subret,ret);
		
}

/*
 * Function:	ManageTickMarks
 *
 * Description: Internal function that manages the tickmark object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	OverlayLayer	ovnew - The new overlay layer
 *		OverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageTickMarks
#if __STDC__
(
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init				       
)
#else
(ovnew,ovold,init)
	OverlayLayer	ovnew;
	OverlayLayer	ovold;
	NhlBoolean	init;				       
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name;
	char			*e_text;
	OverlayLayerPart	*ovp = &ovnew->overlay;
	OverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	char			buffer[MAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * If not displaying tickmarks just call the SetValues function --
 * this call is only to avoid the no set values called error.
 */
	if (ovp->display_tickmarks == Nhl_ovNever && ovp->tickmarks != NULL) {
		return _NhlALSetValuesChild(ovp->tickmarks->base.id,
					    (Layer)ovnew,sargs,nargs);
	}

/*
 * If no tickmark object exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->tickmarks == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".TickMark");
		subret = _NhlCreateChild(&tmpid,buffer,tickMarkLayerClass,
				(Layer)ovnew,
				NhlNvpXF,ovnew->view.x,
				NhlNvpYF,ovnew->view.y,
				NhlNvpWidthF,ovnew->view.width,
				NhlNvpHeightF,ovnew->view.height,
				NhlNtmXBDataLeftF,ovp->x_b_data_left,
				NhlNtmXBDataRightF,ovp->x_b_data_right,
				NhlNtmYLDataBottomF,ovp->y_l_data_bottom,
				NhlNtmYLDataTopF,ovp->y_l_data_top,
				NULL);
		if ((ret = MIN(ret,subret)) < WARNING || 
		    (ovp->tickmarks = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating TickMark object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	} else {
		if (ovnew->view.x != ovold->view.x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpXF,ovnew->view.x);
		if (ovnew->view.y != ovold->view.y)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpYF,ovnew->view.y);
		if (ovnew->view.width != ovold->view.width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovnew->view.width);
		if (ovnew->view.x != ovold->view.x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovnew->view.height);
		if (ovp->x_b_data_left != oovp->x_b_data_left)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBDataLeftF,ovp->x_b_data_left);
		if (ovp->x_b_data_right != oovp->x_b_data_right)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmXBDataRightF,ovp->x_b_data_right);
		if (ovp->y_l_data_bottom != oovp->y_l_data_bottom)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLDataBottomF,ovp->y_l_data_bottom);
		if (ovp->y_l_data_top != oovp->y_l_data_top)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtmYLDataTopF,ovp->y_l_data_top);

		subret = _NhlALSetValuesChild(ovp->tickmarks->base.id,
					      (Layer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: Error updating TickMark object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	}

	return ret;
}

/*
 * Function:	ManageTitles
 *
 * Description: Internal function that manages the title object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	OverlayLayer	ovnew - The new overlay layer
 *		OverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageTitles
#if __STDC__
(
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init				       
)
#else
(ovnew,ovold,init)
	OverlayLayer	ovnew;
	OverlayLayer	ovold;
	NhlBoolean	init;				       
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name;
	char			*e_text;
	OverlayLayerPart	*ovp = &ovnew->overlay;
	OverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	NhlBoundingBox		bbox;
	char			buffer[MAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * If not displaying titles just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (ovp->display_titles == Nhl_ovNever && ovp->titles != NULL) {
		return _NhlALSetValuesChild(ovp->titles->base.id,
					    (Layer)ovnew,sargs,nargs);
	}
/*
 * Get the bounding box, then set the title positions with respect to it.
 */

	if (ovp->tickmarks != NULL)
		subret = NhlGetBB(ovp->tickmarks->base.id,&bbox);
	else
		subret = NhlGetBB(ovnew->base.id,&bbox);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(FATAL);
	}
	ovp->ti_x = bbox.l; 
	ovp->ti_y = bbox.t;
	ovp->ti_width = bbox.r - bbox.l; 
	ovp->ti_height = bbox.t - bbox.b;
	
	switch(ovp->ti_main_position) {
	case CENTER:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			((ovnew->view.x + ovnew->view.width/2.0) -
			 (ovp->ti_x + ovp->ti_width/2.0));
		break;
	case LEFT:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			(ovnew->view.x - ovp->ti_x);
		break;
	case RIGHT:
		ovp->real_main_offset_x = ovp->ti_main_offset_x +
			((ovnew->view.x + ovnew->view.width) - 
			 (ovp->ti_x + ovp->ti_width));
		break;
	case TOP:
	case BOTTOM:
	default:
		e_text = "%s: Invalid value for main axis title position";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(FATAL);
	}

	switch(ovp->ti_x_axis_position) {
	case CENTER:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			((ovnew->view.x + ovnew->view.width/2.0) -
			 (ovp->ti_x + ovp->ti_width/2.0));
		break;
	case LEFT:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			(ovnew->view.x - ovp->ti_x);
		break;
	case RIGHT:
		ovp->real_x_axis_offset_x = ovp->ti_x_axis_offset_x +
			((ovnew->view.x + ovnew->view.width) - 
			 (ovp->ti_x + ovp->ti_width));
		break;
	case TOP:
	case BOTTOM:
	default:
		e_text = "%s: Invalid value for x axis title position";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(FATAL);
	}

	switch(ovp->ti_y_axis_position) {
	case CENTER:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y +
			((ovnew->view.y - ovnew->view.height/2.0) -
			 (ovp->ti_y - ovp->ti_height/2.0));
		break;
	case TOP:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y +
			(ovnew->view.y - ovp->ti_y);
		break;
	case BOTTOM:
		ovp->real_y_axis_offset_y = ovp->ti_y_axis_offset_y + 
			((ovnew->view.y - ovnew->view.height) - 
			 (ovp->ti_y - ovp->ti_height));
		break;
	case LEFT:
	case RIGHT:
	default:
		e_text = "%s: Invalid value for y axis title position";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return(FATAL);
	}

/*
 * If no title object exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->titles == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".Title");
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiMainOffsetXF,ovp->real_main_offset_x);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiXAxisOffsetXF,ovp->real_x_axis_offset_x);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiYAxisOffsetYF,ovp->real_y_axis_offset_y);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiXAxisPosition,ovp->ti_x_axis_position);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiYAxisPosition,ovp->ti_y_axis_position);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtiMainPosition,ovp->ti_main_position);

		subret = _NhlALCreateChild(&tmpid,buffer,titleLayerClass,
					   (Layer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < WARNING || 
		    (ovp->titles = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating Title object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	} else {
		
		if (ovp->ti_x != oovp->ti_x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpXF,ovp->ti_x);
		if (ovp->ti_y != oovp->ti_y)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpYF,ovp->ti_y);
		if (ovp->ti_width != oovp->ti_width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovp->ti_width);
		if (ovp->ti_height != oovp->ti_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovp->ti_height);
		if (ovp->real_main_offset_x != oovp->real_main_offset_x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiMainOffsetXF,ovp->real_main_offset_x);
		if (ovp->real_x_axis_offset_x != oovp->real_x_axis_offset_x)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiXAxisOffsetXF,
				   ovp->real_x_axis_offset_x);
		if (ovp->real_y_axis_offset_y != oovp->real_y_axis_offset_y)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiYAxisOffsetYF,
				   ovp->real_y_axis_offset_y);
		if (ovp->ti_x_axis_position != oovp->ti_x_axis_position)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiXAxisPosition,
				   ovp->ti_x_axis_position);
		if (ovp->ti_y_axis_position != oovp->ti_y_axis_position)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiYAxisPosition,
				   ovp->ti_y_axis_position);
		if (ovp->ti_main_position != oovp->ti_main_position)
			NhlSetSArg(&sargs[nargs++],
				   NhlNtiMainPosition,
				   ovp->ti_main_position);

		subret = _NhlALSetValuesChild(ovp->titles->base.id,
					      (Layer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: Error updating Title object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	}

	return ret;
}

/*
 * Function:	ManageLabelBar
 *
 * Description: Internal function that manages the labelbar object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	OverlayLayer	ovnew - The new overlay layer
 *		OverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageLabelBar
#if __STDC__
(
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,args,num_args)
	OverlayLayer	ovnew;
	OverlayLayer	ovold;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name;
	char			*e_text;
	OverlayLayerPart	*ovp = &ovnew->overlay;
	OverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	NhlBoundingBox		bbox;
	char			buffer[MAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			width, height;
	NhlJustification	just;
	NhlBoolean		user_just = True, 
				user_x_off = True, 
				user_y_off = True,
				user_orient = True;

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * If not displaying a labelbar just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (ovp->display_labelbar != Nhl_ovNever) {
		if (ovp->display_labelbar != oovp->display_labelbar)
			NhlSetSArg(&sargs[nargs++],NhlNlbLabelBar,True);
	}
	else if(ovp->labelbar != NULL) {
		if (ovp->display_labelbar != oovp->display_labelbar)
			NhlSetSArg(&sargs[nargs++],NhlNlbLabelBar,False);
		return _NhlALSetValuesChild(ovp->labelbar->base.id,
					    (Layer)ovnew,sargs,nargs);
	}
/*
 * If the side has changed make automatic adjustments to LabelBar 
 * orientation, x and y offsets, only if the user has not set these resources 
 */
	if (init || (ovp->lbar_side != oovp->lbar_side) ||
		     (ovp->lbar_pos != oovp->lbar_pos)) {

		if (! _NhlArgIsSet(args,num_args,NhlNlbJustification)) {
			user_just = False;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNlbOrientation)) {
			user_orient = False;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNovLabelBarXOffsetF)) {
			user_x_off = False;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNovLabelBarYOffsetF)) {
			user_y_off = False;
		}

		if (! user_x_off && ! user_y_off) {

			float t;
			NhlBoolean side = 
				(ovp->lbar_side == NhlRIGHT || 
				 ovp->lbar_side == NhlLEFT) ?
					 True : False;

			if (init || (side != (oovp->lbar_side == NhlRIGHT || 
					      oovp->lbar_side == NhlLEFT))) {
				t = MIN(ovp->lbar_x_off,ovp->lbar_y_off);
                                if (side) {
                                        ovp->lbar_x_off = MAX(ovp->lbar_x_off,
                                                              ovp->lbar_y_off);
                                        ovp->lbar_y_off = t;
                                }
                                else {
                                        ovp->lbar_y_off = MAX(ovp->lbar_x_off,
                                                              ovp->lbar_y_off);
                                        ovp->lbar_x_off = t;
                                }
			}
		}
			    
		switch (ovp->lbar_side) {
		case NhlBOTH:
		case NhlCENTER:
		default:
			e_text =
			     "%s: setting invalid %s enumeration value to %s";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNovLabelBarSide, "NhlRIGHT");
			ret = MIN(ret,WARNING);
			ovp->lbar_side = NhlRIGHT;
			/* falling through */
		case NhlRIGHT:
			ovp->lbar_x_off = user_x_off || ovp->lbar_x_off > 0.0 ?
				ovp->lbar_x_off : - ovp->lbar_x_off;
			ovp->lbar_orient = user_orient ? 
				ovp->lbar_orient : NhlVERTICAL;
			break;
		case NhlLEFT:
			ovp->lbar_x_off = user_x_off || ovp->lbar_x_off < 0.0 ?
				ovp->lbar_x_off : - ovp->lbar_x_off;
			ovp->lbar_orient = user_orient ? 
				ovp->lbar_orient : NhlVERTICAL;
			break;
		case NhlTOP:
			ovp->lbar_y_off = user_y_off || ovp->lbar_y_off > 0.0 ?
				ovp->lbar_y_off : - ovp->lbar_y_off;
			ovp->lbar_orient = user_orient ? 
				ovp->lbar_orient : NhlHORIZONTAL;
			break;
		case NhlBOTTOM:
			ovp->lbar_y_off = user_y_off || ovp->lbar_y_off < 0.0 ?
				ovp->lbar_y_off : - ovp->lbar_y_off;
			ovp->lbar_orient = user_orient ? 
				ovp->lbar_orient : NhlHORIZONTAL;
			break;
		}
	}
/*
 * If the orientation changes adjust the width and height, unless these
 * resources are explicitly set.
 */
	if (init || ovp->lbar_orient != oovp->lbar_orient) {

		float t;

		if (! _NhlArgIsSet(args,num_args,NhlNovLabelBarWidthF) &&
		    ! _NhlArgIsSet(args,num_args,NhlNovLabelBarHeightF)) {
			
			if (ovp->lbar_orient == NhlVERTICAL) {
				t = MIN(ovp->lbar_height,ovp->lbar_width);
				ovp->lbar_height = MAX(ovp->lbar_height,
						       ovp->lbar_width);
				ovp->lbar_width = t;
			}
			else  {
				t = MIN(ovp->lbar_height,ovp->lbar_width);
				ovp->lbar_width = MAX(ovp->lbar_height,
						       ovp->lbar_width);
				ovp->lbar_height = t;
			}
		}
	}

/*
 * Get the bounding box, then set the labelbar position with respect to it.
 */

	if (ovp->titles != NULL)
		subret = NhlGetBB(ovp->titles->base.id,&bbox);
	else if (ovp->tickmarks != NULL)
		subret = NhlGetBB(ovp->tickmarks->base.id,&bbox);
	else
		subret = NhlGetBB(ovnew->base.id,&bbox);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(FATAL);
	}
/*
 * Set the position, height and width of the LabelBar based on the
 * side and position resources. Also set the LabelBar justification, (which
 * determines the fixed point about which the labelbar repositions itself
 * if it has to changes size due to a label rotation or text size change, etc.
 */
	if (ovp->lbar_side == NhlRIGHT || ovp->lbar_side == NhlLEFT) {

		height = ovnew->view.height;
		width = bbox.r - bbox.l;

		ovp->lbar_x = (ovp->lbar_side == NhlRIGHT ? bbox.r : 
			  bbox.l - ovp->lbar_width) + ovp->lbar_x_off;
		switch(ovp->lbar_pos) {
		case NhlRIGHT:
		case NhlLEFT:
		case NhlBOTH:
		default:
			e_text =
			     "%s: setting invalid %s enumeration value to %s";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNovLabelBarPosition, "NhlCENTER");
			ret = MIN(ret,WARNING);
			ovp->lbar_pos = NhlCENTER;
			/* falling through */

		case NhlCENTER:
			ovp->lbar_y = ovnew->view.y - height/2.0 + 
				ovp->lbar_y_off + ovp->lbar_height/2.0;
			just = ovp->lbar_side == NhlRIGHT ? 
				NhlCENTERLEFT : NhlCENTERRIGHT;
			break;
		case NhlTOP:
			ovp->lbar_y = ovnew->view.y + ovp->lbar_y_off;
			just = ovp->lbar_side == NhlRIGHT ? 
				NhlTOPLEFT : NhlTOPRIGHT;
			break;
		case NhlBOTTOM:
			ovp->lbar_y = ovnew->view.y - height + 
				ovp->lbar_y_off + ovp->lbar_height;
			just = ovp->lbar_side == NhlRIGHT ? 
				NhlBOTTOMLEFT : NhlBOTTOMRIGHT;
			break;
		}
	}
	else {

		height = bbox.t - bbox.b;
		width = ovnew->view.width;

		ovp->lbar_y = (ovp->lbar_side == NhlTOP ? 
			  bbox.t + ovp->lbar_height : bbox.b) 
			+ ovp->lbar_y_off;
		switch(ovp->lbar_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		case NhlBOTH:
		default:
			e_text =
			     "%s: setting invalid %s enumeration value to %s";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNovLabelBarPosition, "NhlCENTER");
			ret = MIN(ret,WARNING);
			ovp->lbar_pos = NhlCENTER;
			/* falling through */

		case NhlCENTER:
			ovp->lbar_x = ovnew->view.x + width/2.0 + 
				ovp->lbar_x_off - ovp->lbar_width/2.0;
			just = ovp->lbar_side == NhlTOP ? 
				NhlBOTTOMCENTER : NhlTOPCENTER;
			break;
		case NhlRIGHT:
			ovp->lbar_x = ovnew->view.x + width + 
				ovp->lbar_x_off - ovp->lbar_width;
			just = ovp->lbar_side == NhlTOP ? 
				NhlBOTTOMRIGHT : NhlTOPRIGHT;
			break;
		case NhlLEFT:
			ovp->lbar_x = ovnew->view.x + ovp->lbar_x_off;
			just = ovp->lbar_side == NhlTOP ? 
				NhlBOTTOMLEFT : NhlTOPLEFT;
			break;
		}
	}
	if (user_just)
		just = ovp->lbar_just;
	else
		ovp->lbar_just = just;

/*
 * If no title object exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->labelbar == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".LabelBar");
		subret = _NhlCreateChild(&tmpid,buffer,labelBarLayerClass,
					 (Layer)ovnew,
					 NhlNvpXF,ovp->lbar_x,
					 NhlNvpYF,ovp->lbar_y,
					 NhlNvpWidthF,ovp->lbar_width,
					 NhlNvpHeightF,ovp->lbar_height,
					 NhlNlbJustification,ovp->lbar_just,
					 NhlNlbOrientation,ovp->lbar_orient,
					 NULL);
		if ((ret = MIN(ret,subret)) < WARNING || 
		    (ovp->labelbar = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating Title object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	} else {

		if (ovp->lbar_x != oovp->lbar_x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->lbar_x);
		if (ovp->lbar_y != oovp->lbar_y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->lbar_y);
		if (ovp->lbar_width != oovp->lbar_width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovp->lbar_width);
		if (ovp->lbar_height != oovp->lbar_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovp->lbar_height);
		if (ovp->lbar_just != oovp->lbar_just)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlbJustification,ovp->lbar_just);
		if (ovp->lbar_orient != oovp->lbar_orient)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlbOrientation,ovp->lbar_orient);
		subret = _NhlALSetValuesChild(ovp->labelbar->base.id,
					      (Layer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: Error updating Title object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	}



	return ret;
}


/*
 * Function:	ManageLegend
 *
 * Description: Internal function that manages the legend object
 *		for both the Initialize and SetValues routines.
 *
 * In Args:	OverlayLayer	ovnew - The new overlay layer
 *		OverlayLayer	ovold - The old overlay layer
 *		int		init - called from Initialize?
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
ManageLegend
#if __STDC__
(
	OverlayLayer	ovnew,
	OverlayLayer	ovold,
	NhlBoolean	init,
	_NhlArgList	args,
	int		num_args
)
#else
(ovnew,ovold,init,args,num_args)
	OverlayLayer	ovnew;
	OverlayLayer	ovold;
	NhlBoolean	init;				       
	_NhlArgList	args;
	int		num_args;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*entry_name;
	char			*e_text;
	OverlayLayerPart	*ovp = &ovnew->overlay;
	OverlayLayerPart	*oovp = &ovold->overlay;
	int			tmpid = -1;
	NhlBoundingBox		bbox;
	char			buffer[MAXFNAMELEN];
        NhlSArg			sargs[16];
        int			nargs = 0;
	float			width, height;
	NhlJustification	just;
	NhlBoolean		user_just = True; 

	entry_name = (init) ? "OverlayInitialize" : "OverlaySetValues";

/*
 * If not displaying a legend just call the SetValues function --
 * only to avoid the no set values called error.
 */
	if (ovp->display_legend != Nhl_ovNever) {
		if (ovp->display_legend != oovp->display_legend)
			NhlSetSArg(&sargs[nargs++],NhlNlgLegend,True);
	}
	else if(ovp->legend != NULL) {
		if (ovp->display_legend != oovp->display_legend)
			NhlSetSArg(&sargs[nargs++],NhlNlgLegend,False);
		return _NhlALSetValuesChild(ovp->legend->base.id,
					    (Layer)ovnew,sargs,nargs);
	}
/*
 * If the side has changed make automatic adjustments to Legend 
 * orientation, x and y offsets, only if the user has not set these resources 
 */
	if (init || (ovp->lgnd_side != oovp->lgnd_side) ||
		     (ovp->lgnd_pos != oovp->lgnd_pos)) {

		NhlBoolean	user_x_off = True, 
				user_y_off = True;


		if (! _NhlArgIsSet(args,num_args,NhlNlgJustification)) {
			user_just = False;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNovLegendXOffsetF)) {
			user_x_off = False;
		}
		if (! _NhlArgIsSet(args,num_args,NhlNovLegendYOffsetF)) {
			user_y_off = False;
		}
		switch (ovp->lgnd_side) {
		case NhlBOTH:
		case NhlCENTER:
		default:
			e_text =
			     "%s: setting invalid %s enumeration value to %s";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNovLegendSide, "NhlRIGHT");
			ret = MIN(ret,WARNING);
			ovp->lgnd_side = NhlRIGHT;
			/* falling through */
		case NhlRIGHT:
			ovp->lgnd_x_off = user_x_off || ovp->lgnd_x_off > 0.0 ?
				ovp->lgnd_x_off : - ovp->lgnd_x_off;
			break;
		case NhlLEFT:
			ovp->lgnd_x_off = user_x_off || ovp->lgnd_x_off < 0.0 ?
				ovp->lgnd_x_off : - ovp->lgnd_x_off;
			break;
		case NhlTOP:
			ovp->lgnd_y_off = user_y_off || ovp->lgnd_y_off > 0.0 ?
				ovp->lgnd_y_off : - ovp->lgnd_y_off;
			break;
		case NhlBOTTOM:
			ovp->lgnd_y_off = user_y_off || ovp->lgnd_y_off < 0.0 ?
				ovp->lgnd_y_off : - ovp->lgnd_y_off;
			break;
		}
	}

/*
 * Get the bounding box, then set the legend position with respect to it.
 */

	if (ovp->titles != NULL)
		subret = NhlGetBB(ovp->titles->base.id,&bbox);
	else if (ovp->tickmarks != NULL)
		subret = NhlGetBB(ovp->tickmarks->base.id,&bbox);
	else
		subret = NhlGetBB(ovnew->base.id,&bbox);

	if ((ret = MIN(ret,subret)) < WARNING) {
		e_text = "%s: Error getting bounding box";
		NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
		return(FATAL);
	}
/*
 * Set the position, height and width of the Legend based on the
 * side and position resources. Also set the Legend justification, (which
 * determines the fixed point about which the legend repositions itself
 * if it has to changes size due to a label rotation or text size change, etc.
 */
	if (ovp->lgnd_side == NhlRIGHT || ovp->lgnd_side == NhlLEFT) {

		height = ovnew->view.height;
		width = bbox.r - bbox.l;

		ovp->lgnd_x = (ovp->lgnd_side == NhlRIGHT ? bbox.r : 
			  bbox.l - ovp->lgnd_width) + ovp->lgnd_x_off;
		switch(ovp->lgnd_pos) {
		case NhlRIGHT:
		case NhlLEFT:
		case NhlBOTH:
		default:
			e_text =
			     "%s: setting invalid %s enumeration value to %s";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNovLegendPosition, "NhlCENTER");
			ret = MIN(ret,WARNING);
			ovp->lgnd_pos = NhlCENTER;
			/* falling through */

		case NhlCENTER:
			ovp->lgnd_y = ovnew->view.y - height/2.0 + 
				ovp->lgnd_y_off + ovp->lgnd_height/2.0;
			just = ovp->lgnd_side == NhlRIGHT ? 
				NhlCENTERLEFT : NhlCENTERRIGHT;
			break;
		case NhlTOP:
			ovp->lgnd_y = ovnew->view.y + ovp->lgnd_y_off;
			just = ovp->lgnd_side == NhlRIGHT ? 
				NhlTOPLEFT : NhlTOPRIGHT;
			break;
		case NhlBOTTOM:
			ovp->lgnd_y = ovnew->view.y - height + 
				ovp->lgnd_y_off + ovp->lgnd_height;
			just = ovp->lgnd_side == NhlRIGHT ? 
				NhlBOTTOMLEFT : NhlBOTTOMRIGHT;
			break;
		}
	}
	else {

		height = bbox.t - bbox.b;
		width = ovnew->view.width;

		ovp->lgnd_y = (ovp->lgnd_side == NhlTOP ? 
			  bbox.t + ovp->lgnd_height : bbox.b) 
			+ ovp->lgnd_y_off;
		switch(ovp->lgnd_pos) {
		case NhlTOP:
		case NhlBOTTOM:
		case NhlBOTH:
		default:
			e_text =
			     "%s: setting invalid %s enumeration value to %s";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
				  NhlNovLegendPosition, "NhlCENTER");
			ret = MIN(ret,WARNING);
			ovp->lgnd_pos = NhlCENTER;
			/* falling through */

		case NhlCENTER:
			ovp->lgnd_x = ovnew->view.x + width/2.0 + 
				ovp->lgnd_x_off - ovp->lgnd_width/2.0;
			just = ovp->lgnd_side == NhlTOP ? 
				NhlBOTTOMCENTER : NhlTOPCENTER;
			break;
		case NhlRIGHT:
			ovp->lgnd_x = ovnew->view.x + width + 
				ovp->lgnd_x_off - ovp->lgnd_width;
			just = ovp->lgnd_side == NhlTOP ? 
				NhlBOTTOMRIGHT : NhlTOPRIGHT;
			break;
		case NhlLEFT:
			ovp->lgnd_x = ovnew->view.x + ovp->lgnd_x_off;
			just = ovp->lgnd_side == NhlTOP ? 
				NhlBOTTOMLEFT : NhlTOPLEFT;
			break;
		}
	}
	if (user_just)
		just = ovp->lgnd_just;
	else
		ovp->lgnd_just = just;

/*
 * If no title object exists, create it; otherwise just set the relevant
 * resources.
 */
	if (ovp->legend == NULL) {	
		strcpy(buffer,ovnew->base.name);
		strcat(buffer,".Legend");
		subret = _NhlCreateChild(&tmpid,buffer,legendLayerClass,
					 (Layer)ovnew,
					 NhlNvpXF,ovp->lgnd_x,
					 NhlNvpYF,ovp->lgnd_y,
					 NhlNvpWidthF,ovp->lgnd_width,
					 NhlNvpHeightF,ovp->lgnd_height,
					 NhlNlgJustification,ovp->lgnd_just,
					 NhlNlgOrientation,ovp->lgnd_orient,
					 NULL);
		if ((ret = MIN(ret,subret)) < WARNING || 
		    (ovp->legend = _NhlGetLayer(tmpid)) == NULL) {
			e_text = "%s: Error creating Title object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	} else {

		if (ovp->lgnd_x != oovp->lgnd_x)
			NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovp->lgnd_x);
		if (ovp->lgnd_y != oovp->lgnd_y)
			NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovp->lgnd_y);
		if (ovp->lgnd_width != oovp->lgnd_width)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpWidthF,ovp->lgnd_width);
		if (ovp->lgnd_height != oovp->lgnd_height)
			NhlSetSArg(&sargs[nargs++],
				   NhlNvpHeightF,ovp->lgnd_height);
		if (ovp->lgnd_just != oovp->lgnd_just)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlgJustification,ovp->lgnd_just);
		if (ovp->lgnd_orient != oovp->lgnd_orient)
			NhlSetSArg(&sargs[nargs++],
				   NhlNlgOrientation,ovp->lgnd_orient);
		subret = _NhlALSetValuesChild(ovp->legend->base.id,
					      (Layer)ovnew,sargs,nargs);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: Error updating Title object";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(FATAL);
		}
	}



	return ret;
}



/* low level overlay mapping functions */

void nhlcxy_
#if __STDC__
(float *xin,float *yin, float* xout, float* yout)
#else
(xin,yin,xout,yout)
        float *xin;
        float *yin;
        float *xout;
        float *yout;
#endif
{
        int status = 0;

        if (Overlay_Trans_Obj == NULL) {
		_NhlCompcToWin(Trans_Obj,Plot,xin,yin,1,xout,yout,
			       &status,NULL,NULL);
	}
        else {
		_NhlCompcToData(Trans_Obj,Plot,xin,yin,1,xout,yout,
				&status,NULL,NULL);

#if 0
		fprintf (stderr,"inter: %f %f : ",*xout,*yout);
#endif

		_NhlDataToWin(Overlay_Trans_Obj,Overlay_Plot,
			     xout,yout,1,xout,yout,&status,NULL,NULL);
        }

#if 0
	fprintf (stderr,"%f %f : %f %f \n",*xin,*yin,*xout,*yout);
#endif

}


/*
 * The first two global functions allow the user to add, remove, and 
 * control the drawing sequence of plots in an overlay.
 */

/*
 * Function:	NhlAddToOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of overlay base plot
 *		plot_id		id of plot to add to overlay
 *		after_id	id of plot already a member of the overlay
 *				that the new plot should be drawn after --
 *				if < 0 then the new plot is placed at the
 *				end of the overlay sequence.
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlAddToOverlay
#if  __STDC__
(int base_id, int plot_id, int after_id)
#else
(base_id, plot_id, after_id)
        int base_id;
	int plot_id;
	int after_id;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "NhlAddToOverlay";

	Layer			base = _NhlGetLayer(base_id);
	Layer			plot = _NhlGetLayer(plot_id);
	Layer			after = _NhlGetLayer(after_id);

	TransformLayerPart	*base_tfp;
	OverlayLayer		ovl;
	OverlayLayerPart	*ovp;
	TransformLayerPart	*plot_tfp;
	TransformLayerClassRec	*plot_classp;

	NhlGenArray		ga;
	int			plot_count = 0;
	ovRec			**sub_recs = NULL;
	int			i, j;

/*
 * Check validity of the plot layers, then root out the pointer to the overlay
 * layer.
 */
	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid base plot id";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	if (plot == NULL || ! _NhlIsTransform(plot)) {
		e_text = "%s: invalid overlay plot id";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	if (base == plot) {
		e_text = "%s: overlay member and base plot ids are the same";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
		return WARNING;
	}

	plot_tfp = &(((TransformLayer)plot)->trans);
	base_tfp = &(((TransformLayer)base)->trans);
	if (! base_tfp->overlay_plot_base ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: no overlay initialized for base plot";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	ovl = (OverlayLayer) base_tfp->overlay_object;
	ovp = &(ovl->overlay);

/*
 * Test to ensure the added plot can handle becoming an overlay member. 
 */
	plot_classp = (TransformLayerClassRec *) _NhlClass(plot);
	if (plot_classp->trans_class.overlay_capability ==
	    					_tfNotOverlayCapable ||
	    plot_classp->trans_class.overlay_capability ==
	    					_tfOverlayBaseOnly) {
		e_text = "%s: plot class %s cannot be overlay member";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,
			  plot_classp->base_class.class_name);
		return WARNING;
	}

/*
 * If the plot is already an overlay member (but not a base plot) it cannot
 * be part of another overlay (even the same overlay).
 * If the plot is an overlay base already, any overlays it has acquired 
 * become part of the overlay it has been added to. Get the record 
 * containing the list of overlays plots plus their associated overlay objs.
 * Also set the argument that will tell its overlay object it is no longer
 * a master overlay object.
 * If the plot is only not an overlay base or an overlay member, simply
 * allocate and fill in a single overlay record for it.
 */

	if (plot_tfp->overlay_status == _tfCurrentOverlayMember) {
		e_text = "%s: plot ID %d is already an overlay member";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,plot_id);
		return WARNING;
	}
	else if (plot_tfp->overlay_status == _tfCurrentOverlayBase) {

		if (plot_tfp->overlay_object == NULL || 
		    ! _NhlIsTransform(plot_tfp->overlay_object)) {
		      e_text = "%s: plot ID %d has inconsistent overlay info";
		        NhlPError(WARNING,E_UNKNOWN,e_text,entry_name,plot_id);
			return WARNING;
		}

		subret = NhlGetValues(plot_tfp->overlay_object->base.id,
				      NhlNovOverlayRecs, &ga, NULL);

		if ((ret = MIN(ret,subret)) < WARNING) {
			return ret;
		}
		if (ga == NULL || ga->size != sizeof(ovRec *)) {
			e_text = "%s: error retrieving internal gen array";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}

		plot_count = ga->num_elements;
		sub_recs = (ovRec **) ga->data;

		ga->my_data = False;
		NhlFreeGenArray(ga);

	}
	else {
		if ((sub_recs = (ovRec **) 
		     NhlMalloc(sizeof(ovRec *))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
		if ((sub_recs[0] = (ovRec *) 
		     NhlMalloc(sizeof(ovRec))) == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
		plot_count = 1;
		sub_recs[0]->plot = (TransformLayer) _NhlGetLayer(plot_id);
		sub_recs[0]->ov_obj = NULL;
	}
			

/*
 * Reallocate the array of overlay record pointers if necessary
 */
	if (ovp->overlay_alloc < ovp->overlay_count + plot_count) {
		ovp->ov_recs = (ovRec **)
			NhlRealloc(ovp->ov_recs, sizeof(ovRec *) *
				   (ovp->overlay_count + 
				    MAX(NhlOV_ALLOC_UNIT,plot_count)));
		if (ovp->ov_recs == NULL) {
			e_text = "%s: dynamic memory allocation error";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
		ovp->overlay_alloc += MAX(NhlOV_ALLOC_UNIT,plot_count);
	}

/*
 * If no after plot is specified put the new plot at the end of the array.
 * Otherwise, rearrange the array so that the new plot follows the plot
 * specified by the after plot id. An invalid after plot id generates a
 * WARNING; the plot is placed at the end of the array.
 */
	
	if (after == NULL) {

		for (i = 0; i < plot_count; i++)
			ovp->ov_recs[ovp->overlay_count+i] = sub_recs[i]; 
	}
	else if (! _NhlIsTransform(after)) {

		e_text = "%s: invalid after plot id";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
		ret = MIN(ret,WARNING);

		for (i = 0; i < plot_count; i++)
			ovp->ov_recs[ovp->overlay_count+i] = sub_recs[i]; 
	}
	else {
		for (i = 0; i <= ovp->overlay_count; i++) {

			if (i == ovp->overlay_count) { /* after not found */
				e_text = "%s: invalid after plot id";
				NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
				ret = MIN(ret,WARNING);
				for (j = 0; j < plot_count; j++)
					ovp->ov_recs[ovp->overlay_count+j] = 
						sub_recs[j];

	}
			else if (after == (Layer) ovp->ov_recs[i]->plot) {
				for (j = ovp->overlay_count - 1;j > i; j--) {
				      ovp->ov_recs[j+plot_count] =
					      ovp->ov_recs[j];
			        }
				for (j = 0; j < plot_count; j++)
					ovp->ov_recs[j+i+1] = sub_recs[j];
				break;
			}
		}
	}
		
/*
 * Call set values for each plot added to the overlay to inform it of its
 * new status, adjusting its view to be identical to the overlay's view.
 * If it contains an overlay, the overlay must be informed that it is
 * no longer a master overlay.
 */
	for (i = 0; i < plot_count; i++) {
		NhlSArg			sargs[10];
		int			nargs = 0;

		if (i == 0 && 
		    plot_tfp->overlay_status == _tfCurrentOverlayBase) {
			NhlSetSArg(&sargs[nargs++],NhlNovMasterOverlay,False);
		}
		NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus, 
			   _tfCurrentOverlayMember);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayObject,base_tfp->overlay_object);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayTrans, ovp->overlay_trans_obj);
		NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovl->view.x);
		NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovl->view.y);
		NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovl->view.width);
		NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovl->view.height);

		subret = NhlALSetValues(sub_recs[i]->plot->base.id,
					sargs,nargs); 
		if ((ret = MIN(subret,ret)) < WARNING)
			return ret;
	}


/*
 * Bump up the overlay count, free the sub_recs pointer array, and NULL
 * added overlay record pointer array elements not yet in use.
 */
	ovp->overlay_count += plot_count;
	NhlFree(sub_recs);
	for (i = ovp->overlay_count; i < ovp->overlay_alloc; i++) {
		ovp->ov_recs[i] = NULL;
	}

	return ret;
}

/*
 * Function:	NhlRemoveFromOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of overlay base plot
 *		plot_id		id of plot to add to overlay
 *		restore		if True, restores any
 *				member overlays that belonged initially to
 *				the plot being removed, thus removing them
 *				from the overlay based on the base_id. 
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlRemoveFromOverlay
#if  __STDC__
(int base_id, int plot_id, NhlBoolean restore)
#else
(base_id, plot_id,restore)
        int base_id;
	int plot_id;
	NhlBoolean restore;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	Layer			base = _NhlGetLayer(base_id);
	Layer			plot = _NhlGetLayer(plot_id);

	TransformLayerPart	*base_tfp;
	OverlayLayerPart	*ovp;
	int			i, j;


/*
 * Check validity of the plot layers, then root out the pointer to the overlay
 * layer.
 */
	if (base == NULL || ! _NhlIsTransform(base)) {
		e_text = "%s: invalid base plot id";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	if (plot == NULL || ! _NhlIsTransform(plot)) {
		e_text = "%s: invalid overlay plot id";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	base_tfp = &(((TransformLayer)base)->trans);
	if (! base_tfp->overlay_plot_base ||
	    base_tfp->overlay_object == NULL || 
	    ! _NhlIsTransform(base_tfp->overlay_object)) {
		e_text = "%s: no overlay initialized for base plot";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	ovp = (OverlayLayerPart *) 
		&(((OverlayLayer)base_tfp->overlay_object)->overlay);
	if (ovp->ov_recs[0]->plot != (TransformLayer) base) {
		e_text = "%s: base is not currently an overlay base plot";
		NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
		return WARNING;
	}
/*
 * This code simply provides an alternative to the SetValues interface
 * removing the overlay object from a base plot. The restore parameter is
 * ignored in this situation
 */
	if (plot_id == base_id) {
		NhlSArg			sarg;

		NhlSetSArg(&sarg,NhlNtfOverlayPlotBase,False);
		subret = NhlALSetValues(base_id,&sarg,1); 
		return MIN(subret,ret);
	}

/*
 * Note: the value of ovp->overlay_count changes during this loop, but
 * not until the correct plot has been found. A break follows, so the 
 * no test is made after the change.
 */
	for (i = 0; i <= ovp->overlay_count; i++) {
		if (i == ovp->overlay_count) {
			e_text = "%s: plot not found in overlay";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			return WARNING;
		}
		if (plot == (Layer) ovp->ov_recs[i]->plot) {
			
			if (restore && ovp->ov_recs[i]->ov_obj != NULL) {
				subret = RestoreOverlayBase(ovp,i);
				if ((ret = MIN(subret,ret)) < WARNING) {
					return ret;
				}
			}
			else if (ovp->ov_recs[i]->ov_obj != NULL) {

				subret = RemoveOverlayBase(ovp,i);
				if ((ret = MIN(subret,ret)) < WARNING) {
					return ret;
				}
			}
			else {
				NhlSArg			sargs[3];
				int			nargs = 0;

				for (j = i; j < ovp->overlay_count - 1; j++) {
					ovp->ov_recs[j] = ovp->ov_recs[j+1];
				}
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayObject, NULL);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayTrans,NULL);
				NhlSetSArg(&sargs[nargs++],
					   NhlNtfOverlayStatus,
					   _tfNotInOverlay);

				subret = NhlALSetValues(plot->base.id,
							sargs,nargs); 
				if ((ret = MIN(subret,ret)) < WARNING)
					return ret;

				ovp->ov_recs[--ovp->overlay_count] = NULL;
			}
			break;
		}
	}

	return ret;
}

/*
 * Function:	DissolveOverlay
 *
 * Description: Internal function that breaks an overlay into its
 *		component parts. Invokes whenever an overlay is destroyed.
 *		Restores the state of overlay plot member to their
 *		condition before being added to the current overlay. 
 *
 * In Args:	Layer overlay_object - the overlay object being dissolved
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
DissolveOverlay
#if __STDC__
(
	Layer		overlay_object
)
#else
(overlay_object)
	Layer			overlay_object;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
#if 0
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
#endif
	OverlayLayerPart	*ovp = 
				     &((OverlayLayer)overlay_object)->overlay;
	int			i;
	NhlSArg			sargs[3];
	int			nargs = 0;

	while (ovp->overlay_count > 1) {
			
		if (ovp->ov_recs[1]->ov_obj != NULL) {
			subret = RestoreOverlayBase(ovp,1);
			if ((ret = MIN(subret,ret)) < WARNING) {
				return ret;
			}
		}
		else {
			NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject, NULL);
			NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,NULL);
			NhlSetSArg(&sargs[nargs++],
				   NhlNtfOverlayStatus,_tfNotInOverlay);

			subret = NhlALSetValues(ovp->ov_recs[1]->plot->base.id,
						sargs,nargs); 
			if ((ret = MIN(subret,ret)) < WARNING)
				return ret;

			for (i = 1; i < ovp->overlay_count - 1; i++) {
				ovp->ov_recs[i] = ovp->ov_recs[i+1];
			}
			ovp->ov_recs[--ovp->overlay_count] = NULL;
			nargs = 0;
		}
	}

	return MIN(subret,ret);
}

/*
 * Function:	RemoveOverlayBase
 *
 * Description: Internal function that removes an overlay base plot without
 *		removing any member plots that originally belonged to it.
 *
 * In Args:	OverlayLayerPart ovp - overlay object that plot is being
 * 				       removed from
 *		int plot_number -- the array position in the overlay record of
 *			       	   the plot being removed
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
RemoveOverlayBase
#if __STDC__
(
	OverlayLayerPart	*ovp,
	int			plot_number
)
#else
(ovp,plot_number)
	OverlayLayerPart	*ovp;
	int			plot_number;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	OverlayLayer		plot_ovl = (OverlayLayer) 
					ovp->ov_recs[plot_number]->ov_obj;
	TransformLayer		plot = ovp->ov_recs[plot_number]->plot;
	NhlGenArray		ga;
	ovRec			**ov_recs = NULL;
	NhlSArg			sargs[10];
        int			nargs = 0;
	int			i;
	int			count = 1;


	for (i = plot_number; i < ovp->overlay_count - 1; i++) {
		ovp->ov_recs[i] = ovp->ov_recs[i+1];
	}
	ovp->ov_recs[--ovp->overlay_count] = NULL;
/*
 * Create a GenArray of 1 element in order to set the OverlayRecs resource
 * of the overlay plot. This will erase its memory of its overlay plots.
 */

	ov_recs = (ovRec **) NhlMalloc(sizeof(ovRec *));
	if (ov_recs == NULL) {
		e_text = "%s: dynamic memory allocation error";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	ov_recs[0] = (ovRec *) NhlMalloc(ovp->overlay_count * sizeof(ovRec));
	if (ov_recs[0] == NULL) {
		e_text ="%s: dynamic memory allocation error";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	ov_recs[0]->plot = plot;
	ov_recs[0]->ov_obj = (Layer) plot_ovl;
			
	ga = NhlCreateGenArray((NhlPointer)ov_recs,NhlTPointer,
			       sizeof(ovRec *),1,&count);
	if (ga == NULL) {
		e_text = "%s: error creating %s GenArray";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,NhlNovOverlayRecs);
		return FATAL;
	}
	ga->my_data = True;

	NhlSetSArg(&sargs[nargs++],NhlNovOverlayRecs, ga);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus,_tfCurrentOverlayBase);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject,plot_ovl);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,plot->trans.trans_obj);
	NhlSetSArg(&sargs[nargs++],NhlNovMasterOverlay,True);
	
	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
	ret = MIN(subret,ret);
		  
	NhlFree(ov_recs[0]);
	NhlFreeGenArray(ga);

	return ret;

}

/*
 * Function:	RestoreOverlayBase
 *
 * Description: Internal function that restores the overlay state of a plot
 *		that was originally an overlay base when it is removed from 
 *		another overlay.
 *
 * In Args:	OverlayLayerPart ovp - overlay object that plot is being
 * 				       removed from
 *		int plot_number -- the array position in the overlay record of
 *			       	   the plot being removed
 *
 * Out Args:
 *
 * Return Values:	ErrorConditions
 *
 * Side Effects:	NONE
 */
static NhlErrorTypes
RestoreOverlayBase
#if __STDC__
(
	OverlayLayerPart	*ovp,
	int			plot_number
)
#else
(ovp,plot_number)
	OverlayLayerPart	*ovp;
	int			plot_number;
#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	OverlayLayer		plot_ovl = (OverlayLayer) 
					ovp->ov_recs[plot_number]->ov_obj;
	TransformLayer		plot = ovp->ov_recs[plot_number]->plot;
	NhlGenArray		ga;
	ovRec			**sub_recs = NULL;
	NhlSArg			sargs[10];
        int			nargs = 0;
	int			i, j, k;
	int			new_plot_count;

/*
 * Assuming the overlay pointer for the plot is valid, fetch its overlay
 * record. Then loop through the overlay record and try to match each
 * record with a record in the current overlay object. For any record that
 * matches, remove it from the current overlay object. Any records that are
 * not found must have been removed from the current overlay object; they
 * must therefore be removed from the plot's overlay object as well.
 */
	if (! _NhlIsTransform((Layer) plot_ovl)) {
		e_text = "%s: internal inconsistency in record of plot ID %d";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name,plot->base.id);
		return FATAL;
	}

	subret = NhlGetValues(plot_ovl->base.id,
			      NhlNovOverlayRecs, &ga, NULL);

	if ((ret = MIN(ret,subret)) < WARNING) {
		return ret;
	}
	if (ga == NULL || ga->size != sizeof(ovRec *)) {
		e_text = "%s: error retrieving internal gen array";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}

	new_plot_count = ga->num_elements;
	sub_recs = (ovRec **) ga->data;

	for (i = 0; i < new_plot_count; ) {
		for (j = 0; j <= ovp->overlay_count; j++) {
			if (j == ovp->overlay_count) {
				NhlFree(sub_recs[i]);
				for (k = i; k < new_plot_count - 1; k++) {
					sub_recs[k] = sub_recs[k+1];
				}
				sub_recs[--new_plot_count] = NULL;
			}
			else if (ovp->ov_recs[j]->plot == sub_recs[i]->plot) {
				NhlFree(ovp->ov_recs[j]);
				for (k = j; k < ovp->overlay_count - 1; k++) {
					ovp->ov_recs[k] = ovp->ov_recs[k+1];
				}
				ovp->ov_recs[--ovp->overlay_count] = NULL;
				i++;
				break;
			}
		}
	}

/*	
 * Update the overlay transform resources to indicate the new status
 * of each plot in the overlay that is being restored.
 */
	for (i = 1; i < new_plot_count; i++) {
		nargs = 0;
		NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject, plot_ovl);
		NhlSetSArg(&sargs[nargs++],
			   NhlNtfOverlayTrans,plot->trans.trans_obj);

		subret = NhlALSetValues(sub_recs[i]->plot->base.id,
					sargs,nargs); 
		if ((ret = MIN(subret,ret)) < WARNING)
			return ret;

	}
			

/*
 * If the new plot count is different from the old, (indicating that a plot
 * that was a member of the plot overlay was removed while it was a member
 * of the current overlay) use the same GenArray to send the new contents 
 * of the overlay record to the plot (and therefore to its overlay object).
 */
	if (new_plot_count != ga->num_elements) {
		ga->num_elements = ga->len_dimensions[0] = new_plot_count;
		NhlSetSArg(&sargs[nargs++],NhlNovOverlayRecs, ga);
	}


/*
 * Set the trans and overlay resources for the newly restored 
 * overlay base plot.
 */

	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayStatus,_tfCurrentOverlayBase);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayObject,plot_ovl);
	NhlSetSArg(&sargs[nargs++],NhlNtfOverlayTrans,plot->trans.trans_obj);
	NhlSetSArg(&sargs[nargs++],NhlNovMasterOverlay,True);
	
	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
	ret = MIN(subret,ret);
		  
/*
 * Free the GenArray -- since the my_data flag is set True,
 * the pointer array will be freed by the GenArray free call, but it is still
 * necessary to free each of the overlay records.
 */

	for (i = 0; i < new_plot_count; i++) {
		NhlFree(sub_recs[i]);
	}
	NhlFreeGenArray(ga); /* frees sub_recs since my_data is True */

	return ret;

}


/*
 * Function:	_NhlManageOverlay
 *
 * Description: Generic function that manages an overlay for a plot.
 *		Calls Initialize or SetValues for an overlay depending
 *		on the conditions. Additional resources may be set.
 *
 * In Args:	
 *		xnew		new instance record
 *		xold		old instance record if not initializing
 *		init		true if initialization
 *		sargs   	set args list
 *              nargs   	number of arguments
 *
 * Out Args:	overlay_object (in/out) pointer to private storage for the
 *				        overlay ptr
 *
 * Return Values:	Error Conditions
 *
 * Side Effects:	Objects created and destroyed.
 */
extern NhlErrorTypes _NhlManageOverlay
#if  __STDC__
(
	Layer		*overlay_object,
	Layer		lnew,
	Layer		lold,
	NhlBoolean	init,
	NhlSArgList	sargs,
	int		nargs,
	char		*entry_name
)
#else 
(overlay_object,lnew,lold,init,sargs,nargs,entry_name)
	Layer		*overlay_object;				   
	Layer		lnew;
	Layer		lold;
	NhlBoolean	init;
	NhlSArgList	sargs;
	int		nargs;
	char		*entry_name;

#endif
{
	NhlErrorTypes		ret = NOERROR, subret = NOERROR;
	char 			*e_text;
	TransformLayerPart	*tfp = &(((TransformLayer)lnew)->trans);
	TransformLayerPart	*otfp = &(((TransformLayer)lold)->trans);
	ViewLayerPart		*vwp = &(((ViewLayer)lnew)->view);
	int			tmpid = -1;
	char			buffer[MAXFNAMELEN];
	NhlBoolean		created = False;
	NhlSArg			*lsargs;

	if (! init && 
	    (*overlay_object != NULL && ! _NhlIsTransform(*overlay_object))) {
		e_text = "%s: invalid overlay object passed in";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
		
	lsargs = (NhlSArg *) NhlMalloc((nargs+4) * sizeof(NhlSArg));
	if (lsargs == NULL) {
		e_text ="%s: dynamic memory allocation error";
		NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
		return FATAL;
	}
	if (nargs > 0) {
		memcpy(lsargs,sargs,nargs*sizeof(NhlSArg));
	}

	if (init ||
	    tfp->overlay_plot_base != otfp->overlay_plot_base) {

		if (tfp->overlay_status == _tfCurrentOverlayMember) {
			e_text = 
	       "%s: cannot change overlay base status while an overlay member";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			tfp->overlay_plot_base = (*overlay_object == NULL) ?
				False : True;
		}
		else if (tfp->overlay_plot_base == True) {

			strcpy(buffer,lnew->base.name);
			strcat(buffer,".Overlay");
			NhlSetSArg(&lsargs[nargs++],NhlNvpXF,vwp->x);
			NhlSetSArg(&lsargs[nargs++],NhlNvpYF,vwp->y);
			NhlSetSArg(&lsargs[nargs++],NhlNvpWidthF,vwp->width);
			NhlSetSArg(&lsargs[nargs++],NhlNvpHeightF,vwp->height);
		
			subret = _NhlALCreateChild(&tmpid,buffer,
						   overlayLayerClass,lnew,
						   lsargs,nargs);

			ret = MIN(ret,subret);

			if (ret < WARNING || (*overlay_object = 
					      _NhlGetLayer(tmpid)) == NULL) {
				e_text = "%s: overlay creation failure";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
				return(FATAL);
			}
			tfp->overlay_object = *overlay_object;
			tfp->overlay_status = _tfCurrentOverlayBase;
			tfp->overlay_trans_obj = tfp->trans_obj;
			created = True;
		}
		else if (! init && *overlay_object != NULL) {

			subret = _NhlDestroyChild(
					      (*overlay_object)->base.id,lnew);
			if ((ret = MIN(ret,subret)) < WARNING) {
				return ret;
			}
			tfp->overlay_object = NULL;
			tfp->overlay_status = _tfNotInOverlay;
			tfp->overlay_trans_obj = NULL;
			*overlay_object = NULL;
		}
	}
	
/*
 * If this plot is an overlay member (not the base plot), its view should
 * be the same as that of the overlay. If it is not, the user must have
 * set it: in this case issue a warning and set it to the view of the
 * overlay.
 */
	
	if (! created &&
	    tfp->overlay_status == _tfCurrentOverlayMember) {

		ViewLayer ovvl = (ViewLayer) tfp->overlay_object;

		if (tfp->overlay_object == NULL || 
		    ! _NhlIsTransform(tfp->overlay_object)) {
			e_text = "%s: inconsistent overlay state";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return(FATAL);
		}

		if (vwp->x != ovvl->view.x ||
		    vwp->y != ovvl->view.y ||
		    vwp->width != ovvl->view.width ||
		    vwp->height != ovvl->view.height) {

			vwp->x = ovvl->view.x;
			vwp->y = ovvl->view.y;
			vwp->width = ovvl->view.width;
			vwp->height = ovvl->view.height;

			e_text =
			"%s: attempt to set overlay member plot view ignored";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret,WARNING);
		}
	}
		
	if (! created && *overlay_object != NULL) {
		ViewLayerPart		*ovwp = &(((ViewLayer)lold)->view);

		if (vwp->x != ovwp->x)
			NhlSetSArg(&lsargs[nargs++],NhlNvpXF,vwp->x);
		if (vwp->y != ovwp->y)
			NhlSetSArg(&lsargs[nargs++],NhlNvpYF,vwp->y);
		if (vwp->width != ovwp->width)
			NhlSetSArg(&lsargs[nargs++],NhlNvpWidthF,vwp->width);
		if (vwp->height != ovwp->height)
			NhlSetSArg(&lsargs[nargs++],NhlNvpHeightF,vwp->height);
		
		subret = _NhlALSetValuesChild((*overlay_object)->base.id,
					      lnew,lsargs,nargs);

		if ((ret = MIN(subret, ret)) < WARNING) {
			e_text = "%s: error setting overlay object view";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
	}

	return ret;
}

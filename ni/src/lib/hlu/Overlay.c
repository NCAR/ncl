/*
 *      $Id: Overlay.c,v 1.2 1993-12-22 00:56:15 dbrown Exp $
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
	NhlSArg			sargs[10];
        int			nargs = 0;
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
 * Test to ensure the added plot can handle being overlaid. 
 */
	plot_classp = (TransformLayerClassRec *) _NhlClass(plot);
	if (! plot_classp->trans_class.handles_overlays) {
		e_text = "%s: plot class %s cannot be overlaid";
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

		NhlSetSArg(&sargs[nargs++],NhlNovMasterOverlay,False);
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
 * Bump up the overlay count and free the sub_recs pointer array.
 */
	ovp->overlay_count += plot_count;
	NhlFree(sub_recs);
		
/*
 * Call set values on the plot added to the overlay to inform it of its
 * new status, adjusting its view to be identical to the overlay's view.
 * If it contains an overlay, the overlay must be informed that it is
 * no longer a master overlay.
 */
	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayObject,base_tfp->overlay_object);
	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayTrans, ovp->overlay_trans_obj);
	NhlSetSArg(&sargs[nargs++],
		   NhlNtfOverlayStatus, _tfCurrentOverlayMember);
	NhlSetSArg(&sargs[nargs++],NhlNvpXF,ovl->view.x);
	NhlSetSArg(&sargs[nargs++],NhlNvpYF,ovl->view.y);
	NhlSetSArg(&sargs[nargs++],NhlNvpWidthF,ovl->view.width);
	NhlSetSArg(&sargs[nargs++],NhlNvpHeightF,ovl->view.height);

	subret = NhlALSetValues(plot->base.id,sargs,nargs); 
		  
	return ret;
}

/*
 * Function:	NhlRemoveFromOverlay
 *
 * Description:	
 *
 * In Args:	base_id		id of overlay base plot
 *		plot_id		id of plot to add to overlay
 *
 * Out Args:	NONE
 *
 * Return Values: Error Conditions
 *
 * Side Effects: NONE
 */	

NhlErrorTypes NhlRemoveFromOverlay
#if  __STDC__
(int base_id, int plot_id)
#else
(base_id, plot_id)
        int base_id;
	int plot_id;
#endif
{
	NhlErrorTypes		ret = NOERROR;
	char			*e_text;
	char			*entry_name = "NhlRemoveFromOverlay";
	Layer			base = _NhlGetLayer(base_id);
	Layer			plot = _NhlGetLayer(plot_id);

	TransformLayerPart	*base_tfp;
	OverlayLayerPart	*ovp;
	TransformLayerPart	*plot_tfp;
	int i, j;

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
	if (plot == base) {
		e_text = "%s: cannot remove base plot from overlay";
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
	ovp = (OverlayLayerPart *) 
		&(((OverlayLayer)base_tfp->overlay_object)->overlay);

	for (i = 1; i <= ovp->overlay_count; i++) {
		if (i == ovp->overlay_count) {
			e_text = "%s: plot not found in overlay";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			return WARNING;
		}
		if (plot == (Layer) ovp->ov_recs[i]->plot) {
#if 0
			if (ovp->ov_recs[i].ov_obj != NULL) {
				RestoreOverlayBase(plot,ovp,i);
			}
#endif
			for (j = i; j < ovp->overlay_count - 1; j++) {
				ovp->ov_recs[j] = ovp->ov_recs[j+1];
			}
			ovp->overlay_count--;
			NhlSetValues(plot->base.id, 
				     NhlNtfOverlayObject, NULL,
				     NhlNtfOverlayTrans, NULL,
				     NULL);
			break;
		}
	}

	return ret;
}

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
	NhlErrorTypes ret = NOERROR;
	NhlErrorTypes lret = NOERROR;

	return MIN(lret,ret);
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
				NhlMalloc(ovp->overlay_count * 
					  sizeof(ovRec *));
			if (ov_recs == NULL) {
				e_text = "%s: dynamic memory allocation error";
				NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
				return FATAL;
			}

			for (j = 0; j < ovp->overlay_count; j++) {

				ov_recs[j] = (ovRec *)
					NhlMalloc(ovp->overlay_count * 
						  sizeof(ovRec));
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
					  NhlNovOverlayIds);
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
	OverlayLayerPart	*ovp = &((OverlayLayer) inst)->overlay;
	NhlErrorTypes		ret = NOERROR;
	int			i;

/*
 * Free the overlay record pointer array
 */
	for (i=0; i < ovp->overlay_count; i++)
		NhlFree(ovp->ov_recs[i]);

	NhlFree(ovp->ov_recs);


	return(ret);
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
	NhlBoolean		tm_sides[4]; /* left,right,bottom,top */
	int			tm_ids[4] = {-1,-1,-1,-1};
	int			i, j, count;
	BaseLayerPart		*basep;

/*
 * Set the overlay trans. Then call the draw 
 * function for each of the overlay plots in turn.
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
 * Call the pre-draw, draw, and post-draw methods for each plot object
 * in turn. Note that the draw methods for the base plot get invoked twice.
 * Therefore the base plot must contain logic to avoid repeating its draw
 * code, and most important, setting up an infinite recursive loop.
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		if (!_NhlIsTransObj(ovp->ov_recs[i]->plot->trans.trans_obj) || 
		    ! _NhlIsTransform(ovp->ov_recs[i]->plot)) {
			e_text = "%s: invalid trans object or plot class";
			NhlPError(FATAL,E_UNKNOWN,e_text,entry_name);
			return FATAL;
		}
		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (Layer) ovp->ov_recs[i]->plot;

		subret = _NhlPreDraw(Plot);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: error in plot pre-draw";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

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

	for (i = 0; i < ovp->overlay_count; i++) {
		Trans_Obj = ovp->ov_recs[i]->plot->trans.trans_obj;
		Plot = (Layer) ovp->ov_recs[i]->plot;

		subret = _NhlPostDraw(Plot);

		if ((ret = MIN(ret,subret)) < WARNING) {
			e_text = "%s: error in plot post-draw";
			NhlPError(FATAL,E_UNKNOWN,e_text, entry_name);
			return(ret);
		}
	}

/* this code is going away */

#if 0

/*
 * For each side of the plot, find the first valid tick mark object that
 * has a tick mark object with a scale along that side and draw it.
 */
	count = 0;
	for (i = 0; i < ovp->overlay_count; i++) {
		if (ovp->overlays[i]->tick_mark == NULL) {
			continue;
		}
		basep = &(ovp->overlays[i]->tick_mark->base);
		if (strcmp(basep->layer_class->base_class.class_name, 
			   "TickMark")) {
			e_text = "%s: skipping invalid TickMark object";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret, WARNING);
		}
		else {
			NhlGetValues(basep->id,
				     NhlNtmYLOn, &tm_sides[0],
				     NhlNtmYROn, &tm_sides[1],
				     NhlNtmXBOn, &tm_sides[2],
				     NhlNtmXTOn, &tm_sides[3],
				     NULL);
			for (j = 0; j < 4; j++) {
				if (tm_sides[j] && (tm_ids[j] < 0)) {
					tm_ids[j] = basep->id;
					count++;
				}
			}
			if (count == 4) break;
		}
	}
	for (i = 0; i < 4; i++) {
		if (tm_ids[i] >= 0) {
			NhlDraw(tm_ids[i]);
			for (j = i; j < 4; j++) {
				if (tm_ids[j] == tm_ids[i]) tm_ids[j] = -1;
			}
		}
	}
				

/*
 * Find the first valid title and draw it
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		if (ovp->overlays[i]->title == NULL) {
			continue;
		}
		basep = &(ovp->overlays[i]->title->base);
		if (strcmp(basep->layer_class->base_class.class_name, 
			   "Title")) {
			e_text = "%s: skipping invalid Title object";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret, WARNING);
		}
		else {
			NhlDraw(basep->id);
			break;
		}
	}

/*
 * Draw all legends, labelbar, and custom annotation objects
 */

	for (i = 0; i < ovp->overlay_count; i++) {
		if (ovp->overlays[i]->legend == NULL) {
			continue;
		}
		basep = &(ovp->overlays[i]->legend->base);
		if (strcmp(basep->layer_class->base_class.class_name, 
			   "Legend")) {
			e_text = "%s: skipping invalid Legend object";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret, WARNING);
		}
		else {
			NhlDraw(basep->id);
		}
	}

	for (i = 0; i < ovp->overlay_count; i++) {
		if (ovp->overlays[i]->labelbar == NULL) {
			continue;
		}
		basep = &(ovp->overlays[i]->labelbar->base);
		if (strcmp(basep->layer_class->base_class.class_name, 
			   "LabelBar")) {
			e_text = "%s: skipping invalid LabelBar object";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret, WARNING);
		}
		else {
			NhlDraw(basep->id);
		}
	}

	for (i = 0; i < ovp->overlay_count; i++) {
		if (ovp->overlays[i]->custom1 == NULL) {
			continue;
		}
		basep = &(ovp->overlays[i]->custom1->base);
		if (! _NhlIsView(ovp->overlays[i]->custom1)) {
			e_text = "%s: skipping invalid custom object";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret, WARNING);
		}
		else {
			NhlDraw(basep->id);
		}
	}

	for (i = 0; i < ovp->overlay_count; i++) {
		if (ovp->overlays[i]->custom2 == NULL) {
			continue;
		}
		basep = &(ovp->overlays[i]->custom2->base);
		if (! _NhlIsView(ovp->overlays[i]->custom2)) {
			e_text = "%s: skipping invalid custom object";
			NhlPError(WARNING,E_UNKNOWN,e_text,entry_name);
			ret = MIN(ret, WARNING);
		}
		else {
			NhlDraw(basep->id);
		}
	}

#endif
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

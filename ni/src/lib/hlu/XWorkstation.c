/*
 *      $Id: XWorkstation.c,v 1.2 1993-10-19 17:53:22 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		XWorkstation.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 15 10:00:09 MDT 1992
 *
 *	Description:	Responsible for managing the X workstation element
 */

#include <stdio.h>
#include <strings.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/XWorkstationP.h>


static NhlResource resources[] = {
	{ NhlNwkWindowId, NhlCwkWindowId, NhlTInteger, sizeof(int),
	NhlOffset(XWorkstationLayerRec,xwork.window_id), NhlTImmediate,
								(NhlPointer)-1},
	{ NhlNwkColorMapId, NhlCwkColorMapId, NhlTInteger, sizeof(int),
	NhlOffset(XWorkstationLayerRec,xwork.color_map_id), NhlTImmediate,
								(NhlPointer)-1},
	{ NhlNwkPause, NhlCwkPause, NhlTBoolean, sizeof(NhlBoolean),
	NhlOffset(XWorkstationLayerRec,xwork.pause), NhlTImmediate,
							(NhlPointer)True}
};

/*
* XWorkstation base_class method declarations
*/

static NhlErrorTypes XWorkstationInitialize(
#ifdef NhlNeedProto
        LayerClass,     /* class */
        Layer,          /* req */
        Layer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes XWorkstationSetValues(
#ifdef NhlNeedProto
        Layer,		/* old */
        Layer,		/* reference */
        Layer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

/*
 * XWorkstation xwork_class method declarations
 */
static NhlErrorTypes XWorkstationClear(
#if	NhlNeedProto
	Layer	l	/* workstation layer to clear	*/
#endif
);


XWorkstationLayerClassRec xWorkstationLayerClassRec = {
        {
/* class_name			*/	"XWorkstation",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(XWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(LayerClass)&workstationLayerClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	NULL,
/* layer_initialize		*/	XWorkstationInitialize,
/* layer_set_values		*/	XWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	NULL,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
        },
        {
/* open_work		*/	NULL,
/* close_work		*/	NULL,
/* activate_work	*/	NULL,
/* deactivate_work	*/	NULL,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	XWorkstationClear,
/* lineto_work 		*/	NULL,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker
	},
	{
/* foo */	NULL
	}
};

LayerClass xWorkstationLayerClass = (LayerClass)&xWorkstationLayerClassRec;



/*
 * Function:	XWorkstationInitialize
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
static NhlErrorTypes XWorkstationInitialize
#if __STDC__
(LayerClass class, Layer req, Layer new, _NhlArgList args, int num_args)
#else
(class,req,new,args,num_args)
        LayerClass class;
        Layer req;
        Layer new;
        _NhlArgList args;
        int num_args; 
#endif
{
	XWorkstationLayer	wnew = (XWorkstationLayer) new;

	if(wnew->xwork.window_id == -1) {
/*
* Not sure if this is ignored or not
*/
		wnew->work.gkswksconid = 2;
		wnew->work.gkswkstype = 8;
		
	} else {
		wnew->work.gkswksconid = wnew->xwork.window_id;
		wnew->work.gkswkstype = 7;
		/*
		 * Force pause to False if the user provides a window id
		 * GKS can't grab event's and still allow the user to grab
		 * events.
		 */
		wnew->xwork.pause = False;
	}

	return NOERROR;
}


/*
 * Function:	XWorkstationSetValues
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
static NhlErrorTypes XWorkstationSetValues
#if __STDC__
(Layer old, Layer reference, Layer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        Layer old;
        Layer reference;
        Layer new; 
        _NhlArgList args;
        int num_args;
#endif
{
	XWorkstationLayer	wold = (XWorkstationLayer) old;
	XWorkstationLayer	wnew = (XWorkstationLayer) new;
	NhlErrorTypes		ret = NOERROR, lret = NOERROR;

	if(wnew->xwork.window_id != wold->xwork.window_id){
		NhlPError(WARNING,E_UNKNOWN,"XWorkstation: Window Id cannot be changed after workstation is created");
		wnew->xwork.window_id = wold->xwork.window_id;
		lret = WARNING;
	}

	if(wnew->xwork.pause){
		if(wnew->xwork.window_id != -1){
			NhlPError(WARNING,E_UNKNOWN,"XWorkstation: NhlNwkPause must be false if NhlNwkWindowId is provided");
			wnew->xwork.pause = False;
			ret = WARNING;
		}
	}

	return MIN(ret,lret);
}

/*
 * Function:	XWorkstationClear
 *
 * Description:	This function is used to clear the X Workstation, it uses
 *		it's superclasses function to do this.  The only reason
 *		this function was needed was to impliment the pause
 *		for default X driver use.
 *
 * In Args:	
 *		Layer	l	workstation layer to clear
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
XWorkstationClear
#if	__STDC__
(
	Layer	l	/* workstation layer to clear	*/
)
#else
(l)
	Layer	l;	/* workstation layer to clear	*/
#endif
{
	WorkstationLayerClass	lc = (WorkstationLayerClass)
							workstationLayerClass;
	XWorkstationLayer	xl = (XWorkstationLayer)l;
	Gescape_in_data		indat;
	Gescape_out_data	*outdat;
	char			wkid[15];

	sprintf(wkid,"%d",_NhlWorkstationId(l));

	indat.escape_r1.size = strlen(wkid);
	indat.escape_r1.data = wkid;

	if(xl->xwork.pause)
		gescape(-1396,&indat,NULL,&outdat);

	return (*(lc->work_class.clear_work))(l);
}

/*
 *      $Id: XWorkstation.c,v 1.3 1994-01-27 21:27:33 boote Exp $
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
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/XWorkstationP.h>

/*
 * Function:	IsSetFuncs
 *
 * Description:	These functions are used to determine if the user set the
 *		resources described.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
WindowSet
#if	__STDC__
(
	NrmName		name,
	NrmClass	class,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,class,base,offset)
	NrmName		name;
	NrmClass	class;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	NhlXWorkstationLayer	xwork = (NhlXWorkstationLayer)base;

	xwork->xwork.window_id_set = False;
	xwork->xwork.window_id = 0;

	return	NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes
CMapSet
#if	__STDC__
(
	NrmName		name,
	NrmClass	class,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,class,base,offset)
	NrmName		name;
	NrmClass	class;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	NhlXWorkstationLayer	xwork = (NhlXWorkstationLayer)base;

	xwork->xwork.color_map_id_set = False;
	xwork->xwork.color_map_id = 0;

	return	NhlNOERROR;
}

/*ARGSUSED*/
static NhlErrorTypes
PauseSet
#if	__STDC__
(
	NrmName		name,
	NrmClass	class,
	NhlPointer	base,
	unsigned int	offset
)
#else
(name,class,base,offset)
	NrmName		name;
	NrmClass	class;
	NhlPointer	base;
	unsigned int	offset;
#endif
{
	NhlXWorkstationLayer	xwork = (NhlXWorkstationLayer)base;

	xwork->xwork.pause_set = False;
	xwork->xwork.pause = True;

	return	NhlNOERROR;
}

#define	Oset(field)	NhlOffset(NhlXWorkstationLayerRec,xwork.field)
static NhlResource resources[] = {
	{ "no.res", "no.res", NhlTBoolean, sizeof(NhlBoolean),
		Oset(window_id_set), NhlTImmediate, (NhlPointer)True},
	{ "no.res", "no.res", NhlTBoolean, sizeof(NhlBoolean),
		Oset(color_map_id_set), NhlTImmediate, (NhlPointer)True},
	{ "no.res", "no.res", NhlTBoolean, sizeof(NhlBoolean),
		Oset(pause_set), NhlTImmediate, (NhlPointer)True},
	{ NhlNwkWindowId, NhlCwkWindowId, NhlTInteger, sizeof(int),
		Oset(window_id), NhlTProcedure, (NhlPointer)WindowSet},
	{ NhlNwkColorMapId, NhlCwkColorMapId, NhlTInteger, sizeof(int),
		Oset(color_map_id), NhlTProcedure, (NhlPointer)CMapSet},
	{ NhlNwkPause, NhlCwkPause, NhlTBoolean, sizeof(NhlBoolean),
		Oset(pause), NhlTProcedure, (NhlPointer)PauseSet}
};
#undef	Oset

/*
* XWorkstation base_class method declarations
*/

static NhlErrorTypes XWorkstationInitialize(
#ifdef NhlNeedProto
        NhlLayerClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes XWorkstationSetValues(
#ifdef NhlNeedProto
        NhlLayer,	/* old */
        NhlLayer,	/* reference */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

/*
 * XWorkstation xwork_class method declarations
 */
static NhlErrorTypes XWorkstationClear(
#if	NhlNeedProto
	NhlLayer	l	/* workstation layer to clear	*/
#endif
);


NhlXWorkstationLayerClassRec NhlxWorkstationLayerClassRec = {
        {
/* class_name			*/	"XWorkstation",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlXWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)
						&NhlworkstationLayerClassRec,

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

NhlLayerClass NhlxWorkstationLayerClass = (NhlLayerClass)
						&NhlxWorkstationLayerClassRec;



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
(
	NhlLayerClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlLayerClass	class;
        NhlLayer	req;
        NhlLayer	new;
        _NhlArgList	args;
        int		num_args; 
#endif
{
	char			*error_lead="XWorkstationInitialize";
	NhlXWorkstationLayer	wnew = (NhlXWorkstationLayer) new;
	NhlErrorTypes		ret = NhlNOERROR;

	if(wnew->xwork.window_id_set == False) {
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
		if((wnew->xwork.pause_set) && (wnew->xwork.pause)){
			NhlPError(NhlINFO,NhlEUNKNOWN,
	"%s:If the %s resource is specified, the %s resource must be False",
					error_lead,NhlNwkWindowId,NhlNwkPause);
			ret = NhlINFO;
		}
		wnew->xwork.pause = False;
	}

	return ret;
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
(NhlLayer old, NhlLayer reference, NhlLayer new, _NhlArgList args, int num_args)
#else
(old,reference,new,args,num_args)
        NhlLayer old;
        NhlLayer reference;
        NhlLayer new; 
        _NhlArgList args;
        int num_args;
#endif
{
	NhlXWorkstationLayer	wold = (NhlXWorkstationLayer) old;
	NhlXWorkstationLayer	wnew = (NhlXWorkstationLayer) new;
	NhlErrorTypes		ret = NhlNOERROR, lret = NhlNOERROR;

	if(wnew->xwork.window_id != wold->xwork.window_id){
		NhlPError(NhlWARNING,NhlEUNKNOWN,"XWorkstation: Window Id cannot be changed after workstation is created");
		wnew->xwork.window_id = wold->xwork.window_id;
		lret = NhlWARNING;
	}

	if(wnew->xwork.pause){
		if(wnew->xwork.window_id_set == True){
			NhlPError(NhlWARNING,NhlEUNKNOWN,"XWorkstation: NhlNwkPause must be false if NhlNwkWindowId is provided");
			wnew->xwork.pause = False;
			ret = NhlWARNING;
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
 *		NhlLayer	l	workstation layer to clear
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
	NhlLayer	l	/* workstation layer to clear	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to clear	*/
#endif
{
	NhlWorkstationLayerClass	lc = (NhlWorkstationLayerClass)
						NhlworkstationLayerClass;
	NhlXWorkstationLayer		xl = (NhlXWorkstationLayer)l;
	Gescape_in_data			indat;
	Gescape_out_data		*outdat;
	char				wkid[15];

	sprintf(wkid,"%d",_NhlWorkstationId(l));

	indat.escape_r1.size = strlen(wkid);
	indat.escape_r1.data = wkid;

	if(xl->xwork.pause)
		gescape(-1396,&indat,NULL,&outdat);

	return (*(lc->work_class.clear_work))(l);
}

/*
 *      $Id: XWorkstation.c,v 1.14 1995-12-19 20:39:40 boote Exp $
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
#include <ncarg/hlu/ConvertersP.h>

#define	Oset(field)	NhlOffset(NhlXWorkstationLayerRec,xwork.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(window_id_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{NhlNwkWindowId,NhlCwkWindowId,NhlTInteger,sizeof(int),Oset(window_id),
		NhlTProcedure,(NhlPointer)_NhlResUnset,_NhlRES_NOSACCESS,NULL},
	{NhlNwkXColorMode,NhlCwkXColorMode,NhlTXColorMode,sizeof(NhlXColorMode),
		Oset(xcolor_mode),NhlTImmediate,(NhlPointer)NhlSHARE,
		_NhlRES_NOSACCESS,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(pause_set),
		NhlTImmediate,(NhlPointer)True,_NhlRES_NOACCESS,NULL},
	{NhlNwkPause,NhlCwkPause,NhlTBoolean,sizeof(NhlBoolean),
		Oset(pause),NhlTProcedure,(NhlPointer)_NhlResUnset,0,NULL}

/* End-documented-resources */

};
#undef	Oset

/*
* XWorkstation base_class method declarations
*/

static NhlErrorTypes XWorkstationClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes XWorkstationInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes XWorkstationSetValues(
#if	NhlNeedProto
        NhlLayer,	/* old */
        NhlLayer,	/* reference */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes XWorkstationOpen(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

static NhlErrorTypes XWorkstationClear(
#if	NhlNeedProto
	NhlLayer	l	/* workstation layer to clear	*/
#endif
);


NhlXWorkstationClassRec NhlxWorkstationClassRec = {
        {
/* class_name			*/	"xWorkstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlXWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)
						&NhlworkstationClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	XWorkstationClassInitialize,
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
/* def_background	*/	{0.0,0.0,0.0},
/* open_work		*/	XWorkstationOpen,
/* close_work		*/	NhlInheritClose,
/* activate_work	*/	NhlInheritActivate,
/* deactivate_work	*/	NhlInheritDeactivate,
/* alloc_colors		*/	NhlInheritAllocateColors,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	XWorkstationClear,
/* lineto_work 		*/	NhlInheritLineTo,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker
	},
	{
/* foo */	NULL
	}
};

NhlClass NhlxWorkstationClass = (NhlClass)&NhlxWorkstationClassRec;

/*
 * Function:	nhlfxworkstationclass
 *
 * Description:	Fortran ref function for xwork class.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global Fortran
 * Returns:	NhlClass
 * Side Effect:	
 */
NhlClass
_NHLCALLF(nhlfxworkstationclass,NHLFXWORKSTATIONCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlxWorkstationClass;
}

/*
 * Function:	XWorkstationClassInitialize
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
XWorkstationClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlEnumVals	cmvals[] = {
		{NhlSHARE,	"share"},
		{NhlPRIVATE,	"private"}
	};

	(void)_NhlRegisterEnumType(NhlxWorkstationClass,NhlTXColorMode,cmvals,
		NhlNumber(cmvals));

	return NhlNOERROR;
}

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
#if	NhlNeedProto
(
	NhlClass	class,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
        NhlClass	class;
        NhlLayer	req;
        NhlLayer	new;
        _NhlArgList	args;
        int		num_args; 
#endif
{
	char			*error_lead="XWorkstationInitialize";
	NhlXWorkstationLayer	wnew = (NhlXWorkstationLayer) new;
	NhlErrorTypes		ret = NhlNOERROR;

	if(!wnew->xwork.pause_set) wnew->xwork.pause = True;

	if(!wnew->xwork.window_id_set) {
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

		if(wnew->xwork.xcolor_mode == NhlPRIVATE)
			NhlPError(NhlINFO,NhlEUNKNOWN,
	"%s:If the %s resource is specified, the %s resource must be SHARE",
				error_lead,NhlNwkWindowId,NhlNwkXColorMode);
		wnew->xwork.xcolor_mode = NhlSHARE;
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
#if	NhlNeedProto
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
	char			func[]="XWorkstationSetValues";
	NhlXWorkstationLayer	wnew = (NhlXWorkstationLayer)new;

	if(wnew->xwork.pause && wnew->xwork.window_id_set){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s must be False if NhlNwkWindowId is specified",
			func,NhlNwkPause,NhlNwkWindowId);
		wnew->xwork.pause = False;
		return NhlWARNING;
	}

	return NhlNOERROR;
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
#if	NhlNeedProto
(
	NhlLayer	l	/* workstation layer to clear	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to clear	*/
#endif
{
	NhlWorkstationClass	lc = (NhlWorkstationClass)
						NhlworkstationClass;
	NhlXWorkstationLayer		xl = (NhlXWorkstationLayer)l;
	Gescape_in_data			indat;
	Gescape_out_data		*outdat;
	char				wkid[15];

	if(xl->xwork.pause){
		sprintf(wkid,"%d",_NhlWorkstationId(l));
		indat.escape_r1.size = strlen(wkid);
		indat.escape_r1.data = wkid;
		gescape(-1396,&indat,NULL,&outdat);
	}

	return (*(lc->work_class.clear_work))(l);
}

/*
 * Function:	XWorkstationOpen
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
static NhlErrorTypes
XWorkstationOpen
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	char				func[]="XWorkstationOpen";
	NhlXWorkstationLayer		xl = (NhlXWorkstationLayer)l;
	NhlXWorkstationLayerPart	*xp = &xl->xwork;
	int				i=2;

	if(xl->work.gkswkstype == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unknown workstation type");
		return(NhlFATAL);
		
	} 
	if(xl->work.gkswksconid == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unknown workstation connection id");
		return(NhlFATAL);
	}
	while(wksisopn(i)) {
		i++;
	}
	xl->work.gkswksid = i;

	_NHLCALLF(gopwk,GOPWK)(&(xl->work.gkswksid),&(xl->work.gkswksconid),
		&(xl->work.gkswkstype));
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
	gset_clip_ind(GIND_NO_CLIP);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func)){
		return NhlFATAL;
	}

	if(xp->xcolor_mode == NhlPRIVATE){
		c_ngseti("wo",_NhlWorkstationId(l));
		c_ngseti("pr",True);
	}

	return _NhlAllocateColors(l);
}

/*
 *      $Id: XWorkstation.c,v 1.35 2003-11-25 22:41:34 dbrown Exp $
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
 *	Description:	Responsible for managing the X workstation element */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/hlu/XWorkstationP.h>
#include <ncarg/hlu/ConvertersP.h>

#define	Oset(field)	NhlOffset(NhlXWorkstationLayerRec,xwork.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(window_id_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNwkWindowId,NhlCwkWindowId,NhlTInteger,sizeof(int),Oset(window_id),
		 NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),_NhlRES_NOSACCESS,NULL},
	{NhlNwkXColorMode,NhlCwkXColorMode,NhlTXColorMode,sizeof(NhlXColorMode),
	 	Oset(xcolor_mode),NhlTImmediate,_NhlUSET((NhlPointer)-1),
		0,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(pause_set),
		 NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNwkPause,NhlCwkPause,NhlTBoolean,sizeof(NhlBoolean),
		 Oset(pause),NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

	{"no.res","no.res",NhlTInteger,sizeof(int),Oset(xwinconfig.type),
		 NhlTImmediate,_NhlUSET((NhlPointer)NGC_XWINCONFIG),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{"no.res","no.res",NhlTInteger,sizeof(int),Oset(xwinconfig.work_id),
		 NhlTImmediate,_NhlUSET((NhlPointer)-1),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNwkX,NhlCwkX,NhlTInteger,sizeof(int),Oset(xwinconfig.x),
		 NhlTImmediate,_NhlUSET((NhlPointer)-1),_NhlRES_NOSACCESS,NULL},
	{NhlNwkY,NhlCwkY,NhlTInteger,sizeof(int),Oset(xwinconfig.y),
		 NhlTImmediate,_NhlUSET((NhlPointer)-1),_NhlRES_NOSACCESS,NULL},
	{NhlNwkWidth,NhlCwkWidth,NhlTInteger,sizeof(int),
         	Oset(xwinconfig.width),
		 NhlTImmediate,_NhlUSET((NhlPointer)-1),_NhlRES_NOSACCESS,NULL},
	{NhlNwkHeight,NhlCwkHeight,NhlTInteger,sizeof(int),
		Oset(xwinconfig.height),NhlTImmediate,
		 _NhlUSET((NhlPointer)-1),_NhlRES_NOSACCESS,NULL},
	{NhlNwkTitle,NhlCwkTitle,NhlTString,sizeof(NhlString),
		 Oset(xwinconfig.title),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
	{NhlNwkIconTitle,NhlCwkIconTitle,NhlTString,sizeof(NhlString),
		 Oset(xwinconfig.icon_title),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
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

static NhlErrorTypes XWorkstationDestroy(
#if	NhlNeedProto
	NhlLayer
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

static NhlErrorTypes XWorkstationGetValues(
#if	NhlNeedProto
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
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

static NhlErrorTypes XWorkstationAllocateColors(
#if  NhlNeedProto
	NhlWorkstationLayer	wl,
	NhlPrivateColor		*new,
	NhlPrivateColor		*old
#endif
);

static NrmQuark Qtitle = NrmNULLQUARK;
static NrmQuark Qicon_title = NrmNULLQUARK;

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
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	NULL,
/* class_initialize		*/	XWorkstationClassInitialize,
/* layer_initialize		*/	XWorkstationInitialize,
/* layer_set_values		*/	XWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	XWorkstationGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	XWorkstationDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
        },
        {
/* current_wks_count	*/	NhlInheritCurrentWksCount, 
/* gks_wks_recs		*/	NhlInheritGksWksRecs,
/* hlu_wks_flag		*/	NhlInheritHluWksFlag,
/* def_background	*/	{0.0,0.0,0.0},
/* rgb_dbm		*/	NULL,
/* pal			*/	NhlInheritPalette,
/* open_work		*/	XWorkstationOpen,
/* close_work		*/	NhlInheritClose,
/* activate_work	*/	NhlInheritActivate,
/* deactivate_work	*/	NhlInheritDeactivate,
/* alloc_colors		*/	XWorkstationAllocateColors,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	XWorkstationClear,
/* lineto_work 		*/	NhlInheritLineTo,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker,
/* notify_work		*/	NULL,
/* update_drawbb        */      NULL
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
		{NhlSHARE,	"Share"},
		{NhlSHARE,	"Shared"},
		{NhlPRIVATE,	"Private"},
		{NhlMIXED,	"Mixed"}
	};

	(void)_NhlRegisterEnumType(NhlxWorkstationClass,NhlTXColorMode,cmvals,
		NhlNumber(cmvals));

        Qtitle = NrmStringToQuark(NhlNwkTitle);
        Qicon_title = NrmStringToQuark(NhlNwkIconTitle);

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
	NhlWorkstationClassPart	*wcp =
				&((NhlWorkstationClass)class)->work_class;
	NhlErrorTypes		ret = NhlNOERROR;
	char			*tstr;
        
	if(*wcp->current_wks_count >= MAX_OPEN_WKS){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
              "%s: Limit reached for number of simultaneous GKS Workstations",
			error_lead);
		return NhlFATAL;
	}

	if(!wnew->xwork.pause_set) wnew->xwork.pause = True;

	if(!wnew->xwork.window_id_set) {
	/*
	 * Not sure if this is ignored or not
	 */
		wnew->work.gkswksconid = 2;
		wnew->work.gkswkstype = 8;
		if(wnew->xwork.xcolor_mode == -1)
			wnew->xwork.xcolor_mode = NhlMIXED;
		
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

		if((wnew->xwork.xcolor_mode == NhlPRIVATE) ||
					(wnew->xwork.xcolor_mode == NhlMIXED))
			NhlPError(NhlINFO,NhlEUNKNOWN,
	"%s:If the %s resource is specified, the %s resource must be SHARE",
				error_lead,NhlNwkWindowId,NhlNwkXColorMode);
		wnew->xwork.xcolor_mode = NhlSHARE;
	}

	if(!wnew->xwork.xwinconfig.title)
		wnew->xwork.xwinconfig.title = (char*)wnew->base.name;
	tstr = wnew->xwork.xwinconfig.title;
	wnew->xwork.xwinconfig.title =
				(char*)NhlMalloc((unsigned)strlen(tstr)+1);
	if(!wnew->xwork.xwinconfig.title){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(wnew->xwork.xwinconfig.title,tstr);

	if(!wnew->xwork.xwinconfig.icon_title)
		wnew->xwork.xwinconfig.icon_title = (char*)wnew->base.name;
	tstr = wnew->xwork.xwinconfig.icon_title;
	wnew->xwork.xwinconfig.icon_title =
				(char*)NhlMalloc((unsigned)strlen(tstr)+1);
	if(!wnew->xwork.xwinconfig.icon_title){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(wnew->xwork.xwinconfig.icon_title,tstr);

	return ret;
}

/*
 * Function:	XWorkstationDestroy
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
XWorkstationDestroy
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlXWorkstationLayer		xl = (NhlXWorkstationLayer)l;

	NhlFree(xl->xwork.xwinconfig.title);
	NhlFree(xl->xwork.xwinconfig.icon_title);

	return NhlNOERROR;
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
	char				func[]="XWorkstationSetValues";
	NhlXWorkstationLayer		wnew = (NhlXWorkstationLayer)new;
	NhlXWorkstationLayer		wold = (NhlXWorkstationLayer)old;
	NhlXWorkstationLayerPart	*xp = &wnew->xwork;
	NhlXWorkstationLayerPart	*op = &wold->xwork;
	int				wkid;
	int				err_num;
	NhlErrorTypes			ret = NhlNOERROR;

	if(wnew->xwork.pause && wnew->xwork.window_id_set){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s must be False if NhlNwkWindowId is specified",
			func,NhlNwkPause,NhlNwkWindowId);
		wnew->xwork.pause = False;
		ret = NhlWARNING;
	}

	wkid = _NhlWorkstationId(new);
	if(op->xcolor_mode != xp->xcolor_mode){
		if(xp->window_id_set){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:%s must be \"share\" if %s is specified",
				func,NhlNwkXColorMode,NhlNwkWindowId);
			xp->xcolor_mode = op->xcolor_mode;
			ret = NhlWARNING;
		}
		else{
			switch (xp->xcolor_mode){
				case NhlPRIVATE:
				c_ngseti("pc",wkid);
				break;
				case NhlSHARE:
				c_ngseti("sc",wkid);
				break;
				case NhlMIXED:
				default:
				c_ngseti("mc",wkid);
				break;
			}
			if(c_nerro(&err_num) && err_num == _NhlGKSERRNUM){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"%s:Unable to change %s",func,
					NhlNwkXColorMode);
				c_errof();
				ret = NhlWARNING;
				xp->xcolor_mode = op->xcolor_mode;
			}
		}
	}

	return ret;
}

/*
 * Function:	XWorkstationGetValues
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
XWorkstationGetValues
#if	NhlNeedProto
(
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
)
#else
(l,args,nargs)
	NhlLayer	l;
	_NhlArgList	args;
	int		nargs;
#endif
{
	char			func[] = "XWorkstationGetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlXWorkstationLayerPart *xp = &((NhlXWorkstationLayer)l)->xwork;
	int			i,size;
        NhlString		string,res;
        
	for(i=0;i<nargs;i++){
                string = NULL;
		if((args[i].quark == Qtitle) && xp->xwinconfig.title){
                        size = strlen(xp->xwinconfig.title)+1;
                        string = xp->xwinconfig.title;
                        res = NhlNwkTitle;
                }
                else if ((args[i].quark == Qicon_title) &&
                         xp->xwinconfig.icon_title){
                        size = strlen(xp->xwinconfig.icon_title)+1;
                        string = xp->xwinconfig.icon_title;
                        res = NhlNwkIconTitle;
                }
                if (string) {
			*(NhlString*)args[i].value.ptrval = NhlMalloc(size);
			if(*(NhlString*)args[i].value.ptrval == NULL){
				NhlPError(NhlWARNING,ENOMEM,
				"%s:Unable to allocate memory to retrieve %s",
							func,res);
				ret = MIN(ret,NhlWARNING);
			}
			strcpy(*(NhlString*)args[i].value.ptrval,string);
                }
	}

	return ret;
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

static void
GetSizeProc
#if	NhlNeedProto
(
	void		*closure,
	unsigned long	size
)
#else
(closure,size)
	void		*closure;
	unsigned long	size;
#endif
{
	int	*vsdev = (int*)closure;

	*vsdev = (int)size;

	return;
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
	NhlWorkstationClassPart	*wcp =
		&((NhlWorkstationClass)xl->base.layer_class)->work_class;
	_NGCXGetSizeChg			xgsc;
	Gescape_in_data			gesc_in_xgsc;
	Gescape_in_data			gesc_in_xwconf;

	if(xl->work.gkswkstype == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Unknown workstation type");
		return(NhlFATAL);
		
	} 
	if(xl->work.gkswksconid == NhlFATAL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"Unknown workstation connection id");
		return(NhlFATAL);
	}

	switch (xp->xcolor_mode){
		case NhlPRIVATE:
		c_ngseti("pc",-1);
		break;
		case NhlSHARE:
		c_ngseti("sc",-1);
		break;
		case NhlMIXED:
		default:
		c_ngseti("mc",-1);
		break;
	}

	gesc_in_xwconf.escape_r1.data = &xl->xwork.xwinconfig;
	gesc_in_xwconf.escape_r1.size = 0;
	gescape(NGESC_CNATIVE,&gesc_in_xwconf,NULL,NULL);

        _NhlUpdateGksWksRecs(l,True,&xl->work.gkswksid);
        *wcp->hlu_wks_flag = True;
	_NHLCALLF(gopwk,GOPWK)(&(xl->work.gkswksid),&(xl->work.gkswksconid),
		&(xl->work.gkswkstype));
	if(_NhlLLErrCheckPrnt(NhlFATAL,func))
		return NhlFATAL;
	gset_clip_ind(GIND_NO_CLIP);
	if(_NhlLLErrCheckPrnt(NhlWARNING,func)){
		return NhlFATAL;
	}
	gesc_in_xgsc.escape_r1.data = &xgsc;
	gesc_in_xgsc.escape_r1.size = 0;
	xgsc.type = NGC_XSIZECHG;
	xgsc.work_id = xl->work.gkswksid;
	xgsc.xget_size = GetSizeProc;
	xgsc.closure = &xl->work.vswidth_dev_units;
	gescape(NGESC_CNATIVE,&gesc_in_xgsc,NULL,NULL);

	return _NhlAllocateColors((NhlWorkstationLayer)l);
}

/*
 * Function:	XWorkstationAllocateColors
 *
 * Description: Used to allocate colors.
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
XWorkstationAllocateColors
#if  NhlNeedProto
(
	NhlWorkstationLayer 	wl,
	NhlPrivateColor		*old,
	NhlPrivateColor		*new
)
#else
(wl,old,new)
	NhlWorkstationLayer	wl;
	NhlPrivateColor		*old;
	NhlPrivateColor		*new;
#endif
{
	char				func[] = "XWorkstationAllocateColors";
	NhlXWorkstationLayer		xwk = (NhlXWorkstationLayer)wl;
	NhlWorkstationClassPart	*wcp =
		&((NhlWorkstationClass)wl->base.layer_class)->work_class;
	Gcolr_rep			tcrep;
	int				i;
	NhlPrivateColor			*pcmap = new;
	NhlXPixel			*xpixnums = xwk->xwork.xpixels;
	NhlErrorTypes			ret = NhlNOERROR;
	_NGCXGetXPix			getxpix;
	_NGCXFreeCi			freeci;
	Gescape_in_data			gesc_in_getxpix;
	Gescape_in_data			gesc_in_freeci;

	gesc_in_getxpix.escape_r1.data = &getxpix;
	gesc_in_getxpix.escape_r1.size = 0;
	gesc_in_freeci.escape_r1.data = &freeci;
	gesc_in_freeci.escape_r1.size = 0;
	getxpix.type = NGC_XGETXPIX;
	freeci.type = NGC_XFREECI;
	getxpix.work_id = freeci.work_id = wl->work.gkswksid;

	for ( i = 0; i < _NhlMAX_COLOR_MAP; i++) {
		switch(pcmap[i].cstat){
			case _NhlCOLNEW:
			case _NhlCOLCHANGE:
				tcrep.rgb.red = pcmap[i].red;
				tcrep.rgb.green = pcmap[i].green;
				tcrep.rgb.blue= pcmap[i].blue;
				gset_colr_rep(wl->work.gkswksid,i,&tcrep);
				if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
					ret = NhlWARNING;
					pcmap[i].cstat = _NhlCOLUNSET;
					freeci.gksci = i;
					gescape(NGESC_CNATIVE,&gesc_in_freeci,
						NULL,NULL);
					(void)_NhlLLErrCheckPrnt(NhlWARNING,
									func);
				}
				else {
					pcmap[i].cstat = _NhlCOLSET;
					pcmap[i].ci = i;
					getxpix.gksci = i;
					gescape(NGESC_CNATIVE,&gesc_in_getxpix,
						NULL,NULL);
					if(!_NhlLLErrCheckPrnt(NhlWARNING,func))
						xpixnums[i] = getxpix.xpixnum;
				}
				break;

			case _NhlCOLREMOVE:
				pcmap[i].cstat = _NhlCOLUNSET;
				freeci.gksci = i;
				gescape(NGESC_CNATIVE,&gesc_in_freeci,
								NULL,NULL);
				(void)_NhlLLErrCheckPrnt(NhlWARNING,func);
			case _NhlCOLUNSET:
				pcmap[i].red = -1.0;
				pcmap[i].green = -1.0;
				pcmap[i].blue = -1.0;
				break;

			case _NhlCOLSET:
				break;
		}
	}

	/*
	 * The background and the foreground MUST be defined.
	 */
	if(pcmap[NhlBACKGROUND].cstat == _NhlCOLUNSET){
		tcrep.rgb.red = pcmap[NhlBACKGROUND].red =
							wcp->def_background[0];
		tcrep.rgb.green = pcmap[NhlBACKGROUND].green =
							wcp->def_background[1];
		tcrep.rgb.blue = pcmap[NhlBACKGROUND].blue =
							wcp->def_background[2];
		gset_colr_rep(wl->work.gkswksid,NhlBACKGROUND,&tcrep);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
			ret = NhlWARNING;
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Problems setting Background:Undefined results",
				func);
		}
		pcmap[NhlBACKGROUND].cstat = _NhlCOLSET;
		getxpix.gksci = NhlBACKGROUND;
		gescape(NGESC_CNATIVE,&gesc_in_getxpix,NULL,NULL);
		if(!_NhlLLErrCheckPrnt(NhlWARNING,func))
			xpixnums[NhlBACKGROUND] = getxpix.xpixnum;
	}

	if(pcmap[NhlFOREGROUND].cstat == _NhlCOLUNSET){
		if (pcmap[NhlBACKGROUND].red * pcmap[NhlBACKGROUND].red +
		    pcmap[NhlBACKGROUND].green * pcmap[NhlBACKGROUND].green +
		    pcmap[NhlBACKGROUND].blue * pcmap[NhlBACKGROUND].blue
		    < .75) {
			tcrep.rgb.red = pcmap[NhlFOREGROUND].red =
			tcrep.rgb.green = pcmap[NhlFOREGROUND].green =
			tcrep.rgb.blue = pcmap[NhlFOREGROUND].blue = 1.0;
		}
		else {
			tcrep.rgb.red = pcmap[NhlFOREGROUND].red =
			tcrep.rgb.green = pcmap[NhlFOREGROUND].green =
			tcrep.rgb.blue = pcmap[NhlFOREGROUND].blue = 0.0;
		}
		gset_colr_rep(wl->work.gkswksid,NhlFOREGROUND,&tcrep);
		if(_NhlLLErrCheckPrnt(NhlWARNING,func)) {
			ret = NhlWARNING;
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Problems setting Foreground:Undefined results",
				func);
		}
		pcmap[NhlFOREGROUND].cstat = _NhlCOLSET;
		getxpix.gksci = NhlFOREGROUND;
		gescape(NGESC_CNATIVE,&gesc_in_getxpix,NULL,NULL);
		if(!_NhlLLErrCheckPrnt(NhlWARNING,func))
			xpixnums[NhlFOREGROUND] = getxpix.xpixnum;
	}

	return ret;
}

NhlErrorTypes
_NhlGetXPixel
#if  NhlNeedProto
(
	NhlLayer	l,
	int		hlu_indx,
	NhlXPixel	*xpix
)
#else
(l,hlu_indx,xpix)
	NhlLayer	l;
	int		hlu_indx;
	NhlXPixel	*xpix;
#endif
{
	char	func[] = "_NhlGetXPixel";
	NhlXWorkstationLayer	wl = (NhlXWorkstationLayer)l;

	if(!l || !_NhlIsXWorkstation(l)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid XWorkstation object",
			func);
		return NhlFATAL;
	}


	if(hlu_indx < 0){
		*xpix = wl->xwork.xpixels[NhlFOREGROUND];
		return NhlWARNING;
	}

	if(hlu_indx >= wl->work.color_map_len){
		hlu_indx = hlu_indx % (wl->work.color_map_len - 1);
		if(!hlu_indx)
			hlu_indx = wl->work.color_map_len - 1;
	}

	if(wl->work.private_color_map[hlu_indx].cstat != _NhlCOLSET){
		*xpix = wl->xwork.xpixels[NhlFOREGROUND];
		return NhlWARNING;
	}

	*xpix = wl->xwork.xpixels[hlu_indx];
	return NhlNOERROR;
}

NhlErrorTypes
NhlGetXPixel
#if  NhlNeedProto
(
	int		id,
	int		hlu_indx,
	NhlXPixel	*xpix
)
#else
(id,hlu_indx,xpix)
	int		id;
	int		hlu_indx;
	NhlXPixel	*xpix;
#endif
{
	NhlLayer	l = _NhlGetLayer(id);

	if(!l)
		return NhlFATAL;

	return _NhlGetXPixel(l,hlu_indx,xpix);
}

/*
 *      $Id: ImageWorkstation.c,v 1.5 2010-01-21 22:16:48 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ImageWorkstation.c
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
#include <ncarg/hlu/ImageWorkstationP.h>
#include <ncarg/hlu/ConvertersP.h>

#define	Oset(field)	NhlOffset(NhlImageWorkstationLayerRec,imagework.field)
static NhlResource resources[] = {

/* Begin-documented-resources */

#ifdef BuildPNG
	{NhlNwkImageFormat,NhlCwkImageFormat,NhlTImageFormat,
	 sizeof(NhlImageFormat),
		Oset(pixconfig.format),NhlTImmediate,_NhlUSET((NhlPointer)NhlPNG),
		_NhlRES_NOSACCESS,NULL},
#else
	{NhlNwkImageFormat,NhlCwkImageFormat,NhlTImageFormat,
	 sizeof(NhlImageFormat),
		Oset(pixconfig.format),NhlTImmediate,_NhlUSET((NhlPointer)NhlXWD),
		_NhlRES_NOSACCESS,NULL},
#endif
	{NhlNwkImageFileName,NhlCwkImageFileName,NhlTString,
		sizeof(NhlString),Oset(pixconfig.filename),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},

	{NhlNwkXColorMode,NhlCwkXColorMode,
	 NhlTXColorMode,sizeof(NhlXColorMode),
	 Oset(xcolor_mode),NhlTImmediate,_NhlUSET((NhlPointer)-1),
	 0,NULL},
	{"no.res","no.res",NhlTBoolean,sizeof(NhlBoolean),Oset(pause_set),
		NhlTImmediate,_NhlUSET((NhlPointer)True),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{NhlNwkPause,NhlCwkPause,NhlTBoolean,sizeof(NhlBoolean),
		Oset(pause),NhlTProcedure,_NhlUSET((NhlPointer)_NhlResUnset),0,NULL},

	{"no.res","no.res",NhlTInteger,sizeof(int),Oset(pixconfig.type),
		NhlTImmediate,_NhlUSET((NhlPointer)NGC_PIXCONFIG),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
	{"no.res","no.res",NhlTInteger,sizeof(int),Oset(pixconfig.work_id),
		NhlTImmediate,_NhlUSET((NhlPointer)-1),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},

	{NhlNwkWidth,NhlCwkWidth,NhlTInteger,sizeof(int),
         	Oset(pixconfig.width),
		NhlTImmediate,_NhlUSET((NhlPointer)512),_NhlRES_NOSACCESS,NULL},
	{NhlNwkHeight,NhlCwkHeight,NhlTInteger,sizeof(int),
		Oset(pixconfig.height),NhlTImmediate,
		_NhlUSET((NhlPointer)512),_NhlRES_NOSACCESS,NULL},

/* End-documented-resources */

};
#undef	Oset

/*
* ImageWorkstation base_class method declarations
*/

static NhlErrorTypes ImageWorkstationClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes ImageWorkstationInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes ImageWorkstationDestroy(
#if	NhlNeedProto
	NhlLayer
#endif
);

static NhlErrorTypes ImageWorkstationSetValues(
#if	NhlNeedProto
        NhlLayer,	/* old */
        NhlLayer,	/* reference */
        NhlLayer,	/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes ImageWorkstationGetValues(
#if	NhlNeedProto
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes ImageWorkstationOpen(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

static NhlErrorTypes ImageWorkstationClear(
#if	NhlNeedProto
	NhlLayer	l	/* workstation layer to clear	*/
#endif
);

static NhlErrorTypes ImageWorkstationAllocateColors(
#if  NhlNeedProto
	NhlWorkstationLayer	wl,
	NhlPrivateColor		*new,
	NhlPrivateColor		*old
#endif
);

static NrmQuark Qfilename = NrmNULLQUARK;

NhlImageWorkstationClassRec NhlimageWorkstationClassRec = {
        {
/* class_name			*/	"xwdimageWorkstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlImageWorkstationLayerRec),
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
/* class_initialize		*/	ImageWorkstationClassInitialize,
/* layer_initialize		*/	ImageWorkstationInitialize,
/* layer_set_values		*/	ImageWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	ImageWorkstationGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	ImageWorkstationDestroy,

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
/* open_work		*/	ImageWorkstationOpen,
/* close_work		*/	NhlInheritClose,
/* activate_work	*/	NhlInheritActivate,
/* deactivate_work	*/	NhlInheritDeactivate,
/* alloc_colors		*/	ImageWorkstationAllocateColors,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	ImageWorkstationClear,
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

NhlClass NhlimageWorkstationClass = (NhlClass)&NhlimageWorkstationClassRec;

/*
 * Function:	nhlfimageworkstationclass
 *
 * Description:	Fortran ref function for imagework class.
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
_NHLCALLF(nhlfimageworkstationclass,NHLFIMAGEWORKSTATIONCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlimageWorkstationClass;
}

/*
 * Function:	ImageWorkstationClassInitialize
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
ImageWorkstationClassInitialize
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
	_NhlEnumVals	fmtvals[] = {
		{NhlXWD,	"XWD"},
#ifdef BuildPNG
		{NhlPNG,	"PNG"},
#endif
	};


	(void)_NhlRegisterEnumType(NhlimageWorkstationClass,
				   NhlTXColorMode,cmvals,
		NhlNumber(cmvals));

	(void)_NhlRegisterEnumType(NhlimageWorkstationClass,
				   NhlTImageFormat,
				   fmtvals,NhlNumber(fmtvals));


	Qfilename = NrmStringToQuark(NhlNwkImageFileName);

	return NhlNOERROR;
}

/*
 * Function:	ImageWorkstationInitialize
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
static NhlErrorTypes ImageWorkstationInitialize
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
	char			*error_lead="ImageWorkstationInitialize";
	NhlImageWorkstationLayer	wnew = (NhlImageWorkstationLayer) new;
	NhlWorkstationClassPart	*wcp =
				&((NhlWorkstationClass)class)->work_class;
	NhlErrorTypes		ret = NhlNOERROR;
	char			*tstr = NULL;

	if(*wcp->current_wks_count >= MAX_OPEN_WKS){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
              "%s: Limit reached for number of simultaneous GKS Workstations",
			error_lead);
		return NhlFATAL;
	}

	if(!wnew->imagework.pause_set) wnew->imagework.pause = True;

	wnew->work.gkswksconid = 2;
	wnew->work.gkswkstype = 9;
	if(wnew->imagework.xcolor_mode == -1)
		wnew->imagework.xcolor_mode = NhlMIXED;

	if (wnew->imagework.pixconfig.filename) {
		tstr = (char*)_NGResolvePath
			(wnew->imagework.pixconfig.filename);
		if(!tstr){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Unable to resolve path name for \"%s\", defaulting %s",
				  error_lead,
				  wnew->imagework.pixconfig.filename,
				  NhlNwkImageFileName);
			ret = NhlWARNING;
		}
	}
	if (! tstr) {
		tstr = (char*)new->base.name;
	}
	wnew->imagework.pixconfig.filename = NhlMalloc(strlen(tstr)+1);
	if(!wnew->imagework.pixconfig.filename){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(wnew->imagework.pixconfig.filename,tstr);

	return ret;
}

/*
 * Function:	ImageWorkstationDestroy
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
ImageWorkstationDestroy
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlImageWorkstationLayer		xl = (NhlImageWorkstationLayer)l;

	NhlFree(xl->imagework.pixconfig.filename);

	return NhlNOERROR;
}

/*
 * Function:	ImageWorkstationSetValues
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
static NhlErrorTypes ImageWorkstationSetValues
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
	char				func[]="ImageWorkstationSetValues";
	NhlImageWorkstationLayer		wnew = (NhlImageWorkstationLayer)new;
	NhlImageWorkstationLayer		wold = (NhlImageWorkstationLayer)old;
	NhlImageWorkstationLayerPart	*xp = &wnew->imagework;
	NhlImageWorkstationLayerPart	*op = &wold->imagework;
	int				wkid;
	int				err_num;
	NhlErrorTypes			ret = NhlNOERROR;

	wkid = _NhlWorkstationId(new);
	if(op->xcolor_mode != xp->xcolor_mode){
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

	return ret;
}

/*
 * Function:	ImageWorkstationGetValues
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
ImageWorkstationGetValues
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
	char			func[] = "ImageWorkstationGetValues";
	NhlErrorTypes		ret = NhlNOERROR;
	NhlImageWorkstationLayerPart *xp = &((NhlImageWorkstationLayer)l)->imagework;
	int			i,size;
        NhlString		string,res;

	for(i=0;i<nargs;i++){
                string = NULL;
		if((args[i].quark == Qfilename) && xp->pixconfig.filename){
                        size = strlen(xp->pixconfig.filename)+1;
                        string = xp->pixconfig.filename;
                        res = NhlNwkImageFileName;
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
 * Function:	ImageWorkstationClear
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
ImageWorkstationClear
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
	NhlImageWorkstationLayer	xl = (NhlImageWorkstationLayer)l;
	Gescape_in_data			indat;
	Gescape_out_data		*outdat;
	char				wkid[15];

	if(xl->imagework.pause){
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
 * Function:	ImageWorkstationOpen
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
ImageWorkstationOpen
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	char				func[]="ImageWorkstationOpen";
	NhlImageWorkstationLayer	xl = (NhlImageWorkstationLayer)l;
	NhlImageWorkstationLayerPart	*xp = &xl->imagework;
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

	gesc_in_xwconf.escape_r1.data = &xl->imagework.pixconfig;
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
 * Function:	ImageWorkstationAllocateColors
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
ImageWorkstationAllocateColors
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
	char			  func[] = "ImageWorkstationAllocateColors";
	NhlImageWorkstationLayer  xwk = (NhlImageWorkstationLayer)wl;
	NhlWorkstationClassPart	*wcp =
		&((NhlWorkstationClass)wl->base.layer_class)->work_class;
	Gcolr_rep			tcrep;
	int				i;
	NhlPrivateColor			*pcmap = new;
	NhlXPixel			*xpixnums = xwk->imagework.xpixels;
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
					if(!_NhlLLErrCheckPrnt
					   (NhlWARNING,func))
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

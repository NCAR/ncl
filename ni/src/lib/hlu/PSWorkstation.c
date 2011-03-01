/*
 *      $Id: PSWorkstation.c,v 1.21 2010-03-29 16:30:03 brownrig Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		PSWorkstation.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 24 00:28:37 MST 1995
 *
 *	Description:
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/PSWorkstationP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/pageutil.h>

#define Oset(field)	NhlOffset(NhlPSWorkstationLayerRec,ps.field)
static NhlResource resources[] = {
/* Begin-documented-resources */

	{NhlNwkPSFormat,NhlCwkPSFormat,NhlTPSFormat,sizeof(NhlPSFormat),
		Oset(format),NhlTImmediate,_NhlUSET((NhlPointer)NhlPS),
		_NhlRES_NOSACCESS,NULL},
	{NhlNwkVisualType,NhlCwkVisualType,NhlTVisualType,sizeof(NhlVisualType),
		Oset(visual),NhlTImmediate,_NhlUSET((NhlPointer)NhlCOLOR),
		_NhlRES_NOSACCESS,NULL},
	{NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
		sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlPORTRAIT),_NhlRES_DEFAULT,NULL},
	{NhlNwkPSFileName,NhlCwkPSFileName,NhlTString,
		sizeof(NhlString),Oset(filename),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
	{NhlNwkPSResolution,NhlCwkPSResolution,NhlTInteger,
		sizeof(int),Oset(resolution),NhlTImmediate,
		_NhlUSET((NhlPointer)1800),_NhlRES_NOSACCESS,NULL},
	{NhlNwkFullBackground,NhlCwkFullBackground,NhlTBoolean,
		sizeof(NhlBoolean),Oset(full_background),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL},
	{NhlNwkColorModel,NhlCwkColorModel,NhlTColorModel,
	 	sizeof(NhlColorModel),
		Oset(color_model),NhlTImmediate,_NhlUSET((NhlPointer)NhlRGB),
		_NhlRES_NOSACCESS,NULL},
 	{NhlNwkSuppressBackground,NhlCwkSuppressBackground,NhlTBoolean,
		sizeof(NhlBoolean),Oset(suppress_background),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_NOSACCESS,NULL},
 	{NhlNwkSuppressBBInfo,NhlCwkSuppressBBInfo,NhlTBoolean,
		sizeof(NhlBoolean),Oset(suppress_bbinfo),NhlTImmediate,
		_NhlUSET((NhlPointer)False),_NhlRES_NOSACCESS,NULL},

    /* these page size and margins are initialized as "-1" here, and are given appropriate
     * values when the workstation is opened, depending upon which resources are actually
     * available at that time.
     */
    {NhlNwkPaperSize,NhlCwkPaperSize,NhlTString,
        sizeof(NhlString),Oset(paper_size),NhlTImmediate,
        _NhlUSET(PAGEUTIL_DEFAULT_PAPERSIZE),_NhlRES_NOSACCESS,NULL},
    {NhlNwkPaperWidthF, NhlCwkPaperWidthF, NhlTFloat,
        sizeof(float), Oset(page_width), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkPaperHeightF, NhlCwkPaperHeightF, NhlTFloat,
        sizeof(float), Oset(page_height), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
        sizeof(int),Oset(lower_x),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
        sizeof(int),Oset(lower_y),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
        sizeof(int),Oset(upper_x),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},
    {NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
        sizeof(int),Oset(upper_y),NhlTImmediate,
        _NhlUSET((NhlPointer)-1),_NhlRES_DEFAULT,NULL},


/* End-documented-resources */
};

/*
* PSWorkstation base_class method declarations
*/

static NhlErrorTypes PSWorkstationClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes PSWorkstationInitialize(
#if	NhlNeedProto
        NhlClass,     /* class */
        NhlLayer,          /* req */
        NhlLayer,          /* new */
        _NhlArgList,        /* args */
        int             /* num_args */
#endif
);

static NhlErrorTypes PSWorkstationClassPartInitialize(
#if	NhlNeedProto
        NhlClass      /* lc */
#endif
);

static NhlErrorTypes PSWorkstationDestroy(
#if	NhlNeedProto
        NhlLayer           /* inst */
#endif
);

static NhlErrorTypes PSWorkstationSetValues(
#if	NhlNeedProto
        NhlLayer,		/* old */
        NhlLayer,		/* reference */
        NhlLayer,		/* new */
        _NhlArgList,	/* args */
        int		/* num_args*/
#endif
);

static NhlErrorTypes PSWorkstationGetValues(
#if	NhlNeedProto
	NhlLayer, /*l */
	_NhlArgList, /* args */
	int	/*nargs*/
#endif
);

/*
* PSWorkstation work_class method declarations
*/

static NhlErrorTypes PSWorkstationOpen(
#if	NhlNeedProto
	NhlLayer /* instance */
#endif
);

static NhlErrorTypes PSWorkstationActivate(
#if	NhlNeedProto
	NhlLayer	l	/* instance	*/
#endif
);

static NhlErrorTypes PSWorkstationClear(
#if	NhlNeedProto
	NhlLayer	l	/* instance	*/
#endif
);

static NhlErrorTypes PSUpdateDrawBB(
#if	NhlNeedProto
	NhlLayer	wl,
	NhlBoundingBox *bbox
#endif
);

NhlPSWorkstationClassRec NhlpsWorkstationClassRec = {
        {
/* class_name			*/	"psWorkstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlPSWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlworkstationClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	PSWorkstationClassPartInitialize,
/* class_initialize		*/	PSWorkstationClassInitialize,
/* layer_initialize		*/	PSWorkstationInitialize,
/* layer_set_values		*/	PSWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	PSWorkstationGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	PSWorkstationDestroy,

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
/* def_background	*/	{1.0,1.0,1.0},
/* rgb_dbm		*/	NULL,
/* pal			*/	NhlInheritPalette,
/* open_work		*/	PSWorkstationOpen,
/* close_work		*/	NhlInheritClose,
/* activate_work	*/	PSWorkstationActivate,
/* deactivate_work	*/	NhlInheritDeactivate,
/* alloc_colors		*/	NhlInheritAllocateColors,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	PSWorkstationClear,
/* lineto_work		*/	NhlInheritLineTo,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker,
/* notify_work		*/	NULL,
/* update_drawbb        */      PSUpdateDrawBB
	},
	{
/* foo	*/			0
	}
};

NhlClass NhlpsWorkstationClass = (NhlClass)&NhlpsWorkstationClassRec;

/*
 * Function:	nhlfpsworkstationclass
 *
 * Description:	fortran ref to this class
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
_NHLCALLF(nhlfpsworkstationclass,NHLFPSWORKSTATIONCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlpsWorkstationClass;
}

/*
 * Function:	PSWorkstationClassPartInitialize
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
PSWorkstationClassPartInitialize
#if	NhlNeedProto
(
        NhlClass	lc
)
#else
(lc)
        NhlClass	lc;
#endif
{

	return NhlNOERROR;
}

static NrmQuark	fnameQ = NrmNULLQUARK;

/*
 * Function:	PSWorkstationClassInitialize
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
PSWorkstationClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlEnumVals	visvals[] = {
		{NhlCOLOR,	"Color"},
		{NhlMONOCHROME,	"Monochrome"}
	};
	_NhlEnumVals	fmtvals[] = {
		{NhlPS,		"PS"},
		{NhlEPS,	"EPS"},
		{NhlEPSI,	"EPSI"}
	};
	_NhlEnumVals	orientvals[] = {
		{NhlPORTRAIT,	"Portrait"},
		{NhlLANDSCAPE,	"Landscape"}
	};
	_NhlEnumVals	colmodelvals[] = {
		{NhlCMYK,	"CMYK"},
		{NhlRGB,	"RGB"}
	};


	(void)_NhlRegisterEnumType(NhlpsWorkstationClass,NhlTVisualType,
		visvals,NhlNumber(visvals));
	(void)_NhlRegisterEnumType(NhlpsWorkstationClass,NhlTPSFormat,
		fmtvals,NhlNumber(fmtvals));
	(void)_NhlRegisterEnumType(NhlpsWorkstationClass,NhlTWorkOrientation,
		orientvals,NhlNumber(orientvals));
	(void)_NhlRegisterEnumType(NhlpsWorkstationClass,NhlTColorModel,
		colmodelvals,NhlNumber(colmodelvals));

	fnameQ = NrmStringToQuark(NhlNwkPSFileName);

	return NhlNOERROR;
}

/*
 * Function:	PSWorkstationInitialize
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
static NhlErrorTypes PSWorkstationInitialize
#if	NhlNeedProto
(NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args)
#else
(lclass,req,new,args,num_args)
        NhlClass lclass;
        NhlLayer req;
        NhlLayer new;
        _NhlArgList args;
        int num_args;
#endif
{
	char				func[]="PSWorkstationInitialize";
	NhlPSWorkstationLayer		wnew = (NhlPSWorkstationLayer)new;
	NhlPSWorkstationLayerPart	*np = &wnew->ps;
	char				*tfname = NULL;
	char				buff[_NhlMAXFNAMELEN];
	NhlErrorTypes			ret = NhlNOERROR;

	wnew->work.gkswkstype = PSBASE+np->format+np->visual+np->orientation;
	wnew->work.gkswksconid = 0;

	if(np->filename){
		tfname = (char*)_NGResolvePath(np->filename);
		if(!tfname){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:Unable to resolve path name for \"%s\", defaulting %s",
				func,np->filename,NhlNwkPSFileName);
			ret = NhlWARNING;
		}
	}

	if(!tfname){
		strcpy(buff,new->base.name);
		strcat(buff,".");
		switch(np->format){
			case NhlPS:
				strcat(buff,"ps");
				break;
			case NhlEPS:
				strcat(buff,"eps");
				break;
			case NhlEPSI:
				strcat(buff,"epsi");
				break;
			default:
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					"%s:Unsupported PS format %d?",func,
					np->format);
				return NhlFATAL;
		}
		tfname = buff;
	}
	if (strlen(tfname) > _NhlMAXLLUPATHLEN) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "%s: Filepath %s exceeds maximum length of %d", func,
			  tfname,_NhlMAXLLUPATHLEN);
		return NhlFATAL;
	}
		
	np->filename = NhlMalloc(strlen(tfname)+1);
	if(!np->filename){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(np->filename,tfname);

    if (np->paper_size) {
        char* tmpStr = np->paper_size;
        np->paper_size = NhlMalloc(strlen(tmpStr) + 1);
        if (!np->paper_size) {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }
        strcpy(np->paper_size, tmpStr);
    }

	if(np->lower_x > 0 && np->upper_x > 0 && np->lower_x >= np->upper_x){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Device X Coordinates invalid, defaulting",func);
		ret = NhlWARNING;
		np->lower_x = 36;
		np->upper_x = 576;
	}
	if(np->lower_y > 0 && np->upper_y > 0 && np->lower_y >= np->upper_y){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Device Y Coordinates invalid, defaulting",func);
		ret = NhlWARNING;
		np->lower_y = 126;
		np->upper_y = 666;
	}
	np->dev_bounds_updated = False;

	return ret;
}

/*
 * Function:	PSWorkstationSetValues
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
static NhlErrorTypes
PSWorkstationSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	ref,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
#else
(old,ref,new,args,nargs)
	NhlLayer	old;
	NhlLayer	ref;
	NhlLayer	new;
	_NhlArgList	args;
	int		nargs;
#endif
{
	char			  func[]="PSWorkstationInitialize";
	NhlPSWorkstationLayerPart *np = &((NhlPSWorkstationLayer)new)->ps;
	NhlPSWorkstationLayerPart *op = &((NhlPSWorkstationLayer)old)->ps;
	NhlErrorTypes ret = NhlNOERROR;

	if(np->full_background != op->full_background){
		c_ngseti("wo",_NhlWorkstationId(new));
		c_ngseti("fu",np->full_background);
	}
	if (np->lower_x != op->lower_x ||
	    np->upper_x != op->upper_x ||
	    np->lower_y != op->lower_y ||
	    np->upper_y != op->upper_y ||
	    np->orientation != op->orientation)
		np->dev_bounds_updated = True;

	if(np->lower_x >= np->upper_x){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Device X Coordinates invalid, defaulting",func);
		ret = NhlWARNING;
		np->lower_x = 36;
		np->upper_x = 576;
	}
	if(np->lower_y >= np->upper_y){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:Device Y Coordinates invalid, defaulting",func);
		ret = NhlWARNING;
		np->lower_y = 126;
		np->upper_y = 666;
	}

	return ret;
}

/*
 * Function:	PSWorkstationGetValues
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
PSWorkstationGetValues
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
	char				func[]="PSWorkStationGetValues";
	register int			i;
	NhlPSWorkstationLayerPart	*pp = &((NhlPSWorkstationLayer)l)->ps;
	NhlString			str;
	NhlErrorTypes			ret = NhlNOERROR;

	for(i=0;i<nargs;i++) {
		str = NULL;

		if(args[i].quark == fnameQ) {
			str = pp->filename;
		}

		if(str != NULL){
			*(NhlString *)args[i].value.ptrval =
						NhlMalloc(strlen(str)+1);
			if(!*(NhlString *)args[i].value.ptrval){
				NhlPError(NhlWARNING,ENOMEM,
					"%s:Unable to retrieve %s",func,
					NrmQuarkToString(args[i].quark));
				ret = NhlWARNING;
			}
			else
				strcpy(*(NhlString *)args[i].value.ptrval,str);
		}
	}

	return ret;
}

/*
 * Function:    PSWorkstationDestroy
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
PSWorkstationDestroy
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlPSWorkstationLayerPart	*psp = &((NhlPSWorkstationLayer)l)->ps;

	NhlFree(psp->filename);
    NhlFree(psp->paper_size);

	return NhlNOERROR;
}

/*
 * Function:	PSWorkstationOpen
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
PSWorkstationOpen
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlWorkstationLayer		work = (NhlWorkstationLayer)l;
	NhlPSWorkstationLayerPart	*pp = &((NhlPSWorkstationLayer)l)->ps;
	NhlErrorTypes			ret;
	int				d,w,h;
	int				su = 0;

    /* make use of a shared utility method that contains all the page-sizing logic common to cairo-document,
     * postscript, and PDF workstations. See pageutil.c
     */
    NhlPageInfo pageInfo;
    pageInfo.paperSize = pp->paper_size;
    pageInfo.paperSizeResName = NhlNwkPaperSize;
    pageInfo.paperWidthIn = pp->page_width;
    pageInfo.paperWidthResName = NhlNwkPaperWidthF;
    pageInfo.paperHeightIn = pp->page_height;
    pageInfo.paperHeightResName = NhlNwkPaperHeightF;

    ret = nhlGetPaperSize(&pageInfo);

    /* unbundle returned values */
    pp->page_width = pageInfo.paperWidthIn;
    pp->page_height = pageInfo.paperHeightIn;
    pp->lower_x = (pp->lower_x < 0) ? pageInfo.leftMargin : pp->lower_x;
    pp->upper_x = (pp->upper_x < 0) ? pageInfo.rightMargin: pp->upper_x;
    pp->lower_y = (pp->lower_y < 0) ? pageInfo.bottomMargin : pp->lower_y;
    pp->upper_y = (pp->upper_y < 0) ? pageInfo.topMargin : pp->upper_y;


	c_ngsetc("me",pp->filename);
    c_ngseti("sw", pageInfo.pageWidthPts);
    c_ngseti("sh", pageInfo.pageHeightPts);
	c_ngseti("co",pp->resolution/72);
	c_ngseti("cm",pp->color_model);

	if (pp->suppress_background && pp->suppress_bbinfo)
		su = 1;
	else if (pp->suppress_background)
		su = 2;
	else if (pp->suppress_bbinfo)
		su = 3;

	c_ngseti("su",su);

	ret = (*NhlworkstationClassRec.work_class.open_work)(l);

	c_ngseti("wo",_NhlWorkstationId(l));
	c_ngseti("lx",pp->lower_x);
	c_ngseti("ux",pp->upper_x);
	c_ngseti("ly",pp->lower_y);
	c_ngseti("uy",pp->upper_y);
	c_ngseti("pl",pp->orientation);
	c_ngseti("fu",pp->full_background);

	w = pp->upper_x - pp->lower_x;
	h = pp->upper_y - pp->lower_y;
	d = MAX(w,h);
	work->work.vswidth_dev_units = (d/72)*pp->resolution;
	pp->bbox.set = 0;
	return ret;
}

/*
 * Function:	PSWorkstationActivate
 *
 * Description:
 *
 * In Args:
 *		NhlLayer	l
 *
 * Out Args:
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:
 */
static NhlErrorTypes
PSWorkstationActivate
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
	NhlWorkstationLayerPart *wp = &((NhlWorkstationLayer)l)->work;
	NhlPSWorkstationLayerPart *pp = &((NhlPSWorkstationLayer)l)->ps;
	int w,h,d;

	if (wp->cleared && pp->dev_bounds_updated) {
		c_ngseti("wo",_NhlWorkstationId(l));
		c_ngseti("lx",pp->lower_x);
		c_ngseti("ux",pp->upper_x);
		c_ngseti("ly",pp->lower_y);
		c_ngseti("uy",pp->upper_y);
		c_ngseti("pl",pp->orientation);
		pp->dev_bounds_updated = False;
	}
	w = pp->upper_x - pp->lower_x;
	h = pp->upper_y - pp->lower_y;
	d = MAX(w,h);
	wp->vswidth_dev_units = (d/72)*pp->resolution;

	return (*(lc->work_class.activate_work))(l);
}

/*
 * Function:	PSWorkstationClear
 *
 * Description:	This function is used to clear the workstation
 *
 * In Args:
 *		NhlLayer	l	workstation layer to update
 *
 * Out Args:
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:
 */
static NhlErrorTypes
PSWorkstationClear
#if	NhlNeedProto
(
	NhlLayer	l	/* workstation layer to update	*/
)
#else
(l)
	NhlLayer	l;	/* workstation layer to update	*/
#endif
{
	NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
	NhlPSWorkstationLayerPart *pp = &((NhlPSWorkstationLayer)l)->ps;
	int w,h;
	int bblx,bbux,bbly,bbuy;
	float s,xoff, yoff;


	w = pp->upper_x - pp->lower_x;
	h = pp->upper_y - pp->lower_y;

	if (w > h) {
		xoff = pp->lower_x + (w - h) / 2.0;
		yoff = pp->lower_y;
	}
	else {
		xoff = pp->lower_x;
		yoff = pp->lower_y + (w - h) / 2.0;
	}
	xoff = pp->lower_x;
	yoff = pp->lower_y;
	if (pp->orientation == NhlPORTRAIT) {
		s = MIN(w,h);
		bblx = xoff + s * pp->bbox.l;
		bbly = yoff + s * pp->bbox.b;

		s = MAX(w,h);
		bbux = xoff + s * pp->bbox.r;
		if (xoff + s * pp->bbox.r > bbux)
			bbux++;
		bbuy = yoff + s * pp->bbox.t;
		if (yoff + s * pp->bbox.t > bbuy)
			bbuy++;
	}
	else {
		s = MIN(w,h);
		bblx = xoff + s * pp->bbox.b - 1;
		bbly = yoff + s * (1.0 - pp->bbox.r) -1;

		s = MAX(w,h);
		bbux = xoff + s * pp->bbox.t;
		if (xoff + s * pp->bbox.t > bbux)
			bbux++;
		bbuy = yoff + s * (1.0 - pp->bbox.l);
		if (yoff + s * (1.0 - pp->bbox.l) > bbuy)
			bbuy++;
	}

	pp->bbox.set = 0;

	if (pp->format == NhlEPS || pp->format == NhlEPSI) {
		c_ngseti("wo",_NhlWorkstationId(l));
		c_ngseti("AX",bblx);
		c_ngseti("BX",bbux);
		c_ngseti("AY",bbly);
		c_ngseti("BY",bbuy);
	}

	return (*(lc->work_class.clear_work))(l);
}


/*
 * Function:	PSUpdateDrawBB
 *
 * Description:
 *
 * In Args:
 *		NhlLayer	wl
 *
 * Out Args:
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:
 */
NhlErrorTypes PSUpdateDrawBB
#if	NhlNeedProto
(
	NhlLayer	wl,
	NhlBoundingBox *bbox
)
#else
(wl)
	NhlLayer	wl,
	NhlBoundingBox *bbox
#endif
{
	NhlPSWorkstationLayerPart *pp = &((NhlPSWorkstationLayer)wl)->ps;

	if (! pp->bbox.set) {
		pp->bbox.l = bbox->l;
		pp->bbox.r = bbox->r;
		pp->bbox.b = bbox->b;
		pp->bbox.t = bbox->t;
		pp->bbox.set = 1;
	}
	else {
		if (bbox->l < pp->bbox.l)
			pp->bbox.l = bbox->l;
		if (bbox->r > pp->bbox.r)
			pp->bbox.r = bbox->r;
		if (bbox->b < pp->bbox.b)
			pp->bbox.b = bbox->b;
		if (bbox->t > pp->bbox.t)
			pp->bbox.t = bbox->t;
	}
	return NhlNOERROR;
}

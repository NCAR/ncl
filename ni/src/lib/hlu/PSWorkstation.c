/*
 *      $Id: PSWorkstation.c,v 1.3 1995-04-07 10:43:17 boote Exp $
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

#define Oset(field)	NhlOffset(NhlPSWorkstationLayerRec,ps.field)
static NhlResource resources[] = {
/* Begin-documented-resources */

	{NhlNwkPSFormat,NhlCwkPSFormat,NhlTPSFormat,sizeof(NhlPSFormat),
		Oset(format),NhlTImmediate,(NhlPointer)NhlPS,
		_NhlRES_NOSACCESS,NULL},
	{NhlNwkVisualType,NhlCwkVisualType,NhlTVisualType,sizeof(NhlVisualType),
		Oset(visual),NhlTImmediate,(NhlPointer)NhlCOLOR,
		_NhlRES_NOSACCESS,NULL},
	{NhlNwkOrientation,NhlCwkOrientation,NhlTWorkOrientation,
		sizeof(NhlWorkOrientation),Oset(orientation),NhlTImmediate,
		(NhlPointer)NhlCOLOR,_NhlRES_NOSACCESS,NULL},
	{NhlNwkPSFileName,NhlCwkPSFileName,NhlTString,
		sizeof(NhlString),Oset(filename),NhlTImmediate,
		(NhlPointer)NULL,_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
	{NhlNwkPSResolution,NhlCwkPSResolution,NhlTInteger,
		sizeof(int),Oset(resolution),NhlTImmediate,
		(NhlPointer)1800,_NhlRES_NOSACCESS,NULL},
	{NhlNwkDeviceLowerX,NhlCwkDeviceLowerX,NhlTInteger,
		sizeof(int),Oset(lower_x),NhlTImmediate,
		(NhlPointer)36,_NhlRES_NOSACCESS,NULL},
	{NhlNwkDeviceLowerY,NhlCwkDeviceLowerY,NhlTInteger,
		sizeof(int),Oset(lower_y),NhlTImmediate,
		(NhlPointer)126,_NhlRES_NOSACCESS,NULL},
	{NhlNwkDeviceUpperX,NhlCwkDeviceUpperX,NhlTInteger,
		sizeof(int),Oset(upper_x),NhlTImmediate,
		(NhlPointer)576,_NhlRES_NOSACCESS,NULL},
	{NhlNwkDeviceUpperY,NhlCwkDeviceUpperY,NhlTInteger,
		sizeof(int),Oset(upper_y),NhlTImmediate,
		(NhlPointer)666,_NhlRES_NOSACCESS,NULL},
	{NhlNwkFullBackground,NhlCwkFullBackground,NhlTBoolean,
		sizeof(NhlBoolean),Oset(full_background),NhlTImmediate,
		(NhlPointer)False,_NhlRES_DEFAULT,NULL},

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

static int	NumCurrentPS = 0;

NhlPSWorkstationClassRec NhlpsWorkstationClassRec = {
        {
/* class_name			*/	"psWorkstationClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlPSWorkstationLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlworkstationClassRec,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	PSWorkstationClassPartInitialize,
/* class_initialize		*/	PSWorkstationClassInitialize,
/* layer_initialize		*/	PSWorkstationInitialize,
/* layer_set_values		*/	PSWorkstationSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
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
/* open_work		*/	PSWorkstationOpen,
/* close_work		*/	NhlInheritClose,
/* activate_work	*/	NhlInheritActivate,
/* deactivate_work	*/	NhlInheritDeactivate,
/* update_work		*/	NhlInheritUpdate,
/* clear_work		*/	NhlInheritClear,
/* lineto_work		*/	NhlInheritLineTo,
/* fill_work		*/	NhlInheritFill,
/* marker_work		*/	NhlInheritMarker
	},
	{
/* num_current	*/		&NumCurrentPS
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
	NhlPSWorkstationClass	psc = (NhlPSWorkstationClass)lc;
	NhlPSWorkstationClass	pssc;

	if(psc->base_class.superclass==(NhlClass)NhlworkstationClass)
		return NhlNOERROR;

	pssc = (NhlPSWorkstationClass)psc->base_class.superclass;
	psc->ps_class.num_current = pssc->ps_class.num_current;

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
		{NhlCOLOR,	"color"},
		{NhlMONOCHROME,	"monochrome"}
	};
	_NhlEnumVals	fmtvals[] = {
		{NhlPS,		"ps"},
		{NhlEPS,	"eps"},
		{NhlEPSI,	"epsi"}
	};
	_NhlEnumVals	orientvals[] = {
		{NhlPORTRAIT,	"portrait"},
		{NhlLANDSCAPE,	"landscape"}
	};

	(void)_NhlRegisterEnumType(NhlTVisualType,visvals,NhlNumber(visvals));
	(void)_NhlRegisterEnumType(NhlTPSFormat,fmtvals,NhlNumber(fmtvals));
	(void)_NhlRegisterEnumType(NhlTWorkOrientation,orientvals,
							NhlNumber(orientvals));

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
	NhlPSWorkstationClassPart	*pscp =
				&((NhlPSWorkstationClass)lclass)->ps_class;
	NhlPSWorkstationLayer		wnew = (NhlPSWorkstationLayer)new;
	NhlPSWorkstationLayerPart	*np = &wnew->ps;
	char				*tfname = NULL;
	char				buff[_NhlMAXFNAMELEN];
	NhlErrorTypes			ret = NhlNOERROR;

	if(*pscp->num_current >= MAX_OPEN_PS){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:Can only have %d %s objects at a time",
			func,MAX_OPEN_PS,lclass->base_class.class_name);
		return NhlFATAL;
	}

	(*pscp->num_current)++;

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
	np->filename = NhlMalloc(strlen(tfname)+1);
	if(!np->filename){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(np->filename,tfname);

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
	NhlPSWorkstationLayerPart	*np = &((NhlPSWorkstationLayer)new)->ps;
	NhlPSWorkstationLayerPart	*op = &((NhlPSWorkstationLayer)old)->ps;

	if(np->full_background != op->full_background){
		c_ngseti("wo",_NhlWorkstationId(new));
		c_ngseti("fu",np->full_background);
	}

	return NhlNOERROR;
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
	NhlPSWorkstationClassPart	*pscp =
		&((NhlPSWorkstationClass)l->base.layer_class)->ps_class;

	NhlFree(psp->filename);
	*(pscp->num_current)--;

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
	NhlPSWorkstationLayerPart	*pp = &((NhlPSWorkstationLayer)l)->ps;
	NhlErrorTypes			ret;

	c_ngsetc("me",pp->filename);
	c_ngseti("co",(pp->resolution/72 + 1));
	c_ngseti("lx",pp->lower_x);
	c_ngseti("ux",pp->upper_x);
	c_ngseti("ly",pp->lower_y);
	c_ngseti("uy",pp->upper_y);

	ret = (*NhlworkstationClassRec.work_class.open_work)(l);

	c_ngseti("wo",_NhlWorkstationId(l));
	c_ngseti("fu",pp->full_background);

	return ret;
}

/*
 *      $Id: App.c,v 1.9 1995-02-17 10:22:52 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		App.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 29 12:38:53 MDT 1994
 *
 *	Description:	
 */
#include <ncarg/hlu/AppP.h>
#include <ncarg/hlu/ConvertersP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/ErrorI.h>
#include <ncarg/hlu/Workspace.h>

static NhlErrorTypes AppLayerClassPartInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc
#endif
);

static NhlErrorTypes AppClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes AppInitialize(
#if	NhlNeedProto
	NhlLayerClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes AppSetValues(
#if	NhlNeedProto
	NhlLayer	old,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes AppGetValues(
#if	NhlNeedProto
	NhlLayer	l,
	_NhlArgList	args,
	int		nargs
#endif
);

static NhlErrorTypes AppLayerDestroy(
#if	NhlNeedProto
	NhlLayer	l
#endif
);

/*
 * Function:	GetSysAppDir
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
GetSysAppDir
#if	NhlNeedProto
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
	char		func[] = "GetSysAppDir";
	NhlAppLayer	app = (NhlAppLayer)base;
	Const char	*sysappres = _NGGetNCARGEnv("sysappres");

	if(!sysappres){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:Unable to get a default value for the system App Resource directory",func);
		return NhlFATAL;
	}

	app->app.sys_appdir = (NhlString)sysappres;

	return NhlNOERROR;
}

/* Resources */
#define	Oset(field)	NhlOffset(NhlAppLayerRec,app.field)
static NhlResource resources[] = {
/* Begin-documented-resources */
	{NhlNappUsrDir,NhlCappUsrDir,NhlTString,sizeof(NhlString),
		Oset(usr_appdir),NhlTImmediate,(NhlPointer)"./",0,
							(NhlFreeFunc)NhlFree},
	{NhlNappSysDir,NhlCappSysDir,NhlTString,sizeof(NhlString),
		Oset(sys_appdir),NhlTProcedure,(NhlPointer)GetSysAppDir,0,
							(NhlFreeFunc)NhlFree},
	{NhlNappFileSuffix,NhlCappFileSuffix,NhlTString,sizeof(NhlString),
		Oset(file_suffix),NhlTImmediate,(NhlPointer)".res",0,
							(NhlFreeFunc)NhlFree},
	{NhlNappDefaultParent,NhlCappDefaultParent,NhlTBoolean,
		sizeof(NhlBoolean),
		Oset(default_parent),NhlTImmediate,(NhlPointer)False,0,NULL},
/* End-documented-resources */
	{_NhlNappMode,_NhlCappMode,NhlTInteger,sizeof(_NhlC_OR_F),
		Oset(init_mode),NhlTImmediate,(NhlPointer)_NhlNONE,0,NULL},
	{_NhlNdefApp,_NhlCdefApp,NhlTBoolean,sizeof(NhlBoolean),
		Oset(default_app),NhlTImmediate,(NhlPointer)False,0,NULL},
	{_NhlNnoAppDB,_NhlCnoAppDB,NhlTBoolean,sizeof(NhlBoolean),
		Oset(no_appDB),NhlTImmediate,(NhlPointer)False,0,NULL}
};
#undef Oset

NhlAppLayerClassRec NhlappLayerClassRec = {
	{
/* class_name			*/	"appLayerClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlAppLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlLayerClass)&NhlbaseLayerClassRec,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,

/* class_part_initialize	*/	AppLayerClassPartInitialize,
/* class_initialize		*/	AppClassInitialize,
/* layer_initialize		*/	AppInitialize,
/* layer_set_values		*/	AppSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	AppGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	AppLayerDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	{
/* default_app			*/	NULL,
/* current_app			*/	NULL,
/* baseDB			*/	NULL,
/* error_id			*/	0,
/* workspace_id			*/	0,
/* app_objs			*/	NULL
	}
};

NhlLayerClass NhlappLayerClass = (NhlLayerClass)&NhlappLayerClassRec;

/*
 * Function:	nhlfappclass
 *
 * Description:	fortran ref to this class
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NhlLayerClass
_NHLCALLF(nhlfapplayerclass,NHLFAPPLAYERCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlappLayerClass;
}

/*
 * Function:	InitBaseDB
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
InitBaseDB
#if	NhlNeedProto
(
	NhlAppLayerClass	alc
)
#else
(alc)
	NhlAppLayerClass	alc;
#endif
{
	Const char		*sysfile=NULL;
	Const char		*usrfile=NULL;
	NhlErrorTypes		ret = NhlNOERROR;

	sysfile = _NGGetNCARGEnv("sysresfile");
	usrfile = _NGGetNCARGEnv("usrresfile");

	if((void *)sysfile == (void *)NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Unable to Get System Resource File Name?");
		ret = MIN(ret,NhlWARNING);
	}
	else
		alc->app_class.baseDB = NrmGetFileDB(sysfile);
	
	if((void *)alc->app_class.baseDB == (void *)NULL){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			"Unable to load System Resource File %s",sysfile);
		ret = MIN(ret,NhlWARNING);
	}

	if((void *)usrfile == (void *)NULL){
		NhlPError(NhlINFO,NhlEUNKNOWN,
				"Unable to Get User Resource File Name?");
		ret = MIN(ret,NhlINFO);
	}
	else
		NrmCombineFileDB(usrfile,&alc->app_class.baseDB,True);

	/*
	 * Make sure the baseDB has at least one resource...
	 */
	NrmPutStringRes(&alc->app_class.baseDB,"DBLoaded","True");

	return ret;
}

/*
 * Function:	AppLayerClassPartInitialize
 *
 * Description:	This function is called to initialize the base_class
 *		part of every layer class record.  It basically initializes
 *		the resources list of each class to include the resources
 *		of it's super classes and also converts the entire list
 *		into a quarked list so strcmp's don't have to occur in
 *		the rest of the library.
 *
 * In Args:	
 *		NhlLayerClass	lc	pointer to class structure to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
AppLayerClassPartInitialize
#if	NhlNeedProto
(
	NhlLayerClass	lc	/* pointer to class structure to update	*/
)
#else
(lc)
	NhlLayerClass	lc;	/* pointer to class structure to update	*/
#endif
{
	NhlAppLayerClass	alc = (NhlAppLayerClass)lc;
	NhlAppLayerClassPart	*alcp = &alc->app_class;

	alcp->default_app = NULL;
	alcp->current_app = NULL;
	InitBaseDB(alc);

	return NhlNOERROR;
}

static NrmQuark	usrdirQ = NrmNULLQUARK;
static NrmQuark	sysdirQ = NrmNULLQUARK;
static NrmQuark	filesuffQ = NrmNULLQUARK;
static NrmQuark	defparQ = NrmNULLQUARK;
static _NhlC_OR_F lang_type = _NhlNONE;

/*
 * Function:	AppClassInitialize
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
AppClassInitialize
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NrmInitialize();
	_NhlConvertersInitialize();
	_NhlResourceListInitialize();

	usrdirQ = NrmStringToQuark(NhlNappUsrDir);
	sysdirQ = NrmStringToQuark(NhlNappSysDir);
	filesuffQ = NrmStringToQuark(NhlNappFileSuffix);
	defparQ = NrmStringToQuark(NhlNappDefaultParent);

	return NhlNOERROR;
}

/*
 * Function:	AppInitialize
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
/*ARGSUSED*/
static NhlErrorTypes
AppInitialize
#if	NhlNeedProto
(
	NhlLayerClass		lc,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList		args,
	int			nargs
)
#else
(lc,req,new,args,nargs)
	NhlLayerClass		lc;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList		args;
	int			nargs;
#endif
{
	char			func[] = "AppInitialize";
	NhlErrorTypes		ret = NhlNOERROR,lret = NhlNOERROR;
	NhlAppLayer		anew = (NhlAppLayer)new;
	NhlAppLayerClass	ac = (NhlAppLayerClass)anew->base.layer_class;
	Const char		*cs = NULL;
	char			tname[_NhlMAXFNAMELEN];


	if(anew->app.default_app){
		if(ac->app_class.default_app){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:There is already a \"default\" App Object!",func);
			return NhlFATAL;
		}
		ac->app_class.default_app = anew;
	}

	if(anew->app.default_parent || !ac->app_class.current_app){
		ac->app_class.current_app = anew;
		anew->app.default_parent = False;
	}

	if(!ac->app_class.default_app){
		int	tint;

		if(lang_type == _NhlNONE){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:HLU library must be initialized before an App Object can be created!",func);
			return NhlFATAL;
		}

		lret = NhlVACreate(&tint,anew->base.name,NhlappLayerClass,0,
				_NhlNappMode,	lang_type,
				_NhlNdefApp,	True,
				NULL);
		if(lret < NhlWARNING)
			return lret;
		ret = MIN(ret,lret);
	}

	if(anew->app.no_appDB){
		anew->app.usr_appdir = NULL;
		anew->app.sys_appdir = NULL;
		anew->app.file_suffix = NULL;
		anew->app.appDB = NULL;
	}
	else{
		cs = anew->app.file_suffix;
		anew->app.file_suffix = NhlMalloc((strlen(cs)+1)*sizeof(char));
		if(!anew->app.file_suffix){
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return NhlFATAL;
		}
		strcpy(anew->app.file_suffix,cs);

		cs = _NGGetNCARGEnv("sysresfile");
		if((void *)cs == (void *)NULL){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"Unable to Get System Resource File Name?");
			ret = MIN(ret,NhlWARNING);
		}
		else
			anew->app.appDB = NrmGetFileDB(cs);
	

		cs = _NGResolvePath(anew->app.sys_appdir);
		if(cs){
			anew->app.sys_appdir =
					NhlMalloc((strlen(cs)+1)*sizeof(char));
			if(!anew->app.sys_appdir){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			strcpy(anew->app.sys_appdir,cs);
			strcpy(tname,anew->app.sys_appdir);
			strcat(tname,_NhlPATHDELIMITER);
			strcat(tname,anew->base.name);
			strcat(tname,anew->app.file_suffix);
			NrmCombineFileDB(tname,&anew->app.appDB,True);
		}

		cs = _NGGetNCARGEnv("usrresfile");
		if((void *)cs == (void *)NULL){
			NhlPError(NhlINFO,NhlEUNKNOWN,
				"Unable to Get User Resource File Name?");
			ret = MIN(ret,NhlINFO);
		}
		else
			NrmCombineFileDB(cs,&anew->app.appDB,True);

		cs = _NGResolvePath(anew->app.usr_appdir);
		if(cs){
			anew->app.usr_appdir =
					NhlMalloc((strlen(cs)+1)*sizeof(char));
			if(!anew->app.usr_appdir){
				NhlPError(NhlFATAL,ENOMEM,NULL);
				return NhlFATAL;
			}
			strcpy(anew->app.usr_appdir,cs);
			strcpy(tname,anew->app.usr_appdir);
			strcat(tname,_NhlPATHDELIMITER);
			strcat(tname,anew->base.name);
			strcat(tname,anew->app.file_suffix);
			NrmCombineFileDB(tname,&anew->app.appDB,True);
		}
	}

	/*
	 * insure error reporting is available.
	 */
	if(ac->app_class.error_id < 1){
		ret = NhlVACreate(&ac->app_class.error_id,"error",
				NhlerrorLayerClass,new->base.id,
				_NhlNerrMode,	anew->app.init_mode,
				NULL);
		if(ret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Error Creating ErrorClass object");
			return NhlFATAL;
		}
	}

	/*
	 * insure Workspace is available.
	 */
	if(ac->app_class.workspace_id < 1){
		lret = NhlVACreate(&ac->app_class.workspace_id,"workspace",
					NhlworkspaceLayerClass,new->base.id,
				NULL);
		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Error Creating Workspace object");
			return NhlFATAL;
		}
	}
	ret = MIN(ret,lret);

	if(ac->app_class.default_app != anew){
		NhlAppTable	at = NhlMalloc(sizeof(NhlAppTableRec));

		if(!at){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		at->app = anew;
		at->next = ac->app_class.app_objs;
		ac->app_class.app_objs = at;
	}

	return ret;
}

/*
 * Function:	AppSetValues
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
/*ARGSUSED*/
static NhlErrorTypes
AppSetValues
#if	NhlNeedProto
(
	NhlLayer	old,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
#else
(old,req,new,args,nargs)
	NhlLayer	old;
	NhlLayer	req;
	NhlLayer	new;
	_NhlArgList	args;
	int		nargs;
#endif
{
	char			func[] = "AppSetValues";
	NhlAppLayer		newapp = (NhlAppLayer)new;
	NhlAppLayerPart		*np = &newapp->app;
	NhlAppLayer		oldapp = (NhlAppLayer)old;
	NhlAppLayerPart		*op = &oldapp->app;
	NhlAppLayerClass	ac = (NhlAppLayerClass)new->base.layer_class;
	NhlErrorTypes		ret = NhlNOERROR;

	if(np->usr_appdir != op->usr_appdir){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s is only settable at Create time.",func,NhlNappUsrDir);
		np->usr_appdir = op->usr_appdir;
		ret = NhlWARNING;
	}
	if(np->sys_appdir != op->sys_appdir){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s is only settable at Create time.",func,NhlNappSysDir);
		np->sys_appdir = op->sys_appdir;
		ret = NhlWARNING;
	}
	if(np->file_suffix != op->file_suffix){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s is only settable at Create time.",func,NhlNappFileSuffix);
		np->file_suffix = op->file_suffix;
		ret = NhlWARNING;
	}
	if(np->init_mode != op->init_mode){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s is only settable at Create time.",func,_NhlNappMode);
		np->init_mode = op->init_mode;
		ret = NhlWARNING;
	}
	if(np->default_app != op->default_app){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
		"%s:%s is only settable at Create time.",func,_NhlNdefApp);
		np->default_app = op->default_app;
		ret = NhlWARNING;
	}

	if(np->default_parent){
		ac->app_class.current_app = newapp;
		np->default_parent = False;
	}

	return ret;
}

/*
 * Function:	AppGetValues
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
/*ARGSUSED*/
static NhlErrorTypes
AppGetValues
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
	char			func[] = "AppGetValues";
	NhlAppLayer		al = (NhlAppLayer)l;
	NhlAppLayerPart		*alp = &al->app;
	NhlAppLayerClass	alc = (NhlAppLayerClass)al->base.layer_class;
	int			i;
	NhlErrorTypes		ret = NhlNOERROR;
	NhlString		tstring;

	for(i=0;i<nargs;i++){

		tstring = NULL;
		if(args[i].quark == usrdirQ)
			tstring = alp->usr_appdir;
		else if(args[i].quark == sysdirQ)
			tstring = alp->sys_appdir;
		else if(args[i].quark == filesuffQ)
			tstring = alp->file_suffix;
		else if(args[i].quark == defparQ){
			*(NhlBoolean*)args[i].value.ptrval =
			(al->base.id == alc->app_class.current_app->base.id);
		}

		if(tstring){
			*(NhlString*)args[i].value.ptrval =
				NhlMalloc((strlen(tstring)+1)*sizeof(char));
			if(!*(NhlString*)args[i].value.ptrval){
				NhlPError(NhlWARNING,ENOMEM,"%s:Retrieving %s",
					func,NrmQuarkToString(args[i].quark));
				ret = MIN(ret,NhlWARNING);
				continue;
			}
			strcpy(*(NhlString*)args[i].value.ptrval,tstring);
		}
	}

	return ret;
}

/*
 * Function:	AppLayerDestroy
 *
 * Description:	This function is used to clean up any memory that has
 *		been allocated in the base part of the layer. It is called
 *		from the NhlDestroy method.
 *
 * In Args:	
 *		NhlLayer	l	layer to destroy
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
AppLayerDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	char			func[] = "AppLayerDestroy";
	NhlAppLayer		al = (NhlAppLayer)l;
	NhlAppLayerPart		*alp = &al->app;
	NhlAppLayerClass	alc = (NhlAppLayerClass)al->base.layer_class;
	NhlAppLayerClassPart	*alcp = &alc->app_class;
	NhlErrorTypes		ret = NhlNOERROR;

	NhlFree(alp->usr_appdir);
	NhlFree(alp->sys_appdir);
	NhlFree(alp->file_suffix);
	NrmDestroyDB(alp->appDB);

	/*
	 * If this is the "default_app", then destroy *ALL* app objects
	 * that have been created, and therefore *ALL* objects.
	 * This is how the NhlClose function has been implimented.
	 */
	if(al == alcp->default_app){
		while(alcp->app_objs)
			NhlDestroy(alcp->app_objs->app->base.id);
		NrmDestroyDB(alcp->baseDB);
		alcp->baseDB = NULL;
		alcp->default_app = NULL;
		alcp->current_app = NULL;
		NhlDestroy(alcp->error_id);
		NhlDestroy(alcp->workspace_id);
		alcp->error_id = 0;
		alcp->workspace_id = 0;
		_NhlDestroyRLList();
	}
	/*
	 * Otherwise, remove this layer from the app_objs list.
	 */
	else{
		NhlAppTable	*tblptr = &alcp->app_objs;
		NhlAppTable	tnode = NULL;
		NhlBoolean	found = False;

		while(*tblptr){
			if((*tblptr)->app == al){
				found = True;
				tnode = *tblptr;
				*tblptr = (*tblptr)->next;
				NhlFree(tnode);
				break;
			}
			tblptr = &(*tblptr)->next;
		}

		if(!found){
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"%s:Unable to remove %s from App list",func,
				l->base.name);
			ret = NhlWARNING;
		}
	}

	if(al == alcp->current_app)
		alcp->current_app = alcp->default_app;

	return ret;
}

/*
 * Private API
 */

/*
 * Function:	_NhlGetCurrentApp
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
NhlLayer
_NhlGetCurrentApp
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return (NhlLayer)NhlappLayerClassRec.app_class.current_app;
}

/*
 * Function:	_NhlGetBaseDB
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
NrmDatabase
_NhlGetResDB
#if	NhlNeedProto
(
	NhlLayer	l
)
#else
(l)
	NhlLayer	l;
#endif
{
	NhlAppLayer		al = (NhlAppLayer)l->base.appobj;
	NhlAppLayerClass	alc = (NhlAppLayerClass)al->base.layer_class;

	if(((NhlLayer)al == l) || !al->app.appDB)
		return alc->app_class.baseDB;

	return al->app.appDB;
}

/*
 * Function:	_NhlSetLang
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
void
_NhlSetLang
#if	NhlNeedProto
(
	_NhlC_OR_F	ltype
)
#else
(ltype)
	_NhlC_OR_F	ltype;
#endif
{
	lang_type = ltype;

	return;
}

/*
 * Public API
 */

/*
 * Function:	NhlAppGetDefaultParentID
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
int
NhlAppGetDefaultParentID
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	if((NhlappLayerClassRec.app_class.current_app) &&
			(NhlappLayerClassRec.app_class.current_app !=
				NhlappLayerClassRec.app_class.default_app))

		return NhlappLayerClassRec.app_class.current_app->base.id;

	return (int)NhlFATAL;
}

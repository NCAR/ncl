/*
 *      $Id: App.c,v 1.41 2007-03-12 18:16:22 dbrown Exp $
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
#include <ncarg/hlu/ResListP.h>

static _NhlRawClassCB appc_callbacks[] = {
	{_NhlCBappDefParentChange,NULL,0,NULL,NULL,NULL},
};

static NhlErrorTypes AppClassPartInitialize(
#if	NhlNeedProto
	NhlClass	lc
#endif
);

static NhlErrorTypes AppClassInitialize(
#if	NhlNeedProto
	void
#endif
);

static NhlErrorTypes AppInitialize(
#if	NhlNeedProto
	NhlClass	lc,
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

static NhlErrorTypes AppDestroy(
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

#define appDefResDef	{NhlNappDefaultParent,NhlCappDefaultParent,	\
	NhlTBoolean,sizeof(NhlBoolean),Oset(default_parent),		\
	NhlTImmediate,_NhlUSET((NhlPointer)False),_NhlRES_DEFAULT,NULL}
#define appResResDef {NhlNappResources,NhlCappResources,		\
	NhlTStringGenArray,sizeof(NhlGenArray),Oset(resources),		\
	NhlTImmediate,_NhlUSET((NhlPointer)NULL),_NhlRES_NOSACCESS,		\
	(NhlFreeFunc)NhlFreeGenArray}

static NhlResource more_res[] = {
	appDefResDef,
	appResResDef
};

static NhlResource resources[] = {
/* Begin-documented-resources */
	{NhlNappUsrDir,NhlCappUsrDir,NhlTString,sizeof(NhlString),
		Oset(usr_appdir),NhlTImmediate,_NhlUSET((NhlPointer)"./"),
		_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
	{NhlNappSysDir,NhlCappSysDir,NhlTString,sizeof(NhlString),
		Oset(sys_appdir),NhlTProcedure,_NhlUSET((NhlPointer)GetSysAppDir),
		_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
	{NhlNappFileSuffix,NhlCappFileSuffix,NhlTString,sizeof(NhlString),
		Oset(file_suffix),NhlTImmediate,_NhlUSET((NhlPointer)".res"),
		_NhlRES_NOSACCESS,(NhlFreeFunc)NhlFree},
	appDefResDef,
	appResResDef,
/* End-documented-resources */
	{_NhlNappMode,_NhlCappMode,NhlTInteger,sizeof(_NhlC_OR_F),
		Oset(init_mode),NhlTImmediate,_NhlUSET((NhlPointer)_NhlNONE),
		_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNdefApp,_NhlCdefApp,NhlTBoolean,sizeof(NhlBoolean),
		Oset(default_app),NhlTImmediate,_NhlUSET((NhlPointer)False),
		_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNnoAppDB,_NhlCnoAppDB,NhlTBoolean,sizeof(NhlBoolean),
		Oset(no_appDB),NhlTImmediate,_NhlUSET((NhlPointer)False),
         	_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNappResourceStrings,_NhlCappResourceStrings,NhlTPointer,
		sizeof(NhlPointer),Oset(res_strings),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNappCommandLineOpts,_NhlCappCommandLineOpts,NhlTPointer,
		sizeof(NhlPointer),Oset(clineopts),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNappArgcInOut,_NhlCappArgcInOut,NhlTPointer,
		sizeof(NhlPointer),Oset(argc_in_out),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{_NhlNappArgvInOut,_NhlCappArgvInOut,NhlTPointer,
		sizeof(NhlPointer),Oset(argv_in_out),NhlTImmediate,
		_NhlUSET((NhlPointer)NULL),_NhlRES_CONLY|_NhlRES_PRIVATE,NULL},
	{NhlNobjAppObj,NhlCobjAppObj,NhlTInteger,sizeof(int),
		NhlOffset(NhlBaseLayerRec,base.appid),NhlTImmediate,
		_NhlUSET((NhlPointer)NhlDEFAULT_APP),
         	_NhlRES_NOACCESS|_NhlRES_PRIVATE,NULL},
};

#undef	appDefResDef
#undef	appResResDef
#undef	Oset

NhlAppClassRec NhlappClassRec = {
	{
/* class_name			*/	"appClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NhlAppLayerRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlbaseClassRec,
/* cvt_table			*/	NULL,

/* resources			*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	NULL,
/* num_callbacks		*/	0,
/* class_callbacks		*/	appc_callbacks,
/* num_class_callbacks		*/	NhlNumber(appc_callbacks),

/* class_part_initialize	*/	AppClassPartInitialize,
/* class_initialize		*/	AppClassInitialize,
/* layer_initialize		*/	AppInitialize,
/* layer_set_values		*/	AppSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	AppGetValues,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	AppDestroy,

/* child_resources		*/	NULL,

/* layer_draw			*/	NULL,

/* layer_pre_draw		*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL
	},
	{
/* resources			*/	more_res,
/* num_resources		*/	NhlNumber(more_res),
/* default_app			*/	NULL,
/* current_app			*/	NULL,
/* baseDB			*/	NULL,
/* error_id			*/	0,
/* workspace_id			*/	0,
/* app_objs			*/	NULL,
/* default_guidata		*/	NULL
	}
};

NhlClass NhlappClass = (NhlClass)&NhlappClassRec;

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
NhlClass
_NHLCALLF(nhlfappclass,NHLFAPPCLASS)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlappClass;
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
	NhlAppClass	alc
)
#else
(alc)
	NhlAppClass	alc;
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

static NrmQuark	usrdirQ = NrmNULLQUARK;
static NrmQuark	sysdirQ = NrmNULLQUARK;
static NrmQuark	filesuffQ = NrmNULLQUARK;
static NrmQuark	defparQ = NrmNULLQUARK;
static NrmQuark	appresQ = NrmNULLQUARK;
static _NhlC_OR_F lang_type = _NhlNONE;
static NrmResource def_app_res;


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

	_NhlCompileResourceList((NhlClass)&NhlappClassRec,
		NhlappClassRec.app_class.resources,
		NhlappClassRec.app_class.num_resources);

	usrdirQ = NrmStringToQuark(NhlNappUsrDir);
	sysdirQ = NrmStringToQuark(NhlNappSysDir);
	filesuffQ = NrmStringToQuark(NhlNappFileSuffix);
	defparQ = NrmStringToQuark(NhlNappDefaultParent);
	appresQ = NrmStringToQuark(NhlNappResources);

	def_app_res.nrm_name= def_app_res.nrm_class= NrmStringToQuark("no.res");
	def_app_res.nrm_type = def_app_res.nrm_default_type =
						NrmStringToQuark(NhlTString);
	def_app_res.nrm_size = sizeof(NhlString);
	def_app_res.nrm_default_val.strval = NULL;
	def_app_res.res_info = _NhlRES_DEFAULT;
	def_app_res.free_func = (NhlFreeFunc)NhlFree;

	return NhlNOERROR;
}

/*
 * Function:	AppClassPartInitialize
 *
 * Description:	This function is called to initialize the base_class
 *		part of every layer class record.  It basically initializes
 *		the resources list of each class to include the resources
 *		of it's super classes and also converts the entire list
 *		into a quarked list so strcmp's don't have to occur in
 *		the rest of the library.
 *
 * In Args:	
 *		NhlClass	lc	pointer to class structure to update
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
/*ARGSUSED*/
static NhlErrorTypes
AppClassPartInitialize
#if	NhlNeedProto
(
	NhlClass	lc	/* pointer to class structure to update	*/
)
#else
(lc)
	NhlClass	lc;	/* pointer to class structure to update	*/
#endif
{
	NhlAppClass	alc = (NhlAppClass)lc;
	NhlAppClassPart	*alcp = &alc->app_class;

	alcp->default_app = NULL;
	alcp->current_app = NULL;
	InitBaseDB(alc);
	alcp->default_guidata = NULL;

	return NhlNOERROR;
}

static void
DefaultParentChange
#if	NhlNeedProto
(
	NhlAppClass	ac
)
#else
(ac)
	NhlAppClass	ac;
#endif
{
	NhlArgVal cbdata;
	NhlArgVal sel;

	NhlINITVAR(cbdata);
	NhlINITVAR(sel);

	sel.lngval = 0;
	cbdata.lngval = ac->app_class.current_app->base.id;
	_NhlCallClassCallbacks((NhlClass)ac,_NhlCBappDefParentChange,
								sel,cbdata);

	return;
}

static int
CompareRes
#if	NhlNeedProto
(
	Const void	*ov,
	Const void	*tv
)
#else
(ov,tv)
	Const void	*ov;
	Const void	*tv;
#endif
{
	NrmResource	*one = (NrmResource*)ov;
	NrmResource	*two = (NrmResource*)tv;

	if(one->nrm_name < two->nrm_name)
		return -1;
	if(one->nrm_name > two->nrm_name)
		return 1;
	return 0;
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
	NhlClass		lc,
	NhlLayer		req,
	NhlLayer		new,
	_NhlArgList		args,
	int			nargs
)
#else
(lc,req,new,args,nargs)
	NhlClass		lc;
	NhlLayer		req;
	NhlLayer		new;
	_NhlArgList		args;
	int			nargs;
#endif
{
	char			func[] = "AppInitialize";
	NhlErrorTypes		ret = NhlNOERROR,lret = NhlNOERROR;
	NhlAppLayer		anew = (NhlAppLayer)new;
	NhlAppClass		ac = (NhlAppClass)anew->base.layer_class;
	Const char		*cs = NULL;
	char			tname[_NhlMAXFNAMELEN];
	NrmQuark		nameQ[2],classQ[2];
	_NhlArg			largs[_NhlMAXARGLIST];
	int			nlargs = 0;
	int			i;
	int			indx;
	NrmResourceList		rlist;
	_NhlConvertContext	context;
	NrmDatabase		db;

	if(anew->app.default_app){
		if(ac->app_class.default_app){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
			"%s:There is already a \"default\" App Object!",func);
			return NhlFATAL;
		}
		ac->app_class.default_app = anew;
	}

	if(!ac->app_class.default_app){
		int	tint;

		if(lang_type == _NhlNONE){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
"%s:HLU library must be initialized before an App Object can be created!",func);
			return NhlFATAL;
		}

		lret = NhlVACreate(&tint,"hlu",NhlappClass,0,
				_NhlNappMode,	lang_type,
				_NhlNdefApp,	True,
				_NhlNnoAppDB,	True,
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
			if (strlen(tname) + strlen(anew->base.name) 
			    + strlen(anew->app.file_suffix) + 1 > _NhlMAXFNAMELEN) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "App path length is too long");
				return NhlFATAL;
			}
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
			if (strlen(tname) + strlen(anew->base.name) 
			    + strlen(anew->app.file_suffix) + 1 > _NhlMAXFNAMELEN) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "App path length is too long");
				return NhlFATAL;
			}
			strcat(tname,anew->base.name);
			strcat(tname,anew->app.file_suffix);
			NrmCombineFileDB(tname,&anew->app.appDB,True);
		}

		if(anew->app.res_strings){
			NhlString	*rstrings = anew->app.res_strings;

			while(*rstrings)
				NrmPutLineResource(&anew->app.appDB,
								*rstrings++);
		}

		if(anew->app.clineopts && anew->app.argc_in_out &&
			anew->app.argv_in_out && (*anew->app.argc_in_out > 0)){
			NrmParseCommand(&anew->app.appDB,anew->app.clineopts,
				anew->base.name,anew->app.argc_in_out,
				anew->app.argv_in_out);
		}
	}
	anew->app.res_strings = NULL;
	anew->app.clineopts = NULL;
	anew->app.argc_in_out = NULL;
	anew->app.argv_in_out = NULL;

	/*
	 * insure error reporting is available.
	 */
	if(ac->app_class.error_id < 1){
		ret = NhlVACreate(&ac->app_class.error_id,"error",
				NhlerrorClass,new->base.id,
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
					NhlworkspaceClass,new->base.id,
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

	/*
	 * Now, re-retrieve resources that depend upon a complete resdb.
	 */

	nameQ[0] = new->base.nrm_name;
	nameQ[1] = NrmNULLQUARK;
	classQ[0] = ac->base_class.nrm_class;
	classQ[1] = NrmNULLQUARK;

	rlist = (NrmResourceList)ac->app_class.resources;
	context = _NhlCreateConvertContext(new);
	if(anew->app.appDB)
		db = anew->app.appDB;
	else
		db = ac->app_class.baseDB;

	if(!context){
		NhlPError(NhlFATAL,ENOMEM,"%s:No Converter Context.",func);
		return NhlFATAL;
	}

	for(i=0;i < ac->app_class.num_resources;i++){
		indx = _NhlArgIsSet(args,nargs,
					NrmQuarkToString(rlist[i].nrm_name));
		if(!indx)
			continue;

		largs[nlargs++] = args[indx-1];
	}

	lret = _NhlGetResources(context,db,(char*)new,nameQ,classQ,
			(NrmResourceList)ac->app_class.resources,
			ac->app_class.num_resources,largs,nlargs,NULL);
	if(lret < NhlWARNING){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
				"%s:Problems retrieving resources...",func);
		return NhlFATAL;
	}
	ret = MIN(lret,ret);

	if((anew != ac->app_class.default_app) &&
		(!ac->app_class.current_app || anew->app.default_parent)){
		ac->app_class.current_app = anew;
		anew->app.default_parent = True;
	}

	if(anew->app.resources){
		NhlString	*res_names;
		
		anew->app.resources = _NhlCopyGenArray(anew->app.resources,
									True);
		if(!anew->app.resources){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}

		res_names = anew->app.resources->data;
		anew->app.nres = anew->app.resources->num_elements;

		anew->app.values = (NhlString*)NhlMalloc(sizeof(NhlString) *
							anew->app.nres);
		anew->app.res = (NrmResourceList)
			NhlMalloc(sizeof(NrmResource) * anew->app.nres);
		if(!anew->app.values || !anew->app.res){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}

		for(i=0;i < anew->app.nres;i++){
			anew->app.res[i] = def_app_res;
			anew->app.res[i].nrm_name =
						NrmStringToQuark(res_names[i]);
		}
		
		qsort(anew->app.res,
		      anew->app.nres,sizeof(NrmResource),CompareRes);
	
		for(i=0;i < anew->app.nres;i++){
			anew->app.res[i].nrm_offset = sizeof(NhlString)*i;
		}
		lret = _NhlGetResources(context,db,
				(char*)anew->app.values,nameQ,classQ,
				anew->app.res,anew->app.nres,
				anew->app.args,anew->app.nargs,NULL);

		if(lret < NhlWARNING){
			NhlPError(NhlFATAL,NhlEUNKNOWN,
		       "%s:Problems retrieving app defined resources...",func);
			return NhlFATAL;
		}
		ret = MIN(lret,ret);

		for(i=0;i < anew->app.nres;i++){
			NhlString	tstring;

			tstring = anew->app.values[i];

			if(tstring){
				anew->app.values[i] =
						NhlMalloc(strlen(tstring)+1);
				if(!anew->app.values[i]){
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return NhlFATAL;
				}
				strcpy(anew->app.values[i],tstring);
			}
		}
	}
	else{
		anew->app.values = NULL;
		anew->app.res = NULL;
		anew->app.nres = 0;
		if(anew->app.nargs){
			for(i=0;i < anew->app.nargs;i++){
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is not a defined resource of App object \"%s\"",
				func,NrmQuarkToString(anew->app.args[i].quark),
				anew->base.name);
			}
			ret = MIN(NhlWARNING,ret);
		}
	}

	_NhlFreeConvertContext(context);
	NhlFree(anew->app.args);
	anew->app.args = NULL;
	anew->app.nargs = 0;

	if(!_NhlArgIsSet(args,nargs,_NhlNguiData))
		new->base.gui_data = ac->app_class.default_guidata;

	/*
	 * May need to create an initialize_hook for this...
	 */
	if(anew->app.default_parent){
		anew->app.default_parent = False;
		DefaultParentChange((NhlAppClass)anew->base.layer_class);
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
	NhlAppClass		ac = (NhlAppClass)new->base.layer_class;
	int			i;
	_NhlConvertContext	context;
	NhlErrorTypes		ret = NhlNOERROR,lret=NhlNOERROR;

	if(_NhlArgIsSet(args,nargs,NhlNappDefaultParent)){
		if(np->default_parent){
			if(ac->app_class.current_app == newapp){
				np->default_parent = False;
			}
			else{
				ac->app_class.current_app = newapp;
			}
		}
		/*
		 * newapp is currently the default_parent, and the user
		 * doesn't want it to be.
		 */
		else if(newapp == ac->app_class.current_app){
			np->default_parent = True;
			ac->app_class.current_app = ac->app_class.default_app;
		}
	}

	if(np->args){
		if(np->res){
			NhlString	*new_vals;

			context = _NhlCreateConvertContext(new);
			if(!context){
				NhlPError(NhlFATAL,ENOMEM,
					"%s:No Converter Context.",func);
				return NhlFATAL;
			}

			new_vals = NhlMalloc(sizeof(NhlString)*np->nres);
			if(!new_vals){
				NHLPERROR((NhlFATAL,ENOMEM,NULL));
				return NhlFATAL;
			}
			memcpy(new_vals,np->values,sizeof(NhlString)*np->nres);

			lret = _NhlSetValues(context,(char*)new_vals,
					np->res,np->nres,np->args,np->nargs);

			for(i=0;i<np->nres;i++){
				if(np->values[i] != new_vals[i]){
					NhlFree(np->values[i]);
					np->values[i] =
					NhlMalloc(strlen(new_vals[i])+1);
					if(!np->values[i]){
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return NhlFATAL;
					}
					strcpy(np->values[i],new_vals[i]);
				}
			}
			NhlFree(new_vals);
			_NhlFreeConvertContext(context);
		}
		else{
			for(i=0;i<np->nargs;i++)
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is not a defined resource of App object \"%s\"",
				func,NrmQuarkToString(np->args[i].quark),
				new->base.name);
			ret = MIN(NhlWARNING,ret);
		}

		NhlFree(np->args);
		np->args = NULL;
		np->nargs = 0;
	}

	if(np->default_parent){
		np->default_parent = False;
		DefaultParentChange(ac);
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
	NhlAppClass	alc = (NhlAppClass)al->base.layer_class;
	int			i,j;
	NhlErrorTypes		ret = NhlNOERROR,lret=NhlNOERROR;
	NhlString		tstring;
	NhlGenArray		tgen;

	for(i=0;i<nargs;i++){

		tstring = NULL;
		tgen = NULL;

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
		else if(args[i].quark == appresQ){
			tgen = alp->resources;
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
		if(tgen){
			*(NhlGenArray*)args[i].value.ptrval =
						_NhlCopyGenArray(tgen,True);
			if(!*(NhlGenArray*)args[i].value.ptrval){
				NhlPError(NhlWARNING,ENOMEM,"%s:Retrieving %s",
					func,NrmQuarkToString(args[i].quark));
				ret = MIN(ret,NhlWARNING);
				continue;
			}
		}
	}

	if(alp->args){
		if(alp->res){
			lret = _NhlGetValues((char*)alp->values,
				alp->res,alp->nres,alp->args,alp->nargs);
			ret=MIN(lret,ret);

			for(i=0;i<alp->nargs;i++){

				tstring = NULL;

				for(j=0;j<alp->nres;j++){
					if(alp->args[i].quark ==
							alp->res[j].nrm_name){
						tstring=alp->values[j];
						break;
					}
				}
						
				if(tstring){
					*(NhlString*)alp->args[i].value.ptrval =
						NhlMalloc((strlen(tstring)+1)*
								sizeof(char));
					if(!*(NhlString*)
						alp->args[i].value.ptrval){
						NhlPError(NhlWARNING,ENOMEM,
							"%s:Retrieving %s",
							func,
							NrmQuarkToString(
							alp->args[i].quark));
						ret = MIN(ret,NhlWARNING);
						continue;
					}
					strcpy(*(NhlString*)
						alp->args[i].value.ptrval,
								tstring);
				}
			}
		}
		else{
			for(i=0;i<alp->nargs;i++)
				NhlPError(NhlWARNING,NhlEUNKNOWN,
			"%s:%s is not a defined resource of App object \"%s\"",
				func,NrmQuarkToString(alp->args[i].quark),
				al->base.name);
			ret = MIN(NhlWARNING,ret);
		}

		NhlFree(alp->args);
		alp->args = NULL;
		alp->nargs = 0;
	}

	return ret;
}

/*
 * Function:	AppDestroy
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
AppDestroy
#if	NhlNeedProto
(
	NhlLayer	l	/* layer to destroy	*/
)
#else
(l)
	NhlLayer	l;	/* layer to destroy	*/
#endif
{
	char			func[] = "AppDestroy";
	NhlAppLayer		al = (NhlAppLayer)l;
	NhlAppLayerPart		*alp = &al->app;
	NhlAppClass		alc = (NhlAppClass)al->base.layer_class;
	NhlAppClassPart		*alcp = &alc->app_class;
	NhlErrorTypes		lret = NhlNOERROR;
	NhlErrorTypes		ret = NhlNOERROR;
	int			i;

	/*
	 * If this is the "default_app", then destroy *ALL* app objects
	 * that have been created, and therefore *ALL* objects.
	 * This is how the NhlClose function has been implimented.
	 */
	if(al == alcp->default_app){
		_NhlAllChildList	*chl;
		while(alcp->app_objs){
			lret = NhlDestroy(alcp->app_objs->app->base.id);
			ret = MIN(ret,lret);
		}
		alcp->default_app = NULL;
		alcp->current_app = NULL;

		/*
		 * Destroy all children except for error and workspace.
		 * (Some of them may be "clients" of error and workspace so
		 * they have to be destroyed first.)
		 */
		chl = &al->base.all_children;
		while(*chl){
			if((*chl)->pid == alcp->error_id ||
					(*chl)->pid == alcp->workspace_id)
				chl = &(*chl)->next;
			else{
				lret = NhlDestroy((*chl)->pid);
				ret = MIN(ret,lret);
			}
		}

		lret = NhlDestroy(alcp->error_id);
		ret = MIN(ret,lret);
		lret = NhlDestroy(alcp->workspace_id);
		ret = MIN(ret,lret);
		alcp->error_id = 0;
		alcp->workspace_id = 0;
		_NhlDestroyRLList();
		NrmDestroyDB(alcp->baseDB);
		alcp->baseDB = NULL;
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

	if(al == alcp->current_app){
		alcp->current_app = alcp->default_app;
		DefaultParentChange(alc);
	}

	NhlFree(alp->usr_appdir);
	NhlFree(alp->sys_appdir);
	NhlFree(alp->file_suffix);
	NrmDestroyDB(alp->appDB);
	alp->appDB = NULL;
	NhlFreeGenArray(alp->resources);
	for(i=0; i < alp->nres; i++)
		NhlFree(alp->values[i]);
	NhlFree(alp->values);
	NhlFree(alp->res);

	return ret;
}

/*
 * Private API
 */

/*
 * Function:	_NhlGetDefaultApp
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
_NhlGetDefaultApp
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	return NhlappClassRec.app_class.default_app->base.id;
}

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
	return (NhlLayer)NhlappClassRec.app_class.current_app;
}

/*
 * Function:	_NhlGetResDB
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
	NhlAppLayer	al;

        if (l) {
                al = (NhlAppLayer)l->base.appobj;
                if(((NhlLayer)al == l) || !al->app.appDB) {
                        NhlAppClass alc = (NhlAppClass)al->base.layer_class;
                        return alc->app_class.baseDB;
                }
        }
        else {
                al = (NhlAppLayer) _NhlGetCurrentApp();
        }

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
 * Function:	_NhlSortAppArgs
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
NhlErrorTypes
_NhlSortAppArgs
#if	NhlNeedProto
(
	NhlLayer	l,
	_NhlArgList	args_in,
	int		nargs_in,
	_NhlArgList	*args_out,
	int		*nargs_out
)
#else
(l,args_in,nargs_in,args_out,nargs_out)
	NhlLayer	l;
	_NhlArgList	args_in;
	int		nargs_in;
	_NhlArgList	*args_out;
	int		*nargs_out;
#endif
{
	NhlClass		lc = _NhlClass(l);
	NhlAppLayerPart		*alp = &((NhlAppLayer)l)->app;
	int			i;

	*nargs_out = 0;

	/*
	 * Deal with easy case.
	 */
	if(nargs_in == 0){
		alp->args = NULL;
		alp->nargs = 0;
		return NhlNOERROR;
	}

	/*
	 * Now we actually have to do some work...
	 */
	alp->args = NhlMalloc(sizeof(_NhlArg)*nargs_in);
	alp->nargs = 0;

	if(!alp->args){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	for(i=0;i < nargs_in;i++){
		/*
		 * if arg is in App class, then arg is added to that list
		 * otherwise it is added to the alp->args, so it can be
		 * treated as an arg to the "app defined" resources.
		 */
		if(_NhlResInClass(lc,args_in[i].quark))
			(*args_out)[(*nargs_out)++] = args_in[i];
		else
			alp->args[alp->nargs++] = args_in[i];
	}

	return NhlNOERROR;
}

/*
 * Public API
 */

int
NhlGetParentId
#if	NhlNeedProto
( int id)
#else
(id)
int id;
#endif
{
	NhlLayer l = _NhlGetLayer(id);
	if(l != NULL) {
		if((l->base.parent == NULL)||((NhlAppLayer)l->base.parent == NhlappClassRec.app_class.default_app)) {
			return(0);
		} else {
			return(l->base.parent->base.id);
		}
	}
	return 0;
}

/*
 * Function:	NhlAppGetDefaultParentId
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
NhlAppGetDefaultParentId
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	if((NhlappClassRec.app_class.current_app) &&
			(NhlappClassRec.app_class.current_app !=
				NhlappClassRec.app_class.default_app))

		return NhlappClassRec.app_class.current_app->base.id;

	return 0;
}

/*
 * Function:	nhlpfappgetdefaultparentid
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
_NHLCALLF(nhlpfappgetdefaultparentid,NHLPFAPPGETDEFAULTPARENTID)
#if	NhlNeedProto
(
	int	*id
)
#else
(id)
	int	*id;
#endif
{
	*id = NhlAppGetDefaultParentId();
}

/*
 * Function:	NhlIsApp
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
NhlBoolean
NhlIsApp
#if	NhlNeedProto
(
	int	pid
)
#else
(pid)
	int	pid;
#endif
{
	NhlLayer	l = _NhlGetLayer(pid);

	if(l && _NhlIsApp(l))
		return True;

	return False;
}

/*
 * Function:	nhlpfisapp
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
void _NHLCALLF(nhlpfisapp,NHLPFISAPP)
#if	NhlNeedProto
(
	int	*id,
	int	*status
)
#else
(id,status)
	int	*id;
	int	*status;
#endif
{
	*status = NhlIsApp(*id);

	return;
}

void
_NhlAppSetDefGuiData
#if	NhlNeedProto
(
	NhlPointer	ptr
)
#else
(ptr)
	NhlPointer	ptr;
#endif
{
	((NhlAppClass)NhlappClass)->app_class.default_guidata = ptr;

	return;
}

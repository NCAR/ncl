/*
 *      $Id: app.c,v 1.21 1999-06-02 23:01:18 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		app.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 29 15:55:07 MDT 1996
 *
 *	Description:	
 */

#include <stdlib.h>
#include <dirent.h>

#include <ncarg/ngo/appP.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/ngo/ncledit.h>
#include <ncarg/ngo/nclstate.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

#define	Oset(field)	NhlOffset(NgAppMgrRec,app.field)
static NhlResource resources[] = {

	{NgNappName,NgCappName,NhlTString,sizeof(NhlString),
		Oset(app_name),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,(NhlFreeFunc)NhlFree},
	{NgNappClass,NgCappClass,NhlTString,sizeof(NhlString),
		Oset(app_class),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_CGONLY,(NhlFreeFunc)NhlFree},
	{NgNappNclState,NgCappNclState,NhlTInteger,sizeof(int),Oset(nclstate),
		NhlTImmediate,_NhlUSET((NhlPointer)NhlDEFAULT_APP),
		_NhlRES_SGONLY,(NhlFreeFunc)NULL},
	{NgNappWksState,NgCappWksState,NhlTPointer,sizeof(NhlPointer),
	 	Oset(wks_state),NhlTImmediate,_NhlUSET((NhlPointer)NULL),
		_NhlRES_SGONLY,(NhlFreeFunc)NULL},

};
#undef	Oset

static _NhlRawObjCB callbacks[] = {
	{NgCBAppGoChange,NhlOffset(NgAppMgrRec,app.gochangecb),
		0,NULL,NULL,NULL},
};

static NhlErrorTypes AppMgrClassPartInitialize(
	NhlClass	lc
);

static NhlErrorTypes AppMgrClassInitialize(
	void
);

static NhlErrorTypes AppMgrInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes AppMgrSetValues(
        NhlLayer	old,
        NhlLayer	reference,
        NhlLayer	new,
        _NhlArgList	args,
        int             num_args
);

static NhlErrorTypes AppMgrDestroy(
	NhlLayer	l
);

static void AppError(
	NgAppMgr	app
);

NgAppMgrClassRec NgappMgrClassRec = {
	{
/* class_name			*/	"appMgrClass",
/* nrm_class			*/	NrmNULLQUARK,
/* layer_size			*/	sizeof(NgAppMgrRec),
/* class_inited			*/	False,
/* superclass			*/	(NhlClass)&NhlobjClassRec,
/* cvt_table			*/	NULL,

/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* all_resources		*/	NULL,
/* callbacks			*/	callbacks,
/* num_callbacks		*/	NhlNumber(callbacks),
/* class_callbacks		*/	NULL,
/* num_class_callbacks		*/	0,

/* class_part_initialize	*/	AppMgrClassPartInitialize,
/* class_initialize		*/	AppMgrClassInitialize,
/* layer_initialize		*/	AppMgrInitialize,
/* layer_set_values		*/	AppMgrSetValues,
/* layer_set_values_hook	*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_reparent		*/	NULL,
/* layer_destroy		*/	AppMgrDestroy,

	},
	{
/* num_mgrs			*/	0,
/* run_proc			*/	AppError,
/* dev_wproc			*/	(NgDevWorkProc)AppError,
	}
};

NhlClass NgappMgrClass = (NhlClass)&NgappMgrClassRec;


static NrmQuark Qngselectedwork = NrmNULLQUARK;

/*
 * Function:	AppMgrClassPartInitialize
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
AppMgrClassPartInitialize
(
	NhlClass	lc
)
{
        
	Qngselectedwork = NrmStringToQuark(Ng_SELECTED_WORK);
        
	return NhlNOERROR;
}

static void GetColormapsInPath
(
	const char	*path
)
{
	struct dirent	*dirp;  
	DIR		*dp;
	int		i,j;
	int		count;
	char		fullpath[1024];
	char		*endp;
	float   	colormap[768] = { -1.0,-1.0,-1.0,-1.0,-1.0,-1.0 };
	int		min_ix = 6;
	float		max_cval;

/*
 * These colormaps do not overwrite the background and foreground
 * colors. Initially we set foreground to black and background to white.
 * but when the user actually loads one of these color maps these are
 * replaced with the workstation's current background and foreground.
 * Eventually there will be an option for specifying a range of indexes
 * into which the colormap should fit.
 */
	
	if (! path) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
	   "Invalid directory encountered in colormap path specification"));
		return;
	}

	if ((dp = opendir(path)) == NULL) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "Invalid colormap directory: %s",path));
		return;
	}

	strcpy(fullpath,path);
	endp = fullpath + strlen(fullpath);
	*endp++ = '/';
	*endp = '\0';

	while ( (dirp = readdir(dp)) != NULL) {
		char *cp;
		FILE *fp;
		char buf[256];
		int dot_pos;

		if (! strcmp(dirp->d_name, ".")  ||
		    ! strcmp(dirp->d_name, ".."))
			continue;	
		if (! (cp = strrchr(dirp->d_name,'.')))
			continue;
		dot_pos = cp - dirp->d_name;
		cp++;
		if (! cp || 
		    (strcmp(cp,"rgb") && 
		     strcmp(cp,"ncmap") &&
		     strcmp(cp,"gp"))) 
			continue;
		
		strcpy(endp,dirp->d_name);
		fp = fopen(fullpath,"r");
		if (! fp) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "Unable to open colormap file %s: ignoring",
				   dirp->d_name));
			continue;
		}
		i = min_ix;
		max_cval = 0.0;
		while (cp = fgets(buf,255,fp)) {
			char *next,*tcp = cp;
			float f;
			while (isspace(*tcp))
				tcp++;
			if (! (isdigit(*tcp) || *tcp == '.'))
				continue;
			while (1) {
				if (i > 767)
					break;
				f = strtod(tcp,&next);
				if (next == tcp)
					break;
				tcp = next;
				while (isspace(*tcp) || *tcp == ',')
					tcp++;
				colormap[i] = f;
				max_cval = MAX(max_cval,colormap[i]);
				i++;
			}
		}
		count = i;
		if (max_cval > 1.0) {
			if (max_cval < 256.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 255.0;
				}
			}
			else if (max_cval <= 256.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 256.0;
				}
			}
			else if (max_cval < 65536.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 65535.0;
				}
			}
			else if (max_cval <= 65536.0) {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= 65536.0;
				}
			}
			else {
				for (i = min_ix; i < count; i++) {
					colormap[i] /= max_cval;
				}
			}
		}
		
		*(endp + dot_pos) = '\0';
		NhlPalSetColormap(NhlworkstationClass,endp,
				  (NhlColor *)colormap,count / 3);
		
	}
	
	return;
}

static void ReadUserColormaps
(
	void
)
{
	const char *path;
	char buf[1024];
	char *cp,*last_cp = buf;

	path = getenv(NDV_COLORMAP_PATH);
	if (! path) {
		fprintf(stderr,
		     "%s environment variable not set:\n\tdefaulting to %s\n",
			NDV_COLORMAP_PATH,_NgDEFAULT_COLORMAP_PATH);
		path = _NgDEFAULT_COLORMAP_PATH;
		
	}
	if (! path)
		return;
	
	strcpy(buf,path);
/*
 * Search for directories in the path from the end of the path string. 
 * That way, when duplicate names are found the files in directories at the
 * front of the path will replace the ones at the end.
 */
	cp = strrchr(buf,':');
	if (! cp) {
		GetColormapsInPath(_NGResolvePath(buf));
		return;
	}
	while (cp) {
		if (*(cp+1)) {
			GetColormapsInPath(_NGResolvePath(cp+1));
		}
		*cp = '\0';
		if (cp > buf) {
			cp = strrchr(cp - 1, ':');
			if (! cp) {
				GetColormapsInPath(_NGResolvePath(buf));
			}
		}
	}
	return;
}

/*
 * Function:	AppMgrClassInitialize
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
AppMgrClassInitialize
(
	void
)
{

/*
 * need to initialize the workstation class in order to create a
 * Palette object, so that the user-defined color maps can be read in
 * before the first workstation is created. This will allow user-defined
 * color maps to be used as the default color map.
 */
	_NhlInitializeClass(NhlworkstationClass);
	ReadUserColormaps();

	return NhlNOERROR;
}

/*
 * Function:	AppMgrInitialize
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
AppMgrInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char		func[] = "AppMgrInitialize";
	NgAppMgrClass	ac = (NgAppMgrClass)lc;
	NgAppMgrPart	*app = &((NgAppMgr)new)->app;
	NhlClass	amc = new->base.layer_class;

	if(amc == NgappMgrClass){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"Cannot create an AppMgr class!!"));
		return NhlFATAL;
	}

	if(ac->app_class.num_mgrs){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"%s:Only one AppMgr can be created",func));
		return NhlFATAL;
	}

	if(!app->app_name || !app->app_class){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:A Required resource not set",func));
		return NhlFATAL;
	}

	app->wp = NULL;
	app->go = NULL;
	app->active = NULL;
	app->selected_work_id = -1;

	ac->app_class.num_mgrs++;

	_NhlAppSetDefGuiData((NhlPointer)new->base.id);
	NhlVASetValues(new->base.appobj->base.id,
		_NhlNguiData,	(NhlPointer)new->base.id,
		NULL);

        
	return NhlNOERROR;
}


/*
 * Function:	AppMgrSetValues
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
AppMgrSetValues
(
        NhlLayer        old,
        NhlLayer        reference,
        NhlLayer        new,
        _NhlArgList     args,
        int             num_args
)
{
	char		func[] = "AppMgrSetValues";
	NgAppMgrPart	*app = &((NgAppMgr)new)->app;
	NgAppMgrPart	*oldapp = &((NgAppMgr)old)->app;
	NhlErrorTypes	ret = NhlNOERROR;

	if (_NhlArgIsSet(args,num_args,NgNappNclState) &&
	    oldapp->nclstate != NhlDEFAULT_APP) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: Can only set %s once",func,NgNappNclState));
		app->nclstate = oldapp->nclstate;
		ret = MIN(NhlWARNING,ret);	
	}
	if (_NhlArgIsSet(args,num_args,NgNappWksState) && 
	    oldapp->wks_state != NULL) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "%s: Can only set %s once",func,NgNappWksState));
		app->wks_state = oldapp->wks_state;
		ret = MIN(NhlWARNING,ret);	
	}

	return ret;
}

static void
FreeGOList
(
	_NgAppGOList	gol
)
{
	int	i,j;

	if(!gol)
		return;

	FreeGOList(gol->next);

	i=0;
	while(i < _NgGOLISTSIZE && gol->num){
		if(gol->go[i] != NhlDEFAULT_APP){
			NhlDestroy(gol->go[i]);
			gol->num--;
		}
		i++;
	}

	NhlFree(gol);

	return;
}

/*
 * Function:	AppMgrDestroy
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
AppMgrDestroy
(
	NhlLayer	l
)
{
	NgAppMgr	app = (NgAppMgr)l;
	NgAppMgrPart	*ap = &app->app;
	NgAppMgrClass	ac = (NgAppMgrClass)NgappMgrClass;
	int		i;
	_NgWorkProc	wk1,wk2;
	_NgAppFStack	afs1,afs2;

	wk1 = ap->wp;
	while(wk1){
		wk2 = wk1;
		wk1 = wk1->next;
		NhlFree(wk2);
	}

	FreeGOList(ap->go);

	afs1 = ap->active;
	while(afs1){
		afs2 = afs1;
		afs1 = afs2->next;
		NhlFree(afs2);
	}

	return NhlNOERROR;
}

/*
 * Function:	AppError
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
static void
AppError
(
	NgAppMgr	app
)
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,
			"AppError:AppMgr subclass \"%s\" messed up.",
			_NhlClassName(app->base.layer_class));
	return;
}

static void
DeleteWksCB
(
        NhlArgVal       cbdata,
        NhlArgVal       udata
)
{
        NgAppMgr	app = (NgAppMgr) udata.ptrval;
        NgNclAny        sym = (NgNclAny) cbdata.ptrval;
        NrmQuark        qsymname, *qvars;
        int		i,sel_id,id,id_count,count;
        int		*id_array;
	char		line[512];

        qsymname = NrmStringToQuark(sym->name);
        if (qsymname == Qngselectedwork)
                return;

        sel_id = NgNclGetHluObjId(app->app.nclstate,Ng_SELECTED_WORK,
                              &id_count,&id_array);
        if (id_count > 1)
                NhlFree(id_array);

        if (sel_id != app->app.selected_work_id)
                return;

            /* find another workstation, and assign selected work to it.
               Otherwise delete the selected work variable */
        
        qvars = NclGetHLUVarSymNames(&count);

        for (i = 0; i < count; i++) {
                if (qvars[i] == Qngselectedwork)
                        continue;
		id = NgNclGetHluObjId
			(app->app.nclstate,NrmQuarkToString(qvars[i]),
			 &id_count,&id_array);
		if (id <= NhlNULLOBJID)
			continue;
                if (id_count > 1)
                        NhlFree(id_array);
                if (_NhlIsWorkstation(_NhlGetLayer(id))) {
			if (id != sel_id)
				NgAppSetSelectedWork
					(app->base.id,
					 NrmQuarkToString(qvars[i]));
                        NclFree(qvars);
                        return;
                }
        }
	app->app.selected_work_id = NhlNULLOBJID;
        sprintf(line,"delete(%s)\n",Ng_SELECTED_WORK);
        (void)NgNclSubmitBlock(app->app.nclstate,line);
        NclFree(qvars);
        
        return;
}

/************************************************************************
 *									*
 *				Exported API				*
 *									*
 ***********************************************************************/

/*
 * Function:	NgAppRun
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
NgAppRun
(
	int	appid
)
{
	char		func[] = "NgAppRun";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;
	NhlArgVal	sel,user_data;
        NhlLayer        ncl;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}
        
        ncl = _NhlGetLayer(app->app.nclstate);
	NhlINITVAR(sel);
        sel.lngval = NgNclCBDELETE_HLUVAR;
	NhlINITVAR(user_data);
        user_data.ptrval = app;
        app->app.delete_wks_cb = _NhlAddObjCallback
                        (ncl,NgCBnsObject,sel,DeleteWksCB,user_data);

	ac = (NgAppMgrClass)app->base.layer_class;

	if(!ac->app_class.run_proc){
		AppError(app);
		return;
	}

	(*(ac->app_class.run_proc))(app);

	return;
}

/*
 * Function:	NgAppQuit
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
NgAppQuit
(
	int	appid
)
{
	char		func[] = "NgAppQuit";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;
	NhlDestroy(app->base.id);
/*
 * TODO:	call a quitConfirm callback - iff cbdata comes back true,
 *		then call quitNow callback. If that returns, exit.
 *              for now, just close ncl and then the hlu library.
 */
        NclCloseServer();
        
	exit(0);

	return;
}

static NhlBoolean
DoWork
(
	NgAppMgr	app
)
{
	NhlBoolean	done;
	_NgWorkProc	tmp = app->app.wp;

	done = (*tmp->proc)(tmp->cdata);

	if(done){
		app->app.wp = tmp->next;
		NhlFree(tmp);
	}

	return !app->app.wp;
}


/*
 * Function:	NgAddWorkProc
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
NgAddWorkProc
(
	int		appid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
)
{
	char		func[] = "NgAddWorkProc";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;
	_NgWorkProc	wp,*tmp;
	NhlBoolean	pending;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;

	if(!ac->app_class.dev_wproc){
		AppError(app);
		return;
	}

	pending = (app->app.wp && True);
	wp = NhlMalloc(sizeof(_NgWorkProcRec));
	if(!wp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	wp->proc = work_proc;
	wp->cdata = cdata;
	wp->next = NULL;

	for(tmp = &app->app.wp;*tmp;tmp = &(*tmp)->next);
	*tmp = wp;

	/*
	 * Only install a work proc at the device level, if there
	 * isn't one already installed.
	 */
	if(!pending)
		(*(ac->app_class.dev_wproc))(app,DoWork);

	return;
}

/*
 * Function:	NgAddPriorityWorkProc
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
NgAddPriorityWorkProc
(
	int		appid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
)
{
	char		func[] = "NgAddPriorityWorkProc";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	NgAppMgrClass	ac;
	_NgWorkProc	wp,*twp;
	NhlBoolean	pending;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	ac = (NgAppMgrClass)app->base.layer_class;

	if(!ac->app_class.dev_wproc){
		AppError(app);
		return;
	}

	pending = (app->app.wp && True);
	wp = NhlMalloc(sizeof(_NgWorkProcRec));
	if(!wp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	wp->proc = work_proc;
	wp->cdata = cdata;
	wp->next = NULL;
	twp = &app->app.wp;
	while (*twp) 
		twp = &((*twp)->next);
	*twp = wp;

	/*
	 * Only install a work proc at the device level, if there
	 * isn't one already installed.
	 */
	if(!pending)
		(*(ac->app_class.dev_wproc))(app,DoWork);

	return;
}

/*
 * Function:	NgRemoveWorkProc
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
NgRemoveWorkProc
(
	int		appid,
	NgWorkProc	work_proc,
	NhlPointer	cdata
)
{
	char		func[] = "NgRemoveWorkProc";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgWorkProc	tmp,*wp;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return;
	}

	wp = &app->app.wp;

	while(*wp){
		if(((*wp)->proc == work_proc) &&
			((*wp)->cdata == cdata)){

			tmp = *wp;
			*wp = (*wp)->next;
			NhlFree(tmp);

			return;
		}

		wp = &(*wp)->next;
	}

	NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"%s:Unable to remove proc",func));
	return;
}

#define	NgCBWPCALLING	(0x01)
#define	NgCBWPCLEANOUT	(0x02)
#define	NgCBWPDESTROY	(0x04)

typedef struct _NgCBWP_WPLRec _NgCBWP_WPLRec, *_NgCBWP_WPL;

struct NgCBWPRec{
	int		appmgrid;
	NgCBWPCopyFunc	copy_func;
	NgCBWPFreeFunc	free_func;
	_NhlCBFunc	cb_func;
	NhlArgVal	udata;

	int		state;
	int		co_id;
	_NhlCB		cb;
	_NhlCB		ldestroycb;
	_NhlCB		adestroycb;
	_NgCBWP_WPL	wp_list;
};

struct _NgCBWP_WPLRec{
	NgCBWP		cbwp;
	NhlArgVal	cbdata;
	_NgCBWP_WPL	next;
};

static void
_NgCBWPCleanout(
	NgCBWP		cbwp
);

/*
 * This function actually executes the cb_func that was added with the
 * NgCBWPAdd call - it frees the cbdata after calling the cb_func.
 */
static NhlBoolean
_NgCBWPWorkProc
(
	NhlPointer	cdata
)
{
	_NgCBWP_WPL	wpnode = (_NgCBWP_WPL)cdata;
	_NgCBWP_WPL	*wpptr;
	NgCBWP		cbwp = wpnode->cbwp;

	if(cbwp->state & NgCBWPCALLING)
		return False;

	cbwp->state = NgCBWPCALLING;
	(*cbwp->cb_func)(wpnode->cbdata,cbwp->udata);
	if(cbwp->free_func)
		(*cbwp->free_func)(wpnode->cbdata);

	wpptr = &cbwp->wp_list;
	while(*wpptr){
		if(*wpptr == wpnode){
			*wpptr = wpnode->next;
			break;
		}
		wpptr = &(*wpptr)->next;
	}

	NhlFree(wpnode);

	cbwp->state &= ~NgCBWPCALLING;
	/*
	 * If this CBWP list was marked for destroy during the call of
	 * the function - then free the whole thing now.
	 */
	if(cbwp->state & NgCBWPDESTROY)
		NgCBWPDestroy(cbwp);
	else if(cbwp->state & NgCBWPCLEANOUT)
		_NgCBWPCleanout(cbwp);

	return True;
}

/*
 * This is the function that is actually called by the NhlLayer's callback
 * list.  It just copies the cbdata and adds a work proc that will call the
 * users routine with the copy of the cbdata.
 */
static void
_NgCBWPCallback
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NhlArgVal	lcbdata;
	_NgCBWP_WPL	wpnode;
	NgCBWP		cbwp = (NgCBWP)udata.ptrval;

	if(cbwp->state & (NgCBWPDESTROY | NgCBWPCLEANOUT))
		return;

	NhlINITVAR(lcbdata);
	if(cbwp->copy_func){
		if(!(*cbwp->copy_func)(cbdata,cbwp->udata,&lcbdata))
			return;
	}
	else
		lcbdata = cbdata;

	wpnode = NhlMalloc(sizeof(_NgCBWP_WPLRec));
	if(!wpnode){
		if(cbwp->free_func)
			(*cbwp->free_func)(lcbdata);
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	wpnode->cbwp = cbwp;
	wpnode->cbdata = lcbdata;
	wpnode->next = cbwp->wp_list;
	cbwp->wp_list = wpnode;
	NgAddWorkProc(cbwp->appmgrid,_NgCBWPWorkProc,(NhlPointer)wpnode);

	return;
}

static void
_NgCBWPCleanout
(
	NgCBWP		cbwp
)
{
	_NgCBWP_WPL	node;

	if(cbwp->state & NgCBWPCALLING){
		cbwp->state |= NgCBWPCLEANOUT;
		return;
	}

	_NhlCBDelete(cbwp->cb);
	_NhlCBDelete(cbwp->ldestroycb);
	_NhlCBDelete(cbwp->adestroycb);
	cbwp->cb = cbwp->ldestroycb = cbwp->adestroycb = NULL;

	if((cbwp->co_id == NhlDEFAULT_APP) || (cbwp->co_id == cbwp->appmgrid)){
		while(cbwp->wp_list){
			node = cbwp->wp_list;
			cbwp->wp_list = node->next;
			if(cbwp->free_func)
				(*cbwp->free_func)(node->cbdata);
			NgRemoveWorkProc(cbwp->appmgrid,_NgCBWPWorkProc,
							(NhlPointer)node);
			NhlFree(node);
		}
	}

	return;
}

/*
 * This function is added as Destroy callback to the appmgr and to the
 * NhlLayer that the original callback list was installed to.  It removes
 * the callback, and if the object being destroyed is the appmgr, then
 * it also free's the _NgCBWP_WPL list since the work proc's won't be
 * executed anyway.
 */
static void
_NgCBWPDeleteCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NgCBWP		cbwp = (NgCBWP)udata.ptrval;
	NhlLayer	l = (NhlLayer)cbdata.ptrval;

	if(l->base.id != cbwp->appmgrid)
		cbwp->co_id = l->base.id;

	_NgCBWPCleanout(cbwp);


	return;
}

/*
 * Function:	NgCBWPAdd
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
extern NgCBWP
NgCBWPAdd
(
	int		appmgrid,
	NgCBWPCopyFunc	copy_func,
	NgCBWPFreeFunc	free_func,
	NhlLayer	l,
	NhlString	cbname,
	NhlArgVal	sel,
	_NhlCBFunc	cb_func,
	NhlArgVal	udata
)
{
	char		func[] = "NgCBWPAdd";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appmgrid);
	NhlArgVal	ludata,lsel;
	NgCBWP		cbwp;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"%s:Invalid NgAppMgr object!",
									func);
		return NULL;
	}

	cbwp = NhlMalloc(sizeof(NgCBWPRec));
	if(!cbwp){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	cbwp->state = 0;
	cbwp->co_id = NhlDEFAULT_APP;
	NhlINITVAR(ludata);
	NhlINITVAR(lsel);
	ludata.ptrval = cbwp;
	cbwp->cb = _NhlAddObjCallback(l,cbname,sel,_NgCBWPCallback,ludata);
	if(!cbwp->cb){
		NhlFree(cbwp);
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to add CBWP",func));
		return NULL;
	}

	cbwp->ldestroycb = _NhlAddObjCallback(l,_NhlCBobjDestroy,
						lsel,_NgCBWPDeleteCB,ludata);
	cbwp->adestroycb = _NhlAddObjCallback((NhlLayer)app,_NhlCBobjDestroy,
						lsel,_NgCBWPDeleteCB,ludata);

	cbwp->appmgrid = appmgrid;
	cbwp->copy_func = copy_func;
	cbwp->free_func = free_func;
	cbwp->cb_func = cb_func;
	cbwp->udata = udata;

	cbwp->wp_list = NULL;

	return cbwp;
}

/*
 * Function:	NgCBWPDestroy
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
extern void
NgCBWPDestroy
(
	NgCBWP	cbwp
)
{
	_NgCBWP_WPL	node;

	if(!cbwp)
		return;

	if(cbwp->state & NgCBWPCALLING){
		cbwp->state |= NgCBWPDESTROY;
		return;
	}

	cbwp->co_id = NhlDEFAULT_APP;
	_NgCBWPCleanout(cbwp);

	NhlFree(cbwp);

	return;
}

#if 0
/* not used now -- leaving in until I'm sure it's not really necessary */

static void
CompressGOList
(
	_NgAppGOList	*list
)
{
	_NgAppGOList	end=NULL;
	_NgAppGOList	next;
	int		n;

	while(*list){
		next = (*list)->next;
		n = _NgGOLISTSIZE - (*list)->num;
		if(!next || (n <= 0)){
			list = &(*list)->next;
			continue;
		}
		memcpy(&(*list)->go[(*list)->num],next->go,sizeof(int)*n);
		(*list)->num += n;
		next->num -= n;
		if(next->num > 0){
			memmove(next->go,&next->go[n],sizeof(int)*next->num);
			list = &(*list)->next;
		}
		else{
			/*
			 * if node is empty, save it in the "end" list.
			 */
			(*list)->next = next->next;
			next->next = end;
			end = next;
		}
	}

	/*
	 * Place empty nodes on end of list.
	 */
	*list = end;

	return;
}
#endif
/*
 * Function:	NgAppAddGO
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
NgAppAddGO
(
	int		appid,
	int		goid
)
{
	char			func[] = "NgAppAddGO";
	NgAppMgr		app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList		*go;
	int			i;
	NhlBoolean		compress=False;
	NhlArgVal		cbdata,sel;
	NgAppGoChangeRec	gc;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	/*
	 * Find the last node with entries...
	 */
	REDO:
	for(go = &app->app.go;*go;go=&(*go)->next){
		if ((*go)->num <  _NgGOLISTSIZE)
			break;
#if 0
		/*
		 * If the current node is full, get the next one.
		 */
		if((*go)->num >= _NgGOLISTSIZE){
			/*
			 * If this is the last node, and it is full, try
			 * compressing the list.
			 */
			if(!compress && !(*go)->next){
				CompressGOList(&app->app.go);
				compress=True;
				goto REDO;
			}
			continue;
		}

		/*
		 * There is room in the current node - if there is no next
		 * node, or it is empty, then we can add the go to the
		 * current node.
		 */
		if(!(*go)->next || ((*go)->next->num <= 0))
			break;
#endif
	}

	if(!*go){
		*go = NhlMalloc(sizeof(_NgAppGOListRec));
		if(!*go){
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		memset((*go)->go,NhlDEFAULT_APP,sizeof(int)*_NgGOLISTSIZE);
		(*go)->num = 0;
		(*go)->next = NULL;
	}

	(*go)->go[(*go)->num++] = goid;
	if(app->app.active)
		NgGOSensitive(goid,False);

	NhlINITVAR(cbdata);
	NhlINITVAR(sel);
	gc.reason = NgAppGoAdd;
	gc.goid = goid;
	cbdata.ptrval = &gc;
	sel.lngval = 0; /* ignored */
	_NhlCallObjCallbacks((NhlLayer)app,NgCBAppGoChange,sel,cbdata);

	return;
}

/*
 * Function:	NgAppRemoveGO
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
NgAppRemoveGO
(
	int	appid,
	int	goid
)
{
	char			func[] = "NgAppRemoveGO";
	NgAppMgr		app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList		*go;
	int			i,j;
	NhlArgVal		cbdata,sel;
	NgAppGoChangeRec	gc;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = &app->app.go;

	while(*go && (*go)->num){
		for(i=0;i<(*go)->num;i++){
			if((*go)->go[i] == goid){
				NgAppReleaseFocus(appid,goid);
				(*go)->num--;
				for(j=i;j<(*go)->num;j++)
					(*go)->go[j] = (*go)->go[j+1];
				(*go)->go[(*go)->num] =NhlDEFAULT_APP;
#if 0
				if((*go)->num){
					for(j=i;j<(*go)->num;j++)
						(*go)->go[j] = (*go)->go[j+1];
					(*go)->go[(*go)->num] =NhlDEFAULT_APP;
				}
				else{
					/*
					 * Move empty record to end of list
					 */
					_NgAppGOList	tmp = *go;

					tmp->go[0] = NhlDEFAULT_APP;
					*go = (*go)->next;
					while(*go)
						go = &(*go)->next;
					*go = tmp;
				}
#endif

				NhlINITVAR(cbdata);
				NhlINITVAR(sel);
				gc.reason = NgAppGoRemove;
				gc.goid = goid;
				cbdata.ptrval = &gc;
				sel.lngval = 0; /* ignored */
				_NhlCallObjCallbacks((NhlLayer)app,
						NgCBAppGoChange,sel,cbdata);
				return;
			}
		}
		go = &(*go)->next;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Unable to find goid %d",func,goid));
	return;
}

void
NgAppEnumerateGO
(
	int		appid,
	NgAppGOEnumFunc	enumerate,
	NhlPointer	udata
)
{
	char		func[] = "NgAppEnumerateGO";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppGOList	go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	go = app->app.go;

	while(go && go->num){
		for(i=0;i<go->num;i++){
			if(!(*enumerate)(go->go[i],udata))
				return;
		}
		go = go->next;
	}

	return;
}

/*
 * Function:	NgAppGrabFocus
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
NgAppGrabFocus
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppGrabFocus";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppFStack	fs;
	_NgAppGOList	go;
	int		i;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	fs = NhlMalloc(sizeof(_NgAppFStackRec));
	if(!fs){
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	fs->active = goid;
	fs->next = app->app.active;
	app->app.active = fs;

	go = app->app.go;
	while(go){
		for(i=0;i<go->num;i++){
			NgGOSensitive(go->go[i],(go->go[i] == goid));
		}
		go = go->next;
	}

	return;
}

/*
 * Function:	NgAppReleaseFocus
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
NgAppReleaseFocus
(
	int	appid,
	int	goid
)
{
	char		func[] = "NgAppReleaseFocus";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
	_NgAppFStack	*fsp,fs=NULL;
	_NgAppGOList	go;
	int		i;
	NhlBoolean	enable = False;

	if(!app || !_NhlIsClass((NhlLayer)app,NgappMgrClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid appid.",func));
		return;
	}

	fsp = &app->app.active;
	while(*fsp){
		if((*fsp)->active == goid){
			fs = *fsp;
			*fsp = (*fsp)->next;
			break;
		}
		fsp = &(*fsp)->next;
	}

	if(!fs) return;
	NhlFree(fs);

	if(app->app.active)
		goid = app->app.active->active;
	else
		enable = True;

	go = app->app.go;
	while(go){
		for(i=0;i<go->num;i++){
			NgGOSensitive(go->go[i],
					(enable || (go->go[i] == goid)));
		}
		go = go->next;
	}
}

/*
 * Function:	NgAppSetSelectedWork
 *
 * Description:	removes the destroy callback associated with the current
 *              selected workstation, adds a destroy callback for the new
 *              selected workstation, then sets the workstation.
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
NgAppSetSelectedWork
(
	int		appid,
        NhlString	symbol_name
)
{
	char		func[] = "NgAppSetSelectedWork";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
        NhlErrorTypes   ret = NhlNOERROR;
        int		id,id_count;
        int		*id_array;
	char	line[512];

        id = NgNclGetHluObjId(app->app.nclstate,symbol_name,
                              &id_count,&id_array);
        if (id_count > 1)
                NhlFree(id_array);
        if (! _NhlIsWorkstation(_NhlGetLayer(id))) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                           "%s: invalid workstation symbol_name",func));
                return NhlFATAL;
        }
        sprintf(line,"%s = %s\n",Ng_SELECTED_WORK,symbol_name);
        (void)NgNclSubmitBlock(app->app.nclstate,line);

        app->app.selected_work_id = id;
        
        return ret;
}

/*
 * Function:	NgAppGetSelectedWork
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
NgAppGetSelectedWork
(
	int		appid,
	NhlBoolean	create,
        NhlBoolean	*created
)
{
	char		func[] = "NgAppGetSelectedWork";
	NgAppMgr	app = (NgAppMgr)_NhlGetLayer(appid);
        int		selected_id,count,*ids;

	if (! create)
		return app->app.selected_work_id;

        *created = False;
        if (! NclSymbolDefined(Ng_SELECTED_WORK)) {
                char *name, line[512];
                    /*
                     * Submit to nclstate
                     */
                name = NgNclGetSymName(app->app.nclstate,"Xwk",True);
                sprintf(line,
               "%s = create \"%s\" xWorkstationClass defaultapp\nend create\n",
                        name,name);
                (void)NgNclSubmitBlock(app->app.nclstate,line);
                sprintf(line,"%s = %s\n",Ng_SELECTED_WORK,name);
                (void)NgNclSubmitBlock(app->app.nclstate,line);
                *created = True;
        }
        selected_id = NgNclGetHluObjId
                (app->app.nclstate,Ng_SELECTED_WORK,&count,&ids);
        if (count > 1) {
                NHLPERROR((NhlINFO,NhlEUNKNOWN,
                           "%s:selected workstation variable is an array",
                           func));
                NhlFree(ids);
        }
	app->app.selected_work_id = selected_id;
        return selected_id;
}

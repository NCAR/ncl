/*
 *      $Id: plotapp.c,v 1.4 1999-08-28 00:18:43 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotapp.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 15 17:03:14 MST 1999
 *
 *	Description:	
 */


#include <ncarg/ngo/plotappP.h>
#include <ncarg/ngo/browseP.h>
#include <ncarg/hlu/App.h>
#include <ncarg/hlu/ConvertP.h>
#include <ncarg/ngo/nclstate.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/wait.h>

static NrmQuark QString = NrmNULLQUARK;
static NrmQuark QndvData = NrmNULLQUARK;
static NrmQuark QndvObjects = NrmNULLQUARK;
static NrmQuark QndvFuncDirs = NrmNULLQUARK;
static NrmQuark QndvFuncFiles = NrmNULLQUARK;
static NrmQuark QndvFunctions = NrmNULLQUARK;
static NrmQuark QndvExecFunction = NrmNULLQUARK;
static NrmQuark QpmOverlays = NrmNULLQUARK;
static NrmQuark QmpDataLimitObject = NrmNULLQUARK;
static NrmQuark QndvUpdateFunc = NrmNULLQUARK;

static NrmQuark *QAppResList = NULL;

typedef char ObjResType;

#define _NgRES_REGULAR		0
#define _NgRES_FAKE		1
#define _NgRES_PROFILE		2
#define _NgRES_UPDATE_FUNC	3

static NrmQuark *QFakeRes[] = { 
	&QpmOverlays,&QmpDataLimitObject,&QndvUpdateFunc
};

/*
 * Enhanced resource file key strings and characters
 */
#define PROFILESTRING "%Profile" /* signature for a Profile type resource */
#define DYNRES_TOKEN "@"      /* the dynamic resource binding token */
#define COORD_TOKEN "!"      /* indicates a coord resource  */

/*
 * This record type describes the data profile for a resource
 */

typedef struct _AppResProfileRec {
	NrmQuark	qname;
	NrmQuark	qrefres;
	int		vis;
	int		required;
	int		mindims;
	int		maxdims;
	NhlPointer	data;
	int		save_to_compare;
} AppResProfileRec, *AppResProfile;

/*
 * Each field in the data profile is recognized in the resource file within
 * a string array resource where each element is a name-value pair separated
 * by the colon character. The names are defined by a static array of the
 * following records. The index field matches the resource to the field in
 * the AppResProfile record. Note that not all resources are supported yet.
 */
typedef struct _ProfResRec {
	NhlString	name;
	int		len;
	int		index;
} ProfResRec;

static ProfResRec ProfRes[] = {
	{"Name", 0, 0},
	{"Reference", 0, 1},
	{"Visibility", 0 , 2},
	{"SaveForCompare", 0, 3}
};

/*
 * List of AppObject resources (identified by <object_name>DYNRES_TOKEN in the
 * res file).
 */

struct _AppObjectRec; /* forward reference */
struct _AppDataRec; /* forward reference */

typedef union _AppSym {
	struct _AppObjectRec *o;
	struct _AppDataRec   *d;
} AppSym;

typedef char RefKind;

#define _NgOBJ_REF	0
#define _NgDATA_REF	1

/*
 * When references to objects or data appear in the resfile they are 
 * dilimited by '$'. The character following the '$' may qualify the
 * the reference as a coordinate(!), an attribute(@) or as a subset(\().
 * For subsets white space may be permitted eventually. It's not yet
 * understood.
 */
typedef char RefType;

#define _NgREF_REGULAR		0
#define _NgREF_COORD		1
#define _NgREF_ATTR		2
#define _NgREF_SUBSET		3

typedef struct _AppResSymRefRec {
	struct _AppResSymRefRec *next;
	RefKind			kind;	/* symbol is obj or data */
	AppSym			sym;
 	RefType			rtype;	 
	int			offset;  /* from start to first '$' */
	int			count;   /*  characters to final '$'*/
} AppResSymRefRec, *AppResSymRef;

typedef struct _AppObjResRec {
	struct _AppObjResRec	*next;
	NrmQuark		qres;
	NhlString		value;
	ObjResType		type;
	AppResProfile		prof;
	AppResSymRef		symrefs;
} AppObjResRec, *AppObjRes;

/*
 * All the references to data and object symbols are recorded here -- one
 * record for each object resource that references the symbol. The starting
 * location(s) of the symbol in the resource value string are recorded
 * in the locs field. 
 */


/* this should go away in a little while */

typedef struct _AppResRefLocRec {
	struct _AppResRefLocRec *next;
	struct _AppObjectRec	*obj;	/* -> to actual AppObject element */
	AppObjRes		objres;   /* -> to the actual AppObjRes el. */
	int			loc_count; /* #refs in this obj resource val */
	char			**locs;   /* location list -> to initial '$'*/
} AppResRefLocRec, *AppResRefLoc;

/*
 * Data resources are all synthetic, and attach to the data symbol, not
 * to HLU data objects
 */

typedef struct _AppDataResRec {
	struct _AppDataResRec	*next;
	NrmQuark		qres;
	NhlString		value;
	NhlBoolean		is_coord;
	int			coord_ix;
	AppResSymRef		symrefs;
} AppDataResRec, *AppDataRes;

/*
 * Each AppData symbol in the res file is recorded here
 */

typedef struct _AppDataRec {		  
	struct _AppDataRec	*next;
	NrmQuark		qdataname;
	NhlString		description;
	NhlBoolean		required;
	int			ndims;
	AppResRefLoc		dlocs; /* I don't think these are needed */
	AppDataRes		datares;
} AppDataRec, *AppData;

/*
 * List of AppObject objects
 */

typedef struct _AppObjectRec {
	struct _AppObjectRec	*next;
	NrmQuark		qbasename;
	NhlClass		class;
	AppObjRes		objres;
	AppResRefLoc		olocs; /* I don't think these are needed */
} AppObjectRec, *AppObject;

/*
 * Central data structure for each PlotApp
 */

typedef struct _PlotAppRec {
	struct _PlotAppRec	*next;
	int			go_id;
	int			app_id;
	NhlString		class_name;	/* the 'lead' obj class */
	NrmQuark		qstyle_name;
	NhlString		priv_style_name;
	NhlBoolean		preview;
	int			ref_count;
	NrmQuark  		qname;
	NrmQuark		qdir;
	NrmQuark		*qapp_res;   /* NULL-terminated quark list */
	NrmQuark		*qobj_deps;  /* objects in dependency order */
	NhlClass		*obj_classes;  /* classes of the dep objects */
	int			obj_count;
	AppObject		objects;  
	int			data_count;
	AppData			data;
} PlotAppRec, *PlotApp;

static PlotApp PlotAppList = NULL;


typedef struct _DataTableRec {
	AppData		appdata;
	NrmQuark	qvar;
	NrmQuark	qfile;
	NrmQuark	qdims[10]; /* dims listed in fast to slow order */
	int		dim_ix[10]; /* indexes of the dims in the var */
	NhlBoolean	reorder;
	NclApiDataList	*dl;
	NhlBoolean	free_dl;
	NgVarDataRec	vd_rec;
} DataTableRec, *DataTable;

static DataTable Data_Table = NULL;
static int DataTableAllocCount = 0;

typedef struct _FuncFileRec {
	struct _FuncFileRec *next;
	NrmQuark	    qfuncdir;
	NrmQuark	    qfuncfile;
} FuncFileRec, *FuncFile;

static FuncFile FuncFileList = NULL;

static _NhlConvertContext 
StringResToStringArray 
(
	NhlString	resval,
	int		*count,
	NhlString	**str_array,
	_NhlConvertContext context
)
{
	NhlGenArray	ga;
        NrmValue 	from, to;

/*
 * The user of this routine needs to free the context when done using the
 * string array. Do NOT free the string array itself.
 */

	from.size = sizeof(NhlString);
	from.data.ptrval = resval;
	to.size = sizeof(NhlGenArray);
	to.data.ptrval = &ga;

	if (! context)
		context = _NhlCreateConvertContext(NULL);
	if (! context)
		return NULL;

	_NhlConvertContextClass(context,NhlappClass);
	_NhlConvertData(context,
			NrmStringToQuark(NhlTString),
			NrmStringToQuark(NhlTStringGenArray),
			&from,&to);

	if (! ga || ga->num_dimensions != 1 || 
	    ga->num_elements < 1)
		return NULL;

	*count = ga->num_elements;

	*str_array = (NhlString *) ga->data;

	return context;
}

static void
SetAppResProfile
(
	AppObjRes objres,
	AppObjRes profres
	)
{
        _NhlConvertContext context = NULL;
	AppResProfile	resprof = NhlMalloc(sizeof(AppResProfileRec));
	NhlString	*prof_vals;
	int		prof_count;
	int		i,j;
	static NhlBoolean first = True;

	if (first) {
		for (i = 0; i < NhlNumber(ProfRes); i++) {
			ProfRes[i].len = strlen(ProfRes[i].name);
		}
		first = False;
	}

	if (! resprof) {
 		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}

	resprof->qname = NrmNULLQUARK;
	resprof->qrefres = NrmNULLQUARK;
	resprof->vis = -1;
	resprof->required = -1;
	resprof->mindims = -1;
	resprof->maxdims = -1;
	resprof->data = (NhlPointer)-1;
	resprof->save_to_compare = -1;

	context = StringResToStringArray
		(profres->value,&prof_count,&prof_vals,NULL);

	for (i = 0; i < prof_count; i++) {
		for (j = 0; j < NhlNumber(ProfRes); j++) {
			char *cp;
			if (strncmp(prof_vals[i],
				    ProfRes[j].name,ProfRes[j].len))
				continue;
			cp = prof_vals[i] + ProfRes[j].len;
			while (*cp && isspace(*cp))
				cp++;
			if (*cp != ':')
				continue;
			cp++;
			while (*cp && isspace(*cp))
				cp++;
			if (*cp) {
				switch (ProfRes[j].index) {
				case 0:
					resprof->qname = NrmStringToQuark(cp);
					break;
				case 1:
					resprof->qrefres = 
						NrmStringToQuark(cp);
					break;
				case 2:
					if (! strcasecmp(cp,"False"))
						resprof->vis = False;
					else
						resprof->vis = True;
					break;
				case 3:
					if (! strcasecmp(cp,"False"))
						resprof->save_to_compare = 
							False;
					else
						resprof->save_to_compare = 
							True;
					break;
				}
			}
			break;
		}
	}
	objres->prof = resprof;

	_NhlFreeConvertContext(context);

	return;
}

static void ParseAppObjRes
(
	PlotApp		papp,
 	AppObject	appobj
)
{
	int 		i,j;
	char 		*obj_name = NrmQuarkToString(appobj->qbasename);
	int 		len, toklen;
	NrmQuark	*qappres;
	AppObjRes 	objres;
	int		update_func_len = strlen(NhlNndvUpdateFunc);

	len = strlen(obj_name);
	toklen = strlen(DYNRES_TOKEN);

	for (i = 0; papp->qapp_res[i] != NrmNULLQUARK; i++) {
		char buf[256];
		char *res,*cp;
		NrmQuark qobjres,qfake;
		NhlString value,objresstr;

		res = NrmQuarkToString(papp->qapp_res[i]);

		if (strncmp(res,obj_name,len))
		    continue;
		if (! strlen(res) > len + toklen )
			continue;
		if (strncmp(&res[len],DYNRES_TOKEN,toklen))
			continue;
		objresstr = &res[len+toklen];

		objres = NhlMalloc(sizeof(AppObjResRec));
		objres->next = appobj->objres;
		appobj->objres = objres;

		NhlVAGetValues(papp->app_id,
			       res,&value,
			       NULL);
		objres->value = value;

		objres->type = _NgRES_REGULAR;
		objres->prof = NULL;
		objres->symrefs = NULL;
		if ((cp = strstr(objresstr,PROFILESTRING))) {
			int reslen = cp - objresstr;
			objres->type = _NgRES_PROFILE;
			strncpy(buf,objresstr,reslen);
			buf[reslen] = '\0';
			qobjres = NrmStringToQuark(buf);
			objres->qres = qobjres;
		}
		else if (! strncmp(objresstr,
				 NhlNndvUpdateFunc,update_func_len)) {
			qobjres = NrmStringToQuark(objresstr);
			objres->qres = qobjres;
			objres->type = _NgRES_UPDATE_FUNC;
		}
		else {
			qobjres = NrmStringToQuark(objresstr);
			objres->qres = qobjres;
			for (j = 0; j < NhlNumber(QFakeRes); j++) {
				if (*QFakeRes[j] == qobjres) {
					objres->type = _NgRES_FAKE;
					break;
				}
			}
		}
#if 0
		printf("\tobject: %s type: %d res: %s value: %s\n",
		       obj_name,objres->type,
		       NrmQuarkToString(objres->qres),
		       objres->value);
#endif
	}
	for (objres = appobj->objres; objres; objres = objres->next) {
		if (objres->type == _NgRES_PROFILE) {
			AppObjRes 	refres;
			for (refres = appobj->objres; refres; 
			     refres = refres->next) {
				if (refres == objres)
					continue;
				if (objres->qres == refres->qres) {
					SetAppResProfile(refres,objres);
				}
			}
		}
	}
			
		
}

static AppResRefLoc GetAppRefLocs
(
	PlotApp		papp,
	NhlString	refname
)
{
	int		i;
	AppObject	appobj;
	int		len = strlen(refname);
	AppResRefLoc 	base_rloc = NULL,last_rloc = NULL;;

	for (appobj = papp->objects; appobj != NULL; appobj = appobj->next) {
		AppObjRes res;
		for (res = appobj->objres; res != NULL; res = res->next) {
			AppResRefLoc rloc = NULL;
			char *loc = strchr(res->value,'$');
			while (loc) {
				if (! strncmp(loc+1,refname,len) &&
				    *(loc + len + 1) == '$') {
					if (! rloc) {
						rloc = NhlMalloc
						    (sizeof(AppResRefLocRec));
						rloc->obj = appobj;
						rloc->objres = res;
						rloc->loc_count = 0;
						rloc->locs = NULL;
						rloc->next = NULL;
					}
					rloc->loc_count++;
					rloc->locs = NhlRealloc
						(rloc->locs,rloc->loc_count *
						 sizeof(char *));
					rloc->locs[rloc->loc_count-1] = loc;
					loc = strchr(loc+len+2,'$');
				}
				else {
					/* the '$'s come in pairs remember */
					loc = strchr(loc+1,'$');
					loc = strchr(loc+1,'$');
				}
			}
			if (rloc) {
#if 0
				printf("%s: %d refs to %s\n",
				       NrmQuarkToString(res->qres),
				       rloc->loc_count,refname);
#endif
				if (! base_rloc)
					base_rloc = rloc;
				if (last_rloc)
					last_rloc->next = rloc;
				last_rloc = rloc;
			}
		}
	}
	return base_rloc;
}		

 
static void RecordObjects
(
	PlotApp		papp,
	NhlString	objres
)
{
	NgGO		go;
	int		i;
	NhlString	objects_str;
	NhlString	*objs;
	int		obj_count;
        _NhlConvertContext context = NULL;
	AppObject 	appobj,last_appobj;

	go = (NgGO)_NhlGetLayer(papp->go_id);
	if (! go)
		return;

	NhlVAGetValues(papp->app_id,
		       objres,&objects_str,
		       NULL);

	if (! objects_str)
		return;

	context = StringResToStringArray(objects_str,&obj_count,&objs,NULL);

	papp->obj_count = obj_count;
	for (i = 0, last_appobj = NULL; i < obj_count; i++) {
		NhlClass class;
		char *str,*name, *class_str;
	       
		str = objs[i];
		while (*str && isspace(*str))
			str++;
		name = str;
		while (*str && ! (isspace(*str) || *str == ':'))
			str++;
		*str = '\0';
		str++;
		while (*str && (isspace(*str) || *str == ':'))
			str++;
		class_str = str;
#if 0
		printf("name %s class %s\n",name,class_str);
#endif
		class = NgNclHluClassPtrFromName(go->go.nclstate,class_str);
		if (! class)
			continue;
		appobj = NhlMalloc(sizeof(AppObjectRec));
		appobj->qbasename = NrmStringToQuark(name);
		appobj->class = class;
		appobj->objres = NULL;
		appobj->next = NULL;
		if (last_appobj)
			last_appobj->next = appobj;
		else
			papp->objects = appobj;
			
		last_appobj = appobj;
		ParseAppObjRes(papp,appobj);
	}
	/*
	 * Now that all resources have been parsed, find references from
	 * one object to another.
	 */
	for (appobj = papp->objects; appobj; appobj = appobj->next) {
		appobj->olocs = GetAppRefLocs
			(papp,NrmQuarkToString(appobj->qbasename));
	}
	NhlFree(objects_str);
	_NhlFreeConvertContext(context);
	return;
}


static void ParseAppDataRes
(
	PlotApp		papp,
	AppData		appdata
)
{
	int 		i,j;
	char 		*data_sym = NrmQuarkToString(appdata->qdataname);
	int 		len, toklen, coord_toklen;
	NrmQuark	*qappres;
	AppDataRes 	datares;

	len = strlen(data_sym);
	toklen = strlen(DYNRES_TOKEN);
	coord_toklen = strlen(COORD_TOKEN);

	for (i = 0; papp->qapp_res[i] != NrmNULLQUARK; i++) {
		char buf[256];
		char *res,*cp;
		NrmQuark qdatares;
		NhlString value,dataresstr;
		NhlBoolean is_coord = False;
		int	coord_ix = 0;

		res = NrmQuarkToString(papp->qapp_res[i]);

		if (strncmp(res,data_sym,len))
		    continue;
		if (! strlen(res) > len + toklen )
			continue;
		cp = &res[len];
		if (! strncmp(cp,COORD_TOKEN,coord_toklen)) {
			char *tcp;
			coord_ix = strtol(cp+1,&tcp,10);
			is_coord = tcp > cp + 1 ? True : False;
			cp = tcp;
		}
		
		if (strncmp(cp,DYNRES_TOKEN,toklen))
			continue;
		dataresstr = cp + toklen;
		NhlVAGetValues(papp->app_id,
			       res,&value,
			       NULL);
/*
 * certain pseudo resources are stored globally
 */
		if (! strcmp(dataresstr,"Description")) {
			appdata->description = value;
#if 0
			printf("\tdata sym: %s res: %s value: %s\n",
			       data_sym,dataresstr,value);
#endif
		}
		else if (! strcmp(dataresstr,"Required")) {
			if (! strcasecmp(value,"True"))
				appdata->required = True;
#if 0
			printf("\tdata sym: %s res: %s value: %s\n",
			       data_sym,dataresstr,value);
#endif
		}
		else {
			datares = NhlMalloc(sizeof(AppDataResRec));
			datares->next = appdata->datares;
			appdata->datares = datares;

			datares->value = value;
			datares->is_coord = is_coord;
			datares->coord_ix = coord_ix;
			datares->symrefs = NULL;

			qdatares = NrmStringToQuark(dataresstr);
			datares->qres = qdatares;
#if 0
			printf("\tdata sym: %s res: %s value: %s",
			       data_sym,dataresstr,value);
			if (datares->is_coord)
				printf(" coord: %d\n",datares->coord_ix);
			else 
				printf("\n");
#endif
		}
	}
	return;
}
	     
static void RecordData
(
	PlotApp		papp,
	NhlString	datares
)
{
	NgGO		go;
	int		i;
	NhlString	data_str;
	NhlString	*data;
	int		data_count;
        _NhlConvertContext context = NULL;

	go = (NgGO)_NhlGetLayer(papp->go_id);
	if (! go)
		return;

	NhlVAGetValues(papp->app_id,
		       datares,&data_str,
		       NULL);

	if (! data_str)
		return;

	context = StringResToStringArray(data_str,&data_count,&data,NULL);

/*
 * work backwards so that data symbols end up in the same order as they
 * are entered in the plot style res file.
 */
	papp->data_count = data_count;
	for (i = data_count - 1; i >= 0; i--) {
		AppData    appdata;
		AppResRefLoc dlocs;
		char *str,*name,*tcp;
		int  ndims;
		
		str = data[i];
		while (*str && isspace(*str))
			str++;
		name = str;
		while (*str && ! (isspace(*str) || *str == ':'))
			str++;
		*str = '\0';
		str++;
		while (*str && (isspace(*str) || *str == ':'))
			str++;
		ndims = strtol(str,&tcp,10);
#if 0
		printf("name: %s ndims: %d \n",name,ndims);
#endif
		appdata = NhlMalloc(sizeof(AppDataRec));
		appdata->qdataname = NrmStringToQuark(name);
		appdata->ndims = ndims;
		appdata->datares = NULL;
		appdata->next = papp->data;
		appdata->required = False;
		appdata->description = NULL;
		papp->data = appdata;
		appdata->dlocs = GetAppRefLocs(papp,name);
		ParseAppDataRes(papp,appdata);
	}

	NhlFree(data_str);
	_NhlFreeConvertContext(context);
	return;
}

static NhlBoolean Loaded
(
	NrmQuark  qdir,
	NrmQuark  qfuncfile
)
{
	FuncFile  ffile;

	for (ffile = FuncFileList; ffile; ffile = ffile->next)
		if (qdir == ffile->qfuncdir && qfuncfile == ffile->qfuncfile)
			return True;

	return False;
}

static NhlBoolean Readable
(
	NrmQuark  qdir,
	NrmQuark  qfuncfile
)
{
        struct stat statbuf;
	NhlString funcfile = NrmQuarkToString(qfuncfile);
	NhlString dir = NrmQuarkToString(qdir);
	char buf[1024];


	if (funcfile[0] == '/') { /* full path -- ignore the directory */
		sprintf(buf,"%s",funcfile);
	}
	else {
		sprintf(buf,"%s/%s",dir,funcfile);
	}
	if (stat(buf,&statbuf))
		return False;

	if ((statbuf.st_mode & S_IROTH) ||
	    ((getgid() == statbuf.st_gid) && (statbuf.st_mode & S_IRGRP)) ||
	    ((getuid() == statbuf.st_uid) && (statbuf.st_mode & S_IRUSR)))
		return True;

	return False;
}

static void Load
(
	NgGO	  go,
	NrmQuark  qdir,
	NrmQuark  qfuncfile
)
{
	NhlString	funcfile = NrmQuarkToString(qfuncfile);
	NhlString	dir = NrmQuarkToString(qdir);
	char		buf[1024];
	FuncFile	ffile;
	
	if (funcfile[0] == '/') {
		sprintf(buf,"load \"%s\"\n",funcfile);
	}
	else {
		sprintf(buf,"load \"%s/%s\"\n",dir,funcfile);
	}

	(void)NgNclSubmitLine(go->go.nclstate,buf,True);

	ffile = NhlMalloc(sizeof(FuncFileRec));
	if (! ffile) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return;
	}
	ffile->qfuncdir = qdir;
	ffile->qfuncfile = qfuncfile;

	ffile->next = FuncFileList;
	FuncFileList = ffile;
	
	return;
}
static void RecordAndLoadFunctions
(
	PlotApp		papp,
	NhlString	funcdirs,
	NhlString	funcfiles,
	NhlString	execfunc
)
{
	NgGO		go;
	int		i,j;
	NhlString	func_dir_array;
	NhlString	func_file_array;
	char 		buf[256];
	NhlString	*func_files = NULL,*func_dirs = NULL;
	int		func_file_count = 0, func_dir_count = 0;

	_NhlConvertContext context = NULL;

	go = (NgGO)_NhlGetLayer(papp->go_id);
	if (! go)
		return;

	NhlVAGetValues(papp->app_id,
		       funcdirs,&func_dir_array,
		       funcfiles,&func_file_array,
		       NULL);

	/*
	 * If no func files the func dirs are irrelevant
	 */
	if (! func_file_array)
		return;

	context = StringResToStringArray(func_file_array,
					 &func_file_count,&func_files,NULL);
	if (func_dir_array) {
		context = StringResToStringArray
			(func_dir_array,&func_dir_count,&func_dirs,context);
	}
	/*
	 * Eventually we'll also look through directories specified by
	 * the NDV_FUNCTION_FILE_PATH. But for now just use the directory
	 * where the resource file was found. Note that the func file
	 * may be specified as a full path in which case the directory 
	 * is ignored.
	 */

	if (! func_dir_count) {
		for (i = 0; i < func_file_count; i++) {
			NrmQuark qfuncfile = NrmStringToQuark(func_files[i]);
			if (! Loaded(papp->qdir,qfuncfile) &&
			    Readable(papp->qdir,qfuncfile))
				Load(go,papp->qdir,qfuncfile);
		}
	}
	else {
		for (i = 0; i < func_file_count; i++) {
			NrmQuark qfuncfile = NrmStringToQuark(func_files[i]);
			for (j = 0; j < func_dir_count; j++) {
				NrmQuark qdir = NrmStringToQuark(func_dirs[j]);
				if (! Loaded(qdir,qfuncfile) &&
				    Readable(qdir,qfuncfile)) {
					Load(go,qdir,qfuncfile);
					break;
				}
			}
		}
	}

	if (context)
		_NhlFreeConvertContext(context);
	return;
}

static void RecordObjResRefs
(
	PlotApp	papp,
	AppObjRes res
)
{
	char buf[256];
	char *sp,*ep;
	int  rlen;

	strcpy(buf,res->value);
	rlen = strlen(buf);

	ep = buf;
	while ((sp = strchr(ep,'$')) != NULL) {
		NrmQuark	qref;
		AppObject	appobj;
		AppData		appdata;
		NhlBoolean	found = False;
		RefType		rtype;

		ep = strchr(sp+1,'$');
		if (! ep)
			return;

		if (ep < buf + rlen + 1) {
			char *cp;

			switch (*(ep+1)) {
			default:
				rtype = _NgREF_REGULAR;
				break;
			case '!':
				rtype = _NgREF_COORD;
				break;
			case '@':
				rtype = _NgREF_ATTR;
				break;
			case '(':
				rtype = _NgREF_SUBSET;
				break;
			}
		}
		*ep = '\0';

		qref = NrmStringToQuark(sp+1);

		/*
		 * Since the refs are stored from back to front, that is
		 * how they will be replaced. This ensures that the offset
		 * value from the front of the string will be correct 
		 * in the substituted string as each substitution is made.
		 */
		
		for (appdata = papp->data; appdata; appdata = appdata->next) {
			if (appdata->qdataname == qref) {
				AppResSymRef symref = 
					NhlMalloc(sizeof(AppResSymRefRec));
				if (! symref) {
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return;
				}
				symref->next = res->symrefs;
				res->symrefs = symref;
				symref->sym.d = appdata;
				symref->kind = _NgDATA_REF;
				symref->rtype = rtype;
				symref->offset = sp - buf;
				symref->count = ep - sp;
				found = True;
				break;
			}
		}
		if (found) {
			ep++;
			continue;
		}
		for (appobj = papp->objects; appobj; appobj = appobj->next) {
			if (appobj->qbasename == qref) {
				AppResSymRef symref = 
					NhlMalloc(sizeof(AppResSymRefRec));
				if (! symref) {
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return;
				}
				symref->next = res->symrefs;
				res->symrefs = symref;
				symref->sym.o = appobj;
				symref->kind = _NgOBJ_REF;
				symref->rtype = rtype;
				symref->offset = sp - buf;
				symref->count  = ep - sp;
				break;
			}
		}
	        ep++;
	}
	return;
}


static void RecordDataResRefs
(
	PlotApp	papp,
	AppDataRes res
)
{
	char buf[256];
	char *sp,*ep;
	int  rlen;

	strcpy(buf,res->value);
	rlen = strlen(buf);

	ep = buf;
	while ((sp = strchr(ep,'$')) != NULL) {
		NrmQuark qref;
		AppObject appobj;
		AppData   appdata;
		NhlBoolean   found = False;
		RefType    rtype;
		
		ep = strchr(sp+1,'$');
		if (! sp)
			return;

		if (ep < buf + rlen + 1) {
			switch (*(ep+1)) {
			default:
				rtype = _NgREF_REGULAR;
				break;
			case '!':
				rtype = _NgREF_COORD;
				break;
			case '@':
				rtype = _NgREF_ATTR;
				break;
			case '(':
				rtype = _NgREF_SUBSET;
				break;
			}
		}
		*ep = '\0';
		qref = NrmStringToQuark(sp + 1);
		
		for (appdata = papp->data; appdata; appdata = appdata->next) {
			if (appdata->qdataname == qref) {
				AppResSymRef symref = 
					NhlMalloc(sizeof(AppResSymRefRec));
				if (! symref) {
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return;
				}
				symref->next = res->symrefs;
				res->symrefs = symref;
				symref->sym.d = appdata;
				symref->kind = _NgDATA_REF;
				symref->rtype = rtype;
				symref->offset = sp - buf;
				symref->count = ep - sp;
				found = True;
				break;
			}
		}
		if (found) {
			ep++;
			continue;
		}
		for (appobj = papp->objects; appobj; appobj = appobj->next) {
			if (appobj->qbasename == qref) {
				AppResSymRef symref = 
					NhlMalloc(sizeof(AppResSymRefRec));
				if (! symref) {
					NHLPERROR((NhlFATAL,ENOMEM,NULL));
					return;
				}
				symref->next = res->symrefs;
				res->symrefs = symref;
				symref->sym.o = appobj;
				symref->kind = _NgOBJ_REF;
				symref->rtype = rtype;
				symref->offset = sp - buf;
				symref->count = ep - sp;
				break;
			}
		}
		ep++;
	}
	return;
}
				
	       
		
static void RecordObjectAndDataReferences
(
	PlotApp	papp
)
{
	AppObject	appobj;
	AppData		appdata;

	for (appobj = papp->objects; appobj != NULL; appobj = appobj->next) {
		AppObjRes res;
		for (res = appobj->objres; res != NULL; res = res->next) {
			RecordObjResRefs(papp,res);
		}
		
	}
	for (appdata = papp->data; appdata != NULL; appdata = appdata->next) {
		AppDataRes res;
		for (res = appdata->datares; res != NULL; res = res->next) {
			RecordDataResRefs(papp,res);
		}
	}
	return;
}
static void ParseAppResources
(
	PlotApp	papp
)
{
	NhlGenArray	appres;
	NhlString	*res;
	int		i;
	int		obj_ix = -1, data_ix = -1;
	int		funcdir_ix = -1, funcfile_ix = -1, execfunc_ix = -1;
	NhlString 	funcdirs, funcfiles, execfunc;
	
	NhlVAGetValues(papp->app_id,
		       NhlNappResources,&appres,
		       NULL);

	if (! appres || appres->num_dimensions != 1 || 
	    appres->num_elements < 1)
		return;

	res = (NhlString *) appres->data;

	papp->qapp_res = NhlMalloc(sizeof(NrmQuark) *
				  (appres->num_elements + 1));

	for (i = 0; i < appres->num_elements; i++) {
#if 0
		printf("%s\n",res[i]);
#endif
		papp->qapp_res[i] = NrmStringToQuark(res[i]);
		if (papp->qapp_res[i] == QndvObjects)
			obj_ix = i;
		if (papp->qapp_res[i] == QndvData)
			data_ix = i;
		if (papp->qapp_res[i] == QndvFuncDirs)
			funcdir_ix = i;
		if (papp->qapp_res[i] == QndvFuncFiles)
			funcfile_ix = i;
		if (papp->qapp_res[i] == QndvExecFunction)
			execfunc_ix = i;
	}
	papp->qapp_res[i] = NrmNULLQUARK;

	if (obj_ix >= 0) {
		RecordObjects(papp,res[obj_ix]);
	}
	if (data_ix >= 0) {
		RecordData(papp,res[data_ix]);
	}
	RecordObjectAndDataReferences(papp);

	funcdirs = (funcdir_ix >= 0) ? res[funcdir_ix] : NULL;
	funcfiles = (funcdir_ix >= 0) ? res[funcfile_ix] : NULL;
	execfunc = (execfunc_ix >= 0) ? res[execfunc_ix] : NULL;

	RecordAndLoadFunctions(papp,funcdirs,funcfiles,execfunc);

	NhlFreeGenArray(appres);

	return;
}

static PlotApp Plot_App;

static int ObjectComp
(
        const void *p1,
        const void *p2
)
{
        const NrmQuark q1 = *(NrmQuark *) p1;
        const NrmQuark q2 = *(NrmQuark *) p2;
	AppObject appobj;
	AppObjRes objres;

 	for (appobj = Plot_App->objects; appobj; appobj = appobj->next) {
		if (appobj->qbasename != q1)
			continue;
		for (objres = appobj->objres; objres; objres = objres->next) {
			AppResSymRef	symref;
			for (symref = objres->symrefs; symref; 
			     symref = symref->next) {
				if (! symref->kind == _NgOBJ_REF)
					continue;
				if (symref->sym.o->qbasename == q2)
					return 1;
			}
		}
	}
 	for (appobj = Plot_App->objects; appobj; appobj = appobj->next) {
		if (appobj->qbasename != q2)
			continue;
		for (objres = appobj->objres; objres; objres = objres->next) {
			AppResSymRef	symref;
			for (symref = objres->symrefs; symref; 
			     symref = symref->next) {
				if (! symref->kind == _NgOBJ_REF)
					continue;
				if (symref->sym.o->qbasename == q1)
					return -1;
			}
		}
	}
        return 0;
}

static int ReorderObjDependencies
(
	NrmQuark  *qobjs,
	int	  obj_count,
	NrmQuark  qbefore,
	NrmQuark  qafter
)
{
	int i,before_ix, after_ix;

	for (i = 0; i < obj_count; i++) {
		if (qobjs[i] == qbefore)
			before_ix = i;
		if (qobjs[i] == qafter)
			after_ix = i;
	}
	if (before_ix > after_ix) {
		NrmQuark qt = qobjs[before_ix];
		for (i = before_ix; i > after_ix; i--) {
			qobjs[i] = qobjs[i-1];
		}
		qobjs[after_ix] = qt;
		return after_ix + 1;
	}
	return -1;
}
static void SetUpObjDependencyList
(
	PlotApp	papp
)
{
	AppObject appobj;
	int obj_count = papp->obj_count;
	int i;
	int end = -1;

	papp->qobj_deps = NhlMalloc(sizeof(NrmQuark) * (obj_count + 1));
	papp->obj_classes = NhlMalloc(sizeof(NhlClass) * (obj_count + 1));
	
	/* 
	 * First populate the dependency list with the objects;
	 * then move things around based on dependencies
	 */
	Plot_App = papp;
	for (appobj = papp->objects,i = 0; appobj; appobj = appobj->next) {
		papp->qobj_deps[i++] = appobj->qbasename;
	}
	papp->qobj_deps[obj_count] = NrmNULLQUARK;

#if 0
	qsort(papp->qobj_deps,obj_count,sizeof(NrmQuark),ObjectComp);

	while (end < obj_count) {
		for (appobj = papp->objects; appobj; appobj = appobj->next) {
			AppResRefLoc rloc;
			NrmQuark     qlastname = NrmNULLQUARK;
			if (! appobj->olocs)
				continue;
			for (rloc = appobj->olocs; rloc; rloc = rloc->next) {
				if (rloc->obj->qbasename == 
				    appobj->qbasename ||
				    rloc->obj->qbasename == qlastname)
					continue;
				end = ReorderObjDependencies
					(papp->qobj_deps,obj_count,
					 appobj->qbasename,
					 rloc->obj->qbasename);
				qlastname = rloc->obj->qbasename;
			}
		}
	}
#endif
	for (i = 0; i < obj_count; i++) {
		for (appobj = papp->objects; appobj; appobj = appobj->next) {
			if (papp->qobj_deps[i] == appobj->qbasename) {
				papp->obj_classes[i] = appobj->class;
				break;
			}
		}
#if 0
		printf("Class %s: %s, ",
		       papp->obj_classes[i]->base_class.class_name,
		       NrmQuarkToString(papp->qobj_deps[i]));
#endif
	}
#if 0
	printf("\n");
#endif

	return;
}
static NhlString
PreProcessPlotStyleFile
(
	const char *priv_dir,
	NhlString dir,
	NhlString pstyle
)
{
	char buf[1024];
        struct stat statbuf;
        FILE *fpin, *fpout;
	char *cp;
	static char priv_pstyle_buf[256];
	int i,pid;

	static char outfilehead[] = "*appResources : (/ \\\n\t";

	sprintf(buf,"%s/%s.res",dir,pstyle);
/*
 * if no res file, just return
 */
	if (stat(buf,&statbuf))
		return NULL;

	if (! (fpin = fopen(buf,"r"))) {
		NHLPERROR((NhlFATAL,
			   NhlEUNKNOWN,"Error opening resource file: %s",
			   buf));
		return NULL;
	}
/*
 * the private plotstyle name is composed of the prefix "_Ng", followed by
 * the plotstyle name, the process id, and finally the ".res" suffix.
 */
	
	pid = getpid();
	sprintf(priv_pstyle_buf,"_Ng%s%d",pstyle,pid);
	sprintf(buf,"%s/%s.res",priv_dir,priv_pstyle_buf);
	if (! (fpout = fopen(buf,"w"))) {
		NHLPERROR((NhlFATAL,
			   NhlEUNKNOWN,"Error opening resource file: %s",
			   buf));
		fclose(fpin);
		return NULL;
	}


/*
 * write out the appResources first, 
 * then everthing else
 */
	fputs(outfilehead,fpout);
	fputs(NrmQuarkToString(QAppResList[0]),fpout);
	for (i = 1; QAppResList[i] != NrmNULLQUARK; i++) {
		fputs(" , \\\n\t",fpout);
		fputs(NrmQuarkToString(QAppResList[i]),fpout);
	}
	

	while (cp = fgets(buf,255,fpin)) {
		char *acp,*ccp,*scp,*dcp;
		while (isspace(*cp))
			cp++;
		if (*cp == '!')
			continue;
		ccp = strchr(cp,':');
		if (! ccp)
			continue;
		acp = strchr(cp,'@');
		if (! acp)
			continue;
		if (acp > ccp)
			continue;
		*(ccp--) = '\0';
		while (isspace(*ccp))
			*(ccp--) = '\0';
		scp = strchr(cp,'*');
		if (scp && scp < acp)
			cp = scp + 1;
		dcp = strchr(cp,'.');
		if (dcp && dcp < acp)
			cp = dcp + 1;
		fputs(" , \\\n\t",fpout);
		fputs(cp,fpout);
	}
	fputs(" /)\n\n",fpout);

	rewind(fpin);
	while (cp = fgets(buf,255,fpin)) {
		fputs(cp,fpout);
	}
	fclose(fpin);
	fclose(fpout);

	return priv_pstyle_buf;
}	
static int CreatePlotApp
(
	int		go_id,
	NrmQuark	qplot_style,
	NrmQuark	qplot_style_dir,
	NhlBoolean	preview,
	NhlString	*priv_style_name
)
{
	NhlErrorTypes	ret = NhlNOERROR;
	char		priv_dirbuf[512];
	NhlString	plotstyledir,plotstyle,priv_plotstyle;
	const char 	*tmpdir;
	NgResData 	app_res_data;
        NgSetResProc	setresproc[2];
        NhlPointer	appresdata;
	int		app_id;
/*
 * The plot style res file is preprocessed to make all resources specified
 * with the "@" symbol into application resources. All the predefined 
 * application resources are also inserted into the res file at this point.
 * That way the user does not have to worry about these resources, or need
 * to list them in two places. After preprocessing a temporary file is 
 * written into the tmp dir. This is the resfile that the application
 * actually reads. It is deleted as soon as the App object is created.
 */

	if (qplot_style == NrmNULLQUARK) 
		return NhlNULLOBJID;
	plotstyle = NrmQuarkToString(qplot_style);


#if DEBUG_PLOTAPP
	fprintf(stderr,"plot style %s\n",plotstyle);
#endif

	if (qplot_style_dir != NrmNULLQUARK) {
		plotstyledir = NrmQuarkToString(qplot_style_dir);
	}
	else {
		plotstyledir = "./plot_styles";
	}

	tmpdir =  GetNCARGPath("tmp");
	if (! tmpdir) {
		return NhlNULLOBJID;
	}
	priv_plotstyle = PreProcessPlotStyleFile
		(tmpdir,plotstyledir,plotstyle);

	if (! priv_plotstyle) 
		return NhlFATAL;


	if (preview)
		strcpy(priv_dirbuf,tmpdir);
	else 
		sprintf(priv_dirbuf,"\"%s\"",tmpdir);

	app_res_data = NgReallocResData(NULL,1);
	if (! app_res_data)
		return NhlNULLOBJID;

	app_res_data->res_count = 1;
	app_res_data->res[0] = "appUsrDir";
	app_res_data->types[0] = QString;
	app_res_data->values[0] = (NhlPointer) priv_dirbuf;

	if (preview) {
		NgPreviewResProc	resproc;

		resproc = NgPreviewResList;
		appresdata = &app_res_data;
		ret = NgCreatePreviewGraphic
			(go_id,&app_id,priv_plotstyle,priv_plotstyle,
			 NULL,"appClass",1,&resproc,&appresdata);
	}
	else {
		NgSetResProc	resproc;

		resproc = NgAddResList;
		appresdata = app_res_data;
		ret = NgCreateGraphic
			(go_id,&app_id,priv_plotstyle,priv_plotstyle,
			 NULL,"appClass",1,&resproc,&appresdata);
	}

	if (! app_id || ret < NhlWARNING)
		return (int) ret;

	sprintf(priv_dirbuf,"%s/%s.res",tmpdir,priv_plotstyle);
	errno = 0;
	if (unlink(priv_dirbuf) != 0) {
		NHLPERROR((NhlWARNING,errno,
			   "Error unlinking private plotstyle res file %s",
			   priv_dirbuf));
	}

	NgFreeResData(app_res_data);

	*priv_style_name = NhlMalloc(strlen(priv_plotstyle)+1);
	if (! *priv_style_name) {
 		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}
	strcpy(*priv_style_name,priv_plotstyle);

	return app_id;
}

static void DestroyPlotApp
(
	int		go_id,
	NrmQuark	qplot_style,
	int		app_id,
	NhlBoolean	preview
)
{
	if (! qplot_style)
		return;

	if (preview)
		NgDestroyPreviewGraphic(go_id,app_id);
	else
		NgDestroyGraphic(go_id,NrmQuarkToString(qplot_style));

	return;
}

void NgDeletePlotAppRef
(
	NrmQuark	qplotstyle
)
{
	PlotApp	*papp = &PlotAppList;
	PlotApp	dpapp = NULL;
	NrmQuark	qname;

	while (*papp) {
		if ((*papp)->qname == qplotstyle) {
			dpapp = *papp;
			dpapp->ref_count--;
#if 0
			printf("delete ref count: %d\n",dpapp->ref_count);
#endif
			if (dpapp->ref_count > 0)
				return;
			*papp = (*papp)->next;
			break;
		}
		papp = &((*papp)->next);
	}
	if (! dpapp) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "No references left to plot style: %s",
			   NrmQuarkToString(qplotstyle)));
		return;
	}

	if (dpapp->preview)
		NgDestroyPreviewGraphic(dpapp->go_id,dpapp->app_id);
	else 
		NgDestroyGraphic(dpapp->go_id,dpapp->priv_style_name);

	if (dpapp->qapp_res)
		NhlFree(dpapp->qapp_res);
 	if (dpapp->qobj_deps)
		NhlFree(dpapp->qobj_deps);
 	if (dpapp->obj_classes)
		NhlFree(dpapp->obj_classes);
 	if (dpapp->priv_style_name)
		NhlFree(dpapp->priv_style_name);

	while (dpapp->objects) {
		AppObject obj = dpapp->objects;
		dpapp->objects = obj->next;
		while (obj->objres) {
			AppObjRes objres = obj->objres;
			obj->objres = objres->next;
			NhlFree(objres);
		}
		NhlFree(obj);
	}
	while (dpapp->data) {
		AppData data = dpapp->data;
		dpapp->data = data->next;
		while (data->dlocs) {
			AppResRefLoc dloc = data->dlocs;
			data->dlocs = dloc->next;
			NhlFree(dloc->locs);
			NhlFree(dloc);
		}
		NhlFree(data);
	}

	NhlFree(dpapp);

	return;
}

int NgNewPlotAppRef
(
	int		go_id,
	NhlString	plot_style,
	NhlString	plot_style_dir,
	NhlString	plot_style_name,
	NhlString	plot_style_class_name,
	NhlBoolean	preview
)
{
	NrmQuark	qplotstyle,qplotstyledir,qstylename;
	PlotApp 	papp = PlotAppList;
	int 		app_id;
	NrmQuark	qname;
	NhlBoolean	found = False;
	NgGO		go = (NgGO)_NhlGetLayer(go_id);
	NhlString	priv_style_name;

	if (! go)
		return (int) NhlNULLOBJID;

/*
 * force preview to False; eventually we may get rid of this parameter.
 * the app object is private, so the user won't be setting any of its
 * resources.
 */
	preview = False;
	qplotstyle = NrmStringToQuark(plot_style);
	qplotstyledir = NrmStringToQuark(plot_style_dir);
	qstylename = NrmStringToQuark(plot_style_name);

	if (QString == NrmNULLQUARK) {
		int qapp_rescount = 0;
		QString = NrmStringToQuark(NhlTString);
		QndvFuncDirs = NrmStringToQuark(NhlNndvFuncDirs);
		qapp_rescount++;
		QndvFuncFiles = NrmStringToQuark(NhlNndvFuncFiles);
		qapp_rescount++;
		QndvFunctions = NrmStringToQuark(NhlNndvFunctions);
		qapp_rescount++;
		QndvExecFunction = NrmStringToQuark(NhlNndvExecFunction);
		qapp_rescount++;
		QndvObjects = NrmStringToQuark(NhlNndvObjects);
		qapp_rescount++;
		QndvData = NrmStringToQuark(NhlNndvData);
		qapp_rescount++;
		QpmOverlays = NrmStringToQuark(NhlNpmOverlays);
		qapp_rescount++;
		QmpDataLimitObject = NrmStringToQuark(NhlNmpDataLimitObject);
		qapp_rescount++;
		QndvUpdateFunc = NrmStringToQuark(NhlNndvUpdateFunc);
		qapp_rescount++;
		QAppResList = NhlMalloc(sizeof(NrmQuark)*(qapp_rescount + 1));
		qapp_rescount = 0;
		QAppResList[qapp_rescount++] = QndvFuncDirs;
		QAppResList[qapp_rescount++] = QndvFuncFiles;
		QAppResList[qapp_rescount++] = QndvFunctions;
		QAppResList[qapp_rescount++] = QndvExecFunction;
		QAppResList[qapp_rescount++] = QndvObjects;
		QAppResList[qapp_rescount++] = QndvData;
		QAppResList[qapp_rescount++] = QpmOverlays;
		QAppResList[qapp_rescount++] = QmpDataLimitObject;
		QAppResList[qapp_rescount++] = QndvUpdateFunc;
		QAppResList[qapp_rescount] = NrmNULLQUARK;
	}
	while (papp) {
		if (papp->qname == qplotstyle) {
			papp->ref_count++;
#if 0
			printf("add ref count: %d\n",papp->ref_count);
#endif

			if (papp->preview && ! preview) {
				DestroyPlotApp(go_id,qplotstyle,
					       papp->app_id,True);
				found = True;
				break;
			}
			else if (! _NhlGetLayer(papp->app_id)) {
				found = True;
				break;
			}
			return papp->app_id;
		}
		papp = papp->next;
	}
#if 0
	printf("creating plot app: %s\n",NrmQuarkToString(qplotstyle));
#endif

	app_id = CreatePlotApp(go_id,qplotstyle,qplotstyledir,preview,
			       &priv_style_name);
	if (app_id <= NhlNULLOBJID)
		return NhlNULLOBJID;

	if (! found) {
		papp = NhlMalloc(sizeof(PlotAppRec));
		if (! papp) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return (int) NhlFATAL;
		}
		papp->ref_count = 1;
		papp->next = PlotAppList;
		PlotAppList = papp;
		papp->objects = NULL;
		papp->data = NULL;
		papp->qapp_res = NULL;
	}

	papp->go_id = go_id;
	papp->app_id = app_id;
	papp->qname = qplotstyle;
	papp->qdir = qplotstyledir;
	papp->preview = preview;
	papp->qstyle_name = qstylename;
	papp->priv_style_name = priv_style_name;
	papp->class_name = plot_style_class_name;
	papp->obj_count = 0;
	papp->data_count = 0;

	ParseAppResources(papp);

	SetUpObjDependencyList(papp);

	return app_id;
}

static NgDataItem DataItemInProfile
(
	NgDataProfile	dprof,
	AppObject	obj,
	AppObjRes	res
	
)
{
	int i;
	NgDataItem ditem;

	for (i = 0; i < dprof->n_dataitems; i++) {
		ditem = dprof->ditems[i];
		if (ditem->resq == res->qres &&
		    ditem->qhlu_name == obj->qbasename)
				
			return ditem;
	}
	return NULL;
}

static void EditDataItem
(
	NgDataItem	ditem,
	AppObject	obj,
	AppObjRes	res
	
)
{
	int i;
	AppResProfile rprof = res->prof;	

	if (! rprof)
		return;

	if (rprof->qname != NrmNULLQUARK)
		ditem->name = NrmQuarkToString(rprof->qname);
	if (rprof->vis > -1)
		ditem->vis = (NhlBoolean) rprof->vis;
	else
		ditem->vis = True;
	if (rprof->required > -1) 
		ditem->required = (NhlBoolean) rprof->required;
	if (rprof->mindims > -1) 
		ditem->mindims = rprof->mindims;
	if (rprof->maxdims > -1) 
		ditem->maxdims = rprof->maxdims;
	if ((int)rprof->data > -1) 
		ditem->data = rprof->data;
	if (rprof->save_to_compare > -1) 
		ditem->save_to_compare = (NhlBoolean)rprof->save_to_compare;

	return;

}
static NhlBoolean SyntheticResource
(
	AppObjRes	res
)
{
	int i;

	for (i = 0; i < NhlNumber(QFakeRes); i++) {
		if (*(QFakeRes[i]) == res->qres)
			return True;
	}
	return False;
}

static void MergeObjResDataItems
(
	NgGO		go,
	NgDataProfile	dprof,
	AppObject	obj
	)
{
	AppObjRes	res;
	int		rescount = 0,n_dataitems;
	int		update_func_len = strlen(NhlNndvUpdateFunc);
	
	for (res = obj->objres; res; res = res->next) {
		if (res->type == _NgRES_PROFILE)
			continue;
		else if (DataItemInProfile(dprof,obj,res)) {
			continue;
		}
			rescount++;
	}
	n_dataitems = dprof->n_dataitems + rescount;

	dprof->ditems = NhlRealloc
		(dprof->ditems,n_dataitems * sizeof(NgDataItem));

	for (res = obj->objres; res; res = res->next) {
		NgDataItem ditem;
		AppResProfile rprof = res->prof;
		NgDataItemType ditype = _NgCONFIG;
		NhlBoolean set_only = False;
		NhlPointer data = (NhlPointer) -1;
		NrmQuark qres = res->qres;

		if (res->type == _NgRES_PROFILE)
			continue;

		set_only = True;
		if (res->type == _NgRES_UPDATE_FUNC) {
			NhlString resname = NrmQuarkToString(qres);
			int sequence;
			ditype = _NgUPDATEFUNC;

			if (! strcmp(resname,NhlNndvUpdateFunc))
				sequence = 0;
			else {
				sequence = strtol(&resname[update_func_len],
						  NULL,0);
			}
			data = (NhlPointer) sequence;
			if (rprof) {
				rprof->data = data;
			}
		}
		else if (SyntheticResource(res)) {
			ditype = _NgSYNTHETIC;
		}

		if ((ditem = DataItemInProfile(dprof,obj,res))) {
			EditDataItem(ditem,obj,res);
		}
		else if (rprof) {
			ditem = NgNewDataItem
				(rprof->qname,qres,obj->qbasename,
				 obj->class,ditype,rprof->mindims,
				 rprof->maxdims,
				 rprof->data < 0 ? NULL : rprof->data,
				 rprof->required < 0 ? False : rprof->required,
				 rprof->vis < 0 ? True : rprof->vis,
				 set_only,
				 rprof->save_to_compare < 0 ? 
				 False : rprof->save_to_compare);
			NgAppendDataProfileItem(dprof,ditem,rprof->qrefres);
		}
		else {
			ditem = NgNewDataItem
				(qres,qres,obj->qbasename,
				 obj->class,ditype,0,1,data,
				 False,True,set_only,False);
			NgAppendDataProfileItem(dprof,ditem,NrmNULLQUARK);
		}
		ditem->appres_info = (NhlPointer) res;
	}
	return;
}
	
NgDataProfile NgNewPlotAppDataProfile
(
	int		go_id,
	NrmQuark	qplotstyle
)
{
	PlotApp	papp = PlotAppList;
	NrmQuark	qname;
	AppObject	obj;
	NgDataProfile	dprof = NULL;
	NgGO		go = (NgGO) _NhlGetLayer(go_id);

	if (! go)
		return NULL;

	while (papp) {
		if (papp->qname == qplotstyle) {
			break;
		}
		papp = papp->next;
	}
	if (! papp)
		return NULL;

	if (! papp->objects) {
		/*
		 * No objects listed explicitly. But there still has to
		 * be at least one
		 */
			
		if (papp->class_name) {
			return NgNewDataProfile(go,papp->class_name);
		}
		else {
			return NULL;
		}
	}
	if (! strcmp(papp->class_name,NGPLOTCLASS)) {
		dprof = NgNewDataProfile(go,papp->class_name);
	}
	for (obj = papp->objects; obj; obj = obj->next) {
		if (NgHasDataProfile(go,obj->class->base_class.class_name))
			dprof = NgMergeDataProfiles
				(go,dprof,NrmQuarkToString(obj->qbasename),
				 obj->class->base_class.class_name);
		if (! dprof)
			return NULL; 

		MergeObjResDataItems(go,dprof,obj);
	}
	dprof->qpstyle = papp->qname;
	dprof->obj_count = papp->obj_count;
	dprof->qobjects = papp->qobj_deps;
	dprof->obj_classes = papp->obj_classes;

	return dprof;
}
static NhlBoolean PerlMatch
(
	NhlString pattern,
	NhlString match_text
)
{
	char cmd[] = "echo %s | perl  -e 'while (<>){exit 1 if %s; exit 0}'";
	char buf[512];
	int ret;

	sprintf(buf,cmd,match_text,pattern);
	errno = 0;
	ret = system(buf);
#if 0
	perror(match_text);
	printf("status %d %d\n",WIFEXITED(ret), WEXITSTATUS(ret));
#endif
	if (WEXITSTATUS(ret) == 1) {
		return True;
	}
	return False;
}

static NhlBoolean MatchPattern
(
	AppData	  appdata,
	int	  dcount,
	NrmQuark  qfile,
	NrmQuark  qvar
)
{
	AppDataRes	datares;
	NhlString	*patterns = NULL;
	int		pat_count = 0;
	_NhlConvertContext context = NULL;
	int i;
	NhlBoolean matched = False;
	
	for (datares = appdata->datares; datares; datares = datares->next) {
		if (! datares->is_coord && 
		    strstr(NrmQuarkToString(datares->qres),"Pattern")) {
			context = StringResToStringArray
				(datares->value,
				 &pat_count,&patterns,context);
			break;
		}
	}
	if (! pat_count)	/* no pattern specified -- match is True */
		matched = True;
	else {
		for (i = 0; i < pat_count; i++) {
			/*
			 * calling perl: this will be replaced with a lib call 
			 */
			if (PerlMatch(patterns[i],NrmQuarkToString(qvar))) {
				matched = True;
				break;
			}
		}
	}
	if (context)
		_NhlFreeConvertContext(context);
	return matched ? True : False;
}

static NhlBoolean AlreadyAssigned
(
	int 		dcount,
	NrmQuark	qfile,
	NrmQuark	qvar
)
{
	int i;

	for (i = 0; i < dcount; i++)
		if (Data_Table[i].qvar == qvar && 
		    Data_Table[i].qfile == qfile)
			return True;

	return False;
}

#define MIN_ELEMENTS 3
static NhlBoolean MatchDimensions
(
	AppData	  appdata,
	int	  dcount
)
{
	AppDataRes	datares;
	DataTable	dt = &Data_Table[dcount];
	NhlString	patterns[10] = { NULL,NULL,NULL,NULL,NULL,
					 NULL,NULL,NULL,NULL,NULL };
	NhlBoolean	matched[10] = { False, False, False, False, False,
				        False, False, False, False, False };
	int		i,j,ix;
	NclApiVarInfoRec *vinfo;

	vinfo = dt->dl->u.var;

	for (datares = appdata->datares; datares; datares = datares->next) {
		if (datares->is_coord && 
		    strstr(NrmQuarkToString(datares->qres),"Pattern")) {
			if (datares->coord_ix < 0)
				ix = abs(datares->coord_ix) - 1;
			else 
				ix = datares->coord_ix;
			if (ix > 9)
				continue;
			patterns[ix] = datares->value;
		}
	}
	for (i = 0; i < appdata->ndims; i++) {
		if (patterns[i]) {
			for (j = vinfo->n_dims - 1; j >=0; j--) {
				if (! matched[j] && 
				    vinfo->dim_info[j].dim_quark > 
				    NrmNULLQUARK) {
					if (PerlMatch
					    (patterns[i],NrmQuarkToString
					     (vinfo->dim_info[j].dim_quark))) {
						dt->qdims[i] = 
						  vinfo->dim_info[j].dim_quark;
						matched[j] = True;
						dt->dim_ix[i] = j;
						break;
					}
				}
			}
		}
	}
	for (i = 0; i < appdata->ndims; i++) {
		if (! dt->qdims[i] > NrmNULLQUARK) {
			if (patterns[i])

				return False;
			for (j = vinfo->n_dims - 1; j >=0; j--) {
				if (! matched[j]) {
					dt->dim_ix[i] = j;
					dt->qdims[i] = 
						vinfo->dim_info[j].dim_quark;
					matched[j] = True;
					break;
				}
			}
		}
	}
	
	return True;
}


static NhlBoolean MatchUnits
(
	AppData	  appdata,
	int	  dcount
)
{
	return True;
}

static NhlBoolean MatchAttributes
(
	AppData	  appdata,
	int	  dcount
)
{
	return True;
}

static NhlBoolean GetInfo
(
	DataTable dt,
	NgVarData vdata
)
{

	if (dt->dl) {
		if (dt->free_dl) {
			NclFreeDataList(dt->dl);
		}
		dt->dl = NULL;
	}
	if (vdata->dl) {
		dt->dl = vdata->dl;
		dt->free_dl = False;
	}
	else if (dt->qfile && dt->qvar) {
		dt->dl = NclGetFileVarInfo(dt->qfile,dt->qvar);
		dt->free_dl = True;
	}
	else if (dt->qvar) {
		dt->dl = NclGetVarInfo(dt->qvar);
		dt->free_dl = True;
	}
	else {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
		return False;
	}
	return True;
}
static NgVarData MatchVarData
(
	PlotApp		papp,
	AppData		appdata,
	int		dcount,
	int		count,
	NgVarData	*vdata
)
{
	AppDataRes	datares;
	NhlString	*patterns = NULL;
	int		pat_count = 0;
	_NhlConvertContext context = NULL;
	NgVarData	vd;
	int		i,j;
	DataTable	dt = &Data_Table[dcount];

	if (! (count && vdata))
		return False;

	dt->appdata = appdata;

	for (i = 0; i < count; i++) {
		if (vdata[i]->ndims < appdata->ndims)
			continue;
		if (AlreadyAssigned(dcount,vdata[i]->qfile,vdata[i]->qvar))
			continue;
			
		if (! MatchPattern(appdata,
				   dcount,vdata[i]->qfile,vdata[i]->qvar))
			continue;
		dt->qfile = vdata[i]->qfile;
		dt->qvar = vdata[i]->qvar;
		if (! GetInfo(dt,vdata[i])) {
			dt->qvar = NrmNULLQUARK;
			return False;
		}
		if (! MatchDimensions(appdata,dcount))
			continue;
		if (! MatchUnits(appdata,dcount))
			continue;
		if (! MatchAttributes(appdata,dcount))
			continue;
		return vdata[i];
	}
	return NULL;
}

static NgVarData MatchVar
(
	PlotApp		papp,
	AppData		appdata,
	int		dcount,
	NrmQuark	*qvars
)
{
	return NULL;
}

static NgVarData MatchVarFromFile
(
	PlotApp		papp,
	AppData		appdata,
	int		dcount,
	NrmQuark	*qfiles
)
{
	return NULL;
}

static NhlBoolean MatchCoordAttr
(
	NrmQuark	qfile,
	NrmQuark	qvar,
	int		qdim,
	char		*buf,
	char		**endp
)
{
        NclApiDataList  *dl;
	NclApiVarInfoRec *vinfo;
	char tbuf[256];
	char *cp;
	int i;

/*
 * The end pointer is already set. It should only be modified if the 
 * attribute match fails and therefore the attribute name needs to
 * be wiped out of the resource value.
 */
 	
	if (qfile > NrmNULLQUARK)
		dl = NclGetFileVarCoordInfo(qfile,qvar,qdim);
	else
		dl = NclGetVarCoordInfo(qvar,qdim);

	strncpy(tbuf,buf,255);
	cp = tbuf;
	if (! (isalpha(*cp) || *cp == '_')) {
		/*
		 * in this situation the attribute name is not valid. 
		 * all that's getting 'wiped' is the '@' symbol. An
		 * ncl syntax error is likely. Should more of the string
		 * be eliminated?
		 */
		*endp = buf; 
		return False;
	}
	else {
		cp++;
		while (isalnum(*cp) || *cp == '_')
			cp++;
		*cp = '\0';
		if (!dl) {
			*endp = buf + (cp - tbuf);
			return False;
		}
		vinfo = dl->u.var;
		for (i = 0; i < vinfo->n_atts; i++) {
			if (! strcmp(tbuf,
				     NrmQuarkToString(vinfo->attnames[i])))
				break;
		}
	}
	if (i == vinfo->n_atts) {
		*endp = buf + (cp - tbuf);
		return False;
	}
	return True;
}

static void WriteCoords
(
	char 		*buf,
	NclApiVarInfoRec *vinfo,
	DataTable	dt,
	NgVarData	vd
)
{
	int i,coord_ix = dt->appdata->ndims - 1;
	char *cp;
		
	sprintf(buf,"(");
	for (i = 0; i < vinfo->n_dims; i++) {
		cp = &buf[strlen(buf)];
		if (i == dt->dim_ix[coord_ix]) {
			if (vd->start[i] == 0 &&
			     vd->finish[i] == vinfo->dim_info[i].dim_size - 1 
			     && vd->stride[i] == 1) {
					sprintf(cp,":,");
			}
			else {
				sprintf(cp,"%d:%d:%d,",
					vd->start[i],vd->finish[i],
					vd->stride[i]);
			}
			coord_ix--;
		}
		else { /* one element only is accepted */
			sprintf(cp,"%d,",vd->start[i]);
		}
	}
	buf[strlen(buf)-1] = ')';
	return;
}

static void WriteReorderedCoords
(
	char 		*buf,
	NclApiVarInfoRec *vinfo,
	DataTable	dt,
	NgVarData	vd
)
{
	int i,j;
	char *cp;
	int dim_ix;
	NrmQuark qdim;
/*
 * this takes two passes through the dimensions:
 * unreferenced dimensions will be the slow ones and have only one element.
 */
		
	sprintf(buf,"(");
	for (i = 0, dim_ix = 0; i < vinfo->n_dims; i++) {
		cp = &buf[strlen(buf)];
		for (j = 0; j < dt->appdata->ndims; j++) {
			if (i == dt->dim_ix[j])
				break;
		}
		if (j < dt->appdata->ndims) {
			/* dim found in the Data Table -worry about it later */
			continue;
		}
		qdim = vinfo->dim_info[i].dim_quark;
		if (qdim <= NrmNULLQUARK) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		   "Variable %s unnamed dimension: reordering not possible",
				   NrmQuarkToString(dt->qvar)));
			return;
		}
		 /* one element only is accepted */

		sprintf(cp,"%s | %d,",NrmQuarkToString(qdim),vd->start[i]);
		dim_ix++;
	}
/*
 * If the vdata subsection has never been set explicitly, then it will be
 * wrong when dimensions are going to be reordered based on name. In this
 * case, we cheat a little, by checking the range of the
 * fast moving dimensions instead of range of the dimension that
 * we are interested in. Sound confusing? It is. If not explicitly set (in
 * the varpage at least), it will have the set_state _NgDEFAULT_SHAPE. 
 */
	for (i = dt->appdata->ndims - 1; i >=0; i--) {
		int ix = dt->dim_ix[i];

		cp = &buf[strlen(buf)];
		qdim = vinfo->dim_info[ix].dim_quark;
		if (qdim <= NrmNULLQUARK) { 
			/* shouldn't be possible here, but check anyway */
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		   "Variable %s unnamed dimension: reordering not possible",
				   NrmQuarkToString(dt->qvar)));
			return;
		}
 		if (vd->start[ix] == 0 &&
		     vd->finish[ix] == vinfo->dim_info[ix].dim_size - 1 &&
		     vd->stride[ix] == 1) {
			sprintf(cp,"%s | :,",NrmQuarkToString(qdim));
		}
		else {
			sprintf(cp,"%s | %d:%d:%d,",NrmQuarkToString(qdim),
				vd->start[ix],vd->finish[ix],vd->stride[ix]);
		}
		dim_ix++;
	}
	buf[strlen(buf)-1] = ')';
	return;
}

static void ReplaceDataSymRef
(
	PlotApp		papp,
	AppObjRes	res,
	int		index,
	char		*buf,
	AppResSymRef	sref
)
{
	char		tbuf[256];
	DataTable	dt = &Data_Table[index];
	int		coord_ix,n_to_move;
	char		*cp,*sp,*ep;
	NclApiVarInfoRec *vinfo;
	int 		i;
	NrmQuark	qdim;
	NgVarData	vd;

	if (! dt->qvar) /* this data is not specified yet */
		return;

	if (! dt->dl) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Internal error: no var info"));
		return;
	}
		
	vinfo = dt->dl->u.var;
	vd = &dt->vd_rec;
	switch (sref->rtype) {
	case _NgREF_ATTR:
		/* first see if the attribute named is actually an
		 * attribute of this var
		 * the attribute starts beyond the char count by 2
		 */
		cp = &buf[sref->offset + sref->count + 2];
		strncpy(tbuf,cp,255);
		cp = tbuf;
		if (! (isalpha(*cp) || *cp == '_')) {
			i = vinfo->n_atts;
			*cp = '\0';
		}
		else {
			cp++;
			while (isalnum(*cp) || *cp == '_')
				cp++;
			*cp = '\0';
			for (i = 0; i < vinfo->n_atts; i++) {
				if (! strcmp(tbuf,
				     NrmQuarkToString(vinfo->attnames[i])))
					break;
			}
		}
		if (i == vinfo->n_atts) {
			/* replace with empty string */
			sp = &buf[sref->offset+sref->count + strlen(tbuf)+2];
			sprintf(tbuf,"\"\"");
		}
		else if (dt->qfile) {
			sprintf(tbuf,"%s->%s",
				NrmQuarkToString(dt->qfile),
				NrmQuarkToString(dt->qvar));
			sp = &buf[sref->offset + sref->count + 1];
		}
		else {
			sprintf(tbuf,"%s",
				NrmQuarkToString(dt->qvar));
			sp = &buf[sref->offset + sref->count + 1];
		}
		break;
	case _NgREF_COORD:
		/*
		 * the coordinate index is beyond the char count by 2
		 */
		cp = &buf[sref->offset + sref->count + 2];

		coord_ix = strtol(cp,&sp,10);
		if (! (sp > cp)) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"Syntax error in res file coord index specification"));
			return;
		}
		/*
		 * the DataTable is stored in fast to slow order
		 */
		if (coord_ix >= 0) {
			coord_ix = vinfo->n_dims - coord_ix - 1;
		}
		else {
			coord_ix = -coord_ix - 1;
		}
		if (coord_ix < 0 || coord_ix > 4) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Invalid coord index in res file specification"));
			return;
		}
		qdim = dt->qdims[coord_ix];
		
		/* check for coordinate attribute */
		if (*sp == '@') { 
			if (!(qdim > NrmNULLQUARK && 
			      MatchCoordAttr(dt->qfile,dt->qvar,
					     qdim,sp+1,&sp))) {
				sprintf(tbuf,"\"\"");
				break;
			}
		}
		if (qdim > NrmNULLQUARK) {
			/* 
			 * This might be just a named dimension or it
			 * might actually be a coord var, so find out.
			 */
			for (i = 0; i < vinfo->n_dims; i++) {
				if (qdim == vinfo->coordnames[i])
					break;
			}
			if (i == vinfo->n_dims)
				qdim = NrmNULLQUARK;
		}
		if (qdim > NrmNULLQUARK) {
			if (dt->qfile) {
				sprintf(tbuf,"%s->%s",
					NrmQuarkToString(dt->qfile),
					NrmQuarkToString(qdim));
			}
			else {
				sprintf(tbuf,"%s&%s",
					NrmQuarkToString(dt->qvar),
					NrmQuarkToString(qdim));
			}
			if (vd->start[i] == 0 && 
			    vd->finish[i] == vinfo->dim_info[i].dim_size - 1 &&
			    vd->stride[i] == 1) {
				sprintf(&tbuf[strlen(tbuf)],"(:)");
			}
			else {
				sprintf(&tbuf[strlen(tbuf)],"(%d:%d:%d)",
					vd->start[i],vd->finish[i],
					vd->stride[i]);
			}
		}
		else {
			tbuf[0] = '\0';
		}
		break;
	case _NgREF_REGULAR:
		if (dt->qfile) {
			sprintf(tbuf,"%s->%s",
				NrmQuarkToString(dt->qfile),
				NrmQuarkToString(dt->qvar));
		}
		else {
			sprintf(tbuf,"%s",
				NrmQuarkToString(dt->qvar));
		}
		if (! dt->reorder) {
 			WriteCoords(&tbuf[strlen(tbuf)],vinfo,dt,vd);
		}
		else {
 			WriteReorderedCoords(&tbuf[strlen(tbuf)],vinfo,dt,vd);
		}
		sp = &buf[sref->offset + sref->count + 1];
		break;
		
	default:
		break;
	}

	ep = &buf[sref->offset + strlen(tbuf)];
	n_to_move = strlen(sp);
	memmove(ep,sp,n_to_move);
	*(ep+n_to_move) = '\0';

	strncpy(&buf[sref->offset],tbuf,strlen(tbuf));

	return;
}

static void ReplaceObjSymRef
(
	PlotApp		papp,
	AppObjRes	res,
	AppObject 	appobj,
	char		*buf,
	AppResSymRef	sref,
	NhlString	plotname
)
{
	char		tbuf[256];
	int		i,ix = -1;
	char		*cp,*sp,*ep;
	int		n_to_move;

	/* 
	 * Find the array index for this graphic object, in the 
	 * dependency ordered list (which will be the order the 
	 * objects actually get created in)
	 */
	for (i = 0; i < papp->obj_count; i++) {
		if (papp->qobj_deps[i] == appobj->qbasename)
			ix = i;
	}
	if (ix == -1) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return;
	}
	sprintf(tbuf,"%s_%s",plotname,NrmQuarkToString(papp->qobj_deps[ix]));

	sp = &buf[sref->offset + sref->count + 1];
	ep = &buf[sref->offset + strlen(tbuf)];
	n_to_move = strlen(sp);
	memmove(ep,sp,n_to_move);
	*(ep+n_to_move) = '\0';

	strncpy(&buf[sref->offset],tbuf,strlen(tbuf));

	return;
}
static void ReplaceSymRef
(
	PlotApp		papp,
	AppObjRes	res,
	char		*buf,
	AppResSymRef	sref,
	NhlString	plotname
)
{
	char tbuf[256];
	int  i;

	if (sref->kind == _NgDATA_REF) {
		for (i = 0; i < papp->data_count; i++) {
			if (sref->sym.d == Data_Table[i].appdata) {
				ReplaceDataSymRef(papp,res,i,buf,sref);
			}
		}
	}
	else {
		AppObject appobj;
		for (appobj = papp->objects; appobj; appobj = appobj->next) {
			if (sref->sym.o == appobj) {
				ReplaceObjSymRef(papp,res,
						 appobj,buf,sref,plotname);
			}
		}
	}
	
	return;
}

static void SubstituteVarSyms
(
	PlotApp		papp,
	AppData		appdata,
	NhlString	plotname,
	NgDataProfile	dprof
)
{
	int i;

	for (i = 0; i < dprof->n_dataitems; i++) {
		char buf[512];
		NgDataItem ditem = dprof->ditems[i];
		AppObjRes res = (AppObjRes)ditem->appres_info;
		AppResSymRef sref;

		if (! res)
			continue;
		strcpy(buf,res->value);

		for (sref = res->symrefs; sref; sref = sref->next) {
			ReplaceSymRef(papp,res,buf,sref,plotname);
		}
		NgSetExpressionVarData(papp->go_id,ditem->vdata,buf,False);
	}
		
	return;
}

static void EvaluateDataProfileVars
(
	PlotApp		papp,
	AppData		appdata,
	NgDataProfile	dprof
)
{
	return;
}

/*
 * the DataTable is a static structure reinitialized everytime a DataProfile
 * instance is evaluated. 
 */
static void InitializeDataTable
(
	int count
)
{
	if (count > DataTableAllocCount) {
		Data_Table = NhlRealloc
			(Data_Table,count * sizeof(DataTableRec));
		if (! Data_Table) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}

		DataTableAllocCount = count;
	}
	memset(Data_Table,(char) 0,count * sizeof(DataTableRec));
	return;
}

NhlBoolean NgPlotAppDataUsable
(
	int		go_id,
	NrmQuark	qplotstyle,
	NgVarData	vdata
)
{
	PlotApp	papp = PlotAppList;
	NgGO		go = (NgGO) _NhlGetLayer(go_id);
	AppData		data;
	int		dcount;
	

	if (! go)
		return NhlFATAL;

	while (papp) {
		if (papp->qname == qplotstyle) {
			break;
		}
		papp = papp->next;
	}
	if (! papp)
		return NhlFATAL;
/*
 * Right now we're just check to see that at least one data reference 
 * is looking for a data var with number of dimensions less than or
 * equal to the number of dimensions in the data var under test.
 * But we also need to check that the var meets pattern and unit and
 * whatever other requirements come up.
 */
	for (data = papp->data; data; data = data->next) {
		if (data->ndims <= vdata->ndims)
			return True;
	}
	return False;
}

NhlErrorTypes InitializePlotDataRecs
(
	NgPlotData	plotdata,
	NrmQuark	qname,
	NhlString	description,
	NhlBoolean	required,
	int		ndims
)
{

	plotdata->qname = qname;
	plotdata->description = description;
	plotdata->required = required;
	plotdata->ndims = ndims;
	plotdata->vdata = NgNewVarData();
	if (! plotdata->vdata)
		return NhlFATAL;

	return NhlNOERROR;
}

void NgFreePlotDataRecs
(
	NgPlotData	plotdata,
	int		count
)
{
	int i;

	for (i = 0; i < count; i++) 
		NgFreeVarData(plotdata[i].vdata);

	NhlFree(plotdata);

	return;
}
					    
NhlErrorTypes NgUpdatePlotAppDataProfile
(
	int		go_id,
	NrmQuark	qplotstyle,
	NhlString	plotname,
	NgDataProfile	dprof
)
{
	PlotApp	papp = PlotAppList;
	NgGO		go = (NgGO) _NhlGetLayer(go_id);
	AppData		data;
	int		dcount;
	NgPlotData 	plotdata;

	if (! go)
		return NhlFATAL;

	while (papp) {
		if (papp->qname == qplotstyle) {
			break;
		}
		papp = papp->next;
	}
	if (! papp)
		return NhlFATAL;
	return NhlNOERROR;
}

static NhlBoolean DimReorderRequired
(
	DataTable dt
)
{
	int i;

	for (i = 1; i < dt->appdata->ndims; i++) {
		if (dt->dim_ix[i] > dt->dim_ix[i-1])
			return True;
	}
	return False;
}

static void TransferVarData
(
	DataTable dt,
	NgVarData vdata
)
{
	NclApiVarInfoRec *vinfo = dt->dl->u.var;
	int i,j,coord_ix = dt->appdata->ndims - 1;
	NgVarData vd = &dt->vd_rec;

	dt->reorder = DimReorderRequired(dt);

	NgCopyVarData(vd,vdata);
/*
 * If the vdata has a default shape, ignore the shape set in the vdata
 * and base the shape on the expected dimension names.
 * Otherwise the vdata shape prevails.
 */

	if (vdata->set_state != _NgDEFAULT_SHAPE)
		return;

	if (! dt->reorder) {
		for (i = 0; i < vinfo->n_dims; i++) {
			vd->start[i] = 0;
			vd->stride[i] = 1;
			if (i != dt->dim_ix[coord_ix]) {
				vd->finish[i] = vd->start[i];
				continue;
			}
			vd->finish[i] = vinfo->dim_info[i].dim_size - 1;
			coord_ix--;
		}
		return;
	}
	for (i = 0; i < vinfo->n_dims; i++) {
		vd->start[i] = 0;
		vd->stride[i] = 1;

		for (j = 0; j < dt->appdata->ndims; j++) {
			if (i == dt->dim_ix[j])
				break;
		}
		if (j == dt->appdata->ndims) {
			/* not found */
			vd->finish[i] = vd->start[i];
			continue;
		}
		vd->finish[i] = vinfo->dim_info[i].dim_size - 1;
	}
}

/*
 * This function is designed to set up the data profile given a variety of
 * kinds of input. If NgVarData records are supplied, they are used first to
 * try to match all the data symbols in the plot style. Otherwise, first
 * the single vars are used (the default slice will be taken). Last the
 * file list is examined.
 */

NhlErrorTypes NgSetPlotAppDataVars
(
	int		go_id,
	NrmQuark	qplotstyle,
	NhlString	plotname,
	NgDataProfile	dprof,
	NrmQuark	*qfiles,	/* NULL-terminated list of files */
	NrmQuark	*qvars,		/* NULL-terminated list of vars */
	int		varcount,
	NgVarData	*vardata,
	NrmQuark	qgraphics	/* graphic obj array - not used */
)
{
	PlotApp	papp = PlotAppList;
	NgGO		go = (NgGO) _NhlGetLayer(go_id);
	AppData		data;
	int		dcount;
	int		init_plot_data = False;
	NgVarData	vdata;
	

	if (! go)
		return NhlFATAL;

	while (papp) {
		if (papp->qname == qplotstyle) {
			break;
		}
		papp = papp->next;
	}
	if (! papp)
		return NhlFATAL;
	
	InitializeDataTable(papp->data_count);
	if (! dprof->plotdata) {
		dprof->plotdata = 
			NhlMalloc(papp->data_count * sizeof(NgPlotDataRec));
		if (! dprof->plotdata) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NhlFATAL;
		}
		dprof->plotdata_count = papp->data_count;
		init_plot_data = True;
	}

	dcount = 0;
	for (data = papp->data; data; data = data->next) {
		NgPlotData pdata = &dprof->plotdata[dcount];
		DataTable dt = &Data_Table[dcount];
		NhlBoolean free = False;

		if (init_plot_data)
			InitializePlotDataRecs
				(pdata,data->qdataname,data->description,
				 data->required,data->ndims);
		vdata = MatchVarData(papp,data,dcount,varcount,vardata);

		if (! vdata) {
			vdata = MatchVar(papp,data,dcount,qvars);
			free = True;
		}
		if (! vdata) {
			vdata = MatchVarFromFile(papp,data,dcount,qfiles);
			free = True;
		}
		
		if (vdata) {
			TransferVarData(dt,vdata);
			NgCopyVarData(pdata->vdata,&dt->vd_rec);
			if (free)
				NgFreeVarData(vdata);
		}
		dcount++;
	}
	SubstituteVarSyms(papp,data,plotname,dprof);
#if 0
	EvaluateDataProfileVars(papp,data,dprof);
#endif

	return NhlNOERROR;
}

static NhlErrorTypes HandleOverlayRes
(
	PlotApp		papp,
	NrmQuark	qplot,
	NgDataItem	ditem,
	NhlBoolean	preview
)
{
	NrmQuark		qobj = ditem->qhlu_name;
	_NhlConvertContext	context;
	int 			count;
	NhlString		*objects;
	int			i;
	char			ncl_name[256];
	NgGO			go = (NgGO)_NhlGetLayer(papp->go_id);

	if (preview) {
		/* not handled yet */
		return NhlNOERROR;
	}
	if (ditem->vdata->set_state != _NgEXPRESSION)
		return NhlNOERROR;

	if (! go)
		return NhlFATAL;

	context = StringResToStringArray
		(ditem->vdata->expr_val,&count,&objects,NULL);
	
	if (! context) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NhlFATAL;
	}

	sprintf(ncl_name,"%s_%s",
		NrmQuarkToString(qplot),
		NrmQuarkToString(qobj));

	for (i = 0; i < count; i++) {
		char buf[512];

		sprintf(buf,"overlay(%s,%s)\n",ncl_name,objects[i]);
		(void)NgNclSubmitBlock(go->go.nclstate,buf);
	}
		
	return NhlNOERROR;
}

static NhlErrorTypes HandleDataLimitObjectRes
(
	PlotApp		papp,
	NrmQuark	qplot,
	NgDataItem	ditem,
	NgResData	resdata,
	int		*res_count,
	NhlBoolean	preview
)
{
	NrmQuark		qobj = ditem->qhlu_name;
	int			i;
	char			ncl_name[256];
	NgGO			go = (NgGO)_NhlGetLayer(papp->go_id);
	float			xmin,ymin,xmax,ymax;
	char			buf[32];
	char			*cp;
	NgVarData		vdata = ditem->vdata;
#if 0
	NhlString	res_names[] = {
		"mpLimitMode","mpRelativeCenterLon",
		"mpMinLonF","mpMaxLonF","mpMinLatF","mpMaxLatF" };
#endif
	NhlString	res_names[] = {
		"mpLimitMode",
		"mpMinLonF","mpMaxLonF","mpMinLatF","mpMaxLatF" };
	int hlu_id,count,*id_array;

	if (preview) {
		/* not handled yet */
		return NhlNOERROR;
	}
	if (ditem->vdata->set_state != _NgEXPRESSION)
		return NhlNOERROR;

	if (! go)
		return NhlFATAL;

	if (! (vdata->set_state == _NgEXPRESSION && vdata->expr_val))
		return NhlFATAL;

	hlu_id = NgNclGetHluObjId(go->go.nclstate,vdata->expr_val,&count,
				  &id_array);

	if (hlu_id <= NhlNULLOBJID) {
		return NhlFATAL;
	}
	NhlVAGetValues(hlu_id,
		       "trXMinF",&xmin,
		       "trXMaxF",&xmax,
		       "trYMinF",&ymin,
		       "trYMaxF",&ymax,
		       NULL);

	if (*res_count + 6 >= resdata->res_alloced) {
		resdata = NgReallocResData(resdata,*res_count+6);
		if (! resdata)
			return NhlFATAL;
	}
	for (i = 0; i < 5; i++) {
		resdata->res[*res_count] = res_names[i];
		resdata->types[*res_count] = QString;

		switch (i) {
		case 0:
			sprintf(buf,"\"LatLon\"");
			break;
		case 1:
			sprintf(buf,"%f",xmin);
			break;
		case 2:
			sprintf(buf,"%f",xmax);
			break;
		case 3:
			sprintf(buf,"%f",ymin);
			break;
		case 4:
			sprintf(buf,"%f",ymax);
			break;
		}
		cp = NhlMalloc(strlen(buf)+1);
		strcpy(cp,buf);
		resdata->values[*res_count] = cp;
		(*res_count)++;
	}

	return NhlNOERROR;
}

extern NhlErrorTypes NgHandleSyntheticResource
(
	int		go_id,
	NrmQuark	qplotstyle,
	NrmQuark	qplot,
	NgDataProfile	dprof,
	int		obj_ix,
	int		item_ix,
	NgResData	resdata,
	int		*res_count,
	NhlBoolean	preview
)
{
	PlotApp		papp = PlotAppList;
	NgDataItem 	ditem = dprof->ditems[item_ix];
	NrmQuark   	qobj = dprof->qobjects[obj_ix];
	NrmQuark   	qres = ditem->resq;

	while (papp) {
		if (papp->qname == qplotstyle) {
			break;
		}
		papp = papp->next;
	}
	if (! papp) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "NgHandleSyntheticResource: invalid plot style"));
		return NhlFATAL;
	}

	if (qres == QpmOverlays)
		return HandleOverlayRes(papp,qplot,ditem,preview);
	else if (qres == QmpDataLimitObject)
		return HandleDataLimitObjectRes
			(papp,qplot,ditem,resdata,res_count,preview);
	
	NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		   "NgHandleSyntheticResource: invalid synthetic resource"));

	return NhlFATAL;
}

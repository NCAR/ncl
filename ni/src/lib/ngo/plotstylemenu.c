/*
 *      $Id: plotstylemenu.c,v 1.3 1999-09-29 02:06:01 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotstylemenu.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jun  6 17:26:33 MDT 1997
 *
 *	Description:	
 */

#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

#include <ncarg/ngo/plotstylemenuP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/plotpage.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/varpage.h>
#include <ncarg/ngo/plotapp.h>
#include <ncarg/ngo/graphic.h>
#include <ncarg/hlu/XWorkstation.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <ncarg/ngo/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PushBG.h>
#include <Xm/TextF.h>
#include  <Xm/Form.h>
#include  <Xm/LabelG.h>
#include <Xm/SeparatoG.h>
#include <Xm/ToggleBG.h>
#include <Xm/Frame.h>


#define _NgTOPPATH "Plot Styles"
#define _NgDEFAULT_PLOTSTYLE_PATH "$NCARG_ROOT/lib/ncarg/plot_styles"
#define _NgRESFILE_EXT ".res"
#define _NgPRIVATE_PREFIX "_Ng"
#define _NgDIRINFOFILE_NAME "DIRECTORY" _NgRESFILE_EXT

#define RES_STYLENAME		"*ndvPlotStyleName"
#define RES_CLASS		"*ndvPlotClass"
#define RES_PLOTNAME		"*ndvPlotName"
#define RES_DIRECTORY_TITLE	"*ndvDirectoryTitle"


typedef struct _NgDefSymbol {
	NhlString class_name;
	NhlString def_symbol;
} NgDefSymbolRec, *NgDefSymbol;

static NgDefSymbolRec Def_Symbols[] = {
	{  NGPLOTCLASS, "plot" },
	{ "contourPlotClass", "cn_plot" },
	{ "streamlinePlotClass", "st_plot" },
	{ "vectorPlotClass", "vc_plot" },
	{ "xyPlotClass", "xy_plot" },
	{ "coordArraysClass", "ca_data" },
	{ "scalarFieldClass", "sf_data" },
	{ "vectorFieldClass",  "vf_data"} };


typedef struct _NgPlotStyleRec {
	NhlString 	pstyle;
	NhlString	name;
	NhlString	class_name;
	NhlString	plot_name;
	NhlString	path;
} NgPlotStyleRec, *NgPlotStyle;

typedef struct _NgPathRec NgPathRec, *NgPath;

typedef NgPath (*NgProcessEntry)(
	NgPath		path 
);

struct _NgPathRec {
	NgPath		next;
	NgPath		subpaths;
	int		subpathcount; /* count of the subpaths */
	NgPath		*subpathptrs; /* array of ptrs to each subpath node */
	NhlString	dirpath;
	NhlString	name;
	NgProcessEntry	process;
	NhlPointer	data;
};

typedef void (*PathFunc) 
(
	NgPath		path,
	int		level,
	NhlPointer	path_func_data
);

NgPlotStyle	PlotStyles = NULL;


NgPlotStyleRec  VarPlotStyle = { NULL, NULL, NULL, "ncl_var",NULL };
NgPathRec	VarPath = { NULL,NULL,0,NULL,NULL,NULL,NULL,&VarPlotStyle };

static NgPath	PlotStylePaths = NULL;
static NgPath   PlotStylePathTop = NULL;
static int 	MaxLevel;

static void PlotMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
);

static NhlString GetDefaultSymbol
(
        NhlString       class_name
        )
{
	int i;
	NhlString ret_str;

	
	for (i = 0; i < NhlNumber(Def_Symbols); i++) {
		if (!strcmp(class_name,Def_Symbols[i].class_name)) {
			ret_str = NhlMalloc(
				strlen(Def_Symbols[i].class_name) + 1);
			strcpy(ret_str,Def_Symbols[i].def_symbol);
			return(ret_str);
		}
	}
	return NULL;
}

static void FreePath
(
	NgPath path
)
{
	if (path->subpaths)
		FreePath(path->subpaths);
	if (path->next)
		FreePath(path->next);

	if (path->name)
		NhlFree(path->name);
	if (path->dirpath)
		NhlFree(path->dirpath);

	NhlFree(path);

	return;
}

static void FreePlotStyle
(
	NgPlotStyle pstyle
)
{
	if (! pstyle)
		return;

	if (pstyle->name && pstyle->name != pstyle->pstyle)
		NhlFree(pstyle->name);
	if (pstyle->class_name)
		NhlFree(pstyle->class_name);
	if (pstyle->pstyle)
		NhlFree(pstyle->pstyle);
	if (pstyle->path)
		NhlFree(pstyle->path);

	NhlFree(pstyle);

	return;
}

FILE	*OpenResFile
(
	NhlString	dirpath,
	NhlString	name
)
{
	char		*fullpath;
	FILE		*fp;

	if (! dirpath) {
		fp = fopen(name,"r");
		if (! fp) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "Error opening file: %s",name));
			return NULL;
		}
		return fp;
	}


	fullpath = NhlMalloc(strlen(dirpath) + strlen(name) + 2);
	if (! fullpath) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	sprintf(fullpath,"%s/%s",dirpath,name);
	
	fp = fopen(fullpath,"r");
	if (! fp) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Error opening file: %s",fullpath));
		return NULL;
	}
		
	NhlFree(fullpath);

	return fp;
}

static void ProcessDirInfoFile
(
	NgPath		path
)
{
	char *cp;
	static int dirtitle_len = -1;
	char buf[256];
	FILE *fp;

	if (dirtitle_len < 0) {
		dirtitle_len = strlen(RES_DIRECTORY_TITLE);
	}
	if (! path->dirpath) {
		fp = OpenResFile(path->name,_NgDIRINFOFILE_NAME);
	}
	else {
		char *dirpath = NhlMalloc
			(strlen(path->dirpath) + strlen(path->name) + 2);
		if (! dirpath) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		sprintf(dirpath,"%s/%s",path->dirpath,path->name);
		fp = OpenResFile(dirpath,_NgDIRINFOFILE_NAME);
	}
	if (! fp)
		return;

	while (cp = fgets(buf,255,fp)) {
		char *title,*np;

		while (np = strrchr(cp,'\n'))
			*np = '\0';
		while (isspace(*cp))
			cp++;
		if (*cp == '!')
			continue;
		if (! strncmp(cp,RES_DIRECTORY_TITLE,dirtitle_len)) {
			np = cp + dirtitle_len;
			while (*np == ':' || isspace(*np))
				np++;
			if (! *np) 
				continue;
			title = NhlMalloc(strlen(np)+1);
			strcpy(title,np);
			np = &title[strlen(title)-1];
			while (isspace(*np)) {
				*np = '\0';
				np--;
			}
			path->data = (NhlPointer) title;
		}
	}
}

static NgPath ProcessEntry
(
	NgPath		path
)
{
	char *cp;
	static int	stylename_len, class_len, plotname_len = -1;
	static int	private_prefix_len;
	FILE 		*fp;
	NgPlotStyle	pstyle = NULL;
	char		buf[256];

		
	if (plotname_len < 0) {
		plotname_len = strlen(RES_PLOTNAME);
		stylename_len = strlen(RES_STYLENAME);
		class_len = strlen(RES_CLASS);
		private_prefix_len = strlen(_NgPRIVATE_PREFIX);
	}
		
	if (! (path && path->name))
		goto forget_it;
	if (! strncmp(path->name,_NgPRIVATE_PREFIX,private_prefix_len))
		goto forget_it;
	
	cp = strrchr(path->name,'.');
	if (! cp)
		goto forget_it;
	if (strcmp(cp,_NgRESFILE_EXT))
		goto forget_it;

	fp = OpenResFile(path->dirpath,path->name);
	if (! fp) {
		goto forget_it;
	}

	pstyle = NhlMalloc(sizeof(NgPlotStyleRec));
	pstyle->name = NULL;
	pstyle->class_name = NULL;
	pstyle->plot_name = NULL;
	pstyle->path = NULL;
	path->data = (NhlPointer) pstyle;

	while (cp = fgets(buf,255,fp)) {
		char *name,*np;

		while (np = strrchr(cp,'\n'))
			*np = '\0';
		while (isspace(*cp))
			cp++;
		if (*cp == '!')
			continue;
		if (! strncmp(cp,RES_STYLENAME,stylename_len)) {
			np = cp + stylename_len;
			while (*np == ':' || isspace(*np))
				np++;
			if (! *np) 
				continue;
			name = NhlMalloc(strlen(np)+1);
			strcpy(name,np);
			np = &name[strlen(name)-1];
			while (isspace(*np)) {
				*np = '\0';
				np--;
			}
			pstyle->name = name;
		}
		else if (! strncmp(cp,RES_CLASS,class_len)) {
			char *class_name;

			np = cp + class_len;
			while (*np == ':' || isspace(*np))
				np++;
			if (! *np) 
				continue;
			class_name = NhlMalloc(strlen(np)+1);
			strcpy(class_name,np);
			np = &class_name[strlen(class_name)-1];
			while (isspace(*np)) {
				*np = '\0';
				np--;
			}
			pstyle->class_name = class_name;
		}
		else if (! strncmp(cp,RES_PLOTNAME,plotname_len)) {

			np = cp + plotname_len;
			while (*np == ':' || isspace(*np))
				np++;
			if (! *np) 
				continue;
			name = NhlMalloc(strlen(np)+1);
			strcpy(name,np);
			np = &name[strlen(name)-1];
			while (isspace(*np)) {
				*np = '\0';
				np--;
			}
			pstyle->plot_name = name;
		}
	}
	fclose(fp);

	if (! pstyle->class_name) {
		/*
		 * If no class name is specified the plot is
		 * assumed to be a generic plot and is 
		 * assigned the pseudo-class NGPLOTCLASS name.
		 */
		pstyle->class_name = NhlMalloc(strlen(NGPLOTCLASS)+1);
		strcpy(pstyle->class_name,NGPLOTCLASS);
	}
#if 0
	if (strcmp(pstyle->class_name,NGPLOTCLASS) &&
	    ! NgHasDataProfile(priv->go,pstyle->class_name)) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
           "No data profile associated with class name %s in %s: ignoring",
			   pstyle->class_name,
			   dirp->d_name));
		goto forget_it;
	}
#endif
	if (! pstyle->plot_name) {
		pstyle->plot_name = GetDefaultSymbol(pstyle->class_name);
	}
		
	cp = strrchr(path->name,'/');
	if (! cp)
		cp = path->name;
	strncpy(buf,cp,255);
	cp = strrchr(buf,'.');
	if (cp)
		*cp = '\0';
	pstyle->pstyle = NhlMalloc(strlen(buf)+1);
	strcpy(pstyle->pstyle,buf);
	if (! pstyle->name)
		pstyle->name = pstyle->pstyle;

	return path;

 forget_it:	
	if (pstyle)
		FreePlotStyle(pstyle);
	FreePath(path);
	return NULL;
}

	
static NgPath InitializePath
(
	const char *dirname,
	const char *pathname,
	const char *name
)
{
	NgPath		path;
	NhlString	pname;
	NhlString	dirpathname;

	if (! name)
		return NULL;

	path = NhlMalloc(sizeof(NgPathRec));
	pname =  NhlMalloc(strlen(name) + 1);
	if (! (path && pname) ) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	strcpy(pname,name);

	if (dirname && pathname) {
		dirpathname = 
			NhlMalloc(strlen(dirname) + strlen(pathname) + 2);
		if (! dirpathname) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
		sprintf(dirpathname,"%s/%s",dirname,pathname);
	}
	else if (pathname) {
		dirpathname = NhlMalloc(strlen(pathname) + 1);
		if (! dirpathname) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return NULL;
		}
		sprintf(dirpathname,"%s",pathname);
	}
	else
		dirpathname = NULL;
	
	path->dirpath = dirpathname;	
	path->name = pname;
	path->next = NULL;
	path->subpaths = NULL;
	path->process = ProcessEntry;
	path->data = NULL;
	path->subpathptrs = NULL;

	return path;
}


static NgPath ProcessPath
(
	NgPath path
)
{
        struct stat	statbuf;
	struct dirent	*dirp;  
	DIR		*dp;
	NgPath		*pathptr;
	NhlString	fullpath;

	if ( ! path)
		return NULL;

	if (path->dirpath) {
		fullpath = NhlMalloc
			(strlen(path->dirpath) + strlen(path->name) + 2);
	}
	else {
		fullpath = NhlMalloc(strlen(path->name) + 1);
	}
	if (! fullpath) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}

	if (path->dirpath) {
		sprintf(fullpath,"%s/%s",path->dirpath,path->name);
	}
	else {
		strcpy(fullpath,path->name);
	}
		
	if (stat(fullpath,&statbuf) < 0) {
		FreePath(path);
		NhlFree(fullpath);
		return NULL;
	}

	if (S_ISREG(statbuf.st_mode)) {
		path = (*path->process)(path);
		return path;
	}

	if (! S_ISDIR(statbuf.st_mode)) {
		FreePath(path);
		NhlFree(fullpath);
		return NULL;
	}
	
	if ((dp = opendir(fullpath)) == NULL) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Invalid directory encountered: %s",path));
		NhlFree(fullpath);
		return NULL;
	}
	pathptr = &path->subpaths;
	while ( (dirp = readdir(dp)) != NULL) {
		NgPath newpath;
		if (! strcmp(dirp->d_name, ".")  ||
		    ! strcmp(dirp->d_name, ".."))
			continue;
		else if (! strcmp(dirp->d_name,_NgDIRINFOFILE_NAME)) {
			ProcessDirInfoFile(path);
			continue;
		}
		newpath = InitializePath
			(path->dirpath,path->name,dirp->d_name);
		if (newpath) {
			*pathptr = ProcessPath(newpath);
			if (*pathptr)
				pathptr = &(*pathptr)->next;
		}
	}
	closedir(dp);
	NhlFree(fullpath);

	if (! path->subpaths) {
		FreePath(path);
		return NULL;
	}
	return path;
}


static void IteratePath
(
	NgPath		path,
	int		level,
	NhlBoolean	preorder,
	PathFunc	path_func,
	NhlPointer	path_func_data
)
{
	NgPath p;

	if ( ! path)
		return;
	if (level > MaxLevel)
		MaxLevel = level;
	for (p = path; p; p = p->next) {
		if (preorder)
			(*path_func)(p,level,path_func_data);
		level++;
		IteratePath(p->subpaths,
			    level,preorder,path_func,path_func_data);
		level--;
		if (! preorder)
			(*path_func)(p,level,path_func_data);
	}
	return;
}

static void IteratePathPtrs
(
	NgPath		path,
	int		level,
	NhlBoolean	preorder,
	PathFunc	path_func,
	NhlPointer	path_func_data
)
{
	int i;
	NgPath p;

	if ( ! path)
		return;
	if (level > MaxLevel)
		MaxLevel = level;
	if (preorder)
		(*path_func)(path,level,path_func_data);

	level++;
	for (i = 0; i < path->subpathcount; i++) {
		p = path->subpathptrs[i];
		IteratePathPtrs(p,level,preorder,path_func,path_func_data);
	}
	level--;

	if (! preorder)
		(*path_func)(path,level,path_func_data);

	return;
}
		    

static void CancelCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotStyleMenuRec	*priv = (PlotStyleMenuRec *)udata;
	NgPlotStyle	pstyle;

        XtVaGetValues(w,
                      XmNuserData,&pstyle,
                      NULL);

	XtDestroyWidget(priv->create_dialog);
	priv->create_dialog = NULL;

	return;
}

static void
InsufficientDimsMesg(
	Widget		w,
	NgVarData	vdata,
	NgPlotStyle	pstyle)
{		
	char message[512];
	char dname[128];

	if (vdata->qfile) {
		sprintf(dname,"%s->%s",
			NrmQuarkToString(vdata->qfile),
			NrmQuarkToString(vdata->qvar));
	}
	else {
		sprintf(dname,"%s",
			NrmQuarkToString(vdata->qvar));
	}
	sprintf(message,
		"Data var %s has insufficient dimensionality for %s plot",
		dname,pstyle->name);
				
	XmLMessageBox(w,message,True);
	return;
}


static NhlBoolean
QualifiedNclSymbol
(
	char *var_string,
	char **start,		/* location of first character */
	char **end		/* location + 1 of last character */
)
{
	char *cp = var_string;
	int len = 1;

	*start = *end = NULL;
	
	if (! (cp && strlen(cp)))
		return False;
	while (isspace(*cp))
		cp++;

	if (! (isalpha(*cp) || *cp == '_'))
		return False;
	*start = cp;
	cp++;

	while (isalnum(*cp) || *cp == '_')
		cp++,len++;

	if (len > 256)
		return False;
	*end = cp;

	while(isspace(*cp))
		cp++;

	return (*cp == '\0' ? True : False);
}

static NhlBoolean SetSelectedWorkstation
(
	PlotStyleMenuRec	*priv 
)
{
	NhlString xwktext,xwk_start,xwk_end,xwk_name;
	
	if (priv->new_xwk) {
		int hlu_id;
		XtVaGetValues(priv->new_xwk_text,
			      XmNvalue,&xwktext,
			      NULL);

		if (QualifiedNclSymbol(xwktext,&xwk_start,&xwk_end)) {
			*xwk_end = '\0';
			xwk_name = xwk_start;
		}
		else {
			char message[512];
			if (xwk_end)
				*xwk_end = '\0';
			if (xwk_start)
				xwk_name = xwk_start;
			else
				xwk_name = xwktext;
			sprintf(message,
				"\"%s\" is not a valid variable name.",
				xwk_name);
			XmLMessageBox(priv->create_dialog,message,True);
			return False;
		}
		xwk_name = NgNclGetSymName(priv->nsid,xwk_name,False);
		NgCreateGraphic(priv->go->base.id,&hlu_id,xwk_name,xwk_name,
				NULL,"xWorkstationClass",0,NULL,NULL);
		XtFree(xwktext);
	}
	else {
		Widget pb;
		NrmQuark qxwk_name;

		XtVaGetValues(priv->xwk_menu,
			      XmNmenuHistory,&pb,
			      NULL);
		XtVaGetValues(pb,
			      XmNuserData,&qxwk_name,
			      NULL);
		if (qxwk_name <= NrmNULLQUARK) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "This shouldn't happen"));
			return False;
		}
		xwk_name = NrmQuarkToString(qxwk_name);
	}
	NgAppSetSelectedWork(priv->go->go.appmgr,xwk_name);
	return True;
}
		
		
static void CreateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotStyleMenuRec	*priv = (PlotStyleMenuRec *)udata;
	NgPlotStyleMenu	*pub = &priv->public;
        NgDataProfile	prof;
	NgPlotStyle	pstyle;
        NrmQuark	qname;
        char		*vartext,*vstart,*vend;
        NgPageId	page_id;
	NgVarData	vdata = pub->vdata;
        
#if	DEBUG_PLOTSTYLEMENU
        fprintf(stderr,"in create cb\n");
#endif

        XtVaGetValues(w,
                      XmNuserData,&pstyle,
                      NULL);
		
	/* need to qualify text string, and warn user if it's already
	   a symbol */

        XtVaGetValues(priv->dialog_text,
                      XmNvalue,&vartext,
                      NULL);

	if (QualifiedNclSymbol(vartext,&vstart,&vend)) {
		*vend = '\0';
	}
	else {
		char message[512];
		char *vstr;
		if (vend)
			*vend = '\0';
		vstr = vstart ? vstart : vartext;
		sprintf(message,
			"\"%s\" is not a valid variable name.",vstr);
			XmLMessageBox(priv->create_dialog,message,True);
		XtDestroyWidget(priv->create_dialog);
		priv->create_dialog = NULL;
		XtFree(vartext);
                return;
	}

        if (! pstyle->class_name) { 
		/* 
		 * If the class is missing, it means that we're just
		 * copying the data to a variable.
		 */

		NgNclCopyShapedVar(priv->nsid,vstart,
				   vdata->qfile,vdata->qvar,vdata->ndims,
				   vdata->start,vdata->finish,vdata->stride);
		XtDestroyWidget(priv->create_dialog);
		priv->create_dialog = NULL;
		XtFree(vartext);
                return;
        }

	if (! SetSelectedWorkstation(priv)) {
		XtDestroyWidget(priv->create_dialog);
		priv->create_dialog = NULL;
		XtFree(vartext);
		return;
	}

        if (!strcmp(pstyle->class_name,NGPLOTCLASS)){
		brPlotObjCreateRec plot_create_rec;
		int app_id;
		char varname[256];
		NgPlotPage *plotpage;
                
		app_id = NgNewPlotAppRef
			(priv->go->base.id,pstyle->pstyle,pstyle->path,
			 pstyle->name,pstyle->class_name,False);
		if (! app_id) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "error referencing plot style"));
                        return;
		}
		
		if (! NgPlotAppDataUsable(priv->go->base.id,
					  NrmStringToQuark(pstyle->pstyle),
					  vdata)) {
			InsufficientDimsMesg(w,vdata,pstyle);
			NgDeletePlotAppRef(NrmStringToQuark(pstyle->pstyle));
			return;
		}

		strcpy(varname,NgNclGetSymName(priv->nsid,vstart,False));
		plot_create_rec.obj_count = 0;
		plot_create_rec.obj_ids = NULL;
		plot_create_rec.class_name = pstyle->class_name;
		plot_create_rec.plot_style = pstyle->pstyle;
		plot_create_rec.plot_style_dir = pstyle->path;
		plot_create_rec.plot_style_name = pstyle->name;
		plot_create_rec.has_input_data = True;
		plot_create_rec.state = _plotNOTCREATED;
		plot_create_rec.vdata = &vdata;
		plot_create_rec.vdata_count = 1;
		plot_create_rec.app_id = app_id;

		/*
		 * Open the page
		 */
                qname = NrmStringToQuark(varname);
                page_id = NgOpenPage
			(priv->go->base.id,_brPLOTVAR,&qname,1,
			 (NhlPointer)&plot_create_rec);
                if (page_id <= NgNoPage) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "unable to open hlu page"));
                        return;
                }
		plotpage = (NgPlotPage *)NgPageData(priv->go->base.id,page_id);
		/* 
		 * If the Create button is pressed the widget will be the
		 * create_dialog, because Create is really the OK button, and
		 * that's how motif works when OK is pressed. 
		 * If Configure is pressed the widget will be the configure
		 * push button.
		 * Call the Update function which will do a create if the
		 * Create button is pressed.
		 */
		if (w == priv->create_dialog && ! plotpage->config_required)
			NgUpdatePage(priv->go->base.id,page_id);
                
        }
	else {
		brHluObjCreateRec hlu_create_rec;
                char buf[256];
		int app_id;
                NhlString varname = NgNclGetSymName(priv->nsid,vstart,False);
		/*
		 * Create an app object for the plot style
		 */
		
		app_id = NgNewPlotAppRef
			(priv->go->base.id,pstyle->pstyle,
			 pstyle->path,pstyle->name,pstyle->class_name,False);

		prof = NgNewPlotAppDataProfile
			(priv->go->base.id,
			 NrmStringToQuark(pstyle->pstyle));

		if (prof->ditems[0]->mindims > vdata->ndims) {
			InsufficientDimsMesg(w,vdata,pstyle);
			NgDeletePlotAppRef(NrmStringToQuark(pstyle->pstyle));
			NgFreeDataProfile(prof);
			return;
		}
                    /* create the NCL graphic variable using this name now
                       in order that it won't be "stolen" before the hlu
                       object actually gets created */
                
                sprintf(buf,"%s = new(1,graphic)\n",varname);
                (void)NgNclSubmitBlock(priv->nsid,buf);

                qname = NrmStringToQuark(varname);
                page_id = NgOpenPage
			(priv->go->base.id,_brHLUVAR,&qname,1,NULL);
                if (page_id <= NgNoPage) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "unable to open hlu page"));
                        return;
                }

#if	DEBUG_PLOTSTYLEMENU
		fprintf(stderr,"%s\n",prof->class_name);
#endif

		/*
		 * Set the variable data into the data profile.
		 */

		NgSetDataProfileVar(prof,vdata,True,True);

		hlu_create_rec.obj_id = NhlNULLOBJID;
		hlu_create_rec.app_id = app_id;
		hlu_create_rec.class_name = prof->class_name;
		hlu_create_rec.plot_style = pstyle->pstyle;
		hlu_create_rec.plot_style_dir = pstyle->path;
		hlu_create_rec.has_input_data = True;
		hlu_create_rec.state = _hluNOTCREATED;
		hlu_create_rec.dprof = prof;

		NgPostPageMessage(priv->go->base.id,pub->page_id,
				  _NgVARDATALINK_REQ,_brHLUVAR,NrmNULLQUARK,
				  qname,_NgHLUOBJCREATE,
				  (NhlPointer)&hlu_create_rec,True,
				  NULL,True);
		/* 
		 * If the Create button is pressed the widget will be the
		 * create_dialog, because Create is really the OK button.
		 * If Configure is pressed the widget will be the configure
		 * push button
		 */
		if (w == priv->create_dialog)
			NgUpdatePage(priv->go->base.id,page_id);
                
        }
	XtDestroyWidget(priv->create_dialog);
	priv->create_dialog = NULL;
	XtFree(vartext);
        return;
}
static void ChooseNewOrExistingXwkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	XmToggleButtonCallbackStruct *cbs =
                (XmToggleButtonCallbackStruct*)cb_data;
	PlotStyleMenuRec	*priv = (PlotStyleMenuRec	*)udata;

	if (w == priv->exist_tgl) {
#if	DEBUG_PLOTSTYLEMENU
		fprintf(stderr,"existing: set = %d\n",cbs->set);
#endif
		XtSetSensitive(priv->xwk_optmenu,cbs->set);
		priv->new_xwk = cbs->set ? False : True;
		if (cbs->set) {
			XmProcessTraversal
				(priv->xwk_optmenu, XmTRAVERSE_CURRENT);
		}
	}
	else if (w == priv->new_tgl) {
#if	DEBUG_PLOTSTYLEMENU
		fprintf(stderr,"new: set = %d\n",cbs->set);
#endif
		XtSetSensitive(priv->new_xwk_text,cbs->set);
		priv->new_xwk = cbs->set ? True : False;
		if (cbs->set) {
			XmProcessTraversal
				(priv->new_xwk_text, XmTRAVERSE_CURRENT);
		}
	}
	else
		return;

	return;
}

static void AddXwksButtons
(
       PlotStyleMenuRec	*priv,
       Widget		parent
)
{
	int 	i,count;
	NrmQuark *qvars = NclGetHLUVarSymNames(&count);
	int	hlu_id,id_count,*id_array;
	Widget  selected = NULL;
	NhlBoolean	created;
	int	sel_xwk_id;

	if (! qvars)
		return;

	sel_xwk_id = NgAppGetSelectedWork(priv->go->go.appmgr,False,&created);
	if (!NhlIsClass(sel_xwk_id,NhlxWorkstationClass))
		sel_xwk_id = NhlNULLOBJID;

	for (i = 0; i < count; i++) {
		Widget pb;
                char *name = NrmQuarkToString(qvars[i]);

                if (! strncmp(name,"_Ng",3))
                        continue;

		hlu_id = NgNclGetHluObjId(priv->go->go.nclstate,
					  name,&id_count,&id_array);

		/* not interested in array variables here */
	       
		if (id_count > 1)
			NhlFree(id_array);

		if (! NhlIsClass(hlu_id,NhlxWorkstationClass))
			continue;

                pb = XtVaCreateManagedWidget
			(name,
			 xmPushButtonGadgetClass,parent,
			 XmNalignment,	XmALIGNMENT_CENTER,
			 XmNuserData, qvars[i],
			 NULL);
		if (hlu_id == sel_xwk_id)
			selected = pb;
	}		
	
	if (selected) {
		XtVaSetValues(parent,
			      XmNmenuHistory,selected,
			      NULL);
	}
	return;

}
static void CreateDialog
(
       PlotStyleMenuRec	*priv,
       NgPlotStyle 	pstyle
       )
{
	Arg	args[50];
	int	nargs;
	NgPlotStyleMenu	*pub = &priv->public;
	char    buf[128] = "";
        XmString xmname;
        Widget  form,label,help,menush,frame,radio;
	char *name, *new_name;
	NhlBoolean is_regvar = False;
	NhlString label_str,plot_str = "Plot Name: ",var_str = "Var Name: ";

#if	DEBUG_PLOTSTYLEMENU
        fprintf(stderr,"%s\n",pstyle->name);
#endif
	
	if (! pstyle)
		return;
        if (pstyle->class_name) {
                sprintf(buf,"Create New %s Plot",pstyle->name);
	}
	else {
		is_regvar = True;
                sprintf(buf,"Create New Variable");
	}
        
        xmname = NgXAppCreateXmString(priv->go->go.appmgr,buf);
#if	DEBUG_PLOTSTYLEMENU
        fprintf(stderr,"%s\n",buf);
#endif
        nargs = 0;
	XtSetArg(args[nargs],XmNdialogTitle,xmname);nargs++;
	XtSetArg(args[nargs],XmNuserData,pstyle);nargs++;
	name = NgNclGetSymName(priv->nsid,pstyle->plot_name,True);

	priv->create_dialog = XmCreateMessageDialog
		(pub->menubar,"CreateDialog",args,nargs);
	help = XmMessageBoxGetChild
		(priv->create_dialog,XmDIALOG_HELP_BUTTON);
	XtUnmanageChild(help);

	if (is_regvar) {
		priv->config_pb = NULL;
		label_str = var_str;
	}
	else {
		priv->config_pb = XtVaCreateManagedWidget
			("ConfigurePB",
			 xmPushButtonGadgetClass,priv->create_dialog,
			 XmNuserData,pstyle,
			 NULL);
		XtAddCallback
			(priv->config_pb,XmNactivateCallback,CreateCB,priv);
		label_str = plot_str;
	}

	XtAddCallback(priv->create_dialog,
		      XmNokCallback,CreateCB,priv);
	XtAddCallback(priv->create_dialog,XmNcancelCallback,
		      CancelCB,priv);
	form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,
		 priv->create_dialog,
		 NULL);

	label = XtVaCreateManagedWidget
		(label_str,xmLabelGadgetClass,
		 form,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNbottomAttachment,XmATTACH_NONE,
		 NULL);
	priv->dialog_text = XtVaCreateManagedWidget
		("dialog",xmTextFieldWidgetClass,
		 form,
		 XmNleftAttachment,XmATTACH_WIDGET,
		 XmNleftWidget,label,
		 XmNbottomAttachment,XmATTACH_NONE,
		 XmNvalue,name,
		 XmNresizeWidth,True,
		 XmNbackground,priv->go->go.edit_field_pixel,
		 NULL);

	if (! is_regvar) {
		/* show the workstation dialog */

		frame = XtVaCreateManagedWidget
			("frame",xmFrameWidgetClass,
			 form,
			 XmNtopOffset,10,
			 XmNtopAttachment,XmATTACH_WIDGET,
			 XmNtopWidget,priv->dialog_text,
			 XmNbottomAttachment,XmATTACH_NONE,
			 NULL);


		form = XtVaCreateManagedWidget
			("form",xmFormWidgetClass,
			 frame,
			 NULL);

		label = XtVaCreateManagedWidget
			("Display plot in ",xmLabelGadgetClass,
			 form,
			 XmNrightAttachment,XmATTACH_NONE,
			 XmNbottomAttachment,XmATTACH_NONE,
			 NULL);

		radio = XtVaCreateManagedWidget
			("radio",xmRowColumnWidgetClass,form,
			 XmNrowColumnType,XmWORK_AREA,
			 XmNradioBehavior,True,
			 XmNisHomogeneous,True,
			 XmNorientation,XmVERTICAL,
			 XmNrightAttachment,XmATTACH_NONE,
			 XmNbottomAttachment,XmATTACH_NONE,
			 XmNtopAttachment,XmATTACH_WIDGET,
			 XmNtopWidget,label,
			 NULL);

		priv->exist_tgl = XtVaCreateManagedWidget
			("Existing window:",xmToggleButtonGadgetClass,
			 radio,
			 XmNset,True,
			 NULL);
		XtAddCallback(priv->exist_tgl,XmNvalueChangedCallback,
			      ChooseNewOrExistingXwkCB,priv); 

		priv->new_tgl = XtVaCreateManagedWidget
			("New window:",xmToggleButtonGadgetClass,
			 radio,
			 NULL);
		XtAddCallback(priv->new_tgl,XmNvalueChangedCallback,
			      ChooseNewOrExistingXwkCB,priv); 

		menush =
			XtVaCreatePopupShell
			("menush",xmMenuShellWidgetClass,
			 form,
			 XmNwidth,		5,
			 XmNheight,		5,
			 XmNallowShellResize,	True,
			 XmNoverrideRedirect,	True,
			 XmNdepth,	XcbGetDepth(priv->go->go.xcb),
			 XmNcolormap,	XcbGetColormap(priv->go->go.xcb),
			 XmNvisual,	XcbGetVisual(priv->go->go.xcb),
			 NULL);

		priv->xwk_menu = XtVaCreateWidget
			("menu",xmRowColumnWidgetClass,menush,
			 XmNrowColumnType,XmMENU_PULLDOWN,
			 NULL);

		AddXwksButtons(priv,priv->xwk_menu);
		priv->xwk_optmenu = XtVaCreateManagedWidget
			("optMenu",
			 xmRowColumnWidgetClass,form,
			 XmNspacing,0,
			 XmNtopAttachment,XmATTACH_WIDGET,
			 XmNtopWidget,label,
			 XmNleftAttachment,XmATTACH_WIDGET,
			 XmNleftWidget,radio,
			 XmNbottomAttachment,XmATTACH_NONE,
			 XmNrowColumnType,XmMENU_OPTION,
			 XmNsubMenuId,priv->xwk_menu,
			 NULL);
		priv->new_xwk = False;
		new_name = NgNclGetSymName(priv->go->go.nclstate,
					   "Xwk",True);
		if (priv->new_xwk_name)
			NhlFree(priv->new_xwk_name);
		priv->new_xwk_name = NhlMalloc(strlen(new_name)+1);
		strcpy(priv->new_xwk_name,new_name);

		priv->new_xwk_text = XtVaCreateManagedWidget
			("dialog",xmTextFieldWidgetClass,
			 form,
			 XmNtopAttachment,XmATTACH_WIDGET,
			 XmNtopWidget,priv->xwk_optmenu,
			 XmNleftAttachment,XmATTACH_WIDGET,
			 XmNleftWidget,radio,
			 XmNleftOffset,8,
			 XmNbottomAttachment,XmATTACH_NONE,
			 XmNvalue,priv->new_xwk_name,
			 XmNresizeWidth,True,
			 XmNbackground,priv->go->go.edit_field_pixel,
			 XmNsensitive,False,
                         NULL);
	}

	NgXAppFreeXmString(priv->go->go.appmgr,xmname);
        XtManageChild(priv->create_dialog);
        _NgGOWidgetTranslations(priv->go,priv->create_dialog);
	XmProcessTraversal(priv->dialog_text, XmTRAVERSE_CURRENT);

        return;
        
}

static void CreateDialogCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotStyleMenuRec	*priv = (PlotStyleMenuRec	*)udata;
	NgPath		path;
	NgPlotStyle	pstyle = NULL;

#if	DEBUG_PLOTSTYLEMENU
        fprintf(stderr,"in plot create cb\n");
#endif

        XtVaGetValues(w,
                      XmNuserData,&path,
                      NULL);
	if (! path)
		return;
	if (path->data) {
		pstyle = (NgPlotStyle) path->data;
		pstyle->path = path->dirpath;
	}
        CreateDialog(priv,pstyle);
        return;
        
}

typedef struct _NgMenuInfoRec {
	NgPath  path;
	Widget	menush;
	int button_count;
	Widget *buttons;
} NgMenuInfoRec, *NgMenuInfo;

		
static void UnmapMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	NgMenuInfo minfo;
	int 	 i;

#if	DEBUG_PLOTSTYLEMENU
        fprintf(stderr,"in unmap menu cb\n");
#endif

	XtVaGetValues(w,
		      XmNuserData,&minfo,
		      NULL);
	for (i = 0; i < minfo->button_count; i++) {
		XtDestroyWidget(minfo->buttons[i]);
	}
	if (minfo->menush)
		XtDestroyWidget(minfo->menush);
	if (minfo->button_count)
		NhlFree(minfo->buttons);
	NhlFree(minfo);
}

static Widget CreateButtonWithSubmenu
(
	PlotStyleMenuRec	*priv,
	NgPath 		path,
	Widget		parent,
	Widget		*menush
)
{
	Widget		menu;
	XmString   	xmdir;
	NgMenuInfo	minfo = NULL;

	if (! *menush) {
		*menush = XtVaCreatePopupShell
			("override_sh",xmMenuShellWidgetClass,parent,
			 XmNwidth,		5,
			 XmNheight,		5,
			 XmNallowShellResize,	True,
			 XtNoverrideRedirect,	True,
			 XmNdepth,		XcbGetDepth(priv->go->go.xcb),
			 XmNcolormap,	      XcbGetColormap(priv->go->go.xcb),
			 XmNvisual,		XcbGetVisual(priv->go->go.xcb),
			 NULL);
	}

	minfo = NhlMalloc(sizeof(NgMenuInfoRec));
	minfo->path = path;
	minfo->menush = NULL;
	minfo->buttons = NULL;
	minfo->button_count = 0;
	
        menu =  XtVaCreateWidget
                ("plotStyleMenu",xmRowColumnWidgetClass,*menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
		 XmNuserData,		minfo,
                 NULL);

	XtAddCallback(menu,
		      XmNmapCallback,PlotMenuCB,priv);
	if (path == PlotStylePathTop) {
		xmdir = NgXAppCreateXmString(priv->go->go.appmgr,_NgTOPPATH);
	}
	else if (path->data) {
		xmdir = NgXAppCreateXmString
			(priv->go->go.appmgr,(NhlString)path->data);
	}
	else {
		xmdir = NgXAppCreateXmString(priv->go->go.appmgr,path->name);
	}

	XtVaCreateManagedWidget
                ("plotStyleBtn",xmCascadeButtonGadgetClass,parent,
                 XmNsubMenuId,menu,
		 XmNlabelString,xmdir,
                 NULL);
	NgXAppFreeXmString(priv->go->go.appmgr,xmdir);

	return menu;
}
	

static void PlotMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotStyleMenuRec	*priv = (PlotStyleMenuRec	*)udata;
        int		i;
	NgPath		path;
	int		dircount = 0;
	Widget		menush = NULL;
	NgMenuInfo	minfo = NULL;
	Widget		button;

#if	DEBUG_PLOTSTYLEMENU
        fprintf(stderr,"in plot menu cb\n");
#endif
	
	XtVaGetValues(w,
		      XmNuserData,&minfo,
		      NULL);
	if (! minfo) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "This shouldn't happen"));
		return;
	}
	path = minfo->path;
	for (i = 0; i < path->subpathcount; i++) {
		NgPath p = path->subpathptrs[i];
		if (p->subpathcount)
			dircount++;
		else
			break;
	}

	if (path->subpathcount == minfo->button_count)
		return;

	for (i = 0; i < dircount; i++) {
		NgPath p = path->subpathptrs[i];
		CreateButtonWithSubmenu(priv,p,w,&menush);
	}
	minfo->button_count = path->subpathcount;

	if (dircount == path->subpathcount)
		return;
	else if (dircount) {
		/* 
		 * add a separator if there are directories and plot styles
		 * in this directory
		 */
		XtVaCreateManagedWidget("sep",xmSeparatorGadgetClass,w,NULL);
	}

	for (i = dircount; i < path->subpathcount; i++) {
		NgPath p = path->subpathptrs[i];
		NgPlotStyle	pstyle = (NgPlotStyle)p->data;
		
		button = XtVaCreateManagedWidget
                                (pstyle->name,
                                 xmCascadeButtonGadgetClass,w,
                                 XmNuserData,p,
                                 NULL);
		XtAddCallback(button,
			      XmNactivateCallback,CreateDialogCB,
			      priv);
	}

        return;
}

NhlErrorTypes NgUpdatePlotStyleMenu
(
        NgPlotStyleMenu		*plot_spec_menu
        )
{

        return NhlNOERROR;
}


static int pathsort
(
        const void *p1,
        const void *p2
)
{
        const NgPath path1 = *(NgPath *) p1;
        const NgPath path2 = *(NgPath *) p2;

	if (path1->subpathcount && path2->subpathcount) {
		NhlString str1, str2;
		/* 
		 * both are directories -- 
		 * if has data it's the directory name
		 */
		str1 = path1->data ? (NhlString) path1->data : path1->name;
		str2 = path2->data ? (NhlString) path2->data : path2->name;
		return strcmp(str1,str2);
	}
	/* 
	 * directories always ahead of files
	 */
	else if (path1->subpathcount) {
		return -1;
	}
	else if (path2->subpathcount) {
		return 1;
	}
	else {
		/*
		 * both are files
		 */
		NgPlotStyle pstyle1 = (NgPlotStyle) path1->data;
		NgPlotStyle pstyle2 = (NgPlotStyle) path2->data;
		return (strcmp(pstyle1->name,pstyle2->name));
	}
}
static void SetUpPathPtrs
(
	NgPath		path,
	int		level,
	NhlPointer	data
) 
{
	int i,count = 0;
	NgPath p;

	for (p = path->subpaths; p; p = p->next)
		count++;

	path->subpathcount = count;

	if (! count) 
		return;

	path->subpathptrs = NhlMalloc(count * sizeof(NgPath));

	for (i = 0, p = path->subpaths; p; i++, p = p->next)
		path->subpathptrs[i] = p;

	qsort(path->subpathptrs,count,sizeof(NgPath),pathsort);

	return;
}

static void PrintPath
(
	NgPath		path,
	int		level,
	NhlPointer	data
) 
{
	int i;

	for (i = 1; i < level; i++)
		fprintf(stderr,"\t");
	if (path->data) {
		NgPlotStyle pstyle = (NgPlotStyle) path->data;
		fprintf(stderr,"%s\n",pstyle->name);
	}
	else {
		fprintf(stderr,"%s\n",path->name);
	}
}
static void ProcessPlotStylePaths
(
	PlotStyleMenuRec	*priv
)
{
	NhlString path;
	NgPath pathp;

	path = getenv(NDV_PLOT_STYLE_PATH);

	if (! path) {
		fprintf(stderr,
		      "%s environment variable not set\n\tdefaulting to %s\n",
			NDV_PLOT_STYLE_PATH,_NgDEFAULT_PLOTSTYLE_PATH);
		path = _NgDEFAULT_PLOTSTYLE_PATH;
	}
	PlotStylePaths = InitializePath(NULL,NULL,_NgTOPPATH);
	if (path) {
		char *cp,*last_cp;
		NgPath *pathptr = &PlotStylePaths->subpaths;
		char *buf = NhlMalloc(strlen(path)+1);
		strcpy(buf,path);
		last_cp = buf;
		while (cp = strchr(last_cp,':')) {
			*cp = '\0';
			if (*last_cp) {
				pathp = InitializePath
					(NULL,NULL,_NGResolvePath(last_cp));
				*pathptr = ProcessPath(pathp);
				if (*pathptr)
					pathptr = &(*pathptr)->next;
			}
			last_cp = cp + 1;
		}
		if (*last_cp) {
			pathp = InitializePath
				(NULL,NULL,_NGResolvePath(last_cp));
			*pathptr = ProcessPath(pathp);
		}
		NhlFree(buf);
	}

	IteratePath(PlotStylePaths,0,False,SetUpPathPtrs,NULL);
#if	DEBUG_PLOTSTYLEMENU
	IteratePathPtrs(PlotStylePaths,0,True,PrintPath,NULL);
	fprintf(stderr,"\n---------------------------------\n");
	IteratePathPtrs(PlotStylePaths,0,False,PrintPath,NULL);
#endif

	PlotStylePathTop = PlotStylePaths;
	/*
	 * Top level directories with only a single subdirectory and no
	 * plot style files are discarded from the menu hierarchy.
	 */

	while (PlotStylePathTop->subpaths && 
	       ! PlotStylePathTop->subpaths->next) {
		PlotStylePathTop = PlotStylePathTop->subpaths;
	}
	return;
}

NgPlotStyleMenu *
NgCreatePlotStyleMenu
(
        NgGO            go,
        Widget		parent
)
{
	PlotStyleMenuRec	*priv;
	NgPlotStyleMenu	*pub;
        Widget		menush = NULL;


        priv = NhlMalloc(sizeof(PlotStyleMenuRec));
        priv->go = go;
        priv->create_dialog = NULL;
	priv->new_xwk_name = NULL;
	pub = &priv->public;

	if (! PlotStylePaths)
		ProcessPlotStylePaths(priv);
        
	NhlVAGetValues(priv->go->go.appmgr,
		NgNappNclState,	&priv->nsid,
		NULL);
        
        
        pub->menubar =  XtVaCreateManagedWidget
                ("CreateMenu",xmRowColumnWidgetClass,
                 parent,
                 XmNrowColumnType,      XmMENU_BAR,
                 NULL);

	priv->plot.buttons = NULL;
	priv->plot.menu = CreateButtonWithSubmenu
		(priv,PlotStylePathTop,pub->menubar,&menush);

        priv->var.menu = XtVaCreateWidget
                ("Variable",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 XmNuserData,&VarPlotStyle,
                 NULL);
	XtAddCallback(priv->var.menu,
		      XmNmapCallback,CreateDialogCB,priv);

	pub->var_mbutton = 
                XtVaCreateManagedWidget
                ("Variable",xmCascadeButtonGadgetClass,
                 pub->menubar,
                 XmNuserData,&VarPath,
                 NULL);
        XtAddCallback(pub->var_mbutton,
                      XmNactivateCallback,CreateDialogCB,
                      priv);


	XtManageChild(priv->plot.menu);
	XtManageChild(priv->var.menu);

        return pub;
        
}

void NgDestroyPlotStyleMenu
(
        NgPlotStyleMenu		*plot_spec_menu
        )
{
	NgPlotStyleMenu	*pub = plot_spec_menu;
	PlotStyleMenuRec	*priv = (PlotStyleMenuRec	*)pub;

        if (priv->plot.buttons)
                NhlFree(priv->plot.buttons);
        
        NhlFree(priv);

        return;
}

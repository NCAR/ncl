/*
 *      $Id: plotspecmenu.c,v 1.12 1999-07-30 03:20:58 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		plotspecmenu.c
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

#include <ncarg/ngo/plotspecmenuP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/sort.h>
#include <ncarg/ngo/hlupage.h>
#include <ncarg/ngo/plotpage.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/varpage.h>
#include <ncarg/ngo/plotapp.h>
#include <ncarg/ngo/graphic.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <ncarg/ngo/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/PushB.h>
#include <Xm/TextF.h>
#include  <Xm/Form.h>
#include  <Xm/LabelG.h>

#define _NgDEFAULT_PLOTSTYLE_PATH "$NCARG_ROOT/lib/ncarg/plot_styles"

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


NgPlotStyle	PlotStyles = NULL;


NgPlotStyleRec  VarPlotStyle = { NULL, NULL, NULL, "ncl_var",NULL };

static int	PlotStyleCount = 0;
static 	NhlString  PlotStylePath[4] = { NULL,NULL,NULL,NULL };
static int PlotStylePathCount = 0;


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
		    

static void CancelCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec	*)udata;
	NgPlotSpecMenu	*pub = &priv->public;
	NgPlotStyle	pstyle;

        XtVaGetValues(w,
                      XmNuserData,&pstyle,
                      NULL);
	return;
}

static void
InsufficientDimsMesg(
	Widget		w,
	NgVarData	vdata,
	NgPlotStyle	pstyle)
{		
	char message[256];
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

static void CreateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec *)udata;
	NgPlotSpecMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        NgDataProfile	prof;
	NgPlotStyle	pstyle;
        NrmQuark	qname;
        char		*vartext,*varname;
        NgPageId	page_id;
	int		i;
	NgVarData	vdata = pub->vdata;
        
#if	DEBUG_PLOTSPECMENU
        fprintf(stderr,"in create cb\n");
#endif

        XtVaGetValues(w,
                      XmNuserData,&pstyle,
                      NULL);
		
        XtVaGetValues(priv->dialog_text,
                      XmNvalue,&vartext,
                      NULL);

	/* need to qualify text string, and warn user if it's already
	   a symbol */

        if (! pstyle->class_name) { 
		/* 
		 * If the class is missing, it means that we're just
		 * copying the data to a variable.
		 */

		NgNclCopyShapedVar(priv->nsid,vartext,
				   vdata->qfile,vdata->qvar,vdata->ndims,
				   vdata->start,vdata->finish,vdata->stride);
                return;
        }
        else if (!strcmp(pstyle->class_name,NGPLOTCLASS)){
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

		strcpy(varname,NgNclGetSymName(priv->nsid,vartext,False));
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
                NhlString varname = NgNclGetSymName(priv->nsid,vartext,False);
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

#if	DEBUG_PLOTSPECMENU
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
        return;
}

static void CreateDialog
(
       PlotSpecMenuRec	*priv,
       NgPlotStyle 	pstyle
       )
{
	Arg	args[50];
	int	nargs;
	NgPlotSpecMenu	*pub = &priv->public;
	char    buf[128] = "";
        XmString xmname;
        Widget  form,label,help;
	char *name;
        NgDataProfile	prof = NULL;

#if	DEBUG_PLOTSPECMENU
        fprintf(stderr,"%s\n",pstyle->name);
#endif
        if (pstyle->class_name) {
                sprintf(buf,"Create %s Plot",pstyle->name);
	}
	else {
                sprintf(buf,"Create Ncl Variable");
	}
        
        xmname = NgXAppCreateXmString(priv->go->go.appmgr,buf);
#if	DEBUG_PLOTSPECMENU
        fprintf(stderr,"%s\n",buf);
#endif
        nargs = 0;
	XtSetArg(args[nargs],XmNdialogTitle,xmname);nargs++;
	XtSetArg(args[nargs],XmNuserData,pstyle);nargs++;
	name = NgNclGetSymName(priv->nsid,pstyle->plot_name,True);
        if (! priv->create_dialog) {
                priv->create_dialog = XmCreateMessageDialog
                        (pub->menubar,"CreateDialog",args,nargs);
                help = XmMessageBoxGetChild
                        (priv->create_dialog,XmDIALOG_HELP_BUTTON);
                XtUnmanageChild(help);
		priv->config_pb = XtVaCreateManagedWidget
			("ConfigurePB",
			 xmPushButtonWidgetClass,priv->create_dialog,
			 XmNuserData,pstyle,
			 NULL);
		XtAddCallback
			(priv->config_pb,XmNactivateCallback,CreateCB,priv);

		XtAddCallback(priv->create_dialog,
			      XmNokCallback,CreateCB,priv);
		XtAddCallback(priv->create_dialog,XmNcancelCallback,
			      CancelCB,priv);
		form = XtVaCreateManagedWidget
                        ("form",xmFormWidgetClass,
                         priv->create_dialog,
                         NULL);

                label = XtVaCreateManagedWidget
                        ("Name",xmLabelGadgetClass,
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
                         NULL);
        }
	else {
		XtSetValues(priv->create_dialog,args,nargs);
                XtVaSetValues(priv->dialog_text,
                              XmNvalue,name,
                              NULL);
	}
	if (! pstyle->class_name) {
		if (XtIsManaged(priv->config_pb))
			XtUnmanageChild(priv->config_pb);
	}
	else if (! XtIsManaged(priv->config_pb)) {
		 XtManageChild(priv->config_pb);
	}
	XmStringFree(xmname);
        XtManageChild(priv->create_dialog);
        
        return;
        
}

static void CreateDialogCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec	*)udata;
	NgPlotSpecMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
	NgPlotStyle	pstyle;

#if	DEBUG_PLOTSPECMENU
        fprintf(stderr,"in plot create cb\n");
#endif

        XtVaGetValues(w,
                      XmNuserData,&pstyle,
                      NULL);
        CreateDialog(priv,pstyle);
        return;
        
}

static void PlotMenuCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec	*)udata;
	NgPlotSpecMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        int		i;

#if	DEBUG_PLOTSPECMENU
        fprintf(stderr,"in plot menu cb\n");
#endif
        if (plot->count == 0) {
                plot->count = PlotStyleCount;
                plot->buttons = NhlMalloc(plot->count * sizeof(Widget));
                for (i = 0; i < plot->count; i++) {
                        plot->buttons[i] = XtVaCreateManagedWidget
                                (PlotStyles[i].name,
                                 xmCascadeButtonGadgetClass,
                                 plot->menu,
                                 XmNuserData,&PlotStyles[i],
                                 NULL);
                        XtAddCallback(plot->buttons[i],
                                      XmNactivateCallback,CreateDialogCB,
                                      priv);
                }
        }
        return;
}
NhlErrorTypes NgUpdatePlotSpecMenu
(
        NgPlotSpecMenu		*plot_spec_menu
        )
{
	NgPlotSpecMenu	*pub = plot_spec_menu;
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec	*)pub;
        return NhlNOERROR;
}

#define RES_STYLENAME	"*ndvPlotStyleName"
#define RES_CLASS	"*ndvPlotClass"
#define RES_PLOTNAME	"*ndvPlotName"

static void
GetPlotStylesInPath
(
	PlotSpecMenuRec	*priv,
	const char	*path
)
{
	struct stat	statbuf;
	struct dirent	*dirp;  
	DIR		*dp;
	int		i,j;
	int		count, totalcount;
	char		fullpath[1024];
	char		*endp;
	int 		stylenamelen,classlen,plotnamelen;
	FILE 		*fp = NULL;

	if (! path) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
	   "Invalid directory encountered in plot style path specification"));
		return;
	}

	for (i = 0; i < PlotStylePathCount; i ++) {
		if (! strcmp(path,PlotStylePath[i])) /* dir already read */
			return;
	}

	if ((dp = opendir(path)) == NULL) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "Invalid plot style directory: %s",path));
		return;
	}
/*
 * first just count the possibilities
 */
	count = 0;
	while ( (dirp = readdir(dp)) != NULL) {
		char *cp;
		char buf[256];

		if (! strcmp(dirp->d_name, ".")  ||
		    ! strcmp(dirp->d_name, ".."))
			continue;	
		if (! (cp = strrchr(dirp->d_name,'.')))
			continue;
		cp++;
		if (! cp || strcmp(cp,"res") || strlen(cp) != 3) 
			continue;
		count++;
	}
	if (! count) {
		closedir(dp);
		return;
	}

	PlotStylePath[PlotStylePathCount] = NhlMalloc(strlen(path)+1);
	strcpy(PlotStylePath[PlotStylePathCount],path);

	totalcount = PlotStyleCount + count;
	PlotStyles = NhlRealloc(PlotStyles,
				totalcount * sizeof(NgPlotStyleRec));
	for (i = PlotStyleCount; i < totalcount; i++) 
		memset(&PlotStyles[i],(char)0,sizeof(NgPlotStyleRec));

	if (! PlotStyles) {
		 NHLPERROR((NhlFATAL,ENOMEM,NULL));
		closedir(dp);
		return;
	}
	rewinddir(dp);

	count = PlotStyleCount;
	strcpy(fullpath,path);
	endp = fullpath + strlen(fullpath);
	*endp++ = '/';
	*endp = '\0';

	stylenamelen = strlen(RES_STYLENAME);
	classlen = strlen(RES_CLASS);
	plotnamelen = strlen(RES_PLOTNAME);

	while ( (dirp = readdir(dp)) != NULL) {
		char *cp;
		char buf[256];
		NhlBoolean duplicate = False;

		if (! strcmp(dirp->d_name, ".")  ||
		    ! strcmp(dirp->d_name, "..") ||
		    ! strncmp(dirp->d_name,"_Ng",3) )
			continue;	
		if (! (cp = strrchr(dirp->d_name,'.')))
			continue;
		cp++;
		if (! cp || strcmp(cp,"res") || strlen(cp) != 3) 
			continue;
		
		strcpy(endp,dirp->d_name);
		if (fp)
			fclose(fp);

		fp = fopen(fullpath,"r");
		if (! fp) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "Unable to open resource file %s: ignoring",
				   dirp->d_name));
			continue;
		}

		if (PlotStyles[count].name)
			NhlFree(PlotStyles[count].name);
		if (PlotStyles[count].class_name)
			NhlFree(PlotStyles[count].class_name);
		if (PlotStyles[count].plot_name)
			NhlFree(PlotStyles[count].plot_name);
		PlotStyles[count].name = NULL;
		PlotStyles[count].class_name = NULL;
		PlotStyles[count].plot_name = NULL;
		while (cp = fgets(buf,255,fp)) {
			char *name,*np;
			NhlClass class;

			while (np = strrchr(cp,'\n'))
			       *np = '\0';
			while (isspace(*cp))
				cp++;
			if (*cp == '!')
				continue;
			if (! strncmp(cp,RES_STYLENAME,stylenamelen)) {
				np = cp + stylenamelen;
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
				if (PlotStyles[count].name)
					NhlFree(PlotStyles[count].name);
				PlotStyles[count].name = name;
			}
			else if (! strncmp(cp,RES_CLASS,classlen)) {
				char *class_name;

				np = cp + classlen;
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
				if (PlotStyles[count].class_name)
					NhlFree(PlotStyles[count].class_name);
				PlotStyles[count].class_name = class_name;
			}
			else if (! strncmp(cp,RES_PLOTNAME,plotnamelen)) {

				np = cp + plotnamelen;
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
				if (PlotStyles[count].plot_name)
					NhlFree(PlotStyles[count].plot_name);
				PlotStyles[count].plot_name = name;
			}
		}
#if 0
		if (! PlotStyles[count].pstyle) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "Plot style resources file %s invalid: ignoring",
				   dirp->d_name));
			continue;
		}
#endif

		if (! PlotStyles[count].class_name) {
			/*
			 * If no class name is specified the plot is
			 * assumed to be a generic plot and is 
			 * assigned the pseudo-class NGPLOTCLASS name.
			 */
			PlotStyles[count].class_name = 
				NhlMalloc(strlen(NGPLOTCLASS)+1);
			strcpy(PlotStyles[count].class_name,NGPLOTCLASS);
		}

		strcpy(buf,dirp->d_name);
		cp = strrchr(buf,'.');
		*cp = '\0';

		/* 
		 * If this plot style name matches any in previously
		 * parsed directories, skip it.
		 */
		for (j = 0; j < PlotStyleCount; j++) {
			if (! strcmp(buf,PlotStyles[j].pstyle)) {
				duplicate = True;
				break;
			}
		}
		if (duplicate)
			continue;

		if (strcmp(PlotStyles[count].class_name,NGPLOTCLASS) &&
		    ! NgHasDataProfile(priv->go,
				       PlotStyles[count].class_name)) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
 	       "No data profile associated with class name %s in %s: ignoring",
			   PlotStyles[count].class_name,
				   dirp->d_name));
			continue;
		}
		if (! PlotStyles[count].plot_name)
			PlotStyles[count].plot_name = 
				GetDefaultSymbol
			      (PlotStyles[count].class_name);

		PlotStyles[count].pstyle = NhlMalloc(strlen(buf)+1);
		strcpy(PlotStyles[count].pstyle,buf);
		if (! PlotStyles[count].name)
			PlotStyles[count].name = PlotStyles[count].pstyle;
		PlotStyles[count].path = PlotStylePath[PlotStylePathCount];
		count++;
	}
	if (fp)
		fclose(fp);
        closedir(dp);
	if (count) {
		PlotStylePathCount++;
		PlotStyleCount = count;
	}
	else {
		NhlFree(PlotStylePath[PlotStylePathCount]);
	}

	return;
}

/*
 * Plot style directory search:
 * 	1. NDV_PLOT_STYLE_PATH environment variable
 *	2. $NCARG_ROOT/lib/ncarg/plot_styles
 * all found plot styles are merged. if 2 plot styles have the same name
 * then the first one is used.
 */
static void 
UpdatePlotStyles
(
	PlotSpecMenuRec	*priv
)
{
	NhlString path;
	char buf[512];

	path = getenv(NDV_PLOT_STYLE_PATH);

	if (! path) {
		fprintf(stderr,
		      "%s environment variable not set\n\tdefaulting to %s\n",
			NDV_PLOT_STYLE_PATH,_NgDEFAULT_PLOTSTYLE_PATH);
		path = _NgDEFAULT_PLOTSTYLE_PATH;
	}
	if (path) {
		char *cp,*last_cp = buf;
		strcpy(buf,path);
		while (cp = strchr(last_cp,':')) {
			*cp = '\0';
			if (*last_cp)
				GetPlotStylesInPath(priv,
						    _NGResolvePath(last_cp));
			last_cp = cp + 1;
		}
		if (*last_cp)
			GetPlotStylesInPath(priv,_NGResolvePath(last_cp));
	}

	return;
}

NgPlotSpecMenu *
NgCreatePlotSpecMenu
(
        NgGO            go,
        Widget		parent
)
{
	PlotSpecMenuRec	*priv;
	NgPlotSpecMenu	*pub;
        Widget		menush;
	int		i;


        priv = NhlMalloc(sizeof(PlotSpecMenuRec));
        priv->go = go;
        priv->create_dialog = NULL;
	pub = &priv->public;
	if (! PlotStyles)
		UpdatePlotStyles(priv);
        
	NhlVAGetValues(priv->go->go.appmgr,
		NgNappNclState,	&priv->nsid,
		NULL);
        
        priv->plot.count = priv->var.count = priv->data.count = 0;
        priv->plot.alloced = priv->var.alloced = priv->data.alloced = 0;
        
        pub->menubar =  XtVaCreateManagedWidget
                ("CreateMenu",xmRowColumnWidgetClass,
                 parent,
                 XmNrowColumnType,      XmMENU_BAR,
                 NULL);

	menush = XtVaCreatePopupShell("override_sh",xmMenuShellWidgetClass,
						                 pub->menubar,
		XmNwidth,		5,
		XmNheight,		5,
		XmNallowShellResize,	True,
		XtNoverrideRedirect,	True,
		XmNdepth,		XcbGetDepth(priv->go->go.xcb),
		XmNcolormap,		XcbGetColormap(priv->go->go.xcb),
		XmNvisual,		XcbGetVisual(priv->go->go.xcb),
		NULL);
        
        priv->plot.menu =  XtVaCreateWidget
                ("Plot",xmRowColumnWidgetClass,menush,
                 XmNrowColumnType,	XmMENU_PULLDOWN,
                 NULL);
	XtAddCallback(priv->plot.menu,
		      XmNmapCallback,PlotMenuCB,priv);

	pub->plot_mbutton = 
                XtVaCreateManagedWidget
                ("Plot",xmCascadeButtonGadgetClass,
                 pub->menubar,
                 XmNsubMenuId,	priv->plot.menu,
                 NULL);

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
                 XmNuserData,&VarPlotStyle,
                 NULL);
        XtAddCallback(pub->var_mbutton,
                      XmNactivateCallback,CreateDialogCB,
                      priv);
        
	XtManageChild(priv->plot.menu);
	XtManageChild(priv->var.menu);

        return pub;
        
}

void NgDestroyPlotSpecMenu
(
        NgPlotSpecMenu		*plot_spec_menu
        )
{
	NgPlotSpecMenu	*pub = plot_spec_menu;
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec	*)pub;

        if (priv->plot.count)
                NhlFree(priv->plot.buttons);
        
        NhlFree(priv);

        return;
}

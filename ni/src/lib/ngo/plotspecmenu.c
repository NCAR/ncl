/*
 *      $Id: plotspecmenu.c,v 1.6 1999-01-11 19:36:27 dbrown Exp $
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
#include <ncarg/ngo/nclstate.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/RowColumn.h>
#include <ncarg/ngo/CascadeBG.h>
#include <Xm/MenuShell.h>
#include <Xm/MessageB.h>
#include <Xm/TextF.h>
#include  <Xm/Form.h>
#include  <Xm/LabelG.h>

typedef struct _NgDefSymbol {
	NhlString class_name;
	NhlString def_symbol;
} NgDefSymbolRec, *NgDefSymbol;

static NgDefSymbolRec Def_Symbols[] = {
	{ "contourPlotClass", "cn_obj" },
	{ "streamlinePlotClass", "st_obj" },
	{ "vectorPlotClass", "vc_obj" },
	{ "xyPlotClass", "xy_obj" },
	{ "coordArraysClass", "ca_obj" },
	{ "scalarFieldClass", "sf_obj" },
	{ "vectorFieldClass",  "vf_obj"} };

typedef struct _NgPlotStyleRec {
	NhlString 	pstyle;
	NhlString	name;
	NhlClass	class;
	NhlString	def_symbol;
	NgDataProfile	dprof;
} NgPlotStyleRec, *NgPlotStyle;

NgPlotStyle	PlotStyles = NULL;

static NgDataProfileRec VarDataProf =
	{_NgDEFAULT,NULL,NULL,0,0,{ 0 },False,NULL };

NgPlotStyleRec  VarPlotStyle = { NULL, NULL, NULL, "ncl_var", &VarDataProf };

static int	PlotStyleCount = 0;
static 	char	*PlotStyleDir = NULL;

static NhlString GetDefaultSymbol
(
        NhlString       class_name
        )
{
	int i;
	
	for (i = 0; i < NhlNumber(Def_Symbols); i++)
		if (!strcmp(class_name,Def_Symbols[i].class_name))
			return(Def_Symbols[i].def_symbol);
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

	NgFreeDataProfile(pstyle->dprof);
	pstyle->dprof = NULL;

	return;
}
static void CopyShapedVar
(
        PlotSpecMenuRec *priv,
        NhlString       varname
        )
{
        char buf[256];
        NgPlotSpecMenu  *pub = &priv->public;
        int i;

        if (pub->qsymbol)
                sprintf(buf,"%s = %s->%s(",
                        NgNclGetSymName(priv->nsid,varname,False),
                        NrmQuarkToString(pub->qsymbol),
                        NrmQuarkToString(pub->vinfo->name));
        else
                sprintf(buf,"%s = %s(",
                        NgNclGetSymName(priv->nsid,varname,False),
                        NrmQuarkToString(pub->vinfo->name));
        for (i = 0; i < pub->vinfo->n_dims; i++) 
		sprintf(&buf[strlen(buf)],"%d:%d:%d,",pub->start[i],
			pub->finish[i],pub->stride[i]);
        
        	/* backup 1 to remove last comma */
	sprintf(&buf[strlen(buf)-1],")\n");

	(void)NgNclSubmitBlock(priv->nsid,buf);

        return;
}

static NhlBoolean
CoordItem(
       NgDataProfile 	prof,
       int		index
)
{
	int i;
	for (i = 0; i < prof->ditems[prof->master_data_ix]->n_dims; i++) {
		if (prof->coord_items[i] == index)
			return True;
	}
	return False;
}

static void LowerCase(char *string)
{
        char *cp = string;

        while (*cp != '\0') {
                *cp = tolower(*cp);
                cp++;
        }
}
static NhlBoolean ConformingVar
(
	NrmQuark		qfile,
	NrmQuark 		qvar,
	NclApiVarInfoRec	*vinfo	
)
{
	NclApiDataList  *dl;
	NclApiVarInfoRec *tvinfo;
	int i;

	if (qfile > NrmNULLQUARK)
		dl = NclGetFileVarInfo(qfile,qvar);
	else
		dl = NclGetVarInfo(qvar);

	if (! dl)
		return False;

	tvinfo = dl->u.var;

	if (tvinfo->n_dims != vinfo->n_dims) {
		NclFreeDataList(dl);
		return False;
	}

	for (i = 0; i < vinfo->n_dims; i++) {
		if (tvinfo->dim_info[i].dim_quark != 
		    vinfo->dim_info[i].dim_quark ||
		    tvinfo->dim_info[i].dim_size != 
		    vinfo->dim_info[i].dim_size) {
			NclFreeDataList(dl);
			return False;
		}
	}

	NclFreeDataList(dl);
	return True;
}
	
static void GetExtraVectorData
(
       PlotSpecMenuRec	*priv,
       NgDataProfileRec *prof,
       NrmQuark *secondary_file,
       NrmQuark *secondary_var,
       NhlBoolean *secondary_is_udata
)
{
	NgPlotSpecMenu	*pub = &priv->public;
	char namebuf[256];
	char *tname, *ftname;
	char *uname= NULL,*vname = NULL;
	char *cp;
	char testch[] = { 'u','U','v','V','\0' };
	char *var;
	int cix,ix,i;
	NhlBoolean found = False, file_found = False;
	NclApiDataList *finfo,*vinfo;
	NclApiDataList *dl;

	*secondary_var = *secondary_file = NrmNULLQUARK;
	*secondary_is_udata = False;

	if (! pub->vinfo)
		return;
	tname = NrmQuarkToString(pub->vinfo->name);
/*
 * Look through current file if file variable; else look through regular
 * variable list.
 */
	if (pub->qsymbol) {
		finfo =	NclGetFileInfo(pub->qsymbol);
		for (cix = 0; testch[cix] != '\0'; cix++) {
			cp = strchr(tname,testch[cix]);
			if (! cp)
				continue;
			ix = cp - tname;
			strcpy(namebuf,tname);
			namebuf[ix] = cix < 2 ? testch[cix+2] : testch[cix-2];
			for (i = 0; i < finfo->u.file->n_vars; i++) {
				var = NrmQuarkToString
					(finfo->u.file->var_names[i]);
				if (! strcmp(var,namebuf) &&
				    ConformingVar
				    (pub->qsymbol,
				     finfo->u.file->var_names[i],pub->vinfo)) {
					*secondary_file = pub->qsymbol;
					*secondary_var = 
						finfo->u.file->var_names[i];
					*secondary_is_udata = cix >= 2;
					found = True;
					break;
				}
			}
			if (found)
				break;
		}
		NclFreeDataList(finfo);
		if (found)
			return;
	}	
	else {
		dl = NclGetVarList();
		for (cix = 0; testch[cix] != '\0'; cix++) {
			cp = strchr(tname,testch[cix]);
			if (! cp)
				continue;
			ix = cp - tname;
			strcpy(namebuf,tname);
			namebuf[ix] = cix < 2 ? testch[cix+2] : testch[cix-2];
			for (vinfo = dl; vinfo; vinfo = vinfo->next) {
				var = NrmQuarkToString(vinfo->u.var->name);
				if (! strcmp(var,namebuf) &&
				    ConformingVar
				    (NrmNULLQUARK,
				     vinfo->u.var->name,pub->vinfo)) {
					*secondary_var = vinfo->u.var->name;
					*secondary_is_udata = cix >= 2;
					found = True;
					break;
				}
			}
			if (found) 
				break;
		}
		NclFreeDataList(dl);
		if (found)
			return;
	}
/*
 * Look through other files for possible match. If a file name matches in
 * the same way as the var name then it is used. Otherwise the match will
 * default to the first file with a matching var name that is conforming.
 */
	dl = NclGetFileList();
	if (pub->qsymbol) {
		ftname = NrmQuarkToString(pub->qsymbol);
		for (cix = 0; testch[cix] != '\0'; cix++) {
			cp = strchr(ftname,testch[cix]);
			if (! cp)
				continue;
			ix = cp - ftname;
			strcpy(namebuf,ftname);
			namebuf[ix] = cix < 2 ? testch[cix+2] : testch[cix-2];
			for (finfo = dl; finfo; finfo = finfo->next) {
				char *file;
				file = NrmQuarkToString(finfo->u.file->name);
				if (! strcmp(file,namebuf)) {
					file_found = True;
					break;
				}
			}
			if (file_found)
				break;
		}
	}
	if (file_found) {
		for (cix = 0; testch[cix] != '\0'; cix++) {
			cp = strchr(tname,testch[cix]);
			if (! cp)
				continue;
			ix = cp - tname;
			strcpy(namebuf,tname);
			namebuf[ix] = cix < 2 ? testch[cix+2] : testch[cix-2];
			for (i = 0; i < finfo->u.file->n_vars; i++) {
				var = NrmQuarkToString
					(finfo->u.file->var_names[i]);
				if (! strcmp(var,namebuf) && 
				    ConformingVar
				    (finfo->u.file->name,
				     finfo->u.file->var_names[i],pub->vinfo)){
					*secondary_file = finfo->u.file->name;
					*secondary_var = 
						finfo->u.file->var_names[i];
					*secondary_is_udata = cix >= 2;
					found = True;
					break;
				}
			}
			if (found)
				break;
		}
		if (found) {
			NclFreeDataList(dl);
			return;
		}
	}

/*
 * look through other files
 */			
	for (cix = 0; testch[cix] != '\0'; cix++) {
		cp = strchr(tname,testch[cix]);
		if (! cp)
			continue;
		ix = cp - tname;
		strcpy(namebuf,tname);
		namebuf[ix] = cix < 2 ? testch[cix+2] : testch[cix-2];
		for (finfo = dl; finfo; finfo = finfo->next) {
			for (i = 0; i < finfo->u.file->n_vars; i++) {
				var = NrmQuarkToString
					(finfo->u.file->var_names[i]);
				if (! strcmp(var,namebuf) &&
				    ConformingVar
				    (finfo->u.file->name,
				     finfo->u.file->var_names[i],pub->vinfo)){
					*secondary_file = finfo->u.file->name;
					*secondary_var = 
						finfo->u.file->var_names[i];
					*secondary_is_udata = cix >= 2;
					found = True;
					break;
				}
			}
			if (found)
				break;
		}
		if (found) 
			break;
	}
	NclFreeDataList(dl);
	if (found)
		return;

/*
 * look through other local vars
 */			
	dl = NclGetVarList();
	for (cix = 0; testch[cix] != '\0'; cix++) {
		cp = strchr(tname,testch[cix]);
		if (! cp)
			continue;
		ix = cp - tname;
		strcpy(namebuf,tname);
		namebuf[ix] = cix < 2 ? testch[cix+2] : testch[cix-2];
		for (vinfo = dl; vinfo; vinfo = vinfo->next) {
			var = NrmQuarkToString(vinfo->u.var->name);
			if (! strcmp(var,namebuf) &&
			    ConformingVar
			    (NrmNULLQUARK,
			     vinfo->u.var->name,pub->vinfo)) {
				*secondary_var = vinfo->u.var->name;
				*secondary_is_udata = cix >= 2;
				found = True;
				break;
			}
		}
		if (found) 
			break;
	}
	NclFreeDataList(dl);

	return;
}			

static void SetVarData
(
       PlotSpecMenuRec	*priv,
       NgDataProfile	prof
)
{
	NgPlotSpecMenu	*pub = &priv->public;
	int		i,j,cix,var_dim_count = 0;
	NhlBoolean	dims_supplied[32];
	int 		primary_data_item = 0;
	int		last_dim = -1;
	int		max_dims;
	NrmQuark	secondary_file,secondary_var;
	int             secondary_data_item = -1;
	NhlBoolean	vector_data = False;

	if (prof->type == _NgSTREAMLINEPLOT ||
	    prof->type == _NgVECTORPLOT ||
	    prof->type == _NgVECTORFIELD) {
		NhlBoolean	secondary_is_u;
		vector_data = True;
		GetExtraVectorData(priv,prof,&secondary_file,&secondary_var,
			&secondary_is_u);
		if (secondary_var != NrmNULLQUARK) {
			if (secondary_is_u) {
				primary_data_item = 1;
				secondary_data_item = 0;
			}
			else {
				secondary_data_item = 1;
			}
		}
        }
	prof->master_data_ix = primary_data_item;

	for (i = 0; i < pub->vinfo->n_dims; i++) {
		if ((pub->finish[i] - pub->start[i]) /pub->stride[i] >= 1) {
			dims_supplied[i] = True;
			var_dim_count++;
			last_dim = i;
		}
		else {
			dims_supplied[i] = False;
		}
	}
	max_dims = abs(prof->ditems[primary_data_item]->n_dims);
	if (prof->ditems[primary_data_item]->n_dims == 0)
		;
	else if (var_dim_count < prof->ditems[primary_data_item]->n_dims) {
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "insufficient dimensionality for %s",
			   prof->ditems[primary_data_item]->name));
	}
	else if (var_dim_count > max_dims) {
		int dims_used = 0;
		for (i = pub->vinfo->n_dims - 1; i >= 0; i--) {
			if (dims_supplied[i]) {
				if (dims_used < max_dims)
					dims_used++;
				else 
					dims_supplied[i] = False;
			}
		}
		var_dim_count = dims_used;
	}
	for (i = 0; i < prof->n_dataitems; i++) {
		NgVarData vdata = prof->ditems[i]->vdata;
		if (i == primary_data_item) {
			if (pub->vinfo->n_dims > vdata->dims_alloced) {
				int size = pub->vinfo->n_dims * sizeof(int);
				vdata->start = NhlRealloc(vdata->start,size);
				vdata->finish = NhlRealloc(vdata->finish,size);
				vdata->stride = NhlRealloc(vdata->stride,size);
				vdata->dims_alloced = var_dim_count;
			}
			vdata->ndims = pub->vinfo->n_dims;
			for (j = 0; j < pub->vinfo->n_dims; j++) {
				if (! dims_supplied[j]) {
					vdata->finish[j] = vdata->start[j] =
						pub->start[j];
					vdata->stride[j] = 1;
					continue;
				}
				vdata->start[j] = pub->start[j];
				vdata->finish[j] = pub->finish[j];
				vdata->stride[j] = pub->stride[j];
			}
			vdata->qfile = pub->qsymbol;
			vdata->qvar = pub->vinfo->name;
			vdata->data_ix = i;
			vdata->set = True;
			vdata->new_val = True;
		}
		else if (i == secondary_data_item) {
			vdata->qfile = secondary_file;
			vdata->qvar = secondary_var;
			vdata->data_ix = i;
			if (pub->vinfo->n_dims > vdata->dims_alloced) {
				int size = pub->vinfo->n_dims * sizeof(int);
				vdata->start = NhlRealloc(vdata->start,size);
				vdata->finish = NhlRealloc(vdata->finish,size);
				vdata->stride = NhlRealloc(vdata->stride,size);
				vdata->dims_alloced = var_dim_count;
			}
			vdata->ndims = pub->vinfo->n_dims;
			for (j = 0; j < pub->vinfo->n_dims; j++) {
				if (! dims_supplied[j]) {
					vdata->finish[j] = vdata->start[j] =
						pub->start[j];
					vdata->stride[j] = 1;
					continue;
				}
				vdata->start[j] = pub->start[j];
				vdata->finish[j] = pub->finish[j];
				vdata->stride[j] = pub->stride[j];
			}
			vdata->new_val = True;
			vdata->set = False;
		}
		else if (CoordItem(prof,i)) {
			if (last_dim < 0 ||
			    pub->vinfo->coordnames[last_dim] <= NrmNULLQUARK)
				continue;
			var_dim_count = 1;
			if (var_dim_count > vdata->dims_alloced) {
				int size = var_dim_count * sizeof(int);
				vdata->start = NhlRealloc(vdata->start,size);
				vdata->finish = NhlRealloc(vdata->finish,size);
				vdata->stride = NhlRealloc(vdata->stride,size);
				vdata->dims_alloced = var_dim_count;
			}
			vdata->ndims = 1;
			vdata->start[0] = pub->start[last_dim];
			vdata->finish[0] = pub->finish[last_dim];
			vdata->stride[0] = pub->stride[last_dim];
			vdata->qfile = pub->qsymbol;
			vdata->qvar = pub->vinfo->coordnames[last_dim];
			vdata->data_ix = i;
			vdata->new_val = True;
			vdata->set = False;
			last_dim--;
			while (last_dim > -1) {
				if (dims_supplied[last_dim])
					break;
				last_dim--;
			}
		}
	}
}
	

static void CreateCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
	PlotSpecMenuRec	*priv = (PlotSpecMenuRec	*)udata;
	NgPlotSpecMenu	*pub = &priv->public;
        NgMenuRec	*plot = &priv->plot;
        NgDataProfile	prof;
	NgPlotStyle	pstyle;
        NrmQuark	qname;
        char		*vartext;
        NgPageId	page_id;
        NgHluPage	*hlu_page;
	int		i;
        
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
        if (! pstyle->class) { /* copy to a variable */
                CopyShapedVar(priv,vartext);
                return;
        }
        else {
                char buf[256];
                NhlString varname = NgNclGetSymName(priv->nsid,vartext,False);
                
                    /* create the NCL graphic variable using this name now
                       in order that it won't be "stolen" before the hlu
                       object actually gets created */
                
                sprintf(buf,"%s = new(1,graphic)\n",varname);
                (void)NgNclSubmitBlock(priv->nsid,buf);

                qname = NrmStringToQuark(vartext);
                page_id = NgOpenPage(priv->go->base.id,_brHLUVAR,&qname,1);
                if (page_id <= NgNoPage) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "unable to open hlu page"));
                        return;
                }
                hlu_page = (NgHluPage *)NgPageData(priv->go->base.id,page_id);
                if (! hlu_page) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "unable to get public page data"));
                        return;
                }

		if (NgHasDataProfile(priv->go,hlu_page->class_name)) {
			prof = NgGetDataProfile
				(priv->go,
				 pstyle->class->base_class.class_name);
			if (! prof)
				return;
			else
				prof->linked = True;
		}

#if	DEBUG_PLOTSPECMENU
		fprintf(stderr,"%s\n",prof->class_name);
#endif
		pstyle->dprof = prof;

		SetVarData(priv,prof);
		
		hlu_page->data_profile = prof;
                hlu_page->class_name = prof->class_name;
		hlu_page->plot_style = pstyle->pstyle;
		hlu_page->plot_style_dir = PlotStyleDir;

#if	DEBUG_PLOTSPECMENU
        fprintf(stderr,"setting plot style %s\n",pstyle->pstyle);
#endif

                if (NgUpdatePage(priv->go->base.id,page_id) < NhlWARNING) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "error updating hlu page"));
                        return;
                }
                (*pub->output_notify)(pub->pdata,page_id);
                
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
        if (pstyle->class) {
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
	name = NgNclGetSymName(priv->nsid,pstyle->def_symbol,True);
        if (! priv->create_dialog) {
                priv->create_dialog = XmCreateMessageDialog
                        (pub->menubar,"CreateDialog",args,nargs);
                help = XmMessageBoxGetChild
                        (priv->create_dialog,XmDIALOG_HELP_BUTTON);
                XtUnmanageChild(help);
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

static void 
UpdatePlotStyles
(
	PlotSpecMenuRec	*priv
)
{
	struct stat		statbuf;
	struct dirent		*dirp;  
	DIR			*dp;
	int			ret;
	char			*ptr;
	static	int last_count = 0;
	int	count = 0;
	char	*endp,fullpath[1024];
	int i;

	if (! PlotStyleDir) {
		PlotStyleDir = getenv("NDV_PLOT_STYLE_DIR");
		if (! PlotStyleDir) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                                   "NDV_PLOT_STYLE_DIR environment variable not set; assuming value ./plot_styles"));
			PlotStyleDir = "./plot_styles";
		}
	}

	if ((dp = opendir(PlotStyleDir)) == NULL) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Invalid plot style directory: %s",PlotStyleDir));
		return;
	}
/*
 * first just count the possibilities
 */
	while ( (dirp = readdir(dp)) != NULL) {
		char *cp;
		FILE *fp;
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
	if (count == last_count)
		return;

	PlotStyles = NhlRealloc(PlotStyles,count * sizeof(NgPlotStyleRec));
	last_count = count;
	rewinddir(dp);

	count = 0;
	strcpy(fullpath,PlotStyleDir);
	endp = fullpath + strlen(fullpath);
	*endp++ = '/';
	*endp = '\0';

	while ( (dirp = readdir(dp)) != NULL) {
		NhlBoolean gotname = False;
		NhlBoolean gotclass = False;
		char *cp;
		FILE *fp;
		char buf[256];

		if (! strcmp(dirp->d_name, ".")  ||
		    ! strcmp(dirp->d_name, ".."))
			continue;	
		if (! (cp = strrchr(dirp->d_name,'.')))
			continue;
		cp++;
		if (! cp || strcmp(cp,"res") || strlen(cp) != 3) 
			continue;
		
		strcpy(endp,dirp->d_name);
		fp = fopen(fullpath,"r");
		if (! fp) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				   "Unable to open resource file %s: ignoring",
				   dirp->d_name));
			continue;
		}
		while (cp = fgets(buf,255,fp)) {
			char *name,*np;
			NhlClass class;

			while (np = strrchr(cp,'\n'))
			       *np = '\0';
			if (! gotname &&
			    (np = strstr(buf,"*ndvPlotStyleName")) != NULL) {
				np += strlen("*ndvPlotStyleName");
				while (*np == ':' || isspace(*np))
					np++;
				if (! *np) 
					continue;
				name = NhlMalloc(strlen(np)+1);
				strcpy(name,np);
				PlotStyles[count].name = name;
				gotname = True;
			}
			if (! gotclass &&
			    (np = strstr(buf,"*ndvPlotClass")) != NULL) {
				np += strlen("*ndvPlotClass");
				while (*np == ':' || isspace(*np))
					np++;
				if (! *np) 
					continue;
				class = NgNclHluClassPtrFromName
					(priv->go->go.nclstate,np);
				if (! class) {
					NHLPERROR((NhlWARNING,NhlEUNKNOWN,
					   "Invalid class %s in %s: ignoring",
						   name,dirp->d_name));
					continue;
				}
				if (! NgHasDataProfile
				    (priv->go,class->base_class.class_name)) {
					NHLPERROR((NhlWARNING,NhlEUNKNOWN,
	       "No data profile associated with class name %s in %s: ignoring",
						   name,dirp->d_name));
					continue;
				}
				PlotStyles[count].def_symbol = 
					GetDefaultSymbol(np);

				PlotStyles[count].class = class;
				PlotStyles[count].dprof = NULL;
				gotclass = True;
			}
			if (gotname && gotclass)
				break;
		}
		if (gotclass) {
			strcpy(buf,dirp->d_name);
			cp = strrchr(buf,'.');
			*cp = '\0';
			PlotStyles[count].pstyle = 
				NhlMalloc(strlen(buf)+1);
			strcpy(PlotStyles[count].pstyle,buf);
			if (! gotname)
				PlotStyles[count].name =
                                        PlotStyles[count].pstyle;
			count++;
			continue;
		}
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			   "Plot style resources file %s invalid: ignoring",
			   dirp->d_name));
			
	}
        closedir(dp);
	PlotStyleCount = count;
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

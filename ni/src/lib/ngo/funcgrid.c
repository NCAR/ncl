/*
 *      $Id: funcgrid.c,v 1.5 2000-02-09 23:41:35 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		funcgrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Dec  6 14:01:33 MST 1999
 *
 *	Description:	
 */

#include <ncarg/ngo/funcgridP.h>
#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/shell.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include <Xm/PushBG.h>
#include <Xm/ToggleBG.h>
#include <Xm/Form.h>
#include  <ncarg/ngo/Grid.h>
#include <float.h>

static NhlBoolean 
QualifyAndInsertVariable
(
	NgFuncGridRec *fgp,
	int                 index,
	char                *var_string
);

#define SYSTEM_ERROR "System error"
#define INVALID_INPUT "Invalid input"
#define INVALID_SHAPE "Dimension size or count error"

static NhlBoolean Colors_Set = False;
static Pixel Foreground,Background;
static char *Buffer;
static int  BufSize;
static int  Max_Width;
static int  CWidth;

static Pixmap Check_Pixmap,No_Check_Pixmap,Mask_Pixmap,
	Button_Out_Pixmap,Button_In_Pixmap;

#define COL_COUNT	4

#define NAME_COL	0
#define EDIT_COL	1
#define ENABLED_COL	2
#define MODIFIED_COL	3

static NhlBoolean UpdateBufSize
(
	int req_size
)
{
	int newsize;

	if (req_size < BufSize)
		return True;

	newsize = BufSize;
	while (newsize <= req_size)
		newsize += BUFINC;
		
	Buffer = NhlRealloc(Buffer,newsize);

	if (! Buffer) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return False;
	}
	return True;
}

static int DataIndex(
	NgFuncGridRec *fgp,
	int row
)
{
	int i,vis_count = 0;

	for (i = 0; i < fgp->data_profile->n_dataitems; i++) {
		if (! fgp->data_profile->ditems[i]->vis)
			continue;
		if (vis_count == row)
			return i;
			vis_count++;
	}
	return -1;
}

static int RowIndex(
	NgFuncGridRec *fgp,
	int data_ix
)
{
	int i,row_ix = -1;

	if (data_ix >= fgp->data_profile->n_dataitems)
		return -1;
		
	for (i = 0; i <= data_ix; i++) {
		if (! fgp->data_profile->ditems[i]->vis)
			continue;
		row_ix++;
	}
	return row_ix;
}

/*
 *******************************************************************
 * func popup tool functions
 */	

static void PopupFuncToolAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec funcgridactions[] = {
	{ "PopupFuncToolAction", PopupFuncToolAction }
};

static void AdjustFuncToolGeometry
(
	NhlPointer pdata
)
{
        NgFuncGridRec	*fgp = (NgFuncGridRec *)pdata;
	Position		x;
	Dimension		w,twidth;

	return;
}

static void CompleteFuncToolEdit
(
	NgFuncGridRec *fgp
)
{
	NgFuncGrid *pub = &fgp->public;
	NhlBoolean is_new;
	int row_ix,data_ix,page_id;
	Boolean restore,enable_change_state,enabled,modified = True;
	NgDataItem ditem;
	NhlBoolean undo_edits = False;
	NgResInfo rinfo = NULL;

	NhlString new_value = NgGetFuncTreeValue
		(fgp->func_tree,&data_ix,&is_new);
	row_ix = RowIndex(fgp,fgp->data_ix);
	ditem = fgp->data_profile->ditems[fgp->data_ix];
	if (ditem)
		rinfo = ditem->res_info;

	if (! rinfo) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "internal error retrieving func tool value"));
		return;
	}
		

	XtVaGetValues(fgp->enable_tgl,
		      XmNset,&enable_change_state,
		      NULL);
	/* 
	 * first get current enable state
	 * then toggle enabled state depending on whether enable tgl set
	 */

	enabled = ditem->vdata->set_state != _NgUSER_DISABLED ?
		True : False; 
	if (enable_change_state)
		enabled = ! enabled;

	XtVaGetValues(fgp->restore_tgl,
		      XmNset,&restore,
		      NULL);
	/*
	 * if restoring, all edit changes are ignored.
	 * disabling is applied after the edit changes (if any)
	 */

	if (restore) {
		ditem->vdata->set_state = rinfo->init_state;
		ditem->vdata->cflags = _NgSYMBOL_CHANGE;
		modified = False;
	}
	else if (! new_value) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "internal error retrieving func tool value"));
		return;
	}
	else if (! is_new) {
		;	
	}
	else {
		if (enabled)
			QualifyAndInsertVariable(fgp,fgp->data_ix,new_value);
		else if (enable_change_state)
			undo_edits = True;
	}
	if (new_value)
		NhlFree(new_value);
	
	if (! enabled) {
		if (ditem->vdata->set_state != _NgUSER_DISABLED) {
			if (! restore)
				rinfo->last_state = 
					ditem->vdata->set_state;
			ditem->vdata->set_state = _NgUSER_DISABLED;
		}
		XtVaSetValues(fgp->public.grid,
			      XmNcolumn,ENABLED_COL,
			      XmNrow,row_ix,
			      XmNcellPixmap,No_Check_Pixmap,
			      NULL);
		if (rinfo->last_state == rinfo->init_state)
			modified = False;
	}
	else {
		if (ditem->vdata->set_state == _NgUSER_DISABLED) {
			ditem->vdata->set_state = rinfo->last_state;
			ditem->vdata->cflags = _NgALL_CHANGE;
		}
		XtVaSetValues(fgp->public.grid,
			      XmNcolumn,ENABLED_COL,
			      XmNrow,row_ix,
			      XmNcellPixmap,Check_Pixmap,
			      NULL);
		if (ditem->vdata->set_state == rinfo->init_state)
			modified = False;
		if (! restore)
			rinfo->last_state = ditem->vdata->set_state;
	}
	XtVaSetValues(fgp->public.grid,
		      XmNcolumn,MODIFIED_COL,
		      XmNrow,row_ix,
		      XmNcellPixmap,modified ? Check_Pixmap : No_Check_Pixmap,
		      NULL);

	if (enabled && ditem->vdata->cflags) {
		page_id = NgGetPageId
			(fgp->go->base.id,fgp->qname,NrmNULLQUARK);

		NgPostPageMessage(fgp->go->base.id,page_id,_NgNOMESSAGE,
				  _brHLUVAR,NrmNULLQUARK,fgp->qname,
				  _NgDATAPROFILE,fgp->data_profile,
				  True,NULL,True);
	}
	else if (enable_change_state) {
		NgUpdateFuncTree(fgp->func_tree,fgp->qname,
				 fgp->data_ix,fgp->data_profile,enabled);
	}

	return;
}
static void FuncToolCancelCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)udata;
	int row_ix = RowIndex(fgp,fgp->data_ix);

	XtVaSetValues(fgp->public.grid,
		      XmNcolumn,EDIT_COL,
		      XmNrow,row_ix,
		      XmNcellPixmap,Button_Out_Pixmap,
		      NULL);

	NgGOPopdown(fgp->func_tool_id);

	return;
}


static void FuncToolOKCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)udata;
	int row_ix = RowIndex(fgp,fgp->data_ix);

	XtVaSetValues(fgp->public.grid,
		      XmNcolumn,EDIT_COL,
		      XmNrow,row_ix,
		      XmNcellPixmap,Button_Out_Pixmap,
		      NULL);

	NgGOPopdown(fgp->func_tool_id);

	CompleteFuncToolEdit(fgp);

	return;
}

static void SetFuncToolState
(
	NgFuncGridRec *fgp,
	NgDataItem	    ditem
)
{
	NhlString	enable_str;
	XmString	xm_enable_str;
	NhlBoolean	enabled;
	NhlBoolean	modified;
	int		row_ix;
	NgResInfo 	rinfo = ditem->res_info;

	row_ix = RowIndex(fgp,fgp->data_ix);

	enabled = ditem->vdata->set_state != _NgUSER_DISABLED;
	if (enabled)
		modified = ditem->vdata->set_state != rinfo->init_state;
	else
		modified = rinfo->last_state != rinfo->init_state;
		

	XtVaSetValues(fgp->restore_tgl,
		      XmNset,False,
		      XmNsensitive,modified && enabled,
		      NULL);
	enable_str = enabled ? "Disable" : "Enable";
	xm_enable_str = NgXAppCreateXmString
		(fgp->go->go.appmgr,enable_str);

	XtVaSetValues(fgp->enable_tgl,
		      XmNset,False,
		      XmNlabelString,xm_enable_str,
		      NULL);
	NgXAppFreeXmString(fgp->go->go.appmgr,xm_enable_str);

}
static void FuncToolApplyCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)udata;

	CompleteFuncToolEdit(fgp);
	
	SetFuncToolState(fgp,fgp->data_profile->ditems[fgp->data_ix]);

	return;
}

static void CreateFuncTool
(
	NgGO		go,
	NhlPointer	data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)data;
	Widget apply,restore,form;
	char 		buf[256];
	NgDataItem	ditem;
	NhlBoolean	edit_enabled;

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"in create func tool\n");
#endif        

	ditem = fgp->data_profile->ditems[fgp->data_ix];
	if (! ditem)
		return;

	XtAddCallback(go->go.manager,
                      XmNokCallback,FuncToolOKCB,fgp);
	XtAddCallback(go->go.manager,
                      XmNcancelCallback,FuncToolCancelCB,fgp);

	apply = XtVaCreateManagedWidget
		("Apply",xmPushButtonGadgetClass,go->go.manager,NULL);
	XtAddCallback(apply,XmNactivateCallback,FuncToolApplyCB,fgp);

	sprintf(buf,"Edit %s",ditem->name);

	edit_enabled = ditem->vdata->set_state != _NgUSER_DISABLED;

	XtVaSetValues(go->go.shell,
		      XmNtitle,buf,
		      NULL);

	form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,go->go.manager,NULL);

	fgp->func_tree = NgCreateFuncTree(go,form,fgp->qname,
					  fgp->data_ix,
					  fgp->data_profile,edit_enabled);

	XtVaSetValues(fgp->func_tree->tree,
		      XmNbottomAttachment,XmATTACH_NONE,
		      XmNrightAttachment,XmATTACH_FORM,
		      NULL);

	fgp->restore_tgl  = XtVaCreateManagedWidget
		("Restore Defaults",
		 xmToggleButtonGadgetClass,form,
		 XmNtopOffset,8,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,fgp->func_tree->tree,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNset,False,
		 NULL);

	fgp->enable_tgl  = XtVaCreateManagedWidget
		("Disable",
		 xmToggleButtonGadgetClass,form,
		 XmNtopOffset,8,
		 XmNtopAttachment,XmATTACH_WIDGET,
		 XmNtopWidget,fgp->func_tree->tree,
		 XmNleftAttachment,XmATTACH_WIDGET,
		 XmNleftWidget,fgp->restore_tgl,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNset,False,
		 NULL);


	_NgGOWidgetTranslations(go,fgp->func_tree->tree);

	SetFuncToolState(fgp,fgp->data_profile->ditems[fgp->data_ix]);

}

static void PopupFuncTool
(
	NgFuncGridRec *fgp,
	int		    row
)
{
	char 		buf[256];
	NgResInfo	res_info;
	NgGO		func_go;
	XtPointer	rowptr;
	NgDataItem	ditem;
	XtPointer	userdata;

	rowptr =  XmLGridGetRow(fgp->public.grid,XmCONTENT,row);

	XtVaGetValues(fgp->public.grid,
		      XmNrowPtr,rowptr,
                      XmNrowUserData,&userdata,
                      NULL);
	fgp->data_ix = (int) userdata;
	ditem = fgp->data_profile->ditems[fgp->data_ix];
	if (! ditem)
		return;
	res_info = ditem->res_info;
	if (! (res_info))
		return;

	if (fgp->func_tool_id <= NhlNULLOBJID) {
		NhlVACreate(&fgp->func_tool_id,"FuncTool",
			    NgshellClass,fgp->go->base.id,
			    NgNshContentFunc,CreateFuncTool,
			    NgNshContentFuncData,fgp,
			    NULL);
	}
	else {
		NhlBoolean edit_enabled;
		func_go = (NgGO)_NhlGetLayer(fgp->func_tool_id);
		if (! func_go) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				   "error creating shapetool dialog"));
			return;
		}
		sprintf(buf,"Edit %s",ditem->name);
		XtVaSetValues(func_go->go.shell,
			      XmNtitle,buf,
			      NULL);

		SetFuncToolState(fgp,ditem);

		edit_enabled = ditem->vdata->set_state != _NgUSER_DISABLED;
		NgUpdateFuncTree(fgp->func_tree,fgp->qname,
				 fgp->data_ix,fgp->data_profile,edit_enabled);
	}
	
	NgGOPopup(fgp->func_tool_id);

}
static void PopupFuncToolAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
        NgFuncGridRec *fgp;
	XButtonEvent	*xb = (XButtonEvent *) xev;
	unsigned char	row_type,col_type;
	int		row,col;

#if DEBUG_DATA_VAR_GRID      
	fprintf(stderr,"in popup shaper action\n");
#endif
	XmLGridXYToRowColumn(w,xb->x,xb->y,&row_type,&row,&col_type,&col);

	if (row_type != XmCONTENT)
		return;

	XtVaGetValues(w,
                      XmNuserData,(void*)&fgp,
                      NULL);

	PopupFuncTool(fgp,row);

	return;
}

/*
 *************************************************************************
 * end of func popup tool functions
 */

static char *
ColumnWidths
(
	NgFuncGridRec *fgp
)
{
	int	i;
        char	sizestr[10];
        int	twidth = 0;
	Dimension	fwidth;

	XtVaGetValues(XtParent(fgp->parent),
		      XmNwidth,&fwidth,
		      NULL);
        
        Buffer[0] = '\0';
	for (i=0; i < 4; i++) {
                int width = fgp->cwidths[i];
#if 0
                if (width + twidth > Max_Width)
                        width = Max_Width - twidth;
		if (i == 1) 
			width = MAX(width,fwidth/CWidth - twidth - CWidth);
#endif
                twidth += width;
                sprintf(sizestr,"%dc ",width);
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_FUNC_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
TitleText
(
	NgFuncGridRec	*fgp
)
{
        int i,len = 0;
        
	for (i = 0; i < COL_COUNT; i++) {
		switch (i) {
		case NAME_COL:
			sprintf(&Buffer[len],"%s|","Resource Link Functions");
			fgp->cwidths[0] = strlen(Buffer) - len;
			len = strlen(Buffer);
			break;
		case EDIT_COL:
			sprintf(&Buffer[len],"%s|","Edit");
			fgp->cwidths[1] = strlen(Buffer) - len;
			len = strlen(Buffer);
			break;
		case ENABLED_COL:
			sprintf(&Buffer[len],"%s|","Enabled");
			fgp->cwidths[2] = strlen(Buffer) - len;
			len = strlen(Buffer);
			break;
		case MODIFIED_COL:
			sprintf(&Buffer[len],"%s","Modified");
			fgp->cwidths[3] = strlen(Buffer) - len;
			len = strlen(Buffer);
			break;
		}
	}
        
#if DEBUG_FUNC_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

        return Buffer;
}

static XmString
Column0String
(
	NgFuncGridRec	*fgp,
        int			dataix
)
{
	XmString xmstring;
	


	if (! (fgp->data_profile && fgp->data_profile->ditems[dataix]))
		sprintf(Buffer,"<null>");
	else if (! UpdateBufSize
		 (strlen(fgp->data_profile->ditems[dataix]->name)))
		sprintf(Buffer,"<null>");
	else {
		sprintf(Buffer,"%s",fgp->data_profile->ditems[dataix]->name);
	}

	fgp->cwidths[0] = MAX(fgp->cwidths[0],strlen(Buffer)+2);

	xmstring = NgXAppCreateXmString(fgp->go->go.appmgr,Buffer);

	return xmstring;
}

static XmString
Column1String
(
	NgFuncGridRec	*fgp,
        int			dataix
)
{
	NgVarData vdata;
	XmString xmstring;
	int i;

	vdata = fgp->data_profile->ditems[dataix]->vdata;

	if (!vdata) {
		sprintf(Buffer,"<null>");
	}
	else if (vdata->set_state == _NgUNKNOWN_DATA) {
		sprintf(Buffer,"<unknown %d-d data>",vdata->ndims);
		if (vdata->ndims)
			sprintf(&Buffer[strlen(Buffer)-1],": size (");
		for (i=0; i< vdata->ndims; i++) {
			sprintf(&Buffer[strlen(Buffer)],"%ld,",
				vdata->finish[i]+1);
		}
		if (vdata->ndims) {
			/* back up 1 to remove final comma */
			sprintf(&Buffer[strlen(Buffer)-1],")>");
		}
	}
	else if (vdata->set_state == _NgEXPRESSION || 
		 vdata->set_state == _NgUSER_EXPRESSION ||
		 vdata->set_state == _NgBOGUS_EXPRESSION) {
		if (! UpdateBufSize(strlen(vdata->expr_val)+4))
			sprintf(Buffer,"<null>");
		else
			sprintf(Buffer,vdata->expr_val);
	}
	else if (! vdata->qvar) {
		sprintf(Buffer,"<null>");
	}
	else {
		if (vdata->qfile && vdata->qvar && vdata->qcoord)
			sprintf(Buffer,"%s->%s&%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar),
				NrmQuarkToString(vdata->qcoord));
		else if (vdata->qfile && vdata->qvar)
			sprintf(Buffer,"%s->%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar));
		else if (vdata->qvar && vdata->qcoord)
			sprintf(Buffer,"%s&%s(",
				NrmQuarkToString(vdata->qvar),
				NrmQuarkToString(vdata->qcoord));
		else
			sprintf(Buffer,"%s(",
				NrmQuarkToString(vdata->qvar));

		for (i=0; i< vdata->ndims; i++) {
			if ((vdata->finish[i] - vdata->start[i])
			    /vdata->stride[i] == 0)
				sprintf(&Buffer[strlen(Buffer)],
					"%ld,",vdata->start[i]);
			else if (vdata->stride[i] == 1)
				sprintf(&Buffer[strlen(Buffer)],"%ld:%ld,",
					vdata->start[i],vdata->finish[i]);
			else
				sprintf(&Buffer[strlen(Buffer)],"%ld:%ld:%ld,",
					vdata->start[i],
					vdata->finish[i],vdata->stride[i]);
		}
		/* back up 1 to remove final comma */
		Buffer[strlen(Buffer)-1] = ')';
	}
	fgp->cwidths[1] = MAX(fgp->cwidths[1],strlen(Buffer)+10);

	xmstring = NgXAppCreateXmString(fgp->go->go.appmgr,Buffer);

	return xmstring;
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

static void
ErrorMessage(
	NgFuncGridRec *fgp,
	NhlString	    message
)
{
	XmLMessageBox(fgp->public.grid,message,True);

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

static NhlBoolean
UnshapedFileVar
(
	char *var_string,
	char **file_start,
	char **file_end,
	char **var_start,
	char **var_end
)
{
	char *cp = strstr(var_string,"->");
	char buf[512];
        int offset;
	
	if (! cp)
		return False;

	strncpy(buf,var_string,MIN(512,cp-var_string));
	buf[cp-var_string] = '\0';

	if (! QualifiedNclSymbol(buf,file_start,file_end))
		return False;

        offset = *file_start - buf;
        *file_start = var_string + offset;
        offset = *file_end - buf;
        *file_end = var_string + offset;

        strcpy(buf,cp+2);
	if (! QualifiedNclSymbol(buf,var_start,var_end))
                return False;

        offset = *var_start - buf;
        *var_start = cp + 2 + offset;
        offset = *var_end - buf;
        *var_end = cp + 2 + offset;
        
        return True;
}

static NhlBoolean
UnshapedFileCoordVar
(
	char *var_string,
	char **file_start,
	char **file_end,
	char **var_start,
	char **var_end,
	char **coord_start,
	char **coord_end
)
{
	char *cp = strstr(var_string,"->");
	char *cp1 = strstr(var_string,"&");
	char buf[512];
        int offset;
	
	if (! (cp && cp1))
		return False;

	strncpy(buf,var_string,MIN(512,cp-var_string));
	buf[cp-var_string] = '\0';

	if (! QualifiedNclSymbol(buf,file_start,file_end))
		return False;

        offset = *file_start - buf;
        *file_start = var_string + offset;
        offset = *file_end - buf;
        *file_end = var_string + offset;

        strcpy(buf,cp+2);
	buf[cp1 - cp - 2] = '\0';
	if (! QualifiedNclSymbol(buf,var_start,var_end))
                return False;

        offset = *var_start - buf;
        *var_start = cp + 2 + offset;
        offset = *var_end - buf;
        *var_end = cp + 2 + offset;
        
        strcpy(buf,cp1+1);
	if (! QualifiedNclSymbol(buf,coord_start,coord_end))
                return False;
        offset = *coord_start - buf;
        *coord_start = cp1 + 1 + offset;
        offset = *coord_end - buf;
        *coord_end = cp1 + 1 + offset;

        return True;
}
static NhlBoolean
UnshapedCoordVar
(
	char *var_string,
	char **var_start,
	char **var_end,
	char **coord_start,
	char **coord_end
)
{
	char *cp = strstr(var_string,"&");
	char buf[512];
        int offset;
	
	if (! cp)
		return False;

	strncpy(buf,var_string,MIN(512,cp-var_string));
	buf[cp-var_string] = '\0';

	if (! QualifiedNclSymbol(buf,var_start,var_end))
		return False;

        offset = *var_start - buf;
        *var_start = var_string + offset;
        offset = *var_end - buf;
        *var_end = var_string + offset;

        strcpy(buf,cp+1);
	if (! QualifiedNclSymbol(buf,coord_start,coord_end))
                return False;

        offset = *coord_start - buf;
        *coord_start = cp + 1 + offset;
        offset = *coord_end - buf;
        *coord_end = cp + 1 + offset;
        
        return True;
}

static void
GetUnshapedRegularVar
(
	NgFuncGridRec *fgp,
	char                *var,
	NrmQuark	    *qvar
)

{
	NclApiDataList *dl,*vinfo;
	
	*qvar = NrmNULLQUARK;

	dl = NclGetVarList();
	if (! dl) {
		ErrorMessage(fgp,SYSTEM_ERROR);
		return;
	}

	for (vinfo = dl; vinfo; vinfo = vinfo->next) {
		NhlString tvar = NrmQuarkToString(vinfo->u.var->name);
		if (! strcmp(tvar,var)) {
			*qvar = vinfo->u.var->name;
			NclFreeDataList(dl);
			return;
		}
	}
	NclFreeDataList(dl);
	return;
}

static void
GetUnshapedFileVar
(
	NgFuncGridRec *fgp,
	char                *file,
	char		    *var,
	NrmQuark	    *qfile,
	NrmQuark	    *qvar
)

{
	NclApiDataList *dl,*finfo;
	int i;
	
	*qvar = *qfile = NrmNULLQUARK;

	dl = NclGetFileList();
	if (! dl) {
		ErrorMessage(fgp,SYSTEM_ERROR);
		return;
	}
	for (finfo = dl; finfo; finfo = finfo->next) {
		NhlString tfile = NrmQuarkToString(finfo->u.file->name);
		if (! strcmp(tfile,file)) {
			*qfile = finfo->u.file->name;
			break;
		}
	}
	if (! *qfile)
		return;

	for (i = 0; i < finfo->u.file->n_vars; i++) {
		NhlString tvar = NrmQuarkToString(finfo->u.file->var_names[i]);
		if (!strcmp(tvar,var)) {
			*qvar = finfo->u.file->var_names[i];
			NclFreeDataList(dl);
			return;
		}
	}
	NclFreeDataList(dl);
	return;
}


static void
GetUnshapedFileCoordVar
(
	NgFuncGridRec *fgp,
	char                *file,
	char		    *var,
	char		    *coord,
	NrmQuark	    *qfile,
	NrmQuark	    *qvar,
	NrmQuark	    *qcoord
)

{
	NclApiDataList *dl,*finfo;
	int i;
	
	*qvar = *qfile = *qcoord = NrmNULLQUARK;

	dl = NclGetFileList();
	if (! dl) {
		ErrorMessage(fgp,SYSTEM_ERROR);
		return;
	}
	for (finfo = dl; finfo; finfo = finfo->next) {
		NhlString tfile = NrmQuarkToString(finfo->u.file->name);
		if (! strcmp(tfile,file)) {
			*qfile = finfo->u.file->name;
			break;
		}
	}
	if (! *qfile) {
		NclFreeDataList(dl);
		return;
	}

	for (i = 0; i < finfo->u.file->n_vars; i++) {
		NhlString tvar = NrmQuarkToString(finfo->u.file->var_names[i]);
		if (!strcmp(tvar,var)) {
			*qvar = finfo->u.file->var_names[i];
			break;
		}
	}
	NclFreeDataList(dl);

	if (! *qvar)
		return;

	dl = NclGetFileVarInfo(*qfile,*qvar);

	for (i = 0; i < dl->u.var->n_dims; i++) {
		NhlString tcoord = NrmQuarkToString(dl->u.var->coordnames[i]);
		if (!strcmp(tcoord,coord)) {
			*qcoord = dl->u.var->coordnames[i];
			NclFreeDataList(dl);
			return;
		}
	}
	NclFreeDataList(dl);
	
	return;
}


static void
GetUnshapedCoordVar
(
	NgFuncGridRec *fgp,
	char                *var,
	char		    *coord,
	NrmQuark	    *qvar,
	NrmQuark	    *qcoord
)

{
	NclApiDataList *dl,*vinfo;
	int i;
	
	*qvar = *qcoord = NrmNULLQUARK;

	dl = NclGetVarList();
	if (! dl) {
		ErrorMessage(fgp,SYSTEM_ERROR);
		return;
	}
	for (vinfo = dl; vinfo; vinfo = vinfo->next) {
		NhlString tvar = NrmQuarkToString(vinfo->u.var->name);
		if (! strcmp(tvar,var)) {
			*qvar = vinfo->u.var->name;
			break;
		}
	}
	if (! *qvar)
		return;

	for (i = 0; i < vinfo->u.var->n_dims; i++) {
		NhlString tcoord = 
			NrmQuarkToString(vinfo->u.var->coordnames[i]);
		if (tcoord && !strcmp(tcoord,coord)) {
			*qcoord = vinfo->u.var->coordnames[i];
			NclFreeDataList(dl);
			return;
		}
	}
	NclFreeDataList(dl);
	return;
}

/*
 * The explicit parameter distinguishes between an entry of all white space
 * and an entry of the string "null". All white space restores default
 * assignment of the resource value, whereas "null" means that the user intends
 * that the resource value be explicitly "null".
 */
static NhlBoolean 
EmptySymbol
(
	char            *var_string,
	NhlBoolean	*explicit
)
{
	char *sp;
	
	*explicit = False;
	sp = var_string;
	
	while (*sp != '\0' && isspace(*sp))
		sp++;
	if (*sp == '\0')
		return True;
	else if (! strncmp(sp,"null",4)) {
		sp += 4;
		while (*sp != '\0' && isspace(*sp))
			sp++;
		if (*sp == '\0') {
			*explicit = True;
			return True;
		}
	}

	return False;
}

static NhlBoolean
PossibleNclExpression
(
	char *var_string,
	char **start		/* location of first non-space character */
)
{
/*
 * Although this function does not determine that a string is actually an
 * expression, it does some basic sanity checks to eliminate some character 
 * combinations that Ncl chokes on.
 */
	char *cp = var_string;
	int  paren_level = 0;
	int  array_level = 0;
	NhlBoolean in_quote = False;

	*start = NULL;

	if (! (cp && strlen(cp)))
		return False;

	while (isspace(*cp))
		cp++;
/*
 * the first char must be a number, a letter, an underscore, a minus, or
 * an open paren. (I think that is all that qualify). No, it could be a
 * quoted string.
 */

	switch (*cp) {
	case '-':
	case '_':
		break;
	case '(':
		paren_level++;
		if (*(cp+1) && *(cp+1) == '/') 
			array_level++;
		break;
	case ')':
		paren_level--;
		break;
	case '"':
		in_quote = True;
		break;
	default:
		if (! isalnum(*cp))
			return False;
	}

	*start = cp;
	cp++;

	while (*cp) {
		if (paren_level < 0 || array_level < 0)
			return False;
		if (in_quote) {
			while (*cp &&  *cp != '"' && 
			       (isspace(*cp) || isprint(*cp)))
				cp++;
			if (! *cp == '"')
				return False;
			in_quote = False;
			cp++;
			continue;
		}
		if (isspace(*cp)) {
			cp++;
			continue;
		}
		switch (*cp) {
		case '(':
			paren_level++;
			if (*(cp+1) && *(cp+1) == '/') 
				array_level++;
			break;
		case ')':
			paren_level--;
			if (*(cp-1) == '/') 
				array_level--;
			break;	
		case '"':
			in_quote = True;
			break;
		default:
			if (! isprint(*cp))
				return False;
		}
		cp++;
	}
	if (in_quote || paren_level != 0 || array_level != 0)
		return False;
/*
 * wierd chars at the end cause the function to fail
 */

	return (*cp == '\0' ? True : False);
}

static NhlBoolean
SaveExpressionString(
	NgFuncGridRec *fgp,
	int                 index,
	char                *var_string
)
{
	NgDataProfile prof =  fgp->data_profile;
	NgVarData vdata = prof->ditems[index]->vdata;
	NhlBoolean	eval;

	eval = prof->ditems[index]->save_to_compare ? 
		_NgCONDITIONAL_EVAL : _NgNOEVAL;

	return NgSetExpressionVarData
		(fgp->go->base.id,vdata,var_string,eval,True);
}

static NhlBoolean 
QualifyAndInsertVariable
(
	NgFuncGridRec *fgp,
	int                 index,
	char                *var_string
)
{
	NgDataProfile prof =  fgp->data_profile;
	NrmQuark qfile = NrmNULLQUARK,
		qvar = NrmNULLQUARK,qcoord = NrmNULLQUARK;
	char *vsp,*vep,*fsp,*fep,*csp,*cep;
	int mix = prof->master_data_ix;
	NclApiDataList *dl = NULL;
	NgVarData vdata;
	NhlBoolean explicit;
	NgVarData last_vdata = NgNewVarData();
	NhlString message = SYSTEM_ERROR;

        if (! last_vdata)
                goto error_ret;

	vdata = prof->ditems[index]->vdata;
	NgCopyVarData(last_vdata,vdata);

	if (EmptySymbol(var_string,&explicit)) {
		if (! explicit) {
			if (! NgSetVarData
			    (NULL,vdata,NrmNULLQUARK,NrmNULLQUARK,NrmNULLQUARK,
			     0,NULL,NULL,NULL,_NgVAR_UNSET)) {
				goto error_ret;
			}
		}
		else { 
			if (! NgSetVarData
			    (NULL,vdata,NrmNULLQUARK,NrmNULLQUARK,NrmNULLQUARK,
			     0,NULL,NULL,NULL,_NgSHAPED_VAR)) {
				goto error_ret;
			}
		}

		if (prof->type == _NgXYPLOT && mix == index) {
			mix = prof->master_data_ix = (mix == 0) ? 1 : 0;
		}
		if (index == mix) {
			if (! NgSetDataProfileVar(prof,vdata,False,True))
				goto error_ret;
		}
		else if (! NgSetDependentVarData(prof,-1,False)) {
			goto error_ret;
		}
		NgFreeVarData(last_vdata);
		return True;

		/* Done with empty symbol processing */

	}
	else if (QualifiedNclSymbol(var_string,&vsp,&vep)) {
		char buf[512];
		strncpy(buf,vsp,vep-vsp);
		buf[vep-vsp] = '\0';
		GetUnshapedRegularVar(fgp,buf,&qvar);
		if (! qvar) {
			message = INVALID_INPUT;
			goto error_ret;
		}
	}
	else if (UnshapedFileVar(var_string,&fsp,&fep,&vsp,&vep)) {
		char fbuf[512],vbuf[512];
		strncpy(fbuf,fsp,fep-fsp);
		fbuf[fep-fsp] = '\0';
		strncpy(vbuf,vsp,vep-vsp);
		vbuf[vep-vsp] = '\0';

		GetUnshapedFileVar(fgp,fbuf,vbuf,&qfile,&qvar);
		if (! (qvar && qfile)) {
			message = INVALID_INPUT;
			goto error_ret;
		}
	}
	else if (UnshapedFileCoordVar
		 (var_string,&fsp,&fep,&vsp,&vep,&csp,&cep)) {
		char fbuf[512],vbuf[512],cbuf[512];
		strncpy(fbuf,fsp,fep-fsp);
		fbuf[fep-fsp] = '\0';
		strncpy(vbuf,vsp,vep-vsp);
		vbuf[vep-vsp] = '\0';
		strncpy(cbuf,csp,cep-csp);
		cbuf[cep-csp] = '\0';

		GetUnshapedFileCoordVar
			(fgp,fbuf,vbuf,cbuf,&qfile,&qvar,&qcoord);
		if (! (qvar && qfile && qcoord)) {
			message = INVALID_INPUT;
			goto error_ret;
		}
	}
	else if (UnshapedCoordVar(var_string,&vsp,&vep,&csp,&cep)) {
		char vbuf[512],cbuf[512];
		strncpy(vbuf,vsp,vep-vsp);
		vbuf[vep-vsp] = '\0';
		strncpy(cbuf,csp,cep-csp);
		cbuf[cep-csp] = '\0';

		GetUnshapedCoordVar(fgp,vbuf,cbuf,&qvar,&qcoord);
		if (! (qvar && qcoord)) {
			message = INVALID_INPUT;
			goto error_ret;
		}
	}
	else if (PossibleNclExpression(var_string,&vsp)) {
		if (! SaveExpressionString(fgp,index,vsp)) {
			message = INVALID_INPUT;
			goto error_ret;
		}
		if (index == mix) {
			if (! NgSetDataProfileVar(prof,vdata,False,True)) {
				goto error_ret;
			}
		}
		else if (! NgConformingDataItem(prof->ditems[index])) {
			NhlBoolean user = False;	
			/* 
			 * Here it's necessary to reevaluate back to 
			 * the previous expression, if there is one;
			 * Otherwise, restore last variable.
			 */

			switch (last_vdata->set_state) {
			case _NgUSER_EXPRESSION:
				user = True;
				/* drop through */
			case _NgEXPRESSION:
			case _NgBOGUS_EXPRESSION:
				if (last_vdata->expr_val) {
					NgSetExpressionVarData
						(fgp->go->base.id,vdata,
						 last_vdata->expr_val,
						 _NgCONDITIONAL_EVAL,user);
					break;
				}
				/* drop through */
			default:
				NgCopyVarData(vdata,last_vdata);
				break;
			}
			NgSetDependentVarData(prof,-1,False);
			message = INVALID_SHAPE;
			goto error_ret;
		}
		NgFreeVarData(last_vdata);
		return True;
	}
	else {
		message = INVALID_INPUT;
		goto error_ret;
	}


	if (qfile && qvar && qcoord)
		dl = NclGetFileVarCoordInfo(qfile,qvar,qcoord);
	else if (qfile && qvar)
		dl = NclGetFileVarInfo(qfile,qvar);
	else if (qvar && qcoord)
		dl = NclGetVarCoordInfo(qvar,qcoord);
	else if (qvar)
		dl = NclGetVarInfo(qvar);
	if (! dl) {
		goto error_ret;
	}

	if (index == prof->master_data_ix) {
		if (dl->u.var->n_dims < prof->ditems[index]->mindims) {
			message = INVALID_SHAPE;
			goto error_ret;
		}
		if (! NgSetVarData(dl,vdata,qfile,qvar,qcoord,
				   MIN(dl->u.var->n_dims,
				       prof->ditems[index]->maxdims),
				   NULL,NULL,NULL,_NgDEFAULT_SHAPE)) {
			goto error_ret;
		}
		else if (! NgSetDataProfileVar(prof,vdata,False,True)) {
			goto error_ret;
		}

		NgFreeVarData(last_vdata);
		return True;
	}
	/* 
	 * If we get to here the shape has been defaulted, so set the
	 * VarData to default values, and then process
	 * dependencies
	 */
	if (! NgSetVarData(dl,vdata,qfile,qvar,qcoord,
			   MIN(dl->u.var->n_dims,
			       prof->ditems[index]->maxdims),
			   NULL,NULL,NULL,_NgDEFAULT_SHAPE)) {
		goto error_ret;
	}
	if  (!NgSetDependentVarData(prof,-1,False)) {
		goto error_ret;
	}

	NgFreeVarData(last_vdata);
	return True;

 error_ret:
	ErrorMessage(fgp,message);

        if (dl)
                NclFreeDataList(dl);
        if (last_vdata)
                NgFreeVarData(last_vdata);
	return False;
}

static void
EditCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)data;
	NgFuncGrid *pub = &fgp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *) cb_data;
        XmLGridColumn colptr;
        XmLGridRow rowptr;
	char *new_string,*save_text = NULL;
	int data_ix;

#if DEBUG_FUNC_GRID
	printf("entered FuncGrid EditCB\n");
#endif


	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,cb->column);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

        switch (cb->reason) {
            case XmCR_EDIT_INSERT:
#if DEBUG_FUNC_GRID      
                    fprintf(stderr,"edit insert\n");
#endif
                    XtVaSetValues(fgp->text,
                                  XmNcursorPosition,0,
                                  XmNborderWidth,2,
                                  XmNcursorPositionVisible,True,
				  XmNbackground,fgp->go->go.select_pixel,
                                  NULL);
		    if (! fgp->text_dropped) {
			    XmTextSetInsertionPosition(fgp->text,0);
		    }
		    else {
			    char *cur_string;
			    fgp->text_dropped = False;
			    XmStringGetLtoR(fgp->edit_save_string,
					    XmFONTLIST_DEFAULT_TAG,
					    &cur_string);
			    XmTextInsert(fgp->text,0,cur_string);
			    XmTextSetInsertionPosition(fgp->text,
						       strlen(cur_string));
			    XtFree(cur_string);
		    }
		    fgp->in_edit = True;
                    return;
            case XmCR_EDIT_BEGIN:
#if DEBUG_FUNC_GRID
                    fprintf(stderr,"edit begin\n");
#endif

		    if (fgp->edit_save_string)
			    XmStringFree(fgp->edit_save_string);
                    XtVaGetValues
                            (pub->grid,
                             XmNcolumnPtr,colptr,
                             XmNrowPtr,rowptr,
                             XmNcellString,&fgp->edit_save_string,
                             NULL);
        
                    XtVaSetValues(fgp->text,
				  XmNbackground,fgp->go->go.select_pixel,
                                  NULL);
		    fgp->in_edit = True;
                    return;
            case XmCR_EDIT_CANCEL:
#if DEBUG_FUNC_GRID      
                    fprintf(stderr,"edit cancel\n");
#endif
                    XtVaSetValues(fgp->text,
				  XmNbackground,fgp->go->go.edit_field_pixel,
                                  NULL);
		    fgp->in_edit = False;
                    return;
            case XmCR_EDIT_COMPLETE:
#if DEBUG_FUNC_GRID      
                    fprintf(stderr,"edit complete\n");
#endif
		    if (! fgp->in_edit) 
			    return;

                    XtVaSetValues(fgp->text,
				  XmNbackground,fgp->go->go.edit_field_pixel,
                                  NULL);

		    fgp->in_edit = False;
                    break;
        }
/*
 * Only get here on edit complete
 */

	new_string = XmTextGetString(fgp->text);

	data_ix = DataIndex(fgp,cb->row);
	if (fgp->edit_save_string) {
		XmStringGetLtoR(fgp->edit_save_string,
				XmFONTLIST_DEFAULT_TAG,&save_text);
	}
	if (! new_string ||
	    (save_text && ! strcmp(new_string,save_text)) ||
	    ! QualifyAndInsertVariable(fgp,data_ix,new_string)) {
		if (fgp->edit_save_string)
			XtVaSetValues(pub->grid,
				      XmNcolumn,1,
				      XmNrow,cb->row,
				      XmNcellString,fgp->edit_save_string,
				      NULL);
	}
	else {
		NgDataProfile dprof = fgp->data_profile;
		int page_id;
		XmString xmstr;

		xmstr = Column0String(fgp,data_ix);

		XtVaSetValues(pub->grid,
			      XmNrow,cb->row,
			      XmNcolumn,0,
			      XmNcellString,xmstr,
			      NULL);
		NgXAppFreeXmString(fgp->go->go.appmgr,xmstr);
#if 0
		xmstr = Column1String(fgp,data_ix);

		XtVaSetValues(pub->grid,
			      XmNrow,cb->row,
			      XmNcolumn,1,
			      XmNcellString,xmstr,
			      NULL);
		NgXAppFreeXmString(fgp->go->go.appmgr,xmstr);
#endif
		page_id = NgGetPageId
			(fgp->go->base.id,fgp->qname,NrmNULLQUARK);

		NgPostPageMessage(fgp->go->base.id,page_id,_NgNOMESSAGE,
				  _brHLUVAR,NrmNULLQUARK,fgp->qname,
				  _NgDATAPROFILE,dprof,True,NULL,True);

	}
	if (save_text)
		XtFree(save_text);

	return;
}

static void
ToggleEnabled
(
	NgFuncGridRec *fgp,
	int		    row
)
{
	NgDataProfile dprof = fgp->data_profile;
	int data_ix = DataIndex(fgp,row);
	NgDataItem ditem = dprof->ditems[data_ix];
	NgResInfo rinfo = ditem->res_info;

	if (ditem->vdata->set_state != _NgUSER_DISABLED) {
		rinfo->last_state = ditem->vdata->set_state;
		ditem->vdata->set_state = _NgUSER_DISABLED;
		XtVaSetValues(fgp->public.grid,
			      XmNcolumn,ENABLED_COL,
			      XmNrow,row,
			      XmNcellPixmap,No_Check_Pixmap,
			      NULL);
	}
	else {
		ditem->vdata->set_state = rinfo->last_state;
		XtVaSetValues(fgp->public.grid,
			      XmNcolumn,ENABLED_COL,
			      XmNrow,row,
			      XmNcellPixmap,Check_Pixmap,
			      NULL);
	}
	return;
}

	
static void
SelectCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)data;
	NgFuncGrid *pub = &fgp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *) cb_data;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;

#if DEBUG_FUNC_GRID      
	fprintf(stderr,"entered FuncGrid SelectCB\n");
#endif

        if (! Colors_Set) {

                Colors_Set = True;
        
                colptr = XmLGridGetColumn(pub->grid,XmCONTENT,0);
                rowptr = XmLGridGetRow(pub->grid,XmHEADING,0);
                XtVaGetValues(pub->grid,
                              XmNcolumnPtr,colptr,
                              XmNrowPtr,rowptr,
                              XmNcellForeground,&Foreground,
                              XmNcellBackground,&Background,
                              NULL);
        }


	if (fgp->selected_row > -1) {

                    /* restore last selected */
		XtVaSetValues(pub->grid,
			      XmNcolumn,NAME_COL,
			      XmNrow,fgp->selected_row,
			      XmNcellForeground,Foreground,
			      XmNcellBackground,Background,
			      NULL);
		XtVaSetValues(pub->grid,
			      XmNcolumn,EDIT_COL,
			      XmNrow,fgp->selected_row,
			      XmNcellPixmap,Button_Out_Pixmap,
			      NULL);

		if (fgp->in_edit) {
			XmLGridEditComplete(pub->grid);
		}
	}
	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,cb->column);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

        XtVaSetValues(pub->grid,
                      XmNcolumn,NAME_COL,
                      XmNrow,cb->row,
                      XmNcellForeground,Background,
                      XmNcellBackground,Foreground,
                      NULL);
	fgp->selected_row = cb->row;

	switch (cb->column) {
	case NAME_COL:
	case MODIFIED_COL:
	case ENABLED_COL:
		return;
	case EDIT_COL:
		XtVaSetValues(pub->grid,
			      XmNcolumn,EDIT_COL,
			      XmNrow,cb->row,
			      XmNcellPixmap,Button_In_Pixmap,
			      NULL);
		PopupFuncTool(fgp,cb->row);
		break;
#if 0
	case ENABLED_COL:
		ToggleEnabled(fgp,cb->row);
#endif
	}
#if 0

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);
	if (editable) {

		if (! fgp->text_dropped) {
			if (fgp->edit_save_string)
				XmStringFree(fgp->edit_save_string);
			XtVaGetValues
				(pub->grid,
				 XmNcolumnPtr,colptr,
				 XmNrowPtr,rowptr,
				 XmNcellString,&fgp->edit_save_string,
				 NULL);
		}
		XmLGridEditBegin(pub->grid,True,cb->row,True);
	}
#endif


	return;
}


NhlErrorTypes NgSynchronizeFuncGridState
(
        NgFuncGrid	*func_grid
)
{
        NgFuncGridRec *fgp;
	NgDataItem ditem;
        NhlBoolean edit_enabled;

        fgp = (NgFuncGridRec *) func_grid;
        if (!fgp) return NhlFATAL;

	if (fgp->func_tool_id == NhlNULLOBJID)
		return NhlNOERROR;

	if (fgp->data_ix < 0) {
		NgUpdateFuncTree(fgp->func_tree,fgp->qname,
				 fgp->data_ix,fgp->data_profile,False);
		return NhlNOERROR;
	}

	XmLGridEditComplete(fgp->func_tree->tree);

	ditem = fgp->data_profile->ditems[fgp->data_ix];
	if (! ditem)
		return NhlFATAL;
	
	edit_enabled = ditem->vdata->set_state != _NgUSER_DISABLED;
	NgUpdateFuncTree(fgp->func_tree,fgp->qname,
			 fgp->data_ix,fgp->data_profile,edit_enabled);

	return NhlNOERROR;
}


NhlErrorTypes NgUpdateFuncGrid
(
        NgFuncGrid	*func_grid,
        NrmQuark		qname,
        NgDataProfile		data_profile
       )
{
        NgFuncGridRec *fgp;
        int	i;
        static NhlBoolean	first = True;
	int	row;
        
        fgp = (NgFuncGridRec *) func_grid;
        if (!fgp) return NhlFATAL;
        if (first) {
                int		root_w;
                short		cw,ch;
                XmFontList      fontlist;
                
                XtVaGetValues(func_grid->grid,
                              XmNfontList,&fontlist,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                root_w = WidthOfScreen(XtScreen(func_grid->grid));
                Max_Width = root_w / cw - cw;
		CWidth = cw;
                first = False;
        }
        fgp->data_profile = data_profile;
        fgp->qname = qname;
	fgp->vis_row_count = 0;
	fgp->data_ix = -1;
        for (i = 0; i < data_profile->n_dataitems; i++) {
		if (data_profile->ditems[i]->vis)
			fgp->vis_row_count++;
        }
	
        XtVaSetValues(func_grid->grid,
		      XmNselectionPolicy,XmSELECT_NONE,
                      XmNrows,fgp->vis_row_count,
                      NULL);
        XtVaSetValues(func_grid->grid,
		      XmNcolumn,EDIT_COL,
                      XmNrowRangeStart,0,
		      XmNrowRangeEnd,fgp->vis_row_count-1,
		      XmNcellType,XmPIXMAP_CELL,
		      XmNcellPixmap,Button_Out_Pixmap,
		      XmNcellBackground,fgp->go->go.edit_field_pixel,
                      NULL);
        XtVaSetValues(func_grid->grid,
		      XmNcolumn,ENABLED_COL,
                      XmNrowRangeStart,0,
		      XmNrowRangeEnd,fgp->vis_row_count-1,
		      XmNcellType,XmPIXMAP_CELL,
		      XmNcellPixmap,Check_Pixmap,
                      NULL);
        XtVaSetValues(func_grid->grid,
		      XmNcolumn,MODIFIED_COL,
                      XmNrowRangeStart,0,
		      XmNrowRangeEnd,fgp->vis_row_count-1,
		      XmNcellType,XmPIXMAP_CELL,
		      XmNcellPixmap,No_Check_Pixmap,
                      NULL);


        for (i = 0; i < 3; i++)
                fgp->cwidths[i] = 0;
        
        XmLGridSetStringsPos(func_grid->grid,XmHEADING,0,XmCONTENT,0,
                             TitleText(fgp));
	XtVaSetValues(func_grid->grid,
		      XmNrowType,XmHEADING,
		      XmNrow,0,
		      XmNcolumn,NAME_COL,
		      XmNcellAlignment, XmALIGNMENT_RIGHT,
		      XmNcellMarginRight,CWidth,
		      NULL);
	XtVaSetValues(func_grid->grid,
		      XmNrowType,XmHEADING,
		      XmNrow,0,
		      XmNcolumn,ENABLED_COL,
		      XmNcellAlignment, XmALIGNMENT_LEFT,
		      XmNcellMarginLeft,CWidth,
		      NULL);

	row = 0;
        for (i = 0; i < data_profile->n_dataitems; i++) {
		XmString xmstr;
		NgDataItem ditem = data_profile->ditems[i];
		NgResInfo rinfo = ditem->res_info;

		if (! ditem->vis)
			continue;

		xmstr = Column0String(fgp,i);

		XtVaSetValues(func_grid->grid,
			      XmNrow,row,
			      XmNcolumn,NAME_COL,
			      XmNcellString,xmstr,
			      XmNcellAlignment, XmALIGNMENT_RIGHT,
			      XmNcellMarginRight,CWidth,
			      XmNrowUserData,i,
			      NULL);
		NgXAppFreeXmString(fgp->go->go.appmgr,xmstr);
		if (ditem->vdata->set_state == _NgUSER_DISABLED) {
			XtVaSetValues(func_grid->grid,
				      XmNrow,row,
				      XmNcolumn,ENABLED_COL,
				      XmNcellPixmap,No_Check_Pixmap,
				      NULL);
			if (rinfo && rinfo->last_state != rinfo->init_state) {
				XtVaSetValues(func_grid->grid,
					      XmNrow,row,
					      XmNcolumn,MODIFIED_COL,
					      XmNcellPixmap,Check_Pixmap,
					      NULL);
			}
		}
		else if (rinfo && 
			 ditem->vdata->set_state != rinfo->init_state) {
			XtVaSetValues(func_grid->grid,
				      XmNrow,row,
				      XmNcolumn,MODIFIED_COL,
				      XmNcellPixmap,Check_Pixmap,
				      NULL);
		}
		row++;
        }
        XtVaSetValues(func_grid->grid,
                      XmNsimpleWidths,ColumnWidths(fgp),
                      NULL);

	if (! fgp->created) {
		fgp->created = True;
		XtMapWidget(func_grid->grid);
		XtVaSetValues(func_grid->grid,
			      XmNimmediateDraw,False,
			      NULL);
	} 

        return NhlNOERROR;
}
static void
FocusEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{

	switch (event->type) {
	case FocusOut:
#if DEBUG_FUNC_GRID      
                    fprintf(stderr,"focus out\n");
#endif
#if 0
		if (fgp->in_edit) {
			XmLGridEditComplete(fgp->public.grid);
		}
#endif
		return;
	case FocusIn:
#if DEBUG_FUNC_GRID      
                    fprintf(stderr,"focus in\n");
#endif
		break;
	}
        
	return;
}

static void StartCellDropCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)udata;
	NgFuncGrid *pub = &fgp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        XmLGridRow	rowptr;
        XmLGridColumn	colptr;

#if DEBUG_FUNC_GRID      
	fprintf(stderr,"in funcgrid start cell drop cb\n");
#endif        
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);
        colptr = XmLGridGetColumn(pub->grid,XmCONTENT,cb->column);

	if (cb->column != EDIT_COL)
		return;

	if (fgp->edit_save_string)
		XmStringFree(fgp->edit_save_string);
		
	XtVaGetValues
		(pub->grid,
		 XmNcolumnPtr,colptr,
		 XmNrowPtr,rowptr,
		 XmNcellString,&fgp->edit_save_string,
		 NULL);
	return;
}

static void CellDropCB 
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cb_data
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)udata;
	NgFuncGrid *pub = &fgp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *)cb_data;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;

#if DEBUG_FUNC_GRID      
	fprintf(stderr,"in funcgrid cell drop cb\n");
#endif        

	if (cb->column != EDIT_COL)
		return;

        if (! Colors_Set) {

                Colors_Set = True;
        
                colptr = XmLGridGetColumn(pub->grid,XmCONTENT,NAME_COL);
                rowptr = XmLGridGetRow(pub->grid,XmHEADING,0);
                XtVaGetValues(pub->grid,
                              XmNcolumnPtr,colptr,
                              XmNrowPtr,rowptr,
                              XmNcellForeground,&Foreground,
                              XmNcellBackground,&Background,
                              NULL);
        }

	if (fgp->selected_row > -1) {

                    /* restore last selected */
		XtVaSetValues(pub->grid,
			      XmNcolumn,NAME_COL,
			      XmNrow,fgp->selected_row,
			      XmNcellForeground,Foreground,
			      XmNcellBackground,Background,
			      NULL);
		if (fgp->in_edit) {
			XmLGridEditComplete(pub->grid);
		}
	}

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,EDIT_COL);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);
        XtVaSetValues(pub->grid,
                      XmNcolumn,0,
                      XmNrow,cb->row,
                      XmNcellForeground,Background,
                      XmNcellBackground,Foreground,
                      NULL);
	if (editable) {
		fgp->text_dropped = True;
		XmLGridEditBegin(pub->grid,True,cb->row,True);
	}

	fgp->selected_row = cb->row;
	
	return;
}
#if 0
static void TimeoutCB 
(
	XtPointer	data,
        XtIntervalId	*timer
        )
{
	NgFuncGridRec *fgp = (NgFuncGridRec *)data;

	XSync(fgp->go->go.x->dpy,False);
	XtVaSetValues(fgp->public.grid,
		      XmNimmediateDraw,False,
		      NULL);
}

static void
MapEH
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
        NgFuncGridRec *fgp = (NgFuncGridRec *)udata;
	Dimension	height;

	if(event->type != MapNotify)
		return;

	XtAppAddTimeOut(fgp->go->go.x->app,500,TimeoutCB,fgp);

	XtRemoveEventHandler(w,StructureNotifyMask,False,MapEH,NULL);

	return;
}
#endif
NgFuncGrid *NgCreateFuncGrid
(
	NgGO			go,
        Widget			parent,
        NrmQuark		qname,
        NgDataProfile		data_profile
        )
{
        NgFuncGridRec *fgp;
        NgFuncGrid *func_grid;
        static NhlBoolean first = True;

        if (first) {
 		NgBrowse browse = (NgBrowse) go;

                Buffer = NhlMalloc(BUFINC);
		BufSize = BUFINC;
		Check_Pixmap = browse->browse.pixmaps.check;
		No_Check_Pixmap = browse->browse.pixmaps.no_check;
		Mask_Pixmap = browse->browse.pixmaps.mask_check;
		Button_Out_Pixmap = browse->browse.pixmaps.button_out;
		Button_In_Pixmap = browse->browse.pixmaps.button_in;
                first = False;
        }
	XtAppAddActions(go->go.x->app,
                        funcgridactions,
			NhlNumber(funcgridactions));
        
        fgp = NhlMalloc(sizeof(NgFuncGridRec));
        if (!fgp) return NULL;
        func_grid = &fgp->public;
	fgp->go = go;
        fgp->data_profile = data_profile;
        fgp->qname = qname;
	fgp->created = False;
	fgp->selected_row = -1;
	fgp->parent = parent;
	fgp->in_edit = False;
	fgp->edit_save_string = NULL;
	fgp->text_dropped = False;
	fgp->func_tool_id  = NhlNULLOBJID;
	fgp->func_tree = NULL;
	fgp->data_ix = -1;
	fgp->restore_tgl = NULL;
        
        func_grid->grid = XtVaCreateManagedWidget
                ("FuncGrid",
                 xmlGridWidgetClass,parent,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNselectionPolicy,XmSELECT_NONE,
		 XmNautoSelect,False,
                 XmNcolumns,4,
                 XmNrows,0,
		 XmNimmediateDraw,True,
		 XmNmappedWhenManaged,False,
                 NULL);
        XmLGridAddRows(func_grid->grid,XmHEADING,0,1);
	XtVaSetValues(func_grid->grid,
		      XmNuserData,func_grid,
		      NULL);
#if 0
        XtAddCallback
		(func_grid->grid,XmNeditCallback,EditCB,fgp);
#endif
        XtAddCallback
		(func_grid->grid,XmNselectCallback,SelectCB,fgp);
        XtAddCallback(func_grid->grid,
		      XmNcellDropCallback,CellDropCB,fgp);
        XtAddCallback(func_grid->grid,
		      XmNcellStartDropCallback,StartCellDropCB,fgp);
	XtVaGetValues(func_grid->grid,
		      XmNtextWidget,&fgp->text,
		      NULL);
        XtAddEventHandler(fgp->text,FocusChangeMask,
                          False,FocusEH,fgp);
#if 0
        XtAddEventHandler(fgp->text,StructureNotifyMask,
                          False,MapEH,fgp);
#endif        
        return func_grid;
}

void NgDeactivateFuncGrid
(
        NgFuncGrid		*func_grid
        )
{
	NgFuncGrid	*pub = func_grid;
        NgFuncGridRec *fgp;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;
        
        fgp = (NgFuncGridRec *) pub;

	if (fgp->selected_row <= -1) 
		return;

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,EDIT_COL);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,fgp->selected_row);

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);

	XtVaSetValues(pub->grid,
		      XmNcolumn,0,
		      XmNrow,fgp->selected_row,
		      XmNcellForeground,Foreground,
		      XmNcellBackground,Background,
		      NULL);
	if (editable) {
		XtVaSetValues(pub->grid,
			      XmNcolumn,EDIT_COL,
			      XmNrow,fgp->selected_row,
			      XmNcellBackground,fgp->go->go.edit_field_pixel,
			      NULL);
		XmLGridEditCancel(pub->grid);
	}
	fgp->in_edit = False;
	XmLGridDeselectAllRows(pub->grid,False);

	fgp->selected_row = -1;

	return;

}
		
        
void NgDestroyFuncGrid
(
        NgFuncGrid		*func_grid
        )
{
        NgFuncGridRec *fgp;
        
        fgp = (NgFuncGridRec *) func_grid;
        if (!fgp) return;

	if (fgp->edit_save_string)
		XmStringFree(fgp->edit_save_string);
        NhlFree(fgp);
        
        return;
}

        

/*
 *      $Id: datasourcegrid.c,v 1.3 1999-02-23 03:56:46 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		datasourcegrid.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Sun Jun 22 14:31:22 MDT 1997
 *
 *	Description:	
 */

#include <ncarg/ngo/datasourcegridP.h>
#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>

#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Text.h>
#include  <ncarg/ngo/Grid.h>
#include <float.h>

#define SYSTEM_ERROR "System error"
#define INVALID_INPUT "Invalid input"
#define INVALID_SHAPE "Dimension size or count error"

static Pixel Foreground,Background;
static char *Buffer;
static int  Buflen;
static int  Max_Width;
static int  CWidth;
static Dimension  Row_Height;
static XmString  Edit_Save_String = NULL;
static  NrmQuark QTestVar = NrmNULLQUARK;

static int DataIndex(
	NgDataSourceGridRec *dsp,
	int row
)
{
	int i,vis_count = 0;

	for (i = 0; i < dsp->data_profile->n_dataitems; i++) {
		if (! dsp->data_profile->ditems[i]->vis)
			continue;
		if (vis_count == row)
			return i;
			vis_count++;
	}
	return -1;
}
static char *
ColumnWidths
(
	NgDataSourceGridRec *dsp
)
{
	int	i;
        char	sizestr[10];
        int	twidth = 0;
	Dimension	fwidth;

	XtVaGetValues(XtParent(dsp->parent),
		      XmNwidth,&fwidth,
		      NULL);
        
        Buffer[0] = '\0';
	for (i=0; i < 2; i++) {
                int width = dsp->cwidths[i];
                if (width + twidth > Max_Width)
                        width = Max_Width - twidth;
		if (i == 1) 
			width = MAX(width,fwidth/CWidth - twidth - CWidth);
                twidth += width;
                sprintf(sizestr,"%dc ",width);
		strcat(Buffer,sizestr);
	}
        Buffer[strlen(Buffer)-1] = '\0';
#if DEBUG_DATA_SOURCE_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        
        return Buffer;
}

static char *
TitleText
(
	NgDataSourceGridRec	*dsp
)
{
        NgDataSourceGrid data_source_grid = dsp->public;
        int len;
        
        
        sprintf(Buffer,"%s|",dsp->data_profile->class_name);
        len = dsp->cwidths[0] = strlen(Buffer);
        
        sprintf(&Buffer[len],"%s","Data Variables");
        dsp->cwidths[1] = strlen(Buffer) - len;
        
#if DEBUG_DATA_SOURCE_GRID      
        fprintf(stderr,"%s\n",Buffer);
#endif        

        return Buffer;
}

static XmString
Column0String
(
	NgDataSourceGridRec	*dsp,
        int			dataix
)
{
	XmString xmstring;

	if (! (dsp->data_profile && dsp->data_profile->ditems[dataix]))
		sprintf(Buffer,"<null>");
	else {
		sprintf(Buffer,"%s",dsp->data_profile->ditems[dataix]->name);
	}

	dsp->cwidths[0] = MAX(dsp->cwidths[0],strlen(Buffer)+2);

	xmstring = NgXAppCreateXmString(dsp->go->go.appmgr,Buffer);

	return xmstring;
}

static XmString
Column1String
(
	NgDataSourceGridRec	*dsp,
        int			dataix
)
{
        NgDataSourceGrid *pub = &dsp->public;
	NgVarData vdata;
	XmString xmstring;
	int i;

	vdata = dsp->data_profile->ditems[dataix]->vdata;

	if (!vdata) {
		sprintf(Buffer,"<null>");
	}
	else if (vdata->set_state == _NgUNKNOWN_DATA) {
		sprintf(Buffer,"<unknown %d-d data>",vdata->ndims);
		if (vdata->ndims)
			sprintf(&Buffer[strlen(Buffer)-1],": size (");
		for (i=0; i< vdata->ndims; i++) {
			sprintf(&Buffer[strlen(Buffer)],"%d,",
				vdata->finish[i]+1);
		}
		if (vdata->ndims) {
			/* back up 1 to remove final comma */
			sprintf(&Buffer[strlen(Buffer)-1],")>");
		}
	}
	else if (vdata->set_state == _NgEXPRESSION) {
		sprintf(Buffer,vdata->expr_val);
	}
	else if (! vdata->qvar) {
		sprintf(Buffer,"<null>");
	}
	else {
		if (vdata->qfile && vdata->qvar)
			sprintf(Buffer,"%s->%s(",
				NrmQuarkToString(vdata->qfile),
				NrmQuarkToString(vdata->qvar));
		else
			sprintf(Buffer,"%s(",
				NrmQuarkToString(vdata->qvar));

		for (i=0; i< vdata->ndims; i++) {
			if ((vdata->finish[i] - vdata->start[i])
			    /vdata->stride[i] == 0)
				sprintf(&Buffer[strlen(Buffer)],
					"%d,",vdata->start[i]);
			else if (vdata->stride[i] == 1)
				sprintf(&Buffer[strlen(Buffer)],"%d:%d,",
					vdata->start[i],vdata->finish[i]);
			else
				sprintf(&Buffer[strlen(Buffer)],"%d:%d:%d,",
					vdata->start[i],
					vdata->finish[i],vdata->stride[i]);
		}
		/* back up 1 to remove final comma */
		Buffer[strlen(Buffer)-1] = ')';
	}
	dsp->cwidths[1] = MAX(dsp->cwidths[1],strlen(Buffer)+10);

	xmstring = NgXAppCreateXmString(dsp->go->go.appmgr,Buffer);

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
	NgDataSourceGridRec *dsp,
	NhlString	    message
)
{
	XmLMessageBox(dsp->public.grid,message,True);

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

static void
GetUnshapedRegularVar
(
	NgDataSourceGridRec *dsp,
	char                *var,
	NrmQuark	    *qvar
)

{
	NclApiDataList *dl,*vinfo;
	
	*qvar = NrmNULLQUARK;

	dl = NclGetVarList();
	if (! dl) {
		ErrorMessage(dsp,SYSTEM_ERROR);
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
	NgDataSourceGridRec *dsp,
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
		ErrorMessage(dsp,SYSTEM_ERROR);
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
	char *sp,*ep;
	
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
	char **start		/* location of first character */
)
{
/*
 * Although this function does not determine that a string is actually an
 * expression, it is designed to eliminate wierd character combinations that
 * Ncl does not handle very well.
 */
	char *cp = var_string;

	*start = NULL;

	if (! (cp && strlen(cp)))
		return False;

	while (isspace(*cp))
		cp++;
/*
 * the first char must be a number, a letter, an underscore, a minus, or
 * an open paren. (I think that is all that qualify).
 */
	if (! (isalnum(*cp) || *cp == '_' || *cp == '-' || *cp == '('))
		return False;
	*start = cp;
	cp++;

/*
 * subsequent characters must be printable
 */

	while (isprint(*cp))
		cp++;

/*
 * wierd chars at the end cause the function to fail
 */

	return (*cp == '\0' ? True : False);
}

static NhlBoolean
SaveExpressionString(
	NgDataSourceGridRec *dsp,
	int                 index,
	char                *var_string
)
{
	NgDataProfile prof =  dsp->data_profile;
	NgVarData vdata = prof->ditems[index]->vdata;

	return NgSetExpressionVarData(dsp->go->base.id,vdata,var_string);
}

static NhlBoolean 
QualifyAndInsertVariable
(
	NgDataSourceGridRec *dsp,
	int                 index,
	char                *var_string
)
{
        NgDataSourceGrid *pub = &dsp->public;
	NgDataProfile prof =  dsp->data_profile;
	NrmQuark qfile = NrmNULLQUARK,qvar = NrmNULLQUARK;
	char *vsp,*vep,*fsp,*fep;
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
			    (NULL,vdata,NrmNULLQUARK,NrmNULLQUARK,0,
			     NULL,NULL,NULL,_NgVAR_UNSET)) {
				goto error_ret;
			}
		}
		else { 
			if (! NgSetVarData
			    (NULL,vdata,NrmNULLQUARK,NrmNULLQUARK,0,
			     NULL,NULL,NULL,_NgSHAPED_VAR)) {
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
		GetUnshapedRegularVar(dsp,buf,&qvar);
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

		GetUnshapedFileVar(dsp,fbuf,vbuf,&qfile,&qvar);
		if (! (qvar && qfile)) {
			message = INVALID_INPUT;
			goto error_ret;
		}
	}
	else if (PossibleNclExpression(var_string,&vsp)) {
		if (! SaveExpressionString(dsp,index,vsp)) {
			message = INVALID_INPUT;
			goto error_ret;
		}
		if (index == mix) {
			if (! NgSetDataProfileVar(prof,vdata,False,True)) {
				goto error_ret;
			}
		}
		else if (! NgConformingDataItem(prof->ditems[index])) {
			/* 
			 * Here it's necessary to reevaluate back to 
			 * the previous expression, if there is one;
			 * Otherwise, restore last variable.
			 */

			if (last_vdata->expr_val)
				NgSetExpressionVarData
					(dsp->go->base.id,vdata,
					 last_vdata->expr_val);
			else {
				NgCopyVarData(vdata,last_vdata);
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


	if (qfile && qvar)
		dl = NclGetFileVarInfo(qfile,qvar);
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
		if (! NgSetVarData(dl,vdata,qfile,qvar,
				   MIN(dl->u.var->n_dims,
				       prof->ditems[index]->maxdims),
				   NULL,NULL,NULL,_NgDEFAULT_SHAPE)) {
			goto error_ret;
		}
		else if (! NgSetDataProfileVar(prof,vdata,False,True)) {
			goto error_ret;
		}

		NgFreeVarData(last_vdata);
                NclFreeDataList(dl);
		return True;
	}
	/* 
	 * If we get to here the shape has been defaulted, so set the
	 * VarData to default values, and then process
	 * dependencies
	 */
	if (! NgSetVarData(dl,vdata,qfile,qvar,
			   MIN(dl->u.var->n_dims,
			       prof->ditems[index]->maxdims),
			   NULL,NULL,NULL,_NgDEFAULT_SHAPE)) {
		goto error_ret;
	}
	if  (!NgSetDependentVarData(prof,-1,False)) {
		goto error_ret;
	}

	NgFreeVarData(last_vdata);
        NclFreeDataList(dl);
	return True;

 error_ret:
	ErrorMessage(dsp,message);

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
        NgDataSourceGridRec *dsp = (NgDataSourceGridRec *)data;
	NgDataSourceGrid *pub = &dsp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *) cb_data;
        XmLGridColumn colptr;
        XmLGridRow rowptr;
        Widget text;
	char *new_string,*save_text = NULL;
	int data_ix;

#if DEBUG_DATA_SOURCE_GRID
	printf("entered EditCB\n");
#endif


	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,cb->column);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,cb->row);

        switch (cb->reason) {
            case XmCR_EDIT_BEGIN:
#if DEBUG_DATA_SOURCE_GRID
                    fprintf(stderr,"edit begin\n");
#endif

		    if (Edit_Save_String)
			    XmStringFree(Edit_Save_String);
                    XtVaGetValues
                            (pub->grid,
                             XmNcolumnPtr,colptr,
                             XmNrowPtr,rowptr,
                             XmNcellString,&Edit_Save_String,
                             NULL);
        
                    XtVaGetValues(pub->grid,
                                  XmNtextWidget,&text,
                                  NULL);
                    XtVaSetValues(text,
                                  XtVaTypedArg,XmNbackground,
                                  XmRString,"lightsalmon",12,
                                  NULL);
		    dsp->in_edit = True;
                    return;
            case XmCR_EDIT_INSERT:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"edit insert\n");
#endif
		    if (Edit_Save_String)
			    XmStringFree(Edit_Save_String);
		    Edit_Save_String = NULL;
                    XtVaGetValues
                            (pub->grid,
                             XmNcolumnPtr,colptr,
                             XmNrowPtr,rowptr,
                             XmNcellString,&Edit_Save_String,
			     NULL);

                    XtVaGetValues(pub->grid,
                                  XmNtextWidget,&text,
                                  NULL);

                    XtVaSetValues(text,
                                  XmNcursorPosition,0,
                                  XmNborderWidth,2,
                                  XmNcursorPositionVisible,True,
                                  XtVaTypedArg,XmNbackground,
                                  XmRString,"lightsalmon",12,
                                  NULL);
		    dsp->in_edit = True;
                    return;
            case XmCR_EDIT_CANCEL:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"edit cancel\n");
#endif

		    if (Edit_Save_String)
			    XmStringFree(Edit_Save_String);
		    Edit_Save_String = NULL;
                    XtVaGetValues(pub->grid,
                                  XmNtextWidget,&text,
                                  NULL);
                    XtVaSetValues(text,
                                  XtVaTypedArg,XmNbackground,
                                  XmRString,"#d0d0d0",8,
                                  NULL);
		    dsp->in_edit = False;
                    return;
            case XmCR_EDIT_COMPLETE:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"edit complete\n");
#endif

                    XtVaGetValues(pub->grid,
                                  XmNtextWidget,&text,
                                  NULL);
                    XtVaSetValues(text,
                                  XtVaTypedArg,XmNbackground,
                                  XmRString,"#d0d0d0",8,
                                  NULL);

		    dsp->in_edit = False;
                    break;
        }
/*
 * Only get here on edit complete
 */

	XtVaGetValues(pub->grid,
		      XmNtextWidget,&text,
		      NULL);
	new_string = XmTextGetString(text);

	data_ix = DataIndex(dsp,cb->row);
	if (Edit_Save_String) {
		XmStringGetLtoR
			(Edit_Save_String,XmFONTLIST_DEFAULT_TAG,&save_text);
	}
	if (! new_string ||
	    (save_text && ! strcmp(new_string,save_text)) ||
	    ! QualifyAndInsertVariable(dsp,data_ix,new_string)) {
		if (Edit_Save_String)
			XtVaSetValues(pub->grid,
				      XmNcolumn,1,
				      XmNrow,cb->row,
				      XmNcellString,Edit_Save_String,
				      NULL);
	}
	else {
		NgDataProfile dprof = dsp->data_profile;
		int mix = dprof->master_data_ix;
		int page_id;
		brPageType ptype;
		XmString xmstr;

		xmstr = Column0String(dsp,data_ix);

		XtVaSetValues(pub->grid,
			      XmNrow,cb->row,
			      XmNcolumn,0,
			      XmNcellString,xmstr,
			      NULL);
		NgXAppFreeXmString(dsp->go->go.appmgr,xmstr);

		xmstr = Column1String(dsp,data_ix);

		XtVaSetValues(pub->grid,
			      XmNrow,cb->row,
			      XmNcolumn,1,
			      XmNcellString,xmstr,
			      NULL);
		NgXAppFreeXmString(dsp->go->go.appmgr,xmstr);

		page_id = NgGetPageId
			(dsp->go->base.id,dsp->qname,NrmNULLQUARK);
		ptype = dprof->ditems[mix]->vdata->qfile != NrmNULLQUARK ? 
			_brFILEVAR : _brREGVAR;

		NgPostPageMessage(dsp->go->base.id,page_id,_NgNOMESSAGE,
				  _brHLUVAR,NrmNULLQUARK,dsp->qname,
				  _NgDATAPROFILE,dprof,True,NULL,True);

	}
	
	if (Edit_Save_String)
		XmStringFree(Edit_Save_String);
	Edit_Save_String = NULL;
	if (save_text)
		XtFree(save_text);

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
        NgDataSourceGridRec *dsp = (NgDataSourceGridRec *)data;
	NgDataSourceGrid *pub = &dsp->public;
        XmLGridCallbackStruct *cb = (XmLGridCallbackStruct *) cb_data;
	static int first = True;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;

#if DEBUG_DATA_SOURCE_GRID      
	fprintf(stderr,"entered SelectCB\n");
#endif

        if (cb->row == dsp->selected_row && Edit_Save_String)
                return;

        if (first) {

                first = False;
                
        
                colptr = XmLGridGetColumn(pub->grid,XmCONTENT,0);
                rowptr = XmLGridGetRow(pub->grid,XmHEADING,0);
                XtVaGetValues(pub->grid,
                              XmNcolumnPtr,colptr,
                              XmNrowPtr,rowptr,
                              XmNcellForeground,&Foreground,
                              XmNcellBackground,&Background,
                              NULL);
        }

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,1);

	if (dsp->selected_row > -1) {

		rowptr = XmLGridGetRow(pub->grid,XmCONTENT,dsp->selected_row);
		XtVaGetValues(pub->grid,
			      XmNcolumnPtr,colptr,
			      XmNrowPtr,rowptr,
			      XmNcellEditable,&editable,
			      NULL);

                    /* restore last selected */
		XtVaSetValues(pub->grid,
			      XmNcolumn,0,
			      XmNrow,dsp->selected_row,
			      XmNcellForeground,Foreground,
			      XmNcellBackground,Background,
			      NULL);
		if (dsp->in_edit) {
			XmLGridEditComplete(pub->grid);
		}
	}
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
		XmLGridEditBegin(pub->grid,True,cb->row,1);
	}

	dsp->selected_row = cb->row;

	return;
}


NhlErrorTypes NgUpdateDataSourceGrid
(
        NgDataSourceGrid	*data_source_grid,
        NrmQuark		qname,
        NgDataProfile		data_profile
       )
{
        NhlErrorTypes ret;
        NgDataSourceGridRec *dsp;
        int	nattrs,i;
        Dimension	height;
        NhlBoolean	first = True;
	int	row;
        
        dsp = (NgDataSourceGridRec *) data_source_grid;
        if (!dsp) return NhlFATAL;
        if (first) {
                int		root_w;
                short		cw,ch;
                XmFontList      fontlist;
                
                XtVaGetValues(data_source_grid->grid,
                              XmNfontList,&fontlist,
                              NULL);
                XmLFontListGetDimensions(fontlist,&cw,&ch,True);
                root_w = WidthOfScreen(XtScreen(data_source_grid->grid));
                Max_Width = root_w / cw - cw;
                Row_Height = ch + 2;
		CWidth = cw;
                first = False;
        }
        dsp->data_profile = data_profile;
        dsp->qname = qname;
	dsp->vis_row_count = 0;
        for (i = 0; i < data_profile->n_dataitems; i++) {
		if (data_profile->ditems[i]->vis)
			dsp->vis_row_count++;
        }
        XtVaSetValues(data_source_grid->grid,
		      XmNselectionPolicy,XmSELECT_NONE,
                      XmNrows,dsp->vis_row_count,
                      NULL);

        for (i = 0; i < 2; i++)
                dsp->cwidths[i] = 0;
        
        XmLGridSetStringsPos(data_source_grid->grid,XmHEADING,0,XmCONTENT,0,
                             TitleText(dsp));
	XtVaSetValues(data_source_grid->grid,
		      XmNrowType,XmHEADING,
		      XmNrow,0,
		      XmNcolumn,0,
		      XmNcellAlignment, XmALIGNMENT_RIGHT,
		      XmNcellMarginRight,CWidth,
		      NULL);
	XtVaSetValues(data_source_grid->grid,
		      XmNrowType,XmHEADING,
		      XmNrow,0,
		      XmNcolumn,1,
		      XmNcellAlignment, XmALIGNMENT_LEFT,
		      XmNcellMarginLeft,CWidth,
		      NULL);
	
	row = 0;
        for (i = 0; i < data_profile->n_dataitems; i++) {
		XmString xmstr;

		if (! data_profile->ditems[i]->vis)
			continue;

		xmstr = Column0String(dsp,i);

		XtVaSetValues(data_source_grid->grid,
			      XmNrow,row,
			      XmNcolumn,0,
			      XmNcellString,xmstr,
			      XmNcellAlignment, XmALIGNMENT_RIGHT,
			      XmNcellMarginRight,CWidth,
			      NULL);
		NgXAppFreeXmString(dsp->go->go.appmgr,xmstr);

		xmstr = Column1String(dsp,i);

		XtVaSetValues(data_source_grid->grid,
			      XmNrow,row,
			      XmNcolumn,1,
			      XmNcellMarginLeft,CWidth,
			      XmNcellString,xmstr,
			      XmNcellAlignment, XmALIGNMENT_LEFT,
			      XmNcellEditable,True,
			      XtVaTypedArg,XmNcellBackground,
			      XmRString,"#d0d0d0",8,
			      NULL);
		NgXAppFreeXmString(dsp->go->go.appmgr,xmstr);
		row++;
        }
        XtVaSetValues(data_source_grid->grid,
                      XmNsimpleWidths,ColumnWidths(dsp),
                      NULL);

	if (! dsp->created) {
		dsp->created = True;
		XtVaSetValues(data_source_grid->grid,
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
	NgDataSourceGridRec *dsp = (NgDataSourceGridRec *)udata;        

	switch (event->type) {
	case FocusOut:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"focus out\n");
#endif
		if (dsp->in_edit) {
			XmLGridEditComplete(dsp->public.grid);
		}
		return;
	case FocusIn:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"focus in\n");
#endif
		break;
	}
        
	return;
}

NgDataSourceGrid *NgCreateDataSourceGrid
(
	NgGO			go,
        Widget			parent,
        NrmQuark		qname,
        NgDataProfile		data_profile
        )
{
        NhlErrorTypes ret;
        NgDataSourceGridRec *dsp;
        NgDataSourceGrid *data_source_grid;
        int nattrs;
        static NhlBoolean first = True;
	Widget text;

        if (first) {
                Buffer = NhlMalloc(BUFINC);
                Buflen = BUFINC;
                first = False;
        }
        
        dsp = NhlMalloc(sizeof(NgDataSourceGridRec));
        if (!dsp) return NULL;
        data_source_grid = &dsp->public;
	dsp->go = go;
        dsp->data_profile = data_profile;
        dsp->qname = qname;
	dsp->created = False;
	dsp->selected_row = -1;
	dsp->parent = parent;
	dsp->in_edit = False;
        
        data_source_grid->grid = XtVaCreateManagedWidget
                ("DataSourceGrid",
                 xmlGridWidgetClass,parent,
                 XmNverticalSizePolicy,XmVARIABLE,
                 XmNhorizontalSizePolicy,XmVARIABLE,
                 XmNselectionPolicy,XmSELECT_NONE,
		 XmNautoSelect,False,
                 XmNcolumns,2,
                 XmNrows,0,
		 XmNimmediateDraw,True,
                 NULL);
        XmLGridAddRows(data_source_grid->grid,XmHEADING,0,1);

        XtAddCallback
		(data_source_grid->grid,XmNeditCallback,EditCB,dsp);
        XtAddCallback
		(data_source_grid->grid,XmNselectCallback,SelectCB,dsp);
	XtVaGetValues(data_source_grid->grid,
		      XmNtextWidget,&text,
		      NULL);
        XtAddEventHandler(text,FocusChangeMask,
                          False,FocusEH,dsp);
        
        return data_source_grid;
}

void NgDeactivateDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid
        )
{
	NgDataSourceGrid	*pub = data_source_grid;
        NgDataSourceGridRec *dsp;
        Boolean	editable;
	XmLGridColumn colptr;
	XmLGridRow rowptr;
        
        dsp = (NgDataSourceGridRec *) pub;

	if (dsp->selected_row <= -1) 
		return;

	colptr = XmLGridGetColumn(pub->grid,XmCONTENT,1);
	rowptr = XmLGridGetRow(pub->grid,XmCONTENT,dsp->selected_row);

	XtVaGetValues(pub->grid,
		      XmNcolumnPtr,colptr,
		      XmNrowPtr,rowptr,
		      XmNcellEditable,&editable,
		      NULL);

	XtVaSetValues(pub->grid,
		      XmNcolumn,0,
		      XmNrow,dsp->selected_row,
		      XmNcellForeground,Foreground,
		      XmNcellBackground,Background,
		      NULL);
	if (editable) {
		XtVaSetValues(pub->grid,
			      XmNcolumn,1,
			      XmNrow,dsp->selected_row,
			      XtVaTypedArg,XmNcellBackground,
			      XmRString,"#d0d0d0",8,
			      NULL);
		XmLGridEditCancel(pub->grid);
	}
	dsp->in_edit = False;
	XmLGridDeselectAllRows(pub->grid,False);

	dsp->selected_row = -1;

	return;

}
		
        
void NgDestroyDataSourceGrid
(
        NgDataSourceGrid		*data_source_grid
        )
{
        NgDataSourceGridRec *dsp;
        
        dsp = (NgDataSourceGridRec *) data_source_grid;
        if (!dsp) return;

        NhlFree(dsp);
        
        return;
}

        

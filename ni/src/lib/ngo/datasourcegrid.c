/*
 *      $Id: datasourcegrid.c,v 1.2 1999-01-11 19:36:24 dbrown Exp $
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

static Pixel Foreground,Background;
static char *Buffer;
static int  Buflen;
static int  Max_Width;
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
        
        Buffer[0] = '\0';
	for (i=0; i < 2; i++) {
                int width = dsp->cwidths[i];
                if (width + twidth > Max_Width)
                        width = Max_Width - twidth;
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

static char *
DataText
(
	NgDataSourceGridRec	*dsp,
        int			dataix
)
{
        int cwidth0,cwidth1,len;
        char buf[128];
        NgDataSourceGrid *pub = &dsp->public;
	NgVarData vdata;
	int i;
        
	sprintf(Buffer,"null|null");
	if (! (dsp->data_profile && dsp->data_profile->ditems[dataix]))
		return Buffer;

        sprintf(Buffer,"%s|",dsp->data_profile->ditems[dataix]->name);
        cwidth0 = strlen(Buffer);
        dsp->cwidths[0] = MAX(dsp->cwidths[0],cwidth0-1);
        
	vdata = dsp->data_profile->ditems[dataix]->vdata;
	if (! (vdata && vdata->qvar)) {
		sprintf(&Buffer[cwidth0],"null");
		dsp->cwidths[1] = MAX(dsp->cwidths[1],
				      strlen(Buffer)-cwidth0-1);
		return Buffer;
	}

	if (vdata->qfile && vdata->qvar)
		sprintf(&Buffer[cwidth0],"%s->%s(",
			NrmQuarkToString(vdata->qfile),
			NrmQuarkToString(vdata->qvar));
	else if (vdata->qvar)
		sprintf(&Buffer[cwidth0],"%s(",
			NrmQuarkToString(vdata->qvar));

	for (i=0; i< vdata->ndims; i++) {
		if ((vdata->finish[i] - vdata->start[i])
		    /vdata->stride[i] == 0) {
			sprintf(&Buffer[strlen(Buffer)],"%d,",vdata->start[i]);
		}
		else {
			sprintf(&Buffer[strlen(Buffer)],"%d:%d:%d,",
				vdata->start[i],
				vdata->finish[i],vdata->stride[i]);
		}
	}
	/* back up 1 to remove final comma */
	Buffer[strlen(Buffer)-1] = ')';
	dsp->cwidths[1] = MAX(dsp->cwidths[1],strlen(Buffer)-cwidth0-1);
        
        return Buffer;
}

static NhlBoolean
CoordItem(
       NgDataProfile 	prof,
       int 		index,
       int 		*coord_num
)
{
	int i;

	*coord_num = -1;
	for (i = 0; i < abs(prof->ditems[prof->master_data_ix]->n_dims); i++) {
		if (prof->coord_items[i] == index) {
			*coord_num = i;
			return True;
		}
		/* hack for XyPlot and CoordArrays */
		else if (prof->coord_items[i] < 0 &&
			 index != prof->master_data_ix) {
			*coord_num = 0;
			return True;
		}
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

static void
ErrorMessage()
{
	printf("error\n");
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
		ErrorMessage();
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
		ErrorMessage();
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
static NhlBoolean 
EmptySymbol
(
	char                *var_string
)
{
	char *sp,*ep;
	
	sp = var_string;
	
	while (*sp != '\0' && isspace(*sp))
		sp++;
	if (*sp == '\0')
		return True;
	else if (! strncmp(sp,"null",4)) {
		sp += 4;
		while (*sp != '\0' && isspace(*sp))
			sp++;
		if (*sp == '\0')
			return True;
	}

	return False;
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
	char *cp,*tcp;
	int mix = prof->master_data_ix;
	int i,coord_num;
	NclApiDataList *dl = NULL,*vinfo = NULL,*finfo = NULL;
	NgVarData mvar,vdata;

	if (EmptySymbol(var_string)) {
		vdata = prof->ditems[index]->vdata;
		vdata->qfile = vdata->qvar = NrmNULLQUARK;
		vdata->ndims = 0;
		vdata->set = vdata->new_val = True;
		if (prof->ditems[index]->required)
			return False;
		if (prof->type == _NgXYPLOT && mix == index) {
			prof->master_data_ix = (mix == 0) ? 1 : 0;
		}
		return True;
		/*******************/
	}
		
	if (QualifiedNclSymbol(var_string,&vsp,&vep)) {
		char buf[512];
		strncpy(buf,vsp,vep-vsp);
		buf[vep-vsp] = '\0';
		GetUnshapedRegularVar(dsp,buf,&qvar);
		if (! qvar)
			return False;
	}
	else if (UnshapedFileVar(var_string,&fsp,&fep,&vsp,&vep)) {
		char fbuf[512],vbuf[512];
		strncpy(fbuf,fsp,fep-fsp);
		fbuf[fep-fsp] = '\0';
		strncpy(vbuf,vsp,vep-vsp);
		vbuf[vep-vsp] = '\0';

		GetUnshapedFileVar(dsp,fbuf,vbuf,&qfile,&qvar);
		if (! (qvar && qfile)) 
			return False;
	}
#if 0
	else if (ValidExpression(var_string)) {
		SaveExpressionString(dsp,var_string);
		return True;
	}
#endif
	else {
		ErrorMessage();
		return False;
	}
		
	if (qfile && qvar)
		vinfo = NclGetFileVarInfo(qfile,qvar);
	else if (qvar)
		vinfo = NclGetVarInfo(qvar);

	if (! vinfo) {
		ErrorMessage();
		return False;
	}
	mvar = prof->ditems[mix]->vdata;
	vdata = prof->ditems[index]->vdata;

	if (prof->type == _NgXYPLOT) {
		if (index != mix) {
			if (vinfo->u.var->n_dims > mvar->ndims) {
				prof->master_data_ix = mix = index;
				mvar = vdata;
			}
		}
		else if (index == 0) {
			if (vinfo->u.var->n_dims < prof->ditems[1]->n_dims) {
				prof->master_data_ix = mix = 1;
				mvar = prof->ditems[mix]->vdata;
			}
		}
		else {
			if (vinfo->u.var->n_dims < prof->ditems[0]->n_dims) {
				prof->master_data_ix = mix = 0;
				mvar = prof->ditems[mix]->vdata;
			}
		}
	}
			

	if (! mvar->dl) {
		if (mvar == vdata)
			mvar->dl = vinfo;
		else if (mvar->qfile)
			mvar->dl = NclGetFileVarInfo(mvar->qfile,mvar->qvar);
		else
			mvar->dl = NclGetVarInfo(mvar->qvar);
		if (! mvar->dl) {
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "error setting master variable data info\n"));
		}
	}

	if (mvar == vdata) { 
		int dims_used = 0;
		/* set the master variable -- default shape is last two
		   non-unary dimensions */
		if (vinfo->u.var->n_dims < prof->ditems[mix]->n_dims) {
			ErrorMessage();
			return False;
		}
		vdata->ndims = vinfo->u.var->n_dims;
		if (vdata->dims_alloced < vdata->ndims) {
			vdata->start = NhlRealloc
				(vdata->start,vdata->ndims * sizeof(int));
			vdata->finish = NhlRealloc
				(vdata->finish,vdata->ndims * sizeof(int));
			vdata->stride = NhlRealloc
				(vdata->stride,vdata->ndims * sizeof(int));
			vdata->dims_alloced = vdata->ndims;
		}
		for (i = vdata->ndims - 1; i > -1; i--) {

			vdata->start[i] = 0;
			vdata->stride[i] = 1;
			
			if (dims_used == prof->ditems[mix]->n_dims ||
			    vinfo->u.var->dim_info[i].dim_size < 2) {
				vdata->start[i] = vdata->finish[i] = 0;
				continue;
			}
			vdata->finish[i] = 
				vinfo->u.var->dim_info[i].dim_size - 1;
			dims_used++;
		}
		vdata->qfile = qfile;
		vdata->qvar = qvar;
		vdata->data_ix = index;
		vdata->type = qfile ? FILEVAR : NORMAL;
		vdata->dl = vinfo;
		vdata->set = vdata->new_val = True;
	}
	else if (CoordItem(prof,index,&coord_num) && 
		 vinfo->u.var->n_dims == 1) {
		int coord_used = -1;
			
		/*
		 * higher coord nums mean a lower dimension number
		 */
			
		for (i = mvar->ndims-1; i > -1; i--) {
			if (abs((mvar->finish[i] - mvar->start[i]) /
				mvar->stride[i]) > 0) {
				coord_used++;
			}
			if (coord_used == coord_num)
				break;
		}
		if (mvar->dl->u.var->dim_info[i].dim_size !=
		    vinfo->u.var->dim_info[0].dim_size) {
			ErrorMessage();
			return False;
		}
		vdata->qfile = qfile;
		vdata->qvar = qvar;
		vdata->ndims = 1;
		if (vdata->dims_alloced < 1) {
			vdata->start = NhlRealloc(vdata->start,sizeof(int));
			vdata->finish = NhlRealloc(vdata->finish,sizeof(int));
			vdata->stride = NhlRealloc(vdata->stride,sizeof(int));
			vdata->dims_alloced = 1;
		}
		vdata->start[0] = mvar->start[i];
		vdata->finish[0] = mvar->finish[i];
		vdata->stride[0] = mvar->stride[i];
		vdata->data_ix = index;
		vdata->type = qfile ? FILEVAR : NORMAL;
		vdata->dl = NULL;
		vdata->new_val = vdata->set = True;
	}
	else {
		if (vinfo->u.var->n_dims != mvar->dl->u.var->n_dims) {
			ErrorMessage();
			return False;
		}
			
		for (i = 0; i < vinfo->u.var->n_dims; i++) {
			if (mvar->dl->u.var->dim_info[i].dim_size !=
			    vinfo->u.var->dim_info[i].dim_size) {
				ErrorMessage();
				return False;
			}
		}
		vdata->qfile = qfile;
		vdata->qvar = qvar;
		vdata->ndims = vinfo->u.var->n_dims;
		if (vdata->dims_alloced < vdata->ndims) {
			vdata->start = NhlRealloc
				(vdata->start,vdata->ndims * sizeof(int));
			vdata->finish = NhlRealloc
				(vdata->finish,vdata->ndims * sizeof(int));
			vdata->stride = NhlRealloc
				(vdata->stride,vdata->ndims * sizeof(int));
			vdata->dims_alloced = vdata->ndims;
		}
		memcpy(vdata->start,mvar->start,vdata->ndims * sizeof(int));
		memcpy(vdata->finish,mvar->finish,vdata->ndims * sizeof(int));
		memcpy(vdata->stride,mvar->stride,vdata->ndims * sizeof(int));
		vdata->data_ix = index;
		vdata->type = qfile ? FILEVAR : NORMAL;
		vdata->dl = NULL;
		vdata->set = vdata->new_val = True;
	}
	if (vdata->dl != vinfo)
		NclFreeDataList(vinfo);

	return True;
	       
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
	char *new_string;
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
                    return;
            case XmCR_EDIT_CANCEL:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"edit cancel\n");
#endif

		    if (Edit_Save_String)
			    XmStringFree(Edit_Save_String);
		    Edit_Save_String = NULL;
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
                    return;
            case XmCR_EDIT_COMPLETE:
#if DEBUG_DATA_SOURCE_GRID      
                    fprintf(stderr,"edit complete\n");
#endif

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
	if (! (new_string && 
		 QualifyAndInsertVariable(dsp,data_ix,new_string))) {
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

		XmLGridSetStringsPos
                        (pub->grid,XmCONTENT,cb->row,XmCONTENT,0,
                         DataText(dsp,data_ix));

		page_id = NgGetPageId
			(dsp->go->base.id,dsp->qname,NrmNULLQUARK);
		ptype = dprof->ditems[mix]->vdata->qfile != NrmNULLQUARK ? 
			_brFILEVAR : _brREGVAR;

		NgPageOutputNotify(dsp->go->base.id,page_id,ptype,
				   dprof->ditems[data_ix]->vdata);
	}
	
	if (Edit_Save_String)
		XmStringFree(Edit_Save_String);
	Edit_Save_String = NULL;

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
		if (editable)
			XtVaSetValues(pub->grid,
				      XmNcolumn,1,
				      XmNrow,dsp->selected_row,
				      XtVaTypedArg,XmNcellBackground,
				      XmRString,"#d0d0d0",8,
				      NULL);
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
		XtVaSetValues(pub->grid,
			      XmNcolumn,1,
			      XmNrow,cb->row,
			      XtVaTypedArg,XmNcellBackground,
			      XmRString,"lightsalmon",12,
			      NULL);
		XmLGridEditBegin(pub->grid,True,cb->row,1);
	}

	dsp->selected_row = cb->row;

	return;
}


static void
CellFocusCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	printf("entered CellFocusCB\n");
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
	row = 0;
        for (i = 0; i < data_profile->n_dataitems; i++) {
		if (! data_profile->ditems[i]->vis)
			continue;
                XmLGridSetStringsPos
                        (data_source_grid->grid,XmCONTENT,row,XmCONTENT,0,
                         DataText(dsp,i));
#if 0
		if (! (data_profile->linked && 
		       i == data_profile->master_data_ix))
#endif
			XtVaSetValues(data_source_grid->grid,
				      XmNcolumn,1,
				      XmNrow,row,
				      XmNcellEditable,True,
				      XtVaTypedArg,XmNcellBackground,
				      XmRString,"#d0d0d0",8,
				      NULL);
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
#if 0
        XtAddCallback
		(data_source_grid->grid,XmNcellFocusCallback,CellFocusCB,dsp);
#endif
        
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

        

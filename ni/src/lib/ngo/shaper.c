/*
 *      $Id: shaper.c,v 1.4 1997-06-23 21:06:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		shaper.c
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug  5 17:25:12 MDT 1996
 *
 *	Description:	
 */
#include <Xm/Xm.h>
#include <Xm/Protocols.h>
#include <Xm/Form.h>
#include <Xm/Frame.h>
#include <Xm/PushB.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/Text.h>
#include <Xm/TextF.h>
#include <Xm/Frame.h>
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/MessageB.h>
#include  <XmL/Grid.h>

#include <ncarg/ngo/xutil.h>
#include <ncarg/ngo/stringutil.h>
#include <ncarg/ngo/shaperP.h>


static void ShaperAction(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
);

static XtActionsRec myact[] = {
        { "ShaperAction", ShaperAction },
};

static void
UpdateCoordDataGrid
(
	NgShaper	*si
)
{
	NgShaperRec *shaper = si->shaper;
        NrmQuark qsymbol;
	int i,dim_ix = shaper->tgl_coord;
        static NhlBoolean first = True;
        static Pixel background;
	unsigned char rowtype;
	NhlBoolean all_selected = True;

        if (first) {
                XmLGridColumn colptr;
                XmLGridRow rowptr;
                int nrows,ncols;
                
                XtVaGetValues(shaper->datagrid->grid,
                              XmNrows,&nrows,
                              XmNcolumns,&ncols,
                              NULL);
                if (nrows && ncols) {
                        first = False;
                
                        colptr = XmLGridGetColumn
                                (shaper->datagrid->grid,XmCONTENT,0);
                        rowptr = XmLGridGetRow
                                (shaper->datagrid->grid,XmCONTENT,0);
                        XtVaGetValues(shaper->datagrid->grid,
                                      XmNrowPtr,rowptr,
                                      XmNcolumnPtr,colptr,
                                      XmNcellBackground,&background,
                                      NULL);
                }
                
        }
        if (! first)
                XtVaSetValues(shaper->datagrid->grid,
                              XmNcolumn,-1,
                              XmNrow,-1,
                              XmNrowType,XmALL_TYPES,
                              XmNcellBackground,background,
                              NULL);

	qsymbol = si->qfile ? si->qfile : si->vinfo->name;
	if (si->vinfo->coordnames[dim_ix] == -1) {
		char sval[32];
                
                NgUpdateDataGrid(shaper->datagrid,qsymbol,NULL,
				 &shaper->start,
				 &shaper->finish,&shaper->stride);
		rowtype = XmALL_TYPES;
	}
	else {
		if (shaper->tgl_coord_dlist &&
		    shaper->tgl_coord_dlist->u.var->name !=
		    si->vinfo->coordnames[dim_ix]) {
                        NclFreeDataList(shaper->tgl_coord_dlist);
                        shaper->tgl_coord_dlist = NULL;
                }
                if (! shaper->tgl_coord_dlist && si->qfile) {
                     shaper->tgl_coord_dlist = NclGetFileVarCoordInfo
                             (si->qfile,si->vinfo->name,
                              si->vinfo->coordnames[dim_ix]);
                }
                else if (! shaper->tgl_coord_dlist) {
                     shaper->tgl_coord_dlist = NclGetVarCoordInfo
                             (si->vinfo->name,si->vinfo->coordnames[dim_ix]);
                }
                NgUpdateDataGrid(shaper->datagrid,qsymbol,
				 shaper->tgl_coord_dlist->u.var,
				 &shaper->start,&shaper->finish,
				 &shaper->stride);
		rowtype = XmCONTENT;
	}

        if (shaper->selected_only_set) {
		XtVaSetValues(shaper->datagrid->grid,
			      XmNcolumn,-1,
			      XmNrow,-1,
			      XmNrowType,rowtype,
			      XtVaTypedArg,XmNcellBackground,
			      XmRString,"#d0d0d0",8,
			      NULL);
	}
	else {
		for (i = 0; i < si->vinfo->dim_info[dim_ix].dim_size; i++) {
                	if (shaper->selected[i])
				XtVaSetValues(shaper->datagrid->grid,
					      XmNcolumn,i,
					      XmNrow,-1,
					      XmNrowType,rowtype,
					      XtVaTypedArg,XmNcellBackground,
					      XmRString,"#d0d0d0",8,
					      NULL);
			else 
				XtVaSetValues(shaper->datagrid->grid,
					      XmNcolumn,i,
					      XmNrow,-1,
					      XmNrowType,rowtype,
					      XmNcellBackground,background,
					      NULL);
		}
        }
	for (i = 0; i < si->vinfo->dim_info[dim_ix].dim_size; i++)
		if (! shaper->selected[i]) {
			all_selected = False;
			break;
		}
	XtVaSetValues(shaper->all_selected_tgl,
		      XmNsensitive,(!all_selected),
		      NULL);
        
	return;
}

static void
UpdateShaperCoord
(
	NgShaper	*si
)
{
	NgShaperRec *shaper = si->shaper;
	Boolean set;
	int startpos = 0, finishpos = 0,end;
	int dim_ix = shaper->tgl_coord;
	int i,dimsize;
	long start,finish,stride,count;
	NhlBoolean reversed = False;
	char buf[128];
        
	if (shaper->new_coord) {
                XmString xmstr;
                sprintf(buf,"%s Values",
                        NrmQuarkToString
                        (si->vinfo->dim_info[dim_ix].dim_quark));
                xmstr = NgXAppCreateXmString(si->go->go.appmgr,buf);
                XtVaSetValues(shaper->datagrid_toggle,
                              XmNlabelString,xmstr,
                              NULL);
                NgXAppFreeXmString(si->go->go.appmgr,xmstr);
        }
        
	if (shaper->selected_only_set != 
	    shaper->coords_selected_only_set[dim_ix] && 
            XtIsManaged(shaper->all_selected_tgl))
                XtVaSetValues(shaper->all_selected_tgl,
                              XmNset,shaper->coords_selected_only_set[dim_ix],
                              NULL);
        shaper->selected_only_set = shaper->coords_selected_only_set[dim_ix];
        
	if (si->start[dim_ix] > si->finish[dim_ix])
		reversed = True;
#if 0        
	else if (si->start[dim_ix] == si->finish[dim_ix])
                reversed = shaper->reverse_set;
#endif        

	if ((reversed && ! shaper->reverse_set) ||
	    (! reversed && shaper->reverse_set)) {
		XtVaSetValues(shaper->reverse_tgl,
			      XmNset,(Boolean)reversed,
			      NULL);
		shaper->reverse_set = reversed;
	}

        if (shaper->new_coord) {
                if (shaper->selected)
                        NhlFree(shaper->selected);
                shaper->selected =
                        NhlMalloc(sizeof(NhlByte) *
                                  si->vinfo->dim_info[dim_ix].dim_size);
        }
        
        if (! reversed) {
                for (i = 0;i < si->vinfo->dim_info[dim_ix].dim_size; i++) {
                        if (i < si->start[dim_ix])
                                shaper->selected[i] = 0;
                        else if (i > si->finish[dim_ix])
                                shaper->selected[i] = 0;
                        else if (((i - si->start[dim_ix])
                                  % si->stride[dim_ix]) == 0)
                                shaper->selected[i] = 1;
                        else
                                shaper->selected[i] = 0;
                }
        }
        else {
                for (i = 0; i < si->vinfo->dim_info[dim_ix].dim_size; i++) {
                        int ix;
                        ix = si->vinfo->dim_info[dim_ix].dim_size-1-i;
                        if (ix > si->start[dim_ix]) 
                                shaper->selected[i] = 0;
                        else if (ix < si->finish[dim_ix])
                                shaper->selected[i] = 0;
                        else if (((si->start[dim_ix] - ix)
                                  % si->stride[dim_ix]) == 0)
                                shaper->selected[i] = 1;
                        else
                                shaper->selected[i] = 0;
                }
        }
	if (! shaper->selected_only_set) {
		shaper->start =
                        reversed ? si->vinfo->dim_info[dim_ix].dim_size-1 : 0;
		shaper->finish =
                        reversed ? 0 : si->vinfo->dim_info[dim_ix].dim_size-1;
		shaper->stride = 1;
	}
	else {
		shaper->start = si->start[dim_ix];
		shaper->finish = si->finish[dim_ix];
		shaper->stride = si->stride[dim_ix];
	}

        if (si->start[dim_ix] != si->finish[dim_ix]) {
                shaper->synchro_step_set = False;
                XtVaSetValues(shaper->synchro_step_tgl,
                            XmNset,False,
                            NULL);
                XtSetSensitive(shaper->synchro_step_tgl,False);
        }
        else {
                XtSetSensitive(shaper->synchro_step_tgl,True);
                shaper->synchro_step_set =
                        shaper->shapeinfogrid->synchro_step;
                XtVaSetValues(shaper->synchro_step_tgl,
                              XmNset,shaper->synchro_step_set,
                              NULL);
        }
	
	XtVaGetValues(shaper->datagrid_toggle,
		      XmNset,&set,
		      NULL);
        
	if (set && shaper->datagrid)
		UpdateCoordDataGrid(si);

	shaper->new_coord = False;

        (*si->output_notify)(si->pdata,NgNoPage);
        
	return;
 
}

static void
UpdateShape
(
	NgShaper	*si
)
{

	Arg	args[50];
	int	nargs;
	int i;
	NgShaperRec *shaper = si->shaper;
	int dim_ix;
	int t,start, finish, stride;
        Dimension frame_width, form_width;

        dim_ix = shaper->tgl_coord;
        if (dim_ix == -1) {
#if	DEBUG_SHAPER
                fprintf(stderr,"no shaping changes\n");
#endif
                return;
        }
        start = si->start[dim_ix];
        finish = si->finish[dim_ix];
        stride = si->stride[dim_ix];
        
	if (shaper->new_rev_val) {
		int tmp = start;
		if (shaper->reverse_set) {
			start = MAX(start,finish);
			finish = MIN(tmp,finish);
		}
		else {
			start = MIN(start,finish);
			finish = MAX(tmp,finish);
		}
		shaper->new_rev_val = False;
	}

	if (start != si->start[dim_ix] || finish != si->finish[dim_ix] ||
	    stride != si->stride[dim_ix]) {
		Dimension width,oldwidth;
		XmString xmstr;
		
		si->new_shape = True;
		si->start[dim_ix] = start;
		si->finish[dim_ix] = finish;
		si->stride[dim_ix] = stride;
	}

	dim_ix = shaper->tgl_coord;

	if (shaper->tgl_coord > -1) {
		UpdateShaperCoord(si);
	}
	si->new_data = False;

        shaper->shapeinfogrid->start = si->start;
        shaper->shapeinfogrid->finish = si->finish;
        shaper->shapeinfogrid->stride = si->stride;
        shaper->shapeinfogrid->selected_dim = shaper->tgl_coord;

        NgUpdateShapeInfoGrid(shaper->shapeinfogrid,si->qfile,si->vinfo);
        XtVaGetValues(si->frame,
                      XmNwidth,&frame_width,
                      NULL);
        XtVaGetValues(shaper->form,
                      XmNwidth,&form_width,
                      NULL);
        si->sub_width = frame_width - form_width;
        
	return;
}

void
DimSelectNotify
(
	NhlPointer data
        )
{
	NgShaper	*si = (NgShaper *) data;
        NgShaperRec	*shaper = si->shaper;
        
        shaper->tgl_coord = shaper->shapeinfogrid->selected_dim;
        shaper->new_coord = True;
	UpdateShaperCoord(si);

        if (si->start[shaper->tgl_coord] == si->finish[shaper->tgl_coord]) {
                shaper->synchro_step_set =
                        shaper->shapeinfogrid->synchro_step;
                XtVaSetValues(shaper->synchro_step_tgl,
                              XmNset,shaper->synchro_step_set,
                              NULL);
                XtSetSensitive(shaper->synchro_step_tgl,True);
        }
        else {
                shaper->synchro_step_set = False;
                XtVaSetValues(shaper->synchro_step_tgl,
                            XmNset,False,
                            NULL);
                XtSetSensitive(shaper->synchro_step_tgl,False);
        }
                
        (*si->geo_notify)(si->pdata);
	return;
}

void
ShapeNotify
(
        NhlPointer data
        )
{
	NgShaper	*si = (NgShaper *) data;

        UpdateShaperCoord(si);
        (*si->geo_notify)(si->pdata);
        return;
        
}

        
void 
NgShaperOn
(
	NgShaper	*si,
	NhlBoolean      on
)
{
	NgShaperRec *shaper = si->shaper;

	if (! shaper)
		return;

	if (on) {
		if (! XtIsManaged(shaper->form))
			XtManageChild(shaper->form);
	}
	else {
		if (XtIsManaged(shaper->form))
			XtUnmanageChild(shaper->form);
	}
	return;
}
static void
CancelShaperCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	Arg	args[50];
	int	nargs;

	NgShaperOn(si,False);
	return;
}

static void
ShaperCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *) cb_data;
	
	if (cb->reason == XmCR_CANCEL) {
		NgShaperOn(si,False);
		return;
	}

#if	DEBUG_SHAPER
        fprintf(stderr,"in ShaperCB\n");
#endif
	UpdateShape(si);

#if 0
	if (si->new_shape) {
		(si->apply)(si->pdata);
		si->new_shape = False;
	}
#endif
	if (cb->reason == XmCR_OK) {
		NgShaperOn(si,False);
		return;
	}
	if (si->restore) {
		NgDoShaper(si);
	}

        (*si->geo_notify)(si->pdata);
	return;
}

static void
ToggleShaperCoordCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	Arg	args[50];
	int	nargs;
	int i;
	char *address;
	NgShaperRec *shaper = si->shaper;
	Boolean set;

	UpdateShape(si);
        (*si->geo_notify)(si->pdata);
	return;
}


static void
ShaperReverseCoordsCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	int dim_ix;
	NgShaperRec *shaper = si->shaper;
	int start,finish,tmp;
	Boolean set = XmToggleButtonGadgetGetState(shaper->reverse_tgl);

	shaper->new_rev_val = True;
	shaper->reverse_set = set;

	UpdateShape(si);
        (*si->geo_notify)(si->pdata);
	return;

}

static void
ShaperSynchroStepCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	int dim_ix;
	NgShaperRec *shaper = si->shaper;
	int start,finish,tmp;
	Boolean set = XmToggleButtonGadgetGetState(shaper->synchro_step_tgl);

	shaper->synchro_step_set = set;

        NgShapeInfoGridSynchroStepMode(shaper->shapeinfogrid,set);
        
	return;

}


static void
SelectedElementsOnlyCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	Arg	args[50];
	int	nargs;
	int i;
	char *address;
	NgShaperRec *shaper = si->shaper;
	Boolean set;
	int val,size,increment,page;

	set = XmToggleButtonGadgetGetState(shaper->all_selected_tgl);

	if (shaper->tgl_coord != -1)
		shaper->coords_selected_only_set[shaper->tgl_coord] = set;
	if (shaper->selected_only_set != set) {
		shaper->selected_only_set = set;
		UpdateShaperCoord(si);
	}

	return;
}

static void
ToggleCoordGridCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	NgShaperRec 	*shaper = si->shaper;
	Boolean		set;

#if	DEBUG_SHAPER
        fprintf(stderr,"in ToggleCoordGridCB\n");
#endif
	XtVaGetValues(w,
		      XmNset,&set,
		      NULL);

	if (set && ! shaper->datagrid) {
		NrmQuark qsymbol = si->qfile ? si->qfile : si->vinfo->name;
                
		shaper->datagrid = NgCreateDataGrid
			(si->go,shaper->form,qsymbol,NULL,False,False);
                si->datagrid = shaper->datagrid;
		shaper->all_selected_tgl = XtVaCreateManagedWidget
			("Selected Elements Only",
			 xmToggleButtonGadgetClass,shaper->form,
                         NULL);
		XtAddCallback(shaper->all_selected_tgl,
			      XmNvalueChangedCallback,
			      SelectedElementsOnlyCB,si);
		shaper->selected_only_set = False;
		UpdateCoordDataGrid(si);
		XtVaSetValues(shaper->datagrid->grid,
			      XmNbottomAttachment,XmATTACH_WIDGET,
			      XmNbottomWidget,shaper->shapeinfogrid->grid,
                              XmNrightAttachment,XmATTACH_NONE,
			      XmNtopAttachment,XmATTACH_NONE,
			      NULL);
		XtVaSetValues(shaper->datagrid_toggle,
			      XmNbottomWidget,shaper->datagrid->grid,
			      NULL);
		XtVaSetValues(shaper->all_selected_tgl,
			 XmNbottomAttachment,XmATTACH_WIDGET,
			 XmNbottomWidget,shaper->datagrid->grid,
			 XmNleftAttachment,XmATTACH_WIDGET,
			 XmNleftOffset,20,
			 XmNleftWidget,shaper->datagrid_toggle,
			 XmNrightAttachment,XmATTACH_NONE,
                         XmNrightWidget,shaper->shapeinfogrid->grid,
			 XmNtopAttachment,XmATTACH_NONE,
			 NULL);
                (*si->geo_notify)(si->pdata);
	}
	else if (set) {
		XtManageChild(shaper->datagrid->grid);
		XtManageChild(shaper->all_selected_tgl);
                XtVaSetValues(shaper->all_selected_tgl,
                              XmNset,shaper->selected_only_set,
                              NULL);
                XtVaSetValues(shaper->datagrid_toggle,
			      XmNbottomWidget,shaper->datagrid->grid,
			      NULL);
		UpdateCoordDataGrid(si);
	}
	else {
		XtUnmanageChild(shaper->datagrid->grid);
		XtUnmanageChild(shaper->all_selected_tgl);
		XtVaSetValues(shaper->datagrid_toggle,
			      XmNbottomWidget,shaper->shapeinfogrid->grid,
			      NULL);
	}
        (*si->geo_notify)(si->pdata);
	return;

}
static void
ShaperCoordsorIndexesCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	Arg	args[50];
	int	nargs;
	int i;
	char *address;
	NgShaperRec *shaper = si->shaper;
	Boolean set;
	int val,size,increment,page;

	set = XmToggleButtonGadgetGetState(shaper->indexes_tgl);
        
	if (shaper->indexes_set != set) {
		shaper->indexes_set = set;
                shaper->shapeinfogrid->selected_dim = shaper->tgl_coord;
                shaper->shapeinfogrid->index_mode = set;
                NgUpdateShapeInfoGrid
                        (shaper->shapeinfogrid,si->qfile,si->vinfo);
	}

	return;
}
static void VcrCB
(
        Widget          w,
        XtPointer       data,
        XtPointer       cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	NgShaperRec 	*shaper = si->shaper;
        NgVcrControl    vcr = shaper->vcr;
	Boolean		sensitive;
        Boolean		synchro_mode_update;

#if	DEBUG_SHAPER
        fprintf(stderr,"in VcrCB\n");
#endif
        synchro_mode_update = shaper->synchro_step_set ? True : False;
        
        if (shaper->edit_timer_set) {
                XtRemoveTimeOut(shaper->edit_timer_id);
                shaper->edit_timer_set = False;
        }

        if (w == vcr->begin) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_MIN_VAL,synchro_mode_update);
        }
        else if (w == vcr->reverse && shaper->synchro_step_set) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_DECREMENT,True);
        }
        else if (w == vcr->start_stop) {
                NgShapeInfoGridEditFocusCellComplete(shaper->shapeinfogrid);
        }
        else if (w == vcr->forward && shaper->synchro_step_set) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_INCREMENT,True);
        }
        else if (w == vcr->end) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_MAX_VAL,synchro_mode_update);
        }
	sensitive = (si->start[shaper->tgl_coord] == 
		     si->finish[shaper->tgl_coord]) ? True : False;
	XtSetSensitive(shaper->synchro_step_tgl,sensitive);

        return;
}
static void EditTimeoutCB
(
	XtPointer	data,
        XtIntervalId	*timer
        )
{
	NgShaper	*si = (NgShaper *)data;
	NgShaperRec 	*shaper = si->shaper;
        NgVcrControl    vcr = shaper->vcr;
	Boolean		sensitive;
        
#if	DEBUG_SHAPER
	fprintf(stderr,"ListTimeoutCB(IN)\n");
#endif
        if (shaper->edit_how == NG_DECREMENT) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_DECREMENT,False);
        }
        else {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_INCREMENT,False);
        }
	sensitive = (si->start[shaper->tgl_coord] == 
		     si->finish[shaper->tgl_coord]) ? True : False;
	XtSetSensitive(shaper->synchro_step_tgl,sensitive);

        MAX(50,shaper->edit_timeout_value /= 1.1);
        shaper->edit_timer_set = True;
        shaper->edit_timer_id = XtAppAddTimeOut(si->go->go.x->app,
                                                shaper->edit_timeout_value,
                                                EditTimeoutCB,si);
}

static void VcrArmCB
(
        Widget          w,
        XtPointer       data,
        XtPointer       cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	NgShaperRec 	*shaper = si->shaper;
        NgVcrControl    vcr = shaper->vcr;
	Boolean		sensitive;

#if	DEBUG_SHAPER
        fprintf(stderr,"in VcrArmCB\n");
#endif

        if (w == vcr->reverse) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_DECREMENT,False);
                shaper->edit_how = NG_DECREMENT;
        }
        else if (w == vcr->forward) {
                NgShapeInfoGridEditFocusCell(shaper->shapeinfogrid,
                                             NG_INCREMENT,False);
                shaper->edit_how = NG_INCREMENT;
        }

	sensitive = (si->start[shaper->tgl_coord] == 
		     si->finish[shaper->tgl_coord]) ? True : False;
	XtSetSensitive(shaper->synchro_step_tgl,sensitive);
	  
        shaper->edit_timer_set = True;
        shaper->edit_timer_id = XtAppAddTimeOut(si->go->go.x->app,
                                            250,EditTimeoutCB,si);
        shaper->edit_timeout_value = 250;
                                            
	return;
}

void NgDeactivateShaper
(
	NgShaper	*si
)
{
	NgShaperRec *shaper = si->shaper;
        Boolean set;

        if (shaper->datagrid)
                NgDeactivateDataGrid(shaper->datagrid);
        
        XtVaGetValues(shaper->datagrid_toggle,
                      XmNset,&set,
                      NULL);
        if (set) {
                XtUnmanageChild(shaper->datagrid->grid);
                XtUnmanageChild(shaper->all_selected_tgl);
                XtVaSetValues(shaper->datagrid_toggle,
                              XmNset,False,
                              XmNbottomWidget,shaper->shapeinfogrid->grid,
                              NULL);
        }
        shaper->tgl_coord = -1;

        NgDeactivateShapeInfoGrid(shaper->shapeinfogrid);
        
        return;
}

void NgUpdateShaperCoordDataGrid
(
	NgShaper	*si
)
{
        if (si->datagrid && XtIsManaged(si->datagrid->grid))
                UpdateCoordDataGrid(si);
        return;
}

void NgDestroyShaper
(
	NgShaper	*si
)
{
	NgShaperRec *shaper = si->shaper;

        NgDeactivateShaper(si);
        NgDestroyDataGrid(shaper->datagrid);
        NgDestroyShapeInfoGrid(shaper->shapeinfogrid);
        NgDestroyVcrControl(shaper->vcr);

        if (shaper->coord_indexes_set) {
                NhlFree(shaper->coord_indexes_set);
        }
        if (shaper->selected) {
                NhlFree(shaper->selected);
        }
        if (shaper->tgl_coord_dlist) {
                NclFreeDataList(shaper->tgl_coord_dlist);
        }
        if (shaper->coords_selected_only_set) {
                NhlFree(shaper->coords_selected_only_set);
        }
        
        NhlFree(shaper);

        NhlFree(si);

        return;
}


void NgDoShaper
(
	NgShaper	*si
)
{
	Arg	args[50];
	int	nargs;
	char    buf[128] = "";
	XmString xmtitle;
	Widget  apply;
	NgShaperRec *shaper = si->shaper;
	int i, max_digits = 0;
	NhlBoolean new = False;
	Widget start_label,end_label,stride_label;
        Widget dim_elem_label2;
	static NhlBoolean first = True;
	Widget bottom_widget;
	Dimension height;
	Widget frame;
        NrmQuark qsymbol;
        NhlBoolean has_coord_vars = False;

	if (first) {
                first = False;
		XtAppAddActions(si->go->go.x->app,myact,NhlNumber(myact));
	}
	if (shaper == NULL) {
		shaper = NhlMalloc(sizeof(NgShaperRec));
		si->shaper = shaper;
		si->new_shape = False;
		si->restore = False;
                si->datagrid = NULL;
                si->sub_width = 0;

		shaper->coords_alloced = 0;
                shaper->datagrid = NULL;
                shaper->tgl_coord_dlist = NULL;
		shaper->all_selected_tgl = NULL;
		shaper->selected_only_set = False;
		shaper->indexes_tgl = NULL;
		shaper->coords_selected_only_set = NULL;
		shaper->coord_indexes_set = NULL;
		shaper->indexes_set = False;
		shaper->tgl_coord = - 1;
		shaper->new_coord = True;
		si->new_data = True;
		shaper->reverse_set = False;
		shaper->new_rev_val = False;
                shaper->selected = NULL;
                shaper->edit_timer_set = False;
		new = True;
	}
	else if (si->new_data) {
		shaper->tgl_coord = - 1;
		shaper->new_coord = True;
	}
	else if (si->restore) {
		shaper->new_coord = True;
	}

/* Main shaper form box */

	if (new) {
		nargs = 0;
                si->frame = XtCreateManagedWidget
                        ("frame",xmFrameWidgetClass,si->parent,args,nargs);
		shaper->form = XtCreateManagedWidget
                        ("form",xmFormWidgetClass,si->frame,args,nargs);
	}

/* indexes toggle */
	nargs = 0;
	if (new) {
		XtSetArg(args[nargs],XmNindicatorOn,True);nargs++;
		XtSetArg(args[nargs],XmNheight,25);nargs++;
		XtSetArg(args[nargs],XmNrecomputeSize,False);nargs++;
		XtSetArg(args[nargs],
			 XmNtopAttachment,XmATTACH_NONE);nargs++;
		XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_NONE);nargs++;
		shaper->indexes_tgl = 
			XmCreateToggleButtonGadget(shaper->form,"Index Mode",
						   args,nargs);
		XmToggleButtonGadgetSetState(shaper->indexes_tgl,
					     False,False);
		XtManageChild(shaper->indexes_tgl);
		XtAddCallback(shaper->indexes_tgl,
			      XmNvalueChangedCallback,
			      ShaperCoordsorIndexesCB,si);
	}
	else if (si->new_data) {
		XmToggleButtonGadgetSetState(shaper->indexes_tgl,
					     False,False);
		shaper->indexes_set = False;
	}
        for (i = 0; i < si->vinfo->n_dims; i++) {
                if (si->vinfo->coordnames[i] > NrmNULLQUARK) {
                        has_coord_vars = True;
                        break;
                }
        }
        if (! has_coord_vars) {
                shaper->indexes_set = True;
                XtVaSetValues(shaper->indexes_tgl,
                              XmNsensitive,False,
                              XmNset,True,
                              NULL);
        }
        else {
                shaper->indexes_set = False;
                XtVaSetValues(shaper->indexes_tgl,
                              XmNsensitive,True,
                              XmNset,False,
                              NULL);
        }

/* 'VCR' control for increment/decrement editing */

        if (new) {
                shaper->vcr = NgCreateVcrControl
                        (si->go,shaper->form,20,True,
                         True,False,True,True,True,False,True);
                XtVaSetValues(shaper->vcr->form,
                              XmNleftAttachment,XmATTACH_WIDGET,
                              XmNleftWidget,shaper->indexes_tgl,
                              XmNtopAttachment,XmATTACH_NONE,
                              XmNrightAttachment,XmATTACH_NONE,
                              NULL);
        
                XtAddCallback
                        (shaper->vcr->begin,XmNactivateCallback,VcrCB,si);
                XtAddCallback
                        (shaper->vcr->reverse,XmNactivateCallback,VcrCB,si);
                XtAddCallback
                        (shaper->vcr->reverse,XmNarmCallback,VcrArmCB,si);
                XtAddCallback
                        (shaper->vcr->start_stop,XmNactivateCallback,VcrCB,si);
                XtAddCallback
                        (shaper->vcr->forward,XmNarmCallback,VcrArmCB,si);
                XtAddCallback
                        (shaper->vcr->forward,XmNactivateCallback,VcrCB,si);
                XtAddCallback
                        (shaper->vcr->end,XmNactivateCallback,VcrCB,si);
        }
        
/* synchro step toggle button */
        
	nargs = 0;
	if (new) {
		XtSetArg(args[nargs],XmNuserData,(void*)si); nargs++;
		XtSetArg(args[nargs],
			 XmNleftAttachment,XmATTACH_WIDGET);nargs++;
		XtSetArg(args[nargs],XmNleftWidget,
			 shaper->vcr->form);nargs++;
		XtSetArg(args[nargs],
			 XmNtopAttachment,XmATTACH_NONE);nargs++;
		XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_NONE);nargs++;
		XtSetArg(args[nargs],XmNheight,25);nargs++;
		shaper->synchro_step_tgl = 
			XmCreateToggleButtonGadget(shaper->form,"Synchro Step",
						   args,nargs);
		XmToggleButtonGadgetSetState(shaper->synchro_step_tgl,
					     False,False);
		XtManageChild(shaper->synchro_step_tgl);
		XtAddCallback(shaper->synchro_step_tgl,XmNvalueChangedCallback,
			      ShaperSynchroStepCB,si);
		shaper->synchro_step_set = False;
	}
	else if (si->new_data) {
		XtSetValues(shaper->synchro_step_tgl,args,nargs);
		XmToggleButtonGadgetSetState(shaper->synchro_step_tgl,
					     False,False);
		shaper->synchro_step_set = False;
	}
        
/* Reverse toggle button */
	nargs = 0;
	if (new) {
		XtSetArg(args[nargs],XmNuserData,(void*)si); nargs++;
		XtSetArg(args[nargs],
			 XmNleftAttachment,XmATTACH_WIDGET);nargs++;
		XtSetArg(args[nargs],XmNleftWidget,
			 shaper->synchro_step_tgl);nargs++;
		XtSetArg(args[nargs],
			 XmNtopAttachment,XmATTACH_NONE);nargs++;
		XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_NONE);nargs++;
		XtSetArg(args[nargs],XmNheight,25);nargs++;
		shaper->reverse_tgl = 
			XmCreateToggleButtonGadget(shaper->form,"Reverse",
						   args,nargs);
		XmToggleButtonGadgetSetState(shaper->reverse_tgl,
					     False,False);
		XtManageChild(shaper->reverse_tgl);
		XtAddCallback(shaper->reverse_tgl,XmNvalueChangedCallback,
			      ShaperReverseCoordsCB,si);
	}
	else if (si->new_data) {
		XmToggleButtonGadgetSetState(shaper->reverse_tgl,
					     False,False);
		shaper->reverse_set = False;
		shaper->new_rev_val = False;
	}
	bottom_widget = shaper->reverse_tgl;

/* Shape Info Grid */        

        if (new) {
                shaper->shapeinfogrid = NgCreateShapeInfoGrid
                        (si->go,shaper->form,si->qfile,si->vinfo,False,False);
                XtVaSetValues(shaper->shapeinfogrid->grid,
                              XmNbottomAttachment,XmATTACH_WIDGET,
                              XmNbottomWidget,bottom_widget,
                              XmNrightAttachment,XmATTACH_NONE,
                              XmNbottomOffset,5,
                              XmNtopAttachment,XmATTACH_NONE,
                              NULL);
                bottom_widget = shaper->shapeinfogrid->grid;
                shaper->shapeinfogrid->shape_notify = ShapeNotify;
                shaper->shapeinfogrid->dim_select_notify = DimSelectNotify;
                shaper->shapeinfogrid->notify_data = si;
                shaper->shapeinfogrid->index_mode = shaper->indexes_set;
        }

/* highlighted dimension datagrid toggle */

        if (new) {
                shaper->datagrid_toggle = XtVaCreateManagedWidget
                        ("Dimension Values",
                         xmToggleButtonGadgetClass,shaper->form,
                         XmNbottomAttachment,XmATTACH_WIDGET,
                         XmNbottomWidget,bottom_widget,
                         XmNrightAttachment,XmATTACH_NONE,
                         XmNtopAttachment,XmATTACH_NONE,
                         NULL);
                XtAddCallback(shaper->datagrid_toggle,
                              XmNvalueChangedCallback,ToggleCoordGridCB,si);
        }
	else if (si->new_data) {
		XmToggleButtonGadgetSetState(shaper->datagrid_toggle,
					     False,False);
	}
        
	if (si->vinfo->n_dims > shaper->coords_alloced) {
		shaper->coords_selected_only_set = 
		  NhlRealloc(shaper->coords_selected_only_set,
			     sizeof(Boolean)*si->vinfo->n_dims);
		shaper->coord_indexes_set =
                        NhlRealloc(shaper->coord_indexes_set,
                                   sizeof(Boolean)*si->vinfo->n_dims);
	}

	if (si->new_data) {
		for (i=0;i<si->vinfo->n_dims;i++) {
			shaper->coords_selected_only_set[i] = False;
		}
		shaper->coords_alloced = 
			MAX(shaper->coords_alloced,si->vinfo->n_dims);
	}

	nargs = 0;
	if (si->new_data || shaper->new_coord) {

		if (si->new_data) {
			shaper->tgl_coord = si->vinfo->n_dims - 1;
		}
		UpdateShape(si);
		si->restore = False;
	}
	
        (*si->geo_notify)(si->pdata);
	return;
}

static void
ShaperAction
(
	Widget		w,
	XEvent		*xev,
	String		*params,
	Cardinal	*num_params
)
{
	NgShaper	*si;
	Arg	args[10];
	int	nargs;

#if	DEBUG_SHAPER
        fprintf(stderr,"in shaper action\n");
#endif
	nargs = 0;	
	XtSetArg(args[nargs],XmNuserData,(void*)&si);nargs++;
	XtGetValues(w,args,nargs);

	UpdateShape(si);
#if 0
	if (si->new_shape) {
		(si->apply)(si->pdata);
		si->new_shape = False;
	}

	if (si->restore) {
		NgDoShaper(si);
	}
#endif
	return;
}

/*
 *      $Id: shaper.c,v 1.19 1999-12-07 19:08:50 dbrown Exp $
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
#include <Xm/Label.h>
#include <Xm/LabelG.h>
#include <Xm/ToggleB.h>
#include <Xm/ToggleBG.h>
#include <Xm/MessageB.h>
#include  <ncarg/ngo/Grid.h>

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
	NgShaperRec *shaper = (NgShaperRec *)si;
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
                
                XtVaGetValues(si->datagrid->grid,
                              XmNrows,&nrows,
                              XmNcolumns,&ncols,
                              NULL);
                if (nrows && ncols) {
                        first = False;
                
                        colptr = XmLGridGetColumn
                                (si->datagrid->grid,XmCONTENT,0);
                        rowptr = XmLGridGetRow
                                (si->datagrid->grid,XmCONTENT,0);
                        XtVaGetValues(si->datagrid->grid,
                                      XmNrowPtr,rowptr,
                                      XmNcolumnPtr,colptr,
                                      XmNcellBackground,&background,
                                      NULL);
                }
                
        }
        if (! first)
                XtVaSetValues(si->datagrid->grid,
                              XmNcolumn,-1,
                              XmNrow,-1,
                              XmNrowType,XmALL_TYPES,
                              XmNcellBackground,background,
                              NULL);

	qsymbol = si->qfile ? si->qfile : si->vinfo->name;
	if (si->vinfo->coordnames[dim_ix] == -1) {
                NgUpdateDataGrid(si->datagrid,qsymbol,NULL,
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
		if (! shaper->tgl_coord_dlist) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "Internal error getting var coord info");
			return;
		}
		NgUpdateDataGrid(si->datagrid,qsymbol,
				 shaper->tgl_coord_dlist->u.var,
				 &shaper->start,&shaper->finish,
				 &shaper->stride);
		rowtype = XmCONTENT;
	}

        if (shaper->selected_only_set) {
		XtVaSetValues(si->datagrid->grid,
			      XmNcolumn,-1,
			      XmNrow,-1,
			      XmNrowType,rowtype,
			      XmNcellBackground,
			      shaper->go->go.edit_field_pixel,
			      NULL);
	}
	else {
		for (i = 0; i < si->vinfo->dim_info[dim_ix].dim_size; i++) {
                	if (shaper->selected[i])
				XtVaSetValues(si->datagrid->grid,
					      XmNcolumn,i,
					      XmNrow,-1,
					      XmNrowType,rowtype,
					      XmNcellBackground,
					      shaper->go->go.edit_field_pixel,
					      NULL);
			else 
				XtVaSetValues(si->datagrid->grid,
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
	NgShaper	*si,
	NhlBoolean	output_notify
)
{
	NgShaperRec *shaper = (NgShaperRec *)si;
	Boolean set;
	int dim_ix = shaper->tgl_coord;
	int i;
	NhlBoolean reversed = False;
	char buf[128];
        
	if (shaper->new_coord) {
                XmString xmstr;
                char *name;

                if (si->vinfo->dim_info[dim_ix].dim_quark <= NrmNULLQUARK)
                        name = "<unnamed>";
                else
                        name = NrmQuarkToString
                                (si->vinfo->dim_info[dim_ix].dim_quark);
                sprintf(buf,"%s Values",name);
                xmstr = NgXAppCreateXmString(shaper->go->go.appmgr,buf);
                XtVaSetValues(shaper->datagrid_tgl,
                              XmNlabelString,xmstr,
                              NULL);
                NgXAppFreeXmString(shaper->go->go.appmgr,xmstr);
        }
        
	if (shaper->selected_only_set !=
	    shaper->coords_selected_only_set[dim_ix] && 
	    shaper->all_selected_tgl &&
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
		shaper->synchro_step_set = 
			shaper->coords_synchro_step_set[dim_ix] = False;
		XmToggleButtonGadgetSetState
			(shaper->synchro_step_tgl,False,False);
                XtSetSensitive(shaper->synchro_step_tgl,False);
        }
        else {
                XtSetSensitive(shaper->synchro_step_tgl,True);
                shaper->synchro_step_set =
			shaper->coords_synchro_step_set[dim_ix];
		XmToggleButtonGadgetSetState
			(shaper->synchro_step_tgl,
			 shaper->synchro_step_set,False);
        }
	
	XtVaGetValues(shaper->datagrid_tgl,
		      XmNset,&set,
		      NULL);
        
	if (set && si->datagrid)
		UpdateCoordDataGrid(si);

	shaper->new_coord = False;

        if (output_notify && si->shape_notify && si->pdata)
		(*si->shape_notify)(si->pdata);
        
	return;
 
}

static void
UpdateShape
(
	NgShaper	*si,
	NhlBoolean	output_notify
)
{

	NgShaperRec *shaper = (NgShaperRec *)si;
	int dim_ix;
	int start, finish, stride;
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
		
		si->start[dim_ix] = start;
		si->finish[dim_ix] = finish;
		si->stride[dim_ix] = stride;
	}

	dim_ix = shaper->tgl_coord;

	if (shaper->tgl_coord > -1) {
		UpdateShaperCoord(si,output_notify);
	}
	shaper->new_data = False;

        si->shapeinfogrid->start = si->start;
        si->shapeinfogrid->finish = si->finish;
        si->shapeinfogrid->stride = si->stride;
        si->shapeinfogrid->selected_dim = shaper->tgl_coord;

        NgUpdateShapeInfoGrid(si->shapeinfogrid,si->qfile,si->vinfo);
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
        NgShaperRec	*shaper = (NgShaperRec *) si;
        
        shaper->tgl_coord = si->shapeinfogrid->selected_dim;
        shaper->new_coord = True;
	UpdateShaperCoord(si,False);
	NgShapeInfoGridSynchroStepMode
		(si->shapeinfogrid,shaper->synchro_step_set);

        if (si->geo_notify && si->pdata)
                (*si->geo_notify)(si->pdata);

	return;
}

/*
 * This is the primary avenue for the ShapeInfoGrid to inform the Shaper
 * of changes to the coordinate variables. The Shaper will pass this
 * information on to its parent object.
 */

void
ShapeNotify
(
        NhlPointer data
        )
{
	NgShaper	*si = (NgShaper *) data;

/*
 * output_notify is set True here 
 */
        UpdateShaperCoord(si,True);
        if (si->geo_notify && si->pdata)
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
	NgShaperRec *shaper = (NgShaperRec *)si;

	if (! shaper)
		return;

	if (on) {
		if (! XtIsManaged(si->frame))
			XtManageChild(si->frame);
	}
	else {
		if (XtIsManaged(si->frame))
			XtUnmanageChild(si->frame);
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
	NgShaperRec	*shaper = (NgShaperRec *)si;
	XmAnyCallbackStruct *cb = (XmAnyCallbackStruct *) cb_data;
	
	if (cb->reason == XmCR_CANCEL) {
		NgShaperOn(si,False);
		return;
	}

#if	DEBUG_SHAPER
        fprintf(stderr,"in ShaperCB\n");
#endif
	UpdateShape(si,False);

	if (cb->reason == XmCR_OK) {
		NgShaperOn(si,False);
		return;
	}
	if (shaper->restore) {
		NgDoShaper(si);
	}

        if (si->geo_notify && si->pdata)
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

	UpdateShape(si,False);
        if (si->geo_notify && si->pdata)
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
	NgShaperRec *shaper = (NgShaperRec *)si;
	Boolean set = XmToggleButtonGadgetGetState(shaper->reverse_tgl);

	shaper->new_rev_val = True;
	shaper->reverse_set = set;

/*
 * Reversal of coordinates needs to be passed on to the parent object.
 * So this is one of the cases where the output_notify parameter is set
 * True.
 */
	UpdateShape(si,True);
        if (si->geo_notify && si->pdata)
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
	NgShaperRec *shaper = (NgShaperRec *)si;
	Boolean set = XmToggleButtonGadgetGetState(shaper->synchro_step_tgl);

	shaper->synchro_step_set = set;
	shaper->coords_synchro_step_set[shaper->tgl_coord] = set;

        NgShapeInfoGridSynchroStepMode(si->shapeinfogrid,set);
        
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
	NgShaperRec *shaper = (NgShaperRec *)si;
	Boolean set;

	set = XmToggleButtonGadgetGetState(shaper->all_selected_tgl);

	if (shaper->tgl_coord != -1)
		shaper->coords_selected_only_set[shaper->tgl_coord] = set;
	if (shaper->selected_only_set != set) {
		shaper->selected_only_set = set;
		UpdateShaperCoord(si,False);
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
	NgShaperRec 	*shaper = (NgShaperRec *)si;
	Boolean		set;

#if	DEBUG_SHAPER
        fprintf(stderr,"in ToggleCoordGridCB\n");
#endif
	XtVaGetValues(w,
		      XmNset,&set,
		      NULL);

	if (set && ! si->datagrid) {
		NrmQuark qsymbol = si->qfile ? si->qfile : si->vinfo->name;
                
		si->datagrid = NgCreateDataGrid
			(shaper->go,shaper->form,qsymbol,NULL,False,False);
		shaper->all_selected_tgl = XtVaCreateManagedWidget
			("Selected Elements Only",
			 xmToggleButtonGadgetClass,shaper->form,
                         NULL);
		XtAddCallback(shaper->all_selected_tgl,
			      XmNvalueChangedCallback,
			      SelectedElementsOnlyCB,si);
		UpdateCoordDataGrid(si);
		XtVaSetValues(si->datagrid->grid,
			      XmNbottomAttachment,XmATTACH_WIDGET,
			      XmNbottomWidget,si->shapeinfogrid->grid,
                              XmNrightAttachment,XmATTACH_NONE,
			      XmNtopAttachment,XmATTACH_NONE,
			      NULL);
		XtVaSetValues(shaper->datagrid_tgl,
			      XmNbottomWidget,si->datagrid->grid,
			      NULL);
		XtVaSetValues(shaper->all_selected_tgl,
			 XmNbottomAttachment,XmATTACH_WIDGET,
			 XmNbottomWidget,si->datagrid->grid,
			 XmNleftAttachment,XmATTACH_WIDGET,
			 XmNleftOffset,20,
			 XmNleftWidget,shaper->datagrid_tgl,
			 XmNrightAttachment,XmATTACH_NONE,
                         XmNrightWidget,si->shapeinfogrid->grid,
			 XmNtopAttachment,XmATTACH_NONE,
			 NULL);
		_NgGOWidgetTranslations(shaper->go,shaper->all_selected_tgl);
		_NgGOWidgetTranslations(shaper->go,si->datagrid->grid);
	}
	else if (set) {
		XtManageChild(si->datagrid->grid);
		XtManageChild(shaper->all_selected_tgl);
                XtVaSetValues(shaper->all_selected_tgl,
                              XmNset,shaper->selected_only_set,
                              NULL);
                XtVaSetValues(shaper->datagrid_tgl,
			      XmNbottomWidget,si->datagrid->grid,
			      NULL);
		UpdateCoordDataGrid(si);
	}
	else {
		XtUnmanageChild(si->datagrid->grid);
		XtUnmanageChild(shaper->all_selected_tgl);
		XtVaSetValues(shaper->datagrid_tgl,
			      XmNbottomWidget,si->shapeinfogrid->grid,
			      NULL);
	}
        if (si->geo_notify && si->pdata)
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
	NgShaperRec *shaper = (NgShaperRec *)si;
	Boolean set;

	set = XmToggleButtonGadgetGetState(shaper->indexes_tgl);
        
	if (shaper->indexes_set != set) {
		shaper->indexes_set = set;
                si->shapeinfogrid->selected_dim = shaper->tgl_coord;
                si->shapeinfogrid->index_mode = set;
                NgUpdateShapeInfoGrid
                        (si->shapeinfogrid,si->qfile,si->vinfo);
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
	NgShaperRec 	*shaper = (NgShaperRec *)si;
        NgVcrControl    vcr = shaper->vcr;
	Boolean		sensitive;
        Boolean		synchro_mode_update;

#if	DEBUG_SHAPER
        fprintf(stderr,"in VcrCB\n");
#endif
        synchro_mode_update = shaper->synchro_step_set ? True : False;
        
        if (shaper->edit_timer_set) {
#if	DEBUG_SHAPER
		fprintf(stderr,"removing edit timeout cb from VcrCB\n");
#endif
                XtRemoveTimeOut(shaper->edit_timer_id);
                shaper->edit_timer_set = False;
        }

        if (w == vcr->begin) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_MIN_VAL,synchro_mode_update);
        }
        else if (w == vcr->reverse && shaper->synchro_step_set) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_DECREMENT,True);
        }
        else if (w == vcr->start_stop) {
                NgShapeInfoGridEditFocusCellComplete(si->shapeinfogrid,True);
        }
        else if (w == vcr->forward && shaper->synchro_step_set) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_INCREMENT,True);
        }
        else if (w == vcr->end) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
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
	NgShaperRec 	*shaper = (NgShaperRec *)si;
	Boolean		sensitive;
        
#if	DEBUG_SHAPER
	fprintf(stderr,"EditTimeoutCB(IN)\n");
#endif
	if (! shaper->edit_timer_set)
		return;

        if (shaper->edit_how == NG_DECREMENT) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_DECREMENT,False);
        }
        else {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_INCREMENT,False);
        }
	sensitive = (si->start[shaper->tgl_coord] == 
		     si->finish[shaper->tgl_coord]) ? True : False;
	XtSetSensitive(shaper->synchro_step_tgl,sensitive);

        shaper->edit_timeout_value = 
		MAX(3,shaper->edit_timeout_value /= 1.05);
#if	DEBUG_SHAPER
	fprintf("setting edit timeout cb from EditTimeoutCB\n");
#endif
        shaper->edit_timer_id = XtAppAddTimeOut(shaper->go->go.x->app,
                                                shaper->edit_timeout_value,
                                                EditTimeoutCB,si);
        shaper->edit_timer_set = True;
}

static void VcrArmCB
(
        Widget          w,
        XtPointer       data,
        XtPointer       cb_data
)
{
	NgShaper	*si = (NgShaper *)data;
	NgShaperRec 	*shaper = (NgShaperRec *)si;
        NgVcrControl    vcr = shaper->vcr;
	Boolean		sensitive;

#if	DEBUG_SHAPER
        fprintf(stderr,"in VcrArmCB\n");
#endif

        if (w == vcr->reverse) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_DECREMENT,False);
                shaper->edit_how = NG_DECREMENT;
        }
        else if (w == vcr->forward) {
                NgShapeInfoGridEditFocusCell(si->shapeinfogrid,
                                             NG_INCREMENT,False);
                shaper->edit_how = NG_INCREMENT;
        }

	sensitive = (si->start[shaper->tgl_coord] == 
		     si->finish[shaper->tgl_coord]) ? True : False;
	XtSetSensitive(shaper->synchro_step_tgl,sensitive);
	  
#if	DEBUG_SHAPER
	fprintf(stderr,"setting edit timeout cb from VcrArmCB\n");
#endif
        shaper->edit_timer_id = XtAppAddTimeOut(shaper->go->go.x->app,
                                            250,EditTimeoutCB,si);
        shaper->edit_timer_set = True;
        shaper->edit_timeout_value = 250;
                                            
	return;
}

void NgDeactivateShaper
(
	NgShaper	*si
)
{
	NgShaperRec *shaper = (NgShaperRec *)si;

#if 0
        if (si->datagrid)
                NgDeactivateDataGrid(si->datagrid);
#endif

	if (si->datagrid) {
		NgDestroyDataGrid(si->datagrid);
		si->datagrid = NULL;
	}
	if (shaper->all_selected_tgl) {
		XtDestroyWidget(shaper->all_selected_tgl);
		shaper->all_selected_tgl = NULL;
	}
	XtVaSetValues(shaper->datagrid_tgl,
		      XmNset,False,
		      XmNbottomWidget,si->shapeinfogrid->grid,
		      NULL);
        shaper->tgl_coord = -1;

        NgDeactivateShapeInfoGrid(si->shapeinfogrid);
	shaper->selected_only_set = False; 
	shaper->synchro_step_set = False;

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
	NgShaperRec *shaper = (NgShaperRec *)si;

        NgDeactivateShaper(si);
        NgDestroyDataGrid(si->datagrid);
        NgDestroyShapeInfoGrid(si->shapeinfogrid);
        NgDestroyVcrControl(shaper->vcr);

        if (shaper->selected) {
                NhlFree(shaper->selected);
        }
        if (shaper->tgl_coord_dlist) {
                NclFreeDataList(shaper->tgl_coord_dlist);
        }
        if (shaper->coords_selected_only_set) {
                NhlFree(shaper->coords_selected_only_set);
        }
        if (shaper->coords_synchro_step_set) {
                NhlFree(shaper->coords_synchro_step_set);
        }
        
        NhlFree(shaper);

        return;
}


void NgDoShaper
(
	NgShaper	*si
)
{
	NgShaperRec *shaper = (NgShaperRec *)si;
	int i;
	static NhlBoolean first = True;
        NhlBoolean has_coord_vars = False;

	if (first) {
                first = False;
		XtAppAddActions(shaper->go->go.x->app,myact,NhlNumber(myact));
	}

	if (shaper->new_data) {
		shaper->new_coord = True;
	}
	else if (shaper->restore) {
		shaper->new_coord = True;
	}

/* indexes toggle */
	if (shaper->new_data) {
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

        
/* synchro step toggle button */
        
	if (shaper->new_data) {
		XmToggleButtonGadgetSetState(shaper->synchro_step_tgl,
					     False,False);
		shaper->synchro_step_set = False;
	}
        
/* Reverse toggle button */

	if (shaper->new_data) {
		XmToggleButtonGadgetSetState(shaper->reverse_tgl,
					     False,False);
		shaper->reverse_set = False;
		shaper->new_rev_val = False;
	}

/* highlighted dimension datagrid toggle */

	if (shaper->new_data) {
		XmToggleButtonGadgetSetState(shaper->datagrid_tgl,
					     False,False);
	}
        
	if (si->vinfo->n_dims > shaper->coords_alloced) {
		shaper->coords_selected_only_set = 
		  NhlRealloc(shaper->coords_selected_only_set,
			     sizeof(Boolean)*si->vinfo->n_dims);
		shaper->coords_synchro_step_set = 
		  NhlRealloc(shaper->coords_synchro_step_set,
			     sizeof(Boolean)*si->vinfo->n_dims);
	}

	if (shaper->new_data) {
		for (i=0;i<si->vinfo->n_dims;i++) {
			shaper->coords_selected_only_set[i] = False;
			shaper->coords_synchro_step_set[i] = False;
		}
		shaper->coords_alloced = 
			MAX(shaper->coords_alloced,si->vinfo->n_dims);
	}

	if (shaper->new_data || shaper->new_coord) {

		if (shaper->tgl_coord == -1) {
			shaper->tgl_coord = si->vinfo->n_dims - 1;
		}
		UpdateShape(si,False);
	}
	
	shaper->restore = False;
        if (si->geo_notify && si->pdata)
                (*si->geo_notify)(si->pdata);
	return;
}

static NgShaperRec *NewShaper
(
	NgGO		go,
	Widget		parent
	)
{
	NgShaperRec 	*shaper = NhlMalloc(sizeof(NgShaperRec));
	NgShaper    	*si;
	Arg		args[50];
	int	    	nargs;
	
	if (! (shaper)) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	shaper->go = go;
	shaper->parent = parent;

	si = &shaper->si;

	si->qfile = NrmNULLQUARK;
	si->vinfo = NULL;
	si->start = NULL;
	si->finish = NULL;
	si->stride = NULL;

	si->datagrid = NULL;
	si->sub_width = 0;
	si->geo_notify = NULL;
	si->shape_notify = NULL;

	shaper->all_selected_tgl = NULL;
	shaper->indexes_tgl = NULL;
	
        shaper->tgl_coord = -1;
	shaper->new_data = True;
	shaper->restore = False;
	shaper->coords_alloced = 0;
	shaper->tgl_coord_dlist = NULL;
	shaper->selected_only_set = False;
	shaper->coords_selected_only_set = NULL;
	shaper->indexes_set = False;
	shaper->new_coord = True;
	shaper->reverse_set = False;
	shaper->new_rev_val = False;
	shaper->selected = NULL;
	shaper->edit_timer_set = False;
	shaper->synchro_step_set = False;
	shaper->coords_synchro_step_set = NULL;

	si->frame = XtVaCreateManagedWidget
		("frame",xmFrameWidgetClass,shaper->parent,NULL);
	shaper->form = XtVaCreateManagedWidget
		("form",xmFormWidgetClass,si->frame,NULL);

/* indexes toggle */

	nargs = 0;
	XtSetArg(args[nargs],XmNindicatorOn,True);nargs++;
	XtSetArg(args[nargs],XmNheight,25);nargs++;
	XtSetArg(args[nargs],XmNrecomputeSize,False);nargs++;
	XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_NONE);nargs++;
	XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_NONE);nargs++;
	shaper->indexes_tgl = XmCreateToggleButtonGadget
		(shaper->form,"Index Mode",args,nargs);
	XmToggleButtonGadgetSetState(shaper->indexes_tgl,False,False);
	XtManageChild(shaper->indexes_tgl);
	XtAddCallback(shaper->indexes_tgl,XmNvalueChangedCallback,
		      ShaperCoordsorIndexesCB,si);


/* 'VCR' control for increment/decrement editing */

	shaper->vcr = NgCreateVcrControl
		(shaper->go,"ElementStepper",shaper->form,20,True,
		 True,False,True,True,True,False,True);
	XtVaSetValues(shaper->vcr->form,
		      XmNleftAttachment,XmATTACH_WIDGET,
		      XmNleftWidget,shaper->indexes_tgl,
		      XmNtopAttachment,XmATTACH_NONE,
		      XmNrightAttachment,XmATTACH_NONE,
		      NULL);
        
	XtAddCallback(shaper->vcr->begin,XmNactivateCallback,VcrCB,si);
	XtAddCallback(shaper->vcr->reverse,XmNactivateCallback,VcrCB,si);
	XtAddCallback(shaper->vcr->reverse,XmNarmCallback,VcrArmCB,si);
	XtAddCallback(shaper->vcr->start_stop,XmNactivateCallback,VcrCB,si);
	XtAddCallback(shaper->vcr->forward,XmNarmCallback,VcrArmCB,si);
	XtAddCallback(shaper->vcr->forward,XmNactivateCallback,VcrCB,si);
	XtAddCallback(shaper->vcr->end,XmNactivateCallback,VcrCB,si);

/* synchro step toggle button */
        
	nargs = 0;
	XtSetArg(args[nargs],XmNuserData,(void*)si); nargs++;
	XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_WIDGET);nargs++;
	XtSetArg(args[nargs],XmNleftWidget,shaper->vcr->form);nargs++;
	XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_NONE);nargs++;
	XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_NONE);nargs++;
	XtSetArg(args[nargs],XmNheight,25);nargs++;
	shaper->synchro_step_tgl =XmCreateToggleButtonGadget
		(shaper->form,"Synchro Step",args,nargs);
	XmToggleButtonGadgetSetState(shaper->synchro_step_tgl,False,False);
	XtManageChild(shaper->synchro_step_tgl);
	XtAddCallback(shaper->synchro_step_tgl,XmNvalueChangedCallback,
		      ShaperSynchroStepCB,si);

/* Reverse toggle button */

	nargs = 0;
	XtSetArg(args[nargs],XmNuserData,(void*)si); nargs++;
	XtSetArg(args[nargs],XmNleftAttachment,XmATTACH_WIDGET);nargs++;
	XtSetArg(args[nargs],XmNleftWidget,shaper->synchro_step_tgl);nargs++;
	XtSetArg(args[nargs],XmNtopAttachment,XmATTACH_NONE);nargs++;
	XtSetArg(args[nargs],XmNrightAttachment,XmATTACH_NONE);nargs++;
	XtSetArg(args[nargs],XmNheight,25);nargs++;
	si->reverse = shaper->reverse_tgl = XmCreateToggleButtonGadget
		(shaper->form,"Reverse",args,nargs);
	XmToggleButtonGadgetSetState(shaper->reverse_tgl,False,False);
	XtManageChild(shaper->reverse_tgl);
	XtAddCallback(shaper->reverse_tgl,XmNvalueChangedCallback,
		      ShaperReverseCoordsCB,si);

/* Shape Info Grid */        

	si->shapeinfogrid = NgCreateShapeInfoGrid
		(shaper->go,shaper->form,si->qfile,si->vinfo,False,False);

	XtVaSetValues(si->shapeinfogrid->grid,
		      XmNbottomAttachment,XmATTACH_WIDGET,
		      XmNbottomWidget,shaper->reverse_tgl,
		      XmNrightAttachment,XmATTACH_NONE,
		      XmNbottomOffset,5,
		      XmNtopAttachment,XmATTACH_NONE,
		      NULL);
	si->shapeinfogrid->shape_notify = ShapeNotify;
	si->shapeinfogrid->dim_select_notify = DimSelectNotify;
	si->shapeinfogrid->notify_data = si;
	si->shapeinfogrid->index_mode = shaper->indexes_set;

/* highlighted dimension datagrid toggle */

	shaper->datagrid_tgl = XtVaCreateManagedWidget
		("Dimension Values",
		 xmToggleButtonGadgetClass,shaper->form,
		 XmNbottomAttachment,XmATTACH_WIDGET,
		 XmNbottomWidget,si->shapeinfogrid->grid,
		 XmNrightAttachment,XmATTACH_NONE,
		 XmNtopAttachment,XmATTACH_NONE,
		 NULL);

	XtAddCallback(shaper->datagrid_tgl,
		      XmNvalueChangedCallback,ToggleCoordGridCB,si);

	_NgGOWidgetTranslations(shaper->go,si->frame);

	return shaper;
}

NgShaper *NgCreateShaper
(
	NgGO		go,
	Widget		parent,
	NrmQuark	qfile,
	long		*start,
	long		*finish,
	long		*stride,
	NclApiVarInfoRec  *vinfo
	)
{
	NgShaperRec *shaper = NewShaper(go,parent);
	NgShaper *si;

	if (! shaper)
		return NULL;

	si = &shaper->si;

	si->qfile = qfile;
	si->vinfo = vinfo;
	si->start = start;
	si->finish = finish;
	si->stride = stride;

	return (NgShaper *) shaper;
}

NhlErrorTypes NgUpdateShaper(
	NgShaper	*si,
	NrmQuark	qfile,
	long		*start,
	long		*finish,
	long		*stride,
	NclApiVarInfoRec  *vinfo
)
{
	NgShaperRec *shaper = (NgShaperRec *)si;
	NhlBoolean new = False;
	
	if (qfile != si->qfile)
		new = True;
	if (! si->vinfo || memcmp(vinfo,si->vinfo,sizeof(NclApiVarInfoRec)))
		new = True;
	if (! (si->start && si->finish && si->stride))
		new = True;
	if (memcmp(si->start,start,vinfo->n_dims * sizeof(long)))
		new = True;
	if (memcmp(si->finish,finish,vinfo->n_dims * sizeof(long)))
		new = True;
	if (memcmp(si->stride,stride,vinfo->n_dims * sizeof(long)))
		new = True;
	si->qfile = qfile;
	si->start = start;
	si->finish = finish;
	si->stride = stride;
	si->vinfo = vinfo;

	if (! si)
		return NhlFATAL;

	if (new)
		shaper->new_data = True;
	NgDoShaper(si);

	return NhlNOERROR;
}
		
NgShaper *NgDupShaper
(
	NgGO		go,
	Widget		parent,
	NgShaper	*si,
	NgShaper	*oldsi,
	NrmQuark	qfile,
	long		*start,
	long		*finish,
	long		*stride,
	NclApiVarInfoRec  *vinfo
	)
{
	NgShaperRec	*shaper;
	NgShaperRec	*old_shaper = (NgShaperRec *) oldsi;
	Boolean 	state;
	int		i;

	if (! old_shaper) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "shaper to dup not specified"));
		return NULL;
	}
		
	if (si)
		shaper = (NgShaperRec *) si;
	else
		shaper = NewShaper(go,parent);
	if (! shaper) {
		NHLPERROR((NhlFATAL,ENOMEM,NULL));
		return NULL;
	}
	si = &shaper->si;

	si->qfile = qfile;
	si->vinfo = vinfo;
	si->start = start;
	si->finish = finish;
	si->stride = stride;

	shaper->new_data = True;
	shaper->tgl_coord = old_shaper->tgl_coord;
	shaper->selected_only_set = old_shaper->selected_only_set;
	si->pdata = NULL;

	NgDoShaper(si);

	for (i=0;i<si->vinfo->n_dims;i++) {
		shaper->coords_selected_only_set[i] = 
			old_shaper->coords_selected_only_set[i];
		shaper->coords_synchro_step_set[i] = 
			old_shaper->coords_synchro_step_set[i];
	}
	
	si->shapeinfogrid->edit_row = oldsi->shapeinfogrid->edit_row;
	NgSetShapeInfoGridSetFocusCell(si->shapeinfogrid);
	state = XmToggleButtonGetState(old_shaper->datagrid_tgl);
	XmToggleButtonSetState(shaper->datagrid_tgl,state,True);
	if (state) {
		state = XmToggleButtonGetState(old_shaper->all_selected_tgl);
		XmToggleButtonSetState(shaper->all_selected_tgl,state,True);
	}
	state = XmToggleButtonGetState(old_shaper->indexes_tgl);
	XmToggleButtonSetState(shaper->indexes_tgl,state,True);
	state = XmToggleButtonGetState(old_shaper->synchro_step_tgl);
	XmToggleButtonSetState(shaper->synchro_step_tgl,state,True);

	return si;
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

	UpdateShape(si,False);
	return;
}

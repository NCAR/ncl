/*
 *      $Id: colormap.c,v 1.3 1999-05-22 00:36:16 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  199r			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		colormap.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 28 10:40:48 MDT 1998
 *
 *	Description:	
 */
#include <math.h>

#include <ncarg/hlu/WorkstationI.h>
#include <ncarg/ncl/defs.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/colormapP.h>
#include <Xcb/xcbP.h>

#include <Xm/Xm.h>
#include <Xm/Frame.h>
#include <Xm/Form.h>
#include <Xm/LabelG.h>
#include <Xm/Scale.h>
#include <Xm/PushBG.h>
#include <Xm/SeparatoG.h>
#include <Xm/CascadeBG.h>
#include <Xm/RowColumn.h>
#include <Xm/TextF.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/DrawnB.h>

#define	Oset(field)	NhlOffset(NgColorMapRec,colormap.field)
static NhlResource resources[] = {
	{NgNcmWork,NgCcmWork,NhlTInteger,sizeof(int),
	 Oset(work),NhlTImmediate,_NhlUSET((NhlPointer)NhlNULLOBJID),
	 _NhlRES_CONLY,NULL},      
};
#undef	Oset

static NhlErrorTypes ColorMapInitialize(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
);

static NhlErrorTypes ColorMapDestroy(
	NhlLayer	l
);

static NhlBoolean ColorMapCreateWin(
	NgGO	go
);

static NhlBoolean ColorMapCreateWinHook(
	NgGO	go
);

NgColorMapClassRec NgcolorMapClassRec = {
	{
/* class_name		*/	"colorMapClass",
/* nrm_class		*/	NrmNULLQUARK,
/* layer_size		*/	sizeof(NgColorMapRec),
/* class_inited		*/	False,
/* superclass		*/	(NhlClass)&NggOClassRec,
/* cvt_table		*/	NULL,

/* layer_resources	*/	resources,
/* num_resources	*/	NhlNumber(resources),
/* all_resources	*/	NULL,
/* callbacks		*/	NULL,
/* num_callbacks	*/	0,
/* class_callbacks	*/	NULL,
/* num_class_callbacks	*/	0,

/* class_part_initialize*/	NULL,
/* class_initialize	*/	NULL,
/* layer_initialize	*/	ColorMapInitialize,
/* layer_set_values	*/	NULL,
/* layer_set_values_hook*/	NULL,
/* layer_get_values	*/	NULL,
/* layer_reparent	*/	NULL,
/* layer_destroy	*/	ColorMapDestroy,

	},
	{
/* dialog		*/	NULL,
/* toplevel		*/	NULL,
/* manager		*/	NULL,

/* top_win_chain	*/	False,
/* create_win		*/	ColorMapCreateWin,
/* create_win_hook	*/	ColorMapCreateWinHook,
/* close		*/	_NgGOInheritClose,
	},
	{
/* foo			*/	0,
	},
};

NhlClass NgcolorMapClass = (NhlClass)&NgcolorMapClassRec;

static NhlErrorTypes
ColorMapInitialize
(
	NhlClass	lc,
	NhlLayer	req,
	NhlLayer	new,
	_NhlArgList	args,
	int		nargs
)
{
	char			func[] = "ColorMapInitialize";
	NgColorMap		cm = (NgColorMap)new;
	NgColorMapPart		*np = &((NgColorMap)new)->colormap;
	NgColorMapPart		*rp = &((NgColorMap)req)->colormap;
	int			n,i;
	NhlErrorTypes		ret=NhlNOERROR;
	float			red,green,blue;
	XColor			xcol;

	if(!NhlIsClass(np->work,NhlworkstationClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid work",func));
		return NhlFATAL;
	}

	np->focus = False;
	np->free_colors = False;

	np->wgc = None;
	np->bgc = None;

	np->cdef_allocated = False;

	np->changed = False;

	np->cmap_len = 0;
	np->edit_cmap_len = 0;
	memset(np->allocated,0,sizeof(NhlBoolean)*NhlwkMAX_COLORS);

	xcol.pixel = 0;
	xcol.flags = (DoRed|DoGreen|DoBlue);
	for(i=0;i<NhlwkMAX_COLORS;i++)
		np->cmap[i] = np->edit_cmap[i] = xcol;

	np->sel_indx = -1;

	return NhlNOERROR;
}

static NhlErrorTypes
ColorMapDestroy
(
	NhlLayer	l
)
{
	NgColorMap	cm = (NgColorMap)l;
	NgColorMapPart	*np = &cm->colormap;
	unsigned long	pixels[NhlwkMAX_COLORS];
	int		i,n=0;

	if(np->free_colors){
		XcbFreeColor(cm->go.xcb,np->white);
		XcbFreeColor(cm->go.xcb,np->black);
	}
	if(np->wgc)
		XFreeGC(cm->go.x->dpy,np->wgc);
	if(np->bgc)
		XFreeGC(cm->go.x->dpy,np->bgc);

	for(i=0;i<NhlwkMAX_COLORS;i++)
		if(np->allocated[i])
			pixels[n++] = np->edit_cmap[i].pixel;

	if(n>0)
		XcbFreeColors(cm->go.xcb,pixels,n);

	return NhlNOERROR;
}

static void
ExposeSelCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	NgColorMap	cm = (NgColorMap)udata;
	NgColorMapPart	*cp = &cm->colormap;

	XDrawRectangle(cm->go.x->dpy,XtWindow(w),cp->wgc,0,0,cp->w_width-1,
							cp->w_height-1);

	XDrawRectangle(cm->go.x->dpy,XtWindow(w),cp->bgc,1,1,cp->w_width-3,
							cp->w_height-3);

	return;
}

static void
UnSelectCurrent
(
	NgColorMap	cm
)
{
	NgColorMapPart	*cp = &cm->colormap;

	/*
	 * Remove ExposeCallback(draws selected border.)
	 */
	XtRemoveCallback(cp->indx[cp->sel_indx],XmNexposeCallback,
							ExposeSelCB,cm);

	XClearWindow(cm->go.x->dpy,XtWindow(cp->indx[cp->sel_indx]));
	return;
}

static void
SetCurrent
(
	NgColorMap	cm
)
{
	NgColorMapPart	*cp = &cm->colormap;
	int		indx = cp->sel_indx;
	XColor		xcol;
	float		red,green,blue;
	char		buffer[10];

	red = ((float)cp->edit_cmap[indx].red)/_NgCM_MAX_CVAL;
	green = ((float)cp->edit_cmap[indx].green)/_NgCM_MAX_CVAL;
	blue = ((float)cp->edit_cmap[indx].blue)/_NgCM_MAX_CVAL;
	XmScaleSetValue(cp->rscale,(int)(red*1000 + .5));
	XmScaleSetValue(cp->gscale,(int)(green*1000 + .5));
	XmScaleSetValue(cp->bscale,(int)(blue*1000 + .5));
	XtVaSetValues(cp->elabel,
		XmNbackground,	cp->edit_cmap[indx].pixel,
		NULL);

	red = ((float)cp->cmap[indx].red)/_NgCM_MAX_CVAL;
	green = ((float)cp->cmap[indx].green)/_NgCM_MAX_CVAL;
	blue = ((float)cp->cmap[indx].blue)/_NgCM_MAX_CVAL;
	sprintf(buffer,"%3d",indx);
	XmTextFieldSetString(cp->indxt,buffer);
	sprintf(buffer,"%4.3f",red);
	XmTextFieldSetString(cp->redt,buffer);
	sprintf(buffer,"%4.3f",green);
	XmTextFieldSetString(cp->greent,buffer);
	sprintf(buffer,"%4.3f",blue);
	XmTextFieldSetString(cp->bluet,buffer);

	cp->cdef.flags = (DoRed|DoGreen|DoBlue);
	cp->cdef.red = cp->cmap[indx].red;
	cp->cdef.green = cp->cmap[indx].green;
	cp->cdef.blue = cp->cmap[indx].blue;

	if(cp->cdef_allocated)
		XcbFreeColor(cm->go.xcb,cp->cdef.pixel);
	XcbAllocROColor(cm->go.xcb,&cp->cdef);
	cp->cdef_allocated = True;

	XtVaSetValues(cp->cur_def,
		XmNbackground,	cp->cdef.pixel,
		NULL);

	XDrawRectangle(cm->go.x->dpy,XtWindow(cp->indx[cp->sel_indx]),
				cp->wgc,0,0,cp->w_width-1,cp->w_height-1);

	XDrawRectangle(cm->go.x->dpy,XtWindow(cp->indx[cp->sel_indx]),
				cp->bgc,1,1,cp->w_width-3,cp->w_height-3);

	return;
}

static void
SelectIndx
(
	NgColorMap	cm,
	int		indx
)
{
	NgColorMapPart	*cp = &cm->colormap;

	cp->sel_indx = indx;
	cp->changed = False;
	/*
	 * Add ExposeCallback(draws selected border.)
	 */
	XtAddCallback(cp->indx[indx],XmNexposeCallback,ExposeSelCB,cm);

	SetCurrent(cm);

	return;
}

static void
SetCmapSize
(
	NgColorMap	cm,
	int		size
)
{
	char		func[] = "SetCmapSize";
	NgColorMapPart	*cp = &cm->colormap;
	int		i;

	if(size < 2 || size > NhlwkMAX_COLORS){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Invalid colormap size:%d",
			func,size));
	}
	else{
		cp->edit_cmap_len = size;
	}

	if(cp->edit_cmap_len > cp->num_indx_mapped){
		for(i=cp->num_indx_mapped;i<cp->edit_cmap_len;i++)
			XtSetMappedWhenManaged(cp->indx[i],True);
	}
	else if(cp->num_indx_mapped > cp->edit_cmap_len){
		for(i=cp->edit_cmap_len;i<cp->num_indx_mapped;i++)
			XtSetMappedWhenManaged(cp->indx[i],False);
	}
	cp->num_indx_mapped = cp->edit_cmap_len;

	XmScaleSetValue(cp->csize,cp->edit_cmap_len);

	return;
}

static NhlBoolean
UpdateCmap
(
	NgColorMap	cm
)
{
	NgColorMapPart		*cp = &cm->colormap;
	int			i,rindx;
	int			len;
	float			red,green,blue;
	NhlErrorTypes		ret;
	XColor			xcol;

	ret = NhlVAGetValues(cp->work,
		NhlNwkColorMapLen,	&len,
		NULL);
	if(ret < NhlWARNING)
		return False;
	cp->cmap_len = len;

	for(i=cp->cmap_len;i<NhlwkMAX_COLORS;i++){
		if(cp->allocated[i])
			XcbFreeColor(cm->go.xcb,cp->edit_cmap[i].pixel);
		cp->allocated[i] = False;
		cp->cmap_status[i] = _NgCM_NOCHANGE;
		cp->cmap[i].pixel = cp->edit_cmap[i].pixel = cp->white;
		cp->cmap[i].red = cp->cmap[i].green =
			cp->cmap[i].blue = cp->edit_cmap[i].red =
			cp->edit_cmap[i].green = cp->edit_cmap[i].blue =
			_NgCM_MAX_CVAL;
		XtVaSetValues(cp->indx[i],
			XmNbackground,	cp->white,
			NULL);
	}
	xcol.flags = (DoRed|DoGreen|DoBlue);
	for(i=0;i<cp->cmap_len;i++){
		rindx = NhlGetColor(cp->work,i,&red,&green,&blue);
		xcol.red = cp->cmap[i].red = red * _NgCM_MAX_CVAL;
		xcol.green = cp->cmap[i].green = green * _NgCM_MAX_CVAL;
		xcol.blue = cp->cmap[i].blue = blue * _NgCM_MAX_CVAL;
		if(cp->allocated[i] &&
				((xcol.red != cp->edit_cmap[i].red) ||
				(xcol.green != cp->edit_cmap[i].green) ||
				(xcol.blue != cp->edit_cmap[i].blue))){
			XcbFreeColor(cm->go.xcb,cp->edit_cmap[i].pixel);
			cp->allocated[i] = False;
		}
		if(!cp->allocated[i]){
			XcbAllocROColor(cm->go.xcb,&xcol);
			cp->allocated[i] = True;
			cp->edit_cmap[i] = xcol;
		}
		if(rindx != i)
			cp->cmap_status[i] = _NgCM_EDITED;
		else
			cp->cmap_status[i] = _NgCM_NOCHANGE;
		XtVaSetValues(cp->indx[i],
			XmNbackground,	cp->edit_cmap[i].pixel,
			NULL);
	}

	SetCmapSize(cm,cp->cmap_len);

	if(cp->sel_indx > -1)
		UnSelectCurrent(cm);
	if(cp->sel_indx < cp->edit_cmap_len && cp->sel_indx > -1)
		SelectIndx(cm,cp->sel_indx);
	else
		SelectIndx(cm,0);

	return True;
}

static void
Grab
(
	Widget		w,
	XtPointer	udata,
	XEvent		*event,
	Boolean		*cont
)
{
	NgColorMap	cm = (NgColorMap)udata;

	if(event->type != MapNotify)
		return;
	if (event->xmap.window != XtWindow(cm->go.shell))
		return;
	if(cm->colormap.focus)
		return;

	NgAppGrabFocus(cm->go.appmgr,cm->base.id);
	cm->colormap.focus = True;
	(void)UpdateCmap(cm);

	return;
}

static void
Release
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	NgColorMap	cm = (NgColorMap)udata;

	if(!cm->colormap.focus)
		return;
	NgAppReleaseFocus(cm->go.appmgr,cm->base.id);
	cm->colormap.focus = False;

	return;
}

static void
ResetColCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	NgColorMap	cm = (NgColorMap)udata;
	NgColorMapPart	*cp = &cm->colormap;
	float		f;
	XColor		xcol;
	int		i = cp->sel_indx;


	if(cp->cmap_status[i] || (i >= cp->cmap_len)){

		if(cp->allocated[i]){
			XcbFreeColor(cm->go.xcb,cp->edit_cmap[i].pixel);
		}

		cp->edit_cmap[i].red = cp->cmap[i].red;
		cp->edit_cmap[i].green = cp->cmap[i].green;
		cp->edit_cmap[i].blue = cp->cmap[i].blue;
		XcbAllocROColor(cm->go.xcb,&cp->edit_cmap[i]);
		cp->allocated[i] = True;
		if(NhlGetColor(cp->work,i,&f,&f,&f) != i)
			cp->cmap_status[i] = _NgCM_EDITED;
		else
			cp->cmap_status[i] = _NgCM_NOCHANGE;
		XtVaSetValues(cp->indx[i],
			XmNbackground,	cp->edit_cmap[i].pixel,
			NULL);
		XtVaSetValues(cp->elabel,
			XmNbackground,	cp->edit_cmap[i].pixel,
			NULL);
	}

	cp->changed = False;

	SetCurrent(cm);

	return;
}

static void
SelDButtonCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	XmDrawnButtonCallbackStruct	*dbcb =
					(XmDrawnButtonCallbackStruct *)cdata;
	XButtonEvent	*bpe = (XButtonEvent*)dbcb->event;
	NgColorMap	cm = (NgColorMap)udata;
	NgColorMapPart	*cp = &cm->colormap;
	XtPointer	userData;
	int		indx;

	if(bpe->type != ButtonRelease){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
						"Not a ButtonRelease Event???");
		return;
	}

	/*
	 * XmNuserData holds the index for this cell.
	 */
	XtVaGetValues(w,
		XmNuserData,	&userData,
		NULL);
	indx = (int)userData;

	if(bpe->button == Button1){
		if(indx == cp->sel_indx) return;

		UnSelectCurrent(cm);
		SelectIndx(cm,indx);
	}
	else if(bpe->button == Button2){
		int	min = MIN(indx,cp->sel_indx);
		int	max = MAX(indx,cp->sel_indx);
		int	diff = max - min;
		int	i,index;
		float	mred,mgreen,mblue;
		float	dred,dgreen,dblue;
		XColor	xcol;

		if(diff < 2)
			return;

		/*
		 * get any current edit's in the "sel_indx" into the "edit_cmap"
		 */
		UnSelectCurrent(cm);
		SelectIndx(cm,cp->sel_indx);

		mred = cp->edit_cmap[min].red;
		mgreen = cp->edit_cmap[min].green;
		mblue = cp->edit_cmap[min].blue;
		dred = ((float)(cp->edit_cmap[max].red) - mred) / diff;
		dgreen = ((float)(cp->edit_cmap[max].green) - mgreen) / diff;
		dblue = ((float)(cp->edit_cmap[max].blue) - mblue) / diff;

		for(i=1;i<diff;i++){
			index = min + i;
			xcol.red = mred + dred * i;
			xcol.green = mgreen + dgreen * i;
			xcol.blue = mblue + dblue * i;

			if(cp->allocated[index] &&
				((xcol.red != cp->edit_cmap[index].red) ||
				(xcol.green != cp->edit_cmap[index].green) ||
				(xcol.blue != cp->edit_cmap[index].blue))){
				XcbFreeColor(cm->go.xcb,
						cp->edit_cmap[index].pixel);
				cp->allocated[index] = False;
			}
			if(!cp->allocated[index]){
				XcbAllocROColor(cm->go.xcb,&xcol);
				cp->allocated[index] = True;
				cp->edit_cmap[index] = xcol;
			}
			cp->cmap_status[i] = _NgCM_EDITED;

			XtVaSetValues(cp->indx[index],
				XmNbackground,	cp->edit_cmap[i].pixel,
				NULL);
		}
	}

	return;
}

static void
DragColorCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	char			func[] = "DragColorCB";
	NgColorMap		cm = (NgColorMap)udata;
	NgColorMapPart		*cp = &cm->colormap;
	XmScaleCallbackStruct	*cbs = (XmScaleCallbackStruct *)cdata;
	XtPointer		userData;
	XColor			*xcol;

	XtVaGetValues(w,
		XmNuserData,	&userData,
		NULL);

	xcol = &cp->edit_cmap[cp->sel_indx];
	if(cp->allocated[cp->sel_indx])
		XcbFreeColor(cm->go.xcb,xcol->pixel);

	switch((int)userData){

	case 1:		/*red*/
		xcol->red = (short)((float)cbs->value/1000*_NgCM_MAX_CVAL);
		break;

	case 2:		/*green*/
		xcol->green = (short)((float)cbs->value/1000*_NgCM_MAX_CVAL);
		break;

	case 3:		/*blue*/
		xcol->blue = (short)((float)cbs->value/1000*_NgCM_MAX_CVAL);
		break;

	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"%s:Called from invalid widget",func));
		return;
	}

	XcbAllocROColor(cm->go.xcb,xcol);
	cp->allocated[cp->sel_indx] = True;
	cp->cmap_status[cp->sel_indx] = _NgCM_EDITED;

	XtVaSetValues(cp->indx[cp->sel_indx],
			XmNbackground,	xcol->pixel,
			NULL);
	XtVaSetValues(cp->elabel,
			XmNbackground,	xcol->pixel,
			NULL);

	cp->changed = True;
	return;
}

static void
DragSizeCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	char			func[] = "DragSizeCB";
	NgColorMap		cm = (NgColorMap)udata;
	XmScaleCallbackStruct	*cbs = (XmScaleCallbackStruct *)cdata;

	if(cbs->value == cm->colormap.edit_cmap_len)
		return;

	SetCmapSize(cm,cbs->value);
	if(cm->colormap.sel_indx >= cm->colormap.edit_cmap_len){
		UnSelectCurrent(cm);
		SelectIndx(cm,0);
	}

	return;
}

static void
SetWorkstationCmap
(
	NgColorMap	cm
)
{
	char		func[]="SetWorkstationCmap";
	NgColorMapPart	*cp = &cm->colormap;
	int		i,min,max,limit,num_chg;
	int		chgs[NhlwkMAX_COLORS];
	NhlBoolean	fullcmap = False;
	char		buffer[NCL_MAX_STRING];
	char		wkbuff[NCL_MAX_STRING];
	char		*wkname;
	_NgColorStatus	status;

	wkname = NgNclGetHLURef(cm->go.nclstate,cp->work);
	if(!wkname){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to get symbol name for workstation",func));
		return;
	}
	strcpy(wkbuff,wkname);

	min = MIN(cp->edit_cmap_len,cp->cmap_len);
	max = MAX(cp->edit_cmap_len,cp->cmap_len);
	status = (cp->edit_cmap_len > cp->cmap_len)?_NgCM_EDITED:_NgCM_FREED;
	for(i=min;i<max;i++)
		cp->cmap_status[i] = status;

	for(i=0,num_chg=0;i<max;i++){
		if(cp->cmap_status[i]){
			chgs[num_chg] = i;
			num_chg++;
		}
	}

	if(num_chg)
		NgNclSubmitLine(cm->go.nclstate,"begin\n",True);

	for(i=0;i<num_chg;i++){
		if(cp->cmap_status[chgs[i]] == _NgCM_EDITED)
			sprintf(buffer,"NhlSetColor(%s,%d,%f,%f,%f)\n",
				wkbuff,
				chgs[i],
				(float)(cp->edit_cmap[chgs[i]].red) /
						(float)_NgCM_MAX_CVAL,
				(float)(cp->edit_cmap[chgs[i]].green) /
						(float)_NgCM_MAX_CVAL,
				(float)(cp->edit_cmap[chgs[i]].blue) /
						(float)_NgCM_MAX_CVAL);
		else
			sprintf(buffer,"NhlFreeColor(%s,%d)\n",
				wkbuff,
				chgs[i]);
		NgNclSubmitLine(cm->go.nclstate,buffer,False);
	}

	if(num_chg){
		sprintf(buffer,"clear(%s)\n",wkbuff);
		NgNclSubmitLine(cm->go.nclstate,buffer,False);
		sprintf(buffer,"draw(%s)\n",wkbuff);
		NgNclSubmitLine(cm->go.nclstate,buffer,False);
		NgNclSubmitLine(cm->go.nclstate,"end\n",False);
	}
}

static void
CmapApplyCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	NgColorMap	cm = (NgColorMap)udata;

	UnSelectCurrent(cm);
	SelectIndx(cm,cm->colormap.sel_indx);
	SetWorkstationCmap(cm);

	UpdateCmap(cm);

	return;
}

static void
CmapOkCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	NgColorMap	cm = (NgColorMap)udata;

	UnSelectCurrent(cm);
	cm->colormap.sel_indx = -1;
	NgGOPopdown(cm->base.id);
	SetWorkstationCmap(cm);

	return;
}

static void
ResetColorCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char		func[] = "ResetColorCB";
	NgColorMap	cm = (NgColorMap)udata;

	if(!UpdateCmap(cm))
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:Can't update cmap!",func));
	return;
}


static void
FreePalNameCB
(
	Widget		w,
	XtPointer	data,
	XtPointer	cb_data
)
{
	NhlFree(data);

	return;
}

static void
SetPaletteCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cdata
)
{
	NgColorMap	cm = (NgColorMap)udata;
	NgColorMapPart	*cp = &cm->colormap;
	int		chg_list[NhlwkMAX_COLORS];
	int		chg_len = 0;
	int		i,index;
	XColor		xcol;
	NhlString	name;
	NhlColor	*cmap;
	int		cmap_len;
	NhlErrorTypes	ret;
	NhlGenArray	fg_ga,bg_ga;
	float		*color;

	XtVaGetValues(w,
		XmNuserData,	&name,
		NULL);

	ret = NhlPalGetColormap(NhlClassOfObject(cp->work),name,&cmap,
								&cmap_len);
	if(ret < NhlNOERROR)
		return;

	NhlVAGetValues(cp->work,
		       NhlNwkBackgroundColor,&bg_ga,
		       NhlNwkForegroundColor,&fg_ga,
		       NULL);
	if (! (bg_ga && fg_ga)) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "Unable to get background/foreground colors"));
		return;
	}

	color = (float *) bg_ga->data;
	cmap[0][0] = color[0];
	cmap[0][1] = color[1];
	cmap[0][2] = color[2];
	color = (float *) fg_ga->data;
	cmap[1][0] = color[0];
	cmap[1][1] = color[1];
	cmap[1][2] = color[2];


	for(i=0;i < cmap_len;i++){
		if(cmap[i][0] == -1.0)
			continue;
		chg_list[chg_len++] = i;
	}

	for(i=0;i < chg_len;i++){
		index = chg_list[i];
		xcol.red = cmap[index][0] * _NgCM_MAX_CVAL;
		xcol.green = cmap[index][1] * _NgCM_MAX_CVAL;
		xcol.blue = cmap[index][2] * _NgCM_MAX_CVAL;
		if(cp->allocated[index] &&
			((xcol.red != cp->edit_cmap[index].red) ||
			(xcol.green != cp->edit_cmap[index].green) ||
			(xcol.blue != cp->edit_cmap[index].blue))){
			XcbFreeColor(cm->go.xcb,cp->edit_cmap[index].pixel);
			cp->allocated[index] = False;
		}
		if(!cp->allocated[index]){
			XcbAllocROColor(cm->go.xcb,&xcol);
			cp->allocated[index] = True;
			cp->edit_cmap[index] = xcol;
		}
		cp->cmap_status[index] = _NgCM_EDITED;
		XtVaSetValues(cp->indx[index],
			XmNbackground,	cp->edit_cmap[index].pixel,
			NULL);
	}

	NhlFree(cmap);

	SetCmapSize(cm,cmap_len);

	if(cp->sel_indx > -1)
		UnSelectCurrent(cm);
	if(cp->sel_indx < cp->edit_cmap_len && cp->sel_indx > -1)
		SelectIndx(cm,cp->sel_indx);
	else
		SelectIndx(cm,0);

	return;
}

static NhlBoolean
ColorMapCreateWin
(
	NgGO	go
)
{
	char		func[]="ColorMapCreateWin";
	NgColorMapPart	*np = &((NgColorMap)go)->colormap;

	Widget		ok,apply,cancel,help,bottom,cmapdpy;
	Widget		mbar,pmenu,w;
	Widget		cframe,cform,clabel,csize;
	Widget		curframe,curform;
	Widget		indxl,indxt,redt,redl,greent,greenl,bluet,bluel;
	Widget		eframe,eform,sform,elabel;
	Widget		rsframe,gsframe,bsframe;
	Widget		cdefl,rsform,gsform,bsform,bscale,gscale,rscale;
	Widget		bscalel,gscalel,rscalel;
	int		i;
	NhlString	*pname;
	int		num_pname;
	XColor		wcol,bcol;

	XtAddEventHandler(go->go.shell,StructureNotifyMask,False,Grab,go);
	XtAddCallback(go->go.shell,XmNpopdownCallback,Release,go);
	if(!XcbAllocNamedColor(go->go.xcb,"white",&wcol) ||
				!XcbAllocNamedColor(go->go.xcb,"black",&bcol)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"%s:Unable to allocate white/black",func));
		return False;
	}
	np->free_colors = True;
	np->white = wcol.pixel;
	np->black = bcol.pixel;

	cframe = XtVaCreateManagedWidget("cframe",xmFrameWidgetClass,
							go->go.manager,
		NULL);

	cform = XtVaCreateManagedWidget("cform",xmFormWidgetClass,cframe,
		NULL);

	mbar = XmCreateMenuBar(cform,"mbar",NULL,0);
	XtManageChild(mbar);

	clabel = XtVaCreateManagedWidget("clabel",xmLabelGadgetClass,cform,
		XmNleftWidget,	mbar,
		NULL);

	csize = XtVaCreateManagedWidget("csize",xmScaleWidgetClass,cform,
		XmNleftWidget,	clabel,
		NULL);

	XtAddCallback(csize,XmNvalueChangedCallback,DragSizeCB,go);
	np->csize = csize;

	/*
	 * Create list of palettes in a pulldown menu.
	 */

	pmenu = XmCreatePulldownMenu(mbar,"PMenu",NULL,0);

	w = XtVaCreateWidget("Current Colormap",xmPushButtonGadgetClass,pmenu,
		NULL);
	XtManageChild(w);
	XtAddCallback(w,XmNactivateCallback,ResetColorCB,go);

	XtVaCreateManagedWidget("sep",xmSeparatorGadgetClass,pmenu,
		NULL);

	num_pname = NhlPalGetDefined(NhlClassOfObject(np->work),&pname);

	for(i=0;i<num_pname;i++){
		w = XtVaCreateManagedWidget(pname[i],xmPushButtonGadgetClass,
									pmenu,
			XmNuserData,	pname[i],
			NULL);
		XtAddCallback(w,XmNactivateCallback,SetPaletteCB,go);
		XtAddCallback(w,XmNdestroyCallback,FreePalNameCB,pname[i]);
	}
	/*
	 * Don't free the individual strings since they are userData.
	 */
	NhlFree(pname);

	XtVaCreateManagedWidget("pal",xmCascadeButtonGadgetClass,mbar,
		XmNsubMenuId,	pmenu,
		NULL);

	ok = XtVaCreateManagedWidget("ok",xmPushButtonGadgetClass,
								go->go.manager,
		NULL);
	XtAddCallback(ok,XmNactivateCallback,CmapOkCB,go);

	XtVaSetValues(go->go.manager,
		XmNdefaultButton,	ok,
		NULL);

	apply = XtVaCreateManagedWidget("apply",xmPushButtonGadgetClass,
								go->go.manager,
		NULL);
	XtAddCallback(apply,XmNactivateCallback,CmapApplyCB,go);

	cancel = XtVaCreateManagedWidget("cancel",xmPushButtonGadgetClass,
								go->go.manager,
		NULL);
	XtAddCallback(cancel,XmNactivateCallback,_NgGOPopdownCB,
							(XtPointer)go->base.id);

	help = XtVaCreateManagedWidget("help",xmPushButtonGadgetClass,
								go->go.manager,
		XmNsensitive,	False,
		NULL);

	bottom = XtVaCreateManagedWidget("bottom",xmSeparatorGadgetClass,
								go->go.manager,
		XmNbottomWidget,	ok,
		NULL);

	curframe = XtVaCreateManagedWidget("curframe",xmFrameWidgetClass,
								go->go.manager,
		XmNbottomWidget,	bottom,
		NULL);

	curform = XtVaCreateManagedWidget("curform",xmFormWidgetClass,curframe,
		NULL);

	indxt = XtVaCreateManagedWidget("indxt",xmTextFieldWidgetClass,curform,
		NULL);
	np->indxt = indxt;

	indxl = XtVaCreateManagedWidget("indxl",xmLabelGadgetClass,curform,
		XmNtopWidget,		indxt,
		XmNrightWidget,		indxt,
		NULL);

	redt = XtVaCreateManagedWidget("redt",xmTextFieldWidgetClass,curform,
		NULL);
	np->redt = redt;

	redl = XtVaCreateManagedWidget("redl",xmLabelGadgetClass,curform,
		XmNrightWidget,		redt,
		XmNtopWidget,		redt,
		NULL);

	greent = XtVaCreateManagedWidget("greent",xmTextFieldWidgetClass,
									curform,
		NULL);
	np->greent = greent;

	greenl = XtVaCreateManagedWidget("greenl",xmLabelGadgetClass,curform,
		XmNrightWidget,		greent,
		XmNtopWidget,		greent,
		NULL);

	bluet = XtVaCreateManagedWidget("bluet",xmTextFieldWidgetClass,curform,
		NULL);
	np->bluet = bluet;

	bluel = XtVaCreateManagedWidget("bluel",xmLabelGadgetClass,curform,
		XmNrightWidget,		bluet,
		XmNtopWidget,		bluet,
		NULL);

	np->cur_def = XtVaCreateManagedWidget("cur_def",xmPushButtonWidgetClass,
								curform,
		XmNbottomWidget,	bluet,
		NULL);
	XtAddCallback(np->cur_def,XmNactivateCallback,ResetColCB,go);

	cdefl = XtVaCreateManagedWidget("cdefl",xmLabelGadgetClass,curform,
		XmNbottomWidget,	bluet,
		XmNrightWidget,		np->cur_def,
		XmNtopWidget,		np->cur_def,
		NULL);

	eframe = XtVaCreateManagedWidget("eframe",xmFrameWidgetClass,
								go->go.manager,
		XmNtopWidget,		cframe,
		XmNbottomWidget,	curframe,
		NULL);

	eform = XtVaCreateManagedWidget("eform",xmFormWidgetClass,eframe,
		NULL);

	sform = XtVaCreateManagedWidget("sform",xmFormWidgetClass,eform,
		NULL);

	np->elabel = elabel = XtVaCreateManagedWidget("elabel",
						xmLabelWidgetClass,sform,
		NULL);

	bsframe = XtVaCreateManagedWidget("bsframe",xmFrameWidgetClass,sform,
		XmNtopWidget,	elabel,
		NULL);

	bsform = XtVaCreateManagedWidget("bsform",xmFormWidgetClass,bsframe,
		NULL);

	bscalel = XtVaCreateManagedWidget("bscalel",xmLabelGadgetClass,bsform,
		NULL);

	bscale = XtVaCreateManagedWidget("bscale",xmScaleWidgetClass,bsform,
		XmNtopWidget,		bscalel,
		XmNuserData,		3,
		NULL);
	XtAddCallback(bscale,XmNvalueChangedCallback,DragColorCB,go);
	XtAddCallback(bscale,XmNdragCallback,DragColorCB,go);
	np->bscale = bscale;

	gsframe = XtVaCreateManagedWidget("gsframe",xmFrameWidgetClass,sform,
		XmNrightWidget,	bsframe,
		XmNtopWidget,	elabel,
		NULL);

	gsform = XtVaCreateManagedWidget("gsform",xmFormWidgetClass,gsframe,
		NULL);

	gscalel = XtVaCreateManagedWidget("gscalel",xmLabelGadgetClass,gsform,
		NULL);

	gscale = XtVaCreateManagedWidget("gscale",xmScaleWidgetClass,gsform,
		XmNtopWidget,		gscalel,
		XmNuserData,		2,
		NULL);
	XtAddCallback(gscale,XmNvalueChangedCallback,DragColorCB,go);
	XtAddCallback(gscale,XmNdragCallback,DragColorCB,go);
	np->gscale = gscale;

	rsframe = XtVaCreateManagedWidget("rsframe",xmFrameWidgetClass,sform,
		XmNrightWidget,	gsframe,
		XmNtopWidget,	elabel,
		NULL);

	rsform = XtVaCreateManagedWidget("rsform",xmFormWidgetClass,rsframe,
		NULL);

	rscalel = XtVaCreateManagedWidget("rscalel",xmLabelGadgetClass,rsform,
		NULL);

	rscale = XtVaCreateManagedWidget("rscale",xmScaleWidgetClass,rsform,
		XmNtopWidget,		rscalel,
		XmNuserData,		1,
		NULL);
	XtAddCallback(rscale,XmNvalueChangedCallback,DragColorCB,go);
	XtAddCallback(rscale,XmNdragCallback,DragColorCB,go);
	np->rscale = rscale;

	cmapdpy = XtVaCreateManagedWidget("cmapdpy",xmRowColumnWidgetClass,
									eform,
		XmNrightWidget,		sform,
		XmNorientation,		XmHORIZONTAL,
		XmNpacking,		XmPACK_COLUMN,
		XmNnumColumns,		(int)sqrt((double)NhlwkMAX_COLORS),
		NULL);

	for(i=0;i<NhlwkMAX_COLORS;i++){
		np->indx[i] = XtVaCreateManagedWidget("indx",
					xmDrawnButtonWidgetClass,cmapdpy,
			XmNmappedWhenManaged,	False,
			XmNuserData,		(XtPointer)i,
			XmNborderColor,		np->black,
			NULL);
		XtAddCallback(np->indx[i],XmNactivateCallback,SelDButtonCB,go);
	}
	XtManageChildren(np->indx,NhlwkMAX_COLORS);
	np->num_indx_mapped = 0;

	/*
	 * Setup graphics to draw selected box around the selected "indx".
	 */
	XtVaGetValues(np->indx[0],
		XmNwidth,	&np->w_width,
		XmNheight,	&np->w_height,
		NULL);

	return True;
}

static NhlBoolean
ColorMapCreateWinHook
(
	NgGO	go
)
{
	char		func[]="ColorMapCreateWinHook";
	NgColorMapPart	*np = &((NgColorMap)go)->colormap;
	XGCValues	gcvalues;

	XtRealizeWidget(go->go.shell);

	gcvalues.function = GXcopy;
	gcvalues.line_width = 1;
	gcvalues.foreground = np->white;
	np->wgc = XCreateGC(go->go.x->dpy,XtWindow(go->go.manager),
		(GCLineWidth|GCFunction|GCForeground),&gcvalues);
	gcvalues.foreground = np->black;
	np->bgc = XCreateGC(go->go.x->dpy,XtWindow(go->go.manager),
		(GCLineWidth|GCFunction|GCForeground),&gcvalues);

	return True;
}

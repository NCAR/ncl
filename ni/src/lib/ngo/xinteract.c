/*
 *      $Id: xinteract.c,v 1.2 1998-11-20 04:11:04 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		xinteract.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 28 17:50:54 MDT 1998
 *
 *	Description:	This file contains all the eventhandling functions
 *			for the xdemo application.
 */

#define	FUZZFACTOR	(3)

#include <X11/cursorfont.h>
#include <ncarg/ngo/nclstate.h>
#include <ncarg/ngo/xinteractP.h>
#include <ncarg/gksP.h>
#include <ncarg/ngo/xwkP.h>
#include <ncarg/ngo/mwin.h>

#ifdef  DEBUG
#define XGRABSERVER(dpy)
#define XUNGRABSERVER(dpy)
#else
#define XGRABSERVER(dpy)        XGrabServer(dpy)
#define XUNGRABSERVER(dpy)      XUngrabServer(dpy)
#endif

typedef enum {MODX, MODY, MODPT, MOVE} RubberType;


typedef struct _RubberRec {
        RubberType      type;
        XPoint          offset_p0;
        XPoint          offset_p1;
        NgXBBox         bbox;
	XPoint		xbox[5];
} RubberRec, *RubberPtr;



static void Select(
	NgXWk		xwk,
	XButtonEvent	*event,
	NhlBoolean	do_rubber,
	NhlBoolean	limit_to_vp
);

/*
 * Function:	CalcBBox
 *
 * Description:	This function takes a *NgXBBox and calculates the
 *		box array element of that structure given p0 and p1.
 *
 * In Args:	
 *		NgXBBox	*bbox	the BoundingBox structure to calc
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	void
 * Side Effect:	
 */
void
CalcBBox
(
	NgXBBox	*bbox,
	XPoint  *xbox  /* array of 5 XPoints */
)
{
	xbox[0] = bbox->p0;
	xbox[1].x = bbox->p0.x;
	xbox[1].y = bbox->p1.y;
	xbox[2] = bbox->p1;
	xbox[3].x = bbox->p1.x;
	xbox[3].y = bbox->p0.y;
	xbox[4] = bbox->p0;

	return;
}

/*
 * Function:	RubberRect
 *
 * Description:	This function is used to draw the rectangle when the user
 *		drag's the mouse in the frame.  It is also responsible for
 *		removing the previous rectangle.
 *
 * In Args:	
 *		RubberPtr	rubber,	rubberBand structure
 *		Window		w	window
 *
 * Out Args:	
 *
 * Scope:	Global
 * Returns:	void
 * Side Effect:	
 */
static void
RubberRect
(
	RubberPtr	rubber,	/* rubberBand structure		*/
	Display		*dpy,
	Window		w,	/* window			*/
	GC		XorGC
)
{
	int lastx = -99999;
	int lasty = -99999;
	int newx, newy;
	int dummy_int;
	Window dummy_win;
	XEvent	event;
        short mnx,mny,mxx,mxy;

	/*
	 * Draw original bounding box rubber-> p0 and p1 are defined
	 * properly.
	 */
	CalcBBox(&rubber->bbox,rubber->xbox);
	XDrawLines(dpy,w,XorGC,rubber->xbox,5,CoordModeOrigin);

	while(1) {

		XQueryPointer(dpy,w,&dummy_win,&dummy_win,&dummy_int,
			      &dummy_int,&newx,&newy,
			      (unsigned int *)&dummy_int);

		if((lastx != newx) || (lasty != newy)){

			lastx = newx;
			lasty = newy;
			/*
			 * calculate new values for bounding box
			 */
			switch(rubber->type) {

				case MODX:
					rubber->bbox.p1.x = newx;
					break;

				case MODY:
					rubber->bbox.p1.y = newy;
					break;

				case MODPT:
					rubber->bbox.p1.x = newx;
					rubber->bbox.p1.y = newy;
					break;

				case MOVE:
					rubber->bbox.p0.x =
						newx + rubber->offset_p0.x;
					rubber->bbox.p0.y =
						newy + rubber->offset_p0.y;
					rubber->bbox.p1.x =
						newx + rubber->offset_p1.x;
					rubber->bbox.p1.y =
						newy + rubber->offset_p1.y;
					break;

				default:
					NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Rubber called with invalid type");
			}

			/*
			 * erase old bounding box
			 */
			XDrawLines(dpy,w,XorGC,rubber->xbox,5,
							CoordModeOrigin);

			/*
			 * calculate line segments for new box.
			 */
			CalcBBox(&rubber->bbox,rubber->xbox);

			/*
			 * draw new bounding box
			 */
			XDrawLines(dpy,w,XorGC,rubber->xbox,5,
							CoordModeOrigin);
		}

		if (XCheckMaskEvent(dpy, ButtonReleaseMask, &event)){
			break;
		}
	}


	/*
	 * Erase bounding box
	 */
	XDrawLines(dpy,w,XorGC,rubber->xbox,5,CoordModeOrigin);

	/* 
	 * Normalize the XBBox such that p0.x <= p1.x and p0.y <= p1.y
	 */
	mnx = MIN(rubber->bbox.p0.x,rubber->bbox.p1.x);
	mny = MIN(rubber->bbox.p0.y,rubber->bbox.p1.y);
	mxx = MAX(rubber->bbox.p0.x,rubber->bbox.p1.x);
	mxy = MAX(rubber->bbox.p0.y,rubber->bbox.p1.y);
	rubber->bbox.p0.x = mnx;
	rubber->bbox.p0.y = mny;
	rubber->bbox.p1.x = mxx;
	rubber->bbox.p1.y = mxy;

	return;
}

static void XorDrawViewPort(
	NgXWk		xwk,
	int		view_id,
	NhlBoolean	erase
)
{
	NhlLayer l = _NhlGetLayer(view_id);
	NgViewObj vobj = NULL;
	XPoint xbox[5];

#if DEBUG_XINTERACT
	fprintf(stderr,
		"entering xordraw - erase %d xwk->xwk.select_rect_vis %d\n",
		erase,xwk->xwk.select_rect_vis);
#endif
	if (erase && ! xwk->xwk.select_rect_vis)
		return;
	if (! erase && xwk->xwk.select_rect_vis)
		return;

	if ( l)
		vobj = (NgViewObj) l->base.gui_data2;	

	if (! vobj) {
		NgWksState 	wks_state;
		/*
		 * The object may already have been deleted so try an
		 *  alternate route to get the object info: looking 
		 * through the view tree.
		 */	
		NhlVAGetValues(xwk->go.appmgr,
			       NgNappWksState,&wks_state,
			       NULL);
	
		vobj = NgGetView(wks_state,view_id);
		if (! vobj)
			return;
	}

	CalcBBox(&vobj->xvp,xbox);
	
	XDrawLines(XtDisplay(xwk->xwk.graphics),XtWindow(xwk->xwk.graphics),
		   xwk->xwk.xor_gc,xbox,5,CoordModeOrigin);

	xwk->xwk.select_rect_vis = ! erase;

#if DEBUG_XINTERACT
	fprintf(stderr,
		"leaving xordraw - erase %d xwk->xwk.select_rect_vis %d\n",
		erase,xwk->xwk.select_rect_vis);
#endif

	return;
}

static void SetViewPort
(
	NgXWk		xwk,
	int		view_id,
	NgXBBox		*xvp,
	NhlBoolean	move_only
	)
{
	char		func[]="SetViewport";
	float		x,y,width,height;
	NhlString 	view_name;
	char		buf[1024];
	NhlString	res_names[4] = 
	{NhlNvpXF,NhlNvpYF,NhlNvpWidthF,NhlNvpHeightF};
	float		fv[4];
	NhlString  	svalues[4];
	char		cfv[4][32];
	int		i,block_id,count;

	view_name = NgNclGetHLURef(xwk->go.nclstate,view_id);

	NgXCoordToNDC(xwk->base.id,xvp,&fv[0],&fv[1],&fv[2],&fv[3]);

	count = move_only ? 2 : 4;
	buf[0] = '\0';
	for (i = 0; i < count; i++) {
		svalues[i] = &cfv[i][0];
		sprintf(svalues[i],"%f",fv[i]);
	}
	block_id = NgNclVisBlockBegin(xwk->go.nclstate,_NgSETVAL,view_name,
				      NULL,NULL);
	NgNclVisBlockAddResList(xwk->go.nclstate,block_id,count,
				res_names,svalues,NULL);
	NgNclVisBlockEnd(xwk->go.nclstate,block_id);

	return;
}


static NhlBoolean ClearViewBB
(
	NgXWk		xwk,
	int		view_id,
	NhlBoolean	force_clear
	)
{
	NhlLayer	l = _NhlGetLayer(view_id);
	NgViewObj	vobj = NULL;
	NhlBoolean	do_clear = True;
	
/*
 * If condtional is true, then the BB is only cleared if it is different
 * from the previous BB.
 */
	if (l) 
		vobj = (NgViewObj) l->base.gui_data2;

	if (! vobj) {
		NgWksState 	wks_state = NULL;
		/*
		 * The object may already have been deleted so try an
		 *  alternate route to get the object info: looking 
		 * through the view tree.
		 */	

		NhlVAGetValues(xwk->go.appmgr,
			       NgNappWksState,&wks_state,
			       NULL);
	
		vobj = NgGetView(wks_state,view_id);
		if (! vobj)
			return False;
	}

	if (! force_clear) {
		NhlBoundingBox  thebox;
		NgXBBox		bbox;

		NhlGetBB(view_id,&thebox);
		NgNDCToXCoord(xwk->base.id,&bbox,
			      thebox.l,thebox.t,
			      thebox.r - thebox.l,thebox.t - thebox.b);

		if (! memcmp(&bbox,&vobj->xbbox,sizeof(NgXBBox)))
			do_clear = False;
	}
		
	if (do_clear) {
		Display		*dpy = XtDisplay(xwk->xwk.graphics);
		Dimension	w,h;
		NgXBBox		*cbbox = &vobj->xbbox;

		w = cbbox->p1.x - cbbox->p0.x;
		h = cbbox->p1.y - cbbox->p0.y;
		
		XClearArea(dpy,XtWindow(xwk->xwk.graphics),
			   cbbox->p0.x,cbbox->p0.y,w,h,False);
		XSync(dpy,False);
	}
	return do_clear;
	
}

static int GetIntersectingViews
(
	NgXWk		xwk,
	int		view_id,
	int		**inter_view_ids
	)
{
	NhlLayer	l = _NhlGetLayer(view_id);
	NgViewObj	vobj;
	NgWksState 	wks_state;
	int		count = 0;

	if (! l)
		return 0;
	vobj = (NgViewObj) l->base.gui_data2;
	if (! vobj)
		return count;

	NhlVAGetValues(xwk->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);


	NgGetViewsInRegion(wks_state,xwk->xwk.xwork->base.id,False,
			   &vobj->xbbox,inter_view_ids,&count);

	return count;
}

static void Manipulate
(
	NgXWk		xwk,
	XButtonEvent	*event,
	NhlLayer	l
)
{
	static int		firsttime = 0;
	static Cursor		TLCornerCursor;
	static Cursor		BLCornerCursor;
	static Cursor		TRCornerCursor;
	static Cursor		BRCornerCursor;
	static Cursor		LsideCursor;
	static Cursor		RsideCursor;
	static Cursor		TsideCursor;
	static Cursor		BsideCursor;
	static Cursor		MoveCursor;


	RubberRec		rubber = {MODPT,0,};
	NgViewObj 		vobj;
	Display			*dpy = XtDisplay(xwk->xwk.graphics);
	Window			win = XtWindow(xwk->xwk.graphics);
	NgWksObj		wks;
	NhlBoolean		save_auto_refresh;

	if (! l)
		return;

	if(firsttime == 0){
		firsttime = 1;
		TLCornerCursor = XCreateFontCursor(dpy,XC_top_left_corner);
		BLCornerCursor = XCreateFontCursor(dpy,XC_bottom_left_corner);
		TRCornerCursor = XCreateFontCursor(dpy,XC_top_right_corner);
		BRCornerCursor = XCreateFontCursor(dpy,XC_bottom_right_corner);
		LsideCursor = XCreateFontCursor(dpy, XC_left_side);
		RsideCursor = XCreateFontCursor(dpy, XC_right_side);
		TsideCursor = XCreateFontCursor(dpy, XC_top_side);
		BsideCursor = XCreateFontCursor(dpy, XC_bottom_side);
		MoveCursor = XCreateFontCursor(dpy, XC_fleur);
	}
	vobj = (NgViewObj) l->base.gui_data2;


	XGRABSERVER(dpy);
	if(XGrabPointer(dpy,win,False,
			(Button2MotionMask|ButtonPressMask|ButtonReleaseMask),
			GrabModeAsync,GrabModeAsync,win,None,
			CurrentTime) != GrabSuccess){

			NhlPError(NhlFATAL,NhlEUNKNOWN,
						"Unable to grab pointer");
			XUNGRABSERVER(dpy);
			return;
	}

	/*
	 * Calculate the type of rubberBanding that should be occuring
	 * rubber.bbox.p0 is the const point - p1 is the one
	 * that moves with the rubberbanding action.
	 * p0 of object is TL corner p1 of object is BR corner.
	 */

	if(event->x < (vobj->xvp.p0.x + FUZZFACTOR)){
		/*
		 * Pointer is on Left Side
		 */
		if(event->y < (vobj->xvp.p0.y + FUZZFACTOR)){
			/*
			 * Pointer is near LeftTop Corner
			 * make BR corner const(p0).
			 */
			XDefineCursor(dpy,win,TLCornerCursor);
			rubber.bbox.p0.x = vobj->xvp.p1.x;
			rubber.bbox.p0.y = vobj->xvp.p1.y;
			rubber.bbox.p1.x = vobj->xvp.p0.x;
			rubber.bbox.p1.y = vobj->xvp.p0.y;
			rubber.type = MODPT;
		}
		else if(event->y > (vobj->xvp.p1.y - FUZZFACTOR)){
			/*
			 * Pointer is near LeftBottom Corner
			 * make TR corner const(p0).
			 */
			XDefineCursor(dpy,win,BLCornerCursor);
			rubber.bbox.p0.x = vobj->xvp.p1.x;
			rubber.bbox.p0.y = vobj->xvp.p0.y;
			rubber.bbox.p1.x = vobj->xvp.p0.x;
			rubber.bbox.p1.y = vobj->xvp.p1.y;
			rubber.type = MODPT;
		}
		else{
			/*
			 * Pointer is on left side only
			 * make BR corner const(p0).
			 */
			XDefineCursor(dpy,win,LsideCursor);
			rubber.bbox.p0.x = vobj->xvp.p1.x;
			rubber.bbox.p0.y = vobj->xvp.p1.y;
			rubber.bbox.p1.x = vobj->xvp.p0.x;
			rubber.bbox.p1.y = vobj->xvp.p0.y;
			rubber.type = MODX;
		}
	}
	else if(event->x > (vobj->xvp.p1.x - FUZZFACTOR)){
		/*
		 * Pointer is on Right Side
		 */
		if(event->y < (vobj->xvp.p0.y + FUZZFACTOR)){
			/*
			 * Pointer is near RightTop Corner
			 * make BL corner const(p0).
			 */
			XDefineCursor(dpy,win,TRCornerCursor);
			rubber.bbox.p0.x = vobj->xvp.p0.x;
			rubber.bbox.p0.y = vobj->xvp.p1.y;
			rubber.bbox.p1.x = vobj->xvp.p1.x;
			rubber.bbox.p1.y = vobj->xvp.p0.y;
			rubber.type = MODPT;
		}
		else if(event->y > (vobj->xvp.p1.y - FUZZFACTOR)){
			/*
			 * Pointer is near RightBottom Corner
			 * make TL corner const(p0).
			 */
			XDefineCursor(dpy,win,BRCornerCursor);
			rubber.bbox.p0.x = vobj->xvp.p0.x;
			rubber.bbox.p0.y = vobj->xvp.p0.y;
			rubber.bbox.p1.x = vobj->xvp.p1.x;
			rubber.bbox.p1.y = vobj->xvp.p1.y;
			rubber.type = MODPT;
		}
		else{
			/*
			 * Pointer is on right side only
			 * make TL corner const(p0).
			 */
			XDefineCursor(dpy,win,RsideCursor);
			rubber.bbox.p0.x = vobj->xvp.p0.x;
			rubber.bbox.p0.y = vobj->xvp.p0.y;
			rubber.bbox.p1.x = vobj->xvp.p1.x;
			rubber.bbox.p1.y = vobj->xvp.p1.y;
			rubber.type = MODX;
		}
	}
	else{
		/*
		 * Pointer is in the middle (x) section.
		 */
		if(event->y < (vobj->xvp.p0.y + FUZZFACTOR)){
			/*
			 * Pointer is on the TOP
			 * make BL corner const(p0).
			 */
			XDefineCursor(dpy,win,TsideCursor);
			rubber.bbox.p0.x = vobj->xvp.p0.x;
			rubber.bbox.p0.y = vobj->xvp.p1.y;
			rubber.bbox.p1.x = vobj->xvp.p1.x;
			rubber.bbox.p1.y = vobj->xvp.p0.y;
			rubber.type = MODY;
		}
		else if(event->y > (vobj->xvp.p1.y - FUZZFACTOR)){
			/*
			 * Pointer is near BOTTOM
			 * make TL corner const(p0).
			 */
			XDefineCursor(dpy,win,BsideCursor);
			rubber.bbox.p0.x = vobj->xvp.p0.x;
			rubber.bbox.p0.y = vobj->xvp.p0.y;
			rubber.bbox.p1.x = vobj->xvp.p1.x;
			rubber.bbox.p1.y = vobj->xvp.p1.y;
			rubber.type = MODY;
		}
		else{
			/*
			 * Pointer is in the exact middle of the
			 * object - need to move object - not resize.
			 * set the bbox the same as object and
			 * set the offsets.
			 */
			XDefineCursor(dpy,win,MoveCursor);
			rubber.bbox.p0.x = vobj->xvp.p0.x;
			rubber.bbox.p0.y = vobj->xvp.p0.y;
			rubber.bbox.p1.x = vobj->xvp.p1.x;
			rubber.bbox.p1.y = vobj->xvp.p1.y;
			rubber.offset_p0.x =
					rubber.bbox.p0.x - event->x;
			rubber.offset_p0.y =
					rubber.bbox.p0.y - event->y;
			rubber.offset_p1.x =
					rubber.bbox.p1.x - event->x;
			rubber.offset_p1.y =
					rubber.bbox.p1.y - event->y;
			rubber.type = MOVE;
		}
	}

	/* Erase the current bounding box*/
	XorDrawViewPort(xwk,l->base.id,True);

	RubberRect(&rubber,dpy,win,xwk->xwk.xor_gc);

	XUNGRABSERVER(dpy);
	XUndefineCursor(dpy,win);
	XUngrabPointer(dpy, CurrentTime);


	/*
	 * region has been defined - Set the viewport.
	 */

	ClearViewBB(xwk,l->base.id,True);

	/*
	 * turn off auto_refresh temporarily because we don't want the 
	 * View tree set values callback to do anything 
	 * (because we're already doing it)
	 */

	wks = (NgWksObj) xwk->xwk.xwork->base.gui_data2;
	if (wks) {
		save_auto_refresh = wks->auto_refresh;
		wks->auto_refresh = False;
	}


	SetViewPort(xwk,l->base.id,&rubber.bbox,rubber.type == MOVE);
	/*
	 * Draw the view (updates the view tree state with new location data)
	 */

	NgDrawView(xwk->base.id,l->base.id,True);


	/* Draw the new bounding box for the object */
	XorDrawViewPort(xwk,l->base.id,False);

	if (wks) 
		wks->auto_refresh = save_auto_refresh;

	
	return;
}

/*
 * Function:	ManipulateEH
 *
 * Description:	This event handler is installed to change the cursor in the
 *		window to a manipulation cursor when the cursor is near the
 *		bounding box of the selected object.
 *
 * In Args:	
 *		Widget		w,	widget
 *		XtPointer	data,	data installed with EH
 *		XEvent		*ev,	event
 *		Boolean		*cont	cont to dispatch
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
void
ManipulateEH
(
	Widget		w,	/* widget			*/
	XtPointer	data,	/* data installed with EH	*/
	XEvent		*ev,	/* event			*/
	Boolean		*cont	/* cont to dispatch		*/
)
{
	XButtonEvent		*event = (XButtonEvent*)ev;
	NgXWk			xwk = (NgXWk) data;
	NgViewObj 		vobj;
	NhlLayer 		l;

#if DEBUG_XINTERACT
fprintf(stderr,"In ManipulateEH()\n");
#endif

	if(event->type != ButtonPress){
		NhlPError(NhlWARNING,NhlEUNKNOWN,
				"ModifyEH called for non-ButtonPress event");
		return;
	}
		
	/*
	 * Only interested in Button2
	 */
	if(event->button != Button2)
		return;

	/*
	 * If button press isn't inside selected object
	 * see if it's inside another object; if so change selection
	 */
	l = _NhlGetLayer(xwk->xwk.selected_view_id);
	if (! l) {
		/* 
		 * go ahead and see if another view can be selected
		 *
		 */
		Select(xwk,event,False,True);
		l = _NhlGetLayer(xwk->xwk.selected_view_id);
	}
	else {
		vobj = (NgViewObj) l->base.gui_data2;
		if(	(event->x < (vobj->xvp.p0.x - FUZZFACTOR))	||
			(event->x > (vobj->xvp.p1.x + FUZZFACTOR))	||
			(event->y < (vobj->xvp.p0.y - FUZZFACTOR))	||
			(event->y > (vobj->xvp.p1.y + FUZZFACTOR))	) {
			/* 
			 * go ahead and see if another view can be selected
			 *
			 */
			Select(xwk,event,False,True);
			l = _NhlGetLayer(xwk->xwk.selected_view_id);
		}
	}
	Manipulate(xwk,event,l);

	return;
}


static void Select
(
	NgXWk		xwk,
	XButtonEvent	*event,
	NhlBoolean	do_rubber,
	NhlBoolean	limit_to_vp
)
{
	static RubberRec	rubber = {MODPT,0,};
	Display			*dpy = XtDisplay(xwk->xwk.graphics);
	NgWksState 		wks_state;
	int			view_count;

	rubber.bbox.p0.x = rubber.bbox.p1.x = event->x;
	rubber.bbox.p0.y = rubber.bbox.p1.y = event->y;

	if (do_rubber) {
		XGRABSERVER(dpy);
		if(XGrabPointer
		   (dpy,XtWindow(xwk->xwk.graphics),False,
		    (ButtonReleaseMask|Button1MotionMask),GrabModeAsync,
		    GrabModeAsync,None,None,CurrentTime) != GrabSuccess){

			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "Unable to grab pointer");
			XUNGRABSERVER(dpy);
			return;
		}
		RubberRect(&rubber,dpy,
			   XtWindow(xwk->xwk.graphics),xwk->xwk.xor_gc);

		XUngrabPointer(dpy, CurrentTime);
		XUNGRABSERVER(dpy);
	}
	/*
	 * region has been defined - see if any objects are in it.
	 */

	NhlVAGetValues(xwk->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);
	view_count = xwk->xwk.view_alloc_count;
	NgGetViewsInRegion(wks_state,xwk->xwk.xwork->base.id,limit_to_vp,
			   &rubber.bbox,&xwk->xwk.views,&view_count);
	xwk->xwk.view_alloc_count = MAX(view_count,xwk->xwk.view_alloc_count);
#if DEBUG_XINTERACT
	fprintf(stderr,"view count %d \n",view_count);
#endif

	XUngrabPointer(dpy, CurrentTime);
	XUNGRABSERVER(dpy);

	if (xwk->xwk.selected_view_id) {
#if DEBUG_XINTERACT
		fprintf(stderr,"erasing %d\n",xwk->xwk.selected_view_id);
#endif
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,True);
		XtRemoveEventHandler(xwk->xwk.graphics,ButtonPressMask,False,
                                ManipulateEH,(XtPointer)xwk);
		xwk->xwk.manipulate_eh_active = False;
		xwk->xwk.selected_view_id = NhlNULLOBJID;
		xwk->xwk.selected_view_ix = -1;
		xwk->xwk.lastp.x = xwk->xwk.lastp.y = (Position) -1;
		xwk->xwk.view_count = 0;
	}
	if (view_count > 0) {
		xwk->xwk.selected_view_id = xwk->xwk.views[0];
		xwk->xwk.selected_view_ix = 0;
#if DEBUG_XINTERACT
		fprintf(stderr,"drawing selected %d\n",xwk->xwk.selected_view_id);
#endif
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
		XtAddEventHandler(xwk->xwk.graphics,ButtonPressMask,False,
				  (XtEventHandler)ManipulateEH,
				  (XtPointer)xwk);
		xwk->xwk.manipulate_eh_active = True;
		xwk->xwk.view_count = view_count;
		xwk->xwk.lastp.x = event->x;
		xwk->xwk.lastp.y = event->y;
	}
	return;
}

/*
 * Function:	_NgSelectionEH
 *
 * Description:	This function is used to make a selection on the frame.
 *		It works in conjuction with the rectEH to select an
 *		area of the frame.
 *
 * In Args:	
 *		Widget		w,	widget
 *		XtPointer	data,	data installed with EH
 *		XEvent		*ev	event
 *
 * Out Args:	
 *
 * Scope:	global private
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
void
_NgSelectionEH
(
	Widget		w,	/* widget			*/
	XtPointer	data,	/* data installed with EH	*/
	XEvent		*ev,	/* event			*/
	Boolean		*cont	/* cont to dispatch		*/
)
{
	XButtonEvent		*event = (XButtonEvent*)ev;
	NgXWk			xwk = (NgXWk) data;

	if(event->type != ButtonPress)
		return;

	if(event->button == Button2 && !xwk->xwk.manipulate_eh_active){
		Select(xwk,event,False,True);
		Manipulate(xwk,event,_NhlGetLayer(xwk->xwk.selected_view_id));
	}

	/*
	 * This event handler is only interested in button 1
	 */
	if(event->button != Button1){
		return;
	}

	/* 
	 * Cycle through views if in the same place and more than one
	 * view 
	 */
	if (xwk->xwk.view_count > 1 &&   
	    event->x <= xwk->xwk.lastp.x + FUZZFACTOR &&
	    event->x >= xwk->xwk.lastp.x - FUZZFACTOR &&
	    event->y <= xwk->xwk.lastp.y + FUZZFACTOR &&
	    event->y >= xwk->xwk.lastp.y - FUZZFACTOR) {
#if DEBUG_XINTERACT
		fprintf(stderr,"erasing %d\n",xwk->xwk.selected_view_id);
#endif
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,True);
#if DEBUG_XINTERACT
		fprintf(stderr,"last sel ix %d ",xwk->xwk.selected_view_ix);
#endif
		xwk->xwk.selected_view_ix = 
			(xwk->xwk.selected_view_ix + 1) % xwk->xwk.view_count;
		xwk->xwk.selected_view_id = 
			xwk->xwk.views[xwk->xwk.selected_view_ix];
#if DEBUG_XINTERACT
		fprintf(stderr,"new ix %d drawing selected %d\n",
		       xwk->xwk.selected_view_ix,
		       xwk->xwk.selected_view_id);
#endif
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
		xwk->xwk.lastp.x = event->x;
		xwk->xwk.lastp.y = event->y;
		return;
	}
		
	Select(xwk,event,False,False);

	return;
}


static void
WorkstationAction(
	NgXWk		xwk,
	NhlString	action,
	NhlString	func
)
{

	int	nclstate=NhlDEFAULT_APP;
	char	*ref;
	char	cmdbuff[1024];

	NhlVAGetValues(xwk->go.appmgr,
		       NgNappNclState,	&nclstate,
		       NULL);
	if(!NhlIsClass(nclstate,NgnclStateClass)){
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			   "%s:Invalid nclstate object %d",func,nclstate));
		return;
	}

	ref = NgNclGetHLURef(nclstate,xwk->xwk.xwork->base.id);
	if(ref){
		sprintf(cmdbuff,"%s(%s)\n",action,ref);
		(void)NgNclSubmitBlock(nclstate,cmdbuff);
	}

	return;
}	

void
_NgClearAllViewsCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char			func[]="ClearAllViewsCB";
	XmAnyCallbackStruct	*xmcb = 
		(XmAnyCallbackStruct*)cbdata;
	NgXWk			xwk = (NgXWk)udata;

#if DEBUG_XINTERACT
	fprintf(stderr,"clear all views cb\n");
#endif

	WorkstationAction(xwk,"clear",func);
	xwk->xwk.select_rect_vis = False;

	return;
}

void
_NgDrawAllViewsCB
(
	Widget		w,
	XtPointer	udata,
	XtPointer	cbdata
)
{
	char			func[]="DrawAllViewsCB";
	NgXWk			xwk = (NgXWk)udata;

#if DEBUG_XINTERACT
	fprintf(stderr,"draw all views cb\n");
#endif

	WorkstationAction(xwk,"clear",func);
	xwk->xwk.select_rect_vis = False;
	WorkstationAction(xwk,"draw",func);

#if 0
	if (xwk->xwk.selected_view_id) {
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
	}
#endif
	return;
}

/*
 * Function:	NgNDCToXCoord
 *
 * Description:	This function sets the bbox->p0,p1 parts of an object record
 *		to the correct X coord's given the x,y,width,height in NDC
 *		in the object.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global
 * Returns:	void
 * Side Effect:	
 */
void
NgNDCToXCoord
(
	int		xwkid,
	NgXBBox		*xbbox,
	float		x,	
	float		y,	
	float		width,	
	float		height
)
{
	char func[] = "NgNDCToXCoord";
	int dev_width, dev_height;
	NgXWk xwks = (NgXWk)_NhlGetLayer(xwkid);

	if (! xwks) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid xwkid",func));
		return;
	}

	dev_width = xwks->xwk.grw;
	dev_height = xwks->xwk.grh;

	/*
	 * I -1 to the UL corner to fix some rounding errors, and
	 * +1 to LR corner.
	 */
	xbbox->p0.x = (short)(x * (float)dev_width);
	xbbox->p1.x = (short)((x + width) * (float)dev_width);
	xbbox->p0.y = (short)((1.0 - y) * (float)dev_height);
	xbbox->p1.y = (short)((1.0 - (y - height)) * (float)dev_height);

	xbbox->p0.x-= 3;
	xbbox->p0.y-= 3;
	xbbox->p1.x++;
	xbbox->p1.y++;

	return;
}

/*
 * Function:	NgXCoordToNDC(
 *
 * Description:	This function uses the bbox.p0 and bbox.p1 part of the given
 *		object to determine the correct x,y,width, and height parts.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global
 * Returns:	void
 * Side Effect:	
 */
extern void NgXCoordToNDC
(
	int		xwkid,
	NgXBBox		*xbbox,
	float		*x,	
	float		*y,	
	float		*width,	
	float		*height
)
{
	char func[] = "NgNDCToXCoord";
	int dev_width, dev_height;
	NgXWk xwks = (NgXWk)_NhlGetLayer(xwkid);
	short	minX;
	short	maxX;
	short	minY;
	short	maxY;

	if (! xwks) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s:invalid xwkid",func));
		return;
	}

	dev_width = xwks->xwk.grw;
	dev_height = xwks->xwk.grh;

	minX = MIN(xbbox->p0.x,xbbox->p1.x);
	maxX = MAX(xbbox->p0.x,xbbox->p1.x);
	minY = MIN(xbbox->p0.y,xbbox->p1.y);
	maxY = MAX(xbbox->p0.y,xbbox->p1.y);

	/*
	 * Update the xbbox to p0<p1 p0 is upper left corner.
	 */
	xbbox->p0.x = minX;
	xbbox->p0.y = minY;
	xbbox->p1.x = maxX;
	xbbox->p1.y = maxY;

	/*
	 * "3" should eventually be 1 + tmBorderThicknessF - for all objects
	 * that use the TickMark.
	 */
	minX+= 3;
	minY+= 3;
	maxX--;
	maxY--;

	/*
	 * Update the x,y,width,height fields
	 */
	*x = (float)minX / (float)dev_width;
	*y = ((float)1.0 - ((float)minY / (float)dev_height));
	*width = (((float)maxX / (float)dev_width) - *x);
	*height = (*y - ((float)1.0 - ((float)maxY / (float)dev_height)));

	return;
}

/*
 * this routine expects a top level view
 */
extern void NgDrawXwkView
(
	int		xwkid,
	int		view_id,
	NhlBoolean	force_clear
)
{

	NgXWk xwk = (NgXWk)_NhlGetLayer(xwkid);
        char buf[512];
	NhlString wk_name,view_name;
	NgWksState wks_state;
	int top_level_count = 0;
	int *top_level_views;
	int *draw_table;
	int vcount = 0, *iviews = NULL;
	int i,j,k;
	NhlBoolean cleared,entered = False;

	if (!xwk)
		return;

	if (!xwk->go.up) 
		NgXWorkPopup(xwk->go.appmgr,xwk->base.id);

	NhlVAGetValues(xwk->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);

	if (xwk->xwk.draw_single_view) {
		NhlClearWorkstation(xwk->xwk.xwork->base.id);
		xwk->xwk.select_rect_vis = False;
		NhlDraw(view_id);
		NgUpdateViewBB(wks_state,view_id); 
#if 0
		if (xwk->xwk.selected_view_id = view_id) {
			XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
		}
#endif
		return;
	}

	if (view_id == xwk->xwk.selected_view_id) {
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,True);
	}
	cleared = ClearViewBB(xwk,view_id,force_clear);

	/* 
	 * Build a draw table of all views that need to be redrawn. These
	 * include any views (underneath the view_id view) whose BB 
	 * intersects the area occupied by the the old location of view_id 
	 * and views on top of these views (recursively) (if cleared is true);
	 * and views on top of view_id (recursively). 
	 * First find the old location intersections. As we draw, find
	 * intersections with each view drawn, possibly adding new views
	 * to the draw table.
	 */

	top_level_count = NgTopLevelViews
		(wks_state,xwk->xwk.xwork->base.id,&top_level_views);
	if (top_level_count) {
		draw_table = NhlMalloc(top_level_count * sizeof(int));
		if (! draw_table) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		memset(draw_table,(char) 0,top_level_count * sizeof(int));
	}
	if (! cleared && top_level_count) {
		/* only put view_id into the draw_table */
		for (i = 0; i < top_level_count; i++) {
			if (top_level_views[i] == view_id) {
				draw_table[i] = view_id;
				entered = True;
			}
		}
	}
	if (! entered) {
		if (top_level_count)
			vcount = GetIntersectingViews(xwk,view_id,&iviews);
		if (! vcount) {
			/* 
			 * if vcount == 0, then the view must not have been 
			 * entered into the view tree yet.
			 */
#if 0
			view_name = NgNclGetHLURef(xwk->go.nclstate,view_id);
			sprintf(buf,"draw(%s)\n",view_name);
			(void)NgNclSubmitBlock(xwk->go.nclstate,buf);
#endif
			if (_NhlGetLayer(view_id))
				NhlDraw(view_id);
			if (xwk->xwk.selected_view_id) {
				XorDrawViewPort
					(xwk,xwk->xwk.selected_view_id,False);
			}
			return;
		}
		for (i = 0,j = 0; i < vcount; i++) {
			/* 
			 * assumes iviews a subset of top_level_views with the
			 * same ordering
			 */
			while (top_level_views[j] != iviews[i])
				j++;
			draw_table[j] = iviews[i];
		}
		NhlFree(iviews);
		iviews = NULL;
	}
/*
 * This is the main draw loop
 */
	for (i = 0; i < top_level_count; i++) {
		if (! _NhlGetLayer(draw_table[i]))
			continue;
#if 0
		view_name = NgNclGetHLURef
			(xwk->go.nclstate,draw_table[i]);
		if (! view_name)
			continue;
		sprintf(buf,"draw(%s)\n",view_name);
		(void)NgNclSubmitBlock(xwk->go.nclstate,buf);
#endif
		NhlDraw(draw_table[i]);
		if (draw_table[i] == view_id && cleared) {
			NgUpdateViewBB(wks_state,view_id);
		}
		vcount = GetIntersectingViews(xwk,draw_table[i],&iviews);
		for (j = 0,k = 0; j < vcount; j++) {
			/* 
			 * assumes iviews a subset of top_level_views with the
			 * same ordering
			 */
			while (top_level_views[k] != iviews[j])
				k++;
			draw_table[k] = iviews[j];
		}
		NhlFree(iviews);
		iviews = NULL;
	}

	if (xwk->xwk.selected_view_id) {
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
	}
	return;
}


/*
 * this routine is like the draw routine except that it does not
 * draw the view. Used to erase views that are being deleted or made into
 * overlays or annotations. 
 */

extern void NgClearXwkView
(
	int		xwkid,
	int		view_id
)
{

	NgXWk xwk = (NgXWk)_NhlGetLayer(xwkid);
        char buf[512];
	NhlString wk_name,view_name;
	NgWksState wks_state;
	NgViewObj vobj = NULL;
	int top_level_count = 0;
	int *top_level_views;
	int *draw_table;
	int vcount = 0, *iviews = NULL;
	int i,j,k;

	if (!xwk)
		return;

	NhlVAGetValues(xwk->go.appmgr,
		       NgNappWksState,&wks_state,
		       NULL);


	if (xwk->xwk.selected_view_id)	
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,True);
	ClearViewBB(xwk,view_id,True);

	/* 
	 * Build a draw table of all views that need to be redrawn. These
	 * include any views whose BB intersects the area occupied by the
	 * the old location of view_id; views on top of these views 
	 * (recursively); and views on top of view_id (recursively). 
	 * First find the old location intersections. As we draw, find
	 * intersections with each view drawn, possibly adding new views
	 * to the draw table.
	 */

	vobj = NgGetView(wks_state,view_id);
	if (! vobj) /* there is no information about the object */
		return;

	top_level_count = NgTopLevelViews
		(wks_state,xwk->xwk.xwork->base.id,&top_level_views);
	if (top_level_count) {
		draw_table = NhlMalloc(top_level_count * sizeof(int));
		if (! draw_table) {
			NHLPERROR((NhlFATAL,ENOMEM,NULL));
			return;
		}
		memset(draw_table,(char) 0,top_level_count * sizeof(int));
	
		NgGetViewsInRegion(wks_state,xwk->xwk.xwork->base.id,False,
				   &vobj->xbbox,&iviews,&vcount);
	}
	if (! vcount) {
		if (xwk->xwk.selected_view_id != view_id) {
			XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
		}
		return;
	}
	for (i = 0,j = 0; i < vcount; i++) {
		/* 
		 * assumes iviews a subset of top_level_views with the
		 * same ordering
		 */
		while (top_level_views[j] != iviews[i])
			j++;
		draw_table[j] = iviews[i];
	}
	NhlFree(iviews);
	iviews = NULL;
	for (i = 0; i < top_level_count; i++) {
		if (!_NhlGetLayer(draw_table[i]) || draw_table[i] == view_id)
			continue;
#if 0
		view_name = NgNclGetHLURef
			(xwk->go.nclstate,draw_table[i]);
		if (! view_name)
			continue;
		sprintf(buf,"draw(%s)\n",view_name);
		(void)NgNclSubmitBlock(xwk->go.nclstate,buf);
#endif
		NhlDraw(draw_table[i]);
		vcount = GetIntersectingViews(xwk,draw_table[i],&iviews);
		for (j = 0,k = 0; j < vcount; j++) {
			/* 
			 * assumes iviews a subset of top_level_views with the
			 * same ordering
			 */
			while (top_level_views[k] != iviews[j])
				k++;
			draw_table[k] = iviews[j];
		}
		NhlFree(iviews);
		iviews = NULL;
	}
	if (xwk->xwk.selected_view_id != view_id) {
		XorDrawViewPort(xwk,xwk->xwk.selected_view_id,False);
	}
	return;
}

extern void NgSetSelectedXwkView
(
	int		xwkid,
	int		view_id
)
{
	NgXWk xwk = (NgXWk)_NhlGetLayer(xwkid);
	NgWksState wks_state;
	NhlLayer l = _NhlGetLayer(view_id);
	NgViewObj  vobj;
	Position   x,y;
	XButtonEvent  ev;

	if (!(xwk && l))
		return;


	vobj = (NgViewObj)l->base.gui_data2;

	if (!vobj)
		return;

	x = 0.5 * (vobj->xvp.p0.x + vobj->xvp.p1.x);
	y = 0.5 * (vobj->xvp.p0.y + vobj->xvp.p1.y);
	ev.x = x;
	ev.y = y;
	Select(xwk,&ev,False,False);
}	


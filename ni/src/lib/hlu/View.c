
/*
 *      $Id: View.c,v 1.1 1993-04-30 17:25:45 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		View.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Sep 1 10:06:57 MDT 1992
 *
 *	Description:	View.c is the second class. All classes that draw
 *			on to the screen (are viewed hence the name) are
 *			subclassed from this class. There are two main purposes
 *			addressed by this class. The first and most important
 *			is geometry management. The view resources define an
 *			area of the screen where, in the case of plots that
 *			draw data plots, on the screen the plot will be drawn.
 *			There are some exceptions, for example the titles class
 *			does not draw within the area define by its core but
 *			places its titles outside of the area defined by the
 *			view resources.
 *			
 */

#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ViewP.h>
#include <ncarg/hlu/hluutil.h>

/* SUPPRESS 112 */

static NhlResource resources[] = {
	{ NhlNvpXF, NhlCvpXF, NhlTFloat, sizeof(float),
		NhlOffset(ViewLayerRec,view.x),NhlTString,"0.1"},
	{ NhlNvpYF, NhlCvpYF, NhlTFloat, sizeof(float),
		NhlOffset(ViewLayerRec,view.y),NhlTString,"0.90"},
	{ NhlNvpWidthF, NhlCvpWidthF, NhlTFloat, sizeof(float),
		NhlOffset(ViewLayerRec,view.width),NhlTString,"0.8"},
	{ NhlNvpHeightF, NhlCvpHeightF, NhlTFloat, sizeof(float),
		NhlOffset(ViewLayerRec,view.height),NhlTString,"0.8"},
	{ NhlNvpKeepAspect, NhlCvpKeepAspect, NhlTInteger, sizeof(int),
		NhlOffset(ViewLayerRec,view.keep_aspect),NhlTString,"0"}
};

/*
* Base Methods used
*/
static NhlErrorTypes	ViewSetValues(
#ifdef NhlNeedProto 
	Layer,		/* old */
	Layer,		/* reference */
	Layer,		/* new */
	_NhlArgList,	/* args */
	int		/* num_args*/
#endif
);

static NhlErrorTypes	ViewInitialize(
#ifdef NhlNeedProto
	LayerClass,	/* class */
	Layer,		/* req */
	Layer,		/* new */
	_NhlArgList,	/* args */
	int		/* num_args */
#endif
);

static NhlErrorTypes	 ViewDestroy(
#ifdef NhlNeedProto
	Layer		/* inst */
#endif
);

static NhlErrorTypes	ViewClassInitialize();

static NhlErrorTypes	ViewClassPartInitialize(
#ifdef NhlNeedProto
	LayerClass	/* lc */
#endif
);

/*
* View Methods used
*/

static NhlErrorTypes    ViewGetBB(
#ifdef NhlNeedProto
	Layer		instance,
	NhlBoundingBox *thebox
#endif
);


ViewLayerClassRec viewLayerClassRec = {
        {
/* superclass			*/	(LayerClass)&baseLayerClassRec,  
/* class_name			*/	"View",
/* nrm_class			*/	NrmNULLQUARK, 
/* layer_size			*/	sizeof(ViewLayerRec),
/* layer_resources		*/	resources,
/* num_resources		*/	NhlNumber(resources),
/* child_resources		*/	NULL,
/* all_resources		*/	NULL,
/* class_part_initialize	*/	ViewClassPartInitialize,
/* class_inited			*/	False,
/* class_initialize		*/	ViewClassInitialize,
/* layer_initialize		*/	ViewInitialize,
/* layer_set_values		*/	ViewSetValues,
/* layer_set_values_not		*/	NULL,
/* layer_get_values		*/	NULL,
/* layer_pre_draw		*/	NULL,
/* layer_draw			*/	NULL,
/* layer_draw_segonly		*/	NULL,
/* layer_post_draw		*/	NULL,
/* layer_clear			*/	NULL,
/* layer_destroy		*/	ViewDestroy
        },
        {
/* segment_workstation	*/	-1,
/*
* Only need to define one of these if a) plot has children and b) if drawing
* outside of x,y,width and height
*/
/* get_bb	*/	ViewGetBB 
	}
};
	
LayerClass viewLayerClass = (LayerClass)&viewLayerClassRec;


/*
 * Function:	ViewSetValues
 *
 * Description: ViewSetValues when a change in x, y width and height has 
 *	occured the set values computes new segment transformations for each
 *	segment in the plot_segments_list. These are segment belonging to plots
 *	directly subclassed not mix-in being used by this plot. Next the set
 *	values computes a transformation matrix from the old x,y,width and 
 *	height to the new x,y,width and height. This is done using the routines
 *	located in the Segments module. With the new transformation matix all
 *      of the x,y,width, and height fields of all of the children are 
 *	transformed. If the children are subclassed from View then this whole
 *  	process is repeated recursively inside of the children. 
 *
 * In Args:	Standard set_values method argument list.
 *
 * Out Args:		new contains modified information about segments and
 *			children;	
 *
 * Return Values:	NONE
 *
 * Side Effects:	Items in plot_segments_list possibly changed.
 *			Geometry of children possibly changed.
 *			fr,fl,fb,and ft fields in instance possibly changed.
 */
/*ARGSUSED*/
static NhlErrorTypes	ViewSetValues 
#if __STDC__
(
	Layer		old,
	Layer		reference,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(old,reference,new,args,num_args)
	Layer		old;
	Layer		reference;
	Layer		new;
	_NhlArgList	args;
	int		num_args;
#endif
{
	ViewLayer	oldl = (ViewLayer)old;
	ViewLayer	newl = (ViewLayer)new;
	float		orig_x[3];
	float		orig_y[3];
	float		new_x[3];
	float		new_y[3];
	float		xl,yt,xr,yb,tmpx,tmpy,tmpwidth,tmpheight;
	LayerList	step;
	NhlSegTransList	steptrans;
	NhlErrorTypes ret = NOERROR;

	if((newl->view.x != oldl->view.x) ||
	   (newl->view.y != oldl->view.y) ||
	   (newl->view.width != oldl->view.width) ||
	   (newl->view.height != oldl->view.height)) {
/*
* MOD:
* decided if perserving A-ratio is needed then compute new width or
* height and proced.
*/
		if((newl->view.keep_aspect)&&(oldl->view.keep_aspect)){
/*
* height is too big
*/
			if(newl->view.height/newl->view.width>newl->view.aspect){
				newl->view.height = newl->view.width 	
					* newl->view.aspect;
				
			} else {
/*
* width is too big
*/
				newl->view.width = newl->view.height
					* (1.0/newl->view.aspect);
			}
			
		} else if( newl->view.keep_aspect) {
			newl->view.aspect = newl->view.height/newl->view.width;
		}

		newl->view.fl = newl->view.x;
		newl->view.fr = newl->view.x + newl->view.width;
		newl->view.ft = newl->view.y;
		newl->view.fb = newl->view.y - newl->view.height;
/*
Remember the ordering needed!!!!
p1-------------p2
|
|
|
|
|
p0

p0 = (x[0],y[0])
p1 = (x[1],y[1])
p2 = (x[2],y[2])
*/

		orig_x[0] = oldl->view.fl;
		orig_y[0] = oldl->view.fb;
		orig_x[1] = oldl->view.fl;
		orig_y[1] = oldl->view.ft;
		orig_x[2] = oldl->view.fr;
		orig_y[2] = oldl->view.ft;

		new_x[0] = newl->view.fl;
		new_y[0] = newl->view.fb;
		new_x[1] = newl->view.fl;
		new_y[1] = newl->view.ft;
		new_x[2] = newl->view.fr;
		new_y[2] = newl->view.ft;

/*
* Children are transformed first. Always compute the following transformation 
* so subclasses can use it to move points around
*/		
		(void)_NhlResetSegTransDat(newl->view.thetrans_children,
			orig_x,orig_y);
		(void)_NhlComputeSegTrans(newl->view.thetrans_children,
			newl->view.trans_children,new_x,new_y);
		if(newl->view.children != NULL) {
/*
* This stuff may not be needed or may cause some problems. Why? First consider
* how child resources are set. A field in the parent is reserved for the childs
* resource and the parent creates a new unique resource name for it. When
* the parents set_values is called it builds a arg list and calls SetValues
* on the child. Now if the child is transformed with the source below the 
* parents fields will be wrong and hence any GetValues calls that just returns
* the parents fields could return incorrect information. The solution is 
* a) either require the parent GetValues to call GetValues for the children or
* b) require the parent SetValues to recompute the childrens fields or
* c) compute only the transformation matrix above and require the parent
* 	evaluate the transformation or recompute the childrens fields.
* 
* a) is probably the best and most analogous to the SetValues behavior. It
* will just be promulgated that parents are responsible for maintaining the
* relevancy of their fields which hold values of children resources.
* 
* There is also the option to not add children to the children list.
*/
			step = newl->view.children;
			while(step!=NULL) {
				ret =NhlGetValues(step->layer->base.id,
					NhlNvpXF, &tmpx,
					NhlNvpYF, &tmpy,
					NhlNvpWidthF, &tmpwidth,
					NhlNvpHeightF, &tmpheight, NULL);
				if(ret < WARNING)
					return(ret);

				_NhlEvalTrans(newl->view.trans_children,
					tmpx,tmpy,&xl,&yt);
				_NhlEvalTrans(newl->view.trans_children,
					tmpx+tmpwidth,tmpy-tmpheight,&xr,&yb);

				ret = NhlSetValues(step->layer->base.id,
					NhlNvpXF, xl,
					NhlNvpYF, yt,
					NhlNvpWidthF, xr - xl,
					NhlNvpHeightF, yt - yb, NULL);
				if(ret<WARNING)
					return(ret);

				step = step->next;
			}

		}
/*
* Segment Transformations belonging to this instance are computed and set.
* All segments in this instance must use the x,y,width,height of this instance
* to determine where they plot. In this way all segment transformations can
* computed from the new x,y,width and height and the transformation will be
* valid. The transformation matices of each segement may be different however
* depending on whether any of the segments have been redrawn since they were
* originally drawn.
*/
		if(newl->view.plot_segments_list != NULL) {
			steptrans = newl->view.plot_segments_list;
			while(steptrans != NULL) {
				_NhlComputeSegTrans(steptrans->seg_trans_dat,
					steptrans->seg_trans,new_x,new_y);
				_NhlSetSegTrans( steptrans->seg_trans_dat,
					steptrans->seg_trans);
				steptrans = steptrans->next;
			}
		}
	} 
	return(ret);
}
	
/*
 * Function:	ViewInitialize
 *
 * Description: Computes fractional coordinate points fl,fr,ft,fb. Initializes
 *		thetrans_children, plot_segments_list, and copies the 
 *		segment workstation id into the instance structure from the 
 *		Class.
 *
 * In Args: 	Standard initialize method parameters
 *
 * Out Args:	fields in newl changed.
 *
 * Return Values: none
 *
 * Side Effects: fields in new changed.
 */
/*ARGSUSED*/
static NhlErrorTypes	ViewInitialize
#if __STDC__
(
	LayerClass 	class,
	Layer		req,
	Layer		new,
	_NhlArgList	args,
	int		num_args
)
#else
(class,req,new,args,num_args)
	LayerClass 		class;
	Layer			req;
	Layer			new;
	_NhlArgList		args;
#endif
{
	ViewLayer	newl = (ViewLayer) new;
	ViewLayerClass	lcl = (ViewLayerClass) class;
	float	x[3],y[3];
	NhlErrorTypes ret = NOERROR;
/*
* vpX,vpY,vpWidth and vpHeight better be set already !!!
* These should be set with system, user, or application defaults or through
* the create call
*/
	newl->view.fl = newl->view.x;
	newl->view.fr = newl->view.x + newl->view.width;
	newl->view.ft = newl->view.y;
	newl->view.fb = newl->view.y - newl->view.height;

	x[0] = newl->view.fl;
	y[0] = newl->view.fb;
	x[1] = newl->view.fl;
	y[1] = newl->view.ft;
	x[2] = newl->view.fr;
	y[2] = newl->view.ft;


	newl->view.thetrans_children = _NhlInitSegTransDat(x,y);
	
	newl->view.plot_segments_list = NULL;
	newl->view.segment_wksid = lcl->view_class.segment_workstation;
	newl->view.children = NULL;

	newl->view.aspect = newl->view.height/newl->view.width;
	return(ret);
}



/*
 * Function:	ViewClassPartInitialize
 *
 * Description:	Used to propagate the segment_workstation id down the class
 *		heirarchy.
 *
 * In Args:	lc
 *
 * Out Args:	lc
 *
 * Return Values: NONE
 *
 * Side Effects: 
 */
static NhlErrorTypes	ViewClassPartInitialize
#if __STDC__
(
	LayerClass	lc
)
#else
(lc)
	LayerClass 	lc;
#endif
{
	ViewLayerClass	vlc = (ViewLayerClass)lc;
	LayerClass	sc = vlc->base_class.superclass;

	if(sc != &baseLayerClassRec) {
		vlc->view_class.segment_workstation = 
			((ViewLayerClass)sc)->view_class.segment_workstation;
	}
	return(NOERROR);
}

/*
 * Function:	ViewClassInitialize
 *
 * Description: Opens the segment workstation.
 *
 * In Args: NONE
 *
 * Out Args: NONE
 *
 * Return Values: NONE
 *
 * Side Effects: Sets the segment_workstation field in the viewLayerClassRec
 * 		structure which is then propagated down the class heirarchy
 *		by ClassPartInitialize.
 */
static NhlErrorTypes	ViewClassInitialize()
{
	
	int i = NhlDEFAULT_SEG_WKS;
	int cid = NhlDEFAULT_CONNECTION_ID;
	int wtp = NhlDEFAULT_SEG_WKS_TYPE;

/*
* GKS BETTER BE OPEN !!!!
*/

	while(wksisopn(i)) {
		i++;
	}
/* FORTRAN */ gopwk_(&i,&cid,&wtp);
	gactivate_ws(i);
	viewLayerClassRec.view_class.segment_workstation = i;
	return(NOERROR);
}

/*
 * Function:	ViewDestroy
 *
 * Description:	Frees dynamically allocated parts of the instance record.
 *
 * In Args: inst
 *
 * Out Args: inst
 *
 * Return Values: NONE
 *
 * Side Effects: transformations are gone gone gone.
 */

static NhlErrorTypes	ViewDestroy
#if __STDC__
(Layer inst )
#else
(inst)
	Layer inst;
#endif
{
	ViewLayer layer = (ViewLayer) inst;
	NhlSegTransList	step,tmp;
	LayerList	step1,tmp1;

	step1 = layer->view.children;
	if(step1 != NULL)
	_NhlDestroySegTransDat(layer->view.thetrans_children);
	layer->view.children = NULL;
	while(step1 != NULL) {
		tmp1 = step1->next;
		NhlFree(step1);
		step1 = tmp1;
	}

	step = layer->view.plot_segments_list;
	layer->view.plot_segments_list = NULL;
	while(step != NULL) {
		_NhlDestroySegTransDat(step->seg_trans_dat);
		tmp = step->next;
		NhlFree(step);
		step = tmp;
	}
	
	return(NOERROR);

}

/*
 * Function:	_NhlAddViewChildLayer
 *
 * Description: Inserts into the children list a new child for which 
 *	geometry changes will be automatically propagated.
 *
 * In Args: instance	is the parent layer instance;
 *	    child	pointer to child to be inserted.	
 *
 * Out Args: NONE
 *
 * Return Values: NONE
 *
 * Side Effects: instance structure contents modified.
 */

void _NhlAddViewChildLayer
#if __STDC__
( Layer  instance , Layer  child )
#else
(instance,child)
	Layer	instance;
	Layer	child;
#endif
{
	ViewLayer parent = (ViewLayer) instance;
	LayerList	step;

	if(parent->view.children == NULL) {
		parent->view.children = (LayerList)NhlMalloc(
				(unsigned)sizeof(LayerListNode));
		parent->view.children->next = NULL;
		parent->view.children->layer = child;
		return;
	} else {
		step = (LayerList)NhlMalloc((unsigned)sizeof(LayerListNode));
		step->layer = child;
		step->next = parent->view.children;
		parent->view.children = step;
		return;
	}
	
}


/*
 * Function:	_NhlDeleteViewChildLayer
 *
 * Description: Searches current child list for child and then deletes and
 *		frees storage.
 *
 * In Args: instance	is the current View instance
 *	    child	is the child to delete
 *
 * Out Args: NONE
 *
 * Return Values: NONE
 *
 * Side Effects: child list is altered in parent.
 */

void _NhlDeleteViewChildLayer
#if	__STDC__
( Layer instance , Layer child )
#else
(instance,child)
	Layer	instance;
	Layer	child;
#endif
{
	ViewLayer	parent = (ViewLayer) instance;
	LayerList	step,tmp;

	step = parent->view.children;

	if(step != NULL) {
		if(step->layer == child) {
			parent->view.children = step->next;
			NhlFree(step);
			return;
		}
		while(step->next != NULL) {
			if(step->next->layer == child) {
				tmp = step->next;
				step->next = step->next->next;
				NhlFree(tmp);
				return;	
			}
			step = step->next;
		}
	}
	return;
}


/*
 * Function:	_NhlNewViewSegment
 *
 * Description: Returns a new NhlTransDat instance to the callee and inserts
 *		the new instance into the current istances plot_segments_list.
 *
 * In Args: 	instance	the instance to add a new segment to.
 *
 * Out Args:	NONE
 *
 * Return Values:	Pointer to new NhlTransDat instance.
 *
 * Side Effects:	Changes to instance plot_segments_list field.
 */

NhlTransDat *_NhlNewViewSegment
#if __STDC__
(Layer instance )
#else
(instance)
	Layer	instance;
#endif
{
	ViewLayer	parent = (ViewLayer) instance;
	NhlSegTransList	step;
	float	x[3],y[3];

	x[0] = parent->view.fl;
	y[0] = parent->view.fb;
	x[1] = parent->view.fl;
	y[1] = parent->view.ft;
	x[2] = parent->view.fr;
	y[2] = parent->view.ft;



	if(parent->view.plot_segments_list == NULL) {
		parent->view.plot_segments_list = (NhlSegTransList)
				NhlMalloc(sizeof(NhlSegTransListNode));	
		step = parent->view.plot_segments_list;
		step->next = NULL;
		step->seg_trans_dat = _NhlInitSegTransDat(x,y);
		
	} else {
		step = (NhlSegTransList)NhlMalloc(sizeof(NhlSegTransListNode));
		step->seg_trans_dat = _NhlInitSegTransDat(x,y);
		step->next = parent->view.plot_segments_list;
		parent->view.plot_segments_list = step;

	}
/*
* Initialize transformation just in case
*/

		step->seg_trans[0] = 1.0;
		step->seg_trans[1] = 0.0;
		step->seg_trans[2] = 0.0;
		step->seg_trans[3] = 1.0;
		step->seg_trans[4] = 0.0;
		step->seg_trans[5] = 0.0;
		return(step->seg_trans_dat);
}


/*
 * Function:	_NhlDeleteViewSegment
 *
 * Description: Does oposite of _NhlNewViewSegment.
 *
 * In Args:  instance 	current layer instance
 *	     transdat	pointer to NhlTransDat to delete
 *
 * Out Args: NONE
 *
 * Return Values: NONE
 *
 * Side Effects: plot_segments_list changed in instance. Segment deleted
 *		from GKS.
 */

void _NhlDeleteViewSegment
#if	__STDC__
(Layer    instance, NhlTransDat*  transdat )
#else
(instance,transdat)
	Layer	instance;
	NhlTransDat*	transdat;
#endif
{
	ViewLayer	parent = (ViewLayer) instance;
	NhlSegTransList	step,tmp;

	step = parent->view.plot_segments_list;

	if(parent->view.plot_segments_list != NULL) {
		if(parent->view.plot_segments_list->seg_trans_dat == transdat) {
			step = parent->view.plot_segments_list;
			parent->view.plot_segments_list = step->next;
			_NhlDestroySegTransDat(step->seg_trans_dat);
			NhlFree(step);
		} else {
			while(step->next != NULL) {
				if(step->next->seg_trans_dat == transdat) {
					tmp = step->next;
					step->next = step->next->next;
					_NhlDestroySegTransDat(tmp->seg_trans_dat);
					NhlFree(tmp);
				}
			}
		}
	}
	return;
}



/*
 * Function:	_NhlResetViewSegment
 *
 * Description:  Resets the segment tranformation so that current location
 *		is the identity transformation. This must be called every
 *		time a segment is redrawn. 
 *
 * In Args:	Layer instance		view object instance
 *		NhlTransDat *segdat	segment transformation data needed for
 *					drawing segment.
 *
 * Out Args:	NONE
 *
 * Return Values: Error values	
 *
 * Side Effects: segdat contains new location information so future 
 *		transformations can be compute from segdat.
 */
NhlErrorTypes _NhlResetViewSegment
#if __STDC__
(Layer instance,NhlTransDat *segdat )
#else
(instance,segdat)
	Layer	instance;
	NhlTransDat *segdat;
#endif
{
	ViewLayer	parent = (ViewLayer) instance;
	NhlSegTransList	step;
	float	x[3],y[3];

	x[0] = parent->view.fl;
	y[0] = parent->view.fb;
	x[1] = parent->view.fl;
	y[1] = parent->view.ft;
	x[2] = parent->view.fr;
	y[2] = parent->view.ft;


	
	
	_NhlResetSegTransDat(segdat,x,y);
	
	step = parent->view.plot_segments_list;
	if(step == NULL) {
		NhlPError(WARNING,E_UNKNOWN,"_NhlResetViewSegment: No segments initialized, can't reset segment");
		return(WARNING);
	} else {
		while(step != NULL) {
			if(step->seg_trans_dat->id == segdat->id) {
				break;
			}
		}
		if(step  == NULL) {
			NhlPError(WARNING,E_UNKNOWN,"_NhlResetViewSegment: No segments found in view segment list");
			return(WARNING);
		} else {
/*
* Important to reset segment transformation to identity 
*/

			step->seg_trans[0] = 1.0;
			step->seg_trans[1] = 0.0;
			step->seg_trans[2] = 0.0;
			step->seg_trans[3] = 1.0;
			step->seg_trans[4] = 0.0;
			step->seg_trans[5] = 0.0;
		}
	}
	return(NOERROR);
}


/*
 * Function:	_NhlAddBBInfo
 *
 * Description:	public convienence function for setting bounding box 
 *		inforormation in the context of at get_bb method.
 *
 * In Args:	t	top value in fractional coordinate system.
 *		b	bottom value in fractional coordinate system.
 *		r	right      "
 *		l       left       "
 *
 * Out Args:
 *		thebox	Bounding box information for current instance
 *
 * Return Values: NONE
 *
 * Side Effects: thebox  changes.
 */
void	_NhlAddBBInfo
#if __STDC__
(float t, float b, float r, float l, NhlBoundingBox *thebox)
#else
(t,b,r,l,thebox)
	float t;
	float b;
	float r;
	float l;
	NhlBoundingBox *thebox;
#endif
{

	if(!thebox->set) {
		thebox->set = 1;
		thebox->t = t;
		thebox->r = r;
		thebox->b = b;
		thebox->l = l;
	} else {
		if(thebox->t < t) {
			thebox->t = t;
		} 
		if(thebox->b > b) {
			thebox->b = b;
		} 
		if(thebox->r < r) {
			thebox->r = r;
		} 
		if(thebox->l > l) {
			thebox->l = l;
		} 
	}
	return;
}

/*
 * Function:	ViewGetBB	
 *
 * Description: Simply performs default BB function by adding objects viewport
 *		to boudning box data structure.
 *
 * In Args:	Layer instance		object instance for which BB is req'ed
 *		NhlBoundingBox *thebox	Data Structure containing BB info
 *
 * Out Args:	NONE
 *
 * Return Values: Error types
 *
 * Side Effects: thebox is modified
 */
static NhlErrorTypes ViewGetBB
#if __STDC__
(Layer instance, NhlBoundingBox *thebox)
#else
(instance,thebox)
	Layer instance;
	NhlBoundingBox *thebox;
#endif
{
	ViewLayer vinstance = (ViewLayer) instance;

	_NhlAddBBInfo(vinstance->view.ft,vinstance->view.fb,
		vinstance->view.fr,vinstance->view.fl,thebox);
	
	return(NOERROR);
	
}

/*
 * Function:	_NhlInternalSetView
 *
 * Description: Is used by subclasses of view to adjuct location for object
 *		without invoking the public SetValues interface. This means that
 *		the geometry of the children and segments remain unaffected.
 *
 * In Args:	ViewLayer theview	instance of object
 *		float x,y,width,height  new viewport information
 *		int keep_asp		new keep_aspect resource
 *
 * Out Args: NONE
 *
 * Return Values: NONE
 *
 * Side Effects: theview values changed.
 */
void _NhlInternalSetView
#if __STDC__
(ViewLayer theview,float x, float y, float width, float height, int keep_asp)
#else
(theview,x,y,width,height,keep_asp)
ViewLayer theview;
float	x;
float	y;
float	width;
float	height;
int	keep_asp;
#endif
{
	float xpoints[3];
	float ypoints[3];

        theview->view.x = x;
        theview->view.y = y;
        theview->view.width = width;
        theview->view.height = height;
        theview->view.aspect = theview->view.height/theview->view.width;
        theview->view.keep_aspect = keep_asp;
        theview->view.fl = theview->view.x;
        theview->view.fr = theview->view.x + theview->view.width;
        theview->view.fb = theview->view.y - theview->view.height;
        theview->view.ft = theview->view.y;
        xpoints[0] = theview->view.fl;
        ypoints[0] = theview->view.fb;
        xpoints[1] = theview->view.fl;
        ypoints[1] = theview->view.ft;
        xpoints[2] = theview->view.fr;
        ypoints[2] = theview->view.ft;
        _NhlResetSegTransDat(theview->view.thetrans_children,xpoints,ypoints);
	return;

}

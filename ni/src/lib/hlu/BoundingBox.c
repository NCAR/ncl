/*
 *      $Id: BoundingBox.c,v 1.9.4.1 2008-03-28 20:37:33 grubin Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		BoundingBox.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 1 17:02:50 MDT 1992
 *
 *	Description:	Public function for retrieving bounding box 
 *			information. It will return four points that
 *			represent a bounding box that encloses all of the
 *			elements of a plot that the plot wants to be grouped
 *			into one box. Sometimes a plot may not include 
 *			items like labelbars and legends as part of the
 *			BB. This is up to 	
 */
#include <stdio.h>
#include <string.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ViewP.h>



/*
 * Function:	CallGetBB
 *
 * Description:	Recursive function that calls get_bb method for Views
 *
 * In Args:	instance	current instance.
 *		class		current supperclass.
 *		thebox		current bounding box information
 *
 * Out Args:	thebox		new bounding box information
 *
 * Return Values:	error
 *
 * Side Effects: NONE
 */
static NhlErrorTypes CallGetBB
#if	NhlNeedProto
(
NhlLayer instance,
NhlClass class,
NhlBoundingBox *thebox
)
#else
(instance,class,thebox)
	NhlLayer	instance;
	NhlClass class;
	NhlBoundingBox *thebox;
#endif
{
	NhlViewClass	vclass = (NhlViewClass) class;
	NhlErrorTypes ancestor=NhlNOERROR, ret = NhlNOERROR;

	if(vclass->base_class.superclass->base_class.class_inited &
							_NhlViewClassFlag){
		ancestor =CallGetBB(instance,vclass->base_class.superclass,
			thebox);

		if(ancestor < NhlWARNING)
			return(ancestor);
	}

	if(vclass->view_class.get_bb != NULL) 
		ret = (*(vclass->view_class.get_bb))(instance,thebox);

	return(MIN(ancestor,ret));
}

/*
 * Function:	_NhlGetBB
 *
 * Description: Internal _NhlGetBB function that makes sure layer is a view
 *		type of object and resets the BoundingBox struct that was
 *		passed in then it calls CallGetBB.
 *
 * In Args:	instance	current Layer instance
 *		thebox		structure passed infrom calling enironment.
 *
 * Out Args:	thebox		contains BB info for instance
 *
 * Return Values: NONE
 *
 * Side Effects: NONE
 */
NhlErrorTypes _NhlGetBB
#if	NhlNeedProto
(
	NhlLayer instance,
	NhlBoundingBox *thebox
)
#else
(instance,thebox)
	NhlLayer instance;
	NhlBoundingBox* thebox;
#endif
{
	if(_NhlIsView(instance)) {
		return(CallGetBB(instance,instance->base.layer_class,
				thebox));
	} else {

	NhlPError(NhlFATAL,NhlEUNKNOWN,"Can't get  BB info for non-view object");

		return(NhlFATAL);
	}
}

/*
 * Function:	NhlGetBB
 *
 * Description:	public function for Getting BB info for an object.
 *
 * In Args: 	pid		plot id
 *		thebox		struct to place BB info in.
 *
 * Out Args:	thebox		contains BB infor
 *
 * Return Values: error types
 *
 * Side Effects: NONE
 */

NhlErrorTypes NhlGetBB
#if	NhlNeedProto
(int pid,NhlBoundingBox *thebox)
#else
(pid,thebox)
	int pid;
	NhlBoundingBox *thebox;
#endif
{
	NhlLayer instance;

	thebox->set = 0;
	thebox->t  = 0.0;
	thebox->b  = 0.0;
	thebox->l  = 0.0;
	thebox->r  = 0.0;
	if (NhlIsView(pid)) {
		instance = _NhlGetLayer(pid);
		return(_NhlGetBB(instance,thebox));
	}	
	else if (NhlIsWorkstation(pid)) {
		int *views;
		int i;
		ng_size_t count;
		int grlist;
		NhlErrorTypes ret = NhlNOERROR,subret;

		grlist = NhlRLCreate(NhlGETRL);
		NhlRLClear(grlist);
		NhlRLGetIntegerArray(grlist,NhlNwkTopLevelViews,&views,&count);
		NhlGetValues(pid,grlist);
		for (i = 0; i < count; i++) {
			instance = _NhlGetLayer(views[i]);
		        subret = _NhlGetBB(instance,thebox);
			if ((ret = MIN(ret,subret)) < NhlWARNING) {
				return ret;
			}
		}
		NhlFree(views);
		NhlRLDestroy(grlist);
		return ret;
	}
	else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid plot ID=%d passed to NhlGetBB",pid);
		return(NhlFATAL);
	}
}

/*
 * Function:	nhlf_getbb
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NHLCALLF(nhlfpgetbb,NHLFPGETBB)
#if	NhlNeedProto
(
	int	*pid,
	float	*top,
	float	*bottom,
	float	*left,
	float	*right,
	int	*err
)
#else
(pid,top,bottom,left,right,err)
	int	*pid;
	float	*top;
	float	*bottom;
	float	*left;
	float	*right;
	int	*err;
#endif
{
	NhlBoundingBox	box;

	*err = NhlGetBB(*pid,&box);

	*top = box.t;
	*bottom = box.b;
	*left = box.l;
	*right = box.r;

	return;
}

/*
 *      $Id: Reparent.c,v 1.4 1994-01-10 19:48:49 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Reparent.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Feb 17 16:48:13 MST 1993
 *
 *	Description:	This file contains the functions needed to reparent
 *			a plot object to a new workstation parent. Design docs
 *			are NhlDOCREF(/design/hlu/Reparent.html,here)
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>

/*
 * Function:	CallReparent
 *
 * Description:	This function calls the the reparent methode of the layer
 *		provided.  This is a down chained method - ie the base
 *		is called first down to the actual class of the layer.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallReparent
#if	__STDC__
(
	Layer		l,	/* layer to call reparent for	*/
	Layer		parent,	/* layer to call reparent for	*/
	LayerClass	lc	/* class or superclass of l	*/
)
#else
(l,parent,lc)
	Layer		l;	/* layer to call reparent for	*/
	Layer		parent;	/* layer to call reparent for	*/
	LayerClass	lc;	/* class or superclass of l	*/
#endif
{
	NhlErrorTypes	scret = NOERROR;
	NhlErrorTypes	lcret = NOERROR;

	if(lc->base_class.superclass != NULL){
		scret = CallReparent(l,parent,lc->base_class.superclass);

		if(scret < WARNING)
			return scret;
	}

	if(lc->base_class.layer_reparent != NULL)
		lcret = (*(lc->base_class.layer_reparent))(l,parent);

	return MIN(scret,lcret);
}

/*
 * Function:	Reparent
 *
 * Description:	This function reparents a given layer to a new parent layer.
 *		Eventually it calls a methode in the child so the
 *		child has a chance to change any resources it wants, based
 *		on it's new hierachy.
 *
 * In Args:	
 *		Layer	child,	child to re-parent
 *		Layer	parent	new parent
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	child get's reparented to parent - resource values of child
 *		may change if the child has a reparent method that does that.
 */
static NhlErrorTypes
Reparent
#if	__STDC__
(
	Layer	child,	/* child to re-parent	*/
	Layer	parent	/* new parent		*/
)
#else
(child,parent)
	Layer	child;	/* child to re-parent	*/
	Layer	parent;	/* new parent		*/
#endif
{
	_NhlAllChildList	*tnodeptr;
	_NhlAllChildList	tnode;

	if(child->base.parent == (Layer)NULL){
		NhlPError(FATAL,E_UNKNOWN,
					"Can not reparent a parentless child");
		return FATAL;
	}

	if(_NhlIsObj(parent)){
		NhlPError(FATAL,E_UNKNOWN,
					"Parent found unsuitable for adoption");
		return FATAL;
	}

	if(parent != child->base.parent){
		/*
		 * First remove child from it's parent's all_children list
		 * set tnode to the child's record.
		 */

		tnodeptr = &child->base.parent->base.all_children;
		tnode = (_NhlAllChildList)NULL;

		while(*tnodeptr != (_NhlAllChildList)NULL){
			if((*tnodeptr)->pid == child->base.id){
				tnode = *tnodeptr;
				*tnodeptr = (*tnodeptr)->next;
				break;
			}
			tnodeptr = &(*tnodeptr)->next;
		}

		if(tnode == (_NhlAllChildList)NULL){
			NhlPError(FATAL,E_UNKNOWN,
			"Unable to remove PID#%d from parent",child->base.id);
			return FATAL;
		}

		/*
		 * Add child's record to the new parent
		 */
		tnode->next = parent->base.all_children;
		parent->base.all_children = tnode;

		/*
		 * Change child's parent pointer to new parent
		 */
		child->base.parent = parent;
	}

	return CallReparent(child,parent,child->base.layer_class);
}

/*
 * Function:	_NhlReparent
 *
 * Description:	This function takes a two pid's.  The first one is
 *		the child the second is the new parent for that child.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Private
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(_NhlReparent)
NhlErrorTypes
_NhlReparent
#if	__STDC__
(
	Layer	child,
	Layer	parent
)
#else
(child,parent)
	Layer	child;
	Layer	parent;
#endif
{
	if((child == NULL) || (parent == NULL)){
		NhlPError(FATAL,E_UNKNOWN,"_NhlReparent:passed a NULL layer");

		return FATAL;
	}

	return Reparent(child,parent);
}

/*
 * Function:	NhlChangeWorkstation
 *
 * Description:	This function takes a plot id, and a Workstation id and
 *		reparents the plot to the Workstation as well as reseting
 *		the Worstation pointer of the plot and all it's desendents
 *		to the new Workstation.  This function only accepts direct
 *		children of Workstations as valid plot's to reparent.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlDOCTAG(NhlChangeWorkstation)
NhlErrorTypes
NhlChangeWorkstation
#if	__STDC__
(
	int	plotid,		/* Id of plot to reparent	*/
	int	workid		/* Id of new Workstation parent	*/
)
#else
(plotid,workid)
	int	plotid;		/* Id of plot to reparent	*/
	int	workid;		/* Id of new Workstation parent	*/
#endif
{
	Layer		plot = _NhlGetLayer(plotid);
	Layer		work = _NhlGetLayer(workid);

	if((plot == (Layer)NULL) || (work == (Layer)NULL)){
		NhlPError(FATAL,E_UNKNOWN,
			"NhlChangeWorkstation-invalid plotid=%d or workid=%d",
								plotid,workid);
		return FATAL;
	}

	if(!_NhlIsWorkstation(plot->base.parent)){
		NhlPError(FATAL,E_UNKNOWN,
			"ChangeWorkstation-plot must be child of Workstation");
		return FATAL;
	}

	if(!_NhlIsWorkstation(work)){
		NhlPError(FATAL,E_UNKNOWN,
			"ChangeWorkstation-workid must be id of Workstation");
		return FATAL;
	}

	return Reparent(plot,work);
}

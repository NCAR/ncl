/*
 *      $Id: Reparent.c,v 1.1 1993-04-30 17:23:49 boote Exp $
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
 *			a plot object to a new workstation parent.
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
/*ARGSUSED*/
static NhlErrorTypes
CallReparent
#if	__STDC__
(
	Layer		l,	/* layer to call reparent for	*/
	LayerClass	lc	/* class or superclass of l	*/
)
#else
(l,lc)
	Layer		l;	/* layer to call reparent for	*/
	LayerClass	lc;	/* class or superclass of l	*/
#endif
{
	NhlErrorTypes	scret = NOERROR;
	NhlErrorTypes	lcret = NOERROR;

#ifdef	NOTYET
	if(lc->base_class.superclass != NULL){
		scret = CallReparent(l,lc->base_class.superclass);

		if(scret < WARNING)
			return scret;
	}

	if(lc->base_class.reparent != NULL)
		lcret = (*(lc->base_class.reparent))(l);

#endif
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
				"Unable to remove PID#%d from parent",
								child->base.id);
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

	return CallReparent(child,child->base.layer_class);
}

/*
 * Function:	ChangeWorkstation
 *
 * Description:	This function changes the workstation pointer of a layer
 *		and all it's children. This function assumes it is given
 *		a valid Workstation Layer.  It will however, check that
 *		the given pid is valid.
 *
 * In Args:	
 *		int	pid,	layer to change workstation pointer for
 *		Layer	w	workstation pointer to change to
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
ChangeWorkstation
#if	__STDC__
(
	int	pid,	/* layer to change workstation pointer for	*/
	Layer	w	/* workstation pointer to change to		*/
)
#else
(pid,w)
	int	pid;	/* layer to change workstation pointer for	*/
	Layer	w;	/* workstation pointer to change to		*/
#endif
{
	Layer			l = _NhlGetLayer(pid);
	_NhlAllChildList	tnode;
	NhlErrorTypes		ret = NOERROR, lret = NOERROR;

	if(l == (Layer)NULL){
		NhlPError(WARNING,E_UNKNOWN,
			"Trying to change workstation of non-existant PID#%d",
									pid);
		return WARNING;
	}

	tnode = l->base.all_children;

	while(tnode != (_NhlAllChildList)NULL){
		lret = ChangeWorkstation(tnode->pid,w);

		ret = MIN(lret,ret);

		tnode = tnode->next;
	}

	l->base.wkptr = w;

	return ret;
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
	NhlErrorTypes	ret = NOERROR, lret = NOERROR;

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

	ret = Reparent(plot,work);

	if(ret < WARNING){
		return ret;
	}

	lret = ChangeWorkstation(plot->base.id,work);

	if(lret < WARNING){
		return lret;
	}

	return MIN(ret,lret);
}

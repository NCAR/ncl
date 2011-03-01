/*
 *      $Id: Reparent.c,v 1.12 1997-05-14 22:53:35 boote Exp $
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
#include <ncarg/hlu/TransformI.h>

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
#if	NhlNeedProto
(
	NhlLayer	l,	/* layer to call reparent for	*/
	NhlLayer	parent,	/* layer to call reparent for	*/
	NhlClass	lc	/* class or superclass of l	*/
)
#else
(l,parent,lc)
	NhlLayer	l;	/* layer to call reparent for	*/
	NhlLayer	parent;	/* layer to call reparent for	*/
	NhlClass	lc;	/* class or superclass of l	*/
#endif
{
	NhlErrorTypes	scret = NhlNOERROR;
	NhlErrorTypes	lcret = NhlNOERROR;

	if(lc->base_class.superclass != NULL){
		scret = CallReparent(l,parent,lc->base_class.superclass);

		if(scret < NhlWARNING)
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
 *		NhlLayer	child,	child to re-parent
 *		NhlLayer	parent	new parent
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
#if	NhlNeedProto
(
	NhlLayer	child,	/* child to re-parent	*/
	NhlLayer	parent	/* new parent		*/
)
#else
(child,parent)
	NhlLayer	child;	/* child to re-parent	*/
	NhlLayer	parent;	/* new parent		*/
#endif
{

	if(child->base.parent == (NhlLayer)NULL){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Can not reparent a parentless child");
		return NhlFATAL;
	}

	if(_NhlIsObj(parent)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"Parent found unsuitable for adoption");
		return NhlFATAL;
	}

	if(!_NhlBaseMoveChild(parent,child))
		return NhlFATAL;

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
#if	NhlNeedProto
(
	NhlLayer	child,
	NhlLayer	parent
)
#else
(child,parent)
	NhlLayer	child;
	NhlLayer	parent;
#endif
{
	if((child == NULL) || (parent == NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
					"_NhlReparent:passed a NULL layer");

		return NhlFATAL;
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
#if	NhlNeedProto
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
	NhlLayer		plot = _NhlGetLayer(plotid);
	NhlLayer		work = _NhlGetLayer(workid);

	if((plot == (NhlLayer)NULL) || (work == (NhlLayer)NULL)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"NhlChangeWorkstation-invalid plotid=%d or workid=%d",
								plotid,workid);
		return NhlFATAL;
	}

	if(!_NhlIsWorkstation(plot->base.parent)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"ChangeWorkstation-plot must be child of Workstation");
		return NhlFATAL;
	}
	if(_NhlIsPlotMember(plotid)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
	       "ChangeWorkstation-plot must not be an annotation or overlay");
		return NhlFATAL;
	}

	if(!_NhlIsWorkstation(work)){
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"ChangeWorkstation-workid must be id of Workstation");
		return NhlFATAL;
	}

	return Reparent(plot,work);
}

/*
 * Function:	nhlf_changeworkstation
 *
 * Description:	fortran wrapper for NhlChangeWorkstation
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	global private fortran
 * Returns:	void
 * Side Effect:	
 */
void
_NHLCALLF(nhlfpchangeworkstation,NHLFPCHANGEWORKSTATION)
#if	NhlNeedProto
(
	int	*pid,
	int	*wid,
	int	*err
)
#else
(pid,wid,err)
	int	*pid;
	int	*wid;
	int	*err;
#endif
{
	*err = NhlChangeWorkstation(*pid,*wid);

	return;
}

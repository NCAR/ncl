/*
 *      $Id: Draw.c,v 1.2 1993-10-19 17:50:41 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Draw.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:33:52 MDT 1992
 *
 *	Description:	This file contains all the functions neccessary for
 *			a given layer instance to draw it's information.
 */
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>


/*
 * Function:	CallDraw
 *
 * Description:	This function is used to call the draw method, it is
 *		a super-to-sub classed chained method that actually
 *		makes the ncarg graphics calls.  This function is
 *		the one that actually traverses the class hierachy
 *		of the object.
 *
 * In Args:	
 *		Layer		l,	object to draw
 *		LayerClass	class	class or super-class of object
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
static NhlErrorTypes
CallDraw
#if	__STDC__
(
	Layer		l,	/* object to draw	*/
	LayerClass	class	/* class or super-class of object	*/
)
#else
(l,class)
	Layer		l;	/* object to draw	*/
	LayerClass	class;	/* class or super-class of object	*/
#endif
{
	NhlErrorTypes superclassret = NOERROR, localret = NOERROR;

	if(class->base_class.superclass != NULL ) {
		superclassret = CallDraw(l,class->base_class.superclass);
		if(superclassret < WARNING)
			return superclassret;
	}

	if(class->base_class.layer_draw != NULL) {
		localret = (*(class->base_class.layer_draw))(l);
	}

	return(MIN(superclassret,localret));
}

/*
 * Function:	NhlDraw
 *
 * Description:	This function notifies an object it should draw by calling
 *		the draw method of the object.
 *
 * In Args:	
 *		int	id	id of object to draw
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	NhlErrorTypes
 * Side Effect:	
 */
NhlErrorTypes
NhlDraw
#if	__STDC__
(
	int	id	/* id of object to draw	*/
)
#else
(id)
	int id;
#endif
{
	Layer layer = _NhlGetLayer(id);

	if((layer == NULL) || !_NhlIsBase(layer)){
		NhlPError(FATAL,E_UNKNOWN,
				"Invalid plot ID= %d passed to NhlDraw",id);
		return(FATAL);
	}

	return CallDraw(layer,layer->base.layer_class);
}

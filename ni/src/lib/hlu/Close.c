/*
 *      $Id: Close.c,v 1.12 1997-05-05 21:45:00 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Close.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 13:32:01 MDT 1992
 *
 *	Description:	
 */
#include <stdio.h>
#include <ncarg/hlu/AppP.h>


/*
 * Function:	_NhlClose
 *
 * Description:	This function is responsible for free'ing all the memory
 *		allocated on behalf of the hlu library.
 *
 * In Args:	void
 *
 * Out Args:	
 *
 * Scope:	Global Public
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static void _NhlClose
#if	NhlNeedProto
(
	_NhlC_OR_F	close_type
)
#else
(close_type)
	_NhlC_OR_F	close_type;
#endif
{
	/*
	 * Destroy all remaining layer objects. By destroying the "default_app".
	 */
	if(NhlappClassRec.app_class.default_app)
		NhlDestroy(NhlappClassRec.app_class.default_app->base.id);

	return;
}

/*
 * Function:	NhlClose
 *
 * Description:	"C" interface close function
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
NhlClose
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlClose(_NhlCLIB);

	return;
}

/*
 * Function:	_nhlfclose
 *
 * Description:	"Fortran" interface close function.
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
_NHLCALLF(nhlpfclose,NHLPFCLOSE)
#if	NhlNeedProto
(
	void
)
#else
()
#endif
{
	_NhlClose(_NhlFLIB);

	return;
}

/*
 *      $Id: Close.c,v 1.6 1994-05-12 23:50:27 boote Exp $
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
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/ErrorP.h>
#include <ncarg/hlu/WorkspaceP.h>


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
 * Returns:	void (should mabey be NhlErrorTypes?)
 * Side Effect:	
 */
/*ARGSUSED*/
static void _NhlClose
#if	__STDC__
(
	_NhlC_OR_F	close_type
)
#else
(close_type)
	_NhlC_OR_F	close_type;
#endif
{
	/* destroy the workspace manager */

	_NhlCloseWorkspace();

	/* free error handling	*/
	_NhlCloseError();
	_NhlDestroyResDatabase();
	_NhlDestroyLayerTable();
	_NhlDestroyRLList();

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
#if	__STDC__
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
_NHLCALLF(nhl_fclose,NHL_FCLOSE)
#if	__STDC__
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

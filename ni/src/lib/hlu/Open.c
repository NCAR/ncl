/*
 *      $Id: Open.c,v 1.7 1994-05-12 23:51:57 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Open.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 09:50:11 MDT 1992
 *
 *	Description:	This file contains the functions neccessary to
 *			initialize the hlu library.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/FortranP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/ErrorP.h>
#include <ncarg/hlu/WorkspaceP.h>

/*
 * Function:	_NhlOpen
 *
 * Description:	internal init function - called for "C" and  "Fortran"
 *		interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	static
 * Returns:	void
 * Side Effect:	
 */
/*ARGSUSED*/
static void _NhlOpen
#if	__STDC__
(
	_NhlC_OR_F	init_type
)
#else
(init_type)
	_NhlC_OR_F	init_type;
#endif
{
	/* Initialize Resource Mngmt stuff */
	_NrmInitialize();
	_NhlConvertersInitialize(init_type);
	_NhlResourceListInitialize();
	_NhlInitResDatabase();

	/* Initialize Error handling */
	_NhlInitError(init_type);
	_NhlInitRLList();
	_NhlInitGetValues();
	_NhlInitWorkspace();

	return;
}

/*
 * Function:	NhlOpen
 *
 * Description:	Init function for "C" interface.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void NhlOpen
#if	__STDC__
(
	void
)
#else
()
#endif
{
	_NhlOpen(_NhlCLIB);

	return;
}

/*
 * Function:	nhlfopen
 *
 * Description:	init hlu library for use from the "Fortran" bindings.
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
_NHLCALLF(nhl_fopen,NHL_FOPEN)
#if	__STDC__
(
	void
)
#else
()
#endif
{
	_NhlOpen(_NhlFLIB);
	_NhlFortranInit();

	return;
}

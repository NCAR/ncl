/*
 *      $Id: Close.c,v 1.1 1993-04-30 17:21:21 boote Exp $
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
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/ErrorP.h>


/*
 * Function:	NhlClose
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
void NhlClose
#if	__STDC__
(
	void
)
#else
()
#endif
{
	/* free error handling	*/
	_NhlCloseError();
}

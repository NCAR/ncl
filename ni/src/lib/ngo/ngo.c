/*
 *      $Id: ngo.c,v 1.1 1996-10-10 18:55:26 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ngo.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Sep 23 16:20:56 MDT 1996
 *
 *	Description:	
 */
#include <ncarg/hlu/BaseP.h>
#include <ncarg/ngo/ngoP.h>

/*
 * Function:	NgDestroyMeCB
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
void
NgDestroyMeCB
(
	NhlArgVal	cbdata,
	NhlArgVal	udata
)
{
	NhlLayer	l = (NhlLayer)udata.ptrval;

	NhlDestroy(l->base.id);
}

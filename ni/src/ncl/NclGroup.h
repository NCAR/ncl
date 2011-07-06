/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  2009			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Wei Hunag
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Nov 17 16:38:27 MST 2009
 *
 *	Description:	
 */
#ifndef NclGroup_h
#define NclGroup_h

#include "NclData.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"

extern NclGroup *_NclGroupCreate(
       NclObj		/* inst */,
       NclObjClass		/* theclass */,
       NclObjTypes		/* obj_type */,
       unsigned int	/* obj_type_mask */,
       NclStatus		/* status */,
       NclFile		/* file_in */,
       NclQuark		/* group_name */
);

#endif /* NclGroup_h */


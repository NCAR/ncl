/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  2010			*
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
 *	Description:	
 */
#ifndef NclAdvancedGroup_h
#define NclAdvancedGroup_h

#include "NclData.h"
#include "NclFile.h"
#include "NclAdvancedFile.h"
#include "NclFileInterfaces.h"

extern NclAdvancedFile _NclAdvancedGroupCreate(NclObj       inst,
                                    NclObjClass  theclass,
                                    NclObjTypes  obj_type,
                                    unsigned int obj_type_mask,
                                    NclStatus    status,
                                    NclFile      file_in,
                                    NclQuark     group_name);
/*
static void UpdateAdvancedGroupDims(NclAdvancedFile group_out, NclFileGrpNode *grpnode);
*/

#endif /* NclAdvancedGroup_h */


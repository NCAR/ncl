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
#ifndef NclNewGroup_h
#define NclNewGroup_h

#include "NclData.h"
#include "NclFile.h"
#include "NclNewFile.h"
#include "NclFileInterfaces.h"

extern NclGroup *_NclNewGroupCreate(NclObj       inst,
                                    NclObjClass  theclass,
                                    NclObjTypes  obj_type,
                                    unsigned int obj_type_mask,
                                    NclStatus    status,
                                    NclFile      file_in,
                                    NclQuark     group_name);
/*
static void UpdateNewGroupDims(NclNewFile group_out, NclFileGrpNode *grpnode);
*/

#endif /* NclNewGroup_h */


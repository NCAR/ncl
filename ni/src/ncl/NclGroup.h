/*
 *      $Id: NclGroup.h,v 1.2 2010-04-14 21:29:47 huangwei Exp $
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

typedef NclFileClassRec         *NclGroupClass;
typedef struct _NclFileClassRec  NclGroupClassRec;
typedef struct _NclFileClassPart NclGroupClassPart;
typedef struct _NclFilePart      NclGroupPart;

extern NclObjClass nclGroupClass;

extern NclGroupClassRec nclGroupClassRec;

extern NclFile _NclCreateGroup(
#if NhlNeedProto
    NclObj		/* inst */,
    NclObjClass		/* theclass */,
    NclObjTypes		/* obj_type */,
    unsigned int	/* obj_type_mask */,
    NclStatus		/* status */,
    NclFile		/* file_in */,
    NclQuark		/* group_name */
#endif
);

#endif /* NclGroup_h */


/*
 *      $Id: DataItem.h,v 1.3 1994-01-22 01:59:09 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataItem.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jun 24 10:18:42 MDT 1993
 *
 *	Description:	Public declarations of the DataItem class.
 */
#ifndef _NDataItem_h
#define _NDataItem_h
#include <ncarg/hlu/Base.h>
#include <ncarg/hlu/DataMgr.h>

/*
 * These resources are not actually in the DataItem record, but the
 * names should be standardized so the names are defined here.  The
 * reason they are not in the DataItem record is that they don't necessarily
 * apply to all DataItem's.
 */
#define	NhlNdiUserData	"diUserData"
#define	NhlCdiUserData	"DiUserData"

#define	NhlNdiType	"diType"
#define	NhlCdiType	"DiType"

#define	NhlCdiCopyData	"DiCopyData"
#define	NhlCdiMissingValue	"DiMissingValue"

typedef struct _DataItemLayerClassRec *DataItemLayerClass;
typedef struct _DataItemLayerRec *DataItemLayer;

extern LayerClass dataItemLayerClass;

#endif /*_NDataItem_h */

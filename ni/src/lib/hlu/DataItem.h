/*
 *      $Id: DataItem.h,v 1.1 1993-07-12 22:36:08 boote Exp $
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

#define	NhlNUserData	"userData"
#define	NhlCUserData	"UserData"

typedef struct _DataItemLayerClassRec *DataItemLayerClass;
typedef struct _DataItemLayerRec *DataItemLayer;

extern LayerClass dataItemLayerClass;

#endif /*_NDataItem_h */

/*
 *      $Id: DataItem.h,v 1.10 2004-07-23 21:24:55 dbrown Exp $
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
 * This type applies to 2D (or maybe eventually 3D .. nD) coordinate arrays,
 * (superceded by the TransObj type NhlGridType - so this is deprecated now)
 */

typedef enum _NhldiGridType {
	NhlBASICGRID,
	NhlSPHERICALGRID,
	NhlMESHGRID,
	NhlCELLGRID,
	NhlSEAMGRID
} NhldiGridType;

#define NhlTdiGridType "diGridType"
	

/*
 * These resources are not actually in the DataItem record, but the
 * names should be standardized so the names are defined here.  The
 * reason they are not in the DataItem record is that they don't necessarily
 * apply to all DataItem's.
 */
#define	NhlNdiUserData	"diUserData"
#define	NhlCdiUserData	"DiUserData"

#define	NhlCdiCopyData	"DiCopyData"
#define	NhlCdiMissingValue	"DiMissingValue"

extern NhlBoolean NhlIsDataItem(
#if	NhlNeedProto
	int	pid
#endif
);

extern NhlClass NhldataItemClass;

#endif /*_NDataItem_h */

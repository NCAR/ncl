/*
 *      $Id: DataComm.h,v 1.1 1993-07-12 22:36:01 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		DataComm.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 16 17:45:07 MDT 1993
 *
 *	Description:	Public header file for the DataComm class.
 */
#ifndef _NDataComm_h
#define _NDataComm_h
#include <ncarg/hlu/Transform.h>

typedef struct _DataCommLayerClassRec *DataCommLayerClass;
typedef struct _DataCommLayerRec *DataCommLayer;

extern LayerClass dataCommLayerClass;

#endif /*_NDataComm_h */

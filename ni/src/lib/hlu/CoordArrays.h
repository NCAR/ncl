/*
 *      $Id: CoordArrays.h,v 1.2 1994-01-21 19:29:31 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrays.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:34:00 MDT 1993
 *
 *	Description:	Public declarations for CoordArrays object.
 */
#ifndef _NCoordArrays_h
#define _NCoordArrays_h
#include <ncarg/hlu/DataItem.h>

/*
 * New Resource Names
 */

#define	NhlNcaXArray	"caXArray"
#define	NhlCcaXArray	"CaXArray"
#define	NhlNcaYArray	"caYArray"
#define	NhlCcaYArray	"CaYArray"

#define	NhlNcaXCast	"caXCast"
#define	NhlCcaXCast	"CaXCast"
#define	NhlNcaYCast	"caYCast"
#define	NhlCcaYCast	"CaYCast"

#define	NhlNcaCopyArrays	"caCopyArrays"
#define	NhlCcaCopyArrays	"CaCopyArrays"

#define	NhlNcaXMissingF	"caXMissingF"
#define	NhlCcaXMissingF	"CaXMissingF"
#define	NhlNcaYMissingF	"caYMissingF"
#define	NhlCcaYMissingF	"CaYMissingF"
#define	NhlNcaXMaxF	"caXMaxF"
#define	NhlCcaXMaxF	"CaXMaxF"
#define	NhlNcaYMaxF	"caYMaxF"
#define	NhlCcaYMaxF	"CaYMaxF"
#define	NhlNcaXMinF	"caXMinF"
#define	NhlCcaXMinF	"CaXMinF"
#define	NhlNcaYMinF	"caYMinF"
#define	NhlCcaYMinF	"CaYMinF"

#define	NhlNcaXMissing	"caXMissing"
#define	NhlCcaXMissing	"CaXMissing"
#define	NhlNcaYMissing	"caYMissing"
#define	NhlCcaYMissing	"CaYMissing"
#define	NhlNcaXMax	"caXMax"
#define	NhlCcaXMax	"CaXMax"
#define	NhlNcaYMax	"caYMax"
#define	NhlCcaYMax	"CaYMax"
#define	NhlNcaXMin	"caXMin"
#define	NhlCcaXMin	"CaXMin"
#define	NhlNcaYMin	"caYMin"
#define	NhlCcaYMin	"CaYMin"

typedef struct _CoordArraysLayerClassRec *CoordArraysLayerClass;
typedef struct _CoordArraysLayerRec *CoordArraysLayer;

extern LayerClass coordArraysLayerClass;

#endif /*_NCoordArrays_h */

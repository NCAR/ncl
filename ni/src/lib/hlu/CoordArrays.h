/*
 *      $Id: CoordArrays.h,v 1.4 1994-01-27 21:22:09 boote Exp $
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

#define	NhlNcaXMissingF	"caXMissingF"
#define	NhlNcaYMissingF	"caYMissingF"
#define	NhlNcaXMaxF	"caXMaxF"
#define	NhlCcaXMaxF	"CaXMaxF"
#define	NhlNcaYMaxF	"caYMaxF"
#define	NhlCcaYMaxF	"CaYMaxF"
#define	NhlNcaXMinF	"caXMinF"
#define	NhlCcaXMinF	"CaXMinF"
#define	NhlNcaYMinF	"caYMinF"
#define	NhlCcaYMinF	"CaYMinF"

#define	NhlNcaXMissing	"caXMissing"
#define	NhlNcaYMissing	"caYMissing"
#define	NhlNcaXMax	"caXMax"
#define	NhlCcaXMax	"CaXMax"
#define	NhlNcaYMax	"caYMax"
#define	NhlCcaYMax	"CaYMax"
#define	NhlNcaXMin	"caXMin"
#define	NhlCcaXMin	"CaXMin"
#define	NhlNcaYMin	"caYMin"
#define	NhlCcaYMin	"CaYMin"

extern NhlLayerClass NhlcoordArraysLayerClass;

#endif /*_NCoordArrays_h */

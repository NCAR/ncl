/*
 *      $Id: CoordArrays.h,v 1.5 1994-07-12 20:51:43 boote Exp $
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

#define	NhlNcaXMissingV	"caXMissingV"
#define	NhlNcaYMissingV	"caYMissingV"
#define	NhlNcaXMaxV	"caXMaxV"
#define	NhlCcaXMaxV	"CaXMaxV"
#define	NhlNcaYMaxV	"caYMaxV"
#define	NhlCcaYMaxV	"CaYMaxV"
#define	NhlNcaXMinV	"caXMinV"
#define	NhlCcaXMinV	"CaXMinV"
#define	NhlNcaYMinV	"caYMinV"
#define	NhlCcaYMinV	"CaYMinV"

extern NhlLayerClass NhlcoordArraysLayerClass;

#endif /*_NCoordArrays_h */

/*
 *      $Id: CoordArrays.h,v 1.9 1995-04-07 10:41:21 boote Exp $
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
#define	NhlNcaYCast	"caYCast"
#define	NhlCcaCast	"CaCast"

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

#define	NhlTcaCastMode	"caCastMode"

typedef enum _NhlcaCastMode {
	NhlSINGLEVECTOR = 1,
	NhlMULTIPLEVECTORS = 2,
	NhlSPLITVECTORS = 3
} NhlcaCastMode;

extern NhlClass NhlcoordArraysClass;

#endif /*_NCoordArrays_h */

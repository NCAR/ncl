/*
 *      $Id: ScalarField.h,v 1.1 1994-04-29 21:31:29 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ScalarField.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Apr  6 17:49:06 MDT 1994
 *
 *	Description:	Public declarations for ScalarField object.
 */
#ifndef _NScalarField_h
#define _NScalarField_h
#include <ncarg/hlu/DataItem.h>

/*
 * Instance Resources
 */

#define NhlNsfDataArray		"sfDataArray"
#define	NhlNsfXArray		"sfXArray"
#define	NhlNsfYArray		"sfYArray"
#define	NhlNsfCopyData		"sfCopyData"
#define NhlNsfDataOrder		"sfDataOrder"

#define	NhlNsfMissingValueV	"sfMissingValueV"
#define	NhlNsfDataMinV		"sfDataMinV"
#define	NhlNsfDataMaxV		"sfDataMaxV"
#define	NhlNsfXMinV		"sfXMinV"
#define	NhlNsfXMaxV		"sfXMaxV"
#define	NhlNsfYMinV		"sfYMinV"
#define	NhlNsfYMaxV		"sfYMaxV"
#define	NhlNsfXSubsetMinV	"sfXSubsetMinV"
#define	NhlNsfXSubsetMaxV	"sfXSubsetMaxV"
#define	NhlNsfYSubsetMinV	"sfYSubsetMinV"
#define	NhlNsfYSubsetMaxV	"sfYSubsetMaxV"
#define	NhlNsfXIndexMin		"sfXIndexMin"
#define	NhlNsfXIndexMax		"sfXIndexMax"
#define	NhlNsfYIndexMin		"sfYIndexMin"
#define	NhlNsfYIndexMax		"sfYIndexMax"
#define	NhlNsfXStride		"sfXStride"
#define	NhlNsfYStride		"sfYStride"

/*
 * Class Resources
 */

#define NhlCsfDataArray		"SfDataArray"
#define	NhlCsfXArray		"SfXArray"
#define	NhlCsfYArray		"SfYArray"
#define	NhlCsfCopyData		"SfCopyData"
#define NhlCsfDataOrder		"SfDataOrder"

#define	NhlCsfMissingValueV	"SfMissingValueV"
#define	NhlCsfDataMinV		"SfDataMinV"
#define	NhlCsfDataMaxV		"SfDataMaxV"
#define	NhlCsfXMinV		"SfXMinV"
#define	NhlCsfXMaxV		"SfXMaxV"
#define	NhlCsfYMinV		"SfYMinV"
#define	NhlCsfYMaxV		"SfYMaxV"
#define	NhlCsfXSubsetMinV	"SfXSubsetMinV"
#define	NhlCsfXSubsetMaxV	"SfXSubsetMaxV"
#define	NhlCsfYSubsetMinV	"SfYSubsetMinV"
#define	NhlCsfYSubsetMaxV	"SfYSubsetMaxV"
#define	NhlCsfXIndexMin		"SfXIndexMin"
#define	NhlCsfXIndexMax		"SfXIndexMax"
#define	NhlCsfYIndexMin		"SfYIndexMin"
#define	NhlCsfYIndexMax		"SfYIndexMax"
#define	NhlCsfXStride		"SfXStride"
#define	NhlCsfYStride		"SfYStride"

extern NhlLayerClass NhlscalarFieldLayerClass;

#endif /*_NScalarField_h */

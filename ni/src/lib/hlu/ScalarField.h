/*
 *      $Id: ScalarField.h,v 1.6 2002-07-02 01:26:40 dbrown Exp $
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
#define	NhlNsfGridType		"sfGridType"
#define	NhlNsfSubsetByIndex	"sfSubsetByIndex"
#define	NhlNsfCopyData		"sfCopyData"
#define NhlNsfExchangeDimensions	"sfExchangeDimensions"

#define	NhlNsfMissingValueV	"sfMissingValueV"
#define	NhlNsfDataMinV		"sfDataMinV"
#define	NhlNsfDataMaxV		"sfDataMaxV"
#define	NhlNsfXCStartV		"sfXCStartV"
#define	NhlNsfXCEndV		"sfXCEndV"
#define	NhlNsfYCStartV		"sfYCStartV"
#define	NhlNsfYCEndV		"sfYCEndV"
#define	NhlNsfXCStartSubsetV	"sfXCStartSubsetV"
#define	NhlNsfXCEndSubsetV	"sfXCEndSubsetV"
#define	NhlNsfYCStartSubsetV	"sfYCStartSubsetV"
#define	NhlNsfYCEndSubsetV	"sfYCEndSubsetV"
#define	NhlNsfXCStartIndex	"sfXCStartIndex"
#define	NhlNsfXCEndIndex	"sfXCEndIndex"
#define	NhlNsfYCStartIndex	"sfYCStartIndex"
#define	NhlNsfYCEndIndex	"sfYCEndIndex"
#define	NhlNsfXCStride		"sfXCStride"
#define	NhlNsfYCStride		"sfYCStride"
#define	NhlNsfXCActualStartF	"sfXCActualStartF"	/* read-only */
#define	NhlNsfXCActualEndF	"sfXCActualEndF"	/* read-only */
#define	NhlNsfXCElementCount	"sfXCElementCount"	/* read-only */
#define	NhlNsfYCActualStartF	"sfYCActualStartF"	/* read-only */
#define	NhlNsfYCActualEndF	"sfYCActualEndF"	/* read-only */
#define	NhlNsfYCElementCount	"sfYCElementCount"	/* read-only */

/*
 * Class Resources
 */

#define NhlCsfDataArray		"SfDataArray"
#define	NhlCsfXArray		"SfXArray"
#define	NhlCsfYArray		"SfYArray"
#define	NhlCsfGridType		"SfGridType"
#define	NhlCsfSubsetByIndex	"SfSubsetByIndex"
#define	NhlCsfCopyData		"SfCopyData"
#define NhlCsfExchangeDimensions	"SfExchangeDimensions"

#define	NhlCsfMissingValueV	"SfMissingValueV"
#define	NhlCsfDataMinV		"SfDataMinV"
#define	NhlCsfDataMaxV		"SfDataMaxV"
#define	NhlCsfXCStartV		"SfXCStartV"
#define	NhlCsfXCEndV		"SfXCEndV"
#define	NhlCsfYCStartV		"SfYCStartV"
#define	NhlCsfYCEndV		"SfYCEndV"
#define	NhlCsfXCStartSubsetV	"SfXCStartSubsetV"
#define	NhlCsfXCEndSubsetV	"SfXCEndSubsetV"
#define	NhlCsfYCStartSubsetV	"SfYCStartSubsetV"
#define	NhlCsfYCEndSubsetV	"SfYCEndSubsetV"
#define	NhlCsfXCStartIndex	"SfXCStartIndex"
#define	NhlCsfXCEndIndex	"SfXCEndIndex"
#define	NhlCsfYCStartIndex	"SfYCStartIndex"
#define	NhlCsfYCEndIndex	"SfYCEndIndex"
#define	NhlCsfXCStride		"SfXCStride"
#define	NhlCsfYCStride		"SfYCStride"
#define	NhlCsfXCActualStartF	"SfXCActualStartF"
#define	NhlCsfXCActualEndF	"SfXCActualEndF"
#define	NhlCsfXCElementCount	"SfXCElementCount"
#define	NhlCsfYCActualStartF	"SfYCActualStartF"
#define	NhlCsfYCActualEndF	"SfYCActualEndF"
#define	NhlCsfYCElementCount	"SfYCElementCount"

extern NhlClass NhlscalarFieldClass;

#endif /*_NScalarField_h */

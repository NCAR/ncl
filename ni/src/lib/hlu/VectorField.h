/*
 *      $Id: VectorField.h,v 1.3 2002-07-03 01:09:57 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		VectorField.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 28 11:47:36 MDT 1995
 *
 *	Description:	Public declarations for VectorField object.
 */
#ifndef _NVectorField_h
#define _NVectorField_h
#include <ncarg/hlu/DataItem.h>

/*
 * Instance Resources
 */

#define NhlNvfDataArray		"vfDataArray"
#define NhlNvfUDataArray	"vfUDataArray"
#define NhlNvfVDataArray	"vfVDataArray"
#define	NhlNvfXArray		"vfXArray"
#define	NhlNvfYArray		"vfYArray"
#define NhlNvfGridType          "vfGridType"
#define NhlNvfPolarData		"vfPolarData"
#define	NhlNvfSubsetByIndex	"vfSubsetByIndex"
#define	NhlNvfCopyData		"vfCopyData"
#define NhlNvfExchangeDimensions	"vfExchangeDimensions"
#define NhlNvfExchangeUVData	"vfExchangeUVData"

#define NhlNvfSingleMissingValue	"vfSingleMissingValue"
#define	NhlNvfMissingUValueV	"vfMissingUValueV"
#define	NhlNvfMissingVValueV	"vfMissingVValueV"
#define	NhlNvfMagMinV		"vfMagMinV"
#define	NhlNvfMagMaxV		"vfMagMaxV"
#define	NhlNvfUMinV		"vfUMinV"
#define	NhlNvfUMaxV		"vfUMaxV"
#define	NhlNvfVMinV		"vfVMinV"
#define	NhlNvfVMaxV		"vfVMaxV"
#define	NhlNvfXCStartV		"vfXCStartV"
#define	NhlNvfXCEndV		"vfXCEndV"
#define	NhlNvfYCStartV		"vfYCStartV"
#define	NhlNvfYCEndV		"vfYCEndV"
#define	NhlNvfXCStartSubsetV	"vfXCStartSubsetV"
#define	NhlNvfXCEndSubsetV	"vfXCEndSubsetV"
#define	NhlNvfYCStartSubsetV	"vfYCStartSubsetV"
#define	NhlNvfYCEndSubsetV	"vfYCEndSubsetV"
#define	NhlNvfXCStartIndex	"vfXCStartIndex"
#define	NhlNvfXCEndIndex	"vfXCEndIndex"
#define	NhlNvfYCStartIndex	"vfYCStartIndex"
#define	NhlNvfYCEndIndex	"vfYCEndIndex"
#define	NhlNvfXCStride		"vfXCStride"
#define	NhlNvfYCStride		"vfYCStride"
#define	NhlNvfXCActualStartF	"vfXCActualStartF"	/* read-only */
#define	NhlNvfXCActualEndF	"vfXCActualEndF"	/* read-only */
#define	NhlNvfXCElementCount	"vfXCElementCount"	/* read-only */
#define	NhlNvfYCActualStartF	"vfYCActualStartF"	/* read-only */
#define	NhlNvfYCActualEndF	"vfYCActualEndF"	/* read-only */
#define	NhlNvfYCElementCount	"vfYCElementCount"	/* read-only */

/*
 * Class Resources
 */

#define NhlCvfDataArray		"VfDataArray"
#define NhlCvfUDataArray	"VfUDataArray"
#define NhlCvfVDataArray	"VfVDataArray"
#define	NhlCvfXArray		"VfXArray"
#define	NhlCvfYArray		"VfYArray"
#define NhlCvfGridType          "VfGridType"
#define NhlCvfPolarData		"VfPolarData"
#define	NhlCvfSubsetByIndex	"VfSubsetByIndex"
#define	NhlCvfCopyData		"VfCopyData"
#define NhlCvfExchangeDimensions	"VfExchangeDimensions"
#define NhlCvfExchangeUVData	"VfExchangeUVData"

#define NhlCvfSingleMissingValue	"VfSingleMissingValue"
#define	NhlCvfMissingUValueV	"VfMissingUValueV"
#define	NhlCvfMissingVValueV	"VfMissingVValueV"
#define	NhlCvfMagMinV		"VfMagMinV"
#define	NhlCvfMagMaxV		"VfMagMaxV"
#define	NhlCvfUMinV		"VfUMinV"
#define	NhlCvfUMaxV		"VfUMaxV"
#define	NhlCvfVMinV		"VfVMinV"
#define	NhlCvfVMaxV		"VfVMaxV"
#define	NhlCvfXCStartV		"VfXCStartV"
#define	NhlCvfXCEndV		"VfXCEndV"
#define	NhlCvfYCStartV		"VfYCStartV"
#define	NhlCvfYCEndV		"VfYCEndV"
#define	NhlCvfXCStartSubsetV	"VfXCStartSubsetV"
#define	NhlCvfXCEndSubsetV	"VfXCEndSubsetV"
#define	NhlCvfYCStartSubsetV	"VfYCStartSubsetV"
#define	NhlCvfYCEndSubsetV	"VfYCEndSubsetV"
#define	NhlCvfXCStartIndex	"VfXCStartIndex"
#define	NhlCvfXCEndIndex	"VfXCEndIndex"
#define	NhlCvfYCStartIndex	"VfYCStartIndex"
#define	NhlCvfYCEndIndex	"VfYCEndIndex"
#define	NhlCvfXCStride		"VfXCStride"
#define	NhlCvfYCStride		"VfYCStride"
#define	NhlCvfXCActualStartF	"VfXCActualStartF"
#define	NhlCvfXCActualEndF	"VfXCActualEndF"
#define	NhlCvfXCElementCount	"VfXCElementCount"
#define	NhlCvfYCActualStartF	"VfYCActualStartF"
#define	NhlCvfYCActualEndF	"VfYCActualEndF"
#define	NhlCvfYCElementCount	"VfYCElementCount"

extern NhlClass NhlvectorFieldClass;

#endif /*_NVectorField_h */

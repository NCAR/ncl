/*
 *      $Id: CoordArrTable.h,v 1.7 1995-04-07 10:41:13 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CoordArrTable.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 28 11:34:00 MDT 1993
 *
 *	Description:	Public declarations for CoordArrTable object.
 */
#ifndef _NCoordArrTable_h
#define _NCoordArrTable_h
#include <ncarg/hlu/DataItem.h>

#define	NhlNctXTableType	"ctXTableType"
#define	NhlCctXTableType	"CtXTableType"
#define	NhlNctYTableType	"ctYTableType"
#define	NhlCctYTableType	"CtYTableType"
#define	NhlNctXElementSize	"ctXElementSize"
#define	NhlCctXElementSize	"CtXElementSize"
#define	NhlNctYElementSize	"ctYElementSize"
#define	NhlCctYElementSize	"CtYElementSize"
#define	NhlNctXTable		"ctXTable"
#define	NhlCctXTable		"CtXTable"
#define	NhlNctYTable		"ctYTable"
#define	NhlCctYTable		"CtYTable"
#define	NhlNctXTableLengths	"ctXTableLengths"
#define	NhlCctXTableLengths	"CtXTableLengths"
#define	NhlNctYTableLengths	"ctYTableLengths"
#define	NhlCctYTableLengths	"CtYTableLengths"

#define	NhlNctCopyTables	"ctCopyTables"

#define	NhlNctXMissingV		"ctXMissingV"
#define	NhlNctYMissingV		"ctYMissingV"
#define	NhlNctXMaxV		"ctXMaxV"
#define	NhlCctXMaxV		"CtXMaxV"
#define	NhlNctYMaxV		"ctYMaxV"
#define	NhlCctYMaxV		"CtYMaxV"
#define	NhlNctXMinV		"ctXMinV"
#define	NhlCctXMinV		"CtXMinV"
#define	NhlNctYMinV		"ctYMinV"
#define	NhlCctYMinV		"CtYMinV"

extern NhlClass NhlcoordArrTableClass;

#endif /*_NCoordArrTable_h */

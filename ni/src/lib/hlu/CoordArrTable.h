/*
 *      $Id: CoordArrTable.h,v 1.3 1994-01-14 23:36:05 boote Exp $
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

#define	NhlNctXTable	"ctXTable"
#define	NhlCctXTable	"CtXTable"
#define	NhlNctYTable	"ctYTable"
#define	NhlCctYTable	"CtYTable"
#define	NhlNctXTableLengths	"ctXTableLengths"
#define	NhlCctXTableLengths	"CtXTableLengths"
#define	NhlNctYTableLengths	"ctYTableLengths"
#define	NhlCctYTableLengths	"CtYTableLengths"

#define	NhlNctCopyTables	"ctCopyTables"
#define	NhlCctCopyTables	"CtCopyTables"

#define	NhlNctXMissingF	"ctXMissingF"
#define	NhlCctXMissingF	"CtXMissingF"
#define	NhlNctYMissingF	"ctYMissingF"
#define	NhlCctYMissingF	"CtYMissingF"
#define	NhlNctXMaxF	"ctXMaxF"
#define	NhlCctXMaxF	"CtXMaxF"
#define	NhlNctYMaxF	"ctYMaxF"
#define	NhlCctYMaxF	"CtYMaxF"
#define	NhlNctXMinF	"ctXMinF"
#define	NhlCctXMinF	"CtXMinF"
#define	NhlNctYMinF	"ctYMinF"
#define	NhlCctYMinF	"CtYMinF"

#define	NhlNctXMissing	"ctXMissing"
#define	NhlCctXMissing	"CtXMissing"
#define	NhlNctYMissing	"ctYMissing"
#define	NhlCctYMissing	"CtYMissing"
#define	NhlNctXMax	"ctXMax"
#define	NhlCctXMax	"CtXMax"
#define	NhlNctYMax	"ctYMax"
#define	NhlCctYMax	"CtYMax"
#define	NhlNctXMin	"ctXMin"
#define	NhlCctXMin	"CtXMin"
#define	NhlNctYMin	"ctYMin"
#define	NhlCctYMin	"CtYMin"

typedef struct _CoordArrTableLayerClassRec *CoordArrTableLayerClass;
typedef struct _CoordArrTableLayerRec *CoordArrTableLayer;

extern LayerClass coordArrTableLayerClass;

#endif /*_NCoordArrTable_h */

/*
 *      $Id: MeshScalarField.h,v 1.1 2004-07-23 21:24:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		MeshScalarField.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 23 18:48:07 MDT 2004
 *
 *	Description:	Public declarations for MeshScalarField object.
 */
#ifndef _NMeshScalarField_h
#define _NMeshScalarField_h
#include <ncarg/hlu/ScalarField.h>

#define NhlNsfElementNodes 	"sfElementNodes"
#define NhlNsfNodeIndexes	"sfNodeIndexes"
#define NhlNsfFirstNodeIndex    "sfFirstNodeIndex"
#define NhlNsfXCellBounds       "sfXCellBounds"
#define NhlNsfYCellBounds       "sfYCellBounds"

#define NhlCsfElementNodes 	"SfElementNodes"
#define NhlCsfNodeIndexes	"SfNodeIndexes"
#define NhlCsfFirstNodeIndex    "SfFirstNodeIndex"
#define NhlCsfXCellBounds       "SfXCellBounds"
#define NhlCsfYCellBounds       "SfYCellBounds"

extern NhlClass NhlmeshScalarFieldClass;

#endif /*_NMeshScalarField_h */

/*
 *      $Id: TransObj.h,v 1.16 2004-01-23 22:46:53 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		TransObj.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Oct 16 10:48:21 MDT 1992
 *
 *	Description:	This is the public header file for the TransObj class.
 *			The TransObjClass is responsible for managing 
 *			transformations DATA==>VIEWPORT definitions.
 */
#ifndef _NTransObj_h
#define  _NTransObj_h

#include <ncarg/hlu/Base.h>

typedef enum _NhlAxisType { 
	NhlIRREGULARAXIS, 
	NhlLINEARAXIS, 
	NhlLOGAXIS 
} NhlAxisType;

#define NhlTAxisType "AxisType"

typedef enum _NhlGridType {
	NhltrMAP = 0,            /* Map */
	NhltrLOGLIN = 1,         /* LogLin */
	NhltrIRREGULAR = 2,      /* Irregular */
	NhltrCURVILINEAR = 3,    /* Curvilinear */
	NhltrSPHERICAL = 4,       /* Spherical */
	NhltrTRIANGULARMESH = 5   /* TriangularMesh */
} NhlGridType;

#define NhlTGridType "GridType"


#define NhlNtrXMinF		"trXMinF"
#define NhlCtrXMinF		"TrXMinF"
#define NhlNtrXMaxF		"trXMaxF"
#define NhlCtrXMaxF		"TrXMaxF"
#define NhlNtrXReverse		"trXReverse"
#define NhlCtrXReverse		"TrXReverse"

#define NhlNtrYMinF		"trYMinF"
#define NhlCtrYMinF		"TrYMinF"
#define NhlNtrYMaxF		"trYMaxF"
#define NhlCtrYMaxF		"TrYMaxF"
#define NhlNtrYReverse		"trYReverse"
#define NhlCtrYReverse		"TrYReverse"

#define NhlNtrOutOfRangeF	"trOutOfRangeF"
#define NhlCtrOutOfRangeF	"TrOutOfRangeF"
#define NhlNtrResolutionF	"trResolutionF"
#define NhlCtrResolutionF	"TrResolutionF"

#define NhlNtrLineInterpolationOn 	"trLineInterpolationOn"
#define NhlCtrLineInterpolationOn 	"TrLineInterpolationOn"

#define NhlNtrGridType			"trGridType"
#define NhlCtrGridType			"TrGridType"

/*
 * both Irregular and Curvilinear define these array resources
 */

#define NhlNtrXCoordPoints	"trXCoordPoints"
#define NhlCtrXCoordPoints	"TrXCoordPoints"
#define NhlNtrYCoordPoints	"trYCoordPoints"
#define NhlCtrYCoordPoints	"TrYCoordPoints"

extern NhlClass NhltransObjClass;


#endif  /*_NTransObj_h*/

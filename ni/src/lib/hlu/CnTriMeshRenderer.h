/*
 *      $Id: CnTriMeshRenderer.h,v 1.2 2004-07-23 21:24:54 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		CnTriMeshRenderer.h
 *
 *	Author:		David Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Sep 26 15:10:39 MDT 2003
 *
 *	Description:	Public header for CnTriMeshRenderer class.
 */

#ifndef _NCNTRIMESHRENDERER_h
#define _NCNTRIMESHRENDERER_h

#include <ncarg/hlu/CnRenderer.h>

extern NhlClass			NhlcnTriMeshRendererClass;

/* update modes */

#define TRIMESH_NOUPDATE 0
#define TRIMESH_DATAUPDATE 1
#define TRIMESH_NEWMESH 2

#define NhlNtriMeshUpdateMode   "triMesh.UpdateMode"
#define NhlCtriMeshUpdateMode   "TriMesh.UpdateMode"



#endif /*_NCNTRIMESHRENDERER_h */

/*
 *      $Id: createmenus.h,v 1.3 1997-07-23 22:23:35 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		createmenus.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Mar 17 20:52:04 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_CREATEMENUS_H
#define	_NG_CREATEMENUS_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

/*
 * Public api
 */

#define NgLineContour   "Line Contour"
#define NgFillContour   "Fill Contour"
#define NgRasterContour "Raster Contour"
#define NgInterpolatedRasterContour "Interpolated Raster Contour"
#define NgStreamline    "Streamline"
#define NgLineVector    "Line Vector"
#define NgFillVector    "Fill Vector"
#define NgLineXy        "Line Xy"
#define NgScatterXy     "Scatter Xy"
#define NgCoordArray    "CoordArray"
#define NgScalarField   "ScalarField"
#define NgVectorField   "VectorField"
#define NgNclVariable   "NclVariable"

typedef enum _NgDataSinkType {
        ngLINECONTOUR,
        ngFILLCONTOUR,
        ngRASTERCONTOUR,
        ngINTERPOLATEDRASTERCONTOUR,
        ngSTREAMLINE,
        ngLINEVECTOR,
        ngFILLVECTOR,
        ngLINEXY,
        ngSCATTERXY,
        ngCOORDARRAY,
        ngSCALARFIELD,
        ngVECTORFIELD,
        ngNCLVARIABLE
} NgDataSinkType;

typedef struct _NgDataSinkRec
{
        NhlString	name;
        NgDataSinkType	type;
        NhlString	def_name;
        NhlString	class_name;
        int		n_dataitems;
        int		n_datadims[8];
        NhlString	data_names[8];
        NrmQuark	data_resnames[8];
        int		coord_ix[3];
} NgDataSinkRec;
        
typedef struct _NgCreateMenus
{
        Widget		menubar;
        Widget		plot_mbutton;
        Widget		var_mbutton;
        Widget		data_mbutton;
        NrmQuark		qsymbol;
        NclApiVarInfoRec	*vinfo;
	long			*start;
	long			*finish;
	long			*stride;
        PageOutputNotify	output_notify;
        NhlPointer		pdata;
} NgCreateMenus;

NgCreateMenus *
NgCreateCreateMenus(
        NgGO		go,
        Widget		parent
        );

NhlErrorTypes
NgUpdateCreateMenus
(
        NgCreateMenus		*create_menus
        );
        
void
NgDestroyCreateMenus
(
        NgCreateMenus		*create_menus
        );
        

#endif	/* _NG_CREATEMENUS_H */

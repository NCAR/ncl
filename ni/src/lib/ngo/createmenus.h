/*
 *      $Id: createmenus.h,v 1.1 1997-06-20 16:38:14 dbrown Exp $
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
#define NgStreamline    "Streamline"
#define NgLineVector    "Line Vector"
#define NgFillVector    "Fill Vector"
#define NgLineXy        "Line Xy"
#define NgScatterXy     "Scatter Xy"

typedef enum __NgPlotType {
        ngLINECONTOUR,
        ngFILLCONTOUR,
        ngRASTERCONTOUR,
        ngSTREAMLINE,
        ngLINEVECTOR,
        ngFILLVECTOR,
        ngLINEXY,
        ngSCATTERXY
} _NgPlotType;

#define NgCoordArray    "CoordArray"
#define NgScalarField   "ScalarField"
#define NgVectorField   "VectorField"
#define NgNclVariable   "NclVariable"

typedef enum __NgDataItemType {
        ngCOORDARRAY = 1 + (int) ngSCATTERXY,
        ngSCALARFIELD,
        ngVECTORFIELD,
        ngNCLVARIABLE
} _NgDataItemType;

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

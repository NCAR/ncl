/*
 *      $Id: AddHLUObjs.c,v 1.24 2010-01-07 23:05:10 brownrig Exp $
 */
/************************************************************************
 *                                                                       *
 *			     Copyright (C)  1994                                    *
 *	     University Corporation for Atmospheric Research                *
 *			     All Rights Reserved                                    *
 *									                                    *
 ************************************************************************/
/*
 *	File:		AddHLUObjs.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 7 11:07:41 MST 1994
 *
 *	Description:
 */

#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/BaseP.h>
#include "HluClasses.h"

    extern void _NclAddSingleObj(
#if	NhlNeedProto
            char * /*name*/,
            struct _NhlClassRec * /* the_ptr */
#endif
            );

    void _NclAddHLUObjs
#if	NhlNeedProto
    (void)
#else
    ()
#endif
    {
        _NclAddSingleObj(NhltickMarkClass->base_class.class_name, NhltickMarkClass);
        _NclAddSingleObj(NhltitleClass->base_class.class_name, NhltitleClass);
        /* Note that we are mapping the original X11 driver's name into the cairo windowWorkstation -- RLB 2/2012 */
        _NclAddSingleObj(NhlxWorkstationClass->base_class.class_name, NhlcairoWindowWorkstationClass);
        _NclAddSingleObj(NhlimageWorkstationClass->base_class.class_name, NhlimageWorkstationClass);
        _NclAddSingleObj(NhlncgmWorkstationClass->base_class.class_name, NhlncgmWorkstationClass);
        _NclAddSingleObj(NhlcontourPlotClass->base_class.class_name, NhlcontourPlotClass);
        _NclAddSingleObj(NhltextItemClass->base_class.class_name, NhltextItemClass);
        _NclAddSingleObj(NhlxyPlotClass->base_class.class_name, NhlxyPlotClass);
        _NclAddSingleObj(NhllabelBarClass->base_class.class_name, NhllabelBarClass);
        _NclAddSingleObj(NhllegendClass->base_class.class_name, NhllegendClass);
        _NclAddSingleObj(NhlcoordArraysClass->base_class.class_name, NhlcoordArraysClass);
        _NclAddSingleObj(NhlscalarFieldClass->base_class.class_name, NhlscalarFieldClass);
        _NclAddSingleObj(NhlmeshScalarFieldClass->base_class.class_name, NhlmeshScalarFieldClass);
        _NclAddSingleObj(NhlmapPlotClass->base_class.class_name, NhlmapPlotClass);
        _NclAddSingleObj(NhlappClass->base_class.class_name, NhlappClass);
        _NclAddSingleObj(NhlannoManagerClass->base_class.class_name, NhlannoManagerClass);
        _NclAddSingleObj(NhlpsWorkstationClass->base_class.class_name, NhlpsWorkstationClass);
        _NclAddSingleObj(NhlpdfWorkstationClass->base_class.class_name, NhlpdfWorkstationClass);
        _NclAddSingleObj(NhllogLinPlotClass->base_class.class_name, NhllogLinPlotClass);
        _NclAddSingleObj(NhlirregularPlotClass->base_class.class_name, NhlirregularPlotClass);
        _NclAddSingleObj(NhlvectorPlotClass->base_class.class_name, NhlvectorPlotClass);
        _NclAddSingleObj(NhlvectorFieldClass->base_class.class_name, NhlvectorFieldClass);
        _NclAddSingleObj(NhlstreamlinePlotClass->base_class.class_name, NhlstreamlinePlotClass);
        _NclAddSingleObj(NhlgraphicStyleClass->base_class.class_name, NhlgraphicStyleClass);
        _NclAddSingleObj(NhlprimitiveClass->base_class.class_name, NhlprimitiveClass);
        _NclAddSingleObj(NhlcairoDocumentWorkstationClass->base_class.class_name, NhlcairoDocumentWorkstationClass);
        _NclAddSingleObj(NhlcairoImageWorkstationClass->base_class.class_name, NhlcairoImageWorkstationClass);
        _NclAddSingleObj(NhlcairoWindowWorkstationClass->base_class.class_name, NhlcairoWindowWorkstationClass);
        return;

    }

#ifdef __cpluplus
}
#endif

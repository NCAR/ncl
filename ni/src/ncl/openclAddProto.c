/*
 *      $Id: openclAddProto.c,v 1.13 2010-02-01 22:28:48 huangwei Exp $
 */
/************************************************************************
*                                                                   *
*            Copyright (C)  2009                                    *
*       University Corporation for Atmospheric Research             *
*           All Rights Reserved                                     *
*                                                                   *
************************************************************************/
/*
 *    File:		openclAddProto.c
 *
 *    Author:		Wei Huang
 *    		National Center for Atmospheric Research
 *    		PO 3000, Boulder, Colorado
 *
 *    Date:		Fri Aug 31 10:07:50 MDT 2012
 *
 *    Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclBuiltIns.h"
#include "MathFuncs.h"
#include "HLUFunctions.h"

extern NhlErrorTypes _Ncldim_avg_n_cl(void);

void NclAddOpenCLBuiltInFuncs(void)
{
    void *args;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    int nargs = 0;
    
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args,nargs,"numeric",0,NclANY); nargs++;
    SetArgTemplate(args,nargs,NclANY,1,NclANY); nargs++;
    NclRegisterFunc( _Ncldim_avg_n_cl,args,"dim_avg_n_cl",nargs);

    return;
}

#ifdef __cplusplus
}
#endif


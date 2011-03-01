/*
 *      $Id: javaAddProto.c,v 1.2 2009-12-04 15:23:07 huangwei Exp $
 */
/************************************************************************
*                                                                   *
*            Copyright (C)  2009                                    *
*       University Corporation for Atmospheric Research             *
*           All Rights Reserved                                     *
*                                                                   *
************************************************************************/
/*
 *    File:		javaAddProto.c
 *
 *    Author:		Wei Huang
 *    		National Center for Atmospheric Research
 *    		PO 3000, Boulder, Colorado
 *
 *    Date:		Fri March 20 11:24:07 MDT 2009
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

extern NhlErrorTypes _NclIgenerateVarList(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIgenerateVarRange(
#if NhlNeedProto
void
#endif
);

void NclAddJavaBuiltInFuncs
#if     NhlNeedProto
(void)
#else
()
#endif
{
    void *args;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    int nargs = 0;

    nargs = 0;
    args = NewArgs(1);
    dimsizes[0] = 1;
    SetArgTemplate(args, nargs, "file", 0, NclANY);  nargs++;
    NclRegisterFunc(_NclIgenerateVarList, args, "generateVarList", nargs);

    nargs = 0;
    args = NewArgs(1);
    SetArgTemplate(args,nargs,NclANY,0,NclANY);nargs++;
    NclRegisterProc(_NclIgenerateVarRange,args,"generateVarRange",nargs);

    return;
}

#ifdef __cplusplus
}
#endif


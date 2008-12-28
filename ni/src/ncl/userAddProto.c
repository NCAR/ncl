/*
 *      $Id: userAddProto.c,v 1.2 2008-12-28 17:13:47 huangwei Exp $
 */
/************************************************************************
*                                                                   *
*            Copyright (C)  1993                                    *
*       University Corporation for Atmospheric Research             *
*           All Rights Reserved                                     *
*                                                                   *
************************************************************************/
/*
 *    File:		AddBuiltIns.c
 *
 *    Author:		Ethan Alpert
 *    		National Center for Atmospheric Research
 *    		PO 3000, Boulder, Colorado
 *
 *    Date:		Fri Oct 15 10:43:07 MDT 1993
 *
 *    Description:	
 */
#ifdef __cpluplus
extern "C" {
#endif

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "NclBuiltIns.h"
#include "MathFuncs.h"
#include "HLUFunctions.h"

extern NhlErrorTypes _NclgetColsInString(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclgetArraySubString(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclgetColsFromString(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclremoveCharFromString(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclswitchCharInString(
#if NhlNeedProto
void
#endif
);

void NclAddUserBuiltInFuncs
#if     NhlNeedProto
(void)
#else
()
#endif
{
    void *args;
    int dimsizes[NCL_MAX_DIMENSIONS];
    int nargs = 0;

    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, 0, "string", 1, NclANY); nargs++;
    SetArgTemplate(args, 1, "string", 1, NclANY); nargs++;
    NclRegisterFunc(_NclgetColsInString, args, "getColsInString", nargs);

  /*
   * char **getArraySubString(char **arrayString, int rows, int col, const char *delim);
   */

    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, 0, "string", 1, NclANY); nargs++;
    SetArgTemplate(args, 1, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, 2, "string", 1, NclANY); nargs++;
    NclRegisterFunc(_NclgetArraySubString, args, "getArraySubString", nargs);
    
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, 0, "string", 1, NclANY); nargs++;
    SetArgTemplate(args, 1, "numeric", 1, NclANY); nargs++;
    SetArgTemplate(args, 2, "numeric", 1, NclANY); nargs++;
    NclRegisterFunc(_NclgetColsFromString, args, "getColsFromString", nargs);
    
    nargs = 0;
    args = NewArgs(2);
    SetArgTemplate(args, 0, "string", 1, NclANY); nargs++;
    SetArgTemplate(args, 1, "string", 1, NclANY); nargs++;
    NclRegisterFunc(_NclremoveCharFromString, args, "removeCharFromString", nargs);
    
    nargs = 0;
    args = NewArgs(3);
    SetArgTemplate(args, 0, "string", 1, NclANY); nargs++;
    SetArgTemplate(args, 1, "string", 1, NclANY); nargs++;
    SetArgTemplate(args, 2, "string", 1, NclANY); nargs++;
    NclRegisterFunc(_NclswitchCharInString, args, "switchCharInString", nargs);

    return;
}

#ifdef __cpluplus
}
#endif


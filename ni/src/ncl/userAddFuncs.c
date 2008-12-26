/*
 *      $Id: userAddFuncs.c,v 1.1 2008-12-26 15:12:02 huangwei Exp $
 */
/************************************************************************
*                                                                       *
*                   Copyright (C)  1995                                 *
*           University Corporation for Atmospheric Research             *
*                   All Rights Reserved                                 *
*                                                                       *
************************************************************************/
/*
 *	File:		BuiltInFuncs.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 7 12:12:21 MST 1995
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ncarg/c.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/ncargC.h>
#include <ncarg/c.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include "defs.h"
#include <errno.h>
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "parser.h"
#include "OpsList.h"
#include "ApiRecords.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"
#include "FileSupport.h"
#include "NclAtt.h"
#include "NclList.h"
#include "ListSupport.h"
#include "NclFileInterfaces.h"
#include <signal.h>
#include <netcdf.h>

extern int cmd_line;
extern short NCLnoSysPager;
extern char *nclf;

NhlErrorTypes _NclgetColsInString
#if     NhlNeedProto
(void)
#else
()
#endif
{
    string *str;
    string *delim;

    int ndim_str, dimsz_str[NCL_MAX_DIMENSIONS];
    int ndim_delim, dimsz_delim[NCL_MAX_DIMENSIONS];
    int has_missing_str, found_missing_str = 0;
    int has_missing_delim, found_missing_delim = 0;
    NclScalar   missing_str, ret_missing_str;
    NclScalar   missing_delim, ret_missing_delim;
    NclBasicDataTypes type_str, type_delim;
  
    int sz = 0;
    int i;

    char *tmp_str;
    char *tmp_delim;
    char *result = NULL;
    int *cols;
    
    str = (string *) NclGetArgValue(
                        0,
                        2,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        &type_str,
                        DONT_CARE);

  /*
   *printf("ndim_str is <%d>\n", ndim_str);
   *printf("dimsz_str is <%d>\n", dimsz_str[0]);
   *printf("missing_str is <%d>\n", missing_str);
   *printf("has_missing_str is <%d>\n", has_missing_str);
   *printf("type_str is <%d>\n", (int) type_str);
   *printf("str is <%s>\n", (char *)str);
   */

    for (i = 0; i < ndim_str; i++)
        sz += dimsz_str[i];

    if (sz < 1)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

  /*
   *sz = strlen(NrmQuarkToString(str[0]));
   *printf("sz is <%d>\n", sz);
   */

    delim = (string *) NclGetArgValue(
                        1,
                        2,
                        &ndim_delim,
                        dimsz_delim,
                        &missing_delim,
                        &has_missing_delim,
                        &type_delim,
                        DONT_CARE);

  /*
   *printf("ndim_delim is <%d>\n", ndim_delim);
   *printf("dimsz_delim is <%d>\n", dimsz_delim[0]);
   *printf("missing_delim is <%d>\n", missing_delim);
   *printf("has_missing_delim is <%d>\n", has_missing_delim);
   *printf("type_delim is <%d>\n", (int) type_delim);
   *printf("delim is <%s>\n", delim);
   */

    sz = 0;
    for (i = 0; i < ndim_delim; i++)
        sz += dimsz_delim[i];

    if (sz < 1)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    tmp_str = (char *) NrmQuarkToString(str[0]);
  /*
   *printf("tmp_str is <%s>\n", tmp_str);
   */

    tmp_delim = (char *) NrmQuarkToString(delim[0]);
  /*
   *printf("tmp_delim is <%s>\n", tmp_delim);
   */

    cols = NclMalloc((unsigned int) sizeof(int));
    cols[0] = 0;

    if (has_missing_str && found_missing_str)
    {
        ret_missing_str.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) cols, ndim_str, dimsz_str, &ret_missing_str, NCL_int, 0);
    }
    else
    {
        result = strtok(tmp_str, tmp_delim);
        while(result != NULL)
        {
            cols[0] ++;
          /*
           *printf("str[%d] is <%s>\n", cols[0], result);
           */
            result = strtok(NULL, tmp_delim);
        }

        return NclReturnValue((void *) cols, ndim_str, dimsz_str, NULL, NCL_int, 0);
    }

    NclFree(cols);
}


NhlErrorTypes _NclgetArraySubString
#if     NhlNeedProto
(void)
#else
()
#endif
{
    string *strs;
    string *delim;

    int ndim_strs, dimsz_strs[NCL_MAX_DIMENSIONS];
    int ndim_rows, dimsz_rows[NCL_MAX_DIMENSIONS];
    int ndim_cols, dimsz_cols[NCL_MAX_DIMENSIONS];
    int ndim_delim, dimsz_delim[NCL_MAX_DIMENSIONS];
    int has_missing_strs, found_missing_strs = 0;
    int has_missing_rows, found_missing_rows = 0;
    int has_missing_cols, found_missing_cols = 0;
    int has_missing_delim, found_missing_delim = 0;
    NclScalar   missing_strs, ret_missing_strs;
    NclScalar   missing_rows, ret_missing_rows;
    NclScalar   missing_cols, ret_missing_cols;
    NclScalar   missing_delim, ret_missing_delim;
    NclBasicDataTypes type_strs, type_rows, type_cols, type_delim;
  
    int i, n;

    char *tmp_str;
    char *tmp_delim;
    char *result = NULL;
    string *arraySubString;
    int *rows;
    int *cols;
    
    strs = (string *) NclGetArgValue(
                        0,
                        4,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        &type_strs,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, errno, "getArraySubString: memory allocation error.");
        return NhlFATAL;
    }

  /*
    printf("\n\n\n\n");
    printf("ndim_strs is <%d>\n", ndim_strs);
    printf("dimsz_strs is <%d>\n", dimsz_strs[0]);
   *printf("missing_strs is <%d>\n", missing_strs);
   *printf("has_missing_strs is <%d>\n", has_missing_strs);
   *printf("type_strs is <%d>\n", (int) type_strs);
   */

    rows = (int *) NclGetArgValue(
                        1,
                        4,
                        &ndim_rows,
                        dimsz_rows,
                        &missing_rows,
                        &has_missing_rows,
                        &type_rows,
                        DONT_CARE);

  /*
    printf("\n\n");
   *printf("ndim_rows is <%d>\n", ndim_rows);
   *printf("dimsz_rows is <%d>\n", dimsz_rows[0]);
   *printf("missing_rows is <%d>\n", missing_rows);
   *printf("has_missing_rows is <%d>\n", has_missing_rows);
   *printf("type_rows is <%d>\n", (int) type_rows);
    printf("rows is <%d>\n", rows[0]);
   */

    if (rows == NULL)
    {
        NhlPError(NhlFATAL, errno, "getArraySubString: memory allocation error.");
        return NhlFATAL;
    }

    cols = (int *) NclGetArgValue(
                        2,
                        4,
                        &ndim_cols,
                        dimsz_cols,
                        &missing_cols,
                        &has_missing_cols,
                        &type_cols,
                        DONT_CARE);

  /*
    printf("\n\n");
   *printf("ndim_cols is <%d>\n", ndim_cols);
   *printf("dimsz_cols is <%d>\n", dimsz_cols[0]);
   *printf("missing_cols is <%d>\n", missing_cols);
   *printf("has_missing_cols is <%d>\n", has_missing_cols);
   *printf("type_cols is <%d>\n", (int) type_cols);
    printf("cols is <%d>\n", cols[0]);
   */

    if (cols == NULL)
    {
        NhlPError(NhlFATAL, errno, "getArraySubString: memory allocation error.");
        return NhlFATAL;
    }

    delim = (string *) NclGetArgValue(
                        3,
                        4,
                        &ndim_delim,
                        dimsz_delim,
                        &missing_delim,
                        &has_missing_delim,
                        &type_delim,
                        DONT_CARE);

    if (delim == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    tmp_delim = (char *) NrmQuarkToString(delim[0]);

  /*
    printf("\n\n");
   *printf("ndim_delim is <%d>\n", ndim_delim);
   *printf("dimsz_delim is <%d>\n", dimsz_delim[0]);
   *printf("missing_delim is <%d>\n", missing_delim);
   *printf("has_missing_delim is <%d>\n", has_missing_delim);
   *printf("type_delim is <%d>\n", (int) type_delim);
    printf("tmp_delim is <%s>\n", tmp_delim);
   */

    arraySubString = (string *) malloc(rows[0]*1024);

    if (has_missing_strs && found_missing_strs)
    {
        ret_missing_strs.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) arraySubString, ndim_strs, dimsz_strs, &ret_missing_strs, NCL_int, 0);
    }
    else
    {
        for(i=0; i<rows[0]; i++)
        {
            tmp_str = (char *) NrmQuarkToString(strs[i]);
          /*
            printf("line %4d: tmp_str is <%s>\n", i, tmp_str);
           */
            result = strtok(tmp_str, tmp_delim);
            n = 0;
            while(result != NULL)
            {
                n++;
                if(n == cols[0])
                {
                    arraySubString[i] = NrmStringToQuark(result);
                  /*
                    printf("colStrings[%d] is <%s>, transback: <%s>\n", i, result, NrmQuarkToString(arraySubString[i]));
                   */
                    break;
                }
                result = strtok(NULL, tmp_delim);
            }
        }

        return NclReturnValue(arraySubString, ndim_strs, dimsz_strs, NULL, NCL_string, 1);
    }

    free(arraySubString);
  /*
    NclFree(arraySubString);
   */
}

#ifdef __cplusplus
}
#endif

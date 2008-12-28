/*
 *      $Id: userAddFuncs.c,v 1.2 2008-12-28 17:13:46 huangwei Exp $
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
  
    char tmp_str[2048];
    char tmp_delim[1024];
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

    if (str == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    delim = (string *) NclGetArgValue(
                        1,
                        2,
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

    strcpy(tmp_str, (char *) NrmQuarkToString(str[0]));

    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

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
    int ndim_cols, dimsz_cols[NCL_MAX_DIMENSIONS];
    int ndim_delim, dimsz_delim[NCL_MAX_DIMENSIONS];
    int has_missing_strs, found_missing_strs = 0;
    int has_missing_cols, found_missing_cols = 0;
    int has_missing_delim, found_missing_delim = 0;
    NclScalar   missing_strs, ret_missing_strs;
    NclScalar   missing_cols, ret_missing_cols;
    NclScalar   missing_delim, ret_missing_delim;
    NclBasicDataTypes type_strs, type_cols, type_delim;
  
    int i, j, k, n;

    char tmp_str[2048];
    char tmp_delim[1024];
    char *result = NULL;
    string *arraySubString;
    int str_size;
    int *cols;
    
    strs = (string *) NclGetArgValue(
                        0,
                        3,
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

    cols = (int *) NclGetArgValue(
                        1,
                        3,
                        &ndim_cols,
                        dimsz_cols,
                        &missing_cols,
                        &has_missing_cols,
                        &type_cols,
                        DONT_CARE);

    if (cols == NULL)
    {
        NhlPError(NhlFATAL, errno, "getArraySubString: memory allocation error.");
        return NhlFATAL;
    }

    delim = (string *) NclGetArgValue(
                        2,
                        3,
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

    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

    str_size = 1;
    for(k=0; k<ndim_strs; k++)
        str_size *= dimsz_strs[k];

    arraySubString = (string *) malloc(str_size*1024);

    if (has_missing_strs && found_missing_strs)
    {
        ret_missing_strs.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) arraySubString, ndim_strs, dimsz_strs, &ret_missing_strs, NCL_int, 0);
    }
    else
    {
        i = 0;
        for(k=0; k<ndim_strs; k++)
        for(j=0; j<dimsz_strs[k]; j++)
        {
            strcpy(tmp_str, (char *) NrmQuarkToString(strs[i]));
            result = strtok(tmp_str, tmp_delim);
            n = 0;
            while(result != NULL)
            {
                n++;
                if(n == cols[0])
                {
                    arraySubString[i] = NrmStringToQuark(result);
                    break;
                }
                result = strtok(NULL, tmp_delim);
            }

            i++;
        }

        return NclReturnValue(arraySubString, ndim_strs, dimsz_strs, NULL, NCL_string, 1);
    }

    NclFree(arraySubString);
  /*
    free(arraySubString);
   */
}


NhlErrorTypes _NclgetColsFromString
#if     NhlNeedProto
(void)
#else
()
#endif
{
    string *strs;

    int ndim_strs, dimsz_strs[NCL_MAX_DIMENSIONS];
    int ndim_start, dimsz_start[NCL_MAX_DIMENSIONS];
    int ndim_end, dimsz_end[NCL_MAX_DIMENSIONS];
    int has_missing_strs, found_missing_strs = 0;
    int has_missing_start, found_missing_start = 0;
    int has_missing_end, found_missing_end = 0;
    NclScalar   missing_strs, ret_missing_strs;
    NclScalar   missing_start, ret_missing_start;
    NclScalar   missing_end, ret_missing_end;
    NclBasicDataTypes type_strs, type_start, type_end;
  
    int i, j, k, m, n;

    char tmp_str[2048];
    char result[2048];
    string *arraySubString;
    int str_size;
    int *startCol;
    int *endCol;
    
    strs = (string *) NclGetArgValue(
                        0,
                        3,
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

    startCol = (int *) NclGetArgValue(
                        1,
                        3,
                        &ndim_start,
                        dimsz_start,
                        &missing_start,
                        &has_missing_start,
                        &type_start,
                        DONT_CARE);

    if (startCol == NULL)
    {
        NhlPError(NhlFATAL, errno, "getArraySubString: memory allocation error.");
        return NhlFATAL;
    }

    endCol = (int *) NclGetArgValue(
                        2,
                        3,
                        &ndim_end,
                        dimsz_end,
                        &missing_end,
                        &has_missing_end,
                        &type_end,
                        DONT_CARE);

    if (endCol == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    str_size = 1;
    for(k=0; k<ndim_strs; k++)
        str_size *= dimsz_strs[k];

    arraySubString = (string *) malloc(str_size*1024);

    if (has_missing_strs && found_missing_strs)
    {
        ret_missing_strs.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) arraySubString, ndim_strs, dimsz_strs, &ret_missing_strs, NCL_int, 0);
    }
    else
    {
        i = 0;
        for(k=0; k<ndim_strs; k++)
        for(j=0; j<dimsz_strs[k]; j++)
        {
            strcpy(tmp_str, (char *) NrmQuarkToString(strs[i]));

            if(startCol[0] < 1)
            { startCol[0] = 1; }

            m=0;
            for(n=startCol[0]-1; n<endCol[0]; n++)
            {
                result[m++] = tmp_str[n];
            }
            result[m] = '\0';
            arraySubString[i] = NrmStringToQuark(result);

            i++;
        }

        return NclReturnValue(arraySubString, ndim_strs, dimsz_strs, NULL, NCL_string, 1);
    }

    free(arraySubString);
}


NhlErrorTypes _NclremoveCharFromString
#if     NhlNeedProto
(void)
#else
()
#endif
{
    string *str;
    string *chr;

    int ndim_str, dimsz_str[NCL_MAX_DIMENSIONS];
    int ndim_chr, dimsz_chr[NCL_MAX_DIMENSIONS];
    int has_missing_str, found_missing_str = 0;
    int has_missing_chr, found_missing_chr = 0;
    NclScalar   missing_str, ret_missing_str;
    NclScalar   missing_chr, ret_missing_chr;
    NclBasicDataTypes type_str, type_chr;
  
    char tmp_str[2048];
    char tmp_chr;
    string *arrayString;
    int i, j, k, m, n;
    int str_size;
    int cols;
    
    str = (string *) NclGetArgValue(
                        0,
                        2,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        &type_str,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    chr = (string *) NclGetArgValue(
                        1,
                        2,
                        &ndim_chr,
                        dimsz_chr,
                        &missing_chr,
                        &has_missing_chr,
                        &type_chr,
                        DONT_CARE);

    if (chr == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    strcpy(tmp_str, (char *) NrmQuarkToString(chr[0]));
    tmp_chr = tmp_str[0];

    str_size = 1;
    for(k=0; k<ndim_str; k++)
        str_size *= dimsz_str[k];

    arrayString = (string *) malloc(str_size*1024);

    if (has_missing_str && found_missing_str)
    {
        ret_missing_str.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) arrayString, ndim_str, dimsz_str, &ret_missing_str, NCL_int, 0);
    }
    else
    {
        i = 0;
        for(k=0; k<ndim_str; k++)
        for(j=0; j<dimsz_str[k]; j++)
        {
            strcpy(tmp_str, (char *) NrmQuarkToString(str[i]));

            cols = strlen(tmp_str);
            m=0;
            for(n=0; n<cols; n++)
            {
                if(tmp_str[n] != tmp_chr)
                {
                    tmp_str[m++] = tmp_str[n];
                }
            }
            tmp_str[m] = '\0';
            arrayString[i] = NrmStringToQuark(tmp_str);

            i++;
        }

        return NclReturnValue(arrayString, ndim_str, dimsz_str, NULL, NCL_string, 1);
    }

    free(arrayString);

}

NhlErrorTypes _NclswitchCharInString
#if     NhlNeedProto
(void)
#else
()
#endif
{
    string *str;
    string *o_c;
    string *n_c;

    int ndim_str, dimsz_str[NCL_MAX_DIMENSIONS];
    int ndim_o_c, dimsz_o_c[NCL_MAX_DIMENSIONS];
    int ndim_n_c, dimsz_n_c[NCL_MAX_DIMENSIONS];
    int has_missing_str, found_missing_str = 0;
    int has_missing_o_c, found_missing_o_c = 0;
    int has_missing_n_c, found_missing_n_c = 0;
    NclScalar   missing_str, ret_missing_str;
    NclScalar   missing_o_c, ret_missing_o_c;
    NclScalar   missing_n_c, ret_missing_n_c;
    NclBasicDataTypes type_str, type_o_c, type_n_c;
  
    char tmp_str[2048];
    char tmp_o_c;
    char tmp_n_c;
    string *arrayString;
    int i, j, k, n;
    int str_size;
    int cols;

    str = (string *) NclGetArgValue(
                        0,
                        2,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        &type_str,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    o_c = (string *) NclGetArgValue(
                        1,
                        2,
                        &ndim_o_c,
                        dimsz_o_c,
                        &missing_o_c,
                        &has_missing_o_c,
                        &type_o_c,
                        DONT_CARE);

    if (o_c == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    strcpy(tmp_str, (char *) NrmQuarkToString(o_c[0]));
    tmp_o_c = tmp_str[0];

    n_c = (string *) NclGetArgValue(
                        1,
                        2,
                        &ndim_n_c,
                        dimsz_n_c,
                        &missing_n_c,
                        &has_missing_n_c,
                        &type_n_c,
                        DONT_CARE);

    if (n_c == NULL)
    {
        NhlPError(NhlFATAL, errno, "getColsInString: memory allocation error.");
        return NhlFATAL;
    }

    strcpy(tmp_str, (char *) NrmQuarkToString(n_c[0]));
    tmp_n_c = tmp_str[0];

    str_size = 1;
    for(k=0; k<ndim_str; k++)
        str_size *= dimsz_str[k];

    arrayString = (string *) malloc(str_size*1024);

    if (has_missing_str && found_missing_str)
    {
        ret_missing_str.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) arrayString, ndim_str, dimsz_str, &ret_missing_str, NCL_int, 0);
    }
    else
    {
        i = 0;
        for(k=0; k<ndim_str; k++)
        for(j=0; j<dimsz_str[k]; j++)
        {
            strcpy(tmp_str, (char *) NrmQuarkToString(str[i]));

            cols = strlen(tmp_str);
            for(n=0; n<cols; n++)
            {
                if(tmp_str[n] != tmp_o_c)
                {
                    tmp_str[n] = tmp_n_c;
                }
            }
            arrayString[i] = NrmStringToQuark(tmp_str);

            i++;
        }

        return NclReturnValue(arrayString, ndim_str, dimsz_str, NULL, NCL_string, 1);
    }

    free(arrayString);

}

#ifdef __cplusplus
}
#endif


/*
 *      $Id: userAddFuncs.c,v 1.30 2010-02-07 01:34:26 haley Exp $
 */
/************************************************************************
*                                                                       *
*                   Copyright (C)  2009                                 *
*           University Corporation for Atmospheric Research             *
*                   All Rights Reserved                                 *
*                                                                       *
************************************************************************/
/*
 *	File:		userAddFuncs.c
 *
 *	Author:		Wei Huang
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Mar 20 11:25:53 MDT 2009
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>
#include <ncarg/c.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/Error.h>
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
#include <ctype.h>
#include <string.h>

#define MAX_PRINT_SPACES	128
#define MAX_LIST_ELEMENT	128
#define MAX_PRINT_NAME_LENGTH	1024

#define NCL_INITIAL_STRING_LENGTH	8192

NhlErrorTypes _Nclstr_fields_count
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;
    NclQuark *delim;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_delim;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   missing_delim;
    NclScalar   ret_missing;
  
    char *tmp_str;
    char *tmp_delim;
    char *result = NULL;
    int *fields;
    ng_size_t i;
    ng_size_t str_size = 1;
    int max_length = 0;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL,ENOMEM,"str_fields_count: input string is null.");
        return NhlFATAL;
    }

    delim = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_delim,
                        &has_missing_delim,
                        NULL,
                        DONT_CARE);

    if (delim == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_fields_count: delimiter is null.");
        return NhlFATAL;
    }

/*
 * Get the default integer missing value.
 *
 * This will be used if "bad" data is input, like a missing input
 * string or a missing delimiter.
 */
    ret_missing.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

    for(i=0; i<ndim_strs; i++)
    {
        str_size *= dimsz_strs[i];
    }

    fields = (int *) NclMalloc((unsigned int) sizeof(int) * str_size);
    if (! fields)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    tmp_str = (char *) NrmQuarkToString(delim[0]);
    tmp_delim = (char *) NclMalloc(strlen(tmp_str)+2);
    if (! tmp_delim)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

    for(i=0; i<str_size; i++)
    {
/*
 * Let the missing value string length get included here, because
 * it might be the longest string.
 */
      tmp_str = (char *) NrmQuarkToString(strs[i]);

      if (max_length < strlen(tmp_str))
          max_length = strlen(tmp_str);
    }
    max_length ++;

    tmp_str = (char *) NclMalloc(max_length);
    if (! tmp_str)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if ( (has_missing_strs  && strs[i]  == missing_strs.stringval) ||
             (has_missing_delim && delim[0] == missing_delim.stringval) )
        {
             fields[i] = ret_missing.intval;
             has_missing_ret = 1;
             continue;
        }

        strcpy(tmp_str, (char *) NrmQuarkToString(strs[i]));

        fields[i] = 0;

        result = strtok(tmp_str, tmp_delim);
        while(result != NULL)
        {
            fields[i] ++;
            result = strtok(NULL, tmp_delim);
        }
    }

    NclFree(tmp_str);
    NclFree(tmp_delim);

    return NclReturnValue((void *) fields, ndim_strs, dimsz_strs, 
                          (has_missing_ret ? &ret_missing : NULL),
                          NCL_int, 0);
}


NhlErrorTypes _Nclstr_get_field
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;
    NclQuark *delim;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_delim;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   missing_delim;
    NclScalar   ret_missing;
  
    ng_size_t i;
    int n;

    char *tmp_str;
    char *tmp_delim;
    char *result = NULL;
    NclQuark *arraySubString;
    ng_size_t str_size;
    int *field;
/*
 * I didn't see how these two variables were needed, so commenting out.
 *
 *    int *field_record;
 *    int has_miss_field = 0;
 */
    int max_length = 0;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        3,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_get_field: input string is null.");
        return NhlFATAL;
    }

    field = (int *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    delim = (NclQuark *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        &missing_delim,
                        &has_missing_delim,
                        NULL,
                        DONT_CARE);

    if(has_missing_strs)
        ret_missing.stringval = missing_strs.stringval;
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_strs; i++)
        str_size *= dimsz_strs[i];

    tmp_str = (char *) NrmQuarkToString(delim[0]);
    tmp_delim = (char *) NclMalloc(strlen(tmp_str)+2);
    if (! tmp_delim)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(strs[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    tmp_str = (char *) NclMalloc(max_length);
    if (! tmp_str)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    arraySubString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arraySubString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
  /*
   *field_record = (int *) NclMalloc(str_size*sizeof(int));
   *if (! field_record)
   *{
   *    NHLPERROR((NhlFATAL,ENOMEM,NULL));
   *    return NhlFATAL;
   *}
   */

    for(i=0; i<str_size; i++)
    {
      /* Default to an empty string. */
        arraySubString[i] = NrmStringToQuark("");

        if ((has_missing_delim && delim[0] == missing_delim.stringval) ||
            (has_missing_strs && strs[i] == missing_strs.stringval))
        {
            has_missing_ret = 1;
	    arraySubString[i] = ret_missing.stringval;
            continue;
        }
        else
        {
          /*
           * field_record[i] = 1;
           */
            strcpy(tmp_str, (char *) NrmQuarkToString(strs[i]));
            result = strtok(tmp_str, tmp_delim);
            n = 0;
            while(result != NULL)
            {
                n++;
                if(n == field[0])
                {
                    arraySubString[i] = NrmStringToQuark(result);
                  /*
                   * field_record[i] = 0;
                   */
                    break;
                }
                result = strtok(NULL, tmp_delim);
            }

          /*
           * has_miss_field += field_record[i];
           */
      }
    }

  /*
   *if(has_miss_field)
   *{
   *    NhlPError(NhlWARNING, NhlEUNKNOWN,
   *             "Input field is too big, which causes some or all new fields to missing.");
   *}
   */

    NclFree(tmp_str);
    NclFree(tmp_delim);
   /*
    *NclFree(field_record);
    */

    return NclReturnValue(arraySubString, ndim_strs, dimsz_strs, (has_missing_ret ? &ret_missing : NULL), NCL_string, 0);

}


NhlErrorTypes _Nclstr_split
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;
    NclQuark *delim;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_delim;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   missing_delim;
    NclScalar   ret_missing;
  
    int n;

    char *tmp_str;
    char *tmp_delim;
    char *result = NULL;
    NclQuark *arraySubString;

    int max_length = 1;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_split: input string is null.");
        return NhlFATAL;
    }

    delim = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_delim,
                        &has_missing_delim,
                        NULL,
                        DONT_CARE);

    if(has_missing_strs)
        ret_missing.stringval = missing_strs.stringval;
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    tmp_delim = (char *) NclMalloc(strlen(NrmQuarkToString(delim[0]))+2);
    if (! tmp_delim)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

    arraySubString = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! arraySubString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    if ((has_missing_delim && delim[0] == missing_delim.stringval) ||
        (has_missing_strs && strs[0] == missing_strs.stringval))
    {
        has_missing_ret = 1;
        arraySubString[0] = ret_missing.stringval;
    }
    else
    {
        n = strlen((char *) NrmQuarkToString(strs[0])) + 2;
        tmp_str = (char *) NclMalloc(n);
        if (! tmp_str)
        {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }

        strcpy(tmp_str, (char *) NrmQuarkToString(strs[0]));
        result = strtok(tmp_str, tmp_delim);
        n = 0;
        while(result != NULL)
        {
            arraySubString[n] = NrmStringToQuark(result);
            n++;
            if(n >= max_length)
            {
                max_length *= 2;
                arraySubString = (NclQuark *) NclRealloc(arraySubString, max_length*sizeof(NclQuark));
                if (! arraySubString)
                {
                    NHLPERROR((NhlFATAL,ENOMEM,NULL));
                    return NhlFATAL;
                }
            }
            result = strtok(NULL, tmp_delim);
        }
        arraySubString = (NclQuark *) NclRealloc(arraySubString, n*sizeof(NclQuark));
        ndim_strs = 1;
        dimsz_strs[0] = n;

        NclFree(tmp_str);
    }

    NclFree(tmp_delim);
    
    return NclReturnValue(arraySubString, ndim_strs, dimsz_strs, (has_missing_ret ? &ret_missing : NULL), NCL_string, 0);
}


/*
 * This function is targeting to split CSV strings.
 */

NhlErrorTypes _Nclstr_split_csv(void)
{
    NclQuark *strs;
    NclQuark *delim;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_delim;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   missing_delim;
    NclScalar   ret_missing;
  
    int *in_option;
    int option;

    int i, m, n;

    char *tmp_str = NULL;
    char *tmp_delim = NULL;

    NclQuark *return_strs = NULL;
    int num_fields = 0;
    int max_fields = 1;
    int total_in_strs = 1;
    int total_out_strs = 1;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        3,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_get_field: input string is null.");
        return NhlFATAL;
    }

    delim = (NclQuark *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        &missing_delim,
                        &has_missing_delim,
                        NULL,
                        DONT_CARE);

    if(has_missing_strs)
    {
        has_missing_ret = 1;
        ret_missing.stringval = missing_strs.stringval;
    }
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    tmp_delim = (char *) NclMalloc(strlen(NrmQuarkToString(delim[0]))+2);
    if (! tmp_delim)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

    in_option = (int *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    option = in_option[0];

    if(0 > option)
        option = 0;
    else if(3 < option)
        option = 3;

    for(n = 0; n < ndim_strs; n++)
        total_in_strs *= dimsz_strs[n];

    if(has_missing_delim && delim[0] == missing_delim.stringval)
    {
        has_missing_ret = 1;

        return_strs = (NclQuark *) NclCalloc(total_in_strs, sizeof(NclQuark));
        if (! return_strs)
        {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }

        for(n = 0; n < total_in_strs; n++)
            return_strs[n] = ret_missing.stringval;
    }
    else
    {
        int len_delim = strlen(tmp_delim);
        int len_str = 1 + strlen((char *) NrmQuarkToString(strs[0]));
        int max_str_len = len_str;
        int pre_pos = -1;
        char prt_str[NCL_INITIAL_STRING_LENGTH];

        int in_dq = 0;
        int in_sq = 0;

        ndim_strs ++;
        max_fields = 1;

        if(NULL == tmp_str)
            tmp_str = (char *) NclCalloc(max_str_len, sizeof(char));
        else
            tmp_str = (char *) NclRealloc(tmp_str, max_str_len * sizeof(char));
        if (! tmp_str)
        {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }

        strcpy(tmp_str, (char *) NrmQuarkToString(strs[0]));

        num_fields = 0;

      /*
       *fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\toption = %d\n", option);
       *fprintf(stderr, "\tlen_str = %d, len_delim = %d\n", len_str, len_delim);
       *fprintf(stderr, "\tthe string: <%s>\n", tmp_str);
       *fprintf(stderr, "\tthe delimitor: <%s>\n\n", tmp_delim);
       */
        
        n = 0;
        pre_pos = 0;
        in_dq = 0;
        in_sq = 0;

      /*
       *fprintf(stderr, "Input string: <%s>\n", tmp_str);
       */

        while(n < len_str)
        {
          /*
           *fprintf(stderr, "\tcurrent string %d: <%s>\n", n, tmp_str+n);
           *fprintf(stderr, "\tstrncmp(<%s>, <%s>, %d) = %d\n",
           *                   tmp_str+n, tmp_delim, len_delim,
           *                   strncmp(tmp_str+n, tmp_delim, len_delim));
           */

            if(0 == strncmp(tmp_str+n, tmp_delim, len_delim))
            {
                if(n - pre_pos)
                {
                    strncpy(prt_str, tmp_str+pre_pos, n - pre_pos);
                    prt_str[n-pre_pos] = '\0';
#if 0
                  /*
                   */
                    fprintf(stderr, "\tnew string %d: <%s>\n", num_fields, prt_str);
                }
                else
                {
                  /*
                   */
                    fprintf(stderr, "\tnew string %d: <missing>\n", num_fields);
#endif
                }

                if(in_dq || in_sq)
                {
                   ++n;
                   continue;
                }

                ++num_fields;

                n += len_delim;

                pre_pos = n;
            }
            else
            {
                if('"' == tmp_str[n])
                {
                    if(in_dq)
                        in_dq = 0;
                    else
                        in_dq = 1;
                    if(option%2)
                        in_dq = 0;
                }

                if('\'' == tmp_str[n])
                {
                    if(in_sq)
                        in_sq = 0;
                    else
                        in_sq = 1;
                    if(1 < option)
                        in_sq = 0;
                }

                ++n;
            }
        }

        num_fields ++;

        max_fields = num_fields;

loop_through_strings:
        dimsz_strs[ndim_strs - 1] = max_fields;

        total_out_strs = total_in_strs * max_fields;

        if(NULL == return_strs)
            return_strs = (NclQuark *) NclCalloc(total_out_strs, sizeof(NclQuark));
        else
            return_strs = (NclQuark *) NclRealloc(return_strs, total_out_strs * sizeof(NclQuark));
        if (! return_strs)
        {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }

        for(n = 0; n < total_out_strs; n++)
        {
            return_strs[n] = ret_missing.stringval;
        }

        for(i = 0; i < total_in_strs; i++)
        {
            len_str = 1 + strlen((char *) NrmQuarkToString(strs[i]));
            while(max_str_len < len_str)
            {
                max_str_len *= 2;

                tmp_str = (char *) NclRealloc(tmp_str, max_str_len * sizeof(char));
                if (! tmp_str)
                {
                    NHLPERROR((NhlFATAL,ENOMEM,NULL));
                    return NhlFATAL;
                }
            }

            strcpy(tmp_str, (char *) NrmQuarkToString(strs[i]));

          /*
           *fprintf(stderr, "Input string %d: <%s>\n", i, tmp_str);
           */

            num_fields = 0;

            m = i * max_fields;
            n = 0;
            pre_pos = 0;
            in_dq = 0;
            in_sq = 0;

            while(n < len_str)
            {
                if(0 == strncmp(tmp_str+n, tmp_delim, len_delim))
                {
                    if(in_dq || in_sq)
                    {
                       ++n;
                       continue;
                    }

                    if(n - pre_pos)
                    {
                        strncpy(prt_str, tmp_str+pre_pos, n - pre_pos);
                        prt_str[n-pre_pos] = '\0';

                        return_strs[m] = NrmStringToQuark(prt_str);
                    }
                    else
                    {
                        has_missing_ret = 1;
                    }

                    ++m;
                    
                    ++num_fields;

                    if(max_fields < num_fields)
                    {
                        max_fields = num_fields + 1;
                        goto loop_through_strings;
                    }

                    n += len_delim;
                    pre_pos = n;
                }
                else
                {
                    if('"' == tmp_str[n])
                    {
                        if(in_dq)
                            in_dq = 0;
                        else
                            in_dq = 1;
                        if(option%2)
                            in_dq = 0;
                    }

                    if('\'' == tmp_str[n])
                    {
                        if(in_sq)
                            in_sq = 0;
                        else
                            in_sq = 1;
                        if(1 < option)
                            in_sq = 0;
                    }

                    ++n;
                }
            }

            n = len_str - 1;
            if(n - pre_pos)
            {
                strncpy(prt_str, tmp_str+pre_pos, n - pre_pos);
                prt_str[n-pre_pos] = '\0';

                return_strs[m] = NrmStringToQuark(prt_str);
            }
        }

        NclFree(tmp_str);
    }

    NclFree(tmp_delim);
    
    return NclReturnValue(return_strs, ndim_strs, dimsz_strs, (has_missing_ret ? &ret_missing : NULL), NCL_string, 0);
}


NhlErrorTypes _Nclstr_get_cols
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing = 0;
    NclScalar   missing_strs;
    NclScalar   ret_missing;
  
    ng_size_t i;
    int m, n;

    char *tmp_str;
    char *result;
    NclQuark *arraySubString;
    ng_size_t str_size;
    int ms, me, ns, ne, is, ie;
    int *startCol, sC;
    int *endCol, eC;
    int max_length = 0;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        3,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_get_cols: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_strs)
    {
        ret_missing.stringval = missing_strs.stringval;
    }
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    startCol = (int *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    if (startCol == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_get_cols: input start column is null.");
        return NhlFATAL;
    }

    endCol = (int *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    if (endCol == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_get_cols: input end column is null.");
        return NhlFATAL;
    }

/*
 * Make copies so they don't get changed before going back
 * to calling routine. 
 */
    sC = startCol[0];
    eC = endCol[0];

    str_size = 1;
    for(i=0; i<ndim_strs; i++)
        str_size *= dimsz_strs[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(strs[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arraySubString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arraySubString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    ms = sC;
    me = eC;

    if(sC < 0)
        sC = INT_MAX;

    if(eC < 0)
        eC = INT_MAX;

    if(eC < sC)
    {
        is = sC;
        ne = eC - 1;

        for(i=0; i<str_size; i++)
        {
            if (has_missing_strs && strs[i] == missing_strs.stringval)
            {
                arraySubString[i] = ret_missing.stringval;
                has_missing = 1;
                continue;
            }

            tmp_str = (char *) NrmQuarkToString(strs[i]);

            ns = strlen(tmp_str);
            if(is < ns)
            {
                ns = is;
            }
            else
            {
                if(ms < 0)
                   ns += ms;
                else
                   ns --;
            }

            m=0;
            for(n=ns; n>ne; n--)
            {
                result[m++] = tmp_str[n];
            }
            result[m] = '\0';
            arraySubString[i] = NrmStringToQuark(result);
        }

      /*
       *NhlPError(NhlWARNING, NhlEUNKNOWN,
       *         "Input start column is larger than end column, field will be reversed.");
       */

    }
    else if(eC > sC)
    {
        ns = sC;
        ie = eC;

        for(i=0; i<str_size; i++)
        {
            if (has_missing_strs && strs[i] == missing_strs.stringval)
            {
                arraySubString[i] = ret_missing.stringval;
                has_missing = 1;
                continue;
            }

            tmp_str = (char *) NrmQuarkToString(strs[i]);

            ne = strlen(tmp_str);
            if(ie < ne)
            {
                ne = ie + 1;
            }
            else
            {
                if(me < 0)
                   ne += me + 1;
            }

            m=0;
            for(n=ns; n<ne; n++)
            {
                result[m++] = tmp_str[n];
            }
            result[m] = '\0';
            arraySubString[i] = NrmStringToQuark(result);
        }
    }
    else if(ms >= 0)
    {
        ns = sC;
        ie = eC;

        for(i=0; i<str_size; i++)
        {
            if (has_missing_strs && strs[i] == missing_strs.stringval)
            {
                arraySubString[i] = ret_missing.stringval;
                has_missing = 1;
                continue;
            }

            tmp_str = (char *) NrmQuarkToString(strs[i]);

            ne = strlen(tmp_str);
            if(ie < ne)
            {
                ne = ie + 1;
            }

            m=0;
            for(n=ns; n<ne; n++)
            {
                result[m++] = tmp_str[n];
            }
            result[m] = '\0';
            arraySubString[i] = NrmStringToQuark(result);
        }
    }
    else
    {
        if(ms <= me)
        {
            for(i=0; i<str_size; i++)
            {
                if (has_missing_strs && strs[i] == missing_strs.stringval)
                {
                    arraySubString[i] = ret_missing.stringval;
                    has_missing = 1;
                    continue;
                }

                tmp_str = (char *) NrmQuarkToString(strs[i]);
    
                ie = strlen(tmp_str);
                ns = ie + ms;
                ne = ie + me + 1;
                if(ns < 0)
                {
                    ns = 0;
                }
                if(ne > ie)
                {
                    ne = ie;
                }

                m=0;
                for(n=ns; n<ne; n++)
                {
                    result[m++] = tmp_str[n];
                }
                result[m] = '\0';
                arraySubString[i] = NrmStringToQuark(result);
            }
        }
        else
        {
            for(i=0; i<str_size; i++)
            {
                if (has_missing_strs && strs[i] == missing_strs.stringval)
                {
                    arraySubString[i] = ret_missing.stringval;
                    has_missing = 1;
                    continue;
                }

                tmp_str = (char *) NrmQuarkToString(strs[i]);

                ie = strlen(tmp_str);
                ns = ie + me;
                ne = ie + ms;
                if(ns < 0)
                {
                    ns = 0;
                }
                if(ne >= ie)
                {
                    ne = ie - 1;
                }

                m=0;
                for(n=ne; n>=ns; n--)
                {
                    result[m++] = tmp_str[n];
                }
                result[m] = '\0';
                arraySubString[i] = NrmStringToQuark(result);
            }
        }
    }

    NclFree(result);

    return NclReturnValue(arraySubString, ndim_strs, dimsz_strs, ( has_missing ? &ret_missing : NULL), NCL_string, 0);

}

NhlErrorTypes _Nclstr_split_by_length
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;
    int    *leng;
    NclQuark *new_strs;

    int ndim_strs;
    int ndim_leng;
    int ndim_news;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    ng_size_t dimsz_leng[NCL_MAX_DIMENSIONS];
    ng_size_t dimsz_news[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_leng;
    int has_missing_news;
    ng_size_t str_size;
    ng_size_t len_size;
    ng_size_t new_size;
    NclScalar   missing_strs;
    NclScalar   missing_leng;
    NclScalar   missing_news;
  
    ng_size_t i, n;

    char *tmp_str;
    char *result;
    int *news_length;
    int sl, ip, ns, ne;
    int number_splitted = 1;
    int max_length = 1;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_get_cols: input string is null.");
        return NhlFATAL;
    }

    has_missing_news = has_missing_strs;
    if(has_missing_strs)
        missing_news.stringval = missing_strs.stringval;
    else
        missing_news.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    leng = (int *) NclGetArgValue(
                        1,
                        2,
                        &ndim_leng,
                        dimsz_leng,
                        &missing_leng,
                        &has_missing_leng,
                        NULL,
                        DONT_CARE);

    if (leng == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_split_by_length: input leng is null.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_strs; i++)
    {
        str_size *= dimsz_strs[i];
        dimsz_news[i] = dimsz_strs[i];
    }

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(strs[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    
    news_length = (int *) NclCalloc(max_length + 1, sizeof(int));
    if (! news_length)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    len_size = 1;
    for(i=0; i<ndim_leng; i++)
    {
        len_size *= dimsz_leng[i];
    }

    if(len_size == 1)
    {
        if(leng[0] < 1)
        {
            number_splitted = max_length;
            leng[0] = 1;
        }
        else
        {
            number_splitted = max_length/leng[0];
            if(number_splitted * leng[0] < max_length)
                number_splitted ++;
        }

        if(max_length < leng[0])
            max_length = leng[0];

        for(i=0; i<number_splitted; i++)
        {
            news_length[i] = leng[0];
        }
    }
    else
    {
        int tmp_length = max_length;
        number_splitted = 0;
        while(tmp_length > 0)
        {
            for(i=0; i<len_size; i++)
            {
                tmp_length -= leng[i];
                news_length[number_splitted] = leng[i];
                number_splitted++;
                if(tmp_length < 1)
                    break;
            }
        }
    }
  /*
   *fprintf(stderr, "\tnumber_splitted = %d\n", number_splitted);
   */

    result = (char *) NclCalloc(max_length + 1, sizeof(char));
    if (!result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    if((1 == dimsz_strs[0]) && (1 == ndim_strs))
    {
        dimsz_news[0] = number_splitted;
        ndim_news = 1;
    }
    else
    {
        dimsz_news[ndim_strs] = number_splitted;
        ndim_news = ndim_strs + 1;
    }

  /*
   *fprintf(stderr, "\tndim_news = %d\n", ndim_news);
   */

    new_size = 1;
    for(i=0; i<ndim_news; i++)
    {
        new_size *= dimsz_news[i];
    }
  /*
   *fprintf(stderr, "\tnew_size = %d\n", new_size);
   */
    
    new_strs = (NclQuark *) NclMalloc(new_size*sizeof(NclQuark));
    if (!new_strs)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        ns = i * number_splitted;
        ne = ns + number_splitted;

        if (has_missing_strs && strs[i] == missing_strs.stringval)
        {
            for(n=ns; n<ne; n++)
                new_strs[n] = missing_news.stringval;
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(strs[i]);
      /*
       *fprintf(stderr, "\tstrs[%d]: <%s>\n", i, tmp_str);
       */

        sl = strlen(tmp_str) - 1;
        ip = 0;
        for(n=ns; n<ne; n++)
        {
            if(ip > sl)
            {
                new_strs[n] = missing_news.stringval;
              /*
               *fprintf(stderr, "\tnew_strs[%d]: missing\n", n);
               */
            }
            else
            {
                strncpy(result, tmp_str + ip, news_length[n-ns]);
                result[news_length[n-ns]] = '\0';
              /*
               *fprintf(stderr, "\tnew_strs[%d]: <%s>, leng: %d\n",
               *                   n, result, news_length[n-ns]);
               *fprintf(stderr, "\tstrlen(tmp_str + ip) = %d, news_length[n-ns] = %d\n",
               *                   strlen(tmp_str + ip), news_length[n-ns]);
               */

                ip += news_length[n-ns];
                new_strs[n] = NrmStringToQuark(result);
                memset(result, 0, max_length);
            }
        }
    }

    NclFree(result);
    NclFree(news_length);

    return NclReturnValue(new_strs, ndim_news, dimsz_news,
                         (has_missing_news ? &missing_news : NULL),
                          NCL_string, 0);
}

NhlErrorTypes _Nclstr_sub_str
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;
    NclQuark *o_s;
    NclQuark *n_s;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing_o_s;
    int has_missing_n_s;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   missing_o_s;
    NclScalar   missing_n_s;
    NclScalar   ret_missing;

    char *tmp_str;
    char *new_str;
    char *tmp_o_s;
    char *tmp_n_s;
    NclQuark *arrayString;
    ng_size_t i;
    ng_size_t str_size;
    int m, n, nf, nn;
    ng_size_t current_size = 0;
    int cols, o_s_len, n_s_len;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        3,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_sub_str: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    o_s = (NclQuark *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        &missing_o_s,
                        &has_missing_o_s,
                        NULL,
                        DONT_CARE);

    if (o_s == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_sub_str: input old string is null.");
        return NhlFATAL;
    }

    tmp_str = (char *) NrmQuarkToString(o_s[0]);
    tmp_o_s = (char *) NclMalloc(strlen(tmp_str) + 2);
    if (! tmp_o_s)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_o_s, tmp_str);

    n_s = (NclQuark *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        &missing_n_s,
                        &has_missing_n_s,
                        NULL,
                        DONT_CARE);

    if (n_s == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_sub_str: input new string is null.");
        return NhlFATAL;
    }

    tmp_str = (char *) NrmQuarkToString(n_s[0]);
    tmp_n_s = (char *) NclMalloc(strlen(tmp_str) + 2);
    if (! tmp_n_s)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_n_s, tmp_str);

    o_s_len = strlen(tmp_o_s);
    n_s_len = strlen(tmp_n_s);

  /*
   *printf("o_s: <%s>\n", tmp_o_s);
   *printf("n_s: <%s>\n", tmp_n_s);
   */

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    arrayString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    new_str = (char *) NclMalloc(NCL_INITIAL_STRING_LENGTH);
    if (! new_str)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    current_size = NCL_INITIAL_STRING_LENGTH;
    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
            arrayString[i] = ret_missing.stringval;
            has_missing = 1;
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(str[i]);

        if ( (has_missing_n_s && n_s[i] == missing_n_s.stringval) ||
             (has_missing_o_s && o_s[i] == missing_o_s.stringval))
        {
          arrayString[i] = NrmStringToQuark(tmp_str);
          continue;
        }

        cols = strlen(tmp_str);
/*
 * Special case where input string and o_s are both empty.
 */
        if(cols == 0 && o_s_len == 0) 
        {
	  if(n_s_len >= NCL_INITIAL_STRING_LENGTH) {
	    new_str = (char *) NclRealloc(new_str, n_s_len+1);
	  }
          strncpy(new_str,tmp_n_s,n_s_len);
          new_str[n_s_len] = '\0';
          arrayString[i] = NrmStringToQuark(new_str);
          continue;
        }
        m = cols + n_s_len - o_s_len;
        if(current_size <= m)
        {
            current_size *= 2;
            new_str = (char *) NclRealloc(new_str, current_size);
        }

        m = 0;
        n = 0;
        while(n<cols)
        {
            if(tmp_str[n] != tmp_o_s[0])
            {
                new_str[m++] = tmp_str[n++];
                continue;
            }

            nf = n;
            for(nn=0; nn<o_s_len; nn++)
            {
                if(tmp_o_s[nn] != tmp_str[n+nn])
                {
                    nf = -1;
                    break;
                }
            }

            if(nf > -1)
            {
                for(nn=0; nn<n_s_len; nn++)
                {
                    new_str[m++] = tmp_n_s[nn];
                }
                n += o_s_len;
                continue;
            }

            new_str[m++] = tmp_str[n++];
        }

        new_str[m] = '\0';

        arrayString[i] = NrmStringToQuark(new_str);
    }

    NclFree(new_str);
    NclFree(tmp_o_s);
    NclFree(tmp_n_s);

    return NclReturnValue(arrayString, ndim_str, dimsz_str, ( has_missing ? &ret_missing : NULL ), NCL_string, 0);

}


NhlErrorTypes _Nclstr_is_blank
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs = 0;
    NclScalar   missing_strs;
    NclScalar   ret_missing;
  
    char *tmp_str;
    int n;
    ng_size_t i;
    logical *tmp_val;
    ng_size_t str_sz = 1;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    str_sz = 1;
    for(i=0; i<ndim_strs; i++)
    {
        str_sz *= dimsz_strs[i];
    }

    tmp_val = (logical *) NclMalloc((unsigned int) sizeof(logical) * str_sz);
    if (! tmp_val)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    if (strs == NULL)
    {
        return NclReturnValue((void *) tmp_val, ndim_strs, dimsz_strs, NULL, NCL_logical, 0);
    }
    else
    {
        if(has_missing_strs)
        {
            ret_missing.logicalval = ((NclTypeClass) nclTypelogicalClass)->type_class.default_mis.logicalval;
            for(i=0; i<str_sz; i++)
            {
                if (has_missing_strs && strs[i] == missing_strs.stringval)
                {
                    tmp_val[i] = ret_missing.logicalval;
                }
                else
                {
                    tmp_str = (char *) NrmQuarkToString(strs[i]);
                    tmp_val[i] = 1;
                    for(n=0; n<strlen(tmp_str); n++)
                    {
                        switch(tmp_str[n])
                        {  
                            case ' ':
                            case '\t':
                            case '\n':
                            case '\f':
                            case '\r':
                            case '\v':
                                continue;
                            default:
                                tmp_val[i] = 0;
                                break;
                        }
                    }
                }
            }
            return NclReturnValue((void *) tmp_val, ndim_strs, dimsz_strs, &ret_missing, NCL_logical, 0);
        }
        else
        {
            for(i=0; i<str_sz; i++)
            {
                tmp_str = (char *) NrmQuarkToString(strs[i]);
                tmp_val[i] = 1;
                for(n=0; n<strlen(tmp_str); n++)
                {
                    switch(tmp_str[n])
                    {   
                        case ' ':
                        case '\t':
                        case '\n':
                        case '\f':
                        case '\r':
                        case '\v':
                            continue;
                        default:
                            tmp_val[i] = 0;
                            break;
                    }
                }   
            }

            return NclReturnValue((void *) tmp_val, ndim_strs, dimsz_strs, NULL, NCL_logical, 0);
        }
    }
}


NhlErrorTypes _Nclstr_left_strip
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    int m, n;
    ng_size_t str_size;
    int cols;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_left_strip: input string is null.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));
        tmp_str = result;

        m = 0;
        n = 0;
        cols = strlen(tmp_str);

        while(n<cols)
        {
            if((tmp_str[n] == ' ') || (tmp_str[n] == '\t'))
                n++;
            else
                break;
        }

        while(n<cols)
        {
            tmp_str[m++] = tmp_str[n++];
        }

        tmp_str[m] = '\0';
        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);

    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);
}


NhlErrorTypes _Nclstr_right_strip
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    ng_size_t str_size;
    int n;
    int cols;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_right_strip: input string is null.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));
        tmp_str = result;

        cols = strlen(tmp_str);

        n = cols - 1;
        while(n >= 0)
        {
            if((tmp_str[n] == ' ') || (tmp_str[n] == '\t') || (tmp_str[n] == '\n'))
                tmp_str[n--] = '\0';
            else
                break;
        }
        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);

    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);
}


NhlErrorTypes _Nclstr_strip
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    int m, n;
    ng_size_t str_size;
    int cols;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_strip: input string is null.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));
        tmp_str = result;

        cols = strlen(tmp_str);
        n = cols - 1;
        while(n >= 0)
        {
            if((tmp_str[n] == ' ') || (tmp_str[n] == '\t') || (tmp_str[n] == '\n'))
                tmp_str[n--] = '\0';
            else
                break;
        }

        m = 0;
        n = 0;
        cols = strlen(tmp_str);

        while(n<cols)
        {
            if((tmp_str[n] == ' ') || (tmp_str[n] == '\t'))
                n++;
            else
                break;
        }

        while(n<cols)
        {
            tmp_str[m++] = tmp_str[n++];
        }

        tmp_str[m] = '\0';
        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);

    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);
}


NhlErrorTypes _Nclstr_squeeze
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    ng_size_t str_size;
    int m, n;
    int cols;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_squeeze: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));
        tmp_str = result;

      /*Strip off the ending space/TAB */
        cols = strlen(tmp_str);
        n = cols - 1;
        while(n >= 0)
        {
            if((tmp_str[n] == ' ') || (tmp_str[n] == '\t') || (tmp_str[n] == '\n'))
                tmp_str[n--] = '\0';
            else
                break;
        }

      /*Strip off the leading space/TAB */
        n = 0;
        cols = strlen(tmp_str);

        while(n<cols)
        {
            if((tmp_str[n] == ' ') || (tmp_str[n] == '\t'))
                n++;
            else
                break;
        }

        if(n)
        {
            m = 0;
            while(n<cols)
            {
                tmp_str[m++] = tmp_str[n++];
            }
            tmp_str[m] = '\0';
        }

      /*Squeeze off the middle double (or more) space/TAB */
        m = 0;
        n = 0;
        cols = strlen(tmp_str);

        while(n<cols)
        {
            if(tmp_str[n] == '\t')
                tmp_str[n] = ' ';

            if(tmp_str[n] == ' ')
            {
                tmp_str[m++] = tmp_str[n++];

                while((tmp_str[n] == ' ') || (tmp_str[n] == '\t'))
                {
                    n++;
                }
            }

            tmp_str[m++] = tmp_str[n++];
        }

        tmp_str[m] = '\0';
        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);
    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);
}


NhlErrorTypes _Nclstr_index_of_substr
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;
    NclQuark *substr;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str = 0;
    int has_missing_substr = 0;
    int has_missing_range = 0;
    NclScalar   missing_str;
    NclScalar   missing_substr;
    NclScalar   missing_range;
    NclScalar   ret_missing;
  
    char *tmp_str;
    char *tmp_substr;
    int *range;
    int *index;
    int max_count = 1;
    int cur_limit = NCL_INITIAL_STRING_LENGTH;
    ng_size_t i;
    int m, n;
    int count = 0;
    int ndim_index;
    ng_size_t dimsz_index[NCL_MAX_DIMENSIONS];

    ndim_index = 1;
    dimsz_index[0] = 1;
    
    str = (NclQuark *) NclGetArgValue(
                        0,
                        3,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_index_of_substr: a NULL string is not allowed.");
        return NhlFATAL;
    }

    substr = (NclQuark *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        &missing_substr,
                        &has_missing_substr,
                        NULL,
                        DONT_CARE);

    if (substr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_index_of_subsr: input string is NULL.");
        return NhlFATAL;
    }

    range = (int *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        &missing_range,
                        &has_missing_range,
                        NULL,
                        DONT_CARE);

    if (range == NULL)
    {
        max_count = cur_limit;
    }
    else if(has_missing_range)
    {
        max_count = cur_limit;
    }
    else
    {
        if(range[0] < 0)
        {
            count = -1;
            max_count = 1;
        }
        else if(range[0] > 0)
        {
            max_count = range[0];
        }
        else
            max_count = cur_limit;
    }

    ret_missing.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
    index = NclMalloc((unsigned int) sizeof(int) * max_count);
    if (! index)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    index[0] = ret_missing.intval;

    tmp_str = (char *) NrmQuarkToString(str[0]);

    tmp_substr = (char *) NrmQuarkToString(substr[0]);

  /*
   *printf("str: <%s>\n", tmp_str);
   *printf("substr: <%s>\n", tmp_substr);
   */

    if((has_missing_str    && str[0]    == missing_str.stringval) || 
       (has_missing_substr && substr[0] == missing_substr.stringval) ||
       (strlen(tmp_substr) > strlen(tmp_str)))
    {
        dimsz_index[0] = 1;
        return NclReturnValue((void *) index, ndim_index, dimsz_index, &ret_missing, NCL_int, 0);
    }

    if(count < 0)
    {
        count = 1;
        for(i=strlen(tmp_str) - strlen(tmp_substr); i>=0; i--)
        {
            if(tmp_substr[0] == tmp_str[i])
            {
                m = 1;
                for(n=1; n<strlen(tmp_substr); n++)
                {
                  /*
                   *printf("str[%d]: <%c>\n", i+n, tmp_str[i+n]);
                   *printf("substr[%d]: <%c>\n", n, tmp_substr[n]);
                   */
            
                    if(tmp_substr[n] != tmp_str[i+n])
                    {   
                        m = 0;
                        break;
                    }
                }

                if(m)
                {
                    index[0] = i;
                    break;
                }
            }
        }

        dimsz_index[0] = 1;
        return NclReturnValue((void *) index, ndim_index, dimsz_index, &ret_missing, NCL_int, 0);
    }

    count = 0;
    i=0;
    while(i<strlen(tmp_str) - strlen(tmp_substr) + 1)
    {
      /*
       *printf("str[%d]: <%c>\n", i, tmp_str[i]);
       *printf("substr[%d]: <%c>\n", 0, tmp_substr[0]);
       */
            
        if(tmp_substr[0] == tmp_str[i])
        {
            m = 1;
            for(n=1; n<strlen(tmp_substr); n++)
            {
                if(tmp_substr[n] != tmp_str[i+n])
                {   
                    m = 0;
                    break;
                }
            }

            if(m)
            {
                index[count++] = i;
                if(count >= max_count)
                {
                    if((cur_limit - count) > 0)
                    {
                        break;
                    }

                    cur_limit += NCL_INITIAL_STRING_LENGTH;
                    max_count = cur_limit;
                    index = (int *) NclRealloc(index, sizeof(int) * max_count);
                }
                i += strlen(tmp_substr);
                continue;
            }
        }
        i++;
    }

    if(count < 1)
        count = 1;

    index = (int *) NclRealloc(index, sizeof(int) * count);
    dimsz_index[0] = count;
    return NclReturnValue((void *) index, ndim_index, dimsz_index, &ret_missing, NCL_int, 0);
}

NhlErrorTypes _Nclstr_upper
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    int n;
    ng_size_t str_size;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_squeeze: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));

        max_length = strlen(result);
        for(n=0; n<max_length; n++)
        {
            if((result[n] >= 'a') && (result[n] <= 'z'))
                result[n] += 'A' - 'a';
        }

        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);
    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);

}


NhlErrorTypes _Nclstr_lower
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    int n;
    ng_size_t str_size;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_squeeze: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));

        max_length = strlen(result);
        for(n=0; n<max_length; n++)
        {
            if((result[n] >= 'A') && (result[n] <= 'Z'))
                result[n] += 'a' - 'A';
        }

        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);
    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);

}


NhlErrorTypes _Nclstr_switch
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    char *tmp_str;
    ng_size_t i;
    int n;
    ng_size_t str_size;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_squeeze: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result, (char *) NrmQuarkToString(str[i]));

        max_length = strlen(result);
        for(n=0; n<max_length; n++)
        {
            if((result[n] >= 'a') && (result[n] <= 'z'))
                result[n] += 'A' - 'a';
            else if((result[n] >= 'A') && (result[n] <= 'Z'))
                result[n] += 'a' - 'A';
        }

        arrayOfString[i] = NrmStringToQuark(result);
    }

    NclFree(result);
    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);

}


NhlErrorTypes _Nclstr_capital
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    ng_size_t i;
    int n;
    ng_size_t str_size;

    NclQuark *arrayOfString;
    char *result;
    int max_length = 0;
    int len;
    int capitalize = 1;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_squeeze: input string is null.");
        return NhlFATAL;
    }


    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];


    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
	len = strlen((char *) NrmQuarkToString(str[i]));
        if (max_length < len)
	    max_length = len;
    }
    max_length ++;

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_str && str[i] == missing_str.stringval)
        {
           arrayOfString[i] = str[i];
           has_missing = 1;
           continue;
        }

        strcpy(result,NrmQuarkToString(str[i]));

        capitalize = 1;
        len = strlen(result);
        for(n=0; n<len; n++)
        {
            switch (result[n])
            {
                case ' ':
                case '\t':
                case '\n':
                case '\f':
                case '\r':
                case '\v':
                    capitalize = 1;
                    break;
                default:
                    if(capitalize)
                    {
                        if((result[n] >= 'a') && (result[n] <= 'z'))
                            result[n] += 'A' - 'a';
                    }
                    capitalize = 0;
            }
        }

        arrayOfString[i] = NrmStringToQuark(result);
    }
    NclFree(result);
    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);

}


NhlErrorTypes _Nclstr_join
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;
    NclQuark *delim;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_delim;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   missing_delim;
    NclScalar   ret_missing;
  
    ng_size_t i;
    int n;

    int ndim;
    ng_size_t dimsz[1];
    char *result;

    char *tmp_str;
    char *tmp_delim;
    NclQuark *new_string;
    ng_size_t str_size;
    ng_size_t max_length = 1;
    ng_size_t total_length = 0;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_join: input string is null.");
        return NhlFATAL;
    }

    delim = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_delim,
                        &has_missing_delim,
                        NULL,
                        DONT_CARE);

    if (delim == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_join: delimiter is null.");
        return NhlFATAL;
    }

    if(has_missing_strs)
        ret_missing.stringval = missing_strs.stringval;
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_strs; i++)
        str_size *= dimsz_strs[i];

    tmp_str = (char *) NrmQuarkToString(delim[0]);
    n = strlen(tmp_str) + 2;
    tmp_delim = (char *) NclMalloc(n);
    if (! tmp_delim)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_delim, (char *) NrmQuarkToString(delim[0]));

    total_length = str_size * n;
    if(tmp_delim[0] < 1)
        strcpy(tmp_delim, " ");

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(strs[i]);
        total_length += strlen(tmp_str) + 1;
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;

    result = (char *) NclMalloc(total_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (i)
            strcat(result, tmp_delim);

        if (has_missing_strs && strs[i] == missing_strs.stringval)
        {
            has_missing_ret = 1;
        }
        if (i)
            strcat(result, (char *) NrmQuarkToString(strs[i]));
        else
            strcpy(result, (char *) NrmQuarkToString(strs[i]));
    }

    new_string[0] = NrmStringToQuark(result);

    NclFree(tmp_delim);
    NclFree(result);

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, (has_missing_ret ? &ret_missing : NULL), NCL_string, 0);

}


NhlErrorTypes _Nclstr_concat
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   ret_missing;
  
    ng_size_t i;

    int ndim;
    ng_size_t dimsz[1];
    char *result;

    char *tmp_str;
    NclQuark *new_string;
    ng_size_t str_size;
    ng_size_t max_length = 1;
    ng_size_t total_length = 0;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_concat: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_strs)
        ret_missing.stringval = missing_strs.stringval;
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_strs; i++)
        str_size *= dimsz_strs[i];

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(strs[i]);
        total_length += strlen(tmp_str) + 1;
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length ++;
    total_length += str_size;

    result = (char *) NclMalloc(total_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_strs && strs[i] == missing_strs.stringval)
        {
            has_missing_ret = 1;
        }
        if (i)
            strcat(result, (char *) NrmQuarkToString(strs[i]));
        else
            strcpy(result, (char *) NrmQuarkToString(strs[i]));
    }

    new_string[0] = NrmStringToQuark(result);

    NclFree(result);

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, (has_missing_ret ? &ret_missing : NULL), NCL_string, 0);

}

NhlErrorTypes _Nclstr_insert
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *strs;
    NclQuark *insert;

    int ndim_strs;
    ng_size_t dimsz_strs[NCL_MAX_DIMENSIONS];
    int has_missing_strs;
    int has_missing_ret = 0;
    NclScalar   missing_strs;
    NclScalar   ret_missing;
  
    ng_size_t i;
    int j, m, n;

    char *tmp_str;
    char *tmp_insert;
    char *result;
    NclQuark *new_string;
    ng_size_t str_size;
    int *position;
    ng_size_t max_length = 0;
    
    strs = (NclQuark *) NclGetArgValue(
                        0,
                        3,
                        &ndim_strs,
                        dimsz_strs,
                        &missing_strs,
                        &has_missing_strs,
                        NULL,
                        DONT_CARE);

    if (strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_insert: input string is null.");
        return NhlFATAL;
    }

    insert = (NclQuark *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    if (insert == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_insert: insert is null.");
        return NhlFATAL;
    }

    position = (int *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    if (position == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_insert: position is null.");
        return NhlFATAL;
    }

    if(has_missing_strs)
        ret_missing.stringval = missing_strs.stringval;
    else
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

    str_size = 1;
    for(i=0; i<ndim_strs; i++)
        str_size *= dimsz_strs[i];

    tmp_str = (char *) NrmQuarkToString(insert[0]);
    tmp_insert = (char *) NclMalloc(strlen(tmp_str)+2);
    if (! tmp_insert)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    strcpy(tmp_insert, (char *) NrmQuarkToString(insert[0]));

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(strs[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);
    }
    max_length += 1+abs(position[0]) + strlen(tmp_insert);

    new_string = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    result = (char *) NclMalloc(max_length);
    if (! result)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    tmp_str = (char *) NclMalloc(max_length);
    if (! tmp_str)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if (has_missing_strs && strs[i] == missing_strs.stringval)
        {
            has_missing_ret = 1;
            new_string[i] = ret_missing.stringval;
        }
        else
        {
            strcpy(tmp_str, (char *) NrmQuarkToString(strs[i]));

            n = 0;
            if(position[0] >= 0)
            {
                m = position[0];
                if(m < strlen(tmp_str))
                {
                    for(j = 0; j < m; j++)
                        result[n++] = tmp_str[j];
                    for(j = 0; j < strlen(tmp_insert); j++)
                        result[n++] = tmp_insert[j];
                    for(j = m; j < strlen(tmp_str); j++)
                        result[n++] = tmp_str[j];
                }
                else
                {
                    for(j = 0; j < strlen(tmp_str); j++)
                        result[n++] = tmp_str[j];
                    for(j = strlen(tmp_str); j < m; j++)
                        result[n++] = ' ';
                    for(j = 0; j < strlen(tmp_insert); j++)
                        result[n++] = tmp_insert[j];
                }
            }
            else
            {
                m = strlen(tmp_str)+position[0];
                if(m >= 0)
                {
                    for(j = 0; j <= m; j++)
                        result[n++] = tmp_str[j];
                    for(j = 0; j < strlen(tmp_insert); j++)
                        result[n++] = tmp_insert[j];
                    for(j = m+1; j < strlen(tmp_str); j++)
                        result[n++] = tmp_str[j];
                }
                else
                {
                    for(j = 0; j < strlen(tmp_insert); j++)
                        result[n++] = tmp_insert[j];
                    for(j = m; j < 0; j++)
                        result[n++] = ' ';
                    for(j = 0; j < strlen(tmp_str); j++)
                        result[n++] = tmp_str[j];
                }      
            }
            result[n] = '\0';
            new_string[i] = NrmStringToQuark(result);
        }
    }

    NclFree(result);
    NclFree(tmp_insert);
    NclFree(tmp_str);

    return NclReturnValue(new_string, ndim_strs, dimsz_strs, (has_missing_ret ? &ret_missing : NULL), NCL_string, 0);

}

NhlErrorTypes _Nclstr_get_comma
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark(",");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_get_space
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark(" ");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_get_tab
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark("\t");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_get_sq
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark("\'");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_get_dq
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark("\"");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_get_nl
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark("\n");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_get_cr
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark("\r");

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclshow_ascii
#if     NhlNeedProto
(void)
#else
()
#endif
{
    char percent = '\%';
    fprintf(stderr, "\t\t\tThe decimal set:\n");
    fprintf(stderr, "       0 nul    1 soh    2 stx    3 etx    4 eot    5 enq    6 ack    7 bel\n");
    fprintf(stderr, "       8 bs     9 ht    10 nl    11 vt    12 np    13 cr    14 so    15 si\n");
    fprintf(stderr, "      16 dle   17 dc1   18 dc2   19 dc3   20 dc4   21 nak   22 syn   23 etb\n");
    fprintf(stderr, "      24 can   25 em    26 sub   27 esc   28 fs    29 gs    30 rs    31 us\n");
    fprintf(stderr, "      32 sp    33  !    34  \"    35  #    36  $    37  %c    38  &    39  \'\n", percent);
    fprintf(stderr, "      40  (    41  )    42  *    43  +    44  ,    45  -    46  .    47  /\n");
    fprintf(stderr, "      48  0    49  1    50  2    51  3    52  4    53  5    54  6    55  7\n");
    fprintf(stderr, "      56  8    57  9    58  :    59  ;    60  <    61  =    62  >    63  ?\n");
    fprintf(stderr, "      64  @    65  A    66  B    67  C    68  D    69  E    70  F    71  G\n");
    fprintf(stderr, "      72  H    73  I    74  J    75  K    76  L    77  M    78  N    79  O\n");
    fprintf(stderr, "      80  P    81  Q    82  R    83  S    84  T    85  U    86  V    87  W\n");
    fprintf(stderr, "      88  X    89  Y    90  Z    91  [    92  \\    93  ]    94  ^    95  _\n");
    fprintf(stderr, "      96  `    97  a    98  b    99  c   100  d   101  e   102  f   103  g\n");
    fprintf(stderr, "     104  h   105  i   106  j   107  k   108  l   109  m   110  n   111  o\n");
    fprintf(stderr, "     112  p   113  q   114  r   115  s   116  t   117  u   118  v   119  w\n");
    fprintf(stderr, "     120  x   121  y   122  z   123  {   124  |   125  }   126  ~   127 del\n");

    return(NhlNOERROR);
}

NhlErrorTypes _Nclstr_from_int
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int    *in;

    int ndim_n;
    ng_size_t dimsz_n[NCL_MAX_DIMENSIONS];
    int has_missing_n;
    NclScalar   missing_n;

    int ndim;
    ng_size_t dimsz[1];
    NclQuark *new_string;
    char cs[2];
  
    in = (int *) NclGetArgValue(
                        0,
                        1,
                        &ndim_n,
                        dimsz_n,
                        &missing_n,
                        &has_missing_n,
                        NULL,
                        DONT_CARE);

    if (in == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_from_int: input in is null.");
        return NhlFATAL;
    }

    cs[0] = in[0];
    cs[1] = '\0';

    new_string = (NclQuark *) NclMalloc(sizeof(NclQuark));
    if (! new_string)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    new_string[0] = NrmStringToQuark(cs);

    ndim = 1;
    dimsz[0] = 1;
    return NclReturnValue(new_string, ndim, dimsz, NULL, NCL_string, 0);

    NclFree(new_string);
}

NhlErrorTypes _Nclstr_match
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    char *tmp_exp;
    NclQuark *output_strs;
    ng_size_t i;
    ng_size_t str_size;
    ng_size_t output_str_size = 0;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match: input string is null.");
        return NhlFATAL;
    }

    if(type != NCL_string)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match: Invalid input string.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        ret_missing.stringval = missing_input_strs.stringval;
        has_missing = 1;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match: input expression is null.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_strs = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! output_strs)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    tmp_exp = (char *) NrmQuarkToString(input_expr[0]);
  /*
   *fprintf(stderr, "\tinput_expr: <%s>\n", tmp_exp);
   */

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(NULL != strstr(tmp_str, tmp_exp))
        {
            output_strs[output_str_size] = input_strs[i];
          /*
           *fprintf(stderr, "\toutput_strs[%d]: <%s>\n",
           *        output_str_size, NrmQuarkToString(output_strs[output_str_size]));
           */
            output_str_size ++;
        }
    }

    if(output_str_size)
        output_strs = (NclQuark *) NclRealloc(output_strs, output_str_size*sizeof(NclQuark));
    else
    {
        has_missing = 1;
        output_strs = (NclQuark *) NclRealloc(output_strs, sizeof(NclQuark));
        output_strs[output_str_size] = ret_missing.stringval;
      /*
       *output_strs[output_str_size] = NrmStringToQuark("NO MATCH");
       */
        output_str_size = 1;
    }

    return NclReturnValue(output_strs, 1, &output_str_size, ( has_missing ? &ret_missing : NULL ), NCL_string, 0);
}

NhlErrorTypes _Nclstr_match_ic
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    char *tmp_exp;
    NclQuark *output_strs;
    ng_size_t i, n;
    ng_size_t str_size;
    ng_size_t output_str_size = 0;

    ng_size_t nstr = 1024;
    char *low_str;
    char *low_exp;

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: input string is null.");
        return NhlFATAL;
    }

    if(type != NCL_string)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Invalid input string.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        ret_missing.stringval = missing_input_strs.stringval;
        has_missing = 1;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: input expression is null.");
        return NhlFATAL;
    }

    tmp_exp = (char *) NrmQuarkToString(input_expr[0]);

    low_exp = (char *)NclMalloc((strlen(tmp_exp) + 1) * sizeof(char));
    if(NULL == low_exp)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Cannot allocate memory for working string.");
        return NhlFATAL;
    }

    memset(low_exp, 0, strlen(tmp_exp) + 1);

    for(n=0; n<strlen(tmp_exp); n++)
    {
        if((tmp_exp[n] >= 'A') && (tmp_exp[n] <= 'Z'))
            low_exp[n] = tmp_exp[n] + 'a' - 'A';
        else
            low_exp[n] = tmp_exp[n];
    }

    low_str = (char *)NclMalloc(nstr * sizeof(char));
    if(NULL == low_str)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Cannot allocate memory for working string.");
        return NhlFATAL;
    }

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tlower case input_expr: <%s>\n", low_exp);
   */

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_strs = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! output_strs)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);

      /*
       *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tlower case input_str[%d]: <%s>\n", i, low_str);
       */

        if(nstr <= strlen(tmp_str))
        {
            nstr = 1 + strlen(tmp_str);
            low_str = (char *)NclRealloc(low_str, nstr * sizeof(char));
            if(NULL == low_str)
            {
                NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Cannot allocate memory for working string.");
                return NhlFATAL;
            }
        }

        memset(low_str, 0, nstr);

        for(n=0; n<strlen(tmp_str); n++)
        {
            if((tmp_str[n] >= 'A') && (tmp_str[n] <= 'Z'))
                low_str[n] = tmp_str[n] + 'a' - 'A';
            else
                low_str[n] = tmp_str[n];
        }

        if(NULL != strstr(low_str, low_exp))
        {
            output_strs[output_str_size] = input_strs[i];
          /*
           *fprintf(stderr, "\toutput_strs[%d]: <%s>\n",
           *        output_str_size, NrmQuarkToString(output_strs[output_str_size]));
           */
            output_str_size ++;
        }
    }

    NclFree(low_str);
    NclFree(low_exp);

    if(output_str_size)
        output_strs = (NclQuark *) NclRealloc(output_strs, output_str_size*sizeof(NclQuark));
    else
    {
        has_missing = 1;
        output_strs = (NclQuark *) NclRealloc(output_strs, sizeof(NclQuark));
        output_strs[output_str_size] = ret_missing.stringval;
      /*
       *output_strs[output_str_size] = NrmStringToQuark("NO MATCH");
       */
        output_str_size = 1;
    }

    return NclReturnValue(output_strs, 1, &output_str_size, ( has_missing ? &ret_missing : NULL ), NCL_string, 0);
}

NhlErrorTypes _Nclstr_match_ind
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    char *tmp_exp;
    int *output_inds;
    ng_size_t i;
    ng_size_t str_size;
    ng_size_t output_ind_size = 0;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        has_missing = 1;
    }
    ret_missing.intval =  (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind: input expression is null.");
        return NhlFATAL;
    }

    tmp_exp = (char *) NrmQuarkToString(input_expr[0]);
  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tinput_expr: <%s>\n", tmp_exp);
   */

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_inds = (int *) NclMalloc(str_size*sizeof(int));
    if (! output_inds)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(NULL != strstr(tmp_str, tmp_exp))
        {
            output_inds[output_ind_size] = i;
            output_ind_size ++;
        }
    }

    if(output_ind_size)
        output_inds = (int *) NclRealloc(output_inds, output_ind_size*sizeof(int));
    else
    {
        has_missing = 1;
        output_inds = (int *) NclRealloc(output_inds, sizeof(int));
        output_inds[output_ind_size] = ret_missing.intval;
      /*
       *output_inds[output_ind_size] = NrmStringToQuark("NO MATCH");
       */
        output_ind_size = 1;
    }

    return NclReturnValue(output_inds, 1, &output_ind_size, ( has_missing ? &ret_missing : NULL ), NCL_int, 0);
}

NhlErrorTypes _Nclstr_match_ind_ic
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    char *tmp_exp;
    int *output_inds;
    ng_size_t i, n;
    ng_size_t str_size;
    ng_size_t output_ind_size = 0;

    ng_size_t nstr = 1024;
    char *low_str;
    char *low_exp;

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_ic: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        has_missing = 1;
    }
    ret_missing.intval =  (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_ic: input expression is null.");
        return NhlFATAL;
    }

    tmp_exp = (char *) NrmQuarkToString(input_expr[0]);

    low_exp = (char *)NclMalloc((strlen(tmp_exp) + 1) * sizeof(char));
    if(NULL == low_exp)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Cannot allocate memory for working string.");
        return NhlFATAL;
    }

    memset(low_exp, 0, strlen(tmp_exp) + 1);

    for(n=0; n<strlen(tmp_exp); n++)
    {
        if((tmp_exp[n] >= 'A') && (tmp_exp[n] <= 'Z'))
            low_exp[n] = tmp_exp[n] + 'a' - 'A';
        else
            low_exp[n] = tmp_exp[n];
    }

    low_str = (char *)NclMalloc(nstr * sizeof(char));
    if(NULL == low_str)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Cannot allocate memory for working string.");
         return NhlFATAL;
     }
 
  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tlower case input_expr: <%s>\n", low_exp);
   */

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_inds = (int *) NclMalloc(str_size*sizeof(int));
    if (! output_inds)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(nstr <= strlen(tmp_str))
        {
            nstr = 101 + strlen(tmp_str);
            low_str = (char *)NclRealloc(low_str, nstr * sizeof(char));
            if(NULL == low_str)
            {
                NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic: Cannot allocate memory for working string.");
                return NhlFATAL;
            }
        }

        memset(low_str, 0, nstr);

        for(n=0; n<strlen(tmp_str); n++)
        {
            if((tmp_str[n] >= 'A') && (tmp_str[n] <= 'Z'))
                low_str[n] = tmp_str[n] + 'a' - 'A';
            else
                low_str[n] = tmp_str[n];
        }

        if(NULL != strstr(low_str, low_exp))
        {
            output_inds[output_ind_size] = i;
            output_ind_size ++;
        }
    }

    if(output_ind_size)
        output_inds = (int *) NclRealloc(output_inds, output_ind_size*sizeof(int));
    else
    {
        has_missing = 1;
        output_inds = (int *) NclRealloc(output_inds, sizeof(int));
        output_inds[output_ind_size] = ret_missing.intval;
      /*
       *output_inds[output_ind_size] = NrmStringToQuark("NO MATCH");
       */
        output_ind_size = 1;
    }

    NclFree(low_str);
    NclFree(low_exp);

    return NclReturnValue(output_inds, 1, &output_ind_size, ( has_missing ? &ret_missing : NULL ), NCL_int, 0);
}

NhlErrorTypes _Nclstr_sort
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark *str;

    int ndim_str;
    ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
    int has_missing_str;
    int has_missing = 0;
    NclScalar   missing_str;
    NclScalar   ret_missing;
  
    ng_size_t i,j;
    int n;
    ng_size_t str_size;

    NclQuark *arrayOfString;
    char **sa;
    char *tmp_str;
    int max_length = 0;

    str = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        NULL,
                        DONT_CARE);

    if (str == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_squeeze: input string is null.");
        return NhlFATAL;
    }


    if(has_missing_str)
    {
        ret_missing.stringval = missing_str.stringval;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    str_size = 1;
    for(i=0; i<ndim_str; i++)
        str_size *= dimsz_str[i];

    arrayOfString = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! arrayOfString)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        tmp_str = (char *) NrmQuarkToString(str[i]);
        if (max_length < strlen(tmp_str))
            max_length = strlen(tmp_str);

        if (has_missing_str && str[i] == missing_str.stringval)
        {
           has_missing = 1;
        }
    }
    max_length ++;

    tmp_str = (char *) NclMalloc(max_length);
    if (! tmp_str)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    sa = (char **)  NclMalloc(str_size * sizeof(char *));
    if (! sa)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }
    for(i=0; i<str_size; i++)
    {
        sa[i] = (char *) NclMalloc(max_length);
        if (! sa[i])
        {
            NHLPERROR((NhlFATAL,ENOMEM,NULL));
            return NhlFATAL;
        }
        strcpy(sa[i], (char *) NrmQuarkToString(str[i]));
    }

    for(i=0; i<str_size; i++)
    {
        for(j=i+1; j<str_size; j++)
        {
            n = strcmp(sa[i], sa[j]);
            if(n < 0)
            {
                strcpy(tmp_str, sa[i]);
                strcpy(sa[i], sa[j]);
                strcpy(sa[j], tmp_str);
            }
        }
        arrayOfString[i] = NrmStringToQuark(sa[i]);
        free(sa[i]);
    }

    NclFree(tmp_str);
    NclFree(sa);
    return NclReturnValue(arrayOfString, ndim_str, dimsz_str, (has_missing ? &ret_missing : NULL), NCL_string, 0);

}

NhlErrorTypes process_list(FILE *fp, obj *list_id, char *fmtstr, int *ndvdl, int output)
{
    NclVar  thevar;
    NclList thelist = NULL;
    NclMultiDValData thevalue = NULL;

    size_t i = 0;
    int nelems = 0;
    int maxelems = MAX_LIST_ELEMENT;
    int truelems = 0;
    NclBasicDataTypes type[MAX_LIST_ELEMENT];
    char              prefix[NCL_INITIAL_STRING_LENGTH];
    char              format[MAX_LIST_ELEMENT][MAX_PRINT_NAME_LENGTH];
    size_t            size[MAX_LIST_ELEMENT];
    size_t maxlen = 0;

    char  name[MAX_LIST_ELEMENT][MAX_PRINT_NAME_LENGTH];
    char *tmp_sp = NULL;
    char *tmp = NULL;
    char *result = NULL;

    NclListObjList *step;
    NclObj          theobj;
    NclObj          cur_obj;

    char prntln[NCL_INITIAL_STRING_LENGTH];
    char buffer[NCL_INITIAL_STRING_LENGTH];
    char separator[MAX_LIST_ELEMENT][MAX_PRINT_NAME_LENGTH];

    int nstart, length, remain;
    int has_separator[MAX_LIST_ELEMENT];

    length = 1 + strlen(fmtstr);
    tmp = (char*)NclCalloc(length, sizeof(char));
    strcpy(tmp, fmtstr);

    tmp_sp = tmp;

    if('%' != fmtstr[0])
    {
        strcpy(prefix, tmp);
        tmp = strchr(tmp, '%');
        i = strlen(prefix) - strlen(tmp);
        prefix[i] = '\0';
    }
    else
        prefix[0] = '\0';

    theobj = _NclGetObj(*list_id);
    thevar = (NclVar)theobj;
    thelist = (NclList) theobj;

    nelems = 0;
    memset(format, 0, MAX_LIST_ELEMENT * MAX_PRINT_NAME_LENGTH);
    result = strtok(tmp, "%");
    while(result != NULL)
    {
        if(MAX_LIST_ELEMENT > nelems)
        {
            strcpy(format[nelems], "%");
            strcat(format[nelems], result);
        }
        else
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: can only handle list less than %d elements.\n",
                                            "process_list", MAX_LIST_ELEMENT);
            return(NhlFATAL);
        }

        result = strtok(NULL, "%");
        ++nelems;
    }

    maxelems = nelems;

    nelems = 0;
    step = thelist->list.first;
    while(step != NULL)
    {
        cur_obj = (NclObj)_NclGetObj(step->obj_id);

        switch(cur_obj->obj.obj_type)
        {
            case Ncl_Var:
            case Ncl_FileVar:
                 theobj = _NclGetObj(cur_obj->obj.id);
                 thevar = (NclVar)theobj;

                 if(thevar->var.thesym != NULL)
                     strcpy(name[nelems], thevar->var.thesym->name);
                 else if(thevar->var.var_quark != -1)
                     strcpy(name[nelems], NrmQuarkToString(thevar->var.var_quark));
                 else
                     strcpy(name[nelems], "unnamed");

                 thevalue = (NclMultiDValData)_NclGetObj(thevar->var.thevalue_id);
                 if(thevalue->obj.obj_type_mask & Ncl_MultiDValnclfileData)
                 {
                   /*
                    *fprintf(stderr, "Type: file\n");
                    *if (thevalue->multidval.missing_value.has_missing &&
                    *    *(obj*)thevalue->multidval.val == thevalue->multidval.missing_value.value.objval) {
                    *    nclfprintf(stderr, "(0) File Missing Value : %d\n",*(obj*)thevalue->multidval.val);
                    *}
                    *else {
                    *    NclObj file = _NclGetObj(*(int*)thevalue->multidval.val);
                    *    _NclPrintFileSummary(file,fp);
                    *}
                    */
         
                     NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: can not print file list yet.\n", "process_list");
                     return(NhlFATAL);
                 }
                 else if(thevalue->obj.obj_type_mask & Ncl_MultiDVallistData)
                 {
                     NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: can not print list in list yet.\n", "process_list");
                     return(NhlFATAL);
                 }
                 else
                 {
                     type[nelems] = NCL_none;
                     size[nelems] = 0;
                     if(thevalue != NULL)
                     {
                         type[nelems] = thevalue->multidval.data_type;
                         if(thevalue->multidval.totalelements > maxlen)
                             maxlen = thevalue->multidval.totalelements;
                         size[nelems] = thevalue->multidval.totalelements;
         
                     }
                 }

                 break;
            default:
                 fprintf(stderr, "\tin file: %s, line: %d\n", __FILE__, __LINE__);
                 fprintf(stderr, "\tUNRECOGNIZED cur_obj->obj.obj_type %d: %o\n", nelems, cur_obj->obj.obj_type);
        }

        has_separator[nelems] = 0;
        length = strlen(format[nelems]) - 1;

        if(! isalpha(format[nelems][length]))
        {
            has_separator[nelems] = 1;
            strcpy(separator[nelems], format[nelems] + length);
        }

        step = step->next;
        ++nelems;
    }

    if(maxelems > 1)
    {
        nelems = maxelems - 2;
        if(has_separator[nelems])
        {
            has_separator[nelems + 1] = 1;
            strcpy(separator[nelems + 1], " ");
        }
    }

    theobj = (NclObj)_NclGetObj(*list_id);
    thelist = (NclList) theobj;
    truelems = (int)thelist->list.nelem;

    if(maxelems > truelems)
       maxelems = truelems;

    /*Print value of elements*/
    if(! output)
    {
        for(nelems = 0; nelems < maxelems; ++nelems)
            ndvdl[nelems] = 0;
    }

    for(i = 0; i < maxlen; ++i)
    {
        memset(prntln, 0, NCL_INITIAL_STRING_LENGTH);
        if('\0' == prefix[0])
        {
            prntln[0] = '\0';
            nstart = 0;
        }
        else
        {
            strcpy(prntln, prefix);
            nstart = strlen(prefix);
        }

        step = thelist->list.first;
        for(nelems = 0; nelems < maxelems; ++nelems)
        {
            memset(buffer, 0, NCL_INITIAL_STRING_LENGTH);
            if(i < size[nelems])
            {
                cur_obj = (NclObj)_NclGetObj(step->obj_id);

                theobj = _NclGetObj(cur_obj->obj.id);
                thevar = (NclVar)theobj;

                thevalue = (NclMultiDValData)_NclGetObj(thevar->var.thevalue_id);

                if(thevalue != NULL)
                {
                    switch (type[nelems])
                    {
                        case NCL_string:
                             {
                                 NclQuark *sp = (NclQuark *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], NrmQuarkToString(*(sp + i)));
                             }
                             break;
                        case NCL_float:
                             {
                                 float *fp = (float *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(fp + i));
                             }
                             break;
                        case NCL_double:
                             {
                                 double *dp = (double *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(dp + i));
                             }
                             break;
                        case NCL_int64:
                             {
                                 long long *llp = (long long *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(llp + i));
                             }
                             break;
                        case NCL_uint64:
                             {
                                 unsigned long long *llp = (unsigned long long *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(llp + i));
                             }
                             break;
                        case NCL_long:
                             {
                                 long *lp = (long *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(lp + i));
                             }
                             break;
                        case NCL_ulong:
                             {
                                 unsigned long *lp = (unsigned long *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(lp + i));
                             }
                             break;
                        case NCL_int:
                             {
                                 int *ip = (int *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(ip + i));
                             }
                             break;
                        case NCL_uint:
                             {
                                 unsigned int *ip = (unsigned int *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(ip + i));
                             }
                             break;
                        case NCL_short:
                             {
                                 short *sp = (short *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(sp + i));
                             }
                             break;
                        case NCL_ushort:
                             {
                                 unsigned short *sp = (unsigned short *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(sp + i));
                             }
                             break;
                        case NCL_byte:
                             {
                                 char *cp = (char *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(cp + i));
                             }
                             break;
                        case NCL_char:
                             {
                                 unsigned char *cp = (unsigned char *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], *(cp + i));
                             }
                             break;
                        case NCL_logical:
                             {
                                 logical *lp = (logical *) thevalue->multidval.val;
                                 sprintf(buffer, format[nelems], (*(lp + i) ? "True" : "False"));
                             }
                             break;
                        default:
                             memset(buffer, ' ', ndvdl[nelems]);
                             buffer[ndvdl[nelems]] = '\0';

                             fprintf(stderr, "\tin file: %s, line: %d\n", __FILE__, __LINE__);
                             fprintf(stderr, "\tUNRECOGNIZED thevalue->multidval.data_type 0x%o\n", thevalue->multidval.data_type);
                    }
                }

                length = strlen(buffer);

                if(length > ndvdl[nelems])
                    ndvdl[nelems] = length;

                if(has_separator[nelems])
                {
                    strcat(prntln, buffer);
                    nstart += length;
                }
                else
                {
                    remain = ndvdl[nelems] - length;

                    if(remain)
                    {
#if 0
                       memset(prntln + nstart, ' ', remain);
                       prntln[nstart + remain] = '\0';
                       strcat(prntln, buffer);
#else
                       if(nelems)
                       {
                           strcat(prntln, " ");
                           strcat(prntln, buffer);
                           memset(prntln + length + 2 + nstart, ' ', remain + 1);
                           prntln[nstart + length + 2 + remain] = '\0';
                       }
                       else
                       {
                           strcat(prntln, buffer);
                           memset(prntln + length + 1 + nstart, ' ', remain + 1);
                           prntln[nstart + length + 1 + remain] = '\0';
                       }
#endif
                       nstart += ndvdl[nelems];
                    }
                    else
                    {
                       if(nelems)
                       {
                           strcat(prntln, " ");
                           strcat(prntln, buffer);
                       }
                       else
                       {
                           strcat(prntln, buffer);
                       }
                       nstart += ndvdl[nelems];
                    }
                }
            }
            else
            {
                memset(buffer, ' ', ndvdl[nelems] + 1);
                buffer[ndvdl[nelems]] = '\0';

                if(has_separator[nelems])
                {
                    buffer[ndvdl[nelems] - strlen(separator[nelems])] = '\0';
                    strcat(buffer, separator[nelems]);
                }
                else
                    if(nelems)
                        strcat(buffer, " ");
                strcat(prntln, buffer);
                nstart += ndvdl[nelems];
            }

            step = step->next;
        }

        if(output)
            fprintf(fp, "%s\n", prntln);
    }

  /*
   */
    if(NULL != tmp_sp)
        NclFree(tmp_sp);

    return(NhlNOERROR);
}

NhlErrorTypes _Nclprint_table(void)
{
    obj *list_id;

    NclQuark *qformat;
    char   *format;

    int ndvdl[MAX_LIST_ELEMENT];

    FILE *fp = _NclGetOutputStream();

  /*
   *fprintf(stderr, "\nEnter _Nclprint_table, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    list_id = (obj *)NclGetArgValue(
               0,
               2,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               DONT_CARE);

    qformat = (NclQuark *)NclGetArgValue(
              1,
              2,
              NULL,
              NULL,
              NULL,
              NULL,
              NULL,
              DONT_CARE);

    format = NrmQuarkToString(*qformat);
    if(NULL == format)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclprint_table: unknown format.\n");
        return(NhlFATAL);
    }

    process_list(fp, list_id, format, ndvdl, 0);
    process_list(fp, list_id, format, ndvdl, 1);

  /*
   *fprintf(stderr, "Leave _Nclprint_table, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return(NhlNOERROR);
}

NhlErrorTypes _Nclwrite_table(void)
{
    obj *list_id;

    NclQuark *qformat;
    NclQuark *qfilename;
    NclQuark *qoption;

    char *format;
    char *filename;
    char *option;

    FILE *fp = NULL;

    int ndvdl[MAX_LIST_ELEMENT];

  /*
   *fprintf(stderr, "\nEnter _Nclwrite_table, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    qfilename = (NclQuark *)NclGetArgValue(
              0,
              4,
              NULL,
              NULL,
              NULL,
              NULL,
              NULL,
              DONT_CARE);

    filename = NrmQuarkToString(*qfilename);
    if(NULL == filename)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclwrite_table: unknown filename.\n");
        return(NhlFATAL);
    }

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfilename = <%s>\n", filename);
   */

    qoption = (NclQuark *)NclGetArgValue(
              1,
              4,
              NULL,
              NULL,
              NULL,
              NULL,
              NULL,
              DONT_CARE);

    option = NrmQuarkToString(*qoption);
    if(NULL == option)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclwrite_table: unknown write option.\n");
        return(NhlFATAL);
    }

    list_id = (obj *)NclGetArgValue(
               2,
               4,
               NULL,
               NULL,
               NULL,
               NULL,
               NULL,
               DONT_CARE);

    if(NULL == list_id)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclwrite_table: NULL list.\n");
        return(NhlFATAL);
    }

    qformat = (NclQuark *)NclGetArgValue(
              3,
              4,
              NULL,
              NULL,
              NULL,
              NULL,
              NULL,
              DONT_CARE);

    format = NrmQuarkToString(*qformat);
    if(NULL == format)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclwrite_table: unknown format.\n");
        return(NhlFATAL);
    }

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tformat = <%s>\n", format);
   */

    fp = fopen(filename, option);

    process_list(fp, list_id, format, ndvdl, 0);
    process_list(fp, list_id, format, ndvdl, 1);

    fclose(fp);
  /*
   *fprintf(stderr, "Leave _Nclwrite_table, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return(NhlNOERROR);
}

NhlErrorTypes _NclgetNbitsFromUint64()
{
    unsigned long long *ui64;
    int *nskip;
    int *nbits;

    int ndim_ui64;
    int has_missing_ui64;
    int has_missing_ret = 0;

    NclScalar missing_ui64;
    NclScalar ret_missing;
  
    ng_size_t dimsz_ui64[NCL_MAX_DIMENSIONS];
    ng_size_t i;

    unsigned long long *newui64;
    unsigned long long mask = 1;
    ng_size_t total_size;
    
    ui64 = (unsigned long long *) NclGetArgValue(
                        0,
                        3,
                        &ndim_ui64,
                        dimsz_ui64,
                        &missing_ui64,
                        &has_missing_ui64,
                        NULL,
                        DONT_CARE);

    if(NULL == ui64)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "getNbitsFromUint64: input uint64 array is null.");
        return NhlFATAL;
    }

    nskip = (int *) NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    if(NULL == nskip)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "getNbitsFromUint64: input nskip is null.");
        return NhlFATAL;
    }

    nbits = (int *) NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        DONT_CARE);

    if(NULL == nbits)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "getNbitsFromUint64: input nbits is null.");
        return NhlFATAL;
    }

    if(has_missing_ui64)
        ret_missing.uint64val = missing_ui64.uint64val;
    else
        ret_missing.uint64val = (uint64) ((NclTypeClass) nclTypeuint64Class)->type_class.default_mis.uint64val;

    total_size = 1;
    for(i=0; i<ndim_ui64; i++)
        total_size *= dimsz_ui64[i];

    newui64 = (unsigned long long *) NclMalloc(total_size*sizeof(unsigned long long));
    if (! newui64)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    mask = 1;
    for(i = 1; i < nbits[0]; ++i)
    {
        mask = mask << 1;
        ++mask;
    }

    if(has_missing_ui64)
    {
        for(i=0; i<total_size; i++)
        {
            if(ui64[i] == missing_ui64.uint64val)
            {
                has_missing_ret = 1;
	        newui64[i] = ret_missing.uint64val;
            }
            else
            {
                newui64[i] = (ui64[i] >> nskip[0]) & mask;
            }
        }
    }
    else
    {
        has_missing_ret = 0;
        for(i=0; i<total_size; i++)
        {
            newui64[i] = (ui64[i] >> nskip[0]) & mask;
        }
    }

    return NclReturnValue(newui64, ndim_ui64, dimsz_ui64, (has_missing_ret ? &ret_missing : NULL), NCL_uint64, 0);

}

NhlErrorTypes _Nclstr_match_regex(void)
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    NclQuark *output_strs;
    ng_size_t i;
    ng_size_t str_size;
    ng_size_t output_str_size = 0;

    regex_t expr;
    regmatch_t rm;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_regex: input string is null.");
        return NhlFATAL;
    }

    if(type != NCL_string)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_regex: Invalid input string.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        ret_missing.stringval = missing_input_strs.stringval;
        has_missing = 1;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_regex: input expression is null.");
        return NhlFATAL;
    }

    if(type == NCL_string)
    {
        char *reg_exp = NrmQuarkToString(input_expr[0]);
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\treg_exp: <%s>\n", reg_exp);
       *fprintf(stderr, "\tstrlen(reg_exp) = %d\n", strlen(reg_exp));
       */

        if(strlen(reg_exp) > 0)
        {
            if(regcomp(&expr,reg_exp,REG_EXTENDED) != 0)
            {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"str_match_regex: Invalid expression");
                return NhlFATAL;
            }
        }
        else
        {
            str_size = 1;

            output_strs = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
            if (! output_strs)
            {
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return NhlFATAL;
            }
            
            output_str_size = 1;
            output_strs[0] = ret_missing.stringval;
            return NclReturnValue(output_strs, 1, &output_str_size,
                                  &ret_missing, NCL_string, 0);
        }
    }
    else
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_regex: input expression is not a string.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_strs = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! output_strs)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(regexec(&expr,tmp_str,1,&rm,0) == 0)
        {
            output_strs[output_str_size] = input_strs[i];
          /*
           *fprintf(stderr, "\toutput_strs[%d]: <%s>\n",
           *        output_str_size, NrmQuarkToString(output_strs[output_str_size]));
           */
            output_str_size ++;
        }
    }

    if(output_str_size)
        output_strs = (NclQuark *) NclRealloc(output_strs, output_str_size*sizeof(NclQuark));
    else
    {
        has_missing = 1;
        output_strs = (NclQuark *) NclRealloc(output_strs, sizeof(NclQuark));
        output_strs[output_str_size] = ret_missing.stringval;
      /*
       *output_strs[output_str_size] = NrmStringToQuark("NO MATCH");
       */
        output_str_size = 1;
    }

    return NclReturnValue(output_strs, 1, &output_str_size, ( has_missing ? &ret_missing : NULL ), NCL_string, 0);
}

NhlErrorTypes _Nclstr_match_ic_regex(void)
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    NclQuark *output_strs;
    ng_size_t i;
    ng_size_t str_size;
    ng_size_t output_str_size = 0;

    regex_t expr;
    regmatch_t rm;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic_regex: input string is null.");
        return NhlFATAL;
    }

    if(type != NCL_string)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic_regex: Invalid input string.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        ret_missing.stringval = missing_input_strs.stringval;
        has_missing = 1;
    }
    else
    {
        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    }

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic_regex: input expression is null.");
        return NhlFATAL;
    }

    if(type == NCL_string)
    {
        char *reg_exp = NrmQuarkToString(input_expr[0]);
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\treg_exp: <%s>\n", reg_exp);
       */
        if(strlen(reg_exp) > 0)
        {
            if(regcomp(&expr,reg_exp,REG_ICASE|REG_EXTENDED) != 0)
            {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"str_match_ic_regex: Invalid expression");
                return NhlFATAL;
            }
        }
    }
    else
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ic_regex: input expression is not a string.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_strs = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! output_strs)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(regexec(&expr,tmp_str,1,&rm,0) == 0)
        {
            output_strs[output_str_size] = input_strs[i];
          /*
           *fprintf(stderr, "\toutput_strs[%d]: <%s>\n",
           *        output_str_size, NrmQuarkToString(output_strs[output_str_size]));
           */
            output_str_size ++;
        }
    }

    if(output_str_size)
        output_strs = (NclQuark *) NclRealloc(output_strs, output_str_size*sizeof(NclQuark));
    else
    {
        has_missing = 1;
        output_strs = (NclQuark *) NclRealloc(output_strs, sizeof(NclQuark));
        output_strs[output_str_size] = ret_missing.stringval;
      /*
       *output_strs[output_str_size] = NrmStringToQuark("NO MATCH");
       */
        output_str_size = 1;
    }

    return NclReturnValue(output_strs, 1, &output_str_size, ( has_missing ? &ret_missing : NULL ), NCL_string, 0);
}

NhlErrorTypes _Nclstr_match_ind_regex(void)
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    int *output_inds;
    ng_size_t i;
    ng_size_t str_size;
    ng_size_t output_ind_size = 0;

    regex_t expr;
    regmatch_t rm;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_regex: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        has_missing = 1;
    }
    ret_missing.intval =  (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_regex: input expression is null.");
        return NhlFATAL;
    }

    if(type == NCL_string)
    {
        char *reg_exp = NrmQuarkToString(input_expr[0]);
        if(strlen(reg_exp) > 0)
        {
            if(regcomp(&expr,reg_exp,REG_EXTENDED) != 0)
            {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"str_match_ind_regex: Invalid expression");
                return NhlFATAL;
            }
        }
    }
    else
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_regex: input expression is not a string.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_inds = (int *) NclMalloc(str_size*sizeof(int));
    if (! output_inds)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(regexec(&expr,tmp_str,1,&rm,0) == 0)
        {
            output_inds[output_ind_size] = i;
            output_ind_size ++;
        }
    }

    if(output_ind_size)
        output_inds = (int *) NclRealloc(output_inds, output_ind_size*sizeof(int));
    else
    {
        has_missing = 1;
        output_inds = (int *) NclRealloc(output_inds, sizeof(int));
        output_inds[output_ind_size] = ret_missing.intval;
      /*
       *output_inds[output_ind_size] = NrmStringToQuark("NO MATCH");
       */
        output_ind_size = 1;
    }

    return NclReturnValue(output_inds, 1, &output_ind_size, ( has_missing ? &ret_missing : NULL ), NCL_int, 0);
}

NhlErrorTypes _Nclstr_match_ind_ic_regex(void)
{
    NclQuark *input_strs;
    NclQuark *input_expr;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_expr;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_expr;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *tmp_str;
    int *output_inds;
    ng_size_t i;
    ng_size_t str_size;
    ng_size_t output_ind_size = 0;

    regex_t expr;
    regmatch_t rm;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_ic_regex: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        has_missing = 1;
    }
    ret_missing.intval =  (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

    input_expr = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_expr,
                        &has_missing_input_expr,
                        &type,
                        0);

    if (input_expr == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_ic_regex: input expression is null.");
        return NhlFATAL;
    }

    if(type == NCL_string)
    {
        char *reg_exp = NrmQuarkToString(input_expr[0]);
        if(strlen(reg_exp) > 0)
        {
            if(regcomp(&expr,reg_exp,REG_ICASE|REG_EXTENDED) != 0)
            {
                NhlPError(NhlWARNING,NhlEUNKNOWN,"str_match_ind_ic_regex: Invalid expression");
                return NhlFATAL;
            }
        }
    }
    else
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_ic_regex: input expression is not a string.");
        return NhlFATAL;
    }

    str_size = 1;
    for(i=0; i<ndim_input_strs; i++)
        str_size *= dimsz_input_strs[i];

    output_inds = (int *) NclMalloc(str_size*sizeof(int));
    if (! output_inds)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_expr && (input_expr[i] == missing_input_expr.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);
      /*
       *fprintf(stderr, "\tinput_strs[%d]: <%s>\n", i, tmp_str);
       */

        if(regexec(&expr,tmp_str,1,&rm,0) == 0)
        {
            output_inds[output_ind_size] = i;
            output_ind_size ++;
        }
    }

    if(output_ind_size)
        output_inds = (int *) NclRealloc(output_inds, output_ind_size*sizeof(int));
    else
    {
        has_missing = 1;
        output_inds = (int *) NclRealloc(output_inds, sizeof(int));
        output_inds[output_ind_size] = ret_missing.intval;
      /*
       *output_inds[output_ind_size] = NrmStringToQuark("NO MATCH");
       */
        output_ind_size = 1;
    }

    return NclReturnValue(output_inds, 1, &output_ind_size, ( has_missing ? &ret_missing : NULL ), NCL_int, 0);
}

NhlErrorTypes _Nclstr_remove_leading_str(void)
{
    NclQuark *input_strs;
    NclQuark *input_lead;

    int ndim_input_strs;
    ng_size_t dimsz_input_strs[NCL_MAX_DIMENSIONS];
    int has_missing_input_strs;
    int has_missing_input_lead;
    int has_missing = 0;
    NclScalar missing_input_strs;
    NclScalar missing_input_lead;
    NclScalar ret_missing;

    NclBasicDataTypes type;

    char *lead_str;
    char *tmp_str;
    NclQuark *output_strs;
    ng_size_t i;
    ng_size_t str_size;
    int lead_len = 0;
    int new_len = 0;

  /*
   *fprintf(stderr, "in file: %s, line: %d\n", __FILE__, __LINE__);
   */

    input_strs = (NclQuark *) NclGetArgValue(
                        0,
                        2,
                        &ndim_input_strs,
                        dimsz_input_strs,
                        &missing_input_strs,
                        &has_missing_input_strs,
                        &type,
                        0);

    if (input_strs == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_match_ind_ic_regex: input string is null.");
        return NhlFATAL;
    }

    if(has_missing_input_strs)
    {
        has_missing = 1;
    }
    ret_missing.intval =  (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

    input_lead = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &missing_input_lead,
                        &has_missing_input_lead,
                        &type,
                        0);

    if (input_lead == NULL)
    {
        NhlPError(NhlFATAL, NhlEUNKNOWN, "str_remove_leading_str: input leading string is null.");
        return NhlFATAL;
    }

    lead_str = NrmQuarkToString(input_lead[0]);
    lead_len = strlen(lead_str);

    str_size = 1;
    for(i = 0; i < ndim_input_strs; ++i)
        str_size *= dimsz_input_strs[i];

    output_strs = (NclQuark *) NclMalloc(str_size*sizeof(NclQuark));
    if (! output_strs)
    {
        NHLPERROR((NhlFATAL,ENOMEM,NULL));
        return NhlFATAL;
    }

    for(i=0; i<str_size; i++)
    {
        if(has_missing_input_strs && input_strs[i] == missing_input_strs.stringval)
        {
            has_missing = 1;
            continue;
        }

        if(has_missing_input_lead && (input_lead[i] == missing_input_lead.stringval))
        {
            continue;
        }

        tmp_str = (char *) NrmQuarkToString(input_strs[i]);

        new_len = strlen(tmp_str);
      
        if(new_len <= lead_len)
            output_strs[i] = NrmStringToQuark("");
        else
        {
            if(strncmp(tmp_str, lead_str, lead_len-1))
                output_strs[i] = input_strs[i];
            else
                output_strs[i] = NrmStringToQuark(tmp_str+lead_len);
        }
      /*
       *fprintf(stderr, "\tinput_strs[%ld]: <%s>, lead-str: <%s>\n", i, tmp_str, lead_str);
       *fprintf(stderr, "\toutput_strs[%ld]: <%s>\n", i, NrmQuarkToString(output_strs[i]));
       */
    }

    return NclReturnValue(output_strs, ndim_input_strs, dimsz_input_strs, ( has_missing ? &ret_missing : NULL ), NCL_string, 0);
}

#ifdef __cplusplus
}
#endif


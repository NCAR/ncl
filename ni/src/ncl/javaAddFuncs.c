/*
 *      $Id: javaAddFuncs.c,v 1.4 2010-01-13 20:15:24 huangwei Exp $
 */
/************************************************************************
*                                                                       *
*                   Copyright (C)  2009                                 *
*           University Corporation for Atmospheric Research             *
*                   All Rights Reserved                                 *
*                                                                       *
************************************************************************/
/*
 *    File:        javaAddFuncs.c
 *
 *    Author:        Wei Huang
 *            National Center for Atmospheric Research
 *            PO 3000, Boulder, Colorado
 *
 *    Date:        Fri Mar 20 11:25:53 MDT 2009
 *
 *    Description:    
 */
#ifdef __cplusplus
extern "C"
{
#endif
#include <stdio.h>
#include <stdlib.h>
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

NhlErrorTypes _JavaMin(
#if    NhlNeedProto
NclMultiDValData tmp_md, FILE *fr
#endif
);

NhlErrorTypes _JavaMax(
#if    NhlNeedProto
NclMultiDValData tmp_md, FILE *fr
#endif
);

NclQuark FileGetDimName(
#if     NhlNeedProto
NclFile /* thefile */,
int /*num*/
#endif
);

NhlErrorTypes _NclIgenerateVarRange
#if    NhlNeedProto
(void)
#else
()
#endif
{
    NclStackEntry data;
    NclMultiDValData tmp_md = NULL;
    NhlErrorTypes min, max;
    FILE *fr;

    fr = fopen(".var_range", "w");

    data = _NclGetArg(0,1,DONT_CARE);

    switch(data.kind)
    {
        case NclStk_VAR:
            tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
            break;
        case NclStk_VAL:
            tmp_md = (NclMultiDValData)data.u.data_obj;
            break;
        default:
            NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-Variable passed to generateVarRange.");
            return(NhlFATAL);
    }
    if(tmp_md == NULL)
    {
        fprintf(fr, "-999\n999\n");
        return(NhlFATAL);
    }
    
    min = _JavaMin(tmp_md, fr);
    max = _JavaMax(tmp_md, fr);

    fclose(fr);
    return NhlNOERROR;
}

void JavaPrintValue(void *out_val, NclBasicDataTypes data_type, FILE *fr)
{
    switch(data_type)
    {
        case NCL_int:
            {
            int *iv = (int *) out_val;
            fprintf(fr, "%d\n", *iv);
            break;
            }
        case NCL_long:
            {
            long *lv = (long *) out_val;
            fprintf(fr, "%ld\n", *lv);
            break;
            }
        case NCL_float:
            {
            float *fv = (float *) out_val;
            fprintf(fr, "%f\n", *fv);
            break;
            }
        case NCL_double:
            {
            double *dv = (double *) out_val;
            fprintf(fr, "%g\n", *dv);
            break;
            }
        default:
            fprintf(fr, "99999\n");
    }
}

NhlErrorTypes _JavaMin
#if    NhlNeedProto
(NclMultiDValData tmp_md, FILE *fr)
#else
()
#endif
{
    void *out_val;
    ng_size_t dimsizes = 1;
    void *tmp;
    logical result;
    ng_size_t i;

    if(tmp_md->multidval.missing_value.has_missing)
    {
        i = 0;
        while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
            i++;
        if(i == tmp_md->multidval.totalelements)
        {
            fprintf(fr, "-999\n");
            return(NclReturnValue(
                &tmp_md->multidval.type->type_class.default_mis,
                1,
                &dimsizes,
                &tmp_md->multidval.type->type_class.default_mis,
                tmp_md->multidval.data_type,
                1
            ));
        }

        tmp= &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
        for(; i < tmp_md->multidval.totalelements; i++)
        {
            _Nclgt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,&(tmp_md->multidval.missing_value.value),1,1);
            if(result == 1)
            {
                tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
                result = 0;
            }
        }
        out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
        memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
        JavaPrintValue(out_val, tmp_md->multidval.data_type, fr);
        return(NclReturnValue(
            out_val,
            1,
            &dimsizes,
            NULL,
            tmp_md->multidval.data_type,
            0
        ));
    }
    else
    {
        tmp= tmp_md->multidval.val;
        for(i = 0; i < tmp_md->multidval.totalelements; i++)
        {
            _Nclgt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,NULL,1,1);
            if(result == 1)
            {
                tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
                result = 0;
            }
        }
        out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
        memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
        JavaPrintValue(out_val, tmp_md->multidval.data_type, fr);
        return(NclReturnValue(
            out_val,
            1,
            &dimsizes,
            NULL,
            tmp_md->multidval.data_type,
            0
        ));
    }
}

NhlErrorTypes _JavaMax
#if    NhlNeedProto
(NclMultiDValData tmp_md, FILE *fr)
#else
()
#endif
{
    void *out_val;
    ng_size_t dimsizes = 1;
    void *tmp;
    logical result;
    ng_size_t i;

    if(tmp_md->multidval.missing_value.has_missing)
    {
        i = 0;
        while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
            i++;
        if(i == tmp_md->multidval.totalelements)
        {
            fprintf(fr, "999\n");
            return(NclReturnValue(
                &tmp_md->multidval.type->type_class.default_mis,
                1,
                &dimsizes,
                &tmp_md->multidval.type->type_class.default_mis,
                tmp_md->multidval.data_type,
                1
            ));
        }

        tmp= &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
        for(; i < tmp_md->multidval.totalelements; i++)
        {
            _Ncllt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,&(tmp_md->multidval.missing_value.value),1,1);
            if(result == 1)
            {
                tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
                result = 0;
            }
        }
        out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
        memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
        JavaPrintValue(out_val, tmp_md->multidval.data_type, fr);
        return(NclReturnValue(
            out_val,
            1,
            &dimsizes,
            NULL,
            tmp_md->multidval.data_type,
            0
        ));
    }
    else
    {
        tmp= tmp_md->multidval.val;
        for(i = 0; i < tmp_md->multidval.totalelements; i++)
        {
            _Ncllt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,NULL,1,1);
            if(result == 1)
            {
                tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
                result = 0;
            }
        }
        out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
        memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
        JavaPrintValue(out_val, tmp_md->multidval.data_type, fr);

        return(NclReturnValue(
            out_val,
            1,
            &dimsizes,
            NULL,
            tmp_md->multidval.data_type,
            0
        ));
    }
}

NhlErrorTypes _NclIgenerateVarList
# if	NhlNeedProto
(void)
# else
()
# endif /* NhlNeedProto */
{
    /* file variables */
    NclFile thefile;
    int *fid;

    NclMultiDValData tmp_md;
    ng_size_t i,j;

    FILE *fd;
    FILE *fa;
    FILE *fm;

    char var_name[256];
    char var_type[256];
    ng_size_t n;

    /* dimensions */
    ng_size_t dimsizes = 1;

    i = 0;

    /* get file information */
    fid = (int *) NclGetArgValue(
                    0,
                    1,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);

    thefile = (NclFile) _NclGetObj((int) *fid);

    if (thefile == NULL)
    {
        NhlPError(NhlWARNING, NhlEUNKNOWN, " generateVarList(): undefined file variable");
        return(NhlWARNING);;
    }

    fa = fopen(".file_attributes", "w");
    fprintf(fa,"filename: <%s>\n", NrmQuarkToString(thefile->file.fname));
    fprintf(fa,"filepath: <%s>\n", NrmQuarkToString(thefile->file.fpath));
    fprintf(fa,"global attributes:\n");

    for(i = 0; i < thefile->file.n_file_atts; i++)
    {
        if(thefile->file.file_atts[i] != NULL)
        {
            fprintf(fa,"%s\t: ", NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark));

            if(thefile->file.file_atts[i]->num_elements == 1)
            {
                tmp_md = _NclFileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
                _Nclprint(tmp_md->multidval.type,fa,tmp_md->multidval.val);
                fprintf(fa,"\n");
            }
            else if (thefile->file.file_atts[i]->num_elements > 1 &&
                     thefile->file.file_atts[i]->num_elements < 11)
            {
                tmp_md = _NclFileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
                fprintf(fa,"( ");
                for (j = 0; j < tmp_md->multidval.totalelements; j++)
                {
                    char *val = (char*)tmp_md->multidval.val + 
                                j * tmp_md->multidval.type->type_class.size; 
                    _Nclprint(tmp_md->multidval.type,fa,val);

                    if (j < tmp_md->multidval.totalelements - 1)
                    {
                        fprintf(fa,", ");
                    }
                }
                fprintf(fa," )\n");
            }
            else
            {
		    fprintf(fa,"<ARRAY of %ld elements>\n",(long)thefile->file.file_atts[i]->num_elements);
            }
        }
    }
    fclose(fa);

    fd = fopen(".ncl_dim_info", "w");

    for(i = 0; i< thefile->file.n_file_dims; i++)
    {
        strcpy(var_name, NrmQuarkToString(thefile->file.file_dim_info[i]->dim_name_quark));
        n = thefile->file.file_dim_info[i]->dim_size;
        fprintf(fd, "%s\t%ld\n", var_name, (long)n);
    }
    fclose(fd);

  /*
   *fv = fopen(".ncl_var_info", "w");
   *for(i = 0; i < thefile->file.n_vars; i++)
   *{
   *    if(thefile->file.var_info[i] != NULL)
   *    {
   *        strcpy(var_name, NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
   *        fprintf(fv, "%s\n", var_name);
   *    }
   *}
   *fclose(fv);

    fprintf(stdout, "\n\n\nhit _NclIgenerateVarList. file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stdout, "\tthefile->file.n_vars: %d\n", thefile->file.n_vars);
   */

    fm = fopen(".ncl_var_info", "w");
    for(i = 0; i < thefile->file.n_vars; i++)
    {
        if(thefile->file.var_info[i] != NULL)
        {
            strcpy(var_name, NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
            strcpy(var_type, _NclBasicDataTypeToName(thefile->file.var_info[i]->data_type));
            fprintf(fm,"%s\t%s", var_name, var_type);

            for(j=0; j<thefile->file.var_info[i]->num_dimensions; j++)
            {
                /* Here var_type represents dim name. */
                strcpy(var_type, NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j])));
                fprintf(fm,"\t%s", var_type);
            }
            fprintf(fm,"\n");

          /*
            step = thefile->file.var_att_info[i];
            while(step != NULL)
            {
                fprintf(fm,"%s\t: ", NrmQuarkToString(step->the_att->att_name_quark));

                if(step->the_att->num_elements == 1)
                {
                    tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
                    _Nclprint(tmp_md->multidval.type,fm,tmp_md->multidval.val);

                    fprintf(fm,"\n");
                }
                else if (step->the_att->num_elements > 1 &&
                         step->the_att->num_elements < 11)
                {
                    tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
                    fprintf(fm,"( ");

                    for (j = 0; j < tmp_md->multidval.totalelements; j++)
                    {
                        char *val = (char*)tmp_md->multidval.val + 
                            j * tmp_md->multidval.type->type_class.size; 
                        _Nclprint(tmp_md->multidval.type,fm,val);

                        if (j < tmp_md->multidval.totalelements - 1)
                        {
                            fprintf(fm,", ");
                        }
                    }

                    fprintf(fm," )\n");
                }
                else
                {
                    fprintf(fm,"<ARRAY of %d elements>\n",step->the_att->num_elements);
                }
                step = step->next;
            }
            fprintf(fm,"\n");
           */
        }
    }
    fclose(fm);
    
    i = 0;
    return NclReturnValue((void *) &i, 1, &dimsizes, NULL, NCL_int, 1);
}

#ifdef __cplusplus
}
#endif


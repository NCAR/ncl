/************************************************************************
*                                                                       *
*                 Copyright (C)  1994                                   *
*         University Corporation for Atmospheric Research               *
*                 All Rights Reserved                                   *
*                                                                       *
************************************************************************/
/*
 * $Id:		NclNewHDFEOS5.c 13961 2012-11-26 17:42:46Z huangwei $
 *
 * Author:	Wei Huang
 *		National Center for Atmospheric Research
 *		PO 3000, Boulder, Colorado
 *
 * Date:	November 16, 2012
 *
 * Description:    
 */

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "NclDataDefs.h"
#include "NclData.h"
#include "NclFileInterfaces.h"
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <HE5_HdfEosDef.h>

#include "AdvancedFileSupport.h"

#define HE5_BUF_SIZE	32768
#define MAX_SW  4
#define MAX_GD  4
#define MAX_PT  4
#define MAX_ZA  4
#define MAX_DIM 32
#define MAX_ATT 128
#define MAX_FLD 1024
#define MAX_LVL 128
#define MAX_VAR 1024

#define HE5_MAX_STRING_LENGTH   2048

typedef enum {SWATH, POINT, GRID, ZA} HE5Type;

static void getHE5SwathData(NclFileGrpNode *grpnode, NclQuark path);
static void getHE5GridData(NclFileGrpNode *grpnode, NclQuark path);
static void getHE5PointData(NclFileGrpNode *grpnode, NclQuark path);
static void getHE5ZonalAverageData(NclFileGrpNode *grpnode, NclQuark path);

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

static void _reallocnames(int na, int *ma, NclQuark *hdf_names, NclQuark *ncl_names)
{
    if(na > *ma)
    {
        while(na > *ma)
            *ma *= 2;
        hdf_names = (NclQuark *)NclRealloc(hdf_names, (*ma) * sizeof(NclQuark));
        ncl_names = (NclQuark *)NclRealloc(ncl_names, (*ma) * sizeof(NclQuark));
    }
}

static int MyHE5setOrigincode(double *upper_left, double *lower_right)
{
    int origincode = HE5_HDFE_GD_UL;

  /*x-direction*/
    if(lower_right[0] > upper_left[0])
    {
      /*y-direction*/
        if(lower_right[1] > upper_left[1])
        {
             origincode = HE5_HDFE_GD_UL;
        }
        else
        {
             origincode = HE5_HDFE_GD_LL;
        }
    }
    else
    {
      /*y-direction*/
        if(lower_right[1] > upper_left[1])
        {
             origincode = HE5_HDFE_GD_UR;
        }
        else
        {
             origincode = HE5_HDFE_GD_LR;
        }
    }

    return origincode;
}

int HE5unsigned(long typenumber)
{
    if((typenumber == H5T_NATIVE_INT) ||
       (typenumber == H5T_NATIVE_SHORT) ||
       (typenumber == H5T_NATIVE_SCHAR) ||
       (typenumber == H5T_NATIVE_CHAR) ||
       (typenumber == H5T_NATIVE_LONG) ||
       (typenumber == H5T_NATIVE_LLONG) ||
       (typenumber == H5T_NATIVE_FLOAT) ||
       (typenumber == H5T_NATIVE_DOUBLE) ||
       (typenumber == H5T_NATIVE_LDOUBLE) ||
       (typenumber == H5T_NATIVE_INT32) ||
       (typenumber == H5T_NATIVE_INT16) ||
       (typenumber == H5T_NATIVE_INT8) ||
       (typenumber == H5T_NATIVE_INT32) ||
       (typenumber == H5T_NATIVE_INT64))
        return(0);

    if((typenumber == H5T_NATIVE_UINT) ||
       (typenumber == H5T_NATIVE_USHORT) ||
       (typenumber == H5T_NATIVE_UCHAR) ||
       (typenumber == H5T_NATIVE_ULONG) ||
       (typenumber == H5T_NATIVE_ULLONG) ||
       (typenumber == H5T_NATIVE_UINT32) ||
       (typenumber == H5T_NATIVE_UINT16) ||
       (typenumber == H5T_NATIVE_UINT8) ||
       (typenumber == H5T_NATIVE_UINT32) ||
       (typenumber == H5T_NATIVE_UINT64))
        return(1);

    switch(typenumber)
    {
        case HE5T_NATIVE_INT:
        case HE5T_NATIVE_SHORT:
        case HE5T_NATIVE_SCHAR:
        case HE5T_NATIVE_LONG:
        case HE5T_NATIVE_LLONG:
        case HE5T_NATIVE_FLOAT:
        case HE5T_NATIVE_DOUBLE:
        case HE5T_NATIVE_LDOUBLE:
        case HE5T_NATIVE_INT8:
        case HE5T_NATIVE_INT16:
        case HE5T_NATIVE_INT32:
        case HE5T_NATIVE_INT64:
             return(0);
        case HE5T_NATIVE_UINT:
        case HE5T_NATIVE_USHORT:
        case HE5T_NATIVE_UCHAR:
        case HE5T_NATIVE_ULONG:
        case HE5T_NATIVE_ULLONG:
        case HE5T_NATIVE_UINT8:
        case HE5T_NATIVE_UINT16:
        case HE5T_NATIVE_UINT32:
        case HE5T_NATIVE_UINT64:
             return(1);
        default:
             return(0);
    }
}

static NclBasicDataTypes HE5MapTypeNumber(long typenumber){
    if(typenumber == H5T_NATIVE_INT)
            return(NCL_int);
    else if(typenumber == H5T_NATIVE_UINT)
            return(NCL_uint);
    else if(typenumber == H5T_NATIVE_SHORT)
            return(NCL_short);
    else if(typenumber == H5T_NATIVE_USHORT)
            return(NCL_ushort);
    else if(typenumber == H5T_NATIVE_SCHAR)
            return(NCL_byte);
    else if(typenumber == H5T_NATIVE_CHAR)
            return(NCL_char);
    else if(typenumber == H5T_NATIVE_UCHAR)
            return(NCL_char);
    else if(typenumber == H5T_NATIVE_LONG)
            return(NCL_long);
    else if(typenumber == H5T_NATIVE_ULONG)
            return(NCL_ulong);
    else if(typenumber == H5T_NATIVE_LLONG)
            return(NCL_int64);
    else if(typenumber == H5T_NATIVE_ULLONG)
            return(NCL_uint64);
    else if(typenumber == H5T_NATIVE_FLOAT)
            return(NCL_float);
    else if(typenumber == H5T_NATIVE_DOUBLE)
            return(NCL_double);
    else if(typenumber == H5T_NATIVE_LDOUBLE)
            return(NCL_double);
    else if(typenumber == H5T_NATIVE_INT32)
            return(NCL_int);
    else if(typenumber == H5T_NATIVE_UINT32)
            return(NCL_uint);
    else if(typenumber == H5T_NATIVE_INT16)
            return(NCL_short);
    else if(typenumber == H5T_NATIVE_UINT16)
            return(NCL_ushort);
    else if(typenumber == H5T_NATIVE_INT8)
            return(NCL_byte);
    else if(typenumber == H5T_NATIVE_UINT8)
            return(NCL_char);
    else if(typenumber == H5T_NATIVE_INT32)
            return(NCL_long);
    else if(typenumber == H5T_NATIVE_UINT32)
            return(NCL_ulong);
    else if(typenumber == H5T_NATIVE_INT64)
            return(NCL_int64);
    else if(typenumber == H5T_NATIVE_UINT64)
            return(NCL_uint64);

    switch(typenumber)
        {
            case HE5T_NATIVE_INT:
                 return(NCL_int);
            case HE5T_NATIVE_UINT:
                 return(NCL_int);
            case HE5T_NATIVE_SHORT:
                 return(NCL_short);
            case HE5T_NATIVE_USHORT:
                 return(NCL_ushort);
            case HE5T_NATIVE_SCHAR:
                 return(NCL_byte);
            case HE5T_NATIVE_CHAR:
            case HE5T_NATIVE_UCHAR:
                 return(NCL_char);
            case HE5T_NATIVE_LONG:
                 return(NCL_long);
            case HE5T_NATIVE_ULONG:
                 return(NCL_ulong);
            case HE5T_NATIVE_LLONG:
                 return(NCL_int64);
            case HE5T_NATIVE_ULLONG:
                 return(NCL_uint64);
            case HE5T_NATIVE_FLOAT:
          /*
           *case HE5T_NATIVE_REAL:
           */
                 return(NCL_float);
            case HE5T_NATIVE_DOUBLE:
            case HE5T_NATIVE_LDOUBLE:
                 return(NCL_double);
            case HE5T_NATIVE_INT8:
                 return(NCL_byte);
            case HE5T_NATIVE_UINT8:
                 return(NCL_byte);
            case HE5T_NATIVE_INT16:
                 return(NCL_short);
            case HE5T_NATIVE_UINT16:
                 return(NCL_ushort);
            case HE5T_NATIVE_INT32:
                 return(NCL_int);
            case HE5T_NATIVE_UINT32:
                 return(NCL_uint);
            case HE5T_NATIVE_INT64:
                 return(NCL_int64);
            case HE5T_NATIVE_UINT64:
                 return(NCL_uint64);
            case HE5T_CHARSTRING:
                 return(NCL_string);
            default:
                 NhlPError(NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5: Unsupported type encountered");
         return(NCL_none);
    
    }
}

static char *_make_proper_string_end(const char *input_name)
{
    char *output_name;
    int i, n;
    char *name;

    name = strdup(input_name);
    n = strlen(name);
    i = n - 1;

    while(i)
    {
      /*
       *if(((name[i] >= 'a') && (name[i] <= 'z'))
       *|| ((name[i] >= 'A') && (name[i] <= 'Z'))
       *|| ((name[i] >= '0') && (name[i] <= '9'))
       *||   name[i] == '_')
       */
        if((name[i] > 32) && (name[i] < 127))
        {
            name[i+1] = '\0';
            n = i+2;
            break;
        }
        else
        {
            name[i] = '\0';
            i--;
        }
    }
    output_name = (char *)NclMalloc(n);
    if(output_name == NULL)
    {
      /*
       *fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for output_name, in file: %s, line: %d\n",
       *        __FILE__, __LINE__);
       */
        NhlPError(NhlWARNING,NhlEUNKNOWN,"UNABLE TO ALLOCATE MEMORY for output_name");
        return NULL;
    }

    strncpy(output_name, name, n-1);
    output_name[n-1] = '\0';

    free(name);
    return output_name;
}

static void _synchHE5GrpVarDims(NclFileGrpNode *grpnode, NclFileVarNode *varnode)
{
    NclFileDimNode *gdimnode;
    NclFileDimNode *vdimnode;
    int i, m, n;
    int not_in_group = 1;

    if(NULL != grpnode->dim_rec)
    {
        m = grpnode->dim_rec->n_dims;
        if(NULL != varnode->dim_rec)
        {
            for(i = 0; i < varnode->dim_rec->n_dims; ++i)
            {
                vdimnode = &(varnode->dim_rec->dim_node[n]);
                for(n = 0; n < grpnode->dim_rec->n_dims; ++n)
                {
                    gdimnode = &(grpnode->dim_rec->dim_node[n]);
                    if(gdimnode->name == vdimnode->name);
                    {
                       not_in_group = 0;
                       break;
                    }
                }

                if(not_in_group)
                {
                    _addNclDimNode(&(grpnode->dim_rec), vdimnode->name, m,
			           vdimnode->size, 0);
                    ++m;
                }
            }
        }
    }
}

static void HE5ParseName
#if NhlNeedProto
(char names_in[], NclQuark *hdf_names, NclQuark *ncl_names, long n_names)
#else
(name_ins,ncl_names,hdf_names)
char names_in[];
NclQuark *ncl_names;
NclQuark *hdf_names;
long n_names;
#endif
{
    int i;
    char *tmp,*tmp2;
    
    tmp = names_in;
    for(i = 0; i < n_names; i++) {
        if((tmp2 = strchr(tmp,','))!= 0) {
            *tmp2 = '\0';
        }
        hdf_names[i] = NrmStringToQuark(tmp);
        tmp2 = tmp;
        while(*tmp2 != '\0') {
            if(!isalnum(*tmp2)) {
                *tmp2 = '_';
            }
            tmp2++;
        }
        ncl_names[i] = NrmStringToQuark(tmp);
        tmp = tmp2 + 1;
    }    
    return;
}

static void _addHE5Dim(NclFileDimNode *dimnode, NclQuark name, long size)
{
    dimnode->name = name;
    dimnode->size = size;
    dimnode->is_unlimited = 0;
}

static void _addHE5Att(NclFileAttNode *attnode, NclQuark att_ncl_name,
                       void *value, int n_elem, NclBasicDataTypes type)
{
    attnode->type = type;
    attnode->value = value;
    attnode->n_elem = n_elem;
}

static void _addHE5DimMapInfo(NclFileGrpNode *grpnode, NrmQuark swath_ncl_name,
                              int nmaps, char *dimmaps, long *off, long *inc)
{
    int i;
    char *tcp,*cp,*dim1, *dim2;
    char name_buf[1024];
    int* mapvals;

    cp = dimmaps;
    for(i = 0; i < nmaps; ++i)
    {
        dim1 = cp;
        cp = strchr(cp,'/');
        if (cp)
        {
            *cp = '\0';
            cp++;
        }
        for (tcp = dim1; *tcp != '\0'; tcp++)
        {
            if(!isalnum(*tcp)) {
                *tcp = '_';
            }
        }
        dim2 = cp;
        cp = strchr(cp,',');
        if (cp)
        {
            *cp = '\0';
            cp++;
        }
        for (tcp = dim2; *tcp != '\0'; tcp++)
        {
            if(!isalnum(*tcp))
            {
                *tcp = '_';
            }
        }

        if (off[i] >= 0 && inc[i] >= 0)
        {
            sprintf(name_buf,"%s_to_%s_mapping_offset_and_increment",dim1,dim2);
        }
        else
        {
            sprintf(name_buf,"%s_to_%s_mapping_offset_and_increment",dim2,dim1);
        }
        mapvals = NclMalloc(2 * sizeof(int));
        mapvals[0] = abs(off[i]);
        mapvals[1] = abs(inc[i]);

        _addNclAttNode(&(grpnode->att_rec), NrmStringToQuark(name_buf),
                       NCL_int, 2, (void *)mapvals);
        NclFree(mapvals);
    }
}

static void _addHE5IndexedMapVars(NclFileGrpNode *grpnode, NrmQuark swath_hdf_name,
                                  NrmQuark swath_ncl_name, int nmaps,
                                  char *idxmaps, hsize_t *sizes)
{
    int i;
    char *tcp,*cp,*dim1, *dim2;
    char name_buf[1024];
    NrmQuark hdf_name1;
    ng_size_t size;

    cp = idxmaps;
    for(i = 0; i < nmaps; ++i)
    {
        dim1 = cp;
        cp = strchr(cp,'/');
        if (cp)
        {
            *cp = '\0';
            cp++;
        }
        hdf_name1 = NrmStringToQuark(dim1);
        for (tcp = dim1; *tcp != '\0'; tcp++)
        {
            if(!isalnum(*tcp))
            {
                *tcp = '_';
            }
        }
        dim2 = cp;
        cp = strchr(cp,',');
        if (cp)
        {
            *cp = '\0';
            cp++;
        }
        for (tcp = dim2; *tcp != '\0'; tcp++)
        {
            if(!isalnum(*tcp))
            {
                *tcp = '_';
            }
        }

        _addNclDimNode(&(grpnode->dim_rec), hdf_name1, grpnode->dim_rec->n_dims, sizes[i], 0);

        sprintf(name_buf,"%s_index_mapping",dim2);

        size = (int)sizes[i];
        _addNclVarNodeToGrpNode(grpnode, NrmStringToQuark(name_buf), grpnode->var_rec->n_vars,
                                NCL_int, 1, &hdf_name1, &size);
    }
}


static void *HE5InitializeFileRec
#if    NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormatType *format;
#endif
{
    static int first = 1;
    NclFileGrpNode *grpnode = NULL;

    if (first)
    {
        Qmissing_val = NrmStringToQuark("missing_value");
        Qfill_val = NrmStringToQuark("_FillValue");
        first = 0;
    }

    /*nc_set_log_level(3);*/
    nc_set_log_level(3);

    grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
    assert(grpnode);

    grpnode->fid = -1;
    grpnode->gid = -1;
    grpnode->pid = -1;
    grpnode->name = NrmStringToQuark("/");
    grpnode->pname = NrmStringToQuark("-");
    grpnode->real_name = NrmStringToQuark("/");
    grpnode->path = -1;
    grpnode->extension = -1;

    grpnode->open = 0;
    grpnode->header_reserve_space = 0;
    grpnode->define_mode = -1;
    grpnode->other_src = NULL;
    grpnode->parent = NULL;

    *format = _NclNewHE5;
    setvbuf(stderr,NULL,_IONBF,0);
    return (void *) grpnode;
}


static void getHE5SwathData(NclFileGrpNode *parentgrpnode, NclQuark path)
{
    hid_t HE5_SWfid = FAIL;
    hid_t HE5_SWid = FAIL;

    long natts = 0;
    long nlocatts = 0;

    herr_t status = FAIL;

    long nsw;
    long ngeofields;
    long ndata = 0;
    long ndims;
    long nmaps;
    long ngrp_atts;

    char maxdimlist[HE5_BUF_SIZE];
    long str_buf_size;

    NclQuark *sw_hdf_names;
    NclQuark *sw_ncl_names;
    int max_sw = MAX_SW;

    NclQuark *var_hdf_names;
    NclQuark *var_ncl_names;
    int      *var_ranks = NULL;
    int max_var = MAX_VAR;

    NclQuark *dim_hdf_names;
    NclQuark *dim_ncl_names;
    hsize_t *dimsizes;
    int max_dim = MAX_DIM;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    NclQuark fv_quark = NrmStringToQuark("_FillValue");
    NclQuark mv_quark = NrmStringToQuark("MissingValue");

    hid_t tmp_type;

    void *tmp_value;
    hid_t att_type;
    hsize_t att_size;

    char *buffer;
    int cur_buf_size = HE5_BUF_SIZE;

    int i,j,k;
    boolean no_fill_value = TRUE;

    NclBasicDataTypes baseNclType = NCL_none;

    NclFileGrpNode   *grpnode = NULL;
    NclFileGrpRecord *grprec  = NULL;

    NclFileVarRecord *varrec  = NULL;
    NclFileVarNode   *varnode = NULL;

    NclFileAttNode   *attnode = NULL;
    NclFileAttRecord *attrec  = NULL;

    NclFileDimNode   *dimnode = NULL;
    NclFileDimRecord *dimrec  = NULL;

    hid_t       dtype;
    H5T_class_t classid;
    H5T_order_t order;
    size_t      size;
    int         count;

    nsw = HE5_SWinqswath(NrmQuarkToString(path),NULL,&str_buf_size);
    if(nsw < 1)
    {
        return;
    }

    while(nsw > max_sw)
        max_sw *= 2;

    sw_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_sw);
    sw_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_sw);

    var_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ranks     = (int *)NclCalloc(max_var, sizeof(int));

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dimsizes = (hsize_t *)NclMalloc(sizeof(hsize_t)*max_dim*2);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    if (str_buf_size >= cur_buf_size)
    {
        while(str_buf_size >= cur_buf_size)
            cur_buf_size *= 2;
    }

    buffer = NclMalloc(cur_buf_size);

    nsw = HE5_SWinqswath(NrmQuarkToString(path),buffer,&str_buf_size);

    buffer[str_buf_size] = '\0';

    HE5_SWfid = HE5_SWopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    grprec = _NclFileGrpAlloc(nsw);

    parentgrpnode->grp_rec = grprec;
    parentgrpnode->path = path;
    parentgrpnode->gid = HE5_SWfid;
    parentgrpnode->fid = HE5_SWfid;
    parentgrpnode->define_mode = SWATH;

    HE5ParseName(buffer,sw_hdf_names,sw_ncl_names,nsw);

    /* global attributes from file */
    ngrp_atts = HE5_EHinqglbattrs(HE5_SWfid,NULL,&str_buf_size);

    if(ngrp_atts > 0 )
    {
        if(ngrp_atts > max_att)
        {
            _reallocnames(ngrp_atts, &max_att, att_hdf_names, att_ncl_names);
        }

        attrec = _NclFileAttAlloc(ngrp_atts);
        assert(attrec);

        attrec->id  = HE5_SWfid;
        attrec->gid = HE5_SWfid;
        attrec->aid = HE5_SWfid;

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }

        ngrp_atts = HE5_EHinqglbattrs(HE5_SWfid,buffer,&str_buf_size);
        buffer[str_buf_size] = '\0';

      /*
       *fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tngrp_atts = %ld, attnames: <%s>\n", ngrp_atts, buffer);
       */

        HE5ParseName(buffer, att_hdf_names, att_ncl_names, ngrp_atts);

        for(k = 0; k < ngrp_atts; ++k)
        { 
            attnode = &(attrec->att_node[k]);
            attnode->is_virtual = 0;
            attnode->is_opaque = 0;
            attnode->is_vlen = 0;
            attnode->is_compound = 0;
            attnode->name = att_hdf_names[k];

          /*
           *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr,"\tAtt No. %d, name: <%s>, ncl_name: <%s>\n",
           *                k, NrmQuarkToString(att_hdf_names[k]),
           *                NrmQuarkToString(att_ncl_names[k]));
           */

            if(HE5_EHglbattrinfo(HE5_SWfid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
            {
                baseNclType = HE5MapTypeNumber(att_type);
              /*
               *Note: size should be datatype size in bytes
               *(max number of charcters for character string arrays
               */
                status = HE5_EHinqglbdatatype(HE5_SWfid, NrmQuarkToString(att_hdf_names[k]),
                                             &dtype, &classid, &order, &size);

                if(NCL_string == baseNclType)
                {
                    tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
                  /*tmp_value = (void*)NclCalloc((att_size+1) * size, 1);*/
                    if(HE5_EHreadglbattr(HE5_SWfid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                        *new_value = NrmStringToQuark(tmp_value);
                        att_size = 1;
                        _addHE5Att(attnode, att_ncl_names[k], new_value,
                                  (int) att_size, baseNclType);
                    }
                    NclFree(tmp_value);
                }
                else
                {
                    tmp_value = (void*)NclCalloc(size, att_size);
                    if(HE5_EHreadglbattr(HE5_SWfid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                                  (int) att_size, baseNclType);
                    }
                }
            }
        }
        parentgrpnode->att_rec = attrec;
    }

    for(i = 0; i < nsw; ++i)
    {
        HE5_SWid = HE5_SWattach(HE5_SWfid,NrmQuarkToString(sw_hdf_names[i]));

        if(HE5_SWid < 1)
            continue;

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\ti = %d, HE5_SWid = %d, sw_hdf_names[%d]: <%s>\n",
       *                  i, HE5_SWid, i, NrmQuarkToString(sw_hdf_names[i]));
       */

        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        grprec->grp_node[i] = grpnode;
        grpnode->path = parentgrpnode->path;
        grpnode->parent = parentgrpnode;
        grpnode->pid = HE5_SWfid;
        grpnode->gid = HE5_SWid;
        grpnode->fid = HE5_SWid;
        grpnode->pname = -1;
        grpnode->name = sw_hdf_names[i];
        grpnode->define_mode = SWATH;

      /*global attributes from file*/
        natts = HE5_SWinqattrs(HE5_SWid,NULL,&str_buf_size);

        if(natts > 0 )
        {
            if(natts > max_att)
            {
                _reallocnames(natts, &max_att, att_hdf_names, att_ncl_names);
            }

            attrec = _NclFileAttAlloc(natts);
            assert(attrec);

            attrec->id = HE5_SWid;
            attrec->gid = HE5_SWid;
            attrec->aid = HE5_SWid;

            if(str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            natts = HE5_SWinqattrs(HE5_SWid,buffer,&str_buf_size);
            buffer[str_buf_size] = '\0';

          /*
           *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tglobal natts = %ld, attnames: <%s>\n", natts, buffer);
           */

            HE5ParseName(buffer, att_hdf_names, att_ncl_names, natts);

            for(k = 0; k < natts; k++)
            { 
                attnode = &(attrec->att_node[k]);
                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = att_hdf_names[k];

              /*
               *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr,"\tAtt No. %d, name: <%s>\n", k, NrmQuarkToString(att_hdf_names[k]));
               */

                if(HE5_SWattrinfo(HE5_SWid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
                  /*
                   *Note: size should be datatype size in bytes
                   *(max number of charcters for character string arrays
                    status = HE5_EHinqdatatype(HE5_SWid, NrmQuarkToString(att_hdf_names[k]),
                                            &dtype, &classid, &order, &size);
                    tmp_value = (void*)NclCalloc((att_size+1) * size, 1);
                    tmp_value = (void*)NclCalloc(size, att_size);
                   */

                    attnode->type = HE5MapTypeNumber(att_type);
                    if(NCL_string == attnode->type)
                        tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
                    else
                        tmp_value = (void*)NclCalloc(att_size, _NclSizeOf(attnode->type));

                    if(HE5_SWreadattr(HE5_SWid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0)
                    {
                        if(NCL_string == attnode->type)
                        {
                            NclQuark *new_value = (NclQuark *) NclCalloc(1, sizeof(NclQuark));
                            *new_value = NrmStringToQuark(tmp_value);
                            att_size = 1;
                            _addHE5Att(attnode, att_ncl_names[k], new_value, (int) att_size, attnode->type);
                            NclFree(tmp_value);
                        }
                        else
                        {
                            _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                                       (int) att_size, attnode->type);
                        }
                    }
                }
            }
            grpnode->att_rec = attrec;
        }

       /*dimensions*/
        ndims = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTDIM, &str_buf_size);
        if(1 > ndims)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, 
                  "NclNewHDFEOS5: An internal HDF error occurred while reading (%s) can't continue",
                  NrmQuarkToString(path));
            return;
        }
        else if(ndims > max_dim)
        {
            _reallocnames(ndims, &max_dim, dim_hdf_names, dim_ncl_names);
            dimsizes = (hsize_t *)NclRealloc(dimsizes, max_dim * sizeof(hsize_t));
        }

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tndims = %d, str_buf_size = %ld\n", ndims, str_buf_size);
       */

        if(str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }

        ndims = HE5_SWinqdims(HE5_SWid,buffer,dimsizes);
	if(ndims > 0)
	{
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

            dimrec = _NclFileDimAlloc(ndims);
            dimrec->gid = HE5_SWid;
	}

        for(j = 0; j < ndims; ++j)
        {
          /*
           *fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
           *fprintf(stderr, "\tDim No. %d: size = %ld, name: <%s> ncl-name: <%s>\n",
           *                 j, (long)dimsizes[j],
           *                 NrmQuarkToString(dim_hdf_names[j]),
           *                 NrmQuarkToString(dim_ncl_names[j]));
           */

            dimnode = &(dimrec->dim_node[j]);

            dimnode->id = j;

            _addHE5Dim(dimnode, dim_hdf_names[j], dimsizes[j]);
        }
        grpnode->dim_rec = dimrec;

      /*Dimension mappings*/
        nmaps = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTMAP, &str_buf_size);

        if (nmaps > 0)
        {
            long *off, *inc;
            off = NclMalloc(nmaps * sizeof(long));
            inc = NclMalloc(nmaps * sizeof(long));
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }
            nmaps = HE5_SWinqmaps(HE5_SWid, buffer, off, inc);
            _addHE5DimMapInfo(grpnode,sw_ncl_names[i],nmaps,buffer,off,inc);
            NclFree(off);
            NclFree(inc);
        }

      /*Indexed Dimension Mappings*/
        nmaps = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTIMAP, &str_buf_size);
        if (nmaps > 0)
        {
            hsize_t *sizes;
            sizes = NclMalloc(nmaps * sizeof(long));
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }
            nmaps = HE5_SWinqidxmaps(HE5_SWid, buffer, sizes);
            _addHE5IndexedMapVars(grpnode,sw_hdf_names[i],sw_ncl_names[i],nmaps,buffer,sizes);
        }

        /* Geolocation fields */
        ngeofields = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTGFLD, &str_buf_size);

        ndata = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTDFLD, &str_buf_size);

	if(ndata + ngeofields)
	{
            varrec = _NclFileVarAlloc(ndata + ngeofields);
            varrec->gid = HE5_SWid;
	}
	else
	    varrec = NULL;
        grpnode->var_rec = varrec;

        if((ndata > max_var) || (ngeofields > max_var))
        {
            j = (ngeofields > ndata)?ngeofields:ndata;
            _reallocnames(j, &max_var, var_hdf_names, var_ncl_names);
            var_ranks  = (int *)NclRealloc(var_ranks, max_var * sizeof(int));
        }

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }

        ndata = HE5_SWinqdatafields(HE5_SWid,buffer,var_ranks,NULL);
        buffer[str_buf_size] = '\0';

      /*
       *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
       *fprintf(stderr, "\tndata = %d, varnames: %s\n", ndata, buffer);
       */

        HE5ParseName(buffer,var_hdf_names,var_ncl_names,ndata);

        for(j = 0; j < ndata; ++j)
        {
            no_fill_value = TRUE;

            varnode = &(varrec->var_node[j]);
            varnode->id = j;
            varnode->gid = HE5_SWid;
            varnode->name = var_hdf_names[j];
            varnode->real_name = sw_hdf_names[i];
            varnode->value = NULL;
            varnode->is_chunked = 0;
            varnode->is_compound = 0;

            nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NULL,&str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),buffer,&str_buf_size);
            buffer[str_buf_size] = '\0';

            if(0 < nlocatts)
            {
              /*
               *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\tnlocatts = %d, attnames: %s\n", nlocatts, buffer);
               *fprintf(stderr, "\tVar %d, name: %s, nlocatts = %d, attnames: %s\n",
               *                 j, NrmQuarkToString(var_hdf_names[j]), nlocatts, buffer);
               */

                if(max_att < nlocatts)
                {
                    _reallocnames(nlocatts, &max_att, att_hdf_names, att_ncl_names);
                }

                HE5ParseName(buffer,att_hdf_names,att_ncl_names,nlocatts);

                attrec = _NclFileAttAlloc(nlocatts);
                assert(attrec);

                attrec->id = HE5_SWid;
                attrec->gid = HE5_SWid;
                attrec->aid = j;

                varnode->att_rec = attrec;

                for(k = 0; k < nlocatts; k++)
                {
                    status = HE5_SWlocattrinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),
                                               NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size);
                    if(status)
                        continue;

                  /*
                   *Note: size should be datatype size in bytes
                   *(max number of charcters for character string arrays
                   *status = HE5_EHinqlocdatatype(HE5_SWid, NrmQuarkToString(att_hdf_names[k]),
                   *                        &dtype, &classid, &order, &size);
                   *tmp_value = (void*)NclCalloc((att_size+1) * size, 1);
                   *tmp_value = (void*)NclCalloc(size, att_size);
                   */

                    attnode->type = HE5MapTypeNumber(att_type);
                    if(NCL_string == attnode->type)
                        tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
                    else
                        tmp_value = (void*)NclCalloc(att_size, _NclSizeOf(baseNclType));

                    status = HE5_SWreadlocattr(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),
                                               NrmQuarkToString(att_hdf_names[k]),tmp_value);
                    if(status < 0)
                    {
                        printf("\tHE5_SWreadattr Failed.\n");
                        free(tmp_value);
                        continue;
                    }

                    if((fv_quark == att_hdf_names[k]) || (mv_quark == att_hdf_names[k]))
                        no_fill_value = FALSE;

                    attnode = &(attrec->att_node[k]);
                    attnode->is_virtual = 0;
                    attnode->is_opaque = 0;
                    attnode->is_vlen = 0;
                    attnode->is_compound = 0;
                    attnode->name = att_hdf_names[k];

                    if(NCL_string == attnode->type)
                    {
                        NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                        *new_value = NrmStringToQuark(tmp_value);
                        att_size = 1;

                        _addHE5Att(attnode, att_ncl_names[k], new_value, att_size, attnode->type);
                        NclFree(tmp_value);
                    }
                    else
                    {
                        _addHE5Att(attnode, att_ncl_names[k], tmp_value, att_size, attnode->type);
                    }
                }
            }

            if(HE5_SWfieldinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),
                       &count,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
            {
		ndims = count;
              /*
               *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\tndims = %d\n", ndims);
               */

                if(0 < ndims)
                {
                    dimrec = _NclFileDimAlloc(ndims);
                    dimrec->gid = HE5_SWid;
                    varnode->dim_rec = dimrec;

                    buffer[str_buf_size] = '\0';
                    HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);
                    for(k = 0; k < ndims; ++k)
                    {
                      /*
                       *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                       *fprintf(stderr, "\tVar %d, dim %d: size = %d, name: <%s>, ncl_name: <%s>\n",
                       *                 (int)j, (int)k, (int)dimsizes[k],
                       *                 NrmQuarkToString(dim_hdf_names[k]),
                       *                 NrmQuarkToString(dim_ncl_names[k]));
                       */

                        dimnode = &(dimrec->dim_node[k]);

                        dimnode->id = k;

			if(0 == str_buf_size)
		        {
			    int n, unlimited = 1;
			    for(n = 0; n < grpnode->dim_rec->n_dims; ++n)
                            {
                                if(dimsizes[k] == grpnode->dim_rec->dim_node[n].size)
				{
				    dim_hdf_names[k] = grpnode->dim_rec->dim_node[n].name;
				    unlimited = 0;
				    break;
                                }
                            }

			    if(unlimited)
                            {
				dim_hdf_names[k] = grpnode->dim_rec->dim_node[0].name;
				if(dimsizes[k] != grpnode->dim_rec->dim_node[0].size)
				    grpnode->dim_rec->dim_node[0].size = dimsizes[k];
		            }
		        }

                        _addHE5Dim(dimnode, dim_hdf_names[k], dimsizes[k]);
                    }
                }

                varnode->type = HE5MapTypeNumber(tmp_type);

              /*
               *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\tvar %d name: <%s>\n", j, NrmQuarkToString(varnode->name));
               */
#if 0
                if(HE5unsigned(tmp_type))
                {
                    int *is_unsigned = (int*)NclMalloc(sizeof(int));
                    *is_unsigned = 1;
                    _addNclAttNode(&attrec, NrmStringToQuark("unsigned"), NCL_logical, 1, (void*)is_unsigned);
                    NclFree(is_unsigned);
                    ++nlocatts;
                }
#endif
                _synchHE5GrpVarDims(grpnode, varnode);
            }

#if 0
            if(no_fill_value)
            {
                if(HE5_SWgetfillvalue(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1)
                {
                    tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                    *tmp_missing = missing;
                    _addNclAttNode(&attrec, NrmStringToQuark("_FillValue"), varnode->type, 1, (void*)tmp_missing);
                    NclFree(tmp_missing);
                    ++nlocatts;

                    if(1 == nlocatts)
                    {
                        attrec->id = -1;
                        attrec->gid = -1;
                        attrec->aid = -1;
                        varnode->att_rec = attrec;
                    }
                }
            }
#endif
        }

        /* Geolocation fields */
        ngeofields = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTGFLD, &str_buf_size);

        if(ngeofields > 0)
        {
            NclBasicDataTypes baseNclType = NCL_none;

            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            ngeofields = HE5_SWinqgeofields(HE5_SWid,buffer,var_ranks,NULL);
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,var_hdf_names,var_ncl_names,ngeofields);

            for(j = 0; j < ngeofields; ++j)
            {
                no_fill_value = TRUE;

                varnode = &(varrec->var_node[j + ndata]);
                varnode->id = j + ndata;
                varnode->gid = HE5_SWid;
                varnode->name = var_hdf_names[j];
                varnode->real_name = sw_hdf_names[i];
                varnode->value = NULL;
                varnode->is_chunked = 0;
                varnode->is_compound = 0;

                nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NULL,&str_buf_size);
                if (str_buf_size >= cur_buf_size)
                {
                    while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                    buffer = NclRealloc(buffer, cur_buf_size);
                }

                nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),buffer,&str_buf_size);
                buffer[str_buf_size] = '\0';

                if(nlocatts > max_att)
                {
                    _reallocnames(nlocatts, &max_att, att_hdf_names, att_ncl_names);
                }

                HE5ParseName(buffer,att_hdf_names,att_ncl_names,nlocatts);

		if(nlocatts)
		{
                    attrec = _NclFileAttAlloc(nlocatts);
                    assert(attrec);

                    attrec->id = -1;
                    attrec->gid = HE5_SWid;
                    attrec->aid = j;
                    varnode->att_rec = attrec;
		}

                for(k = 0; k < nlocatts; ++k)
                {       
                    status = HE5_SWlocattrinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size);
                    if(status)
                        continue;

                    baseNclType = HE5MapTypeNumber(att_type);

                  /*
                   *Note: size should be datatype size in bytes
                   *(max number of charcters for character string arrays
                   *status = HE5_EHinqlocdatatype(HE5_SWid, NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(att_hdf_names[k]),
                   *                        &dtype, &classid, &order, &size);
                   *if(NCL_string == baseNclType)
                   *    tmp_value = (void*)NclCalloc((att_size+1) * size, 1);
                   *else
                   *    tmp_value = (void*)NclCalloc(size, att_size);
                   */

                    if(NCL_string == baseNclType)
                        tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
                    else
                        tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(baseNclType));

                    status = HE5_SWreadlocattr(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(att_hdf_names[k]),tmp_value);
                    if(status < 0)
                    {
                        printf("\tHE5_SWreadattr Failed.\n");
                        free(tmp_value);
                        continue;
                    }

                    attnode = &(attrec->att_node[k]);
                    attnode->is_virtual = 0;
                    attnode->is_opaque = 0;
                    attnode->is_vlen = 0;
                    attnode->is_compound = 0;
                    attnode->name = att_hdf_names[k];

                  /*
                   *fprintf(stderr, "file %s, line: %d\n", __LINE__, __FILE__);
                   *fprintf(stderr,"\tVar %d, Att No. %d, name: <%s>\n", j, k, NrmQuarkToString(att_hdf_names[k]));
                   */

                    if((fv_quark == att_hdf_names[k]) || (mv_quark == att_hdf_names[k]))
                        no_fill_value = FALSE;

                    if(NCL_string == baseNclType)
                    {
                        NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                        *new_value = NrmStringToQuark(tmp_value);
                        att_size = 1;
                        _addHE5Att(attnode, att_ncl_names[k], new_value, att_size, baseNclType);
                        NclFree(tmp_value);
                    }
                    else
                        _addHE5Att(attnode, att_ncl_names[k], tmp_value,att_size, baseNclType);
                }
#if 0
                if(no_fill_value)
                {
                    if(HE5_SWgetfillvalue(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1)
                    {
                        tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                        *tmp_missing = missing;
                        _addNclAttNode(&attrec, NrmStringToQuark("_FillValue"), varnode->type, 1, (void*)tmp_missing);
                        NclFree(tmp_missing);
                        ++nlocatts;

                        if(1 == nlocatts)
                        {
                            attrec->id = -1;
                            attrec->gid = HE5_SWid;
                            attrec->aid = -1;
                            varnode->att_rec = attrec;
                        }
                    }
                }
#endif
                if(HE5_SWfieldinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),
                           &count,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
                {
                    ndims = count;
                    buffer[str_buf_size] = '\0';

                    if(ndims > max_dim)
                    {
                        _reallocnames(ndims, &max_dim, dim_hdf_names, dim_ncl_names);
                        dimsizes = (hsize_t *)NclRealloc(dimsizes, sizeof(hsize_t)*max_dim);
                    }

                    dimrec = _NclFileDimAlloc(ndims);
                    dimrec->gid = HE5_SWid;
                    varnode->dim_rec = dimrec;
    
                  /*
                   *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                   *fprintf(stderr, "\tndims = %d\n", ndims);
                   */
 
                    HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);
                    for(k = 0; k < ndims; ++k)
                    {
                      /*
                       *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                       *fprintf(stderr, "\tVar %d, dim %d: size = %d, name: <%s>, ncl_name: <%s>\n",
                       *                 (int)j, (int)k, (int)dimsizes[k],
                       *                 NrmQuarkToString(dim_hdf_names[k]),
                       *                 NrmQuarkToString(dim_ncl_names[k]));
                       */

                        dimnode = &(dimrec->dim_node[k]);

                        dimnode->id = k;

			if(0 == str_buf_size)
		        {
			    int n, unlimited = 1;
			    for(n = 0; n < grpnode->dim_rec->n_dims; ++n)
                            {
                                if(dimsizes[k] == grpnode->dim_rec->dim_node[n].size)
				{
				    dim_hdf_names[k] = grpnode->dim_rec->dim_node[n].name;
				    unlimited = 0;
				    break;
                                }
                            }

			    if(unlimited)
                            {
				dim_hdf_names[k] = grpnode->dim_rec->dim_node[0].name;
				if(dimsizes[k] != grpnode->dim_rec->dim_node[0].size)
				    grpnode->dim_rec->dim_node[0].size = dimsizes[k];
		            }
		        }

                        _addHE5Dim(dimnode, dim_hdf_names[k], dimsizes[k]);
                    }

                    varnode->type = HE5MapTypeNumber(tmp_type);
                    varnode->name = var_hdf_names[j];

                  /*
                   *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                   *fprintf(stderr, "\tvar %d name: <%s>\n", j, NrmQuarkToString(varnode->name));
                   */
#if 0
                    if(HE5unsigned(tmp_type))
                    {
                        int *is_unsigned = (int*)NclMalloc(sizeof(int));
                        *is_unsigned = 1;
                        _addNclAttNode(&attrec, NrmStringToQuark("unsigned"), NCL_logical, 1, (void*)is_unsigned);
                        NclFree(is_unsigned);
                        ++nlocatts;

                        if(1 == nlocatts)
                        {
                            attrec->id = -1;
                            attrec->gid = -1;
                            attrec->aid = -1;
                            varnode->att_rec = attrec;
                        }
                    }
#endif
                }
            }
        }
        HE5_SWdetach(HE5_SWid);    
    }

    HE5_SWclose(HE5_SWfid);

    NclFree(buffer);

    NclFree(sw_hdf_names);
    NclFree(sw_ncl_names);

    NclFree(dim_hdf_names);
    NclFree(dim_ncl_names);
    NclFree(dimsizes);

    NclFree(var_hdf_names);
    NclFree(var_ncl_names);
    NclFree(var_ranks);

    NclFree(att_ncl_names);
    NclFree(att_hdf_names);
}

static void getHE5GridData(NclFileGrpNode *parentgrpnode, NclQuark path)
{
    hid_t HE5_GDfid = 0;
    hid_t HE5_GDid = 0;

    long natts = FAIL;
    long nlocatts = FAIL;

    long ngd;
    long ndata = 0;
    long ndims;
    long ngrp_atts;

    long str_buf_size;

    int projcode = -1;
    int zonecode = -1;
    int spherecode = -1;
    int origincode = -1;
    int pixregcode = -1;
    double projparm[MAX_VAR];
    double upper_left[2],lower_right[2];

    char maxdimlist[HE5_BUF_SIZE];

    NclQuark *gd_hdf_names;
    NclQuark *gd_ncl_names;
    int max_gd = MAX_GD;

    NclQuark *dim_hdf_names;
    NclQuark *dim_ncl_names;
    hsize_t  *dimsizes;
    int max_dim = MAX_DIM;

    NclQuark *var_hdf_names;
    NclQuark *var_ncl_names;
    int      *var_ranks = NULL;
    int max_var = MAX_VAR;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    NclQuark fv_quark = NrmStringToQuark("_FillValue");
    NclQuark mv_quark = NrmStringToQuark("MissingValue");

    hid_t tmp_type;

    void *tmp_value;
    hid_t att_type;
    hsize_t att_size;

    NclScalar missing;
    NclScalar *tmp_missing;

    int *is_unsigned;
    long xdimsize;
    long ydimsize;
    char *buffer;
    int cur_buf_size = HE5_BUF_SIZE;

    boolean no_fill_value = TRUE;
    int i,j,k;

    int status;
    NrmQuark qproj_name = NrmNULLQUARK;
    char *tmp_hdf_name;

    NclFileGrpNode   *grpnode = NULL;
    NclFileGrpRecord *grprec  = NULL;

    NclFileVarRecord *varrec  = NULL;
    NclFileVarNode   *varnode = NULL;

    NclFileAttNode   *attnode = NULL;
    NclFileAttRecord *attrec  = NULL;

    NclFileDimNode   *dimnode = NULL;
    NclFileDimRecord *dimrec  = NULL;

    hid_t       dtype;
    H5T_class_t classid;
    H5T_order_t order;
    size_t      size;
    int         count;

  /*
   *NrmQuark dim_names[2];
   *hsize_t dim_sizes[2];
   */

    ngd = HE5_GDinqgrid(NrmQuarkToString(path),NULL,&str_buf_size);

    while(ngd > max_gd)
        max_gd *= 2;

    gd_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_gd);
    gd_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_gd);

    var_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ranks     = (int *)NclMalloc(sizeof(int)*max_var);

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dimsizes = (hsize_t *)NclMalloc(sizeof(hsize_t)*max_dim);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    grprec = _NclFileGrpAlloc(ngd);

    parentgrpnode->grp_rec = grprec;

    parentgrpnode->path = path;

    if(str_buf_size >= cur_buf_size)
    {
        while(str_buf_size >= cur_buf_size)
            cur_buf_size *= 2;
    }

    buffer = NclMalloc(cur_buf_size);
    ngd = HE5_GDinqgrid(NrmQuarkToString(path),buffer,&str_buf_size);
    buffer[str_buf_size] = '\0';
    HE5ParseName(buffer,gd_hdf_names,gd_ncl_names,ngd);

    HE5_GDfid = HE5_GDopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    parentgrpnode->gid = HE5_GDfid;
    parentgrpnode->fid = HE5_GDfid;
    parentgrpnode->define_mode = GRID;

  /*
   *global attributes from file
   */
    ngrp_atts = HE5_EHinqglbattrs(HE5_GDfid,NULL,&str_buf_size);

  /*
   *fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
   */

    if(ngrp_atts > 0 )
    {
        NclQuark tmpquark0 = NrmStringToQuark("SourceChannel");
        NclQuark tmpquark1 = NrmStringToQuark("SurfaceReflectanceFlagCode");
        NclQuark tmpquark2 = NrmStringToQuark("SurfaceReflectanceFlagName");

        attrec = _NclFileAttAlloc(ngrp_atts);
        assert(attrec);

        attrec->id = -1;
        attrec->gid = HE5_GDfid;
        attrec->aid = -1;

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }

        ngrp_atts = HE5_EHinqglbattrs(HE5_GDfid,buffer,&str_buf_size);
        if(max_att < ngrp_atts)
        {
            _reallocnames(ngrp_atts, &max_att, att_hdf_names, att_ncl_names);
        }

      /*
       *fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tngrp_atts = %ld, attnames: <%s>\n", ngrp_atts, buffer);
       */

        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer, att_hdf_names, att_ncl_names, ngrp_atts);

        for(k = 0; k < ngrp_atts; k++)
        { 
            attnode = &(attrec->att_node[k]);
            attnode->is_virtual = 0;
            attnode->is_opaque = 0;
            attnode->is_vlen = 0;
            attnode->is_compound = 0;
            attnode->name = att_hdf_names[k];

          /*
           *We have problem to read attribute: "SurfaceReflectanceFlagCode".
           *So we skip it.
           */

            if(tmpquark0 == att_hdf_names[k])
            {
                char *tmpstr = "UV2";
                att_size = strlen(tmpstr);
                tmp_value = (void*)NclCalloc(att_size+1, 1);
                memcpy(tmp_value, tmpstr, att_size);
                _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                           (int) att_size, NCL_string);
            }
            else if(tmpquark1 == att_hdf_names[k])
            {
                char *tmpstr = "255,250,240,230,220,210,195,185,175,100,90,80,70,60,0";
                att_size = strlen(tmpstr);
                tmp_value = (void*)NclCalloc(att_size+1, 1);
                memcpy(tmp_value, tmpstr, att_size);
                _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                           (int) att_size, NCL_string);
            }
            else if(tmpquark2 == att_hdf_names[k])
            {
                char *tmpstr = "not enough data";
                att_size = strlen(tmpstr);
                tmp_value = (void*)NclCalloc(att_size+1, 1);
                memcpy(tmp_value, tmpstr, att_size);
                _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                           (int) att_size, NCL_string);
            }
            else
            {
                if(HE5_EHglbattrinfo(HE5_GDfid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
                    NclBasicDataTypes baseNclType = HE5MapTypeNumber(att_type);
                  /*
                   *Note: size should be datatype size in bytes
                   *(max number of charcters for character string arrays
                   */
                    status = HE5_EHinqglbdatatype(HE5_GDfid, NrmQuarkToString(att_hdf_names[k]),
                                              &dtype, &classid, &order, &size);

                  /*
                   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tstatus = %ld, dtype = %ld, classid = %ld\n",
                   *       (long) status, (long) dtype, (long) classid);
                   *fprintf(stderr, "\torder = %ld, array_element_Byte_size = %ld\n",
                   *       (long) order, (long) size);
                   */

                    if(NCL_string == baseNclType)
                    {
                        if(1 == att_size)
                            tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
			else
                            tmp_value = (void*)NclCalloc((att_size+1) * size, 1);
		    }
                    else
                        tmp_value = (void*)NclCalloc(size, att_size);

                    if(HE5_EHreadglbattr(HE5_GDfid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                      /*
                       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                       *fprintf(stderr, "Att No. %d, name: <%s>, att_size = %ld, size = %d\n",
                       *                 k, NrmQuarkToString(att_hdf_names[k]), att_size, size);
                       */

                        if(NCL_string == baseNclType)
                        {
                            NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                            *new_value = NrmStringToQuark(tmp_value);

                            _addHE5Att(attnode, att_ncl_names[k], new_value, 1, baseNclType);

                            NclFree(tmp_value);
                        }
                        else
                        {
                            _addHE5Att(attnode, att_ncl_names[k], tmp_value, (int) att_size, baseNclType);
                        }
                    }
                }
            }
        }
        parentgrpnode->att_rec = attrec;
    }

    for(i = 0; i < ngd; i++)
    {
        tmp_hdf_name = _make_proper_string_end(NrmQuarkToString(gd_hdf_names[i]));
        HE5_GDid = HE5_GDattach(HE5_GDfid,tmp_hdf_name);
        free(tmp_hdf_name);
        if(0 >= HE5_GDid)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclNewHDFEOS5: An internal HDF error occurred while reading (%s) can't continue",
                  NrmQuarkToString(path)));
            return;
        }

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\ti = %d, HE5_GDid = %d, grpname: <%s>\n", i, HE5_GDid, NrmQuarkToString(gd_hdf_names[i]));
       */

        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        grprec->grp_node[i] = grpnode;
        grpnode->path = parentgrpnode->path;
        grpnode->parent = parentgrpnode;
        grpnode->pid = HE5_GDfid;
        grpnode->fid = HE5_GDfid;
        grpnode->gid = HE5_GDid;
        grpnode->pname = -1;
        grpnode->name = gd_hdf_names[i];
        grpnode->define_mode = GRID;

        status = HE5_GDprojinfo(HE5_GDid,&projcode,&zonecode,&spherecode,projparm);
        if (status == FAIL)
        {
            projcode = -1;
        }
        else
        {
            switch(projcode)
            {
            case HE5_GCTP_GEO:
                qproj_name = NrmStringToQuark("Geographic");
                break;
            case HE5_GCTP_UTM:
                qproj_name = NrmStringToQuark("Universal Transverse Mercator");
                break;
            case HE5_GCTP_ALBERS:
                qproj_name = NrmStringToQuark("Albers Conical Equal_Area");
                break;
            case HE5_GCTP_LAMCC:
                qproj_name = NrmStringToQuark("Lambert Conformal Conic");
                break;
            case HE5_GCTP_MERCAT:
                qproj_name = NrmStringToQuark("Mercator");
                break;
            case HE5_GCTP_PS:
                qproj_name = NrmStringToQuark("Polar Stereographic");
                break;
            case HE5_GCTP_POLYC:
                qproj_name = NrmStringToQuark("Polyconic");
                break;
            case HE5_GCTP_TM:
                qproj_name = NrmStringToQuark("Transverse Mercator");
                break;
            case HE5_GCTP_LAMAZ:
                qproj_name = NrmStringToQuark("Lambert Azimuthal Equal Area");
                break;
            case HE5_GCTP_HOM:
                qproj_name = NrmStringToQuark("Hotine Oblique Mercator");
                break;
            case HE5_GCTP_SOM:
                qproj_name = NrmStringToQuark("Space Oblique Mercator");
                break;
            case HE5_GCTP_GOOD:
                qproj_name = NrmStringToQuark("Interrupted Goode Homolosine");
                break;
            case HE5_GCTP_ISINUS:
                qproj_name = NrmStringToQuark("Integerized Sinusoidal Projection");
                break;
            case HE5_GCTP_CEA:
            case HE5_GCTP_BCEA:
                qproj_name = NrmStringToQuark("Cylindrical Equal-Area Projection");
                break;
            default:
                printf("Unsupported projection: projcode: %d, zonecode: %d, spherecode: %d, projparm: %f\n",
                        projcode, zonecode, spherecode, projparm[1]);
                NhlPError(NhlFATAL,NhlEUNKNOWN, "NclNewHDFEOS5: Unsupported projection found, parameter as above.");
                return;
            }

          /*
           *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tprojcode = %d, qproj_name: <%s>\n", projcode, NrmQuarkToString(qproj_name));
           */
        }            

        natts = HE5_GDinqattrs(HE5_GDid,NULL,&str_buf_size);
        if(natts > 0 )
        {
            attrec = _NclFileAttAlloc(natts);
            assert(attrec);

            attrec->id = -1;
            attrec->gid = HE5_GDid;
            attrec->aid = -1;

            if(str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            natts = HE5_GDinqattrs(HE5_GDid,buffer,&str_buf_size);

          /*
           *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tnatts = %ld, attnames: <%s>\n", natts, buffer);
           */

            if(max_att < natts)
            {
                _reallocnames(natts, &max_att, att_hdf_names, att_ncl_names);
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,att_hdf_names,att_ncl_names,natts);
            for(k = 0; k < natts; ++k)
            { 
                attnode = &(attrec->att_node[k]);
                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = att_hdf_names[k];

              /*
               *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr,"\tAtt No. %d, name: <%s>\n", k, NrmQuarkToString(att_hdf_names[k]));
               */


                if(HE5_GDattrinfo(HE5_GDid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
		    attnode->type = HE5MapTypeNumber(att_type);
                    if(NCL_string == attnode->type)
                        tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
		    else
                        tmp_value = (void*)NclCalloc(att_size, _NclSizeOf(attnode->type));
                    if(HE5_GDreadattr(HE5_GDid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        if(NCL_string == attnode->type)
                        {
                            NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                            *new_value = NrmStringToQuark(tmp_value);

                            _addHE5Att(attnode, att_ncl_names[k], new_value, 1, attnode->type);

                            NclFree(tmp_value);
                        }
			else
                            _addHE5Att(attnode, att_ncl_names[k], tmp_value, (int) att_size, attnode->type);
                    }
                }
            }
            grpnode->att_rec = attrec;
        }

        ndims = HE5_GDnentries(HE5_GDid,HE5_HDFE_NENTDIM,&str_buf_size);
        if(ndims > 0 )
        {
          /*
           *fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
           *fprintf(stderr, "\tndims = %ld\n", ndims);
           */

            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            ndims = HE5_GDinqdims(HE5_GDid,buffer,dimsizes);
            if(max_dim < ndims)
            {
                _reallocnames(ndims, &max_dim, dim_hdf_names, dim_ncl_names);
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

            dimrec = _NclFileDimAlloc(ndims);
            dimrec->gid = HE5_GDid;

            for(j = 0; j < ndims; ++j)
            {
              /*
               *fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\tDim No. %d: size = %ld, name: <%s> ncl-name: <%s>\n",
               *                 j, (long)dimsizes[j],
               *                 NrmQuarkToString(dim_hdf_names[j]),
               *                 NrmQuarkToString(dim_ncl_names[j]));
               */

                dimnode = &(dimrec->dim_node[j]);

                dimnode->id = j;

                _addHE5Dim(dimnode, dim_hdf_names[j], dimsizes[j]);
            }
            grpnode->dim_rec = dimrec;
        }
       
        ndata = HE5_GDnentries(HE5_GDid, HE5_HDFE_NENTDFLD, &str_buf_size);

      /*
       *fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
       *fprintf(stderr, "\nndata = %ld\n", ndata);
       */

        if(ndata > max_var)
        {
             _reallocnames(ndata, &max_var, var_hdf_names, var_ncl_names);
             var_ranks = (int *)NclRealloc(var_ranks, sizeof(int)*max_var);
        }

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }
        ndata = HE5_GDinqfields(HE5_GDid,buffer,var_ranks,NULL);
        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,var_hdf_names,var_ncl_names,ndata);

        varrec = _NclFileVarAlloc(ndata);
        varrec->gid = HE5_GDid;
        grpnode->var_rec = varrec;

        for(j = 0; j < ndata; ++j)
        {
            varnode = &(varrec->var_node[j]);
            varnode->id = j;
            varnode->gid = HE5_GDid;
            varnode->name = var_hdf_names[j];
            varnode->real_name = gd_hdf_names[i];
            varnode->value = NULL;
            varnode->is_chunked = 0;
            varnode->is_compound = 0;

            no_fill_value = TRUE;

            nlocatts = HE5_GDinqlocattrs(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),NULL,&str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_GDinqlocattrs(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),buffer,&str_buf_size);

            if(max_att < nlocatts)
            {
                _reallocnames(nlocatts, &max_att, att_hdf_names, att_ncl_names);
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,att_hdf_names,att_ncl_names,nlocatts);

            attrec = _NclFileAttAlloc(nlocatts);
            assert(attrec);

            attrec->id = -1;
            attrec->gid = HE5_GDid;
            attrec->aid = j;

            varnode->att_rec = attrec;

            for(k = 0; k < nlocatts; ++k)
            {
                status = HE5_GDlocattrinfo(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size);
                if(status)
                    continue;

                attnode = &(attrec->att_node[k]);
                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = att_hdf_names[k];

              /*
               *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr,"\tVar %d, Att No. %d, name: <%s>, att_size = %ld\n",
	       *		  j, k, NrmQuarkToString(att_hdf_names[k]), att_size);
               */

                attnode->type = HE5MapTypeNumber(att_type);
                if(NCL_string == attnode->type)
                    tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
		else
                    tmp_value = (void *) NclCalloc(att_size, _NclSizeOf(attnode->type));
                status = HE5_GDreadlocattr(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(att_hdf_names[k]),tmp_value);
                if(status < 0)
                {
                    fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
                    fprintf(stderr, "\tHE5_GDreadattr Failed.\n");
                    free(tmp_value);
                    continue;
                }

                if((fv_quark == att_hdf_names[k]) || (mv_quark == att_hdf_names[k]))
                    no_fill_value = FALSE;

                if(NCL_string == attnode->type)
                {
                    NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                    *new_value = NrmStringToQuark(tmp_value);

                    _addHE5Att(attnode, att_ncl_names[k], new_value, 1, attnode->type);

                    NclFree(tmp_value);
                }
                else
                    _addHE5Att(attnode, att_ncl_names[k], tmp_value, att_size, attnode->type);
            }

            if(no_fill_value)
            {
                if(HE5_GDgetfillvalue(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1)
                {
                    tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                    *tmp_missing = missing;
                    _addNclAttNode(&attrec, NrmStringToQuark("_FillValue"), varnode->type, 1, (void*)tmp_missing);
                    NclFree(tmp_missing);
                    ++nlocatts;
                }
            }

            if(HE5_GDfieldinfo(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),&count,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
            {
                ndims = count;
                buffer[str_buf_size] = '\0';
                HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

                dimrec = _NclFileDimAlloc(ndims);
                dimrec->gid = HE5_GDid;
                varnode->dim_rec = dimrec;

              /*
               *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\nndims = %d\n", ndims);
               */

                for(k = 0; k < ndims; ++k)
                {
                  /*
                   *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                   *fprintf(stderr, "Var %d, dim %d: size = %d, name: <%s>, ncl_name: <%s>\n",
                   *                 (int)j, (int)k, (int)dimsizes[k],
                   *                 NrmQuarkToString(dim_hdf_names[k]),
                   *                 NrmQuarkToString(dim_ncl_names[k]));
                   */

                    dimnode = &(dimrec->dim_node[k]);

                    dimnode->id = k;

                    _addHE5Dim(dimnode, dim_hdf_names[k], dimsizes[k]);
                }

                varnode->type = HE5MapTypeNumber(tmp_type);
                varnode->name = var_hdf_names[j];

              /*
               *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\tvar %d name: <%s>\n", j, NrmQuarkToString(varnode->name));
               */

                if(HE5unsigned(tmp_type))
                {
                    is_unsigned = (int*)NclMalloc(sizeof(int));
                    *is_unsigned = 1;
                    _addNclAttNode(&attrec, NrmStringToQuark("unsigned"), NCL_logical, 1, (void*)is_unsigned);
                    NclFree(is_unsigned);
                    ++nlocatts;
                }

                _synchHE5GrpVarDims(grpnode, varnode);
            }

            if(projcode > -1)
            {
                NrmQuark *tmp_quark = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
                *tmp_quark = qproj_name;
                _addNclAttNode(&attrec, NrmStringToQuark("projection"), NCL_string, 1, (void*)tmp_quark);
                NclFree(tmp_quark);
            }

            if((var_hdf_names[j] != NrmStringToQuark("Longitude")) &&
               (var_hdf_names[j] != NrmStringToQuark("Latitude")))
            {
                NrmQuark *tmp_quark = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
                *tmp_quark = var_hdf_names[j];
                _addNclAttNode(&attrec, NrmStringToQuark("long_name"), NCL_string, 1, (void*)tmp_quark);
                NclFree(tmp_quark);
            }

          /*
           *Some he5 files do not have GridOrigin, and PixelRegistration,
           *which result HE5_GDorigininfo and HE5_GDpixreginfo failure.
           *By default, as these two functions (actually all other eos5 functions)
           *they print some diagnostic info.
           *But some users do not want to see these, so we turn it off for these
           *two functions.
           *
           *Wei Huang, 06/30/2011.
           */
            HE5_EHset_error_on(2, 0);
            status = HE5_GDorigininfo(HE5_GDid,&origincode);
            if(status == FAIL)
            {
                NHLPERROR((NhlINFO,NhlEUNKNOWN,
                    "NclHDFEOS GDorigininfo: origincode = %d\n", origincode));
                /*origincode = HE5_HDFE_GD_UL;*/
            }
    
            status = HE5_GDpixreginfo(HE5_GDid,&pixregcode);
            if(status == FAIL)
            {
                NHLPERROR((NhlINFO,NhlEUNKNOWN,
                    "NclHDFEOS HE5_GDpixreginfo: pixregcode = %d\n", pixregcode));
                pixregcode = HE5_HDFE_CENTER;
            }
          /*Turn error diagnose back on*/
            HE5_EHset_error_on(1, 0);
    
            status = HE5_GDgridinfo(HE5_GDid,&xdimsize,&ydimsize,upper_left,lower_right);
            if(status == FAIL)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "NclHDFEOS HE5_GDgridinfo: xdimsize = %d, ydimsize = %d\n", xdimsize, ydimsize));
            }
    
            status = HE5_GDprojinfo(HE5_GDid,&projcode,&zonecode,&spherecode,projparm);
            if (status == FAIL)
            {
                NHLPERROR((NhlWARNING,NhlEUNKNOWN, 
                      "NclNewHDFEOS5: Invalid projection information for GRID (%s); no coordinates will be provided",
                      NrmQuarkToString(gd_hdf_names[i])));
            }
    
          /*
           *dim_names[1] = NrmStringToQuark("XDim");
           *dim_names[0] = NrmStringToQuark("YDim");
           *dim_sizes[0] = ydimsize;
           *dim_sizes[1] = xdimsize;
           */

            if (projcode != HE5_GCTP_GEO)
            {
                long cols[4],rows[4];
                double lat2d[4], lon2d[4];
    
                cols[0] = 0;
                rows[0] = 0;
                cols[1] = xdimsize - 1;
                rows[1] = 0;
                cols[2] = xdimsize - 1;
                rows[2] = ydimsize - 1;
                cols[3] = 0;
                rows[3] = ydimsize - 1;
    
                if(origincode < 0)
                {
                    origincode = MyHE5setOrigincode(upper_left, lower_right);
                }
    
                HE5_GDij2ll(projcode,zonecode,projparm,spherecode,xdimsize,ydimsize,
                    upper_left,lower_right,4,rows,cols,lon2d,lat2d,pixregcode,origincode);
    

                fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                fprintf(stderr, "\tprojcode = %d, HE5_GCTP_GEO = %d\n", projcode, HE5_GCTP_GEO);
                fprintf(stderr, "\txdimsize = %ld, ydimsize = %ld\n", xdimsize, ydimsize);
                fprintf(stderr, "\nNEED TO WORK IN THIS PIECE\n\n");
            }
        }
        HE5_GDdetach(HE5_GDid);    
    }

    HE5_GDclose(HE5_GDfid);

    NclFree(buffer);

    NclFree(gd_hdf_names);
    NclFree(gd_ncl_names);

    NclFree(dim_hdf_names);
    NclFree(dim_ncl_names);
    NclFree(dimsizes);

    NclFree(var_hdf_names);
    NclFree(var_ncl_names);
    NclFree(var_ranks);

    NclFree(att_ncl_names);
    NclFree(att_hdf_names);
}

static void getHE5PointData(NclFileGrpNode *parentgrpnode, NclQuark path)
{
    hid_t HE5_PTfid = FAIL;
    hid_t HE5_PTid = FAIL;

    int natts = 0;
    int nlocatts = 0;

    herr_t status = FAIL;

    long npt;
    int nfields = 0;
    int nlevels = 0;
    int nrecs = 0;
    int ndims = 0;

    long str_buf_size;
    hsize_t *dimsizes;
    int max_nlevels = MAX_DIM;

    NclQuark *pt_hdf_names;
    NclQuark *pt_ncl_names;
    int max_pt = MAX_PT;

    NclQuark *fld_hdf_names;
    NclQuark *fld_ncl_names;
    int max_fld = MAX_FLD;

    NclQuark *lvl_hdf_names;
    NclQuark *lvl_ncl_names;
    int max_lvl = MAX_LVL;

    NclQuark *dim_hdf_names;
    NclQuark *dim_ncl_names;
    int max_dim = 2;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    NclFileGrpNode   *grpnode = NULL;
    NclFileGrpRecord *grprec  = NULL;

    NclFileVarRecord *varrec  = NULL;
    NclFileVarNode   *varnode = NULL;

    NclFileAttNode   *attnode = NULL;
    NclFileAttRecord *attrec  = NULL;

    NclFileDimNode   *dimnode = NULL;
    NclFileDimRecord *dimrec  = NULL;

    void *tmp_value;
    hid_t att_type;
    hsize_t att_size;

    char *buffer;
    char level_name[HE5_BUF_SIZE];
    int cur_buf_size = HE5_BUF_SIZE;

    int n, pt, lvl, fld,  att, dim, loc;
    HE5_CmpDTSinfo levelInfo; /* Level information data structure */

    /* User-defined structure to read level data to */
    /* -------------------------------------------- */
    typedef struct
    {
        double   time;
        float    con[4];
        char     spec[8];
     } Sensor;

    Sensor *sensor_buffer;
    int max_sensors = HE5_BUF_SIZE;

#if 0    /*Copied from HE5_HdfEosDef.h for convience*/
typedef struct
{
  int                     nfields;                 /* Number of data fields    */
  int                     rank[HE5_FLDNUMBERMAX];  /* Fields rank array        */
  int                     array[HE5_FLDNUMBERMAX]; /* Flag if field is an array*/
  char                    *fieldname[HE5_FLDNUMBERMAX];/* Array of field names */
                                                   /* Array of dimension sizes */
  size_t                  dims[HE5_FLDNUMBERMAX][HE5_DTSETRANKMAX];
  size_t                  datasize;                /* Size of data (bytes)     */
  size_t                  offset[HE5_FLDNUMBERMAX];/* Array of field offsets   */
  hid_t                   dtype[HE5_FLDNUMBERMAX]; /* Array of field type IDs  */
  hid_t                   numtype[HE5_FLDNUMBERMAX];/* Array of field number type IDs */
  H5T_class_t             dclass[HE5_FLDNUMBERMAX];/* Array of field class IDs */
} HE5_CmpDTSinfo;
#endif

    npt = HE5_PTinqpoint(NrmQuarkToString(path),NULL,&str_buf_size);
    if(npt < 1)
    {
        return;
    }

    if (str_buf_size >= cur_buf_size)
    {
        while(str_buf_size >= cur_buf_size)
            cur_buf_size *= 2;
    }

    buffer = NclMalloc(cur_buf_size);
    dimsizes = NclMalloc(max_nlevels * sizeof(hsize_t));
    sensor_buffer = (Sensor *)NclMalloc(max_sensors*sizeof(Sensor));

    while(npt > max_pt)
        max_pt *= 2;

    pt_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_pt);
    pt_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_pt);

    fld_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_fld);
    fld_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_fld);

    lvl_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_lvl);
    lvl_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_lvl);

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    npt = HE5_PTinqpoint(NrmQuarkToString(path),buffer,&str_buf_size);
    HE5_PTfid = HE5_PTopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    buffer[str_buf_size] = '\0';
    HE5ParseName(buffer, pt_hdf_names, pt_ncl_names, npt);

    grprec = _NclFileGrpAlloc(npt);

    parentgrpnode->grp_rec = grprec;
    parentgrpnode->path = path;
    parentgrpnode->fid = HE5_PTfid;
    parentgrpnode->gid = HE5_PTfid;
    parentgrpnode->define_mode = POINT;

    for(pt = 0; pt < npt; pt++)
    {
        HE5_PTid = HE5_PTattach(HE5_PTfid,NrmQuarkToString(pt_hdf_names[pt]));

        if(HE5_PTid < 1)
            continue;

      /*
       */
        fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tpt = %d, HE5_PTid = %d, grpname: <%s>\n",
                           pt, HE5_PTid, NrmQuarkToString(pt_hdf_names[pt]));

        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        grprec->grp_node[pt] = grpnode;
        grpnode->path = parentgrpnode->path;
        grpnode->parent = parentgrpnode;
        grpnode->pid = HE5_PTfid;
        grpnode->fid = HE5_PTfid;
        grpnode->gid = HE5_PTid;
        grpnode->pname = -1;
        grpnode->name = pt_hdf_names[pt];
        grpnode->define_mode = POINT;

        /* global attributes from file */
        natts = HE5_PTinqattrs(HE5_PTid,NULL,&str_buf_size);
        if(natts > 0 )
        {
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;

                buffer = NclRealloc(buffer, cur_buf_size);
            }

            natts = HE5_PTinqattrs(HE5_PTid,buffer,&str_buf_size);
            if(natts > max_att)
            {
                while(natts > max_att)
                    max_att *= 2;
                att_hdf_names = (NclQuark *)NclRealloc(att_hdf_names, sizeof(NclQuark)*max_att);
                att_ncl_names = (NclQuark *)NclRealloc(att_ncl_names, sizeof(NclQuark)*max_att);
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer, att_hdf_names, att_ncl_names, natts);

            attrec = _NclFileAttAlloc(natts);
            assert(attrec);

            attrec->id = HE5_PTid;
            attrec->gid = HE5_PTid;
            attrec->aid = pt;

            for(att = 0; att < natts; ++att)
            { 
                if(HE5_PTattrinfo(HE5_PTid,NrmQuarkToString(att_hdf_names[att]),&att_type,&att_size)==0)
                {
                    attnode->type = HE5MapTypeNumber(att_type);
                    if(NCL_string == attnode->type)
                        tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
		    else
                        tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(attnode->type));
                    if(HE5_PTreadattr(HE5_PTid,NrmQuarkToString(att_hdf_names[att]),tmp_value)==0 )
                    {
                        if(NCL_string == attnode->type)
                        {
                            NclQuark *new_value = (NclQuark *)NclMalloc(sizeof(NclQuark));
                            *new_value = NrmStringToQuark(tmp_value);
        
                            _addHE5Att(attnode, att_ncl_names[att], new_value, 1, attnode->type);

                            NclFree(tmp_value);
                        }
                        else
                                _addHE5Att(attnode, att_ncl_names[att], tmp_value, (int) att_size, attnode->type);
                    }
                }
            }
            grpnode->att_rec = attrec;
        }

        nlevels = HE5_PTnlevels(HE5_PTid);

        if(nlevels > max_lvl)
        {
            while(nlevels > max_lvl)
                max_lvl *= 2;
            lvl_hdf_names = (NclQuark *)NclRealloc(lvl_hdf_names, sizeof(NclQuark)*max_lvl);
            lvl_ncl_names = (NclQuark *)NclRealloc(lvl_ncl_names, sizeof(NclQuark)*max_lvl);
        }

        ndims = 2;
        dim_hdf_names[0] = NrmStringToQuark("fields");
        dim_hdf_names[1] = NrmStringToQuark("records");
        dim_ncl_names[0] = NrmStringToQuark("fields");
        dim_ncl_names[1] = NrmStringToQuark("records");

        varrec = _NclFileVarAlloc(nlevels);
        varrec->gid = HE5_PTid;
        grpnode->var_rec = varrec;

        for(lvl = 0; lvl < nlevels; lvl++)
        {
            status = HE5_PTgetlevelname(HE5_PTid, lvl, level_name, &str_buf_size);

            level_name[str_buf_size] = '\0';
            lvl_hdf_names[lvl] = NrmStringToQuark(level_name);
            for(n = 0; n < strlen(level_name); n++)
            {
                if((level_name[n] == '-') || (level_name[n] == ' '))
                    level_name[n] = '-';
            }
            lvl_ncl_names[lvl] = NrmStringToQuark(level_name);

            /* Get level information */
            /* --------------------- */
            status = HE5_PTlevelinfo(HE5_PTid, lvl, &levelInfo);
      
            nfields = levelInfo.nfields;
            if(nfields > max_fld)
            {
                while(nfields > max_fld)
                    max_fld *= 2;
                fld_hdf_names = (NclQuark *)NclRealloc(fld_hdf_names, sizeof(NclQuark)*max_fld);
                fld_ncl_names = (NclQuark *)NclRealloc(fld_ncl_names, sizeof(NclQuark)*max_fld);
            }

            varnode = &(varrec->var_node[lvl]);
            varnode->id = lvl;
            varnode->gid = HE5_PTid;
            varnode->name = fld_hdf_names[lvl];
            varnode->real_name = fld_ncl_names[lvl];
            varnode->value = NULL;
            varnode->is_chunked = 0;
            varnode->is_compound = 0;

            nrecs = HE5_PTnrecs(HE5_PTid, lvl);
            if(nrecs > max_sensors)
            {
                while(nrecs > max_sensors)
                    max_sensors *= 2;
                sensor_buffer = (Sensor *)NclRealloc(sensor_buffer, max_sensors*sizeof(Sensor));
            }

            dimsizes[0] = nfields;
            dimsizes[1] = nrecs;

            fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
            fprintf(stderr, "\tndims = %d\n", ndims);

            dimrec = _NclFileDimAlloc(ndims);
            dimrec->gid = HE5_PTid;

            for(dim = 0; dim < ndims; dim++)
            {
                fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
                fprintf(stderr, "\tDim No. %d: size = %ld, name: <%s> ncl-name: <%s>\n",
                                 dim, (long)dimsizes[dim],
                                 NrmQuarkToString(dim_hdf_names[dim]),
                                 NrmQuarkToString(dim_ncl_names[dim]));

                dimnode = &(dimrec->dim_node[dim]);
    
                dimnode->id = dim;
    
                _addHE5Dim(dimnode, dim_hdf_names[dim], dimsizes[dim]);
            }
            varnode->dim_rec = dimrec;

            _synchHE5GrpVarDims(grpnode, varnode);

            varnode->type = NCL_double;
            varnode->name = lvl_hdf_names[lvl];

            nlocatts = HE5_PTinqlocattrs(HE5_PTid, level_name, NULL, &str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_PTinqlocattrs(HE5_PTid, level_name, buffer, &str_buf_size);
            if(nlocatts > max_att)
            {
                _reallocnames(nlocatts, &max_att, att_hdf_names, att_ncl_names);
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,att_hdf_names,att_ncl_names,nlocatts);

            attrec = _NclFileAttAlloc(nlocatts);
            assert(attrec);

            attrec->id = -1;
            attrec->gid = HE5_PTid;
            attrec->aid = pt;

            varnode->att_rec = attrec;

            for(loc = 0; loc < nlocatts; loc++)
            {
                status = HE5_PTlocattrinfo(HE5_PTid,NrmQuarkToString(lvl_hdf_names[lvl]),
                                                    NrmQuarkToString(att_hdf_names[loc]),
                                                    &att_type,&att_size);
                if(status == 0)
                {
                    switch(HE5MapTypeNumber(att_type))
                    {
                        case NCL_string:
                             {
                                 NclQuark *qvalue = (NclQuark *)NclMalloc(sizeof(NclQuark));
                                 tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
                                 status = HE5_PTreadlocattr(HE5_PTid,NrmQuarkToString(lvl_hdf_names[lvl]),
                                                            NrmQuarkToString(att_hdf_names[loc]),tmp_value);
				 *qvalue = NrmStringToQuark(tmp_value);
                                 _addHE5Att(attnode, att_ncl_names[loc], qvalue, 1, NCL_string);
                                 NclFree(tmp_value);
                                 break;
                             }
                        default:
                             tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                             status = HE5_PTreadlocattr(HE5_PTid,NrmQuarkToString(lvl_hdf_names[lvl]),
                                                        NrmQuarkToString(att_hdf_names[loc]),tmp_value);
                             _addHE5Att(attnode, att_ncl_names[loc], tmp_value,
                                        att_size, HE5MapTypeNumber(att_type));
                    }
                }
            }    

#if 0
            /* Add new fields attributes to var. */
            {
                NclQuark *vptr;
                char *nameptr[nfields];
                int *rankptr;
                int *typeptr;
                int *dimsptr;
                int *classptr;
                int dim_len = 0;
                int n = strlen(level_name);
                char *new_value = (char *)NclMalloc(n+1);
                strncpy(new_value, level_name, n);

                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = NrmStringToQuark(new_value);
                free(new_value);
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("level_name"),(void*)tmp_value,1,NCL_string);

                for(fld = 0; fld < nfields; fld++)
                {
                    dim_len += (int)levelInfo.dims[fld];
                }

                vptr = (NclQuark *)NclMalloc(nfields * sizeof(NclQuark));
                rankptr = (int *)NclMalloc(nfields * sizeof(int));
                typeptr = (int *)NclMalloc(nfields * sizeof(int));
                dimsptr = (int *)NclMalloc(dim_len * sizeof(int));

                classptr = (int *)NclMalloc(nfields * sizeof(int));

                for(fld = 0; fld < nfields; fld++)
                {
                    nameptr[fld] = (char *)NclMalloc(strlen(levelInfo.fieldname[fld])+1);
                    strcpy(nameptr[fld], levelInfo.fieldname[fld]);
                    vptr[fld] = NrmStringToQuark(nameptr[fld]);
                    for(dim = 0; dim < levelInfo.rank[fld]; dim++)
                    {
                        dimsptr[n++] = (int)levelInfo.dims[fld][dim];
                    }
                    rankptr[fld] = levelInfo.rank[fld];
                    typeptr[fld] = levelInfo.dtype[fld];
                    classptr[fld] = levelInfo.dclass[fld];
                }

                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("field_name"),(void *)vptr,nfields,NCL_string);
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("rank"),(void *)rankptr,nfields,NCL_int);
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("dtype"),(void *)typeptr,nfields,NCL_int);
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("dims"),(void *)dimsptr,dim_len,NCL_int);
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("class"),(void *)classptr,nfields,NCL_int);
            }    
#endif

#if 1
            fprintf(stderr, "pt: %d, level[%d]: <%s>, nfields: %d, nrecs: %d\n", pt, lvl, level_name, nfields, nrecs);
            for(fld = 0; fld < nfields; fld++)
            {
                printf("\n");
                printf("\tField %d name: <%s>\n", fld, levelInfo.fieldname[fld]);
                printf("\tField %d rank: <%d>\n", fld, levelInfo.rank[fld]);
                printf("\tField %d type: <%d>\n", fld, levelInfo.dtype[fld]);
                for(dim = 0; dim < levelInfo.rank[fld]; dim++)
                {
                    printf("\tField %d dims: <%d>\n", fld, (int)levelInfo.dims[fld][dim]);
                }
                printf("\tField %d class: <%d>\n", fld, levelInfo.dclass[fld]);
            }
#endif
        }
        HE5_PTdetach(HE5_PTid);    
    }

    HE5_PTclose(HE5_PTfid);

    NclFree(sensor_buffer);
    NclFree(buffer);

    NclFree(pt_hdf_names);
    NclFree(pt_ncl_names);

    NclFree(fld_hdf_names);
    NclFree(fld_ncl_names);

    NclFree(lvl_hdf_names);
    NclFree(lvl_ncl_names);

    NclFree(dim_hdf_names);
    NclFree(dim_ncl_names);

    NclFree(att_hdf_names);
    NclFree(att_ncl_names);
}

void getHE5ZonalAverageData(NclFileGrpNode *parentgrpnode, NclQuark path)
{
    hid_t HE5_ZAfid = FAIL;
    hid_t HE5_ZAid = FAIL;

    long natts = FAIL;
    long nlocatts = FAIL;

    herr_t status = FAIL;

    long nza = 0;
    long ndata = 0;
    long ndims = 0;

    long str_buf_size;

    hsize_t *dimsizes;

    char maxdimlist[HE5_BUF_SIZE];

    NclQuark *za_hdf_names;
    NclQuark *za_ncl_names;
    int max_za = MAX_ZA;

    NclQuark *dim_hdf_names;
    NclQuark *dim_ncl_names;
    int max_dim = MAX_DIM;

    NclQuark *var_hdf_names;
    NclQuark *var_ncl_names;
    int max_var = MAX_VAR;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    NclQuark fv_quark = NrmStringToQuark("_FillValue");
    NclQuark mv_quark = NrmStringToQuark("MissingValue");

    hid_t tmp_type;

    void *tmp_value;
    hid_t att_type;
    hsize_t att_size;

    NclScalar missing;
    NclScalar *tmp_missing;

    int *is_unsigned;
    char *buffer;
    int cur_buf_size = HE5_BUF_SIZE;
    int *var_ranks;
    int *var_types;
    int max_fields = MAX_VAR;

    int za,att,dim,nv,loc;
    boolean no_fill_value = TRUE;

    NclFileGrpNode   *grpnode = NULL;
    NclFileGrpRecord *grprec  = NULL;

    NclFileVarRecord *varrec  = NULL;
    NclFileVarNode   *varnode = NULL;

    NclFileAttNode   *attnode = NULL;
    NclFileAttRecord *attrec  = NULL;

    NclFileDimNode   *dimnode = NULL;
    NclFileDimRecord *dimrec  = NULL;

    nza = HE5_ZAinqza(NrmQuarkToString(path),NULL,&str_buf_size);
    if(nza < 1)
    {
        return;
    }

    if (str_buf_size >= cur_buf_size)
    {
        cur_buf_size *= 2;
    }

    buffer = NclMalloc(cur_buf_size);
    var_ranks = NclMalloc(max_var * sizeof(int));
    var_types = NclMalloc(max_var * sizeof(int));
    dimsizes = NclMalloc(max_dim * sizeof(hsize_t));

    while(nza > max_za)
        max_za *= 2;

    za_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_za);
    za_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_za);

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);

    var_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    nza = HE5_ZAinqza(NrmQuarkToString(path),buffer,&str_buf_size);
    HE5_ZAfid = HE5_ZAopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    buffer[str_buf_size] = '\0';
    HE5ParseName(buffer, za_hdf_names, za_ncl_names, nza);

    grprec = _NclFileGrpAlloc(nza);

    parentgrpnode->grp_rec = grprec;
    parentgrpnode->path = path;
    parentgrpnode->fid = HE5_ZAfid;
    parentgrpnode->gid = HE5_ZAfid;
    parentgrpnode->define_mode = ZA;

    for(za = 0; za < nza; ++za)
    {
        HE5_ZAid = HE5_ZAattach(HE5_ZAfid,NrmQuarkToString(za_hdf_names[za]));

        if(HE5_ZAid < 1)
            continue;

      /*
       */
        fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tza = %d, HE5_ZAid = %d, grpname: <%s>\n",
                           za, HE5_ZAid, NrmQuarkToString(za_hdf_names[za]));

        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        grprec->grp_node[za] = grpnode;
        grpnode->path = parentgrpnode->path;
        grpnode->parent = parentgrpnode;
        grpnode->pid = HE5_ZAfid;
        grpnode->fid = HE5_ZAfid;
        grpnode->gid = HE5_ZAid;
        grpnode->pname = -1;
        grpnode->name = za_hdf_names[za];
        grpnode->define_mode = ZA;

        /* global attributes from file */
        natts = HE5_ZAinqattrs(HE5_ZAid,NULL,&str_buf_size);
        if(natts > 0 )
        {
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            natts = HE5_ZAinqattrs(HE5_ZAid,buffer,&str_buf_size);
            if(natts > max_att)
            {
                while(natts > max_att)
                    max_att *= 2;
                att_hdf_names = (NclQuark *)NclRealloc(att_hdf_names, sizeof(NclQuark)*max_att);
                att_ncl_names = (NclQuark *)NclRealloc(att_ncl_names, sizeof(NclQuark)*max_att);
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer, att_hdf_names, att_ncl_names, natts);

            attrec = _NclFileAttAlloc(natts);
            assert(attrec);

            attrec->id = za;
            attrec->gid = HE5_ZAid;
            attrec->aid = za;

            for(att = 0; att < natts; att++)
            { 
                attnode = &(attrec->att_node[att]);
                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = att_hdf_names[att];

                if(HE5_ZAattrinfo(HE5_ZAid,NrmQuarkToString(att_hdf_names[att]),&att_type,&att_size)==0)
                {
                    {
                        switch(HE5MapTypeNumber(att_type))
                        {
                            case NCL_string:
                                 {
                                 NclQuark *qvalue = (NclQuark *)NclMalloc(sizeof(NclQuark));
                                 tmp_value = (void*)NclCalloc(HE5_MAX_STRING_LENGTH, 1);
                                 HE5_ZAreadattr(HE5_ZAid,NrmQuarkToString(att_hdf_names[att]),tmp_value);
				 *qvalue = NrmStringToQuark(tmp_value);
                                 _addHE5Att(attnode, att_ncl_names[att], qvalue, 1, NCL_string);
                                 NclFree(tmp_value);
                                 break;
                                 }
                            default:
                                 tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                                 HE5_ZAreadattr(HE5_ZAid,NrmQuarkToString(att_hdf_names[att]),tmp_value);
                                 _addHE5Att(attnode, att_ncl_names[att], tmp_value,
                                            att_size, HE5MapTypeNumber(att_type));
                        }
                    }
                }
            }
            grpnode->att_rec = attrec;
        }

        /* dimensions */
        ndims = HE5_ZAnentries(HE5_ZAid, HE5_HDFE_NENTDIM, &str_buf_size);
        if (ndims < 1)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, 
                  "NclNewHDFEOS5: An internal HDF error occurred while reading (%s) can't continue",
                  NrmQuarkToString(path));
            return;
        }

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }
        if (ndims > max_dim)
        {
            while(ndims > max_dim)
                max_dim *= 2;
            dim_hdf_names = (NclQuark *)NclRealloc(dim_hdf_names, sizeof(NclQuark)*max_dim);
            dim_ncl_names = (NclQuark *)NclRealloc(dim_ncl_names, sizeof(NclQuark)*max_dim);
        }

        ndims = HE5_ZAinqdims(HE5_ZAid,buffer,dimsizes);
        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

      /*
       *fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
       *fprintf(stderr, "\tndims = %ld\n", ndims);
       */

        dimrec = _NclFileDimAlloc(ndims);
        dimrec->gid = HE5_ZAid;

        for(dim = 0; dim < ndims; dim++)
        {
          /*
           *fprintf(stderr, "\tat line: %d, file: %s\n", __LINE__, __FILE__);
           *fprintf(stderr, "\tDim No. %d: size = %ld, name: <%s> ncl-name: <%s>\n",
           *                 dim, (long)dimsizes[dim],
           *                 NrmQuarkToString(dim_hdf_names[dim]),
           *                 NrmQuarkToString(dim_ncl_names[dim]));
           */

            dimnode = &(dimrec->dim_node[dim]);

            dimnode->id = dim;

            _addHE5Dim(dimnode, dim_hdf_names[dim], dimsizes[dim]);
        }
        grpnode->dim_rec = dimrec;

        ndata = HE5_ZAnentries(HE5_ZAid, HE5_HDFE_NENTDFLD, &str_buf_size);
        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }

        if (ndata > max_fields)
        {
            while(ndata > max_fields)
                max_fields *= 2;
            var_ranks = NclRealloc(var_ranks, max_var * sizeof(int));
            var_types = NclRealloc(var_types, max_var * sizeof(int));
        }

        ndata = HE5_ZAinquire(HE5_ZAid,buffer,var_ranks,var_types);
        if(ndata > max_var)
        {
            _reallocnames(ndata, &max_var, var_hdf_names, var_ncl_names);
        }

        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,var_hdf_names,var_ncl_names,ndata);

        varrec = _NclFileVarAlloc(ndata);
        varrec->gid = HE5_ZAid;
        grpnode->var_rec = varrec;

        for(nv = 0; nv < ndata; nv++)
        {
            no_fill_value = TRUE;

            varnode = &(varrec->var_node[nv]);
            varnode->id = nv;
            varnode->gid = HE5_ZAid;
            varnode->name = var_hdf_names[nv];
            varnode->real_name = za_hdf_names[za];
            varnode->value = NULL;
            varnode->is_chunked = 0;
            varnode->is_compound = 0;

            nlocatts = HE5_ZAinqlocattrs(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),NULL,&str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_ZAinqlocattrs(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),buffer,&str_buf_size);
            if(nlocatts > max_att)
            {
                _reallocnames(nlocatts, &max_att, att_hdf_names, att_ncl_names);
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,att_hdf_names,att_ncl_names,nlocatts);

            attrec = _NclFileAttAlloc(nlocatts);
            assert(attrec);

            attrec->id = nv;
            attrec->gid = HE5_ZAid;
            attrec->aid = HE5_ZAid;

            varnode->att_rec = attrec;

            if (var_ranks[nv] > max_dim)
            {
                _reallocnames(var_ranks[nv], &max_dim, dim_hdf_names, dim_ncl_names);
                dimsizes = NclRealloc(dimsizes, max_dim * sizeof(hsize_t));
            }

            if(HE5_ZAinfo(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),
                       &dim,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
            {
                ndims = dim;
                buffer[str_buf_size] = '\0';
                HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

                dimrec = _NclFileDimAlloc(ndims);
                dimrec->gid = HE5_ZAid;
                varnode->dim_rec = dimrec;

                _synchHE5GrpVarDims(grpnode, varnode);

              /*
               *fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
               *fprintf(stderr, "\tndims = %d\n", ndims);
               */

                for(dim = 0; dim < ndims; dim++)
                {
                    fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                    fprintf(stderr, "\nVar %d, dim %d: size = %d, name: <%s>, ncl_name: <%s>\n",
                                     (int)za, (int)dim, (int)dimsizes[dim],
                                     NrmQuarkToString(dim_hdf_names[dim]),
                                     NrmQuarkToString(dim_ncl_names[dim]));

                    dimnode = &(dimrec->dim_node[dim]);

                    dimnode->id = dim;

                    _addHE5Dim(dimnode, dim_hdf_names[dim], dimsizes[dim]);
                }

                varnode->type = HE5MapTypeNumber(tmp_type);
                varnode->name = var_hdf_names[nv];

                fprintf(stderr, "\nat line: %d, file: %s\n", __LINE__, __FILE__);
                fprintf(stderr, "\tvar %d name: <%s>\n", nv, NrmQuarkToString(varnode->name));

                if(HE5unsigned(tmp_type)) {
                    is_unsigned = (int*)NclMalloc(sizeof(int));
                    *is_unsigned = 1;
                    _addNclAttNode(&attrec, NrmStringToQuark("unsigned"), NCL_logical,
                                   1, (void*)is_unsigned);
                    NclFree(is_unsigned);
                    ++nlocatts;
                }
            }

            for(loc = 0; loc < nlocatts; loc++)
            {
                status = HE5_ZAlocattrinfo(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),NrmQuarkToString(att_hdf_names[loc]),&att_type,&att_size);
                if(status)
                    continue;

                tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                status = HE5_ZAreadlocattr(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),NrmQuarkToString(att_hdf_names[loc]),tmp_value);
                if(status < 0)
                {
                    printf("\tHE5_ZAreadattr Failed.\n");
                    free(tmp_value);
                    continue;
                }

                if((fv_quark == att_hdf_names[loc]) || (mv_quark == att_hdf_names[loc]))
                    no_fill_value = FALSE;

                attnode = &(attrec->att_node[loc]);
                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = att_hdf_names[loc];

              /*
               */
                fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                fprintf(stderr,"\tVar %d, Att No. %d, name: <%s>\n", nv, loc, NrmQuarkToString(att_hdf_names[loc]));

                _addHE5Att(attnode, att_ncl_names[loc], tmp_value, att_size, HE5MapTypeNumber(att_type));
            }

            if(no_fill_value)
            {
                if(HE5_ZAgetfillvalue(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),&missing) != -1)
                {
                    tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                    *tmp_missing = missing;
                    _addNclAttNode(&attrec, NrmStringToQuark("_FillValue"), varnode->type, 1, (void*)tmp_missing);
                    NclFree(tmp_missing);
                    ++nlocatts;
                }
            }
        }

        HE5_ZAdetach(HE5_ZAid);    
    }

    HE5_ZAclose(HE5_ZAfid);

    NclFree(za_hdf_names);
    NclFree(za_ncl_names);

    NclFree(dim_hdf_names);
    NclFree(dim_ncl_names);

    NclFree(var_hdf_names);
    NclFree(var_ncl_names);

    NclFree(att_hdf_names);
    NclFree(att_ncl_names);

    NclFree(dimsizes);
    NclFree(var_ranks);
    NclFree(var_types);
    NclFree(buffer);
}

static void HE5FreeFileRec(void *therec)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;

    if(NULL != grpnode)
        FileDestroyGrpNode(grpnode);
}

static void *HE5OpenFile(void *rec,NclQuark path,int wr_status)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) rec;
    long str_buf_size = 0;
    long nsw = 0;
    long ngd = 0;
    long npt = 0;
    long nza = 0;

    if(wr_status <= 0)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5: HDF-EOS5 are currently read only in NCL"));
        return(NULL);
    }

    if(NULL == grpnode)
    {
        return(NULL);
    }

    grpnode->path = path;
    grpnode->status = wr_status;
    grpnode->compress_level = 0;

  /*
   *fprintf(stderr,"\nEnter HE5OpenFile, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr,"\tpath: <%s>\n", (char *)NrmQuarkToString(path));
   */

    nsw = HE5_SWinqswath(NrmQuarkToString(path),NULL,&str_buf_size);
    if(nsw)
        getHE5SwathData(grpnode, path);

    ngd = HE5_GDinqgrid(NrmQuarkToString(path),NULL,&str_buf_size);
    if(ngd)
        getHE5GridData(grpnode, path);

    npt = HE5_PTinqpoint(NrmQuarkToString(path),NULL,&str_buf_size);
    if(npt)
        getHE5PointData(grpnode, path);

    nza = HE5_ZAinqza(NrmQuarkToString(path),NULL,&str_buf_size);
    if(nza > 0)
        getHE5ZonalAverageData(grpnode, path);

    if((npt == 0) && (nsw == 0) && (ngd == 0) && (nza == 0))
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5: No swath, grid or point data found. File is not HE5"));
        return(NULL);
    }

    return ((void*)grpnode);
}

static NclFVarRec *HE5GetCoordInfo
#if    NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
    fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tHave not done anyhting with HE5GetCoordInfo yet.\n");
    fprintf(stderr, "\nfile: %s, line: %d\n\n", __FILE__, __LINE__);
    return NULL;
}

#if 0
static int HE5_GDreadCoordVar
(long HE5_GDid, NclFileGrpNode *varnode, hssize_t *start, hsize_t *stride, hsize_t *edge, void *storage)
{
    int origincode = -1;
    int projcode = -1;
    int zonecode = -1;
    int spherecode = -1;
    int pixregcode = -1;
    long xdimsize,ydimsize;
    double upper_left[2],lower_right[2];
    double projparm[15];
    long *cols, *rows;
    int i, j;
    int total;
    double *latitude, *longitude;
    int status;
    int islon;

      /*
       *Some he5 files do not have GridOrigin, and PixelRegistration,
       *which result HE5_GDorigininfo and HE5_GDpixreginfo failure.
       *By default, as these two functions (actually all other eos5 functions)
       *they print some diagnostic info.
       *But some users do not want to see these, so we turn it off for these
       *two functions.
       *
       *Wei Huang, 06/30/2011.
       */
        HE5_EHset_error_on(2, 0);
        status = HE5_GDorigininfo(HE5_GDid,&origincode);
        if(status == FAIL)
        {
            NHLPERROR((NhlINFO,NhlEUNKNOWN,
                "NclHDFEOS GDorigininfo: origincode = %d\n", origincode));
            /*origincode = HE5_HDFE_GD_UL;*/
        }   

        status = HE5_GDpixreginfo(HE5_GDid,&pixregcode);
        if(status == FAIL)
        {
            NHLPERROR((NhlINFO,NhlEUNKNOWN,
                "NclHDFEOS HE5_GDpixreginfo: pixregcode = %d\n", pixregcode));
            pixregcode = HE5_HDFE_CENTER;
        }
      /*Turn error diagnose back on*/
        HE5_EHset_error_on(1, 0);

        status = HE5_GDgridinfo(HE5_GDid,&xdimsize,&ydimsize,upper_left,lower_right);
        if(status == FAIL)
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                "NclHDFEOS HE5_GDgridinfo: xdimsize = %d, ydimsize = %d\n", xdimsize, ydimsize));
        }

        status = HE5_GDprojinfo(HE5_GDid,&projcode,&zonecode,&spherecode,projparm);
        if (status == FAIL)
        {
            NHLPERROR((NhlWARNING,NhlEUNKNOWN, 
                  "NclNewHDFEOS5: Invalid projection information.\n"));
        }

    if (varnode->name == NrmStringToQuark("lon"))
    {
        islon = 1;
    }
    else
    {
        islon = 0;
    }
    if (varnode->dim_rec->n_dims == 1)
    {
        total = edge[0];

        cols = NclMalloc(total * sizeof(long));
        rows = NclMalloc(total * sizeof(long));
        if (islon) {
            for (i = 0; i < total; i++) {
                long jx = 0;
                long ix = start[0] + i * stride[0];
                cols[i] = ix;
                rows[i] = jx;
            }
        }
        else {
            for (j = 0; j < total; j++) {
                long jx =  start[0] + j * stride[0];
                long ix = 0;
                cols[j] = ix;
                rows[j] = jx;
            }
        }
    }
    else {
        total = edge[0] * edge[1];
        cols = NclMalloc(total * sizeof(long));
        rows = NclMalloc(total * sizeof(long));
        for (j = 0; j < edge[0]; j++) {
            for (i = 0; i < edge[1]; i++) {
                long jx = start[0] + j * stride[0];
                long ix = start[1] + i * stride[1];
                cols[j * edge[1] + i] = ix;
                rows[j * edge[1] + i] = jx;
            }
        }
    }
    if (islon)
    {
        latitude = NclMalloc(total * sizeof(double));
        longitude = (double *) storage;
    }
    else
    {
        longitude = NclMalloc(total * sizeof(double));
        latitude = (double *) storage;
    }

        if(origincode < 0)
        {
            origincode = MyHE5setOrigincode(upper_left, lower_right);
        }

    HE5_GDij2ll(projcode,zonecode,projparm,spherecode,xdimsize,ydimsize,
        upper_left,lower_right,total,rows,cols,longitude,latitude,pixregcode,origincode);
                
    if (islon)
        NclFree(latitude);
    else
    {
          /*
           *This is a kludge fix for OMI [L3] data,
           *which the data is started from LL(Low-Left),
           *instead of normal UL (Upper-Left).
           *(In short, we need to reverse the latitude from north->south to south->north.)
           */
        if((projcode == HE5_GCTP_GEO) && (origincode == HE5_HDFE_GD_LL))
        {
            double tmp;
            j = total;
            for(i = 0; i < total/2; i++)
            {
                j--;
                tmp = latitude[j];
                latitude[j] = latitude[i];
                latitude[i] = tmp;
            }
        }

        NclFree(longitude);
    }
        
    NclFree(rows);
    NclFree(cols);

    return 0;
}
#endif

static void *_readHE5GridVar(NclFileGrpNode *grpnode, NclQuark thevar,
                             long *start, long *finish, long *stride,
                             void *storage)
{
    NclFileVarNode *varnode = NULL;
    int j,out = 0;
    hid_t fid; 
    hid_t did; 
    hssize_t starti[NCL_MAX_DIMENSIONS];
    hsize_t stridei[NCL_MAX_DIMENSIONS];
    hsize_t edgei[NCL_MAX_DIMENSIONS];
    float tmpf;
    char *tmp_hdf_name;

  /*
   *fprintf(stderr, "\nEnter _readHE5GridVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   */

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL == varnode)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "Can not find variable (%s) from file (%s)",
            NrmQuarkToString(thevar),
            NrmQuarkToString(grpnode->path)));

        return(NULL);
    }

    fid = HE5_GDopen(NrmQuarkToString(grpnode->path),H5F_ACC_RDONLY);
    tmp_hdf_name = _make_proper_string_end(NrmQuarkToString(varnode->real_name));
    did = HE5_GDattach(fid,tmp_hdf_name);

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n",
   *fprintf(stderr, "\tfid = %ld, did = %d, tmp_hdf_name: <%s>\n",
   *                 (long)fid, (long)did, tmp_hdf_name);
   */

    free(tmp_hdf_name);

    for(j = 0; j < varnode->dim_rec->n_dims; ++j)
    {
        starti[j] = (hsize_t)start[j] ;
        stridei[j] = (hsize_t)stride[j];
        tmpf = stridei[j];
        edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
    }

    out = HE5_GDreadfield(did,NrmQuarkToString(varnode->name),starti,stridei,edgei,storage);
    if(0 == out)
    {
        HE5_GDdetach(did);
        HE5_GDclose(fid);
        return(storage);
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5: Error ocurred while reading can't continue");
        return(NULL);
    }
}

static void *_readHE5SwathVar(NclFileGrpNode *grpnode, NclQuark thevar,
                              long *start, long *finish, long *stride,
                              void *storage)
{
    NclFileVarNode *varnode = NULL;
    int j,out = 0;
    hid_t fid;
    hid_t did;
    hssize_t starti[NCL_MAX_DIMENSIONS];
    hsize_t stridei[NCL_MAX_DIMENSIONS];
    hsize_t edgei[NCL_MAX_DIMENSIONS];
    float tmpf;
    char *tmp_hdf_name;
    hsize_t total_size = 1;

    char path_string[1024];
    char extension_string[16];

  /*
   *fprintf(stderr, "\nEnter _readHE5SwathVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(grpnode->path));
   */

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL == varnode)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "Can not find variable (%s) from file (%s)",
            NrmQuarkToString(thevar),
            NrmQuarkToString(grpnode->path)));

        return(NULL);
    }

#if 1
    strcpy(path_string, NrmQuarkToString(grpnode->path));
    strcpy(extension_string, NrmQuarkToString(grpnode->extension));

    if(NULL == strstr(path_string, extension_string))
    {
        strcat(path_string, ".");
        strcat(path_string, extension_string);
    }
    fid = HE5_SWopen(path_string,H5F_ACC_RDONLY);
    tmp_hdf_name = _make_proper_string_end(NrmQuarkToString(varnode->real_name));
    did = HE5_SWattach(fid,tmp_hdf_name);
#else
    fid = grpnode->pid;
    did = grpnode->id;
#endif

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfid = %ld, did = %d, tmp_hdf_name: <%s>\n",
   *                 (long)fid, (long)did, tmp_hdf_name);
   */

    free(tmp_hdf_name);

    for(j = 0; j < varnode->dim_rec->n_dims; ++j)
    {
        starti[j] = (hsize_t)start[j] ;
        stridei[j] = (hsize_t)stride[j];
        tmpf = stridei[j];
        edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
        total_size *= edgei[j];
    }

    if(NCL_string == varnode->type)
    {
        char **datbuf;
        NclQuark *quark_ptr;
        datbuf = (char **)NclMalloc(total_size * sizeof(char *));
        for(j = 0; j < total_size; j++)
        {
            datbuf[j] = (char *)NclMalloc(HE5_BUF_SIZE * sizeof(char));
        }

        out = HE5_SWreadfield(did,NrmQuarkToString(varnode->name),starti,stridei,edgei,datbuf);
        quark_ptr = (NrmQuark *) storage;
        for(j = 0; j < total_size; j++)
        {
            quark_ptr[j] = NrmStringToQuark(datbuf[j]);
            NclFree(datbuf[j]);
        }
        NclFree(datbuf);
    }
    else
    {
        out = HE5_SWreadfield(did,NrmQuarkToString(varnode->name),starti,stridei,edgei,storage);
    }

    if(out == 0)
    {
        HE5_SWdetach(did);
        HE5_SWclose(fid);
        return(storage);
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5: Error ocurred while reading can't continue");
        return(NULL);
    }
}


static void *HE5ReadVar(void *therec, NclQuark thevar,
                        long *start, long *finish, long *stride,
                        void *storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    void *out_data = NULL;

    switch(grpnode->define_mode)
    {
        case GRID:
             out_data = _readHE5GridVar(grpnode, thevar, start, finish, stride, storage);
             break;
        case SWATH:
             out_data = _readHE5SwathVar(grpnode, thevar, start, finish, stride, storage);
             break;
        case POINT:
             NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5 can not hanlde POINT data yet."));
             break;
        case ZA:
#if 1
             NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclNewHDFEOS5 can not hanlde ZA data yet."));
#else
             out_data = _readHE5ZAVar(grpnode, thevar, start, finish, stride, storage);
#endif
             break;
        default:
             fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
             fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
    }

    return out_data;
}

static void *HE5ReadCoord(void *therec, NclQuark thevar,
                          long *start, long *finish, long *stride,
                          void *storage)
{
    return(HE5ReadVar(therec,thevar,start,finish,stride,storage));
}

static void *HE5ReadAtt(void *therec,NclQuark theatt,void* storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileAttNode *attnode;
    int i;

    for(i = 0; i < grpnode->att_rec->n_atts; i++)
    {
        attnode = &(grpnode->att_rec->att_node[i]);
        if(attnode->name == theatt)
        {
            if(attnode->value != NULL)
            {
                if(attnode->the_nc_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val))
                {
                    *(NclQuark *)storage = *(NclQuark *)(attnode->value);
                }
                else
                {
                    memcpy(storage, attnode->value,
                           _NclSizeOf(attnode->type) * attnode->n_elem);
                }

                return(storage);
            }
        }
    }
    return(NULL);
}

static void *HE5ReadVarAtt(void *therec, NclQuark thevar, NclQuark theatt, void *storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    NclFileAttNode *attnode;

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL != varnode)
    {
        attnode = _getAttNodeFromNclFileVarNode(varnode, theatt);

        if(NULL != attnode)
        {
            if(NULL != attnode->value)
            {
                if(attnode->the_nc_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val))
                {
                    *(NclQuark *)storage = *(NclQuark *)(attnode->value);
                }
                else
                {
                    memcpy(storage, attnode->value,
                           nctypelen(attnode->the_nc_type) * attnode->n_elem);
                }
                return(storage);
            }
            if(attnode->is_virtual)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNewHDFEOS5: Error retrieving value for is_virtual attribute (%s) of (%s->%s)",
                    NrmQuarkToString(theatt),NrmQuarkToString(grpnode->name),NrmQuarkToString(thevar));
                return NULL;
            }
        }
    }
    return(NULL);
}

NclFormatFunctionRec NewHE5Rec = {
/* NclInitializeFileRecFunc initialize_file_rec */      HE5InitializeFileRec,
/* NclCreateFileFunc        create_file; */             NULL,
/* NclOpenFileFunc          open_file; */               HE5OpenFile,
/* NclFreeFileRecFunc       free_file_rec; */           HE5FreeFileRec,
/* NclGetVarNamesFunc       get_var_names; */           GetGrpVarNames,
/* NclGetVarInfoFunc        get_var_info; */            GetVarInfo,
/* NclGetDimNamesFunc       get_dim_names; */           GetGrpDimNames,
/* NclGetDimInfoFunc        get_dim_info; */            GetDimInfo,
/* NclGetAttNamesFunc       get_att_names; */           GetGrpAttNames,
/* NclGetAttInfoFunc        get_att_info; */            GetAttInfo,
/* NclGetVarAttNamesFunc    get_var_att_names; */       GetVarAttNamesFromGrp,
/* NclGetVarAttInfoFunc     get_var_att_info; */        GetVarAttInfo,
/* NclGetCoordInfoFunc      get_coord_info; */          HE5GetCoordInfo,
/* NclReadCoordFunc         read_coord; */              HE5ReadCoord,
/* NclReadCoordFunc         read_coord; */              NULL,
/* NclReadVarFunc           read_var; */                HE5ReadVar,
/* NclReadVarFunc           read_var; */                NULL,
/* NclReadAttFunc           read_att; */                HE5ReadAtt,
/* NclReadVarAttFunc        read_var_att; */            HE5ReadVarAtt,
/* NclWriteCoordFunc        write_coord; */             NULL,
/* NclWriteCoordFunc        write_coord; */             NULL,
/* NclWriteVarFunc          write_var; */               NULL,
/* NclWriteVarFunc          write_var; */               NULL,
/* NclWriteAttFunc          write_att; */               NULL,
/* NclWriteVarAttFunc       write_var_att; */           NULL,
/* NclAddDimFunc            add_dim; */                 NULL,
/* NclAddChunkDimFunc       add_chunk_dim; */           NULL,
/* NclRenameDim             rename_dim; */              NULL,
/* NclAddVarFunc            add_var; */                 NULL,
/* NclAddVarFunc            add_coord_var; */           NULL,
/* NclAddAttFunc            add_att; */                 NULL,
/* NclAddVarAttFunc         add_var_att; */             NULL,
/* NclMapFormatTypeToNcl    map_format_type_to_ncl; */  NULL,
/* NclMapNclTypeToFormat    map_ncl_type_to_format; */  NULL,
/* NclDelAttFunc            del_att; */                 NULL,
/* NclDelVarAttFunc         del_var_att; */             NULL,
/* NclGetGrpNamesFunc       get_grp_names; */           NULL,
/* NclGetGrpInfoFunc        get_grp_info; */            NULL,
/* NclGetGrpAttNamesFunc    get_grp_att_names; */       NULL,
/* NclGetGrpAttInfoFunc     get_grp_att_info; */        NULL,
/* NclAddGrpFunc            add_grp; */                 NULL,
/* NclAddVlenFunc           add_vlen; */                NULL,
/* NclAddEnumFunc           add_enum; */                NULL,
/* NclAddOpaqueFunc         add_opaque; */              NULL,
/* NclAddCompoundFunc       add_compound; */            NULL,
/* NclWriteCompoundFunc     write_compound; */          NULL,
/* NclSetOptionFunc         set_option;  */             NULL
};

NclFormatFunctionRecPtr NewHE5AddFileFormat(void)
{
    return(&NewHE5Rec);
}


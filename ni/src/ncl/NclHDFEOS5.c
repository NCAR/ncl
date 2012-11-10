/*
 *      $Id: NclHDFEOS5.c,v 1.6.6.1 2010/05/02 18:26:58 haley Exp $
 */
/************************************************************************
*                                    *
*                 Copyright (C)  1994            *
*         University Corporation for Atmospheric Research        *
*                 All Rights Reserved            *
*                                    *
************************************************************************/
/*
 *    File:        
 *
 *    Author:        Wei Huang
 *            National Center for Atmospheric Research
 *            PO 3000, Boulder, Colorado
 *
 *    Date:        April 9, 2009
 *
 *    Description:    
 */
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#define HAVE_NETCDF
#include "NclDataDefs.h"
#include "NclData.h"
#include "NclFileInterfaces.h"
#include <math.h>
#include <ctype.h>
#include <string.h>
#include <HE5_HdfEosDef.h>

#include "NewFileSupport.h"

#define HE5_BUF_SIZE 4096
#define MAX_SW  4
#define MAX_GD  4
#define MAX_PT  4
#define MAX_ZA  4
#define MAX_DIM 16
#define MAX_LOC 16
#define MAX_ATT 32
#define MAX_TMP 32
#define MAX_FLD 32
#define MAX_LVL 32
#define MAX_VAR 128
#define MAX_NDIMS       16
#define MAX_FIELDS      1024

typedef struct _HE5FileRecord HE5FileRecord;
typedef struct _HE5VarInqRec HE5VarInqRec;
typedef struct _HE5DimInqRec HE5DimInqRec;
typedef struct _HE5AttInqRec HE5AttInqRec;
typedef struct _HE5VarInqRecList HE5VarInqRecList;
typedef struct _HE5DimInqRecList HE5DimInqRecList;
typedef struct _HE5AttInqRecList HE5AttInqRecList;

struct _HE5VarInqRecList {
    HE5VarInqRec *var_inq;
    HE5VarInqRecList *next;
};

struct _HE5DimInqRecList {
    HE5DimInqRec *dim_inq;
    HE5DimInqRecList *next;
};


struct _HE5AttInqRecList {
    HE5AttInqRec *att_inq;
    HE5AttInqRecList *next;
};

typedef enum { SWATH,POINT,GRID,ZA} HE5Type;

struct _HE5VarInqRec {
    NclQuark name;
    NclQuark hdf_name;
    NclQuark index_dim;
    /*long hdf_data_id;*/
    HE5Type var_class;
    NclQuark var_class_name;
    long typenumber;
    long n_dims;
    long dim[MAX_DIM];
    long n_int_atts;
    HE5AttInqRecList *att_int_list;
};

struct _HE5DimInqRec {
    NclQuark name;
    NclQuark hdf_name;
    int ncldim_id;
    long size;
    int is_unlimited;
};

struct _HE5AttInqRec {
    NclQuark name;
    NclQuark hdf_name;
    void *value;
    int n_elem;
    NclBasicDataTypes type;
};


struct _HE5FileRecord {
NclQuark        file_path_q;
int             wr_status;
int             n_vars;
HE5VarInqRecList *vars;
int             n_dims;
HE5DimInqRecList *dims;
int             n_int_atts;
HE5AttInqRecList *att_int_list;
};

static void getHE5SwathData(HE5FileRecord *the_file, NclQuark path);
static void getHE5GridData(NclFileGrpNode *grpnode, NclQuark path);
static void getHE5PointData(HE5FileRecord *the_file, NclQuark path);
static void getHE5ZonalAverageData(HE5FileRecord *the_file, NclQuark path);

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

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
        else if((typenumber == H5T_NATIVE_UINT) ||
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
                 NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS5: Unsupported type encountered");
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

static void HE5IntAddVar
#if NhlNeedProto
(HE5VarInqRecList **vars,NclQuark hdf_name, NclQuark ncl_name, HE5DimInqRecList *dims, long n_dims, hsize_t *dims_sizes, long type, NclQuark* dimlist, HE5Type class,NclQuark class_name,NclQuark ncl_class_name)
#else
(vars, hdf_name, ncl_name, dims, n_dims, dims_sizes, type,dimlist,class,class_name,ncl_class_name)
HE5VarInqRecList **vars;
NclQuark hdf_name;
NclQuark ncl_name;
HE5DimInqRecList *dims;
long n_dims;
hsize_t *dims_sizes; 
long type; 
NclQuark* dimlist;
HE5Type class;
NclQuark class_name;
NclQuark ncl_class_name;
#endif 
{
    HE5VarInqRecList *tmp_node = (HE5VarInqRecList *)NclMalloc(sizeof(HE5VarInqRecList));
    HE5DimInqRecList *step = NULL;
    char buffer[4096];
    int i;

    tmp_node->var_inq = (HE5VarInqRec*)NclMalloc(sizeof(HE5VarInqRec));
    strcpy(buffer,NrmQuarkToString(ncl_name));
    strcat(buffer,"_");
    strcat(buffer,NrmQuarkToString(ncl_class_name));
    tmp_node->var_inq->name = NrmStringToQuark(buffer);
    tmp_node->var_inq->hdf_name = hdf_name;
    tmp_node->var_inq->index_dim = NrmNULLQUARK;
    tmp_node->var_inq->typenumber = type;
    tmp_node->var_inq->n_dims = n_dims;
    tmp_node->var_inq->att_int_list = NULL;
    tmp_node->var_inq->n_int_atts = 0;
    tmp_node->var_inq->var_class = class;
    tmp_node->var_inq->var_class_name = class_name;
    for(i = 0; i < n_dims; i++) {
        step = dims;
        while(step != NULL) {
            strcpy(buffer,NrmQuarkToString(dimlist[i]));
            strcat(buffer,"_");
            strcat(buffer,NrmQuarkToString(ncl_class_name));
            if(NrmStringToQuark(buffer) == step->dim_inq->name) {
                tmp_node->var_inq->dim[i] = step->dim_inq->ncldim_id;
                /* If the dimension is unlimited now we know its current size */
                if (step->dim_inq->is_unlimited) {
                    step->dim_inq->size = dims_sizes[i];
                }
                break;
            }
            step = step->next;
        }
    }
    tmp_node->next = *vars;
    *vars = tmp_node;

}

static void _addHE5Dim(NclFileDimNode *dimnode, NclQuark name, long size)
{
    dimnode->name = name;
    dimnode->size = size;
    dimnode->is_unlimited = 0;
}

static void HE5IntAddDim
#if NhlNeedProto
(HE5DimInqRecList **dims,int *n_dims, NclQuark hdf_name, NclQuark ncl_name, long size,NclQuark class_name,NclQuark ncl_class_name)
#else
(dims,n_dims, hdf_name, ncl_name, size,class_name)
HE5DimInqRecList **dims;
int *n_dims; 
NclQuark hdf_name; 
NclQuark ncl_name; 
long size;
NclQuark class_name;
NclQuark ncl_class_name;
#endif 
{
    HE5DimInqRecList * tmp_node ;
    HE5DimInqRecList * step;
    char buffer[4096];

    strcpy(buffer,NrmQuarkToString(ncl_name));
    strcat(buffer,"_");
    strcat(buffer, NrmQuarkToString(ncl_class_name));    
    step = *dims;
    while(step != NULL)
    {
        if(step->dim_inq->name == NrmStringToQuark(buffer))
        {
              /*Correct the dim size, if different from previous read.*/
                if(step->dim_inq->size < size)
            {
                  /*
                   *fprintf(stderr, "\t\nfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tFind: <%s>, old size: %d, new size: %d\n",
                   *    buffer, step->dim_inq->size, size);
                   */
                step->dim_inq->size = size;
            }
              /*Correct the dim size, if different from previous read.*/
                if(step->dim_inq->size < size)
            {
                  /*
                   *fprintf(stderr, "\t\nfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tFind: <%s>, old size: %d, new size: %d\n",
                   *    buffer, step->dim_inq->size, size);
                   */
                step->dim_inq->size = size;
            }
            return;
        }
        else
        {
            step = step->next;
        }
    }

    tmp_node = (HE5DimInqRecList*)NclMalloc(sizeof(HE5DimInqRecList));
    tmp_node->dim_inq = (HE5DimInqRec*)NclMalloc(sizeof(HE5DimInqRec));
    tmp_node->dim_inq->name = NrmStringToQuark(buffer);
    tmp_node->dim_inq->hdf_name = hdf_name;
    tmp_node->dim_inq->is_unlimited = size == 0 ? 1 : 0;
    if (! tmp_node->dim_inq->is_unlimited)
    {
        tmp_node->dim_inq->size = size;
    }
    else
    {
        /* set to 0 until we find a variable with this dimension  */
        tmp_node->dim_inq->size = size;
    }
    tmp_node->dim_inq->ncldim_id = *n_dims;
    tmp_node->next = *dims;
    *dims = tmp_node;
    (*n_dims)++;
}

static void HE5IntAddAtt
#if NhlNeedProto
(HE5VarInqRec* thevar,NclQuark ncl_name,void* value, int n_elem, NclBasicDataTypes type) 
#else
( thevar, ncl_name, value, n_elem, type) 
HE5VarInqRec* thevar;
NclQuark ncl_name; 
void *value;
int n_elem;
NclBasicDataTypes type;
#endif
{
    HE5AttInqRecList *tmp_node = (HE5AttInqRecList*)NclMalloc(sizeof(HE5AttInqRecList));
    char buffer[HE5_BUF_SIZE];
    NrmQuark *tmp_quark;

    tmp_node->att_inq = (HE5AttInqRec*)NclMalloc(sizeof(HE5AttInqRec));
    tmp_node->att_inq->name = ncl_name;

    if(type != NCL_char || (tmp_node->att_inq->name == Qfill_val || tmp_node->att_inq->name == Qmissing_val)) {
        tmp_node->att_inq->value = value;
        tmp_node->att_inq->type = type;
        tmp_node->att_inq->n_elem = n_elem;
    } else {
        tmp_node->att_inq->type = NCL_string;
        memcpy(buffer,value,n_elem);
        buffer[n_elem] = '\0';
        tmp_quark = (NclQuark*)NclMalloc(sizeof(NclQuark));    
        *tmp_quark = NrmStringToQuark(buffer);
        tmp_node->att_inq->value = (void*)tmp_quark;
        tmp_node->att_inq->n_elem = 1;
        NclFree(value);
    }
    tmp_node->next = thevar->att_int_list;
    thevar->att_int_list = tmp_node;
    thevar->n_int_atts++;
}

static void _addHE5Att(NclFileAttNode *attnode, NclQuark att_ncl_name,
                       void *value, int n_elem, NclBasicDataTypes type)
{
    attnode->type = type;

    if(NCL_string == type)
    {
        char buffer[HE5_BUF_SIZE];

        memset(buffer, 0, HE5_BUF_SIZE);
        memcpy(buffer, value, n_elem);
        buffer[n_elem] = '\0';

        fprintf(stderr,"\nfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr,"\tAtt n_elem %d, name: <%s>, value: <%s>\n",
                          n_elem, NrmQuarkToString(att_ncl_name), buffer);

        attnode->value = (NclQuark*)NclCalloc(1, sizeof(NclQuark));
        *(NrmQuark *)attnode->value = NrmStringToQuark(buffer);
        attnode->n_elem = 1;
        NclFree(value);
    }
    else
    {
        attnode->value = value;
        attnode->n_elem = n_elem;
    }
}




static void HE5IntFileAddAtt(HE5FileRecord *the_file,NclQuark sw_ncl_name,NclQuark att_ncl_name,void *value,int n_elem, NclBasicDataTypes type)
{
    HE5AttInqRecList *tmp_node = (HE5AttInqRecList*)NclMalloc(sizeof(HE5AttInqRecList));
    NrmQuark *tmp_quark;
    int lenbuf = n_elem + 2 + HE5_BUF_SIZE;
    char *buffer = (char *) NclMalloc(lenbuf);

    strcpy(buffer,NrmQuarkToString(att_ncl_name));
    strcat(buffer,"_");
    strcat(buffer,NrmQuarkToString(sw_ncl_name));

    tmp_node->att_inq = (HE5AttInqRec*)NclMalloc(sizeof(HE5AttInqRec));
    tmp_node->att_inq->name = NrmStringToQuark(buffer);

        switch(type)
        {
        case NCL_string:
        tmp_node->att_inq->type = NCL_string;
        memcpy(buffer,value,n_elem);
        buffer[n_elem] = '\0';
        tmp_quark = (NclQuark*)NclMalloc(sizeof(NclQuark));    
        *tmp_quark = NrmStringToQuark(buffer);
        tmp_node->att_inq->value = (void*)tmp_quark;
        tmp_node->att_inq->n_elem = 1;
        NclFree(value);
                break;
        default:
        tmp_node->att_inq->value = value;
        tmp_node->att_inq->type = type;
        tmp_node->att_inq->n_elem = n_elem;
    }

    tmp_node->next = the_file->att_int_list;
    the_file->att_int_list = tmp_node;
    the_file->n_int_atts++;
    NclFree(buffer);
}

static void HE5IntAddDimMapInfo
(HE5FileRecord *the_file,NrmQuark swath_ncl_name,int nmaps,char *dimmaps,long *off, long *inc)
{
    int i;
    char *tcp,*cp,*dim1, *dim2;
    char name_buf[1024];
    int* mapvals;

    cp = dimmaps;
    for (i = 0; i < nmaps; i++) {
        dim1 = cp;
        cp = strchr(cp,'/');
        if (cp) {
            *cp = '\0';
            cp++;
        }
        for (tcp = dim1; *tcp != '\0'; tcp++) {
            if(!isalnum(*tcp)) {
                *tcp = '_';
            }
        }
        dim2 = cp;
        cp = strchr(cp,',');
        if (cp) {
            *cp = '\0';
            cp++;
        }
        for (tcp = dim2; *tcp != '\0'; tcp++) {
            if(!isalnum(*tcp)) {
                *tcp = '_';
            }
        }
        if (off[i] >= 0 && inc[i] >= 0) {
            sprintf(name_buf,"%s_to_%s_mapping_offset_and_increment",dim1,dim2);
        }
        else {
            sprintf(name_buf,"%s_to_%s_mapping_offset_and_increment",dim2,dim1);
        }
        mapvals = NclMalloc(2 * sizeof(int));
        mapvals[0] = abs(off[i]);
        mapvals[1] = abs(inc[i]);
        HE5IntFileAddAtt(the_file,swath_ncl_name,NrmStringToQuark(name_buf),(void *)mapvals,2,NCL_int);
    }
}

static void HE5IntAddIndexedMapVars
(HE5FileRecord *the_file,NrmQuark swath_hdf_name,NrmQuark swath_ncl_name,int nmaps,char *idxmaps,hsize_t *sizes)
{
    int i;
    char *tcp,*cp,*dim1, *dim2;
    char name_buf[1024];
    NrmQuark hdf_name1,ncl_name1,hdf_name2;

    cp = idxmaps;
    for (i = 0; i < nmaps; i++) {
        dim1 = cp;
        cp = strchr(cp,'/');
        if (cp) {
            *cp = '\0';
            cp++;
        }
        hdf_name1 = NrmStringToQuark(dim1);
        for (tcp = dim1; *tcp != '\0'; tcp++) {
            if(!isalnum(*tcp)) {
                *tcp = '_';
            }
        }
        ncl_name1 = NrmStringToQuark(dim1);
        dim2 = cp;
        cp = strchr(cp,',');
        if (cp) {
            *cp = '\0';
            cp++;
        }
        hdf_name2 = NrmStringToQuark(dim2);
        for (tcp = dim2; *tcp != '\0'; tcp++) {
            if(!isalnum(*tcp)) {
                *tcp = '_';
            }
        }
        HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),hdf_name1,ncl_name1,sizes[i],
                swath_hdf_name,swath_ncl_name);
        sprintf(name_buf,"%s_index_mapping",dim2);

        HE5IntAddVar(&(the_file->vars),hdf_name2,NrmStringToQuark(name_buf),
                the_file->dims,1,&(sizes[i]),HE5T_NATIVE_INT32,&ncl_name1,
                SWATH,swath_hdf_name,swath_ncl_name);
        /* we can assume that added variable is at the beginning of the variable list */
        the_file->vars->var_inq->index_dim = hdf_name1;
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

    grpnode->id = -1;
    grpnode->pid = -1;
    grpnode->name = NrmStringToQuark("/");
    grpnode->pname = NrmStringToQuark("-");
    grpnode->real_name = NrmStringToQuark("/");
    grpnode->path = -1;
    grpnode->extension = -1;

    grpnode->open = 0;
    grpnode->header_reserve_space = 0;
    grpnode->define_mode = 0;
    grpnode->other_src = NULL;
    grpnode->parent = NULL;

    *format = _NclHDFEOS5;
    setvbuf(stderr,NULL,_IONBF,0);
    return (void *) grpnode;
}


static void getHE5SwathData
#if    NhlNeedProto
(HE5FileRecord *the_file, NclQuark path)
#else
(the_file, path)
HE5FileRecord *the_file;
NclQuark path;
#endif
{
    hid_t HE5_SWfid = FAIL;
    hid_t HE5_SWid = FAIL;

    long natts = FAIL;
    long nlocatts = FAIL;

    herr_t status = FAIL;

    long nsw;
    long ngeofields;
    long ndata = 0;
    long ndims;
    long nmaps;
    long ngrp_atts;

    long str_buf_size;
    hsize_t *dimsizes;
    int max_ndims = MAX_NDIMS;

    char maxdimlist[HE5_BUF_SIZE];
    char tmp_name[HE5_BUF_SIZE];

    NclQuark *sw_hdf_names;
    NclQuark *sw_ncl_names;
    int max_sw = MAX_SW;

    NclQuark *dim_hdf_names;
    NclQuark *dim_ncl_names;
    int max_dim = MAX_DIM;

    NclQuark *var_hdf_names;
    NclQuark *var_ncl_names;
    int max_var = MAX_VAR;

    NclQuark *loc_hdf_names;
    NclQuark *loc_ncl_names;
    int max_loc = MAX_LOC;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    NclQuark *tmp_ncl_names;
    NclQuark *tmp_hdf_names;
    int max_tmp = MAX_TMP;

    int tmp_rank;
    hid_t tmp_type;

    void *tmp_value;
    hid_t att_type;
    hsize_t att_size;

    NclScalar missing;
    NclScalar *tmp_missing;

    int *is_unsigned;
    char *buffer;
    int cur_buf_size = HE5_BUF_SIZE;
    int *field_ranks;
    int max_fields = MAX_FIELDS;
    HE5VarInqRecList *vstep;

    int i,j,k;
    boolean no_fill_value = TRUE;
    int need_check_units = 1;

    nsw = HE5_SWinqswath(NrmQuarkToString(path),NULL,&str_buf_size);
    if(nsw < 1)
    {
        return;
    }

    if (str_buf_size >= cur_buf_size)
    {
        while(str_buf_size >= cur_buf_size)
            cur_buf_size *= 2;
    }

    buffer = NclMalloc(cur_buf_size);
    field_ranks = NclMalloc(max_fields * sizeof(long));
    dimsizes = NclMalloc(max_ndims * sizeof(hsize_t));

    sw_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_sw);
    sw_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_sw);

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);

    var_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);

    loc_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_loc);
    loc_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_loc);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    tmp_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_tmp);
    tmp_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_tmp);

    nsw = HE5_SWinqswath(NrmQuarkToString(path),buffer,&str_buf_size);

    buffer[str_buf_size] = '\0';

    HE5_SWfid = HE5_SWopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    HE5ParseName(buffer,sw_hdf_names,sw_ncl_names,nsw);

    for(i = 0; i < nsw; i++)
    {
        NrmQuark lat_name = NrmNULLQUARK, lon_name = NrmNULLQUARK;
        long y_dim_num = -1, x_dim_num = -1;
        HE5_SWid = HE5_SWattach(HE5_SWfid,NrmQuarkToString(sw_hdf_names[i]));

        if(HE5_SWid < 1)
            continue;

        /* global attributes from file */
        natts = HE5_SWinqattrs(HE5_SWid,NULL,&str_buf_size);

        if(natts > 0 )
        {
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            natts = HE5_SWinqattrs(HE5_SWid,buffer,&str_buf_size);
            if(natts > max_att)
            {
                max_att = natts + 1;
                att_hdf_names = (NclQuark *)NclRealloc(att_hdf_names, sizeof(NclQuark)*max_att);
                att_ncl_names = (NclQuark *)NclRealloc(att_ncl_names, sizeof(NclQuark)*max_att);
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer, att_hdf_names, att_ncl_names, natts);

            for(k = 0; k < natts; k++)
            { 
                if(HE5_SWattrinfo(HE5_SWid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
                    tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    if(HE5_SWreadattr(HE5_SWid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        HE5IntFileAddAtt(the_file,sw_ncl_names[i],att_ncl_names[k],
                                    tmp_value,(int) att_size,
                                    HE5MapTypeNumber(att_type));

                        if(strcmp("ScaleFactor", NrmQuarkToString(att_ncl_names[k])) == 0)
                        {
                            void *add_value;
                            add_value = (void *)NclMalloc(sizeof(NclQuark));
                            *(NclQuark *)add_value = NrmStringToQuark("scale_factor");

                            att_size = strlen("scale_factor") - 1;
                            tmp_value = (void*)NclMalloc(att_size + 1);
                            strcpy((char *)tmp_value, "scale_factor");
                            HE5IntFileAddAtt(the_file,sw_ncl_names[i],
                                    *(NclQuark *)add_value, tmp_value, (int) att_size,
                                    HE5MapTypeNumber(att_type));
                        }
                        else if(strcmp("Offset", NrmQuarkToString(att_ncl_names[k])) == 0)
                        {
                            void *add_value;
                            add_value = (void *)NclMalloc(sizeof(NclQuark));
                            *(NclQuark *)add_value = NrmStringToQuark("scale_factor");
                            att_size = strlen("add_offset") - 1;
                            tmp_value = (void*)NclMalloc(att_size + 1);
                            strcpy((char *)tmp_value, "add_offset");
                            HE5IntFileAddAtt(the_file,sw_ncl_names[i],
                                    *(NclQuark *)add_value, tmp_value, (int) att_size,
                                    HE5MapTypeNumber(att_type));
                        }
                    }
                }
            }
        }

        /* global attributes from file */
        ngrp_atts = HE5_EHinqglbattrs(HE5_SWfid,NULL,&str_buf_size);

        if(ngrp_atts > 0 )
        {
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            ngrp_atts = HE5_EHinqglbattrs(HE5_SWfid,buffer,&str_buf_size);
            if(ngrp_atts > max_att)
            {
        max_att = ngrp_atts + 1;
                att_hdf_names = (NclQuark *)NclRealloc(att_hdf_names, sizeof(NclQuark)*max_att);
                att_ncl_names = (NclQuark *)NclRealloc(att_ncl_names, sizeof(NclQuark)*max_att);
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer, att_hdf_names, att_ncl_names, ngrp_atts);

            for(k = 0; k < ngrp_atts; k++)
            { 
                if(HE5_EHglbattrinfo(HE5_SWfid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
                    tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    if(HE5_EHreadglbattr(HE5_SWfid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        HE5IntFileAddAtt(the_file,sw_ncl_names[i],att_ncl_names[k],
                                    tmp_value,(int) att_size,
                                    HE5MapTypeNumber(att_type));

                        if(strcmp("ScaleFactor", NrmQuarkToString(att_ncl_names[k])) == 0)
                        {
                            void *add_value;
                            add_value = (void *)NclMalloc(sizeof(NclQuark));
                            *(NclQuark *)add_value = NrmStringToQuark("scale_factor");

                            att_size = strlen("scale_factor") - 1;
                            tmp_value = (void*)NclMalloc(att_size + 1);
                            strcpy((char *)tmp_value, "scale_factor");
                            HE5IntFileAddAtt(the_file,sw_ncl_names[i],
                                    *(NclQuark *)add_value, tmp_value, (int) att_size,
                                    HE5MapTypeNumber(att_type));
                        }
                        else if(strcmp("Offset", NrmQuarkToString(att_ncl_names[k])) == 0)
                        {
                            void *add_value;
                            add_value = (void *)NclMalloc(sizeof(NclQuark));
                            *(NclQuark *)add_value = NrmStringToQuark("scale_factor");
                            att_size = strlen("add_offset") - 1;
                            tmp_value = (void*)NclMalloc(att_size + 1);
                            strcpy((char *)tmp_value, "add_offset");
                            HE5IntFileAddAtt(the_file,sw_ncl_names[i],
                                    *(NclQuark *)add_value, tmp_value, (int) att_size,
                                    HE5MapTypeNumber(att_type));
                        }
                    }
                }
            }
        }

        /* dimensions */
        ndims = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTDIM, &str_buf_size);
        if (ndims < 1)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, 
                  "NclHDFEOS5: An internal HDF error occurred while reading (%s) can't continue",
                  NrmQuarkToString(path));
            return;
        }

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }

        ndims = HE5_SWinqdims(HE5_SWid,buffer,dimsizes);
        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

        for(j = 0; j < ndims; j++)
        {
            HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],
                    dimsizes[j],sw_hdf_names[i],sw_ncl_names[i]);
        }

        /* Dimension mappings */
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
            HE5IntAddDimMapInfo(the_file,sw_ncl_names[i],nmaps,buffer,off,inc);
            NclFree(off);
            NclFree(inc);
        }

        /* Indexed Dimension Mappings */
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
            HE5IntAddIndexedMapVars(the_file,sw_hdf_names[i],sw_ncl_names[i],nmaps,buffer,sizes);
            the_file->n_vars += nmaps;
        }

        /* Geolocation fields */
        ngeofields = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTGFLD, &str_buf_size);
        if (ngeofields > max_fields)
        {
            max_fields = ngeofields + 1;
            field_ranks = NclRealloc(field_ranks,max_fields * sizeof(long));
        }

        if (ngeofields > 0)
        {
            if(ngeofields > max_var)
            {
                while(ngeofields > max_var)
                    max_var *= 2;
                var_hdf_names = (NclQuark *)NclRealloc(var_hdf_names, sizeof(NclQuark)*max_var);
                var_ncl_names = (NclQuark *)NclRealloc(var_ncl_names, sizeof(NclQuark)*max_var);
            }

            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            ngeofields = HE5_SWinqgeofields(HE5_SWid,buffer,field_ranks,NULL);
            the_file->n_vars += ngeofields;

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,var_hdf_names,var_ncl_names,ngeofields);
            for(j = 0; j < ngeofields; j++)
            {
                no_fill_value = TRUE;
                need_check_units = 1;

                nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NULL,&str_buf_size);
                if (str_buf_size >= cur_buf_size)
                {
                    while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                    buffer = NclRealloc(buffer, cur_buf_size);
                }

                nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),buffer,&str_buf_size);

                if(nlocatts > max_loc)
                {
                    while(nlocatts > max_loc)
                        max_loc *= 2;
                    loc_hdf_names = (NclQuark *)NclRealloc(loc_hdf_names, sizeof(NclQuark)*max_loc);
                    loc_ncl_names = (NclQuark *)NclRealloc(loc_ncl_names, sizeof(NclQuark)*max_loc);
                }
                buffer[str_buf_size] = '\0';
                HE5ParseName(buffer,loc_hdf_names,loc_ncl_names,nlocatts);

                if (field_ranks[j] > max_ndims)
                {
                    max_ndims = field_ranks[j] + 2;
                    dimsizes = NclRealloc(dimsizes, max_ndims * sizeof(hsize_t));
                }

                if(HE5_SWfieldinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),
                           &tmp_rank,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
                {
                    buffer[str_buf_size] = '\0';
                    HE5ParseName(buffer,tmp_hdf_names,tmp_ncl_names,tmp_rank);
                    for(k = 0; k < tmp_rank; k++)
                    {
                        HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),
                                tmp_hdf_names[k],tmp_ncl_names[k],dimsizes[k],sw_hdf_names[i],
                                sw_ncl_names[i]);
                    }

                    HE5IntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],
                            the_file->dims,tmp_rank,dimsizes,tmp_type,tmp_ncl_names,
                            SWATH,sw_hdf_names[i],sw_ncl_names[i]);

                    if(HE5unsigned(tmp_type))
                    {
                        is_unsigned = (int*)NclMalloc(sizeof(int));
                        *is_unsigned = 1;
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),
                                (void*)is_unsigned,1,NCL_logical);
                    }

                    strcpy(tmp_name, NrmQuarkToString(var_hdf_names[j]));
                    if (strncmp(tmp_name, "Longitude", 9) == 0)
                    {
                        need_check_units = 0;
                        NrmQuark *qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("degrees_east");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
                                (void*)qval,1,NCL_string);
                        qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("longitude");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
                                (void*)qval,1,NCL_string);
                        lon_name = the_file->vars->var_inq->name;
                        if (tmp_rank == 2) {
                            y_dim_num = the_file->vars->var_inq->dim[0];
                            x_dim_num = the_file->vars->var_inq->dim[1];
                        }
                    }
                    else if (strncmp(tmp_name, "Latitude", 8) == 0)
                    {
                        need_check_units = 0;
                        NrmQuark *qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("degrees_north");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
                                (void*)qval,1,NCL_string);
                        qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("latitude");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
                                (void*)qval,1,NCL_string);
                        lat_name = the_file->vars->var_inq->name;
                    }
                    else if (strncmp(tmp_name, "Colatitude", 10) == 0)
                    {
                        need_check_units = 0;
                        NrmQuark *qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("degrees");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
                                (void*)qval,1,NCL_string);
                        qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("colatitude");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
                                (void*)qval,1,NCL_string);
                        lat_name = the_file->vars->var_inq->name;
                    }
                    else if ((strncmp(tmp_name, "Time", 4) == 0) && HE5MapTypeNumber(tmp_type) == NCL_double)
                    {
                        need_check_units = 0;
                        NrmQuark *qval;
                        qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("units value presumes use of TAI93 (International Atomic Time) format");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("Note"),
                                (void*)qval,1,NCL_string);
                        qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("seconds since 1993-1-1 00:00:00");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
                                (void*)qval,1,NCL_string);
                        qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
                        *qval = NrmStringToQuark("time");
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
                                (void*)qval,1,NCL_string);
                    }
                    else
                    {
                        tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                        *(NclQuark*)tmp_value = var_hdf_names[j];
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
                    }
                }

                for(k = 0; k < nlocatts; k++)
                {       
                    status = HE5_SWlocattrinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(loc_hdf_names[k]),&att_type,&att_size);
                    if(status == 0)
                    {
                        tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                        status = HE5_SWreadlocattr(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(loc_hdf_names[k]),tmp_value);
                        if(status < 0)
                        {
                            printf("\tHE5_SWreadattr Failed.\n");
                            free(tmp_value);
                        }
                        else
                        {
                            if (strcmp("_FillValue", NrmQuarkToString(loc_hdf_names[k])) == 0)
                                no_fill_value = FALSE;

                            switch(HE5MapTypeNumber(att_type))
                            {
                                case NCL_char:
                    if (loc_ncl_names[k] == Qfill_val || loc_ncl_names[k] == Qmissing_val) {
                        HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[k],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        break;
                    }
                    /* fall through */
                                case NCL_string:
                                     {
                                     char *new_value = (char *)NclMalloc(att_size+1);
                                     strncpy(new_value, (char *)tmp_value, att_size);
                                     new_value[att_size] = '\0';
                                     free(tmp_value);
                                     tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                                     *(NclQuark*)tmp_value = NrmStringToQuark(new_value);
                                     HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[k],(void*)tmp_value,1,NCL_string);
                                     if((strcmp("Units", NrmQuarkToString(loc_ncl_names[k])) == 0) &&
                                         strcmp("NoUnits", new_value) && need_check_units)
                                     {
                                         void *add_value;
                                         add_value = (void*)NclMalloc(sizeof(NclQuark));
                                         *(NclQuark*)add_value = NrmStringToQuark(new_value);
                                         HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),(void*)add_value,1,NCL_string);
                                     }
                                     NclFree(new_value);
                                     break;
                                     }
                                default:
                                     HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[k],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                            }
                        }
                    }
                }

                if (no_fill_value)
                {
                    if(HE5_SWgetfillvalue(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1)
                    {
                        tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                        *tmp_missing = missing;
                        HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("_FillValue"),
                                (void*)tmp_missing,1,NCL_string);
                    }
                }
            }
        }

        ndata = HE5_SWnentries(HE5_SWid, HE5_HDFE_NENTDFLD, &str_buf_size);
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
            field_ranks = NclRealloc(field_ranks,max_fields * sizeof(long));
        }

        if(ndata > max_var)
        {
            while(ndata > max_var)
                max_var *= 2;
            var_hdf_names = (NclQuark *)NclRealloc(var_hdf_names, sizeof(NclQuark)*max_var);
            var_ncl_names = (NclQuark *)NclRealloc(var_ncl_names, sizeof(NclQuark)*max_var);
        }

        ndata = HE5_SWinqdatafields(HE5_SWid,buffer,field_ranks,NULL);
        the_file->n_vars += ndata;
        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,var_hdf_names,var_ncl_names,ndata);

        for(j = 0; j < ndata; j++)
        {
            no_fill_value = TRUE;

            nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NULL,&str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_SWinqlocattrs(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),buffer,&str_buf_size);
            if(nlocatts > max_loc)
            {
                while(nlocatts > max_loc)
                    max_loc *= 2;
                loc_hdf_names = (NclQuark *)NclRealloc(loc_hdf_names, sizeof(NclQuark)*max_loc);
                loc_ncl_names = (NclQuark *)NclRealloc(loc_ncl_names, sizeof(NclQuark)*max_loc);
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,loc_hdf_names,loc_ncl_names,nlocatts);

            if (field_ranks[j] > max_ndims)
            {
                max_ndims = field_ranks[j] + 2;
                dimsizes = NclRealloc(dimsizes, max_ndims * sizeof(hsize_t));
            }

            if(HE5_SWfieldinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),
                       &tmp_rank,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
            {
                buffer[str_buf_size] = '\0';
                HE5ParseName(buffer,tmp_hdf_names,tmp_ncl_names,tmp_rank);
                for(k = 0; k < tmp_rank; k++)
                {
                    HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),
                            tmp_hdf_names[k],tmp_ncl_names[k],dimsizes[k],sw_hdf_names[i],
                            sw_ncl_names[i]);
                }

                HE5IntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],
                        the_file->dims,tmp_rank,dimsizes,tmp_type,tmp_ncl_names,
                        SWATH,sw_hdf_names[i],sw_ncl_names[i]);

                if(HE5unsigned(tmp_type)) {
                    is_unsigned = (int*)NclMalloc(sizeof(int));
                    *is_unsigned = 1;
                    HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),
                            (void*)is_unsigned,1,NCL_logical);
                }

                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = var_hdf_names[j];
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
            }

            for(k = 0; k < nlocatts; k++)
            {
                status = HE5_SWlocattrinfo(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(loc_hdf_names[k]),&att_type,&att_size);
                if(status == 0)
                {
                    tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    status = HE5_SWreadlocattr(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(loc_hdf_names[k]),tmp_value);
                    if(status < 0)
                    {
                        printf("\tHE5_SWreadattr Failed.\n");
                        free(tmp_value);
                    }
                    else
                    {
                        if (strcmp("_FillValue", NrmQuarkToString(loc_hdf_names[k])) == 0)
                            no_fill_value = FALSE;
 
                        switch(HE5MapTypeNumber(att_type))
                        {
                            case NCL_char:
                    if (loc_ncl_names[k] == Qfill_val || loc_ncl_names[k] == Qmissing_val) {
                        HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[k],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        break;
                    }
                    /* fall through */
                            case NCL_string:
                                 {
                                 char *new_value = (char *)NclMalloc(att_size+1);
                                 strncpy(new_value, (char *)tmp_value, att_size);
                                 new_value[att_size] = '\0';
                                 free(tmp_value);
                                 tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                                 *(NclQuark*)tmp_value = NrmStringToQuark(new_value);
                                 HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[k],(void*)tmp_value,1,NCL_string);
                                 if((strcmp("Units", NrmQuarkToString(loc_ncl_names[k])) == 0) &&
                                     strcmp("NoUnits", new_value))
                                 {  
                                     void *add_value;
                                     add_value = (void*)NclMalloc(sizeof(NclQuark));
                                     *(NclQuark*)add_value = NrmStringToQuark(new_value);
                                     HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),(void*)add_value,1,NCL_string);
                                 }
                                 NclFree(new_value);
                                 break;
                                 }
                            default:
                                 HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[k],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        }
                    }
                }
            }

/*
            if(no_fill_value)
            {
                if(HE5_SWgetfillvalue(HE5_SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1)
                {
                    tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                    *tmp_missing = missing;
                    HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("_FillValue"),
                            (void*)tmp_missing,1,NCL_string);
                }
            }
*/
        }

        if (! (lon_name == NrmNULLQUARK || lat_name == NrmNULLQUARK ||
               y_dim_num == -1 || x_dim_num == -1)) { 
            for (vstep = the_file->vars; vstep != NULL; vstep = vstep->next) {
                if (vstep->var_inq->var_class != SWATH ||
                    vstep->var_inq->var_class_name != sw_hdf_names[i] ||
                    vstep->var_inq->name == lat_name ||
                    vstep->var_inq->name == lon_name ||
                    vstep->var_inq->n_dims < 2 ||
                    vstep->var_inq->dim[vstep->var_inq->n_dims-1] != x_dim_num ||
                    vstep->var_inq->dim[vstep->var_inq->n_dims-2] != y_dim_num)
                    continue;
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                sprintf(buffer,"%s, %s",NrmQuarkToString(lat_name),NrmQuarkToString(lon_name));
                *(NclQuark*)tmp_value = NrmStringToQuark(buffer);
                HE5IntAddAtt(vstep->var_inq,NrmStringToQuark("coordinates"),(void*)tmp_value,1,NCL_string);
            }
        }

        HE5_SWdetach(HE5_SWid);    
    }

    HE5_SWclose(HE5_SWfid);

    NclFree(sw_hdf_names);
    NclFree(sw_ncl_names);

    NclFree(dim_hdf_names);
    NclFree(dim_ncl_names);

    NclFree(var_hdf_names);
    NclFree(var_ncl_names);

    NclFree(loc_hdf_names);
    NclFree(loc_ncl_names);

    NclFree(att_hdf_names);
    NclFree(att_ncl_names);

    NclFree(tmp_hdf_names);
    NclFree(tmp_ncl_names);

    NclFree(dimsizes);
    NclFree(field_ranks);
    NclFree(buffer);
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
    hsize_t dimsizes[MAX_NDIMS];

    int projcode = -1;
    int zonecode = -1;
    int spherecode = -1;
    int origincode = -1;
    int pixregcode = -1;
    double projparm[MAX_VAR];
    double upper_left[2],lower_right[2];

    char maxdimlist[HE5_BUF_SIZE];
    char tmp_name[HE5_BUF_SIZE];

    NclQuark gd_hdf_names[MAX_GD];
    NclQuark gd_ncl_names[MAX_GD];

    NclQuark dim_hdf_names[MAX_DIM];
    NclQuark dim_ncl_names[MAX_DIM];

    NclQuark var_hdf_names[MAX_VAR];
    NclQuark var_ncl_names[MAX_VAR];

    NclQuark loc_hdf_names[MAX_LOC];
    NclQuark loc_ncl_names[MAX_LOC];
    int      loc_record[MAX_LOC];

    NclQuark att_ncl_names[MAX_ATT];
    NclQuark att_hdf_names[MAX_ATT];

    NclQuark tmp_ncl_names[MAX_TMP];
    NclQuark tmp_hdf_names[MAX_TMP];

    int tmp_rank;
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
    int field_ranks[MAX_FIELDS];

    HE5VarInqRecList *vstep;

    boolean no_fill_value = TRUE;
    int i,j,k;

    int status;
    int has_xdim_var = 0;
    int has_ydim_var = 0;
    NrmQuark xdim_name = NrmNULLQUARK;
    NrmQuark ydim_name = NrmNULLQUARK;
    NrmQuark qproj_name = NrmNULLQUARK;
    char *tmp_hdf_name;
    char  grpname[16];
    int need_check = 1;
    int has_xdim = 0;
    int has_ydim = 0;

    NclFileGrpNode   *grpnode = NULL;
    NclFileGrpRecord *grprec  = NULL;

    NclFileVarRecord *varrec  = NULL;
    NclFileVarNode   *varnode = NULL;

    NclFileAttNode   *attnode = NULL;
    NclFileAttRecord *attrec  = NULL;

    NclFileDimNode   *dimnode = NULL;
    NclFileDimRecord *dimrec  = NULL;

    NrmQuark dim_names[2];
    hsize_t dim_sizes[2];

    ngd = HE5_GDinqgrid(NrmQuarkToString(path),NULL,&str_buf_size);

    grprec = _NclFileGrpAlloc(ngd);

    parentgrpnode->grp_rec = grprec;

    if(str_buf_size >= cur_buf_size)
    {
        while(str_buf_size >= cur_buf_size)
            cur_buf_size *= 2;
    }

    buffer = NclMalloc(cur_buf_size);
    ngd = HE5_GDinqgrid(NrmQuarkToString(path),buffer,&str_buf_size);
    HE5ParseName(buffer,gd_hdf_names,gd_ncl_names,ngd);
    buffer[str_buf_size] = '\0';

    HE5_GDfid = HE5_GDopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    parentgrpnode->id = HE5_GDfid;

  /*
   *global attributes from file
   */
    ngrp_atts = HE5_EHinqglbattrs(HE5_GDfid,NULL,&str_buf_size);

  /*
   */
    fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);

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
        if(MAX_ATT < ngrp_atts)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: number of attributes <%d> is more than MAX_ATT <%d>\n",
                      ngrp_atts, MAX_ATT));
            ngrp_atts = MAX_ATT;
        }

      /*
       */
        fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tngrp_atts = %ld, attnames: <%s>\n", ngrp_atts, buffer);

        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer, att_hdf_names, att_ncl_names, ngrp_atts);

        for(k = 0; k < ngrp_atts; k++)
        { 
            attnode = &(attrec->att_node[k]);

          /*
           */
            fprintf(stderr,"\tAtt No. %d, name: <%s>\n", k, NrmQuarkToString(att_hdf_names[k]));

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
                tmp_value = (void*)NclMalloc(att_size);
                memcpy(tmp_value, tmpstr, att_size);
                _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                           (int) att_size, NCL_string);
            }
            else if(tmpquark1 == att_hdf_names[k])
            {
                char *tmpstr = "255,250,240,230,220,210,195,185,175,100,90,80,70,60,0";
                att_size = strlen(tmpstr);
                tmp_value = (void*)NclMalloc(att_size);
                memcpy(tmp_value, tmpstr, att_size);
                _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                           (int) att_size, NCL_string);
            }
            else if(tmpquark2 == att_hdf_names[k])
            {
                char *tmpstr = "not enough data";
                att_size = strlen(tmpstr);
                tmp_value = (void*)NclMalloc(att_size);
                memcpy(tmp_value, tmpstr, att_size);
                _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                           (int) att_size, NCL_string);
            }
            else
            {
                if(HE5_EHglbattrinfo(HE5_GDfid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
                    tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    if(HE5_EHreadglbattr(HE5_GDfid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                                   (int) att_size, HE5MapTypeNumber(att_type));
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
            NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: An internal HDF error occurred while reading (%s) can't continue",
                  NrmQuarkToString(path)));
            return;
        }

        sprintf(grpname, "GRID_%2.2d", i+1);

      /*
       */
        fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\ti = %d, HE5_GDid = %d, grpname: <%s>\n", i, HE5_GDid, grpname);

        grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
        grprec->grp_node[i] = grpnode;
        grpnode->parent = parentgrpnode;
        grpnode->pid = HE5_GDfid;
        grpnode->id = HE5_GDid;
        grpnode->pname = -1;
        grpnode->name = NrmStringToQuark(grpname);

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
                NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: Unsupported projection found, parameter as above.");
                return;
            }

          /*
           */
            fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
            fprintf(stderr, "\tprojcode = %d, qproj_name: <%s>\n", projcode, NrmQuarkToString(qproj_name));
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

            if(MAX_ATT < natts)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: number of attributes <%d> is more than MAX_ATT <%d>\n",
                          natts, MAX_ATT));
                natts = MAX_ATT;
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,att_hdf_names,att_ncl_names,natts);
            for(k = 0; k < natts; ++k)
            { 
                attnode = &(attrec->att_node[k]);

              /*
               *fprintf(stderr,"\tAtt No. %d, name: <%s>\n", k, NrmQuarkToString(att_hdf_names[k]));
               */

                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = att_hdf_names[k];

                if(HE5_GDattrinfo(HE5_GDid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0)
                {
                    tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    if(HE5_GDreadattr(HE5_GDid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 )
                    {
                        _addHE5Att(attnode, att_ncl_names[k], tmp_value,
                                   (int) att_size, HE5MapTypeNumber(att_type));
                    }
                }
            }
            grpnode->att_rec = attrec;
        }

        ndims = HE5_GDnentries(HE5_GDid,HE5_HDFE_NENTDIM,&str_buf_size);
        if(ndims > 0 )
        {
            fprintf(stderr, "\tat line: %d, file: %s, function: %s\n",
                               __LINE__, __FILE__, __PRETTY_FUNCTION__);
            fprintf(stderr, "\tndims = %ld\n", ndims);

            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            ndims = HE5_GDinqdims(HE5_GDid,buffer,dimsizes);
            if(MAX_NDIMS < ndims)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: number of dimensions <%d> is more than MAX_NDIMS <%d>\n",
                          ndims, MAX_NDIMS));
                ndims = MAX_NDIMS;
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,dim_hdf_names,dim_ncl_names,ndims);

            dimrec = _NclFileDimAlloc(ndims);
            dimrec->gid = HE5_GDid;

            for(j = 0; j < ndims; ++j)
            {
                fprintf(stderr, "\tat line: %d, file: %s, function: %s\n",
                               __LINE__, __FILE__, __PRETTY_FUNCTION__);
                fprintf(stderr, "\tDim No. %d: size = %ld, name: <%s> ncl-name: <%s>\n",
                                 j, (long)dimsizes[j],
                                 NrmQuarkToString(dim_hdf_names[j]),
                                 NrmQuarkToString(dim_ncl_names[j]));

                dimnode = &(dimrec->dim_node[j]);

                dimnode->id = j;

                _addHE5Dim(dimnode, dim_hdf_names[j], dimsizes[j]);
            }
            grpnode->dim_rec = dimrec;
        }
       
        ndata = HE5_GDnentries(HE5_GDid, HE5_HDFE_NENTDFLD, &str_buf_size);

        fprintf(stderr, "\tat line: %d, file: %s, function: %s\n",
                         __LINE__, __FILE__, __PRETTY_FUNCTION__);
        fprintf(stderr, "\nndata = %ld\n", ndata);

        if(ndata > MAX_FIELDS)
        {
             NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: number of fields <%d> is more than MAX_FIELDS <%d>\n",
                        MAX_FIELDS, MAX_ATT));
            ndata = MAX_FIELDS;
        }

        if (str_buf_size >= cur_buf_size)
        {
            while(str_buf_size >= cur_buf_size)
                cur_buf_size *= 2;
            buffer = NclRealloc(buffer, cur_buf_size);
        }
        ndata = HE5_GDinqfields(HE5_GDid,buffer,field_ranks,NULL);
        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,var_hdf_names,var_ncl_names,ndata);

        varrec = _NclFileVarAlloc(ndata);
        varrec->gid = HE5_GDid;
        grpnode->var_rec = varrec;

        has_xdim = 0;
        has_ydim = 0;

        for(j = 0; j < ndata; ++j)
        {
            need_check = 2;

            varnode = &(varrec->var_node[j]);
            varnode->id = j;
            varnode->gid = HE5_GDid;
            varnode->name = var_hdf_names[j];
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

            if(MAX_ATT < nlocatts)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: number of attributes <%d> is more than MAX_ATT <%d>\n",
                          nlocatts, MAX_ATT));
                nlocatts = MAX_ATT;
            }

            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,loc_hdf_names,loc_ncl_names,nlocatts);

            attrec = _NclFileAttAlloc(nlocatts);
            assert(attrec);

            attrec->id = -1;
            attrec->gid = HE5_GDid;
            attrec->aid = j;

            varnode->att_rec = attrec;

            for(k = 0; k < nlocatts; ++k)
            {
                status = HE5_GDlocattrinfo(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(loc_hdf_names[k]),&att_type,&att_size);
                if(status)
                    continue;

                attnode = &(attrec->att_node[k]);

              /*
               */
                fprintf(stderr,"\tVar %d, Att No. %d, name: <%s>\n", j, k, NrmQuarkToString(loc_hdf_names[k]));

                attnode->is_virtual = 0;
                attnode->is_opaque = 0;
                attnode->is_vlen = 0;
                attnode->is_compound = 0;
                attnode->name = loc_hdf_names[k];

                tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                status = HE5_GDreadlocattr(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),NrmQuarkToString(loc_hdf_names[k]),tmp_value);
                if(status < 0)
                {
                    fprintf(stderr, "\tat line: %d, file: %s, function: %s\n",
                                     __LINE__, __FILE__, __PRETTY_FUNCTION__);
                    fprintf(stderr, "\tHE5_GDreadattr Failed.\n");
                    free(tmp_value);
                    continue;
                }

                if(0 == strcmp("_FillValue", NrmQuarkToString(loc_hdf_names[k])))
                    no_fill_value = FALSE;

                switch(HE5MapTypeNumber(att_type))
                {
                    case NCL_char:
                         if (loc_ncl_names[k] == Qfill_val || loc_ncl_names[k] == Qmissing_val)
                         {
                             _addHE5Att(attnode, loc_ncl_names[k], tmp_value, 1, NCL_int);
                             attnode->type = NCL_string;
                             break;
                         }
                  /*fall through*/
                    case NCL_string:
                         {
                             char *new_value = (char *)NclMalloc(att_size+1);
                             strncpy(new_value, (char *)tmp_value, att_size);
                             new_value[att_size] = '\0';
                             free(tmp_value);
                             tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                             *(NclQuark*)tmp_value = NrmStringToQuark(new_value);

                             _addHE5Att(attnode, loc_ncl_names[k], tmp_value, 1, NCL_int);
                             attnode->type = NCL_string;
                        }
                    default:
                             _addHE5Att(attnode, loc_ncl_names[k], tmp_value,att_size, HE5MapTypeNumber(att_type));
                }
            }

            if(no_fill_value)
            {
                if(HE5_GDgetfillvalue(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1)
                {
                    tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                    *tmp_missing = missing;
                    _addNclAttNode(&attrec, NrmStringToQuark("_FillValue"), NCL_int, 1, (void*)tmp_missing);
                    attnode = &(attrec->att_node[nlocatts]);
                    attnode->type = NCL_string;
                    ++nlocatts;
                }
            }

            if(MAX_NDIMS < field_ranks[j])
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN, "NclHDFEOS5: dim rank <%d> is more than MAX_NDIMS <%d>\n",
                          field_ranks[j], MAX_NDIMS));
                return;
            }

            if (NrmStringToQuark("XDim") == var_hdf_names[j]) 
                has_xdim_var = 1;
            if (NrmStringToQuark("YDim") == var_hdf_names[j]) 
                has_ydim_var = 1;

            if(HE5_GDfieldinfo(HE5_GDid,NrmQuarkToString(var_hdf_names[j]),&tmp_rank,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
            {
                buffer[str_buf_size] = '\0';
                HE5ParseName(buffer,tmp_hdf_names,tmp_ncl_names,tmp_rank);

                dimrec = _NclFileDimAlloc(tmp_rank);
                dimrec->gid = HE5_GDid;
                varnode->dim_rec = dimrec;

                fprintf(stderr, "\nat line: %d, file: %s, function: %s\n",
                                 __LINE__, __FILE__, __PRETTY_FUNCTION__);
                fprintf(stderr, "\ntmp_rank = %d\n", tmp_rank);

                for(k = 0; k < tmp_rank; ++k)
                {
                    fprintf(stderr, "\nat line: %d, file: %s, function: %s\n",
                                     __LINE__, __FILE__, __PRETTY_FUNCTION__);
                    fprintf(stderr, "\nVar %d, dim %d: size = %d, name: <%s>, ncl_name: <%s>\n",
                                     (int)j, (int)k, (int)dimsizes[k],
                                     NrmQuarkToString(tmp_hdf_names[k]),
                                     NrmQuarkToString(tmp_ncl_names[k]));

                    dimnode = &(dimrec->dim_node[k]);

                    if (tmp_hdf_names[k] == NrmStringToQuark("XDim")) 
                        has_xdim = 1;
                    if (tmp_hdf_names[k] == NrmStringToQuark("YDim")) 
                        has_ydim = 1;

                    dimnode->id = k;

                    _addHE5Dim(dimnode, tmp_hdf_names[k], dimsizes[k]);
                }

                varnode->type = HE5MapTypeNumber(tmp_type);
                varnode->name = var_hdf_names[j];

                fprintf(stderr, "\nat line: %d, file: %s, function: %s\n",
                                 __LINE__, __FILE__, __PRETTY_FUNCTION__);
                fprintf(stderr, "\tvar %d name: <%s>\n", j, NrmQuarkToString(varnode->name));

                if(HE5unsigned(tmp_type))
                {
                    is_unsigned = (int*)NclMalloc(sizeof(int));
                    *is_unsigned = 1;
                    _addNclAttNode(&attrec, NrmStringToQuark("unsigned"), NCL_logical, 1, (void*)is_unsigned);
                    attnode = &(attrec->att_node[nlocatts]);
                    ++nlocatts;
                }
            }

            if (projcode > -1 && has_xdim && has_ydim)
            {
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = qproj_name;
                _addNclAttNode(&attrec, NrmStringToQuark("projection"), NCL_logical, 1, (void*)tmp_value);
                attnode = &(attrec->att_node[nlocatts]);
                attnode->type = NCL_string;
                ++nlocatts;
            }

            tmp_value = (void*)NclMalloc(sizeof(NclQuark));
            *(NclQuark*)tmp_value = var_hdf_names[j];
            if((var_hdf_names[j] != NrmStringToQuark("Longitude")) &&
               (var_hdf_names[j] != NrmStringToQuark("Latitude")))
            {
                _addNclAttNode(&attrec, NrmStringToQuark("long_name"), NCL_logical, 1, (void*)tmp_value);
                attnode = &(attrec->att_node[nlocatts]);
                attnode->type = NCL_string;
                ++nlocatts;
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
                      "NclHDFEOS5: Invalid projection information for GRID (%s); no coordinates will be provided",
                      NrmQuarkToString(gd_hdf_names[i])));
            }
    
            dim_names[1] = NrmStringToQuark("XDim");
            dim_names[0] = NrmStringToQuark("YDim");
            dim_sizes[0] = ydimsize;
            dim_sizes[1] = xdimsize;

            if (projcode != HE5_GCTP_GEO)
            {
                long cols[4],rows[4];
                double lat2d[4], lon2d[4];
                double *corners;
    
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
    

                fprintf(stderr, "\nat line: %d, file: %s, function: %s\n",
                                 __LINE__, __FILE__, __PRETTY_FUNCTION__);
                fprintf(stderr, "\tprojcode = %d, HE5_GCTP_GEO = %d\n", projcode, HE5_GCTP_GEO);
                fprintf(stderr, "\txdimsize = %ld, ydimsize = %ld\n", xdimsize, ydimsize);
                fprintf(stderr, "\nNEED TO WORK IN THIS PIECE\n\n");
    
#if 0
                the_file->n_vars += 2;
                HE5IntAddVar(&(the_file->vars),NrmStringToQuark("lon"),NrmStringToQuark("GridLon"),
                        the_file->dims,2,dim_sizes,HE5T_NATIVE_DOUBLE,dim_names,GRID,gd_hdf_names[i],gd_ncl_names[i]);
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = NrmStringToQuark("degrees_east");
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),(void*)tmp_value,1,NCL_string);
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = NrmStringToQuark("longitude");
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
                corners = (double*)NclMalloc(4 * sizeof(double));
                memcpy(corners,lon2d,4 * sizeof(double));
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("corners"),(void*)corners,4,NCL_double);
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = qproj_name;
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
                xdim_name = the_file->vars->var_inq->name;
                
                HE5IntAddVar(&(the_file->vars),NrmStringToQuark("lat"),NrmStringToQuark("GridLat"),
                        the_file->dims,2,dim_sizes,HE5T_NATIVE_DOUBLE,dim_names,GRID,gd_hdf_names[i],gd_ncl_names[i]);
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = NrmStringToQuark("degrees_north");
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),(void*)tmp_value,1,NCL_string);
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = NrmStringToQuark("latitude");
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
                corners = (double*)NclMalloc(4 * sizeof(double));
                memcpy(corners,lat2d,4 * sizeof(double));
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("corners"),(void*)corners,4,NCL_double);
                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = qproj_name;
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
                ydim_name = the_file->vars->var_inq->name;
#endif
            }
        }
        HE5_GDdetach(HE5_GDid);    
    }

    HE5_GDclose(HE5_GDfid);

    NclFree(buffer);
}

static void getHE5PointData
#if    NhlNeedProto
(HE5FileRecord *the_file, NclQuark path)
#else
(the_file, path)
HE5FileRecord *the_file;
NclQuark path;
#endif
{
    hid_t HE5_PTfid = FAIL;
    hid_t HE5_PTid = FAIL;

    int natts = FAIL;
    int nlocatts = FAIL;

    herr_t status = FAIL;

    long npt;
    int nfields = 0;
    int nlevels = 0;
    int nrecs = 0;

    long str_buf_size;
    hsize_t *dimsizes;
    int max_nlevels = MAX_NDIMS;

    NclQuark *pt_hdf_names;
    NclQuark *pt_ncl_names;
    int max_pt = MAX_PT;

    NclQuark *fld_hdf_names;
    NclQuark *fld_ncl_names;
    int max_fld = MAX_FLD;

    NclQuark *lvl_hdf_names;
    NclQuark *lvl_ncl_names;
    int max_lvl = MAX_LVL;

    NclQuark *loc_hdf_names;
    NclQuark *loc_ncl_names;
    int max_loc = MAX_LOC;

    NclQuark *dim_hdf_names;
    NclQuark *dim_ncl_names;
    int max_dim = 2;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    int tmp_rank;
    hid_t tmp_type;

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

    loc_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_loc);
    loc_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_loc);

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    npt = HE5_PTinqpoint(NrmQuarkToString(path),buffer,&str_buf_size);
    HE5_PTfid = HE5_PTopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    buffer[str_buf_size] = '\0';
    HE5ParseName(buffer, pt_hdf_names, pt_ncl_names, npt);

    for(pt = 0; pt < npt; pt++)
    {

        HE5_PTid = HE5_PTattach(HE5_PTfid,NrmQuarkToString(pt_hdf_names[pt]));

        if(HE5_PTid < 1)
            continue;

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

            for(att = 0; att < natts; att++)
            { 
                if(HE5_PTattrinfo(HE5_PTid,NrmQuarkToString(att_hdf_names[att]),&att_type,&att_size)==0)
                {
                    tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    if(HE5_PTreadattr(HE5_PTid,NrmQuarkToString(att_hdf_names[att]),tmp_value)==0 )
                    {
                        HE5IntFileAddAtt(the_file,pt_ncl_names[pt],att_ncl_names[att],
                                    tmp_value,(int) att_size,
                                    HE5MapTypeNumber(att_type));
                    }
                }
            }
        }

        nlevels = HE5_PTnlevels(HE5_PTid);

        if(nlevels > max_lvl)
        {
            while(nlevels > max_lvl)
                max_lvl *= 2;
            lvl_hdf_names = (NclQuark *)NclRealloc(lvl_hdf_names, sizeof(NclQuark)*max_lvl);
            lvl_ncl_names = (NclQuark *)NclRealloc(lvl_ncl_names, sizeof(NclQuark)*max_lvl);
        }

        the_file->n_vars += nlevels;
        tmp_rank = 2;
        dim_hdf_names[0] = NrmStringToQuark("fields");
        dim_hdf_names[1] = NrmStringToQuark("records");
        dim_ncl_names[0] = NrmStringToQuark("fields");
        dim_ncl_names[1] = NrmStringToQuark("records");

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

            nrecs = HE5_PTnrecs(HE5_PTid, lvl);
            if(nrecs > max_sensors)
            {
                while(nrecs > max_sensors)
                    max_sensors *= 2;
                sensor_buffer = (Sensor *)NclRealloc(sensor_buffer, max_sensors*sizeof(Sensor));
            }

            dimsizes[0] = nfields;
            dimsizes[1] = nrecs;

            for(dim = 0; dim < tmp_rank; dim++)
            {
                HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),
                        dim_hdf_names[dim],dim_ncl_names[dim],dimsizes[dim],pt_hdf_names[pt],
                        pt_ncl_names[pt]);
            }

            /* Fake tmp_type here. */
            tmp_type = H5T_NATIVE_DOUBLE;
            HE5IntAddVar(&(the_file->vars),lvl_hdf_names[lvl],lvl_ncl_names[lvl],
                               the_file->dims,tmp_rank,dimsizes,tmp_type,dim_ncl_names,
                               POINT,pt_hdf_names[pt],pt_ncl_names[pt]);

            nlocatts = HE5_PTinqlocattrs(HE5_PTid, level_name, NULL, &str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_PTinqlocattrs(HE5_PTid, level_name, buffer, &str_buf_size);
            if(nlocatts > max_loc)
            {
                while(nlocatts > max_loc) 
                    max_loc *= 2;
                loc_hdf_names = (NclQuark *)NclRealloc(loc_hdf_names, sizeof(NclQuark)*max_loc);
                loc_ncl_names = (NclQuark *)NclRealloc(loc_ncl_names, sizeof(NclQuark)*max_loc);
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,loc_hdf_names,loc_ncl_names,nlocatts);

            for(loc = 0; loc < nlocatts; loc++)
            {
                status = HE5_PTlocattrinfo(HE5_PTid,NrmQuarkToString(lvl_hdf_names[lvl]),
                                                    NrmQuarkToString(loc_hdf_names[loc]),
                                                    &att_type,&att_size);
                if(status == 0)
                {
                    tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    status = HE5_PTreadlocattr(HE5_PTid,NrmQuarkToString(lvl_hdf_names[lvl]),
                                                        NrmQuarkToString(loc_hdf_names[loc]),tmp_value);
                    if(status < 0)
                    {
                        printf("\tHE5_PTreadlocattr Failed.\n");
                        free(tmp_value);
                    }
                    else
                    {
                        switch(HE5MapTypeNumber(att_type))
                        {
                            case NCL_char:
                    if (loc_ncl_names[loc] == Qfill_val || loc_ncl_names[loc] == Qmissing_val) {
                        HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[loc],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        break;
                    }
                    /* fall through */
                            case NCL_string:
                                 {
                                 char *new_value = (char *)NclMalloc(att_size+1);
                                 strncpy(new_value, (char *)tmp_value, att_size);
                                 new_value[att_size] = '\0';
                                 free(tmp_value);
                                 tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                                 *(NclQuark*)tmp_value = NrmStringToQuark(new_value);
                                 HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[loc],(void*)tmp_value,1,NCL_string);
                                 break;
                                 }
                            default:
                                 HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[loc],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        }
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
            printf("pt: %d, level[%d]: <%s>, nfields: %d, nrecs: %d\n", pt, lvl, level_name, nfields, nrecs);
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

    NclFree(pt_hdf_names);
    NclFree(pt_ncl_names);

    NclFree(fld_hdf_names);
    NclFree(fld_ncl_names);

    NclFree(lvl_hdf_names);
    NclFree(lvl_ncl_names);

    NclFree(loc_hdf_names);
    NclFree(loc_ncl_names);

    NclFree(dim_hdf_names);
    NclFree(dim_ncl_names);

    NclFree(att_hdf_names);
    NclFree(att_ncl_names);

    NclFree(sensor_buffer);
    NclFree(dimsizes);
    NclFree(buffer);
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
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclHDFEOS5: HDF-EOS5 are currently read only in NCL"));
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
   */
    fprintf(stderr,"\nEnter HE5OpenFile, file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr,"\tpath: <%s>\n", (char *)NrmQuarkToString(path));

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
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclHDFEOS5: No swath, grid or point data found. File is not HE5"));
        return(NULL);
    }

    return ((void*)grpnode);
}

void getHE5ZonalAverageData(HE5FileRecord *the_file, NclQuark path)
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

    int max_ndims = MAX_NDIMS;

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

    NclQuark *loc_hdf_names;
    NclQuark *loc_ncl_names;
    int max_loc = MAX_LOC;

    NclQuark *att_ncl_names;
    NclQuark *att_hdf_names;
    int max_att = MAX_ATT;

    NclQuark *tmp_ncl_names;
    NclQuark *tmp_hdf_names;
    int max_tmp = MAX_TMP;

    int tmp_rank;
    hid_t tmp_type;

    void *tmp_value;
    hid_t att_type;
    hsize_t att_size;

    NclScalar missing;
    NclScalar *tmp_missing;

    int *is_unsigned;
    char *buffer;
    int cur_buf_size = HE5_BUF_SIZE;
    int *field_ranks;
    int *field_types;
    int max_fields = MAX_FIELDS;

    int za,att,dim,nv,loc;
    boolean no_fill_value = TRUE;

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
    field_ranks = NclMalloc(max_fields * sizeof(long));
    field_types = NclMalloc(max_fields * sizeof(long));
    dimsizes = NclMalloc(max_ndims * sizeof(hsize_t));

    while(nza > max_za)
        max_za *= 2;

    za_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_za);
    za_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_za);

    dim_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);
    dim_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_dim);

    var_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);
    var_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_var);

    loc_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_loc);
    loc_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_loc);

    att_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);
    att_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_att);

    tmp_hdf_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_tmp);
    tmp_ncl_names = (NclQuark *)NclMalloc(sizeof(NclQuark)*max_tmp);

    nza = HE5_ZAinqza(NrmQuarkToString(path),buffer,&str_buf_size);
    HE5_ZAfid = HE5_ZAopen(NrmQuarkToString(path),H5F_ACC_RDONLY);

    buffer[str_buf_size] = '\0';
    HE5ParseName(buffer, za_hdf_names, za_ncl_names, nza);

    for(za = 0; za < nza; za++)
    {
        HE5_ZAid = HE5_ZAattach(HE5_ZAfid,NrmQuarkToString(za_hdf_names[za]));

        if(HE5_ZAid < 1)
            continue;

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

            for(att = 0; att < natts; att++)
            { 
                if(HE5_ZAattrinfo(HE5_ZAid,NrmQuarkToString(att_hdf_names[att]),&att_type,&att_size)==0)
                {
                    tmp_value = (void*)NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    if(HE5_ZAreadattr(HE5_ZAid,NrmQuarkToString(att_hdf_names[att]),tmp_value)==0 )
                    {
                        HE5IntFileAddAtt(the_file,za_ncl_names[za],att_ncl_names[att],
                                    tmp_value,(int) att_size,
                                    HE5MapTypeNumber(att_type));
                    }
                }
            }
        }

        /* dimensions */
        ndims = HE5_ZAnentries(HE5_ZAid, HE5_HDFE_NENTDIM, &str_buf_size);
        if (ndims < 1)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, 
                  "NclHDFEOS5: An internal HDF error occurred while reading (%s) can't continue",
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

        for(dim = 0; dim < ndims; dim++)
        {
            HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[dim],dim_ncl_names[dim],
                    dimsizes[dim],za_hdf_names[za],za_ncl_names[za]);
        }

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
            field_ranks = NclRealloc(field_ranks,max_fields * sizeof(long));
            field_types = NclRealloc(field_types,max_fields * sizeof(long));
        }

        ndata = HE5_ZAinquire(HE5_ZAid,buffer,field_ranks,field_types);
        if (ndata > max_var)
        {
            while(ndata > max_var)
                max_var *= 2;
            var_hdf_names = (NclQuark *)NclRealloc(var_hdf_names, sizeof(NclQuark)*max_var);
            var_ncl_names = (NclQuark *)NclRealloc(var_ncl_names, sizeof(NclQuark)*max_var);
        }

        buffer[str_buf_size] = '\0';
        HE5ParseName(buffer,var_hdf_names,var_ncl_names,ndata);

        the_file->n_vars += ndata;

        for(nv = 0; nv < ndata; nv++)
        {
            no_fill_value = TRUE;

            nlocatts = HE5_ZAinqlocattrs(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),NULL,&str_buf_size);
            if (str_buf_size >= cur_buf_size)
            {
                while(str_buf_size >= cur_buf_size)
                    cur_buf_size *= 2;
                buffer = NclRealloc(buffer, cur_buf_size);
            }

            nlocatts = HE5_ZAinqlocattrs(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),buffer,&str_buf_size);
            if(nlocatts > max_loc)
            {
                while(nlocatts > max_loc)
                    max_loc *= 2;
                loc_hdf_names = (NclQuark *)NclRealloc(loc_hdf_names, sizeof(NclQuark)*max_loc);
                loc_ncl_names = (NclQuark *)NclRealloc(loc_ncl_names, sizeof(NclQuark)*max_loc);
            }
            buffer[str_buf_size] = '\0';
            HE5ParseName(buffer,loc_hdf_names,loc_ncl_names,nlocatts);

            if (field_ranks[nv] > max_ndims)
            {
                while(field_ranks[nv] > max_ndims)
                    max_ndims *= 2;
                dimsizes = NclRealloc(dimsizes, max_ndims * sizeof(hsize_t));
            }

            if(HE5_ZAinfo(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),
                       &tmp_rank,dimsizes,&tmp_type,buffer,maxdimlist) == 0)
            {
                buffer[str_buf_size] = '\0';
                HE5ParseName(buffer,tmp_hdf_names,tmp_ncl_names,tmp_rank);
                for(dim = 0; dim < tmp_rank; dim++)
                {
                    HE5IntAddDim(&(the_file->dims),&(the_file->n_dims),
                            tmp_hdf_names[dim],tmp_ncl_names[dim],dimsizes[dim],za_hdf_names[za],
                            za_ncl_names[za]);
                }

                HE5IntAddVar(&(the_file->vars),var_hdf_names[nv],var_ncl_names[nv],
                        the_file->dims,tmp_rank,dimsizes,tmp_type,tmp_ncl_names,
                        ZA,za_hdf_names[za],za_ncl_names[za]);

                if(HE5unsigned(tmp_type)) {
                    is_unsigned = (int*)NclMalloc(sizeof(int));
                    *is_unsigned = 1;
                    HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),
                            (void*)is_unsigned,1,NCL_logical);
                }

                tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                *(NclQuark*)tmp_value = var_hdf_names[nv];
                HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
            }

            for(loc = 0; loc < nlocatts; loc++)
            {
                status = HE5_ZAlocattrinfo(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),NrmQuarkToString(loc_hdf_names[loc]),&att_type,&att_size);
                if(status == 0)
                {
                    tmp_value = (void *) NclMalloc(att_size * _NclSizeOf(HE5MapTypeNumber(att_type)));
                    status = HE5_ZAreadlocattr(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),NrmQuarkToString(loc_hdf_names[loc]),tmp_value);
                    if(status < 0)
                    {
                        printf("\tHE5_ZAreadattr Failed.\n");
                        free(tmp_value);
                    }
                    else
                    {
                        if (strcmp("_FillValue", NrmQuarkToString(loc_hdf_names[loc])) == 0)
                            no_fill_value = FALSE;
 
                        switch(HE5MapTypeNumber(att_type))
                        {
                            case NCL_char:
                    if (loc_ncl_names[loc] == Qfill_val || loc_ncl_names[loc] == Qmissing_val) {
                        HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[loc],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        break;
                    }
                    /* fall through */
                            case NCL_string:
                                 {
                                 char *new_value = (char *)NclMalloc(att_size+1);
                                 strncpy(new_value, (char *)tmp_value, att_size);
                                 new_value[att_size] = '\0';
                                 free(tmp_value);
                                 tmp_value = (void*)NclMalloc(sizeof(NclQuark));
                                 *(NclQuark*)tmp_value = NrmStringToQuark(new_value);
                                 HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[loc],(void*)tmp_value,1,NCL_string);
                                 break;
                                 }
                            default:
                                 HE5IntAddAtt(the_file->vars->var_inq,loc_ncl_names[loc],(void*)tmp_value,1,HE5MapTypeNumber(att_type));
                        }
                    }
                }
            }

            if(no_fill_value)
            {
                if(HE5_ZAgetfillvalue(HE5_ZAid,NrmQuarkToString(var_hdf_names[nv]),&missing) != -1)
                {
                    tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
                    *tmp_missing = missing;
                    HE5IntAddAtt(the_file->vars->var_inq,NrmStringToQuark("_FillValue"),
                            (void*)tmp_missing,1,NCL_string);
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

    NclFree(loc_hdf_names);
    NclFree(loc_ncl_names);

    NclFree(att_hdf_names);
    NclFree(att_ncl_names);

    NclFree(tmp_hdf_names);
    NclFree(tmp_ncl_names);

    NclFree(dimsizes);
    NclFree(field_ranks);
    NclFree(field_types);
    NclFree(buffer);
}

static void HE5FreeFileRec
#if    NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList * thevars,*tmpvar;
    HE5AttInqRecList * theatts,*tmpatt;
    HE5DimInqRecList *thedims,*tmpdim;
     
    thevars = thefile->vars;
    while(thevars != NULL) {
        theatts = thevars->var_inq->att_int_list;
        while(theatts!= NULL) {
            NclFree(theatts->att_inq->value);
            NclFree(theatts->att_inq);
            tmpatt = theatts;
            theatts=theatts->next;
            NclFree(tmpatt);
        }
        NclFree(thevars->var_inq);
        
        tmpvar = thevars;
        thevars = thevars->next;
        NclFree(tmpvar);
    }
    thedims = thefile->dims;
    while (thedims != NULL) {
        NclFree(thedims->dim_inq);
        tmpdim = thedims;
        thedims = thedims->next;
        NclFree(tmpdim);
    }
    theatts = thefile->att_int_list;
    while (theatts != NULL) {
        NclFree(theatts->att_inq->value);
        NclFree(theatts->att_inq);
        tmpatt = theatts;
        theatts = theatts->next;
        NclFree(tmpatt);
    }
    NclFree(thefile);
}
static NclQuark* HE5GetVarNames
#if    NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList * thelist;
    int i;
    NclQuark *names;

    names = NclMalloc(sizeof(NclQuark)*thefile->n_vars);
    *num_vars = thefile->n_vars;

    thelist = thefile->vars;
    for (i = 0; i < thefile->n_vars; i++) {
        names[i] = thelist->var_inq->name;
        thelist = thelist->next;
    }
    return(names);
    
}
static NclFVarRec *HE5GetVarInfo
#if    NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif

{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList * thelist;
    NclFVarRec *var_info = NclMalloc(sizeof(NclFVarRec));
    int i,j;

    thelist = thefile->vars;
    memset(var_info,0,sizeof(NclFVarRec));
    for (i = 0; i < thefile->n_vars; i++) {
        if(thelist->var_inq->name == var_name) {
            var_info->var_name_quark = var_name;
            var_info->data_type = HE5MapTypeNumber(thelist->var_inq->typenumber);
            var_info->num_dimensions = thelist->var_inq->n_dims;
            for(j = 0; j < thelist->var_inq->n_dims; j++) {
                var_info->file_dim_num[j] = thelist->var_inq->dim[j];
            }
            return(var_info);
        }
        thelist = thelist->next;
    }
    return NULL;
    
}
static NclQuark *HE5GetDimNames
#if    NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5DimInqRecList * thelist;
    NclQuark* names; 
    int i;

    thelist = thefile->dims;
    names = NclMalloc(sizeof(NclQuark)*thefile->n_dims);
    *num_dims = thefile->n_dims;
    for(i =0; i < thefile->n_dims; i++) {
        names[thelist->dim_inq->ncldim_id] = thelist->dim_inq->name;
        thelist=thelist->next;
    }
    return(names);

}
static NclFDimRec *HE5GetDimInfo
#if    NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5DimInqRecList * thelist;
    NclFDimRec *dim_info = (NclFDimRec*) NclMalloc(sizeof(NclFDimRec));
    int i;

    thelist = thefile->dims;
    for(i =0; i < thefile->n_dims; i++) {
        if(dim_name_q == thelist->dim_inq->name) {
            dim_info->dim_name_quark = dim_name_q;
            dim_info->is_unlimited = thelist->dim_inq->is_unlimited;
            dim_info->dim_size = thelist->dim_inq->size;
            return(dim_info);
        }
        thelist= thelist->next;
    }
    return NULL;
}
static NclQuark *HE5GetAttNames
#if    NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5AttInqRecList * the_int_att_list;
    NclQuark* output = NULL;

    *num_atts = 0;
    if(thefile->n_int_atts > 0) {
        output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(thefile->n_int_atts));
        the_int_att_list = thefile->att_int_list;
        while(the_int_att_list != NULL) {
            output[*num_atts] = the_int_att_list->att_inq->name;
            *num_atts += 1;
            the_int_att_list = the_int_att_list->next;
        }
    }
    return(output);
}
static NclFAttRec* HE5GetAttInfo
#if    NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5AttInqRecList * the_int_att_list;
    NclFAttRec* output = NULL;

    if(thefile->n_int_atts > 0) {
        the_int_att_list = thefile->att_int_list;
        while(the_int_att_list != NULL) {
            if(att_name_q == the_int_att_list->att_inq->name) {
                output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                output->att_name_quark = att_name_q;
                output->data_type = the_int_att_list->att_inq->type;
                output->num_elements = the_int_att_list->att_inq->n_elem;
                return(output);
            } else {
                the_int_att_list = the_int_att_list->next;
            }
        }
    }
    return(output);
}
static NclQuark *HE5GetVarAttNames
#if    NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList * thelist;
    HE5AttInqRecList * the_int_att_list;
    NclQuark* output = NULL;
    int i;

    *num_atts = 0;
    thelist = thefile->vars;
    for(i = 0; i < thefile->n_vars; i++) {
        if(thevar == thelist->var_inq->name) {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(thelist->var_inq->n_int_atts));
            the_int_att_list = thelist->var_inq->att_int_list;
            while(the_int_att_list != NULL) {
                output[*num_atts] = the_int_att_list->att_inq->name;
                *num_atts += 1;
                the_int_att_list = the_int_att_list->next;
            }
            return(output);
        } else {
            thelist = thelist->next;
        }
    }
    return(NULL);
}
static NclFAttRec *HE5GetVarAttInfo
#if    NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList * thelist;
    HE5AttInqRecList * the_int_att_list;
    NclFAttRec* output = NULL;
    int i;

    thelist = thefile->vars;
    for(i = 0; i < thefile->n_vars; i++) {
        if(thevar == thelist->var_inq->name) {
            the_int_att_list = thelist->var_inq->att_int_list;
            while(the_int_att_list != NULL) {
                if(theatt == the_int_att_list->att_inq->name) {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_int_att_list->att_inq->type;
                    output->num_elements = the_int_att_list->att_inq->n_elem;
                    return(output);
                } else {
                    the_int_att_list = the_int_att_list->next;
                }
            }
        } else {
            thelist = thelist->next;
        }
    }
    return(NULL);
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

static int HE5_GDreadCoordVar
(long HE5_GDid, HE5VarInqRec *var, hssize_t *start, hsize_t *stride, hsize_t *edge, void *storage)
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
                  "NclHDFEOS5: Invalid projection information.\n"));
        }

    if (var->hdf_name == NrmStringToQuark("lon")) {
        islon = 1;
    }
    else {
        islon = 0;
    }
    if (var->n_dims == 1) {
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
    if (islon) {
        latitude = NclMalloc(total * sizeof(double));
        longitude = (double *) storage;
    }
    else {
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


static void *HE5ReadVar
#if    NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
        HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList *thelist;
    int i,j,out = 0;
    hid_t fid; 
    hid_t did; 
    hssize_t starti[NCL_MAX_DIMENSIONS];
    hsize_t stridei[NCL_MAX_DIMENSIONS];
    hsize_t edgei[NCL_MAX_DIMENSIONS];
    float tmpf;
    char *tmp_hdf_name;
    hsize_t total_size = 1;

    thelist = thefile->vars;
    for(i = 0; i < thefile->n_vars; i++) {
        if(thevar == thelist->var_inq->name ) {
            switch(thelist->var_inq->var_class) {
            case GRID:
                fid = HE5_GDopen(NrmQuarkToString(thefile->file_path_q),H5F_ACC_RDONLY);
                tmp_hdf_name = _make_proper_string_end(NrmQuarkToString(thelist->var_inq->var_class_name));
                did = HE5_GDattach(fid,tmp_hdf_name);
                free(tmp_hdf_name);
                for(j = 0; j < thelist->var_inq->n_dims; j++) {
                    starti[j] = (hsize_t)start[j] ;
                    stridei[j] = (hsize_t)stride[j];
                    tmpf = stridei[j];
                                    edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
                }
                if (thelist->var_inq->hdf_name == NrmStringToQuark("lat") ||
                    thelist->var_inq->hdf_name == NrmStringToQuark("lon")) { /* a coordinate variable */
                    out = HE5_GDreadCoordVar(did,thelist->var_inq,starti,stridei,edgei,storage);
                }
                else {
                    out = HE5_GDreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
                }
                if(out == 0) {
                    HE5_GDdetach(did);
                    HE5_GDclose(fid);
                    return(storage);
                } else {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS5: Error ocurred while reading can't continue");
                    return(NULL);
                }
                break;
            case SWATH:
                fid = HE5_SWopen(NrmQuarkToString(thefile->file_path_q),H5F_ACC_RDONLY);
                did = HE5_SWattach(fid,NrmQuarkToString(thelist->var_inq->var_class_name));
                for(j = 0; j < thelist->var_inq->n_dims; j++) {
                    starti[j] = (hssize_t)start[j] ;
                    stridei[j] = (hsize_t)stride[j];
                    tmpf = stridei[j];
                                    edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
                                    total_size *= edgei[j];
                                    total_size *= edgei[j];
                }
                if (thelist->var_inq->index_dim != NrmNULLQUARK) {
                    long dimsize = HE5_SWdiminfo(did,NrmQuarkToString(thelist->var_inq->index_dim));
                    if (edgei[0] == dimsize) {
                         HE5_SWidxmapinfo(did,NrmQuarkToString(thelist->var_inq->index_dim),
                                  NrmQuarkToString(thelist->var_inq->hdf_name),storage);
                    }
                    else {
                        long* tmpout;
                        tmpout = NclMalloc(sizeof(long) * dimsize);
                        HE5_SWidxmapinfo(did,NrmQuarkToString(thelist->var_inq->index_dim),
                                 NrmQuarkToString(thelist->var_inq->hdf_name),tmpout);
                        for (j = 0; j < edgei[0]; j++) {
                            *(((int*)storage) + j) = (int) (*(tmpout + (long)starti[0] + (long)(stridei[0] * j)));
                        }
                        NclFree(tmpout);
                    }
                }
                else {
                    if(thelist->var_inq->typenumber == HE5T_CHARSTRING)
                    {
                            char **datbuf;
                            NrmQuark *quark_ptr;
                            datbuf = (char **)NclMalloc(total_size * sizeof(char *));
                        for(j = 0; j < total_size; j++)
                        {
                            datbuf[j] = (char *)NclMalloc(HE5_BUF_SIZE * sizeof(char));
                        }
                        out = HE5_SWreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,datbuf);
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
                        out = HE5_SWreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
                    }
                }
                if(out == 0) {
                    HE5_SWdetach(did);
                    HE5_SWclose(fid);
                    return(storage);
                } else {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS5: Error ocurred while reading can't continue");
                    return(NULL);
                }
                break;
            case POINT:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclHDFEOS5 can not hanlde POINT data yet."));
                return(NULL);
                break;
            case ZA:
                fid = HE5_ZAopen(NrmQuarkToString(thefile->file_path_q),H5F_ACC_RDONLY);
                did = HE5_ZAattach(fid,NrmQuarkToString(thelist->var_inq->var_class_name));
                for(j = 0; j < thelist->var_inq->n_dims; j++)
                {
                    starti[j] = (hssize_t)start[j] ;
                    stridei[j] = (hsize_t)stride[j];
                    tmpf = stridei[j];
                                    edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
                                    total_size *= edgei[j];
                                    total_size *= edgei[j];
                }

                if (thelist->var_inq->index_dim != NrmNULLQUARK)
                {
                    long dimsize = (long) HE5_ZAdiminfo(did,NrmQuarkToString(thelist->var_inq->index_dim));
                    if (edgei[0] == dimsize)
                    {
#if 1
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "NclHDFEOS5: Error ocurred while reading ZA data."));
#else
Wei, 11/9/2011.
This part of code is from Swath, but did not find coresponding ZA code.
H. Joe Lee case worked.
We will come back if someone hit it.
                         HE5_ZAidxmapinfo(did,NrmQuarkToString(thelist->var_inq->index_dim),
                                  NrmQuarkToString(thelist->var_inq->hdf_name),storage);
#endif
                    }
                    else
                    {
#if 1
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                            "NclHDFEOS5: Error ocurred while reading ZA data."));
#else
Wei, 11/9/2011.
This part of code is from Swath, but did not find coresponding ZA code.
H. Joe Lee case worked.
We will come back if someone hit it.
                        long* tmpout;
                        tmpout = NclMalloc(sizeof(long) * dimsize);
                        HE5_ZAidxmapinfo(did,NrmQuarkToString(thelist->var_inq->index_dim),
                                 NrmQuarkToString(thelist->var_inq->hdf_name),tmpout);
                        for (j = 0; j < edgei[0]; j++) {
                            *(((int*)storage) + j) = (int) (*(tmpout + (long)starti[0] + (long)(stridei[0] * j)));
                        }
                        NclFree(tmpout);
#endif
                    }
                }
                else
                {
                    if(thelist->var_inq->typenumber == HE5T_CHARSTRING)
                    {
                            char **datbuf;
                            NrmQuark *quark_ptr;
                            datbuf = (char **)NclMalloc(total_size * sizeof(char *));
                        for(j = 0; j < total_size; j++)
                        {
                            datbuf[j] = (char *)NclMalloc(HE5_BUF_SIZE * sizeof(char));
                        }
                        out = HE5_ZAread(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,datbuf);
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
                        out = HE5_ZAread(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
                    }
                }

                if(out == 0) {
                    HE5_ZAdetach(did);
                    HE5_ZAclose(fid);
                    return(storage);
                } else {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                        "NclHDFEOS5: Error ocurred while reading ZA data."));
                    return(NULL);
                }
                break;
            }
            
        }
        thelist= thelist->next;
    }
    return(NULL);
}
static void *HE5ReadCoord
#if    NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
    return(HE5ReadVar(therec,thevar,start,finish,stride,storage));
}

static void *HE5ReadAtt
#if    NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5AttInqRecList * the_int_att_list;

    the_int_att_list = thefile->att_int_list;
    while(the_int_att_list != NULL) {
        if(theatt == the_int_att_list->att_inq->name) {
            memcpy(storage,the_int_att_list->att_inq->value,_NclSizeOf( the_int_att_list->att_inq->type)* the_int_att_list->att_inq->n_elem);
            return(storage);
        } else {
            the_int_att_list = the_int_att_list->next;
        }
    }
    return(NULL);
}
static void *HE5ReadVarAtt
#if    NhlNeedProto
(void * therec, NclQuark thevar, NclQuark theatt, void * storage)
#else
(therec, thevar, theatt, storage)
void * therec;
NclQuark thevar;
NclQuark theatt;
void* storage;
#endif
{
    HE5FileRecord * thefile = (HE5FileRecord *) therec;
    HE5VarInqRecList * thelist;
    HE5AttInqRecList * the_int_att_list;
    int i;

    thelist = thefile->vars;
    for(i = 0; i < thefile->n_vars; i++) {
        if(thevar == thelist->var_inq->name) {
            the_int_att_list = thelist->var_inq->att_int_list;
            while(the_int_att_list != NULL) {
                if(theatt == the_int_att_list->att_inq->name) {
                    memcpy(storage,the_int_att_list->att_inq->value,_NclSizeOf( the_int_att_list->att_inq->type)* the_int_att_list->att_inq->n_elem);
                    return(storage);
                } else {
                    the_int_att_list = the_int_att_list->next;
                }
            }
        } else {
            thelist = thelist->next;
        }
    }
    return(NULL);
}

NclFormatFunctionRec HE5Rec = {
/* NclInitializeFileRecFunc initialize_file_rec */      HE5InitializeFileRec,
/* NclCreateFileFunc        create_file; */             NULL,
/* NclOpenFileFunc          open_file; */               HE5OpenFile,
/* NclFreeFileRecFunc       free_file_rec; */           HE5FreeFileRec,
/* NclGetVarNamesFunc       get_var_names; */           HE5GetVarNames,
/* NclGetVarInfoFunc        get_var_info; */            HE5GetVarInfo,
/* NclGetDimNamesFunc       get_dim_names; */           HE5GetDimNames,
/* NclGetDimInfoFunc        get_dim_info; */            HE5GetDimInfo,
/* NclGetAttNamesFunc       get_att_names; */           HE5GetAttNames,
/* NclGetAttInfoFunc        get_att_info; */            HE5GetAttInfo,
/* NclGetVarAttNamesFunc    get_var_att_names; */       HE5GetVarAttNames,
/* NclGetVarAttInfoFunc     get_var_att_info; */        HE5GetVarAttInfo,
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

NclFormatFunctionRecPtr HDFEOS5AddFileFormat(void)
{
    return(&HE5Rec);
}


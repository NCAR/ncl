/*
 *      $Id: NclHDF5.c,v 1.4 2010-04-28 23:02:03 huangwei Exp $
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
 *    Date:        Wed Jun 24 10:15:21 MDT 2009
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
#include <hdf5.h>
#include "NclData.h"
#include "DataSupport.h"
#include "NclFileInterfaces.h"
#include <math.h>
#include <ctype.h>
#include <string.h>

#ifndef MAX_HDF5_NAME_LENGTH
#define MAX_HDF5_NAME_LENGTH    256
#endif

#ifndef MAX_HDF5_DIMS
#define MAX_HDF5_DIMS    32
#endif

#ifndef SUCCEED
#define SUCCEED    0
#endif

#ifndef FAILED
#define FAILED    (-1)
#endif

unsigned int _closest_prime(unsigned int prime_in);

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

#include "h5data_struct.h"

#define H5_USE_CACHE_OPT         0
#define H5_COMPRESSION_LEVEL_OPT 1
#define H5_CACHE_SIZE_OPT        2
#define H5_CACHE_NELEMS_OPT      3
#define H5_CACHE_PREEMPTION_OPT  4
#define H5_NUM_OPTIONS           5

void *_Ncl2H5type(NclBasicDataTypes type);

typedef struct _HDF5compound_component_t HDF5compound_component_t;
typedef struct _HDF5compound_t HDF5compound_t;
typedef struct _HDF5FileRecord HDF5FileRecord;
typedef struct _HDF5GrpInqRec HDF5GrpInqRec;
typedef struct _HDF5VarInqRec HDF5VarInqRec;
typedef struct _HDF5DimInqRec HDF5DimInqRec;
typedef struct _HDF5AttInqRec HDF5AttInqRec;
typedef struct _HDF5GrpInqRecList HDF5GrpInqRecList;
typedef struct _HDF5VarInqRecList HDF5VarInqRecList;
typedef struct _HDF5DimInqRecList HDF5DimInqRecList;
typedef struct _HDF5AttInqRecList HDF5AttInqRecList;
typedef struct _HDF5Options HDF5Options;

struct _HDF5compound_component_t
{
    NclQuark          name;   /* name */
    NclBasicDataTypes type;   /* type */
    nclH5size_t       offset;
    int               is_str;  /* is the component string */
};

struct _HDF5compound_t
{
    nclH5size_t        nom;    /* number of members */
    nclH5size_t        size;   /* size of compound data */
    HDF5compound_component_t *member;
};

struct _HDF5GrpInqRecList
{
    HDF5GrpInqRec *grp_inq;
    HDF5GrpInqRecList *next;
};

struct _HDF5VarInqRecList
{
    HDF5VarInqRec *var_inq;
    HDF5VarInqRecList *next;
};

struct _HDF5DimInqRecList
{
    HDF5DimInqRec *dim_inq;
    HDF5DimInqRecList *next;
};

struct _HDF5AttInqRecList
{
    HDF5AttInqRec *att_inq;
    HDF5AttInqRecList *next;
};

struct _HDF5GrpInqRec
{
    hid_t    id;
    NclQuark file;
    NclQuark name;
    NclQuark hdf5_name;
    NclQuark full_name;
    nclH5size_t n_dims;
    nclH5size_t dim[MAX_HDF5_DIMS];
    NclBasicDataTypes type;
    nclH5size_t n_atts;
    HDF5AttInqRecList *att_list;
    nclH5size_t n_vars;
    HDF5VarInqRecList *var_list;
    nclH5size_t n_grps;
    HDF5GrpInqRecList *grp_list;
};

struct _HDF5VarInqRec
{
    hid_t    id;
    NclQuark name;
    NclQuark hdf5_name;
    NclQuark full_name;
    NclQuark index_dim;
    NclQuark var_class_name;
    NclBasicDataTypes type;
    nclH5size_t typenumber;
    nclH5size_t n_dims;
    nclH5size_t dim[MAX_HDF5_DIMS];
    NclQuark dim_name[MAX_HDF5_DIMS];
    int  has_dim_name;
    nclH5size_t n_atts;
    HDF5AttInqRecList *att_list;
    HDF5compound_t    *compound;
    void              *value;

    nclH5size_t deflate_pass;
    nclH5size_t n_chunk_dims;
    nclH5size_t chunk_dim[MAX_HDF5_DIMS];
    nclH5size_t n_unlimit_dims;
    nclH5size_t unlimit_dim[MAX_HDF5_DIMS];

    int         compress_level;

    int         use_cache;
    nclH5size_t cache_size;
    nclH5size_t cache_nelems;
    float       cache_preemption;
};

struct _HDF5DimInqRec
{
    NclQuark name;
    NclQuark description;
    NclQuark dataset_name;
    nclH5size_t size;
    int ncldim_id;
    int is_unlimited;
    int is_dataset;
};

struct _HDF5AttInqRec
{
    hid_t    id;
    NclQuark name;
    void *value;
    nclH5size_t n_elem;
    NclBasicDataTypes type;
};

struct _HDF5Options
{
    NclQuark name;
    NclBasicDataTypes data_type;
    int n_values;
    void *values;
};

struct _HDF5FileRecord
{
    NclQuark             file_path_q;
    int                  wr_status;
    hid_t                id;
    int                  open;
    int                  format;
    int                  define_mode;
    int                  has_scalar_dim;
    nclH5size_t          n_grps;
    HDF5GrpInqRecList   *grp_list;
    nclH5size_t          n_vars;
    HDF5VarInqRecList   *var_list;
    nclH5size_t          n_dims;
    HDF5DimInqRecList   *dim_list;
    int                  compress_level;
    int                  n_chunk_dims;
    HDF5DimInqRecList   *chunk_dims;
    nclH5size_t          n_atts;
    HDF5AttInqRecList   *att_list;
    NclHDF5group_node_t *h5_group;

    nclH5size_t          deflate_pass;
/*
    nclH5size_t          n_chunk_dims;
    nclH5size_t          chunk_dim[MAX_HDF5_DIMS];
*/
    nclH5size_t          n_unlimit_dims;
    nclH5size_t          unlimit_dim[MAX_HDF5_DIMS];

    int                  use_cache;
    nclH5size_t          cache_size;
    nclH5size_t          cache_nelems;
    float                cache_preemption;
    int                  n_options;
    HDF5Options         *options;
};

#define NUMPOSDIMNAMES	6

NclQuark possibleDimNames[NUMPOSDIMNAMES];

static int _H5_initializeOptions 
#if    NhlNeedProto
(HDF5FileRecord *tmp)
#else
(tmp)
HDF5FileRecord *tmp;
#endif
{
    HDF5Options *options;

    tmp->n_options = H5_NUM_OPTIONS;

    possibleDimNames[0] = NrmStringToQuark("coordinates");
    possibleDimNames[1] = NrmStringToQuark("DimensionNames");
    possibleDimNames[2] = NrmStringToQuark("Dimensions");
    possibleDimNames[3] = NrmStringToQuark("DIMSCALE");
    possibleDimNames[4] = NrmStringToQuark("DIMENSION_LIST");
    possibleDimNames[5] = NrmStringToQuark("HDF4_DIMENSION_LIST");
    
    options = NclMalloc(tmp->n_options * sizeof(HDF5Options));
    if (! options)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        return 0;
    }

    options[H5_USE_CACHE_OPT].name = NrmStringToQuark("usecache");
    options[H5_USE_CACHE_OPT].data_type = NCL_int;
    options[H5_USE_CACHE_OPT].n_values = 1;
    options[H5_USE_CACHE_OPT].values = (void *) 0;

    options[H5_COMPRESSION_LEVEL_OPT].name = NrmStringToQuark("compressionlevel");
    options[H5_COMPRESSION_LEVEL_OPT].data_type = NCL_int;
    options[H5_COMPRESSION_LEVEL_OPT].n_values = 1;
    options[H5_COMPRESSION_LEVEL_OPT].values = (void *) -1;

    options[H5_CACHE_SIZE_OPT].name = NrmStringToQuark("cachesize");
    options[H5_CACHE_SIZE_OPT].data_type = NCL_int;
    options[H5_CACHE_SIZE_OPT].n_values = 1;
    options[H5_CACHE_SIZE_OPT].values = (void *) 3200000;

    options[H5_CACHE_NELEMS_OPT].name = NrmStringToQuark("cachenelems");
    options[H5_CACHE_NELEMS_OPT].data_type = NCL_int;
    options[H5_CACHE_NELEMS_OPT].n_values = 1;
    options[H5_CACHE_NELEMS_OPT].values = (void *) 1009;

    options[H5_CACHE_PREEMPTION_OPT].name = NrmStringToQuark("cachepreemption");
    options[H5_CACHE_PREEMPTION_OPT].data_type = NCL_float;
    options[H5_CACHE_PREEMPTION_OPT].n_values = 1;
    options[H5_CACHE_PREEMPTION_OPT].values = (void *) 0;

    tmp->options = options;
    return 1;
}

static void *HDF5InitializeFileRec
#if     NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormat *format;
#endif
{
    static int first = True;
    HDF5FileRecord *therec = NULL;

    if (first)
    {
        Qmissing_val = NrmStringToQuark("missing_value");
        Qfill_val = NrmStringToQuark("_FillValue");
        first = False;
    }

    therec = (HDF5FileRecord*) NclCalloc(1, sizeof(HDF5FileRecord));
    if (! therec)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        return NULL;
    }

    if(! _H5_initializeOptions(therec))
    {
        NclFree(therec);
        return NULL;
    }

    *format = _NclHDF5;
    return (void *) therec;
}

static NclFGrpRec *HDF5GetGrpInfo_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, int n_dims, HDF5DimInqRecList *dim_list, NclQuark grp_name)
#else
(grp_inq, n_dims, dim_list, grp_name)
HDF5GrpInqRec *grp_inq;
int n_dims;
HDF5DimInqRecList *dim_list;
NclQuark grp_name;
#endif
{
    HDF5GrpInqRecList *grplist;
    NclFGrpRec *grp_info = NULL;
    int i;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetGrpInfo_inGroup. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrp_inq->n_grps: %d\n", grp_inq->n_grps);
   */

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == grp_name) ||
           (grplist->grp_inq->hdf5_name == grp_name) ||
           (grplist->grp_inq->name == grp_name))
        {
            grp_info = (NclFGrpRec *) NclMalloc(sizeof(NclFGrpRec));
            grp_info->grp_name_quark = grp_name;
            grp_info->data_type = grplist->grp_inq->type;
            grp_info->grp_full_name_quark = grplist->grp_inq->full_name;
            grp_info->num_dimensions = 0;
            return(grp_info);
        }

        grp_info = HDF5GetGrpInfo_inGroup(grplist->grp_inq, n_dims, dim_list, grp_name);

        if(grp_info)
        {
            return(grp_info);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclFGrpRec *HDF5GetGrpInfo
#if     NhlNeedProto
(void *therec, NclQuark grp_name)
#else
(therec, grp_name)
void *therec;
NclQuark grp_name;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    NclFGrpRec *grp_info = NULL;
    int i;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetGrpInfo. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrp_name: %s\n", NrmQuarkToString(grp_name));
   *fprintf(stderr, "\tthefile->n_grps: %d\n", thefile->n_grps);
   */

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == grp_name) ||
           (grplist->grp_inq->hdf5_name == grp_name) ||
           (grplist->grp_inq->name == grp_name))
        {
            grp_info = NclMalloc(sizeof(NclFGrpRec));
            grp_info->grp_name_quark = grp_name;
            grp_info->grp_full_name_quark = grplist->grp_inq->full_name;
            grp_info->data_type = grplist->grp_inq->type;
            grp_info->num_dimensions = 0;
          /*
           *fprintf(stderr, "\tFind Grp No. %d: grplist->grp_inq->name: <%s>\n", i, NrmQuarkToString(grplist->grp_inq->name));
           *fprintf(stderr, "\tFind Grp No. %d: grplist->grp_inq->full_name: <%s>\n", i, NrmQuarkToString(grplist->grp_inq->full_name));
           */
            return(grp_info);
        }

        grp_info = HDF5GetGrpInfo_inGroup(grplist->grp_inq, thefile->n_dims, thefile->dim_list, grp_name);

        if(grp_info)
        {
            return(grp_info);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static void HDF5Set_var_info
#if     NhlNeedProto
(HDF5VarInqRec *var_inq, NclFVarRec *var_info, HDF5DimInqRecList *dim_list, int n_dims)
#else
(var_inq, var_info, dim_list, n_dims)
HDF5VarInqRec *var_inq;
NclFVarRec *var_info;
HDF5DimInqRecList *dim_list;
int n_dims;
#endif
{
    char *long_name;
    char *short_name;
    NclQuark quark_name;

    HDF5DimInqRecList *cur_dim_list;
    int j, n;

    var_info->var_name_quark = var_inq->name;
    var_info->var_real_name_quark = var_inq->hdf5_name;
    var_info->var_full_name_quark = var_inq->full_name;
    var_info->data_type = var_inq->type;
    var_info->num_dimensions = var_inq->n_dims;
    var_info->num_compounds = 0;

    if(var_inq->compound)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tvar_info->name: <%s>", NrmQuarkToString(var_inq->name));
       *fprintf(stderr, "\tvar_info->type: <%s>", (char *)_Ncl2H5type(var_inq->type));
       *fprintf(stderr, "\tcompound->nom: <%d>\n", var_inq->compound->nom);
       */
        var_info->num_compounds = var_inq->compound->nom;
        assert(var_info->num_compounds < NCL_MAX_COMPOUND_COMPONETS);
        for(n = 0; n < var_info->num_compounds; n++)
        {
          /*
           *fprintf(stderr, "\tvar_info->component_name[%d]: <%s>\n", n, NrmQuarkToString(var_inq->compound->member[n].name));
           *fprintf(stderr, "\tvar_info->component_type[%d]: <%s>\n", n, (char *)_Ncl2H5type(var_inq->compound->member[n].type));
           */
            var_info->component_name[n] = var_inq->compound->member[n].name;
            var_info->component_type[n] = var_inq->compound->member[n].type;
        }
    }

  /*
   *fprintf(stderr, "\tvar_info->num_dimensions: %d\n", var_info->num_dimensions);
   */

    long_name = NrmQuarkToString(var_inq->full_name);
    short_name = strrchr(long_name, '/');
    if(short_name == NULL)
        quark_name = NrmStringToQuark(long_name);
    else
        quark_name = NrmStringToQuark(short_name+1);

    if(var_inq->has_dim_name)
    {
        for(j = 0; j < var_inq->n_dims; j++)
        {
            long_name = NrmQuarkToString(var_inq->dim_name[j]);
            short_name = strrchr(long_name, '/');
            if(short_name == NULL)
                quark_name = NrmStringToQuark(long_name);
            else
                quark_name = NrmStringToQuark(short_name+1);

            cur_dim_list = dim_list;
            var_info->file_dim_num[j] = 0;
            for(n = 0; n < n_dims; n++)
            {
                if(cur_dim_list->dim_inq->name == quark_name)
                {
                    var_info->file_dim_num[j] = cur_dim_list->dim_inq->ncldim_id;
                    break;
                }
                cur_dim_list = cur_dim_list->next;
            }
        }
    }
    else
    {
        for(j = 0; j < var_inq->n_dims; j++)
        {
            cur_dim_list = dim_list;
            var_info->file_dim_num[j] = 0;
            for(n = 0; n < n_dims; n++)
            {
                if(cur_dim_list->dim_inq->size == var_inq->dim[j])
                {
                    var_info->file_dim_num[j] = cur_dim_list->dim_inq->ncldim_id;
                    break;
                }
                cur_dim_list = cur_dim_list->next;
            }
        }
    }
}

static NclFVarRec *HDF5GetVarInfo_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, int n_dims, HDF5DimInqRecList *dim_list, NclQuark var_name)
#else
(grp_inq, n_dims, dim_list, var_name)
HDF5GrpInqRec *grp_inq;
int n_dims;
HDF5DimInqRecList *dim_list;
NclQuark var_name;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    NclFVarRec *var_info = NULL;
    int i;

    varlist = grp_inq->var_list;
    for (i = 0; i < grp_inq->n_vars; i++)
    {
        if((varlist->var_inq->full_name == var_name) ||
           (varlist->var_inq->hdf5_name == var_name) ||
           (varlist->var_inq->name == var_name))
        {
            var_info = (NclFVarRec *) NclMalloc(sizeof(NclFVarRec));
            var_info->var_name_quark = var_name;
            var_info->num_compounds = 0;
            HDF5Set_var_info(varlist->var_inq, var_info, dim_list, n_dims);
            return(var_info);
        }
        varlist = varlist->next;
    }

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == var_name) ||
           (grplist->grp_inq->hdf5_name == var_name) ||
           (grplist->grp_inq->name == var_name))
        {
            var_info = (NclFVarRec *) NclMalloc(sizeof(NclFVarRec));
            var_info->var_name_quark = var_name;
            var_info->var_real_name_quark = grplist->grp_inq->hdf5_name;
            var_info->var_full_name_quark = grplist->grp_inq->full_name;
            var_info->data_type = grplist->grp_inq->type;
            var_info->num_dimensions = 0;
            var_info->num_compounds = 0;
            return(var_info);
        }

        var_info = HDF5GetVarInfo_inGroup(grplist->grp_inq, n_dims, dim_list, var_name);

        if(var_info)
        {
            return(var_info);
        }

        grplist = grplist->next;
    }

    return(var_info);
}

static NclFVarRec *HDF5GetVarInfo
#if     NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    NclFVarRec *var_info = NULL;
    int i;

    varlist = thefile->var_list;
    for (i = 0; i < thefile->n_vars; i++)
    {
        if((varlist->var_inq->full_name == var_name) ||
           (varlist->var_inq->hdf5_name == var_name) ||
           (varlist->var_inq->name == var_name))
        {
            var_info = (NclFVarRec *) NclMalloc(sizeof(NclFVarRec));
            assert(var_info);
            var_info->var_name_quark = var_name;
            HDF5Set_var_info(varlist->var_inq, var_info, thefile->dim_list, thefile->n_dims);
            return(var_info);
        }
        varlist = varlist->next;
    }

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == var_name) ||
           (grplist->grp_inq->hdf5_name == var_name) ||
           (grplist->grp_inq->name == var_name))
        {
            var_info = NclMalloc(sizeof(NclFVarRec));
            var_info->var_name_quark = var_name;
            var_info->var_real_name_quark = grplist->grp_inq->hdf5_name;
            var_info->var_full_name_quark = grplist->grp_inq->full_name;
            var_info->data_type = grplist->grp_inq->type;
            var_info->num_dimensions = 0;
            var_info->num_compounds = 0;
            return(var_info);
        }

        var_info = HDF5GetVarInfo_inGroup(grplist->grp_inq, thefile->n_dims, thefile->dim_list, var_name);

        if(var_info)
        {
            return(var_info);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclBasicDataTypes _HDF52Ncl_type(const char *type_name)
{ 
    NclBasicDataTypes type = NCL_none;

    if(strcmp(type_name, "float") == 0)
        type = NCL_float;
    else if(strcmp(type_name, "double") == 0)
        type = NCL_double;
    else if(strcmp(type_name, "string") == 0)
        type = NCL_string;
    else if(strcmp(type_name, "int64") == 0)
        type = NCL_int64;
    else if(strcmp(type_name, "uint64") == 0)
        type = NCL_uint64;
    else if(strcmp(type_name, "long") == 0)
        type = NCL_long;
    else if(strcmp(type_name, "ulong") == 0)
        type = NCL_ulong;
    else if(strcmp(type_name, "integer") == 0)
        type = NCL_int;
    else if(strcmp(type_name, "int") == 0)
        type = NCL_int;
    else if(strcmp(type_name, "uint") == 0)
        type = NCL_uint;
    else if(strcmp(type_name, "short") == 0)
        type = NCL_short;
    else if(strcmp(type_name, "ushort") == 0)
        type = NCL_ushort;
    else if(strcmp(type_name, "char") == 0)
        type = NCL_char;
    else if(strcmp(type_name, "byte") == 0)
        type = NCL_byte;
    else if(strcmp(type_name, "ubyte") == 0)
        type = NCL_ubyte;
    else if(strcmp(type_name, "compound") == 0)
        type = NCL_compound;
    else if(0 == strcmp("opaque", type_name))
        return(NCL_opaque);
    else if(0 == strcmp("enum", type_name))
        return(NCL_enum);
    else if(0 == strcmp("object reference", type_name))
        return(NCL_reference);
    else if(0 == strcmp("dataset region reference", type_name))
        return(NCL_reference);
    else
    {
      /*
       *fprintf(stderr, "\n\nUNKNOWN TYPE (in _HDF52Ncl_type): <%s>\n", type_name);
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       */
        type = NCL_none;
    }
    
    return type;
}

void *_Ncl2H5type(NclBasicDataTypes type)
{ 
    char *H5type;

    switch(type)
    {
        case NCL_float:
            H5type = strdup("float");
            break;
        case NCL_double:
            H5type = strdup("double");
            break;
        case NCL_string:
          /*Need to return NULL, as NclFile.c which check for NCL_string again.
           *Wei, 3/31/2010
           *H5type = strdup("string");
           */
            return(NULL);
            break;
        case NCL_int64:
            H5type = strdup("int64");
            break;
        case NCL_uint64:
            H5type = strdup("uint64");
            break;
        case NCL_long:
            H5type = strdup("long");
            break;
        case NCL_ulong:
            H5type = strdup("ulong");
            break;
        case NCL_int:
            H5type = strdup("int");
            break;
        case NCL_uint:
            H5type = strdup("uint");
            break;
        case NCL_short:
            H5type = strdup("short");
            break;
        case NCL_ushort:
            H5type = strdup("ushort");
            break;
        case NCL_char:
            H5type = strdup("char");
            break;
        case NCL_byte:
            H5type = strdup("byte");
            break;
        case NCL_ubyte:
            H5type = strdup("ubyte");
            break;
        case NCL_compound:
            H5type = strdup("compound");
            break;
        default:
            return(NULL);
    }
    
    return (void *)H5type;
}

static NclQuark *HDF5GetGrpAttNames_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thegrp, int *num_atts)
#else
(grp_inq , thegrp, num_atts)
HDF5GrpInqRec *grp_inq;
NclQuark thegrp;
int *num_atts;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5AttInqRecList *the_att_list;
    NclQuark* output = NULL;
    int i, j;

    *num_atts = 0;

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->name == thegrp) ||
           (grplist->grp_inq->full_name == thegrp))
        {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(grplist->grp_inq->n_atts));
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                output[*num_atts] = the_att_list->att_inq->name;
                *num_atts += 1;
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetGrpAttNames_inGroup(grplist->grp_inq, thegrp, num_atts);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclQuark *HDF5GetGrpAttNames
#if     NhlNeedProto
(void *therec , NclQuark thegrp, int *num_atts)
#else
(therec , thegrp, num_atts)
void *therec;
NclQuark thegrp;
int *num_atts;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5AttInqRecList *the_att_list;
    NclQuark* output = NULL;
    int i, j;

    *num_atts = 0;

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thegrp) ||
           (grplist->grp_inq->hdf5_name == thegrp) ||
           (grplist->grp_inq->name == thegrp))
        {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(grplist->grp_inq->n_atts));
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                output[*num_atts] = the_att_list->att_inq->name;
                *num_atts += 1;
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetGrpAttNames_inGroup(grplist->grp_inq, thegrp, num_atts);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclQuark *HDF5GetVarAttNames_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thevar, int *num_atts)
#else
(grp_inq , thevar, num_atts)
HDF5GrpInqRec *grp_inq;
NclQuark thevar;
int *num_atts;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    HDF5AttInqRecList *the_att_list;
    NclQuark* output = NULL;
    int i, j;

    *num_atts = 0;
    varlist = grp_inq->var_list;
    for(i = 0; i < grp_inq->n_vars; i++)
    {
        if((thevar == varlist->var_inq->full_name) || 
           (thevar == varlist->var_inq->hdf5_name) ||
           (thevar == varlist->var_inq->name))
        {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(varlist->var_inq->n_atts));
            the_att_list = varlist->var_inq->att_list;
            for(j = 0; j < varlist->var_inq->n_atts; j++)
            {
                output[*num_atts] = the_att_list->att_inq->name;
                *num_atts += 1;
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        varlist = varlist->next;
    }

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thevar) ||
           (grplist->grp_inq->hdf5_name == thevar) ||
           (grplist->grp_inq->name == thevar))
        {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(grplist->grp_inq->n_atts));
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                output[*num_atts] = the_att_list->att_inq->name;
                *num_atts += 1;
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetVarAttNames_inGroup(grplist->grp_inq, thevar, num_atts);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclQuark *HDF5GetVarAttNames
#if     NhlNeedProto
(void *therec , NclQuark thevar, int *num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int *num_atts;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    HDF5AttInqRecList *the_att_list;
    NclQuark* output = NULL;
    int i, j;

    *num_atts = 0;
    varlist = thefile->var_list;
    for(i = 0; i < thefile->n_vars; i++)
    {
        if((thevar == varlist->var_inq->name) || 
           (thevar == varlist->var_inq->hdf5_name) ||
           (thevar == varlist->var_inq->full_name))
        {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(varlist->var_inq->n_atts));
            the_att_list = varlist->var_inq->att_list;
            for(j = 0; j < varlist->var_inq->n_atts; j++)
            {
                output[*num_atts] = the_att_list->att_inq->name;
                *num_atts += 1;
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        varlist = varlist->next;
    }

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thevar) ||
           (grplist->grp_inq->hdf5_name == thevar) ||
           (grplist->grp_inq->name == thevar))
        {
            output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(grplist->grp_inq->n_atts));
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                output[*num_atts] = the_att_list->att_inq->name;
                *num_atts += 1;
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetVarAttNames_inGroup(grplist->grp_inq, thevar, num_atts);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclFAttRec *HDF5GetGrpAttInfo_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thegrp, NclQuark theatt)
#else
(grp_inq, thegrp, theatt)
HDF5GrpInqRec *grp_inq;
NclQuark thegrp;
NclQuark theatt;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5AttInqRecList *the_att_list;
    NclFAttRec* output = NULL;
    int i, j;

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thegrp) ||
           (grplist->grp_inq->hdf5_name == thegrp) ||
           (grplist->grp_inq->name == thegrp))
        {
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_att_list->att_inq->type;
                    output->num_elements = the_att_list->att_inq->n_elem;
                    return(output);
                }
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetGrpAttInfo_inGroup(grplist->grp_inq, thegrp, theatt);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclFAttRec *HDF5GetGrpAttInfo
#if     NhlNeedProto
(void *therec, NclQuark thegrp, NclQuark theatt)
#else
(therec, thegrp, theatt)
void *therec;
NclQuark thegrp;
NclQuark theatt;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5AttInqRecList *the_att_list;
    NclFAttRec* output = NULL;
    int i, j;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetGrpAttInfo. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthegrp: <%s>\n", NrmQuarkToString(thegrp));
   *fprintf(stderr, "\ttheatt: <%s>\n", NrmQuarkToString(theatt));
   */

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thegrp) ||
           (grplist->grp_inq->hdf5_name == thegrp) ||
           (grplist->grp_inq->name == thegrp))
        {
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_att_list->att_inq->type;
                    output->num_elements = the_att_list->att_inq->n_elem;
                    return(output);
                }
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetGrpAttInfo_inGroup(grplist->grp_inq, thegrp, theatt);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclFAttRec *HDF5GetVarAttInfo_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thevar, NclQuark theatt)
#else
(grp_inq, thevar, theatt)
HDF5GrpInqRec *grp_inq;
NclQuark thevar;
NclQuark theatt;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    HDF5AttInqRecList *the_att_list;
    NclFAttRec* output = NULL;
    int i, j;

    varlist = grp_inq->var_list;
    for(i = 0; i < grp_inq->n_vars; i++)
    {
        if((thevar == varlist->var_inq->name) ||
           (thevar == varlist->var_inq->hdf5_name) ||
           (thevar == varlist->var_inq->full_name))
        {
            the_att_list = varlist->var_inq->att_list;
            for(j = 0; j < varlist->var_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_att_list->att_inq->type;
                    output->num_elements = the_att_list->att_inq->n_elem;
                    return(output);
                }
                the_att_list = the_att_list->next;
            }
        }
        varlist = varlist->next;
    }

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thevar) ||
           (grplist->grp_inq->hdf5_name == thevar) ||
           (grplist->grp_inq->name == thevar))
        {
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_att_list->att_inq->type;
                    output->num_elements = the_att_list->att_inq->n_elem;
                    return(output);
                }
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetVarAttInfo_inGroup(grplist->grp_inq, thevar, theatt);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclFAttRec *HDF5GetVarAttInfo
#if     NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    HDF5AttInqRecList *the_att_list;
    NclFAttRec* output = NULL;
    int i, j;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetVarAttInfo. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\ttheatt: <%s>\n", NrmQuarkToString(theatt));
   */

    varlist = thefile->var_list;
    for(i = 0; i < thefile->n_vars; i++)
    {
        if((thevar == varlist->var_inq->full_name) ||
           (thevar == varlist->var_inq->hdf5_name) ||
           (thevar == varlist->var_inq->name))
        {
            the_att_list = varlist->var_inq->att_list;
            for(j = 0; j < varlist->var_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_att_list->att_inq->type;
                    output->num_elements = the_att_list->att_inq->n_elem;
                    return(output);
                }
                the_att_list = the_att_list->next;
            }
        }
        varlist = varlist->next;
    }

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thevar) ||
           (grplist->grp_inq->hdf5_name == thevar) ||
           (grplist->grp_inq->name == thevar))
        {
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                    output->att_name_quark = theatt;
                    output->data_type = the_att_list->att_inq->type;
                    output->num_elements = the_att_list->att_inq->n_elem;
                    return(output);
                }
                the_att_list = the_att_list->next;
            }
            return(output);
        }

        output = HDF5GetVarAttInfo_inGroup(grplist->grp_inq, thevar, theatt);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static NclQuark *HDF5GetDimNames
#if     NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5DimInqRecList * thelist;
    NclQuark* names = NULL;
    int i;

    if(NULL == thefile)
	return(names);

    thelist = thefile->dim_list;
    names = NclMalloc(sizeof(NclQuark)*thefile->n_dims);
    *num_dims = thefile->n_dims;
    for(i = 0; i < thefile->n_dims; i++)
    {
        names[thelist->dim_inq->ncldim_id] = thelist->dim_inq->name;
      /*
       *names[i] = thelist->dim_inq->name;
       *fprintf(stderr, "\tnames[%d]: %s\n",
       *        thelist->dim_inq->ncldim_id, NrmQuarkToString(names[thelist->dim_inq->ncldim_id]));
       */
        thelist=thelist->next;
    }

    return(names);
}

static NclFDimRec *HDF5GetDimInfo
#if     NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5DimInqRecList * thelist;
    NclFDimRec *dim_info = NULL;
    int i;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetDimInfo. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tdim_name_q: %s\n", NrmQuarkToString(dim_name_q));
   */

    thelist = thefile->dim_list;
    for(i =0; i < thefile->n_dims; i++)
    {
        if(dim_name_q == thelist->dim_inq->name)
        {
            dim_info = (NclFDimRec*) NclMalloc(sizeof(NclFDimRec));
            dim_info->dim_name_quark = dim_name_q;
            dim_info->is_unlimited = thelist->dim_inq->is_unlimited;
            dim_info->dim_size = thelist->dim_inq->size;
          /*
           *fprintf(stderr, "\tdim_info->dim_name_quark: %s\n", NrmQuarkToString(dim_info->dim_name_quark));
           *fprintf(stderr, "\tdim_info->dim_size: %d\n", dim_info->dim_size);
           *fprintf(stderr, "\tthelist->dim_inq->ncldim_id: %d\n", thelist->dim_inq->ncldim_id);
           */
            return(dim_info);
        }
        thelist= thelist->next;
    }
    return(dim_info);
}

static NclQuark *HDF5GetAttNames
#if     NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5AttInqRecList * the_att_list;
    NclQuark* output = NULL;
    int i;

    *num_atts = 0;
    if(thefile->n_atts > 0)
    {
        output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(thefile->n_atts));
        the_att_list = thefile->att_list;
        for(i = 0; i < thefile->n_atts; i++)
        {
            output[*num_atts] = the_att_list->att_inq->name;
            *num_atts += 1;
            the_att_list = the_att_list->next;
        }
    }

    return(output);
}

static NclFAttRec* HDF5GetAttInfo
#if     NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5AttInqRecList * the_att_list;
    NclFAttRec* output = NULL;

    if(thefile->n_atts > 0)
    {
        the_att_list = thefile->att_list;
        while(the_att_list != NULL)
        {
            if(att_name_q == the_att_list->att_inq->name)
            {
                output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
                output->att_name_quark = att_name_q;
                output->data_type = the_att_list->att_inq->type;
                output->num_elements = the_att_list->att_inq->n_elem;
                return(output);
            }
            else
            {
                the_att_list = the_att_list->next;
            }
        }
    }
    return(output);
}

void _setHDF5AttValue(HDF5AttInqRecList *new_att_list,
                      HDF5AttInqRec *new_att,
                      NclHDF5attr_node_t *attr_node,
                      char *var_name,
                      int update,
                      int len)
{
    switch(new_att_list->att_inq->type)
    {
        case NCL_double:
        case NCL_float:
        case NCL_int64:
        case NCL_uint64:
        case NCL_long:
        case NCL_ulong:
        case NCL_int:
        case NCL_uint:
        case NCL_short:
        case NCL_ushort:
        case NCL_byte:
             new_att->n_elem = len;
             new_att->value = NclMalloc(attr_node->nbytes);
    
             if(!new_att->value)
             {
                 NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att->value, in file: %s, line: %d\n",
                         __FILE__, __LINE__);
                 return;
             }
             memcpy(new_att->value, attr_node->value, attr_node->nbytes);
             break;
        case NCL_char:
        case NCL_string:
             {
                 char *buffer;
                 NclQuark *tmp_quark;
                 int latlon = 0;
		 int i;
		 tmp_quark = (NclQuark *)NclMalloc(len * sizeof(NrmQuark *));
		 for (i = 0; i < len; i++) {
			 tmp_quark[i] = ((NclQuark *)attr_node->value)[i];
		 }
			 

#if 0
                 buffer = (char *)NclMalloc((1 + attr_node->nbytes) * sizeof(char));
                 if(!buffer)
                 {
                     NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for buffer, in file: %s, line: %d\n",
                             __FILE__, __LINE__);
                     return;
                 }

                 memcpy(buffer, attr_node->value, attr_node->nbytes);
                 len = attr_node->nbytes - 1;
                 buffer[len] = '\0';


                 tmp_quark = (NclQuark*)NclMalloc(sizeof(NclQuark));
                 if(!tmp_quark)
                 {
                     NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for tmp_quark, in file: %s, line: %d\n",
                             __FILE__, __LINE__);
                     return;
                 }
#endif
                 if(update)
                 {
                     if(0 == strcmp("LATITUDE", var_name))
                     {
                         if(0 == strcmp("units", attr_node->name))
                         {
                             latlon = 1;
                             *tmp_quark = NrmStringToQuark("degree_north");
                         }
                     }
                     else if(0 == strcmp("LONGITUDE", var_name))
                     {
                         if(0 == strcmp("units", attr_node->name))
                         {
                             latlon = 1;
                             *tmp_quark = NrmStringToQuark("degree_east");
                         }
                     }
                 }

		 new_att->value = (void *)tmp_quark;
                 if(latlon)
                 {
			 new_att->n_elem = 1;
                 }
		 else {
			 new_att->n_elem = len;
		 }

             }
             break;
        default:
           /*
            *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
            *fprintf(stderr, "\tnew_att->type: <0%o>\n", (unsigned int) new_att->type);
            *fprintf(stderr, "\tDONOT know how to print new_att->value\n");
            */
             break;
    }
}

static int _HDF5get_var_att_list(HDF5AttInqRecList **HDF5var_att_list,
                                 NclHDF5attr_list_t *NclHDF5attr_list,
                                 int update, char *var_name)
{
    NclHDF5attr_list_t *curHDF5attr_list;
    int i = 0;
    int len = 0;
    int n_atts = 0;

    curHDF5attr_list = NclHDF5attr_list;

    while(curHDF5attr_list)
    {
        NclHDF5attr_node_t *attr_node = curHDF5attr_list->attr_node;
        HDF5AttInqRecList *new_att_list = NclCalloc(1, sizeof(HDF5AttInqRecList));
        HDF5AttInqRec *new_att = NclCalloc(1, sizeof(HDF5AttInqRec));

        if(!new_att_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NhlFATAL;
        }

        if(!new_att)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NhlFATAL;
        }

        n_atts++;
        new_att_list->att_inq = new_att;
        new_att->name = NrmStringToQuark(attr_node->name);
        new_att->type = _HDF52Ncl_type(attr_node->type_name);

        new_att_list->next = *HDF5var_att_list;
        *HDF5var_att_list = new_att_list;

        len = 1;
        for(i = 0; i < attr_node->ndims; i++)
        {
            len *= attr_node->dims[i];
        }

        _setHDF5AttValue(new_att_list, new_att, attr_node,
                         var_name, update, len);

        curHDF5attr_list = curHDF5attr_list->next;
    }

    return n_atts;
}

HDF5AttInqRec *_find_dim_att_inq(HDF5AttInqRecList *head_att_list, int n_atts, NclQuark attname)
{
    HDF5AttInqRecList *att_list = head_att_list;
    HDF5AttInqRec *att_inq = NULL;
    int n;

    att_list = head_att_list;
    for(n = 0; n < n_atts; n++)
    {
        att_inq = att_list->att_inq;

        if(att_inq->type != NCL_string) {
		att_list = att_list->next;
		continue;
	}

        if(attname == att_inq->name)
            return att_inq;

        att_list = att_list->next;
    }

    return NULL;
}

void HDF5SetVarDimName(HDF5VarInqRec *var_inq)
{
    HDF5AttInqRec *att_inq;
    char *ori_str = NULL;
    char *tmp_str = NULL;
    char *result = NULL;
    char delimiter[3] = " ,";

    int i = 0;
    int n = 0;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5SetVarDimName. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_inq->name: <%s>\n", NrmQuarkToString(var_inq->name));
   */

    for(n = 0; n < NUMPOSDIMNAMES; ++n)
    {
        if(var_inq->has_dim_name)
            return;

        att_inq = _find_dim_att_inq(var_inq->att_list, var_inq->n_atts, possibleDimNames[n]);

        if(NULL == att_inq)
            continue;

        if(att_inq->type != NCL_string)
            continue;

      /*
       *fprintf(stderr, "\tFind Attr: <%s>\n", NrmQuarkToString(att_inq->name));
       */

        i = 0;
        ori_str = NrmQuarkToString(*(NclQuark *) att_inq->value);
        tmp_str = strdup(ori_str);

      /*
       *fprintf(stderr, "\tOri Str: <%s>\n", ori_str);
       */

        result = strtok(tmp_str, delimiter);
        while(NULL != result)
        {
            var_inq->dim_name[i] = NrmStringToQuark(result);
          /*
           *fprintf(stderr, "\tdim %d: <%s>\n", i, result);
           */

            result = strtok(NULL, delimiter);
            ++i;
            if(i >= var_inq->n_dims)
                break;
        }

        free(tmp_str);
        var_inq->has_dim_name = 1;
    }
}

void _HDF5Build_grp_list_inGroup(HDF5GrpInqRec **the_grp, NclHDF5group_node_t *HDF5group)
{
    HDF5GrpInqRecList *grp_list = NULL;
    HDF5VarInqRecList *var_list = NULL;
    HDF5AttInqRecList *att_list = NULL;

    NclHDF5attr_list_t *h5grp_att_list;
    NclHDF5attr_node_t *attr_node;

    NclHDF5dataset_list_t *dataset_list;
    NclHDF5dataset_node_t *dataset_node;

    NclHDF5group_list_t *h5group_list;
    NclHDF5group_node_t *h5group_node;
    char *ptr2shortname;
    char tmp_str[1024];

    int i = 0;
    int len = 0;
    int n = 0;
    int n_grps = 0;
    int n_vars = 0;
    int n_atts = 0;

    *the_grp = NclCalloc(1, sizeof(HDF5GrpInqRec));
    if(!the_grp)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for the_grp, in file: %s, line: %d\n",
                __FILE__, __LINE__);
        return;
    }

    (*the_grp)->id = HDF5group->id;

    if(strlen(HDF5group->name))
    {
        ptr2shortname = strrchr(HDF5group->name, '/');
        ptr2shortname = ptr2shortname + 1;
        (*the_grp)->hdf5_name = NrmStringToQuark(ptr2shortname);
        (*the_grp)->full_name = NrmStringToQuark(HDF5group->name);
        strcpy(tmp_str, ptr2shortname);
        for(n = 0; n < strlen(tmp_str); n++)
        {
            if(! isalnum(tmp_str[n]))
            {
                tmp_str[n] = '_';
            }
        }
        (*the_grp)->name = NrmStringToQuark(tmp_str);
    }
    else
    {
        (*the_grp)->name = NrmStringToQuark("");
        (*the_grp)->hdf5_name = NrmStringToQuark("");
        (*the_grp)->full_name = NrmStringToQuark("/");
	ptr2shortname = "";
    }

  /*
   *fprintf(stderr, "\n\n\nhit _HDF5Build_grp_list. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tHDF5group->name: <%s>\n", HDF5group->name);
   *fprintf(stderr, "\tgrp->name: <%s>\n", NrmQuarkToString((*the_grp)->name));
   *fprintf(stderr, "\tgrp->hdf5_name: <%s>\n", NrmQuarkToString((*the_grp)->hdf5_name));
   *fprintf(stderr, "\tgrp->full_name: <%s>\n", NrmQuarkToString((*the_grp)->full_name));
   */

    (*the_grp)->file = NrmStringToQuark(HDF5group->file);
    (*the_grp)->type = NCL_group;

    h5grp_att_list = HDF5group->attr_list;

    while(h5grp_att_list)
    {
        HDF5AttInqRec *new_att = NclCalloc(1, sizeof(HDF5AttInqRec));
        HDF5AttInqRecList *new_att_list;

        attr_node = h5grp_att_list->attr_node;

        n_atts++;

        new_att_list = NclCalloc(1, sizeof(HDF5AttInqRecList));
        if(!new_att_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        new_att = NclCalloc(1, sizeof(HDF5DimInqRec));
        if(!new_att)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        new_att_list->att_inq = new_att;
        new_att->name = NrmStringToQuark(attr_node->name);
        new_att->type = _HDF52Ncl_type(attr_node->type_name);

        new_att_list->next = att_list;
        att_list = new_att_list;

        len = 1;
        for(i = 0; i < attr_node->ndims; i++)
        {
            len *= attr_node->dims[i];
        }

        _setHDF5AttValue(new_att_list, new_att_list->att_inq, attr_node,
                         ptr2shortname, 0, len);

        h5grp_att_list = h5grp_att_list->next;
    }

    (*the_grp)->n_atts = n_atts;
    (*the_grp)->att_list = att_list;

    dataset_list = HDF5group->dataset_list;

    while(dataset_list)
    {
        HDF5VarInqRecList *var_cur_list = NULL;
        HDF5AttInqRecList *var_att_list;

        dataset_node = dataset_list->dataset_node;

        var_cur_list = NclCalloc(1, sizeof(HDF5VarInqRecList));
        if(!var_cur_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for var_cur_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        var_cur_list->var_inq = NclCalloc(1, sizeof(HDF5VarInqRec));
        if(!var_cur_list->var_inq)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for var_cur_list->var_inq, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        if(dataset_node->attr_list)
        {
            var_cur_list->var_inq->n_atts = _HDF5get_var_att_list(&var_att_list, dataset_node->attr_list, 1, dataset_node->short_name);
            var_cur_list->var_inq->att_list = var_att_list;
        }
        else
        {
            var_cur_list->var_inq->n_atts = 0;
            var_cur_list->var_inq->att_list = NULL;
        }

        n_vars++;

        var_cur_list->var_inq->n_dims = dataset_node->ndims;
        var_cur_list->var_inq->full_name = NrmStringToQuark(dataset_node->name);
        var_cur_list->var_inq->hdf5_name = NrmStringToQuark(dataset_node->short_name);
        var_cur_list->var_inq->id = dataset_node->id;

        strcpy(tmp_str, dataset_node->short_name);
        for(n = 0; n < strlen(tmp_str); n++)
        {
            if(! isalnum(tmp_str[n]))
            {
                tmp_str[n] = '_';
            }
        }
        var_cur_list->var_inq->name = NrmStringToQuark(tmp_str);

      /*
       *fprintf(stderr, "\n\n\tvar_cur_list. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tvar_cur_list->name: <%s>\n", NrmQuarkToString(var_cur_list->var_inq->name));
       *fprintf(stderr, "\tvar_cur_list->hdf5_name: <%s>\n", NrmQuarkToString(var_cur_list->var_inq->hdf5_name));
       *fprintf(stderr, "\tvar_cur_list->full_name: <%s>\n", NrmQuarkToString(var_cur_list->var_inq->full_name));
       */
  
        var_cur_list->var_inq->type = _HDF52Ncl_type(dataset_node->type_name);

        if(var_cur_list->var_inq->type == NCL_none)
        {
          /*
           *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "dataset_node->type_name: <%s>\n", dataset_node->type_name);
           *fprintf(stderr, "dataset_node->type: <0%o>\n", (long) dataset_node->type);
           */
            var_cur_list->var_inq->type = NCL_none;
        }
 
        for(i = 0; i < dataset_node->ndims; i++)
        {
            var_cur_list->var_inq->dim[i] = (long) dataset_node->dims[i];
            var_cur_list->var_inq->dim_name[i] = NrmStringToQuark(dataset_node->dim_name[i]);
        }

        if(!var_cur_list->var_inq->has_dim_name)
            HDF5SetVarDimName(var_cur_list->var_inq);

        if(var_cur_list->var_inq->type == NCL_compound)
        {
          /*
           *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "dataset_node->type_name: <%s>\n", dataset_node->type_name);
           *fprintf(stderr, "dataset_node->type: 0%o\n", (long) dataset_node->type);
           *fprintf(stderr, "dataset_node->ndims: %d\n", dataset_node->ndims);
           */
            var_cur_list->var_inq->compound = (HDF5compound_t *) NclMalloc(sizeof(HDF5compound_t));
            if(! var_cur_list->var_inq->compound)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can not allocate memory for: var_cur_list->var_inq->compound"));
                return;
            }

            var_cur_list->var_inq->compound->member = (HDF5compound_component_t *)
                                                       NclMalloc(dataset_node->compound.nom *
                                                       sizeof(HDF5compound_component_t));
            if(! var_cur_list->var_inq->compound->member)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclHDF5: Can not allocate memory for: var_cur_list->var_inq->compound->member"));
                return;
            }

            var_cur_list->var_inq->compound->nom = dataset_node->compound.nom;
            for(i = 0; i < dataset_node->compound.nom; i++)
            {
              /*
               *fprintf(stderr, "\tcompound->member[%d].name: <%s>\n",
               *            i, dataset_node->compound.member[i].name);
               *fprintf(stderr, "\tcompound->member[%d].type: <%s>\n",
               *            i, dataset_node->compound.member[i].type);
               *fprintf(stderr, "\tcompound->member[%d].type: %s\n",
               *            i, dataset_node->compound.member[i].type);
               *fprintf(stderr, "\tcompound->member[%d].offset: %d\n",
               *            i, dataset_node->compound.member[i].offset);
               */
                var_cur_list->var_inq->compound->member[i].name = NrmStringToQuark(dataset_node->compound.member[i].name);
                var_cur_list->var_inq->compound->member[i].type = _HDF52Ncl_type(dataset_node->compound.member[i].type);
                var_cur_list->var_inq->compound->member[i].offset = dataset_node->compound.member[i].offset;
                var_cur_list->var_inq->compound->member[i].is_str = dataset_node->compound.member[i].is_str;
            }
        }
        else
        {
            var_cur_list->var_inq->compound = NULL;
        }

        var_cur_list->next = var_list;
        var_list = var_cur_list;

        dataset_list = dataset_list->next;
    }

    (*the_grp)->n_vars = n_vars;
    (*the_grp)->var_list = var_list;

    h5group_list = HDF5group->group_list;

    while(h5group_list)
    {
        HDF5GrpInqRecList *grp_cur_list = NULL;

        n_grps++;

        h5group_node = h5group_list->group_node;

        grp_cur_list = NclCalloc(1, sizeof(HDF5GrpInqRecList));
        if(!grp_cur_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for grp_cur_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        _HDF5Build_grp_list_inGroup(&(grp_cur_list->grp_inq), h5group_node);

        grp_cur_list->next = grp_list;
        grp_list = grp_cur_list;

        h5group_list = h5group_list->next;
    }

    (*the_grp)->n_grps = n_grps;
    (*the_grp)->grp_list = grp_list;

    HDF5group->num_groups = n_grps;
    HDF5group->num_datasets = n_vars;
    HDF5group->num_attrs = n_atts;
}

HDF5GrpInqRec *_HDF5Build_grp_list(NclHDF5group_node_t *HDF5group)
{
    HDF5GrpInqRecList *grp_list = NULL;
    HDF5VarInqRecList *var_list = NULL;
    HDF5AttInqRecList *att_list = NULL;

    HDF5GrpInqRec *the_grp;

    NclHDF5attr_list_t *h5grp_att_list;
    NclHDF5attr_node_t *attr_node;

    NclHDF5dataset_list_t *dataset_list;
    NclHDF5dataset_node_t *dataset_node;

    NclHDF5group_list_t *h5group_list;
    NclHDF5group_node_t *h5group_node;
    char *ptr2shortname;
    char tmp_str[1024];

    int i = 0;
    int len = 0;
    int n = 0;
    int n_grps = 0;
    int n_vars = 0;
    int n_atts = 0;

    the_grp = NclCalloc(1, sizeof(HDF5GrpInqRec));
    if(!the_grp)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for the_grp, in file: %s, line: %d\n",
                __FILE__, __LINE__);
        return NULL;
    }

    if(strlen(HDF5group->name))
    {
        ptr2shortname = strrchr(HDF5group->name, '/');
        ptr2shortname = ptr2shortname + 1;
        the_grp->hdf5_name = NrmStringToQuark(ptr2shortname);
        the_grp->full_name = NrmStringToQuark(HDF5group->name);
        the_grp->id = HDF5group->id;
        strcpy(tmp_str, ptr2shortname);
        for(n = 0; n < strlen(tmp_str); n++)
        {
            if(! isalnum(tmp_str[n]))
            {
                tmp_str[n] = '_';
            }
        }
        the_grp->name = NrmStringToQuark(tmp_str);
    }
    else
    {
        the_grp->name = NrmStringToQuark("");
        the_grp->hdf5_name = NrmStringToQuark("");
        the_grp->full_name = NrmStringToQuark("/");
	ptr2shortname = "";
    }

  /*
   *fprintf(stderr, "\n\n\nhit _HDF5Build_grp_list. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tHDF5group->name: <%s>\n", HDF5group->name);
   *fprintf(stderr, "\tgrp->name: <%s>\n", NrmQuarkToString(the_grp->name));
   *fprintf(stderr, "\tgrp->hdf5_name: <%s>\n", NrmQuarkToString(the_grp->hdf5_name));
   *fprintf(stderr, "\tgrp->full_name: <%s>\n", NrmQuarkToString(the_grp->full_name));
   */

    the_grp->file = NrmStringToQuark(HDF5group->file);
    the_grp->type = NCL_group;

    h5grp_att_list = HDF5group->attr_list;

    while(h5grp_att_list)
    {
        HDF5AttInqRec *new_att = NclCalloc(1, sizeof(HDF5AttInqRec));
        HDF5AttInqRecList *new_att_list;

        attr_node = h5grp_att_list->attr_node;

        n_atts++;

        new_att_list = NclCalloc(1, sizeof(HDF5AttInqRecList));
        if(!new_att_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        new_att = NclCalloc(1, sizeof(HDF5DimInqRec));
        if(!new_att)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for new_att, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        new_att_list->att_inq = new_att;
        new_att->name = NrmStringToQuark(attr_node->name);
        new_att->type = _HDF52Ncl_type(attr_node->type_name);

        new_att_list->next = att_list;
        att_list = new_att_list;

        len = 1;
        for(i = 0; i < attr_node->ndims; i++)
        {
            len *= attr_node->dims[i];
        }

        _setHDF5AttValue(new_att_list, new_att_list->att_inq, attr_node,
                         ptr2shortname, 0, len);

        h5grp_att_list = h5grp_att_list->next;
    }

    the_grp->n_atts = n_atts;
    the_grp->att_list = att_list;

    dataset_list = HDF5group->dataset_list;

    while(dataset_list)
    {
        HDF5VarInqRecList *var_cur_list = NULL;
        HDF5AttInqRecList *var_att_list;

        dataset_node = dataset_list->dataset_node;

        var_cur_list = NclCalloc(1, sizeof(HDF5VarInqRecList));
        if(!var_cur_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for var_cur_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        var_cur_list->var_inq = NclCalloc(1, sizeof(HDF5VarInqRec));
        if(!var_cur_list->var_inq)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for var_cur_list->var_inq, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        if(dataset_node->attr_list)
        {
            var_cur_list->var_inq->n_atts = _HDF5get_var_att_list(&var_att_list, dataset_node->attr_list, 1, dataset_node->short_name);
            var_cur_list->var_inq->att_list = var_att_list;
        }
        else
        {
            var_cur_list->var_inq->n_atts = 0;
            var_cur_list->var_inq->att_list = NULL;
        }

        n_vars++;

        var_cur_list->var_inq->n_dims = dataset_node->ndims;
        var_cur_list->var_inq->full_name = NrmStringToQuark(dataset_node->name);
        var_cur_list->var_inq->hdf5_name = NrmStringToQuark(dataset_node->short_name);
        var_cur_list->var_inq->id = dataset_node->id;

        strcpy(tmp_str, dataset_node->short_name);
        for(n = 0; n < strlen(tmp_str); n++)
        {
            if(! isalnum(tmp_str[n]))
            {
                tmp_str[n] = '_';
            }
        }
        var_cur_list->var_inq->name = NrmStringToQuark(tmp_str);
  
        var_cur_list->var_inq->type = _HDF52Ncl_type(dataset_node->type_name);

      /*
       *if(var_cur_list->var_inq->type == NCL_none)
       *{
       *   *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
       *   *fprintf(stderr, "dataset_node->type_name: <%s>\n", dataset_node->type_name);
       *   *fprintf(stderr, "dataset_node->type: <0%o>\n", (long) dataset_node->type);
       *    var_cur_list->var_inq->type = NCL_none;
       *}
       */

        if (! strcmp(dataset_node->space_name,"SCALAR")) {
		var_cur_list->var_inq->dim[i] = (long) 1;
		var_cur_list->var_inq->n_dims = 1;
		var_cur_list->var_inq->dim_name[i] = NrmStringToQuark("ncl_scalar");
	}
        else {
		for(i = 0; i < dataset_node->ndims; i++)
		{
			var_cur_list->var_inq->dim[i] = (long) dataset_node->dims[i];
			var_cur_list->var_inq->dim_name[i] = NrmStringToQuark(dataset_node->dim_name[i]);
		}
	}
        if(var_cur_list->var_inq->type == NCL_compound)
        {
          /*
           *fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "dataset_node->type_name: <%s>\n", dataset_node->type_name);
           *fprintf(stderr, "dataset_node->type: 0%o\n", (long) dataset_node->type);
           *fprintf(stderr, "dataset_node->ndims: %d\n", dataset_node->ndims);
           *fprintf(stderr, "dataset_node->compound->nom: %d\n", dataset_node->compound->nom);
           *fprintf(stderr, "dataset_node->compound->size: %d\n", dataset_node->compound->size);
           */
            var_cur_list->var_inq->compound = (HDF5compound_t *) NclMalloc(sizeof(HDF5compound_t));
            if(! var_cur_list->var_inq->compound)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can not allocate memory for: var_cur_list->var_inq->compound"));
                return(NULL);
            }
            var_cur_list->var_inq->compound->member = (HDF5compound_component_t *)
                                                      NclMalloc(dataset_node->compound.nom *
                                                      sizeof(HDF5compound_component_t));
            if(! var_cur_list->var_inq->compound->member)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclHDF5: Can not allocate memory for: var_cur_list->var_inq->compound->member"));
                return(NULL);
            }
            var_cur_list->var_inq->compound->nom = dataset_node->compound.nom;
            var_cur_list->var_inq->compound->size = dataset_node->compound.size;

            for(i = 0; i < dataset_node->compound.nom; i++)
            {
              /*
               *fprintf(stderr, "\tcompound->member[%d].name: <%s>\n",
               *            i, dataset_node->compound->member[i].name);
               *fprintf(stderr, "\tcompound->member[%d].type: <%s>\n",
               *            i, dataset_node->compound->member[i].type);
               *fprintf(stderr, "\tcompound->member[%d].type: %s\n",
               *            i, dataset_node->compound->member[i].type);
               *fprintf(stderr, "\tcompound->member[%d].offset: %d\n",
               *            i, dataset_node->compound->member[i].offset);
               */
                var_cur_list->var_inq->compound->member[i].name = NrmStringToQuark(dataset_node->compound.member[i].name);
                var_cur_list->var_inq->compound->member[i].type = _HDF52Ncl_type(dataset_node->compound.member[i].type);
                var_cur_list->var_inq->compound->member[i].offset = dataset_node->compound.member[i].offset;
                var_cur_list->var_inq->compound->member[i].is_str = dataset_node->compound.member[i].is_str;
            }
        }
        else
        {
            var_cur_list->var_inq->compound = NULL;
        }

        HDF5SetVarDimName(var_cur_list->var_inq);

        var_cur_list->next = var_list;
        var_list = var_cur_list;

        dataset_list = dataset_list->next;
    }

    the_grp->n_vars = n_vars;
    the_grp->var_list = var_list;

    h5group_list = HDF5group->group_list;

    while(h5group_list)
    {
        HDF5GrpInqRecList *grp_cur_list = NULL;

        n_grps++;

        h5group_node = h5group_list->group_node;

        grp_cur_list = NclCalloc(1, sizeof(HDF5GrpInqRecList));
        if(!grp_cur_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for grp_cur_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        _HDF5Build_grp_list_inGroup(&(grp_cur_list->grp_inq), h5group_node);

        grp_cur_list->next = grp_list;
        grp_list = grp_cur_list;

        h5group_list = h5group_list->next;
    }

    the_grp->n_grps = n_grps;
    the_grp->grp_list = grp_list;

    HDF5group->num_groups = n_grps;
    HDF5group->num_datasets = n_vars;
    HDF5group->num_attrs = n_atts;

    return the_grp;
}

int _HDF5Build_dim_list_from_dim_group(HDF5DimInqRecList **dim_list,
                                       NclHDF5group_node_t *dim_group)
{
    NclHDF5group_list_t   *group_list;
    HDF5DimInqRecList *cur_list = NULL;

    int n;
    int n_dims = 0;

  /*
   *fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims: %d\n", n_dims);
   */

    group_list = dim_group->group_list;

    while(group_list)
    {
        NclHDF5group_node_t *group_node = group_list->group_node;
        NclHDF5attr_list_t *attr_list;
        char *short_name = strrchr(group_node->name, '/');

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tGroup %d, name: <%s>\n", n, group_node->name);
       *fprintf(stderr, "\tGroup %d, type_name: <%s>\n", n, group_node->type_name);
       */

        cur_list = NclCalloc(1, sizeof(HDF5DimInqRecList));
        if(!cur_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NhlFATAL;
        }

        cur_list->dim_inq = NclCalloc(1, sizeof(HDF5DimInqRec));
        if(!cur_list->dim_inq)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list->dim_inq, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NhlFATAL;
        }

        cur_list->dim_inq->is_dataset = 0;
        cur_list->dim_inq->is_unlimited = 0;
        cur_list->dim_inq->name = NrmStringToQuark(short_name+1);

        attr_list = group_node->attr_list;

        for(n = 0; n < group_node->num_attrs; n++)
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tAttr %d, name: <%s>\n", n, attr_list->attr_node->name);
           *fprintf(stderr, "\tAttr %d, type_name: <%s>\n", n, attr_list->attr_node->type_name);
           *fprintf(stderr, "\tAttr %d, ndims: <%d>\n", n, attr_list->attr_node->ndims);
           *if(attr_list->attr_node->ndims)
           *   *fprintf(stderr, "\tAttr %d, dims[0]: <%d>\n", n, attr_list->attr_node->dims[0]);
           */

            if(0 == strcmp(attr_list->attr_node->name, "Size"))
            {
                long *lp = (long *)attr_list->attr_node->value;
                cur_list->dim_inq->ncldim_id = n_dims;
                cur_list->dim_inq->size = *lp;

              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tvalue: %d\n", *lp);
               *fprintf(stderr, "\tcur_list->dim_inq->ncldim_id: %d: name <%s>\n",
               *        cur_list->dim_inq->ncldim_id, NrmQuarkToString(cur_list->dim_inq->name));
               *fprintf(stderr, "\tcur_list->dim_inq->size: %ld\n", cur_list->dim_inq->size);
               */
            }
            else if(0 == strcmp(attr_list->attr_node->name, "Description"))
            {
                char *cp = (char *)attr_list->attr_node->value;
                cur_list->dim_inq->description = NrmStringToQuark(cp);
              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tdescription: <%s>\n", cp);
               */
            }
          /*
           *else
           *{
               *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tAttr %d, name: <%s>\n", n, attr_list->attr_node->name);
               *fprintf(stderr, "\tAttr %d, type_name: <%s>\n", n, attr_list->attr_node->type_name);
           *}
           */
            attr_list = attr_list->next;
        }

        cur_list->next = *dim_list;
        *dim_list = cur_list;

        n_dims++;

        group_list = group_list->next;
    }

    return n_dims;
}

static NclHDF5attr_list_t *_get_dim_attr_list(NclHDF5attr_list_t *head_attr_list, char *dimstr)
{
    NclHDF5attr_list_t *attr_list = head_attr_list;

    while(attr_list)
    {
        if(0 == strcmp(attr_list->attr_node->name, dimstr))
            return attr_list;

        attr_list = attr_list->next;
    }

    return NULL;
}

static void _update_dim_list(HDF5DimInqRecList **dim_list, int *n_dims, NclHDF5dataset_node_t *dataset_node)
{
    NclHDF5attr_list_t *attr_list = NULL;

    int i, k, n;
    int num_new_dim;
    int num_old_dim;
    int found_new;
    int has_updated = 0;
    nclH5size_t old_dim_size[4*MAX_HDF5_DIMS];
    NclQuark old_dim_name[4*MAX_HDF5_DIMS];
    NclQuark new_dim_name[4*MAX_HDF5_DIMS];
    NclQuark tmp_name;
    char *tmp_str = NULL;

    char *ori_str = NULL;
    char *result = NULL;
    char delimiter[3] = " ,";
    int is_dataset = 0;

    HDF5DimInqRecList *cur_list = NULL;

  /*
   *fprintf(stderr, "\n\n\nhit _update_dim_list. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims: %d\n", *n_dims);
   */

    cur_list = *dim_list;
    num_old_dim = 0;
    while((NULL != cur_list) && (num_old_dim < *n_dims))
    {
      /*
       *fprintf(stderr, "\tOld Dim %d: name <%s>, size: %ld\n", i, NrmQuarkToString(cur_list->dim_inq->name), cur_list->dim_inq->size);
       */
        old_dim_name[num_old_dim] = cur_list->dim_inq->name;
        old_dim_size[num_old_dim] = cur_list->dim_inq->size;
        cur_list = cur_list->next;
	++num_old_dim;
    }

    for(n = 0; n < NUMPOSDIMNAMES; ++n)
    {
        if(has_updated)
            return;

        attr_list = _get_dim_attr_list(dataset_node->attr_list, NrmQuarkToString(possibleDimNames[n]));

        if(NULL == attr_list)
            continue;

        num_new_dim = 0;
	ori_str = NrmQuarkToString(*(NclQuark *) attr_list->attr_node->value);
        tmp_str = strdup(ori_str);
	result = strtok(tmp_str, delimiter);
        while(result != NULL)
        {
            new_dim_name[num_new_dim] = NrmStringToQuark(result);
          /*
           *fprintf(stderr, "\tresult: %s\n", result);
           *fprintf(stderr, "\tnew_dim_name[%d]: %s\n", num_new_dim, NrmQuarkToString(new_dim_name[num_new_dim]));
           */

            result = strtok(NULL, delimiter);
            num_new_dim++;
        }
        free(tmp_str);

      /*
       *fprintf(stderr, "\tnum_new_dim: %d\n", num_new_dim);
       *fprintf(stderr, "\tn_dims: %d\n", *n_dims);
       *fprintf(stderr, "\tattr_list->attr_node->ndims: %d\n", attr_list->attr_node->ndims);
       */

	if(num_new_dim != dataset_node->ndims)
            break;

        for(i = 0; i < num_new_dim; i++)
        {
            tmp_str = strrchr(NrmQuarkToString(new_dim_name[i]), '/');
            if(tmp_str)
            {
                tmp_name = NrmStringToQuark(tmp_str + 1);
                is_dataset = 1;
            }
            else
            {
                tmp_name = new_dim_name[i];
                is_dataset = 0;
            }

            found_new = 1;
            for(k = 0; k < num_old_dim; k++)
            {
                if(old_dim_name[k] == tmp_name)
                {
                    found_new = 0;
                    break;
                }
            }

            if(found_new)
            {
                cur_list = NclCalloc(1, sizeof(HDF5DimInqRecList));
                if(!cur_list)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list, in file: %s, line: %d\n",
                            __FILE__, __LINE__);
                    return;
                }

                cur_list->dim_inq = NclCalloc(1, sizeof(HDF5DimInqRec));
                if(!cur_list->dim_inq)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list->dim_inq, in file: %s, line: %d\n",
                            __FILE__, __LINE__);
                    return;
                }

                cur_list->dim_inq->is_dataset = is_dataset;
                cur_list->dim_inq->is_unlimited = 0;
                cur_list->dim_inq->name = tmp_name;

                if(tmp_name != new_dim_name[i])
                {
                    cur_list->dim_inq->dataset_name = new_dim_name[i];
                }

                cur_list->dim_inq->ncldim_id = *n_dims;
                cur_list->dim_inq->size = (long) dataset_node->dims[i];
                strcpy(dataset_node->dim_name[i], NrmQuarkToString(tmp_name));

              /*
               *fprintf(stderr, "\n\n\nhit _HDF5Build_dim_list. file: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tOld Dim %d: name <%s>\n", *n_dims, NrmQuarkToString(old_dim_name[*n_dims]));
               *fprintf(stderr, "\tcur_list->dim_inq->size: %ld\n", cur_list->dim_inq->size);
               *fprintf(stderr, "\tcur_list->dim_inq->ncldim_id: %d: name <%s>\n",
               *                   cur_list->dim_inq->ncldim_id, NrmQuarkToString(tmp_name));
               */

                cur_list->next = *dim_list;
                *dim_list = cur_list;

                (*n_dims)++;
            }
        }

        has_updated = 1;
    }

    if(has_updated)
	return;

    for(i = 0; i < dataset_node->ndims; ++i)
    {
        found_new = 1;

        if(dataset_node->dim_name[i][0])
        {
            for(n = 0; n < num_old_dim; ++n)
            {
                if((old_dim_size[n] == dataset_node->dims[i]) &&
                   (NrmStringToQuark(dataset_node->dim_name[i]) == old_dim_name[n]))
                {
                    found_new = 0;
		    break;
	        }
	    }
            continue;
	}
	else
        {
            for(n = 0; n < num_old_dim; ++n)
            {
                if(old_dim_size[n] == dataset_node->dims[i])
                {
		    strcpy(dataset_node->dim_name[i], NrmQuarkToString(old_dim_name[n]));
                    found_new = 0;
		    break;
	        }
	    }

            if(! found_new)
                continue;
	}

        cur_list = NclCalloc(1, sizeof(HDF5DimInqRecList));
        if(!cur_list)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        cur_list->dim_inq = NclCalloc(1, sizeof(HDF5DimInqRec));
        if(!cur_list->dim_inq)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list->dim_inq, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return;
        }

        sprintf(dataset_node->dim_name[i], "DIM_%.3d", *n_dims);
        cur_list->dim_inq->is_dataset = 0;
        cur_list->dim_inq->is_unlimited = 0;
        cur_list->dim_inq->name = NrmStringToQuark(dataset_node->dim_name[i]);

        cur_list->dim_inq->ncldim_id = *n_dims;
        cur_list->dim_inq->size = (long) dataset_node->dims[i];

        cur_list->next = *dim_list;
        *dim_list = cur_list;

        (*n_dims)++;
    }
}

static void _HDF5Build_dim_list(HDF5DimInqRecList **dim_list, int *n_dims, NclHDF5group_node_t *HDF5group)
{
    NclHDF5dataset_list_t *dataset_list = NULL;
    NclHDF5group_list_t   *group_list = NULL;

    NclHDF5dataset_node_t *dataset_node = NULL;
    NclHDF5attr_list_t *attr_list = NULL;
    NclHDF5attr_list_t *new_attr_list = NULL;
    short need_add_fillvalue_attribute = 0;

    int n;

  /*
   *fprintf(stderr, "\n\n\nhit _HDF5Build_dim_list. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims: %d\n", *n_dims);
   */

    dataset_list = HDF5group->dataset_list;

    while(dataset_list)
    {
        dataset_node = dataset_list->dataset_node;
        attr_list = dataset_node->attr_list;

        _update_dim_list(dim_list, n_dims, dataset_node);

        need_add_fillvalue_attribute = 1;

        while(attr_list)
        {
            if(0 == strcmp(attr_list->attr_node->name, "_FillValue"))
            {
                need_add_fillvalue_attribute = 0;
                break;
            }
            attr_list = attr_list->next;
        }

        if(need_add_fillvalue_attribute)
        {
            attr_list = dataset_node->attr_list;

            while(attr_list)
            {
                if((0 == strcmp(attr_list->attr_node->name, "MissingValue")) ||
                   (0 == strcmp(attr_list->attr_node->name, "CodeMissingValue")))
                {
                   new_attr_list = (NclHDF5attr_list_t *)NclCalloc(1, sizeof(NclHDF5attr_list_t));
                   new_attr_list->attr_node = NclCalloc(1, sizeof(NclHDF5attr_node_t));
                   if(!new_attr_list->attr_node)
                   {
                       fprintf(stderr, "Failed to allocated memory for new_attr_list->attr_node. in file: %s, line: %d\n",
                               __FILE__, __LINE__);
                       return;
                   }

                   strcpy(new_attr_list->attr_node->name, "_FillValue");
                   strcpy(new_attr_list->attr_node->type_name, dataset_node->type_name);
                   strcpy(new_attr_list->attr_node->dataspace, attr_list->attr_node->dataspace);

                   new_attr_list->attr_node->id = attr_list->attr_node->id;
                   new_attr_list->attr_node->type = dataset_node->type;
                   new_attr_list->attr_node->p_type = attr_list->attr_node->p_type;
                   new_attr_list->attr_node->space = attr_list->attr_node->space;
                   new_attr_list->attr_node->space_type = attr_list->attr_node->space_type;
                   new_attr_list->attr_node->counter = 1 + dataset_node->attr_list->attr_node->counter;
                   new_attr_list->attr_node->nbytes = _NclSizeOf(_HDF52Ncl_type(dataset_node->type_name));
                   new_attr_list->attr_node->ndims = attr_list->attr_node->ndims;
                   for(n = 0; n < new_attr_list->attr_node->ndims; ++n)
                       new_attr_list->attr_node->dims[n] = attr_list->attr_node->dims[n];
                   new_attr_list->attr_node->value = NclCalloc(1, new_attr_list->attr_node->nbytes);

                   if(0 == strcmp("string", attr_list->attr_node->type_name))
                   {
                       NclQuark qav = NrmStringToQuark((char*)attr_list->attr_node->value);
                       _NclScalarForcedCoerce((void*)&qav, NCL_string,
                                              new_attr_list->attr_node->value, _HDF52Ncl_type(new_attr_list->attr_node->type_name));
                   }
                   else
                   {
                       _NclScalarForcedCoerce(attr_list->attr_node->value,
                                              _HDF52Ncl_type(attr_list->attr_node->type_name),
                                              new_attr_list->attr_node->value, _HDF52Ncl_type(new_attr_list->attr_node->type_name));
                   }

                   new_attr_list->next = dataset_node->attr_list;
                   dataset_node->attr_list = new_attr_list;
                   ++dataset_node->num_attrs;

                   break;
               }
               attr_list = attr_list->next;
            }
        }

        dataset_list = dataset_list->next;
    }

    group_list = HDF5group->group_list;
    while(group_list)
    {
        _HDF5Build_dim_list(dim_list, n_dims, group_list->group_node);

        group_list = group_list->next;
    }
}

static void _HDF5Create_dim_list(HDF5DimInqRecList **dim_list, int *n_dims, NclHDF5group_node_t *HDF5group)
{
    NclHDF5dataset_list_t *dataset_list;
    NclHDF5group_list_t   *group_list;
    HDF5DimInqRecList *cur_list = NULL;

    int  i, k;
    int  found_new = 0;
    long old_dim_size[4*MAX_HDF5_DIMS];
    long new_dim_size[4*MAX_HDF5_DIMS];

    char new_name[8];

  /*
   *fprintf(stderr, "\n\n\nhit _HDF5Create_dim_list. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims: %d\n", *n_dims);
   */

    cur_list = *dim_list;
    for(i = 0; i < *n_dims; i++)
    {
        old_dim_size[i] = cur_list->dim_inq->size;
        cur_list = cur_list->next;
    }

    dataset_list = HDF5group->dataset_list;

    while(dataset_list)
    {
        NclHDF5dataset_node_t *dataset_node = dataset_list->dataset_node;

        for(i = 0; i < dataset_node->ndims; i++)
        {
            new_dim_size[i] = (long) dataset_node->dims[i];

            found_new = 1;

            for(k = 0; k < *n_dims; k++)
            {
                if(old_dim_size[k] == new_dim_size[i])
                {
                    found_new = 0;
                    break;
                }
            }

            if(found_new)
            {
                sprintf(new_name, "DIM_%.3d", *n_dims);
                old_dim_size[*n_dims] = new_dim_size[i];
                new_name[7] = '\0';

                cur_list = NclCalloc(1, sizeof(HDF5DimInqRecList));
                if(!cur_list)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list, in file: %s, line: %d\n",
                            __FILE__, __LINE__);
                    return;
                }

                cur_list->dim_inq = NclCalloc(1, sizeof(HDF5DimInqRec));
                if(!cur_list->dim_inq)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list->dim_inq, in file: %s, line: %d\n",
                            __FILE__, __LINE__);
                    return;
                }

                cur_list->dim_inq->is_unlimited = 0;
                cur_list->dim_inq->is_dataset = 0;
                cur_list->dim_inq->name = NrmStringToQuark(new_name);
                cur_list->dim_inq->dataset_name = NrmStringToQuark(new_name);
                cur_list->dim_inq->description = NrmStringToQuark(new_name);

                cur_list->dim_inq->ncldim_id = *n_dims;
                cur_list->dim_inq->size = (nclH5size_t) new_dim_size[i];

              /*
               *fprintf(stderr, "\n_HDF5Create_dim_list. file: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tcur_list->dim_inq->size: %ld", (long) cur_list->dim_inq->size);
               *fprintf(stderr, "\tdescription: <%s>",
               *                 NrmQuarkToString(cur_list->dim_inq->description));
               *fprintf(stderr, "\tncldim_id: %d: name <%s>\n",
               *                 cur_list->dim_inq->ncldim_id,
               *                 NrmQuarkToString(cur_list->dim_inq->name));
               */

                cur_list->next = *dim_list;
                *dim_list = cur_list;

                (*n_dims)++;
            }
        }

        dataset_list = dataset_list->next;
    }

    group_list = HDF5group->group_list;
    while(group_list)
    {
        _HDF5Create_dim_list(dim_list, n_dims, group_list->group_node);

        group_list = group_list->next;
    }
}

static void *HDF5ReadAtt
#if     NhlNeedProto
(void *therec, NclQuark theatt, void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5AttInqRecList * the_att_list;

    the_att_list = thefile->att_list;
    while(the_att_list != NULL)
    {
        if(theatt == the_att_list->att_inq->name)
        {
            memcpy(storage, the_att_list->att_inq->value,
                   _NclSizeOf( the_att_list->att_inq->type)* the_att_list->att_inq->n_elem);
            return(storage);
        }
        the_att_list = the_att_list->next;
    }
    return(NULL);
}

static void *HDF5ReadVarAtt_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thevar, NclQuark theatt, void *storage)
#else
(grp_inq, thevar, theatt, storage)
HDF5GrpInqRec *grp_inq;
NclQuark thevar;
NclQuark theatt;
void *storage;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList * varlist;
    HDF5AttInqRecList * the_att_list;
    void *output = NULL;
    int i, j;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5ReadVarAtt_inGroup. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\ttheatt: <%s>\n", NrmQuarkToString(theatt));
   */

    varlist = grp_inq->var_list;
    for(i = 0; i < grp_inq->n_vars; i++)
    {
        if((thevar == varlist->var_inq->full_name) ||
           (thevar == varlist->var_inq->hdf5_name) ||
           (thevar == varlist->var_inq->name))
        {
            the_att_list = varlist->var_inq->att_list;
            for(j = 0; j < varlist->var_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    memcpy(storage, the_att_list->att_inq->value,
                           _NclSizeOf( the_att_list->att_inq->type) * the_att_list->att_inq->n_elem);
                    return(storage);
                }
                the_att_list = the_att_list->next;
            }
        }
        varlist = varlist->next;
    }

    grplist = grp_inq->grp_list;
    for (i = 0; i < grp_inq->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thevar) ||
           (grplist->grp_inq->hdf5_name == thevar) ||
           (grplist->grp_inq->name == thevar))
        {
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    memcpy(storage, the_att_list->att_inq->value,
                           _NclSizeOf( the_att_list->att_inq->type) * the_att_list->att_inq->n_elem);
                    return(storage);
                }
                the_att_list = the_att_list->next;
            }
        }

        output = HDF5ReadVarAtt_inGroup(grplist->grp_inq, thevar, theatt, storage);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static void *HDF5ReadVarAtt
#if     NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt, void *storage)
#else
(therec, thevar, theatt, storage)
void *therec;
NclQuark thevar;
NclQuark theatt;
void *storage;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList * varlist;
    HDF5AttInqRecList * the_att_list;
    void *output = NULL;
    int i, j;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5ReadVarAtt. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\ttheatt: <%s>\n", NrmQuarkToString(theatt));
   */

    varlist = thefile->var_list;
    for(i = 0; i < thefile->n_vars; i++)
    {
        if((thevar == varlist->var_inq->full_name) ||
           (thevar == varlist->var_inq->hdf5_name) ||
           (thevar == varlist->var_inq->name))
        {
            the_att_list = varlist->var_inq->att_list;
            for(j = 0; j < varlist->var_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    memcpy(storage, the_att_list->att_inq->value,
                           _NclSizeOf( the_att_list->att_inq->type) * the_att_list->att_inq->n_elem);
                    return(storage);
                }
                the_att_list = the_att_list->next;
            }
        }
        varlist = varlist->next;
    }

    grplist = thefile->grp_list;
    for (i = 0; i < thefile->n_grps; i++)
    {
        if((grplist->grp_inq->full_name == thevar) ||
           (grplist->grp_inq->hdf5_name == thevar) ||
           (grplist->grp_inq->name == thevar))
        {
            the_att_list = grplist->grp_inq->att_list;
            for(j = 0; j < grplist->grp_inq->n_atts; j++)
            {
                if(theatt == the_att_list->att_inq->name)
                {
                    memcpy(storage, the_att_list->att_inq->value,
                           _NclSizeOf( the_att_list->att_inq->type) * the_att_list->att_inq->n_elem);
                    return(storage);
                }
                the_att_list = the_att_list->next;
            }
        }

        output = HDF5ReadVarAtt_inGroup(grplist->grp_inq, thevar, theatt, storage);

        if(output)
        {
            return(output);
        }

        grplist = grplist->next;
    }

    return(NULL);
}

static void _printHDF5dim_list
#if    NhlNeedProto
(HDF5DimInqRecList *dim_list, int n_dims)
#else
(dim_list, n_dims)
HDF5DimInqRecList *dim_list;
int n_dims;
#endif
{
    HDF5DimInqRecList *cur_list;
    int n = 0;
    cur_list = dim_list;
    for(n = 0; n < n_dims; n++)
    {
        fprintf(stderr, "Dim No. %d, ", n);
        fprintf(stderr, "size: %ld,",
                         (long) cur_list->dim_inq->size);
        fprintf(stderr, "id: %ld, ",
                         (long) cur_list->dim_inq->ncldim_id);
        fprintf(stderr, " name: <%s>, is_unlimited: %d",
                         NrmQuarkToString(cur_list->dim_inq->name),
                         cur_list->dim_inq->is_unlimited);

        if(cur_list->dim_inq->is_dataset)
            fprintf(stderr, ", is_dataset: %d, full_name: <%s>\n",
                             cur_list->dim_inq->is_dataset,
                         NrmQuarkToString(cur_list->dim_inq->dataset_name));
        else
            fprintf(stderr, "\n");
        cur_list = cur_list->next;
    }
}

static void *HDF5OpenFile
#if    NhlNeedProto
(void *rec, NclQuark path, int wr_status)
#else
(rec, path, wr_status)
void *rec;
NclQuark path;
int wr_status;
#endif
{
    HDF5FileRecord *the_file = (HDF5FileRecord *) rec;
    NclHDF5group_node_t *h5_group = NULL;
    NclHDF5group_node_t *dim_group = NULL;

    HDF5DimInqRecList *dim_list = NULL;
    HDF5GrpInqRec *grp_inq = NULL;

    HDF5DimInqRecList *pre_list = NULL;
    HDF5DimInqRecList *cur_list = NULL;
    int n = 0;

    char *filename = NULL;
    int n_dims = 0;

    herr_t status;

    if(the_file == NULL)
    {
        return(NULL);
    }

    /*printf("opening file as HDF5\n");*/
    the_file->file_path_q = path;
    the_file->wr_status = wr_status;

    the_file->n_grps = 0;
    the_file->n_vars = 0;
    the_file->n_dims = 0;
    the_file->n_atts = 0;

    the_file->grp_list = NULL;
    the_file->var_list = NULL;
    the_file->dim_list = NULL;
    the_file->att_list = NULL;

    if(wr_status <= 0)
    {
        if(the_file->open)
        {
            return the_file;
        }
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"NclHDF5: Working on the write part\n"));
        return(NULL);
    }

    the_file->h5_group = NULL;

    filename = strdup(NrmQuarkToString(path));

    status = _NclHDF5check_obj(filename, &h5_group);
    free(filename);

    if(status == FAILED)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDF5: Unable to open file: <%s>, at line: %d\n", filename, __FILE__, __LINE__);
        return(NULL);
    }

  /*
   *_NclHDF5print_group(h5_group);
   */

    _HDF5Build_dim_list(&dim_list, &n_dims, h5_group);

  /*
   *fprintf(stderr, "\n\tin file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims = %d\n", n_dims);
   */

    if(n_dims < 1)
    {
        dim_group = _find_HDF5Group("/Dimensions", h5_group);

        if(dim_group)
        {
          /*
           *_NclHDF5print_group(dim_group);
           */

            n_dims = _HDF5Build_dim_list_from_dim_group(&dim_list, dim_group);
        }
    }

  /*
   *fprintf(stderr, "\n\tin file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims = %d\n", n_dims);
   */

    if(n_dims < 1)
    {
        _HDF5Create_dim_list(&dim_list, &n_dims, h5_group);

      /*
       *fprintf(stderr, "\n\tin file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tn_dims = %d\n", n_dims);
       *fprintf(stderr, "\tthe_file->n_dims = %d\n", the_file->n_dims);
       */

        the_file->n_dims = n_dims;
        for(n = 0; n < n_dims; n++)
        {
            cur_list = NclCalloc(1, sizeof(HDF5DimInqRecList));
            if(!cur_list)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list, in file: %s, line: %d\n",
                        __FILE__, __LINE__);
                return NULL;
            }
    
            cur_list->dim_inq = NclCalloc(1, sizeof(HDF5DimInqRec));
            if(!cur_list->dim_inq)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN, "UNABLE TO ALLOCATE MEMORY for cur_list->dim_inq, in file: %s, line: %d\n",
                        __FILE__, __LINE__);
                return NULL;
            }
    
            pre_list = dim_list;
            cur_list->dim_inq->is_unlimited = pre_list->dim_inq->is_unlimited;
            cur_list->dim_inq->is_dataset = pre_list->dim_inq->is_dataset;
            cur_list->dim_inq->name = pre_list->dim_inq->name;
            cur_list->dim_inq->dataset_name = pre_list->dim_inq->dataset_name;
            cur_list->dim_inq->description = pre_list->dim_inq->description;

            cur_list->dim_inq->ncldim_id = pre_list->dim_inq->ncldim_id;
            cur_list->dim_inq->size = pre_list->dim_inq->size;

            cur_list->next = the_file->dim_list;
            the_file->dim_list = cur_list;

          /*
           *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tcur_list->dim_inq->size: %ld", (long) cur_list->dim_inq->size);
           *fprintf(stderr, "\tdescription: <%s>",
           *                 NrmQuarkToString(cur_list->dim_inq->description));
           *fprintf(stderr, "\tncldim_id: %d: name <%s>\n",
           *                 cur_list->dim_inq->ncldim_id,
           *                 NrmQuarkToString(cur_list->dim_inq->name));
           */
 
            pre_list = pre_list->next;
            dim_list->next = NULL;
            free(dim_list->dim_inq);
            free(dim_list);
            dim_list = pre_list;
        }
    }
    else
    {
        the_file->n_dims = n_dims;
        the_file->dim_list = dim_list;
    }

  /*
   *fprintf(stderr, "\n\tin file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims = %d\n", n_dims);
   *_printHDF5dim_list(the_file->dim_list, n_dims);
   */

    grp_inq = _HDF5Build_grp_list(h5_group);

    the_file->h5_group = h5_group;

    the_file->n_grps = grp_inq->n_grps;
    the_file->grp_list = grp_inq->grp_list;

    the_file->n_vars = grp_inq->n_vars;
    the_file->var_list = grp_inq->var_list;

    the_file->n_atts = grp_inq->n_atts;
    the_file->att_list = grp_inq->att_list;

    return(the_file);
}

static void _HDF5free_dim_list
#if     NhlNeedProto
(HDF5DimInqRecList *dim_list, int n_dims)
#else
(dim_list, n_dims)
HDF5DimInqRecList *dim_list;
int n_dims;
#endif
{
    HDF5DimInqRecList *cur_list;
    int j;

    for(j = 0; j < n_dims; j++)
    {
        cur_list = dim_list;
        dim_list = dim_list->next;
        cur_list->next = NULL;

        free(cur_list->dim_inq);
        free(cur_list);
    }
}

static void _HDF5free_att_list
#if     NhlNeedProto
(HDF5AttInqRecList *att_list, int n_atts)
#else
(att_list, n_atts)
HDF5AttInqRecList *att_list;
int n_atts;
#endif
{
    HDF5AttInqRecList *cur_list;
    int j;

    for(j = 0; j < n_atts; j++)
    {
        cur_list = att_list;
        att_list = att_list->next;
        cur_list->next = NULL;

      /*
       *if(cur_list->att_inq->value)
       *    free(cur_list->att_inq->value);
       */
        free(cur_list->att_inq);
        free(cur_list);
    }
}

static void _HDF5free_var_list
#if     NhlNeedProto
(HDF5VarInqRecList *var_list, int n_vars)
#else
(var_list, n_vars)
HDF5VarInqRecList *var_list;
int n_vars;
#endif
{
    HDF5VarInqRecList *cur_list;
    int i;

    for(i = 0; i < n_vars; i++)
    {
        cur_list = var_list;
        var_list = var_list->next;
        cur_list->next = NULL;

        _HDF5free_att_list(cur_list->var_inq->att_list, cur_list->var_inq->n_atts);

        if(cur_list->var_inq->compound)
        {
            if(cur_list->var_inq->compound->member)
            {
                free(cur_list->var_inq->compound->member);
            }
            free(cur_list->var_inq->compound);
        }
        if(cur_list->var_inq->value)
            free(cur_list->var_inq->value);
        free(cur_list->var_inq);
        free(cur_list);
    }
}

static void _HDF5free_grp_list
#if     NhlNeedProto
(HDF5GrpInqRecList *grp_list, int n_grps)
#else
(grp_list, n_grps)
HDF5GrpInqRecList *grp_list;
int n_grps;
#endif
{
    HDF5GrpInqRecList *cur_list;
    int i;

    for(i = 0; i < n_grps; i++)
    {
        cur_list = grp_list;
        grp_list = grp_list->next;
        cur_list->next = NULL;

        _HDF5free_att_list(cur_list->grp_inq->att_list, cur_list->grp_inq->n_atts);
        _HDF5free_var_list(cur_list->grp_inq->var_list, cur_list->grp_inq->n_vars);
        _HDF5free_grp_list(cur_list->grp_inq->grp_list, cur_list->grp_inq->n_grps);

        free(cur_list->grp_inq);
        free(cur_list);
    }
}


static void HDF5FreeRec
#if    NhlNeedProto
(void *rec)
#else
(rec)
void *rec;
#endif
{
    HDF5FileRecord *the_file = (HDF5FileRecord *) rec;
    _NclHDF5free_group(the_file->h5_group);
    _HDF5free_dim_list(the_file->dim_list, the_file->n_dims);
    _HDF5free_att_list(the_file->att_list, the_file->n_atts);
    _HDF5free_var_list(the_file->var_list, the_file->n_vars);
    _HDF5free_grp_list(the_file->grp_list, the_file->n_grps);
    if(the_file->options)
        free(the_file->options);
    free(the_file);
}

static void HDF5addName
#if     NhlNeedProto
(NclQuark *names, int *nv, NclQuark name_quark, int *mv)
#else
(name, nv, name_quark, mv)
NclQuark *names;
int *nv;
NclQuark name_quark;
int mv;
#endif
{
    if(*nv >= *mv)
    {
        *mv *= 2;
        names = (NclQuark *) NclRealloc(names, sizeof(NclQuark) * (*mv));
    }
    names[*nv] = name_quark;
    (*nv) ++;
}

static NclQuark *HDF5GetVarNames_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, int *num_vars)
#else
(grp_inq, num_vars)
HDF5GrpInqRec *grp_inq;
int *num_vars;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    NclQuark *names;
    int n = 0;
    int m = 0;
    int nv = 0;
    int mv = 1024;

    NclQuark *grp_names;
    int       grp_nvars;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetVarNames_inGroup. file: %s, line: %d\n", __FILE__, __LINE__);
   */

    names = NclMalloc(sizeof(NclQuark)*mv);

    varlist = grp_inq->var_list;
    for(n = 0; n < grp_inq->n_vars; n++)
    {
        HDF5addName(names, &nv, varlist->var_inq->full_name, &mv);
      /*
       *fprintf(stderr, "\tnames[%d]: <%s>\n", nv-1, NrmQuarkToString(names[nv-1]));
       *HDF5addName(names, &nv, varlist->var_inq->name, &mv);
       */
        varlist = varlist->next;
    }

    grplist = grp_inq->grp_list;
    for(n = 0; n < grp_inq->n_grps; n++)
    {
        HDF5addName(names, &nv, grplist->grp_inq->full_name, &mv);
      /*
       *fprintf(stderr, "\tnames[%d]: <%s>\n", nv-1, NrmQuarkToString(names[nv-1]));
       */

        grp_names = HDF5GetVarNames_inGroup(grplist->grp_inq, &grp_nvars);
        
        for(m = 0; m < grp_nvars; m++)
        {
            HDF5addName(names, &nv, grp_names[m], &mv);
          /*
           *fprintf(stderr, "\tnames[%d]: <%s>\n", nv-1, NrmQuarkToString(names[nv-1]));
           */
        }

        free(grp_names);
        grplist = grplist->next;
    }

    if(nv)
    {
        names = (NclQuark *) NclRealloc(names, sizeof(NclQuark) * nv);
    }
    *num_vars = nv;
    return(names);
}

static NclQuark *HDF5GetVarNames
#if     NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *varlist;
    NclQuark *names;
    int n = 0;
    int nv = 0;
    int m = 0;
    int mv = 1024;

    NclQuark *grp_names;
    int       grp_nvars;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetVarNames. file: %s, line: %d\n", __FILE__, __LINE__);
   */

    names = NclMalloc(sizeof(NclQuark)*mv);

    varlist = thefile->var_list;
    for(n = 0; n < thefile->n_vars; n++)
    {
        HDF5addName(names, &nv, varlist->var_inq->full_name, &mv);
      /*
       *fprintf(stderr, "\tnames[%d]: <%s>\n", nv-1, NrmQuarkToString(names[nv-1]));
       *HDF5addName(names, &nv, varlist->var_inq->name, &mv);
       */
        varlist = varlist->next;
    }

    grplist = thefile->grp_list;
    for(n = 0; n < thefile->n_grps; n++)
    {
        HDF5addName(names, &nv, grplist->grp_inq->full_name, &mv);
      /*
       *fprintf(stderr, "\tnames[%d]: <%s>\n", nv-1, NrmQuarkToString(names[nv-1]));
       */

        grp_names = HDF5GetVarNames_inGroup(grplist->grp_inq, &grp_nvars);

        for(m = 0; m < grp_nvars; m++)
        {
            HDF5addName(names, &nv, grp_names[m], &mv);
          /*
           *fprintf(stderr, "\tnames[%d]: <%s>\n", nv-1, NrmQuarkToString(names[nv-1]));
           */
        }

        free(grp_names);
        grplist = grplist->next;
    }

  /*We do not need to reallocate memory here, as the memory used will be release after the function call.
   *Wei, 09/07/2012.
   *names = (NclQuark *) NclRealloc(names, sizeof(NclQuark) * nv);
   */
    *num_vars = nv;
    return(names);
}

static NclQuark *HDF5GetGrpNames_inGroup
#if     NhlNeedProto
(HDF5GrpInqRec *grp_inq, int *num_grps)
#else
(grp_inq, num_grps)
HDF5GrpInqRec *grp_inq;
int *num_grps;
#endif
{
    HDF5GrpInqRecList *grplist;
    NclQuark *names;
    int n = 0;
    int m = 0;
    int nv = 0;
    int mv = 1024;

    NclQuark *grp_names;
    int       grp_ngrps = 0;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetGrpNames_inGroup. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrp_inq->n_grps: %d\n", grp_inq->n_grps);
   */

    names = NclMalloc(sizeof(NclQuark)*mv);

    grplist = grp_inq->grp_list;
    for(n = 0; n < grp_inq->n_grps; n++)
    {
        HDF5addName(names, &nv, grplist->grp_inq->full_name, &mv);
      /*
       *fprintf(stderr, "\tgroup names[%d]: <%s>\n", nv, NrmQuarkToString(names[nv]));
       */

        grp_names = HDF5GetGrpNames_inGroup(grplist->grp_inq, &grp_ngrps);
    
        for(m = 0; m < grp_ngrps; m++)
        {
            HDF5addName(names, &nv, grp_names[m], &mv);
          /*
           *fprintf(stderr, "\tgroup names[%d]: <%s>\n", nv, NrmQuarkToString(names[nv]));
           */
        }

        if(grp_names)
            NclFree(grp_names);

        grplist = grplist->next;
    }

    if(nv < 1)
    {
        NclFree(names);
        names = NULL;
    }
    else
    {
        names = (NclQuark *) NclRealloc(names, sizeof(NclQuark) * nv);
    }
    *num_grps = nv;
    return(names);
}

static NclQuark *HDF5GetGrpNames
#if     NhlNeedProto
(void* therec, int *num_grps)
#else
(therec, num_grps)
void* therec;
int *num_grps;
#endif
{
    HDF5FileRecord *thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    NclQuark *names;
    int n = 0;
    int nv = 0;
    int m = 0;
    int mv = 1024;

    NclQuark *grp_names;
    int       grp_ngrps = 0;

  /*
   *fprintf(stderr, "\n\n\nhit HDF5GetGrpNames. file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthefile->n_grps: %d\n", thefile->n_grps);
   */

    names = NclMalloc(sizeof(NclQuark)*mv);

    grplist = thefile->grp_list;
    for(n = 0; n < thefile->n_grps; n++)
    {
        HDF5addName(names, &nv, grplist->grp_inq->full_name, &mv);

      /*
       *fprintf(stderr, "\tgroup names[%d]: <%s>\n", nv, NrmQuarkToString(names[nv]));
       */

        grp_names = HDF5GetGrpNames_inGroup(grplist->grp_inq, &grp_ngrps);

        for(m = 0; m < grp_ngrps; m++)
        {
            HDF5addName(names, &nv, grp_names[m], &mv);
          /*
           *fprintf(stderr, "\tgroup names[%d]: <%s>\n", nv, NrmQuarkToString(names[nv]));
           */
        }

        if(grp_names)
            NclFree(grp_names);

        grplist = grplist->next;
    }

    if(nv < 1)
    {
        NclFree(names);
        names = NULL;
    }
    else
    {
        names = (NclQuark *) NclRealloc(names, sizeof(NclQuark) * nv);
    }
    *num_grps = nv;
    return(names);
}

static int HDF5ReadVar_inGroup
#if    NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thevar, long* start, long* finish,
    long* stride, void* storage, NclQuark file_path_q,
    NclHDF5group_node_t *h5_group)
#else
(grp_inq, thevar, start, finish, stride, storage, file_path_q, h5_group)
HDF5GrpInqRec *grp_inq;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
NclQuark file_path_q;
NclHDF5group_node_t *h5_group;
#endif
{
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *thelist;
    int i, j, n;
  /*
   *float tmpf;
   *hssize_t starti[NCL_MAX_DIMENSIONS];
   *hsize_t edgei[NCL_MAX_DIMENSIONS];
   */
    hsize_t stridei[NCL_MAX_DIMENSIONS];
    int found = 0;
    int no_stride = 1;
    NclQuark chkvar = thevar;

    char *dot_ptr;
    char var_str[1024];
    char component[1024];
    int  is_compound = 0;

    strcpy(var_str, NrmQuarkToString(thevar));
    dot_ptr = strchr(var_str, '.');
    if(dot_ptr && (NULL == strchr(dot_ptr, '/')))
    {
        is_compound = 1;
        strcpy(component, dot_ptr);
        dot_ptr[0] = '\0';
        chkvar = NrmStringToQuark(var_str);
    }

  /*
   *fprintf(stderr, "\n\n\nhit HDF5ReadVar_inGroup, file: <%s>, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfilename = <%s>\n", NrmQuarkToString(file_path_q));
   *fprintf(stderr, "\tthegrp = <%s>\n", NrmQuarkToString(grp_inq->full_name));
   *fprintf(stderr, "\tthevar = <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\tchkvar = <%s>\n", NrmQuarkToString(chkvar));
   */

    thelist = grp_inq->var_list;
    for(i = 0; i < grp_inq->n_vars; i++)
    {
      /*
       *fprintf(stderr, "\ti = %d, thelist->var_inq->name = <%s>\n",
       *        i, NrmQuarkToString(thelist->var_inq->name));
       */
        if((chkvar == thelist->var_inq->full_name) ||
           (chkvar == thelist->var_inq->hdf5_name) ||
           (chkvar == thelist->var_inq->name))
        {
            NclHDF5data_t *NclHDF5data = NULL;
            hid_t fid;
            char filename[256];
            char dataset_name[256];
            strcpy(filename, NrmQuarkToString(file_path_q));
            strcpy(dataset_name, NrmQuarkToString(thelist->var_inq->full_name));
            if(is_compound)
            {
                strcat(dataset_name, component);
            }

          /*
           *fprintf(stderr, "\n\n\nhit HDF5ReadVar_inGroup, file: <%s>, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tfound thelist->var_inq->full_name = <%s>\n",
           *       NrmQuarkToString(thelist->var_inq->full_name));
           *fprintf(stderr, "\ttype = 0%o\n", thelist->var_inq->type);
           *fprintf(stderr, "\tdataset_name: <%s>\n", dataset_name);
           */

            fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);

            if(fid < 0)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                         "NclHDF5: Unable to open file: <%s>, in file: <%s>, at line: %d\n",
                          filename, __FILE__, __LINE__);
                H5close();
                return (0);
            }

            NclHDF5data = _NclHDF5get_data_with_name(fid, dataset_name, h5_group, start, finish, stride);

            for(j = 0; j < thelist->var_inq->n_dims; j++)
            {
                stridei[j] = (hsize_t)stride[j];
              /*
               *tmpf = stridei[j];
               *starti[j] = (hsize_t)start[j] ;
               *edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
               *fprintf(stderr, "\n\n\tin file: <%s>, at line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tstarti[%d] = %ld, stridei[%d] = %ld, edgei[%d] = %ld\n",
               *        j, (long) starti[j], j, (long) stridei[j], j, (long) edgei[j]);
               *fprintf(stderr, "\tstart[%d] = %ld, stride[%d] = %ld, start[%d] = %ld, finish[%d] = %ld\n",
               *        j, (long) start[j], j, (long) stride[j], j, (long) start[j], j, (long) finish[j]);
               */

                if(stridei[j] != 1)
                    no_stride = 0;
            }

            H5close();

            if(NclHDF5data)
            {
              /*
               *fprintf(stderr, "\nfile: <%s>, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tNclHDF5data->nbytes = %d\n", NclHDF5data->nbytes);
               *fprintf(stderr, "\tNclHDF5data->type: <%s>\n", NclHDF5data->type);
               */

                if(no_stride)
                {
                    if(0 == strcmp("string", NclHDF5data->type))
                    {
                        NclQuark *qp;
                        char **tmp_char_array;

                        NclHDF5data->nbytes = 1;
                        if(NclHDF5data->ndims > 0)
                        {
                            for(i=0; i<NclHDF5data->ndims; i++)
                            {
                                NclHDF5data->nbytes *= NclHDF5data->dims[i];
                            }
                        }

                        qp = (NclQuark *) NclCalloc(NclHDF5data->nbytes, sizeof(NclQuark));
                        assert(qp);

                        tmp_char_array = (char **) NclHDF5data->value;
                        for(i=0; i<NclHDF5data->nbytes; i++)
                        {
                            qp[i] = NrmStringToQuark(tmp_char_array[i]);
                        }

                        NclHDF5data->nbytes *= sizeof(NclQuark);
                        memcpy(storage, qp, NclHDF5data->nbytes);
                        free(qp);
                    }
                    else
                    {
                        memcpy(storage, NclHDF5data->value, NclHDF5data->nbytes);
                    }
                }
                else
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can not handle stride yet in HDF5ReadVar_inGroup"));
                    return(0);
                }
              /*
               *fprintf(stderr, "\tfile: <%s>, line: %d\n", __FILE__, __LINE__);
               */
                _NclHDF5free_data(NclHDF5data);
              /*
               *fprintf(stderr, "\n\n\nEND HDF5ReadVar_inGroup, file: <%s>, line: %d\n", __FILE__, __LINE__);
               */
                return(1);
            }

            NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Error in reading var from a group"));
            return(0);
        }
        thelist= thelist->next;
    }

    grplist = grp_inq->grp_list;
    for(n = 0; n < grp_inq->n_grps; n++)
    {
        if((chkvar == grplist->grp_inq->full_name) ||
           (chkvar == grplist->grp_inq->hdf5_name) ||
           (chkvar == grplist->grp_inq->name))
        {
#if 0
            memcpy(storage, &chkvar, sizeof(NclQuark));
            return 1;
        }

        found = HDF5ReadVar_inGroup(grplist->grp_inq, thevar, start, finish, stride,
                                    storage, file_path_q, h5_group);

        if(found)
        {
            return (found);
        }
#else
            found = HDF5ReadVar_inGroup(grplist->grp_inq, thevar, start, finish, stride,
                                        storage, file_path_q, h5_group);

            if(found)
            {
                return (found);
            }
        }
#endif

        grplist = grplist->next;
    }
  /*
   *fprintf(stderr, "\n\n\nEND HDF5ReadVar_inGroup, file: <%s>, line: %d\n", __FILE__, __LINE__);
   */

    return 0;
}

static void *HDF5ReadVar
#if    NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish, long* stride, void* storage)
#else
(therec, thevar, start, finish, stride, storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5GrpInqRecList *grplist;
    HDF5VarInqRecList *thelist;
    int i, j, n;
    hssize_t starti[NCL_MAX_DIMENSIONS];
    hsize_t stridei[NCL_MAX_DIMENSIONS];
    hsize_t edgei[NCL_MAX_DIMENSIONS];
    float tmpf;
    int found = 0;
    int no_stride = 1;

    NclQuark chkvar = thevar;

    char *dot_ptr;
    char var_str[1024];
    char component[1024];
    int  is_compound = 0;

    strcpy(var_str, NrmQuarkToString(thevar));
    dot_ptr = strchr(var_str, '.');
    if(dot_ptr && (NULL == strchr(dot_ptr, '/')))
    {
        is_compound = 1;
        strcpy(component, dot_ptr);
        dot_ptr[0] = '\0';
        chkvar = NrmStringToQuark(var_str);
    }

  /*
   *fprintf(stderr, "\nhit HDF5ReadVar, file: <%s>, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tfilename = <%s>\n", NrmQuarkToString(thefile->file_path_q));
   *fprintf(stderr, "\tthevar = <%s>\n", NrmQuarkToString(thevar));
   */

    thelist = thefile->var_list;
    for(i = 0; i < thefile->n_vars; i++)
    {
        if((chkvar == thelist->var_inq->full_name) ||
           (chkvar == thelist->var_inq->hdf5_name) ||
           (chkvar == thelist->var_inq->name))
        {
            NclHDF5data_t *NclHDF5data = NULL;
            hid_t fid;
            char filename[256];
            char dataset_name[256];
            strcpy(filename, NrmQuarkToString(thefile->file_path_q));
            strcpy(dataset_name, NrmQuarkToString(thelist->var_inq->full_name));
            if(is_compound)
            {
                strcat(dataset_name, component);
            }

            fid = H5Fopen(filename, H5F_ACC_RDONLY, H5P_DEFAULT);

            if(fid < 0)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                         "NclHDF5: Unable to open file: <%s>, in file: <%s>, at line: %d\n",
                          filename, __FILE__, __LINE__);
                H5close();
                return (NULL);
            }

            NclHDF5data = _NclHDF5get_data_with_name(fid, dataset_name, thefile->h5_group, start, finish, stride);
#if 0
            for(j = 0; j < thelist->var_inq->n_dims; j++)
            {
                starti[j] = (hsize_t)start[j] ;
                stridei[j] = (hsize_t)stride[j];
                tmpf = stridei[j];
                edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;

                if(stridei[j] != 1)
                    no_stride = 0;

              /*
               *fprintf(stderr, "\n\nin file: <%s>, at line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tstarti[%d] = %ld, stridei[%d] = %ld, edgei[%d] = %ld\n",
               *        j, (long) starti[j], j, (long) stridei[j], j, (long) edgei[j]);
               *fprintf(stderr, "\tstart[%d] = %ld, stride[%d] = %ld, start[%d] = %ld, finish[%d] = %ld\n",
               *        j, (long) start[j], j, (long) stride[j], j, (long) start[j], j, (long) finish[j]);
               */
            }
#endif

            H5close();

            if(NclHDF5data)
            {
              /*
               *fprintf(stderr, "\nfile: <%s>, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tNclHDF5data->nbytes = %d\n", NclHDF5data->nbytes);
               */

                if(no_stride)
                {
		    if (NclHDF5data->is_str == 1 || NclHDF5data->is_str == 2)  /* fixed length string array */
                    {
			char *cp;
                        NclQuark *qp = NclCalloc(NclHDF5data->nbytes, sizeof(NclQuark));
                        if(!qp)
                        {
                            NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Failed to allocated memory for curAttrList. in file: %s, line: %d\n",
                                    __FILE__, __LINE__));
                            return 0;
                        }
			cp = (char *)NclHDF5data->value;
                        for(j = 0; j < NclHDF5data->nbytes; j++) {
				int len = strlen(cp);
				if (NclHDF5data->is_str == 2) {
					char *rcp = cp + len - 1;
					while (*rcp == ' ')
						*(rcp--) = '\0';
				}
				qp[j] = NrmStringToQuark(cp);
				cp += len + 1;
			}
                        memcpy(storage, qp, NclHDF5data->nbytes*sizeof(NclQuark));
                        free(qp);
                    }
		    else if (NclHDF5data->is_str == 3) { /* variable length string array */
			char **cpp;
                        NclQuark *qp = NclCalloc(NclHDF5data->nbytes, sizeof(NclQuark));
                        if(!qp)
                        {
                            NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Failed to allocated memory for curAttrList. in file: %s, line: %d\n",
                                    __FILE__, __LINE__));
                            return 0;
                        }
			cpp = (char **)NclHDF5data->value;
                        for(j = 0; j < NclHDF5data->nbytes; j++) {
                            qp[j] = NrmStringToQuark(cpp[j]);
			}
                        memcpy(storage, qp, NclHDF5data->nbytes*sizeof(NclQuark));
                        free(qp);
                    }
                    else
                    {
                        memcpy(storage, NclHDF5data->value, NclHDF5data->nbytes);
                    }
                }
                else
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Can not handle stride yet in HDF5ReadVar_inGroup"));
                    return(NULL);
                }
                _NclHDF5free_data(NclHDF5data);
                return(storage);
            }

            NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDF5: Error ocurred while reading can't continue");
            return(NULL);
        }
        thelist= thelist->next;
    }

    grplist = thefile->grp_list;
    for(n = 0; n < thefile->n_grps; n++)
    {
        if((chkvar == grplist->grp_inq->full_name) ||
           (chkvar == grplist->grp_inq->hdf5_name) ||
           (chkvar == grplist->grp_inq->name))
        {
            memcpy(storage, &chkvar, sizeof(NclQuark));
            return storage;
        }

        found = HDF5ReadVar_inGroup(grplist->grp_inq, thevar, start, finish, stride,
                                      storage, thefile->file_path_q, thefile->h5_group);

        if(found)
            return (storage);

        grplist = grplist->next;
    }

    return(NULL);
}

static void *HDF5ReadCoord
#if     NhlNeedProto
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
  /*
   *fprintf(stderr, "READ COORD HERE. file: <%s>, line: %d\n", __FILE__, __LINE__);
   */
    return(HDF5ReadVar(therec,thevar,start,finish,stride,storage));
}

static NclFVarRec *HDF5GetCoordInfo
#if     NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
    fprintf(stderr, "NEEDS TO FINISH HDF5GetCoordInfo. file: <%s>, line: %d\n", __FILE__, __LINE__);
    return NULL;
}

static NhlErrorTypes HDF5AddCoordVar
#if    NhlNeedProto
(void *therec, NclQuark thevar,NclBasicDataTypes data_type)
#else
(therec,thevar,data_type)
void *therec;
NclQuark thevar;
NclBasicDataTypes data_type;
#endif
{


    fprintf(stderr, "NEEDS TO FIGURE OUT ADD COORD VAR. file: <%s>, line: %d\n", __FILE__, __LINE__);
    return NhlNOERROR;
}

static void *HDF5CreateFile
#if    NhlNeedProto
(void *rec,NclQuark path)
#else
(rec,path)
void *rec;
NclQuark path;
#endif
{
    HDF5FileRecord *tmp = (HDF5FileRecord*)rec;
    int id = 0;
    int mode = H5P_DEFAULT;
    int format = 1;
    
    id = H5Fcreate(NrmQuarkToString(path), H5F_ACC_TRUNC, mode, H5P_DEFAULT);

      /*
       *fprintf(stderr, "HDF5CreateFile in file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "HDF5CreateFile create file: <%s>\n", NrmQuarkToString(path));
       *fprintf(stderr, "HDF5CreateFile file id: <%d>\n", id);
       */

    if(id > 0)
    {
        tmp->id = id;
        tmp->define_mode = 1;
        tmp->format = format;
        tmp->open = 1;
        tmp->has_scalar_dim = 0;

        tmp->h5_group = _NclHDF5allocate_group(id,
                                               NrmQuarkToString(path),
                                               "/", H5O_TYPE_GROUP);

        return(HDF5OpenFile(rec,path,-1));
    }
    else
    {
        return(NULL);
    }
}

static NhlErrorTypes HDF5WriteVar
#if    NhlNeedProto
(void *therec, NclQuark thevar, void *data, long *start, long *finish, long *stride)
#else
(therec, thevar, data, start, finish, stride)
void* therec;
NclQuark thevar;
void *data;
long *start;
long *finish;
long *stride;
#endif
{
    HDF5FileRecord * thefile = (HDF5FileRecord *) therec;
    HDF5VarInqRecList *thelist;
    int i, j;
    hid_t fid; 
    hsize_t starti[NCL_MAX_DIMENSIONS];
    hsize_t stridei[NCL_MAX_DIMENSIONS];
    hsize_t edgei[NCL_MAX_DIMENSIONS];
    float tmpf;
    int no_stride = 1;

    NclQuark chkvar = thevar;

    char *dot_ptr;
    char var_str[1024];
    char component[1024];
    int  is_compound = 0;
    int  ret_code = 0;

    strcpy(var_str, NrmQuarkToString(thevar));
    dot_ptr = strchr(var_str, '.');
    if(dot_ptr && (NULL == strchr(dot_ptr, '/')))
    {
        is_compound = 1;
        strcpy(component, dot_ptr);
        dot_ptr[0] = '\0';
        chkvar = NrmStringToQuark(var_str);
    }

    thelist = thefile->var_list;
    for(i = 0; i < thefile->n_vars; i++)
    {
        if((chkvar == thelist->var_inq->full_name) ||
           (chkvar == thelist->var_inq->hdf5_name) ||
           (chkvar == thelist->var_inq->name))
        {
            char *typename;
            char dataset_name[256];
            hsize_t *dims;
            hsize_t *chunk_dims;
            hsize_t rank;

            strcpy(dataset_name, NrmQuarkToString(thelist->var_inq->full_name));
            if(is_compound)
            {
                strcat(dataset_name, component);
            }

            if (thefile->open)
            {
                fid = thefile->id;
            }
            else
            {
              /*
               *fid = H5Fopen(NrmQuarkToString(thefile->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
               */
                fid = H5Fopen(NrmQuarkToString(thefile->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                if(fid < 1)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "HDF5: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(thefile->file_path_q));
                    return(NhlFATAL);
                }
                thefile->id = fid;
                thefile->define_mode = 0;
                thefile->open = 1;
            }

            rank = (hsize_t) thelist->var_inq->n_dims;
            dims = (hsize_t *) NclCalloc(rank, sizeof(hsize_t));
            if(dims == NULL)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "HDF5WriteVar: Could not allocate memory for dims. %s, %d", __FILE__, __LINE__));
                return(NhlFATAL);
            }
            chunk_dims = (hsize_t *) NclCalloc(rank, sizeof(hsize_t));
            if(chunk_dims == NULL)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "HDF5WriteVar: Could not allocate memory for chunk_dims. %s, %d", __FILE__, __LINE__));
                return(NhlFATAL);
            }

            for(j = 0; j < thelist->var_inq->n_dims; j++)
            {
                starti[j] = (hsize_t)start[j] ;
                stridei[j] = (hsize_t)stride[j];
                tmpf = stridei[j];
                edgei[j] =(hsize_t)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
                dims[j] = edgei[j];

                if(stridei[j] != 1)
                    no_stride = 0;
            }

            typename = (char *)_Ncl2H5type(thelist->var_inq->type);

            if(thelist->var_inq->n_chunk_dims)
            {
                for(j = 0; j < thelist->var_inq->n_chunk_dims; j++)
                {
                    chunk_dims[j] = (hsize_t)thelist->var_inq->chunk_dim[j];
                }

                ret_code = _write_chunkedH5dataset(fid, rank, dims, chunk_dims, data,
                                                   typename, dataset_name,
                                                   thefile->h5_group);
            }
            else
            {
                ret_code = _writeH5dataset(fid, rank, dims, data,
                                           typename, dataset_name,
                                           thefile->h5_group);
            }

            if(typename)
                free(typename);

            if(ret_code)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"HDF5WriteVar: Error ocurred while writing can't continue"));
                return(NhlFATAL);
            }
            else
            {
                return(NhlNOERROR);
            }
        }
        thelist= thelist->next;
    }

#if 0
    grplist = thefile->grp_list;
    for(n = 0; n < thefile->n_grps; n++)
    {
        if((chkvar == grplist->grp_inq->full_name) ||
           (chkvar == grplist->grp_inq->hdf5_name) ||
           (chkvar == grplist->grp_inq->name))
        {
            memcpy(storage, &chkvar, sizeof(NclQuark));
            return storage;
        }

        found = HDF5writeVar_inGroup(grplist->grp_inq, thevar, start, finish, stride,
                                      storage, thefile->file_path_q, thefile->h5_group);

        if(found)
            return (storage);

        grplist = grplist->next;
    }
#else
    fprintf(stderr, "group in HDF5WriteVar: file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "group in HDF5WriteVar: HAVE NOT DONE ANYTHING FOR GROUP YET!!!\n\n");
    fprintf(stderr, "group in HDF5WriteVar: file: %s, line: %d\n", __FILE__, __LINE__);
#endif

    return(NhlFATAL);
}

static NhlErrorTypes HDF5AddDim
#if    NhlNeedProto
(void* therec, NclQuark thedim, ng_size_t size,int is_unlimited)
#else
(therec, thedim, size,is_unlimited)
void* therec;
NclQuark thedim;
ng_size_t size;
int is_unlimited;
#endif
{
    HDF5FileRecord *rec = (HDF5FileRecord*) therec;
    hid_t fid;
    HDF5DimInqRecList *stepdl;
    int add_scalar = 0;

    if(rec->wr_status <=  0)
    {
        if(thedim == NrmStringToQuark("ncl_scalar"))
        {
            if (size != 1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "in HDF5AddDim: \"ncl_scalar\" is a reserved file dimension name in NCL, this name can only represent dimensions of size 1");
                return(NhlFATAL);
            }
            add_scalar = 1;
        }
        else
        {
            if (rec->open)
            {
                fid = rec->id;
            }
            else
            {
              /*
               *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
               */
                fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                if(fid < 0)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "HDF5: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    return(NhlFATAL);
                }
                rec->id = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
        }

        stepdl = rec->dim_list;

        if (add_scalar)
        {
            rec->has_scalar_dim = 1;
            rec->dim_list = (HDF5DimInqRecList*)NclMalloc(
                (unsigned) sizeof(HDF5DimInqRecList));
            rec->dim_list->dim_inq = (HDF5DimInqRec*)NclMalloc(
                (unsigned)sizeof(HDF5DimInqRec));
            rec->dim_list->next = stepdl;
            rec->dim_list->dim_inq->ncldim_id = -5;
            rec->dim_list->dim_inq->size = 1;
            rec->dim_list->dim_inq->is_unlimited = 0;
            rec->dim_list->dim_inq->name = NrmStringToQuark("ncl_scalar");
            rec->n_dims++;
        }
        else if(stepdl == NULL)
        {
            rec->dim_list = (HDF5DimInqRecList*)NclMalloc((unsigned)sizeof(HDF5DimInqRecList));
            rec->dim_list->dim_inq = (HDF5DimInqRec*)NclMalloc((unsigned)sizeof(HDF5DimInqRec));
            rec->dim_list->dim_inq->ncldim_id = 0;
            rec->dim_list->dim_inq->name = thedim;
            if(is_unlimited)
            {
                rec->dim_list->dim_inq->size = (long)0;
            }
            else
            {
                rec->dim_list->dim_inq->size = (long)size;
            }
            rec->dim_list->dim_inq->is_unlimited= is_unlimited;
            rec->dim_list->next = NULL;
            rec->n_dims = 1;
        }
        else
        {
            while(stepdl->next != NULL)
            {
                stepdl = stepdl->next;
            }
            stepdl->next = (HDF5DimInqRecList*)NclMalloc((unsigned)sizeof(HDF5DimInqRecList));
            stepdl->next->dim_inq = (HDF5DimInqRec*)NclMalloc((unsigned)sizeof(HDF5DimInqRec));
            stepdl->next->dim_inq->ncldim_id = rec->n_dims;
            stepdl->next->dim_inq->name = thedim;
            if(is_unlimited)
            {
                stepdl->next->dim_inq->size = (long)0;
            }
            else
            {
                stepdl->next->dim_inq->size = (long)size;
            }
            stepdl->next->dim_inq->is_unlimited= is_unlimited;
            stepdl->next->next = NULL;
            rec->n_dims++;
        }
        return(NhlNOERROR);
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

static NhlErrorTypes HDF5AddVar
#if    NhlNeedProto
(void* therec, NclQuark thevar, NclBasicDataTypes data_type, int n_dims,NclQuark *dim_names, long* dim_sizes)
#else
(therec,thevar,data_type,n_dims,dim_names,dim_sizes)
void* therec;
NclQuark thevar;
NclBasicDataTypes data_type;
int n_dims;
NclQuark *dim_names;
long* dim_sizes;
#endif
{
    HDF5FileRecord* rec = (HDF5FileRecord*)therec;
    HDF5VarInqRecList *stepvl = NULL;
    hid_t fid;
    int i;
    char *typename;
    int dim_ids[MAX_HDF5_DIMS];
    HDF5DimInqRecList* stepdl = NULL;
    int add_scalar_dim = 0;

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            fid = rec->id;
        }
        else
        {
          /*
           *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
           */
            fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
            if(fid < 0)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "HDF5AddVar: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->id = fid;
            rec->define_mode = 0;
            rec->open = 1;
        }

        typename = (char *)_Ncl2H5type(data_type);
/*
* All dimensions are correct dimensions for the file
*/
        dim_ids[0] = -999;
        for(i = 0; i < n_dims; i++)
        {
            stepdl = rec->dim_list;
            while(stepdl != NULL)
            {
                if(stepdl->dim_inq->name == dim_names[i])
                {
                    if((n_dims > 1)&&(dim_names[i] == NrmStringToQuark("ncl_scalar")))
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF5: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return(NhlFATAL);
                    }
                    dim_ids[i] = stepdl->dim_inq->size;
                    break;
                }
                else
                {
                    stepdl = stepdl->next;
                }
            }
        } 
        if (dim_ids[0] == -999)
        {
            if (n_dims == 1 && dim_sizes[0] == 1 && dim_names[0] == NrmStringToQuark("ncl_scalar"))
            {
                dim_ids[0] = -5;
                add_scalar_dim = 1;
            }
            else
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: internal error adding variable");
                return(NhlFATAL);
            }
        }

        if(typename != NULL)
        {
            rec->define_mode = 1;
            rec->n_vars++;
    
            _addH5dataset(n_dims, (hsize_t *)dim_ids,
                          typename, NrmQuarkToString(thevar),
                          rec->h5_group);

            stepvl = rec->var_list;
            if(stepvl == NULL)
            {
                rec->var_list = (HDF5VarInqRecList *)NclMalloc(
                                (unsigned)sizeof(HDF5VarInqRecList));
                rec->var_list->next = NULL;
                rec->var_list->var_inq = (HDF5VarInqRec *)NclCalloc(1,
                                         (unsigned)sizeof(HDF5VarInqRec));
                rec->var_list->var_inq->id = fid;
                rec->var_list->var_inq->name = thevar;
                rec->var_list->var_inq->full_name = thevar;
                rec->var_list->var_inq->hdf5_name = thevar;
                rec->var_list->var_inq->type = data_type;
                rec->var_list->var_inq->n_dims = n_dims;
                rec->var_list->var_inq->deflate_pass = 6;
                rec->var_list->var_inq->n_chunk_dims = 0;
                rec->var_list->var_inq->n_unlimit_dims = 0;
                rec->var_list->var_inq->has_dim_name = 0;
                rec->var_list->var_inq->n_atts = 0;
                rec->var_list->var_inq->att_list = NULL;
                rec->var_list->var_inq->compound = NULL;
                rec->var_list->var_inq->value = NULL;
                for(i = 0 ; i < n_dims; i++)
                {
                    rec->var_list->var_inq->dim[i] = dim_ids[i];
                    rec->var_list->var_inq->chunk_dim[i] = dim_ids[i];
                    rec->var_list->var_inq->unlimit_dim[i] = dim_ids[i];
                }
                if(dim_names != NULL)
                {
                    rec->var_list->var_inq->has_dim_name = n_dims;
                    for(i = 0 ; i < n_dims; i++)
                    {
                        rec->var_list->var_inq->dim_name[i] = dim_names[i];
                    }
                }
            }
            else
            {
                while(stepvl->next != NULL)
                {
                    stepvl= stepvl->next;
                }
                stepvl->next = (HDF5VarInqRecList *)NclMalloc(
                               (unsigned)sizeof(HDF5VarInqRecList));
                stepvl->next->var_inq = (HDF5VarInqRec*)NclCalloc(1,
                                        (unsigned)sizeof(HDF5VarInqRec));
                stepvl->next->next = NULL;
                stepvl->next->var_inq->id = fid;
                stepvl->next->var_inq->name = thevar;
                stepvl->next->var_inq->full_name = thevar;
                stepvl->next->var_inq->hdf5_name = thevar;
                stepvl->next->var_inq->type = data_type;
                stepvl->next->var_inq->n_dims = n_dims;
                stepvl->next->var_inq->has_dim_name = 0;
                stepvl->next->var_inq->n_atts = 0;
                stepvl->next->var_inq->att_list = NULL;
                stepvl->next->var_inq->compound = NULL;
                stepvl->next->var_inq->value = NULL;
                for(i = 0 ; i < n_dims; i++)
                {
                    stepvl->next->var_inq->dim[i] = dim_ids[i];
                }
                if(dim_names != NULL)
                {
                    stepvl->next->var_inq->has_dim_name = n_dims;
                    for(i = 0 ; i < n_dims; i++)
                    {
                        stepvl->next->var_inq->dim_name[i] = dim_names[i];
                    }
                }
            }
            if (add_scalar_dim)
            {
                rec->has_scalar_dim = 1;
                stepdl = rec->dim_list;
                rec->dim_list = (HDF5DimInqRecList *)NclMalloc(
                                (unsigned) sizeof(HDF5DimInqRecList));
                rec->dim_list->dim_inq = (HDF5DimInqRec *)NclMalloc(
                                         (unsigned)sizeof(HDF5DimInqRec));
                rec->dim_list->next = stepdl;
                rec->dim_list->dim_inq->ncldim_id = -5;
                rec->dim_list->dim_inq->size = 1;
                rec->dim_list->dim_inq->is_unlimited = 0;
                rec->dim_list->dim_inq->name = NrmStringToQuark("ncl_scalar");
                rec->n_dims++;
            }
            NclFree(typename);
            return(NhlNOERROR);
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }

    return(NhlFATAL);
}

static NhlErrorTypes HDF5AddVarChunk
#if    NhlNeedProto
(void* therec, NclQuark thevar, int n_dims, nclH5size_t *dims)
#else
(therec,thevar,n_dims,dims)
void* therec;
NclQuark thevar;
int n_dims;
ng_size_t *dims;
#endif
{
    HDF5FileRecord* rec = (HDF5FileRecord*)therec;
    HDF5VarInqRecList *stepvl = NULL;
    int i,ret = NhlNOERROR;
    hsize_t *chunk_dims;

    if(rec->wr_status <= 0)
    {
        chunk_dims = (hsize_t *) NclCalloc(n_dims, sizeof(hsize_t));
        if(chunk_dims == NULL)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                      "HDF5AddVarChunk: Could not allocate memory for chunk_dims. %s, %d", __FILE__, __LINE__));
            return(NhlFATAL);
        }
        for(i = 0 ; i < n_dims; i++)
        {
            chunk_dims[i] = (hsize_t)dims[i];
        }

        _addChunk2H5dataset(n_dims, chunk_dims,
                            NrmQuarkToString(thevar),
                            rec->h5_group);

        stepvl = rec->var_list;
        while(stepvl != NULL)
        {
            if(stepvl->var_inq->name == thevar)
            {
                if(n_dims != stepvl->var_inq->n_dims)
                {    
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                             "File (%s) was opened as a read only file, can not write to it",
                              NrmQuarkToString(rec->file_path_q));
                    ret = NhlFATAL;
                    break;
                }

                stepvl->var_inq->n_chunk_dims = n_dims;
                for(i = 0 ; i < n_dims; i++)
                {
                    stepvl->var_inq->chunk_dim[i] = dims[i];
                    if(dims[i] > stepvl->var_inq->dim[i])
                    {
                        if(stepvl->var_inq->dim[i])
                            stepvl->var_inq->chunk_dim[i] = stepvl->var_inq->dim[i];
                    }
                }
                ret = NhlNOERROR;
                break;
            }
            stepvl= stepvl->next;
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "File (%s) was opened as a read only file, can not write to it",
                  NrmQuarkToString(rec->file_path_q));
        ret = NhlFATAL;
    }

    return(ret);
}

static NhlErrorTypes HDF5WriteAtt
#if    NhlNeedProto
(void *therec, NclQuark theatt, void *data )
#else
(therec, theatt, data )
void *therec;
NclQuark theatt;
void *data;
#endif
{
    HDF5FileRecord* rec = (HDF5FileRecord *)therec;
    HDF5AttInqRecList *stepal;
    hid_t fid;
    hid_t ret = -1;
    char *buffer=NULL;

    if(rec->wr_status <= 0)
    {
        stepal = rec->att_list;
        while(stepal != NULL)
        {
            if(stepal->att_inq->name == theatt)
            {
                if (rec->open)
                {
                    fid = rec->id;
                }
                else
                {
                  /*
                   *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
                   */
                    fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                    if(fid < 0)
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                              "HDF5WriteAtt: Could not reopen the file (%s) for writing in file: %s, line: %d\n",
                              NrmQuarkToString(rec->file_path_q), __FILE__, __LINE__);
                        return(NhlFATAL);
                    }
                    rec->id = fid;
                    rec->define_mode = 0;
                    rec->open = 1;
                }
                if(stepal->att_inq->type == NCL_string && !(theatt == Qfill_val || theatt == Qmissing_val))
                {
                    buffer = NrmQuarkToString(*(NclQuark*)data);
                    hsize_t rank = 1;
                    hsize_t dims[1];
                    dims[0] = strlen(buffer);
                    ret = (hid_t) _add_attr2group(rec->id, rank, dims, (void*)buffer,
                          "string", NrmQuarkToString(theatt),
                          "/", rec->h5_group);
                    if (stepal->att_inq->value != NULL)
                        memcpy(stepal->att_inq->value,data,sizeof(NclQuark));
                }
                else
                {
                    hsize_t rank = 1;
                    hsize_t dims[1];
                    dims[0] = stepal->att_inq->n_elem;
                    buffer = (char *)_Ncl2H5type(stepal->att_inq->type);
                    ret = (hid_t) _add_attr2group(rec->id, rank, dims, (void*)data,
                          buffer, NrmQuarkToString(theatt),
                          "/", rec->h5_group);
                    if (stepal->att_inq->value != NULL)
                    {
                        memcpy(stepal->att_inq->value,data,
                                _NclSizeOf(stepal->att_inq->type)*stepal->att_inq->n_elem);
                    }
                }
    
                if(ret < 0)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                             "HDF5: An error occurred while attempting to write the attribute (%s) to file (%s)",
                              NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
                    return(NhlFATAL);
                }
                return(NhlNOERROR);
            }
            else
            {
                stepal = stepal->next;
            }
        }    
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "File (%s) was opened as a read only file, can not write to it",
                  NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

static void _HDF5CacheAttValue
#if     NhlNeedProto
(HDF5AttInqRec *att_inq,void *value)
#else
(att_inq,value)
    HDF5AttInqRec *att_inq;
    void *value;
#endif
{
    if (att_inq->type < 1 || value == NULL)
    {
        att_inq->value = NULL; 
    }   
    else if (att_inq->type == NCL_string && !(att_inq->name == Qfill_val || att_inq->name == Qmissing_val))
    {
        char *tmp = NclMalloc(att_inq->n_elem + 1);
        strncpy(tmp,value,att_inq->n_elem);
        tmp[att_inq->n_elem] = '\0'; 
        att_inq->value = NclMalloc(sizeof(NclQuark));
        *(NclQuark*)att_inq->value = NrmStringToQuark(tmp);
        NclFree(tmp);
    }   
    else
    {
        int sz = _NclSizeOf(att_inq->type);
        att_inq->value = NclMalloc(sz * att_inq->n_elem);
        memcpy(att_inq->value,value,sz * att_inq->n_elem);
    }   
}   

static NhlErrorTypes HDF5AddAtt
#if    NhlNeedProto
(void *therec,NclQuark theatt, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,theatt,data_type,n_items,values)
    void *therec;
    NclQuark theatt;
    NclBasicDataTypes data_type;
    int n_items;
    void * values;
#endif
{
    HDF5FileRecord *rec = (HDF5FileRecord *)therec;
    HDF5AttInqRecList *stepal;
    int i;
    hid_t ret;
    hid_t fid;
    char *typename;

    if(rec->wr_status <= 0)
    {
        typename = (char *)_Ncl2H5type(data_type);
        if(typename != NULL)
        {
            if (rec->open)
            {
                fid = rec->id;
            }
            else
            {
              /*
               *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
               */
                fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                if(fid < 1)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "HDF5: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    NclFree(typename);
                    return(NhlFATAL);
                }
                rec->id = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
            {
                hsize_t rank = 1;
                hsize_t dims[1];
                dims[0] = n_items;
                ret = (hid_t) _add_attr2group(rec->id, rank, dims, (void*)values,
                      "string", NrmQuarkToString(theatt),
                      "/", rec->h5_group);
            }
            if(ret > 0)
            {
                stepal = rec->att_list;
                if(stepal == NULL)
                {
                    rec->att_list = (HDF5AttInqRecList *)NclMalloc(
                                    (unsigned)sizeof(HDF5AttInqRecList));
                    rec->att_list->att_inq= (HDF5AttInqRec *)NclMalloc(
                                             (unsigned)sizeof(HDF5AttInqRec));
                    rec->att_list->next = NULL;
                    rec->att_list->att_inq->id = 1;
                    rec->att_list->att_inq->name = theatt;
                    rec->att_list->att_inq->type = data_type;
                    rec->att_list->att_inq->n_elem = n_items;
                    _HDF5CacheAttValue(rec->att_list->att_inq,values);
                }
                else
                {    
                    i = 0;
                    while(stepal->next != NULL)
                    {
                        stepal = stepal->next; 
                        i++;
                    }
                    stepal->next = (HDF5AttInqRecList *)NclMalloc(
                                   (unsigned)sizeof(HDF5AttInqRecList));
                    stepal->next->att_inq = (HDF5AttInqRec *)NclMalloc(
                                            (unsigned)sizeof(HDF5AttInqRec));
                    stepal->next->att_inq->id = i;
                    stepal->next->att_inq->name = theatt;
                    stepal->next->att_inq->type = data_type;
                    stepal->next->att_inq->n_elem = n_items;
                    stepal->next->next = NULL;
                    _HDF5CacheAttValue(stepal->next->att_inq,values);
                }
                rec->n_atts++;
                NclFree(typename);
                return(NhlNOERROR);
            } 
        } 
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

static NhlErrorTypes HDF5AddVarAtt
#if    NhlNeedProto
(void *therec,NclQuark thevar, NclQuark theatt, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,thevar,theatt,data_type,n_items,values)
    void *therec;
    NclQuark thevar;
    NclQuark theatt;
    NclBasicDataTypes data_type;
    int n_items;
    void * values;
#endif
{
    HDF5FileRecord *rec = (HDF5FileRecord*)therec;
    HDF5AttInqRecList* stepal;
    HDF5VarInqRecList* stepvl;
    char *typename;
    int i;
    hid_t fid,ret;
    int find_var = 0;
    
    if(rec->wr_status <= 0)
    {
        typename = (char *)_Ncl2H5type(data_type);
        if(typename != NULL)
        {
            if (rec->open)
            {
                fid = rec->id;
            }
            else
            {
              /*
               *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
               */
                fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                if(fid < 1)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "HDF5: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    NclFree(typename);
                    return(NhlFATAL);
                }
                rec->id = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
            stepvl = rec->var_list;    
            while(stepvl != NULL)
            {
                if(stepvl->var_inq->name == thevar)
                {
                    find_var = 1;
                    break;
                }
                else
                {
                    stepvl = stepvl->next;
                }
            }
            if(((0 == strcmp(typename, "string")) || (0 == strcmp(typename, "char"))) &&
               !(theatt == Qfill_val || theatt == Qmissing_val))
            {
                hsize_t rank = 1;
                hsize_t dims[1];
                dims[0] = strlen((char *)values);
                ret = (hid_t) _add_attr2dataset(rec->id, rank, dims, (void*)values,
                       "string", NrmQuarkToString(theatt),
                       NrmQuarkToString(thevar), rec->h5_group);
            }
            else
            {
                hsize_t rank = 1;
                hsize_t dims[1];
                dims[0] = n_items;
                ret = (hid_t) _add_attr2dataset(rec->id, rank, dims, (void*)values,
                       typename, NrmQuarkToString(theatt),
                       NrmQuarkToString(thevar), rec->h5_group);
            }

            if(find_var)
            {
                stepal = stepvl->var_inq->att_list;
                if(stepal == NULL)
                {
                    stepvl->var_inq->att_list= (HDF5AttInqRecList*)NclMalloc((unsigned)
                        sizeof(HDF5AttInqRecList));
                    stepvl->var_inq->att_list->att_inq = (HDF5AttInqRec*)NclMalloc((unsigned)sizeof(HDF5AttInqRec));
                    stepvl->var_inq->att_list->next = NULL;
                    stepvl->var_inq->att_list->att_inq->name = theatt;
                    stepvl->var_inq->att_list->att_inq->type = data_type;
                    stepvl->var_inq->att_list->att_inq->n_elem = n_items;
                    _HDF5CacheAttValue(stepvl->var_inq->att_list->att_inq,values);
                    stepvl->var_inq->n_atts = 1;
                }
                else
                {    
                    i = 0;
                    while(stepal->next != NULL)
                    {
                        stepal = stepal->next; 
                        i++;
                    }
                    stepal->next = (HDF5AttInqRecList*)NclMalloc((unsigned)sizeof(HDF5AttInqRecList));
                    stepal->next->att_inq = (HDF5AttInqRec*)NclMalloc((unsigned)sizeof(HDF5AttInqRec));
                    stepal->next->att_inq->name = theatt;
                    stepal->next->att_inq->type = data_type;
                    stepal->next->att_inq->n_elem = n_items;
                    stepal->next->next = NULL;
                    _HDF5CacheAttValue(stepal->next->att_inq,values);
                    stepvl->var_inq->n_atts++ ;
                }
                NclFree(typename);

                if(ret)
                    return(NhlINFO);
                else
                    return(NhlNOERROR);
            } 
        } 
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }

    return(NhlFATAL);
}

static NhlErrorTypes HDF5WriteCoord
#if     NhlNeedProto
(void *therec, NclQuark thevar, void* data, long* start, long* finish,long* stride)
#else
(therec, thevar, data, start, finish,stride)
void *therec;
NclQuark thevar;
void* data;
long* start;
long* finish;
long* stride;
#endif
{
        return(HDF5WriteVar(therec,thevar,data,start,finish,stride));
}

static NhlErrorTypes HDF5DelAtt
#if     NhlNeedProto
(void *therec, NclQuark theatt)
#else 
(therec, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
#if 0

    HDF5FileRecord* rec = (HDF5FileRecord*)therec;
    HDF5AttInqRecList *stepal;
    HDF5AttInqRecList *prev;
    hid_t fid;
    hid_t ret;


    if(rec->wr_status <= 0)
    {
        stepal = rec->file_atts;
        prev = NULL;
        while (stepal != NULL)
        {
            if (stepal->att_inq->name != theatt)
            {
                prev = stepal;
                stepal = stepal->next;
                continue;
            }
            if (! stepal->att_inq->virtual)
            {
                if (rec->open)
                {
                    fid = rec->id;
                }
                else
                {
                  /*
                   *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
                   */
                    fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                    if(fid < 1)
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                              "HDF5: Could not reopen the file (%s) for writing",
                              NrmQuarkToString(rec->file_path_q));
                        NclFree(typename);
                        return(NhlFATAL);
                    }
                    rec->id = fid;
                    rec->define_mode = 0;
                    rec->open = 1;
                }
            }

            if (! prev)
            {
                rec->file_atts = stepal->next;
            }
            else
            {
                prev->next = stepal->next;
            }
            if (stepal->att_inq->value)
                NclFree(stepal->att_inq->value);

            NclFree(stepal->att_inq);
            NclFree(stepal);
            if(ret == -1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF5: An error occurred while attempting to delete the attribute (%s) from file (%s)",
                      NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            return(NhlNOERROR);
        }
    }
    else
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                  "File (%s) was opened as a read only file, can not write to it",
                   NrmQuarkToString(rec->file_path_q)));
    }
    return(NhlFATAL);
#endif

    return(NhlNOERROR);
}

int _HDF5WriteVarAtt2Group
#if    NhlNeedProto
(HDF5GrpInqRec *grp_inq, NclQuark thevar, NclQuark theatt, void* data, NclHDF5group_node_t *h5_group)
#else
(grp_inq, thevar, theatt, data, h5_group)
HDF5GrpInqRec *grp_inq;
NclQuark thevar;
NclQuark theatt;
void* data;
NclHDF5group_node_t *h5_group;
#endif
{
    HDF5AttInqRecList *stepal;
    HDF5VarInqRecList *stepvl;
    HDF5GrpInqRecList *stepgl;
    int n;
    int ret = 0;
    char * buffer = NULL;

    stepvl = grp_inq->var_list;
    for(n=0; n<grp_inq->n_vars; n++)
    {
        if(stepvl->var_inq->name == thevar)
        {
            stepal = stepvl->var_inq->att_list;
            while(stepal != NULL)
            {
                if(stepal->att_inq->name == theatt)
                {
                    if(stepal->att_inq->type == NCL_string && !(theatt == Qfill_val || theatt == Qmissing_val))
                    {
                        buffer = NrmQuarkToString(*(NclQuark*)data);
                        hsize_t rank = 1;
                        hsize_t dims[1];
                        dims[0] = strlen(buffer);
                        ret = (hid_t) _add_attr2dataset(grp_inq->id, rank, dims, (void*)buffer,
                                                       "string", NrmQuarkToString(theatt),
                                                       NrmQuarkToString(thevar), h5_group);
                        if (stepal->att_inq->value != NULL)
                            memcpy(stepal->att_inq->value,buffer,sizeof(NclQuark));
                    }
                    else
                    {
                        hsize_t rank = 1;
                        hsize_t dims[1];
                        dims[0] = stepal->att_inq->n_elem;
                        ret = (hid_t) _add_attr2dataset(grp_inq->id, rank, dims, data,
                                                       "string", NrmQuarkToString(theatt),
                                                       NrmQuarkToString(thevar), h5_group);
                        if (stepal->att_inq->value != NULL)
                        {
                            memcpy(stepal->att_inq->value,data,
                                    _NclSizeOf(stepal->att_inq->type)*stepal->att_inq->n_elem);
                        }
                    }

                    if(ret == -1)
                    {
                        if (theatt == NrmStringToQuark("_FillValue"))
                        {
                            NhlPError(NhlWARNING,NhlEUNKNOWN,
                                     "HDF5: HDF5 does not allow the _FillValue attribute to be modified after data written to variable (%s) in file (%s)",
                                      NrmQuarkToString(thevar),NrmQuarkToString(grp_inq->file));
                        }
                        else
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                     "HDF5: An error occurred while attempting to write the attribute (%s) to variable (%s) in file (%s)",
                                      NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(grp_inq->file));
                        }
                    }
                    return(1);
                }
                else
                {    
                    stepal = stepal->next;
                }
            }    
        }
        else
        {
            stepvl = stepvl->next;
        }
    } 

    stepgl = grp_inq->grp_list;
    for(n=0; n<grp_inq->n_grps; n++)
    {
        ret = _HDF5WriteVarAtt2Group(stepgl->grp_inq, thevar, theatt, data, h5_group);
        if(ret)
        {
            return (ret);
        }
        else
        {
            stepgl = stepgl->next;
        }
    } 

    return(0);
}

static NhlErrorTypes HDF5WriteVarAtt 
#if    NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt, void* data)
#else
(therec,thevar, theatt,  data )
void *therec;
NclQuark thevar;
NclQuark theatt;
void* data;
#endif
{
    HDF5FileRecord* rec = (HDF5FileRecord*)therec;
    HDF5AttInqRecList *stepal;
    HDF5VarInqRecList *stepvl;
    HDF5GrpInqRecList *stepgl;
    int fid;
    int n;
    int ret;
    char * buffer = NULL;

    if(rec->wr_status <= 0)
    {
        stepvl = rec->var_list;
        while(stepvl != NULL)
        {
            if(stepvl->var_inq->name == thevar)
            {
                stepal = stepvl->var_inq->att_list;
                while(stepal != NULL)
                {
                    if(stepal->att_inq->name == theatt)
                    {
                        if (rec->open)
                        {
                            fid = rec->id;
                        }
                        else
                        {
                          /*
                           *fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_RDONLY, H5P_DEFAULT);
                           */
                            fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                            if(fid < 1)
                            {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                      "HDF5: Could not reopen the file (%s) for writing",
                                      NrmQuarkToString(rec->file_path_q));
                                return(NhlFATAL);
                            }
                            rec->id = fid;
                            rec->define_mode = 0;
                            rec->open = 1;
                        }

                        if(stepal->att_inq->type == NCL_string && !(theatt == Qfill_val || theatt == Qmissing_val))
                        {
                            buffer = NrmQuarkToString(*(NclQuark*)data);
                            hsize_t rank = 1;
                            hsize_t dims[1];
                            dims[0] = strlen(buffer);
                            ret = (hid_t) _add_attr2dataset(rec->id, rank, dims, (void*)buffer,
                                                           "string", NrmQuarkToString(theatt),
                                                           NrmQuarkToString(thevar), rec->h5_group);
                            if (stepal->att_inq->value != NULL)
                                memcpy(stepal->att_inq->value,buffer,sizeof(NclQuark));
                        }
                        else
                        {
                            hsize_t rank = 1;
                            hsize_t dims[1];
                            dims[0] = stepal->att_inq->n_elem;
                            ret = (hid_t) _add_attr2dataset(rec->id, rank, dims, data,
                                                           "string", NrmQuarkToString(theatt),
                                                           NrmQuarkToString(thevar), rec->h5_group);
                            if (stepal->att_inq->value != NULL)
                            {
                                memcpy(stepal->att_inq->value,data,
                                        _NclSizeOf(stepal->att_inq->type)*stepal->att_inq->n_elem);
                            }
                        }

                        if(ret == -1)
                        {
                            if (theatt == NrmStringToQuark("_FillValue") && rec->format > 2)
                            {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"HDF5: HDF5 does not allow the _FillValue attribute to be modified after data written to variable (%s) in file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                                return (NhlWARNING);
                            }
                            else
                            {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF5: An error occurred while attempting to write the attribute (%s) to variable (%s) in file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                                return(NhlFATAL);
                            }
                        }
                        return(NhlNOERROR);
                    }
                    else
                    {    
                        stepal = stepal->next;
                    }
                }    
            }
            else
            {
                stepvl = stepvl->next;
            }
        } 

        stepgl = rec->grp_list;
        for(n=0; n<rec->n_grps; n++)
        {
            ret = _HDF5WriteVarAtt2Group(stepgl->grp_inq, thevar, theatt, data, rec->h5_group);
            if(ret)
            {
                return (NhlNOERROR);
            }
            else
            {
                stepgl = stepgl->next;
            }
        }
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

static NhlErrorTypes HDF5AddVarChunkCache
#if    NhlNeedProto
(void* therec, NclQuark thevar, nclH5size_t cache_size, nclH5size_t cache_nelems, float cache_preemption)
#else
(therec, thevar, cache_size, cache_nelems, cache_preemption)
void* therec;
NclQuark thevar;
nclH5size_t cache_size;
nclH5size_t cache_nelems;
float cache_preemption;
#endif
{
    HDF5FileRecord* rec = (HDF5FileRecord*)therec;
    HDF5VarInqRecList *stepvl = NULL;
    int ret = NhlNOERROR;
    int fid;

  /*
   *fprintf(stderr, "Enter HDF5AddVarChunkCache, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            fid = rec->id;
        }
        else
        {
            fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
            if(fid < 1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "HDF5: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->id = fid;
            rec->define_mode = 0;
            rec->open = 1;
        }

        stepvl = rec->var_list;
        while(stepvl != NULL)
        {
            if(stepvl->var_inq->name == thevar)
            {
                if((cache_size > 0) && (cache_nelems > 0))
                    stepvl->var_inq->use_cache = 1;
                stepvl->var_inq->cache_size = cache_size;
                stepvl->var_inq->cache_nelems = cache_nelems;
                if(cache_preemption < 0.0)
                    stepvl->var_inq->cache_preemption = 0.0;
                else if(cache_preemption > 1.0)
                    stepvl->var_inq->cache_preemption = 1.0;
                else
                    stepvl->var_inq->cache_preemption = cache_preemption;

/*
                if(stepvl->var_inq->use_cache)
		{
		    ret = nc_set_var_chunk_cache(fid, stepvl->var_inq->varid,
                                                 cache_size, cache_nelems,
                                                 stepvl->var_inq->cache_preemption);
		}
*/
                ret = NhlNOERROR;
                break;
            }
            stepvl= stepvl->next;
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "File (%s) was opened as a read only file, can not write to it",
                  NrmQuarkToString(rec->file_path_q));
        ret = NhlFATAL;
    }

  /*
   *fprintf(stderr, "Leave HDF5AddVarChunkCache, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    return(ret);
}

static NhlErrorTypes HDF5SetVarCompressLevel
#if    NhlNeedProto
(void* therec, NclQuark thevar, int compress_level)
#else
(therec,thevar,compress_level)
void* therec;
NclQuark thevar;
int compress_level;
#endif
{
    HDF5FileRecord* rec = (HDF5FileRecord*)therec;
    HDF5VarInqRecList *stepvl = NULL;
    int ret = NhlNOERROR;
    int fid;
    int shuffle = 0;
    int deflate = compress_level;
    int deflate_level = compress_level;

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            fid = rec->id;
        }
        else
        {
            fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
            if(fid < 1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "HDF5: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->id = fid;
            rec->define_mode = 0;
            rec->open = 1;
        }

        stepvl = rec->var_list;
        while(stepvl != NULL)
        {
            if(stepvl->var_inq->name == thevar)
            {
                stepvl->var_inq->compress_level = compress_level;
                if(compress_level > 0)
                    deflate = compress_level;
/*
                ret = nc_def_var_deflate(fid, stepvl->var_inq->varid, shuffle,
                                         deflate, deflate_level);
*/
                ret = NhlNOERROR;
                break;
            }
            stepvl= stepvl->next;
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "File (%s) was opened as a read only file, can not write to it",
                  NrmQuarkToString(rec->file_path_q));
        ret = NhlFATAL;
    }

    return(ret);
}

static NhlErrorTypes HDF5AddChunkDim
#if    NhlNeedProto
(void* therec, NclQuark thedim, nclH5size_t size,int is_unlimited)
#else
(therec, thedim, size,is_unlimited)
void* therec;
NclQuark thedim;
nclH5size_t size;
int is_unlimited;
#endif
{
    HDF5FileRecord *rec = (HDF5FileRecord*) therec;
    int fid;
    HDF5DimInqRecList *stepdl;
    int ret = -1;
    int add_scalar = 0;

    if(rec->wr_status <=  0)
    {
        if(thedim == NrmStringToQuark("ncl_scalar"))
        {
            if (size != 1)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "HDF5: \"ncl_scalar\" is a reserved file dimension name in NCL, %s",
                    "this name can only represent dimensions of size 1"));
                return(NhlFATAL);
            }
            add_scalar = 1;
        }
        else
        {
            if (rec->open)
            {
                fid = rec->id;
            }
            else
            {
                fid = H5Fopen(NrmQuarkToString(rec->file_path_q), H5F_ACC_TRUNC, H5P_DEFAULT);
                if(fid < 1)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "HDF5: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    return(NhlFATAL);
                }
                rec->id = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
        }
        stepdl = rec->chunk_dims;

        if (add_scalar)
        {
            rec->has_scalar_dim = 1;
            rec->chunk_dims = (HDF5DimInqRecList*)NclMalloc(
                (unsigned) sizeof(HDF5DimInqRecList));
            rec->chunk_dims->dim_inq = (HDF5DimInqRec*)NclMalloc(
                (unsigned)sizeof(HDF5DimInqRec));
            rec->chunk_dims->next = stepdl;
            rec->chunk_dims->dim_inq->ncldim_id = -5;
            rec->chunk_dims->dim_inq->size = 1;
            rec->chunk_dims->dim_inq->is_unlimited = 0;
            rec->chunk_dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
            rec->n_chunk_dims++;
        }
        else if(stepdl == NULL)
        {
            rec->chunk_dims = (HDF5DimInqRecList*)NclMalloc((unsigned)sizeof(HDF5DimInqRecList));
            rec->chunk_dims->dim_inq = (HDF5DimInqRec*)NclMalloc((unsigned)sizeof(HDF5DimInqRec));
            rec->chunk_dims->dim_inq->ncldim_id = (nclH5size_t)ret;
            rec->chunk_dims->dim_inq->name = thedim;
            rec->chunk_dims->dim_inq->size = (nclH5size_t)size;
            rec->chunk_dims->dim_inq->is_unlimited= is_unlimited;
            if(rec->chunk_dims->dim_inq->size < 1)
                rec->chunk_dims->dim_inq->size = (nclH5size_t)1;
            rec->chunk_dims->next = NULL;
            rec->n_chunk_dims = 1;
        }
        else
        {
            while(stepdl->next != NULL)
            {
                stepdl = stepdl->next;
            }
            stepdl->next = (HDF5DimInqRecList*)NclMalloc((unsigned)sizeof(HDF5DimInqRecList));
            stepdl->next->dim_inq = (HDF5DimInqRec*)NclMalloc((unsigned)sizeof(HDF5DimInqRec));
            stepdl->next->dim_inq->ncldim_id = (nclH5size_t)ret;
            stepdl->next->dim_inq->name = thedim;
            stepdl->next->dim_inq->size = (nclH5size_t)size;
            stepdl->next->dim_inq->is_unlimited= is_unlimited;
            if(stepdl->next->dim_inq->size < 1)
                stepdl->next->dim_inq->size = (nclH5size_t)1;
            stepdl->next->next = NULL;
            rec->n_chunk_dims++;
        }
        return(NhlNOERROR);
    }
    else
    {    
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "File (%s) was opened as a read only file, can not write to it",
            NrmQuarkToString(rec->file_path_q)));
    }
    return(NhlFATAL);
}

static NhlErrorTypes HDF5SetOption
#if	NhlNeedProto
(void *therec,NclQuark option, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,theatt,data_type,n_items,values)
    void *therec;
    NclQuark theatt;
    NclBasicDataTypes data_type;
    int n_items;
    void * values;
#endif
{
    HDF5FileRecord *rec = (HDF5FileRecord*)therec;

    if (option == NrmStringToQuark("compressionlevel"))
    {
        if (*(int*)values < -1 || *(int*)values > 9)
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                     "HDF5SetOption: option (%s) value cannot be less than -1 or greater than 9",
                      NrmQuarkToString(option));
            return(NhlWARNING);
        }
        rec->options[H5_COMPRESSION_LEVEL_OPT].values = (void*) values;
    }
    else if (option == NrmStringToQuark("usecache"))
    {
        rec->options[H5_USE_CACHE_OPT].values = values;
    }
    else if (option == NrmStringToQuark("cachesize"))
    {
        if (*(int*)values < 1)
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                     "HDF5SetOption: option (%s) value cannot be less than 1",
                     NrmQuarkToString(option));
            return(NhlWARNING);
        }
        rec->options[H5_CACHE_SIZE_OPT].values = values;
    }
    else if (option == NrmStringToQuark("cachenelems"))
    {
        if (*(int*)values < 3)
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                     "HDF5SetOption: option (%s) value cannot be less than 3",
                      NrmQuarkToString(option));
            return(NhlWARNING);
        }
        else
        {
            unsigned int *iv = (unsigned int *)values;
            *iv = _closest_prime(*iv);
            rec->options[H5_CACHE_NELEMS_OPT].values = (void*) iv;
        }
    }
    else if (option == NrmStringToQuark("cachepreemption"))
    {
        float *fv = (float *)values;
        rec->options[H5_CACHE_PREEMPTION_OPT].values = (void*) fv;
    }

    return NhlNOERROR;
}

NclFormatFunctionRec HDF5Rec = {
/* NclInitializeFileRecFunc initialize_file_rec */ HDF5InitializeFileRec,
/* NclCreateFileFunc        create_file; */        HDF5CreateFile,
/* NclOpenFileFunc          open_file; */          HDF5OpenFile,
/* NclFreeFileRecFunc       free_file_rec; */      HDF5FreeRec,
/* NclGetVarNamesFunc       get_var_names; */      HDF5GetVarNames,
/* NclGetVarInfoFunc        get_var_info; */       HDF5GetVarInfo,
/* NclGetDimNamesFunc       get_dim_names; */      HDF5GetDimNames,
/* NclGetDimInfoFunc        get_dim_info; */       HDF5GetDimInfo,
/* NclGetAttNamesFunc       get_att_names; */      HDF5GetAttNames,
/* NclGetAttInfoFunc        get_att_info; */       HDF5GetAttInfo,
/* NclGetVarAttNamesFunc    get_var_att_names; */  HDF5GetVarAttNames,
/* NclGetVarAttInfoFunc     get_var_att_info; */   HDF5GetVarAttInfo,
/* NclGetCoordInfoFunc      get_coord_info; */     HDF5GetCoordInfo,
/* NclReadCoordFunc         read_coord; */         HDF5ReadCoord,
/* NclReadCoordFunc         read_coord; */         NULL,
/* NclReadVarFunc           read_var; */           HDF5ReadVar,
/* NclReadVarFunc           read_var; */           NULL,
/* NclReadAttFunc           read_att; */           HDF5ReadAtt,
/* NclReadVarAttFunc        read_var_att; */       HDF5ReadVarAtt,
/* NclWriteCoordFunc        write_coord; */        HDF5WriteCoord,
/* NclWriteCoordFunc        write_coord_ns; */     NULL,
/* NclWriteVarFunc          write_var; */          HDF5WriteVar,
/* NclWriteVarFunc          write_var_ns; */       NULL,
/* NclWriteAttFunc          write_att; */          HDF5WriteAtt,
/* NclWriteVarAttFunc       write_var_att; */      HDF5WriteVarAtt,
/* NclAddDimFunc            add_dim; */            HDF5AddDim,
/* NclAddChunkDimFunc       add_chunk_dim; */      HDF5AddChunkDim,
/* NclRenameDimFunc         rename_dim; */         NULL,
/* NclAddVarFunc            add_var; */            HDF5AddVar,
/* NclAddVarChunkFunc       add_var_chunk; */      HDF5AddVarChunk,
/* NclAddVarChunkCacheFunc  add_var_chunk_cache; */ HDF5AddVarChunkCache,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ HDF5SetVarCompressLevel,
/* NclAddVarFunc            add_coord_var; */      HDF5AddCoordVar,
/* NclAddAttFunc            add_att; */            HDF5AddAtt,
/* NclAddVarAttFunc         add_var_att; */        HDF5AddVarAtt,
/* NclMapFormatTypeToNcl    map_format_type_to_ncl; */    NULL,
/* NclMapNclTypeToFormat    map_ncl_type_to_format; */    _Ncl2H5type,
/* NclDelAttFunc            del_att; */            HDF5DelAtt,
/* NclDelVarAttFunc         del_var_att; */        NULL,
/* NclGetGrpNamesFunc       get_grp_names; */      HDF5GetGrpNames,
/* NclGetGrpInfoFunc        get_grp_info; */       HDF5GetGrpInfo,
/* NclGetGrpAttNamesFunc    get_grp_att_names; */  HDF5GetGrpAttNames,
/* NclGetGrpAttInfoFunc     get_grp_att_info; */   HDF5GetGrpAttInfo,
/* NclAddGrpFunc           add_grp; */                  NULL,
/* NclSetOptionFunc         set_option;  */        HDF5SetOption
};

NclFormatFunctionRecPtr H5AddFileFormat 
#if    NhlNeedProto
(void)
#else 
()
#endif
{
    return(&HDF5Rec);
}


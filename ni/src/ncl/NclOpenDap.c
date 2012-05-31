/*
 *      $URL$
 *      $Id: NclOpenDap.c,v 1.60 2010-05-06 22:52:28 huangwei Exp $
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
 *    Date:        Fri Jul 15 11:45:21 MDT 2010
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
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclData.h"

#include   <stdio.h>
#include   <sys/types.h>
#include   <sys/stat.h>
#include   <fcntl.h>
#include   <unistd.h>
#include   <string.h>
#include   <strings.h>
#include   <dirent.h>
#include   <stdlib.h>

#include <liboc-dap/oc.h>
#include <assert.h>
#include <netcdf.h>
#include <math.h>
#include <unistd.h>

#include "NclNewFileStructure.h"

#define MAX_NAME_LENGTH	1024

static size_t ChunkSizeHint;

enum OpenDap_Type
{
    dods_null_c,
    dods_byte_c,
    dods_int16_c,
    dods_uint16_c,
    dods_int32_c,
    dods_uint32_c,
    dods_float32_c,
    dods_float64_c,
    dods_str_c,
    dods_url_c,
    dods_array_c,
    dods_structure_c,
    dods_sequence_c,
    dods_grid_c
};

NclFileDimRecord *_OpenDap_get_dims(int gid, int varid);
NclFileAttRecord *_OpenDap_get_atts(int gid, int n_atts);
NclFileVarRecord *_OpenDap_get_vars(int gid, int n_vars, int *has_scalar_dim);

typedef struct _OpenDapFileRecord OpenDapFileRecord;
typedef struct _OpenDapVarInqRec OpenDapVarInqRec;
typedef struct _OpenDapDimInqRec OpenDapDimInqRec;
typedef struct _OpenDapAttInqRec OpenDapAttInqRec;
typedef struct _OpenDapVarInqRecList OpenDapVarInqRecList;
typedef struct _OpenDapDimInqRecList OpenDapDimInqRecList;
typedef struct _OpenDapAttInqRecList OpenDapAttInqRecList;
typedef struct _OpenDapOptions OpenDapOptions;

struct _OpenDapVarInqRecList
{
    OpenDapVarInqRec *var_inq;
    OpenDapVarInqRecList *next;
};

struct _OpenDapDimInqRecList
{
    OpenDapDimInqRec *dim_inq;
    OpenDapDimInqRecList *next;
};

struct _OpenDapAttInqRecList
{
    OpenDapAttInqRec *att_inq;
    OpenDapAttInqRecList *next;
};

struct _OpenDapVarInqRec
{
    int varid;
    NclQuark name;
    nc_type    data_type;
    int    n_dims;
    int    dim[MAX_VAR_DIMS];
    int    compress_level;
    int    n_chunk_dims;
    ng_size_t    chunk_dim[MAX_VAR_DIMS];
    int    use_cache;
    ng_size_t    cache_size;
    ng_size_t    cache_nelems;
    float    cache_preemption;
    int    natts;
    OpenDapAttInqRecList *att_list;
    void *value;
};

struct _OpenDapDimInqRec
{
    int dimid;
    int is_unlimited;
    NclQuark name;
    long size;
};
    
struct _OpenDapAttInqRec
{
    int att_num;
    NclQuark name;
    int    varid;
    nc_type data_type;
    int    len;
    void    *value;
    int virtual;
};

            
struct _OpenDapOptions
{
    NclQuark name;
    NclBasicDataTypes data_type;
    int n_values;
    void *values;
};

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

struct _OpenDapFileRecord
{
NclQuark    file_path_q;
int        wr_status;
int        n_vars;
OpenDapVarInqRecList *vars;
int        n_dims;
OpenDapDimInqRecList *dims;
int        compress_level;
int        n_chunk_dims;
OpenDapDimInqRecList *chunk_dims;
int        has_scalar_dim;
int        n_file_atts;
OpenDapAttInqRecList *file_atts;
int             n_options;
OpenDapOptions   *options;
size_t            fid;
/*
 * since any value of fid is now allowed (including -1) the
 * 'open' flag member is added to indicate whether the file open state
 */
int             open;    
int             header_reserve_space;
int             define_mode;
int             format;
int        	use_cache;
ng_size_t       cache_size;
ng_size_t       cache_nelems;
float        cache_preemption;
};

static NhlErrorTypes DapAddVarChunk(void* therec, NclQuark thevar, int n_chunk_dims, ng_size_t *chunk_dims);

NclFileAttRecord *_OpenDap_get_atts(int gid, int n_atts)
{
    char buffer[NC_MAX_NAME + 2];
    int i;
    size_t alen;
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;

    if(n_atts < 1)
        return NULL;

  /*
   */
    fprintf(stderr, "\nEnter _OpenDap_get_atts, file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tgid: %d, n_atts: %d\n", gid, n_atts);

    attrec = _NclFileAttAlloc(n_atts);
    assert(attrec);

    attrec->id = -1;
    attrec->gid = gid;
    attrec->aid = 0;

    for(i = 0; i < n_atts; i++)
    {
        attnode = &(attrec->att_node[i]);

        oc_inq_att_name(gid, i, buffer);

      /*
       */
        fprintf(stderr,"\tAtt No. %d, name: <%s>\n", i, buffer);

        attnode->is_virtual = 0;
        attnode->is_opaque = 0;
        attnode->is_vlen = 0;
        attnode->is_compound = 0;
        attnode->name = NrmStringToQuark(buffer);

      /*
        nc_inq_attname(gid, aid, i, buffer);
        nc_inq_att(gid, aid, buffer, &(attnode->the_nc_type), &alen);
        attnode->n_elem = alen;
       */

      /*
        OpenDapGetAttrVal(gid, aid, attnode);
        fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tattnode->name: <%s>, the_nc_type = %d, n_elem = %d\n",
                           buffer, attnode->the_nc_type, attnode->n_elem);
       */
    }

  /*
   */
    fprintf(stderr, "Leave _OpenDap_get_atts, file: %s, line: %d\n\n", __FILE__, __LINE__);

    return attrec;
}

NclFileDimRecord *_OpenDap_get_dims(int gid, int varid)
{
    char buffer[MAX_NAME_LENGTH];
    char buffer2[MAX_NAME_LENGTH];
    int ndims_grp;
    int dimid, i, j;
    int status = 0;

    NclFileDimRecord *dimrec = NULL;

  /*
   */
    fprintf(stderr, "\nEnter _OpenDap_get_dims, file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tgid = %d\n", gid);
    fprintf(stderr, "\tvarid = %d\n", varid);

  /*Find the number of dimids defined in this group.*/
    ndims_grp = oc_inq_array_ndim(gid, varid);

  /*
   */
    fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tndims_grp = %d\n", ndims_grp);

    if(ndims_grp < 1)
        return dimrec;

    dimrec = _NclFileDimAlloc(ndims_grp);
    dimrec->gid = gid;

    for(i = 0 ; i < ndims_grp; i++)
    {
        dimrec->dim_node[i].id = i;
        dimrec->dim_node[i].is_unlimited = 0;

        dimrec->dim_node[i].size = (ng_size_t) oc_inq_dim_size(gid, i);
        status = oc_inq_dim_name(gid, i, buffer);
        dimrec->dim_node[i].name = NrmStringToQuark(buffer);

      /*
       */
        fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\ti = %d, name: <%s>, size: %ld\n",
                i, buffer, dimrec->dim_node[i].size);
        fprintf(stderr,"\tId: %d, dim No. %d, name: <%s>, size: %ld\n",
                gid, i, buffer, dimrec->dim_node[i].size);
    }

  /*
   */
    fprintf(stderr, "Leave _OpenDap_get_dims, file: %s, line: %d\n\n", __FILE__, __LINE__);

    return dimrec;
}

NclFileVarRecord *_OpenDap_get_vars(int gid, int n_vars, int *has_scalar_dim)
{
    char buffer[MAX_NAME_LENGTH];
    char *cptr;
    int  i, j, n_atts, n_dims;
    long tmp_size = 0;

    NclFileVarRecord  *varrec;
    NclFileDimRecord  *dimrec;
    NclFileAttRecord  *attrec;

    NclFileVarNode    *varnode;
    NclFileDimNode    *dimnode;
    NclFileAttNode    *attnode;

    int    rc = -1;
    int    storage_in = -1;
    size_t chunksizes[MAX_VAR_DIMS];

    int  shufflep = -1;
    int  deflatep = -1;

  /*
   */
    fprintf(stderr, "\nEnter _OpenDap_get_vars, file: %s, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tgid = %d\n", gid);
    fprintf(stderr, "\tn_vars = %d\n", n_vars);

    if(n_vars < 1)
        return NULL;

    *has_scalar_dim = 0;

    varrec = _NclFileVarAlloc(n_vars);
    varrec->gid = gid;

    for(i = 0; i < n_vars; i++)
    {
        varnode = &(varrec->var_node[i]);
        varnode->id = i;
        varnode->gid = gid;
        varnode->value = NULL;
        varnode->is_chunked = 0;
        varnode->is_compound = 0;

        oc_inq_var_name(gid, i, buffer);

        fprintf(stderr, "\tVar No. %d, real_name: <%s>\n", i, buffer);

        varnode->real_name = NrmStringToQuark(buffer);

        cptr = strrchr(buffer, '/');

        if(NULL == cptr)
            varnode->name = NrmStringToQuark(buffer);
        else
            varnode->name = NrmStringToQuark(cptr + 1);

        varnode->type = oc_inq_var_type(gid, i);

      /*
       */
        fprintf(stderr, "\tVar No. %d, name: <%s>\n",
                         i, NrmQuarkToString(varnode->name));
        fprintf(stderr, "\t\tvarnode->type = %d, dods_array_c = %d\n",
                         varnode->type, dods_array_c);

#if 0
        varnode->type = OpenDapMapToNcl(&(varnode->the_nc_type));
#endif

        if(dods_str_c == varnode->type)
           n_dims = 0;
        else
           n_dims = oc_inq_array_ndim(gid, i);

        if(0 == n_dims)
        {
            dimrec = _NclFileDimAlloc(1);
            dimrec->dim_node[0].id = -5;
            *has_scalar_dim = 1;
        }
        else
        {
            dimrec = _NclFileDimAlloc(n_dims);
        }

        dimrec->gid = gid;
        varnode->dim_rec = dimrec;

      /*
       */
        fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tn_dims %d\n", n_dims);

        varnode->is_chunked = 0;

        for(j = 0; j < n_dims; j++)
        {
            dimrec->dim_node[j].id = j;
            dimrec->dim_node[j].is_unlimited = 0;
            dimrec->dim_node[j].size = oc_inq_dim_size(gid, j);
            oc_inq_dim_name(gid, j, buffer);
            dimrec->dim_node[j].name = NrmStringToQuark(buffer);

          /*
           */
            fprintf(stderr, "\t\tDim No. %d, name: <%s>, size: %d\n",
                            j, NrmQuarkToString(dimrec->dim_node[j].name),
                            dimrec->dim_node[j].size);
        }

#if 0
        varnode->att_rec = _OpenDap_get_atts(gid, i, n_atts);
#endif
    }

  /*
   */
    fprintf(stderr, "Leave _OpenDap_get_vars, file: %s, line: %d\n\n", __FILE__, __LINE__);

    return varrec;
}

static NclBasicDataTypes DapMapToNcl(void* the_type)
{
    static int first = 1;
    static NclBasicDataTypes long_type;
    if(first)
    {
        if(sizeof(nclong) == _NclSizeOf(NCL_int))
        {
            long_type = NCL_int;
        }
        else if(sizeof(nclong) == _NclSizeOf(NCL_long))
        {
            long_type = NCL_long;
        }
        else
        {
            long_type = NCL_none;
        }
        first = 0;
    }

    switch(*(nc_type*)the_type)
    {
    case NC_BYTE:
        return(NCL_byte);
    case NC_CHAR:
        return(NCL_char);
    case NC_SHORT:
        return(NCL_short);
    case NC_LONG:
        return(long_type);
    case NC_FLOAT:
        return(NCL_float);
    case NC_DOUBLE:
        return(NCL_double);
    default:
        return(NCL_none);
    }
}

static void *DapMapFromNcl
#if    NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
    NclBasicDataTypes the_type;
#endif
{
    static int first = 1;
    static NclBasicDataTypes long_type;
    void *out_type = (void*)NclMalloc((unsigned)sizeof(nc_type));;
    if(first)
    {
        if(sizeof(nclong) == _NclSizeOf(NCL_long))
        {
            long_type = NCL_long;
        }
        else
        if(sizeof(nclong) == _NclSizeOf(NCL_int))
        {
            long_type = NCL_int;
        }
        else
        {
            long_type = NCL_none;
        }
        first = 0;
    }

#if 0
    switch(the_type)
    {
    case NCL_byte:
        *(nc_type*)out_type = NC_BYTE;
                break;
    case NCL_char:
        *(nc_type*)out_type = NC_CHAR;
                break;
    case NCL_short:
        *(nc_type*)out_type = NC_SHORT;
                break;
    case NCL_int:
    case NCL_logical:
        *(nc_type*)out_type = NC_LONG;
                break;
    case NCL_long:
        if(long_type == the_type)
        {
            *(nc_type*)out_type = NC_LONG;
        }
        else
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,"Can't map type, netCDF does not support 64 bit longs, NCL will try to promote type to double, errors may occur");
            NclFree(out_type);
            return(NULL);
        }
        break;
    case NCL_float:
        *(nc_type*)out_type = NC_FLOAT;
        break;
    case NCL_double:
        *(nc_type*)out_type = NC_DOUBLE;
        break;
        default:
        NclFree(out_type);
        out_type = NULL;
    }
#endif
    return(out_type);
}

static void DapGetAttrVal
#if    NhlNeedProto
(int ncid,OpenDapAttInqRec* att_inq)
#else
(ncid,att_inq)
int ncid,
OpenDapAttInqRec* att_inq
#endif
{
    char *tmp;
    int ret;

    if (att_inq->virtual)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Error retrieving value for virtual attribute %s",
              NrmQuarkToString(att_inq->name));
        return;
    }
    if (att_inq->data_type < 1)
    {
        att_inq->value = NULL;
    }
    else if(!(att_inq->name == Qfill_val || att_inq->name == Qmissing_val))
    {
        tmp = (char*)NclMalloc(att_inq->len+1);
        tmp[att_inq->len] = '\0';
        ret = ncattget(ncid,att_inq->varid,NrmQuarkToString(att_inq->name),tmp);
        att_inq->value = NclMalloc(sizeof(NclQuark));
        *(string *)att_inq->value = NrmStringToQuark(tmp);
        NclFree(tmp);
    } 
    else
    {
        att_inq->value = NclMalloc(nctypelen(att_inq->data_type)*att_inq->len);
        ret = ncattget(ncid,att_inq->varid,NrmQuarkToString(att_inq->name),att_inq->value);
    }
    return;
}

static void DapGetDimVals
#if    NhlNeedProto
(int ncid,OpenDapFileRecord* frec)
#else
(ncid,frec)
int ncid,
OpenDapAttInqRec* frec
#endif
{
    OpenDapDimInqRecList *dl = frec->dims;
    long start = 0;
    int ret;

    for(dl; dl != NULL; dl = dl->next)
    {
        OpenDapDimInqRec *dim_inq = dl->dim_inq;
        OpenDapVarInqRecList *vl = frec->vars;

        for (vl; vl != NULL; vl = vl->next)
        {
            if (vl->var_inq->name != dim_inq->name)
                continue;
            break;
        }
        if (! vl || vl->var_inq->n_dims > 1 || (vl->var_inq->dim[0] != dim_inq->dimid))
            continue;
        if (dim_inq->size == 0)
            continue;
            vl->var_inq->value = NclMalloc(nctypelen(vl->var_inq->data_type) * dim_inq->size);
        ret = ncvarget(ncid,vl->var_inq->varid,&start,&dim_inq->size,vl->var_inq->value);
    }
    return;
}

static void CloseOrNot(NclFileGrpNode *grpnode, int fid, int sync)
{
    if (sync)
    {
        nc_sync(fid);
    }
    grpnode->open = 1;
    grpnode->fid = fid;
}

static void EndDefineModeIf
#if    NhlNeedProto
(OpenDapFileRecord *rec, int fid)
#else
(rec,fid)
OpenDapFileRecord *rec;
int fid;
#endif
{
    /* 
     * The header space will not be reserved unless at least one variable has been defined;
     * hence the double condition.
     */
    if (rec->define_mode)
    {
        if (rec->n_vars > 0 && rec->header_reserve_space > 0)
        {
            nc__enddef(fid,rec->header_reserve_space,4,0,4);
            rec->header_reserve_space = 0;
        }
        else
        {
            ncendef(fid);
        }
        rec->define_mode = 0;
    }
    return;
}

static void *DapInitializeFileRec
#if    NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormatType *format;
#endif
{
    static int first = True;
    OpenDapFileRecord *therec = NULL;
    ng_size_t blksize = getpagesize();

    if (first)
    {
        Qmissing_val = NrmStringToQuark("missing_value");
        Qfill_val = NrmStringToQuark("_FillValue");
        first = False;
    }

        /*nc_set_log_level(3);*/
        nc_set_log_level(3);

    ChunkSizeHint = 64 * blksize;
    therec = (OpenDapFileRecord*)NclCalloc(1, sizeof(OpenDapFileRecord));
    if (! therec)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        return NULL;
    }
    therec->fid = -1;
    therec->open = 0;
    therec->header_reserve_space = 0;
    therec->define_mode = 0;
    *format = _NclOPENDAP;
    setvbuf(stderr,NULL,_IONBF,0);
    return (void *) therec;
}

static void *DapOpenFile(void *rootgrp, NclQuark path, int wr_status)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) rootgrp;
    NclFileDimNode *dimnode;
    NclFileDimNode *vardimnode;
    NclFileVarNode *varnode;

    int fid;
    int ret = 0;
    int dummy = 0;
    char buffer[MAX_NAME_LENGTH];
    char buffer2[MAX_NAME_LENGTH];
    int i,j,has_scalar_dim = 0;
    long tmp_size;

    int n_atts, n_dims, n_vars;

    int has_swath;

    fprintf(stderr, "\nenter DapOpenFile, file: <%s>, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tpath: <%s>\n", NrmQuarkToString(path));

    if(NULL == grpnode)
    {
        return(NULL);
    }

    grpnode->path = path;
    grpnode->status = wr_status;
    grpnode->compress_level = 0;

    if(grpnode->open)
    {
        fid = grpnode->id;
    }
    else
    {
        ret = oc_open(NrmQuarkToString(path), &fid);
        grpnode->define_mode = 0;
        grpnode->fid = fid;
        grpnode->id  = fid;

        fprintf(stderr, "\tfile: <%s>, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tret=%d, fid=%d\n", ret, fid);
    }

    if(ret)
    { 
        printf("\tfile: %s, line: %d\n", __FILE__, __LINE__);
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "OpenDap: The specified file (%s) cannot be opened; invalid file or system error",
                  NrmQuarkToString(path));
        return(NULL);
    }

    grpnode->open = 1;
    oc_inq(fid, &n_dims, &n_vars, &n_atts);

    fprintf(stderr, "\n\tfile: <%s>, line: %d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tn_vars=%d\n", n_vars);
    fprintf(stderr, "\tn_dims=%d\n", n_dims);
    fprintf(stderr, "\tn_atts=%d\n", n_atts);

    has_swath = oc_he5_has_swath(fid);
    fprintf(stderr, "\thas_swath=%d\n", has_swath);

    grpnode->grp_rec = NULL;
    grpnode->udt_rec = NULL;
    grpnode->chunk_dim_rec = NULL;

    if(n_atts)
       grpnode->att_rec = _OpenDap_get_atts(fid, n_atts);
    else
       grpnode->att_rec = NULL;

    if(n_dims)
       grpnode->dim_rec = _OpenDap_get_dims(fid, 0);
    else
       grpnode->dim_rec = NULL;

    if(n_vars)
       grpnode->var_rec = _OpenDap_get_vars(fid, n_vars, &has_scalar_dim);
    else
       grpnode->var_rec = NULL;

#if 0
    DapGetDimVals(fid,tmp);
#endif

    CloseOrNot(grpnode,fid,0);

    fprintf(stderr, "leave DapOpenFile, file: <%s>, line: %d\n\n", __FILE__, __LINE__);

    return (void *) grpnode;
}

static void *DapCreateFile(void *rec,NclQuark path)
{
    OpenDapFileRecord *tmp = (OpenDapFileRecord*)rec;
    int fid = 0;
    int nc_ret, mode = 0;
    int format = -1;

    nc_ret = nc__create(NrmQuarkToString(path),mode,1024,&ChunkSizeHint,&fid);
    if(nc_ret == NC_NOERR)
    {
        tmp->fid = fid;
        tmp->define_mode = 1;
        tmp->format = format;
        tmp->open = 1;
        DapOpenFile(rec,path,-1);
    }
}

static void DapFreeFileRec
#if    NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
        OpenDapVarInqRecList *stepvl;
        OpenDapDimInqRecList *stepdl;

    if (rec->open)
    {
        ncclose(rec->fid);
    }
    stepal = rec->file_atts;
    while(rec->file_atts != NULL)
    {
        stepal = rec->file_atts;
        NclFree(stepal->att_inq->value);
        NclFree(stepal->att_inq);
        rec->file_atts = rec->file_atts->next;
        NclFree(stepal);
    }
    stepdl = rec->dims;
    while(rec->dims != NULL)
    {
        stepdl = rec->dims;
        NclFree(stepdl->dim_inq);
        rec->dims= rec->dims->next;
        NclFree(stepdl);
    }
    stepdl = rec->chunk_dims;
    while(rec->chunk_dims != NULL)
    {
        stepdl = rec->chunk_dims;
        NclFree(stepdl->dim_inq);
        rec->chunk_dims= rec->chunk_dims->next;
        NclFree(stepdl);
    }

    while(rec->vars != NULL)
    {
        stepvl = rec->vars;
        while(stepvl->var_inq->att_list != NULL)
    {
            stepal = stepvl->var_inq->att_list;
            NclFree(stepvl->var_inq->att_list->att_inq->value);
            NclFree(stepvl->var_inq->att_list->att_inq);
            stepvl->var_inq->att_list = stepal->next;
            NclFree(stepal);
        }
        if (stepvl->var_inq->value != NULL)
            NclFree(stepvl->var_inq->value);
        NclFree(stepvl->var_inq);
        rec->vars = rec->vars->next;
        NclFree(stepvl);
    }
    NclFree(rec->options);
    NclFree(rec);
    return;
}

static NclQuark* DapGetVarNames
#if    NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    NclQuark *out_quarks;
    OpenDapVarInqRecList *stepvl;
    int i;

    out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_vars);
    stepvl = rec->vars;
    for(i = 0; i < rec->n_vars; i++)
    {
        out_quarks[i] = stepvl->var_inq->name;
        stepvl=stepvl->next;
    }
    *num_vars = rec->n_vars;;
    return(out_quarks);
}

static NclFVarRec *DapGetVarInfo
#if    NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapVarInqRecList *stepvl;
    OpenDapDimInqRecList *stepdl;
    NclFVarRec *tmp;
    int j;

    stepvl = rec->vars;
    while(stepvl != NULL)
    {
        if(stepvl->var_inq->name == var_name)
    {
            tmp = (NclFVarRec*)NclMalloc((unsigned)sizeof(NclFVarRec));
            tmp->var_name_quark = stepvl->var_inq->name;
            tmp->var_real_name_quark = stepvl->var_inq->name;
            tmp->var_full_name_quark = stepvl->var_inq->name;
            tmp->data_type = DapMapToNcl((void*)&(stepvl->var_inq->data_type));
            tmp->num_dimensions = stepvl->var_inq->n_dims;
            for(j=0; j< stepvl->var_inq->n_dims; j++)
    {
                stepdl = rec->dims;
                while(stepdl->dim_inq->dimid != stepvl->var_inq->dim[j])
    {
                    stepdl = stepdl->next;
                }
                if(stepdl->dim_inq->dimid == -5)
    {
                    tmp->file_dim_num[j] = 0;
                }
    else if(rec->has_scalar_dim)
    {
                    tmp->file_dim_num[j] = stepdl->dim_inq->dimid + 1;
                }
    else
    {
                    tmp->file_dim_num[j] = stepdl->dim_inq->dimid;
                }
            }
            return(tmp);
        }
    else
    {
            stepvl = stepvl->next;
        }
    }
    return(NULL);
}

static NclQuark *DapGetDimNames
#if    NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    NclQuark *out_quarks;
    OpenDapDimInqRecList *stepdl;
    int i;

    out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_dims);
    stepdl = rec->dims;
    for(i = 0; i < rec->n_dims; i++)
    {
        out_quarks[i] = stepdl->dim_inq->name;
        stepdl=stepdl->next;
    }
    *num_dims = rec->n_dims;;
    return(out_quarks);
}

static NclFDimRec *DapGetDimInfo
#if    NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    NclFDimRec *tmp;
    OpenDapDimInqRecList *stepdl;

    stepdl = rec->dims;
    while(stepdl != NULL)
    {
        if(stepdl->dim_inq->name == dim_name_q)
    {
            tmp = (NclFDimRec*)NclMalloc((unsigned)sizeof(NclFDimRec));
            tmp->dim_name_quark = dim_name_q;
            tmp->dim_size = stepdl->dim_inq->size;
            tmp->is_unlimited  = stepdl->dim_inq->is_unlimited;
            return(tmp);
        }
    else
    {
            stepdl = stepdl->next;
        }
    }
    return(NULL);
}
static NclQuark *DapGetAttNames
#if    NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{    
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
    NclQuark *out_list = NULL;
    int i;

    out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_file_atts);
    stepal = rec->file_atts;
    for(i = 0; i< rec->n_file_atts; i++)
    {
        out_list[i] = stepal->att_inq->name;
        stepal = stepal->next;
    }
    *num_atts = rec->n_file_atts;
    return(out_list);
}

static NclFAttRec* DapGetAttInfo
#if    NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
    NclFAttRec *tmp;

    stepal = rec->file_atts;
    while(stepal != NULL)
    {
        if(stepal->att_inq->name == att_name_q)
    {
            tmp=(NclFAttRec*)NclMalloc((unsigned)sizeof(NclFAttRec));
            tmp->att_name_quark = att_name_q;
/*
* For convenience I make all character attributes strings (except if its the _FillValue or missing_value of a character variable - dib 2009-03-05))
*/
            if(stepal->att_inq->data_type == NC_CHAR && !(att_name_q == Qfill_val || att_name_q == Qmissing_val)) 
    {
                tmp->data_type = NCL_string;
                tmp->num_elements = 1;
            }
    else
    {
                tmp->data_type = DapMapToNcl((void*)&(stepal->att_inq->data_type));
                tmp->num_elements = stepal->att_inq->len;
            }
            return(tmp);
        }
    else
    {
            stepal = stepal->next;
        }
    }

    return(NULL);
}

static NclQuark *DapGetVarAttNames
#if    NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapVarInqRecList *stepvl;
    OpenDapAttInqRecList *stepal;
    NclQuark *out_list = NULL;    
    int i;
    *num_atts = 0;
    stepvl = rec->vars;
    while(stepvl != NULL)
    {
        if(stepvl->var_inq->name== thevar)
    {
            stepal = stepvl->var_inq->att_list;
            out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark) * stepvl->var_inq->natts);
            *num_atts = stepvl->var_inq->natts;
            for(i = 0; i< stepvl->var_inq->natts; i++)
    {
                out_list[i] = stepal->att_inq->name;
                stepal = stepal->next;
            }
            return(out_list);
        }
    else
    {
            stepvl = stepvl->next;
        }
    }
        
    return(NULL);
}

static NclFAttRec *DapGetVarAttInfo
#if    NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapVarInqRecList *stepvl;
    OpenDapAttInqRecList *stepal;
    NclFAttRec *tmp = NULL;

    stepvl = rec->vars;
    while(stepvl != NULL)
    {
        if(stepvl->var_inq->name == thevar)
    {
            stepal = stepvl->var_inq->att_list;
            while(stepal != NULL)
    {
                if(stepal->att_inq->name == theatt)
    {
                    tmp= (NclFAttRec*)NclMalloc((unsigned)
                        sizeof(NclFAttRec));
                    tmp->att_name_quark = theatt;
                    if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val))
    {

                        tmp->data_type = NCL_string;
                        tmp->num_elements = 1;
                    }
    else
    {
                        tmp->data_type = DapMapToNcl((void*)&stepal->att_inq->data_type);
                        tmp->num_elements = stepal->att_inq->len;
                    }
                    return(tmp);
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
        
    return(NULL);
}

static NclFVarRec *DapGetCoordInfo
#if    NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
    return(DapGetVarInfo(therec,thevar));
}

/*
 * this is for 1-D variables only - basically for coordinate variables.
 */
static void *DapGetCachedValue
#if    NhlNeedProto
(OpenDapVarInqRec *var_inq, long start, long finish,long stride,void* storage)
#else
(var_inq,start,finish,stride,storage)
OpenDapVarInqRec *var_inq;
long start;
long finish;
long stride;
void* storage;
#endif
{
    long i,j;
    int tsize = var_inq->data_type < 1 ? 1 : nctypelen(var_inq->data_type);

    for (j = 0, i = start; i <= finish; i += stride,j++)
    {
        memcpy(((char*)storage) + j * tsize,((char *)var_inq->value) + i * tsize,tsize);
    }
    return storage;
}

static void *DapReadVar
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
    OpenDapFileRecord *rec = (OpenDapFileRecord*) therec;
    OpenDapVarInqRecList *stepvl;
    OpenDapDimInqRecList *stepdl; 
    void *out_data;
    ng_size_t n_elem = 1;
    int fid = -1;
    int ret = -1,i;
    int nc_ret = NC_NOERR;
    int no_stride = 1;
    long count[MAX_NC_DIMS];

    stepvl = rec->vars;
    while(stepvl != NULL)
    {
        if(stepvl->var_inq->name == thevar)
    {
            if (stepvl->var_inq->value != NULL && stepvl->var_inq->n_dims == 1)
    {
                return DapGetCachedValue(stepvl->var_inq,start[0],finish[0],stride[0],storage);
            }
            for(i= 0; i< stepvl->var_inq->n_dims; i++)
    {
                int dimid;
                count[i] = (int)floor((finish[i] - start[i])/(double)stride[i]) + 1;
                n_elem *= count[i];
                if(stride[i] != 1)
    {
                    no_stride = 0;
                }
                dimid = stepvl->var_inq->dim[i];
                for (stepdl = rec->dims; stepdl !=NULL; stepdl = stepdl->next)
    {
                    if (stepdl->dim_inq->dimid == dimid) 
                        break;
                }
            }
            out_data = storage;

            if (rec->open)
    {
                fid = rec->fid;
                EndDefineModeIf(rec,fid);
            }
            else if (no_stride)
    {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&fid);
                rec->define_mode = 0;
                rec->fid = fid;
                /* printf ("got size = %d\n",ChunkSizeHint); */
            }
            else
    {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&fid);
                rec->define_mode = 0;
                rec->fid = fid;
            }
                
            if(nc_ret != NC_NOERR)
    {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "OpenDap: Could not reopen the file (%s) for reading",
                      NrmQuarkToString(rec->file_path_q));
                return(NULL);
            }

            rec->open = 1;
            if(no_stride)
    {    
                ret = ncvargetg(fid,
                    stepvl->var_inq->varid,
                    start,
                    count,
                    NULL,
                    NULL,
                    out_data);

            }
    else
    {
                ret = ncvargetg(fid,
                    stepvl->var_inq->varid,
                    start,
                    count,
                    stride,
                    NULL,
                    out_data);
            }
/*
            CloseOrNot(rec,fid,0);
*/
            if(ret == -1)
    {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: An error occurred while attempting to read variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                return(NULL);
            }
    else
    {
                return(storage);
            }
        }
    else
    {
            stepvl = stepvl->next;
        }
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
    return(NULL);
}

static void *DapReadCoord
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
    return(DapReadVar(therec,thevar,start,finish,stride,storage));
}


static void *DapReadAtt
#if    NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
    int fid;
    int ret ;
    char *tmp;
    int nc_ret;

    stepal = rec->file_atts;
    while(stepal != NULL)
    {
        if(stepal->att_inq->name == theatt)
    {
            if (stepal->att_inq->value != NULL)
    {
                if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val))
    {
                    *(string*)storage = *(string*)(stepal->att_inq->value);
                }
    else
    {
                    memcpy(storage,stepal->att_inq->value,
                           nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
                }
                return(storage);
            }
            if (rec->open)
    {
                fid = rec->fid;
            }                
            else
    {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
    {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "OpenDap: Could not reopen the file (%s) for reading",
                          NrmQuarkToString(rec->file_path_q));
                    return(NULL);
                }
                rec->open = 1;
                rec->fid = fid;
                rec->define_mode = 0;
            }
            
            if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val))
    {
                tmp = (char*)NclMalloc(stepal->att_inq->len+1);
                tmp[stepal->att_inq->len] = '\0';
                ret = ncattget(fid,NC_GLOBAL,NrmQuarkToString(theatt),tmp);
                *(string*)storage = NrmStringToQuark(tmp);
                NclFree(tmp);
            }
    else
    {
                ret = ncattget(fid,NC_GLOBAL,NrmQuarkToString(theatt),storage);
            }
            if (ret != -1) 
                return(storage);
            else
    {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Error retrieving value for global ttribute (%s) of (%s)",
                      NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
                return NULL;
            }
        }
    else
    {
            stepal = stepal->next;
        }
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Attribute (%s) is not a global attribute of file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
    return(NULL);
}

static void *DapReadVarAtt
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
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
    OpenDapVarInqRecList *stepvl;
    int fid;
    int nc_ret;
    int ret;
    char *tmp;

    stepvl = rec->vars;
    while(stepvl != NULL)
    {
        if(stepvl->var_inq->name == thevar)
    {
            stepal = stepvl->var_inq->att_list;
            while(stepal != NULL)
    {
                if(stepal->att_inq->name == theatt)
    {
                    if (stepal->att_inq->value != NULL)
    {
                        if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val))
    {
                            *(string*)storage = *(string*)(stepal->att_inq->value);
                        }
    else
    {
                            memcpy(storage,stepal->att_inq->value,
                                   nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
                        }
                        return(storage);
                    }
                    if (stepal->att_inq->virtual)
    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Error retrieving value for virtual attribute (%s) of (%s->%s)",
                              NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
                        return NULL;
                    }
                    if (rec->open)
    {
                        fid = rec->fid;
                    }
                    else
    {
                        nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&fid);

                        if(nc_ret != NC_NOERR)
    {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                  "OpenDap: Could not reopen the file (%s) for reading",
                                  NrmQuarkToString(rec->file_path_q));
                            return(NULL);
                        }
                        rec->fid = fid;
                        rec->define_mode = 0;
                        rec->open = 1;
                    }
            
                    if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt  == Qmissing_val))
    {
    
                        tmp = (char*)NclMalloc(stepal->att_inq->len + 1);
                        tmp[stepal->att_inq->len] = '\0';
                        ret = ncattget(fid,stepvl->var_inq->varid,NrmQuarkToString(theatt),tmp);
                        *(string*)storage = NrmStringToQuark(tmp);
                        NclFree(tmp);
                    }
    else
    {
                        ret = ncattget(fid,stepvl->var_inq->varid,NrmQuarkToString(theatt),storage);
                    }
/*
                    CloseOrNot(rec,fid,0);
*/
                    if(ret != -1)
                        return(storage);
                    else
    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Error retrieving value for Attribute (%s) of (%s->%s)",
                              NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
                        return NULL;
                    }
                }
    else
    {
                    stepal = stepal->next;
                }
            }
            break;
        }
    else
    {
            stepvl = stepvl->next;
        }
    }
    NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: Attribute (%s) is not a variable attribute of (%s->%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
    return(NULL);
}

static NhlErrorTypes DapDelAtt
#if     NhlNeedProto
(void *therec, NclQuark theatt)
#else 
(therec, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
    OpenDapAttInqRecList *prev;
    int fid;
    int nc_ret;
    int ret = 0;

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
                    fid = rec->fid;
                }
                else
    {
                    nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
                    if(nc_ret != NC_NOERR)
    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                              "OpenDap: Could not reopen the file (%s) for writing",
                              NrmQuarkToString(rec->file_path_q));
                        return(NhlFATAL);
                    }
                    rec->fid = fid;
                    rec->define_mode = 0;
                    rec->open = 1;
                }
                if (! rec->define_mode)
    {
                    ncredef(fid);
                    rec->define_mode = 1;
                }
                ret = ncattdel(fid,NC_GLOBAL,(const char*)NrmQuarkToString(theatt));
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
                NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: An error occurred while attempting to delete the attribute (%s) from file (%s)",
                      NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            return(NhlNOERROR);
        }
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

static NhlErrorTypes DapDelVarAtt
#if     NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else 
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList *stepal;
    OpenDapAttInqRecList *prev;
    OpenDapVarInqRecList *stepvl;
    int fid;
    int nc_ret;
    int ret = 0;

    if(rec->wr_status <= 0)
    {
        stepvl = rec->vars;
        while(stepvl != NULL)
    {
            if(stepvl->var_inq->name == thevar)
    {
                stepal = stepvl->var_inq->att_list;
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
                            fid = rec->fid;
                        }
                        else
    {
                            nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
                            if(nc_ret != NC_NOERR)
    {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                      "OpenDap: Could not reopen the file (%s) for writing",
                                      NrmQuarkToString(rec->file_path_q));
                                return(NhlFATAL);
                            }
                            rec->fid = fid;
                            rec->define_mode = 0;
                            rec->open = 1;
                        }
                        if (! rec->define_mode)
    {
                            ncredef(fid);
                            rec->define_mode = 1;
                        }
                        ret = ncattdel(fid,stepvl->var_inq->varid,(const char*)NrmQuarkToString(theatt));
                    }
                    if (! prev)
    {
                        stepvl->var_inq->att_list = stepal->next;
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
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                              "OpenDap: An error occurred while attempting to delete the attribute (%s) from variable (%s) in file (%s)",
                              NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                        return(NhlFATAL);
                    }
                    return(NhlNOERROR);
                } 
            }
    else
    {
                stepvl = stepvl->next;
            }
        } 
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

static NhlErrorTypes DapAddDim(void* therec, NclQuark thedim, int size,int is_unlimited)
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*) therec;
    int fid;
    int nc_ret;
    OpenDapDimInqRecList *stepdl;
    int ret = -1;
    int add_scalar = 0;

    if(rec->wr_status <=  0)
    {
        
        if(thedim == NrmStringToQuark("ncl_scalar"))
        {
            if (size != 1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "OpenDap: \"ncl_scalar\" is a reserved file dimension name in NCL, this name can only represent dimensions of size 1");
                return(NhlFATAL);
            }
            add_scalar = 1;
        }
        else
        {
            if (rec->open)
            {
                fid = rec->fid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "OpenDap: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    return(NhlFATAL);
                }
                rec->fid = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
            if (! rec->define_mode)
            {
                ncredef(fid);
                rec->define_mode = 1;
            }
            if(is_unlimited)
    {
                ret = ncdimdef(fid,NrmQuarkToString(thedim),NC_UNLIMITED);

            }
    else
    {
                ret = ncdimdef(fid,NrmQuarkToString(thedim),(long)size);
            }
            if(ret == -1)
    {
                return(NhlFATAL);
            }
        }
        stepdl = rec->dims;

        if (add_scalar)
    {
            rec->has_scalar_dim = 1;
            rec->dims = (OpenDapDimInqRecList*)NclMalloc(
                (unsigned) sizeof(OpenDapDimInqRecList));
            rec->dims->dim_inq = (OpenDapDimInqRec*)NclMalloc(
                (unsigned)sizeof(OpenDapDimInqRec));
            rec->dims->next = stepdl;
            rec->dims->dim_inq->dimid = -5;
            rec->dims->dim_inq->size = 1;
            rec->dims->dim_inq->is_unlimited = 0;
            rec->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
            rec->n_dims++;
        }
        else if(stepdl == NULL)
    {
            rec->dims = (OpenDapDimInqRecList*)NclMalloc((unsigned)sizeof(OpenDapDimInqRecList));
            rec->dims->dim_inq = (OpenDapDimInqRec*)NclMalloc((unsigned)sizeof(OpenDapDimInqRec));
            rec->dims->dim_inq->dimid = ret;
            rec->dims->dim_inq->name = thedim;
            if(is_unlimited)
    {
                rec->dims->dim_inq->size = (long)0;
            }
    else
    {
                rec->dims->dim_inq->size = (long)size;
            }
            rec->dims->dim_inq->is_unlimited= is_unlimited;
            rec->dims->next = NULL;
            rec->n_dims = 1;
        }
        else
    {
            while(stepdl->next != NULL)
    {
                stepdl = stepdl->next;
            }
            stepdl->next = (OpenDapDimInqRecList*)NclMalloc((unsigned)sizeof(OpenDapDimInqRecList));
            stepdl->next->dim_inq = (OpenDapDimInqRec*)NclMalloc((unsigned)sizeof(OpenDapDimInqRec));
            stepdl->next->dim_inq->dimid = ret;
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

static NhlErrorTypes DapAddVar
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
    OpenDapFileRecord* rec = (OpenDapFileRecord*)therec;
    OpenDapVarInqRecList *stepvl = NULL;
    int fid,i;
    int ret;
    int nc_ret;
    nc_type *the_data_type;
    int dim_ids[MAX_NC_DIMS];
    OpenDapDimInqRecList* stepdl = NULL;
    int add_scalar_dim = 0;

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            fid = (int) rec->fid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "OpenDap: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->fid = fid;
            rec->define_mode = 0;
            rec->open = 1;
        }
        the_data_type = DapMapFromNcl(data_type);
/*
* All dimensions are correct dimensions for the file
*/
        dim_ids[0] = -999;
        for(i = 0; i < n_dims; i++)
        {
            stepdl = rec->dims;
            while(stepdl != NULL)
            {
                if(stepdl->dim_inq->name == dim_names[i]){
                    if((n_dims > 1)&&(dim_names[i] == NrmStringToQuark("ncl_scalar")))
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return(NhlFATAL);
                    }
                    dim_ids[i] = stepdl->dim_inq->dimid;
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
                NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap: internal error adding variable");
                return(NhlFATAL);
            }
        }
        if(the_data_type != NULL)
        {
            int var_id;
            if (! rec->define_mode)
            {
                ncredef(fid);
                rec->define_mode = 1;
            }
            if((n_dims == 1)&&(dim_ids[0] == -5))
            {
                ret = nc_def_var(fid,NrmQuarkToString(thevar),*the_data_type, 0, NULL,&var_id);
            }
            else
            {
                ret = nc_def_var(fid,NrmQuarkToString(thevar),
                         *the_data_type, n_dims, dim_ids,&var_id);
            }
            if(ret < 0)
            {
                printf("file: %s, line: %d\n", __FILE__, __LINE__);
                NhlPError(NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(ret));
                NclFree(the_data_type);
                return(NhlFATAL);
            } 
            rec->n_vars++;
    
            stepvl = rec->vars;
            if(stepvl == NULL)
            {
                rec->vars = (OpenDapVarInqRecList*)NclMalloc(
                                        (unsigned)sizeof(OpenDapVarInqRecList));
                rec->vars->next = NULL;
                rec->vars->var_inq = (OpenDapVarInqRec*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRec));
                rec->vars->var_inq->varid = var_id;
                rec->vars->var_inq->name = thevar;
                rec->vars->var_inq->data_type = *the_data_type;
                rec->vars->var_inq->n_dims = n_dims;
                rec->vars->var_inq->n_chunk_dims = 0;
                rec->vars->var_inq->use_cache = 0;
                rec->vars->var_inq->natts = 0;
                rec->vars->var_inq->att_list = NULL;
                rec->vars->var_inq->value = NULL;
                for(i = 0 ; i< n_dims; i++)
                {
                    rec->vars->var_inq->dim[i] = dim_ids[i];
                    rec->vars->var_inq->chunk_dim[i] = dim_ids[i];
                }
            }
            else
            {
                while(stepvl->next != NULL)
                {
                    stepvl= stepvl->next;
                }
                stepvl->next = (OpenDapVarInqRecList*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRecList));
                stepvl->next->var_inq = (OpenDapVarInqRec*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRec));
                stepvl->next->next = NULL;
                stepvl->next->var_inq->varid = var_id;
                stepvl->next->var_inq->name = thevar;
                stepvl->next->var_inq->data_type = *the_data_type;
                stepvl->next->var_inq->n_dims = n_dims;
                stepvl->next->var_inq->n_chunk_dims = 0;
                stepvl->next->var_inq->use_cache = 0;
                stepvl->next->var_inq->natts = 0;
                stepvl->next->var_inq->att_list = NULL;
                stepvl->next->var_inq->value = NULL;
                for(i = 0 ; i< n_dims; i++)
                {
                    stepvl->next->var_inq->dim[i] = dim_ids[i];
                }
            }
            if (add_scalar_dim)
            {
                rec->has_scalar_dim = 1;
                stepdl = rec->dims;
                rec->dims = (OpenDapDimInqRecList*)NclMalloc(
                    (unsigned) sizeof(OpenDapDimInqRecList));
                rec->dims->dim_inq = (OpenDapDimInqRec*)NclMalloc(
                    (unsigned)sizeof(OpenDapDimInqRec));
                rec->dims->next = stepdl;
                rec->dims->dim_inq->dimid = -5;
                rec->dims->dim_inq->size = 1;
                rec->dims->dim_inq->is_unlimited = 0;
                rec->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
                rec->n_dims++;
            }
            NclFree(the_data_type);
            return(NhlNOERROR);
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}

#if 0
/* dib 7/13/05 I don't think this is used so let's eliminate it */
static NhlErrorTypes DapAddCoordVar
#if    NhlNeedProto
(void *therec, NclQuark thevar,NclBasicDataTypes data_type)
#else
(therec,thevar,data_type)
void *therec;
NclQuark thevar;
NclBasicDataTypes data_type;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapDimInqRecList *stepdl = NULL;
    OpenDapVarInqRecList *stepvl = NULL;
    int fid;
    int nc_ret;
    int ret,size;
    nc_type *the_data_type;

    if(rec->wr_status <= 0)
    {
        if (rec->open)
    {
            fid = rec->fid;
        }
        else
    {
            nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
    {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "OpenDap: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->fid = fid;
            rec->define_mode = 0;
            rec->open = 1;
        }
        the_data_type = DapMapFromNcl(data_type);
        if(the_data_type != NULL)
    {
            stepdl = rec->dims;
            while(stepdl != NULL )
    {
                if(stepdl->dim_inq->name == thevar){
                    if (! rec->define_mode)
    {
                        ncredef(fid);
                        rec->define_mode = 1;
                    }
                    size = stepdl->dim_inq->size;
                    ret = ncvardef(fid,NrmQuarkToString(thevar),*the_data_type,1,&size);
                    if(ret == -1)
    {
                        ncabort(fid);
                        ncclose(fid);
                        rec->fid = -1;
                        rec->open = 0;
                        NclFree(the_data_type);
                        return(NhlFATAL);
                    } 
                }
            } 
            stepvl = rec->vars;
            if(stepvl == NULL)
    {
                rec->vars = (OpenDapVarInqRecList*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRecList));
                rec->vars->next = NULL;
                rec->vars->var_inq = (OpenDapVarInqRec*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRec*));
                rec->vars->var_inq->varid = ret;
                rec->vars->var_inq->name = thevar;
                rec->vars->var_inq->data_type = *the_data_type;
                rec->vars->var_inq->n_dims = 1;
                rec->vars->var_inq->dim[0] = stepdl->dim_inq->dimid;
                rec->vars->var_inq->natts = 0;
                rec->vars->var_inq->att_list = NULL;
                rec->vars->var_inq->value = NULL;
                rec->n_vars++;
            }
    else
    {
                while(stepvl->next != NULL)
    {
                    stepvl = stepvl->next;
                }
                stepvl->next = (OpenDapVarInqRecList*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRecList));
                stepvl->next->next = NULL;
                stepvl->next->var_inq = (OpenDapVarInqRec*)NclMalloc(
                    (unsigned)sizeof(OpenDapVarInqRec*));
                stepvl->next->var_inq->varid = ret;
                stepvl->next->var_inq->name = thevar;
                stepvl->next->var_inq->data_type = *the_data_type;
                stepvl->next->var_inq->n_dims = 1;
                stepvl->next->var_inq->dim[0] = stepdl->dim_inq->dimid;
                stepvl->next->var_inq->natts = 0;
                stepvl->next->var_inq->att_list = NULL;
                stepvl->next->var_inq->value = NULL;
                rec->n_vars++;
            }
            NclFree(the_data_type);
        }
    else
    {
            ncclose(fid);
            rec->fid = -1;
            rec->open = 0;
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
    }
    return(NhlFATAL);
}
#endif

static NhlErrorTypes DapRenameDim
#if    NhlNeedProto
(void* therec, NclQuark from, NclQuark to)
#else
(therec, from, to)
void* therec;
NclQuark from;
NclQuark to;
#endif
{
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapDimInqRecList *stepdl;
    int fid;
    int ret;
    int nc_ret;

    if(to == NrmStringToQuark("ncl_scalar"))
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,"OpenDap : \"ncl_scalar\" is a reserved file dimension name in NCL: other dimensions can not be changed to it");
                return(NhlFATAL);
    }
    stepdl = rec->dims;
    while(stepdl != NULL)
    {
        if(stepdl->dim_inq->name == from)
    {
            if(stepdl->dim_inq->dimid == -5)
    {
                stepdl->dim_inq->name = to;
                return(NhlNOERROR);
            }
            if (rec->open)
    {
                fid = rec->fid;
            }
            else
    {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
    {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "OpenDap: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    return(NhlFATAL);
                }
                rec->fid = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
            if (! rec->define_mode)
    {
                ncredef(fid);
                rec->define_mode = 1;
            }

            ret = ncdimrename(fid,stepdl->dim_inq->dimid,NrmQuarkToString(to));
            if(ret == -1)
    {
                return(NhlFATAL);
            }
    else
    {
                stepdl->dim_inq->name = to;
                return(NhlNOERROR);
            }
        }
    else
    {
            stepdl = stepdl->next;
        }
    }
    return(NhlFATAL);
}

static void DapCacheAttValue
#if    NhlNeedProto
(OpenDapAttInqRec *att_inq,void *value)
#else
(att_inq,value)
    OpenDapAttInqRec *att_inq;
    void *value;
#endif
{
    if (att_inq->data_type < 1 || value == NULL)
    {
        att_inq->value = NULL;
    }
    else if (att_inq->data_type == NC_CHAR && !(att_inq->name == Qfill_val || att_inq->name == Qmissing_val))
    {
        char *tmp = NclMalloc(att_inq->len + 1);
        strncpy(tmp,value,att_inq->len);
        tmp[att_inq->len] = '\0';
        att_inq->value = NclMalloc(sizeof(NclQuark));
        *(string*)att_inq->value = NrmStringToQuark(tmp);
        NclFree(tmp);
    }
    else
    {
        att_inq->value = NclMalloc(nctypelen(att_inq->data_type) * att_inq->len);
        memcpy(att_inq->value,value,nctypelen(att_inq->data_type) * att_inq->len);
    }
    return;
}

static NhlErrorTypes DapAddAtt
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
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList* stepal;
    nc_type *the_data_type;
    int i,ret;
    int fid;
    int nc_ret;
    

    if(rec->wr_status <= 0)
    {
        the_data_type = (nc_type*)DapMapFromNcl(data_type);
        if(the_data_type != NULL)
    {
            if (rec->open)
    {
                fid = rec->fid;
            }
            else
    {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
    {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "OpenDap: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    NclFree(the_data_type);
                    return(NhlFATAL);
                }
                rec->fid = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
            if (! rec->define_mode)
    {
                ncredef(fid);
                rec->define_mode = 1;
            }
            ret = ncattput(fid,NC_GLOBAL,NrmQuarkToString(theatt),*the_data_type,n_items,values);
            if(ret != -1 )
    {
                stepal = rec->file_atts;
                if(stepal == NULL)
    {
                    rec->file_atts = (OpenDapAttInqRecList*)NclMalloc((unsigned)
                        sizeof(OpenDapAttInqRecList));
                    rec->file_atts->att_inq= (OpenDapAttInqRec*)NclMalloc((unsigned)sizeof(OpenDapAttInqRec));
                    rec->file_atts->next = NULL;
                    rec->file_atts->att_inq->att_num = 1;
                    rec->file_atts->att_inq->virtual = 0;
                    rec->file_atts->att_inq->name = theatt;
                    rec->file_atts->att_inq->data_type = *the_data_type;
                    rec->file_atts->att_inq->len = n_items;
                    DapCacheAttValue(rec->file_atts->att_inq,values);
                }
    else
    {    
                    i = 0;
                    while(stepal->next != NULL)
    {
                        stepal = stepal->next; 
                        i++;
                    }
                    stepal->next = (OpenDapAttInqRecList*)NclMalloc((unsigned)sizeof(OpenDapAttInqRecList));
                    stepal->next->att_inq = (OpenDapAttInqRec*)NclMalloc((unsigned)sizeof(OpenDapAttInqRec));
                    stepal->next->att_inq->att_num = i;
                    stepal->next->att_inq->virtual = 0;
                    stepal->next->att_inq->name = theatt;
                    stepal->next->att_inq->data_type = *the_data_type;
                    stepal->next->att_inq->len = n_items;
                    stepal->next->next = NULL;
                    DapCacheAttValue(stepal->next->att_inq,values);
                }
                rec->n_file_atts++;
                NclFree(the_data_type);
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

static NhlErrorTypes DapAddVarAtt
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
    OpenDapFileRecord *rec = (OpenDapFileRecord*)therec;
    OpenDapAttInqRecList* stepal;
    OpenDapVarInqRecList* stepvl;
    nc_type *the_data_type;
    int i;
    int fid;
    int ret;
    int nc_ret;
    
    if(rec->wr_status <= 0)
    {
        the_data_type = (nc_type*)DapMapFromNcl(data_type);
        if(the_data_type != NULL)
    {
            if (rec->open)
    {
                fid = rec->fid;
            }
            else
    {
                nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
    {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "OpenDap: Could not reopen the file (%s) for writing",
                          NrmQuarkToString(rec->file_path_q));
                    NclFree(the_data_type);
                    return(NhlFATAL);
                }
                rec->fid = fid;
                rec->define_mode = 0;
                rec->open = 1;
            }
            stepvl = rec->vars;    
            while(stepvl != NULL)
    {
                if(stepvl->var_inq->name == thevar)
    {
                    break;
                }
    else
    {
                    stepvl = stepvl->next;
                }
            }
            if (! rec->define_mode)
    {
                ncredef(fid);
                rec->define_mode = 1;
            }
            ret = ncattput(fid,stepvl->var_inq->varid,NrmQuarkToString(theatt),*the_data_type,n_items,values);
            if(ret != -1 )
    {
                stepal = stepvl->var_inq->att_list;
                if(stepal == NULL)
    {
                    stepvl->var_inq->att_list= (OpenDapAttInqRecList*)NclMalloc((unsigned)
                        sizeof(OpenDapAttInqRecList));
                    stepvl->var_inq->att_list->att_inq = (OpenDapAttInqRec*)NclMalloc((unsigned)sizeof(OpenDapAttInqRec));
                    stepvl->var_inq->att_list->next = NULL;
                    stepvl->var_inq->att_list->att_inq->att_num = 0;
                    stepvl->var_inq->att_list->att_inq->virtual = 0;
                    stepvl->var_inq->att_list->att_inq->name = theatt;
                    stepvl->var_inq->att_list->att_inq->data_type = *the_data_type;
                    stepvl->var_inq->att_list->att_inq->len = n_items;
                    DapCacheAttValue(stepvl->var_inq->att_list->att_inq,values);
                    stepvl->var_inq->natts = 1;
                }
    else
    {    
                    i = 0;
                    while(stepal->next != NULL)
    {
                        stepal = stepal->next; 
                        i++;
                    }
                    stepal->next = (OpenDapAttInqRecList*)NclMalloc((unsigned)sizeof(OpenDapAttInqRecList));
                    stepal->next->att_inq = (OpenDapAttInqRec*)NclMalloc((unsigned)sizeof(OpenDapAttInqRec));
                    stepal->next->att_inq->att_num = i;
                    stepal->next->att_inq->virtual = 0;
                    stepal->next->att_inq->name = theatt;
                    stepal->next->att_inq->data_type = *the_data_type;
                    stepal->next->att_inq->len = n_items;
                    stepal->next->next = NULL;
                    DapCacheAttValue(stepal->next->att_inq,values);
                    stepvl->var_inq->natts++ ;
                }
                NclFree(the_data_type);
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

NclFormatFunctionRec OpenDapRec =
{
/* NclInitializeFileRecFunc initialize_file_rec */ DapInitializeFileRec,
/* NclCreateFileFunc       create_file; */         DapCreateFile,
/* NclOpenFileFunc         open_file; */           DapOpenFile,
/* NclFreeFileRecFunc      free_file_rec; */       DapFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */       DapGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */        DapGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */       DapGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */        DapGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */       DapGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */        DapGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */   DapGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */    DapGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */      DapGetCoordInfo,
/* NclReadCoordFunc        read_coord; */          DapReadCoord,
/* NclReadCoordFunc        read_coord; */          NULL,
/* NclReadVarFunc          read_var; */            DapReadVar,
/* NclReadVarFunc          read_var; */            NULL,
/* NclReadAttFunc          read_att; */            NULL, /*DapReadAtt,*/
/* NclReadVarAttFunc       read_var_att; */        NULL, /*DapReadVarAtt,*/
/* NclWriteCoordFunc       write_coord; */         NULL,
/* NclWriteCoordFunc       write_coord; */         NULL,
/* NclWriteVarFunc         write_var; */           NULL,
/* NclWriteVarFunc         write_var; */           NULL,
/* NclWriteAttFunc         write_att; */           NULL,
/* NclWriteVarAttFunc      write_var_att; */       NULL,
/* NclAddDimFunc           add_dim; */             DapAddDim,
/* NclAddChunkDimFunc      add_chunk_dim; */       NULL,
/* NclRenameDimFunc        rename_dim; */          DapRenameDim,
/* NclAddVarFunc           add_var; */             DapAddVar,
/* NclAddVarChunkFunc      add_var_chunk; */       NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */ NULL,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ NULL,
/* NclAddVarFunc           add_coord_var; */       NULL,
/* NclAddAttFunc           add_att; */             DapAddAtt,
/* NclAddVarAttFunc        add_var_att; */         DapAddVarAtt,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */    DapMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */    DapMapFromNcl,
/* NclDelAttFunc           del_att; */             DapDelAtt,
/* NclDelVarAttFunc        del_var_att; */         DapDelVarAtt,
#include "NclGrpFuncs.null"
/* NclSetOptionFunc        set_option;  */         NULL
};
NclFormatFunctionRecPtr OpenDapAddFileFormat 
#if    NhlNeedProto
(void)
#else 
()
#endif
{
    return(&OpenDapRec);
}


/************************************************************************
*                                    *
*                 Copyright (C)  1994            *
*         University Corporation for Atmospheric Research        *
*                 All Rights Reserved            *
*                                    *
************************************************************************/

/*
 * $URL
 * $Rev
 * $Id
 * $Author
 * $Date
 */

/*
 * Description:    
 */

#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <string.h>
#include <strings.h>
#include <dirent.h>
#include <stdlib.h>
#include <assert.h>

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
#include "NclVar.h"
#include "NclFile.h"
#include "NclAdvancedFile.h"
#include "NclList.h"
#include "VarSupport.h"
#include "ListSupport.h"
#include "NclData.h"
#include "AdvancedFileSupport.h"

#include <math.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <netcdf.h>

#define NETCDF_DEBUG    0

#ifndef MAX_NCL_NAME_LENGTH
#define MAX_NCL_NAME_LENGTH    256
#endif

#include "NclAdvancedFileStructure.h"

static ng_usize_t ChunkSizeHint;

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

static NhlErrorTypes NC4AddVar(void* therec, NclQuark thevar,
                               NclBasicDataTypes data_type, int n_dims,
                               NclQuark *dim_names, long *dim_sizes);
NhlErrorTypes NC4AddVarChunk(void* therec, NclQuark thevar,
                             int n_chunk_dims, ng_size_t *chunk_dims);
void StartNC4DefineMode(NclFileGrpNode *rootgrp, int id);
void EndNC4DefineMode(NclFileGrpNode *rootgrp, int id);
void NC4GetAttrVal(int ncid, int si, NclFileAttNode *attnode);
void NC4GetDimVals(int ncid, NclFileGrpNode *grpnode);
void CloseOrSync(NclFileGrpNode *rootgrp, int id, int sync);
void *NC4OpenFile(void *rootgrp, NclQuark path, int status);
void _NC4_get_grpnode(int pid, int gid, NclQuark pn, NclFileGrpNode *parentgrpnode, NclFileGrpNode *grpnode);

NclFileGrpRecord *_NC4_get_grprec(int gid, int unlimited_dim_idx, NclFileGrpNode *parentgrpnode);
NclFileVarRecord *_NC4_get_vars(NclFileGrpNode *grpnode, int n_vars, int *has_scalar_dim,
                                int unlimited_dim_idx, char *grpnamestr);
NclFileDimRecord *_NC4_get_dims(int gid, int n_dims, int unlimited_dim_idx);
NclFileAttRecord *_NC4_get_atts(int gid, int aid, int n_atts);
NclFileUDTRecord *_NC4_get_udts(int gid, int uid, int n_udts);

void _NC4_add_udt(NclFileUDTRecord **rootudtrec,
                  int gid, int uid, NclQuark name,
                  int ncl_class, nc_type base_nc_type,
                  size_t size, size_t nfields, 
                  NclQuark *mem_name, NclBasicDataTypes *mem_type);

void check_err(const int stat, const int line, const char *file)
{
    if(stat != NC_NOERR)
    {
        fprintf(stderr, "line %d of %s: %s\n", line, file, 
                         nc_strerror(stat));
        exit(-1);
    }
}

static NclBasicDataTypes NC4MapToNcl(void* the_type)
{
    switch(*(nc_type*)the_type)
    {
        case NC_CHAR:
            return(NCL_char);
        case NC_BYTE:
            return(NCL_byte);
        case NC_UBYTE:
            return(NCL_ubyte);
        case NC_SHORT:
            return(NCL_short);
        case NC_USHORT:
            return(NCL_ushort);
        case NC_INT:
            return(NCL_int);
        case NC_UINT:
            return(NCL_uint);
          /*
           *case NC_LONG:
           *    return(NCL_long);
           *case NC_ULONG:
           *    return(NCL_ulong);
           */
        case NC_UINT64:
            return(NCL_uint64);
        case NC_INT64:
            return(NCL_int64);
        case NC_FLOAT:
            return(NCL_float);
        case NC_DOUBLE:
            return(NCL_double);
        case NC_STRING:
            return(NCL_string);
        case NC_VLEN:
            return(NCL_list);
        case NC_OPAQUE:
            return(NCL_opaque);
        case NC_ENUM:
            return(NCL_enum);
        case NC_COMPOUND:
            return(NCL_compound);
        default:
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tthe_type = %d\n", *(nc_type*)the_type);
           *fprintf(stderr, "\tUnknown NetCDF4 type.\n");
           */
            return(NCL_none);
    }
}

static void *NC4MapFromNcl(NclBasicDataTypes the_type)
{
    void *out_type = (void*)NclMalloc((unsigned)sizeof(nc_type));;

    switch(the_type)
    {
        case NCL_char:
             *(nc_type*)out_type = NC_CHAR;
             break;
        case NCL_byte:
             *(nc_type*)out_type = NC_BYTE;
             break;
        case NCL_ubyte:
             *(nc_type*)out_type = NC_UBYTE;
             break;
        case NCL_short:
             *(nc_type*)out_type = NC_SHORT;
             break;
        case NCL_ushort:
             *(nc_type*)out_type = NC_USHORT;
             break;
        case NCL_int:
        case NCL_logical:
             *(nc_type*)out_type = NC_INT;
             break;
        case NCL_uint:
             *(nc_type*)out_type = NC_UINT;
             break;
        case NCL_ulong:
             if(_NclSizeOf(NCL_ulong) == _NclSizeOf(NCL_uint64))
                 *(nc_type*)out_type = NC_UINT64;
             else
                 *(nc_type*)out_type = NC_UINT;
             break;
        case NCL_long:
             if(_NclSizeOf(NCL_long) == _NclSizeOf(NCL_int64))
                 *(nc_type*)out_type = NC_INT64;
             else
                 *(nc_type*)out_type = NC_INT;
             break;
        case NCL_uint64:
             *(nc_type*)out_type = NC_UINT64;
             break;
        case NCL_string:
             *(nc_type*)out_type = NC_STRING;
             break;
        case NCL_int64:
             *(nc_type*)out_type = NC_INT64;
             break;
        case NCL_float:
             *(nc_type*)out_type = NC_FLOAT;
             break;
        case NCL_double:
             *(nc_type*)out_type = NC_DOUBLE;
             break;
        case NCL_list:
             *(nc_type*)out_type = NC_VLEN;
             break;
        case NCL_opaque:
             *(nc_type*)out_type = NC_OPAQUE;
             break;
        case NCL_enum:
             *(nc_type*)out_type = NC_ENUM;
             break;
        case NCL_compound:
             *(nc_type*)out_type = NC_COMPOUND;
             break;
        default:
             NclFree(out_type);
             out_type = NULL;
             fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tthe_type = %d\n", the_type);
             fprintf(stderr, "\tUnknown NCL type.\n");
    }
    return(out_type);
}

static int NC4InitializeOptions(NclFileGrpNode *grpnode)
{
    NCLOptions *options;
    int n = 0;

    if(Ncl_NUMBER_OF_FILE_OPTIONS == grpnode->n_options)
        return 0;

    grpnode->n_options = Ncl_NUMBER_OF_FILE_OPTIONS;

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpnode->n_options = %d\n", grpnode->n_options);
   */
    
    options = (NCLOptions *)NclCalloc(grpnode->n_options, sizeof(NCLOptions));
    if(NULL == options)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        return 1;
    }

    for(n = 0; n < grpnode->n_options; ++n)
    {
        options[n].name = -1;
        options[n].type = -1;
        options[n].size = 0;
        options[n].values = NULL;
    }

    options[Ncl_PREFILL].name = NrmStringToQuark("prefill");
    options[Ncl_PREFILL].type = NCL_logical;
    options[Ncl_PREFILL].size = 1;
    options[Ncl_PREFILL].values = (void *) NclCalloc(1, _NclSizeOf(NCL_logical));
    *(int *)options[Ncl_PREFILL].values = 1;
    options[Ncl_DEFINE_MODE].name = NrmStringToQuark("definemode");
    options[Ncl_DEFINE_MODE].type = NCL_logical;
    options[Ncl_DEFINE_MODE].size = 1;
    options[Ncl_DEFINE_MODE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_logical));
    *(int *)options[Ncl_DEFINE_MODE].values = 0;
    options[Ncl_HEADER_RESERVE_SPACE].name = NrmStringToQuark("headerreservespace");
    options[Ncl_HEADER_RESERVE_SPACE].type = NCL_int;
    options[Ncl_HEADER_RESERVE_SPACE].size = 1;
    options[Ncl_HEADER_RESERVE_SPACE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_HEADER_RESERVE_SPACE].values = 0;
    options[Ncl_SUPPRESS_CLOSE].name = NrmStringToQuark("suppressclose");
    options[Ncl_SUPPRESS_CLOSE].type = NCL_int;
    options[Ncl_SUPPRESS_CLOSE].size = 1;
    options[Ncl_SUPPRESS_CLOSE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_SUPPRESS_CLOSE].values = 1;
    options[Ncl_FORMAT].name = NrmStringToQuark("format");
    options[Ncl_FORMAT].type = NCL_string;
    options[Ncl_FORMAT].size = 1;
    options[Ncl_FORMAT].values = (void *) NclCalloc(1, _NclSizeOf(NCL_string));
    *(NrmQuark *)options[Ncl_FORMAT].values = NrmStringToQuark("netcdf4");
    options[Ncl_MISSING_TO_FILL_VALUE].name = NrmStringToQuark("missingtofillvalue");
    options[Ncl_MISSING_TO_FILL_VALUE].type = NCL_int;
    options[Ncl_MISSING_TO_FILL_VALUE].size = 1;
    options[Ncl_MISSING_TO_FILL_VALUE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_MISSING_TO_FILL_VALUE].values = 1;

    options[Ncl_SHUFFLE].name = NrmStringToQuark("shuffle");
    options[Ncl_SHUFFLE].type = NCL_int;
    options[Ncl_SHUFFLE].size = 1;
    options[Ncl_SHUFFLE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_SHUFFLE].values = 1;

    options[Ncl_ADVANCED_FILE_STRUCTURE].name = NrmStringToQuark("filestructure");
    options[Ncl_ADVANCED_FILE_STRUCTURE].type = NCL_string;
    options[Ncl_ADVANCED_FILE_STRUCTURE].size = 1;
    options[Ncl_ADVANCED_FILE_STRUCTURE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_string));
    *(NrmQuark *)options[Ncl_ADVANCED_FILE_STRUCTURE].values = NrmStringToQuark("advanced");

    options[Ncl_COMPRESSION_LEVEL].name = NrmStringToQuark("compressionlevel");
    options[Ncl_COMPRESSION_LEVEL].type = NCL_int;
    options[Ncl_COMPRESSION_LEVEL].size = 1;
    options[Ncl_COMPRESSION_LEVEL].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_COMPRESSION_LEVEL].values = 0;

    options[Ncl_USE_CACHE].name = NrmStringToQuark("usecache");
    options[Ncl_USE_CACHE].type = NCL_int;
    options[Ncl_USE_CACHE].size = 1;
    options[Ncl_USE_CACHE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_USE_CACHE].values = 1;

    options[Ncl_CACHE_SIZE].name = NrmStringToQuark("cachesize");
    options[Ncl_CACHE_SIZE].type = NCL_int;
    options[Ncl_CACHE_SIZE].size = 1;
    options[Ncl_CACHE_SIZE].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_CACHE_SIZE].values = 3200000;

    options[Ncl_CACHE_NELEMS].name = NrmStringToQuark("cachenelems");
    options[Ncl_CACHE_NELEMS].type = NCL_int;
    options[Ncl_CACHE_NELEMS].size = 1;
    options[Ncl_CACHE_NELEMS].values = (void *) NclCalloc(1, _NclSizeOf(NCL_int));
    *(int *)options[Ncl_CACHE_NELEMS].values = 1009;

    options[Ncl_CACHE_PREEMPTION].name = NrmStringToQuark("cachepreemption");
    options[Ncl_CACHE_PREEMPTION].type = NCL_float;
    options[Ncl_CACHE_PREEMPTION].size = 1;
    options[Ncl_CACHE_PREEMPTION].values = (void *) NclCalloc(1, _NclSizeOf(NCL_float));
    *(float *)options[Ncl_CACHE_PREEMPTION].values = 0.25;

    grpnode->options = options;
    return 0;
}

void _NC4_add_udt(NclFileUDTRecord **rootudtrec,
                  int gid, int uid, NclQuark name,
                  int ncl_class, nc_type base_nc_type,
                  size_t size, size_t nfields,
                  NclQuark *mem_name, NclBasicDataTypes *mem_type)
{
    NclFileUDTRecord *udtrec = *rootudtrec;
    NclFileUDTNode   *udtnode;
    int n = 0;

  /*
   *fprintf(stderr, "\nEnter _NC4_add_udt, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid: %d, uid: %d, name: <%s>\n", gid, uid, NrmQuarkToString(name));
   */

    if(NULL == udtrec)
    {
        udtrec = _NclFileUDTAlloc(1);
        assert(udtrec);
        *rootudtrec = udtrec;

        udtrec->gid = gid;
        udtrec->uid = uid;
	udtrec->n_udts = 0;  /* this gets bumped up below */
    }
  
    if(udtrec->n_udts >= udtrec->max_udts)
    {
        _NclFileUDTRealloc(udtrec);
    }

    udtnode = &(udtrec->udt_node[udtrec->n_udts]);

    udtnode->id = uid;
    udtnode->name = name;
    udtnode->type = base_nc_type;
    udtnode->size = size;
    udtnode->ncl_class = ncl_class;
    udtnode->max_fields = nfields;
    udtnode->n_fields = nfields;

    udtnode->mem_name = (NclQuark *)NclCalloc(nfields, sizeof(NclQuark));
    assert(udtnode->mem_name);
    udtnode->mem_type = (NclBasicDataTypes *)NclCalloc(nfields, sizeof(NclBasicDataTypes));
    assert(udtnode->mem_type);

    for(n = 0; n < nfields; n++)
    {
        udtnode->mem_name[n] = mem_name[n];
        udtnode->mem_type[n] = mem_type[n];
    }

    udtrec->n_udts++;
  /*
   *fprintf(stderr, "Leave _NC4_add_udt, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
}

static void *NC4InitializeFileRec(NclFileFormat *format)
{
    static int first = True;
    NclFileGrpNode *grpnode = NULL;
    ng_size_t blksize = getpagesize();

    if (first)
    {
        Qmissing_val = NrmStringToQuark("missing_value");
        Qfill_val = NrmStringToQuark("_FillValue");
        first = False;
    }

    /*nc_set_log_level(3);*/
    nc_set_log_level(3);

    ChunkSizeHint = 2 * blksize;

    grpnode = (NclFileGrpNode *)NclCalloc(1, sizeof(NclFileGrpNode));
    assert(grpnode);

    grpnode->gid = -1;
    grpnode->fid = -1;
    grpnode->pid = -1;
    grpnode->name = NrmStringToQuark("/");
    grpnode->pname = -1;
    grpnode->real_name = NrmStringToQuark("/");
    grpnode->path = -1;
    grpnode->extension = -1;

    grpnode->open = 0;
    grpnode->header_reserve_space = 0;
    grpnode->define_mode = 0;
    grpnode->other_src = NULL;
    grpnode->parent = NULL;

    NC4InitializeOptions(grpnode);

    *format = _NclNETCDF4;
    setvbuf(stderr,NULL,_IONBF,0);
    return (void *) grpnode;
}

static void *NC4CreateFile(void *rootgrp,NclQuark path)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)rootgrp;
    int fid = 0;
    int nc_ret, mode;
    int format = 4;

  /*
   *NC4InitializeOptions(grpnode);
   */

    if (*(NrmQuark *)(grpnode->options[Ncl_FORMAT].values) == 
              NrmStringToQuark("classic"))
    {
        mode = (NC_NOCLOBBER);
        format = 1;
    }
    else if ((*(NrmQuark *)(grpnode->options[Ncl_FORMAT].values) == 
         NrmStringToQuark("largefile")) ||
        (*(NrmQuark*)(grpnode->options[Ncl_FORMAT].values) == 
         NrmStringToQuark("64bitoffset")))
    {
        mode = (NC_NOCLOBBER|NC_64BIT_OFFSET);
        format = 2;
    }
    else if (*(NrmQuark *)(grpnode->options[Ncl_FORMAT].values) == 
              NrmStringToQuark("netcdf4classic"))
    {
        mode = (NC_NOCLOBBER|NC_CLASSIC_MODEL);
        format = 3;
    }
    else if (*(NrmQuark *)(grpnode->options[Ncl_FORMAT].values) == 
              NrmStringToQuark("netcdf4"))
    {
        mode = (NC_NETCDF4);
        format = 4;
    }
    else
    {
        mode = NC_NETCDF4;
        format = 4;
    }

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tNC4CreateFile format = %d\n", format);

   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\t(NrmQuark)(grpnode->options[Ncl_FORMAT].values) = %s\n",
   *                 NrmQuarkToString((NrmQuark)(*(NclQuark *)grpnode->options[Ncl_FORMAT].values)));
   *fprintf(stderr, "\tNC4CreateFile format = %d\n", format);
   */

    nc_ret = nc__create(NrmQuarkToString(path),mode,2048,&ChunkSizeHint,&fid);

    if(nc_ret == NC_NOERR)
    {
        grpnode->fid = fid;
        grpnode->gid = fid;
        grpnode->file_format = _NclNETCDF4;
        grpnode->format = format;
        grpnode->open = 1;
        grpnode->define_mode = 1;

        CloseOrSync(grpnode, fid, 0);

        return ((void *)grpnode);
    }

    return(NULL);
}

int set_compound_attnode(int ncid, int aid, NclFileAttNode **thenode)
{
    NclFileAttNode *attnode = *thenode;
    NclFileCompoundRecord *comprec;
    NclFileCompoundNode   *compnode;
    char buffer[NC_MAX_NAME + 2];

    nc_type ftype;
    int rank;
    int *sides;
    int i, fidx;

    size_t alen;
    size_t offset;
    size_t size;
    size_t nfields;
    nc_type xtype;
    nc_type base_nc_type;
    int ncl_class;

    void *data;

    int rc = -1;
  /*
   *fprintf(stderr, "\nEnter set_compound_attnode, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    xtype = attnode->the_nc_type;
    alen  = attnode->n_elem;

    nc_inq_user_type(ncid, xtype, buffer, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_typep: %d, nfieldsp: %d, classp: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_COMPOUND:
             rc = 0;
             break;
        default:
             fprintf(stderr, "\nset_compound_attnode file: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tWe Thought It Was Compound Data, But NOT.\n");
             rc = -1;
             break;
    }

    comprec = _NclFileCompoundAlloc(nfields);
    comprec->name = NrmStringToQuark(buffer);
    comprec->size = size;
    comprec->nfields = nfields;
    comprec->type = NC_COMPOUND;
    comprec->xtype = xtype;
    comprec->base_nc_type = base_nc_type;

    data = (void *) NclCalloc(alen * comprec->size, sizeof(void));
    assert(data);

    nc_get_att(ncid, aid, NrmQuarkToString(attnode->name), data);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tncid = %d, aid = %d\n", ncid, aid);
   *fprintf(stderr, "\tname: <%s>\n", NrmQuarkToString(attnode->name));
   */

    for(fidx = 0; fidx < nfields; fidx++)
    {

        compnode = &(comprec->compnode[fidx]);
        
        sides = NULL;
        nc_inq_compound_field(ncid, xtype, fidx, buffer,
                          &offset, &ftype, &rank, sides);

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tfidx: %d, offset: %ld\n", fidx, (long)offset);
       *fprintf(stderr, "\trank: %d\n", rank);
       *fprintf(stderr, "\tftype: %d\n", (long)ftype);
       *fprintf(stderr, "\tname: <%s>\n", buffer);
       *fprintf(stderr, "\tftype = %d, NC_CHAR = %d, NC_STRING = %d\n", ftype, NC_CHAR, NC_STRING);
       */

        if(rank > 0)
        {
            sides = (int *) NclCalloc(rank, sizeof(int));

            nc_inq_compound_field(ncid, xtype, fidx, NULL,
                                  NULL, NULL, NULL, sides);
        }

        compnode->the_nc_type = ftype;
        compnode->type = NC4MapToNcl(&ftype);
        compnode->name = NrmStringToQuark(buffer);
        compnode->offset = offset;
        compnode->rank = rank;
        compnode->sides = sides;
        compnode->nvals = 1;

      /*
       *nc_inq_compound_size(ncid, xtype, &size);
       */

        for(i = 0; i < rank; i++)
        {
            compnode->nvals *= sides[i];
        }

        switch(ftype)
        {
            case NC_STRING:
                 {
                    size_t clen;
                    NclQuark *qv;
                    char  *carr;
                    void  *vp = data + offset;
                    clen = 1 + strlen(((char **)vp)[0]);
                    carr = NclCalloc(clen, sizeof(char));
                    assert(carr);
                    qv = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
                    assert(qv);
                    snprintf(carr, clen, "%s", ((char **)vp)[0]);
                    *qv = NrmStringToQuark(carr);
                    compnode->value = (void *)qv;

                  /*
                   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tcarr: <%s>\n", carr);
                   */

                    free(carr);
                 }
                 break;
            default:
                 fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                 fprintf(stderr, "\tWe DO NOT KNOW how to handle ftype <%d> yet.\n\n", ftype);
                 exit ( -1 );
                 break;
        }
    }

    attnode->value = (void *)comprec;
    attnode->is_opaque = 0;
    attnode->is_vlen = 0;
    attnode->is_enum = 0;
    attnode->is_compound = 1;

  /*
   *fprintf(stderr, "Leave set_compound_attnode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return (rc);
}

int set_vlen_attnode(int ncid, int aid, NclFileAttNode **thenode)
{
    NclFileAttNode *attnode = *thenode;
    NclFileVlenRecord *vlenrec;

    int typeid;
    size_t size;
    nc_type base_nc_type;
    char name[NC_MAX_NAME + 1];
    char udt_name[NC_MAX_NAME + 1];
    int  ncl_class;
    int i, j;
    size_t nvl;
    size_t wlen = 1;
    nc_vlen_t *values;
    
    int rc = -1;

  /*
   *fprintf(stderr, "\nEnter set_vlen_attnode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tncid = %d, aid = %d\n", ncid, aid);
   */

    strcpy(name, NrmQuarkToString(attnode->name));

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tattnode->name: <%s>\n", name);
   */

    rc = nc_inq_att(ncid, aid, name, &typeid, &nvl);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\ttypeid = %d, nvl = %d\n", typeid, nvl);
   */

    rc = nc_inq_user_type(ncid, typeid, udt_name, &size, &base_nc_type, NULL, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbase_nc_type = %d, size = %d, udt_name = <%s>, ncl_class = %d\n",
   *                   base_nc_type, (int)size, udt_name, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_VLEN:
             rc = 0;
             break;
        default:
             fprintf(stderr, "\nset_vlen_attnode file: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tWe Thought It Was VLEN Data, But NOT.\n");
             rc = -1;
             break;
    }

    vlenrec = (NclFileVlenRecord *)NclCalloc(1, sizeof(NclFileVlenRecord));
    assert(vlenrec);
    vlenrec->vs = (int *)NclCalloc(nvl, sizeof(int));
    assert(vlenrec->vs);
    vlenrec->ve = (int *)NclCalloc(nvl, sizeof(int));
    assert(vlenrec->ve);

    vlenrec->name = NrmStringToQuark(name);
    vlenrec->max_vlens = nvl;
    vlenrec->n_vlens = nvl;
    vlenrec->xtype = typeid;
    vlenrec->base_nc_type = base_nc_type;
    vlenrec->ncl_class = ncl_class;
    vlenrec->size = size;

    vlenrec->type = NC4MapToNcl(&base_nc_type);

    values = (nc_vlen_t *)NclCalloc(nvl, sizeof(nc_vlen_t));
    assert(values);

    nc_get_att(ncid, aid, name, values);

    wlen = 0;
    for(i = 0; i < nvl; i++)
    {
      /*
       *fprintf(stderr, "\n\tvalues[%d].len = %d\n", i, values[i].len);
       */

        wlen += values[i].len;

        if(i)
        {
           vlenrec->vs[i] = vlenrec->ve[i-1];
        }
        else
        {
           vlenrec->vs[i] = 0;
        }
        vlenrec->ve[i] = wlen;

      /*
       *fprintf(stderr, "\twlen = %d\n", wlen);
       *fprintf(stderr, "\tvs[%d] = %d, ve[%d] = %d\n", i, vlenrec->vs[i], i, vlenrec->ve[i]);

       *for(j = 0; j < values[i].len; j++)
       *{
       *    iptr = (int *)values[i].p;
       *    fprintf(stderr, "\tvalues[%d].p = %d\n", j, iptr[j]);
       *}
       */
    }

    vlenrec->values = (void *)NclCalloc(wlen * _NclSizeOf(vlenrec->type), sizeof(void));
    assert(vlenrec->values);

    for(i = 0; i < nvl; i++)
    {
        j = vlenrec->vs[i] * _NclSizeOf(vlenrec->type);
        wlen = values[i].len * _NclSizeOf(vlenrec->type);
        memcpy(vlenrec->values + j, (void *)(values[i].p), wlen);
    }
    nc_free_vlens(nvl, values);
    free(values);

    attnode->is_opaque = 0;
    attnode->is_enum = 0;
    attnode->is_vlen = 1;
    attnode->is_compound = 0;
    attnode->value = (void *)vlenrec;

  /*
   *fprintf(stderr, "Leave set_vlen_attnode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (rc);
}

int set_opaque_attnode(int ncid, int aid, NclFileAttNode **thenode)
{
    NclFileAttNode *attnode = *thenode;
    NclFileOpaqueRecord *opaquerec;

    int typeid;
    size_t size;
    nc_type base_nc_type;
    char name[NC_MAX_NAME + 1];
    char buffer[NC_MAX_NAME + 1];
    char udt_name[NC_MAX_NAME + 1];
    int  ncl_class;
    size_t nv;
    size_t wlen = 1;
    int rc = -1;
  /*
   *fprintf(stderr, "\nEnter set_opaque_attnode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tncid = %d, aid = %d\n", ncid, aid);
   */

    strcpy(name, NrmQuarkToString(attnode->name));

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tattnode->name: <%s>\n", name);
   */

    rc = nc_inq_att(ncid, aid, name, &typeid, &nv);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\ttypeid = %d, nv = %d\n", typeid, nv);
   */

    rc = nc_inq_user_type(ncid, typeid, udt_name, &size, &base_nc_type, NULL, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbase_nc_type = %d, size = %d, udt_name = <%s>, ncl_class = %d\n",
   *                   base_nc_type, (int)size, udt_name, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_OPAQUE:
             rc = 0;
             break;
        default:
             fprintf(stderr, "\nset_opaque_attnode file: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tWe Thought It Was OPAQUE Data, But NOT.\n");
             rc = -1;
             break;
    }

    rc = nc_inq_opaque(ncid, typeid, buffer, &size);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbuffer: <%s>, size = %d\n", buffer, size);
   */

    opaquerec = (NclFileOpaqueRecord *)NclCalloc(1, sizeof(NclFileOpaqueRecord));
    assert(opaquerec);

    opaquerec->name = NrmStringToQuark(name);
    opaquerec->max_opaques = nv;
    opaquerec->n_opaques = nv;
    if(base_nc_type)
        opaquerec->type = NC4MapToNcl(&base_nc_type);
    else
        opaquerec->type = NCL_byte;
    opaquerec->base_nc_type = base_nc_type;
    opaquerec->xtype = typeid;
    opaquerec->size = size;
  
    wlen = nv * size;
    opaquerec->values = (void *) NclCalloc(wlen, sizeof(void));
    assert(opaquerec->values);

    nc_get_att(ncid, aid, name, opaquerec->values);

    attnode->is_opaque = 1;
    attnode->is_enum = 0;
    attnode->is_vlen = 0;
    attnode->is_compound = 0;
    attnode->type = NC4MapToNcl(&base_nc_type);
    attnode->n_elem = wlen;

    attnode->value = (void *)opaquerec;

  /*
   *{
   *    int n = 0;
   *    char *cptr = (char *)opaquerec->values;
   *    fprintf(stderr, "\tvalues = {");
   *    for(i = 0; i < nv; i++)
   *    {
   *        if(i)
   *            fprintf(stderr, ", {");
   *        else
   *            fprintf(stderr, " {");

   *        for(j = 0; j < size; j++)
   *        {
   *            if(j)
   *                fprintf(stderr, ", %c", cptr[n]);
   *            else
   *                fprintf(stderr, "%c", cptr[n]);
   *            n++;
   *        }

   *        fprintf(stderr, "}");
   *    }
   *    fprintf(stderr, " }\n");
   *}
   */

  /*
   *fprintf(stderr, "Leave set_opaque_attnode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (rc);
}

int set_enum_attnode(int ncid, int aid, NclFileAttNode **thenode)
{
    NclFileAttNode *attnode = *thenode;
    NclFileEnumRecord *enumrec;
    NclFileEnumNode   *enumnode;

    int typeid;
    nc_type base_nc_type;
    char buffer[NC_MAX_NAME + 1];
    char name[NC_MAX_NAME + 1];
    char udt_name[NC_MAX_NAME + 1];
    int ncl_class;
    int i;
    size_t na;
    int rc = -1;

    size_t size;
    size_t base_size;
    size_t num_members;

    void *vv;
  /*
   *fprintf(stderr, "\nEnter set_enum_attnode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tncid = %d, aid = %d\n", ncid, aid);
   */

    strcpy(name, NrmQuarkToString(attnode->name));

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tattnode->name: <%s>\n", name);
   */

    rc = nc_inq_att(ncid, aid, name, &typeid, &na);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\ttypeid = %d, na = %d\n", typeid, na);
   */

    rc = nc_inq_user_type(ncid, typeid, udt_name, &size, &base_nc_type, NULL, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbase_nc_type = %d, size = %d, udt_name = <%s>, ncl_class = %d\n",
   *                   base_nc_type, (int)size, udt_name, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_ENUM:
             rc = 0;
             break;
        default:
             fprintf(stderr, "\nset_enum_attnode file: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tWe Thought It Was ENUM Data, But NOT.\n");
             rc = -1;
             break;
    }

    rc = nc_inq_enum(ncid, typeid, buffer, &base_nc_type,
                     &base_size, &num_members);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbuffer: <%s>, base_size = %d, base_nc_type = %d, num_members = %d\n",
   *                   buffer, base_size, base_nc_type, num_members);
   */

    enumrec = _NclFileEnumAlloc(num_members);

    enumrec->name = NrmStringToQuark(udt_name);
    enumrec->type = NC4MapToNcl(&base_nc_type);
    enumrec->size = num_members;
    enumrec->xtype = typeid;
    enumrec->base_nc_type = base_nc_type;
  
    vv = (void *)NclCalloc(1, _NclSizeOf(enumrec->type));
    assert(vv);

    for(i = 0; i < num_members; i++)
    {
        rc = nc_inq_enum_member(ncid, typeid, i, buffer, vv);
        enumnode = &(enumrec->enum_node[i]);
        enumnode->name = NrmStringToQuark(buffer);
        enumnode->value = *(long long *)vv;

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tMember %d: name <%s>, value = %lld\n",
       *                   i, buffer, *(long long *)vv);

       *rc = nc_inq_enum_ident(ncid, typeid, enumnode->value, name);

       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tMember %d: name <%s>, value = %lld\n",
       *                   i, name, enumnode->value);
       */
    }

    NclFree(vv);

    enumrec->values = (void *)NclCalloc(na, _NclSizeOf(enumrec->type));
    nc_get_att(ncid, aid, name, enumrec->values);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tenumrec->values = %d\n", *(int*)enumrec->values);
   */

    attnode->is_enum = 1;
    attnode->is_opaque = 0;
    attnode->is_vlen = 0;
    attnode->is_compound = 0;
    attnode->type = NCL_enum;
    attnode->n_elem = 1;

    attnode->value = (void *)enumrec;

  /*
   *attnode->type = NC4MapToNcl(&base_nc_type);
   *fprintf(stderr, "Leave set_enum_attnode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (rc);
}

int set_special_attnode(int ncid, int aid, NclFileAttNode **thenode)
{
    NclFileAttNode *attnode = *thenode;
    char buffer[NC_MAX_NAME + 2];

    size_t size;
    size_t nfields;
    nc_type xtype;
    nc_type base_nc_type;
    int ncl_class;

    int rc = -1;

  /*
   *fprintf(stderr, "\nEnter set_special_attnode, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    xtype = attnode->the_nc_type;

  /*
   *fprintf(stderr, "\tattnode->name: <%s>\n", NrmQuarkToString(attnode->name));
   *fprintf(stderr, "\tattnode->the_nc_type: %d\n", xtype);
   */

    nc_inq_user_type(ncid, xtype, buffer, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbuffer: <%s>\n", buffer);

   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_type: %d, nfields: %d, ncl_class: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_VLEN:
             rc = set_vlen_attnode(ncid, aid, thenode);
             break;
        case NC_OPAQUE: 
             rc = set_opaque_attnode(ncid, aid, thenode);
             break;
        case NC_ENUM: 
             rc = set_enum_attnode(ncid, aid, thenode);
             break;
        case NC_COMPOUND:
             rc = set_compound_attnode(ncid, aid, thenode);
             break;
        default:
             fprintf(stderr, "\nset_special_attnode file: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tWe Thought It Was One of (Vlen, Opaque, Enum, or Compound), But NOT.\n");
             exit ( -1 );
             break;
    }

  /*
   *fprintf(stderr, "Leave set_special_attnode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return (rc);
}

NclFileCompoundRecord *get_nc4_compoundrec(int ncid, nc_type xtype, NrmQuark **componentnamesptr)
{
    NclFileCompoundRecord *comprec;
    NclFileCompoundNode   *compnode;
    char buffer[NC_MAX_NAME + 2];

    nc_type ftype;
    int rank;
    int *sides;
    int i, fidx;

    size_t offset;
    size_t size;
    size_t nfields;
    nc_type base_nc_type;
    int ncl_class;

    NrmQuark *componentnames;

  /*
   *fprintf(stderr, "\nEnter get_nc4_compoundrec, file: %s, line: %d\n", __FILE__, __LINE__);
   */

    nc_inq_user_type(ncid, xtype, buffer, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_typep: %d, nfieldsp: %d, classp: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_COMPOUND:
             break;
        default:
             fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tncl_class  %d\n", ncl_class);
             fprintf(stderr, "\tWe Thought It Was Compound Data, But NOT.\n");
             exit ( -1 );
             break;
    }

    comprec = _NclFileCompoundAlloc(nfields);
    comprec->name = NrmStringToQuark(buffer);
    comprec->size = size;
    comprec->nfields = nfields;
    comprec->type = NC_COMPOUND;
    comprec->xtype = xtype;
    comprec->base_nc_type = base_nc_type;

    componentnames = (NrmQuark*)NclMalloc(nfields * sizeof(NrmQuark));
    *componentnamesptr = componentnames;

    for(fidx = 0; fidx < nfields; fidx++)
    {

        compnode = &(comprec->compnode[fidx]);
        
        sides = NULL;
        nc_inq_compound_field(ncid, xtype, fidx, buffer,
                          &offset, &ftype, &rank, sides);

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tfidx: %d, offset: %ld\n", fidx, (long)offset);
       *fprintf(stderr, "\trank: %d\n", rank);
       *fprintf(stderr, "\tftype: %d\n", (long)ftype);
       *fprintf(stderr, "\tname: <%s>\n", buffer);
       *fprintf(stderr, "\tftype = %d, NC_CHAR = %d, NC_STRING = %d\n", ftype, NC_CHAR, NC_STRING);
       */

        if(rank > 0)
        {
            sides = (int *) NclCalloc(rank, sizeof(int));

            nc_inq_compound_field(ncid, xtype, fidx, NULL,
                                  NULL, NULL, NULL, sides);
        }

        compnode->the_nc_type = ftype;
        compnode->type = NC4MapToNcl(&ftype);
        compnode->name = NrmStringToQuark(buffer);
        compnode->offset = offset;
        compnode->rank = rank;
        compnode->sides = sides;
        compnode->nvals = 1;

        componentnames[fidx] = compnode->name;

        nc_inq_compound_size(ncid, xtype, &size);

        for(i = 0; i < rank; i++)
        {
            compnode->nvals *= sides[i];
        }

        if(NCL_none == compnode->type)
        {
            compnode->type = 1000 + ftype;
        }
    }

  /*
   *fprintf(stderr, "Leave get_nc4_compoundrec, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return comprec;
}

NclMultiDValData get_nc4_compoundlist(int ncid, int varid, size_t n_elem,
                                      long *start, long *finish, long *stride, int get_all)
{
    NclList compoundlist;
    size_t size;
    size_t nfields;
    nc_type base_nc_type, xtype;
    int ncl_class;

    int dimids[NCL_MAX_DIMENSIONS];
    char var_name[NC_MAX_NAME];
    char buffer[NC_MAX_NAME];
    int natts, ndims, ncompounddims;
    nc_type var_type;
    int i, k;
    int  complength = 1;

    size_t nvals = 1;
    void *values;
    int *position;

    NclQuark  dimnames[NCL_MAX_DIMENSIONS];
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    size_t   fullsizes[NCL_MAX_DIMENSIONS];
    ng_size_t compounddimsizes[1];

    void *compoundvalues;
    NclVar compoundvar;

    obj *listids = NULL;

    NclMultiDValData compound_md = NULL;

    nc_inq_var(ncid, varid, buffer, &xtype, &ndims, dimids, &natts);
    nc_inq_user_type(ncid, xtype, buffer, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_type: %d, nfields: %d, ncl_class: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_COMPOUND:
             break;
        default:
             fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tncl_class  %d\n", ncl_class);
             fprintf(stderr, "\tWe Thought It Was COMPOUND Data, But NOT.\n");
             exit ( -1 );
             break;
    }

    nc_inq_compound(ncid, xtype, buffer, &size, &nfields);
    complength = (int)size;
  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tsize = %d, nfields = %d\n", (int)size, nfields);
   */

    nc_inq_var(ncid, varid, var_name, &var_type, &ndims, dimids, &natts);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_name: <%s>, var_type = %d, ndims = %d, dimids[0] = %d, natts = %d\n",
   *                   var_name, var_type, ndims, dimids[0], natts);
   */

    nvals = 1;
    for(i = 0; i < ndims; ++i)
    {
        nc_inq_dim(ncid, dimids[i], buffer, &size);
        dimsizes[i] = (ng_size_t)floor((finish[i] - start[i])/(double)stride[i]) + 1;
        fullsizes[i] = (size_t)size;
        dimnames[i] = NrmStringToQuark(buffer);
        nvals *= size;
      /*
       *fprintf(stderr, "\tdim[%d] name: %s, size: %d\n", i, buffer, (int)size);
       */
    }

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnvals = %d, complength = %d\n", (int)nvals, (int)complength);
   */

    values = (void *)NclCalloc(nvals * complength, sizeof(void));
    assert(values);

    listids = (obj *)NclMalloc(n_elem * sizeof(obj));
    assert(listids);

    _NclBuildArrayOfList(listids, ndims, dimsizes);

    nc_get_var(ncid, varid, values);

    ncompounddims = 1;
    compounddimsizes[0] = complength;
    dimnames[0] = NrmStringToQuark("compound_dim");

    if(get_all)
    {
        for(i = 0; i < nvals; i++)
        {
            compoundvalues = (void *)NclCalloc(complength, sizeof(void));
            assert(compoundvalues);
            memcpy(compoundvalues, values + i * complength, complength);

            sprintf(buffer, "%s_%3.3d", var_name, i);
            compoundvar = _NclCreateVlenVar(buffer, compoundvalues,
                                        ncompounddims, dimnames,
                                        compounddimsizes, NCL_char);
            compoundlist = (NclList)_NclGetObj(listids[i]);
            _NclListAppend((NclObj)compoundlist, (NclObj)compoundvar);
        }
    }
    else
    {
	size_t length;
	long j, j1, j2;
        position = (int *)NclCalloc(n_elem, sizeof(int));
        assert(position);

	if(1 == ndims)
        {
            nvals = 0;
            for(j = start[0]; j <= finish[0]; j += stride[0])
	    {
                position[nvals] = j;
                ++nvals;
	    }
	}
	else if(2 == ndims)
        {
            nvals = 0;
            for(j1 = start[1]; j1 <= finish[1]; j1 += stride[1])
	    {
                for(j = start[0]; j <= finish[0]; j += stride[0])
	        {
                    position[nvals] = fullsizes[0]*j1 + j;
                    ++nvals;
	        }
	    }
	}
	else if(3 == ndims)
        {
            nvals = 0;
            for(j2 = start[2]; j2 <= finish[2]; j1 += stride[2])
	    {
                for(j1 = start[1]; j1 <= finish[1]; j1 += stride[1])
	        {
		    length = fullsizes[1]*fullsizes[0]*j2;
                    for(j = start[0]; j <= finish[0]; j += stride[0])
	            {
                        position[nvals] = (length + fullsizes[0])*j1 + j;
                        ++nvals;
	            }
	        }
	    }
	}
	else
        {
            free(values);
            free(position);
            fprintf(stderr, "\tCan not handle compound list with 4d and up.\n");
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Error in get_nc4_compoundlist in file (%s), at line: %d\n",
                                  __FILE__, __LINE__));
	    return compound_md;
        }

        for(k = 0; k < n_elem; k++)
        {
            i = position[k];
            compoundvalues = (void *)NclCalloc(complength, sizeof(void));
            assert(compoundvalues);
            memcpy(compoundvalues, values + i * complength, complength);

            sprintf(buffer, "%s_%3.3d", var_name, i);
            compoundvar = _NclCreateVlenVar(buffer, compoundvalues,
                                        ncompounddims, dimnames,
                                        compounddimsizes, NCL_char);
            compoundlist = (NclList)_NclGetObj(listids[k]);
            _NclListAppend((NclObj)compoundlist, (NclObj)compoundvar);
        }
        free(position);
    }

    free(values);

    compound_md = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,0,listids,
                                              NULL,ndims,dimsizes,TEMPORARY,NULL);

    return compound_md;
}

NclMultiDValData get_nc4_vlenlist(int ncid, int varid, nc_type xtype, NclBasicDataTypes* vlentype)
{
    NclList vlenlist;
    size_t size;
    size_t nfields;
    nc_type base_nc_type;
    int ncl_class;

    int dimids[NCL_MAX_DIMENSIONS];
    char var_name[NC_MAX_NAME];
    char buffer[NC_MAX_NAME];
    int natts, ndims, nvlendims;
    nc_type var_type;
    int i;

    size_t wlen = 1;
    size_t vlen = 1;
    nc_vlen_t *values;

    NclQuark  dimnames[NCL_MAX_DIMENSIONS];
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    ng_size_t vlendimsizes[1];

    void *vlenvalues;
    NclVar vlenvar;

    obj *listids = NULL;

    NclMultiDValData vlen_md;
  /*
   *int *iptr;

   *fprintf(stderr, "\nEnter get_nc4_vlenlist, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvarid = %d, xtype = %d\n", varid, xtype);
   */

    nc_inq_user_type(ncid, xtype, buffer, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_type: %d, nfields: %d, ncl_class: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_VLEN:
             break;
        default:
             fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tncl_class  %d\n", ncl_class);
             fprintf(stderr, "\tWe Thought It Was Vlen Data, But NOT.\n");
             exit ( -1 );
             break;
    }

    nc_inq_vlen(ncid, xtype, buffer, &size, &base_nc_type);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tbase_nc_type = %d, size = %d, buffer = <%s>, sizeof(nc_vlen_t) = %d\n",
   *                   base_nc_type, (int)size, buffer, sizeof(nc_vlen_t));
   */

    nc_inq_var(ncid, varid, var_name, &var_type, &ndims, dimids, &natts);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_name: <%s>, var_type = %d, ndims = %d, dimids[0] = %d, natts = %d\n",
   *                   var_name, var_type, ndims, dimids[0], natts);
   */

    wlen = 1;
    for(i = 0; i < ndims; i++)
    {
        nc_inq_dim(ncid, dimids[i], buffer, &size);
        dimsizes[i] = (ng_size_t)size;
        dimnames[i] = NrmStringToQuark(buffer);
        wlen *= size;
    }

    values = (void *)NclCalloc(wlen, sizeof(nc_vlen_t));
    assert(values);

    listids = (obj *)NclMalloc(wlen * sizeof(obj));
    assert(listids);

    _NclBuildArrayOfList(listids, ndims, dimsizes);

    nc_get_var(ncid, varid, values);

    *vlentype = NC4MapToNcl(&base_nc_type);

    nvlendims = 1;
    dimnames[0] = NrmStringToQuark("vlendim");
    for(i = 0; i < wlen; i++)
    {
        vlendimsizes[0] = (ng_size_t)values[i].len;

        vlen = values[i].len * _NclSizeOf(*vlentype);
        vlenvalues = (void *)NclCalloc(vlen, sizeof(void));
        assert(vlenvalues);
        memcpy(vlenvalues, values[i].p, vlen);

        sprintf(buffer, "%s_%3.3d", var_name, i);
        vlenvar = _NclCreateVlenVar(buffer, vlenvalues,
                                    nvlendims, dimnames,
                                    vlendimsizes, *vlentype);
        vlenlist = (NclList)_NclGetObj(listids[i]);
        _NclListAppend((NclObj)vlenlist, (NclObj)vlenvar);
    }

    nc_free_vlens(wlen, values);
    free(values);

    vlen_md = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,0,listids,
                                          NULL,ndims,dimsizes,TEMPORARY,NULL);

    return vlen_md;
}

void *get_nc4_opaque(int ncid, int varid, nc_type xtype)
{
    NclFileOpaqueRecord *opaquerec;
    size_t size;
    size_t nfields;
    nc_type base_nc_type;
    int ncl_class;

    int dimids[NCL_MAX_DIMENSIONS];
    char var_name[NC_MAX_NAME+1];
    char buffer[NC_MAX_NAME + 2];
    int natts, ndims;
    nc_type var_type;
    int i;

    size_t wlen = 1;

    NclBasicDataTypes opaquetype;

  /*
   *fprintf(stderr, "\nEnter get_nc4_opaque, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvarid = %d, xtype = %d\n", varid, xtype);
   */

    nc_inq_user_type(ncid, xtype, buffer, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_type: %d, nfields: %d, ncl_class: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_OPAQUE:
             break;
        default:
             fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tncl_class  %d\n", ncl_class);
             fprintf(stderr, "\tWe Thought It Was OPAQUE Data, But NOT.\n");
             exit ( -1 );
             break;
    }

    nc_inq_opaque(ncid, xtype, buffer, &nfields);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>, nfields = %d\n", buffer, (int)nfields);
   */

    if(base_nc_type)
        opaquetype = NC4MapToNcl(&base_nc_type);
    else
        opaquetype = NCL_byte;

    opaquerec = (NclFileOpaqueRecord *)NclCalloc(1, sizeof(NclFileOpaqueRecord));
    assert(opaquerec);

    opaquerec->name = NrmStringToQuark(buffer);
    if(base_nc_type)
        opaquerec->type = NC4MapToNcl(&base_nc_type);
    else
        opaquerec->type = NCL_byte;
    opaquerec->base_nc_type = base_nc_type;
    opaquerec->xtype = xtype;
    opaquerec->size = nfields;

    nc_inq_var(ncid, varid, var_name, &var_type, &ndims, dimids, &natts);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_name: <%s>, var_type = %d, ndims = %d, dimids[0] = %d, natts = %d\n",
   *                   var_name, var_type, ndims, dimids[0], natts);
   */

    wlen = 1;
    for(i = 0; i < ndims; i++)
    {
        nc_inq_dim(ncid, dimids[i], buffer, &size);
        wlen *= size;
    }

    opaquerec->max_opaques = wlen;
    opaquerec->n_opaques = wlen;

    opaquerec->values = (void *)NclCalloc(wlen * nfields, _NclSizeOf(opaquetype));
    assert(opaquerec->values);

    nc_get_var(ncid, varid, opaquerec->values);

  /*
   *{
   *    unsigned char *cptr = (unsigned char *) opaquerec->values;

   *    n = 0;
   *    fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *    fprintf(stderr, "\tvalues = {");
   *    for(i = 0; i < wlen; i++)
   *    {
   *        fprintf(stderr, " {");
   *
   *        for(j = 0; j < nfields; j++)
   *        {
   *            if(j)
   *                fprintf(stderr, ", ");
   *            fprintf(stderr, "%c", cptr[n]);
   *            n++;
   *        }
   *        fprintf(stderr, "}");
   *    }
   *    fprintf(stderr, " }\n");
   *}
   */

  /*
   *fprintf(stderr, "Leave get_nc4_opaque, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return (void *) opaquerec;
}

void *get_nc4_enum(int ncid, int varid, nc_type xtype)
{
    NclFileEnumRecord *enumrec;
    NclFileEnumNode   *enumnode;
    size_t size;
    size_t nfields;
    nc_type base_nc_type;
    int ncl_class;

    int dimids[NCL_MAX_DIMENSIONS];
    char var_name[NC_MAX_NAME+1];
    char buffer[NC_MAX_NAME + 2];
    char udt_name[NC_MAX_NAME + 2];
    int natts, ndims;
    nc_type var_type;
    int i;

    size_t wlen;
    size_t base_size;
    size_t num_members;
    void *vv;

    int rc = -1;

  /*
   *fprintf(stderr, "\nEnter get_nc4_enum, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvarid = %d, xtype = %d\n", varid, xtype);
   */

    nc_inq_user_type(ncid, xtype, udt_name, &size, 
                    &base_nc_type, &nfields, &ncl_class);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>\n", buffer);
   *fprintf(stderr, "\tsize: %d, base_nc_type: %d, nfields: %d, ncl_class: %d\n",
   *                  size, base_nc_type, nfields, ncl_class);
   */

    switch(ncl_class)
    {
        case NC_ENUM:
             break;
        default:
             fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
             fprintf(stderr, "\tncl_class  %d\n", ncl_class);
             fprintf(stderr, "\tWe Thought It Was ENUM Data, But NOT.\n");
             exit ( -1 );
             break;
    }

    rc = nc_inq_enum(ncid, xtype, buffer, &base_nc_type,
                     &base_size, &num_members);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tname: <%s>, base_size = %d, num_members = %d, base_nc_type = %d\n",
   *                   buffer, base_size, (int)num_members, base_nc_type);
   */

    enumrec = _NclFileEnumAlloc(num_members);

    enumrec->name = NrmStringToQuark(udt_name);
    enumrec->type = NC4MapToNcl(&base_nc_type);
    enumrec->base_nc_type = base_nc_type;
    enumrec->xtype = xtype;
    enumrec->size = num_members;

    vv = (void *)NclCalloc(1, _NclSizeOf(enumrec->type));
    assert(vv);

    for(i = 0; i < num_members; i++)
    {
        rc = nc_inq_enum_member(ncid, xtype, i, buffer, vv);
        enumnode = &(enumrec->enum_node[i]);
        enumnode->name = NrmStringToQuark(buffer);
        enumnode->value = *(long long *)vv;

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tMember %d: name <%s>, value = %lld\n",
       *                   i, buffer, *(long long *)vv);

       *rc = nc_inq_enum_ident(ncid, typeid, enumnode->value, name);

       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tMember %d: name <%s>, value = %lld\n",
       *                   i, name, enumnode->value);
       */
    }

    if(rc != NC_NOERR)
    {
        char *emsg = (char *) nc_strerror(rc);
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,emsg));
    }

    NclFree(vv);

    nc_inq_var(ncid, varid, var_name, &var_type, &ndims, dimids, &natts);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_name: <%s>, var_type = %d, ndims = %d, dimids[0] = %d, natts = %d\n",
   *                   var_name, var_type, ndims, dimids[0], natts);
   */

    wlen = 1;
    for(i = 0; i < ndims; i++)
    {
        nc_inq_dim(ncid, dimids[i], buffer, &size);
        wlen *= size;
    }

    enumrec->values = (void *)NclCalloc(wlen, _NclSizeOf(enumrec->type));
    assert(enumrec->values);

    nc_get_var(ncid, varid, enumrec->values);

  /*
   *{
   *    unsigned char *cptr = (unsigned char *) enumrec->values;

   *    n = 0;
   *    fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *    for(i = 0; i < wlen; i++)
   *    {
   *        fprintf(stderr, "\tvalues[%d] = %d\n", i, (int)cptr[i]);
   *    }
   *}
   */

  /*
   *fprintf(stderr, "Leave get_nc4_enum, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return (void *) enumrec;
}

NclFileAttRecord *_NC4_get_atts(int gid, int aid, int n_atts)
{
    char buffer[NC_MAX_NAME + 2];
    int i;
    size_t alen;
    NclFileAttRecord *attrec;
    NclFileAttNode   *attnode;

    short has_missing = 0;
    short need_add_fillvalue = 1;
    int   missing_idx = -1;

    if(n_atts < 1)
    {
        return NULL;
    }

  /*
   *fprintf(stderr, "\nEnter _NC4_get_atts, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid: %d, aid: %d, n_atts: %d\n", gid, aid, n_atts);
   */

    attrec = _NclFileAttAlloc(n_atts);
    assert(attrec);

    attrec->id = -1;
    attrec->gid = gid;
    attrec->aid = aid;

    for(i = 0; i < n_atts; i++)
    {
        attnode = &(attrec->att_node[i]);

        ncattname(gid,aid,i,buffer);

      /*
       *fprintf(stderr,"\tAtt No. %d, name: <%s>\n", i, buffer);
       */

        attnode->is_virtual = 0;
        attnode->is_opaque = 0;
        attnode->is_vlen = 0;
        attnode->is_compound = 0;
        attnode->name = NrmStringToQuark(buffer);

      /*
       *ncattinq(gid,aid,buffer,
       *    &(attnode->the_nc_type),
       *    &(attnode->n_elem));

       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tattnode->name: <%s>, the_nc_type = %d, n_elem = %d\n",
       *                 buffer, attnode->the_nc_type, attnode->n_elem);
       */

        nc_inq_attname(gid, aid, i, buffer);
        nc_inq_att(gid, aid, buffer, &(attnode->the_nc_type), &alen);
        attnode->n_elem = alen;
        attnode->type = NC4MapToNcl(&(attnode->the_nc_type));

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tattnode->name: <%s>, the_nc_type = %d, n_elem = %d\n",
       *                   buffer, attnode->the_nc_type, attnode->n_elem);
       */

        NC4GetAttrVal(gid, aid, attnode);

      /*Check if we need to add "_FillValue".*/
        if(Qmissing_val == attnode->name)
        {
            has_missing = 1;
            missing_idx = i;
        }
        if(Qfill_val == attnode->name)
            need_add_fillvalue = 0;
    }

  /*If we need_add_fillvalue and has_missing, then add an extra attribute to att_rec.*/
    if(need_add_fillvalue)
    {
        if(has_missing && (-1 < missing_idx))
        {
            attnode = &(attrec->att_node[missing_idx]);
            _addNclAttNode(&attrec, Qfill_val, attnode->type, attnode->n_elem, attnode->value);
        }
    }

  /*
   *fprintf(stderr, "Leave _NC4_get_atts, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return attrec;
}

NclFileUDTRecord *_NC4_get_udts(int gid, int uid, int n_udts)
{
    char buffer[NC_MAX_NAME + 2];
    int i, n;
    NclFileUDTRecord *udtrec;
    NclFileUDTNode   *udtnode;
    int *typeids;

    NclQuark          *mem_name;
    NclBasicDataTypes *mem_type;
    NclBasicDataTypes ncl_type;

    size_t  size;
    size_t  nfields;
    nc_type base_nc_type;
    int     ncl_class;
    nc_type ftype;

    void   *val;

    if(n_udts < 1)
    {
        return NULL;
    }

  /*
   *fprintf(stderr, "\nEnter _NC4_get_udts, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid: %d, uid: %d, n_udts: %d\n", gid, uid, n_udts);
   */

    udtrec = _NclFileUDTAlloc(n_udts);
    assert(udtrec);

    udtrec->gid = gid;
    udtrec->uid = uid;

    typeids = (int *)NclCalloc(n_udts, sizeof(int));
    assert(typeids);

    nc_inq_typeids(gid, &n_udts, typeids);

    for(i = 0; i < n_udts; i++)
    {
        udtnode = &(udtrec->udt_node[i]);

        nc_inq_user_type(gid, typeids[i], buffer, &size,
                        &base_nc_type, &nfields, &ncl_class);
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tudt No. %d, name: <%s>\n", i, buffer);
       *fprintf(stderr, "\tsize: %d, base_nc_typep: %d, nfieldsp: %d, classp: %d\n",
       *                  size, base_nc_type, nfields, ncl_class);
       */

        udtnode->id = typeids[i];
        udtnode->name = NrmStringToQuark(buffer);
        udtnode->type = base_nc_type;
        udtnode->size = size;
        udtnode->ncl_class = ncl_class;
        udtnode->max_fields = nfields;
        udtnode->n_fields = nfields;

        mem_name = (NclQuark *) NclCalloc(nfields, sizeof(NclQuark));
        mem_type = (NclBasicDataTypes *) NclCalloc(nfields, sizeof(NclBasicDataTypes));

        switch(ncl_class)
        {
            case NC_VLEN:
            case NC_OPAQUE:
            case NC_COMPOUND:
                 for(n = 0; n < nfields; n++)
                 {
                     nc_inq_compound_field(gid, typeids[i], n, buffer,
                                           NULL, &ftype, NULL, NULL);
                     mem_name[n] = NrmStringToQuark(buffer);
                     mem_type[n] = NC4MapToNcl(&ftype);
                   /*
                    *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                    *fprintf(stderr, "\tField %d, name: <%s>, type: %d\n", n, buffer, ftype);
                    */
                 }
                 break;
            case NC_ENUM:
                 nc_inq_enum(gid, typeids[i], buffer, &base_nc_type, &size, &nfields);
                 ncl_type = NC4MapToNcl(&base_nc_type);
                 val = (void *)NclCalloc(1, _NclSizeOf(ncl_type));
                 assert(val);
                 for(n = 0; n < nfields; n++)
                 {
                     nc_inq_enum_member(gid, typeids[i], n, buffer, val);
                     mem_name[n] = NrmStringToQuark(buffer);
                     mem_type[n] = NC4MapToNcl(&ncl_type);
                   /*
                    *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                    *fprintf(stderr, "\tMember %d, name: <%s>, value: %lld\n",
                    *                   n, buffer, *(int64 *)val);
                    */
                 }
                 break;
             default:
                 fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                 fprintf(stderr, "\tDO NOT KNOW HOW TO HANDLE NC4-CLASS: %d\n", ncl_class);
                 exit (-1);
        }

        udtnode->mem_name = mem_name;
        udtnode->mem_type = mem_type;
    }

    free(typeids);

  /*
   *fprintf(stderr, "Leave _NC4_get_udts, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return udtrec;
}

/* For netCDF-4 compiles, check to see if the file has any
 * groups. If it does, this function is called recursively
 * on each of them.
 */
NclFileGrpRecord *_NC4_get_grprec(int gid, int unlimited_dim_idx, NclFileGrpNode *parentgrpnode)
{
    NclFileGrpRecord *grprec = NULL;
    int nc_status;
    int n, numgrps = 0;
    int *ncids;
    NclQuark gname;
    char buffer[NC_MAX_NAME + 1];

  /*
   *fprintf(stderr, "\nEnter _NC4_get_grprec, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid = %d\n", gid);
   */

    /* See how many groups there are. */
    nc_status = nc_inq_grps(gid, &numgrps, NULL);

    if(nc_status != NC_NOERR)
    {
        char *emsg = (char *) nc_strerror(nc_status);
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,emsg));
    }

    if(numgrps < 1)
        return grprec;

    grprec = _NclFileGrpAlloc(numgrps);

    nc_inq_grpname(gid, buffer);
    gname = NrmStringToQuark(buffer);

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid = %d, numgrps = %d\n", gid, numgrps);
   *fprintf(stderr, "\tgid = %d, group name: <%s>\n\n", gid, buffer);
   */

    /* Allocate memory to hold the list of group ids. */
    ncids = NclMalloc((numgrps) * sizeof(int));
    assert(ncids);

    /* Get the list of group ids. */
    nc_inq_grps(gid, NULL, ncids);

    /* Call this function for each group. */
    for(n = 0; n < numgrps; n++)
        _NC4_get_grpnode(gid, ncids[n], gname, parentgrpnode, grprec->grp_node[n]);

    free(ncids);

    return grprec;
}

void _NC4_get_grpnode(int pid, int gid, NclQuark pn, NclFileGrpNode *parentgrpnode, NclFileGrpNode *grpnode)
{
    int nc_ret;
    int n_dims = 0;
    int n_vars = 0;
    int n_atts = 0;
    int numgrps = 0;
    int unlimited_dim_idx = 0;
    int has_scalar_dim = 0;
    char *pgn;
    char buffer[NC_MAX_NAME + 1];
    char group_name[NC_MAX_NAME + 1];

  /*
   *fprintf(stderr, "\nEnter _NC4_get_grpnode, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tpid = %d, parent name: <%s>\n",
   *        pid, NrmQuarkToString(pn));
   */

    nc_ret = nc_inq_grpname(gid, group_name);

    pgn = NrmQuarkToString(pn);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid = %d, group name: <%s>, parent-group-name: <%s>\n",
   *                 gid, group_name, pgn);
   */

    grpnode->pid = pid;
    grpnode->gid = gid;
    grpnode->pname = pn;
    grpnode->name = NrmStringToQuark(group_name);
    grpnode->parent = parentgrpnode;

    if(NULL != parentgrpnode)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tparentgrpnode->real_name: <%s>\n", NrmQuarkToString(parentgrpnode->real_name));
       */

        if(strcmp("/", pgn))
        {
            sprintf(buffer, "%s/%s", NrmQuarkToString(parentgrpnode->real_name), group_name);
        }
        else
        {
            sprintf(buffer, "%s%s", NrmQuarkToString(parentgrpnode->real_name), group_name);
        }
        grpnode->real_name = NrmStringToQuark(buffer);

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tgrpnode->real_name: <%s>\n", NrmQuarkToString(grpnode->real_name));
       */
    }
    else
    {
        if('/' == group_name[0])
        {
            sprintf(buffer, "%s", group_name);
        }
        else
        {
            sprintf(buffer, "/%s", group_name);
        }
        grpnode->real_name = NrmStringToQuark(buffer);
    }

    grpnode->extension = parentgrpnode->extension;
    grpnode->file_format = parentgrpnode->file_format;
    grpnode->status = parentgrpnode->status;
    grpnode->open = parentgrpnode->open;
    grpnode->format = parentgrpnode->format;
    grpnode->define_mode = parentgrpnode->define_mode;
    grpnode->compress_level = parentgrpnode->compress_level;
    grpnode->is_chunked = parentgrpnode->is_chunked;
    grpnode->use_cache = parentgrpnode->use_cache;
    grpnode->cache_size = parentgrpnode->cache_size;
    grpnode->cache_nelems = parentgrpnode->cache_nelems;
    grpnode->cache_preemption = parentgrpnode->cache_preemption;

    NC4InitializeOptions(grpnode);

#if 0
    grpnode->chunk_dim_rec = parentgrpnode->chunk_dim_rec;
    grpnode->unlimit_dim_rec = parentgrpnode->unlimit_dim_rec;
    grpnode->dim_rec = parentgrpnode->dim_rec;
    grpnode->att_rec = parentgrpnode->att_rec;
    grpnode->var_rec = parentgrpnode->var_rec;
    grpnode->coord_var_rec = parentgrpnode->coord_var_rec;
    grpnode->udt_rec = parentgrpnode->udt_rec;
    grpnode->parent = NULL;
#endif

    nc_ret = ncinquire(gid, &n_dims, &n_vars, &n_atts, &unlimited_dim_idx);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims = %d, n_vars = %d, n_atts = %d\n",
   *        n_dims, n_vars, n_atts);
   */

    grpnode->att_rec = _NC4_get_atts(gid, NC_GLOBAL, n_atts);

    grpnode->dim_rec = _NC4_get_dims(gid, n_dims, unlimited_dim_idx);

    /*
    grpnode->var_rec = _NC4_get_vars(gid, n_vars, &has_scalar_dim,
                                     unlimited_dim_idx, NrmQuarkToString(grpnode->real_name));
    * we need the grpnode inside this function to most easily evaluate the number of unlimited
    * dimensions and figure out which one they are 
    */
    grpnode->var_rec = _NC4_get_vars(grpnode, n_vars, &has_scalar_dim,
                                     unlimited_dim_idx, NrmQuarkToString(grpnode->real_name));

    if(n_dims)
        NC4GetDimVals(gid, grpnode);

    nc_ret = nc_inq_grps(gid, &numgrps, NULL);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnumgrps = %d\n", numgrps);
   */

    if(numgrps > 0)
        grpnode->grp_rec = _NC4_get_grprec(gid, unlimited_dim_idx, grpnode);
    else
        grpnode->grp_rec = NULL;

    if(nc_ret != NC_NOERR)
    {
        char *emsg = (char *) nc_strerror(nc_ret);
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,emsg));
    }

  /*
   *fprintf(stderr, "Leave _NC4_get_grpnode, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
}

NclFileDimRecord *_NC4_get_dims(int gid, int n_dims, int unlimited_dim_idx)
{
    char buffer[MAX_NC_NAME];
    size_t tmp_size = 0;
    int ndims_grp;
    int *dimids_grp;
    int nunlim;
    int *unlimids;
    int dimid, i, j;

    NclFileDimRecord *dimrec = NULL;

  /*
   *fprintf(stderr, "\nEnter _NC4_get_dims, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid = %d\n", gid);
   *fprintf(stderr, "\tn_dims = %d\n", n_dims);
   */

    /* Find the number of dimids defined in this group. */
    nc_inq_ndims(gid, &ndims_grp);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tndims_grp = %d\n", ndims_grp);
   */

    if(ndims_grp < 1)
        return dimrec;

    dimids_grp = (int *)NclCalloc((ndims_grp + 1), sizeof(int));
    assert(dimids_grp);

    /* Find the dimension ids in this group. */
    nc_inq_dimids(gid, 0, dimids_grp, 0);

    /* Find the number of unlimited dimensions and get their IDs */
    nc_inq_unlimdims(gid, &nunlim, NULL);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnunlim = %d\n", nunlim);
   */

    unlimids = (int *)NclCalloc((nunlim + 1), sizeof(int));
    assert(unlimids);
    nc_inq_unlimdims(gid, &nunlim, unlimids);

    dimrec = _NclFileDimAlloc(ndims_grp);
    dimrec->gid = gid;

    for(i = 0 ; i < ndims_grp; i++)
    {
        dimid = dimids_grp[i];
        tmp_size = 0;
        dimrec->dim_node[i].id = dimid;
        dimrec->dim_node[i].is_unlimited = 0;

        for(j = 0; j < nunlim; j++)
        {
            if(dimid == unlimids[j])
            {
                dimrec->dim_node[i].is_unlimited = 1;
                break;
            }
        }

        nc_inq_dim(gid, dimid, buffer, &tmp_size);
        dimrec->dim_node[i].size = (ng_size_t) tmp_size;

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\ti = %d, name: <%s>, size: %ld\n",
       *        i, buffer, dimrec->dim_node[i].size);
       *fprintf(stderr,"\tId: %d, dim No. %d, name: <%s>, size: %ld\n",
       *        gid, i, buffer, dimrec->dim_node[i].size);
       */

        dimrec->dim_node[i].name = NrmStringToQuark(buffer);
    }

    NclFree(dimids_grp);
    NclFree(unlimids);

  /*
   *fprintf(stderr, "Leave _NC4_get_dims, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return dimrec;
}

NclFileVarRecord *_NC4_get_vars(NclFileGrpNode *grpnode, int n_vars, int *has_scalar_dim,
                                int unlimited_dim_idx, char *grpnamestr)
{
    char buffer[MAX_NC_NAME];
    char buffer2[MAX_NC_NAME];
    int  nc_dim_id[MAX_VAR_DIMS];
    int  i, j, n_atts, n_dims, nc_dims;
    long tmp_size = 0;
    int gid = grpnode->gid;

    NclFileVarRecord  *varrec;
    NclFileDimRecord  *dimrec;
    NclFileAttRecord  *attrec;

    NclFileVarNode    *varnode;
    NclFileAttNode    *attnode;

    int    storage_in = -1;
    size_t chunksizes[MAX_VAR_DIMS];

    int  deflatep = -1;

  /*
   *fprintf(stderr, "\nEnter _NC4_get_vars, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid = %d\n", gid);
   *fprintf(stderr, "\tn_vars = %d\n", n_vars);
   */

    if(n_vars < 1)
        return NULL;

    *has_scalar_dim = 0;

    varrec = _NclFileVarAlloc(n_vars);
    varrec->gid = gid;

    for(i = 0 ; i < n_vars; i++)
    {
        varnode = &(varrec->var_node[i]);
        varnode->id = i;
        varnode->gid = gid;
        varnode->value = NULL;
        varnode->is_chunked = 0;
        varnode->is_compound = 0;

        ncvarinq(gid,i,buffer,
            &(varnode->the_nc_type),
            &nc_dims, nc_dim_id, &n_atts);

        if(0 == strcmp("/", grpnamestr))
            sprintf(buffer2, "%s%s", grpnamestr, buffer);
        else
            sprintf(buffer2, "%s/%s", grpnamestr, buffer);

        varnode->name = NrmStringToQuark(buffer);
        varnode->real_name = NrmStringToQuark(buffer2);
        varnode->type = NC4MapToNcl(&(varnode->the_nc_type));

        varnode->att_rec = _NC4_get_atts(gid, i, n_atts);
        attrec = varnode->att_rec;

        if(NCL_none == varnode->type)
        {
            size_t size;
            size_t nfields;
            nc_type base_nc_type;
            int ncl_class;

            nc_inq_user_type(gid, varnode->the_nc_type, buffer, &size, 
                            &base_nc_type, &nfields, &ncl_class);

            varnode->base_type = NC4MapToNcl(&base_nc_type);

            switch(ncl_class)
            {
                case NC_OPAQUE: 
                     {
                         varnode->type = NCL_opaque;
                     }
                     break;
                case NC_ENUM: 
                     {
                         varnode->type = NCL_enum;
                     }
                     break;
                case NC_VLEN:
                     {
                         varnode->type = NCL_list;
                     }
                     break;
                case NC_COMPOUND:
                     {
                         NrmQuark *componentnames = NULL;
                         NrmQuark compatt = NrmStringToQuark("component_names");
                         NclFileCompoundRecord *comprec = get_nc4_compoundrec(gid, varnode->the_nc_type, &componentnames);
                         varnode->type = NCL_compound;
                         varnode->is_compound = 1;
                         varnode->comprec = comprec;

                         _addNclAttNode(&(varnode->att_rec), compatt, NCL_string, comprec->nfields, (void*)componentnames);
			 if(NULL != componentnames)
                             NclFree(componentnames);
                     }
                     break;
                default:
                     fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
                     fprintf(stderr, "\tWe Thought It Was Compound Data, But NOT.\n");
                     exit ( -1 );
                     break;
            }
        }
      /*
       *else
       *{
       *    fprintf(stderr, "\tVar No. %d, name: <%s>, type: <%s>, real_name: <%s>\n", i,
       *                     NrmQuarkToString(varnode->name),
       *                     _NclBasicDataTypeToName(varnode->type),
       *                     NrmQuarkToString(varnode->real_name));
       *}
       */

        if(0 == nc_dims)
        {
            n_dims = 1;
            dimrec = _NclFileDimAlloc(n_dims);
            dimrec->dim_node[0].id = -5;
            dimrec->dim_node[0].size = 1;
            dimrec->dim_node[0].name = NrmStringToQuark("ncl_scalar");
            dimrec->dim_node[0].description = NrmStringToQuark("NC4 Scalar Dimension");
            *has_scalar_dim = 1;

          /*
           *continue;
           */
        }
        else
        {
            n_dims = nc_dims;
            dimrec = _NclFileDimAlloc(n_dims);
        }

        dimrec->gid = gid;
        varnode->dim_rec = dimrec;

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tnc_dims %d\n", nc_dims);
       */
        
        for(j = 0; j < nc_dims; j++)
        {
	    NclFileDimRecord *gdimrec;
	    NclFileGrpNode *gnode;
	    int k;

            tmp_size = 0;
            ncdiminq(gid,nc_dim_id[j],buffer2,&tmp_size);
            dimrec->dim_node[j].id = nc_dim_id[j];

            /*dimrec->dim_node[j].is_unlimited = (unlimited_dim_idx == nc_dim_id[j])?1:0;*/
	    gnode = grpnode;
	    dimrec->dim_node[j].is_unlimited = 0;
	    while (gnode != NULL) {
		    int found = 0;
		    if (! gnode->dim_rec) {
			    gnode = gnode->parent;
			    continue;
		    }
		    gdimrec = gnode->dim_rec;
		    for (k = 0; k < gdimrec->n_dims; k++) {
			    if (gdimrec->dim_node[k].id == dimrec->dim_node[j].id) {
				    dimrec->dim_node[j].is_unlimited = gdimrec->dim_node[k].is_unlimited;
				    found = 1;
				    break;
			    }
		    }
		    if (found) break;
	    }
            dimrec->dim_node[j].name = NrmStringToQuark(buffer2);
            dimrec->dim_node[j].size = tmp_size;

	    /*
	     * fprintf(stderr, "\t\tDim No. %d, name: <%s>, size: %d, id: %d, unlim: %s\n",
	     * j, NrmQuarkToString(dimrec->dim_node[j].name),
	     *    dimrec->dim_node[j].size, nc_dim_id[j],dimrec->dim_node[j].is_unlimited? "yes" : "no");
	     */
        }

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tn_atts %d\n", n_atts);
       */
        
        for(j = 0; j < n_atts; j++)
        {
            attnode = &attrec->att_node[j];

          /*
           *fprintf(stderr, "\tAtt No. %d, name: <%s>, type: %s, var-type: %s\n",
           *                j, NrmQuarkToString(attnode->name),
           *                _NclBasicDataTypeToName(attnode->type),
           *                _NclBasicDataTypeToName(varnode->type));
           */

            if((Qfill_val == attnode->name) && (attnode->type != varnode->type))
            {
                NclFileAttNode *newattnode;
                NclQuark Qori_fill_val = NrmStringToQuark("Ori_FillValue");

                if(NCL_enum == varnode->type)
                    break;

                NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                    "_FillValue attribute type (%s) differs from variable (%s) type.\n\t%s\n",
                     _NclBasicDataTypeToName(attnode->type), _NclBasicDataTypeToName(varnode->type),
                    "forcing type conversion; it may result in overflow and/or loss of precision"));

                _addNclAttNode(&varnode->att_rec, Qori_fill_val, attnode->type, attnode->n_elem, attnode->value);
                newattnode = &attrec->att_node[varnode->att_rec->n_atts];

		NclFree(attnode->value);
		varnode->att_rec->att_node[j].type = varnode->type;
		varnode->att_rec->att_node[j].value = (void*)NclMalloc(attnode->n_elem * _NclSizeOf(varnode->type));

                _NclScalarForcedCoerce(newattnode->value, newattnode->type,
                                       varnode->att_rec->att_node[j].value,
                                       varnode->att_rec->att_node[j].type);

	      /*
               *fprintf(stderr, "\tattnode->name: %s, type: %s\n",
	       *		 NrmQuarkToString(attnode->name), _NclBasicDataTypeToName(attnode->type));
               *fprintf(stderr, "\tattnode->value: %f, orig-value: %s\n",
	       *		 *(float*)attnode->value, NrmQuarkToString(*(NrmQuark*)newattnode->value));
	       */

		break;
            }

          /*
           *fprintf(stderr, "\tAtt No. %d, name: <%s>, type: %s, var-type: %s\n",
           *                j, NrmQuarkToString(attnode->name),
           *                _NclBasicDataTypeToName(attnode->type),
           *                _NclBasicDataTypeToName(varnode->type));
           */
        }

      /* How to check variable chunking:
       * int nc_inq_var_chunking(int ncid, int varid, int *storagep, size_t *chunksizesp);
       * ncid
       *     NetCDF ID, from a previous call to nc_open or nc_create.
       * varid
       *     Variable ID.
       * *storagep
       *     Address of returned storage property,
       *     returned as NC_CONTIGUOUS if this variable uses contiguous storage,
       *     or NC_CHUNKED if it uses chunked storage.
       * *chunksizesp
       *     A pointer to an array list of chunk sizes.
       *     The array must have one chunksize for each dimension in the variable. 
       */
        nc_inq_var_chunking(gid, i, &storage_in, chunksizes);
        varnode->is_chunked = 0;
        if(NC_CHUNKED == storage_in)
        {
            nc_inq_var_deflate(gid, i, &(varnode->shuffle), &deflatep, &(varnode->compress_level));

          /*
           *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\t\tvarnode->shuffle = %d, deflatep = %d, compress_level = %d\n",
           *                     varnode->shuffle, deflatep, varnode->compress_level);
           */

            varnode->is_chunked = 1;
            dimrec = _NclFileDimAlloc(n_dims);
            dimrec->gid = gid;
            varnode->chunk_dim_rec = dimrec;

            for(j = 0; j < nc_dims; j++)
            {
                dimrec->dim_node[j].id = nc_dim_id[j];
                dimrec->dim_node[j].is_unlimited = (unlimited_dim_idx == nc_dim_id[j])?1:0;
                dimrec->dim_node[j].name = varnode->dim_rec->dim_node[j].name;
                dimrec->dim_node[j].size = chunksizes[j];


		/* fprintf(stderr, "\t\tChunk_Dim No. %d, name: <%s>, size: %d, id: %d, unlim: %s\n",
		 *                j, NrmQuarkToString(dimrec->dim_node[j].name),
		 *                dimrec->dim_node[j].size, nc_dim_id[j]);
		 */
            }
        }
    }

  /*
   *fprintf(stderr, "Leave _NC4_get_vars, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return varrec;
}

void *NC4OpenFile(void *rootgrp, NclQuark path, int status)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) rootgrp;
    NclFileDimNode *dimnode;
    NclFileDimNode *vardimnode;
    NclFileVarNode *varnode;

    int fid;
    int nc_ret = NC_NOERR;
    int unlimited_dim_idx;
    int has_scalar_dim = 0;
    int numgrps = 0;
    int ntypes = 0;
    int n_atts, n_dims, n_vars;

    int i, j, k;

    if(NULL == grpnode)
    {
        return(NULL);
    }

    grpnode->path = path;
    grpnode->status = status;
    grpnode->compress_level = 0;
    grpnode->shuffle = 1;

  /*
   *fprintf(stderr,"\nNC4OpenFile, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr,"\tpath: <%s>\n", (char *)NrmQuarkToString(path));
   */

    if(grpnode->open)
    {
        fid = grpnode->gid;
    }
    else if(status > 0)
    {
        nc_ret = nc__open(NrmQuarkToString(path),NC_NOWRITE,&ChunkSizeHint,&fid);
        grpnode->define_mode = 0;
        grpnode->gid = fid;
        grpnode->fid = fid;
    }
    else
    {
        nc_ret = nc__open(NrmQuarkToString(path),NC_WRITE,&ChunkSizeHint,&fid);
        grpnode->define_mode = 1;
        grpnode->open = 1;
        grpnode->fid = fid;
        grpnode->gid = fid;
        nc_redef(fid);
    }

    if(nc_ret != NC_NOERR)
    { 
        char *emsg = (char *) nc_strerror(nc_ret);
        if (emsg == NULL)
        {
            emsg = "NclNetCDF4: The specified file (%s) cannot be opened; invalid file or system error";
	    if ((! strncmp(NrmQuarkToString(path),"http://",7)) || (! strncmp(NrmQuarkToString(path),"https://",8)))
            {
                emsg = "The specified URL (%s) does not reference an active DODS server or cannot be processed by the DODS server";
            }
        }

        NHLPERROR((NhlFATAL,NhlEUNKNOWN,emsg,NrmQuarkToString(path)));
        NclFree(grpnode);
        return(NULL);
    }

    grpnode->open = 1;

    if(status < 0)
        return((void*)grpnode);

    ncinquire(fid, &n_dims, &n_vars, &n_atts, &unlimited_dim_idx);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tn_dims = %d, n_vars = %d, n_atts = %d\n",
   *        n_dims, n_vars, n_atts);
   */

    if(n_atts)
        grpnode->att_rec = _NC4_get_atts(fid, NC_GLOBAL, n_atts);

    if(n_dims)
        grpnode->dim_rec = _NC4_get_dims(fid, n_dims, unlimited_dim_idx);

    if(n_vars)
        grpnode->var_rec = _NC4_get_vars(grpnode, n_vars, &has_scalar_dim,
                                         unlimited_dim_idx, NrmQuarkToString(grpnode->real_name));

    /*check chunking info*/
    if((NULL != grpnode->dim_rec) && (NULL != grpnode->var_rec) && (! grpnode->is_chunked))
    {
        grpnode->shuffle = 1;
        grpnode->compress_level = 0;
        i = 0;
        dimnode = &(grpnode->dim_rec->dim_node[i]);

        for(j = 0; j < grpnode->var_rec->n_vars; ++j)
        {
            varnode = &(grpnode->var_rec->var_node[j]);

            if(grpnode->compress_level < varnode->compress_level)
                grpnode->compress_level = varnode->compress_level;

            if(varnode->dim_rec->n_dims < 2)
                continue;

            if(varnode->is_chunked && (NULL != varnode->chunk_dim_rec))
            {
                grpnode->is_chunked = varnode->is_chunked;
                for(k = 0; k < varnode->chunk_dim_rec->n_dims; ++k)
                {
                    vardimnode = &(varnode->chunk_dim_rec->dim_node[k]);
                    if((NULL != vardimnode) && (NULL != dimnode) && (vardimnode->name == dimnode->name))
                    {
                        _addNclDimNode(&(grpnode->chunk_dim_rec), dimnode->name, vardimnode->id,
                                       vardimnode->size, vardimnode->is_unlimited);

                        ++i;
                        if(i < grpnode->dim_rec->n_dims)
                            dimnode = &(grpnode->dim_rec->dim_node[i]);
                        else
                            dimnode = NULL;
                    }
                }
            }
    
            if(i >= grpnode->dim_rec->n_dims)
                break;
        }
    }

    nc_inq_format(fid,&(grpnode->format));

    switch(grpnode->format)
    {
          case NC_FORMAT_NETCDF4:
               grpnode->kind = NrmStringToQuark("NETCDF4");
               break;
          case NC_FORMAT_NETCDF4_CLASSIC:
               grpnode->kind = NrmStringToQuark("NETCDF4 CLASSIC");
               break;
          case NC_FORMAT_64BIT:
               grpnode->kind = NrmStringToQuark("64BIT OFFSET");
               break;
          case NC_FORMAT_CLASSIC:
               grpnode->kind = NrmStringToQuark("CLASSIC");
               break;
          default:
               grpnode->kind = NrmStringToQuark("UNKNOWN");
               break;
    }

  /*Are there any user defined types?*/
    nc_inq_typeids(fid, &ntypes, NULL);
    if(ntypes)
    {
      /*
       *fprintf(stderr,"\tfile: %s, line: %d", __FILE__, __LINE__);
       *fprintf(stderr,"\tntypes: %d\n", ntypes);
       */

        grpnode->udt_rec = _NC4_get_udts(fid, NC_GLOBAL, ntypes);
    }

  /* See how many groups there are. */
    nc_ret = nc_inq_grps(fid, &numgrps, NULL);

    if(numgrps)
    {
      /*
       *fprintf(stderr,"\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr,"\tnumgrps = %d\n", numgrps);
       */

        grpnode->grp_rec = _NC4_get_grprec(fid, unlimited_dim_idx, grpnode);
    }

    if(has_scalar_dim)
    {
        if(NULL == grpnode->dim_rec)
            grpnode->dim_rec = _NclFileDimAlloc(1 + n_dims);
        else
            grpnode->dim_rec->dim_node = (NclFileDimNode *)NclRealloc(grpnode->dim_rec->dim_node,
                                                                      (1 + n_dims) * sizeof(NclFileDimNode));
        assert(grpnode->dim_rec->dim_node);
        grpnode->has_scalar_dim = 1;

        grpnode->dim_rec->dim_node[n_dims].id = -5;
        grpnode->dim_rec->dim_node[n_dims].size = 1;
        grpnode->dim_rec->dim_node[n_dims].is_unlimited = 0;
        grpnode->dim_rec->dim_node[n_dims].name = NrmStringToQuark("ncl_scalar");
    }
    else
    {
        grpnode->has_scalar_dim = 0;
    }

    if(n_dims)
        NC4GetDimVals(fid, grpnode);

    CloseOrSync(grpnode, fid, 0);

    return((void*)grpnode);
}

static void NC4FreeFileRec(void* therec)
{
    int ret;

    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;

    if(grpnode->open)
    {
        if((0 <= grpnode->fid) && (0 > grpnode->pid))
        {
            ret = ncclose(grpnode->fid);
            if(NC_NOERR != ret)
            {
              /*
               fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *NHLPERROR((NhlWARNING,NhlEUNKNOWN,(char*)nc_strerror(ret)));
               */
                NHLPERROR((NhlINFO,NhlEUNKNOWN,(char*)nc_strerror(ret)));
            }
            grpnode->fid = -1;
            grpnode->gid = -1;
        }
        grpnode->open = 0;
    }

    FileDestroyGrpNode(grpnode);
}

void NC4GetAttrVal(int ncid, int aid, NclFileAttNode *attnode)
{
    char *tmp;

  /*
   *fprintf(stderr, "\nEnter NC4GetAttrVal, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tncid = %d, aid = %d\n", ncid, aid);
   */

    if (attnode->the_nc_type < 1)
    {
        attnode->value = NULL;
        attnode->type = NCL_none;
    }
    else if(attnode->the_nc_type == NC_CHAR)
    {
        tmp = (char*)NclMalloc(attnode->n_elem+1);
        assert(tmp);
        ncattget(ncid,aid,NrmQuarkToString(attnode->name),tmp);
        tmp[attnode->n_elem] = '\0';
        attnode->value = NclMalloc(sizeof(NclQuark));
        assert(attnode->value);
        *(NclQuark *)attnode->value = NrmStringToQuark(tmp);
        NclFree(tmp);
        attnode->type = NCL_string;
        attnode->n_elem = 1;
    } 
    else if(attnode->the_nc_type > NC_MAX_ATOMIC_TYPE)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tattnode->the_nc_type: %d\n", attnode->the_nc_type);
       *fprintf(stderr, "\tattnode->n_elem: %d\n", attnode->n_elem);
       */

        set_special_attnode(ncid, aid, &attnode);
    }
    else
    {
        attnode->type = NC4MapToNcl(&(attnode->the_nc_type));
        if(NCL_string == attnode->type)
        {
            int n;
            char **ppc;
            NclQuark *pq;
            if(attnode->n_elem)
            {
                ppc = (char **)NclCalloc(attnode->n_elem, sizeof(char *));
                assert(ppc);
                attnode->value = NclMalloc(attnode->n_elem * sizeof(NclQuark));
                assert(attnode->value);
                pq = attnode->value;
                ncattget(ncid,aid,NrmQuarkToString(attnode->name), ppc);
                for(n = 0; n < attnode->n_elem; n++)
                {
                    pq[n] = NrmStringToQuark(ppc[n]);
                    free(ppc[n]);
                }
                free(ppc);
            }
            else
            {
                attnode->n_elem = 1;
                attnode->value = NclMalloc(attnode->n_elem * sizeof(NclQuark));
                assert(attnode->value);
                pq = attnode->value;
                pq[0] = NrmStringToQuark("");
            }
        }
        else
        {
            if(1 > attnode->n_elem)
                attnode->n_elem = 1;
            attnode->value = NclCalloc(attnode->n_elem, nctypelen(attnode->the_nc_type));
            assert(attnode->value);
            ncattget(ncid,aid,NrmQuarkToString(attnode->name),attnode->value);
        }
    }

  /*
   *fprintf(stderr, "\tattnode->name: <%s>, attnode->type: <%s>\n",
   *                 NrmQuarkToString(attnode->name),
   *                 _NclBasicDataTypeToName(attnode->type));
   *fprintf(stderr, "Leave NC4GetAttrVal, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return;
}

void NC4GetDimVals(int ncid, NclFileGrpNode *grpnode)
{
    NclFileDimNode *dimnode;
    NclFileVarNode *varnode;
    long start = 0;
    int i, n;

    if(NULL == grpnode->dim_rec)
        return;

    if(NULL == grpnode->var_rec)
        return;

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tncid = %d\n", ncid);
   */

    if(grpnode->dim_rec->n_dims < 1)
        return;

    for(i = 0; i < grpnode->dim_rec->n_dims; i++)
    {
        dimnode = &(grpnode->dim_rec->dim_node[i]);
        varnode = _getVarNodeFromNclFileGrpNode(grpnode, dimnode->name);

        if(NULL == varnode)
            continue;

        if(varnode->dim_rec->n_dims > 1)
            continue;

        if(NULL == varnode->value)
        {
            varnode->value = NclCalloc(nctypelen(varnode->the_nc_type) * dimnode->size, 1);

            if(NCL_string == varnode->type)
            {
                char **tmp_strs;
                NclQuark *qptr = (NclQuark *) varnode->value;

                tmp_strs = (char **)NclCalloc(dimnode->size, sizeof(char *));
                assert(tmp_strs);

                ncvargetg(varnode->gid,
                          varnode->id,
                          &start,
                          &(dimnode->size),
                          NULL,
                          NULL,
                          (char *)tmp_strs);

                for(n = 0; n < dimnode->size; n++)
                {
                    qptr[n] = NrmStringToQuark(tmp_strs[n]);
                    free(tmp_strs[n]);
                }

                free(tmp_strs);
            }
            else
            {
                ncvarget(ncid, varnode->id, &start, &(dimnode->size), varnode->value);
            }
        }

        _addNclCoordVarNode(&(grpnode->coord_var_rec), varnode);
    }
}

void CloseOrSync(NclFileGrpNode *rootgrp, int fid, int sync)
{
    int ret = NC_NOERR;

    if(sync)
    {
        ret = nc_sync(fid);
        rootgrp->open = 1;
        rootgrp->fid = fid;
        rootgrp->gid = fid;
    }
    else
    {
        if((0 == *(int *)(rootgrp->options[Ncl_SUPPRESS_CLOSE].values)) && rootgrp->open)
        {
            rootgrp->open = 0;
            rootgrp->pid = -1;
            rootgrp->fid = -1;
            rootgrp->gid = -1;
            ret = ncclose(fid);
        }
    }

    if(NC_NOERR != ret)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,(char*)nc_strerror(ret)));
    }
}

static void _checking_nc4_chunking(NclFileGrpNode *grpnode, int id)
{
    NclFileVarNode *varnode;

    NclFileDimNode *dimnode;
    NclFileDimNode *chunkdimnode;

    ng_size_t *dims;
    size_t *chunk_dims;
    size_t *var_chunk_dims;

    int deflate = 1;
    int deflate_level = 1;
    int nc_ret;
    int i = 0;
    int j = 0;
    int storage = NC_CHUNKED;

    if(NULL != grpnode->chunk_dim_rec)
    {
        dims = (ng_size_t *) NclCalloc(grpnode->dim_rec->n_dims, sizeof(ng_size_t));
        assert(dims);
        chunk_dims = (size_t *) NclCalloc(grpnode->dim_rec->n_dims, sizeof(size_t));
        assert(chunk_dims);
        var_chunk_dims = (size_t *) NclCalloc(grpnode->dim_rec->n_dims, sizeof(size_t));
        assert(var_chunk_dims);

        for(i = 0; i < grpnode->dim_rec->n_dims; i++)
        {
            dimnode = &(grpnode->dim_rec->dim_node[i]);
            chunkdimnode = &(grpnode->chunk_dim_rec->dim_node[i]);

            if(NULL == chunkdimnode)
            {
              /*
               *fprintf(stderr, "dim name: <%s> has no chunk name related.\n",
               *                 NrmQuarkToString(dimnode->name));
               *fprintf(stderr, "No more file-wise chunking and compress check.\n");
               */

                break;
            }

            if(dimnode->name != chunkdimnode->name)
            {
              /*
               *fprintf(stderr, "dim name: <%s> and chunk_dim name: <%s> are different.\n",
               *    NrmQuarkToString(dimnode->name), NrmQuarkToString(chunkdimnode->name));
               *fprintf(stderr, "No more file-wise chunking and compress check.\n");
               */

                break;
            }

            dims[i] = dimnode->size;

            chunk_dims[i] = (size_t)chunkdimnode->size;
        }

        if(NULL == grpnode->var_rec)
            return;

        StartNC4DefineMode(grpnode, id);

        for(j = 0; j < grpnode->var_rec->n_vars; j++)
        {
            varnode = &(grpnode->var_rec->var_node[j]);

            if(NULL == varnode->chunk_dim_rec)
            {
                for(i = 0; i < varnode->dim_rec->n_dims; i++)
                {
                    chunkdimnode = _getChunkDimNodeFromNclFileGrpNode(grpnode,
                                       varnode->dim_rec->dim_node[i].name);
                    _addNclDimNode(&(varnode->chunk_dim_rec),
                                   varnode->dim_rec->dim_node[i].name,
                                   chunkdimnode->size, chunkdimnode->id,
                                   varnode->dim_rec->dim_node[i].is_unlimited);
                    var_chunk_dims[i] = (size_t)chunkdimnode->size;
                }
	    }
	    else
            {
                for(i = 0; i < varnode->dim_rec->n_dims; i++)
                {
                    chunkdimnode = _getChunkDimNodeFromNclFileGrpNode(grpnode,
                                       varnode->dim_rec->dim_node[i].name);
                    var_chunk_dims[i] = (size_t)chunkdimnode->size;
                }
	    }

            if(varnode->is_chunked)
            {
                nc_ret = nc_def_var_chunking(id, varnode->id, storage, var_chunk_dims);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlINFO,NhlEUNKNOWN,
                          "%s: Error in nc_def_var_chunking in file (%s) for writing, at line: %d\n",
                                  __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
		    /*
                    check_err(nc_ret, __LINE__, __FILE__);
                    return;
		    */
                }

                if((grpnode->compress_level > 0) || (varnode->compress_level > 0))
                {
                    if(grpnode->compress_level > varnode->compress_level)
                        varnode->compress_level = grpnode->compress_level;
                    varnode->shuffle = *(int *)(grpnode->options[Ncl_SHUFFLE].values);
                    deflate_level = varnode->compress_level;

                  /*
                   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\t\tvarnode->shuffle = %d, compress_level = %d\n",
                   *                     varnode->shuffle, varnode->compress_level);
                   */

                    nc_ret = nc_def_var_deflate(id, varnode->id, varnode->shuffle,
                                deflate, deflate_level);
                }

                if(NULL == grpnode->options[Ncl_USE_CACHE].values)
                    varnode->use_cache = 0;
                else
                    varnode->use_cache = *(int *)(grpnode->options[Ncl_USE_CACHE].values);
                if(varnode->use_cache)
                {
                    int *isv = (int *)(grpnode->options[Ncl_CACHE_SIZE].values);
                    int *iev = (int *)(grpnode->options[Ncl_CACHE_NELEMS].values);
                    float *fv = (float *)(grpnode->options[Ncl_CACHE_PREEMPTION].values);
                    varnode->cache_size = isv[0];
                    varnode->cache_nelems = iev[0];
                    varnode->cache_preemption = fv[0];
                    nc_ret = nc_set_var_chunk_cache(id, varnode->id, varnode->cache_size,
                                                    varnode->cache_nelems,
                                                    varnode->cache_preemption);
                }
            }
        }

        EndNC4DefineMode(grpnode, id);

        free(dims);
        free(chunk_dims);
        free(var_chunk_dims);
    }
}

void StartNC4DefineMode(NclFileGrpNode *grpnode, int id)
{
    if(! grpnode->define_mode)
    {
       if(NC_NOERR != ncredef(id))
       {
           NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                  "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                  __FILE__, id, __LINE__));
       }
   }
   grpnode->define_mode = 1;
}

void EndNC4DefineMode(NclFileGrpNode *rootgrp, int id)
{
    if(rootgrp->define_mode)
    {
        if((NULL != rootgrp->var_rec) && (rootgrp->header_reserve_space > 0))
        {
            nc__enddef(id,rootgrp->header_reserve_space,4,0,4);
            rootgrp->header_reserve_space = 0;
        }
        else
        {
            nc_enddef(id);
        }
    }
    rootgrp->define_mode = 0;
}

/*
 *static NclFileVarNode *NC4GetCoordInfo(void* therec, NclQuark thevar)
 */
static NclFVarRec *NC4GetCoordInfo(void* therec, NclQuark thevar)
{
    return((NclFVarRec *)GetVarInfo(therec,thevar));
}

static void *NC4ReadVar(void *therec, NclQuark thevar,
                        long *start, long *finish, long *stride, void *storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclFileVarNode *varnode;
    NclFileDimNode *dimnode;
    void *out_data;
    size_t n_elem = 1;
    int fid = -1;
    int ret = NhlNOERROR;
    size_t i, k, n;
    int nc_ret = NC_NOERR;
    int get_all = 1;
    int no_stride = 1;
    size_t count[MAX_NC_DIMS];
    size_t locstart[MAX_NC_DIMS];

  /*
   *fprintf(stderr, "\nEnter NC4ReadVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   *fprintf(stderr, "\tgrpnode->gid = %d\n", grpnode->gid);
   */

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL != varnode)
    {
        if(NCL_compound != varnode->type)
        {
            if(varnode->value != NULL && varnode->dim_rec->n_dims == 1)
            if((1 == varnode->dim_rec->n_dims) && (NULL != varnode->value))
            {
                return GetCachedValue(varnode,start[0],finish[0],stride[0],storage);
            }
        }

        for(i = 0; i < varnode->dim_rec->n_dims; i++)
        {
            dimnode = &(varnode->dim_rec->dim_node[i]);
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tstart[%d] = %d, finish[%d] = %d, stride[%d] = %d\n",
           *                 i, start[i], i, finish[i], i, stride[i]);
           */
            count[i] = (int)floor((finish[i] - start[i])/(double)stride[i]) + 1;
            locstart[i] = start[i];
            n_elem *= count[i];
            if(stride[i] != 1)
            {
                no_stride = 0;
            }
            if(count[i] != (size_t) dimnode->size)
            {
                get_all = 0;
            }
        }

        out_data = storage;

        if(grpnode->open)
        {
            fid = grpnode->gid;
          /*
           *_checking_nc4_chunking(grpnode,fid);
           */
        }
        else
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tgrpnode->path: <%s>\n", NrmQuarkToString(grpnode->path));
           */

            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_NOWRITE,&ChunkSizeHint,&fid);
            grpnode->define_mode = 0;
            grpnode->fid = fid;
            grpnode->gid = fid;
        }
                
        if(nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for reading, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
            return(NULL);
        }

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tvarnode->gid: %d, varnode->id: %d\n", varnode->gid, varnode->id);
       */

        grpnode->open = 1;

        if(NCL_compound == varnode->type)
        {
            NclFileCompoundNode *compnode;
            char name[1024];
            int ndims, natts;
            size_t nfields, size;
            nc_type xtype;
            size_t offset;
            int dimids[64];
            void *values;
            long numval = 1;
            int  complength = 1;

            nc_type field_typeid;
            int field_sizes[64];
            int field_ndims = 0;

            int found = 0;

            char *struct_name = NULL;
            char *component_name = NULL;
          
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tvarnode->gid = %d, varnode->id = %d\n",
           *                   varnode->gid, varnode->id);
           */

            nc_inq_var(varnode->gid, varnode->id, name, &xtype, &ndims, dimids, &natts);
            varnode->udt_type = NCL_UDT_compound;

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tname: <%s>, xtype = %d, ndims = %d, natts = %d\n",
           *                   name, (int)xtype, ndims, natts);
           *fprintf(stderr, "\tndims = %d, dimids[0] = %d\n",
           *                   ndims, dimids[0]);
           *fprintf(stderr, "\tvarnode->dim_rec->n_dims = %d, varnode->dim_rec->dim_node[0].size = %d\n",
           *                   varnode->dim_rec->n_dims, varnode->dim_rec->dim_node[0].size);
           */

            nc_inq_compound(varnode->gid, xtype, name, &size, &nfields);

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tname: <%s>, nfields = %d, size = %d\n", name, (int)nfields, (int)size);
           */

            for(i = 0; i < varnode->dim_rec->n_dims; i++)
                numval *= varnode->dim_rec->dim_node[i].size;

            component_name =  _getComponentName(NrmQuarkToString(thevar), &struct_name);
	    NclFree(struct_name);
            if(NULL != component_name)
            {
                found = 0;
                for(n = 0; n < nfields; n++)
                {
                    nc_inq_compound_field(varnode->gid, xtype, n, name, &offset, &field_typeid,
                                         &field_ndims, field_sizes);
                  /*
                   *fprintf(stderr, "\n\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tname[%d]: <%s>\n", n, name);
                   *fprintf(stderr, "\toffset = %d, field_typeid = %d, field_ndims = %d, field_sizes[0] = %d\n",
                   *                   offset, field_typeid, field_ndims, field_sizes[0]);
                   */
                    if(0 == strcmp(component_name, name))
                    {
                        for(i = 0; i < varnode->comprec->n_comps; i++)
                        {
                            compnode = &(varnode->comprec->compnode[i]);
                          /*
                           *fprintf(stderr, "\tname[%d]: <%s>, compnode[%d]->name: <%s>\n", n, name,
                           *                   i, NrmQuarkToString(compnode->name));
                           */
                            if(0 == strcmp(NrmQuarkToString(compnode->name), component_name))
                            {
                                for(k = 0; k < field_ndims; k++)
                                    complength *= field_sizes[k];

                                ret = NhlNOERROR;
                                found = 1;
                              /*
                               *fprintf(stderr, "\toffset = %d, complength = %d\n", offset, complength);
                               *fprintf(stderr, "\tname[%d]: <%s>, compnode[%d]->name: <%s>\n", n, name,
                               *                  i, NrmQuarkToString(compnode->name));
                               */
                                break;
                            }
                            compnode = NULL;
                        }

                        if(found)
                            break;
                    }
                }

               if(found)
               {
                   values = NclCalloc(numval * size, 1);
                   assert(values);
               }
               else
               {
                   NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                       "NclNetCDF4: Can not find variable (%s) from file (%s)",
                       NrmQuarkToString(thevar),NrmQuarkToString(grpnode->path)));

                   return(NULL);
               }

               nc_get_var(varnode->gid, varnode->id, values);

               complength *= _NclSizeOf(compnode->type);

             /*
              *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
              *fprintf(stderr, "\toffset = %d, complength = %d\n", offset, complength);
              *fprintf(stderr, "\tnumval = %d, size = %d\n", numval, size);
              */

               for(i = 0; i < numval; i++)
               {
                   memcpy(out_data + i * complength, values + i * size + offset, complength);
               }

               free(values);
               ret = NhlNOERROR;
            }
            else
            {
              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tvarnode->name: <%s>\n", NrmQuarkToString(varnode->name));
               */

                varnode->udt_type = NCL_UDT_compound;
                storage = (void *)get_nc4_compoundlist(varnode->gid, varnode->id, n_elem,
                                                       start, finish, stride, get_all);
                ret = NhlNOERROR;
            }
	    NclFree(component_name);
        }
        else if(NCL_list == varnode->type)
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tvarnode->name: <%s>, varnode->type = %d\n",
           *                 NrmQuarkToString(varnode->name), varnode->type);
           */

            varnode->udt_type = NCL_UDT_vlen;
            storage = (void *)get_nc4_vlenlist(varnode->gid, varnode->id, varnode->the_nc_type, &varnode->base_type);
            ret = NhlNOERROR;
        }
        else if(NCL_opaque == varnode->type)
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tvarnode->name: <%s>, varnode->type = %d, varnode->the_nc_type = %d\n",
           *                 NrmQuarkToString(varnode->name), varnode->type, varnode->the_nc_type);
           */

            storage = (void *)get_nc4_opaque(varnode->gid, varnode->id, varnode->the_nc_type);
            ret = NhlNOERROR;
        }
        else if(NCL_enum == varnode->type)
        {
            NclFileEnumRecord *enumrec;
            void *values;
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tvarnode->name: <%s>, varnode->type = %d, varnode->the_nc_type = %d\n",
           *                 NrmQuarkToString(varnode->name), varnode->type, varnode->the_nc_type);
           */

            storage = (void *)get_nc4_enum(varnode->gid, varnode->id, varnode->the_nc_type);

          /*Let add two more attributes to varnode here.
           *1. an array of the enum names
           *2. an array of the enum value
           */

            enumrec = (NclFileEnumRecord *)storage;

            values = (void *)NclCalloc(enumrec->size, sizeof(NclQuark));
            assert(values);

            for(n = 0; n < enumrec->size; n++)
            {
                memcpy(values + n * sizeof(NclQuark), &(enumrec->enum_node[n].name), sizeof(NclQuark));
            }

            ret = _addNclAttNode(&(varnode->att_rec), NrmStringToQuark("enum_name"),
                                 NCL_string, enumrec->size, values);

            NclFree(values);

            values = (void *)NclCalloc(enumrec->size, _NclSizeOf(enumrec->type));
            assert(values);

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tenumrec->name: <%s>, enumrec->type = %d\n",
           *                 NrmQuarkToString(enumrec->name), enumrec->type);
           */

            switch(_NclSizeOf(enumrec->type))
            {
                case 1:
                     {
                          char tv;
                          for(n = 0; n < enumrec->size; n++)
                          {
                              tv = (char) enumrec->enum_node[n].value;
                              memcpy(values + n, &tv, 1);
                            /*
                             *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                             *fprintf(stderr, "\ttv[%d] = %d\n", n, tv);
                             */
                          }
                     }
                     break;
                case 2:
                     {
                          short tv;
                          for(n = 0; n < enumrec->size; n++)
                          {
                              tv = (short) enumrec->enum_node[n].value;
                              memcpy(values + 2 * n, &tv, 2);
                          }
                     }
                     break;
                case 4:
                     {
                          int tv;
                          for(n = 0; n < enumrec->size; n++)
                          {
                              tv = (int) enumrec->enum_node[n].value;
                              memcpy(values + 4 * n, &tv, 4);
                          }
                     }
                     break;
                default:
                     {
                          int64 tv;
                          for(n = 0; n < enumrec->size; n++)
                          {
                              tv = (int64) enumrec->enum_node[n].value;
                              memcpy(values + n * sizeof(int64), &tv, sizeof(int64));
                          }
                     }
            }

            ret = _addNclAttNode(&(varnode->att_rec), NrmStringToQuark("enum_value"),
                                 enumrec->type, enumrec->size, values);
            NclFree(values);
        }
        else if (NCL_string == varnode->type) {
                char **tmp_strs;
                NclQuark *qptr = (NclQuark *) out_data;

		/*
		 *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
		 *fprintf(stderr, "\tn_elem = %d\n", n_elem);
		 */

                tmp_strs = (char **)NclCalloc(n_elem, sizeof(char *));
                assert(tmp_strs);

		/*
		 *for(n = 0; n < n_elem; n++)
		 *{
		 *    fprintf(stderr, "\tn = %d\n", n);
		 *}
		 */
                
                if(no_stride)
                {    
			nc_ret = ncvargetg(varnode->gid,
					varnode->id,
					start,
					(long *)count,
					NULL,
					NULL,
					(char *)tmp_strs);
                }
                else
                {
			nc_ret = ncvargetg(varnode->gid,
					varnode->id,
					start,
					(long *)count,
					stride,
					NULL,
					(char *)tmp_strs);
                }

                for(n = 0; n < n_elem; n++)
                {
			/*
			 *fprintf(stderr, "\tstrin[%d]: <%s>\n", n, tmp_strs[n]);
			 */
			qptr[n] = NrmStringToQuark(tmp_strs[n]);
			free(tmp_strs[n]);
                }

                free(tmp_strs);
	}
	else {
                if(get_all)
                {
			nc_ret = nc_get_var(varnode->gid, varnode->id, out_data);
                }
                else if(no_stride)
                {
			nc_ret = nc_get_vara(varnode->gid, varnode->id,
					  locstart, count, out_data);
                }
                else
                {
			nc_ret = nc_get_vars(varnode->gid, varnode->id,
					  locstart, count, stride, out_data);
                }
	}


        CloseOrSync(grpnode, fid, 0);

        if(ret < NhlWARNING || nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                "NclNetCDF4: Error reading variable (%s) from file (%s)",
                NrmQuarkToString(thevar),NrmQuarkToString(grpnode->path)));

            return(NULL);
        }

      /*
       *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
       *fprintf(stderr, "Leave NC4ReadVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */

        return(storage);
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: Variable (%s) is not an element of file (%s)",
        NrmQuarkToString(thevar),NrmQuarkToString(grpnode->path));

    return(NULL);
}

static void *NC4ReadCoord
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
    return(NC4ReadVar(therec,thevar,start,finish,stride,storage));
}


static void *NC4ReadAtt(void *therec, NclQuark theatt, void *storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileAttNode *attnode;
    int i,fid,ret ;
    char *tmp;
    int nc_ret;

    for(i = 0; i < grpnode->att_rec->n_atts; i++)
    {
        attnode = &(grpnode->att_rec->att_node[i]);
        if(attnode->name == theatt)
        {
            if(attnode->value != NULL)
            {
                if(attnode->the_nc_type == NC_CHAR)
                {
                    *(NclQuark *)storage = *(NclQuark *)(attnode->value);
                }
                else
                {
                    memcpy(storage, attnode->value,
                           nctypelen(attnode->the_nc_type)*attnode->n_elem);
                }
                return(storage);
            }
            if (grpnode->open)
            {
                fid = grpnode->gid;
            }                
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_NOWRITE,&ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                          "NclNetCDF4: Could not reopen (%s) for reading",
                          NrmQuarkToString(grpnode->path));
                    return(NULL);
                }
                grpnode->open = 1;
                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 0;
            }
            
            if(attnode->the_nc_type == NC_CHAR)
            {
                tmp = (char *)NclCalloc(attnode->n_elem+1, sizeof(char));
                assert(tmp);
                tmp[attnode->n_elem] = '\0';
                ret = ncattget(fid,NC_GLOBAL,NrmQuarkToString(theatt),tmp);
                *(NclQuark *)storage = NrmStringToQuark(tmp);
                NclFree(tmp);
            }
            else
            {
                ret = ncattget(fid,NC_GLOBAL,NrmQuarkToString(theatt),storage);
            }

            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                if(grpnode->open)
                {
                    _checking_nc4_chunking(grpnode,fid);
                }
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }

            if (ret != -1) 
                return(storage);
            else
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: Error retrieving global attribute (%s) from (%s)",
                    NrmQuarkToString(theatt),NrmQuarkToString(grpnode->path));
                return NULL;
            }
        }
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: (%s) is not a global attribute of file (%s)",
        NrmQuarkToString(theatt),NrmQuarkToString(grpnode->path));
    return(NULL);
}

static void *NC4ReadVarAtt(void *therec, NclQuark thevar, NclQuark theatt, void *storage)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    NclFileAttNode *attnode;
    int fid;
    int nc_ret;
    int ret;
    char *tmp;

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

    if(NULL != varnode)
    {
        attnode = _getAttNodeFromNclFileVarNode(varnode, theatt);

        if(NULL != attnode)
        {
            if(NULL != attnode->value)
            {
                if(attnode->the_nc_type == NC_CHAR)
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
                    "NclNetCDF4: Error retrieving value for is_virtual attribute (%s) of (%s->%s)",
                    NrmQuarkToString(theatt),NrmQuarkToString(grpnode->name),NrmQuarkToString(thevar));
                return NULL;
            }

            if(grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_NOWRITE,&ChunkSizeHint,&fid);

                if(nc_ret != NC_NOERR) {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NULL);
                }
                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 0;
                grpnode->open = 1;
            }
            
            if(attnode->the_nc_type == NC_CHAR)
            {
                tmp = (char*)NclMalloc(attnode->n_elem + 1);
                tmp[attnode->n_elem] = '\0';
                ret = ncattget(fid, varnode->att_rec->id,NrmQuarkToString(theatt),tmp);
                *(NclQuark *)storage = NrmStringToQuark(tmp);
                NclFree(tmp);
            }
            else
            {
                ret = ncattget(fid,varnode->id,NrmQuarkToString(theatt),storage);
            }

          /*
           *CloseOrSync(grpnode, fid, 0);
           */

            if(ret != -1)
                return(storage);
            else
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"NclNetCDF4: Error retrieving value for Attribute (%s) of (%s->%s)",
                      NrmQuarkToString(theatt),NrmQuarkToString(grpnode->path),NrmQuarkToString(thevar));
                return NULL;
            }
        }
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: Attribute (%s) is not a variable attribute of (%s->%s)",
        NrmQuarkToString(theatt),NrmQuarkToString(grpnode->path),NrmQuarkToString(thevar));
    return(NULL);
}

static NhlErrorTypes NC4WriteVar(void *therec, NclQuark thevar, void *data,
                                 long *start, long *finish, long *stride)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    int fid;
    int nc_ret = NC_NOERR;
    NclFileVarNode *varnode;
    NclFileDimNode *dimnode;
    size_t count[MAX_NC_DIMS];
    size_t locstart[MAX_NC_DIMS];
    ng_size_t n_elem = 1;
    int in_whole = 1;
    int no_stride = 1;
    int i,n;
    int ret;
    int fill_mode;

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>\n", NrmQuarkToString(thevar));
   */

    if(grpnode->status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);

        if(NULL != varnode)
        {
          /*
           * for now, simply disable caching the value if a variable gets written to
           */
            if(NULL != varnode->value)
            {
                NclFree(varnode->value);
                varnode->value = NULL;
            }

            for(i = 0; i < varnode->dim_rec->n_dims; i++)
            {
                count[i] = (size_t)floor((finish[i] - start[i])/(double)stride[i]) + 1;
                locstart[i] = (size_t)start[i];
                n_elem *= (ng_size_t)count[i];
                if(stride[i] != 1)
                {
                    no_stride = 0;
                }
                dimnode = &(varnode->dim_rec->dim_node[i]);
                if(dimnode->is_unlimited)
                {
                    dimnode->size = MAX(finish[i] + 1, dimnode->size);
		    in_whole = 0;
                }

                if(0 != locstart[i] || (count[i] != (size_t)dimnode->size))
                    in_whole = 0;

              /*
               *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tDim %d: count[%d] = %ld, locstart[%d] = %ld\n",
	       *		i, i, count[i], i, locstart[i]);
	       *fprintf(stderr, "\tn_elem = %ld, no_stride = %d\n",
	       *		n_elem, no_stride);
	       *fprintf(stderr, "\tdimnode->is_unlimited = %d, dimnode->size = %d\n",
	       *		dimnode->is_unlimited, dimnode->size);
	       *fprintf(stderr, "\tfinish[%d] = %ld, stride[%d] = %ld\n",
               *                i, finish[i], i, stride[i]);
               */
            }
                    
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n\n", __FILE__, __LINE__);
           */

            if(grpnode->open)
            {
                fid = grpnode->gid;
                _checking_nc4_chunking(grpnode,fid);
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path), NC_WRITE,
                                 &ChunkSizeHint,&fid);
                grpnode->fid = fid;
                grpnode->gid = fid;

                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                              "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                              __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NhlFATAL);
                }
            }

            grpnode->open = 1;
            if(0 == *(int *)(grpnode->options[Ncl_PREFILL].values))
            {
                nc_set_fill(fid,NC_NOFILL,&fill_mode);

              /*
               *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
               *fprintf(stderr, "\tfill_mode = %d\n", fill_mode);
               */
            }

            EndNC4DefineMode(grpnode, fid);

            if(NCL_list == varnode->type)
            {
#if 0
                NclListObjList  *tmp = NULL;
                NclObj           tmpobj;
                NclVar           tmpvar;
                NclMultiDValData tmp_md;
                NclList          vlist    = (NclList)_NclGetObj(*(int *)data);
                nc_vlen_t       *vlendata = (nc_vlen_t *)NclCalloc(vlist->list.nelem,
                                                                  sizeof(nc_vlen_t));
                assert(vlendata);

                tmp = vlist->list.first;
                for(n = 0; n < vlist->list.nelem; n++)
                {
                    tmpobj = (NclObj)_NclGetObj(tmp->obj_id);
                    tmpvar = (NclVar)_NclGetObj(tmpobj->obj.id);
                    tmp_md = (NclMultiDValData)_NclGetObj(tmpvar->var.thevalue_id);

                    vlendata[n].p = tmp_md->multidval.val;
                    vlendata[n].len = 1;
                    for(i = 0; i < tmp_md->multidval.n_dims; i++)
                        vlendata[n].len *= tmp_md->multidval.dim_sizes[i];
                    tmp = tmp->next;
                }
#else
                NclListObjList  *list_list = NULL;
                NclObj           listobj;
                NclVar           listvar;
                NclMultiDValData val_md;
                NclList          vlist    = NULL;
                int*             dlist    = (int *)data;
                nc_vlen_t       *vlendata = NULL;

                vlendata = (nc_vlen_t *)NclCalloc(n_elem, sizeof(nc_vlen_t));
                assert(vlendata);

                for(n = 0; n < n_elem; ++n)
                {
                    vlist = (NclList)_NclGetObj(dlist[n]);
                    list_list = vlist->list.first;
                    listobj = (NclObj)_NclGetObj(list_list->obj_id);
                    listvar = (NclVar)_NclGetObj(listobj->obj.id);
                    val_md = (NclMultiDValData)_NclGetObj(listvar->var.thevalue_id);

                    vlendata[n].p = val_md->multidval.val;
                    vlendata[n].len = 1;
                    for(i = 0; i < val_md->multidval.n_dims; i++)
                        vlendata[n].len *= val_md->multidval.dim_sizes[i];
                }
#endif
                ret = nc_put_var(fid, varnode->id, vlendata);
                if(NC_NOERR != ret)
                    check_err(ret, __LINE__, __FILE__);

                NclFree(vlendata);
            }
            else if(NCL_string == varnode->type)
            {
                char **tmpstr = (char **)NclCalloc(n_elem, sizeof(char *));
                NclQuark *qd = (NclQuark *)data;

		for (i = 0; i < n_elem; i++) {
			tmpstr[i] = NrmQuarkToString(qd[i]);
		}
                if(no_stride && in_whole)
                {
                    ret = nc_put_var_string(fid, varnode->id, (const char **)tmpstr);
                }
                else
                {
                    ret = nc_put_vara_string(fid, varnode->id,
                                             locstart, count, (const char **)tmpstr);
                }

                NclFree(tmpstr);
            }
            else if(NCL_compound == varnode->type)
            {
                size_t n_dims = 0;
                size_t data_size = 1;
                void *data_value = NULL;
                int  *obj_id = (int *)data;

                NclMultiDValData theval = NULL;
                NclList        comp_list;
                NclListObjList *step = NULL;
                NclVar cur_var = NULL;

                NclFileCompoundRecord *comp_rec = varnode->comprec;
                NclFileCompoundNode   *compnode = NULL;

                n_dims = varnode->dim_rec->n_dims;

                data_size = 1;
                for(n = 0; n < n_dims; n++)
                {
                    dimnode = &(varnode->dim_rec->dim_node[n]);
                    data_size *= (size_t) dimnode->size;
                }

                if(NULL != comp_rec)
                {
                    size_t cur_mem_loc = 0;
                    size_t compound_size = 0;

                    size_t *mem_len = (size_t *)NclCalloc(comp_rec->n_comps, sizeof(size_t));
                    if (! mem_len)
                    {
                        NHLPERROR((NhlFATAL,ENOMEM,NULL));
                        return NhlFATAL;
                    }

                    if(comp_rec->n_comps)
                    {
                        for(n = 0; n < comp_rec->n_comps; n++)
                        {
                            compnode = &(comp_rec->compnode[n]);
        
                            mem_len[n] = ((size_t) _NclSizeOf(compnode->type)
                                         *(size_t) compnode->nvals);
                            compound_size += mem_len[n];
                        }
                    }
        
                    data_value = (void *)NclCalloc((ng_usize_t)(data_size*compound_size), sizeof(void));
                    assert(data_value);
        
                    cur_mem_loc = 0;

                    for(i = 0; i < data_size; ++i)
                    {
                        comp_list = (NclList)_NclGetObj(obj_id[i]);
                        step = comp_list->list.first;
                        for(n = 0; n < comp_rec->n_comps; ++n)
                        {
                            cur_var = (NclVar)_NclGetObj(step->obj_id);
                            if(Ncl_Var == cur_var->obj.obj_type)
                            {
                                theval = (NclMultiDValData)_NclGetObj(cur_var->var.thevalue_id);
                                memcpy(data_value + cur_mem_loc,
                                           theval->multidval.val, mem_len[n]);
        
                                cur_mem_loc += mem_len[n];
                            }
                            else
                            {
                                fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                                fprintf(stderr, "\tUnknown cur_var->obj.obj_type: 0%x\n", cur_var->obj.obj_type);
                            }

                            step = step->next;
                        }
                    }
        
                    if(no_stride)
                    {
                        if(in_whole)
                            ret = nc_put_var(fid, varnode->id, data_value);
                        else
                            ret = ncvarputg(fid, varnode->id,
                                            start, (long *)count, NULL, NULL,
                                            data_value);
                    }
                    else
                    {
                        ret = ncvarputg(fid, varnode->id,
                                        start, (long *)count, stride, NULL,
                                        data_value);
                    }

                    NclFree(data_value);
		    NclFree(mem_len);
                }
            }
            else if(NCL_enum == varnode->type)
            {
                ret = nc_put_var(fid, varnode->id, (unsigned char*)data);
            }
            else
            {
                if(no_stride)
                {
                    ret = ncvarputg(fid,
                          varnode->id,
                          start,
                          (long *)count,
                          NULL,
                          NULL,
                          data);
                }
                else
                {
                    ret = ncvarputg(fid,
                          varnode->id,
                          start,
                          (long *)count,
                          stride,
                          NULL,
                          data);
                }
            }
    
            if(NC_NOERR != ret)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: Error to write variable (%s) to file (%s)",
                     NrmQuarkToString(thevar),NrmQuarkToString(grpnode->path));
                return(NhlFATAL);
            }

          /*
           *fprintf(stderr, "Leave NC4WriteVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
           */
            return(NhlNOERROR);
        }
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: Error to write read-only file (%s)",
        NrmQuarkToString(grpnode->path));
    return(NhlFATAL);
}
    
static NhlErrorTypes NC4WriteCoord(void *therec, NclQuark thevar, void* data,
                                   long* start, long* finish,long* stride)
{
    return(NC4WriteVar(therec,thevar,data,start,finish,stride));
}


static NhlErrorTypes NC4WriteAtt(void *therec, NclQuark theatt, void *data)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileAttNode *attnode;
    int fid;
    int nc_ret;
    int ret = -1;
    char *buffer=NULL;

    if(grpnode->status <= 0)
    {
        attnode = _getAttNodeFromNclFileGrpNode(grpnode, theatt);
        if(NULL != attnode)
        {
            if(grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path), NC_WRITE,
                                 &ChunkSizeHint, &fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NhlFATAL);
                }
                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            if(attnode->the_nc_type == NC_CHAR)
            {
                buffer = NrmQuarkToString(*(NclQuark*)data);
                if((strlen(buffer)+1 > attnode->n_elem) || attnode->is_virtual)
                {
                    if (! grpnode->define_mode)
                    {
                        nc_ret = ncredef(fid);
                        if(nc_ret != NC_NOERR)
                        {
                            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                  "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                                  __FILE__, fid, __LINE__));
                            return(NhlFATAL);
                        }

                        grpnode->define_mode = 1;
                        ret = nc_put_att(fid, NC_GLOBAL,NrmQuarkToString(theatt),
                                         attnode->the_nc_type,strlen(buffer),(void*)buffer);
                    }

                    if(attnode->value != NULL)
                        memcpy(attnode->value,data,sizeof(NclQuark));
                    attnode->is_virtual = 0;
                }
            }
            else
            {
                if(attnode->is_virtual)
                {
                    if(! grpnode->define_mode)
                    {
                        nc_ret = ncredef(fid);
                        if(nc_ret != NC_NOERR)
                        {
                            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                                  "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                                  __FILE__, fid, __LINE__));
                            return(NhlFATAL);
                        }
                        grpnode->define_mode = 1;
                    }
                }
                ret = nc_put_att(fid, NC_GLOBAL, NrmQuarkToString(theatt),
                                 attnode->the_nc_type, attnode->n_elem, data);
                if (attnode->value != NULL)
                {
                    memcpy(attnode->value,data,
                            nctypelen(attnode->the_nc_type)*attnode->n_elem);
                }
                attnode->is_virtual = 0;
            }
    
            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode,fid,0);
               */
            }
                    
            if(ret == -1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: Error to write attribute (%s) to file (%s)",
                    NrmQuarkToString(theatt),NrmQuarkToString(grpnode->path));
                return(NhlFATAL);
            }
            return(NhlNOERROR);
        }    
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: Error to write attribute to read-only file (%s)",
        NrmQuarkToString(grpnode->path));
    return(NhlFATAL);
}

static NhlErrorTypes NC4DelAtt(void *therec, NclQuark theatt)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileAttNode *attnode;
    int j, fid;
    int nc_ret;
    int ret = 0;

    if(grpnode->status <= 0)
    {
        if(NULL == grpnode->att_rec)
            return(NhlNOERROR);

        for(j = 0; j < grpnode->att_rec->n_atts; j++)
        {
            attnode = &(grpnode->att_rec->att_node[j]);
            if(attnode->name != theatt)
                continue;

            if(! attnode->is_virtual)
            {
                if(grpnode->open)
                {
                    fid = grpnode->gid;
                }
                else
                {
                    nc_ret = nc__open(NrmQuarkToString(grpnode->path), NC_WRITE,
                                     &ChunkSizeHint,&fid);

                    if(nc_ret != NC_NOERR)
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                        return(NhlFATAL);
                    }
                    grpnode->fid = fid;
                    grpnode->gid = fid;
                    grpnode->define_mode = 1;
                    grpnode->open = 1;
                }

                if (! grpnode->define_mode)
                {
                  /*
                   *nc_ret = ncredef(fid);
                   *if(nc_ret != NC_NOERR)
                   *{
                   *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                   *          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                   *          __FILE__, fid, __LINE__));
                   *    return(NhlFATAL);
                   *}
                   */

                    grpnode->define_mode = 1;
                }

                ret = ncattdel(fid,NC_GLOBAL,(const char*)NrmQuarkToString(theatt));
                if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
                {
                    _checking_nc4_chunking(grpnode,fid);
                  /*
                   *CloseOrSync(grpnode, fid, 0);
                   */
                }
            }

          /*
           *_NclDelAtt(grpnode->att_rec, theatt);
           */
            _delNclAttNode(&(grpnode->att_rec), theatt);

            if(ret == -1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: Error to delete attribute (%s) from file (%s)",
                     NrmQuarkToString(theatt),NrmQuarkToString(grpnode->path));
                return(NhlFATAL);
            }
            return(NhlNOERROR);
        }
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: Not permit to write to read-only file (%s)",
        NrmQuarkToString(grpnode->path));
    return(NhlFATAL);
}

static NhlErrorTypes NC4DelVarAtt(void *therec, NclQuark thevar, NclQuark theatt)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    NclFileAttNode *attnode = NULL;
    int fid;
    int nc_ret;
    int ret = 0;

    if(grpnode->status <= 0)
    {
        varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
        if(NULL == varnode)
            return(NhlNOERROR);

        attnode = _getAttNodeFromNclFileVarNode(varnode, theatt);
        if(NULL == attnode)
            return(NhlNOERROR);

        if(! attnode->is_virtual)
        {
            if(grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path), NC_WRITE,
                                 &ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NhlFATAL);
                }
                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NhlFATAL);
                }
                grpnode->define_mode = 1;
            }

            ret = ncattdel(fid,varnode->id,(const char*)NrmQuarkToString(theatt));
            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
        }

      /*
       *_NclDelAtt(grpnode->att_rec, theatt);
       */
        _delNclAttNode(&(varnode->att_rec), theatt);

        if(ret == -1)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                  "NclNetCDF4: Error to delete attribute (%s) from variable (%s) in file (%s)",
                  NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(grpnode->path));
            return(NhlFATAL);
        }
        return(NhlNOERROR);
    } 

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: NO write to read-only file (%s)",
         NrmQuarkToString(grpnode->path));
    return(NhlFATAL);
}
static NhlErrorTypes NC4WriteVarAtt(void *therec, NclQuark thevar,
                                    NclQuark theatt, void* data)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileAttNode *attnode;
    NclFileVarNode *varnode;
    NclFileAttRecord *attrec;
    int fid;
    int nc_ret;
    int ret;
    char *buffer = NULL;

    if(grpnode->status > 0)
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclNetCDF4:NC4WriteVarAtt NOT permitted write to read-only file (%s)",
             NrmQuarkToString(grpnode->path));
        return(NhlFATAL);
    }

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
    if(NULL == varnode)
        return(NhlNOERROR);

    attnode = _getAttNodeFromNclFileVarNode(varnode, theatt);
    attrec = varnode->att_rec;
    if(NULL != attnode)
    {
        if (! attnode->is_virtual)
        {
            /* if the value is the same as before don't bother writing it */
            if(attnode->the_nc_type == NC_CHAR)
            {    
                if (*(NrmQuark*)attnode->value == *(NrmQuark*)data)
                    return NhlNOERROR;
            }
            else
            {
                memcmp(attnode->value,data,
                         nctypelen(attnode->the_nc_type)*attnode->n_elem);
                return NhlNOERROR;
            }
        }

        if (grpnode->open)
        {
            fid = grpnode->gid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                             &ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                return(NhlFATAL);
            }
            grpnode->fid = fid;
            grpnode->gid = fid;
            grpnode->define_mode = 1;
            grpnode->open = 1;
        }

        if(attnode->the_nc_type == NC_CHAR)
        {
            buffer = NrmQuarkToString(*(NclQuark*)data);
            if(strlen(buffer)+1 > attnode->n_elem || attnode->is_virtual)
            {
                if (! grpnode->define_mode)
                {
                    nc_ret = ncredef(fid);
                    if(nc_ret != NC_NOERR)
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                              "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                              __FILE__, fid, __LINE__));
                        return(NhlFATAL);
                    }
                    grpnode->define_mode = 1;
                }
            }
            ret = nc_put_att(fid, attrec->aid, NrmQuarkToString(theatt),
                             attnode->the_nc_type, strlen(buffer), buffer);
            if (ret != -1 && attnode->value != NULL)
                memcpy(attnode->value,data,sizeof(NclQuark));
            attnode->is_virtual = 0;
        }
        else
        {
            if(attnode->is_virtual)
            {
                if (! grpnode->define_mode)
                {
                    nc_ret = ncredef(fid);
                    if(nc_ret != NC_NOERR)
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                              "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                              __FILE__, fid, __LINE__));
                        return(NhlFATAL);
                    }
                    grpnode->define_mode = 1;
                }
            }
            ret = nc_put_att(fid,attrec->aid,NrmQuarkToString(theatt),
                             attnode->the_nc_type,attnode->n_elem,data);

            if (ret != -1 && attnode->value != NULL)
            {
                memcpy(attnode->value,data,
                       nctypelen(attnode->the_nc_type)*attnode->n_elem);
            }
            attnode->is_virtual = 0;
        }

        if(0 == *(int * )(grpnode->options[Ncl_DEFINE_MODE].values))
        {
            _checking_nc4_chunking(grpnode,fid);
          /*
           *CloseOrSync(grpnode, fid, 0);
           */
        }

        if(ret == -1)
        {
            if (theatt == NrmStringToQuark("_FillValue") && grpnode->format > 2)
            {
                NhlPError(NhlWARNING,NhlEUNKNOWN,
                    "NclNetCDF4: NetCDF 4 does not allow the _FillValue attribute to be modified after data written to variable (%s) in file (%s)",
                     NrmQuarkToString(thevar),NrmQuarkToString(grpnode->path));
                return (NhlWARNING);
            }
            else
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                     "NclNetCDF4: Error to write attribute (%s) to variable (%s) in file (%s)",
                      NrmQuarkToString(theatt),NrmQuarkToString(thevar),
                      NrmQuarkToString(grpnode->path));
                return(NhlFATAL);
            }
        }
    }

    return(NhlNOERROR);
}    

NhlErrorTypes NC4AddVarChunk(void* therec, NclQuark thevar,
                             int n_chunk_dims, ng_size_t *chunk_dims)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    NclFileDimRecord *dimrec = NULL;
    NclFileDimNode *dimnode;
    NclFileDimRecord *chunkdimrec = NULL;
    NclFileDimNode *chunkdimnode;
    size_t chunksizes[NCL_MAX_DIMENSIONS];
    int i,ret = NhlNOERROR;
    int fid;
    int nc_ret;

    int storage = NC_CHUNKED;

  /*
   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
   */

    if(grpnode->status > 0)
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "File (%s) was opened as read only, can not write to it",
                  NrmQuarkToString(grpnode->path));
        return (NhlFATAL);
    }

    if(grpnode->open)
    {
        fid = grpnode->gid;
    }
    else
    {
        nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                         &ChunkSizeHint,&fid);
        if(nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
            return(NhlFATAL);
        }
        grpnode->fid = fid;
        grpnode->gid = fid;
        grpnode->define_mode = 1;
        grpnode->open = 1;
    }

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
    if(NULL != varnode)
    {
        if(NULL != varnode->chunk_dim_rec)
        {
            if(n_chunk_dims != varnode->chunk_dim_rec->n_dims)
            {    
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                         "Var (%s) has different chunk_dims to its dimensionality.\n",
                          NrmQuarkToString(thevar));
                ret = NhlFATAL;
                return (ret);
            }
        }

     /*
      *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
      *fprintf(stderr, "\tn_chunk_dims = %d\n", n_chunk_dims);
      */

       chunkdimrec = _NclFileDimAlloc(n_chunk_dims);
       chunkdimrec->gid = grpnode->gid;
       varnode->chunk_dim_rec = chunkdimrec;
       varnode->is_chunked = 1;
       dimrec = varnode->dim_rec;

        for(i = 0 ; i < n_chunk_dims; i++)
        {
            dimnode = &(dimrec->dim_node[i]);
            chunkdimnode = &(chunkdimrec->dim_node[i]);
            chunkdimnode->id = i;
            chunkdimnode->name = dimnode->name;
            chunkdimnode->size = (ng_size_t)chunk_dims[i];
            chunksizes[i] = (size_t)chunk_dims[i];
          /*
           *fprintf(stderr, "\tcheck dim %d size %ld\n", i, (long)varnode->chunk_dim_rec->dim_node[i].size);
           */
        }

        if(! grpnode->define_mode)
        {
            nc_ret = ncredef(fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                      "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                      __FILE__, fid, __LINE__));
                return(NhlFATAL);
            }
            grpnode->define_mode = 1;
        }

        nc_ret = nc_def_var_chunking(fid, varnode->id, storage, chunksizes);
        if(nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Error in nc_def_var_chunking in file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
            return(NhlFATAL);
            check_err(nc_ret, __LINE__, __FILE__);
        }
        ret = NhlNOERROR;
    }

    return (ret);
}

static NhlErrorTypes NC4AddVarChunkCache(void* therec, NclQuark thevar,
                                         ng_size_t cache_size, ng_size_t cache_nelems,
                                         float cache_preemption)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    int ret = NhlNOERROR;
    int fid;
    int nc_ret;

    if(grpnode->status > 0)
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "NclNetCDF4: File (%s) was opened as read only, can not write to it",
                  NrmQuarkToString(grpnode->path));
        return (NhlFATAL);
    }

    if (grpnode->open)
    {
        fid = grpnode->gid;
    }
    else
    {
        nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                         &ChunkSizeHint,&fid);
        if(nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
            return(NhlFATAL);
        }
        grpnode->fid = fid;
        grpnode->gid = fid;
        grpnode->define_mode = 1;
        grpnode->open = 1;
    }

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
    if(NULL != varnode)
    {
        if((cache_size > 0) && (cache_nelems > 0))
            varnode->use_cache = 1;
        varnode->cache_size = cache_size;
        varnode->cache_nelems = cache_nelems;
        if(cache_preemption < 0.0)
            varnode->cache_preemption = 0.0;
        else if(cache_preemption > 1.0)
            varnode->cache_preemption = 1.0;
        else
            varnode->cache_preemption = cache_preemption;

        if(varnode->use_cache)
        {
            nc_ret = nc_set_var_chunk_cache(fid, varnode->id,
                                            cache_size, cache_nelems,
                                            varnode->cache_preemption);
        }
        ret = NhlNOERROR;
    }

    return(ret);
}

static NhlErrorTypes NC4SetVarCompressLevel(void* therec, NclQuark thevar,
                                            int compress_level)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    NhlErrorTypes ret = NhlNOERROR;
    int fid;
    int deflate = 1;
    int nc_ret;

    if(grpnode->status > 0)
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "NclNetCDF4: File (%s) was opened as read only, can not write to it",
                  NrmQuarkToString(grpnode->path));
        return (NhlFATAL);
    }

    if (grpnode->open)
    {
        fid = grpnode->gid;
    }
    else
    {
        nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                         &ChunkSizeHint,&fid);
        if(nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
            return(NhlFATAL);
        }
        grpnode->fid = fid;
        grpnode->gid = fid;
        grpnode->define_mode = 1;
        grpnode->open = 1;
    }

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
    if(NULL != varnode)
    {
        varnode->compress_level = compress_level;
        if(compress_level > 0)
            deflate = 1;
        varnode->shuffle = *(int *)(grpnode->options[Ncl_SHUFFLE].values);

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\t\tvarnode->shuffle = %d, compress_level = %d\n",
       *                     varnode->shuffle, varnode->compress_level);
       */

        nc_ret = nc_def_var_deflate(fid, varnode->id, varnode->shuffle,
                                    deflate, varnode->compress_level);
    }

    return(ret);
}

static NhlErrorTypes NC4AddDim(void* therec, NclQuark thedim,
                               ng_size_t size, int is_unlimited)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    int fid;
    int nc_ret;
    int add_scalar = 0;
    int dimidp = (int)size;

  /*
   *fprintf(stderr, "\nEnter NC4AddDim, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthedim: <%s>, size: %ld\n", NrmQuarkToString(thedim), size);
   *fprintf(stderr, "\tgrpnode->gid = %ld\n", grpnode->gid);
   */

    if(grpnode->status <=  0)
    {
        if(thedim == NrmStringToQuark("ncl_scalar"))
        {
            if (size != 1)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: \"ncl_scalar\" is a reserved file dimension name in NCL, this name can only represent dimensions of size 1");
                return(NhlFATAL);
            }
            add_scalar = 1;
        }
        else
        {
            if(grpnode->open)
            {
                fid = grpnode->gid;
                grpnode->define_mode = 1;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                                 &ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NhlFATAL);
                }
                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            if(! grpnode->define_mode)
            {
              /*
               *nc_ret = ncredef(fid);
               *if(nc_ret != NC_NOERR)
               *{
               *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
               *          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
               *          __FILE__, fid, __LINE__));
               *    return(NhlFATAL);
               *}
               */
                grpnode->define_mode = 1;
            }

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tfid = %d, grpnode->define_mode = %d\n", fid, grpnode->define_mode);
           *fprintf(stderr, "\tis_unlimited = %d\n", is_unlimited);
           *fprintf(stderr, "\tfid = %d, thedim: <%s>, size = %ld\n",
           *                   fid, NrmQuarkToString(thedim), (long)size);
           */

            if(is_unlimited)
            {
                nc_ret = nc_def_dim(fid, NrmQuarkToString(thedim), NC_UNLIMITED, &dimidp);
            }
            else
            {
                nc_ret = nc_def_dim(fid, NrmQuarkToString(thedim), (size_t)size, &dimidp);
            }

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tnc_ret = %d, NhlFATAL = %d\n", nc_ret, NhlFATAL);
           *fprintf(stderr, "\tfid = %d, thedim: <%s>, size = %ld, did = %d, nc_ret = %d\n",
           *                   fid, NrmQuarkToString(thedim), (long)size, dimidp, nc_ret);
           */

            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }

            if(nc_ret != NC_NOERR)
            {
                nc_strerror(nc_ret);

                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: Error to define dim: <%s> with length: %ld\n",
                     NrmQuarkToString(thedim), (long)size));
                return(NhlFATAL);
            }
        }

        if(add_scalar)
        {
            NclQuark ns_name = NrmStringToQuark("ncl_scalar");
            grpnode->has_scalar_dim = 1;

            _addNclDimNode(&(grpnode->dim_rec), ns_name, dimidp, -5, 1);
        }
        else
        {
            _addNclDimNode(&(grpnode->dim_rec), thedim, dimidp, size, is_unlimited);
        }
      /*
       *nc_ret = grpnode->dim_rec->n_dims - 1;
       *fprintf(stderr, "\tthedim: <%s>, dimid: %ld\n", NrmQuarkToString(thedim),
       *                 grpnode->dim_rec->dim_node[nc_ret].id);
       *fprintf(stderr, "Leave NC4AddDim, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */
        return(NhlNOERROR);
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclNetCDF4: File (%s) was opened as read only, can not write to it",
             NrmQuarkToString(grpnode->path));
    }
    return(NhlFATAL);
}

static NhlErrorTypes NC4AddChunkDim(void* therec, NclQuark thedim,
                                    ng_size_t size, int is_unlimited)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileDimNode *dimnode = NULL;
    int fid;
    int nc_ret;
    int add_scalar = 0;

  /*
   *fprintf(stderr, "\nEnter NC4AddChunkDim, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthedim: <%s>, size: %d\n", NrmQuarkToString(thedim), size);
   */

    if(grpnode->status <=  0)
    {
        grpnode->is_chunked = 1;
        if(thedim == NrmStringToQuark("ncl_scalar"))
        {
            if(size != 1)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: \"ncl_scalar\" is a reserved file dimension name in NCL, %s",
                    "this name can only represent dimensions of size 1"));
                return(NhlFATAL);
            }
            add_scalar = 1;
        }
        else
        {
            if(grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                                 &ChunkSizeHint,&fid);

                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NhlFATAL);
                }
                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NhlFATAL);
                }
                grpnode->define_mode = 1;
            }
        }

        dimnode = _getDimNodeFromNclFileGrpNode(grpnode, thedim);
        if(NULL == grpnode->chunk_dim_rec)
        {
            int n = 0;
            NclFileDimRecord *dimrec = NULL;

            dimrec = grpnode->dim_rec;
            grpnode->chunk_dim_rec = _NclFileDimAlloc(dimrec->n_dims);
            grpnode->chunk_dim_rec->n_dims = dimrec->n_dims;

            for(n = 0; n < dimrec->n_dims; ++n)
	    {
                grpnode->chunk_dim_rec->dim_node[n].name = dimrec->dim_node[n].name;
                grpnode->chunk_dim_rec->dim_node[n].size = dimrec->dim_node[n].size;
                grpnode->chunk_dim_rec->dim_node[n].id   = dimrec->dim_node[n].id;
                grpnode->chunk_dim_rec->dim_node[n].is_unlimited = 0;
            }
        }

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tthedim: <%s>, size: %d\n", NrmQuarkToString(thedim), size);
       */

        if (add_scalar)
        {
            NclQuark ns_name = NrmStringToQuark("ncl_scalar");
            grpnode->has_scalar_dim = 1;

            dimnode = _getDimNodeFromNclFileGrpNode(grpnode, ns_name);
            _addNclChunkDimNode(&(grpnode->chunk_dim_rec), ns_name, dimnode->id, -5, 1);
        }
        else
        {
            _addNclChunkDimNode(&(grpnode->chunk_dim_rec), thedim, dimnode->id, size, is_unlimited);
        }
      /*
       *fprintf(stderr, "Leave NC4AddChunkDim, file: %s, line: %d\n\n", __FILE__, __LINE__);
       */
        return(NhlNOERROR);
    }
    else
    {    
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "NclNetCDF4: File (%s) was opened as read only, can not write to it",
             NrmQuarkToString(grpnode->path)));
    }
    return(NhlFATAL);
}

static NhlErrorTypes NC4AddVar(void* therec, NclQuark thevar,
                               NclBasicDataTypes data_type, int n_dims,
                               NclQuark *dim_names, long *dim_sizes)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode = NULL;
    int fid,i,j,ret;
    int nc_ret;
    nc_type *the_data_type;
    int dim_ids[MAX_NC_DIMS];
    int add_scalar_dim = 0;
    int fill_mode;
    int shuffle = 1;
    int deflate = 1;
    int deflate_level = 1;

  /*
   *fprintf(stderr, "\nEnter NC4AddVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>, type: <%s>, n_dims = %d\n",
   *                   NrmQuarkToString(thevar),
   *                   _NclBasicDataTypeToName(data_type), n_dims);
   */

    if(grpnode->status <= 0)
    {
        if(grpnode->open)
        {
            fid = grpnode->gid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                             &ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                return(NhlFATAL);
            }
            grpnode->fid = fid;
            grpnode->gid = fid;
            grpnode->define_mode = 1;
            grpnode->open = 1;
        }

        if(0 == *(int *)(grpnode->options[Ncl_PREFILL].values))
        {
            nc_set_fill(fid,NC_NOFILL,&fill_mode);

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tfill_mode = %d\n", fill_mode);
           */
        }

        the_data_type = NC4MapFromNcl(data_type);
/*
* All dimensions are correct dimensions for the file
*/
        dim_ids[0] = -999;
        if(NULL != grpnode->dim_rec)
        {
        for(i = 0; i < n_dims; i++)
        {
            for(j = 0; j < grpnode->dim_rec->n_dims; j++)
            {
                if(grpnode->dim_rec->dim_node[j].name == dim_names[i])
                {
                    if((n_dims > 1)&&(dim_names[i] == NrmStringToQuark("ncl_scalar")))
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "NclNetCDF4: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return(NhlFATAL);
                    }
                    dim_ids[i] = grpnode->dim_rec->dim_node[j].id;
                  /*
                   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\tdimname[%d]: <%s>, dimid[%d] = %d\n",
                   *                 i, NrmQuarkToString(dim_names[i]), i, dim_ids[i]);
                   */
                    break;
                }
            }
        } 
        } 

        if (dim_ids[0] == -999)
        {
            if (n_dims == 1 && dim_names[0] == NrmStringToQuark("ncl_scalar") &&
               (1 == dim_sizes[0] || (-5 == dim_sizes[0])))
            {
                dim_ids[0] = -5;
                add_scalar_dim = 1;
            }
            else
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "NclNetCDF4: internal error adding variable");
                return(NhlFATAL);
            }
        }

        if(the_data_type != NULL)
        {
            int var_id = -1;
            if(! grpnode->define_mode)
            {
              /*
               *nc_ret = ncredef(fid);
               *if(nc_ret != NC_NOERR)
               *{
               *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
               *          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
               *          __FILE__, fid, __LINE__));
               *    return(NhlFATAL);
               *}
               */
                grpnode->define_mode = 1;
            }

            if((n_dims == 1)&&(dim_ids[0] == -5))
            {
                ret = nc_def_var(fid,NrmQuarkToString(thevar),*the_data_type,
                                 0, NULL,&var_id);
            }
            else
            {
                shuffle = *(int *)(grpnode->options[Ncl_SHUFFLE].values);
                deflate = 1;
                deflate_level = *(int *)(grpnode->options[Ncl_COMPRESSION_LEVEL].values);

                ret = nc_def_var(fid,NrmQuarkToString(thevar),
                         *the_data_type, n_dims, dim_ids, &var_id);

                if((ret == NC_NOERR) && (grpnode->format > 2) && (deflate_level > 0))
                {
                  /*
                   *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                   *fprintf(stderr, "\t\tshuffle = %d, compress_level = %d\n",
                   *                    shuffle, deflate_level);
                   */
                    ret  = nc_def_var_deflate(fid,var_id,shuffle,deflate,deflate_level);
                }
            }

            if(ret < 0)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(ret)));
                NclFree(the_data_type);
                return(NhlFATAL);
            } 

            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
    
            _addNclVarNodeToGrpNode(grpnode, thevar, var_id, data_type,
                                    n_dims, dim_names, dim_sizes);

            i = grpnode->var_rec->n_vars - 1;
            varnode = &(grpnode->var_rec->var_node[i]);
            varnode->gid = grpnode->gid;
            varnode->shuffle = *(int *)(grpnode->options[Ncl_SHUFFLE].values);
            for(i = 0 ; i< n_dims; i++)
            {
                varnode->dim_rec->dim_node[i].id = dim_ids[i];
              /*
               *varnode->chunk_dim_rec->dim_node[i].id = dim_ids[i];
               */
            }

            if(add_scalar_dim)
            {
                NclQuark dim_name = NrmStringToQuark("ncl_scalar");
                grpnode->has_scalar_dim = 1;

                _addNclDimNode(&(grpnode->dim_rec), dim_name, -999, -5, 1);
            }
            NclFree(the_data_type);

          /*
           *fprintf(stderr, "\tthevar: <%s>, id: var_id = %d\n", 
           *                   NrmQuarkToString(thevar), varnode->id);
           *fprintf(stderr, "Leave NC4AddVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
           */
            return(NhlNOERROR);
        }
        else
        {
            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclNetCDF4: File (%s) was opened as a read only file, can not write to it",
             NrmQuarkToString(grpnode->path));
    }
    return(NhlFATAL);
}

static NhlErrorTypes NC4RenameDim(void* therec, NclQuark from, NclQuark to)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    int i,fid,ret;
    int nc_ret;

    if(to == NrmStringToQuark("ncl_scalar"))
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclNetCDF4 : \"ncl_scalar\" is a reserved file dimension name in NCL: other dimensions can not be changed to it");
        return(NhlFATAL);
    }

    for(i = 0; i < grpnode->dim_rec->n_dims; i++)
    {
        if(grpnode->dim_rec->dim_node[i].name == from)
        {
            if(grpnode->dim_rec->dim_node[i].id == -5)
            {
                grpnode->dim_rec->dim_node[i].name = to;
                return(NhlNOERROR);
            }

            if (grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                                 &ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    return(NhlFATAL);
                }

                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NhlFATAL);
                }
                grpnode->define_mode = 1;
            }

            ret = ncdimrename(fid,grpnode->dim_rec->dim_node[i].id,NrmQuarkToString(to));

            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }

            if(ret == -1)
            {
                return(NhlFATAL);
            }
            else
            {
                grpnode->dim_rec->dim_node[i].name = to;
                return(NhlNOERROR);
            }
        }
    }
    return(NhlFATAL);
}

static NhlErrorTypes NC4AddAtt(void *therec, NclQuark theatt,
                               NclBasicDataTypes data_type, int n_items, void * values)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    nc_type *the_data_type;
    int i,ret;
    int fid;
    int nc_ret;
    
    if(grpnode->status <= 0)
    {
        the_data_type = (nc_type*)NC4MapFromNcl(data_type);
        if(the_data_type != NULL)
        {
            if(grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                                 &ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    NclFree(the_data_type);
                    return(NhlFATAL);
                }

                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            if(! grpnode->define_mode)
            {
              /*
               *nc_ret = ncredef(fid);
               *if(nc_ret != NC_NOERR)
               *{
               *    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
               *          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
               *          __FILE__, fid, __LINE__));
               *    return(NhlFATAL);
               *}
               */
                grpnode->define_mode = 1;
            }

            if(NC_STRING == *the_data_type)
            {
                NclQuark  *qv     = (NclQuark *)values;
                char     **tmpstr = (char **)NclCalloc(n_items, sizeof(char *));
                assert(tmpstr);
                for(i = 0; i < n_items; i++)
                {
                    tmpstr[i] = (char *)NclCalloc(1 + strlen(NrmQuarkToString(qv[i])), sizeof(char));
                    assert(tmpstr[i]);
                    strcpy(tmpstr[i], NrmQuarkToString(qv[i]));
                }

                ret = nc_put_att_string(fid, NC_GLOBAL, NrmQuarkToString(theatt),
                                        (size_t)n_items, (const char **)tmpstr);
                for(i = 0; i < n_items; i++)
                {
                    NclFree(tmpstr[i]);
                }
                NclFree(tmpstr);
            }
            else
            {
                ret = nc_put_att(fid, NC_GLOBAL, NrmQuarkToString(theatt),
                                 *the_data_type, n_items, values);
            }

            if(ret != -1)
            {
                ret = _addNclAttNode(&(grpnode->att_rec), theatt,
                                     data_type, n_items, values);
                NclFree(the_data_type);
                return(NhlNOERROR);
            } 

            if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode,fid,0);
               */
            }
        } 
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NclNetCDF4: File (%s) was opened as a read only file, can not write to it",
             NrmQuarkToString(grpnode->path));
    }
    return(NhlFATAL);
}

static NhlErrorTypes NC4AddVarAtt(void *therec, NclQuark thevar, NclQuark theatt,
                                  NclBasicDataTypes data_type, int n_items, void * values)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    nc_type *the_data_type;
    int i;
    int fid,ret;
    int nc_ret;
    
  /*
   *fprintf(stderr, "\nEnter NC4AddVarAtt, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>, theatt: <%s>\n",
   *        NrmQuarkToString(thevar), NrmQuarkToString(theatt));
   */

    if(grpnode->status <= 0)
    {
        the_data_type = (nc_type*)NC4MapFromNcl(data_type);
        if(the_data_type != NULL)
        {
            if(grpnode->open)
            {
                fid = grpnode->gid;
            }
            else
            {
                nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                                 &ChunkSizeHint,&fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                    NclFree(the_data_type);
                    return(NhlFATAL);
                }

                grpnode->fid = fid;
                grpnode->gid = fid;
                grpnode->define_mode = 1;
                grpnode->open = 1;
            }

            varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
            if(NULL != varnode)
            {
                if (! grpnode->define_mode)
                {
                    nc_ret = ncredef(fid);
                    if(nc_ret != NC_NOERR)
                    {
                        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                              "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                              __FILE__, fid, __LINE__));
                        return(NhlFATAL);
                    }
                    grpnode->define_mode = 1;
                }

                if(NC_STRING == *the_data_type)
                {
                    NclQuark  *qv     = (NclQuark *)values;
                    char     **tmpstr = (char **)NclCalloc(n_items, sizeof(char *));
                    assert(tmpstr);
                    for(i = 0; i < n_items; i++)
                    {
                        tmpstr[i] = (char *)NclCalloc(1 + strlen(NrmQuarkToString(qv[i])), sizeof(char));
                        assert(tmpstr[i]);
                        strcpy(tmpstr[i], NrmQuarkToString(qv[i]));
                    }

                    ret = nc_put_att_string(fid, varnode->id, NrmQuarkToString(theatt),
                                            (size_t)n_items, (const char **)tmpstr);
                    for(i = 0; i < n_items; i++)
                    {
                        NclFree(tmpstr[i]);
                    }
                    NclFree(tmpstr);
                }
                else
                {
                    ret = nc_put_att(fid, varnode->id, NrmQuarkToString(theatt),
                                 *the_data_type, n_items, values);
                }

                if(ret != -1)
                {
                    ret = _addNclAttNode(&(varnode->att_rec), theatt,
                                         data_type, n_items, values);
                    NclFree(the_data_type);
                  /*
                   *fprintf(stderr, "Leave NC4AddVarAtt, file: %s, line: %d\n\n", __FILE__, __LINE__);
                   */
                    return(NhlNOERROR);
                } 

                if(0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
                {
                    _checking_nc4_chunking(grpnode,fid);
                  /*
                   *CloseOrSync(grpnode, fid, 0);
                   */
                }
            } 
        } 
    }

    NhlPError(NhlFATAL,NhlEUNKNOWN,
        "NclNetCDF4: File (%s) was opened as a read only file, can not write to it",
         NrmQuarkToString(grpnode->path));
    return(NhlFATAL);
}

static NhlErrorTypes NC4SetOption(void *rootgrp, NclQuark option,
                                  NclBasicDataTypes data_type, int n_items, void *values)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)rootgrp;

  /*
   *fprintf(stderr, "\nEnter NC4SetOption, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\toption: <%s>\n", NrmQuarkToString(option));
   */

    if (option ==  NrmStringToQuark("prefill"))
    {
        _NclCopyOption(&grpnode->options[Ncl_PREFILL], option, data_type, n_items, values);
    }
    else if (option == NrmStringToQuark("definemode"))
    {
        _NclCopyOption(&grpnode->options[Ncl_DEFINE_MODE], option, data_type, n_items, values);
        if((0 == *(int *)(grpnode->options[Ncl_DEFINE_MODE].values)) && grpnode->open &&
           (1 == grpnode->define_mode))
        {
             StartNC4DefineMode(grpnode, grpnode->gid);
        }
    }
    else if (option == NrmStringToQuark("headerreservespace"))
    {
        _NclCopyOption(&grpnode->options[Ncl_HEADER_RESERVE_SPACE], option, data_type, n_items, values);
        grpnode->header_reserve_space = *(int *) grpnode->options[Ncl_HEADER_RESERVE_SPACE].values;
    }
    else if (option == NrmStringToQuark("suppressclose"))
    {
        _NclCopyOption(&grpnode->options[Ncl_SUPPRESS_CLOSE], option, data_type, n_items, values);
        if((0 == *(int *)(grpnode->options[Ncl_SUPPRESS_CLOSE].values)) && grpnode->open)
        {
            CloseOrSync(grpnode,grpnode->fid,1);
        }
    }
    else if (option == NrmStringToQuark("format"))
    {
        _NclCopyOption(&grpnode->options[Ncl_FORMAT], option, data_type, n_items, values);
    }
    else if (option == NrmStringToQuark("missingtofillvalue"))
    {
        _NclCopyOption(&grpnode->options[Ncl_MISSING_TO_FILL_VALUE], option, data_type, n_items, values);
    }
    else if (option == NrmStringToQuark("shuffle"))
    {
        _NclCopyOption(&grpnode->options[Ncl_SHUFFLE], option, data_type, n_items, values);

      /*
       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\t\tset shuffle = %d\n", *(int *)values);
       */
    }
    else if (option == NrmStringToQuark("compressionlevel"))
    {
        if (*(int*)values < -1 || *(int*)values > 9)
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                  "NC4SetOption: option (%s) value cannot be less than -1 or greater than 9",NrmQuarkToString(option));
            return(NhlWARNING);
        }
        _NclCopyOption(&grpnode->options[Ncl_COMPRESSION_LEVEL], option, data_type, n_items, values);
    }
    else if (option == NrmStringToQuark("usecache"))
    {
        _NclCopyOption(&grpnode->options[Ncl_USE_CACHE], option, data_type, n_items, values);
    }
    else if (option == NrmStringToQuark("cachesize"))
    {
        if (*(int*)values < 1)
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                  "NC4SetOption: option (%s) value cannot be less than 1",NrmQuarkToString(option));
            return(NhlWARNING);
        }
        _NclCopyOption(&grpnode->options[Ncl_CACHE_SIZE], option, data_type, n_items, values);
    }
    else if (option == NrmStringToQuark("cachenelems"))
    {
        if (*(int*)values < 3)
        {
            NhlPError(NhlWARNING,NhlEUNKNOWN,
                  "NC4SetOption: option (%s) value cannot be less than 3",NrmQuarkToString(option));
            return(NhlWARNING);
        }
        else
        {
            _NclCopyOption(&grpnode->options[Ncl_CACHE_NELEMS], option, data_type, n_items, values);
        }
    }
    else if (option == NrmStringToQuark("cachepreemption"))
    {
        _NclCopyOption(&grpnode->options[Ncl_CACHE_PREEMPTION], option, data_type, n_items, values);
    }

  /*
   *fprintf(stderr, "\toption: <%s>\n", NrmQuarkToString(option));
   *fprintf(stderr, "Leave NC4SetOption, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    
    return NhlNOERROR;
}

NhlErrorTypes NC4AddGrp(void *rec, NclQuark grpname)
{
    NclFileGrpNode *rootgrpnode = (NclFileGrpNode *) rec;
    NhlErrorTypes ret = NhlNOERROR;
    char buffer[NC_MAX_NAME];
    int id;
    int n;

    strcpy(buffer, NrmQuarkToString(grpname));
    n = nc_def_grp(rootgrpnode->gid, buffer, &id);

    if(NC_NOERR != n)
        check_err(n, __LINE__, __FILE__);

    ret = AddNewGrp(rec, grpname, (size_t)id);

    return ret;
}

static NhlErrorTypes NC4AddVlenVar(void* therec, NclQuark thevar, NclBasicDataTypes ncl_type,
                                   nc_type vlen_type_id, ng_size_t n_dims,
                                   NclQuark *dim_names, long *dim_sizes)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode = NULL;
    int fid,i,j;
    int nc_ret;
    int dim_ids[MAX_NC_DIMS];

  /*
   *fprintf(stderr, "\nEnter NC4AddVlenVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>, vlen_type_id: %d, n_dims = %d\n",
   *                   NrmQuarkToString(thevar),
   *                   vlen_type_id, n_dims);
   */

    if(grpnode->status <= 0)
    {
        memset(dim_ids, 0, MAX_NC_DIMS * sizeof(int));

        if(grpnode->open)
        {
            fid = grpnode->gid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                             &ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                check_err(nc_ret, __LINE__, __FILE__);
                return(NhlFATAL);
            }
            grpnode->fid = fid;
            grpnode->gid = fid;
            grpnode->define_mode = 1;
            grpnode->open = 1;
        }

        dim_ids[0] = -999;
        for(i = 0; i < n_dims; i++)
        {
            for(j = 0; j < grpnode->dim_rec->n_dims; j++)
            {
                if(grpnode->dim_rec->dim_node[j].name == dim_names[i])
                {
                    if(NrmStringToQuark("ncl_scalar") == dim_names[i])
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "NC4AddVlenVar: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return(NhlFATAL);
                    }
                    dim_ids[i] = grpnode->dim_rec->dim_node[j].id;
                    break;
                }
            }
        } 

        if (dim_ids[0] == -999)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                "NC4AddVlenVar: internal error adding variable");
            return(NhlFATAL);
        }
        else
        {
            int var_id = -1;
            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NhlFATAL);
                }
                grpnode->define_mode = 1;
            }

            nc_ret = nc_def_var(fid, NrmQuarkToString(thevar),
                                vlen_type_id, n_dims, dim_ids, &var_id);

            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(nc_ret)));
                check_err(nc_ret, __LINE__, __FILE__);
                return(NhlFATAL);
            } 

            if(0== *(int *)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
    
            _addNclVarNodeToGrpNode(grpnode, thevar, var_id, NCL_list,
                                    n_dims, dim_names, dim_sizes);

            i = grpnode->var_rec->n_vars - 1;
            varnode = &(grpnode->var_rec->var_node[i]);
            varnode->gid = grpnode->gid;
            for(i = 0 ; i < n_dims; i++)
            {
                varnode->dim_rec->dim_node[i].id = dim_ids[i];
            }

            varnode->base_type = ncl_type;
            varnode->udt_type = NCL_UDT_vlen;

          /*
           *fprintf(stderr, "\tthevar: <%s>, id: var_id = %d\n", 
           *                   NrmQuarkToString(thevar), varnode->id);
           *fprintf(stderr, "Leave NC4AddVlenVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
           */
            return(NhlNOERROR);
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NC4AddVlenVar: File (%s) was opened as a read only file, can not write to it",
             NrmQuarkToString(grpnode->path));
    }

    return(NhlFATAL);
}

NhlErrorTypes NC4AddVlen(void *rec, NclQuark vlen_name, NclQuark var_name,
                         NclQuark type, NclQuark *dim_names, ng_size_t n_dims)
{
    NclFileGrpNode *rootgrpnode = (NclFileGrpNode *) rec;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *dimnode;
    int nc_ret = 0;

    NclBasicDataTypes ncl_type;
    nc_type *nc_base_type;
    nc_type nc_vlen_type_id;

    NclQuark *mem_name;
    NclBasicDataTypes *mem_type;

    long *dim_sizes;
    ng_size_t n = 0;

    mem_name = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
    assert(mem_name);
    mem_type = (NclBasicDataTypes *)NclCalloc(1, sizeof(NclBasicDataTypes));
    assert(mem_type);

    dim_sizes = (long *)NclCalloc(n_dims, sizeof(long));
    assert(dim_sizes);

  /*
   *fprintf(stderr, "\nEnter NC4AddVlen, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvlen_name: <%s>, var_name: <%s>, type: <%s>, dim_name[0]: <%s>\n",
   *                 NrmQuarkToString(vlen_name), NrmQuarkToString(var_name),
   *                 NrmQuarkToString(type), NrmQuarkToString(dim_name[0]));
   */

  /*
   *Use this function to define a variable length array type.
   *Usage
   *     nc_def_vlen(int ncid, const char *name, nc_type base_typeid, nc_type *xtypep);
   *ncid
   *    The ncid of the file to create the VLEN type in.
   *name
   *    A name for the VLEN type.
   *base_typeid
   *    The typeid of the base type of the VLEN. For example,
   *    for a VLEN of shorts, the base type is NC_SHORT.
   *    This can be a user defined type.
   *xtypep
   *    A pointer to an nc_type variable.
   *    The typeid of the new VLEN type will be set here.
   */

    ncl_type = _nameToNclBasicDataType(type);
    nc_base_type = NC4MapFromNcl(ncl_type);
    nc_ret = nc_def_vlen(rootgrpnode->gid, NrmQuarkToString(vlen_name),
                         *nc_base_type, &nc_vlen_type_id);

    if(NC_NOERR != nc_ret)
        check_err(nc_ret, __LINE__, __FILE__);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnc_vlen_type_id = %d\n", nc_vlen_type_id);
   */

    mem_name[0] = vlen_name;
    mem_type[0] = ncl_type;

    _NC4_add_udt(&(rootgrpnode->udt_rec),
                  rootgrpnode->gid, nc_vlen_type_id, vlen_name,
                  NC_VLEN, *nc_base_type,
                  0, 1, mem_name, mem_type);

    NclFree(nc_base_type);

    for(n = 0; n < n_dims; ++n)
    {
        dimnode = _getDimNodeFromNclFileGrpNode(rootgrpnode, dim_names[n]);
        dim_sizes[n] = (long) dimnode->size;
    }
    ret =  NC4AddVlenVar(rec, var_name, ncl_type, nc_vlen_type_id, n_dims, dim_names, dim_sizes);

  /*
   *NclFree(mem_name);
   *NclFree(mem_type);
   */
    NclFree(dim_sizes);

  /*
   *fprintf(stderr, "\tdim_sizes[0]= %d\n", dim_sizes[0]);
   *fprintf(stderr, "Leave NC4AddVlen, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

static NhlErrorTypes NC4AddEnumVar(void* therec, NclQuark thevar,
                                   nc_type enum_type_id, int n_dims,
                                   NclQuark *dim_names, long *dim_sizes,
                                   NclBasicDataTypes ncl_type)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode = NULL;
    int fid,i,j;
    int nc_ret;
    int dim_ids[MAX_NC_DIMS];

  /*
   *fprintf(stderr, "\nEnter NC4AddEnumVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>, enum_type_id: %d, n_dims = %d\n",
   *                   NrmQuarkToString(thevar),
   *                   enum_type_id, n_dims);
   */

    if(grpnode->status <= 0)
    {
        memset(dim_ids, 0, MAX_NC_DIMS * sizeof(int));

        if(grpnode->open)
        {
            fid = grpnode->gid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                             &ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                check_err(nc_ret, __LINE__, __FILE__);
                return(NhlFATAL);
            }
            grpnode->fid = fid;
            grpnode->gid = fid;
            grpnode->define_mode = 1;
            grpnode->open = 1;
        }

        dim_ids[0] = -999;
        for(i = 0; i < n_dims; i++)
        {
            for(j = 0; j < grpnode->dim_rec->n_dims; j++)
            {
                if(grpnode->dim_rec->dim_node[j].name == dim_names[i])
                {
                    if(NrmStringToQuark("ncl_scalar") == dim_names[i])
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "NC4AddEnumVar: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return(NhlFATAL);
                    }
                    dim_ids[i] = grpnode->dim_rec->dim_node[j].id;
                    break;
                }
            }
        } 

        if (dim_ids[0] == -999)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                "NC4AddEnumVar: internal error adding variable");
            return(NhlFATAL);
        }
        else
        {
            int var_id = -1;
            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NhlFATAL);
                }
                grpnode->define_mode = 1;
            }

            nc_ret = nc_def_var(fid, NrmQuarkToString(thevar),
                                enum_type_id, n_dims, dim_ids, &var_id);

            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(nc_ret)));
                check_err(nc_ret, __LINE__, __FILE__);
                return(NhlFATAL);
            } 

            if(0 == *(int*)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
    
            _addNclVarNodeToGrpNode(grpnode, thevar, var_id, NCL_enum,
                                    n_dims, dim_names, dim_sizes);

            i = grpnode->var_rec->n_vars - 1;
            varnode = &(grpnode->var_rec->var_node[i]);
            varnode->gid = grpnode->gid;
            varnode->base_type = ncl_type;
            for(i = 0 ; i < n_dims; i++)
            {
                varnode->dim_rec->dim_node[i].id = dim_ids[i];
            }

          /*
           *fprintf(stderr, "\tthevar: <%s>, id: var_id = %d\n", 
           *                   NrmQuarkToString(thevar), varnode->id);
           *fprintf(stderr, "Leave NC4AddEnumVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
           */
            return(NhlNOERROR);
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NC4AddEnumVar: File (%s) was opened as a read only file, can not write to it",
             NrmQuarkToString(grpnode->path));
    }

    return(NhlFATAL);
}

NhlErrorTypes NC4AddEnum(void *rec, NclQuark enum_name, NclQuark var_name,
                         NclQuark dim_name, NclQuark *mem_name, void *mem_value,
                         ng_size_t n_mems, NclBasicDataTypes val_type)
{
    NclFileGrpNode *rootgrpnode = (NclFileGrpNode *) rec;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *dimnode;
    int n = -1;
    int nc_ret = 0;

    nc_type *nc_base_type;
    nc_type nc_enum_type_id;

    NclQuark *udt_mem_name;
    NclBasicDataTypes *udt_mem_type;

    int n_dims = 1;
    NclQuark *dim_names;
    long *dim_sizes;

    void *enum_single_value = (void *)NclCalloc(1, _NclSizeOf(val_type));
    assert(enum_single_value);

    udt_mem_name = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
    assert(udt_mem_name);
    udt_mem_type = (NclBasicDataTypes *)NclCalloc(1, sizeof(NclBasicDataTypes));
    assert(udt_mem_type);

    dim_names = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
    assert(dim_names);
    dim_sizes = (long *)NclCalloc(1, sizeof(long));
    assert(dim_sizes);

  /*
   *fprintf(stderr, "\nEnter NC4AddEnum, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tenum_name: <%s>, var_name: <%s>, dim_name: <%s>\n",
   *                 NrmQuarkToString(enum_name), NrmQuarkToString(var_name),
   *                 NrmQuarkToString(dim_name));
   */

    nc_base_type = NC4MapFromNcl(val_type);
    nc_ret = nc_def_enum(rootgrpnode->gid, *nc_base_type,
                         NrmQuarkToString(enum_name), &nc_enum_type_id);

    if(NC_NOERR != nc_ret)
        check_err(nc_ret, __LINE__, __FILE__);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnc_enum_type_id = %d\n", nc_enum_type_id);
   */

    for(n = 0; n < n_mems; n++)
    {
        memcpy(enum_single_value, mem_value + _NclSizeOf(val_type)*n, _NclSizeOf(val_type));
        nc_insert_enum(rootgrpnode->gid, nc_enum_type_id,
                       NrmQuarkToString(mem_name[n]), enum_single_value);
    }

    NclFree(enum_single_value);

    udt_mem_name[0] = enum_name;
    udt_mem_type[0] = val_type;

    _NC4_add_udt(&(rootgrpnode->udt_rec),
                  rootgrpnode->gid, nc_enum_type_id, enum_name,
                  NC_ENUM, *nc_base_type,
                  0, 1, udt_mem_name, udt_mem_type);

    NclFree(nc_base_type);

    dimnode = _getDimNodeFromNclFileGrpNode(rootgrpnode, dim_name);
    dim_names[0] = dim_name;
    dim_sizes[0] = (long) dimnode->size;
    ret =  NC4AddEnumVar(rec, var_name, nc_enum_type_id, n_dims, dim_names, dim_sizes,
                         val_type);

  /*
   *NclFree(udt_mem_name);
   *NclFree(udt_mem_type);
   */
    NclFree(dim_names);
    NclFree(dim_sizes);

  /*
   *fprintf(stderr, "\tdim_sizes[0]= %d\n", dim_sizes[0]);
   *fprintf(stderr, "Leave NC4AddEnum, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

static NhlErrorTypes NC4AddOpaqueVar(void* therec, NclQuark thevar,
                                   nc_type opaque_type_id, int n_dims,
                                   NclQuark *dim_names, long *dim_sizes)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode = NULL;
    int fid,i,j;
    int nc_ret;
    int dim_ids[MAX_NC_DIMS];

  /*
   *fprintf(stderr, "\nEnter NC4AddOpaqueVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>, opaque_type_id: %d, n_dims = %d\n",
   *                   NrmQuarkToString(thevar),
   *                   opaque_type_id, n_dims);
   */

    if(grpnode->status <= 0)
    {
        memset(dim_ids, 0, MAX_NC_DIMS * sizeof(int));

        if(grpnode->open)
        {
            fid = grpnode->gid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                             &ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                check_err(nc_ret, __LINE__, __FILE__);
                return(NhlFATAL);
            }
            grpnode->fid = fid;
            grpnode->gid = fid;
            grpnode->define_mode = 1;
            grpnode->open = 1;
        }

        dim_ids[0] = -999;
        for(i = 0; i < n_dims; i++)
        {
            for(j = 0; j < grpnode->dim_rec->n_dims; j++)
            {
                if(grpnode->dim_rec->dim_node[j].name == dim_names[i])
                {
                    if(NrmStringToQuark("ncl_scalar") == dim_names[i])
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "NC4AddOpaqueVar: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return(NhlFATAL);
                    }
                    dim_ids[i] = grpnode->dim_rec->dim_node[j].id;
                    break;
                }
            }
        } 

        if (dim_ids[0] == -999)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                "NC4AddOpaqueVar: internal error adding variable");
            return(NhlFATAL);
        }
        else
        {
            int var_id = -1;
            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NhlFATAL);
                }
                grpnode->define_mode = 1;
            }

            nc_ret = nc_def_var(fid, NrmQuarkToString(thevar),
                                opaque_type_id, n_dims, dim_ids, &var_id);

            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(nc_ret)));
                check_err(nc_ret, __LINE__, __FILE__);
                return(NhlFATAL);
            } 

            if(0== *(int*)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
    
            _addNclVarNodeToGrpNode(grpnode, thevar, var_id, NCL_ubyte,
                                    n_dims, dim_names, dim_sizes);

            i = grpnode->var_rec->n_vars - 1;
            varnode = &(grpnode->var_rec->var_node[i]);
            varnode->gid = grpnode->gid;
            for(i = 0 ; i < n_dims; i++)
            {
                varnode->dim_rec->dim_node[i].id = dim_ids[i];
            }

          /*
           *fprintf(stderr, "\tthevar: <%s>, id: var_id = %d\n", 
           *                   NrmQuarkToString(thevar), varnode->id);
           *fprintf(stderr, "Leave NC4AddOpaqueVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
           */
            return(NhlNOERROR);
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "NC4AddOpaqueVar: File (%s) was opened as a read only file, can not write to it",
             NrmQuarkToString(grpnode->path));
    }

    return(NhlFATAL);
}

NhlErrorTypes NC4AddOpaque(void *rec, NclQuark opaque_name, NclQuark var_name,
                           int var_size, NclQuark dim_name)
{
    NclFileGrpNode *rootgrpnode = (NclFileGrpNode *) rec;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *dimnode;
    int nc_ret = 0;

    nc_type nc_opaque_type_id;

    NclQuark *mem_name;
    NclBasicDataTypes *mem_type;

    int n_dims = 1;
    NclQuark *dim_names;
    long *dim_sizes;

    mem_name = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
    assert(mem_name);
    mem_type = (NclBasicDataTypes *)NclCalloc(1, sizeof(NclBasicDataTypes));
    assert(mem_type);

    dim_names = (NclQuark *)NclCalloc(1, sizeof(NclQuark));
    assert(dim_names);
    dim_sizes = (long *)NclCalloc(1, sizeof(long));
    assert(dim_sizes);

  /*
   *fprintf(stderr, "\nEnter NC4AddOpaque, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\topaque_name: <%s>, var_name: <%s>, var_size: %d, dim_name: <%s>\n",
   *                 NrmQuarkToString(opaque_name), NrmQuarkToString(var_name),
   *                 var_size, NrmQuarkToString(dim_name));
   */

    nc_ret = nc_def_opaque(rootgrpnode->gid, var_size,
                           NrmQuarkToString(opaque_name), &nc_opaque_type_id);

    if(NC_NOERR != nc_ret)
        check_err(nc_ret, __LINE__, __FILE__);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tnc_opaque_type_id = %d\n", nc_opaque_type_id);
   */

    mem_name[0] = opaque_name;
    mem_type[0] = NCL_ubyte;

    _NC4_add_udt(&(rootgrpnode->udt_rec),
                  rootgrpnode->gid, nc_opaque_type_id, opaque_name,
                  NC_OPAQUE, NC_UBYTE,
                  0, 1, mem_name, mem_type);

    dimnode = _getDimNodeFromNclFileGrpNode(rootgrpnode, dim_name);
    dim_names[0] = dim_name;
    dim_sizes[0] = (long) dimnode->size;
    ret =  NC4AddOpaqueVar(rec, var_name, nc_opaque_type_id, n_dims, dim_names, dim_sizes);


    NclFree(mem_name);
    NclFree(mem_type);
   
    NclFree(dim_names);
    NclFree(dim_sizes);

  /*
   *fprintf(stderr, "\tdim_sizes[0]= %d\n", dim_sizes[0]);
   *fprintf(stderr, "Leave NC4AddOpaque, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

static NclFileVarNode *defNC4CompoundVar(void* therec, NclQuark thevar,
                                        nc_type compound_type_id, int n_dims,
                                        NclQuark *dim_names, long *dim_sizes)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode = NULL;
    int fid,i,j;
    int nc_ret;
    int dim_ids[MAX_NC_DIMS];

  /*
   *fprintf(stderr, "\nEnter defNC4CompoundVar, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tthevar: <%s>, compound_type_id: %d, n_dims = %d\n",
   *                   NrmQuarkToString(thevar),
   *                   compound_type_id, n_dims);
   */

    if(grpnode->status <= 0)
    {
        memset(dim_ids, 0, MAX_NC_DIMS * sizeof(int));

        if(grpnode->open)
        {
            fid = grpnode->gid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                             &ChunkSizeHint,&fid);
            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
                check_err(nc_ret, __LINE__, __FILE__);
                return (varnode);
            }
            grpnode->fid = fid;
            grpnode->gid = fid;
            grpnode->define_mode = 1;
            grpnode->open = 1;
        }

        dim_ids[0] = -999;
        for(i = 0; i < n_dims; i++)
        {
            for(j = 0; j < grpnode->dim_rec->n_dims; j++)
            {
                if(grpnode->dim_rec->dim_node[j].name == dim_names[i])
                {
                    if(NrmStringToQuark("ncl_scalar") == dim_names[i])
                    {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                            "defNC4CompoundVar: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
                        return (varnode);
                    }
                    dim_ids[i] = grpnode->dim_rec->dim_node[j].id;
                    break;
                }
            }
        } 

        if (dim_ids[0] == -999)
        {
            NhlPError(NhlFATAL,NhlEUNKNOWN,
                "defNC4CompoundVar: internal error adding variable");
            return (varnode);
        }
        else
        {
            int var_id = -1;
            if(! grpnode->define_mode)
            {
                nc_ret = ncredef(fid);
                if(nc_ret != NC_NOERR)
                {
                    NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not redef the file id (%d) for writing, at line: %d\n",
                          __FILE__, fid, __LINE__));
                    return(NULL);
                }
                grpnode->define_mode = 1;
            }

            nc_ret = nc_def_var(fid, NrmQuarkToString(thevar),
                                compound_type_id, n_dims, dim_ids, &var_id);

            if(nc_ret != NC_NOERR)
            {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(nc_ret)));
              /*
               *check_err(nc_ret, __LINE__, __FILE__);
               */
                return (varnode);
            } 

            if(0 == *(int*)(grpnode->options[Ncl_DEFINE_MODE].values))
            {
                _checking_nc4_chunking(grpnode,fid);
              /*
               *CloseOrSync(grpnode, fid, 0);
               */
            }
    
            _addNclVarNodeToGrpNode(grpnode, thevar, var_id, NCL_ubyte,
                                    n_dims, dim_names, dim_sizes);

            i = grpnode->var_rec->n_vars - 1;
            varnode = &(grpnode->var_rec->var_node[i]);
            varnode->gid = grpnode->gid;
            for(i = 0 ; i < n_dims; i++)
            {
                varnode->dim_rec->dim_node[i].id = dim_ids[i];
            }

            varnode->type = NCL_compound;
            varnode->is_compound = 1;

          /*
           *fprintf(stderr, "\tthevar: <%s>, id: var_id = %d\n", 
           *                   NrmQuarkToString(thevar), varnode->id);
           *fprintf(stderr, "Leave defNC4CompoundVar, file: %s, line: %d\n\n", __FILE__, __LINE__);
           */
            return (varnode);
        }
    }
    else
    {    
        NhlPError(NhlFATAL,NhlEUNKNOWN,
            "defNC4CompoundVar: File (%s) was opened as a read only file, can not write to it",
             NrmQuarkToString(grpnode->path));
    }

    return (varnode);
}

NhlErrorTypes NC4AddCompound(void *rec, NclQuark compound_name, NclQuark var_name,
                             ng_size_t n_dims, NclQuark *dim_name, ng_size_t n_mems,
                             NclQuark *mem_name, NclQuark *mem_type, int *mem_size)
{
    NclFileGrpNode   *grpnode = (NclFileGrpNode *) rec;
    int n = -1;
    int nc_ret = 0;
    NhlErrorTypes ret = NhlNOERROR;

    nc_type nc_compound_type_id;

    NclQuark *udt_mem_name = NULL;
    NclBasicDataTypes *udt_mem_type = NULL;

    size_t compound_length = 0;
    size_t component_size  = 4;
    size_t *mem_offset = NULL;
    int ims = 0;
    nc_type **mem_nc_type;
    nc_type *tmp_nc_type;

  /*
   *fprintf(stderr, "\nEnter NC4AddCompound, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tcompound_name: <%s>, var_name: <%s>\n",
   *                 NrmQuarkToString(compound_name), NrmQuarkToString(var_name));
   */

    udt_mem_name = (NclQuark *)NclCalloc(n_mems, sizeof(NclQuark));
    assert(udt_mem_name);
    udt_mem_type = (NclBasicDataTypes *)NclCalloc(n_mems, sizeof(NclBasicDataTypes));
    assert(udt_mem_type);

    mem_nc_type = (nc_type **)NclCalloc(n_mems, sizeof(NclBasicDataTypes *));
    assert(mem_nc_type);

    mem_offset = (size_t *)NclCalloc(n_mems, sizeof(size_t));
    assert(mem_offset);

    for(n = 0; n < n_mems; n++)
    {
        udt_mem_name[n] = mem_name[n];
        udt_mem_type[n] = _nameToNclBasicDataType(mem_type[n]);

        mem_nc_type[n] = NC4MapFromNcl(udt_mem_type[n]);

        if(0 == n)
           mem_offset[n] = 0;
        else
        {
           mem_offset[n] = mem_offset[n-1] + component_size;
        }

        if(mem_size[n] < 1)
            mem_size[n] = 1;

        component_size = get_sizeof(mem_size[n], _NclSizeOf(udt_mem_type[n]));
        compound_length += component_size;

      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tmem_size[%d] = %d\n", n, mem_size[n]);
       *fprintf(stderr, "\tmem: %d, name: <%s>, type: <%s>, size: %ld, offset: %ld\n",
       *                 n, NrmQuarkToString(mem_name[n]), NrmQuarkToString(mem_type[n]),
       *                 (long)component_size, (long)mem_offset[n]);
       */
    }

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tcompound_length = %d\n", compound_length);
   */

    nc_ret = nc_def_compound(grpnode->gid, compound_length,
                             NrmQuarkToString(compound_name),
                             &nc_compound_type_id);

    if(NC_NOERR != nc_ret)
        check_err(nc_ret, __LINE__, __FILE__);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpnode->gid = %d\n", grpnode->gid);
   *fprintf(stderr, "\tcompound_name: <%s>\n", NrmQuarkToString(compound_name));
   *fprintf(stderr, "\tnc_compound_type_id = %d\n", nc_compound_type_id);
   */

    for(n = 0; n < n_mems; n++)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tmem_offset[%d] = %d\n", n, mem_offset[n]);
       */

        tmp_nc_type = mem_nc_type[n];
        if(mem_size[n] > 1)
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tarray component nc_type = %d, NC_INT = %d\n", *tmp_nc_type, NC_INT);
           *fprintf(stderr, "\tarray component mem_size[%d] = %d\n", n, mem_size[n]);
           *fprintf(stderr, "\tarray component mem_name[%d]: <%s>\n",
           *                   n, NrmQuarkToString(mem_name[n]));
           */

            ims = (int) mem_size[n];
            nc_insert_array_compound(grpnode->gid, nc_compound_type_id,
                                     NrmQuarkToString(mem_name[n]),
                                     mem_offset[n], *tmp_nc_type, 1, &ims);
        }
        else
        {
          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tsimple component nc_type = %d, NC_INT = %d\n", *tmp_nc_type, NC_INT);
           *fprintf(stderr, "\tsimple component mem_size[%d] = %d\n", n, mem_size[n]);
           *fprintf(stderr, "\tsimple component mem_name[%d]: <%s>\n",
           *                   n, NrmQuarkToString(mem_name[n]));
           */

            nc_insert_compound(grpnode->gid, nc_compound_type_id,
                               NrmQuarkToString(mem_name[n]),
                               mem_offset[n], *tmp_nc_type);
        }
    }

    _NC4_add_udt(&(grpnode->udt_rec),
                  grpnode->gid, nc_compound_type_id, compound_name,
                  NC_COMPOUND, NC_COMPOUND,
                  compound_length, n_mems, udt_mem_name, udt_mem_type);

    if(n_dims)
    {
        NclFileDimNode   *dimnode = NULL;
        NclFileVarNode   *varnode = NULL;
        int *dim_size = NULL;
        long *long_dim_size = NULL;

        dim_size = (int *)NclCalloc(n_dims, sizeof(int));
        assert(dim_size);

        long_dim_size = (long *)NclCalloc(n_dims, sizeof(long));
        assert(long_dim_size);

        for(n = 0; n < n_dims; n++)
        {
            dimnode = _getDimNodeFromNclFileGrpNode(grpnode, dim_name[n]);
            dim_size[n] = dimnode->size;
            long_dim_size[n] = (long) dimnode->size;

          /*
           *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tdim %d, name: <%s>, size: %d\n",
           *                 n, NrmQuarkToString(dim_name[n]), dim_size[n]);
           */
        }

        varnode = defNC4CompoundVar(grpnode, var_name, nc_compound_type_id,
                                    n_dims, dim_name, long_dim_size);

        if(NULL != varnode)
        {
            NclFileCompoundRecord *comp_rec = _NclFileCompoundAlloc(n_mems);
            NclFileCompoundNode   *compnode = NULL;

            comp_rec->name = compound_name;
            comp_rec->size = compound_length;
            comp_rec->type = NC_COMPOUND;
            comp_rec->xtype = nc_compound_type_id;
            comp_rec->base_nc_type = nc_compound_type_id;

            for(n = 0; n < n_mems; n++)
            {
                compnode = &(comp_rec->compnode[n]);

                compnode->name = udt_mem_name[n];
                compnode->type = udt_mem_type[n];
                compnode->the_nc_type = *(mem_nc_type[n]);
                compnode->offset = mem_offset[n];
                compnode->rank = 1;
                compnode->nvals = mem_size[n];
                compnode->sides = NULL;
                compnode->value = NULL;
            }

            varnode->comprec = comp_rec;
        }

        NclFree(dim_size);
        NclFree(long_dim_size);
    }

    NclFree(udt_mem_name);
    NclFree(udt_mem_type);
    NclFree(mem_offset);

    for(n = 0; n < n_mems; n++)
    {
        NclFree(mem_nc_type[n]);
    }

    NclFree(mem_nc_type);

  /*
   *fprintf(stderr, "Leave NC4AddCompound, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NhlErrorTypes NC4WriteCompound(void *rec, NclQuark compound_name, NclQuark var_name,
                               ng_size_t n_mems, NclQuark *mem_name, NclList thelist)
{
    NclFileGrpNode   *grpnode = (NclFileGrpNode *) rec;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileDimNode   *dimnode = NULL;
    NclFileVarNode   *varnode = NULL;
    int fid;
    int n = -1;
    int nc_ret = 0;

    nc_type var_id;

    int n_dims = 1;

    size_t data_size = 1;
    void  *data_value = NULL;

  /*
   *fprintf(stderr, "\nEnter NC4WriteCompound, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tcompound_name: <%s>, var_name: <%s>, n_mems = %d, mem_name[0]: <%s>\n",
   *                 NrmQuarkToString(compound_name), NrmQuarkToString(var_name),
   *                 n_mems, NrmQuarkToString(mem_name[0]));
   */

    if(grpnode->open)
    {
        fid = grpnode->gid;
    }
    else
    {
        nc_ret = nc__open(NrmQuarkToString(grpnode->path),NC_WRITE,
                         &ChunkSizeHint,&fid);
        if(nc_ret != NC_NOERR)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                          "%s: Could not reopen the file (%s) for writing, at line: %d\n",
                          __FILE__, NrmQuarkToString(grpnode->path), __LINE__));
            check_err(nc_ret, __LINE__, __FILE__);
            return(NhlFATAL);
        }
        grpnode->fid = fid;
        grpnode->gid = fid;
        grpnode->define_mode = 1;
        grpnode->open = 1;
    }

    nc_ret = nc_inq_varid(grpnode->gid, NrmQuarkToString(var_name), &var_id);

  /*
   *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tvar_name: <%s>, var_id = %d\n", NrmQuarkToString(var_name), var_id);
   *fprintf(stderr, "\tgrpnode->gid = %d\n", grpnode->gid);
   *fprintf(stderr, "\tNC_NOERR = %d, nc_ret = %d\n", NC_NOERR, nc_ret);
   */

    if(NC_NOERR != nc_ret)
        check_err(nc_ret, __LINE__, __FILE__);

    varnode = _getVarNodeFromNclFileGrpNode(grpnode, var_name);

    if((NULL != varnode) && (NULL != thelist))
    {
        NclListObjList *list_list = thelist->list.first;

        NclVar self = NULL;
        NclMultiDValData theval = NULL;

        NclFileCompoundRecord *comp_rec = varnode->comprec;
        NclFileCompoundNode   *compnode = NULL;

        n_dims = varnode->dim_rec->n_dims;

        data_size = 1;
        for(n = 0; n < n_dims; n++)
        {
            dimnode = &(varnode->dim_rec->dim_node[n]);
            data_size *= (size_t) dimnode->size;
        }

        if(NULL != comp_rec)
        {
            size_t cur_mem_loc = 0;
            size_t compound_size = 0;
            int current_component = 0;

            size_t *mem_len = (size_t *)NclCalloc(n_mems, sizeof(size_t));
            if (! mem_len)
            {
                NHLPERROR((NhlFATAL,ENOMEM,NULL));
                return NhlFATAL;
            }

            if(comp_rec->n_comps == n_mems)
            {
                for(n = 0; n < n_mems; n++)
                {
                    compnode = &(comp_rec->compnode[n]);

                    mem_len[n] = (size_t) _NclSizeOf(compnode->type)
                               * (size_t) compnode->nvals;

                    compound_size += mem_len[n];
                }
            }

            data_value = (void *)NclCalloc((ng_usize_t)(data_size*compound_size), sizeof(void));
            assert(data_value);

            current_component = 0;
            while(NULL != list_list)
            {
                self = (NclVar)_NclGetObj(list_list->obj_id);
                if(self != NULL)
                {
                    theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);

                    if(Ncl_MultiDValData == theval->obj.obj_type)
                    {
                        cur_mem_loc = mem_len[current_component];
                        for(n = 0; n < data_size; ++n)
                        {
                            memcpy(data_value + cur_mem_loc,
                                   theval->multidval.val + n * mem_len[current_component],
                                   mem_len[current_component]);

                            cur_mem_loc += compound_size;
                        }
                    }
                }

                list_list = list_list->next;
            }

            ret = nc_put_var(fid, varnode->id, data_value);
            if(NC_NOERR != ret)
                check_err(ret, __LINE__, __FILE__);

            NclFree(data_value);
        }
    }

  /*
   *fprintf(stderr, "Leave NC4WriteCompound, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return ret;
}

NclFormatFunctionRec NC4Rec =
{
    /* NclInitializeFileRecFunc initialize_file_rec */       NC4InitializeFileRec,
    /* NclCreateFileFunc       create_file; */               NC4CreateFile,
    /* NclOpenFileFunc         open_file; */                 NC4OpenFile,
    /* NclFreeFileRecFunc      free_file_rec; */             NC4FreeFileRec,
    /* NclGetVarNamesFunc      get_var_names; */             GetGrpVarNames,
    /* NclGetVarInfoFunc       get_var_info; */              GetVarInfo,
    /* NclGetDimNamesFunc      get_dim_names; */             GetGrpDimNames,
    /* NclGetDimInfoFunc       get_dim_info; */              GetDimInfo,
    /* NclGetAttNamesFunc      get_att_names; */             GetGrpAttNames,
    /* NclGetAttInfoFunc       get_att_info; */              GetAttInfo,
    /* NclGetVarAttNamesFunc   get_var_att_names; */         GetVarAttNamesFromGrp,
    /* NclGetVarAttInfoFunc    get_var_att_info; */          GetVarAttInfo,
    /* NclGetCoordInfoFunc     get_coord_info; */            NC4GetCoordInfo,
    /* NclReadCoordFunc        read_coord; */                NC4ReadCoord,
    /* NclReadCoordFunc        read_coord; */                NULL,
    /* NclReadVarFunc          read_var; */                  NC4ReadVar,
    /* NclReadVarFunc          read_var; */                  NULL,
    /* NclReadAttFunc          read_att; */                  NC4ReadAtt,
    /* NclReadVarAttFunc       read_var_att; */              NC4ReadVarAtt,
    /* NclWriteCoordFunc       write_coord; */               NC4WriteCoord,
    /* NclWriteCoordFunc       write_coord; */               NULL,
    /* NclWriteVarFunc         write_var; */                 NC4WriteVar,
    /* NclWriteVarFunc         write_var; */                 NULL,
    /* NclWriteAttFunc         write_att; */                 NC4WriteAtt,
    /* NclWriteVarAttFunc      write_var_att; */             NC4WriteVarAtt,
    /* NclAddDimFunc           add_dim; */                   NC4AddDim,
    /* NclAddChunkDimFunc      add_chunk_dim; */             NC4AddChunkDim,
    /* NclRenameDimFunc        rename_dim; */                NC4RenameDim,
    /* NclAddVarFunc           add_var; */                   NC4AddVar,
    /* NclAddVarChunkFunc      add_var_chunk; */             NC4AddVarChunk,
    /* NclAddVarChunkCacheFunc add_var_chunk_cache; */       NC4AddVarChunkCache,
    /* NclSetVarCompressLevelFunc set_var_compress_level; */ NC4SetVarCompressLevel,
    /* NclAddVarFunc           add_coord_var; */             NULL,
    /* NclAddAttFunc           add_att; */                   NC4AddAtt,
    /* NclAddVarAttFunc        add_var_att; */               NC4AddVarAtt,
    /* NclMapFormatTypeToNcl   map_format_type_to_ncl; */    NC4MapToNcl,
    /* NclMapNclTypeToFormat   map_ncl_type_to_format; */    NC4MapFromNcl,
    /* NclDelAttFunc           del_att; */                   NC4DelAtt,
    /* NclDelVarAttFunc        del_var_att; */               NC4DelVarAtt,
    /* NclGetGrpNamesFunc      get_grp_names; */             _NclGetGrpNames,
    /* NclGetGrpInfoFunc       get_grp_info; */              NULL,
    /* NclGetGrpAttNamesFunc   get_grp_att_names; */         NULL, 
    /* NclGetGrpAttInfoFunc    get_grp_att_info; */          NULL,
    /* NclAddGrpFunc           add_grp; */                   NC4AddGrp,
    /* NclAddVlenFunc          add_vlen; */                  NC4AddVlen,
    /* NclAddEnumFunc          add_enum; */                  NC4AddEnum,
    /* NclAddOpaqueFunc        add_opaque; */                NC4AddOpaque,
    /* NclAddCompoundFunc      add_compound; */              NC4AddCompound,
    /* NclWriteCompoundFunc    write_compound; */            NC4WriteCompound,
    /* NclSetOptionFunc        set_option;  */               NC4SetOption
};

NclFormatFunctionRecPtr NC4AddFileFormat(void)
{
    return(&NC4Rec);
}


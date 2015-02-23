/*
 *      $Id: NclNetCdf.c,v 1.60 2010-05-06 22:52:28 huangwei Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 13 10:15:21 MDT 1994
 *
 *	Description:	
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
#include "NclVar.h"
#include "VarSupport.h"
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

#include <netcdf.h>
#include <math.h>
#include <unistd.h>

#define NETCDF_DEBUG 0

static ng_usize_t ChunkSizeHint;

typedef struct _NetCdfFileRecord NetCdfFileRecord;
typedef struct _NetCdfVarInqRec NetCdfVarInqRec;
typedef struct _NetCdfDimInqRec NetCdfDimInqRec;
typedef struct _NetCdfAttInqRec NetCdfAttInqRec;
typedef struct _NetCdfVarInqRecList NetCdfVarInqRecList;
typedef struct _NetCdfDimInqRecList NetCdfDimInqRecList;
typedef struct _NetCdfAttInqRecList NetCdfAttInqRecList;
typedef struct _NetCdfOptions NetCdfOptions;

struct _NetCdfVarInqRecList {
	NetCdfVarInqRec *var_inq;
	NetCdfVarInqRecList *next;
};

struct _NetCdfDimInqRecList {
	NetCdfDimInqRec *dim_inq;
	NetCdfDimInqRecList *next;
};

struct _NetCdfAttInqRecList {
	NetCdfAttInqRec *att_inq;
	NetCdfAttInqRecList *next;
};

struct _NetCdfVarInqRec {
	int varid;
	NclQuark name;
	nc_type	data_type;
	int	n_dims;
	int	dim[MAX_VAR_DIMS];
	int	compress_level;
	int	n_chunk_dims;
	ng_size_t	chunk_dim[MAX_VAR_DIMS];
	int	use_cache;
	ng_size_t	cache_size;
	ng_size_t	cache_nelems;
	float	cache_preemption;
	int	natts;
	NetCdfAttInqRecList *att_list;
	void *value;
};

struct _NetCdfDimInqRec {
	int dimid;
	int is_unlimited;
	NclQuark name;
	ng_size_t size;
};
	
struct _NetCdfAttInqRec {
	int att_num;
	NclQuark name;
	int	varid;
	nc_type data_type;
	int	len;
	void	*value;
	int virtual;
};

			
struct _NetCdfOptions {
	NclQuark name;
	NclBasicDataTypes data_type;
	int n_values;
	void *values;
};

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

#define NC_PREFILL_OPT 0
#define NC_DEFINE_MODE_OPT 1
#define NC_HEADER_SPACE_OPT 2
#define NC_SUPPRESS_CLOSE_OPT 3
#define NC_FORMAT_OPT 4
#define NC_MISSING_TO_FILL_VALUE_OPT 5

#ifdef USE_NETCDF4_FEATURES
#define NC_COMPRESSION_LEVEL_OPT 6
#define NC_USE_CACHE_OPT	 7
#define NC_CACHE_SIZE_OPT	 8
#define NC_CACHE_NELEMS_OPT	 9
#define NC_CACHE_PREEMPTION_OPT	 10
#define NC_NUM_OPTIONS 11
#else
#define NC_NUM_OPTIONS 6
#endif

struct _NetCdfFileRecord {
NclQuark	file_path_q;
int		wr_status;
int		n_vars;
NetCdfVarInqRecList *vars;
int		n_dims;
NetCdfDimInqRecList *dims;
int		compress_level;
int		n_chunk_dims;
NetCdfDimInqRecList *chunk_dims;
int		has_scalar_dim;
int		n_file_atts;
NetCdfAttInqRecList *file_atts;
int             n_options;
NetCdfOptions   *options;
int             cdfid;
/*
 * since any value of cdfid is now allowed (including -1) the
 * 'open' flag member is added to indicate whether the file open state
 */
int             open;    
int             header_reserve_space;
int             define_mode;
int             format;
int		use_cache;
ng_size_t		cache_size;
ng_size_t		cache_nelems;
float		cache_preemption;
};

static NhlErrorTypes NetAddVarChunk(void* therec, NclQuark thevar, int n_chunk_dims, ng_size_t *chunk_dims);

static NclBasicDataTypes NetMapToNcl 
#if	NhlNeedProto
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
	static int first = 1;
	static NclBasicDataTypes long_type;
	if(first) {
		if(sizeof(nclong) == _NclSizeOf(NCL_int)) {
			long_type = NCL_int;
		} else if(sizeof(nclong) == _NclSizeOf(NCL_long)) {
			long_type = NCL_long;
		} else {
			long_type = NCL_none;
		}
		first = 0;
	}
	switch(*(nc_type*)the_type) {
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

static void *NetMapFromNcl
#if	NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
	static int first = 1;
	static NclBasicDataTypes long_type;
	void *out_type = (void*)NclMalloc((unsigned)sizeof(nc_type));;
	if(first) {
		if(sizeof(nclong) == _NclSizeOf(NCL_long)) {
			long_type = NCL_long;
		} else if(sizeof(nclong) == _NclSizeOf(NCL_int)) {
			long_type = NCL_int;
		} else {
			long_type = NCL_none;
		}
		first = 0;
	}

	switch(the_type) {
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
		if(long_type == the_type) {
			*(nc_type*)out_type = NC_LONG;
		} else {
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
	return(out_type);
}

static void NetGetAttrVal
#if	NhlNeedProto
(int ncid,NetCdfAttInqRec* att_inq)
#else
(ncid,att_inq)
int ncid,
NetCdfAttInqRec* att_inq
#endif
{
	char *tmp;
	int ret;
#if NETCDF_DEBUG
        void *value;
#endif                

	if (att_inq->virtual) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Error retrieving value for virtual attribute %s",
			  NrmQuarkToString(att_inq->name));
		return;
	}
	if (att_inq->data_type < 1) {
		att_inq->value = NULL;
	}
	else if(att_inq->data_type == NC_CHAR && !(att_inq->name == Qfill_val || att_inq->name == Qmissing_val)) {
		tmp = (char*)NclMalloc(att_inq->len+1);
		tmp[att_inq->len] = '\0';
		ret = ncattget(ncid,att_inq->varid,NrmQuarkToString(att_inq->name),tmp);
#if NETCDF_DEBUG
		value = NclMalloc(att_inq->len+1);
		fprintf(stderr,"ncattget(%d,%d,\"%s\",value);\n",ncid,att_inq->varid,NrmQuarkToString(att_inq->name));
#endif                
		att_inq->value = NclMalloc(sizeof(NclQuark));
		*(NclQuark *)att_inq->value = NrmStringToQuark(tmp);
		NclFree(tmp);
	} 
	else {
		att_inq->value = NclMalloc(nctypelen(att_inq->data_type)*att_inq->len);
		ret = ncattget(ncid,att_inq->varid,NrmQuarkToString(att_inq->name),att_inq->value);
#if NETCDF_DEBUG
		value = NclMalloc(nctypelen(att_inq->data_type)*att_inq->len);
		fprintf(stderr,"ncattget(%d,%d,\"%s\",value);\n",ncid,att_inq->varid,NrmQuarkToString(att_inq->name));
#endif                
	}
	return;
}

static void NetGetDimVals
#if	NhlNeedProto
(int ncid,NetCdfFileRecord* frec)
#else
(ncid,frec)
int ncid,
NetCdfAttInqRec* frec
#endif
{
	NetCdfDimInqRecList *dl = frec->dims;
	long start = 0;
	int ret;

	for(; dl != NULL; dl = dl->next) {
		NetCdfDimInqRec *dim_inq = dl->dim_inq;
		NetCdfVarInqRecList *vl = frec->vars;

		for (; vl != NULL; vl = vl->next) {
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
#if NETCDF_DEBUG
		fprintf(stderr,"ncvarget(%d,%d,start,size,value);\n",ncid,vl->var_inq->varid);
#endif
	}
	return;
}

static void CloseOrNot
#if	NhlNeedProto
(NetCdfFileRecord *rec, int cdfid, int sync)
#else
(tmp,cdfid,sync)
NetCdfFileRecord *rec;
int cdfid;
int sync;
#endif
{

	if ((int)(rec->options[NC_SUPPRESS_CLOSE_OPT].values) == 0) {
		ncclose(cdfid);
#if NETCDF_DEBUG
		fprintf(stderr,"CloseOrNot:ncclose(%d);\n",cdfid);
#endif                
		rec->open = 0;
		rec->cdfid = -1;
	}
	else {
		if (sync) {
			nc_sync(cdfid);
#if NETCDF_DEBUG
			fprintf(stderr,"nc_sync(%d);\n",cdfid);
#endif                
		}
		rec->open = 1;
		rec->cdfid = cdfid;
	}
}

static void _checking_nc4_chunking
#if     NhlNeedProto
(NetCdfFileRecord *rec, int cdfid)
#else
(rec,cdfid)
NetCdfFileRecord *rec;
int cdfid;
#endif
{
#ifdef USE_NETCDF4_FEATURES
	NetCdfVarInqRecList *stepvl;
	NetCdfVarInqRec *var_inq;

	NetCdfDimInqRecList *file_dim_list;
	NetCdfDimInqRecList *chunk_dim_list;

	NetCdfDimInqRec *dim_inq;
	NetCdfDimInqRec *chunk_dim_inq;

	ng_size_t *dims;
	ng_size_t *chunk_dims;

	int shuffle = 0;
	int deflate = 1;
	int deflate_level = 1;
	int nc_ret;
	int nd = 0;
	int nv = 0;
	int storage = NC_CHUNKED;

	if(rec->n_chunk_dims)
	{
		dims = (ng_size_t *) NclMalloc(rec->n_dims*sizeof(ng_size_t));
		chunk_dims = (ng_size_t *) NclMalloc(rec->n_dims*sizeof(ng_size_t));

		nd = 0;
		file_dim_list = rec->dims;
		chunk_dim_list = rec->chunk_dims;
		while(file_dim_list != NULL)
		{
			dim_inq = file_dim_list->dim_inq;
			chunk_dim_inq = chunk_dim_list->dim_inq;

			if(dim_inq->name != chunk_dim_inq->name)
			{
				fprintf(stderr, "dim name: <%s> and chunk_dim name: <%s> are different.\n",
					NrmQuarkToString(dim_inq->name), NrmQuarkToString(chunk_dim_inq->name));
				fprintf(stderr, "No more file-wise chunking and compress check.\n");
				break;
			}

			dims[dim_inq->dimid] = dim_inq->size;

			chunk_dim_inq->dimid = dim_inq->dimid;
			chunk_dims[dim_inq->dimid] = chunk_dim_inq->size;
			nd++;
			file_dim_list = file_dim_list->next;
			chunk_dim_list = chunk_dim_list->next;
		}

		nv = 0;
        	stepvl = rec->vars;
        	while(stepvl != NULL)
        	{
			var_inq = stepvl->var_inq;
            		if(var_inq->n_chunk_dims < 1)
            		{
				for(nd = 0; nd < var_inq->n_dims; nd++)
				{
					var_inq->chunk_dim[nd] = chunk_dims[var_inq->dim[nd]];
				}
				var_inq->n_chunk_dims = var_inq->n_dims;
                		nc_ret = nc_def_var_chunking(cdfid, var_inq->varid, storage,
							     (size_t *)var_inq->chunk_dim);

            			if((rec->compress_level > 0) && (var_inq->compress_level < 1))
            			{
					var_inq->compress_level = rec->compress_level;
					deflate = var_inq->compress_level;
					deflate_level = deflate;
					nc_ret = nc_def_var_deflate(cdfid, var_inq->varid, shuffle,
								deflate, deflate_level);
            			}

				var_inq->use_cache = (int)(rec->options[NC_USE_CACHE_OPT].values);
            			if(var_inq->use_cache)
            			{
					int *isv = (int *)(rec->options[NC_CACHE_SIZE_OPT].values);
					int *iev = (int *)(rec->options[NC_CACHE_NELEMS_OPT].values);
					float *fv = (float *)(rec->options[NC_CACHE_PREEMPTION_OPT].values);
					var_inq->cache_size = isv[0];
					var_inq->cache_nelems = iev[0];
					var_inq->cache_preemption = fv[0];
					nc_ret = nc_set_var_chunk_cache(cdfid, var_inq->varid,
                                                		var_inq->cache_size, var_inq->cache_nelems,
                                                		var_inq->cache_preemption);
            			}
			}
			nv ++;
            		stepvl= stepvl->next;
        	}

		free(dims);
		free(chunk_dims);
	}
#endif                
}

static void EndDefineModeIf
#if	NhlNeedProto
(NetCdfFileRecord *rec, int cdfid)
#else
(rec,cdfid)
NetCdfFileRecord *rec;
int cdfid;
#endif
{
	/* 
	 * The header space will not be reserved unless at least one variable has been defined;
	 * hence the double condition.
	 */
	if (rec->define_mode) {
#ifdef USE_NETCDF4_FEATURES
		_checking_nc4_chunking(rec,cdfid);
#endif                
		if (rec->n_vars > 0 && rec->header_reserve_space > 0) {
			nc__enddef(cdfid,rec->header_reserve_space,4,0,4);
#if NETCDF_DEBUG
			fprintf(stderr,"nc__enddef(%d,%d,4,0,4);\n",cdfid,rec->header_reserve_space);
#endif                
			rec->header_reserve_space = 0;
		}
		else {
			ncendef(cdfid);
#if NETCDF_DEBUG
			fprintf(stderr,"ncendef(%d);\n",cdfid);
#endif                
		}
		rec->define_mode = 0;
	}
	return;
}

static int InitializeOptions 
#if	NhlNeedProto
(NetCdfFileRecord *tmp)
#else
(tmp)
NetCdfFileRecord *tmp;
#endif
{
	NetCdfOptions *options;

	tmp->n_options = NC_NUM_OPTIONS;
	
	options = NclMalloc(tmp->n_options * sizeof(NetCdfOptions));
	if (! options) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return 0;
	}
	options[NC_PREFILL_OPT].name = NrmStringToQuark("prefill");
	options[NC_PREFILL_OPT].data_type = NCL_logical;
	options[NC_PREFILL_OPT].n_values = 1;
	options[NC_PREFILL_OPT].values = (void *) 1;
	options[NC_DEFINE_MODE_OPT].name = NrmStringToQuark("definemode");
	options[NC_DEFINE_MODE_OPT].data_type = NCL_logical;
	options[NC_DEFINE_MODE_OPT].n_values = 1;
	options[NC_DEFINE_MODE_OPT].values = (void *) 0;
	options[NC_HEADER_SPACE_OPT].name = NrmStringToQuark("headerreservespace");
	options[NC_HEADER_SPACE_OPT].data_type = NCL_int;
	options[NC_HEADER_SPACE_OPT].n_values = 1;
	options[NC_HEADER_SPACE_OPT].values = (void *) 0;
	options[NC_SUPPRESS_CLOSE_OPT].name = NrmStringToQuark("suppressclose");
	options[NC_SUPPRESS_CLOSE_OPT].data_type = NCL_int;
	options[NC_SUPPRESS_CLOSE_OPT].n_values = 1;
	options[NC_SUPPRESS_CLOSE_OPT].values = (void *) 0;
	options[NC_FORMAT_OPT].name = NrmStringToQuark("format");
	options[NC_FORMAT_OPT].data_type = NCL_string;
	options[NC_FORMAT_OPT].n_values = 1;
	options[NC_FORMAT_OPT].values = (void *) NrmStringToQuark("classic");
	options[NC_MISSING_TO_FILL_VALUE_OPT].name = NrmStringToQuark("missingtofillvalue");
	options[NC_MISSING_TO_FILL_VALUE_OPT].data_type = NCL_int;
	options[NC_MISSING_TO_FILL_VALUE_OPT].n_values = 1;
	options[NC_MISSING_TO_FILL_VALUE_OPT].values = (void *) 1;

#ifdef USE_NETCDF4_FEATURES
	options[NC_COMPRESSION_LEVEL_OPT].name = NrmStringToQuark("compressionlevel");
	options[NC_COMPRESSION_LEVEL_OPT].data_type = NCL_int;
	options[NC_COMPRESSION_LEVEL_OPT].n_values = 1;
	options[NC_COMPRESSION_LEVEL_OPT].values = (void *) -1;

	options[NC_USE_CACHE_OPT].name = NrmStringToQuark("usecache");
	options[NC_USE_CACHE_OPT].data_type = NCL_int;
	options[NC_USE_CACHE_OPT].n_values = 1;
	options[NC_USE_CACHE_OPT].values = (void *) 0;

	options[NC_CACHE_SIZE_OPT].name = NrmStringToQuark("cachesize");
	options[NC_CACHE_SIZE_OPT].data_type = NCL_int;
	options[NC_CACHE_SIZE_OPT].n_values = 1;
	options[NC_CACHE_SIZE_OPT].values = (void *) 3200000;

	options[NC_CACHE_NELEMS_OPT].name = NrmStringToQuark("cachenelems");
	options[NC_CACHE_NELEMS_OPT].data_type = NCL_int;
	options[NC_CACHE_NELEMS_OPT].n_values = 1;
	options[NC_CACHE_NELEMS_OPT].values = (void *) 1009;

	options[NC_CACHE_PREEMPTION_OPT].name = NrmStringToQuark("cachepreemption");
	options[NC_CACHE_PREEMPTION_OPT].data_type = NCL_float;
	options[NC_CACHE_PREEMPTION_OPT].n_values = 1;
	options[NC_CACHE_PREEMPTION_OPT].values = (void *) 0;
#endif

	tmp->options = options;
	return 1;
}

static void *NetInitializeFileRec
#if	NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormatType *format;
#endif
{
	static int first = True;
	NetCdfFileRecord *therec = NULL;
	ng_size_t blksize = getpagesize();

	if (first) {
		Qmissing_val = NrmStringToQuark("missing_value");
		Qfill_val = NrmStringToQuark("_FillValue");
		first = False;
	}

        /*nc_set_log_level(3);*/
        nc_set_log_level(3);

	ChunkSizeHint = 64 * blksize;
	therec = (NetCdfFileRecord*)NclCalloc(1, sizeof(NetCdfFileRecord));
	if (! therec) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}
	therec->cdfid = -1;
	therec->open = 0;
	therec->header_reserve_space = 0;
	therec->define_mode = 0;
	if (! InitializeOptions(therec)) {
		NclFree(therec);
		return NULL;
	}
	*format = _NclNETCDF;
	setvbuf(stderr,NULL,_IONBF,0);
	return (void *) therec;
}

static void *NetOpenFile
#if	NhlNeedProto
(void *rec,NclQuark path,int wr_status)
#else
(rec,path,wr_status)
void *rec;
NclQuark path;
int wr_status;
#endif
{
	NetCdfFileRecord *tmp = (NetCdfFileRecord*) rec;
	int cdfid;
	int nc_ret = NC_NOERR;
	int dummy;
	char buffer[4*NC_MAX_NAME+1];
	char buffer2[4*NC_MAX_NAME+1];
	int i,j,has_scalar_dim = 0,nvars = 0;
	long tmp_size;
	NetCdfAttInqRecList **stepalptr;
	NetCdfVarInqRecList **stepvlptr;
	NetCdfVarInqRecList *tmpvlptr;
	NetCdfDimInqRecList **stepdlptr;
	NetCdfDimInqRecList *tmpdlptr;

	if(tmp == NULL) {
		return(NULL);
	}
	tmp->file_path_q = path;
	tmp->wr_status = wr_status;
	tmp->n_vars = 0;
	tmp->vars= NULL;
	tmp->n_dims = 0;
	tmp->dims = NULL;
	tmp->n_file_atts = 0;
	tmp->file_atts= NULL;
	tmp->compress_level = 0;
	tmp->n_chunk_dims = 0;
	tmp->chunk_dims= NULL;

	if (tmp->open) {
		cdfid = tmp->cdfid;
	}
	else if(wr_status > 0) {
		nc_ret = nc__open(NrmQuarkToString(path),NC_NOWRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
		fprintf(stderr,"nc__open(\"%s\",NC_NOWRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(path));
		fprintf(stderr,"%d: cdfid = %d\n",cdfid);
#endif                
		tmp->define_mode = 0;
		tmp->cdfid = cdfid;
	} else {
		nc_ret = nc__open(NrmQuarkToString(path),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
		fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(path));
#endif                
		tmp->define_mode = 0;
		tmp->cdfid = cdfid;
	}

	if(nc_ret != NC_NOERR) { 
		char *emsg = (char *) nc_strerror(nc_ret);
		if (emsg == NULL) {
			emsg = "NetCDF: The specified file (%s) cannot be opened; invalid file or system error";
			if (! strncmp(NrmQuarkToString(path),"http://",7)) {
				emsg = "The specified URL (%s) does not reference an active DODS server or cannot be processed by the DODS server";
			}
		}

		/*
		NhlPError(NhlWARNING,NhlEUNKNOWN,emsg,NrmQuarkToString(path));
		*/
		NclFree(tmp);
		return(NULL);
	}

	tmp->open = 1;
#ifdef USE_NETCDF4_FEATURES
	nc_inq_format(cdfid,&(tmp->format));
#if NETCDF_DEBUG
	fprintf(stderr,"nc_inq_format(%d,&format);\n",cdfid);
#endif                
#endif
	ncinquire(cdfid,&(tmp->n_dims),&(tmp->n_vars),&(tmp->n_file_atts),&dummy);
#if NETCDF_DEBUG
	fprintf(stderr,"ncinquire(%d,&n_dims,&n_vars,&n_file_atts,&dummy);\n",cdfid);
#endif                
	stepdlptr = &(tmp->dims);
	if(tmp->n_dims != 0) {
		for(i = 0 ; i < tmp->n_dims; i++) {
			*stepdlptr = (NetCdfDimInqRecList*)NclMalloc(
					(unsigned) sizeof(NetCdfDimInqRecList));
			(*stepdlptr)->dim_inq = (NetCdfDimInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfDimInqRec));
			(*stepdlptr)->dim_inq->is_unlimited = (i==dummy)?1:0;
			(*stepdlptr)->next = NULL;
			(*stepdlptr)->dim_inq->dimid = i;
			nc_inq_dim(cdfid,i,buffer,&((*stepdlptr)->dim_inq->size));
#if NETCDF_DEBUG
			fprintf(stderr,"nc_inq_dim(%d,%d,buffer,&dim_size);\n",cdfid,i);
#endif                

/*
			if((*stepdlptr)->dim_inq->size == 0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"NetCdf: %s is a zero length dimension some variables may be ignored",buffer);
			}
*/
			(*stepdlptr)->dim_inq->name = NrmStringToQuark(buffer);
			stepdlptr = &((*stepdlptr)->next);
		}
	} else {
		tmp->dims = NULL;
	}
	stepvlptr = &(tmp->vars);
	nvars = tmp->n_vars;
	if(tmp->n_vars != 0 ) {
		for(i = 0 ; i < nvars; i++) {
			*stepvlptr = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned) sizeof(NetCdfVarInqRecList));
			(*stepvlptr)->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec));
			(*stepvlptr)->next = NULL;
			(*stepvlptr)->var_inq->varid = i;
			(*stepvlptr)->var_inq->value = NULL;
			ncvarinq(cdfid,i,buffer,
				&((*stepvlptr)->var_inq->data_type),
				&((*stepvlptr)->var_inq->n_dims),
				((*stepvlptr)->var_inq->dim),
				&((*stepvlptr)->var_inq->natts)
				);
#if NETCDF_DEBUG
			fprintf(stderr,"ncvarinq(%d,%d,buffer,&data_type,&n_dims,dims,&n_atts);\n",cdfid,i);
#endif                
			for(j = 0; j < ((*stepvlptr)->var_inq->n_dims); j++) {
				tmp_size = 0;
				nc_inq_dim(cdfid,((*stepvlptr)->var_inq->dim)[j],buffer2,&tmp_size);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_inq_dim(%d,%d,buffer,&size);\n",cdfid,((*stepvlptr)->var_inq->dim)[j]);
#endif                
			}
			if(j != ((*stepvlptr)->var_inq->n_dims)) {
				tmpvlptr = *stepvlptr;
				*stepvlptr = NULL;
				tmp->n_vars--;
				NclFree(tmpvlptr->var_inq);
				NclFree(tmpvlptr);
				
			} else {
				if(((*stepvlptr)->var_inq->n_dims) == 0) {
					((*stepvlptr)->var_inq->dim)[0] = -5;
					((*stepvlptr)->var_inq->n_dims) = 1;
					has_scalar_dim = 1;
				}
				(*stepvlptr)->var_inq->name = NrmStringToQuark(buffer);
				stepalptr = &((*stepvlptr)->var_inq->att_list);
				if(((*stepvlptr)->var_inq->natts) != 0) {
					int has_fill = 0;
					NetCdfAttInqRec* missing_val_recp = NULL;
					
					for(j = 0; j < ((*stepvlptr)->var_inq->natts); j++) {
						ncattname(cdfid,i,j,buffer);
#if NETCDF_DEBUG
						fprintf(stderr,"ncattname(%d,%d,%d,buffer);\n",cdfid,i,j);
#endif                
						(*stepalptr) = (NetCdfAttInqRecList*)NclMalloc(
							(unsigned)sizeof(NetCdfAttInqRecList));
						(*stepalptr)->att_inq = (NetCdfAttInqRec*)NclMalloc(
							(unsigned)sizeof(NetCdfAttInqRec));
						(*stepalptr)->next = NULL;
						(*stepalptr)->att_inq->att_num = j;
						(*stepalptr)->att_inq->virtual = 0;
						(*stepalptr)->att_inq->name = NrmStringToQuark(buffer);
						if ((*stepalptr)->att_inq->name == Qmissing_val) {
							missing_val_recp = (*stepalptr)->att_inq;
						}
						(*stepalptr)->att_inq->varid = i;
						ncattinq(cdfid,i,buffer,
							&((*stepalptr)->att_inq->data_type),
							&((*stepalptr)->att_inq->len));
#if NETCDF_DEBUG
						fprintf(stderr,"ncattinq(%d,%d,buffer,&data_type,&size);\n",cdfid,i);
#endif                
						NetGetAttrVal(cdfid,(*stepalptr)->att_inq);
						if ((*stepalptr)->att_inq->name == Qfill_val)
						{
							void *fv = NclMalloc(nctypelen((*stepvlptr)->var_inq->data_type));
#if NETCDF_DEBUG
							NHLPERROR((NhlWARNING,NhlEUNKNOWN,
								"NetOpenFile: _FillValue attribute (%s) type differs from variable (%s), forcing type conversion; may result in overflow and/or loss of precision",
								buffer, NrmQuarkToString((*stepvlptr)->var_inq->name)));
#endif                
							_NclScalarForcedCoerce((*stepalptr)->att_inq->value,
									       NetMapToNcl(&((*stepalptr)->att_inq->data_type)),
									       fv, NetMapToNcl(&((*stepvlptr)->var_inq->data_type)));

							NclFree((*stepalptr)->att_inq->value);
							(*stepalptr)->att_inq->len = 1;
							(*stepalptr)->att_inq->value = fv;
							(*stepalptr)->att_inq->data_type = (*stepvlptr)->var_inq->data_type;
							has_fill = 1;
						}
						stepalptr = &((*stepalptr)->next);
					}
					
					if (((int)(tmp->options[NC_MISSING_TO_FILL_VALUE_OPT].values) == 1) &&
					    missing_val_recp  && ! has_fill) {
						if (missing_val_recp->data_type == (*stepvlptr)->var_inq->data_type) {
							(*stepalptr) = (NetCdfAttInqRecList*)NclMalloc(
								(unsigned)sizeof(NetCdfAttInqRecList));
							(*stepalptr)->att_inq = (NetCdfAttInqRec*)NclMalloc(
								(unsigned)sizeof(NetCdfAttInqRec));
							(*stepalptr)->next = NULL;
							(*stepalptr)->att_inq->att_num = (*stepvlptr)->var_inq->natts;
							(*stepvlptr)->var_inq->natts++;
							(*stepalptr)->att_inq->name = Qfill_val;
							(*stepalptr)->att_inq->varid = i;
							(*stepalptr)->att_inq->data_type =  missing_val_recp->data_type;
							(*stepalptr)->att_inq->len = missing_val_recp->len;
							(*stepalptr)->att_inq->value = 
								NclMalloc(nctypelen(missing_val_recp->data_type)* missing_val_recp->len);
							memcpy((*stepalptr)->att_inq->value, missing_val_recp->value,
							       nctypelen(missing_val_recp->data_type)* missing_val_recp->len);
							(*stepalptr)->att_inq->virtual = 1;
						}
						else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,
								  "NetOpenFile: MissingToFillValue option set True, but missing_value attribute and data variable (%s) types differ: not adding virtual _FillValue attribute",NrmQuarkToString((*stepvlptr)->var_inq->name));
						}
					}
				} else {
					((*stepvlptr)->var_inq->att_list) = NULL;
				}
				stepvlptr = &((*stepvlptr)->next);
			}
		}
		if(has_scalar_dim) {
			tmp->has_scalar_dim = 1;
			tmpdlptr = tmp->dims;
			tmp->dims = (NetCdfDimInqRecList*)NclMalloc(
					(unsigned) sizeof(NetCdfDimInqRecList));
			tmp->dims->dim_inq = (NetCdfDimInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfDimInqRec));
			tmp->dims->next = tmpdlptr;
			tmp->dims->dim_inq->dimid = -5;
			tmp->dims->dim_inq->size = 1;
			tmp->dims->dim_inq->is_unlimited = 0;
			tmp->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
			tmp->n_dims++;
		} else {
			tmp->has_scalar_dim = 0;
		}
	} else {
		tmp->vars = NULL;
		tmp->has_scalar_dim = 0;
	}
	if(tmp->n_file_atts != 0 ) {
		stepalptr = &(tmp->file_atts);
		for(i = 0; i < tmp->n_file_atts; i++) {
			*stepalptr = (NetCdfAttInqRecList*)NclMalloc(
				(unsigned)sizeof(NetCdfAttInqRecList));
			(*stepalptr)->att_inq = (NetCdfAttInqRec*)NclMalloc(
				(unsigned)sizeof(NetCdfAttInqRec));
			(*stepalptr)->next = NULL;
			ncattname(cdfid,NC_GLOBAL,i,buffer);
#if NETCDF_DEBUG
			fprintf(stderr,"ncattname(%d,NC_GLOBAL,%d,buffer);\n",cdfid,i);
#endif                
			(*stepalptr)->att_inq->att_num = i;
			(*stepalptr)->att_inq->virtual = 0;
			(*stepalptr)->att_inq->name = NrmStringToQuark(buffer);
			(*stepalptr)->att_inq->varid = NC_GLOBAL;
			ncattinq(cdfid,NC_GLOBAL,buffer,
					&((*stepalptr)->att_inq->data_type),
                                	&((*stepalptr)->att_inq->len));
#if NETCDF_DEBUG
			fprintf(stderr,"ncattinq(%d,NC_GLOBAL,buffer,&data_type,&size);\n",cdfid);
#endif                
			NetGetAttrVal(cdfid,(*stepalptr)->att_inq);
       	        	stepalptr = &((*stepalptr)->next);
		}
	} else {
		tmp->file_atts = NULL;
	}

	NetGetDimVals(cdfid,tmp);

	CloseOrNot(tmp,cdfid,0);
	return((void*)tmp);
}

static void *NetCreateFile
#if	NhlNeedProto
(void *rec,NclQuark path)
#else
(rec,path)
void *rec;
NclQuark path;
#endif
{
	NetCdfFileRecord *tmp = (NetCdfFileRecord*)rec;
	int id = 0;
	int nc_ret, mode;
	int format;

	if (((NrmQuark)(tmp->options[NC_FORMAT_OPT].values) == 
	     NrmStringToQuark("largefile")) ||
	    ((NrmQuark)(tmp->options[NC_FORMAT_OPT].values) == 
	     NrmStringToQuark("64bitoffset"))) {
		mode = (NC_NOCLOBBER|NC_64BIT_OFFSET);
		format = 2;
	}
#ifdef USE_NETCDF4_FEATURES
	else if ((NrmQuark)(tmp->options[NC_FORMAT_OPT].values) == 
	    NrmStringToQuark("netcdf4classic")) {
		mode = (NC_NOCLOBBER|NC_NETCDF4|NC_CLASSIC_MODEL);
		format = 4;
	}
#endif
	else {
		mode = (NC_NOCLOBBER);
		format = 1;
	}

	nc_ret = nc__create(NrmQuarkToString(path),mode,1024,&ChunkSizeHint,&id);
#if NETCDF_DEBUG
	fprintf(stderr,"nc__create(\"%s\",0x%x,1024,%d,&id);\n",NrmQuarkToString(path),mode,ChunkSizeHint);
#endif                
	if(nc_ret == NC_NOERR) {
		tmp->cdfid = id;
		tmp->define_mode = 1;
		tmp->format = format;
		tmp->open = 1;
		if ((int)(tmp->options[NC_DEFINE_MODE_OPT].values) == 0) {
			EndDefineModeIf(tmp,id);
			CloseOrNot(tmp,id,0);
		}
		return(NetOpenFile(rec,path,-1));
	} else {
		return(NULL);
	}
}

static void NetFreeFileRec
#if	NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
        NetCdfVarInqRecList *stepvl;
        NetCdfDimInqRecList *stepdl;

	if (rec->open) {
		ncclose(rec->cdfid);
#if NETCDF_DEBUG
		fprintf(stderr,"NetFreeFileRec:ncclose(%d);\n",rec->cdfid);
#endif                
	}
	stepal = rec->file_atts;
	while(rec->file_atts != NULL) {
		stepal = rec->file_atts;
		NclFree(stepal->att_inq->value);
		NclFree(stepal->att_inq);
		rec->file_atts = rec->file_atts->next;
		NclFree(stepal);
	}
	stepdl = rec->dims;
	while(rec->dims != NULL) {
		stepdl = rec->dims;
		NclFree(stepdl->dim_inq);
		rec->dims= rec->dims->next;
		NclFree(stepdl);
	}
	stepdl = rec->chunk_dims;
	while(rec->chunk_dims != NULL) {
		stepdl = rec->chunk_dims;
		NclFree(stepdl->dim_inq);
		rec->chunk_dims= rec->chunk_dims->next;
		NclFree(stepdl);
	}

	while(rec->vars != NULL) {
		stepvl = rec->vars;
		while(stepvl->var_inq->att_list != NULL) {
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

static NclQuark* NetGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NclQuark *out_quarks;
	NetCdfVarInqRecList *stepvl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_vars);
	stepvl = rec->vars;
	for(i = 0; i < rec->n_vars; i++) {
		out_quarks[i] = stepvl->var_inq->name;
		stepvl=stepvl->next;
	}
	*num_vars = rec->n_vars;;
	return(out_quarks);
}

static NclFVarRec *NetGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfDimInqRecList *stepdl;
	NclFVarRec *tmp;
	int j;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == var_name) {
			tmp = (NclFVarRec*)NclMalloc((unsigned)sizeof(NclFVarRec));
			tmp->var_name_quark = stepvl->var_inq->name;
			tmp->var_real_name_quark = stepvl->var_inq->name;
			tmp->var_full_name_quark = stepvl->var_inq->name;
			tmp->data_type = NetMapToNcl((void*)&(stepvl->var_inq->data_type));
			tmp->num_dimensions = stepvl->var_inq->n_dims;
			for(j=0; j< stepvl->var_inq->n_dims; j++) {
				stepdl = rec->dims;
				while(stepdl->dim_inq->dimid != stepvl->var_inq->dim[j]) {
					stepdl = stepdl->next;
				}
				if(stepdl->dim_inq->dimid == -5) {
					tmp->file_dim_num[j] = 0;
				} else if(rec->has_scalar_dim) {
					tmp->file_dim_num[j] = stepdl->dim_inq->dimid + 1;
				} else {
					tmp->file_dim_num[j] = stepdl->dim_inq->dimid;
				}
			}
			return(tmp);
		} else {
			stepvl = stepvl->next;
		}
	}
	return(NULL);
}

static NclQuark *NetGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NclQuark *out_quarks;
	NetCdfDimInqRecList *stepdl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_dims);
	stepdl = rec->dims;
	for(i = 0; i < rec->n_dims; i++) {
		out_quarks[i] = stepdl->dim_inq->name;
		stepdl=stepdl->next;
	}
	*num_dims = rec->n_dims;;
	return(out_quarks);
}

static NclFDimRec *NetGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NclFDimRec *tmp;
	NetCdfDimInqRecList *stepdl;

	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->name == dim_name_q) {
			tmp = (NclFDimRec*)NclMalloc((unsigned)sizeof(NclFDimRec));
			tmp->dim_name_quark = dim_name_q;
			tmp->dim_size = stepdl->dim_inq->size;
			tmp->is_unlimited  = stepdl->dim_inq->is_unlimited;
			return(tmp);
		} else {
			stepdl = stepdl->next;
		}
	}
	return(NULL);
}
static NclQuark *NetGetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NclQuark *out_list = NULL;
	int i;

	out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_file_atts);
	stepal = rec->file_atts;
	for(i = 0; i< rec->n_file_atts; i++) {
		out_list[i] = stepal->att_inq->name;
		stepal = stepal->next;
	}
	*num_atts = rec->n_file_atts;
	return(out_list);
}

static NclFAttRec* NetGetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NclFAttRec *tmp;

	stepal = rec->file_atts;
	while(stepal != NULL) {
		if(stepal->att_inq->name == att_name_q) {
			tmp=(NclFAttRec*)NclMalloc((unsigned)sizeof(NclFAttRec));
			tmp->att_name_quark = att_name_q;
/*
* For convenience I make all character attributes strings (except if its the _FillValue or missing_value of a character variable - dib 2009-03-05))
*/
			if(stepal->att_inq->data_type == NC_CHAR && !(att_name_q == Qfill_val || att_name_q == Qmissing_val))  {
				tmp->data_type = NCL_string;
				tmp->num_elements = 1;
			} else {
				tmp->data_type = NetMapToNcl((void*)&(stepal->att_inq->data_type));
				tmp->num_elements = stepal->att_inq->len;
			}
			return(tmp);
		} else {
			stepal = stepal->next;
		}
	}

	return(NULL);
}

static NclQuark *NetGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfAttInqRecList *stepal;
	NclQuark *out_list = NULL;	
	int i;
	*num_atts = 0;
	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name== thevar) {
			stepal = stepvl->var_inq->att_list;
			out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark) * stepvl->var_inq->natts);
			*num_atts = stepvl->var_inq->natts;
			for(i = 0; i< stepvl->var_inq->natts; i++) {
				out_list[i] = stepal->att_inq->name;
				stepal = stepal->next;
			}
			return(out_list);
		} else {
			stepvl = stepvl->next;
		}
	}
		
	return(NULL);
}

static NclFAttRec *NetGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfAttInqRecList *stepal;
	NclFAttRec *tmp = NULL;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			stepal = stepvl->var_inq->att_list;
			while(stepal != NULL) {
				if(stepal->att_inq->name == theatt) {
					tmp= (NclFAttRec*)NclMalloc((unsigned)
						sizeof(NclFAttRec));
					tmp->att_name_quark = theatt;
					if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {

						tmp->data_type = NCL_string;
						tmp->num_elements = 1;
					} else {
						tmp->data_type = NetMapToNcl((void*)&stepal->att_inq->data_type);
						tmp->num_elements = stepal->att_inq->len;
					}
					return(tmp);
				} else {
					stepal = stepal->next;
				}
			}
		} else {
			stepvl = stepvl->next;
		}
	}
		
	return(NULL);
}

static NclFVarRec *NetGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
	return(NetGetVarInfo(therec,thevar));
}

/*
 * this is for 1-D variables only - basically for coordinate variables.
 */
static void *NetGetCachedValue
#if	NhlNeedProto
(NetCdfVarInqRec *var_inq, long start, long finish,long stride,void* storage)
#else
(var_inq,start,finish,stride,storage)
NetCdfVarInqRec *var_inq;
long start;
long finish;
long stride;
void* storage;
#endif
{
	long i,j;
	int tsize = var_inq->data_type < 1 ? 1 : nctypelen(var_inq->data_type);

	for (j = 0, i = start; i <= finish; i += stride,j++) {
		memcpy(((char*)storage) + j * tsize,((char *)var_inq->value) + i * tsize,tsize);
	}
	return storage;
}

static void *NetReadVar
#if	NhlNeedProto
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
	NetCdfFileRecord *rec = (NetCdfFileRecord*) therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfDimInqRecList *stepdl; 
	void *out_data;
	ng_size_t n_elem = 1;
	int cdfid = -1;
	int ret = -1,i;
	int nc_ret = NC_NOERR;
	int no_stride = 1;
	long count[MAX_NC_DIMS];

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			if (stepvl->var_inq->value != NULL && stepvl->var_inq->n_dims == 1) {
				return NetGetCachedValue(stepvl->var_inq,start[0],finish[0],stride[0],storage);
			}
			for(i= 0; i< stepvl->var_inq->n_dims; i++) {
				int dimid;
				count[i] = (long)((finish[i] - start[i])/stride[i]) + 1;
				n_elem *= count[i];
				if(stride[i] != 1) {
					no_stride = 0;
				}
				dimid = stepvl->var_inq->dim[i];
				for (stepdl = rec->dims; stepdl !=NULL; stepdl = stepdl->next) {
					if (stepdl->dim_inq->dimid == dimid) 
						break;
				}
			}
			out_data = storage;

			if (rec->open) {
				cdfid = rec->cdfid;
				EndDefineModeIf(rec,cdfid);
			}
			else if (no_stride) {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_NOWRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				rec->define_mode = 0;
				rec->cdfid = cdfid;
				/* printf ("got size = %d\n",ChunkSizeHint); */
			}
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_NOWRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				rec->define_mode = 0;
				rec->cdfid = cdfid;
			}
				
			if(nc_ret != NC_NOERR) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "NetCdf: Could not reopen the file (%s) for reading",
					  NrmQuarkToString(rec->file_path_q));
				return(NULL);
			}

			rec->open = 1;
			if(no_stride) {	
				ret = nc_get_vara(cdfid,
					stepvl->var_inq->varid,
					start,
					count,
					out_data);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_get_vara(%d,%d,start,count,NULL,NULL,outdata);\n",cdfid,stepvl->var_inq->varid);
#endif                

			} else {
				ret = nc_get_vars(cdfid,
					stepvl->var_inq->varid,
					start,
					count,
					stride,
					out_data);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_get_vars(%d,%d,start,count,stride,NULL,outdata);\n",cdfid,stepvl->var_inq->varid);
#endif
			}
			CloseOrNot(rec,cdfid,0);
			if(ret == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occurred while attempting to read variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
				return(NULL);
			} else {
				return(storage);
			}
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static void *NetReadCoord
#if	NhlNeedProto
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
	return(NetReadVar(therec,thevar,start,finish,stride,storage));
}


static void *NetReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	int cdfid,ret ;
	char *tmp;
	int nc_ret;

	stepal = rec->file_atts;
	while(stepal != NULL) {
		if(stepal->att_inq->name == theatt) {
			if (stepal->att_inq->value != NULL) {
				if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {
					*(NclQuark*)storage = *(NclQuark*)(stepal->att_inq->value);
				} else {
					memcpy(storage,stepal->att_inq->value,
					       nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
				}
				return(storage);
			}
			if (rec->open) {
				cdfid = rec->cdfid;
			}				
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_NOWRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				if(nc_ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for reading",
						  NrmQuarkToString(rec->file_path_q));
					return(NULL);
				}
				rec->open = 1;
				rec->cdfid = cdfid;
				rec->define_mode = 0;
			}
			
			if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {
				tmp = (char*)NclMalloc(stepal->att_inq->len+1);
				tmp[stepal->att_inq->len] = '\0';
				ret = ncattget(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),tmp);
#if NETCDF_DEBUG
				fprintf(stderr,"ncattget(%d,NC_GLOBAL,%s,buffer);\n",cdfid,NrmQuarkToString(theatt));
#endif                
				*(NclQuark*)storage = NrmStringToQuark(tmp);
				NclFree(tmp);
			} else {
				ret = ncattget(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),storage);
#if NETCDF_DEBUG
				fprintf(stderr,"ncattget(%d,NC_GLOBAL,%s,buffer);\n",cdfid,NrmQuarkToString(theatt));
#endif                
			}
			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				if (rec->open) {
					EndDefineModeIf(rec, cdfid);
				}
				CloseOrNot(rec,cdfid,0);
			}
			if (ret != -1) 
				return(storage);
			else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Error retrieving value for global ttribute (%s) of (%s)",
					  NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
				return NULL;
			}
		} else {
			stepal = stepal->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Attribute (%s) is not a global attribute of file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static void *NetReadVarAtt
#if	NhlNeedProto
(void * therec, NclQuark thevar, NclQuark theatt, void * storage)
#else
(therec, thevar, theatt, storage)
void * therec;
NclQuark thevar;
NclQuark theatt;
void* storage;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NetCdfVarInqRecList *stepvl;
	int cdfid;
	int nc_ret;
	int ret;
	char *tmp;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			stepal = stepvl->var_inq->att_list;
			while(stepal != NULL) {
				if(stepal->att_inq->name == theatt) {
					if (stepal->att_inq->value != NULL) {
						if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {
							*(NclQuark*)storage = *(NclQuark*)(stepal->att_inq->value);
						} else {
							memcpy(storage,stepal->att_inq->value,
							       nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
						}
						return(storage);
					}
					if (stepal->att_inq->virtual) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Error retrieving value for virtual attribute (%s) of (%s->%s)",
							  NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
						return NULL;
					}
					if (rec->open) {
						cdfid = rec->cdfid;
					}
					else {
						nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_NOWRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
						fprintf(stderr,"nc__open(\"%s\",NC_NOWRITE,&ChunkSizeHint,&cdfid);\n",
						       NrmQuarkToString(rec->file_path_q));
#endif                

						if(nc_ret != NC_NOERR) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,
								  "NetCdf: Could not reopen the file (%s) for reading",
								  NrmQuarkToString(rec->file_path_q));
							return(NULL);
						}
						rec->cdfid = cdfid;
						rec->define_mode = 0;
						rec->open = 1;
					}
			
					if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt  == Qmissing_val)) {
	
						tmp = (char*)NclMalloc(stepal->att_inq->len + 1);
						tmp[stepal->att_inq->len] = '\0';
						ret = ncattget(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),tmp);
#if NETCDF_DEBUG
						fprintf(stderr,"ncattget(%d,%d,\"%s\",buffer);\n",cdfid,stepvl->var_inq->varid,
						       NrmQuarkToString(theatt));
#endif                
						*(NclQuark*)storage = NrmStringToQuark(tmp);
						NclFree(tmp);
					} else {
						ret = ncattget(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),storage);
#if NETCDF_DEBUG
						fprintf(stderr,"ncattget(%d,%d,\"%s\",value);\n",cdfid,stepvl->var_inq->varid,
						       NrmQuarkToString(theatt));
#endif                
					}
					CloseOrNot(rec,cdfid,0);
					if(ret != -1)
						return(storage);
					else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Error retrieving value for Attribute (%s) of (%s->%s)",
							  NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
						return NULL;
					}
				} else {
					stepal = stepal->next;
				}
			}
			break;
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Attribute (%s) is not a variable attribute of (%s->%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
	return(NULL);
}
static NhlErrorTypes NetWriteVar
#if	NhlNeedProto
(void * therec, NclQuark thevar, void *data, long* start, long *finish,long *stride)
#else
(therec, thevar, data, start, finish,stride)
void * therec;
NclQuark thevar;
void *data;
long *start;
long *finish;
long *stride;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	int cdfid;
	int nc_ret = NC_NOERR;
	NetCdfVarInqRecList *stepvl; 
	NetCdfDimInqRecList *stepdl; 
	long count[MAX_NC_DIMS];
	ng_size_t n_elem = 1;
	int i,no_stride = 1,k;
	int ret;
	int fill_mode;

	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				/*
				 * for now, simply disable caching the value if a variable gets written to
				 */
				if (stepvl->var_inq->value != NULL) {
					NclFree(stepvl->var_inq->value);
					stepvl->var_inq->value = NULL;
				}
				for(i= 0; i< stepvl->var_inq->n_dims; i++) {
					count[i] = (long)((finish[i] - start[i])/stride[i]) + 1;
					n_elem *= count[i];
					if(stride[i] != 1) {
						no_stride = 0;
					}
					stepdl = rec->dims;
					for(k = 0; ((stepdl!=NULL)&&(stepdl->dim_inq->dimid < stepvl->var_inq->dim[i])); k ++) {
						stepdl = stepdl->next;
					}
					if(stepdl->dim_inq->is_unlimited) {
						stepdl->dim_inq->size = MAX(finish[i] + 1,stepdl->dim_inq->size);
					}
					
				}
				if (rec->open) {
					cdfid = rec->cdfid;
					EndDefineModeIf(rec, cdfid);
				}
				else if (no_stride) {
					nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
					fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
					/* printf ("got size = %d\n",ChunkSizeHint); */
				}
				else {
					nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
					fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				}
				if(nc_ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for writing",
						  NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}

				rec->cdfid = cdfid;
				rec->define_mode = 0;
				rec->open = 1;
				if ((int)(rec->options[NC_PREFILL_OPT].values) == 0) {
					nc_set_fill(cdfid,NC_NOFILL,&fill_mode);
#if NETCDF_DEBUG
					fprintf(stderr,"nc_set_fill(%d,NC_NOFILL,&fill_mode);\n",cdfid);
#endif                
				}

	
				if(no_stride) {
					ret = nc_put_vara(cdfid,
							  stepvl->var_inq->varid,
							  (const size_t *)start,
							  (const size_t *)count,
							  data);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_put_vara(%d,%d,start,count,NULL,NULL,outdata);\n",cdfid,stepvl->var_inq->varid);
#endif
				} else {
					ret = nc_put_vars(cdfid,
							  stepvl->var_inq->varid,
							  (const size_t *)start,
							  (const size_t *)count,
							  (const ptrdiff_t *)stride,
							  data);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_put_vars(%d,%d,start,count,stride,NULL,outdata);\n",cdfid,stepvl->var_inq->varid);
#endif
				}
	
				CloseOrNot(rec,cdfid,1);
				if(ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"%s: error attempting to write variable (%s) to file (%s)",nc_strerror(ret),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				} else {
					return(NhlNOERROR);
				}
			} else {
				stepvl = stepvl->next;
			}
		}
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);

	
}
static NhlErrorTypes NetWriteCoord
#if	NhlNeedProto
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
	return(NetWriteVar(therec,thevar,data,start,finish,stride));
}


static NhlErrorTypes NetWriteAtt
#if	NhlNeedProto
(void *therec, NclQuark theatt, void *data )
#else
(therec, theatt, data )
void *therec;
NclQuark theatt;
void *data;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	int cdfid;
	int nc_ret;
	int ret = -1;
	char *buffer=NULL;

	if(rec->wr_status <= 0) {
		stepal = rec->file_atts;
		while(stepal != NULL) {
			if(stepal->att_inq->name == theatt) {
				if (rec->open) {
					cdfid = rec->cdfid;
				}
				else {
					nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
					fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
					if(nc_ret != NC_NOERR) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,
							  "NetCdf: Could not reopen the file (%s) for writing",
							  NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}
					rec->cdfid = cdfid;
					rec->define_mode = 0;
					rec->open = 1;
				}
				if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {
					buffer = NrmQuarkToString(*(NclQuark*)data);
					if(strlen(buffer)+1 > stepal->att_inq->len || stepal->att_inq->virtual) {
						if (! rec->define_mode) {
							ncredef(cdfid);
#if NETCDF_DEBUG
							fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
							rec->define_mode = 1;
						}
					}
					ret = ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),
						       stepal->att_inq->data_type,strlen(buffer),(void*)buffer);
#if NETCDF_DEBUG
					fprintf(stderr,"ncattput(%d,NC_GLOBAL,\"%s\",%d,%d,buffer);\n",cdfid,
					       NrmQuarkToString(theatt),(int)stepal->att_inq->data_type,strlen(buffer));
#endif                
					if (stepal->att_inq->value != NULL)
						memcpy(stepal->att_inq->value,data,sizeof(NclQuark));
					stepal->att_inq->virtual = 0;
				} else {
					if(stepal->att_inq->virtual) {
						if (! rec->define_mode) {
							ncredef(cdfid);
#if NETCDF_DEBUG
							fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
							rec->define_mode = 1;
						}
					}
					ret = ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),
						       stepal->att_inq->data_type,stepal->att_inq->len,data);
#if NETCDF_DEBUG
					fprintf(stderr,"ncattput(%d,NC_GLOBAL,\"%s\",%d,%d,data);\n",cdfid,
					       NrmQuarkToString(theatt),(int)stepal->att_inq->data_type,stepal->att_inq->len);
#endif                
					if (stepal->att_inq->value != NULL) {
						memcpy(stepal->att_inq->value,data,
						        nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
					}
					stepal->att_inq->virtual = 0;
				}
	
				
				if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
					EndDefineModeIf(rec, cdfid);
					CloseOrNot(rec,cdfid,0);
				}
					
				if(ret == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occurred while attempting to write the attribute (%s) to file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
				return(NhlNOERROR);
			} else {
				stepal = stepal->next;
			}
		}	
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetDelAtt
#if 	NhlNeedProto
(void *therec, NclQuark theatt)
#else 
(therec, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NetCdfAttInqRecList *prev;
	int cdfid;
	int nc_ret;
	int ret = 0;

	if(rec->wr_status <= 0) {
		stepal = rec->file_atts;
		prev = NULL;
		while (stepal != NULL) {
			if (stepal->att_inq->name != theatt) {
				prev = stepal;
				stepal = stepal->next;
				continue;
			}
			if (! stepal->att_inq->virtual) {
				if (rec->open) {
					cdfid = rec->cdfid;
				}
				else {
					nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
					fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
					if(nc_ret != NC_NOERR) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,
							  "NetCdf: Could not reopen the file (%s) for writing",
							  NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}
					rec->cdfid = cdfid;
					rec->define_mode = 0;
					rec->open = 1;
				}
				if (! rec->define_mode) {
					ncredef(cdfid);
#if NETCDF_DEBUG
					fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
					rec->define_mode = 1;
				}
				ret = ncattdel(cdfid,NC_GLOBAL,(const char*)NrmQuarkToString(theatt));
#if NETCDF_DEBUG
				fprintf(stderr,"ncattdel(%d,NC_GLOBAL,\"%s\");\n",cdfid,NrmQuarkToString(theatt));
#endif                
				if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
					EndDefineModeIf(rec, cdfid);
					CloseOrNot(rec,cdfid,0);
				}
			}
			if (! prev) {
				rec->file_atts = stepal->next;
			}
			else {
				prev->next = stepal->next;
			}
			if (stepal->att_inq->value)
				NclFree(stepal->att_inq->value);
			NclFree(stepal->att_inq);
			NclFree(stepal);
			if(ret == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occurred while attempting to delete the attribute (%s) from file (%s)",
					  NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			return(NhlNOERROR);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetDelVarAtt
#if 	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else 
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NetCdfAttInqRecList *prev;
	NetCdfVarInqRecList *stepvl;
	int cdfid;
	int nc_ret;
	int ret = 0;

	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				stepal = stepvl->var_inq->att_list;
				prev = NULL;
				while (stepal != NULL) {
					if (stepal->att_inq->name != theatt) {
						prev = stepal;
						stepal = stepal->next;
						continue;
					}
					if (! stepal->att_inq->virtual) {
						if (rec->open) {
							cdfid = rec->cdfid;
						}
						else {
							nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
							fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",
							       NrmQuarkToString(rec->file_path_q));
#endif                
							if(nc_ret != NC_NOERR) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,
									  "NetCdf: Could not reopen the file (%s) for writing",
									  NrmQuarkToString(rec->file_path_q));
								return(NhlFATAL);
							}
							rec->cdfid = cdfid;
							rec->define_mode = 0;
							rec->open = 1;
						}
						if (! rec->define_mode) {
							ncredef(cdfid);
#if NETCDF_DEBUG
							fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
							rec->define_mode = 1;
						}
						ret = ncattdel(cdfid,stepvl->var_inq->varid,(const char*)NrmQuarkToString(theatt));
#if NETCDF_DEBUG
						fprintf(stderr,"ncattdel(%d,%d,\"%s\");\n",cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt));
#endif                
						if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
							EndDefineModeIf(rec, cdfid);
							CloseOrNot(rec,cdfid,0);
						}
					}
					if (! prev) {
						stepvl->var_inq->att_list = stepal->next;
					}
					else {
						prev->next = stepal->next;
					}
					if (stepal->att_inq->value)
						NclFree(stepal->att_inq->value);
					NclFree(stepal->att_inq);
					NclFree(stepal);
					if(ret == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,
							  "NetCdf: An error occurred while attempting to delete the attribute (%s) from variable (%s) in file (%s)",
							  NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
						return(NhlFATAL);
					}
					return(NhlNOERROR);
				} 
			} else {
				stepvl = stepvl->next;
			}
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}
static NhlErrorTypes NetWriteVarAtt 
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt, void* data)
#else
(therec,thevar, theatt,  data )
void *therec;
NclQuark thevar;
NclQuark theatt;
void* data;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NetCdfVarInqRecList *stepvl;
	int cdfid;
	int nc_ret;
	int ret;
	char * buffer = NULL;

	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				stepal = stepvl->var_inq->att_list;
				while(stepal != NULL) {
					if(stepal->att_inq->name == theatt) {
						if (! stepal->att_inq->virtual) {
							/* if the value is the same as before don't bother writing it */
							if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {	
								if (*(NrmQuark*)stepal->att_inq->value == *(NrmQuark*)data) {
									return NhlNOERROR;
								}
							}
							else {
								if (! memcmp(stepal->att_inq->value,data,
									     nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len)) {
									return NhlNOERROR;
								}
							}
						}
						if (rec->open) {
							cdfid = rec->cdfid;
						}
						else {
							nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
							fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",
							       NrmQuarkToString(rec->file_path_q));
#endif                
							if(nc_ret != NC_NOERR) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,
									  "NetCdf: Could not reopen the file (%s) for writing",
									  NrmQuarkToString(rec->file_path_q));
								return(NhlFATAL);
							}
							rec->cdfid = cdfid;
							rec->define_mode = 0;
							rec->open = 1;
						}
						if(stepal->att_inq->data_type == NC_CHAR && !(theatt == Qfill_val || theatt == Qmissing_val)) {	
							buffer = NrmQuarkToString(*(NclQuark*)data);
							if(strlen(buffer)+1 > stepal->att_inq->len || stepal->att_inq->virtual) {
								if (! rec->define_mode) {
									ncredef(cdfid);
#if NETCDF_DEBUG
									fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
									rec->define_mode = 1;
								}
							}
							ret = ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),
								       stepal->att_inq->data_type,strlen(buffer),buffer);
#if NETCDF_DEBUG
							fprintf(stderr,"ncattput(%d,%d,\"%s\",%d,%d,buffer);\n",cdfid,
							       stepvl->var_inq->varid,
							       NrmQuarkToString(theatt),(int)stepal->att_inq->data_type,strlen(buffer));
#endif                
							if (ret != -1 && stepal->att_inq->value != NULL)
								memcpy(stepal->att_inq->value,data,sizeof(NclQuark));
							stepal->att_inq->virtual = 0;
						}
						else {
							if(stepal->att_inq->virtual) {
								if (! rec->define_mode) {
									ncredef(cdfid);
#if NETCDF_DEBUG
									fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
									rec->define_mode = 1;
								}
							}
							ret = ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),
								       stepal->att_inq->data_type,stepal->att_inq->len,data);
#if NETCDF_DEBUG
							fprintf(stderr,"ncattput(%d,%d,\"%s\",%d,%d,data);\n",cdfid,
							       stepvl->var_inq->varid,
							       NrmQuarkToString(theatt),(int)stepal->att_inq->data_type,stepal->att_inq->len);
#endif                
							if (ret != -1 && stepal->att_inq->value != NULL) {
								memcpy(stepal->att_inq->value,data,
								       nctypelen(stepal->att_inq->data_type)*stepal->att_inq->len);
							}
							stepal->att_inq->virtual = 0;
						}
		
						if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
							EndDefineModeIf(rec, cdfid);
							CloseOrNot(rec,cdfid,0);
						}
						if(ret == -1) {
							if (theatt == NrmStringToQuark("_FillValue") && rec->format > 2) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"NetCdf: NetCDF 4 does not allow the _FillValue attribute to be modified after data written to variable (%s) in file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
								return (NhlWARNING);
							}
							else {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occurred while attempting to write the attribute (%s) to variable (%s) in file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
								return(NhlFATAL);
							}
						}
						return(NhlNOERROR);
					} else {	
						stepal = stepal->next;
					}
				}	
			} else {
				stepvl = stepvl->next;
			}
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddVarChunk
#if    NhlNeedProto
(void* therec, NclQuark thevar, int n_chunk_dims, ng_size_t *chunk_dims)
#else
(therec,thevar,n_chunk_dims,chunk_dims)
void* therec;
NclQuark thevar;
int n_chunk_dims;
ng_size_t *chunk_dims;
#endif
{
    NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
    NetCdfVarInqRecList *stepvl = NULL;
    int i,ret = NhlNOERROR;
#ifdef USE_NETCDF4_FEATURES
    int cdfid;
    int nc_ret;
    int storage = NC_CHUNKED;

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            cdfid = rec->cdfid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
            if(nc_ret != NC_NOERR)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "NetCdf: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->cdfid = cdfid;
            rec->define_mode = 0;
            rec->open = 1;
        }

        stepvl = rec->vars;
        while(stepvl != NULL)
        {
            if(stepvl->var_inq->name == thevar)
            {
                if(n_chunk_dims != stepvl->var_inq->n_dims)
                {    
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                             "Var (%s) has different chunk_dims to its dimensionality.\n",
                              NrmQuarkToString(thevar));
                    ret = NhlFATAL;
                    break;
                }

                stepvl->var_inq->n_chunk_dims = n_chunk_dims;
                for(i = 0 ; i < n_chunk_dims; i++)
                {
                    stepvl->var_inq->chunk_dim[i] = (ng_size_t)chunk_dims[i];
                }
                nc_ret = nc_def_var_chunking(cdfid, stepvl->var_inq->varid, storage,
                                             (size_t *)stepvl->var_inq->chunk_dim);
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

#endif
    return(ret);
}

static NhlErrorTypes NetAddVarChunkCache
#if    NhlNeedProto
(void* therec, NclQuark thevar, ng_size_t cache_size, ng_size_t cache_nelems, float cache_preemption)
#else
(therec, thevar, cache_size, cache_nelems, cache_preemption)
void* therec;
NclQuark thevar;
ng_size_t cache_size;
ng_size_t cache_nelems;
float cache_preemption;
#endif
{
    NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
    NetCdfVarInqRecList *stepvl = NULL;
    int ret = NhlNOERROR;
    int cdfid;
    int nc_ret;
#ifdef USE_NETCDF4_FEATURES

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            cdfid = rec->cdfid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
            if(nc_ret != NC_NOERR)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "NetAddVarChunkCache: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->cdfid = cdfid;
            rec->define_mode = 0;
            rec->open = 1;
        }

        stepvl = rec->vars;
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

                if(stepvl->var_inq->use_cache)
		{
			nc_ret = nc_set_var_chunk_cache(cdfid, stepvl->var_inq->varid,
                                                cache_size, cache_nelems,
                                                stepvl->var_inq->cache_preemption);
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

#endif
    return(ret);
}

static NhlErrorTypes NetSetVarCompressLevel
#if    NhlNeedProto
(void* therec, NclQuark thevar, int compress_level)
#else
(therec,thevar,compress_level)
void* therec;
NclQuark thevar;
int compress_level;
#endif
{
    NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
    NetCdfVarInqRecList *stepvl = NULL;
    int ret = NhlNOERROR;
    int cdfid;
    int shuffle = 0;
    int deflate = compress_level;
    int deflate_level = compress_level;
    int nc_ret;
#ifdef USE_NETCDF4_FEATURES

    if(rec->wr_status <= 0)
    {
        if (rec->open)
        {
            cdfid = rec->cdfid;
        }
        else
        {
            nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
            if(nc_ret != NC_NOERR)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                      "NetCdf: Could not reopen the file (%s) for writing",
                      NrmQuarkToString(rec->file_path_q));
                return(NhlFATAL);
            }
            rec->cdfid = cdfid;
            rec->define_mode = 0;
            rec->open = 1;
        }

        stepvl = rec->vars;
        while(stepvl != NULL)
        {
            if(stepvl->var_inq->name == thevar)
            {
                stepvl->var_inq->compress_level = compress_level;
                if(compress_level > 0)
                    deflate = compress_level;
                nc_ret = nc_def_var_deflate(cdfid, stepvl->var_inq->varid, shuffle,
                                            deflate, deflate_level);
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
#endif

    return(ret);
}

static NhlErrorTypes NetAddDim
#if	NhlNeedProto
(void* therec, NclQuark thedim, ng_size_t size,int is_unlimited)
#else
(therec, thedim, size,is_unlimited)
void* therec;
NclQuark thedim;
ng_size_t size;
int is_unlimited;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*) therec;
	int cdfid;
	int nc_ret;
	NetCdfDimInqRecList *stepdl;
	int ret = -1;
	int add_scalar = 0;

	if(rec->wr_status <=  0) {
		
		if(thedim == NrmStringToQuark("ncl_scalar")) {
			if (size != 1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "NetCdf: \"ncl_scalar\" is a reserved file dimension name in NCL, this name can only represent dimensions of size 1");
				return(NhlFATAL);
			}
			add_scalar = 1;
		}
		else {
			if (rec->open) {
				cdfid = rec->cdfid;
			}
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				if(nc_ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for writing",
						  NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
				rec->cdfid = cdfid;
				rec->define_mode = 0;
				rec->open = 1;
			}
			if (! rec->define_mode) {
				ncredef(cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
				rec->define_mode = 1;
			}
			if(is_unlimited) {
				ret = ncdimdef(cdfid,NrmQuarkToString(thedim),NC_UNLIMITED);
#if NETCDF_DEBUG
				fprintf(stderr,"dimid = ncdimdef(%d,\"%s\",NC_UNLIMITED);\n",cdfid,NrmQuarkToString(thedim));
				fprintf(stderr,"dimid = %d\n",ret);
#endif                

			} else {
				ret = ncdimdef(cdfid,NrmQuarkToString(thedim),(long)size);
#if NETCDF_DEBUG
				fprintf(stderr,"dimid = ncdimdef(%d,\"%s\",%d);\n",cdfid,NrmQuarkToString(thedim),size);
				fprintf(stderr,"dimid = %d\n",ret);
#endif                
			}
			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				EndDefineModeIf(rec, cdfid);
				CloseOrNot(rec,cdfid,0);
			}
			if(ret == -1) {
				return(NhlFATAL);
			}
		}
		stepdl = rec->dims;

		if (add_scalar) {
			rec->has_scalar_dim = 1;
			rec->dims = (NetCdfDimInqRecList*)NclMalloc(
				(unsigned) sizeof(NetCdfDimInqRecList));
			rec->dims->dim_inq = (NetCdfDimInqRec*)NclMalloc(
				(unsigned)sizeof(NetCdfDimInqRec));
			rec->dims->next = stepdl;
			rec->dims->dim_inq->dimid = -5;
			rec->dims->dim_inq->size = 1;
			rec->dims->dim_inq->is_unlimited = 0;
			rec->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
			rec->n_dims++;
		}
		else if(stepdl == NULL) {
			rec->dims = (NetCdfDimInqRecList*)NclMalloc((unsigned)sizeof(NetCdfDimInqRecList));
			rec->dims->dim_inq = (NetCdfDimInqRec*)NclMalloc((unsigned)sizeof(NetCdfDimInqRec));
			rec->dims->dim_inq->dimid = ret;
			rec->dims->dim_inq->name = thedim;
			if(is_unlimited) {
				rec->dims->dim_inq->size = (long)0;
			} else {
				rec->dims->dim_inq->size = (long)size;
			}
			rec->dims->dim_inq->is_unlimited= is_unlimited;
			rec->dims->next = NULL;
			rec->n_dims = 1;
		}
		else {
			while(stepdl->next != NULL) {
				stepdl = stepdl->next;
			}
			stepdl->next = (NetCdfDimInqRecList*)NclMalloc((unsigned)sizeof(NetCdfDimInqRecList));
			stepdl->next->dim_inq = (NetCdfDimInqRec*)NclMalloc((unsigned)sizeof(NetCdfDimInqRec));
			stepdl->next->dim_inq->dimid = ret;
			stepdl->next->dim_inq->name = thedim;
			if(is_unlimited) {
				stepdl->next->dim_inq->size = (long)0;
			} else {
				stepdl->next->dim_inq->size = (long)size;
			}
			stepdl->next->dim_inq->is_unlimited= is_unlimited;
			stepdl->next->next = NULL;
			rec->n_dims++;
		}
		return(NhlNOERROR);
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddChunkDim
#if	NhlNeedProto
(void* therec, NclQuark thedim, ng_size_t size,int is_unlimited)
#else
(therec, thedim, size,is_unlimited)
void* therec;
NclQuark thedim;
ng_size_t size;
int is_unlimited;
#endif
{
#ifdef USE_NETCDF4_FEATURES
	NetCdfFileRecord *rec = (NetCdfFileRecord*) therec;
	int cdfid;
	int nc_ret;
	NetCdfDimInqRecList *stepdl;
	int ret = -1;
	int add_scalar = 0;

	if(rec->wr_status <=  0) {
		
		if(thedim == NrmStringToQuark("ncl_scalar")) {
			if (size != 1) {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"NetCdf: \"ncl_scalar\" is a reserved file dimension name in NCL, %s",
					"this name can only represent dimensions of size 1"));
				return(NhlFATAL);
			}
			add_scalar = 1;
		}
		else {
			if (rec->open) {
				cdfid = rec->cdfid;
			}
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				if(nc_ret != NC_NOERR) {
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for writing",
						  NrmQuarkToString(rec->file_path_q)));
					return(NhlFATAL);
				}
				rec->cdfid = cdfid;
				rec->define_mode = 0;
				rec->open = 1;
			}
			if (! rec->define_mode) {
				ncredef(cdfid);
				rec->define_mode = 1;
			}
		}
		stepdl = rec->chunk_dims;

		if (add_scalar) {
			rec->has_scalar_dim = 1;
			rec->chunk_dims = (NetCdfDimInqRecList*)NclMalloc(
				(unsigned) sizeof(NetCdfDimInqRecList));
			rec->chunk_dims->dim_inq = (NetCdfDimInqRec*)NclMalloc(
				(unsigned)sizeof(NetCdfDimInqRec));
			rec->chunk_dims->next = stepdl;
			rec->chunk_dims->dim_inq->dimid = -5;
			rec->chunk_dims->dim_inq->size = 1;
			rec->chunk_dims->dim_inq->is_unlimited = 0;
			rec->chunk_dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
			rec->n_chunk_dims++;
		}
		else if(stepdl == NULL) {
			rec->chunk_dims = (NetCdfDimInqRecList*)NclMalloc((unsigned)sizeof(NetCdfDimInqRecList));
			rec->chunk_dims->dim_inq = (NetCdfDimInqRec*)NclMalloc((unsigned)sizeof(NetCdfDimInqRec));
			rec->chunk_dims->dim_inq->dimid = ret;
			rec->chunk_dims->dim_inq->name = thedim;
			rec->chunk_dims->dim_inq->size = (long)size;
			rec->chunk_dims->dim_inq->is_unlimited= is_unlimited;
			if(rec->chunk_dims->dim_inq->size < 1)
				rec->chunk_dims->dim_inq->size = (long)1;
			rec->chunk_dims->next = NULL;
			rec->n_chunk_dims = 1;
		}
		else {
			while(stepdl->next != NULL) {
				stepdl = stepdl->next;
			}
			stepdl->next = (NetCdfDimInqRecList*)NclMalloc((unsigned)sizeof(NetCdfDimInqRecList));
			stepdl->next->dim_inq = (NetCdfDimInqRec*)NclMalloc((unsigned)sizeof(NetCdfDimInqRec));
			stepdl->next->dim_inq->dimid = ret;
			stepdl->next->dim_inq->name = thedim;
			stepdl->next->dim_inq->size = (long)size;
			stepdl->next->dim_inq->is_unlimited= is_unlimited;
			if(stepdl->next->dim_inq->size < 1)
				stepdl->next->dim_inq->size = (long)1;
			stepdl->next->next = NULL;
			rec->n_chunk_dims++;
		}
		return(NhlNOERROR);
	} else {	
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"File (%s) was opened as a read only file, can not write to it",
			NrmQuarkToString(rec->file_path_q)));
	}
#endif
	return(NhlFATAL);
}

/*ARGSUSED*/
static NhlErrorTypes NetAddVar
#if	NhlNeedProto
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
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl = NULL;
	int cdfid,i,ret;
	int nc_ret;
	nc_type *the_data_type;
	int dim_ids[MAX_NC_DIMS];
	NetCdfDimInqRecList* stepdl = NULL;
	int add_scalar_dim = 0;
	int fill_mode;

	if(rec->wr_status <= 0) {
		if (rec->open) {
			cdfid = rec->cdfid;
		}
		else {
			nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
			fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
			if(nc_ret != NC_NOERR) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "NetCdf: Could not reopen the file (%s) for writing",
					  NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			rec->cdfid = cdfid;
			rec->define_mode = 0;
			rec->open = 1;
		}
		if ((int)(rec->options[NC_PREFILL_OPT].values) == 0) {
			nc_set_fill(cdfid,NC_NOFILL,&fill_mode);
#if NETCDF_DEBUG
			fprintf(stderr,"nc_set_fill(%d,NC_NOFILL,&fill_mode);\n",cdfid);
#endif                
		}
		the_data_type = NetMapFromNcl(data_type);
/*
* All dimensions are correct dimensions for the file
*/
		dim_ids[0] = -999;
		for(i = 0; i < n_dims; i++) {
			stepdl = rec->dims;
			while(stepdl != NULL) {
				if(stepdl->dim_inq->name == dim_names[i]){
					if((n_dims > 1)&&(dim_names[i] == NrmStringToQuark("ncl_scalar"))) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: the reserved file dimension name \"ncl_scalar\" was used in a value with more than one dimension, can not add variable");
						return(NhlFATAL);
					}
					dim_ids[i] = stepdl->dim_inq->dimid;
					break;
				} else {
					stepdl = stepdl->next;
				}
			}
		} 
		if (dim_ids[0] == -999) {
			if (n_dims == 1 && dim_sizes[0] == 1 && dim_names[0] == NrmStringToQuark("ncl_scalar")) {
				dim_ids[0] = -5;
				add_scalar_dim = 1;
			}
			else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: internal error adding variable");
				return(NhlFATAL);
			}
		}
		if(the_data_type != NULL) {
			int var_id;
			if (! rec->define_mode) {
				ncredef(cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
				rec->define_mode = 1;
			}
			if((n_dims == 1)&&(dim_ids[0] == -5)) {
				ret = nc_def_var(cdfid,NrmQuarkToString(thevar),*the_data_type, 0, NULL,&var_id);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_def_var(%d,\"%s\",%d,0,NULL,&var_id);\n",cdfid,NrmQuarkToString(thevar),(int)*the_data_type);
#endif                
			} else {
				ret = nc_def_var(cdfid,NrmQuarkToString(thevar),
						 *the_data_type, n_dims, dim_ids,&var_id);
#if NETCDF_DEBUG
				fprintf(stderr,"nc_def_var(%d,\"%s\",%d,%d,dim_ids,&var_id);\n",
				       cdfid,NrmQuarkToString(thevar),(int)*the_data_type,n_dims);
#endif                
#ifdef USE_NETCDF4_FEATURES
				if (ret == NC_NOERR && rec->format > 2 &&
				    ((int)(rec->options[NC_COMPRESSION_LEVEL_OPT].values) > -1)) {
					int shuffle = 1;
					int deflate = 1;
					int deflate_level;
					deflate_level = (int)(rec->options[NC_COMPRESSION_LEVEL_OPT].values);
					ret  = nc_def_var_deflate(cdfid,var_id,shuffle,deflate,deflate_level);
#if NETCDF_DEBUG
					fprintf(stderr,"nc_def_var_deflate(%d,%d,%d,%d,%d);\n",cdfid,var_id,shuffle,deflate,deflate_level);
#endif                
				}
#endif
			}
			if(ret < 0) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,(char*)nc_strerror(ret));
				NclFree(the_data_type);
				return(NhlFATAL);
			} 
			rec->n_vars++;
			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				EndDefineModeIf(rec, cdfid);
				CloseOrNot(rec,cdfid,0);
			}
	
			stepvl = rec->vars;
			if(stepvl == NULL) {
				rec->vars = (NetCdfVarInqRecList*)NclMalloc(
                                        (unsigned)sizeof(NetCdfVarInqRecList));
				rec->vars->next = NULL;
				rec->vars->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec));
				rec->vars->var_inq->varid = var_id;
				rec->vars->var_inq->name = thevar;
				rec->vars->var_inq->data_type = *the_data_type;
				rec->vars->var_inq->n_dims = n_dims;
				rec->vars->var_inq->n_chunk_dims = 0;
				rec->vars->var_inq->use_cache = 0;
				rec->vars->var_inq->natts = 0;
				rec->vars->var_inq->att_list = NULL;
				rec->vars->var_inq->value = NULL;
				for(i = 0 ; i< n_dims; i++) {
					rec->vars->var_inq->dim[i] = dim_ids[i];
					rec->vars->var_inq->chunk_dim[i] = dim_ids[i];
				}
			} else {
				while(stepvl->next != NULL) {
					stepvl= stepvl->next;
				}
				stepvl->next = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRecList));
				stepvl->next->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec));
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
				for(i = 0 ; i< n_dims; i++) {
					stepvl->next->var_inq->dim[i] = dim_ids[i];
				}
			}
			if (add_scalar_dim) {
				rec->has_scalar_dim = 1;
				stepdl = rec->dims;
				rec->dims = (NetCdfDimInqRecList*)NclMalloc(
					(unsigned) sizeof(NetCdfDimInqRecList));
				rec->dims->dim_inq = (NetCdfDimInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfDimInqRec));
				rec->dims->next = stepdl;
				rec->dims->dim_inq->dimid = -5;
				rec->dims->dim_inq->size = 1;
				rec->dims->dim_inq->is_unlimited = 0;
				rec->dims->dim_inq->name = NrmStringToQuark("ncl_scalar");
				rec->n_dims++;
			}
			NclFree(the_data_type);
			return(NhlNOERROR);
		} else {
			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				EndDefineModeIf(rec, cdfid);
				CloseOrNot(rec,cdfid,0);
			}
		}
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

#if 0
/* dib 7/13/05 I don't think this is used so let's eliminate it */
static NhlErrorTypes NetAddCoordVar
#if	NhlNeedProto
(void *therec, NclQuark thevar,NclBasicDataTypes data_type)
#else
(therec,thevar,data_type)
void *therec;
NclQuark thevar;
NclBasicDataTypes data_type;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfDimInqRecList *stepdl = NULL;
	NetCdfVarInqRecList *stepvl = NULL;
	int cdfid;
	int nc_ret;
	int ret,size;
	nc_type *the_data_type;

	if(rec->wr_status <= 0) {
		if (rec->open) {
			cdfid = rec->cdfid;
		}
		else {
			nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
			fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
			if(nc_ret != NC_NOERR) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "NetCdf: Could not reopen the file (%s) for writing",
					  NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			rec->cdfid = cdfid;
			rec->define_mode = 0;
			rec->open = 1;
		}
		the_data_type = NetMapFromNcl(data_type);
		if(the_data_type != NULL) {
			stepdl = rec->dims;
			while(stepdl != NULL ) {
				if(stepdl->dim_inq->name == thevar){
					if (! rec->define_mode) {
						ncredef(cdfid);
#if NETCDF_DEBUG
						fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
						rec->define_mode = 1;
					}
					size = stepdl->dim_inq->size;
					ret = ncvardef(cdfid,NrmQuarkToString(thevar),*the_data_type,1,&size);
#if NETCDF_DEBUG
					fprintf(stderr,"varid = ncvardef(%d,\"%s\",%d,1,size);\n",cdfid,NrmQuarkToString(thevar),*the_data_type);
					fprintf(stderr,"varid = %d\n",ret);
#endif                
					if(ret == -1) {
						ncabort(cdfid);
						ncclose(cdfid);
#if NETCDF_DEBUG
						fprintf(stderr,"ncabort(%d);\n",cdfid);
						fprintf(stderr,"ncclose(%d);\n",cdfid);
#endif                
						rec->cdfid = -1;
						rec->open = 0;
						NclFree(the_data_type);
						return(NhlFATAL);
					} 
				}
			} 
			stepvl = rec->vars;
			if(stepvl == NULL) {
				rec->vars = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRecList));
				rec->vars->next = NULL;
				rec->vars->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec*));
				rec->vars->var_inq->varid = ret;
				rec->vars->var_inq->name = thevar;
				rec->vars->var_inq->data_type = *the_data_type;
				rec->vars->var_inq->n_dims = 1;
				rec->vars->var_inq->dim[0] = stepdl->dim_inq->dimid;
				rec->vars->var_inq->natts = 0;
				rec->vars->var_inq->att_list = NULL;
				rec->vars->var_inq->value = NULL;
				rec->n_vars++;
			} else {
				while(stepvl->next != NULL) {
					stepvl = stepvl->next;
				}
				stepvl->next = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRecList));
				stepvl->next->next = NULL;
				stepvl->next->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec*));
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
		} else {
			ncclose(cdfid);
#if NETCDF_DEBUG
			fprintf(stderr,"ncclose(%d);\n",cdfid);
#endif                
			rec->cdfid = -1;
			rec->open = 0;
		}
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}
#endif

static NhlErrorTypes NetRenameDim
#if	NhlNeedProto
(void* therec, NclQuark from, NclQuark to)
#else
(therec, from, to)
void* therec;
NclQuark from;
NclQuark to;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfDimInqRecList *stepdl;
	int cdfid,ret;
	int nc_ret;

	if(to == NrmStringToQuark("ncl_scalar")) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf : \"ncl_scalar\" is a reserved file dimension name in NCL: other dimensions can not be changed to it");
                return(NhlFATAL);
	}
	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->name == from) {
			if(stepdl->dim_inq->dimid == -5) {
				stepdl->dim_inq->name = to;
				return(NhlNOERROR);
			}
			if (rec->open) {
				cdfid = rec->cdfid;
			}
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				if(nc_ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for writing",
						  NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
				rec->cdfid = cdfid;
				rec->define_mode = 0;
				rec->open = 1;
			}
			if (! rec->define_mode) {
				ncredef(cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
				rec->define_mode = 1;
			}
			ret = ncdimrename(cdfid,stepdl->dim_inq->dimid,NrmQuarkToString(to));
#if NETCDF_DEBUG
			fprintf(stderr,"ncdimrename(%d,%d,\"%d\");\n",cdfid,stepdl->dim_inq->dimid,NrmQuarkToString(to));
#endif                

			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				EndDefineModeIf(rec, cdfid);
				CloseOrNot(rec,cdfid,0);
			}
			if(ret == -1) {
				return(NhlFATAL);
			} else {
				stepdl->dim_inq->name = to;
				return(NhlNOERROR);
			}
		} else {
			stepdl = stepdl->next;
		}
	}
	return(NhlFATAL);
}

static void NetCacheAttValue
#if	NhlNeedProto
(NetCdfAttInqRec *att_inq,void *value)
#else
(att_inq,value)
	NetCdfAttInqRec *att_inq;
	void *value;
#endif
{
	if (att_inq->data_type < 1 || value == NULL) {
		att_inq->value = NULL;
	}
	else if (att_inq->data_type == NC_CHAR && !(att_inq->name == Qfill_val || att_inq->name == Qmissing_val)) {
		char *tmp = NclMalloc(att_inq->len + 1);
		strncpy(tmp,value,att_inq->len);
		tmp[att_inq->len] = '\0';
		att_inq->value = NclMalloc(sizeof(NclQuark));
		*(NclQuark*)att_inq->value = NrmStringToQuark(tmp);
		NclFree(tmp);
	}
	else {
		att_inq->value = NclMalloc(nctypelen(att_inq->data_type) * att_inq->len);
		memcpy(att_inq->value,value,nctypelen(att_inq->data_type) * att_inq->len);
	}
	return;
}

static NhlErrorTypes NetAddAtt
#if	NhlNeedProto
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
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList* stepal;
	nc_type *the_data_type;
	int i,ret;
	int cdfid;
	int nc_ret;
	

	if(rec->wr_status <= 0) {
		the_data_type = (nc_type*)NetMapFromNcl(data_type);
		if(the_data_type != NULL) {
			if (rec->open) {
				cdfid = rec->cdfid;
			}
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				if(nc_ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for writing",
						  NrmQuarkToString(rec->file_path_q));
					NclFree(the_data_type);
					return(NhlFATAL);
				}
				rec->cdfid = cdfid;
				rec->define_mode = 0;
				rec->open = 1;
			}
			if (! rec->define_mode) {
				ncredef(cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
				rec->define_mode = 1;
			}
			ret = ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),*the_data_type,n_items,values);
#if NETCDF_DEBUG
			fprintf(stderr,"ncattput(%d,NC_GLOBAL,\"%s\",%d,%d,values);\n",cdfid,
			       NrmQuarkToString(theatt),(int)*the_data_type,n_items);
#endif                
			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				EndDefineModeIf(rec, cdfid);
				CloseOrNot(rec,cdfid,0);
			}
			if(ret != -1 ) {
				stepal = rec->file_atts;
				if(stepal == NULL) {
					rec->file_atts = (NetCdfAttInqRecList*)NclMalloc((unsigned)
						sizeof(NetCdfAttInqRecList));
					rec->file_atts->att_inq= (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					rec->file_atts->next = NULL;
					rec->file_atts->att_inq->att_num = 1;
					rec->file_atts->att_inq->virtual = 0;
					rec->file_atts->att_inq->name = theatt;
					rec->file_atts->att_inq->data_type = *the_data_type;
					rec->file_atts->att_inq->len = n_items;
					NetCacheAttValue(rec->file_atts->att_inq,values);
				} else {	
					i = 0;
					while(stepal->next != NULL) {
						stepal = stepal->next; 
						i++;
					}
					stepal->next = (NetCdfAttInqRecList*)NclMalloc((unsigned)sizeof(NetCdfAttInqRecList));
					stepal->next->att_inq = (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					stepal->next->att_inq->att_num = i;
					stepal->next->att_inq->virtual = 0;
					stepal->next->att_inq->name = theatt;
					stepal->next->att_inq->data_type = *the_data_type;
					stepal->next->att_inq->len = n_items;
					stepal->next->next = NULL;
					NetCacheAttValue(stepal->next->att_inq,values);
				}
				rec->n_file_atts++;
				NclFree(the_data_type);
				return(NhlNOERROR);
			} 
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddVarAtt
#if	NhlNeedProto
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
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList* stepal;
	NetCdfVarInqRecList* stepvl;
	nc_type *the_data_type;
	int i;
	int cdfid,ret;
	int nc_ret;
	
	if(rec->wr_status <= 0) {
		the_data_type = (nc_type*)NetMapFromNcl(data_type);
		if(the_data_type != NULL) {
			if (rec->open) {
				cdfid = rec->cdfid;
			}
			else {
				nc_ret = nc__open(NrmQuarkToString(rec->file_path_q),NC_WRITE,&ChunkSizeHint,&cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"nc__open(\"%s\",NC_WRITE,&ChunkSizeHint,&cdfid);\n",NrmQuarkToString(rec->file_path_q));
#endif                
				if(nc_ret != NC_NOERR) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "NetCdf: Could not reopen the file (%s) for writing",
						  NrmQuarkToString(rec->file_path_q));
					NclFree(the_data_type);
					return(NhlFATAL);
				}
				rec->cdfid = cdfid;
				rec->define_mode = 0;
				rec->open = 1;
			}
			stepvl = rec->vars;	
			while(stepvl != NULL) {
				if(stepvl->var_inq->name == thevar) {
					break;
				} else {
					stepvl = stepvl->next;
				}
			}
			if (! rec->define_mode) {
				ncredef(cdfid);
#if NETCDF_DEBUG
				fprintf(stderr,"ncredef(%d);\n",cdfid);
#endif                
				rec->define_mode = 1;
			}
			ret = ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),*the_data_type,n_items,values);
#if NETCDF_DEBUG
			fprintf(stderr,"ncattput(%d,%d,\"%s\",%d,%d,values);\n",cdfid,
			       stepvl->var_inq->varid,
			       NrmQuarkToString(theatt),(int)*the_data_type,n_items);
#endif                
			if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0) {
				EndDefineModeIf(rec, cdfid);
				CloseOrNot(rec,cdfid,0);
			}
			if(ret != -1 ) {
				stepal = stepvl->var_inq->att_list;
				if(stepal == NULL) {
					stepvl->var_inq->att_list= (NetCdfAttInqRecList*)NclMalloc((unsigned)
						sizeof(NetCdfAttInqRecList));
					stepvl->var_inq->att_list->att_inq = (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					stepvl->var_inq->att_list->next = NULL;
					stepvl->var_inq->att_list->att_inq->att_num = 0;
					stepvl->var_inq->att_list->att_inq->virtual = 0;
					stepvl->var_inq->att_list->att_inq->name = theatt;
					stepvl->var_inq->att_list->att_inq->data_type = *the_data_type;
					stepvl->var_inq->att_list->att_inq->len = n_items;
					NetCacheAttValue(stepvl->var_inq->att_list->att_inq,values);
					stepvl->var_inq->natts = 1;
				} else {	
					i = 0;
					while(stepal->next != NULL) {
						stepal = stepal->next; 
						i++;
					}
					stepal->next = (NetCdfAttInqRecList*)NclMalloc((unsigned)sizeof(NetCdfAttInqRecList));
					stepal->next->att_inq = (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					stepal->next->att_inq->att_num = i;
					stepal->next->att_inq->virtual = 0;
					stepal->next->att_inq->name = theatt;
					stepal->next->att_inq->data_type = *the_data_type;
					stepal->next->att_inq->len = n_items;
					stepal->next->next = NULL;
					NetCacheAttValue(stepal->next->att_inq,values);
					stepvl->var_inq->natts++ ;
				}
				NclFree(the_data_type);
				return(NhlNOERROR);
			} 
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetSetOption
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
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;

	if (option ==  NrmStringToQuark("prefill")) {
		rec->options[NC_PREFILL_OPT].values = (void*) *(int*)values;
	}
	else if (option == NrmStringToQuark("definemode")) {
		rec->options[NC_DEFINE_MODE_OPT].values = (void*) *(int*)values;
		if ((int)(rec->options[NC_DEFINE_MODE_OPT].values) == 0 && rec->open && rec->define_mode == 1) {
			EndDefineModeIf(rec, rec->cdfid);
			CloseOrNot(rec,rec->cdfid,0);
		}
	}
	else if (option == NrmStringToQuark("headerreservespace")) {
		rec->options[NC_HEADER_SPACE_OPT].values = (void*) *(int*)values;
		if ((int)(rec->options[NC_HEADER_SPACE_OPT].values) > 0) {
			rec->header_reserve_space = (int) rec->options[NC_HEADER_SPACE_OPT].values;
		}
		else if ((int)(rec->options[NC_HEADER_SPACE_OPT].values) < 0) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "NetSetOption: option (%s) value cannot be negative",NrmQuarkToString(option));
			return(NhlWARNING);
		}
	}
	else if (option == NrmStringToQuark("suppressclose")) {
		rec->options[NC_SUPPRESS_CLOSE_OPT].values = (void*) *(int*)values;
		if ((int)(rec->options[NC_SUPPRESS_CLOSE_OPT].values) == 0 && rec->open) {
			CloseOrNot(rec,rec->cdfid,1);
		}
	}
	else if (option == NrmStringToQuark("format")) {
		rec->options[NC_FORMAT_OPT].values = (void*) *(NrmQuark*)values;
		if (rec->file_path_q != NrmNULLQUARK) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "NetSetOption: option (%s) can only be set prior to creating file",NrmQuarkToString(option));
			return(NhlWARNING);
		}
	}
	else if (option == NrmStringToQuark("missingtofillvalue")) {
		rec->options[NC_MISSING_TO_FILL_VALUE_OPT].values = (void*) *(int*)values;
	}
#ifdef USE_NETCDF4_FEATURES
	else if (option == NrmStringToQuark("compressionlevel")) {
		if (*(int*)values < -1 || *(int*)values > 9) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "NetSetOption: option (%s) value cannot be less than -1 or greater than 9",NrmQuarkToString(option));
			return(NhlWARNING);
		}
		rec->options[NC_COMPRESSION_LEVEL_OPT].values = (void*) *(int*)values;
	}
	else if (option == NrmStringToQuark("usecache")) {
		rec->options[NC_USE_CACHE_OPT].values = (void*) *(int*)values;
	}
	else if (option == NrmStringToQuark("cachesize")) {
		if (*(int*)values < 1) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "NetSetOption: option (%s) value cannot be less than 1",NrmQuarkToString(option));
			return(NhlWARNING);
		}
		rec->options[NC_CACHE_SIZE_OPT].values = (void*) *(int*)values;
	}
	else if (option == NrmStringToQuark("cachenelems")) {
		if (*(int*)values < 3) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "NetSetOption: option (%s) value cannot be less than 3",NrmQuarkToString(option));
			return(NhlWARNING);
		}
                else
		{
			unsigned int *iv = (unsigned int *)values;
			*iv = _closest_prime(*iv);
			rec->options[NC_CACHE_NELEMS_OPT].values = (void*) *(int*)iv;
		}
	}
	else if (option == NrmStringToQuark("cachepreemption")) {
		float *fv = (float *)values;
		rec->options[NC_CACHE_PREEMPTION_OPT].values = (void*) fv;
	}
#endif
	
	return NhlNOERROR;
}

NclFormatFunctionRec NetCdfRec = {
/* NclInitializeFileRecFunc initialize_file_rec */      NetInitializeFileRec,
/* NclCreateFileFunc	   create_file; */		NetCreateFile,
/* NclOpenFileFunc         open_file; */		NetOpenFile,
/* NclFreeFileRecFunc      free_file_rec; */		NetFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		NetGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		NetGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		NetGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		NetGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		NetGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		NetGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	NetGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		NetGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		NetGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		NetReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			NetReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			NetReadAtt,
/* NclReadVarAttFunc       read_var_att; */		NetReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NetWriteCoord,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NetWriteVar,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NetWriteAtt,
/* NclWriteVarAttFunc      write_var_att; */		NetWriteVarAtt,
/* NclAddDimFunc           add_dim; */			NetAddDim,
/* NclAddChunkDimFunc      add_chunk_dim; */		NetAddChunkDim,
/* NclRenameDimFunc        rename_dim; */		NetRenameDim,
/* NclAddVarFunc           add_var; */			NetAddVar,
/* NclAddVarChunkFunc      add_var_chunk; */		NetAddVarChunk,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */	NetAddVarChunkCache,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ NetSetVarCompressLevel,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NetAddAtt,
/* NclAddVarAttFunc        add_var_att; */		NetAddVarAtt,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	NetMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	NetMapFromNcl,
/* NclDelAttFunc           del_att; */			NetDelAtt,
/* NclDelVarAttFunc        del_var_att; */		NetDelVarAtt,
#include "NclGrpFuncs.null"
/* NclSetOptionFunc        set_option;  */              NetSetOption
};
NclFormatFunctionRecPtr NetCdfAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&NetCdfRec);
}

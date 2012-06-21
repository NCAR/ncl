
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <math.h>
#include "defs.h"
#include "Symbol.h"
#include "NclVar.h"
#include "NclFile.h"
#include "NclGroup.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclMultiDValData.h"
#include "NclAtt.h"
#include "AttSupport.h"
#include "NclType.h"
#include "TypeSupport.h"
#include "FileSupport.h"
#include "NclMdInc.h"
#include "NclCoordVar.h"
#include "NclCallBacksI.h"

extern int grib_version;

#define NCLFILE_INC -1
#define NCLFILE_DEC -2
#define NCLFILE_VEC 0

NclQuark FileGetDimName(
#if	NhlNeedProto
NclFile /* thefile */,
int /*num*/
#endif
);

static NclObjTypes FileVarRepValue(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */
#endif
);

static int FileIsVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static int FileIsGroup(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static NhlErrorTypes FileWriteVarVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*lhs_var*/,
struct _NclSelectionRecord * /* lhs_sel_ptr */,
struct _NclVarRec* /* rhs_var*/,
struct _NclSelectionRecord * /* rhs_sel_ptr */
#endif
);

static NhlErrorTypes FileWriteVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

static struct _NclVarRec *FileReadVar(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

NclGroup *FileReadGroup(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* group_name */
#endif
);

static struct _NclMultiDValDataRec * FileReadVarValue(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileIsAtt(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static struct _NclMultiDValDataRec* FileReadAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

static NhlErrorTypes FileWriteAtt(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileIsVarAtt(
#if	NhlNeedProto
NclFile /*file*/,
NclQuark /* var */,
NclQuark /* attname*/
#endif
);

static struct _NclMultiDValDataRec *FileReadVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

static NhlErrorTypes FileWriteVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

static int FileIsDim(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* dimname */
#endif
);
static int FileVarIsDim(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* var */,
NclQuark /* dimname */
#endif
);

static struct _NclMultiDValDataRec *FileVarReadDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

static NhlErrorTypes FileVarWriteDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);
static struct _NclMultiDValDataRec *FileReadDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

static NhlErrorTypes FileWriteDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

static int FileIsCoord(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*coord_name */
#endif
);

static struct _NclVarRec *FileReadCoord(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

static NhlErrorTypes FileWriteCoord(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

/*
 * Updates the dimension info
 */

NhlErrorTypes UpdateDims 
#if	NhlNeedProto
(
	NclFile  thefile
)
#else
(thefile)
NclFile thefile;
#endif
{
	NclQuark *name_list;
	int n_names;
	int i;
	int index;

	name_list = (*thefile->file.format_funcs->get_dim_names)(thefile->file.private_rec,&n_names);
	thefile->file.n_file_dims = n_names;
	for(i = 0; i < n_names; i++){
		if (thefile->file.file_dim_info[i])
			NclFree(thefile->file.file_dim_info[i]);
		thefile->file.file_dim_info[i] = (thefile->file.format_funcs->get_dim_info)
			(thefile->file.private_rec,name_list[i]);
		index = FileIsVar(thefile,name_list[i]);
		if(index > -1 && thefile->file.var_info[index]->num_dimensions == 1) {
			thefile->file.coord_vars[i] = thefile->file.var_info[index];
		}
	}
	NclFree((void*)name_list);
	return NhlNOERROR;
}

/*
 * Updates the coord info
 */

static NhlErrorTypes UpdateCoordInfo 
#if	NhlNeedProto
(
	NclFile  thefile,
	NrmQuark varname
)
#else
(thefile)
NclFile thefile;
#endif
{
	int dimid;
	int index;

	dimid = FileIsDim(thefile,varname);
	if (dimid >  -1) {
		index = FileIsVar(thefile,varname);
		if(index > -1 && thefile->file.var_info[index]->num_dimensions == 1) {
			thefile->file.coord_vars[dimid] = thefile->file.var_info[index];
		}
	}
	return NhlNOERROR;
}

static void AdjustForScalarDim
#if	NhlNeedProto
(NclFile thefile)
#else
(thefile)
NclFile thefile;
#endif
{
	int i, j;

	/* since the scalar dim is always first, all the other dims and coord vars need to shift down one element */

	for (i = thefile->file.n_file_dims; i > 0; i--) {
		thefile->file.file_dim_info[i] = thefile->file.file_dim_info[i-1];
		thefile->file.coord_vars[i] = thefile->file.coord_vars[i-1];
	}

	/* also all existing var dim numbers need to be incremented by 1 */
	/* note that the coord_vars field is just a pointer to the matching var_info field, so don't adjust them separately */ 

	for (i = 0; i < thefile->file.n_vars; i++) {
		for (j = 0; j < thefile->file.var_info[i]->num_dimensions; j++) {
			thefile->file.var_info[i]->file_dim_num[j]++;
		}
	}
	thefile->file.file_dim_info[0] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,NrmStringToQuark("ncl_scalar"));
	thefile->file.coord_vars[0] = NULL; /* no coord var for a scalar obviously */
	thefile->file.n_file_dims++;
}



static NhlErrorTypes FileAddDim
#if  NhlNeedProto
(NclFile thefile, NclQuark dimname, ng_size_t dimsize, int is_unlimited)
#else
(thefile, dimname, dimsize, is_unlimited)
NclFile thefile;
NclQuark dimname;
ng_size_t dimsize;
int is_unlimited;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	
	if((thefile->file.wr_status <= 0)&&(thefile->file.format_funcs->add_dim != NULL)) {
		if (dimname == NrmStringToQuark("ncl_scalar")) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAddDim:\"ncl_scalar\" is a reserved file dimension name in NCL; it cannot be defined by the user");
			return(NhlWARNING);
		}
		if((FileIsDim(thefile,dimname)) == -1) {
			ret = (*thefile->file.format_funcs->add_dim)(
				thefile->file.private_rec,
				dimname,
				dimsize,
				is_unlimited);
			if(ret < NhlWARNING) 
			{
				return(ret);
			}
			if(thefile->file.n_file_dims >= thefile->file.max_file_dims)
			{
				_NclReallocFilePart(&(thefile->file), -1, -1, thefile->file.n_file_dims, -1);
			}

			thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,dimname);
			thefile->file.n_file_dims++;
			return(NhlNOERROR);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAddDim: Dimension %s is already defined",NrmQuarkToString(dimname));
			return(NhlWARNING);
		}
	} else {
		fprintf(stdout, "file: %s, line: %d\n", __FILE__, __LINE__);
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileAddChunkDim
#if  NhlNeedProto
(NclFile thefile, NclQuark dimname, ng_size_t dimsize, int is_unlimited)
#else
(thefile, dimname, dimsize, is_unlimited)
NclFile thefile;
NclQuark dimname;
ng_size_t dimsize;
int is_unlimited;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	
	if(thefile->file.wr_status <= 0) {
		if(thefile->file.format_funcs->add_chunk_dim == NULL) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"FileAddChunkDim: file.format_funcs->add_chunk_dim is NOT defined."));
			return(NhlWARNING);
		}

		if (dimname == NrmStringToQuark("ncl_scalar")) {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"FileAddChunkDim:\"ncl_scalar\" is a reserved file dimension name in NCL; it cannot be defined by the user"));
			return(NhlWARNING);
		}
		if((FileIsDim(thefile,dimname)) > -1) {
			ret = (*thefile->file.format_funcs->add_chunk_dim)(
				thefile->file.private_rec,
				dimname,
				dimsize,
				is_unlimited);
			if(ret < NhlWARNING) 
			{
				return(ret);
			}
			return(NhlNOERROR);
		} else {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"FileAddChunkDim: Dimension %s is not defined",NrmQuarkToString(dimname)));
			return(NhlWARNING);
		}
	} else {
		fprintf(stdout, "file: %s, line: %d\n", __FILE__, __LINE__);
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"FileAddChunkDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname)));
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileAddVar
#if	NhlNeedProto
(NclFile thefile, NclQuark varname, NclQuark type, int n_dims, NclQuark *dimnames)
#else
(thefile, varname, type, n_dims, dimnames)
NclFile thefile;
NclQuark varname;
NclQuark type;
int n_dims;
NclQuark *dimnames;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	int i;
	NclTypeClass typec;
	int dindex;
	int add_scalar_dim = 0;
	
	if((thefile->file.wr_status <= 0)&&(thefile->file.format_funcs->add_var != NULL)) {
		if((FileIsVar(thefile,varname)) == -1) {
			for(i = 0; i < n_dims; i++) {
				dindex = FileIsDim(thefile,dimnames[i]);
				if(dindex == -1) {
					if (n_dims == 1 && dimnames[0] == NrmStringToQuark("ncl_scalar")) {
						add_scalar_dim = 1;
						dim_sizes[i] = 1;
					}
					else {
						fprintf(stdout, "FileAddVar, in file: %s, line: %d\n", __FILE__, __LINE__);
						NhlPError(NhlFATAL,NhlEUNKNOWN,"FileAddVar: Dimension (%s) is not currently defined, can't add variable",NrmQuarkToString(dimnames[i]));
						return(NhlFATAL);
					}
				} else {
					dim_sizes[i] = thefile->file.file_dim_info[dindex]->dim_size;
				}
					
			}
			typec = _NclNameToTypeClass(type); 
			if(typec != NULL) {
				ret = (*thefile->file.format_funcs->add_var)(
					thefile->file.private_rec,
					varname,
					typec->type_class.data_type,	
					n_dims,
					dimnames,
					dim_sizes
					);
				if(ret == NhlFATAL) {
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,"FileAddVar: an error occurred while adding a variable to a file, check to make sure data type is supported by the output format"));
				}
			} else {
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"FileAddVar Incorrect type specified, can't add variable (%s)",NrmQuarkToString(varname)));
				ret = NhlFATAL;
			}
			if(ret < NhlWARNING) 
				return(ret);
			if (add_scalar_dim) {
				AdjustForScalarDim(thefile);
			}

			/*
			*fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
			*fprintf(stderr, "\tthefile->file.n_vars = %d, thefile->file.max_vars = %d\n",
			*		thefile->file.n_vars, thefile->file.max_vars);
			*/

			if(thefile->file.n_vars >= thefile->file.max_vars)
			{
				_NclReallocFilePart(&(thefile->file), -1, thefile->file.n_vars, -1, -1);
			}

			thefile->file.var_info[thefile->file.n_vars] = (*thefile->file.format_funcs->get_var_info)(thefile->file.private_rec,varname);
			thefile->file.var_att_info[thefile->file.n_vars] = NULL;
			thefile->file.var_att_ids[thefile->file.n_vars] = -1;
			
			thefile->file.n_vars++;
			UpdateCoordInfo(thefile,varname); 
			return(NhlNOERROR);
		} else {
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,"FileAddVar: Variable %s is already defined, can not redefine",NrmQuarkToString(varname)));
			return(NhlWARNING);
		}
	} else {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"FileAddVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname)));
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileAddVarChunk
#if	NhlNeedProto
(NclFile thefile, NclQuark varname, int n_dims, ng_size_t *dims)
#else
(thefile, varname, n_dims, dims)
NclFile thefile;
NclQuark varname;
int n_dims;
ng_size_t *dims;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	
	if(thefile->file.wr_status <= 0)
	{
		if((FileIsVar(thefile,varname)) > -1)
		{
			if(thefile->file.format_funcs->add_var_chunk != NULL)
			{
				ret = (*thefile->file.format_funcs->add_var_chunk)(
					thefile->file.private_rec,
					varname, n_dims, dims);
				if(ret == NhlFATAL)
				{
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						   "FileAddVarChunk: an error occurred while adding chunk to variable"));
				}
			}
			else
			{
				ret = NhlWARNING;
				NHLPERROR((NhlWARNING,NhlEUNKNOWN,
					   "FileAddVarChunk: add_var_chunk is not defined."));
			}
			return(ret);
		}
		else
		{
			fprintf(stdout, "FileAddVarChunk, in file: %s, line: %d\n", __FILE__, __LINE__);
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"FileAddVarChunk: Variable %s is not defined, can not define chunk",NrmQuarkToString(varname));
			return(NhlWARNING);
		}
	}
	else
	{
		fprintf(stdout, "FileAddVarChunk, in file: %s, line: %d\n", __FILE__, __LINE__);
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"FileAddVarChunk: file (%s) was opened for reading only, can not write",
			NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileAddVarChunkCache
#if	NhlNeedProto
(NclFile thefile, NclQuark varname, ng_size_t cache_size, ng_size_t cache_nelems, float cache_preemption)
#else
(thefile, varname, cache_size, cache_nelems, cache_preemption)
NclFile thefile;
NclQuark varname;
ng_size_t cache_size;
ng_size_t cache_nelems;
float  cache_preemption;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	
	if(thefile->file.wr_status <= 0)
	{
		if((FileIsVar(thefile,varname)) > -1)
		{
			if(thefile->file.format_funcs->add_var_chunk_cache != NULL)
			{
				ret = (*thefile->file.format_funcs->add_var_chunk_cache)(
					thefile->file.private_rec,
					varname, cache_size, cache_nelems, cache_preemption);
				if(ret == NhlFATAL)
				{
					fprintf(stdout, "FileAddVarChunkCache, in file: %s, line: %d\n", __FILE__, __LINE__);
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						"FileAddVarChunkCache: an error occurred while adding chunk to variable");
				}
			}
			else
			{
				ret = NhlWARNING;
				fprintf(stdout, "FileAddVarChunkCache, in file: %s, line: %d\n", __FILE__, __LINE__);
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"FileAddVarChunkCache: add_var_chunk_cache is not defined.");
			}
			return(ret);
		}
		else
		{
			fprintf(stdout, "FileAddVarChunkCache, in file: %s, line: %d\n", __FILE__, __LINE__);
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"FileAddVarChunkCache: Variable %s is not defined, can not define chunk",NrmQuarkToString(varname));
			return(NhlWARNING);
		}
	}
	else
	{
		fprintf(stdout, "FileAddVarChunkCache, in file: %s, line: %d\n", __FILE__, __LINE__);
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"FileAddVarChunkCache: file (%s) was opened for reading only, can not write",
			NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileSetVarCompressLevel
#if	NhlNeedProto
(NclFile thefile, NclQuark varname, int compress_level)
#else
(thefile, varname, compress_level)
NclFile thefile;
NclQuark varname;
int compress_level;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	
	if(thefile->file.wr_status <= 0)
	{
		if((FileIsVar(thefile,varname)) > -1)
		{
			if(thefile->file.format_funcs->set_var_compress_level != NULL)
			{
				ret = (*thefile->file.format_funcs->set_var_compress_level)(
					thefile->file.private_rec,
					varname, compress_level);
				if(ret == NhlFATAL)
				{
					fprintf(stdout, "FileSetVarCompressLevel, in file: %s, line: %d\n", __FILE__, __LINE__);
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						"FileSetVarCompressLevel: an error occurred while adding chunk to variable");
				}
			}
			else
			{
				ret = NhlWARNING;
				fprintf(stdout, "FileSetVarCompressLevel, in file: %s, line: %d\n", __FILE__, __LINE__);
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					"FileSetVarCompressLevel: set_var_compress_level is not defined.");
			}
			return(ret);
		}
		else
		{
			fprintf(stdout, "FileSetVarCompressLevel, in file: %s, line: %d\n", __FILE__, __LINE__);
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				"FileSetVarCompressLevel: Variable %s is not defined, can not define chunk",NrmQuarkToString(varname));
			return(NhlWARNING);
		}
	}
	else
	{
		fprintf(stdout, "FileSetVarCompressLevel, in file: %s, line: %d\n", __FILE__, __LINE__);
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			"FileSetVarCompressLevel: file (%s) was opened for reading only, can not write",
			NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

void FileAttIsBeingDestroyedNotify
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata, udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
        NclAtt theattobj;
	NclFile thefile;
	NclQuark attname;
	NclQuark thevar;
	int index;
	NclFileAttInfoList *thelist;
	NclMultiDValData tmp_md;
	void *val;
        ng_size_t  ne;

	theattobj = (NclAtt)_NclGetObj(((FileCallBackRec*)udata.ptrval)->theattid);
	thefile = (NclFile)_NclGetObj(((FileCallBackRec*)udata.ptrval)->thefileid);
	thevar = ((FileCallBackRec*)udata.ptrval)->thevar;
	attname = cbdata.lngval;

	if((theattobj == NULL) ||(thefile == NULL)) return;

	if(thevar != -1) {
		if(_NclFileDeleteVarAtt(thefile,thevar,attname) < NhlNOERROR) {

/*
* The premis here is that the above only fails when deleting attributes is not
* supported or when the permissions were wrong. Therefore the following is
* valid
*/
			index = _NclFileIsVar(thefile,thevar);
			thelist = thefile->file.var_att_info[index] ;
			while(thelist != NULL) {
				if(thelist->the_att->att_name_quark ==  attname) {
					if(thefile->file.format_funcs->read_var_att != NULL) {
                                		val = NclMalloc(_NclSizeOf(thelist->the_att->data_type)* thelist->the_att->num_elements );
                                		(void)(*thefile->file.format_funcs->read_var_att)(
                                        		thefile->file.private_rec,
                                        		thevar,
                                        		thelist->the_att->att_name_quark,
                                        		val
                                        		);
                                        ne = thelist->the_att->num_elements;
                                		tmp_md = _NclCreateMultiDVal(
                                                		NULL,
                                                		NULL,
                                                		Ncl_MultiDValData,
                                                		0,
                                                		val,
                                                		NULL,
                                                		1,
                                                		&ne,
                                                		TEMPORARY,
                                                		NULL,
                                                		_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thelist->the_att->data_type))
                                                	);
                                			if(tmp_md != NULL) {
                                        			_NclAddAtt(thefile->file.var_att_ids[index],NrmQuarkToString(thelist->the_att->att_name_quark),tmp_md,NULL);
                                			}
					}
					return;
				} else {
					thelist = thelist->next;
				}
			}
		}
	} else {
		if(_NclFileDeleteAtt(thefile,attname)<NhlNOERROR) {
			index = _NclFileIsAtt(thefile,attname);
                	if(thefile->file.format_funcs->read_att != NULL) {
                                val = NclMalloc(_NclSizeOf(thefile->file.file_atts[index]->data_type)* thefile->file.file_atts[index]->num_elements );
                                (void)(*thefile->file.format_funcs->read_att)(
                                        thefile->file.private_rec,
                                        thefile->file.file_atts[index]->att_name_quark,
                                        val
                                        );
                                ne = thefile->file.file_atts[index]->num_elements;
                                tmp_md = _NclCreateMultiDVal(
                                                NULL,
                                                NULL,
                                                Ncl_MultiDValData,
                                                0,
                                                val,
                                                NULL,
                                                1,
                                                &ne,
                                                TEMPORARY,
                                                NULL,
                                                _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.file_atts[index]->data_type)));
                                if(tmp_md != NULL) {
                                        _NclAddAtt(thefile->file.file_atts_id,NrmQuarkToString(thefile->file.file_atts[index]->att_name_quark),tmp_md,NULL);
				 }
			}
		}
	}
}

void LoadVarAtts
#if     NhlNeedProto
(NclFile thefile, NclQuark var)
#else
(NclFile thefile, NclQuark var)
NclFile thefile;
NclQuark var;
#endif
{
	int index;
        NclFileAttInfoList *step;
        int att_id = -1;
        void *val;
        NclMultiDValData tmp_md;
        NhlArgVal udata;
        ng_size_t ne;	

	index = FileIsVar(thefile,var);
        if(index > -1) {
		if(thefile->file.var_att_ids[index] == -1) {
			step = thefile->file.var_att_info[index];
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			while(step != NULL) {
				if (step->the_att->data_type == NCL_none)
					val = NULL;
				else {
					val = NclMalloc(_NclSizeOf(step->the_att->data_type)* step->the_att->num_elements );
					(void)(*thefile->file.format_funcs->read_var_att)(
						thefile->file.private_rec,
						var,
						step->the_att->att_name_quark,
						val
						);
				}
				ne = step->the_att->num_elements;
				tmp_md = _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						val,
						NULL,
						1,
						&ne,
						TEMPORARY,
						NULL,
						_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(step->the_att->data_type))
						);
				if(tmp_md != NULL) {
					_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_md,NULL);
				}
				step = step->next;
			}
			udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
			((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
			((FileCallBackRec*)udata.ptrval)->theattid = att_id;
			((FileCallBackRec*)udata.ptrval)->thevar = var;
			thefile->file.var_att_cb[index] = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
			thefile->file.var_att_udata[index] = (FileCallBackRec*)udata.ptrval;
			thefile->file.var_att_ids[index] = att_id;
			return;
		}
	}
}

NhlErrorTypes _NclFilePrintSummary(NclObj self, FILE *fp)
{
	NclFile thefile = (NclFile)self;
	int i,j;
	NclFileAttInfoList* step;
	int ret = 0;
	NclMultiDValData tmp_md;
	NhlErrorTypes ret1 = NhlNOERROR;
	char *tmp_str;

	ret = nclfprintf(fp,"File path:\t%s\n",NrmQuarkToString(thefile->file.fpath));
	if(ret < 0) {	
		return(NhlWARNING);
	}
	
	ret = nclfprintf(fp,"Number of global attributes:\t %d\n",thefile->file.n_file_atts);
        ret = nclfprintf(fp,"Number of dimensions:\t %d\n",thefile->file.n_file_dims);
	if (thefile->file.n_grps > 0) {
		ret = nclfprintf(fp,"Number of groups:\t %d\n",thefile->file.n_grps);
	}
        ret = nclfprintf(fp,"Number of variables:\t %d\n",thefile->file.n_vars);
	
	/*
	*ret = nclfprintf(fp,"Number of attributes capacity:\t %d\n",thefile->file.max_file_atts);
        *ret = nclfprintf(fp,"Number of dimensions capacity:\t %d\n",thefile->file.max_file_dims);
	*ret = nclfprintf(fp,"Number of groups     capacity:\t %d\n",thefile->file.max_grps);
        *ret = nclfprintf(fp,"Number of variables  capacity:\t %d\n",thefile->file.max_vars);
	*/

	return ret;
}

NhlErrorTypes FilePrint
#if	NhlNeedProto
(NclObj self, FILE    *fp)
#else
(self, fp)
NclObj self;
FILE    *fp;
#endif
{
	NclFile thefile = (NclFile)self;
	int i,j;
	NclFileAttInfoList* step;
	int ret = 0;
	NclMultiDValData tmp_md;
	NhlErrorTypes ret1 = NhlNOERROR;
	char *tmp_str;
	

	ret = nclfprintf(fp,"filename:\t%s\n",NrmQuarkToString(thefile->file.fname));
	if(ret < 0) {	
		return(NhlWARNING);
	}
	ret = nclfprintf(fp,"path:\t%s\n",NrmQuarkToString(thefile->file.fpath));
	if(ret < 0) {	
		return(NhlWARNING);
	}
	ret = nclfprintf(fp,"   file global attributes:\n");
	if(ret < 0) {	
		return(NhlWARNING);
	}
	for(i = 0; i < thefile->file.n_file_atts; i++) {
		if(thefile->file.file_atts[i] != NULL) {
			ret = nclfprintf(fp,"      %s : ",NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark));
			if(ret < 0) {	
				return(NhlWARNING);
			}
			if(thefile->file.file_atts[i]->num_elements == 1) {
				tmp_md = _NclFileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
				ret1 = _Nclprint(tmp_md->multidval.type,fp,tmp_md->multidval.val);
				if(ret < NhlINFO) {	
					return(NhlWARNING);
				}
				ret = nclfprintf(fp,"\n");
				if(ret < 0) {	
					return(NhlWARNING);
				}
			} else if (thefile->file.file_atts[i]->num_elements > 1 &&
				   thefile->file.file_atts[i]->num_elements < 11) {
				tmp_md = _NclFileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
				ret = nclfprintf(fp,"( ");
				if(ret < 0) {
					return(NhlWARNING);
				}
				for (j = 0; j < tmp_md->multidval.totalelements; j++) {
					char *val = (char*)tmp_md->multidval.val + 
						j * tmp_md->multidval.type->type_class.size; 
					ret1 = _Nclprint(tmp_md->multidval.type,fp,val);
					if(ret1 < NhlINFO) {
						return(ret1);
					}
					if (j < tmp_md->multidval.totalelements - 1) {
						ret = nclfprintf(fp,", ");
						if(ret < 0) {
							return(NhlWARNING);
						}
					}
				}
				ret = nclfprintf(fp," )\n");
				if(ret < 0) {
					return(NhlWARNING);
				}
			}
			else {
				ret = nclfprintf(fp,"<ARRAY of %d elements>\n",thefile->file.file_atts[i]->num_elements);
				if(ret < 0) {	
					return(NhlWARNING);
				}
			}
		}
	}
	ret = nclfprintf(fp,"   dimensions:\n");
	if(ret < 0) {	
		return(NhlWARNING);
	}
	for(i = 0; i< thefile->file.n_file_dims; i++) {
		if(thefile->file.file_dim_info[i]->is_unlimited) {
		  	ret = nclfprintf(fp,"      %s = %ld  // unlimited\n",NrmQuarkToString(thefile->file.file_dim_info[i]->dim_name_quark), thefile->file.file_dim_info[i]->dim_size);
		} else {
		 	ret = nclfprintf(fp,"      %s = %ld\n",NrmQuarkToString(thefile->file.file_dim_info[i]->dim_name_quark),thefile->file.file_dim_info[i]->dim_size);
		}
		if(ret < 0) {	
			return(NhlWARNING);
		}
	}
	ret = nclfprintf(fp,"   variables:\n");
	if(ret < 0) {	
		return(NhlWARNING);
	}
      /*
       *fprintf(stdout, "\n\n\nhit FilePrint vars. file: %s, line: %d\n", __FILE__, __LINE__);
       */
	for(i = 0; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i] != NULL) {
			tmp_str = NrmQuarkToString(thefile->file.var_info[i]->var_name_quark);

			if(0 == strcmp("group", _NclBasicDataTypeToName(thefile->file.var_info[i]->data_type)))
			{
				ret = nclfprintf(fp,"      %s <%s>\n\n",
					_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type),
					NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
			}
			else if(0 == strcmp("compound", _NclBasicDataTypeToName(thefile->file.var_info[i]->data_type)))
			{
				ret = nclfprintf(fp,"      %s <%s>",
					_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type),
					NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));

				ret = nclfprintf(fp,"\t(%s",NrmQuarkToString(thefile->file.var_info[i]->component_name[0]));
				for(j=1; j<thefile->file.var_info[i]->num_compounds; j++)
					ret = nclfprintf(fp,", %s",NrmQuarkToString(thefile->file.var_info[i]->component_name[j]));
				ret = nclfprintf(fp,") (%s)\n\n",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[0])));
				if(ret < 0) {	
					return(NhlWARNING);
				}
				continue;
			}
			else
			{
				if(tmp_str[0] == '/')
					continue;

				ret = nclfprintf(fp,"      %s %s ( ",_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type), tmp_str);
				if(ret < 0) {	
					return(NhlWARNING);
				}

				for(j=0; j< thefile->file.var_info[i]->num_dimensions - 1; j++) {
					ret = nclfprintf(fp,"%s, ",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j])));
					if(ret < 0) {	
						return(NhlWARNING);
					}
				}

				ret = nclfprintf(fp,"%s )\n",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[thefile->file.var_info[i]->num_dimensions - 1])));
				if(ret < 0) {	
					return(NhlWARNING);
				}
			}

			step = thefile->file.var_att_info[i];
			while(step != NULL) {
				ret = nclfprintf(fp,"         %s :\t", NrmQuarkToString(step->the_att->att_name_quark));
				if(ret < 0) {	
					return(NhlWARNING);
				}
				if(step->the_att->num_elements == 1) {
					tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
					ret1 = _Nclprint(tmp_md->multidval.type,fp,tmp_md->multidval.val);
					if(ret1 < NhlINFO) {	
						return(NhlWARNING);
					}
					ret = nclfprintf(fp,"\n");
					if(ret < 0) {	
						return(NhlWARNING);
					}
				} else if (step->the_att->num_elements > 1 &&
					   step->the_att->num_elements < 11) {
					tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
					ret = nclfprintf(fp,"( ");
					if(ret < 0) {
						return(NhlWARNING);
					}
					for (j = 0; j < tmp_md->multidval.totalelements; j++) {
						char *val = (char*)tmp_md->multidval.val + 
							j * tmp_md->multidval.type->type_class.size; 
						ret1 = _Nclprint(tmp_md->multidval.type,fp,val);
						if(ret1 < NhlINFO) {
							return(ret1);
						}
						if (j < tmp_md->multidval.totalelements - 1) {
							ret = nclfprintf(fp,", ");
							if(ret < 0) {
								return(NhlWARNING);
							}
						}
					}
					ret = nclfprintf(fp," )\n");
					if(ret < 0) {
						return(NhlWARNING);
					}
				} else {
					ret = nclfprintf(fp,"<ARRAY of %d elements>\n",step->the_att->num_elements);
					if(ret < 0) {	
						return(NhlWARNING);
					}
				}
				step = step->next;
			}
			ret = nclfprintf(fp,"\n");
			if(ret < 0) {	
				return(NhlWARNING);
			}
		}
	}
	
	return(NhlNOERROR);;
}

void FileDestroy
#if	NhlNeedProto
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	NclFile thefile = (NclFile) self;
	int i;
	NclFileAttInfoList *step,*tmp;
	NclRefList *p, *pt;

	_NclUnRegisterObj((NclObj)self);
	if(thefile->file.format_funcs->free_file_rec != NULL) {
		if(thefile->file.private_rec != NULL)
			(*thefile->file.format_funcs->free_file_rec)(thefile->file.private_rec);
	}
	for(i =0 ; i < thefile->file.n_grps; i++) {
		if(NULL != thefile->file.grp_info[i])
			NclFree(thefile->file.grp_info[i]);
		if(thefile->file.grp_att_cb[i] != NULL) {
			NclFree(thefile->file.grp_att_udata[i]);
			_NhlCBDelete(thefile->file.grp_att_cb[i]);
		}
		if(thefile->file.grp_att_ids[i] != -1) {
			_NclDelParent(_NclGetObj(thefile->file.grp_att_ids[i]),self);
		}
		step = thefile->file.grp_att_info[i];	
		while(step != NULL) {
			NclFree(step->the_att);
			tmp = step;
			step = step->next;
			NclFree(tmp);
		}
	}

	if(NULL != thefile->file.grp_info)
        	NclFree(thefile->file.grp_info);
	if(NULL != thefile->file.grp_att_info)
        	NclFree(thefile->file.grp_att_info);
	if(NULL != thefile->file.grp_att_udata)
        	NclFree(thefile->file.grp_att_udata);
	if(NULL != thefile->file.grp_att_cb)
        	NclFree(thefile->file.grp_att_cb);
	if(NULL != thefile->file.grp_att_ids)
        	NclFree(thefile->file.grp_att_ids);

	for(i =0 ; i < thefile->file.n_vars; i++) {
		if(NULL != thefile->file.var_info[i])
			NclFree(thefile->file.var_info[i]);
		if(thefile->file.var_att_cb[i] != NULL) {
			NclFree(thefile->file.var_att_udata[i]);
			_NhlCBDelete(thefile->file.var_att_cb[i]);
		}
		if(thefile->file.var_att_ids[i] != -1) {
			_NclDelParent(_NclGetObj(thefile->file.var_att_ids[i]),self);
		}
		step = thefile->file.var_att_info[i];	
		while(step != NULL) {
			NclFree(step->the_att);
			tmp = step;
			step = step->next;
			NclFree(tmp);
		}
	}

	if(NULL != thefile->file.var_info)
        	NclFree(thefile->file.var_info);
	if(NULL != thefile->file.var_att_info)
        	NclFree(thefile->file.var_att_info);
	if(NULL != thefile->file.var_att_udata)
        	NclFree(thefile->file.var_att_udata);
	if(NULL != thefile->file.var_att_cb)
        	NclFree(thefile->file.var_att_cb);
	if(NULL != thefile->file.var_att_ids)
        	NclFree(thefile->file.var_att_ids);

	for(i =0 ; i < thefile->file.n_file_dims; i++) {
		if(NULL != thefile->file.file_dim_info[i])
			NclFree(thefile->file.file_dim_info[i]);
	}

	if(NULL != thefile->file.file_dim_info)
		NclFree(thefile->file.file_dim_info);

	if(thefile->file.file_atts_id != -1) {
		NclFree(thefile->file.file_att_udata);
		_NhlCBDelete(thefile->file.file_att_cb);
		_NclDelParent(_NclGetObj(thefile->file.file_atts_id),self);
	}
	for(i =0 ; i < thefile->file.n_file_atts; i++) {
		if(NULL != thefile->file.file_atts[i])
			NclFree(thefile->file.file_atts[i]);
	}

	if(NULL != thefile->file.file_atts)
		NclFree(thefile->file.file_atts);

	if(thefile->obj.cblist != NULL) {
		_NhlCBDestroy(thefile->obj.cblist);
	}
	p = thefile->obj.parents;
	while (p) {
		pt = p;
		p = p->next;
		NclFree(pt);
	}
		
	NclFree(thefile);
	return;
}

NhlErrorTypes FileAddParent
#if	NhlNeedProto
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList * tmp = NULL;

        tmp = theobj->obj.parents;
        theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
        theobj->obj.parents->next = tmp;
        theobj->obj.parents->pid = parent->obj.id;
        theobj->obj.ref_count++;
        return(NhlNOERROR);

}

NhlErrorTypes FileDelParent
#if	NhlNeedProto
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList *tmp,*tmp1;
        int found = 0;

        if(theobj->obj.parents == NULL) {
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"FileDelParent: Attempt to delete parent from empty list"));
                return(NhlFATAL);
        }

        tmp = theobj->obj.parents;
        if((tmp!=NULL)&&(tmp->pid == parent->obj.id)) {
                theobj->obj.parents = theobj->obj.parents->next;
                NclFree(tmp);
                tmp = theobj->obj.parents;
                found = 1;
                theobj->obj.ref_count--;
        }
        if((tmp == NULL)&&(found)) {
                _NclDestroyObj(theobj);
                return(NhlNOERROR);
        }
        while(tmp->next != NULL) {
                if(tmp->next->pid == parent->obj.id) {
                        found = 1;
                        tmp1 = tmp->next;
                        tmp->next = tmp->next->next;
                        NclFree(tmp1);
                        theobj->obj.ref_count--;
                        if(theobj->obj.ref_count <= 0)
                                _NclDestroyObj(theobj);
                        return(NhlNOERROR);
                } else {
                        tmp = tmp->next;
                }
        }
        return(NhlWARNING);

}
static NhlErrorTypes InitializeFileClass(
#if NhlNeedProto
void
#endif
);

static void * FileObtainCallData
#if NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
	NclFileClassInfo *tmp = NclMalloc(sizeof(NclFileClassInfo));
	NclFile file = (NclFile)obj;
	
	tmp->obj.obj_id = obj->obj.id;
	tmp->obj.obj_type = NCLFile;
	tmp->file.fname = file->file.fname;
	tmp->file.fpath = file->file.fpath;
	tmp->file.wr_status = file->file.wr_status;
	return((void*)tmp);

}

static NhlErrorTypes FileDelAtt
#if     NhlNeedProto
(NclFile thefile, NclQuark attname)
#else
(thefile, attname)
NclFile thefile;
NclQuark attname;
#endif
{
	int index;
	NhlErrorTypes ret = NhlNOERROR;
	NclFAttRec *tmpal;

	if(thefile->file.wr_status <= 0) {
		index = _NclFileIsAtt(thefile,attname);
		if(index != -1) {
			if(thefile->file.format_funcs->del_att != NULL) {
				ret = (*thefile->file.format_funcs->del_att)(thefile->file.private_rec,attname);
				if(ret < NhlNOERROR) {
					return(ret);
				}
				if(thefile->file.file_atts[index] != NULL) {
					tmpal = thefile->file.file_atts[index];
					for(;index < thefile->file.n_file_atts - 1;index++) {
						thefile->file.file_atts[index] = thefile->file.file_atts[index+1];
					}
					thefile->file.n_file_atts--;
					NclFree(tmpal);
					return(ret);
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute deletion not supported by format");
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to delete undefined attribute from file");
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileDelAtt: file (%s) is read only, can not delete attribute",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
	return(NhlFATAL);
}

static NhlErrorTypes FileDelVarAtt
#if     NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname)
#else
(thefile, var, attname)
NclFile thefile;
NclQuark var;
NclQuark attname;
#endif
{
	int index;
	int vindex;
	NhlErrorTypes ret = NhlNOERROR;
	NclFileAttInfoList  *stepal,*tmpal;

	if(thefile->file.wr_status <= 0) {
		vindex = _NclFileIsVar(thefile,var);
		index = _NclFileVarIsAtt(thefile,var,attname);
		if((index != -1)&&(vindex != -1)) {
			if(thefile->file.format_funcs->del_var_att != NULL) {
				ret = (*thefile->file.format_funcs->del_var_att)(thefile->file.private_rec,var,attname);
				if(ret < NhlNOERROR) {
					return(ret);
				}
				if(thefile->file.var_att_info[vindex] != NULL) {
					stepal = thefile->file.var_att_info[vindex];
					if(stepal->the_att->att_name_quark == attname) {
						tmpal = stepal;
						thefile->file.var_att_info[vindex] = stepal->next;
						NclFree(tmpal);
						return(NhlNOERROR);
					} else {
						while(stepal->next != NULL) {
							if(stepal->next->the_att->att_name_quark == attname) {
								tmpal = stepal->next;
								stepal->next= stepal->next->next;
								NclFree(tmpal);
							} else {
								stepal = stepal->next;
							}
						}
						return(NhlNOERROR);
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute deletion not supported by format");
				return(NhlFATAL);
			}
		} else if(vindex == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to delete attribute from undefined variable");
			return(NhlFATAL);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to delete undefined attribute from variable");
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileDelVarAtt: file (%s) is read only, can not delete attribute",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
	return(NhlFATAL);
}

	

static NhlErrorTypes FileSetFileOption
#if	NhlNeedProto
(
	NclFile  thefile,
	NclQuark format,
	NclQuark option,
	NclMultiDValData value
)
#else
(thefile,format,option,value)
NclFile thefile;
NclQuark format;
NclQuark option;
NclMultiDValData value;
#endif
{
	int i, found, idx;
	NclMultiDValData tmp_md;
	NclQuark loption;
	NclQuark *lvalue = NULL;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	
	loption = _NclGetLower(option);
	if (thefile) {
		if (thefile->file.format_funcs->set_option == NULL) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "FileSetFileOption: file does not support any options");
			return(NhlWARNING);
		}
		found = 0;
		for (i = 0; i < fcp->num_options; i++) {
			if (fcp->options[i].name != loption)
				continue;
			idx = i;
			if (thefile->file.format_funcs == _NclGetFormatFuncs(fcp->options[i].format))
			{
				found = 1;
				break;
			}
		}
		if(found) {
			i = idx;
			if (thefile->file.format_funcs != _NclGetFormatFuncs(fcp->options[i].format)) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: %s is not a recognized option for format %s",
					  NrmQuarkToString(option),NrmQuarkToString(format));
				return(NhlWARNING);
			}
			if (fcp->options[i].access == 1 && thefile->file.wr_status != 1) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: option %s is invalid unless file is opened for reading only",
					  NrmQuarkToString(option));
				return(NhlWARNING);
			}
			else if (fcp->options[i].access == 2 && thefile->file.wr_status > 0) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: option %s is invalid unless file is open for writing",
					  NrmQuarkToString(option));
				return(NhlWARNING);
			}
			else if (fcp->options[i].access == 3 && thefile->file.wr_status != -1) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: option %s is can only be set prior to file creation",
					  NrmQuarkToString(option));
				return(NhlWARNING);
			}
			if (! value) {
				/* if no value specified restore default for this file only - it's not an error */
				tmp_md = fcp->options[i].def_value;
				thefile->file.format_funcs->set_option(thefile->file.private_rec,loption,
									tmp_md->multidval.data_type,
									tmp_md->multidval.totalelements,
									tmp_md->multidval.val);
				return NhlNOERROR;
			}
			tmp_md = _NclCoerceData(value,fcp->options[i].value->multidval.type->type_class.type,NULL);
			if (tmp_md == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: invalid type for %s option value; value must be coercible to %s",
					  NrmQuarkToString(option), 
					  NrmQuarkToString(_NclObjTypeToName(fcp->options[i].value->multidval.type->type_class.type)));
				return(NhlWARNING);
			}
			if (fcp->options[i].valid_values) {
				int ok = 0;
				int j,k;
				if (fcp->options[i].value->multidval.data_type == NCL_string) {
					lvalue = NclMalloc(tmp_md->multidval.totalelements * sizeof(NclQuark));
					ok = 0;
					for (k = 0; k < tmp_md->multidval.totalelements; k++) {
						lvalue[k] = _NclGetLower(*(NclQuark*)(((char *)tmp_md->multidval.val)+ k * sizeof(NclQuark)));
						for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
							NclQuark valid_val = ((string *)fcp->options[i].valid_values->multidval.val)[j];
							if (lvalue[k] != valid_val)
								continue;
							ok = 1;
							break;
						}
					}
					if (! ok) {
						NclFree(lvalue);
						NhlPError(NhlWARNING,NhlEUNKNOWN,
							  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
						return(NhlWARNING);
					}
				}
				else {
					/* doesn't handle array valued options */
					for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
						if (memcmp(tmp_md->multidval.val,
							   (char*)fcp->options[i].valid_values->multidval.val +
							   j * tmp_md->multidval.type->type_class.size,
							   tmp_md->multidval.type->type_class.size)) {
							continue;
						}
						ok = 1;
						break;
					}
				}
				if (! ok) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
					return(NhlWARNING);
				}
			}
			if (lvalue) {
				thefile->file.format_funcs->set_option(thefile->file.private_rec,loption,
									tmp_md->multidval.data_type,
									tmp_md->multidval.totalelements,
									(void *) lvalue);
				NclFree(lvalue);
			}
			else {
				thefile->file.format_funcs->set_option(thefile->file.private_rec,loption,
									tmp_md->multidval.data_type,
									tmp_md->multidval.totalelements,
									tmp_md->multidval.val);
			}
			if (tmp_md != value)
				_NclDestroyObj((NclObj)tmp_md);
			if (fcp->options[i].post_set_option) {
				return (*fcp->options[i].post_set_option)(thefile);
			}
			return NhlNOERROR;
		}
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			  "FileSetFileOption: %s is not a recognized file option for format %s",
			  NrmQuarkToString(option),NrmQuarkToString(format)));
		return(NhlWARNING);
	}
	else if (format != NrmNULLQUARK) {
		found = 0;
		for (i = 0; i < fcp->num_options; i++) {
			if (fcp->options[i].name != loption)
				continue;
			if ((_NclGetFormatFuncs(format) &&
			     _NclGetFormatFuncs(format) == _NclGetFormatFuncs(fcp->options[i].format)) ) {
				found = 1;
				break;
			}
			else if (_NclGetLower(format) == NrmStringToQuark("bin") &&
				 fcp->options[i].format == _NclGetLower(format)) {
				found = 1;
				break;
			}
			else if (! (_NclGetFormatFuncs(format) &&
			       _NclGetFormatFuncs(format) == _NclGetFormatFuncs(fcp->options[i].format)) ) {
				if (_NclGetLower(format) == NrmStringToQuark("shp"))
				{
					fcp->options[i].format = _NclGetLower(format);
					found = 1;
					break;
				}
				else if (! (_NclGetLower(format) == NrmStringToQuark("bin") &&
					fcp->options[i].format == _NclGetLower(format)) ) {
					found = 1;
					break;
				}
			}
		}
		if (found) {
			if (! value) {
				/* if no value specified restore default - it's not an error */
				tmp_md = fcp->options[i].def_value;
				memcpy(fcp->options[i].value->multidval.val,tmp_md->multidval.val,
				       tmp_md->multidval.type->type_class.size);
				return NhlNOERROR;
			}
			tmp_md = _NclCoerceData(value,fcp->options[i].value->multidval.type->type_class.type,NULL);
			if (tmp_md == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
				    "FileSetFileOption: invalid type for %s option value; value must be coercible to %s",
					  NrmQuarkToString(option), 
					  NrmQuarkToString(_NclObjTypeToName(fcp->options[i].value->multidval.type->type_class.type)));
				return(NhlWARNING);
			}
			if (fcp->options[i].valid_values) {
				int ok = 0;
				int j,k;
				if (fcp->options[i].value->multidval.data_type == NCL_string) {
					lvalue = NclMalloc(tmp_md->multidval.totalelements * sizeof(NclQuark));
					ok = 0;
					for (k = 0; k < tmp_md->multidval.totalelements; k++) {
						lvalue[k] = _NclGetLower(*(NclQuark*)(((char *)tmp_md->multidval.val)+ k * sizeof(NclQuark)));
						for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
							NclQuark valid_val = ((string *)fcp->options[i].valid_values->multidval.val)[j];
							if (lvalue[k] != valid_val)
								continue;
							ok = 1;
							break;
						}
					}
					if (! ok) {
						NclFree(lvalue);
						NhlPError(NhlWARNING,NhlEUNKNOWN,
							  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
						return(NhlWARNING);
					}
				}
				else {
					/* doesn't handle array valued options yet -- see the string handling */
					for (j = 0; j < fcp->options[i].valid_values->multidval.totalelements; j++) {
						if (memcmp(tmp_md->multidval.val,
							   (char*)fcp->options[i].valid_values->multidval.val +
							   j * tmp_md->multidval.type->type_class.size,
							   tmp_md->multidval.type->type_class.size)) {
							continue;
						}
						ok = 1;
						break;
					}
				}
				if (! ok) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileSetFileOption: invalid value supplied for option %s",
							  NrmQuarkToString(option));
					return(NhlWARNING);
				}
			}
			if (lvalue) {
				/* store the lower-cased name */
				NclFree(fcp->options[i].value->multidval.val);
				fcp->options[i].value->multidval.val = (void *)lvalue;
				fcp->options[i].value->multidval.totalelements = tmp_md->multidval.totalelements;
			}
			else {
				/* doesn't handle array valued options yet -- see the string handling */
				memcpy(fcp->options[i].value->multidval.val,tmp_md->multidval.val,
				       tmp_md->multidval.type->type_class.size);
			}
			if (tmp_md != value)
				_NclDestroyObj((NclObj)tmp_md);
			return NhlNOERROR;
		}
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			  "FileSetFileOption: %s is not a recognized file option for format %s",
			  NrmQuarkToString(option),NrmQuarkToString(format)));
		return(NhlWARNING);
	}
	else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "FileSetFileOption: invalid file or format");
		return(NhlWARNING);
	}					    
		
	return NhlNOERROR;
}

NhlErrorTypes UpdateGridTypeAtt 
#if	NhlNeedProto
(
	NclFile  thefile
)
#else
(thefile)
NclFile thefile;
#endif
{
	NclQuark *vnames;
	int n_vnames;
	int i;
	int vindex,att_id;
	NrmQuark grid_type_att_name;
	void *val;
	NclMultiDValData tmp_md;

	if (thefile->file.file_format != _NclGRIB2)
		return NhlNOERROR;

	grid_type_att_name = NrmStringToQuark("grid_type");
	vnames = (*thefile->file.format_funcs->get_var_names)(thefile->file.private_rec,&n_vnames);
	for(i = 0; i < n_vnames; i++){
		vindex = FileIsVar(thefile,vnames[i]);
		if(thefile->file.var_att_ids[vindex] == -1) {
			LoadVarAtts(thefile,vnames[i]);
			continue;
		}
		att_id = thefile->file.var_att_ids[vindex];
		if (! _NclIsAtt(att_id,NrmQuarkToString(grid_type_att_name)))
			continue;

		tmp_md = _NclGetAtt(att_id,NrmQuarkToString(grid_type_att_name),NULL);
		(*thefile->file.format_funcs->read_var_att)(thefile->file.private_rec,vnames[i],grid_type_att_name,&val);

		*((NrmQuark*)tmp_md->multidval.val) = (NrmQuark)val;
		_NclAddAtt(att_id,NrmQuarkToString(grid_type_att_name),tmp_md,NULL);
	}
	NclFree((void*)vnames);
	return NhlNOERROR;
}


NclFileOption file_options[] = {
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },  /* NetCDF PreFill */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },  /* NetCDF define mode */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, UpdateGridTypeAtt },  /* GRIB thinned grid interpolation method */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },  /* NetCDF header reserve space */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* NetCDF suppress close option */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 3, NULL },  /* NetCDF file format option */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* Binary file read byte order */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* Binary file write byte order */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, UpdateDims },   /* GRIB initial time coordinate type */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* NetCDF missing to fill value option */
#ifdef USE_NETCDF4
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },         /* NetCDF 4 compression option level */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },         /* NetCDF 4 cache switch */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 3200000, NULL },   /* NetCDF 4 cache size */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 1009, NULL },      /* NetCDF 4 cache nelems */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0.50, NULL },      /* NetCDF 4 cache preemption */
#endif
#ifdef BuildHDF5
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 2, NULL },         /* HDF5 compression option level */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },         /* HDF5 cache switch */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 3200000, NULL },   /* HDF5 cache size */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 1009, NULL },      /* HDF5 cache nelems */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0.50, NULL },      /* HDF5 cache preemption */
#endif
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* GRIB default NCEP parameter table */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* GRIB print record info */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* GRIB single element dimensions */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL },  /* GRIB time period suffix */
	{ NrmNULLQUARK, NrmNULLQUARK, NULL, NULL, NULL, 0, NULL }   /* new file-structure */
};

NclFileClassRec nclFileClassRec = {
	{	
		"NclFileClass",
		sizeof(NclFileRec),
		(NclObjClass)&nclObjClassRec,
		0,
		(NclGenericFunction)FileDestroy,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)InitializeFileClass,
		(NclAddParentFunction)FileAddParent,
		(NclDelParentFunction)FileDelParent,
/* NclPrintSummaryFunction print_summary */ NULL,
		(NclPrintFunction) FilePrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   FileObtainCallData
	},
	{
/*NclFileVarRepValueFunc	rep_val*/		FileVarRepValue,
/*NclFileIsAFunc		is_var*/		FileIsVar,
/*NclAssignFileVarFunc		write_var*/		FileWriteVar,
/*NclAssignFileVarVarFunc	write_var_var*/		FileWriteVarVar,
/*NclGetFileVarFunc		read_var_func*/		FileReadVar,
/*NclGetFileVarValFunc		read_var_val_func*/	FileReadVarValue,
/*NclFileIsAFunc		is_att*/		FileIsAtt,
/*NclReadAttributeFunc		read_att_func*/		FileReadAtt,
/*NclWriteAttributeFunc		write_att_func*/	FileWriteAtt,
/*NclDeleteAttributeFunc	del_att_func*/		FileDelAtt,
/*NclFileVarIsAFunc		is_var_att*/		FileIsVarAtt,
/*NclReadVarAttributeFunc	read_var_att_func*/	FileReadVarAtt,
/*NclWriteVarAttributeFunc	write_var_att_func*/	FileWriteVarAtt,
/*NclDeleteVarAttributeFunc	del_var_att_func*/	FileDelVarAtt,
/*NclFileIsAFunc		is_dim*/		FileIsDim,
/*NclFileVarIsAFunc		is_var_dim*/		FileVarIsDim,
/*NclReadVarDimensionFunc	read_var_dim_func*/	FileVarReadDim,
/*NclWriteVarDimensionFunc	write_var_dim_func*/	FileVarWriteDim,
/*NclReadDimensionFunc		read_dim_func*/		FileReadDim,
/*NclWriteDimensionFunc		write_dim_func*/	FileWriteDim,
/*NclFileIsAFunc		is_coord*/		FileIsCoord,
/*NclReadFileCoordFunc		read_coord_func*/	FileReadCoord,
/*NclWriteFileCoordFunc		write_coord_func*/	FileWriteCoord,
/*NclAddFileDimFunc		add_dim_func*/		FileAddDim,
/*NclAddFileChunkDimFunc	add_chunk_dim_func*/	FileAddChunkDim,
/*NclAddFileVarFunc		add_var_func*/		FileAddVar,
/*NclAddFileVarChunkFunc	add_var_chunk_func*/	FileAddVarChunk,
/*NclAddFileVarChunkCacheFunc	add_var_chunk_cache_func*/	   FileAddVarChunkCache,
/*NclSetFileVarCompressLevelFunc set_var_compress_level_func; */   FileSetVarCompressLevel,
/*NclAddFileVarAttFunc		add_var_att_func*/	NULL,
/*NclAddFileAttFunc		add_att_func*/		NULL,
/*NclSetFileOptionFunc		set_file_option*/	FileSetFileOption,
/*NclFileOption			*options*/		file_options,
/*NclFileIsAFunc		is_group*/		FileIsGroup,
/*NclGetFileGroupFunc		read_group_func*/	FileReadGroup,
		sizeof(file_options) / sizeof(file_options[0])
	}
};

NclObjClass nclFileClass = (NclObjClass)&nclFileClassRec;


NhlErrorTypes InitializeFileOptions
#if NhlNeedProto
(void)
#else
()
#endif
{
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	logical *lval;
	string *sval;
	float *fval;
	int *ival;
	ng_size_t len_dims;
	NhlErrorTypes ret = NhlNOERROR;
	
	
	/* option names are case insensitive and so are string-type 
	 * option values
	 */

	/* NetCDF option PreFill */
	fcp->options[Ncl_PREFILL].format = NrmStringToQuark("nc");
	fcp->options[Ncl_PREFILL].name = NrmStringToQuark("prefill");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_PREFILL].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_PREFILL].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_PREFILL].valid_values = NULL;

	/* NetCDF option DefineMode */
	fcp->options[Ncl_DEFINE_MODE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_DEFINE_MODE].name = NrmStringToQuark("definemode");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_DEFINE_MODE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_DEFINE_MODE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_DEFINE_MODE].valid_values = NULL;
	
	/* GRIB option ThinnedGridInterpolation */

	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].format = NrmStringToQuark("grb");
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].name = NrmStringToQuark("thinnedgridinterpolation");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("cubic");
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("cubic");
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(2 * sizeof(string));
	sval[0] = NrmStringToQuark("linear");
	sval[1] = NrmStringToQuark("cubic");
	len_dims = 2;
	fcp->options[Ncl_THINNED_GRID_INTERPOLATION].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* NetCDF option HeaderReserveSpace */
	fcp->options[Ncl_HEADER_RESERVE_SPACE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_HEADER_RESERVE_SPACE].name = NrmStringToQuark("headerreservespace");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 0;
	fcp->options[Ncl_HEADER_RESERVE_SPACE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 0;
	fcp->options[Ncl_HEADER_RESERVE_SPACE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_HEADER_RESERVE_SPACE].valid_values = NULL;

	/* NetCDF option SuppressClose */
	fcp->options[Ncl_SUPPRESS_CLOSE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_SUPPRESS_CLOSE].name = NrmStringToQuark("suppressclose");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_SUPPRESS_CLOSE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_SUPPRESS_CLOSE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_SUPPRESS_CLOSE].valid_values = NULL;


	/* NetCDF option Format */

	fcp->options[Ncl_FORMAT].format = NrmStringToQuark("nc");
	fcp->options[Ncl_FORMAT].name = NrmStringToQuark("format");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("classic");
	fcp->options[Ncl_FORMAT].value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("classic");
	fcp->options[Ncl_FORMAT].def_value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
#ifdef USE_NETCDF4
	len_dims = 5;
#else
	len_dims = 3;
#endif
	sval = (string*) NclMalloc(len_dims * sizeof(string));
	sval[0] = NrmStringToQuark("classic");
	sval[1] = NrmStringToQuark("64bitoffset");
	sval[2] = NrmStringToQuark("largefile");

#ifdef USE_NETCDF4
	sval[3] = NrmStringToQuark("netcdf4classic");
	sval[4] = NrmStringToQuark("netcdf4");
#endif

	fcp->options[Ncl_FORMAT].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* Binary option ReadByteOrder */

	fcp->options[Ncl_READ_BYTE_ORDER].format = NrmStringToQuark("bin");
	fcp->options[Ncl_READ_BYTE_ORDER].name = NrmStringToQuark("readbyteorder");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_READ_BYTE_ORDER].value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_READ_BYTE_ORDER].def_value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(3 * sizeof(string));
	sval[0] = NrmStringToQuark("native");
	sval[1] = NrmStringToQuark("bigendian");
	sval[2] = NrmStringToQuark("littleendian");
	len_dims = 3;
	fcp->options[Ncl_READ_BYTE_ORDER].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* Binary option WriteByteOrder */

	fcp->options[Ncl_WRITE_BYTE_ORDER].format = NrmStringToQuark("bin");
	fcp->options[Ncl_WRITE_BYTE_ORDER].name = NrmStringToQuark("writebyteorder");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_WRITE_BYTE_ORDER].value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("native");
	fcp->options[Ncl_WRITE_BYTE_ORDER].def_value = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
						    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(3 * sizeof(string));
	sval[0] = NrmStringToQuark("native");
	sval[1] = NrmStringToQuark("bigendian");
	sval[2] = NrmStringToQuark("littleendian");
	len_dims = 3;
	fcp->options[Ncl_WRITE_BYTE_ORDER].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);


	/* Grib option NumericIniitialTimeCoordinates */
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].format = NrmStringToQuark("grb");
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].name = NrmStringToQuark("initialtimecoordinatetype");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("numeric");
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("numeric");
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(2 * sizeof(string));
	sval[0] = NrmStringToQuark("string");
	sval[1] = NrmStringToQuark("numeric");
	len_dims = 2;
	fcp->options[Ncl_INITIAL_TIME_COORDINATE_TYPE].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* NetCDF option MissingToFillValue */
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].name = NrmStringToQuark("missingtofillvalue");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_MISSING_TO_FILL_VALUE].valid_values = NULL;


#ifdef USE_NETCDF4
	/* NetCDF 4 option compression level */
	fcp->options[Ncl_COMPRESSION_LEVEL].format = NrmStringToQuark("nc");
	fcp->options[Ncl_COMPRESSION_LEVEL].name = NrmStringToQuark("compressionlevel");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = -1;
	fcp->options[Ncl_COMPRESSION_LEVEL].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = -1;
	fcp->options[Ncl_COMPRESSION_LEVEL].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_COMPRESSION_LEVEL].valid_values = NULL;

	/* NetCDF 4 option use cache */
	fcp->options[Ncl_USE_CACHE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_USE_CACHE].name = NrmStringToQuark("cachepreemption");
	len_dims = 1;
	fval = (float *) NclMalloc(sizeof(float));
	*fval = 0.50;
	fcp->options[Ncl_USE_CACHE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0.50,(void *)fval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypefloatClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 0;
	fcp->options[Ncl_USE_CACHE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_USE_CACHE].valid_values = NULL;

	/* NetCDF 4 option cache size */
	fcp->options[Ncl_CACHE_SIZE].format = NrmStringToQuark("nc");
	fcp->options[Ncl_CACHE_SIZE].name = NrmStringToQuark("cachesize");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 3*1024*1025;
	fcp->options[Ncl_CACHE_SIZE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 3*1024*1025;
	fcp->options[Ncl_CACHE_SIZE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_CACHE_SIZE].valid_values = NULL;

	/* NetCDF 4 option cache nelems */
	fcp->options[Ncl_CACHE_NELEMS].format = NrmStringToQuark("nc");
	fcp->options[Ncl_CACHE_NELEMS].name = NrmStringToQuark("cachenelems");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 1009;
	fcp->options[Ncl_CACHE_NELEMS].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 1009;
	fcp->options[Ncl_CACHE_NELEMS].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_CACHE_NELEMS].valid_values = NULL;

	/* NetCDF 4 option cache preemption */
	fcp->options[Ncl_CACHE_PREEMPTION].format = NrmStringToQuark("nc");
	fcp->options[Ncl_CACHE_PREEMPTION].name = NrmStringToQuark("cachepreemption");
	len_dims = 1;
	fval = (float *) NclMalloc(sizeof(float));
	*fval = 0.50;
	fcp->options[Ncl_CACHE_PREEMPTION].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0.50,(void *)fval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypefloatClass);
	fval = (float*) NclMalloc(sizeof(float));
	*fval = 0.50;
	fcp->options[Ncl_CACHE_PREEMPTION].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0.50,(void *)fval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypefloatClass);
	fcp->options[Ncl_CACHE_PREEMPTION].valid_values = NULL;
#endif

#ifdef BuildHDF5
	/* HDF5 option compression level */
	fcp->options[Ncl_H5_COMPRESSION_LEVEL].format = NrmStringToQuark("h5");
	fcp->options[Ncl_H5_COMPRESSION_LEVEL].name = NrmStringToQuark("compressionlevel");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = -1;
	fcp->options[Ncl_H5_COMPRESSION_LEVEL].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = -1;
	fcp->options[Ncl_H5_COMPRESSION_LEVEL].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_H5_COMPRESSION_LEVEL].valid_values = NULL;

	/* HDF5 option use cache */
	fcp->options[Ncl_H5_USE_CACHE].format = NrmStringToQuark("h5");
	fcp->options[Ncl_H5_USE_CACHE].name = NrmStringToQuark("cachepreemption");
	len_dims = 1;
	fval = (float *) NclMalloc(sizeof(float));
	*fval = 0.50;
	fcp->options[Ncl_H5_USE_CACHE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0.50,(void *)fval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypefloatClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 0;
	fcp->options[Ncl_H5_USE_CACHE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_H5_USE_CACHE].valid_values = NULL;

	/* HDF5 option cache size */
	fcp->options[Ncl_H5_CACHE_SIZE].format = NrmStringToQuark("h5");
	fcp->options[Ncl_H5_CACHE_SIZE].name = NrmStringToQuark("cachesize");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 3*1024*1025;
	fcp->options[Ncl_H5_CACHE_SIZE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 3*1024*1025;
	fcp->options[Ncl_H5_CACHE_SIZE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_H5_CACHE_SIZE].valid_values = NULL;

	/* HDF5 option cache nelems */
	fcp->options[Ncl_H5_CACHE_NELEMS].format = NrmStringToQuark("h5");
	fcp->options[Ncl_H5_CACHE_NELEMS].name = NrmStringToQuark("cachenelems");
	len_dims = 1;
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 1009;
	fcp->options[Ncl_H5_CACHE_NELEMS].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	ival = (int*) NclMalloc(sizeof(int));
	*ival = 1009;
	fcp->options[Ncl_H5_CACHE_NELEMS].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)ival,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
	fcp->options[Ncl_H5_CACHE_NELEMS].valid_values = NULL;

	/* HDF5 option cache preemption */
	fcp->options[Ncl_H5_CACHE_PREEMPTION].format = NrmStringToQuark("h5");
	fcp->options[Ncl_H5_CACHE_PREEMPTION].name = NrmStringToQuark("cachepreemption");
	len_dims = 1;
	fval = (float *) NclMalloc(sizeof(float));
	*fval = 0.50;
	fcp->options[Ncl_H5_CACHE_PREEMPTION].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0.50,(void *)fval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypefloatClass);
	fval = (float*) NclMalloc(sizeof(float));
	*fval = 0.50;
	fcp->options[Ncl_H5_CACHE_PREEMPTION].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0.50,(void *)fval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypefloatClass);
	fcp->options[Ncl_H5_CACHE_PREEMPTION].valid_values = NULL;
#endif

	/* Grib option Default_NCEP_Ptable */
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].format = NrmStringToQuark("grb");
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].name = NrmStringToQuark("defaultncepptable");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("operational");
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("operational");
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(2 * sizeof(string));
	sval[0] = NrmStringToQuark("operational");
	sval[1] = NrmStringToQuark("reanalysis");
	len_dims = 2;
	fcp->options[Ncl_DEFAULT_NCEP_PTABLE].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);


	/* GRIB (2) option PrintRecordInfo */
	fcp->options[Ncl_PRINT_RECORD_INFO].format = NrmStringToQuark("grb");
	fcp->options[Ncl_PRINT_RECORD_INFO].name = NrmStringToQuark("printrecordinfo");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = False;
	fcp->options[Ncl_PRINT_RECORD_INFO].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = False;
	fcp->options[Ncl_PRINT_RECORD_INFO].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_PRINT_RECORD_INFO].valid_values = NULL;

	/* Grib option Single element dimensions */
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].format = NrmStringToQuark("grb");
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].name = NrmStringToQuark("singleelementdimensions");
	len_dims = 1;
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("none");
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(sizeof(string));
	*sval = NrmStringToQuark("none");
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
	sval = (string*) NclMalloc(7 * sizeof(string));
	sval[0] = NrmStringToQuark("none");
	sval[1] = NrmStringToQuark("all");
	sval[2] = NrmStringToQuark("ensemble");
	sval[3] = NrmStringToQuark("initial_time");
	sval[4] = NrmStringToQuark("forecast_time");
	sval[5] = NrmStringToQuark("level");
	sval[6] = NrmStringToQuark("probability");
	len_dims = 7;
	fcp->options[Ncl_SINGLE_ELEMENT_DIMENSIONS].valid_values = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)sval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypestringClass);

	/* GRIB option TimePeriodSuffix */

	fcp->options[Ncl_TIME_PERIOD_SUFFIX].format = NrmStringToQuark("grb");
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].name = NrmStringToQuark("timeperiodsuffix");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = True;
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_TIME_PERIOD_SUFFIX].valid_values = NULL;

	/* new file-structure */

	fcp->options[Ncl_USE_NEW_HLFS].format = NrmStringToQuark("nc");
	fcp->options[Ncl_USE_NEW_HLFS].name = NrmStringToQuark("usenewhlfs");
	len_dims = 1;
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = False;
	fcp->options[Ncl_USE_NEW_HLFS].value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	lval = (logical*) NclMalloc(sizeof(logical));
	*lval = False;
	fcp->options[Ncl_USE_NEW_HLFS].def_value = 
		_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void *)lval,
				    NULL,1,&len_dims,PERMANENT,NULL,(NclTypeClass)nclTypelogicalClass);
	fcp->options[Ncl_USE_NEW_HLFS].valid_values = NULL;

	/* End of options */

	return ret;
}

static NhlErrorTypes InitializeFileClass
#if NhlNeedProto
(void)
#else
()
#endif
{

	InitializeFileOptions();

	_NclRegisterClassPointer(
		Ncl_File,
		(NclObjClass)&nclFileClassRec
	);
	
	return(NhlNOERROR);
}

void AddAttInfoToList
#if	NhlNeedProto
(NclFileAttInfoList **list_handle,struct _NclFAttRec*     the_att) 
#else 
(list_handle,the_att) 
NclFileAttInfoList **list_handle;
struct _NclFAttRec*     the_att; 
#endif
{
	NclFileAttInfoList *step;
	if(*list_handle == NULL) {
		*list_handle = (NclFileAttInfoList*)NclMalloc(sizeof(NclFileAttInfoList));
		(*list_handle)->next = NULL;
		(*list_handle)->the_att = the_att;
	} else {
		step = *list_handle;
		while(step->next != NULL) {
			step = step->next;
		}
		step->next = (NclFileAttInfoList*)NclMalloc(sizeof(NclFileAttInfoList));
		step->next->next = NULL;
		step->next->the_att = the_att;
	}
	return;
}
static int FileIsVar
#if	NhlNeedProto
(NclFile thefile,NclQuark var)
#else 
(thefile,var)
	NclFile thefile;
	NclQuark var;
#endif
{
	int i, n;
	int has_compound = 0;
	char *dot_ptr;
	char *slash_ptr;
	char var_str[1024];
	NclQuark var_quark;

      /*
       *fprintf(stdout, "\n\n\nhit FileIsVar. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tvar: <%s>\n", NrmQuarkToString(var));
       *fprintf(stdout, "\tthefile->file.n_vars: %d\n", thefile->file.n_vars);
       */

	strcpy(var_str, NrmQuarkToString(var));
	slash_ptr = strrchr(var_str, '/');
	dot_ptr = strchr(var_str, '.');
	if(dot_ptr)
	{
		char var_name[1024];
		char component[1024];
		NclQuark component_name_quark;
		strcpy(component, dot_ptr);
                component_name_quark = NrmStringToQuark(dot_ptr + 1);
                dot_ptr[0] = '\0';
		var_quark = NrmStringToQuark(var_str);
	      /*
               *fprintf(stdout, "\n\n\nhit FileIsVar. file: %s, line: %d\n", __FILE__, __LINE__);
	       *fprintf(stdout, "\tvar: <%s> has . in it.\n\n", var_str);
	       *fprintf(stdout, "\tvar_quark: <%s>\n", NrmQuarkToString(var_quark));
	       *fprintf(stdout, "\tcomponent_name_quark: <%s>\n", NrmQuarkToString(component_name_quark));
	       */
		for(i = 0; i < thefile->file.n_vars; i++) {
			if(thefile->file.var_info[i]->num_compounds)
			{
			has_compound++;
			strcpy(var_name, NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
			dot_ptr = strchr(var_name, '.');
			if(dot_ptr)
			{
				dot_ptr[0] = '\0';
				thefile->file.var_info[i]->var_full_name_quark = NrmStringToQuark(var_name);

				strcpy(var_name, NrmQuarkToString(thefile->file.var_info[i]->var_real_name_quark));
				dot_ptr = strchr(var_name, '.');
				dot_ptr[0] = '\0';
				thefile->file.var_info[i]->var_real_name_quark = NrmStringToQuark(var_name);

				strcpy(var_name, NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
				dot_ptr = strchr(var_name, '.');
				dot_ptr[0] = '\0';
				thefile->file.var_info[i]->var_name_quark = NrmStringToQuark(var_name);
			}
			}
		}

		if(has_compound)
		{
		for(i = 0; i < thefile->file.n_vars; i++) {
		      /*
		       *fprintf(stdout, "\tCheck %d: var_full_name <%s>\n\n", i, 
		       *	NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
		       */
			if((thefile->file.var_info[i]->var_full_name_quark == var_quark) ||
			   (thefile->file.var_info[i]->var_real_name_quark == var_quark) ||
			   (thefile->file.var_info[i]->var_name_quark == var_quark)) {
			      /*
			       *fprintf(stdout, "\tFind var <%s>\n", NrmQuarkToString(var));
			       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_full_name_quark <%s>\n",
			       *	i, NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
			       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_real_name_quark <%s>\n",
			       *	i, NrmQuarkToString(thefile->file.var_info[i]->var_real_name_quark));
			       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_name_quark <%s>\n",
			       *	i, NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
			       */
				strcpy(var_str, NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
				strcat(var_str, component);
				thefile->file.var_info[i]->var_full_name_quark = NrmStringToQuark(var_str);
				strcpy(var_str, NrmQuarkToString(thefile->file.var_info[i]->var_real_name_quark));
				strcat(var_str, component);
				thefile->file.var_info[i]->var_real_name_quark = NrmStringToQuark(var_str);
				strcpy(var_str, NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
				strcat(var_str, component);
				thefile->file.var_info[i]->var_name_quark = NrmStringToQuark(var_str);
			      /*
			       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_full_name_quark <%s>\n",
			       *	i, NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
			       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_real_name_quark <%s>\n",
			       *i, NrmQuarkToString(thefile->file.var_info[i]->var_real_name_quark));
			       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_name_quark <%s>\n",
			       *	i, NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
			       */
				for(n = 0; n < thefile->file.var_info[i]->num_compounds; n++)
				{
					if(thefile->file.var_info[i]->component_name[n] == component_name_quark)
					{
						thefile->file.var_info[i]->data_type = thefile->file.var_info[i]->component_type[n];
						break;
					}
				}
				return(i);
			}
		}
		}
	}

	if(NULL == slash_ptr)
	{
		for(i = 0; i < thefile->file.n_vars; i++) {
			if((thefile->file.var_info[i]->var_full_name_quark == var) ||
			   (thefile->file.var_info[i]->var_real_name_quark == var) ||
			   (thefile->file.var_info[i]->var_name_quark == var)) {
				return(i);
			}
		}
	}
	else
	{
	      /*
               *fprintf(stdout, "\n\n\nhit FileIsVar. file: %s, line: %d\n", __FILE__, __LINE__);
	       *fprintf(stdout, "\tvar: <%s> has / in it.\n\n", var_str);
	       *fprintf(stdout, "\tvar short name: %s.\n\n", slash_ptr+1);
	       */
		for(i = 0; i < thefile->file.n_vars; i++) {
		      /*
		       *fprintf(stdout, "\tCheck %d: var_full_name <%s>\n", i, 
		       *	NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
		       */
			if((thefile->file.var_info[i]->var_full_name_quark == var) ||
			   (thefile->file.var_info[i]->var_real_name_quark == var) ||
			   (thefile->file.var_info[i]->var_name_quark == var)) {
			      /*
			       *fprintf(stdout, "\tFind var_quark <%s>\n", NrmQuarkToString(var));
			       */
				if(thefile->file.var_info[i]->var_full_name_quark == var)
					thefile->file.var_info[i]->var_name_quark = var;
				return(i);
			}
		}
	}
      /*
       *fprintf(stdout, "\n\n\nEnd FileIsVar. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tCANNOT FIND var: <%s>\n", NrmQuarkToString(var));
       */
	return(-1);
}

static int FileIsGroup
#if	NhlNeedProto
(NclFile thefile,NclQuark group)
#else 
(thefile,group)
	NclFile thefile;
	NclQuark group;
#endif
{
	int i;

      /*
        fprintf(stdout, "\n\n\nhit FileIsGroup. file: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stdout, "\tgroup: <%s>\n", NrmQuarkToString(group));
       */
	for(i = 0; i < thefile->file.n_vars; i++) {
	      /*
	       *fprintf(stdout, "\tCheck %d: var_full_name <%s>\n\n", i, 
	       *	NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
	       */
		if((thefile->file.var_info[i]->var_full_name_quark == group) ||
		   (thefile->file.var_info[i]->var_real_name_quark == group) ||
		   (thefile->file.var_info[i]->var_name_quark == group)) {
		      /*
        		fprintf(stdout, "\n\n\nend FileIsGroup. file: %s, line: %d\n", __FILE__, __LINE__);
		        fprintf(stdout, "\tFind group <%s>\n\n", NrmQuarkToString(group));
		       */
			return(i);
		}
	}
      /*
        fprintf(stdout, "\n\n\nEnd FileIsGroup. file: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stdout, "\tCANNOT FIND group: <%s>\n", NrmQuarkToString(group));
       */
	return(-1);
}

static NclObjTypes FileVarRepValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var)
#else 
(thefile, var)
NclFile thefile;
NclQuark var;
#endif
{
	int index; 

	index = FileIsVar(thefile,var);

	if(index > -1) {	
		return(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type));
	} else {
		return(Ncl_None);
	}
}

void ReverseIt
#if	NhlNeedProto
(void *val,void* swap_space,int ndims,int *compare_sel,ng_size_t *dim_sizes,int el_size)
#else
(val,swap_space,ndims,compare_sel,dim_sizes,el_size)
void *val;
void* swap_space;
int ndims;
int *compare_sel;
ng_size_t *dim_sizes;
int el_size;
#endif
{
	ng_size_t i,j;
	char *tmp;
	int block_size = el_size;

	for(i = 1; i < ndims; i++) {
	block_size *= dim_sizes[i];
	}
	tmp = (char*)val;
	if(ndims != 1) {
		for(i = 0; i < *dim_sizes; i++) {
			ReverseIt((void*)(tmp+(i*block_size)),swap_space,ndims-1,&(compare_sel[1]),&(dim_sizes[1]),el_size);
		}
	}
	if(*compare_sel == NCLFILE_DEC) {
		j = *dim_sizes -1;
		for(i = 0; i < (*dim_sizes)/2;i++) {
			memcpy(swap_space,(void*)&(tmp[i*block_size]),block_size);
			memcpy((void*)&(tmp[i*block_size]),(void*)&(tmp[j*block_size]),block_size);
			memcpy((void*)&(tmp[j*block_size]),swap_space,block_size);
			j--;
		}
		
	} 	
	return;
}

static struct _NclMultiDValDataRec* MyFileReadVarValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr,NclDimRec* dim_info,int vtype)
#else 
(thefile, var_name, sel_ptr,dim_info,vtype)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
NclDimRec* dim_info;
int vtype;
#endif
{
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData mis_md = NULL;
	NclScalar missing_value;
	int has_missing = 0;
	void *val = NULL;
	int index;
	long start[NCL_MAX_DIMENSIONS];
	long finish[NCL_MAX_DIMENSIONS];
	long stride[NCL_MAX_DIMENSIONS];
	long real_stride[NCL_MAX_DIMENSIONS];
	int i,j,k,done = 0,inc_done = 0;
	int n_dims_input;
        long  n_elem = 1;
	int n_dims_output = 1;
	long total_elements = 1;
	int has_vectors = 0;
	int has_stride = 0;
	int has_reverse = 0;
	int has_reorder = 0;
	int to = 0,block_read_limit = 1,n_elem_block;
	
	long multiplier_input[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	long current_index[NCL_MAX_DIMENSIONS];
	long current_finish[NCL_MAX_DIMENSIONS];
	int index_map[NCL_MAX_DIMENSIONS];
	ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];
	int keeper[NCL_MAX_DIMENSIONS];
	NclSelection *sel;
	float tmpf;
	long tmpi;
	int swap_size;
	void *swap_space = NULL;
/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/
	index = FileIsVar(thefile,var_name);
	n_dims_input = thefile->file.var_info[index]->num_dimensions;
	if(sel_ptr != NULL) {
		sel = sel_ptr->selection;
		for(i = 0; i < n_dims_input; i++) {
			switch(sel->sel_type) {
			case Ncl_SUB_ALL:
				start[sel->dim_num] = 0;
			case Ncl_SUB_VAL_DEF:
				if(Ncl_SUB_VAL_DEF == sel->sel_type) {
					start[sel->dim_num] = sel->u.sub.start;
				}
				finish[sel->dim_num] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1;
			case Ncl_SUB_DEF_VAL:
				if(sel->sel_type == Ncl_SUB_DEF_VAL) {
					finish[sel->dim_num] = sel->u.sub.finish;
					start[sel->dim_num] = 0;
				} 
			case Ncl_SUBSCR:
				if(sel->u.sub.is_single) {
					keeper[i] = 0;
				} else {
					keeper[i] = 1;
				}
				if(sel->sel_type == Ncl_SUBSCR) {
					start[sel->dim_num] = sel->u.sub.start;
					finish[sel->dim_num] = sel->u.sub.finish;
					stride[sel->dim_num] = sel->u.sub.stride;

				} else {
					stride[sel->dim_num] = sel->u.sub.stride;
				}
				if(finish[sel->dim_num] < start[sel->dim_num]) {
					if(stride[sel->dim_num] < 0) {
						tmpi = finish[sel->dim_num] + (start[sel->dim_num] - finish[sel->dim_num]) % labs(stride[sel->dim_num]);
						finish[sel->dim_num] = start[sel->dim_num];
						start[sel->dim_num] = tmpi;
						compare_sel[sel->dim_num] = NCLFILE_INC;
						stride[sel->dim_num] = -(stride[sel->dim_num]); 
					} else {
						compare_sel[sel->dim_num] = NCLFILE_DEC;
						stride[sel->dim_num] = -(stride[sel->dim_num]); 
						has_reverse = 1;
					}
				} else {
					if(stride[sel->dim_num] < 0) {
						has_reverse = 1;
                                                tmpi = finish[sel->dim_num] - (finish[sel->dim_num] - start[sel->dim_num]) % labs(stride[sel->dim_num]);
                                                finish[sel->dim_num] = start[sel->dim_num];
                                                start[sel->dim_num] = tmpi;
                                                compare_sel[sel->dim_num] = NCLFILE_DEC;
                                                stride[sel->dim_num] = (stride[sel->dim_num]);
                                        } else {
                                                compare_sel[sel->dim_num] = NCLFILE_INC;
                                                stride[sel->dim_num] = (stride[sel->dim_num]);
                                        }

				}
				if(labs(stride[sel->dim_num]) > 1) 
					has_stride = 1;
				if(stride[sel->dim_num] != 0)  {
					tmpi = labs(sel->u.sub.stride);
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");

					stride[sel->dim_num] = 1;
					tmpf = 1;
				}
				n_elem = labs((finish[sel->dim_num] - start[sel->dim_num]) /tmpi) + 1;
				if((sel->u.sub.start > thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1)||(sel->u.sub.start < 0)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				if((sel->u.sub.finish> thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1)||(sel->u.sub.finish< 0)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				

/*
* set when normal subscript
*/
				if(sel->dim_num != i) {
					has_reorder = 1;
				}
				index_map[i] = sel->dim_num;
				break;
			case Ncl_VECSUBSCR:
				keeper[i] = 1;
				if((sel->u.vec.min < 0)||(sel->u.vec.min >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)) {	
					NhlPError(NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				if((sel->u.vec.max < 0)||(sel->u.vec.max >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)) {	
					NhlPError(NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				n_elem = sel->u.vec.n_ind;
			
				stride[sel->dim_num] = 0;
				start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
				has_vectors = 1;
				if(sel->dim_num != i) {
					has_reorder = 1;
				}
				index_map[i] = sel->dim_num;

/*
* 0 when vector subscript
*/
				compare_sel[sel->dim_num] = NCLFILE_VEC;
				break;
			}
			if(sel->dim_num != n_dims_input -1) {
				multiplier_input[sel->dim_num] = 1;
				for(k = sel->dim_num + 1; k < n_dims_input; k++) {
					multiplier_input[sel->dim_num] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
				}
			}
			output_dim_sizes[i] = n_elem;
			(dim_info)[i].dim_num = i;
			(dim_info)[i].dim_size = n_elem;
			(dim_info)[i].dim_quark = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[sel->dim_num]);
			total_elements = total_elements * n_elem;
			sel++;
		}
		sel = sel_ptr->selection;
	} else {
	      /*
	       *fprintf(stdout, "\n\n\nhit MyFileReadVarValue. file: %s, line: %d\n", __FILE__, __LINE__);
	       *fprintf(stdout, "\tn_dims_input = %d\n", n_dims_input);
	       */
		for(i = 0; i< n_dims_input; i++) {
		      /*
		       *fprintf(stdout, "\ti = %d\n", i);
		       */
			start[i] = 0;
			finish[i] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size - 1;
			stride[i] = 1;
			total_elements *= (finish[i] + 1);
			output_dim_sizes[i] = finish[i] + 1;
			(dim_info)[i].dim_num = i;
			(dim_info)[i].dim_size = output_dim_sizes[i];
			(dim_info)[i].dim_quark = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[i]);
			compare_sel[i] = NCLFILE_INC;
			multiplier_input[i] = 1;
			for(k = i + 1; k < n_dims_input; k++) {
				multiplier_input[i] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
			}

		}
		sel = NULL;
	}

	if (total_elements == 0) {
		/* can't return any data because there is a 0-length dimension but nevertheless return what is possible */
		NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadVar: %s contains a 0 length dimension", 
			  NrmQuarkToString(var_name));
		n_dims_output = n_dims_input;
		val = NULL;

		if(sel_ptr != NULL) {
			i = 0;
			while((i <  n_dims_output)&&(n_dims_output > 1)) {
				if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
					for(j = i; j < n_dims_output-1; j++) {
						output_dim_sizes[j] = output_dim_sizes[j+1];
						keeper[j] = keeper[j+1];
						(dim_info)[j] = (dim_info)[j+1];
					}
					n_dims_output--;
				} else {
					i++;
				}
			}
		}

		if(FileIsVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT))!=-1){
			mis_md = FileReadVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
			if(mis_md != NULL) {
				has_missing = 1;
				if (mis_md->multidval.val == NULL) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileReadVar: _FillValue attribute for  variable (%s) in file (%s) has NULL value, substituting default fill value of variable type",
						  NrmQuarkToString(var_name),NrmQuarkToString(thefile->file.fname));
					_NclGetDefaultFillValue(thefile->file.var_info[index]->data_type,&missing_value);
				}
				else if (mis_md->multidval.data_type == thefile->file.var_info[index]->data_type) {
					memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
				}
				else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,
						  "FileReadVar: _FillValue attribute type differs from variable (%s) type in file (%s), forcing type conversion; may result in overflow and/or loss of precision",
						  NrmQuarkToString(var_name),NrmQuarkToString(thefile->file.fname));
					_NclScalarForcedCoerce(mis_md->multidval.val,mis_md->multidval.data_type,
							       (void*)&missing_value,thefile->file.var_info[index]->data_type);
				}
			}
		} 
		if(vtype == FILE_COORD_VAR_ACCESS) {
			tmp_md = _NclOneDValCoordDataCreate(
				NULL,
				NULL,
				Ncl_OneDValCoordData,
				0,
				val,
				(has_missing ? &missing_value:NULL),
				n_dims_output,
				output_dim_sizes,
				TEMPORARY,
				sel_ptr,
				_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
				);
		} else {
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				val,
				(has_missing ? &missing_value:NULL),
				n_dims_output,
				output_dim_sizes,
				TEMPORARY,
				sel_ptr,
				_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
				);
		}
		return(tmp_md);
	}
		

/*
* When ncl gets here all strides are positive and finishs are greater than starts
* and stride and finishes all corespond to their *CORRECT* dimension numbers not
* and reordered dimensions. Any reordering has to occur afterwards.
*/

/*
* Basically everything is different depending on which format_func field contains
* the read function.
*/

/* 
* Take care of simplest case here
*/
	if((vtype == FILE_VAR_ACCESS? thefile->file.format_funcs->read_var != NULL:thefile->file.format_funcs->read_coord != NULL)) {
		if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! val) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
					  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			if(vtype == FILE_VAR_ACCESS) {
				(*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					stride,
					val);
			} else {
				(*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					stride,
					val);
			}
		
			n_dims_output = n_dims_input;
			if(sel_ptr != NULL) {
				i = 0;
				while((i <  n_dims_output)&&(n_dims_output > 1)) {
					if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
						for(j = i; j < n_dims_output-1; j++) {
							output_dim_sizes[j] = output_dim_sizes[j+1];
							keeper[j] = keeper[j+1];
							(dim_info)[j] = (dim_info)[j+1];
						}
						n_dims_output--;
					} else {
						i++;
					}
				}
			}
		} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
/*
* If a reverse is detected it is quicker to read everything in inorder and
* perform swapping in memory. This is easiest done if selections containing
* vectors and dimension reordering are excluded. Unfortunately swap space
* is needed and this could be as large as the product of the sizes of all 
* dimensions with the exception of dimension 0.
*/
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! val) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
					  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			i = 0;
			while((i<n_dims_input)&&(compare_sel[i] != NCLFILE_DEC)){
				i++;
			}
			swap_size = 1;
			for(j = i + 1; j < n_dims_input; j++) {	
				swap_size *= output_dim_sizes[j];
			}
			swap_space = (void*)NclMalloc(swap_size * _NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! swap_space) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to handle dimension reversal for variable <%s> from file <%s>",
					  swap_size*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			for(i = 0;i < n_dims_input; i++) {
				switch(compare_sel[i]) {
				case NCLFILE_INC:
					current_index[i] = start[i];
					current_finish[i] = finish[i];
					real_stride[i] = labs(stride[i]);
					break;
				case NCLFILE_DEC:
/*
* Problem here is that selecting in reverse order could
* alter selection when (finish - start )%stride != 0
* Therefore a new start and finish must be computed to
* produce desired selection
*/
					real_stride[i] = labs(stride[i]);
					current_finish[i] = start[i];
					if(( start[i] - finish[i])%labs(stride[i]) == 0) {
						current_index[i] = finish[i] ;
					} else {
						current_index[i] = finish[i]+ (start[i] - finish[i])%labs(stride[i]);
					}
					break;
				}
			}
			if(vtype == FILE_VAR_ACCESS) {
				(*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					current_index,
					current_finish,
					real_stride,
					(void*)val);
			} else {
				(*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					current_index,
					current_finish,
					real_stride,
					(void*)val);
			}
                        n_dims_output = n_dims_input;
			if(sel_ptr != NULL) {
                        	i = 0;
                        	while((i <  n_dims_output)&&(n_dims_output > 1)) {
                                	if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
                                        	for(j = i; j < n_dims_output-1; j++) {
                                                	output_dim_sizes[j] = output_dim_sizes[j+1];
                                                	keeper[j] = keeper[j+1];
							compare_sel[j] = compare_sel[j+1];
                                                	(dim_info)[j] = (dim_info)[j+1];
                                        	}
                                        	n_dims_output--;
                                	} else {
                                        	i++;
                                	}
                        	}
			}
			ReverseIt(val,swap_space,n_dims_output,compare_sel,output_dim_sizes,_NclSizeOf(thefile->file.var_info[index]->data_type));
			NclFree(swap_space);
		} else {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! val) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
					  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			to = 0;
			block_read_limit = n_dims_input - 1 ;
/*
* Find out what size chunks can be read in at once
*/
			for(i = n_dims_input-1 ; i>= 0; i--) {
				if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
					block_read_limit = i;
					break;
				}
			}
/*
* Initialize starting index, finish and stride values for first read
*/
                        n_elem_block = 1;
                        for(i = 0; i < n_dims_input ; i++) {
                                current_index[index_map[i]] = start[index_map[i]];
                                if(i > block_read_limit) {
/*
* OK to use i here since these indices are in order
*/
                                        n_elem_block *= output_dim_sizes[index_map[i]];
                                        current_finish[index_map[i]] = finish[index_map[i]];
                                        real_stride[index_map[i]] = labs(stride[index_map[i]]);
                                } else {
                                        switch(compare_sel[index_map[i]]) {
                                        case NCLFILE_INC:
                                                current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                real_stride[index_map[i]] = 1;
                                                break;
                                        case NCLFILE_DEC:
                                                current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                real_stride[index_map[i]] = 1;
                                                break;
                                        default:         /* vectors */
                                                current_finish[index_map[i]]  = current_index[index_map[i]];
                                                real_stride[index_map[i]] = 1;
                                                break;
                                        }
                                }
                        }
			while(!done) {
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				} else {
					(*thefile->file.format_funcs->read_coord)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				}
				to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
				if(compare_sel[index_map[block_read_limit]] < 0) {
					current_index[index_map[block_read_limit]] += stride[index_map[block_read_limit]];
					current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
				} else {
					compare_sel[index_map[block_read_limit]]++;
				}
				for(i = block_read_limit; i > 0; i--) {
					switch(compare_sel[index_map[i]]) {
					case NCLFILE_INC:
						if(current_index[index_map[i]] > finish[index_map[i]]) {
							current_index[index_map[i]] = start[index_map[i]];
							if(compare_sel[index_map[i-1]] < 0 ) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}

						} else {
							inc_done = 1;
						}	
						current_finish[index_map[i]] = current_index[index_map[i]] ;
						break;
					case NCLFILE_DEC:
						if(current_index[index_map[i]] < finish[index_map[i]]) {
							current_index[index_map[i]] = start[index_map[i]];
							if(compare_sel[index_map[i-1]] < 0) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}
						} else {	
							inc_done = 1;
						}
						current_finish[index_map[i]] = current_index[index_map[i]];
						break;
					default:
						if(compare_sel[index_map[i]] >= sel[i].u.vec.n_ind) {
							compare_sel[index_map[i]] = 0;
							current_index[index_map[i]] = sel[i].u.vec.ind[0];
							if(compare_sel[index_map[i-1]] < 0 ) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}
						} else {
							current_index[index_map[i]] = sel[i].u.vec.ind[compare_sel[index_map[i]]];
							inc_done = 1;
						}
						current_finish[index_map[i]] = current_index[index_map[i]];
						break;
					} 
					if(inc_done) {
						inc_done = 0;
						break;
					}
				}
				switch(compare_sel[index_map[0]]) {
				case NCLFILE_DEC:
					if(current_index[index_map[0]] < finish[index_map[0]]) 
							done = 1;
					current_finish[index_map[0]] = current_index[index_map[0]]; 
					break;
				case NCLFILE_INC:
					if(current_index[index_map[0]] > finish[index_map[0]]) 
							done = 1;
					current_finish[index_map[0]] = current_index[index_map[0]]; 
					break;
				default:
					if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
							done = 1;
					}  else {
						current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
					}
					current_finish[index_map[0]] = current_index[index_map[0]]; 
				}
			}
			n_dims_output = n_dims_input;
			if(sel_ptr != NULL) {
				i = 0;
				while((i <  n_dims_output)&&(n_dims_output > 1)) {
					if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
						for(j = i; j < n_dims_output-1; j++) {
							output_dim_sizes[j] = output_dim_sizes[j+1];
							keeper[j] = keeper[j+1];
							(dim_info)[j] = (dim_info)[j+1];
						}
						n_dims_output--;
					} else {
						i++;
					}
				}
			}
		}
	} else if((vtype == FILE_VAR_ACCESS ? thefile->file.format_funcs->read_var_ns != NULL : thefile->file.format_funcs->read_coord_ns!= NULL)){
		if(!has_stride) {
			if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! val) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
						  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						start,
						finish,
						val);
				} else {
					(*thefile->file.format_funcs->read_coord_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						start,
						finish,
						val);
				}
			
				n_dims_output = n_dims_input;
				if(sel_ptr != NULL) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								keeper[j] = keeper[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
	/*
	* If a reverse is detected it is quicker to read everything in inorder and
	* perform swapping in memory. This is easiest done if selections containing
	* vectors and dimension reordering are excluded. Unfortunately swap space
	* is needed and this could be as large as the product of the sizes of all 
	* dimensions with the exception of dimension 0.
	*/
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! val) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
						  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				i = 0;
				while((i<n_dims_input)&&(compare_sel[i] != NCLFILE_DEC)){
					i++;
				}
				swap_size = 1;
				for(j = i + 1; j < n_dims_input; j++) {	
					swap_size *= output_dim_sizes[j];
				}
				swap_space = (void*)NclMalloc(swap_size * _NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! swap_space) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to handle dimension reversal for variable <%s> from file <%s>",
						  swap_size*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				for(i = 0;i < n_dims_input; i++) {
					switch(compare_sel[i]) {
					case NCLFILE_INC:
						current_index[i] = start[i];
						current_finish[i] = finish[i];
						break;
					case NCLFILE_DEC:
	/*
	* Problem here is that selecting in reverse order could
	* alter selection when (finish - start )%stride != 0
	* Therefore a new start and finish must be computed to
	* produce desired selection
	*/
						current_finish[i] = start[i];
						current_index[i] = finish[i] ;
						break;
					}
				}
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						(void*)val);
				} else {
					(*thefile->file.format_funcs->read_coord_ns)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						(void*)val);
				}
                        	n_dims_output = n_dims_input;
				if(sel_ptr != NULL) {
                        		i = 0;
                        		while((i <  n_dims_output)&&(n_dims_output > 1)) {
                                		if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
                                        		for(j = i; j < n_dims_output-1; j++) {
                                                		output_dim_sizes[j] = output_dim_sizes[j+1];
                                                		keeper[j] = keeper[j+1];
								compare_sel[j] = compare_sel[j+1];
                                                		(dim_info)[j] = (dim_info)[j+1];
                                        		}
                                        		n_dims_output--;
                                		} else {
                                        		i++;
                                		}
                        		}
				}
				ReverseIt(val,swap_space,n_dims_output,compare_sel,output_dim_sizes,_NclSizeOf(thefile->file.var_info[index]->data_type));
				NclFree(swap_space);
			} else {
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! val) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
						  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				to = 0;
				block_read_limit = n_dims_input - 1 ;
	/*
	* Find out what size chunks can be read in at once
	*/
				for(i = n_dims_input-1 ; i>= 0; i--) {
					if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
						block_read_limit = i;
						break;
					}
				}
	/*
	* Initialize starting index, finish and stride values for first read
	*/
                        	n_elem_block = 1;
                        	for(i = 0; i < n_dims_input ; i++) {
                                	current_index[index_map[i]] = start[index_map[i]];
                                	if(i > block_read_limit) {
	/*
	* OK to use i here since these indices are in order
	*/
                                        	n_elem_block *= output_dim_sizes[i];
                                        	current_finish[index_map[i]] = finish[index_map[i]];
                                	} else {
                                        	switch(compare_sel[index_map[i]]) {
                                        	case NCLFILE_INC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	break;
                                        	case NCLFILE_DEC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	break;
                                        	default:         /* vectors */
                                                	current_finish[index_map[i]]  = current_index[index_map[i]];
                                                	break;
                                        	}
                                	}
                        	}
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
					if(compare_sel[index_map[block_read_limit]] < 0) {
						current_index[index_map[block_read_limit]] += 1;
						current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
					} else {
						compare_sel[index_map[block_read_limit]]++;
					}
					for(i = block_read_limit; i > 0; i--) {
						switch(compare_sel[index_map[i]]) {
						case NCLFILE_INC:
							if(current_index[index_map[i]] > finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += 1; 
								} else {
									compare_sel[index_map[i-1]]++;
								}
	
							} else {
								inc_done = 1;
							}	
							current_finish[index_map[i]] = current_index[index_map[i]] ;
							break;
						case NCLFILE_DEC:
							if(current_index[index_map[i]] < finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0) {
									current_index[index_map[i-1]] += 1; 
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {	
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						default:
							if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
								compare_sel[index_map[i]] = 0;
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += 1; 
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						} 
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					switch(compare_sel[index_map[0]]) {
					case NCLFILE_DEC:
						if(current_index[index_map[0]] < finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					case NCLFILE_INC:
						if(current_index[index_map[0]] > finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					default:
						if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
						}  else {
							current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
						}
						current_finish[index_map[0]] = current_index[index_map[0]]; 
					}
				}
				n_dims_output = n_dims_input;
				if(sel_ptr != NULL ) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			}
		} else {
			if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
/*
* Loop through and find block size. Then implement read like it is vectors
*/
				for(i = n_dims_input-1; i >= 0; i--) {
					if(stride[i] != 1) {
						block_read_limit = i;
						break;
					}
				}
				n_elem_block = 1;
				for(i = 0; i < n_dims_input; i++) {
					current_index[i] = start[i];
					if(i > block_read_limit) {
						n_elem_block *= output_dim_sizes[i];
						current_finish[i] = finish[i];
					} else {
						current_finish[i] = current_index[i];
					}
				}
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! val) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
						  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				to = 0;
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							val);
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
/*
* No reverse here so all is just added
*/
					for(i = block_read_limit; i >= 0 ; i--) {
						if((current_index[i] + stride[i] > finish[i])&&(i != 0)) {
							current_index[i] = start[i];
							current_finish[i] = current_index[i];
						} else {
							current_index[i] = current_index[i] + stride[i];
							current_finish[i] = current_index[i];
							break;
						}
					}
					if(current_index[0]  > finish[0]) {
						done = 1;
					}
					current_finish[0] = current_index[0];
				}
				n_dims_output = n_dims_input;
				if(sel_ptr != NULL) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
/*
* Loop through and file block size. Then implment read like it contains vectors. Finnally call ReverseIt.
*/
				for(i = n_dims_input-1; i >= 0; i--) {
					if(stride[i] != 1) {
						block_read_limit = i;
						break;
					}
				}
				n_elem_block = 1;
				for(i = 0; i < n_dims_input; i++) {
					if(i > block_read_limit) {
/*
* To be in this range stride is 1
* Still could be reverse.
*/
						n_elem_block *= output_dim_sizes[i];
						switch(compare_sel[i]) {
						case NCLFILE_DEC:
							real_stride[i] = (stride[i]);
							current_finish[i] = start[i];
							current_index[i] = finish[i];
							break;
						case NCLFILE_INC:
							real_stride[i] = (stride[i]);
							current_finish[i] = finish[i];
							current_index[i] = start[i];
							break;
						}
					} else {
						switch(compare_sel[i]) {
						case NCLFILE_DEC:
							real_stride[i] = (stride[i]);
							break;
						case NCLFILE_INC:
							real_stride[i] = (stride[i]);
							break;
						}
						current_index[i] = start[i];
						current_finish[i] = current_index[i];
					}
				}
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! val) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
						  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				swap_space = NclMalloc(n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! swap_space) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to handle dimension reversal for variable <%s> from file <%s>",
						  n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				to = 0;
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							val);
					}
					if((n_dims_input - (block_read_limit + 1))>=1) {
						ReverseIt((void*)&(((char*)val)[to]),swap_space,n_dims_input - (block_read_limit + 1),&(compare_sel[block_read_limit+1]),&(output_dim_sizes[block_read_limit+1]),_NclSizeOf(thefile->file.var_info[index]->data_type));
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
/*
* No reverse here so all is just added
*/
					for(i = block_read_limit; i >= 0 ; i--) {
						switch(compare_sel[i]) {
						case NCLFILE_DEC:
							if((current_index[i] + real_stride[i] < finish[i])&&(i != 0)) {
								current_index[i] = start[i];
								current_finish[i] = current_index[i];
							} else {
								current_index[i] +=  real_stride[i];
								current_finish[i] = current_index[i];
								inc_done = 1;
							}
							break;
						case NCLFILE_INC:
							if((current_index[i] + real_stride[i]> finish[i])&&(i != 0)) {
								current_index[i] = start[i];
								current_finish[i] = current_index[i];
							} else {
								current_index[i] +=  real_stride[i];
								current_finish[i] = current_index[i];
								inc_done = 1;
							}
							break;
						}
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					if(compare_sel[0] == NCLFILE_INC) {
						if(current_index[0] > finish[0]) {
							done = 1;
						}
					} else {
						if(current_index[0] < start[0]) {
							done = 1;
						}
					}
					current_finish[0] = current_index[0];
				}
				NclFree(swap_space);
				n_dims_output = n_dims_input;

				if(sel_ptr != NULL)  {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			} else {
/*
* has vectors or reorder or both
*/
				val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
				if (! val) {
					NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
						  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
						  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
						  NrmQuarkToString(thefile->file.fname));
					return(NULL);
				}
				to = 0;
				block_read_limit = n_dims_input - 1 ;
/*
* Find out what size chunks can be read in at once
*/
				for(i = n_dims_input-1 ; i>= 0; i--) {
					if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)||(stride[index_map[i]] != 1)) {
						block_read_limit = i;
						break;
					}
				}
/*
* Initialize starting index, finish and stride values for first read
*/
                        	n_elem_block = 1;
                        	for(i = 0; i < n_dims_input ; i++) {
                                	current_index[index_map[i]] = start[index_map[i]];
                                	if(i > block_read_limit) {
/*
* OK to use i here since these indices are in order also above loop filter strides so stride 
* = 1 ###
*/
                                        	n_elem_block *= output_dim_sizes[index_map[i]];
                                        	current_finish[index_map[i]] = finish[index_map[i]];
                                        	real_stride[index_map[i]] = stride[index_map[i]];
                                	} else {
                                        	switch(compare_sel[index_map[i]]) {
                                        	case NCLFILE_INC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	real_stride[index_map[i]] = stride[index_map[i]];
                                                	break;
                                        	case NCLFILE_DEC:
                                                	current_finish[index_map[i]] = current_index[index_map[i]] ;
                                                	real_stride[index_map[i]] = stride[index_map[i]];
                                                	break;
                                        	default:         /* vectors */
                                                	current_finish[index_map[i]]  = current_index[index_map[i]];
                                                	real_stride[index_map[i]] = stride[index_map[i]];
                                                	break;
                                        	}
                                	}
                        	}
				while(!done) {
					if(vtype == FILE_VAR_ACCESS) {
						(*thefile->file.format_funcs->read_var_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					} else {
						(*thefile->file.format_funcs->read_coord_ns)(
							thefile->file.private_rec,
							thefile->file.var_info[index]->var_name_quark,
							current_index,
							current_finish,
							(void*)&(((char*)val)[to]));
					}
					to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
					if(compare_sel[index_map[block_read_limit]] < 0) {
						current_index[index_map[block_read_limit]] += real_stride[index_map[block_read_limit]];
						current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
					} else {
						compare_sel[index_map[block_read_limit]]++;
					}
					for(i = block_read_limit; i > 0; i--) {
						switch(compare_sel[index_map[i]]) {
						case NCLFILE_INC:
							if(current_index[index_map[i]] > finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += real_stride[index_map[i-1]];
								} else {
									compare_sel[index_map[i-1]]++;
								}
	
							} else {
								inc_done = 1;
							}	
							current_finish[index_map[i]] = current_index[index_map[i]] ;
							break;
						case NCLFILE_DEC:
							if(current_index[index_map[i]] < finish[index_map[i]]) {
								current_index[index_map[i]] = start[index_map[i]];
								if(compare_sel[index_map[i-1]] < 0) {
									current_index[index_map[i-1]] += real_stride[index_map[i-1]];
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {	
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						default:
							if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
								compare_sel[index_map[i]] = 0;
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
								if(compare_sel[index_map[i-1]] < 0 ) {
									current_index[index_map[i-1]] += real_stride[index_map[i-1]];
								} else {
									compare_sel[index_map[i-1]]++;
								}
							} else {
								current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
								inc_done = 1;
							}
							current_finish[index_map[i]] = current_index[index_map[i]];
							break;
						} 
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					switch(compare_sel[index_map[0]]) {
					case NCLFILE_DEC:
						if(current_index[index_map[0]] < finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					case NCLFILE_INC:
						if(current_index[index_map[0]] > finish[index_map[0]]) 
								done = 1;
						current_finish[index_map[0]] = current_index[index_map[0]]; 
						break;
					default:
						if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
						}  else {
							current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
						}
						current_finish[index_map[0]] = current_index[index_map[0]]; 
					}
				}
				n_dims_output = n_dims_input;
				fprintf(stdout,"Temporary comment 9\n");
				if(sel_ptr != NULL) {
					i = 0;
					while((i <  n_dims_output)&&(n_dims_output > 1)) {
						if((output_dim_sizes[i] == 1)&&!(keeper[i])) {
							for(j = i; j < n_dims_output-1; j++) {
								output_dim_sizes[j] = output_dim_sizes[j+1];
								(dim_info)[j] = (dim_info)[j+1];
							}
							n_dims_output--;
						} else {
							i++;
						}
					}
				}
			}
		} 
	} 
	if(FileIsVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT))!=-1){
		mis_md = FileReadVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
		if(mis_md != NULL) {
			has_missing = 1;
			if (mis_md->multidval.val == NULL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  "FileReadVar: _FillValue attribute for  variable (%s) in file (%s) has NULL value, substituting default fill value of variable type",
					  NrmQuarkToString(var_name),NrmQuarkToString(thefile->file.fname));
				_NclGetDefaultFillValue(thefile->file.var_info[index]->data_type,&missing_value);
			}
			if (mis_md->multidval.data_type == thefile->file.var_info[index]->data_type) {
				memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
			}
			else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
	      "FileReadVar: _FillValue attribute type differs from variable (%s) type in file (%s), forcing type conversion; may result in overflow and/or loss of precision",
					  NrmQuarkToString(var_name),NrmQuarkToString(thefile->file.fname));
				_NclScalarForcedCoerce(mis_md->multidval.val,mis_md->multidval.data_type,
						       (void*)&missing_value,thefile->file.var_info[index]->data_type);
			}

		}
	} 
	if(vtype == FILE_COORD_VAR_ACCESS) {
		tmp_md = _NclOneDValCoordDataCreate(
			NULL,
			NULL,
			Ncl_OneDValCoordData,
			0,
			val,
			(has_missing ? &missing_value:NULL),
			n_dims_output,
			output_dim_sizes,
			TEMPORARY,
			sel_ptr,
			_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
			);
	} else {
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			val,
			(has_missing ? &missing_value:NULL),
			n_dims_output,
			output_dim_sizes,
			TEMPORARY,
			sel_ptr,
			_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type))
			);
	}
	return(tmp_md);
}
static struct _NclMultiDValDataRec* FileReadVarValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];

	return(MyFileReadVarValue(thefile, var_name, sel_ptr,dim_info,FILE_VAR_ACCESS));
}

static struct _NclMultiDValDataRec* MyFileReadGroupValue
#if	NhlNeedProto
(NclFile thefile, NclQuark group_name, int vtype)
#else 
(thefile, group_name, vtype)
NclFile thefile;
NclQuark group_name;
int vtype;
#endif
{
	NclMultiDValData tmp_md = NULL;
	int index;

	index = FileIsGroup(thefile,group_name);
	fprintf(stdout, "\n\nMyFileReadGroupValue, file: %s, line:%d\n", __FILE__, __LINE__);
	fprintf(stdout, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
	fprintf(stdout, "\tindex = %d\n", index);

/*
	if (total_elements == 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadGroup: %s contains a 0 length dimension", 
			  NrmQuarkToString(group_name));
		n_dims_output = n_dims_input;
		val = NULL;

		if(FileIsVarAtt(thefile,group_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT))!=-1){
			mis_md = FileReadVarAtt(thefile,group_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
			if(mis_md != NULL) {
				memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
				has_missing = 1;
			}
		} 
	}
		
	if((vtype == FILE_VAR_ACCESS? thefile->file.format_funcs->read_var != NULL:thefile->file.format_funcs->read_coord != NULL)) {
		if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! val) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
					  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			if(vtype == FILE_VAR_ACCESS) {
				(*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->group_name_quark,
					start,
					finish,
					stride,
					val);
			} else {
				(*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->group_name_quark,
					start,
					finish,
					stride,
					val);
			}
		} else if((has_reverse)&&(!has_vectors)&&(!has_reorder)){
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! val) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
					  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			if(vtype == FILE_VAR_ACCESS) {
				(*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->group_name_quark,
					current_index,
					current_finish,
					real_stride,
					(void*)val);
			} else {
				(*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->group_name_quark,
					current_index,
					current_finish,
					real_stride,
					(void*)val);
			}
		} else {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if (! val) {
				NhlPError(NhlFATAL,ENOMEM,"Error allocating %lld bytes to read variable <%s> from file <%s>",
					  total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type),
					  NrmQuarkToString(thefile->file.var_info[index]->var_name_quark),
					  NrmQuarkToString(thefile->file.fname));
				return(NULL);
			}
			while(!done) {
				if(vtype == FILE_VAR_ACCESS) {
					(*thefile->file.format_funcs->read_var)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->group_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				} else {
					(*thefile->file.format_funcs->read_coord)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->group_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				}
				to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
			}
		}
	} 
	if(FileIsVarAtt(thefile,group_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT))!=-1){
		mis_md = FileReadVarAtt(thefile,group_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
		if(mis_md != NULL) {
			memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
			has_missing = 1;
		}
	} 
*/
	return(tmp_md);
}

static struct _NclMultiDValDataRec* FileReadGroupValue
#if	NhlNeedProto
(NclFile thefile, NclQuark group_name)
#else 
(thefile, group_name)
NclFile thefile;
NclQuark group_name;
#endif
{
	fprintf(stdout, "\n\nFileReadGroupValue, file: %s, line:%d\n", __FILE__, __LINE__);
	fprintf(stdout, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));

	return(MyFileReadGroupValue(thefile, group_name, FILE_VAR_ACCESS));
}


static struct _NclVarRec *FileReadVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp_att_md = NULL;
	NclVar tmp_var = NULL;
	int index;
	int att_id,i,j=0;
	NclSelectionRecord tmp_sel;
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	int coords[NCL_MAX_DIMENSIONS];
	NclSelection *sel = NULL;
	NclObj  att_obj = NULL;
	int single = 0;
/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/
	index = FileIsVar(thefile,var_name);
	if(index > -1) {
		tmp_md = MyFileReadVarValue(thefile,var_name,sel_ptr,dim_info,FILE_VAR_ACCESS);
		if(tmp_md == NULL) {
			return(NULL);
		}

		if(thefile->file.var_att_ids[index] == -1)
			LoadVarAtts(thefile,var_name);
		att_id = thefile->file.var_att_ids[index];
		att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
		if(! att_obj) {
			att_id = -1;
		}
		else {
			att_id = att_obj->obj.id;
			if (_NclIsAtt(att_id,"_FillValue")) {
				tmp_att_md = _NclGetAtt(att_id,"_FillValue",NULL);
				if (tmp_att_md->multidval.data_type != tmp_md->multidval.data_type || tmp_att_md->multidval.val == NULL) {
					ng_size_t tmp_size = 1;
					NclScalar *tmp_mis = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
					*tmp_mis = tmp_md->multidval.missing_value.value;
					tmp_att_md = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,
									 (void*)tmp_mis,NULL,1,&tmp_size,TEMPORARY,NULL,
									 tmp_md->multidval.type);
					_NclDeleteAtt(att_id,"_FillValue");
					_NclAddAtt(att_id,"_FillValue",tmp_att_md,NULL);
				}
			}
		}
		if(sel_ptr == NULL) {
/*
* Because some file may allow dimensions of size 1 special care must be taken here
*/
			for(i = 0 ; i < tmp_md->multidval.n_dims; i++){
				if(_NclFileVarIsCoord(thefile,dim_info[i].dim_quark)!= -1) {
					tmp_var = _NclFileReadCoord(thefile,dim_info[i].dim_quark,NULL);
					if(tmp_var != NULL) {
						coords[i] = tmp_var->obj.id;
					} else {
						coords[i] = -1;
					}
				} else {
					coords[i] = -1;
				}
			}
			sel = NULL;
		} else {
			sel = sel_ptr->selection;
			tmp_sel.n_entries = 1;
			tmp_sel.selected_from_sym = NULL;
			tmp_sel.selected_from_var = NULL;
			tmp_sel.selection[0].dim_num = 0;
			j = 0;
			for(i = 0 ; i < thefile->file.var_info[index]->num_dimensions; i++){
				if(_NclFileVarIsCoord(thefile,
					thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_name_quark)!= -1) {
					tmp_sel.selection[0] = sel[i];
					tmp_sel.selection[0].dim_num = 0;
					tmp_var = _NclFileReadCoord(thefile,thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_name_quark,&tmp_sel);
					if(tmp_var != NULL) {
						if(sel[i].sel_type == Ncl_VECSUBSCR) {
							if((tmp_var->var.n_dims == 1)&&(tmp_var->var.dim_info[0].dim_size == 1)) {
								single = 1;
							}
				
						} else {
							if(sel[i].u.sub.start == sel[i].u.sub.finish) {
                                                	        single = sel[i].u.sub.is_single;
                                                	}
						}
						coords[j] = tmp_var->obj.id;
					} else {
						return(NULL);
					}
				} else {
					switch(sel[i].sel_type) {
					case Ncl_VECSUBSCR:
						if(sel[i].u.vec.n_ind == 1) {
							single = 1;
						}
						break;
					case Ncl_SUB_ALL:
						if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_size == 1) {
							single = 0;
						}
						break;
					case Ncl_SUB_VAL_DEF:
						if(sel[i].u.sub.start == thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_size -1) {
							single = 0;
						}
						break;
					case Ncl_SUB_DEF_VAL:
						if(sel[i].u.sub.finish== 0) {
							single = 0;
						}
						break;
					case Ncl_SUBSCR:
						if(sel[i].u.sub.start == sel[i].u.sub.finish) {
							single = sel[i].u.sub.is_single;
						}
						break;
					}
					coords[j] = -1;
				}
				if(single) {
					if(coords[j] != -1) {
						if(att_id == -1) {
							att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
						} 
						_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel[i].dim_num]]->dim_name_quark),_NclVarValueRead(tmp_var,NULL,NULL),&tmp_sel);
						coords[j] = -1;
						if(tmp_var->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)tmp_var);
						}
					}
					single = 0;
				} else {
					j++;
				}
			}
		}
		tmp_var = NULL;
		
		
	
		if(tmp_md != NULL) {
			tmp_var = _NclVarCreate(
				NULL,
				NULL,
				Ncl_Var,
				0,
				NULL,
				tmp_md,
				dim_info,
				att_id,
				coords,
				(sel == NULL ? FILEVAR : FILEVARSUBSEL),
				NrmQuarkToString(var_name),TEMPORARY);
			if(tmp_var == NULL) {
				_NclDestroyObj((NclObj)tmp_md);
				if(att_id != -1) {
					_NclDestroyObj((NclObj)_NclGetObj(att_id));
				}
			}
		}
	}
	return(tmp_var);
}

static int FileIsVarAtt
#if	NhlNeedProto
(NclFile thefile,NclQuark var,NclQuark theatt)
#else 
(thefile,var,theatt)
NclFile thefile;
NclQuark var;
NclQuark theatt;
#endif
{
	int index,i;
	NclFileAttInfoList *step_att;

	index = FileIsVar(thefile,var);
	i = 0;
	if(index > -1) {
		step_att = thefile->file.var_att_info[index];
		while(step_att != NULL) {
			if(step_att->the_att->att_name_quark == theatt) {
				return(i);
			} else {
				i++;
				step_att = step_att->next;
			}
		}
	}
	return(-1);
}
static struct _NclMultiDValDataRec *FileReadVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, var, attname, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int aindex,index;
	NclMultiDValData tmp_md;

	aindex = FileIsVarAtt(thefile,var,attname);
	if(aindex > -1) {
		NclScalar missing_value;
		ng_size_t dim_size = 1;
		char *type_name;
		NclTypeClass type_class;

		index = FileIsVar(thefile,var);
		if(thefile->file.var_att_ids[index] == -1) 
			LoadVarAtts(thefile,var);
		tmp_md = _NclGetAtt(thefile->file.var_att_ids[index],NrmQuarkToString(attname),sel_ptr);
		if (attname != NrmStringToQuark("_FillValue")) 
			return (tmp_md);
		else if (tmp_md->multidval.val == NULL) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "FileReadVar: _FillValue attribute for  variable (%s) in file (%s) has NULL value, substituting default fill value of variable type",
				  NrmQuarkToString(var),NrmQuarkToString(thefile->file.fname));
			_NclGetDefaultFillValue(thefile->file.var_info[index]->data_type,&missing_value);
		}
		else if (tmp_md->multidval.data_type == thefile->file.var_info[index]->data_type) 
			return (tmp_md);
		else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,
			  "FileReadVarAtt: _FillValue attribute type differs from variable (%s) type in file (%s), forcing type conversion; may result in overflow and/or loss of precision",
				  NrmQuarkToString(var),NrmQuarkToString(thefile->file.fname));
			_NclScalarForcedCoerce(tmp_md->multidval.val,tmp_md->multidval.data_type,
					       (void*)&missing_value,thefile->file.var_info[index]->data_type);
		}
		
		type_name = _NclBasicDataTypeToName(thefile->file.var_info[index]->data_type);
		type_class = _NclNameToTypeClass(NrmStringToQuark(type_name));
		return (_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)&missing_value,NULL,1,&dim_size,PERMANENT,NULL,type_class));
	}
	NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadVarAtt: (%s) is not an attribute of (%s)",NrmQuarkToString(attname),NrmQuarkToString(var));
	return(_NclCreateMissing());
}

static int FileIsAtt
#if	NhlNeedProto
(NclFile thefile,NclQuark theatt)
#else 
(thefile,theatt)
NclFile thefile;
NclQuark theatt;
#endif
{
	int i;

	for( i = 0; i < thefile->file.n_file_atts; i++) {
		if(thefile->file.file_atts[i]->att_name_quark == theatt) {
			return(i);
		}
	}
	return(-1);
}


static int FileIsDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name)
#else 
(thefile, dim_name)
NclFile thefile;
NclQuark dim_name;
#endif
{
	int i;
	
	for(i =0; i < thefile->file.n_file_dims; i++) {
		if(thefile->file.file_dim_info[i]->dim_name_quark == dim_name) {
			return(i);
		}
	} 
	return (-1);
}

static int FileVarIsDim
#if	NhlNeedProto
(NclFile thefile,NclQuark var, NclQuark dim_name)
#else 
(thefile,var,dim_name)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
#endif
{
	int i,j;

	for(i = 0 ; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i]->var_name_quark == var) {
			for(j = 0; j < thefile->file.var_info[i]->num_dimensions; j++){
				if(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j]) == dim_name) {
					return(j);
				}
			}
			return(-1);
		}
	}
	return(-1);
}


static int FileIsCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name)
#else 
(thefile,coord_name)
NclFile thefile;
NclQuark coord_name;
#endif
{
	int index;

	index = FileIsDim(thefile,coord_name);
	if(index > -1) {
		if(thefile->file.coord_vars[index] != NULL) {
			return(index);
		} 
	}
	return(-1);
}

static void _NclNullifyFilePart(NclFilePart *file)
{
	int i;

	file->max_grps = 0;
	file->max_vars = 0;
	file->max_file_dims = 0;
	file->max_file_atts = 0;

	file->n_grps = 0;
	file->n_vars = 0;
	file->n_file_dims = 0;
	file->n_file_atts = 0;

	file->file_atts_id = -1;

	file->file_att_cb = NULL;
	file->file_att_udata = NULL;
	file->format_funcs = NULL;
	file->private_rec = NULL;

	file->grp_info = NULL;
	file->grp_att_info = NULL;
	file->grp_att_udata = NULL;
	file->grp_att_cb = NULL;
	file->grp_att_ids = NULL;

	file->var_info = NULL;
	file->var_att_info = NULL;
	file->var_att_udata = NULL;
	file->var_att_cb = NULL;
	file->var_att_ids = NULL;

	file->file_atts = NULL;

	file->file_dim_info = NULL;

	file->coord_vars = NULL;
}

void _NclInitFilePart(NclFilePart *file)
{
	int i;

	file->max_grps = NCL_MAX_FVARS;
	file->max_vars = NCL_MAX_FVARS;
	file->max_file_dims = NCL_MAX_FVARS;
	file->max_file_atts = NCL_MAX_FVARS;

	file->n_grps = 0;
	file->n_vars = 0;
	file->n_file_dims = 0;
	file->n_file_atts = 0;

	file->file_atts_id = -1;

	file->file_att_cb = NULL;
	file->file_att_udata = NULL;
	file->private_rec = NULL;

	file->grp_info = (struct _NclFGrpRec **)NclCalloc(file->max_grps, sizeof(struct _NclFGrpRec *));
	assert(file->grp_info);
	file->grp_att_info = (NclFileAttInfoList **)NclCalloc(file->max_grps, sizeof(NclFileAttInfoList *));
	assert(file->grp_att_info);
	file->grp_att_udata = (struct _FileCallBackRec **)NclCalloc(file->max_grps, sizeof(struct _FileCallBackRec *));
	assert(file->grp_att_udata);
	file->grp_att_cb = (_NhlCB *)NclCalloc(file->max_grps, sizeof(_NhlCB));
	assert(file->grp_att_cb);
	file->grp_att_ids = (int *)NclCalloc(file->max_grps, sizeof(int));
	assert(file->grp_att_ids);

	file->var_info = (struct _NclFVarRec **)NclCalloc(file->max_vars, sizeof(struct _NclFVarRec *));
	assert(file->var_info);
	file->var_att_info = (NclFileAttInfoList **)NclCalloc(file->max_vars, sizeof(NclFileAttInfoList *));
	assert(file->var_att_info);
	file->var_att_udata = (struct _FileCallBackRec **)NclCalloc(file->max_vars, sizeof(struct _FileCallBackRec *));
	assert(file->var_att_udata);
	file->var_att_cb = (_NhlCB *)NclCalloc(file->max_vars, sizeof(_NhlCB));
	assert(file->var_att_cb);
	file->var_att_ids = (int *)NclCalloc(file->max_vars, sizeof(int));
	assert(file->var_att_ids);

	file->file_atts = (struct _NclFAttRec **)NclCalloc(file->max_file_atts, sizeof(struct _NclFAttRec *));
        assert(file->file_atts);

	file->file_dim_info = (struct _NclFDimRec **)NclCalloc(file->max_file_dims, sizeof(struct _NclFDimRec *));
	assert(file->file_dim_info);

	file->coord_vars = (struct _NclFVarRec **)NclCalloc(file->max_file_dims, sizeof(struct _NclFVarRec *));
        assert(file->coord_vars);

	for(i = 0; i < file->max_grps; i++)
	{
		file->grp_info[i] = NULL;
		file->grp_att_info[i] = NULL;
		file->grp_att_udata[i] = NULL;
		file->grp_att_cb[i] = NULL;
		file->grp_att_ids[i] = -1;
	}

	for(i = 0; i < file->max_vars; i++)
	{
		file->var_info[i] = NULL;
		file->var_att_info[i] = NULL;
		file->var_att_udata[i] = NULL;
		file->var_att_cb[i] = NULL;
		file->var_att_ids[i] = -1;
	}

	for(i = 0; i < file->max_file_atts; i++)
	{
		file->file_atts[i] = NULL;
	}

	for(i = 0; i < file->max_file_dims; i++)
	{
		file->file_dim_info[i] = NULL;
		file->coord_vars[i] = NULL;
	}
}

void _NclReallocFilePart(NclFilePart *file,
				int n_grps, int n_vars,
				int n_file_dims, int n_file_atts)
{
	int i;

	if(n_grps > 0)
	{
		int pre_max_grps = file->max_grps;

		if(n_grps < file->max_grps)
		{
			file->max_grps = n_grps + 1;
		}
		else
		{
			while(n_grps >= file->max_grps)
				file->max_grps *= 2;
		}

		file->grp_info = (struct _NclFGrpRec **)NclRealloc(file->grp_info,
							file->max_grps * sizeof(struct _NclFGrpRec *));
		assert(file->grp_info);
		file->grp_att_info = (NclFileAttInfoList **)NclRealloc(file->grp_att_info,
							file->max_grps * sizeof(NclFileAttInfoList *));
		assert(file->grp_att_info);
		file->grp_att_udata = (struct _FileCallBackRec **)NclRealloc(file->grp_att_udata,
							file->max_grps * sizeof(struct _FileCallBackRec *));
		assert(file->grp_att_udata);
		file->grp_att_cb = (_NhlCB *)NclRealloc(file->grp_att_cb, file->max_grps * sizeof(_NhlCB));
		assert(file->grp_att_cb);
		file->grp_att_ids = (int *)NclRealloc(file->grp_att_ids, file->max_grps * sizeof(int));
		assert(file->grp_att_ids);

		for(i = pre_max_grps; i < file->max_grps; i++)
		{
			file->grp_info[i] = NULL;
			file->grp_att_info[i] = NULL;
			file->grp_att_udata[i] = NULL;
			file->grp_att_cb[i] = NULL;
			file->grp_att_ids[i] = -1;
		}
	}

	if(n_vars > 0)
	{
		int pre_max_vars = file->max_vars;

		if(n_vars < file->max_vars)
		{
			file->max_vars = n_vars + 1;
		}
		else
		{
			while(n_vars >= file->max_vars)
				file->max_vars *= 2;
		}

		file->var_info = (struct _NclFVarRec **)NclRealloc(file->var_info,
							file->max_vars * sizeof(struct _NclFVarRec *));
		assert(file->var_info);
		file->var_att_info = (NclFileAttInfoList **)NclRealloc(file->var_att_info,
							file->max_vars * sizeof(NclFileAttInfoList *));
		assert(file->var_att_info);
		file->var_att_udata = (struct _FileCallBackRec **)NclRealloc(file->var_att_udata,
							file->max_vars * sizeof(struct _FileCallBackRec *));
		assert(file->var_att_udata);
		file->var_att_cb = (_NhlCB *)NclRealloc(file->var_att_cb, file->max_vars * sizeof(_NhlCB));
		assert(file->var_att_cb);
		file->var_att_ids = (int *)NclRealloc(file->var_att_ids, file->max_vars * sizeof(int));
		assert(file->var_att_ids);

		for(i = pre_max_vars; i < file->max_vars; i++)
		{
			file->var_info[i] = NULL;
			file->var_att_info[i] = NULL;
			file->var_att_udata[i] = NULL;
			file->var_att_cb[i] = NULL;
			file->var_att_ids[i] = -1;
		}
	}

	if(n_file_dims > 0)
	{
		int pre_max_file_dims = file->max_file_dims;

		if(n_file_dims < file->max_file_dims)
		{
			file->max_file_dims = n_file_dims + 1;
		}
		else
		{
			while(n_file_dims >= file->max_file_dims)
				file->max_file_dims *= 2;
		}

		file->file_dim_info = (struct _NclFDimRec **)NclRealloc(file->file_dim_info,
							file->max_file_dims * sizeof(struct _NclFDimRec *));
		assert(file->file_dim_info);

		file->coord_vars = (struct _NclFVarRec **)NclRealloc(file->coord_vars,
							file->max_file_dims * sizeof(struct _NclFVarRec *));
        	assert(file->coord_vars);

		for(i = pre_max_file_dims; i < file->max_file_dims; i++)
		{
			file->file_dim_info[i] = NULL;
			file->coord_vars[i] = NULL;
		}
	}

	if(n_file_atts > 0)
	{
		int pre_max_file_atts = file->max_file_atts;

		if(n_file_atts < file->max_file_atts)
		{
			file->max_file_atts = n_file_atts + 1;
		}
		else
		{
			while(n_file_atts >= file->max_file_atts)
				file->max_file_atts *= 2;
		}

		file->file_atts = (struct _NclFAttRec **)NclRealloc(file->file_atts,
						file->max_file_atts * sizeof(struct _NclFAttRec *));
        	assert(file->file_atts);

		for(i = pre_max_file_atts; i < file->max_file_atts; i++)
		{
			file->file_atts[i] = NULL;
		}
	}
}

NclFile _NclFileCreate(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
			unsigned int obj_type_mask, NclStatus status,
			NclQuark path, int rw_status, NclQuark file_ext_q,
			NclQuark fname_q, NhlBoolean is_http,
			char *end_of_name, int len_path)
{
	char *the_path = NrmQuarkToString(path);
	NclQuark the_real_path = -1;
	char *tmp_path = NULL;
	int i,j;
	NclFile file_out = NULL;
	int file_out_free = 0;
	NhlErrorTypes ret= NhlNOERROR;
	NclObjClass class_ptr;
	NclQuark *name_list;
	int n_names;
	NclQuark *name_list2;
	int n_names2;
	int index;
	struct stat buf;

	ret = _NclInitClass(nclFileClass);
	if(ret < NhlWARNING) 
		return(NULL);
	if(theclass == NULL) {
		class_ptr = nclFileClass;
	} else {
		class_ptr = theclass;
	}

 	/*
     	 * If a GRIB file, check version.  First verify that the file exists
     	 * and is accessible (path to it must be searchable). _NclFormatEqual handles the 
	 * case-less comparison of all possible variants of the the extension.
	 * Note we also need to check here for extensions added to the real path.
     	 */
	if (_NclFormatEqual(NrmStringToQuark("grb"),NrmStringToQuark(end_of_name))) {
		the_real_path = path;
		if(stat(_NGResolvePath(NrmQuarkToString(path)),&buf) == -1) {
			tmp_path = NclMalloc(len_path+1);
			strncpy(tmp_path,the_path,len_path);
			tmp_path[len_path] = '\0';
			if(stat(_NGResolvePath(tmp_path),&buf) == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "_NclFileCreate: Requested file does not exist as (%s) or as (%s), can't add file",
					  the_path,tmp_path);
				NclFree(tmp_path);
				return(NULL);
			} else {
				the_real_path = NrmStringToQuark(tmp_path);
				NclFree(tmp_path);
			}
		}
                grib_version = _NclGribVersion(NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))));
        }

	if(inst == NULL) {
		file_out = (NclFile)NclMalloc(sizeof(NclFileRec));
		file_out_free = 1;
	} else {
		file_out = (NclFile)inst;
	}

	file_out->file.fname = fname_q;
	file_out->file.file_format = 0;
	file_out->file.file_ext_q = file_ext_q;

	_NclInitFilePart(&(file_out->file));

	file_out->file.format_funcs = _NclGetFormatFuncs(file_ext_q);
	if (! file_out->file.format_funcs) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error has occurred. The file format requested does not appear to be supported, could not open (%s)",NrmQuarkToString(path));
		if(file_out_free) 
			NclFree((void*)file_out);
		return(NULL);
	}
	file_out->file.private_rec = (*file_out->file.format_funcs->initialize_file_rec)(&file_out->file.file_format);
	if(file_out->file.private_rec == NULL)
	{
		NhlPError(NhlFATAL,ENOMEM,NULL);
		if(file_out_free) 
			NclFree((void*)file_out);
		return(NULL);
	}

	if (file_out->file.format_funcs->set_option != NULL) {
		NclFileClassPart *fcp = &(nclFileClassRec.file_class);
		for (i = 0; i < fcp->num_options; i++) {
			if (file_out->file.format_funcs != _NclGetFormatFuncs(fcp->options[i].format)) {
				continue;
			}
			if (fcp->options[i].access == 1 && rw_status != 1)
				continue;
			else if (fcp->options[i].access == 2 && rw_status > 0)
				continue;
			else if (fcp->options[i].access == 3 && rw_status != -1)
				continue;
			file_out->file.format_funcs->set_option(file_out->file.private_rec,fcp->options[i].name,
								fcp->options[i].value->multidval.data_type,
								fcp->options[i].value->multidval.totalelements,
								fcp->options[i].value->multidval.val);
		}
	}					
	if (is_http) {
		file_out->file.fpath = the_real_path = path;
		file_out->file.wr_status = rw_status;
		file_out->file.private_rec = (*file_out->file.format_funcs->open_file)(file_out->file.private_rec,
								the_real_path,rw_status);
		if(! file_out->file.private_rec) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",NrmQuarkToString(the_real_path));
			if(file_out_free) 
				NclFree((void*)file_out);
			return(NULL);
		}
	}
	else {
		if((file_out->file.format_funcs->open_file != NULL)&&((rw_status != -1)||
								      (file_out->file.format_funcs->create_file != NULL))) {
			if(rw_status == -1) {
				file_out->file.fpath = the_real_path = path;
				file_out->file.wr_status = rw_status;
				file_out->file.private_rec = (*file_out->file.format_funcs->create_file)
					(file_out->file.private_rec,
					 NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))));
				if(! file_out->file.private_rec) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not create (%s)",NrmQuarkToString(the_real_path));
					if(file_out_free) 
						NclFree((void*)file_out);
					return(NULL);
				}
			} else {
				if(stat(_NGResolvePath(NrmQuarkToString(path)),&buf) == -1) {
					tmp_path = NclMalloc(len_path+1);
					strncpy(tmp_path,the_path,len_path);
					tmp_path[len_path] = '\0';
					if(stat(_NGResolvePath(tmp_path),&buf) == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclFileCreate: Requested file does not exist as (%s) or as (%s), can't add file",the_path,tmp_path);
						NclFree(tmp_path);
						return(NULL);
					} else {
						the_real_path = NrmStringToQuark(tmp_path);
						file_out->file.fpath = the_real_path;
						NclFree(tmp_path);
					}
				} else {
					the_real_path = path;
					file_out->file.fpath = the_real_path;
				}
				file_out->file.wr_status = rw_status;
				file_out->file.private_rec = (*file_out->file.format_funcs->open_file)
					(file_out->file.private_rec,
					 NrmStringToQuark(_NGResolvePath(NrmQuarkToString(the_real_path))),rw_status);	
				if(NULL == file_out->file.private_rec)
				{
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",NrmQuarkToString(the_real_path));
					if(file_out_free) 
						NclFree((void*)file_out);
					return(NULL);
				}
			}
		} else  {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error in the extension code for the requested file format has occurred, could not open (%s)",NrmQuarkToString(the_real_path));
			if(file_out_free) 
				NclFree((void*)file_out);
			return(NULL);
		}
	}

	if(file_out->file.format_funcs->get_grp_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_grp_names)(file_out->file.private_rec,&n_names);

		_NclReallocFilePart(&(file_out->file), n_names, -1, -1, -1);
		file_out->file.n_grps = n_names;

		for(i = 0; i < n_names; i++){
			file_out->file.grp_info[i] = (*file_out->file.format_funcs->get_grp_info)(file_out->file.private_rec,name_list[i]);
			if(file_out->file.format_funcs->get_grp_att_names != NULL) {
				name_list2 = (*file_out->file.format_funcs->get_grp_att_names)(file_out->file.private_rec,name_list[i],&n_names2);
				for(j = 0; j<n_names2; j++) {
					AddAttInfoToList(&(file_out->file.grp_att_info[i]),
						(*file_out->file.format_funcs->get_grp_att_info)(file_out->file.private_rec,name_list[i],name_list2[j]));
				}
				NclFree((void*)name_list2);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Can not access variable attributes for the file format");
			}
		}
		NclFree((void*)name_list);
	}

	if(file_out->file.format_funcs->get_var_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_var_names)(file_out->file.private_rec,&n_names);

		_NclReallocFilePart(&(file_out->file), file_out->file.n_grps + 1, n_names, -1, -1);
		file_out->file.n_vars = n_names;

		for(i = 0; i < n_names; i++){
			file_out->file.var_info[i] = (*file_out->file.format_funcs->get_var_info)(file_out->file.private_rec,name_list[i]);
			if(file_out->file.format_funcs->get_var_att_names != NULL) {
				name_list2 = (*file_out->file.format_funcs->get_var_att_names)(file_out->file.private_rec,name_list[i],&n_names2);
				for(j = 0; j<n_names2; j++) {
					AddAttInfoToList(&(file_out->file.var_att_info[i]),
						(*file_out->file.format_funcs->get_var_att_info)(file_out->file.private_rec,name_list[i],name_list2[j]));
				}
				NclFree((void*)name_list2);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Can not access variable attributes for the file format");
			}
		}
		NclFree((void*)name_list);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not get variable names for file (%s), can't add file",NrmQuarkToString(the_real_path));
		if(file_out_free) 
			NclFree((void*)file_out);
		return(NULL);
	}
	if(file_out->file.format_funcs->get_dim_names!= NULL) {
		name_list = (*file_out->file.format_funcs->get_dim_names)(file_out->file.private_rec,&n_names);

		_NclReallocFilePart(&(file_out->file), -1, -1, n_names, -1);
		file_out->file.n_file_dims = n_names;

		for(i = 0; i < n_names; i++){
			file_out->file.file_dim_info[i] = (*file_out->file.format_funcs->get_dim_info)(file_out->file.private_rec,name_list[i]);
			index = FileIsVar(file_out,name_list[i]);
			if(index > -1 && file_out->file.var_info[index]->num_dimensions == 1 &&
			   file_out->file.var_info[index]->file_dim_num[0] == i) {
				file_out->file.coord_vars[i] = file_out->file.var_info[index];
			}
		}
		NclFree((void*)name_list);
	} else {
/*
* Need code to free already allocated information
*/
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not get dimension names for file (%s), can't add file",NrmQuarkToString(the_real_path));
		if(file_out_free) 
			NclFree((void*)file_out);
	
		return(NULL);
	}
	if(file_out->file.format_funcs->get_att_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_att_names)(file_out->file.private_rec,&n_names);

		_NclReallocFilePart(&(file_out->file), -1, -1, -1, n_names);
		file_out->file.n_file_atts = n_names;

		for(i = 0; i < n_names; i++) {
			file_out->file.file_atts[i] = (*file_out->file.format_funcs->get_att_info)(file_out->file.private_rec,name_list[i]);
		}
		NclFree((void*)name_list);
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not get attribute names for file (%s), no attributes added ",NrmQuarkToString(the_real_path));
	}
	(void)_NclObjCreate((NclObj)file_out,class_ptr,obj_type,(obj_type_mask | Ncl_File),status);
	if(class_ptr == nclFileClass){
		_NclCallCallBacks((NclObj)file_out,CREATED);
	}
	return(file_out);
}

static NhlErrorTypes MyFileWriteVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr,NclQuark *dim_names,int type)
#else 
(thefile, var, value, sel_ptr,dim_names,type)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
NclQuark *dim_names;
int type;
#endif
{
	NclObjTypes lhs_type,rhs_type;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData mis_md = NULL;
	NclQuark new_dim_quarks[NCL_MAX_DIMENSIONS];
	ng_size_t 	new_dim_sizes[NCL_MAX_DIMENSIONS];
	
	int has_missing = 0;
	int update_unlimited = 0;
	char buffer[8];
	void *val;
	NhlErrorTypes ret = NhlNOERROR;
	int index,dindex;
	long start[NCL_MAX_DIMENSIONS];
	long finish[NCL_MAX_DIMENSIONS];
	long stride[NCL_MAX_DIMENSIONS];
	long real_stride[NCL_MAX_DIMENSIONS];
	int i,j,k,done = 0,inc_done = 0;
	int n_dims_target;
	ng_size_t n_elem = 1;
	int n_dims_selection;
	ng_size_t total_elements = 1;
	int has_vectors = 0;
	int has_stride = 0;
	int has_reverse = 0;
	int has_reorder = 0;
	ng_size_t from = 0,n_elem_block;
	int block_write_limit;
	NclFDimRec *tmpfdim = NULL;
	
	int multiplier_target[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	long current_index[NCL_MAX_DIMENSIONS];
	long current_finish[NCL_MAX_DIMENSIONS];
	int keeper[NCL_MAX_DIMENSIONS];
	int index_map[NCL_MAX_DIMENSIONS];
	ng_size_t selection_dim_sizes[NCL_MAX_DIMENSIONS];
	NclSelection *sel;
	float tmpf;
	NclScalar *tmp_mis;
	NclScalar tmp_scalar;
	ng_size_t tmp_size = 1;
	long tmpi;
	void *data_type;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	int result = 0;
	int free_tmp_md = 0;

	if(thefile->file.wr_status <= 0) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			n_dims_target = thefile->file.var_info[index]->num_dimensions;
			if(sel_ptr != NULL) {
				sel = sel_ptr->selection;
				for(i = 0; i < n_dims_target; i++) {
					switch(sel->sel_type) {
					case Ncl_SUB_ALL:
						start[sel->dim_num] = 0;
					case Ncl_SUB_VAL_DEF:
						if(sel->sel_type == Ncl_SUB_VAL_DEF) {
							start[sel->dim_num] = sel->u.sub.start;
						}
						finish[sel->dim_num] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1;
					case Ncl_SUB_DEF_VAL:
						if(sel->sel_type == Ncl_SUB_DEF_VAL) {
							start[sel->dim_num] = 0;
							finish[sel->dim_num] = sel->u.sub.finish;
						} 
					case Ncl_SUBSCR:
						if(sel->u.sub.is_single) {
							keeper[sel->dim_num] = 0;
						} else {
							keeper[sel->dim_num] = 1;
						}
						if(sel->sel_type == Ncl_SUBSCR) {
							start[sel->dim_num] = sel->u.sub.start;
							finish[sel->dim_num] = sel->u.sub.finish;
							stride[sel->dim_num] = sel->u.sub.stride;
						} else {
							stride[sel->dim_num] = sel->u.sub.stride;
						}
						if(finish[sel->dim_num] < start[sel->dim_num]) {
							if(stride[sel->dim_num] < 0) {
								tmpi = finish[sel->dim_num] + (start[sel->dim_num] - finish[sel->dim_num]) % labs(stride[sel->dim_num]);
								finish[sel->dim_num] = start[sel->dim_num];
								start[sel->dim_num] = tmpi;
								compare_sel[sel->dim_num] = NCLFILE_INC;
								stride[sel->dim_num] = -(stride[sel->dim_num]); 
							} else {
								compare_sel[sel->dim_num] = NCLFILE_DEC;
								stride[sel->dim_num] = -(stride[sel->dim_num]); 
								has_reverse = 1;
							}
						} else {
							if(stride[sel->dim_num] < 0) {
								has_reverse = 1;
								tmpi = finish[sel->dim_num] - (finish[sel->dim_num] - start[sel->dim_num]) % labs(stride[sel->dim_num]);
								finish[sel->dim_num] = start[sel->dim_num];
								start[sel->dim_num] = tmpi;
								compare_sel[sel->dim_num] = NCLFILE_DEC;
								stride[sel->dim_num] = (stride[sel->dim_num]);
							} else {
								compare_sel[sel->dim_num] = NCLFILE_INC;
								stride[sel->dim_num] = (stride[sel->dim_num]);
							}
						}

						if(labs(stride[sel->dim_num]) > 1) {
							has_stride = 1;
						}
						if(stride[sel->dim_num] != 0)  {
							tmpi = labs(sel->u.sub.stride);
						} else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
							stride[sel->dim_num] = 1;
							tmpf = 1;
						}
						n_elem = labs((finish[sel->dim_num] -start[sel->dim_num]) / tmpi) + 1;

						if((sel->u.sub.start > thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1 )||(sel->u.sub.start < 0)) {
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.sub.start < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.sub.start >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
                                		}
                                		if((sel->u.sub.finish> thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size-1)||(sel->u.sub.finish < 0)) {
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.sub.finish < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.sub.finish >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
                                		}
						if(sel->dim_num != i) {
							has_reorder = 1;
						}
						index_map[i] = sel->dim_num;
						break;
					case Ncl_VECSUBSCR:
						keeper[sel->dim_num] = 0;
						if((sel->u.vec.min < 0 ) || (sel->u.vec.min >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)){
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.vec.min < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Vector subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.vec.min >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
						}
						if((sel->u.vec.max < 0)||(sel->u.vec.max >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size)) {
							if(!( thefile->file.file_dim_info[ thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->is_unlimited)||(sel->u.vec.max < 0)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Vector subscript out of range, error in subscript #%d",i);
                                        			return(NhlFATAL);
							} else if(sel->u.vec.max >= thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[sel->dim_num]]->dim_size){
								update_unlimited = 1;
							}
						}
						n_elem = sel->u.vec.n_ind;
						stride[sel->dim_num] = 0;
						start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
						has_vectors = 1;
						index_map[i] = sel->dim_num;
						if(sel->dim_num != i) {
							has_reorder = 1;
						}
						compare_sel[sel->dim_num] = NCLFILE_VEC;
						break;
					}
					multiplier_target[sel->dim_num] = 1;
					if(sel->dim_num != n_dims_target - 1) {
						for(k = sel->dim_num +1 ; k< n_dims_target; k++) {
							multiplier_target[sel->dim_num] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
						}
					}
					selection_dim_sizes[i] =n_elem;
					total_elements = total_elements * n_elem;
					sel++;
				}
				sel = sel_ptr->selection;
			} else {
				for(i = 0 ; i < n_dims_target; i++) {
					keeper[i] = 1;
					start[i] = 0;
					if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->is_unlimited) {
						update_unlimited = 1;
						if(value->multidval.dim_sizes[i]> thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size) {
							finish[i] = value->multidval.dim_sizes[i] -1;
						} else {
							finish[i] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size -1;
						
						}
					} else {
						finish[i] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[i]]->dim_size -1;
					}
					stride[i] = 1;
					index_map[i] = i;
					total_elements *= (finish[i] + 1);
					selection_dim_sizes[i] = (finish[i]+ 1);
					compare_sel[i] = NCLFILE_INC;
					multiplier_target[i] = 1;
					for(k = i + 1; k < n_dims_target; k++) {
						multiplier_target[i] *= (long)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
					}
				}
				sel = NULL;
			}
			n_dims_selection = n_dims_target;		
			i = 0;
			while((i < n_dims_selection)&&(n_dims_selection > 1)) {
				if((selection_dim_sizes[i] == 1)&&!(keeper[i])) {
					for(j = i ; j < n_dims_selection -1;j++) {
						selection_dim_sizes[j] = selection_dim_sizes[j+1];
						keeper[j] = keeper[j+1];
					}
					n_dims_selection--;
				} else {
					i++;
				}
			}
			if(value->multidval.kind != SCALAR) {
				for(i = 0, j = 0; i< n_dims_selection; i++) {
					if (selection_dim_sizes[i] == 1 && value->multidval.dim_sizes[j] != 1)
						continue;
					else if (selection_dim_sizes[i] != 1 && value->multidval.dim_sizes[j] == 1) {
						while (value->multidval.dim_sizes[j] == 1) 
							j++;
					}
					if(selection_dim_sizes[i] != value->multidval.dim_sizes[j]) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension sizes of left hand side do not match right hand side");
						return(NhlFATAL);
					}
					j++;
				}
			} 
			lhs_type = _NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type);
	
			rhs_type = value->multidval.type->type_class.type ;

			has_missing = (FileIsVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT)) > -1 ? 1 :  0);

			if(lhs_type != rhs_type) {
				if(has_missing) {
					mis_md = FileReadVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
					tmp_md = _NclCoerceData(value,lhs_type,(NclScalar*)mis_md->multidval.val);
				} else {
					tmp_md = _NclCoerceData(value,lhs_type,NULL);
				}
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVar: Type mismatch, can't perform assignment");
				}
			} else {
				if((has_missing)&&(value->multidval.missing_value.has_missing)) {
					mis_md = FileReadVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
					_Ncleq(value->multidval.type,(void*)&(result),(void*)&(value->multidval.missing_value.value),(void*)(mis_md->multidval.val),NULL,NULL,1,1);
					if (result) {
						tmp_md = value;
					}
					else if(value->obj.status != PERMANENT) {
						tmp_md = value;
						memcpy(&tmp_scalar,mis_md->multidval.val,mis_md->multidval.totalsize);
						_NclResetMissingValue(tmp_md,(NclScalar*) &tmp_scalar);
					} else {

/* Situation where missing values are not equal and can't just overwrite input's*/

						memcpy(&tmp_scalar,mis_md->multidval.val,mis_md->multidval.totalsize);
						tmp_md = _NclCopyVal(value,&tmp_scalar);
						free_tmp_md = 1;
					}
				} else if(value->multidval.missing_value.has_missing) {
					tmp_mis = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
					*tmp_mis = value->multidval.missing_value.value;
					mis_md = _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_mis,
						NULL,
						1,
						&tmp_size,
						TEMPORARY,
						NULL,
						_NclTypeEnumToTypeClass(lhs_type));
					FileWriteVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),mis_md,NULL);
					tmp_md = value;
				} else {
					tmp_md = value;
				}
			}
			if(tmp_md == NULL) {
				return(NhlFATAL);
			}
			if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var != NULL:thefile->file.format_funcs->write_coord != NULL ) {
				if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(value->multidval.kind != SCALAR)) {
					if(type == FILE_VAR_ACCESS) {
						ret = (*thefile->file.format_funcs->write_var)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish,	
							stride);
					} else {
						ret = (*thefile->file.format_funcs->write_coord)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish,	
							stride);
					}
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}


					return(ret);
				} else {
					if(value->multidval.kind != SCALAR) {
						val = tmp_md->multidval.val;
						from = 0;
						block_write_limit = n_dims_target -1;
						for(i = n_dims_target - 1; i >= 0; i--) {
							if((compare_sel[index_map[i]] != NCLFILE_INC)||(index_map[i] != i)) {
								block_write_limit = i;
								break;
							}
						}
					} else {
						block_write_limit = n_dims_target -1;
						val = tmp_md->multidval.val;
						from = 0;
					}
					n_elem_block = 1;
					for(i = 0; i < n_dims_target; i++) {
						current_index[index_map[i]] = start[index_map[i]];
						if(i > block_write_limit) {
							n_elem_block *= selection_dim_sizes[i];
							current_finish[i] = finish[i];
							real_stride[i] = labs(stride[i]);
						} else {
							current_finish[index_map[i]] = current_index[index_map[i]];
							real_stride[index_map[i]] = 1;
						}
					}
					while(!done) {
						if(type == FILE_VAR_ACCESS) {
							ret = (*thefile->file.format_funcs->write_var) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish,
								real_stride);
						} else {
							ret = (*thefile->file.format_funcs->write_coord) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish,
								real_stride);
						}
						if(ret < NhlWARNING) {
							if(free_tmp_md) {
								_NclDestroyObj((NclObj)tmp_md);
							}
							return(ret);
						}
						if(value->multidval.kind != SCALAR) {
							from += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
						}
						if(compare_sel[index_map[block_write_limit]] < 0) {
							current_index[index_map[block_write_limit]] += stride[index_map[block_write_limit]];
							current_finish[index_map[block_write_limit]] = current_index[index_map[block_write_limit]];
						} else {
							compare_sel[index_map[block_write_limit]]++;
						}
						for( i = block_write_limit; i > 0 ; i--) {
							switch(compare_sel[index_map[i]]) {
							case NCLFILE_INC:
								if(current_index[index_map[i]] > finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										current_index[index_map[i-1]]++;
									}
								} else {
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							case NCLFILE_DEC:
								if(current_index[index_map[i]] < finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									inc_done =1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							default:
								if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
									compare_sel[index_map[i]] = 0;
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
									if(compare_sel[index_map[i-1]] < 0 ) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							}
							if(inc_done) {
								inc_done = 0;
								break;
							}
						}
						switch(compare_sel[index_map[0]]) {
						case NCLFILE_DEC:
							if(current_index[index_map[0]] < finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						case NCLFILE_INC:
							if(current_index[index_map[0]] > finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						default:
							if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
							} else {
								current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
							}
							current_finish[index_map[0]] = current_index[index_map[0]];
						}
					}	
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}

					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}
					return(ret);
				}
			} else if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var_ns != NULL : thefile->file.format_funcs->write_coord_ns != NULL) {
				if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(!has_stride)) {	
					if(type == FILE_VAR_ACCESS) {
						ret = (*thefile->file.format_funcs->write_var_ns)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish
							);
					} else {
						ret = (*thefile->file.format_funcs->write_coord_ns)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish
							);
					}
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}

					return(ret);
				}else{
/*
* Need code here
*/
					if(value->multidval.kind != SCALAR) {
						val = tmp_md->multidval.val;
						from = 0;
						block_write_limit = n_dims_target -1;
						for(i = n_dims_target - 1; i >= 0; i--) {
							if((compare_sel[index_map[i]] != NCLFILE_INC)||(stride[index_map[i]] != 1)||(index_map[i] != i)) {
								block_write_limit = i;
								break;
							}
						}
					} else {
						block_write_limit = n_dims_target -1;
						val = tmp_md->multidval.val;
						from = 0;
					}
					n_elem_block = 1;
					for(i = 0; i < n_dims_target; i++) {
						current_index[index_map[i]] = start[index_map[i]];
						if(i > block_write_limit) {
							n_elem_block *= selection_dim_sizes[i];
							current_finish[i] = finish[i];
							real_stride[i] = stride[i];
						} else {
							current_finish[index_map[i]] = current_index[index_map[i]];
							real_stride[index_map[i]] = 1;
						}
					}
					while(!done) {
						if(type == FILE_VAR_ACCESS) {
							ret = (*thefile->file.format_funcs->write_var_ns) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish
								);
						} else {
							ret = (*thefile->file.format_funcs->write_coord_ns) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish
								);
						}
						if(ret < NhlWARNING) {
							if(free_tmp_md) {
								_NclDestroyObj((NclObj)tmp_md);
							}
							return(ret);
						}
						if(value->multidval.kind != SCALAR) {
							from += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
						}
						if(compare_sel[index_map[block_write_limit]] < 0) {
							current_index[index_map[block_write_limit]] += stride[index_map[block_write_limit]];
							current_finish[index_map[block_write_limit]] = current_index[index_map[block_write_limit]];
						} else {
							compare_sel[index_map[block_write_limit]]++;
						}
						for( i = block_write_limit; i > 0 ; i--) {
							switch(compare_sel[index_map[i]]) {
							case NCLFILE_INC:
								if(current_index[index_map[i]] > finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										current_index[index_map[i-1]]++;
									}
								} else {
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							case NCLFILE_DEC:
								if(current_index[index_map[i]] < finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									inc_done =1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							default:
								if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
									compare_sel[index_map[i]] = 0;
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
									if(compare_sel[index_map[i-1]] < 0 ) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							}
							if(inc_done) {
								inc_done = 0;
								break;
							}
						}
						switch(compare_sel[index_map[0]]) {
						case NCLFILE_DEC:
							if(current_index[index_map[0]] < finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						case NCLFILE_INC:
							if(current_index[index_map[0]] > finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						default:
							if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
							} else {
								current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
							}
							current_finish[index_map[0]] = current_index[index_map[0]];
						}
					}	
/*
* Add unlimited update
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
					if(free_tmp_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}
					return(ret);
				}
			}
		} else {
/*
* Need to add variable to file situation
*/
			if(type == FILE_COORD_VAR_ACCESS) {
				if((dindex = FileIsDim(thefile,var)) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension in file (%s), can not add coordinate variable",NrmQuarkToString(var),NrmQuarkToString(thefile->file.fpath));
					return(NhlFATAL);
				} else if((thefile->file.file_dim_info[dindex]->dim_size == value->multidval.dim_sizes[0])||(thefile->file.file_dim_info[dindex]->is_unlimited)) {
					if(value->multidval.n_dims != 1) {
						NHLPERROR((NhlFATAL,NhlEUNKNOWN,"FILE_COORD_VAR_ACCESS. file: %s, line: %d\n", __FILE__, __LINE__));
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables must be single dimension arrays, attempt to assign (%d) dimension value to coordinate variable",value->multidval.n_dims); 
						return(NhlFATAL);
					}
					new_dim_quarks[0] = var;
					new_dim_sizes[0] = value->multidval.dim_sizes[0];
					start[0] = 0;
					finish[0] = value->multidval.dim_sizes[0] -1 ;
					stride[0] = 1;
					if(thefile->file.file_dim_info[dindex]->is_unlimited) {
						update_unlimited = 1;
					}
				}
			} else {
/*
* Since it is imposible to guess names of dimensions they must
* blindly be added
*/
				if(dim_names == NULL) {
					for(i = 0; i < value->multidval.n_dims; i++) {
						sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
						new_dim_quarks[i] = NrmStringToQuark(buffer);
						new_dim_sizes[i] = (long)value->multidval.dim_sizes[i];
						start[i] = 0;
						finish[i] = value->multidval.dim_sizes[i] -1;
						stride[i] = 1;

						ret = (*thefile->file.format_funcs->add_dim)(
							thefile->file.private_rec,
							new_dim_quarks[i],
							new_dim_sizes[i],
							0);
						if(ret < NhlWARNING) {
							return(ret);
						}

						if(thefile->file.n_file_dims >= thefile->file.max_file_dims)
						{
							_NclReallocFilePart(&(thefile->file), -1, -1, thefile->file.n_file_dims, -1);
						}

						thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,new_dim_quarks[i]);
						thefile->file.n_file_dims++;
				
					}
				} else {
					for(i = 0 ; i < value->multidval.n_dims; i++) {
						if(dim_names[i] != -1) {
							new_dim_quarks[i] = dim_names[i];
						} else {
							sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
							new_dim_quarks[i] = NrmStringToQuark(buffer);
						}
						new_dim_sizes[i] = value->multidval.dim_sizes[i];
						start[i] = 0;
						finish[i] = value->multidval.dim_sizes[i] - 1;
						stride[i] = 1;
						if((dindex = FileIsDim(thefile,dim_names[i])) == -1) {
							ret = (*thefile->file.format_funcs->add_dim)(
								thefile->file.private_rec,
								new_dim_quarks[i],
								new_dim_sizes[i],
								0);
							if(ret < NhlWARNING) {
								return(ret);
							}
							if (value->multidval.n_dims == 1 && new_dim_quarks[i] == NrmStringToQuark("ncl_scalar")) {
								AdjustForScalarDim(thefile);
							}
							else {
								if(thefile->file.n_file_dims >= thefile->file.max_file_dims)
								{
									_NclReallocFilePart(&(thefile->file), -1, -1, thefile->file.n_file_dims, -1);
								}

								thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,new_dim_quarks[i]);
								thefile->file.n_file_dims++;
							}
						} else {
							if((thefile->file.file_dim_info[dindex]->dim_size != value->multidval.dim_sizes[i])&&(!(thefile->file.file_dim_info[dindex]->is_unlimited))) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"File dimension conflict, dimension (%s) has a size of (%d) can not set it to requested size (%d)",NrmQuarkToString(dim_names[i]),thefile->file.file_dim_info[dindex]->dim_size,value->multidval.dim_sizes[i]);
								return(NhlFATAL);
			
							}
							if(thefile->file.file_dim_info[dindex]->is_unlimited) {
								update_unlimited = 1;
							}
						}
					}
				}
			}
/*
* Make sure data can be written
*/
			data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type);
			if(data_type == NULL) {
				from_type = value->multidval.data_type;
				to_type = _NclPromoteType(from_type);
				while((from_type != to_type )&&((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)) {
					from_type = to_type;
					to_type = _NclPromoteType(from_type);
				}
				if(data_type != NULL) {
					NclFree(data_type);
				}
				obj_type = _NclBasicDataTypeToObjType(to_type);
				tmp_md = _NclCoerceData(value,obj_type,NULL);
				if((tmp_md == NULL)||( to_type == value->multidval.data_type)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempting to write variable (%s) of type (%s) which is not representable in the format of file (%s)",
						NrmQuarkToString(var),
						_NclBasicDataTypeToName(value->multidval.data_type),
						NrmQuarkToString(thefile->file.fpath));
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Trying using a type conversion function");
					return(NhlFATAL);
				} else {
					ret = (*thefile->file.format_funcs->add_var)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.data_type,
						tmp_md->multidval.n_dims,
						new_dim_quarks,
						new_dim_sizes
					);
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
				}
			} else {

				ret = (*thefile->file.format_funcs->add_var)(
					thefile->file.private_rec,
					var,
					value->multidval.data_type,
					value->multidval.n_dims,
					new_dim_quarks,
					new_dim_sizes
				);
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
				tmp_md = value;
				NclFree(data_type);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
			if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var != NULL : thefile->file.format_funcs->write_coord != NULL) {
				if(type == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->write_var)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish,
						stride);
				} else {
					ret = (*thefile->file.format_funcs->write_coord)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish,
						stride);
				}
				if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
					_NclDestroyObj((NclObj)tmp_md);
			} else {
				if(type == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->write_var_ns)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish);
				} else {
					ret = (*thefile->file.format_funcs->write_coord_ns)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish);
				}
				if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
					_NclDestroyObj((NclObj)tmp_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
			if(thefile->file.n_vars >= thefile->file.max_vars)
			{
				_NclReallocFilePart(&(thefile->file), -1, thefile->file.n_vars, -1, -1);
			}

			thefile->file.var_info[thefile->file.n_vars] = (*thefile->file.format_funcs->get_var_info)(thefile->file.private_rec,var);
			thefile->file.var_att_info[thefile->file.n_vars] = NULL;
			thefile->file.var_att_ids[thefile->file.n_vars] = -1;
			
			thefile->file.n_vars++;
/*
* Add update unlimited
*/
					if(update_unlimited) {
						for(i = 0; i < thefile->file.n_file_dims;i++) {
							if(thefile->file.file_dim_info[i]->is_unlimited) {
								tmpfdim= thefile->file.file_dim_info[i];
								thefile->file.file_dim_info[i] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,tmpfdim->dim_name_quark);
								NclFree(tmpfdim);
							}
						}
					}
			return(NhlNOERROR);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}
static NhlErrorTypes FileWriteCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, value, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	int dindex;
	int index;

	if(thefile->file.wr_status<=0) {
		dindex = FileIsDim(thefile,coord_name);
		if(dindex > -1) {
			ret = MyFileWriteVar(thefile,coord_name,value,sel_ptr,NULL,FILE_COORD_VAR_ACCESS);
			if(thefile->file.coord_vars[dindex] == NULL) {
				index = FileIsVar(thefile,coord_name);
				if(index > -1 && thefile->file.var_info[index]->num_dimensions == 1) {
					thefile->file.coord_vars[dindex] = thefile->file.var_info[index];
				} 
			}	
			return(ret);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a valid dimension in file (%s), can't write coord_var",NrmQuarkToString(coord_name),NrmQuarkToString(thefile->file.fname));
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteCoord: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}

static NhlErrorTypes FileWriteVarVar
#if	NhlNeedProto
(NclFile thefile, NclQuark lhs_var, struct _NclSelectionRecord * lhs_sel_ptr, struct _NclVarRec* rhs_var, struct _NclSelectionRecord *rhs_sel_ptr)
#else
(thefile , lhs_var, lhs_sel_ptr, rhs_var, rhs_sel_ptr)
NclFile thefile;
NclQuark lhs_var;
struct _NclSelectionRecord *lhs_sel_ptr;
struct _NclVarRec* rhs_var;
struct _NclSelectionRecord *rhs_sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	struct _NclVarRec* tmp_var;
	struct _NclVarRec* tmp_coord_var;
	int i,j,m;
	NclQuark dim_names[NCL_MAX_DIMENSIONS];
	NclAtt theatt;
	NclAttList *step;
	int index,cindex;
	ng_size_t lhs_n_elem;	
	NclSelectionRecord tmp_sel;
        void *tmp_coord;
        char *tmp_ptr;
	NclMultiDValData tmp_md;
	NclMultiDValData coord_md;
	struct _NclVarRec* cvar;
	ng_size_t dimsize = -1;

	tmp_sel.n_entries = 1;
	tmp_sel.selected_from_sym = NULL;
	tmp_sel.selected_from_var = NULL;

	if(thefile->file.wr_status<=0) {
		tmp_var = _NclVarRead(rhs_var,rhs_sel_ptr);
		if (! tmp_var) {
			return NhlFATAL;
		}
		tmp_md = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
		if (! tmp_md) {
			return NhlFATAL;
		}
		for ( i = 0; i < tmp_var->var.n_dims; i++) {
			dim_names[i] = tmp_var->var.dim_info[i].dim_quark;
			if (dim_names[i] == NrmStringToQuark("ncl_scalar"))
				continue;
			if (dim_names[i] > 0) {
				if (FileIsDim(thefile,dim_names[i]) == -1) {
					ret = FileAddDim(thefile,dim_names[i],tmp_var->var.dim_info[i].dim_size,False);
				}
			}
			else {
				char buffer[32];
				sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
				ret = FileAddDim(thefile,NrmStringToQuark(buffer),tmp_var->var.dim_info[i].dim_size,False);
				dim_names[i] = NrmStringToQuark(buffer);
			}
		}
		index = FileIsVar(thefile,lhs_var);
		if (index < 0) {
			ret = FileAddVar(thefile,lhs_var,
					 NrmStringToQuark(_NclBasicDataTypeToName(tmp_md->multidval.type->type_class.data_type)),
					 tmp_var->var.n_dims,dim_names);
			if(ret < NhlWARNING) {
				return(ret);
			}
		}
		if(rhs_var->var.att_id != -1) {
			theatt = (NclAtt)_NclGetObj(rhs_var->var.att_id);
			step = theatt->att.att_list;
			while(step != NULL) {
				ret = FileWriteVarAtt(thefile,lhs_var,step->quark,step->attvalue,NULL);
				if(ret < NhlNOERROR){
					NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "FileWriteVarVar: Could not write attribute (%s) to variable (%s) in file (%s), continuing anyway",
						  NrmQuarkToString(step->quark),
						  NrmQuarkToString(lhs_var),
						  NrmQuarkToString(thefile->file.fname));
					ret = NhlWARNING;
				}
				step = step->next;
			}
		}
		ret = MyFileWriteVar(thefile,lhs_var,tmp_md,lhs_sel_ptr,dim_names,FILE_VAR_ACCESS);
		if(ret < NhlWARNING) {
			return(ret);
		}
		index = FileIsVar(thefile,lhs_var);
		if(lhs_sel_ptr != NULL) {
			j = 0;
			for(i = 0; i < lhs_sel_ptr->n_entries; i++) {
				if(!lhs_sel_ptr->selection[i].u.sub.is_single ) {
					switch(lhs_sel_ptr->selection[i].sel_type) {
					case Ncl_VECSUBSCR:
						lhs_n_elem = lhs_sel_ptr->selection[i].u.vec.n_ind;
						break;
					default:
						lhs_n_elem = (ng_size_t)labs((lhs_sel_ptr->selection[i].u.sub.finish - lhs_sel_ptr->selection[i].u.sub.start)/lhs_sel_ptr->selection[i].u.sub.stride) + 1;
						break;
					}
					if(tmp_var->var.dim_info[j].dim_quark > 0) {
						if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark != tmp_var->var.dim_info[j].dim_quark) {
/*
 * Dimnames are unequal give warning then overwrite
 */
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Dimension names of left hand side and right hand side do not match, overwriting dimension (%s), use (/ .. /) if this is not the desired result",NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark) );
							_NclFileWriteDim(thefile,thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark,thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]);

						} 
/*
 * Now dimension names are equal, proceed to write coordinate variable
 */
						if(tmp_var->var.coord_vars[j] != -1) {
							cindex = FileIsCoord(thefile,tmp_var->var.dim_info[j].dim_quark);
							if(cindex != -1) {
/*
 * Simply write coordinate using sel_ptr
 */
								tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
								tmp_sel.selection[0].dim_num = 0;
								ret = _NclFileWriteCoord(thefile,tmp_var->var.dim_info[j].dim_quark,_NclVarValueRead((NclVar)_NclGetObj(tmp_var->var.coord_vars[j]),NULL,NULL),&tmp_sel);
								cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);

							} else {
/*
 * Need to create a temporary missing value filled array and write it then make and assigment using 
 * sel_ptr
 */
								cvar = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
								tmp_md = (NclMultiDValData)_NclGetObj(cvar->var.thevalue_id);
								cindex = FileIsVar(thefile,tmp_var->var.dim_info[j].dim_quark);
								if (cindex < 0) {
									NclQuark name = tmp_var->var.dim_info[j].dim_quark;
									ret = FileAddVar(thefile,tmp_var->var.dim_info[j].dim_quark,
											 NrmStringToQuark(_NclBasicDataTypeToName(tmp_md->multidval.type->type_class.data_type)),
											 1,&name);
									if(ret < NhlWARNING) {
										return(ret);
									}
								}
								dimsize = (int)thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_size;
								if(cvar->var.att_id != -1) {
									theatt = (NclAtt)_NclGetObj(cvar->var.att_id);
									step = theatt->att.att_list;
									while(step != NULL) {
										ret = FileWriteVarAtt(thefile,tmp_var->var.dim_info[j].dim_quark,step->quark,step->attvalue,NULL);
										if(ret < NhlWARNING){
											NhlPError(NhlWARNING,NhlEUNKNOWN,
                                                "FileWriteVarVar: Could not write attribute (%s) to variable (%s) in file (%s), continuing anyway",
												  NrmQuarkToString(step->quark),
												  NrmQuarkToString(tmp_var->var.dim_info[i].dim_quark),
												  NrmQuarkToString(thefile->file.fname));
											ret = NhlWARNING;
										}
										step = step->next;
				
									}
								}
								tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
								tmp_sel.selection[0].dim_num = 0;
								ret = _NclFileWriteCoord(thefile,tmp_var->var.dim_info[j].dim_quark,tmp_md,&tmp_sel);
							}
							if(ret < NhlWARNING) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not write coordinate variable (%s) to file (%s), continuing anyway",NrmQuarkToString(tmp_var->var.dim_info[i].dim_quark),NrmQuarkToString(thefile->file.fname));
								ret = NhlWARNING;
							}
						}
				
					} else if(thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[lhs_sel_ptr->selection[i].dim_num]]->dim_name_quark > 0) {

/*
 * right hand side has no dimension name and hence no coordinate variable so give warning and proceed
 */
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Right hand side has no dimension name can not delete dimension of a file, use (/ .. /) to avoid this message");
					}
					j++;
				} 
			} 
		} else {
			for(i = 0, j = 0; i < thefile->file.var_info[index]->num_dimensions; i++) {
				int file_dim_num = thefile->file.var_info[index]->file_dim_num[i];

				if (thefile->file.file_dim_info[file_dim_num]->dim_size  == 1 && tmp_var->var.dim_info[j].dim_size != 1) 
					continue;
				else if (thefile->file.file_dim_info[file_dim_num]->dim_size != 1 && tmp_var->var.dim_info[j].dim_size == 1) {
					while (tmp_var->var.dim_info[j].dim_size == 1)
						j++;
				}

				if(tmp_var->var.dim_info[j].dim_quark > 0) {
					if(thefile->file.file_dim_info[file_dim_num]->dim_name_quark != tmp_var->var.dim_info[j].dim_quark) {
/*
 * Dimnames are unequal give warning then overwrite
 */
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Dimension names of left hand side and right hand side do not match, overwriting dimension (%s), use (/ .. /) if this is not the desired result",NrmQuarkToString(thefile->file.file_dim_info[file_dim_num]->dim_name_quark) );
						_NclFileWriteDim(thefile,tmp_var->var.dim_info[j].dim_quark,file_dim_num);

					} 
					if(tmp_var->var.coord_vars[j] != -1) {
						if (tmp_var->var.dim_info[j].dim_quark == NrmStringToQuark("ncl_scalar")) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Variable (%s) has coordinate variable named \"ncl_scalar\"; not writing coodinate variable to file (%s)",
								  NrmQuarkToString(tmp_var->var.var_quark),NrmQuarkToString(thefile->file.fname));
							ret = NhlWARNING;
							continue;
						}
						tmp_coord_var = (NclVar)_NclGetObj(tmp_var->var.coord_vars[j]);
						cindex = FileIsVar(thefile,tmp_var->var.dim_info[j].dim_quark);
						coord_md = _NclVarValueRead(tmp_coord_var,NULL,NULL);
						if (cindex < 0) {
							NclQuark name = tmp_var->var.dim_info[j].dim_quark;
							ret = FileAddVar(thefile,tmp_var->var.dim_info[j].dim_quark,
									 NrmStringToQuark(_NclBasicDataTypeToName(coord_md->multidval.type->type_class.data_type)),
									 1,&name);
							if(ret < NhlWARNING) {
								return(ret);
							}
						}
						if(tmp_coord_var->var.att_id != -1) {
							theatt = (NclAtt)_NclGetObj(tmp_coord_var->var.att_id);
							step = theatt->att.att_list;
							while(step != NULL) {
								ret = FileWriteVarAtt(thefile,tmp_var->var.dim_info[j].dim_quark,step->quark,step->attvalue,NULL);
								if(ret < NhlWARNING){
									NhlPError(NhlWARNING,NhlEUNKNOWN,
										  "FileWriteVarVar: Could not write attribute (%s) to variable (%s) in file (%s), continuing anyway",
										  NrmQuarkToString(step->quark),
										  NrmQuarkToString(tmp_var->var.dim_info[j].dim_quark),
										  NrmQuarkToString(thefile->file.fname));
									ret = NhlWARNING;
								}
								step = step->next;
			
							}
						}
						ret = FileWriteCoord(thefile,tmp_var->var.dim_info[j].dim_quark,coord_md,NULL);
						if(ret < NhlWARNING) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"FileWriteVarVar: Could not write coordinate variable (%s) to file (%s), continuing anyway",
								  NrmQuarkToString(tmp_var->var.dim_info[j].dim_quark),NrmQuarkToString(thefile->file.fname));
							ret = NhlWARNING;
						} 
					} else if(thefile->file.coord_vars[file_dim_num] != NULL) {

/*
 * right hand side has no dimension name and hence no coordinate variable so give warning and proceed
 */
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Right hand side has no coordinate variable can not delete coordinate variable of a file, use (/ .. /) to avoid this message");
						ret = NhlWARNING;
					}
				}
				j++;
			}
		}
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVarVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}


static NhlErrorTypes FileWriteVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, value, sel_ptr)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	return(MyFileWriteVar(thefile,var,value,sel_ptr,NULL,FILE_VAR_ACCESS));
}


static NhlErrorTypes FileWriteVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname,struct _NclMultiDValDataRec* value, struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, attname,value, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	int exists;
	NclMultiDValData tmp_att_md,tmp_md,last_att_val_md;
	int att_id;
	NhlErrorTypes ret = NhlNOERROR;
	int index = -1;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	void *data_type;

	if(thefile->file.wr_status<=0) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			if(thefile->file.var_att_ids[index] == -1) 
				LoadVarAtts(thefile,var);

			att_id = thefile->file.var_att_ids[index];
/*
 * Hereis the trick. It is easier to let the _NclAddAtt... functions deal
 * with the coercion than to figure out what it should be 
 */
			exists = _NclIsAtt(att_id,NrmQuarkToString(attname));

			if((exists)&&(thefile->file.format_funcs->write_att != NULL))  {
				/* get the last att val in case there's an error writing the att */
				last_att_val_md = _NclCopyVal(_NclGetAtt(att_id,NrmQuarkToString(attname),NULL),NULL);
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
				if(ret < NhlWARNING) {
                                	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString( attname));
                                	return(NhlFATAL);
                        	}
				tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
				ret = (*thefile->file.format_funcs->write_var_att)(
					thefile->file.private_rec,
					var,
					attname,
					tmp_att_md->multidval.val
					);
				if (ret < NhlNOERROR) {
					ret = MIN(ret,_NclAddAtt(att_id,NrmQuarkToString(attname),last_att_val_md,NULL));
				}
				else {
					_NclDestroyObj((NclObj)last_att_val_md);
				}
				return(ret);
			} else if((!exists)&&(thefile->file.format_funcs->add_att != NULL)){
				if(value->multidval.data_type == NCL_char) {	
					if (attname != NrmStringToQuark(NCL_MISSING_VALUE_ATT))
						tmp_md = _NclCharMdToStringMd(value);
					else
						tmp_md = value;
					ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
					if(ret < NhlWARNING) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list",NrmQuarkToString(attname));
						return(NhlFATAL);
					}
					ret = (*thefile->file.format_funcs->add_var_att)(
						thefile->file.private_rec,
						var,
						attname,
						value->multidval.data_type,
						value->multidval.totalelements,
						value->multidval.val
						);
					if(ret > NhlFATAL) {
						AddAttInfoToList(&(thefile->file.var_att_info[index]), (*thefile->file.format_funcs->get_var_att_info)(thefile->file.private_rec,var,attname));
					}
					if (ret < NhlNOERROR) {
						_NclDeleteAtt(att_id,NrmQuarkToString(attname));
					}
				} else {
					data_type = (void *)(*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type);
					if(data_type == NULL) {
						if(value->multidval.data_type == NCL_string) {
							tmp_md = _NclStringMdToCharMd(value);
							/* 
							 * simple hack to get rid of the null terminator, which should not be written to the output file
							 */
							tmp_md->multidval.totalelements--;
							tmp_md->multidval.totalsize--;
							tmp_md->multidval.dim_sizes[0]--;
							/*
							 * end hack
							 */
							ret = _NclFileWriteVarAtt(thefile,var,attname,tmp_md,sel_ptr);
							_NclDestroyObj((NclObj)tmp_md);
							return(ret);
						} else {
							from_type = value->multidval.data_type;
							to_type = _NclPromoteType(from_type);
							while((from_type != to_type)&&((data_type =(*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)) {
								from_type = to_type;
								to_type = _NclPromoteType(from_type);
							}
							if(data_type != NULL) {
								NclFree(data_type);
							}
							if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"The type (%s) is not representable as an attribute in the file (%s)",_NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->file.fpath));
								return(NhlFATAL);
							} else {
								NclFree(data_type);
								obj_type = _NclBasicDataTypeToObjType(to_type);
								tmp_md = _NclCoerceData(value,obj_type,NULL);
							}
						}
						
					} else {
						NclFree(data_type);
						tmp_md = value;
					}
					ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
					if(ret < NhlWARNING) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list",NrmQuarkToString(attname));
						return(NhlFATAL);
					}
					ret = (*thefile->file.format_funcs->add_var_att)(
						thefile->file.private_rec,
						var,
						attname,
						tmp_md->multidval.data_type,
						tmp_md->multidval.totalelements,
						tmp_md->multidval.val
						);
					if(ret > NhlWARNING) {
						AddAttInfoToList(&(thefile->file.var_att_info[index]), (*thefile->file.format_funcs->get_var_att_info)(thefile->file.private_rec,var,attname));
						return(ret);
					} else {
						_NclDeleteAtt(att_id,NrmQuarkToString(attname));
					}
				}
				return(ret);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a variable in file (%s)",NrmQuarkToString(var),NrmQuarkToString(thefile->file.fname));
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVarAtt: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static struct _NclMultiDValDataRec *FileReadAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int aindex;
	int att_id = -1,i;
	void *val;
	NclMultiDValData tmp_md;
	NhlArgVal udata;
        ng_size_t ne;
	aindex = FileIsAtt(thefile,attname);
	if(aindex > -1) {
		if(thefile->file.file_atts_id != -1) {
			return(_NclGetAtt(thefile->file.file_atts_id,NrmQuarkToString(attname),sel_ptr));
		}
		if(thefile->file.format_funcs->read_att != NULL) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			for(i = 0; i < thefile->file.n_file_atts; i++){
				val = NclMalloc(_NclSizeOf(thefile->file.file_atts[i]->data_type)* thefile->file.file_atts[i]->num_elements );
				(void)(*thefile->file.format_funcs->read_att)(
					thefile->file.private_rec,
					thefile->file.file_atts[i]->att_name_quark,
					val
					);
				ne = thefile->file.file_atts[i]->num_elements;
				tmp_md = _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						val,
						NULL,
						1,
						&ne,
						TEMPORARY,
						NULL,
						_NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.file_atts[i]->data_type)));
				if(tmp_md != NULL) {
					_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_md,NULL);
				}
			}
			udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
			((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
			((FileCallBackRec*)udata.ptrval)->theattid = att_id;
			((FileCallBackRec*)udata.ptrval)->thevar = -1;
			thefile->file.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
			thefile->file.file_att_udata = (FileCallBackRec*)udata.ptrval;
			if(att_id != -1) {	
				thefile->file.file_atts_id = att_id;
				return(_NclGetAtt(thefile->file.file_atts_id,NrmQuarkToString(attname),sel_ptr));
			}
		}
	}
	NhlPError(NhlWARNING,NhlEUNKNOWN,"FileReadVarAtt: (%s) is not an attribute of (%s)",NrmQuarkToString(attname),NrmQuarkToString(thefile->file.fname));
	return(_NclCreateMissing());
}

static NhlErrorTypes FileWriteAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, value, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int i,exists;
	NclMultiDValData tmp_att_md,tmp_md;
	int att_id;
	NhlErrorTypes ret = NhlNOERROR;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	void *data_type;
	NhlArgVal udata;

	if(thefile->file.wr_status<=0) {
		if(thefile->file.file_atts_id == -1) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			for(i= 0; i < thefile->file.n_file_atts; i++) {
				tmp_att_md = FileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
				if(tmp_att_md != NULL) {
					if(tmp_att_md->obj.status == TEMPORARY){
						_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_att_md,NULL);
					} else {
						tmp_att_md = _NclCopyVal(tmp_att_md, NULL);
						_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_att_md,NULL);
					}
				}
			}
			thefile->file.file_atts_id = att_id;
			udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
			((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
			((FileCallBackRec*)udata.ptrval)->theattid = att_id;
			((FileCallBackRec*)udata.ptrval)->thevar = -1;
			thefile->file.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
			thefile->file.file_att_udata = (FileCallBackRec*)udata.ptrval;
		}  else {
			att_id = thefile->file.file_atts_id;
		}
		exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
		if((exists)&&(thefile->file.format_funcs->write_att != NULL))  {
/*
 * Hereis the trick. It is easier to let the _NclAddAtt... functions deal
 * with the coercion than to figure out what it should be 
 */
			ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
			if(ret < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
				return(NhlFATAL);
			}
			tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
			ret = (*thefile->file.format_funcs->write_att)(
				thefile->file.private_rec,
				attname,
				tmp_att_md->multidval.val
				);
			return(ret);
		} else if((!exists)&&(thefile->file.format_funcs->add_att != NULL)){
			if(value->multidval.data_type == NCL_char) {
				tmp_md = _NclCharMdToStringMd(value);
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
       	        		if(ret < NhlWARNING) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
					return(NhlFATAL);
				}
				ret = (*thefile->file.format_funcs->add_att)(
					thefile->file.private_rec,
					attname,
					value->multidval.data_type,
					value->multidval.totalelements,
					value->multidval.val
					);
				if(ret > NhlFATAL) {
					if(thefile->file.n_file_atts >= thefile->file.max_file_atts)
					{
						_NclReallocFilePart(&(thefile->file), -1, -1, -1, thefile->file.n_file_atts);
					}

					thefile->file.file_atts[thefile->file.n_file_atts] = (*thefile->file.format_funcs->get_att_info)(thefile->file.private_rec,attname);
					if(thefile->file.file_atts[thefile->file.n_file_atts] != NULL) {
						thefile->file.n_file_atts++;
					}
				}
				if (ret < NhlNOERROR) {
					_NclDeleteAtt(att_id,NrmQuarkToString(attname));
				}
			} else {
				if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type)) == NULL)  {
					if(value->multidval.data_type == NCL_string) {
						tmp_md = _NclStringMdToCharMd(value);
						/* 
						 * simple hack to get rid of the null terminator, which should not be written to the output file
						 */
						tmp_md->multidval.totalelements--;
						tmp_md->multidval.totalsize--;
						tmp_md->multidval.dim_sizes[0]--;
						/*
						 * end hack
						 */
						ret = _NclFileWriteAtt(thefile,attname,tmp_md,sel_ptr);
						_NclDestroyObj((NclObj)tmp_md);
						return(ret);
					} else {
						from_type = value->multidval.data_type;
						to_type = _NclPromoteType(from_type);
						while((from_type != to_type )&&((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)) {
							from_type = to_type;
							to_type = _NclPromoteType(from_type);
						}
						if(data_type != NULL) {
							NclFree(data_type);
						}
						if((data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(to_type))==NULL)  {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"The type (%s) is not representable as an attribute in the file (%s)",_NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->file.fpath));
							return(NhlFATAL);
	
						} else {
							NclFree(data_type);
							obj_type = _NclBasicDataTypeToObjType(to_type);
							tmp_md = _NclCoerceData(value,obj_type,NULL);
						}
					}
				} else {
					NclFree(data_type);
					tmp_md= value;
				}
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
       	        		if(ret < NhlWARNING) {
       		                 	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
       		                 	return(NhlFATAL);
       		         	}
				ret = (*thefile->file.format_funcs->add_att)(
					thefile->file.private_rec,
					attname,
					tmp_md->multidval.data_type,
					tmp_md->multidval.totalelements,
					tmp_md->multidval.val
					);
				if((tmp_md != value)&&(tmp_md->obj.status != PERMANENT)) {
					_NclDestroyObj((NclObj)tmp_md);
				}
				if(ret > NhlWARNING) {
					if(thefile->file.n_file_atts >= thefile->file.max_file_atts)
					{
						_NclReallocFilePart(&(thefile->file), -1, -1, -1, thefile->file.n_file_atts);
					}

					thefile->file.file_atts[thefile->file.n_file_atts] = (*thefile->file.format_funcs->get_att_info)(thefile->file.private_rec,attname);
					if(thefile->file.file_atts[thefile->file.n_file_atts] != NULL) {
						thefile->file.n_file_atts++;
					}
					return(ret);
				} else {
					_NclDeleteAtt(att_id,NrmQuarkToString(attname));
				}
			}
			return(ret);
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteAtt: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}


static struct _NclMultiDValDataRec* FileVarReadDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile, var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	int index;
	int i;
	int *tmpi;
	NclQuark *tmpq;
	ng_size_t output_dim_sizes = 1;
	
	
	index = FileIsVar(thefile,var);
	if(index > -1) {
		if(dim_name > -1) {
			for( i=0; i < thefile->file.var_info[index]->num_dimensions; i++) {
				if(FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[i]) == dim_name) {
					tmpi = (int*)NclMalloc(sizeof(int));
					*tmpi = i;
					return( _NclCreateMultiDVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmpi,
						NULL,
						1,
						&output_dim_sizes,
						TEMPORARY,
						NULL,
						(NclTypeClass)nclTypeintClass));
				}
			}
		} else if ( dim_num > -1) {
			if(dim_num < thefile->file.var_info[index]->num_dimensions) {
				tmpq = (NclQuark*)NclMalloc(sizeof(NclQuark));
				*tmpq = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[dim_num]);	
				return( _NclCreateMultiDVal(
					NULL,
					NULL,
					Ncl_MultiDValData,
					0,
					(void*)tmpq,
					NULL,
					1,
					&output_dim_sizes,
					TEMPORARY,
					NULL,
					(NclTypeClass)nclTypestringClass));
			}
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension number (%d) is out of range for variable (%s->%s)",dim_num,NrmQuarkToString(thefile->file.fname),NrmQuarkToString(var));
			return(NULL);
		} 
	}
	return(NULL);
}
/*
* dim_num is dim number of variable and dim_name is new name
*/
static NhlErrorTypes FileVarWriteDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile,var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	int index;
	NclQuark old_name;

	if(thefile->file.wr_status <= 0) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			if((dim_num > -1)&&(dim_num < thefile->file.var_info[index]->num_dimensions)) {
				old_name = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[dim_num]);
				if(thefile->file.format_funcs->rename_dim != NULL) {
					if((*thefile->file.format_funcs->rename_dim)(
						thefile->file.private_rec,
						old_name,
						dim_name) < NhlWARNING) {
						
						NhlPError(NhlFATAL,NhlEUNKNOWN,
							"Cannot rename dimension (%d) in variable (%s)",
							dim_num,
							NrmQuarkToString(var));
						return(NhlFATAL);
					} else {
						thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[dim_num]]->dim_name_quark = dim_name;
						return(NhlNOERROR);
					}
				}
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileVarWriteDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static struct _NclMultiDValDataRec* FileReadDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	int i = 0;
	NclQuark *tmps;
	int *tmpl;
	ng_size_t output_dim_sizes = 1;
	if(dim_name != -1) {
		for(i =0; i< thefile->file.n_file_dims; i++) {
			if(thefile->file.file_dim_info[i]->dim_name_quark) {
				tmpl = (int*)NclMalloc(sizeof(int));
				*tmpl = i;
				return( _NclCreateMultiDVal(
					NULL,
					NULL,
					Ncl_MultiDValData,
					0,
					(void*)tmpl,
					NULL,
					1,
					&output_dim_sizes,
					TEMPORARY,
					NULL,
					(NclTypeClass)nclTypeintClass));
			}
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a defined dimension in file (%s)",NrmQuarkToString(dim_name),NrmQuarkToString(thefile->file.fname));
		return(NULL);
	} else if(dim_num > -1){
		if(dim_num < thefile->file.n_file_dims) {
			tmps = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmps = thefile->file.file_dim_info[dim_num]->dim_name_quark;
			return( _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				(void*)tmps,
				NULL,
				1,
				&output_dim_sizes,
				TEMPORARY,
				NULL,
				(NclTypeClass)nclTypestringClass));
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension #%ld is out of range",dim_num);
		return(NULL);

	} else {
		return(NULL);
	}
}

/*
* dim_num is the number of the dimension dim_name is the name to change it to
*/
static NhlErrorTypes FileWriteDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	if(thefile->file.wr_status <= 0) {
		if((dim_num > -1)&&(dim_num < thefile->file.n_file_dims)) {
			if(thefile->file.format_funcs->rename_dim != NULL) {
				if((*thefile->file.format_funcs->rename_dim)(
					thefile->file.private_rec,
					thefile->file.file_dim_info[dim_num]->dim_name_quark,
					dim_name)  < NhlWARNING) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not change dimension (%d) to (%s) for file (%s)",dim_num,NrmQuarkToString(dim_name),NrmQuarkToString(thefile->file.fname));
					return(NhlFATAL);
				} else {
					thefile->file.file_dim_info[dim_num]->dim_name_quark = dim_name;
					return(NhlNOERROR);
				}
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) is out of range for file (%s)",dim_num,NrmQuarkToString(thefile->file.fname));
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteDim: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}

static struct _NclVarRec* FileReadCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclSelection *sel;
	int index;
	NclMultiDValData tmp_md;
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	int att_id = -1;
	NclObj att_obj = NULL;
	NclVar tmp_var = NULL;

	if(FileIsCoord(thefile,coord_name) > -1){
		index = FileIsVar(thefile,coord_name);
		tmp_md = MyFileReadVarValue(thefile,coord_name,sel_ptr,dim_info,
			FILE_COORD_VAR_ACCESS);
		if(tmp_md == NULL) 
			return(NULL);
		if(thefile->file.var_att_ids[index] == -1) 
			LoadVarAtts(thefile,coord_name);
		att_id = thefile->file.var_att_ids[index];
		att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
		if(att_obj != NULL) {
			att_id = att_obj->obj.id;
		} else {
			att_id = -1;
		}
		if(sel_ptr != NULL) {
			sel = sel_ptr->selection;
		} else {
			sel = NULL;
		}
		if(tmp_md != NULL) {
			tmp_var = _NclCoordVarCreate(
					NULL,
					NULL,
					Ncl_CoordVar,
					0,
					NULL,
					tmp_md,
					dim_info,
					att_id,
					NULL,
					((sel== NULL)? COORD:COORDSUBSEL),
					NrmQuarkToString(coord_name),
					TEMPORARY);
			if(tmp_var == NULL) {
				_NclDestroyObj((NclObj)tmp_md);
				if(att_obj != NULL) {
					_NclDestroyObj((NclObj)att_obj);
				}
			}
		}
		return(tmp_var);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is no  a coordinate variable for file (%s)",NrmQuarkToString(coord_name),NrmQuarkToString(thefile->file.fname));
	}
	return(NULL);
}

	

NclQuark FileGetDimName
#if	NhlNeedProto
(NclFile thefile, int num)
#else
(thefile, num)
NclFile thefile;
int num;
#endif
{
	if((num > -1)&&(num < thefile->file.n_file_dims)) {
		return(thefile->file.file_dim_info[num]->dim_name_quark);
	} else {
		return(-1);
	}
}


NclGroup *FileReadGroup
#if	NhlNeedProto
(NclFile thefile, NclQuark group_name)
#else 
(thefile, group_name)
NclFile thefile;
NclQuark group_name;
#endif
{
	NclGroup *group_out = NULL;
	int index;

	index = FileIsGroup(thefile,group_name);

      /*
       *fprintf(stdout, "\n\nFileReadGroup, file: %s, line:%d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
       *fprintf(stdout, "\tindex = %d\n", index);
       */

	if(index < 0)
		return (NULL);

#ifdef USE_NETCDF4_FEATURES
	group_out = _NclCreateGroup(NULL,NULL,Ncl_File,0,TEMPORARY,thefile,group_name);
#endif

#if 0
	if(group_out != NULL) {
		*id = group_out->obj.id;
		out_md = _NclMultiDValnclfileDataCreate(NULL,NULL,Ncl_MultiDValnclfileData,0,id,NULL,1,&dim_size,TEMPORARY,NULL);
		if(out_md != NULL) {
			out_data.kind = NclStk_VAL;
			out_data.u.data_obj = out_md;
			_NclPlaceReturn(out_data);
		} else {
			NclFree(id);
			_NclDestroyObj((NclObj)group_out);
		}
	} else {
		obj *tmp_obj = NULL; 
		tmp_obj =(obj*) NclMalloc(((NclTypeClass)nclTypeobjClass)->type_class.size);
		*tmp_obj = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
		out_md = _NclMultiDValnclfileDataCreate(
				NULL,
				NULL,
				Ncl_MultiDValnclfileData,
				0,
				(void*)tmp_obj,
				(void*)&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,
				1,
				&dim_size,
				TEMPORARY,
				NULL);
		if(out_md != NULL) {
			out_data.kind = NclStk_VAL;
			out_data.u.data_obj = out_md;
			_NclPlaceReturn(out_data);
			NclFree(id);
		} else {
			NclFree(id);
			_NclDestroyObj((NclObj)group_out);
		}
	}
#endif

      /*
       *fprintf(stdout, "\n\nend FileReadGroup, file: %s, line:%d\n", __FILE__, __LINE__);
       */
	return (group_out);
}


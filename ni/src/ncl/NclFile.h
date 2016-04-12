
/*
 *      $Id: NclFile.h,v 1.26 2010-04-28 23:02:03 huangwei Exp $
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
 *	Date:		Thu Jan 13 14:55:29 MST 1994
 *
 *	Description:	
 */
#ifndef NclFile_h
#define NclFile_h

#include "NclData.h"
#include "NclFileInterfaces.h"

#define FILE_COORD_VAR_ACCESS 0
#define FILE_VAR_ACCESS 1

#define NCLFILE_INC -1
#define NCLFILE_DEC -2
#define NCLFILE_VEC 0

extern int grib_version;

extern short NCLadvancedFileStructure[_NioNumberOfFileStructOptions];

typedef struct _NclFileRec NclFileRec;
typedef struct _NclFileClassRec NclFileClassRec;
typedef NclFileRec *NclFile;
typedef NclFileClassRec *NclFileClass;

typedef NclFileRec NclGroup;

typedef NclObjTypes (*NclFileVarRepValueFunc)(
#if	NhlNeedProto
NclFile	/* thefile */,
NclQuark /* var */
#endif
);

typedef struct _NclMultiDValDataRec* (*NclFileVarCoerceFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclObjTypes             /*coerce_to_obj*/,
NclScalar *             /*new_missing*/
#endif
);

typedef int (*NclFileVarIsAFunc)(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* var */,
NclQuark /* name */
#endif
);

typedef int (*NclFileIsAFunc)(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

typedef NhlErrorTypes (*NclAssignFileVarFunc)(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

typedef struct _NclVarRec* (*NclGetFileVarFunc)(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef NclGroup* (*NclGetFileGroupFunc)(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* group_name */
#endif
);

typedef struct _NclMultiDValDataRec* (*NclGetFileVarValFunc)(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadVarAttributeFunc)(
#if	NhlNeedProto
NclFile /* thefile */, 
NclQuark /* var */,
NclQuark /* attname */, 
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclDeleteVarAttributeFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */
#endif
);

typedef NhlErrorTypes (*NclWriteVarAttributeFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadAttributeFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclDeleteAttributeFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */
#endif
);
typedef NhlErrorTypes (*NclWriteAttributeFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadVarDimensionFunc)(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

typedef NhlErrorTypes (*NclWriteVarDimensionFunc)(
#if	NhlNeedProto
NclFile /*thefile*/, 
NclQuark /* var */,
NclQuark /*dim_name*/, 
long /*dim_num */
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadDimensionFunc)(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

typedef NhlErrorTypes (*NclWriteDimensionFunc)(
#if	NhlNeedProto
NclFile /*thefile*/, 
NclQuark /*dim_name*/, 
long /*dim_num */
#endif
);

typedef struct _NclVarRec * (*NclReadFileCoordFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

typedef NhlErrorTypes (*NclWriteFileCoordFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);


typedef NhlErrorTypes (*NclAssignFileVarVarFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /*lhs_var*/,
struct _NclSelectionRecord* /*lhs_sel_ptr*/,
struct _NclVarRec* /* rhs_var*/,
struct _NclSelectionRecord* /*rhs_sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclAddFileDimFunc)(
#if	NhlNeedProto
NclFile	/*thefile*/,
NclQuark /* dimname */,
ng_size_t	/*dimsize*/,
int	/*is_unlimited*/
#endif
);

typedef NhlErrorTypes (*NclAddFileChunkDimFunc)(
#if	NhlNeedProto
NclFile	/*thefile*/,
NclQuark /* dimname */,
ng_size_t	/*dimsize*/,
int	/*is_unlimited*/
#endif
);

typedef NhlErrorTypes (*NclAddFileVarFunc)(
#if	NhlNeedProto
NclFile	/*thefile*/,
NclQuark /* var_name */,
NclQuark /* type */,
int	/*n_dims*/,
NclQuark */* dimnames */
#endif
);

typedef NhlErrorTypes (*NclAddFileVarChunkFunc)(
#if	NhlNeedProto
NclFile	/*thefile*/,
NclQuark /* var_name */,
int	 /*n_dims*/,
ng_size_t *    /* dims */
#endif
);

typedef NhlErrorTypes (*NclAddFileVarChunkCacheFunc)(
#if	NhlNeedProto
NclFile	 /* thefile */,
NclQuark /* var_name */,
ng_size_t	 /* cache_size */,
ng_size_t	 /* cache_nelems */,
float    /* cache_preemption */
#endif
);

typedef NhlErrorTypes (*NclSetFileVarCompressLevelFunc)(
#if	NhlNeedProto
NclFile	/*thefile*/,
NclQuark /* var_name */,
int	 /* compress_level */
#endif
);

typedef NhlErrorTypes (*NclAddFileVarAttFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/
#endif
);

typedef NhlErrorTypes (*NclAddFileAttFunc)(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/
#endif
);

typedef NhlErrorTypes (*NclSetFileOptionFunc)(
#if	NhlNeedProto
NclFile thefile,
NclQuark format,
NclQuark option,
NclMultiDValData value
#endif
);

typedef NhlErrorTypes (*NclPostSetOptionFunc) (
#if	NhlNeedProto
NclFile thefile
#endif
);

typedef struct _NclFileOption {
	NclQuark format;
	NclQuark name;
	NclMultiDValData value;
	NclMultiDValData def_value;
	NclMultiDValData valid_values;
	int access;    /* 0 - any (read-only or read-write; 1 - read-only; 2 - read-write 3 - create only */ 
	NclPostSetOptionFunc post_set_option;
} NclFileOption;

typedef enum {
	Ncl_PREFILL = 0,
	Ncl_DEFINE_MODE,
	Ncl_THINNED_GRID_INTERPOLATION,
	Ncl_HEADER_RESERVE_SPACE,
	Ncl_SUPPRESS_CLOSE,
	Ncl_FORMAT,
	Ncl_READ_BYTE_ORDER,
	Ncl_WRITE_BYTE_ORDER,
	Ncl_INITIAL_TIME_COORDINATE_TYPE,
	Ncl_MISSING_TO_FILL_VALUE,
	Ncl_SHUFFLE,
	Ncl_COMPRESSION_LEVEL,
	Ncl_USE_CACHE,
	Ncl_CACHE_SIZE,
	Ncl_CACHE_NELEMS,
	Ncl_CACHE_PREEMPTION,
	Ncl_DEFAULT_NCEP_PTABLE,
	Ncl_PRINT_RECORD_INFO,
	Ncl_SINGLE_ELEMENT_DIMENSIONS,
	Ncl_TIME_PERIOD_SUFFIX,
	Ncl_ADVANCED_FILE_STRUCTURE,
	Ncl_RECORD_MARKER_SIZE,
	Ncl_GRIB_CACHE_SIZE,
	Ncl_KEEP_OPEN,
	Ncl_NUMBER_OF_FILE_OPTIONS
} NclFileOptionValues;

typedef struct _NclFileClassPart {

	NclFileVarRepValueFunc	rep_val;
	NclFileIsAFunc		is_var;
	NclAssignFileVarFunc	write_var;
	NclAssignFileVarVarFunc	write_var_var;
	NclGetFileVarFunc 	read_var_func;
	NclGetFileVarValFunc    read_var_val_func;
	NclFileIsAFunc		is_att;
	NclReadAttributeFunc	read_att_func;	
	NclWriteAttributeFunc	write_att_func;	
	NclDeleteAttributeFunc	del_att_func;	
	NclFileVarIsAFunc		is_var_att;
	NclReadVarAttributeFunc	read_var_att_func;	
	NclWriteVarAttributeFunc	write_var_att_func;	
	NclDeleteVarAttributeFunc	del_var_att_func;	
	NclFileIsAFunc		is_dim;
	NclFileVarIsAFunc		is_var_dim;
	NclReadVarDimensionFunc 	read_var_dim_func;	
	NclWriteVarDimensionFunc 	write_var_dim_func;	
	NclReadDimensionFunc 	read_dim_func;	
	NclWriteDimensionFunc 	write_dim_func;	
	NclFileIsAFunc		is_coord;
	NclReadFileCoordFunc	read_coord_func;
	NclWriteFileCoordFunc	write_coord_func;
	NclAddFileDimFunc	add_dim_func;
	NclAddFileChunkDimFunc	add_chunk_dim_func;
	NclAddFileVarFunc	add_var_func;
	NclAddFileVarChunkFunc	add_var_chunk_func;
	NclAddFileVarChunkCacheFunc	add_var_chunk_cache_func;
	NclSetFileVarCompressLevelFunc	set_var_compress_level_func;
	NclAddFileVarAttFunc	add_var_att_func;
	NclAddFileAttFunc	add_att_func;
	NclSetFileOptionFunc    set_file_option;
	NclFileOption           *options;
	NclFileIsAFunc		is_group;
	NclGetFileGroupFunc 	read_group_func;
	int                     num_options;
} NclFileClassPart;

typedef struct _NclFileAttInfoList {
	struct _NclFAttRec*	the_att;
	struct _NclFileAttInfoList * next;
} NclFileAttInfoList;

typedef struct _NclFilePart {
	NclQuark	fname;
	NclQuark	fpath;
	NclQuark	file_ext_q;
	int		wr_status;
	NclFileFormat	file_format;
	int             advanced_file_structure;

	int		         max_grps;
	int		         n_grps;
	struct _NclFGrpRec      **grp_info;
	NclFileAttInfoList      **grp_att_info;
	_NhlCB		         *grp_att_cb;
	struct _FileCallBackRec **grp_att_udata;
	int 		         *grp_att_ids;

	int		         max_vars;
	int		         n_vars;
	struct _NclFVarRec      **var_info;
	NclFileAttInfoList      **var_att_info;
	_NhlCB		         *var_att_cb;
	struct _FileCallBackRec **var_att_udata;
	int 		         *var_att_ids;

	int 	   	         max_file_dims;
	int 	   	         n_file_dims;
	struct _NclFDimRec  	**file_dim_info;
	struct _NclFVarRec	**coord_vars;

	int                      max_file_atts;
	int                      n_file_atts;
	struct _NclFAttRec	**file_atts;
	int 	                 file_atts_id;
	_NhlCB 		         file_att_cb;
	struct _FileCallBackRec *file_att_udata;
	struct _NclFormatFunctionRecord *format_funcs;
	void	*private_rec;
}NclFilePart;
 
struct _NclFileClassRec{
	NclObjClassPart	obj_class;
	NclFileClassPart file_class;
};

struct _NclFileRec {
	NclObjPart      obj;
	NclFilePart	file;
};


extern NclObjClass nclFileClass;

extern NclFileClassRec nclFileClassRec;

extern NclFile _NclOpenFile(
#if	NhlNeedProto
	NclObj	/* inst */,
	NclObjClass /*theclass*/,
	NclObjTypes /* obj_type */,
	unsigned int /*obj_type_mask*/,
	NclStatus /*status*/,
	NclQuark /*path */,
	int 	/*rw_status*/
#endif
);

extern NclQuark FileGetDimName(
#if     NhlNeedProto
NclFile /* thefile */,
int /*num*/
#endif
);

extern void LoadVarAtts(
#if     NhlNeedProto
NclFile thefile, NclQuark var
#endif
);

extern NhlErrorTypes UpdateDims(
#if     NhlNeedProto
        NclFile  thefile
#endif
);

extern void AddAttInfoToList(
#if     NhlNeedProto
	NclFileAttInfoList **list_handle, struct _NclFAttRec *the_att
#endif
);

extern void FileAttIsBeingDestroyedNotify(
#if     NhlNeedProto
NhlArgVal cbdata, NhlArgVal udata
#endif
);

typedef struct _FileCallBackRec {
	int	thefileid;
	int	theattid;
	int	thevar;
}FileCallBackRec;

NclFile _NclFileCreate(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                        unsigned int obj_type_mask, NclStatus status,
                        NclQuark path, int rw_status,
			NclQuark file_ext_q,
			NclQuark fname_q, NhlBoolean is_http, 
			char *end_of_name, int len_path);
extern void ReverseIt(void *val,void* swap_space,int ndims,int *compare_sel,
			ng_size_t *dim_sizes,int el_size);
void _NclReallocFilePart(NclFilePart *file,
                                int n_grps, int n_vars,
                                int n_file_dims, int n_file_atts);
NhlErrorTypes _NclFilePrintSummary(NclObj self, FILE *fp);
extern void *FileObtainCallData(NclObj obj, unsigned int type);
extern NhlErrorTypes FileAddParent(struct _NclObjRec *theobj, struct _NclObjRec *parent);
extern NhlErrorTypes FileDelParent(struct _NclObjRec *theobj, struct _NclObjRec *parent);
extern NhlErrorTypes InitializeFileOptions(NclFileOption *foptions);
extern NclFileOption file_options[Ncl_NUMBER_OF_FILE_OPTIONS];

#endif /* NclFile_h */

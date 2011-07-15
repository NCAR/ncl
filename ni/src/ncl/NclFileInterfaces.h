#ifndef NclFileInterfaces_h
#define NclFileInterfaces_h
typedef struct _NclFormatFunctionRecord *NclFormatFunctionRecPtr;
typedef struct _NclFormatFunctionRecord NclFormatFunctionRec;
typedef struct _NclFAttRec	NclFAttRec;
typedef struct _NclFGrpRec	NclFGrpRec;
typedef struct _NclFVarRec	NclFVarRec;
typedef struct _NclFDimRec	NclFDimRec;


struct _NclFAttRec {
	NclQuark att_name_quark;
	NclBasicDataTypes data_type;
	ng_size_t	num_elements;
};

struct _NclFGrpRec {
	NclQuark grp_name_quark;
	NclQuark grp_real_name_quark;
	NclQuark grp_full_name_quark;
	NclBasicDataTypes data_type;
	int	num_dimensions;
	int     file_dim_num[NCL_MAX_DIMENSIONS];
};

struct _NclFVarRec {
	NclQuark var_name_quark;
	NclQuark var_real_name_quark;
	NclQuark var_full_name_quark;
	NclBasicDataTypes data_type;
	int	num_dimensions;
/*
	ng_size_t	dim_sizes[NCL_MAX_DIMENSIONS];
*/
	int     file_dim_num[NCL_MAX_DIMENSIONS];

	int	 num_compounds;
	NclQuark component_name[NCL_MAX_COMPOUND_COMPONETS];
	NclBasicDataTypes component_type[NCL_MAX_COMPOUND_COMPONETS];
};

struct _NclFDimRec {
	NclQuark dim_name_quark;
	ng_size_t dim_size;
	int is_unlimited;
};	


typedef NclFormatFunctionRecPtr (*NclAddFileFormat) (
#if	NhlNeedProto
void
#endif
);

typedef void (*NclFreeFileRecFunc)(
#if	NhlNeedProto
void *
#endif
);
/*
* Returns pointer a pointer to a special record used by
* the file format to store private information about the
* file, as well as an enumerated variable indicating the
* supported file type class used to process the file.
*/
typedef enum _NclFileFormat {
	_NclCCM = 1,
	_NclNETCDF,
	_NclHDF,
	_NclHDF5,
	_NclHDFEOS,
	_NclHDFEOS5,
	_NclGRIB,
	_NclGRIB2,
#ifdef BuildOPENDAP
	_NclOPENDAP,
#endif
        _NclOGR,
        _NclNewOGR,
	_NclNETCDF4
} NclFileFormat;

typedef void * (*NclInitializeFileRecFunc)(
#if	NhlNeedProto
	NclFileFormat *format
#endif
);

/* for historical reasons these also return the private record,
 * but they no longer create it
 */

typedef void * (*NclCreateFileFunc)(
#if	NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
NclQuark 	/* Path name of file to be opened */
#endif
);

typedef void * (*NclOpenFileFunc)(
#if	NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
NclQuark ,	/* Path name of file to be opened */
int 		/* 0 for read/write, 1 for read only */
#endif
);

/*
* Returns a list of variable names for the opened file and the number of
* variables in the file.
*/
typedef NclQuark* (*NclGetVarNamesFunc)(
#if	NhlNeedProto
void *,	 /* Private record used to identify which file is being accessed */
int *	 /* Returns the number of variables in file */
#endif
);

/*
* Returns a structure containg type, dimension names and dimension sizes
*/
typedef NclFVarRec* (*NclGetVarInfoFunc)(
#if	NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
NclQuark /* variable name */
#endif
);

/*
* Returns an array of dimension names
*/
typedef NclQuark* (*NclGetDimNamesFunc)(
#if	NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
int *  /* Returns the number of dimensions in file */
#endif
);

/*
* Returns information on individual dimension
*/
typedef NclFDimRec *(*NclGetDimInfoFunc)(
#if	NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
NclQuark	/* name of dimension */
#endif
);

/*
* Returns all atribute names belonging to the file not individual variables
* though
*/
typedef NclQuark* (*NclGetAttNamesFunc)(
#if	NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
int *	/* number of attributes in return list */	
#endif
);

/*
* Returns a record containing the type of the attribute and the number of
* elements
*/
typedef NclFAttRec *(*NclGetAttInfoFunc)(
#if	NhlNeedProto
void *, 
NclQuark  /* name of attribute requested */
#endif
);

/*
* Returns all atribute names belonging to the variable 
*/
typedef NclQuark* (*NclGetVarAttNamesFunc)(
#if	NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
NclQuark, /* the variable */
int *	/* number of attributes in return list */	
#endif
);

/*
* Returns information about type and size of attribute
*/
typedef NclFAttRec *(*NclGetVarAttInfoFunc)(
#if	NhlNeedProto
void *,	
NclQuark, /* Variable name */
NclQuark  /* Atribute name */
#endif
);

/*
* Returns information about type and size of coordinate variable
*/
typedef NclFVarRec *(*NclGetCoordInfoFunc)(
#if	NhlNeedProto
void *,
NclQuark  /* dimension name */
#endif
);

/*
* Reads the data out of a coordinate variable
*/
typedef void *(*NclReadCoordFunc)(
#if	NhlNeedProto
void *,
NclQuark,  /* Var record of coordinate */
long *,	 /* Beginning index */
long *,    /* dimension sizes of slice (i.e count in each direction) */
long *,	/* stride*/
void *
#endif
);

typedef void *(*NclReadNoStrideCoordFunc)(
#if	NhlNeedProto
void *,
NclQuark,  /* Var record of coordinate */
long *,	 /* Beginning index */
long *,    /* dimension sizes of slice (i.e count in each direction) */
void *
#endif
);

/*
* Reads the data out of a variable
*/
typedef void *(*NclReadVarFunc)(
#if	NhlNeedProto
void *,
NclQuark,  /* Var record of coordinate */
long*,	 /* Beginning index */
long*,    /* dimension sizes of slice (i.e count in each direction) */
long*,
void *
#endif
);

typedef void *(*NclReadNoStrideVarFunc)(
#if	NhlNeedProto
void *,
NclQuark,  /* Var record of coordinate */
long*,	 /* Beginning index */
long*,    /* dimension sizes of slice (i.e count in each direction) */
void *
#endif
);

/*
* Read data out of an attribute
*/

typedef void *(*NclReadAttFunc)(
#if	NhlNeedProto
void *,
NclQuark, /* Attribute record */
void *
#endif
);

/*
* Read data out of a file attribute
*/
typedef void *(*NclReadVarAttFunc)(
#if	NhlNeedProto
void *,
NclQuark, /* var record */
NclQuark, /* Attribute record */
void *
#endif
);

typedef NhlErrorTypes (*NclWriteCoordFunc)(
#if	NhlNeedProto
void *,
NclQuark,
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *,			/* ending index */
long *
#endif
);

typedef NhlErrorTypes (*NclWriteNoStrideCoordFunc)(
#if	NhlNeedProto
void *,
NclQuark,
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *			/* ending index */
#endif
);

typedef NhlErrorTypes (*NclWriteVarFunc)(
#if	NhlNeedProto
void *,
NclQuark,  /* Variable to write to */
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *,			/* ending index */
long *
#endif
);

typedef NhlErrorTypes (*NclWriteNoStrideVarFunc)(
#if	NhlNeedProto
void *,
NclQuark,  /* Variable to write to */
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *			/* ending index */
#endif
);

typedef NhlErrorTypes (*NclWriteAttFunc)(
#if	NhlNeedProto
void *,
NclQuark,	/* the attribute */
void *		/* Block of data */
#endif
);


typedef NhlErrorTypes (*NclWriteVarAttFunc)(
#if	NhlNeedProto
void *,
NclQuark,	/* the att quark */
NclQuark,	/* the var quark */
void *		/* Block of data */
#endif
);


typedef NhlErrorTypes (*NclRenameDimFunc) (
#if	NhlNeedProto
void *,
NclQuark  /* from */,
NclQuark  /* to */
#endif
);

typedef NhlErrorTypes (*NclAddDimFunc) (
#if	NhlNeedProto
void *,
NclQuark,
ng_size_t,
int
#endif
);

typedef NhlErrorTypes (*NclAddChunkDimFunc) (
#if	NhlNeedProto
void *,
NclQuark,
ng_size_t,
int
#endif
);

typedef NhlErrorTypes (*NclAddGrpFunc)(void *record, NclQuark grpname);
typedef NhlErrorTypes (*NclAddVlenFunc)(void *record, NclQuark vlen_name, NclQuark var_name,
                                        NclQuark type, NclQuark dim_name);
typedef NhlErrorTypes (*NclAddEnumFunc)(void *record, NclQuark enum_name, NclQuark var_name,
                                        NclQuark dim_name, NclQuark  *mem_name, void *mem_value,
                                        ng_size_t n_mems, NclBasicDataTypes val_type);
typedef NhlErrorTypes (*NclAddOpaqueFunc)(void *record, NclQuark opaque_name, NclQuark var_name,
                                          int var_size, NclQuark dim_name);
typedef NhlErrorTypes (*NclAddCompoundFunc)(void *record, NclQuark compound_name, NclQuark var_name,
                                            ng_size_t n_dims, NclQuark *dim_name, ng_size_t n_mems,
                                            NclQuark *mem_name, NclQuark *mem_type, int *mem_size);

typedef NhlErrorTypes (*NclAddVarCoordFunc) (
#if	NhlNeedProto
void*, /* record */
NclQuark, /*var_name */
NclBasicDataTypes /*data_type*/
#endif
);
typedef NhlErrorTypes (*NclAddVarFunc) (
#if	NhlNeedProto
void*, /* record */
NclQuark, /*var_name */
NclBasicDataTypes, /* data_type */
int, /* n_dims */
NclQuark *, /* dim_names */
ng_size_t * /* dim_sizes */
#endif
);

typedef NhlErrorTypes (*NclAddVarChunkFunc) (
#if     NhlNeedProto
void*, /* record */
NclQuark, /*var_name */
int, /* n_dims */
ng_size_t * /* dims */
#endif
);

typedef NhlErrorTypes (*NclAddVarChunkCacheFunc) (
#if     NhlNeedProto
void *,   /* record */
NclQuark, /* var_name */
ng_size_t,   /* cache_size */
ng_size_t,   /* cache_nelems */
float     /* cache_preemption */
#endif
);

typedef NhlErrorTypes (*NclSetVarCompressLevelFunc) (
#if     NhlNeedProto
void*,    /* record */
NclQuark, /*var_name */
int       /* compress-level */
#endif
);

typedef NhlErrorTypes (*NclDelVarAttFunc) (
#if	NhlNeedProto
void * /*therec*/,
NclQuark /*thevar*/,
NclQuark /*theatt*/
#endif
);
typedef NhlErrorTypes (*NclAddVarAttFunc) (
#if	NhlNeedProto
void * /*therec*/,
NclQuark /*thevar*/,
NclQuark /*theatt*/,
NclBasicDataTypes /*data_type*/,
int  /*n_items*/,
void * /*values*/
#endif
);

typedef NhlErrorTypes (*NclDelAttFunc) (
#if	NhlNeedProto
void * /*therec*/,
NclQuark /*theatt*/
#endif
);
typedef NhlErrorTypes (*NclAddAttFunc) (
#if	NhlNeedProto
void * /*therec*/,
NclQuark /*theatt*/,
NclBasicDataTypes /*data_type*/,
int  /*n_items*/,
void * /*values*/
#endif
);

typedef NclBasicDataTypes (*NclMapFormatTypeToNcl)(
#if	NhlNeedProto
void *
#endif
);

typedef void* (*NclMapNclTypeToFormat)(
#if	NhlNeedProto
NclBasicDataTypes
#endif
);

typedef NhlErrorTypes (*NclSetOptionFunc) (
#if	NhlNeedProto
void * /*therec*/,
NclQuark /*option*/,
NclBasicDataTypes /*data_type*/,
int  /*n_items*/,
void * /*values*/
#endif
);

/*
* Returns an array of string containg the group names.
*/
typedef NclQuark* (*NclGetGrpNamesFunc)(
#if	NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
int *   /* number of total groups. */
#endif
);

/*
* Returns a structure containg type, dimension names and dimension sizes
*/
typedef NclFGrpRec* (*NclGetGrpInfoFunc)(
#if	NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
NclQuark /* group name */
#endif
);

/*
* Returns all atribute names belonging to the group.
*/
typedef NclQuark* (*NclGetGrpAttNamesFunc)(
#if	NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
NclQuark, /* the group */
int *   /* number of attributes in return list */
#endif
);

/*
* Returns a record containing the type of the attribute and the number of
* elements
*/
typedef NclFAttRec *(*NclGetGrpAttInfoFunc)(
#if	NhlNeedProto
void *,
NclQuark, /* Group name */
NclQuark  /* Atribute name */
#endif
);

struct _NclFormatFunctionRecord {
NclInitializeFileRecFunc	initialize_file_rec;
NclCreateFileFunc	create_file;
NclOpenFileFunc		open_file;
NclFreeFileRecFunc	free_file_rec;
NclGetVarNamesFunc	get_var_names;
NclGetVarInfoFunc	get_var_info;
NclGetDimNamesFunc	get_dim_names;
NclGetDimInfoFunc	get_dim_info;
NclGetAttNamesFunc	get_att_names;
NclGetAttInfoFunc	get_att_info;
NclGetVarAttNamesFunc	get_var_att_names;
NclGetVarAttInfoFunc	get_var_att_info;
NclGetCoordInfoFunc	get_coord_info;
NclReadCoordFunc	read_coord;
NclReadNoStrideCoordFunc	read_coord_ns;
NclReadVarFunc		read_var;
NclReadNoStrideVarFunc		read_var_ns;
NclReadAttFunc		read_att;
NclReadVarAttFunc	read_var_att;
NclWriteCoordFunc	write_coord;
NclWriteNoStrideCoordFunc	write_coord_ns;
NclWriteVarFunc		write_var;
NclWriteNoStrideVarFunc		write_var_ns;
NclWriteAttFunc		write_att;
NclWriteVarAttFunc	write_var_att;
NclAddDimFunc 		add_dim;
NclAddChunkDimFunc 	add_chunk_dim;
NclRenameDimFunc 	rename_dim;
NclAddVarFunc		add_var;
NclAddVarChunkFunc	add_var_chunk;
NclAddVarChunkCacheFunc	add_var_chunk_cache;
NclSetVarCompressLevelFunc  set_var_compress_level;
NclAddVarCoordFunc	add_coord_var;
NclAddAttFunc		add_att;
NclAddVarAttFunc	add_var_att;
NclMapFormatTypeToNcl	map_format_type_to_ncl;
NclMapNclTypeToFormat	map_ncl_type_to_format;
NclDelAttFunc		del_att;
NclDelVarAttFunc	del_var_att;
NclGetGrpNamesFunc	get_grp_names;
NclGetGrpInfoFunc       get_grp_info;
NclGetGrpAttNamesFunc   get_grp_att_names;
NclGetGrpAttInfoFunc    get_grp_att_info;
NclAddGrpFunc		add_grp;
NclAddVlenFunc		add_vlen;
NclAddEnumFunc		add_enum;
NclAddOpaqueFunc	add_opaque;
NclAddCompoundFunc	add_compound;
NclSetOptionFunc        set_option;  
};

extern void _NclRegisterFormat(
#if	NhlNeedProto
        NclAddFileFormat /* add_format */,
        char*   /* file_extension */
#endif
);

extern NclFormatFunctionRecPtr _NclGetFormatFuncs(
#if	NhlNeedProto
NclQuark /* file_extension */
#endif
);

logical _NclFormatEqual(
#if NhlNeedProto
	NclQuark file_ext1, 
	NclQuark file_ext2
#endif
);

int _NclGribVersion(
#if NhlNeedProto
	NclQuark path
#endif
);

extern NclFormatFunctionRecPtr _NclGetFormatFuncsWithNewHLFS
                               (NclQuark file_extension);

#endif

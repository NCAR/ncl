typedef struct _NclFormatFunctionRecord *NclFormatFunctionRecPtr;
typedef struct _NclFormatFunctionRecord NclFormatFunctionRec;
typedef struct _NclFAttRec	NclFAttRec;
typedef struct _NclFVarRec	NclFVarRec;
typedef struct _NclFDimRec	NclFDimRec;


struct _NclFAttRec {
	NclQuark att_name_quark;
	NclBasicDataTypes data_type;
	int	num_elements;
};

struct _NclFVarRec {
	NclQuark var_name_quark;
	NclBasicDataTypes data_type;
	int	num_dimensions;
	int	dim_sizes[NCL_MAX_DIMENSIONS];
	int     file_dim_num[NCL_MAX_DIMENSIONS];
};

struct _NclFDimRec {
	NclQuark dim_name_quark;
	long dim_size;
};	

typedef NclFormatFunctionRecPtr (*NclAddFileFormat) (
#ifdef NhlNeedProto
void
#endif
);

typedef void (*NclFreeFileRecFunc)(
#ifdef NhlNeedProto
void *
#endif
);
/*
* Returns pointer a pointer to a special record used byt
* the file format to store private information about the
* file.
*/
typedef void * (*NclGetFileRecFunc)(
#ifdef NhlNeedProto
NclQuark ,	/* Path name of file to be opened */
int 		/* 0 for read/write, 1 for read only */
#endif
);

/*
* Returns a list of variable names for the opened file and the number of
* variables in the file.
*/
typedef NclQuark* (*NclGetVarNamesFunc)(
#ifdef NhlNeedProto
void *,	 /* Private record used to identify which file is being accessed */
int *	 /* Returns the number of variables in file */
#endif
);

/*
* Returns a structure containg type, dimension names and dimension sizes
*/
typedef NclFVarRec* (*NclGetVarInfoFunc)(
#ifdef NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
NclQuark /* variable name */
#endif
);

/*
* Returns an array of dimension names
*/
typedef NclQuark* (*NclGetDimNamesFunc)(
#ifdef NhlNeedProto
void *, /* Private record used to identify which file is being accessed */
int *  /* Returns the number of dimensions in file */
#endif
);

/*
* Returns information on individual dimension
*/
typedef NclFDimRec *(*NclGetDimInfoFunc)(
#ifdef NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
NclQuark	/* name of dimension */
#endif
);

/*
* Returns all atribute names belonging to the file not individual variables
* though
*/
typedef NclQuark* (*NclGetAttNamesFunc)(
#ifdef NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
int *	/* number of attributes in return list */	
#endif
);

/*
* Returns a record containing the type of the attribute and the number of
* elements
*/
typedef NclFAttRec *(*NclGetAttInfoFunc)(
#ifdef NhlNeedProto
void *, 
NclQuark  /* name of attribute requested */
#endif
);

/*
* Returns all atribute names belonging to the variable 
*/
typedef NclQuark* (*NclGetVarAttNamesFunc)(
#ifdef NhlNeedProto
void *,  /* Private record used to identify which file is being accessed */
NclQuark, /* the variable */
int *	/* number of attributes in return list */	
#endif
);

/*
* Returns information about type and size of attribute
*/
typedef NclFAttRec *(*NclGetVarAttInfoFunc)(
#ifdef NhlNeedProto
void *,	
NclQuark, /* Variable name */
NclQuark  /* Atribute name */
#endif
);

/*
* Returns information about type and size of coordinate variable
*/
typedef NclFVarRec *(*NclGetCoordInfoFunc)(
#ifdef NhlNeedProto
void *,
NclQuark  /* dimension name */
#endif
);

/*
* Reads the data out of a coordinate variable
*/
typedef void *(*NclReadCoordFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,  /* Var record of coordinate */
long *,	 /* Beginning index */
long *,    /* dimension sizes of slice (i.e count in each direction) */
long *,	/* stride*/
void *
#endif
);

typedef void *(*NclReadNoStrideCoordFunc)(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
void *,
NclQuark,  /* Var record of coordinate */
long*,	 /* Beginning index */
long*,    /* dimension sizes of slice (i.e count in each direction) */
long*,
void *
#endif
);

typedef void *(*NclReadNoStrideVarFunc)(
#ifdef NhlNeedProto
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
#ifdef NhlNeedProto
void *,
NclQuark, /* Attribute record */
void *
#endif
);

/*
* Read data out of a file attribute
*/
typedef void *(*NclReadVarAttFunc)(
#ifdef NhlNeedProto
void *,
NclQuark, /* var record */
NclQuark, /* Attribute record */
void *
#endif
);

typedef NhlErrorTypes (*NclWriteCoordFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *,			/* ending index */
long *
#endif
);

typedef NhlErrorTypes (*NclWriteNoStrideCoordFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *			/* ending index */
#endif
);

typedef NhlErrorTypes (*NclWriteVarFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,  /* Variable to write to */
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *,			/* ending index */
long *
#endif
);

typedef NhlErrorTypes (*NclWriteNoStrideVarFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,  /* Variable to write to */
void *,		       /* Pointer to block of data */
long *,			/* Beginning index */
long *			/* ending index */
#endif
);

typedef NhlErrorTypes (*NclWriteAttFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,	/* the attribute */
void *		/* Block of data */
#endif
);


typedef NhlErrorTypes (*NclWriteVarAttFunc)(
#ifdef NhlNeedProto
void *,
NclQuark,	/* the att quark */
NclQuark,	/* the var quark */
void *		/* Block of data */
#endif
);


typedef NhlErrorTypes (*NclRenameDimFunc) (
#ifdef NhlNeedProto
void *,
NclQuark  /* from */,
NclQuark  /* to */
#endif
);

typedef NhlErrorTypes (*NclAddDimFunc) (
#ifdef NhlNeedProto
void *,
NclQuark,
int
#endif
);

typedef NhlErrorTypes (*NclAddVarCoordFunc) (
#ifdef NhlNeedProto
void*, /* record */
NclQuark, /*var_name */
NclBasicDataTypes /*data_type*/
#endif
);
typedef NhlErrorTypes (*NclAddVarFunc) (
#ifdef NhlNeedProto
void*, /* record */
NclQuark, /*var_name */
NclBasicDataTypes, /* data_type */
int, /* n_dims */
NclQuark *, /* dim_names */
long * /* dim_sizes */
#endif
);

typedef NhlErrorTypes (*NclAddVarAttFunc) (
#ifdef NhlNeedProto
void * /*therec*/,
NclQuark /*thevar*/,
NclQuark /*theatt*/,
NclBasicDataTypes /*data_type*/,
int  /*n_items*/,
void * /*values*/
#endif
);

typedef NhlErrorTypes (*NclAddAttFunc) (
#ifdef NhlNeedProto
void * /*therec*/,
NclQuark /*theatt*/,
NclBasicDataTypes /*data_type*/,
int  /*n_items*/,
void * /*values*/
#endif
);

typedef NclBasicDataTypes (*NclMapFormatTypeToNcl)(
#ifdef NhlNeedProto
void *
#endif
);

typedef void* (*NclMapNclTypeToFormat)(
#ifdef NhlNeedProto
NclBasicDataTypes
#endif
);

struct _NclFormatFunctionRecord {
NclGetFileRecFunc	get_file_rec;
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
NclRenameDimFunc 		rename_dim;
NclAddVarFunc		add_var;
NclAddVarCoordFunc add_coord_var;
NclAddAttFunc		add_att;
NclAddVarAttFunc	add_var_att;
NclMapFormatTypeToNcl	map_format_type_to_ncl;
NclMapNclTypeToFormat	map_ncl_type_to_format;
};

extern void _NclRegisterFormat(
#ifdef NhlNeedProto
        NclAddFileFormat /* add_format */,
        char*   /* file_extension */
#endif
);

extern NclFormatFunctionRecPtr _NclGetFormatFuncs(
#ifdef NhlNeedProto
NclQuark /* file_extension */
#endif
);


typedef struct _NclApiDataList NclApiDataList;
typedef struct _NclApiFuncInfoRec NclApiFuncInfoRec;
typedef struct _NclApiVarInfoRec NclApiVarInfoRec;
typedef struct _NclApiFileInfoRec NclApiFileInfoRec;
typedef struct _NclApiHLUVarInfoRec NclApiHLUVarInfoRec;
typedef struct _NclApiHLUObjInfoRec NclApiHLUObjInfoRec;
typedef struct _NclApiHLUInfoRec NclApiHLUInfoRec;

#define VARIABLE_LIST 1
#define PROCFUNC_LIST 2
#define FILE_LIST 3
#define FILEVAR_LIST 4
#define HLU_LIST 5
#define HLUOBJ_LIST 6
#ifndef NclQuarkIsDef
typedef long NclQuark;
#define NclQuarkIsDef
#endif

struct  _NclApiDataList {
	int kind;
	struct _NclApiDataList *next;
	union {
		struct _NclApiFuncInfoRec *func;
		struct _NclApiVarInfoRec *var;
		struct _NclApiFileInfoRec *file;
		struct _NclApiHLUVarInfoRec *hlu;
		struct _NclApiHLUObjInfoRec *hlu_obj;
	} u;
};

struct _NclApiHLUObjInfoRec {
	NclQuark name;
	int obj_id;
	NhlClass obj_class;
};
struct _NclApiHLUVarInfoRec {
	NclQuark name;
	int	n_objs;
	struct _NclApiHLUInfoRec* objs;
};

struct _NclApiHLUInfoRec {
	NclQuark obj_name;
	NclQuark obj_class;
	int 	obj_id;
};
typedef struct _NclApiArgTemplate {
        int n_dims;
        ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	NclQuark arg_data_type;
        NclQuark arg_sym;
        int is_dimsizes;
} NclApiArgTemplate;

struct _NclApiFuncInfoRec {
	NclQuark name;
	int 	kind;
	int	nparams;
	struct _NclApiArgTemplate *theargs;
};

struct _NclApiVarInfoRec {
	NclQuark name;
	int data_type;
	NclVarTypes 	type;	/* value, hlu, file */
	int	n_dims;
	NclDimRec *dim_info;
	NclQuark	coordnames[NCL_MAX_DIMENSIONS];
	int	n_atts;
	NclQuark	*attnames;
};

struct _NclApiFileInfoRec {
	NclQuark name;
	NclQuark path;
	int file_format;
	int wr_status;
	int 	n_dims;
	NclDimRec *dim_info;
	int	n_atts;
	NclQuark *attnames;
	int	n_vars;
	NclQuark *var_names;
};



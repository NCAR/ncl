
typedef struct _NclApiDataList NclApiDataList;
typedef struct _NclApiFuncInfoRec NclApiFuncInfoRec;
typedef struct _NclApiVarInfoRec NclApiVarInfoRec;
typedef struct _NclApiFileInfoRec NclApiFileInfoRec;
typedef struct _NclApiHLUVarInfoRec NclApiHLUVarInfoRec;
typedef struct _NclApiHLUInfoRec NclApiHLUInfoRec;

#define VARIABLE_LIST 1
#define PROCFUNC_LIST 2
#define FILE_LIST 3
#define FILEVAR_LIST 4
#define HLU_LIST 5

struct  _NclApiDataList {
	int kind;
	struct _NclApiDataList *next;
	union {
		struct _NclApiFuncInfoRec *func;
		struct _NclApiVarInfoRec *var;
		struct _NclApiFileInfoRec *file;
		struct _NclApiHLUVarInfoRec *hlu;
	} u;
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
struct _NclApiFuncInfoRec {
	NclQuark name;
	int 	kind;
	int	nparams;
	struct _NclArgTemplate *theargs;
};

struct _NclApiVarInfoRec {
	NclQuark name;
	NclQuark data_type_quark;
	NclVarTypes 	type;	/* value, hlu, file */
	int	n_dims;
	NclDimRec *dim_info;
	int	n_atts;
	NclQuark	*attnames;
};

struct _NclApiFileInfoRec {
	NclQuark name;
	NclQuark path;
	int file_type;
	int wr_status;
	int 	n_dims;
	NclDimRec *dim_info;
	int	n_atts;
	NclQuark *attnames;
	int	n_vars;
	NclQuark *var_names;
};



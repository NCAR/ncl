#include "NclMultiDValData.h"
#include "NclOneDValCoordData.h"

extern void NGCALLF(mdppos,MDPPOS)(double *, double *, double *, double *);
extern void NGCALLF(mdproj,MDPROJ)(char *, double *, double *, double *, int);
extern void NGCALLF(mdpset,MDPSET)(char *, double *, double *, double *, double *, int);
extern void NGCALLF(mapbd,MAPBD)(void);
extern void NGCALLF(mdpint,MDPINT)();
extern void NGCALLF(gdswiz,GDSWIZ)(int *, int *, int *, float *,
                                   float *, float *, float *, float *,
                                   int *, int *, float *, float *);
extern void NGCALLF(mdptrn,MDPTRN)(double *, double *, double *, double *);
extern void NGCALLF(mdptri,MDPTRI)(double *, double *, double *, double *);
extern void NGCALLF(qu2reg3,QU2REG3)(float *, int *, int *, int *, int *,
                                     float *, int *, int *, int *, int *,
                                     int *, float *, float *, float *);
extern void NGCALLF(gaqdnio,GAQDNIO)(int *, double *, double *, double *, int *, int *);
extern void NGCALLF(maptrn,MAPTRN)(float *, float*, float *, float *);
extern void NGCALLF(maptri,MAPTRI)(float *, float*, float *, float *);
extern void _NclInitMapTrans(char *, double, double, double);

typedef struct tble2 {
        int  num;
        char *long_name;
        char *units;
        char *abrev;
}TBLE2;

typedef union cvt{
        char c[4];
        unsigned int value;
        int ivalue;
}CVT;
typedef union cvtf {
        char c[4];
        float value;
}CVTF;

typedef struct _git{
	short year;
	short days_from_jan1;
	short minute_of_day;
}GIT;

typedef struct _ens {
	int extension_type; /* 0 - NCEP, 1 .. n - ECMWF  local definition */
	int type;
	int id;
	int prod_id;
	TBLE2 *prob_param; 
	int prob_type;
	float lower_prob;
	float upper_prob;
	int  n_members;
} ENS;

typedef struct ens_list {
	int ens_ix;
	ENS ens;
	int n_it;
	GIT *it_vals;
	int n_ft;
	int *ft_vals;
	int n_lv;
	int *lv_vals;
	int *lv_vals1;
	struct it_list *thelist;
	struct ens_list *next;
}ENSLIST;


typedef struct it_list {
	GIT it;
	int n_ft;
	int *ft_vals;
	int n_lv;
	int *lv_vals;
	int *lv_vals1;
	struct ft_list *thelist;
	struct it_list *next;
}ITLIST;

typedef struct ft_list {
	int ft;
	int n_lv;
	int *lv_vals;
	int *lv_vals1;
	struct _GribRecordInqRecList *thelist;
	struct ft_list *next;
}FTLIST;


typedef struct _ptableinfo {
	struct _ptableinfo *next;
	int center;
	int subcenter;
	int version;
	char *name;
	int pcount;
	TBLE2 *table;
} PtableInfo;

#define GRIBEOF  0
#define GRIBERROR -1
#define GRIBOK 1

typedef struct _GribFileRecord GribFileRecord;
typedef struct _GribRecordInqRec GribRecordInqRec;
typedef struct _GribDimInqRec GribDimInqRec;
typedef struct _GribAttInqRec GribAttInqRec;
typedef struct _GribRecordInqRecList GribRecordInqRecList;
typedef struct _GribDimInqRecList GribDimInqRecList;
typedef struct _GribAttInqRecList GribAttInqRecList;
typedef struct _GribParamList GribParamList;
typedef struct _GribInternalVarList GribInternalVarList;
typedef struct _GribInternalVarRec GribInternalVarRec;
typedef struct _NclGribFVarRec      NclGribFVarRec;
typedef struct _NclGribCacheRec      NclGribCacheRec;
typedef struct _NclGribCacheList      NclGribCacheList;
typedef struct _GribOptions          GribOptions;

struct _NclGribCacheList {
	int grid_number;
	int has_gds;
	int grid_gds_tbl_index;
	int n_dims;
	ng_size_t dimsizes[3];
        int dim_ids[3];
	int n_entries;
	NclMultiDValData int_missing_rec;       /* shared by all integer vars */
	NclMultiDValData float_missing_rec;     /* shared by all float vars */
	struct _NclGribCacheList *next;
	struct _NclGribCacheRec *thelist;
	struct _NclGribCacheRec *tail;
};

struct _NclGribCacheRec {
	struct _NclGribCacheRec *prev;
	struct _NclGribCacheRec *next;
	GribRecordInqRec *rec;
	NclMultiDValData thevalue;
};

struct _NclGribFVarRec {
        NclQuark var_name_quark;
	NclQuark long_name_q;
	NclQuark units_q;
        NclBasicDataTypes data_type;
	int 	doff;
        int     num_dimensions;
        ng_size_t     dim_sizes[NCL_MAX_DIMENSIONS];
        int     file_dim_num[NCL_MAX_DIMENSIONS];
};
struct _GribInternalVarList {
	GribInternalVarRec *int_var;
	GribInternalVarList *next;
};

struct _GribInternalVarRec {
	NclQuark var_name_q;
	NclGribFVarRec var_info;
	NclMultiDValData value;
	int n_atts;
	GribAttInqRecList *theatts;
};

struct _GribParamList {
	int param_number;
	int ptable_version;
	unsigned char aux_ids[4];
	int grid_number;
	int grid_tbl_index;
	int has_gds;
	int gds_type;
	int grid_gds_tbl_index;
	int n_entries;
	int time_range_indicator;
	int time_period;      /* 0 unless  ave,diff, or acc: then p2 - p1 */
	int time_unit_indicator;
	int variable_time_unit;
	int level_indicator;
	GIT minimum_it;
	NclGribFVarRec var_info;
	NrmQuark aux_coords[2];
	int ensemble_isatt;
	NclMultiDValData ensemble;
	NclOneDValCoordData ens_indexes;
	TBLE2 *prob_param;
	NclOneDValCoordData probability;
	NclMultiDValData lower_probs;
	NclMultiDValData upper_probs;
	int yymmddhh_isatt;
	NclOneDValCoordData yymmddhh;
	GIT *it_vals;
	int forecast_time_isatt;
	NclOneDValCoordData forecast_time;
	int levels_isatt;
	int levels_has_two;
	NclOneDValCoordData levels;
	NclMultiDValData levels0;
	NclMultiDValData levels1;
	GribRecordInqRec *ref_rec;   /* pointer to the first non-missing record in the list after the array sort is completed */
	GribRecordInqRecList *thelist;
	int n_atts;
	GribAttInqRecList *theatts;
	void *info;
	GribParamList *next;
};
struct _GribRecordInqRecList {
	GribRecordInqRec *rec_inq;
	GribRecordInqRecList *next;
};

struct _GribDimInqRecList {
	GribDimInqRec *dim_inq;
	GribDimInqRecList *next;
};

struct _GribAttInqRecList {
	GribAttInqRec *att_inq;
	GribAttInqRecList *next;
};

struct _GribRecordInqRec {
	NclQuark var_name_q;
	int param_number;
	int ptable_version;
	TBLE2 *ptable_rec;
	int grid_tbl_index;
	int grid_number;
	int version;
	int rec_num;
	int center_ix;
	int eff_center; /* the effective center -- that provides the tables */
/*
* This is the time offset from the beginning reference
* time of the parameter set. The units are set in
* the GribParamList structure
*/
	GIT initial_time;
	long time_offset;
	int time_period;      /* this is P2 - P1 -- the period for acc, avg and diff type records */
	int level_indicator;
	int level0;
	int level1;
	int level_index; /* index into the level string and units tables */
	unsigned char *pds;
	int pds_size;
	char *var_name;
	NclQuark long_name_q;
	NclQuark units_q;
	off_t offset;
	unsigned int bds_off;
	unsigned int bds_flags;
	unsigned int bds_size;
	int int_or_float;
	int has_gds;
	unsigned char *gds;
	unsigned int gds_off;
	unsigned int gds_size;
	int gds_type;
	int grid_gds_tbl_index;
	int interp_method;   /* 0 - linear ; 1 - cubic */
	int has_bms;
	unsigned int bms_off;
	unsigned int bms_size;
	NclMultiDValData the_dat;
	int is_ensemble;
	ENS ens;
};

struct _GribDimInqRec {
	int dim_number; /* assigned in order of occurance in grib record */
	int sub_type_id;    /* identifier of the dimension type */
	int is_gds;
	int gds_size;
	unsigned char *gds;
	NclQuark dim_name;
/*	long size;*/
	ng_size_t size;
	int is_uv; /* only applicable to staggered grids */
};
	
struct _GribAttInqRec {
	NclQuark name;
	NclMultiDValData thevalue;
};

#define GRIB_THINNED_GRID_INTERPOLATION_OPT 0
#define GRIB_INITIAL_TIME_COORDINATE_TYPE_OPT 1
#define GRIB_DEFAULT_NCEP_PTABLE_OPT 2
#define GRIB_PRINT_RECORD_INFO_OPT 3
#define GRIB_SINGLE_ELEMENT_DIMENSIONS_OPT 4
#define GRIB_TIME_PERIOD_SUFFIX_OPT 5
#define GRIB_CACHE_SIZE_OPT 6
#define GRIB_NUM_OPTIONS 7

#define GRIB_No_Dims 0
#define GRIB_All_Dims  0xffffffff
#define GRIB_Level_Dims 0x1
#define GRIB_Forecast_Time_Dims 0x2
#define GRIB_Initial_Time_Dims 0x4
#define GRIB_Ensemble_Dims 0x8

struct _GribOptions {
	NclQuark name;
	NclBasicDataTypes data_type;
	int n_values;
	void *values;
};

struct _GribFileRecord {
NclQuark	file_path_q;
int		wr_status;
int		n_vars;
GribParamList	*var_list;
int 		n_internal_vars;
GribInternalVarList	*internal_var_list;
int		total_dims;
int		n_scalar_dims;
GribDimInqRecList *scalar_dims;
int		n_ensemble_dims;
GribDimInqRecList *ensemble_dims;
int		n_it_dims;
GribDimInqRecList *it_dims;
int		n_ft_dims;
GribDimInqRecList *ft_dims;
int		n_lv_dims;
GribDimInqRecList *lv_dims;
int		n_grid_dims;
GribDimInqRecList *grid_dims;
NclGribCacheList *grib_grid_cache;
int             n_options;
GribOptions     *options;
int             single_dims;
};

typedef int (*GribUnPackData)(
#if NhlNeedProto
int /*fd*/,
void** /* output */,
void**  /* missing */,
GribRecordInqRec* /* therec */,
GribParamList* /* thevarrec */
#endif
);	

typedef void (*GribGetGDSGrid)(
#if NhlNeedProto
GribParamList* thevarrec,
float** lat,
int * n_dims_lat,
ng_size_t ** dimsizes_lat,
float ** lon,
int * n_dims_lon,
ng_size_t **dimsizes_lon,
float **rot,
int * n_dims_rot,
ng_size_t **dimsizes_rot,
GribAttInqRecList ** lat_att_list,
int * nlatatts,
GribAttInqRecList ** lon_att_list,
int * nlonatts,
GribAttInqRecList ** rot_att_list,
int * nrotatts
#endif
);
	
typedef void (*GribGetGrid)(
#if NhlNeedProto
GribParamList* thevarrec,
float** lat,
int * n_dims_lat,
ng_size_t ** dimsizes_lat,
float ** lon,
int * n_dims_lon,
ng_size_t **dimsizes_lon,
float **rot,
GribAttInqRecList ** lat_att_list,
int * nlatatts,
GribAttInqRecList ** lon_att_list,
int * nlonatts,
GribAttInqRecList ** rot_att_list,
int * nrotatts
#endif
);	

typedef void (*GribGetGridAtts)(
#if NhlNeedProto
GribParamList*,
GribAttInqRecList ** /*lat_att_list*/,
int * /*nlatatts*/,
GribAttInqRecList ** /*lon_att_list*/,
int * /*lonatts*/,
int do_rot,
int grid_oriented,
GribAttInqRecList ** /*rot_att_list*/,
int * /*rotatts*/
#endif
);

typedef struct gridinfo {
	GribUnPackData un_pack;
	GribGetGrid get_grid;
	GribGetGridAtts get_grid_atts;
	char *grid_name;
}GridInfoRecord;
typedef struct gridgdsinfo {
	GribUnPackData un_pack;
	GribGetGDSGrid get_gds_grid;
	char *grid_name;
}GridGDSInfoRecord;

extern int CnvtToDecimal(
#if     NhlNeedProto
int /*n_bytes*/,
unsigned char * /*val*/
#endif
);

extern unsigned int UnsignedCnvtToDecimal(
#if     NhlNeedProto
int /*n_bytes*/,
unsigned char * /*val*/
#endif
);

extern int Is_UV(
#if     NhlNeedProto
int param_number
#endif
);

typedef struct _GribTable {
	int index;
	char *name;
} GribTable;

#if 0
#define DEFAULT_MISSING_FLOAT ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval
#endif
#define DEFAULT_MISSING_FLOAT 1e20
#define DEFAULT_MISSING_INT ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval

extern int GetNextGribOffset(int gribfile, off_t *offset, unsigned int *totalsize,
                             off_t startoff, off_t *nextoff,int* version, ng_size_t *bds_size);

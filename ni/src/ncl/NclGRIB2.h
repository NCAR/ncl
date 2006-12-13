#include "NclMultiDValData.h"
#include "NclOneDValCoordData.h"


# define GRIB2EOF    0
# define GRIB2ERROR -1
# define GRIB2OK     1

typedef struct _git2{
	short year;
	short days_from_jan1;
	short minute_of_day;
} G2_GIT;

typedef struct g2_tble2 {
        int  num;
        char *long_name;
        char *units;
        char *abrev;
} G2_TBLE2;

typedef struct g2_ens {
	int type;
	int id;
	int prod_id;
} G2_ENS;

typedef struct g2_ens_list {
	int ens_ix;
	G2_ENS ens;
	int n_it;
	G2_GIT *it_vals;
	int n_ft;
	int *ft_vals;
	int n_lv;
	float *lv_vals;
	float *lv_vals1;
	struct g2_it_list *thelist;
	struct g2_ens_list *next;
} G2_ENSLIST;

typedef struct g2_it_list {
    G2_GIT    it;
    int n_ft;
    int *ft_vals;
    int n_lv;
    float *lv_vals;
    float *lv_vals1;
    struct g2_ft_list   *thelist;
    struct g2_it_list   *next;
} G2_ITLIST;

typedef struct g2_ft_list {
    int ft;
    int n_lv;
    float *lv_vals;
    float *lv_vals1;
    struct _Grib2RecordInqRecList   *thelist;
    struct g2_ft_list   *next;
} G2_FTLIST;


/*
 * Derived from NCEP WMO GRIB2 Documentation
 * http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc.shtml
 *
 * NCEP WMO GRIB2 Documentation
 *    http://www.nco.ncep.noaa.gov/pmb/docs/grib2/grib2_doc.shtml
 *
 * ECMWF uses the WMO definitions, and does not (seem to) produce
 * tables that indicate its choices for variable short names.
 */

typedef struct _g2codeTable {
    int oct;
    char    *cat;
    char    *descrip;
    char    *shname;
    char    *units;
} g2codeTable;

typedef struct  _g2date_time {
    int year;
    int mon;
    int day;
    int hour;
    int min;
    int sec;
    long    dataDate;
    int     dataTime;
} G2date_time;

typedef struct _g2Sec0 {
    char    gid[5];
    int secid;
    int seclen;
    int discipline;
    int edition;
    int msglen;
} G2Sec0;

typedef struct _g2Sec1 {
    int secid;
    int centerID;
    char    *center_name;
    int subcenterID;
    char    *subcenter_name;
    int master_table_ver;
    int local_table_ver;
    int ref_time;
    char    *sig_ref_time;
    G2date_time date_time;
    int prod_status;
    char    *proc_prod_status;
    int data_type;
    char    *proc_data_type;
} G2Sec1;

typedef struct _g2Sec2 {
    int secid;
    int locallen;
    unsigned char   *local;
} G2Sec2;


/* scale factor for grid lat/lon points */
# define    G2_SCALE_FACTOR     1000000

typedef struct  _g2shapeOfEarth {
/*    char    *earthShape;*/
    int shapeOfEarth;
    int scale_factor_rad_sph_earth;
    int scaled_val_rad_sph_earth;
    int scale_factor_maj_axis_obl_sph_earth;
    int scaled_val_maj_axis_obl_sph_earth;
    int scale_factor_min_axis_obl_sph_earth;
    int scaled_val_min_axis_obl_sph_earth;
    int npts_along_parallel;
    int npts_along_meridian;
    int angl_init_prod_domain;
    int subdiv_basic_angle;
    int lat_first_gridpt;   /* not scaled; factor == 1000000 */
    int lat_last_gridpt;    /* not scaled; factor == 1000000 */
    int lon_first_gridpt;   /* not scaled; factor == 1000000 */
    int lon_last_gridpt;    /* not scaled; factor == 1000000 */
    int idir_incr;          /* not scaled; factor == 1000000 */
    int jdir_incr;          /* not scaled; factor == 1000000 */
    float   idir_incr_scaled;
    float   jdir_incr_scaled;
} G2shapeOfEarth;

typedef struct  _g2resComponentFlags {
    int   idir_given;
    int   jdir_given;
    int   uv_vectors;
} G2resComponentFlags;

typedef struct  _g2scanModeFlags {
    int   idir;
    int   jdir;
    int   adj_ijdir_consec;
    int   scan_dir;
} G2scanModeFlags;

/* Grid Def Template (GDS) */
typedef struct _g2Sec3 {
    int secid;
    int grid_def_src;
    int grid_num;
    char    *grid_def_name;
    int num_grid_data_pts;
    int num_oct_opt;
    int interp_opt_num_pts;
    char    *interp_opt_name;
    int *grid_list_num_oct_opt;
    int grid_list_num_oct_num;
    int grid_def_templ_num;
    G2shapeOfEarth  *shape_of_earth;
    G2resComponentFlags *res_comp;
    G2scanModeFlags *scan_mode;
    float   lat_first_gridpt;       /* scaled */
    float   lon_first_gridpt;       /* scaled */
    float   lat_last_gridpt;        /* scaled */
    float   lon_last_gridpt;        /* scaled */
} G2Sec3;

/* PDS Parameters */
typedef struct _g2prodParams {
    /* variable info */
    int    param_cat;
    char    *param_cat_name;
    int    param_num;
    char    *param_name;
    char    *short_name;
    char    *units;

    /* generating process info */
    unsigned int    gen_process;
    char    *gen_proc_name;
    int    bkgd_gen_process;
    int    gen_processID;
    int    hrs_after_reftime_cutoff;
    int    min_after_reftime_cutoff;
    int    time_range;
    char    *time_range_unit;
    int    forecast_time;

    /* horizontal layer / level info */
    unsigned int    typeof_first_fixed_sfc;
    char    *first_fixed_sfc;
    char    *units_first_fixed_sfc;
    int    scale_factor_first_fixed_sfc;
    int    scaled_val_first_fixed_sfc;

    unsigned int    typeof_second_fixed_sfc;
    char    *second_fixed_sfc;
    char    *units_second_fixed_sfc;
    int    scale_factor_second_fixed_sfc;
    int    scaled_val_second_fixed_sfc;

    /* level: scaled val of first sfc / 100 */
    int level;

    /* ensemble info */
    unsigned int    typeof_ensemble_fx;
    char    *ensemble_fx_type;
    unsigned int    perturb_num;
    unsigned int    num_fx_ensemble;
    /* statistical processing */
    int    year_end_overall_time_interval;
    int    mon_end_overall_time_interval;
    int    day_end_overall_time_interval;
    int    hour_end_overall_time_interval;
    int    min_end_overall_time_interval;
    int    sec_end_overall_time_interval;
    int    num_timerange_spec_time_interval_calc;
    int    total_num_missing_data_vals;
    int    typeof_stat_proc;
    char    *stat_proc;
    int    typeof_incr_betw_fields;
    char    *incr_betw_fields;
    int    ind_time_range_unit_stat_proc_done;
    char    *itr_unit;
    int    len_time_range_unit_stat_proc_done;
    int    ind_time_unit_incr_succ_fields;
    char    *itr_succ_unit;
    unsigned int    time_incr_betw_fields;
} G2prodParams;

/* Product Def Template (PDS) */
typedef struct _g2Sec4 {
    int secid;
    int pds_num;
    char    *prod_def_name;
    G2prodParams    *prod_params;
    int num_coord;
    float   *coord_list;
} G2Sec4;

typedef struct _g2dataRepr {
    double   refVal;
    int bin_scale_factor;
    int dec_scale_factor;
    int    nbits_packed_val;
    int    typeof_field_vals;
    char    *field_vals;
} G2dataRepr;

/* Data Representation Template Section (DRS) */
typedef struct _g2Sec5 {
    int secid;
    int drt_templ_num;
    char    *drt_desc;
    G2dataRepr  *data_repr;
    int ndpts;
} G2Sec5;

/* Bitmap Section */
typedef struct _g2Sec6 {
    int secid;
    int unpacked;
    int expanded;
    int bmap_ind;
    char    *bmap_desc;
    int *bmap;
} G2Sec6;

typedef struct _g2Sec7 {
    int secid;
    int offset;
    void*   data;
} G2Sec7;

typedef struct  _g2Rec {
    off_t offset;
    int rec_size;
    int version;        /* GRIB version */
    char *table_source_name;
    G2Sec0  sec0;
    G2Sec1  sec1;
    /*
     * Sections 2 through 7 are repeatable
     * as 2-7, 3-7 or 4-7
     * 
     * Section 2 is optional (local use)
     */
    int numrecs;        /* total number of GRIB2 records */
    int num_rptd;       /* number of times sec 2-7 repeated */
    G2Sec2** sec2;
    G2Sec3** sec3;
    G2Sec4** sec4;
    G2Sec5** sec5;
    G2Sec6** sec6;
    G2Sec7** sec7;
} G2Rec;


#if 0
/* Grid Def Template (GDS) */
typedef struct _g2GDS {
    int secid;
    int grid_def_src;
    int grid_num;
    char    *grid_def_name;
    int num_grid_data_pts;
    int num_oct_opt;
    int interp_opt_num_pts;
    char    *interp_opt_name;
    int *grid_list_num_oct_opt;
    int grid_list_num_oct_num;
    int grid_def_templ_num;
    G2shapeOfEarth  *shape_of_earth;
    G2resComponentFlags *res_comp;
    G2scanModeFlags *scan_mode;
    float   lat_first_gridpt;       /* scaled */
    float   lon_first_gridpt;       /* scaled */
    float   lat_last_gridpt;        /* scaled */
    float   lon_last_gridpt;        /* scaled */
} G2_GDS;
#endif

typedef G2Sec3 G2_GDS;

/* Product Def Template (PDS) */
typedef struct _g2PDS {
    int secid;
    int pds_num;
    char    *prod_def_name;
    G2prodParams    *prod_params;
    int num_coord;
    float   *coord_list;
} G2_PDS;


typedef struct  _Grib2FileRecord        Grib2FileRecord;
typedef struct  _Grib2RecordInqRec      Grib2RecordInqRec;
typedef struct  _Grib2DimInqRec         Grib2DimInqRec;
typedef struct  _Grib2AttInqRec         Grib2AttInqRec;
typedef struct  _Grib2RecordInqRecList  Grib2RecordInqRecList;
typedef struct  _Grib2DimInqRecList     Grib2DimInqRecList;
typedef struct  _Grib2AttInqRecList     Grib2AttInqRecList;
typedef struct  _Grib2ParamList         Grib2ParamList;
typedef struct  _Grib2InternalVarList   Grib2InternalVarList;
typedef struct  _Grib2InternalVarRec    Grib2InternalVarRec;
typedef struct  _NclGrib2FVarRec        NclGrib2FVarRec;
typedef struct  _NclGrib2CacheRec       NclGrib2CacheRec;
typedef struct  _NclGrib2CacheList      NclGrib2CacheList;
typedef struct  _Grib2Options           Grib2Options;

struct _NclGrib2CacheList {
    int grid_number;
    int has_gds;
    int grid_gds_tbl_index;
    int n_dims;
    int dimsizes[3];
    int n_entries;
    struct _NclGrib2CacheList   *next;
    struct _NclGrib2CacheRec    *thelist;
    struct _NclGrib2CacheRec    *tail;
};

struct _NclGrib2CacheRec {
    struct _NclGrib2CacheRec    *prev;
    struct _NclGrib2CacheRec    *next;
    Grib2RecordInqRec   *rec;
    NclMultiDValData    thevalue;
};

struct _NclGrib2FVarRec {
    NclQuark var_name_quark;
    NclQuark long_name_q;
    NclQuark units_q;
    NclBasicDataTypes data_type;
    int doff;
    int num_dimensions;
    int dim_sizes[NCL_MAX_DIMENSIONS];
    int file_dim_num[NCL_MAX_DIMENSIONS];
};
struct _Grib2InternalVarList {
    Grib2InternalVarRec     *int_var;
    Grib2InternalVarList    *next;
};

struct _Grib2InternalVarRec {
    NclQuark    var_name_q;
    NclGrib2FVarRec var_info;
    NclMultiDValData    value;
    int n_atts;
    Grib2AttInqRecList  *theatts;
};

/*
 * The following structure is used to distinguish records that
 * need to be separate variables. 
 * A comparision of the GDS values is also needed;
 */
typedef struct _Grib2VarTraits { 
	int center;
	int subcenter;
	int prod_status;
	int proc_data_type;
	int sig_ref_time;
	int pds_template;
	int discipline;
	int param_cat;
	int param_number;
	int stat_proc_type;
	int first_level_type;
	int second_level_type;
} Grib2VarTraits;

struct _Grib2ParamList {
    int param_number;
    Grib2VarTraits traits;
    int grid_number;
    int n_entries;
    int time_range_indicator;
    int time_period;            /* 0 unless ave,diff, or acc; then: (p2 - p1) */
    int time_unit_indicator;
    int variable_time_unit;
    int level_indicator;
    G2_GIT minimum_it;
    NclGrib2FVarRec var_info;
    NrmQuark    aux_coords[2];
    int ensemble_isatt;
    NclMultiDValData ensemble;
    NclOneDValCoordData ens_indexes;
    int yymmddhh_isatt;
    NclOneDValCoordData yymmddhh;
    G2_GIT *it_vals;
    int forecast_time_isatt;
    NclOneDValCoordData forecast_time;
    int levels_isatt;
    int levels_has_two;
    NclOneDValCoordData levels;
    NclMultiDValData    levels0;
    NclMultiDValData    levels1;
    Grib2RecordInqRecList   *thelist;
    int n_atts;
    Grib2AttInqRecList  *theatts;
    Grib2ParamList  *next;
};

struct _Grib2RecordInqRecList {
    Grib2RecordInqRec   *rec_inq;
    Grib2RecordInqRecList   *next;
};

struct _Grib2DimInqRecList {
    Grib2DimInqRec  *dim_inq;
    Grib2DimInqRecList  *next;
};

struct _Grib2AttInqRecList {
    Grib2AttInqRec  *att_inq;
    Grib2AttInqRecList  *next;
};


struct _Grib2RecordInqRec {
    off_t offset;
    int field_num;
    int rec_size;
    Grib2VarTraits traits;
    NclQuark    var_name_q;
    int param_number;
    G2_TBLE2   *ptable_rec;
    int grid_number;
    int version;
    int rec_num;
    int center;
    int sub_center;
    char *table_source;
    /*
     * Time offset from beginning reference time of the parameter set.
     * Units are set in the Grib2ParamList structure
     */
    G2_GIT initial_time;
    long time_offset;

    /* this is P2 - P1 -- the period for acc, avg and diff type records */
    int time_period;
    int level_indicator;
    float level0;
    float level1;

    int time_range_indicator;
    int time_unit_indicator;
    int per1,   /* P1 */
        per2;   /* P2 */

    int year;
    int mon;
    int day;
    int hour;
    int min;
    int sec;

    int gen_proc;       /* model from originating center; defined in GDS */

    char    *var_name;
    NclQuark    long_name_q;
    NclQuark    units_q;
    unsigned int    bds_flags;
    unsigned int    bds_size;
    int int_or_float;
    G2_PDS  *pds;

    int has_gds;
    int gds_type;
    G2_GDS  *gds;
    int La1,    /* latitude */
        La2,
        Lo1,    /* longitude */
        Lo2;
    int res_comp_flags;
    int interp_method;   /* 0 - linear; 1 - cubic */

    int has_bms;
    unsigned int    bms_size;
    NclMultiDValData    the_dat;
    int is_ensemble;
    G2_ENS ens;
};

struct _Grib2DimInqRec {
    int dim_number;     /* assigned in order of occurence in grib record */
    NclQuark    dim_name;
    long    size;
    G2_GDS  *gds;       /* for horizontal dims only */
    int grid_number;    /* grid template number */
};
    
struct _Grib2AttInqRec {
    NclQuark    name;
    NclMultiDValData    thevalue;
};

# define    GRIB2_THINNED_GRID_INTERPOLATION_OPT    0
# define    GRIB2_INITIAL_TIME_COORDINATE_TYPE_OPT  1
# define    GRIB2_DEFAULT_NCEP_PTABLE_OPT           2
# define    GRIB2_NUM_OPTIONS                       3

struct _Grib2Options {
    NclQuark    name;
    NclBasicDataTypes   data_type;
    int n_values;
    void    *values;
};

struct _Grib2FileRecord {
    NclQuark    file_path_q;
    int wr_status;
    int n_vars;
    Grib2ParamList  *var_list;
    int n_internal_vars;
    Grib2InternalVarList    *internal_var_list;
    int total_dims;
    int n_scalar_dims;
    Grib2DimInqRecList  *scalar_dims;
    int n_ensemble_dims;
    Grib2DimInqRecList  *ensemble_dims;
    int n_it_dims;
    Grib2DimInqRecList  *it_dims;
    int n_ft_dims;
    Grib2DimInqRecList  *ft_dims;
    int n_lv_dims;
    Grib2DimInqRecList  *lv_dims;
    int n_grids;  /* number of lat/lon or real_im/lat/lon pairs */
    int n_grid_dims;
    Grib2DimInqRecList  *grid_dims;
    NclGrib2CacheList   *grib_grid_cache;
    int n_options;
    Grib2Options    *options;
};


typedef int (*Grib2UnPackData)(
#if NhlNeedProto
    FILE *                  /* fd */,
    void**                  /* output */,
    void**                  /* missing */,
    Grib2RecordInqRec*      /* therec */,
    Grib2ParamList*         /* thevarrec */
#endif
);  

typedef void (*Grib2GetGDSGrid)(
#if NhlNeedProto
    Grib2ParamList* thevarrec,
    float** lat,
    int * n_dims_lat,
    int ** dimsizes_lat,
    float ** lon,
    int * n_dims_lon,
    int **dimsizes_lon,
    float **rot,
    int * n_dims_rot,
    int **dimsizes_rot,
    Grib2AttInqRecList ** lat_att_list,
    int * nlatatts,
    Grib2AttInqRecList ** lon_att_list,
    int * nlonatts,
    Grib2AttInqRecList ** rot_att_list,
    int * nrotatts
#endif
);
    
typedef void (*Grib2GetGrid)(
#if NhlNeedProto
Grib2ParamList* thevarrec,
float** lat,
int * n_dims_lat,
int ** dimsizes_lat,
float ** lon,
int * n_dims_lon,
int **dimsizes_lon,
float **rot,
Grib2AttInqRecList ** lat_att_list,
int * nlatatts,
Grib2AttInqRecList ** lon_att_list,
int * nlonatts,
Grib2AttInqRecList ** rot_att_list,
int * nrotatts
#endif
);  

typedef void (*Grib2GetGridAtts)(
#if NhlNeedProto
Grib2ParamList*,
Grib2AttInqRecList ** /*lat_att_list*/,
int * /*nlatatts*/,
Grib2AttInqRecList ** /*lon_att_list*/,
int * /*lonatts*/,
int do_rot,
int grid_oriented,
Grib2AttInqRecList ** /*rot_att_list*/,
int * /*rotatts*/
#endif
);

typedef struct g2_gridinfo {
    Grib2UnPackData un_pack;
    Grib2GetGrid    get_grid;
    Grib2GetGridAtts    get_grid_atts;
    char    *grid_name;
} g2_GridInfoRecord;

typedef struct g2_gridgdsinfo {
    Grib2UnPackData un_pack;
    Grib2GetGDSGrid get_gds_grid;
    char    *grid_name;
} g2_GridGDSInfoRecord;

/*
 * Code table
 */
typedef struct codeTable {
    int oct;                /* GRIB2 file octet number */
    char    *cat;           /* category; typically numeric, can be character-based */
    char    *descrip;       /* description */
    char    *shname;        /* short name (not always provided and/or created */
    char    *units;         /* variable units (not always applicable) */
} g2_codeTable;

# define    G2_DEFAULT_MISSING_FLOAT \
        ((NclTypeClass) nclTypefloatClass)->type_class.default_mis.floatval

# define    G2_DEFAULT_MISSING_INT \
        ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval


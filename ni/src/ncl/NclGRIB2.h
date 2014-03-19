#include "NclMultiDValData.h"
#include "NclOneDValCoordData.h"


# define GRIB2EOF    0
# define GRIB2ERROR -1
# define GRIB2OK     1

typedef struct _git2{
	short year;
	short days_from_jan1;
	short minute_of_day;
	short seconds;
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
	int prob_type;
	int lower_limit_scale;
	int lower_limit_value;
	int upper_limit_scale;
	int upper_limit_value;
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
    int cat;
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

typedef struct _g2EarthParams {
	int shapeOfEarth;
	int scale_factor_rad_sph_earth;
	int scaled_val_rad_sph_earth;
	int scale_factor_maj_axis_obl_sph_earth;
	int scaled_val_maj_axis_obl_sph_earth;
	int scale_factor_min_axis_obl_sph_earth;
	int scaled_val_min_axis_obl_sph_earth;
} g2EarthParams;

typedef struct _g2CETemplate {    /* template 0 : lat/lon */
	g2EarthParams ep;
	int npts_along_parallel;
	int npts_along_meridian;
	int angl_init_prod_domain;
	int subdiv_basic_angle;
	int lat_first_gridpt;   /* not scaled; factor == 1000000 */
	int lon_first_gridpt;   /* not scaled; factor == 1000000 */
	int res_comp_flags;
	int lat_last_gridpt;    /* not scaled; factor == 1000000 */
	int lon_last_gridpt;    /* not scaled; factor == 1000000 */
	int idir_incr;          /* not scaled; factor == 1000000 */
	int jdir_incr;          /* not scaled; factor == 1000000 */
	int scan_mode_flags;
} g2CETemplate;

typedef struct _g2RotCETemplate {   /* template 1 : rotated lat/lon */
	g2CETemplate ce;
	int lat_south_pole_proj;
	int lon_south_pole_proj;
	int rot_ang_proj;
} g2RotCETemplate;

typedef struct _g2ArakawaRotLLTemplate {   /* template 1 : rotated lat/lon */
	g2EarthParams ep;
	int npts_along_parallel;
	int npts_along_meridian;
	int angl_init_prod_domain;
	int subdiv_basic_angle;
	int lat_first_gridpt;   /* not scaled; factor == 1000000 */
	int lon_first_gridpt;   /* not scaled; factor == 1000000 */
	int res_comp_flags;
	int center_lat;    /* not scaled; factor == 1000000 */
	int center_lon;    /* not scaled; factor == 1000000 */
	int idir_incr;          /* not scaled; factor == 1000000 */
	int jdir_incr;          /* not scaled; factor == 1000000 */
	int scan_mode_flags;
	int lat_last_gridpt;
	int lon_last_gridpt;
} g2ArakawaRLLTemplate;

typedef struct _g2METemplate {      /* template 10 : mercator */
	g2EarthParams ep;
	int npts_along_parallel;
	int npts_along_meridian;
	int lat_first_gridpt;   /* not scaled; factor == 1000000 */
	int lon_first_gridpt;   /* not scaled; factor == 1000000 */
	int res_comp_flags;
	int latD_intersect;
	int lat_last_gridpt;    /* not scaled; factor == 1000000 */
	int lon_last_gridpt;    /* not scaled; factor == 1000000 */
	int scan_mode_flags;
	int orientation;
	int idir_incr;          /* units of 10-3 meters (at latitude latD_intersect) */
	int jdir_incr;          /* units of 10-3 meters (at latitude latD_intersect) */
} g2METemplate;

typedef struct _g2STTemplate {   /* template 20 : polar stereographic */
	g2EarthParams ep;
	int npts_along_x_axis;
	int npts_along_y_axis;
	int lat_first_gridpt;   /* not scaled; factor == 1000000 */
	int lon_first_gridpt;   /* not scaled; factor == 1000000 */
	int res_comp_flags;
	int latD_intersect;
	int loV_orientation;    /* not scaled; factor == 1000000 */
	int dx_incr;            /* units of 10-3 meters (at latitude latD_intersect) */
	int dy_incr;            /* units of 10-3 meters (at latitude latD_intersect) */
	int proj_center_flag;
	int scan_mode_flags;
} g2STTemplate;

typedef struct _g2LCTemplate {   /* template 30 : lambert conformal */
	g2EarthParams ep;
	int npts_along_x_axis;
	int npts_along_y_axis;
	int lat_first_gridpt;   /* not scaled; factor == 1000000 */
	int lon_first_gridpt;   /* not scaled; factor == 1000000 */
	int res_comp_flags;
	int latD_intersect;
	int loV_central_meridian;    /* not scaled; factor == 1000000 */
	int dx_incr;            /* units of 10-3 meters (at latitude latD_intersect) */
	int dy_incr;            /* units of 10-3 meters (at latitude latD_intersect) */
	int proj_center_flag;
	int scan_mode_flags;
	int latin1; /* first latitude at which secant cone cuts the sphere */
	int latin2; /* second latitude at which secant cone cuts the sphere */
	int lat_south_pole_proj;
	int lon_south_pole_proj;
} g2LCTemplate;

typedef struct _g2GATemplate {    /* template 40 : gaussian lat/lon */
	g2EarthParams ep;
	int npts_along_parallel;
	int npts_along_meridian;
	int angl_init_prod_domain;
	int subdiv_basic_angle;
	int lat_first_gridpt;   /* not scaled; factor == 1000000 */
	int lon_first_gridpt;   /* not scaled; factor == 1000000 */
	int res_comp_flags;
	int lat_last_gridpt;    /* not scaled; factor == 1000000 */
	int lon_last_gridpt;    /* not scaled; factor == 1000000 */
	int idir_incr;          /* not scaled; factor == 1000000 */
	int nparallels_pole2equator;
	int scan_mode_flags;
} g2GATemplate;

typedef struct _g2SHTemplate {  /* template 50 */
	int j_pent_res;
	int k_pent_res;
	int m_pent_res;
	int rep_type;  /* table 3.6 */
	int rep_mode;  /* table 3.7 */
} g2SHTemplate;

typedef struct _g2SVTemplate {   /* template 90 : space view perspective or orthographic */
	g2EarthParams ep;
	int npts_along_x_axis;
	int npts_along_y_axis;
	int lat_sub_satellite_pt;
	int lon_sub_satellite_pt;
	int res_comp_flags;
	int dx;                 /* apparent diameter of Earth in grid lengths, x-direction */
	int dy;                 /* apparent diameter of Earth in grid lengths, y-direction */
	int xp_grid;            /* x-coordinate of sub-satellite point (units of 10-3 grid length as integer) */
	int yp_grid;            /* y-coordinate of sub-satellite point (units of 10-3 grid length as integer) */
	int scan_mode_flags;
	int orientation;        /* grid orientation (angle between the increasing y-axis and the sub-satellite point meridian */
	int Nr_altitude;        /* altitude of camera from center of earth (units of earth radius at equator x 10-6) */
	int x_origin;           /* x-coordinate of origin of sector image */
	int y_origin;           /* y-coordinate of origin of sector image */
} g2SVTemplate;


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
    int grid_num;          /* template */
    char    *grid_def_name;
    int num_grid_data_pts;
    int num_oct_opt;
    int interp_opt_num_pts;
    char    *interp_opt_name;
    int *grid_list_num_oct_opt;
    int grid_list_num_oct_num;
    int grid_def_templ_num;
    int len_grid_template;
    int *grid_template;
    int is_thinned_grid;
    int scan_mode_offset;
#if 0
    G2shapeOfEarth  *shape_of_earth;
    G2resComponentFlags *res_comp;
    G2scanModeFlags *scan_mode;
    float   lat_first_gridpt;       /* scaled */
    float   lon_first_gridpt;       /* scaled */
    float   lat_last_gridpt;        /* scaled */
    float   lon_last_gridpt;        /* scaled */
#endif
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
    int    time_range_unit_id;
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
    G2date_time end_overall_time_interval;
    int    num_timerange_spec_time_interval_calc;
    int    total_num_missing_data_vals;
    int    typeof_stat_proc;
    int    spatial_proc;
    char    *stat_proc;
    int    typeof_incr_betw_fields;
    char    *incr_betw_fields;
    int    ind_time_range_unit_stat_proc_done;
    char    *itr_unit;
    int    len_time_range_unit_stat_proc_done;
    int    ind_time_unit_incr_succ_fields;
    char    *itr_succ_unit;
    unsigned int    time_incr_betw_fields;
    int    p1, p2; /* for NCEP local stat definitions */
    int    n_grids;
    /* probability forecasts */
    int forecast_probability_number;
    int total_forecast_probabilities;
    int probability_type;
    int scale_factor_lower_limit;
    int scaled_value_lower_limit;
    int scale_factor_upper_limit;
    int scaled_value_upper_limit;
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
    int own_missing;
    NclScalar missing;
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
    size_t offset;
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
    int grid_index;
    int grid_number;
    int n_dims;
    ng_size_t dimsizes[3];
    int n_entries;
    NclMultiDValData int_missing_rec;       /* shared by all integer vars */
    NclMultiDValData float_missing_rec;     /* shared by all float vars */
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
    NclQuark param_q;
    NclBasicDataTypes data_type;
    int doff;
    int num_dimensions;
    ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
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
	int gen_process_type;
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
    int grid_index;
    G2_GDS  *gds;
    int n_entries;
    int forecast_time_iszero;
    int time_period;            /* 0 unless ave,diff, or acc; then: (p2 - p1) */
    int forecast_time_units;
    int time_period_units;
    int variable_time_unit; /* boolean: is there more than a single time unit */
    int p1; /* for NCEP local stat definitions */ 
    int p2; /* for NCEP local stat definitions */ 
    int n_grids; /* for NCEP local stat definitions */ 
    int level_indicator;
    int has_bmap;
    int has_own_missing;
    NclScalar missing;
    G2_GIT minimum_it;
    NclGrib2FVarRec var_info;
    NrmQuark    aux_coords[2];
    int ensemble_isatt;
    NclMultiDValData ensemble;
    NclOneDValCoordData ens_indexes;
    int prob_type;
    NclOneDValCoordData probability; /* the 'probability' dimension is shared with the ensemble dimension */
    NclMultiDValData lower_probs;
    NclMultiDValData upper_probs;
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
    Grib2RecordInqRec   *ref_rec;
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
    size_t offset;
    int field_num;
    int rec_size;
    int rec_num;
    int version;
    NrmQuark qcenter_name;

    Grib2VarTraits traits;
    NclQuark    var_name_q;
    char *table_source;

    /*
     * Time offset from beginning reference time of the parameter set.
     * Units are set in the Grib2ParamList structure
     */
    G2_GIT initial_time;
    G2_GIT end_overall_interval;
    int overall_interval_seconds;
    int forecast_time; /* from the template */
    long time_offset; /* converted to common units and adjusted to account for
			 the time period -- this is used as the NCL GRIB2 "forecast_time" */
    int time_period;
    int forecast_time_units;
    int time_period_units;
    int    p1, p2; /* for NCEP local stat definitions */
    int    n_grids;
    int spatial_proc;

    int level_indicator;
    float level0;
    float level1;

    int grid_number;
    G2_GDS  *gds;
    int interp_method;   /* 0 - linear; 1 - cubic */
    int has_bmap;
    int has_own_missing;
    NclScalar missing;

    unsigned int    bds_flags;
    int int_or_float;
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
    int grid_index;
};
    
struct _Grib2AttInqRec {
    NclQuark    name;
    NclMultiDValData    thevalue;
};

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
    int n_probability_dims;
    Grib2DimInqRecList  *probability_dims;
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
    int single_dims;
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
    ng_size_t ** dimsizes_lat,
    float ** lon,
    int * n_dims_lon,
    ng_size_t **dimsizes_lon,
    float **rot,
    int * n_dims_rot,
    ng_size_t **dimsizes_rot,
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
ng_size_t ** dimsizes_lat,
float ** lon,
int * n_dims_lon,
ng_size_t **dimsizes_lon,
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

#if 0
# define    G2_DEFAULT_MISSING_FLOAT \
        ((NclTypeClass) nclTypefloatClass)->type_class.default_mis.floatval
#endif
#define G2_DEFAULT_MISSING_FLOAT 1e20
# define    G2_DEFAULT_MISSING_INT \
        ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval


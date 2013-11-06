/*
 *	$Id: gks.h,v 1.4 2008-07-27 03:55:37 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

#ifndef _gks_h
#define _gks_h

#include <stdio.h>
#include <sys/types.h>
#include <ncarg/c.h>

#ifdef  __cplusplus
#define NCARG_PROTO_BEGIN       extern "C" {
#define NCARG_PROTO_END         }
#else
#define NCARG_PROTO_BEGIN
#define NCARG_PROTO_END 
#endif

#if defined(cray)
#include <fortran.h>
#endif
    
/*
 *    A.1 Data Types in Compilation Order
 */
/* 
 *    A.1.1 Basic Types
 */

typedef double     Gdouble;         /* double precision number  */
typedef float      Gfloat;          /* floating point number    */
typedef int        Gint;            /* integer                  */ 
typedef Gfloat Gtran_matrix[2][3];  /* transformation matrix    */

typedef struct {
    size_t size;   /* size of data    */
    void   *data;  /* pointer to data */
} Gdata;

/*
 *    A.1.2 Enumeration Types
 */

typedef enum {                     /* aspect source flag  */
    GASF_BUNDLED,
    GASF_INDIV
} Gasf;

typedef enum {
    GATTR_LINE,
    GATTR_MARKER,
    GATTR_TEXT,
    GATTR_FILL
}  Gattrs;

typedef enum {                     /* clipping indicator  */
    GIND_NO_CLIP,
    GIND_CLIP
} Gclip_ind;

typedef enum {                      /* colour available */
    GAVAIL_MONOCHR,
    GAVAIL_COLR
} Gcolr_avail;

typedef enum {                     /* control flag  */
    GFLAG_COND,
    GFLAG_ALWAYS
} Gctrl_flag;

typedef enum {                     /* device coordinate units */
    GDC_METRES,
    GDC_OTHER
} Gdc_units;

typedef enum {
    GDEFER_ASAP,
    GDEFER_BNIG,
    GDEFER_BNIL,
    GDEFER_ASTI
} Gdefer_mode;

typedef enum {                     /* display space empty */
    GDISP_NOT_EMPTY,
    GDISP_EMPTY
} Gdisp_space_empty;

typedef enum {                     /* echo switch */
    GSWITCH_NO_ECHO,
    GSWITCH_ECHO
} Gecho_switch;

typedef enum {                     /* fill area interior style */
    GSTYLE_HOLLOW,
    GSTYLE_SOLID,
    GSTYLE_PAT,
    GSTYLE_HATCH,
            GSTYLE
} Gfill_int_style;

typedef enum {                     /* horizontal text alignment */
    GHOR_NORM,
    GHOR_LEFT,
    GHOR_CTR,
    GHOR_RIGHT
} Ghor_text_align;

typedef enum {                     /* [input] status */
    GIN_STATUS_OK,
    GIN_STATUS_NONE,
    GIN_STATUS_NO_IN
} Gin_status;

typedef enum {                     /* inquire type */
    GINQ_SET,
    GINQ_REALIZED
} Ginq_type;

typedef enum {                     /* implicit regeneration mode */
    GIRG_SUPPR,
    GIRG_ALLOWED
} Girg_mode;

typedef enum {                     /* GKS level */
    GLEVEL_0A, GLEVEL_0B, GLEVEL_0C, GLEVEL_1A, GLEVEL_1B,
    GLEVEL_1C, GLEVEL_2A, GLEVEL_2B, GLEVEL_2C
} Glevel;

typedef enum {                  /* new frame [action] necessary [at] update */
    GNEW_NO,
    GNEW_YES
} Gnew_frame_nec_upd;

typedef enum {                     /* operating mode */
    GOP_REQ,
    GOP_SAMPLE,
    GOP_EVENT
} Gop_mode;

typedef enum {                     /* operating state */
    GST_GKCL,
    GST_GKOP,
    GST_WSOP,
    GST_WSAC,
    GST_SGOP
} Gop_st;

typedef enum {                     /* presence [of] invalid [values] */
    GINVAL_ABSENT,
    GINVAL_PRESENT
} Gpres_inval;

typedef enum {                     /* text path */
    GPATH_RIGHT,
    GPATH_LEFT,
    GPATH_UP,
    GPATH_DOWN
} Gtext_path;

typedef enum {                     /* text precision */
    GPREC_STRING,
    GPREC_CHAR,
    GPREC_STROKE
} Gtext_prec;

typedef enum {
    GFLAG_POSTPONE,
    GFLAG_PERFORM
} Gupd_regen_flag;

typedef enum {
    GUPD_NOT_PEND,
    GUPD_PEND
} Gupd_st;

typedef enum {                     /* vertical text alignment */
    GVERT_NORM,
    GVERT_TOP,
    GVERT_CAP,
    GVERT_HALF,
    GVERT_BASE,
    GVERT_BOTTOM
} Gvert_text_align;

typedef enum {                     /* workstation category */
    GCAT_OUT,
    GCAT_IN, 
    GCAT_OUTIN, 
    GCAT_WISS,
    GCAT_MO,
    GCAT_MI
} Gws_cat;

typedef enum {                     /* workstation classification */
    GCLASS_VEC,
    GCLASS_RASTER,
    GCAT_OTHER
} Gws_class;

typedef enum {                     /* workstation state */
    GWS_INACTIVE,
    GWS_ACTIVE
} Gws_st;

/*
 *    A.1.3 Simple Structures
 */

typedef union {
    Gdata pet_r1;  /* pet 1 data  */
    Gdata pet_u1;  /* pet -1 data */
                   /* etc.        */
} Gstring_data;
          
		   
typedef struct {                   /* aspect source flags */
    Gasf linetype;
    Gasf linewidth;
    Gasf line_colr_ind;
    Gasf marker_type;
    Gasf marker_size;
    Gasf marker_colr_ind;
    Gasf text_font_prec;
    Gasf char_expan;
    Gasf char_space;
    Gasf text_colr_ind;
    Gasf fill_int_style;
    Gasf fill_style_ind;
    Gasf fill_colr_ind;
} Gasfs;

typedef struct {
    Gint        num_colrs;     /* num. of colours               */
    Gcolr_avail colr_avail;    /* colour availability           */
    Gint        num_pred_inds; /* num. of predef. coour indices */
} Gcolr_facs;

typedef enum {
    GCOORD_WC,
    GCOORD_NDC
} Gcoord_switch;

typedef struct {               /* fill area bundle */
    Gfill_int_style int_style; /* fill area interior style  */
    Gint            style_ind; /* fill area style index     */
    Gint            colr_ind;  /* fill area colour index    */
} Gfill_bundle;

typedef struct {   /* float size */
    Gfloat size_x; /* x size */
    Gfloat size_y; /* y size */
} Gfloat_size;

typedef struct {   /* integer list */
    Gint num_ints; /* num. of integers in list */
    Gint *ints;    /* list of integers         */
} Gint_list;

typedef struct {  /* integer size */
    Gint size_x;  /* x size */
    Gint size_y;  /* y size */
} Gint_size;

typedef struct {   /* limit */
    Gfloat x_min;  /* x min */
    Gfloat x_max;  /* x max */
    Gfloat y_min;  /* y min */
    Gfloat y_max;  /* y min */
} Glimit;

typedef struct {      /* polyline bundle */
    Gint    type;     /* linetype               */
    Gdouble width;    /* linewidth scale factor */
    Gint    colr_ind; /* polyline colour index  */
} Gline_bundle;

typedef struct {      /* polymarker bundle */
    Gint    type;     /* marker type              */
    Gdouble size;     /* marker size scale factor */
    Gint    colr_ind; /* polymarker colour index  */
} Gmarker_bundle;

typedef struct {          /* maximum [length of] workstation state tables */
                          /* max. num. of :                  */
    Gint line_bundles;    /* polyline bundle table entries   */
    Gint marker_bundles;  /* polymarker bundle table entries */
    Gint text_bundles;    /* text bundle table entries       */
    Gint fill_bundles;    /* fill area bundle table entries  */
    Gint pat_reps;        /* pattern table entries           */
    Gint colr_reps;       /* colour table entries            */
} Gmax_ws_st_tables;

typedef struct {    /* point */
    Gfloat x;       /* x coordinate */
    Gfloat y;       /* y coordinate */
} Gpoint;

typedef struct {             /* text alignment */
    Ghor_text_align   hor;   /* horizontal component */
    Gvert_text_align  vert;  /* vertical component   */
} Gtext_align;

typedef struct {        /* text font and precision */
    Gint         font;  /* text font      */
    Gtext_prec   prec;  /* text precision */
} Gtext_font_prec;

typedef struct {     /* vector */
    Gfloat delta_x;  /* x coordinate */
    Gfloat delta_y;  /* y coordinate */
} Gvec;

/*
 *    A.1.4 Nested Structures
 */

typedef struct {          /* clipping */
    Gclip_ind clip_ind;   /* clipping indicator */
    Glimit    clip_rect;  /* clipping rectangle */
} Gclip;

typedef struct {             /* display space size */
    Gdc_units   dc_units;    /* device coordinate units           */
    Gfloat_size size_dc;     /* displace size [in] dc [units]     */
    Gint_size   size_raster; /* displace size [in] raster [units] */
} Gdisp_space_size;

typedef struct {                    /* fill area facilities */
    Gint            num_int_styles; /* num. of interior styles           */
    Gfill_int_style int_style[4];   /* list of available interior styles */
    Gint_list       hatch_styles;   /* list of available hatch styles    */
    Gint            num_pred_inds;  /* num. of predef. fill area indices */
} Gfill_facs;

typedef struct {                     /* individual attributes */
    Gint            linetype;        /* linetype                   */
    Gdouble         linewidth;       /* linewidth scale factor     */
    Gint            line_colr_ind;   /* polyline color index       */
    Gint            marker_type;     /* marker type                */
    Gdouble         marker_size;     /* marker size                */
    Gint            marker_colr_ind; /* polymarker color index     */
    Gtext_font_prec text_font_prec;  /* text font and precision    */
    Gdouble         char_expan;      /* character expansion factor */
    Gdouble         char_space;      /* character spacing          */
    Gint            text_colr_ind;   /* text color index           */
    Gfill_int_style fill_int_style;  /* fill area interior style   */
    Gint            fill_style_ind;  /* fill area style index      */
    Gint            fill_colr_ind;   /* fill area color index      */
    Gasfs           asfs;            /* aspect source flags        */
} Gindiv_attrs;

typedef struct {             /* polyline facilities */
    Gint_list types;         /* list of linetypes                */
    Gint      num_widths;    /* num. of available linewidths     */
    Gdouble   nom_width;     /* nominal linewidth                */
    Gdouble   min_width;     /* min. linewidth                   */
    Gdouble   max_width;     /* max. linewidth                   */
    Gint      num_pred_inds; /* num. of predef. polyline indices */
} Gline_facs;

typedef struct {              /* polymarker facilities */
    Gint_list types;          /* list of marker types               */
    Gint      num_sizes;      /* num. of available marker sizes     */
    Gdouble   nom_size;       /* nominal marker size                */
    Gdouble   min_size;       /* min. marker size                   */
    Gdouble   max_size;       /* max. marker size                   */
    Gint      num_pred_inds;  /* num. of predef. polymarker indices */
} Gmarker_facs;

typedef struct {            /* pattern representation */
    Gint_size dims;         /* pattern's dimensions */
    Gint      *colr_array;  /* colour array         */
} Gpat_rep;

typedef struct {                /* primitive attributes */
    Gint        line_ind;       /* polyline index          */
    Gint        marker_ind;     /* polymarker index        */
    Gint        text_ind;       /* text index              */
    Gdouble     char_ht;        /* character height        */
    Gvec        char_up_vec;    /* character up vector     */
    Gdouble     char_width;     /* character width         */
    Gvec        char_base_vec;  /* character base vector   */
    Gtext_path  text_path;      /* text path               */
    Gtext_align text_align;     /* text alignment          */
    Gint        fill_ind;       /* fill area index         */
    Gvec        pat_width_vec;  /* pattern width vector    */
    Gvec        pat_ht_vec;     /* pattern height vector   */
    Gpoint      pat_ref_point;  /* pattern reference point */
} Gprim_attrs;

typedef struct {        /* point list */
    Gint   num_points;  /* num. of points in the list */
    Gpoint *points;     /* list of points             */
} Gpoint_list;

typedef struct {     /* rectangle */
    Gpoint p;        /* point p */
    Gpoint q;        /* point q */
} Grect;

typedef struct {    /* red green blue color specification */
    Gfloat   red;   /* red intensity   */
    Gfloat green;   /* green intensity */
    Gfloat  blue;   /* blue intensity  */
} Grgb;

typedef struct {         /* text extent */
    Gpoint concat_point; /* concatenation point       */
    Gpoint paral[4];     /* text extent parallelogram */
} Gtext_extent;

typedef struct {                    /* text bundle */
    Gtext_font_prec text_font_prec; /* text font and precision */
    Gdouble         char_expan;     /* character expansion     */
    Gdouble         char_space;     /* character spacing       */
    Gint            colr_ind;       /* text colour index       */
} Gtext_bundle;

typedef struct {                     /* text facilities */
    Gint            num_font_precs;  /* num. of fonts and precision     */
    Gtext_font_prec *font_precs;     /* list of fonts and precisions    */
    Gint            num_char_hts;    /* num. of character heights       */
    Gdouble         min_char_ht;     /* minimum character height        */
    Gdouble         max_char_ht;     /* maximum character height        */
    Gint            num_char_expans; /* num. of char. expansion factors */
    Gdouble         min_char_expan;  /* minimum expansion factor        */
    Gdouble         max_char_expan;  /* maximum expansion factor        */
    Gint            num_pred_inds;   /* num. of predef. text indices    */
} Gtext_facs;

typedef struct {        /* transformation */
    Glimit win;         /* window   */
    Glimit  vp;         /* viewport */
} Gtran;

typedef struct {        /* workstation maximum numbers */
    Gint simult_open;   /* max. num. of simult. open wss            */
    Gint simult_active; /* max. num. of simult. active wss          */
    Gint assoc_seg;     /* max. num. of wss associated with segment */
} Gws_max_nums;

/*
 *    A.1.5 Implementation Dependent Types
 */

typedef	struct _GstoreRec_	*Gstore;         /* store                    */

typedef union {
    Grgb   rgb;  /* Red Green Blue colour specification  */
} Gcolr_rep;

typedef union {
	/*
	 * These are just generic ones - Since the union is made up of all
	 * the same type you don't really have to use the one named for
	 * what you are using.
	 */
    Gdata escape_r1; /* escape 1 data record  */
    Gdata escape_u1; /* escape -1 data record */
                     /* etc.                  */
	/* These are the actual ones defined by NCAR GKS */
    Gdata escape_u1391;	/*	set Metafile name for cgm workstation	*/
    Gdata escape_u1392; /*	FLASH4 support				*/
    Gdata escape_u1393;	/*	Picture name				*/
    Gdata escape_u1394;	/*	root for segment filenames		*/
    Gdata escape_u1395;	/*	Cause a pause in ctrans processing	*/
    Gdata escape_u1396;	/*	pause in X workstation			*/
    Gdata escape_u1398;	/*	Max number of error messages for abort	*/
    Gdata escape_u1399;	/*	GKS clipping				*/
    Gdata escape_u1400;	/*	Color sensitivity - X workstation	*/
    Gdata escape_u1401;	/*	Private Colormap - X workstation	*/
    Gdata escape_u1510;	/*	beg seg copy PS				*/
    Gdata escape_u1511;	/*	end seg copy PS				*/
    Gdata escape_u1512;	/*	Spacing between fill lines PS		*/
    Gdata escape_u1513;	/*	Spacing between hatch lines PS		*/
    Gdata escape_u1514;	/*	max size stack PS			*/
    Gdata escape_u1515;	/*	Max points in path PS			*/
    Gdata escape_u1516;	/*	Scale factor nom-linewidth PS		*/
    Gdata escape_u1517;	/*	Background fills entire page PS		*/
    Gdata escape_u1518;	/*	Line joins PS				*/
    Gdata escape_u1519;	/*	Line caps PS				*/
    Gdata escape_u1520;	/*	Miter limit PS				*/
    Gdata escape_u1521;	/*	Coord points for pict positioning PS	*/
    Gdata escape_u1522;	/*	Scale factor for coordinates PS		*/
} Gescape_in_data;

typedef union {
    Gdata escape_r1; /* escape 1 data record  */
    Gdata escape_u1; /* escape -1 data record */
                     /* etc.                  */
	/*
	 * Actually used by NCARG GKS.
	 */
    Gdata escape_u1390;	/* returns the string "NCAR_GKS0A--VERSION_4.0"	*/
				/* don't forget to use Gstore! */
} Gescape_out_data;

typedef union {
    Gdata gdp_r1;    /* escape 1 data record */
    Gdata escape_u1; /* escape -1 data record */
                     /* etc. */
} Ggdp_data;

typedef struct {
    Gint type;                         /* item type                   */
    Gint length;                       /* item data record length     */
    union {
        Gctrl_flag clear_ws;           /* control flag                */
        Gupd_regen_flag upd_ws;        /* regen. flag                 */
        struct {
            Gdefer_mode defer_mode;    /* deferral mode               */
            Girg_mode irg_mode;        /* irg mode                    */
        } defer_st;                    /* deferral state              */
        /* etc. */
        Gdata gdp_unsupp;              /* GDPs not supported by impl. */
        Gdata impl_dep;                /* impl. dependent             */
    } data;
} Gitem_data;



/* This macro protects C function names from C++ name-mangling. */
NCARG_PROTO_BEGIN

/*
 *    A.3.1 Control Functions
 */

extern void gactivate_ws(
#ifdef  NeedFuncProto
    Gint ws_id  /* workstation identifier */
#endif
);

extern void gclose_gks(
#ifdef  NeedFuncProto
    void
#endif
);

extern void gclear_ws(
#ifdef  NeedFuncProto
    Gint ws_id,             /* workstation identifier */
    Gctrl_flag  ctrl_flag   /* control flag           */
#endif
);

extern void gupd_ws(
#ifdef  NeedFuncProto
    Gint            ws_id,           /* workstation identifier   */
    Gupd_regen_flag upd_regen_flag   /* update regeneration flag */
#endif
);

extern void gclose_ws(
#ifdef  NeedFuncProto
    Gint ws_id  /* workstation identifier */
#endif
);

extern void gdeactivate_ws(
#ifdef  NeedFuncProto
    Gint ws_id  /* workstation identifier */
#endif
);

extern void gescape(
#ifdef  NeedFuncProto
    Gint                   func_id,      /* escape function identifier    */
    const Gescape_in_data  *in_data,     /* escape input data record      */
    Gstore                 *store_data,  /* storage for output data       */
    Gescape_out_data       **out_data    /* OUT escape output data record */
#endif
);

extern void gopen_gks(
#ifdef  NeedFuncProto
    const char   *err_file,   /* name of error file                */
    size_t        mem_unit    /* size_t units of memory available
                                 for buffer space                  */
#endif
);

extern void gopen_ws(
#ifdef  NeedFuncProto
    Gint        ws_id,      /* workstation identifier  */
    const char  *conn_id,   /* connection identifier   */
    Gint        ws_type     /* workstation type        */
#endif
);


/*
 *    A.3.2 Output Functions
 */

extern void gcell_array(
#ifdef  NeedFuncProto
    const Grect    *rect,        /* cell rectangle */
    const Gpat_rep *colr_array   /* colour array   */
#endif
);

extern void gfill_area(
#ifdef  NeedFuncProto
    const Gpoint_list *point_list    /* list of points */
#endif
);

extern void gpolyline(
#ifdef  NeedFuncProto
    const Gpoint_list *point_list /* list of points */
#endif
);

extern void gpolymarker(
#ifdef  NeedFuncProto
    const Gpoint_list *point_list /* list of points */
#endif
);

extern void gtext(
#ifdef  NeedFuncProto
    const Gpoint *text_pos,    /* text position    */
    const char   *char_string  /* character string */
#endif
);

extern void ggdp(
#ifdef  NeedFuncProto
    const Gpoint_list   *point_list, /* list of points  */
    Gint                gdp_id,      /* gdp identifier  */
    const Ggdp_data     *gdp_data    /* gdp data record */
#endif
);

/*
 *    A.3.3 Output Attribute Functions
 */

extern void gset_line_ind(
#ifdef  NeedFuncProto
    Gint line_ind  /*  polyline index  */
#endif
);

extern void gset_line_colr_ind(
#ifdef  NeedFuncProto
    Gint line_colr_ind  /* polyline colour index */
#endif
);

extern void gset_marker_ind(
#ifdef  NeedFuncProto
    Gint marker_ind  /*  polymarker index  */
#endif
);

extern void gset_asfs(
#ifdef  NeedFuncProto
    const Gasfs *list_asf  /* list of aspect source flags */
#endif
);

extern void gset_char_ht(
#ifdef  NeedFuncProto
    Gdouble char_ht  /* character height */
#endif
);

extern void gset_char_space(
#ifdef  NeedFuncProto
    Gdouble char_space  /* character spacing */
#endif
);

extern void gset_char_up_vec(
#ifdef  NeedFuncProto
    const Gvec *char_up_vec  /* character up vector */
#endif
);

extern void gset_char_expan(
#ifdef  NeedFuncProto
    Gdouble char_expan  /* character expansion factor */
#endif
);

extern void gset_colr_rep(
#ifdef  NeedFuncProto
    Gint            ws_id,     /* workstation identifier */
    Gint            colr_ind,  /* colour index           */
    const Gcolr_rep *colr_rep  /* colour representation  */
#endif
);

extern void gset_fill_colr_ind(
#ifdef  NeedFuncProto
    Gint fill_colr_ind  /* fill area colour index  */
#endif
);

extern void gset_pat_size(
#ifdef  NeedFuncProto
    Gdouble x_size, /* x size */
    Gdouble y_size  /* y size */
#endif
);

extern void gset_pat_ref_point(
#ifdef  NeedFuncProto
    const Gpoint *pat_ref_point  /* pattern reference point */
#endif
);

extern void gset_fill_int_style(
#ifdef  NeedFuncProto
    Gfill_int_style fill_int_style  /* fill area style index */
#endif
);

extern void gset_fill_style_ind(
#ifdef  NeedFuncProto
    Gint fill_style_ind  /* fill area style index */
#endif
);

extern void gset_linetype(
#ifdef  NeedFuncProto
    Gint linetype  /* linetype */
#endif
);

extern void gset_linewidth(
#ifdef  NeedFuncProto
    Gdouble linewidth  /* linewidth scale factor  */
#endif
);

extern void gset_marker_type(
#ifdef  NeedFuncProto
    Gint marker_type  /* marker type  */
#endif
);

extern void gset_marker_size(
#ifdef  NeedFuncProto
    Gdouble marker_size  /* marker size scale factor */
#endif
);

extern void gset_marker_colr_ind(
#ifdef  NeedFuncProto
    Gint marker_colr_ind  /* polymarker colour index */
#endif
);

extern void gset_text_ind(
#ifdef  NeedFuncProto
    Gint text_ind  /*  text index  */
#endif
);

extern void gset_text_align(
#ifdef  NeedFuncProto
    const Gtext_align *text_align  /* text alignment */
#endif
);

extern void gset_fill_ind(
#ifdef  NeedFuncProto
    Gint fill_ind  /*  fill area index  */
#endif
);

extern void gset_text_colr_ind(
#ifdef  NeedFuncProto
    Gint text_colr_ind  /* text colour index */
#endif
);

extern void gset_text_font_prec(
#ifdef  NeedFuncProto
    const Gtext_font_prec *text_font_prec  /* text font and precision */
#endif
);

extern void gset_text_path(
#ifdef  NeedFuncProto
    Gtext_path text_path  /* text path */
#endif
);


/*
 *    A.3.4 Transformation Functions
 */

extern void gset_win(
#ifdef  NeedFuncProto
    Gint            tran_num,      /* transformation number  */
    const Glimit    *win_limits    /* window limits          */
#endif
);

extern void gset_vp(
#ifdef  NeedFuncProto
    Gint            tran_num,     /* transformation number */
    const Glimit    *vp_limits    /* viewport limits       */
#endif
);

extern void gsel_norm_tran(
#ifdef  NeedFuncProto
    Gint tran_num  /* transformation number */
#endif
);

extern void gset_clip_ind(
#ifdef  NeedFuncProto
    Gclip_ind clip_ind  /* clipping indicator */
#endif
);

extern void gset_ws_win(
#ifdef  NeedFuncProto
    Gint   ws_id,           /* workstation identifier    */
    Glimit *ws_win_limits   /* workstation window limits */
#endif
);

extern void gset_ws_vp(
#ifdef  NeedFuncProto
    Gint   ws_id,          /* workstation identifier      */
    Glimit *ws_vp_limits   /* workstation viewport limits */
#endif
);

/*
 *    A.3.5 Segment Functions
 */

extern void gcreate_seg(
#ifdef  NeedFuncProto
    Gint seg_name  /* segment name */
#endif
);

extern void gclose_seg(
#ifdef  NeedFuncProto
    void
#endif
);

extern void gdel_seg(
#ifdef  NeedFuncProto
    Gint seg_name  /* segment name */
#endif
);

extern void gcopy_seg_ws(
#ifdef  NeedFuncProto
    Gint ws_id,    /* workstation identifier */
    Gint seg_name  /* segment name           */
#endif
);

extern void gset_seg_tran(
#ifdef  NeedFuncProto
    Gint seg_name,                 /* segment name          */
    const Gtran_matrix tran_matrix /* transformation matrix */
#endif
);

/*
 *    A.3.6  Input Functions
 */

extern void ginit_string(
#ifdef  NeedFuncProto
    Gint               ws_id,        /* workstation identifier  */
    Gint               string_num,   /* string device number    */
    const char         *init_string, /* initial string          */
    Gint               pet,          /* prompt and echo type    */
    const Glimit       *echo_area,   /* echo area               */
    Gint               in_buf_size,  /* input buffer size       */
    Gint               init_cur_pos, /* initial cursor position */
    const Gstring_data *string_data  /* string data record      */
#endif
);

extern void gset_string_mode(
#ifdef  NeedFuncProto
    Gint         ws_id,      /* workstation identifier */
    Gint         string_num, /* string device number   */
    Gop_mode     op_mode,    /* operating mode         */
    Gecho_switch echo_switch /* echo switch            */
#endif
);

extern void greq_string(
#ifdef  NeedFuncProto
    Gint       ws_id,      /* workstation identifier */
    Gint       string_num, /* string device number   */
    Gin_status *in_status, /* OUT [input] status     */
    char       *string     /* OUT requested string   */
#endif
);

/*
 *    A.3.7 GKSM Functions
 */

extern void gwrite_item(
#ifdef  NeedFuncProto
    Gint ws_id,                   /* workstation identifier  */
    Gint item_type,               /* item type               */
    Gint item_data_length,        /* item data record length */
    const Gitem_data *item_data   /* item data record        */
#endif
);

extern void gget_item_type(
#ifdef  NeedFuncProto
    Gint ws_id,              /* workstation identifier      */
    Gint *item_type,         /* OUT item type               */
    Gint *item_data_length   /* OUT item data record length */
#endif
);

extern void gread_item(
#ifdef  NeedFuncProto
    Gint ws_id,                 /* workstation identifier  */
    Gint max_item_data_length,  /* max item data record length */
    Gitem_data *item_data       /* OUT item data record        */
#endif
);

extern void ginterpret_item(
#ifdef  NeedFuncProto
    Gint type,                    /* item type               */
    Gint item_data_length,        /* item data record length */
    const Gitem_data *item_data   /* item data record        */
#endif
);

/*
 *    A.3.8 Inquire Functions
 */

extern void ginq_asfs(
#ifdef  NeedFuncProto
    Gint  *err_ind,   /* OUT error indicator             */
    Gasfs *list_asf   /* OUT current aspect source flags */
#endif
);

extern void ginq_char_ht(
#ifdef  NeedFuncProto
    Gint    *err_ind,  /* OUT error indicator          */
    Gdouble *char_ht   /* OUT current character height */
#endif
);

extern void ginq_char_space(
#ifdef  NeedFuncProto
    Gint    *err_ind,     /* OUT error indicator           */
    Gdouble *char_space   /* OUT current character spacing */
#endif
);

extern void ginq_char_up_vec(
#ifdef  NeedFuncProto
    Gint *err_ind,      /* OUT error indicator             */
    Gvec *char_up_vec   /* OUT current character up vector */
#endif
);

extern void ginq_char_width(
#ifdef  NeedFuncProto
    Gint    *err_ind,    /* OUT error indicator          */
    Gdouble *char_width  /* OUT current character width  */
#endif
);

extern void ginq_char_base_vec(
#ifdef  NeedFuncProto
    Gint *err_ind,       /* OUT error indicator                */
    Gvec *char_base_vec  /* OUT current character base vector  */
#endif
);

extern void ginq_char_expan(
#ifdef  NeedFuncProto
    Gint    *err_ind,    /* OUT error indicator                    */
    Gdouble *char_expan  /* OUT current character expansion factor */
#endif
);

extern void ginq_name_open_seg(
#ifdef  NeedFuncProto
    Gint *err_ind,      /* OUT error indicator      */
    Gint *name_open_seg /* OUT name of open segment */
#endif
);

extern void ginq_set_seg_names(
#ifdef  NeedFuncProto
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *seg_names,          /* OUT list of segment names  */
    Gint      *length_list         /* OUT length of list in GKS  */
#endif
);

extern void ginq_clip(
#ifdef  NeedFuncProto
    Gint  *err_ind,        /* OUT error indicator                         */
    Gclip *clip_ind_rect   /* OUT current clippig indicator and rectangle */
#endif
);

extern void ginq_cur_norm_tran_num(
#ifdef  NeedFuncProto
    Gint *err_ind,        /* OUT error indicator        */
    Gint *norm_tran_num   /* OUT current normalization
                             transformation number      */
#endif
);

extern void ginq_list_norm_tran_nums(
#ifdef  NeedFuncProto
    Gint num_elems_appl_list,  /* length of application list                */
    Gint start_pos,            /* starting position                         */
    Gint *err_ind,             /* OUT error indicator                       */
    Gint_list *norm_tran_num,  /* OUT list of normalization transformation 
                                  numbers                                   */
    Gint *length_list          /* OUT length of list in GKS                 */
#endif
);

extern void ginq_norm_tran(
#ifdef  NeedFuncProto
    Gint         num,  /* normalization transformation number */   
    Gint    *err_ind,  /* OUT error indicator                 */
    Gtran *norm_tran   /* OUT normalization tranformation     */
#endif
);

extern void ginq_ws_st(
#ifdef  NeedFuncProto
    Gint   ws_id,    /* workstation identifier */
    Gint   *err_ind, /* OUT error indicator    */
    Gws_st *ws_st    /* OUT workstation state  */
#endif
);

extern void ginq_ws_defer_upd_sts(
#ifdef  NeedFuncProto
    Gint               ws_id,       /* workstation identifier         */
    Gint               *err_ind,    /* OUT error indicator            */
    Gdefer_mode        *defer_mode, /* OUT deferral mode              */
    Girg_mode          *irg_mode,   /* OUT implicit regeneration mode */
    Gdisp_space_empty  *disp_empty, /* OUT display space empty        */
    Gnew_frame_nec_upd *new_frame   /* OUT new frame action necessary 
                                       at update                      */
#endif
);

extern void ginq_fill_colr_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,
    Gint *fill_colr_ind
#endif
);

extern void ginq_fill_int_style(
#ifdef  NeedFuncProto
    Gint            *err_ind,         /* OUT current error indicator       */
    Gfill_int_style *fill_int_style   /* OUT current fill area style index */
#endif
);

extern void ginq_fill_style_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,         /* OUT current error indicator       */
    Gint *fill_style_ind   /* OUT current fill area style index */
#endif
);

extern void ginq_linetype(
#ifdef  NeedFuncProto
    Gint *err_ind,   /* OUT error indicator  */
    Gint *linetype   /* OUT current linetype */
#endif
);

extern void ginq_linewidth(
#ifdef  NeedFuncProto
    Gint    *err_ind,    /* OUT error indicator                 */
    Gdouble *linewidth   /* OUT current linewidth scale factor  */
#endif
);

extern void ginq_marker_type(
#ifdef  NeedFuncProto
    Gint *err_ind,      /* OUT error indicator      */
    Gint *marker_type   /* OUT current marker type  */
#endif
);

extern void ginq_marker_size(
#ifdef  NeedFuncProto
    Gint    *err_ind,      /* OUT error indicator                  */
    Gdouble *marker_size   /* OUT current marker size scale factor */
#endif
);

extern void ginq_max_norm_tran_num(
#ifdef  NeedFuncProto
    Gint *err_ind,           /* OUT error indicator                       */
    Gint *max_norm_tran_num  /* OUT maximum normalization transformation  */ 
                             /* number                                    */
#endif
);

extern void ginq_set_open_wss(
#ifdef  NeedFuncProto
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *open_ws,            /* OUT list of open ws ids    */
    Gint      *length_list         /* OUT length of list in GKS  */
#endif
);

extern void ginq_set_active_wss(
#ifdef  NeedFuncProto
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *active_ws,          /* OUT list of active ws ids  */
    Gint      *length_list         /* OUT length of list in GKS  */
#endif
);

extern void ginq_cur_prim_attrs(
#ifdef  NeedFuncProto
    Gint        *err_ind,  /* OUT error indicator                       */
    Gprim_attrs *prim_attr /* OUT current primitive attribute structure */
#endif
);

extern void ginq_line_ind(
#ifdef  NeedFuncProto
    Gint *err_ind, /*  OUT error indicator         */
    Gint *line_ind /*  OUT current polyline index  */
#endif
);

extern void ginq_marker_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,   /*  OUT error indicator           */
    Gint *marker_ind /*  OUT current polymarker index  */
#endif
);

extern void ginq_text_extent(
#ifdef  NeedFuncProto
    Gint         ws_id,    /* workstation identifier      */
    const Gpoint *pos,     /* text position               */
    const char   *str,     /* text string                 */
    Gint         *err_ind, /* OUT error indicator         */
    Gtext_extent *extent   /* OUT concatentation point and*/
                           /* text extent parallelogram   */
#endif
);

extern void ginq_list_colr_inds(
#ifdef  NeedFuncProto
    Gint      ws_id,              /* workstation identifier             */
    Gint      num_elems_appl_list,/* length of application list         */
    Gint      start_pos,          /* starting position                  */
    Gint      *err_ind,           /* OUT error indicator                */
    Gint_list *def_colr_inds,     /* OUT list of defined colour indices */
    Gint      *length_list        /* OUT length of list in GKS          */
#endif
);

extern void ginq_colr_rep(
#ifdef  NeedFuncProto
    Gint       ws_id,     /* workstation identifier     */
    Gint       colr_ind,  /* colour index               */
    Ginq_type  type,      /* type of returned values    */
    Gint       *err_ind,  /* OUT error indicator        */
    Gcolr_rep  *colr_rep  /* OUT colour representation  */
#endif
);

extern void ginq_ws_tran(
#ifdef  NeedFuncProto
    Gint    ws_id,           /* workstation identifier                      */
    Gint    *err_ind,        /* OUT error indicator                         */
    Gupd_st *ws_tran_upd_st, /* OUT workstation transformation update state */
    Glimit  *req_ws_win,     /* OUT requested workstation window            */
    Glimit  *cur_ws_win,     /* OUT current workstation window              */
    Glimit  *req_ws_vp,      /* OUT requested workstation viewport          */
    Glimit  *cur_ws_vp       /* OUT current workstation viewport            */
#endif
);

extern void ginq_ws_cat(
#ifdef  NeedFuncProto
    Gint    ws_type,  /* workstation type         */
    Gint    *err_ind, /* OUT error indicator      */
    Gws_cat *cat      /* OUT workstation category */
#endif
);

extern void ginq_ws_class(
#ifdef  NeedFuncProto
    Gint      ws_type,  /* workstation type      */
    Gint      *err_ind, /* OUT error indicator   */
    Gws_class *wsclass  /* OUT workstation class */
#endif
);

extern void ginq_disp_space_size(
#ifdef  NeedFuncProto
    Gint             ws_type,    /* workstation type         */
    Gint             *err_ind,   /* OUT error indicator      */
    Gdisp_space_size *disp_size  /* OUT display [space] size */
#endif
);

extern void ginq_line_facs(
#ifdef  NeedFuncProto
    Gint       ws_type,             /* workstation type                   */
    Gint       num_elems_appl_list, /* length of application list         */
    Gint       start_pos,           /* starting position                  */
    Gint       *err_ind,            /* OUT error indicator                */
    Gline_facs *line_facs,          /* OUT polyline facilities            */
    Gint       *length_list         /* OUT length of linetype list in GKS */
#endif
);

extern void ginq_pred_line_rep(
#ifdef  NEEDFUNCPROTO
    Gint         ws_type,           /* workstation type             */
    Gint         ind,               /* predefined index             */
    Gint         *err_ind,          /* OUT error indicator          */
    Gline_bundle *line_rep  /* OUT predefined polyline rep. */
#endif
);

extern void ginq_marker_facs(
#ifdef  NeedFuncProto
    Gint         ws_type,             /* workstation type                      */
    Gint         num_elems_appl_list, /* length of application list            */
    Gint         start_pos,           /* starting position                     */
    Gint         *err_ind,            /* OUT error indicator                   */
    Gmarker_facs *marker_facs,        /* OUT polymarker facilities             */
    Gint          *length_list        /* OUT length of marker type list in GKS */
#endif
);

extern void ginq_pred_marker_rep(
#ifdef  NeedFuncProto
    Gint           ws_type,     /* workstation type               */
    Gint           ind,         /* predefined index               */
    Gint           *err_ind,    /* OUT error indicator            */
    Gmarker_bundle *marker_rep  /* OUT predefined polymarker rep. */
#endif
);

extern void ginq_text_facs(
#ifdef  NeedFuncProto
    Gint       ws_type,             /* workstation type                            */
    Gint       num_elems_appl_list, /* length of application list                  */
    Gint       start_pos,           /* starting position                           */
    Gint       *err_ind,            /* OUT error indicator                         */
    Gtext_facs *text_facs,          /* OUT text facilities                         */
    Gint       *length_list         /* OUT length of text font and precision list */
                                    /* in GKS                                      */
#endif
);

extern void ginq_pred_text_rep(
#ifdef  NeedFuncProto
    Gint         ws_type,   /* workstation type         */
    Gint         ind,       /* predefined index         */
    Gint         *err_ind,  /* OUT error indicator      */
    Gtext_bundle *text_rep  /* OUT predefined text rep. */
#endif
);

extern void ginq_fill_facs(
#ifdef  NeedFuncProto
    Gint       ws_type,           /* workstation type                */
    Gint       hatch_length,      /* length of hatch style list      */
    Gint       hatch_start_pos,   /* hatch style starting position   */
    Gint       *err_ind,          /* OUT error indicator             */
    Gfill_facs *fill_facs,        /* OUT fill area facilities        */
    Gint       *act_hatch_length  /* OUT length of hatch list in GKS */
#endif
);

extern void ginq_pred_fill_rep(
#ifdef  NeedFuncProto
    Gint         ws_type,   /* workstation type              */
    Gint         ind,       /* predefined index              */
    Gint         *err_ind,  /* OUT error indicator           */
    Gfill_bundle *fill_rep  /* OUT predefined fill area rep. */
#endif
);

extern void ginq_pat_facs(
#ifdef  NeedFuncProto
    Gint ws_type,       /* workstation type                    */
    Gint *err_ind,      /* OUT error indicator                 */
    Gint *num_pred_inds /* OUT num. of predef. pattern indices */
#endif
);

extern void ginq_pred_pat_rep(
#ifdef  NeedFuncProto
    Gint     ws_type,    /* workstation type            */
    Gint     ind,        /* predefined index            */
    Gstore   *store,     /* size of buffer              */
    Gint     *err_ind,   /* OUT error indicator         */
    Gpat_rep **pat_rep   /* OUT predefined pattern rep. */
#endif
);

extern void ginq_colr_facs(
#ifdef  NeedFuncProto
    Gint       ws_type,    /* workstation type      */
    Gint       *err_ind,   /* OUT error indicator   */
    Gcolr_facs *colr_facs  /* OUT colour facilities */
#endif
);

extern void ginq_pred_colr_rep(
#ifdef  NeedFuncProto
    Gint      ws_type,  /* workstation identifier     */
    Gint      ind,      /* predefined index           */
    Gint      *err_ind, /* OUT error indicator        */
    Gcolr_rep *colr_rep /* OUT predefined colour rep. */
#endif
);

extern void ginq_list_avail_gdps(
#ifdef  NeedFuncProto
    Gint      ws_type,             /* workstation identifier     */
    Gint      num_elems_appl_list, /* length of application list */
    Gint      start_pos,           /* starting position          */
    Gint      *err_ind,            /* OUT error indicator        */
    Gint_list *gdp,                /* OUT list of GDPs           */
    Gint      *length_list         /* OUT length of list in GKS  */
#endif
);

extern void ginq_gdp(
#ifdef  NeedFuncProto
    Gint   ws_type,   /* workstation type            */
    Gint   gdp,       /* GDP function number         */
    Gint   *err_ind,  /* OUT error indicator         */
    Gint   *num_attr, /* OUT num. of attributes used */
    Gattrs attr[4]    /* OUT list of attributes used */
#endif
);

extern void ginq_max_ws_st_tables(
#ifdef  NeedFuncProto
    Gint              ws_type,  /* workstation type                  */
    Gint              *err_ind, /* OUT error indicator               */
    Gmax_ws_st_tables *lengths  /* OUT lengths of workstation tables */
#endif
);

extern void ginq_pixel_array_dims(
#ifdef  NeedFuncProto
    Gint      ws_id,      /* workstation identifier     */
    Grect     *rect,      /* rectangle                  */
    Gint      *err_ind,   /* OUT error indicator        */
    Gint_size *dims       /* OUT pixel array dimensions */
#endif
);

extern void ginq_pixel_array(
#ifdef  NeedFuncProto
    Gint        ws_id,               /* workstation identifier         */
    Gpoint      *pixel_loc,          /* pixel location                 */
    Gint_size   *dims,               /* pixel array dimensions         */
    Gint        num_elems_appl_list, /* length of application list     */
    Gint        start_pos,           /* starting position              */
    Gint        *err_ind,            /* OUT error indicator            */
    Gpres_inval *pres_inval,         /* OUT presence of invalid values */
    Gint_list   *pixel_array,        /* OUT colour index array         */
    Gint        *length_list         /* OUT length of list in GKS      */
#endif
);

extern void ginq_pixel(
#ifdef  NeedFuncProto
    Gint   ws_id,             /* workstation identifier */
    const  Gpoint *pixel_loc, /* pixel location         */
    Gint   *err_ind,          /* OUT error indicator    */
    Gint   *colr_ind          /* OUT colour index       */
#endif
);

extern void ginq_text_ind(
#ifdef  NeedFuncProto
    Gint *err_ind, /*  OUT error indicator     */
    Gint *text_ind /*  OUT current text index  */
#endif
);

extern void ginq_ws_conn_type(
#ifdef  NeedFuncProto
    Gint ws_id,         /* workstation identifier                  */
    Gint string_length, /* string length for connection identifier */
    Gint *err_ind,      /* OUT error indicator                     */
    char *conn_id,      /* OUT connection identifier               */
    Gint *ws_type       /* OUT workstation type                    */
#endif
);

extern void ginq_line_colr_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,       /* OUT error indicator               */
    Gint *line_colr_ind  /* OUT current polyline colour index */
#endif
);

extern void ginq_marker_colr_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,          /* OUT error indicator                 */
    Gint *marker_colr_ind   /* OUT current polymarker colour index */
#endif
);

extern void ginq_op_st(
#ifdef  NeedFuncProto
    Gop_st *op_st  /* OUT operating state value */
#endif
);

extern void ginq_level_gks(
#ifdef  NeedFuncProto
    Gint   *err_ind,  /* OUT error indicator */
    Glevel *level     /* OUT level of GKS    */
#endif
);

extern void ginq_list_avail_ws_types(
#ifdef  NeedFuncProto
    Gint      num_elems_appl_list, /* length of application list     */
    Gint      start_pos,           /* starting position              */
    Gint      *err_ind,            /* OUT error indicator            */
    Gint_list *ws_type,            /* OUT list of available ws types */
    Gint      *length_list         /* OUT length of list in GKS      */
#endif
);

extern void ginq_ws_max_nums(
#ifdef  NeedFuncProto
    Gint         *err_ind,    /* OUT error indicator             */
    Gws_max_nums *ws_max_num  /* OUT workstation maximum numbers */
#endif
);

extern void ginq_text_align(
#ifdef  NeedFuncProto
    Gint        *err_ind,     /* OUT error indicator          */
    Gtext_align *text_align   /* OUT current text alignment   */
#endif
);

extern void ginq_fill_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,  /*  OUT error indicator          */    
    Gint *fill_ind  /*  OUT current fill area index  */
#endif
);

extern void ginq_pat_size(
#ifdef  NeedFuncProto
    Gint    *err_ind, /* OUT error indicator */
    Gdouble *x_size,  /* OUT x size          */
    Gdouble *y_size   /* OUT y size          */
#endif
);

extern void ginq_pat_width_vec(
#ifdef  NeedFuncProto
    Gint *err_ind,        /* OUT error indicator              */
    Gvec *pat_width_vec   /* OUT current pattern width vector */
#endif
);

extern void ginq_pat_ht_vec(
#ifdef  NeedFuncProto
    Gint *err_ind,     /* OUT error indicator               */
    Gvec *pat_ht_vec   /* OUT current pattern height vector */
#endif
);

extern void ginq_pat_ref_point(
#ifdef  NeedFuncProto
    Gint   *err_ind,      /* OUT error indicator                 */
    Gpoint *pat_ref_point /* OUT current pattern reference point */
#endif
);

extern void ginq_text_colr_ind(
#ifdef  NeedFuncProto
    Gint *err_ind,        /* OUT error indicator     */
    Gint *text_colr_ind   /* OUT text colour index   */
#endif
);

extern void ginq_text_font_prec(
#ifdef  NeedFuncProto
    Gint            *err_ind,         /* OUT error indicator                 */
    Gtext_font_prec *text_font_prec   /* OUT current text font and precision */
#endif
);

extern void ginq_text_path(
#ifdef  NeedFuncProto
    Gint       *err_ind,    /* OUT error indicator   */
    Gtext_path *text_path   /* OUT current text path */
#endif
);

/*
 *    A.3.9 Utility Functions
 */

extern void geval_tran_matrix(
#ifdef  NeedFuncProto
    const Gpoint   *point,        /* fixed point               */
    const Gvec     *shift,        /* shift vector              */
    Gdouble        angle,         /* rotation angle            */
    const Gvec     *scale,        /* scale factors             */
    Gcoord_switch  coord_switch,  /* corrdinate switch         */
    Gtran_matrix   tran_matrix    /* OUT transformation matrix */
#endif
);

extern void gaccum_tran_matrix(
#ifdef  NeedFuncProto
    const Gtran_matrix matrix,        /* transformation matrix     */
    const Gpoint       *point,        /* fixed point               */
    const Gvec         *shift,        /* shift vector              */
    Gdouble            angle,         /* rotation angle            */
    const Gvec         *scale,        /* scale factors             */
    Gcoord_switch      coord_switch,  /* coordinate switch         */
    Gtran_matrix       tran_matrix    /* OUT transformation matrix */
#endif
);

extern void gcreate_store(
#ifdef  NeedFuncProto
    Gint   *err_ind,   /* OUT error indicator         */
    Gstore *store      /* OUT pointer to storage data */
#endif
);

extern void gdel_store(
#ifdef  NeedFuncProto
    Gstore store      /* storage to be deleted */
#endif
);

/*
 *    A.3.10 Error Handling Functions
 */

extern void gemergency_close_gks(
#ifdef  NeedFuncProto
    void
#endif
);

extern void gerr_hand(
#ifdef  NeedFuncProto
    Gint       err_num,  /* error number                               */
    Gint       func_num, /* number of function that detected the error */
    const char *err_f    /* name of error file                         */
#endif
);

extern void gerr_log(
#ifdef  NeedFuncProto
    Gint       err_num,  /* error number                               */
    Gint       func_num, /* number of function that detected the error */
    const char *err_f    /* name of error file                         */
#endif
);

NCARG_PROTO_END

#endif  /* _gks_h */

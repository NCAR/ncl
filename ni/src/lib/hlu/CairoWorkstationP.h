/*
 *  $Id: CairoWorkstationP.h,v 1.1 2009-12-10 18:07:01 brownrig Exp $
 */

# ifndef    _NCairoWorkstationP_h
# define    _NCairoWorkstationP_h

# include   <ncarg/hlu/WorkstationP.h>
# include   <ncarg/hlu/CairoWorkstation.h>

/*
 * CAIRO workstation type identifiers start at 40.
 *
 */

# define    CPS         (40)
# define    CPNG        (41)
# define    CPDF        (42)

typedef struct _NhlCairoWorkstationLayerPart {
    NhlCairoFormat     format;             /* CPS, CPNG, CPDF, etc. */
    NhlString          filename;
    NhlWorkOrientation orientation;        /* PORTRAIT or LANDSCAPE */
    int resolution;
    int lower_x;
    int lower_y;
    int upper_x;
    int upper_y;
    /* Private internal fields */
    NhlBoolean  dev_bounds_updated;

} NhlCairoWorkstationLayerPart;

typedef struct  _NhlCairoWorkstationLayerRec {
    NhlBaseLayerPart    base;
    NhlWorkstationLayerPart     work;
    NhlCairoWorkstationLayerPart  cairo;
} NhlCairoWorkstationLayerRec;

typedef struct  _NhlCairoWorkstationClassPart {
    int place_holder;
} NhlCairoWorkstationClassPart;

typedef struct  _NhlCairoWorkstationClassRec {
    NhlBaseClassPart    base_class;
    NhlWorkstationClassPart     work_class;
    NhlCairoWorkstationClassPart  cairo_class;
} NhlCairoWorkstationClassRec;

typedef struct _NhlCairoWorkstationLayerRec   *NhlCairoWorkstationLayer;
typedef struct _NhlCairoWorkstationClassRec   *NhlCairoWorkstationClass;


extern NhlCairoWorkstationClassRec    NhlcairoWorkstationClassRec;

# endif /* _NCairoWorkstationP_h */

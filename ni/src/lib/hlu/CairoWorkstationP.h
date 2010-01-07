/*
 *  $Id: CairoWorkstationP.h,v 1.3 2010-01-07 23:06:01 brownrig Exp $
 */

# ifndef    _NCairoWorkstationP_h
# define    _NCairoWorkstationP_h

# include   <ncarg/hlu/WorkstationP.h>
# include   <ncarg/hlu/CairoWorkstation.h>
# include   <ncarg/gksP.h>

/*
 * CAIRO workstation type identifiers start at 40.
 *
 */

# define    CPS         (40)
# define    CPNG        (41)
# define    CPDF        (42)

/* We are creating distinct workstation objects for PS/PDF versus image-based cairo output formats.
 * However, we'll use common methods and structs. In the struct below, some fields are applicable
 * to only one or the other workstation type.
 */
typedef struct _NhlCairoWorkstationLayerPart {
    NhlCairoFormat     format;             /* CPS, CPNG, CPDF, etc. */
    NhlString          filename;
    int lower_x;
    int lower_y;
    int upper_x;
    int upper_y;

    /* fields for PS/PDF workstations */
    NhlWorkOrientation orientation;               /* PORTRAIT or LANDSCAPE */
    int dpi;                                      /* dots/inch */

    /* fields for image-based workstations */
    _NGCPixConfig      pixconfig;

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


extern NhlCairoWorkstationClassRec    NhlcairoPSPDFWorkstationClassRec;
extern NhlCairoWorkstationClassRec    NhlcairoImageWorkstationClassRec;

# endif /* _NCairoWorkstationP_h */

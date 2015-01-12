/*
 *  $Id$
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
# define    CTIFF       (43)
# define    CX11        (44)
# define    CEPS        (45)
# define    CQT         (46)
# define    CSVG        (47)

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
    NhlBoolean cairo_fill_hack;

    /* fields for PS/PDF workstations */
    NhlString          paper_size;      /* standard paper name, e.g., "legal", "A2", etc. */
    NhlWorkOrientation orientation;     /* PORTRAIT or LANDSCAPE */
    float              page_width;      /* inches */
    float              page_height;

    /* fields for image-based workstations */
    _NGCPixConfig      pixconfig;

    /* fields for window-based workstations */
    NhlBoolean         window_id_set;
    int                window_id;
    NhlBoolean         pause_set;
    NhlBoolean         pause;
    _NGCXWinConfig     xwinconfig;

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


extern NhlCairoWorkstationClassRec    NhlcairoDocumentWorkstationClassRec;
extern NhlCairoWorkstationClassRec    NhlcairoImageWorkstationClassRec;
extern NhlCairoWorkstationClassRec    NhlcairoWindowWorkstationClassRec;
#ifdef BuildQtEnabled
extern NhlCairoWorkstationClassRec    NhlcairoQtWorkstationClassRec;
#endif

# endif /* _NCairoWorkstationP_h */

/*
 *  $Id: PDFWorkstationP.h,v 1.1 2003-02-27 18:14:35 grubin Exp $
 */

# ifndef    _NPDFWorkstationP_h
# define    _NPDFWorkstationP_h

# include   <ncarg/hlu/WorkstationP.h>
# include   <ncarg/hlu/PDFWorkstation.h>

# define    MAX_OPEN_PDF    (1)

/*
 * PDF workstation type identifiers start at 11.
 */
# define    PDFBASE      (11)

typedef struct _NhlPDFWorkstationLayerPart {
    /* User setable resource fields */

    NhlPDFFormat    format;
    NhlVisualType   visual;
    NhlWorkOrientation  orientation;
    NhlColorModel   color_model;

    NhlString   filename;

    int resolution;

    int lower_x;
    int lower_y;
    int upper_x;
    int upper_y;

    NhlBoolean  full_background;
    NhlBoolean  suppress_background;
    NhlBoolean  suppress_bbinfo;

    /* Private internal fields */
    NhlBoolean  dev_bounds_updated;
    
} NhlPDFWorkstationLayerPart;

typedef struct  _NhlPDFWorkstationLayerRec {
    NhlBaseLayerPart    base;
    NhlWorkstationLayerPart     work;
    NhlPDFWorkstationLayerPart  pdf;
} NhlPDFWorkstationLayerRec;

typedef struct  _NhlPDFWorkstationClassPart {
    int foo;
} NhlPDFWorkstationClassPart;

typedef struct  _NhlPDFWorkstationClassRec {
    NhlBaseClassPart    base_class;
    NhlWorkstationClassPart     work_class;
    NhlPDFWorkstationClassPart  pdf_class;
} NhlPDFWorkstationClassRec;

typedef struct _NhlPDFWorkstationLayerRec   *NhlPDFWorkstationLayer;
typedef struct _NhlPDFWorkstationClassRec   *NhlPDFWorkstationClass;

extern NhlPDFWorkstationClassRec    NhlpdfWorkstationClassRec;

# endif /* _NPDFWorkstationP_h */

/*
 *  $Id: PDFWorkstationP.h,v 1.2 2003-03-06 23:22:50 grubin Exp $
 */

# ifndef    _NPDFWorkstationP_h
# define    _NPDFWorkstationP_h

# include   <ncarg/hlu/WorkstationP.h>
# include   <ncarg/hlu/PDFWorkstation.h>

# define    MAX_OPEN_PDF    (1)

/*
 * PDF workstation type identifiers start at 11.
 * Types are: PORTRAIT or LANDSCAPE.
 */
# define    PDFBASE         (11)
# define    PDFPORTRAIT     (11)
# define    PDFLANDSCAPE    (12)

typedef struct _NhlPDFWorkstationLayerPart {
    /* User setable resource fields */

    NhlPDFFormat    format;             /* PDF */
    NhlVisualType   visual;             /* always COLOR */
    NhlWorkOrientation  orientation;    /* PORTRAIT or LANDSCAPE */
    NhlColorModel   color_model;        /* RGB or user defined */

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

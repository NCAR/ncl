/*
 *      $Id: PDFWorkstation.h,v 1.1 2003-02-27 18:14:35 grubin Exp $
 */

# ifndef    _NPDFWorkstation_h
# define    _NPDFWorkstation_h

# include   <ncarg/hlu/Workstation.h>

# define    NhlNwkPDFFormat     "wkPDFFormat"
# define    NhlCwkPDFFormat     "WkPDFFormat"

# define    NhlNwkPDFFileName   "wkPDFFileName"
# define    NhlCwkPDFFileName   "WkPDFFileName"

# define    NhlNwkPDFResolution     "wkPDFResolution"
# define    NhlCwkPDFResolution     "WkPDFResolution"

/*
 * See: Workstation.h for common, shared resources for visual type,
 * orientation, background, device upper/lower coordinates, and 
 * color model.
 */

/*
 * New Types.
 */

# define    NhlTPDFFormat   "PDFFormat"
typedef enum _NhlPDFFormat {
    NhlPDF = 0
} NhlPDFFormat;

extern NhlClass NhlpdfWorkstationClass;

# endif /* _NPDFWorkstation_h */

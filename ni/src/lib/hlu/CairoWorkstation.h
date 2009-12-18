/*
 *      $Id: CairoWorkstation.h,v 1.2 2009-12-18 23:15:50 brownrig Exp $
 */

# ifndef    _NCarioWorkstation_h
# define    _NCarioWorkstation_h

# include   <ncarg/hlu/Workstation.h>


# define    NhlNwkCairoFormat     "wkCairoFormat"
# define    NhlCwkCairoFormat     "WkCairoFormat"

# define    NhlNwkCairoFileName   "wkCairoFileName"
# define    NhlCwkCairoFileName   "WkCairoFileName"

# define    NhlNwkPDFResolution   "wkPDFResolution"
# define    NhlCwkPDFResolution   "WkPDFResolution"

/* These next 4 are duplicated in XWorkstation; should probably be refactored into Workstation -- RLB 12/2009 */
# define NhlNwkWidth              "wkWidth"
# define NhlCwkWidth              "WkWidth"
# define NhlNwkHeight             "wkHeight"
# define NhlCwkHeight             "WkHeight"

/*
 * See: Workstation.h for common, shared resources for visual type,
 * orientation, background, device upper/lower coordinates, and
 * color model.
 */

/*
 * New Types.
 */


# define    NhlTCairoFormat   "CairoFormat"
typedef enum _NhlCairoFormat {
    NhlCPS  = 0,
    NhlCPNG = 1,
    NhlCPDF = 2,
} NhlCairoFormat;

extern NhlClass NhlcairoWorkstationClass;


# endif /* _NCairoWorkstation_h */

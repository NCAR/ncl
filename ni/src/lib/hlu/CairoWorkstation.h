/*
 *      $Id: CairoWorkstation.h,v 1.1 2009-12-10 18:07:01 brownrig Exp $
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

/*
 *      $Id: CairoWorkstation.h,v 1.6 2010-03-16 20:31:29 brownrig Exp $
 */

# ifndef    _NCarioWorkstation_h
# define    _NCarioWorkstation_h

# include   <ncarg/hlu/Workstation.h>


# define    NhlNwkFormat     "wkFormat"
# define    NhlCwkFormat     "WkFormat"

# define    NhlNwkFileName   "wkFileName"
# define    NhlCwkFileName   "WkFileName"

# define    NhlNwkPDFResolution   "wkPDFResolution"
# define    NhlCwkPDFResolution   "WkPDFResolution"

# define    _NhlNwkPixConf   "_wkPixConf"
# define    _NhlCwkPixConf   "_WkPixConf"

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
    NhlCTIFF = 3
} NhlCairoFormat;

extern NhlClass NhlcairoPSPDFWorkstationClass;
extern NhlClass NhlcairoImageWorkstationClass;


# endif /* _NCairoWorkstation_h */

/*
 *      $Id$
 */

#ifndef    _NCarioWorkstation_h
#define    _NCarioWorkstation_h

#include   <ncarg/hlu/Workstation.h>


#define    NhlNwkFormat     "wkFormat"
#define    NhlCwkFormat     "WkFormat"

#define    NhlNwkFileName   "wkFileName"
#define    NhlCwkFileName   "WkFileName"

#define    _NhlNwkPixConf   "_wkPixConf"
#define    _NhlCwkPixConf   "_WkPixConf"

/* WindowWorkstation resources */
#define    NhlNwkWindowId   "wkWindowId"
#define    NhlCwkWindowId   "WkWindowId"

#define    NhlNwkPause      "wkPause"
#define    NhlCwkPause      "WkPause"

#define    NhlNpositionX    "wkPositionX"
#define    NhlCpositionX    "WkPositionX"

#define    NhlNpositionY    "wkPositionY"
#define    NhlCpositionY    "WkPositionY"

#define    NhlNwkTitle      "wkWinTitle"
#define    NhlCwkTitle      "WkWinTitle"

#define    NhlNwkIconTitle  "wkIconTitle"
#define    NhlCwkIconTitle  "WkIconTitle"

#define    NhlNwkCairoFillWorkaround "wkCairoFillWorkaround"
#define    NhlCwkCairoFillWorkaround "WKCairoFillWorkaround"
/*
 * See: Workstation.h for common, shared resources for visual type,
 * orientation, background, device upper/lower coordinates, and
 * color model.
 */

/*
 * New Types.
 */


#define    NhlTCairoFormat   "CairoFormat"

typedef enum _NhlCairoFormat {
    NhlCPS   = 0,
    NhlCPNG  = 1,
    NhlCPDF  = 2,
    NhlCTIFF = 3,
    NhlCX11  = 4,
    NhlCEPS  = 5,
    NhlCQT   = 6,
    NhlCSVG  = 7        
} NhlCairoFormat;

extern NhlClass NhlcairoDocumentWorkstationClass;
extern NhlClass NhlcairoImageWorkstationClass;
extern NhlClass NhlcairoWindowWorkstationClass;
#ifdef BuildQtEnabled
extern NhlClass NhlcairoQtWorkstationClass;
#endif

#endif /* _NCairoWorkstation_h */

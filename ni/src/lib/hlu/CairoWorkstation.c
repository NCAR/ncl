/*
 *      $Id$
 */

#include   <stdio.h>
#include   <string.h>
#include   <ncarg/hlu/CairoWorkstationP.h>
#include   <ncarg/hlu/ConvertersP.h>
#include   <ncarg/hlu/pageutil.h>
#include   <ncarg/hlu/color.h>
#include "hlu.h"
#include "CairoWorkstation.h"

#define    Oset(field)     NhlOffset(NhlCairoWorkstationLayerRec, cairo.field)

/* resources for the Document (PS-PDF) Workstation */
static NhlResource resourcesDocumentWS[] = {
    /* Begin-documented-resources */

    {NhlNwkFormat, NhlCwkFormat, NhlTCairoFormat, sizeof (NhlCairoFormat),
        Oset(format), NhlTImmediate, _NhlUSET((NhlPointer) NhlCPS),
        _NhlRES_NOSACCESS, NULL},
    {NhlNwkFileName, NhlCwkFileName, NhlTString,
        sizeof (NhlString), Oset(filename), NhlTImmediate,
        _NhlUSET((NhlPointer) NULL), _NhlRES_NOSACCESS, (NhlFreeFunc) NhlFree},
    {NhlNwkOrientation, NhlCwkOrientation, NhlTWorkOrientation,
        sizeof (NhlWorkOrientation), Oset(orientation), NhlTImmediate,
        _NhlUSET((NhlPointer) NhlPORTRAIT), _NhlRES_DEFAULT, NULL},

    /* these page size and margins are initialized as "-1" here, and are given appropriate
     * values when the workstation is opened, depending upon which resources are actually
     * available at that time.
     */
    {NhlNwkPaperSize, NhlCwkPaperSize, NhlTString,
        sizeof (NhlString), Oset(paper_size), NhlTImmediate,
        _NhlUSET(PAGEUTIL_DEFAULT_PAPERSIZE), _NhlRES_NOSACCESS, NULL},
    {NhlNwkPaperWidthF, NhlCwkPaperWidthF, NhlTFloat,
        sizeof (float), Oset(page_width), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkPaperHeightF, NhlCwkPaperHeightF, NhlTFloat,
        sizeof (float), Oset(page_height), NhlTString,
        _NhlUSET("-1."), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceLowerX, NhlCwkDeviceLowerX, NhlTInteger,
        sizeof (int), Oset(lower_x), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceLowerY, NhlCwkDeviceLowerY, NhlTInteger,
        sizeof (int), Oset(lower_y), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceUpperX, NhlCwkDeviceUpperX, NhlTInteger,
        sizeof (int), Oset(upper_x), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceUpperY, NhlCwkDeviceUpperY, NhlTInteger,
        sizeof (int), Oset(upper_y), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},

    /* End-documented-resources */
        
    /* HACK:  Enables the paint-clip-stroke drawing hack intended to deal with faint gaps sometime seen between 
     * adject filled regions. See Jira ncl-1913 
     */
    {NhlNwkCairoFillWorkaround, NhlCwkCairoFillWorkaround, NhlTBoolean,
        sizeof (NhlBoolean), Oset(cairo_fill_hack), NhlTImmediate,
        _NhlUSET((NhlPointer) False), _NhlRES_DEFAULT, NULL},
};

/* Resources for the Image Workstation */
static NhlResource resourcesImageWS[] = {
    /* Begin-documented-resources */

    {NhlNwkFormat, NhlCwkFormat, NhlTCairoFormat, sizeof (NhlCairoFormat),
        Oset(format), NhlTImmediate, _NhlUSET((NhlPointer) NhlCPNG),
        _NhlRES_NOSACCESS, NULL},
    {NhlNwkFileName, NhlCwkFileName, NhlTString,
        sizeof (NhlString), Oset(filename), NhlTImmediate,
        _NhlUSET((NhlPointer) NULL), _NhlRES_NOSACCESS, (NhlFreeFunc) NhlFree},
    {NhlNwkOrientation, NhlCwkOrientation, NhlTWorkOrientation,
        sizeof (NhlWorkOrientation), Oset(orientation), NhlTImmediate,
        _NhlUSET((NhlPointer) NhlPORTRAIT), _NhlRES_DEFAULT, NULL},

    {NhlNwkDeviceLowerX, NhlCwkDeviceLowerX, NhlTInteger,
        sizeof (int), Oset(lower_x), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceLowerY, NhlCwkDeviceLowerY, NhlTInteger,
        sizeof (int), Oset(lower_y), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceUpperX, NhlCwkDeviceUpperX, NhlTInteger,
        sizeof (int), Oset(upper_x), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},
    {NhlNwkDeviceUpperY, NhlCwkDeviceUpperY, NhlTInteger,
        sizeof (int), Oset(upper_y), NhlTImmediate,
        _NhlUSET((NhlPointer) - 1), _NhlRES_DEFAULT, NULL},

    /* Resources for image-based output formats. We'll use the existing NGC_PIXCONFIG struct
     * for this purpose, but not all of its fields will be utilized.
     */
    {_NhlNwkPixConf, _NhlCwkPixConf, NhlTInteger, sizeof (int), Oset(pixconfig.type),
        NhlTImmediate, _NhlUSET((NhlPointer) NGC_PIXCONFIG),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNwkWidth, NhlCwkWidth, NhlTInteger, sizeof (int),
        Oset(pixconfig.width), NhlTImmediate,
        _NhlUSET((NhlPointer) 1024), _NhlRES_NOSACCESS, NULL},
    {NhlNwkHeight, NhlCwkHeight, NhlTInteger, sizeof (int),
        Oset(pixconfig.height), NhlTImmediate,
        _NhlUSET((NhlPointer) 1024), _NhlRES_NOSACCESS, NULL},

    /* See Jira ncl-1913.  The drawing hack does not apply to ImageWorkstations, but we silently include this resource 
     * here to suppress warnings if a user has it set -- it should quietly do nothing, rather than throw warnings that
     * its not going to do anything!
     */
    {NhlNwkCairoFillWorkaround, NhlCwkCairoFillWorkaround, NhlTBoolean,
        sizeof (NhlBoolean), Oset(cairo_fill_hack), NhlTImmediate,
        _NhlUSET((NhlPointer) False), _NhlRES_DEFAULT, NULL},
};

static NhlResource resourcesWindowWS[] = {
    {NhlNwkFormat, NhlCwkFormat, NhlTCairoFormat, sizeof (NhlCairoFormat),
        Oset(format), NhlTImmediate, _NhlUSET((NhlPointer) NhlCX11),
        _NhlRES_NOSACCESS, NULL},

    {"no.res", "no.res", NhlTBoolean, sizeof (NhlBoolean), Oset(window_id_set),
        NhlTImmediate, _NhlUSET((NhlPointer) True),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNwkWindowId, NhlCwkWindowId, NhlTInteger, sizeof (int), Oset(window_id),
        NhlTProcedure, _NhlUSET((NhlPointer) _NhlResUnset), _NhlRES_NOSACCESS, NULL},
    {"no.res", "no.res", NhlTBoolean, sizeof (NhlBoolean), Oset(pause_set),
        NhlTImmediate, _NhlUSET((NhlPointer) True),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNwkPause, NhlCwkPause, NhlTBoolean, sizeof (NhlBoolean),
        Oset(pause), NhlTProcedure, _NhlUSET((NhlPointer) _NhlResUnset), 0, NULL},

    {"no.res", "no.res", NhlTInteger, sizeof (int), Oset(xwinconfig.type),
        NhlTImmediate, _NhlUSET((NhlPointer) NGC_XWINCONFIG),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNpositionX, NhlCpositionX, NhlTInteger, sizeof (int), Oset(xwinconfig.x),
        NhlTImmediate, _NhlUSET((NhlPointer) 0), _NhlRES_NOSACCESS, NULL},
    {NhlNpositionY, NhlCpositionY, NhlTInteger, sizeof (int), Oset(xwinconfig.y),
        NhlTImmediate, _NhlUSET((NhlPointer) 0), _NhlRES_NOSACCESS, NULL},
    {NhlNwkWidth, NhlCwkWidth, NhlTInteger, sizeof (int),
        Oset(xwinconfig.width),
        NhlTImmediate, _NhlUSET((NhlPointer) 1000), _NhlRES_NOSACCESS, NULL},
    {NhlNwkHeight, NhlCwkHeight, NhlTInteger, sizeof (int),
        Oset(xwinconfig.height), NhlTImmediate,
        _NhlUSET((NhlPointer) 1000), _NhlRES_NOSACCESS, NULL},
    {NhlNwkTitle, NhlCwkTitle, NhlTString, sizeof (NhlString),
        Oset(xwinconfig.title), NhlTImmediate, _NhlUSET((NhlPointer) NULL),
        _NhlRES_NOSACCESS, (NhlFreeFunc) NhlFree},
    {NhlNwkIconTitle, NhlCwkIconTitle, NhlTString, sizeof (NhlString),
        Oset(xwinconfig.icon_title), NhlTImmediate, _NhlUSET((NhlPointer) NULL),
        _NhlRES_NOSACCESS, (NhlFreeFunc) NhlFree},

    /* See Jira ncl-1913.  The drawing hack does not apply to ImageWorkstations, but we silently include this resource 
     * here to suppress warnings if a user has it set -- it should quietly do nothing, rather than throw warnings that
     * its not going to do anything!
     */
    {NhlNwkCairoFillWorkaround, NhlCwkCairoFillWorkaround, NhlTBoolean,
        sizeof (NhlBoolean), Oset(cairo_fill_hack), NhlTImmediate,
        _NhlUSET((NhlPointer) False), _NhlRES_DEFAULT, NULL},
};

#ifdef BuildQtEnabled
static NhlResource resourcesQtWS[] = {
    {NhlNwkFormat, NhlCwkFormat, NhlTCairoFormat, sizeof (NhlCairoFormat),
        Oset(format), NhlTImmediate, _NhlUSET((NhlPointer) NhlCQT),
        _NhlRES_NOSACCESS, NULL},

    {"no.res", "no.res", NhlTBoolean, sizeof (NhlBoolean), Oset(window_id_set),
        NhlTImmediate, _NhlUSET((NhlPointer) True),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNwkWindowId, NhlCwkWindowId, NhlTInteger, sizeof (int), Oset(window_id),
        NhlTProcedure, _NhlUSET((NhlPointer) _NhlResUnset), _NhlRES_NOSACCESS, NULL},
    {"no.res", "no.res", NhlTBoolean, sizeof (NhlBoolean), Oset(pause_set),
        NhlTImmediate, _NhlUSET((NhlPointer) True),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNwkPause, NhlCwkPause, NhlTBoolean, sizeof (NhlBoolean),
        Oset(pause), NhlTProcedure, _NhlUSET((NhlPointer) _NhlResUnset), 0, NULL},

    {"no.res", "no.res", NhlTInteger, sizeof (int), Oset(xwinconfig.type),
        NhlTImmediate, _NhlUSET((NhlPointer) NGC_XWINCONFIG),
        _NhlRES_NOACCESS | _NhlRES_PRIVATE, NULL},
    {NhlNpositionX, NhlCpositionX, NhlTInteger, sizeof (int), Oset(xwinconfig.x),
        NhlTImmediate, _NhlUSET((NhlPointer) 0), _NhlRES_NOSACCESS, NULL},
    {NhlNpositionY, NhlCpositionY, NhlTInteger, sizeof (int), Oset(xwinconfig.y),
        NhlTImmediate, _NhlUSET((NhlPointer) 0), _NhlRES_NOSACCESS, NULL},
    {NhlNwkWidth, NhlCwkWidth, NhlTInteger, sizeof (int),
        Oset(xwinconfig.width),
        NhlTImmediate, _NhlUSET((NhlPointer) 512), _NhlRES_NOSACCESS, NULL},
    {NhlNwkHeight, NhlCwkHeight, NhlTInteger, sizeof (int),
        Oset(xwinconfig.height), NhlTImmediate,
        _NhlUSET((NhlPointer) 512), _NhlRES_NOSACCESS, NULL},
    {NhlNwkTitle, NhlCwkTitle, NhlTString, sizeof (NhlString),
        Oset(xwinconfig.title), NhlTImmediate, _NhlUSET((NhlPointer) NULL),
        _NhlRES_NOSACCESS, (NhlFreeFunc) NhlFree},
    {NhlNwkIconTitle, NhlCwkIconTitle, NhlTString, sizeof (NhlString),
        Oset(xwinconfig.icon_title), NhlTImmediate, _NhlUSET((NhlPointer) NULL),
        _NhlRES_NOSACCESS, (NhlFreeFunc) NhlFree},
    
    /* See Jira ncl-1913.  The drawing hack does not apply to ImageWorkstations, but we silently include this resource 
     * here to suppress warnings if a user has it set -- it should quietly do nothing, rather than throw warnings that
     * its not going to do anything!
     */
    {NhlNwkCairoFillWorkaround, NhlCwkCairoFillWorkaround, NhlTBoolean,
        sizeof (NhlBoolean), Oset(cairo_fill_hack), NhlTImmediate,
        _NhlUSET((NhlPointer) False), _NhlRES_DEFAULT, NULL},
};
#endif

/* forward declarations of helper functions */
static NhlErrorTypes fixupFilename(NhlCairoWorkstationLayer layer, char* filenameSuffix, char* callingFunc);
static NhlErrorTypes checkUlLRBounds(NhlCairoWorkstationLayerPart*, char*);
static void setCairoFillHackValue(NhlLayer layer);
static void setGeoreferenceData(NhlCairoWorkstationLayer wksLayer, NhlLayer mapLayer);

/*
 * CairoWorkstation base_class method declarations
 */

static NhlErrorTypes CairoWorkstationClassInitialize(void);

static NhlErrorTypes CairoWorkstationClassPartInitialize(NhlClass lc);

static NhlErrorTypes CairoWorkstationDestroy(NhlLayer inst);

static NhlErrorTypes CairoWorkstationSetValues(NhlLayer old, NhlLayer reference,
        NhlLayer new, _NhlArgList args, int num_args);

static NhlErrorTypes CairoWorkstationGetValues(NhlLayer l, _NhlArgList args, int num_args);

/*
 * CairoWorkstation work_class method declarations
 */
static NhlErrorTypes CairoDocumentWorkstationInitialize(NhlClass class, NhlLayer req,
        NhlLayer new, _NhlArgList args, int num_args);
static NhlErrorTypes CairoImageWorkstationInitialize(NhlClass class, NhlLayer req,
        NhlLayer new, _NhlArgList args, int num_args);
static NhlErrorTypes CairoWindowWorkstationInitialize(NhlClass class, NhlLayer req,
        NhlLayer new, _NhlArgList args, int num_args);
#ifdef BuildQtEnabled
static NhlErrorTypes CairoQtWorkstationInitialize(NhlClass class, NhlLayer req,
        NhlLayer new, _NhlArgList args, int num_args);
#endif

static NhlErrorTypes CairoDocumentWorkstationOpen(NhlLayer instance);
static NhlErrorTypes CairoImageWorkstationOpen(NhlLayer instance);
static NhlErrorTypes CairoWindowWorkstationOpen(NhlLayer instance);
#ifdef BuildQtEnabled
static NhlErrorTypes CairoQtWorkstationOpen(NhlLayer instance);
#endif

static NhlErrorTypes CairoDocumentWorkstationActivate(NhlLayer instance);
static NhlErrorTypes CairoImageWorkstationActivate(NhlLayer instance);
static NhlErrorTypes CairoWindowWorkstationActivate(NhlLayer instance);
#ifdef BuildQtEnabled
static NhlErrorTypes CairoQtWorkstationActivate(NhlLayer instance);
#endif

static NhlErrorTypes CairoImageWorkstationClear(NhlLayer instance);
static NhlErrorTypes CairoWindowWorkstationClear(NhlLayer instance);
#ifdef BuildQtEnabled
static NhlErrorTypes CairoQtWorkstationClear(NhlLayer instance);
#endif

/* class-record for PS/PDF output formats */
NhlCairoWorkstationClassRec NhlcairoDocumentWorkstationClassRec = {
    {
        /* class_name           */ "documentWorkstationClass",
        /* nrm_class            */ NrmNULLQUARK,
        /* layer_size           */ sizeof (NhlCairoWorkstationLayerRec),
        /* class_inited         */ False,
        /* superclass           */ (NhlClass) & NhlworkstationClassRec,
        /* cvt_table            */ NULL,

        /* layer_resources      */ resourcesDocumentWS,
        /* num_resources        */ NhlNumber(resourcesDocumentWS),
        /* all_resources        */ NULL,
        /* callbacks            */ NULL,
        /* num_callbacks        */ 0,
        /* class_callbacks      */ NULL,
        /* num_class_callbacks  */ 0,

        /* class_part_initialize */ CairoWorkstationClassPartInitialize,
        /* class_initialize      */ CairoWorkstationClassInitialize,
        /* layer_initialize      */ CairoDocumentWorkstationInitialize,
        /* layer_set_values      */ CairoWorkstationSetValues,
        /* layer_set_values_hook */ NULL,
        /* layer_get_values      */ CairoWorkstationGetValues,
        /* layer_reparent        */ NULL,
        /* layer_destroy         */ CairoWorkstationDestroy,

        /* child_resources       */ NULL,

        /* layer_draw            */ NULL,

        /* layer_pre_draw        */ NULL,
        /* layer_draw_segonly    */ NULL,
        /* layer_post_draw       */ NULL,
        /* layer_clear           */ NULL
    },

    {
        /* current_wks_count */ NhlInheritCurrentWksCount,
        /* gks_wks_recs      */ NhlInheritGksWksRecs,
        /* hlu_wks_flag      */ NhlInheritHluWksFlag,
        /* def_background    */
        {1.0, 1.0, 1.0},
        /* rgb_dbm           */ NULL,
        /* pal               */ NhlInheritPalette,
        /* open_work         */ CairoDocumentWorkstationOpen,
        /* close_work        */ NhlInheritClose,
        /* activate_work     */ CairoDocumentWorkstationActivate,
        /* deactivate_work   */ NhlInheritDeactivate,
        /* alloc_colors      */ NhlInheritAllocateColors,
        /* update_work       */ NhlInheritUpdate,
        /* clear_work        */ NhlInheritClear,
        /* lineto_work       */ NhlInheritLineTo,
        /* fill_work         */ NhlInheritFill,
        /* marker_work       */ NhlInheritMarker,
        /* notify_work       */ NULL,
        /* update_drawbb     */ NULL
    },

    {
        /* foo  */ 0
    }

};

/* class-record for image-based output formats */
NhlCairoWorkstationClassRec NhlcairoImageWorkstationClassRec = {
    {
        /* class_name           */ "imageWorkstationClass",
        /* nrm_class            */ NrmNULLQUARK,
        /* layer_size           */ sizeof (NhlCairoWorkstationLayerRec),
        /* class_inited         */ False,
        /* superclass           */ (NhlClass) & NhlworkstationClassRec,
        /* cvt_table            */ NULL,

        /* layer_resources      */ resourcesImageWS,
        /* num_resources        */ NhlNumber(resourcesImageWS),
        /* all_resources        */ NULL,
        /* callbacks            */ NULL,
        /* num_callbacks        */ 0,
        /* class_callbacks      */ NULL,
        /* num_class_callbacks  */ 0,

        /* class_part_initialize */ CairoWorkstationClassPartInitialize,
        /* class_initialize      */ CairoWorkstationClassInitialize,
        /* layer_initialize      */ CairoImageWorkstationInitialize,
        /* layer_set_values      */ CairoWorkstationSetValues,
        /* layer_set_values_hook */ NULL,
        /* layer_get_values      */ CairoWorkstationGetValues,
        /* layer_reparent        */ NULL,
        /* layer_destroy         */ CairoWorkstationDestroy,

        /* child_resources       */ NULL,

        /* layer_draw            */ NULL,

        /* layer_pre_draw        */ NULL,
        /* layer_draw_segonly    */ NULL,
        /* layer_post_draw       */ NULL,
        /* layer_clear           */ NULL
    },

    {
        /* current_wks_count */ NhlInheritCurrentWksCount,
        /* gks_wks_recs      */ NhlInheritGksWksRecs,
        /* hlu_wks_flag      */ NhlInheritHluWksFlag,
        /* def_background    */
        {1.0, 1.0, 1.0},
        /* rgb_dbm           */ NULL,
        /* pal               */ NhlInheritPalette,
        /* open_work         */ CairoImageWorkstationOpen,
        /* close_work        */ NhlInheritClose,
        /* activate_work     */ CairoImageWorkstationActivate,
        /* deactivate_work   */ NhlInheritDeactivate,
        /* alloc_colors      */ NhlInheritAllocateColors,
        /* update_work       */ NhlInheritUpdate,
        /* clear_work        */ /*** CairoImageWorkstationClear, ****/ NhlInheritClear,
        /* lineto_work       */ NhlInheritLineTo,
        /* fill_work         */ NhlInheritFill,
        /* marker_work       */ NhlInheritMarker,
        /* notify_work       */ NULL,
        /* update_drawbb     */ NULL
    },

    {
        /* foo  */ 0
    }

};

/* class-record for window-based output formats */
NhlCairoWorkstationClassRec NhlcairoWindowWorkstationClassRec = {
    {
        /* class_name           */ "windowWorkstationClass",
        /* nrm_class            */ NrmNULLQUARK,
        /* layer_size           */ sizeof (NhlCairoWorkstationLayerRec),
        /* class_inited         */ False,
        /* superclass           */ (NhlClass) & NhlworkstationClassRec,
        /* cvt_table            */ NULL,

        /* layer_resources      */ resourcesWindowWS,
        /* num_resources        */ NhlNumber(resourcesWindowWS),
        /* all_resources        */ NULL,
        /* callbacks            */ NULL,
        /* num_callbacks        */ 0,
        /* class_callbacks      */ NULL,
        /* num_class_callbacks  */ 0,

        /* class_part_initialize */ CairoWorkstationClassPartInitialize,
        /* class_initialize      */ CairoWorkstationClassInitialize,
        /* layer_initialize      */ CairoWindowWorkstationInitialize,
        /* layer_set_values      */ CairoWorkstationSetValues,
        /* layer_set_values_hook */ NULL,
        /* layer_get_values      */ CairoWorkstationGetValues,
        /* layer_reparent        */ NULL,
        /* layer_destroy         */ CairoWorkstationDestroy,

        /* child_resources       */ NULL,

        /* layer_draw            */ NULL,

        /* layer_pre_draw        */ NULL,
        /* layer_draw_segonly    */ NULL,
        /* layer_post_draw       */ NULL,
        /* layer_clear           */ NULL
    },

    {
        /* current_wks_count */ NhlInheritCurrentWksCount,
        /* gks_wks_recs      */ NhlInheritGksWksRecs,
        /* hlu_wks_flag      */ NhlInheritHluWksFlag,
        /* def_background    */
        {1.0, 1.0, 1.0},
        /* rgb_dbm           */ NULL,
        /* pal               */ NhlInheritPalette,
        /* open_work         */ CairoWindowWorkstationOpen,
        /* close_work        */ NhlInheritClose,
        /* activate_work     */ CairoWindowWorkstationActivate,
        /* deactivate_work   */ NhlInheritDeactivate,
        /* alloc_colors      */ NhlInheritAllocateColors,
        /* update_work       */ NhlInheritUpdate,
        /* clear_work        */ CairoWindowWorkstationClear,
        /* lineto_work       */ NhlInheritLineTo,
        /* fill_work         */ NhlInheritFill,
        /* marker_work       */ NhlInheritMarker,
        /* notify_work       */ NULL,
        /* update_drawbb     */ NULL
    },

    {
        /* foo  */ 0
    }

};

#ifdef BuildQtEnabled
/* class-record for qt-based output formats */
NhlCairoWorkstationClassRec NhlcairoQtWorkstationClassRec = {
    {
        /* class_name           */ "qtWorkstationClass",
        /* nrm_class            */ NrmNULLQUARK,
        /* layer_size           */ sizeof (NhlCairoWorkstationLayerRec),
        /* class_inited         */ False,
        /* superclass           */ (NhlClass) & NhlworkstationClassRec,
        /* cvt_table            */ NULL,

        /* layer_resources      */ resourcesQtWS,
        /* num_resources        */ NhlNumber(resourcesQtWS),
        /* all_resources        */ NULL,
        /* callbacks            */ NULL,
        /* num_callbacks        */ 0,
        /* class_callbacks      */ NULL,
        /* num_class_callbacks  */ 0,

        /* class_part_initialize */ CairoWorkstationClassPartInitialize,
        /* class_initialize      */ CairoWorkstationClassInitialize,
        /* layer_initialize      */ CairoQtWorkstationInitialize,
        /* layer_set_values      */ CairoWorkstationSetValues,
        /* layer_set_values_hook */ NULL,
        /* layer_get_values      */ CairoWorkstationGetValues,
        /* layer_reparent        */ NULL,
        /* layer_destroy         */ CairoWorkstationDestroy,

        /* child_resources       */ NULL,

        /* layer_draw            */ NULL,

        /* layer_pre_draw        */ NULL,
        /* layer_draw_segonly    */ NULL,
        /* layer_post_draw       */ NULL,
        /* layer_clear           */ NULL
    },

    {
        /* current_wks_count */ NhlInheritCurrentWksCount,
        /* gks_wks_recs      */ NhlInheritGksWksRecs,
        /* hlu_wks_flag      */ NhlInheritHluWksFlag,
        /* def_background    */
        {1.0, 1.0, 1.0},
        /* rgb_dbm           */ NULL,
        /* pal               */ NhlInheritPalette,
        /* open_work         */ CairoQtWorkstationOpen,
        /* close_work        */ NhlInheritClose,
        /* activate_work     */ CairoQtWorkstationActivate,
        /* deactivate_work   */ NhlInheritDeactivate,
        /* alloc_colors      */ NhlInheritAllocateColors,
        /* update_work       */ NhlInheritUpdate,
        /* clear_work        */ CairoQtWorkstationClear,
        /* lineto_work       */ NhlInheritLineTo,
        /* fill_work         */ NhlInheritFill,
        /* marker_work       */ NhlInheritMarker,
        /* notify_work       */ NULL,
        /* update_drawbb     */ NULL
    },

    {
        /* foo  */ 0
    }

};
#endif

NhlClass NhlcairoDocumentWorkstationClass = (NhlClass) & NhlcairoDocumentWorkstationClassRec;
NhlClass NhlcairoImageWorkstationClass = (NhlClass) & NhlcairoImageWorkstationClassRec;
NhlClass NhlcairoWindowWorkstationClass = (NhlClass) & NhlcairoWindowWorkstationClassRec;
#ifdef BuildQtEnabled
NhlClass NhlcairoQtWorkstationClass = (NhlClass) & NhlcairoQtWorkstationClassRec;
#endif

/*
 * Function:    nhlfcairoXXXXXworkstationclass
 *
 * Description: fortran ref to this class
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:   global Fortran
 * Returns: NhlClass
 * Side Effect:
 */

NhlClass
_NHLCALLF(nhlfcairopspdfworkstationclass, NHLFCAIRODOCUMENTWORKSTATIONCLASS)
(void) { return NhlcairoDocumentWorkstationClass; }

NhlClass
_NHLCALLF(nhlfcairoimageworkstationclass, NHLFCAIROIMAGEWORKSTATIONCLASS)
(void) { return NhlcairoImageWorkstationClass; }

NhlClass
_NHLCALLF(nhlfcairowindowworkstationclass, NHLFCAIROWINDOWWORKSTATIONCLASS)
(void) { return NhlcairoWindowWorkstationClass; }

#ifdef BuildQtEnabled
NhlClass
_NHLCALLF(nhlfcairoqtworkstationclass, NHLFCAIROQTWORKSTATIONCLASS)
(void) { return NhlcairoQtWorkstationClass; }
#endif

/*
 * Function:    CairoWorkstationClassPartInitialize
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:
 * Returns:
 * Side Effect:
 */

static NhlErrorTypes
CairoWorkstationClassPartInitialize(NhlClass layerClass) {
    return NhlNOERROR;
}


static NrmQuark fnameQ = NrmNULLQUARK;
/*
 * Function:    CairoWorkstationClassInitialize
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:
 * Returns:
 * Side Effect:
 */

static NhlErrorTypes
CairoWorkstationClassInitialize(void) {
    static int classInitialized = 0;

    if (!classInitialized) {
        _NhlEnumVals documentFormats[] = {
            {NhlCPS, "PS"},
            {NhlCPS, "NEWPS"},
            {NhlCPDF, "PDF"},
            {NhlCPDF, "NEWPDF"},
            {NhlCEPS, "EPS"},
            {NhlCSVG, "SVG"}
        };

        _NhlEnumVals imageFormats[] = {
            {NhlCPNG, "NEWPNG"},
            {NhlCPNG, "PNG"},
            {NhlCTIFF, "TIFF"},
        };

        _NhlEnumVals windowFormats[] = {
            {NhlCX11, "X11"}
        };

        _NhlEnumVals qtFormats[] = {
            {NhlCQT, "QT"}
        };


        _NhlEnumVals orientvals[] = {
            {NhlPORTRAIT, "Portrait"},
            {NhlLANDSCAPE, "Landscape"}
        };


        (void) _NhlRegisterEnumType(NhlcairoDocumentWorkstationClass, NhlTCairoFormat,
                documentFormats, NhlNumber(documentFormats));
        (void) _NhlRegisterEnumType(NhlcairoImageWorkstationClass, NhlTCairoFormat,
                imageFormats, NhlNumber(imageFormats));
        (void) _NhlRegisterEnumType(NhlcairoWindowWorkstationClass, NhlTCairoFormat,
                windowFormats, NhlNumber(windowFormats));
#ifdef BuildQtEnabled
        (void) _NhlRegisterEnumType(NhlcairoQtWorkstationClass, NhlTCairoFormat,
                qtFormats, NhlNumber(qtFormats));
#endif

        (void) _NhlRegisterEnumType(NhlcairoDocumentWorkstationClass, NhlTWorkOrientation,
                orientvals, NhlNumber(orientvals));

        fnameQ = NrmStringToQuark(NhlNwkFileName);

        classInitialized = 1;
    }

    return NhlNOERROR;
}

/*
 * Function:    CairoXXXXXWorkstationInitialize
 *
 * Description: Set Cairo Workstation type, filename, device coordinates
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes
CairoDocumentWorkstationInitialize(NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args) {
    char func[] = "CairoDocumentWorkstationInitialize";

    NhlCairoWorkstationLayer newCairo = (NhlCairoWorkstationLayer) new;
    NhlCairoWorkstationLayerPart *cairo = &newCairo->cairo;
    NhlErrorTypes ret1 = NhlNOERROR;
    NhlErrorTypes ret2 = NhlNOERROR;

    newCairo->work.gkswksconid = 0;
    cairo->dev_bounds_updated = False;

    /*
     * Set gkswkstype
     */
    char* suffix;
    switch (cairo->format) {
        case NhlCPS:
            newCairo->work.gkswkstype = CPS;
            suffix = "ps";
            break;

        case NhlCPDF:
            newCairo->work.gkswkstype = CPDF;
            suffix = "pdf";
            break;

        case NhlCEPS:
            newCairo->work.gkswkstype = CEPS;
            suffix = "eps";
            break;
            
        case NhlCSVG:
            newCairo->work.gkswkstype = CSVG;
            suffix = "svg";
            break;            

        default:
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "%s: Invalid format \"%d\" defaulting to postscript",
                    func, cairo->format);
            newCairo->work.gkswkstype = CPS;
            suffix = "ps";
            break;
    }

    ret1 = fixupFilename(newCairo, suffix, func);
    ret2 = checkUlLRBounds(cairo, func);

    /* make a copy of this string */
    char* tmpStr = cairo->paper_size;
    cairo->paper_size = NhlMalloc(strlen(tmpStr) + 1);
    if (!cairo->paper_size) {
        NHLPERROR((NhlFATAL, ENOMEM, NULL));
        return NhlFATAL;
    }
    strcpy(cairo->paper_size, tmpStr);


    /* NOTE THIS IMPLICITLY USES THE FACT THAT ERROR-RETURN-VALUES ARE MORE NEGATIVE WITH INCREASING SEVERITY */
    return ((ret1 < ret2) ? ret1 : ret2);
}

static NhlErrorTypes
CairoImageWorkstationInitialize(NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args) {
    char func[] = "CairoImageWorkstationInitialize";

    NhlCairoWorkstationLayer newCairo = (NhlCairoWorkstationLayer) new;
    NhlCairoWorkstationLayerPart *cairo = &newCairo->cairo;
    NhlErrorTypes ret1 = NhlNOERROR;
    NhlErrorTypes ret2 = NhlNOERROR;

    newCairo->work.gkswksconid = 0;
    cairo->dev_bounds_updated = False;

    /*
     * Set gkswkstype
     */
    char* suffix;
    switch (cairo->format) {
        case NhlCPNG:
            newCairo->work.gkswkstype = CPNG;
            suffix = "png";
            break;

        case NhlCTIFF:
            newCairo->work.gkswkstype = CTIFF;
            suffix = "tif";
            break;
            
        default:
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "%s: Invalid format \"%d\" defaulting to PNG",
                    func, cairo->format);
            newCairo->work.gkswkstype = CPNG;
            suffix = "png";
            break;
    }

    ret1 = fixupFilename(newCairo, suffix, func);
    ret2 = checkUlLRBounds(cairo, func);

    /* NOTE THIS IMPLICITLY USES THE FACT THAT ERROR-RETURN-VALUES ARE MORE NEGATIVE WITH INCREASING SEVERITY */
    return ((ret1 < ret2) ? ret1 : ret2);
}

static NhlErrorTypes
CairoWindowWorkstationInitialize(NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args) {
    char func[] = "CairoWindowWorkstationInitialize";

    NhlCairoWorkstationLayer newCairo = (NhlCairoWorkstationLayer) new;
    NhlCairoWorkstationLayerPart *cairoLayer = &newCairo->cairo;
    NhlErrorTypes ret = NhlNOERROR;
    char* tstr;

    newCairo->work.gkswksconid = 0;
    cairoLayer->dev_bounds_updated = False;

    /*
     * Set gkswkstype
     */
    if (cairoLayer->format != NhlCX11) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
                "%s: Invalid format \"%d\" defaulting to X11",
                func, cairoLayer->format);
    }
    newCairo->work.gkswkstype = CX11;

    /*
     * What follows was adapted from the Xworkstation.c HLU.  I'm not certain the logic
     * with respect to the "wkPause" resource is correct, but we'll go with the older behavior for
     * now --RLB 11/2011.
     */
    if (!cairoLayer->pause_set) cairoLayer->pause = True;

    if (cairoLayer->window_id_set) {
        newCairo->work.gkswksconid = cairoLayer->window_id;
        /*
         * Force pause to False if the user provides a window id
         * GKS can't grab event's and still allow the user to grab
         * events.
         */
        if ((cairoLayer->pause_set) && (cairoLayer->pause)) {
            NhlPError(NhlINFO, NhlEUNKNOWN,
                    "%s:If the %s resource is specified, the %s resource must be False",
                    func, NhlNwkWindowId, NhlNwkPause);
            ret = NhlINFO;
        }
        cairoLayer->pause = False;

    }

    if (!cairoLayer->xwinconfig.title)
        cairoLayer->xwinconfig.title = (char*) newCairo->base.name;
    tstr = cairoLayer->xwinconfig.title;
    cairoLayer->xwinconfig.title =
            (char*) NhlMalloc((unsigned) strlen(tstr) + 1);
    if (!cairoLayer->xwinconfig.title) {
        NHLPERROR((NhlFATAL, ENOMEM, NULL));
        return NhlFATAL;
    }
    strcpy(cairoLayer->xwinconfig.title, tstr);

    if (!cairoLayer->xwinconfig.icon_title)
        cairoLayer->xwinconfig.icon_title = (char*) newCairo->base.name;
    tstr = cairoLayer->xwinconfig.icon_title;
    cairoLayer->xwinconfig.icon_title =
            (char*) NhlMalloc((unsigned) strlen(tstr) + 1);
    if (!cairoLayer->xwinconfig.icon_title) {
        NHLPERROR((NhlFATAL, ENOMEM, NULL));
        return NhlFATAL;
    }
    strcpy(cairoLayer->xwinconfig.icon_title, tstr);

    return ret;
}

#ifdef BuildQtEnabled
static NhlErrorTypes
CairoQtWorkstationInitialize(NhlClass lclass, NhlLayer req, NhlLayer new, _NhlArgList args, int num_args) {
    char func[] = "CairoQtWorkstationInitialize";

    NhlCairoWorkstationLayer newCairo = (NhlCairoWorkstationLayer) new;
    NhlCairoWorkstationLayerPart *cairoLayer = &newCairo->cairo;
    NhlErrorTypes ret = NhlNOERROR;
    char* tstr;

    newCairo->work.gkswksconid = 0;
    cairoLayer->dev_bounds_updated = False;

    /*
     * Set gkswkstype
     */
    if(cairoLayer->format != NhlCQT)
    {
        NHLPERROR((NhlWARNING, NhlEUNKNOWN,
                "%s: Invalid format \"%d\" defaulting to QT",
                func, cairoLayer->format));
    }
    newCairo->work.gkswkstype = CQT;

    cairoLayer->pause = True;

    if (cairoLayer->window_id_set)
    {
        newCairo->work.gkswksconid = cairoLayer->window_id;
        /*
         * Force pause to False if the user provides a window id
         * GKS can't grab event's and still allow the user to grab
         * events.
         */
        if ((cairoLayer->pause_set) && (cairoLayer->pause))
        {
            NHLPERROR((NhlINFO, NhlEUNKNOWN,
                    "%s:If the %s resource is specified, the %s resource must be False",
                    func, NhlNwkWindowId, NhlNwkPause));
            ret = NhlINFO;
        }
        cairoLayer->pause = False;
    }

    if (!cairoLayer->xwinconfig.title)
        cairoLayer->xwinconfig.title = (char*) newCairo->base.name;
    tstr = cairoLayer->xwinconfig.title;
    cairoLayer->xwinconfig.title =
            (char*) NhlMalloc((unsigned) strlen(tstr) + 1);
    if (!cairoLayer->xwinconfig.title) {
        NHLPERROR((NhlFATAL, ENOMEM, NULL));
        return NhlFATAL;
    }
    strcpy(cairoLayer->xwinconfig.title, tstr);

    if (!cairoLayer->xwinconfig.icon_title)
        cairoLayer->xwinconfig.icon_title = (char*) newCairo->base.name;
    tstr = cairoLayer->xwinconfig.icon_title;
    cairoLayer->xwinconfig.icon_title =
            (char*) NhlMalloc((unsigned) strlen(tstr) + 1);
    if (!cairoLayer->xwinconfig.icon_title) {
        NHLPERROR((NhlFATAL, ENOMEM, NULL));
        return NhlFATAL;
    }
    strcpy(cairoLayer->xwinconfig.icon_title, tstr);

    return ret;
}
#endif

/*
 * Function:    CairoWorkstationSetValues
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes
CairoWorkstationSetValues(NhlLayer old, NhlLayer ref, NhlLayer new, _NhlArgList args, int nargs) {
    char func[] = "CairoWorkstationInitialize";
    NhlCairoWorkstationLayerPart *newLp = &((NhlCairoWorkstationLayer) new)->cairo;
    NhlCairoWorkstationLayerPart *oldLp = &((NhlCairoWorkstationLayer) old)->cairo;
    NhlWorkstationLayerPart *newParent = &((NhlWorkstationLayer) new)->work;
    NhlWorkstationLayerPart *oldParent = &((NhlWorkstationLayer) old)->work;
    NhlErrorTypes ret = NhlNOERROR;

#if 0
    if (np->full_background != op->full_background) {
        c_ngseti("wo", _NhlWorkstationId(new));
        c_ngseti("fu", np->full_background);
    }
#endif

    if (newLp->lower_x != oldLp->lower_x ||
            newLp->upper_x != oldLp->upper_x ||
            newLp->lower_y != oldLp->lower_y ||
            newLp->upper_y != oldLp->upper_y ||
            newLp->orientation != oldLp->orientation)
        newLp->dev_bounds_updated = True;

    if (newLp->lower_x >= newLp->upper_x) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
                "%s:Device X Coordinates invalid, defaulting", func);
        ret = NhlWARNING;
        newLp->lower_x = 36;
        newLp->upper_x = 576;
    }

    if (newLp->lower_y >= newLp->upper_y) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
                "%s:Device Y Coordinates invalid, defaulting", func);
        ret = NhlWARNING;
        newLp->lower_y = 126;
        newLp->upper_y = 666;
    }
    
    if (newParent->bkgnd_opacity != oldParent->bkgnd_opacity) {
        _NhlSetBackgroundOpacity(new, newParent->bkgnd_opacity);
    }
    
    if (newParent->antialias != oldParent->antialias) {
        _NhlSetAntialiasingMode(new, NhlNON_TEXT_ANTIALIAS_MODE);
    }
    
    return ret;
}

/*
 * Function:    CairoWorkstationGetValues
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Scope:
 * Returns:
 * Side Effect:
 */
static NhlErrorTypes
CairoWorkstationGetValues(NhlLayer l, _NhlArgList args, int nargs) {
    char func[] = "CairoWorkStationGetValues";
    register int i;
    NhlCairoWorkstationLayerPart *cairo = &((NhlCairoWorkstationLayer) l)->cairo;
    NhlString str;
    NhlErrorTypes ret = NhlNOERROR;

    for (i = 0; i < nargs; i++) {
        str = NULL;

        if (args[i].quark == fnameQ) {
            str = cairo->filename;
        }

        if (str != NULL) {
            *(NhlString *) args[i].value.ptrval = NhlMalloc(strlen(str) + 1);
            if (! *(NhlString *) args[i].value.ptrval) {
                NhlPError(NhlWARNING, ENOMEM,
                        "%s:Unable to retrieve %s", func,
                        NrmQuarkToString(args[i].quark));
                ret = NhlWARNING;
            } else
                strcpy(*(NhlString *) args[i].value.ptrval, str);
        }
    }

    return ret;
}
/*
 * Function:    CairoWorkstationDestroy
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */
static NhlErrorTypes
CairoWorkstationDestroy(NhlLayer l) {
    NhlCairoWorkstationLayer layer = (NhlCairoWorkstationLayer) l;
    NhlCairoWorkstationLayerPart *cairo = &((NhlCairoWorkstationLayer) l)->cairo;

    if (cairo->format == NhlCX11) {
        if (cairo->xwinconfig.title)
            NhlFree(cairo->xwinconfig.title);
        if (cairo->xwinconfig.icon_title)
            NhlFree(cairo->xwinconfig.icon_title);
    }
  /*
   *else if (cairo->format == NhlCQT) {
   *    fprintf(stderr, "file %s, line: %d, function: %s\n",
   *                    __FILE__, __LINE__, __PRETTY_FUNCTION__);
   *}
   */
    else {
        if (cairo->filename)
            NhlFree(cairo->filename);
    }

    if (cairo->format == NhlCPS || cairo->format  == NhlCPDF || cairo->format == NhlCEPS)
        NhlFree(cairo->paper_size);

    return NhlNOERROR;
}

/*
 * Function:    CairoXXXXWorkstationOpen
 *
 * Description:
 *
 * In Args:
 *
 * Out Args:
 *
 * Return Values:
 *
 * Side Effects:
 */

static NhlErrorTypes
CairoDocumentWorkstationOpen(NhlLayer l) {
    NhlWorkstationLayer work = (NhlWorkstationLayer) l;
    NhlCairoWorkstationLayerPart *cairo = &((NhlCairoWorkstationLayer) l)->cairo;
    NhlErrorTypes ret;
    int d, w, h;


    /* make use of a shared utility method that contains all the page-sizing logic common to cairo-document,
     * postscript, and PDF workstations. See pageutil.c
     */
    NhlPageInfo pageInfo;
    pageInfo.paperSize = cairo->paper_size;
    pageInfo.paperSizeResName = NhlNwkPaperSize;
    pageInfo.paperWidthIn = cairo->page_width;
    pageInfo.paperWidthResName = NhlNwkPaperWidthF;
    pageInfo.paperHeightIn = cairo->page_height;
    pageInfo.paperHeightResName = NhlNwkPaperHeightF;

    ret = nhlGetPaperSize(&pageInfo);

    /* unbundle returned values */
    cairo->page_width = pageInfo.paperWidthIn;
    cairo->page_height = pageInfo.paperHeightIn;
    cairo->lower_x = (cairo->lower_x < 0) ? pageInfo.leftMargin : cairo->lower_x;
    cairo->upper_x = (cairo->upper_x < 0) ? pageInfo.rightMargin : cairo->upper_x;
    cairo->lower_y = (cairo->lower_y < 0) ? pageInfo.bottomMargin : cairo->lower_y;
    cairo->upper_y = (cairo->upper_y < 0) ? pageInfo.topMargin : cairo->upper_y;

    /* Note that these can be set for the "next" workstation */
    c_ngsetc("me", cairo->filename);
    c_ngseti("pw", pageInfo.pageWidthPts);
    c_ngseti("ph", pageInfo.pageHeightPts);
    c_ngseti("lx", cairo->lower_x);
    c_ngseti("ux", cairo->upper_x);
    c_ngseti("ly", cairo->lower_y);
    c_ngseti("uy", cairo->upper_y);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    /* these have to be set after the workstation is opened... */
    c_ngseti("wo", _NhlWorkstationId(l));
    c_ngseti("pl", cairo->orientation);

    w = cairo->upper_x - cairo->lower_x;
    h = cairo->upper_y - cairo->lower_y;
    d = MAX(w, h);
    
    setCairoFillHackValue(l);

    return ret;
}

static NhlErrorTypes
CairoImageWorkstationOpen(NhlLayer l) {
    NhlWorkstationLayer work = (NhlWorkstationLayer) l;
    NhlCairoWorkstationLayerPart *cairo = &((NhlCairoWorkstationLayer) l)->cairo;
    Gescape_in_data gesc_in_pixconf;
    NhlErrorTypes ret;

    /* we need to calculate the NDC frame within the image... */
    int minRange = (cairo->pixconfig.width < cairo->pixconfig.height) ? cairo->pixconfig.width : cairo->pixconfig.height;
    int adjust = (cairo->pixconfig.width - minRange) / 2;
    cairo->lower_x = adjust;
    cairo->upper_x = cairo->pixconfig.width - adjust;
    adjust = (cairo->pixconfig.height - minRange) / 2;
    cairo->lower_y = adjust;
    cairo->upper_y = cairo->pixconfig.height - adjust;

    /* Note that these can be set for the "next" workstation */
    c_ngsetc("me", cairo->filename);
    c_ngseti("lx", cairo->lower_x);
    c_ngseti("ux", cairo->upper_x);
    c_ngseti("ly", cairo->lower_y);
    c_ngseti("uy", cairo->upper_y);
    
    /* image width/height must be set before opening workstation */
    cairo->pixconfig.work_id = -1; /* part of the escape mechanism; -1 means "apply to *next* workstation */
    gesc_in_pixconf.escape_r1.data = &cairo->pixconfig;
    gesc_in_pixconf.escape_r1.size = sizeof (cairo->pixconfig);
    gescape(NGESC_CNATIVE, &gesc_in_pixconf, NULL, NULL);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    return ret;
}

static NhlErrorTypes
CairoWindowWorkstationOpen(NhlLayer l) {
    char func[] = "XWorkstationOpen";
    NhlWorkstationLayer work = (NhlWorkstationLayer) l;
    NhlCairoWorkstationLayerPart *cairo = &((NhlCairoWorkstationLayer) l)->cairo;
    Gescape_in_data gesc_in_xwconf;
    NhlErrorTypes ret;

    /* we need to calculate the NDC frame within the image... */
    int minRange = (cairo->xwinconfig.width < cairo->xwinconfig.height) ? cairo->xwinconfig.width : cairo->xwinconfig.height;
    int adjust = (cairo->xwinconfig.width - minRange) / 2;
    cairo->lower_x = adjust;
    cairo->upper_x = cairo->xwinconfig.width - adjust;
    adjust = (cairo->xwinconfig.height - minRange) / 2;
    cairo->lower_y = adjust;
    cairo->upper_y = cairo->xwinconfig.height - adjust;

    /* Note that these can be set for the "next" workstation */
    c_ngseti("lx", cairo->lower_x);
    c_ngseti("ux", cairo->upper_x);
    c_ngseti("ly", cairo->lower_y);
    c_ngseti("uy", cairo->upper_y);

    /* image width/height must be set before opening workstation */
    cairo->xwinconfig.work_id = -1; /* part of the escape mechanism; -1 means "apply to *next* workstation */
    gesc_in_xwconf.escape_r1.data = &cairo->xwinconfig;
    gesc_in_xwconf.escape_r1.size = sizeof(cairo->xwinconfig);
    gescape(NGESC_CNATIVE, &gesc_in_xwconf, NULL, NULL);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    return ret;
}

#ifdef BuildQtEnabled
static NhlErrorTypes
CairoQtWorkstationOpen(NhlLayer l) {
    char func[] = "QtWorkstationOpen";
    NhlWorkstationLayer work = (NhlWorkstationLayer) l;
    NhlCairoWorkstationLayerPart *cairo = &((NhlCairoWorkstationLayer) l)->cairo;
    Gescape_in_data gesc_in_xwconf;
    NhlErrorTypes ret;

    /* we need to calculate the NDC frame within the image... */
    int minRange = (cairo->xwinconfig.width < cairo->xwinconfig.height) ? cairo->xwinconfig.width : cairo->xwinconfig.height;
    int adjust = (cairo->xwinconfig.width - minRange) / 2;
    cairo->lower_x = adjust;
    cairo->upper_x = cairo->xwinconfig.width - adjust;
    adjust = (cairo->xwinconfig.height - minRange) / 2;
    cairo->lower_y = adjust;
    cairo->upper_y = cairo->xwinconfig.height - adjust;

    /* Note that these can be set for the "next" workstation */
    c_ngseti("lx", cairo->lower_x);
    c_ngseti("ux", cairo->upper_x);
    c_ngseti("ly", cairo->lower_y);
    c_ngseti("uy", cairo->upper_y);

    /* image width/height must be set before opening workstation */
    cairo->xwinconfig.work_id = -1; /* part of the escape mechanism; -1 means "apply to *next* workstation */
    gesc_in_xwconf.escape_r1.data = &cairo->xwinconfig;
    gesc_in_xwconf.escape_r1.size = sizeof(cairo->xwinconfig);
    gescape(NGESC_CNATIVE, &gesc_in_xwconf, NULL, NULL);

    ret = (*NhlworkstationClassRec.work_class.open_work)(l);

    return ret;
}
#endif

/*
 * Function:    CairoXXXXXWorkstationActivate
 *
 * Description:
 *
 * In Args:
 *      NhlLayer    l
 *
 * Out Args:
 *
 * Scope:   static
 * Returns: NhlErrorTypes
 * Side Effect:
 */

static NhlErrorTypes
CairoDocumentWorkstationActivate(NhlLayer l) {
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    NhlWorkstationLayerPart *wp = &((NhlWorkstationLayer) l)->work;
    NhlCairoWorkstationLayerPart *pp = &((NhlCairoWorkstationLayer) l)->cairo;
    int w, h, d;

    if (wp->cleared && pp->dev_bounds_updated) {
        c_ngseti("wo", _NhlWorkstationId(l));
        c_ngseti("lx", pp->lower_x);
        c_ngseti("ux", pp->upper_x);
        c_ngseti("ly", pp->lower_y);
        c_ngseti("uy", pp->upper_y);
        c_ngseti("pl", pp->orientation);
        pp->dev_bounds_updated = False;
    }

    w = pp->upper_x - pp->lower_x;
    h = pp->upper_y - pp->lower_y;
    d = MAX(w, h);

    return (*(lc->work_class.activate_work))(l);
}

static NhlErrorTypes
CairoImageWorkstationActivate(NhlLayer l) {
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    return (*(lc->work_class.activate_work))(l);
}

static NhlErrorTypes
CairoWindowWorkstationActivate(NhlLayer l) {
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    return (*(lc->work_class.activate_work))(l);
}

#ifdef BuildQtEnabled
static NhlErrorTypes
CairoQtWorkstationActivate(NhlLayer l) {
    NhlWorkstationClass lc = (NhlWorkstationClass) NhlworkstationClass;
    return (*(lc->work_class.activate_work))(l);
}
#endif

/*
 * Function:	CairoImageWorkstationClear
 *
 */
static NhlErrorTypes
CairoImageWorkstationClear(NhlLayer layer) {
#if 0   /******* THE GEOTIFF IMPLEMENTATION IS INCOMPLETE, AND NEEDS REFACTORING;  MAKE THIS DO NOTHING FOR NOW --RLB 5/2015 ****/    
    NhlWorkstationClass wksClass = (NhlWorkstationClass) NhlworkstationClass;
    NhlCairoWorkstationLayer cairoLayer  = (NhlCairoWorkstationLayer) layer;
    Gescape_in_data indat;
    Gescape_out_data *outdat;
    char wkid[15];

#if 0    
    if (cairoLayer->cairo.pause) {
        sprintf(wkid, "%d", _NhlWorkstationId(layer));
        indat.escape_r1.size = strlen(wkid);
        indat.escape_r1.data = wkid;
        gescape(-1396, &indat, NULL, &outdat);
    }
#endif
    int i;
    int pid = layer->base.id;
    int* views;
    ng_size_t count;
    NhlLayer instance;
    NhlErrorTypes ret, subret;
    int grlist = NhlRLCreate(NhlGETRL);
    NhlRLClear(grlist);
    NhlRLGetIntegerArray(grlist, NhlNwkTopLevelViews, &views, &count);
    NhlGetValues(pid, grlist);
    for (i = 0; i < count; i++) {
        instance = _NhlGetLayer(views[i]);
        if (instance->base.layer_class != NhlmapPlotClass)
            continue;

        setGeoreferenceData(cairoLayer, instance);
    }
    
    NhlFree(views);
    NhlRLDestroy(grlist);

    
    return (*(wksClass->work_class.clear_work))(layer);
#endif    
}

/*
 * Function:	CairoWindowWorkstationClear
 *
 * Borrowed from the XWorkstationClear function;  implements the
 * pause-for-user-input between clearing successive windows/plots.
 *
 */
static NhlErrorTypes
CairoWindowWorkstationClear(NhlLayer layer) {
    NhlWorkstationClass wksClass = (NhlWorkstationClass) NhlworkstationClass;
    NhlCairoWorkstationLayer cairoLayer  = (NhlCairoWorkstationLayer) layer;
    Gescape_in_data indat;
    Gescape_out_data *outdat;
    char wkid[15];

    if (cairoLayer->cairo.pause) {
        sprintf(wkid, "%d", _NhlWorkstationId(layer));
        indat.escape_r1.size = strlen(wkid);
        indat.escape_r1.data = wkid;
        gescape(-1396, &indat, NULL, &outdat);
    }

    return (*(wksClass->work_class.clear_work))(layer);
}

#ifdef BuildQtEnabled
static NhlErrorTypes
CairoQtWorkstationClear(NhlLayer layer) {
    NhlWorkstationClass wksClass = (NhlWorkstationClass) NhlworkstationClass;

    return (*(wksClass->work_class.clear_work))(layer);
}
#endif

/*
 * fixupFilename()
 *
 * Convenience function, called by workstation-types that write to a file.  Operates by side-effects, modifying
 * the layer's "filename" field.
 *
 */
NhlErrorTypes
fixupFilename(NhlCairoWorkstationLayer layer, char* filenameSuffix, char* callingFunc)
{
    NhlCairoWorkstationLayerPart *cairoLayer = &(layer->cairo);
    char *tfname = NULL;
    char buff[_NhlMAXFNAMELEN];
    NhlErrorTypes ret = NhlNOERROR;

    if (cairoLayer->filename) {
        tfname = (char*) _NGResolvePath(cairoLayer->filename);
        if (!tfname) {
            NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "%s:Unable to resolve path name for \"%s\", defaulting %s",
                    callingFunc, cairoLayer->filename, NhlNwkFileName);
            ret = NhlWARNING;
        }
    }

    if (!tfname) {
        strcpy(buff, layer->base.name);
        strcat(buff, ".");
        strcat(buff, filenameSuffix);
        tfname = buff;
    }

    if (strlen(tfname) > _NhlMAXLLUPATHLEN) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
                "%s: Filepath %s exceeds maximum length of %d", callingFunc,
                tfname, _NhlMAXLLUPATHLEN);
        return NhlFATAL;
    }

    /* This looks like a mem-leak at first glance, but upstream in _NhlCreate, a "context" has been keeping
     * track of memory allocated temporarily for the Layer, and cleans that up.
     *
     */
    cairoLayer->filename = NhlMalloc(strlen(tfname) + 1);
    if (!cairoLayer->filename) {
        NHLPERROR((NhlFATAL, ENOMEM, NULL));
        return NhlFATAL;
    }
    strcpy(cairoLayer->filename, tfname);

    return ret;
}

/*
 * checkUlLrBounds()
 *
 * Convenience function, called by workstations' initialize methods to test upper-left/lower-right
 * parameters for sanity.  If anything is wrong, sets those fields to default values and issues a
 * warning.
 *
 */
NhlErrorTypes
checkUlLRBounds(NhlCairoWorkstationLayerPart *cairoLayer, char* callingFunc)
{
    NhlErrorTypes ret = NhlNOERROR;

    if (cairoLayer->lower_x > 0 && cairoLayer->upper_x > 0 && cairoLayer->lower_x >= cairoLayer->upper_x) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
                "%s:Device X Coordinates invalid, defaulting", callingFunc);
        ret = NhlWARNING;
        cairoLayer->lower_x = 36;
        cairoLayer->upper_x = 576;
    }

    if (cairoLayer->lower_y > 0 && cairoLayer->upper_y > 0 && cairoLayer->lower_y >= cairoLayer->upper_y) {
        NhlPError(NhlWARNING, NhlEUNKNOWN,
                "%s:Device Y Coordinates invalid, defaulting", callingFunc);
        ret = NhlWARNING;
        cairoLayer->lower_y = 126;
        cairoLayer->upper_y = 666;
    }

    return ret;
}


/*
 * This hack is intended to address a deficiency in cairo's "fill" operation on document workstations (i.e., the
 * dreaded "thin white lines/gaps" problem). See Jira ticket ncl-1913.
 * 
 */

void setCairoFillHackValue(NhlLayer layer)
{
    NhlCairoWorkstationLayer wLayer = (NhlCairoWorkstationLayer) layer;        
    NhlBoolean fillMode = wLayer->cairo.cairo_fill_hack;        
    _NGCCairoFillHack fillHackRec;
    Gescape_in_data gesc;
    
    fillHackRec.type = NGC_CAIROFILLHACK;
    fillHackRec.work_id = wLayer->work.gkswksid;
    fillHackRec.fill_mode_boolean = fillMode;
    gesc.escape_r1.data = &fillHackRec;
    gesc.escape_r1.size = sizeof(fillHackRec);
    gescape(NGESC_CNATIVE, &gesc, NULL, NULL);                                
}       

#if 0    /* NEED TO REFACTOR WHERE WE GET THIS INFO */
#include "MapPlot.h"
#endif
static void setGeoreferenceData(NhlCairoWorkstationLayer wksLayer, NhlLayer mapLayer) {
#if 0  /*********** THIS CODE IS IN AN INTERMEDIATE STATE; PRESERVE IT, BUT MAKE THE FUNCTION DO NOTHING FOR NOW --RLB 5/2015  *******/
    _NGCGeoReference georefRec;
    Gescape_in_data gesc;

    NhlProjection projection;
    NhlMapLimitMode limit_mode;
    float center_lat, rotation;
    float min_lat, max_lat, min_lon, max_lon;
    NhlBoolean rel_center_lat, rel_center_lon;
    float center_lon = 0.0;
    NhlBoundingBox bbox;

/******** ALL THIS IS TEMPORARY FOR DETERMINING BEHAVIOR UNDER DIFFERENT LIMIT_MODES */
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftNDCF, &bbox.l,
            NhlNmpRightNDCF, &bbox.r,
            NhlNmpBottomNDCF, &bbox.b,
            NhlNmpTopNDCF, &bbox.t,
            NhlNmpProjection, &projection,
            NhlNmpLimitMode, &limit_mode,
            NhlNmpMinLatF, &min_lat,
            NhlNmpMaxLatF, &max_lat,
            NhlNmpMinLonF, &min_lon,
            NhlNmpMaxLonF, &max_lon,
            NhlNmpRelativeCenterLat, &rel_center_lat,
            NhlNmpCenterLatF, &center_lat,
            NhlNmpCenterLonF, &center_lon,
            NhlNmpRelativeCenterLon, &rel_center_lon,
            NhlNmpCenterRotF, &rotation,
            NULL);
    printf("projection:  %d,  limitMode: %d\n", projection, limit_mode);
    printf("  lon: %f .. %f,  lat: %f .. %f\n", min_lon, max_lon, min_lat, max_lat);
    printf("  center lon/lat: %f, %f\n", center_lon, center_lat);
    printf("  relcen lon/lat: %f, %f\n", rel_center_lon, rel_center_lat);
    printf("  rotation: %f\n", rotation);
    printf("  NDC (lrbt):  %f, %f, %f, %f\n", bbox.l, bbox.r, bbox.b, bbox.t);
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftCornerLonF, &bbox.l,
            NhlNmpRightCornerLonF, &bbox.r,
            NhlNmpLeftCornerLatF, &bbox.b,
            NhlNmpRightCornerLatF, &bbox.t,
            NULL);
    printf("  Corner (lrbt):  %f, %f, %f, %f\n", bbox.l, bbox.r, bbox.b, bbox.t);
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftPointLonF, &bbox.l,
            NhlNmpRightPointLonF, &bbox.r,
            NhlNmpBottomPointLatF, &bbox.b,
            NhlNmpTopPointLatF, &bbox.t,
            NULL);
    printf("  Point (lrbt):  %f, %f, %f, %f\n", bbox.l, bbox.r, bbox.b, bbox.t);
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftAngleF, &bbox.l,
            NhlNmpRightAngleF, &bbox.r,
            NhlNmpBottomAngleF, &bbox.b,
            NhlNmpTopAngleF, &bbox.t,
            NULL);
    printf("  Angle (lrbt):  %f, %f, %f, %f\n", bbox.l, bbox.r, bbox.b, bbox.t);
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftMapPosF, &bbox.l,
            NhlNmpRightMapPosF, &bbox.r,
            NhlNmpBottomMapPosF, &bbox.b,
            NhlNmpTopMapPosF, &bbox.t,
            NULL);
    printf("  MapPos (lrbt):  %f, %f, %f, %f\n", bbox.l, bbox.r, bbox.b, bbox.t);
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftWindowF, &bbox.l,
            NhlNmpRightWindowF, &bbox.r,
            NhlNmpBottomWindowF, &bbox.b,
            NhlNmpTopWindowF, &bbox.t,
            NULL);
    printf("  Window (lrbt):  %f, %f, %f, %f\n", bbox.l, bbox.r, bbox.b, bbox.t);

    float meridian, par1, par2, sat1, sat2, dist;
    NhlVAGetValues(mapLayer->base.id,
            NhlNmpLambertMeridianF, &meridian,
            NhlNmpLambertParallel1F, &par1,
            NhlNmpLambertParallel2F, &par2,
            NhlNmpSatelliteAngle1F, &sat1,
            NhlNmpSatelliteAngle2F, &sat2,
            NhlNmpSatelliteDistF, &dist,
            NULL);
    printf("Lambert meridian: %f,  stdpar1: %f,  stdpar2: %f\n", meridian, par1, par2);
    printf("Satellite dist: %f,  angle1: %f, angle2: %f\n", dist, sat1, sat2);
/****** END TEMPORARY CODE */


    
    georefRec.type = NGC_GEOREFERENCE;
    georefRec.work_id = wksLayer->work.gkswksid;


    /* get projection, limit_mode, and location of map corners in NDC space */
    NhlVAGetValues(mapLayer->base.id,
        NhlNmpProjection,    &projection,
        NhlNmpLimitMode,     &limit_mode,
        NhlNmpLeftMapPosF, &bbox.l,
        NhlNmpRightMapPosF, &bbox.r,
        NhlNmpBottomMapPosF, &bbox.b,
        NhlNmpTopMapPosF, &bbox.t,
        NULL);
    
    georefRec.ndcX[0] = bbox.l;  georefRec.ndcY[0] = bbox.b;
    georefRec.ndcX[1] = bbox.r;  georefRec.ndcY[1] = bbox.b;
    georefRec.ndcX[2] = bbox.r;  georefRec.ndcY[2] = bbox.t;
    georefRec.ndcX[3] = bbox.l;  georefRec.ndcY[3] = bbox.t;
    
    float oor;
    int status;
    NhlNDCToData(mapLayer->base.id, georefRec.ndcX, georefRec.ndcY, 4, 
            georefRec.worldX, georefRec.worldY, NULL, NULL, &status, &oor);
    
    printf("status = %d, oor = %f\n", status, oor);
    int i;
    for (i=0; i<4; i++) 
        printf("%f,%f   %f,%f\n", georefRec.ndcX[i], georefRec.ndcY[i], georefRec.worldX[i], georefRec.worldY[i]);
    
    /* Implementation Note:
     * The HLU's notation of map projections is defined in MapTransObj.h.  We somehow have to communicate that to 
     * the graphics subsystem. One approach would be to create yet another enumeration, presumably in gksP.h, that maps
     * the HLU's notion of projection onto a graphics-level notion. At the graphics-level, that in turn ultimately 
     * has to be mapped onto GeoTiff's notion.  Rather than create such a clunky mechanism, we'll just map HLU codes
     * straight to Geotiff codes here and be done with it.  Admittedly this bleeds knowledge of Geotiff into the 
     * CairoWorkstation, rather than encapsulating it all in the low-level graphics driver. But heck, this whole function
     * is intended to do one-and-only-one thing -- facilitate the writing a geotiff;  there is no higher abstraction
     * going on here.     --RLB 
     */
    switch (projection) {
	case NhlORTHOGRAPHIC:            georefRec.projCode = 21; break;
        case NhlSTEREOGRAPHIC:           georefRec.projCode = 14; break;
        case NhlLAMBERTEQUALAREA:        georefRec.projCode = 10; break;
	case NhlGNOMONIC:                georefRec.projCode = 19; break;
        case NhlAZIMUTHALEQUIDISTANT:    georefRec.projCode = 12; break;
        case NhlSATELLITE:               georefRec.projCode =  0; break;   /* no mapping onto Geotiff */
	case NhlPSEUDOMOLLWEIDE:         georefRec.projCode =  0; break;
        case NhlMERCATOR:                georefRec.projCode =  7; break;
        case NhlCYLINDRICALEQUIDISTANT:  georefRec.projCode = 13; break;   /* Equidistantconic in Geotiff */
	case NhlLAMBERTCONFORMAL:        georefRec.projCode =  8; break;
        case NhlROBINSON:                georefRec.projCode = 23; break;
        case NhlCYLINDRICALEQUALAREA:    georefRec.projCode = 17; break;   /* Equirectangular in Geotiff */
        case NhlROTATEDMERCATOR:         georefRec.projCode =  0; break;
        case NhlAITOFF:                  georefRec.projCode =  0; break;
        case NhlHAMMER:                  georefRec.projCode =  0; break;
        case NhlMOLLWEIDE:               georefRec.projCode =  0; break;
        case NhlWINKELTRIPEL:            georefRec.projCode =  0; break;
        default:                         georefRec.projCode =  0;                          
    }
    
    if (georefRec.projCode == 0) {  /* bail if true -- we don't have a valid mapping */
        return;
    }
        
#if 0    
    georefRec.ndcLLX = bbox.l;
    georefRec.ndcLLY = bbox.b;
    georefRec.ndcURX = bbox.r;
    georefRec.ndcURY = bbox.t;
    
    /* the value of limit_mode determines how we find the geo-space coordinates of the map corners */
    bbox.l = bbox.r = bbox.b = bbox.t = 0.;
    if (limit_mode == NhlMAXIMALAREA || limit_mode == NhlLATLON) {
        NhlVAGetValues(mapLayer->base.id,
            NhlNmpMinLonF, &bbox.l,
            NhlNmpMaxLonF, &bbox.r,
            NhlNmpMinLatF, &bbox.b,
            NhlNmpMaxLatF, &bbox.t,
            NULL);        
    }
    else if (limit_mode == NhlCORNERS) {
        NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftCornerLonF,  &bbox.l,
            NhlNmpRightCornerLonF, &bbox.r,
            NhlNmpLeftCornerLatF,  &bbox.b,
            NhlNmpRightCornerLatF, &bbox.t,
            NULL);
    }
    
    /* just speculating on these -- have not found an instance to verify the behavior */
    else if (limit_mode == NhlANGLES) {
        NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftAngleF,   &bbox.l,
            NhlNmpRightAngleF,  &bbox.r,
            NhlNmpBottomAngleF, &bbox.b,
            NhlNmpTopAngleF,    &bbox.t,
            NULL);
    }
    else if (limit_mode == NhlPOINTS) {
        NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftPointLonF,   &bbox.l,
            NhlNmpRightPointLonF,  &bbox.r,
            NhlNmpBottomPointLatF, &bbox.b,
            NhlNmpTopPointLatF,    &bbox.t,
            NULL);
    }
    else if (limit_mode == NhlWINDOW) {
        NhlVAGetValues(mapLayer->base.id,
            NhlNmpLeftWindowF,   &bbox.l,
            NhlNmpRightWindowF,  &bbox.r,
            NhlNmpBottomWindowF, &bbox.b,
            NhlNmpTopWindowF,    &bbox.t,
            NULL);
    }
    
    georefRec.lonLL = bbox.l;
    georefRec.latLL = bbox.b;
    georefRec.lonUR = bbox.r;
    georefRec.latUR = bbox.t;
#endif
    
    /* These projections have additional parameters */
    if (projection == NhlLAMBERTCONFORMAL) {
        float meridian, par1, par2;
        NhlVAGetValues(mapLayer->base.id,
            NhlNmpLambertMeridianF,  &meridian,
            NhlNmpLambertParallel1F, &par1,
            NhlNmpLambertParallel2F, &par2,
            NULL);
        georefRec.meridianOrDist = meridian;
        georefRec.parOrAngle1 = par1;
        georefRec.parOrAngle2 = par2;
    }
    
    /* send it all down the pike... */
    gesc.escape_r1.data = &georefRec;
    gesc.escape_r1.size = sizeof (georefRec);
    gescape(NGESC_CNATIVE, &gesc, NULL, NULL);
#endif    
}

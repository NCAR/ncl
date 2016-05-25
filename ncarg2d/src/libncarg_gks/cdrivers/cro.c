/*
 *      $Id: cro.c,v 1.15.2.3 2010-05-07 15:51:56 brownrig Exp $
 */
/*
 *
 *      File:           cro.c
 *
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu Feb 28 18:03:09 MST 2008
 *
 *      Description:    This file contains the cairo device driver
 *                      functions.
 */
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <time.h>

#include <math.h>
#include <stdlib.h>
#include <errno.h>
#include <ncarg/ncargC.h>
#include <ncarg/gksP.h>
#include <ncarg/c.h>
#include <ncarg/gks.h>
#include "gksc.h"
#include "gks.h"
#include "common.h"
#include "cro.h"
#include "cro_device.h"
#include "crotiff.h"
#include "croddi.h"
#include "argb.h"

#define JIRA_494 1   /* temporary, until issued resolved */

#define PI 3.1415926
#define RINT(A) ((A) > 0 ? (int) ((A) + 0.5) : -(int) (0.5 - (A)))

/* forward declarations... */
static const char *getFileNameRoot(int, const char*, const char*);
static char *getRegularOutputFilename(int, const char*, const char*, const char*);
static char *getIndexedOutputFilename(int, const char*, int, const char*, const char*);
static void setSurfaceTransform(CROddp *psa);
static void CROinit(CROddp *, int *);
static void CROpict_init(GKSC *gksc);
static void CROInitCairoContext(CROddp* psa, cairo_t* context, cairo_surface_t* surface);
static unsigned int pack_argb(struct color_value);
static struct color_value unpack_argb(unsigned int*, unsigned int);
static int ccompar(const void *p1, const void *p2);
static void cascsrt(float xa[], int ip[], int n);
static float *csort_array;
static void reverse_chrs(char*);
static int cro_CEsc(CROddp *psa, _NGCesc *cesc);

#ifdef BuildQtEnabled
extern void croActivateQt(CROddp *psa);
#endif

/* this looks like it should be static, but there's a pattern where the SoftFill() routines
 * are declared external for this, the PS, and the PDF drivers. Leaving it for now.  --RLB 1/2010.
 */
void cro_SoftFill(GKSC *gksc, float angle, float spl);

/* helper functions for X11-based surfaces */
extern cairo_surface_t* croCreateNativeDisplaySurface(CROddp* psa);
extern void croX11Pause(cairo_surface_t* surface);
extern void croFreeNativeSurface(cairo_surface_t* surface);
extern void croActivateX11(CROddp* psa, cairo_surface_t* surface);

#ifdef BuildQtEnabled
/* Globals for QT-based interactive view tool (to be named?) */
cairo_surface_t *qt_surface = NULL;
cairo_t         *qt_context = NULL;
int qt_screen_width  = 1000;
int qt_screen_height = 1000;
#endif

/*
 *  Functions and globals for mapping workstation IDs into indices for the
 *  cairo context and surface arrays.
 */

#define NUM_CONTEXT 20  /* number of allowable contexts */
static int contextNum = 0;
static cairo_surface_t *cairoSurfaces[NUM_CONTEXT];
static cairo_t         *cairoContexts[NUM_CONTEXT];
static int             cairoEnvIndices[NUM_CONTEXT];

static int getCairoEnvIndex(int wksId) {
    int i;
    for (i = 0; i < NUM_CONTEXT; i++) {
        if (cairoEnvIndices[i] == wksId) {
            return i;
        }
    }

    return -1;
}

static cairo_t* getContext(int wksId) {
    return cairoContexts[getCairoEnvIndex(wksId)];
}

static cairo_surface_t* getSurface(int wksId) {
    return cairoSurfaces[getCairoEnvIndex(wksId)];    
}

static void saveCairoEnv(int wksId, cairo_t* context, cairo_surface_t* surface) {
    cairoEnvIndices[contextNum] = wksId;
    cairoContexts[contextNum] = context;
    cairoSurfaces[contextNum] = surface;
    ++contextNum;
}

static void removeCairoEnv(int wkid) {
    int i;
    int index = getCairoEnvIndex(wkid);
    for (i=index+1; i<contextNum; i++) {
        cairoSurfaces[i-1] = cairoSurfaces[i];
        cairoContexts[i-1] = cairoContexts[i];
        cairoEnvIndices[i-1] = cairoEnvIndices[i];
    }

    --contextNum;
}

/* Tracing/Debugging functions.
 * We used to trace by calling getenv("CRO_TRACE") at the top of every function that follows, printing 
 * a message if the environment variable was set.  I've refactored all that logic into a "trace()" function.
 * "trace" is actually a function pointer, initially pointing to an initialization function that performs 
 * the getenv() test just once. "trace" is then modified to point to a function that does the per-invocation 
 * testing/printing.
 */
static int isTracingOn;
static void (*trace)(const char*);
static void traceTest(const char* message) {
    if (isTracingOn)
        printf("%s\n", message);
}
static void traceInit(const char* message) {
    isTracingOn = (getenv("CRO_TRACE")) ? 1 : 0;
    trace = traceTest;
    trace(message);
}
static void (*trace)(const char*) = &traceInit;

/*
 *  Picture initialization.  Called once by the first drawing routine (polyline, polymarker, fill, cell, text)
 *  that gets invoked.
 *
 */
void CROpict_init(GKSC *gksc) {
    /*
     *  Put out background.
     */
    CROddp *psa = (CROddp *) gksc->ddp;

    struct color_value cval;

    trace("Got to CROpict_init");
    
    cairo_t* context = getContext(psa->wks_id);

    /*
     *  Get the background color and set the source to the background color.
     */
    cval = unpack_argb(psa->ctable, 0);
    
    /* Save this for Wei; transparent background can now be had via wkBackgroundOpacityF resource */
    if(CQT == psa->wks_type)
    {
        cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, 0.0);
    }
    else
    {
        cairo_set_source_rgba(context, cval.red, cval.green, cval.blue,
                              ALPHA_BLEND(cval.alpha, psa->background_alpha));
        cairo_set_operator(context, CAIRO_OPERATOR_SOURCE);
    }

    /* NOTE: This is likely not quite right, but I don't understand the use of the clipping rectangle below. In any case,
     * the code that does the Right Thing for PS/PDF does not result in a complete fill for image-based formats,
     * so we proceed differently  --RLB
     */
    if (psa->wks_type == CPS || psa->wks_type == CPDF || psa->wks_type == CEPS) {
        double xl, yt, xr, yb;

        /*
         *  Save the current clip extents and reset the clipping rectangle to max.
         */
        cairo_clip_extents(context, &xl, &yt, &xr, &yb);
        cairo_reset_clip(context);

        /*
         *  Set the clipping rectangle to the surface area.
         */
        cairo_move_to(context, 0., 0.);
        cairo_line_to(context, psa->dspace.urx - psa->dspace.llx, 0.);
        cairo_line_to(context, psa->dspace.urx - psa->dspace.llx, 
                psa->dspace.ury - psa->dspace.lly);
        cairo_line_to(context, 0., psa->dspace.ury - psa->dspace.lly);
        cairo_line_to(context, 0., 0.);
        cairo_clip(context);

        /*
         *  Fill the surface clip region with the background color.
         */
        cairo_move_to(context, 0., 0.);
        cairo_line_to(context, psa->dspace.urx - psa->dspace.llx, 0.);
        cairo_line_to(context, psa->dspace.urx - psa->dspace.llx, 
                psa->dspace.ury - psa->dspace.lly);
        cairo_line_to(context, 0., psa->dspace.ury - psa->dspace.lly);
        cairo_line_to(context, 0., 0.);

        cairo_fill(context);

        /*
         *  Restore the clipping rectangle to what it was on entry.
         *  cairo_clip clears the path.
         */
        cairo_move_to(context, xl, yt);
        cairo_line_to(context, xr, yt);
        cairo_line_to(context, xr, yb);
        cairo_line_to(context, xl, yb);
        cairo_line_to(context, xl, yt);
        cairo_clip(context);
    } else {
        /* code for image-based formats */
        cairo_save(context);
        cairo_reset_clip(context);
        cairo_identity_matrix(context);
        cairo_rectangle(context, 0, 0, psa->image_width, psa->image_height);
        cairo_fill(context);
        cairo_restore(context);
    }

    cairo_set_operator(context, CAIRO_OPERATOR_OVER);
    psa->pict_empty = FALSE;

    return;
}


/*
 * Set the current dash pattern depending on the line type.
 */
void CROset_dashpattern(CROddp *psa) {
    float nominal_dash_size = 1., dash_size, dot_size, gap_size;
    double *dashes = (double *) NULL;

    trace("Got to CRset_dashpattern");
    
    cairo_t* context = getContext(psa->wks_id);

    dash_size = 6. * nominal_dash_size;
    dot_size = 1. * nominal_dash_size;
    gap_size = 4. * nominal_dash_size;
    switch (psa->attributes.linetype) {
    case SOLID_LINE:
        cairo_set_dash(context, dashes, 0, 0.);
        break;
    case DASHED_LINE:
        dashes = (double *) calloc(2, sizeof(double));
        *dashes = (double) dash_size;
        *(dashes + 1) = (double) gap_size;
        cairo_set_dash(context, dashes, 2, 0.);
        break;
    case DOTTED_LINE:
        dashes = (double *) calloc(1, sizeof(double));
        *dashes = (double) dot_size;
        cairo_set_dash(context, dashes, 1, 0.);
        break;
    case DASH_DOT_LINE:
        dashes = (double *) calloc(4, sizeof(double));
        *dashes = (double) dash_size;
        *(dashes + 1) = (double) gap_size;
        *(dashes + 2) = (double) dot_size;
        *(dashes + 3) = (double) gap_size;
        cairo_set_dash(context, dashes, 4, 0.);
        break;
    case DASH_DOT_DOT_LINE:
        dashes = (double *) calloc(3, sizeof(double));
        *dashes = (double) dash_size;
        *(dashes + 1) = (double) gap_size;
        *(dashes + 2) = (double) dot_size;
        *(dashes + 3) = (double) gap_size;
        *(dashes + 4) = (double) dot_size;
        *(dashes + 5) = (double) gap_size;
        cairo_set_dash(context, dashes, 6, 0.);
        break;
    default:
        cairo_set_dash(context, dashes, 0, 0.);
        break;
    }
    free(dashes);
}


/*
 *  Return current clipping rectangle (in user coordinates).
 */
CROClipRect GetCROClipping(CROddp *psa) {
    static CROClipRect rect;

    double x1, y1, x2, y2;
    cairo_clip_extents(getContext(psa->wks_id), &x1, &y1,
            &x2, &y2);
    rect.llx = x1;
    rect.urx = x2;
    rect.lly = y2;
    rect.ury = y1;
    return (rect);
}


int cro_ActivateWorkstation(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    trace("Got to cro_ActivateWorkstation");

    if (psa->wks_type == CX11) {
      croActivateX11(psa, getSurface(psa->wks_id));
      setSurfaceTransform(psa);
    }
#ifdef BuildQtEnabled
    else if (psa->wks_type == CQT) {
    /*
     *fprintf(stderr, "\nfile %s, line: %d, function: %s\n",
     *                 __FILE__, __LINE__, __PRETTY_FUNCTION__);
     */
      croActivateQt(psa);
      setSurfaceTransform(psa);
    }
#endif

    return (0);
}


/*
 *  Set up indexing for cell arrays.  This interpolates between
 *  the output image size and the cell array.
 *
 *  On entry:
 *    image_height    : height of image in pixels
 *    image_width     : width of image in pixels
 *    *rows           : Has dimension ny
 *    *cols           : Has dimension nx
 *    nx              : number of cells per row
 *    ny              : number of cells per column
 *  On exit:
 *    *rows           : row[i] specifies number of pixels for a cell in row i
 *    *cols           : col[i] specifies number of pixels for a cell in col i
 */
static void get_cell_pixel_multiples(int image_width, int image_height,
        int *rows, int *cols, int nx, int ny) {
    int i;
    int left, right;
    double inc, tmp;

    /*
     * map cell array onto available pixels. Use current IEEE
     * rounding rules to determine whether a cell boundary includes
     * a boundary pixel or not. rows[i] and cols[j] contain
     * the number of pixels that make up cell[i,j]
     */
    inc = (double) image_width / (double) nx;
    for (right = 0, tmp = 0.0, i = 0; i < nx; i++) { /* map cols*/
        left = right;
        tmp += inc;
        right = (int) RINT(tmp);
        cols[i] = right - left;
    }

    inc = (double) image_height / (double) ny;
    for (right = 0, tmp = 0.0, i = 0; i < ny; i++) { /* map rows*/
        left = right;
        tmp += inc;
        right = (int) RINT(tmp);
        rows[i] = right - left;
    }
}


void get_cell_index_limits(GKSC *gksc, CROPoint P, CROPoint Q, int nx, int ny,
        int *j_start, int *j_end, int *j_inc, int *i_start, int *i_end,
        int *i_inc, double *x_offset, double *y_offset) {

    CROddp *psa = (CROddp *) gksc->ddp;

    if ((P.x < Q.x) && (P.y < Q.y)) { /* P lower left, Q upper right. */
        *x_offset = (double) (P.x * psa->dspace.xspan);
        *y_offset = (double) (P.y * psa->dspace.yspan);
        *j_start = 0;
        *j_end = ny;
        *j_inc = 1;
        *i_start = 0;
        *i_end = nx;
        *i_inc = 1;
    } else if ((P.x < Q.x) && (P.y > Q.y)) { /* P upper left, Q lower right. */
        *x_offset = (double) (P.x * psa->dspace.xspan);
        *y_offset = (double) (Q.y * psa->dspace.yspan);
        *j_start = ny - 1;
        *j_end = -1;
        *j_inc = -1;
        *i_start = 0;
        *i_end = nx;
        *i_inc = 1;
    } else if ((Q.x < P.x) && (Q.y > P.y)) { /* Q upper left, P lower right. */
        *x_offset = (double) (Q.x * psa->dspace.xspan);
        *y_offset = (double) (P.y * psa->dspace.yspan);
        *j_start = 0;
        *j_end = ny;
        *j_inc = 1;
        *i_start = nx - 1;
        *i_end = -1;
        *i_inc = -1;
    } else if ((Q.x < P.x) && (Q.y < P.y)) { /* Q lower left, P upper right. */
        *x_offset = (double) (Q.x * psa->dspace.xspan);
        *y_offset = (double) (Q.y * psa->dspace.yspan);
        *j_start = ny - 1;
        *j_end = -1;
        *j_inc = -1;
        *i_start = nx - 1;
        *i_end = -1;
        *i_inc = -1;
    }

}


int cro_Cellarray(GKSC *gksc) {

    int nx, ny;
    int i, j, k, l, image_width, image_height, kount;
    int j_start, j_end, j_inc, i_start, i_end, i_inc;
    int *rows, *cols;
    unsigned int *iar;
    unsigned char *data;
    cairo_surface_t *cell_image;

    CROPoint P, Q;

    CROddp *psa = (CROddp *) gksc->ddp;
    CROPoint *corners = (CROPoint *) gksc->p.list;

    int *iptr = (int *) gksc->i.list;
    int *colia = (int *) gksc->x.list;

    double x_offset, y_offset;

    cairo_pattern_t *pattern;

    trace("Got to cro_Cellarray");
    
    cairo_t* context = getContext(psa->wks_id);

    /*
     *  Save current color.
     */
    pattern = cairo_get_source(context);
    cairo_pattern_reference(pattern);

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    nx = iptr[0];
    ny = iptr[1];
    P = corners[0];
    Q = corners[1];

    image_width = RINT(fabs(Q.x - P.x)*(psa->dspace.xspan));
    image_height = RINT(fabs(Q.y - P.y)*(psa->dspace.yspan));
    rows = (int *) calloc(ny, sizeof(int));
    cols = (int *) calloc(nx, sizeof(int));

    iar = (unsigned int *) calloc(image_width * image_height,
            sizeof(unsigned int));
    get_cell_pixel_multiples(image_width, image_height, rows, cols, nx, ny);
    get_cell_index_limits(gksc, P, Q, nx, ny, &j_start, &j_end, &j_inc,
            &i_start, &i_end, &i_inc, &x_offset, &y_offset);
    kount = 0;
    for (j = j_start; j != j_end; j = j + j_inc) {
        for (l = 0; l < rows[j]; l++) {
            for (i = i_start; i != i_end; i = i + i_inc) {
                for (k = 0; k < cols[i]; k++) {
                    unsigned int color = colia[j * nx + i];
                    if ((color & ARGB_MASK) > 0) {
                        struct color_value cval = unpack_argb(NULL, color);
                        /* cairo image_surface requires pre-multiplied alpha values! */
                        cval.alpha = ALPHA_BLEND(cval.alpha, psa->attributes.fill_alpha);
                        cval.red   = ALPHA_BLEND(cval.red,   psa->attributes.fill_alpha);
                        cval.green = ALPHA_BLEND(cval.green, psa->attributes.fill_alpha);
                        cval.blue  = ALPHA_BLEND(cval.blue,  psa->attributes.fill_alpha);
                        iar[kount] = pack_argb(cval);
                    }
                    else
                        iar[kount] = (psa->ctable)[color];
                    kount++;
                }
            }
        }
    }

    data = (unsigned char *) iar;
    cell_image = cairo_image_surface_create_for_data(data, CAIRO_FORMAT_ARGB32,
            image_width, image_height, 4* image_width );

    cairo_set_source_surface(context, cell_image, x_offset, y_offset);
    cairo_paint(context);

    /* free up resources need to create the cairo_surface... */
    cairo_surface_finish(cell_image);
    cairo_surface_destroy(cell_image);
    free(data);
    free(rows);
    free(cols);

    /*
     *  Restore color.
     */
  /*
   *if(CQT == psa->wks_type)
   *    cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, 0.0);
   *else
   *    cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, cval.alpha);
   */
    cairo_set_source(context, pattern);
    cairo_pattern_destroy(pattern);

    return (0);
}


int cro_ClearWorkstation(GKSC *gksc) {
    int ret = 0;
    char* outputFile;

    trace("Got to cro_ClearWorkstation");

    CROddp *psa;
    psa = (CROddp *) gksc->ddp;

    cairo_t* context = getContext(psa->wks_id);
    
    cairo_stroke(context);
    cairo_show_page(context);

    if (psa->wks_type == CPS || psa->wks_type == CPDF || psa->wks_type == CEPS) {
        cairo_surface_flush(getSurface(psa->wks_id));
    }
    else if (psa->wks_type == CPNG) {
        outputFile = getIndexedOutputFilename(psa->wks_id, psa->output_file, psa->frame_count,
            "NCARG_GKS_CPNGOUTPUT", ".png");
        ret = cairo_surface_write_to_png(getSurface(psa->wks_id), outputFile);
        if (ret != 0)
            ret = ERR_CRO_OPN;
        psa->frame_count++;
        free(outputFile);
    } 
#ifdef __JIRA1530__ 
    else if (psa->wks_type == CX11) {
        cairo_push_group(context);
    }
#endif

    else if (psa->wks_type == CTIFF) {
        cairo_surface_flush(getSurface(psa->wks_id));
        if (psa->useTiffCompression)
            ret = crotiff_writeImageCompressed(psa->tiffClosure, psa->georefData, getSurface(psa->wks_id));
        else
            ret = crotiff_writeImage(psa->tiffClosure, psa->georefData, getSurface(psa->wks_id));

        /* our policy is to clear any georef information now so as to not have stale or 
         * invalid info for any subsequent plots.
         */
        if (psa->georefData)
            free(psa->georefData);
    }

    else if (psa->wks_type == CSVG) {
        /* At this point, we know we need to write the SVG file; create an appropriately named SVG file/surface,
         * and replay the recording surface into it. As there seems to be no way to clear a recording surface, 
         * delete it and create a new one for the next frame (if there is one -- we have no way of knowing in 
         * advance).
         */
        outputFile = getIndexedOutputFilename(psa->wks_id, psa->output_file, psa->frame_count,
            "", ".svg");
        cairo_surface_t *surface = cairo_svg_surface_create(outputFile, psa->paper_width, psa->paper_height);
        if (surface == NULL)
            return (ERR_NO_DISPLAY);
        cairo_t* svgContext = cairo_create(surface);
        cairo_set_source_surface (svgContext, getSurface(psa->wks_id), 0.0, 0.0);
        cairo_paint(svgContext);
        cairo_destroy(svgContext);
        cairo_surface_destroy(surface);
        
        /* reset the recording surface... */
        cairo_surface_destroy(getSurface(psa->wks_id));
        removeCairoEnv(psa->wks_id);
        cairo_surface_t* recSurface = cairo_recording_surface_create(CAIRO_CONTENT_COLOR_ALPHA, NULL);
        cairo_t* recContext = cairo_create(recSurface);
        saveCairoEnv(psa->wks_id, recContext, recSurface);        
        CROInitCairoContext(psa, recContext, recSurface);
        
        psa->frame_count++;
        free(outputFile);
    }


#ifdef BuildQtEnabled
    else if (psa->wks_type == CQT)
    {
      /*
       *fprintf(stderr, "\nfile %s, line: %d, function: %s\n",
       *                 __FILE__, __LINE__, __PRETTY_FUNCTION__);
       *fprintf(stderr, "\tWrite image.\n\n");
       */

        psa->image_width  = qt_screen_width;
        psa->image_height = qt_screen_height;

        ret = cairo_qt_surface_get_image(getSurface(psa->wks_id));
        if (ret != 0)
            ret = ERR_CRO_OPN;

      /*
       */
        psa->frame_count++;
    }
#endif

    psa->pict_empty = TRUE;

    return ret;
}


int cro_CloseWorkstation(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;

    trace("Got to cro_CloseWorkstation");

    psa->pict_empty = TRUE;
    if (psa->output_file)
        free(psa->output_file);

    if (psa->max_color > 0)
        free(psa->ctable);

    if (psa->wks_type == CX11) {
        croFreeNativeSurface(getSurface(psa->wks_id));
    } else if (psa->wks_type == CTIFF) {
        crotiff_closeFile(psa->tiffClosure);
    }
    
#ifdef BuildQtEnabled
    else if (psa->wks_type == CQT) {
      /*
       *fprintf(stderr, "\nfile %s, line: %d, function: %s\n",
       *                 __FILE__, __LINE__, __PRETTY_FUNCTION__);
       *fprintf(stderr, "\tWrite image to qt-screen.\n\n");
       */
      /*Do not free surface, as this surface is not allocated (defined) in NCL.
       *croFreeNativeSurface(getSurface(psa->wks_id));
       */
    }
#endif

    cairo_destroy(getContext(psa->wks_id));
    removeCairoEnv(psa->wks_id);
    free(psa);
    
    return (0);
}


int cro_DeactivateWorkstation(GKSC *gksc) {

    trace("Got to cro_DeactivateWorkstation");

    return (0);
}

int cro_Esc(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    char *sptr = (char *) gksc->s.list, *strng;
    int *iptr = (int *) gksc->i.list;
    _NGCesc *cesc = (_NGCesc*) gksc->native;

    int escape_id = iptr[0], plflag;
    float rscale;

    trace("Got to cro_Esc");

    switch (escape_id) {
        case -1521: /* Corner points for positioning plot on the page */
            /** RLB - Not clear what this should be.  Its currently never set anywhere.
             rscale = 1./psa->scaling;  **/
            rscale = 1.0;
            strng = strtok(sptr, " ");
            psa->dspace.llx = (int) (rscale * (float) atoi(strng));
            strng = strtok((char *) NULL, " ");
            psa->dspace.lly = (int) (rscale * (float) atoi(strng));
            strng = strtok((char *) NULL, " ");
            psa->dspace.urx = (int) (rscale * (float) atoi(strng));
            strng = strtok((char *) NULL, " ");
            psa->dspace.ury = (int) (rscale * (float) atoi(strng));

            psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
            psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

            psa->cro_clip.llx = psa->dspace.llx;
            psa->cro_clip.lly = psa->dspace.lly;
            psa->cro_clip.urx = psa->dspace.urx;
            psa->cro_clip.ury = psa->dspace.ury;
            psa->cro_clip.null = FALSE;

            setSurfaceTransform(psa);
            break;

        case -1525: /* Specify portrait/landscape mode */
            strng = strtok(sptr, " ");
            strng = strtok((char *) NULL, " ");
            plflag = (int) atoi(strng);
            if (plflag == 0) {
                psa->orientation = PORTRAIT;
            } else {
                psa->orientation = LANDSCAPE;
            }

            setSurfaceTransform(psa);
            break;

        case -1529: /* paper width, in points */
            strng = strtok(sptr, " ");
            psa->paper_width = atoi(strng);
            break;

        case -1530: /* paper height, in points */
            strng = strtok(sptr, " ");
            psa->paper_height = atoi(strng);
            break;

        case ESCAPE_PAUSE:
            if (psa->wks_type == CX11) {
#ifdef __JIRA1530__ 
                cairo_pop_group_to_source(getContext(psa->wks_id));
                cairo_paint(getContext(psa->wks_id));
#endif
                croX11Pause(getSurface(psa->wks_id));
            }
            break;

        case NGESC_CNATIVE: /* C-escape mechanism */
            cro_CEsc(psa, cesc);
    }

    return (0);
}

/* Handles the so-called C-Escapes, or native-escape (escapes outside the defined GKS escapes) */
static int cro_CEsc(CROddp *psa, _NGCesc *cesc) {
    _NGCPixConfig *pixconf;
    _NGCAlpha     *alphaConfig;
    _NGCAntiAlias *aliasConfig;
    _NGCCairoFillHack *fillMode;
    _NGCGeoReference *georef;
    
    switch (cesc->type) {
        case NGC_PIXCONFIG: /* get resolution of image-based output formats */
            pixconf = (_NGCPixConfig*) cesc;
            psa->image_width = pixconf->width;
            psa->image_height = pixconf->height;
            break;
        case NGC_SETALPHA: /* set various alpha properties */
            alphaConfig = (_NGCAlpha*) cesc;
            switch (alphaConfig->graphicAttrib) {
                case NGC_LINEALPHA:
                    psa->attributes.line_alpha = alphaConfig->alpha;
                    break;
                case NGC_FILLALPHA:
                    psa->attributes.fill_alpha = alphaConfig->alpha;
                    break;
                case NGC_MARKERALPHA:
                    psa->attributes.marker_alpha = alphaConfig->alpha;
                    break;
                case NGC_TEXTALPHA:
                    psa->attributes.text_alpha = alphaConfig->alpha;
                    break;
                case NGC_BACKGROUNDALPHA:
                    psa->background_alpha = alphaConfig->alpha;
                    break;
            }
            break;
        case NGC_GETALPHA: /* get various alpha properties */
            alphaConfig = (_NGCAlpha*) cesc;
            switch (alphaConfig->graphicAttrib) {
                case NGC_LINEALPHA:
                    alphaConfig->alpha = psa->attributes.line_alpha;
                    break;
                case NGC_FILLALPHA:
                    alphaConfig->alpha = psa->attributes.fill_alpha;
                    break;
                case NGC_MARKERALPHA:
                    alphaConfig->alpha = psa->attributes.marker_alpha;
                    break;
                case NGC_TEXTALPHA:
                    alphaConfig->alpha = psa->attributes.text_alpha;
                    break;
                case NGC_BACKGROUNDALPHA:
                    alphaConfig->alpha = psa->background_alpha;
                    break;
            }
            break;
        case NGC_ANTIALIAS: /* settings for cairo's antialiasing */
            aliasConfig = (_NGCAntiAlias*) cesc;
            cairo_set_antialias(getContext(psa->wks_id), (aliasConfig->antialias_boolean) ?
                CAIRO_ANTIALIAS_DEFAULT :
                CAIRO_ANTIALIAS_NONE);
            break;
        case NGC_CAIROFILLHACK: /* cairo fill-hack: see Jira ncl-1913  */
            fillMode = (_NGCCairoFillHack*) cesc;
            psa->cairo_fill_hack = fillMode->fill_mode_boolean;
            break;
            
        case NGC_GEOREFERENCE:  /* receive georeferencing info... */
            georef = (_NGCGeoReference*) cesc;
            if (!psa->georefData) {
                psa->georefData = (TiffGeoReference*) calloc(sizeof(TiffGeoReference), 1);
                if (psa->georefData == NULL) {
                    ESprintf(ERR_CRO_MEMORY, "CRO: TiffGeoReference malloc(%d)", sizeof(TiffGeoReference));
                    return (ERR_CRO_MEMORY);
                }
            }
            
            psa->georefData->projCode = georef->projCode;
            psa->georefData->meridian = georef->meridianOrDist;
            psa->georefData->stdPar1 = georef->parOrAngle1;
            psa->georefData->stdPar2 = georef->parOrAngle2;                                   
            int i;
            for (i=0; i<4; i++) {
                psa->georefData->worldX[i] = georef->worldX[i];
                psa->georefData->worldY[i] = georef->worldY[i];
                psa->georefData->ndcX[i] = georef->ndcX[i];
                psa->georefData->ndcY[i] = georef->ndcY[i];
            }
            
            break;
    }
 
    return(0);
}



int cro_FillArea(GKSC *gksc) {
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;
    int npoints = gksc->p.num, i;

    float clwidth;
    struct color_value cval;

    trace("Got to cro_FillArea");

    cairo_t* context = getContext(psa->wks_id);
    
    clwidth = (float) cairo_get_line_width(context);

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    cairo_set_line_width(context, 1.1 * (psa->dspace.yspan) * (psa->sfill_spacing));

    cval = unpack_argb(psa->ctable, psa->attributes.fill_colr_ind);
    cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, 
            ALPHA_BLEND(cval.alpha, psa->attributes.fill_alpha));

    switch (psa->attributes.fill_int_style) {
    case HOLLOW_FILL: /* Put out polyline */
        cairo_move_to(context, 
                pptr[0].x * (float) psa->dspace.xspan, 
                pptr[0].y * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(context, 
                    pptr[i].x * (float) psa->dspace.xspan, 
                    pptr[i].y * (float) psa->dspace.yspan);
        }
        cairo_line_to(context, 
                pptr[0].x * (float) psa->dspace.xspan, 
                pptr[0].y * (float) psa->dspace.yspan);
        cairo_stroke(context);
        break;
    case SOLID_FILL:
    case SOLID_TEXT_FILL:
        cairo_move_to(context, 
                pptr[0].x * (double) psa->dspace.xspan, 
                pptr[0].y * (double) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(context, 
                    pptr[i].x * (double) psa->dspace.xspan, 
                    pptr[i].y * (double) psa->dspace.yspan);
        }
        cairo_line_to(context, 
                pptr[0].x * (double) psa->dspace.xspan, 
                pptr[0].y * (double) psa->dspace.yspan);
        
        /* Intended to address Jira 1593:  Previously we just performed a fill on the path.
         * Now, we perform a preserving-fill followed by a stroke of the path.
         * 
         * Update 3/14/14:  The saga continues!  What is now known is that the "thin white lines"
         * problem happens along long, straight bordering filled regions. It happens no matter what
         * for the document backends (pdf, ps, svg);  it occurs in the image backends as well, but
         * disappears if anti-aliasing is turned off.  A simple cairo program was put together to 
         * demonstrate the problem and filed as bug #76090 with the CairoGraphics people.
         *
         * The original strategy of performing a fill, followed by a stroke clipped to the path, has proven 
         * problematic:  it causes much larger ps/pdf/svg output files, can be extremely slow for image backends, 
         * and is suspected in some mysterious SEGVs occurring deep in the cairo pipeline, related to clipping.
         * 
         * The solution now is to:
         * - For image types, we'll turn antialiasing off for filled regions, but otherwise preserve the setting as 
         *   applied to text and lines.
         *   
         * - For document types, there are certain known plots that wreak havoc upon their viewers.  By default the 
         *   fill-clip-stroke drawing strategy is disabled;  we'll perform a straightforward fill operation, which is 
         *   safe but prone to white-lines. However, for certain users and in cases that the white lines are unacceptable,
         *   we've implemented an undocumented resource -- wkCairoFillWorkaround -- that if set, will invoke the 
         *   fill-clip-stroke drawing method. 
         * 
         * Hopefully, someday soon, a fix to cairo will eliminate all this nonsense!
         * 
         * This addresses a slew of Jira tickets: 1593, 1667, 1860, 1896, 1913
         * R. Brownrigg
         * 
         */

        if (psa->attributes.fill_int_style == SOLID_TEXT_FILL) { 
            cairo_fill(context); 
        }
        else if (psa->is_vector_type == TRUE) {   /* pdf, ps, svg, etc. */
            if (psa->cairo_fill_hack == FALSE || cval.alpha < 1.0 || psa->attributes.fill_alpha < 1.0) {
                cairo_fill(context);
            } else {
	      cairo_save(context);
	      cairo_clip_preserve(context);
	      cairo_set_line_width(context, 2.0);
	      cairo_fill_preserve(context);
	      cairo_stroke(context);
	      cairo_restore(context);
	    }
        }
        else {  /* image backends */
            cairo_antialias_t saveAlias = cairo_get_antialias(context);
            cairo_set_antialias(context, CAIRO_ANTIALIAS_NONE);
            cairo_fill(context);
	    cairo_set_antialias(context, saveAlias);
        }
        break;
    case PATTERN_FILL: /* currently not implemented, issue polyline */
        cairo_move_to(context, 
                pptr[0].x * (float) psa->dspace.xspan, 
                pptr[0].y * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(context, 
                    pptr[i].x * (float) psa->dspace.xspan, 
                    pptr[i].y * (float) psa->dspace.yspan);
        }
        cairo_stroke(context);
        break;
    case HATCH_FILL:
        switch (psa->attributes.fill_style_ind) {
        case HORIZONTAL_HATCH:
            cro_SoftFill(gksc, 0., psa->hatch_spacing);
            cairo_stroke(context);
            break;
        case VERTICAL_HATCH:
            cro_SoftFill(gksc, 90., psa->hatch_spacing);
            cairo_stroke(context);
            break;
        case POSITIVE_HATCH:
            cro_SoftFill(gksc, 45., psa->hatch_spacing);
            cairo_stroke(context);
            break;
        case NEGATIVE_HATCH:
            cro_SoftFill(gksc, 135., psa->hatch_spacing);
            cairo_stroke(context);
            break;
        case HORIZ_VERT_HATCH:
            cro_SoftFill(gksc, 0., psa->hatch_spacing);
            cro_SoftFill(gksc, 90., psa->hatch_spacing);
            cairo_stroke(context);
            break;
        case POS_NEG_HATCH:
            cro_SoftFill(gksc, 45., psa->hatch_spacing);
            cro_SoftFill(gksc, 135., psa->hatch_spacing);
            cairo_stroke(context);
            break;
        default:
            cairo_move_to(context, 
                    pptr[0].x * (float) psa->dspace.xspan, 
                    pptr[0].y * (float) psa->dspace.yspan);
            for (i = 1; i < npoints; i++) {
                cairo_line_to(context,
                        pptr[i].x * (float) psa->dspace.xspan, 
                        pptr[i].y * (float) psa->dspace.yspan);
            }
            cairo_line_to(context, 
                    pptr[0].x * (float) psa->dspace.xspan, 
                    pptr[0].y * (float) psa->dspace.yspan);
            cairo_stroke(context);
            break;
        }
        break;
    default:
        cairo_move_to(context, 
                pptr[0].x * (float) psa->dspace.xspan, 
                pptr[0].y * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(context, 
                    pptr[i].x * (float) psa->dspace.xspan, 
                    pptr[i].y * (float) psa->dspace.yspan);
        }
        cairo_line_to(context, 
                pptr[0].x * (float) psa->dspace.xspan, 
                pptr[0].y * (float) psa->dspace.yspan);
        cairo_stroke(context);
        break;
    }
    cairo_set_line_width(context, clwidth);
    return (0);
}


int cro_GetColorRepresentation(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;

    int *xptr = (int *) gksc->x.list;
    CROColor *rgbptr = (CROColor *) gksc->rgb.list;
    struct color_value cval;

    int index = xptr[0];

    trace("Got to cro_GetColorRepresentation");

    cval = unpack_argb(psa->ctable, index);

    rgbptr[0].r = cval.red;
    rgbptr[0].g = cval.green;
    rgbptr[0].b = cval.blue;

    return (0);
}

#ifdef BuildQtEnabled
void setCairoQtSurface(cairo_surface_t *surface)
{
    qt_surface = surface;
}

cairo_t *getCairoQtContext()
{
    return qt_context;
}

void setCairoQtWinSize(int width, int height)
{
    qt_screen_width = width;
    qt_screen_height = height;
}
#endif

int cro_OpenWorkstation(GKSC *gksc) {

    char *filenameBasis = (char *) gksc->s.list;
    CROddp *psa;
    int *pint;
    extern int orig_wks_id;

    trace("Got to cro_OpenWorkstation");
    
    /*
     *  Provide the gksc with the device dependent data.
     */
    psa = (CROddp *) malloc(sizeof(CROddp));
    if (psa == (CROddp *) NULL) {
        ESprintf(ERR_CRO_MEMORY, "CRO: malloc(%d)", sizeof(CROddp));
        return (ERR_CRO_MEMORY);
    }
    gksc->ddp = (GKSC_Ptr) psa;

    pint = (int *) (gksc->i.list);
    psa->wks_type = *(pint + 1);
    psa->wks_id = orig_wks_id;
    
    /*
     *  Initialize all transformations as well as the device coordinate
     *  space (store these in the device dependent data).
     */
    TransformSetWindow(&(psa->tsystem), 0.0, 0.0, 1.0, 1.0);
    TransformSetViewport(&(psa->tsystem), 0.0, 0.0, 1.0, 1.0);
    TransformSetNDScreenSpace(&(psa->tsystem), 0.0, 0.0, 1.0, 1.0);
    TransformSetScreenSpace(&(psa->tsystem), 0.0, 0.0, 1.0, 1.0);
    psa->transform = TransformGetTransform(&psa->tsystem);

    CROinit(psa, pint + 2); /* Initialize local data. */

    cairo_t*         context;
    cairo_surface_t* surface;
    if (psa->wks_type == CPS || psa->wks_type == CEPS) {
        /*
         *  Create a Postscript workstation.
         */
        psa->output_file = getRegularOutputFilename(psa->wks_id, filenameBasis, "NCARG_GKS_CPSOUTPUT", 
                (psa->wks_type == CPS) ? ".ps" : ".eps");
        surface = cairo_ps_surface_create(psa->output_file,psa->paper_width, psa->paper_height);
        if (psa->wks_type == CEPS)
            cairo_ps_surface_set_eps(surface, TRUE);
        cairo_ps_surface_set_size(surface, psa->paper_width, psa->paper_height);
        context = cairo_create(surface);
        saveCairoEnv(orig_wks_id, context, surface);
        psa->orientation = PORTRAIT;
        psa->is_vector_type = TRUE;
    }

    else if (psa->wks_type == CPDF) {
        /*
         *  Create a PDF workstation.
         */
        psa->output_file = getRegularOutputFilename(psa->wks_id, filenameBasis, "NCARG_GKS_CPDFOUTPUT", ".pdf");
        surface = cairo_pdf_surface_create(psa->output_file, psa->paper_width, psa->paper_height);
        context = cairo_create(surface);
        saveCairoEnv(orig_wks_id, context, surface);
        psa->orientation = PORTRAIT;
        psa->is_vector_type = TRUE;
    }

    else if (psa->wks_type == CPNG) {
        /*
         *  Create a PNG workstation.
         */
        surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, psa->image_width, psa->image_height);
        context = cairo_create(surface);
        saveCairoEnv(orig_wks_id, context, surface);
        psa->output_file = (char*) malloc(strlen(filenameBasis) + 1);
        strcpy(psa->output_file, filenameBasis);
        psa->is_vector_type = FALSE;
    }

    else if (psa->wks_type == CTIFF) {
        /*
         *  Create a (geo)Tiff workstation.
         */
        surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, psa->image_width, psa->image_height);
        context = cairo_create(surface);
        saveCairoEnv(orig_wks_id, context, surface);        
        char* outputFile = getRegularOutputFilename(psa->wks_id, filenameBasis, "NCARG_GKS_CTIFFOUTPUT", ".tif");
        psa->tiffClosure = crotiff_openFile(outputFile);
        free(outputFile);
        psa->is_vector_type = FALSE;
    }

    else if (psa->wks_type == CX11) {
        surface = croCreateNativeDisplaySurface(psa);
        if (surface == NULL)
            return (ERR_NO_DISPLAY);
        context = cairo_create(surface);
        saveCairoEnv(orig_wks_id, context, surface);
        psa->image_width = cairo_xlib_surface_get_width(surface);
        psa->image_height = cairo_xlib_surface_get_height(surface);
        psa->is_vector_type = FALSE;
    }

    else if (psa->wks_type == CSVG) {
        /*
         * Create a SVG workstation.  
         * Although SVG can support multiple pages per file, virtually no viewer/browser supports such a beast at this time.
         * So we'll create a file per frame/plot, similarly to PNGs or TIFFs. This gets messy, because when the 
         * cairo SVG-surface is created, it creates an initially empty file. When cro_ClearWorkstation
         * is later called, the file should written and a new surface created in anticipation of a subsequent 
         * frame/plot. But we have no way of knowing in advance that there will be other frames/plots, and we don't want
         * to leave an empty and spurious file laying around (nor want to keep track of it a ensure it gets deleted).
         * 
         * So the strategy is to create a recording surface, and in cro_CloseWorkstation, replay the recording 
         * surface into a proper SVG surface. Then we re-create another recording surface for any subsequent frame/plot.
         * Since this is memory-based, the last one will simply get dropped on the floor. This admittedly incurs 
         * overhead, but it seems to be acceptable.
         */
        psa->output_file = getIndexedOutputFilename(psa->wks_id, filenameBasis, psa->frame_count, "", ".svg");
        surface = cairo_recording_surface_create(CAIRO_CONTENT_COLOR_ALPHA, NULL);
        if (surface == NULL)
            return (ERR_NO_DISPLAY);
        context = cairo_create(surface);
        saveCairoEnv(orig_wks_id, context, surface);
        psa->is_vector_type = TRUE;
    }
    
#ifdef BuildQtEnabled
    else if (psa->wks_type == CQT)
    {
        double width  = (double) qt_screen_width;
        double height = (double) qt_screen_height;

      /*
       *fprintf(stderr, "\nfile %s, line: %d, function: %s\n",
       *                 __FILE__, __LINE__, __PRETTY_FUNCTION__);
       *fprintf(stderr, "\tpsa->image_width = %d, psa->image_height = %d\n",
       *                 (int) psa->image_width, (int) psa->image_height);
       *fprintf(stderr, "\tWe should create surface with QPainter.\n");
       *fprintf(stderr, "\tBut as link to Qt could be pain, we just use other function to create a surface.\n\n");
       */

        if(NULL != qt_surface)
            surface = qt_surface;
        else
            surface = cairo_image_surface_create(CAIRO_FORMAT_ARGB32, psa->image_width, psa->image_height);

        context = cairo_create(surface);
        cairo_scale(context, width, height);
        saveCairoEnv(orig_wks_id, context, surface);

        qt_context = context;

        cairo_save(context);
        cairo_set_source_rgba(context, 1.0, 1.0, 1.0, 1.0);
        cairo_set_operator(context, CAIRO_OPERATOR_SOURCE);
        cairo_paint(context);
        cairo_restore(context);

        psa->image_height = qt_screen_height;
        psa->image_width  = qt_screen_width;
        psa->is_vector_type = FALSE;
    }
#endif

    CROInitCairoContext(psa, context, surface);

    return (0);
}

/*
 * CROInitCairoContext()
 * 
 * This originally appeared in cro_OpenWorkstation();  was refactored into this static method when SVG support was
 * added because we have to create a new SVG surface for each plot-frame that gets created.
 * 
 */
void CROInitCairoContext(CROddp* psa, cairo_t* context, cairo_surface_t* surface) {
    /*
     *  Set fill rule to even/odd.
     */
    cairo_set_fill_rule(context, CAIRO_FILL_RULE_EVEN_ODD);

    /*
     *  Set default line cap and line join to round.
     */
    cairo_set_line_cap(context, CAIRO_LINE_CAP_ROUND);
    cairo_set_line_join(context, CAIRO_LINE_JOIN_ROUND);

    if(CQT != psa->wks_type)
        cairo_surface_destroy(surface);

    /*
     *  Set the default linewidth.
     */
    cairo_set_line_width(context, psa->attributes.linewidth);

    /*
     *  Set clipping rectangle to max.
     */
    cairo_new_path(context);
    cairo_move_to(context, psa->dspace.llx, psa->dspace.lly);
    cairo_line_to(context, psa->dspace.llx + psa->dspace.xspan, psa->dspace.lly);
    cairo_line_to(context, psa->dspace.llx + psa->dspace.xspan, psa->dspace.lly + psa->dspace.yspan);
    cairo_line_to(context, psa->dspace.llx, psa->dspace.lly + psa->dspace.yspan);
    cairo_line_to(context, psa->dspace.llx, psa->dspace.lly);
    cairo_clip(context);

    setSurfaceTransform(psa);

    /*
     *  Initialize color table for this workstation.
     */
    psa->ctable = (unsigned int *) calloc(2, sizeof(unsigned int));
    psa->max_color = 2;

    /*
     *  Define the default foreground (black) and background (white)
     *  colors and draw the background.
     *& Select the foreground color.
     */
#ifdef BuildQtEnabled
    if(CQT == psa->wks_type)
    {
       /*Qt & OpenGL*/
        (psa->ctable)[0] = 0xFFFFFF00;
        (psa->ctable)[1] = 0xFF000000;
        cairo_set_source_rgba(context, 1., 1., 1., 0.);
    }
    else
#endif
    {
       /*Others*/
        (psa->ctable)[0] = 0xFFFFFFFF;
        (psa->ctable)[1] = 0xFF000000;
        cairo_set_source_rgba(context, 0., 0., 0., 1.);
    }

#ifdef __JIRA1530__   
        /* Jira NCL-1530:  this is a work-around between a buggy interaction with XQuartz 2.7.x
         * and cairo. We do this last to preserve the graphics-state set in the previous
         * lines.    9/20/2012  RLB
         */
    if (psa->wks_type == CX11) {
        cairo_push_group(context);
    }
#endif
    
}

int cro_Polyline(GKSC *gksc) {
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;

    int npoints = gksc->p.num, i, ier, ltype;
    struct color_value cval;

    trace("Got to cro_Polyline");

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    cairo_t* context = getContext(psa->wks_id);
    /*
     *  Set the dash pattern based on the line type.
     */
    cval = unpack_argb(psa->ctable, psa->attributes.line_colr_ind);
    cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, 
            ALPHA_BLEND(cval.alpha, psa->attributes.line_alpha));

    cairo_set_line_width(context, (psa->nominal_width_scale) * (psa->attributes.linewidth));
    cairo_new_sub_path(context);

    /*
     *  Use butt ends if not solid line.
     */
    ginq_linetype(&ier, &ltype);
    if (ltype != 1) {
        cairo_set_line_cap(context, CAIRO_LINE_CAP_BUTT);
    } else {
        cairo_set_line_cap(context, CAIRO_LINE_CAP_ROUND);
    }
    CROset_dashpattern(psa);

    cairo_move_to(context, pptr[0].x * (float) psa->dspace.xspan, pptr[0].y * (float) psa->dspace.yspan);
    for (i = 1; i < npoints; i++) {
        cairo_line_to(context, 
                pptr[i].x * (float) psa->dspace.xspan, 
                pptr[i].y * (float) psa->dspace.yspan);
    }
    cairo_stroke(context);

    /*
     *  Set line cap back to round in case it was changed.
     */
    cairo_set_line_cap(context, CAIRO_LINE_CAP_ROUND);

    return (0);
}


int cro_Polymarker(GKSC *gksc) {
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;
    int npoints = gksc->p.num, i;

    int marker_type;
    float marker_size, orig_line_width, xc, yc, mscale;
    struct color_value cval;
    cairo_line_cap_t orig_cap_type;

    trace("Got to cro_Polymarker");

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }
    
    cairo_t* context = getContext(psa->wks_id);

    marker_type = psa->attributes.marker_type;
    marker_size = ((psa->transform.y_scale) * (psa->attributes.marker_size));

    cval = unpack_argb(psa->ctable, psa->attributes.marker_colr_ind);
    cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, 
            ALPHA_BLEND(cval.alpha, psa->attributes.marker_alpha));

    /*
     *  Get the current setting for line cap and set it to round so that
     *  a dot will be drawn in the degenerate case.  Do the same for
     *  linewidth.
     */
    orig_cap_type = cairo_get_line_cap(context);
    cairo_set_line_cap(context, CAIRO_LINE_CAP_ROUND);
    orig_line_width = cairo_get_line_width(context);
    cairo_set_line_width(context, 1.0);

    switch (marker_type) {
    case DOT_MARKER:

        /*
         *  Dot markers cannot be scaled.
         */
        cairo_set_line_cap(context, CAIRO_LINE_CAP_ROUND);
        cairo_set_line_width(context, 0.5);
        for (i = 0; i < npoints; i++) {
#ifndef JIRA_494
            cairo_move_to(context, 
                    pptr[i].x * (float) psa->dspace.xspan, 
                    pptr[i].y * (float) psa->dspace.yspan);
            cairo_line_to(context, 
                    pptr[i].x * (float) psa->dspace.xspan, 
                    pptr[i].y * (float) psa->dspace.yspan);
#else
            cairo_arc(context, 
                    pptr[i].x * (float) psa->dspace.xspan, 
                    pptr[i].y * (float) psa->dspace.yspan, 0.25, 0., 6.2831853);
#endif
            cairo_stroke(context);
        }
        cairo_set_line_width(context, orig_line_width);
        break;
    case PLUS_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75;
            cairo_move_to(context, xc, yc - mscale * marker_size);
            cairo_line_to(context, xc, yc + mscale * marker_size);

            cairo_stroke(context);
            cairo_move_to(context, xc - mscale * marker_size, yc);
            cairo_line_to(context, xc + mscale * marker_size, yc);
            cairo_stroke(context);
        }
        break;
    case STAR_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75 * marker_size;

            cairo_move_to(context, xc, yc - mscale);
            cairo_line_to(context, xc, yc + mscale);
            cairo_stroke(context);

            cairo_move_to(context, xc - 0.866 * mscale, yc - 0.5 * mscale);
            cairo_line_to(context, xc + 0.866 * mscale, yc + 0.5 * mscale);
            cairo_stroke(context);

            cairo_move_to(context, xc - 0.866 * mscale, yc + 0.5 * mscale);
            cairo_line_to(context, xc + 0.866 * mscale, yc - 0.5 * mscale);
            cairo_stroke(context);

        }
        break;
    case CIRCLE_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75;
            cairo_move_to(context, xc, yc);
            cairo_new_sub_path(context);
            cairo_arc(context, xc, yc, mscale * marker_size, 0., 2. * M_PI);
            cairo_stroke(context);
        }
        break;
    case X_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 3. * marker_size;

            cairo_move_to(context, xc - 0.707 * mscale, yc - 0.707 * mscale);
            cairo_line_to(context, xc + 0.707 * mscale, yc + 0.707 * mscale);
            cairo_stroke(context);

            cairo_move_to(context, xc - 0.707 * mscale, yc + 0.707 * mscale);
            cairo_line_to(context, xc + 0.707 * mscale, yc - 0.707 * mscale);
            cairo_stroke(context);

        }
        break;
    default:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75 * marker_size;

            cairo_move_to(context, xc, yc - mscale);
            cairo_line_to(context, xc, yc + mscale);
            cairo_stroke(context);

            cairo_move_to(context, xc - 0.866 * mscale, yc - 0.5 * mscale);
            cairo_line_to(context, xc + 0.866 * mscale, yc + 0.5 * mscale);
            cairo_stroke(context);

            cairo_move_to(context, xc - 0.866 * mscale, yc + 0.5 * mscale);
            cairo_line_to(context, xc + 0.866 * mscale, yc - 0.5 * mscale);
            cairo_stroke(context);

        }
        break;
    }

    /*
     *  Restore line cap type and line width.
     */
    cairo_set_line_cap(context, orig_cap_type);
    cairo_set_line_width(context, orig_line_width);
    return (0);
}


int cro_SetCharacterExpansionFactor(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    trace("Got to cro_SetCharacterExpansionFactor");

    psa->attributes.char_expan = fptr[0];
    return (0);
}


int cro_SetCharacterHeightAndUpVector(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    float *fptr = (float *) gksc->f.list;

    double up_x = (double) fptr[0];
    double up_y = (double) fptr[2];
    double base_x = (double) fptr[1];
    double base_y = (double) fptr[3];

    trace("Got to cro_SetCharacterHeightAndUpVector");

    /*
     *  Transform to workstation viewport space.
     */
    up_x *= (psa->transform).x_scale;
    up_y *= (psa->transform).y_scale;
    base_x *= (psa->transform).x_scale;
    base_y *= (psa->transform).y_scale;
    psa->attributes.char_up_vec_x = (float) up_x;
    psa->attributes.char_up_vec_y = (float) up_y;
    psa->attributes.char_base_vec_x = (float) base_x;
    psa->attributes.char_base_vec_y = (float) base_y;
    psa->attributes.char_ht = MAG(up_x,up_y);

    return (0);
}


int cro_SetCharacterSpacing(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    trace("Got to cro_SetCharacterSpacing");

    psa->attributes.char_space = fptr[0];
    return (0);
}


int cro_SetClipIndicator(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *iptr = (int *) gksc->i.list;
    CROPoint *pptr = (CROPoint *) gksc->p.list;

    int clip_flag;
    float pllx, purx, plly, pury;

    trace("Got to cro_SetClipIndicator");
    
    cairo_t* context = getContext(psa->wks_id);

    clip_flag = iptr[0];

    pllx = pptr[0].x;
    plly = pptr[0].y;
    purx = pptr[1].x;
    pury = pptr[1].y;

    psa->attributes.norm_tran = iptr[1]; /* GKS normalization number */

    if (clip_flag) {
        /*
         *  Turn clipping on.
         */
        cairo_new_path(context);
        /*
         *  Set the clip rectangle.
         */
        cairo_reset_clip(context);
        cairo_move_to(context, pllx * psa->dspace.xspan, plly * psa->dspace.yspan);
        cairo_line_to(context, purx * psa->dspace.xspan, plly * psa->dspace.yspan);
        cairo_line_to(context, purx * psa->dspace.xspan, pury * psa->dspace.yspan);
        cairo_line_to(context, pllx * psa->dspace.xspan, pury * psa->dspace.yspan);
        cairo_line_to(context, pllx * psa->dspace.xspan, plly * psa->dspace.yspan);
        cairo_clip(context);

        psa->attributes.clip_ind = 1;
    } else {
        psa->attributes.clip_ind = 0;
        cairo_reset_clip(context);
        cairo_new_path(context);
        cairo_move_to(context, 0, 0);
        cairo_line_to(context, psa->dspace.xspan, 0);
        cairo_line_to(context, psa->dspace.xspan, psa->dspace.yspan);
        cairo_line_to(context, 0, psa->dspace.yspan);
        cairo_line_to(context, 0, 0);
        cairo_clip(context);
    }
    return (0);
}


int cro_SetColorRepresentation(GKSC *gksc) {
    int *xptr = (int *) gksc->x.list;
    CROColor *rgbptr = (CROColor *) gksc->rgb.list;
    CROddp *psa = (CROddp *) gksc->ddp;

    unsigned index = (unsigned) xptr[0];
    int i;
    struct color_value cval;

    trace("Got to cro_SetColorRepresentation");

    if ((index & ARGB_MASK) > 0)
    	return 0;   /* value is a 32-bit color, not an index */

    if (psa->max_color < index + 1) {
        psa->ctable = (unsigned int*) realloc(psa->ctable, (index + 1)
                * sizeof(unsigned int));
        /*
         *  Set any color table entries that are between index+1 and psa->max_color
         *  to the foreground color.
         */
        for (i = psa->max_color; i < index; i++) {
            (psa->ctable)[i] = (psa->ctable)[1];
        }
        psa->max_color = index + 1;
    }
    cval.alpha = 1.;
    cval.red = (float) rgbptr[0].r;
    cval.green = (float) rgbptr[0].g;
    cval.blue = (float) rgbptr[0].b;

    (psa->ctable)[index] = pack_argb(cval);

    return (0);
}


int cro_SetFillAreaColorIndex(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    int *xptr = (int *) gksc->x.list;

    trace("Got to cro_SetFillAreaColorIndex");
    
    psa->attributes.fill_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetFillAreaInteriorStyle(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *iptr = (int *) gksc->i.list;

    trace("Got to cro_SetFillAreaInteriorStyle");

    psa->attributes.fill_int_style = iptr[0];

    return (0);
}


int cro_SetFillAreaStyleIndex(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;

    int *iptr = (int *) gksc->i.list;

    trace("Got to cro_SetFillAreaStyleIndex");

    psa->attributes.fill_style_ind = iptr[0];
    return (0);
}


int cro_SetLineWidthScaleFactor(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    trace("Got to cro_SetLineWidthScaleFactor");

    if (fptr[0] <= 0.2) {
        psa->attributes.linewidth = 0.2;
    } else {
        psa->attributes.linewidth = fptr[0];
    }
    return (0);
}


int cro_SetLinetype(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    int *iptr = (int *) gksc->i.list;

    trace("Got to cro_SetLinetype");

    psa->attributes.linetype = iptr[0];
    return (0);
}


int cro_SetMarkerSizeScaleFactor(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    trace("Got to cro_setMarkerSizeScaleFactor");

    psa->attributes.marker_size = fptr[0];
    return (0);
}


int cro_SetMarkerType(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    int *iptr = (int *) gksc->i.list;

    trace("Got to cro_SetMarkerType");

    psa->attributes.marker_type = iptr[0];
    return (0);
}


int cro_SetPolylineColorIndex(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->x.list;

    trace("Got to cro_SetPolylineColorIndex");

    psa->attributes.line_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetPolymarkerColorIndex(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->x.list;

    trace("Got to cro_SetPolymarkerColorIndex");

    psa->attributes.marker_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetTextAlignment(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->i.list;

    trace("Got to cro_SetTextAlignment");

    psa->attributes.text_align_horiz = (unsigned int) xptr[0];
    psa->attributes.text_align_vert = (unsigned int) xptr[1];
    return (0);
}


int cro_SetTextColorIndex(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->x.list;

    trace("Got to cro_SetTextColorIndex");

    psa->attributes.text_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetTextFontAndPrecision(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    static int ifst = 0;

    /*
     *  Extract only the font number since we will use only the
     *  highest precision.
     */
    int *iptr = (int *) gksc->i.list;

    trace("Got to cro_SetTextFontAndPrecision");

    if (ifst == 0) {
        psa->attributes.text_font = 0;
        ifst++;
    } else {
        psa->attributes.text_font = abs(iptr[0]);
    }

    return (0);
}


int cro_SetTextPath(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    int *iptr = (int *) gksc->i.list;

    trace("Got to cro_SetTextPath");

    psa->attributes.text_path = iptr[0];
    return (0);
}


int cro_SetViewport(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    trace("Got to cro_SetViewport");

    /*
     *  If the workstation viewport has changed, update the
     *  transformation.
     */
    if ((psa->tsystem.viewport.llx != fptr[0]) || (psa->tsystem.viewport.urx
            != fptr[1]) || (psa->tsystem.viewport.lly != fptr[2])
            || (psa->tsystem.viewport.ury != fptr[3])) {
        TransformSetViewport(&psa->tsystem, fptr[0], fptr[2], fptr[1], fptr[3]);
        psa->transform = TransformGetTransform(&psa->tsystem);
    }
    return (0);
}


int cro_SetWindow(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    trace("Got to cro_SetWindow");

    if ((fptr[0] >= fptr[1]) || (fptr[2] >= fptr[3])) {
        return (ERR_INV_RECT);
    }

    /*
     *  If the workstation window has changed, update the
     *  transformation.
     */
    if ((psa->tsystem.window.llx != fptr[0]) || (psa->tsystem.window.urx
            != fptr[1]) || (psa->tsystem.window.lly != fptr[2])
            || (psa->tsystem.window.ury != fptr[3])) {
        TransformSetWindow(&psa->tsystem, fptr[0], fptr[2], fptr[1], fptr[3]);
        psa->transform = TransformGetTransform(&psa->tsystem);
    }
    return (0);
}


int cro_Text(GKSC *gksc) {
    /*
     *  Supports the GKS GTX functions.
     */
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;
    char *sptr = (char *) gksc->s.list, *font_path, *font_name, *db_path;
    char single_char[2];
    struct color_value cval;
    float base_mag, tcos, cprod, cang, cspace;
    int slen, error, i, char_num_mx;
    double horiz_len, left_space, right_space, maximum_width = 0.;

    float xc, yc, xpos, ypos, X_height, x_del, y_del;
    static int kount = 0;

    static FT_Library library;
    static FT_Face face;
    cairo_font_face_t *font_face;
    cairo_matrix_t fmatrix, scl_matrix;
    cairo_text_extents_t textents;
    cairo_font_extents_t fextents;
    
    cairo_t* context = getContext(psa->wks_id);

    trace("Got to cro_Text");

    cairo_text_extents(context, sptr, &textents);

    cairo_get_font_matrix(context, &fmatrix);

    cairo_matrix_scale(&fmatrix, 1., -1.);
    cairo_set_font_matrix(context, &fmatrix);
    cairo_get_font_matrix(context, &fmatrix);

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    /*
     *  Input coordinates are in NDC space.
     */
    xc = pptr[0].x;
    yc = pptr[0].y;

    /*
     *  Initialize the Freetype library and font faces (this is
     *  done only once).
     */
    if (kount == 0) {
        error = FT_Init_FreeType(&library);
        if (error) {
            printf("Error initializing FreeType library\n");
            return 1;
        }
        /*
         *  Establish the default font faces.
         */

        /*
         * The following is strictly ad hoc.  Ultimately there should be a
         * table associating font numbers (psa->attributes.text_font) with
         * font names, or some other way to associate such.  Here we are
         * just using Vera.ttf as the single available font.
         *
         * Also, should need to get the font database path only once.
         */
        font_name = "Vera.ttf";
        db_path = (char *) GetNCARGPath("ftfonts"); /* Path for font database */
        font_path = (char *) calloc(strlen(db_path) + strlen("/") + strlen(
                font_name) + 1, sizeof(char));
        strcpy(font_path, db_path);
        strcat(font_path, "/");
        strcat(font_path, font_name);
        error = FT_New_Face(library, font_path, 0, &face);
        if (error == FT_Err_Unknown_File_Format) {
            printf(
                    "The font file could be opened and read, \n but it appears that its font format is unsupported\n");
            return 1;
        }
        free(font_path);
        kount++;
    }

    font_face = cairo_ft_font_face_create_for_ft_face(face, 0);

    cairo_set_font_face(context, font_face);
    cairo_font_extents(context, &fextents);

    /*
     *  Character height.
     */
    cairo_set_font_size(context, (psa->attributes.char_ht * psa->dspace.yspan / .728));
    cairo_get_font_matrix(context, &fmatrix);

    /*
     *  Get height of capital X.
     */
    single_char[0] = 'X';
    single_char[1] = 0;
    cairo_text_extents(context, single_char, &textents);

    X_height = textents.height;
    cairo_matrix_scale(&fmatrix, 1., -1.);
    cairo_set_font_matrix(context, &fmatrix);
    cairo_text_extents(context, sptr, &textents);

    slen = (int) strlen(sptr);

    /*
     *  Character color.
     */
    cval = unpack_argb(psa->ctable, psa->attributes.text_colr_ind);
    cairo_set_source_rgba(context, cval.red, cval.green, cval.blue, 
            ALPHA_BLEND(cval.alpha, psa->attributes.text_alpha));

    /*
     *  Set the horizontal alignment for the "NORMAL" settings.
     */
    if (psa->attributes.text_align_horiz == NORMAL_ALIGNMENT_HORIZ) {
        switch (psa->attributes.text_path) {
        case RIGHT_TEXT_PATH:
            psa->attributes.text_align_horiz = LEFT_ALIGNMENT_HORIZ;
            break;
        case LEFT_TEXT_PATH:
            psa->attributes.text_align_horiz = RIGHT_ALIGNMENT_HORIZ;
            break;
        case DOWN_TEXT_PATH:
            psa->attributes.text_align_horiz = CENTER_ALIGNMENT_HORIZ;
            break;
        case UP_TEXT_PATH:
            psa->attributes.text_align_horiz = CENTER_ALIGNMENT_HORIZ;
            break;
        default:
            psa->attributes.text_align_horiz = LEFT_ALIGNMENT_HORIZ;
            break;
        }
    }
    if (psa->attributes.text_align_vert == NORMAL_ALIGNMENT_VERT) {
        switch (psa->attributes.text_path) {
        case RIGHT_TEXT_PATH:
            psa->attributes.text_align_vert = BASE_ALIGNMENT_VERT;
            break;
        case LEFT_TEXT_PATH:
            psa->attributes.text_align_vert = BASE_ALIGNMENT_VERT;
            break;
        case DOWN_TEXT_PATH:
            psa->attributes.text_align_vert = TOP_ALIGNMENT_VERT;
            break;
        case UP_TEXT_PATH:
            psa->attributes.text_align_vert = BASE_ALIGNMENT_VERT;
            break;
        default:
            psa->attributes.text_align_vert = BASE_ALIGNMENT_VERT;
            break;
        }
    }

    cairo_text_extents(context, sptr, &textents);
    horiz_len = textents.width;
    /*
     *  The text extents returned from the above include white space
     *  at the left of the first character and the right of the last
     *  character.  GKS wants to align at character edges, so we subtract
     *  off the small additional white spaces.
     */
    single_char[0] = *sptr;
    single_char[1] = 0;
    cairo_text_extents(context, single_char, &textents);
    left_space = textents.x_bearing;
    single_char[0] = *(sptr + strlen(sptr) - 1);
    single_char[1] = 0;
    cairo_text_extents(context, single_char, &textents);
    right_space = textents.x_advance - textents.width - textents.x_bearing;

    xpos = xc * psa->dspace.xspan;
    if (psa->attributes.text_align_horiz == LEFT_ALIGNMENT_HORIZ) {
        x_del = -left_space;
    } else if (psa->attributes.text_align_horiz == CENTER_ALIGNMENT_HORIZ) {
        x_del = -0.5 * horiz_len - left_space;
    } else if (psa->attributes.text_align_horiz == RIGHT_ALIGNMENT_HORIZ) {
        x_del = -horiz_len - right_space;
    }

    /*
     *  Rotation angle.
     */
    /*
     *  Find the text angle based on the base vector
     *  and its angle with the vector (1.,0.) using the dot product divided by
     *  the magnitudes to get the arccos.
     */
    base_mag = sqrt(psa->attributes.char_base_vec_x
            * psa->attributes.char_base_vec_x + psa->attributes.char_base_vec_y
            * psa->attributes.char_base_vec_y);
    tcos = psa->attributes.char_base_vec_x / base_mag;
    if (tcos > 1.)
        tcos = 1.;
    if (tcos < -1.)
        tcos = -1.;
    cang = acos(tcos);
    cprod = psa->attributes.char_base_vec_y; /* cross product */
    if (cprod > 0.)
        cang = -cang;
    ypos = yc * psa->dspace.yspan;

    cairo_save(context);
    cairo_move_to(context, xpos, ypos);
    cairo_rotate(context, -cang);

    /*
     *  Vertical alignments (NORMAL has been converted appropriately above).
     */
    switch (psa->attributes.text_align_vert) {
    case TOP_ALIGNMENT_VERT:
        y_del = -1.25 * X_height;
        break;
    case CAP_ALIGNMENT_VERT:
        y_del = -X_height;
        break;
    case HALF_ALIGNMENT_VERT:
        y_del = -0.5 * X_height;
        break;
    case BASE_ALIGNMENT_VERT:
        y_del = 0.;
        break;
    case BOTTOM_ALIGNMENT_VERT:
        y_del = 0.3 * X_height;
        break;
    }

    /*
     *  If the spacing, expansion factor, and path are all default, then
     *  use the full string in sptr; otherwise the characters have to be
     *  plotted one at a time.
     */
    if (psa->attributes.char_space == CHAR_SPACE_DEFAULT
            && psa->attributes.char_expan == CHAR_EXPAN_DEFAULT
            && psa->attributes.text_path == TEXT_PATH_DEFAULT) {
        cairo_rel_move_to(context, x_del, y_del);
        cairo_show_text(context, sptr);
    } else {
        /*
         *  Intercharacter spacing.
         */
        cspace = X_height * (psa->attributes.char_space);
        /*
         *  Effect the expansion factor.
         */
        if (psa->attributes.char_expan != CHAR_EXPAN_DEFAULT) {
            cairo_get_font_matrix(context, &fmatrix);
            scl_matrix = fmatrix;
            cairo_matrix_scale(&scl_matrix, psa->attributes.char_expan, 1.);
            cairo_set_font_matrix(context, &scl_matrix);
        }
        /*
         *  Deal with the non-standard text paths.
         */
        switch (psa->attributes.text_path) {
        case LEFT_TEXT_PATH: /* note fall through to RIGHT_TEXT_PATH */
            reverse_chrs(sptr);
        case RIGHT_TEXT_PATH:
            /*
             *  Only need to handle the character spacings and expansion factors
             *  that make this case different from the default RIGHT_TEXT_PATH.
             */
            cairo_text_extents(context, sptr, &textents);
            if (psa->attributes.text_align_horiz == CENTER_ALIGNMENT_HORIZ) {
                x_del = x_del * psa->attributes.char_expan; /* Scale current pos. */
                x_del = x_del - 0.5 * (slen - 1) * cspace;
            } else if (psa->attributes.text_align_horiz
                    == RIGHT_ALIGNMENT_HORIZ) {
                x_del = x_del * psa->attributes.char_expan;
                x_del = x_del - (slen - 1) * cspace;
            }
            cairo_rel_move_to(context, x_del, y_del);
            for (i = 0; i < strlen(sptr); i++) {
                single_char[0] = *(sptr + i);
                single_char[1] = 0;
                cairo_text_extents(context, single_char, &textents);
                cairo_show_text(context, single_char);
                cairo_rel_move_to(context, cspace, 0.);
            }
            break;
        case UP_TEXT_PATH:
            reverse_chrs(sptr); /* note fall through to DOWN_TEXT_PATH */
        case DOWN_TEXT_PATH:
            /*
             *  Get the text extents for the first character to use for centering.
             */
            cairo_save(context);
            single_char[0] = *(sptr);
            single_char[1] = 0;
            cairo_text_extents(context, single_char, &textents);
            /*
             *  Rotate the string.
             */
            cairo_rotate(context, cang);
            switch (psa->attributes.text_align_vert) {
            /*
             *  Calculate the vertical adjustment (NORMAL has been converted to the
             *  appropriate alignment above).
             */
            case TOP_ALIGNMENT_VERT:
                y_del = -1.5 * X_height;
                break;
            case CAP_ALIGNMENT_VERT:
                y_del = -X_height;
                break;
            case HALF_ALIGNMENT_VERT:
                y_del = (slen - 2) * 0.5 * 1.5 * X_height + 0.5 * (slen - 1)
                        * cspace;
                break;
            case BASE_ALIGNMENT_VERT:
                y_del = 1.5 * X_height * (slen - 1) + (slen - 1) * cspace;
                break;
            case BOTTOM_ALIGNMENT_VERT:
                y_del = (1.5 + 0.07) * X_height * (slen - 1) + (slen - 1)
                        * cspace;
                break;
            }
            /*
             *  Find the maximum width of the characters in the string to use for
             *  horizonal adjustment.
             */
            maximum_width = 0.;
            for (i = 0; i < strlen(sptr); i++) {
                single_char[0] = *(sptr + i);
                single_char[1] = 0;
                cairo_text_extents(context, single_char, &textents);
                if (textents.width > maximum_width) {
                    maximum_width = MAX(maximum_width,textents.width);
                    char_num_mx = i;
                }
            }
            single_char[0] = *(sptr + char_num_mx);
            single_char[1] = 0;
            cairo_text_extents(context, single_char, &textents);

            /*
             *  Translate to the string start point.
             */
            cairo_translate(context, (xc* psa->dspace.xspan), (yc * psa->dspace.yspan));
            cairo_rotate(context, -cang);
            /*
             *  Draw characters one at a time.
             */
            for (i = 0; i < strlen(sptr); i++) {
                single_char[0] = *(sptr + i);
                single_char[1] = 0;
                cairo_text_extents(context, single_char, &textents);

                /*
                 *  Quantities for the horizontal adjustments. ("NORMAL" horizontal
                 *  alignment has been converted to the appropriate alignment above).
                 */
                switch (psa->attributes.text_align_horiz) {
                case LEFT_ALIGNMENT_HORIZ:
                    x_del = 0.5 * maximum_width;
                    break;
                case CENTER_ALIGNMENT_HORIZ:
                    x_del = 0.;
                    break;
                case RIGHT_ALIGNMENT_HORIZ:
                    x_del = -0.5 * maximum_width;
                    break;
                }

                /*
                 *  Draw characters.
                 */
                single_char[0] = *(sptr + i);
                single_char[1] = 0;
                cairo_text_extents(context, single_char, &textents);
                /*
                 *  The quantity:
                 *
                 *          x_del - textents.x_bearing - 0.5*textents.width
                 *
                 *  is the distance that an individual character has to be shifted
                 *  in order for its center to align with the center of the character
                 *  of maximum width.
                 */
                if (i == 0) {
                    cairo_rel_move_to(context,
                            x_del - textents.x_bearing - 0.5 * textents.width,
                            y_del);
                    cairo_show_text(context, single_char);
                    /*
                     *  Move back to the base horizontal position.
                     */
                    cairo_rel_move_to(context, 
                            (0.5 * textents.width + textents.x_bearing - x_del) 
                            - textents.x_advance, 0.);
                } else {
                    cairo_text_extents(context, single_char, &textents);
                    cairo_rel_move_to(context,
                            x_del - textents.x_bearing - 0.5 * textents.width,
                            -1.5 * X_height - cspace);
                    cairo_show_text(context, single_char);
                    cairo_rel_move_to(context, 
                            (0.5 * textents.width + textents.x_bearing - x_del) 
                            - textents.x_advance, 0.);
                }
            }
            cairo_restore(context);
            break;
        }
    }
    cairo_restore(context);

    return (0);
}


int cro_UpdateWorkstation(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    trace("Got to cro_UpdateWorkstation");
    
    cairo_surface_flush(getSurface(psa->wks_id));
    return (0);
}


/*
 *  getFileNameRoot()
 *
 *  Returns a filename root.  NOTE that caller does not own the memory,
 *  and should make a copy of the returned string.
 *
 *  There are three ways that the root name for an output file
 *  can be set:
 *
 *    1. default
 *        In this case the root name will be "cairoX", where "X" is
 *        the integer workstation ID.
 *
 *    2. set by a call to NGSETC('ME','root_name')
 *        Setting the root name in this case will take precedence
 *        over the default setting. Note well that the NGSETC call
 *        must be made before a workstation is opened--it will apply
 *        to the next opened workstation.  Multiple simultaneously
 *        open workstations can be given different names
 *        using this method.
 *
 *    3. set via an environment variable, if the argument "envVar" is
 *        not null and is the name of an environment variable at runtime.
 *        A setting of the environment variable
 *        takes precedence over methods 1.) and 2.) above in
 *        establishing the root name of the output file.
 *        Note well that using an environment variable to specify a filename
 *        only makes sense in the case of having a single output file for
 *        a given workstation type (i.e., postscript, pdf, png, etc.)
 */
const char *getFileNameRoot(int wkid, const char *file_name, const char *envVar) {
    static const int maxName = 1024;
    static char tname[1025];
    static char *tch;
    static const char* defaultName = "DEFAULT";

    /*
     *  Name set by an environment variable
     */
    if (envVar != NULL) {
        tch = getenv(envVar);
        if (tch != NULL)
            return tch;
    }

    /*
     *  Name set by a call to NGMISC.
     */
    if (file_name != NULL && strlen(file_name) > 0 && strncmp(file_name,
            defaultName, strlen(defaultName)) != 0)
    {
        /* the given plotfile name is handed to us as a fixed length buffer, padded on the right with spaces. 
         * Trim those. 
         */
        int i;
        int len = 0;
        for (i=strlen(file_name)-1; i>0; i--) {
            if (file_name[i] != ' ') {
                len = i + 1;
                break;
            }
        }
    
        len = (len > maxName) ? maxName : len;
        strncpy(tname, file_name, len);
        tname[len] = '\0';
        return tname;
    }

    /*
     *  Default
     */
    memset(tname, 0, maxName);
    (void) sprintf(tname, "cairo%d", wkid);
    return (tname);
}

/*
 * getRegularOutputFilename()
 *
 * A utility function to create appropriate filenames for formats such as
 * postscript/PDF, which support multiple pages in a single file.
 *
 */
char* getRegularOutputFilename(int wks_id, const char* file_name, const char* envVar, const char* suffix) {
    /* get a root name */
    const char* root = getFileNameRoot(wks_id, file_name, envVar);
    int rootLen = strlen(root);

    /* we append a suffix below, but file_name may already have it */
    int suffixLen = (suffix) ? strlen(suffix) : 0;
    if (rootLen >= suffixLen && strncmp(root + strlen(root) - suffixLen, suffix, suffixLen) == 0)
        rootLen -= suffixLen;

    char* name = (char*) calloc(rootLen + suffixLen+1, 1); /* room for suffix + null */
    strncpy(name, root, rootLen);
    strcat(name, suffix);

    return name;

}

/*
 * getIndexedOutputFilename()
 *
 * A utility function to create filenames for formats that do not support multiple
 * images/pages. If more than one such file is to be written, then an index-number
 * is appended to the filename root. The index is omitted on the first such file,
 * and later added if there is indeed more than one file to be written.
 *
 */
char* getIndexedOutputFilename(int wks_id, const char* file_name, int frameNumber,
		const char* envVar, const char* suffix)
{
    /* get a root name... */
    const char* root = getFileNameRoot(wks_id, file_name, envVar);
    int rootLen = strlen(root);

    /* we append a suffix; remove it if already present on file_name */
    int suffixLen = (suffix) ? strlen(suffix) : 0;
    if (rootLen >= suffixLen && strncmp(root + strlen(root) - suffixLen, suffix, suffixLen) == 0)
        rootLen -= suffixLen;

    /* If only one file is generated, we want it named with out the .nnnnnn sequence number.
     * Unfortunately, we won't know until and unless we write a subsequent file. So the scheme is to
     * assume only one file is to be written, and we'll rename it if we find we are writing
     * more than one.
     */

    char* name;
    if (frameNumber == 0) {
    	name = (char*) calloc(rootLen + suffixLen+1, 1); /* room for suffix + null */
    	strncpy(name, root, rootLen);
        strcat(name, suffix);
    }
    else {
        const int seqNumLen = 8;  /* sequence num written as ".nnnnnn" plus a null */
        char seqNum[seqNumLen];
    	name = (char*) calloc(rootLen + suffixLen + seqNumLen + 1, 1);  /* +1 for a null */

        if (frameNumber == 1) {
            /* have to rename the first file that was written so that it has a sequence number */

            /* reconstruct old name... */
            char* oldName = (char*) calloc(rootLen + suffixLen+1, 1); /* room for suffix + null */
            strncpy(oldName, root, rootLen);
            strcat(oldName, suffix);

            /* new name... */
            sprintf(seqNum, ".%06d", 1);
            strncpy(name, root, rootLen);
            strcat(name, seqNum);
            strcat(name, suffix);
            int status = rename(oldName, name);
            if (status) {
                ESprintf(ERR_CRO_RENAME, "CRO: error renaming file from \"%s\" to \"%s\"; reason: (%d)\n",
                	oldName, name, status);
            }
            free(oldName);
            memset(name, '\0', strlen(name));  /* reset this for subsequent calls to strncpy/strcat */
        }

    	sprintf(seqNum, ".%06d", frameNumber + 1);
        strncpy(name, root, rootLen);
        strcat(name, seqNum);
        strcat(name, suffix);
    }

    return name;
}


static void CROinit(CROddp *psa, int *coords) {

    trace("Got to CROinit");

    psa->output_file = NULL;
    psa->window_title = NULL;
    psa->icon_title = NULL;
    psa->hatch_spacing = CRO_HATCH_SPACING;
    psa->path_size = MAX_PATH;
    psa->line_join = ROUND;
    psa->line_cap = ROUNDED;
    psa->nominal_width_scale = 0.5;
    psa->full_background = FALSE;
    psa->suppress_flag = SUPPRESS_FLAG;
    psa->miter_limit = MITER_LIMIT_DEFAULT;
    psa->sfill_spacing = CRO_FILL_SPACING;
    psa->frame_count = 0;
    psa->is_vector_type = FALSE;
    psa->cairo_fill_hack = FALSE;

#if 0 /* THIS IS NOT HANDED DOWN FROM HLU LAYER; coords+6 is undefined -- RLB*/
    /*
     *  Flag to suppress putting out background color rectangle.
     */
    psa->suppress_flag = *(coords+6);
#endif

    psa->background = FALSE;
    psa->background_alpha = 1.0;
    psa->pict_empty = TRUE;
    psa->page_number = 1;

    psa->attributes.linetype = LINETYPE_DEFAULT;
    psa->attributes.linetype_set = LINETYPE_DEFAULT;
    psa->attributes.linewidth = LINEWIDTH_DEFAULT;
    psa->attributes.linewidth_set = LINEWIDTH_DEFAULT;
    psa->attributes.line_colr_ind = LINE_COLR_DEFAULT;
    psa->attributes.line_alpha = 1.0;
    psa->attributes.marker_type = MARKER_TYPE_DEFAULT;
    psa->attributes.marker_size = MARKER_SIZE_DEFAULT;
    psa->attributes.marker_colr_ind = MARKER_COLR_IND_DEFAULT;
    psa->attributes.marker_alpha = 1.0;
    psa->attributes.text_font = TEXT_FONT_DEFAULT;
    psa->attributes.text_prec = TEXT_PREC_DEFAULT;
    psa->attributes.char_expan = CHAR_EXPAN_DEFAULT;
    psa->attributes.char_space = CHAR_SPACE_DEFAULT;
    psa->attributes.text_colr_ind = TEXT_COLR_IND_DEFAULT;
    psa->attributes.text_alpha = 1.0;
    psa->attributes.char_ht = CHAR_HT_DEFAULT;
    psa->attributes.char_up_vec_x = CHAR_UP_VEC_X_DEFAULT;
    psa->attributes.char_up_vec_y = CHAR_UP_VEC_Y_DEFAULT;
    psa->attributes.char_base_vec_x = CHAR_BASE_VEC_X_DEFAULT;
    psa->attributes.char_base_vec_y = CHAR_BASE_VEC_y_DEFAULT;
    psa->attributes.text_path = TEXT_PATH_DEFAULT;
    psa->attributes.text_align_horiz = TEXT_ALIGN_HORIZ_DEFAULT;
    psa->attributes.text_align_vert = TEXT_ALIGN_VERT_DEFAULT;
    psa->attributes.fill_int_style = FILL_INT_STYLE_DEFAULT;
    psa->attributes.fill_style_ind = FILL_STYLE_IND_DEFAULT;
    psa->attributes.fill_colr_ind = FILL_COLR_IND_DEFAULT;
    psa->attributes.fill_alpha = 1.0;
    psa->attributes.cro_colr_ind = CRO_COLR_IND_DEFAULT;
    psa->attributes.clip_ind = CLIP_IND_DEFAULT;

    psa->image_width = DEFAULT_IMAGE_WIDTH;
    psa->image_height = DEFAULT_IMAGE_HEIGHT;
    psa->paper_width = PSPDF_PAGESIZE_X;
    psa->paper_height = PSPDF_PAGESIZE_Y;
    psa->window_pos_x = 0;
    psa->window_pos_y = 0;

    int paperWidth = *(coords+4);
    int paperHeight = *(coords+5);
    if (paperWidth != 0 && paperWidth != -9999 && paperHeight != 0 && paperHeight != -9999) {
        psa->paper_width = paperWidth;
        psa->paper_height = paperHeight;
    }

    /* apply any escapes */
    _NGCesc *cesc;
    _NGCPixConfig *pixc;
    _NGCXWinConfig *xwinc;
    while ((cesc = _NGGetCEscInit())) {
        switch (cesc->type) {
        case NGC_PIXCONFIG:
            pixc = (_NGCPixConfig*) cesc;
            psa->image_width = pixc->width;
            psa->image_height = pixc->height;
            break;
        case NGC_XWINCONFIG:
            xwinc = (_NGCXWinConfig*) cesc;
            psa->window_title = xwinc->title;
            psa->icon_title = xwinc->icon_title;
            psa->image_width = xwinc->width;
            psa->image_height = xwinc->height;
            psa->window_pos_x = xwinc->x;
            psa->window_pos_y = xwinc->y;
            break;
        }
    }

    /* set up page/image layout and transformations */
    int cllx, clly, curx, cury;
    cllx = *coords;
    clly = *(coords + 1);
    curx = *(coords + 2);
    cury = *(coords + 3);
    if ((cllx != -9999) && (clly != -9999) && (curx != -9999)
            && (cury != -9999)) {
        psa->dspace.llx = (int) (((float) cllx));
        psa->dspace.urx = (int) (((float) curx));
        psa->dspace.lly = (int) (((float) clly));
        psa->dspace.ury = (int) (((float) cury));
    } else {
        psa->dspace.llx = (int) (((float) LLX_DEFAULT));
        psa->dspace.urx = (int) (((float) URX_DEFAULT));
        psa->dspace.lly = (int) (((float) LLY_DEFAULT));
        psa->dspace.ury = (int) (((float) URY_DEFAULT));
    }

    psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
    psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

    psa->cro_clip.llx = psa->dspace.llx;
    psa->cro_clip.lly = psa->dspace.lly;
    psa->cro_clip.urx = psa->dspace.urx;
    psa->cro_clip.ury = psa->dspace.ury;
    psa->cro_clip.null = FALSE;
    
    psa->useTiffCompression = TRUE;
    psa->tiffClosure = NULL;
    psa->georefData  = NULL;
}


unsigned int pack_argb(struct color_value cval) {
    /*
     *  Store as rgba
     */
    int ir, ig, ib, ia;
    unsigned int rval;
    ia = (int) (cval.alpha * 255.);
    ir = (int) (cval.red * 255.);
    ig = (int) (cval.green * 255.);
    ib = (int) (cval.blue * 255.);
    rval = ib;
    rval = (ig << 8) | rval;
    rval = (ir << 16) | rval;
    rval = (ia << 24) | rval;
    return rval;
}


struct color_value unpack_argb(unsigned int* ctable, unsigned int index) {
    struct color_value cval;

    unsigned int argb = ((index & ARGB_MASK) > 0)
            ? index          /* ARGB color... */
            : ctable[index]; /* color-index */
    cval.alpha = ((ALPHA_MASK & argb) >> 24) / 63.;   /* recall alpha is encoded with a limited dynamic range */
    cval.red   = ((RED_MASK   & argb) >> 16) / 255.;
    cval.green = ((GREEN_MASK & argb) >>  8) / 255.;
    cval.blue  = ((BLUE_MASK  & argb))       / 255.;

    return cval;
}


void setSurfaceTransform(CROddp *psa) {
    /*
     *  Translate and flip (since GKS origin is at bottom left) to center the plot.
     *  Since the final plot is to be flipped top to bottom, we want
     *  to translate in the Y direction to the original top.
     */

    double angle, tx, ty;
    double scale = 1.0;
    cairo_t* context = getContext(psa->wks_id);

    /* Landscape is only supported for PS/PDF, not for image-based formats. */
    if (psa->wks_type == CPS || psa->wks_type == CPDF || psa->wks_type == CEPS) {
	    if (psa->orientation == LANDSCAPE) {
		    angle = PI / 2.0;
		    tx = psa->dspace.llx;
		    ty = psa->paper_height - psa->dspace.ury;
	    } else {
		    angle = 0.;
		    tx = psa->dspace.llx;
		    ty = psa->paper_height - psa->dspace.lly;
	    }
    }
    else {
            angle = 0.;
	    tx = psa->dspace.llx;
	    ty = psa->dspace.ury;
    }
    cairo_identity_matrix(context);
    cairo_translate(context, tx, ty);
    cairo_rotate(context, angle);
    cairo_scale(context, scale, -scale);
}


/*
 *      Author:         Fred Clare
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Wed Sep 24 14:48:18 MDT 2008
 *
 *      Description:    This file contains routines for doing software
 *                      fill.  The argument "angle" specifies the angle
 *                      of the fill lines; the spacing between the fill
 *                      lines is extracted from the device dependent data.
 *                      This program is a conversion of the algorithm
 *                      implemented in Fortran in the NCAR Softfill package.
 */
void cro_SoftFill(GKSC *gksc, float angle, float spl) {
    float xco, yco, spi, rangle, tmp, smalld = .000001, *rst, tmpx, tmpy;
    int *ind, nnd, nra;
    int jnd, knd, ipt, ipe, indx1, indx2, previous_point, following_point;
    int i, isp, ipx, lnd, ip1, ip2, in1, in2, jn1, jn2, jnt;
    CROPoint *points, opoint;

    CROddp *psa;

    trace("Got to cro_SoftFill");

    psa = (CROddp *) gksc->ddp;
    points = (CROPoint *) (gksc->p).list;
    nra = (gksc->p).num;

    rangle = 0.017453292519943 * angle; /* converts angle to radians */

    /*
     *  Allocate memory.
     *
     *    rst --  The first nra elements of the rst array are used to
     *            store the directed distances of the points in the given
     *            polygon from the base line "xco*x+yco*y=0" (see code below
     *            for the computation of xco and yco).  The second nra
     *            elements (starting with rst[nra]) of rst are used to
     *            store the points of intersection of the current fill line
     *            with the line segments of the polygon (only one number is
     *            required since we know the distance of the current fill
     *            line from the base line).
     *
     *    ind --  The first nra elements of ind are used to store a permutation
     *            vector that orders the directed distances in rst[0] through
     *            rst[nra-1].  The second nra elements of rst, starting with
     *            rst[nra], is a permutation vector ordering the points of
     *            intersection stored in rst[nra], rst[nra+1], ... .  The
     *            third nra elements of ind are pointers to the points of
     *            intersection of the current line with the points of the
     *            input polygon.  These pointers are stored backwards in
     *            ind beginning with ind[3*nra].  Point "n" in this list
     *            refers to the line segment in the original polygon that
     *            begins at the previous point and terminates at point "n".
     */

    rst = (float *) malloc(2 * nra * sizeof(float));
    ind = (int *) malloc(3 * nra * sizeof(int));
    nnd = 3* nra ;

    /*
     *  Compute the constants "xco" and "yco" such that any line having an
     *  equation of the form "xco*x+yco*y=c" will have the desired angle.
     */
    xco = (float) (-sin((double) rangle));
    yco = (float) (cos((double) rangle));

    /*
     *  Compute the directed distances of the given points from the line
     *  "xco*x+yco*y=0".
     */
    for (i = 0; i < nra; ++i)
        rst[i] = xco * points[i].x + yco * points[i].y;

    /*
     *  Generate a list of indices of the distances, sorted by increasing
     *  distance.  rst[ind[1]], rst[ind[2]], ... rst[ind[nra]]
     *  is a list of the directed distances of the given points, in increasing
     *  numerical order.
     */
    cascsrt(rst, ind, nra);

    /*
     *  Draw lines at distances "spi" from the line "xco*x+yco*y=0" which are
     *  multiples of "spl" between the smallest and largest point distances.
     *  jnd points to the index of that point having the greatest distance
     *  less than the distance of the last line drawn (initially 0) and knd
     *  points to the end of the list of line segments which the last line
     *  drawn intersected - it is stored backwards at the end of ind - the
     *  initial value specifies that this list is empty.
     */
    jnd = -1;
    knd = nnd;

    /*
     *  ipt is the index of the next point past the last line drawn and ipe
     *  is the index of the last point.
     */
    ipt = ind[0];
    ipe = ind[nra - 1];
    indx1 = (int) (rst[ipt] / spl) - 1;
    indx2 = (int) (rst[ipe] / spl) + 1;
    for (isp = indx1; isp <= indx2; isp++) {
        spi = (float) isp * spl;

        /*
         *  Advance jnd to reflect the number of points passed over by the
         *  algorithm and update the list of pointers to intersecting lines.
         */
        while ((jnd < nra - 1) && (spi > rst[ipt])) {
            previous_point = (ipt + nra - 1) % nra;
            following_point = (ipt + 1) % nra;
            if (rst[previous_point] < rst[ipt]) {
                ipx = previous_point;

                /*
                 *  Remove intersecting line
                 */
                if (knd < nnd) {
                    for (i = knd; i < nnd; ++i) {
                        if (ind[i] == ipx) {
                            ind[i] = ind[knd];
                            ++knd;
                            break;
                        }
                    }
                }
            } else if (rst[previous_point] > rst[ipt]) {

                /*
                 *  Add an intersecting line.
                 */
                ipx = previous_point;
                --knd;
                ind[knd] = ipx;
            }
            if (rst[ipt] > rst[following_point]) {
                ipx = ipt;

                /*
                 *  Remove intersecting line
                 */
                if (knd < nnd) {
                    for (i = knd; i < nnd; ++i) {
                        if (ind[i] == ipx) {
                            ind[i] = ind[knd];
                            ++knd;
                            break;
                        }
                    }
                }
            } else if (rst[ipt] < rst[following_point]) {

                /*
                 *  Add an intersecting line.
                 */
                ipx = ipt;
                --knd;
                ind[knd] = ipx;
            }
            ++jnd;
            ipt = ind[jnd + 1];
        }
        /*
         *  Compute a set of values representing the intersection points of the
         *  current line with the line segments of the polygon.
         */
        if (knd < nnd) {
            lnd = nra - 1;
            if (fabs(xco) > fabs(yco)) {
                for (i = knd; i < nnd; ++i) {
                    ip1 = ind[i];
                    ip2 = (ind[i] + 1) % nra;
                    ++lnd;
                    tmp = xco * (points[ip2].x - points[ip1].x) + yco
                            * (points[ip2].y - points[ip1].y);
                    if (fabs(tmp) > smalld) {
                        rst[lnd] = (spi * (points[ip2].y - points[ip1].y) - xco
                                * (points[ip1].x * points[ip2].y
                                        - points[ip2].x * points[ip1].y)) / tmp;
                    } else {
                        rst[lnd] = .5* (points [ip1].y + points[ip2].y);
                    }
                }
            }
            else
            {
                for (i = knd; i < nnd; ++i)
                {
                    ip1 = ind[i];
                    ip2 = (ind[i] + 1) % nra;
                    ++lnd;
                    tmp = xco * (points[ip2].x - points[ip1].x)
                    + yco * (points[ip2].y - points[ip1].y);
                    if (fabs(tmp) > smalld) {
                        rst[lnd] = (spi * (points[ip2].x - points[ip1].x) + yco*
                                (points[ip1].x*points[ip2].y - points[ip2].x*points[ip1].y))/tmp;
                    }
                    else
                    {
                        rst[lnd] = .5*(points[ip1].x+points[ip2].x);
                    }
                }
            }

            /*
             *  Put these values in ascending order.  Actually, once again,
             *  we set up an index array specifying the order.
             */
            cascsrt (rst + nra, ind + nra, lnd - nra + 1);

            /*
             *  Draw the line segments specified by the list.
             */
            cairo_t* context = getContext(psa->wks_id);
            in1 = nra;
            if (fabs (xco) > fabs (yco))
            {
                while (in1 < lnd)
                {
                    jn1 = nra + ind[in1];
                    in2 = in1 + 1;
                    for(;;)
                    {
                        jn2 = nra + ind[in2];
                        if (in2 >= lnd)
                            break;
                        jnt = nra + ind[in2 + 1];
                        if ((rst[jnt] - rst[jn2]) > smalld)
                            break;
                        in2 += 2;
                    }
                    if (rst[jn2] - rst[jn1] > smalld)
                    {
                        opoint.x = (spi - yco * rst[jn1]) / xco;
                        opoint.y = rst[jn1];
                        tmpx = opoint.x * (float) psa->dspace.xspan;
                        tmpy = opoint.y * (float) psa->dspace.yspan;
                        cairo_move_to(context,tmpx,tmpy);
                        opoint.x = (spi - yco * rst[jn2]) / xco;
                        opoint.y = rst[jn2];
                        tmpx = opoint.x * (float) psa->dspace.xspan;
                        tmpy = opoint.y * (float) psa->dspace.yspan;
                        cairo_line_to(context,tmpx,tmpy);
                        cairo_stroke(context);
                    }
                    in1 = in2 + 1;
                }
            }
            else
            {
                while (in1 < lnd)
                {
                    jn1 = nra + ind[in1];
                    in2 = in1 + 1;
                    for(;;)
                    {
                        jn2 = nra + ind[in2];
                        if (in2 >= lnd)
                            break;
                        jnt = nra + ind[in2 + 1];
                        if (rst[jnt] - rst[jn2] > smalld)
                            break;
                        in2 += 2;
                    }
                    if (rst[jn2] - rst[jn1] > smalld)
                    {
                        opoint.x = rst[jn1];
                        opoint.y = (spi - xco * rst[jn1]) / yco;
                        tmpx = opoint.x * (float) psa->dspace.xspan;
                        tmpy = opoint.y * (float) psa->dspace.yspan;
                        cairo_move_to(context,tmpx,tmpy);
                        opoint.x = rst[jn2];
                        opoint.y = (spi - xco * rst[jn2]) / yco;
                        tmpx = opoint.x * (float) psa->dspace.xspan;
                        tmpy = opoint.y * (float) psa->dspace.yspan;
                        cairo_line_to(context,tmpx,tmpy);
                        cairo_stroke(context);
                    }
                    in1 = in2 + 1;
                }
            }
        }
    }
    free (rst);
    free (ind);
}

/*
 *  Given an array of  n  floating values in  xa,
 *  cascsrt returns a permutation vector ip such that
 *
 *   xa[ip[i]] <= xa[ip[j]]
 *         for all i,j such that  1 <= i <= j <= n .
 *
 *  This function uses the C library function qsort.
 */
static void cascsrt(float xa[], int ip[], int n)

{
    int i;

    if (n <= 0) {
        return;
    } else if (n == 1) {
        ip[0] = 0;
        return;
    }

    csort_array = xa;

    for (i = 0; i < n; i++) {
        ip[i] = i;
    }

    qsort((void *) ip, n, sizeof(ip[0]), ccompar);
    return;
}


static int ccompar(const void *p1, const void *p2) {
    float difference;
    int *i1, *i2;
    i1 = (int *) p1;
    i2 = (int *) p2;
    difference = csort_array[*i1] - csort_array[*i2];
    if (difference < 0.0)
        return (-1);
    if (difference > 0.0)
        return (1);
    return (0);
}


/*
 *  Function for reversing the characters in a string.
 */
void reverse_chrs(char *str) {
    int strl = strlen(str), i, index;
    char ctmp;

    for (i = 0; i < strl / 2; i++) {
        index = strl - i - 1;
        strncpy(&ctmp, str + i, 1);
        strncpy(str + i, str + index, 1);
        strncpy(str + index, &ctmp, 1);
    }
}

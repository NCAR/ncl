/*
 *      $Id: cro.c,v 1.16 2010-04-02 16:36:16 brownrig Exp $
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

#include <cairo/cairo.h>
#include <cairo/cairo-ps.h>
#include <cairo/cairo-pdf.h>
#include <cairo/cairo-ft.h>

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
#include "cro_device.h"
#include "croddi.h"
#include "cro.h"

#define JIRA_494 1   /* temporary, until issued resolved */

#define PI 3.1415926
#define RINT(A) ((A) > 0 ? (int) ((A) + 0.5) : -(int) (0.5 - (A)))
#define NUM_CONTEXT 20  /* number of allowable contexts */

static const char *getFileNameRoot(int, const char*, const char*);
static char *getRegularOutputFilename(int, const char*, const char*, const char*);
static char *getIndexedOutputFilename(int, const char*, int, const char*, const char*);
static void setSurfaceTransform(CROddp *psa);
static void CROinit(CROddp *, int *);
static void CROpict_init(GKSC *gksc);
static unsigned int pack_argb(struct color_value);
static struct color_value unpack_argb(unsigned int);
static int ccompar(const void *p1, const void *p2);
static void cascsrt(float xa[], int ip[], int n);
static float *csort_array;
static void reverse_chrs(char*);

extern crotiff_writeImage(const char* filename, cairo_surface_t* surface);

/* this looks like it should be static, but there's a pattern where the SoftFill() routines
 * are declared external for this, the PS, and the PDF drivers. Leaving it for now.  --RLB 1/2010.
 */
void cro_SoftFill(GKSC *gksc, float angle, float spl);

/*
 *  Globals
 */

/*
 *  Functions for mapping workstation IDs into indices for the
 *  cario_context array.
 */
int context_num = 0;
cairo_surface_t *cairo_surface[NUM_CONTEXT];
cairo_t         *cairo_context[NUM_CONTEXT];
int             context_indices[NUM_CONTEXT];

static int context_index(int wkid) {
    int i;
    for (i = 0; i < NUM_CONTEXT; i++) {
        if (context_indices[i] == wkid) {
            return i;
        }
    }
}

static void add_context_index(int cindex, int wkid) {
    context_indices[cindex] = wkid;
    cindex++;
}

static void remove_context(int wkid) {
    int i;
    int index = context_index(wkid);
    for (i=index+1; i<context_num; i++) {
        cairo_surface[i-1] = cairo_surface[i];
        cairo_context[i-1] = cairo_context[i];
        context_indices[i-1] = context_indices[i];
    }

    --context_num;
}

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

    if (getenv("CRO_TRACE")) {
        printf("Got to CROpict_init\n");
    }

    /*
     *  Get the background color and set the source to the background color.
     */
    cval = unpack_argb((psa->ctable)[0]);
    cairo_set_source_rgba(cairo_context[context_index(psa->wks_id)], cval.red,
            cval.green, cval.blue, cval.alpha);

    /* NOTE: This is likely not quite right, but I don't understand the use of the clipping rectangle below. In any case,
     * the code that does the Right Thing for PS/PDF does not result in a complete fill for image-based formats,
     * so we proceed differently  --RLB
     */
    if (psa->wks_type == CPS || psa->wks_type == CPDF) {
        double xl, yt, xr, yb;

        /*
         *  Save the current clip extents and reset the clipping rectangle to max.
         */
        cairo_clip_extents(cairo_context[context_index(psa->wks_id)], &xl, &yt,
                &xr, &yb);
        cairo_reset_clip(cairo_context[context_index(psa->wks_id)]);

        /*
         *  Set the clipping rectangle to the surface area.
         */
        cairo_move_to(cairo_context[context_index(psa->wks_id)], 0., 0.);
        cairo_line_to(cairo_context[context_index(psa->wks_id)],
                psa->dspace.urx - psa->dspace.llx, 0.);
        cairo_line_to(cairo_context[context_index(psa->wks_id)],
                psa->dspace.urx - psa->dspace.llx, psa->dspace.ury
                        - psa->dspace.lly);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], 0.,
                psa->dspace.ury - psa->dspace.lly);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], 0., 0.);
        cairo_clip(cairo_context[context_index(psa->wks_id)]);

        /*
         *  Fill the surface clip region with the background color.
         */
        cairo_move_to(cairo_context[context_index(psa->wks_id)], 0., 0.);
        cairo_line_to(cairo_context[context_index(psa->wks_id)],
                psa->dspace.urx - psa->dspace.llx, 0.);
        cairo_line_to(cairo_context[context_index(psa->wks_id)],
                psa->dspace.urx - psa->dspace.llx, psa->dspace.ury
                        - psa->dspace.lly);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], 0.,
                psa->dspace.ury - psa->dspace.lly);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], 0., 0.);

        cairo_fill(cairo_context[context_index(psa->wks_id)]);

        /*
         *  Restore the clipping rectangle to what it was on entry.
         *  cairo_clip clears the path.
         */
        cairo_move_to(cairo_context[context_index(psa->wks_id)], xl, yt);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], xr, yt);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], xr, yb);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], xl, yb);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], xl, yt);
        cairo_clip(cairo_context[context_index(psa->wks_id)]);
    } else {
        /* code for image-based formats */
        cairo_save(cairo_context[context_index(psa->wks_id)]);
        cairo_reset_clip(cairo_context[context_index(psa->wks_id)]);
        cairo_identity_matrix(cairo_context[context_index(psa->wks_id)]);
        cairo_rectangle(cairo_context[context_index(psa->wks_id)], 0, 0,
                psa->image_width, psa->image_height);
        cairo_fill(cairo_context[context_index(psa->wks_id)]);
        cairo_restore(cairo_context[context_index(psa->wks_id)]);
    }

    psa->pict_empty = FALSE;

    return;
}


/*
 * Set the current dash pattern depending on the line type.
 */
void CROset_dashpattern(CROddp *psa) {
    float nominal_dash_size = 1., dash_size, dot_size, gap_size;
    double *dashes = (double *) NULL;

    if (getenv("CRO_TRACE")) {
        printf("Got to CRset_dashpattern\n");
    }

    dash_size = 6. * nominal_dash_size;
    dot_size = 1. * nominal_dash_size;
    gap_size = 4. * nominal_dash_size;
    switch (psa->attributes.linetype) {
    case SOLID_LINE:
        cairo_set_dash(cairo_context[context_index(psa->wks_id)], dashes, 0, 0.);
        break;
    case DASHED_LINE:
        dashes = (double *) calloc(2, sizeof(double));
        *dashes = (double) dash_size;
        *(dashes + 1) = (double) gap_size;
        cairo_set_dash(cairo_context[context_index(psa->wks_id)], dashes, 2, 0.);
        break;
    case DOTTED_LINE:
        dashes = (double *) calloc(1, sizeof(double));
        *dashes = (double) dot_size;
        cairo_set_dash(cairo_context[context_index(psa->wks_id)], dashes, 1, 0.);
        break;
    case DASH_DOT_LINE:
        dashes = (double *) calloc(4, sizeof(double));
        *dashes = (double) dash_size;
        *(dashes + 1) = (double) gap_size;
        *(dashes + 2) = (double) dot_size;
        *(dashes + 3) = (double) gap_size;
        cairo_set_dash(cairo_context[context_index(psa->wks_id)], dashes, 4, 0.);
        break;
    case DASH_DOT_DOT_LINE:
        dashes = (double *) calloc(3, sizeof(double));
        *dashes = (double) dash_size;
        *(dashes + 1) = (double) gap_size;
        *(dashes + 2) = (double) dot_size;
        *(dashes + 3) = (double) gap_size;
        *(dashes + 4) = (double) dot_size;
        *(dashes + 5) = (double) gap_size;
        cairo_set_dash(cairo_context[context_index(psa->wks_id)], dashes, 6, 0.);
        break;
    default:
        cairo_set_dash(cairo_context[context_index(psa->wks_id)], dashes, 0, 0.);
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
    cairo_clip_extents(cairo_context[context_index(psa->wks_id)], &x1, &y1,
            &x2, &y2);
    rect.llx = x1;
    rect.urx = x2;
    rect.lly = y2;
    rect.ury = y1;
    return (rect);
}


int cro_ActivateWorkstation(GKSC *gksc) {

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_ActivateWorkstation\n");
    }

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


get_cell_index_limits(GKSC *gksc, CROPoint P, CROPoint Q, int nx, int ny,
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
    int i, j, k, l, image_width, image_height, kount, stat;
    int j_start, j_end, j_inc, i_start, i_end, i_inc;
    int *rows, *cols;
    unsigned int *iar;
    unsigned char *data;
    cairo_surface_t *cell_image;

    CROPoint P, Q, R;

    CROddp *psa = (CROddp *) gksc->ddp;
    CROPoint *corners = (CROPoint *) gksc->p.list;

    int *iptr = (int *) gksc->i.list;
    int *colia = (int *) gksc->x.list;

    double x_offset, y_offset;

    double tred, tgreen, tblue, talpha;
    struct color_value cval;
    cairo_pattern_t *pattern;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_Cellarray\n");
    }

    /*
     *  Save current color.
     */
    pattern = cairo_get_source(cairo_context[context_index(psa->wks_id)]);
    if (cairo_pattern_get_rgba(pattern, &tred, &tgreen, &tblue, &talpha)
            != CAIRO_STATUS_SUCCESS) {
        printf("cro_Text: can only retrieve current color for solid patterns\n");
        return (1);
    }
    cval = unpack_argb((psa->ctable)[psa->attributes.text_colr_ind]);

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    nx = iptr[0];
    ny = iptr[1];
    P = corners[0];
    Q = corners[1];
    R = corners[2];

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
                    iar[kount] = (psa->ctable)[colia[j * nx + i]];
                    kount++;
                }
            }
        }
    }

    data = (unsigned char *) iar;
    cell_image = cairo_image_surface_create_for_data(data, CAIRO_FORMAT_ARGB32,
            image_width, image_height, 4* image_width );
    stat = cairo_surface_status(cell_image);

    cairo_set_source_surface(cairo_context[context_index(psa->wks_id)],
            cell_image, x_offset, y_offset);
    cairo_paint(cairo_context[context_index(psa->wks_id)]);

    /* free up resources need to create the cairo_surface... */
    cairo_surface_finish(cell_image);
    cairo_surface_destroy(cell_image);
    free(data);
    free(rows);
    free(cols);

    /*
     *  Restore color.
     */
    cairo_set_source_rgba(cairo_context[context_index(psa->wks_id)], cval.red,
            cval.green, cval.blue, cval.alpha);
    return (0);
}


int cro_ClearWorkstation(GKSC *gksc) {
    int ret = 0;
    char* outputFile;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_ClearWorkstation\n");
    }

    CROddp *psa;
    psa = (CROddp *) gksc->ddp;

    cairo_stroke(cairo_context[context_index(psa->wks_id)]);
    cairo_show_page(cairo_context[context_index(psa->wks_id)]);

    if (psa->wks_type == CPS || psa->wks_type == CPDF) {
        cairo_surface_flush(cairo_surface[context_index(psa->wks_id)]);

    } else if (psa->wks_type == CPNG) {
        outputFile = getIndexedOutputFilename(psa->wks_id, psa->output_file, psa->frame_count,
            "NCARG_GKS_CPNGOUTPUT", ".png");
        ret = cairo_surface_write_to_png(cairo_surface[context_index(psa->wks_id)],
                outputFile);
        if (ret != 0)
            ret = ERR_CRO_OPN;
        psa->frame_count++;
        free(outputFile);
    }

    else if (psa->wks_type == CTIFF) {
        outputFile = getIndexedOutputFilename(psa->wks_id, psa->output_file, psa->frame_count,
            "NCARG_GKS_CTIFFOUTPUT", ".tif");
        ret = crotiff_writeImage(outputFile, cairo_surface[context_index(psa->wks_id)]);
        psa->frame_count++;
        free(outputFile);
    }

    psa->pict_empty = TRUE;

    return ret;
}


int cro_CloseWorkstation(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_CloseWorkstation\n");
    }

    psa->pict_empty = TRUE;
    if (psa->output_file)
        free(psa->output_file);

    if (psa->max_color > 0)
        free(psa->ctable);

    cairo_destroy(cairo_context[context_index(psa->wks_id)]);
    remove_context(psa->wks_id);
    free(psa);

    return (0);
}


int cro_DeactivateWorkstation(GKSC *gksc) {

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_DeactivateWorkstation\n");
    }

    return (0);
}


int cro_Esc(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    char *sptr = (char *) gksc->s.list, *strng;
    int *iptr = (int *) gksc->i.list;
    _NGCesc *cesc = (_NGCesc*) gksc->native;
    _NGCPixConfig *pixconf;

    int escape_id = iptr[0], plflag;
    float rscale, logox, logoy, logos;
    static int saved_color_index;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_Esc\n");
    }

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

    case -1529:  /* paper width, in points */
        strng = strtok(sptr, " ");
        psa->paper_width = atoi(strng);
        break;

    case -1530:  /* paper height, in points */
        strng = strtok(sptr, " ");
        psa->paper_height = atoi(strng);
        break;

    case NGESC_CNATIVE: /* C-escape mechanism;  get resolution of image-based output formats */
        switch (cesc->type) {
        case NGC_PIXCONFIG:
            pixconf = (_NGCPixConfig*) cesc;
            psa->image_width = pixconf->width;
            psa->image_height = pixconf->height;
            break;
        }
    }

    return (0);
}


int cro_FillArea(GKSC *gksc) {
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;
    int npoints = gksc->p.num, i;

    float clwidth;
    struct color_value cval;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_FillArea\n");
    }

    clwidth = (float) cairo_get_line_width(cairo_context[context_index(
            psa->wks_id)]);

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    cairo_set_line_width(cairo_context[context_index(psa->wks_id)], 1.1
            * (psa->dspace.yspan) * (psa->sfill_spacing));

    cval = unpack_argb((psa->ctable)[psa->attributes.fill_colr_ind]);
    cairo_set_source_rgba(cairo_context[context_index(psa->wks_id)], cval.red,
            cval.green, cval.blue, cval.alpha);

    switch (psa->attributes.fill_int_style) {
    case HOLLOW_FILL: /* Put out polyline */
        cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan);
        }
        cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        cairo_stroke(cairo_context[context_index(psa->wks_id)]);
        break;
    case SOLID_FILL:
        cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan);
        }
        cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        cairo_fill(cairo_context[context_index(psa->wks_id)]);
        break;
    case PATTERN_FILL: /* currently not implemented, issue polyline */
        cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan);
        }
        cairo_stroke(cairo_context[context_index(psa->wks_id)]);
        break;
    case HATCH_FILL:
        switch (psa->attributes.fill_style_ind) {
        case HORIZONTAL_HATCH:
            cro_SoftFill(gksc, 0., psa->hatch_spacing);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        case VERTICAL_HATCH:
            cro_SoftFill(gksc, 90., psa->hatch_spacing);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        case POSITIVE_HATCH:
            cro_SoftFill(gksc, 45., psa->hatch_spacing);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        case NEGATIVE_HATCH:
            cro_SoftFill(gksc, 135., psa->hatch_spacing);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        case HORIZ_VERT_HATCH:
            cro_SoftFill(gksc, 0., psa->hatch_spacing);
            cro_SoftFill(gksc, 90., psa->hatch_spacing);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        case POS_NEG_HATCH:
            cro_SoftFill(gksc, 45., psa->hatch_spacing);
            cro_SoftFill(gksc, 135., psa->hatch_spacing);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        default:
            cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                    * (float) psa->dspace.xspan, pptr[0].y
                    * (float) psa->dspace.yspan);
            for (i = 1; i < npoints; i++) {
                cairo_line_to(cairo_context[context_index(psa->wks_id)],
                        pptr[i].x * (float) psa->dspace.xspan, pptr[i].y
                                * (float) psa->dspace.yspan);
            }
            cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                    * (float) psa->dspace.xspan, pptr[0].y
                    * (float) psa->dspace.yspan);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            break;
        }
        break;
    default:
        cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        for (i = 1; i < npoints; i++) {
            cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan);
        }
        cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
                * (float) psa->dspace.xspan, pptr[0].y
                * (float) psa->dspace.yspan);
        cairo_stroke(cairo_context[context_index(psa->wks_id)]);
        break;
    }
    cairo_set_line_width(cairo_context[context_index(psa->wks_id)], clwidth);
    return (0);
}


int cro_GetColorRepresentation(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;

    int *xptr = (int *) gksc->x.list;
    CROColor *rgbptr = (CROColor *) gksc->rgb.list;
    struct color_value cval;

    int index = xptr[0];

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_GetColorRepresentation\n");
    }

    cval = unpack_argb((psa->ctable)[index]);

    rgbptr[0].r = cval.red;
    rgbptr[0].g = cval.green;
    rgbptr[0].b = cval.blue;

    return (0);
}


int cro_OpenWorkstation(GKSC *gksc) {

    char *sptr = (char *) gksc->s.list;
    CROddp *psa;
    char *ctmp;
    int *pint;
    extern int orig_wks_id;
    static CROClipRect rect;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_OpenWorkstation\n");
    }

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

    if (psa->wks_type == CPS) {
        /*
         *  Create a Postscript workstation.
         */
        psa->output_file = getRegularOutputFilename(psa->wks_id, sptr, "NCARG_GKS_CPSOUTPUT", ".ps");
        cairo_surface[context_num] = cairo_ps_surface_create(psa->output_file,
                psa->paper_width, psa->paper_height);
        cairo_ps_surface_set_size(cairo_surface[context_num], psa->paper_width, psa->paper_height);
        cairo_context[context_num] = cairo_create(cairo_surface[context_num]);
        add_context_index(context_num, orig_wks_id);
        psa->orientation = PORTRAIT;
    }

    else if (psa->wks_type == CPDF) {
        /*
         *  Create a PDF workstation.
         */
        psa->output_file = getRegularOutputFilename(psa->wks_id, sptr, "NCARG_GKS_CPDFOUTPUT", ".pdf");
        cairo_surface[context_num] = cairo_pdf_surface_create(psa->output_file,
                psa->paper_width, psa->paper_height);
        cairo_context[context_num] = cairo_create(cairo_surface[context_num]);
        add_context_index(context_num, orig_wks_id);
        psa->orientation = PORTRAIT;
    }

    else if (psa->wks_type == CPNG) {
        /*
         *  Create a PNG workstation.
         */
        cairo_surface[context_num] = cairo_image_surface_create(
                CAIRO_FORMAT_ARGB32, psa->image_width, psa->image_height);
        cairo_context[context_num] = cairo_create(cairo_surface[context_num]);
        add_context_index(context_num, orig_wks_id);
        psa->output_file = (char*) malloc(strlen(sptr) + 1);
        strcpy(psa->output_file, sptr);
    }

    else if (psa->wks_type == CTIFF) {
        /*
         *  Create a (geo)Tiff workstation.
         */
        cairo_surface[context_num] = cairo_image_surface_create(
                CAIRO_FORMAT_ARGB32, psa->image_width, psa->image_height);
        cairo_context[context_num] = cairo_create(cairo_surface[context_num]);
        add_context_index(context_num, orig_wks_id);
        psa->output_file = (char*) malloc(strlen(sptr) + 1);
        strcpy(psa->output_file, sptr);
    }

    /*
     *  Set fill rule to even/odd.
     */
    cairo_set_fill_rule(cairo_context[context_num], CAIRO_FILL_RULE_EVEN_ODD);

    /*
     *  Set default line cap and line join to round.
     */
    cairo_set_line_cap(cairo_context[context_num], CAIRO_LINE_CAP_ROUND);
    cairo_set_line_join(cairo_context[context_num], CAIRO_LINE_JOIN_ROUND);
    cairo_surface_destroy(cairo_surface[context_num]);

    /*
     *  Set the default linewidth.
     */
    cairo_set_line_width(cairo_context[context_num], psa->attributes.linewidth);

    /*
     *  Set clipping rectangle to max.
     */
    cairo_new_path(cairo_context[context_num]);
    cairo_move_to(cairo_context[context_num], psa->dspace.llx, psa->dspace.lly);
    cairo_line_to(cairo_context[context_num], psa->dspace.llx
            + psa->dspace.xspan, psa->dspace.lly);
    cairo_line_to(cairo_context[context_num], psa->dspace.llx
            + psa->dspace.xspan, psa->dspace.lly + psa->dspace.yspan);
    cairo_line_to(cairo_context[context_num], psa->dspace.llx, psa->dspace.lly
            + psa->dspace.yspan);
    cairo_line_to(cairo_context[context_num], psa->dspace.llx, psa->dspace.lly);
    cairo_clip(cairo_context[context_num]);

    rect = GetCROClipping(psa);

    setSurfaceTransform(psa);

    /*
     *  Initialize color table for this workstation.
     */
    psa->ctable = (unsigned int *) calloc(2, sizeof(unsigned int));
    psa->max_color = 2;

    /*
     *  Define the default foreground (black) and background (white)
     *  colors and draw the background.
     */
    (psa->ctable)[0] = 0xFFFFFFFF;
    (psa->ctable)[1] = 0xFF000000;
    /*
     *  Select the foreground color.
     */
    cairo_set_source_rgba(cairo_context[context_num], 0., 0., 0., 1.);

    context_num++;

    return (0);
}


int cro_Polyline(GKSC *gksc) {
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;

    int npoints = gksc->p.num, i, ier, ltype;
    struct color_value cval;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_Polyline\n");
    }

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    /*
     *  Set the dash pattern based on the line type.
     */
    cval = unpack_argb((psa->ctable)[psa->attributes.line_colr_ind]);
    cairo_set_source_rgba(cairo_context[context_index(psa->wks_id)], cval.red,
            cval.green, cval.blue, cval.alpha);

    cairo_set_line_width(cairo_context[context_index(psa->wks_id)],
            (psa->nominal_width_scale) * (psa->attributes.linewidth));
    cairo_new_sub_path(cairo_context[context_index(psa->wks_id)]);

    /*
     *  Use butt ends if not solid line.
     */
    ginq_linetype(&ier, &ltype);
    if (ltype != 1) {
        cairo_set_line_cap(cairo_context[context_index(psa->wks_id)],
                CAIRO_LINE_CAP_BUTT);
    } else {
        cairo_set_line_cap(cairo_context[context_index(psa->wks_id)],
                CAIRO_LINE_CAP_ROUND);
    }
    CROset_dashpattern(psa);

    cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[0].x
            * (float) psa->dspace.xspan, pptr[0].y * (float) psa->dspace.yspan);
    for (i = 1; i < npoints; i++) {
        cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                * (float) psa->dspace.xspan, pptr[i].y
                * (float) psa->dspace.yspan);
    }
    cairo_stroke(cairo_context[context_index(psa->wks_id)]);

    /*
     *  Set line cap back to round in case it was changed.
     */
    cairo_set_line_cap(cairo_context[context_index(psa->wks_id)],
            CAIRO_LINE_CAP_ROUND);

    return (0);
}


int cro_Polymarker(GKSC *gksc) {
    CROPoint *pptr = (CROPoint *) gksc->p.list;
    CROddp *psa = (CROddp *) gksc->ddp;
    int npoints = gksc->p.num, i;
    double red, green, blue, alpha;

    int marker_type, current_line_color;
    float marker_size, orig_line_width, xc, yc, mscale;
    struct color_value cval;
    cairo_line_cap_t orig_cap_type;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_Polymarker\n");
    }

    if (psa->pict_empty) {
        CROpict_init(gksc);
    }

    marker_type = psa->attributes.marker_type;
    marker_size = ((psa->transform.y_scale) * (psa->attributes.marker_size));

    /*
     *  Retrieve current line color index so we can reset it
     *  at the end, since we will be using Polylines for the
     *  markers.
     */
    current_line_color = psa->attributes.line_colr_ind;
    cval = unpack_argb((psa->ctable)[psa->attributes.marker_colr_ind]);
    cairo_set_source_rgba(cairo_context[context_index(psa->wks_id)], cval.red,
            cval.green, cval.blue, cval.alpha);

    /*
     *  Get the current setting for line cap and set it to round so that
     *  a dot will be drawn in the degenerate case.  Do the same for
     *  linewidth.
     */
    orig_cap_type = cairo_get_line_cap(
            cairo_context[context_index(psa->wks_id)]);
    cairo_set_line_cap(cairo_context[context_index(psa->wks_id)],
            CAIRO_LINE_CAP_ROUND);
    orig_line_width = cairo_get_line_width(cairo_context[context_index(
            psa->wks_id)]);
    cairo_set_line_width(cairo_context[context_index(psa->wks_id)], 1.0);

    switch (marker_type) {
    case DOT_MARKER:

        /*
         *  Dot markers cannot be scaled.
         */
        cairo_set_line_cap(cairo_context[context_index(psa->wks_id)],
                CAIRO_LINE_CAP_ROUND);
        cairo_set_line_width(cairo_context[context_index(psa->wks_id)], 0.5);
        for (i = 0; i < npoints; i++) {
#ifndef JIRA_494
            cairo_move_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan);
#else
            cairo_arc(cairo_context[context_index(psa->wks_id)], pptr[i].x
                    * (float) psa->dspace.xspan, pptr[i].y
                    * (float) psa->dspace.yspan, 0.25, 0., 6.2831853);
#endif
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
        }
        cairo_set_line_width(cairo_context[context_index(psa->wks_id)],
                orig_line_width);
        break;
    case PLUS_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75;
            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc, yc
                    - mscale * marker_size);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc, yc
                    + mscale * marker_size);

            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc
                    - mscale * marker_size, yc);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc
                    + mscale * marker_size, yc);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
        }
        break;
    case STAR_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75 * marker_size;

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc, yc
                    - mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc, yc
                    + mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc - 0.866
                    * mscale, yc - 0.5 * mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc + 0.866
                    * mscale, yc + 0.5 * mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc - 0.866
                    * mscale, yc + 0.5 * mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc + 0.866
                    * mscale, yc - 0.5 * mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

        }
        break;
    case CIRCLE_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75;
            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc, yc);
            cairo_new_sub_path(cairo_context[context_index(psa->wks_id)]);
            cairo_arc(cairo_context[context_index(psa->wks_id)], xc, yc, mscale
                    * marker_size, 0., 2. * M_PI);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);
        }
        break;
    case X_MARKER:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 3. * marker_size;

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc - 0.707
                    * mscale, yc - 0.707 * mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc + 0.707
                    * mscale, yc + 0.707 * mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc - 0.707
                    * mscale, yc + 0.707 * mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc + 0.707
                    * mscale, yc - 0.707 * mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

        }
        break;
    default:
        for (i = 0; i < npoints; i++) {
            xc = pptr[i].x * (float) psa->dspace.xspan;
            yc = pptr[i].y * (float) psa->dspace.yspan;
            mscale = 2.75 * marker_size;

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc, yc
                    - mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc, yc
                    + mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc - 0.866
                    * mscale, yc - 0.5 * mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc + 0.866
                    * mscale, yc + 0.5 * mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

            cairo_move_to(cairo_context[context_index(psa->wks_id)], xc - 0.866
                    * mscale, yc + 0.5 * mscale);
            cairo_line_to(cairo_context[context_index(psa->wks_id)], xc + 0.866
                    * mscale, yc - 0.5 * mscale);
            cairo_stroke(cairo_context[context_index(psa->wks_id)]);

        }
        break;
    }

    /*
     *  Restore line cap type and line width.
     */
    cairo_set_line_cap(cairo_context[context_index(psa->wks_id)], orig_cap_type);
    cairo_set_line_width(cairo_context[context_index(psa->wks_id)],
            orig_line_width);
    return (0);
}


int cro_SetCharacterExpansionFactor(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetCharacterExpansionFactor\n");
    }

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

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetCharacterHeightAndUpVector\n");
    }

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

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetCharacterSpacing\n");
    }

    psa->attributes.char_space = fptr[0];
    return (0);
}


int cro_SetClipIndicator(GKSC *gksc) {
    CROClipRect rect;

    CROddp *psa = (CROddp *) gksc->ddp;
    int *iptr = (int *) gksc->i.list;
    CROPoint *pptr = (CROPoint *) gksc->p.list;

    int clip_flag;
    float pllx, purx, plly, pury;
    float tllx, turx, tlly, tury;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetClipIndicator\n");
    }

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
        cairo_new_path(cairo_context[context_index(psa->wks_id)]);
        /*
         *  Set the clip rectangle.
         */
        cairo_reset_clip(cairo_context[context_index(psa->wks_id)]);
        cairo_move_to(cairo_context[context_index(psa->wks_id)], pllx
                * psa->dspace.xspan, plly * psa->dspace.yspan);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], purx
                * psa->dspace.xspan, plly * psa->dspace.yspan);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], purx
                * psa->dspace.xspan, pury * psa->dspace.yspan);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], pllx
                * psa->dspace.xspan, pury * psa->dspace.yspan);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], pllx
                * psa->dspace.xspan, plly * psa->dspace.yspan);
        cairo_clip(cairo_context[context_index(psa->wks_id)]);

        psa->attributes.clip_ind = 1;
    } else {
        psa->attributes.clip_ind = 0;
        cairo_reset_clip(cairo_context[context_index(psa->wks_id)]);
        cairo_new_path(cairo_context[context_index(psa->wks_id)]);
        cairo_move_to(cairo_context[context_index(psa->wks_id)], 0, 0);
        cairo_line_to(cairo_context[context_index(psa->wks_id)],
                psa->dspace.xspan, 0);
        cairo_line_to(cairo_context[context_index(psa->wks_id)],
                psa->dspace.xspan, psa->dspace.yspan);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], 0,
                psa->dspace.yspan);
        cairo_line_to(cairo_context[context_index(psa->wks_id)], 0, 0);
        cairo_clip(cairo_context[context_index(psa->wks_id)]);
    }
    return (0);
}


int cro_SetColorRepresentation(GKSC *gksc) {
    int *xptr = (int *) gksc->x.list;
    CROColor *rgbptr = (CROColor *) gksc->rgb.list;
    CROddp *psa = (CROddp *) gksc->ddp;

    unsigned index = (unsigned) xptr[0];
    int i;
    double xl, yt, xr, yb;

    struct color_value cval;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetColorRepresentation\n");
    }

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

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetFillAreaColorIndex: %d\n", (unsigned int) xptr[0]);
    }
    psa->attributes.fill_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetFillAreaInteriorStyle(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *iptr = (int *) gksc->i.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetFillAreaInteriorStyle\n");
    }

    psa->attributes.fill_int_style = iptr[0];

    return (0);
}


int cro_SetFillAreaStyleIndex(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;

    int *iptr = (int *) gksc->i.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetFillAreaStyleIndex\n");
    }

    psa->attributes.fill_style_ind = iptr[0];
    return (0);
}


int cro_SetLineWidthScaleFactor(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetLineWidthScaleFactor\n");
    }

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

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetLinetype\n");
    }

    psa->attributes.linetype = iptr[0];
    return (0);
}


int cro_SetMarkerSizeScaleFactor(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_setMarkerSizeScaleFactor\n");
    }

    psa->attributes.marker_size = fptr[0];
    return (0);
}


int cro_SetMarkerType(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    int *iptr = (int *) gksc->i.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetMarkerType\n");
    }

    psa->attributes.marker_type = iptr[0];
    return (0);
}


int cro_SetPolylineColorIndex(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->x.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetPolylineColorIndex\n");
    }
    psa->attributes.line_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetPolymarkerColorIndex(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->x.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetPolymarkerColorIndex\n");
    }

    psa->attributes.marker_colr_ind = (unsigned int) xptr[0];
    return (0);
}


int cro_SetTextAlignment(GKSC *gksc) {
    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->i.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetTextAlignment\n");
    }

    psa->attributes.text_align_horiz = (unsigned int) xptr[0];
    psa->attributes.text_align_vert = (unsigned int) xptr[1];
    return (0);
}


int cro_SetTextColorIndex(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    int *xptr = (int *) gksc->x.list;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetTextColorIndex\n");
    }

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

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetTextFontAndPrecision\n");
    }

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

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetTextPath\n");
    }

    psa->attributes.text_path = iptr[0];
    return (0);
}


int cro_SetViewport(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;
    float *fptr = (float *) gksc->f.list;

    CROClipRect *Crect;
    int rec_chg = 0;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetViewport\n");
    }

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

    CROClipRect *Crect;
    int rec_chg = 0;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SetWindow\n");
    }

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
    char single_char[2], next_char[2];
    struct color_value cval;
    float base_mag, tcos, cprod, cang, cspace, up_down_string_width;
    int slen, error, i, char_num_mx;
    double *dashes, horiz_len, left_space, right_space, maximum_width = 0.;

    float xc, yc, xpos, ypos, X_height, scl, x_del, y_del;
    static int kount = 0;

    static FT_Library library;
    static FT_Face face;
    cairo_font_face_t *font_face;
    cairo_matrix_t fmatrix, scl_matrix;
    cairo_text_extents_t textents, next_extents;
    cairo_font_extents_t fextents;

    cairo_text_extents(cairo_context[context_index(psa->wks_id)], sptr,
            &textents);

    cairo_get_font_matrix(cairo_context[context_index(psa->wks_id)], &fmatrix);

    cairo_matrix_scale(&fmatrix, 1., -1.);
    cairo_set_font_matrix(cairo_context[context_index(psa->wks_id)], &fmatrix);
    cairo_get_font_matrix(cairo_context[context_index(psa->wks_id)], &fmatrix);
    if (getenv("CRO_TRACE")) {
        printf("Got to cro_Text\n");
    }

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

    cairo_set_font_face(cairo_context[context_index(psa->wks_id)], font_face);
    cairo_font_extents(cairo_context[context_index(psa->wks_id)], &fextents);

    /*
     *  Character height.
     */
    cairo_set_font_size(cairo_context[context_index(psa->wks_id)],
            (psa->attributes.char_ht * psa->dspace.yspan / .728));
    cairo_get_font_matrix(cairo_context[context_index(psa->wks_id)], &fmatrix);

    /*
     *  Get height of capital X.
     */
    single_char[0] = 'X';
    single_char[1] = 0;
    cairo_text_extents(cairo_context[context_index(psa->wks_id)], single_char,
            &textents);

    X_height = textents.height;
    cairo_matrix_scale(&fmatrix, 1., -1.);
    cairo_set_font_matrix(cairo_context[context_index(psa->wks_id)], &fmatrix);
    cairo_text_extents(cairo_context[context_index(psa->wks_id)], sptr,
            &textents);

    slen = (int) strlen(sptr);

    /*
     *  Character color.
     */
    cval = unpack_argb((psa->ctable)[psa->attributes.text_colr_ind]);
    cairo_set_source_rgba(cairo_context[context_index(psa->wks_id)], cval.red,
            cval.green, cval.blue, cval.alpha);

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

    cairo_text_extents(cairo_context[context_index(psa->wks_id)], sptr,
            &textents);
    horiz_len = textents.width;
    /*
     *  The text extents returned from the above include white space
     *  at the left of the first character and the right of the last
     *  character.  GKS wants to align at character edges, so we subtract
     *  off the small additional white spaces.
     */
    single_char[0] = *sptr;
    single_char[1] = 0;
    cairo_text_extents(cairo_context[context_index(psa->wks_id)], single_char,
            &textents);
    left_space = textents.x_bearing;
    single_char[0] = *(sptr + strlen(sptr) - 1);
    single_char[1] = 0;
    cairo_text_extents(cairo_context[context_index(psa->wks_id)], single_char,
            &textents);
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

    cairo_save(cairo_context[context_index(psa->wks_id)]);
    cairo_move_to(cairo_context[context_index(psa->wks_id)], xpos, ypos);
    cairo_rotate(cairo_context[context_index(psa->wks_id)], -cang);

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
        cairo_rel_move_to(cairo_context[context_index(psa->wks_id)], x_del,
                y_del);
        cairo_show_text(cairo_context[context_index(psa->wks_id)], sptr);
    } else {
        /*
         *  Intercharacter spacing.
         */
        cspace = X_height * (psa->attributes.char_space);
        /*
         *  Effect the expansion factor.
         */
        if (psa->attributes.char_expan != CHAR_EXPAN_DEFAULT) {
            cairo_get_font_matrix(cairo_context[context_index(psa->wks_id)],
                    &fmatrix);
            scl_matrix = fmatrix;
            cairo_matrix_scale(&scl_matrix, psa->attributes.char_expan, 1.);
            cairo_set_font_matrix(cairo_context[context_index(psa->wks_id)],
                    &scl_matrix);
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
            cairo_text_extents(cairo_context[context_index(psa->wks_id)], sptr,
                    &textents);
            if (psa->attributes.text_align_horiz == CENTER_ALIGNMENT_HORIZ) {
                x_del = x_del * psa->attributes.char_expan; /* Scale current pos. */
                x_del = x_del - 0.5 * (slen - 1) * cspace;
            } else if (psa->attributes.text_align_horiz
                    == RIGHT_ALIGNMENT_HORIZ) {
                x_del = x_del * psa->attributes.char_expan;
                x_del = x_del - (slen - 1) * cspace;
            }
            cairo_rel_move_to(cairo_context[context_index(psa->wks_id)], x_del,
                    y_del);
            for (i = 0; i < strlen(sptr); i++) {
                single_char[0] = *(sptr + i);
                single_char[1] = 0;
                cairo_text_extents(cairo_context[context_index(psa->wks_id)],
                        single_char, &textents);
                cairo_show_text(cairo_context[context_index(psa->wks_id)],
                        single_char);
                cairo_rel_move_to(cairo_context[context_index(psa->wks_id)],
                        cspace, 0.);
            }
            break;
        case UP_TEXT_PATH:
            reverse_chrs(sptr); /* note fall through to DOWN_TEXT_PATH */
        case DOWN_TEXT_PATH:
            /*
             *  Get the text extents for the first character to use for centering.
             */
            cairo_save(cairo_context[context_index(psa->wks_id)]);
            single_char[0] = *(sptr);
            single_char[1] = 0;
            cairo_text_extents(cairo_context[context_index(psa->wks_id)],
                    single_char, &textents);
            /*
             *  Rotate the string.
             */
            cairo_rotate(cairo_context[context_index(psa->wks_id)], cang);
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
                cairo_text_extents(cairo_context[context_index(psa->wks_id)],
                        single_char, &textents);
                if (textents.width > maximum_width) {
                    maximum_width = MAX(maximum_width,textents.width);
                    char_num_mx = i;
                }
            }
            single_char[0] = *(sptr + char_num_mx);
            single_char[1] = 0;
            cairo_text_extents(cairo_context[context_index(psa->wks_id)],
                    single_char, &textents);

            /*
             *  Translate to the string start point.
             */
            cairo_translate(cairo_context[context_index(psa->wks_id)], (xc
                    * psa->dspace.xspan), (yc * psa->dspace.yspan));
            cairo_rotate(cairo_context[context_index(psa->wks_id)], -cang);
            /*
             *  Draw characters one at a time.
             */
            for (i = 0; i < strlen(sptr); i++) {
                single_char[0] = *(sptr + i);
                single_char[1] = 0;
                cairo_text_extents(cairo_context[context_index(psa->wks_id)],
                        single_char, &textents);

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
                cairo_text_extents(cairo_context[context_index(psa->wks_id)],
                        single_char, &textents);
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
                    cairo_rel_move_to(
                            cairo_context[context_index(psa->wks_id)],
                            x_del - textents.x_bearing - 0.5 * textents.width,
                            y_del);
                    cairo_show_text(cairo_context[context_index(psa->wks_id)],
                            single_char);
                    /*
                     *  Move back to the base horizontal position.
                     */
                    cairo_rel_move_to(
                            cairo_context[context_index(psa->wks_id)], (0.5
                                    * textents.width + textents.x_bearing
                                    - x_del) - textents.x_advance, 0.);
                } else {
                    cairo_text_extents(
                            cairo_context[context_index(psa->wks_id)],
                            single_char, &textents);
                    cairo_rel_move_to(
                            cairo_context[context_index(psa->wks_id)],
                            x_del - textents.x_bearing - 0.5 * textents.width,
                            -1.5 * X_height - cspace);
                    cairo_show_text(cairo_context[context_index(psa->wks_id)],
                            single_char);
                    cairo_rel_move_to(
                            cairo_context[context_index(psa->wks_id)], (0.5
                                    * textents.width + textents.x_bearing
                                    - x_del) - textents.x_advance, 0.);
                }
            }
            cairo_restore(cairo_context[context_index(psa->wks_id)]);
            break;
        }
    }
    cairo_restore(cairo_context[context_index(psa->wks_id)]);

    return (0);
}


int cro_UpdateWorkstation(GKSC *gksc) {

    CROddp *psa = (CROddp *) gksc->ddp;

    if (getenv("CRO_TRACE")) {
        printf("Got to cro_UpdateWorkstation\n");
    }
    cairo_surface_flush(cairo_surface[context_index(psa->wks_id)]);
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
    static const int maxName = 256;
    static char tname[257];
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
    	char* space = strchr(file_name, ' ');
    	if (!space)
    		return file_name;
        int len = (int)(space - file_name);
        len = (len > maxName) ? maxName : len;
        strncpy(tname, file_name, len);
        tname[len] = '\0';
        return tname;
    }

    /*
     *  Default
     */
    memset(tname, 0, 257);
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
 * A utility function to create filenames for formats that do no support multiple
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
    float rscale;

    if (getenv("CRO_TRACE")) {
        printf("Got to CROinit\n");
    }

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

#if 0 /* THIS IS NOT HANDED DOWN FROM HLU LAYER; coords+6 is undefined -- RLB*/
    /*
     *  Flag to suppress putting out background color rectangle.
     */
    psa->suppress_flag = *(coords+6);
#endif

    psa->background = FALSE;
    psa->pict_empty = TRUE;
    psa->page_number = 1;

    psa->attributes.linetype = LINETYPE_DEFAULT;
    psa->attributes.linetype_set = LINETYPE_DEFAULT;
    psa->attributes.linewidth = LINEWIDTH_DEFAULT;
    psa->attributes.linewidth_set = LINEWIDTH_DEFAULT;
    psa->attributes.line_colr_ind = LINE_COLR_DEFAULT;
    psa->attributes.marker_type = MARKER_TYPE_DEFAULT;
    psa->attributes.marker_size = MARKER_SIZE_DEFAULT;
    psa->attributes.marker_colr_ind = MARKER_COLR_IND_DEFAULT;
    psa->attributes.text_font = TEXT_FONT_DEFAULT;
    psa->attributes.text_prec = TEXT_PREC_DEFAULT;
    psa->attributes.char_expan = CHAR_EXPAN_DEFAULT;
    psa->attributes.char_space = CHAR_SPACE_DEFAULT;
    psa->attributes.text_colr_ind = TEXT_COLR_IND_DEFAULT;
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
    psa->attributes.cro_colr_ind = CRO_COLR_IND_DEFAULT;
    psa->attributes.clip_ind = CLIP_IND_DEFAULT;

    psa->image_width = DEFAULT_IMAGE_WIDTH;
    psa->image_height = DEFAULT_IMAGE_HEIGHT;
    psa->paper_width = PSPDF_PAGESIZE_X;
    psa->paper_height = PSPDF_PAGESIZE_Y;

    int paperWidth = *(coords+4);
    int paperHeight = *(coords+5);
    if (paperWidth != 0 && paperWidth != -9999 && paperHeight != 0 && paperHeight != -9999) {
        psa->paper_width = paperWidth;
        psa->paper_height = paperHeight;
    }

    /* apply any escapes */
    _NGCesc *cesc;
    _NGCPixConfig *pixc;
    while (cesc = _NGGetCEscInit()) {
        switch (cesc->type) {
        case NGC_PIXCONFIG:
            pixc = (_NGCPixConfig*) cesc;
            psa->image_width = pixc->width;
            psa->image_height = pixc->height;
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


struct color_value unpack_argb(unsigned int argb) {
    struct color_value cval;
    cval.blue = ((float) (argb & 255) / 255.);
    cval.green = ((float) ((argb >> 8) & 255) / 255.);
    cval.red = ((float) ((argb >> 16) & 255) / 255.);
    cval.alpha = ((float) ((argb >> 24) & 255) / 255.);
    return cval;
}


void setSurfaceTransform(CROddp *psa) {
    /*
     *  Translate and flip (since GKS origin is at bottom left) to center the plot.
     *  Since the final plot is to be flipped top to bottom, we want
     *  to translate in the Y direction to the original top.
     */

    double angle, tx, ty;
    /* Landscape is only supported for PS/PDF, not for image-based formats. */
    if ((psa->wks_type == CPS || psa->wks_type == CPDF) && psa->orientation == LANDSCAPE) {
        angle = PI / 2.0;
        tx = psa->dspace.lly;
        ty = -psa->dspace.llx;
    } else {
        angle = 0.;
        tx = psa->dspace.llx;
        ty = psa->dspace.ury;
    }

    double scale = 1.0;

    cairo_t* ctx = cairo_context[context_index(psa->wks_id)];
    cairo_identity_matrix(ctx);
    cairo_rotate(ctx, angle);
    cairo_translate(ctx, tx, ty);
    cairo_scale(ctx, scale, -scale);
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
    int ocounter, counter_inc = 3;
    CROPoint *points, opoint;

    CROddp *psa;
    if (getenv("CRO_TRACE")) {
        printf("Got to cro_SoftFill\n");
    }

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
    ocounter = 0;
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
                        cairo_move_to(cairo_context[context_index(psa->wks_id)],tmpx,tmpy);
                        opoint.x = (spi - yco * rst[jn2]) / xco;
                        opoint.y = rst[jn2];
                        tmpx = opoint.x * (float) psa->dspace.xspan;
                        tmpy = opoint.y * (float) psa->dspace.yspan;
                        cairo_line_to(cairo_context[context_index(psa->wks_id)],tmpx,tmpy);
                        cairo_stroke(cairo_context[context_index(psa->wks_id)]);
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
                        cairo_move_to(cairo_context[context_index(psa->wks_id)],tmpx,tmpy);
                        opoint.x = rst[jn2];
                        opoint.y = (spi - xco * rst[jn2]) / yco;
                        tmpx = opoint.x * (float) psa->dspace.xspan;
                        tmpy = opoint.y * (float) psa->dspace.yspan;
                        cairo_line_to(cairo_context[context_index(psa->wks_id)],tmpx,tmpy);
                        cairo_stroke(cairo_context[context_index(psa->wks_id)]);
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

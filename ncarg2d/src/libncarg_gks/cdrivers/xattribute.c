/*
 *      $Id: xattribute.c,v 1.15 2008-07-23 17:28:02 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *      File:           xattribute.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu May 16 15:42:15 MDT 1991
 *
 *      Description:    This file contains routines for handling gks output
 *                      attribute functions for the x device driver
 */
#include <stdio.h>
#include <stdlib.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include "x.h"
#include "x_device.h"
#include "xddi.h"
#include "xattribute.h"
#include "text.h"
#include "transform.h"

static  int
set_foreground_color
#ifdef  NeedFuncProto
(
        Display         *dpy,
        GC              gc,
        Pixeltype       *color_pal,
        unsigned        index
)
#else
(dpy,gc,color_pal,index)
        Display         *dpy;
        GC              gc;
        Pixeltype       *color_pal;
        unsigned        index;
#endif
{
        Pixeltype       pixel;

        if (index > (MAX_COLORS - 1)) {
                return(ERR_MAX_COLOR);
        }

        pixel = color_pal[index];
        XSetForeground(dpy, gc, pixel);
        return(0);
}

int
X11_SetLinetype
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;


        int             *iptr = (int *) gksc->i.list;
        int             line_type = iptr[0];

        int             status = 0;

        XGCValues       gcv;

        static  char    
                dashlist[] = {DASHSIZE,GAPSIZE};

        static  char    
                dotlist[] = {DOTSIZE,GAPSIZE};

        static  char    
                dashdotlist[] = {
                        DASHSIZE,GAPSIZE,DOTSIZE,GAPSIZE
                        };

        switch (line_type) {
        
        case    SOLID_LINE:
                gcv.line_style = LineSolid;
                break;

        case    DASHED_LINE:
                XSetDashes(dpy,xi->line_gc,0,dashlist,(int) (sizeof(dashlist)));
                gcv.line_style = LineOnOffDash;
                break;

        case    DOTTED_LINE:
                XSetDashes(dpy,xi->line_gc, 0, dotlist,(int) (sizeof(dotlist)));
                gcv.line_style = LineOnOffDash;
                break;

        case    DASH_DOT_LINE:
                XSetDashes(
                        dpy, xi->line_gc,0,dashdotlist,
                        (int) (sizeof(dashdotlist))
                        );
                gcv.line_style = LineOnOffDash;
                break;

        default:
                status = ERR_INV_LINE;
        
        }

        /* 
         * change line GC to reflect changes
         */
        XChangeGC(dpy, xi->line_gc, GCLineStyle, &gcv);

        return(status);
}

int
X11_SetLineWidthScaleFactor
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        float           *fptr = (float *) gksc->f.list;
        float           line_width = fptr[0];

        int             lw = RINT(line_width);
        unsigned long   mask;
        XGCValues       gcv;

        if (lw < 0) lw = 0;

        if (lw == 0) {
                gcv.function = GXnoop;
                mask = GCFunction;
        }
        else if (lw == 1) {
                gcv.line_width = 0;             /* draw fast 1 pixel lines */
                gcv.join_style = JoinMiter;
                gcv.function = GXcopy;
                mask = GCLineWidth | GCJoinStyle | GCFunction;
        }
        else {
                /*
                 * for fat lines change the join style to round instead of
                 * miter
                 */
                gcv.line_width = lw;
                gcv.join_style = JoinRound;
                gcv.function = GXcopy;
                mask = GCLineWidth | GCJoinStyle | GCFunction;
        }

        /*
         * change line GC to reflect changes
         */
        XChangeGC(dpy, xi->line_gc, mask, &gcv);

        return(0);
}


int
X11_SetPolylineColorIndex
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        int             *xptr = (int *) gksc->x.list;
        unsigned        index   = (unsigned) xptr[0];

        xi->line_index = index;
        return(set_foreground_color(dpy, xi->line_gc, xi->color_pal, index));
}

int
X11_SetMarkerType
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;
        int             marker_type = iptr[0];

        /*
         * no direct support for markers in X. Have to simulate them in 
         * software
         */
        xi->marker_type = marker_type;

        return(0);
}


int
X11_SetMarkerSizeScaleFactor
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;
        float           marker_size = fptr[0];

        /*
         * no direct support for markers in X. Have to simulate them in 
         * software
         */
        xi->marker_size = RINT(marker_size);

        return(0);
}

int
X11_SetPolymarkerColorIndex
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        int             *xptr = (int *) gksc->x.list;
        unsigned        index   = (unsigned) xptr[0];

        xi->marker_index = index;
        return(set_foreground_color(dpy, xi->marker_gc, xi->color_pal, index));
}


int
X11_SetTextFontAndPrecision
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        int             *iptr = (int *) gksc->i.list;
        int     font            = iptr[0];
        int     precision       = iptr[1];

        TextAttribute   ta;
        unsigned long   mask = 0;

        ta.text_font = font;
        ta.text_precision = precision;

        mask = (TEXT_FONT_SG | TEXT_PRECISION_SG);

        return(SetTextAttribute(&ta, mask));

}

int
X11_SetCharacterExpansionFactor
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        float           *fptr = (float *) gksc->f.list;

        float           exp_factor = fptr[0];

        TextAttribute   ta;
        unsigned long   mask = 0;

        ta.char_expan_factor = exp_factor;

        mask = (CHAR_EXPAN_FACTOR_SG);

        return(SetTextAttribute(&ta, mask));
}


int
X11_SetCharacterSpacing
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        float           spacing = fptr[0];

        TextAttribute   ta;
        unsigned long   mask = 0;

        ta.char_spacing = spacing;

        /*
         * spacing is expected to be in device coordinate space => convert
         * from NDC to DC
         */
        spacing *= ABS(xi->transform.x_scale);

        mask = (CHAR_SPACING_SG);

        return(SetTextAttribute(&ta, mask));
}

int
X11_SetTextColorIndex
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        int             *xptr = (int *) gksc->x.list;
        unsigned        index   = (unsigned) xptr[0];

        xi->text_index = index;
        return(set_foreground_color(dpy, xi->text_gc, xi->color_pal, index));
}


int
X11_SetCharacterHeightAndUpVector
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;

        float           *fptr = (float *) gksc->f.list;

        float           x_up = fptr[0];
        float           x_base = fptr[1];
        float           y_up = fptr[2];
        float           y_base = fptr[3];

        float           height = MAG(y_up, y_base);

        TextAttribute   ta;
        unsigned long   mask = 0;

        /*
         * height is expected to be in device coordinate space => convert
         * from NDC to DC
         */
        height *= ABS(xi->transform.y_scale);

        ta.orientation.x_up = x_up;
        ta.orientation.y_up = y_up;
        ta.orientation.x_base = x_base;
        ta.orientation.y_base = y_base;

        ta.char_height = height;

        mask = (CHAR_ORIENTATION_SG | CHAR_HEIGHT_SG);

        return(SetTextAttribute(&ta, mask));
}

int
X11_SetTextPath
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        int             *iptr = (int *) gksc->i.list;

        int     path    = iptr[0];

        TextAttribute   ta;
        unsigned long   mask = 0;

        ta.text_path = path;

        mask = (TEXT_PATH_SG);

        return(SetTextAttribute(&ta, mask));
}


int
X11_SetTextAlignment
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        int             *iptr = (int *) gksc->i.list;

        int     horiz_align     = iptr[0];
        int     vert_align      = iptr[1];

        TextAttribute   ta;
        unsigned long   mask = 0;

        ta.text_alignment.horizontal = horiz_align;
        ta.text_alignment.vertical = vert_align;

        mask = (TEXT_ALIGNMENT_SG);

        return(SetTextAttribute(&ta, mask));
}

int
X11_SetFillAreaInteriorStyle
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        int     fill_style      = iptr[0];

        xi->fill_style = fill_style;

        return(0);
}


int
X11_SetFillAreaStyleIndex
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;

        int             *iptr = (int *) gksc->i.list;

        int     hatch_index     = iptr[0];

        xi->hatch_index = hatch_index;

        return(0);
}

int
X11_SetFillAreaColorIndex
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        int             *xptr = (int *) gksc->x.list;
        unsigned        index   = (unsigned) xptr[0];

        xi->fill_index = index;
        return(set_foreground_color(dpy, xi->fill_gc, xi->color_pal, index));

}

/*
 * Function:    update_gcs
 *
 * Description: This function checks the GC's and determines if any of them
 *              were using a color that should be updated.
 *
 * In Args:     
 *
 * Out Args:    
 *
 * Scope:       
 * Returns:     
 * Side Effect: 
 */
static void
update_gcs
#ifdef  NeedFuncProto
(
        Xddp            *xi,
        int             index
)
#else
(xi,index)
        Xddp            *xi;
        int             index;
#endif
{
        XGCValues       gcv;            /* struc for manip. a GC*/
        /* 
         * Reset the GC's that were set using the index that is changing.
         */
        gcv.background = xi->color_pal[0];
        gcv.foreground = xi->color_pal[index];

        if((index == 0) || (index == xi->line_index))
                XChangeGC(xi->dpy,xi->line_gc,(GCForeground|GCBackground),&gcv);
        if((index == 0) || (index == xi->fill_index))
                XChangeGC(xi->dpy,xi->fill_gc,(GCForeground|GCBackground),&gcv);
        if((index == 0) || (index == xi->marker_index))
                XChangeGC(xi->dpy,xi->marker_gc,(GCForeground|GCBackground),
                                                                        &gcv);
        if((index == 0) || (index == xi->cell_index))
                XChangeGC(xi->dpy,xi->cell_gc,(GCForeground|GCBackground),&gcv);
        if((index == 0) || (index == xi->text_index))
                XChangeGC(xi->dpy,xi->text_gc,(GCForeground|GCBackground),&gcv);

        gcv.background = xi->color_pal[index];
        gcv.foreground = xi->color_pal[0];
        if((index == 0) || (index == xi->bg_index))
                XChangeGC(xi->dpy,xi->bg_gc,(GCForeground|GCBackground),&gcv);

        return;
}

void
X11_free_ci(
        Xddp            *xi,
        unsigned        index
)
{
        int             *color_info = xi->color_info;
        XddpColorStatus *color_status = xi->color_status;
        int             *color_def = xi->color_def;

        if(color_info[index] > -1){
                color_status[color_info[index]].ref_count--;

                if(color_status[color_info[index]].ref_count < 1){
                        if(xi->free_colors){
                                (*xi->free_colors)(xi->cref,
                                &color_status[color_info[index]].xpixnum,1);
                        }
                        else if(xi->cmap_ro){
                                XFreeColors(xi->dpy,xi->cmap,
                                &color_status[color_info[index]].xpixnum,1,0);
                        }
                        if(xi->x_ref_count)
                                color_def[color_status[
                                                color_info[index]].xpixnum]--;
                }
                color_info[index] = -1;
        }
}

int
X11_SetColorRepresentation
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp            *xi = (Xddp *) gksc->ddp;
        Display         *dpy = xi->dpy;
        Window          win = xi->win;
        Colormap        cmap = xi->cmap;

        int             *xptr = (int *) gksc->x.list;
        XColor          *rgbptr = (XColor *) gksc->rgb.list;
        XColor          tcolor = rgbptr[0];

        unsigned        index   = (unsigned) xptr[0];
        Pixeltype       *color_pal = xi->color_pal;
        int             *color_info = xi->color_info;
        XddpColorStatus *color_status = xi->color_status;
        int             *color_def = xi->color_def;
        float           color_error = 0;
        int             i,xindx;
        

        if (index > (MAX_COLORS - 1)) {
                return(ERR_MAX_COLOR);
        }

        if (! xi->color_ava) return(0); /* not a color device   */

        /*
         * If this index has a color allocated for it, free it up
         */
        X11_free_ci(xi,index);

        if(xi->alloc_color){
                /*
                 * This function is not allowed to fail to get a color.
                 * If it can't allocate a new color, it needs to return
                 * the next best value.
                 */
                (*xi->alloc_color)(xi->cref,&rgbptr[0]);
                /*
                 * Color Allocation succeded - place all the color information
                 * in the color_pal, color_info and color_status arrays.
                 */

                for(i=0;i < MAX_COLORS;i++)
                        if(color_status[i].ref_count == 0)
                                break;
                color_info[index] = i;
                color_status[i].ref_count = 1;
                color_status[i].red = rgbptr->red;
                color_status[i].green = rgbptr->green;
                color_status[i].blue = rgbptr->blue;
                color_status[i].xpixnum = color_pal[index] = rgbptr->pixel;
        }
        else if(xi->mycmap && !xi->cmap_ro && xi->x_ref_count){
                Boolean xindx_found = False;

                /*
                 * Find open color_status index (i)
                 */
                for(i=0;i < MAX_COLORS;i++)
                        if(color_status[i].ref_count == 0)
                                break;
                /*
                 * Find open X pixel (xindx)
                 */
                for(xindx=xi->max_x_colors-1;xindx>=0;xindx--){
                        if(!color_def[xindx]){
                                xindx_found = True;
                                break;
                        }
                }
                if(!xindx_found) goto TROUBLE;
                color_info[index] = i;
                rgbptr->pixel = xindx;
                color_status[i].xpixnum = color_pal[index] = xindx;
                XStoreColor(xi->dpy,xi->cmap,&rgbptr[0]);
                color_status[i].ref_count = 1;
                color_status[i].red = rgbptr->red;
                color_status[i].green = rgbptr->green;
                color_status[i].blue = rgbptr->blue;
                color_def[xindx]++;
        }
        else if(xi->cmap_ro && XAllocColor(dpy, cmap, &rgbptr[0])){
                /*
                 * Color Allocation succeded - place all the color information
                 * in the color_pal, color_info and color_status arrays.
                 */

                for(i=0;i < MAX_COLORS;i++)
                        if(color_status[i].ref_count == 0)
                                break;
                color_info[index] = i;
                color_status[i].ref_count = 1;
                color_status[i].red = rgbptr->red;
                color_status[i].green = rgbptr->green;
                color_status[i].blue = rgbptr->blue;
                color_status[i].xpixnum = color_pal[index] = rgbptr->pixel;
                if(xi->x_ref_count) color_def[rgbptr->pixel]++;
        }
        else if(xi->color_model == CM_MIXED){
                /*
                 * Color Fault - switch to a private colormap, and retry.
                 */
                X11_private_color(xi);
                return X11_SetColorRepresentation(gksc);
        }
        else{
                /*
                 * unable to allocate a new color cell.
                 * Dither to one of our current colors.
                 */
                int     minindx;
                float   minval;
                float   curval;
                float   tfloat;
                int     j;
TROUBLE:
                minindx = -1;
                minval = 0;

                for(j=0;j<MAX_COLORS;j++){
                        if(color_status[j].ref_count > 0){
                                tfloat = tcolor.red;
                                tfloat -= color_status[j].red;
                                curval = (tfloat * tfloat);
                                tfloat = tcolor.green;
                                tfloat -= color_status[j].green;
                                curval += (tfloat * tfloat);
                                tfloat = tcolor.blue;
                                tfloat -= color_status[j].blue;
                                curval += (tfloat * tfloat);

                                /*SUPPRESS766*/
                                if((minindx == -1)||(curval < minval)){
                                        minval = curval;
                                        minindx = j;
                                }
                        }
                }

                if(minindx > -1){
                        /*
                         * This is the color to use.
                         */
                        color_info[index] = minindx;
                        color_pal[index] =color_status[minindx].xpixnum;
                        color_status[minindx].ref_count++;
                        color_error = minval;
                }
                else{
                        /*
                         * We don't own any colors?!?!
                         */
                        color_pal[index] = WhitePixelOfScreen(xi->scr);
                        color_info[index] = -1;
                        tfloat = tcolor.red;
                        tfloat -= MAX_INTENSITY;
                        color_error = (tfloat * tfloat);
                        tfloat = tcolor.green;
                        tfloat -= MAX_INTENSITY;
                        color_error += (tfloat * tfloat);
                        tfloat = tcolor.blue;
                        tfloat -= MAX_INTENSITY;
                        color_error += (tfloat * tfloat);
                }
        }

        /*
         * if index == 0 then change background color
         */
        if (index == 0) {
                XSetWindowBackground(dpy, win, color_pal[index]);
                XClearWindow(dpy, win);
        }

        /*
         * update the GC's to the new color.
         */
        update_gcs(xi,index);

        if(xi->percent_colerr){
                float   local;
                int     tint = color_info[index];

                if(color_error != 0.0){
                        if(tint > -1){
                        local = tcolor.red;
                        local -= color_status[tint].red;
                        color_error = local * local;
                        local = tcolor.green;
                        local -= color_status[tint].green;
                        color_error += (local * local);
                        local = tcolor.blue;
                        local -= color_status[tint].blue;
                        color_error += (local * local);
                        }
                        else{
                        /*
                         * if tint is -1, we are using WhitePixel.
                         */
                        local = tcolor.red;
                        local -= 1.0;
                        color_error = local * local;
                        local = tcolor.green;
                        local -= 1.0;
                        color_error += (local * local);
                        local = tcolor.blue;
                        local -= 1.0;
                        color_error += (local * local);
                        }
                }

                if(color_error > xi->pcerr_sqr)
                        return ERR_DIFF_COLOR;
        }

        return 0;
}

int
X11_SetClipIndicator
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp    *xi = (Xddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        int     *iptr = (int *) gksc->i.list;
        XPoint  *pptr = (XPoint *) gksc->p.list;

        int     clip_flag = iptr[0];
        XPoint  *llptr = &pptr[0];
        XPoint  *urptr = &pptr[1];

        XRectangle rect;

        if (clip_flag) {

                /*
                 * turn clipping on
                 */

                /*
                 * X has origin at upper left, NDC space has origin at
                 * lower left
                 */
                rect.x = llptr->x;
                rect.y = urptr->y;
                rect.width = urptr->x - llptr->x + 1;
                rect.height = llptr->y - urptr->y + 1;

                XSetClipRectangles(dpy, xi->line_gc, 0, 0, &rect, 1, Unsorted);
                XSetClipRectangles(dpy, xi->marker_gc, 0, 0, &rect, 1,Unsorted);
                XSetClipRectangles(dpy, xi->text_gc, 0, 0, &rect, 1, Unsorted);
                XSetClipRectangles(dpy, xi->fill_gc, 0, 0, &rect, 1, Unsorted);
                XSetClipRectangles(dpy, xi->cell_gc, 0, 0, &rect, 1, Unsorted);
                XSetClipRectangles(dpy, xi->bg_gc, 0, 0, &rect, 1, Unsorted);

        }
        else {
                /*
                 * turn clipping off
                 */
                XSetClipMask(dpy, xi->line_gc, (Pixmap) None);
                XSetClipMask(dpy, xi->marker_gc, (Pixmap) None);
                XSetClipMask(dpy, xi->text_gc, (Pixmap) None);
                XSetClipMask(dpy, xi->fill_gc, (Pixmap) None);
                XSetClipMask(dpy, xi->cell_gc, (Pixmap) None);
                XSetClipMask(dpy, xi->bg_gc, (Pixmap) None);
        }

        return(0);
}


int
X11_SetWindow
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp            *xi = (Xddp *) gksc->ddp;
        float           *fptr = (float *) gksc->f.list;

        float           llx = fptr[0];
        float           urx = fptr[1];
        float           lly = fptr[2];
        float           ury = fptr[3];

        if ((llx == urx) || (lly == ury)) {
                return (ERR_INV_RECT);
        }

        TransformSetWindow(&xi->tsystem, llx, lly, urx, ury);
        xi->transform = TransformGetTransform(&xi->tsystem);

        return(0);
}

int
X11_SetViewport
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp            *xi = (Xddp *) gksc->ddp;
        float           *fptr = (float *) gksc->f.list;

        float           llx = fptr[0];
        float           urx = fptr[1];
        float           lly = fptr[2];
        float           ury = fptr[3];

        if ((llx == urx) || (lly == ury)) {
                return (ERR_INV_RECT);
        }

        TransformSetViewport(&xi->tsystem, llx, lly, urx, ury);
        xi->transform = TransformGetTransform(&xi->tsystem);

        return(0);
}

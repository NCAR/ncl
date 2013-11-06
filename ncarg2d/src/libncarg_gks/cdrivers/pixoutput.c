/*
 *      $Id: pixoutput.c,v 1.3 2008-07-23 17:28:01 haley Exp $
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
 *      File:           pixoutput.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu May 16 15:54:55 MDT 1991
 *
 *      Description:    This file contains routines for handling gks output
 *                      functions for the x device driver
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <ncarg/c.h>
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include "x.h"
#include "pix_device.h"
#include "pixddi.h"
#include "text.h"

/*      
 * requested dimensions for fill tile. 
 */
#define PWIDTH          8
#define PHEIGHT         8
/*
 *      hatch_fill
 *      [internal]
 *
 *      Simulate hatch filled polygons with tiles.
 *
 * on entry
 *      *dpy            : the display
 *      win             : window to render to
 *      *pptr           : vertice list for polygon
 *      n               : size of pptr
 *      hatch_index     : hatch style to simulate
 *      fill_gc         : gc to use for filled polygons
 *      bg_gc           : gc for drawing in background
 *
 * on exit
 *      return          : 0 => error, else gks error returned
 */
static  int hatch_fill
#ifdef  NeedFuncProto
(
        Display         *dpy,
        Drawable          win,
        XPoint          *pptr,
        int             n,
        int             hatch_index,
        GC              fill_gc,
        GC              *hatch_gc,
        unsigned int    depth
)
#else
(dpy,win,pptr,n,hatch_index,fill_gc,bg_gc,depth)
        Display         *dpy;
        Drawable          win;
        XPoint          *pptr;
        int             n;
        int             hatch_index;
        GC              fill_gc,
                        *hatch_gc;
        unsigned int    depth;
#endif
{

        static  struct  {       /* a pixmap for tileing a filled polygon*/
                XPoint  P[4];
                Pixmap  tileid;
                int     width,
                height;
                } tile = { {{0,0}, {0,0}, {0,0}, {0,0}}, 0, 0, 0 };
 	               
        static  Boolean first = TRUE;

        int     status = 0;

        if (first) {
                XQueryBestStipple(dpy, win, PWIDTH, PHEIGHT,
                (unsigned int *) &tile.width, (unsigned int *) &tile.height);

                first = FALSE;

        }


        /*      
         *      Create pixmap for stipple.
         *      This needs to be done for each invocation of
         *      the polygon routine because once a stipple is
         *      set in a GC, X does not allow it to be changed.
         */
        if((tile.tileid = XCreatePixmap(dpy,win,tile.width,tile.height,
                                                                1)) == 0){

                ESprintf(ERR_CRT_PIXMAP, "XCreatePixmap(,,,,)");
                return(ERR_CRT_PIXMAP);
        }

        if(!*hatch_gc){
                *hatch_gc = XCreateGC(dpy,tile.tileid,0,NULL);
                if(!*hatch_gc){
                        ESprintf(ERR_INTRNL_MEMORY, "XCreateGC(,,,)");
                        XFreePixmap(dpy, tile.tileid);
                        return ERR_INTRNL_MEMORY;
                }
        }

        tile.P[1].x = tile.P[2].x = tile.width;
        tile.P[2].y = tile.P[3].y = tile.height;


        /*      
         * clear the stipple
         */
        XSetFunction(dpy,*hatch_gc,GXclear);
        XFillPolygon(dpy,tile.tileid,*hatch_gc,tile.P,4,Convex,CoordModeOrigin);

        XSetFunction(dpy,*hatch_gc,GXset);

        switch (hatch_index) {

        default:
                /* fall through */

        case HORIZONTAL_HATCH:

                /* draw new pattern     */
                XDrawLine(dpy, tile.tileid, *hatch_gc, (int) 0,
                        tile.height-1, tile.width, tile.height-1);

                break;

        case VERTICAL_HATCH:
                XDrawLine(dpy, tile.tileid, *hatch_gc, tile.width-1,
                                0, tile.width-1, tile.height);

                break;

        case POSITIVE_HATCH:
                XDrawLine(dpy,tile.tileid,*hatch_gc,
                                tile.width-1,0,0,tile.height-1);
                break;

        case NEGATIVE_HATCH:
                XDrawLine(dpy, tile.tileid, *hatch_gc,0,0,tile.width,tile.height);
                break;

        case HORIZ_VERT_HATCH:
                XDrawLine(dpy, tile.tileid, *hatch_gc, 
                        0, tile.height-1, tile.width, tile.height-1);

                XDrawLine(dpy, tile.tileid, *hatch_gc, 
                        tile.width-1,0, tile.width-1, tile.height);
                break;

        case POS_NEG_HATCH:
                XDrawLine(dpy, tile.tileid, *hatch_gc,tile.width-1,0,0,tile.height-1);
                XDrawLine(dpy, tile.tileid, *hatch_gc,0,0,tile.width,tile.height);
                break;

        }

        /* 
         * set the GC stipple parameter to our stipple 
         */
        XSetStipple(dpy, fill_gc, tile.tileid);

        /* 
         * change the fill style to use the tile 
         */
        XSetFillStyle(dpy, fill_gc, FillStippled);

        XFillPolygon(dpy, win, fill_gc, pptr, n, Complex, CoordModeOrigin);
                        
        /* 
         * set fill style back to normal
         */
        XSetFillStyle(dpy, fill_gc, FillSolid);

        /*
         * free the tile
         */
        XFreePixmap(dpy, tile.tileid);

        return(status);
}

/*
 *      text_strokes
 *      [internal]
 *
 *      This function is passed to Text_() to be used for stroking characters
 *      for a particular font.
 * 
 * on entry
 *      *x              : array of x coordinates in DC space
 *      *y              : array of y coordinates in DC space
 *      num             : number of coordinates
 *      lines_data      : pointer to PIXddp for this invocation of PIX_Text()
 */
static  void    text_strokes(x, y, num, lines_data)
        int             *x, *y;
        unsigned        num;
        Voidptr         lines_data;
{

        PIXddp    *xi = (PIXddp *) lines_data;
        Display *dpy = xi->dpy;

        static  XPoint  *pptr = (XPoint *) NULL;
        static  unsigned        point_buf_size = 0;

        int     i;

        if (point_buf_size < num) {
                point_buf_size = num + 10;
                pptr = (XPoint *) malloc (point_buf_size * sizeof(XPoint));
                if (! pptr) {
                        point_buf_size = 0;
                        return;
                }
        }

        
        for(i=0; i<num; i++) {
                pptr[i].x = x[i];
                /*
                 * Text routine coordinate origin is at lower left corner
                 * X window coordinate origin is at upper left => transform 
                 * back to upper left;
                 */
                pptr[i].y = -(y[i]);
        }

        XDrawLines(dpy, xi->pix, xi->text_gc, pptr, num, CoordModeOrigin);
}

/*ARGSUSED*/
static  int non_rect_cell_array(dpy, win, gc, color_pal, P, Q, R, nx, ny, xptr)
        Display *dpy;
        Drawable  win;
        GC      gc;
        Pixeltype       *color_pal;
        XPoint  P, Q, R;
        int     nx, ny;
        int     *xptr;
{
        return(0);      /* non rectangular cell arrays are not supported */
}


/*
 * macro for copying a pixel from a pixel table into a character array
 */
#define PUT_PIX(pal, pal_ind, dst, size)        \
        {                                       \
        int     i;                              \
        char    *s, *d;                         \
        s = (char *) &(pal)[(pal_ind)];         \
        d = (char *) dst;                       \
        for (i=0; i<size; i++) {                \
                *d++ = *s++;                    \
        }                                       \
        }
                



/*
 *      set_up_indexing
 *      [internal]
 *
 *      Perform interpolation of Cell array data necessitated by imperfect
 *      mapping of cell array cells onto device pixels.
 *
 * on entry
 *      image_height    : height of image in pixels
 *      image_width     : width of image in pixels
 *      *rows           : Has dimension ny
 *      *cols           : Has dimension nx
 *      nx              : number of cells per row
 *      ny              : number of cells per column
 * on exit
 *      *rows           : row[i] specifies number of pixels for a cell in row i
 *      *cols           : col[i] specifies number of pixels for a cell in col i
 */
static  void    set_up_indexing(image_width, image_height, rows, cols, nx, ny)
        unsigned int    image_width,
                        image_height;
        int             *rows, 
                        *cols;
        unsigned        nx, ny;
{
        int     i;
        int     left, right;
        double  inc, tmp;

        /*
         * map cell array onto available pixels. Use current IEEE
         * rounding rules to determine whether a cell boundry includes
         * a boundry pixel or not. rows[i] and cols[j] contain
         * the number of pixels that make up cell[i,j] 
         */
        inc = (double) image_width / (double) nx;
        for( right = 0, tmp = 0.0,i = 0; i < nx; i++) { /* map cols*/
                left = right;
                tmp += inc;
                right =  (int) RINT(tmp);
                cols[i] = right - left;
        }

        inc = (double) image_height / (double) ny;
        for( right = 0, tmp = 0.0,i = 0; i < ny; i++) { /* map rows*/
                left = right;
                tmp += inc;
                right =  (int) RINT(tmp);
                rows[i] = right - left;
        }
}



/*
 *      set_up_address
 *      [internal]
 *
 *      calulate addressing information for manipulating ximage data for
 *      rendering a cell array. This function assumes the corner points
 *      P,Q,R are as described in the ANSI standard for CGM.
 *
 * on entry
 *      P,Q,R           : cell array corners
 *      image_size      : size of image data in bytes
 *      pad             : number of bytes of padding at end of a scan line
 *      bpp             : size of pixel in bytes
 *      bpl             : number of bytes per line in the ximage
 * on exit
 *      *step_x         : increment in x direction
 *      *step_y         : increment in y direction
 *      *start_x        : x destination of ximage in drawable
 *      *start_y        : y destination of ximage in drawable
 *      *data           : initialized to proper starting address
 */
static  void    set_up_address(P, Q, R, image_size, pad, bpp, 
                        bpl, step_x, step_y, start_x, start_y, data)
        XPoint          P, Q, R;
        unsigned int    image_size,
                        pad,
                        bpp,
                        bpl;
        int             *step_x,
                        *step_y;
        int             *start_x,
                        *start_y;
        char            **data;
{

        /*
         * determine addressing information based on relative positions of 
         * P, Q and R.
         */

        if (P.x < R.x) {
                *start_x = P.x;
                *step_x = bpp;
                if (R.y < Q.y) {
                        *start_y = R.y;
                        *step_y = bpl;
                        *data += 0;
                }
                else {
                        *start_y = Q.y;
                        *step_y = -bpl;
                        *data += (image_size - bpl);
                }
        }
        else {
                *start_x = R.x;
                *step_x = -bpp;
                if (R.y < Q.y) {
                        *start_y = R.y;
                        *step_y = bpl;
                        *data += (bpl - pad - bpp);
                }
                else {
                        *start_y = Q.y;
                        *step_y = -bpl;
                        *data += (image_size - (bpp + pad));
                }
        }
}

/*
 *      encode_pixels
 *      [internal]
 *
 *      encode the color palette into a form that is easier to access
 *      with PUT_PIX()
 *
 * on entry
 *      *src            : list of pixels
 *      n               : len of src
 *      pixel_size      : size of a single pixel
 *      byte_order      : byte order to encode for (LSBFirst | MSBFirst)
 * on exit
 *      *dst            : the encoded pixels
 */
static  void    encode_pixels(src, dst, n, pixel_size, byte_order)
        Pixeltype       *src, *dst;
        unsigned        n, 
                        pixel_size;
        int             byte_order;
{

        unsigned long   swaptest = 1;
        Boolean         swap = FALSE;
        unsigned char   *left, *right, c;

        int             i, j;


        /*
         * find out if we're on a byte swapped (LSBFirst) machine
         */
        if (((*(char *) &swaptest) && (byte_order != LSBFirst))
                || (!(*(char *) &swaptest) && (byte_order == LSBFirst))) {

                swap = TRUE;
        }


        /*
         * encode the pixel table
         */
        for (i=0; i<n; i++) {

                dst[i] = src[i];

                /*
                 * swap byte if needed
                 */
                if (swap) {
                        left = (unsigned char *) &dst[i];
                        right = left + sizeof (dst[i]) - 1;
                        while (left < right) {
                                c = *left;
                                *left++ = *right;
                                *right-- = c;
                        }
                }

                /*
                 * left shift data so first significant byte is the 
                 * first byte (only need to do this if byte order is 
                 * MSBFirst, else its already done)
                 */
                if (byte_order == MSBFirst) {
                        left = (unsigned char *) &dst[i];
                        right = left + sizeof (dst[i]) - pixel_size;
                        for (j=0; j<pixel_size; j++) {
                                *left++ = *right++;
                        }
                }
        }
}


/*
 *      cell_array
 *      [internal]
 *
 *      render a rectangular cell array
 *
 * on entry
 *      *dpy            : the display
 *      win             : window to render to
 *      gc              : graphics context to use for pixel transfer
 *      *color_pal      : array of X pixels
 *      P,Q,R           : corners of the cell array (See CGM standard)
 *      nx              : number of cells in x direction
 *      ny              : number of cells in y direction
 *      *xptr           : list of indexes into color_pal
 * on exit
 *      return          : 0 => Ok, else error
 */
static  int cell_array
#if     NeedFuncProto
(
        Display         *dpy,
        Visual          *visual,
        unsigned int    depth,
        Drawable          win,
        GC              gc,
        Pixeltype       *color_pal,
        XPoint          P,
        XPoint          Q,
        XPoint          R,
        int             nx,
        int             ny,
        int             *xptr
)
#else
(dpy,vis,depth,win,gc,color_pal,P,Q,R,nx,ny,xptr)
        Display         *dpy;
        Visual          *visual;
        unsigned int    depth;
        Drawable          win;
        GC              gc;
        Pixeltype       *color_pal;
        XPoint          P,
                        Q,
                        R;
        int             nx,
                        ny;
        int             *xptr;
#endif
{
        unsigned int    image_height,   /* image height in pixels       */
                        image_width,    /* image width in pixels        */
                        image_size,     /* size of image data in bytes  */
                        pad;            /* number of bytes of padding   */
        unsigned        pixel_size;     /* size of a single pixel       */
        char            *data,          /* image data                   */
                        *cptr;

        int             step_x,         /* step size for incrementing in
                                         * x direction within the image
                                         */
                        step_y;         /* step size for incrementing in
                                         * y direction within the image
                                         */

        int             start_x, 
                        start_y;        /* destination of image in drawable */

        int             *rows, 
                        *cols;          /* information about the number of
                                         * pixels making up a row (col) in
                                         * a the cell at row (col)[i]
                                         */
        int             *index_array,   /* color indeces for a cell row */
                        index;          /* color index for current cell */

        Pixeltype       pixels[MAX_COLORS];
        XImage          *ximage;        /* the X image                  */

        register int    i,j,k,l;

        image_width = ABS(P.x - Q.x) + 1;
        image_height = ABS(P.y - Q.y) + 1;

        /*
         * don't know how to handle a cell array with zero dimension
         */
        if (nx == 0 || ny == 0) {
                ESprintf(ERR_CELL_WIDTH, "Cell array has zero width or height");
                return(ERR_CELL_WIDTH);
        }

        if ((rows = (int *) malloc ((unsigned) ny * sizeof (int))) == NULL) {
                ESprintf(ERR_CELL_MEMORY, "malloc(%d)", ny * sizeof(int));
                return(ERR_CELL_MEMORY);
        }
        if ((cols = (int *) malloc ((unsigned) nx * sizeof (int))) == NULL) {
                ESprintf(ERR_CELL_MEMORY, "malloc(%d)", ny * sizeof(int));
                return(ERR_CELL_MEMORY);
        }
        if ((index_array = (int *) malloc ((unsigned) nx * sizeof (int)))
                                                                == NULL) {
                ESprintf(ERR_CELL_MEMORY, "malloc(%d)", nx * sizeof(int));
                return(ERR_CELL_MEMORY);
        }

        if ((ximage = XCreateImage(dpy, visual, depth, ZPixmap, 0, NULL,
                image_width, image_height, 32, 0)) == NULL) {

                ESprintf(ERR_CRT_IMAGE, "XCreateImage(,,,,,,)");
                return(ERR_CRT_IMAGE);
        }

        image_size = ximage->bytes_per_line * image_height;

        if ((ximage->data = malloc(image_size)) == NULL){
                ESprintf(ERR_IMAGE_MEMORY, "malloc(%d)", image_size);
                return(ERR_IMAGE_MEMORY);
        }
        data = ximage->data;
        pad = ximage->bytes_per_line - image_width;

        if (ximage->bits_per_pixel % 8) {
                ESprintf(ERR_PIXEL_SIZE, "Pixel size must be byte multiple");
                return(ERR_PIXEL_SIZE);
        }

        pixel_size = ximage->bits_per_pixel / 8;

 
        /*
         * encode the color palette into a form that is easier to access
         * with PUT_PIX()
         */
        encode_pixels(color_pal, pixels, MAX_COLORS, pixel_size, 
                        ximage->byte_order);

        /*
         * calculate x & y steping size, position of image in the window,
         * and starting address for data destination
         */
        set_up_address(P, Q, R, image_size, pad, pixel_size, 
                        ximage->bytes_per_line, &step_x, &step_y, 
                        &start_x, &start_y, &data);

        /*
         * set up rows and cols arrays with info about number of pixels
         * making up each cell. We do this to avoid floating point arithmatic
         * later on
         */
        set_up_indexing(image_width, image_height, rows, cols, nx, ny);


        /*
         * process the rows
         */
        for (i=0; i<ny; i++) {

                /* 
                 * load array of color indecies for row[i] of cells
                 */
                for (k=0; k<nx; k++, xptr++) {
                        index_array[k] = *xptr;
                }

                /*      
                 * the rows of pixels per cell[i]
                 */
                for (j=0; j < rows[i]; j++) {

                        cptr = data;
                        /*
                         * the coloumns
                         */
                        for (k=0; k<nx; k++) {


                                /*
                                 * the coloums of pixels per cell
                                 */
                                index = index_array[k];
                                for (l=0; l<cols[k]; l++) {
                                        PUT_PIX(pixels,index, cptr,pixel_size);
                                        cptr += step_x;
                                }
                        
                        }
                        data += step_y; /* skip to next row     */
                }
        }

        /*
         * copy image to the window
         */
        XPutImage(dpy, win, gc, ximage, 0, 0, start_x, start_y,
                                        image_width, image_height);



        XDestroyImage(ximage);  /* frees ximage->data too       */
        free((char *) rows);
        free((char *) cols);
        free((char *) index_array);

        return(0);
}
/*ARGSUSED*/
int PIX_Polyline(gksc)
        GKSC    *gksc;
{
        PIXddp    *xi = (PIXddp *) gksc->ddp;
        Display *dpy = xi->dpy;

        XPoint  *pptr = (XPoint *) gksc->p.list;
        int     n = gksc->p.num;

        XDrawLines(dpy, xi->pix, xi->line_gc, pptr, n, CoordModeOrigin);

        return(0);
}

/*ARGSUSED*/
int PIX_Polymarker(gksc)
        GKSC    *gksc;
{
        PIXddp    *xi = (PIXddp *) gksc->ddp;
        Display *dpy = xi->dpy;
        Drawable  win = xi->pix;

        int     n = gksc->p.num;
        XPoint  *pptr = (XPoint *) gksc->p.list;
        int     marker_type = xi->marker_type;
        float   marker_size = xi->marker_size;
        GC      marker_gc = xi->marker_gc;

        int     status = 0;

        int     offset;
        register int    i;

        offset = xi->dim * 0.005 * marker_size;

        switch (marker_type) {
        case X_MARKER:  
                for(i=0; i<n; i++) {
                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x - offset, pptr[i].y - offset,
                                pptr[i].x + offset, pptr[i].y + offset);

                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x - offset, pptr[i].y + offset,
                                pptr[i].x + offset, pptr[i].y - offset);
                }
                break;
        case CIRCLE_MARKER:
                for(i=0; i<n; i++) {
                        XDrawArc(dpy, win, marker_gc, 
                                pptr[i].x - offset, pptr[i].y - offset,
                                offset * 2, offset * 2,
                                (360 * 64), (360 * 64));
                }
                break;
        case STAR_MARKER:
                for(i=0; i<n; i++) {
                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x - offset, pptr[i].y,
                                pptr[i].x + offset, pptr[i].y);

                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x, pptr[i].y + offset,
                                pptr[i].x, pptr[i].y - offset);

                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x - offset, pptr[i].y - offset,
                                pptr[i].x + offset, pptr[i].y + offset);

                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x - offset, pptr[i].y + offset,
                                pptr[i].x + offset, pptr[i].y - offset);
                }
                break;
        case PLUS_MARKER:
                for(i=0; i<n; i++) {
                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x - offset, pptr[i].y,
                                pptr[i].x + offset, pptr[i].y);

                        XDrawLine(dpy, win, marker_gc,  
                                pptr[i].x, pptr[i].y + offset,
                                pptr[i].x, pptr[i].y - offset);
                }
                break;
        case DOT_MARKER:
                for(i=0; i<n; i++) 
                        XDrawPoint(dpy, win, marker_gc, 
                                pptr[i].x, pptr[i].y);
                break;
        default:
                /* unsupported polymarker type  */
                status = ERR_INV_MARKER;        
                break;
        }

        return(status);
}

/*ARGSUSED*/
int PIX_Text(gksc)
        GKSC    *gksc;
{
        PIXddp    *xi = (PIXddp *) gksc->ddp;

        XPoint          *pptr = (XPoint *) gksc->p.list;
        char            *sptr = (char *) gksc->s.list;

        int     x,y;

        x = pptr->x;
        /*
         * Text expect coordinate origin to be lower left corner. X window
         * coordinate origin is at upper left => transform to lower left
         */
        y = -(pptr->y);


        return(Text_(x, y, sptr, text_strokes, (Voidptr) xi));
}

/*ARGSUSED*/
int PIX_FillArea(gksc)
        GKSC    *gksc;
{
        PIXddp    *xi = (PIXddp *) gksc->ddp;
        Display *dpy = xi->dpy;
        Drawable  win = xi->win;

        int     n = gksc->p.num;
        XPoint  *pptr = (XPoint *) gksc->p.list;
        GC      gc = xi->fill_gc;

        int     fill_style = xi->fill_style;
        int     hatch_index = xi->hatch_index;

        int     status = 0;


        /*
         *      switch on interior style of fill area
         */
        switch (fill_style) {
                default:
                        /* fall through to HOLLOW_FILL  */

                case    HOLLOW_FILL:

                        /*      
                         * just draw a polyline 
                         */
                        XDrawLines(dpy, win, gc, pptr, n, CoordModeOrigin);
                        XDrawLines(dpy, xi->pix, gc, pptr, n, CoordModeOrigin);

                        /*
                         * make sure first and last point are coincident
                         */
                        if (pptr[0].x != pptr[n-1].x || 
                                pptr[0].y != pptr[n-1].y) {

                                /*
                                 * close the polygon
                                 */
                                XDrawLine(dpy, xi->pix, gc, 
                                        pptr[0].x, pptr[0].y,
                                        pptr[n-1].x, pptr[n-1].y);
                        }

                        break;

                case    SOLID_FILL:
                case    SOLID_TEXT_FILL:
                        XFillPolygon(dpy, xi->pix, gc, pptr, n, 
                                                Complex, CoordModeOrigin);
                        break;

                case    PATTERN_FILL:

                        /*
                         * code to invoke a pattern routine not supported
                         */
                        status = ERR_FILL_PAT;

                case    HATCH_FILL:

                        /*      
                         *      Hatch indecies are simulated using a fill
                         *      stipple for the area. See section 5.4.3.
                         */
                
                        status = hatch_fill(dpy,xi->pix,pptr,n,hatch_index,gc,
                                                &xi->hatch_gc,xi->depth);
                        break;

        }

        return(status);
}


/*ARGSUSED*/
int PIX_Cellarray(gksc)
        GKSC    *gksc;
{
        PIXddp    *xi = (PIXddp *) gksc->ddp;
        Display *dpy = xi->dpy;
        GC      gc = xi->cell_gc;
        Pixeltype *color_pal = xi->color_pal;

        XPoint  *pptr = (XPoint *) gksc->p.list;
        int             *iptr = (int *) gksc->i.list;
        int             *xptr = (int *) gksc->x.list;

        int             nx = iptr[0];   /* number of cols       */
        int             ny = iptr[1];   /* number of rows       */
        XPoint  *Pptr = &pptr[0];
        XPoint  *Qptr = &pptr[1];
        XPoint  *Rptr = &pptr[2];

        /*
         * see if cell array is rectangular or not
         */
        if (Rptr->x != Qptr->x || Pptr->y != Rptr->y) {
                return (non_rect_cell_array(dpy, xi->pix, gc, color_pal, 
					    *Pptr, 
					    *Qptr, *Rptr, nx, ny, xptr));
        }

        /*
         * cell array is a rectangluar
         */
        return(cell_array(dpy,xi->vis,xi->depth,xi->pix,gc,color_pal,
			  *Pptr,*Qptr,*Rptr,nx,ny,xptr));
                
}

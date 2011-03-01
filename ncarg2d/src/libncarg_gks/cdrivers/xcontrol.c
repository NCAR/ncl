/*
 *      $Id: xcontrol.c,v 1.27.8.1 2010-03-17 20:53:31 brownrig Exp $
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
 *      File:           xcontrol.c
 *
 *      Author:         John Clyne
 *                      National Center for Atmospheric Research
 *                      PO 3000, Boulder, Colorado
 *
 *      Date:           Thu May 16 15:44:37 MDT 1991
 *
 *      Description:    This file contains routines for handling gks control
 *                      functions for the x device driver
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <ncarg/c.h>
#include <ncarg/gks.h>
#include "gks_device.h"
#include "common.h"
#include "gksc.h"
#include "gks.h"
#include <ncarg/gksP.h>
#include "x.h"
#include "x_device.h"
#include "xddi.h"

extern void gerr_hand(Gint, Gint, const char *);

/*
 * Function:    X11_private_color
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
void
X11_private_color
#ifdef  NeedFuncProto
(
        Xddp            *xi
)
#else
(xi)
        Xddp            *xi;
#endif
{
        Colormap        newcmap;
        XColor          *colors;
        int             i;

        if(xi->mycmap || xi->alloc_color)
                return;

        /*
         * See if we have to use a RO/RW color model
         */
        switch(xi->vis->class){
                /*
                 * RO model
                 */
                case TrueColor:
                case StaticColor:
                case StaticGray:
                case DirectColor:

                xi->cmap = XCopyColormapAndFree(xi->dpy,xi->cmap);
                xi->cmap_ro = True;

                break;

                /*
                 * RW model
                 */
                default:

                newcmap = XCreateColormap(xi->dpy,xi->win,xi->vis,AllocAll);
                if((colors = (void *)malloc(sizeof(XColor)*xi->max_x_colors))){
                        for(i=0;i<xi->max_x_colors;i++)
                                colors[i].pixel = i;
                        XQueryColors(xi->dpy,xi->cmap,colors,xi->max_x_colors);
                        XStoreColors(xi->dpy,newcmap,colors,xi->max_x_colors);
                        free(colors);
                }
        
                XFreeColormap(xi->dpy,xi->cmap);
                xi->cmap = newcmap;
                xi->cmap_ro = False;
                break;
        }

        xi->mycmap = True;
        XSetWindowColormap(xi->dpy,xi->win,xi->cmap);

        return;
}

/*
 * Function:    init_color
 *
 * Description: initialize the color table and the color indirection table.
 *
 * In Args:     
 *              Xddp -  X device dependent pointer. The color tables are
 *                      initialized.
 *
 * Out Args:    
 *
 * Scope:       static
 * Returns:     void
 * Side Effect: 
 */
static  void
init_color
#ifdef  NeedFuncProto
(
        Xddp            *xi
)
#else
(xi)
        Xddp            *xi;
#endif
{
        int             i;
        int             fg_indx;
        XColor          tcolor;
        XColor          *colors;
        XGCValues       gcv;            /* struc for manip. a GC*/

        xi->max_x_colors = 2;
        for(i=1;i<xi->depth;i++)
                xi->max_x_colors *= 2;

        if(xi->alloc_color){
                xi->x_ref_count = False;
                xi->color_model = CM_SHARED;
        }
        else{
                switch(xi->vis->class){
                        case TrueColor:
                        case StaticColor:
                        case StaticGray:
                        case DirectColor:
                                xi->x_ref_count = False;
                                break;
                        default:
                                xi->x_ref_count = True;
                }
        }

        if(xi->x_ref_count){
                xi->color_def = (void *)malloc(sizeof(int)*xi->max_x_colors);
                if(xi->color_def)
                        memset((char *)xi->color_def,0,
                                                sizeof(int)*xi->max_x_colors);
                else{
                        xi->x_ref_count = False;
                        xi->color_def = NULL;
                }
        }
        else
                xi->color_def = NULL;

        switch(xi->color_model){

                default:
                case CM_UNDEFINED:

                if(xi->xwtype == XUSRWIN)
                        xi->color_model = CM_SHARED;
                else
                        xi->color_model = CM_MIXED;

                case CM_MIXED:
                case CM_SHARED:

                xi->mycmap = False;
                xi->cmap_ro = True;

                break;

                case CM_PRIVATE:
                xi->mycmap = True;

                /*
                 * See if we have to use a RO/RW color model
                 */
                switch(xi->vis->class){
                        Colormap        newcmap;
                        /*
                         * RO model
                         */
                        case TrueColor:
                        case StaticColor:
                        case StaticGray:
                        case DirectColor:

                        xi->cmap_ro = True;
                        xi->cmap = XCopyColormapAndFree(xi->dpy,xi->cmap);
                        break;

                        /*
                         * RW model
                         */
                        default:

                        newcmap = XCreateColormap(xi->dpy,xi->win,xi->vis,
                                                                AllocAll);
                        if((colors = (void *)malloc(sizeof(XColor)*xi->max_x_colors))){
                                for(i=0;i<xi->max_x_colors;i++)
                                        colors[i].pixel = i;
                                XQueryColors(xi->dpy,xi->cmap,colors,
                                                        xi->max_x_colors);
                                XStoreColors(xi->dpy,newcmap,colors,
                                                        xi->max_x_colors);
                                free(colors);
                        }
        
                        XFreeColormap(xi->dpy,xi->cmap);
                        xi->cmap = newcmap;
                        xi->cmap_ro = False;
                        break;
                }
                XSetWindowColormap(xi->dpy,xi->win,xi->cmap);
                break;
        }

        /*
         * init background(gks 0) to black and foreground(gks 1) to white.
         */
        tcolor.flags = (DoRed | DoGreen | DoBlue);
        tcolor.pad = '\0';

        /*
         * Background
         */
        tcolor.red = (unsigned short)0;
        tcolor.green = (unsigned short)0;
        tcolor.blue = (unsigned short)0;
        if(xi->alloc_color)
                (*xi->alloc_color)(xi->cref,&tcolor);
        else if(xi->cmap_ro)
                XAllocColor(xi->dpy, xi->cmap, &tcolor);
        else{
                tcolor.pixel = BlackPixelOfScreen(xi->scr);
                XStoreColor(xi->dpy,xi->cmap,&tcolor);
        }
        xi->color_info[0] = 0;
        xi->color_status[0].xpixnum = xi->color_pal[0] = tcolor.pixel;
        xi->color_status[0].ref_count = 1;
        xi->color_status[0].red = tcolor.red;
        xi->color_status[0].green = tcolor.green;
        xi->color_status[0].blue = tcolor.blue;
        if(xi->x_ref_count)
                xi->color_def[tcolor.pixel]++;

        /*
         * Foreground
         */
        tcolor.red = (unsigned short)MAX_INTENSITY;
        tcolor.green = (unsigned short)MAX_INTENSITY;
        tcolor.blue = (unsigned short)MAX_INTENSITY;
        if(xi->alloc_color)
                (*xi->alloc_color)(xi->cref,&tcolor);
        else if(xi->cmap_ro)
                XAllocColor(xi->dpy, xi->cmap, &tcolor);
        else{
                tcolor.pixel = WhitePixelOfScreen(xi->scr);
                XStoreColor(xi->dpy,xi->cmap,&tcolor);
        }
        xi->color_info[1] = 1;
        xi->color_status[1].xpixnum = xi->color_pal[1] = tcolor.pixel;
        xi->color_status[1].ref_count = 1;
        xi->color_status[1].red = tcolor.red;
        xi->color_status[1].green = tcolor.green;
        xi->color_status[1].blue = tcolor.blue;
        if(xi->x_ref_count)
                xi->color_def[tcolor.pixel]++;

        /*
         * Set all remaining colors in the gks colormap to the same color
         * as foreground gks_cmap(1).
         */
        fg_indx = xi->color_info[1];
        for (i=2; i<MAX_COLORS; i++) {
                xi->color_pal[i] = xi->color_status[fg_indx].xpixnum;
                xi->color_status[fg_indx].ref_count++;
                xi->color_info[i] = fg_indx;
                xi->color_status[i].ref_count = 0;
        }

        /* 
         * Create all the GC's so the graphics primatives use the
         * foreground color (gks 1) of the colormap, and background color
         * (gks 0).
         */
        gcv.background = xi->color_pal[0];
        gcv.foreground = xi->color_pal[1];

        xi->line_gc = XCreateGC(xi->dpy,xi->win,(GCForeground|GCBackground),
                                                                        &gcv);
        xi->fill_gc = XCreateGC(xi->dpy,xi->win,(GCForeground|GCBackground),
                                                                        &gcv);
        xi->marker_gc = XCreateGC(xi->dpy,xi->win,(GCForeground|GCBackground),
                                                                        &gcv);
        xi->cell_gc = XCreateGC(xi->dpy,xi->win,(GCForeground|GCBackground),
                                                                        &gcv);
        xi->text_gc = XCreateGC(xi->dpy,xi->win,(GCForeground|GCBackground),
                                                                        &gcv);

        /*
         * create a background gc (gc for drawing in background color)
         */
        gcv.background = xi->color_pal[1];
        gcv.foreground = xi->color_pal[0];
        xi->bg_gc = XCreateGC(xi->dpy,xi->win,(GCForeground|GCBackground),&gcv);

        /*
         * init hatch_gc to None.  If we use one, we need a depth one drawable
         * to create it.
         */
        xi->hatch_gc = None;

        /*
         * If the index in these vars change, then the corresponding GC
         * has to be updated.
         */
        xi->line_index = 1;
        xi->fill_index = 1;
        xi->marker_index = 1;
        xi->cell_index = 1;
        xi->text_index = 1;
        xi->bg_index = 1;

        return;
}

static void
free_all_colors
#ifdef  NeedFuncProto
(
        Xddp            *xi
)
#else
(xi)
        Xddp            *xi;
#endif
{
        int                             i,n=0;
        XddpColorStatus *color_status = xi->color_status;
        unsigned long   pixels[MAX_COLORS];

        if(!xi->free_colors && !xi->cmap_ro)
                return;

        for(i=0;i<MAX_COLORS;i++){
                if(color_status[i].ref_count > 0){
                        pixels[n++] = color_status[i].xpixnum;
                }
        }
        if(n>0){
                if(xi->free_colors)
                        (*xi->free_colors)(xi->cref,pixels,n);
                else if(xi->cmap_ro)
                        XFreeColors(xi->dpy,xi->cmap,pixels,n,0);
        }

        return;
}
        
static  void
pause
#ifdef  NeedFuncProto
(
        Display *dpy
)
#else
(dpy)
        Display *dpy;
#endif
{
        XEvent  event;

        /*
         * discard all events that a impatient user
         * may have aquired while waiting for a plot to finnish
         */
        while(XCheckMaskEvent(dpy,ButtonPressMask|KeyPressMask,&event));

        /*
         * wait for next buttonpress or keypress
         */
        XMaskEvent(dpy,ButtonPressMask|KeyPressMask,&event);

        return;
}

static Window
CreateXWorkWindow
#ifdef  NeedFuncProto
(
        Display         *dpy,
        _NGCXWinConfig  *xwc
)
#else
(dpy,xwc)
        Display         *dpy;
        _NGCXWinConfig  *xwc;
#endif
{
        Window                  win;
        static  XWMHints        xwmh = {
                (InputHint | StateHint ),/* flags                       */
                True,                   /* input                        */
                NormalState,            /* initial_state                */
                0,                      /* icon pixmap                  */
                0,                      /* icon window                  */
                0, 0,                   /* icon location                */
                0,                      /* icon mask                    */
                0                       /* Window group                 */
        };
        static  XClassHint      xch = {
                "xgks",                 /* resource name                */
                "Xgks"                  /* class name                   */
        };
        XSizeHints              xsh = { /* Size hints for window manager*/
                (PMinSize),
                0,0,                    /* obsolete ????                */
                DEFAULT_WIDTH,          /* obsolete ????                */
                DEFAULT_HEIGHT,         /* obsolete ????                */
                MIN_WIDTH, MIN_HEIGHT,  /* minimum usefull win dim      */      
                0,0,                    /* max dim (not used)           */
                0,0,                    /* not used                     */
                {0,0},                  /* not used                     */
                {0,0},                  /* not used                     */
                0,
                0,                      /* dimensions of window         */
                0
        };
        char                    *geometry=NULL;
        int                     geom_mask = 0;
        XSetWindowAttributes    xswa;   /* Set Window Attribute struct  */
        XTextProperty           window_name, icon_name;
        unsigned long           bw = 0; /* Border width                 */
        XEvent                   event; /* Event received               */
        Atom                    wm_del;

        /*
         * get the geometry resource string from the resource manager
         */
        if (!geometry) geometry = XGetDefault (dpy, xch.res_name, "geometry");
        if (!geometry) geometry = XGetDefault (dpy, xch.res_name, "Geometry");
        if (!geometry) geometry = XGetDefault (dpy, xch.res_class, "geometry");
        if (!geometry) geometry = XGetDefault (dpy, xch.res_class, "Geometry");

        if (geometry) {
                geom_mask = XParseGeometry (geometry, &xsh.x, &xsh.y,
                                (unsigned int *)&xsh.width,
                                (unsigned int *)&xsh.height);
        }

        /*
         * if xwc is set, it takes precedence over "geometry" resource.
         */
        if(xwc){
                if(xwc->x >= 0){
                        xsh.x = xwc->x;
                        geom_mask &= ~XNegative;
                        geom_mask |= XValue;
                }
                if(xwc->y >= 0){
                        xsh.y = xwc->y;
                        geom_mask &= ~YNegative;
                        geom_mask |= YValue;
                }
                if(xwc->width >= 0){
                        xsh.width = xwc->width;
                        geom_mask |= WidthValue;
                }
                if(xwc->height >= 0){
                        xsh.height = xwc->height;
                        geom_mask |= HeightValue;
                }
        }

        /*
         * see if user specified a window position. 
         */
        if ((geom_mask & XValue) || (geom_mask & YValue)) {
                xsh.flags |= USPosition;
        }

        /*
         * deal with negative position
         */
        if ((geom_mask & XValue) && (geom_mask & XNegative)) {
                xsh.x = DisplayWidth (dpy, DefaultScreen(dpy)) + xsh.x -
                xsh.width - bw * 2;
        }

        if ((geom_mask & YValue) && (geom_mask & YNegative)) {
                xsh.y = DisplayWidth (dpy, DefaultScreen(dpy)) + xsh.y -
                xsh.height - bw * 2;
        }


        /*
         * see if user specified a dimension, else we use program defaults
         */
        if ((geom_mask & WidthValue) || (geom_mask & HeightValue)) {
                xsh.flags |= USSize;
        }
        else {
                xsh.flags |= PSize;
        }

        /*
         * Ensure that the window's colormap field points to the default
         * colormap,  so that the window manager knows the correct 
         * colormap to use for the window.  
         */
        xswa.bit_gravity = CenterGravity;
        xswa.backing_store = WhenMapped;
        xswa.background_pixel = BlackPixel(dpy, DefaultScreen(dpy));
        xswa.border_pixel = BlackPixel(dpy, DefaultScreen(dpy));

        /* 
         * Create the Window with the information in the XSizeHints, the
         * border width, and the border & background pixels.
         */
        win = XCreateWindow(dpy, RootWindow(dpy,DefaultScreen(dpy)),
                xsh.x, xsh.y, xsh.width, xsh.height,
                bw,CopyFromParent,InputOutput,CopyFromParent,
                (CWBitGravity|CWBackingStore|CWBackPixel|CWBorderPixel),&xswa);

        /*
         * Set the standard properties for the window managers. 
         */
        window_name.encoding = XA_STRING;
        window_name.format = 8;
        if(xwc && xwc->title)
                window_name.value = (unsigned char *) xwc->title;
        else
                window_name.value = (unsigned char *) "NCAR Xgks";
        window_name.nitems = strlen ((char *)window_name.value);

        icon_name.encoding = XA_STRING;
        icon_name.format = 8;
        if(xwc && xwc->icon_title)
                icon_name.value = (unsigned char *) xwc->icon_title;
        else
                icon_name.value = (unsigned char *) "xgks";
        icon_name.nitems = strlen ((char *)icon_name.value);

        XSetWMProperties(dpy,win,&window_name,&icon_name,NULL,0,&xsh,&xwmh,
                                                                        &xch);

        /* 
         * Select notification of Expose event that is generated when
         * the window is first mapped (becomes visible) to the screen.
         */
        XSelectInput(dpy, win, ExposureMask);

        /*
         * Map the window to make it visible.
         */
        XMapWindow(dpy, win);

        /*
         *      get expose event as window becomes visible. we can't
         *      draw until after this 
         */
        while(1) {
                /* get next event       */
                XNextEvent(dpy, &event);

                /* 
                 * find the last expose event on the event queue.
                 */
                if (event.type == Expose && event.xexpose.count == 0) {

                        /*
                         * Remove any other pending Expose events from 
                         * the queue to avoid multiple repaints. 
                         */
                        /*SUPPRESS570*/
                        while (XCheckTypedEvent(dpy, Expose, &event));
                
                        break;
                }
        }

        /*
         * Select input for "pause" and destroy of window.
         */
        XSelectInput(dpy,win,
                        (ButtonPressMask|KeyPressMask|StructureNotifyMask));

        /*
         * Request clientMessage events for WM_DELETE_WINDOW.
         */

        wm_del = XInternAtom(dpy,"WM_DELETE_WINDOW",False);
        XSetWMProtocols(dpy,win,&wm_del,1);

        return win;
}

int
X11_Exec
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp                    *xi = (Xddp *) gksc->ddp;

#if 0
        XEvent                  event;
        XClientMessageEvent     *xcme;
#endif

        if((gksc->opcode == OPEN_WORKSTATION) ||
                        (gksc->opcode == CLOSE_WORKSTATION))
                goto DONE;

        if(xi->dead){
                ESprintf(ERR_WIN_ATTRIB,"Window Destroyed");
                return(ERR_WIN_ATTRIB);
        }
#if 0
        while(XCheckTypedEvent(xi->dpy,ClientMessage,&event) ||
                        XCheckMaskEvent(xi->dpy,StructureNotifyMask,&event)){

                switch (event.type) {
                case    DestroyNotify:
                        xi->dead = True;
                        break;

                case    ClientMessage:
                        xcme = (XClientMessageEvent*)&event;
                        xi->dead = True;
                        break;

                default:
                        break;
                }
                if(xi->dead)
                        break;
        }
        if(xi->dead){
                free_all_colors(xi);
                XFreeGC(xi->dpy,xi->line_gc);
                XFreeGC(xi->dpy,xi->fill_gc);
                XFreeGC(xi->dpy,xi->marker_gc);
                XFreeGC(xi->dpy,xi->cell_gc);
                XFreeGC(xi->dpy,xi->text_gc);
                XFreeGC(xi->dpy,xi->bg_gc);
                XCloseDisplay(xi->dpy);
                ESprintf(ERR_WIN_ATTRIB,"Window Destroyed");
                return(ERR_WIN_ATTRIB);
        }
#endif

DONE:

        return (*(gksc->operations[gksc->opcode]))(gksc);
}

int
X11_OpenWorkstation
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp                    *xi;
        static char             dpy_mem[MAX_DPY_LEN];
        static char             *dpy_name=NULL; /* Display name */
        XWindowAttributes       xwa;            /* Get Attributes       */
        CoordSpace              square_screen;
        int                     *iptr = (int *) gksc->i.list;
        _NGCesc                 *cesc;
        _NGCXWinConfig          *xwc=NULL;

        if((xi = (Xddp *) malloc (sizeof (Xddp))) == (Xddp *) NULL){
                ESprintf(ERR_DTABLE_MEMORY, "malloc(%d)", sizeof(Xddp));
                return(ERR_DTABLE_MEMORY);
        }

        switch(iptr[1]){
                case XREG:
                case XUSRWIN:
                        xi->xwtype = (XWorkType)iptr[1];
                        break;

                default:
                        (void)free(xi);
                        return ERR_INV_WK_TYPE;
        }

        gksc->ddp = (GKSC_Ptr) xi;

        xi->dead = False;
        xi->alloc_color = NULL;
        xi->free_colors = NULL;
        xi->cref = NULL;

        xi->size_change = NULL;
        xi->sref = NULL;

        while((cesc = _NGGetCEscInit())){
                _NGCXAllocColor *xac;
                switch(cesc->type){
                        case NGC_XALLOCCOLOR:
                                xac = (_NGCXAllocColor*)cesc;
                                xi->alloc_color = xac->xalloc_color;
                                xi->free_colors = xac->xfree_colors;
                                xi->cref = xac->cref;
                                break;
                        
                        case NGC_XWINCONFIG:
                                xwc = (_NGCXWinConfig*)cesc;

                                break;
                        default:
                                gerr_hand(182,11,NULL);
                }
        }

        /*
         * only get the DISPLAY env. var the first time we get called.
         */
        if(dpy_name == NULL){
                dpy_name = getenv("DISPLAY");

                if(dpy_name == NULL){
                        ESprintf(ERR_NO_DISPLAY,
                                "X11 \"DISPLAY\" env. variable not set");
                        return ERR_NO_DISPLAY;
                }

                strcpy(dpy_mem,dpy_name);
                dpy_name = dpy_mem;
        }

        if((xi->dpy = XOpenDisplay(dpy_name)) == NULL){
                ESprintf(ERR_OPN_DISPLAY, "  Error on opening X display (%s)", 
                        dpy_name);
                return(ERR_OPN_DISPLAY);
        }

        if(xi->xwtype == XUSRWIN){
                /*
                 * Window id is first element in iptr for type 7
                 */
                xi->win = (Window)iptr[0];
        }
        else{
                xi->win = CreateXWorkWindow(xi->dpy,xwc);
        }

        if(XGetWindowAttributes(xi->dpy,xi->win,&xwa) == 0){
                ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
                return ERR_WIN_ATTRIB;
        }

        if(xwc){
                xwc->x = xwa.x;
                xwc->y = xwa.y;
                xwc->width = xwa.width;
                xwc->height = xwa.height;
        }
        xi->scr = xwa.screen;
        xi->vis = xwa.visual;
        xi->cmap = xwa.colormap;
        xi->color_model = (XColModel)iptr[2];

        TransformSetWindow(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);
        TransformSetViewport(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);
        TransformSetNDScreenSpace(&xi->tsystem, 0.0, 0.0, 1.0, 1.0);

        square_screen = ComputeLargestSquare(
                (double) 0.0, (double) (xwa.height - 1),
                (double) (xwa.width - 1), (double) 0.0
        );

        TransformSetScreenSpace(
                &xi->tsystem, square_screen.llx, square_screen.lly, 
                square_screen.urx, square_screen.ury
        );

        xi->transform = TransformGetTransform(&xi->tsystem);
        xi->dim = square_screen.urx - square_screen.llx + 1;
        if(xi->size_change)
                (*xi->size_change)(xi->sref,xi->dim);

        /*
         * all output primitives will use Color_ava to see 
         * if they have a colour display
         */
        if (xwa.depth == 1)
                /* one plane monochrome display */
                xi->color_ava = FALSE;
        else
                xi->color_ava = TRUE;

        xi->depth = xwa.depth;

        init_color(xi);

        xi->marker_size = 1.0;

        xi->percent_colerr = DEF_COLOR_ERR;
        xi->pcerr_sqr = (float)DEF_COLOR_ERR*((float)MAX_INTEN_DIST/(float)100);
        xi->pcerr_sqr *= xi->pcerr_sqr;

        return(0);
}

int
X11_ActivateWorkstation
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
        Window  win = xi->win;
        XEvent                  event;
        XClientMessageEvent     *xcme;
        XWindowAttributes       xwa;    /* Get window attributes        */
        CoordSpace      square_screen;

        while(XCheckTypedEvent(xi->dpy,ClientMessage,&event) ||
                        XCheckMaskEvent(xi->dpy,StructureNotifyMask,&event)){

                switch (event.type) {
                case    DestroyNotify:
                        xi->dead = True;
                        break;

                case    ClientMessage:
                        xcme = (XClientMessageEvent*)&event;
                        xi->dead = True;
                        break;

                default:
                        break;
                }
                if(xi->dead)
                        break;
        }
        if(xi->dead){
                free_all_colors(xi);
                XFreeGC(xi->dpy,xi->line_gc);
                XFreeGC(xi->dpy,xi->fill_gc);
                XFreeGC(xi->dpy,xi->marker_gc);
                XFreeGC(xi->dpy,xi->cell_gc);
                XFreeGC(xi->dpy,xi->text_gc);
                XFreeGC(xi->dpy,xi->bg_gc);
                XCloseDisplay(xi->dpy);
                ESprintf(ERR_WIN_ATTRIB,"Window Destroyed");
                return(ERR_WIN_ATTRIB);
        }

        XSync(dpy,False);
        /*
         *      Find out how big the window is; calculate the
         *      coordinate translation macros.
         */
        if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
                ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
                return(ERR_WIN_ATTRIB);
        }

        square_screen = ComputeLargestSquare(
                (double) 0.0, (double) (xwa.height - 1),
                (double) (xwa.width - 1), (double) 0.0
        );
        TransformSetScreenSpace(
                &xi->tsystem, square_screen.llx, square_screen.lly, 
                square_screen.urx, square_screen.ury
        );

        xi->transform = TransformGetTransform(&xi->tsystem);

        xi->dim = square_screen.urx - square_screen.llx + 1;
        if(xi->size_change)
                (*xi->size_change)(xi->sref,xi->dim);

        return(0);
}

/*ARGSUSED*/
int
X11_DeactivateWorkstation
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        return(0);
}

int
X11_UpdateWorkstation
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
        XEvent                  event;
        XClientMessageEvent     *xcme;

        while(XCheckTypedEvent(xi->dpy,ClientMessage,&event) ||
                        XCheckMaskEvent(xi->dpy,StructureNotifyMask,&event)){

                switch (event.type) {
                case    DestroyNotify:
                        xi->dead = True;
                        break;

                case    ClientMessage:
                        xcme = (XClientMessageEvent*)&event;
                        xi->dead = True;
                        break;

                default:
                        break;
                }
                if(xi->dead)
                        break;
        }
        if(xi->dead){
                free_all_colors(xi);
                XFreeGC(xi->dpy,xi->line_gc);
                XFreeGC(xi->dpy,xi->fill_gc);
                XFreeGC(xi->dpy,xi->marker_gc);
                XFreeGC(xi->dpy,xi->cell_gc);
                XFreeGC(xi->dpy,xi->text_gc);
                XFreeGC(xi->dpy,xi->bg_gc);
                XCloseDisplay(xi->dpy);
                ESprintf(ERR_WIN_ATTRIB,"Window Destroyed");
                return(ERR_WIN_ATTRIB);
        }
        XSync(dpy, False);
        return(0);
}


int
X11_CloseWorkstation
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

        if(!xi->dead){
                free_all_colors(xi);
                XFreeGC(dpy,xi->line_gc);
                XFreeGC(dpy,xi->fill_gc);
                XFreeGC(dpy,xi->marker_gc);
                XFreeGC(dpy,xi->cell_gc);
                XFreeGC(dpy,xi->text_gc);
                XFreeGC(dpy,xi->bg_gc);
                XCloseDisplay(dpy);
        }
        if(xi->color_def) free(xi->color_def);
        free((char *) xi);

        return(0);
}

int
X11_ClearWorkstation
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
        Window  win = xi->win;

        XWindowAttributes       xwa;    /* Get window attributes        */
        CoordSpace      square_screen;

        /* 
         *      clear the screen
         */
        XClearWindow(dpy,win);

        /*
         *      find out how big window is. calculate coordinate translation 
         *      macros. (The user may have resized the window between frames).
         */
        if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
                ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
                return(ERR_WIN_ATTRIB);
        }

        square_screen = ComputeLargestSquare(
                (double) 0.0, (double) (xwa.height - 1),
                (double) (xwa.width - 1), (double) 0.0
        );
        TransformSetScreenSpace(
                &xi->tsystem, square_screen.llx, square_screen.lly, 
                square_screen.urx, square_screen.ury
        );

        xi->transform = TransformGetTransform(&xi->tsystem);

        xi->dim = square_screen.urx - square_screen.llx + 1;
        if(xi->size_change)
                (*xi->size_change)(xi->sref,xi->dim);


        return(0);
}

int
X11_Esc
#ifdef  NeedFuncProto
(
        GKSC    *gksc
)
#else
(gksc)
        GKSC    *gksc;
#endif
{
        Xddp                    *xi = (Xddp *) gksc->ddp;
        char                    *sptr = (char *) gksc->s.list;
        int                     *iptr = (int *) gksc->i.list;
        char                    *tstr;
        int                     tint;
        _NGCesc                 *cesc = (_NGCesc*)gksc->native;
        _NGCXGetXPix            *gxpix;
        _NGCXFreeCi             *fci;
        _NGCXGetSizeChg         *getsizechg;

        switch (iptr[0]) {
        case    ESCAPE_PAUSE:
                /*
                 * Pause does nothing for XUSRWIN type
                 */
                if(xi->xwtype == XUSRWIN) break;

                pause(xi->dpy);
                break;

        case    ESCAPE_PRIVATE_CMAP:

                if(xi->mycmap){
                        xi->color_model = CM_PRIVATE;
                        break;
                }

                X11_private_color(xi);
                break;

        case    ESCAPE_COLOR_ERROR:
                /* first token is wkid */
                tstr = strtok(sptr," ");
                /* second token is data */
                tstr = strtok(NULL," ");
                if(tstr == NULL)
                        return ERR_INV_DATA;
                tint = atoi(tstr);
                if((tint < 0 )|| (tint > 100))
                        return ERR_INV_DATA;
                if (tint == 100) tint = 0;
                xi->percent_colerr = tint;
                xi->pcerr_sqr = (float)tint*((float)MAX_INTEN_DIST/(float)100);
                xi->pcerr_sqr *= xi->pcerr_sqr;
                break;

        case    ESCAPE_COLOR_MODEL:
                /* first token is wkid */
                tstr = strtok(sptr," ");
                /* second token is data */
                tstr = strtok(NULL," ");
                if(tstr == NULL)
                        return ERR_INV_DATA;
                tint = atoi(tstr);
                if((tint < 0 )|| (tint > 2))
                        return ERR_INV_DATA;
                if(tint == xi->color_model){
                        /*
                         * noop - no change.
                         */
                        ;
                }
                else if(tint == CM_MIXED){
                        /*
                         * setting to mixed doesn't immediately change anything.
                         * It just means GSCR reacts differently on a
                         * color fault.
                         */
                        xi->color_model = (XColModel)tint;
                }
                else if(tint == CM_PRIVATE){
                        /*
                         * Moving to CM_PRIVATE.
                         * Allocate Colormap, and copy default colormap into
                         * it to minimize flashing.
                         * This is the same as a color fault in
                         * SetColorRepresentation for CM_MIXED.
                         */
                        X11_private_color(xi);
                        xi->color_model = (XColModel)tint;
                }
                else if(xi->mycmap){
                        /*
                         * Can't go back to shared once we allocate a private
                         * colormap.
                         */
                        return ERR_CHNG_CMODEL;
                }
                else{
                        /*
                         * moving to shared from mixed without already
                         * having allocated a cmap - no work.
                         */
                        xi->color_model = (XColModel)tint;
                }
                break;

        case NGESC_CNATIVE:
                switch(cesc->type){
                case NGC_XGETXPIX:
                        gxpix = (_NGCXGetXPix*)cesc;
                        if(gxpix->gksci > (MAX_COLORS - 1))
                                return ERR_MAX_COLOR;
                        gxpix->xpixnum = xi->color_pal[gxpix->gksci];
                        break;
                case NGC_XFREECI:
                        fci = (_NGCXFreeCi*)cesc;
                        X11_free_ci(xi,fci->gksci);
                        break;
                case NGC_XSIZECHG:
                        getsizechg = (_NGCXGetSizeChg*)cesc;
                        xi->size_change = getsizechg->xget_size;
                        xi->sref = getsizechg->closure;
                        if(xi->size_change)
                                (*xi->size_change)(xi->sref,xi->dim);
                        break;
                case NGC_SETALPHA:   /* alpha get/set ignored here */
                case NGC_GETALPHA:
						break;
                default:
                        return ERR_INV_ESCAPE;
                }

                break;
        case ESCAPE_COORDS_0:   /*  Ignore if this ever happens */
                break;
        case ESCAPE_COORDS_1:   /*  Ignore if this ever happens */
                break;
        case ESCAPE_ORIENT:     /*  Ignore if this ever happens */
                break;
        default:
                return ERR_INV_ESCAPE;
        }

        return 0;
}

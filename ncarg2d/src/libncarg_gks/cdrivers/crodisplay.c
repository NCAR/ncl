/*
 * $Id: crodisplay.c,v 1.1.2.1 2010-03-17 20:54:21 brownrig Exp $
 *
 */

#include <stdlib.h>
#include <string.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>
#include <cairo/cairo.h>
#include <ncarg/gksP.h>
#include "gksc.h"
#include "croddi.h"
#include "x.h"

static Window createXWorkWindow(
    Display         *dpy,
    _NGCXWinConfig  *xwc
    );

cairo_surface_t*
croCreateNativeDisplaySurface(CROddp* psa)
{
    _NGCesc                 *cesc;
    _NGCXWinConfig          *xwc=NULL;
    static char             dpy_mem[MAX_DPY_LEN];
    static char             *dpy_name=NULL; /* Display name */
    Display*                display;
    Window                  window;
    XWindowAttributes       xwa;
    cairo_surface_t*        surface;

    while(cesc = _NGGetCEscInit()){
        switch(cesc->type){
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
    if (dpy_name == NULL) {
        dpy_name = getenv("DISPLAY");

        if (dpy_name == NULL) {
            ESprintf(ERR_NO_DISPLAY, "X11 \"DISPLAY\" env. variable not set");
            return NULL;
        }

        strcpy(dpy_mem, dpy_name);
        dpy_name = dpy_mem;
    }

    if ((display = XOpenDisplay(dpy_name)) == NULL){
        ESprintf(ERR_OPN_DISPLAY, "  Error on opening X display (%s)",
                 dpy_name);
        return NULL;
    }

    window = createXWorkWindow(display, xwc);


    if(XGetWindowAttributes(display, window ,&xwa) == 0){
        ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
        return NULL;
    }

    /**** HOW TO TELL WE HAVE A TrueColor VISUAL???? ***/
    surface = cairo_xlib_surface_create(display, window, xwa.visual,
                                        xwa.width, xwa.height);

#if 0
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

#endif
    return surface;
}

/*
 * createXworkWindow()
 *
 * Utility to create an X-Window.
 * Copied from function by the same name in xcontrol.c
 *
 */
static Window
createXWorkWindow(Display *dpy, _NGCXWinConfig  *xwc)
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

void croX11Pause(cairo_surface_t* surface) {
    XEvent  event;
    Display* display = cairo_xlib_surface_get_display(surface);

    if (!display)
        return;

    if (XCheckMaskEvent(display,ButtonPressMask|KeyPressMask,&event)) {

        cairo_xlib_surface_set_size(surface, event.xresizerequest.width, event.xresizerequest.height);
    }
    /*
     * discard all events that a impatient user
     * may have aquired while waiting for a plot to finnish
     */
    while(XCheckMaskEvent(display,ButtonPressMask|KeyPressMask,&event));

    /*
     * wait for next buttonpress or keypress
     */
    XMaskEvent(display,ButtonPressMask|KeyPressMask,&event);

    return;
}


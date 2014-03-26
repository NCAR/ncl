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
#include <cairo/cairo-xlib.h>
#include <ncarg/gksP.h>
#include "gksc.h"
#include "cro.h"
#include "croddi.h"
#include "x.h"

static Window createXWorkWindow(
    Display         *dpy,
    CROddp          *xwc
    );

/*
 * croCreateNativeDisplaySurface()
 *
 * Currently only returns an X11 window; named with high hopes that someday this will support
 * native windows on all platforms.
 *
 */
cairo_surface_t*
croCreateNativeDisplaySurface(CROddp* psa)
{
    static char             dpy_mem[MAX_DPY_LEN];
    static char             *dpy_name=NULL; /* Display name */
    Display*                display;
    Window                  window;
    XWindowAttributes       xwa;
    cairo_surface_t*        surface;

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

    window = createXWorkWindow(display, psa);


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
 *
 */
void
croFreeNativeSurface(cairo_surface_t* surface) {
    /* free all X11 resources...  */
    XCloseDisplay(cairo_xlib_surface_get_display(surface));
}

/*
 * createXworkWindow()
 *
 * Utility to create an X-Window.
 * Borrowed heavily from function by the same name in xcontrol.c
 *
 */
static Window
createXWorkWindow(Display *dpy, CROddp* psa)
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
    static XClassHint xch = {
        "xgks", /* resource name                */
        "Xgks"  /* class name                   */
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

    xsh.x = psa->window_pos_x;
    xsh.y = psa->window_pos_y;
    xsh.width = psa->image_width;
    xsh.height = psa->image_height;
    geom_mask = XValue | YValue | WidthValue | HeightValue;

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
    xswa.background_pixel = WhitePixel(dpy, DefaultScreen(dpy));
    xswa.border_pixel = WhitePixel(dpy, DefaultScreen(dpy)); 

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
    window_name.value = (unsigned char *) psa->window_title;
    window_name.nitems = strlen ((char *)window_name.value);

    icon_name.encoding = XA_STRING;
    icon_name.format = 8;
    icon_name.value = (unsigned char *) psa->icon_title;
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

#include <stdio.h>

/*
 * croX11Pause()
 *
 * Mechanism by which the X11 workstation's windows remain visible until the user
 * clicks a mouse button or types a key.
 *
 */
void croX11Pause(cairo_surface_t* surface) {
    XEvent  event;
    Display* display = cairo_xlib_surface_get_display(surface);

    if (!display)
        return;

    /*
     * discard all events that a impatient user
     * may have acquired while waiting for a plot to finnish
     */
    cairo_surface_flush(surface);
    while(XCheckMaskEvent(display,ButtonPressMask|KeyPressMask,&event)) { /* loop until false */ }
    
    /*
     * wait for next buttonpress or keypress
     */
    XMaskEvent(display,ButtonPressMask|KeyPressMask,&event);

    return;
}

void 
croActivateX11(CROddp *psa, cairo_surface_t* surface)
{
  static int initBack = 1;

    /* code for this function was heavily barrowed from the original X11 driver --RLB */
    XWindowAttributes       xwa;    /* Get window attributes        */
    CoordSpace              square_screen;

    Display *dpy = cairo_xlib_surface_get_display(surface);
    Window   win = cairo_xlib_surface_get_drawable(surface);

    if (initBack) {
       /* We want to set the window's background/border to the 
	* workstation's background color, but can't do it at window
	* creation time because the workstation color map is not 
	* available then. We do it once here, on the first activation.
	*/
        cairo_surface_flush(surface);
        XSetWindowBackground(dpy, win, psa->ctable[0]);
        XSetWindowBorder(dpy, win, psa->ctable[0]);
        initBack = 0;
    }

    XSync(dpy, False);
    
    /*
     *      Find out how big the window is; calculate the
     *      coordinate translation macros.
     */
    if (XGetWindowAttributes(dpy, win, &xwa) == 0) {
        ESprintf(ERR_WIN_ATTRIB, "XGetWindowAttributes(,,)");
        return;
    }

    square_screen = ComputeLargestSquare(
        (double) 0.0, (double) (xwa.height - 1),
	(double) (xwa.width - 1), (double) 0.0
    );

    psa->image_width = xwa.width; /*square_screen.urx; */
    psa->image_height = xwa.height; /*square_screen.lly;*/

    psa->dspace.llx = square_screen.llx;
    psa->dspace.urx = square_screen.urx;
    psa->dspace.lly = square_screen.ury;   /* note flip of y-axis */
    psa->dspace.ury = square_screen.lly;   /*         "           */

    psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
    psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

    if (psa->cro_clip.null == FALSE) {
      psa->cro_clip.llx = psa->dspace.llx;
      psa->cro_clip.lly = psa->dspace.lly;
      psa->cro_clip.urx = psa->dspace.urx;
      psa->cro_clip.ury = psa->dspace.ury;
    }

    cairo_xlib_surface_set_size(surface, xwa.width, xwa.height);

    return;
}

#ifdef BuildQtEnabled
/* This function is modified from croActivateX11 above. Wei Huang */
void croActivateQt(CROddp *psa)
{
    cairo_t* context = getContext(psa->wks_id);

    CoordSpace square_screen;

    double width  = (double) qt_screen_width;
    double height = (double) qt_screen_height;

#if 0
    static int qtInitBack = 1;

    if(qtInitBack)
    {
        cairo_surface_flush(qt_surface);
        qtInitBack = 0;
    }
#endif

  /*
   *fprintf(stderr, "\nin funtion: <%s>, line: %d, file: <%s>\n",
   *                 __PRETTY_FUNCTION__, __LINE__, __FILE__);
   */

    cairo_scale(context, width, height);

    psa->image_width  = qt_screen_width;
    psa->image_height = qt_screen_height;

    psa->dspace.llx = (double) 0.0;
    psa->dspace.lly = (double) 0.0;
    psa->dspace.urx = width - 1.0;
    psa->dspace.ury = height - 1.0;
  /*
   *square_screen = ComputeLargestSquare
   *               (psa->dspace.llx, psa->dspace.lly,
   *                psa->dspace.urx, psa->dspace.ury);
   */
    square_screen = ComputeLargestSquare((double)0.0, height, width, (double)0.0);

    psa->dspace.llx = square_screen.llx;
    psa->dspace.urx = square_screen.urx;
    psa->dspace.lly = square_screen.ury;   /* note flip of y-axis */
    psa->dspace.ury = square_screen.lly;   /*         "           */

    psa->dspace.xspan = ((psa->dspace.urx) - (psa->dspace.llx));
    psa->dspace.yspan = ((psa->dspace.ury) - (psa->dspace.lly));

    if (psa->cro_clip.null == FALSE)
    {
      psa->cro_clip.llx = psa->dspace.llx;
      psa->cro_clip.lly = psa->dspace.lly;
      psa->cro_clip.urx = psa->dspace.urx;
      psa->cro_clip.ury = psa->dspace.ury;
    }

  /*
   *fprintf(stderr, "out funtion: <%s>, line: %d, file: <%s>\n",
   *                 __PRETTY_FUNCTION__, __LINE__, __FILE__);
   */
}
#endif


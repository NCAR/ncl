/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993, 1994 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.4
*/ 
#ifdef REV_INFO
#ifndef lint
static char rcsid[] = "$RCSfile: CascadeBG.c,v $ $Revision: 1.1 $ $Date: 1998-09-18 23:47:35 $"
#endif
#endif
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */

#include <string.h>
#include <ncarg/ngo/CascadeBGP.h>
#include <Xm/BaseClassP.h>
#include <X11/keysymdef.h>
#include <X11/ShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/MenuShellP.h>
#include <Xm/RowColumnP.h>
#include <Xm/LabelGP.h>
#include <Xm/LabelP.h>
#include <Xm/CascadeBP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/CacheP.h>
#include <Xm/DrawP.h>

#ifndef	Max
#define Max(a,b)	(((a)>(b))?(a):(b))
#endif

#define CASCADE_PIX_SPACE    4	/* pixels between label and bit map */
#define MAP_DELAY_DEFAULT   180
#define EVENTS              ((unsigned int) (ButtonPressMask | \
			       ButtonReleaseMask | EnterWindowMask | \
			       LeaveWindowMask))

#define WRONGPARENT	"XmCascadeButton[Gadget] must have xmRowColumnWidgetClass parent with\nrowColumnType XmMENU_PULLDOWN, XmMENU_POPUP, XmMENU_BAR or XmMENU_OPTION."
#define WRONGSUBMENU	"Only XmMENU_PULLDOWN XmRowColumnWidgets can be submenus."
#define WRONGMAPDELAY	"MapDelay must be >= 0."


/********    Static Function Declarations    ********/
#ifdef _NO_PROTO

static void ClassInitialize() ;
static void ClassPartInitialize() ;
static void SecondaryObjectCreate() ;
static void InitializePosthook() ;
static int _XmCascadeBCacheCompare() ;
static void BorderHighlight() ;
static void BorderUnhighlight() ;
static void DrawShadow() ;
static void DrawCascade() ;
static void position_cascade() ;
static void Redisplay() ;
static void InputDispatch() ;
static Boolean VisualChange() ;
static void Arm() ;
static void ArmAndPost() ;
static void ArmAndActivate() ;
static void Disarm() ;
static void PostTimeout() ;
static void DelayedArm() ;
static void CheckDisarm() ;
static void StartDrag() ;
static void Select() ;
static void DoSelect() ;
static void KeySelect() ;
static void MenuBarSelect() ;
static void MenuBarEnter() ;
static void MenuBarLeave() ;
static void size_cascade() ;
static void setup_cascade() ;
static void Destroy() ;
static void Resize() ;
static Boolean SetValuesPrehook() ;
static void GetValuesPrehook() ;
static void GetValuesPosthook() ;
static Boolean SetValuesPosthook() ;
static Boolean SetValues() ;
static void Initialize() ;
static Cardinal GetCascadeBGClassSecResData() ;
static XtPointer GetCascadeBGClassSecResBase() ;

#else

static void ClassInitialize( void ) ;
static void ClassPartInitialize( 
                        WidgetClass wc) ;
static void SecondaryObjectCreate( 
                        Widget req,
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args) ;
static void InitializePosthook( 
                        Widget req,
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args) ;
static int _XmCascadeBCacheCompare( 
                        XtPointer A,
                        XtPointer B) ;
static void BorderHighlight( 
                        Widget wid) ;
static void BorderUnhighlight( 
                        Widget wid) ;
static void DrawShadow( 
                        XmCascadeButtonGadget cb) ;
static void DrawCascade( 
                        register XmCascadeButtonGadget cb) ;
static void position_cascade( 
                        XmCascadeButtonGadget cascadebtn) ;
static void Redisplay( 
                        Widget wid,
                        XEvent *event,
                        Region region) ;
static Boolean VisualChange(
                        Widget wid,
                        Widget cmw,
                        Widget nmw) ;
static void InputDispatch( 
                        Widget wid,
                        XEvent *event,
                        Mask event_mask) ;
static void Arm( 
                        XmCascadeButtonGadget cb) ;
static void ArmAndPost( 
                        XmCascadeButtonGadget cb,
                        XEvent *event) ;
static void ArmAndActivate( 
                        Widget wid,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params) ;
static void Disarm( 
                        XmCascadeButtonGadget cb,
#if NeedWidePrototypes
                        int unpost) ;
#else
                        Boolean unpost) ;
#endif /* NeedWidePrototypes */
static void PostTimeout( 
                        XtPointer closure,
                        XtIntervalId *id) ;
static void DelayedArm( 
                        XmCascadeButtonGadget cb,
                        XEvent *event) ;
static void CheckDisarm( 
                        XmCascadeButtonGadget cb,
                        XEvent *event) ;
static void StartDrag( 
                        XmCascadeButtonGadget cb,
                        XEvent *event) ;
static void Select( 
                        XmCascadeButtonGadget cb,
                        XEvent *event,
#if NeedWidePrototypes
                        int doCascade) ;
#else
                        Boolean doCascade) ;
#endif /* NeedWidePrototypes */
static void DoSelect( 
                        XmCascadeButtonGadget cb,
                        XEvent *event) ;
static void KeySelect( 
                        XmCascadeButtonGadget cb,
                        XEvent *event) ;
static void MenuBarSelect( 
                        Widget wid,
                        XEvent *event) ;
static void MenuBarEnter( 
                        Widget wid,
                        XEvent *event) ;
static void MenuBarLeave( 
                        Widget wid) ;
static void size_cascade( 
                        XmCascadeButtonGadget cascadebtn) ;
static void setup_cascade( 
                        XmCascadeButtonGadget cascadebtn,
#if NeedWidePrototypes
                        int adjustWidth,
                        int adjustHeight) ;
#else
                        Boolean adjustWidth,
                        Boolean adjustHeight) ;
#endif /* NeedWidePrototypes */
static void Destroy( 
                        Widget wid) ;
static void Resize( 
                        Widget wid) ;
static Boolean SetValuesPrehook( 
                        Widget oldParent,
                        Widget refParent,
                        Widget newParent,
                        ArgList args,
                        Cardinal *num_args) ;
static void GetValuesPrehook( 
                        Widget newParent,
                        ArgList args,
                        Cardinal *num_args) ;
static void GetValuesPosthook( 
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args) ;
static Boolean SetValuesPosthook( 
                        Widget current,
                        Widget req,
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args) ;
static Boolean SetValues( 
                        Widget cw,
                        Widget rw,
                        Widget nw,
                        ArgList args,
                        Cardinal *num_args) ;
static void Initialize( 
                        Widget rw,
                        Widget nw,
                        ArgList args,
                        Cardinal *num_args) ;
static Cardinal GetCascadeBGClassSecResData( 
                        WidgetClass w_class,
                        XmSecondaryResourceData **data_rtn) ;
static XtPointer GetCascadeBGClassSecResBase( 
                        Widget widget,
                        XtPointer client_data) ;

#endif /* _NO_PROTO */
/********    End Static Function Declarations    ********/

static XtResource resources[] = 
{
    {   XmNactivateCallback,
        XmCCallback,
        XmRCallback,
        sizeof (XtCallbackList),
        XtOffsetOf( struct _XmCascadeButtonGadgetRec, cascade_button.activate_callback),
        XmRCallback,
        NULL
    },

    {   XmNcascadingCallback,
        XmCCallback,
        XmRCallback,
        sizeof (XtCallbackList),
        XtOffsetOf( struct _XmCascadeButtonGadgetRec, cascade_button.cascade_callback),
        XmRCallback,
        NULL
    },

    {	XmNsubMenuId, 
	XmCMenuWidget,				/* submenu */
	XmRMenuWidget, 
	sizeof (Widget),
	XtOffsetOf( struct _XmCascadeButtonGadgetRec, cascade_button.submenu), 
	XmRMenuWidget, 
	(XtPointer) 0
    },
    {
        XmNshadowThickness,
        XmCShadowThickness,
        XmRHorizontalDimension,
        sizeof (Dimension),
        XtOffsetOf( struct _XmCascadeButtonGadgetRec, gadget.shadow_thickness),
        XmRImmediate,
        (XtPointer) 2
    },

    {
        XmNtraversalOn,
        XmCTraversalOn,
        XmRBoolean,
        sizeof (Boolean),
        XtOffsetOf( struct _XmGadgetRec, gadget.traversal_on),
        XmRImmediate,
        (XtPointer) True
    },

    {
        XmNhighlightThickness,
        XmCHighlightThickness,
        XmRHorizontalDimension,
        sizeof (Dimension),
        XtOffsetOf( struct _XmGadgetRec, gadget.highlight_thickness),
        XmRImmediate,
        (XtPointer) 2
    },
};       


static XtResource cache_resources[] =
{

   {   XmNcascadePixmap, 
       XmCPixmap, 
       XmRGadgetPixmap,
       sizeof(Pixmap),
       XtOffsetOf( struct _XmCascadeButtonGCacheObjRec,
                 cascade_button_cache.cascade_pixmap), 
       XmRImmediate,
       (XtPointer) XmUNSPECIFIED_PIXMAP
   },

   {   XmNmappingDelay,
       XmCMappingDelay,
       XmRInt,
       sizeof (int),
       XtOffsetOf( struct _XmCascadeButtonGCacheObjRec,
	         cascade_button_cache.map_delay),
       XmRImmediate,
       (XtPointer) MAP_DELAY_DEFAULT
   },
};


static XmCacheClassPart CascadeButtonClassCachePart = {
    {NULL, 0, 0},        /* head of class cache list */
    _XmCacheCopy,       /* Copy routine     */
    _XmCacheDelete,     /* Delete routine   */
    _XmCascadeBCacheCompare,    /* Comparison routine   */
};

static XmBaseClassExtRec   CascadeBGClassExtensionRec = {
    NULL,    					    /* next_extension        */
    NULLQUARK,    				    /* record_typ  	     */
    XmBaseClassExtVersion,      		    /* version  	     */
    sizeof(XmBaseClassExtRec),  		    /* record_size  	     */
    XmInheritInitializePrehook,   		    /* initializePrehook     */
    SetValuesPrehook,   			    /* setValuesPrehook	     */
    InitializePosthook,   			    /* initializePosthook    */
    SetValuesPosthook,   			    /* setValuesPosthook     */
    (WidgetClass)&xmCascadeButtonGCacheObjClassRec, /* secondaryObjectClass  */
    SecondaryObjectCreate,                 	    /* secondaryObjectCreate */
    GetCascadeBGClassSecResData,                    /*  GetCascadeBGClassSecResData, getSecResData       */
    {0},           				    /* fast subclass         */
    GetValuesPrehook,				    /* getValuesPrehook      */
    GetValuesPosthook,				    /* getValuesPosthook      */
    NULL,                                     /* classPartInitPrehook */
    NULL,                                     /* classPartInitPosthook*/
    NULL,                                     /* ext_resources        */
    NULL,                                     /* compiled_ext_resources*/
    0,                                        /* num_ext_resources    */
    FALSE,                                    /* use_sub_resources    */
    XmInheritWidgetNavigable,                 /* widgetNavigable      */
    XmInheritFocusChange,                     /* focusChange          */
};

/* ext rec static initialization */
externaldef(xmcascadebuttongcacheobjclassrec)
XmCascadeButtonGCacheObjClassRec xmCascadeButtonGCacheObjClassRec =
{
  {
      /* superclass         */    (WidgetClass) &xmLabelGCacheObjClassRec,
      /* class_name         */    "XmCascadeButtonGadget",
      /* widget_size        */    sizeof(XmCascadeButtonGCacheObjRec),
      /* class_initialize   */    NULL,
      /* chained class init */    NULL,
      /* class_inited       */    False,
      /* initialize         */    NULL,
      /* initialize hook    */    NULL,
      /* realize            */    NULL,
      /* actions            */    NULL,
      /* num_actions        */    0,
      /* resources          */    cache_resources,
      /* num_resources      */    XtNumber(cache_resources),
      /* xrm_class          */    NULLQUARK,
      /* compress_motion    */    False,
      /* compress_exposure  */    False,
      /* compress enter/exit*/    False,
      /* visible_interest   */    False,
      /* destroy            */    NULL,
      /* resize             */    NULL,
      /* expose             */    NULL,
      /* set_values         */    NULL,
      /* set values hook    */    NULL,
      /* set values almost  */    NULL,
      /* get values hook    */    NULL,
      /* accept_focus       */    NULL,
      /* version            */    XtVersion,
      /* callback offsetlst */    NULL,
      /* default trans      */    NULL,
      /* query geo proc     */    NULL,
      /* display accelerator*/    NULL,
      /* extension record   */    NULL,
   },

   {
      /* synthetic resources */   NULL,
      /* num_syn_resources   */   0,
      /* extension           */   NULL,
   },
};


/*
 * static initialization of the cascade button widget class record, 
 * must do each field
 */

XmGadgetClassExtRec _XmCascadeBGadClassExtRec = {
     NULL,
     NULLQUARK,
     XmGadgetClassExtVersion,
     sizeof(XmGadgetClassExtRec),
     XmInheritBaselineProc,                  /* widget_baseline */
     XmInheritDisplayRectProc,               /* widget_display_rect */
};

externaldef(xmcascadebuttongadgetclassrec) XmCascadeButtonGadgetClassRec xmCascadeButtonGadgetClassRec = 
{
    {
	(WidgetClass) &xmLabelGadgetClassRec,	/* superclass ptr	*/
	"XmCascadeButtonGadget",		/* class_name	*/
	sizeof (XmCascadeButtonGadgetRec),	/* size of Pulldown widget */
	ClassInitialize,			/* class init proc */
	ClassPartInitialize,			/* chained class init */
	FALSE,					/* class is not init'ed */
	Initialize,				/* widget init proc */
	NULL,					/* init_hook proc */
    	NULL,					/* widget realize proc */
    	NULL,					/* class action table */
	0,
	resources,				/* this class's resource list */
	XtNumber (resources),			/*  "	  " resource_count */
    	NULLQUARK,				/* xrm_class	        */
    	TRUE,					/* do compress motion */
    	XtExposeCompressMaximal,		/* do compress exposure */
	TRUE,					/* don't compress enter-leave */
   	FALSE,					/* no VisibilityNotify */
	Destroy,				/* class destroy proc */
	Resize,					/* class resize proc */
	Redisplay,				/* expose proc */
	SetValues,				/* set_value proc */
	NULL,					/* set_value_hook proc */
	XtInheritSetValuesAlmost,		/* set_value_almost proc */
	NULL,					/* get_values_hook */
	NULL,					/* class accept focus proc */
	XtVersion,				/* current version */
    	NULL,					/* callback offset list */
    	NULL,					/* default translation table */
	XtInheritQueryGeometry,			/* query geo proc */
	NULL,				        /* display accelerator*/
	(XtPointer)&CascadeBGClassExtensionRec,	/* extension */
    },
    {
			/* Gadget Class record */
	BorderHighlight,			/* border_highlight */
	BorderUnhighlight,			/* border_uhighlight */
	ArmAndActivate,				/* arm & activate */
	InputDispatch,				/* input dispatch routine */
	VisualChange,				/* visual change routine */
	NULL,					/* syn resources */
	0,					/* num syn_resources */
	&CascadeButtonClassCachePart,		/* class cache part   */
	(XtPointer)&_XmCascadeBGadClassExtRec,	/* extension */
    },
    {			/* Label Class record */
	XmInheritWidgetProc,	        	/* set override callback */
	XmInheritMenuProc,      		/* menu procedures       */
	NULL,					/* extension */
    },
    {			/* cascade_button class record */
        NULL,  				        /* extension          */
    }
};

/*
 * now make a public symbol that points to this class record
 */

externaldef(xmcascadebuttongadgetclass) WidgetClass xmCascadeButtonGadgetClass = 
                          (WidgetClass) &xmCascadeButtonGadgetClassRec;

/*
 * use caching to share arrow data
 */
static XmCacheClassPart ArrowPixmapCache = {
   {NULL, 0, 0},                /* head of class cache list */
   _XmCacheCopy,                /* Copy routine     */
   _XmArrowPixmapCacheDelete,   /* Delete routine   */
   _XmArrowPixmapCacheCompare,  /* Comparison routine   */
};


static void 
#ifdef _NO_PROTO
ClassInitialize()
#else
ClassInitialize( void )
#endif /* _NO_PROTO */
{
  Cardinal                    wc_num_res, sc_num_res;
  XtResource                  *merged_list;
  int                         i, j;
  XtResourceList              uncompiled;
  Cardinal                    num;

/**************************************************************************
   Label's and Cascadebutton's resource lists are being merged into one
   and assigned to xmCascadeButtonGCacheObjClassRec. This is for performance
   reasons, since, instead of two calls to XtGetSubResources() XtGetSubvaluse()
   and XtSetSubvalues() for both the superclass and the widget class, now
   we have just one call with a merged resource list.
   NOTE: At this point the resource lists for Label and Cascadebutton do
         have unique entries, but if there are resources in the superclass
         that are being overwritten by the subclass then the merged_lists
         need to be created differently.
****************************************************************************/

  wc_num_res = xmCascadeButtonGCacheObjClassRec.object_class.num_resources;

  sc_num_res = xmLabelGCacheObjClassRec.object_class.num_resources;

  merged_list = (XtResource *)XtMalloc((sizeof(XtResource) * (wc_num_res +
                                                                 sc_num_res)));

  _XmTransformSubResources(xmLabelGCacheObjClassRec.object_class.resources,
                           sc_num_res, &uncompiled, &num);

  for (i = 0; i < num; i++)
  {

  merged_list[i] = uncompiled[i];

  }
  XtFree((char *)uncompiled);

  for (i = 0, j = num; i < wc_num_res; i++, j++)
  {
   merged_list[j] =
        xmCascadeButtonGCacheObjClassRec.object_class.resources[i];
  }

  xmCascadeButtonGCacheObjClassRec.object_class.resources = merged_list;
  xmCascadeButtonGCacheObjClassRec.object_class.num_resources =
                wc_num_res + sc_num_res ;

  CascadeBGClassExtensionRec.record_type = XmQmotif;
}

/*
 * set up fast subclassing
 */
static void 
#ifdef _NO_PROTO
ClassPartInitialize( wc )
        WidgetClass wc ;
#else
ClassPartInitialize(
        WidgetClass wc )
#endif /* _NO_PROTO */
{
   _XmFastSubclassInit (wc, XmCASCADE_BUTTON_GADGET_BIT);
}

/************************************************************************
*
*  SecondaryObjectCreate
*
************************************************************************/
/* ARGSUSED */
static void 
#ifdef _NO_PROTO
SecondaryObjectCreate( req, new_w, args, num_args )
        Widget req ;
        Widget new_w ;
        ArgList args ;
        Cardinal *num_args ;
#else
SecondaryObjectCreate(
        Widget req,
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
  XmBaseClassExt              *cePtr;
  XmWidgetExtData             extData;
  WidgetClass                 wc;
  Cardinal                    size;
  XtPointer                   newSec, reqSec;

  cePtr = _XmGetBaseClassExtPtr(XtClass(new_w), XmQmotif);
  wc = (*cePtr)->secondaryObjectClass;
  size = wc->core_class.widget_size;

  newSec = _XmExtObjAlloc(size);
  reqSec = _XmExtObjAlloc(size);

    /*
     * Since the resource lists for label and cascadebutton were merged at
     * ClassInitialize time we need to make only one call to
     * XtGetSubresources()
     */


  XtGetSubresources(new_w,
                    newSec,
                    NULL, NULL,
                    wc->core_class.resources,
                    wc->core_class.num_resources,
                    args, *num_args );

  extData = (XmWidgetExtData) XtCalloc(1, sizeof(XmWidgetExtDataRec));
  extData->widget = (Widget)newSec;
  extData->reqWidget = (Widget)reqSec;

  ((XmCascadeButtonGCacheObject)newSec)->ext.extensionType = XmCACHE_EXTENSION;
  ((XmCascadeButtonGCacheObject)newSec)->ext.logicalParent = new_w;

  _XmPushWidgetExtData(new_w, extData,
                         ((XmCascadeButtonGCacheObject)newSec)->ext.extensionType);
  memcpy(reqSec, newSec, size);

  /*
   * fill out cache pointers
  */
   LabG_Cache(new_w) = &(((XmLabelGCacheObject)extData->widget)->label_cache);
   LabG_Cache(req) = &(((XmLabelGCacheObject)extData->reqWidget)->label_cache);

   CBG_Cache(new_w) =
     &(((XmCascadeButtonGCacheObject)extData->widget)->cascade_button_cache);
   CBG_Cache(req) =
     &(((XmCascadeButtonGCacheObject)extData->reqWidget)->cascade_button_cache);

}


/************************************************************************
 *
 *  InitializePosthook
 *
 ************************************************************************/
/* ARGSUSED */
static void 
#ifdef _NO_PROTO
InitializePosthook( req, new_w, args, num_args )
        Widget req ;
        Widget new_w ;
        ArgList args ;
        Cardinal *num_args ;
#else
InitializePosthook(
        Widget req,
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
    XmWidgetExtData     ext;
    XmCascadeButtonGadget  cbw = (XmCascadeButtonGadget)new_w;

    /*
    * - register parts in cache.
    * - update cache pointers
    * - and free req
    */

    LabG_Cache(cbw) = (XmLabelGCacheObjPart *)
      _XmCachePart( LabG_ClassCachePart(cbw),
                    (XtPointer) LabG_Cache(cbw),
                    sizeof(XmLabelGCacheObjPart));

    CBG_Cache(cbw) = (XmCascadeButtonGCacheObjPart *)
           _XmCachePart( CBG_ClassCachePart(cbw),
                         (XtPointer) CBG_Cache(cbw),
                         sizeof(XmCascadeButtonGCacheObjPart));

   /*
    * might want to break up into per-class work that gets explicitly
    * chained. For right now, each class has to replicate all
    * superclass logic in hook routine
    */

   /*
    * free the req subobject used for comparisons
    */
    _XmPopWidgetExtData((Widget) cbw, &ext, XmCACHE_EXTENSION);
    _XmExtObjFree((XtPointer)ext->widget);
    _XmExtObjFree(ext->reqWidget);
    XtFree( (char *) ext);
    
}

/*******************************************************************
 *
 *  _XmCascadeBCacheCompare
 *
 *******************************************************************/
static int 
#ifdef _NO_PROTO
_XmCascadeBCacheCompare( A, B )
        XtPointer A ;
        XtPointer B ;
#else
_XmCascadeBCacheCompare(
        XtPointer A,
        XtPointer B )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGCacheObjPart *cascadeB_inst = (XmCascadeButtonGCacheObjPart *) A ;
        XmCascadeButtonGCacheObjPart *cascadeB_cache_inst = (XmCascadeButtonGCacheObjPart *) B ;
   if((cascadeB_inst->cascade_pixmap == cascadeB_cache_inst->cascade_pixmap) &&
      (cascadeB_inst->map_delay == cascadeB_cache_inst->map_delay) &&
      (cascadeB_inst->armed_pixmap == cascadeB_cache_inst->armed_pixmap)) 
        return 1;
   else
        return 0;

}

/*******************************************************************
 * _XmArrowPixmapCacheCompare()
 *******************************************************************/
int 
#ifdef _NO_PROTO
_XmArrowPixmapCacheCompare( A, B )
        XtPointer A ;
        XtPointer B ;
#else
_XmArrowPixmapCacheCompare(
        XtPointer A,
        XtPointer B )
#endif /* _NO_PROTO */
{
   XmArrowPixmap *arrowpix_rec = (XmArrowPixmap *) A ;
   XmArrowPixmap *arrowpix_cache_rec = (XmArrowPixmap *) B ;

   if ((arrowpix_rec->height == arrowpix_cache_rec->height) &&
       (arrowpix_rec->width == arrowpix_cache_rec->width) &&
       (arrowpix_rec->screen == arrowpix_cache_rec->screen) &&
       (arrowpix_rec->depth == arrowpix_cache_rec->depth) &&
       (arrowpix_rec->top_shadow_color ==
          arrowpix_cache_rec->top_shadow_color) &&
       (arrowpix_rec->bottom_shadow_color ==
          arrowpix_cache_rec->bottom_shadow_color) &&
       (arrowpix_rec->foreground_color ==
          arrowpix_cache_rec->foreground_color))
      return 1;
   else
      return 0;
}

/*******************************************************************
 * _XmArrowPixmapCacheDelete()
 *******************************************************************/
void 
#ifdef _NO_PROTO
_XmArrowPixmapCacheDelete( data )
        XtPointer data ;
#else
_XmArrowPixmapCacheDelete(
        XtPointer data )
#endif /* _NO_PROTO */
{
        Pixmap pixmap = (Pixmap) data ;
   XmGadgetCachePtr ptr;
   XmArrowPixmap *arrowpix_rec;

   ptr =  (XmGadgetCachePtr)(ClassCacheHead(&ArrowPixmapCache)).next;
   while (ptr)
   {
     arrowpix_rec = (XmArrowPixmap *)(CacheDataPtr(ptr));
     if (pixmap == arrowpix_rec->pixmap)
     {
        if (--ptr->ref_count <= 0)
        {
          (ptr->prev)->next = ptr->next;
          if (ptr->next)                    /* not the last record */
            (ptr->next)->prev = ptr->prev;
          XFreePixmap(arrowpix_rec->display, arrowpix_rec->pixmap);
          XtFree( (char *) ptr);
        }
        return;
     }
     else
        ptr = (XmGadgetCachePtr)ptr->next;
   }
}

/*
 * Border highlighting is only allowed for option menus.  Otherwise
 * the button is armed (does not pop up submenus).
 */
static void 
#ifdef _NO_PROTO
BorderHighlight( wid )
        Widget wid ;
#else
BorderHighlight(
        Widget wid )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid ;
   if (LabG_MenuType(cb) == XmMENU_OPTION)
      (* ((XmGadgetClass) xmGadgetClass)->
             gadget_class.border_highlight) ((Widget) cb);

   else
      Arm (cb);
}



/*
 * Border unhighlighting only done in option menus.  Otherwise the button
 * is disarmed (does not pop down submenus).
 */
static void 
#ifdef _NO_PROTO
BorderUnhighlight( wid )
        Widget wid ;
#else
BorderUnhighlight(
        Widget wid )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid ;
   if (LabG_MenuType(cb) == XmMENU_OPTION)
      (* ((XmGadgetClass) xmGadgetClass)->
             gadget_class.border_unhighlight) ((Widget) cb);

   else
      Disarm (cb, False);
}


/*
 * Draw the 3D shadow around the widget if its in an option menu or if the
 * widget is armed.
 */
static void 
#ifdef _NO_PROTO
DrawShadow( cb )
        XmCascadeButtonGadget cb ;
#else
DrawShadow(
        XmCascadeButtonGadget cb )
#endif /* _NO_PROTO */
{
   if (CBG_IsArmed(cb) ||
      (LabG_MenuType(cb) == XmMENU_OPTION))
   {
      if (XtIsRealized(XtParent(cb)))
      {
	 _XmDrawShadows (XtDisplay (cb), XtWindow (XtParent(cb)),
			XmParentTopShadowGC(cb),
			XmParentBottomShadowGC(cb),
			cb->gadget.highlight_thickness + cb->rectangle.x,
			cb->gadget.highlight_thickness + cb->rectangle.y,
			 cb->rectangle.width - 2 * 
			cb->gadget.highlight_thickness,
			 cb->rectangle.height - 2 * 
			cb->gadget.highlight_thickness,
			cb->gadget.shadow_thickness,
			XmSHADOW_OUT);
      }
   }
}

static void 
#ifdef _NO_PROTO
DrawCascade( cb )
        register XmCascadeButtonGadget cb ;
#else
DrawCascade(
        register XmCascadeButtonGadget cb )
#endif /* _NO_PROTO */
{
   if ((CBG_HasCascade(cb)) && (CBG_Cascade_width(cb) != 0))
   {
      /* draw the casacade */
      if ((LabG_MenuType(cb) == XmMENU_OPTION) &&
	  (CBG_CascadePixmap(cb) == XmUNSPECIFIED_PIXMAP))
      {
	 Dimension height, width;
	 Dimension offset_y;

	 switch(CBG_Cascade_height(cb) - 2*G_ShadowThickness(cb)) {
	    case 5:
	    case 6:
	       height = 1;
	       width =  CBG_Cascade_width(cb) - 3;
	       break;
	    case 7:
	    case 8:
	    case 9:
	       height = 2;
	       width =  CBG_Cascade_width(cb) - 4;
	       break;
	    case 10:
	    case 11:
	    case 12:
	    case 13:
	       height = 3;
	       width =  CBG_Cascade_width(cb) - 5;
	       break;
	    default:
	       height = 4;
	       width =  CBG_Cascade_width(cb) - 6;
	       break;
	 }
	 width -= 2*G_ShadowThickness(cb);
	 offset_y =  (CBG_Cascade_height(cb)- height)/2;

	 XFillRectangle(XtDisplay(cb), XtWindow(XtParent(cb)),
	    XmParentBackgroundGC(cb),
	    cb->rectangle.x + CBG_Cascade_x(cb) + G_ShadowThickness(cb),
	    cb->rectangle.y + CBG_Cascade_y(cb) + offset_y,
	    width, height);
	    
	 _XmDrawShadows(XtDisplay(cb), XtWindow(XtParent(cb)),
	    XmParentTopShadowGC(cb), XmParentBottomShadowGC(cb),
	    cb->rectangle.x + CBG_Cascade_x(cb),
	    cb->rectangle.y + CBG_Cascade_y(cb) + offset_y -
	       G_ShadowThickness(cb),
	    width + (2* G_ShadowThickness(cb)),
	    height +  (2* G_ShadowThickness(cb)),
	    G_ShadowThickness(cb), XmSHADOW_OUT);
      }
      else
	 XCopyArea (XtDisplay(cb), 
	    CBG_IsArmed(cb) && (CBG_ArmedPixmap(cb) != XmUNSPECIFIED_PIXMAP) ?
	       CBG_ArmedPixmap(cb) : CBG_CascadePixmap(cb), 
	    XtWindow(XtParent(cb)),
	    LabG_NormalGC(cb), 0, 0, 
	    CBG_Cascade_width(cb), CBG_Cascade_height(cb),
	    cb->rectangle.x + CBG_Cascade_x(cb), 
	    cb->rectangle.y + CBG_Cascade_y(cb));
   }
}


/*
 * set up the cascade position.  
 */
static void 
#ifdef _NO_PROTO
position_cascade( cascadebtn )
        XmCascadeButtonGadget cascadebtn ;
#else
position_cascade(
        XmCascadeButtonGadget cascadebtn )
#endif /* _NO_PROTO */
{
   Dimension buffer;

   if (CBG_HasCascade(cascadebtn))
   { 
      CBG_Cascade_x(cascadebtn) = cascadebtn->rectangle.width -
                               cascadebtn->gadget.highlight_thickness -
                               cascadebtn->gadget.shadow_thickness -
			       LabG_MarginWidth(cascadebtn) -
                               CBG_Cascade_width(cascadebtn);

      buffer = cascadebtn->gadget.highlight_thickness +
             cascadebtn->gadget.shadow_thickness +
             LabG_MarginHeight(cascadebtn);

      CBG_Cascade_y(cascadebtn) = buffer +
                               ((cascadebtn->rectangle.height -  2*buffer) -
                                CBG_Cascade_height(cascadebtn)) / 2;
   }
   else
   {
      CBG_Cascade_y(cascadebtn) = 0;
      CBG_Cascade_x(cascadebtn) = 0;
   }
}

/*
 * redisplay the widget
 */
static void 
#ifdef _NO_PROTO
Redisplay( wid, event, region )
        Widget wid ;
        XEvent *event ;
        Region region ;
#else
Redisplay(
        Widget wid,
        XEvent *event,
        Region region )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid ;

    if (XtIsRealized (cb))
    {
       if ((LabG_MenuType(cb) == XmMENU_POPUP) ||
	   (LabG_MenuType(cb) == XmMENU_PULLDOWN))
       {
	  XmMenuShellWidget mshell = (XmMenuShellWidget)XtParent(XtParent(cb));

	  if (!mshell->shell.popped_up)
	      return;
       }

       /*
	* This might be necessary in an option menu to keep the glyph from
	* moving when it's items vary in size.
	*/
       if (LabG_MenuType(cb) == XmMENU_OPTION)
          position_cascade(cb);

        /* Label class does most of the work */
	(* xmLabelGadgetClassRec.rect_class.expose)((Widget)cb, event, region);

	DrawCascade(cb);
	DrawShadow (cb);
    }
}


/************************************************************************
 *
 *  VisualChange
 *      This function is called from XmManagerClass set values when
 *      the managers visuals have changed.  The gadget regenerates any
 *      GC based on the visual changes and returns True indicating a
 *      redraw is needed.  Otherwize, False is returned.
 *
 ************************************************************************/
static Boolean
#ifdef _NO_PROTO
VisualChange( wid, cmw, nmw )
        Widget wid ;
        Widget cmw ;
        Widget nmw ;
#else
VisualChange(
        Widget wid,
        Widget cmw,
        Widget nmw )
#endif /* _NO_PROTO */
{
   XmCascadeButtonGadget cbg = (XmCascadeButtonGadget) wid;
   XmManagerWidget curmw = (XmManagerWidget) cmw ;
   XmManagerWidget newmw = (XmManagerWidget) nmw ;
   XmCascadeButtonGCacheObjPart oldCopy;

   if (curmw->manager.foreground != newmw->manager.foreground ||
       curmw->core.background_pixel != newmw->core.background_pixel)
   {
      if (CBG_ArmedPixmap(cbg) != XmUNSPECIFIED_PIXMAP)
      {
         _XmArrowPixmapCacheDelete((XtPointer) CBG_ArmedPixmap(cbg));
         _XmArrowPixmapCacheDelete((XtPointer) CBG_CascadePixmap(cbg));

         _XmCacheCopy((XtPointer) CBG_Cache(cbg), &oldCopy,
            sizeof(XmCascadeButtonGCacheObjPart));
         _XmCacheDelete ((XtPointer) CBG_Cache(cbg));
         CBG_Cache(cbg) = &oldCopy;

         CBG_ArmedPixmap(cbg) = XmUNSPECIFIED_PIXMAP;
         CBG_CascadePixmap(cbg) = XmUNSPECIFIED_PIXMAP;
         _XmCreateArrowPixmaps((Widget) cbg);

         CBG_Cache(cbg) = (XmCascadeButtonGCacheObjPart *) _XmCachePart(
                           CBG_ClassCachePart(cbg), (XtPointer) CBG_Cache(cbg),
                          sizeof(XmCascadeButtonGCacheObjPart));

         size_cascade (cbg);

         return (True);
      }
   }
   return (False);
}

/*
 * Input sent by a manger and dispatched here.  The gadget handles Arm,
 * Activate, Enter, Leave, FocusIn, FocusOut and Help events.
 */
static void 
#ifdef _NO_PROTO
InputDispatch( wid, event, event_mask )
        Widget wid ;
        XEvent *event ;
        Mask event_mask ;
#else
InputDispatch(
        Widget wid,
        XEvent *event,
        Mask event_mask )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid ;
   if (event_mask & XmARM_EVENT)
   {
      if (LabG_MenuType(cb) == XmMENU_OPTION)
         ArmAndPost (cb, event);

      else if (LabG_MenuType(cb) == XmMENU_BAR)
	  MenuBarSelect ((Widget) cb, event);
      
      else 
         StartDrag (cb, event);
   }

   else if (event_mask & XmACTIVATE_EVENT)
   {
      if ((LabG_MenuType(cb) == XmMENU_PULLDOWN) ||
          (LabG_MenuType(cb) == XmMENU_POPUP) ||
	  (LabG_MenuType(cb) == XmMENU_BAR))
      {
          if (event->type == ButtonRelease)
             DoSelect (cb, event);

          else if (event->type == KeyPress)
             KeySelect (cb, event);
      }
      /* else option menu - do nothing if menu was not posted on btndown */
   }

   else if (event_mask & XmENTER_EVENT)
   {
      if (LabG_MenuType(cb) == XmMENU_BAR)
	  MenuBarEnter ((Widget) cb, event);

      else if (LabG_MenuType(cb) == XmMENU_OPTION)
	  _XmEnterGadget ((Widget) cb, event, NULL, NULL);

      else 
	  DelayedArm (cb, event);
   }

   else if (event_mask & XmLEAVE_EVENT)
   {
      if (LabG_MenuType(cb) == XmMENU_BAR)
	  MenuBarLeave ((Widget) cb);
      else if (LabG_MenuType(cb) == XmMENU_OPTION)
	  _XmLeaveGadget( (Widget) cb, event, NULL, NULL);
      else
	  CheckDisarm (cb, event);
   }

   else if (event_mask & XmFOCUS_IN_EVENT)
      (* (((XmCascadeButtonGadgetClassRec *)(cb->object.widget_class))->
		gadget_class.border_highlight)) ((Widget) cb);

   else if (event_mask & XmFOCUS_OUT_EVENT)
   {
      if (((LabG_MenuType(cb) == XmMENU_POPUP) || 
           (LabG_MenuType(cb) == XmMENU_PULLDOWN)) &&
          ((((XmManagerWidget)XtParent(cb))->manager.active_child == 
	    (Widget)cb) &&
	   (CBG_Submenu(cb))))
      {
	  XmMenuShellWidget mshell =
	      (XmMenuShellWidget) XtParent(CBG_Submenu(cb));

	  if ((mshell->composite.children[0] == CBG_Submenu(cb)) &&
	      (XmIsMenuShell(mshell)) &&
	      (mshell->shell.popped_up))
	      return;
      }

      (* (((XmCascadeButtonGadgetClassRec *)(cb->object.widget_class))->
		gadget_class.border_unhighlight)) ((Widget) cb);
    } 
    else if (event_mask & XmHELP_EVENT)
       _XmCBHelp((Widget) cb, event, NULL, NULL); 
}


/*
 * Arming the CascadeButtonGadget consists of setting the armed bit
 * and drawing the 3D shadow.  CascadeButtonGadgets in
 * option menus are never armed since they will never get the event
 * to cause it to unarm.
 */
static void 
#ifdef _NO_PROTO
Arm( cb )
        XmCascadeButtonGadget cb ;
#else
Arm(
        XmCascadeButtonGadget cb )
#endif /* _NO_PROTO */
{
   if ((LabG_MenuType(cb) != XmMENU_OPTION) &&
       (! CBG_IsArmed(cb)))
   {
      CBG_SetArmed(cb, TRUE);
      DrawCascade(cb);
      DrawShadow (cb);
   }
   XmProcessTraversal((Widget) cb, XmTRAVERSE_CURRENT);
}



/*
 * Post any submenus and then arm the gadget.  The order is important for
 * performance.
 */
static void 
#ifdef _NO_PROTO
ArmAndPost( cb, event )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
#else
ArmAndPost(
        XmCascadeButtonGadget cb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmMenuState mst = _XmGetMenuState((Widget)cb);

   if (!CBG_IsArmed(cb))
   {
      if ((LabG_MenuType(cb) == XmMENU_OPTION) &&
	  (XtParent(cb) == mst->RC_ReplayInfo.toplevel_menu) &&
	  (event->xbutton.time == mst->RC_ReplayInfo.time))
	 return;

      _XmCascadingPopup ((Widget) cb, event, TRUE);
      Arm(cb);

      /*
       * Option menus must be armed since the post just arms the
       * submenu
       */
      if (LabG_MenuType(cb) == XmMENU_OPTION)
      {
	  (* xmLabelGadgetClassRec.label_class.menuProcs)
	      (XmMENU_ARM, (Widget) cb);
      }

      /*
       * Record so spring loaded DispatchEvent() doesn't handle this event
       */
      if (event)
	  _XmRecordEvent(event);
   }
}

/*
 * class function to cause the cascade button to be armed and selected
 */
static void 
#ifdef _NO_PROTO
ArmAndActivate( wid, event, params, num_params )
        Widget wid ;
        XEvent *event ;
        String *params ;
        Cardinal *num_params ;
#else
ArmAndActivate(
        Widget wid,
        XEvent *event,
        String *params,
        Cardinal *num_params )
#endif /* _NO_PROTO */
{
   XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid ;
   XmAnyCallbackStruct cback;
   XmRowColumnWidget parent = (XmRowColumnWidget) XtParent(cb);
   Time _time = __XmGetDefaultTime(wid, event);


   /* check if event has been processed */
   if (event && !_XmIsEventUnique(event))
      return;

   switch (LabG_MenuType(cb))
   {
    case XmMENU_OPTION:
    {
       ArmAndPost (cb, event);
       if (CBG_Submenu(cb))
       {
          /*
           * if XmProcessTraversal() fails, it's possible that the pane
           * has no traversable children, so reset the focus to the pane.
           */
          if (!XmProcessTraversal(CBG_Submenu(cb), XmTRAVERSE_CURRENT))
             XtSetKeyboardFocus(XtParent(CBG_Submenu(cb)), CBG_Submenu(cb));
       }
       break;
    }

    case XmMENU_PULLDOWN:
    case XmMENU_POPUP:
    {
       /* In case the tear off is active but not armed or grabbed */
       (* xmLabelGadgetClassRec.label_class.menuProcs)
	   (XmMENU_TEAR_OFF_ARM, (Widget)parent);

       Select (cb, event, TRUE);
       if (CBG_Submenu(cb))
       {
          /*
           * if XmProcessTraversal() fails, it's possible that the pane
           * has no traversable children, so reset the focus to the pane.
           */
          if (!XmProcessTraversal(CBG_Submenu(cb), XmTRAVERSE_CURRENT))
             XtSetKeyboardFocus(XtParent(CBG_Submenu(cb)), CBG_Submenu(cb));
       }
       break;
    }

    case XmMENU_BAR:
    {
       ShellWidget myShell = NULL;

       /* Shared menupanes require some additional checks */
       if (CBG_Submenu(cb))
	   myShell = (ShellWidget)XtParent(CBG_Submenu(cb));

       if (myShell && 
	   XmIsMenuShell(myShell) &&         /* not torn ?! */
           (myShell->shell.popped_up) &&
	   (myShell->composite.children[0] == CBG_Submenu(cb)) &&
	   (cb == (XmCascadeButtonGadget)RC_CascadeBtn(CBG_Submenu(cb))))
       {
	  (* xmLabelGadgetClassRec.label_class.menuProcs)
	      (XmMENU_POPDOWN, (Widget) parent, NULL, event, NULL);

	  Disarm (cb, FALSE);
       }

       else 
       {
	  /* call the cascading callbacks first thing */
	  cback.reason = XmCR_CASCADING;
	  cback.event = event;
	  XtCallCallbackList ((Widget) cb, CBG_CascadeCall(cb), &cback);

	  /*
	   * check if the traversing flag is set true.  This indicates
	   * that we are in a traverse and don't want to activate if
	   * there is no submenu attached.  Set during KDown in menubar.
	   */
	  if (CBG_Traversing(cb) && !CBG_Submenu(cb))
	      return;

	  if (! RC_IsArmed (parent))
	  {
	     _XmMenuFocus((Widget) parent, XmMENU_BEGIN, _time);

	     if (CBG_Submenu (cb))
		 (* xmLabelGadgetClassRec.label_class.menuProcs)
		     (XmMENU_ARM, (Widget) cb);
	  }
	  else
	      (* xmLabelGadgetClassRec.label_class.menuProcs)
		  (XmMENU_BAR_CLEANUP, (Widget) parent);

	  /* do the select without calling the cascading callbacks again */
	  Select (cb, event, FALSE);

          /* To support menu replay, keep the pointer in sync mode */
          XAllowEvents(XtDisplay(cb), SyncPointer, _time);

	  if (CBG_Submenu(cb))
	  {
             /*
              * if XmProcessTraversal() fails, it's possible that the pane
              * has no traversable children, so reset the focus to the pane.
              */
             if (!XmProcessTraversal(CBG_Submenu(cb), XmTRAVERSE_CURRENT))
                XtSetKeyboardFocus(XtParent(CBG_Submenu(cb)), CBG_Submenu(cb));
	  }
	  else
	  {
	     (* xmLabelGadgetClassRec.label_class.menuProcs)
		 (XmMENU_DISARM, (Widget) parent);

	     _XmMenuFocus(XtParent(cb), XmMENU_END, _time);
	     XtUngrabPointer(XtParent(cb), _time);
	  }
       }
       
       break;
    }
   }   
   /* Record so spring loaded DispatchEvent() doesn't recall this routine.  */
   if (event)
      _XmRecordEvent(event);
}


/*
 * Disarm the menu.  This may include popping down any submenu that is up
 * and removing the timeout to post a submenu.
 */
static void 
#ifdef _NO_PROTO
Disarm( cb, unpost )
        XmCascadeButtonGadget cb ;
        Boolean unpost ;
#else
Disarm(
        XmCascadeButtonGadget cb,
#if NeedWidePrototypes
        int unpost )
#else
        Boolean unpost )
#endif /* NeedWidePrototypes */
#endif /* _NO_PROTO */
{
   Widget rowcol = XtParent(cb);
   
   if (CBG_IsArmed(cb))
   {
      CBG_SetArmed(cb,FALSE);

      /* popdown  any posted submenus */
      if ((unpost) &&  (RC_PopupPosted(rowcol)))
      {
	  (*(((XmMenuShellClassRec *)xmMenuShellWidgetClass)->
	     menu_shell_class.popdownEveryone))
	      (RC_PopupPosted(rowcol),NULL, NULL, NULL);
      }

      /* if a delayed arm is pending, remove it */
      if (CBG_Timer(cb))
      {
         XtRemoveTimeOut (CBG_Timer(cb));
         CBG_Timer(cb) = 0; 
      }

      /* if the shadow is drawn and the menupane is not going down, erase it */
      if ((! RC_PoppingDown(rowcol)) || RC_TornOff(rowcol))
      {
	 if (XtIsRealized(rowcol))
	 {
	    _XmClearBorder (XtDisplay (cb), XtWindow (rowcol),
			    cb->gadget.highlight_thickness + cb->rectangle.x,
			    cb->gadget.highlight_thickness + cb->rectangle.y,
			    cb->rectangle.width - 2 * 
			     cb->gadget.highlight_thickness,
			    cb->rectangle.height - 2 * 
			     cb->gadget.highlight_thickness,
			    cb->gadget.shadow_thickness);
	 }
      }
      DrawCascade(cb);
   }
}


/*
 * called when the post delay timeout occurs.
 */
static void
#ifdef _NO_PROTO
PostTimeout( closure, id )
	XtPointer closure ;
	XtIntervalId *id ;
#else
PostTimeout(
	XtPointer closure,
	XtIntervalId *id)
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) closure ;

   if (CBG_Timer(cb))
   {
      CBG_Timer(cb) = 0;
    
      _XmCascadingPopup ((Widget) cb, NULL, TRUE);

   }
}



/*
 * set the timer to post the submenu if a leave event does
 * not occur first.
 */
static void 
#ifdef _NO_PROTO
DelayedArm( cb, event )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
#else
DelayedArm(
        XmCascadeButtonGadget cb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   if ((! CBG_IsArmed(cb)) &&
       (((XmMenuShellWidget) XtParent(XtParent(cb)))->shell.popped_up) &&
       _XmGetInDragMode((Widget) cb))

   {
      if (CBG_MapDelay(cb) <= 0)
      {
	 /* don't delay, just do it */
	 ArmAndPost (cb, event);
      }
      else
      {
         CBG_Timer(cb) = XtAppAddTimeOut(
			       XtWidgetToApplicationContext( (Widget) cb), 
			       (unsigned long) CBG_MapDelay(cb),
			       PostTimeout, (XtPointer) cb) ;
         Arm(cb);
      }
   }
}


/*
 * if traversal is not on and the mouse
 * has not entered its cascading submenu, disarm the
 * CascadeButtonGadget.
 */
static void 
#ifdef _NO_PROTO
CheckDisarm( cb, event )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
#else
CheckDisarm(
        XmCascadeButtonGadget cb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmMenuShellWidget submenushell;
   XMotionEvent * entEvent = (XMotionEvent *) event;

   if (_XmGetInDragMode((Widget) cb))
   {
      if ((CBG_IsArmed(cb)) && 
          (CBG_Submenu(cb)))
      {
         submenushell = (XmMenuShellWidget) XtParent (CBG_Submenu(cb));
   
         if (submenushell->shell.popped_up)
         {
            if ((entEvent->x_root >= submenushell->core.x) &&
                (entEvent->x_root <  submenushell->core.x + 
                                     submenushell->core.width +
                                     (submenushell->core.border_width << 1)) &&
                (entEvent->y_root >= submenushell->core.y) &&
                (entEvent->y_root <  submenushell->core.y + 
                                     submenushell->core.height +
	   			     (submenushell->core.border_width << 1)))

  	        /* then we are in the cascading submenu, don't disarm */
 	        return;

#ifdef entEvent_is_LEAVE_EVENT
             /*
              * When kick starting a cascading menu from a tear off, we grab
              * the pointer to the parent rc when the cascade has the focus
              * (In StartDrag().  A leave window event is generated (with
              * mode = NotifyGrab) which we don't wish to recognize.
              */
             if ((entEvent->mode == NotifyGrab) &&
                 !XmIsMenuShell(XtParent(XtParent(cb))))
                return;
#endif
         }
      }
      Disarm (cb, TRUE);
   }
}


/*
 * post submenu and disable traversal.  These functions must be called
 * in this order.
 */
static void 
#ifdef _NO_PROTO
StartDrag( cb, event )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
#else
StartDrag(
        XmCascadeButtonGadget cb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmRowColumnWidget parent = (XmRowColumnWidget) XtParent(cb);
   XmMenuShellWidget mshell = (XmMenuShellWidget) XtParent(parent);

   if (((LabG_MenuType(cb) == XmMENU_PULLDOWN) ||
	(LabG_MenuType(cb) == XmMENU_POPUP)) &&
       ! mshell->shell.popped_up)
   {
      return;
   }

   /* In case the tear off is active but not armed or grabbed */
   (* xmLabelGadgetClassRec.label_class.menuProcs)
       (XmMENU_TEAR_OFF_ARM, (Widget)parent);

   _XmSetInDragMode((Widget) cb, True);

   _XmCascadingPopup ((Widget) cb, event, TRUE);
   Arm (cb);
   
   /* record event so MenuShell does not process it */
   _XmRecordEvent (event);
}


/*
 * do the popup and if there is not a submenu, bring down the menu system.
 */
static void 
#ifdef _NO_PROTO
Select( cb, event, doCascade )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
        Boolean doCascade ;
#else
Select(
        XmCascadeButtonGadget cb,
        XEvent *event,
#if NeedWidePrototypes
        int doCascade )
#else
        Boolean doCascade )
#endif /* NeedWidePrototypes */
#endif /* _NO_PROTO */
{
   XmAnyCallbackStruct cback;

   _XmCascadingPopup ((Widget) cb, event, doCascade);

   /*
    * check if there is a submenu here in case this changed during 
    * the cascadeing callbacks
    */
   if (CBG_Submenu(cb) == NULL)
   {
      (* xmLabelGadgetClassRec.label_class.menuProcs)
	  (XmMENU_POPDOWN, XtParent(cb), NULL, event, NULL);

      Disarm (cb, FALSE);
      (* xmLabelGadgetClassRec.label_class.menuProcs)
         (XmMENU_DISARM, XtParent(cb), NULL, NULL, NULL);
      
      cback.event = event;
      cback.reason = XmCR_ACTIVATE;
      
      if (XmIsRowColumn(XtParent(cb)))
      {
	 (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_CALLBACK, 
							  XtParent(cb),
							  FALSE, cb,
							  &cback);
      }

      if ((! LabG_SkipCallback(cb)) &&
	  (CBG_ActivateCall(cb)))
      {
	 XtCallCallbackList ((Widget) cb, CBG_ActivateCall(cb), &cback);
      }
   }
   else 
   { 
      Arm (cb); 
   }
}



/*
 * if there is a submenu, enable traversal.
 * call select to do the work
 */
static void 
#ifdef _NO_PROTO
DoSelect( cb, event )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
#else
DoSelect(
        XmCascadeButtonGadget cb,
        XEvent *event )
#endif /* _NO_PROTO */
{
  Time _time = __XmGetDefaultTime((Widget) cb, event);

   if ((LabG_MenuType(cb) == XmMENU_BAR) &&
       ! RC_IsArmed(XtParent(cb)))
       return;
       
   /*
    * make sure the shell is popped up, this takes care of a corner case
    * that can occur with rapid pressing of the mouse button
    */
   if (((LabG_MenuType(cb) == XmMENU_PULLDOWN) ||
	(LabG_MenuType(cb) == XmMENU_POPUP)) &&
       (!((XmMenuShellWidget) XtParent(XtParent(cb)))->shell.popped_up))
   {
      return;
   }

   Select(cb, event, (Boolean)(CBG_Submenu(cb) != NULL));

   /*
    * don't let the menu shell widget process this event
    */
   _XmRecordEvent (event);
   
   _XmSetInDragMode((Widget) cb, False);

   if (CBG_Submenu(cb))
   {
      /*
       * if XmProcessTraversal() fails, it's possible that the pane has
       * no traversable children, so reset the focus to the pane.
       */
      if (!XmProcessTraversal(CBG_Submenu(cb), XmTRAVERSE_CURRENT))
         XtSetKeyboardFocus(XtParent(CBG_Submenu(cb)), CBG_Submenu(cb));
   }
   else
   {
      /* *** Move this call to Select() ***
       *
       * (* xmLabelGadgetClassRec.label_class.menuProcs)
       *    (XmMENU_DISARM, XtParent(cb));
       */

      if (LabG_MenuType(cb) == XmMENU_BAR)
      {
	 _XmMenuFocus(XtParent(cb), XmMENU_END, _time);
	 XtUngrabPointer(XtParent(cb), _time);
      }
   }
}


/*
 * if the menu system traversal is enabled, do a select
 */
static void 
#ifdef _NO_PROTO
KeySelect( cb, event )
        XmCascadeButtonGadget cb ;
        XEvent *event ;
#else
KeySelect(
        XmCascadeButtonGadget cb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmRowColumnWidget parent = (XmRowColumnWidget) XtParent(cb);

   /* check if event has been processed */
   if (!_XmIsEventUnique(event))
      return;

   if (!_XmGetInDragMode((Widget) cb) && RC_IsArmed(parent))
   {
      if (LabG_MenuType(cb) == XmMENU_BAR)
	  (* xmLabelGadgetClassRec.label_class.menuProcs)
	      (XmMENU_BAR_CLEANUP, (Widget) parent);
	  
      Select(cb, event, TRUE);

      if (CBG_Submenu(cb)) 
      {
         XmProcessTraversal(CBG_Submenu(cb), XmTRAVERSE_CURRENT);
      }
   }

   /* record so that menuShell does not process this event */
   _XmRecordEvent(event);
}


/*
 * If the menu system is not active, arm it and arm this cascadebutton
 * else start the drag mode
 */
static void 
#ifdef _NO_PROTO
MenuBarSelect( wid, event )
        Widget wid ;
        XEvent *event ;
#else
MenuBarSelect(
        Widget wid,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmCascadeButtonWidget cb = (XmCascadeButtonWidget) wid ;
   Boolean validButton;
   Time _time = __XmGetDefaultTime(wid, event);

   if (RC_IsArmed ((XmRowColumnWidget) XtParent(cb)))
   {
      /* Cleanup the PM menubar mode, if enabled */
      (* xmLabelGadgetClassRec.label_class.menuProcs)
	  (XmMENU_BAR_CLEANUP, XtParent(cb));

      if (!CBG_Submenu(cb))
      {
	 _XmMenuFocus(XtParent(cb), XmMENU_MIDDLE, _time);
      }

      StartDrag ((XmCascadeButtonGadget) cb, event);

   }

   else
   {
      (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_BUTTON,
						       XtParent(cb), NULL,
						       event, &validButton);
   
      if (validButton)
      {
         _XmMenuFocus(XtParent(cb), XmMENU_BEGIN, _time);

         (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_ARM, 
                                                                  (Widget) cb);

         _XmSetInDragMode((Widget) cb, True);

         _XmCascadingPopup ((Widget) cb, event, TRUE);

	 /* To support menu replay, keep the pointer in sync mode */
	 XAllowEvents(XtDisplay(cb), SyncPointer, _time);

	 if (!CBG_Submenu(cb))
	 {  
	    /*
	     * since no submenu is posted, check if the grab has occured
	     * and if not, do the pointer grab now.
	     */
	    if (RC_BeingArmed(XtParent(cb)))
	    {
	       Cursor cursor;

	       cursor = XmGetMenuCursor(XtDisplay(cb));

               _XmGrabPointer(XtParent(cb), True, EVENTS,
                  GrabModeAsync, GrabModeAsync, None, cursor, _time);

	       RC_SetBeingArmed(XtParent(cb), False);
	    }
	 } 
	 
	 /* record so that menuShell doesn't process this event */
	 _XmRecordEvent (event);
      }
   }
}


/* 
 * If the menu is active, post submenu and arm.
 */
static void 
#ifdef _NO_PROTO
MenuBarEnter( wid, event )
        Widget wid ;
        XEvent *event ;
#else
MenuBarEnter(
        Widget wid,
        XEvent *event )
#endif /* _NO_PROTO */
{
   register XmCascadeButtonWidget cb = (XmCascadeButtonWidget) wid ;
   XmRowColumnWidget rc = (XmRowColumnWidget)XtParent(cb);

   if ((RC_IsArmed (rc)) && (! CBG_IsArmed(cb))
       && _XmGetInDragMode((Widget) cb))
   {
      if (!CBG_Submenu(cb))
      {
	 _XmMenuFocus( (Widget) rc, XmMENU_MIDDLE, 
		      __XmGetDefaultTime(wid, event));
      }

      _XmCascadingPopup ((Widget) cb, event, TRUE);
      Arm((XmCascadeButtonGadget) cb);
   }
}


/*
 * unless our submenu is posted or traversal is on, disarm
 */
static void 
#ifdef _NO_PROTO
MenuBarLeave( wid )
        Widget wid ;
#else
MenuBarLeave(
        Widget wid )
#endif /* _NO_PROTO */
{
        register XmCascadeButtonWidget cb = (XmCascadeButtonWidget) wid ;
   XmMenuShellWidget submenuShell;

   if (RC_IsArmed (XtParent (cb)))
   {
      if (CBG_Submenu(cb))
      {
         submenuShell = (XmMenuShellWidget) XtParent(CBG_Submenu(cb));

         if (submenuShell->shell.popped_up)
            return;
      }  
   
      if (_XmGetInDragMode((Widget) cb))
         Disarm ((XmCascadeButtonGadget) cb, TRUE);   
   }
}


/*
 * Create the CB and CBG 3d arrows.
 * The Pixmaps must be unspecified going into this routine.  This helps to
 * make sure arrow cache is sync'd up as well as not accidentally overwriting
 * application's pixmap arrow if set.
 */
void 
#ifdef _NO_PROTO
_XmCreateArrowPixmaps( wid )
        Widget wid ;
#else
_XmCreateArrowPixmaps(
        Widget wid )
#endif /* _NO_PROTO */
{
   XmCascadeButtonWidget cb = (XmCascadeButtonWidget) wid ;
   XmArrowPixmap cpart;
   XmArrowPixmap *armed_arrow, *unarmed_arrow;
   GC gc, tsGC, bsGC;
   XGCValues values;
   Pixmap pixmap;
   int ht, st;
   Pixel tsc, bsc, bg;
   Dimension side;
   Screen *screen;
   int depth;
   unsigned char arrow_direction;
   unsigned short text_height;

   screen = XtScreen(cb);
   if (XmIsGadget(cb))
   {
      if ((CBG_CascadePixmap(cb) != XmUNSPECIFIED_PIXMAP) &&
          (CBG_ArmedPixmap(cb) != XmUNSPECIFIED_PIXMAP))
        return;

      ht = G_HighlightThickness(cb);
      st = G_ShadowThickness(cb);
      tsc = ((XmManagerWidget)XtParent(cb))->manager.top_shadow_color;
      bsc = ((XmManagerWidget)XtParent(cb))->manager.bottom_shadow_color;
      bg = ((XmManagerWidget)XtParent(cb))->core.background_pixel;
      tsGC = ((XmManagerWidget)XtParent(cb))->manager.top_shadow_GC;
      bsGC = ((XmManagerWidget)XtParent(cb))->manager.bottom_shadow_GC;
      arrow_direction =
         (LabG_StringDirection(cb) == XmSTRING_DIRECTION_L_TO_R) ?
         XmARROW_RIGHT : XmARROW_LEFT;
      text_height = LabG_TextRect_height(cb);
      depth = XtParent(cb)->core.depth;
   }
   else
   {
      if ((CB_CascadePixmap(cb) != XmUNSPECIFIED_PIXMAP) &&
          (CB_ArmedPixmap(cb) != XmUNSPECIFIED_PIXMAP))
        return;

      ht = cb->primitive.highlight_thickness;
      st = cb->primitive.shadow_thickness;
      tsc =  cb->primitive.top_shadow_color;
      bsc =  cb->primitive.bottom_shadow_color;
      bg = cb->core.background_pixel;
      tsGC = cb->primitive.top_shadow_GC;
      bsGC = cb->primitive.bottom_shadow_GC;
      arrow_direction =
         (cb->label.string_direction == XmSTRING_DIRECTION_L_TO_R) ?
         XmARROW_RIGHT : XmARROW_LEFT;
      text_height = Lab_TextRect_height(cb);
      depth = cb->core.depth;
   }

   side = Max( (text_height * 2 / 3) + 2 * (ht + st),
               (2*(ht + (st-1) +1)) +1 );       /* see _XmGetArrowDrawRects() */

   cpart.height = cpart.width = side;
   cpart.depth = depth;
   cpart.top_shadow_color = tsc;
   cpart.bottom_shadow_color = bsc;
   cpart.foreground_color = bg;
   cpart.display = XtDisplay(cb);
   cpart.screen = screen;
   cpart.pixmap = XmUNSPECIFIED_PIXMAP;

   /*
    * Create or get an existing arrow cache record for the unarmed arrow
    */
   unarmed_arrow = (XmArrowPixmap *)
      _XmCachePart(&ArrowPixmapCache, (XtPointer) &cpart, sizeof(XmArrowPixmap));

   /*
    * Create or get an existing arrow cache record for the armed arrow
    */
   cpart.top_shadow_color = bsc;
   cpart.bottom_shadow_color = tsc;

   armed_arrow =  (XmArrowPixmap *)
      _XmCachePart(&ArrowPixmapCache, (XtPointer) &cpart, sizeof(XmArrowPixmap));

   if ((unarmed_arrow->pixmap == XmUNSPECIFIED_PIXMAP) ||
       (armed_arrow->pixmap == XmUNSPECIFIED_PIXMAP))
   {
      values.foreground = values.background = bg;
      values.graphics_exposures = False;
      gc = XtGetGC ((Widget) cb,
                   GCForeground | GCBackground | GCGraphicsExposures, &values);

      /* armed arrow */
      if (armed_arrow->pixmap == XmUNSPECIFIED_PIXMAP)
      {
         pixmap = XCreatePixmap(XtDisplay(cb), RootWindowOfScreen(screen),
            side, side, depth);

         armed_arrow->pixmap = pixmap;

         XFillRectangle(XtDisplay(cb), pixmap, gc, 0, 0, side, side);
	 _XmDrawArrow(XtDisplay((Widget)cb), pixmap,
		bsGC, tsGC, gc,
		ht + st - 1,
		ht + st - 1,
		side - 2*(ht + st - 1),
		side - 2*(ht + st - 1),
		st, arrow_direction);
      }

      /* standard (unarmed) pixmap */
      if (unarmed_arrow->pixmap == XmUNSPECIFIED_PIXMAP)
      {
         pixmap = XCreatePixmap(XtDisplay(cb),  RootWindowOfScreen(screen),
            side, side, depth);

         unarmed_arrow->pixmap = pixmap;

         XFillRectangle(XtDisplay(cb), pixmap, gc, 0, 0, side, side);
	 _XmDrawArrow(XtDisplay((Widget)cb), pixmap,
	 tsGC, bsGC, gc, 
	 	ht + st - 1, 
	 	ht + st - 1, 
	 	side - 2*(ht + st - 1),
	 	side - 2*(ht + st - 1), 
	 	2, arrow_direction);
      }
      XtReleaseGC( (Widget) cb, gc);
   }
   if (XmIsGadget(cb))
   {
      CBG_ArmedPixmap(cb) = armed_arrow->pixmap;
      CBG_CascadePixmap(cb) = unarmed_arrow->pixmap;
   }
   else
   {
      CB_ArmedPixmap(cb) = armed_arrow->pixmap;
      CB_CascadePixmap(cb) = unarmed_arrow->pixmap;
   }
}


/*
 * get the cascade size set up
 */
static void 
#ifdef _NO_PROTO
size_cascade( cascadebtn )
        XmCascadeButtonGadget cascadebtn ;
#else
size_cascade(
        XmCascadeButtonGadget cascadebtn )
#endif /* _NO_PROTO */
{
    Window rootwin;
    int x,y;					/* must be int */
    unsigned int width, height, border, depth;  /* must be unsigned int */

    if (CBG_CascadePixmap(cascadebtn) != XmUNSPECIFIED_PIXMAP)
    {
       XGetGeometry(XtDisplay(cascadebtn), CBG_CascadePixmap(cascadebtn),
		    &rootwin, &x, &y, &width, &height,
		    &border, &depth);

       CBG_Cascade_width(cascadebtn) = (Dimension) width;
       CBG_Cascade_height(cascadebtn) = (Dimension) height;
    }
    else
    {
       if (LabG_MenuType(cascadebtn) == XmMENU_OPTION)
       {
	  CBG_Cascade_width(cascadebtn) = 
	     CBG_Cascade_height(cascadebtn) = 
		Max(LabG_TextRect(cascadebtn).height,
		    LabG_AccTextRect(cascadebtn).height) + 
	        2 * cascadebtn->gadget.shadow_thickness;     /* glyph shadow */

       }
       else
       {
	  CBG_Cascade_width(cascadebtn) = 0;
	  CBG_Cascade_height(cascadebtn) = 0;
       }
    }
}


/*
 * set up the cascade size and location
 */
static void 
#ifdef _NO_PROTO
setup_cascade( cascadebtn, adjustWidth, adjustHeight )
        XmCascadeButtonGadget cascadebtn ;
        Boolean adjustWidth ;
        Boolean adjustHeight ;
#else
setup_cascade(
        XmCascadeButtonGadget cascadebtn,
#if NeedWidePrototypes
        int adjustWidth,
        int adjustHeight )
#else
        Boolean adjustWidth,
        Boolean adjustHeight )
#endif /* NeedWidePrototypes */
#endif /* _NO_PROTO */
{
   Dimension delta;

   if (CBG_HasCascade(cascadebtn))
   {
      /*
       *  modify the size of the CascadeButtonGadget to acommadate the
       *  cascade, if needed.  The cascade should fit inside MarginRight.
       */
      if ((CBG_Cascade_width(cascadebtn) + CASCADE_PIX_SPACE) >
	  LabG_MarginRight(cascadebtn))
      {
	 delta = CBG_Cascade_width(cascadebtn) + CASCADE_PIX_SPACE -
	     LabG_MarginRight(cascadebtn);
	 LabG_MarginRight(cascadebtn) = LabG_MarginRight(cascadebtn) +
	   delta;

	 if (adjustWidth)
	     cascadebtn->rectangle.width += delta;

	 else
	 {
	    if (LabG_Alignment(cascadebtn) == XmALIGNMENT_END)
		LabG_TextRect_x(cascadebtn) -= delta;
	    else if (LabG_Alignment(cascadebtn) == XmALIGNMENT_CENTER)
		LabG_TextRect_x(cascadebtn) -= delta/2;
          
	 }
      }
	
      /*
       * the cascade height should fit inside of 
       * TextRect + marginTop + marginBottom
       */
      delta = CBG_Cascade_height(cascadebtn) +
                   2 * (LabG_MarginHeight(cascadebtn) +
		         cascadebtn->gadget.shadow_thickness +
		         cascadebtn->gadget.highlight_thickness);
      
      if (delta > cascadebtn->rectangle.height)
      {
	 delta -= cascadebtn->rectangle.height;
	 LabG_MarginTop(cascadebtn) = LabG_MarginTop(cascadebtn) + 
	   (delta/2);
	 LabG_TextRect_y(cascadebtn) += delta/2;
	 LabG_MarginBottom(cascadebtn) = LabG_MarginBottom(cascadebtn) + 
	   delta - (delta/2);
	 
	 if (adjustHeight)
	     cascadebtn->rectangle.height += delta;
      }
   }

   position_cascade(cascadebtn);
}


/*
 * Destroy the widget
 */
static void 
#ifdef _NO_PROTO
Destroy( wid )
        Widget wid ;
#else
Destroy(
        Widget wid )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid ;
    XmRowColumnWidget submenu = (XmRowColumnWidget) CBG_Submenu(cb);

    /*
     * break the submenu link
     */
    if (submenu != NULL)
	(* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_SUBMENU,
                                                  (Widget) submenu, FALSE, cb);

    if (CBG_Timer(cb))
         XtRemoveTimeOut (CBG_Timer(cb));
    
    XtRemoveAllCallbacks ((Widget) cb, XmNactivateCallback);
    XtRemoveAllCallbacks ((Widget) cb, XmNcascadingCallback);

    if (CBG_ArmedPixmap(cb) != XmUNSPECIFIED_PIXMAP)
    {
       _XmArrowPixmapCacheDelete((XtPointer) CBG_ArmedPixmap(cb));
       _XmArrowPixmapCacheDelete((XtPointer) CBG_CascadePixmap(cb));
    }
    _XmCacheDelete((XtPointer) CBG_Cache(cb));
}
                         

/*
 * routine to resize a cascade button, called by the parent
 * geometery manager
 */
static void 
#ifdef _NO_PROTO
Resize( wid )
        Widget wid ;
#else
Resize(
        Widget wid )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget cb = (XmCascadeButtonGadget) wid;
   if (cb)
   {
      /* Label class does it's work */

      (* xmLabelGadgetClassRec.rect_class.resize) ((Widget) cb);

      /* move the cascade too */
      size_cascade(cb);
      position_cascade (cb);
   }
}


/************************************************************************
 *
 *  SetValuesPrehook
 *
 ************************************************************************/
/* ARGSUSED */
static Boolean 
#ifdef _NO_PROTO
SetValuesPrehook( oldParent, refParent, newParent, args, num_args )
        Widget oldParent ;
        Widget refParent ;
        Widget newParent ;
        ArgList args ;
        Cardinal *num_args ;
#else
SetValuesPrehook(
        Widget oldParent,
        Widget refParent,
        Widget newParent,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
    XmWidgetExtData             extData;
    XmBaseClassExt              *cePtr;
    WidgetClass                 ec;
    Cardinal                    size;
    XmCascadeButtonGCacheObject newSec, reqSec;

    cePtr = _XmGetBaseClassExtPtr(XtClass(newParent), XmQmotif);
    ec = (*cePtr)->secondaryObjectClass;
    size = ec->core_class.widget_size;

    newSec = (XmCascadeButtonGCacheObject)_XmExtObjAlloc(size);
    reqSec = (XmCascadeButtonGCacheObject)_XmExtObjAlloc(size);

    newSec->object.self = (Widget)newSec;
    newSec->object.widget_class = ec;
    newSec->object.parent = XtParent(newParent);
    newSec->object.xrm_name = newParent->core.xrm_name;
    newSec->object.being_destroyed = False;
    newSec->object.destroy_callbacks = NULL;
    newSec->object.constraints = NULL;

    newSec->ext.logicalParent = newParent;
    newSec->ext.extensionType = XmCACHE_EXTENSION;

    memcpy( &(newSec->label_cache),
            LabG_Cache(newParent),
            sizeof(XmLabelGCacheObjPart));

    memcpy( &(newSec->cascade_button_cache),
            CBG_Cache(newParent),
            sizeof(XmCascadeButtonGCacheObjPart));

    extData = (XmWidgetExtData) XtCalloc(1, sizeof(XmWidgetExtDataRec));
    extData->widget = (Widget)newSec;
    extData->reqWidget = (Widget)reqSec;
    _XmPushWidgetExtData(newParent, extData, XmCACHE_EXTENSION);

    /*
     * Since the resource lists for label and cascadebutton were merged at
     * ClassInitialize time we need to make only one call to
     * XtSetSubvalues()
     */

    XtSetSubvalues((XtPointer)newSec,
                    ec->core_class.resources,
                    ec->core_class.num_resources,
                    args, *num_args);

    memcpy((XtPointer)reqSec, (XtPointer)newSec, size);

    LabG_Cache(newParent) = &(((XmLabelGCacheObject)newSec)->label_cache);
    LabG_Cache(refParent) = &(((XmLabelGCacheObject)extData->reqWidget)->label_cache);

    CBG_Cache(newParent) =
        &(((XmCascadeButtonGCacheObject)newSec)->cascade_button_cache);
    CBG_Cache(refParent) =
        &(((XmCascadeButtonGCacheObject)extData->reqWidget)->cascade_button_cache);

    _XmExtImportArgs((Widget)newSec, args, num_args);

    return FALSE;
}

/************************************************************************
 *
 *  GetValuesPrehook
 *
 ************************************************************************/
/* ARGSUSED */
static void 
#ifdef _NO_PROTO
GetValuesPrehook( newParent, args, num_args )
        Widget newParent ;
        ArgList args ;
        Cardinal *num_args ;
#else
GetValuesPrehook(
        Widget newParent,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
    XmWidgetExtData             extData;
    XmBaseClassExt              *cePtr;
    WidgetClass                 ec;
    Cardinal 			size;
    XmCascadeButtonGCacheObject newSec;

    cePtr = _XmGetBaseClassExtPtr(XtClass(newParent), XmQmotif);
    ec = (*cePtr)->secondaryObjectClass;
    size = ec->core_class.widget_size;

    newSec = (XmCascadeButtonGCacheObject)_XmExtObjAlloc(size);

    newSec->object.self = (Widget)newSec;
    newSec->object.widget_class = ec;
    newSec->object.parent = XtParent(newParent);
    newSec->object.xrm_name = newParent->core.xrm_name;
    newSec->object.being_destroyed = False;
    newSec->object.destroy_callbacks = NULL;
    newSec->object.constraints = NULL;

    newSec->ext.logicalParent = newParent;
    newSec->ext.extensionType = XmCACHE_EXTENSION;

    memcpy( &(newSec->label_cache),
            LabG_Cache(newParent),
            sizeof(XmLabelGCacheObjPart));

    memcpy( &(newSec->cascade_button_cache),
            CBG_Cache(newParent),
            sizeof(XmCascadeButtonGCacheObjPart));

    extData = (XmWidgetExtData) XtCalloc(1, sizeof(XmWidgetExtDataRec));
    extData->widget = (Widget)newSec;
    _XmPushWidgetExtData(newParent, extData, XmCACHE_EXTENSION);

    /* Note that if a resource is defined in the superclass's as well as a
   subclass's resource list and if a NULL is passed in as the third
   argument to XtSetArg, then when a GetSubValues() is done by the
   superclass the NULL is replaced by a value. Now when the subclass
   gets the arglist it doesn't see a NULL and thinks it's an address
   it needs to stuff a value into and sure enough it breaks.
   This means that we have to pass the same arglist with the NULL to
   both the superclass and subclass and propagate the values up once
   the XtGetSubValues() are done.*/

    /*
     * Since the resource lists for label and cascadebutton were merged at
     * ClassInitialize time we need to make only one call to
     * XtGetSubvalues()
     */

    XtGetSubvalues((XtPointer)newSec,
                   ec->core_class.resources,
                   ec->core_class.num_resources,
                   args, *num_args);    

    _XmExtGetValuesHook((Widget)newSec, args, num_args);
}

/************************************************************************
 *
 *  GetValuesPosthook
 *
 ************************************************************************/
/* ARGSUSED */
static void 
#ifdef _NO_PROTO
GetValuesPosthook( new_w, args, num_args )
        Widget new_w ;
        ArgList args ;
        Cardinal *num_args ;
#else
GetValuesPosthook(
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
    XmWidgetExtData             ext;

    _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

    _XmExtObjFree(ext->widget);
    XtFree( (char *) ext);
}


/************************************************************************
 *
 *  SetValuesPosthook
 *
 ************************************************************************/
/* ARGSUSED */
static Boolean 
#ifdef _NO_PROTO
SetValuesPosthook( current, req, new_w, args, num_args )
        Widget current ;
        Widget req ;
        Widget new_w ;
        ArgList args ;
        Cardinal *num_args ;
#else
SetValuesPosthook(
        Widget current,
        Widget req,
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
   XmWidgetExtData                  ext;
   /*
    * - register parts in cache.
    * - update cache pointers
    * - and free req
    */

  /* assign if changed! */
  if (!_XmLabelCacheCompare(LabG_Cache(new_w), LabG_Cache(current)))
  {
      _XmCacheDelete((XtPointer) LabG_Cache(current));  /* delete the old one */
      LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
                     _XmCachePart(LabG_ClassCachePart(new_w),
                                 (XtPointer) LabG_Cache(new_w),
                                 sizeof(XmLabelGCacheObjPart));
  }
  else
       LabG_Cache(new_w) = LabG_Cache(current);

  /* assign if changed! */
  if (!_XmCascadeBCacheCompare(CBG_Cache(new_w),
                              CBG_Cache(current)))
  {
      _XmCacheDelete((XtPointer) CBG_Cache(current));  /* delete the old one */
      CBG_Cache(new_w) = (XmCascadeButtonGCacheObjPart *)
                     _XmCachePart(CBG_ClassCachePart(new_w),
                                 (XtPointer) CBG_Cache(new_w),
                                 sizeof(XmCascadeButtonGCacheObjPart));
  }
  else
       CBG_Cache(new_w) = CBG_Cache(current);

  _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

  _XmExtObjFree(ext->widget);
  _XmExtObjFree(ext->reqWidget);

  XtFree( (char *) ext);

  return FALSE;
}



/*
 * Set Values
 */
static Boolean 
#ifdef _NO_PROTO
SetValues( cw, rw, nw, args, num_args )
        Widget cw ;
        Widget rw ;
        Widget nw ;
        ArgList args ;
        Cardinal *num_args ;
#else
SetValues(
        Widget cw,
        Widget rw,
        Widget nw,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
    XmCascadeButtonGadget old = (XmCascadeButtonGadget) cw ;
    XmCascadeButtonGadget requested = (XmCascadeButtonGadget) rw ;
    XmCascadeButtonGadget new_w = (XmCascadeButtonGadget) nw ;
    Boolean flag = FALSE;
    Boolean adjustWidth = FALSE;
    Boolean adjustHeight = FALSE;

    if ((CBG_Submenu(new_w)) &&
	((! XmIsRowColumn(CBG_Submenu(new_w))) ||
	 (RC_Type(CBG_Submenu(new_w)) != XmMENU_PULLDOWN)))
    {
       CBG_Submenu(new_w) = NULL;
       _XmWarning( (Widget) new_w, WRONGSUBMENU);
    }

    if (CBG_MapDelay(new_w) < 0) 
    {
       CBG_MapDelay(new_w) = CBG_MapDelay(old);
       _XmWarning( (Widget) new_w, WRONGMAPDELAY);
    }

    /* if there is a change to submenu, notify menu system */
    if (CBG_Submenu(old) != CBG_Submenu(new_w))
    {
       /* We must pass new_w as the fourth parameter to menuprocs' XmMENU_SUBMENU
	* because old is a copy!  The eventual call to SetCascadeField() does 
	* a widget ID comparison and we must pass the real widget (new_w).
	*/
       if (CBG_Submenu(old))
	   (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_SUBMENU,
							    CBG_Submenu(old),
							    FALSE, new_w);

       if (CBG_Submenu(new_w))
	  (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_SUBMENU,
							   CBG_Submenu(new_w), 
							   TRUE, new_w);
    }

    if (LabG_MenuType(new_w) == XmMENU_BAR)
	new_w->gadget.traversal_on = TRUE;
    
    /* handle the cascade pixmap indicator */
    else if (LabG_MenuType(new_w) == XmMENU_PULLDOWN ||
	     LabG_MenuType(new_w) == XmMENU_POPUP ||
	     LabG_MenuType(new_w) == XmMENU_OPTION)
    {
       /* don't let traversal change */
       if (LabG_MenuType(new_w) != XmMENU_OPTION)
          new_w->gadget.traversal_on = TRUE;

       if (LabG_RecomputeSize(new_w) || (requested->rectangle.width <= 0))
	  adjustWidth = TRUE;
       
       if (LabG_RecomputeSize(new_w) || (requested->rectangle.height <= 0))
	  adjustHeight = TRUE;

       /* get new pixmap size */
       if (CBG_CascadePixmap (old) != CBG_CascadePixmap (new_w))
       {
          if (CBG_ArmedPixmap(old) != XmUNSPECIFIED_PIXMAP)
          {
             _XmArrowPixmapCacheDelete((XtPointer) CBG_ArmedPixmap(old));
             _XmArrowPixmapCacheDelete((XtPointer) CBG_CascadePixmap(old));
          }
          CBG_ArmedPixmap(new_w) = XmUNSPECIFIED_PIXMAP;
	  size_cascade (new_w);
       } else
          if ( (LabG_MenuType(new_w) != XmMENU_OPTION) &&
	       (((CBG_CascadePixmap(new_w) ==  XmUNSPECIFIED_PIXMAP) &&
		  (!CBG_Submenu(old) && CBG_Submenu(new_w))) ||
	        ((CBG_ArmedPixmap(old) != XmUNSPECIFIED_PIXMAP) &&
	          (LabG_TextRect_height(old) != LabG_TextRect_height(new_w)))) )
          {
                _XmArrowPixmapCacheDelete((XtPointer) CBG_ArmedPixmap(old));
                _XmArrowPixmapCacheDelete((XtPointer) CBG_CascadePixmap(old));
                CBG_ArmedPixmap(new_w) = XmUNSPECIFIED_PIXMAP;
                CBG_CascadePixmap(new_w) = XmUNSPECIFIED_PIXMAP;
                _XmCreateArrowPixmaps((Widget) new_w);
                size_cascade (new_w);
          }
	  
       /*
        * resize gadget if cascade appeared or disappeared, or if the
	* cascade pixmap changed size.
	*/
       if ((CBG_CascadePixmap (old) != CBG_CascadePixmap (new_w))  ||
	    (LabG_LabelType(old) != LabG_LabelType(new_w)) ||
	    (CBG_Submenu(old) != CBG_Submenu(new_w)))
       {
	  setup_cascade (new_w, adjustWidth, adjustHeight);

	  /* if there wasn't a cascade, and still isn't, don't redraw */
	  if (CBG_Submenu(old) || CBG_Submenu(new_w))
	      flag = TRUE;
       }

       /* make sure that other changes did not scrunch our pixmap */
       else if (CBG_Submenu(new_w))
       {
	  if ((new_w->gadget.highlight_thickness !=
	       old->gadget.highlight_thickness)                     ||
	      (new_w->gadget.shadow_thickness !=
	       old->gadget.shadow_thickness)                        ||
	      (LabG_MarginRight (new_w) != LabG_MarginRight (old))    ||
	      (LabG_MarginHeight (new_w) != LabG_MarginHeight (old))  ||
	      (LabG_MarginTop (new_w) != LabG_MarginTop (old))	    ||
	      (LabG_MarginBottom (new_w) != LabG_MarginBottom (old)))
	  {

             setup_cascade (new_w, adjustWidth, adjustHeight);
	     flag = TRUE;
	  }

	  else if ((LabG_MarginWidth(new_w) != LabG_MarginWidth(old)) ||
		   (new_w->rectangle.width != old->rectangle.width)   ||
		   (new_w->rectangle.height != old->rectangle.height))
	      
	  {
	     position_cascade (new_w);
	     flag = TRUE;
	  }
       }
    }
    
    /* don't allow this to change */
    new_w->gadget.event_mask = XmARM_EVENT | XmACTIVATE_EVENT | 
                             XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |
                             XmENTER_EVENT | XmLEAVE_EVENT | XmHELP_EVENT;

    return (flag);
}


/*
 * Initialize
 */
static void 
#ifdef _NO_PROTO
Initialize( rw, nw, args, num_args )
        Widget rw ;
        Widget nw ;
        ArgList args ;
        Cardinal *num_args ;
#else
Initialize(
        Widget rw,
        Widget nw,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
        XmCascadeButtonGadget req = (XmCascadeButtonGadget) rw ;
        XmCascadeButtonGadget new_w = (XmCascadeButtonGadget) nw ;
   Boolean adjustWidth = FALSE;
   Boolean adjustHeight = FALSE;


   XmRowColumnWidget    submenu = (XmRowColumnWidget) CBG_Submenu (new_w);

   if (! (LabG_MenuType(new_w) == XmMENU_BAR ||
	  LabG_MenuType(new_w) == XmMENU_PULLDOWN ||
	  LabG_MenuType(new_w) == XmMENU_POPUP    ||
	  LabG_MenuType(new_w) == XmMENU_OPTION))
   {
      _XmWarning( (Widget) new_w, WRONGPARENT);
   }

   /* if menuProcs is not set up yet, try again */

   if (xmLabelGadgetClassRec.label_class.menuProcs == NULL)
       xmLabelGadgetClassRec.label_class.menuProcs =
	   (XmMenuProc) _XmGetMenuProcContext();

   CBG_ArmedPixmap(new_w) =  XmUNSPECIFIED_PIXMAP;

   /*
    * if the user did not specify a margin width, and we are
    * in a menuBar, set up the default.  First, find out what was the
    * request value (not in request since this is in the cached data)
    */
   if (LabG_MenuType(new_w) == XmMENU_BAR)
   {
      Dimension requestedMarginWidth;
      XtResource request_resources;

      request_resources.resource_name = XmNmarginWidth;
      request_resources.resource_class = XmCMarginWidth;
      request_resources.resource_type = XmRHorizontalDimension;
      request_resources.resource_size = sizeof (Dimension);
      request_resources.default_type = XmRImmediate;
      request_resources.resource_offset = 0;
      request_resources.default_addr = (XtPointer) XmINVALID_DIMENSION;
   
      XtGetSubresources(XtParent(new_w), &requestedMarginWidth, XtName(new_w),
			new_w->object.widget_class->core_class.class_name,
			&request_resources, 1, args, *num_args);

      if (requestedMarginWidth == XmINVALID_DIMENSION)
      {
	 LabG_MarginWidth(new_w) = 6;
      }
   }

   /* for other menu types, we may need to initialize the cascade pixmap */
   else
   if ((LabG_MenuType(new_w) != XmMENU_OPTION))
   {
      if (submenu && CBG_CascadePixmap(new_w) == XmUNSPECIFIED_PIXMAP)
	 _XmCreateArrowPixmaps((Widget) new_w);
   }

   /* CR 7651: Clear before setting. */
   CBG_Armed(new_w) = 0;
   CBG_SetArmed(new_w, FALSE);
   CBG_SetTraverse (new_w, FALSE);
   CBG_Timer(new_w) = 0;

   if ((submenu) &&
       ((! XmIsRowColumn(submenu)) ||
	(RC_Type(submenu) != XmMENU_PULLDOWN)))
   {
      submenu = NULL;
      _XmWarning( (Widget) new_w, WRONGSUBMENU);
   }

   if (CBG_MapDelay(new_w) < 0) 
   {
      CBG_MapDelay(new_w) = MAP_DELAY_DEFAULT;
      _XmWarning( (Widget) new_w, WRONGMAPDELAY);
   }
       
   /* call submenu's class function to set the link  */
   if (submenu != NULL)
      (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_SUBMENU,
						       CBG_Submenu(new_w), 
						       TRUE, new_w);

   if (LabG_MenuType(new_w) == XmMENU_PULLDOWN	||
       LabG_MenuType(new_w) == XmMENU_POPUP ||
       LabG_MenuType(new_w) == XmMENU_OPTION)
   {
      if (req->rectangle.width <= 0)
	  adjustWidth = TRUE;
      
      if (req->rectangle.height <= 0)
	  adjustHeight = TRUE;
      
      /* get pixmap size and set up gadget to allow room for it */
      size_cascade (new_w);
      setup_cascade (new_w, adjustWidth, adjustHeight);
   }

   if (LabG_MenuType(new_w) == XmMENU_BAR ||
       LabG_MenuType(new_w) == XmMENU_PULLDOWN	||
       LabG_MenuType(new_w) == XmMENU_POPUP)
   {
      new_w->gadget.traversal_on = TRUE;
   }
	

   /* 
    * initialize the input types 
    */
   new_w->gadget.event_mask = XmARM_EVENT | XmACTIVATE_EVENT | 
       XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT |
	   XmENTER_EVENT | XmLEAVE_EVENT | XmHELP_EVENT;

}


/*
 *************************************************************************
 *
 * Public Routines                                                        
 *
 *************************************************************************
 */
Widget 
#ifdef _NO_PROTO
XmCreateCascadeButtonGadget( parent, name, al, ac )
        Widget parent ;
        char *name ;
        ArgList al ;
        Cardinal ac ;
#else
XmCreateCascadeButtonGadget(
        Widget parent,
        char *name,
        ArgList al,
        Cardinal ac )
#endif /* _NO_PROTO */
{
    Widget cb;
 
    cb = XtCreateWidget(name, xmCascadeButtonGadgetClass, parent, al, ac);

    return (cb);
}


/*
 * Arm or disarm the gadget.  This routine does not pop up or down submenus
 */
void 
#ifdef _NO_PROTO
XmCascadeButtonGadgetHighlight( wid, highlight )
        Widget wid ;
        Boolean highlight ;
#else
XmCascadeButtonGadgetHighlight(
        Widget wid,
#if NeedWidePrototypes
        int highlight )
#else
        Boolean highlight )
#endif /* NeedWidePrototypes */
#endif /* _NO_PROTO */
{
   XmCascadeButtonGadget cb  = (XmCascadeButtonGadget) wid;
   if ((cb) && XmIsCascadeButtonGadget(cb))
   {
      if (highlight)
         Arm (cb);

      else
         Disarm (cb, FALSE);
   }
}

/****************************************************
 *   Functions for manipulating Secondary Resources.
 *********************************************************/
/*
 * GetCascadeBGSecResData()
 *    Create a XmSecondaryResourceDataRec for each secondary resource;
 *    Put the pointers to these records in an array of pointers;
 *    Return the pointer to the array of pointers.
 */
/*ARGSUSED*/
static Cardinal 
#ifdef _NO_PROTO
GetCascadeBGClassSecResData( w_class, data_rtn )
        WidgetClass w_class ;
        XmSecondaryResourceData **data_rtn ;
#else
GetCascadeBGClassSecResData(
        WidgetClass w_class,
        XmSecondaryResourceData **data_rtn )
#endif /* _NO_PROTO */
{   int arrayCount /* = 0 */;
    XmBaseClassExt  bcePtr;
    String  resource_class, resource_name;
    XtPointer  client_data;

    bcePtr = &(CascadeBGClassExtensionRec );
    client_data = NULL;
    resource_class = NULL;
    resource_name = NULL;
    arrayCount =
      _XmSecondaryResourceData ( bcePtr, data_rtn, client_data,
                                resource_name, resource_class,
                                GetCascadeBGClassSecResBase) ;
    return (arrayCount);
}

/*
 * GetCascadeBGClassResBase ()
 *   retrun the address of the base of resources.
 */
static XtPointer 
#ifdef _NO_PROTO
GetCascadeBGClassSecResBase( widget, client_data )
        Widget widget ;
        XtPointer client_data ;
#else
GetCascadeBGClassSecResBase(
        Widget widget,
        XtPointer client_data )
#endif /* _NO_PROTO */
{       XtPointer  widgetSecdataPtr;
    size_t labg_cache_size = sizeof (XmLabelGCacheObjPart);
    size_t cascadebg_cache_size = sizeof (XmCascadeButtonGCacheObjPart);
        char *cp;

    widgetSecdataPtr = (XtPointer)
                      (XtMalloc ( labg_cache_size + cascadebg_cache_size + 1));

    if (widgetSecdataPtr)
          { cp = (char *) widgetSecdataPtr;
            memcpy( cp, LabG_Cache(widget), labg_cache_size);
            cp += labg_cache_size;
            memcpy( cp, CBG_Cache(widget), cascadebg_cache_size);
          }
/* else Warning: error cannot allocate Memory */
/*     widgetSecdataPtr = (XtPointer) ( LabG_Cache(widget)); */

        return (widgetSecdataPtr);
}

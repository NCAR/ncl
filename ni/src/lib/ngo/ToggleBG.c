/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993, 1994 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.4
*/ 
#ifdef REV_INFO
#ifndef lint
static char rcsid[] = "$RCSfile: ToggleBG.c,v $ $Revision: 1.1 $ $Date: 1998-09-18 23:47:36 $"
#endif
#endif
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
/*
 * Include files & Static Routine Definitions
 */
#include <stdio.h>

#include <Xm/XmP.h>
#include <X11/ShellP.h>
#include <Xm/BaseClassP.h>
#include <Xm/CacheP.h>
#include <Xm/CascadeB.h>
#include <Xm/DrawP.h>
#include <Xm/ExtObjectP.h>
#include <Xm/LabelGP.h>
#include <Xm/ManagerP.h>
#include <Xm/MenuUtilP.h>
#include <ncarg/ngo/ToggleBGP.h>

#define XmINVALID_TYPE  255	/* dynamic default flag for IndicatorType */
#define XmINVALID_BOOLEAN 85    /* dynamic default flag for VisibleWhenOff */

#ifndef Max
#define Max(x,y) (((x)>(y))?(x):(y))
#endif
#ifndef Min
#define Min(x,y) (((x)<(y))?(x):(y))
#endif

#define PixmapOn(w)           (TBG_OnPixmap(w))
#define PixmapOff(w)          (LabG_Pixmap(w))
#define Pixmap_Insen_On(w)     (TBG_InsenPixmap(w))
#define Pixmap_Insen_Off(w)    (LabG_PixmapInsensitive(w))
#define IsNull(p)              (p == XmUNSPECIFIED_PIXMAP)
#define IsOn(w)                (TBG_VisualSet(w))


/********    Static Function Declarations    ********/
#ifdef _NO_PROTO

static void ClassInitialize() ;
static void ClassPartInitialize() ;
static void SecondaryObjectCreate() ;
static void InitializePosthook() ;
static Boolean VisualChange() ;
static void InputDispatch() ;
static void SetAndDisplayPixmap() ;
static void Help() ;
static void ToggleButtonCallback() ;
static void Leave() ;
static void Enter() ;
static void Arm() ;
static void Select() ;
static void Disarm() ;
static void ArmAndActivate() ;
static void BtnDown() ;
static void BtnUp() ;
static void GetGC() ;
static void Initialize() ;
static void Destroy() ;
static void DrawToggle() ;
static void BorderHighlight() ;
static void BorderUnhighlight() ;
static void KeySelect() ;
static void ComputeSpace() ;
static void Redisplay() ;
static void Resize() ;
static Boolean SetValuesPrehook() ;
static void GetValuesPrehook() ;
static void GetValuesPosthook() ;
static Boolean SetValuesPosthook() ;
static Boolean SetValues() ;
static Cardinal GetToggleBGClassSecResData() ;
static XtPointer GetToggleBGClassSecResBase() ;
static void DrawToggleLabel() ;
static void DrawToggleShadow() ;
static void SetToggleSize() ;

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
static Boolean VisualChange( 
                        Widget wid,
                        Widget cmw,
                        Widget nmw) ;
static void InputDispatch( 
                        Widget wid,
                        XEvent *event,
                        Mask event_mask) ;
static void SetAndDisplayPixmap( 
                        Widget w,
                        XEvent *event,
                        Region region) ;
static void Help( 
                        XmToggleButtonGadget tb,
                        XEvent *event) ;
static void ToggleButtonCallback( 
                        XmToggleButtonGadget data,
                        unsigned int reason,
                        unsigned int value,
                        XEvent *event) ;
static void Leave( 
                        XmToggleButtonGadget w,
                        XEvent *event) ;
static void Enter( 
                        XmToggleButtonGadget w,
                        XEvent *event) ;
static void Arm( 
                        Widget w,
                        XEvent *event) ;
static void Select( 
                        XmToggleButtonGadget tb,
                        XEvent *event) ;
static void Disarm( 
                        XmToggleButtonGadget tb,
                        XEvent *event) ;
static void ArmAndActivate( 
                        Widget wid,
                        XEvent *event,
                        String *params,
                        Cardinal *num_params) ;
static void BtnDown( 
                        XmToggleButtonGadget tb,
                        XEvent *event) ;
static void BtnUp( 
                        XmToggleButtonGadget tb,
                        XEvent *event) ;
static void GetGC( 
                        XmToggleButtonGadget tw) ;
static void Initialize( 
                        Widget rw,
                        Widget nw,
                        ArgList args,
                        Cardinal *num_args) ;
static void Destroy( 
                        Widget w) ;
static void DrawToggle( 
                        XmToggleButtonGadget w) ;
static void BorderHighlight( 
                        Widget wid) ;
static void BorderUnhighlight( 
                        Widget wid) ;
static void KeySelect( 
                        XmToggleButtonGadget tb,
                        XEvent *event) ;
static void ComputeSpace( 
                        XmToggleButtonGadget tb) ;
static void Redisplay( 
                        Widget w,
                        XEvent *event,
                        Region region) ;
static void Resize( 
                        Widget w) ;
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
                        Widget current,
                        Widget request,
                        Widget new_w,
                        ArgList args,
                        Cardinal *num_args) ;
static Cardinal GetToggleBGClassSecResData( 
                        WidgetClass w_class,
                        XmSecondaryResourceData **data_rtn) ;
static XtPointer GetToggleBGClassSecResBase( 
                        Widget widget,
                        XtPointer client_data) ;
static void DrawToggleLabel( 
                        XmToggleButtonGadget tb) ;
static void DrawToggleShadow( 
                        XmToggleButtonGadget tb) ;
static void SetToggleSize( 
                        XmToggleButtonGadget newtbg) ;

#endif /* _NO_PROTO */
/********    End Static Function Declarations    ********/


/*************************************<->*************************************
 *
 *
 *   Description:  resource list for class: ToggleButton
 *   -----------
 *
 *   Provides default resource settings for instances of this class.
 *   To get full set of default settings, examine resouce list of super
 *   classes of this class.
 *
 *************************************<->***********************************/

static XtResource cache_resources[] = 
{
 {
     XmNindicatorSize,
     XmCIndicatorSize,
     XmRVerticalDimension,
     sizeof(Dimension),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.indicator_dim),
     XmRImmediate, (XtPointer) XmINVALID_DIMENSION
   },

   {
     XmNindicatorType, XmCIndicatorType, XmRIndicatorType,sizeof(unsigned char),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.ind_type),
     XmRImmediate, (XtPointer) XmINVALID_TYPE
   },

   {
     XmNvisibleWhenOff, XmCVisibleWhenOff, XmRBoolean, sizeof(Boolean),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.visible),
     XmRImmediate, (XtPointer) XmINVALID_BOOLEAN
   },

   {
     XmNspacing, 
     XmCSpacing, 
     XmRHorizontalDimension,
     sizeof(Dimension),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.spacing),
     XmRImmediate, (XtPointer) 4
   },

   {
     XmNselectPixmap, XmCSelectPixmap, XmRGadgetPixmap, sizeof(Pixmap),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.on_pixmap),
     XmRImmediate, (XtPointer) XmUNSPECIFIED_PIXMAP 
   },

   {
     XmNselectInsensitivePixmap, XmCSelectInsensitivePixmap, XmRGadgetPixmap, sizeof(Pixmap),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.insen_pixmap),
     XmRImmediate, (XtPointer) XmUNSPECIFIED_PIXMAP
   },

   {
      XmNindicatorOn, XmCIndicatorOn, XmRBoolean, sizeof (Boolean),
      XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.ind_on),
      XmRImmediate, (XtPointer) True
   },

   {
      XmNfillOnSelect, XmCFillOnSelect, XmRBoolean, sizeof (Boolean),
      XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.fill_on_select),
      XmRImmediate, (XtPointer) XmINVALID_BOOLEAN

   },

   {
      XmNselectColor, XmCSelectColor, XmRPixel, sizeof (Pixel),
      XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.select_color),
      XmRCallProc, (XtPointer) _XmSelectColorDefault

   },

};


/************************************************
The uncached resources for ToggleButton
************************************************/
 

static XtResource resources[] =
{
   {
      XmNset, XmCSet, XmRBoolean, sizeof(Boolean),
      XtOffsetOf( struct _XmToggleButtonGadgetRec, toggle.set),
      XmRImmediate, (XtPointer) False
   },

   {
      XmNvalueChangedCallback, XmCValueChangedCallback, XmRCallback,
      sizeof (XtCallbackList),
      XtOffsetOf( struct _XmToggleButtonGadgetRec, toggle.value_changed_CB),
      XmRPointer, (XtPointer)NULL 
   },

   {
      XmNarmCallback, XmCArmCallback, XmRCallback,
      sizeof (XtCallbackList),
      XtOffsetOf( struct _XmToggleButtonGadgetRec, toggle.arm_CB),
      XmRPointer, (XtPointer)NULL 
   },

   {
      XmNdisarmCallback, XmCDisarmCallback, XmRCallback,
      sizeof (XtCallbackList),
      XtOffsetOf( struct _XmToggleButtonGadgetRec, toggle.disarm_CB),
      XmRPointer, (XtPointer)NULL 
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
   }
};

/*  Definition for resources that need special processing in get values  */

static XmSyntheticResource cache_syn_resources[] =
{
   { XmNspacing,
     sizeof (Dimension),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.spacing),
     _XmFromHorizontalPixels,
     _XmToHorizontalPixels
   },

   { XmNindicatorSize,
     sizeof (Dimension),
     XtOffsetOf( struct _XmToggleButtonGCacheObjRec, toggle_cache.indicator_dim),
     _XmFromVerticalPixels,
     _XmToVerticalPixels
   },


};

/*****************************************************************
 * 
 *   Class Record definitions
 *
 ****************************************************************/

static XmCacheClassPart ToggleButtonClassCachePart = {
    {NULL, 0, 0},        /* head of class cache list */
    _XmCacheCopy,       /* Copy routine     */
    _XmCacheDelete,     /* Delete routine   */
    _XmToggleBCacheCompare,    /* Comparison routine   */
};

static XmBaseClassExtRec   ToggleBGClassExtensionRec = {
    NULL,    				/*   next_extension    */
    NULLQUARK, 				/* record_typ  */
    XmBaseClassExtVersion,		/*  version  */
    sizeof(XmBaseClassExtRec),		/* record_size  */
    XmInheritInitializePrehook, 	/*  initializePrehook  */
    SetValuesPrehook, 			/* setValuesPrehoo  */
    InitializePosthook, 		/* initializePosthook  */
    SetValuesPosthook, 			/* setValuesPosthook  */
    (WidgetClass)&xmToggleButtonGCacheObjClassRec,	/* secondaryObjectClass */
    SecondaryObjectCreate,  	        /* secondaryObjectCreate */
    GetToggleBGClassSecResData,         /* getSecResData  */
    {0},           			/* Other Flags  */
    GetValuesPrehook, 			/* getValuesPrehoo  */
    GetValuesPosthook, 			/* getValuesPosthoo  */
    (XtWidgetClassProc)NULL,            /* classPartInitPrehook */
    (XtWidgetClassProc)NULL,            /* classPartInitPosthook */
    NULL,                               /* ext_resources */
    NULL,                               /* compiled_ext_resources */
    0,                                  /* num_ext_resources */
    FALSE,                              /* use_sub_resources */
    XmInheritWidgetNavigable,           /* widgetNavigable */
    XmInheritFocusChange,               /* focusChange */
    (XmWrapperData)NULL,		/* wrapperData */
};


/* ext rec static initialization */
externaldef(xmtogglebuttongcacheobjclassrec)
XmToggleButtonGCacheObjClassRec xmToggleButtonGCacheObjClassRec =
{
  {
      /* superclass         */    (WidgetClass) &xmLabelGCacheObjClassRec,
      /* class_name         */    "XmToggleButtonGadget",
      /* widget_size        */    sizeof(XmToggleButtonGCacheObjRec),
      /* class_initialize   */    (XtProc)NULL,
      /* chained class init */    (XtWidgetClassProc)NULL,
      /* class_inited       */    False,
      /* initialize         */    (XtInitProc)NULL,
      /* initialize hook    */    (XtArgsProc)NULL,
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
      /* destroy            */    (XtWidgetProc)NULL,
      /* resize             */    NULL,
      /* expose             */    NULL,
      /* set_values         */    (XtSetValuesFunc)NULL,
      /* set values hook    */    (XtArgsFunc)NULL,
      /* set values almost  */    NULL,
      /* get values hook    */    (XtArgsProc)NULL,
      /* accept_focus       */    NULL,
      /* version            */    XtVersion,
      /* callback offsetlst */    NULL,
      /* default trans      */    NULL,
      /* query geo proc     */    NULL,
      /* display accelerator*/    NULL,
      /* extension record   */    NULL,
  },

  {
      /* synthetic resources */   cache_syn_resources,
      /* num_syn_resources   */   XtNumber(cache_syn_resources),
      /* extension           */   NULL,
  }
};

/*************************************<->*************************************
 *
 *
 *   Description:  global class record for instances of class: ToggleButton
 *   -----------
 *
 *   Defines default field settings for this class record.
 *
 *************************************<->***********************************/

XmGadgetClassExtRec _XmToggleBGadClassExtRec = {
     NULL,
     NULLQUARK,
     XmGadgetClassExtVersion,
     sizeof(XmGadgetClassExtRec),
     XmInheritBaselineProc,                  /* widget_baseline */
     XmInheritDisplayRectProc,               /* widget_display_rect */
};

externaldef(xmtogglebuttongadgetclassrec)
	XmToggleButtonGadgetClassRec xmToggleButtonGadgetClassRec = {
   {
    /* superclass	  */	(WidgetClass) &xmLabelGadgetClassRec,
    /* class_name	  */	"XmToggleButtonGadget",
    /* widget_size	  */	sizeof(XmToggleButtonGadgetRec),
    /* class_initialize   */    ClassInitialize,
    /* class_part_init    */    ClassPartInitialize,				
    /* class_inited       */	FALSE,
    /* INITialize	  */	Initialize,
    /* initialize_hook    */    (XtArgsProc)NULL,
    /* realize	  */	NULL,
    /* actions		  */	NULL,
    /* num_actions	  */	0,
    /* resources	  */	resources,
    /* num_resources	  */	XtNumber(resources),
    /* xrm_class	  */	NULLQUARK,
    /* compress_motion	  */	TRUE,
    /* compress_exposure  */	XtExposeCompressMaximal,
    /* compress_enterlv   */    TRUE,
    /* visible_interest	  */	FALSE,
    /* destroy		  */	Destroy,
    /* resize		  */	Resize,
    /* expose		  */	Redisplay,
    /* set_values	  */	SetValues,
    /* set_values_hook    */    (XtArgsFunc)NULL,
    /* set_values_almost  */    XtInheritSetValuesAlmost,
    /* get_values_hook    */	(XtArgsProc)NULL,
    /* accept_focus       */  NULL,
    /* version            */	XtVersion,
    /* callback_private   */    NULL,
    /* tm_table           */    NULL,
    /* query_geometry     */	XtInheritQueryGeometry, 
    /* display_accelerator */   NULL,
    /* extension          */    (XtPointer)&ToggleBGClassExtensionRec,
   },

   {        /* gadget class record */
    /* border_highlight             */	BorderHighlight,
    /* border_unhighlight           */  BorderUnhighlight,
    /* arm_and_activate             */  ArmAndActivate,
    /* input_dispatch               */  InputDispatch,
    /* visual_change                */  VisualChange,
    /* syn resources                */  NULL,         
    /* num syn_resources            */  0,    
    /* class cache part   	    */	&ToggleButtonClassCachePart,
    /* extension                    */  (XtPointer)&_XmToggleBGadClassExtRec,
   },

   {        /* label class record */
    /* SetOverrideCallback     */    XmInheritWidgetProc,
    /* menu proc entry 	       */    XmInheritMenuProc,
    /* extension               */    NULL,
   },

   {	    /* toggle class record */
    /* extension               */    NULL,
   }
};

externaldef(xmtogglebuttongadgetclass) 
  WidgetClass xmToggleButtonGadgetClass = 
            (WidgetClass)&xmToggleButtonGadgetClassRec;

/***********************************************************
*
*  ClassInitialize
*
************************************************************/
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
   Label's and Togglebutton's resource lists are being merged into one
   and assigned to xmToggleButtonGCacheObjClassRec. This is for performance
   reasons, since, instead of two calls to XtGetSubResources() XtGetSubvaluse()
   and XtSetSubvalues() for both the superclass and the widget class, now
   we have just one call with a merged resource list.
   NOTE: At this point the resource lists for Label and Togglebutton do
         have unique entries, but if there are resources in the superclass
         that are being overwritten by the subclass then the merged_lists
         need to be created differently.
****************************************************************************/

  wc_num_res = xmToggleButtonGCacheObjClassRec.object_class.num_resources;

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
        xmToggleButtonGCacheObjClassRec.object_class.resources[i];
  }

  xmToggleButtonGCacheObjClassRec.object_class.resources = merged_list;
  xmToggleButtonGCacheObjClassRec.object_class.num_resources =
                wc_num_res + sc_num_res ;

  ToggleBGClassExtensionRec.record_type = XmQmotif;
}

/************************************************************************
 * 
 * ClassPartInitialize
 *   Set up fast subclassing for the gadget.
 *
 ***********************************************************************/
static void 
#ifdef _NO_PROTO
ClassPartInitialize( wc )
        WidgetClass wc ;
#else
ClassPartInitialize(
        WidgetClass wc )
#endif /* _NO_PROTO */
{
  _XmFastSubclassInit (wc, XmTOGGLE_BUTTON_GADGET_BIT);
}

/*******************************************************************
 *
 *  _XmToggleBCacheCompare
 *
 *******************************************************************/
 int 
#ifdef _NO_PROTO
_XmToggleBCacheCompare( A, B )
        XtPointer A ;
        XtPointer B ;
#else
_XmToggleBCacheCompare(
        XtPointer A,
        XtPointer B )
#endif /* _NO_PROTO */
{
        XmToggleButtonGCacheObjPart *toggleB_inst = 
                                            (XmToggleButtonGCacheObjPart *) A ;
        XmToggleButtonGCacheObjPart *toggleB_cache_inst =
                                            (XmToggleButtonGCacheObjPart *) B ;
    if((toggleB_inst->ind_type == toggleB_cache_inst->ind_type) &&
       (toggleB_inst->visible == toggleB_cache_inst->visible) &&
       (toggleB_inst->spacing == toggleB_cache_inst->spacing) &&
       (toggleB_inst->indicator_dim == toggleB_cache_inst->indicator_dim) &&
       (toggleB_inst->on_pixmap == toggleB_cache_inst->on_pixmap) &&
       (toggleB_inst->insen_pixmap == toggleB_cache_inst->insen_pixmap) &&
       (toggleB_inst->ind_on == toggleB_cache_inst->ind_on) &&
       (toggleB_inst->fill_on_select == toggleB_cache_inst->fill_on_select) &&
       (toggleB_inst->select_color == toggleB_cache_inst->select_color) &&
       (toggleB_inst->select_GC == toggleB_cache_inst->select_GC) &&
       (toggleB_inst-> background_gc == toggleB_cache_inst->background_gc)) 
       return 1;
    else
       return 0;
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
     * Since the resource lists for label and togglebutton were merged at
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

  ((XmToggleButtonGCacheObject)newSec)->ext.extensionType = XmCACHE_EXTENSION;
  ((XmToggleButtonGCacheObject)newSec)->ext.logicalParent = new_w;

  _XmPushWidgetExtData(new_w, extData,
                         ((XmToggleButtonGCacheObject)newSec)->ext.extensionType);
  memcpy(reqSec, newSec, size);

  /*
   * fill out cache pointers
   */


  LabG_Cache(new_w) = &(((XmLabelGCacheObject)extData->widget)->label_cache);
  LabG_Cache(req) = &(((XmLabelGCacheObject)extData->reqWidget)->label_cache);

  TBG_Cache(new_w) =
	&(((XmToggleButtonGCacheObject)extData->widget)->toggle_cache);
  TBG_Cache(req) =
        &(((XmToggleButtonGCacheObject)extData->reqWidget)->toggle_cache);

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
    XmToggleButtonGadget  tbw = (XmToggleButtonGadget)new_w;

   /*
    * - register parts in cache.
    * - update cache pointers
    * - and free req
    */

    LabG_Cache(tbw) = (XmLabelGCacheObjPart *)
                      _XmCachePart( LabG_ClassCachePart(tbw),
		                    (XtPointer) LabG_Cache(tbw),
				    sizeof(XmLabelGCacheObjPart));

    TBG_Cache(tbw) = (XmToggleButtonGCacheObjPart *)
	     _XmCachePart( TBG_ClassCachePart(tbw),
                           (XtPointer) TBG_Cache(tbw),
		           sizeof(XmToggleButtonGCacheObjPart));

    /*
     * might want to break up into per-class work that gets explicitly
     * chained. For right now, each class has to replicate all
     * superclass logic in hook routine
     */

     /*
      * free the req subobject used for comparisons
      */
      _XmPopWidgetExtData((Widget) tbw, &ext, XmCACHE_EXTENSION);
      _XmExtObjFree((XtPointer)ext->widget);
      _XmExtObjFree(ext->reqWidget);
      XtFree( (char *) ext);
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
        XmGadget gw = (XmGadget) wid ;
        XmManagerWidget curmw = (XmManagerWidget) cmw ;
        XmManagerWidget newmw = (XmManagerWidget) nmw ;
        XmToggleButtonGCacheObjPart  oldCopy;
   XmToggleButtonGadget tbg = (XmToggleButtonGadget) gw;

   if (curmw->manager.foreground != newmw->manager.foreground ||
       curmw->core.background_pixel != newmw->core.background_pixel)
   {
      XtReleaseGC (XtParent (tbg), TBG_SelectGC(tbg));
      XtReleaseGC (XtParent (tbg), TBG_BackgroundGC(tbg));
  
      /* Since the GC's are cached we need to make the following calls */
      /* to update the cache correctly */
   
      _XmCacheCopy((XtPointer) TBG_Cache(tbg), &oldCopy, sizeof(XmToggleButtonGCacheObjPart));
      _XmCacheDelete ((XtPointer) TBG_Cache(tbg));
      TBG_Cache(tbg) = &oldCopy;
      GetGC (tbg);
      TBG_Cache(tbg) = (XmToggleButtonGCacheObjPart *)
      _XmCachePart(TBG_ClassCachePart(tbg),
                      (XtPointer) TBG_Cache(tbg),
                      sizeof(XmToggleButtonGCacheObjPart));
      return (True);
   }

   return (False);
}


/************************************************************************
 *
 *  InputDispatch
 *     This function catches input sent by a manager and dispatches it
 *     to the individual routines.
 *
 ************************************************************************/
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
        XmToggleButtonGadget tb = (XmToggleButtonGadget) wid ;
   if (event_mask & XmARM_EVENT)
   {
     if (LabG_MenuType(tb) == XmMENU_PULLDOWN ||
	 LabG_MenuType(tb) == XmMENU_POPUP)
      BtnDown(tb, event);
     else
      Arm ((Widget) tb, event);
   }
/* BEGIN OSF Fix pir 2097 */
   else if (event_mask & XmMULTI_ARM_EVENT)
   {
     if (LabG_MenuType(tb) != XmMENU_PULLDOWN &&
         LabG_MenuType(tb) != XmMENU_POPUP)
     {
      Arm ((Widget) tb, event);
     }
   }
/* END OSF Fix pir 2097 */

   else if (event_mask & XmACTIVATE_EVENT)
   {
     if (LabG_MenuType(tb) == XmMENU_PULLDOWN ||
	 LabG_MenuType(tb) == XmMENU_POPUP)
     {
      if (event->type == ButtonRelease)
        BtnUp(tb, event);
      else /* Assume KeyPress or KeyRelease */
        KeySelect (tb, event);
     }
     else
     {
      Select(tb, event);
      Disarm (tb, event);
     }
   }
/* BEGIN OSF Fix pir 2097 */
   else if (event_mask & XmMULTI_ACTIVATE_EVENT)
   {
     if (LabG_MenuType(tb) != XmMENU_PULLDOWN &&
         LabG_MenuType(tb) != XmMENU_POPUP)
     {
      Select(tb, event);
      Disarm (tb, event);
     }
   }
/* END OSF Fix pir 2097 */

   else if (event_mask & XmHELP_EVENT) Help (tb, event);
   else if (event_mask & XmENTER_EVENT) Enter (tb, event);
   else if (event_mask & XmLEAVE_EVENT) Leave (tb, event);
   else if (event_mask & XmFOCUS_IN_EVENT)
               (*(((XmToggleButtonGadgetClass) XtClass( tb))
                              ->gadget_class.border_highlight))( (Widget) tb) ;
   else if (event_mask & XmFOCUS_OUT_EVENT)
               (*(((XmToggleButtonGadgetClass) XtClass( tb))
                              ->gadget_class.border_unhighlight))
		   ( (Widget) tb) ;
   else if (event_mask & XmBDRAG_EVENT) _XmProcessDrag ((Widget) tb,
                                                            event, NULL, NULL);
}

/*********************************************************************
 *
 * redisplayPixmap
 *   does the apropriate calculations based on the toggle button's
 *   current pixmap and calls label's Redisplay routine.
 *
 * This routine was added to fix CR 4839 and CR 4838
 * D. Rand 7/6/92
 * 
 ***********************************************************************/

static void
#ifdef _NO_PROTO
redisplayPixmap(tb, event, region)
     XmToggleButtonGadget tb;
     XEvent *event;
     Region region;
#else
redisplayPixmap(XmToggleButtonGadget tb, XEvent *event, Region region)
#endif
{
  Pixmap todo;
  Window root;
  int rx, ry;
  unsigned int onH = 0, onW = 0, border, d;
  unsigned int w, h;
  int x, y, offset;
  short saveY;
  unsigned short saveWidth, saveHeight;
  
  offset = tb -> gadget.highlight_thickness + tb -> gadget.shadow_thickness;

  x = offset + LabG_MarginWidth(tb) + LabG_MarginLeft(tb);

  y = offset + LabG_MarginHeight(tb) + LabG_MarginTop(tb);

  w = tb -> rectangle.width - x - offset 
    - LabG_MarginRight(tb) - LabG_MarginWidth(tb);

  w = Max(0, (int)w);

  h = tb -> rectangle.height - y - offset - LabG_MarginBottom(tb)
    - LabG_MarginHeight(tb);

  h = Max(0, (int)h);

  x += tb -> rectangle.x;

  y += tb -> rectangle.y;

  XClearArea(XtDisplay(tb), XtWindow(tb), x, y, w, h, False);

  todo = tb -> label.pixmap;

  if ( (! XtIsSensitive ((Widget) tb)) && tb -> label.pixmap_insen )
    todo = tb -> label.pixmap_insen;
      
  if ( ! IsNull(todo) )
    XGetGeometry (XtDisplay(tb),
		  todo,
		  &root,	/* returned root window */
		  &rx, &ry,	/* returned x, y of pixmap */
		  &onW, &onH,	/* returned width, height of pixmap */
		  &border,	/* returned border width */
		  &d);		/* returned depth */

  saveY = LabG_TextRect_y(tb);
  saveWidth = LabG_TextRect_width(tb);
  saveHeight = LabG_TextRect_height(tb);

  h = (tb -> rectangle.height - onH) / 2 ;
  LabG_TextRect_y(tb) = Max(0, (int)h);
  LabG_TextRect_height(tb) = onH;
  LabG_TextRect_width(tb) = onW;
  (* xmLabelGadgetClassRec.rect_class.expose) ((Widget) tb, event, region);

  LabG_TextRect_y(tb) = saveY;
  LabG_TextRect_width(tb) = saveWidth;
  LabG_TextRect_height(tb) = saveHeight;
}


/***********************************************************************
 *
 * SetAndDisplayPixmap
 *    Sets the appropriate on, off pixmap in label's pixmap field and
 *    calls label's Redisplay routine.
 *
 ***********************************************************************/
static void 
#ifdef _NO_PROTO
SetAndDisplayPixmap( w, event, region )
        Widget w ;
        XEvent *event ;
        Region region ;
#else
SetAndDisplayPixmap(
        Widget w,
        XEvent *event,
        Region region )
#endif /* _NO_PROTO */
{
    XmToggleButtonGadget tb = (XmToggleButtonGadget) w ;
 if (IsOn (tb))
 {
  if (XtIsSensitive(w))
  {
   if ( ! IsNull (PixmapOn (tb)))
   {
     Pixmap tempPix;

     tempPix = PixmapOff(tb);
     PixmapOff(tb) = PixmapOn(tb);
     redisplayPixmap(tb, event, region);
     PixmapOff(tb) = tempPix;
   }
   else
     redisplayPixmap(tb, event, region);
  }

  else
  {
   if ( ! IsNull (Pixmap_Insen_On (tb)))
   {
     Pixmap tempPix;

     tempPix = Pixmap_Insen_Off(tb);
     Pixmap_Insen_Off(tb) = Pixmap_Insen_On(tb);
     redisplayPixmap(tb, event, region);
     Pixmap_Insen_Off(tb)  = tempPix;
   }
   else
     redisplayPixmap(tb, event, region);
  }
 }

 else
   redisplayPixmap(tb, event, region);
}



/*************************************************************************
 *
 *  Help
 *     This routine is called if the user has made a help selection
 *     on the gadget.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
Help( tb, event )
        XmToggleButtonGadget tb ;
        XEvent *event ;
#else
Help(
        XmToggleButtonGadget tb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   Boolean is_menupane = (LabG_MenuType(tb) == XmMENU_PULLDOWN) ||
			 (LabG_MenuType(tb) == XmMENU_POPUP);

   if (is_menupane)
   {
      (* xmLabelGadgetClassRec.label_class.menuProcs)
	  (XmMENU_BUTTON_POPDOWN, XtParent(tb), NULL, event, NULL);
   }

   ToggleButtonCallback(tb, XmCR_HELP, TBG_Set(tb), event);

   if (is_menupane)
   {
      (* xmLabelGadgetClassRec.label_class.menuProcs)
	 (XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL, 
	 XtParent(tb), NULL, event, NULL);
   }
}


/*************************************************************************
 *
 * ToggleButtonCallback
 *    This is the widget's application callback routine
 *
 *************************************************************************/
static void 
#ifdef _NO_PROTO
ToggleButtonCallback( data, reason, value, event )
        XmToggleButtonGadget data ;
        unsigned int reason ;
        unsigned int value ;
        XEvent *event ;
#else
ToggleButtonCallback(
        XmToggleButtonGadget data,
        unsigned int reason,
        unsigned int value,
        XEvent *event )
#endif /* _NO_PROTO */
{

    XmToggleButtonCallbackStruct temp;

    temp.reason = reason;
    temp.set= value;
    temp.event  = event;

    switch (reason)
      {
        case XmCR_VALUE_CHANGED:
            XtCallCallbackList ((Widget) data, TBG_ValueChangedCB(data), &temp);
            break;

        case XmCR_ARM          :
            XtCallCallbackList ((Widget) data, TBG_ArmCB(data), &temp);
            break;

        case XmCR_DISARM       :
            XtCallCallbackList ((Widget) data, TBG_DisarmCB(data), &temp);
            break;

        case XmCR_HELP         :
	        _XmSocorro( (Widget) data, event, NULL, NULL);
            break;
       }

}


/**************************************************************************
 *
 *   Leave
 *     This procedure is called when  the mouse button is pressed and  the
 *     cursor moves out of the widget's window. This procedure is used
 *     to change the visuals.
 *
*************************************************************************/
static void 
#ifdef _NO_PROTO
Leave( w, event )
        XmToggleButtonGadget w ;
        XEvent *event ;
#else
Leave(
        XmToggleButtonGadget w,
        XEvent *event )
#endif /* _NO_PROTO */
{
   int edge, x, y;
   Boolean fill;
   XmManagerWidget mw;

   if (LabG_MenuType(w) == XmMENU_PULLDOWN ||
       LabG_MenuType(w) == XmMENU_POPUP)
 {
    if ( _XmGetInDragMode((Widget)w) && TBG_Armed(w))
    {
       _XmClearBorder (XtDisplay (w), XtWindow (w),
		     w-> rectangle.x + w-> gadget.highlight_thickness,
		     w->rectangle .y + w-> gadget.highlight_thickness,
		     w-> rectangle.width - 2 *
			 w->gadget.highlight_thickness,
		     w-> rectangle.height - 2 *
			  w->gadget.highlight_thickness,
		     w-> gadget.shadow_thickness);

       TBG_Armed(w) = FALSE;
       
       if (TBG_DisarmCB(w))
       {
         XFlush (XtDisplay (w));

         ToggleButtonCallback(w, XmCR_DISARM, TBG_Set(w), event);
       }

    }
 }

 else
 { _XmLeaveGadget( (Widget) w, event, NULL, NULL);

   /* CR 8020: We may have armed while outside the toggle. */
   IsOn(w) = TBG_Set(w);

   mw = (XmManagerWidget) XtParent(w);
   if( TBG_IndicatorSet(w) || _XmStringEmpty(LabG__label(w)) ) {
     edge = TBG_IndicatorDim(w);
   } else {
     edge = Min((int)TBG_IndicatorDim(w), 
           Max(0, (int)w->rectangle.height - 2*(w->gadget.highlight_thickness +
                                           w->gadget.shadow_thickness +
                                          (int)LabG_MarginHeight(w)) +
                                           LabG_MarginTop(w) +
                                           LabG_MarginBottom(w)));
   }

   if (DefaultDepthOfScreen (XtScreen (w)) == 1) /* Monochrome Display */
      fill = FALSE;
   else
   {
      if ((mw->manager.top_shadow_color != TBG_SelectColor(w)) &&
          (mw->manager.bottom_shadow_color != TBG_SelectColor(w)))
         fill = TRUE;
      else
         fill = FALSE;
   }
   
   x = w->rectangle.x +
       w->gadget.highlight_thickness + w->gadget.shadow_thickness +
       LabG_MarginHeight(w);
 
   if ( TBG_IndicatorSet(w) || _XmStringEmpty(LabG__label(w)) )
      y = w->rectangle.y + (w->rectangle.height - TBG_IndicatorDim(w))/2;
   else
   {
      y = w->rectangle.y + LabG_TextRect(w).y;
      if (LabG_MenuType(w) == XmMENU_POPUP ||
          LabG_MenuType(w) == XmMENU_PULLDOWN)
         y += (TBG_IndicatorDim(w) + 2) / 4; /* adjust in menu */
   }
 

   if ((TBG_IndType(w)) == XmN_OF_MANY)
   {

    if (TBG_Armed(w) == TRUE)
    { 
      if (TBG_IndOn(w))
      {
          /* if the toggle indicator is square shaped then adjust the
             indicator width and height, so that it looks proportional
             to a diamond shaped indicator of the same width and height */

          int new_edge;

          new_edge = edge - 3 - ((edge - 10)/10); /* Subtract 3 pixels + 1  */
                                              /* pixel for every 10 pixels, */
                                              /* from the width and height. */

         /* Adjust x,y so that the indicator is centered relative to the label*/
          y = y + ((edge - new_edge) / 2);
          x = x + ((edge - new_edge) / 2);
          edge = new_edge;


          if ((TBG_Visible(w)) ||
              ((!TBG_Visible(w)) && (IsOn(w))))
          {
             _XmDrawShadows (XtDisplay (w), XtWindow (w), 
                           ((IsOn(w)) ? 
                             XmParentBottomShadowGC(w) :
                             XmParentTopShadowGC(w)),
                           ((IsOn(w)) ? 
                             XmParentTopShadowGC(w) :
                             XmParentBottomShadowGC(w)), 
                           x, y, edge, edge, 2, XmSHADOW_OUT);

    
             if (TBG_FillOnSelect(w))
                if (edge > (fill ? 4 : 6))
			XFillRectangle (XtDisplay ((Widget) w), 
                                              XtWindow ((Widget) w),
                                             ((IsOn(w)) ?
                                              TBG_SelectGC(w) :
                                              TBG_BackgroundGC(w)),
                                             ((fill) ? x+2 : x+3),
                                             ((fill) ? y+2 : y+3),
                                             ((fill) ? edge-4 : edge-6),
                                             ((fill) ? edge-4 : edge-6));
          }


          if (!TBG_Visible(w))
          {
             if (!IsOn(w))
                if (edge > 0)
			XFillRectangle( XtDisplay ((Widget) w),
                                              XtWindow ((Widget) w),
                                              TBG_BackgroundGC(w),
                                              x, y, edge, edge);
          }

     }     
     else
     {
        if (w->gadget.shadow_thickness> 0)
          DrawToggleShadow (w);
        if (TBG_FillOnSelect(w) && !LabG_IsPixmap(w))
          DrawToggleLabel (w);
     }
     if (LabG_IsPixmap(w))
     {
       SetAndDisplayPixmap((Widget) w, event, NULL);
     }
    }

   }
   else
   {
    if (TBG_Armed(w) == TRUE)
    { 
      if (TBG_IndOn(w))
      {
         if ((TBG_Visible(w)) ||
             ((!TBG_Visible(w)) && (IsOn(w))))
            _XmDrawDiamond(XtDisplay(w), XtWindow(w),
                          ((IsOn(w)) ?
                           XmParentBottomShadowGC(w) :
                           XmParentTopShadowGC(w)),
                          ((IsOn(w)) ?
                           XmParentTopShadowGC(w) :
                           XmParentBottomShadowGC(w)),
                          (((IsOn(w))  &&
                            (TBG_FillOnSelect(w))) ?
                           TBG_SelectGC(w) :
                           TBG_BackgroundGC(w)),
                          x, y, edge, edge,
                          w->gadget.shadow_thickness, fill);
      
         if (!TBG_Visible(w))
         {
            if (!IsOn(w))
               if (edge > 0)
			XFillRectangle( XtDisplay ((Widget) w),
                                             XtWindow ((Widget) w),
                                             TBG_BackgroundGC(w),
                                             x, y, edge, edge);
         }
      }
      else
      {
         if (w->gadget.shadow_thickness> 0) DrawToggleShadow (w);
         if (TBG_FillOnSelect(w) && !LabG_IsPixmap(w)) DrawToggleLabel (w);
      }
      if (LabG_IsPixmap(w))
      {
         SetAndDisplayPixmap( (Widget) w, event, NULL);
      }
    } 
   }
 }
}


/**************************************************************************
 *
 *  Enter
 *    This procedure is called when the mouse button is pressed and the
 *    cursor reenters the widget's window. This procedure changes the visuals
 *    accordingly.
 *
 **************************************************************************/
static void 
#ifdef _NO_PROTO
Enter( w, event )
        XmToggleButtonGadget w ;
        XEvent *event ;
#else
Enter(
        XmToggleButtonGadget w,
        XEvent *event )
#endif /* _NO_PROTO */
{
   int edge, x, y;
   Boolean  fill;
   XmManagerWidget mw;

   if (LabG_MenuType(w) == XmMENU_PULLDOWN ||
       LabG_MenuType(w) == XmMENU_POPUP)
   {
      if ((((ShellWidget) XtParent(XtParent(w)))->shell.popped_up) &&
          _XmGetInDragMode((Widget)w))
      {
         if (TBG_Armed(w))
	    return;

	 /* So KHelp event is delivered correctly */
	 _XmSetFocusFlag( XtParent(XtParent(w)), (1<<1), TRUE);
         XtSetKeyboardFocus(XtParent(XtParent(w)), (Widget)w);
         _XmSetFocusFlag( XtParent(XtParent(w)), (1<<1), FALSE);

	 _XmDrawShadows (XtDisplay (w), XtWindow (w),
		       XmParentTopShadowGC(w),
		       XmParentBottomShadowGC(w),
		       w->rectangle.x + w -> gadget.highlight_thickness,
		       w->rectangle.y + w -> gadget.highlight_thickness,
		       w -> rectangle.width -
			 2 * w->gadget.highlight_thickness,
		       w -> rectangle.height -
			 2 * w->gadget.highlight_thickness,
		       w -> gadget.shadow_thickness, XmSHADOW_OUT);

	 TBG_Armed(w) = TRUE;

	 if (TBG_ArmCB(w))
	 { 
	    XFlush (XtDisplay (w));
	    ToggleButtonCallback(w, XmCR_ARM, TBG_Set(w), event);
	 }
      }
   }

   else
   {  _XmEnterGadget( (Widget) w, event, NULL, NULL);  
      mw = (XmManagerWidget) XtParent(w);
      
      /* CR 8020: We may have armed while outside the toggle. */
      IsOn(w) = !TBG_Set(w);

      if( TBG_IndicatorSet(w) || _XmStringEmpty(LabG__label(w)) ) {
        edge = TBG_IndicatorDim(w);
      } else {
	edge = Min((int)TBG_IndicatorDim(w), 
		 Max(0, (int)w->rectangle.height -
		     2*(w->gadget.highlight_thickness +
			w->gadget.shadow_thickness +
			(int)LabG_MarginHeight(w)) +
		     LabG_MarginTop(w) +
		     LabG_MarginBottom(w)));
      }

      if (DefaultDepthOfScreen (XtScreen (w)) == 1) /* Monochrome Display */
        fill = FALSE;
      else
      {
        if ((mw->manager.top_shadow_color != TBG_SelectColor(w)) &&
	    (mw->manager.bottom_shadow_color != TBG_SelectColor(w)))
	  fill = TRUE;
        else
	  fill = FALSE;
      }

      x = w->rectangle.x +
	  w->gadget.highlight_thickness + w->gadget.shadow_thickness +
	      LabG_MarginHeight(w);
 
      if( TBG_IndicatorSet(w) || _XmStringEmpty(LabG__label(w)) )
         y = w->rectangle.y + (w->rectangle.height - TBG_IndicatorDim(w))/2;
      else
      {
         y = w->rectangle.y + LabG_TextRect(w).y;
         if (LabG_MenuType(w) == XmMENU_POPUP ||
             LabG_MenuType(w) == XmMENU_PULLDOWN)
            y += (TBG_IndicatorDim(w) + 2) / 4; /* adjust in menu */
      }

      if ((TBG_IndType(w)) == XmN_OF_MANY) 
      {
	 if (TBG_Armed(w) == TRUE)
	 { 
	    if (TBG_IndOn(w))
	    {
	       /* if the toggle indicator is square shaped then adjust the
		  indicator width and height, so that it looks proportional
		  to a diamond shaped indicator of the same width and height */

	       int new_edge;
	       new_edge = edge - 3 - ((edge - 10)/10);
	                                      /* Subtract 3 pixels + 1  */
                                              /* pixel for every 10 pixels, */
                                              /* from the width and height. */

	       /* Adjust x,y so that the indicator is centered relative to the
		  label*/
	       y = y + ((edge - new_edge) / 2);
	       x = x + ((edge - new_edge) / 2);
	       edge = new_edge;

	       if ((TBG_Visible(w)) ||
		   ((!TBG_Visible(w)) && (IsOn(w))))
	       {

		  _XmDrawShadows (XtDisplay (w), XtWindow (w), 
                           ((IsOn(w)) ? 
                              XmParentBottomShadowGC(w) :
                              XmParentTopShadowGC(w)),
                           ((IsOn(w)) ? 
                              XmParentTopShadowGC(w) :
                              XmParentBottomShadowGC(w)), 
                            x, y, edge, edge, 2, XmSHADOW_OUT);

		  if (TBG_FillOnSelect(w))
		      if (edge > (fill ? 4 : 6))
			  XFillRectangle(XtDisplay((Widget) w),
                                             XtWindow ((Widget) w),
                                             ((IsOn(w)) ?
                                             TBG_SelectGC(w) :
                                             TBG_BackgroundGC(w)),
                                             ((fill) ? x+2 : x+3),
                                             ((fill) ? y+2 : y+3),
                                             ((fill) ? edge-4 : edge-6),
                                             ((fill) ? edge-4 : edge-6));
	       }
 
	       if (!TBG_Visible(w))
	       {
		  if (!IsOn(w))
		      if (edge > 0)
			  XFillRectangle( XtDisplay ((Widget) w),
                                             XtWindow ((Widget) w),
                                             TBG_BackgroundGC(w),
                                             x, y, edge, edge);
	       }
	    }
	    else
	    {
               if (w->gadget.shadow_thickness> 0)  DrawToggleShadow (w);
               if (TBG_FillOnSelect(w) && !LabG_IsPixmap(w))
                 DrawToggleLabel (w);
	    }
	    if (LabG_IsPixmap(w))
	    {
	       SetAndDisplayPixmap((Widget) w, event, NULL);
	    }
	 }
      }
      else 
      {
	 if (TBG_Armed(w) == TRUE) 
	 { 
	    if (TBG_IndOn(w))
	    {
	       if ((TBG_Visible(w)) ||
		   ((!TBG_Visible(w)) && (IsOn(w))))
                  _XmDrawDiamond (XtDisplay(w), XtWindow(w),
                                 ((IsOn(w)) ?
                                  XmParentBottomShadowGC(w) :
                                  XmParentTopShadowGC(w)),
                                 ((IsOn(w)) ?
                                  XmParentTopShadowGC(w) :
                                  XmParentBottomShadowGC(w)),
                                 (((IsOn(w)) &&
                                   (TBG_FillOnSelect(w))) ?
                                  TBG_SelectGC(w) :
                                  TBG_BackgroundGC(w)),
                                 x, y, edge, edge,
                                 w->gadget.shadow_thickness,fill);

	       if (!TBG_Visible(w))
	       {
		  if (!IsOn(w))
		      if (edge > 0)
			  XFillRectangle( XtDisplay ((Widget) w),
					 XtWindow ((Widget) w),
					 TBG_BackgroundGC(w),
					 x, y, edge, edge);
	       }
	    }
	    else
	    {
               if (w->gadget.shadow_thickness> 0)  DrawToggleShadow (w);
               if (TBG_FillOnSelect(w) && !LabG_IsPixmap(w))
                 DrawToggleLabel (w);
	    }
	    if (LabG_IsPixmap(w))
	    {
	       SetAndDisplayPixmap( (Widget) w, event, NULL);
	    }
	 }
      }
   }
}


/************************************************************************
 *
 *     Arm
 *        This function processes button 1 down occuring on the togglebutton.
 *        Mark the togglebutton as armed and display it armed.
 *        The callbacks for XmNarmCallback are called.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
Arm( w, event )
        Widget w ;
        XEvent *event ;
#else
Arm(
        Widget w,
        XEvent *event )
#endif /* _NO_PROTO */
{
  XmToggleButtonGadget tb = (XmToggleButtonGadget)w;

  IsOn(tb) = (TBG_Set(tb) == TRUE) ? FALSE : TRUE;
  TBG_Armed(tb) = TRUE;
  if (TBG_IndOn(tb))
  {
    DrawToggle((XmToggleButtonGadget) w);
  }
  else
  {
     if(tb->gadget.shadow_thickness> 0)  DrawToggleShadow (tb);
     if (TBG_FillOnSelect(w) && !LabG_IsPixmap(w)) DrawToggleLabel (tb);
  }
  if (LabG_IsPixmap(tb))
  {
     SetAndDisplayPixmap( (Widget) tb, event, NULL);
  }
    
  if (TBG_ArmCB(tb))
  {
     XFlush(XtDisplay(tb));
     ToggleButtonCallback(tb, XmCR_ARM, TBG_Set(tb), event);
  }
     
}


/************************************************************************
 *
 *     Select
 *       Mark the togglebutton as unarmed (i.e. inactive).
 *       If the button release occurs inside of the ToggleButton, the
 *       callbacks for XmNvalueChangedCallback are called.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
Select( tb, event )
        XmToggleButtonGadget tb ;
        XEvent *event ;
#else
Select(
        XmToggleButtonGadget tb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmToggleButtonCallbackStruct call_value;
   XButtonEvent *buttonEvent = (XButtonEvent *) event;
   Boolean hit;

   TBG_Armed(tb) = FALSE;
   
   /* CR 8068: Verify that this is in fact a button event. */
   hit = ((event->xany.type == ButtonPress || 
	   event->xany.type == ButtonRelease) &&
	  (buttonEvent->x < tb->rectangle.x + tb->rectangle.width) &&
	  (buttonEvent->y < tb->rectangle.y + tb->rectangle.height) &&
	  (buttonEvent->x >= tb->rectangle.x) &&
	  (buttonEvent->y >= tb->rectangle.y));

   if (hit)
     TBG_Set(tb) = (TBG_Set(tb)) ? FALSE : TRUE;

   /* Redisplay after changing state. */
   (* (((XmToggleButtonGadgetClassRec *)(tb->object.widget_class))->
		rect_class.expose)) ((Widget) tb, event, (Region) NULL);

   if (hit)
   {
      /* if the parent is a RowColumn, notify it about the select */
      if (XmIsRowColumn(XtParent(tb)))
      {
	 call_value.reason = XmCR_VALUE_CHANGED;
	 call_value.event = event;
	 call_value.set = TBG_Set(tb);
	 (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_CALLBACK, 
							  XtParent(tb), FALSE,
							  tb, &call_value);
      }

      if ((! LabG_SkipCallback(tb)) &&
	  (TBG_ValueChangedCB(tb)))
      {
	 XFlush(XtDisplay(tb));
	 ToggleButtonCallback(tb, XmCR_VALUE_CHANGED, TBG_Set(tb), event);
      }
   }
}


/**********************************************************************
 *
 *    Disarm
 *     The callbacks for XmNdisarmCallback are called..
 *
 ************************************************************************/
 static void 
#ifdef _NO_PROTO
Disarm( tb, event )
        XmToggleButtonGadget tb ;
        XEvent *event ;
#else
Disarm(
        XmToggleButtonGadget tb,
        XEvent *event )
#endif /* _NO_PROTO */
{ 
   if (TBG_DisarmCB(tb))
     ToggleButtonCallback(tb, XmCR_DISARM, TBG_Set(tb), event);
 }


/************************************************************************
 *
 *     ArmAndActivate
 *       This routine arms and activates a ToggleButton. It is called on
 *       <Key> Return and a <Key> Space, as well as when a mnemonic or
 *       button accelerator has been activated.
 *    Modify: Current implementation does care to draw shadows if indicator
 *	     is set to false; This is being modified.
 ************************************************************************/
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
   XmToggleButtonGadget tb = (XmToggleButtonGadget) wid ;
   XmToggleButtonCallbackStruct call_value;
   Boolean already_armed = TBG_Armed(tb);
   Boolean is_menupane = (LabG_MenuType(tb) == XmMENU_PULLDOWN) ||
			 (LabG_MenuType(tb) == XmMENU_POPUP);
   Boolean parent_is_torn;
   Boolean torn_has_focus = FALSE;

   if (is_menupane && !XmIsMenuShell(XtParent(XtParent(tb))))
   {
      parent_is_torn = TRUE;

      if (_XmFocusIsInShell((Widget)tb))
      {
         /* In case allowAcceleratedInsensitiveUnmanagedMenuItems is True */
         if (!XtIsSensitive((Widget)tb) || (!XtIsManaged((Widget)tb)))
            return;
         torn_has_focus = TRUE;
      }
   } else
      parent_is_torn = FALSE;


   TBG_Armed(tb) = FALSE;    

   TBG_Set(tb) = (TBG_Set(tb) == TRUE) ? FALSE : TRUE;
   IsOn(tb) = TBG_Set(tb);

   if (is_menupane)
   {
      if (parent_is_torn && !torn_has_focus)
      {
	 /* Freeze tear off visuals in case accelerators are not in
	  * same context
	  */
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_RESTORE_TEAROFF_TO_MENUSHELL, XtParent(tb), NULL,
	    event, NULL);
      }

      if (torn_has_focus)
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_POPDOWN, XtParent(tb), NULL, event, NULL);
      else
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_BUTTON_POPDOWN, XtParent(tb), NULL, event, NULL);

      if (torn_has_focus)
	 XmProcessTraversal((Widget) tb, XmTRAVERSE_CURRENT);

      /* Draw the toggle indicator in case of tear off */
      if (TBG_IndOn(tb))
         DrawToggle(tb);
      else
         if (TBG_FillOnSelect(tb) && !LabG_IsPixmap(tb))
            DrawToggleLabel (tb);
      if (LabG_IsPixmap(tb))
         SetAndDisplayPixmap( (Widget) tb, NULL, NULL);
   }
   else
   { 
      if (TBG_IndOn(tb)) 
	 DrawToggle(tb);
      else
      {
        if(tb->gadget.shadow_thickness> 0)  DrawToggleShadow (tb);
        if (TBG_FillOnSelect(tb) && !LabG_IsPixmap(tb)) DrawToggleLabel (tb);
      }
 
      if (LabG_IsPixmap(tb))
	 SetAndDisplayPixmap( (Widget) tb, event, NULL);
   }

   /* If the parent is a RowColumn, set the lastSelectToplevel before the arm.
    * It's ok if this is recalled later.
    */
   if (XmIsRowColumn(XtParent(tb)))
   {
      (* xmLabelGadgetClassRec.label_class.menuProcs) (
         XmMENU_GET_LAST_SELECT_TOPLEVEL, XtParent(tb));
   }

   if (TBG_ArmCB(tb) && !already_armed)
       ToggleButtonCallback(tb, XmCR_ARM, TBG_Set(tb), event);

   /* if the parent is a RowColumn, notify it about the select */
   if (XmIsRowColumn(XtParent(tb)))
   {
      call_value.reason = XmCR_VALUE_CHANGED;
      call_value.event = event;
      call_value.set = TBG_Set(tb);
      (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_CALLBACK,
						       XtParent(tb),  FALSE,
						       tb, &call_value);
   }
   
   if ((! LabG_SkipCallback(tb)) &&
       (TBG_ValueChangedCB(tb)))
   {
      XFlush(XtDisplay(tb));
      ToggleButtonCallback(tb, XmCR_VALUE_CHANGED, TBG_Set(tb), event);
   }

   if (TBG_DisarmCB(tb))
   {
      XFlush(XtDisplay(tb));
      ToggleButtonCallback(tb, XmCR_DISARM, TBG_Set(tb), event);
   }

   if (is_menupane)
   {
      if (torn_has_focus)
      {
	 TBG_Armed(tb) = TRUE;    
	 if (TBG_ArmCB(tb))
	 {
	    XFlush(XtDisplay(tb));
	    ToggleButtonCallback(tb, XmCR_ARM, TBG_Set(tb), event);
	 }
      }
      else
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL, 
	    XtParent(tb), NULL, event, NULL);
   }
}


/************************************************************************
 *
 *     BtnDown
 *       This function processes a button down occuring on the togglebutton
 *       when it is in a popup, pulldown, or option menu.
 *       Popdown the posted menu.
 *       Turn parent's traversal off.
 *       Mark the togglebutton as armed (i.e. active).
 *       The callbacks for XmNarmCallback are called.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
BtnDown( tb, event )
        XmToggleButtonGadget tb ;
        XEvent *event ;
#else
BtnDown(
        XmToggleButtonGadget tb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   Boolean already_armed;
   ShellWidget popup;

   _XmSetInDragMode((Widget)tb, True);

   /* Popdown other popups that may be up */
   if (!(popup = (ShellWidget)_XmGetRC_PopupPosted(XtParent(tb))))
   {
      if (!XmIsMenuShell(XtParent(XtParent(tb))))
      {
         /* In case tear off not armed and no grabs in place, do it now.
          * Ok if already armed and grabbed - nothing done.
          */
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_TEAR_OFF_ARM, XtParent(tb));
      }
   }

   if (popup)
   {
      Widget w;

      if (popup->shell.popped_up)
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_SHELL_POPDOWN, (Widget) popup, NULL, event, NULL);

      /* If the active_child is a cascade (highlighted), then unhighlight it. */
      w = ((XmManagerWidget)XtParent(tb))->manager.active_child;
      if (w && (XmIsCascadeButton(w) || XmIsCascadeButtonGadget(w)))
	  XmCascadeButtonHighlight (w, FALSE);
   }

   /* Set focus to this button.  This must follow the possible
    * unhighlighting of the CascadeButton else it'll screw up active_child.
    */
   (void)XmProcessTraversal( (Widget) tb, XmTRAVERSE_CURRENT);
	 /* get the location cursor - get consistent with Gadgets */

   _XmDrawShadows (XtDisplay (tb), XtWindow (tb),
	      XmParentTopShadowGC(tb),
	      XmParentBottomShadowGC(tb),
	      tb->rectangle.x + tb -> gadget.highlight_thickness,
	      tb->rectangle.y + tb -> gadget.highlight_thickness,
	      tb -> rectangle.width - 2 * tb->gadget.highlight_thickness,
	      tb -> rectangle.height - 2 * tb->gadget.highlight_thickness,
	      tb -> gadget.shadow_thickness, XmSHADOW_OUT);

   already_armed = TBG_Armed(tb);

   TBG_Armed(tb) = TRUE;

   if (TBG_ArmCB(tb) && !already_armed)
   {
     XFlush (XtDisplay (tb));

     ToggleButtonCallback(tb, XmCR_ARM, TBG_Set(tb), event);
   }

   _XmRecordEvent (event);
}


/************************************************************************
 *
 *     BtnUp
 *       This function processes a button up occuring on the togglebutton
 *       when it is in a popup, pulldown, or option menu.
 *       Mark the togglebutton as unarmed (i.e. inactive).
 *       The callbacks for XmNvalueChangedCallback are called.
 *       The callbacks for XmNdisarmCallback are called.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
BtnUp( tb, event )
        XmToggleButtonGadget tb ;
        XEvent *event ;
#else
BtnUp(
        XmToggleButtonGadget tb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XButtonEvent *buttonEvent = (XButtonEvent *) event;
   XmToggleButtonCallbackStruct call_value;
   Boolean popped_up;
   Boolean valid_event;
   Boolean is_menupane = (LabG_MenuType(tb) == XmMENU_PULLDOWN) ||
			 (LabG_MenuType(tb) == XmMENU_POPUP);
   Widget shell = XtParent(XtParent(tb));

   TBG_Armed(tb) = FALSE;


   /* We need to validate the event in case the XmMENU_POPDOWN restores the
    * submenu to the transient shell.  The tear off control becomes unmanaged
    * and the submenu's (and menu item children) layout/geometry changes.
    */
   valid_event = ((buttonEvent->x <= tb->rectangle.x + tb->rectangle.width) &&
       (buttonEvent->y <= tb->rectangle.y + tb->rectangle.height) &&
       (buttonEvent->x >= tb->rectangle.x) &&
       (buttonEvent->y >= tb->rectangle.y));

   
   if (is_menupane && !XmIsMenuShell(shell))
      (* xmLabelGadgetClassRec.label_class.menuProcs)
	 (XmMENU_POPDOWN, (Widget) tb, NULL, event, &popped_up);
   else
      (* xmLabelGadgetClassRec.label_class.menuProcs)
	 (XmMENU_BUTTON_POPDOWN, (Widget) tb, NULL, event, &popped_up);

   _XmRecordEvent(event);

   if (popped_up)
   {
      return;
   }

   if (valid_event)
   {
      TBG_Set(tb) = (TBG_Set(tb) == TRUE) ? FALSE : TRUE;
      IsOn(tb) = TBG_Set(tb);

      /* if the parent is a RowColumn, notify it about the select */
      if (XmIsRowColumn(XtParent(tb)))
      {
	 call_value.reason = XmCR_VALUE_CHANGED;
	 call_value.event = event;
	 call_value.set = TBG_Set(tb);
	 (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_CALLBACK, 
							  XtParent(tb), FALSE,
							   tb, &call_value);
      }
      
      if ((! LabG_SkipCallback(tb)) &&
	  (TBG_ValueChangedCB(tb)))
      {
	 XFlush(XtDisplay(tb));
	 ToggleButtonCallback(tb, XmCR_VALUE_CHANGED, TBG_Set(tb), event);
      }
      
      if (TBG_DisarmCB(tb))
	  ToggleButtonCallback(tb, XmCR_DISARM, TBG_Set(tb), event);

      /* If the original shell does not indicate an active menu, but rather a
       * tear off pane, leave the button in an armed state.
       */
      if (!XmIsMenuShell(shell))
      { 
	 if (XtIsSensitive(tb))
	 {
	    if (TBG_IndOn(tb)) 
	       DrawToggle(tb);
	    else
	       {
	       if (TBG_FillOnSelect(tb) && !LabG_IsPixmap(tb))
		   DrawToggleLabel (tb);
	       }
	    if (LabG_IsPixmap(tb))
	       SetAndDisplayPixmap( (Widget) tb, event, NULL);

	    TBG_Armed(tb) = TRUE;
	    if (TBG_ArmCB(tb))
	    {
	       XFlush(XtDisplay(tb));
	       ToggleButtonCallback(tb, XmCR_ARM, TBG_Set(tb), event);
	    }
	 }
      } 
      else
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL, 
	    XtParent(tb), NULL, event, NULL);
   }

   _XmSetInDragMode((Widget)tb, False);

   /* For the benefit of tear off menus, we must set the focus item
    * to this button.  In normal menus, this would not be a problem
    * because the focus is cleared when the menu is unposted.
    */
   if (!XmIsMenuShell(shell))
      XmProcessTraversal((Widget) tb, XmTRAVERSE_CURRENT);
}


/************************************************************************
 *
 *  GetGC
 *	Get the graphics context to be used to fill the interior of
 *	a square or diamond when selected.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
GetGC( tw )
        XmToggleButtonGadget tw ;
#else
GetGC(
        XmToggleButtonGadget tw )
#endif /* _NO_PROTO */
{
   XGCValues values;
   XtGCMask  valueMask;
   XmManagerWidget mw;
   XFontStruct *fs = (XFontStruct *) NULL;

   mw = (XmManagerWidget) XtParent(tw);

   valueMask = GCForeground | GCBackground | GCFillStyle | GCGraphicsExposures;
   if ((DefaultDepthOfScreen (XtScreen (tw)) == 1) /*  Monochrome Display */
        && (mw->core.background_pixel == TBG_SelectColor(tw)))
       values.foreground = mw->manager.foreground;
   else
       values.foreground = TBG_SelectColor(tw);
   values.background =  mw->core.background_pixel;
   values.fill_style = FillSolid;
   values.graphics_exposures = FALSE;

   TBG_SelectGC(tw) = XtGetGC ((Widget) mw, valueMask, &values); 

   valueMask = GCForeground | GCBackground | GCFillStyle | GCGraphicsExposures;

   /* When foreground and select colors coincide, this GC is used
    * by XmLabel to draw the text. It requires a font to pacify
    * the XmString draw functions.
    */
   _XmFontListGetDefaultFont(LabG_Font(tw), &fs);
   if (fs != NULL) {
      valueMask |= GCFont;
      values.font = fs->fid;
   }

   values.foreground = mw->core.background_pixel;
   values.background = mw->manager.foreground;
   values.fill_style = FillSolid;
   values.graphics_exposures = FALSE;

   TBG_BackgroundGC(tw) = XtGetGC((Widget) mw, valueMask, &values);

               
}


/*************************************<->*************************************
 *
 *  Initialize
 *    If the rectangle height and width fields are set to 0, treat that as a 
 *    flag, and compute the optimum size for this button.  Then using what ever
 *    the rectangle fields are set to, compute the text placement fields.
 *************************************<->***********************************/
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
{   XmToggleButtonGadget request = (XmToggleButtonGadget) rw ;
    XmToggleButtonGadget new_w = (XmToggleButtonGadget) nw ;
    int maxIndicatorSize;   /* Max Indicator size permissible */
	int	delta;
	int	boxSize;

    TBG_Armed(new_w) = FALSE;

    /* if menuProcs is not set up yet, try again */
    if (xmLabelGadgetClassRec.label_class.menuProcs == (XmMenuProc)NULL)
	xmLabelGadgetClassRec.label_class.menuProcs =
	    (XmMenuProc) _XmGetMenuProcContext();


    if (LabG_MenuType(new_w) == XmMENU_POPUP ||
	LabG_MenuType(new_w) == XmMENU_PULLDOWN)
    {
       if (new_w->gadget.shadow_thickness <= 0)
	   new_w->gadget.shadow_thickness = 2;

       if (TBG_Visible(new_w) == XmINVALID_BOOLEAN)
	   TBG_Visible(new_w) = FALSE;

       new_w->gadget.traversal_on = TRUE;
    }

    else
    {
       if (TBG_Visible(new_w) == XmINVALID_BOOLEAN)
	   TBG_Visible(new_w) = TRUE;
    }

    /*
     * If fillOnSelect has not been set, copy indicatorOn.
     * This provides 1.1 compatibility: fillOnSelect == true was
     * the default and had no effect when indicatorOn == false.
     * In 1.2 it causes the background to be filled with selectColor.
     * Don't want to surprise applications which set indicatorOn, let
     * fillOnSelect default to true, and didn't expect the background
     * to be filled.
     */  
    if (TBG_FillOnSelect(new_w) == XmINVALID_BOOLEAN)
         TBG_FillOnSelect(new_w) = TBG_IndOn(new_w);

    /*
     * if the indicatorType has not been set, then
     * find out if radio behavior is set for RowColumn parents and
     * then set indicatorType.  If radio behavior is true, default to
     * one of many, else default to n of many.
     */
    if(    (TBG_IndType( new_w) == XmINVALID_TYPE)
        || !XmRepTypeValidValue( XmRepTypeGetId(XmRIndicatorType),
                                          TBG_IndType( new_w), (Widget) new_w)    )
    {
       if  (XmIsRowColumn(XtParent(new_w)))
       {
	  Arg arg[1];
	  Boolean radio;

	  XtSetArg (arg[0], XmNradioBehavior, &radio);
	  XtGetValues (XtParent(new_w), arg, 1);

	  if (radio)
	      TBG_IndType(new_w) = XmONE_OF_MANY;
	  else
	      TBG_IndType(new_w) = XmN_OF_MANY;
       }
       else
	   TBG_IndType(new_w) = XmN_OF_MANY;
    }

    if (IsNull (PixmapOff(new_w)) &&            /* no Off pixmap but do have */
        ! IsNull (PixmapOn(new_w)))           /* an On, so use that */
    {
       PixmapOff(new_w) = PixmapOn(new_w);
       if (request->rectangle.width == 0)
         new_w->rectangle.width = 0;
       if (request->rectangle.height == 0)
         new_w->rectangle.height = 0;

       _XmCalcLabelGDimensions(nw);
        (* xmLabelGadgetClassRec.rect_class.resize)( (Widget) new_w);
    }

    if (IsNull (Pixmap_Insen_Off(new_w)) &&        /* no Off pixmap but do have */
        ! IsNull (Pixmap_Insen_On(new_w)))           /* an On, so use that */
    {
       Pixmap_Insen_Off(new_w) = Pixmap_Insen_On(new_w);
       if (request->rectangle.width == 0)
         new_w->rectangle.width = 0;
       if (request->rectangle.height == 0)
         new_w->rectangle.height = 0;

       _XmCalcLabelGDimensions(nw);
        (* xmLabelGadgetClassRec.rect_class.resize)( (Widget) new_w);
    }

/* BEGIN OSF Fix pir 1778 */
    if (LabG_IsPixmap(new_w) &&
       (!IsNull(PixmapOff(new_w)) || !IsNull(PixmapOn(new_w)) ||
        !IsNull(Pixmap_Insen_Off(new_w)) || !IsNull(Pixmap_Insen_On(new_w))))
    {
       if (request->rectangle.width == 0)
         new_w->rectangle.width = 0;
       if (request->rectangle.height == 0)
         new_w->rectangle.height = 0;
       SetToggleSize(new_w);
    }
/* END OSF Fix pir 1778 */
    if (TBG_IndicatorDim(new_w) == XmINVALID_DIMENSION)  {
      TBG_IndicatorSet(new_w) = LabG_IsPixmap(new_w);
      if (TBG_IndOn(new_w))
      {
	/* DETERMINE HOW HIGH THE TOGGLE INDICATOR SHOULD BE */

	if LabG_IsPixmap(new_w) 
	{
	  /*set indicatorSize proportional to size of pixmap*/
	  if (LabG_TextRect(new_w).height < 13)
	    TBG_IndicatorDim(new_w) = LabG_TextRect(new_w).height;
	  else 
	    TBG_IndicatorDim(new_w) = 13 + (LabG_TextRect(new_w).height/13);
	}
	else /*set indicatorSize proportional to size of font*/
	{
	  Dimension height;
	  int line_count;

	  height = _XmStringHeight (LabG_Font(new_w), LabG__label(new_w));
	  if( (line_count = _XmStringLineCount (LabG__label(new_w))) < 1)
	    line_count = 1;
	  /* Shiz recommends toggles in menus have smaller indicators */
	  if (LabG_MenuType(new_w) == XmMENU_POPUP ||          
	      LabG_MenuType(new_w) == XmMENU_PULLDOWN) {
             TBG_IndicatorDim(new_w) = Max(XmDEFAULT_INDICATOR_DIM,
               (height / ((Dimension)line_count))*2/3);
           } else
               TBG_IndicatorDim(new_w) = Max(XmDEFAULT_INDICATOR_DIM,
                 height / ((Dimension)line_count));

	}
      } else
        TBG_IndicatorDim(new_w) = 0;
    } else
      TBG_IndicatorSet(new_w) = TRUE;

    if (TBG_IndOn(new_w))
    {
 /*
  *   Enlarge the text rectangle if needed to accomodate the size of
  *     indicator button. Adjust the dimenions of superclass Label-Gadget
  *	    so that the toggle-button may be accommodated in it.
  */
/* BEGIN OSF Fix pir 2480 */
    if (LabG_MenuType(new_w) != XmMENU_POPUP &&
        LabG_MenuType(new_w) != XmMENU_PULLDOWN)
      maxIndicatorSize = TBG_IndicatorDim(new_w) +
                         2 * (new_w->gadget.shadow_thickness +
                         Xm3D_ENHANCE_PIXEL);
    else
      maxIndicatorSize = TBG_IndicatorDim(new_w);
/* END OSF Fix pir 2480 */

    boxSize = (int) (LabG_TextRect(new_w).height) +
			   (int) LabG_MarginTop (new_w) +(int) LabG_MarginBottom (new_w);

     if (maxIndicatorSize > boxSize)
	  { delta = maxIndicatorSize - boxSize;
         LabG_MarginTop (new_w) += delta/2;
         LabG_MarginBottom (new_w) += delta /2;
	  } 

    /* Make room for toggle indicator and spacing */
        if ((LabG_MarginLeft(new_w)) < (TBG_IndicatorDim(new_w) +
                                       TBG_Spacing(new_w))) {
            LabG_MarginLeft(new_w) =  TBG_IndicatorDim(new_w) +
		                    TBG_Spacing(new_w);
	}
    }

    if (request->rectangle.width == 0)
    {

        new_w->rectangle.width = LabG_TextRect(new_w).width +
                          2 * LabG_MarginHeight(new_w) +   
                          LabG_MarginRight(new_w) +
                          LabG_MarginLeft(new_w) +
		          2 * (new_w->gadget.highlight_thickness +
                               new_w->gadget.shadow_thickness);

        if (new_w->rectangle.width == 0)
           new_w->rectangle.width = 1;
    
        if ((LabG__acceleratorText(new_w) != NULL) && (TBG_IndOn(new_w)))
             LabG_AccTextRect(new_w).x = new_w->rectangle.width -
                                         new_w->gadget.highlight_thickness -
                                         new_w->gadget.shadow_thickness -
                                         LabG_MarginHeight(new_w) -
                                         LabG_MarginRight(new_w) +
                                         LABELG_ACC_PAD;


    }

    if (request->rectangle.height == 0) 
        new_w->rectangle.height = Max(TBG_IndicatorDim(new_w),
           LabG_TextRect(new_w).height + 2 * LabG_MarginHeight(new_w) +
               LabG_MarginTop(new_w) + LabG_MarginBottom(new_w)) +
	       2 * (new_w->gadget.highlight_thickness +
                    new_w->gadget.shadow_thickness);  

    LabG_TextRect(new_w).y = new_w->gadget.highlight_thickness
           + new_w->gadget.shadow_thickness
               + LabG_MarginHeight(new_w) + LabG_MarginTop(new_w) +
                   ((new_w->rectangle.height - LabG_MarginTop(new_w)
                     - LabG_MarginBottom(new_w)
                     - (2 * (LabG_MarginHeight(new_w)
                             + new_w->gadget.highlight_thickness
                             + new_w->gadget.shadow_thickness))
                     - LabG_TextRect(new_w).height) / 2);

    if (new_w->rectangle.height == 0)
       new_w->rectangle.height = 1;


    if (TBG_Set(new_w))
        IsOn(new_w) = TRUE; /* When toggles first come up, if
                                           XmNset is TRUE, then they are
                                           displayed set */
    else
        IsOn(new_w) = FALSE;

/* BEGIN OSF Fix pir 2097 */
    new_w->gadget.event_mask = XmARM_EVENT | XmACTIVATE_EVENT |
      XmMULTI_ARM_EVENT | XmMULTI_ACTIVATE_EVENT | XmHELP_EVENT |
        XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT | XmENTER_EVENT | XmLEAVE_EVENT |
          XmBDRAG_EVENT;
/* END OSF Fix pir 2097 */

    (* (new_w->object.widget_class->core_class.resize)) ((Widget) new_w);
    GetGC (new_w);

}
   





/************************************************************************
 *
 *  Destroy
 *	Free toggleButton's graphic context.
 *
 ************************************************************************/
static void 
#ifdef _NO_PROTO
Destroy( w )
        Widget w ;
#else
Destroy(
        Widget w )
#endif /* _NO_PROTO */
{
   XmToggleButtonGadget tw = (XmToggleButtonGadget) w;

   XtReleaseGC (XtParent(tw), TBG_SelectGC(tw));
   XtReleaseGC (XtParent(tw), TBG_BackgroundGC(tw));

   XtRemoveAllCallbacks ((Widget) tw, XmNvalueChangedCallback);
   XtRemoveAllCallbacks ((Widget) tw, XmNarmCallback);
   XtRemoveAllCallbacks ((Widget) tw, XmNdisarmCallback);

   _XmCacheDelete( (XtPointer) TBG_Cache(tw));
}





/*************************************<->*************************************
 *
 *  DrawToggle
 *     Depending on the state of this widget, draw the ToggleButton.
 *
 *************************************<->***********************************/
static void 
#ifdef _NO_PROTO
DrawToggle( w )
        XmToggleButtonGadget w ;
#else
DrawToggle(
        XmToggleButtonGadget w )
#endif /* _NO_PROTO */
{
   int x, y, edge;
   Boolean fill;
   XmManagerWidget mw;

  mw = (XmManagerWidget) XtParent(w);

  if( TBG_IndicatorSet(w) || _XmStringEmpty(LabG__label(w)) ) {
    edge = TBG_IndicatorDim(w);
  } else {
  edge = Min((int)TBG_IndicatorDim(w), 
           Max(0, (int)w->rectangle.height - 2*(w->gadget.highlight_thickness +
                                           w->gadget.shadow_thickness +
                                          (int)LabG_MarginHeight(w)) +
                                           LabG_MarginTop(w) +
                                           LabG_MarginBottom(w)));
  }

  if (DefaultDepthOfScreen (XtScreen (w)) == 1) /* Monochrome Display */
     fill = FALSE;
  else
  {
     if ((mw->manager.top_shadow_color != TBG_SelectColor(w)) &&
         (mw->manager.bottom_shadow_color != TBG_SelectColor(w)))
        fill = TRUE;
     else
        fill = FALSE;
  }  

  x = w->rectangle.x +
      w->gadget.highlight_thickness + w->gadget.shadow_thickness +
      LabG_MarginHeight(w);
 
  if( TBG_IndicatorSet(w) || _XmStringEmpty(LabG__label(w)) )
    y = w->rectangle.y + 
      (int)((w->rectangle.height - TBG_IndicatorDim(w)))/2;
  else
  {
     y = w->rectangle.y + LabG_TextRect(w).y;
     if (LabG_MenuType(w) == XmMENU_POPUP ||
         LabG_MenuType(w) == XmMENU_PULLDOWN)
        y += (TBG_IndicatorDim(w) + 2) / 4; /* adjust in menu */
  }

  if ((TBG_IndType(w)) == XmN_OF_MANY)
  {
     /* if the toggle indicator is square shaped then adjust the
	indicator width and height, so that it looks proportional
	to a diamond shaped indicator of the same width and height */

     int new_edge;
     new_edge = edge - 3 - ((edge - 10)/10); /* Subtract 3 pixels + 1 pixel */
                                              /* for every 10 pixels, from   */
                                              /* width and height.           */

     /* Adjust x,y so that the indicator is centered relative to the label */
     y = y + ((edge - new_edge) / 2);
     x = x + ((edge - new_edge) / 2);
     edge = new_edge;

     if ((TBG_Visible(w)) ||
	 ((!TBG_Visible(w)) && (IsOn(w))))
     {
       _XmDrawShadows(XtDisplay (w), XtWindow (w),
                    ((IsOn(w)) ?
                               XmParentBottomShadowGC(w) :
                               XmParentTopShadowGC(w)),
                    ((IsOn(w)) ?
                               XmParentTopShadowGC(w) :
                               XmParentBottomShadowGC(w)),
                    x, y, edge, edge, 2, XmSHADOW_OUT);

       if (edge > (fill ? 4 : 6))
           XFillRectangle (XtDisplay ((Widget) w),
                           XtWindow ((Widget) w),
                           (((IsOn(w)) &&
                                (TBG_FillOnSelect(w))) ?
                               TBG_SelectGC(w) :
                               TBG_BackgroundGC(w)),
                           ((fill) ? x+2 : x+3),
                           ((fill) ? y+2 : y+3),
                           ((fill) ? edge-4 : edge-6),
                           ((fill) ? edge-4 : edge-6));
     } 
  }

  else
  {
     if ((TBG_Visible(w)) ||
	 ((!TBG_Visible(w)) && (IsOn(w))))
        _XmDrawDiamond (XtDisplay(w), XtWindow(w),
                                 ((IsOn(w)) ?
                                  XmParentBottomShadowGC(w) :
                                  XmParentTopShadowGC(w)),
                                 ((IsOn(w)) ?
                                  XmParentTopShadowGC(w) :
                                  XmParentBottomShadowGC(w)),
                                 (((IsOn(w)) &&
                                   (TBG_FillOnSelect(w))) ?
                                  TBG_SelectGC(w) :
                                  TBG_BackgroundGC(w)),
                                 x, y, edge, edge,
                                 w->gadget.shadow_thickness,fill);
  } 

   if ((!TBG_Visible(w)) && (!IsOn(w)))
   {
      if (edge > 0)
	  XFillRectangle( XtDisplay ((Widget) w),
			 XtWindow ((Widget) w),
			 TBG_BackgroundGC(w),
			 x, y, edge, edge);
   }
}

/*************************************<->*************************************
 *
 *  BorderHighlight
 *
 *************************************<->***********************************/
static void 
#ifdef _NO_PROTO
BorderHighlight( wid )
        Widget wid ;
#else
BorderHighlight(
        Widget wid )
#endif /* _NO_PROTO */
{
        XmToggleButtonGadget tb = (XmToggleButtonGadget) wid ;
   XEvent * event = NULL;

   if (LabG_MenuType(tb) == XmMENU_PULLDOWN ||
       LabG_MenuType(tb) == XmMENU_POPUP)
   {
      _XmDrawShadows (XtDisplay (tb), XtWindow (tb),
		    XmParentTopShadowGC(tb),
		    XmParentBottomShadowGC(tb),
		    tb->rectangle.x + tb -> gadget.highlight_thickness,
		    tb->rectangle.y + tb -> gadget.highlight_thickness,
		    tb -> rectangle.width -
		      2 * tb->gadget.highlight_thickness,
		      tb -> rectangle.height -
		      2 * tb->gadget.highlight_thickness,
		    tb -> gadget.shadow_thickness, XmSHADOW_OUT);
   
      TBG_Armed(tb) = TRUE;
      
      if (TBG_ArmCB(tb))
      {
	 XFlush (XtDisplay (tb));
	 ToggleButtonCallback(tb, XmCR_ARM, TBG_Set(tb), event);
      }
   }
   else
   {   (*(xmLabelGadgetClassRec.gadget_class.border_highlight))((Widget) tb) ;
       } 

}


/*************************************<->*************************************
 *
 *  BorderUnhighlight
 *
 *************************************<->***********************************/
static void 
#ifdef _NO_PROTO
BorderUnhighlight( wid )
        Widget wid ;
#else
BorderUnhighlight(
        Widget wid )
#endif /* _NO_PROTO */
{
        XmToggleButtonGadget tb = (XmToggleButtonGadget) wid ;
   XEvent * event = NULL;

   if (LabG_MenuType(tb) == XmMENU_PULLDOWN ||
       LabG_MenuType(tb) == XmMENU_POPUP)
   {
      if (!TBG_Armed(tb))
          return;

      _XmClearBorder (XtDisplay (tb), XtWindow (tb),
		      tb->rectangle.x + tb -> gadget.highlight_thickness,
		      tb->rectangle.y + tb -> gadget.highlight_thickness,
		      tb -> rectangle.width -
			 2 * tb->gadget.highlight_thickness,
		      tb -> rectangle.height -
			 2 * tb->gadget.highlight_thickness,
		      tb -> gadget.shadow_thickness);
      
      TBG_Armed(tb) = FALSE;

      if (TBG_DisarmCB(tb))
      {
	 XFlush (XtDisplay (tb));
	 ToggleButtonCallback(tb, XmCR_DISARM, TBG_Set(tb), event);
      }
   }
   else
   {   (*(xmLabelGadgetClassRec.gadget_class.border_unhighlight))(
                                                                 (Widget) tb) ;
       } 
}


/*************************************<->*************************************
 *
 *  KeySelect
 *    If the menu system traversal is enabled, do an activate and disarm
 *
 *************************************<->***********************************/
static void 
#ifdef _NO_PROTO
KeySelect( tb, event )
        XmToggleButtonGadget tb ;
        XEvent *event ;
#else
KeySelect(
        XmToggleButtonGadget tb,
        XEvent *event )
#endif /* _NO_PROTO */
{
   XmToggleButtonCallbackStruct call_value;

   if (!_XmIsEventUnique(event))
      return;

   if (!_XmGetInDragMode((Widget)tb))
   {

      if (TBG_IndOn(tb))
         DrawToggle(tb);
      else
         if (TBG_FillOnSelect(tb) && !LabG_IsPixmap(tb))
            DrawToggleLabel (tb);
      if (LabG_IsPixmap(tb))
         SetAndDisplayPixmap( (Widget) tb, NULL, NULL);

      TBG_Armed(tb) = FALSE;
      TBG_Set(tb) = (TBG_Set(tb) == TRUE) ? FALSE : TRUE;

      if (XmIsRowColumn(XtParent(tb)))
      {
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_BUTTON_POPDOWN,  XtParent(tb), NULL, event, NULL);
      }

      _XmRecordEvent(event);

      /* if the parent is a RowColumn, notify it about the select */
      if (XmIsRowColumn(XtParent(tb)))
      {
	 call_value.reason = XmCR_VALUE_CHANGED;
	 call_value.event = event;
	 call_value.set = TBG_Set(tb);
	 (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_CALLBACK, 
							  XtParent(tb), FALSE,
							  tb, &call_value);
      }
      
      if ((! LabG_SkipCallback(tb)) &&
	  (TBG_ValueChangedCB(tb)))
      {
	 XFlush(XtDisplay(tb));
	 ToggleButtonCallback(tb, XmCR_VALUE_CHANGED, TBG_Set(tb), event);
      }

      if (XmIsRowColumn(XtParent(tb)))
      {
	 (* xmLabelGadgetClassRec.label_class.menuProcs)
	    (XmMENU_RESTORE_EXCLUDED_TEAROFF_TO_TOPLEVEL_SHELL, 
	    XtParent(tb), NULL, event, NULL);
      }
   }
}

/************************************************************************
 *
 * Compute Space
 *
 ***********************************************************************/
static void 
#ifdef _NO_PROTO
ComputeSpace( tb )
        XmToggleButtonGadget tb ;
#else
ComputeSpace(
        XmToggleButtonGadget tb )
#endif /* _NO_PROTO */
{

   int needed_width;
   int needed_height;

  /* COMPUTE SPACE FOR DRAWING TOGGLE */

   needed_width = LabG_TextRect(tb).width +
                  LabG_MarginLeft(tb) + LabG_MarginRight(tb) +
                  (2 * (tb->gadget.shadow_thickness +
                        tb->gadget.highlight_thickness +
                        LabG_MarginHeight(tb)));

   needed_height = LabG_TextRect(tb).height +
                   LabG_MarginTop(tb) + LabG_MarginBottom(tb) +
                   (2 * (tb->gadget.shadow_thickness +
                         tb->gadget.highlight_thickness +
                         LabG_MarginHeight(tb)));

   if (needed_height > tb->rectangle.height)
      if (TBG_IndOn(tb))
          LabG_TextRect(tb).y = tb->gadget.shadow_thickness +
                                tb->gadget.highlight_thickness +
                                LabG_MarginHeight(tb) +
                                LabG_MarginTop(tb) +
                                ((tb->rectangle.height - LabG_MarginTop(tb)
                                - LabG_MarginBottom(tb)
                                - (2 * (LabG_MarginHeight(tb)
                                + tb->gadget.highlight_thickness
                                + tb->gadget.shadow_thickness))
                                - LabG_TextRect(tb).height) / 2);

   if ((needed_width > tb->rectangle.width) ||
     ((LabG_Alignment(tb) == XmALIGNMENT_BEGINNING)
       && (needed_width < tb->rectangle.width)) ||
     ((LabG_Alignment(tb) == XmALIGNMENT_CENTER)
       && (needed_width < tb->rectangle.width)
       && (tb->rectangle.width - needed_width < LabG_MarginLeft(tb))) ||
     (needed_width == tb->rectangle.width))
   {

       if (TBG_IndOn(tb))
          LabG_TextRect(tb).x =  tb->gadget.shadow_thickness +
                                 tb->gadget.highlight_thickness +
                                 LabG_MarginHeight(tb) +
                                 LabG_MarginLeft(tb);
   } 

} /* ComputeSpace */

/*************************************<->*************************************
 *
 *  Redisplay(w, event, region) 
 *     Cause the widget, identified by w, to be redisplayed.
 *
 *************************************<->***********************************/
static void 
#ifdef _NO_PROTO
Redisplay( w, event, region )
        Widget w ;
        XEvent *event ;
        Region region ;
#else
Redisplay(
        Widget w,
        XEvent *event,
        Region region )
#endif /* _NO_PROTO */
{
   register XmToggleButtonGadget tb = (XmToggleButtonGadget) w;

   /* Fix CR #4884, D. Rand 6/4/92 */
   if (! XtIsRealized(w) ) return;
   /* End Fix */

   if (LabG_MenuType(tb) == XmMENU_PULLDOWN ||
       LabG_MenuType(tb) == XmMENU_POPUP) 
   {
      ShellWidget mshell = (ShellWidget) XtParent(XtParent(tb));
      if (! mshell->shell.popped_up)
	  return;
   }
   
   ComputeSpace(tb);

   if (LabG_IsPixmap (tb))
       SetAndDisplayPixmap( (Widget) tb, event, region);
   else
   {
      if (!TBG_IndOn(tb) && TBG_FillOnSelect(tb))
       DrawToggleLabel (tb);
      else
       (* xmLabelGadgetClassRec.rect_class.expose)((Widget)tb, event, region);
   } 

   if (TBG_IndOn(tb))
   {
     if (! TBG_Armed(tb))
       IsOn(tb) = TBG_Set(tb);
     DrawToggle(tb);
   }

   if (LabG_MenuType(tb) == XmMENU_PULLDOWN ||
       LabG_MenuType(tb) == XmMENU_POPUP) 
   {
      if (TBG_Armed(tb))
	 _XmDrawShadows (XtDisplay (tb), XtWindow (tb),
		       XmParentTopShadowGC(tb),
		       XmParentBottomShadowGC(tb),
		       tb->rectangle.x +
		       tb -> gadget.highlight_thickness,
		       tb->rectangle.y +
		       tb -> gadget.highlight_thickness,
		       tb->rectangle.width- 2*
			 tb->gadget.highlight_thickness,
		       tb->rectangle.height-2*
			 tb->gadget.highlight_thickness,
		       tb -> gadget.shadow_thickness,
		       XmSHADOW_OUT);
   }

   else
   {
      DrawToggleShadow (tb);
   }
}


/**************************************************************************
 * Resize(w, event)
 **************************************************************************/
static void 
#ifdef _NO_PROTO
Resize( w )
        Widget w ;
#else
Resize(
        Widget w )
#endif /* _NO_PROTO */
{
  register XmToggleButtonGadget tb = (XmToggleButtonGadget) w;
/* BEGIN OSF Fix pir 1778 */
   if (LabG_IsPixmap(w)) 
     SetToggleSize(tb);
   else
/* END OSF Fix pir 1778 */
     (* xmLabelGadgetClassRec.rect_class.resize)( (Widget) tb);
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
    Cardinal			size;
    XmToggleButtonGCacheObject  newSec, reqSec;

    cePtr = _XmGetBaseClassExtPtr(XtClass(newParent), XmQmotif);
    ec = (*cePtr)->secondaryObjectClass;
    size = ec->core_class.widget_size;

    newSec = (XmToggleButtonGCacheObject)_XmExtObjAlloc(size);
    reqSec = (XmToggleButtonGCacheObject)_XmExtObjAlloc(size);
    
    newSec->object.self = (Widget)newSec;
    newSec->object.widget_class = ec;
    newSec->object.parent = XtParent(newParent);
    newSec->object.xrm_name = newParent->core.xrm_name;
    newSec->object.being_destroyed = False;
    newSec->object.destroy_callbacks = NULL;
    newSec->object.constraints = NULL;

    newSec->ext.logicalParent = newParent;
    newSec->ext.extensionType = XmCACHE_EXTENSION;

    memcpy (&(newSec->label_cache), 
            LabG_Cache(newParent),
            sizeof(XmLabelGCacheObjPart));

    memcpy(&(newSec->toggle_cache), 
           TBG_Cache(newParent),
           sizeof(XmToggleButtonGCacheObjPart));

    extData = (XmWidgetExtData) XtCalloc(1, sizeof(XmWidgetExtDataRec));
    extData->widget = (Widget)newSec;
    extData->reqWidget = (Widget)reqSec;
    _XmPushWidgetExtData(newParent, extData, XmCACHE_EXTENSION);

    /*
     * Since the resource lists for label and togglebutton were merged at
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

    TBG_Cache(newParent) =
        &(((XmToggleButtonGCacheObject)newSec)->toggle_cache);
    TBG_Cache(refParent) =
        &(((XmToggleButtonGCacheObject)extData->reqWidget)->toggle_cache);

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
    Cardinal                    size;
    XmToggleButtonGCacheObject  newSec;

    cePtr = _XmGetBaseClassExtPtr(XtClass(newParent), XmQmotif);
    ec = (*cePtr)->secondaryObjectClass;
    size = ec->core_class.widget_size;

    newSec = (XmToggleButtonGCacheObject)_XmExtObjAlloc(size);

    newSec->object.self = (Widget)newSec;
    newSec->object.widget_class = ec;
    newSec->object.parent = XtParent(newParent);
    newSec->object.xrm_name = newParent->core.xrm_name;
    newSec->object.being_destroyed = False;
    newSec->object.destroy_callbacks = NULL;
    newSec->object.constraints = NULL;

    newSec->ext.logicalParent = newParent;
    newSec->ext.extensionType = XmCACHE_EXTENSION;

    memcpy (&(newSec->label_cache),
            LabG_Cache(newParent),
            sizeof(XmLabelGCacheObjPart));

    memcpy (&(newSec->toggle_cache),
            TBG_Cache(newParent),
            sizeof(XmToggleButtonGCacheObjPart));

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
     * Since the resource lists for label and togglebutton were merged at
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
    XmWidgetExtData             ext;

    /*
     * - register parts in cache.
     * - update cache pointers
     * - and free req
     */


  /* assign if changed! */
     if (!_XmLabelCacheCompare(LabG_Cache(new_w),
                         LabG_Cache(current)))
     {
	_XmCacheDelete( (XtPointer) LabG_Cache(current));  /* delete the old one */
	 LabG_Cache(new_w) = (XmLabelGCacheObjPart *)
	    _XmCachePart(LabG_ClassCachePart(new_w),
	                 (XtPointer) LabG_Cache(new_w),
	                 sizeof(XmLabelGCacheObjPart));
     } else
            LabG_Cache(new_w) = LabG_Cache(current);


    /* assign if changed! */
     if (!_XmToggleBCacheCompare(TBG_Cache(new_w),
                         TBG_Cache(current)))
     {
       _XmCacheDelete( (XtPointer) TBG_Cache(current));  /* delete the old one */
       TBG_Cache(new_w) = (XmToggleButtonGCacheObjPart *)
	 _XmCachePart(TBG_ClassCachePart(new_w),
	              (XtPointer) TBG_Cache(new_w),
		      sizeof(XmToggleButtonGCacheObjPart));
     } else
        TBG_Cache(new_w) = TBG_Cache(current);


      _XmPopWidgetExtData(new_w, &ext, XmCACHE_EXTENSION);

      _XmExtObjFree(ext->widget);
      _XmExtObjFree(ext->reqWidget);

      XtFree( (char *) ext);
 
      return FALSE;
}




/***************************************************************************
 *
 *
 *  SetValues(current, request, new_w)
 *     This is the set values procedure for the ToggleButton class.  It is
 *     called last (the set values rtnes for its superclasses are called
 *     first).
 *
 *
 *************************************<->***********************************/
static Boolean 
#ifdef _NO_PROTO
SetValues( current, request, new_w, args, num_args )
        Widget current ;
        Widget request ;
        Widget new_w ;
        ArgList args ;
        Cardinal *num_args ;
#else
SetValues(
        Widget current,
        Widget request,
        Widget new_w,
        ArgList args,
        Cardinal *num_args )
#endif /* _NO_PROTO */
{
    XmToggleButtonGadget curcbox = (XmToggleButtonGadget) current;
    XmToggleButtonGadget newcbox = (XmToggleButtonGadget) new_w;
    XmToggleButtonGadget reqcbox = (XmToggleButtonGadget) request;
    Boolean  flag = FALSE;    /* our return value */
    
    int maxIndicatorSize;   /* Max Indicator size permissible */
	int delta; 
	int boxSize;

    /**********************************************************************
     * Calculate the window size:  The assumption here is that if
     * the width and height are the same in the new and current instance
     * record that those fields were not changed with set values.  Therefore
     * its okay to recompute the necessary width and height.  However, if
     * the new and current do have different width/heights then leave them
     * alone because that's what the user wants.
     *********************************************************************/


   if (IsNull (PixmapOff(newcbox)) &&     /* no Off pixmap but do have */
       ! IsNull (PixmapOn(newcbox)))     /* an On, so use that */
   {
         PixmapOff(newcbox) = PixmapOn(newcbox);
         if ((LabG_RecomputeSize(newcbox)) &&
            (request->core.width == current->core.width))
              new_w->core.width = 0;
         if ((LabG_RecomputeSize(newcbox)) &&
            (request->core.height == current->core.height))
              new_w->core.height = 0;

         _XmCalcLabelGDimensions(new_w);
         (* xmLabelGadgetClassRec.rect_class.resize)( (Widget) newcbox);
   }

   if (IsNull (Pixmap_Insen_Off(newcbox)) &&  /* no Off pixmap but do have */
       ! IsNull (Pixmap_Insen_On(newcbox)))   /* an On, so use that */
   {
         Pixmap_Insen_Off(newcbox) = Pixmap_Insen_On(newcbox);
         if ((LabG_RecomputeSize(newcbox)) &&
            (request->core.width == current->core.width))
              new_w->core.width = 0;
         if ((LabG_RecomputeSize(newcbox)) &&
            (request->core.height == current->core.height))
              new_w->core.height = 0;

         _XmCalcLabelGDimensions(new_w);
         (* xmLabelGadgetClassRec.rect_class.resize)( (Widget) newcbox);
   }

/* BEGIN OSF Fix pir 1778 */
   /* Have to reset the TextRect width because label's resize will have
        mucked with it. */
   if (LabG_IsPixmap(newcbox) &&
      (!IsNull(PixmapOff(newcbox)) || !IsNull(PixmapOn(newcbox)) ||
      !IsNull(Pixmap_Insen_Off(newcbox)) ||
      !IsNull(Pixmap_Insen_On(newcbox))))
   {
      if (LabG_RecomputeSize(newcbox))
      {
         if (request->core.width == current->core.width)
            new_w->core.width = 0;
         if (request->core.height == current->core.height)
            new_w->core.height = 0;
      }

      SetToggleSize(newcbox);
   }
/* END OSF Fix pir 1778 */

   if ((LabG__label(newcbox) != LabG__label(curcbox)) ||
      (PixmapOff(newcbox) != PixmapOff(curcbox)) ||
      (LabG_Font(newcbox) != LabG_Font(curcbox)) ||
      (TBG_Spacing(newcbox) != TBG_Spacing(curcbox)) ||
      (PixmapOn(newcbox) != PixmapOn(curcbox)) ||
      (Pixmap_Insen_On(newcbox) != Pixmap_Insen_On(curcbox)) ||
      (TBG_IndOn(newcbox) != TBG_IndOn(curcbox)) ||
      (TBG_IndicatorDim(newcbox) != TBG_IndicatorDim(curcbox)) ||
      (LabG_IsPixmap(newcbox) != LabG_IsPixmap(curcbox)))
   {
       if (LabG_RecomputeSize(newcbox))
       {
         if (reqcbox->rectangle.width == curcbox->rectangle.width)
            newcbox->rectangle.width = 0;
         if (reqcbox->rectangle.height == curcbox->rectangle.height)
            newcbox->rectangle.height = 0;
       }

       if ((PixmapOn(newcbox) != PixmapOn(curcbox)) ||
           (Pixmap_Insen_On(newcbox) != Pixmap_Insen_On(curcbox)))
       {

          _XmCalcLabelGDimensions(new_w);
/* BEGIN OSF Fix pir 1778 */
          SetToggleSize(newcbox);
/* END OSF Fix pir 1778 */
       }


       if (( TBG_IndicatorDim(newcbox) == XmINVALID_DIMENSION ) ||
           ( PixmapOff(newcbox) != PixmapOff(curcbox)))
                 TBG_IndicatorSet(newcbox) = FALSE;

       if (!TBG_IndicatorSet(newcbox))
       {
	 if ((LabG__label(newcbox) != LabG__label(curcbox)) ||
	     (PixmapOff(newcbox) != PixmapOff(curcbox)) ||
	     (LabG_Font(newcbox) != LabG_Font(curcbox)) ||
	     (TBG_IndOn(newcbox) != TBG_IndOn(curcbox)))
	 {
	   if LabG_IsPixmap(new_w)
	   {
	      if (LabG_TextRect(newcbox).height < 13)
		 TBG_IndicatorDim(newcbox) = LabG_TextRect(newcbox).height;
	      else
		 TBG_IndicatorDim(newcbox) = 13 + (LabG_TextRect(newcbox).height/13);
	   }
	   else
	   {
	      Dimension height;
	      int line_count;

	      height = _XmStringHeight (LabG_Font(newcbox), LabG__label(newcbox));
	      line_count = _XmStringLineCount (LabG__label(newcbox));
	      if (line_count < 1)
                line_count = 1;

/* 
 * Fix for 5203 - Make the calculation for the indicator_dim be the same
 *                as in the Initialize procedure, i.e. Popup and Pulldown
 *                menus should have smaller indicators
 */
              if (LabG_MenuType(new_w) == XmMENU_POPUP ||
                  LabG_MenuType(new_w) == XmMENU_PULLDOWN) {
                 TBG_IndicatorDim(newcbox) = Max(XmDEFAULT_INDICATOR_DIM,
                   (height / ((Dimension)line_count))*2/3);
              } else
                 TBG_IndicatorDim(newcbox) = Max(XmDEFAULT_INDICATOR_DIM,
                    height / ((Dimension)line_count));
	   }
/*
 * End 5203 Fix
 */
	 } 
       } 

       if (LabG_IsPixmap(newcbox))
        TBG_IndicatorSet(newcbox) = TRUE;

       if (TBG_IndOn(newcbox))
         {
 /*
  * Fix CR 5568 - If the indicator is on and the user has changed the
  *             indicator dimension, calculate the new top and bottom
  *             margins in a place where they can effect the core width
  *             and height.
  */
  /*  Recompute the Top and bottom margins and the height of the text
   *      rectangle to  accommodate the size of toggle indicator.
   *  if (we are given a new toggleIndicator size)
   *        { if (user has given new top or bottom margin)
   *                       { compute to accomodate new toggle button size;
   *               }
   *           else (user has set new top/bottom margin)
   *               { Recompute margin to accommodate new toogleButtonIndicatorS
iz
 e;
   *                   }
   *        }
   */
    if (TBG_IndicatorDim(newcbox) != TBG_IndicatorDim(curcbox))
    { maxIndicatorSize = (int) (TBG_IndicatorDim(newcbox)) +
                              2 * (newcbox->gadget.shadow_thickness +
                                          Xm3D_ENHANCE_PIXEL);
          boxSize = (int) (LabG_TextRect(newcbox).height) +
                             (int) (LabG_MarginTop(newcbox)) +
                              (int)(LabG_MarginBottom(newcbox));
         if (maxIndicatorSize != boxSize)
           { delta = maxIndicatorSize - boxSize;
             if ( LabG_MarginTop(newcbox) == LabG_MarginTop(curcbox))
                    /* User has not specified new top margin */
                   { LabG_MarginTop(newcbox) = Max ( XmDEFAULT_TOP_MARGIN,
                                                 (int) LabG_MarginTop(newcbox) +
                                                 delta/2);
                   }
             else
               /* User has sepcified a top margin  and
                         Margin must not be less than user specified amount */
                  { LabG_MarginTop(newcbox) = Max( LabG_MarginTop(newcbox),
                                                  (LabG_MarginTop(newcbox)
                                                   + delta/2));
                  }

         if ( LabG_MarginBottom(newcbox) == LabG_MarginBottom(curcbox))
            /* User has not specified new bottom margin */
           { LabG_MarginBottom(newcbox) = Max ( XmDEFAULT_BOTTOM_MARGIN,
                         (int) LabG_MarginBottom(newcbox) + delta/2);
           }
         else
           /* User has sepcified a bottom margin  and
             Margin must not be less than user specified amount */
          { LabG_MarginBottom(newcbox) = Max( LabG_MarginBottom(newcbox),
                             (LabG_MarginBottom(newcbox) + delta/2));
          }
        }
     }

          if ((LabG_MarginLeft(newcbox) <
		(TBG_IndicatorDim(newcbox) + TBG_Spacing(newcbox))) ||
	       TBG_Spacing(newcbox) != TBG_Spacing(curcbox))
            LabG_MarginLeft(newcbox) = TBG_IndicatorDim(newcbox) +
                                            TBG_Spacing(newcbox);
         }

       if (LabG_RecomputeSize(newcbox))
       {
         if (reqcbox->rectangle.width == curcbox->rectangle.width)
            newcbox->rectangle.width = 0;
         if (reqcbox->rectangle.height == curcbox->rectangle.height)
            newcbox->rectangle.height = 0;
       }

       if (newcbox->rectangle.width == 0)
       {
         newcbox->rectangle.width =
                   LabG_TextRect(newcbox).width +
                   LabG_MarginLeft(newcbox) + LabG_MarginRight(newcbox) +
                   2 * (newcbox->gadget.highlight_thickness +
                        newcbox->gadget.shadow_thickness +
                        LabG_MarginHeight(newcbox));

         if (newcbox->rectangle.width == 0)
            newcbox->rectangle.width = 1;

         flag = TRUE;
       }

       if (newcbox->rectangle.height == 0)
       {
         newcbox->rectangle.height = Max(TBG_IndicatorDim(newcbox),
            LabG_TextRect(newcbox).height + 2*LabG_MarginHeight(newcbox) +
                     LabG_MarginTop(newcbox) + LabG_MarginBottom(newcbox)) +
                    2 * (newcbox->gadget.highlight_thickness +
                         newcbox->gadget.shadow_thickness);

         if (newcbox->rectangle.height == 0)
            newcbox->rectangle.height = 1;

         flag = TRUE;
       }


   }



    if ((TBG_IndType(curcbox) != TBG_IndType(newcbox)) ||
       (TBG_Visible(curcbox) != TBG_Visible(newcbox))) 
    {
      if(    !XmRepTypeValidValue( XmRepTypeGetId(XmRIndicatorType),
                                  TBG_IndType( newcbox), (Widget) newcbox)    )
      {
         TBG_IndType(newcbox) = TBG_IndType(curcbox);
      }
       flag = True;
    }

    if (TBG_SelectColor(curcbox) != TBG_SelectColor(newcbox) ||
        XtParent(curcbox)->core.background_pixel != 
	           XtParent(newcbox)->core.background_pixel)
    {
      XtReleaseGC(XtParent(curcbox), TBG_SelectGC(curcbox));
      XtReleaseGC(XtParent(curcbox), TBG_BackgroundGC(curcbox));
      GetGC(newcbox);
      flag = TRUE;
    }


    if (TBG_Set(curcbox) != TBG_Set(newcbox))
    {
       IsOn(newcbox) = TBG_Set(newcbox);
          if (flag == False && TBG_IndOn (newcbox) && XtIsRealized(newcbox))
						DrawToggle (newcbox);
          /**    flag = True;           **/
    }

/* BEGIN OSF Fix pir 2097 */
    newcbox->gadget.event_mask = XmARM_EVENT | XmACTIVATE_EVENT |
      XmMULTI_ARM_EVENT | XmMULTI_ACTIVATE_EVENT | XmHELP_EVENT |
        XmFOCUS_IN_EVENT | XmFOCUS_OUT_EVENT | XmENTER_EVENT | XmLEAVE_EVENT |
          XmBDRAG_EVENT;
/* END OSF Fix pir 2097 */

    return(flag);
}

/***************************************************************
 *
 * XmToggleButtonGadgetGetState  
 *    This function gets the state of the toggle gadget.
 *
 ***************************************************************/
Boolean 
#ifdef _NO_PROTO
XmToggleButtonGadgetGetState( w )
        Widget w ;
#else
XmToggleButtonGadgetGetState(
        Widget w )
#endif /* _NO_PROTO */
{
     XmToggleButtonGadget tg = (XmToggleButtonGadget) w;

     return (TBG_Set(tg));
}

/****************************************************************
 *
 * XmToggleButtonGadgetSetState
 *    This function sets the state of the toggle gadget.
 *
 ****************************************************************/
void 
#ifdef _NO_PROTO
XmToggleButtonGadgetSetState( w, newstate, notify )
        Widget w ;
        Boolean newstate ;
        Boolean notify ;
#else
XmToggleButtonGadgetSetState(
        Widget w,
#if NeedWidePrototypes
        int newstate,
        int notify )
#else
        Boolean newstate,
        Boolean notify )
#endif /* NeedWidePrototypes */
#endif /* _NO_PROTO */
{

    XmToggleButtonGadget tg = (XmToggleButtonGadget) w;

    if (TBG_Set(tg) != newstate)
    {
      TBG_Set(tg) = newstate;
      IsOn(tg) = newstate;
      if (XtIsRealized (tg))
      {
        if (TBG_IndOn(tg))
          DrawToggle(tg);
        else
        {
           if (tg->gadget.shadow_thickness> 0)
             DrawToggleShadow (tg);
           if (TBG_FillOnSelect(tg) && !LabG_IsPixmap(tg))
             DrawToggleLabel (tg);
        }
        if (LabG_IsPixmap(tg))
          SetAndDisplayPixmap( (Widget) tg, NULL, NULL);
      }
      if (notify)
      {
	 /* if the parent is a RowColumn, notify it about the select */
	 if (XmIsRowColumn(XtParent(tg)))
	 {
	    XmToggleButtonCallbackStruct call_value;
	    call_value.reason = XmCR_VALUE_CHANGED;
	    call_value.event = NULL;
	    call_value.set = TBG_Set(tg);
	    (* xmLabelGadgetClassRec.label_class.menuProcs) (XmMENU_CALLBACK,
               XtParent(tg), FALSE, tg, &call_value);
	 }

	 if ((! LabG_SkipCallback(tg)) &&
	     (TBG_ValueChangedCB(tg)))
	 {
	    if (XtIsRealized (tg))
	       XFlush (XtDisplay (tg));
	    ToggleButtonCallback(tg, XmCR_VALUE_CHANGED, TBG_Set(tg), NULL);
	 }
      }
    }
} 

/***********************************************************************
 *
 * XmCreateToggleButtonGadget
 *   Creates an instance of a togglebutton and returns the widget id.
 *
 ***********************************************************************/
Widget 
#ifdef _NO_PROTO
XmCreateToggleButtonGadget( parent, name, arglist, argCount )
        Widget parent ;
        char *name ;
        Arg *arglist ;
        Cardinal argCount ;
#else
XmCreateToggleButtonGadget(
        Widget parent,
        char *name,
        Arg *arglist,
        Cardinal argCount )
#endif /* _NO_PROTO */
{
    return (XtCreateWidget(name,xmToggleButtonGadgetClass,parent,arglist,argCount));
}

/****************************************************
 *   Functions for manipulating Secondary Resources.
 *********************************************************/
/*
 * GetPushBGSecResData()
 *    Create a XmSecondaryResourceDataRec for each secondary resource;
 *    Put the pointers to these records in an array of pointers;
 *    Return the pointer to the array of pointers.
 */
static Cardinal 
#ifdef _NO_PROTO
GetToggleBGClassSecResData( w_class, data_rtn )
        WidgetClass w_class ;
        XmSecondaryResourceData **data_rtn ;
#else
GetToggleBGClassSecResData(
        WidgetClass w_class,
        XmSecondaryResourceData **data_rtn )
#endif /* _NO_PROTO */
{   int arrayCount = 0;
    XmBaseClassExt  bcePtr;
    String  resource_class, resource_name;
    XtPointer  client_data;

    bcePtr = &(  ToggleBGClassExtensionRec );
    client_data = NULL;
    resource_class = NULL;
    resource_name = NULL;
    arrayCount =
      _XmSecondaryResourceData ( bcePtr, data_rtn, client_data,
                resource_name, resource_class,
                GetToggleBGClassSecResBase) ;
    return (arrayCount);

}


/*
 * GetToggleBGClassResBase ()
 *   return the address of the base of resources.
 */
static XtPointer 
#ifdef _NO_PROTO
GetToggleBGClassSecResBase( widget, client_data )
        Widget widget ;
        XtPointer client_data ;
#else
GetToggleBGClassSecResBase(
        Widget widget,
        XtPointer client_data )
#endif /* _NO_PROTO */
{   XtPointer  widgetSecdataPtr;
    int  labg_cache_size = sizeof (XmLabelGCacheObjPart);
    int  togglebg_cache_size = sizeof (XmToggleButtonGCacheObjPart);
    char *cp;

    widgetSecdataPtr = (XtPointer)
            (XtMalloc ( labg_cache_size + togglebg_cache_size + 1));

    if (widgetSecdataPtr)
      { cp = (char *) widgetSecdataPtr;
        memcpy (cp, LabG_Cache(widget), labg_cache_size);
        cp += labg_cache_size;
        memcpy (cp, TBG_Cache(widget), togglebg_cache_size);
      }
/* else Warning: error cannot allocate Memory */


	return ( widgetSecdataPtr);
}
/*
 * DrawToggleLabel (tb)
 *    Called when XmNindicatorOn  is set to false and XmNfillOnSelect
 *    is set true. Fill toggle with selectColor or background
 *    depending on toggle value, and draw label.
 */
static void 
#ifdef _NO_PROTO
DrawToggleLabel( tb )
        XmToggleButtonGadget tb ;
#else
DrawToggleLabel(
        XmToggleButtonGadget tb )
#endif /* _NO_PROTO */
{
    XmManagerWidget mw = (XmManagerWidget) XtParent(tb);
    Dimension margin = tb->gadget.highlight_thickness +
                      tb->gadget.shadow_thickness;
    Position fx = tb->rectangle.x + margin;
    Position fy = tb->rectangle.y + margin;
    int fw = tb->rectangle.width - 2 * margin;
    int fh = tb->rectangle.height - 2 * margin;
    Boolean restore_gc = False;
    GC tmp_gc = NULL;

    if ((mw->manager.top_shadow_color == TBG_SelectColor(tb)) ||
       (mw->manager.bottom_shadow_color == TBG_SelectColor(tb)))
    {
      fx += 1;
      fy += 1;
      fw -= 2;
      fh -= 2;
    }

    if (fw < 0 || fh < 0)
      return;

    XFillRectangle (XtDisplay(tb), XtWindow(tb), (IsOn(tb) ?
                      TBG_SelectGC(tb) : TBG_BackgroundGC(tb)),
                      fx, fy, fw, fh);

    if (mw->manager.foreground == TBG_SelectColor(tb) && IsOn(tb))
    {
        tmp_gc =  LabG_NormalGC(tb);
        LabG_NormalGC(tb) = TBG_BackgroundGC(tb);
        restore_gc = True;
    }

    (* xmLabelGadgetClassRec.rect_class.expose) ((Widget)tb, NULL, NULL);

    if (restore_gc)
      LabG_NormalGC(tb) = tmp_gc;
}

/*
 * DrawToggleShadow (tb)
 *   - Should be called only if ToggleShadow are to be drawn ;
 *  if the IndicatorOn resource is set to false top and bottom shadows
 *  will be switched depending on whether the Toggle is selected or
 *  unselected.
 */
static void 
#ifdef _NO_PROTO
DrawToggleShadow( tb )
        XmToggleButtonGadget tb ;
#else
DrawToggleShadow(
        XmToggleButtonGadget tb )
#endif /* _NO_PROTO */
{   
   GC topgc, bottomgc;
   int dx, dy, width, height;
   int hilite_thickness;

   if (!(TBG_IndOn(tb)))
   { 
      if (IsOn(tb)) 
      { 
	 topgc = XmParentBottomShadowGC(tb);
	 bottomgc = XmParentTopShadowGC(tb);
      }
      else
      { 
	 topgc = XmParentTopShadowGC(tb);
         bottomgc = XmParentBottomShadowGC(tb);
      }
   }
   else
   { 
      topgc = XmParentTopShadowGC(tb);
      bottomgc = XmParentBottomShadowGC(tb);
   }

   hilite_thickness = tb->gadget.highlight_thickness;
   dx = (int)(tb->rectangle.x + hilite_thickness);
   dy = (int)(tb->rectangle.y + hilite_thickness);
   width = (int) ( tb->rectangle.width - (hilite_thickness << 1));
   height = (int) ( tb->rectangle.height - (hilite_thickness << 1));

   _XmDrawShadows (XtDisplay (tb), XtWindow (tb),
		topgc, bottomgc, dx, dy, width, height,
		tb-> gadget.shadow_thickness, XmSHADOW_OUT);
}

/* BEGIN OSF Fix pir 1778 */
/************************************************************************
 *
 * SetToggleSize(newtbg)
 * Set size properly when XmNselectPixmap or XmNselectInsensitivePixmaps
 * are set in addition to the corresponding labelPixmaps.  Have to pick
 * the largest dimensions.
 *
 ************************************************************************/

static void
#ifdef _NO_PROTO
SetToggleSize(newtbg)
	XmToggleButtonGadget newtbg;
#else
SetToggleSize(
	XmToggleButtonGadget newtbg)
#endif /* _NO_PROTO */
 {
   int leftx, rightx, dispx;
   Window root;
   int x,y;
   unsigned int  onW = 0 , onH = 0, offW = 0, offH = 0, border, d;

    /* initialize TextRect width and height to 0, reset if needed */
    LabG_TextRect(newtbg).width = 0;
    LabG_TextRect(newtbg).height = 0;
    LabG_AccTextRect(newtbg).width = 0;
    LabG_AccTextRect(newtbg).height = 0;

   /* We know it's a pixmap so find out how how big it is */
    if (XtIsSensitive((Widget) newtbg))
      {
        if (!IsNull(PixmapOn(newtbg)))
	  XGetGeometry (XtDisplay(newtbg),
			PixmapOn(newtbg),
			&root,		/* returned root window */
			&x, &y,		/* returned x, y of pixmap */
			&onW, &onH,	/* returned width, height of pixmap */
			&border,	/* returned border width */
			&d);		/* returned depth */

        if (!IsNull(PixmapOff(newtbg)))
	  XGetGeometry (XtDisplay(newtbg),
			PixmapOff(newtbg),
			&root,		/* returned root window */
			&x, &y,		/* returned x, y of pixmap */
			&offW, &offH,	/* returned width, height of pixmap */
			&border,	/* returned border width */
			&d);		/* returned depth */
      }
    else
      {
        if (!IsNull(Pixmap_Insen_On(newtbg)))
	  XGetGeometry (XtDisplay(newtbg),
			Pixmap_Insen_On(newtbg),
			&root,		/* returned root window */
			&x, &y,		/* returned x, y of pixmap */
			&onW, &onH,	/* width, height of pixmap */
			&border,	/* border width */
			&d);		/* depth */

        if (!IsNull(Pixmap_Insen_Off(newtbg)))
	  XGetGeometry (XtDisplay(newtbg),
			Pixmap_Insen_Off(newtbg),
			&root,		/* returned root window */
			&x, &y,		/* returned x, y of pixmap */
			&offW, &offH,	/* width, height of pixmap */
			&border,	/* border width */
			&d);		/* depth */

      }
   LabG_TextRect(newtbg).width = (unsigned short) ((onW > offW) ? onW : offW);
   LabG_TextRect(newtbg).height = (unsigned short) ((onH > offH) ? onH : offH);

   if (LabG__acceleratorText(newtbg) != NULL)
       {
        Dimension w,h ;

        /*
         * If we have a string then size it.
         */
        if (!_XmStringEmpty (LabG__acceleratorText(newtbg)))
        {
           _XmStringExtent(LabG_Font(newtbg),
                           LabG__acceleratorText(newtbg), &w, &h);
           LabG_AccTextRect(newtbg).width = (unsigned short)w;
           LabG_AccTextRect(newtbg).height = (unsigned short)h;
        }
       }

    /* increase margin width if necessary to accomadate accelerator text */
    if (LabG__acceleratorText(newtbg) != NULL)

        if (LabG_MarginRight(newtbg) <
          LabG_AccTextRect(newtbg).width + LABELG_ACC_PAD)
        {
         LabG_MarginRight(newtbg) =
             LabG_AccTextRect(newtbg).width + LABELG_ACC_PAD;
        }

    /* Has a width been specified?  */

    if (newtbg->rectangle.width == 0)
        newtbg->rectangle.width =
            LabG_TextRect(newtbg).width +
              LabG_MarginLeft(newtbg) + LabG_MarginRight(newtbg) +
                  (2 * (LabG_MarginWidth(newtbg) +
                        newtbg->gadget.highlight_thickness
                               + newtbg->gadget.shadow_thickness));

    leftx =  newtbg->gadget.highlight_thickness +
             newtbg->gadget.shadow_thickness + LabG_MarginWidth(newtbg) +
             LabG_MarginLeft(newtbg);

    rightx = newtbg->rectangle.width - newtbg->gadget.highlight_thickness -
             newtbg->gadget.shadow_thickness - LabG_MarginWidth(newtbg) -
             LabG_MarginRight(newtbg);


    switch (LabG_Alignment(newtbg))
    {
     case XmALIGNMENT_BEGINNING:
         LabG_TextRect(newtbg).x = leftx;
       break;

     case XmALIGNMENT_END:
       LabG_TextRect(newtbg).x = rightx - LabG_TextRect(newtbg).width;
       break;

     default:
       /* XmALIGNMENT_CENTER */
       dispx = ( (rightx -leftx) - LabG_TextRect(newtbg).width)/2;
       LabG_TextRect(newtbg).x = leftx + dispx;

       break;
    }

    /*  Has a height been specified?  */
    if (newtbg->rectangle.height == 0)
        newtbg->rectangle.height = Max(LabG_TextRect(newtbg).height,
                                    LabG_AccTextRect(newtbg).height)
          + LabG_MarginTop(newtbg)
              + LabG_MarginBottom(newtbg)
                  + (2 * (LabG_MarginHeight(newtbg)
                          + newtbg->gadget.highlight_thickness
                          + newtbg->gadget.shadow_thickness));

    LabG_TextRect(newtbg).y = newtbg->gadget.highlight_thickness
          + newtbg->gadget.shadow_thickness
              + LabG_MarginHeight(newtbg) + LabG_MarginTop(newtbg) +
                  ((newtbg->rectangle.height - LabG_MarginTop(newtbg)
                    - LabG_MarginBottom(newtbg)
                    - (2 * (LabG_MarginHeight(newtbg)
                            + newtbg->gadget.highlight_thickness
                            + newtbg->gadget.shadow_thickness))
                    - LabG_TextRect(newtbg).height) / 2);

    if (LabG__acceleratorText(newtbg) != NULL)
    {

       LabG_AccTextRect(newtbg).x = (newtbg->rectangle.width -
          newtbg->gadget.highlight_thickness -
          newtbg->gadget.shadow_thickness -
          LabG_MarginWidth(newtbg) -
          LabG_MarginRight(newtbg) +
          LABELG_ACC_PAD);

       LabG_AccTextRect(newtbg).y = newtbg->gadget.highlight_thickness
             + newtbg->gadget.shadow_thickness
                 + LabG_MarginHeight(newtbg) + LabG_MarginTop(newtbg) +
                     ((newtbg->rectangle.height - LabG_MarginTop(newtbg)
                       - LabG_MarginBottom(newtbg)
                       - (2 * (LabG_MarginHeight(newtbg)
                               + newtbg->gadget.highlight_thickness
                               + newtbg->gadget.shadow_thickness))
                       - LabG_AccTextRect(newtbg).height) / 2);

     }

    if (newtbg->rectangle.width == 0)    /* set core width and height to a */
        newtbg->rectangle.width = 1;     /* default value so that it doesn't */
    if (newtbg->rectangle.height == 0)   /* cause a Toolkit Error */
        newtbg->rectangle.height = 1;
 }
/* END OSF Fix pir 1778 */

/* 
 * (c) Copyright 1989, 1990, 1991, 1992, 1993, 1994 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.4
*/ 
/*   $RCSfile: CascadeBGP.h,v $ $Revision: 1.1 $ $Date: 1998-09-18 23:47:35 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
#ifndef  _XmCascadeBGP_h
#define  _XmCascadeBGP_h

#include <ncarg/ngo/CascadeBG.h>
#include <Xm/LabelGP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*********************************************************************
 * The Arrow Cache record for the menu cascade button
 *********************************************************************/
typedef struct _XmArrowPixmap
{
   Dimension height, width;
   unsigned int depth;
   Pixel top_shadow_color, bottom_shadow_color, foreground_color;
   Display *display;
   Screen *screen;
   Pixmap pixmap;
} XmArrowPixmap;


/*********************************************************************/
/* The CascadeButton Gadget Cache Object's class and instance records*/
/*********************************************************************/


typedef struct _XmCascadeButtonGCacheObjClassPart
{
    int foo;

} XmCascadeButtonGCacheObjClassPart;


typedef struct _XmCascadeButtonGCacheObjClassRec
{
        ObjectClassPart                     object_class;
        XmExtClassPart                      ext_class;
        XmLabelGCacheObjClassPart           label_class_cache;
        XmCascadeButtonGCacheObjClassPart   cascade_button_class_cache;

} XmCascadeButtonGCacheObjClassRec;

externalref XmCascadeButtonGCacheObjClassRec xmCascadeButtonGCacheObjClassRec;


typedef struct _XmCascadeButtonGCacheObjPart
{
    Pixmap              cascade_pixmap;         /* pixmap for the cascade */
    int                 map_delay;              /* time delay for posting */
    Pixmap		armed_pixmap;
} XmCascadeButtonGCacheObjPart;

typedef struct _XmCascadeButtonGCacheObjRec
{
    ObjectPart                   object;
    XmExtPart                    ext;
    XmLabelGCacheObjPart         label_cache;
    XmCascadeButtonGCacheObjPart cascade_button_cache;
} XmCascadeButtonGCacheObjRec;

/* The CascadeButtonGadget instance record */

typedef	struct _XmCascadeButtonGadgetPart
{			/* resources */
    Widget		submenu;		/* the menu to pull down */
    XtCallbackList	activate_callback;	/* widget fired callback */
    XtCallbackList	cascade_callback;	/* optional callback, called */
						/* when the menu is about */
						/* to be pulled down */
			/* internal fields */
    Boolean		armed;			/* armed flag */
    XRectangle		cascade_rect;		/* location of cascade*/
    XtIntervalId	timer;			/* timeout id */
    XmCascadeButtonGCacheObjPart         *cache;

} XmCascadeButtonGadgetPart;


/* Full instance record declaration */

typedef struct _XmCascadeButtonGadgetRec
{
    ObjectPart		       object;
    RectObjPart                rectangle;
    XmGadgetPart               gadget;
    XmLabelGadgetPart          label;
    XmCascadeButtonGadgetPart  cascade_button;
} XmCascadeButtonGadgetRec;



/* CascadeButton class structure */

typedef struct 
{
    XtPointer	extension;	/* Pointer to extension record */
} XmCascadeButtonGadgetClassPart;


/* Full class record declaration for CascadeButton class */

typedef struct _XmCascadeButtonGadgetClassRec 
{
    RectObjClassPart               rect_class;
    XmGadgetClassPart              gadget_class;
    XmLabelGadgetClassPart         label_class;
    XmCascadeButtonGadgetClassPart cascade_button_class;
} XmCascadeButtonGadgetClassRec;


externalref XmCascadeButtonGadgetClassRec   xmCascadeButtonGadgetClassRec;

/* Access macro definitions  for UNcached fields*/

#define CBG_Submenu(cb)		(((XmCascadeButtonGadget) 		    \
                                  cb)->cascade_button.submenu)
#define CBG_ActivateCall(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.activate_callback)
#define CBG_CascadeCall(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.cascade_callback)
#define CBG_Armed(cb)		(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.armed)
#define CBG_CascadeRect(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.cascade_rect)
#define CBG_Timer(cb)           (((XmCascadeButtonGadget)                    \
				  cb)->cascade_button.timer)
#define CBG_Cascade_x(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.cascade_rect.x)
#define CBG_Cascade_y(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.cascade_rect.y)
#define CBG_Cascade_width(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.cascade_rect.width)
#define CBG_Cascade_height(cb)	(((XmCascadeButtonGadget)                    \
                                  cb)->cascade_button.cascade_rect.height)
#define CBG_HasCascade(cb)      (((LabG_MenuType(cb) == XmMENU_PULLDOWN)  || \
			          (LabG_MenuType(cb) == XmMENU_POPUP) ||     \
                                  (LabG_MenuType(cb) == XmMENU_OPTION)) &&   \
			         (CBG_Submenu(cb)))

#define XmCBG_ARMED_BIT	      (1 << 0)	
#define XmCBG_TRAVERSE_BIT     (1 << 1)

#define CBG_IsArmed(cb)	 (((XmCascadeButtonGadget)(cb))->cascade_button.armed \
			  & XmCB_ARMED_BIT)
#define CBG_Traversing(cb) (((XmCascadeButtonGadget)                          \
			    (cb))->cascade_button.armed & XmCBG_TRAVERSE_BIT)

#define CBG_SetBit(byte,bit,v)  byte = (byte & (~bit)) | (v ? bit : 0)

#define CBG_SetArmed(cb,v)  CBG_SetBit (((XmCascadeButtonGadget)	     \
				       (cb))->cascade_button.armed,          \
				      XmCBG_ARMED_BIT, v)

#define CBG_SetTraverse(cb,v)  CBG_SetBit (((XmCascadeButtonGadget)	     \
				       (cb))->cascade_button.armed,          \
				      XmCBG_TRAVERSE_BIT, v)


				  
/* Access macro definitions  for Cached fields*/

#define CBG_CascadePixmap(cb)   (((XmCascadeButtonGadget)                    \
				  cb)->cascade_button.cache->cascade_pixmap)
#define CBG_MapDelay(cb)        (((XmCascadeButtonGadget)                    \
				  cb)->cascade_button.cache->map_delay)
#define CBG_ArmedPixmap(cb)	(((XmCascadeButtonGadget)                    \
				  cb)->cascade_button.cache->armed_pixmap)


/******************************/
/* Convenience Macros         */
/******************************/


#define CBG_Cache(w)                    (((XmCascadeButtonGadget)(w))->\
					   cascade_button.cache)
#define CBG_ClassCachePart(w) \
	(((XmCascadeButtonGadgetClass)xmCascadeButtonGadgetClass)->gadget_class.cache_part)


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

extern int _XmArrowPixmapCacheCompare() ;
extern void _XmArrowPixmapCacheDelete() ;
extern void _XmCreateArrowPixmaps() ;

#else

extern int _XmArrowPixmapCacheCompare( 
                        XtPointer A,
                        XtPointer B) ;
extern void _XmArrowPixmapCacheDelete( 
                        XtPointer data) ;
extern void _XmCreateArrowPixmaps( 
                        Widget wid) ;

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif  /* _XmCascadeBGP_h */
/* DON'T ADD STUFF AFTER THIS #endif */

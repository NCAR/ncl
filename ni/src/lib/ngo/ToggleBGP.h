/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2.1
*/ 
/*   $RCSfile: ToggleBGP.h,v $ $Revision: 1.1 $ $Date: 1998-09-18 23:47:37 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
/********************************************
 *
 *   No new fields need to be defined
 *   for the Toggle widget class record
 *
 ********************************************/

#ifndef _XmToggleButtonGP_h
#define _XmToggleButtonGP_h

#include <ncarg/ngo/ToggleBG.h>
#include <Xm/LabelGP.h>

#ifdef __cplusplus
extern "C" {
#endif

/*************************************************************/
/* The  ToggleButton Gadget Cache Object's class and instance records*/
/*************************************************************/


typedef struct _XmToggleButtonGCacheObjClassPart
{
    int foo;
} XmToggleButtonGCacheObjClassPart;


typedef struct _XmToggleButtonGCacheObjClassRec 
{
	ObjectClassPart                     object_class;
        XmExtClassPart                      ext_class;
	XmLabelGCacheObjClassPart           label_class_cache;
	XmToggleButtonGCacheObjClassPart    toggle_class_cache;
} XmToggleButtonGCacheObjClassRec;

externalref XmToggleButtonGCacheObjClassRec xmToggleButtonGCacheObjClassRec;


typedef struct _XmToggleButtonGCacheObjPart
{ 
   unsigned char	ind_type;
   Boolean		visible;
   Dimension		spacing;
   Dimension		indicator_dim;
   Pixmap		on_pixmap; 
   Pixmap		insen_pixmap; 
   Boolean		ind_on;
   Boolean		fill_on_select;
   Pixel		select_color;
   GC			select_GC;
   GC			background_gc;
} XmToggleButtonGCacheObjPart;

typedef struct _XmToggleButtonGCacheObjRec
{
    ObjectPart                              object;
    XmExtPart                		    ext;
    XmLabelGCacheObjPart     		    label_cache;
    XmToggleButtonGCacheObjPart             toggle_cache;
} XmToggleButtonGCacheObjRec;


/****************************************************
 *
 * Full class record declaration for Toggle class
 *
 ****************************************************/

typedef struct _XmToggleButtonGadgetClassPart
 {
   XtPointer				   extension;
 } XmToggleButtonGadgetClassPart;


typedef struct _XmToggleButtonGadgetClassRec {
    RectObjClassPart  	 	  	rect_class;
    XmGadgetClassPart  			gadget_class;
    XmLabelGadgetClassPart 	    	label_class;
    XmToggleButtonGadgetClassPart	toggle_class;
} XmToggleButtonGadgetClassRec;


externalref XmToggleButtonGadgetClassRec xmToggleButtonGadgetClassRec;


typedef struct _XmToggleButtonGadgetPart
{ 
   Boolean		indicator_set;
   Boolean		set;
   Boolean      	visual_set; /* used for visuals and does not reflect
   		                            the true state of the button */
   Boolean     		Armed;
   XtCallbackList       value_changed_CB,
			arm_CB,
		        disarm_CB;

   XmToggleButtonGCacheObjPart  *cache; /* Replace cache instance fields */
					/* with a pointer */
} XmToggleButtonGadgetPart;



/****************************************************************
 *
 * Full instance record declaration
 *
 ****************************************************************/

typedef struct _XmToggleButtonGadgetRec {
    ObjectPart			object;
    RectObjPart			rectangle;
    XmGadgetPart		gadget;
    XmLabelGadgetPart		label;
    XmToggleButtonGadgetPart	toggle;
} XmToggleButtonGadgetRec;


/* MACROS */
/**********/
 
/* Macros for cached instance fields */

#define TBG_IndType(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->ind_type)
#define TBG_Visible(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->visible)
#define TBG_Spacing(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->spacing)
#define TBG_IndicatorDim(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->indicator_dim)
#define TBG_OnPixmap(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->on_pixmap)
#define TBG_InsenPixmap(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->insen_pixmap)
#define TBG_IndOn(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->ind_on)
#define TBG_FillOnSelect(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->fill_on_select)
#define TBG_SelectColor(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->select_color)
#define TBG_SelectGC(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->select_GC)
#define TBG_BackgroundGC(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.cache->background_gc)

/***************************************/
/* Macros for uncached instance fields */

#define TBG_IndicatorSet(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.indicator_set)
#define TBG_Set(w)		(((XmToggleButtonGadget) (w)) -> \
                		   toggle.set)
#define TBG_VisualSet(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.visual_set)
#define TBG_ValueChangedCB(w)	(((XmToggleButtonGadget) (w)) -> \
				   toggle.value_changed_CB)
#define TBG_ArmCB(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.arm_CB)
#define TBG_DisarmCB(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.disarm_CB)
#define TBG_Armed(w)		(((XmToggleButtonGadget) (w)) -> \
				   toggle.Armed)

/******************************/
/* Convenience Macros         */
/******************************/

#define TBG_Cache(w)                    (((XmToggleButtonGadget)(w))->\
					   toggle.cache)
#define TBG_ClassCachePart(w) \
        (((XmToggleButtonGadgetClass)xmToggleButtonGadgetClass)->gadget_class.cache_part)


/********    Private Function Declarations    ********/
#ifdef _NO_PROTO

extern int _XmToggleBCacheCompare() ;

#else

extern int _XmToggleBCacheCompare( 
                        XtPointer A,
                        XtPointer B) ;

#endif /* _NO_PROTO */
/********    End Private Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmToggleButtonGP_h */
/* DON'T ADD ANYTHING AFTER THIS #endif */

/* 
 * (c) Copyright 1989, 1990, 1991, 1992 OPEN SOFTWARE FOUNDATION, INC. 
 * ALL RIGHTS RESERVED 
*/ 
/* 
 * Motif Release 1.2
*/ 
/*   $RCSfile: ToggleBG.h,v $ $Revision: 1.1 $ $Date: 1998-09-18 23:47:36 $ */
/*
*  (c) Copyright 1989, DIGITAL EQUIPMENT CORPORATION, MAYNARD, MASS. */
/*
*  (c) Copyright 1987, 1988, 1989, 1990, 1991, 1992 HEWLETT-PACKARD COMPANY */
/***********************************************************************
 *
 * Toggle Gadget
 *
 ***********************************************************************/
#ifndef	DCEHACKS
#include <Xm/ToggleBG.h>
#else
#ifndef _XmToggleG_h
#define _XmToggleG_h

#include <Xm/Xm.h>

#ifdef __cplusplus
extern "C" {
#endif


externalref WidgetClass xmToggleButtonGadgetClass;

typedef struct _XmToggleButtonGadgetClassRec     *XmToggleButtonGadgetClass;
typedef struct _XmToggleButtonGadgetRec          *XmToggleButtonGadget;
typedef struct _XmToggleButtonGCacheObjRec       *XmToggleButtonGCacheObject;


/*fast subclass define */
#ifndef XmIsToggleButtonGadget
#define XmIsToggleButtonGadget(w)     XtIsSubclass(w, xmToggleButtonGadgetClass)
#endif /* XmIsToggleButtonGadget */


/********    Public Function Declarations    ********/
#ifdef _NO_PROTO

extern Boolean XmToggleButtonGadgetGetState() ;
extern void XmToggleButtonGadgetSetState() ;
extern Widget XmCreateToggleButtonGadget() ;

#else

extern Boolean XmToggleButtonGadgetGetState( 
                        Widget w) ;
extern void XmToggleButtonGadgetSetState( 
                        Widget w,
#if NeedWidePrototypes
                        int newstate,
                        int notify) ;
#else
                        Boolean newstate,
                        Boolean notify) ;
#endif /* NeedWidePrototypes */
extern Widget XmCreateToggleButtonGadget( 
                        Widget parent,
                        char *name,
                        Arg *arglist,
                        Cardinal argCount) ;

#endif /* _NO_PROTO */
/********    End Public Function Declarations    ********/


#ifdef __cplusplus
}  /* Close scope of 'extern "C"' declaration which encloses file. */
#endif

#endif /* _XmToggleG_h */
#endif
/* DON'T ADD ANYTHING AFTER THIS #endif */

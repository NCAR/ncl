/* $Id: MegaB.h,v 1.1 1997-09-08 19:29:19 dbrown Exp $ */

/*
 * Copyright 1994 John L. Cwikla
 *
 * Permission to use, copy, modify, distribute, and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appears in all copies and that
 * both that copyright notice and this permission notice appear in
 * supporting documentation, and that the name of John L. Cwikla or
 * Wolfram Research, Inc not be used in advertising or publicity
 * pertaining to distribution of the software without specific, written
 * prior permission.    John L. Cwikla and Wolfram Research, Inc make no
 * representations about the suitability of this software for any
 * purpose. It is provided "as is" without express or implied warranty.
 *
 * John L. Cwikla and Wolfram Research, Inc disclaim all warranties with
 * regard to this software, including all implied warranties of
 * merchantability and fitness, in no event shall John L. Cwikla or
 * Wolfram Research, Inc be liable for any special, indirect or
 * consequential damages or any damages whatsoever resulting from loss of
 * use, data or profits, whether in an action of contract, negligence or
 * other tortious action, arising out of or in connection with the use or
 * performance of this software.
 *
 * Author:
 *  John L. Cwikla
 *  X Programmer
 *  Wolfram Research Inc.
 *
 *  cwikla@wri.com
*/

#ifndef _MegaTB_h
#define _MegaTB_h

#include <Xm/Xm.h>

#ifndef XmIsMegaButton
#define XmIsMegaButton(w) XtIsSubclass((Widget)w, xmMegaButtonWidgetClass)
#endif 

extern WidgetClass xmMegaButtonWidgetClass;

typedef struct _XmMegaButtonClassRec *XmMegaButtonWidgetClass;
typedef struct _XmMegaButtonRec  *XmMegaButtonWidget;

typedef struct _XmMegaButtonCallbackStruct
{
	int reason;
	XEvent *event;
	int pos;
	XtPointer callbackValue;
	XmString string;
} XmMegaButtonCallbackStruct;

#define XmNfirstVisibleItem "firstVisibleItem"
#define XmCFirstVisibleItem "FirstVisibleItem"

#define XmNpixmaps "pixmaps"
#define XmCPixmaps "Pixmaps"

#define XmNcolors "colors"
#define XmCColors "Colors"

#define XmNrgbVals "rgbVals"
#define XmCRgbVals "RgbVals"

#define XmNrgbVals "rgbVals"
#define XmCRgbVals "RgbVals"

#define XmNxcb     "xcb"
#define XmCXcb 	   "Xcb"

#define XmNfast "fast"
#define XmCFast "Fast"

#define XmNsetPosition "setPosition"
#define XmCSetPosition "SetPosition"

#define XmNbuttonMode "buttonMode"
#define XmCButtonMode "ButtonMode"
#define XmRMegaButtonMode "MegaButtonMode"

#define XmNchunkSize "chunkSize"
#define XmCChunkSize "ChunkSize"

#define XmNcallbackData "callbackData"
#define XmCCallbackData "CallbackData"

enum { XmMODE_TOGGLE_BUTTON,
       XmMODE_PUSH_BUTTON,
       XmMODE_PIXMAP_AND_BUTTON,
       XmMODE_RECT_AND_BUTTON
};

extern void XmMegaButtonAddItem(Widget _w, XmString _item, int _pos, XtPointer _cbData,Pixmap _pixmap);
extern XtPointer XmMegaButtonRemoveItem(Widget _w, int _pos);
extern int XmMegaButtonGetPos(Widget _w);
extern void XmMegaButtonSetPos(Widget _w, int _pos);
extern int XmMegaButtonItemCount(Widget _w, Boolean _forgetFake);
extern XmString XmMegaButtonGetXmStringAtPos(Widget _w, int _pos);
extern void XmMegaButtonSetXmStringAtPos(Widget _w, XmString _xmstring, int _pos);

#endif /* _MegaTB_h */

/* $Id: MegaB.c,v 1.3 1997-09-17 16:41:03 boote Exp $ */
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
 * prior permission.	John L. Cwikla and Wolfram Research, Inc make no
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

#include <Xm/XmP.h>
#include <Xm/XmosP.h>

#include <Xm/BaseClassP.h>
#include <Xm/DrawP.h>
#include <Xm/MenuShellP.h>
#include <Xm/MenuUtilP.h>
#include <Xm/CascadeB.h>

#if HAVE_SOURCE
#include "Xm/TraversalI.h"
#endif /* HAVE_SOURCE */

#include "MegaBP.h"

#include <stdio.h>

#define SCROLL_TIME 100
#define MINI_SEP_HEIGHT 3

static XtWidgetClassProc classPartInit(WidgetClass _wc);
static XtProc classInit();
static XtInitProc initialize(XmMegaButtonWidget _request, XmMegaButtonWidget _new, String *_args, 
		Cardinal *_numArg); 
static XtWidgetProc destroy(XmMegaButtonWidget _mbw);
static void expose(XmMegaButtonWidget _mbw, XEvent *_event, Region _region);
static Boolean setValues( Widget _current, Widget _request, Widget _new);
static void borderHighlight(XmMegaButtonWidget _mbw);
static void borderUnhighlight(XmMegaButtonWidget _mbw);

static void getMaxWidthPos(XmMegaButtonWidget _mbw, int *_maxWidthPos, Dimension *_height);
static void drawMiniSeparator(XmMegaButtonWidget _mbw, Position _y);

static void enterWidget(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void leaveWidget(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void buttonMotion(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void up(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void down(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void buttonUp(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void buttonDown(XmMegaButtonWidget _mbw, XEvent *_event, String *_params,
		Cardinal *_numParms);
static void armAndActivate(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void focusIn(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void focusOut(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams);
static void timedScroll(XtPointer _closure, XtIntervalId *_id);
static Boolean cvtStringToMegaButtonMode(Display *_display, XrmValuePtr _args,
	Cardinal *_numArgs, XrmValuePtr _from, XrmValuePtr _to, XtPointer *_data);

static void drawShadowedItem(XmMegaButtonWidget _mbw, int _pos, Boolean _clear);
static void drawArrow(XmMegaButtonWidget _mbw, int _pos, int _direction, Boolean _sensitive);
static void clearArrow(XmMegaButtonWidget _mbw, int _pos);
static void scroll(XmMegaButtonWidget _mbw, int _direction, Boolean _addTimeout, int _delay);
static void findGoodShowPos(XmMegaButtonWidget _mbw);

static void toggleDrawProc(XmMegaButtonWidget _mbw, Position _x, Position _y, Boolean _on);
static void toggleSpaceProc(XmMegaButtonWidget _mbw, Dimension *_width, Dimension *_height);

static void pixmapDrawProc
	(XmMegaButtonWidget _mbw, Position _x, Position _y, int _pos);
static void pixmapSpaceProc
	(XmMegaButtonWidget _mbw, Dimension *_width, Dimension *_height);
static void rectDrawProc
	(XmMegaButtonWidget _mbw, Position _x, Position _y, int _pos);
static void rectSpaceProc
	(XmMegaButtonWidget _mbw, Dimension *_width, Dimension *_height);

static void recheckSizesAtPos(XmMegaButtonWidget _mbw, int _pos);

static Widget XtGetShell(Widget _w);

#define CORE(w) ((w)->core)
#define PRIM(w) ((w)->primitive)
#define LABEL(w) ((w)->label)
#define PUSH(w) ((w)->push_button)
#define MB(w) ((w)->mega_button)

#define WIDTH(w) ((w)->core.width)
#define HEIGHT(w) ((w)->core.height)

#define VIS_COUNT(w) ((w)->mega_button.visibleItemCount)
#define ICOUNT(w) ((w)->mega_button.itemCount)
#define ITEMS(w) ((w)->mega_button._items)
#define PIXMAPS(w) ((w)->mega_button._pixmaps)
#define COLORS(w) ((w)->mega_button._colors)
#define RGBVALS(w) ((w)->mega_button._rgbvals)
#define XCB(w) ((w)->mega_button._xcb)
#define EXITEMS(w) ((w)->mega_button.exitems)
#define VIS_POS(w) ((w)->mega_button.visiblePos)
#define FPOS(w) ((w)->mega_button.firstVisibleItem)
#define EL_HEIGHT(w) ((w)->mega_button.elementHeight)
#define YOFFSET(w) ((w)->mega_button.yOffset)
#define XOFFSET(w) ((w)->mega_button.xOffset)
#define HAS_ARROWS(w) ((w)->mega_button.hasArrows)
#define SET_POS(w) ((w)->mega_button.setPosition)
#define COPY_GC(w) ((w)->mega_button.copyGC)
#define TIMER(w) ((w)->mega_button.timer)
#define TDIR(w) ((w)->mega_button.timedScrollDirection)
#define SFOS(w) ((w)->mega_button.savedFillOnSelect)
#define IDELAY(w) ((w)->mega_button.initialDelay)
#define RDELAY(w) ((w)->mega_button.repeatDelay)
#define FAKE_ITEM(w) ((w)->mega_button.fakeItem)
#define CUR_SIZE(w) ((w)->mega_button.currentSize)
#define DATA(w) ((w)->mega_button.data)
#define CB_DATA(w) ((w)->mega_button.cbData)
#define MAX_STRING_POS(w) ((w)->mega_button.maxStringPos)
#define MAX_STRING_WIDTH(w) ((w)->mega_button.maxStringWidth)
#define PIX_WIDTH(w) ((w)->mega_button.pixWidth)
#define PIX_HEIGHT(w) ((w)->mega_button.pixHeight)
#define MODE(w) ((w)->mega_button.mode)
#define FILL_ON_SELECT(w) ((w)->mega_button.fillOnSelect)
#define SELECT_COLOR(w) ((w)->mega_button.selectColor)
#define SELECT_GC(w) ((w)->mega_button.selectGC)
#define CHUNK_SIZE(w) ((w)->mega_button.chunkSize)
#define VIS_W_OFF(w) ((w)->mega_button.visibleWhenOff)
#define STR_HEIGHT(w) ((w)->mega_button.stringHeight)
#define BLACK(w) ((w)->mega_button.black)

#define DEFAULT_CHUNK_SIZE 50

static char traversalTranslations[] = 
"<Btn1Motion>: BtnMotion() \n\
<Enter>: Enter() \n\
<Leave>: Leave() \n\
<Key>osfHelp:Help() \n\
<Unmap>:Unmap()\n\
<FocusOut>:FocusOut()\n\
<FocusIn>:FocusIn()\n\
<Key>osfCancel:MenuEscape()\n\
<Key>osfUp: MegaButtonTraverseUp() \n\
<Key>osfDown: MegaButtonTraverseDown() \n\
<Key>osfLeft:MenuTraverseLeft()\n\
<Key>osfRight:MenuTraverseRight()\n\
<Key>osfSelect:ArmAndActivate()\n\
<Key>osfActivate:ArmAndActivate()\n\
~s ~m ~a <Key>Return:ArmAndActivate()\n\
~s ~m ~a <Key>space:ArmAndActivate()";

#if	(XmVERSION >= 2)

#define	_XmStringCreate(s)	XmStringCopy(s)
#define _XmStringCreateExternal(font,s)	(s)

static _XmSelectColorDefault(
	Widget		w,
	int		offset,
	XrmValue	*value
)
{
	XmeGetDefaultPixel(w,XmSELECT,offset,value);
}
#endif

#define TheOffset(field) XtOffset(XmMegaButtonWidget, mega_button.field)
static XtResource resources[] =
{
	{XmNitems, XmCItems, XtRPointer, sizeof(XtPointer),
		TheOffset(exitems), XmRImmediate, (XtPointer)NULL},
	{XmNpixmaps, XmCPixmaps, XtRPointer, sizeof(XtPointer),
		TheOffset(_pixmaps), XmRImmediate, (XtPointer)NULL},
	{XmNcolors, XmCColors, XtRPointer, sizeof(XtPointer),
		TheOffset(_colors), XmRImmediate, (XtPointer)NULL},
	{XmNrgbVals, XmCRgbVals, XtRPointer, sizeof(XtPointer),
		TheOffset(_rgbvals), XmRImmediate, (XtPointer)NULL},
	{XmNxcb, XmCXcb, XtRPointer, sizeof(XtPointer),
		TheOffset(_xcb), XmRImmediate, (XtPointer)NULL},
	{XmNitemCount, XmCItemCount, XtRInt, sizeof(int),
		TheOffset(itemCount), XmRImmediate, (XtPointer)0},
	{XmNvisibleItemCount, XmCVisibleItemCount, XmRInt, sizeof(int),
		TheOffset(visibleItemCount), XtRImmediate, (XtPointer)0},
	{XmNsetPosition, XmCSetPosition, XtRInt, sizeof(int),
		TheOffset(setPosition), XmRImmediate, (XtPointer)-1},
	{XmNrepeatDelay, XmCRepeatDelay, XmRInt, sizeof(int),
		TheOffset(repeatDelay), XmRImmediate, (XtPointer)50},
	{XmNinitialDelay, XmCInitialDelay, XmRInt, sizeof(int),
		TheOffset(initialDelay), XmRImmediate, (XtPointer)250},
	{XmNbuttonMode, XmCButtonMode, XmRMegaButtonMode, sizeof(unsigned char),
		TheOffset(mode), XmRImmediate, (XtPointer)XmMODE_TOGGLE_BUTTON},
	{XmNselectColor, XmCSelectColor, XmRPixel, sizeof(Pixel),
		TheOffset(selectColor), XmRCallProc, (XtPointer)_XmSelectColorDefault},
	{XmNfillOnSelect, XmCFillOnSelect, XmRBoolean, sizeof(Boolean),
		TheOffset(fillOnSelect), XmRImmediate, (XtPointer)FALSE},
	{XmNchunkSize, XmCChunkSize, XmRInt, sizeof(int),
		TheOffset(chunkSize), XmRImmediate, (XtPointer)DEFAULT_CHUNK_SIZE},
	{XmNcallbackData, XmCCallbackData, XmRPointer, sizeof(XtPointer),
		TheOffset(cbData), XmRImmediate, (XtPointer)NULL},
	{XmNvisibleWhenOff, XmCVisibleWhenOff, XmRBoolean, sizeof(Boolean),
		TheOffset(visibleWhenOff), XtRImmediate, (XtPointer)FALSE},
};

#undef TheOffset

static XtActionsRec actions[] = 
{
	{ "Enter",		  (XtActionProc) enterWidget  },
	{ "Leave",		  (XtActionProc) leaveWidget  },
	{ "BtnMotion",		(XtActionProc) buttonMotion },
	{ "MegaButtonTraverseUp",				(XtActionProc) up },
	{ "MegaButtonTraverseDown",				(XtActionProc) down },
	{ "ArmAndActivate", (XtActionProc) armAndActivate },
	{ "BtnUp",		  (XtActionProc) buttonUp },
	{ "BtnDown",	(XtActionProc) buttonDown},
	{ "FocusIn",	 (XtActionProc) focusIn },
	{ "FocusOut",	(XtActionProc) focusOut},

	{"MenuBtnDown",	 (XtActionProc)down},
	{"MenuBtnUp",		(XtActionProc)up},
	{"PulldownBtnDown",	 (XtActionProc)down},
	{"PulldownBtnUp",		(XtActionProc)up},
	{"PopupBtnDown",		(XtActionProc)down},
	{"PopupBtnUp",		  (XtActionProc)up},
	{"MenuBarBtnDown",	  (XtActionProc)down},
	{"MenuBarBtnUp",		(XtActionProc)up},

	{"MenuTraverseUp",	  (XtActionProc)up},
	{"MenuTraverseDown",	(XtActionProc)down},
	{"MenuFocusIn",		 (XtActionProc)focusIn },
	{"MenuFocusOut",		(XtActionProc)focusOut },
	{"MenuUnmap",			(XtActionProc)leaveWidget },
	{"MenuEnter",			(XtActionProc)enterWidget},

}; /* actions */

static XmBaseClassExtRec		megaBBaseClassExtRec = 
{
	NULL,									 /* Next extension		*/
	NULLQUARK,								/* record type XmQmotif */
	XmBaseClassExtVersion,					/* version			  */
	sizeof(XmBaseClassExtRec),				/* size				 */
	XmInheritInitializePrehook,						/* initialize prehook	*/
	XmInheritSetValuesPrehook,				/* set_values prehook	*/
	XmInheritInitializePosthook,						/* initialize posthook  */
	XmInheritSetValuesPosthook,				/* set_values posthook  */
	XmInheritClass,							/* secondary class	  */
	XmInheritSecObjectCreate,				 /* creation proc		*/
	XmInheritGetSecResData,					/* getSecResData		*/
	{0},									  /* fast subclass		*/
	XmInheritGetValuesPrehook,				/* get_values prehook	*/
	XmInheritGetValuesPosthook,				/* get_values posthook  */
	(XtWidgetClassProc)NULL,				  /* classPartInitPrehook */
	(XtWidgetClassProc)NULL,				  /* classPartInitPosthook*/
	NULL,									 /* ext_resources		*/
	NULL,									 /* compiled_ext_resources*/
	0,										/* num_ext_resources	*/
	FALSE,									/* use_sub_resources	*/
	XmInheritWidgetNavigable,				 /* widgetNavigable	  */
	XmInheritFocusChange,					 /* focusChange		  */
};

XmPrimitiveClassExtRec _XmMegaBPrimClassExtRec = 
{
	NULL,
	NULLQUARK,
	XmPrimitiveClassExtVersion,
	sizeof(XmPrimitiveClassExtRec),
	XmInheritBaselineProc,				  /* widget_baseline */
	XmInheritDisplayRectProc,				/* widget_display_rect */
	(XmWidgetMarginsProc)NULL,			  /* widget_margins */
};


XmMegaButtonClassRec xmMegaButtonClassRec =
{
	{
		(WidgetClass)&xmPushButtonClassRec,			  /* superclass */
		"XmMegaButton",								 /* class_name */
		(Cardinal)sizeof(XmMegaButtonRec),		 /* widget size */
		(XtProc)classInit,							  /* class_init */
		(XtWidgetClassProc)classPartInit,			/* class_part_init */
		(XtEnum)FALSE,								/* class_inited */
		(XtInitProc)initialize,						 /* initialize */
		(XtArgsProc)NULL,								/* init_hook */
		XtInheritRealize,							/* realize */
		(XtActionList)actions,							  /* actions */
		(Cardinal)XtNumber(actions),						/* num_actions */
		(XtResourceList)resources,							/* resources */
		(Cardinal)XtNumber(resources),						/* num_resources */
		NULLQUARK,									/* xrm_class */
		TRUE,										/* compress_motion */
		(XtEnum)FALSE,								/* compress_exposur */
		TRUE,										/* compress enterleave */
		FALSE,									  /* visibility_interest */
		(XtWidgetProc)destroy,							  /* destroy */
		(XtWidgetProc)XtInheritResize,
		(XtExposeProc)expose,
		(XtSetValuesFunc)setValues,						  /* set_values */
		(XtArgsFunc)NULL,								/* set_values_hook */
		XtInheritSetValuesAlmost,					/* set_values_almost */
		(XtArgsProc)NULL,								/* get_values_hook */
		NULL, /* XtInheritAcceptFocus,						/* accept_focus */
		XtVersion,									/* version */
		(XtPointer)NULL,								/* callback_private */
		XtInheritTranslations,
		(XtGeometryHandler)NULL, /* XtInheritQueryGeometry,		  /* query_geometry */
		XtInheritDisplayAccelerator,				 /* display_accelerator */
		(XtPointer)&megaBBaseClassExtRec,										/* extension */
	},
	{ 	/* xmPrimitiveClass */
		(XtWidgetProc)borderHighlight,
		(XtWidgetProc)borderUnhighlight,
		XtInheritTranslations,
		(XtActionProc)armAndActivate,
		NULL,
		0,
		(XtPointer)&_XmMegaBPrimClassExtRec,
	},
	{	/* xmLabelClass */
		(XtWidgetProc) XmInheritSetOverrideCallback,
		NULL,
		NULL,
		NULL,
	},
	{ 	/* xmPushButtonClass */
		(XtPointer)NULL,
	},
	{
		XmInheritMegaButtonToggleSpaceProc,
		XmInheritMegaButtonToggleDrawProc,
		XmInheritMegaButtonPixmapSpaceProc,
		XmInheritMegaButtonPixmapDrawProc,
		XmInheritMegaButtonRectSpaceProc,
		XmInheritMegaButtonRectDrawProc,
	}
};

WidgetClass xmMegaButtonWidgetClass = (WidgetClass)&xmMegaButtonClassRec;


static XtWidgetClassProc classPartInit(WidgetClass _wc)
{
	XmMegaButtonWidgetClass mbwc;

	_XmFastSubclassInit(_wc, (XmPUSH_BUTTON_BIT | XmLABEL_BIT |
							XmPRIMITIVE_BIT));

	mbwc = (XmMegaButtonWidgetClass)_wc;
	if (mbwc->mega_button_class.toggleSpaceProc ==
            XmInheritMegaButtonToggleSpaceProc)
		mbwc->mega_button_class.toggleSpaceProc = toggleSpaceProc;
	if (mbwc->mega_button_class.toggleDrawProc ==
            XmInheritMegaButtonToggleDrawProc)
		mbwc->mega_button_class.toggleDrawProc = toggleDrawProc;
	if (mbwc->mega_button_class.pixmapSpaceProc ==
            XmInheritMegaButtonPixmapSpaceProc)
		mbwc->mega_button_class.pixmapSpaceProc = pixmapSpaceProc;
	if (mbwc->mega_button_class.pixmapDrawProc ==
            XmInheritMegaButtonPixmapDrawProc)
		mbwc->mega_button_class.pixmapDrawProc = pixmapDrawProc;
	if (mbwc->mega_button_class.rectSpaceProc ==
            XmInheritMegaButtonRectSpaceProc)
		mbwc->mega_button_class.rectSpaceProc = rectSpaceProc;
	if (mbwc->mega_button_class.rectDrawProc ==
            XmInheritMegaButtonRectDrawProc)
		mbwc->mega_button_class.rectDrawProc = rectDrawProc;
	return (XtWidgetClassProc)NULL;
}

static XtProc classInit()
{
	xmMegaButtonClassRec.label_class.translations = (String)XtParseTranslationTable(traversalTranslations);

	XtSetTypeConverter(XtRString, XmRMegaButtonMode, cvtStringToMegaButtonMode,
		(XtConvertArgList)NULL, 0,
			XtCacheAll, (XtDestructor)NULL);

	return (XtProc)NULL;
}

static XtInitProc initialize(XmMegaButtonWidget _request, XmMegaButtonWidget _new, String *_args, Cardinal *_numArgs)
{
	int i;
	XmString xmstring;
	XGCValues gcValues;
	Dimension width;
        XmMegaButtonClassPart *mbwcp =
                &((XmMegaButtonWidgetClass)XtClass(_new))->mega_button_class;
                
	VIS_POS(_new) = -1;

	FPOS(_new) = 0;
	if ((MODE(_new) == XmMODE_TOGGLE_BUTTON ||
             MODE(_new) == XmMODE_PIXMAP_AND_BUTTON ||
             MODE(_new) == XmMODE_RECT_AND_BUTTON) &&
            (SET_POS(_new) > (ICOUNT(_new)-1)))
		SET_POS(_new) = ICOUNT(_new) - 1;

	if (CHUNK_SIZE(_new) < 1)
		CHUNK_SIZE(_new) = DEFAULT_CHUNK_SIZE;

	if (ICOUNT(_new) == 0)
	{
		if (LABEL(_new)._label == (_XmString) XmUNSPECIFIED)
		{
			xmstring = _XmOSGetLocalizedString ((char *) NULL,  /* reserved */
						(Widget)_new,
						XmNlabelString,
						"\0");

			LABEL(_new)._label =  _XmStringCreate(xmstring);
		}
		else
		if (LABEL(_new)._label == NULL)
		{
			xmstring = _XmOSGetLocalizedString ((char *) NULL,  /* reserved */
						(Widget)_new,
						XmNlabelString,
						CORE(_new).name);

			LABEL(_new)._label =  _XmStringCreate(xmstring);
		}
		ICOUNT(_new) = 1;
		ITEMS(_new) = XtNew(_XmString);
		ITEMS(_new)[0] = LABEL(_new)._label;
		DATA(_new) = XtNew(XtPointer);
		DATA(_new)[0] = (XtPointer)NULL;
		FAKE_ITEM(_new) = TRUE;
	}
	else
	{
		ITEMS(_new) = (_XmString *)XtMalloc(sizeof(_XmString) * ICOUNT(_new));
		DATA(_new) = (XtPointer)XtCalloc(ICOUNT(_new), sizeof(XtPointer));
		if (CB_DATA(_new) != NULL)
			for(i=0;i<ICOUNT(_new);i++)
			{
				ITEMS(_new)[i] = _XmStringCreate(EXITEMS(_new)[i]);
				DATA(_new)[i] = CB_DATA(_new)[i];
			}
		else
			for(i=0;i<ICOUNT(_new);i++)
			{
				ITEMS(_new)[i] = _XmStringCreate(EXITEMS(_new)[i]);
				DATA(_new)[i] = NULL;
			}

		FAKE_ITEM(_new) = FALSE;
	}

	CUR_SIZE(_new) = ICOUNT(_new);

	getMaxWidthPos(_new, &MAX_STRING_POS(_new), &EL_HEIGHT(_new));
	_XmStringExtent(LABEL(_new).font, ITEMS(_new)[0], &width, &STR_HEIGHT(_new));


	EL_HEIGHT(_new) += 2 * PRIM(_new).shadow_thickness;
	HEIGHT(_new) = EL_HEIGHT(_new);

	LABEL(_new)._label = ITEMS(_new)[MAX_STRING_POS(_new)];
	_XmCalcLabelDimensions((Widget)_new);
	MAX_STRING_WIDTH(_new) = LABEL(_new).TextRect.width;
	WIDTH(_new) = 0;
	(*xmLabelWidgetClass->core_class.resize) ((Widget)_new);

	YOFFSET(_new) = LABEL(_new).TextRect.y;
	XOFFSET(_new) = LABEL(_new).TextRect.x;

	if (MODE(_new) == XmMODE_TOGGLE_BUTTON)
		(mbwcp->toggleSpaceProc)(_new, &WIDTH(_new), &EL_HEIGHT(_new));
	else if (MODE(_new) == XmMODE_PIXMAP_AND_BUTTON)
		(mbwcp->pixmapSpaceProc)(_new, &WIDTH(_new), &EL_HEIGHT(_new));
	else if (MODE(_new) == XmMODE_RECT_AND_BUTTON)
		(mbwcp->rectSpaceProc)(_new, &WIDTH(_new), &EL_HEIGHT(_new));

	if (VIS_COUNT(_new) == 0)
	{
		Dimension dheight;
		int vcount;

		dheight = DisplayHeight(XtDisplay(_new), DefaultScreen(XtDisplay(_new)));

		vcount = dheight/EL_HEIGHT(_new) - 4;
		if (vcount <=3)
			vcount = 3;

		VIS_COUNT(_new) = vcount;
	}

	HAS_ARROWS(_new) = ICOUNT(_new) > VIS_COUNT(_new);

	if (HAS_ARROWS(_new))
		HEIGHT(_new) =
			HEIGHT(_new) * (VIS_COUNT(_new) + 2) + (2 * MINI_SEP_HEIGHT);
	else
		HEIGHT(_new) *= ICOUNT(_new);

	gcValues.foreground = CORE(_new).background_pixel;
	COPY_GC(_new) = XtGetGC((Widget)_new, GCForeground, &gcValues);
	
	gcValues.foreground = FILL_ON_SELECT(_new) ? SELECT_COLOR(_new) : CORE(_new).background_pixel;
	gcValues.foreground = SELECT_COLOR(_new);

	SELECT_GC(_new) = XtGetGC((Widget)_new, GCForeground, &gcValues);
        
        {
                XrmValue from_black,to_black;
                
                from_black.size = sizeof(String);
                from_black.addr = (XPointer) "black";
                to_black.size = sizeof(Pixel);
                to_black.addr = (XtPointer)(&BLACK(_new));
                if (! XtConvertAndStore
                    ((Widget)_new,XtRString,&from_black,XtRPixel,&to_black))
                        printf("convert error\n");
        }
        
	TIMER(_new) = -1;

        if (MODE(_new) == XmMODE_RECT_AND_BUTTON && ! COLORS(_new)) {
                COLORS(_new) = (long *)XtMalloc(ICOUNT(_new) * sizeof(long));
                for (i=0;i<ICOUNT(_new);i++)
                        COLORS(_new)[i] = -1;
        }
	return (XtInitProc)NULL;
}

static XtWidgetProc destroy(XmMegaButtonWidget _mbw)
{
	register int i;
        int count = 0;

	XtReleaseGC((Widget)_mbw, COPY_GC(_mbw));
	XtReleaseGC((Widget)_mbw, SELECT_GC(_mbw));

	for(i=0;i<ICOUNT(_mbw);i++)
                if (ITEMS(_mbw)[i] != LABEL(_mbw)._label)
                        _XmStringFree(ITEMS(_mbw)[i]);

	XtFree((char *)ITEMS(_mbw));
	XtFree((char *)DATA(_mbw));
        if (COLORS(_mbw)) {
                for (i=0; i<ICOUNT(_mbw); i++) {
                        if (COLORS(_mbw)[i] > -1) {
                                count++;
                                XcbFreeColor(XCB(_mbw),
                                             (unsigned long)COLORS(_mbw)[i]);
                        }
                }
                XtFree((XtPointer)COLORS(_mbw));
        }
        printf("colors allocated at destroy: %d\n", count);
        
	return (XtWidgetProc)NULL;
}

static void expose(XmMegaButtonWidget _mbw, XEvent *_event, Region _region)
{
	GC gc;
	int i, first;
	Position y;
        XmMegaButtonClassPart *mbwcp =
                &((XmMegaButtonWidgetClass)XtClass(_mbw))->mega_button_class;

	if (_event && _XmIsEventUnique(_event ))
		return; /* from _XmFastExpose... */

	findGoodShowPos(_mbw);

	gc = XtIsSensitive((Widget)_mbw) ? LABEL(_mbw).normal_GC : LABEL(_mbw).insensitive_GC;

	first = FPOS(_mbw);
	y = 0;
	if (HAS_ARROWS(_mbw))
	{
		drawArrow(_mbw, 0, XmARROW_UP, (FPOS(_mbw) != 0));
		y += EL_HEIGHT(_mbw);
		drawMiniSeparator(_mbw, EL_HEIGHT(_mbw));
		y += MINI_SEP_HEIGHT;
	}
	
	y += YOFFSET(_mbw);
	LABEL(_mbw).TextRect.x = XOFFSET(_mbw);

	for(i=0;(i<VIS_COUNT(_mbw)) && ((first + i) < ICOUNT(_mbw)); i++)
	{
		LABEL(_mbw)._label = ITEMS(_mbw)[first+i];
		_XmCalcLabelDimensions((Widget)_mbw);
		
		LABEL(_mbw).TextRect.y = y;

		(*xmLabelWidgetClass->core_class.expose)
                        ((Widget)_mbw,  _event, _region);

		if (MODE(_mbw) == XmMODE_TOGGLE_BUTTON)
		{
			if ((first + i) == SET_POS(_mbw))
				(*mbwcp->toggleDrawProc)
                                        (_mbw,LABEL(_mbw).TextRect.x, y,TRUE);
			else if (VIS_W_OFF(_mbw))
				 (*mbwcp->toggleDrawProc)
                                         (_mbw,LABEL(_mbw).TextRect.x,y,FALSE);
		}
		else if (MODE(_mbw) == XmMODE_PIXMAP_AND_BUTTON)
		{
                        (*mbwcp->pixmapDrawProc)
                                (_mbw,LABEL(_mbw).TextRect.x,y,first+i);
		}
		else if (MODE(_mbw) == XmMODE_RECT_AND_BUTTON)
		{
                        (*mbwcp->rectDrawProc)
                                (_mbw,LABEL(_mbw).TextRect.x,y,first+i);
		}

		y += EL_HEIGHT(_mbw);
	}

	y -= YOFFSET(_mbw);

	if (HAS_ARROWS(_mbw))
	{
		drawMiniSeparator(_mbw, y);
		y += MINI_SEP_HEIGHT;
		drawArrow(_mbw, VIS_COUNT(_mbw)+1, XmARROW_DOWN, ((ICOUNT(_mbw))-FPOS(_mbw)) > VIS_COUNT(_mbw));
	}

	/* drawShadowedItem(_mbw, VIS_POS(_mbw), FALSE); */
}

static Boolean setValues( Widget _current, Widget _request, Widget _new)
{
	return FALSE;
}

static void getMaxWidthPos(XmMegaButtonWidget _mbw, int *_maxWidthPos, Dimension *_height)
{
	int first, i;
	Dimension maxHeight, maxWidth;
	int mwp;

	if (ICOUNT(_mbw) == 0)
	{
		*_maxWidthPos = 0;
		*_height = 1;
		return;
	}

	first = FPOS(_mbw);
	mwp = 0;
	maxHeight = 1;
	maxWidth = 1;
	for(i=0; (first + i) < ICOUNT(_mbw); i++)
	{
		LABEL(_mbw)._label = ITEMS(_mbw)[first+i];
		_XmCalcLabelDimensions((Widget)_mbw);

		if (LABEL(_mbw).TextRect.width > maxWidth)
		{
			maxWidth = LABEL(_mbw).TextRect.width;
			mwp = i;
		}

		if (LABEL(_mbw).TextRect.height > maxHeight)
			maxHeight = LABEL(_mbw).TextRect.height;
	}

	*_maxWidthPos = mwp;
	*_height = maxHeight;
}
		

static void drawShadowedItem(XmMegaButtonWidget _mbw, int _pos, Boolean _clear)
{
	Position y;
	

	if ( (_pos < 0) || (HAS_ARROWS(_mbw) && (_pos > (VIS_COUNT(_mbw)+1))) ||
		(!HAS_ARROWS(_mbw) && (_pos >= VIS_COUNT(_mbw)))
		)
		return;

	y = 0;
	if (HAS_ARROWS(_mbw))
	{
		if (_pos > 0)
			y += MINI_SEP_HEIGHT;

		if (_pos > VIS_COUNT(_mbw) )
			y += MINI_SEP_HEIGHT;
	}

	y += (_pos * EL_HEIGHT(_mbw));

	if (_clear)
	{
		_XmClearBorder(XtDisplay((Widget)_mbw), XtWindow((Widget)_mbw),
				0, y, WIDTH(_mbw), EL_HEIGHT(_mbw),
				PRIM(_mbw).shadow_thickness);
	}
	else
	{
		_XmDrawShadows(XtDisplay((Widget)_mbw), XtWindow((Widget)_mbw),
				PRIM(_mbw).top_shadow_GC,
				PRIM(_mbw).bottom_shadow_GC,
				0, y, WIDTH(_mbw), EL_HEIGHT(_mbw),
				PRIM(_mbw).shadow_thickness,
				XmSHADOW_OUT);
	}
}

static void clearArrow(XmMegaButtonWidget _mbw, int _pos)
{
	Position y = 0;
	if (HAS_ARROWS(_mbw))
	{
		if (_pos > 0)
			y += MINI_SEP_HEIGHT;

		if (_pos > VIS_COUNT(_mbw) )
			y += MINI_SEP_HEIGHT;
	}

	y += (_pos * EL_HEIGHT(_mbw));

	XClearArea(XtDisplay(_mbw), XtWindow(_mbw), 
		PRIM(_mbw).shadow_thickness, y + PRIM(_mbw).shadow_thickness,
		WIDTH(_mbw) - 2 * PRIM(_mbw).shadow_thickness,
		EL_HEIGHT(_mbw) - 2 * PRIM(_mbw).shadow_thickness, FALSE);
}

static int yToPos(XmMegaButtonWidget _mbw, Position _y)
{
	Dimension topOffset, bottomOffset;

	if ((_y >= 0) && HAS_ARROWS(_mbw))
	{
		if ((Dimension)_y <= EL_HEIGHT(_mbw))
			return 0;

		topOffset = EL_HEIGHT(_mbw) + MINI_SEP_HEIGHT;
		if ((Dimension)_y <= topOffset)
			return 0;

		bottomOffset = HEIGHT(_mbw) - EL_HEIGHT(_mbw);
		if ((Dimension)_y >= bottomOffset)
			return VIS_COUNT(_mbw)+1;

		bottomOffset -= MINI_SEP_HEIGHT;

		if ((Dimension)_y >= bottomOffset)
			return VIS_COUNT(_mbw);

		_y -= MINI_SEP_HEIGHT;
	}

	return _y/EL_HEIGHT(_mbw);
}


static void enterWidget(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	if (ICOUNT(_mbw) == 0)
		return;

	XtCallActionProc((Widget)_mbw, "PrimitiveEnter", _event, _params, *_numParams);
	if ( LABEL(_mbw).menu_type != XmWORK_AREA ) 
	{
		if (_XmGetInDragMode((Widget)_mbw))
		{
			drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
			VIS_POS(_mbw) = yToPos(_mbw, _event->xbutton.y);
			drawShadowedItem(_mbw, VIS_POS(_mbw), FALSE);

			if (HAS_ARROWS(_mbw))
			{
				if (VIS_POS(_mbw) == VIS_COUNT(_mbw)+1)
				{
					TDIR(_mbw) = XmTRAVERSE_DOWN;
					TIMER(_mbw) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)_mbw),
						(unsigned long)IDELAY(_mbw),
						timedScroll, (XtPointer)_mbw);
				}
				else
				if (VIS_POS(_mbw) == 0)
				{
					TDIR(_mbw) = XmTRAVERSE_UP;
					TIMER(_mbw) = XtAppAddTimeOut(XtWidgetToApplicationContext((Widget)_mbw),
						(unsigned long)IDELAY(_mbw),
						timedScroll, (XtPointer)_mbw);
				}
			}
		}
	} 
} 

static void leaveWidget(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	if (ICOUNT(_mbw) == 0)
		return;

	XtCallActionProc((Widget)_mbw, "PrimitiveLeave", _event, _params, *_numParams);
	if ( LABEL(_mbw).menu_type != XmWORK_AREA ) 
	{
/* 		if ( _XmGetInDragMode((Widget)_mbw) )  */

		/* drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE); */

		if (TIMER(_mbw) != -1)
		{
			XtRemoveTimeOut(TIMER(_mbw));
			TIMER(_mbw) = -1;
		}
	} 
	drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
} 

static void buttonMotion(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	if (ICOUNT(_mbw) == 0)
		return;


	if ( LABEL(_mbw).menu_type != XmWORK_AREA )
	{
		int pos = yToPos(_mbw, _event->xmotion.y);
		if (pos != VIS_POS(_mbw))
		{
			if (TIMER(_mbw) != -1)
			{
				XtRemoveTimeOut(TIMER(_mbw));
				TIMER(_mbw) = -1;
			}
			drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
			drawShadowedItem(_mbw, pos, FALSE);
			VIS_POS(_mbw) = pos;

			if (HAS_ARROWS(_mbw))
			{
				if (VIS_POS(_mbw) == VIS_COUNT(_mbw)+1)
					scroll(_mbw, XmTRAVERSE_DOWN, TRUE, IDELAY(_mbw));
				else
				if (VIS_POS(_mbw) == 0)
					scroll(_mbw, XmTRAVERSE_UP, TRUE, IDELAY(_mbw));
			}
			_XmSetInDragMode((Widget)_mbw, TRUE);
		}
	}
} 

static void up(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	int vpos;

	if (ICOUNT(_mbw) == 0)
		return;

	if (LABEL(_mbw).menu_type != XmWORK_AREA )
	{
		if (!_XmGetInDragMode((Widget)_mbw))
		{
			vpos = VIS_POS(_mbw);
			vpos--;

			if ((vpos == -1) && (FPOS(_mbw) == 0))
			{
				CompositeWidget parent = (CompositeWidget)XtParent(_mbw);
				if (parent->composite.num_children > 1)
				{
					XtCallActionProc((Widget)_mbw, "PrimitiveTraverseUp", _event, _params, *_numParams);
					return;
				}
			}

			if (HAS_ARROWS(_mbw) && (vpos <= 0))
			{
				if (vpos == 0)
				{
					drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
					drawShadowedItem(_mbw, vpos, FALSE);
				}
				scroll(_mbw, XmTRAVERSE_UP, FALSE, 0);
				VIS_POS(_mbw) = 0;
			}
			else
			{
				if (!HAS_ARROWS(_mbw) && (vpos == -1))
					vpos = ICOUNT(_mbw)-1;

				if (vpos != VIS_POS(_mbw))
				{
					drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
					drawShadowedItem(_mbw, vpos, FALSE);
					VIS_POS(_mbw) = vpos;
				}
			}

			_XmRecordEvent(_event);
		}
	}

}

static void down(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	int vpos;

	if (ICOUNT(_mbw) == 0)
		return;

	if (LABEL(_mbw).menu_type != XmWORK_AREA )
	{
		if (!_XmGetInDragMode((Widget)_mbw))
		{
			vpos = VIS_POS(_mbw);
			vpos++;

			_XmRecordEvent(_event);

			if (HAS_ARROWS(_mbw) && (vpos >= (VIS_COUNT(_mbw) + 1)))
			{
				if (VIS_POS(_mbw) != (VIS_COUNT(_mbw)+1))
				{
					drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
					drawShadowedItem(_mbw, vpos, FALSE);
				}
				else
				if (FPOS(_mbw) >= (ICOUNT(_mbw)-VIS_COUNT(_mbw)))
				{
					CompositeWidget parent = (CompositeWidget)XtParent(_mbw);
					if (parent->composite.num_children > 1)
					{
						XtCallActionProc((Widget)_mbw, "PrimitiveTraverseDown", _event, _params, *_numParams);
						return;
					}
				}

				scroll(_mbw, XmTRAVERSE_DOWN, FALSE, 0);
				VIS_POS(_mbw) = VIS_COUNT(_mbw) + 1;
			}
			else
			{
				if (!HAS_ARROWS(_mbw))
				{
					if (vpos == ICOUNT(_mbw))
					{
						CompositeWidget parent = (CompositeWidget)XtParent(_mbw);
						if (parent->composite.num_children > 1)
						{
							XtCallActionProc((Widget)_mbw, "PrimitiveTraverseDown", _event, _params, *_numParams);
							return;
						}
						else
							vpos = 0;
					}
				}

				drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
				drawShadowedItem(_mbw, vpos, FALSE);
				VIS_POS(_mbw) = vpos;
			}
		}
	}

}

static void buttonUp(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	XmMegaButtonCallbackStruct mtbcs;
	Widget parent, shell;
	Boolean validButton;
	Boolean poppedUp;
	Boolean isMenuPane;

	if (TIMER(_mbw) != -1)
	{
		XtRemoveTimeOut(TIMER(_mbw));
		TIMER(_mbw) = -1;
	}

	if (ICOUNT(_mbw) == 0)
		return;

	isMenuPane = (LABEL(_mbw).menu_type == XmMENU_PULLDOWN) || (LABEL(_mbw).menu_type == XmMENU_POPUP);
	shell = XtGetShell((Widget)_mbw);
	parent = XtParent(_mbw);

	if (_event && (_event->type == ButtonRelease))
	{
		(* xmLabelClassRec.label_class.menuProcs) (XmMENU_BUTTON, parent, NULL, _event, &validButton);
		if (!validButton)
			return;

		VIS_POS(_mbw) = yToPos(_mbw, _event->xbutton.y);
	}

	if (HAS_ARROWS(_mbw) && !_XmGetInDragMode((Widget)_mbw))
	{
		if (VIS_POS(_mbw) == 0)
		{
			_XmRecordEvent(_event);
			return;
		}
		else
		if (VIS_POS(_mbw) == (VIS_COUNT(_mbw)+1))
		{
			_XmRecordEvent(_event);
			return;
		}
	}

	if (isMenuPane && !XmIsMenuShell(shell))
		(* xmLabelClassRec.label_class.menuProcs) (XmMENU_POPDOWN, (Widget) _mbw, NULL, _event, &poppedUp);
	else
		(* xmLabelClassRec.label_class.menuProcs) (XmMENU_BUTTON_POPDOWN, (Widget) _mbw , NULL, 
			_event, &poppedUp);

	_XmRecordEvent(_event);

	if (poppedUp)
		return;

	mtbcs.reason = XmCR_ACTIVATE;
	mtbcs.event = _event;
	if (HAS_ARROWS(_mbw))
	{
		if ((VIS_POS(_mbw) == 0) || (VIS_POS(_mbw) == (VIS_COUNT(_mbw)+1)))
			return;

		mtbcs.pos = FPOS(_mbw) + VIS_POS(_mbw) - 1;
		
	}
	else
		mtbcs.pos = VIS_POS(_mbw);
	mtbcs.callbackValue = DATA(_mbw)[mtbcs.pos];
	mtbcs.string = _XmStringCreateExternal(LABEL(_mbw).font, ITEMS(_mbw)[mtbcs.pos]);

	SET_POS(_mbw) = mtbcs.pos;

#if 0
	if (XmIsRowColumn(parent))
		(* xmLabelClassRec.label_class.menuProcs) (XmMENU_CALLBACK, parent, FALSE, _mbw, (XtPointer)&mtbcs);
	else
#endif
		XtCallCallbackList((Widget)_mbw, PUSH(_mbw).activate_callback, &mtbcs);

	_XmSetInDragMode((Widget)_mbw, False);
}

static void buttonDown(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, 
		Cardinal *_numParms)
{
	ShellWidget popupShell;
	int pos;
        Boolean validButton;
	Widget child;

	if (ICOUNT(_mbw) == 0)
		return;

	XAllowEvents(XtDisplay(_mbw), SyncPointer, CurrentTime);

	if (_event && (_event->type == ButtonPress))
	{
		(* xmLabelClassRec.label_class.menuProcs) (XmMENU_BUTTON, XtParent(_mbw), NULL, _event, &validButton);
		if (!validButton)
			return;

		pos = yToPos(_mbw, _event->xbutton.y);
	}

	if (HAS_ARROWS(_mbw) && !_XmGetInDragMode((Widget)_mbw))
	{
		if (pos == 0)
		{
			drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
			VIS_POS(_mbw) = pos;
			drawShadowedItem(_mbw, VIS_POS(_mbw), FALSE);
			_XmRecordEvent(_event);
			scroll(_mbw, XmTRAVERSE_UP, TRUE, IDELAY(_mbw));
			return;
		}
		else
		if (pos == (VIS_COUNT(_mbw)+1))
		{
			drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
			VIS_POS(_mbw) = pos;
			drawShadowedItem(_mbw, VIS_POS(_mbw), FALSE);
			_XmRecordEvent(_event);
			scroll(_mbw, XmTRAVERSE_DOWN, TRUE, IDELAY(_mbw));
			return;
		}
	}

	VIS_POS(_mbw) = pos;

	_XmSetInDragMode((Widget)_mbw, TRUE);

	popupShell = (ShellWidget)_XmGetRC_PopupPosted(XtParent(_mbw));
	if  (popupShell)
	{
		if (popupShell->shell.popped_up)
			(* xmLabelClassRec.label_class.menuProcs)(XmMENU_SHELL_POPDOWN, (Widget)popupShell, NULL, _event, NULL);

		child = ((XmManagerWidget)XtParent(_mbw))->manager.active_child;
		if (child && (XmIsCascadeButton(child) || XmIsCascadeButtonGadget(child)))
			XmCascadeButtonHighlight (child, FALSE);
	}

	XmProcessTraversal( (Widget)_mbw, XmTRAVERSE_CURRENT);

	_XmSetInDragMode((Widget)_mbw, FALSE);

	_XmRecordEvent(_event);
}

static void armAndActivate(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	XmMegaButtonCallbackStruct mtbcs;

	if (ICOUNT(_mbw) == 0)
		return;

	mtbcs.reason = XmCR_ACTIVATE;
	mtbcs.event = _event;
	if (HAS_ARROWS(_mbw))
	{
		if ((VIS_POS(_mbw) == 0) || (VIS_POS(_mbw) == (VIS_COUNT(_mbw)+1)))
			return;

		mtbcs.pos = FPOS(_mbw) + VIS_POS(_mbw) - 1;

	}
	else
		mtbcs.pos = VIS_POS(_mbw);
	mtbcs.callbackValue = DATA(_mbw)[mtbcs.pos];
	mtbcs.string = _XmStringCreateExternal(LABEL(_mbw).font, ITEMS(_mbw)[mtbcs.pos]);

	SET_POS(_mbw) = mtbcs.pos;

	XFlush(XtDisplay(_mbw));

	XtCallCallbackList((Widget)_mbw, PUSH(_mbw).activate_callback, &mtbcs);
}

static Widget XtGetShell(Widget _w)
{
	Widget temp;

	temp = _w;
	while(temp && !XtIsSubclass(temp, shellWidgetClass))
		temp = XtParent(temp);

	return temp;
}

#if !HAVE_SOURCE

struct _XmFocusDataRec
{
	Widget not_used;
	Widget not_used_2;
	Widget old_focus_item;
};

#endif /* HAVE_SOURCE */

/*
** This is a tad hackish.  The general idea is that there is no way to tell which
** directory the focusIn event came from if we are in a menu.  So get the
** focusData which has the last widget to have focus.  Use this as an index
** into the RowCol's children, and go from there.
*/

static void focusIn(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	int labelCount;
	Widget oldFocus;
	CompositeWidget parent;
	XmFocusData focusData;
	WidgetList children;
	int myI, oldI, i;

	VIS_POS(_mbw) = 0;
	if (ICOUNT(_mbw) == 0)
		return;

	focusData = _XmGetFocusData((Widget)_mbw);

	if (focusData && focusData->old_focus_item)
	{
		oldFocus = focusData->old_focus_item;
		parent = (CompositeWidget)XtParent(_mbw);

		children = parent->composite.children;
		myI = -1;
		oldI = -1;
		labelCount = 0;
		for(i=0;(i<parent->composite.num_children);i++)
		{
			if (children[i] == (Widget)_mbw)
				myI = i;
			else
			if (children[i] == oldFocus)
				oldI = i;
			else
			if (XmIsLabel(children[i]))
				labelCount++;

			if ((myI != -1) && (oldI != -1))
				break;
		}

		if ( (oldI != -1) && ((myI < oldI) ||
			 ((myI > oldI) && labelCount)))
			VIS_POS(_mbw) = (HAS_ARROWS(_mbw) ? VIS_COUNT(_mbw) + 1 : ICOUNT(_mbw)-1);
	}



	drawShadowedItem(_mbw, VIS_POS(_mbw), FALSE);
}

static void focusOut(XmMegaButtonWidget _mbw, XEvent *_event, String *_params, Cardinal *_numParams)
{
	if (ICOUNT(_mbw) == 0)
		return;

	if (TIMER(_mbw) != -1)
	{
		XtRemoveTimeOut(TIMER(_mbw));
		TIMER(_mbw) = -1;
	}

	drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
	VIS_POS(_mbw) = 0;
}

static void borderHighlight(XmMegaButtonWidget _mbw)
{
	if (ICOUNT(_mbw) == 0)
		return;


	if ((LABEL(_mbw).menu_type == XmMENU_PULLDOWN) || (LABEL(_mbw).menu_type == XmMENU_POPUP))
	{
		drawShadowedItem(_mbw, VIS_POS(_mbw), FALSE);
	}
}

static void borderUnhighlight(XmMegaButtonWidget _mbw)
{
	if (ICOUNT(_mbw) == 0)
		return;


	if ((LABEL(_mbw).menu_type == XmMENU_PULLDOWN) || (LABEL(_mbw).menu_type == XmMENU_POPUP))
		drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE);
}

static void drawMiniSeparator(XmMegaButtonWidget _mbw, Position _y)
{
	int i, firstHalf, secondHalf;

	firstHalf = MINI_SEP_HEIGHT/2;
	secondHalf = firstHalf;

	if (MINI_SEP_HEIGHT % 2)
		secondHalf++;

	for(i=secondHalf;i<MINI_SEP_HEIGHT;i++)
		XDrawLine(XtDisplay(_mbw), XtWindow(_mbw), PRIM(_mbw).top_shadow_GC,
			0, _y+i, WIDTH(_mbw), _y+i);

	for(i=0;i<firstHalf;i++)
		XDrawLine(XtDisplay(_mbw), XtWindow(_mbw), PRIM(_mbw).bottom_shadow_GC,
			0, _y+i, WIDTH(_mbw), _y+i);
}


static void drawArrow(XmMegaButtonWidget _mbw, int _pos, int _direction, Boolean _sensitive)
{
	Dimension arrowWidth;
	Position y = 0;

	if (HAS_ARROWS(_mbw))
	{
		if (_pos > 0)
			y += MINI_SEP_HEIGHT;

		if (_pos > VIS_COUNT(_mbw) )
			y += MINI_SEP_HEIGHT;
	}

	y += (_pos * EL_HEIGHT(_mbw));


	arrowWidth = WIDTH(_mbw)/5;

	_XmDrawArrow(XtDisplay((Widget)(_mbw)),
			XtWindow ((Widget)(_mbw)),
			_sensitive ? PRIM(_mbw).top_shadow_GC : PRIM(_mbw).bottom_shadow_GC,
			_sensitive ? PRIM(_mbw).bottom_shadow_GC : PRIM(_mbw).top_shadow_GC,
			_sensitive ? COPY_GC(_mbw) : LABEL(_mbw).insensitive_GC,
			WIDTH(_mbw)/2 - arrowWidth/2,
			y + PRIM(_mbw).shadow_thickness,
			arrowWidth,
			EL_HEIGHT(_mbw) - 2 * (PRIM(_mbw).shadow_thickness),
			PRIM(_mbw).shadow_thickness, _direction);
}

static void timedScroll(XtPointer _closure, XtIntervalId *_id)
{
	XmMegaButtonWidget mbw = (XmMegaButtonWidget)_closure;

	scroll(mbw, TDIR(mbw), TRUE, RDELAY(mbw));
}

static void scroll(XmMegaButtonWidget _mbw, int _direction, Boolean _addTimeout, int _delay)
{
	Position sy, dy;
        XmMegaButtonClassPart *mbwcp =
                &((XmMegaButtonWidgetClass)XtClass(_mbw))->mega_button_class;

	TIMER(_mbw) = -1;

	if (_direction == XmTRAVERSE_DOWN)
	{
		if ((FPOS(_mbw) + VIS_COUNT(_mbw)) >= ICOUNT(_mbw))
			return;

		/* drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE); */

		sy = 2 * EL_HEIGHT(_mbw) + MINI_SEP_HEIGHT;
		dy = sy - EL_HEIGHT(_mbw);

		XCopyArea(XtDisplay(_mbw), XtWindow(_mbw), XtWindow(_mbw),
			COPY_GC(_mbw), 
			0, sy, WIDTH(_mbw), EL_HEIGHT(_mbw) * (VIS_COUNT(_mbw)-1),
			0, dy);

		XClearArea(XtDisplay(_mbw), XtWindow(_mbw),
			PRIM(_mbw).shadow_thickness, dy +
                           EL_HEIGHT(_mbw) * (VIS_COUNT(_mbw)-1),
			WIDTH(_mbw)-2*PRIM(_mbw).shadow_thickness,
			EL_HEIGHT(_mbw), FALSE);

		FPOS(_mbw)++;

		LABEL(_mbw)._label = ITEMS(_mbw)[FPOS(_mbw)+VIS_COUNT(_mbw)-1];
		_XmCalcLabelDimensions((Widget)_mbw);

		LABEL(_mbw).TextRect.y = YOFFSET(_mbw) +
                        (VIS_COUNT(_mbw)*EL_HEIGHT(_mbw)) + MINI_SEP_HEIGHT;
		LABEL(_mbw).TextRect.x = XOFFSET(_mbw);


		(*xmLabelWidgetClass->core_class.expose)
			((Widget)_mbw,  NULL, (Region)NULL);


		if (MODE(_mbw) == XmMODE_TOGGLE_BUTTON)
		{
			if ((FPOS(_mbw)+VIS_COUNT(_mbw)-1) == SET_POS(_mbw))
				(*mbwcp->toggleDrawProc)
                                        (_mbw,LABEL(_mbw).TextRect.x,
                                         LABEL(_mbw).TextRect.y,TRUE);
			else if (VIS_W_OFF(_mbw)) 
				(*mbwcp->toggleDrawProc)
					(_mbw, LABEL(_mbw).TextRect.x,
                                         LABEL(_mbw).TextRect.y,FALSE);
		}
		else if (MODE(_mbw) == XmMODE_PIXMAP_AND_BUTTON)
		{
                        (*mbwcp->pixmapDrawProc)
                                (_mbw,LABEL(_mbw).TextRect.x,
                                 LABEL(_mbw).TextRect.y,
                                 (FPOS(_mbw)+VIS_COUNT(_mbw)-1));
		}
		else if (MODE(_mbw) == XmMODE_RECT_AND_BUTTON)
		{
                        int index = FPOS(_mbw)-1;
                        if (COLORS(_mbw)[index] > -1) {
                                XcbFreeColor
                                        (XCB(_mbw),
                                         (unsigned long)COLORS(_mbw)[index]);
                                COLORS(_mbw)[index] = -1;
                        }
                        (*mbwcp->rectDrawProc)
                                (_mbw,LABEL(_mbw).TextRect.x,
                                 LABEL(_mbw).TextRect.y,
                                 (FPOS(_mbw)+VIS_COUNT(_mbw)-1));
		}

		if (_addTimeout && ((FPOS(_mbw)
                                     + VIS_COUNT(_mbw)) < ICOUNT(_mbw)))
		{
			TDIR(_mbw) = XmTRAVERSE_DOWN;
			TIMER(_mbw) = XtAppAddTimeOut
                                (XtWidgetToApplicationContext((Widget)_mbw),
                                 (unsigned long) _delay,
                                 timedScroll, (XtPointer)_mbw);
		}
			
	}
	else if (_direction == XmTRAVERSE_UP)
	{
		if (FPOS(_mbw) == 0)
			return;

		/* drawShadowedItem(_mbw, VIS_POS(_mbw), TRUE); */

		sy = EL_HEIGHT(_mbw) + MINI_SEP_HEIGHT;
		dy = sy + EL_HEIGHT(_mbw);

		XCopyArea(XtDisplay(_mbw), XtWindow(_mbw), XtWindow(_mbw),
                          COPY_GC(_mbw),
                          0, sy, WIDTH(_mbw),
                          EL_HEIGHT(_mbw) * (VIS_COUNT(_mbw)-1),
                          0, dy);

		XClearArea(XtDisplay(_mbw), XtWindow(_mbw),
                           PRIM(_mbw).shadow_thickness,
                           EL_HEIGHT(_mbw) + MINI_SEP_HEIGHT,
                           WIDTH(_mbw)-2*PRIM(_mbw).shadow_thickness,
                           EL_HEIGHT(_mbw), FALSE);

		FPOS(_mbw)--;

		LABEL(_mbw)._label = ITEMS(_mbw)[FPOS(_mbw)];
		_XmCalcLabelDimensions((Widget)_mbw);

		LABEL(_mbw).TextRect.y = YOFFSET(_mbw)
                        + EL_HEIGHT(_mbw) + MINI_SEP_HEIGHT;
		LABEL(_mbw).TextRect.x = XOFFSET(_mbw);


		(*xmLabelWidgetClass->core_class.expose)
                        ((Widget)_mbw,  NULL, (Region)NULL);

		if (MODE(_mbw) == XmMODE_TOGGLE_BUTTON)
		{
			if (FPOS(_mbw) == SET_POS(_mbw))
				(*mbwcp->toggleDrawProc)
                                        (_mbw,LABEL(_mbw).TextRect.x,
                                         LABEL(_mbw).TextRect.y, TRUE);
			else
			if (VIS_W_OFF(_mbw))
				(*mbwcp->toggleDrawProc)
					(_mbw,LABEL(_mbw).TextRect.x,
                                         LABEL(_mbw).TextRect.y, FALSE);
		}
		else if (MODE(_mbw) == XmMODE_PIXMAP_AND_BUTTON)
		{
                        (*mbwcp->pixmapDrawProc)
                                (_mbw,LABEL(_mbw).TextRect.x,
                                 LABEL(_mbw).TextRect.y,FPOS(_mbw));
		}
		else if (MODE(_mbw) == XmMODE_RECT_AND_BUTTON)
		{
                        int index = FPOS(_mbw)+VIS_COUNT(_mbw);
                        if (COLORS(_mbw)[index] > -1) {
                                XcbFreeColor
                                        (XCB(_mbw),
                                         (unsigned long)COLORS(_mbw)[index]);
                                COLORS(_mbw)[index] = -1;
                        }
                        (*mbwcp->rectDrawProc)
                                (_mbw,LABEL(_mbw).TextRect.x,
                                 LABEL(_mbw).TextRect.y,FPOS(_mbw));
		}


		if (_addTimeout && (FPOS(_mbw) != 0))
		{
			TDIR(_mbw) = XmTRAVERSE_UP;
			TIMER(_mbw) = XtAppAddTimeOut
                                (XtWidgetToApplicationContext((Widget)_mbw),
				(unsigned long) _delay, 
				timedScroll, (XtPointer)_mbw);
		}
	}

	if (_direction == XmTRAVERSE_DOWN)
	{
		if (FPOS(_mbw) != 0)
		{
			if (((ICOUNT(_mbw))-FPOS(_mbw)) <= VIS_COUNT(_mbw))
			{
				clearArrow(_mbw, VIS_COUNT(_mbw)+1);
				drawArrow(_mbw,VIS_COUNT(_mbw)+1,
                                          XmARROW_DOWN, FALSE);
			}
			drawArrow(_mbw, 0, XmARROW_UP, TRUE);
		}

	}
	else
	{
		if (FPOS(_mbw) == 0)
		{
			clearArrow(_mbw, 0);
			drawArrow(_mbw, 0, XmARROW_UP, FALSE);
		}
		drawArrow(_mbw, VIS_COUNT(_mbw)+1, XmARROW_DOWN, TRUE);
	}
}

static void toggleDrawProc(XmMegaButtonWidget _mbw, Position _x, Position _y, Boolean _on)
{
	int ecalc;

	ecalc = STR_HEIGHT(_mbw) - 2 * PRIM(_mbw).shadow_thickness;

	if (ecalc < 5)
		ecalc = 5;

	_y += EL_HEIGHT(_mbw)/2 - ecalc/2 - PRIM(_mbw).shadow_thickness;

	_x -= (ecalc + LABEL(_mbw).margin_width);

	_XmDrawDiamond(XtDisplay((Widget)_mbw), XtWindow((Widget)_mbw),
			_on ? PRIM(_mbw).bottom_shadow_GC : PRIM(_mbw).top_shadow_GC,
			_on ? PRIM(_mbw).top_shadow_GC : PRIM(_mbw).bottom_shadow_GC,
			_on ? SELECT_GC(_mbw) : COPY_GC(_mbw),
			_x, _y,
			ecalc, ecalc,
			PRIM(_mbw).shadow_thickness,
			TRUE);
}

static void toggleSpaceProc(XmMegaButtonWidget _mbw, Dimension *_width, Dimension *_height)
{
	int ecalc = STR_HEIGHT(_mbw) - 2 * PRIM(_mbw).shadow_thickness;
	if (ecalc < 5)
		ecalc = 5;

	XOFFSET(_mbw) += ecalc + LABEL(_mbw).margin_width;
	*_width += ecalc + LABEL(_mbw).margin_width;
}

static void pixmapDrawProc
	(XmMegaButtonWidget _mbw, Position _x, Position _y, int _pos)
{
        
        if (PIXMAPS(_mbw)[_pos] != XmUNSPECIFIED_PIXMAP) {
                _y += EL_HEIGHT(_mbw)/2 -
                        PIX_HEIGHT(_mbw)/2 - PRIM(_mbw).shadow_thickness;

                _x -= (PIX_WIDTH(_mbw) + LABEL(_mbw).margin_width);

                XCopyArea (XtDisplay(_mbw), PIXMAPS(_mbw)[_pos],XtWindow(_mbw),
                           COPY_GC(_mbw), 0, 0,
                           PIX_WIDTH(_mbw),PIX_HEIGHT(_mbw),
                           _x,_y);
        }
        return;
        
}

static void pixmapSpaceProc
	(XmMegaButtonWidget _mbw, Dimension *_width, Dimension *_height)
{
        Window root;
        int  x, y;
        unsigned int  w = 0 , h = 0, border, d;

            /* for now assume all pixmaps are the same size */

        PIX_WIDTH(_mbw) = 0;
        PIX_HEIGHT(_mbw) = 0;
        if (PIXMAPS(_mbw)[0] != XmUNSPECIFIED_PIXMAP) {
                XGetGeometry (XtDisplay(_mbw),
                              PIXMAPS(_mbw)[0],
                              &root,	/* returned root window */
                              &x, &y,	/* returned x, y of pixmap */
                              &w, &h,	/* returned width, height of pixmap */
                              &border,	/* returned border width */
                              &d);      /* returned depth */

                PIX_WIDTH(_mbw) = w;
                PIX_HEIGHT(_mbw) = h;
                XOFFSET(_mbw) += PIX_WIDTH(_mbw) + LABEL(_mbw).margin_width;
                *_width += PIX_WIDTH(_mbw) + LABEL(_mbw).margin_width;
                *_height = *_height > PIX_HEIGHT(_mbw) ?
                        *_height : PIX_HEIGHT(_mbw);
        }
        return;
        
}

static void rectDrawProc
	(XmMegaButtonWidget _mbw, Position _x, Position _y, int _pos)
{

        _y += EL_HEIGHT(_mbw)/2 -
                STR_HEIGHT(_mbw)/2 - PRIM(_mbw).shadow_thickness;
        _x -= (STR_HEIGHT(_mbw) + LABEL(_mbw).margin_width);
        if (COLORS(_mbw)[_pos] >= 0) {
                XSetForeground(XtDisplay(_mbw),COPY_GC(_mbw),
                               (unsigned long)COLORS(_mbw)[_pos]);
                XFillRectangle(XtDisplay(_mbw),
                               XtWindow(_mbw),COPY_GC(_mbw),_x,_y,
                               STR_HEIGHT(_mbw)-1,STR_HEIGHT(_mbw)-1);
        }
        else if (RGBVALS(_mbw) && !(RGBVALS(_mbw)[_pos].r < 0.0)) {
                XColor  color;
                    /* RgbVals does not have an entry for transparent */
                color.red = (int) (RGBVALS(_mbw)[_pos].r * 65535);
                color.green = (int) (RGBVALS(_mbw)[_pos].g * 65535);
                color.blue = (int) (RGBVALS(_mbw)[_pos].b * 65535);
                XcbAllocROColor(XCB(_mbw),&color);
                COLORS(_mbw)[_pos] = (long) color.pixel;
                XSetForeground(XtDisplay(_mbw),COPY_GC(_mbw),
                               (unsigned long)COLORS(_mbw)[_pos]);
                XFillRectangle(XtDisplay(_mbw),
                               XtWindow(_mbw),COPY_GC(_mbw),_x,_y,
                               STR_HEIGHT(_mbw)-1,STR_HEIGHT(_mbw)-1);
        }
        if (SET_POS(_mbw) == _pos)
                XDrawRectangle(XtDisplay(_mbw),
                               XtWindow(_mbw),SELECT_GC(_mbw),_x,_y,
                               STR_HEIGHT(_mbw)-1,STR_HEIGHT(_mbw)-1);
        else {
                XSetForeground(XtDisplay(_mbw),COPY_GC(_mbw),BLACK(_mbw));
                XDrawRectangle(XtDisplay(_mbw),
                               XtWindow(_mbw),COPY_GC(_mbw),_x,_y,
                               STR_HEIGHT(_mbw)-1,STR_HEIGHT(_mbw)-1);
        }
        XSetForeground(XtDisplay(_mbw),
                       COPY_GC(_mbw),CORE(_mbw).background_pixel);
        
        return;
        
}
static void rectSpaceProc
	(XmMegaButtonWidget _mbw, Dimension *_width, Dimension *_height)
{
	int ecalc = STR_HEIGHT(_mbw);
	if (ecalc < 5)
		ecalc = 5;

	XOFFSET(_mbw) += ecalc + LABEL(_mbw).margin_width;
	*_width += ecalc + LABEL(_mbw).margin_width;
}

static void findGoodShowPos(XmMegaButtonWidget _mbw)
{
	int topDiff, bottomDiff;

	if (MODE(_mbw) != XmMODE_TOGGLE_BUTTON)
	{
		FPOS(_mbw) = 0;
		return;
	}

	VIS_POS(_mbw) = 0;
	if (!HAS_ARROWS(_mbw) || (SET_POS(_mbw) == -1))
		FPOS(_mbw) = 0;
	else
	{
		topDiff = SET_POS(_mbw) - VIS_COUNT(_mbw)/2;
		if (topDiff < 0)
			FPOS(_mbw) = 0;
		else
		{
			bottomDiff = (ICOUNT(_mbw)-1) - SET_POS(_mbw);

			if (bottomDiff < VIS_COUNT(_mbw))
				FPOS(_mbw) = (ICOUNT(_mbw))-VIS_COUNT(_mbw);
			else
				FPOS(_mbw) = topDiff;
		}
	}
}

static void toLower(char *_str1, char *_str2, int _length)
{
	int i;
	char *ptr;

	for(ptr=_str1,i=0;(ptr!=NULL) && (i<_length);ptr++,i++)
		*(_str2+i) = tolower(*ptr);
}

static Boolean cvtStringToMegaButtonMode(Display *_display, XrmValuePtr _args,
	Cardinal *_numArgs, XrmValuePtr _from, XrmValuePtr _to, XtPointer *_data)
{
	char *lower;
	static unsigned char mode;
	Boolean badConversion = FALSE;

	if (*_numArgs != 0)
	{
		XtAppWarningMsg(XtDisplayToApplicationContext(_display), "cvtStringToMegaButtonMode", "wrongParamaters",
		"ResourceError",
		"cvtStringToMegaButtonMode needs no arguments.",
		(String *)NULL, (Cardinal *)NULL);
	}

	lower = XtNewString(_from->addr);
	toLower(_from->addr, lower, strlen(_from->addr));

	mode = XmMODE_TOGGLE_BUTTON;

	if (!strncmp(lower, "mode_toggle_button", 18))
		mode = XmMODE_TOGGLE_BUTTON;
	else if (!strncmp(lower, "mode_push_button", 16))
		mode = XmMODE_PUSH_BUTTON;
	else if (!strncmp(lower, "mode_pixmap_and_button", 22))
		mode = XmMODE_PIXMAP_AND_BUTTON;
	else if (!strncmp(lower, "mode_rect_and_button", 20))
		mode = XmMODE_PIXMAP_AND_BUTTON;
	else if (!strncmp(lower, "toggle_button", 13))
		mode = XmMODE_TOGGLE_BUTTON;
	else if (!strncmp(lower, "push_button", 11))
		mode = XmMODE_PUSH_BUTTON;
	else if (!strncmp(lower, "pixmap_and_button", 17))
		mode = XmMODE_PIXMAP_AND_BUTTON;
	else if (!strncmp(lower, "rect_and_button", 15))
		mode = XmMODE_PIXMAP_AND_BUTTON;
	else
		badConversion = TRUE;

	XtFree(lower);

	if (badConversion)
		XtDisplayStringConversionWarning(_display, _from->addr, XmRMegaButtonMode);
	else
	{
		if (_to->addr == NULL)
			_to->addr = (XtPointer)&mode;
		else
		if (_to->size < sizeof(unsigned char))
			badConversion = TRUE;
		else
			*(unsigned char *)_to->addr = mode;
			_to->size = sizeof(unsigned char);
	}

	return !badConversion;
}


void XmMegaButtonAddItem(Widget _w, XmString _item, int _pos, XtPointer _cbData,Pixmap _pixmap)
{
	XmMegaButtonWidget mbw;

	if (!XmIsMegaButton(_w))
		return;

	mbw = (XmMegaButtonWidget)_w;

	if (FAKE_ITEM(mbw))
	{
		ICOUNT(mbw)--;
		FAKE_ITEM(mbw) = FALSE;
		WIDTH(mbw) = 0;
		MAX_STRING_WIDTH(mbw) = 1;
	}

	if ((_pos < 0) ||  (_pos > (ICOUNT(mbw))))
		_pos = ICOUNT(mbw);

	if (ICOUNT(mbw) == CUR_SIZE(mbw))
	{
		CUR_SIZE(mbw) += CHUNK_SIZE(mbw);
		ITEMS(mbw) = (_XmString *)XtRealloc((char *)ITEMS(mbw), sizeof(_XmString) * CUR_SIZE(mbw));
		DATA(mbw) = (XtPointer *)XtRealloc((char *)DATA(mbw), sizeof(XtPointer) * CUR_SIZE(mbw));
	}

	bcopy(ITEMS(mbw)+_pos, ITEMS(mbw)+_pos+1, (ICOUNT(mbw)-_pos) * sizeof(_XmString));
	bcopy(DATA(mbw)+_pos, DATA(mbw)+_pos+1, (ICOUNT(mbw)-_pos) * sizeof(XtPointer));

	ITEMS(mbw)[_pos] = _XmStringCreate(_item);
	DATA(mbw)[_pos] = _cbData;
	ICOUNT(mbw)++;

	recheckSizesAtPos(mbw, _pos);
}

static void recheckSizesAtPos(XmMegaButtonWidget _mbw, int _pos)
{
	ShellWidget popupShell;
	XtGeometryResult result;
        XmMegaButtonClassPart *mbwcp =
                &((XmMegaButtonWidgetClass)XtClass(_mbw))->mega_button_class;

	popupShell = (ShellWidget)XtGetShell((Widget)_mbw);
	if (popupShell && !popupShell->shell.popped_up)
	{
		Dimension width, height;
		Dimension replyWidth, replyHeight, oldWidth;

		width = WIDTH(_mbw);
		height = HEIGHT(_mbw);

		if (!HAS_ARROWS(_mbw))
		{
			HAS_ARROWS(_mbw) = ICOUNT(_mbw) > VIS_COUNT(_mbw);

			if (!HAS_ARROWS(_mbw))
				height = EL_HEIGHT(_mbw) * ICOUNT(_mbw);
			else
				height = EL_HEIGHT(_mbw) * (VIS_COUNT(_mbw) + 2) + 2 * MINI_SEP_HEIGHT;
		}

		LABEL(_mbw)._label = ITEMS(_mbw)[_pos];
		_XmCalcLabelDimensions((Widget)_mbw);

		if (LABEL(_mbw).TextRect.width > MAX_STRING_WIDTH(_mbw))
		{
			MAX_STRING_WIDTH(_mbw) = LABEL(_mbw).TextRect.width;
			MAX_STRING_POS(_mbw) = _pos;
			oldWidth = WIDTH(_mbw);
			WIDTH(_mbw) = 0;
			(*xmLabelWidgetClass->core_class.resize) ((Widget)_mbw);
			width = WIDTH(_mbw);
			WIDTH(_mbw) = oldWidth;

/* 			YOFFSET(_mbw) = LABEL(_mbw).TextRect.y; */
			XOFFSET(_mbw) = LABEL(_mbw).TextRect.x;

			if (MODE(_mbw) == XmMODE_TOGGLE_BUTTON)
				(*mbwcp->toggleSpaceProc)
                                 (_mbw, &width, &EL_HEIGHT(_mbw));
		}

		if (height == 0)
			height = 2 * PRIM(_mbw).shadow_thickness;
		if (width == 0)
			width = 2 * PRIM(_mbw).shadow_thickness;

		while((result = XtMakeResizeRequest((Widget)_mbw, width, height, &replyWidth,
			&replyHeight)) == XtGeometryAlmost)
		{
			width = replyWidth;
			height = replyHeight;
		}
	}
}

XtPointer XmMegaButtonRemoveItem(Widget _w, int _pos)
{
	XtPointer data;
	XmMegaButtonWidget mbw;
	XtGeometryResult result;
	Dimension width, height;
	Dimension replyWidth, replyHeight, oldWidth;

	mbw = (XmMegaButtonWidget)_w;

	if ((_pos < 0) || (_pos >= ICOUNT(mbw)))
		return NULL;

	_XmStringFree(ITEMS(mbw)[_pos]);
	data = DATA(mbw)[_pos];

	ICOUNT(mbw)--;
	bcopy(ITEMS(mbw)+_pos+1, ITEMS(mbw)+_pos, (ICOUNT(mbw)-_pos) * sizeof(_XmString));
	bcopy(DATA(mbw)+_pos+1, DATA(mbw)+_pos, (ICOUNT(mbw)-_pos) * sizeof(XtPointer));

	if (ICOUNT(mbw) != 0)
		recheckSizesAtPos(mbw, 0);

	return data;
}

int XmMegaButtonItemCount(Widget _w, Boolean _forgetFake)
{
	XmMegaButtonWidget mbw;

	mbw = (XmMegaButtonWidget)_w;

	if (_forgetFake && FAKE_ITEM(mbw))
		return 0;
	else
		return (ICOUNT(mbw));
}


int XmMegaButtonGetPos(Widget _w)
{
	XmMegaButtonWidget mbw;

	mbw = (XmMegaButtonWidget)_w;
	if (MODE(mbw) == XmMODE_TOGGLE_BUTTON)
		return SET_POS(mbw);
	else
		return -1;
}

void XmMegaButtonSetPos(Widget _w, int _pos)
{
	XmMegaButtonWidget mbw;

	mbw = (XmMegaButtonWidget)_w;
	if (MODE(mbw) == XmMODE_TOGGLE_BUTTON)
	{
		if (_pos > (ICOUNT(mbw)-1))
			_pos = ICOUNT(mbw) - 1;

		SET_POS(mbw) = _pos;
	}
}

extern XmString XmMegaButtonGetXmStringAtPos(Widget _w, int _pos)
{
	XmMegaButtonWidget mbw = (XmMegaButtonWidget)_w;
	
	if ((_pos >= 0) && (_pos < ICOUNT(mbw)))
		return _XmStringCreateExternal(LABEL(mbw).font, ITEMS(mbw)[_pos]);

	return (XmString)NULL;
}

extern void XmMegaButtonSetXmStringAtPos(Widget _w, XmString _xmstring, int _pos)
{
	XmMegaButtonWidget mbw = (XmMegaButtonWidget)_w;

	if ((_pos >= 0) && (_pos < ICOUNT(mbw)))
	{
		_XmStringFree(ITEMS(mbw)[_pos]);
		ITEMS(mbw)[_pos] = _XmStringCreate(_xmstring);
		recheckSizesAtPos(mbw, _pos);
	}
}

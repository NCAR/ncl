/*
(c) Copyright 1994, 1995, 1996 Microline Software, Inc.  ALL RIGHTS RESERVED
  
THIS PROGRAM BELONGS TO MICROLINE SOFTWARE.  IT IS CONSIDERED A TRADE
SECRET AND IS NOT TO BE DIVULGED OR USED BY PARTIES WHO HAVE NOT
RECEIVED WRITTEN AUTHORIZATION FROM THE OWNER.

THIS SOFTWARE IS FURNISHED UNDER A LICENSE AND MAY BE COPIED AND USED 
ONLY IN ACCORDANCE WITH THE TERMS OF THAT LICENSE AND WITH THE INCLUSION
OF THE ABOVE COPYRIGHT NOTICE.  THIS SOFTWARE AND DOCUMENTATION, AND ITS 
COPYRIGHTS ARE OWNED BY MICROLINE SOFTWARE AND ARE PROTECTED BY UNITED
STATES COPYRIGHT LAWS AND INTERNATIONAL TREATY PROVISIONS.
 
THE INFORMATION IN THIS SOFTWARE IS SUBJECT TO CHANGE WITHOUT NOTICE
AND SHOULD NOT BE CONSTRUED AS A COMMITMENT BY MICROLINE SOFTWARE.

THIS SOFTWARE AND REFERENCE MATERIALS ARE PROVIDED "AS IS" WITHOUT
WARRANTY AS TO THEIR PERFORMANCE, MERCHANTABILITY, FITNESS FOR ANY 
PARTICULAR PURPOSE, OR AGAINST INFRINGEMENT.  MICROLINE SOFTWARE
ASSUMES NO RESPONSIBILITY FOR THE USE OR INABILITY TO USE THIS 
SOFTWARE.

MICROLINE SOFTWARE SHALL NOT BE LIABLE FOR INDIRECT, SPECIAL OR
CONSEQUENTIAL DAMAGES RESULTING FROM THE USE OF THIS PRODUCT. SOME 
STATES DO NOT ALLOW THE EXCLUSION OR LIMITATION OF INCIDENTAL OR
CONSEQUENTIAL DAMAGES, SO THE ABOVE LIMITATIONS MIGHT NOT APPLY TO
YOU.

MICROLINE SOFTWARE SHALL HAVE NO LIABILITY OR RESPONSIBILITY FOR SOFTWARE
ALTERED, MODIFIED, OR CONVERTED BY YOU OR A THIRD PARTY, DAMAGES
RESULTING FROM ACCIDENT, ABUSE OR MISAPPLICATION, OR FOR PROBLEMS DUE
TO THE MALFUNCTION OF YOUR EQUIPMENT OR SOFTWARE NOT SUPPLIED BY
MICROLINE SOFTWARE.
  
U.S. GOVERNMENT RESTRICTED RIGHTS
This Software and documentation are provided with RESTRICTED RIGHTS.
Use, duplication or disclosure by the Government is subject to
restrictions as set forth in subparagraph (c)(1) of the Rights in
Technical Data and Computer Software Clause at DFARS 252.227-7013 or
subparagraphs (c)(1)(ii) and (2) of Commercial Computer Software -
Restricted Rights at 48 CFR 52.227-19, as applicable, supplier is
Microline Software, 41 Sutter St Suite 1374, San Francisco, CA 94104.
*/

#include "ProgressP.h"
#include <stdio.h>
#include <sys/time.h>

#ifdef XmL_ANSIC

static void ClassInitialize();
static void Initialize(Widget , Widget, ArgList, Cardinal *);
static void Resize(Widget);
static void Destroy(Widget);
static void Realize(Widget w, XtValueMask *valueMask,
	XSetWindowAttributes *attr);
static void Redisplay(Widget, XEvent *, Region);
static Boolean SetValues(Widget, Widget, Widget, ArgList, Cardinal *);
static void CopyFontList(XmLProgressWidget p);
static void TimeStr(char *, int);
static void DrawBarMeter(XmLProgressWidget p, XRectangle *rect);
static void DrawBoxesMeter(XmLProgressWidget p, XRectangle *rect);
static void DrawString(XmLProgressWidget, XmString, int, int,
	int, XRectangle *, XRectangle *);
static Boolean CvtStringToMeterStyle(Display *dpy, XrmValuePtr args,
	Cardinal *numArgs, XrmValuePtr fromVal, XrmValuePtr toVal,
	XtPointer *data);

#else

static void ClassInitialize();
static void Initialize();
static void Resize();
static void Destroy();
static void Realize();
static void Redisplay();
static Boolean SetValues();
static void CopyFontList();
static void DrawBarMeter();
static void DrawBoxesMeter();
static void TimeStr();
static void DrawString();
static Boolean CvtStringToMeterStyle();

#endif

static XtResource resources[] = 
	{
		{
		XmNcompleteValue, XmCCompleteValue,
		XtRInt, sizeof(int),
		XtOffset(XmLProgressWidget, progress.completeValue),
		XtRImmediate, (caddr_t)100
		},
		{
		XmNnumBoxes, XmCNumBoxes,
		XtRInt, sizeof(int),
		XtOffset(XmLProgressWidget, progress.numBoxes),
		XtRImmediate, (caddr_t)10
		},
		{
		XmNvalue, XmCValue,
		XtRInt, sizeof(int),
		XtOffset(XmLProgressWidget, progress.value),
		XtRImmediate, (caddr_t)0
		},
		{
		XmNfontList, XmCFontList,
		XmRFontList, sizeof(XmFontList),
		XtOffset(XmLProgressWidget, progress.fontList),
		XmRImmediate, (XtPointer)0,
		},
		{
		XmNmeterStyle, XmCMeterStyle,
		XmRMeterStyle, sizeof(unsigned char),
		XtOffset(XmLProgressWidget, progress.meterStyle),
		XmRImmediate, (XtPointer)XmMETER_BAR,
		},
		{
		XmNshowTime, XmCShowTime,
		XmRBoolean, sizeof(Boolean),
		XtOffset(XmLProgressWidget, progress.showTime),
		XmRImmediate, (XtPointer)False
		},
		{
		XmNshowPercentage, XmCShowPercentage,
		XmRBoolean, sizeof(Boolean),
		XtOffset(XmLProgressWidget, progress.showPercentage),
		XmRImmediate, (XtPointer)True
		}
	};

XmLProgressClassRec xmlProgressClassRec = 
	{
		{ /* Core */
		(WidgetClass)&xmPrimitiveClassRec,      /* superclass */
		"XmLProgress",                          /* class_name */
		sizeof(XmLProgressRec),                 /* widget_size */
		ClassInitialize,                        /* class_initialize */
		NULL,                                   /* class_part_initialize */
		FALSE,                                  /* class_inited */
		(XtInitProc)Initialize,                 /* initialize */
		NULL,                                   /* initialize_hook */
		(XtRealizeProc)Realize,                 /* realize */
		NULL,                                   /* actions */
		0,                                      /* num_actions */
		resources,                              /* resources */
		XtNumber(resources),                    /* num_resources */
		NULLQUARK,                              /* xrm_class */
		TRUE,                                   /* compress_motion */
		FALSE,                                  /* compress_exposure */
		TRUE,                                   /* compress_enterleave */
		TRUE,                                   /* visible_interest */
		(XtWidgetProc)Destroy,                  /* destroy */
		(XtWidgetProc)Resize,                   /* resize */
		(XtExposeProc)Redisplay,                /* expose */
		(XtSetValuesFunc)SetValues,             /* set_values */
		NULL,                                   /* set_values_hook */
		XtInheritSetValuesAlmost,               /* set_values_almost */
		NULL,                                   /* get_values_hook */
		NULL,                                   /* accept_focus */
		XtVersion,                              /* version */
		NULL,                                   /* callback_private */
		XtInheritTranslations,                  /* tm_table */
		NULL,                                   /* query_geometry */
		NULL,                                   /* display_accelerator */
		NULL,                                   /* extension */
		},
		{ /* Primitive */
		(XtWidgetProc)_XtInherit,               /* border_highlight */
		(XtWidgetProc)_XtInherit,               /* border_unhighlight */
		XtInheritTranslations,                  /* translations */
		NULL,                                   /* arm_and_activate */
		NULL,                                   /* syn_resources */
		0,                                      /* num_syn_resources */
		NULL,                                   /* extension */
		},
		{ /* Progress */
		0,                                      /* unused */
		}
	};

WidgetClass xmlProgressWidgetClass = (WidgetClass)&xmlProgressClassRec;

static void ClassInitialize()
	{
	XmLInitialize();

	XtSetTypeConverter(XmRString, XmRMeterStyle, CvtStringToMeterStyle,
		0, 0, XtCacheNone, 0);
	}

static void Initialize(reqW, newW, args, narg)
Widget reqW, newW;
ArgList args;
Cardinal *narg;
	{
	XmLProgressWidget p;

	p = (XmLProgressWidget)newW;

	if (!p->core.width)
		p->core.width = 200;
	if (!p->core.height)
		p->core.height = 30;

	p->progress.gc = 0;
	p->progress.startTime = time(0);
	CopyFontList(p);
	if (p->progress.completeValue < 1)
		{
		XmLWarning(newW, "Initialize() - complete value can't be < 1");
		p->progress.completeValue = 1;
		}
	if (p->progress.numBoxes < 1)
		{
		XmLWarning(newW, "Initialize() - number of boxes can't be < 1");
		p->progress.numBoxes = 1;
		}
	if (p->progress.value < 0)
		{
		XmLWarning(newW, "Initialize() - value can't be < 0");
		p->progress.value = 0;
		}
	if (p->progress.value > p->progress.completeValue)
		{
		XmLWarning(newW, "Initialize() - value can't be > completeValue");
		p->progress.value = p->progress.completeValue;
		}
	XtVaSetValues(newW, XmNtraversalOn, False, NULL);
	}

static void Resize(w)
Widget w;
	{
	Display *dpy;
	Window win;

	if (!XtIsRealized(w))
		return;
	dpy = XtDisplay(w);
	win = XtWindow(w);
	XClearArea(dpy, win, 0, 0, 0, 0, True);
	}

static void Destroy(w)
Widget w;
	{
	Display *dpy;
	XmLProgressWidget p;

	p = (XmLProgressWidget)w;
	dpy = XtDisplay(w);
	if (p->progress.gc)
		{
		XFreeGC(dpy, p->progress.gc);
		XFreeFont(dpy, p->progress.fallbackFont);
		}
	XmFontListFree(p->progress.fontList);
	}

static void Realize(w, valueMask, attr)
Widget w;
XtValueMask *valueMask;
XSetWindowAttributes *attr;
	{
	XmLProgressWidget p;
	Display *dpy;
	WidgetClass superClass;
	XtRealizeProc realize;
	XGCValues values;
	XtGCMask mask;

	p = (XmLProgressWidget)w;
	dpy = XtDisplay(p);
	superClass = xmlProgressWidgetClass->core_class.superclass;
	realize = superClass->core_class.realize;
	(*realize)(w, valueMask, attr);

	if (!p->progress.gc)
		{
		p->progress.fallbackFont = XLoadQueryFont(dpy, "fixed");
		values.font = p->progress.fallbackFont->fid;
		p->progress.gc = XCreateGC(dpy, XtWindow(p), GCFont, &values);
		}
	}

static void Redisplay(w, event, region)
Widget w;
XEvent *event;
Region region;
	{
	XmLProgressWidget p;
	Display *dpy;
	Window win;
	XRectangle rect;
	int st;

	if (!XtIsRealized(w) || !w->core.visible)
		return;

	p = (XmLProgressWidget)w;
	dpy = XtDisplay(w);
	win = XtWindow(w);
	st = p->primitive.shadow_thickness;
	rect.x = st;
	rect.y = st;
	rect.width = p->core.width - st * 2;
	rect.height = p->core.height - st * 2;

	if (p->progress.meterStyle == XmMETER_BAR)
		DrawBarMeter(p, &rect);
	else if (p->progress.meterStyle == XmMETER_BOXES)
		DrawBoxesMeter(p, &rect);

#ifdef MOTIF11
	_XmDrawShadow(dpy, win,
		p->primitive.bottom_shadow_GC,
		p->primitive.top_shadow_GC,
		p->primitive.shadow_thickness,
		0, 0, p->core.width, p->core.height);
#else
	_XmDrawShadows(dpy, win,
		p->primitive.top_shadow_GC,
		p->primitive.bottom_shadow_GC,
		0, 0, p->core.width, p->core.height,
		p->primitive.shadow_thickness,
		XmSHADOW_IN);
#endif
	}

static void DrawBoxesMeter(p, rect)
XmLProgressWidget p;
XRectangle *rect;
	{
	Display *dpy;
	Window win;
	int i, j, st, nb, x1, x2;

	dpy = XtDisplay(p);
	win = XtWindow(p);
	st = p->primitive.shadow_thickness;
	nb = p->progress.numBoxes;
	if (nb * st * 2 > (int)rect->width)
		return;

	if (p->progress.completeValue)
		j = (int)((long)nb * (long)p->progress.value /
			(long)p->progress.completeValue);
	else
		j = 0;
	x2 = 0;
	for (i = 0; i < nb; i++)
		{
		if (i < j)
			XSetForeground(dpy, p->progress.gc, p->primitive.foreground); 
		else
			XSetForeground(dpy, p->progress.gc, p->core.background_pixel);
		x1 = x2;
		if (i == nb - 1)
			x2 = rect->width;
		else
			x2 = ((int)rect->width * (i + 1)) / nb;
		XFillRectangle(dpy, win, p->progress.gc,
			rect->x + x1 + st, rect->y + st,
			x2 - x1 - st * 2, rect->height - st * 2);
#ifdef MOTIF11
		_XmDrawShadow(dpy, win,
			p->primitive.bottom_shadow_GC,
			p->primitive.top_shadow_GC,
			p->primitive.shadow_thickness,
			rect->x + x1, rect->y,
			x2 - x1, rect->height);
#else
		_XmDrawShadows(dpy, win,
			p->primitive.top_shadow_GC,
			p->primitive.bottom_shadow_GC,
			rect->x + x1, rect->y,
			x2 - x1, rect->height,
			p->primitive.shadow_thickness,
			XmSHADOW_IN);
#endif
		}
	}

static void DrawBarMeter(p, rect)
XmLProgressWidget p;
XRectangle *rect;
	{
	Display *dpy;
	Window win;
	int timeLeft, timeSoFar;
	time_t currentTime;
	XmString str;
	Dimension strWidth, strHeight;
	XRectangle lRect, rRect;
	int percent;
	char c[10];
	long l;

	dpy = XtDisplay(p);
	win = XtWindow(p);

	/* Left Rect */
	if (p->progress.completeValue)
		l = (long)rect->width * (long)p->progress.value /
			(long)p->progress.completeValue;
	else
		l = 0;
	lRect.x = rect->x;
	lRect.y = rect->y;
	lRect.width = (Dimension)l;
	lRect.height = rect->height;
	XSetForeground(dpy, p->progress.gc, p->primitive.foreground); 
	XFillRectangle(dpy, win, p->progress.gc, lRect.x, lRect.y,
		lRect.width, lRect.height);

	/* Right Rect */
	rRect.x = rect->x + (int)l;
	rRect.y = rect->y;
	rRect.width = rect->width - (Dimension)l;
	rRect.height = rect->height;
	XSetForeground(dpy, p->progress.gc, p->core.background_pixel);
	XFillRectangle(dpy, win, p->progress.gc, rRect.x, rRect.y,
		rRect.width, rRect.height);

	if (p->progress.completeValue)
		percent = (int)(((long)p->progress.value * 100) /
			(long)p->progress.completeValue);
	else
		percent = 0;

	/* percent complete */
	sprintf(c, "%d%c", percent, '%');
	str = XmStringCreateSimple(c);
	XmStringExtent(p->progress.fontList, str, &strWidth, &strHeight);
	if (p->progress.showPercentage)
		DrawString(p, str, rect->x + rect->width / 2 - (int)strWidth / 2,
			rect->y + rect->height / 2 - (int)strHeight / 2, strWidth,
			&lRect, &rRect);
	XmStringFree(str);

	/* Left Time */
	currentTime = time(0);
	timeSoFar = (int)(currentTime - p->progress.startTime);
	if (p->progress.showTime && p->progress.value &&
		p->progress.value != p->progress.completeValue && timeSoFar)
		{
		TimeStr(c, timeSoFar);
		str = XmStringCreateSimple(c);
		XmStringExtent(p->progress.fontList, str,
			&strWidth, &strHeight);
		DrawString(p, str, rect->x + 5, rect->y + rect->height / 2 -
			(int)strHeight / 2, strWidth, &lRect, &rRect);
		XmStringFree(str);
		}

	/* Right Time */
	timeLeft = 0;
	if (percent)
		timeLeft = (timeSoFar * 100 / percent) - timeSoFar;
	if (p->progress.showTime && percent && percent != 100 && timeLeft)
		{
		TimeStr(c, timeLeft);
		str = XmStringCreateSimple(c);
		XmStringExtent(p->progress.fontList, str,
			&strWidth, &strHeight);
		DrawString(p, str, rect->x + rect->width - strWidth - 5,
			rect->y + rect->height / 2 - (int)strHeight / 2,
			strWidth, &lRect, &rRect);
		XmStringFree(str);
		}
	}

static void DrawString(p, str, x, y, strWidth, lRect, rRect)
XmLProgressWidget p;
XmString str;
int x, y, strWidth;
XRectangle *lRect, *rRect;
	{
	Display *dpy;
	Window win;

	dpy = XtDisplay(p);
	win = XtWindow(p);
	if (lRect->width && lRect->height)
		{
		XSetForeground(dpy, p->progress.gc, p->core.background_pixel);
		XSetClipRectangles(dpy, p->progress.gc, 0, 0, lRect, 1, Unsorted);
		XmStringDraw(dpy, win, p->progress.fontList, str,
			p->progress.gc, x, y, strWidth, XmALIGNMENT_BEGINNING,
			XmSTRING_DIRECTION_L_TO_R, 0);
		XSetClipMask(dpy, p->progress.gc, None);
		}
	if (rRect->width && rRect->height)
		{
		XSetForeground(dpy, p->progress.gc, p->primitive.foreground);
		XSetClipRectangles(dpy, p->progress.gc, 0, 0, rRect, 1, Unsorted);
		XmStringDraw(dpy, win, p->progress.fontList, str,
			p->progress.gc, x, y, strWidth, XmALIGNMENT_BEGINNING,
			XmSTRING_DIRECTION_L_TO_R, 0);
		XSetClipMask(dpy, p->progress.gc, None);
		}
	}

static void TimeStr(c, seconds)
char *c;
int seconds;
	{
	int h, m, s;

	s = seconds;
	m = s / 60;
	s -= m * 60;
	h = m / 60;
	m -= h * 60;
	if (h > 99)
		h = 99;
	if (h > 0 && m < 10)
		sprintf(c, "%d:0%d hr", h, m);
	else if (h > 0)
		sprintf(c, "%d:%d hr", h, m);
	else if (m > 0 && s < 10)
		sprintf(c, "%d:0%d min", m, s);
	else if (m > 0)
		sprintf(c, "%d:%d min", m, s);
	else
		sprintf(c, "%d sec", s);
	}

static Boolean SetValues(curW, reqW, newW, args, narg)
Widget curW, reqW, newW;
ArgList args;
Cardinal *narg;
	{
	XmLProgressWidget cur, p;
	XtAppContext app;

	cur = (XmLProgressWidget)curW;
	p = (XmLProgressWidget)newW;
	app = XtWidgetToApplicationContext(curW);
	if (p->progress.value == 0)
		p->progress.startTime = time(0);
	if (p->progress.completeValue < 1)
		{
		XmLWarning(newW, "SetValues() - complete value can't be < 1");
		p->progress.completeValue = 1;
		}
	if (p->progress.numBoxes < 1)
		{
		XmLWarning(newW, "SetValues() - number of boxes can't be < 1");
		p->progress.numBoxes = 1;
		}
	if (p->progress.value < 0)
		{
		XmLWarning(newW, "SetValues() - value can't be < 0");
		p->progress.value = 0;
		}
	if (p->progress.value > p->progress.completeValue)
		{
		XmLWarning(newW, "SetValues() - value can't be > completeValue");
		p->progress.value = p->progress.completeValue;
		}
	if (p->progress.fontList != cur->progress.fontList)
		{
		XmFontListFree(cur->progress.fontList);
		CopyFontList(p);
		}
	/* display changes immediately since we may be not get back
	   to XNextEvent if the calling application is computing */
	if (p->core.background_pixel != cur->core.background_pixel ||
		p->primitive.foreground != cur->primitive.foreground || 
		p->progress.value != cur->progress.value ||
		p->progress.completeValue != cur->progress.completeValue ||
		p->progress.fontList != cur->progress.fontList ||
		p->progress.showTime != cur->progress.showTime ||
		p->progress.showPercentage != cur->progress.showPercentage ||
		p->progress.meterStyle != cur->progress.meterStyle ||
		p->progress.numBoxes != cur->progress.numBoxes ||
		p->primitive.shadow_thickness != cur->primitive.shadow_thickness)
		{
		Redisplay(newW, 0, 0);
		XFlush(XtDisplay(newW));
		XmUpdateDisplay(newW);
		}
	return FALSE;
	}

static void CopyFontList(p)
XmLProgressWidget p;
	{ 
	if (!p->progress.fontList)
		p->progress.fontList = XmLFontListCopyDefault((Widget)p);
	else
		p->progress.fontList = XmFontListCopy(p->progress.fontList);
	if (!p->progress.fontList)
		XmLWarning((Widget)p, "- fatal error - font list NULL");
	}

static Boolean CvtStringToMeterStyle(dpy, args, narg, fromVal, toVal, data)
Display *dpy;
XrmValuePtr args;
Cardinal *narg;
XrmValuePtr fromVal, toVal;
XtPointer *data;
	{
	static XmLStringToUCharMap map[] =
		{
		{ "METER_BAR", XmMETER_BAR },
		{ "METER_BOXES", XmMETER_BOXES },
		{ 0, 0 },
		};

	return XmLCvtStringToUChar(dpy, "XmRMeterStyle", map, fromVal, toVal);
	}

/*
   Public Functions
*/

Widget XmLCreateProgress(parent, name, arglist, argcount)
Widget parent;
char *name;
ArgList arglist;
Cardinal argcount;
	{
	return XtCreateWidget(name, xmlProgressWidgetClass, parent,
		arglist, argcount);
	}



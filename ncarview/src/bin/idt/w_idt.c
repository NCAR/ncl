/*
 *	$Id: w_idt.c,v 1.5 1991-02-05 13:15:38 clyne Exp $
 */
/*
 *	w_idt.c
 *
 *	Author		John Clyne
 *
 *	Date		Wed Aug  1 12:57:15 MDT 1990
 *
 *	This is the main module for idt (interactive display tool). This 
 *	module contains widget-dependent code only.
 */
#include <stdio.h>
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>

#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/List.h>
#include <X11/Xaw/MenuButton.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/StripChart.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Cardinals.h>

#define	IDT
#include <ncarv.h>
#include "idt.h"
#include "bits.h"

extern	char	*malloc();

AppData	App_Data;	/* global data settable by user resources	*/

/*
 * resources returned in App_data
 */
static  XtResource      resources[] = {
        {XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
                XtOffset(AppDataPtr, x_font), XtRString, "Fixed" },
        {"translatorFont", "TranslatorFont", XtRString, sizeof (char *),
                XtOffset(AppDataPtr, font), XtRString, "font1" },
        {"translatorDevice", "TranslatorDevice", XtRString, sizeof (char *),
                XtOffset(AppDataPtr, device), XtRString, "X11" },
        {"history", "History", XtRBoolean, sizeof (Boolean),
                XtOffset(AppDataPtr, history), XtRString, "False" }
	};


/*
 * resources that need to be set for proper execution of the GUI
 */
static	String fallback_resources[] = {
	"*input:	True",
	"*file*file finder.value:	*",
	"*file*file finder.label:	file finder",
	"*file*file finder*value.translations: #override \\n\
		 <Key>Return: finderTranslation()",
	"*file*selection.label:	selection",
	"*file*selection.value:	\ ",
	"*file*selection*value.translations: #override \\n\
		 <Key>Return: okFileTranslation()",
	"*file*textDisplay*Text*translations: #override \\n\
		 <Key>Return: selectFileTranslation() \\n\
		 <Btn1Down>: select-start() selectFileTranslation()",
	"*simpleDialog*dialog*value.translations: #override \\n\
		 <Key>Return: okSDTranslation()",
	"*scrollbar*orientation:	horizontal",
	"*scrollbar*length:		100",
	"*iconPixmap:	/usr/include/X11/bitmaps/ncarv_idt.bits",
	NULL
	};

void	FinderTranslation(), SelectFileTranslation(), 
		OkFileTranslation(), OkSDTranslation();

/*
 *	Actions to be added to the translation table
 */
XtActionsRec actionTable[] = {
    {"finderTranslation",	FinderTranslation},
    {"okFileTranslation",	OkFileTranslation},
    {"selectFileTranslation",	SelectFileTranslation},
    {"okSDTranslation",		OkSDTranslation}
};

static	XrmOptionDescRec 	options[] = {
	{"-d",	"*translatorDevice",	XrmoptionSepArg,	NULL},
	{"-f",	"*translatorFont",	XrmoptionSepArg,	NULL},
	{"-history",	"*history",	XrmoptionNoArg,		"True"}
	};


static	void create_main_panel(), Syntax();
static	void Select_file(), Display_(), Quit();

extern	void	CreateFileSelectPopup(), CreateDisplayPopoup();
extern	void	InitDisplayModule(), CloseDisplayModule();

void 
main(argc, argv)
	int argc;
	char **argv;
{

	Widget toplevel;
	XtAppContext app_con;

	void SetFileSelection();

	toplevel = XtAppInitialize(&app_con, "Idt", options, XtNumber(options),
			       &argc, argv, fallback_resources, NULL, ZERO);

	/*
	 * get some resource values
	 */
	XtGetApplicationResources(toplevel, &App_Data, resources, 
		XtNumber(resources), NULL, 0);

	if (argc == 2) {
		SetFileSelection(argv[1]);
		argc--;
	}

	if (argc != 1) 
		Syntax(argv[0]);

	InitDisplayModule(argv[0], (short) App_Data.history);

	/*
	 * load application actions into the translation manager
	 */
	XtAppAddActions(app_con, actionTable, XtNumber(actionTable));


	/*
	 * the main control panel
	 */
	create_main_panel(toplevel);

	XtRealizeWidget(toplevel);

	XtAppMainLoop(app_con);
}

/*
 *	create_main_panel
 *	[internal]
 *
 *	Create the main control panel
 *
 * on entry
 *	parent		: the parent widget of the control panel
 */

static void
create_main_panel(parent)
	Widget parent;
{
	Widget paned, form, text, select_file, display, quit;
	Cardinal n;
	Arg args[10];

	XFontStruct	*x_font;
	int	dummy,
		ascent, descent; 	/* retrieve bounds of font	*/
	XCharStruct	overall;

	extern	void	InitText(), AppendText();

	char *line1 = " Copyright (C) 1990 - All Rights Resevered    \n";
	char *line2 = " University Corporations for Atmospheric Research   \n";
	char *line3 = " NCAR View - UNIX Version 3.01   \n";
	char *header;

	/*
	 * create the initial header to be displayed in the main control
	 * panel text widget
	 */
	header = icMalloc((unsigned) 
			(strlen(line1) + strlen(line2) + strlen(line3) + 1));

	(void) strcpy(header, line1);
	(void) strcat(header, line2);
	(void) strcat(header, line3);
	
	paned = XtCreateManagedWidget( "paned", panedWidgetClass, parent,
				 (ArgList) NULL, ZERO );

	/*
	 * create a text widget to display header initialy and to display
	 * messages from the translator
	 */
	n = 0;	
	XtSetArg(args[n], XtNeditType, XawtextRead); n++;
	XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways); n++;
	XtSetArg(args[n], XtNtype, XawAsciiString); n++;
	text = XtCreateManagedWidget("text",asciiTextWidgetClass,paned,args,n);

	InitText(text, MAX_TEXT_LINES);
	AppendText(header);
	cfree(header);

	/*
	 * resize the text widget so it can hold the complete text header and
	 * have scroll bars as well.
	 */
#ifdef	DEAD

	/*
	 * we should be able to get the text widget font as a resouce from 
	 * the text widget. But it seems to be broken
	 */
	n = 0;
	XtSetArg(args[n], XtNfont, &x_font);
	XtGetValues(text, args, n);
#else
	x_font = App_Data.x_font;
#endif

	XTextExtents(x_font,line2,strlen(line2),
				&dummy,&ascent,&descent,&overall);

	n = 0;
	XtSetArg(args[n],XtNwidth,x_font->max_bounds.width * 70); n++;
	XtSetArg(args[n], XtNheight, 5 * (ascent + descent)); n++;
	XtSetValues(text, args, n);

	/*
	 * create the form to hold the main contol panel buttons
	 */

	n = 0;
	form = XtCreateManagedWidget("form",formWidgetClass,paned,args,n);

	n = 0;
	select_file = XtCreateManagedWidget("select file", 
		commandWidgetClass, form ,args,n);

	XtAddCallback(select_file, XtNcallback, Select_file, (XtPointer) NULL);

	n = 0;
	XtSetArg(args[n], XtNfromHoriz, select_file); n++;
	display = XtCreateManagedWidget("display", 
		commandWidgetClass, form ,args,n);

	XtAddCallback(display, XtNcallback, Display_, (XtPointer) NULL);
	n = 0;
	XtSetArg(args[n], XtNfromHoriz, display); n++;
	quit = XtCreateManagedWidget("quit", 
		commandWidgetClass, form ,args,n);

	XtAddCallback(quit, XtNcallback, Quit, (XtPointer) NULL);
}


static void 
Syntax(call)
	char *call;
{
	(void) fprintf(stderr, 
		"%s: Usage: idt [-d device] [-f font] [-h] [filename]\n",
		call);
	exit(1);
}


/*
 *	The Callbacks
 */


/*
 *	Select_file
 *
 *	creates a file selection box popup
 */
/*ARGSUSED*/
static	void Select_file(widget, closure, call_data)
	Widget	widget;
	XtPointer	closure;	/* unused	*/
	XtPointer	call_data;	/* unused	*/
{

	CreateFileSelectPopup(widget);
}

/*
 *	Display_
 *
 *	Creates a display popup menu to display the most recently selected
 *	file.
 */
/*ARGSUSED*/
static	void Display_(widget, closure, call_data)
	Widget	widget;
	XtPointer	closure;	/* unused	*/
	XtPointer	call_data;	/* unused	*/
{
	char	*file;
	void	AppendText();
	void	CreateDisplayPopup();

	extern	char	*GetFileSelection();

	file = GetFileSelection();

	if (! file) {
		AppendText("No file selected. Use \"Select file\"\n");
	}
	else
		CreateDisplayPopup(widget, file);
}

/*ARGSUSED*/
static	void Quit(widget, closure, call_data)
	Widget	widget;
	XtPointer	closure;	/* unused	*/
	XtPointer	call_data;	/* unused	*/
{
	(void) CloseDisplayModule();
	exit(0);
}


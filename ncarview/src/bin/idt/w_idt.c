/*
 *	$Id: w_idt.c,v 1.13 1991-08-15 17:15:43 clyne Exp $
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
        {
	XtNfont, XtCFont, XtRFontStruct, sizeof (XFontStruct *),
                XtOffset(AppDataPtr, x_font), XtRString, "Fixed" 
	},
        {
	"fileSelectAction", "FileSelectAction", XtRString, sizeof(XFontStruct*),
                XtOffset(AppDataPtr, select_action), XtRString, NULL 
	},
        {
	"translatorFont", "TranslatorFont", XtRString, sizeof (char *),
                XtOffset(AppDataPtr, font), XtRString, "font1" 
	},
        {
	"translatorDevice", "TranslatorDevice", XtRString, sizeof (char *),
                XtOffset(AppDataPtr, device), XtRString, "X11" 
	},
        {
	"history", "History", XtRBoolean, sizeof (Boolean),
                XtOffset(AppDataPtr, history), XtRString, "False" 
	},
        {
	"translatorSoft", "TranslatorSoft", XtRBoolean, sizeof (Boolean),
                XtOffset(AppDataPtr, soft), XtRString, "False" 
	},
        {
	"translatorLmin", "TranslatorLmin", XtRString, sizeof (String),
                XtOffset(AppDataPtr, lmin), XtRString, NULL 
	},
        {
	"translatorLmax", "TranslatorLmax", XtRString, sizeof (String),
                XtOffset(AppDataPtr, lmax), XtRString, NULL 
	},
        {
	"translatorLscale", "TranslatorLscale", XtRString, sizeof (String),
                XtOffset(AppDataPtr, lscale), XtRString, NULL 
	},
        {
	"translatorForeground", "TranslatorForeground",XtRString,sizeof(String),
                XtOffset(AppDataPtr, foreground), XtRString, NULL 
	},
        {
	"translatorBackground", "TranslatorBackground",XtRString,sizeof(String),
                XtOffset(AppDataPtr, background), XtRString, NULL 
	},
        {
	"translatorReverse", "TranslatorReverse", XtRBoolean, sizeof (Boolean),
                XtOffset(AppDataPtr, reverse), XtRString, "False" 
	},
        {
	"translatorPal", "TranslatorPal", XtRString, sizeof (char *),
                XtOffset(AppDataPtr, pal), XtRString, NULL 
	},
        {
	"messageHeight", "MessageHeight", XtRInt, sizeof (int),
                XtOffset(AppDataPtr, message_height), XtRString, "5" 
	}
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
	"*file*Text.translations: #override \\n\
		 <Key>Return: selectFileTranslation() \\n\
		 <Btn1Down>: select-start() selectFileTranslation()",
	"*simpleDialog*dialog*value.translations: #override \\n\
		 <Key>Return: okSDTranslation()",
	"*scrollbar*orientation:	horizontal",
	"*scrollbar*length:		100",
	"*fileSelectAction:	display",
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
	{"-d",		"*translatorDevice",	XrmoptionSepArg,	NULL},
	{"-f",		"*translatorFont",	XrmoptionSepArg,	NULL},
	{"-history",	"*history",		XrmoptionNoArg,		"True"},
	{"-soft",	"*translatorSoft",	XrmoptionNoArg,		"True"},
	{"-lmin",	"*translatorLmin",	XrmoptionSepArg,	NULL},
	{"-lmax",	"*translatorLmax",	XrmoptionSepArg,	NULL},
	{"-lscale",	"*translatorLscale",	XrmoptionSepArg,	NULL},
	{"-foreground",	"*translatorForeground",XrmoptionSepArg,	NULL},
	{"-background",	"*translatorBackground",XrmoptionSepArg,	NULL},
	{"-reverse",	"*translatorReverse",	XrmoptionNoArg,		"True"},
	{"-pal",	"*translatorPal",	XrmoptionSepArg,	NULL}
};


static	void create_main_panel(), Syntax();
static	void Select_file(), Display_(), Quit();
static	Widget	displayButton;

extern	void	CreateFileSelectPopup(), CreateDisplayPopoup();
extern	void	InitDisplayModule(), CloseDisplayModule();

void 
main(argc, argv)
	int argc;
	char **argv;
{

	Widget 		toplevel;
	XtAppContext 	app_con;
	char		**targv;	/* translator args	*/
	int		targc;

	char	**get_trans_commandline();
	void 	SetFileSelection();
	void	(*select_action)() = NULL;
	void	action_display();
	char	*meta_fname = NULL;
	void	XAppDirPath();

	/*
	 * hack to ensure idt app resource file is found
	 */
	XAppDirPath();

	toplevel = XtAppInitialize(&app_con, "Idt", options, XtNumber(options),
			       &argc, argv, fallback_resources, NULL, ZERO);

	SetIconResource(toplevel);

	/*
	 * get some resource values
	 */
	XtGetApplicationResources(toplevel, &App_Data, resources, 
		XtNumber(resources), NULL, 0);

	if (App_Data.select_action) {
		if (! strcmp("display", App_Data.select_action)) {
			select_action = action_display;
		}
		else {
			(void) fprintf(stderr, 
				"Warning - unknown select action:%s\n",
				App_Data.select_action);
		}
	}
	/*
	 * build the command line for the translator including any
	 * translator options that may have been passed on the idt command
	 * line
	 */
	targv = get_trans_commandline(&targc, &App_Data);

	if (argc == 2 && *argv[1] != '-') {
		meta_fname = argv[1];
		argc--;
	}

	if (argc != 1) 
		Syntax(argv[0]);

	InitDisplayModule(argv[0], targv, targc, (short) App_Data.history);

	/*
	 * load application actions into the translation manager
	 */
	XtAppAddActions(app_con, actionTable, XtNumber(actionTable));

	

	/*
	 * the main control panel
	 */
	create_main_panel(toplevel, select_action);

	XtRealizeWidget(toplevel);
	if (meta_fname) {
		SetFileSelection(meta_fname, select_action);
	}

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
 *	select_action	: action to be executed on a file selection
 */

static void
create_main_panel(parent, select_action)
	Widget parent;
	void	(*select_action)();
{
	Widget paned, form, text, select_file, quit;
	Cardinal n;
	Arg args[10];

	XFontStruct	*x_font;
	int	dummy,
		ascent, descent; 	/* retrieve bounds of font	*/
	XCharStruct	overall;

	extern	void	InitText(), AppendText();

	char *line1 = " Copyright (C) 1991 - All Rights Reserved    \n";
	char *line2 = " University Corporation for Atmospheric Research   \n";
	char *line3 = " NCAR View - UNIX Version 3.01   \n";
	char *header;

	int	message_height	= App_Data.message_height;

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
	XtSetArg(args[n], XtNheight, message_height * (ascent + descent)); n++;
	XtSetValues(text, args, n);

	/*
	 * create the form to hold the main contol panel buttons
	 */

	n = 0;
	form = XtCreateManagedWidget("form",formWidgetClass,paned,args,n);

	n = 0;
	select_file = XtCreateManagedWidget("select file", 
		commandWidgetClass, form ,args,n);

	XtAddCallback(select_file, XtNcallback, Select_file, 
					(XtPointer) select_action);

	n = 0;
	XtSetArg(args[n], XtNfromHoriz, select_file); n++;
	displayButton = XtCreateManagedWidget("display", 
		commandWidgetClass, form ,args,n);

	XtAddCallback(displayButton, XtNcallback, Display_, (XtPointer) NULL);
	n = 0;
	XtSetArg(args[n], XtNfromHoriz, displayButton); n++;
	quit = XtCreateManagedWidget("quit", 
		commandWidgetClass, form ,args,n);

	XtAddCallback(quit, XtNcallback, Quit, (XtPointer) NULL);
}


static void 
Syntax(call)
	char *call;
{
	(void) fprintf(stderr, 
		"%s: Usage: idt [-d device] [-f font] [-h] [-soft] [-lmin width] [-lmax width] [-lscale width] [-pal pal_fname] [ filename ]\n", call);
	exit(1);
}


/*
 *	get_trans_commandline
 *
 *	build the command line for the translator complete with any options
 *	specifed by the user. Leave room for metafile name to be appended
 *	to end of command line.
 * on entry
 *	app_data	: translator options
 * on exit
 *	targc		: length of command line including metafile name
 *	return		: the command line
 */
static	char	**get_trans_commandline(targc, app_data)
	int	*targc;
	AppData	*app_data;
{
	char	**targv;
	int	i;
	char	*binpath;

	/*
	 * alloc enough memory for translator name, each option specifier and 
	 * possilbly its  argument, and the name of the metafile.
	 */
	targv = (char **) icMalloc((sizeof (char *) * TRANS_ARG_COUNT * 2) + 3);
	i = 0;

	/*
	 * get the path to the translator
	 */
	binpath = GetNCARGPath("BINDIR");
	binpath = binpath ? binpath : BINDIR_DEFAULT;

	/*
	 * the translator is the first arg
	 */
	targv[i] = icMalloc((unsigned) 
		(strlen(binpath) + strlen("/") + (strlen(TRANSLATOR) + 1)));

	(void) strcpy(targv[i], binpath);
	(void) strcat(targv[i], "/");
	(void) strcat(targv[i], TRANSLATOR);
	i++;

	/*
	 * now stuff the command line options in
	 */
	if (app_data->font) {	/* which fontcap	*/
		targv[i] = icMalloc ((unsigned) (strlen (TR_FONT) + 1));
		(void) strcpy(targv[i], TR_FONT);
		i++;
		targv[i] = icMalloc ((unsigned) (strlen(app_data->font) + 1));
		(void) strcpy(targv[i], app_data->font);
		i++;
	}
	if (app_data->device) {	/* which graphcap	*/
		targv[i] = icMalloc ((unsigned) (strlen (TR_DEVICE) + 1));
		(void) strcpy(targv[i], TR_DEVICE);
		i++;
		targv[i] = icMalloc((unsigned ) (strlen(app_data->device) + 1));
		(void) strcpy(targv[i], app_data->device);
		i++;
	}
	if (app_data->soft) {	/* soft filling of polygons	*/
		targv[i] = icMalloc ((unsigned) (strlen (TR_SOFT) + 1));
		(void) strcpy(targv[i], TR_SOFT);
		i++;
	}
	if (app_data->lmin) {	/* mininum line width		*/
		targv[i] = icMalloc ((unsigned) strlen (TR_LMIN) + 1);
		(void) strcpy(targv[i], TR_LMIN);
		i++;
		targv[i] = icMalloc ((unsigned) strlen(app_data->lmin) + 1);
		(void) strcpy(targv[i], app_data->lmin);
		i++;
	}
	if (app_data->lmax) {	/* maximum line width		*/
		targv[i] = icMalloc ((unsigned) strlen (TR_LMAX) + 1);
		(void) strcpy(targv[i], TR_LMAX);
		i++;
		targv[i] = icMalloc ((unsigned) strlen(app_data->lmax) + 1);
		(void) strcpy(targv[i], app_data->lmax);
		i++;
	}
	if (app_data->lscale) {	/* line scaling			*/
		targv[i] = icMalloc ((unsigned) strlen (TR_LSCALE) + 1);
		(void) strcpy(targv[i], TR_LSCALE);
		i++;
		targv[i] = icMalloc ((unsigned) strlen(app_data->lscale) + 1);
		(void) strcpy(targv[i], app_data->lscale);
		i++;
	}
	if (app_data->foreground) {	/* default foreground color	*/
		targv[i] = icMalloc ((unsigned) strlen (TR_FOREGROUND) + 1);
		(void) strcpy(targv[i], TR_FOREGROUND);
		i++;
		targv[i] = icMalloc((unsigned)strlen(app_data->foreground) + 1);
		(void) strcpy(targv[i], app_data->foreground);
		i++;
	}
	if (app_data->background) {	/* default background color	*/
		targv[i] = icMalloc ((unsigned) strlen (TR_BACKGROUND) + 1);
		(void) strcpy(targv[i], TR_BACKGROUND);
		i++;
		targv[i] = icMalloc ((unsigned) strlen(app_data->background)+1);
		(void) strcpy(targv[i], app_data->background);
		i++;
	}
	if (app_data->reverse) {	/* reverse video		*/
		targv[i] = icMalloc ((unsigned) strlen (TR_REVERSE) + 1);
		(void) strcpy(targv[i], TR_REVERSE);
		i++;
	}
	if (app_data->pal) {	/* optional color palette 	*/
		targv[i] = icMalloc ((unsigned) strlen (TR_PAL) + 1);
		(void) strcpy(targv[i], TR_PAL);
		i++;
		targv[i] = icMalloc ((unsigned) strlen(app_data->pal) + 1);
		(void) strcpy(targv[i], app_data->pal);
		i++;
	}

	/*
	 * hold a spot for the metafile. We don't know its name yet
	 */
	targv[i] = NULL;
	i++;

	/*
	 * terminate with a NULL
	 */
	*targc = i;
	targv[i] = NULL;
	return(targv);
}

void	action_display()
{
	XtCallCallbacks(displayButton, XtNcallback, (XtPointer) NULL);
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
static	void Select_file(widget, client_data, call_data)
	Widget	widget;
	XtPointer	client_data;	/* select action	*/
	XtPointer	call_data;	/* unused		*/
{
	void	(*select_action)() = (void (*)()) client_data;

	CreateFileSelectPopup(widget, select_action);
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


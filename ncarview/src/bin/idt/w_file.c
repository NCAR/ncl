/*
 *	$Id: w_file.c,v 1.13 2008-07-27 03:18:39 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/*
 *	w_file.c
 *
 *	Author		John Clyne
 *
 *	Date		Tue Jul 31 18:12:46 MDT 1990
 *
 *	This file together with 'file.c' define a file selection box. 
 *	Widget-dependent code is located in this module. Nonwidget-dependent
 *	code is in 'file.c'
 */
#include <stdio.h>                      /* For the Syntax message */
#include <stdlib.h>                      /* For the Syntax message */
#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include <X11/Xaw/Dialog.h>
#include <X11/Xaw/Paned.h>
#include <X11/Xaw/Scrollbar.h>
#include <X11/Xaw/AsciiText.h>
#include <X11/Xaw/Command.h>

#include <ncarg/c.h>
#include "idt.h"
#include "file.h"
#include "w_file.h"

/*
 * callbacks
 */

static	Widget	textDisplay,	/* widget to display file names	*/
		dialogSelection;

Widget	dialogFinder;		/* this should be made static	*/

static	char	*fileFinder = NULL;	/* current file finder	*/

static	FuncPtrPasser	selectAction;

static	void	destroy_popup(popup)
	Widget	popup;
{
	XtDestroyWidget(popup);
}

/*
 *	File Selection Box Callbacks
 */
/*ARGSUSED*/
static	void	OkFile(widget, client_data, call_data)
	Widget	widget;
	XtPointer	client_data,	/* the popup	*/
			call_data;	/* unused	*/
{
	Widget	popup = (Widget) client_data;
	char	*file;


	/*
	 * get the currently selected file from the selection dialog box
	 * Set it is the users current selection.
	 */
	file = XawDialogGetValueString(dialogSelection);

	(void) SetFileSelection(file, selectAction);

	/*
	 * popdown the popup. We're done
	 */
	destroy_popup(popup);
}

/*ARGSUSED*/
static	void	Finder(widget, client_data, call_data)
	Widget	widget;
	XtPointer	client_data,	/* unused	*/
			call_data;	/* the dialog 	*/
{
	Widget	dialog = (Widget) client_data;
	Arg	args[1];

	String	files;
	char	*file_finder;
	int	longest;
	Cardinal	n;

	if (fileFinder)	free((Voidptr) fileFinder);
	/*
	 * Get the new file finder input by the user. Use it to get a new
	 * set of files. Display the new set of files in the text widget
	 */
	file_finder = XawDialogGetValueString(dialog);
	if (! file_finder) file_finder = "*";
	fileFinder = malloc((unsigned) (strlen(file_finder) + 1));
	if ( !fileFinder) {
		perror("malloc()");
		exit(1);
	}
	(void) strcpy(fileFinder, file_finder);


	files = (String) GetFiles(fileFinder, &longest);

	n = 0;
	XtSetArg(args[n], XtNstring, files);	n++;
	XtSetValues(textDisplay, args, n);

}

/*ARGSUSED*/
static	void	Cancel(widget, client_data, call_data)
	Widget	widget;
	XtPointer	client_data,	/* the popup	*/
			call_data;	/* unused 	*/
{
	Widget	popup = (Widget) client_data;


	destroy_popup(popup);

}

/*
 *	CreateFileSelectPopup()
 *	[exported]
 *
 *	This routine creates a modal popup containing the file selection box
 *	It is important that the popup is modal, and hence grabs the server,
 *	since this prevents multible instantiations of the selection box. The
 *	current code does not provide for multible selection boxes active at
 *	the same time. The popup's resource name is 'file'
 *
 * on entry
 *	button		: widget that determines position of popup
 *	select_action	: function to be invoked when a file is selected
 */
void	CreateFileSelectPopup(button, select_action)
	Widget	button;
	FuncPtrPasser	*select_action;
{

	Arg		args[10];
	Widget	popup, pane;
	Position	x, y;		/* position of popup		*/
	Dimension	width, height;	/* dimension of button		*/
	Cardinal	n;
	XFontStruct	*x_font;	/* font used by application	*/	

	static	XawTextSelectType select_type[] = {
		XawselectLine, XawselectNull
		};		/* selection array for text widget	*/
		

	int	ascent, descent;
	String	files;
	int	longest;

	selectAction = *select_action;

	/*
	* This will position the upper left hand corner of the popup at the
	* center of the widget which invoked this callback, which will also
	* become the parent of the popup.  I don't deal with the possibility
	* that the popup will be all or partially off the edge of the screen.
	*/

	n = 0;
	XtSetArg(args[n], XtNwidth, &width); n++;
	XtSetArg(args[n], XtNheight, &height); n++;
	XtGetValues(button, args, n);
	XtTranslateCoords(button, (Position) (width / 2), 
		(Position) (height / 2), &x, &y);

	n = 0;
	XtSetArg(args[n], XtNx, x);				n++;
	XtSetArg(args[n], XtNy, y);				n++;

	popup = XtCreatePopupShell("file", transientShellWidgetClass, 
		button, args, n);


	/*
	 * a pane widget will manage the popup children
	 */
	pane = XtCreateManagedWidget("pane",
				panedWidgetClass,popup,(ArgList) NULL,0);


	/*
	 * a dialog box for selecting the file finder
	 */
	dialogFinder = XtCreateManagedWidget("file finder",
				dialogWidgetClass, pane, (ArgList) NULL,0);

	XawDialogAddButton(dialogFinder, "finder", Finder, 
		(XtPointer) dialogFinder);

	/*
	 * if this is first invocation then
	 * get the default file finder from resource data base. If none is 
	 * given than use '*' to denote all files (except hidden)
	 */
	if (! fileFinder) {
		char	*file_finder;

		file_finder = XawDialogGetValueString(dialogFinder);
		if (! file_finder) file_finder = "*";
		fileFinder = malloc((unsigned) (strlen(file_finder) + 1));
		if (! fileFinder) {
			perror("malloc()");
			exit(1);
		}
		(void) strcpy(fileFinder, file_finder);

	}

	/*
	 * display the file finder in the file finder box. This code is 
	 * redundant if the resource "file finder.value" is the default.
	 */
	n = 0;
	XtSetArg(args[n], XtNvalue, fileFinder);	n++;
	XtSetValues(dialogFinder, args, n);

	/*
	 * Using the file finder get a list of matching files. The file names
	 * are seperated by newlines. 'longest' contains the length of the 
	 * longest file name and is used to size the text widget. Make sure
	 * longest is at least some "reasonable" size.
	 */ 
	files = (String) GetFiles(fileFinder, &longest);
	longest = longest < 30 ? 30 : longest;

	/*
	 * create the text widget with the initial list of file names. For
	 * now the height and width of the widget are hardcoded based on 
	 * the dimensions of the font being used
	 */
	x_font = App_Data.x_font;
	ascent = x_font->max_bounds.ascent;
	descent = x_font->max_bounds.descent;

        n = 0;
        XtSetArg(args[n], XtNeditType, XawtextAppend); n++;
        XtSetArg(args[n], XtNscrollVertical, XawtextScrollAlways); n++;
        XtSetArg(args[n], XtNscrollHorizontal, XawtextScrollAlways); n++;
        XtSetArg(args[n], XtNtype, XawAsciiString); n++;
        XtSetArg(args[n], XtNstring, files); n++;
        XtSetArg(args[n], XtNselectTypes, select_type); n++;
        XtSetArg(args[n], XtNwidth,x_font->max_bounds.width * (longest+4)); n++;
        XtSetArg(args[n], XtNheight, 10 * (ascent + descent)); n++;

        textDisplay = XtCreateManagedWidget("textDisplay",
		asciiTextWidgetClass,pane,args,n);


	
	/*
	 * create a dialog widget to contain the user selected file and 
	 * buttons for canceling and accepting the selectio
	 */
	n = 0;
	dialogSelection = XtCreateManagedWidget("selection",
		dialogWidgetClass,pane,args,n);

	XawDialogAddButton(dialogSelection, "ok", OkFile, 
		(XtPointer) popup);

	XawDialogAddButton(dialogSelection, "cancel", Cancel, 
		(XtPointer) popup);


	/*
	 * pop the popup in modal mode
	 */
	XtPopup(popup, XtGrabExclusive);
}



/*
 *	The Actions. These actions are available to the translation table
 *	and may be set by the user's resource database.
 */
/*ARGSUSED*/

/*
 *	FinderTranslation
 *
 *	May be invoked to update the file selection finder.
 */
/*ARGSUSED*/
void
FinderTranslation(widget, event, params, num_params)
	Widget widget;		/* not used	*/
	XEvent *event;		/* not used	*/
	String *params;		/* not used	*/
	Cardinal *num_params;	/* not used	*/
{

        XtCallCallbacks(XtNameToWidget(dialogFinder, "finder"),
                XtNcallback, (XtPointer) NULL);

}

/*
 *	OkFileTranslation
 *
 *	May be invoked to call file selection.
 */
/*ARGSUSED*/
void
OkFileTranslation(widget, event, params, num_params)
	Widget widget;		/* not used	*/
	XEvent *event;		/* not used	*/
	String *params;		/* not used	*/
	Cardinal *num_params;	/* not used	*/
{

        XtCallCallbacks(XtNameToWidget(dialogSelection, "ok"),
                XtNcallback, (XtPointer) NULL);
}


/*
 *	SelectFileTranslation
 *
 *	May be invoked to update the dialog selection box with the current
 *	selection.
 */
/*ARGSUSED*/
void
SelectFileTranslation(widget, event, params, num_params)
	Widget widget;		/* not used	*/
	XEvent *event;		/* not used	*/
	String *params;		/* not used	*/
	Cardinal *num_params;	/* not used	*/
{
	Arg 	args[2];
	Cardinal	n;
	XawTextPosition	start, stop;
	XawTextBlock	text_block;

	char	*s;
	char	*t;


	/*
	 * get the position of the current selection from the text widget
	 */
	XawTextGetSelectionPos(textDisplay, &start, &stop);

	/*
	 * if start == stop than there is no selection. Else get the text 
	 * selected. XawTextSourceRead seems to be broken. It should only 
	 * return the number of characters as defined by (stop - start) but
	 * in fact returns all characters proceeding the 'start' position. So
	 * we need to prune the additional junk off.
	 */
	if (start != stop) {
		XawTextSourceRead(XawTextGetSource(textDisplay),
			start,&text_block,stop - start);

		s = text_block.ptr;
	}
	else {
		s = NULL;
	}

	if ( !(t = malloc((unsigned) ((stop - start) +1)))) {
		perror("malloc()");
		return;
	}
	(void) strncpy(t, s, (int) (stop - start));
	t[stop - start] = '\0';

	
	/*
	 * update the dialog selection box with the current selection.
	 */
	n = 0;
	XtSetArg(args[n], XtNvalue, t);	n++;
	XtSetValues(dialogSelection, args, n);

	free((Voidptr) t);
}

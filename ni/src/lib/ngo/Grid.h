/*
 *      $Id: Grid.h,v 1.2 1997-10-03 20:07:43 dbrown Exp $
 */
/*
(c) Copyright 1994, 1995, 1996 Microline Software, Inc.  ALL RIGHTS RESERVED
  
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

#ifndef XmLGridH
#define XmLGridH

#include <ncarg/ngo/XmL.h>
#include <stdio.h>

#ifdef XmL_CPP
extern "C" {
#endif

extern WidgetClass xmlGridWidgetClass;
typedef struct _XmLGridClassRec *XmLGridWidgetClass;
typedef struct _XmLGridRec *XmLGridWidget;
typedef struct _XmLGridRowRec *XmLGridRow;
typedef struct _XmLGridColumnRec *XmLGridColumn;
typedef struct _XmLGridCellRec *XmLGridCell;

#define XmLIsGrid(w) XtIsSubclass((w), xmlGridWidgetClass)

#ifdef XmL_ANSIC

Widget XmLCreateGrid(Widget parent, char *name, ArgList arglist,
	Cardinal argcount);
void XmLGridAddColumns(Widget w, unsigned char type, int position, int count);
void XmLGridAddRows(Widget w, unsigned char type, int position, int count);
Boolean XmLGridColumnIsVisible(Widget w, int column);
Boolean XmLGridCopyPos(Widget w, Time time, unsigned char rowType, int row,
	unsigned char columnType, int column, int nrow, int ncolumn);
Boolean XmLGridCopySelected(Widget w, Time time);
void XmLGridDeleteAllColumns(Widget w, unsigned char type);
void XmLGridDeleteAllRows(Widget w, unsigned char type);
void XmLGridDeleteColumns(Widget w, unsigned char type, int position,
	int count);
void XmLGridDeleteRows(Widget w, unsigned char type, int position, int count);
void XmLGridDeselectAllCells(Widget w, Boolean notify);
void XmLGridDeselectAllColumns(Widget w, Boolean notify);
void XmLGridDeselectAllRows(Widget w, Boolean notify);
void XmLGridDeselectCell(Widget w, int row, int column, Boolean notify);
void XmLGridDeselectColumn(Widget w, int column, Boolean notify);
void XmLGridDeselectRow(Widget w, int row, Boolean notify);
int XmLGridEditBegin(Widget w, Boolean insert, int row, int column);
void XmLGridEditCancel(Widget w);
void XmLGridEditComplete(Widget w);
XmLGridColumn XmLGridGetColumn(Widget w, unsigned char columnType, int column);
void XmLGridGetFocus(Widget w, int *row, int *column, Boolean *focusIn);
XmLGridRow XmLGridGetRow(Widget w, unsigned char rowType, int row);
int XmLGridGetSelectedCellCount(Widget w);
int XmLGridGetSelectedCells(Widget w, int *rowPositions,
	int *columnPositions, int count);
int XmLGridGetSelectedColumnCount(Widget w);
int XmLGridGetSelectedColumns(Widget w, int *positions, int count);
int XmLGridGetSelectedRow(Widget w);
int XmLGridGetSelectedRowCount(Widget w);
int XmLGridGetSelectedRows(Widget w, int *positions, int count);
void XmLGridMoveColumns(Widget w, int newPosition, int position, int count);
void XmLGridMoveRows(Widget w, int newPosition, int position, int count);
Boolean XmLGridPaste(Widget w);
Boolean XmLGridPastePos(Widget w, unsigned char rowType, int row,
	unsigned char columnType, int column);
int XmLGridRead(Widget w, FILE *file, int format, char delimiter);
int XmLGridReadPos(Widget w, FILE *file, int format, char delimiter,
	unsigned char rowType, int row, unsigned char columnType, int column);
void XmLGridRedrawAll(Widget w);
void XmLGridRedrawCell(Widget w, unsigned char rowType, int row,
	unsigned char columnType, int column);
void XmLGridRedrawColumn(Widget w, unsigned char type, int column);
void XmLGridRedrawRow(Widget w, unsigned char type, int row);
void XmLGridReorderColumns(Widget w, int *newPositions,
	int position, int count);
void XmLGridReorderRows(Widget w, int *newPositions,
	int position, int count);
int XmLGridRowColumnToXY(Widget w, unsigned char rowType, int row,
	unsigned char columnType, int column, Boolean clipped, XRectangle *rect);
Boolean XmLGridRowIsVisible(Widget w, int row);
void XmLGridSelectAllCells(Widget w, Boolean notify);
void XmLGridSelectAllColumns(Widget w, Boolean notify);
void XmLGridSelectAllRows(Widget w, Boolean notify);
void XmLGridSelectCell(Widget w, int row, int column, Boolean notify);
void XmLGridSelectColumn(Widget w, int column, Boolean notify);
void XmLGridSelectRow(Widget w, int row, Boolean notify);
int XmLGridSetFocus(Widget w, int row, int column);
int XmLGridSetStrings(Widget w, char *data);
int XmLGridSetStringsPos(Widget w, unsigned char rowType, int row,
	unsigned char columnType, int column, char *data);
int XmLGridWrite(Widget w, FILE *file, int format, char delimiter,
	Boolean skipHidden);
int XmLGridWritePos(Widget w, FILE *file, int format, char delimiter,
	Boolean skipHidden, unsigned char rowType, int row,
	unsigned char columnType, int column, int nrow, int ncolumn);
int XmLGridXYToRowColumn(Widget w, int x, int y, unsigned char *rowType,
	int *row, unsigned char *columnType, int *column);

#else

Widget XmLCreateGrid();
void XmLGridAddColumns();
void XmLGridAddRows();
Boolean XmLGridColumnIsVisible();
Boolean XmLGridCopyPos();
Boolean XmLGridCopySelected();
void XmLGridDeleteAllColumns();
void XmLGridDeleteAllRows();
void XmLGridDeleteColumns();
void XmLGridDeleteRows();
void XmLGridDeselectAllCells();
void XmLGridDeselectAllColumns();
void XmLGridDeselectAllRows();
void XmLGridDeselectCell();
void XmLGridDeselectColumn();
void XmLGridDeselectRow();
int XmLGridEditBegin();
void XmLGridEditCancel();
void XmLGridEditComplete();
XmLGridColumn XmLGridGetColumn();
void XmLGridGetFocus();
XmLGridRow XmLGridGetRow();
Boolean XmLGridPaste();
Boolean XmLGridPastePos();
int XmLGridGetSelectedCellCount();
int XmLGridGetSelectedCells();
int XmLGridGetSelectedColumnCount();
int XmLGridGetSelectedColumns();
int XmLGridGetSelectedRow();
int XmLGridGetSelectedRowCount();
int XmLGridGetSelectedRows();
void XmLGridMoveColumns();
void XmLGridMoveRows();
int XmLGridRead();
int XmLGridReadPos();
void XmLGridRedrawAll();
void XmLGridRedrawCell();
void XmLGridRedrawColumn();
void XmLGridRedrawRow();
void XmLGridReorderColumns();
void XmLGridReorderRows();
int XmLGridRowColumnToXY();
Boolean XmLGridRowIsVisible();
void XmLGridSelectAllCells();
void XmLGridSelectAllColumns();
void XmLGridSelectAllRows();
void XmLGridSelectCell();
void XmLGridSelectColumn();
void XmLGridSelectRow();
int XmLGridSetFocus();
int XmLGridSetStrings();
int XmLGridSetStringsPos();
int XmLGridWrite();
int XmLGridWritePos();
int XmLGridXYToRowColumn();

#endif

#ifdef XmL_CPP
}
#endif
#endif

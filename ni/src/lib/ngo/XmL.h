/*
 *      $Id: XmL.h,v 1.3 1998-02-19 17:12:04 dbrown Exp $
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

#ifndef XmLH
#define XmLH

#ifdef Linux
#define MOTIF20
#endif

#include <Xm/Xm.h>

#if defined(__cplusplus) || defined(c_plusplus)
#define XmL_CPP 1
#endif

#if defined(_STDC_) || defined(__STDC__) || defined(XmL_CPP)
#ifndef _NO_PROTO
#define XmL_ANSIC 1
#endif
#endif

#define XmLVERSION_301

#ifdef XmL_CPP
extern "C" {
#endif

/* shared resources */

#define XmNautoSelect "autoSelect"
#define XmCAutoSelect "AutoSelect"
#define XmNblankBackground "blankBackground"
#define XmCBlankBackground "BlankBackground"
#define XmNdebugLevel "debugLevel"
#define XmCDebugLevel "DebugLevel"

/* Folder resources */

#define XmNacceptResize "acceptResize"
#define XmCAcceptResize "AcceptResize"
#define XmNactiveTab "activeTab"
#define XmCActiveTab "ActiveTab"
#define XmNblankBackgroundPixmap "blankBackgroundPixmap"
#define XmCBlankBackgroundPixmap "BlankBackgroundPixmap"
#define XmNcornerDimension "cornerDimension"
#define XmCCornerDimension "CornerDimension"
#define XmNcornerStyle "cornerStyle"
#define XmCCornerStyle "CornerStyle"
#define XmRCornerStyle "CornerStyle"
#define XmNinactiveBackground "inactiveBackground"
#define XmCInactiveBackground "InactiveBackground"
#define XmNinactiveForeground "inactiveForeground"
#define XmCInactiveForeground "InactiveForeground"
#define XmNpixmapMargin "pixmapMargin"
#define XmCPixmapMargin "PixmapMargin"
#define XmCFolderResizePolicy "FolderResizePolicy"
#define XmRFolderResizePolicy "FolderResizePolicy"
#define XmNrotateWhenLeftRight "rotateWhenLeftRight"
#define XmCRotateWhenLeftRight "RotateWhenLeftRight"
#define XmNtabBarHeight "tabBarHeight"
#define XmCTabBarHeight "TabBarHeight"
#define XmNtabCount "tabCount"
#define XmCTabCount "TabCount"
#define XmNtabPlacement "tabPlacement"
#define XmCTabPlacement "TabPlacement"
#define XmRTabPlacement "TabPlacement"
#define XmNtabsPerRow "tabsPerRow"
#define XmCTabsPerRow "TabsPerRow"
#define XmNtabTranslations "tabTranslations"
#define XmNtabWidgetList "tabWidgetList"

/* Folder Constraint resources */

#define XmNtabFreePixmaps "tabFreePixmaps"
#define XmCTabFreePixmaps "TabFreePixmaps"
#define XmNtabInactivePixmap "tabInactivePixmap"
#define XmCTabInactivePixmap "TabInactivePixmap"
#define XmNtabManagedName "tabManagedName"
#define XmCTabManagedName "TabManagedName"
#define XmNtabManagedWidget "tabManagedWidget"
#define XmCTabManagedWidget "TabManagedWidget"
#define XmNtabPixmap "tabPixmap"
#define XmCTabPixmap "TabPixmap"

/* Folder callbacks */

typedef struct
	{
	int reason;
	XEvent *event;
	int pos;
	int allowActivate;
	} XmLFolderCallbackStruct;

/* Folder defines */

#define XmCORNER_NONE 0
#define XmCORNER_LINE 1
#define XmCORNER_ARC  2

#define XmFOLDER_TOP    0
#define XmFOLDER_LEFT   1
#define XmFOLDER_BOTTOM 2
#define XmFOLDER_RIGHT  3

#define XmRESIZE_STATIC  10
#define XmRESIZE_DYNAMIC 11

/* Grid resources */

#define XmNaddCallback "addCallback"
#define XmNallowColumnHide "allowColumnHide"
#define XmCAllowColumnHide "AllowColumnHide"
#define XmNallowColumnResize "allowColumnResize"
#define XmCAllowColumnResize "AllowColumnResize"
#define XmNallowDragSelected "allowDragSelected"
#define XmCAllowDragSelected "AllowDragSelected"
#define XmNallowDrop "allowDrop"
#define XmCAllowDrop "AllowDrop"
#define XmNallowRowHide "allowRowHide"
#define XmCAllowRowHide "AllowRowHide"
#define XmNallowRowResize "allowRowResize"
#define XmCAllowRowResize "AllowRowResize"
#define XmNbottomFixedCount "bottomFixedCount"
#define XmCBottomFixedCount "BottomFixedCount"
#define XmNbottomFixedMargin "bottomFixedMargin"
#define XmCBottomFixedMargin "BottomFixedMargin"
#define XmNcellDefaults "cellDefaults"
#define XmCCellDefaults "CellDefaults"
#define XmNcellDrawCallback "cellDrawCallback"
#define XmNcellDropCallback "cellDropCallback"
#define XmNcellFocusCallback "cellFocusCallback"
#define XmNcellPasteCallback "cellPasteCallback"
#define XmNdeleteCallback "deleteCallback"
#define XmNdeselectCallback "deselectCallback"
#define XmNeditCallback "editCallback"
#define XmNeditTranslations "editTranslations"
#define XmNfooterColumns "footerColumns"
#define XmCFooterColumns "FooterColumns"
#define XmNfooterRows "footerRows"
#define XmCFooterRows "FooterRows"
#define XmNglobalPixmapHeight "globalPixmapHeight"
#define XmCGlobalPixmapHeight "GlobalPixmapHeight"
#define XmNglobalPixmapWidth "globalPixmapWidth"
#define XmCGlobalPixmapWidth "GlobalPixmapWidth"
#define XmCGridSelectionPolicy "GridSelectionPolicy"
#define XmRGridSelectionPolicy "GridSelectionPolicy"
#define XmRGridSizePolicy "GridSizePolicy"
#define XmNheadingColumns "headingColumns"
#define XmCHeadingColumns "HeadingColumns"
#define XmNheadingRows "headingRows"
#define XmCHeadingRows "HeadingRows"
#define XmNhiddenColumns "hiddenColumns"
#define XmCHiddenColumns "HiddenColumns"
#define XmNhiddenRows "hiddenRows"
#define XmCHiddenRows "HiddenRows"
#define XmNhighlightRowMode "highlightRowMode"
#define XmCHighlightRowMode "HighlightRowMode"
#define XmNhorizontalSizePolicy "horizontalSizePolicy"
#define XmCHorizontalSizePolicy "HorizontalSizePolicy"
#define XmNhsbDisplayPolicy "hsbDisplayPolicy"
#define XmCHsbDisplayPolicy "HsbDisplayPolicy"
#define XmNimmediateDraw "immediateDraw"
#define XmCImmediateDraw "ImmediateDraw"
#define XmNlayoutFrozen "layoutFrozen"
#define XmCLayoutFrozen "LayoutFrozen"
#define XmNleftFixedCount "leftFixedCount"
#define XmCLeftFixedCount "LeftFixedCount"
#define XmNleftFixedMargin "leftFixedMargin"
#define XmCLeftFixedMargin "LeftFixedMargin"
#define XmNrightFixedCount "rightFixedCount"
#define XmCRightFixedCount "RightFixedCount"
#define XmNrightFixedMargin "rightFixedMargin"
#define XmCRightFixedMargin "RightFixedMargin"
#define XmNscrollBarMargin "scrollBarMargin"
#define XmCScrollBarMargin "ScrollBarMargin"
#define XmNscrollCallback "scrollCallback"
#define XmNscrollColumn "scrollColumn"
#define XmCScrollColumn "ScrollColumn"
#define XmNscrollRow "scrollRow"
#define XmCScrollRow "ScrollRow"
#define XmNsimpleHeadings "simpleHeadings"
#define XmCSimpleHeadings "SimpleHeadings"
#define XmNsimpleWidths "simpleWidths"
#define XmCSimpleWidths "SimpleWidths"
#define XmNselectCallback "selectCallback"
#define XmNselectForeground "selectForeground"
#define XmCSelectForeground "SelectForeground"
#define XmNselectBackground "selectBackground"
#define XmCSelectBackground "SelectBackground"
#define XmNshadowRegions "shadowRegions"
#define XmCShadowRegions "ShadowRegions"
#define XmNtextWidget "textWidget"
#define XmCTextWidget "TextWidget"
#define XmNtoggleTopColor "toggleTopColor"
#define XmCToggleTopColor "ToggleTopColor"
#define XmNtoggleBottomColor "toggleBottomColor"
#define XmCToggleBottomColor "ToggleBottomColor"
#define XmNtoggleSize "toggleSize"
#define XmCToggleSize "ToggleSize"
#define XmNtopFixedCount "topFixedCount"
#define XmCTopFixedCount "TopFixedCount"
#define XmNtopFixedMargin "topFixedMargin"
#define XmCTopFixedMargin "TopFixedMargin"
#define XmNtraverseTranslations "traverseTranslations"
#define XmNuseAverageFontWidth "useAverageFontWidth"
#define XmCUseAverageFontWidth "UseAverageFontWidth"
#define XmNverticalSizePolicy "verticalSizePolicy"
#define XmCVerticalSizePolicy "VerticalSizePolicy"
#define XmNvisibleColumns "visibleColumns"
#define XmCVisibleColumns "VisibleColumns"
#define XmNvisibleRows "visibleRows"
#define XmCVisibleRows "VisibleRows"
#define XmNvsbDisplayPolicy "vsbDisplayPolicy"
#define XmCVsbDisplayPolicy "VsbDisplayPolicy"

/* Grid Row/Column/Cell resources */

#define XmNrow "row"
#define XmCGridRow "row"
#define XmNrowHeight "rowHeight"
#define XmCRowHeight "RowHeight"
#define XmNrowPtr "rowPtr"
#define XmNrowRangeEnd "rowRangeEnd"
#define XmCRowRangeEnd "RowRangeEnd"
#define XmNrowRangeStart "rowRangeStart"
#define XmCRowRangeStart "RowRangeStart"
#define XmNrowSizePolicy "rowSizePolicy"
#define XmCRowSizePolicy "RowSizePolicy"
#define XmNrowStep "rowStep"
#define XmCRowStep "RowStep"
#define XmNrowType "rowType"
#define XmCRowType "RowType"
#define XmRRowType "RowType"
#define XmNrowUserData "rowUserData"

#define XmNcolumn "column"
#define XmCGridColumn "Column"
#define XmNcolumnPtr "columnPtr"
#define XmNcolumnRangeEnd "columnRangeEnd"
#define XmCColumnRangeEnd "ColumnRangeEnd"
#define XmNcolumnRangeStart "columnRangeStart"
#define XmCColumnRangeStart "ColumnRangeStart"
#define XmNcolumnSizePolicy "columnSizePolicy"
#define XmCColumnSizePolicy "ColumnSizePolicy"
#define XmNcolumnStep "columnStep"
#define XmCColumnStep "ColumnStep"
#define XmNcolumnType "columnType"
#define XmCColumnType "ColumnType"
#define XmRColumnType "ColumnType"
#define XmNcolumnWidth "columnWidth"
#define XmCColumnWidth "ColumnWidth"
#define XmNcolumnUserData "columnUserData"

#define XmNcellAlignment "cellAlignment"
#define XmCCellAlignment "CellAlignment"
#define XmRCellAlignment "CellAlignment"
#define XmNcellBackground "cellBackground"
#define XmCCellBackground "CellBackground"
#define XmRCellBorderType "CellBorderType"
#define XmNcellBottomBorderType "cellBottomBorderType"
#define XmCCellBottomBorderType "CellBottomBorderType"
#define XmNcellBottomBorderColor "cellBottomBorderColor"
#define XmCCellBottomBorderColor "CellBottomBorderColor"
#define XmNcellColumnSpan "cellColumnSpan"
#define XmCCellColumnSpan "CellColumnSpan"
#define XmNcellEditable "cellEditable"
#define XmCCellEditable "CellEditable"
#define XmNcellForeground "cellForeground"
#define XmCCellForeground "CellForeground"
#define XmNcellFontList "cellFontList"
#define XmCCellFontList "CellFontList"
#define XmNcellLeftBorderType "cellLeftBorderType"
#define XmCCellLeftBorderType "CellLeftBorderType"
#define XmNcellLeftBorderColor "cellLeftBorderColor"
#define XmCCellLeftBorderColor "CellLeftBorderColor"
#define XmNcellMarginBottom "cellMarginBottom"
#define XmCCellMarginBottom "CellMarginBottom"
#define XmNcellMarginLeft "cellMarginLeft"
#define XmCCellMarginLeft "CellMarginLeft"
#define XmNcellMarginRight "cellMarginRight"
#define XmCCellMarginRight "CellMarginRight"
#define XmNcellMarginTop "cellMarginTop"
#define XmCCellMarginTop "CellMarginTop"
#define XmNcellPixmap "cellPixmap"
#define XmCCellPixmap "CellPixmap"
#define XmNcellPixmapMask "cellPixmapMask"
#define XmCCellPixmapMask "CellPixmapMask"
#define XmNcellRightBorderType "cellRightBorderType"
#define XmCCellRightBorderType "CellRightBorderType"
#define XmNcellRightBorderColor "cellRightBorderColor"
#define XmCCellRightBorderColor "CellRightBorderColor"
#define XmNcellRowSpan "cellRowSpan"
#define XmCCellRowSpan "CellRowSpan"
#define XmNcellString "cellString"
#define XmNcellToggleSet "cellToggleSet"
#define XmCCellToggleSet "CellToggleSet"
#define XmNcellTopBorderType "cellTopBorderType"
#define XmCCellTopBorderType "CellTopBorderType"
#define XmNcellTopBorderColor "cellTopBorderColor"
#define XmCCellTopBorderColor "CellTopBorderColor"
#define XmNcellType "cellType"
#define XmCCellType "CellType"
#define XmRCellType "CellType"
#define XmNcellUserData "cellUserData"

/* Grid callbacks */

typedef struct _XmLGridDrawStruct
	{
	GC gc;
	XRectangle *cellRect;
	Dimension topMargin;
	Dimension bottomMargin;
	Dimension leftMargin;
	Dimension rightMargin;
	Pixel foreground;
	Pixel background;
	Pixel selectForeground;
	Pixel selectBackground;
	XmFontList fontList;
	unsigned char alignment;
	Boolean drawSelected;
	int drawFocusType;
	XmStringDirection stringDirection;
	} XmLGridDrawStruct;

typedef struct _XmLGridCallbackStruct
	{
	int reason;
	XEvent *event;
	unsigned char rowType, columnType;
	int row, column;
	XRectangle *clipRect;
	XmLGridDrawStruct *drawInfo;
	void *object;
	} XmLGridCallbackStruct;

#define XmCR_ADD_ROW         900
#define XmCR_ADD_COLUMN      901
#define XmCR_ADD_CELL        902
#define XmCR_CELL_DRAW       903
#define XmCR_CELL_DROP       904
#define XmCR_CELL_FOCUS_IN   905
#define XmCR_CELL_FOCUS_OUT  906
#define XmCR_CELL_PASTE      907
#define XmCR_CONF_TEXT       908
#define XmCR_PREF_WIDTH      909
#define XmCR_DELETE_ROW      910
#define XmCR_DELETE_COLUMN   911
#define XmCR_DELETE_CELL     912
#define XmCR_EDIT_BEGIN      913
#define XmCR_EDIT_INSERT     914
#define XmCR_EDIT_CANCEL     915
#define XmCR_EDIT_COMPLETE   916
#define XmCR_FREE_VALUE      917
#define XmCR_RESIZE_ROW      918
#define XmCR_RESIZE_COLUMN   919
#define XmCR_PREF_HEIGHT     920
#define XmCR_SCROLL_ROW      921
#define XmCR_SCROLL_COLUMN   922
#define XmCR_SELECT_CELL     923
#define XmCR_SELECT_COLUMN   924
#define XmCR_SELECT_ROW      925
#define XmCR_DESELECT_CELL   926
#define XmCR_DESELECT_COLUMN 927
#define XmCR_DESELECT_ROW    928

/* Grid defines */

#define XmCONTENT      0
#define XmHEADING      1
#define XmFOOTER       2
#define XmALL_TYPES    3
#define XmINVALID_TYPE 4

#define XmICON_CELL   0
#define XmPIXMAP_CELL 1
#define XmSTRING_CELL 2

#define XmBORDER_NONE 0
#define XmBORDER_LINE 1
#define XmBORDER_DASH 2

#define XmFORMAT_DELIMITED 1
#define XmFORMAT_XL        2
#define XmFORMAT_PAD       3
#define XmFORMAT_PASTE     4
#define XmFORMAT_DROP      5

#define XmSELECT_NONE         1
#define XmSELECT_SINGLE_ROW   2
#define XmSELECT_BROWSE_ROW   3
#define XmSELECT_MULTIPLE_ROW 4
#define XmSELECT_CELL         5

#define XmDRAW_FOCUS_NONE  1
#define XmDRAW_FOCUS_CELL  2
#define XmDRAW_FOCUS_LEFT  3
#define XmDRAW_FOCUS_MID   4
#define XmDRAW_FOCUS_RIGHT 5

#define XmTRAVERSE_EXTEND_DOWN  20
#define XmTRAVERSE_EXTEND_LEFT  21
#define XmTRAVERSE_EXTEND_RIGHT 22
#define XmTRAVERSE_EXTEND_UP    23
#define XmTRAVERSE_PAGE_DOWN    24
#define XmTRAVERSE_PAGE_LEFT    25
#define XmTRAVERSE_PAGE_RIGHT   26
#define XmTRAVERSE_PAGE_UP      27
#define XmTRAVERSE_TO_BOTTOM    28
#define XmTRAVERSE_TO_TOP       29

#define XmALIGNMENT_LEFT         0
#ifndef XmALIGNMENT_CENTER
#define XmALIGNMENT_CENTER       1
#endif
#define XmALIGNMENT_RIGHT        2
#define XmALIGNMENT_TOP_LEFT     3
#define XmALIGNMENT_TOP          4
#define XmALIGNMENT_TOP_RIGHT    5
#define XmALIGNMENT_BOTTOM_LEFT  6
#define XmALIGNMENT_BOTTOM       7
#define XmALIGNMENT_BOTTOM_RIGHT 8

/* Progress resources */

#define XmNcompleteValue "completeValue"
#define XmCCompleteValue "CompleteValue"
#define XmNnumBoxes "numBoxes"
#define XmCNumBoxes "NumBoxes"
#define XmNmeterStyle "meterStyle"
#define XmCMeterStyle "MeterStyle"
#define XmRMeterStyle "MeterStyle"
#define XmNshowPercentage "showPercentage"
#define XmCShowPercentage "ShowPercentage"
#define XmNshowTime "showTime"
#define XmCShowTime "ShowTime"

/* Progress defines */

#define XmMETER_BAR 0
#define XmMETER_BOXES 1

/* Tree resources */

#define XmNcollapseCallback "collapseCallback"
#define XmNconnectingLineColor "connectingLineColor"
#define XmCConnectingLineColor "ConnectingLineColor"
#define XmNexpandCallback "expandCallback"
#define XmNlevelSpacing "levelSpacing"
#define XmCLevelSpacing "LevelSpacing"
#define XmNplusMinusColor "plusMinusColor"
#define XmCPlusMinusColor "PlusMinusColor"
#define XmNrowExpands "rowExpands"
#define XmCRowExpands "RowExpands"
#define XmNrowIsExpanded "rowIsExpanded"
#define XmCRowIsExpanded "RowIsExpanded"
#define XmNrowLevel "rowLevel"
#define XmCRowLevel "RowLevel"

/* Tree callbacks */

typedef struct
	{
	int level;
	Boolean expands;
	Boolean isExpanded;
	Pixmap pixmap, pixmask;
	XmString string;
	} XmLTreeRowDefinition;

#define XmCR_COLLAPSE_ROW 950
#define XmCR_EXPAND_ROW   951

/* Backwards compatibility */

#ifdef XmLBACKWARDS_COMPATIBILITY

#define XmNfooterColumnCount "footerColumns"
#define XmNfooterRowCount "footerRows"
#define XmNheadingColumnCount "headingColumns"
#define XmNheadingRowCount "headingRows"
#define XmNcellBottomBorderPixel "cellBottomBorderColor"
#define XmCCellBottomBorderPixel "CellBottomBorderColor"
#define XmNcellLeftBorderPixel "cellLeftBorderColor"
#define XmCCellLeftBorderPixel "CellLeftBorderColor"
#define XmNcellRightBorderPixel "cellRightBorderColor"
#define XmCCellRightBorderPixel "CellRightBorderColor"
#define XmNcellTopBorderPixel "cellTopBorderColor"
#define XmCCellTopBorderPixel "CellTopBorderColor"

#define XmTEXT_CELL  250
#define XmLABEL_CELL 251

typedef void XmLCGridRow;
typedef void XmLCGridColumn;
typedef void XmLCGridCell;

#endif

/* Utility defines */

#define XmDRAWNB_ARROW       0
#define XmDRAWNB_ARROWLINE   1
#define XmDRAWNB_DOUBLEARROW 2
#define XmDRAWNB_SQUARE      3
#define XmDRAWNB_DOUBLEBAR   4
#define XmDRAWNB_STRING      5

#define XmDRAWNB_RIGHT 0
#define XmDRAWNB_LEFT  1
#define XmDRAWNB_UP    2
#define XmDRAWNB_DOWN  3

#define XmSTRING_RIGHT 0
#define XmSTRING_LEFT  1
#define XmSTRING_UP    2
#define XmSTRING_DOWN  3

enum { XmLRectInside, XmLRectOutside, XmLRectPartial };

typedef struct
	{
	int pos;
	} XmLArrayItem;

typedef struct _XmLArrayRec *XmLArray;

typedef struct
	{
	char *name;
	unsigned char value;
	} XmLStringToUCharMap;

/* Utility functions */

#ifdef XmL_ANSIC

typedef int (*XmLSortCompareFunc)(void *userData, void *l, void *r);
typedef int (*XmLArrayCompareFunc)(void *, void **, void **);

XmLArray XmLArrayNew(char autonumber, char growFast);
void XmLArrayFree(XmLArray array);
void XmLArrayAdd(XmLArray array, int pos, int count);
int XmLArrayDel(XmLArray array, int pos, int count);
int XmLArraySet(XmLArray array, int pos, void *item);
void *XmLArrayGet(XmLArray array, int pos);
int XmLArrayGetCount(XmLArray array);
int XmLArrayMove(XmLArray array, int newPos, int pos, int count);
int XmLArrayReorder(XmLArray array, int *newPositions,
	int pos, int count);
int XmLArraySort(XmLArray array, XmLArrayCompareFunc compare,
	void *userData, int pos, int count);
Boolean XmLCvtStringToUChar(Display *dpy, char *resname,
	XmLStringToUCharMap *map, XrmValuePtr fromVal, XrmValuePtr toVal);
int XmLDateDaysInMonth(int m, int y);
int XmLDateWeekDay(int m, int d, int y);
void XmLDrawnButtonSetType(Widget w, int drawnType, int drawnDir);
void XmLDrawToggle(Widget w, Boolean state, Dimension size,
	unsigned char alignment, GC gc, Pixel backgroundColor,
	Pixel topColor, Pixel bottomColor, Pixel checkColor,
	XRectangle *rect, XRectangle *clipRect);
XmFontList XmLFontListCopyDefault(Widget widget);
void XmLFontListGetDimensions(XmFontList fontList, short *width,
	short *height, Boolean useAverageWidth);
void XmLInitialize();
int XmLMessageBox(Widget w, char *string, Boolean okOnly);
void XmLPixmapDraw(Widget w, Pixmap pixmap, Pixmap pixmask,
	int pixmapWidth, int pixmapHeight, unsigned char alignment,
	GC gc, XRectangle *rect, XRectangle *clipRect);
int XmLRectIntersect(XRectangle *r1, XRectangle *r2);
Widget XmLShellOfWidget(Widget w);
void XmLSort(void *base, int numItems, unsigned int itemSize,
	XmLSortCompareFunc, void *userData);
void XmLStringDraw(Widget w, XmString string, XmStringDirection stringDir,
	XmFontList fontList, unsigned char alignment, GC gc,
	XRectangle *rect, XRectangle *clipRect);
void XmLStringDrawDirection(Display *dpy, Window win, XmFontList fontlist,
	XmString string, GC gc, int x, int y, Dimension width,
	unsigned char alignment, unsigned char layout_direction,
	unsigned char drawing_direction);
void XmLWarning(Widget w, char *msg);

#else

typedef int (*XmLSortCompareFunc)();
typedef int (*XmLArrayCompareFunc)();

XmLArray XmLArrayNew();
void XmLArrayFree();
void XmLArrayAdd();
int XmLArrayDel();
int XmLArraySet();
void *XmLArrayGet();
int XmLArrayGetCount();
int XmLArrayMove();
int XmLArrayReorder();
int XmLArraySort();
Boolean XmLCvtStringToUChar();
int XmLDateDaysInMonth();
int XmLDateWeekDay();
void XmLDrawnButtonSetType();
void XmLDrawToggle();
XmFontList XmLFontListCopyDefault();
void XmLFontListGetDimensions();
void XmLInitialize();
int XmLMessageBox();
void XmLPixmapDraw();
int XmLRectIntersect();
Widget XmLShellOfWidget();
void XmLSort();
void XmLStringDraw();
void XmLStringDrawDirection();
void XmLWarning();

#endif

#ifdef XmL_CPP
}
#endif
#endif

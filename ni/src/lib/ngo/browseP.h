/*
 *      $Id: browseP.h,v 1.7 1999-03-05 01:02:34 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		browseP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar  4 12:38:45 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_BROWSEP_H_
#define	_NG_BROWSEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/browse.h>
#include <ncarg/ngo/vcrcontrol.h>
#include <ncarg/ngo/varmenus.h>
#include <ncarg/ngo/htmlviewI.h>
#include <ncarg/ngo/XmL.h>

#define DEBUG_ENTRY 1
#define DEBUG_FOLDER 1 << 1
/*
#define DEBUG_DATABROWSER DEBUG_ENTRY | DEBUG_FOLDER
*/
#define DEBUG_DATABROWSER 0
#define brMAX_PANES 16

/*
 * Each page points to the tab that is currently connected to it. Because
 * of the way the XmLTab widget works, tabs, once created always stay in
 * the same order with respect to each other. "Shuffling" of tabs is
 * accomplished by reassigning them to the current correct page. Only
 * tabs at the end of the list, not associated with a page currently, 
 * ever become unmanaged. Each pane contains a list of its tabs in an
 * XmLArray.
 */
         
typedef struct _brTab
{
        Widget			tab;
        NhlBoolean		managed;
} brTab, *brTabList;       

/*
 * The basic structure used to identify a page instance. It contains 
 * non-type specific data only. It contains a pointer to a page data
 * structure instance, brPageData. Somewhat like the tab list, the pane 
 * contains lists of the brPageData structures (separate lists for
 * each page type). These can be reused as pages of each type come and
 * go. 
 */

typedef struct _brPage 
{
        NgGO			go;
        NgPageId		id;
	brPageType		type;
        NrmQuark		qvar;
        NrmQuark		qfile;
        brTab			*tab;
	struct _brPageData	*pdata;
} brPage, *brPageList;
	
/*
 * Each pane contains a list of active pages, plus a list of the tabs, and
 * lists of brPageData structures (one for each page type). It also contains
 * references to the widgets that make up the pane. It keeps track of the
 * state of the pane including which page is the "active" page.
 */

typedef struct _brPane 
{
        NgGO			go;
        NhlBoolean		managed;
        Widget			topform;
        Widget			scroller;
        Widget			clip_window;
        Widget			hsb;
        Widget			vsb;
        Widget			form;
        NhlBoolean		has_folder;
        Widget			folder;
        int			last_pagecount;
        int			tabcount;
        XmLArray		tablist;
        Dimension		max_tab_xtnt;
        int			pagecount;
        XmLArray		pagelist;
	struct _brPageData	*fileref_pages;
	struct _brPageData	*var_pages;
        struct _brPageData	*hlu_pages;
	struct _brPageData	*html_pages;
        NhlBoolean		changed;
        int			remove_pos;
        int			active_pos;
        brPage			*active_page;
	int			htmlview_count;
	XmLArray		htmlview_list;
} brPane;

/*
 * Function pointers for page operations
 */

typedef void (*DestroyPageFunc) (
	NhlPointer data
);

typedef void (*DeactivatePageFunc) (
	brPage	*page
);

typedef void (*PageFocusNotify) (
        brPage *page,
        NhlBoolean in /* False if FocusOut, True if FocusIn */
);

typedef void (*PageInputNotify) (
        brPage *page,
        brPageType output_page_type,
 	NhlPointer output_data
);

typedef NhlPointer (*PublicPageData) (
        brPage *page
);

typedef NhlErrorTypes (*UpdatePage) (
        brPage *page
);

typedef NhlErrorTypes (*PageMessageNotify) (
        brPage *page
);

/*
 * brPageData structures contain pointers to functions that implement 
 * each page type. Once these have been initialized once, they are preserved
 * on the page data lists of each pane, when a particular page goes away.
 */

typedef struct _brPageData 
{
	struct _brPageData	*next;
        brPane			*pane;
  	NhlBoolean		in_use;
        Widget			form;
        NclApiDataList		*dl;
        NhlPointer		type_rec;
	DestroyPageFunc		destroy_page;
	AdjustPageGeoFunc	adjust_page_geo;
	DeactivatePageFunc	deactivate_page;
        PageFocusNotify		page_focus_notify;
        PublicPageData		public_page_data;
        UpdatePage		update_page;
	PageMessageNotify	page_message_notify;
} brPageData, *brPageDataList;

/*
 * this structure is not currently used
 */

typedef struct _brHistory
{
        NrmQuark		*items;
        int			list_size;
        int			start;
        int			end;
        NgVcrControl		vcr;
        Widget			text;
} brHistory;

/*
 * overall control of the panes in a browser instance.
 */

typedef struct _brPaneControl
{
        brPane		*panes[brMAX_PANES];
        int		alloc_count;
        int		current_count;
        int		current_ix;
        Widget		text;
        NgVcrControl	vcr;
        Widget		pane_toggle;
        brPane		*focus_pane;
        int		focus_pos;
} brPaneControl;

typedef struct _NgBrowseClassRec *NgBrowseClass;
typedef struct _NgBrowseRec *NgBrowse;

/*
 * the browser instance part
 */

typedef struct _NgBrowsePart {
/* required fields */
	int		nsid;
	NhlBoolean	mapped;
        Widget		paned_window;
        NgVarMenus	vmenus;
        brHistory	history;
        brPaneControl	pane_ctrl;
} NgBrowsePart;

typedef struct _NgBrowseRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgBrowsePart	browse;
} NgBrowseRec;

typedef struct _NgBrowseClassPart {
	XmLArray	hidden_page_state;
	XmLArray	page_messages;
} NgBrowseClassPart;

typedef struct _NgBrowseClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgBrowseClassPart	browse_class;
} NgBrowseClassRec;

extern void
NgSetFolderSize(
	brPane	*pane,
        Dimension page_width,
        Dimension page_height,
        Dimension *avail_width,
        Dimension *avail_height
        );

extern brPane *_NgGetPaneOfPage(
        int		goid,
        NgPageId	page_id
        );

extern brPage *_NgGetPageRef(
        int		goid,
        NgPageId	page_id
        );

extern void _NgGetPaneVisibleArea(
        NhlLayer go,
        brPane *pane,
        XRectangle *rect
        );


extern NgBrowseClassRec	NgbrowseClassRec;

#endif	/* _NG_BROWSEP_H_ */

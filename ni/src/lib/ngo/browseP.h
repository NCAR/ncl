/*
 *      $Id: browseP.h,v 1.1 1997-06-04 18:08:23 dbrown Exp $
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
#include <XmL/XmL.h>

#define DEBUG_ENTRY 1
#define DEBUG_FOLDER 1 << 1
/*
#define DEBUG_DATABROWSER DEBUG_ENTRY | DEBUG_FOLDER
*/
#define DEBUG_DATABROWSER 0
#define brMAX_PANES 16

typedef struct _brTab
{
	struct _brTab		*next;
        Widget			tab;
        NhlBoolean		managed;
} brTab, *brTabList;       

typedef struct _brPage 
{
        NgGO			go;
	brPageType		type;
        NrmQuark		qvar;
        NrmQuark		qfile;
        brTab			*tab;
	struct _brPageData	*pdata;
} brPage, *brPageList;
	
typedef struct _brPane 
{
        NgGO			go;
        NhlBoolean		managed;
        Widget			topform;
        Widget			scroller;
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
        NhlBoolean		changed;
        int			remove_pos;
        int			active_pos;
} brPane;

typedef void (*DestroyPageFunc) (
	NhlPointer data
);

typedef void (*AdjustPageGeoFunc) (
	NhlPointer data
);

typedef void (*DeactivatePageFunc) (
	brPage	*page
);

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
} brPageData, *brPageDataList;

typedef struct _brHistory
{
        NrmQuark		*items;
        int			list_size;
        int			start;
        int			end;
        NgVcrControl		vcr;
        Widget			text;
} brHistory;

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
	int		foo;
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

extern NgBrowseClassRec	NgbrowseClassRec;

#endif	/* _NG_BROWSEP_H_ */

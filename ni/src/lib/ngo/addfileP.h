/*
 *      $Id: addfileP.h,v 1.3 1997-03-04 02:53:50 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		addfileP.h
 *
 *	Authors:	Jeff W. Boote, David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Oct 14 16:39:29 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_ADDFILEP_H_
#define	_NG_ADDFILEP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/addfile.h>
#include <ncarg/ngo/diminfogrid.h>
#include <ncarg/ngo/attrinfogrid.h>
#include <ncarg/ngo/vcrcontrol.h>


typedef struct _NgAddFileClassRec *NgAddFileClass;
typedef struct _NgAddFileRec *NgAddFile;

/* debug */

#define DEBUG_ADDFILE 0

/* sort options defines */

#define ASCII_SORT 0
#define DIM_SORT 1
#define SIZE_SORT 2
#define NO_SORT 3

/* popup types */

#define DIM_INFO_POPUP 0
#define VAR_ATTRS_POPUP 1
#define GLOBAL_ATTRS_POPUP 2

typedef struct _NgafDimInfoRec 
{
	struct _NgafDimInfoRec	*next;
        NrmQuark		qvar;
        NclApiDataList		*dl;
 	NclApiVarInfoRec	*vinfo;
        Widget 			popup;
        Widget			frame;
        NgDimInfoGrid		grid;
        int			pos;
        NhlBoolean		up;
} NgafDimInfoRec;

typedef struct _NgafAttrInfoRec 
{
	struct _NgafAttrInfoRec	*next;
        NrmQuark		qvar;
        NclApiDataList		*dl;
        Widget 			popup;
        Widget			frame;
	Widget			list;
        NgAttrInfoGrid		grid;
        int			pos;
        NhlBoolean		up;
} NgafAttrInfoRec;

        
typedef struct _NgAddFilePart {
/* required fields */
	int	foo;

/* private fields */
	Widget	vname;
	Widget	rw_optmenu;
        Widget	write_label;
        Widget	read_label;
        Widget	info_optmenu;
        Widget	info_frame;
         
        Widget          filtertext;
        Widget          fselect_box;
        Widget          selecttext;
        Widget		listform;
        Widget          filelist;
        Widget          dirlist;
        Widget          filter_button;
        Widget          vlist;
        Widget          fsize_label;
        Widget          fdate_label;
        Widget          create;
        Widget          change;
        String          dirspec;
        NhlBoolean      readable;
        NhlBoolean      writable;
        XmString        dirmask;
        XmString        pattern;
        XmString        dir;
	NclApiFileInfoRec *finfo;
	NclApiDataList  *dl;
        NgafDimInfoRec	*dim_rec;
        NgafAttrInfoRec	*attr_rec;
        NhlBoolean	vlist_empty;
        int		var_sort_option;
        NhlBoolean	mapped;
        Dimension	user_dir_width;
        NhlBoolean	user_configure;
        NhlBoolean	file_changed;
        NgVcrControl	vcrp;
        XtIntervalId	list_timer_id;
        NhlBoolean	list_timer_set;
        NhlBoolean	list_forward;
        Widget		cur_list;
        int		cur_popup_type;
        NhlBoolean	popped_up;
} NgAddFilePart;

typedef struct _NgAddFileRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgAddFilePart	addfile;
} NgAddFileRec;

typedef struct _NgAddFileClassPart {
	int		foo;
} NgAddFileClassPart;

typedef struct _NgAddFileClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgAddFileClassPart	addfile_class;
} NgAddFileClassRec;

extern NgAddFileClassRec	NgaddFileClassRec;

#endif	/* _NG_ADDFILEP_H_ */

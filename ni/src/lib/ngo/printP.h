/*
 *      $Id: printP.h,v 1.2 1998-08-26 05:16:13 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		printP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 20 14:49:40 MDT 1998
 *
 *	Description:	
 */
#ifndef	_NG_PRINTP_H_
#define	_NG_PRINTP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/print.h>

#define DEBUG_PRINT	0

/* output destination options */
#define prPRINTER	0
#define prFILE		1

/* output file format options */

#define prPS		0
#define prEPS		1
#define prEPSI		2
#define prNCGM		3
#define prGIF		4
#define prPNG		5

#define prFILETYPE_COUNT	6

/* orientation options */
#define prPORTRAIT	0
#define prLANDSCAPE	1
#define prAUTO_ORIENT	2

/* paper size options */

#define prLETTER	0
#define prLEGAL		1
#define prA4		2
#define prA3		3

typedef struct _NgPrintClassRec *NgPrintClass;
typedef struct _NgPrintRec *NgPrint;

typedef struct _NgPrintPart {
/* required fields */
        NhlBoolean update_dialog;
/* private fields */

	Widget		print_tgl;
	Widget		file_tgl;
        String		print_command;
	Widget		print_text_lbl;
	Widget		print_text;
	
	Widget		file_text_lbl;
  	Widget		file_text;
	Widget		file_type_lbl;
	Widget		file_type_optmenu;
	Widget		*file_type_pbs;
	Widget		overwrite_tgl;
	
  /* layout options */
	Widget		orient_lbl;
	Widget		orient_optmenu;
	Widget		*orient_pbs;
	Widget		plot_bounds_lbl;
	Widget		maximize_bb_tgl;
	Widget		full_viewspace_tgl;
	Widget		paper_type_lbl;
	Widget		paper_type_optmenu;
	Widget		*paper_type_pbs;
	Widget		resolution_lbl;
	Widget		resolution_text;

  /* choosing the workstation */
        String		workstation;
	NrmQuark	*qwks;
        Widget		form_parent;
	Widget		wks_menu;
	Widget		wks_optmenu;
	int		selected_work_pos;
        int		selected_work_id;
	int 		wks_count;
	int		wks_alloc;
	Widget		*wks_pb;
	int		wks_pb_count;
	int		wks_pb_alloc;

  /* choosing the views */
	NrmQuark	*qviews;
	int		*view_ids;
	int 		view_count;
	int		view_alloc;
        int		work_view_count;
	Widget		plot_views_lbl;
        Widget		all_tgl;
        Widget		selected_tgl;
        Widget		view_list;

  /* dialog currently posted ? */

        NhlBoolean	up;

} NgPrintPart;

typedef struct _NgPrintRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgPrintPart	print;
} NgPrintRec;

typedef struct _NgPrintClassPart {
	int		foo;
} NgPrintClassPart;

typedef struct _NgPrintClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgPrintClassPart	print_class;
} NgPrintClassRec;

extern NgPrintClassRec	NgprintClassRec;

#endif	/* _NG_PRINTP_H_ */

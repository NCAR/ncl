/*
 *      $Id: ncleditP.h,v 1.4 1997-01-03 01:38:01 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ncleditP.h
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 19 13:35:57 MDT 1996
 *
 *	Description:	
 */
#ifndef	_NG_NCLEDITP_H_
#define	_NG_NCLEDITP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/ncledit.h>

typedef struct _NgNclEditClassRec *NgNclEditClass;
typedef struct _NgNclEditRec *NgNclEdit;

typedef struct _NgNclEditPart {
/* required fields */
	int		nsid;

	char		*rmsg;
	char		*emsg;

/* private fields */

	XmString	cstr;
	XmString	rstr;
	XmString	estr;
	XmString	nstr;

	NhlBoolean	edit;
	NhlBoolean	my_cmd;
	NhlBoolean	print;
	char		*more_cmds;
	XmTextPosition	submit_pos;
	XmTextPosition	reset_pos;
	int		line;

	NhlBoolean	my_focus;

	Widget		holist;
	Widget		vlist;
	Widget		flist;
	Widget		fulist;
	Widget		prompt_text;
	Widget		text;
	Widget		reset;
	Widget		iform;
	Widget		scroll;
	Widget		ilabel;
	Widget		vsbar;

	Window		prompt_win;
	XmFontList	prompt_font;
	GC		high_gc;
	Dimension	prompt_margin;
	Dimension	prompt_width;
	Dimension	text_height;
	Dimension	text_width;
	Dimension	shadow_thickness;
	int		left_offset;
	int		top_offset;
	NhlBoolean	high_drawn;
	Pixel		high_color;
	Pixel		back_color;

	_NhlCB		submitcb;
	_NhlCB		promptcb;
	_NhlCB		resetcb;
	_NhlCB		outputcb;
	_NhlCB		erroutputcb;

} NgNclEditPart;

typedef struct _NgNclEditRec {
	NhlObjLayerPart	base;
	NgGOPart	go;
	NgNclEditPart	ncledit;
} NgNclEditRec;

typedef struct _NgNclEditClassPart {
	int		foo;
} NgNclEditClassPart;

typedef struct _NgNclEditClassRec {
	NhlObjClassPart		base_class;
	NgGOClassPart		go_class;
	NgNclEditClassPart	ncledit_class;
} NgNclEditClassRec;

extern NgNclEditClassRec	NgnclEditClassRec;

#endif	/* _NG_NCLEDITP_H_ */

/*
 *      $Id: htmlviewP.h,v 1.2 1998-09-24 19:54:10 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlviewP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Nov  3 17:35:23 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_HTMLVIEWP_H_
#define	_NG_HTMLVIEWP_H_

#include <ncarg/ngo/goP.h>
#include <ncarg/ngo/htmlviewI.h>
#include <XmHTML/XmHTMLP.h>
#include <XmHTML/XmHTMLI.h>

#define DEBUG_HTML 0
#define BLOCK_ALLOC_SIZE 32

typedef struct __hvHtmlObject
{
        struct __hvHtmlObject	*next;
        int			num_named_anchors;
        int			anchor_words;
        XmHTMLObject		*elements;
        XmHTMLWord		*anchors;
        XmHTMLObjectTable	*named_anchors;
        XmHTMLAnchor		*anchor_data;
        XmHTMLObjectTable	*formatted;
	XmHTMLImage		*images;
        NrmQuark		locator;
} _hvHtmlObject;
        
typedef struct _NgHtmlViewRec
{
	NgHtmlView   	public;
	Boolean		not_yet_mapped;
        NgGO		go;
        NgHtmlViewType	type;
        Widget		parent;
        Widget		scrwin;
        Widget		swvsb;
        Widget		html;
        Widget		vsb;
        Widget		work;
        NhlString	urlbase;
        NhlString	filebase;
        NrmQuark	cur_locator;
	_hvHtmlObject	*html_objects;
} NgHtmlViewRec;

#endif	/* _NG_HTMLVIEWP_H_ */


/*
 *      $Id: htmlview.h,v 1.3 2000-03-21 02:35:43 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlview.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Nov  3 17:35:23 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_HTMLVIEW_H
#define	_NG_HTMLVIEW_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>

/*
 * Public api
 */

typedef enum _NgHtmlViewType
{
        _hbGENERIC, _hbHLURES, _hbNCLFUNC
} NgHtmlViewType;

typedef int HtmlViewId;
#define NgNoHtmlView 0

extern HtmlViewId NgGetHtmlView(
        int		goid,
        NgPageId	page_id,
        NgHtmlViewType	type,
        HtmlViewId	html_view_id,
        NrmQuark	locator,
        NrmQuark	name,
        Widget		requestor,
        Position	requestor_x,
        Position	requestor_y
        );

extern NhlErrorTypes NgSetHtmlViewPosition(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	html_view_id,
        Widget		requestor,
        Position	requestor_x,
        Position	requestor_y
        );

extern NhlErrorTypes NgGetHtmlViewSize(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id,
        Dimension	avail_width,
        NhlBoolean	resize_width,
        Dimension	max_height,
        Dimension	*height,
        Dimension	*width
        );

extern NhlErrorTypes NgShowHtmlView(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id
        );

extern NhlErrorTypes NgReleaseHtmlView(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id
        );

extern NhlErrorTypes NgReleasePageHtmlViews(
        int		goid,
        NgPageId	page_id
        );

extern void NgRefreshHtmlView(
        int		goid,
        NgPageId	page_id,
        HtmlViewId	htmlview_id
        );

#endif	/* _NG_HTMLVIEW_H */

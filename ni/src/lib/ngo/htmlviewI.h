/*
 *      $Id: htmlviewI.h,v 1.1 1998-01-08 01:19:27 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlviewI.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Nov  3 17:35:23 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_HTMLVIEWI_H
#define	_NG_HTMLVIEWI_H

#include <ncarg/ngo/htmlview.h>
#include <ncarg/ngo/browseP.h>

/*
 * Interface api (used by the browser)
 */

typedef struct _NgHtmlView
{
        Widget		htmlview;
        HtmlViewId	id;
        NrmQuark	locator;
        NrmQuark	name;
        Boolean 	managed;
        Boolean 	mapped;
        NgPageId	user_page_id;
        Position   	x,y;
        Dimension  	width,height;
        int        	max_height;
        NhlBoolean	resize_width;
} NgHtmlView;

NgHtmlView *
_NgCreateHtmlView(
        int		goid,
        Widget		parent,
        NgHtmlViewType	type
        );

NhlErrorTypes
_NgSetHtmlContent(
        NgHtmlView	*htmlview,
        NrmQuark	locator,
        NrmQuark	name
        );

NhlErrorTypes
_NgGetHtmlViewSize(
        NgHtmlView	*htmlview,
        Dimension	max_height,
        Dimension	avail_width,
        NhlBoolean	resize_width,
        Dimension	*height,
        Dimension	*width
        );
        
void
_NgMapHtmlView(
        NgHtmlView	*htmlview
        );

void
_NgUnmapHtmlView(
        NgHtmlView	*htmlview
        );

void
_NgDestroyHtmlView(
        NgHtmlView	*htmlview
        );

#endif	/* _NG_HTMLVIEWI_H */

/*
 *      $Id: htmlpageP.h,v 1.1 1999-03-05 16:53:30 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlpageP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon May  5 17:41:15 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_HTMLPAGEP_H_
#define	_NG_HTMLPAGEP_H_

#include <ncarg/ngo/htmlpage.h>
#include <ncarg/ngo/htmlview.h>
#include <ncarg/ngo/browseP.h>

#define DEBUG_HTMLPAGE 0

typedef struct _brHtmlPageRec 
{
	NgGO		go;
	int		page_id;
	HtmlViewId	id;
	Dimension	height;
	Dimension	width;
	NrmQuark	qurl;
} brHtmlPageRec;

extern brPageData *
NgGetHtmlPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state
        );

extern void 
NgRefreshHtmlPage
(
	brPage		*page
);

#endif	/* _NG_HTMLPAGEP_H_ */





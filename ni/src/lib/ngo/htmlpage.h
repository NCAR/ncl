/*
 *      $Id: htmlpage.h,v 1.1 1999-03-05 16:53:30 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		htmlpage.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Nov  3 17:35:23 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_HTMLPAGE_H
#define	_NG_HTMLPAGE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/browse.h>

extern void
NgHtmlPageContentNotify
(
	int		go_id,
	int 		page_id,
	NrmQuark	qurl
);

#endif	/* _NG_HTMLPAGE_H */

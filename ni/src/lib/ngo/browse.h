/*
 *      $Id: browse.h,v 1.2 1997-06-20 16:35:28 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		browse.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar  4 12:38:45 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_BROWSE_H
#define	_NG_BROWSE_H

#include <ncarg/ngo/go.h>
#include <ncarg/hlu/NresDB.h>

extern NhlClass NgbrowseClass;

/*
 * Public api
 */

typedef enum _brPageType 
{
        _brNULL, _brREGVAR, _brFILEREF, _brFILEVAR, _brHLUVAR
} brPageType;

#define NgNoPage 0
typedef int NgPageId;

extern NgPageId NgOpenPage(
        int		goid,
        brPageType	type,
        NrmQuark	*qname,
        int		qcount
        );

extern void NgPageOutputNotify(
        int		goid,
        NgPageId	page_id,
        brPageType	output_page_type,
        NhlPointer	output_data
        );

extern NhlPointer NgPageData(
        int		goid,
        NgPageId	page_id
        );

extern NhlErrorTypes NgUpdatePage(
        int		goid,
        NgPageId	page_id
        );

#endif	/* _NG_BROWSE_H */

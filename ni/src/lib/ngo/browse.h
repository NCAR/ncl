/*
 *      $Id: browse.h,v 1.1 1997-06-04 18:08:22 dbrown Exp $
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
        _brREGVAR, _brFILEREF, _brFILEVAR
} brPageType;

extern NhlErrorTypes NgBrowseOpenPage(
        int		goid,
        brPageType	type,
        NrmQuark	*qname,
        int		qcount
        );


        
#endif	/* _NG_BROWSE_H */

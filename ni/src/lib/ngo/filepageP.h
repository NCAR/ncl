/*
 *      $Id: filepageP.h,v 1.3 1999-02-23 03:56:47 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		filepageP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon May  5 17:41:15 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_FILEPAGEP_H_
#define	_NG_FILEPAGEP_H_

#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/filetree.h>

#define DEBUG_FILEPAGE 0

typedef struct _brFileRefPageRec 
{
        NgFileTree		*filetree;
} brFileRefPageRec;

extern brPageData *
NgGetFileRefPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page,
	NgPageSaveState save_state
        );


#endif	/* _NG_FILEPAGEP_H_ */





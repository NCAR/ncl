/*
 *      $Id: hlupageP.h,v 1.1 1997-06-20 16:35:33 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlupageP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun  9 21:02:27 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_HLUPAGEP_H_
#define	_NG_HLUPAGEP_H_

#include <ncarg/ngo/browseP.h>
#include <ncarg/ngo/varpage.h>

#define DEBUG_HLUPAGE 0

typedef struct _hluData 
{
        NhlString	name;
        Widget		frame;
        Widget		form;
        int		n_items;
        Widget		*labels;
        Widget		*textfields;
} hluData;
        
typedef struct _brHluPageRec 
{
        NhlClass	class;
        Widget		create_update;
        hluData		*data;
        int		var_page_data_count;
        NgVarPageOutput	*var_page_data;
} brHluPageRec;

extern brPageData *
NgGetHluPage(
	NgGO		go,
        brPane		*pane,
	brPage		*page,
        brPage		*copy_page
        );


#endif	/* _NG_HLUPAGEP_H_ */





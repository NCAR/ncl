/*
 *      $Id: hlupage.h,v 1.4 1997-10-03 20:08:05 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		hlupage.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jun 18 17:11:05 MDT 1997
 *
 *	Description:	
 */
#ifndef	_NG_HLUPAGE_H
#define	_NG_HLUPAGE_H

#include <ncarg/ngo/go.h>
#include <ncarg/ngo/plotspecmenu.h>
#include <ncarg/ngo/restree.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif

typedef struct _NgHluPage
{
        NhlString class_name;
        NgDataSinkRec	*data_info;
} NgHluPage;
        
#endif	/* _NG_HLUPAGE_H */

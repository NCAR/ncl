/*
 *      $Id: varpage.h,v 1.4 1999-02-23 03:56:55 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		varpage.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Mar  4 12:38:45 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_VARPAGE_H
#define	_NG_VARPAGE_H

#include <ncarg/ngo/go.h>

#ifndef _NCL_H_
#include <ncarg/ncl/defs.h>
#include <ncarg/ncl/NclDataDefs.h>
#include <ncarg/ncl/ApiRecords.h>
#include <ncarg/ncl/NclApi.h>
#define _NCL_H_
#endif
      
extern void NgVarPageDataUpdate(
	NhlPointer 	pdata
);

  
#endif	/* _NG_VARPAGE_H */

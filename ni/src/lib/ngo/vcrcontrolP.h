/*
 *      $Id: vcrcontrolP.h,v 1.3 2000-03-21 02:35:56 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		vcrcontrolP.h
 *
 *	Author:		David I. Brown
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 10 14:22:58 MST 1997
 *
 *	Description:	
 */
#ifndef	_NG_VCRCONTROLP_H_
#define	_NG_VCRCONTROLP_H_

#include <ncarg/ngo/goP.h>

#include <ncarg/ngo/vcrcontrol.h>

 
#define DEBUG_VCR_CONTROL 0

typedef struct _NgVcrControlRec 
{
	NgVcrControl	public;
        NgGO		go;
        NhlBoolean	horizontal;
        Dimension	size;
} NgVcrControlRec;

#endif	/* _NG_VCRCONTROLP_H_ */

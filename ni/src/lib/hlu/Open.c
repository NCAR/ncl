/*
 *      $Id: Open.c,v 1.3 1994-02-18 02:54:37 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Open.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Aug 31 09:50:11 MDT 1992
 *
 *	Description:	This file contains the functions neccessary to
 *			initialize the hlu library.
 */
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/ResListP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/ResourcesP.h>
#include <ncarg/hlu/ErrorP.h>

void NhlOpen()
{
	/* Initialize Resource Mngmt stuff */
	_NrmInitialize();
	_NhlConvertersInitialize();
	_NhlResourceListInitialize();
	_NhlInitResDatabase();

	/* Initialize Error handling */
	_NhlInitError();
	_NhlInitRLList();
}

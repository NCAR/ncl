/*
 *      $Id: lmlw.c,v 1.2 1997-08-25 20:22:04 boote Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1996			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		lmlw.c
 *
 *	Author:		Jeff W. Boote
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Dec 23 10:37:11 MST 1996
 *
 *	Description:	
 */
#include "lmlw.h"
#include <XmL/XmL.h>
#include <XmL/Folder.h>
#include <XmL/Grid.h>
#include <XmL/Progress.h>
#include <XmL/Tree.h>

static WidgetClass loadxmlw[4];
static void	*loadxmlf[2];

int
NgLoadXmlWidgetLib
(
	int	i
)
{
	loadxmlw[0] = xmlFolderWidgetClass;
	loadxmlw[1] = xmlGridWidgetClass;
	loadxmlw[2] = xmlProgressWidgetClass;
	loadxmlw[3] = xmlTreeWidgetClass;

	loadxmlf[0] = XmLGridEditCancel;
	loadxmlf[1] = XmLGridRowColumnToXY;

	return i+1;
}

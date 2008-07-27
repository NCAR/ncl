/*
 *	$Id: cgmmisc.c,v 1.5 2008-07-27 03:18:43 haley Exp $
 */
/************************************************************************
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
*                                                                       *
************************************************************************/

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1990                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                      NCAR View V3.01 - UNIX Release                  *
*                                                                      *
***********************************************************************/

/*
 *      $Id: cgmmisc.c,v 1.5 2008-07-27 03:18:43 haley Exp $
 */
/*
 *	File:		cgmmisc.c
 *
 *	Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Feb 20 14:29:14 MST 1992
 *
 *	Description:	misc CGM functions.
 */


#include <stdio.h> 
#include "cgmc.h"

MunchCGM(c)
CGMC	*c;
{
	while(c->more) {
		if (Instr_Dec(c) < 1) {
			return(-1);
		}
	}
	return(1);
}

/*
 *	$Id: fort_c.h,v 1.7 2003-01-06 23:30:13 fred Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU General Public License as published     *
* by the Free Software Foundation; either version 2 of the License, or  *
* (at your option) any later version.                                   *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* General Public License for more details.                              *
*                                                                       *
* You should have received a copy of the GNU General Public License     *
* along with this software; if not, write to the Free Software          *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/*
 *      File:		fort_c.h
 *
 *      Author:		John Clyne
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *      Date:		Fri May  3 11:51:44 MDT 1991
 *
 *      Description:	Header file for Fortran/C interface.
 *		
 *	
 *
 */
#ifndef	_fort_c_
#define	_fort_c_

#include <ncarg/c.h>
/*
 *	supported output device identifiers
 */
#define	DEV_CGM		1	/* not supported	*/
#define	DEV_X11P	7
#define	DEV_X11		8
#define	DEV_WISS	9	/* not supported	*/
#define	DEV_CTXT       10
#define	DEV_PDF_P      11       /* PDF portrait */
#define	DEV_PDF_L      12       /* PDF landscape */
#define	DEV_PS         20 	/* generic id for all PS drivers */
#define	DEV_PS_MIN     20       /* smallest id for the PS drivers */
#define	DEV_PS_MAX     31       /* largest id for the PS drivers */


#define	ERR_MSG_MAX	160	/* maximum error message size	*/

#endif	/*	_fort_c_	 */

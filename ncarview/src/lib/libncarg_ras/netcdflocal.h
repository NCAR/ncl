/*
 *      $Id: netcdflocal.h,v 1.2 2000-07-12 18:01:36 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
* This file is free software; you can redistribute it and/or modify     *
* it under the terms of the GNU Lesser General Public License as        *
* published by the Free Software Foundation; either version 2.1 of the  *
* License, or (at your option) any later version.                       *
*                                                                       *
* This software is distributed in the hope that it will be useful, but  *
* WITHOUT ANY WARRANTY; without even the implied warranty of            *
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
* Lesser General Public License for more details.                       *
*                                                                       *
* You should have received a copy of the GNU Lesser General Public      *
* License along with this software; if not, write to the Free Software  *
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307   *
* USA.                                                                  *
*                                                                       *
************************************************************************/

/************************************************************************
*									*
*			     Copyright (C)  1992			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		netcdflocal.h
 *
 *	Author:		Don Middleton
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Sep 10 14:06:23 MDT 1992
 *
 *	Description:	Local header file for Netcdf image driver.
 */

typedef struct _NetcdfInfo {
	int	cdfid;
	int	ndims;
	int	nvars;
	int	ngatts;
	int	xdimid;
} NetcdfInfo;

typedef struct _NetcdfDim {
    char name[MAX_NC_NAME];
    long size;
} NetcdfDim;

typedef struct _NetcdfVar {
    char name[MAX_NC_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
} NetcdfVar;

typedef struct _NetcdfAttribute {
    int var;
    char name[MAX_NC_NAME];
    nc_type type;
    int len;
    void *val;
} NetcdfAttribute;

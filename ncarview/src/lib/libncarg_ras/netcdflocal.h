/*
 *      $Id: netcdflocal.h,v 1.1 1992-09-10 21:18:30 don Exp $
 */
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

/*
 *	$Id: error.h,v 1.8 2008-07-27 03:22:41 haley Exp $
 */
/************************************************************************
*                                                                       *
*                Copyright (C)  2000                                    *
*        University Corporation for Atmospheric Research                *
*                All Rights Reserved                                    *
*                                                                       *
*    The use of this Software is governed by a License Agreement.       *
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
 *	File:		error.h
 *
 *	Author:		Don Middleton
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Aug 12 03:23:03 MDT 1992
 *
 *	Description:	This is the include file for the error.c
 *			error processing module.
 *
 *			NOTE: If you change or add an error
 *			number/message, this file *and* ncarg_ras.h
 *			must be appropriately modified.
 */
#ifndef __RAS_ERROR__
#define __RAS_ERROR__
#endif /*__RAS_ERROR__*/

static const char *ras_errlist[] = {
	"System Error",
	"Internal Error - ",
	"Only 8-bit pixels supported",
	"Only 8-bit intensities supported",
	"Only 8-bit run lengths supported",
	"Image not in correct format",
	"Unsupported image encoding",
	"Improper colormap load",
	"Colormap too big",
	"Image size changed within file",
	"Image encoding changed within file",
	"No format specified",
	"Internal Error - NULL name provided",
	"Unknown format",
	"Invalid colormap entry",
	"No option parameter",
	"Unsupported resolution",
	"Bogus raster structure",
	"Unsupported function",
	"Sun RLE encoding not supported",
	"Parallax frame buffer",
	"Premature EOF",
	"Programming Error",
	"Too many dither bits",
	"Unknown resource",
	"Bogus magic cookie"
};

static const char *hdf_errlist[] = {
	"File not found",
	"Access to file denied",
	"File already open",
	"Too Many DF's or files open",
	"Bad file name on open",
	"Bad file access mode",
	"Other open error",
	"File can't be closed 'cause it isn't open",
	"fclose wouldn't work!",
	"DF is a null pointer",
	"DF has an illegal type: internal error",
	"Feature not currently supported",
	"The DD list is non-existent: internal error",
	"This is not a DF file and it is not 0 length",
	"The DD list already seeded: internal error",
	"Malloc failed",
	"No such tag in the file: search failed",
	"There was a read error",
	"There was a write error",
	"There was a seek error",
	"There are no free DD's left: internal error",
	"illegal WILDCARD tag",
	"illegal WILDCARD reference #",
	"The DF is read only",
	"Calls in wrong order",
	"NULL ptr argument",
	"Invalid len specified",
	"Attempt to seek past end of element",
	"No (more) DDs which match specified tag/ref",
	"Warning: Set contained unknown tag: ignored",
	"negative or zero dimensions specified",
	"Illegal offset specified",
	"Unknown compression scheme specified",
	"No dimension record associated with image",
	"space provided insufficient for size of data",
	"Values not available",
	"File is corrupted",
	"File contained an illegal floating point num",
	"no more reference numbers are available",
	"unknown or unavailable data type specified",
	"unknown or unavailable machine type specified",
	"unknown or unavailable number type specified",
	"unknown or illegal array order specified",
	"bad arguments to routine",
	"serious internal error",
	"the new tag/ref is already used",
	"old element not exist, cannot modify",
	"improper range for attempted acess",
	"the nsdg table is wrong  ",
	"error processing an sdg   ",
	"error processing an ndg    ",
	"Bad fields string passed to Vset routine",
	"Too late to modify this value",
	"Counldn't find VS in file",
	"Too many elements in VGroup",
	"Attempt to merge objs in diff files",
	"Elmt not in vtab[]",
	"Got a bogus aid",
	"There are still active AIDs",
	"Don't know how to convert data type",
	"Generic application-level error"
};

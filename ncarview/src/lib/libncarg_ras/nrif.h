/*
 *	$Id: nrif.h,v 1.6 2000-08-22 03:30:24 haley Exp $
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


#define NRIF_MAGIC		"NRIF"
#define NRIF_CONTROL		0x1000

#define NRIF_HEADER_SIZE	36

typedef enum {
	NRIF_BILEVEL,
	NRIF_BILEVEL_RLE,
	NRIF_INDEXED,
	NRIF_INDEXED_RLE,
	NRIF_DIRECT,
	NRIF_DIRECT_RLE,
	NRIF_DIRECT_SEG,
	NRIF_DIRECT_SEG_RLE
} NrifEncoding;

static char	*nrif_types[] = {
	"NRIF_BILEVEL",
	"NRIF_BILEVEL_RLE",
	"NRIF_INDEXED",
	"NRIF_INDEXED_RLE",
	"NRIF_DIRECT",
	"NRIF_DIRECT_RLE",
	"NRIF_DIRECT_SEG",
	"NRIF_DIRECT_SEG_RLE"
};

typedef struct NrifInfoStruct {

	/* Derived from NRIF header */

	unsigned int	encapsulated;
	unsigned int	vplot;

	/* NRIF encapsulated information */

	unsigned int	objtype;
	unsigned int	control;
	unsigned int	recsize;

	/* NRIF header information */

	char		magic[4];
	unsigned int	flags;
	unsigned int	nx;
	unsigned int	ny;
	unsigned int	cmtlen;
	char		*comment;
	unsigned int	device;
	unsigned int	devlen;
	char		*device_info;
	NrifEncoding	encoding;
	unsigned int	enclen;

	/* Encoding information */

	unsigned int	ncolor;
	unsigned int	ibits;
	unsigned int	cbits;
	unsigned int	rbits;
	unsigned int	fcolred;
	unsigned int	fcolgrn;
	unsigned int	fcolblu;
	unsigned int	bcolred;
	unsigned int	bcolgrn;
	unsigned int	bcolblu;
	unsigned int	pbits;
} NrifInfo;

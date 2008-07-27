/*
 *	$Id: nrif.h,v 1.7 2008-07-27 03:22:41 haley Exp $
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

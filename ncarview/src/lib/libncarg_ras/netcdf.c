/*
 *	$Id: netcdf.c,v 1.6 2008-07-27 03:18:46 haley Exp $
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

/***********************************************************************
*                                                                      *
*                          Copyright (C)  1991                         *
*            University Corporation for Atmospheric Research           *
*                          All Rights Reserved                         *
*                                                                      *
*                                                                      *
***********************************************************************/
/*	File:	abekas.c
 *
 *	Author: Don Middleton
 *		National Center for Atmospheric Research
 *		Scientific Visualization Group
 *		Boulder, Colorado 80307
 *
 *	Date:	8-92
 *
 *	Description:
 *		This file contains a collection of functions
 *		which provides access to a raster sequence
 *		using a general abstraction.
 *
 *		This particular set of routines provides
 *		basic file access functions for the rasterfile
 *		format supported by the Abekas A60 digital
 *		recorder, and also the Quantel Paintbox.
 *
 *		The Abekas A60 has a fixed resolution of 720x486
 *		and images come in RGB and YUV flavors.
 *		
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
#include <netcdf.h>
#include "ncarg_ras.h"
#include "options.h"
#include "netcdflocal.h"

/*LINTLIBRARY*/

static char	*FormatName = "cdf";
static char	*_NetcdfTypeName();

static int	OptionVerbose = False;

Raster *
NetcdfOpen(name)
	char	*name;
{
	char		*errmsg = "NetcdfOpen(\"%s\")";
	Raster		*ras;
	NetcdfInfo	*dep;

	ras = (Raster *) ras_calloc(sizeof(Raster), 1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->dep = ras_calloc(sizeof(NetcdfInfo),1);
	if (ras == (Raster *) NULL) {
		(void) ESprintf(errno, errmsg, name);
		return( (Raster *) NULL );
	}

	ras->fd = -1;
	ras->fp = (FILE *) NULL;

	if (!strcmp(name, "stdin")) {
		(void) ESprintf(E_UNKNOWN, "stdin cannot be used with netCDF");
		return( (Raster *) NULL );
	}

	dep = (NetcdfInfo *) ras->dep;

	dep->cdfid = ncopen(name, NC_NOWRITE);
	if (dep->cdfid == -1) {
		(void) ESprintf(E_UNKNOWN, "NetcdfOpen(\"%s\") failed", name);
		return( (Raster *) NULL );
	}

	ras->name = (char *) ras_calloc( (unsigned) (strlen(name) + 1), 1);
	(void) strcpy(ras->name, name);

	ras->format = (char *) ras_calloc((unsigned) (strlen(FormatName) + 1), 1);
	(void) strcpy(ras->format, FormatName);

	NetcdfSetFunctions(ras);

	return(ras);
}

int
NetcdfRead(ras)
	Raster	*ras;
{
	NetcdfInfo		*dep;
	int			status;
	int			dimid, varid;
	NetcdfDim		dims[MAX_NC_DIMS];
	NetcdfVar		var;
	int			i;
	int			image_exists = True;
	int			image_id = -1;
	long			corners[2], vectors[2];

	dep = (NetcdfInfo *) ras->dep;

	status = ncinquire(dep->cdfid,  &dep->ndims, &dep->nvars,
					&dep->ngatts, &dep->xdimid);
	
	if (dep->xdimid == -1 && ras->read == True) {
		return(RAS_EOF);
	}

	for(dimid = 0; dimid < dep->ndims; dimid++) {
    
		status = ncdiminq(dep->cdfid, dimid, dims[dimid].name,
				  &dims[dimid].size);
		if (status == -1) {
			(void) ESprintf(E_UNKNOWN,
			"NetcdfRead(\"%s\") - ncdiminq() failed on dim id %d",
			dimid);
	    		(void) ncclose(dep->cdfid);
			return(RAS_ERROR);
		}

		if (!strcmp(dims[dimid].name, "xsize")) {
			ras->nx = dims[dimid].size;
		}

		if (!strcmp(dims[dimid].name, "ysize")) {
			ras->ny = dims[dimid].size;
		}
	}

	if (ras->nx == 0 || ras->ny == 0) {
		(void) ESprintf(E_UNKNOWN,
			"NetcdfRead(\"%s\") - No xsize or ysize",
			ras->name);
		(void) ncclose(dep->cdfid);
		return(RAS_ERROR);
	}

	if (OptionVerbose) {
		for(dimid = 0; dimid < dep->ndims; dimid++) {
			(void) fprintf(stderr, "%4d: %s = %d\n", dimid,
						dims[dimid].name,
						dims[dimid].size);
		}
	}

	for(varid = 0; varid < dep->nvars; varid++) {
		status = ncvarinq(dep->cdfid, varid, var.name,
				  &var.type, &var.ndims, var.dims,
				  &var.natts);
		if (status == -1) {
			(void) ESprintf(E_UNKNOWN,
			"NetcdfRead(\"%s\") - ncvarinq failed on dom id %d\n",
			varid);
	    		(void) ncclose(dep->cdfid);
			return(RAS_ERROR);
		}

		if (OptionVerbose) {
		(void) fprintf(stderr,
			"%d: \"%s\" type=%s ndims=%d, \n", varid, var.name,
			_NetcdfTypeName(var.type), var.ndims);
		}
		
		if (!strcmp(var.name, "image")) {
			image_exists = True;
			image_id     = varid;
		}
	}

	if (image_exists == False) {
		(void) ESprintf(E_UNKNOWN,
			"NetcdfRead(\"%s\") - no \"image\" var in file\n",
			ras->name);
		(void) ncclose(dep->cdfid);
		return(RAS_ERROR);
	}

	if (ras->read == False) {
		ras->read      = True;
		ras->file_nx   = ras->nx;
		ras->file_ny   = ras->ny;
		ras->file_type = ras->type;
	}

	ras->type   = RAS_INDEXED;
	ras->length = ras->nx * ras->ny;
	ras->ncolor = RAS_DEFAULT_NCOLORS;

	corners[0] = 0;
	corners[1] = 0;
	vectors[0]  = ras->ny;
	vectors[1]  = ras->nx;

	if (ras->data == (unsigned char *) NULL) {
		ras->data = (unsigned char *) ras_calloc(ras->length, 1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno,
				"NetcdfRead(\"%s\")", ras->name);
			(void) ncclose(dep->cdfid);
			return(RAS_ERROR);
		}
		ras->red  =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->green=(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
		ras->blue =(unsigned char *)ras_calloc((unsigned)ras->ncolor,1);
	}

	for(i=0; i<RAS_DEFAULT_NCOLORS; i++) {
		ras->red[i] = i;
		ras->green[i] = i;
		ras->blue[i] = i;
	}

	status = ncvarget(dep->cdfid, image_id, corners, vectors, ras->data);

#ifdef DEAD
    /* get variable info, with variable attributes */
    for (varid = 0; varid < nvars; varid++) {
	if (ncvarinq(cdfid, varid, var.name, &var.type,
		      &var.ndims, var.dims, &var.natts) == -1) {
	    error("ncvarinq failed on var id %d", varid);
	    (void) ncclose(cdfid); return;
	}
	if (var.ndims > 0)
	  Printf ("(");
	for (id = 0; id < var.ndims; id++) {
	    Printf ("%s%s",
		    dims[var.dims[id]].name,
		    id < var.ndims-1 ? ", " : ")");
	}
	Printf (" ;\n");
	if (dimid == xdimid)
	  Printf ("\t%s = %s ; // (%d currently)\n",dims[dimid].name,
		  "UNLIMITED", dims[dimid].size);
	else
	  Printf ("\t%s = %ld ;\n",dims[dimid].name, dims[dimid].size);
    }

	if (ras->read == False) {
		ras->read      = True;
		ras->file_nx   = RAS_ABEKAS_NX;
		ras->file_ny   = RAS_ABEKAS_NY;
		ras->file_type = RAS_DIRECT;
	}

	/*
	Initialize per-file information on first read. This is
	done for consistency, but there are no headers and
	there is an implicit assumption of a fixed resolution and
	encoding. Nothing can really change from one frame
	to another.
	*/

	if (ras->read == False) {
		ras->read      = True;
		ras->file_nx   = RAS_ABEKAS_NX;
		ras->file_ny   = RAS_ABEKAS_NY;
		ras->file_type = RAS_DIRECT;
	}

	ras->nx     = RAS_ABEKAS_NX;
	ras->ny     = RAS_ABEKAS_NY;
	ras->type   = RAS_DIRECT;
	ras->length = RAS_ABEKAS_NX * RAS_ABEKAS_NY * 3;

	if (ras->data == (unsigned char *) NULL) {
		ras->data=(unsigned char *)ras_calloc((unsigned)ras->length,1);
		if (ras->data == (unsigned char *) NULL) {
			(void) ESprintf(errno, errmsg, ras->name);
			return(RAS_ERROR);
		}
	}

	status = fread((char *)ras->data, 1, ras->length, ras->fp);
	if (status != ras->length) {
		(void) ESprintf(errno, errmsg, ras->name);
		return(RAS_EOF);
	}
#endif /* DEAD */

	return(RAS_OK);
}


/*ARGSUSED*/
Raster *
NetcdfOpenWrite(name, nx, ny, comment, encoding)
	char		*name;
	int		nx;
	int		ny;
	char		*comment;
	RasterEncoding	encoding;
{
	char	*errmsg = "NetcdfOpenWrite(\"%s\")";
	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, errmsg, name);
	return( (Raster *) NULL );
}

int
NetcdfWrite(ras)
	Raster	*ras;
{
	char		*errmsg = "NetcdfWrite(\"%s\")";

	(void) ESprintf(RAS_E_UNSUPPORTED_FUNCTION, errmsg, ras->name);
	return(RAS_ERROR);
}

int
NetcdfClose(ras)
	Raster	*ras;
{
	int		status;
	NetcdfInfo	*dep;

	dep = (NetcdfInfo *) ras->dep;
	
	status = ncclose(dep->cdfid);
	if (status == -1) {
		(void) ESprintf(E_UNKNOWN,
			"NetcdfClose(\"%s\") - ncclose() failed",
			ras->name);
		return(RAS_ERROR);
	}

	status = GenericClose(ras);
	return(status);
}

NetcdfPrintInfo(ras)
	Raster	*ras;
{
	NetcdfInfo	*dep;

	dep = (NetcdfInfo *) ras->dep;


	(void) fprintf(stderr, "\n");
	(void) fprintf(stderr, "Netcdf Image File Information\n");
	(void) fprintf(stderr, "---------------------------\n");
	(void) fprintf(stderr, "cdfid:     %d\n", dep->cdfid);
	(void) fprintf(stderr, "ndims:     %d\n", dep->ndims);
	(void) fprintf(stderr, "nvars:     %d\n", dep->nvars);
	(void) fprintf(stderr, "ngatts:    %d\n", dep->ngatts);
	(void) fprintf(stderr, "xdimid:    %d\n", dep->xdimid);
	
	return(RAS_OK);
}

int
NetcdfSetFunctions(ras)
	Raster	*ras;
{
	extern	int	ImageCount_();

	ras->Open      = NetcdfOpen;
	ras->OpenWrite = NetcdfOpenWrite;
	ras->Read      = NetcdfRead;
	ras->Write     = NetcdfWrite;
	ras->Close     = NetcdfClose;
	ras->PrintInfo = NetcdfPrintInfo;
	ras->ImageCount = ImageCount_;
}

static char *
_NetcdfTypeName(type)
     nc_type type;
{
    switch (type) {
      case NC_BYTE:
	return "byte";
      case NC_CHAR:
	return "char";
      case NC_SHORT:
	return "short";
      case NC_LONG:
	return "long";
      case NC_FLOAT:
	return "float";
      case NC_DOUBLE:
	return "double";
      default:
	return "bogus";
    }
}

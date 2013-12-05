#ifndef _NclHDF_H
#define _NclHDF_H
/*
 *      $Id: NclHDF.c 14204 2013-03-14 14:18:36Z huangwei $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 13 10:15:21 MDT 1994
 *
 *	Description:	
 */

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#define HAVE_NETCDF
#include <mfhdf.h>
#include "NclData.h"
#include "NclFileInterfaces.h"
#include <math.h>
#include <ctype.h>

/*
 * With newer versions of HDF4 (like 4.2r3), some of macro names now 
 * have an H4 prepended.
 *
 * In order to accommodate multiple versions of HDF, Dave B suggested
 * the following code.
 */
#ifndef MAX_VAR_DIMS
#ifdef H4_MAX_VAR_DIMS
#define MAX_VAR_DIMS H4_MAX_VAR_DIMS
#define MAX_NC_NAME H4_MAX_NC_NAME
#define MAX_NC_DIMS H4_MAX_NC_DIMS
#else
#define MAX_VAR_DIMS 32
#define MAX_NC_NAME 256
#define MAX_NC_DIMS 5000
#endif
#endif

typedef struct _HDFFileRecord HDFFileRecord;
typedef struct _HDFVarInqRec HDFVarInqRec;
typedef struct _HDFDimInqRec HDFDimInqRec;
typedef struct _HDFAttInqRec HDFAttInqRec;
typedef struct _HDFVarInqRecList HDFVarInqRecList;
typedef struct _HDFDimInqRecList HDFDimInqRecList;
typedef struct _HDFAttInqRecList HDFAttInqRecList;

struct _HDFVarInqRecList {
	HDFVarInqRec *var_inq;
	HDFVarInqRecList *next;
};

struct _HDFDimInqRecList {
	HDFDimInqRec *dim_inq;
	HDFDimInqRecList *next;
};

struct _HDFAttInqRecList {
	HDFAttInqRec *att_inq;
	HDFAttInqRecList *next;
};

struct _HDFVarInqRec {
	int varid;
	int32 id_ref;
	int32 vg_ref;
	NclQuark name;
	NclQuark hdf_name;
	NclQuark class_name;
	NclQuark var_path;
	nc_type	data_type;
	int     hdf_type;
	int	n_dims;
	int	dim[MAX_VAR_DIMS];
	int	natts;
	HDFAttInqRecList *att_list;
};

struct _HDFDimInqRec {
	int dimid;
	NclQuark name;
	NclQuark hdf_name;
	long size;
	int is_unlimited;
};
	
struct _HDFAttInqRec {
	int att_num;
	NclQuark name;
	NclQuark hdf_name;
	int	varid;
	nc_type data_type;
	int     hdf_type;
	int	len;
	void *value;
	int attr_ix;
};


struct _HDFFileRecord {
NclQuark	file_path_q;
int		wr_status;
int		n_vars;
HDFVarInqRecList *vars;
int		n_dims;
HDFDimInqRecList *dims;
int		has_scalar_dim;
int		n_file_atts;
HDFAttInqRecList *file_atts;
};

void *HDFOpenFile(void *rec,NclQuark path,int wr_status);
void HDFFreeFileRec(void* therec);
#endif


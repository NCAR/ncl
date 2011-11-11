/*
 *      $Id: NclHDFEOS.c,v 1.16 2010-05-06 22:52:28 huangwei Exp $
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
 *	Date:		Wed Mar 1 10:15:20 MDT 2001 
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
#include <hdf/mfhdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclData.h"
#include <math.h>
#include <ctype.h>
#include <HdfEosDef.h>

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

#define HDF_BUFFSIZE 4096

typedef struct _HDFEOSFileRecord HDFEOSFileRecord;
typedef struct _HDFEOSVarInqRec HDFEOSVarInqRec;
typedef struct _HDFEOSDimInqRec HDFEOSDimInqRec;
typedef struct _HDFEOSAttInqRec HDFEOSAttInqRec;
typedef struct _HDFEOSVarInqRecList HDFEOSVarInqRecList;
typedef struct _HDFEOSDimInqRecList HDFEOSDimInqRecList;
typedef struct _HDFEOSAttInqRecList HDFEOSAttInqRecList;

struct _HDFEOSVarInqRecList {
	HDFEOSVarInqRec *var_inq;
	HDFEOSVarInqRecList *next;
};

struct _HDFEOSDimInqRecList {
	HDFEOSDimInqRec *dim_inq;
	HDFEOSDimInqRecList *next;
};


struct _HDFEOSAttInqRecList {
	HDFEOSAttInqRec *att_inq;
	HDFEOSAttInqRecList *next;
};

typedef enum { SWATH,POINT,GRID} HDFEOSType;

struct _HDFEOSVarInqRec {
	NclQuark name;
	NclQuark hdf_name;
	NclQuark index_dim;
	/*int32 hdf_data_id;*/
	HDFEOSType var_class;
	NclQuark var_class_name;
	int32 typenumber;
	int32 n_dims;
	int32 dim[MAX_VAR_DIMS];
	int32 n_int_atts;
	HDFEOSAttInqRecList *att_int_list;
};

struct _HDFEOSDimInqRec {
	NclQuark name;
	NclQuark hdf_name;
	int ncldim_id;
	long size;
	int is_unlimited;
};

struct _HDFEOSAttInqRec {
	NclQuark name;
	NclQuark hdf_name;
	void *value;
	int n_elem;
	NclBasicDataTypes type;
};



struct _HDFEOSFileRecord {
NclQuark        file_path_q;
int             wr_status;
int             n_vars;
HDFEOSVarInqRecList *vars;
int             n_dims;
HDFEOSDimInqRecList *dims;
int             n_int_atts;
HDFEOSAttInqRecList *att_int_list;
};

static NrmQuark Qmissing_val;
static NrmQuark Qfill_val;

int HDFEOSunsigned(int32 typenumber)
{
	switch(typenumber) {
	case DFNT_FLOAT32:
			return(0);
	case DFNT_FLOAT64:
			return(0);
	case DFNT_INT8:
			return(0);
	case DFNT_UINT8:
			return(1);
	case DFNT_INT16:
			return(0);
	case DFNT_UINT16:
			return(1);
	case DFNT_INT32:
			return(0);
	case DFNT_UINT32:
			return(1);
	case DFNT_INT64:
			return(0);
	case DFNT_UINT64:
			return(1);
	case DFNT_CHAR:
			return(0);
	case DFNT_UCHAR:
			return(1);
	default:
			return(0);
	
	}
}
static NclBasicDataTypes HDFEOSMapTypeNumber(int32 typenumber){
	switch(typenumber) {
	case DFNT_FLOAT32:
			return(NCL_float);
	case DFNT_FLOAT64:
			return(NCL_double);
	case DFNT_INT8:
			return(NCL_byte);
	case DFNT_UINT8:
			return(NCL_byte);
	case DFNT_INT16:
			return(NCL_short);
	case DFNT_UINT16:
			return(NCL_short);
	case DFNT_INT32:
			return(NCL_int);
	case DFNT_UINT32:
			return(NCL_int);
	case DFNT_INT64:
			return(NCL_long);
	case DFNT_UINT64:
			return(NCL_long);
	case DFNT_CHAR:
			return(NCL_char);
	case DFNT_UCHAR:
			return(NCL_char);
	default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS: Unsupported type encountered");
			return(NCL_none);
	
	}
}

static void HDFEOSParseName
#if NhlNeedProto
(char names_in[], NclQuark **hdf_names, NclQuark **ncl_names, int32 n_names)
#else
(name_ins,ncl_names,hdf_names)
char names_in[];
NclQuark *ncl_names;
NclQuark *hdf_names;
int32 n_names;
#endif
{
	int i;
	char *tmp,*tmp2;
	
	*hdf_names = NclMalloc(sizeof(NclQuark)*n_names);
	*ncl_names = NclMalloc(sizeof(NclQuark)*n_names);

	tmp = names_in;
	for(i = 0; i < n_names; i++) {
		if((tmp2 = strchr(tmp,','))!= 0) {
			*tmp2 = '\0';
		}
		(*hdf_names)[i] = NrmStringToQuark(tmp);
		tmp2 = tmp;
		while(*tmp2 != '\0') {
			if(!isalnum(*tmp2)) {
				*tmp2 = '_';
			}
			tmp2++;
		}
		(*ncl_names)[i] = NrmStringToQuark(tmp);
		tmp = tmp2 + 1;
	}	
	return;
}

static void HDFEOSIntAddVar
#if NhlNeedProto
(HDFEOSVarInqRecList **vars,NclQuark hdf_name, NclQuark ncl_name, HDFEOSDimInqRecList *dims, int32 n_dims, int32 *dims_sizes, int32 type, NclQuark* dimlist, HDFEOSType class,NclQuark class_name,NclQuark ncl_class_name)
#else
(vars, hdf_name, ncl_name, dims, n_dims, dims_sizes, type,dimlist,class,class_name,ncl_class_name)
HDFEOSVarInqRecList **vars;
NclQuark hdf_name;
NclQuark ncl_name;
HDFEOSDimInqRecList *dims;
int32 n_dims;
int32 *dims_sizes; 
int32 type; 
NclQuark* dimlist;
HDFEOSType class;
NclQuark class_name;
NclQuark ncl_class_name;
#endif 
{
	HDFEOSVarInqRecList *tmp_node = (HDFEOSVarInqRecList *)NclMalloc(sizeof(HDFEOSVarInqRecList));
	HDFEOSDimInqRecList *step = NULL;
	char buffer[4096];
	int i;

	tmp_node->var_inq = (HDFEOSVarInqRec*)NclMalloc(sizeof(HDFEOSVarInqRec));
	strcpy(buffer,NrmQuarkToString(ncl_name));
	strcat(buffer,"_");
	strcat(buffer,NrmQuarkToString(ncl_class_name));
	tmp_node->var_inq->name = NrmStringToQuark(buffer);
	tmp_node->var_inq->hdf_name = hdf_name;
	tmp_node->var_inq->index_dim = NrmNULLQUARK;
	tmp_node->var_inq->typenumber = type;
	tmp_node->var_inq->n_dims = n_dims;
	tmp_node->var_inq->att_int_list = NULL;
	tmp_node->var_inq->n_int_atts = 0;
	tmp_node->var_inq->var_class = class;
	tmp_node->var_inq->var_class_name = class_name;
	for(i = 0; i < n_dims; i++) {
		step = dims;
		while(step != NULL) {
			strcpy(buffer,NrmQuarkToString(dimlist[i]));
			strcat(buffer,"_");
			strcat(buffer,NrmQuarkToString(ncl_class_name));
			if(NrmStringToQuark(buffer) == step->dim_inq->name) {
				tmp_node->var_inq->dim[i] = step->dim_inq->ncldim_id;
				/* If the dimension is unlimited now we know its current size */
				if (step->dim_inq->is_unlimited) {
					step->dim_inq->size = dims_sizes[i];
				}
				break;
			}
			step = step->next;
		}
	}
	tmp_node->next = *vars;
	*vars = tmp_node;

}

static void HDFEOSIntAddDim
#if NhlNeedProto
(HDFEOSDimInqRecList **dims,int *n_dims, NclQuark hdf_name, NclQuark ncl_name, int32 size,NclQuark class_name,NclQuark ncl_class_name,
NclQuark path)
#else
(dims,n_dims, hdf_name, ncl_name, size,class_name)
HDFEOSDimInqRecList **dims;
int *n_dims; 
NclQuark hdf_name; 
NclQuark ncl_name; 
int32 size;
NclQuark class_name;
NclQuark ncl_class_name;
NclQuark path;
#endif 
{
	HDFEOSDimInqRecList * tmp_node ;
	HDFEOSDimInqRecList * step;
	char buffer[4096];

	strcpy(buffer,NrmQuarkToString(ncl_name));
	strcat(buffer,"_");
	strcat(buffer, NrmQuarkToString(ncl_class_name));	
	step = *dims;
	while(step != NULL) {
		if(step->dim_inq->name == NrmStringToQuark(buffer)) {
			return;
		} else {
			step = step->next;
		}
	}

	tmp_node = (HDFEOSDimInqRecList*)NclMalloc(sizeof(HDFEOSDimInqRecList));
	tmp_node->dim_inq = (HDFEOSDimInqRec*)NclMalloc(sizeof(HDFEOSDimInqRec));
	tmp_node->dim_inq->name = NrmStringToQuark(buffer);
	tmp_node->dim_inq->hdf_name = hdf_name;
	tmp_node->dim_inq->is_unlimited = size == 0 ? 1 : 0;
	if (! tmp_node->dim_inq->is_unlimited) {
		tmp_node->dim_inq->size = size;
	}
	else {
		/* set to 0 until we find a variable with this dimension  */
		tmp_node->dim_inq->size = size;
#if 0
		/* reuse buffer; this is a kludge because I can't find any interface to get the size of an unlimited dimension
		 * using the hdfeos swath interface.
		 */
		int dimid;
		long len;
		int cdfid = sd_ncopen(NrmQuarkToString(path),NC_NOWRITE);
		sprintf(buffer,"%s:%s",NrmQuarkToString(hdf_name),NrmQuarkToString(class_name));
		dimid = sd_ncdimid(cdfid,buffer);
		sd_ncdiminq(cdfid,dimid,buffer,&len);
		if (len > 0)
			tmp_node->dim_inq->size = len;	
		else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS: HDF-EOS dim size not accessible");
			len = -1;
		}
		sd_ncclose(cdfid);
#endif
	}
	tmp_node->dim_inq->ncldim_id = *n_dims;
	tmp_node->next = *dims;
	*dims = tmp_node;
	(*n_dims)++;
}

static void HDFEOSIntAddAtt
#if NhlNeedProto
(HDFEOSVarInqRec* thevar,NclQuark ncl_name,void* value, int n_elem, NclBasicDataTypes type) 
#else
( thevar, ncl_name, value, n_elem, type) 
HDFEOSVarInqRec* thevar;
NclQuark ncl_name; 
void *value;
int n_elem;
NclBasicDataTypes type;
#endif
{
	HDFEOSAttInqRecList *tmp_node = (HDFEOSAttInqRecList*)NclMalloc(sizeof(HDFEOSAttInqRecList));
	char buffer[1000];
	NrmQuark *tmp_quark;

	tmp_node->att_inq = (HDFEOSAttInqRec*)NclMalloc(sizeof(HDFEOSAttInqRec));
	tmp_node->att_inq->name = ncl_name;
	if(type != NCL_char || (tmp_node->att_inq->name == Qfill_val || tmp_node->att_inq->name == Qmissing_val)) {
		tmp_node->att_inq->value = value;
		tmp_node->att_inq->type = type;
		tmp_node->att_inq->n_elem = n_elem;
	} else {
		tmp_node->att_inq->type = NCL_string;
		memcpy(buffer,value,n_elem);
		buffer[n_elem] = '\0';
		tmp_quark = (NclQuark*)NclMalloc(sizeof(NclQuark));	
		*tmp_quark = NrmStringToQuark(buffer);
		tmp_node->att_inq->value = (void*)tmp_quark;
		tmp_node->att_inq->n_elem = 1;
		NclFree(value);
	}
	tmp_node->next = thevar->att_int_list;
	thevar->att_int_list = tmp_node;
	thevar->n_int_atts++;
}




static void HDFEOSIntFileAddAtt(HDFEOSFileRecord *the_file,NclQuark sw_ncl_name,NclQuark att_ncl_name,void *value,int n_elem, NclBasicDataTypes type)
{
	HDFEOSAttInqRecList *tmp_node = (HDFEOSAttInqRecList*)NclMalloc(sizeof(HDFEOSAttInqRecList));
	int buflen = 1000;
	char buffer[1000];
	char *bufp;
	int allocated = 0;
	NrmQuark *tmp_quark;

	strcpy(buffer,NrmQuarkToString(att_ncl_name));
	strcat(buffer,"_");
	strcat(buffer,NrmQuarkToString(sw_ncl_name));

	tmp_node->att_inq = (HDFEOSAttInqRec*)NclMalloc(sizeof(HDFEOSAttInqRec));
	tmp_node->att_inq->name = NrmStringToQuark(buffer);
	if(type != NCL_char) {
		tmp_node->att_inq->value = value;
		tmp_node->att_inq->type = type;
		tmp_node->att_inq->n_elem = n_elem;
	} else {
		tmp_node->att_inq->type = NCL_string;
		if (n_elem + 1 > buflen) {
			bufp = NclMalloc(n_elem + 1);
			allocated = 1;
		}
		else {
			bufp =  &buffer[0];
		}
		memcpy(bufp,value,n_elem);
		bufp[n_elem] = '\0';
		tmp_quark = (NclQuark*)NclMalloc(sizeof(NclQuark));	
		*tmp_quark = NrmStringToQuark(bufp);
		tmp_node->att_inq->value = (void*)tmp_quark;
		tmp_node->att_inq->n_elem = 1;
		NclFree(value);
		if (allocated)
			NclFree(bufp);
	}
	tmp_node->next = the_file->att_int_list;
	the_file->att_int_list = tmp_node;
	the_file->n_int_atts++;
}

static void HDFEOSIntAddDimMapInfo
(HDFEOSFileRecord *the_file,NrmQuark swath_ncl_name,int nmaps,char *dimmaps,int32 *off, int32 *inc)
{
	int i;
	char *tcp,*cp,*dim1, *dim2;
	char name_buf[1024];
	int* mapvals;

	cp = dimmaps;
	for (i = 0; i < nmaps; i++) {
		dim1 = cp;
		cp = strchr(cp,'/');
		if (cp) {
			*cp = '\0';
			cp++;
		}
		for (tcp = dim1; *tcp != '\0'; tcp++) {
			if(!isalnum(*tcp)) {
				*tcp = '_';
			}
		}
		dim2 = cp;
		cp = strchr(cp,',');
		if (cp) {
			*cp = '\0';
			cp++;
		}
		for (tcp = dim2; *tcp != '\0'; tcp++) {
			if(!isalnum(*tcp)) {
				*tcp = '_';
			}
		}
		if (off[i] >= 0 && inc[i] >= 0) {
			sprintf(name_buf,"%s_to_%s_mapping_offset_and_increment",dim1,dim2);
		}
		else {
			sprintf(name_buf,"%s_to_%s_mapping_offset_and_increment",dim2,dim1);
		}
		mapvals = NclMalloc(2 * sizeof(int));
		mapvals[0] = abs(off[i]);
		mapvals[1] = abs(inc[i]);
		HDFEOSIntFileAddAtt(the_file,swath_ncl_name,NrmStringToQuark(name_buf),(void *)mapvals,2,NCL_int);
	}
}

static void HDFEOSIntAddIndexedMapVars
(HDFEOSFileRecord *the_file,NrmQuark swath_hdf_name,NrmQuark swath_ncl_name,int nmaps,char *idxmaps,int32 *sizes)
{
	int i;
	char *tcp,*cp,*dim1, *dim2;
	char name_buf[1024];
	NrmQuark hdf_name1,ncl_name1,hdf_name2,ncl_name2;

	cp = idxmaps;
	for (i = 0; i < nmaps; i++) {
		dim1 = cp;
		cp = strchr(cp,'/');
		if (cp) {
			*cp = '\0';
			cp++;
		}
		hdf_name1 = NrmStringToQuark(dim1);
		for (tcp = dim1; *tcp != '\0'; tcp++) {
			if(!isalnum(*tcp)) {
				*tcp = '_';
			}
		}
		ncl_name1 = NrmStringToQuark(dim1);
		dim2 = cp;
		cp = strchr(cp,',');
		if (cp) {
			*cp = '\0';
			cp++;
		}
		hdf_name2 = NrmStringToQuark(dim2);
		for (tcp = dim2; *tcp != '\0'; tcp++) {
			if(!isalnum(*tcp)) {
				*tcp = '_';
			}
		}
		ncl_name2 = NrmStringToQuark(dim2);
		HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),hdf_name1,ncl_name1,sizes[i],
				swath_hdf_name,swath_ncl_name,the_file->file_path_q);
		sprintf(name_buf,"%s_index_mapping",dim2);

		HDFEOSIntAddVar(&(the_file->vars),hdf_name2,NrmStringToQuark(name_buf),
				the_file->dims,1,&(sizes[i]),DFNT_INT32,&ncl_name1,
				SWATH,swath_hdf_name,swath_ncl_name);
		/* we can assume that added variable is at the beginning of the variable list */
		the_file->vars->var_inq->index_dim = hdf_name1;
	}
}


static void *HDFEOSInitializeFileRec
#if	NhlNeedProto
(NclFileFormat *format)
#else
(format)
NclFileFormatType *format;
#endif
{
	HDFEOSFileRecord *therec = NULL;
	static int first = 1;

	if (first) {
		Qmissing_val = NrmStringToQuark("missing_value");
		Qfill_val = NrmStringToQuark("_FillValue");
		first = False;
	}

	therec = (HDFEOSFileRecord*)NclCalloc(1, sizeof(HDFEOSFileRecord));
	if (! therec) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}
	*format = _NclHDFEOS;
	return (void *) therec;
}

static void *HDFEOSOpenFile
#if	NhlNeedProto
(void *rec,NclQuark path,int wr_status)
#else
(rec,path,wr_status)
void *rec;
NclQuark path;
int wr_status;
#endif
{
	HDFEOSFileRecord *the_file = (HDFEOSFileRecord*) rec;
	int32 SWfid = 0;
	int32 PTfid = 0;
	int32 GDfid = 0;
	int32 SWid = 0;
	int32 PTid = 0;
	int32 GDid = 0;
	int32 natts;

	int32 ndims;
	int32 nmaps;
	int32 ngeofields;
	int32 npt,nsw,ngd,i,j,k;
/*	long bsize;*/
	int32 bsize;
	int32 *dimsizes;
	int32 ndata = 0;
	NclQuark *sw_hdf_names;
	NclQuark *sw_ncl_names;
	NclQuark *gd_hdf_names;
	NclQuark *gd_ncl_names;
	NclQuark *pt_hdf_names;
	NclQuark *pt_ncl_names;
	NclQuark *dim_hdf_names;
	NclQuark *dim_ncl_names;
	NclQuark *var_hdf_names;
	NclQuark *var_ncl_names;
	NclQuark *att_hdf_names;
	NclQuark *att_ncl_names;
	NclQuark *tmp_ncl_names;
	NclQuark *tmp_names;
	int32 tmp_rank;
	int32 tmp_type;
	int32 origincode=0;
	int32 projcode = 0;
	int32 zonecode =0 ;
	int32 spherecode = 0;
	void *tmp_value;
	int32 att_type;
	int32 att_size;
	intn nrc,nfd,nlv;
	int32 *fldorder = NULL;
	int32 *fldtype = NULL;
	float64 projparm[15];
	NclScalar missing;
	NclScalar *tmp_missing;
	int *is_unsigned;
	int32 xdimsize,ydimsize;
	float64 upper_left[2],lower_right[2];
	char *buffer;
	int cur_buf_size = 1024;
	int32 *field_ranks;
	int max_fields = 1024;
	HDFEOSVarInqRecList *vstep;

	if(the_file == NULL) {
		return(NULL);
	}

	the_file->file_path_q = path;
	the_file->wr_status = wr_status;
	the_file->n_vars = 0;
	the_file->n_dims= 0;
	the_file->n_int_atts= 0;
	the_file->vars = NULL;
	the_file->dims= NULL;
	the_file->att_int_list = NULL;
	


	if(wr_status <= 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS: HDF-EOS are currently read only in NCL");
		return(NULL);
	}
	buffer = NclMalloc(cur_buf_size);
	field_ranks = NclMalloc(max_fields * sizeof(int32));

	if((nsw = SWinqswath(NrmQuarkToString(path),NULL,&bsize)) > 0) {
		if (bsize + 1 > cur_buf_size) {
			buffer = NclRealloc(buffer,bsize + 1);
			cur_buf_size = bsize + 1;
		}
		nsw = SWinqswath(NrmQuarkToString(path),buffer,&bsize);
		SWfid = SWopen(NrmQuarkToString(path),DFACC_READ);
		HDFEOSParseName(buffer,&sw_hdf_names,&sw_ncl_names,nsw);
		for(i = 0; i < nsw; i++) {
			NrmQuark lat_name = NrmNULLQUARK, lon_name = NrmNULLQUARK;
			int32 y_dim_num = -1, x_dim_num = -1;
			SWid = SWattach(SWfid,NrmQuarkToString(sw_hdf_names[i]));
			if(! (SWid > 0 ))
				continue;
			/* global attributes from file */
			natts = SWinqattrs(SWid,NULL,&bsize);
			if(natts > 0 ) {
				if (bsize + 1 > cur_buf_size) {
					buffer = NclRealloc(buffer,bsize + 1);
					cur_buf_size = bsize + 1;
				}
				natts = SWinqattrs(SWid,buffer,&bsize);
				HDFEOSParseName(buffer,&att_hdf_names,&att_ncl_names,natts);
				for(k = 0; k < natts; k++) { 
					if(SWattrinfo(SWid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0) {
						tmp_value = (void*)NclMalloc(att_size);
						if(SWreadattr(SWid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 ) {
							HDFEOSIntFileAddAtt(the_file,sw_ncl_names[i],att_ncl_names[k],
									    tmp_value,att_size/_NclSizeOf(HDFEOSMapTypeNumber(att_type)),
									    HDFEOSMapTypeNumber(att_type));
						}
			
					}
				}
				NclFree(att_hdf_names);
				NclFree(att_ncl_names);
			}
			/* dimensions */
			ndims = SWnentries(SWid, HDFE_NENTDIM, &bsize);
			if(! (ndims > 0 )) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, 
					  "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",
					  NrmQuarkToString(path));
				return(NULL);
			}
			if (bsize + 1 > cur_buf_size) {
				buffer = NclRealloc(buffer,bsize + 1);
				cur_buf_size = bsize + 1;
			}
			dimsizes = NclMalloc(ndims * sizeof(int32));
			ndims = SWinqdims(SWid,buffer,dimsizes);
			HDFEOSParseName(buffer,&dim_hdf_names,&dim_ncl_names,ndims);
			for(j = 0; j < ndims; j++) {
				HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],
						dimsizes[j],sw_hdf_names[i],sw_ncl_names[i],the_file->file_path_q);
			}
			NclFree(dim_hdf_names);
			NclFree(dim_ncl_names);

			/* Dimension mappings */

			nmaps = SWnentries(SWid, HDFE_NENTMAP, &bsize);
			if (nmaps > 0) {
				int32 *off, *inc;
				off = NclMalloc(nmaps * sizeof(int32));
				inc = NclMalloc(nmaps * sizeof(int32));
				if (bsize + 1 > cur_buf_size) {
					buffer = NclRealloc(buffer,bsize + 1);
					cur_buf_size = bsize + 1;
				}
				nmaps = SWinqmaps(SWid, buffer, off, inc);
				HDFEOSIntAddDimMapInfo(the_file,sw_ncl_names[i],nmaps,buffer,off,inc);
				NclFree(off);
				NclFree(inc);
			}
			/* Indexed Dimension Mappings */
			nmaps = SWnentries(SWid, HDFE_NENTIMAP, &bsize);
			if (nmaps > 0) {
				int32 *sizes;
				sizes = NclMalloc(nmaps * sizeof(int32));
				if (bsize + 1 > cur_buf_size) {
					buffer = NclRealloc(buffer,bsize + 1);
					cur_buf_size = bsize + 1;
				}
				nmaps = SWinqidxmaps(SWid, buffer, sizes);
				HDFEOSIntAddIndexedMapVars(the_file,sw_hdf_names[i],sw_ncl_names[i],nmaps,buffer,sizes);
				the_file->n_vars += nmaps;
			}
			
			/* Geolocation fields */

			ngeofields = SWnentries(SWid, HDFE_NENTGFLD, &bsize);
			if (ngeofields > max_fields) {
				max_fields = ngeofields;
				field_ranks = NclRealloc(field_ranks,max_fields * sizeof(int32));
			}
			if (ngeofields > 0) {
				if (bsize + 1 > cur_buf_size) {
					buffer = NclRealloc(buffer,bsize + 1);
					cur_buf_size = bsize + 1;
				}
				ngeofields = SWinqgeofields(SWid,buffer,field_ranks,NULL);
				the_file->n_vars += ngeofields;
				HDFEOSParseName(buffer,&var_hdf_names,&var_ncl_names,ngeofields);
				for(j = 0; j < ngeofields; j++) {
					if (field_ranks[j] > ndims) {
						ndims = field_ranks[j];
						dimsizes = NclRealloc(dimsizes,ndims * sizeof(int32));
					}
					if(SWfieldinfo(SWid,NrmQuarkToString(var_hdf_names[j]),
						       &tmp_rank,dimsizes,&tmp_type,buffer) == 0) {
						HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
						for(k = 0; k < tmp_rank; k++) {
							HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),
									tmp_names[k],tmp_ncl_names[k],dimsizes[k],sw_hdf_names[i],
									sw_ncl_names[i],the_file->file_path_q);
						}
						HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],
								the_file->dims,tmp_rank,dimsizes,tmp_type,tmp_ncl_names,
								SWATH,sw_hdf_names[i],sw_ncl_names[i]);
						if(SWgetfillvalue(SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1) {
							tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
							*tmp_missing = missing;
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("_FillValue"),
									(void*)tmp_missing,1,HDFEOSMapTypeNumber(tmp_type));
						}
						if(HDFEOSunsigned(tmp_type)) {
							is_unsigned = (int*)NclMalloc(sizeof(int));
							*is_unsigned = 1;
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),
									(void*)is_unsigned,1,NCL_logical);
						}
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = var_hdf_names[j];
						HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("hdfeos_name"),(void*)tmp_value,1,NCL_string);
						if (var_hdf_names[j] == NrmStringToQuark("Longitude")) {
							NrmQuark *qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("degrees_east");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
									(void*)qval,1,NCL_string);
							qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("longitude");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
									(void*)qval,1,NCL_string);
							lon_name = the_file->vars->var_inq->name;
							if (tmp_rank == 2) {
								y_dim_num = the_file->vars->var_inq->dim[0];
								x_dim_num = the_file->vars->var_inq->dim[1];
							}
						}
						else if (var_hdf_names[j] == NrmStringToQuark("Latitude")) {
							NrmQuark *qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("degrees_north");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
									(void*)qval,1,NCL_string);
							qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("latitude");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
									(void*)qval,1,NCL_string);
							lat_name = the_file->vars->var_inq->name;
						}
						else if (var_hdf_names[j] == NrmStringToQuark("Colatitude")) {
							NrmQuark *qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("degrees");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
									(void*)qval,1,NCL_string);
							qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("colatitude");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
									(void*)qval,1,NCL_string);
							lat_name = the_file->vars->var_inq->name;
						}
						else if (var_hdf_names[j] == NrmStringToQuark("Time") && HDFEOSMapTypeNumber(tmp_type) == NCL_double) {
							NrmQuark *qval;
							qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("units value presumes use of TAI93 (International Atomic Time) format");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("Note"),
									(void*)qval,1,NCL_string);
							qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("seconds since 1993-1-1 00:00:00");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),
									(void*)qval,1,NCL_string);
							qval = (NrmQuark *)NclMalloc(sizeof(NrmQuark));
							*qval = NrmStringToQuark("time");
							HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),
									(void*)qval,1,NCL_string);
						}
							    
						NclFree(tmp_ncl_names);
						NclFree(tmp_names);
					}

				}
				NclFree(var_hdf_names);
				NclFree(var_ncl_names);
			}

			ndata = SWnentries(SWid, HDFE_NENTDFLD, &bsize);
			if (bsize + 1 > cur_buf_size) {
				buffer = NclRealloc(buffer,bsize + 1);
				cur_buf_size = bsize + 1;
			}
			if (ndata > max_fields) {
				max_fields = ndata;
				field_ranks = NclRealloc(field_ranks,max_fields * sizeof(int32));
			}
			ndata = SWinqdatafields(SWid,buffer,field_ranks,NULL);
			the_file->n_vars += ndata;
			HDFEOSParseName(buffer,&var_hdf_names,&var_ncl_names,ndata);
			for(j = 0; j < ndata; j++) {
				if (field_ranks[j] > ndims) {
					ndims = field_ranks[j];
					dimsizes = NclRealloc(dimsizes,ndims * sizeof(int32));
				}
				if(SWfieldinfo(SWid,NrmQuarkToString(var_hdf_names[j]),
					       &tmp_rank,dimsizes,&tmp_type,buffer) == 0) {
					HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
					for(k = 0; k < tmp_rank; k++) {
						HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),
								tmp_names[k],tmp_ncl_names[k],dimsizes[k],sw_hdf_names[i],
								sw_ncl_names[i],the_file->file_path_q);
					}
					HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],
							the_file->dims,tmp_rank,dimsizes,tmp_type,tmp_ncl_names,
							SWATH,sw_hdf_names[i],sw_ncl_names[i]);
					if(SWgetfillvalue(SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1) {
						tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
						*tmp_missing = missing;
						HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("_FillValue"),
								(void*)tmp_missing,1,HDFEOSMapTypeNumber(tmp_type));


					}
					if(HDFEOSunsigned(tmp_type)) {
						is_unsigned = (int*)NclMalloc(sizeof(int));
						*is_unsigned = 1;
						HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),
								(void*)is_unsigned,1,NCL_logical);
					}
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = var_hdf_names[j];
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("hdfeos_name"),(void*)tmp_value,1,NCL_string);
					
					NclFree(tmp_ncl_names);
					NclFree(tmp_names);
				}
			}
			if (! (lon_name == NrmNULLQUARK || lat_name == NrmNULLQUARK ||
			       y_dim_num == -1 || x_dim_num == -1)) { 
				for (vstep = the_file->vars; vstep != NULL; vstep = vstep->next) {
					if (vstep->var_inq->var_class != SWATH ||
					    vstep->var_inq->var_class_name != sw_hdf_names[i] ||
					    vstep->var_inq->name == lat_name ||
					    vstep->var_inq->name == lon_name ||
					    vstep->var_inq->n_dims < 2 ||
					    vstep->var_inq->dim[vstep->var_inq->n_dims-1] != x_dim_num ||
					    vstep->var_inq->dim[vstep->var_inq->n_dims-2] != y_dim_num)
						continue;
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					sprintf(buffer,"%s, %s",NrmQuarkToString(lat_name),NrmQuarkToString(lon_name));
					*(NclQuark*)tmp_value = NrmStringToQuark(buffer);
					HDFEOSIntAddAtt(vstep->var_inq,NrmStringToQuark("coordinates"),(void*)tmp_value,1,NCL_string);
				}
			}
			NclFree(var_hdf_names);
			NclFree(var_ncl_names);
			SWdetach(SWid);	
			if (ndims > 0)
				NclFree(dimsizes);
		}
		SWclose(SWfid);
		NclFree(sw_hdf_names);
		NclFree(sw_ncl_names);
	}

	if((ngd = GDinqgrid(NrmQuarkToString(path),NULL,&bsize)) > 0) {
		if (bsize + 1 > cur_buf_size) {
			buffer = NclRealloc(buffer,bsize + 1);
			cur_buf_size = bsize + 1;
		}
		ngd = GDinqgrid(NrmQuarkToString(path),buffer,&bsize);
		GDfid = GDopen(NrmQuarkToString(path),DFACC_READ);
		HDFEOSParseName(buffer,&gd_hdf_names,&gd_ncl_names,ngd);
		for(i = 0; i < ngd; i++) {
			int k;
			int32 pixregcode;
			intn status;
			int has_xdim_var = 0, has_ydim_var = 0;
			NrmQuark xdim_name = NrmNULLQUARK, ydim_name = NrmNULLQUARK;
			NrmQuark qproj_name = NrmNULLQUARK;

			GDid = GDattach(GDfid,NrmQuarkToString(gd_hdf_names[i]));
			if(! (GDid > 0) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",
					  NrmQuarkToString(path));
				return(NULL);
			}
			status = GDprojinfo(GDid,&projcode,&zonecode,&spherecode,projparm);
			if (status == FAIL) {
				projcode = -1;
			}
			else {
				switch(projcode) {
				case GCTP_GEO:
					qproj_name = NrmStringToQuark("Geographic");
					break;
				case GCTP_UTM:
					qproj_name = NrmStringToQuark("Universal Transverse Mercator");
					break;
				case GCTP_ALBERS:
					qproj_name = NrmStringToQuark("Albers Conical Equal_Area");
					break;
				case GCTP_LAMCC:
					qproj_name = NrmStringToQuark("Lambert Conformal Conic");
					break;
				case GCTP_MERCAT:
					qproj_name = NrmStringToQuark("Mercator");
					break;
				case GCTP_PS:
					qproj_name = NrmStringToQuark("Polar Stereographic");
					break;
				case GCTP_POLYC:
					qproj_name = NrmStringToQuark("Polyconic");
					break;
				case GCTP_TM:
					qproj_name = NrmStringToQuark("Transverse Mercator");
					break;
				case GCTP_LAMAZ:
					qproj_name = NrmStringToQuark("Lambert Azimuthal Equal Area");
					break;
				case GCTP_HOM:
					qproj_name = NrmStringToQuark("Hotine Oblique Mercator");
					break;
				case GCTP_SOM:
					qproj_name = NrmStringToQuark("Space Oblique Mercator");
					break;
				case GCTP_GOOD:
					qproj_name = NrmStringToQuark("Interrupted Goode Homolosine");
					break;
				case GCTP_ISINUS:
				case GCTP_ISINUS1:
					qproj_name = NrmStringToQuark("Integerized Sinusoidal Projection");
					break;
				case GCTP_CEA:
				case GCTP_BCEA:
					qproj_name = NrmStringToQuark("Cylindrical Equal-Area Projection");
					break;
				}
			}			
			natts = GDinqattrs(GDid,NULL,&bsize);
			if(natts > 0 ) {
				if (bsize + 1 > cur_buf_size) {
					buffer = NclRealloc(buffer,bsize + 1);
					cur_buf_size = bsize + 1;
				}
				natts = GDinqattrs(GDid,buffer,&bsize);
				HDFEOSParseName(buffer,&att_hdf_names,&att_ncl_names,natts);
				for(k = 0; k < natts; k++) { 
					if(GDattrinfo(GDid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0) {
						tmp_value = (void*)NclMalloc(att_size);
						if(GDreadattr(GDid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 ) {
							HDFEOSIntFileAddAtt(the_file,gd_ncl_names[i],att_ncl_names[k],tmp_value,
									    att_size/_NclSizeOf(HDFEOSMapTypeNumber(att_type)),HDFEOSMapTypeNumber(att_type));
						}
					}
				}
				NclFree(att_hdf_names);
				NclFree(att_ncl_names);
			}
			ndims = GDnentries(GDid,HDFE_NENTDIM,&bsize);
			if(ndims > 0 ) {
				dimsizes = NclMalloc(ndims * sizeof(int32));
				if (bsize + 1 > cur_buf_size) {
					buffer = NclRealloc(buffer,bsize + 1);
					cur_buf_size = bsize + 1;
				}
				ndims = GDinqdims(GDid,buffer,dimsizes);
				HDFEOSParseName(buffer,&dim_hdf_names,&dim_ncl_names,ndims);
				for(j = 0; j < ndims; j++) {
					HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],
							dimsizes[j],gd_hdf_names[i],gd_ncl_names[i],the_file->file_path_q);
				}
				NclFree(dim_hdf_names);
				NclFree(dim_ncl_names);
			}

			ndata = GDnentries(GDid, HDFE_NENTDFLD, &bsize);
			if (ndata > max_fields) {
				max_fields = ndata;
				field_ranks = NclRealloc(field_ranks,max_fields * sizeof(int32));
			}
			if (bsize + 1 > cur_buf_size) {
				buffer = NclRealloc(buffer,bsize + 1);
				cur_buf_size = bsize + 1;
			}
			ndata = GDinqfields(GDid,buffer,field_ranks,NULL);
			the_file->n_vars += ndata;
			HDFEOSParseName(buffer,&var_hdf_names,&var_ncl_names,ndata);
			for(j = 0; j < ndata; j++) {
				int has_xdim = 0, has_ydim = 0;
				HDFEOSVarInqRec *data_var = NULL;
				if (field_ranks[j] > ndims) {
					if (ndims == 0)
						dimsizes = NclMalloc(field_ranks[j] * sizeof(int32));
					else 
						dimsizes = NclRealloc(dimsizes,field_ranks[j] * sizeof(int32));
					ndims = field_ranks[j];
				}
				if (NrmStringToQuark("XDim") == var_hdf_names[j]) 
					has_xdim_var = 1;
				if (NrmStringToQuark("YDim") == var_hdf_names[j]) 
					has_ydim_var = 1;
				if(GDfieldinfo(GDid,NrmQuarkToString(var_hdf_names[j]),&tmp_rank,dimsizes,&tmp_type,buffer) == 0) {
					HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
					for(k = 0; k < tmp_rank; k++) {
						if (tmp_names[k] == NrmStringToQuark("XDim")) 
							has_xdim = 1;
						if (tmp_names[k] == NrmStringToQuark("YDim")) 
							has_ydim = 1;
						HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),tmp_names[k],tmp_ncl_names[k],
								dimsizes[k],gd_hdf_names[i],gd_ncl_names[i],the_file->file_path_q);
					}
						

					HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],
							the_file->dims,tmp_rank,dimsizes,tmp_type,tmp_ncl_names,GRID,gd_hdf_names[i],gd_ncl_names[i]);
					data_var = the_file->vars->var_inq;
					if(GDgetfillvalue(GDid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1) {
						tmp_missing = (NclScalar*)NclMalloc(sizeof(NclScalar));
						*tmp_missing = missing;
						HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("_FillValue"),(void*)tmp_missing,1,HDFEOSMapTypeNumber(tmp_type));


					}
					if(HDFEOSunsigned(tmp_type)) {
						is_unsigned = (int*)NclMalloc(sizeof(int));
						*is_unsigned = 1;
						HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),(void*)is_unsigned,1,NCL_logical);
					}
					NclFree(tmp_ncl_names);
					NclFree(tmp_names);
				}
				if (projcode > -1 && has_xdim && has_ydim) {
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = qproj_name;
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
				}
				tmp_value = (void*)NclMalloc(sizeof(NclQuark));
				*(NclQuark*)tmp_value = var_hdf_names[j];
				HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("hdfeos_name"),(void*)tmp_value,1,NCL_string);
			}

			status = GDorigininfo(GDid,&origincode);
			if (status == SUCCEED)
				status = GDpixreginfo(GDid,&pixregcode);
			if (status == SUCCEED)
				status = GDgridinfo(GDid,&xdimsize,&ydimsize,upper_left,lower_right);
			if (status == SUCCEED)
				status = GDprojinfo(GDid,&projcode,&zonecode,&spherecode,projparm);
			if (status == FAIL) {
				NhlPError(NhlWARNING,NhlEUNKNOWN, 
					  "NclHDFEOS: Invalid projection information for GRID (%s); no coordinates will be provided",
					  NrmQuarkToString(gd_hdf_names[i]));
			}
			else {
				NrmQuark dim_names[2];
				int32 dim_sizes[2];

				dim_names[1] = NrmStringToQuark("XDim");
				HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_names[1],dim_names[1],
						xdimsize,gd_hdf_names[i],gd_ncl_names[i],the_file->file_path_q);
				dim_names[0] = NrmStringToQuark("YDim");
				HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_names[0],dim_names[0],
						ydimsize,gd_hdf_names[i],gd_ncl_names[i],the_file->file_path_q);
				dim_sizes[0] = ydimsize;
				dim_sizes[1] = xdimsize;
				if (projcode == GCTP_GEO) { /* 1D coordinate */
					HDFEOSVarInqRec *var = NULL;
					if (! has_xdim_var) {
						the_file->n_vars++;
						HDFEOSIntAddVar(&(the_file->vars),NrmStringToQuark("lon"),NrmStringToQuark("XDim"),
								the_file->dims,1,&(dim_sizes[1]),DFNT_FLOAT64,&(dim_names[1]),
								GRID,gd_hdf_names[i],gd_ncl_names[i]);
						var = the_file->vars->var_inq;

					}
					else {
						/* some files provide coordinate variables, we don't need to duplicate */
						for (vstep = the_file->vars; vstep != NULL; vstep = vstep->next) {
							if (vstep->var_inq->hdf_name == NrmStringToQuark("XDim")) {
								var = vstep->var_inq;
								break;
							}
						}
					}
					if (var) {
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = NrmStringToQuark("degrees_east");
						HDFEOSIntAddAtt(var,NrmStringToQuark("units"),(void*)tmp_value,1,NCL_string);
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = NrmStringToQuark("longitude");
						HDFEOSIntAddAtt(var,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
						xdim_name = var->name;
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = qproj_name;
						HDFEOSIntAddAtt(var,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
					}
					var = NULL;
					if (! has_ydim_var) {
						the_file->n_vars++;
						HDFEOSIntAddVar(&(the_file->vars),NrmStringToQuark("lat"),NrmStringToQuark("YDim"),
								the_file->dims,1,&(dim_sizes[0]),DFNT_FLOAT64,&(dim_names[0]),
								GRID,gd_hdf_names[i],gd_ncl_names[i]);
						var = the_file->vars->var_inq;
					}
					else {
						/* some files provide coordinate variables, we don't need to duplicate */
						for (vstep = the_file->vars; vstep != NULL; vstep = vstep->next) {
							if (vstep->var_inq->hdf_name == NrmStringToQuark("YDim")) {
								var = vstep->var_inq;
								break;
							}
						}
					}
					if (var) {
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = NrmStringToQuark("degrees_north");
						HDFEOSIntAddAtt(var,NrmStringToQuark("units"),(void*)tmp_value,1,NCL_string);
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = NrmStringToQuark("latitude");
						HDFEOSIntAddAtt(var,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
						tmp_value = (void*)NclMalloc(sizeof(NclQuark));
						*(NclQuark*)tmp_value = qproj_name;
						HDFEOSIntAddAtt(var,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
						ydim_name = var->name;
					}
				}
				else {
					int32 cols[4],rows[4];
					double lat2d[4], lon2d[4];
					double *corners;

					cols[0] = rows[0] = 0;
					cols[1] = xdimsize - 1;
					rows[1] = 0;
					cols[2] = xdimsize - 1;
					rows[2] = ydimsize - 1;
					cols[3] = 0;
					rows[3] = ydimsize - 1;
					GDij2ll(projcode,zonecode,projparm,spherecode,xdimsize,ydimsize,
						upper_left,lower_right,4,rows,cols,lon2d,lat2d,pixregcode,origincode);
					
					the_file->n_vars += 2;
					HDFEOSIntAddVar(&(the_file->vars),NrmStringToQuark("lon"),NrmStringToQuark("GridLon"),
							the_file->dims,2,dim_sizes,DFNT_FLOAT64,dim_names,GRID,gd_hdf_names[i],gd_ncl_names[i]);
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = NrmStringToQuark("degrees_east");
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),(void*)tmp_value,1,NCL_string);
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = NrmStringToQuark("longitude");
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
					corners = (double*)NclMalloc(4 * sizeof(double));
					memcpy(corners,lon2d,4 * sizeof(double));
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("corners"),(void*)corners,4,NCL_double);
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = qproj_name;
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
					xdim_name = the_file->vars->var_inq->name;
					
					HDFEOSIntAddVar(&(the_file->vars),NrmStringToQuark("lat"),NrmStringToQuark("GridLat"),
							the_file->dims,2,dim_sizes,DFNT_FLOAT64,dim_names,GRID,gd_hdf_names[i],gd_ncl_names[i]);
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = NrmStringToQuark("degrees_north");
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("units"),(void*)tmp_value,1,NCL_string);
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = NrmStringToQuark("latitude");
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("long_name"),(void*)tmp_value,1,NCL_string);
					corners = (double*)NclMalloc(4 * sizeof(double));
					memcpy(corners,lat2d,4 * sizeof(double));
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("corners"),(void*)corners,4,NCL_double);
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					*(NclQuark*)tmp_value = qproj_name;
					HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
					ydim_name = the_file->vars->var_inq->name;
				}
			}
			/* add the "coordinates" attribute if not a 1D coordinate and the coordinates have been defined properly */
			if (projcode != GCTP_GEO && ! (xdim_name == NrmNULLQUARK || ydim_name == NrmNULLQUARK)) { 
				for (vstep = the_file->vars; vstep != NULL; vstep = vstep->next) {
					if (vstep->var_inq->var_class != GRID ||
					    vstep->var_inq->var_class_name != gd_hdf_names[i] ||
					    vstep->var_inq->hdf_name == NrmStringToQuark("lon") ||
					    vstep->var_inq->hdf_name == NrmStringToQuark("lat"))
						continue;
					tmp_value = (void*)NclMalloc(sizeof(NclQuark));
					sprintf(buffer,"%s, %s",NrmQuarkToString(ydim_name),NrmQuarkToString(xdim_name));
					*(NclQuark*)tmp_value = NrmStringToQuark(buffer);
					HDFEOSIntAddAtt(vstep->var_inq,NrmStringToQuark("coordinates"),(void*)tmp_value,1,NCL_string);
				}
			}
			GDdetach(GDid);	
			NclFree(var_hdf_names);
			NclFree(var_ncl_names);
			if (ndims > 0)
				NclFree(dimsizes);
		}
		GDclose(GDfid);
		NclFree(gd_hdf_names);
		NclFree(gd_ncl_names);
	}

	if((npt = PTinqpoint(NrmQuarkToString(path),NULL,&bsize)) > 0)  {
		NhlPError(NhlWARNING,NhlEUNKNOWN, "NclHDFEOS: File (%s) contains %d Point data sets; NCL does not yet handle this type of data",
			  NrmQuarkToString(path),npt);
		if (bsize + 1 > cur_buf_size) {
			buffer = NclRealloc(buffer,bsize + 1);
			cur_buf_size = bsize + 1;
		}
		npt = PTinqpoint(NrmQuarkToString(path),buffer,&bsize);
		PTfid = PTopen(NrmQuarkToString(path),DFACC_READ);
		HDFEOSParseName(buffer,&pt_hdf_names,&pt_ncl_names,npt);
		for(i = 0; i < npt; i++) {
			PTid = PTattach(PTfid,NrmQuarkToString(pt_hdf_names[i]));
			if(! (PTid > 0) ) {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",NrmQuarkToString(path));
				return(NULL);
			}
			nlv = PTnlevels(PTid);
			if(nlv > 0) {
				for(j = 0; j < nlv; j++) {
					nrc = PTnrecs(PTid,j);
					nfd = PTnfields(PTid,j,&bsize);
					if (bsize + 1 > cur_buf_size) {
						buffer = NclRealloc(buffer,bsize + 1);
						cur_buf_size = bsize + 1;
					}
					fldtype  = (int32*)NclMalloc(sizeof(int32)*nfd);
					fldorder = (int32*)NclMalloc(sizeof(int32)*nfd);
					nfd = PTlevelinfo(PTid,j,buffer,fldtype,fldorder);
					HDFEOSParseName(buffer,&var_hdf_names,&var_ncl_names,nfd);
					for(k = 0; k < nfd; k++) {
						fprintf(stdout,"Point data set %s\n",NrmQuarkToString(var_hdf_names[k]));
					}
					NclFree(fldtype);
					NclFree(fldorder);
					NclFree(var_hdf_names);
					NclFree(var_ncl_names);
				}
			}
			PTdetach(PTid);	
		}
		PTclose(PTfid);
		NclFree(pt_hdf_names);
		NclFree(pt_ncl_names);
	}

#if 0
	{
		{
			ndims = PTinqdims(PTid,dimnames,dims);
			if(ndims != -1) {
				HDFEOSParseName(dimnames,&dim_hdf_names,&dim_ncl_names,ndims);
				for(j = 0; j < ndims; j++) {
					HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],
							PTdiminfo(PTid,NrmQuarkToString(dim_hdf_names[j])),pt_hdf_names[i],pt_ncl_names[i],the_file->file_path_q);
				}
				ndata = PTinqfields(PTid,fieldlist,NULL,NULL);
				if(ndata != -1) {
					the_file->n_vars += ndata;
					HDFEOSParseName(fieldlist,&var_hdf_names,&var_ncl_names,ndata);
					for(j = 0; j < ndata; j++) {
						if(PTfieldinfo(PTid,NrmQuarkToString(var_hdf_names[j]),&tmp_rank,dims,&tmp_type,buffer) == 0) {
							HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
							for(k = 0; k < tmp_rank; k++) {
								HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),tmp_names[k],
										tmp_ncl_names[k],dims[k],pt_hdf_names[i],pt_ncl_names[i],the_file->file_path_q);
							}
						

							HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],the_file->dims,
									tmp_rank,dims,tmp_type,tmp_ncl_names,GRID,pt_hdf_names[i],pt_ncl_names[i]);
							if(HDFEOSunsigned(tmp_type)) {
								is_unsigned = (int*)NclMalloc(sizeof(int));
								*is_unsigned = 1;
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("unsigned"),(void*)is_unsigned,1,NCL_logical);
							}
							natts = PTinqattrs(PTid,atts,&bsize);
							if(natts > 0 ) {
								HDFEOSParseName(atts,&att_hdf_names,&att_ncl_names,natts);
								for(k = 0; k < natts; k++) { 
									if(PTattrinfo(PTid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0) {
										tmp_value = (void*)NclMalloc(att_size);
										if(PTreadattr(PTid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 ) {
											HDFEOSIntAddAtt(the_file->vars->var_inq,att_ncl_names[k],tmp_value,
													att_size/_NclSizeOf(HDFEOSMapTypeNumber(att_type)),HDFEOSMapTypeNumber(att_type));
										}
			
									}
								}
								NclFree(att_hdf_names);
								NclFree(att_ncl_names);
							}


							NclFree(tmp_ncl_names);
							NclFree(tmp_names);
						}
					}
				}

				natts = PTinqattrs(GDid,atts,&bsize);
				if(natts > 0 ) {
					HDFEOSParseName(atts,&att_hdf_names,&att_ncl_names,natts);
					for(j = 0; j < natts; j++) {
						HDFEOSIntAddAtt(the_file->vars->var_inq,att_hdf_names[j],att_ncl_names[j]);
						fprintf(stdout,"%s\n",NrmQuarkToString(att_ncl_names[j]));
					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",NrmQuarkToString(path));
				return(NULL);
			}
		}
	}
			
#endif

	NclFree(field_ranks);
	NclFree(buffer);
			
	if((npt==0)&&(nsw == 0) && (ngd ==0)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS: No swath, grid or point data found. File is not HDFEOS-4");
		NclFree(the_file);
		return(NULL);
	}
	return(the_file);
}
static void HDFEOSFreeFileRec
#if	NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList * thevars,*tmpvar;
	HDFEOSAttInqRecList * theatts,*tmpatt;
	HDFEOSDimInqRecList *thedims,*tmpdim;
	
	thevars = thefile->vars;
	while(thevars != NULL) {
		theatts = thevars->var_inq->att_int_list;
		while(theatts!= NULL) {
			NclFree(theatts->att_inq->value);
			NclFree(theatts->att_inq);
			tmpatt = theatts;
			theatts=theatts->next;
			NclFree(tmpatt);
		}
		NclFree(thevars->var_inq);
		
		tmpvar = thevars;
		thevars = thevars->next;
		NclFree(tmpvar);
	}
	thedims = thefile->dims;
	while (thedims != NULL) {
		NclFree(thedims->dim_inq);
		tmpdim = thedims;
		thedims = thedims->next;
		NclFree(tmpdim);
	}
	theatts = thefile->att_int_list;
	while (theatts != NULL) {
		NclFree(theatts->att_inq->value);
		NclFree(theatts->att_inq);
		tmpatt = theatts;
		theatts = theatts->next;
		NclFree(tmpatt);
	}
	NclFree(thefile);
}
static NclQuark* HDFEOSGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList * thelist;
	int i;
	NclQuark *names;

	names = NclMalloc(sizeof(NclQuark)*thefile->n_vars);
	*num_vars = thefile->n_vars;

	thelist = thefile->vars;
	for (i = 0; i < thefile->n_vars; i++) {
		names[i] = thelist->var_inq->name;
		thelist = thelist->next;
	}
	return(names);
	
}
static NclFVarRec *HDFEOSGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif

{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList * thelist;
	NclFVarRec *var_info = NclMalloc(sizeof(NclFVarRec));

	int i,j;


	thelist = thefile->vars;
	for (i = 0; i < thefile->n_vars; i++) {
		if(thelist->var_inq->name == var_name) {
			var_info->var_name_quark = var_name;
			var_info->var_full_name_quark = var_name;
			var_info->var_real_name_quark = var_name;
			var_info->data_type = HDFEOSMapTypeNumber(thelist->var_inq->typenumber);
			var_info->num_dimensions = thelist->var_inq->n_dims;
			for(j = 0; j < thelist->var_inq->n_dims; j++) {
				var_info->file_dim_num[j] = thelist->var_inq->dim[j];
			}
			return(var_info);
		}
		thelist = thelist->next;
	}
	return NULL;
}
static NclQuark *HDFEOSGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSDimInqRecList * thelist;
	NclQuark* names; 
	int i;

	thelist = thefile->dims;
	names = NclMalloc(sizeof(NclQuark)*thefile->n_dims);
	*num_dims = thefile->n_dims;
	for(i =0; i < thefile->n_dims; i++) {
		names[thelist->dim_inq->ncldim_id] = thelist->dim_inq->name;
		thelist=thelist->next;
	}
	return(names);

}
static NclFDimRec *HDFEOSGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSDimInqRecList * thelist;
	NclFDimRec *dim_info = (NclFDimRec*) NclMalloc(sizeof(NclFDimRec));
	int i;

	thelist = thefile->dims;
	for(i =0; i < thefile->n_dims; i++) {
		if(dim_name_q == thelist->dim_inq->name) {
			dim_info->dim_name_quark = dim_name_q;
			dim_info->is_unlimited = thelist->dim_inq->is_unlimited;
			dim_info->dim_size = thelist->dim_inq->size;
			return(dim_info);
		}
		thelist= thelist->next;
	}
	return NULL;
}
static NclQuark *HDFEOSGetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSAttInqRecList * the_int_att_list;
	NclQuark* output = NULL;

	*num_atts = 0;
	if(thefile->n_int_atts > 0) {
		output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(thefile->n_int_atts));
		the_int_att_list = thefile->att_int_list;
		while(the_int_att_list != NULL) {
			output[*num_atts] = the_int_att_list->att_inq->name;
			*num_atts += 1;
			the_int_att_list = the_int_att_list->next;
		}
	}
	return(output);
}
static NclFAttRec* HDFEOSGetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSAttInqRecList * the_int_att_list;
	NclFAttRec* output = NULL;

	if(thefile->n_int_atts > 0) {
		the_int_att_list = thefile->att_int_list;
		while(the_int_att_list != NULL) {
			if(att_name_q == the_int_att_list->att_inq->name) {
				output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
				output->att_name_quark = att_name_q;
				output->data_type = the_int_att_list->att_inq->type;
				output->num_elements = the_int_att_list->att_inq->n_elem;
				return(output);
			} else {
				the_int_att_list = the_int_att_list->next;
			}
		}
	}
	return(output);
}
static NclQuark *HDFEOSGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList * thelist;
	HDFEOSAttInqRecList * the_int_att_list;
	NclQuark* output = NULL;
	int i;

	*num_atts = 0;
	thelist = thefile->vars;
	for(i = 0; i < thefile->n_vars; i++) {
		if(thevar == thelist->var_inq->name) {
			output = (NclQuark*)NclMalloc(sizeof(NclQuark)*(thelist->var_inq->n_int_atts));
			the_int_att_list = thelist->var_inq->att_int_list;
			while(the_int_att_list != NULL) {
				output[*num_atts] = the_int_att_list->att_inq->name;
				*num_atts += 1;
				the_int_att_list = the_int_att_list->next;
			}
			return(output);
		} else {
			thelist = thelist->next;
		}
	}
	return(NULL);
}
static NclFAttRec *HDFEOSGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList * thelist;
	HDFEOSAttInqRecList * the_int_att_list;
	NclFAttRec* output = NULL;
	int i;

	thelist = thefile->vars;
	for(i = 0; i < thefile->n_vars; i++) {
		if(thevar == thelist->var_inq->name) {
			the_int_att_list = thelist->var_inq->att_int_list;
			while(the_int_att_list != NULL) {
				if(theatt == the_int_att_list->att_inq->name) {
					output = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
					output->att_name_quark = theatt;
					output->data_type = the_int_att_list->att_inq->type;
					output->num_elements = the_int_att_list->att_inq->n_elem;
					return(output);
				} else {
					the_int_att_list = the_int_att_list->next;
				}
			}
		} else {
			thelist = thelist->next;
		}
	}
	return(NULL);
}
static NclFVarRec *HDFEOSGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
	return NULL;
}

static int ReadCoordVar
(int32 GDid, HDFEOSVarInqRec *var, int32 *start, int32 *stride, int32 *edge, void *storage)
{
	int32 origincode;
	int32 projcode;
	int32 zonecode;
	int32 spherecode;
	int32 pixregcode;
	int32 xdimsize,ydimsize;
	float64 upper_left[2],lower_right[2];
	float64 projparm[15];
	int32 *cols, *rows;
	int i, j;
	int total;
	double *latitude, *longitude;
	intn status;
	int islon;

	status = GDorigininfo(GDid,&origincode);
	if (status == SUCCEED)
		status = GDpixreginfo(GDid,&pixregcode);
	if (status == SUCCEED)
		status = GDgridinfo(GDid,&xdimsize,&ydimsize,upper_left,lower_right);
	if (status == SUCCEED)
		status = GDprojinfo(GDid,&projcode,&zonecode,&spherecode,projparm);
	if (status == FAIL) {
		return -1;
	}

	if (var->hdf_name == NrmStringToQuark("lon")) {
		islon = 1;
	}
	else {
		islon = 0;
	}
	if (var->n_dims == 1) {
		total = edge[0];

		cols = NclMalloc(total * sizeof(int32));
		rows = NclMalloc(total * sizeof(int32));
		if (islon) {
			for (i = 0; i < total; i++) {
				int32 jx = 0;
				int32 ix = start[0] + i * stride[0];
				cols[i] = ix;
				rows[i] = jx;
			}
		}
		else {
			for (j = 0; j < total; j++) {
				int32 jx =  start[0] + j * stride[0];
				int32 ix = 0;
				cols[j] = ix;
				rows[j] = jx;
			}
		}
	}
	else {
		total = edge[0] * edge[1];
		cols = NclMalloc(total * sizeof(int32));
		rows = NclMalloc(total * sizeof(int32));
		for (j = 0; j < edge[0]; j++) {
			for (i = 0; i < edge[1]; i++) {
				int32 jx = start[0] + j * stride[0];
				int32 ix = start[1] + i * stride[1];
				cols[j * edge[1] + i] = ix;
				rows[j * edge[1] + i] = jx;
			}
		}
	}
	if (islon) {
		latitude = NclMalloc(total * sizeof(double));
		longitude = (double *) storage;
	}
	else {
		longitude = NclMalloc(total * sizeof(double));
		latitude = (double *) storage;
	}

	GDij2ll(projcode,zonecode,projparm,spherecode,xdimsize,ydimsize,
		upper_left,lower_right,total,rows,cols,longitude,latitude,pixregcode,origincode);
				
	if (islon)
		NclFree(latitude);
	else
		NclFree(longitude);

		
	NclFree(rows);
	NclFree(cols);

	return 0;
}


static void *HDFEOSReadVar
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
        HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList *thelist;
	int i,j,out = 0;
	int32 fid; 
	int32 did; 
	int32 starti[NCL_MAX_DIMENSIONS];
	int32 stridei[NCL_MAX_DIMENSIONS];
	int32 edgei[NCL_MAX_DIMENSIONS];
	float tmpf;
	

	thelist = thefile->vars;
	for(i = 0; i < thefile->n_vars; i++) {
		if(thevar == thelist->var_inq->name ) {
			switch(thelist->var_inq->var_class) {
			case GRID:
				fid = GDopen(NrmQuarkToString(thefile->file_path_q),DFACC_READ);
				did = GDattach(fid,NrmQuarkToString(thelist->var_inq->var_class_name));
				for(j = 0; j < thelist->var_inq->n_dims; j++) {
					starti[j] = (int32)start[j] ;
					stridei[j] = (int32)stride[j];
					tmpf = stridei[j];
	                                edgei[j] =(int)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
				}
				if (thelist->var_inq->hdf_name == NrmStringToQuark("lat") ||
				    thelist->var_inq->hdf_name == NrmStringToQuark("lon")) { /* a coordinate variable */
					out = ReadCoordVar(did,thelist->var_inq,starti,stridei,edgei,storage);
				}
				else {
					out = GDreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
				}
				if(out == 0) {
					GDdetach(did);
					GDclose(fid);
					return(storage);
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS: Error ocurred while reading can't continue");
					return(NULL);
				}
				break;
			case SWATH:
				fid = SWopen(NrmQuarkToString(thefile->file_path_q),DFACC_READ);
				did = SWattach(fid,NrmQuarkToString(thelist->var_inq->var_class_name));
				for(j = 0; j < thelist->var_inq->n_dims; j++) {
					starti[j] = (int32)start[j] ;
					stridei[j] = (int32)stride[j];
					tmpf = stridei[j];
	                                edgei[j] =(int)(fabs(((double)(finish[j] - start[j]))) /tmpf) + 1;
				}
				if (thelist->var_inq->index_dim != NrmNULLQUARK) {
					int32 dimsize = SWdiminfo(did,NrmQuarkToString(thelist->var_inq->index_dim));
					if (edgei[0] == dimsize) {
						 SWidxmapinfo(did,NrmQuarkToString(thelist->var_inq->index_dim),
							      NrmQuarkToString(thelist->var_inq->hdf_name),storage);
					}
					else {
						int32* tmpout;
						tmpout = NclMalloc(sizeof(int32) * dimsize);
						SWidxmapinfo(did,NrmQuarkToString(thelist->var_inq->index_dim),
							     NrmQuarkToString(thelist->var_inq->hdf_name),tmpout);
						for (j = 0; j < edgei[0]; j++) {
							*(((int*)storage) + j) = *(tmpout + starti[0] + stridei[0] * j);
						}
						NclFree(tmpout);
					}
				}
				else {
					out = SWreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
				}
				if(out == 0) {
					SWdetach(did);
					SWclose(fid);
					return(storage);
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDFEOS: Error ocurred while reading can't continue");
					return(NULL);
				}
				break;
			case POINT:
				return(NULL);
				break;
			}
			
		}
		thelist= thelist->next;
	}
	return(NULL);
}
static void *HDFEOSReadCoord
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,long* stride,void* storage)
#else
(therec, thevar, start, finish,stride,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
long* stride;
void* storage;
#endif
{
	return(HDFEOSReadVar(therec,thevar,start,finish,stride,storage));
}

static void *HDFEOSReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSAttInqRecList * the_int_att_list;

	the_int_att_list = thefile->att_int_list;
	while(the_int_att_list != NULL) {
		if(theatt == the_int_att_list->att_inq->name) {
			memcpy(storage,the_int_att_list->att_inq->value,_NclSizeOf( the_int_att_list->att_inq->type)* the_int_att_list->att_inq->n_elem);
			return(storage);
		} else {
			the_int_att_list = the_int_att_list->next;
		}
	}
	return(NULL);
}
static void *HDFEOSReadVarAtt
#if	NhlNeedProto
(void * therec, NclQuark thevar, NclQuark theatt, void * storage)
#else
(therec, thevar, theatt, storage)
void * therec;
NclQuark thevar;
NclQuark theatt;
void* storage;
#endif
{
	HDFEOSFileRecord * thefile = (HDFEOSFileRecord *) therec;
	HDFEOSVarInqRecList * thelist;
	HDFEOSAttInqRecList * the_int_att_list;
	int i;

	thelist = thefile->vars;
	for(i = 0; i < thefile->n_vars; i++) {
		if(thevar == thelist->var_inq->name) {
			the_int_att_list = thelist->var_inq->att_int_list;
			while(the_int_att_list != NULL) {
				if(theatt == the_int_att_list->att_inq->name) {
					memcpy(storage,the_int_att_list->att_inq->value,_NclSizeOf( the_int_att_list->att_inq->type)* the_int_att_list->att_inq->n_elem);
					return(storage);
				} else {
					the_int_att_list = the_int_att_list->next;
				}
			}
		} else {
			thelist = thelist->next;
		}
	}
	return(NULL);
}
NclFormatFunctionRec HDFEOSRec = {
/* NclInitializeFileRecFunc initialize_file_rec */      HDFEOSInitializeFileRec,
/* NclCreateFileFunc	   create_file; */		NULL,
/* NclOpenFileFunc         open_file; */		HDFEOSOpenFile,
/* NclFreeFileRecFunc      free_file_rec; */		HDFEOSFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		HDFEOSGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		HDFEOSGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		HDFEOSGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		HDFEOSGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		HDFEOSGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		HDFEOSGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	HDFEOSGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		HDFEOSGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		HDFEOSGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		HDFEOSReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			HDFEOSReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			HDFEOSReadAtt,
/* NclReadVarAttFunc       read_var_att; */		HDFEOSReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */		NULL,
/* NclAddDimFunc           add_dim; */			NULL,
/* NclAddChunkDimFunc      add_chunk_dim; */		NULL,
/* NclRenameDimFunc        rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */			NULL,
/* NclAddVarChunkFunc      add_var_chunk; */		NULL,
/* NclAddVarChunkCacheFunc add_var_chunk_cache; */	NULL,
/* NclSetVarCompressLevelFunc set_var_compress_level; */ NULL,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	NULL,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	NULL,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */	        NULL,
#include "NclGrpFuncs.null"
/* NclSetOptionFunc        set_option;  */		NULL
};
NclFormatFunctionRecPtr HDFEOSAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&HDFEOSRec);
}

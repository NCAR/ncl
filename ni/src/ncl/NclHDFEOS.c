/*
 *      $Id: NclHDFEOS.c,v 1.4 2005-07-23 00:49:57 dbrown Exp $
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
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#define HAVE_NETCDF
#include <hdf/mfhdf.h>
#include <hdf/netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include <math.h>
#include <HdfEosDef.h>

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
	if(type != NCL_char) {
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
	char buffer[1000];
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
		memcpy(buffer,value,n_elem);
		buffer[n_elem] = '\0';
		tmp_quark = (NclQuark*)NclMalloc(sizeof(NclQuark));	
		*tmp_quark = NrmStringToQuark(buffer);
		tmp_node->att_inq->value = (void*)tmp_quark;
		tmp_node->att_inq->n_elem = 1;
		NclFree(value);
	}
	tmp_node->next = the_file->att_int_list;
	the_file->att_int_list = tmp_node;
	the_file->n_int_atts++;
}

static void *HDFEOSInitializeFileRec
#if	NhlNeedProto
(void)
#else
()
#endif
{
	HDFEOSFileRecord *therec = NULL;

	therec = (HDFEOSFileRecord*)NclCalloc(1, sizeof(HDFEOSFileRecord));
	if (! therec) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return NULL;
	}
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
	char swathlist[HDF_BUFFSIZE];
	char pointlist[HDF_BUFFSIZE];
	char gridlist[HDF_BUFFSIZE];
	char fieldlist[HDF_BUFFSIZE];
	char dimnames[HDF_BUFFSIZE];
	char atts[HDF_BUFFSIZE];
	char buffer[HDF_BUFFSIZE];
	int32 natts;
	int32 dims[HDF_BUFFSIZE];
	int32 ndims;
	int32 nentries;
	int32 npt,nsw,ngd,i,j,k;
	int ngroups;
	long bsize;
	char *tmp,*tmp2;
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
	int32 HDFid;
	int32 sdInterfaceID;
	int32 ref_array[1000];
	char vgroup_class[100];
	int32 tmp_id;
	intn oginfo;
	int32 orgincode=0;
	int32 projcode = 0;
	int32 zonecode =0 ;
	int32 spherecode = 0;
	void *tmp_value;
	int32 att_type;
	int32 att_size;
	intn nrc,nfd,nlv;
	int32 strbufsize = 0;
	char version[100];
	int32 *fldorder = NULL;
	int32 *fldtype = NULL;

	
	float64 projparm[100];
	NclScalar missing;
	NclScalar *tmp_missing;
	int *is_unsigned;

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
	if((nsw = SWinqswath(NrmQuarkToString(path),swathlist,&bsize)) > 0) {
		SWfid = SWopen(NrmQuarkToString(path),DFACC_READ);
		HDFEOSParseName(swathlist,&sw_hdf_names,&sw_ncl_names,nsw);
		for(i = 0; i < nsw; i++) {
			SWid = SWattach(SWfid,NrmQuarkToString(sw_hdf_names[i]));
			if(SWid > 0 ) {
				natts = SWinqattrs(SWid,atts,&bsize);
				if(natts > 0 ) {
					HDFEOSParseName(atts,&att_hdf_names,&att_ncl_names,natts);
					for(k = 0; k < natts; k++) { 
						if(SWattrinfo(SWid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0) {
							tmp_value = (void*)NclMalloc(att_size);
							if(SWreadattr(SWid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 ) {
								HDFEOSIntFileAddAtt(the_file,sw_ncl_names[i],att_ncl_names[k],tmp_value,att_size/_NclSizeOf(HDFEOSMapTypeNumber(att_type)),HDFEOSMapTypeNumber(att_type));
							}
			
						}
					}
					NclFree(att_hdf_names);
					NclFree(att_ncl_names);
				}
				ndims = SWinqdims(SWid,dimnames,dims);
				if(ndims != -1) {
					HDFEOSParseName(dimnames,&dim_hdf_names,&dim_ncl_names,ndims);
					for(j = 0; j < ndims; j++) {
						HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],SWdiminfo(SWid,NrmQuarkToString(dim_hdf_names[j])),sw_hdf_names[i],sw_ncl_names[i],the_file->file_path_q);
					}
					ndata = SWinqdatafields(SWid,fieldlist,NULL,NULL);
					if(ndata != -1) {
						the_file->n_vars += ndata;
						HDFEOSParseName(fieldlist,&var_hdf_names,&var_ncl_names,ndata);
						for(j = 0; j < ndata; j++) {
							if(SWfieldinfo(SWid,NrmQuarkToString(var_hdf_names[j]),&tmp_rank,dims,&tmp_type,buffer) == 0) {
								HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
								for(k = 0; k < tmp_rank; k++) {
									HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),tmp_names[k],tmp_ncl_names[k],dims[k],sw_hdf_names[i],sw_ncl_names[i],the_file->file_path_q);
								}
						

								HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],the_file->dims,tmp_rank,dims,tmp_type,tmp_ncl_names,SWATH,sw_hdf_names[i],sw_ncl_names[i]);
								if(SWgetfillvalue(SWid,NrmQuarkToString(var_hdf_names[j]),&missing) != -1) {
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
						}
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",NrmQuarkToString(path));
					return(NULL);
				}
			}
			SWdetach(SWid);	
		}
		SWclose(SWfid);
		NclFree(var_hdf_names);
		NclFree(var_ncl_names);
	}
	if((ngd = GDinqgrid(NrmQuarkToString(path),gridlist,&bsize)) > 0) {
		GDfid = GDopen(NrmQuarkToString(path),DFACC_READ);
		HDFEOSParseName(gridlist,&gd_hdf_names,&gd_ncl_names,ngd);
		for(i = 0; i < ngd; i++) {
			GDid = GDattach(GDfid,NrmQuarkToString(gd_hdf_names[i]));
			if(GDid > 0 ) {
				natts = GDinqattrs(GDid,atts,&bsize);
				if(natts > 0 ) {
					HDFEOSParseName(atts,&att_hdf_names,&att_ncl_names,natts);
					for(k = 0; k < natts; k++) { 
						if(GDattrinfo(GDid,NrmQuarkToString(att_hdf_names[k]),&att_type,&att_size)==0) {
							tmp_value = (void*)NclMalloc(att_size);
							if(GDreadattr(GDid,NrmQuarkToString(att_hdf_names[k]),tmp_value)==0 ) {
								HDFEOSIntFileAddAtt(the_file,gd_ncl_names[i],att_ncl_names[k],tmp_value,att_size/_NclSizeOf(HDFEOSMapTypeNumber(att_type)),HDFEOSMapTypeNumber(att_type));
							}
						}
					}
					NclFree(att_hdf_names);
					NclFree(att_ncl_names);
				}
				ndims = GDinqdims(GDid,dimnames,dims);
				if(ndims != -1) {
					HDFEOSParseName(dimnames,&dim_hdf_names,&dim_ncl_names,ndims);
					for(j = 0; j < ndims; j++) {
						HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],GDdiminfo(GDid,NrmQuarkToString(dim_hdf_names[j])),gd_hdf_names[i],gd_ncl_names[i],the_file->file_path_q);
					}
					ndata = GDinqfields(GDid,fieldlist,NULL,NULL);
					if(ndata != -1) {
						the_file->n_vars += ndata;
						HDFEOSParseName(fieldlist,&var_hdf_names,&var_ncl_names,ndata);
						for(j = 0; j < ndata; j++) {
							if(GDfieldinfo(GDid,NrmQuarkToString(var_hdf_names[j]),&tmp_rank,dims,&tmp_type,buffer) == 0) {
								HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
								for(k = 0; k < tmp_rank; k++) {
									HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),tmp_names[k],tmp_ncl_names[k],dims[k],gd_hdf_names[i],gd_ncl_names[i],the_file->file_path_q);
								}
						

								HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],the_file->dims,tmp_rank,dims,tmp_type,tmp_ncl_names,GRID,gd_hdf_names[i],gd_ncl_names[i]);
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

/*
*
*
*
*
*------------> Still need to insert this ifo
*
*
*
*
*/
							oginfo = GDorigininfo(GDid,&orgincode);
							GDprojinfo(GDid,&projcode,&zonecode,&spherecode,projparm);

/*
*
*
*
*
*
*/
							switch(projcode) {
							case GCTP_GEO:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Geographic");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_UTM:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Universal Transverse Mercator");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_LAMCC:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Lambert Conformal Conic");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_PS:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Polar Stereographic");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_TM:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Transverse Mercator");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_LAMAZ:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Lambert Azimuthal Equal Area");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_POLYC:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Polyconic");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_HOM:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Hotline Oblique Mercator");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_SOM:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Space Oblique Mercator");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_GOOD:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Interrupted Goode Homolosine");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							case GCTP_ISINUS:
								tmp_value = (void*)NclMalloc(sizeof(NclQuark));
								*(NclQuark*)tmp_value = NrmStringToQuark("Intergeried Sinusoidal Projection");
								HDFEOSIntAddAtt(the_file->vars->var_inq,NrmStringToQuark("projection"),(void*)tmp_value,1,NCL_string);
								break;
							}
						}
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",NrmQuarkToString(path));
					return(NULL);
				}
			}
			GDdetach(GDid);	
		}
		GDclose(GDfid);
		NclFree(var_hdf_names);
		NclFree(var_ncl_names);
	}
	if((npt = PTinqpoint(NrmQuarkToString(path),pointlist,&bsize)) > 0)  {
		PTfid = PTopen(NrmQuarkToString(path),DFACC_READ);
		HDFEOSParseName(pointlist,&pt_hdf_names,&pt_ncl_names,npt);
		for(i = 0; i < npt; i++) {
			PTid = PTattach(PTfid,NrmQuarkToString(pt_hdf_names[i]));
			if(PTid > 0 ) {
				nlv = PTnlevels(PTid);
				if(nlv > 0) {
					for(j = 0; j < nlv; j++) {
						
						nrc = PTnrecs(PTid,j);
						nfd = PTnfields(PTid,j,&strbufsize);
						fldtype  = (int32*)NclMalloc(sizeof(int32)*nfd);
						fldorder = (int32*)NclMalloc(sizeof(int32)*nfd);
						nfd = PTlevelinfo(PTid,j,fieldlist,fldtype,fldorder);
						HDFEOSParseName(fieldlist,&var_hdf_names,&var_ncl_names,nfd);
						for(k = 0; k < nfd; k++) {
							fprintf(stdout,"%s\n",NrmQuarkToString(var_hdf_names[k]));
						}
					
					}
				}

/*
				ndims = PTinqdims(PTid,dimnames,dims);
				if(ndims != -1) {
					HDFEOSParseName(dimnames,&dim_hdf_names,&dim_ncl_names,ndims);
					for(j = 0; j < ndims; j++) {
						HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),dim_hdf_names[j],dim_ncl_names[j],PTdiminfo(PTid,NrmQuarkToString(dim_hdf_names[j])),pt_hdf_names[i],pt_ncl_names[i],the_file->file_path_q);
					}
					ndata = PTinqfields(PTid,fieldlist,NULL,NULL);
					if(ndata != -1) {
						the_file->n_vars += ndata;
						HDFEOSParseName(fieldlist,&var_hdf_names,&var_ncl_names,ndata);
						for(j = 0; j < ndata; j++) {
							if(PTfieldinfo(PTid,NrmQuarkToString(var_hdf_names[j]),&tmp_rank,dims,&tmp_type,buffer) == 0) {
								HDFEOSParseName(buffer,&tmp_names,&tmp_ncl_names,tmp_rank);
								for(k = 0; k < tmp_rank; k++) {
									HDFEOSIntAddDim(&(the_file->dims),&(the_file->n_dims),tmp_names[k],tmp_ncl_names[k],dims[k],pt_hdf_names[i],pt_ncl_names[i],the_file->file_path_q);
								}
						

								HDFEOSIntAddVar(&(the_file->vars),var_hdf_names[j],var_ncl_names[j],the_file->dims,tmp_rank,dims,tmp_type,tmp_ncl_names,GRID,pt_hdf_names[i],pt_ncl_names[i]);
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
												HDFEOSIntAddAtt(the_file->vars->var_inq,att_ncl_names[k],tmp_value,att_size/_NclSizeOf(HDFEOSMapTypeNumber(att_type)),HDFEOSMapTypeNumber(att_type));
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
*/
/*
					natts = PTinqattrs(GDid,atts,&bsize);
					if(natts > 0 ) {
						HDFEOSParseName(atts,&att_hdf_names,&att_ncl_names,natts);
						for(j = 0; j < natts; j++) {
							HDFEOSIntAddAtt(the_file->vars->var_inq,att_hdf_names[j],att_ncl_names[j]);
							fprintf(stdout,"%s\n",NrmQuarkToString(att_ncl_names[j]));
						}
					}
*/
/*
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN, "NclHDFEOS: An internal HDF error occurred while reading (%s) can't continue",NrmQuarkToString(path));
					return(NULL);
				}
*/
			}
			PTdetach(PTid);	
		}
		PTclose(PTfid);
		NclFree(var_hdf_names);
		NclFree(var_ncl_names);
	}
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
	int32 is_unsigned;

	int i,j;


	thelist = thefile->vars;
	for (i = 0; i < thefile->n_vars; i++) {
		if(thelist->var_inq->name == var_name) {
			var_info->var_name_quark = var_name;
			var_info->data_type = HDFEOSMapTypeNumber(thelist->var_inq->typenumber);
			var_info->num_dimensions = thelist->var_inq->n_dims;
			for(j = 0; j < thelist->var_inq->n_dims; j++) {
				var_info->file_dim_num[j] = thelist->var_inq->dim[j];
			}
			return(var_info);
		}
		thelist = thelist->next;
	}
	
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
	int i;

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
	int i,j,out;
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
				out = GDreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
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
				out = SWreadfield(did,NrmQuarkToString(thelist->var_inq->hdf_name),starti,stridei,edgei,storage);
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
	int i;

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
	NclFAttRec* output = NULL;
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
/* NclGetVarNamesFu_nc      get_var_names; */		HDFEOSGetVarNames,
/* NclGetVarInfoFusd_nc       get_var_info; */		HDFEOSGetVarInfo,
/* NclGetDimNamesFusd_nc      get_dim_names; */		HDFEOSGetDimNames,
/* NclGetDimInfoFusd_nc       get_dim_info; */		HDFEOSGetDimInfo,
/* NclGetAttNamesFusd_nc      get_att_names; */		HDFEOSGetAttNames,
/* NclGetAttInfoFusd_nc       get_att_info; */		HDFEOSGetAttInfo,
/* NclGetVarAttNamesFusd_nc   get_var_att_names; */	HDFEOSGetVarAttNames,
/* NclGetVarAttInfoFusd_nc    get_var_att_info; */		HDFEOSGetVarAttInfo,
/* NclGetCoordInfoFusd_nc     get_coord_info; */		HDFEOSGetCoordInfo,
/* NclReadCoordFusd_nc        read_coord; */		HDFEOSReadCoord,
/* NclReadCoordFusd_nc        read_coord; */		NULL,
/* NclReadVarFusd_nc          read_var; */			HDFEOSReadVar,
/* NclReadVarFusd_nc          read_var; */			NULL,
/* NclReadAttFusd_nc          read_att; */			HDFEOSReadAtt,
/* NclReadVarAttFusd_nc       read_var_att; */		HDFEOSReadVarAtt,
/* NclWriteCoordFusd_nc       write_coord; */		NULL,
/* NclWriteCoordFusd_nc       write_coord; */		NULL,
/* NclWriteVarFusd_nc         write_var; */		NULL,
/* NclWriteVarFusd_nc         write_var; */		NULL,
/* NclWriteAttFusd_nc         write_att; */		NULL,
/* NclWriteVarAttFusd_nc      write_var_att; */		NULL,
/* NclAddDimFusd_nc           add_dim; */			NULL,
/* NclAddDimFusd_nc           rename_dim; */		NULL,
/* NclAddVarFusd_nc           add_var; */			NULL,
/* NclAddVarFusd_nc           add_coord_var; */		NULL,
/* NclAddAttFusd_nc           add_att; */			NULL,
/* NclAddVarAttFusd_nc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_sd_ncl; */	NULL,
/* NclMapNclTypeToFormat   map_sd_ncl_type_to_format; */	NULL,
/* NclDelAttFusd_nc           del_att; */			NULL,
/* NclDelVarAttFusd_nc        del_var_att; */	        NULL,
/* NclSetOptionFunc           set_option;  */           NULL
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

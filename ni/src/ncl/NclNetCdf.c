/*
 *      $Id: NclNetCdf.c,v 1.5 1994-07-27 20:30:30 ethan Exp $
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
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include <math.h>

typedef struct _NetCdfFileRecord NetCdfFileRecord;
typedef struct _NetCdfVarInqRec NetCdfVarInqRec;
typedef struct _NetCdfDimInqRec NetCdfDimInqRec;
typedef struct _NetCdfAttInqRec NetCdfAttInqRec;
typedef struct _NetCdfVarInqRecList NetCdfVarInqRecList;
typedef struct _NetCdfDimInqRecList NetCdfDimInqRecList;
typedef struct _NetCdfAttInqRecList NetCdfAttInqRecList;

struct _NetCdfVarInqRecList {
	NetCdfVarInqRec *var_inq;
	NetCdfVarInqRecList *next;
};

struct _NetCdfDimInqRecList {
	NetCdfDimInqRec *dim_inq;
	NetCdfDimInqRecList *next;
};

struct _NetCdfAttInqRecList {
	NetCdfAttInqRec *att_inq;
	NetCdfAttInqRecList *next;
};

struct _NetCdfVarInqRec {
	int varid;
	NclQuark name;
	nc_type	data_type;
	int	n_dims;
	int	dim[MAX_VAR_DIMS];
	int	natts;
	NetCdfAttInqRecList *att_list;
};

struct _NetCdfDimInqRec {
	int dimid;
	NclQuark name;
	long size;
};
	
struct _NetCdfAttInqRec {
	int att_num;
	NclQuark name;
	int	varid;
	nc_type data_type;
	int	len;
};


struct _NetCdfFileRecord {
NclQuark	file_path_q;
int		wr_status;
int		n_vars;
NetCdfVarInqRecList *vars;
int		n_dims;
NetCdfDimInqRecList *dims;
int		n_file_atts;
NetCdfAttInqRecList *file_atts;
};


static NclBasicDataTypes NetMapToNcl 
#if __STDC__
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
	switch(*(nc_type*)the_type) {
	case NC_BYTE:
		return(NCL_byte);
	case NC_CHAR:
		return(NCL_char);
	case NC_SHORT:
		return(NCL_short);
	case NC_LONG:
		return(NCL_long);
	case NC_FLOAT:
		return(NCL_float);
	case NC_DOUBLE:
		return(NCL_double);
	default:
		return(NCL_none);
	}
}

static void *NetMapFromNcl
#if __STDC__
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
	void *out_type = (void*)NclMalloc((unsigned)sizeof(nc_type));;

	switch(the_type) {
	case NCL_byte:
		*(nc_type*)out_type = NC_BYTE;
                break;
	case NCL_char:
		*(nc_type*)out_type = NC_CHAR;
                break;
	case NCL_short:
		*(nc_type*)out_type = NC_SHORT;
                break;
	case NCL_long:
		*(nc_type*)out_type = NC_LONG;
		break;
	case NCL_float:
		*(nc_type*)out_type = NC_FLOAT;
		break;
	case NCL_double:
		*(nc_type*)out_type = NC_DOUBLE;
		break;
        default:
		NclFree(out_type);
		out_type = NULL;
	}
	return(out_type);
}








static void *NetGetFileRec
#if  __STDC__
(NclQuark path,int wr_status)
#else
(path,wr_status)
NclQuark path;
int wr_status;
#endif
{
	NetCdfFileRecord *tmp = (NetCdfFileRecord*)
			NclMalloc((unsigned)sizeof(NetCdfFileRecord));
	int cdfid;
	int dummy;
	char buffer[MAX_NC_NAME];
	int i,j;
	NetCdfAttInqRecList **stepalptr;
	NetCdfVarInqRecList **stepvlptr;
	NetCdfDimInqRecList **stepdlptr;
	

	if(tmp == NULL) {
		return(NULL);
	}
	tmp->file_path_q = path;
	tmp->wr_status = wr_status;
	tmp->n_vars = 0;
	tmp->vars= NULL;
	tmp->n_dims = 0;
	tmp->dims = NULL;
	tmp->n_file_atts = 0;
	tmp->file_atts= NULL;

	if(wr_status) {
		cdfid = ncopen(NrmQuarkToString(path),NC_NOWRITE);
	} else {
		cdfid = ncopen(NrmQuarkToString(path),NC_WRITE);
	}

	if(cdfid == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The specified netCDF file (%s) does not exist or can't be opened\n",NrmQuarkToString(path));
		return(NULL);
	}

	ncinquire(cdfid,&(tmp->n_dims),&(tmp->n_vars),&(tmp->n_file_atts),&dummy);
	stepdlptr = &(tmp->dims);
	if(tmp->n_dims != 0) {
		for(i = 0 ; i < tmp->n_dims; i++) {
			*stepdlptr = (NetCdfDimInqRecList*)NclMalloc(
					(unsigned) sizeof(NetCdfDimInqRecList));
			(*stepdlptr)->dim_inq = (NetCdfDimInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfDimInqRec));
			(*stepdlptr)->next = NULL;
			(*stepdlptr)->dim_inq->dimid = i;
			ncdiminq(cdfid,i,buffer,&((*stepdlptr)->dim_inq->size));
			(*stepdlptr)->dim_inq->name = NrmStringToQuark(buffer);
			stepdlptr = &((*stepdlptr)->next);
		}
	} else {
		tmp->dims = NULL;
	}
	stepvlptr = &(tmp->vars);
	if(tmp->n_vars != 0 ) {
		for(i = 0 ; i < tmp->n_vars; i++) {
			*stepvlptr = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned) sizeof(NetCdfVarInqRecList));
			(*stepvlptr)->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec));
			(*stepvlptr)->next = NULL;
			(*stepvlptr)->var_inq->varid = i;
			ncvarinq(cdfid,i,buffer,
				&((*stepvlptr)->var_inq->data_type),
				&((*stepvlptr)->var_inq->n_dims),
				((*stepvlptr)->var_inq->dim),
				&((*stepvlptr)->var_inq->natts)
				);
			(*stepvlptr)->var_inq->name = NrmStringToQuark(buffer);
			stepalptr = &((*stepvlptr)->var_inq->att_list);
			if(((*stepvlptr)->var_inq->natts) != 0) {
				for(j = 0; j < ((*stepvlptr)->var_inq->natts); j++) {
					ncattname(cdfid,i,j,buffer);
					(*stepalptr) = (NetCdfAttInqRecList*)NclMalloc(
						(unsigned)sizeof(NetCdfAttInqRecList));
					(*stepalptr)->att_inq = (NetCdfAttInqRec*)NclMalloc(
						(unsigned)sizeof(NetCdfAttInqRec));
					(*stepalptr)->next = NULL;
					(*stepalptr)->att_inq->att_num = j;
					(*stepalptr)->att_inq->name = NrmStringToQuark(buffer);
					(*stepalptr)->att_inq->varid = i;
					ncattinq(cdfid,i,buffer,
						&((*stepalptr)->att_inq->data_type),
						&((*stepalptr)->att_inq->len));
					stepalptr = &((*stepalptr)->next);
				}
			} else {
				((*stepvlptr)->var_inq->att_list) = NULL;
			}
			stepvlptr = &((*stepvlptr)->next);
		}
	} else {
		tmp->vars = NULL;
	}
	if(tmp->n_file_atts != 0 ) {
		stepalptr = &(tmp->file_atts);
		for(i = 0; i < tmp->n_file_atts; i++) {
			*stepalptr = (NetCdfAttInqRecList*)NclMalloc(
				(unsigned)sizeof(NetCdfAttInqRecList));
			(*stepalptr)->att_inq = (NetCdfAttInqRec*)NclMalloc(
				(unsigned)sizeof(NetCdfAttInqRec));
			(*stepalptr)->next = NULL;
			ncattname(cdfid,NC_GLOBAL,i,buffer);
			(*stepalptr)->att_inq->att_num = i;
			(*stepalptr)->att_inq->name = NrmStringToQuark(buffer);
			(*stepalptr)->att_inq->varid = NC_GLOBAL;
			ncattinq(cdfid,NC_GLOBAL,buffer,
					&((*stepalptr)->att_inq->data_type),
                                	&((*stepalptr)->att_inq->len));
       	        	stepalptr = &((*stepalptr)->next);
		}
	} else {
		tmp->file_atts = NULL;
	}
	ncclose(cdfid);
	return((void*)tmp);
}

static void NetFreeFileRec
#if __STDC__
(void* therec)
#else
(therec)
void *therec;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
        NetCdfVarInqRecList *stepvl;
        NetCdfDimInqRecList *stepdl;

	stepal = rec->file_atts;
	while(rec->file_atts != NULL) {
		stepal = rec->file_atts;
		NclFree(stepal->att_inq);
		rec->file_atts = rec->file_atts->next;
		NclFree(stepal);
	}
	stepdl = rec->dims;
	while(rec->dims != NULL) {
		stepdl = rec->dims;
		NclFree(stepdl->dim_inq);
		rec->dims= rec->dims->next;
		NclFree(stepdl);
	}

	while(rec->vars != NULL) {
		stepvl = rec->vars;
		while(stepvl->var_inq->att_list != NULL) {
			stepal = stepvl->var_inq->att_list;
			NclFree(stepvl->var_inq->att_list->att_inq);
			stepvl->var_inq->att_list = stepal->next;
			NclFree(stepal);
		}
		NclFree(stepvl->var_inq);
		rec->vars = rec->vars->next;
		NclFree(stepvl);
	}
	NclFree(rec);
	return;
}

static NclQuark* NetGetVarNames
#if __STDC__
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NclQuark *out_quarks;
	NetCdfVarInqRecList *stepvl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_vars);
	stepvl = rec->vars;
	for(i = 0; i < rec->n_vars; i++) {
		out_quarks[i] = stepvl->var_inq->name;
		stepvl=stepvl->next;
	}
	*num_vars = rec->n_vars;;
	return(out_quarks);
}

static NclFVarRec *NetGetVarInfo
#if __STDC__
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfDimInqRecList *stepdl;
	NclFVarRec *tmp;
	int i,j;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == var_name) {
			tmp = (NclFVarRec*)NclMalloc((unsigned)sizeof(NclFVarRec));
			tmp->var_name_quark = stepvl->var_inq->name;
			tmp->data_type = NetMapToNcl((void*)&(stepvl->var_inq->data_type));
			tmp->num_dimensions = stepvl->var_inq->n_dims;
			for(j=0; j< stepvl->var_inq->n_dims; j++) {
				stepdl = rec->dims;
				while(stepdl->dim_inq->dimid != stepvl->var_inq->dim[j]) {
					stepdl = stepdl->next;
				}
				tmp->dim_sizes[j] = stepdl->dim_inq->size;
				tmp->file_dim_num[j] = stepdl->dim_inq->dimid;
			}
			return(tmp);
		} else {
			stepvl = stepvl->next;
		}
	}
	return(NULL);
}

static NclQuark *NetGetDimNames
#if __STDC__
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NclQuark *out_quarks;
	NetCdfDimInqRecList *stepdl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_dims);
	stepdl = rec->dims;
	for(i = 0; i < rec->n_dims; i++) {
		out_quarks[i] = stepdl->dim_inq->name;
		stepdl=stepdl->next;
	}
	*num_dims = rec->n_dims;;
	return(out_quarks);
}

static NclFDimRec *NetGetDimInfo
#if  __STDC__
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NclFDimRec *tmp;
	NetCdfDimInqRecList *stepdl;

	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->name == dim_name_q) {
			tmp = (NclFDimRec*)NclMalloc((unsigned)sizeof(NclFDimRec));
			tmp->dim_name_quark = dim_name_q;
			tmp->dim_size = stepdl->dim_inq->size;
			return(tmp);
		} else {
			stepdl = stepdl->next;
		}
	}
	return(NULL);
}
static NclQuark *NetGetAttNames
#if  __STDC__
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NclQuark *out_list = NULL;
	int i;

	out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_file_atts);
	stepal = rec->file_atts;
	for(i = 0; i< rec->n_file_atts; i++) {
		out_list[i] = stepal->att_inq->name;
		stepal = stepal->next;
	}
	*num_atts = rec->n_file_atts;
	return(out_list);
}

static NclFAttRec* NetGetAttInfo
#if  __STDC__
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NclFAttRec *tmp;

	stepal = rec->file_atts;
	while(stepal != NULL) {
		if(stepal->att_inq->name == att_name_q) {
			tmp=(NclFAttRec*)NclMalloc((unsigned)sizeof(NclFAttRec));
			tmp->att_name_quark = att_name_q;
/*
* For convenience I make all character attributes strings
*/
			if(stepal->att_inq->data_type == NC_CHAR) {
				tmp->data_type = NCL_string;
				tmp->num_elements = 1;
			} else {
				tmp->data_type = NetMapToNcl((void*)&(stepal->att_inq->data_type));
				tmp->num_elements = stepal->att_inq->len;
			}
			return(tmp);
		} else {
			stepal = stepal->next;
		}
	}

	return(NULL);
}

static NclQuark *NetGetVarAttNames
#if  __STDC__
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfAttInqRecList *stepal;
	NclQuark *out_list = NULL;	
	int i;
	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name== thevar) {
			stepal = stepvl->var_inq->att_list;
			out_list = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark) * stepvl->var_inq->natts);
			*num_atts = stepvl->var_inq->natts;
			for(i = 0; i< stepvl->var_inq->natts; i++) {
				out_list[i] = stepal->att_inq->name;
				stepal = stepal->next;
			}
			return(out_list);
		} else {
			stepvl = stepvl->next;
		}
	}
		
	return(NULL);
}

static NclFAttRec *NetGetVarAttInfo
#if  __STDC__
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl;
	NetCdfAttInqRecList *stepal;
	NclFAttRec *tmp = NULL;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			stepal = stepvl->var_inq->att_list;
			while(stepal != NULL) {
				if(stepal->att_inq->name == theatt) {
					tmp= (NclFAttRec*)NclMalloc((unsigned)
						sizeof(NclFAttRec));
					tmp->att_name_quark = theatt;
					if(stepal->att_inq->data_type == NC_CHAR) {

						tmp->data_type = NCL_string;
						tmp->num_elements = 1;
					} else {
						tmp->data_type = NetMapToNcl((void*)&stepal->att_inq->data_type);
						tmp->num_elements = stepal->att_inq->len;
					}
					return(tmp);
				} else {
					stepal = stepal->next;
				}
			}
		} else {
			stepvl = stepvl->next;
		}
	}
		
	return(NULL);
}

static NclFVarRec *NetGetCoordInfo
#if  __STDC__
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
	return(NetGetVarInfo(therec,thevar));
}


static void *NetReadVar
#if __STDC__
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
	NetCdfFileRecord *rec = (NetCdfFileRecord*) therec;
	NetCdfVarInqRecList *stepvl;
	void *out_data;
	int n_elem = 1;
	int cdfid = -1;
	int ret = -1,i;
	int no_stride = 1;
	long count[MAX_NC_DIMS];

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			for(i= 0; i< stepvl->var_inq->n_dims; i++) {
				count[i] = (int)floor((float)(finish[i] - start[i])/(float)stride[i]) + 1;
				n_elem *= count[i];
				if(stride[i] != 1) {
					no_stride = 0;
				}
			}
			out_data = storage;
			cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_NOWRITE);
				
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for reading",NrmQuarkToString(rec->file_path_q));
				return(NULL);
			}


			if(no_stride) {	
				ret = ncvargetg(cdfid,
					stepvl->var_inq->varid,
					start,
					count,
					NULL,
					NULL,
					out_data);
			} else {
				ret = ncvargetg(cdfid,
					stepvl->var_inq->varid,
					start,
					count,
					stride,
					NULL,
					out_data);
			}
	
			ncclose(cdfid);
			if(ret == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occured while attempting to read variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
				return(NULL);
			} else {
				return(storage);
			}
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static void *NetReadCoord
#if __STDC__
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
	return(NetReadVar(therec,thevar,start,finish,stride,storage));
}


static void *NetReadAtt
#if  __STDC__
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	int cdfid,ret ;
	char *tmp;

	stepal = rec->file_atts;
	while(stepal != NULL) {
		if(stepal->att_inq->name == theatt) {
			cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_NOWRITE);
			
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for reading",NrmQuarkToString(rec->file_path_q));
				return(NULL);
			}
			if(stepal->att_inq->data_type == NC_CHAR) {
				tmp = (char*)NclMalloc(stepal->att_inq->len+1);
				tmp[stepal->att_inq->len] = '\0';
				ret = ncattget(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),tmp);
				*(string*)storage = NrmStringToQuark(tmp);
				NclFree(tmp);
			} else {
				ret = ncattget(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),storage);
			}
			ncclose(cdfid);
			return(storage);
		} else {
			stepal = stepal->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Attribute (%s) is not a global attribute of file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static void *NetReadVarAtt
#if __STDC__
(void * therec, NclQuark thevar, NclQuark theatt, void * storage)
#else
(therec, thevar, theatt, storage)
void * therec;
NclQuark thevar;
NclQuark theatt;
void* storage;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NetCdfVarInqRecList *stepvl;
	int cdfid;
	int ret;
	char *tmp;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->name == thevar) {
			stepal = stepvl->var_inq->att_list;
			while(stepal != NULL) {
				if(stepal->att_inq->name == theatt) {
					cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_NOWRITE);
			
					if(cdfid == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for reading",NrmQuarkToString(rec->file_path_q));
						return(NULL);
					}
					if(stepal->att_inq->data_type == NC_CHAR) {
	
						tmp = (char*)NclMalloc(stepal->att_inq->len + 1);
						tmp[stepal->att_inq->len] = '\0';
						ret = ncattget(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),tmp);
						*(string*)storage = NrmStringToQuark(tmp);
						NclFree(tmp);
					
						
						
					} else {
						ret = ncattget(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),storage);
					}
					ncclose(cdfid);
					if(ret != -1)
						return(storage);
				} else {
					stepal = stepal->next;
				}
			}
			break;
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Attribute (%s) is not a variable attribute of (%s->%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q),NrmQuarkToString(thevar));
	return(NULL);
}
static NhlErrorTypes NetWriteVar
#if  __STDC__
(void * therec, NclQuark thevar, void *data, long* start, long *finish,long *stride)
#else
(therec, thevar, data, start, finish,stride)
void * therec;
NclQuark thevar;
void *data;
long *start;
long *finish;
long *stride;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	int cdfid;
	NetCdfVarInqRecList *stepvl; 
	long count[MAX_NC_DIMS];
	int i,n_elem = 1,no_stride = 1;
	int ret;

	if(!rec->wr_status) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				for(i= 0; i< stepvl->var_inq->n_dims; i++) {
					count[i] = (int)floor((float)(finish[i] - start[i])/(float)stride[i]) + 1;
					n_elem *= count[i];
					if(stride[i] != 1) {
						no_stride = 0;
					}
				}
				cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
				
				if(cdfid == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}

	
				if(no_stride) {
					ret = ncvarputg(cdfid,
						stepvl->var_inq->varid,
						start,
						count,
						NULL,
						NULL,
						data);
				} else {
					ret = ncvarputg(cdfid,
						stepvl->var_inq->varid,
						start,
						count,
						stride,
						NULL,
						data);
				}
	
				ncclose(cdfid);
				if(ret == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occured while attempting to write variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				} else {
					return(NhlNOERROR);
				}
			} else {
				stepvl = stepvl->next;
			}
		}
		
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);

	
}
static NhlErrorTypes NetWriteCoord
#if __STDC__
(void *therec, NclQuark thevar, void* data, long* start, long* finish,long* stride)
#else
(therec, thevar, data, start, finish,stride)
void *therec;
NclQuark thevar;
void* data;
long* start;
long* finish;
long* stride;
#endif
{
	return(NetWriteVar(therec,thevar,data,start,finish,stride));
}


static NhlErrorTypes NetWriteAtt
#if  __STDC__
(void *therec, NclQuark theatt, void *data )
#else
(therec, theatt, data )
void *therec;
NclQuark theatt;
void *data;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	int cdfid;
	int ret = -1;

	if(!rec->wr_status) {
		stepal = rec->file_atts;
		while(stepal != NULL) {
			if(stepal->att_inq->name == theatt) {
				cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
				if(cdfid == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
	
				ret = ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),stepal->att_inq->data_type,stepal->att_inq->len,data);
	
				ncclose(cdfid);
				if(ret == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occured while attempting to write the attribute (%s) to file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(rec->file_path_q));
					return(NhlFATAL);
				}
				return(NhlNOERROR);
			} else {
				stepal = stepal->next;
			}
		}	
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetWriteVarAtt 
#if __STDC__
(void *therec, NclQuark theatt, NclQuark thevar, void* data)
#else
(therec, theatt, thevar, data )
void *therec;
NclQuark theatt;
NclQuark thevar;
void* data;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList *stepal;
	NetCdfVarInqRecList *stepvl;
	int cdfid;
	int ret;
	
	

	if(!rec->wr_status) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->name == thevar) {
				stepal = stepvl->var_inq->att_list;
				while(stepal != NULL) {
					if(stepal->att_inq->name == theatt) {
						cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
						if(cdfid == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
							return(NhlFATAL);
						}
		
						ret = ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),stepal->att_inq->data_type,stepal->att_inq->len,data);
		
						ncclose(cdfid);
						if(ret == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occured while attempting to write the attribute (%s) to variable (%s) in file (%s)",NrmQuarkToString(theatt),NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
							return(NhlFATAL);
						}
						return(NhlNOERROR);
					} else {	
						stepal = stepal->next;
					}
				}	
			} else {
				stepvl = stepvl->next;
			}
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddDim
#if  __STDC__
(void* therec, NclQuark thedim, int size)
#else
(therec, thedim, size)
void* therec;
NclQuark thedim;
int size;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*) therec;
	int cdfid;
	NetCdfDimInqRecList *stepdl;
	int ret = -1;

	if(!rec->wr_status) {
		
		cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
		if(cdfid == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
			return(NhlFATAL);
		}
		ncredef(cdfid);
		ret = ncdimdef(cdfid,NrmQuarkToString(thedim),(long)size);
		ncendef(cdfid);
		ncclose(cdfid);
		if(ret == -1) {
			return(NhlFATAL);
		}
		stepdl = rec->dims;
		if(stepdl == NULL) {
			rec->dims = (NetCdfDimInqRecList*)NclMalloc((unsigned)sizeof(NetCdfDimInqRecList));
			rec->dims->dim_inq = (NetCdfDimInqRec*)NclMalloc((unsigned)sizeof(NetCdfDimInqRec));
			rec->dims->dim_inq->dimid = ret;
			rec->dims->dim_inq->name = thedim;
			rec->dims->dim_inq->size = (long)size;
			rec->dims->next = NULL;
			rec->n_dims = 1;
			return(NhlNOERROR);
		} else {
			while(stepdl->next != NULL) {
				stepdl = stepdl->next;
			}
			stepdl->next = (NetCdfDimInqRecList*)NclMalloc((unsigned)sizeof(NetCdfDimInqRecList));
			stepdl->next->dim_inq = (NetCdfDimInqRec*)NclMalloc((unsigned)sizeof(NetCdfDimInqRec));
			stepdl->next->dim_inq->dimid = ret;
			stepdl->next->dim_inq->name = thedim;
			stepdl->next->dim_inq->size = (long)size;
			stepdl->next->next = NULL;
			rec->n_dims++;
			return(NhlNOERROR);
		}
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddVar
#if __STDC__
(void* therec, NclQuark thevar, NclBasicDataTypes data_type, int n_dims,NclQuark *dim_names, long* dim_sizes)
#else
(therec,thevar,data_type,n_dims,dim_names,dim_sizes)
void* therec;
NclQuark thevar;
NclBasicDataTypes data_type;
int n_dims;
NclQuark *dim_names;
long* dim_sizes;
#endif
{
	NetCdfFileRecord* rec = (NetCdfFileRecord*)therec;
	NetCdfVarInqRecList *stepvl = NULL;
	int cdfid,i,ret;
	nc_type *the_data_type;
	int dim_ids[MAX_NC_DIMS];
	NetCdfDimInqRecList* stepdl = NULL;

	if(!rec->wr_status) {
		cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
		if(cdfid == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
			return(NhlFATAL);
		}
		the_data_type = NetMapFromNcl(data_type);
/*
* All dimensions are correct dimensions for the file
*/
		for(i = 0; i < n_dims; i++) {
			stepdl = rec->dims;
			while(stepdl != NULL) {
				if(stepdl->dim_inq->name == dim_names[i]){
					dim_ids[i] = stepdl->dim_inq->dimid;
					break;
				} else {
					stepdl = stepdl->next;
				}
			}
		} 
		if(the_data_type != NULL) {
			ncredef(cdfid);
			ret = ncvardef(cdfid,NrmQuarkToString(thevar),*the_data_type, n_dims, dim_ids);
			ncendef(cdfid);
			ncclose(cdfid);
			if(ret == -1) {
				return(NhlFATAL);
			} 
	
			stepvl = rec->vars;
			if(stepvl == NULL) {
				rec->vars = (NetCdfVarInqRecList*)NclMalloc(
                                        (unsigned)sizeof(NetCdfVarInqRecList));
				rec->vars->next = NULL;
				rec->vars->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec));
				rec->vars->var_inq->varid = ret;
				rec->vars->var_inq->name = thevar;
				rec->vars->var_inq->data_type = *the_data_type;
				rec->vars->var_inq->n_dims = n_dims;
				rec->vars->var_inq->natts = 0;
				rec->vars->var_inq->att_list = NULL;
				for(i = 0 ; i< n_dims; i++) {
					rec->vars->var_inq->dim[i] = dim_ids[i];
				}
				rec->n_vars = 1;
			} else {
				while(stepvl->next != NULL) {
					stepvl= stepvl->next;
				}
				stepvl->next = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRecList));
				stepvl->next->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec));
				stepvl->next->next = NULL;
				stepvl->next->var_inq->varid = ret;
				stepvl->next->var_inq->name = thevar;
				stepvl->next->var_inq->data_type = *the_data_type;
				stepvl->next->var_inq->n_dims = n_dims;
				stepvl->next->var_inq->natts = 0;
				stepvl->next->var_inq->att_list = NULL;
				for(i = 0 ; i< n_dims; i++) {
					stepvl->next->var_inq->dim[i] = dim_ids[i];
				}
				rec->n_vars++;
			}
			return(NhlNOERROR);
		} else {
			ncclose(cdfid);
		}
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddCoordVar
#if __STDC__
(void *therec, NclQuark thevar,NclBasicDataTypes data_type)
#else
(therec,thevar,data_type)
void *therec;
NclQuark thevar;
NclBasicDataTypes data_type;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfDimInqRecList *stepdl = NULL;
	NetCdfVarInqRecList *stepvl = NULL;
	int cdfid;
	int ret,size;
	nc_type *the_data_type;
	

	if(!rec->wr_status) {
		cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
		if(cdfid == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
			return(NhlFATAL);
		}
		the_data_type = NetMapFromNcl(data_type);
		if(the_data_type != NULL) {
			stepdl = rec->dims;
			while(stepdl != NULL ) {
				if(stepdl->dim_inq->name == thevar){
					ncredef(cdfid);
					size = stepdl->dim_inq->size;
					ret = ncvardef(cdfid,NrmQuarkToString(thevar),*the_data_type,1,&size);
					if(ret == -1) {
						ncabort(cdfid);
						ncclose(cdfid);
						return(NhlFATAL);
					} 
				}
			} 
			stepvl = rec->vars;
			if(stepvl == NULL) {
				rec->vars = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRecList));
				rec->vars->next = NULL;
				rec->vars->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec*));
				rec->vars->var_inq->varid = ret;
				rec->vars->var_inq->name = thevar;
				rec->vars->var_inq->data_type = *the_data_type;
				rec->vars->var_inq->n_dims = 1;
				rec->vars->var_inq->dim[0] = stepdl->dim_inq->dimid;
				rec->vars->var_inq->natts = 0;
				rec->vars->var_inq->att_list = NULL;
				rec->n_vars++;
			} else {
				while(stepvl->next != NULL) {
					stepvl = stepvl->next;
				}
				stepvl->next = (NetCdfVarInqRecList*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRecList));
				stepvl->next->next = NULL;
				stepvl->next->var_inq = (NetCdfVarInqRec*)NclMalloc(
					(unsigned)sizeof(NetCdfVarInqRec*));
				stepvl->next->var_inq->varid = ret;
				stepvl->next->var_inq->name = thevar;
				stepvl->next->var_inq->data_type = *the_data_type;
				stepvl->next->var_inq->n_dims = 1;
				stepvl->next->var_inq->dim[0] = stepdl->dim_inq->dimid;
				stepvl->next->var_inq->natts = 0;
				stepvl->next->var_inq->att_list = NULL;
				rec->n_vars++;
			}
		} else {
			ncclose(cdfid);
		}
	} else {	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetRenameDim
#if __STDC__
(void* therec, NclQuark from, NclQuark to)
#else
(therec, from, to)
void* therec;
NclQuark from;
NclQuark to;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfDimInqRecList *stepdl;
	int cdfid,ret;

	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->name == from) {
			cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			ncredef(cdfid);
			ret = ncdimrename(cdfid,stepdl->dim_inq->dimid,NrmQuarkToString(to));
			ncclose(cdfid);
			if(ret == -1) {
				return(NhlFATAL);
			} else {
				stepdl->dim_inq->name = to;
				return(NhlNOERROR);
			}
		} else {
			stepdl = stepdl->next;
		}
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddAtt
#if __STDC__
(void *therec,NclQuark theatt, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,theatt,data_type,n_items,values)
	void *therec;
	NclQuark theatt;
	NclBasicDataTypes data_type;
	int n_items;
	void * values;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList* stepal;
	nc_type *the_data_type;
	int i,ret;
	int cdfid;
	

	if(!rec->wr_status) {
		the_data_type = (nc_type*)NetMapFromNcl(data_type);
		if(the_data_type != NULL) {
			cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			ncredef(cdfid);
			ret = ncattput(cdfid,NC_GLOBAL,NrmQuarkToString(theatt),*the_data_type,n_items,values);
			ncendef(cdfid);
			ncclose(cdfid);
			if(ret != -1 ) {
				stepal = rec->file_atts;
				if(stepal == NULL) {
					rec->file_atts = (NetCdfAttInqRecList*)NclMalloc((unsigned)
						sizeof(NetCdfAttInqRecList));
					rec->file_atts->att_inq= (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					rec->file_atts->next = NULL;
					rec->file_atts->att_inq->att_num = 1;
					rec->file_atts->att_inq->name = theatt;
					rec->file_atts->att_inq->data_type = *the_data_type;
					rec->file_atts->att_inq->len = n_items;
				} else {	
					i = 0;
					while(stepal->next != NULL) {
						stepal = stepal->next; 
						i++;
					}
					stepal->next = (NetCdfAttInqRecList*)NclMalloc((unsigned)sizeof(NetCdfAttInqRecList));
					stepal->next->att_inq = (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					stepal->next->att_inq->att_num = i;
					stepal->next->att_inq->name = theatt;
					stepal->next->att_inq->data_type = *the_data_type;
					stepal->next->att_inq->len = n_items;
					stepal->next->next = NULL;
				}
				rec->n_file_atts++;
				return(NhlNOERROR);
			} 
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}

static NhlErrorTypes NetAddVarAtt
#if __STDC__
(void *therec,NclQuark thevar, NclQuark theatt, NclBasicDataTypes data_type, int n_items, void * values)
#else
(therec,thevar,theatt,data_type,n_items,values)
	void *therec;
	NclQuark thevar;
	NclQuark theatt;
	NclBasicDataTypes data_type;
	int n_items;
	void * values;
#endif
{
	NetCdfFileRecord *rec = (NetCdfFileRecord*)therec;
	NetCdfAttInqRecList* stepal;
	NetCdfVarInqRecList* stepvl;
	nc_type *the_data_type;
	int i;
	int cdfid,ret;
	
	if(!rec->wr_status ) {
		the_data_type = (nc_type*)NetMapFromNcl(data_type);
		if(the_data_type != NULL) {
			cdfid = ncopen(NrmQuarkToString(rec->file_path_q),NC_WRITE);
			if(cdfid == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: Could not reopen the file (%s) for writing",NrmQuarkToString(rec->file_path_q));
				return(NhlFATAL);
			}
			stepvl = rec->vars;	
			while(stepvl != NULL) {
				if(stepvl->var_inq->name == thevar) {
					break;
				} else {
					stepvl = stepvl->next;
				}
			}
			ncredef(cdfid);
			ret = ncattput(cdfid,stepvl->var_inq->varid,NrmQuarkToString(theatt),*the_data_type,n_items,values);
			ncendef(cdfid);
			ncclose(cdfid);
			if(ret != -1 ) {
				stepal = stepvl->var_inq->att_list;
				if(stepal == NULL) {
					stepvl->var_inq->att_list= (NetCdfAttInqRecList*)NclMalloc((unsigned)
						sizeof(NetCdfAttInqRecList));
					stepvl->var_inq->att_list->att_inq = (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					stepvl->var_inq->att_list->next = NULL;
					stepvl->var_inq->att_list->att_inq->att_num = 0;
					stepvl->var_inq->att_list->att_inq->name = theatt;
					stepvl->var_inq->att_list->att_inq->data_type = *the_data_type;
					stepvl->var_inq->att_list->att_inq->len = n_items;
					stepvl->var_inq->natts = 1;
				} else {	
					i = 0;
					while(stepal->next != NULL) {
						stepal = stepal->next; 
						i++;
					}
					stepal->next = (NetCdfAttInqRecList*)NclMalloc((unsigned)sizeof(NetCdfAttInqRecList));
					stepal->next->att_inq = (NetCdfAttInqRec*)NclMalloc((unsigned)sizeof(NetCdfAttInqRec));
					stepal->next->att_inq->att_num = i;
					stepal->next->att_inq->name = theatt;
					stepal->next->att_inq->data_type = *the_data_type;
					stepal->next->att_inq->len = n_items;
					stepal->next->next = NULL;
					stepvl->var_inq->natts++ ;
				}
				return(NhlNOERROR);
			} 
		} 
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
	}
	return(NhlFATAL);
}


NclFormatFunctionRec NetCdfRec = {
/* NclGetFileRecFunc       get_file_rec; */		NetGetFileRec,
/* NclFreeFileRecFunc      free_file_rec; */		NetFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		NetGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		NetGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		NetGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		NetGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		NetGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		NetGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	NetGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		NetGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		NetGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		NetReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			NetReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			NetReadAtt,
/* NclReadVarAttFunc       read_var_att; */		NetReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NetWriteCoord,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NetWriteVar,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NetWriteAtt,
/* NclWriteVarAttFunc      write_var_att; */		NetWriteVarAtt,
/* NclAddDimFunc           add_dim; */			NetAddDim,
/* NclAddDimFunc           rename_dim; */		NetRenameDim,
/* NclAddVarFunc           add_var; */			NetAddVar,
/* NclAddVarFunc           add_coord_var; */		NetAddCoordVar,
/* NclAddAttFunc           add_att; */			NetAddAtt,
/* NclAddVarAttFunc        add_var_att; */		NetAddVarAtt,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	NetMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	NetMapFromNcl
};
NclFormatFunctionRecPtr NetCdfAddFileFormat 
#if  __STDC__
(void)
#else 
()
#endif
{
	
	return(&NetCdfRec);
}

/*
 *      $Id: NclHDF.c,v 1.3 1996-07-16 20:58:31 ethan Exp $
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
#include "NclDataDefs.h"
#include <netcdf.h>
#include <hdf/hdf.h>
#include "NclFileInterfaces.h"
#include <math.h>
#include <sys/types.h>
#include <sys/stat.h>

extern int _NclSizeOf(
#if     NhlNeedProto
NclBasicDataTypes /*data_type*/
#endif
);

typedef struct _HDFFileRecord HDFFileRecord;
typedef struct _HDFVarInqRec HDFVarInqRec;
typedef struct _HDFDimInqRec HDFDimInqRec;
typedef struct _HDFAttInqRec HDFAttInqRec;
typedef struct _HDFVarInqRecList HDFVarInqRecList;
typedef struct _HDFDimInqRecList HDFDimInqRecList;
typedef struct _HDFAttInqRecList HDFAttInqRecList;


struct _HDFVarInqRecList {
	HDFVarInqRec	*var_inq;
	HDFVarInqRecList *next;
};

struct _HDFDimInqRecList {
	HDFDimInqRec	*dim_inq;
	HDFDimInqRecList *next;
};

struct _HDFAttInqRecList {
	HDFAttInqRec	*att_inq;
	HDFAttInqRecList *next;
};


struct _HDFVarInqRec {
	int	refnum;
	NclQuark name;
	NclQuark ncl_valid_name;
	int	n_dims;
	int	dim_nums[NCL_MAX_DIMENSIONS];
	int32	dim_sizes[NCL_MAX_DIMENSIONS];
	int32	number_type;
	int	n_atts;
	HDFAttInqRecList *atts;
};

struct _HDFFileRecord {
	NclQuark	file_path_q;
	int		wr_status;
	int		n_vars;
	HDFVarInqRecList	*vars;
	int		n_dims;
	HDFDimInqRecList	*dims;
	int		n_file_atts;
	HDFAttInqRecList	*file_atts;
};

struct _HDFAttInqRec {
	NclQuark att_name;
	NclQuark att_val;
};

struct _HDFDimInqRec {
	int	dim_num;
	NclQuark 	name;
	NclQuark 	ncl_valid_name;
	int	is_duplicate;
	long size;
	void *scale;
	int scale_number_type;
	int elemsize;
};


static int IsRef
#if NhlNeedProto
(int refnum,uint16 *rl,int n)
#else
(refnum,rl,n)
int refnum;
int *rl;
int n;
#endif
{
	int i;
	for(i = 0; i < n; i++) {
		if(rl[i] == refnum) 
			return(1);
	}
	return(0);
}

static NclBasicDataTypes HDFMapToNcl 
#if	NhlNeedProto
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
	int thetype = *(int*)the_type;
/*
* Maps HDF types to ncl types
*/
	switch(thetype) {
	case DFNT_INT16:
		return(NCL_short);
	case DFNT_INT32:
		return(NCL_int);
	case DFNT_FLOAT32:
		return(NCL_float);
	case DFNT_FLOAT64:
		return(NCL_double);
	case DFNT_INT8:
		return(NCL_byte);
	}
	return(NCL_none);
		
	
}

static void *HDFMapFromNcl
#if	NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
/*
* Map NCL types to HDF types
*/
	int *out_type = NclMalloc((unsigned)sizeof(int));
	switch(the_type) {
	case NCL_short:
		*out_type = DFNT_INT16;
		break;
	case NCL_int:
		*out_type = DFNT_INT32;
		break;
	case NCL_long:
		*out_type = DFNT_INT32;
		break;
	case NCL_float:
		*out_type = DFNT_FLOAT32;
		break;
	case NCL_double:
		*out_type = DFNT_FLOAT64;
		break;
	case NCL_byte:
		*out_type = DFNT_INT8;
		break;
	default:
		NclFree(out_type);
		out_type = NULL;
	}
	return((void*)out_type);
}

static int HDFAddDumyDimension
#if	NhlNeedProto
(HDFFileRecord *rec,int dim_num,int size)
#else
(rec,dim_num, size,refnum,spath)
HDFFileRecord* rec;
int dim_num;
int size;
#endif
{
	char *tmp = "nodimname";
	char *ncl_valid_str = NULL;
	HDFDimInqRecList *stepdl;
	HDFDimInqRecList *tmpl;
	NclQuark labelq;


	if( dim_num < 10) {
		ncl_valid_str = NclMalloc(strlen(tmp)+2);
	} if(dim_num < 100) {
		ncl_valid_str = NclMalloc(strlen(tmp)+3);
	} else {
		ncl_valid_str = NclMalloc(strlen(tmp)+4);
	}
	strcpy(ncl_valid_str,tmp);
	sprintf(&(ncl_valid_str[strlen(ncl_valid_str)]),"%d",rec->n_dims);
	labelq = NrmStringToQuark(ncl_valid_str);
	tmpl =  (HDFDimInqRecList*)NclMalloc(sizeof(HDFDimInqRecList));
	tmpl->next = NULL;
	tmpl->dim_inq =  (HDFDimInqRec*)NclMalloc(sizeof(HDFDimInqRec));
	tmpl->dim_inq->dim_num = rec->n_dims;
	tmpl->dim_inq->size = (long)size;
	tmpl->dim_inq->name = 0; 
	tmpl->dim_inq->ncl_valid_name = labelq; 
	stepdl = rec->dims;
	if(stepdl == NULL) {
		rec->dims = tmpl;
		rec->n_dims = 1;
	} else {
		while(stepdl->next != NULL){
			stepdl = stepdl->next;
		}
		stepdl->next = tmpl;
		rec->n_dims++;
	}
	return(rec->n_dims-1);
}
static int HDFAddDimension
#if	NhlNeedProto
(HDFFileRecord *rec,int dim_num,int size,int elemsize,intn number_type)
#else
(rec,dim_num, size,refnum,spath,elemsize,number_type)
HDFFileRecord* rec;
int dim_num;
int size;
int elemsize;
intn number_type;
#endif
{
	HDFDimInqRecList *tmpl;
	HDFDimInqRecList *stepd;
	HDFDimInqRecList *last;
	int i,id;
	NclQuark labelq;
	NclQuark valid_labelq;
	char *label;
	int lbl_size;
	NclQuark unitsq;
	char *units;
	int unt_size;
	NclQuark formatq;
	char *format;
	int frmt_size;
	int natts;
	int count;
	char *step,*tmp;
	int is_duplicate = 0;
	int len,ret;
	void *scale = NULL;

	

	DFSDgetdimlen(dim_num + 1,&lbl_size,&unt_size,&frmt_size);
	label = (char*)NclMalloc(lbl_size+1);
	units = (char*)NclMalloc(unt_size+1);
	format = (char*)NclMalloc(frmt_size+1);
	DFSDgetdimstrs(dim_num + 1,label,units,format);
	labelq  = NrmStringToQuark(label);
	formatq = NrmStringToQuark(format);
	unitsq = NrmStringToQuark(units);
	step = label;
	while(*step != '\0') {
		if(!((isalnum(*step))||(*step == '_'))) {
			*step = '_';
                }
                step++;
	}

	stepd = rec->dims;
	last = NULL;
	i = 0;
	while(stepd != NULL) {
		if(stepd->dim_inq->name == labelq) {
			if((stepd->dim_inq->size == size)&&(stepd->dim_inq->scale_number_type == number_type)) {
				break;
			} else {
				is_duplicate = 1;
				last = stepd;
				stepd = stepd->next;
				i++;
			}
		} else {
			last = stepd;
			stepd = stepd->next;
			i++;
		}
	}
	if(i < rec->n_dims) {
		NclFree(label);
		NclFree(units);
		NclFree(format);
		return(i);
	} else {
		if(is_duplicate) {
			step = NclMalloc(strlen(label) + 3);
			strcpy(step,label);
			sprintf(&(step[strlen(label)]),"%d",i);
			valid_labelq = NrmStringToQuark(step);
			NclFree(step);
		} else {
			valid_labelq = NrmStringToQuark(label);
		}
		tmpl =  (HDFDimInqRecList*)NclMalloc(sizeof(HDFDimInqRecList));
		tmpl->next = NULL;
		tmpl->dim_inq =  (HDFDimInqRec*)NclMalloc(sizeof(HDFDimInqRec));
		tmpl->dim_inq->dim_num = rec->n_dims;
		tmpl->dim_inq->size = (long)size;
		tmpl->dim_inq->name = labelq; 
		tmpl->dim_inq->ncl_valid_name = valid_labelq; 
		scale = NclMalloc(elemsize * size);
		ret = DFSDgetdimscale(dim_num,size,scale);
		if(ret != -1) {
			tmpl->dim_inq->scale = scale;
			tmpl->dim_inq->scale_number_type = number_type;
			tmpl->dim_inq->elemsize = elemsize;
		} else {
			NclFree(scale);
			tmpl->dim_inq->scale = NULL;
			tmpl->dim_inq->scale_number_type = number_type;
			tmpl->dim_inq->elemsize = elemsize;
		}
		if(rec->n_dims ==0) {
			rec->dims = tmpl;
		} else {
			last->next = tmpl;
		}
		rec->n_dims++;
		NclFree(label);
		NclFree(units);
		NclFree(format);
		return(rec->n_dims -1);
	}
}






static void *HDFGetFileRec
#if	NhlNeedProto
(NclQuark path,int wr_status)
#else
(path,wr_status)
NclQuark path;
int wr_status;
#endif
{
	HDFFileRecord *tmp = (HDFFileRecord*)NclMalloc(sizeof(HDFFileRecord));
	int sd_id;
	int status;
	int i,j;
	int sds_id;	
	int tmpdimid;
	HDFVarInqRecList** stepvlptr;
	HDFAttInqRecList** stepalptr;
	char *label;
	int lbl_size;
	char *unit;
	int unt_size;
	char *format;
	int frmt_size;
	char *coordsys;
	int crds_size;
	char *spath = (char*)NrmQuarkToString(path);
	char label_list[NCL_MAX_FVARS * NCL_MAX_STRING];
 	uint16 ref_list[NCL_MAX_FVARS];
	int nsets;
	char *step;

	intn nd,ndl,ln,nu,nf,nc;
	uint16 drfl[NCL_MAX_FVARS];
	uint16 urfl[NCL_MAX_FVARS];
	uint16 crfl[NCL_MAX_FVARS];
	uint16 frfl[NCL_MAX_FVARS];
	uint16 dlrfl[NCL_MAX_FVARS];
	char dll[NCL_MAX_FVARS*NCL_MAX_STRING];
	

	tmp->wr_status = wr_status;
	tmp->n_dims = 0;
	tmp->dims = NULL;
	tmp->n_vars = DFANlablist(spath,DFTAG_SD,ref_list,(char*)label_list,NCL_MAX_FVARS,NCL_MAX_STRING,1);
	if(tmp->n_vars == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The specified HDF file (%s) does not exist, can't be opened or does not contain any scientific data sets\n",spath);
		return(NULL);
	}
	nd = DFANlablist(spath,DFTAG_SDD,drfl,dll,NCL_MAX_FVARS,NCL_MAX_STRING,0);

	ndl = DFANlablist(spath,DFTAG_SDL,dlrfl,dll,NCL_MAX_FVARS,NCL_MAX_STRING,0);
	nu = DFANlablist(spath,DFTAG_SDU,urfl,dll,NCL_MAX_FVARS,NCL_MAX_STRING,0);
	nf = DFANlablist(spath,DFTAG_SDF,frfl,dll,NCL_MAX_FVARS,NCL_MAX_STRING,0);
	nc = DFANlablist(spath,DFTAG_SDC,crfl,dll,NCL_MAX_FVARS,NCL_MAX_STRING,0);

	


	stepvlptr = &(tmp->vars);
	for(i = 0; i < tmp->n_vars; i++) {
		*stepvlptr = (HDFVarInqRecList*)NclMalloc((unsigned)sizeof(HDFVarInqRecList));
		(*stepvlptr)->var_inq = (HDFVarInqRec*)NclMalloc((unsigned)sizeof(HDFVarInqRec));
		(*stepvlptr)->next  = NULL;
		(*stepvlptr)->var_inq->refnum= ref_list[i];
		(*stepvlptr)->var_inq->n_atts = 0;
		(*stepvlptr)->var_inq->atts = NULL;
		if(DFSDreadref(spath,ref_list[i])==-1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"The specified HDF file (%s) does not exist or can't be opened\n",NrmQuarkToString(path));
			return(NULL);
		};
		DFSDgetdims(spath,&(*stepvlptr)->var_inq->n_dims,(*stepvlptr)->var_inq->dim_sizes,NCL_MAX_DIMENSIONS);
		DFSDgetNT(&(*stepvlptr)->var_inq->number_type);



		DFSDgetdatalen(&lbl_size,&unt_size,&frmt_size,&crds_size);
		label = (char*)NclMalloc(lbl_size + 1);
		unit = (char*)NclMalloc(unt_size + 1);
		format = (char*)NclMalloc(frmt_size + 1);
		coordsys = (char*)NclMalloc(crds_size + 1);
		DFSDgetdatastrs(label,unit,format,coordsys);
		if(lbl_size > 0) {
			(*stepvlptr)->var_inq->name = NrmStringToQuark(label);
			step = label; 
			while(*step != '\0') {
				if(!((isalnum(*step))||(*step == '_'))) {
					*step = '_';
				} 
				step++;
			}
			(*stepvlptr)->var_inq->ncl_valid_name = NrmStringToQuark(label);
			NclFree(label);
		} else {
			NclFree(label);
			label = (char*)NclMalloc(strlen("novarname") +4);
			strcpy(label,"novarname");
			sprintf(&(label[strlen(label)]),"%d",i);

			(*stepvlptr)->var_inq->name = 0;
			(*stepvlptr)->var_inq->ncl_valid_name = NrmStringToQuark(label);
			NclFree(label);
		}
		stepalptr = &((*stepvlptr)->var_inq->atts);
		if(unt_size > 0) {
			(*stepvlptr)->var_inq->n_atts++;
			*stepalptr = NclMalloc(sizeof(HDFAttInqRecList));
			(*stepalptr)->next = NULL;
			(*stepalptr)->att_inq = NclMalloc(sizeof(HDFAttInqRec));
			(*stepalptr)->att_inq->att_name = NrmStringToQuark("units");
			(*stepalptr)->att_inq->att_val = NrmStringToQuark(unit);
			NclFree(unit);
			stepalptr = &((*stepalptr)->next);
		} else {
			NclFree(unit);
		}
		if(frmt_size > 0) {
			(*stepvlptr)->var_inq->n_atts++;
			*stepalptr = NclMalloc(sizeof(HDFAttInqRecList));
			(*stepalptr)->next = NULL;
			(*stepalptr)->att_inq = NclMalloc(sizeof(HDFAttInqRec));
			(*stepalptr)->att_inq->att_name = NrmStringToQuark("format");
			(*stepalptr)->att_inq->att_val = NrmStringToQuark(format);
			NclFree(format);
			stepalptr = &((*stepalptr)->next);
		} else {
			NclFree(format);
		}
		if(crds_size > 0) {
			(*stepvlptr)->var_inq->n_atts++;
			*stepalptr = NclMalloc(sizeof(HDFAttInqRecList));
			(*stepalptr)->next = NULL;
			(*stepalptr)->att_inq = NclMalloc(sizeof(HDFAttInqRec));
			(*stepalptr)->att_inq->att_name = NrmStringToQuark("coordsys");
			(*stepalptr)->att_inq->att_val = NrmStringToQuark(coordsys);
			NclFree(coordsys);
			stepalptr = &((*stepalptr)->next);
		} else {
			NclFree(coordsys);
		}
/*
* Instert dimensions in to main file list to support file dimension model
*/
		if(((ndl != -1)&&(nu != -1)&&(nf != -1))&&(IsRef((*stepvlptr)->var_inq->refnum,dlrfl,nd))) {
			for(j = 0; j < (*stepvlptr)->var_inq->n_dims; j++) {
	
				(*stepvlptr)->var_inq->dim_nums[j] = HDFAddDimension(tmp,j,(*stepvlptr)->var_inq->dim_sizes[j],_NclSizeOf(HDFMapToNcl((void*)&(*stepvlptr)->var_inq->number_type)),(*stepvlptr)->var_inq->number_type);
			}
		} else {
			for(j = 0; j < (*stepvlptr)->var_inq->n_dims; j++) {
				(*stepvlptr)->var_inq->dim_nums[j] = HDFAddDumyDimension(tmp,j,(*stepvlptr)->var_inq->dim_sizes[j]);
			}
		}


		stepvlptr = &((*stepvlptr)->next);
		
		
	} 
	tmp->n_file_atts = 0;
	tmp->file_atts = NULL;
	tmp->file_path_q = path;
	return((void*)tmp);
}

static void *HDFCreateFileRec
#if	NhlNeedProto
(NclQuark path)
#else
(path)
NclQuark path;
#endif
{
	HDFFileRecord *tmp = (HDFFileRecord*)NclMalloc(sizeof(HDFFileRecord));
        char *spath = (char*)NrmQuarkToString(path);
        char label_list[NCL_MAX_FVARS * NCL_MAX_STRING];
        uint16 ref_list[NCL_MAX_FVARS];
        int nsets;
        char *step;
	struct stat buf;
	int ret;


	ret = stat(NrmQuarkToString(path),&buf);
       	nsets = DFANlablist(spath,DFTAG_SD,ref_list,(char*)label_list,NCL_MAX_FVARS,NCL_MAX_STRING,1);
	if((nsets == -1)&&(ret == -1)) {
		tmp->file_path_q  = path;
		tmp->wr_status = -1;
		tmp->n_vars = 0;
		tmp->vars = NULL;
		tmp->n_dims = 0;
		tmp->dims = NULL;
		tmp->n_file_atts = 0;
		tmp->file_atts = NULL;

		return(tmp);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The specified file (%s) already exists\n",spath);
		NclFree(tmp);
		return(NULL);
	}
}

static void HDFFreeFileRec
#if	NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFVarInqRecList *stepvl;
	HDFAttInqRecList *stepal;
	HDFDimInqRecList *stepdl;
	
	stepal = rec->file_atts;
	while(rec->file_atts != NULL) {
		stepal = rec->file_atts;
		rec->file_atts = rec->file_atts->next;
		NclFree(stepal->att_inq);
		NclFree(stepal);
	}
	stepdl = rec->dims;
	while(rec->dims != NULL) {
		stepdl = rec->dims;
		rec->dims = rec->dims->next;
		NclFree(stepdl->dim_inq);
		NclFree(stepdl);
	}
	stepvl = rec->vars;
	while(rec->vars != NULL) {
		stepvl = rec->vars;
		stepal = stepvl->var_inq->atts;
		while(stepvl->var_inq->atts != NULL) {
			stepal = stepvl->var_inq->atts; 
			stepvl->var_inq->atts = stepal->next;
			NclFree(stepal->att_inq);
			NclFree(stepal);
		}
		rec->vars = rec->vars->next;
		NclFree(stepvl->var_inq);
		NclFree(stepvl);
	}
	NclFree(rec);
	return;
}

static NclQuark* HDFGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	NclQuark *out_quarks;
	HDFVarInqRecList *stepvl;
	int i;

	if(rec->n_vars > 0) {
		out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark) * rec->n_vars);
		stepvl = rec->vars;
		for(i= 0; i < rec->n_vars; i++) {
			out_quarks[i] = stepvl->var_inq->ncl_valid_name;
			stepvl = stepvl->next;
		}
		*num_vars = rec->n_vars;
		return(out_quarks);
	} else {
		*num_vars = 0;
		return(NULL);
	}
}

static NclFVarRec *HDFGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFVarInqRecList  *stepvl;
	HDFDimInqRecList  *stepdl;
	NclFVarRec *tmp;
	int i,j;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->ncl_valid_name == var_name) {
			tmp = (NclFVarRec*)NclMalloc((unsigned)sizeof(NclFVarRec));
			tmp->var_name_quark = stepvl->var_inq->ncl_valid_name;
			tmp->data_type = HDFMapToNcl((void*)&(stepvl->var_inq->number_type));
			tmp->num_dimensions = stepvl->var_inq->n_dims;
			for(j = 0; j < stepvl->var_inq->n_dims; j++) {
				stepdl = rec->dims;
				while(stepdl->dim_inq->dim_num != stepvl->var_inq->dim_nums[j]) {
					stepdl = stepdl->next;
				}
				tmp->dim_sizes[j] = stepdl->dim_inq->size;
				tmp->file_dim_num[j] = stepdl->dim_inq->dim_num;
			}
			return(tmp);
		} else {
			stepvl = stepvl->next;
		}
	}
	return(NULL);
}

static NclQuark *HDFGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	NclQuark *out_quarks;
	HDFDimInqRecList  *stepdl;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*rec->n_dims);
	stepdl = rec->dims;
	for(i =0 ; i < rec->n_dims; i++) {
		out_quarks[i] = stepdl->dim_inq->ncl_valid_name;
		stepdl = stepdl->next;
	}
	*num_dims = rec->n_dims;
	return(out_quarks);
}

static NclFDimRec *HDFGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
	HDFFileRecord * rec = (HDFFileRecord*)therec;
	NclFDimRec *tmp;
	HDFDimInqRecList *stepdl;

	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->ncl_valid_name == dim_name_q) {
			tmp = (NclFDimRec*)NclMalloc((unsigned)sizeof(NclFDimRec));
			tmp->dim_name_quark = stepdl->dim_inq->ncl_valid_name;
			tmp->dim_size = stepdl->dim_inq->size;
			return(tmp);
		} else {
			stepdl  = stepdl->next;
		}
	}
	return(NULL);
}
static NclQuark *HDFGetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	NclQuark *out_quarks;
	HDFAttInqRecList *stepal;
	int i;

	out_quarks = (NclQuark*)NclMalloc((unsigned) sizeof(NclQuark) * rec->n_file_atts);
	stepal = rec->file_atts;
	for(i = 0; i < rec->n_file_atts; i++) {
		out_quarks[i] = stepal->att_inq->att_name;
		stepal = stepal->next;
	}
	*num_atts = rec->n_file_atts;
	return(out_quarks);
}


static NclQuark *HDFGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFAttInqRecList *stepal;
	HDFVarInqRecList *stepvl;
	NclQuark *out_quarks;
	int i;

	stepvl = rec->vars;
	while(stepvl != NULL) { 
		if(stepvl->var_inq->ncl_valid_name == thevar){
			stepal = stepvl->var_inq->atts;
			*num_atts = stepvl->var_inq->n_atts;
			out_quarks = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*stepvl->var_inq->n_atts);
			for(i = 0; i < stepvl->var_inq->n_atts; i++) {
				out_quarks[i] = stepal->att_inq->att_name;
				stepal = stepal->next;
			}
			return(out_quarks);
		} else {
			stepvl = stepvl->next;
		}
	}
	return(out_quarks);
}



static void *HDFReadVarNS
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,void* storage)
#else
(therec, thevar, start, finish,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
void* storage;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
        HDFVarInqRecList *stepvl;
        void *out_data;
	int n_elem = 1;
	int32 real_start[NCL_MAX_DIMENSIONS];
	int32 count[NCL_MAX_DIMENSIONS];
	int32 stride[NCL_MAX_DIMENSIONS];
        int i;
	intn ret;

	for( i = 0; i < NCL_MAX_DIMENSIONS; i++) {
		count[i] = 0;
		stride[i] = 1;
	}
	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->ncl_valid_name == thevar) {
			for(i =0 ; i < stepvl->var_inq->n_dims; i++) {	
				real_start[i] = start[i] + 1;
				count[i] = finish[i] - start[i] + 1;;
				n_elem *= count[i];
			}
			out_data = storage;
			ret = DFSDreadref(NrmQuarkToString(rec->file_path_q),stepvl->var_inq->refnum);
			ret = DFSDreadslab(NrmQuarkToString(rec->file_path_q),real_start,count,stride,out_data,count);
			
			if(ret == -1) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"NclHDF: An error occured while attempting to read variable (%s) from file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                                return(NULL);
                        } else {
                                return(storage);
                        }
	
		} else {
			stepvl = stepvl->next;
		}
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDF: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
	return(NULL);
}

static NhlErrorTypes HDFWriteVarNS
#if	NhlNeedProto
(void * therec, NclQuark thevar, void *data, long* start, long *finish)
#else
(therec, thevar, data, start, finish)
void * therec;
NclQuark thevar;
void *data;
long *start;
long *finish;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
        HDFVarInqRecList *stepvl;
        HDFDimInqRecList *stepdl;
	int32 start_real[NCL_MAX_DIMENSIONS];
	int32 count[NCL_MAX_DIMENSIONS];
	int32 stride[NCL_MAX_DIMENSIONS];
	int i,n_elem = 1, no_stride = 1;
	int ret;
	
	for(i = 0; i < NCL_MAX_DIMENSIONS; i++) {
		count[i] = 0;
		stride[i] = 1;
	}

	if(rec->wr_status <= 0) {
		stepvl = rec->vars;
		while(stepvl != NULL) {
			if(stepvl->var_inq->ncl_valid_name == thevar) {
				for(i = 0; i < stepvl->var_inq->n_dims; i++) {
					count[i] = finish[i] - start[i] + 1;
					start_real[i] = start[i] + 1;
					n_elem *= count[i]; 
				}
				if(stepvl->var_inq->refnum != -1) {
					ret = DFSDwriteref(NrmQuarkToString(rec->file_path_q),stepvl->var_inq->refnum);
					ret = DFSDsetNT(stepvl->var_inq->number_type);
					ret = DFSDsetdims(stepvl->var_inq->n_dims,stepvl->var_inq->dim_sizes);
					ret = DFSDstartslab(NrmQuarkToString(rec->file_path_q));
					ret = DFSDwriteslab(start_real,stride,count,data);
					DFSDendslab();
					if(ret == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occured while attempting to write variable (%s) to file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                                        	return(NhlFATAL);
					} else {
						return(NhlNOERROR);
					}
				} else {
					ret = DFSDclear();
					ret = DFSDsetNT(stepvl->var_inq->number_type);
					ret = DFSDsetdims(stepvl->var_inq->n_dims,stepvl->var_inq->dim_sizes);
					for(i = 0; i < stepvl->var_inq->n_dims; i++) {
						stepdl = rec->dims;
						while(stepdl != NULL) {
							if(stepdl->dim_inq->dim_num == stepvl->var_inq->dim_nums[i]) {
								break;
							}
							stepdl = stepdl->next;
						}
						ret = DFSDsetlengths(strlen(NrmQuarkToString(stepdl->dim_inq->name)),0,0,0);
						ret = DFSDsetdimstrs(i+1,NrmQuarkToString(stepdl->dim_inq->name),"\0","\0");
						
					}
					ret = DFSDsetlengths(strlen(NrmQuarkToString(thevar)),0,0,0);
					ret = DFSDsetdatastrs(NrmQuarkToString(thevar),"\0","\0","\0");
					ret = DFSDadddata(NrmQuarkToString(rec->file_path_q),stepvl->var_inq->n_dims,stepvl->var_inq->dim_sizes,data);
					if(ret == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NetCdf: An error occured while attempting to add variable (%s) to file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));
                                        	return(NhlFATAL);
					} else {
						stepvl->var_inq->refnum = DFSDlastref();
						return(NhlNOERROR);
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
static NhlErrorTypes HDFAddDim
#if	NhlNeedProto
(void* therec, NclQuark thedim, int size)
#else
(therec, thedim, size)
void* therec;
NclQuark thedim;
int size;
#endif
{
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFDimInqRecList *stepd,*last;
	HDFDimInqRecList *tmpl;
	int i,is_duplicate = 0;
	NclQuark valid_labelq;
	char *step;
	

        stepd = rec->dims;
        last = NULL;
        i = 0;
        while(stepd != NULL) {
                if(stepd->dim_inq->name == thedim) {
                        if(stepd->dim_inq->size == size) {
                                break;
                        } else {
                                is_duplicate = 1;
                                last = stepd;
                                stepd = stepd->next;
                                i++;
                        }
                } else {
                        last = stepd;
                        stepd = stepd->next;
                        i++;
                }
        }
        if(i < rec->n_dims) {
                return(NhlNOERROR);
        } else {
                if(is_duplicate) {
                        step = NclMalloc(strlen(NrmQuarkToString(thedim)) + 3);
                        strcpy(step,NrmQuarkToString(thedim));
                        sprintf(&(step[strlen(NrmQuarkToString(thedim))]),"%d",i);
                        valid_labelq = NrmStringToQuark(step);
                        NclFree(step);
                } else {
                        valid_labelq = thedim; 
                }
                tmpl =  (HDFDimInqRecList*)NclMalloc(sizeof(HDFDimInqRecList));
                tmpl->next = NULL;
                tmpl->dim_inq =  (HDFDimInqRec*)NclMalloc(sizeof(HDFDimInqRec));
                tmpl->dim_inq->dim_num = rec->n_dims;
                tmpl->dim_inq->size = (long)size;
                tmpl->dim_inq->name = thedim;
                tmpl->dim_inq->ncl_valid_name = valid_labelq;
		tmpl->dim_inq->is_duplicate = is_duplicate;
                if(rec->n_dims ==0) {
                        rec->dims = tmpl;
                } else {
                        last->next = tmpl;
                }
                rec->n_dims++;
                return(NhlNOERROR);
        }
	return(NhlFATAL);
}
/*ARGSUSED*/
static NhlErrorTypes HDFAddVar
#if	NhlNeedProto
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
	HDFFileRecord *rec = (HDFFileRecord*)therec;
	HDFVarInqRecList *stepvl;
	HDFDimInqRecList *stepdl;
	int dimids[NCL_MAX_DIMENSIONS];
	int dimsizes[NCL_MAX_DIMENSIONS];
	int ret = 1;
	int *number_type;
	int i;

	if(rec->wr_status <= 0) {
/*
* This should be a valid type. The NclFile object tries to map
* internal Ncl types to proper file format types. This function
* shouldn't be called unless an appropriate type is found
*/
		number_type = (int*)HDFMapFromNcl(data_type);
/*
* Pre condition of function is that all dimensions are correct dimensions for the
* file. Adding the dimensions is handled be the NclFile object.
*/
		for(i = 0; i < n_dims; i++) {
			stepdl = rec->dims;
			while(stepdl != NULL) {
				if(stepdl->dim_inq->ncl_valid_name == dim_names[i]) {
					dimids[i] = stepdl->dim_inq->dim_num;
					dimsizes[i] = stepdl->dim_inq->size;
					break;	
				} else {
					dimids[i] = -1;
					stepdl = stepdl->next;
				}
			}
			if(dimids[i] == -1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFAddVar: unknown dimension passed in (%s)",NrmQuarkToString(dim_names[i]));
				return(NhlFATAL);
			}
		}
		if(number_type != NULL) {
			stepvl = rec->vars;
			if(stepvl == NULL) {
				rec->vars = (HDFVarInqRecList*)NclMalloc((unsigned)sizeof(HDFVarInqRecList));
				rec->vars->next = NULL;
				rec->vars->var_inq = (HDFVarInqRec*)NclMalloc((unsigned)sizeof(HDFVarInqRec));
				rec->vars->var_inq->refnum = -1;
				rec->vars->var_inq->name = thevar;	
				rec->vars->var_inq->ncl_valid_name = thevar;
				rec->vars->var_inq->n_dims = n_dims;
				rec->vars->var_inq->n_atts = 0;
                                rec->vars->var_inq->atts= NULL;
                                rec->vars->var_inq->number_type = *number_type;
				for(i = 0 ; i< n_dims; i++) {
                                        rec->vars->var_inq->dim_nums[i] = dimids[i];
                                        rec->vars->var_inq->dim_sizes[i] = dimsizes[i];
                                }
				rec->n_vars = 1;
			} else {
				while(stepvl->next != NULL) {
					stepvl= stepvl->next;
				}
				stepvl->next = (HDFVarInqRecList*)NclMalloc((unsigned)sizeof(HDFVarInqRecList));
				stepvl->next->var_inq = (HDFVarInqRec*)NclMalloc((unsigned)sizeof(HDFVarInqRec));
				stepvl->next->next = NULL;
				stepvl->next->var_inq->refnum = -1;
				stepvl->next->var_inq->name = thevar;
				stepvl->next->var_inq->ncl_valid_name = thevar;
				stepvl->next->var_inq->n_dims = n_dims;
				stepvl->next->var_inq->n_atts = 0;
				stepvl->next->var_inq->atts= NULL;
				stepvl->next->var_inq->number_type = *number_type;
				for(i = 0 ; i< n_dims; i++) {
                                        stepvl->next->var_inq->dim_nums[i] = dimids[i];
                                        stepvl->next->var_inq->dim_sizes[i] = dimsizes[i];
                                }
				rec->n_vars++;
			}
			return(NhlNOERROR);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) was opened as a read only file, can not write to it",NrmQuarkToString(rec->file_path_q));
        }
        return(NhlFATAL);

}

static NhlErrorTypes HDFRenameDim
#if	NhlNeedProto
(void* therec, NclQuark from, NclQuark to)
#else
(therec, from, to)
void* therec;
NclQuark from;
NclQuark to;
#endif
{
/*
	HDFFileRecord *rec = (HDFFileRecord*)therec;
        HDFDimInqRecList *stepdl,*tmp;
	HDFVarInqRecList *stepvl;
	int dim_num,i,ret;
	int32 start_real[NCL_MAX_DIMENSIONS];
	int32 count[NCL_MAX_DIMENSIONS];
	int32 stride[NCL_MAX_DIMENSIONS];
	int dumy = 1;


	for(i =0; i< NCL_MAX_DIMENSIONS; i++) {
		start_real[i] = 1;
		count[i] = 1;
		stride[i] = 1;
	}

	DFSDclear();
	stepdl = rec->dims;
	while(stepdl!=NULL) {
		if(stepdl->dim_inq->ncl_valid_name == to) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFRenameDim: Dimension name (%s) is already in use, can't rename (%s)\n",NrmQuarkToString(from),NrmQuarkToString(to));
			return(NhlFATAL);
		}
		if(stepdl->dim_inq->ncl_valid_name == from) {
			tmp = stepdl;
			dim_num = stepdl->dim_inq->dim_num;
		}
		stepdl = stepdl->next;
	}

	tmp->dim_inq->ncl_valid_name = to;
	tmp->dim_inq->name = to;

	stepvl = rec->vars;
	while(stepvl != NULL) {
		for(i = 0; i < stepvl->var_inq->n_dims; i++) {
			if((stepvl->var_inq->dim_nums[i] == dim_num)&&(stepvl->var_inq->refnum != -1)) {
				DFSDclear();
				ret = DFSDsetdims(stepvl->var_inq->n_dims,stepvl->var_inq->dim_sizes);
				ret = DFSDwriteref(NrmQuarkToString(rec->file_path_q),stepvl->var_inq->refnum);
				ret = DFSDstartslab(NrmQuarkToString(rec->file_path_q));
				ret = DFSDsetlengths(strlen(NrmQuarkToString(to)),0,0,0);
				ret = DFSDsetdimstrs(dim_num+1,NrmQuarkToString(to),"\0","\0");
				ret = DFSDendslab();
			}
		}
		stepvl = stepvl->next;
	}
	return(NhlNOERROR);
*/
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFRenameDim: HDF DFSD interface does not support changing dimension names");
	return(NhlFATAL);
}


static NclFAttRec* HDFGetAttInfo
#if	NhlNeedProto
(void* therec, NclQuark att_name_q)
#else
(therec, att_name_q)
void* therec;
NclQuark att_name_q;
#endif
{
	return(NULL);
}

static NclFAttRec *HDFGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
	HDFFileRecord * rec = (HDFFileRecord*)therec;
	HDFVarInqRecList * stepvl;
	HDFAttInqRecList * stepal;
	NclFAttRec *tmp = NULL; 

	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->ncl_valid_name == thevar) {
			break;
		} else {
			stepvl = stepvl->next;
		}
	}
	if(stepvl != NULL) {
		stepal = stepvl->var_inq->atts;
		while(stepal != NULL) {
			if(stepal->att_inq->att_name == theatt) {
				break;
			} else {
				stepal = stepal->next;
			}
		}
		if(stepal != NULL) {
			tmp = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
			tmp->att_name_quark = theatt;
			tmp->data_type = NCL_string;
			tmp->num_elements = 1;
		}
		return(tmp);
	} 
	return(NULL);
}

static NclFVarRec *HDFGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
	HDFFileRecord * rec = (HDFFileRecord*)therec;
	HDFDimInqRecList* stepdl;
	NclFVarRec *tmp = NULL;
	
	stepdl = rec->dims;
	while(stepdl != NULL) {
		if(stepdl->dim_inq->ncl_valid_name == thevar) {
			break;
		} else {
			stepdl = stepdl->next;
		}
	}
	if((stepdl != NULL)&&(stepdl->dim_inq->scale != NULL) ) {
		tmp = NclMalloc(sizeof(NclFVarRec));
		tmp->var_name_quark = thevar;
		tmp->data_type = HDFMapToNcl((void*)&(stepdl->dim_inq->scale_number_type));
		tmp->num_dimensions = 1;
		tmp->dim_sizes[0] = stepdl->dim_inq->size;
		tmp->file_dim_num[0] = stepdl->dim_inq->dim_num;
		return(tmp);
	} else {
		return(NULL);
	}
}
static void *HDFReadCoordNS
#if	NhlNeedProto
(void* therec, NclQuark thevar, long* start, long* finish,void* storage)
#else
(therec, thevar, start, finish,storage)
void* therec;
NclQuark thevar;
long* start;
long* finish;
void* storage;
#endif
{
	HDFFileRecord * rec = (HDFFileRecord*)therec;
	HDFDimInqRecList* stepdl;
	void* out_data,*scale;
	int step;
	int to = 0;
	stepdl = rec->dims;

	while(stepdl != NULL) {
		if(stepdl->dim_inq->ncl_valid_name == thevar) {
			break;
		} else {
			stepdl = stepdl->next;
		}
	}
	if((stepdl != NULL)&&(stepdl->dim_inq->scale != NULL) ) {
		if(storage == NULL) {
			out_data = NclMalloc(stepdl->dim_inq->elemsize * (*finish - *start +1 ));			
		} else {
			out_data = storage;
		}
		step = *start;
		scale = stepdl->dim_inq->scale;
		while(step <= *finish) {
			memcpy(&(((char*)out_data)[to]),&(((char*)scale)[step * stepdl->dim_inq->elemsize]),stepdl->dim_inq->elemsize);
			to++;
			step++;
		}
		return(out_data);
	} else {
		return(NULL);
	}
}


static void *HDFReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void *therec;
NclQuark theatt;
void* storage;
#endif
{
	return(NULL);
}

static void *HDFReadVarAtt
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
	HDFFileRecord * rec = (HDFFileRecord*)therec;
	HDFVarInqRecList * stepvl;
	HDFAttInqRecList * stepal;
	NclQuark *out_data;

	if(storage == NULL) {
		out_data = (NclQuark*)NclMalloc(sizeof(NclQuark));
	} else {
		out_data = (NclQuark*)storage;
	}
	stepvl = rec->vars;
	while(stepvl != NULL) {
		if(stepvl->var_inq->ncl_valid_name == thevar) {
			break;
		} else {
			stepvl = stepvl->next;
		}
	}
	if(stepvl != NULL) {
		stepal = stepvl->var_inq->atts;
		while(stepal != NULL) {
			if(stepal->att_inq->att_name == theatt) {
				break;
			} else {
				stepal = stepal->next;
			}
		}
		if(stepal != NULL) {
			*out_data = stepal->att_inq->att_val;
		}
		return(out_data);
	} 
	return(NULL);
}

static NhlErrorTypes HDFWriteCoordNS
#if	NhlNeedProto
(void *therec, NclQuark thevar, void* data, long* start, long* finish)
#else
(therec, thevar, data, start, finish)
void *therec;
NclQuark thevar;
void* data;
long* start;
long* finish;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFWriteCoordNS: adding coordinates to already existing variables or changing coordinates is no currently allowed in HDF");
	return(NhlFATAL);
}


static NhlErrorTypes HDFWriteAtt
#if	NhlNeedProto
(void *therec, NclQuark theatt, void *data )
#else
(therec, theatt, data )
void *therec;
NclQuark theatt;
void *data;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFWriteAtt: adding attributes to already existing variables or changing attributes is no currently allowed in HDF");
	return(NhlFATAL);
}

static NhlErrorTypes HDFWriteVarAtt 
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt, void* data)
#else
(therec,thevar, theatt,  data )
void *therec;
NclQuark thevar;
NclQuark theatt;
void* data;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFWriteVarAtt: adding attributes to already existing variables or changing attributes is no currently allowed in HDF");
	return(NhlFATAL);
}
static NhlErrorTypes HDFAddCoordVar
#if	NhlNeedProto
(void *therec, NclQuark thevar,NclBasicDataTypes data_type)
#else
(therec,thevar,data_type)
void *therec;
NclQuark thevar;
NclBasicDataTypes data_type;
#endif
{
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFAddCoordVar: adding coordinates to already existing variables or changing coordinates is no currently allowed in HDF");
	return(NhlFATAL);
}
static NhlErrorTypes HDFAddAtt
#if	NhlNeedProto
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
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFAddAtt: adding attributes to already existing variables or changing attributes is no currently allowed in HDF");
	return(NhlFATAL);
}

static NhlErrorTypes HDFAddVarAtt
#if	NhlNeedProto
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
	NhlPError(NhlFATAL,NhlEUNKNOWN,"HDFAddVarAtt: adding attributes to already existing variables or changing attributes is no currently allowed in HDF");
	return(NhlFATAL);
}


NclFormatFunctionRec HDFRec = {
/* NclCreateFileRecFunc	   create_file_rec; */		HDFCreateFileRec,
/* NclGetFileRecFunc       get_file_rec; */		HDFGetFileRec,
/* NclFreeFileRecFunc      free_file_rec; */		HDFFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		HDFGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		HDFGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		HDFGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		HDFGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		HDFGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		HDFGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	HDFGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		HDFGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		HDFGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadCoordFunc        read_coord; */		HDFReadCoordNS,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadVarFunc          read_var; */			HDFReadVarNS,
/* NclReadAttFunc          read_att; */			HDFReadAtt,
/* NclReadVarAttFunc       read_var_att; */		HDFReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		HDFWriteCoordNS,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		HDFWriteVarNS,
/* NclWriteAttFunc         write_att; */		HDFWriteAtt,
/* NclWriteVarAttFunc      write_var_att; */		HDFWriteVarAtt,
/* NclAddDimFunc           add_dim; */			HDFAddDim,
/* NclAddDimFunc           rename_dim; */		HDFRenameDim,
/* NclAddVarFunc           add_var; */			HDFAddVar,
/* NclAddVarFunc           add_coord_var; */		HDFAddCoordVar,
/* NclAddAttFunc           add_att; */			HDFAddAtt,
/* NclAddVarAttFunc        add_var_att; */		HDFAddVarAtt,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	HDFMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	HDFMapFromNcl,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL
};
NclFormatFunctionRecPtr HDFAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	
	return(&HDFRec);
}

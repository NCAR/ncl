#include <stdlib.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <netcdf.h>
#include "NclDataDefs.h"
#include "NclFileInterfaces.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include "date.h"
#include "NclGRIB.h"
#include "tables.h"
#include <math.h>

#define NCL_GRIB_CACHE_SIZE  2000

extern int grid_index[];
extern int grid_gds_index[];
extern GridInfoRecord grid[];
extern GridInfoRecord grid_gds[];
extern int grid_tbl_len;
extern int grid_gds_tbl_len;
static void  *vbuf;
extern void GribPushAtt(
#if NhlNeedProto
GribAttInqRecList **att_list_ptr,char* name,void *val,int dimsize,NclObjClass type
#endif
);

static void _NclAdjustCacheTypeAndMissing
#if NhlNeedProto
(int int_or_float,NclMultiDValData the_dat,NclScalar *missingv)
#else
(int_or_float,the_dat,missingv)
(int int_or_float,NclMultiDValData the_dat,NclScalar *missingv)
#endif
{
	if(int_or_float) {
		the_dat->multidval.hlu_type_rep[0] = ((NclTypeintClass)nclTypeintClass)->type_class.hlu_type_rep[0];
		the_dat->multidval.hlu_type_rep[1] = ((NclTypeintClass)nclTypeintClass)->type_class.hlu_type_rep[1];
		the_dat->multidval.data_type = ((NclTypeintClass)nclTypeintClass)->type_class.data_type;
		the_dat->multidval.type = (NclTypeClass)nclTypeintClass;
		if(missingv != NULL) {
			the_dat->multidval.missing_value.has_missing = 1;
			the_dat->multidval.missing_value.value = *missingv;
		} else {
			the_dat->multidval.missing_value.has_missing = 0;
		}
	} else {
/*
* Type is float by defaul/
*/
		if(missingv != NULL) {
			the_dat->multidval.missing_value.has_missing = 1;
			the_dat->multidval.missing_value.value = *missingv;
		} else {
			the_dat->multidval.missing_value.has_missing = 0;
		}
	}
}

static void _NclNewGridCache
#if NhlNeedProto
(GribFileRecord *therec,int grid_number,int has_gds,int gds_tbl_index,int n_dims_lat,int *dimsizes_lat,int n_dims_lon,int *dimsizes_lon)
#else
(therec,grid_number,has_gds,gds_tbl_index,n_dims_lat,dimsizes_lat,n_dims_lon,dimsizes_lon)
GribFileRecord *therec;
int grid_number;
int has_gds;
int gds_tbl_index;
int n_dims_lat;
int *dimsizes_lat;
int n_dims_lon;
int *dimsizes_lon;
#endif
{
	NclGribCacheList *newlist;
	if(therec->grib_grid_cache == NULL) {
		therec->grib_grid_cache = NclMalloc(sizeof(NclGribCacheList));
		newlist = NULL;
	} else {
		newlist = therec->grib_grid_cache;
                therec->grib_grid_cache = NclMalloc(sizeof(NclGribCacheList));
	}
		
	therec->grib_grid_cache->grid_number = grid_number;
	therec->grib_grid_cache->has_gds = has_gds;
	therec->grib_grid_cache->grid_gds_tbl_index = gds_tbl_index;
	if((n_dims_lon == 1) &&(n_dims_lat ==1)) {
		therec->grib_grid_cache->n_dims = 2;
		therec->grib_grid_cache->dimsizes[1] = *dimsizes_lon;
		therec->grib_grid_cache->dimsizes[0] = *dimsizes_lat;
	} else if((n_dims_lon ==2) &&(n_dims_lat ==2)&&(dimsizes_lon[0] == dimsizes_lat[0])&&(dimsizes_lon[1] == dimsizes_lat[1])) {
		therec->grib_grid_cache->n_dims = 2;
		therec->grib_grid_cache->dimsizes[1] = dimsizes_lon[1];
		therec->grib_grid_cache->dimsizes[0] = dimsizes_lon[0];
	} else {
		if(n_dims_lat ==2) {
			therec->grib_grid_cache->n_dims = 2;
			therec->grib_grid_cache->dimsizes[1] = dimsizes_lat[1];
			therec->grib_grid_cache->dimsizes[0] = dimsizes_lat[0];
		} else if(n_dims_lon ==2) {
			therec->grib_grid_cache->n_dims = 2;
			therec->grib_grid_cache->dimsizes[1] = dimsizes_lon[1];
			therec->grib_grid_cache->dimsizes[0] = dimsizes_lon[0];
		}
	}
	therec->grib_grid_cache->n_entries = 0;
	therec->grib_grid_cache->thelist = NULL;
	therec->grib_grid_cache->next = newlist;
}
static NclMultiDValData  _NclGetCacheVal
#if	NhlNeedProto
(GribFileRecord *therec,GribParamList *step,GribRecordInqRec *current_rec)
#else
(therec,step,current_rec)
GribFileRecord *therec;
GribParamList *step;
GribRecordInqRec *current_rec;
#endif
{
/*
	NclGribCacheList *thelist;
	void *val;
	thelist = therec->grib_grid_cache;
				val = NclMalloc(sizeof(float)*thelist->dimsizes[0]*thelist->dimsizes[1]);

                                return(_NclCreateVal(NULL,
                                                                NULL,
                                                                Ncl_MultiDValData,
                                                                0,
                                                                val,
                                                                NULL,
                                                                thelist->n_dims,
                                                                thelist->dimsizes,
                                                                PERMANENT,
                                                                NULL,
                                                                nclTypefloatClass));
*/
	NclGribCacheList *thelist;
	NclGribCacheRec *tmp;
	void *val;
	thelist = therec->grib_grid_cache;
	while(thelist != NULL) {
		if((thelist->grid_number == step->grid_number)&&(thelist->has_gds ==step->has_gds)&&(thelist->grid_gds_tbl_index == step->grid_gds_tbl_index)) {
			if(thelist->n_entries == NCL_GRIB_CACHE_SIZE) {
				tmp = thelist->tail;
				tmp->rec->the_dat = NULL;
				tmp->rec = current_rec;
				tmp->prev->next = NULL;
				thelist->tail = tmp->prev;
				tmp->prev = NULL;
				tmp->next = thelist->thelist;
				tmp->next->prev = tmp;
				thelist->thelist = tmp;
				return(tmp->thevalue);
			} if(thelist->n_entries == 0) {
				thelist->thelist = NclMalloc(sizeof(NclGribCacheRec));
				thelist->thelist->prev = NULL;
				thelist->thelist->next = NULL;
				thelist->thelist->rec = current_rec;
				thelist->tail = thelist->thelist;
				val = NclMalloc(sizeof(float)*thelist->dimsizes[0]*thelist->dimsizes[1]);
				thelist->thelist->thevalue = _NclCreateVal(NULL,
								NULL,
								Ncl_MultiDValData,
								0,
								val,
								NULL,
								thelist->n_dims,
								thelist->dimsizes,
								PERMANENT,
								NULL,
								nclTypefloatClass);
				thelist->n_entries = 1;
				return(thelist->thelist->thevalue);
			} else {
				tmp = NclMalloc(sizeof(NclGribCacheRec));
				tmp->prev = NULL;
				tmp->next = thelist->thelist;
				tmp->next->prev = tmp;
				tmp->rec = current_rec;
				val = NclMalloc(sizeof(float)*thelist->dimsizes[0]*thelist->dimsizes[1]);
                                tmp->thevalue = _NclCreateVal(NULL,
                                                                NULL,
                                                                Ncl_MultiDValData,
                                                                0,
                                                                val,
                                                                NULL,
                                                                thelist->n_dims,
                                                                thelist->dimsizes,
                                                                PERMANENT,
                                                                NULL,
                                                                nclTypefloatClass);
				++thelist->n_entries;
				
				thelist->thelist = tmp;
				return(tmp->thevalue);
			}
		} else {
			thelist = thelist->next;
		}
	}
	return(NULL);
}


static NclMultiDValData  _GribGetInternalVar
#if	NhlNeedProto
(GribFileRecord * therec,NclQuark name_q, NclGribFVarRec **vrec)
#else
(therec,name_q)
GribFileRecord * therec;
NclQuark name_q;
#endif
{
	GribInternalVarList *vstep; 

	vstep = therec->internal_var_list;
	while(vstep != NULL ) {
		if(vstep->int_var->var_info.var_name_quark == name_q) {
			*vrec = &vstep->int_var->var_info;
			return(vstep->int_var->value);
		} else {
			vstep = vstep->next;
		}
	}
	*vrec = NULL;
	return(NULL);
}

static void _GribAddInternalVar
#if	NhlNeedProto
(GribFileRecord *therec,NclQuark name_q,int *dim_numbers, NclMultiDValData tmp_md,GribAttInqRecList *attlist,int natts)
#else
(therec,dim_name_q,tmp_md)
GribFileRecord *therec;
NclQuark dim_name_q;
NclMultiDValData *tmp_md;
GribAttInqRecList *attlist;
int natts;
#endif
{
	GribInternalVarList *vstep; 
	GribInternalVarRec *tmp;
	int i;

	tmp = (GribInternalVarRec*)NclMalloc(sizeof(GribInternalVarRec));
	tmp->var_info.data_type = tmp_md->multidval.data_type;
	tmp->var_info.var_name_quark = name_q;
	tmp->var_info.num_dimensions = tmp_md->multidval.n_dims;
	for (i = 0; i < tmp_md->multidval.n_dims; i++) {
/*
		tmp->var_info.dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
*/
		tmp->var_info.file_dim_num[i] = dim_numbers[i];
	}
	tmp->value = tmp_md;
	vstep = (GribInternalVarList*)NclMalloc(sizeof(GribInternalVarList));
	vstep->next = therec->internal_var_list;
	vstep->int_var = tmp;
	vstep->int_var->n_atts = natts;
	vstep->int_var->theatts = attlist;
	therec->internal_var_list = vstep;
	therec->n_internal_vars++;
	return;

}

static void _GribFreeParamRec0
#if	NhlNeedProto
(GribParamList * vstep)
#else
(vstep)
GribParamList * vstep;
#endif
{
	int i;
	GribAttInqRecList *astep= NULL,*tmp =NULL;
	GribRecordInqRecList *list,*tlist;

	if(vstep != NULL){
		if(vstep->it_vals != NULL) {
			NclFree(vstep->it_vals);
		}
		list = vstep->thelist;
		for(i= 0; i< vstep->n_entries; i++) {
			if(list->rec_inq != NULL) {
				if(list->rec_inq->var_name != NULL) {
					NclFree(list->rec_inq->var_name);
				}
				if(list->rec_inq->gds != NULL) {
					NclFree(list->rec_inq->gds);
				}
				if(list->rec_inq->the_dat != NULL) {
					_NclDestroyObj((NclObj)list->rec_inq->the_dat);
				}
				NclFree(list->rec_inq);
			}
			tlist = list;
			list = list->next;
			NclFree(tlist);
		}
		if(vstep->forecast_time != NULL) {
			_NclDestroyObj((NclObj)vstep->forecast_time);
		}
		if(vstep->yymmddhh!= NULL) {
			_NclDestroyObj((NclObj)vstep->yymmddhh);
		}
		if(vstep->levels!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels);
		}
		if(vstep->levels0!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels0);
		}
		if(vstep->levels1!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels1);
		}
		astep = vstep->theatts;
		for(i = 0; i < vstep->n_atts; i++) {
			_NclDestroyObj((NclObj)astep->att_inq->thevalue);
			NclFree(astep->att_inq);	
			tmp = astep;
			astep = astep->next;
			NclFree(tmp);
		}
		NclFree(vstep);
	}
	return;
}
static void _GribFreeParamRec
#if	NhlNeedProto
(GribParamList * vstep)
#else
(vstep)
GribParamList * vstep;
#endif
{
	int i;
	GribAttInqRecList *astep= NULL,*tmp =NULL;
	if(vstep != NULL){
		if(vstep->it_vals != NULL) {
			NclFree(vstep->it_vals);
		}
		for(i= 0; i< vstep->n_entries; i++) {
			if(vstep->thelist[i].rec_inq != NULL) {
				if(vstep->thelist[i].rec_inq->var_name != NULL) {
					NclFree(vstep->thelist[i].rec_inq->var_name);
				}
				if(vstep->thelist[i].rec_inq->gds != NULL) {
					NclFree(vstep->thelist[i].rec_inq->gds);
				}
				if(vstep->thelist[i].rec_inq->the_dat != NULL) {
					_NclDestroyObj((NclObj)vstep->thelist[i].rec_inq->the_dat);
				}
				NclFree(vstep->thelist[i].rec_inq);
			}
		}
		if(vstep->forecast_time != NULL) {
			_NclDestroyObj((NclObj)vstep->forecast_time);
		}
		if(vstep->yymmddhh!= NULL) {
			_NclDestroyObj((NclObj)vstep->yymmddhh);
		}
		if(vstep->levels!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels);
		}
		if(vstep->levels0!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels0);
		}
		if(vstep->levels1!= NULL) {
			_NclDestroyObj((NclObj)vstep->levels1);
		}
		astep = vstep->theatts;
		for(i = 0; i < vstep->n_atts; i++) {
			_NclDestroyObj((NclObj)astep->att_inq->thevalue);
			NclFree(astep->att_inq);	
			tmp = astep;
			astep = astep->next;
			NclFree(tmp);
		}
		NclFree(vstep->thelist);
		NclFree(vstep);
	}
	return;
}

static NclBasicDataTypes GribMapToNcl 
#if	NhlNeedProto
(void* the_type)
#else
(the_type)
	void *the_type;
#endif
{
	int int_or_float = *(int*)the_type;

	if(int_or_float) {
		return(NCL_int);
	} else {
		return(NCL_float);
	}
}

static void *GribMapFromNcl
#if	NhlNeedProto
(NclBasicDataTypes the_type)
#else
(the_type)
	NclBasicDataTypes the_type;
#endif
{
	int *tmp ;

	tmp = (int*)NclMalloc((unsigned)sizeof(int));
	
	switch(the_type) {
	case NCL_int:
		*tmp = 1;
		break;
	case NCL_float:
		*tmp = 0;
		break;
	default:
		*tmp = -1;
	}
	return((void*)tmp);
}

static int GetLVList
#if 	NhlNeedProto
(GribParamList *thevar,GribRecordInqRecList *lstep,int** lv_vals, int** lv_vals1) 
#else
(thevar,lstep,lv_vals, lv_vals1) 
GribParamList *thevar;
GribRecordInqRecList *lstep;
int** lv_vals; 
int** lv_vals1; 
#endif
{
	int n_lvs = 1;
	int i;
	GribRecordInqRecList *strt,*tmp;
	
	strt = lstep;
/*
	fprintf(stdout,"%d/%d/%d\t(%d:%d)-%d,%d\t%d,%d\ttoff=%d\t%d,%d,%d\n",
		(int)strt->rec_inq->pds[13],
		(int)strt->rec_inq->pds[14],
		(int)strt->rec_inq->pds[12],
		(int)strt->rec_inq->pds[15],
		(int)strt->rec_inq->pds[16],
		(int)strt->rec_inq->pds[18],
		(int)strt->rec_inq->pds[19],
		(int)strt->rec_inq->pds[17],
		(int)strt->rec_inq->pds[20],
		strt->rec_inq->time_offset,
		(int)strt->rec_inq->pds[9],
		strt->rec_inq->level0,
		strt->rec_inq->level1);
*/

	while(strt->next != NULL) {
		if((strt->rec_inq->level0 == strt->next->rec_inq->level0)&&(strt->rec_inq->level1 == strt->next->rec_inq->level1)) {
/*
			if((strt->rec_inq->bds_flags & (char)0360) != (strt->next->rec_inq->bds_flags&(char)0360)) {
				fprintf(stdout,"Dup BDSC: Flag error\n");
			}
			if(strt->rec_inq->has_bms != strt->next->rec_inq->has_bms) {
				fprintf(stdout,"Dup BMSC: BMS error\n");
			}
			if(strt->rec_inq->pds_size != strt->next->rec_inq->pds_size) {
				fprintf(stdout,"Dup PDSC: Size Error\n");
			} else {
				fprintf(stdout,"Dup PDSC: %s\t%d\n",(GdsCompare(strt->rec_inq->pds,strt->next->rec_inq->pds,strt->rec_inq->pds_size)?"Equal":"Not Equal"),(int)strt->rec_inq->pds[9]);
			}
			fprintf(stdout,"GDS SIZE (%d)\n",strt->rec_inq->gds_size);
			if(strt->rec_inq->gds_size != strt->next->rec_inq->gds_size) {
				fprintf(stdout,"Dup GDSC: Size Error\n");
			} else {
				fprintf(stdout,"Dup GDSC: %s\n",(GdsCompare(strt->rec_inq->gds,strt->next->rec_inq->gds,strt->rec_inq->gds_size)?"Equal":"Not Equal"));
			}
			fprintf(stdout,"Dup: (%s,%s)\t(%d,%d)\n",strt->rec_inq->var_name,strt->next->rec_inq->var_name,strt->rec_inq->start,strt->next->rec_inq->start);
*/
/*
			NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Duplicate GRIB record found!! NCL has no way of knowing which is valid, so skipping one arbitrarily!");
			tmp = strt->next;
			strt->next = strt->next->next;
			if(tmp->rec_inq->var_name != NULL) {
				NclFree(tmp->rec_inq->var_name);
			}
*
* Very important to update n_entries.
* Needed later on in _MakeArray
*
			thevar->n_entries--;
			NclFree(tmp->rec_inq);
			NclFree(tmp);
*/
			n_lvs++;
			strt = strt->next;
		} else {
/*
			fprintf(stdout,"%d/%d/%d\t(%d:%d)-%d,%d\t%d,%d\ttoff=%d\t%d,%d,%d\n",
				(int)strt->next->rec_inq->pds[13],
				(int)strt->next->rec_inq->pds[14],
				(int)strt->next->rec_inq->pds[12],
				(int)strt->next->rec_inq->pds[15],
				(int)strt->next->rec_inq->pds[16],
				(int)strt->next->rec_inq->pds[18],
				(int)strt->next->rec_inq->pds[19],
				(int)strt->next->rec_inq->pds[17],
				(int)strt->next->rec_inq->pds[20],
				strt->next->rec_inq->time_offset,
				(int)strt->next->rec_inq->pds[9],
				strt->next->rec_inq->level0,
				strt->next->rec_inq->level1);
*/

			n_lvs++;
			strt = strt->next;
		}
	}
	strt = lstep;
	*lv_vals = (int*)NclMalloc((unsigned)sizeof(int)*n_lvs);
	if(strt->rec_inq->level1 != -1) {
		*lv_vals1 = (int*)NclMalloc((unsigned)sizeof(int)*n_lvs);
	}
	for(i = 0; i < n_lvs; i++) {
		(*lv_vals)[i] = strt->rec_inq->level0;
		if(strt->rec_inq->level1 != -1) {
			(*lv_vals1)[i] = strt->rec_inq->level1;
		}
		strt = strt->next;
	}
	return(n_lvs);
}

static void Merge2
#if 	NhlNeedProto
(int *tmp_lvs,int *tmp_lvs1,int *tmp_n_lvs,int *lv_vals,int *lv_vals1,int n_lv,int** out_lvs0,int **out_lvs1)
#else
(tmp_lvs,tmp_lvs,tmp_n_lvs,lv_vals,lv_vals1,n_lv,out_lvs0,out_lvs1)
int *tmp_lvs;
int *tmp_lvs1;
int *tmp_n_lvs;
int *lv_vals;
int *lv_vals1;
int n_lv;
int **out_lvs0;
int **out_lvs1;
#endif
{
	int i,j,k;
	int *tmp_out_lvs = NULL;
	int *tmp_out_lvs1 = NULL;

	i = 0;	
	j = 0;
	k = 0;

	tmp_out_lvs = (int*)NclMalloc((unsigned)sizeof(int)*(*tmp_n_lvs + n_lv));
	tmp_out_lvs1 = (int*)NclMalloc((unsigned)sizeof(int)*(*tmp_n_lvs + n_lv));


		
	while((i < *tmp_n_lvs)&&(j< n_lv)) {
		if((tmp_lvs[i] == lv_vals[j])&&(tmp_lvs1[i] == lv_vals1[j])) {
			tmp_out_lvs[k] = tmp_lvs[i];
			tmp_out_lvs1[k] = tmp_lvs1[i];
			i++;
			j++;
			k++;
		} else if((tmp_lvs[i] < lv_vals[j])||((tmp_lvs[i] == lv_vals[j])&&(tmp_lvs1[i] != lv_vals1[j]))){
			tmp_out_lvs[k] = tmp_lvs[i];
			tmp_out_lvs1[k] = tmp_lvs1[i];
			k++;
			i++;
		} else {
			tmp_out_lvs[k] = lv_vals[j];
			tmp_out_lvs1[k] = lv_vals1[j];
			k++;
			j++;
		}
	}
	if(i< *tmp_n_lvs) {
		for( ; i < *tmp_n_lvs;i++) {
			tmp_out_lvs[k] = tmp_lvs[i];
			tmp_out_lvs1[k] = tmp_lvs1[i];
			k++;
		}	
	} else {
		for( ; j < n_lv ;j++) {
			tmp_out_lvs[k] = lv_vals[j];
			tmp_out_lvs1[k] = lv_vals1[j];
			k++;
		}	
	}
	

	NclFree(tmp_lvs);
	NclFree(tmp_lvs1);
	*tmp_n_lvs = k;	
	*out_lvs0 = tmp_out_lvs;
	*out_lvs1 = tmp_out_lvs1;
	return;
}
static int *Merge
#if 	NhlNeedProto
(int *tmp_lvs,int *tmp_n_lvs,int *lv_vals,int n_lv)
#else
(tmp_lvs,tmp_n_lvs,lv_vals,n_lv)
int *tmp_lvs;
int *tmp_n_lvs;
int *lv_vals;
int n_lv;
#endif
{
	int i,j,k;
	int *out_lvs = NULL;

	i = 0;	
	j = 0;
	k = 0;

	out_lvs = (int*)NclMalloc((unsigned)sizeof(int)*(*tmp_n_lvs + n_lv));


		
	while((i < *tmp_n_lvs)&&(j< n_lv)) {
		if(tmp_lvs[i] == lv_vals[j]) {
			out_lvs[k] = tmp_lvs[i];
			i++;
			j++;
			k++;
		} else if(tmp_lvs[i] < lv_vals[j]) {
			out_lvs[k] = tmp_lvs[i];
			k++;
			i++;
		} else {
			out_lvs[k] = lv_vals[j];
			k++;
			j++;
		}
	}
	if(i< *tmp_n_lvs) {
		for( ; i < *tmp_n_lvs;i++) {
			out_lvs[k] = tmp_lvs[i];
			k++;
		}	
	} else {
		for( ; j < n_lv ;j++) {
			out_lvs[k] = lv_vals[j];
			k++;
		}	
	}
	

	NclFree(tmp_lvs);
	*tmp_n_lvs = k;	
	return(out_lvs);
}
static FTLIST *GetFTList
#if 	NhlNeedProto
(GribParamList *thevar,GribRecordInqRecList *step,int* n_ft,int **ft_vals,int* total_valid_lv ,int** valid_lv_vals,int** valid_lv_vals1)
#else
(thevar,step, n_ft, ft_vals,total_valid_lv, valid_lv_vals, valid_lv_vals1)
GribParamList *thevar;
GribRecordInqRecList *fstep;
int* n_ft;
int **ft_vals;
int* total_valid_lv;
int** valid_lv_vals;
int** valid_lv_vals1;
#endif
{
	int i;
	GribRecordInqRecList *strt,*fnsh,*fstep;
	int n_fts = 0;
	int n_nodes;
	int current_offset;
	FTLIST header;
	FTLIST *the_end,*tmp;
	int *tmp_lvs = NULL;
	int *tmp_lvs1 = NULL;
	int tmp_n_lvs = 0;


	the_end = &header;
	the_end->next = NULL;
	
	strt = fstep = step;

	current_offset = strt->rec_inq->time_offset;
	while(fstep->next != NULL) {
		if(fstep->next->rec_inq->time_offset != current_offset) {
			fnsh = fstep;
			fstep = fstep->next;
			fnsh->next = NULL;
			the_end->next = (FTLIST*)NclMalloc((unsigned)sizeof(FTLIST));
			the_end = the_end->next;
			the_end->ft = current_offset;
			the_end->thelist = strt;
			the_end->next = NULL;
			the_end->lv_vals = NULL;
			the_end->lv_vals1 = NULL;
			the_end->n_lv = 0;
			the_end->n_lv = GetLVList(thevar,strt,&the_end->lv_vals,&the_end->lv_vals1);
			if((strt->rec_inq->level0 != -1)&&(strt->rec_inq->level1 == -1)) {
				if(tmp_lvs == NULL) {
					tmp_lvs = (int*)NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					tmp_n_lvs = the_end->n_lv;
					memcpy((void*)tmp_lvs,the_end->lv_vals,the_end->n_lv*sizeof(int));
				} else {
					tmp_lvs = Merge(tmp_lvs,&tmp_n_lvs,the_end->lv_vals,the_end->n_lv);
				}
			} else if((strt->rec_inq->level0 != -1)&&(strt->rec_inq->level1 != -1)){
/*
* Handle multiple value coordiante levels
*/
				if(tmp_lvs == NULL) {
					tmp_lvs = (int*)NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					tmp_lvs1 = (int*)NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					tmp_n_lvs = the_end->n_lv;
					memcpy((void*)tmp_lvs,the_end->lv_vals,the_end->n_lv*sizeof(int));
					memcpy((void*)tmp_lvs1,the_end->lv_vals1,the_end->n_lv*sizeof(int));
				} else {
					Merge2(tmp_lvs,tmp_lvs1,&tmp_n_lvs,the_end->lv_vals,the_end->lv_vals1,the_end->n_lv,&tmp_lvs,&tmp_lvs1);
				}
			}
			strt = fstep;
			current_offset = strt->rec_inq->time_offset;
			n_fts++;
		} else {
			fstep = fstep->next;
		}
	}
	the_end->next =(FTLIST*)NclMalloc((unsigned)sizeof(FTLIST));
	the_end = the_end->next;
	the_end->ft = current_offset;
	the_end->thelist = strt;
	the_end->next = NULL;
	the_end->lv_vals = NULL;
	the_end->lv_vals1 = NULL;
	the_end->n_lv = 0;
	the_end->n_lv = GetLVList(thevar,strt,&the_end->lv_vals,&the_end->lv_vals1);
	if((strt->rec_inq->level0 != -1)&&(strt->rec_inq->level1 == -1)){
		if(tmp_lvs != NULL) {
			tmp_lvs = Merge(tmp_lvs,&tmp_n_lvs,the_end->lv_vals,the_end->n_lv);
		} else {
			tmp_lvs = (int*)NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
			tmp_n_lvs = the_end->n_lv;
			memcpy((void*)tmp_lvs,the_end->lv_vals,the_end->n_lv*sizeof(int));
		}
	} else if((strt->rec_inq->level0 != -1)&&(strt->rec_inq->level1 != -1)){
/*
* Handle multiple value coordiante levels
*/
		if(tmp_lvs == NULL) {
			tmp_lvs = (int*)NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
			tmp_lvs1 = (int*)NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
			tmp_n_lvs = the_end->n_lv;
			memcpy((void*)tmp_lvs,the_end->lv_vals,the_end->n_lv*sizeof(int));
			memcpy((void*)tmp_lvs1,the_end->lv_vals1,the_end->n_lv*sizeof(int));
		} else {
			Merge2(tmp_lvs,tmp_lvs1,&tmp_n_lvs,the_end->lv_vals,the_end->lv_vals1,the_end->n_lv,&tmp_lvs,&tmp_lvs1);
		}
	}
	n_fts++;
	*n_ft = n_fts;
	*ft_vals = NclMalloc((unsigned)sizeof(int)*n_fts);
	the_end = header.next;
	for (i = 0; i < n_fts; i++) {
		(*ft_vals)[i] = the_end->ft;
		the_end = the_end->next;
	}
	*total_valid_lv = tmp_n_lvs;
	*valid_lv_vals = tmp_lvs;
	*valid_lv_vals1 = tmp_lvs1;
	return(header.next);
}
static GetItQuark
#if NhlNeedProto
(GIT *the_it)
#else
(the_it)
GIT *the_it;
#endif
{
	int y = 0;
	unsigned short mn = 0;
	unsigned short d = 0;
	int h = 0;
	int mi = 0;
	char buffer[100];

	JulianDiffDate(1,1,the_it->year,the_it->days_from_jan1,&d,&mn,&y);
	if(mn < 10) {
		sprintf(buffer,"0%d/",mn);
	} else {
		sprintf(buffer,"%d/",mn);
	}
	if(d < 10) {
		sprintf(&(buffer[strlen(buffer)]),"0%d/",d);
	} else {
		sprintf(&(buffer[strlen(buffer)]),"%d/",d);
	}
	sprintf(&(buffer[strlen(buffer)]),"%d ",y);

	if(((int)the_it->minute_of_day / 60) < 10) {
		sprintf(&(buffer[strlen(buffer)]),"(0%d:",(int)the_it->minute_of_day / 60);
	} else {
		sprintf(&(buffer[strlen(buffer)]),"(%d:",(int)the_it->minute_of_day / 60);
	}
	if(((int)the_it->minute_of_day % 60) < 10 ) {
		sprintf(&(buffer[strlen(buffer)]),"0%d)",(int)the_it->minute_of_day % 60);
	} else {
		sprintf(&(buffer[strlen(buffer)]),"%d)",(int)the_it->minute_of_day % 60);
	}
	return(NrmStringToQuark(buffer));
}


static void _SetAttributeLists
#if 	NhlNeedProto
(GribFileRecord *therec)
#else
(therec)
GribFileRecord *therec;
#endif
{
	GribParamList *step = NULL;
	NclQuark *tmp_string = NULL;
	TBLE2 *the_param = NULL;
	int tmp_dimsizes = 1;
	GribRecordInqRec *grib_rec = NULL;
	GribAttInqRecList *att_list_ptr= NULL;
	GribAttInqRec 	*att_ptr= NULL;
	int i;
	int *tmp_level = NULL;
	void *tmp_fill = NULL;


	step = therec->var_list;
	
	while(step != NULL) {
		the_param = NULL;
/*
* Handle long_name, units, center, sub_center, model and _FillValue
*/	
		for(i = 0; i < step->n_entries; i++) {
			if(step->thelist[i].rec_inq != NULL) {
				grib_rec = step->thelist[i].rec_inq;
				break;
			}
		}
		if(grib_rec->param_tbl_index != -1) {
			if(grib_rec->param_number < 128) {
				the_param = &(params[grib_rec->param_tbl_index]);
			} else {
				switch((int)grib_rec->pds[4]) {
					case 98:
						the_param = &(params_ecmwf[grib_rec->param_tbl_index]);
						break;
					case 59:
						the_param = &(params_fsl[grib_rec->param_tbl_index]);
						break;
					case 7:
				case 8:
					case 9:
						the_param = &(params_nwsnmc[grib_rec->param_tbl_index]);
						break;
					defalut:
						NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unlocatable parameter number (%d) from center (%d), extension of NclGRIB required",grib_rec->param_number,(int)grib_rec->pds[4]);
						break;
	
				}
			}
		}
		
		if(the_param!=NULL) {
/*
* long_name
*/
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("long_name");
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark(the_param->long_name);		
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
			step->theatts = att_list_ptr;
			step->n_atts++;
/*
* units
*/
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("units");
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = NrmStringToQuark(the_param->units);		
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
			step->theatts = att_list_ptr;
			step->n_atts++;
		} else {
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("long_name");
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = grib_rec->long_name_q;		
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
			step->theatts = att_list_ptr;
			step->n_atts++;
/*
* units
*/
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("units");
			tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmp_string = grib_rec->units_q;		
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
			step->theatts = att_list_ptr;
			step->n_atts++;
		}
		att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
		att_list_ptr->next = step->theatts;
		att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
		att_list_ptr->att_inq->name = NrmStringToQuark(NCL_MISSING_VALUE_ATT);
		if(step->var_info.data_type == NCL_int) {
			tmp_fill = NclMalloc(sizeof(int));
			*(int*)tmp_fill = DEFAULT_MISSING_INT;
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_fill, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypeintClass);
		} else {
			tmp_fill = NclMalloc(sizeof(float));
			*(float*)tmp_fill = DEFAULT_MISSING_FLOAT;
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_fill, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypefloatClass);
		}

		step->theatts = att_list_ptr;
		step->n_atts++;

/*
* center
*/
		for( i = 0; i < sizeof(centers_index)/sizeof(int);i++) {
			if(centers_index[i] == (int)grib_rec->pds[4]) {
				att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
				att_list_ptr->next = step->theatts;
				att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
				att_list_ptr->att_inq->name = NrmStringToQuark("center");
				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
				*tmp_string = NrmStringToQuark(centers[i]);		
				att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
				step->theatts = att_list_ptr;
				step->n_atts++;
			}
		}
/*
*  sub_center
*/
		for( i = 0; i < sizeof(sub_centers_index)/sizeof(int);i++) {
			if(sub_centers_index[i] == (int)grib_rec->pds[25]) {
				att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
				att_list_ptr->next = step->theatts;
				att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
				att_list_ptr->att_inq->name = NrmStringToQuark("sub_center");
				tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
				*tmp_string = NrmStringToQuark(sub_centers[i]);		
				att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
				step->theatts = att_list_ptr;
				step->n_atts++;
			}
		}

/*
* model
*/
		switch((int)grib_rec->pds[4]) {
			case 7:
				for( i = 0; i < sizeof(model_index)/sizeof(int);i++) {
					if(model_index[i] == (int)grib_rec->pds[5]) {
						att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
						att_list_ptr->next = step->theatts;
						att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
						att_list_ptr->att_inq->name = NrmStringToQuark("model");
						tmp_string = (NclQuark*)NclMalloc(sizeof(NclQuark));
						*tmp_string = NrmStringToQuark(model[i]);		
						att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_string, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypestringClass);
						step->theatts = att_list_ptr;
						step->n_atts++;
					}
				}
				break;
			default:
				break;
		}

/*
* Handle coordinate attributes,  level, initial_time, forecast_time
*/
		if(step->yymmddhh_isatt) {
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("initial_time");
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->yymmddhh;
/*
* Don't want two references
*/
			step->yymmddhh = NULL;
			step->theatts = att_list_ptr;
			step->n_atts++;
		}
		if(step->forecast_time_isatt) {
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("forecast_time");
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->forecast_time;
/*
* Don't want two references
*/
			step->forecast_time= NULL;
			step->theatts = att_list_ptr;
			step->n_atts++;
		}
		if((step->levels_isatt)&&(!step->levels_has_two)) {
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("level");
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->levels;
/*
* Don't want two references
*/
			step->levels= NULL;
			step->theatts = att_list_ptr;
			step->n_atts++;
		} else if((step->levels_isatt)&&(step->levels_has_two)){
/*
* TEMPORARY would like to change attribute name to reflect level indicator
*/
			tmp_level = (int*)NclMalloc(sizeof(int) * 2);
			att_list_ptr = (GribAttInqRecList*)NclMalloc((unsigned)sizeof(GribAttInqRecList));
			att_list_ptr->next = step->theatts;
			att_list_ptr->att_inq = (GribAttInqRec*)NclMalloc((unsigned)sizeof(GribAttInqRec));
			att_list_ptr->att_inq->name = NrmStringToQuark("level");
/*
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)step->levels0;
*/
			tmp_level[0] = *(int*)step->levels0->multidval.val;
			tmp_level[1] = *(int*)step->levels1->multidval.val;
			tmp_dimsizes = 2;
			att_list_ptr->att_inq->thevalue = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)tmp_level, NULL, 1 , &tmp_dimsizes, PERMANENT, NULL, nclTypeintClass);
			tmp_dimsizes = 1;
/*
* Don't want two references
*/
			_NclDestroyObj((NclObj)step->levels0);
			_NclDestroyObj((NclObj)step->levels1);
			step->levels0= NULL;
			step->levels1= NULL;
			step->theatts = att_list_ptr;
			step->n_atts++;
		}

		step = step->next;
	}
}
int GdsCompare(unsigned char *a,unsigned char *b,int n) {
	int i;
	for( i = 0; i < n ; i++) {
		if(*(a++)!=*(b++)) {	
			return(0);
		}
	}
	return(1);
}
static void _SetFileDimsAndCoordVars
#if 	NhlNeedProto
(GribFileRecord *therec)
#else
(therec)
GribFileRecord *therec;
#endif
{
	GribParamList *step,*last,*tmpstep;
	char buffer[80];
	NclQuark gridx_q,lat_q;
	GribDimInqRecList *dstep,*ptr;
	GribDimInqRec *tmp;
	NclQuark *it_rhs, *it_lhs;
	int *rhs, *lhs;
	int *rhs1, *lhs1;
	float *rhs_f, *lhs_f;
	int i,j;
	int current_dim = 0;
	NclMultiDValData tmp_md;
	NclMultiDValData tmp_md1;
	NclGribFVarRec *test;
	int n_dims_lat;
	int n_dims_lon;
	int *dimsizes_lat = NULL;
	int *dimsizes_lon = NULL;
	float *tmp_lat = NULL;
	float *tmp_lon = NULL;
	NhlErrorTypes is_err = NhlNOERROR;
	int tmp_file_dim_numbers[2];
	char name_buffer[80];
	GribAttInqRecList *att_list_ptr= NULL;
	GribAttInqRecList *tmp_att_list_ptr= NULL;
        GribAttInqRec   *att_ptr= NULL;
	int natts = 0;
	NclQuark *tmp_string = NULL;
	float *tmp_float = NULL;
	int tmp_dimsizes = 1;
	int nlonatts = 0;
	int nlatatts = 0;
	GribAttInqRecList *lat_att_list_ptr;
	GribAttInqRecList *lon_att_list_ptr;


	
	therec->total_dims = 0;
	therec->n_it_dims = 0;
	therec->it_dims = NULL;
	therec->n_ft_dims = 0;
	therec->ft_dims = NULL;
	therec->n_lv_dims = 0;
	therec->lv_dims = NULL;
	therec->n_grid_dims = 0;
	therec->grid_dims = NULL;
	step = therec->var_list;
/*
	step = NULL;
*/
	last = NULL;

	while(step != NULL) {
		current_dim = 0;

		if(!step->yymmddhh_isatt) {
			dstep = therec->it_dims;
			for(i = 0; i < therec->n_it_dims; i++) {
				if(dstep->dim_inq->size == step->yymmddhh->multidval.dim_sizes[0]) {
					tmp_md = _GribGetInternalVar(therec,dstep->dim_inq->dim_name,&test);
					if(tmp_md != NULL) {
						it_lhs = (NclQuark*)tmp_md->multidval.val;
	
						it_rhs = (NclQuark*)step->yymmddhh->multidval.val;
						j = 0;
						while(j<dstep->dim_inq->size) {
							if(it_lhs[j] != it_rhs[j]) {
								break;
							} else {
								j++;
							}
						}
						if(j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep = dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}
/*
* All pointers to coordate will end up in dim list not in param list
*/
			if(dstep == NULL) {
/*
* Need a new dimension entry w name and number
*/
				tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->size = step->yymmddhh->multidval.dim_sizes[0];
				sprintf(buffer,"initial_time%d",therec->total_dims);
				tmp->dim_name = NrmStringToQuark(buffer);
				tmp->is_gds = -1;
				therec->total_dims++;
				ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->it_dims;
				therec->it_dims = ptr;
				therec->n_it_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;
				_GribAddInternalVar(therec,tmp->dim_name,&tmp->dim_number,(NclMultiDValData)step->yymmddhh,NULL,0);
				step->yymmddhh = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj)step->yymmddhh);
				step->yymmddhh = NULL;
			}
			current_dim++;
		}
		if(!step->forecast_time_isatt) {
			dstep = therec->ft_dims;
			for(i = 0; i < therec->n_ft_dims; i++) {
				if(dstep->dim_inq->size == step->forecast_time->multidval.dim_sizes[0]) {
					tmp_md = _GribGetInternalVar(therec,dstep->dim_inq->dim_name,&test);
					if(tmp_md != NULL) {
						lhs = (int*)tmp_md->multidval.val;
						rhs = (int*)step->forecast_time->multidval.val;
						j = 0;
						while(j<dstep->dim_inq->size) {
							if(lhs[j] != rhs[j]) {
								break;
							} else {
								j++;
							}
						}
						if(j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep  = dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}
			if(dstep == NULL) {
/*
* Need a new dimension entry w name and number
*/
				tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->size = step->forecast_time->multidval.dim_sizes[0];
				sprintf(buffer,"forecast_time%d",therec->total_dims);
				tmp->dim_name = NrmStringToQuark(buffer);
				tmp->is_gds = -1;
				therec->total_dims++;
				ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->ft_dims;
				therec->ft_dims = ptr;
				therec->n_ft_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;
				_GribAddInternalVar(therec,tmp->dim_name,&tmp->dim_number,(NclMultiDValData)step->forecast_time,NULL,0);
				step->forecast_time = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj)step->forecast_time);
				step->forecast_time = NULL;
			}
			current_dim++;
		}
		if((!step->levels_isatt)&& (step->levels!=NULL)) {
			dstep = therec->lv_dims;
			for(i = 0; i < therec->n_lv_dims; i++) {
				if(dstep->dim_inq->size == step->levels->multidval.dim_sizes[0]) {
					tmp_md = _GribGetInternalVar(therec,dstep->dim_inq->dim_name,&test);
					if(tmp_md != NULL ) {
						lhs = (int*)tmp_md->multidval.val;
						rhs = (int*)step->levels->multidval.val;
						j = 0;
						while(j<dstep->dim_inq->size) {
							if(lhs[j] != rhs[j]) {
								break;
							} else {
								j++;
							}
						}
						if(j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep= dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}
			if(dstep == NULL) {
/*
* Need a new dimension entry w name and number
*/
				tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->is_gds = -1;
				tmp->size = step->levels->multidval.dim_sizes[0];
				for(i = 0; i < sizeof(level_index)/sizeof(int); i++) {
					if(level_index[i] == step->level_indicator) {
						break;
					}
				}
				if(i < sizeof(level_index)/sizeof(int)) {
					sprintf(buffer,"lv_%s%d",level_str[i],therec->total_dims);
				} else {
					sprintf(buffer,"levels%d",therec->total_dims);
				}
			
				tmp->dim_name = NrmStringToQuark(buffer);
				therec->total_dims++;
				ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->lv_dims;
				therec->lv_dims = ptr;
				therec->n_lv_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;
				

				_GribAddInternalVar(therec,tmp->dim_name,&tmp->dim_number,(NclMultiDValData)step->levels,NULL,0);
				step->levels = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj)step->levels);
				step->levels = NULL;
			}
			current_dim++;
		} else if((!step->levels_isatt)&& (step->levels0 !=NULL)&&(step->levels1 !=NULL)) {

			dstep = therec->lv_dims;
			for(i = 0; i < therec->n_lv_dims; i++) {
				if(dstep->dim_inq->size == step->levels0->multidval.dim_sizes[0]) {
					sprintf(name_buffer,"%s%s",NrmQuarkToString(dstep->dim_inq->dim_name),"_l0");
					tmp_md = _GribGetInternalVar(therec,NrmStringToQuark(name_buffer),&test);
					sprintf(name_buffer,"%s%s",NrmQuarkToString(dstep->dim_inq->dim_name),"_l1");
					tmp_md1 = _GribGetInternalVar(therec,NrmStringToQuark(name_buffer),&test);
					if((tmp_md != NULL )&&(tmp_md1 != NULL) ) {
						lhs = (int*)tmp_md->multidval.val;
						rhs = (int*)step->levels0->multidval.val;
						lhs1 = (int*)tmp_md1->multidval.val;
						rhs1 = (int*)step->levels1->multidval.val;
						j = 0;
						while(j<dstep->dim_inq->size) {
							if((lhs[j] != rhs[j])||(lhs1[j] != rhs1[j])) {
								break;
							} else {
								j++;
							}
						}
						if(j == dstep->dim_inq->size) {
							break;
						} else {
							dstep= dstep->next;
						}
					} else {
						dstep= dstep->next;
					}
				} else {
					dstep = dstep->next;
				}
			}
			if(dstep == NULL) {
/*
* Need a new dimension entry w name and number
*/
				tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
				tmp->dim_number = therec->total_dims;
				tmp->is_gds = -1;
				tmp->size = step->levels0->multidval.dim_sizes[0];
				for(i = 0; i < sizeof(level_index)/sizeof(int); i++) {
					if(level_index[i] == step->level_indicator) {
						break;
					}
				}
				if(i < sizeof(level_index)/sizeof(int)) {
					sprintf(buffer,"lv_%s%d",level_str[i],therec->total_dims);
				} else {
					sprintf(buffer,"levels%d",therec->total_dims);
				}
			
				tmp->dim_name = NrmStringToQuark(buffer);
				therec->total_dims++;
				ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
				ptr->dim_inq = tmp;
				ptr->next = therec->lv_dims;
				therec->lv_dims = ptr;
				therec->n_lv_dims++;
				step->var_info.file_dim_num[current_dim] = tmp->dim_number;
				sprintf(name_buffer,"%s%s",buffer,"_l0");
				_GribAddInternalVar(therec,NrmStringToQuark(name_buffer),&tmp->dim_number,(NclMultiDValData)step->levels0,NULL,0);
				sprintf(name_buffer,"%s%s",buffer,"_l1");
				_GribAddInternalVar(therec,NrmStringToQuark(name_buffer),&tmp->dim_number,(NclMultiDValData)step->levels1,NULL,0);
				step->levels0 = NULL;
				step->levels1 = NULL;
			} else {
				step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
				_NclDestroyObj((NclObj)step->levels0);
				_NclDestroyObj((NclObj)step->levels1);
				step->levels0 = NULL;
				step->levels1 = NULL;
			}
			current_dim++;
		}
/*
* Now its time to get the grid coordinates  and define grid variables
* First switch on whether record has GDS or not
* if not :
*  check to see if dimensions are defined
*  if not: check get_grid field
*     then get_grid.
*     if its more that 1D then
*       define gridx_### and gridy_### and then add variables gridlat_### and gridlon_### if gridx and gridy are 
*     else
*       define lat_### and lon_### and add them as variables of the same name (real coordinate variables)
*/


		if((step->grid_tbl_index != -1)&&(grid[step->grid_tbl_index].get_grid != NULL)) {
/* 
* Search for both gridlat_## and gridx_## grid number will always be added in sequence so finding lat or x means
* you have the lon and y file dimension numbers.
*/
			sprintf(buffer,"gridx_%d",step->grid_number);
			gridx_q = NrmStringToQuark(buffer);
			sprintf(buffer,"lat_%d",step->grid_number);
			lat_q = NrmStringToQuark(buffer);
		} else if((step->has_gds)&&(step->grid_gds_tbl_index != -1)/*&&(step->grid_number == 255)*/) { 
			
			if((grid_gds[step->grid_gds_tbl_index].un_pack == NULL)&&(grid_gds[step->grid_gds_tbl_index].get_grid == NULL)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Parameter (%d) has an unsupported GDS type (%d), GDS's are not currently supported",step->param_number,step->gds_type);
				is_err = NhlFATAL;
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Unsupported grid number (%d) can't decode",step->grid_number);
			is_err = NhlFATAL;
		}
		if(is_err == NhlNOERROR) {

			if((step->has_gds)&&(step->grid_gds_tbl_index != -1)/*&&(step->grid_number == 255)*/) {
/*
* For gds grid it must be decoded every time since several different grid could be defined
* by the smae grid_type number.
*/
/*
				(*grid_gds[step->grid_gds_tbl_index].get_grid)(step,&tmp_lat,&n_dims_lat,&dimsizes_lat,&tmp_lon,&n_dims_lon,&dimsizes_lon);
*/
				dstep = therec->grid_dims;
				while(dstep != NULL) {
					if((dstep->dim_inq->is_gds == step->gds_type)&&
						(dstep->dim_inq->gds_size == step->thelist->rec_inq->gds_size)&&
						(GdsCompare(dstep->dim_inq->gds,step->thelist->rec_inq->gds,step->thelist->rec_inq->gds_size))) {
						break;
					} else {
						dstep = dstep->next;
					}
				}
			}  else {
				dstep = therec->grid_dims;
				while(dstep != NULL) {
					if((dstep->dim_inq->dim_name == gridx_q )||(dstep->dim_inq->dim_name == lat_q)) {
						break;
					} else {
						dstep = dstep->next;
					}
				}
			}
		if(is_err != NhlFATAL) {
			if(dstep == NULL) {
/*
* Grid has not been defined
*/
				if(step->grid_tbl_index!=-1) {
					(*grid[step->grid_tbl_index].get_grid)(step,&tmp_lat,&n_dims_lat,&dimsizes_lat,&tmp_lon,&n_dims_lon,&dimsizes_lon);
					if((grid[step->grid_tbl_index].get_grid_atts) != NULL) {
						nlonatts = 0;
						nlatatts = 0;
						lat_att_list_ptr = NULL;
						lon_att_list_ptr = NULL;
						(*grid[step->grid_tbl_index].get_grid_atts)(step,&lat_att_list_ptr,&nlatatts,&lon_att_list_ptr,&nlonatts);
					}
				} else if(step->grid_gds_tbl_index != -1) {

					(*grid_gds[step->grid_gds_tbl_index].get_grid)(step,&tmp_lat,&n_dims_lat,&dimsizes_lat,&tmp_lon,&n_dims_lon,&dimsizes_lon);
					nlonatts = 0;
					nlatatts = 0;
					lat_att_list_ptr = NULL;
					lon_att_list_ptr = NULL;
				}
				_NclNewGridCache(therec,step->grid_number,step->has_gds,step->grid_gds_tbl_index,n_dims_lat,dimsizes_lat,n_dims_lon,dimsizes_lon);

	/*
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	* Grids always need to be inserted into the grid_dim list in the right order. First lon is pushed then lat so that dstep->dim_inq
	* always points to lat and dstep->next->dim_inq point to lon
	!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	*/
					if((n_dims_lon == 1)&&(n_dims_lat == 1)) {
						step->var_info.dim_sizes[current_dim] = dimsizes_lat[0];
						step->var_info.dim_sizes[current_dim+1] = dimsizes_lon[0];
						step->var_info.file_dim_num[current_dim] = therec->total_dims;
						step->var_info.file_dim_num[current_dim+1] = therec->total_dims + 1;
	/*
	* y or lon first!
	*/
						if((step->has_gds)&&(step->grid_number == 255)) {
							sprintf(buffer,"g%d_lon_%d",step->gds_type,therec->total_dims+1);
						} else {
							sprintf(buffer,"lon_%d",step->grid_number);
						}
						tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
						tmp->dim_number = therec->total_dims + 1;
						tmp->size = dimsizes_lon[0];
						tmp->dim_name = NrmStringToQuark(buffer);
						tmp->is_gds = step->gds_type;
						tmp->gds_size = step->thelist->rec_inq->gds_size;
						tmp->gds = (unsigned char*)NclMalloc(step->thelist->rec_inq->gds_size);
						memcpy(tmp->gds,step->thelist->rec_inq->gds,step->thelist->rec_inq->gds_size);
						ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
						ptr->dim_inq= tmp;
						ptr->next = therec->grid_dims;
						therec->grid_dims = ptr;
						therec->n_grid_dims++;
	/*	
	   not valid for single dimensioned coords
						tmp_float = NclMalloc((unsigned)sizeof(float)*2);
						tmp_float[0] = tmp_lon[0];
						tmp_float[1] = tmp_lon[dimsizes_lon[0]-1];
						GribPushAtt(&lon_att_list_ptr,"corners",tmp_float,2,nclTypefloatClass); nlonatts++;
	*/

						
						_GribAddInternalVar(therec,tmp->dim_name,&tmp->dim_number,(NclMultiDValData)_NclCreateVal(
														NULL,
														NULL,
														Ncl_MultiDValData,
														0,
														(void*)tmp_lon,
														NULL,
														n_dims_lon,
														dimsizes_lon,
														TEMPORARY,
														NULL,
														nclTypefloatClass),lon_att_list_ptr,nlonatts);
						NclFree(dimsizes_lon);
						if((step->has_gds)&&(step->grid_number == 255)) {
							sprintf(buffer,"g%d_lat_%d",step->gds_type,therec->total_dims);
						} else {
							sprintf(buffer,"lat_%d",step->grid_number);
						}
						tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
						tmp->dim_number = therec->total_dims;
						tmp->size = dimsizes_lat[0];
						tmp->dim_name = NrmStringToQuark(buffer);
						tmp->is_gds = step->gds_type;
						tmp->gds_size = step->thelist->rec_inq->gds_size;
						tmp->gds = (unsigned char*)NclMalloc(step->thelist->rec_inq->gds_size);
						memcpy(tmp->gds,step->thelist->rec_inq->gds,step->thelist->rec_inq->gds_size);
						ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
						ptr->dim_inq= tmp;
						ptr->next = therec->grid_dims;
						therec->grid_dims = ptr;
						therec->n_grid_dims++;

	/*	
	   not valid for single dimensioned coords
						tmp_float = NclMalloc((unsigned)sizeof(float)*2);
						tmp_float[0] = tmp_lat[0];
						tmp_float[1] = tmp_lat[dimsizes_lat[0]-1];
						GribPushAtt(&lat_att_list_ptr,"corners",tmp_float,2,nclTypefloatClass); nlatatts++;
	*/

						_GribAddInternalVar(therec,tmp->dim_name,&tmp->dim_number,(NclMultiDValData)_NclCreateVal(
														NULL,
														NULL,
														Ncl_MultiDValData,
														0,
														(void*)tmp_lat,
														NULL,
														n_dims_lat,
														dimsizes_lat,
														TEMPORARY,
														NULL,
														nclTypefloatClass),lat_att_list_ptr,nlatatts);
						NclFree(dimsizes_lat);
						therec->total_dims += 2;



					} else if((n_dims_lon ==2)&&(n_dims_lat ==2)&&(dimsizes_lat[0] == dimsizes_lon[0])&&(dimsizes_lat[1] == dimsizes_lon[1])) {
						step->var_info.dim_sizes[current_dim] = dimsizes_lat[0];
						step->var_info.dim_sizes[current_dim+1] = dimsizes_lon[1];
						step->var_info.file_dim_num[current_dim] = therec->total_dims;
						step->var_info.file_dim_num[current_dim+1] = therec->total_dims + 1;

						if((step->has_gds)&&(step->grid_number == 255)) {
							sprintf(buffer,"g%d_y_%d",step->gds_type,therec->total_dims + 1);
						} else {
							sprintf(buffer,"gridy_%d",step->grid_number);
						}
						tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
						tmp->dim_number = therec->total_dims + 1;
						tmp->size = dimsizes_lon[1];
						tmp->dim_name = NrmStringToQuark(buffer);
						tmp->is_gds = step->gds_type;
						tmp->gds_size = step->thelist->rec_inq->gds_size;
						tmp->gds = (unsigned char*)NclMalloc(step->thelist->rec_inq->gds_size);
						memcpy(tmp->gds,step->thelist->rec_inq->gds,step->thelist->rec_inq->gds_size);
						ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
						ptr->dim_inq= tmp;
						ptr->next = therec->grid_dims;
						therec->grid_dims = ptr;
						therec->n_grid_dims++;
						if((step->has_gds)&&(step->grid_number == 255)) {
							sprintf(buffer,"g%d_lon_%d",step->gds_type,therec->total_dims + 1);
						} else {
							sprintf(buffer,"gridlon_%d",step->grid_number);
						}
						tmp_file_dim_numbers[0] = therec->total_dims;
						tmp_file_dim_numbers[1] = therec->total_dims+ 1;

						tmp_float = NclMalloc((unsigned)sizeof(float)*4);
						tmp_float[0] = tmp_lon[0];
						tmp_float[1] = tmp_lon[dimsizes_lon[1]-1];
						tmp_float[2] = tmp_lon[(dimsizes_lon[0]-1) * dimsizes_lon[1]];
						tmp_float[3] = tmp_lon[(dimsizes_lon[0] * dimsizes_lon[1])-1];
						GribPushAtt(&lon_att_list_ptr,"corners",tmp_float,4,nclTypefloatClass); nlonatts++;

						_GribAddInternalVar(therec,NrmStringToQuark(buffer),tmp_file_dim_numbers,(NclMultiDValData)_NclCreateVal(
														NULL,
														NULL,
														Ncl_MultiDValData,
														0,
														(void*)tmp_lon,
														NULL,
														n_dims_lon,
														dimsizes_lon,
														TEMPORARY,
														NULL,
														nclTypefloatClass),lon_att_list_ptr,nlonatts);
						NclFree(dimsizes_lon);
						if((step->has_gds)&&(step->grid_number == 255)) {
							sprintf(buffer,"g%d_x_%d",step->gds_type,therec->total_dims);
						} else {
							sprintf(buffer,"gridx_%d",step->grid_number);
						}
						tmp = (GribDimInqRec*)NclMalloc((unsigned)sizeof(GribDimInqRec));
						tmp->dim_number = therec->total_dims;
						tmp->size = dimsizes_lat[0];
						tmp->dim_name = NrmStringToQuark(buffer);
						tmp->is_gds = step->gds_type;
						tmp->gds_size = step->thelist->rec_inq->gds_size;
						tmp->gds = (unsigned char*)NclMalloc(step->thelist->rec_inq->gds_size);
						memcpy(tmp->gds,step->thelist->rec_inq->gds,step->thelist->rec_inq->gds_size);
						ptr = (GribDimInqRecList*)NclMalloc((unsigned)sizeof(GribDimInqRecList));
						ptr->dim_inq= tmp;
						ptr->next = therec->grid_dims;
						therec->grid_dims = ptr;
						therec->n_grid_dims++;
						if((step->has_gds)&&(step->grid_number == 255)) {
							sprintf(buffer,"g%d_lat_%d",step->gds_type,therec->total_dims);
						} else {
							sprintf(buffer,"gridlat_%d",step->grid_number);
						}
						tmp_float = NclMalloc((unsigned)sizeof(float)*4);
						tmp_float[0] = tmp_lat[0];
						tmp_float[1] = tmp_lat[dimsizes_lat[1]-1];
						tmp_float[2] = tmp_lat[(dimsizes_lat[0]-1) * dimsizes_lat[1]];
						tmp_float[3] = tmp_lat[(dimsizes_lat[0] * dimsizes_lat[1])-1];
						GribPushAtt(&lat_att_list_ptr,"corners",tmp_float,4,nclTypefloatClass); nlatatts++;

						_GribAddInternalVar(therec,NrmStringToQuark(buffer),tmp_file_dim_numbers,(NclMultiDValData)_NclCreateVal(
														NULL,
														NULL,
														Ncl_MultiDValData,
														0,
														(void*)tmp_lat,
														NULL,
														n_dims_lat,
														dimsizes_lat,
														TEMPORARY,
														NULL,
														nclTypefloatClass),lat_att_list_ptr,nlatatts);
						NclFree(dimsizes_lat);
						therec->total_dims += 2;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Couldn't handle dimension information returned by grid decoding");
						is_err = NhlFATAL;
					}
				} else {
					step->var_info.dim_sizes[current_dim] = dstep->dim_inq->size;
					step->var_info.file_dim_num[current_dim] = dstep->dim_inq->dim_number;
					step->var_info.dim_sizes[current_dim+1] = dstep->next->dim_inq->size;
					step->var_info.file_dim_num[current_dim+1] = dstep->next->dim_inq->dim_number;
				}
			}
		}
		if(is_err == NhlNOERROR) {
			last = step;	
			step = step->next;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Deleting reference to parameter because of decoding error");
			is_err = NhlNOERROR;
			if(last != NULL) {
				last->next = step->next;
			} else {
				therec->var_list = step->next;
			}
			tmpstep = step;
			step = step->next;
			_GribFreeParamRec(tmpstep);
			therec->n_vars--;
		}
	}
	return;
}

static NhlErrorTypes _DetermineDimensionAndGridInfo
#if     NhlNeedProto
(GribFileRecord *therec, GribParamList* step)
#else
(therec,step)
GribFileRecord *therec;
GribParamList* step;
#endif
{
	GribRecordInqRecList *rstep,*strt,*fnsh,*free_rec;
	GIT current_it;
	int n_it = 0,i,j,k,l;
	ITLIST  header;
	ITLIST  *the_end,*free_it;
	FTLIST  *ftstep,*free_ft;
	int *tmp_lv_vals= NULL;
	int *tmp_lv_vals1= NULL;
	int n_tmp_lv_vals = 0;
	int * tmp_ft_vals = NULL;
	int n_tmp_ft_vals = 0;
	GIT *tmp_it_vals = NULL;
	int n_tmp_it_vals = 0;
	NclQuark *it_vals_q = NULL;
	int total;
	char *name;
	NhlErrorTypes returnval = NhlNOERROR;
	
	

	the_end = &header;
	header.next = NULL;
	header.thelist = NULL;
	header.ft_vals = NULL;
	header.lv_vals1 = NULL;
	header.lv_vals = NULL;
	header.n_lv = 0;
	header.n_ft = 0;
	


	if(step->n_entries > 1) {
		strt = rstep  = step->thelist;
		current_it = rstep->rec_inq->initial_time;


		while(rstep->next != NULL) {
			if((rstep->next->rec_inq->initial_time.year != current_it.year)
				||(rstep->next->rec_inq->initial_time.days_from_jan1 != current_it.days_from_jan1)
				||(rstep->next->rec_inq->initial_time.minute_of_day != current_it.minute_of_day)) {
	
				current_it = rstep->next->rec_inq->initial_time;
				fnsh = rstep;
				rstep = rstep->next;
				fnsh->next = NULL;
		
				the_end->next = (ITLIST*)NclMalloc((unsigned)sizeof(ITLIST));
				the_end = the_end->next;
				the_end->next = NULL;
				the_end->it = strt->rec_inq->initial_time;
				the_end->thelist = GetFTList(step,strt,&the_end->n_ft,&the_end->ft_vals,&the_end->n_lv,&the_end->lv_vals,&the_end->lv_vals1);
				strt = rstep;
				n_it++;
			} else {
				rstep = rstep->next;
			}
		}
		the_end->next = (ITLIST*)NclMalloc((unsigned)sizeof(ITLIST));
		the_end = the_end->next;
		the_end->next = NULL;
		the_end->it = strt->rec_inq->initial_time;
		the_end->thelist = GetFTList(step,strt,&the_end->n_ft,&the_end->ft_vals,&the_end->n_lv,&the_end->lv_vals,&the_end->lv_vals1);
		n_it++;
		name = NrmQuarkToString(strt->rec_inq->var_name_q);

		the_end = header.next;

		n_tmp_it_vals = n_it;	
		tmp_it_vals = (GIT*)NclMalloc((unsigned)sizeof(GIT)*n_it);
		it_vals_q = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*n_it);
		i = 0;
		while(the_end != NULL) {
			tmp_it_vals[i] = the_end->it;
			if((the_end->n_lv > 0)&&(the_end->lv_vals1 == NULL) ) {
				if(tmp_lv_vals == NULL) {
					tmp_lv_vals = NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					n_tmp_lv_vals = the_end->n_lv;
					memcpy((void*)tmp_lv_vals,the_end->lv_vals,the_end->n_lv*sizeof(int));
				} else 	{
					tmp_lv_vals  = Merge(tmp_lv_vals,&n_tmp_lv_vals,the_end->lv_vals,the_end->n_lv);
				}
			} else {
				if(tmp_lv_vals == NULL) {
					tmp_lv_vals = NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					tmp_lv_vals1 = NclMalloc((unsigned)sizeof(int)*the_end->n_lv);
					n_tmp_lv_vals = the_end->n_lv;
					memcpy((void*)tmp_lv_vals,the_end->lv_vals,the_end->n_lv*sizeof(int));
					memcpy((void*)tmp_lv_vals1,the_end->lv_vals1,the_end->n_lv*sizeof(int));
				} else 	{
					Merge2(tmp_lv_vals,tmp_lv_vals1,&n_tmp_lv_vals,the_end->lv_vals,the_end->lv_vals1,the_end->n_lv,&tmp_lv_vals,&tmp_lv_vals1);
				}
			}
			if(the_end->n_ft > 0) {
				if(tmp_ft_vals == NULL) {
					tmp_ft_vals = NclMalloc((unsigned)sizeof(int)*the_end->n_ft);
					n_tmp_ft_vals = the_end->n_ft;
					memcpy((void*)tmp_ft_vals,the_end->ft_vals,the_end->n_ft*sizeof(int));
				} else {
					tmp_ft_vals = Merge(tmp_ft_vals,&n_tmp_ft_vals,the_end->ft_vals,the_end->n_ft);
				}
			}
			it_vals_q[i] = GetItQuark(&the_end->it);

			the_end = the_end->next;
/*
			fprintf(stdout,"%s\n",NrmQuarkToString(it_vals_q[i]));
*/
			i++;
		}
/*
		if(n_tmp_lv_vals > 0) {
			fprintf(stdout,"(");
			for(j = 0; j< n_tmp_lv_vals-1; j++) {	
				fprintf(stdout,"%d, ",tmp_lv_vals[j]);
			}

			fprintf(stdout,"%d)\n",tmp_lv_vals[j]);
		}
		if( n_tmp_ft_vals > 0) {
			fprintf(stdout,"(");
			for(j = 0; j< n_tmp_ft_vals-1; j++) {	
				fprintf(stdout,"%d, ",tmp_ft_vals[j]);
			}
			fprintf(stdout,"%d)\n",tmp_ft_vals[j]);
		}
*/
		
	} else {
/*
		if((step->thelist->rec_inq->level0 != -1)&&(step->thelist->rec_inq->level1 == -1)) {
			n_tmp_lv_vals = 1;
			tmp_lv_vals = (int*)NclMalloc(sizeof(int));
			*tmp_lv_vals = step->thelist->rec_inq->level0;
		} else if((step->thelist->rec_inq->level0 != -1)&&(step->thelist->rec_inq->level1 != -1)) {
			n_tmp_lv_vals = 1;
			tmp_lv_vals = (int*)NclMalloc(sizeof(int));
			*tmp_lv_vals = step->thelist->rec_inq->level0;
			tmp_lv_vals1 = (int*)NclMalloc(sizeof(int));
			*tmp_lv_vals1 = step->thelist->rec_inq->level1;
		}
		n_tmp_ft_vals = 1;
		tmp_ft_vals = (int*)NclMalloc(sizeof(int));
		*tmp_ft_vals = step->thelist->rec_inq->time_offset;
*/
		n_tmp_it_vals = 1;
		tmp_it_vals = (GIT*)NclMalloc((unsigned)sizeof(GIT));
		*tmp_it_vals = step->minimum_it;
		it_vals_q = (NclQuark*)NclMalloc(sizeof(NclQuark));
		*it_vals_q = GetItQuark(&step->minimum_it);
		header.next = (ITLIST*)NclMalloc((unsigned)sizeof(ITLIST));
		header.next->lv_vals = NULL;
		header.next->lv_vals1 = NULL;
		header.next->ft_vals = NULL;
		header.next->next = NULL;
		header.next->it = step->minimum_it;
		header.next->thelist = GetFTList(step,step->thelist,&n_tmp_ft_vals,&tmp_ft_vals,&n_tmp_lv_vals,&tmp_lv_vals,&tmp_lv_vals1);
/*
		fprintf(stdout,"%d/%d/%d\t(%d:%d)-%d,%d\t%d,%d\ttoff=%d\t%d,%d,%d\n",
			(int)step->thelist->rec_inq->pds[13],
			(int)step->thelist->rec_inq->pds[14],
			(int)step->thelist->rec_inq->pds[12],
			(int)step->thelist->rec_inq->pds[15],
			(int)step->thelist->rec_inq->pds[16],
			(int)step->thelist->rec_inq->pds[18],
			(int)step->thelist->rec_inq->pds[19],
			(int)step->thelist->rec_inq->pds[17],
			(int)step->thelist->rec_inq->pds[20],
			step->thelist->rec_inq->time_offset,
			(int)step->thelist->rec_inq->pds[9],
			step->thelist->rec_inq->level0,
			step->thelist->rec_inq->level1);
*/
	}

	i = 0;
	step->var_info.num_dimensions = 0;
	if(n_tmp_it_vals > 1 ) {
		step->var_info.dim_sizes[i] = n_tmp_it_vals;
		step->yymmddhh = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)it_vals_q,
					NULL,
					1,
					(void*)&n_tmp_it_vals,
					TEMPORARY,
					NULL,
					nclTypestringClass);
		step->it_vals = tmp_it_vals;
			
		step->yymmddhh_isatt = 0;
		i++;
	} else if(n_tmp_it_vals == 1) {
		step->yymmddhh_isatt = 1;
		step->yymmddhh = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)it_vals_q,
					NULL,
					1,
					(void*)&n_tmp_it_vals,
					TEMPORARY,
					NULL,
					nclTypestringClass);
		step->it_vals = tmp_it_vals;
	} else {
		step->yymmddhh_isatt = 0;
		step->yymmddhh = NULL;
		step->it_vals = NULL;
/*
		fprintf(stdout,"n_it: %d\n",n_tmp_it_vals);
*/
	}
	if(n_tmp_ft_vals > 1 ) {
		step->var_info.dim_sizes[i] = n_tmp_ft_vals;
		step->forecast_time = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)tmp_ft_vals,
					NULL,
					1,
					(void*)&n_tmp_ft_vals,
					TEMPORARY,
					NULL,
					nclTypeintClass);
		step->forecast_time_isatt = 0;
		i++;
	} else if(n_tmp_ft_vals == 1) {
		step->forecast_time = (NclOneDValCoordData)_NclCreateVal(
					NULL,
					NULL,
					Ncl_OneDValCoordData,
					0,
					(void*)tmp_ft_vals,
					NULL,
					1,
					(void*)&n_tmp_ft_vals,
					TEMPORARY,
					NULL,
					nclTypeintClass);
		step->forecast_time_isatt = 1;
	} else {
		step->forecast_time_isatt = 0;
/*
		fprintf(stdout,"n_ft: %d\n",n_tmp_ft_vals);
*/
	}
	if((tmp_lv_vals != NULL)&&(tmp_lv_vals1 == NULL)) {
		if(n_tmp_lv_vals > 1 ) {
			step->var_info.dim_sizes[i] = n_tmp_lv_vals;
			step->levels = (NclOneDValCoordData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_OneDValCoordData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels0 = NULL;
			step->levels1 = NULL;
			i++;
				step->levels_isatt = 0;
		} else if (n_tmp_lv_vals == 1) {
			step->levels_isatt = 1;
			step->levels = (NclOneDValCoordData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_OneDValCoordData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels0 = NULL;
			step->levels1 = NULL;
		} else {
			step->levels_isatt = 0;
/*
			fprintf(stdout,"n_lv: %d\n",n_tmp_lv_vals);
*/
		}
	} else if((tmp_lv_vals != NULL)&&(tmp_lv_vals1 != NULL)) { 
		if(n_tmp_lv_vals > 1 ) {
			step->var_info.dim_sizes[i] = n_tmp_lv_vals;
			step->levels = NULL;
			step->levels0 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels1 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals1,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels_has_two = 1;
			i++;
				step->levels_isatt = 0;
		} else if (n_tmp_lv_vals == 1) {
			step->levels_isatt = 1;
			step->levels = NULL;
			step->levels0 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels1 = (NclMultiDValData)_NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValData,
						0,
						(void*)tmp_lv_vals1,
						NULL,
						1,
						(void*)&n_tmp_lv_vals,
						TEMPORARY,
						NULL,
						nclTypeintClass);
			step->levels_has_two = 1;
		} else {
			step->levels_isatt = 0;
/*
			fprintf(stdout,"n_lv: %d\n",n_tmp_lv_vals);
*/
		}
	} else {
		step->levels_isatt = 0;
	}
	step->var_info.num_dimensions = i + 2;
/*
* Now call grid code to get coordinates
*/
/*
* Now build single array of GribRecordInqRecList*'s
*/
	if( step->var_info.num_dimensions - 2 > 0) {
		total = 1;
		for(i = 0; i < step->var_info.num_dimensions - 2; i++) {
			total *= step->var_info.dim_sizes[i];
		}
		strt = (GribRecordInqRecList*)NclMalloc((unsigned)sizeof(GribRecordInqRecList)*total);
		for (i = 0; i < total; i++) {
			strt[i].rec_inq = (GribRecordInqRec*)10;
			strt[i].next = NULL;
		}
		the_end = header.next;
		i = 0;
		while(the_end != NULL) {
			ftstep = the_end->thelist;
			j = 0;
			while(ftstep != NULL) {
				rstep = ftstep->thelist;
	/* 
	* if either tmp_ft_vals or tmp_lv_vals is null then their values are a don't care
	* this mostly happens with tmp_lv_vals though
	*/
				if((tmp_ft_vals == NULL)||(ftstep->ft == tmp_ft_vals[j])){
					k = 0;
					if(!step->levels_has_two) {
						while(rstep != NULL) {
							if((tmp_lv_vals == NULL) ||(rstep->rec_inq->level0 == tmp_lv_vals[k])) {
								strt[i].rec_inq = rstep->rec_inq;	
								free_rec = rstep;
								rstep = rstep->next;
								NclFree(free_rec);
								k++;
							} else {
								strt[i].rec_inq = NULL;
								fprintf(stdout,"%s is missing ft: %d lv: %d\n",name,tmp_ft_vals[j],tmp_lv_vals[k]);
								k++;
							}
							i++;
						}
						if((rstep == NULL)&&(k < n_tmp_lv_vals)) {
							for( ;k < n_tmp_lv_vals; k++) {
								strt[i].rec_inq = NULL;
								fprintf(stdout,"%s is missing ft: %d lv: %d\n",name,tmp_ft_vals[j],tmp_lv_vals[k]);
								i++;
							}
						}
					} else {
						while(rstep != NULL) {
							if((rstep->rec_inq->level0 == tmp_lv_vals[k])&&(rstep->rec_inq->level1 == tmp_lv_vals1[k])) {
								strt[i].rec_inq = rstep->rec_inq;	
								free_rec = rstep;
								rstep = rstep->next;
								NclFree(free_rec);
								k++;
							} else {
								strt[i].rec_inq = NULL;
								fprintf(stdout,"%s is missing ft: %d lv: (%d,%d)\n",name,tmp_ft_vals[j],tmp_lv_vals[k],tmp_lv_vals1[k]);
								k++;
							}
							i++;
						}
						if((rstep == NULL)&&(k < n_tmp_lv_vals)) {
							for( ;k < n_tmp_lv_vals; k++) {
								strt[i].rec_inq = NULL;
								fprintf(stdout,"%s is missing ft: %d lv: (%d,%d)\n",name,tmp_ft_vals[j],tmp_lv_vals[k],tmp_lv_vals1[k]);
								i++;
							}
						}
					}
					free_ft = ftstep;
					ftstep = ftstep->next;
					if(free_ft->lv_vals != NULL) 
						NclFree(free_ft->lv_vals);
					if(free_ft->lv_vals1 != NULL) 
						NclFree(free_ft->lv_vals1);
					NclFree(free_ft);
					j++;
				} else {
					if(!step->levels_has_two) {
						for( k = 0 /* i already set */; k < n_tmp_lv_vals; i++,k++) {
							strt[i].rec_inq = NULL;
							fprintf(stdout,"%s is missing ft: %d lv: %d\n",name,tmp_ft_vals[j],tmp_lv_vals[k]);
						}
						j++;
					} else {
						for( k = 0 /* i already set */; k < n_tmp_lv_vals; i++,k++) {
							strt[i].rec_inq = NULL;
							fprintf(stdout,"%s is missing ft: %d lv: (%d,%d)\n",name,tmp_ft_vals[j],tmp_lv_vals[k],tmp_lv_vals1[k]);
						}
						j++;
					}
				}
			}
/*
			if(i != total) {
				fprintf(stdout,"HELLO (%d,%d)?\n",i,total);
			}
*/
			
			free_it = the_end;
			the_end = the_end->next;
			if(free_it->lv_vals != NULL) 
				NclFree(free_it->lv_vals);
			if(free_it->lv_vals1 != NULL) 
				NclFree(free_it->lv_vals1);
			if(free_it->ft_vals != NULL) 
				NclFree(free_it->ft_vals);
			NclFree(free_it);
		
		}
		step->thelist = strt;
		step->n_entries = total;
	} else {
		if(header.next != NULL) {
			free_it = header.next;
			if(free_it->lv_vals != NULL) 
				NclFree(free_it->lv_vals);
			if(free_it->lv_vals1 != NULL) 
				NclFree(free_it->lv_vals1);
			if(free_it->ft_vals != NULL) 
				NclFree(free_it->ft_vals);
			if(free_it->thelist != NULL) {
				if(free_it->thelist->lv_vals != NULL)
					NclFree(free_it->thelist->lv_vals);
				if(free_it->thelist->lv_vals1 != NULL)
					NclFree(free_it->thelist->lv_vals1);
				NclFree(free_it->thelist);
			}
			NclFree(free_it);
		}
	}
	

	

	return(returnval);
}


unsigned int UnsignedCnvtToDecimal
#if	NhlNeedProto
(int n_bytes,unsigned char *val)
#else
(n_bytes,val)
int n_bytes;
unsigned char *val;
#endif
{
	CVT tmp;
	int i = 0;
	
	tmp.c[0] = (char)0;
	tmp.c[1] = (char)0;
	tmp.c[2] = (char)0;
	tmp.c[3] = (char)0;
#ifndef ByteSwapped
	if(n_bytes == 4) {
		tmp.c[0] = val[i];
		i++;
	}
	if(n_bytes >= 3) {
		tmp.c[1] = val[i];
		i++;
	}
	if(n_bytes >= 2) {
		tmp.c[2] = val[i];
		i++;
	}
	if(n_bytes >= 1) {
		tmp.c[3] = val[i];
		i++;
	}
#else
	if(n_bytes == 4) {
		tmp.c[3] = val[i];
		i++;
	}
	if(n_bytes >= 3) {
		tmp.c[2] = val[i];
		i++;
	}
	if(n_bytes >= 2) {
		tmp.c[1] = val[i];
		i++;
	}
	if(n_bytes >= 1) {
		tmp.c[0] = val[i];
		i++;
	}
#endif
	return(tmp.value);
}
int CnvtToDecimal
#if	NhlNeedProto
(int n_bytes,unsigned char *val)
#else
(n_bytes,val)
int n_bytes;
unsigned char *val;
#endif
{
	CVT tmp;
	int i = 0;
	
	tmp.c[0] = (char)0;
	tmp.c[1] = (char)0;
	tmp.c[2] = (char)0;
	tmp.c[3] = (char)0;

#ifndef ByteSwapped
	if(n_bytes == 4) {
		tmp.c[0] = val[i];
		i++;
	}
	if(n_bytes >= 3) {
		tmp.c[1] = val[i];
		i++;
	}
	if(n_bytes >= 2) {
		tmp.c[2] = val[i];
		i++;
	}
	if(n_bytes >= 1) {
		tmp.c[3] = val[i];
		i++;
	}
#else
	if(n_bytes == 4) {
		tmp.c[3] = val[i];
		i++;
	}
	if(n_bytes >= 3) {
		tmp.c[2] = val[i];
		i++;
	}
	if(n_bytes >= 2) {
		tmp.c[1] = val[i];
		i++;
	}
	if(n_bytes >= 1) {
		tmp.c[0] = val[i];
		i++;
	}
#endif
	return(tmp.ivalue);
}

int GetNextGribOffset
#if NhlNeedProto
(FILE *gribfile, unsigned int *offset, unsigned int *totalsize, unsigned int startoff, unsigned int *nextoff,int* version)
#else
(gribfile, offset, totalsize, startoff, nextoff,version)
FILE* gribfile;
unsigned int *offset;
unsigned int *totalsize;
unsigned int startoff;
unsigned int *nextoff;
int *version;
#endif
{
	int i,ret1,ret2,ret3,ret4;
	char test[10];
	unsigned char nd[10];
	unsigned char is[10];
	unsigned int size;
	unsigned int t;


	ret1 = 0;
	ret2 = 0;
	ret3 = 0;
	ret4 = 0;

	test[4] = '\0';

	i = startoff;
	fseek(gribfile,i,SEEK_SET);
	while(1) {
		ret1 = fread((void*)is,1,1,gribfile);
		i += ret1;
		if(ret1 > 0) {
			if(is[0] == 'G') {
/*
				fprintf(stdout,"found G\n");
*/
				ret2 = fread((void*)&(is[1]),1,7,gribfile);
				if(ret2 < 7) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Premature end-of-file, file appears to be truncated");
					*totalsize = 0;
					return(GRIBEOF);
				} 
				i += ret2;
				test[0] = is[0];
				test[1] = is[1];
				test[2] = is[2];
				test[3] = is[3];
				*version = UnsignedCnvtToDecimal(1,&(is[7]));
				test[4] = '\0';
/*
				fprintf(stdout,"%d\t%c%c%c%c\n",ret2,is[0],is[1],is[2],is[3]);
*/
				if((!strncmp(test,"GRIB",4))&&(*version ==1)){
/*
					fprintf(stdout,"found GRIB\n");
*/
					*offset =  i - (ret1 + ret2);
					size = UnsignedCnvtToDecimal(3,&(is[4]));
					fseek(gribfile,i+size - (ret1 + ret2) - 4,SEEK_SET);
					ret3 = i+size - (ret1 + ret2) - 4;
					ret4 = fread((void*)nd,1,4,gribfile);
					if(ret4 < 4) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Premature end-of-file, file appears to be truncated");
						*totalsize = 0;
						return(GRIBEOF);
					}	 
					test[0] = nd[0];
					test[1] = nd[1];
					test[2] = nd[2];
					test[3] = nd[3];
					test[4] = '\0';
					if(!strncmp("7777",test,4)) {
/*
					fprintf(stdout,"found 7777\n");
*/
						*nextoff = ret3 + ret4;
						*totalsize = size;
						return(GRIBOK);
					} else {
						*nextoff = i;
						*totalsize = size;
						return(GRIBERROR);
					}
				} else if((!strncmp(test,"GRIB",4))&&(*version ==0)) {
					*offset = i - (ret1 + ret2);
/*
* This gives me the pds size
*/
					size = 4;
					while(1) {
						t = *offset + size;
						fseek(gribfile,t,SEEK_SET);
						ret4 = fread((void*)nd,1,4,gribfile);
						if(ret4 < 4) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Premature end-of-file, file appears to be truncated");
							break;
						}
						test[0] = nd[0];
						test[1] = nd[1];
						test[2] = nd[2];
						test[3] = nd[3];
						test[4] = '\0';
						if(!strncmp("7777",test,4)) {
							size += ret4;
							*nextoff = *offset + size ;
							*totalsize = size;
							return(GRIBOK);
						} else {
							size += UnsignedCnvtToDecimal(3,nd);
						}
						
					}
					*totalsize = size;
					return(GRIBEOF);
				} else if(!strncmp(test,"GRIB",4)){
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Unknown version number (%d),can't read this data",*version);
					return(GRIBEOF);
				}
			}
		} else {
			*totalsize = 0;
			return(GRIBEOF);
		}
	}
}

static int _GetLevels
#if NhlNeedProto
(int *l0,int *l1,int indicator,unsigned char* lv)
#else
(l0,l1,indicator,lv)
int *l0;
int *l1;
int indicator;
unsigned char *lv;
#endif
{
	if(indicator  < 100) {
		*l0 = -1;
		*l1 = -1;
	}
	switch(indicator) {
	case 100:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		return(1);
	case 101:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 102:
		*l0 = -1;
		*l1 = -1;
		return(1);
	case 103:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 104:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 105:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 106:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 107:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		*l1 = -1;
		break;
	case 108:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 109:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 110:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 111:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 112:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 113:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 114:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 115:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 116:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 117:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 119:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 120:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 121:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 125:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 128:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 141:
		*l0 = (int)lv[0];
		*l1 = (int)lv[1];
		break;
	case 160:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 200:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 201:
		*l0 = CnvtToDecimal(2,lv);
		*l1 = -1;
		break;
	case 1:
		*l0 = -1;
		*l1 = -1;
		return(1);
	default: 
		*l0 = -1;
		*l1 = -1;
	}
	return(0);
}


static int _GetTimeOffset 
#if	NhlNeedProto
( int time_indicator, unsigned char *offset)
#else
( time_indicator, offset)
int time_indicator;
unsigned char *offset;
#endif
{
	switch(time_indicator) {
		case 0: /* reference time + P1 */
		case 1: /* reference time + P1 */
			return((int)offset[0]);
		case 2: /* reference time + P1 < t < reference time + P2 */
		case 3: /* Average from reference time + P1 to reference time + P2 */
		case 4: /* Accumulation from reference time + P1 to reference time + P2 */
		case 5: /* Difference from reference time + P1 to reference time + P2 */
			return((int)offset[1]);
			break;
		case 10:/* P1 occupies both bytes */
			return(UnsignedCnvtToDecimal(2,offset));
			break;
		case 51:
		case 113:
		case 114:
		case 115:
		case 116:
		case 117:
		case 118:
		case 123:
		case 124:
		default:
			NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown or unsupported time range indicator detected, continuing");
			return(-1);
	}
}
static int level_comp
#if	NhlNeedProto
(Const void *s1, Const void *s2)
#else
(s1, s2)
void *s1;
void *s2;
#endif
{
	GribRecordInqRecList *s_1 = *(GribRecordInqRecList**)s1;
	GribRecordInqRecList *s_2 = *(GribRecordInqRecList**)s2;

	if((s_1->rec_inq->level0 != -1)&&(s_1->rec_inq->level1 != -1)) {
		if(s_1->rec_inq->level0 == s_2->rec_inq->level0) {
			if(s_1->rec_inq->level1 == s_2->rec_inq->level1) {
				return(s_1->rec_inq->start - s_2->rec_inq->start);
			} else {
				return(s_1->rec_inq->level1 - s_2->rec_inq->level1);
			}
		} else {
			return(s_1->rec_inq->level0 - s_2->rec_inq->level0);
		}
	} else {
		if(s_1->rec_inq->level0 == s_2->rec_inq->level0) {
			return(s_1->rec_inq->start- s_2->rec_inq->start);
		} else {
			return(s_1->rec_inq->level0 - s_2->rec_inq->level0);
		}
	} 
}
static int date_comp
#if 	NhlNeedProto
(Const void *s1, Const void *s2)
#else
(s1, s2)
void *s1;
void *s2;
#endif
{
	GribRecordInqRecList *s_1 = *(GribRecordInqRecList**)s1;
	GribRecordInqRecList *s_2 = *(GribRecordInqRecList**)s2;
	int d1,m1,year1,d2,m2,year2;
	short result = 0;

	result = s_1->rec_inq->initial_time.year - s_2->rec_inq->initial_time.year;
	if(!result) {
		result = s_1->rec_inq->initial_time.days_from_jan1 - s_2->rec_inq->initial_time.days_from_jan1;
		if(!result) {
			result = s_1->rec_inq->initial_time.minute_of_day - s_2->rec_inq->initial_time.minute_of_day;
			if(!result) {
				result = s_1->rec_inq->time_offset- s_2->rec_inq->time_offset;
			}
		}
		
	} 
	return(result);
}

static GribParamList *_NewListNode
#if	NhlNeedProto
(GribRecordInqRec *grib_rec)
#else
(grib_rec)
	GribRecordInqRec* grib_rec;
#endif
{
	GribParamList *tmp = NULL;
	GribRecordInqRecList *list = NULL;

	tmp = (GribParamList*)NclMalloc((unsigned)sizeof(GribParamList));
	tmp->next = NULL;
	list = (GribRecordInqRecList*) NclMalloc((unsigned)sizeof(GribRecordInqRecList));
	list->rec_inq = grib_rec;
	list->next = NULL;
	tmp->thelist = list;
	tmp->var_info.var_name_quark = grib_rec->var_name_q;
	tmp->var_info.data_type = GribMapToNcl((void*)&(grib_rec->int_or_float));
	tmp->param_number = grib_rec->param_number;
	tmp->grid_number = grib_rec->grid_number;
/*
	tmp->grid_number = 255;
*/
	tmp->grid_tbl_index = grib_rec->grid_tbl_index;
	tmp->grid_gds_tbl_index = grib_rec->grid_gds_tbl_index;
	tmp->has_gds= grib_rec->has_gds;
	tmp->gds_type = grib_rec->gds_type;
	tmp->level_indicator = grib_rec->level_indicator;
	tmp->n_entries = 1;
	tmp->minimum_it = grib_rec->initial_time;
	tmp->time_range_indicator = (int)grib_rec->pds[20];
	tmp->time_unit_indicator = (int)grib_rec->pds[17];
	tmp->levels = NULL;
	tmp->levels0 = NULL;
	tmp->levels1 = NULL;
	tmp->levels_has_two = 0;
	tmp->yymmddhh = NULL;
	tmp->forecast_time = NULL;
	tmp->n_atts = 0;
	tmp->theatts = NULL;
	return(tmp);
}

static void _InsertNodeAfter
#if NhlNeedProto
(GribParamList *node, GribParamList *new_node)
#else
(node, new_node)
GribParamList *node; 
GribParamList *new_node;
#endif
{
	GribParamList * tmp;

	tmp = node->next;
	node->next = new_node;
	new_node->next = tmp;
	return;
}

static GribRecordInqRec* _MakeMissingRec
#if NhlNeedProto
(void)
#else
()
#endif
{
	GribRecordInqRec* grib_rec = (GribRecordInqRec*)NclMalloc(sizeof(GribRecordInqRec));

	grib_rec->var_name_q = -1;
	grib_rec->param_number = -1;
	grib_rec->param_tbl_index = -1;
	grib_rec->grid_number = -1;
	grib_rec->grid_tbl_index = -1;
	grib_rec->grid_gds_tbl_index = -1;
	grib_rec->time_offset = -1;
	grib_rec->level0 = -1;
	grib_rec->level1 = -1;
	grib_rec->var_name = NULL;
	grib_rec->long_name_q = -1;
	grib_rec->units_q = -1;
	grib_rec->start = 0;
	grib_rec->bds_off= 0;
	grib_rec->gds = NULL;
	grib_rec->bds_size = 0;
	grib_rec->has_gds= 0;
	grib_rec->gds_off= 0;
	grib_rec->gds_size= 0;
	grib_rec->has_bms= 0;
	grib_rec->bms_off= 0;
	grib_rec->bms_size = 0;
	grib_rec->the_dat = NULL;
	return(grib_rec);
	
}

static void _AddRecordToNode
#if NhlNeedProto
(GribParamList *node, GribRecordInqRec* grib_rec)
#else
(node, grib_rec)
GribParamList *node;
GribRecordInqRec* grib_rec;
#endif
{
	GribRecordInqRecList * grib_rec_list = (GribRecordInqRecList*)NclMalloc((unsigned)sizeof(GribRecordInqRecList));

	

	if((grib_rec->initial_time.year < node->minimum_it.year)

		||((grib_rec->initial_time.year == node->minimum_it.year)	
			&&(grib_rec->initial_time.days_from_jan1 < node->minimum_it.days_from_jan1))

		||((grib_rec->initial_time.year == node->minimum_it.year)
			&&(grib_rec->initial_time.days_from_jan1 == node->minimum_it.days_from_jan1)
			&&(grib_rec->initial_time.minute_of_day < node->minimum_it.minute_of_day))) {

		node->minimum_it = grib_rec->initial_time;

	}
	if(node->time_unit_indicator != (int)grib_rec->pds[17]) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Time unit indicator varies for parameter (%s), continuing anyways.",NrmQuarkToString(grib_rec->var_name_q));
	}
	grib_rec_list->rec_inq = grib_rec;
	grib_rec_list->next = node->thelist;
	node->thelist = grib_rec_list;
	node->n_entries++;
}

static int _IsDef
#if NhlNeedProto
(GribFileRecord *therec, int param_num)
#else
(therec, param_num)
GribFileRecord *therec;
int param_num;
#endif
{
	GribParamList *step = therec->var_list;
	while(step != NULL) {
		if(step->param_number == param_num) return(1);
		step = step->next;
		
	}
	return(0);
}

static int _FirstCheck
#if NhlNeedProto
(GribFileRecord *therec, GribRecordInqRec *grib_rec)
#else
(therec, grib_rec)
GribFileRecord *therec;
GribRecordInqRec *grib_rec;
#endif
{
	GribParamList *step = therec->var_list;

	if((step->param_number > grib_rec->param_number) 
		||((step->param_number == grib_rec->param_number)&&(step->grid_number > grib_rec->grid_number))
		||((step->param_number == grib_rec->param_number)&&(step->grid_number == grib_rec->grid_number)&&(step->time_range_indicator > (int)grib_rec->pds[20]))
		||((step->param_number == grib_rec->param_number)&&(step->grid_number == grib_rec->grid_number)&&(step->time_range_indicator == (int)grib_rec->pds[20])&&(step->level_indicator > grib_rec->level_indicator))) {

		therec->var_list = _NewListNode(grib_rec);
		therec->var_list->next = step;
		therec->n_vars++;
		return(1);
	} else if((step->param_number == grib_rec->param_number)&&(step->grid_number == grib_rec->grid_number)&&(step->time_range_indicator == (int)grib_rec->pds[20])&&(step->level_indicator == grib_rec->level_indicator)){
		_AddRecordToNode(step,grib_rec);
		return(1);
	} else {
		return(0);
	}
}

static void *GribGetFileRec
#if	NhlNeedProto
(NclQuark path,int wr_status)
#else
(path,wr_status)
NclQuark path;
int wr_status;
#endif
{
	FILE* fd;
	int done = 0;
	unsigned int offset = 0;
	unsigned int size = 0;
	unsigned int nextoff = 0;
	GribFileRecord *therec = NULL;
	GribRecordInqRec *grib_rec = NULL;
	GribRecordInqRecList *grib_rec_list = NULL;
	GribParamList *step = NULL,*step2 = NULL, *tmpstep = NULL ;
	int i,j,k,l;
	int ret;
	int toff;
	unsigned char tmpc[4];
	unsigned char buffer[80];
	GribRecordInqRecList **sortar,**ptr,**start_ptr;
	TBLE2 *name_rec = NULL;
	TBLE2 *tmp_name_rec = NULL;
	int tmp_month;
	int tmp_year;
	int tmp_century;
	int tmp_day;
	int tmp_hour;
	int tmp_minute;
	int version;
	NhlErrorTypes retvalue;

	if(wr_status <= 0) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Grib files are read only continueing but opening file as read only");
	}
	fd = fopen(NrmQuarkToString(path),"r");
	vbuf = (void*)NclMalloc(4*getpagesize());
	setvbuf(fd,vbuf,_IOFBF,4*getpagesize());
	
	if(fd != NULL) {
		while(!done) {
			ret = GetNextGribOffset(fd,&offset,&size,offset,&nextoff,&version);
			if(ret == GRIBEOF) {
				done = 1;
			} 
			if((ret != GRIBERROR)&&(size != 0)) {
				grib_rec = NclMalloc((unsigned)sizeof(GribRecordInqRec));
				grib_rec->gds = NULL;
				grib_rec->the_dat = NULL;
				grib_rec->version = version;
				grib_rec->var_name = NULL;
				fseek(fd,offset+(version?8:4),SEEK_SET);
				fread((void*)grib_rec->pds,1,version?28:24,fd);
/*
				fprintf(stdout,"Found: %d\n",(int)(int)grib_rec->pds[8]);
*/
				grib_rec->the_dat = NULL;
				grib_rec->has_gds = (grib_rec->pds[7] & (char)0200) ? 1 : 0;
				grib_rec->has_bms = (grib_rec->pds[7] & (char)0100) ? 1 : 0;
				grib_rec->param_number = (int)grib_rec->pds[8];
				grib_rec->grid_number = (int)grib_rec->pds[6];
/*
				grib_rec->grid_number = 255;
*/
			
/*
				if((grib_rec->has_gds) && (grib_rec->grid_number != 255)) {
					fprintf(stdout,"Found one: %d\n",grib_rec->grid_number);
				} 
				if(grib_rec->has_bms) {
					fprintf(stdout,"Found one with bms (%d,%d)\n",grib_rec->param_number,grib_rec->grid_number);
				}
*/
				grib_rec->start = offset;

				if(version) {
					grib_rec->initial_time.year = (short)(((short)grib_rec->pds[24] - 1 )*100 + (short)(int)grib_rec->pds[12]);
					grib_rec->initial_time.days_from_jan1 = JulianDayDiff(1,1,grib_rec->initial_time.year,(int)grib_rec->pds[14],(int)grib_rec->pds[13],grib_rec->initial_time.year);
					grib_rec->initial_time.minute_of_day = (short)grib_rec->pds[15] * 60 + (short)grib_rec->pds[16];
				} else {
					grib_rec->initial_time.year = (short)(1900 + (short)(int)grib_rec->pds[12]);
					grib_rec->initial_time.days_from_jan1 = JulianDayDiff(1,1,grib_rec->initial_time.year,(int)grib_rec->pds[14],(int)grib_rec->pds[13],grib_rec->initial_time.year);
					grib_rec->initial_time.minute_of_day = (short)grib_rec->pds[15] * 60 + (short)grib_rec->pds[16];
				}
				if(grib_rec->version != 0) {
					if(((int)grib_rec->pds[24] < 1)|| ((int)grib_rec->pds[15] > 24)||((int)grib_rec->pds[16] > 60)||((int)grib_rec->pds[13] > 12)||((int)grib_rec->pds[12] > 100)){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Corrupted record found. Time values out of appropriate ranges, skipping record");
						NhlFree(grib_rec);
						grib_rec = NULL;

					}
				} else {
					if(((int)grib_rec->pds[15] > 24)||((int)grib_rec->pds[16] > 60)||((int)grib_rec->pds[13] > 12)||((int)grib_rec->pds[12] > 100)){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Corrupted record found. Time values out of appropriate ranges, skipping record");
						NhlFree(grib_rec);
						grib_rec = NULL;

					}
				}

			if(grib_rec != NULL) {
				grib_rec->time_offset = 0;
				grib_rec->pds_size = CnvtToDecimal(3,&(grib_rec->pds[0]));
				if(grib_rec->has_gds) {
					fseek(fd,(unsigned)(grib_rec->start + (version?8:4) + grib_rec->pds_size),SEEK_SET);
					fread((void*)buffer,1,6,fd);
					grib_rec->gds_size = CnvtToDecimal(3,buffer);
					grib_rec->gds_off = (version?8:4) + grib_rec->pds_size;
					grib_rec->gds_type = (int)buffer[5];
/*
					fprintf(stdout,"%d\n",grib_rec->gds_type);
*/
					grib_rec->gds = (unsigned char*)NclMalloc((unsigned)sizeof(char)*grib_rec->gds_size);
					fseek(fd,(unsigned)(grib_rec->start + (version?8:4) + grib_rec->pds_size),SEEK_SET);
					fread((void*)grib_rec->gds,1,grib_rec->gds_size,fd);
				} else {
					grib_rec->gds_off = 0;	
					grib_rec->gds_size = 0;
					grib_rec->gds_type = -1;
					grib_rec->gds = NULL;
				}
				if((grib_rec->has_gds) && (grib_rec->grid_number == 255)) {
					for(i = 0; i < grid_gds_tbl_len ; i++) {
						if(grib_rec->gds_type == grid_gds_index[i]) { 
							grib_rec->grid_gds_tbl_index = i;
							break;
						}
					}
					if(i == grid_gds_tbl_len) {
						grib_rec->grid_gds_tbl_index = -1;
					}
					grib_rec->grid_tbl_index = -1;
				} else {
					for(i = 0; i < grid_tbl_len ; i++) {
						if(grib_rec->grid_number == grid_index[i]) { 
							grib_rec->grid_tbl_index = i;
							break;
						}
					}
					if(i == grid_tbl_len) {
						grib_rec->grid_tbl_index = -1;
						if(grib_rec->has_gds) {
							for(i = 0; i < grid_gds_tbl_len ; i++) {
								if(grib_rec->gds_type == grid_gds_index[i]) { 
									grib_rec->grid_gds_tbl_index = i;
									break;
								}
							}
							if(i == grid_gds_tbl_len) {
								grib_rec->grid_gds_tbl_index = -1;
							}
						}
					} else {
						grib_rec->grid_gds_tbl_index = -1;
					}
				}

				if(grib_rec->has_bms) {
					fseek(fd,(unsigned)(grib_rec->start + (version?8:4) + grib_rec->pds_size + grib_rec->gds_size),SEEK_SET);
					fread((void*)tmpc,1,3,fd);
					grib_rec->bms_size = CnvtToDecimal(3,tmpc);
					grib_rec->bms_off = (version?8:4) + grib_rec->pds_size + grib_rec->gds_size;
				} else {
					grib_rec->bms_off = 0;
					grib_rec->bms_size = 0;
				}
				grib_rec->bds_off = (version ? 8:4) + grib_rec->pds_size + grib_rec->bms_size + grib_rec->gds_size;
				fseek(fd,(unsigned)(grib_rec->start + grib_rec->bds_off),SEEK_SET);
				fread((void*)tmpc,1,4,fd);
				grib_rec->bds_flags = tmpc[3];
				grib_rec->bds_size = CnvtToDecimal(3,tmpc);
				fread((void*)tmpc,1,1,fd);
				grib_rec->int_or_float = (int)(tmpc[0]  & (char)0040) ? 1 : 0;
			}

		
				name_rec = NULL;	
				if((grib_rec != NULL) &&(grib_rec->param_number < 128)) {	
					for(i = 0; i < sizeof(params_index)/sizeof(int); i++) {
						if(params_index[i] == grib_rec->param_number) {
							name_rec = &(params[i]);
							break;
						}
					}
					if( i ==  sizeof(params_index)/sizeof(int) ) {
						if(!_IsDef(therec,grib_rec->param_number)) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), using default variable name (VAR_%d)",grib_rec->param_number,grib_rec->param_number);
						}
						i = -1;
					} 
				} else if(grib_rec != NULL) {
					switch((int)grib_rec->pds[4]) {
					case 98: 
						for(i = 0; i < sizeof(params_ecmwf_index)/sizeof(int); i++) {
							if(params_ecmwf_index[i] == grib_rec->param_number) {
								name_rec = &(params_ecmwf[i]);
								break;
							}
						}
						if( i ==  sizeof(params_ecmwf_index)/sizeof(int) ) {
							if(!_IsDef(therec,grib_rec->param_number)) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), using default variable name (VAR_%d)",grib_rec->param_number,grib_rec->param_number);
							}
							i = -1;
						} 
						break;
					 case 59:
						for(i = 0; i < sizeof(params_fsl_index)/sizeof(int); i++) {
							if(params_fsl_index[i] == grib_rec->param_number) {
								name_rec = &(params_fsl[i]);
								break;
							}
						}
						if( i ==  sizeof(params_fsl_index)/sizeof(int) ) {
							if(!_IsDef(therec,grib_rec->param_number)) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), using default variable name (VAR_%d)",grib_rec->param_number,grib_rec->param_number);
							}
							i = -1;
						} 
						
						break;
					case 7:
					case 8:
					case 9:
						for(i = 0; i < sizeof(params_nwsnmc_index)/sizeof(int); i++) {
							if(params_nwsnmc_index[i] == grib_rec->param_number) {
								name_rec = &(params_nwsnmc[i]);
								break;
							}
						}
						if( i ==  sizeof(params_nwsnmc_index)/sizeof(int) ) {
							if(!_IsDef(therec,grib_rec->param_number)) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unknown grib parameter number detected (%d), using default variable name (VAR_%d)",grib_rec->param_number);
							}
							i = -1;
						} 
						break;
					default:
						NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Unlocatable parameter number (%d) from center (%d), using default variable name (VAR_%d)",grib_rec->param_number,(int)grib_rec->pds[4],grib_rec->param_number);
					}
				}
				if((name_rec == NULL)&&(grib_rec!=NULL)) {
					tmp_name_rec = NclMalloc(sizeof(TBLE2));
					tmp_name_rec->abrev = NclMalloc(strlen("VAR_") + 4);
					tmp_name_rec->num = NULL;
					sprintf(tmp_name_rec->abrev,"VAR_%d",grib_rec->param_number);
					tmp_name_rec->long_name = NclMalloc(strlen("Unknown Variable Name") + 1);
					sprintf(tmp_name_rec->long_name,"Unknown Variable Name");
					tmp_name_rec->units= NclMalloc(strlen("unknown") + 1);
					sprintf(tmp_name_rec->units,"unknown");
					name_rec = tmp_name_rec;
				}

				if((name_rec != NULL)&&(grib_rec != NULL)){
					grib_rec->param_tbl_index = i;
					strcpy((char*)buffer,name_rec->abrev);
					if((grib_rec->has_gds)&&(grib_rec->grid_number == 255)) {
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_GDS%d",grib_rec->gds_type);
					} else {
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_%d",grib_rec->grid_number);
					}
					for(i = 0; i < sizeof(level_index)/sizeof(int); i++) {
						if(level_index[i] == (int)grib_rec->pds[9]) { 
							break;
						}
					}
					if(i < sizeof(level_index)/sizeof(int)) {
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_%s",level_str[i]);
					} else {
						if(((int)grib_rec->pds[9]) != 0) {
							sprintf((char*)&(buffer[strlen((char*)buffer)]),"_%d",(int)grib_rec->pds[9]);
						}
					}
					switch((int)grib_rec->pds[20]) {
					case 0:
					case 1:
					case 2:
						break;	
					case 3:
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_ave");
						break;
					case 4:
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_acc");
						break;
					case 5:
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_dif");
						break;
					default:
						sprintf((char*)&(buffer[strlen((char*)buffer)]),"_%d",(int)grib_rec->pds[20]);
						break;
					}
					grib_rec->var_name = (char*)NclMalloc((unsigned)strlen((char*)buffer) + 1);
					strcpy(grib_rec->var_name,(char*)buffer);
					grib_rec->var_name_q = NrmStringToQuark(grib_rec->var_name);
					grib_rec->long_name_q = NrmStringToQuark(name_rec->long_name);
					grib_rec->units_q = NrmStringToQuark(name_rec->units);
					grib_rec->level_indicator = (int)grib_rec->pds[9];
					_GetLevels(&grib_rec->level0,&grib_rec->level1,(int)grib_rec->pds[9],&(grib_rec->pds[10]));
					if(therec == NULL) {
						therec = (GribFileRecord*)NclMalloc((unsigned)sizeof(GribFileRecord));
						therec->n_vars = 0;
						therec->var_list = NULL;
						therec->wr_status = wr_status;	
						therec->file_path_q = path;
						therec->internal_var_list = NULL;
						therec->n_internal_vars = 0;
					}
					if(therec->var_list == NULL) {
						therec->var_list = _NewListNode(grib_rec);
						therec->n_vars = 1;
				} else {
					step = therec->var_list;
					if(!_FirstCheck(therec,grib_rec)) {
/*
* Keep in inorder list
*/
						while((step->next != NULL)
							&&(step->next->param_number < grib_rec->param_number)) {

							step = step->next;
						}
			
						if((step->next == NULL) 
							||(step->next->param_number > grib_rec->param_number)) {
/*
* No current instance of grib_rec->param_number insert immediately
*/

							step2 = _NewListNode(grib_rec);
							_InsertNodeAfter(step,step2);
							therec->n_vars++;
						} else if(step->next->grid_number <= grib_rec->grid_number) {
/*
* At this point param_number found and step->next points to first occurance of param_number
*/
							while((step->next != NULL)
								&&(step->next->param_number==grib_rec->param_number)
								&&(step->next->grid_number < grib_rec->grid_number)) {
								step = step->next;
							}
							if((step->next == NULL)
								||(step->next->param_number != grib_rec->param_number)
								||(step->next->grid_number > grib_rec->grid_number)) {

								step2 = _NewListNode(grib_rec);
								_InsertNodeAfter(step,step2);
								therec->n_vars++;
							} else if(step->next->time_range_indicator <= (int)grib_rec->pds[20]) {
/*
* At this point param_number and grib_number found and step->next points to first occurance of
* grid_number
*/
								while((step->next != NULL)
									&&(step->next->param_number == grib_rec->param_number)
									&&(step->next->grid_number == grib_rec->grid_number)
									&&(step->next->time_range_indicator < (int)grib_rec->pds[20])){
									step = step->next;
								}
								if((step->next == NULL)
									||(step->next->param_number != grib_rec->param_number)
									||(step->next->grid_number != grib_rec->grid_number)
									||(step->next->time_range_indicator > (int)grib_rec->pds[20])) {
									step2 = _NewListNode(grib_rec);
									_InsertNodeAfter(step,step2);
									therec->n_vars++;
								} else if(step->next->level_indicator <= grib_rec->level_indicator) {
									while((step->next != NULL) 
										&&(step->next->param_number == grib_rec->param_number)
										&&(step->next->grid_number == grib_rec->grid_number)
										&&(step->next->time_range_indicator == (int)grib_rec->pds[20])
										&&(step->next->level_indicator < grib_rec->level_indicator)){
										step = step->next;
									}
									if((step->next == NULL)
										||(step->next->param_number != grib_rec->param_number)
										||(step->next->grid_number != grib_rec->grid_number)
										||(step->next->time_range_indicator != (int)grib_rec->pds[20])
										||(step->next->level_indicator > grib_rec->level_indicator)) {
										step2 = _NewListNode(grib_rec);	
										_InsertNodeAfter(step,step2);
										therec->n_vars++;
									} else {
/*
* Att this point it falls through because 
* param_number, grid_number and time_range_indicator level_indicator are equal
* so its time ot add the record
*/
										_AddRecordToNode(step->next,grib_rec);
									}
								} else {
									step2 = _NewListNode(grib_rec);
									_InsertNodeAfter(step,step2);
									therec->n_vars++;
								}
							} else {
								step2 = _NewListNode(grib_rec);
								_InsertNodeAfter(step,step2);
								therec->n_vars++;
							}
						} else {
							step2 = _NewListNode(grib_rec);
							_InsertNodeAfter(step,step2);
							therec->n_vars++;
						}
					}
				}
			}
			if(tmp_name_rec != NULL) {
				NclFree(tmp_name_rec->abrev);	
				NclFree(tmp_name_rec->units);
				NclFree(tmp_name_rec->long_name);
				NclFree(tmp_name_rec);
				tmp_name_rec = NULL;
			}
		} else if(ret==GRIBERROR){
			NhlPError(NhlWARNING, NhlEUNKNOWN, "NclGRIB: Detected incomplete record, skipping record");
		}
		offset = nextoff;
		grib_rec = NULL;
	}
	if(therec != NULL ) {
		therec->grib_grid_cache = NULL;
/*
* Next step is to sort by time and then level each of the variables in the list
*/
		step = therec->var_list;
		k = 0;
		step2 = NULL;
		while(step != NULL) {
			grib_rec_list = step->thelist;


			sortar = (GribRecordInqRecList**)NclMalloc((unsigned)sizeof(GribRecordInqRecList*)*step->n_entries);
			i = 0;	
/*
* Scan through records and compute time offset from top of the grib record. 
* All offsets based time_units_indicator of top of the grib parameter record
* First determine an offset in time units based on time_units_indicator and time_range_indicator
* then determine offset in same units from the top of the parameter list's time reference 
*/		
		
			while(grib_rec_list != NULL) {
				sortar[i] = grib_rec_list;	
				grib_rec_list->rec_inq->time_offset = _GetTimeOffset(
							(int)grib_rec_list->rec_inq->pds[20],
							&(grib_rec_list->rec_inq->pds[18]));
				grib_rec_list = grib_rec_list->next;
				i++;
			}
			qsort((void*)sortar,i,sizeof(GribRecordInqRecList*),date_comp);

		
			j = 0;
			i = 0;
			l = 0;
			ptr = sortar;
			start_ptr = sortar;
			while(j < step->n_entries) {
				start_ptr = ptr;
				i = 0;
				while((j< step->n_entries)&&(date_comp((void*)&(ptr[i]),(void*)start_ptr))==0) {
					i++;
					j++;
				}
				qsort((void*)ptr,i,sizeof(GribRecordInqRecList*),level_comp);
/*
				if((*ptr)->rec_inq->level0 != -1) {
					for(k = 0; k < i-1; k++) {
						if(!level_comp((void*)&(ptr[k]),(void*)&(ptr[k+1]))) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"NclGRIB: Duplicate GRIB record found, skipping record");
							ptr[k] = ptr[k+1];
							l++;

						}
					}
				}
*/
				if(j < step->n_entries) {
					ptr = &(ptr[i]);
				}
			}







/* 
* This is temporary code to print out whats going on
*/	
			step->thelist = sortar[0];
			for(i = 0; i < step->n_entries - 1; i++) {
				sortar[i]->next = sortar[i+1];
			} 
			sortar[step->n_entries - 1]->next = NULL;
/*
* Next step is to determine dimensionality for the variable.
* Dimensionality is detemined [yy:mm:dd:hh:mm] x [forcast offset (P1)] x [levels] x [grid x] x [grid y]
* k is the variable number, step points to GribParamList, and sortar has all elments in order and 
* connected. Missing entrys will be inserted when it is determined that levels or forcast times are missing
*/
/*
* Need to reassign everything incase a Duplicate record was found
*/

/*
			fprintf(stdout,"param# = %d\t%d\t%s\n",step->param_number,step->grid_number,step->thelist->rec_inq->var_name);
*/


/*
* Determine grid and coordinate information as well as dimenionality foreach record
* Also fills in missing values
*/
			retvalue = _DetermineDimensionAndGridInfo(therec,step);
			if((retvalue < NhlNOERROR)&&(step2 == NULL)) {
				step = step->next;
				step2 = therec->var_list; 
				therec->var_list = step;
				therec->n_vars--;
				_GribFreeParamRec(step2);
				step2 = NULL;
				
			} else if(retvalue < NhlNOERROR) {
				tmpstep = step;
				step2->next = step->next;
				step = step->next;
				therec->n_vars--;
				_GribFreeParamRec(tmpstep);
			} else {
				step2 = step;	
				step = step->next;
				k++;
			}
			NclFree(sortar);
			sortar = NULL;
		}
/*
* Now its time to scan variables and detemine all dimensions in this file and combine 
* dimensions that are equal. The last two dimensions will always be the grid dimensions
* Variables can be two dimensions to five. The first three dimensions are 
* initial_time x forcast offset x levels. They'll always be in that order but
* each dimension could be 1 in which case it isn't a real dimension but an attribute
*/
			_SetFileDimsAndCoordVars(therec);
			_SetAttributeLists(therec);


			fclose(fd);	
			NclFree(vbuf);
			return(therec);
		} 
	}
	if(fd != NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Could not open (%s) check permissions",NrmQuarkToString(path));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Could not open (%s) no grib records found",NrmQuarkToString(path));
	}
	return(NULL);
}

static void *GribCreateFileRec
#if	NhlNeedProto
(NclQuark path)
#else
(path)
NclQuark path;
#endif
{
NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Grib files can only be read, Grib file can not be created using NCL");
return(NULL);
}

static void GribFreeFileRec
#if	NhlNeedProto
(void* therec)
#else
(therec)
void *therec;
#endif
{
	GribFileRecord *thefile = (GribFileRecord*)therec;
	GribParamList *vstep,*vstep1;
	GribRecordInqRecList *rstep;
	GribDimInqRecList *dim,*dim1;
	GribInternalVarList *ivars,*itmp;
	GribAttInqRecList *theatts,*tmp;
	int i;
	NclGribCacheList *thelist,*thelist0;
	NclGribCacheRec *ctmp,*ctmp0;

	vstep = thefile->var_list;
	while(vstep != NULL){
		vstep1 = vstep->next;
		_GribFreeParamRec(vstep);
		vstep  = vstep1;
	}
	thelist = thefile->grib_grid_cache;
        while(thelist != NULL) {
		ctmp = thelist->thelist;
		while(ctmp!=NULL) {	
			ctmp0 = ctmp;
			ctmp = ctmp->next;
			NclFree(ctmp0);
		}
		thelist0 = thelist;
		thelist = thelist->next;
		NclFree(thelist0);
	}

	ivars = thefile->internal_var_list;
	while(ivars != NULL) {
		_NclDestroyObj((NclObj)ivars->int_var->value);
		theatts = ivars->int_var->theatts;
		while(theatts != NULL) {
			_NclDestroyObj((NclObj)theatts->att_inq->thevalue);
			NclFree(theatts->att_inq);
			tmp = theatts;
			theatts = theatts->next;
			NclFree(tmp);
		}	
		NclFree(ivars->int_var);
		itmp = ivars;	
		ivars = ivars->next;
		NclFree(itmp);
	}
	dim = thefile->it_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	dim = thefile->ft_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	dim = thefile->lv_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	dim = thefile->grid_dims;
	if(dim != NULL) {
		while(dim != NULL) {
			dim1 = dim->next;
			if(dim->dim_inq != NULL) {
				if(dim->dim_inq->is_gds) {
					NclFree(dim->dim_inq->gds);
				}
				NclFree(dim->dim_inq);
			}
			NclFree(dim);
			dim = dim1;
		}
	}
	NclFree(therec);
}

static NclQuark* GribGetVarNames
#if	NhlNeedProto
(void* therec, int *num_vars)
#else
(therec, num_vars)
void* therec;
int *num_vars;
#endif
{
	GribFileRecord *thefile = (GribFileRecord*)therec;
	GribParamList *step;
	GribInternalVarList *vstep;
	int i;
	NclQuark *arout;

	*num_vars = thefile->n_vars + thefile->n_internal_vars;
	arout = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)* *num_vars);


	step = thefile->var_list;	
	for(i = 0; i < thefile->n_vars; i++) {
		arout[i] = step->var_info.var_name_quark;
		step = step->next;
	}

	vstep = thefile->internal_var_list;
	for(; i < thefile->n_vars + thefile->n_internal_vars; i++) {
		arout[i] = vstep->int_var->var_info.var_name_quark;
		vstep = vstep->next;
	}
	return(arout);
}

static NclFVarRec *GribGetVarInfo
#if	NhlNeedProto
(void *therec, NclQuark var_name)
#else
(therec, var_name)
void *therec;
NclQuark var_name;
#endif
{
GribFileRecord *thefile = (GribFileRecord*)therec;
GribParamList *step;
NclFVarRec *tmp;
GribInternalVarList *vstep;
int i;

vstep = thefile->internal_var_list;
while(vstep != NULL) {
	if(vstep->int_var->var_info.var_name_quark == var_name) {
		tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
		tmp->var_name_quark  = vstep->int_var->var_info.var_name_quark;
		tmp->data_type  = vstep->int_var->var_info.data_type;
		tmp->num_dimensions  = vstep->int_var->var_info.num_dimensions;
		for(i=0;i< tmp->num_dimensions;i++) {
			tmp->file_dim_num[i]  = vstep->int_var->var_info.file_dim_num[i];
		}
		return(tmp);
	} else {
		vstep = vstep->next;
	}
}	

step = thefile->var_list;	
while(step != NULL) {
	if(step->var_info.var_name_quark == var_name) {
		tmp = (NclFVarRec*)NclMalloc(sizeof(NclFVarRec));
		tmp->var_name_quark  = step->var_info.var_name_quark;
		tmp->data_type  = step->var_info.data_type;
		tmp->num_dimensions  = step->var_info.num_dimensions;
		for(i=0;i< tmp->num_dimensions;i++) {
			tmp->file_dim_num[i]  = step->var_info.file_dim_num[i];
		}
		return(tmp);
	} else {
		step = step->next;
	}
}
return(NULL);
}

static NclQuark *GribGetDimNames
#if	NhlNeedProto
(void* therec, int* num_dims)
#else
(therec,num_dims)
void *therec;
int *num_dims;
#endif
{
GribFileRecord *thefile = (GribFileRecord*)therec;
GribDimInqRecList *dstep;
NclQuark *dims;
int i,j;

dims = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*thefile->total_dims);
i = 0;
*num_dims = thefile->total_dims;
dstep = thefile->it_dims;
for(j=0; j < thefile->n_it_dims; j++) {
	dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
	dstep = dstep->next;
}
dstep = thefile->ft_dims;
for(j=0; j < thefile->n_ft_dims; j++) {
	dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
	dstep = dstep->next;
}
dstep = thefile->lv_dims;
for(j=0; j < thefile->n_lv_dims; j++) {
	dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
	dstep = dstep->next;
}
dstep = thefile->grid_dims;
for(j=0; j < thefile->n_grid_dims; j++) {
	dims[dstep->dim_inq->dim_number] = dstep->dim_inq->dim_name;	
	dstep = dstep->next;
}

return(dims);
}

static NclFDimRec *GribGetDimInfo
#if	NhlNeedProto
(void* therec, NclQuark dim_name_q)
#else
(therec,dim_name_q)
void* therec;
NclQuark dim_name_q;
#endif
{
GribFileRecord *thefile = (GribFileRecord*)therec;
GribDimInqRecList *dstep;
NclFDimRec *tmpd = NULL;
char *tmp;

tmp = NrmQuarkToString(dim_name_q);
/*
* first character is either i,f, g or l
*/
	dstep = thefile->it_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return(tmpd);
		}
		dstep = dstep->next;
	}		
	dstep = thefile->ft_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return(tmpd);
		}
		dstep = dstep->next;
	}		
	dstep = thefile->grid_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return(tmpd);
		}
		dstep = dstep->next;
	}		
	dstep = thefile->lv_dims;
	while(dstep != NULL) {
		if(dstep->dim_inq->dim_name == dim_name_q) {
			tmpd = (NclFDimRec*)NclMalloc(sizeof(NclFDimRec));
			tmpd->dim_name_quark = dim_name_q;
			tmpd->dim_size = dstep->dim_inq->size;
			tmpd->is_unlimited = 0;
			return(tmpd);
		}
		dstep = dstep->next;
	}		
	return(NULL);
}

static void *GribReadVar
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
	GribFileRecord *rec = (GribFileRecord*)therec;
	GribParamList *step;
	GribRecordInqRec *current_rec;
	void *out_data;
	long *grid_start;
	long *grid_finish;
	long *grid_stride;
	int n_other_dims;
	int current_index[3];
	int dim_sizes[3] = {-1,-1,-1};
	int i;
	int offset;
	int done = 0,inc_done =0;
	int data_offset = 0;
	void *tmp;
	void *missing;
	NclScalar missingv;
	int int_or_float;
	FILE* fd;
	int grid_dim_sizes[2];
	int n_grid_dims;
	NclMultiDValData tmp_md;
	NclSelectionRecord  sel_ptr;
	GribInternalVarList *vstep;

	vstep = rec->internal_var_list;
	while(vstep != NULL ) {
		if(vstep->int_var->var_info.var_name_quark == thevar) {
			sel_ptr.n_entries = vstep->int_var->var_info.num_dimensions;
			out_data = storage;
			for(i = 0; i < vstep->int_var->var_info.num_dimensions; i++ ) {
				sel_ptr.selection[i].sel_type = Ncl_SUBSCR;
				sel_ptr.selection[i].dim_num = i;
				sel_ptr.selection[i].u.sub.start = start[i];
				sel_ptr.selection[i].u.sub.finish = finish[i];
				sel_ptr.selection[i].u.sub.stride = stride[i];
				sel_ptr.selection[i].u.sub.is_single = 0;
			}
			tmp_md = (NclMultiDValData)_NclReadSubSection((NclData)vstep->int_var->value,&sel_ptr,NULL);
			memcpy((void*)&((char*)out_data)[data_offset],tmp_md->multidval.val,tmp_md->multidval.totalsize);
			if(tmp_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)tmp_md);
			}
			return(out_data);
		}
		vstep = vstep->next;

	}





	step = rec->var_list;
	while(step != NULL) {
		if(step->var_info.var_name_quark == thevar) {
			fd = fopen(NrmQuarkToString(rec->file_path_q),"r");
			vbuf = (void*)NclMalloc(4*getpagesize());
			setvbuf(fd,vbuf,_IOFBF,4*getpagesize());

			out_data = storage;
			grid_start = &(start[(step->var_info.num_dimensions - 2) ]);
			grid_finish = &(finish[(step->var_info.num_dimensions - 2) ]);
			grid_stride = &(stride[(step->var_info.num_dimensions - 2) ]);
			n_other_dims = step->var_info.num_dimensions - 2;
			
			
			for(i = 0; i < n_other_dims; i++) {
				current_index[i] = start[i];
				dim_sizes[i] = step->var_info.dim_sizes[i];
			}
			n_grid_dims = 2;
			grid_dim_sizes[0] = step->var_info.dim_sizes[step->var_info.num_dimensions - 2];
			grid_dim_sizes[1] = step->var_info.dim_sizes[step->var_info.num_dimensions - 1];
			sel_ptr.n_entries = 2;
			sel_ptr.selection[0].sel_type = Ncl_SUBSCR;
			sel_ptr.selection[0].dim_num = 0;
			sel_ptr.selection[0].u.sub.start = grid_start[0];
			sel_ptr.selection[0].u.sub.finish = grid_finish[0];
			sel_ptr.selection[0].u.sub.stride = grid_stride[0];
			sel_ptr.selection[0].u.sub.is_single = 0;
			sel_ptr.selection[1].sel_type = Ncl_SUBSCR;
			sel_ptr.selection[1].dim_num = 1;
			sel_ptr.selection[1].u.sub.start = grid_start[1];
			sel_ptr.selection[1].u.sub.finish = grid_finish[1];
			sel_ptr.selection[1].u.sub.stride = grid_stride[1];
			sel_ptr.selection[1].u.sub.is_single = 0;
			

			offset = 0;
			while(!done) {
				offset = 0;
				if(n_other_dims > 0 ) {
					for(i = 0; i < n_other_dims - 1; i++) {
						offset += dim_sizes[i+1] * current_index[i];
					}
					offset += current_index[n_other_dims-1];
				}
				current_rec = step->thelist[offset].rec_inq;
	/*
	* For now(4/27/98) missing records persist, Eventually I'll implement one missing record per grid type for
	* general use.
	*/
				if(current_rec == NULL) {
					if(step->var_info.data_type == NCL_int) {
						tmp = NclMalloc(sizeof(int) * grid_dim_sizes[0] * grid_dim_sizes[1]);
						for( i = 0; i < grid_dim_sizes[0] * grid_dim_sizes[1]; i++){
							((int*)tmp)[i] = DEFAULT_MISSING_INT;
							
						}
						missingv.intval = DEFAULT_MISSING_INT;
						step->thelist[offset].rec_inq = _MakeMissingRec();
						current_rec = step->thelist[offset].rec_inq;
						current_rec->the_dat = _NclCreateVal(
									NULL,
									NULL,
									Ncl_MultiDValData,
									0,
									tmp,
									&missingv,
									n_grid_dims,
									grid_dim_sizes,
									PERMANENT,
									NULL,
									nclTypeintClass
								);
					} else {
						tmp = NclMalloc(sizeof(float) * grid_dim_sizes[0] * grid_dim_sizes[1]);
						for( i = 0; i < grid_dim_sizes[0] * grid_dim_sizes[1]; i++){
							((float*)tmp)[i] = DEFAULT_MISSING_FLOAT;
						}
						missingv.floatval = DEFAULT_MISSING_FLOAT;

						step->thelist[offset].rec_inq = _MakeMissingRec();
						current_rec = step->thelist[offset].rec_inq;
						current_rec->the_dat = _NclCreateVal(
									NULL,
									NULL,
									Ncl_MultiDValData,
									0,
									tmp,
									&missingv,
									n_grid_dims,
									grid_dim_sizes,
									PERMANENT,
									NULL,
									nclTypefloatClass
								);
					}
				}
				if(current_rec->the_dat == NULL) {
	/*
	* Retrieves LRU cache MultiDVal specific to this grid type
	*/
					current_rec->the_dat = _NclGetCacheVal(therec,step,current_rec);
					if(current_rec->the_dat == NULL){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Unrecoverable caching error reading variable can't continue");
						fclose(fd);
						NclFree(vbuf);
						return(NULL);
					}
	/*
	* grid and grid_gds will overwrite tmp
	*/
					tmp = current_rec->the_dat->multidval.val;
					if((current_rec->has_gds)&&(current_rec->grid_number == 255)&&(current_rec->grid_gds_tbl_index > -1)) {
						if(grid_gds[current_rec->grid_gds_tbl_index].un_pack != NULL) {
							int_or_float = (*grid_gds[current_rec->grid_gds_tbl_index].un_pack)(fd,&tmp,&missing,current_rec,step);
						}
					} else if(current_rec->grid_tbl_index > -1) {
						if(grid[current_rec->grid_tbl_index].un_pack != NULL) {
							int_or_float = (*grid[current_rec->grid_tbl_index].un_pack)(fd,&tmp,&missing,current_rec,step);
						}
					} else if((current_rec->has_gds)&&(current_rec->grid_gds_tbl_index > -1)) {
						if(grid_gds[current_rec->grid_gds_tbl_index].un_pack != NULL) {
							int_or_float = (*grid_gds[current_rec->grid_gds_tbl_index].un_pack)(fd,&tmp,&missing,current_rec,step);
						}
					}
					if(tmp != NULL) {
						if(int_or_float) {
							if(missing != NULL) {
								missingv.intval = *(int*)missing;
							} else {
								missingv.intval = DEFAULT_MISSING_INT;
							}
	/*
	* Needed to fix chicken/egg problem with respect to type and missing values
	*/
							_NclAdjustCacheTypeAndMissing(int_or_float,current_rec->the_dat,(missing == NULL) ? NULL : &missingv);

							NclFree(missing);
						} else {
							if(missing != NULL) {
								missingv.floatval = *(float*)missing;
							} else {
								missingv.floatval = DEFAULT_MISSING_FLOAT;
							}
	/*
	* Needed to fix chicken/egg problem with respect to type and missing values
	*/
							_NclAdjustCacheTypeAndMissing(int_or_float,current_rec->the_dat,(missing == NULL) ? NULL : &missingv);

							NclFree(missing);
						}
					} else {
	/*
	* Need to figure out what to do here
	*/
					}
				} 
				if(current_rec->the_dat != NULL) {
					tmp_md = (NclMultiDValData)_NclReadSubSection((NclData)current_rec->the_dat,&sel_ptr,NULL);
					memcpy((void*)&((char*)out_data)[data_offset],tmp_md->multidval.val,tmp_md->multidval.totalsize);
					data_offset += tmp_md->multidval.totalsize;
					if(tmp_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)tmp_md);
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Unrecoverable erro reading variable can't continue");
					fclose(fd);
					NclFree(vbuf);
					return(NULL);
				}

				if(n_other_dims > 0 ) {	
					current_index[n_other_dims-1] += stride[n_other_dims-1];
					for(i = n_other_dims-1; i > 0 ; i--) {
						if(current_index[i] > finish[i]) {
							current_index[i] = start[i];
							current_index[i-1] += stride[i-1];
						} else {
							inc_done = 1;
						}
						if(inc_done) {
							inc_done = 0;
							break;
						}
					}
					if(current_index[0] > finish[0]) {
						done = 1;
					}
				} else {
					done = 1;
				}
			}
			fclose(fd);
			NclFree(vbuf);
			return(out_data);
		} 
		step = step->next;
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclGRIB: Variable (%s) is not an element of file (%s)",NrmQuarkToString(thevar),NrmQuarkToString(rec->file_path_q));

	return(NULL);
}

static NclFVarRec *GribGetCoordInfo
#if	NhlNeedProto
(void* therec, NclQuark thevar)
#else
(therec, thevar)
void* therec;
NclQuark thevar;
#endif
{
return(GribGetVarInfo(therec, thevar));
}

static void *GribReadCoord
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
return(GribReadVar(therec, thevar, start, finish,stride,storage));
}

static NclQuark *GribGetAttNames
#if	NhlNeedProto
(void* therec,int *num_atts)
#else
(therec,num_atts)
void* therec;
int *num_atts;
#endif
{	
*num_atts = 0;
return(NULL);
}

static NclFAttRec* GribGetAttInfo
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

static NclQuark *GribGetVarAttNames
#if	NhlNeedProto
(void *therec , NclQuark thevar, int* num_atts)
#else
(therec , thevar, num_atts)
void *therec;
NclQuark thevar;
int* num_atts;
#endif
{
GribFileRecord *thefile = (GribFileRecord*)therec;
GribParamList *step;
GribInternalVarList *vstep;
NclQuark *arout;
GribAttInqRecList *theatts;
int i;


vstep = thefile->internal_var_list;
while(vstep != NULL) {
	if(vstep->int_var->var_info.var_name_quark == thevar) {
		*num_atts = vstep->int_var->n_atts;
		arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*vstep->int_var->n_atts);
		theatts = vstep->int_var->theatts;
		break;
	} else {
		vstep = vstep->next;
	}
}	

if(vstep == NULL ) {
	step = thefile->var_list;	
	while(step != NULL) {
		if(step->var_info.var_name_quark == thevar) {
			*num_atts = step->n_atts;
			arout = (NclQuark*)NclMalloc(sizeof(NclQuark)*step->n_atts);
			theatts = step->theatts;
			break;
		} else {
			step = step->next;
		}
	}
}
if((arout != NULL)&&(theatts!= NULL))  {
	for(i = 0; i < *num_atts; i++) {
		arout[i] = theatts->att_inq->name;
		theatts = theatts->next;
	}
	return(arout);
} else {
	*num_atts = 0;	
	return(NULL);
}
}
static NclFAttRec *GribGetVarAttInfo
#if	NhlNeedProto
(void *therec, NclQuark thevar, NclQuark theatt)
#else
(therec, thevar, theatt)
void *therec;
NclQuark thevar;
NclQuark theatt;
#endif
{
GribFileRecord *thefile = (GribFileRecord*)therec;
GribParamList *step;
GribInternalVarList *vstep;
GribAttInqRecList *theatts;
int i;
NclFAttRec *tmp;


vstep = thefile->internal_var_list;
while(vstep != NULL) {
	if(vstep->int_var->var_info.var_name_quark == thevar) {
		theatts = vstep->int_var->theatts;
		break;
	} else {
		vstep = vstep->next;
	}
}	

if(vstep == NULL ) {
	step = thefile->var_list;	
	while(step != NULL) {
		if(step->var_info.var_name_quark == thevar) {
			theatts = step->theatts;
			break;
		} else {
			step = step->next;
		}
	}
}
if(theatts!= NULL)  {
	while(theatts != NULL) {
		if(theatts->att_inq->name == theatt) {
			tmp = (NclFAttRec*)NclMalloc(sizeof(NclFAttRec));
			tmp->att_name_quark = theatt;
			tmp->data_type = theatts->att_inq->thevalue->multidval.data_type;
			tmp->num_elements = theatts->att_inq->thevalue->multidval.totalelements;
			return(tmp);
		}
		theatts = theatts->next;
	}
} else {
	return(NULL);
}
}


static void *GribReadVarAtt
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
GribFileRecord *thefile = (GribFileRecord*)therec;
GribParamList *step;
GribInternalVarList *vstep;
GribAttInqRecList *theatts;
int i;
void *out_dat;


vstep = thefile->internal_var_list;
while(vstep != NULL) {
	if(vstep->int_var->var_info.var_name_quark == thevar) {
		theatts = vstep->int_var->theatts;
		break;
	} else {
		vstep = vstep->next;
	}
}	

if(vstep == NULL ) {
	step = thefile->var_list;	
	while(step != NULL) {
		if(step->var_info.var_name_quark == thevar) {
			theatts = step->theatts;	
			break;
		} else {
			step = step->next;
		}
	}
}
if(theatts!= NULL)  {
	while(theatts != NULL) {
		if(theatts->att_inq->name == theatt) {
			if(storage != NULL) {
				memcpy(storage,theatts->att_inq->thevalue->multidval.val,theatts->att_inq->thevalue->multidval.totalsize);
				return(storage);
			} else {
				out_dat = (void*)NclMalloc(theatts->att_inq->thevalue->multidval.totalsize);
				memcpy(out_dat,theatts->att_inq->thevalue->multidval.val,theatts->att_inq->thevalue->multidval.totalsize);
				return(out_dat);
			}
		}
		theatts = theatts->next;
	}
} else {
	return(NULL);
}
}

static void *GribReadAtt
#if	NhlNeedProto
(void *therec,NclQuark theatt,void* storage)
#else
(therec,theatt,storage)
void * therec;
NclQuark theatt;
void* storage;
#endif
{
return(NULL);
}


NclFormatFunctionRec GribRec = {
/* NclCreateFileRecFunc	   create_file_rec; */		GribCreateFileRec,
/* NclGetFileRecFunc       get_file_rec; */		GribGetFileRec,
/* NclFreeFileRecFunc      free_file_rec; */		GribFreeFileRec,
/* NclGetVarNamesFunc      get_var_names; */		GribGetVarNames,
/* NclGetVarInfoFunc       get_var_info; */		GribGetVarInfo,
/* NclGetDimNamesFunc      get_dim_names; */		GribGetDimNames,
/* NclGetDimInfoFunc       get_dim_info; */		GribGetDimInfo,
/* NclGetAttNamesFunc      get_att_names; */		GribGetAttNames,
/* NclGetAttInfoFunc       get_att_info; */		GribGetAttInfo,
/* NclGetVarAttNamesFunc   get_var_att_names; */	GribGetVarAttNames,
/* NclGetVarAttInfoFunc    get_var_att_info; */		GribGetVarAttInfo,
/* NclGetCoordInfoFunc     get_coord_info; */		GribGetCoordInfo,
/* NclReadCoordFunc        read_coord; */		GribReadCoord,
/* NclReadCoordFunc        read_coord; */		NULL,
/* NclReadVarFunc          read_var; */			GribReadVar,
/* NclReadVarFunc          read_var; */			NULL,
/* NclReadAttFunc          read_att; */			GribReadAtt,
/* NclReadVarAttFunc       read_var_att; */		GribReadVarAtt,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteCoordFunc       write_coord; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteVarFunc         write_var; */		NULL,
/* NclWriteAttFunc         write_att; */		NULL,
/* NclWriteVarAttFunc      write_var_att; */		NULL,
/* NclAddDimFunc           add_dim; */			NULL,
/* NclAddDimFunc           rename_dim; */		NULL,
/* NclAddVarFunc           add_var; */			NULL,
/* NclAddVarFunc           add_coord_var; */		NULL,
/* NclAddAttFunc           add_att; */			NULL,
/* NclAddVarAttFunc        add_var_att; */		NULL,
/* NclMapFormatTypeToNcl   map_format_type_to_ncl; */	GribMapToNcl,
/* NclMapNclTypeToFormat   map_ncl_type_to_format; */	GribMapFromNcl,
/* NclDelAttFunc           del_att; */			NULL,
/* NclDelVarAttFunc        del_var_att; */		NULL
};
NclFormatFunctionRecPtr GribAddFileFormat 
#if	NhlNeedProto
(void)
#else 
()
#endif
{

return(&GribRec);
}

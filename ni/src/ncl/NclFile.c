#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include <math.h>
#include "NclVar.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclMultiDValData.h"
#include "NclAtt.h"
#include "AttSupport.h"

static NclQuark FileGetDimName(
#ifdef NhlNeedProto
NclFile /* thefile */,
int /*num*/
#endif
);

static NclObjTypes FileVarRepValue(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */
#endif
);

#define FILE_COORD_VAR_ACCESS 0
#define FILE_VAR_ACCESS 1

static int FileIsVar(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static NhlErrorTypes FileWriteVarVar(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*lhs_var*/,
struct _NclSelectionRecord * /* lhs_sel_ptr */,
struct _NclVarRec* /* rhs_var*/,
struct _NclSelectionRecord * /* rhs_sel_ptr */
#endif
);

static NhlErrorTypes FileWriteVar(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

static struct _NclVarRec *FileReadVar(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static struct _NclMultiDValDataRec * FileReadVarValue(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileIsAtt(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

static struct _NclMultiDValDataRec* FileReadAtt(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

static NhlErrorTypes FileWriteAtt(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileIsVarAtt(
#ifdef NhlNeedProto
NclFile /*file*/,
NclQuark /* var */,
NclQuark /* attname*/
#endif
);

static struct _NclMultiDValDataRec *FileReadVarAtt(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

static NhlErrorTypes FileWriteVarAtt(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

static int FileIsDim(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* dimname */
#endif
);
static int FileVarIsDim(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* var */,
NclQuark /* dimname */
#endif
);

static struct _NclMultiDValDataRec *FileVarReadDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

static NhlErrorTypes FileVarWriteDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);
static struct _NclMultiDValDataRec *FileReadDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

static NhlErrorTypes FileWriteDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

static int FileIsCoord(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*coord_name */
#endif
);

static struct _NclVarRec *FileReadCoord(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

static NhlErrorTypes FileWriteCoord(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

void FilePrint
#if  __STDC__
(NclObj self, FILE    *fp)
#else
(self, fp)
NclObj self;
FILE    *fp;
#endif
{
	NclFile thefile = (NclFile)self;
	int i,j;

	

	fprintf(fp,"\nfilename:\t%s\n",NrmQuarkToString(thefile->file.fname));
	fprintf(fp,"path:\t%s\n",NrmQuarkToString(thefile->file.fpath));
	fprintf(fp,"\tdimensions:\n");
	for(i = 0; i< thefile->file.n_file_dims; i++) {
		fprintf(fp,"\t\t%s = %ld\n",NrmQuarkToString(thefile->file.file_dim_info[i]->dim_name_quark),	
			thefile->file.file_dim_info[i]->dim_size);
	}
	fprintf(fp,"\tvariables:\n");
	for(i = 0; i < thefile->file.n_vars; i++) {
		fprintf(fp,"\t\t%s %s(",_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type),NrmQuarkToString(thefile->file.var_info[i]->var_name_quark));
		for(j=0; j< thefile->file.var_info[i]->num_dimensions - 1; j++) {
			fprintf(fp,"%s,",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j])));
		}
		fprintf(fp,"%s)\n",NrmQuarkToString(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[thefile->file.var_info[i]->num_dimensions - 1])));
	}
	
	return;
}

void FileDestroy
#if  __STDC__
(NclObj self)
#else
(self)
NclObj self;
#endif
{
	NclFile thefile = (NclFile) self;
	int i;
	NclFileAttInfoList *step,*tmp;

	_NclUnRegisterObj((NclObj)self);
	if(thefile->file.format_funcs->free_file_rec != NULL) {
		(*thefile->file.format_funcs->free_file_rec)(thefile->file.private_rec);
	}
	for(i =0 ; i < thefile->file.n_vars; i++) {
		NclFree(thefile->file.var_info[i]);
		if(thefile->file.var_att_info[i] != NULL) {
			step = thefile->file.var_att_info[i];	
			if(thefile->file.var_att_ids[i]!= -1) {
				_NclDelParent(_NclGetObj(thefile->file.var_att_ids[i]),self);
			}
			while(step != NULL) {
				NclFree(step->the_att);
				tmp = step;
				step = step->next;
				NclFree(tmp);
			}
		}
	}
	for(i =0 ; i < thefile->file.n_file_dims; i++) {
		NclFree(thefile->file.file_dim_info[i]);
	}
	if(thefile->file.file_atts_id != -1) {
		_NclDelParent(_NclGetObj(thefile->file.file_atts_id),self);
	}
	for(i =0 ; i < thefile->file.n_file_atts; i++) {
		NclFree(thefile->file.file_atts[i]);
	}
	NclFree(thefile);
	return;
}

NhlErrorTypes FileAddParent
#if  __STDC__
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList * tmp = NULL;

        tmp = theobj->obj.parents;
        theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
        theobj->obj.parents->next = tmp;
        theobj->obj.parents->pptr = parent;
        theobj->obj.ref_count++;
        return(NhlNOERROR);

}

NhlErrorTypes FileDelParent
#if  __STDC__
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList *tmp,*tmp1;
        int found = 0;

        if(theobj->obj.parents == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"FileDelParent: Attempt to delete parent from empty list");
                return(NhlFATAL);
        }

        tmp = theobj->obj.parents;
        if((tmp!=NULL)&&(tmp->pptr->obj.id == parent->obj.id)) {
                theobj->obj.parents = theobj->obj.parents->next;
                NclFree(tmp);
                tmp = theobj->obj.parents;
                found = 1;
                theobj->obj.ref_count--;
        }
        if((tmp == NULL)&&(found)) {
                _NclDestroyObj(theobj);
                return(NhlNOERROR);
        }
        while(tmp->next != NULL) {
                if(tmp->next->pptr->obj.id == parent->obj.id) {
                        found = 1;
                        tmp1 = tmp->next;
                        tmp->next = tmp->next->next;
                        NclFree(tmp1);
                        theobj->obj.ref_count--;
                        if(theobj->obj.ref_count <= 0)
                                _NclDestroyObj(theobj);
                        return(NhlNOERROR);
                } else {
                        tmp = tmp->next;
                }
        }
        return(NhlWARNING);

}



NclFileClassRec nclFileClassRec = {
	{	
		"NclVarClass",
		sizeof(NclVarRec),
		(NclObjClass)&nclObjClassRec,
		0,
		(NclGenericFunction)FileDestroy,
		(NclSetStatusFunction)NULL,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)NULL,
		(NclAddParentFunction)FileAddParent,
		(NclDelParentFunction)FileDelParent,
		(NclPrintFunction) FilePrint
	},
	{
		FileVarRepValue,
		FileIsVar,
		FileWriteVar,
		FileWriteVarVar,
		FileReadVar,
		FileReadVarValue,
		FileIsAtt,
		FileReadAtt,
		FileWriteAtt,
		FileIsVarAtt,
		FileReadVarAtt,
		FileWriteVarAtt,
		FileIsDim,
		FileVarIsDim,
		FileVarReadDim,
		FileVarWriteDim,
		FileReadDim,
		FileWriteDim,
		FileIsCoord,
		FileReadCoord,
		FileWriteCoord
	}
};

NclObjClass nclFileClass = (NclObjClass)&nclFileClassRec;

static void AddAttInfoToList
#if  __STDC__ 
(NclFileAttInfoList **list_handle,struct _NclFAttRec*     the_att) 
#else 
(list_handle,the_att) 
NclFileAttInfoList **list_handle;
struct _NclFAttRec*     the_att; 
#endif
{
	NclFileAttInfoList *step;
	if(*list_handle == NULL) {
		*list_handle = (NclFileAttInfoList*)NclMalloc(sizeof(NclFileAttInfoList));
		(*list_handle)->next = NULL;
		(*list_handle)->the_att = the_att;
	} else {
		step = *list_handle;
		while(step->next != NULL) {
			step = step->next;
		}
		step->next = (NclFileAttInfoList*)NclMalloc(sizeof(NclFileAttInfoList));
		step->next->next = NULL;
		step->next->the_att = the_att;
	}
	return;
}
static int FileIsVar
#if  __STDC__
(NclFile thefile,NclQuark var)
#else 
(thefile,var)
	NclFile thefile;
	NclQuark var;
#endif
{
	int i;

	for(i = 0; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i]->var_name_quark == var) {
			return(i);
		}
	}
	return(-1);
}

static NclObjTypes FileVarRepValue
#if  __STDC__
(NclFile thefile, NclQuark var)
#else 
(thefile, var)
NclFile thefile;
NclQuark var;
#endif
{
	int index; 

	index = FileIsVar(thefile,var);

	if(index > -1) {	
		return(_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type));
	} else {
		return(Ncl_None);
	}
}



static struct _NclMultiDValDataRec* MyFileReadVarValue
#if  __STDC__
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr,NclDimRec* dim_info,int vtype)
#else 
(thefile, var_name, sel_ptr,dim_info,vtype)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
NclDimRec* dim_info;
int vtype;
#endif
{
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData mis_md = NULL;
	NclScalar missing_value;
	int has_missing = 0;
	void *val,*ret;
	int index;
	long start[NCL_MAX_DIMENSIONS];
	long finish[NCL_MAX_DIMENSIONS];
	long stride[NCL_MAX_DIMENSIONS];
	long real_stride[NCL_MAX_DIMENSIONS];
	int i,j,k,done = 0,inc_done = 0;
	int n_dims_input,n_elem;
	int n_dims_output;
	int total_elements = 1;
	int has_vectors = 0;
	int has_stride = 0;
	int has_reverse = 0;
	int has_reorder = 0;
	int to = 0,block_read_limit,n_elem_block;
	
	int multiplier_input[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	long current_index[NCL_MAX_DIMENSIONS];
	long current_finish[NCL_MAX_DIMENSIONS];
	int index_map[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];
	NclSelection *sel;
	float tmpf;
/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/
	index = FileIsVar(thefile,var_name);
	n_dims_input = thefile->file.var_info[index]->num_dimensions;
	if(sel_ptr != NULL) {
		sel = sel_ptr->selection;
		for(i = 0; i < n_dims_input; i++) {
			switch(sel->sel_type) {
			case Ncl_SUB_ALL:
				start[sel->dim_num] = 0;
			case Ncl_SUB_VAL_DEF:
				finish[sel->dim_num] = thefile->file.var_info[index]->dim_sizes[sel->dim_num]-1;
			case Ncl_SUB_DEF_VAL:
				if(sel->sel_type != Ncl_SUB_VAL_DEF) {
					start[sel->dim_num] = 0;
				} else {
					start[sel->dim_num] = sel->u.sub.start;
				}
			case Ncl_SUBSCR:
				if(sel->sel_type == Ncl_SUBSCR) {
					if(sel->u.sub.finish < sel->u.sub.start) {
						start[sel->dim_num] = sel->u.sub.start;
						finish[sel->dim_num] = sel->u.sub.finish;
						stride[sel->dim_num] = sel->u.sub.stride;
						compare_sel[sel->dim_num] = -1;
						has_reverse = 1;
					} else {
						start[sel->dim_num] = sel->u.sub.start;
						finish[sel->dim_num] = sel->u.sub.finish;
						stride[sel->dim_num] = sel->u.sub.stride;
						compare_sel[sel->dim_num] = -2;
					}
				} else {
					if(sel->u.sub.finish < sel->u.sub.start) {
						has_reverse = 1;
					} 
					stride[sel->dim_num] = sel->u.sub.stride;
					if(finish[sel->dim_num] < start[sel->dim_num]) {
						compare_sel[sel->dim_num] = -1;
					} else {
						compare_sel[sel->dim_num] = -2;
					}
				}
				if(stride[sel->dim_num] > 1) 
					has_stride = 1;
				tmpf = (float)fabs(((float)sel->u.sub.stride));
				n_elem =(int)(fabs(((float)(finish[sel->dim_num] - start[sel->dim_num]))) /tmpf) + 1;
/*
* set when normal subscript
*/
				if(sel->dim_num != i) {
					has_reorder = 1;
				}
				index_map[i] = sel->dim_num;
				break;
			case Ncl_VECSUBSCR:
				if((sel->u.vec.min < 0)||(sel->u.vec.min >= thefile->file.var_info[index]->dim_sizes[sel->dim_num])) {	
					NhlPError(NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				if((sel->u.vec.max < 0)||(sel->u.vec.max >= thefile->file.var_info[index]->dim_sizes[sel->dim_num])) {	
					NhlPError(NhlFATAL,NhlEUNKNOWN, "Subscript out of range, error in subscript #%d",i);
					return(NULL);
				}
				n_elem = sel->u.vec.n_ind;
			
				stride[sel->dim_num] = 0;
				start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
				has_vectors = 1;
				if(sel->dim_num != i) {
					has_reorder = 1;
				}
				index_map[i] = sel->dim_num;

/*
* 0 when vector subscript
*/
				compare_sel[sel->dim_num] = 0;
				break;
			}
			if(sel->dim_num != n_dims_input -1) {
				multiplier_input[sel->dim_num] = 1;
				for(k = sel->dim_num + 1; k < n_dims_input; k++) {
					multiplier_input[sel->dim_num] *= (long)thefile->file.var_info[index]->dim_sizes[k];
				}
			}
			output_dim_sizes[i] = n_elem;
			(dim_info)[i].dim_num = i;
			(dim_info)[i].dim_size = n_elem;
			(dim_info)[i].dim_quark = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[sel->dim_num]);
			total_elements = total_elements * n_elem;
			sel++;
		}
		sel = sel_ptr->selection;
	} else {
		for(i = 0; i< n_dims_input; i++) {
			start[i] = 0;
			finish[i] = thefile->file.var_info[index]->dim_sizes[i] - 1;
			stride[i] = 1;
			total_elements *= thefile->file.var_info[index]->dim_sizes[i];
			output_dim_sizes[i] = thefile->file.var_info[index]->dim_sizes[i];
			(dim_info)[i].dim_num = i;
			(dim_info)[i].dim_size = output_dim_sizes[i];
			(dim_info)[i].dim_quark = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[i]);
			compare_sel[i] = -1;
			multiplier_input[i] = 1;
			for(k = i + 1; k < n_dims_input; k++) {
				multiplier_input[i] *= (long)thefile->file.var_info[index]->dim_sizes[k];
			}

		}
		sel = NULL;
	}
/*
* When ncl gets here all strides are positive and finishs are greater than starts
* and stride and finishes all corespond to their *CORRECT* dimension numbers not
* and reordered dimensions. Any reordering has to occur afterwards.
*/

/*
* Basically everything is different depending on which format_func field contains
* the read function.
*/

/* 
* Take care of simplest case here
*/
	if((vtype == FILE_VAR_ACCESS? thefile->file.format_funcs->read_var != NULL:thefile->file.format_funcs->read_coord != NULL)) {
		if((!has_vectors)&&(!has_reverse)&&(!has_reorder)) {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if(vtype == FILE_VAR_ACCESS) {
				ret = (*thefile->file.format_funcs->read_var)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					stride,
					val);
			} else {
				ret = (*thefile->file.format_funcs->read_coord)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					stride,
					val);
			}
		
			n_dims_output = n_dims_input;
			i = 0;
			while((i <  n_dims_output)&&(n_dims_output > 1)) {
				if(output_dim_sizes[i] == 1) {
					for(j = i; j < n_dims_output-1; j++) {
						output_dim_sizes[j] = output_dim_sizes[j+1];
						(dim_info)[j] = (dim_info)[j+1];
					}
					n_dims_output--;
				} else {
					i++;
				}
			}
		} else {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			to = 0;
			block_read_limit = n_dims_input - 1 ;
/*
* Find out what size chunks can be read in at once
*/
			for(i = n_dims_input-1 ; i>= 0; i--) {
				if((compare_sel[index_map[i]] != -2)||(index_map[i] != i)) {
					block_read_limit = i;
					break;
				}
			}
/*
* Initialize starting index, finish and stride values for first read
*/
			n_elem_block = 1;
			for(i = 0; i < n_dims_input ; i++) {
				current_index[index_map[i]] = start[index_map[i]]; 
				if(i > block_read_limit) {
/*
* OK to use i here since these indices are in order
*/
					n_elem_block *= output_dim_sizes[i];
                                        current_finish[i] = finish[i];
                                        real_stride[i] = stride[i];
				} else {
					switch(compare_sel[index_map[i]]) {
					case -1:
						current_finish[index_map[i]] = current_index[index_map[i]] ;
						real_stride[index_map[i]] = 1;
						break;
					case -2:
						current_finish[index_map[i]] = current_index[index_map[i]] ;
						real_stride[index_map[i]] = 1;
						break;
					default:	 /* vectors */
						current_finish[index_map[i]]  = current_index[index_map[i]];
						real_stride[index_map[i]] = 1; 
						break;
					}
				}
			}
			while(!done) {
				if(vtype == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->read_var)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				} else {
					ret = (*thefile->file.format_funcs->read_coord)(
						thefile->file.private_rec,
						thefile->file.var_info[index]->var_name_quark,
						current_index,
						current_finish,
						real_stride,
						(void*)&(((char*)val)[to]));
				}
				to += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
				if(compare_sel[index_map[block_read_limit]] < 0) {
					current_index[index_map[block_read_limit]] += stride[n_dims_input-1];
					current_finish[index_map[block_read_limit]] = current_index[index_map[block_read_limit]];
				} else {
					compare_sel[index_map[block_read_limit]]++;
				}
				for(i = block_read_limit; i > 0; i--) {
					switch(compare_sel[index_map[i]]) {
					case -2:
						if(current_index[index_map[i]] > finish[index_map[i]]) {
							current_index[index_map[i]] = start[index_map[i]];
							if(compare_sel[index_map[i-1]] < 0 ) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}

						} else {
							inc_done = 1;
						}	
						current_finish[index_map[i]] = current_index[index_map[i]] ;
						break;
					case -1:
						if(current_index[index_map[i]] < finish[index_map[i]]) {
							current_index[index_map[i]] = start[index_map[i]];
							if(compare_sel[index_map[i-1]] < 0) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}
						} else {	
							inc_done = 1;
						}
						current_finish[index_map[i]] = current_index[index_map[i]];
						break;
					default:
						if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
							compare_sel[index_map[i]] = 0;
							current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
							if(compare_sel[index_map[i-1]] < 0 ) {
								current_index[index_map[i-1]] += stride[index_map[i-1]];
							} else {
								compare_sel[index_map[i-1]]++;
							}
						} else {
							current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
							inc_done = 1;
						}
						current_finish[index_map[i]] = current_index[index_map[i]];
						break;
					} 
					if(inc_done) {
						inc_done = 0;
						break;
					}
				}
				switch(compare_sel[index_map[0]]) {
				case -2:
					if(current_index[index_map[0]] > finish[index_map[0]]) 
							done = 1;
					current_finish[index_map[0]] = current_index[index_map[0]]; 
					break;
				case -1:
					if(current_index[index_map[0]] < finish[index_map[0]]) 
							done = 1;
					current_finish[index_map[0]] = current_index[index_map[0]]; 
					break;
				default:
					if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
							done = 1;
					}  else {
						current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
					}
					current_finish[index_map[0]] = current_index[index_map[0]]; 
				}
			}
			n_dims_output = n_dims_input;
			i = 0;
			while((i <  n_dims_output)&&(n_dims_output > 1)) {
				if(output_dim_sizes[i] == 1) {
					for(j = i; j < n_dims_output-1; j++) {
						output_dim_sizes[j] = output_dim_sizes[j+1];
						(dim_info)[j] = (dim_info)[j+1];
					}
					n_dims_output--;
				} else {
					i++;
				}
			}
		}
	} else if((vtype == FILE_VAR_ACCESS ? thefile->file.format_funcs->read_var_ns != NULL : thefile->file.format_funcs->read_coord_ns!= NULL)){
		if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(!has_stride)) {
			val = (void*)NclMalloc(total_elements*_NclSizeOf(thefile->file.var_info[index]->data_type));
			if(vtype == FILE_VAR_ACCESS) {
				ret = (*thefile->file.format_funcs->read_var_ns)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					val);
			} else {
				ret = (*thefile->file.format_funcs->read_coord_ns)(
					thefile->file.private_rec,
					thefile->file.var_info[index]->var_name_quark,
					start,
					finish,
					val);
			}
		
			n_dims_output = n_dims_input;
			i = 0;
			while((i <  n_dims_output)&&(n_dims_output > 1)) {
				if(output_dim_sizes[i] == 1) {
					for(j = i; j < n_dims_output-1; j++) {
						output_dim_sizes[j] = output_dim_sizes[j+1];
						(dim_info)[j] = (dim_info)[j+1];
					}
					n_dims_output--;
				} else {
					i++;
				}
			}
		} else {
			return(NULL);
		}
	} 
	if(FileIsVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT)!=-1)){
		mis_md = FileReadVarAtt(thefile,var_name,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
		if(mis_md != NULL) {
			memcpy((void*)&missing_value,mis_md->multidval.val,_NclSizeOf(mis_md->multidval.data_type));
			has_missing = 1;
		}
	} 
	tmp_md = _NclCreateVal(
		NULL,
		NULL,
		_NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type),
		0,
		val,
		(has_missing ? &missing_value:NULL),
		n_dims_output,
		output_dim_sizes,
		TEMPORARY,
		sel_ptr);
	return(tmp_md);
}
static struct _NclMultiDValDataRec* FileReadVarValue
#if  __STDC__
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];

	return(MyFileReadVarValue(thefile, var_name, sel_ptr,dim_info,FILE_VAR_ACCESS));
}


static struct _NclVarRec *FileReadVar
#if  __STDC__
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp_att_md = NULL;
	NclFileAttInfoList *step = NULL;
	NclVar tmp_var = NULL;
	int index;
	int att_id;
	
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	NclSelection *sel = NULL;
	NclObj  att_obj = NULL;
/*
* By the the time it gets here the file suport routines in that build the selection
* record have made sure var_name is valid and all the demensions in sel_ptr
* are valid. However, the values have not been checked for out_of_ranges
* subscripts
*/
	index = FileIsVar(thefile,var_name);
	if(index > -1) {
		tmp_md = MyFileReadVarValue(thefile,var_name,sel_ptr,dim_info,FILE_VAR_ACCESS);


		if(thefile->file.var_att_ids[index] != -1) {
			att_id = thefile->file.var_att_ids[index];
			att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
			if(att_obj != NULL) {
				att_id = att_obj->obj.id;
			} else {
				att_id = -1;
			}
		} else if(thefile->file.var_att_info[index] != NULL){
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
			step = thefile->file.var_att_info[index];
			while(step != NULL) {
				tmp_att_md = FileReadVarAtt(thefile,thefile->file.var_info[index]->var_name_quark,step->the_att->att_name_quark,NULL);
				if(tmp_att_md != NULL) {
					if(tmp_att_md->obj.status == TEMPORARY) {
						_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_att_md,NULL);
					} else {
						tmp_att_md = _NclCopyVal(tmp_att_md,NULL);
						_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_att_md,NULL);
					}
		
				}
				step = step->next;
			}
		} else {
			att_id = -1;
		}
		if(sel_ptr != NULL) {
			sel = sel_ptr->selection;
		}
		
		
	
		if(tmp_md != NULL) {
			tmp_var = _NclVarCreate(
				NULL,
				NULL,
				Ncl_Var,
				0,
				NULL,
				tmp_md,
				dim_info,
				att_id,
				NULL,
				(sel == NULL ? FILEVAR : FILEVARSUBSEL),
				NrmQuarkToString(var_name));
			if(tmp_var == NULL) {
				_NclDestroyObj((NclObj)tmp_md);
				if(att_id != -1) {
					_NclDestroyObj((NclObj)_NclGetObj(att_id));
				}
			}
		}
	}
	return(tmp_var);
}


static int FileIsVarAtt
#if  __STDC__
(NclFile thefile,NclQuark var,NclQuark theatt)
#else 
(thefile,var,theatt)
NclFile thefile;
NclQuark var;
NclQuark theatt;
#endif
{
	int index,i;
	NclFileAttInfoList *step_att;

	index = FileIsVar(thefile,var);
	i = 0;
	if(index > -1) {
		step_att = thefile->file.var_att_info[index];
		while(step_att != NULL) {
			if(step_att->the_att->att_name_quark == theatt) {
				return(i);
			} else {
				i++;
				step_att = step_att->next;
			}
		}
	}
	return(-1);
}

static struct _NclMultiDValDataRec *FileReadVarAtt
#if  __STDC__
(NclFile thefile, NclQuark var, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, var, attname, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int aindex,index;
	NclFileAttInfoList *step;
	int att_id = -1;
	void *val;
	NclMultiDValData tmp_md;
	
	aindex = FileIsVarAtt(thefile,var,attname);
	if(aindex > -1) {
		index = FileIsVar(thefile,var);
		if(thefile->file.var_att_ids[index] != -1) {
			return(_NclGetAtt(thefile->file.var_att_ids[index],NrmQuarkToString(attname),sel_ptr));
		}
		if(thefile->file.format_funcs->read_var_att != NULL) {
			step = thefile->file.var_att_info[index];
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			while(step != NULL) {
				val = NclMalloc(_NclSizeOf(step->the_att->data_type)* step->the_att->num_elements );
				(void)(*thefile->file.format_funcs->read_var_att)(
					thefile->file.private_rec,
					var,
					step->the_att->att_name_quark,
					val
					);
				tmp_md = _NclCreateVal(
						NULL,
						NULL,
						_NclBasicDataTypeToObjType(step->the_att->data_type),
						0,
						val,
						NULL,
						1,
						&step->the_att->num_elements,
						TEMPORARY,
						NULL);
				if(tmp_md != NULL) {
					_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_md,NULL);
				}
				step = step->next;
			}
			if(att_id != -1) {	
				thefile->file.var_att_ids[index] = att_id;
				return(_NclGetAtt(thefile->file.var_att_ids[index],NrmQuarkToString(attname),sel_ptr));
			}
		}
	}
	return(NULL);
}

static int FileIsAtt
#if  __STDC__
(NclFile thefile,NclQuark theatt)
#else 
(thefile,theatt)
NclFile thefile;
NclQuark theatt;
#endif
{
	int i;

	for( i = 0; i < thefile->file.n_file_atts; i++) {
		if(thefile->file.file_atts[i]->att_name_quark == theatt) {
			return(i);
		}
	}
	return(-1);
}


static int FileIsDim
#if  __STDC__
(NclFile thefile, NclQuark dim_name)
#else 
(thefile, dim_name)
NclFile thefile;
NclQuark dim_name;
#endif
{
	int i;
	
	for(i =0; i < thefile->file.n_file_dims; i++) {
		if(thefile->file.file_dim_info[i]->dim_name_quark == dim_name) {
			return(i);
		}
	} 
	return (-1);
}

static int FileVarIsDim
#if  __STDC__
(NclFile thefile,NclQuark var, NclQuark dim_name)
#else 
(thefile,var,dim_name)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
#endif
{
	int i,j;

	for(i = 0 ; i < thefile->file.n_vars; i++) {
		if(thefile->file.var_info[i]->var_name_quark == var) {
			for(j = 0; j < thefile->file.var_info[i]->num_dimensions; j++){
				if(FileGetDimName(thefile,thefile->file.var_info[i]->file_dim_num[j]) == dim_name) {
					return(j);
				}
			}
			return(-1);
		}
	}
	return(-1);
}


static int FileIsCoord
#if  __STDC__
(NclFile thefile, NclQuark coord_name)
#else 
(thefile,coord_name)
NclFile thefile;
NclQuark coord_name;
#endif
{
	int index;

	index = FileIsDim(thefile,coord_name);
	if(index > -1) {
		if(thefile->file.coord_vars[index] != NULL) {
			return(index);
		} 
	}
	return(-1);
}


NclFile _NclCreateFile
#if   __STDC__
(NclObj  inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, NclStatus status, NclQuark path,int rw_status)
#else
(inst, theclass, obj_type, obj_type_mask, status, path, rw_status)
NclObj  inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
NclStatus status; 
NclQuark path;
int rw_status;
#endif
{
	char *the_path = NrmQuarkToString(path);
	char *last_slash = NULL;
	char *end_of_name = NULL;
	char buffer[NCL_MAX_STRING];
	NclQuark fname_q;
	int i,j;
	NclFile file_out = NULL;
	int file_out_free = 0;
	NhlErrorTypes ret= NhlNOERROR;
	NclObjClass class_ptr;
	NclQuark file_ext_q;
	NclQuark *name_list;
	int n_names;
	NclQuark *name_list2;
	int n_names2;
	int index;

	ret = _NclInitClass(nclFileClass);
	if(ret < NhlWARNING) 
		return(NULL);
	if(theclass == NULL) {
		class_ptr = nclFileClass;
	} else {
		class_ptr = theclass;
	}
	last_slash = strrchr(the_path,'/');
	if(last_slash == NULL) {
		last_slash = the_path;
	}  else {
/*
* skip over '/'
*/
		last_slash++;
	}
	end_of_name = strrchr(last_slash,'.');
	if(end_of_name == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) has no file extension, can't determine type of file to open",NrmQuarkToString(path));
		return(NULL);
	} else {
		i = 0;
		while(last_slash != end_of_name) {
			buffer[i] = *last_slash;
			i++;
			last_slash++;
		}
		buffer[i] = '\0';
		fname_q = NrmStringToQuark(buffer);
/*
* skip over '.'
*/
		end_of_name++;
		file_ext_q = NrmStringToQuark(end_of_name);
		
	}
	if(inst == NULL) {
		file_out = (NclFile)NclMalloc(sizeof(NclFileRec));
		file_out_free = 1;
	} else {
		file_out = (NclFile)inst;
	}
	(void)_NclObjCreate((NclObj)file_out,class_ptr,obj_type,(obj_type_mask | Ncl_File),status);
	file_out->file.fname = fname_q;
	file_out->file.fpath = path;
	file_out->file.file_type = 0;
	file_out->file.n_vars = 0;
	file_out->file.file_atts_id = -1;
	for(i = 0; i < NCL_MAX_FVARS; i++) {
		file_out->file.var_info[i] = NULL;
		file_out->file.file_atts[i] = NULL;
		file_out->file.var_att_info[i] = NULL;
		file_out->file.var_att_ids[i] = -1;
	}
	for(i = 0; i < NCL_MAX_DIMENSIONS; i++) {
		file_out->file.file_dim_info[i] = NULL;
		file_out->file.coord_vars[i] = NULL;
	}
	file_out->file.format_funcs = _NclGetFormatFuncs(file_ext_q);
	if(file_out->file.format_funcs != NULL) {
		if(file_out->file.format_funcs->get_file_rec != NULL) {
			file_out->file.wr_status = rw_status;
			file_out->file.private_rec = (*file_out->file.format_funcs->get_file_rec)(path,rw_status);	
			if(file_out->file.private_rec == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not open (%s)",NrmQuarkToString(path));
				if(file_out_free) 
					NclFree((void*)file_out_free);
				return(NULL);
			}
		} else  {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error in the extension code for the requested file format has occured, could not open (%s)",NrmQuarkToString(path));
		if(file_out_free) 
			NclFree((void*)file_out_free);
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"An internal error in the extension code for the requested file format has occured, could not open (%s)",NrmQuarkToString(path));
		if(file_out_free) 
			NclFree((void*)file_out_free);
		return(NULL);
	}
	if(file_out->file.format_funcs->get_var_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_var_names)(file_out->file.private_rec,&n_names);
		file_out->file.n_vars = n_names;
		for(i = 0; i < n_names; i++){
			file_out->file.var_info[i] = (*file_out->file.format_funcs->get_var_info)(file_out->file.private_rec,name_list[i]);
			if(file_out->file.format_funcs->get_var_att_names != NULL) {
				name_list2 = (*file_out->file.format_funcs->get_var_att_names)(file_out->file.private_rec,name_list[i],&n_names2);
				for(j = 0; j<n_names2; j++) {
					AddAttInfoToList(&(file_out->file.var_att_info[i]),
						(*file_out->file.format_funcs->get_var_att_info)(file_out->file.private_rec,name_list[i],name_list2[j]));
				}
				NclFree((void*)name_list2);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Can not access variable attributes for the file format");
			}
		}
		NclFree((void*)name_list);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not get variable names for file (%s), can't add file",NrmQuarkToString(path));
		if(file_out_free) 
			NclFree((void*)file_out_free);
		return(NULL);
	}
	if(file_out->file.format_funcs->get_dim_names!= NULL) {
		name_list = (*file_out->file.format_funcs->get_dim_names)(file_out->file.private_rec,&n_names);
		file_out->file.n_file_dims = n_names;
		for(i = 0; i < n_names; i++){
			file_out->file.file_dim_info[i] = (*file_out->file.format_funcs->get_dim_info)(file_out->file.private_rec,name_list[i]);
			index = FileIsVar(file_out,name_list[i]);
			if(index > -1) {
				file_out->file.coord_vars[i] = file_out->file.var_info[index];
			}
		}
		NclFree((void*)name_list);
	} else {
/*
* Need code to free already allocated information
*/
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not get dimension names for file (%s), can't add file",NrmQuarkToString(path));
		if(file_out_free) 
			NclFree((void*)file_out_free);
	
		return(NULL);
	}
	if(file_out->file.format_funcs->get_att_names != NULL) {
		name_list = (*file_out->file.format_funcs->get_att_names)(file_out->file.private_rec,&n_names);
		file_out->file.n_file_atts = n_names;
		if(n_names > NCL_MAX_FVARS) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Maximum number of file attributes (%d) exceeded, must ignore some attributes",NCL_MAX_FVARS);
			n_names = NCL_MAX_FVARS;
		}
		for(i = 0; i < n_names; i++) {
			file_out->file.file_atts[i] = (*file_out->file.format_funcs->get_att_info)(file_out->file.private_rec,name_list[i]);
		}
		NclFree((void*)name_list);
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not get attribute names for file (%s), no attributes added ",NrmQuarkToString(path));
	}
	return(file_out);
}

static NhlErrorTypes MyFileWriteVar
#if  __STDC__
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr,NclQuark *dim_names,int type)
#else 
(thefile, var, value, sel_ptr,dim_names,type)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
NclQuark *dim_names;
int type;
#endif
{
	NclObjTypes lhs_type,rhs_type;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData mis_md = NULL;
	NclQuark new_dim_quarks[NCL_MAX_DIMENSIONS];
	long 	new_dim_sizes[NCL_MAX_DIMENSIONS];
	
	int has_missing = 0;
	char buffer[8];
	void *val;
	NhlErrorTypes ret = NhlNOERROR;
	int index,dindex;
	long start[NCL_MAX_DIMENSIONS];
	long finish[NCL_MAX_DIMENSIONS];
	long stride[NCL_MAX_DIMENSIONS];
	long real_stride[NCL_MAX_DIMENSIONS];
	int i,j,k,done = 0,inc_done = 0;
	int n_dims_target,n_elem = 1;
	int n_dims_selection;
	int total_elements = 1;
	int has_vectors = 0;
	int has_stride = 0;
	int has_reverse = 0;
	int has_reorder = 0;
	int from = 0,block_write_limit,n_elem_block;
	
	int multiplier_target[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	long current_index[NCL_MAX_DIMENSIONS];
	long current_finish[NCL_MAX_DIMENSIONS];
	int index_map[NCL_MAX_DIMENSIONS];
	int selection_dim_sizes[NCL_MAX_DIMENSIONS];
	NclSelection *sel;
	float tmpf;
	NclScalar *tmp_mis;
	int tmp_size = 1;
	void *data_type;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;

	if(!thefile->file.wr_status) {
		index = FileIsVar(thefile,var);
		if(index > -1) {
			n_dims_target = thefile->file.var_info[index]->num_dimensions;
			if(sel_ptr != NULL) {
				sel = sel_ptr->selection;
				for(i = 0; i < n_dims_target; i++) {
					switch(sel->sel_type) {
					case Ncl_SUB_ALL:
						start[sel->dim_num] = 0;
					case Ncl_SUB_VAL_DEF:
						finish[sel->dim_num] = thefile->file.var_info[index]->dim_sizes[sel->dim_num]-1;
					case Ncl_SUB_DEF_VAL:
						if(sel->sel_type != Ncl_SUB_VAL_DEF) {
							start[sel->dim_num] = 0;
						} else {
							start[sel->dim_num] = sel->u.sub.start;
						}
					case Ncl_SUBSCR:
						if(sel->sel_type == Ncl_SUBSCR) {
							if(sel->u.sub.finish < sel->u.sub.start) {
								start[sel->dim_num] = sel->u.sub.start;
								finish[sel->dim_num] = sel->u.sub.finish;
								stride[sel->dim_num] = sel->u.sub.stride;
								compare_sel[sel->dim_num] = -1;
								has_reverse = 1;
							} else {
								start[sel->dim_num] = sel->u.sub.start;
								finish[sel->dim_num] = sel->u.sub.finish;
								stride[sel->dim_num] = sel->u.sub.stride;
								compare_sel[sel->dim_num] = -2;
							}
						} else {
							if(sel->u.sub.finish < sel->u.sub.start) {
								has_reverse = 1;
							}
							stride[sel->dim_num] = sel->u.sub.stride;
							if(finish[sel->dim_num] < start[sel->dim_num]) {
								compare_sel[sel->dim_num] = -1;
							} else {
								compare_sel[sel->dim_num] = -2;
							}
						}
						if(stride[sel->dim_num] > 1) {
							has_stride = 1;
						}
						tmpf = (float)fabs(((float)sel->u.sub.stride));
						n_elem = (int)(fabs(((float)(finish[sel->dim_num] -start[sel->dim_num])))/tmpf) + 1;
						if(sel->dim_num != i) {
							has_reorder = 1;
						}
						index_map[i] = sel->dim_num;
						break;
					case Ncl_VECSUBSCR:
						if((sel->u.vec.min < 0 ) || (sel->u.vec.min >= thefile->file.var_info[index]->dim_sizes[sel->dim_num])){
							NhlPError(NhlFATAL,NhlEUNKNOWN,	"Vector subscript out of range, error in subscript #%d",i);
							return(NhlFATAL);
						}
						if((sel->u.vec.max < 0)||(sel->u.vec.max >= thefile->file.var_info[index]->dim_sizes[sel->dim_num])) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,	"Vector subscript out of range, error in subscript #%d",i);
							return(NhlFATAL);
						}
						n_elem = sel->u.vec.n_ind;
						stride[sel->dim_num] = 0;
						start[sel->dim_num] = finish[sel->dim_num] = sel->u.vec.ind[0];
						has_vectors = 1;
						index_map[i] = sel->dim_num;
						if(sel->dim_num != i) {
							has_reorder = 1;
						}
						compare_sel[sel->dim_num] = 0;
						break;
					}
					multiplier_target[sel->dim_num] = 1;
					if(sel->dim_num != n_dims_target - 1) {
						for(k = sel->dim_num +1 ; k< n_dims_target; k++) {
							multiplier_target[sel->dim_num] *= (long)thefile->file.var_info[index]->dim_sizes[k];
						}
					}
					selection_dim_sizes[i] =n_elem;
					total_elements = total_elements * n_elem;
					sel++;
				}
				sel = sel_ptr->selection;
			} else {
				for(i = 0 ; i < n_dims_target; i++) {
					start[i] = 0;
					finish[i] = thefile->file.var_info[index]->dim_sizes[i] -1;
					stride[i] = 1;
					index_map[i] = i;
					total_elements *= thefile->file.var_info[index]->dim_sizes[i];
					selection_dim_sizes[i] = thefile->file.var_info[index]->dim_sizes[i];
					compare_sel[i] = -1;
					multiplier_target[i] = 1;
					for(k = i + 1; k < n_dims_target; k++) {
						multiplier_target[i] *= (long)thefile->file.var_info[index]->dim_sizes[k];
					}
				}
				sel = NULL;
			}
			n_dims_selection = n_dims_target;		
			i = 0;
			while((i < n_dims_selection)&&(n_dims_selection > 1)) {
				if(selection_dim_sizes[i] == 1) {
					for(j = i ; j < n_dims_selection -1;j++) {
						selection_dim_sizes[j] = selection_dim_sizes[j+1];
					}
					n_dims_selection--;
				} else {
					i++;
				}
			}
			if(value->multidval.kind != SCALAR) {
				for(i = 0; i< n_dims_selection; i++) {
					if(selection_dim_sizes[index_map[i]] != value->multidval.dim_sizes[i]) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension sizes of left hand side do not match right hand side");
						return(NhlFATAL);
					}
				}
			} 
			lhs_type = _NclBasicDataTypeToObjType(thefile->file.var_info[index]->data_type);
	
			rhs_type = value->obj.obj_type_mask & NCL_VAL_TYPE_MASK;

			has_missing = (FileIsVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT)) > -1 ? 1 :  0);

			if(lhs_type != rhs_type) {
				if(has_missing) {
					mis_md = FileReadVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
					tmp_md = _NclCoerceData(value,lhs_type,(NclScalar*)mis_md->multidval.val);
				} else {
					tmp_md = _NclCoerceData(value,lhs_type,NULL);
				}
			} else {
				if((has_missing)&&(value->multidval.missing_value.has_missing)) {
					mis_md = FileReadVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),NULL);
					if(value->obj.status != PERMANENT) {
						tmp_md = value;
						_NclResetMissingValue(tmp_md,(NclScalar*) mis_md->multidval.val);
					} else {
						tmp_md = _NclCopyVal(value,(NclScalar*)mis_md->multidval.val);
					}
				} else if(value->multidval.missing_value.has_missing) {
					tmp_mis = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
					*tmp_mis = value->multidval.missing_value.value;
					mis_md = _NclCreateVal(
						NULL,
						NULL,
						lhs_type,
						0,
						(void*)tmp_mis,
						NULL,
						1,
						&tmp_size,
						TEMPORARY,
						NULL);
					FileWriteVarAtt(thefile,var,NrmStringToQuark(NCL_MISSING_VALUE_ATT),mis_md,NULL);
					tmp_md = value;
				} else {
					tmp_md = value;
				}
			}
			if(tmp_md == NULL) {
				return(NhlFATAL);
			}
			if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var != NULL:thefile->file.format_funcs->write_coord != NULL ) {
				if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(value->multidval.kind != SCALAR)) {
					if(type == FILE_VAR_ACCESS) {
						ret = (*thefile->file.format_funcs->write_var)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish,	
							stride);
					} else {
						ret = (*thefile->file.format_funcs->write_coord)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish,	
							stride);
					}
						return(ret);
				} else {
					if(value->multidval.kind != SCALAR) {
						val = tmp_md->multidval.val;
						from = 0;
						block_write_limit = n_dims_target -1;
						for(i = n_dims_target - 1; i >= 0; i--) {
							if((compare_sel[index_map[i]] != -2)||(index_map[i] != i)) {
								block_write_limit = i;
								break;
							}
						}
					} else {
						block_write_limit = n_dims_target -1;
						val = tmp_md->multidval.val;
						from = 0;
					}
					n_elem_block = 1;
					for(i = 0; i < n_dims_target; i++) {
						current_index[index_map[i]] = start[index_map[i]];
						if(i > block_write_limit) {
							n_elem_block *= selection_dim_sizes[i];
							current_finish[i] = finish[i];
							real_stride[i] = stride[i];
						} else {
							current_finish[index_map[i]] = current_index[index_map[i]];
							real_stride[index_map[i]] = 1;
						}
					}
					while(!done) {
						if(type == FILE_VAR_ACCESS) {
							ret = (*thefile->file.format_funcs->write_var) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish,
								real_stride);
						} else {
							ret = (*thefile->file.format_funcs->write_coord) (
								thefile->file.private_rec,
								var,
								(void*)&(((char*)val)[from]),
								current_index,
								current_finish,
								real_stride);
						}
						if(ret < NhlWARNING) {
							return(ret);
						}
						if(value->multidval.kind != SCALAR) {
							from += n_elem_block * _NclSizeOf(thefile->file.var_info[index]->data_type);
						}
						if(compare_sel[index_map[block_write_limit]] < 0) {
							current_index[index_map[block_write_limit]] += stride[n_dims_target-1];
							current_finish[index_map[block_write_limit]] = current_index[index_map[block_write_limit]];
						} else {
							compare_sel[index_map[block_write_limit]]++;
						}
						for( i = block_write_limit; i > 0 ; i--) {
							switch(compare_sel[index_map[i]]) {
							case -2:
								if(current_index[index_map[i]] > finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										current_index[index_map[i-1]]++;
									}
								} else {
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							case -1:
								if(current_index[index_map[i]] < finish[index_map[i]]) {
									current_index[index_map[i]] = start[index_map[i]];
									if(compare_sel[index_map[i-1]] < 0) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									inc_done =1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							default:
								if(compare_sel[index_map[i]] >= sel[index_map[i]].u.vec.n_ind) {
									compare_sel[index_map[i]] = 0;
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[0];
									if(compare_sel[index_map[i-1]] < 0 ) {
										current_index[index_map[i-1]] += stride[index_map[i-1]];
									} else {
										compare_sel[index_map[i-1]]++;
									}
								} else {
									current_index[index_map[i]] = sel[index_map[i]].u.vec.ind[compare_sel[index_map[i]]];
									inc_done = 1;
								}
								current_finish[index_map[i]] = current_index[index_map[i]];
								break;
							}
							if(inc_done) {
								inc_done = 0;
								break;
							}
						}
						switch(compare_sel[index_map[0]]) {
						case -2:
							if(current_index[index_map[0]] > finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						case -1:
							if(current_index[index_map[0]] < finish[index_map[0]])
								done = 1;
							current_finish[index_map[0]] = current_index[index_map[0]];
							break;
						default:
							if(compare_sel[index_map[0]] >= sel[0].u.vec.n_ind) {
								done = 1;
							} else {
								current_index[index_map[0]] = sel[0].u.vec.ind[compare_sel[index_map[0]]];
							}
							current_finish[index_map[0]] = current_index[index_map[0]];
						}
					}	
				}
			} else if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var_ns != NULL : thefile->file.format_funcs->write_coord_ns != NULL) {
				if((!has_vectors)&&(!has_reverse)&&(!has_reorder)&&(!has_stride)) {	
					if(type == FILE_VAR_ACCESS) {
						ret = (*thefile->file.format_funcs->write_var_ns)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish
							);
					} else {
						ret = (*thefile->file.format_funcs->write_coord_ns)(
							thefile->file.private_rec,
							var,
							tmp_md->multidval.val,
							start,
							finish
							);
					}
					return(ret);
				}else{
/*
* Need code here
*/
				}
			}
		} else {
			if(type == FILE_COORD_VAR_ACCESS) {
				if((dindex = FileIsDim(thefile,var)) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension in file (%s), can not add coordinate variable",NrmQuarkToString(var),NrmQuarkToString(thefile->file.fpath));
					return(NhlFATAL);
				} else if(thefile->file.file_dim_info[dindex]->dim_size == value->multidval.dim_sizes[0]){
					if(value->multidval.n_dims != 1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables must be single dimension arrays, attempt to assign (%d) dimension value to coordinate variable",value->multidval.n_dims); 
						return(NhlFATAL);
					}
					new_dim_quarks[0] = var;
					new_dim_sizes[0] = value->multidval.dim_sizes[0];
					start[0] = 0;
					finish[0] = value->multidval.dim_sizes[0] -1 ;
					stride[0] = 1;
				}
			} else {
/*
* Since it is imposible to guess names of dimensions they must
* blindly be added
*/
				if(dim_names == NULL) {
					for(i = 0; i < value->multidval.n_dims; i++) {
						sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
						new_dim_quarks[i] = NrmStringToQuark(buffer);
						new_dim_sizes[i] = (long)value->multidval.dim_sizes[i];
						start[i] = 0;
						finish[i] = value->multidval.dim_sizes[i] -1;
						stride[i] = 1;

						ret = (*thefile->file.format_funcs->add_dim)(
							thefile->file.private_rec,
							new_dim_quarks[i],
							new_dim_sizes[i]);
						if(ret < NhlWARNING) {
							return(ret);
						}
						thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,new_dim_quarks[i]);
						thefile->file.n_file_dims++;
				
					}
				} else {
					for(i = 0 ; i < value->multidval.n_dims; i++) {
						if(dim_names[i] != -1) {
							new_dim_quarks[i] = dim_names[i];
						} else {
							sprintf(buffer,"ncl%d",thefile->file.n_file_dims);
							new_dim_quarks[i] = NrmStringToQuark(buffer);
						}
						new_dim_sizes[i] = value->multidval.dim_sizes[i];
						start[i] = 0;
						finish[i] = value->multidval.dim_sizes[i] - 1;
						stride[i] = 1;
						if((dindex = FileIsDim(thefile,dim_names[i])) == -1) {
							ret = (*thefile->file.format_funcs->add_dim)(
								thefile->file.private_rec,
								new_dim_quarks[i],
								new_dim_sizes[i]);
							if(ret < NhlWARNING) 
								return(ret);
							thefile->file.file_dim_info[thefile->file.n_file_dims] = (*thefile->file.format_funcs->get_dim_info)(thefile->file.private_rec,new_dim_quarks[i]);
							thefile->file.n_file_dims++;
						} else {
							if(thefile->file.file_dim_info[dindex]->dim_size != value->multidval.dim_sizes[i]) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"File dimension conflict, dimension (%s) has a size of (%d) can not set it to requested size (%d)",NrmQuarkToString(dim_names[i]),thefile->file.file_dim_info[dindex]->dim_size,value->multidval.dim_sizes[i]);
								return(NhlFATAL);
			
							}
						}
					}
				}
			}
/*
* Make sure data can be written
*/
			data_type = (*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type);
			if(data_type == NULL) {
				from_type = value->multidval.data_type;
				to_type = _NclPromoteType(from_type);
				while((from_type != to_type )&&((*thefile->file.format_funcs->map_ncl_type_to_format)(to_type)==NULL)) {
					from_type = to_type;
					to_type = _NclPromoteType(from_type);
				}
				obj_type = _NclBasicDataTypeToObjType(to_type);
				tmp_md = _NclCoerceData(value,obj_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempting to write variable (%s) of type (%s) which is not representable in the format of file (%s)",
						NrmQuarkToString(var),
						_NclBasicDataTypeToName(value->multidval.data_type),
						thefile->file.fname);
					return(NhlFATAL);
				} else {
					ret = (*thefile->file.format_funcs->add_var)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.data_type,
						tmp_md->multidval.n_dims,
						new_dim_quarks,
						new_dim_sizes
					);
				}
			} else {

				ret = (*thefile->file.format_funcs->add_var)(
					thefile->file.private_rec,
					var,
					value->multidval.data_type,
					value->multidval.n_dims,
					new_dim_quarks,
					new_dim_sizes
				);
				tmp_md = value;
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
			if((type == FILE_VAR_ACCESS) ? thefile->file.format_funcs->write_var != NULL : thefile->file.format_funcs->write_coord != NULL) {
				if(type == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->write_var)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish,
						stride);
				} else {
					ret = (*thefile->file.format_funcs->write_coord)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish,
						stride);
				}
				if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
					_NclDestroyObj((NclObj)tmp_md);
			} else {
				if(type == FILE_VAR_ACCESS) {
					ret = (*thefile->file.format_funcs->write_var_ns)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish);
				} else {
					ret = (*thefile->file.format_funcs->write_coord_ns)(
						thefile->file.private_rec,
						var,
						tmp_md->multidval.val,
						start,
						finish);
				}
				if((tmp_md!=value)&&(tmp_md->obj.status != PERMANENT))
					_NclDestroyObj((NclObj)tmp_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
			thefile->file.var_info[thefile->file.n_vars] = (*thefile->file.format_funcs->get_var_info)(thefile->file.private_rec,var);
			thefile->file.var_att_info[thefile->file.n_vars] = NULL;
			thefile->file.var_att_ids[thefile->file.n_vars] = -1;
			
			thefile->file.n_vars++;
			return(NhlNOERROR);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"FileWriteVar: file (%s) was opened for reading only, can not write",NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}
static NhlErrorTypes FileWriteCoord
#if  __STDC__
(NclFile thefile, NclQuark coord_name, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, value, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	int dindex;
	int index;
	

	dindex = FileIsDim(thefile,coord_name);
	if(dindex > -1) {
		ret = MyFileWriteVar(thefile,coord_name,value,sel_ptr,NULL,FILE_COORD_VAR_ACCESS);
		if(thefile->file.coord_vars[dindex] == NULL) {
			index = FileIsVar(thefile,coord_name);
			if(index > -1) {
				thefile->file.coord_vars[dindex] = thefile->file.var_info[index];
			} 
		}	
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a valid dimension in file (%s), can't write coord_var",NrmQuarkToString(coord_name),NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}

static NhlErrorTypes FileWriteVarVar
#ifdef NhlNeedProto
(NclFile thefile, NclQuark lhs_var, struct _NclSelectionRecord * lhs_sel_ptr, struct _NclVarRec* rhs_var, struct _NclSelectionRecord *rhs_sel_ptr)
#else
(thefile , lhs_var, lhs_sel_ptr, rhs_var, rhs_sel_ptr)
NclFile thefile;
NclQuark lhs_var;
struct _NclSelectionRecord *lhs_sel_ptr;
struct _NclVarRec* rhs_var;
struct _NclSelectionRecord *rhs_sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	struct _NclVarRec* tmp_var;
	int i;
	NclQuark dim_names[NCL_MAX_DIMENSIONS];
	int tmp_att_id;
	NclAtt theatt;
	NclVar coord_var;
	NclAttList *step;
	

	tmp_var = _NclVarRead(rhs_var,rhs_sel_ptr);
	for ( i = 0; i < tmp_var->var.n_dims; i++) {
		dim_names[i] = tmp_var->var.dim_info[i].dim_quark;
	}
	ret = MyFileWriteVar(thefile,lhs_var,(NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id),lhs_sel_ptr,dim_names,FILE_VAR_ACCESS);
	if(ret < NhlWARNING) {
		return(ret);
	}
	if(rhs_var->var.att_id != -1) {
		theatt = (NclAtt)_NclGetObj(rhs_var->var.att_id);
		step = theatt->att.att_list;
		while(step != NULL) {
			ret = FileWriteVarAtt(thefile,lhs_var,step->quark,step->attvalue,NULL);
			if(ret < NhlWARNING)
				return(ret);
			step = step->next;
		}
	}
	for(i = 0; i < rhs_var->var.n_dims; i++) {
		if(rhs_var->var.coord_vars[i] != -1) {
			tmp_var = (NclVar)_NclGetObj(rhs_var->var.coord_vars[i]);
			ret = FileWriteCoord(thefile,rhs_var->var.dim_info[i].dim_quark,_NclVarValueRead(tmp_var,NULL,NULL),NULL);
			if(ret < NhlWARNING)
				return(ret);
		}
	}
	return(ret);
}


static NhlErrorTypes FileWriteVar
#if  __STDC__
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, value, sel_ptr)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	return(MyFileWriteVar(thefile,var,value,sel_ptr,NULL,FILE_VAR_ACCESS));
}


static NhlErrorTypes FileWriteVarAtt
#if  __STDC__
(NclFile thefile, NclQuark var, NclQuark attname,struct _NclMultiDValDataRec* value, struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, attname,value, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	int exists;
	NclMultiDValData tmp_att_md,tmp_md;
	int att_id;
	NhlErrorTypes ret = NhlNOERROR;
	int index = -1;
	NclFileAttInfoList *step;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	
	index = FileIsVar(thefile,var);
	if(index > -1) {
		if(thefile->file.var_att_ids[index] == -1) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			step = thefile->file.var_att_info[index];
			while(step != NULL){
				tmp_att_md = FileReadVarAtt(thefile,var,step->the_att->att_name_quark,NULL);
				if(tmp_att_md != NULL) {
					if(tmp_att_md->obj.status == TEMPORARY){
						_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_att_md,NULL);
					} else {
						tmp_att_md = _NclCopyVal(tmp_att_md, NULL);
						_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_att_md,NULL);
					}
				}
				step = step->next;
			}
			thefile->file.var_att_ids[index] = att_id;
		}  else {
			att_id = thefile->file.var_att_ids[index];
		}
/*
* Hereis the trick. It is easier to let the _NclAddAtt... functions deal
* with the coercion than to figure out what it should be 
*/
		exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
		if((exists)&&(thefile->file.format_funcs->write_att != NULL))  {
			ret = (*thefile->file.format_funcs->write_var_att)(
				thefile->file.private_rec,
				var,
				attname,
				value->multidval.val
			);
			return(ret);
		} else if((!exists)&&(thefile->file.format_funcs->add_att != NULL)){
			if(value->multidval.data_type == NCL_char) {	
				tmp_md = _NclCharMdToStringMd(value);
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
				if(ret < NhlWARNING) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list",NrmQuarkToString(attname));
					return(NhlFATAL);
				}
				ret = (*thefile->file.format_funcs->add_var_att)(
					thefile->file.private_rec,
					var,
					attname,
					value->multidval.data_type,
					value->multidval.totalelements,
					value->multidval.val
				);
				if(ret > NhlWARNING) {
					AddAttInfoToList(&(thefile->file.var_att_info[index]), (*thefile->file.format_funcs->get_var_att_info)(thefile->file.private_rec,var,attname));
					return(ret);
				} else {
					_NclDeleteAtt(att_id,NrmQuarkToString(attname));
				}
			} else {
				if((*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type) == NULL)  {
					if(value->multidval.data_type == NCL_string) {
						tmp_md = _NclStringMdToCharMd(value);
						return(_NclFileWriteVarAtt(thefile,var,attname,tmp_md,sel_ptr));
					} else {
						from_type = value->multidval.data_type;
						to_type = _NclPromoteType(from_type);
						while((from_type != to_type)&&((*thefile->file.format_funcs->map_ncl_type_to_format)(to_type)==NULL)) {
							from_type = to_type;
							to_type = _NclPromoteType(from_type);
						}
						if((*thefile->file.format_funcs->map_ncl_type_to_format)(to_type)==NULL) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"The type (%s) is not representable as an attribute in the file (%s)",_NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->file.fpath));
							return(NhlFATAL);
						} else {
							obj_type = _NclBasicDataTypeToObjType(to_type);
							tmp_md = _NclCoerceData(value,obj_type,NULL);
						}
					}
					
				} else {
					tmp_md = value;
				}
				ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
				if(ret < NhlWARNING) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list",NrmQuarkToString(attname));
					return(NhlFATAL);
				}
				ret = (*thefile->file.format_funcs->add_var_att)(
					thefile->file.private_rec,
					var,
					attname,
					tmp_md->multidval.data_type,
					tmp_md->multidval.totalelements,
					tmp_md->multidval.val
				);
				if(ret > NhlWARNING) {
					AddAttInfoToList(&(thefile->file.var_att_info[index]), (*thefile->file.format_funcs->get_var_att_info)(thefile->file.private_rec,var,attname));
					return(ret);
				} else {
					_NclDeleteAtt(att_id,NrmQuarkToString(attname));
				}
			}
			return(ret);
		} else {
			return(NhlFATAL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a variable in file (%s)",NrmQuarkToString(var),NrmQuarkToString(thefile->file.fname));
	}
	return(NhlFATAL);
}
static struct _NclMultiDValDataRec *FileReadAtt
#if  __STDC__
(NclFile thefile, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int aindex;
	int att_id = -1,i;
	void *val;
	NclMultiDValData tmp_md;
	
	aindex = FileIsAtt(thefile,attname);
	if(aindex > -1) {
		if(thefile->file.file_atts_id != -1) {
			return(_NclGetAtt(thefile->file.file_atts_id,NrmQuarkToString(attname),sel_ptr));
		}
		if(thefile->file.format_funcs->read_att != NULL) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			for(i = 0; i < thefile->file.n_file_atts; i++){
				val = NclMalloc(_NclSizeOf(thefile->file.file_atts[i]->data_type)* thefile->file.file_atts[i]->num_elements );
				(void)(*thefile->file.format_funcs->read_att)(
					thefile->file.private_rec,
					thefile->file.file_atts[i]->att_name_quark,
					val
					);
				tmp_md = _NclCreateVal(
						NULL,
						NULL,
						_NclBasicDataTypeToObjType(thefile->file.file_atts[i]->data_type),
						0,
						val,
						NULL,
						1,
						&thefile->file.file_atts[i]->num_elements,
						TEMPORARY,
						NULL);
				if(tmp_md != NULL) {
					_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_md,NULL);
				}
			}
			if(att_id != -1) {	
				thefile->file.file_atts_id = att_id;
				return(_NclGetAtt(thefile->file.file_atts_id,NrmQuarkToString(attname),sel_ptr));
			}
		}
	}
	return(NULL);
}

static NhlErrorTypes FileWriteAtt
#if  __STDC__
(NclFile thefile, NclQuark attname, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, value, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int i,exists;
	NclMultiDValData tmp_att_md,tmp_md;
	int att_id;
	NhlErrorTypes ret = NhlNOERROR;
	NclBasicDataTypes from_type,to_type;
	NclObjTypes obj_type;
	

	if(thefile->file.file_atts_id == -1) {
		att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
		for(i= 0; i < thefile->file.n_file_atts; i++) {
			tmp_att_md = FileReadAtt(thefile,thefile->file.file_atts[i]->att_name_quark,NULL);
			if(tmp_att_md != NULL) {
				if(tmp_att_md->obj.status == TEMPORARY){
					_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_att_md,NULL);
				} else {
					tmp_att_md = _NclCopyVal(tmp_att_md, NULL);
					_NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_att_md,NULL);
				}
			}
		}
		thefile->file.file_atts_id = att_id;
	}  else {
		att_id = thefile->file.file_atts_id;
	}
	exists = _NclIsAtt(att_id,NrmQuarkToString(attname));
	if((exists)&&(thefile->file.format_funcs->write_att != NULL))  {
/*
* Hereis the trick. It is easier to let the _NclAddAtt... functions deal
* with the coercion than to figure out what it should be 
*/
		ret = _NclAddAtt(att_id,NrmQuarkToString(attname),value,sel_ptr);
		if(ret < NhlWARNING) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
			return(NhlFATAL);
		}
		tmp_att_md = _NclGetAtt(att_id,NrmQuarkToString(attname),NULL);
		ret = (*thefile->file.format_funcs->write_att)(
			thefile->file.private_rec,
			attname,
			tmp_att_md->multidval.val
		);
		return(ret);
	} else if((!exists)&&(thefile->file.format_funcs->add_att != NULL)){
		if(value->multidval.data_type == NCL_char) {
			tmp_md = _NclCharMdToStringMd(value);
			ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
               		if(ret < NhlWARNING) {
                        	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
                        	return(NhlFATAL);
                	}
			ret = (*thefile->file.format_funcs->add_att)(
				thefile->file.private_rec,
				attname,
				value->multidval.data_type,
				value->multidval.totalelements,
				value->multidval.val
			);
			if(ret > NhlWARNING) {
				thefile->file.file_atts[thefile->file.n_file_atts] = (*thefile->file.format_funcs->get_att_info)(thefile->file.private_rec,attname);
				if(thefile->file.file_atts[thefile->file.n_file_atts] != NULL) {
					thefile->file.n_file_atts++;
				}
				return(ret);
			} else {
				_NclDeleteAtt(att_id,NrmQuarkToString(attname));
			}
		} else {
			if((*thefile->file.format_funcs->map_ncl_type_to_format)(value->multidval.data_type) == NULL)  {
				if(value->multidval.data_type == NCL_string) {
					tmp_md = _NclStringMdToCharMd(value);
					return(_NclFileWriteAtt(thefile,attname,tmp_md,sel_ptr));
				} else {
					from_type = value->multidval.data_type;
					to_type = _NclPromoteType(from_type);
					while((from_type != to_type )&&((*thefile->file.format_funcs->map_ncl_type_to_format)(to_type)==NULL)) {
						from_type = to_type;
						to_type = _NclPromoteType(from_type);
					}
					if((*thefile->file.format_funcs->map_ncl_type_to_format)(to_type)==NULL)  {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"The type (%s) is not representable as an attribute in the file (%s)",_NclBasicDataTypeToName(to_type),NrmQuarkToString(thefile->file.fpath));
                                                 return(NhlFATAL);

					} else {
						obj_type = _NclBasicDataTypeToObjType(to_type);
						tmp_md = _NclCoerceData(value,obj_type,NULL);
					}
				}
			} else {
				tmp_md= value;
			}
			ret = _NclAddAtt(att_id,NrmQuarkToString(attname),tmp_md,sel_ptr);
               		if(ret < NhlWARNING) {
                        	NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not write attribute (%s) to attribute list", NrmQuarkToString(attname));
                        	return(NhlFATAL);
                	}
			ret = (*thefile->file.format_funcs->add_att)(
				thefile->file.private_rec,
				attname,
				tmp_md->multidval.data_type,
				tmp_md->multidval.totalelements,
				tmp_md->multidval.val
			);
			if((tmp_md != value)&&(tmp_md->obj.status != PERMANENT)) {
				_NclDestroyObj((NclObj)tmp_md);
			}
			if(ret > NhlWARNING) {
				thefile->file.file_atts[thefile->file.n_file_atts] = (*thefile->file.format_funcs->get_att_info)(thefile->file.private_rec,attname);
				if(thefile->file.file_atts[thefile->file.n_file_atts] != NULL) {
					thefile->file.n_file_atts++;
				}
				return(ret);
			} else {
				_NclDeleteAtt(att_id,NrmQuarkToString(attname));
			}
		}
		return(ret);
	} 
	return(NhlFATAL);
}


static struct _NclMultiDValDataRec* FileVarReadDim
#if  __STDC__
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile, var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	int index;
	int i;
	int *tmpi;
	NclQuark *tmpq;
	int output_dim_sizes = 1;
	
	
	index = FileIsVar(thefile,var);
	if(index > -1) {
		if(dim_name > -1) {
			for( i=0; i < thefile->file.var_info[index]->num_dimensions; i++) {
				if(FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[i]) == dim_name) {
					tmpi = (int*)NclMalloc(sizeof(int));
					*tmpi = i;
					return( _NclCreateVal(
						NULL,
						NULL,
						Ncl_MultiDValintData,
						0,
						(void*)tmpi,
						NULL,
						1,
						&output_dim_sizes,
						TEMPORARY,
						NULL));
				}
			}
		} else if ( dim_num > -1) {
			if(dim_num < thefile->file.var_info[index]->num_dimensions) {
				tmpq = (NclQuark*)NclMalloc(sizeof(NclQuark));
				*tmpq = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[dim_num]);	
				return( _NclCreateVal(
					NULL,
					NULL,
					Ncl_MultiDValstringData,
					0,
					(void*)tmpq,
					NULL,
					1,
					&output_dim_sizes,
					TEMPORARY,
					NULL));
			}
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension number (%d) is out of range for variable (%s->%s)",dim_num,NrmQuarkToString(thefile->file.fname),NrmQuarkToString(var));
			return(NULL);
		} 
	}
	return(NULL);
}
/*
* dim_num is dim number of variable and dim_name is new name
*/
static NhlErrorTypes FileVarWriteDim
#if  __STDC__
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile,var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	int index;
	NclQuark old_name;
	
	index = FileIsVar(thefile,var);
	if(index > -1) {
		if((dim_num > -1)&&(dim_num < thefile->file.var_info[index]->num_dimensions)) {
			old_name = FileGetDimName(thefile,thefile->file.var_info[index]->file_dim_num[dim_num]);
			if(thefile->file.format_funcs->rename_dim != NULL) {
				if((*thefile->file.format_funcs->rename_dim)(
					thefile->file.private_rec,
					old_name,
					dim_name) < NhlWARNING) {
					
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						"Cannot rename dimension (%d) in variable (%s)",
						dim_num,
						NrmQuarkToString(var));
					return(NhlFATAL);
				} else {
					thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[dim_num]]->dim_name_quark = dim_name;
					return(NhlNOERROR);
				}
			}
		}
	}
	return(NhlFATAL);
}

static struct _NclMultiDValDataRec* FileReadDim
#if  __STDC__
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	int i = 0;
	NclQuark *tmps;
	int *tmpl;
	int output_dim_sizes = 1;
	if(dim_name != -1) {
		for(i =0; i< thefile->file.n_file_dims; i++) {
			if(thefile->file.file_dim_info[i]->dim_name_quark) {
				tmpl = (int*)NclMalloc(sizeof(int));
				*tmpl = i;
				return( _NclCreateVal(
					NULL,
					NULL,
					Ncl_MultiDValintData,
					0,
					(void*)tmpl,
					NULL,
					1,
					&output_dim_sizes,
					TEMPORARY,
					NULL));
			}
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a defined dimension in file (%s)",NrmQuarkToString(dim_name),NrmQuarkToString(thefile->file.fname));
		return(NULL);
	} else if(dim_num > -1){
		if(dim_num < thefile->file.n_file_dims) {
			tmps = (NclQuark*)NclMalloc(sizeof(NclQuark));
			*tmps = thefile->file.file_dim_info[dim_num]->dim_name_quark;
			return( _NclCreateVal(
				NULL,
				NULL,
				Ncl_MultiDValstringData,
				0,
				(void*)tmps,
				NULL,
				1,
				&output_dim_sizes,
				TEMPORARY,
				NULL));
		}
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension #%ld is out of range",dim_num);
		return(NULL);

	} else {
		return(NULL);
	}
}

/*
* dim_num is the number of the dimension dim_name is the name to change it to
*/
static NhlErrorTypes FileWriteDim
#if  __STDC__
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	if((dim_num > -1)&&(dim_num < thefile->file.n_file_dims)) {
		if(thefile->file.format_funcs->rename_dim != NULL) {
			if((*thefile->file.format_funcs->rename_dim)(
				thefile->file.private_rec,
				thefile->file.file_dim_info[dim_num]->dim_name_quark,
				dim_name)  < NhlWARNING) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not change dimension (%d) to (%s) for file (%s)",dim_num,NrmQuarkToString(dim_name),NrmQuarkToString(thefile->file.fname));
				return(NhlFATAL);
			} else {
				thefile->file.file_dim_info[dim_num]->dim_name_quark = dim_name;
				return(NhlNOERROR);
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) is out of range for file (%s)",dim_num,NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);
	}
}

static struct _NclVarRec* FileReadCoord
#if  __STDC__
(NclFile thefile, NclQuark coord_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclSelection *sel;
	int index;
	NclMultiDValData tmp_md,tmp_att_md;
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	int att_id = -1;
	NclObj att_obj;
	NclVar tmp_var = NULL;
	NclFileAttInfoList *step;

	if(FileIsCoord(thefile,coord_name) > -1){
		index = FileIsVar(thefile,coord_name);
		tmp_md = MyFileReadVarValue(thefile,coord_name,sel_ptr,dim_info,
			FILE_COORD_VAR_ACCESS);
		if(thefile->file.var_att_ids[index] != -1) {
			att_id = thefile->file.var_att_ids[index];
			att_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
			if(att_obj != NULL) {
				att_id = att_obj->obj.id;
			} else {
				att_id = -1;
			}
		} else if(thefile->file.var_att_info[index] != NULL) {
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
			step = thefile->file.var_att_info[index];
			while(step != NULL) {
				tmp_att_md = FileReadVarAtt(thefile,thefile->file.var_info[index]->var_name_quark,step->the_att->att_name_quark,NULL);
				if(tmp_att_md != NULL) {
					if(tmp_att_md->obj.status == TEMPORARY){
						_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_att_md,NULL);
					} else {
						tmp_att_md = _NclCopyVal(tmp_att_md, NULL);
						_NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_att_md,NULL);
					}
				}
				step = step->next;
			}
		} else {
			att_id = -1;
		}		
		if(sel_ptr != NULL) {
			sel = sel_ptr->selection;
		} else {
			sel = NULL;
		}
		if(tmp_md != NULL) {
			tmp_var = _NclVarCreate(
					NULL,
					NULL,
					Ncl_Var,
					0,
					NULL,
					tmp_md,
					dim_info,
					att_id,
					NULL,
					(sel == NULL ? COORD : COORDSUBSEL),
					NrmQuarkToString(coord_name));
			if(tmp_var == NULL) {
				_NclDestroyObj((NclObj)tmp_md);
				if(att_id != -1) {
					_NclDestroyObj((NclObj)_NclGetObj(att_id));
				}
			}
		}
		return(tmp_var);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a coordinate variable for file (%s)",NrmQuarkToString(coord_name),NrmQuarkToString(thefile->file.fname));
	}
	return(NULL);
}



static NclQuark FileGetDimName
#if  __STDC__
(NclFile thefile, int num)
#else
(thefile, num)
NclFile thefile;
int num;
#endif
{
	if((num > -1)&&(num < thefile->file.n_file_dims)) {
		return(thefile->file.file_dim_info[num]->dim_name_quark);
	} else {
		return(-1);
	}
}

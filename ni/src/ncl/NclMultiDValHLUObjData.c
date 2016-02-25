
/*
 *      $Id: NclMultiDValHLUObjData.c,v 1.25 2010-01-11 21:36:19 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Mon  29 Apr 1994
 *
 *	Description:	
 */

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#include "defs.h"
#include <errno.h>
#include "NclHLUObj.h"
#include "NclMultiDValHLUObjData.h"
#include "DataSupport.h"
#include "HLUSupport.h"
#include "NclTypeobj.h"
#include "NclVar.h"
#include "VarSupport.h"
#include <math.h>

/*
* udata is the record containing the index and the parent_id.
* cbdata contains the id of the HLUObj child being deleted
* 	if the value at the index doesn't match the nothing is done
*/

static void MultiDVal_HluObj_DestroyNotify
#if 	NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata,udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	NclMultiDValHLUObjData self_md= NULL; 
	obj value = (obj) cbdata.lngval;
	HLUMDCalRec *crec = ( HLUMDCalRec *) udata.ptrval;
	int parent = crec->parent_id;
	int index = crec->index;
	NclScalar mis;
	NclMultiDValData tmp_md = NULL;
	obj *obj_ids = NULL;
	NclRefList *plptr;
	NclScalar *tmp_mis;
	ng_size_t dim_size = 1;
	int replaced = 0;
	NclObj pobj;
	NhlArgVal vcbdata;
	NhlArgVal vcselector;

	self_md = (NclMultiDValHLUObjData)_NclGetObj(parent);
	
	if(self_md != NULL) {
		obj_ids = (obj*)self_md->multidval.val;
		if(self_md->multidval.missing_value.has_missing) {
			mis = self_md->multidval.missing_value.value;
		} else {
			mis = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis;
			replaced = 1;
		}
		if(obj_ids[index] == value) {
			vcselector.lngval = HLUVALCHANGE;
			vcbdata.ptrval = NclMalloc(sizeof(NclHLUCbData));
			((NclHLUCbData*)vcbdata.ptrval)->ncl_id = mis.objval;
			((NclHLUCbData*)vcbdata.ptrval)->off = index;
			((NclHLUCbData*)vcbdata.ptrval)->prev_id = obj_ids[index];
			((NclHLUCbData*)vcbdata.ptrval)->kind = 1;
			_NhlCBCallCallbacks(self_md->obj.cblist,vcselector,vcbdata);
			NclFree(vcbdata.ptrval);


			obj_ids[index] = mis.objval;
			_NhlCBDelete(self_md->multi_obj.cbs[index]);
			self_md->multi_obj.cbs[index] = NULL;
			NclFree(self_md->multi_obj.crecs[index]); 
			self_md->multi_obj.crecs[index] = NULL;
			if((self_md->obj.ref_count > 0)&&(replaced)) {
				plptr = self_md->obj.parents;
				tmp_mis = NclMalloc((unsigned)sizeof(NclScalar));
				*tmp_mis = mis;
				tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,(void*)tmp_mis,NULL,1,&dim_size,PERMANENT,NULL,nclTypeobjClass);
				while(plptr != NULL) {
					pobj = _NclGetObj(plptr->pid);
					if((pobj!=NULL)&&(pobj->obj.obj_type_mask & Ncl_Var)) {
						_NclWriteAtt((NclVar)(pobj),NCL_MISSING_VALUE_ATT,tmp_md,NULL);
					}
					plptr = plptr->next;
				}
				_NclDestroyObj((NclObj)tmp_md);
			}
		} 
	} 
}

static _NhlCB AddDestroyNotify
#if	NhlNeedProto
(NclMultiDValHLUObjData self_md,int index)
#else
(self_md,index)
NclMultiDValHLUObjData self_md;
int index;
#endif
{
	HLUMDCalRec *crec = NclMalloc(sizeof(HLUMDCalRec));
	NhlArgVal udata;
	NhlArgVal selector;
	
	crec->parent_id = self_md->obj.id;
	crec->index = index;
	udata.ptrval = (NhlPointer)crec;
	selector.lngval = 0;
	self_md->multi_obj.crecs[index] = crec;
	return(_NclAddCallback((NclObj)self_md,NULL,MultiDVal_HluObj_DestroyNotify,HLUDESTROYED,&udata));
}
static struct _NclDataRec *MultiDVal_HluObj_ReadSection
#if	NhlNeedProto
(NclData self, NclSelectionRecord * sel,NclScalar *missing)
#else
(self,sel,missing)
	NclData self;
	NclSelectionRecord *sel;
	NclScalar *missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclData output_md;
	NclSelection *sel_ptr;
	obj *val;
	int i,k,from,to;
	NclScalar *tmp = NULL;

	long current_index[NCL_MAX_DIMENSIONS];
	long multiplier[NCL_MAX_DIMENSIONS];
	long compare_sel[NCL_MAX_DIMENSIONS];
	long strider[NCL_MAX_DIMENSIONS];
	ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];

	ng_size_t total_elements = 1;
	int n_dims_input = self_md->multidval.n_dims;
	ng_size_t n_elem=0;
	int done = 0;
	int chckmiss =0;
	int inc_done = 0;

/*
* Pre Conditions: 	number entries in selection record == n_dims
*			object is not scalar
* 			if it is a reorder only one reference to a values 
*				dimension is allowed (no duplicates!!)
*			if a missing value is passed in it is the same
* 			type as the value being selected
*
* First check dimension ranges.
*
* Check to see if vector subscripting is used.
*
* Compute total number of elements in output array.
* 
* Allocate array and perform selection, three types 
* (only normal subs,contains vector subs,reordering selection)
* 
* if no vector subscripting is used then add selection record to
* output object.
*/
	if((!self_md->multidval.missing_value.has_missing)||(missing == NULL)){
		chckmiss = 0;
	} else if((self_md->multidval.missing_value.has_missing)&&(missing != NULL)) {
		chckmiss = 1;
	}
	if(sel!= NULL){
		sel_ptr	= sel->selection;
	}
	for(i = 0 ; i < n_dims_input; i++) {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = (long)self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
*/
		case Ncl_SUBSCR:
			if(sel_ptr->u.sub.finish < sel_ptr->u.sub.start) {

				if(sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        sel_ptr->u.sub.stride = 1;
                                }

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(sel_ptr->u.sub.stride < 0){
                                        current_index[i] = sel_ptr->u.sub.finish + (sel_ptr->u.sub.start - sel_ptr->u.sub.finish) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        compare_sel[i] = -2;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                } else {
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                }


			} else {
				if(sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        sel_ptr->u.sub.stride = 1;
                                }

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;

				if(sel_ptr->u.sub.stride < 0){
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.finish - (sel_ptr->u.sub.finish - sel_ptr->u.sub.start) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        strider[i] = sel_ptr->u.sub.stride;

                                } else {
                                        compare_sel[i] = -2;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = sel_ptr->u.sub.stride;
                                }


			}
			if((sel_ptr->u.sub.start > self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}

			if((sel_ptr->u.sub.finish > self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((sel_ptr->u.vec.min < 0)|| (sel_ptr->u.vec.min > self_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			if((sel_ptr->u.vec.max < 0)|| (sel_ptr->u.vec.max > self_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			n_elem = sel_ptr->u.vec.n_ind;
			strider[i] = 0;
			current_index[i] = sel_ptr->u.vec.ind[0]; 
			compare_sel[i] = 0;
		} 
		if(sel_ptr->dim_num != n_dims_input -1) {
			multiplier[i] = 1;

			for(k = sel_ptr->dim_num + 1; k < n_dims_input ;k++) {
				multiplier[i] *= (long)self_md->multidval.dim_sizes[k];
			}
		} else {
			multiplier[i] = 1;
		}
		output_dim_sizes[i] = n_elem;
		total_elements = total_elements * n_elem;
		sel_ptr++;
	}
	sel_ptr = sel->selection;
/*
* All subscript ranges are valid. whether or not it is a reorder, and
* whether or not a vector subscript is present are known.
* also multiplier contains the appropriate multiplier by which to multiply
* an index to get the actual index in the value vector.
* the output arrays dimension sizes are known and the total number of 
* elements is known 
* the current_index array is inialized to the starting location
*
* compare_sel contains either -2 or -1 for normal subscripts or the
* index >= 0 into the integer vector array.
*/

	val = (obj*)NclMalloc(total_elements * sizeof(obj));
	to = 0;
	while(!done) {
		from = 0;
		for(i = 0; i < n_dims_input;i++) {
			from = from + (current_index[i] * multiplier[i]);
		}
		if(!chckmiss) {
			val[to] = ((obj*)self_md->multidval.val)[from];
		} else {
			val[to] = (((obj*)self_md->multidval.val)[from] == missing->objval) ? missing->objval:((obj*)self_md->multidval.val)[from];
		}
		if(compare_sel[n_dims_input-1] <0) {
			current_index[n_dims_input -1 ] += strider[n_dims_input-1];
		} else {
			compare_sel[n_dims_input-1]++;
		}
		for(k = n_dims_input-1; k >0; k--) {
			switch(compare_sel[k]) {
			case -2: 
				if(current_index[k] > sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(current_index[k] < sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
/*
* Only falls through to here when vector is present because compare_sel
* is only positive when a vector is present
*/
				if(compare_sel[k] >= sel_ptr[k].u.vec.n_ind) {
					compare_sel[k] = 0;
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}

				} else {
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(compare_sel[0]) {
		case -2:
			if(current_index[0] > sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(current_index[0] < sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(compare_sel[0] >= sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				current_index[0] = sel_ptr[0].u.vec.ind[compare_sel[0]];
			}
			break;
		}
		to++;
	}
	i =0;
	while(i < n_dims_input) {
		if(output_dim_sizes[i] < 2) {
			for(k=i; k< n_dims_input-1;k++){
				output_dim_sizes[k] = output_dim_sizes[k+1];
			}
			n_dims_input--;
		} else {
			i++;
		}
	}
	if(n_dims_input == 0) {
/*
* This means every one of the output_dim_sizes was one hence SCALAR 
* subsection
*/
		n_dims_input = 1;
	}
	if((self_md->multidval.missing_value.has_missing)&&
		(missing != NULL)) {
		tmp = missing;
	} else if(self_md->multidval.missing_value.has_missing) {
		tmp = &self_md->multidval.missing_value.value;
	} else {
		tmp = NULL;
	}
	output_md = (NclData)_NclMultiDValHLUObjDataCreate(NULL,
		NULL,
                Ncl_MultiDValHLUObjData,
                0,(void*)val,	
		tmp,
		n_dims_input,output_dim_sizes,
		TEMPORARY,sel);

	return(output_md);		
}


static NhlErrorTypes MultiDVal_HLUObj_md_WriteSection
#if	NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
	NclData target;
	NclSelectionRecord *sel;
	NclData value;
#endif
{
	NclMultiDValHLUObjData target_md = (NclMultiDValHLUObjData)target;
	NclMultiDValHLUObjData value_md = (NclMultiDValHLUObjData)value;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/
	NclSelection *sel_ptr = NULL;
	obj *val;
	int i,k;
	long from,to;

	long current_index[NCL_MAX_DIMENSIONS];
	long multiplier[NCL_MAX_DIMENSIONS];
	long compare_sel[NCL_MAX_DIMENSIONS];
	long strider[NCL_MAX_DIMENSIONS];
	ng_size_t output_dim_sizes[NCL_MAX_DIMENSIONS];

	ng_size_t *dim_sizes_value = value_md->multidval.dim_sizes;
	int n_dims_value = value_md->multidval.n_dims;
	int n_dims_sel = 0;
	ng_size_t total_elements = 1;
	int n_dims_target = target_md->multidval.n_dims;
	ng_size_t n_elem=0;
	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	NclHLUObj tmp_ho = NULL;
	NhlArgVal cbdata;
	NhlArgVal selector;

/*
* preconditions:
*	target and value are same type of object
*	sel is a valid selection record with no duplicate indices and
* 	contains a selection entry for each dimension in target or it
* 	is null implying a direct copy.
*
* 	*****Value is a multidimensional array***** 
*	number of dimensions >= 1 && size > 1
*/
	selector.lngval = HLUVALCHANGE;

	if((target_md->multidval.missing_value.has_missing)&&
		(value_md->multidval.missing_value.has_missing)) {
		if(target_md->multidval.missing_value.value.objval ==
			value_md->multidval.missing_value.value.objval) {
/*
* No need to check when missing values are equal
*/
			chckmiss = 0;
		} else {
			chckmiss = 1;
		}
	} else {
		chckmiss = 0;
	}
	
	if(sel != NULL) {
		sel_ptr = sel->selection;
	} else {
		if(target_md->multidval.totalsize == value_md->multidval.totalsize) {
			cbdata.ptrval = NclMalloc(sizeof(NclHLUCbData));
			val = (obj*)value_md->multidval.val;
			for(i = 0; i< target_md->multidval.totalelements; i++) {
				if(chckmiss) {
					tmp_ho = NULL;
					if(target_md->multidval.missing_value.value.objval != ((obj*)target_md->multidval.val)[i]) {
						tmp_ho = (NclHLUObj)_NclGetObj(((obj*)target_md->multidval.val)[i]);
					} 
					if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
						if(target_md->multi_obj.cbs[i] != NULL) {
							_NhlCBDelete(target_md->multi_obj.cbs[i]);
							target_md->multi_obj.cbs[i] = NULL;
						}
						(void)_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
                			}
					tmp_ho = NULL;
					if(value_md->multidval.missing_value.value.objval != val[i]) {
						tmp_ho = (NclHLUObj)_NclGetObj((int)val[i]);
						((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)val[i];
						((NclHLUCbData*)cbdata.ptrval)->off = i;
						((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[i];
						((NclHLUCbData*)cbdata.ptrval)->kind = 0;
						((obj*)target_md->multidval.val)[i] = val[i];
/*
* ADD HLUVALCHANGE
*/
						
					} else {
						((NclHLUCbData*)cbdata.ptrval)->ncl_id = target_md->multidval.missing_value.value.objval;
						((NclHLUCbData*)cbdata.ptrval)->off = i;
						((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[i];
						((NclHLUCbData*)cbdata.ptrval)->kind = 1;
						((obj*)target_md->multidval.val)[i] = target_md->multidval.missing_value.value.objval;
/*
* ADD HLUVALCHANGE
*/
					}
					selector.lngval = HLUVALCHANGE;
					_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
					if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
			               		(void)_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
						target_md->multi_obj.cbs[i] = AddDestroyNotify(target_md,i);
                			} else {
						target_md->multi_obj.cbs[i] = NULL;
					}

				} else {
					tmp_ho = NULL;
					if(!(target_md->multidval.missing_value.has_missing)||(target_md->multidval.missing_value.value.objval != ((obj*)target_md->multidval.val)[i])) {
						tmp_ho = (NclHLUObj)_NclGetObj(((obj*)target_md->multidval.val)[i]);
					}
					if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
						if(target_md->multi_obj.cbs[i] != NULL) {
							_NhlCBDelete(target_md->multi_obj.cbs[i]);
							target_md->multi_obj.cbs[i] = NULL;
						}
						(void)_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
                			}
					tmp_ho = NULL;
					if(!(value_md->multidval.missing_value.has_missing) ||(value_md->multidval.missing_value.value.objval != val[i])) {
						tmp_ho = (NclHLUObj)_NclGetObj((int)val[i]);
					}
					if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
			               		(void)_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
						target_md->multi_obj.cbs[i] = AddDestroyNotify(target_md,i);
                			} else {
						target_md->multi_obj.cbs[i] = NULL;
					}
					((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)val[i];
					((NclHLUCbData*)cbdata.ptrval)->off = i;
					((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[i];
					((NclHLUCbData*)cbdata.ptrval)->kind = 0;
					((obj*)target_md->multidval.val)[i] = val[i];
					selector.lngval = HLUVALCHANGE;
					_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
/*
* ADD HLUVALCHANGE
*/
				}
			}
			NhlFree(cbdata.ptrval);
			return(NhlNOERROR);
		} else {
			return(NhlFATAL);
		}
	}
	for(i = 0 ; i < n_dims_target; i++) {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = (long)target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
*/
		case Ncl_SUBSCR:
			if(sel_ptr->u.sub.finish < sel_ptr->u.sub.start) {

				if(sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        sel_ptr->u.sub.stride = 1;
                                }

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	/ sel_ptr->u.sub.stride) + 1L;
/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(sel_ptr->u.sub.stride < 0){
                                        current_index[i] = sel_ptr->u.sub.finish + (sel_ptr->u.sub.start - sel_ptr->u.sub.finish) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        compare_sel[i] = -2;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                } else {
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                }


			} else {
				if(sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        sel_ptr->u.sub.stride = 1;
                                }

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	          / sel_ptr->u.sub.stride) + 1L;
				if(sel_ptr->u.sub.stride < 0){
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.finish - (sel_ptr->u.sub.finish - sel_ptr->u.sub.start) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        strider[i] = sel_ptr->u.sub.stride;

                                } else {
                                        compare_sel[i] = -2;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = sel_ptr->u.sub.stride;
                                }


			}
			if((sel_ptr->u.sub.start > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((sel_ptr->u.vec.min < 0)|| (sel_ptr->u.vec.min > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((sel_ptr->u.vec.max < 0)|| (sel_ptr->u.vec.max > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem = sel_ptr->u.vec.n_ind;
			strider[i] = 0;
			current_index[i] = sel_ptr->u.vec.ind[0]; 
			compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(sel_ptr->dim_num != n_dims_target -1) {
			multiplier[i] = 1;

			for(k = sel_ptr->dim_num + 1; k < n_dims_target;k++) {
				multiplier[i] *= (long)target_md->multidval.dim_sizes[k];
			}
		} else {
			multiplier[i] = 1;
		}
		output_dim_sizes[i] = n_elem;
		total_elements = total_elements * n_elem;
		sel_ptr++;
		if(n_elem != 1) {
			if(n_elem != dim_sizes_value[n_dims_sel]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension size mismatch on subscript #%d, left-hand and right-hand side dimensions do not match",i);
				return(NhlFATAL);
			}
			n_dims_sel++;
		}
	}
	if(total_elements != value_md->multidval.totalelements) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension sizes on right hand side of assignment do not match dimension sizes of left hand side");
		return(NhlFATAL);
	}
	if(n_dims_sel != n_dims_value) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Left-hand side and right-hand side of assignment do not have the same number of dimensions");
		return(NhlFATAL);
		
	}
	sel_ptr = sel->selection;
/*
* all dimsizes between value and selection target match. 
* all dimsizes are in valid ranges of target dimensions.
* All subscript ranges are valid.vector subscript is present is known.
* also multiplier contains the appropriate multiplier by which to multiply
* an index to get the actual index in the target vector.
* the output arrays dimension sizes are known and the total number of 
* elements is known 
* the current_index array is inialized to the starting location
*
* compare_sel contains either -2 or -1 for normal subscripts or the
* index >= 0 into the integer vector array.
*/

	val = (obj*)value_md->multidval.val;
	from = 0;
	cbdata.ptrval = NclMalloc(sizeof(NclHLUCbData));
	while(!done) {
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		if(chckmiss) {
			tmp_ho = NULL;
			if(((obj*)target_md->multidval.val)[to] != target_md->multidval.missing_value.value.objval){
				tmp_ho = (NclHLUObj)_NclGetObj(((obj*)target_md->multidval.val)[to]);
				if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
					if(target_md->multi_obj.cbs[to] != NULL) {
						_NhlCBDelete(target_md->multi_obj.cbs[to]);
						target_md->multi_obj.cbs[to] = NULL;
					}
					(void)_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
                		}
			}
			tmp_ho = NULL;
			if(val[from] != value_md->multidval.missing_value.value.objval){
				tmp_ho = (NclHLUObj)_NclGetObj(val[from]);
				if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
					(void)_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
					target_md->multi_obj.cbs[to] = AddDestroyNotify(target_md,to);
                		} else {
					target_md->multi_obj.cbs[to] = NULL;
				}
				((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)val[from];
				((NclHLUCbData*)cbdata.ptrval)->off = to;
				((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[to];
				((NclHLUCbData*)cbdata.ptrval)->kind = 0;
			} else {
				((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)val[from];
				((NclHLUCbData*)cbdata.ptrval)->off = to;
				((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[to];
				((NclHLUCbData*)cbdata.ptrval)->kind = 1;
			}
			((obj*)target_md->multidval.val)[to] = 
				((val[from] == value_md->multidval.missing_value.value.objval) ? 
				target_md->multidval.missing_value.value.objval 
				: val[from]);
			selector.lngval = HLUVALCHANGE;
			_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
/*
* ADD HLUVALCHANGE
*/
		} else {
			
			tmp_ho = NULL;
			if(!(target_md->multidval.missing_value.has_missing)||(target_md->multidval.missing_value.value.objval != ((obj*)target_md->multidval.val)[to])) {
				tmp_ho = (NclHLUObj)_NclGetObj(((obj*)target_md->multidval.val)[to]);
			}
			if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
				if(target_md->multi_obj.cbs[to] != NULL) {
					_NhlCBDelete(target_md->multi_obj.cbs[to]);
					target_md->multi_obj.cbs[to] = NULL;
				}
				(void)_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
                	}
			tmp_ho = NULL;
			if(!(value_md->multidval.missing_value.has_missing) ||(value_md->multidval.missing_value.value.objval != val[from])) {
				tmp_ho = (NclHLUObj)_NclGetObj((int)val[from]);
			}
			if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
				(void)_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
				target_md->multi_obj.cbs[to] = AddDestroyNotify(target_md,to);
                	} else {
				target_md->multi_obj.cbs[to] = NULL;
			}
			((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)val[from];
			((NclHLUCbData*)cbdata.ptrval)->off = to;
			((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[to];
			((NclHLUCbData*)cbdata.ptrval)->kind = 0;
			((obj*)target_md->multidval.val)[to] = val[from];
			selector.lngval = HLUVALCHANGE;
			_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
/*
* ADD HLUVALCHANGE
*/
		}
		if(compare_sel[n_dims_target-1] <0) {
			current_index[n_dims_target -1 ] += strider[n_dims_target-1];
		} else {
			compare_sel[n_dims_target-1]++;
		}
		for(k = n_dims_target-1; k >0; k--) {
			switch(compare_sel[k]) {
			case -2: 
				if(current_index[k] > sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(current_index[k] < sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
/*
* Only falls through to here when vector is present because compare_sel
* is only positive when a vector is present
*/
				if(compare_sel[k] >= sel_ptr[k].u.vec.n_ind) {
					compare_sel[k] = 0;
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}

				} else {
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(compare_sel[0]) {
		case -2:
			if(current_index[0] > sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(current_index[0] < sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(compare_sel[0] >= sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				current_index[0] = sel_ptr[0].u.vec.ind[compare_sel[0]];
			}
			break;
		}
		from++;
	}
	NclFree(cbdata.ptrval);
	return(NhlNOERROR);
}
static NhlErrorTypes MultiDVal_HLUObj_s_WriteSection
#if	NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
	NclData target;
	NclSelectionRecord *sel;
	NclData value;
#endif
{
	NclMultiDValHLUObjData target_md = (NclMultiDValHLUObjData)target;
	NclMultiDValHLUObjData value_md = (NclMultiDValHLUObjData)value;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/
	NclSelection *sel_ptr = sel->selection;
	obj *val;
	int i,k,to;

	int current_index[NCL_MAX_DIMENSIONS];
	int multiplier[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	int strider[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];

	ng_size_t total_elements = 1;
	int n_dims_target = target_md->multidval.n_dims;
	ng_size_t n_elem=0;
	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	NclHLUObj tmp_ho;
	NhlArgVal cbdata;
	NhlArgVal selector;

/*
* preconditions:
*	target and value are same type of object
*	sel is a valid selection record with no duplicate indices and
* 	contains a selection entry for each dimension in target.
*
* 	*****Value is a SCALAR array***** 
*	number of dimensions == 1 && size == 1
*/
	if((target_md->multidval.missing_value.has_missing)&&
		(value_md->multidval.missing_value.has_missing)) {
		if(target_md->multidval.missing_value.value.objval ==
			value_md->multidval.missing_value.value.objval) {
/*
* No need to check when missing values are equal
*/
			chckmiss = 0;
		} else {
			chckmiss = 1;
		}
	} else {
		chckmiss = 0;
	}
	
	for(i = 0 ; i < n_dims_target; i++) {
		switch(sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			sel_ptr->u.sub.finish = target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
*/
		case Ncl_SUBSCR:
			if(sel_ptr->u.sub.finish < sel_ptr->u.sub.start) {

				if(sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        sel_ptr->u.sub.stride = 1;
                                }

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	/ sel_ptr->u.sub.stride) + 1L;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(sel_ptr->u.sub.stride < 0){
                                        current_index[i] = sel_ptr->u.sub.finish + (sel_ptr->u.sub.start - sel_ptr->u.sub.finish) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        compare_sel[i] = -2;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                } else {
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = -(sel_ptr->u.sub.stride);
                                }


			} else {

				if(sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        sel_ptr->u.sub.stride = 1;
                                }

				n_elem = (ng_size_t) labs((sel_ptr->u.sub.start - sel_ptr->u.sub.finish)
					 	/ sel_ptr->u.sub.stride) + 1L;

				if(sel_ptr->u.sub.stride < 0){
                                        compare_sel[i] = -1;
                                        current_index[i] = sel_ptr->u.sub.finish - (sel_ptr->u.sub.finish - sel_ptr->u.sub.start) % abs(sel_ptr->u.sub.stride);
                                        sel_ptr->u.sub.finish = sel_ptr->u.sub.start;
                                        sel_ptr->u.sub.start = current_index[i];
                                        strider[i] = sel_ptr->u.sub.stride;

                                } else {
                                        compare_sel[i] = -2;
                                        current_index[i] = sel_ptr->u.sub.start;
                                        strider[i] = sel_ptr->u.sub.stride;
                                }
			}
			if((sel_ptr->u.sub.start > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((sel_ptr->u.vec.min < 0)|| (sel_ptr->u.vec.min > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((sel_ptr->u.vec.max < 0)|| (sel_ptr->u.vec.max > target_md->multidval.dim_sizes[sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem = sel_ptr->u.vec.n_ind;
			strider[i] = 0;
			current_index[i] = sel_ptr->u.vec.ind[0]; 
			compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(sel_ptr->dim_num != n_dims_target -1) {
			multiplier[i] = 1;

			for(k = sel_ptr->dim_num + 1; k < n_dims_target;k++) {
				multiplier[i] *= target_md->multidval.dim_sizes[k];
			}
		} else {
			multiplier[i] = 1;
		}
		output_dim_sizes[i] = n_elem;
		total_elements = total_elements * n_elem;
		sel_ptr++;
	}
	sel_ptr = sel->selection;
/*
* all dimsizes are in valid ranges of target dimensions.
* All subscript ranges are valid. if vector subscript is present is known.
* also multiplier contains the appropriate multiplier by which to multiply
* an index to get the actual index in the target vector.
* the output arrays dimension sizes are known and the total number of 
* elements is known 
* the current_index array is inialized to the starting location
*
* compare_sel contains either -2 or -1 for normal subscripts or the
* index >= 0 into the integer vector array.
*/

	val = (obj*)value_md->multidval.val;
	if(chckmiss) {
		if(*val == value_md->multidval.missing_value.value.objval){
			*val = target_md->multidval.missing_value.value.objval;
		}
	}
	cbdata.ptrval = NclMalloc(sizeof(NclHLUCbData));
	while(!done) {
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
/*		if(!(target_md->multidval.missing_value.has_missing)||
		   (((obj*)target_md->multidval.val)[to] != target_md->multidval.missing_value.value.objval)) {
*/
		if (! (chckmiss && (((obj*)target_md->multidval.val)[to] == target_md->multidval.missing_value.value.objval))) {
			tmp_ho = (NclHLUObj)_NclGetObj((int)((obj*)target_md->multidval.val)[to]);
			if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
				if(target_md->multi_obj.cbs[to] != NULL) {
					_NhlCBDelete(target_md->multi_obj.cbs[to]);
					target_md->multi_obj.cbs[to] = NULL;
				}
				(void)_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
			}
		}  
/*
* ADD HLUVALCHANGE
*/
		if((!chckmiss)||(*val != target_md->multidval.missing_value.value.objval)) {
			tmp_ho = (NclHLUObj)_NclGetObj((int)*val);
			if((tmp_ho != NULL) &&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
				(void)_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
				target_md->multi_obj.cbs[to] = AddDestroyNotify(target_md,to);
			} else {
				target_md->multi_obj.cbs[to] = NULL;
			}
			((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)*val;
			((NclHLUCbData*)cbdata.ptrval)->off = to;
			((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[to];
			((NclHLUCbData*)cbdata.ptrval)->kind = 0;
		}  else {
			((NclHLUCbData*)cbdata.ptrval)->ncl_id = (int)*val;
			((NclHLUCbData*)cbdata.ptrval)->off = to;
			((NclHLUCbData*)cbdata.ptrval)->prev_id = ((obj*)target_md->multidval.val)[to];
			((NclHLUCbData*)cbdata.ptrval)->kind = 1;
		}
		((obj*)target_md->multidval.val)[to] = *val;
		selector.lngval = HLUVALCHANGE;
		_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
		/*_NclPrintHLURefs();*/
		if(compare_sel[n_dims_target-1] <0) {
			current_index[n_dims_target -1 ] += strider[n_dims_target-1];
		} else {
			compare_sel[n_dims_target-1]++;
		}
		for(k = n_dims_target-1; k >0; k--) {
			switch(compare_sel[k]) {
			case -2: 
				if(current_index[k] > sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(current_index[k] < sel_ptr[k].u.sub.finish) {
					current_index[k] = sel_ptr[k].u.sub.start;
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
/*
* Only falls through to here when vector is present because compare_sel
* is only positive when a vector is present
*/
				if(compare_sel[k] >= sel_ptr[k].u.vec.n_ind) {
					compare_sel[k] = 0;
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					if(compare_sel[k-1] < 0) {
						current_index[k-1] += strider[k-1];
					} else {
						compare_sel[k-1]++;
					}

				} else {
					current_index[k] = sel_ptr[k].u.vec.ind[compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(compare_sel[0]) {
		case -2:
			if(current_index[0] > sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(current_index[0] < sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(compare_sel[0] >= sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				current_index[0] = sel_ptr[0].u.vec.ind[compare_sel[0]];
			}
			break;
		}
	}
	NclFree(cbdata.ptrval);
	return(NhlNOERROR);
}



static void MultiDVal_HLUObj_ResetMissing
#if	NhlNeedProto
(NclData self, NclScalar *missing)
#else
(self,missing)
	NclData self;
	NclScalar *missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	obj theval;
	obj oldval;
	obj *step;
	int i;

	if(missing == NULL) {	
		return;
	} else {
		if(self_md->multidval.missing_value.has_missing) {
			oldval = self_md->multidval.missing_value.value.objval;
			theval = missing->objval;
			step = (obj*)self_md->multidval.val;
			for(i = 0; i< self_md->multidval.totalelements; i++) {
				if(*step == oldval) {
					*step = theval;
				}
				step++;
			}
			self_md->multidval.missing_value.value.objval = theval;
			self_md->multidval.missing_value.has_missing = 1;
		} else {
			self_md->multidval.missing_value.value.objval = missing->objval;
			self_md->multidval.missing_value.has_missing = 1;
/*
* Since didn't have missing value before then there are no missing values to
* replace
*/
			return;
		}
	}
}

static NhlErrorTypes MultiDVal_HLUObj_ReadWriteSection
#if	NhlNeedProto
(NclData to_data, NclSelectionRecord * to_selection, NclData from_data, NclSelectionRecord *from_selection)
#else
(to_data,to_selection,from_data,from_selection)
NclData to_data;
NclSelectionRecord * to_selection;
NclData from_data;
NclSelectionRecord *from_selection;
#endif
{
	NclMultiDValHLUObjData target_md = (NclMultiDValHLUObjData)to_data;
	NclMultiDValHLUObjData value_md = (NclMultiDValHLUObjData)from_data;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/

	int j,i,k;
	long from,to;
	NclSelection *to_sel_ptr = NULL;
	obj *to_val;
	NclSelection *from_sel_ptr = NULL;
	obj *from_val;

	long to_current_index[NCL_MAX_DIMENSIONS];
	long to_multiplier[NCL_MAX_DIMENSIONS];
	long to_compare_sel[NCL_MAX_DIMENSIONS];
	long to_strider[NCL_MAX_DIMENSIONS];
	int to_output_dim_sizes[NCL_MAX_DIMENSIONS];

	long from_current_index[NCL_MAX_DIMENSIONS];
	long from_multiplier[NCL_MAX_DIMENSIONS];
	long from_compare_sel[NCL_MAX_DIMENSIONS];
	long from_strider[NCL_MAX_DIMENSIONS];
	int from_output_dim_sizes[NCL_MAX_DIMENSIONS];

	int n_dims_value = 0,n_dims_value_orig = 0;
	ng_size_t total_elements_value = 1;
	ng_size_t total_elements_target = 1;
	int n_dims_target = 0,n_dims_target_orig = 0;
	ng_size_t n_elem_target=0;
	ng_size_t n_elem_value=0;

	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	NclHLUObj tmp_ho;
	NhlArgVal cbdata;
	NhlArgVal selector;

        for(i = 0; i < NCL_MAX_DIMENSIONS; i++) {
                to_current_index[i] = 0;
                to_multiplier[i] = 0;
                to_compare_sel[i] = 0;
                to_strider[i]=0;
                to_output_dim_sizes[i]=0;
                from_current_index[i]=0;
                from_multiplier[i]=0;
                from_compare_sel[i]=0;
                from_strider[i]=0;
                from_output_dim_sizes[i]=0;
        }


	if((target_md == NULL)||(value_md == NULL) ) {
		return(NhlFATAL);
	}

	n_dims_value_orig = n_dims_value = value_md->multidval.n_dims;
	n_dims_target_orig = n_dims_target = target_md->multidval.n_dims;
	
	

/*
* preconditions:
*	target and value are same type of object
*	sel is a valid selection record with no duplicate indices and
* 	contains a selection entry for each dimension in target or it
* 	is null implying a direct copy.
*
* 	*****Value is a multidimensional array***** 
*	number of dimensions >= 1 && size > 1
*/

	if((target_md->multidval.missing_value.has_missing)&&
		(value_md->multidval.missing_value.has_missing)) {
		if(target_md->multidval.missing_value.value.objval ==
			value_md->multidval.missing_value.value.objval) {
/*
* No need to check when missing values are equal
*/
			chckmiss = 0;
		} else {
			chckmiss = 1;
		}
	} else {
		chckmiss = 0;
	}
	
	if(to_selection != NULL) {
		to_sel_ptr = to_selection->selection;
	} 
	if(from_selection != NULL) {
		from_sel_ptr = from_selection->selection;
	} 
	for(i = 0 ; i < n_dims_target; i++) {
		switch(to_sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			to_sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			to_sel_ptr->u.sub.finish = (long)target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(to_sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				to_sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
*/
		case Ncl_SUBSCR:
			if(to_sel_ptr->u.sub.finish < to_sel_ptr->u.sub.start) {

				if(to_sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        to_sel_ptr->u.sub.stride = 1;
                                }

				n_elem_target = (ng_size_t) labs((to_sel_ptr->u.sub.start - to_sel_ptr->u.sub.finish)
					 	/ to_sel_ptr->u.sub.stride) + 1L;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				if(to_sel_ptr->u.sub.stride < 0){
                                        to_current_index[i] = to_sel_ptr->u.sub.finish + (to_sel_ptr->u.sub.start - to_sel_ptr->u.sub.finish) % abs(to_sel_ptr->u.sub.stride);
                                        to_sel_ptr->u.sub.finish = to_sel_ptr->u.sub.start;
                                        to_sel_ptr->u.sub.start = to_current_index[i];
                                        to_compare_sel[i] = -2;
                                        to_strider[i] = -(to_sel_ptr->u.sub.stride);
                                } else {
                                        to_compare_sel[i] = -1;
                                        to_current_index[i] = to_sel_ptr->u.sub.start;
                                        to_strider[i] = -(to_sel_ptr->u.sub.stride);
                                }


			} else {
				if(to_sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        to_sel_ptr->u.sub.stride = 1;
                                }

				n_elem_target = (ng_size_t) labs((to_sel_ptr->u.sub.start - to_sel_ptr->u.sub.finish)
					 	/ to_sel_ptr->u.sub.stride) + 1L;

				if(to_sel_ptr->u.sub.stride < 0){
                                        to_compare_sel[i] = -1;
                                        to_current_index[i] = to_sel_ptr->u.sub.finish - (to_sel_ptr->u.sub.finish - to_sel_ptr->u.sub.start) % abs(to_sel_ptr->u.sub.stride);
                                        to_sel_ptr->u.sub.finish = to_sel_ptr->u.sub.start;
                                        to_sel_ptr->u.sub.start = to_current_index[i];
                                        to_strider[i] = to_sel_ptr->u.sub.stride;

                                } else {
                                        to_compare_sel[i] = -2;
                                        to_current_index[i] = to_sel_ptr->u.sub.start;
                                        to_strider[i] = to_sel_ptr->u.sub.stride;
                                }

			}
			if((to_sel_ptr->u.sub.start > target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1)||(to_sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((to_sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1)||(to_sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((to_sel_ptr->u.vec.min < 0)|| (to_sel_ptr->u.vec.min > target_md->multidval.dim_sizes[to_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((to_sel_ptr->u.vec.max < 0)|| (to_sel_ptr->u.vec.max > target_md->multidval.dim_sizes[to_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem_target = to_sel_ptr->u.vec.n_ind;
			to_strider[i] = 0;
			to_current_index[i] = to_sel_ptr->u.vec.ind[0]; 
			to_compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(to_sel_ptr->dim_num != n_dims_target -1) {
			to_multiplier[i] = 1;

			for(k = to_sel_ptr->dim_num + 1; k < n_dims_target;k++) {
				to_multiplier[i] *= (long)target_md->multidval.dim_sizes[k];
			}
		} else {
			to_multiplier[i] = 1;
		}
		to_output_dim_sizes[i] = n_elem_target;
		total_elements_target = total_elements_target * n_elem_target;
		to_sel_ptr++;
	}

	for(i = 0 ; i < n_dims_value; i++) {
		switch(from_sel_ptr->sel_type) {
		case Ncl_SUB_ALL:
			from_sel_ptr->u.sub.start = 0;
		case Ncl_SUB_VAL_DEF:
			from_sel_ptr->u.sub.finish = (long)value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1;
		case Ncl_SUB_DEF_VAL:
			if(from_sel_ptr->sel_type != Ncl_SUB_VAL_DEF)
				from_sel_ptr->u.sub.start = 0;
/*
* The above cases make sure the defaults ranges are set. This cannot happen
* until here because dim sizes are not known out side of the object
*/
		case Ncl_SUBSCR:
			if(from_sel_ptr->u.sub.finish < from_sel_ptr->u.sub.start) {

				if(from_sel_ptr->u.sub.stride == 0 ) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
                                        from_sel_ptr->u.sub.stride = 1;
                                }

				n_elem_value = (ng_size_t) labs((from_sel_ptr->u.sub.start - from_sel_ptr->u.sub.finish)
					 	/ from_sel_ptr->u.sub.stride) + 1L;

/*
* Need from be able from determine which type of comparision < or > is needed from
* determine whether the finish has been passed up
*/
				if(from_sel_ptr->u.sub.stride < 0){
                                        from_current_index[i] = from_sel_ptr->u.sub.finish + (from_sel_ptr->u.sub.start - from_sel_ptr->u.sub.finish) % abs(from_sel_ptr->u.sub.stride);
                                        from_sel_ptr->u.sub.finish = from_sel_ptr->u.sub.start;
                                        from_sel_ptr->u.sub.start = from_current_index[i];
                                        from_compare_sel[i] = -2;
                                        from_strider[i] = -(from_sel_ptr->u.sub.stride);
                                } else {
                                        from_compare_sel[i] = -1;
                                        from_current_index[i] = from_sel_ptr->u.sub.start;
                                        from_strider[i] = -(from_sel_ptr->u.sub.stride);
                                }


			} else {
				if(from_sel_ptr->u.sub.stride == 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is less than end and stride is negative, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem_value = (ng_size_t) labs((from_sel_ptr->u.sub.start - from_sel_ptr->u.sub.finish)
					 	/ from_sel_ptr->u.sub.stride) + 1L;

                                if(from_sel_ptr->u.sub.stride < 0){
                                        from_compare_sel[i] = -1;
                                        from_current_index[i] = from_sel_ptr->u.sub.finish - (from_sel_ptr->u.sub.finish - from_sel_ptr->u.sub.start) % abs(from_sel_ptr->u.sub.stride);
                                        from_sel_ptr->u.sub.finish = from_sel_ptr->u.sub.start;
                                        from_sel_ptr->u.sub.start = from_current_index[i];
                                        from_strider[i] = from_sel_ptr->u.sub.stride;

                                } else {
                                        from_compare_sel[i] = -2;
                                        from_current_index[i] = from_sel_ptr->u.sub.start;
                                        from_strider[i] = from_sel_ptr->u.sub.stride;
                                }

			}
			if((from_sel_ptr->u.sub.start > value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1)||(from_sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((from_sel_ptr->u.sub.finish > value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1)||(from_sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			break;
		case Ncl_VECSUBSCR:
/*
* to qualify as a vector subscript must have one dimension, be integer and have
* a dim_size > 1
*/
			if((from_sel_ptr->u.vec.min < 0)|| (from_sel_ptr->u.vec.min > value_md->multidval.dim_sizes[from_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			if((from_sel_ptr->u.vec.max < 0)|| (from_sel_ptr->u.vec.max > value_md->multidval.dim_sizes[from_sel_ptr->dim_num]-1)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			n_elem_value = from_sel_ptr->u.vec.n_ind;
			from_strider[i] = 0;
			from_current_index[i] = from_sel_ptr->u.vec.ind[0]; 
			from_compare_sel[i] = 0;
		} 
/*
* This check should really be done from above before calling this function
* But I put it here just in case.
*/
		if(from_sel_ptr->dim_num != n_dims_value -1) {
			from_multiplier[i] = 1;

			for(k = from_sel_ptr->dim_num + 1; k < n_dims_value;k++) {
				from_multiplier[i] *= (long)value_md->multidval.dim_sizes[k];
			}
		} else {
			from_multiplier[i] = 1;
		}
		from_output_dim_sizes[i] = n_elem_value;
		total_elements_value =total_elements_value * n_elem_value;
		from_sel_ptr++;
	}
        i = 0;
        while(i < n_dims_value) {
                if(from_output_dim_sizes[i] == 1) {
                        for(j = i; j  < n_dims_value; j++) {
                                from_output_dim_sizes[j] = from_output_dim_sizes[j+1];
                        }
                        n_dims_value--;
                } else {
                        i++;
                }
        }
        if(n_dims_value == 0) {
                n_dims_value = 1;
                from_output_dim_sizes[0] = 1;
        }
        i = 0;
        while( i < n_dims_target ) {
                if(to_output_dim_sizes[i] == 1) {
                        for(j = i; j  < n_dims_target; j++) {
                                to_output_dim_sizes[j] = to_output_dim_sizes[j+1];
                        }
                        n_dims_target--;
                } else {
                        i++;
                }
        }
        if(n_dims_target == 0) {
                n_dims_target = 1;
                to_output_dim_sizes[0] = 1;
        }

        if((n_dims_value != 1)||(from_output_dim_sizes[0] !=1)) {
                if(n_dims_target != n_dims_value) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Right hand side of assignment has (%d) dimensions and left hand side has (%d), dimension mismatch",n_dims_value,n_dims_target);
                        return(NhlFATAL);
                }
                for(i = 0; i< n_dims_target; i++) {
                        if(from_output_dim_sizes[i] != to_output_dim_sizes[i]) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension size mismatch, dimension (%d) of left hand side reference does not have the same size as the right hand side reference after subscripting.",i);
                                return(NhlFATAL);
                        }
                }
        }


	to_sel_ptr = to_selection->selection;
	from_sel_ptr = from_selection->selection;
	to_val = (obj*)target_md->multidval.val;
	from_val = (obj*)value_md->multidval.val;
	cbdata.ptrval = NclMalloc(sizeof(NclHLUCbData));
	while(!done) {
		to = 0;
		from = 0;
		for(i = 0; i < n_dims_target_orig;i++) {
			to = to + (to_current_index[i] * to_multiplier[i]);
		}
		for(i = 0; i < n_dims_value_orig;i++) {
			from = from + (from_current_index[i] * from_multiplier[i]);
		}
		if(chckmiss) {
			tmp_ho = NULL;
			if(to_val[to] != target_md->multidval.missing_value.value.objval) {
				tmp_ho = (NclHLUObj)_NclGetObj((int)to_val[to]);
				if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
					if(target_md->multi_obj.cbs[to] != NULL) {
						_NhlCBDelete(target_md->multi_obj.cbs[to]);
						target_md->multi_obj.cbs[to] = NULL;
					}
					_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
				}
			}
			tmp_ho = NULL;
			if(from_val[from] != value_md->multidval.missing_value.value.objval) {
				tmp_ho = (NclHLUObj)_NclGetObj((int)from_val[from]);
				if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
					_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
					target_md->multi_obj.cbs[to] = AddDestroyNotify(target_md,to);
				} else {
					target_md->multi_obj.cbs[to] = NULL;
				}
				((NclHLUCbData*)cbdata.ptrval)->ncl_id = from_val[from];
				((NclHLUCbData*)cbdata.ptrval)->off = to;
				((NclHLUCbData*)cbdata.ptrval)->prev_id = to_val[to];
				((NclHLUCbData*)cbdata.ptrval)->kind = 0;
			} else {
				((NclHLUCbData*)cbdata.ptrval)->ncl_id = from_val[from];
				((NclHLUCbData*)cbdata.ptrval)->off = to;
				((NclHLUCbData*)cbdata.ptrval)->prev_id = to_val[to];
				((NclHLUCbData*)cbdata.ptrval)->kind = 1;
			}
			to_val[to] = 
				((from_val[from] == value_md->multidval.missing_value.value.objval) ? 
				target_md->multidval.missing_value.value.objval 
				: from_val[from]);
	
/*
* ADD HLUVALCHANGE
*/
			selector.lngval = HLUVALCHANGE;
			_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
		} else {
			tmp_ho = NULL;
			if(!(target_md->multidval.missing_value.has_missing)||(to_val[to] != target_md->multidval.missing_value.value.objval)) {
				tmp_ho = (NclHLUObj)_NclGetObj((int)to_val[to]);
				if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
					if(target_md->multi_obj.cbs[to] != NULL) {
						_NhlCBDelete(target_md->multi_obj.cbs[to]);
						target_md->multi_obj.cbs[to] = NULL;
					}
					_NclDelParent((NclObj)tmp_ho,(NclObj)target_md);
				}
			}
			tmp_ho = NULL;
			if(!(value_md->multidval.missing_value.has_missing)||(from_val[from] != value_md->multidval.missing_value.value.objval)) {
				tmp_ho = (NclHLUObj)_NclGetObj((int)from_val[from]);
				if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)) {
					_NclAddParent((NclObj)tmp_ho,(NclObj)target_md);
					target_md->multi_obj.cbs[to] = AddDestroyNotify(target_md,to);
				} else {
					target_md->multi_obj.cbs[to] = NULL;
				}
				((NclHLUCbData*)cbdata.ptrval)->ncl_id = from_val[from];
				((NclHLUCbData*)cbdata.ptrval)->off = to;
				((NclHLUCbData*)cbdata.ptrval)->prev_id = to_val[to];
				((NclHLUCbData*)cbdata.ptrval)->kind = 0;
			} else {
				((NclHLUCbData*)cbdata.ptrval)->ncl_id = from_val[from];
				((NclHLUCbData*)cbdata.ptrval)->off = to;
				((NclHLUCbData*)cbdata.ptrval)->prev_id = to_val[to];
				((NclHLUCbData*)cbdata.ptrval)->kind = 1;
			}
			to_val[to] = from_val[from];
/*
* ADD HLUVALCHANGE
*/
			selector.lngval = HLUVALCHANGE;
			_NhlCBCallCallbacks(target_md->obj.cblist,selector,cbdata);
		}
		if(to_compare_sel[n_dims_target_orig-1] <0) {
			to_current_index[n_dims_target_orig -1 ] += to_strider[n_dims_target_orig-1];
		} else {
			to_compare_sel[n_dims_target_orig-1]++;
		}
		if((n_dims_value != 1)||(from_output_dim_sizes[0] !=1)) {
			if(from_compare_sel[n_dims_value_orig -1] <0) {
				from_current_index[n_dims_value_orig -1 ] += from_strider[n_dims_value_orig-1];
			} else {
				from_compare_sel[n_dims_value_orig-1]++;
			}
		}
		for(k = n_dims_target_orig-1; k >0; k--) {
			switch(to_compare_sel[k]) {
			case -2: 
				if(to_current_index[k] > to_sel_ptr[k].u.sub.finish) {
					to_current_index[k] = to_sel_ptr[k].u.sub.start;
					if(to_compare_sel[k-1] < 0) {
						to_current_index[k-1] += to_strider[k-1];
					} else {
						to_compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			case -1:
				if(to_current_index[k] < to_sel_ptr[k].u.sub.finish) {
					to_current_index[k] = to_sel_ptr[k].u.sub.start;
					if(to_compare_sel[k-1] < 0) {
						to_current_index[k-1] += to_strider[k-1];
					} else {
						to_compare_sel[k-1]++;
					}
				} else {
					inc_done = 1;
				}
				break;
			default:
				if(to_compare_sel[k] >= to_sel_ptr[k].u.vec.n_ind) {
					to_compare_sel[k] = 0;
					to_current_index[k] = to_sel_ptr[k].u.vec.ind[to_compare_sel[k]];
					if(to_compare_sel[k-1] < 0) {
						to_current_index[k-1] += to_strider[k-1];
					} else {
						to_compare_sel[k-1]++;
					}

				} else {
					to_current_index[k] = to_sel_ptr[k].u.vec.ind[to_compare_sel[k]];
					inc_done = 1;
				}
				break;
			}

			if(inc_done) {
				inc_done = 0;
				break;
			}
		}
		switch(to_compare_sel[0]) {
		case -2:
			if(to_current_index[0] > to_sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		case -1:
			if(to_current_index[0] < to_sel_ptr[0].u.sub.finish)
					done = 1;
			break;
		default:
			if(to_compare_sel[0] >= to_sel_ptr[0].u.vec.n_ind) {
					done = 1;
			} else {
				to_current_index[0] = to_sel_ptr[0].u.vec.ind[to_compare_sel[0]];
			}
			break;
		}
		if((n_dims_value != 1)||(from_output_dim_sizes[0] !=1)) {

			for(k = n_dims_value_orig -1; k >0; k--) {
				switch(from_compare_sel[k]) {
				case -2: 
					if(from_current_index[k] > from_sel_ptr[k].u.sub.finish) {
						from_current_index[k] = from_sel_ptr[k].u.sub.start;
						if(from_compare_sel[k-1] < 0) {
							from_current_index[k-1] += from_strider[k-1];
						} else {
							from_compare_sel[k-1]++;
						}
					} else {
						inc_done = 1;
					}
					break;
				case -1:
					if(from_current_index[k] < from_sel_ptr[k].u.sub.finish) {
						from_current_index[k] = from_sel_ptr[k].u.sub.start;
						if(from_compare_sel[k-1] < 0) {
							from_current_index[k-1] += from_strider[k-1];
						} else {
							from_compare_sel[k-1]++;
						}
					} else {
						inc_done = 1;
					}
					break;
				default:
					if(from_compare_sel[k] >= from_sel_ptr[k].u.vec.n_ind) {
						from_compare_sel[k] = 0;
						from_current_index[k] = from_sel_ptr[k].u.vec.ind[from_compare_sel[k]];
						if(from_compare_sel[k-1] < 0) {
							from_current_index[k-1] += from_strider[k-1];
						} else {
							from_compare_sel[k-1]++;
						}
	
					} else {
						from_current_index[k] = from_sel_ptr[k].u.vec.ind[from_compare_sel[k]];
						inc_done = 1;
					}
					break;
				}
	
				if(inc_done) {
					inc_done = 0;
					break;
				}		
			}
			switch(from_compare_sel[0]) {
			case -2:
				if(from_current_index[0] > from_sel_ptr[0].u.sub.finish)
						done = 1;
				break;
			case -1:
				if(from_current_index[0] < from_sel_ptr[0].u.sub.finish)
						done = 1;
				break;
			default:
				if(from_compare_sel[0] >= from_sel_ptr[0].u.vec.n_ind) {
						done = 1;
				} else {
					from_current_index[0] = from_sel_ptr[0].u.vec.ind[from_compare_sel[0]];
				}
				break;
			}
		}
	}
	NclFree(cbdata.ptrval);
	return(NhlNOERROR);
}

static void MultiDVal_HLUObj_Destroy
#if	NhlNeedProto
(NclObj self)
#else
(self)
        NclObj  self;
#endif
{
        NclMultiDValHLUObjData self_md = (NclMultiDValHLUObjData)self;
	NclHLUObj tmp_ho;
	obj *obj_ids,i;

        if(self->obj.cblist != NULL) {
		NhlArgVal selector;
		NhlArgVal cbdata;

		NhlINITVAR(selector);
		NhlINITVAR(cbdata);
		cbdata.intval = self->obj.id;
	        selector.lngval = DESTROYED;

                _NhlCBCallCallbacks(self->obj.cblist,selector,cbdata);
        }

	_NclUnRegisterObj(self);
	
	if(self_md->multidval.sel_rec != NULL) {
		NclFree(self_md->multidval.sel_rec);
	}
	for(i = 0; i < self_md->multidval.totalelements; i++) {
		if(self_md->multi_obj.crecs[i] != NULL) {
			NclFree(self_md->multi_obj.crecs[i]);
		}
	}
	NclFree(self_md->multi_obj.crecs);
	if(self_md->obj.cblist != NULL) {
		_NhlCBDestroy(self_md->obj.cblist);
	}
	NclFree(self_md->multi_obj.cbs);

	if(self_md->multidval.missing_value.has_missing) {
		obj_ids = (obj*)self_md->multidval.val;	
		for(i = 0; i< self_md->multidval.totalelements; i++) {
			if(obj_ids[i] != self_md->multidval.missing_value.value.objval) {
				tmp_ho = (NclHLUObj)_NclGetObj(obj_ids[i]);
				if(tmp_ho != NULL) {
					(void)_NclDelParent((NclObj)tmp_ho,self);
				}
			}
		}
	} else {
		obj_ids = (obj*)self_md->multidval.val;	
		for(i = 0; i< self_md->multidval.totalelements; i++) {
			tmp_ho = (NclHLUObj)_NclGetObj(obj_ids[i]);
			if(tmp_ho != NULL) {
				(void)_NclDelParent((NclObj)tmp_ho,self);
			}
		}
	}
	
	if((self_md->obj.status != STATIC)&&(self_md->multidval.val != NULL)) {
		NclFree(self_md->multidval.val);
	}
	NclFree(self);
	return;
}

static NclData NclMultiDValhluDup
#if	NhlNeedProto
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData) self;
	obj *toval;
	obj *frval;
	obj missing;
	NclScalar themissing;
	int i;
	toval = (obj*)NclMalloc(self_md->multidval.totalsize);
	frval = (obj*)self_md->multidval.val;
	if(toval == NULL) {
		return(NULL);
        }
	if((new_missing == NULL)||(!self_md->multidval.missing_value.has_missing)) {
		memcpy((char*)toval,(char*)frval,self_md->multidval.totalsize);
		themissing = self_md->multidval.missing_value.value;
	} else {
		missing = self_md->multidval.missing_value.value.objval;
		for(i = 0; i < self_md->multidval.totalelements; i++) {
			toval[i] = (frval[i] == missing ?
				new_missing->objval :
				frval[i]);
		}
		themissing = *new_missing;
	}
        return((NclData)_NclMultiDValHLUObjDataCreate(
                NULL,
                NULL,
                Ncl_MultiDValHLUObjData,
                0,
                (void*)toval,
                (self_md->multidval.missing_value.has_missing ? &themissing : NULL),
                self_md->multidval.n_dims,
                self_md->multidval.dim_sizes,
                TEMPORARY,
                NULL));
}

static NhlErrorTypes InitializeHLUObjDataClass(
#if NhlNeedProto
void
#endif
);

static NhlErrorTypes HLUMultiDValAddParent(
#if NhlNeedProto
NclObj, 
NclObj
#endif
);
static NhlErrorTypes HLUMultiDValDelParent(
#if NhlNeedProto
NclObj, 
NclObj
#endif
);


NclMultiDValHLUObjDataClassRec nclMultiDValHLUObjDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValHLUObjData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValHLUObjDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclMultiDValDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	MultiDVal_HLUObj_Destroy,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	InitializeHLUObjDataClass,
	(NclAddParentFunction)HLUMultiDValAddParent,
                (NclDelParentFunction)HLUMultiDValDelParent,
/* NclPrintSummaryFunction print_summary */ NULL,
/* NclPrintFunction print; 	*/	NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclGenericFunction dup; 	*/	NclMultiDValhluDup,
/* NclResetMissingValueFuction dup;	*/	MultiDVal_HLUObj_ResetMissing,
/* NclReadSubSecFunction r_subsection */ MultiDVal_HluObj_ReadSection,
/* NclReadSubSecFunction w_subsection */{
					MultiDVal_HLUObj_md_WriteSection,
					MultiDVal_HLUObj_s_WriteSection
					},
/* NclReadThenWriteSubFunc w_subsection */ MultiDVal_HLUObj_ReadWriteSection,
/* NclDataFunction coerce; 	*/	{NULL,NULL},
/* NclDataFunction multiply; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction plus; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction minus; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction divide; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction exponent; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction mod; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction mat; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction sel_lt; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction sel_gt; 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction not; 	*/	{NULL,NULL},
/* NclDataFunction neg; 	*/	{NULL,NULL},
/* NclDataFunction gt; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction lt; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction ge; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction le; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction ne; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction eq; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction and;	 	*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction or; 		*/	{NULL,NULL,NULL,NULL},
/* NclDataFunction xor;		*/	{NULL,NULL,NULL,NULL},
/* NclIsMissingFunction    is_mis; */	NULL

	},
	{	
		NULL
	},
	{		
		NULL
	}
};

NclObjClass nclMultiDValHLUObjDataClass = (NclObjClass)&nclMultiDValHLUObjDataClassRec;

static NhlErrorTypes InitializeHLUObjDataClass
#if NhlNeedProto
(void)
#else
()
#endif
{
	int i;
	_NclRegisterClassPointer(
		Ncl_MultiDValHLUObjData,
		(NclObjClass)&nclMultiDValHLUObjDataClassRec
	);
#if 0
	nclMultiDValHLUObjDataClassRec.multid_obj_class.ref_table =(HLURefTableNode*) NclMalloc(sizeof(HLURefTableNode)*NCL_SYM_TAB_SIZE);
	for(i = 0; i < NCL_SYM_TAB_SIZE;i++) {
		nclMultiDValHLUObjDataClassRec.multid_obj_class.ref_table[i].id = -1;
		nclMultiDValHLUObjDataClassRec.multid_obj_class.ref_table[i].thelist = NULL;
		nclMultiDValHLUObjDataClassRec.multid_obj_class.ref_table[i].next = NULL;
	}
#endif
	return(NhlNOERROR);
}


struct _NclMultiDValDataRec * _NclMultiDValHLUObjDataCreate
#if	NhlNeedProto
(NclObj inst,NclObjClass theclass,NclObjTypes obj_type,unsigned int obj_type_mask,void *val,NclScalar *missing_value,int n_dims, ng_size_t *dim_sizes,NclStatus status,NclSelectionRecord *sel_rec)
#else
(inst,theclass,obj_type,obj_type_mask, val,missing_value,n_dims,dim_sizes,status,sel_rec)
NclObj inst ;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
ng_size_t *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
#endif
{
	NclMultiDValHLUObjData thevalobj;
	NclObjClass class_ptr= nclMultiDValHLUObjDataClass;
	int i;
	NhlErrorTypes ret1= NhlNOERROR;
	obj *obj_ids;
	NclHLUObj tmp_ho;

	ret1 = _NclInitClass(nclMultiDValHLUObjDataClass);
	if(ret1 < NhlWARNING) {
		return(NULL);
	}
	if(inst == NULL ) {
		thevalobj = (NclMultiDValHLUObjData)NclMalloc(
				(unsigned)nclMultiDValHLUObjDataClassRec.obj_class.obj_size);
	} else {
		thevalobj = (NclMultiDValHLUObjData)inst;
	}
	if(theclass != NULL) {
		class_ptr = theclass;
	} else {
		class_ptr = nclMultiDValHLUObjDataClass;
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/
	_NclCreateMultiDVal((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_MultiDValHLUObjData),val,missing_value,n_dims,dim_sizes,status,sel_rec,(NclTypeClass)nclTypeobjClass);



	
	thevalobj->multidval.data_type = NCL_obj;

	obj_ids = (obj*)thevalobj->multidval.val;
	thevalobj->multi_obj.cbs = (_NhlCB*)NclMalloc(sizeof(_NhlCB)*thevalobj->multidval.totalelements);
	thevalobj->multi_obj.crecs= (HLUMDCalRec**)NclMalloc(sizeof(HLUMDCalRec*)*thevalobj->multidval.totalelements);
	for(i = 0; i<thevalobj->multidval.totalelements; i++) {
		thevalobj->multi_obj.crecs[i] = NULL;
		tmp_ho = (NclHLUObj)_NclGetObj(obj_ids[i]);
		if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
			(void)_NclAddParent((NclObj)tmp_ho,(NclObj)thevalobj);
			thevalobj->multi_obj.cbs[i] = AddDestroyNotify(thevalobj,i);
		} else {
			thevalobj->multi_obj.cbs[i] = NULL;
		}
	}
	
	return((NclMultiDValData)thevalobj);
}

static NhlErrorTypes HLUMultiDValAddParent
#if     NhlNeedProto
(NclObj theobj, NclObj parent)
#else
(theobj, parent)
NclObj theobj;
NclObj parent;
#endif
{
        NclRefList * tmp = NULL;

        tmp = theobj->obj.parents;
        theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
        theobj->obj.parents->next = tmp;
        theobj->obj.parents->pid= parent->obj.id;
        theobj->obj.ref_count++;
        return(NhlNOERROR);
}

static NhlErrorTypes HLUMultiDValDelParent
#if     NhlNeedProto
(NclObj theobj, NclObj parent)
#else
(theobj, parent)
NclObj theobj;
NclObj parent;
#endif
{
        NclRefList *tmp,*tmp1;
        int found = 0;

        if(theobj->obj.parents == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"MultiDValDelParent: Attempt to delete parent from empty list");
                return(NhlFATAL);
        }

        tmp = theobj->obj.parents;
        while((tmp!=NULL)&&(tmp->pid == parent->obj.id)) {
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
                if(tmp->next->pid == parent->obj.id) {
                        found = 1;
                        tmp1 = tmp->next;
                        tmp->next = tmp->next->next;
                        NclFree(tmp1);
                        theobj->obj.ref_count--;
                } else {
                        tmp = tmp->next;
                }
        }
        if(found) {
                if(theobj->obj.ref_count <= 0)
                        _NclDestroyObj(theobj);
                return(NhlNOERROR);
        } else {
                return(NhlWARNING);
        }
}

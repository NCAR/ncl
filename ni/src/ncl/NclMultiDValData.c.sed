
/*
 *      $Id: NclMultiDValData.c.sed,v 1.10 1995-04-05 22:17:17 ethan Exp $
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
 *	Date:		Fri Oct 29 11:36:10 MDT 1993
 *
 *	Description:	
 */

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include <errno.h>
#include "NclMultiDValData.h"
#include "TypeSupport.h"
#include "DataSupport.h"
#include <math.h>
#include "NclTypestring.h"
#include "NclTypechar.h"

INSERTHERE


static struct _NclDataRec *MultiDValReadSection
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
	void *val;
	int i,k,from,to;

	long current_index[NCL_MAX_DIMENSIONS];
	long multiplier[NCL_MAX_DIMENSIONS];
	long compare_sel[NCL_MAX_DIMENSIONS];
	long strider[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];

	int total_elements = 1;
	int n_dims_input = self_md->multidval.n_dims;
	int n_elem=0;
	int done = 0;
	int inc_done = 0;
	int el_size = 0;

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

				if(sel_ptr->u.sub.stride >= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is greater than end and stride is positive, error in subscript #%d",i);
					return(NULL);
				}

				n_elem = (int)(((float)
					(sel_ptr->u.sub.start 
					- sel_ptr->u.sub.finish))
					/(float)fabs(((float)sel_ptr->u.sub.stride))) + 1;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				compare_sel[i] = -1;

			} else {
				if(sel_ptr->u.sub.stride <= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is less than end and stride is negative, error in subscript #%d",i);
					return(NULL);
				}

				n_elem = (int)(((float)
					(sel_ptr->u.sub.finish 
					- sel_ptr->u.sub.start))
					/((float)sel_ptr->u.sub.stride)) + 1;
				compare_sel[i] = -2;

			}
			if((sel_ptr->u.sub.start > self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}

			if((sel_ptr->u.sub.finish > self_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NULL);
			}
			current_index[i] = sel_ptr->u.sub.start;
			strider[i] = sel_ptr->u.sub.stride;
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

	el_size = self_md->multidval.type->type_class.size;
	val = (void*)NclMalloc(total_elements * el_size);
	to = 0;
	while(!done) {
		from = 0;
		for(i = 0; i < n_dims_input;i++) {
			from = from + (current_index[i] * multiplier[i]);
		}
		memcpy((void*)((char*)val + to * el_size),(void*)((char*)self_md->multidval.val + from * el_size),el_size);

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
		_Nclreset_mis(self_md->multidval.type,self_md->multidval.val,&(self_md->multidval.missing_value.value),missing,total_elements);
		output_md = (NclData)_NclCreateVal(NULL,
			NULL,
			self_md->obj.obj_type,
			0,(void*)val,	
			missing,
			n_dims_input,output_dim_sizes,
			TEMPORARY,sel,(NclObjClass)self_md->multidval.type);
	} else if(self_md->multidval.missing_value.has_missing) {
		output_md = (NclData)_NclCreateVal(NULL,
			NULL,
			self_md->obj.obj_type,
			0,(void*)val,	
			&self_md->multidval.missing_value.value,
			n_dims_input,output_dim_sizes,
			TEMPORARY,sel,(NclObjClass)self_md->multidval.type);
	} else {
		output_md = (NclData)_NclCreateVal(NULL,
			NULL,
			self_md->obj.obj_type,
			0,(void*)val,	
			NULL,
			n_dims_input,output_dim_sizes,
			TEMPORARY,sel,(NclObjClass)self_md->multidval.type);
	}

	return(output_md);		
}


static NhlErrorTypes MultiDVal_md_WriteSection
#if	NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
	NclData target;
	NclSelectionRecord *sel;
	NclData value;
#endif
{
	NclMultiDValData target_md = (NclMultiDValData)target;
	NclMultiDValData value_md = (NclMultiDValData)value;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/
	NclSelection *sel_ptr = NULL;
	void *val;
	int i,k;
	long from,to;

	long current_index[NCL_MAX_DIMENSIONS];
	long multiplier[NCL_MAX_DIMENSIONS];
	long compare_sel[NCL_MAX_DIMENSIONS];
	long strider[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];

	int *dim_sizes_value = value_md->multidval.dim_sizes;
	int n_dims_value = value_md->multidval.n_dims;
	int n_dims_sel = 0;
	int total_elements = 1;
	int n_dims_target = target_md->multidval.n_dims;
	int n_elem=0;
	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	logical tmpe;
	int el_size;

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
		_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)&(target_md->multidval.missing_value.value),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);
		if(tmpe) {
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
	el_size = target_md->multidval.type->type_class.size;
	
	if(sel != NULL) {
		sel_ptr = sel->selection;
	} else {
		if(target_md->multidval.totalsize == value_md->multidval.totalsize) {
			memcpy(target_md->multidval.val,value_md->multidval.val,value_md->multidval.totalsize);
			if(chckmiss) {
				_Nclreset_mis(target_md->multidval.type,target_md->multidval.val,&value_md->multidval.missing_value.value,&target_md->multidval.missing_value.value,target_md->multidval.totalelements);
			} 
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

				if(sel_ptr->u.sub.stride >= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is greater than end and stride is positive, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem = (int)(((float)
					(sel_ptr->u.sub.start 
					- sel_ptr->u.sub.finish))
					/(float)fabs(((float)sel_ptr->u.sub.stride))) + 1;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				compare_sel[i] = -1;

			} else {
				if(sel_ptr->u.sub.stride <= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is less than end and stride is negative, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem = (int)(((float)
					(sel_ptr->u.sub.finish 
					- sel_ptr->u.sub.start))
					/((float)sel_ptr->u.sub.stride)) + 1;
				compare_sel[i] = -2;

			}
			if((sel_ptr->u.sub.start > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			current_index[i] = sel_ptr->u.sub.start;
			strider[i] = sel_ptr->u.sub.stride;
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

	val = value_md->multidval.val;
	from = 0;
	while(!done) {
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		memcpy((void*)((char*)target_md->multidval.val + (to * el_size)),(void*)((char*)val + (from * el_size)),el_size);
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
	if(chckmiss) {
		_Nclreset_mis(target_md->multidval.type,target_md->multidval.val,&value_md->multidval.missing_value.value,&target_md->multidval.missing_value.value,target_md->multidval.totalelements);
	} 
	return(NhlNOERROR);
}
static NhlErrorTypes MultiDVal_s_WriteSection
#if	NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
	NclData target;
	NclSelectionRecord *sel;
	NclData value;
#endif
{
	NclMultiDValData target_md = (NclMultiDValData)target;
	NclMultiDValData value_md = (NclMultiDValData)value;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/
	NclSelection *sel_ptr = NULL;
	void *val;
	int i,k,to;

	int current_index[NCL_MAX_DIMENSIONS];
	int multiplier[NCL_MAX_DIMENSIONS];
	int compare_sel[NCL_MAX_DIMENSIONS];
	int strider[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];

	int total_elements = 1;
	int n_dims_target = target_md->multidval.n_dims;
	int n_elem=0;
	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	int el_size;
	logical tmpe;

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
		_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)&(target_md->multidval.missing_value.value),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);
		if(tmpe) {
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
	el_size = target_md->multidval.type->type_class.size;
	if(sel != NULL) {
		sel_ptr = sel->selection;
	} else {
		if(target_md->multidval.totalsize == value_md->multidval.totalsize) {
			memcpy(target_md->multidval.val,value_md->multidval.val,value_md->multidval.totalsize);
                        if(chckmiss) {
                                _Nclreset_mis(target_md->multidval.type,target_md->multidval.val,&value_md->multidval.missing_value.value,&target_md->multidval.missing_value.value,target_md->multidval.totalelements);
                        }
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

				if(sel_ptr->u.sub.stride >= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is greater than end and stride is positive, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem = (int)(((float)
					(sel_ptr->u.sub.start 
					- sel_ptr->u.sub.finish))
					/(float)fabs(((float)sel_ptr->u.sub.stride))) + 1;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				compare_sel[i] = -1;

			} else {
				if(sel_ptr->u.sub.stride <= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is less than end and stride is negative, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem = (int)(((float)
					(sel_ptr->u.sub.finish 
					- sel_ptr->u.sub.start))
					/((float)sel_ptr->u.sub.stride)) + 1;
				compare_sel[i] = -2;

			}
			if((sel_ptr->u.sub.start > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[sel_ptr->dim_num] - 1)||(sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			current_index[i] = sel_ptr->u.sub.start;
			strider[i] = sel_ptr->u.sub.stride;
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

	val = value_md->multidval.val;
	tmpe = 0;
	if(value_md->multidval.missing_value.has_missing)
		_Ncleq(value_md->multidval.type,(void*)&tmpe,(void*)&(value_md->multidval.missing_value.value),val,NULL,NULL,1,1);
/*
* chckmis is true only if *both* md's contain missing values
*/
	if((tmpe) &&(chckmiss)) {
/*
* case where val is missing value and target has missing values also
*/
		val = (void*)&(target_md->multidval.missing_value.value);
	} else if(tmpe){
/*
* case where val is missing value and target has none val is already 
* equal to missing value but target needs to be changed
*/
		target_md->multidval.missing_value.has_missing = 1;
		target_md->multidval.missing_value.value = value_md->multidval.missing_value.value;
		
	}
	while(!done) {
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		memcpy((void*)((char*)target_md->multidval.val+ (to * el_size)),val,el_size);
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
	return(NhlNOERROR);
}




static NhlErrorTypes MultiDVal_ReadWriteSection
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
	NclMultiDValData target_md = (NclMultiDValData)to_data;
	NclMultiDValData value_md = (NclMultiDValData)from_data;
/*
* This selection record applys to the target record and it represents a 
* mapping from the value object into target. 
*/

	int i,k;
	long from,to;
	NclSelection *to_sel_ptr = NULL;
	void *to_val;
	NclSelection *from_sel_ptr = NULL;
	void *from_val;

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

	int n_dims_value = 0;
	int total_elements_value = 1;
	int total_elements_target = 1;
	int n_dims_target = 0;
	int n_elem_target=0;
	int n_elem_value=0;

	int done = 0;
	int inc_done = 0;
	int chckmiss = 0;
	int el_size;
	logical tmpe =0 ;

	if((target_md == NULL)||(value_md == NULL) ) {
		return(NhlFATAL);
	}

	n_dims_value = value_md->multidval.n_dims;
	n_dims_target = target_md->multidval.n_dims;
	
	

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
		_Ncleq(target_md->multidval.type,(void*)&tmpe,(void*)&(target_md->multidval.missing_value.value),(void*)&(value_md->multidval.missing_value.value),NULL,NULL,1,1);
		if(tmpe) {
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
	el_size = target_md->multidval.type->type_class.size;

	
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

				if(to_sel_ptr->u.sub.stride >= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is greater than end and stride is positive, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem_target = (int)(((float)
					(to_sel_ptr->u.sub.start 
					- to_sel_ptr->u.sub.finish))
					/(float)fabs(((float)to_sel_ptr->u.sub.stride))) + 1;

/*
* Need to be able to determine which type of comparision < or > is needed to
* determine whether the finish has been passed up
*/
				to_compare_sel[i] = -1;

			} else {
				if(to_sel_ptr->u.sub.stride <= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is less than end and stride is negative, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem_target = (int)(((float)
					(to_sel_ptr->u.sub.finish 
					- to_sel_ptr->u.sub.start))
					/((float)to_sel_ptr->u.sub.stride)) + 1;
				to_compare_sel[i] = -2;
			}
			if((to_sel_ptr->u.sub.start > target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1)||(to_sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((to_sel_ptr->u.sub.finish > target_md->multidval.dim_sizes[to_sel_ptr->dim_num] - 1)||(to_sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			to_current_index[i] = to_sel_ptr->u.sub.start;
			to_strider[i] = to_sel_ptr->u.sub.stride;
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

				if(from_sel_ptr->u.sub.stride >= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is greater than end and stride is positive, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem_value = (int)(((float)
					(from_sel_ptr->u.sub.start 
					- from_sel_ptr->u.sub.finish))
					/(float)fabs(((float)from_sel_ptr->u.sub.stride))) + 1;

/*
* Need from be able from determine which type of comparision < or > is needed from
* determine whether the finish has been passed up
*/
				from_compare_sel[i] = -1;

			} else {
				if(from_sel_ptr->u.sub.stride <= 0 ) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Invalid stride: start is less than end and stride is negative, error in subscript #%d",i);
					return(NhlFATAL);
				}

				n_elem_value = (int)(((float)
					(from_sel_ptr->u.sub.finish 
					- from_sel_ptr->u.sub.start))
					/((float)from_sel_ptr->u.sub.stride)) + 1;
				from_compare_sel[i] = -2;
			}
			if((from_sel_ptr->u.sub.start > value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1)||(from_sel_ptr->u.sub.start < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}

			if((from_sel_ptr->u.sub.finish > value_md->multidval.dim_sizes[from_sel_ptr->dim_num] - 1)||(from_sel_ptr->u.sub.finish < 0)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript out of range, error in subscript #%d",i);
				return(NhlFATAL);
			}
			from_current_index[i] = from_sel_ptr->u.sub.start;
			from_strider[i] = from_sel_ptr->u.sub.stride;
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
	if(n_dims_target != n_dims_value) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Right hand side of assignment has (%d) dimensions and left hand side has (%d), dimension mismatch",n_dims_value,n_dims_target);
		return(NhlFATAL);
	}
	for(i = 0; i< n_dims_target; i++) {
		if(from_output_dim_sizes[i] != to_output_dim_sizes[i]) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension size mismatch, dimension (%d) of left hand side does not have the same size as the right hand side.",i);
			return(NhlFATAL);
		}
	}

	to_sel_ptr = to_selection->selection;
	from_sel_ptr = from_selection->selection;
	to_val = target_md->multidval.val;
	from_val = value_md->multidval.val;
	while(!done) {
		to = 0;
		from = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (to_current_index[i] * to_multiplier[i]);
		}
		for(i = 0; i < n_dims_value;i++) {
			from = from + (from_current_index[i] * from_multiplier[i]);
		}
		memcpy((void*)((char*)to_val + (to*el_size)),(void*)((char*)from_val + (from * el_size)),el_size);
		if(to_compare_sel[n_dims_target-1] <0) {
			to_current_index[n_dims_target -1 ] += to_strider[n_dims_target-1];
		} else {
			to_compare_sel[n_dims_target-1]++;
		}
		if(from_compare_sel[n_dims_value -1] <0) {
			from_current_index[n_dims_value -1 ] += from_strider[n_dims_value-1];
		} else {
			from_compare_sel[n_dims_value-1]++;
		}
		for(k = n_dims_target-1; k >0; k--) {
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
		for(k = n_dims_value -1; k >0; k--) {
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
	if(chckmiss) {
/*
* First case both value and target have missing values
* so value_md values that were missing and assigned are
* now incorrect in target using _Nclreset_mis works to 
* convert value_md missing values to the target_md 
* missing values
*/
		_Nclreset_mis(value_md->multidval.type,target_md->multidval.val,&(value_md->multidval.missing_value.value),&(target_md->multidval.missing_value.value),target_md->multidval.totalelements);
	
	} else if(value_md->multidval.missing_value.has_missing) {
/*
* This condition is when value has missing value that could've been
* assigned and target doesn't have a missing_value record.
*/
		target_md->multidval.missing_value.has_missing = 1;
		target_md->multidval.missing_value.value = value_md->multidval.missing_value.value;
	}
	return(NhlNOERROR);
}
static NhlErrorTypes MultiDValAddParent
#if	NhlNeedProto
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
	theobj->obj.parents->pptr = parent;
	theobj->obj.ref_count++;
	return(NhlNOERROR);
}

static NhlErrorTypes MultiDValDelParent
#if	NhlNeedProto
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
	while((tmp!=NULL)&&(tmp->pptr->obj.id == parent->obj.id)) {
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

static NclData MultiDValDup
#if	NhlNeedProto
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	void *new_val;
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData output_md = NULL;

	new_val = (void*)NclMalloc((unsigned int)self_md->multidval.totalsize);
	memcpy(new_val,self_md->multidval.val,self_md->multidval.totalsize);

	output_md = _NclCreateVal(
		NULL,
		NULL,
		self_md->obj.obj_type,
		self_md->obj.obj_type_mask,
		new_val,
		((self_md->multidval.missing_value.has_missing)? ((new_missing != NULL) ? new_missing : &(self_md->multidval.missing_value.value)) : NULL),
		self_md->multidval.n_dims,
		self_md->multidval.dim_sizes,
		TEMPORARY,
		NULL,
		(NclObjClass)self_md->multidval.type
	);	
	if(output_md != NULL) {
		if((new_missing != NULL)&&(self_md->multidval.missing_value.has_missing)) {
			_Nclreset_mis(output_md->multidval.type,output_md->multidval.val,&(self_md->multidval.missing_value.value),new_missing,self_md->multidval.totalelements);
			output_md->multidval.missing_value.has_missing = 1;
			output_md->multidval.missing_value.value = *new_missing;
		} 	
	}
	return((NclData)output_md);
}

NclData MultiDVal_md_Not
#if	NhlNeedProto
(NclData self,NclData result)
#else
(self,result)
NclData self;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclMissingRec themissing;
	NclMultiDValData output_md = NULL;
	NclTypeClass the_type;
	void *result_val;

	if(self_md == NULL) return(NULL);

	if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }	
	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.not != NULL)) {

/*
* the_type is not null since requirement if not != NULL not_type != NULL
*/

                the_type = _Nclnot_type(self_md->multidval.type);
                result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
                if(result_val == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Not: Could not allocate memory for result type, can't continue\n");
                        return(NULL);
                }
                if(_Nclnot(
                        self_md->multidval.type,
                        result_val,
                        self_md->multidval.val,
                        (self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
                        self_md->multidval.totalelements) != NhlFATAL) {

                        output_md = _NclCreateMultiDVal(
                                (NclObj)result_md,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                result_val,
                                (themissing.has_missing? &(themissing.value):NULL),
                                self_md->multidval.n_dims,
                                self_md->multidval.dim_sizes,
                                TEMPORARY,
                                NULL,
                                the_type
                        );
                        return((NclData)output_md);
                } else {
                        NclFree(result_val);
                        return(NULL);
                }
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Not: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}
static NclData MultiDVal_md_Neg
#if	NhlNeedProto
(NclData self,NclData result)
#else
(self,result)
NclData self;
NclData result;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData result_md = (NclMultiDValData)result;
	NclMissingRec themissing;
	NclMultiDValData output_md = NULL;
	NclTypeClass the_type;
	void *result_val;

	if(self_md == NULL) return(NULL);

	if(self_md->multidval.missing_value.has_missing) {
                themissing.value = self_md->multidval.missing_value.value;
                themissing.has_missing = 1;
        } else {
                themissing.has_missing = 0;
        }	

	if((self_md->multidval.type != NULL)&&(((NclTypeClass)self_md->multidval.type)->type_class.neg != NULL)) {

/*
* the_type is not null since requirement if neg != NULL neg_type != NULL
*/

                the_type = _Nclneg_type(self_md->multidval.type);
                result_val = (void*)NclMalloc(self_md->multidval.totalelements * the_type->type_class.size);
                if(result_val == NULL) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Neg: Could not allocate memory for result type, can't continue\n");
                        return(NULL);
                }
                if(_Nclneg(
                        self_md->multidval.type,
                        result_val,
                        self_md->multidval.val,
                        (self_md->multidval.missing_value.has_missing?(void*)&self_md->multidval.missing_value.value:NULL),
                        self_md->multidval.totalelements) != NhlFATAL) {

                        output_md = _NclCreateMultiDVal(
                                (NclObj)result_md,
                                NULL,
                                Ncl_MultiDValData,
                                0,
                                result_val,
                                (themissing.has_missing? &(themissing.value):NULL),
                                self_md->multidval.n_dims,
                                self_md->multidval.dim_sizes,
                                TEMPORARY,
                                NULL,
                                the_type
                        );
                        return((NclData)output_md);
                } else {
                        NclFree(result_val);
                        return(NULL);
                }
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Neg: operation not supported on type (%s)",_NclBasicDataTypeToName(self_md->multidval.data_type));
		return(NULL);
	}
}

static int MultiDVal_IsMis
#if	NhlNeedProto
(NclData self, void* v_one)
#else
(self,v_one)
NclData self_md;
void *v_one;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	logical res;

	if((self_md != NULL)&&(self_md->multidval.type != NULL)&&(self_md->multidval.missing_value.has_missing)) {
		if(_Ncleq(self_md->multidval.type,(void*)&res,(void*)&self_md->multidval.missing_value.value,v_one,NULL,NULL,1,1) != NhlFATAL) {
			return((int)res);
		}
	}
	return(0);
}

static NclData MultiDVal_md_Coerce
#if	NhlNeedProto
(NclData self, NclObjTypes coerce_to_obj, NclScalar *new_missing)
#else
(self,coerce_to_obj,new_missing)
NclData self;
NclObjTypes coerce_to_obj;
NclScalar *new_missing;
#endif
{
	NclTypeClass to_type;
	NclTypeClass from_type;
	NclMultiDValData self_md = (NclMultiDValData)self;
	NclMultiDValData output_md = NULL;
	void *result_val = NULL;
	int limit = 1,from = 0;
	int n_dims = 0;
	int step = 0,i;
	int dimsizes[NCL_MAX_DIMENSIONS];

	if((self_md != NULL)&&(self_md->multidval.type != NULL)) {
		from_type = self_md->multidval.type;
		to_type = _NclTypeEnumToTypeClass(coerce_to_obj);
		if(self_md->multidval.type == to_type) {
			if((self_md->multidval.missing_value.has_missing)&&(new_missing != NULL)) 
				_NclResetMissingValue(self_md,new_missing);
			return(self);
		} else {
			if(to_type != (NclTypeClass)nclTypeClass) {
				if((from_type == (NclTypeClass)nclTypecharClass)&&(to_type == (NclTypeClass)nclTypestringClass)){
					for(i = 0; i < self_md->multidval.n_dims - 1; i++) {
						limit *= self_md->multidval.dim_sizes[i];
						dimsizes[i] = self_md->multidval.dim_sizes[i];
					}
					n_dims = self_md->multidval.n_dims - 1;
					if(n_dims == 0) {
						n_dims = 1;
						dimsizes[i] = 1;
					}
					step = self_md->multidval.dim_sizes[self_md->multidval.n_dims - 1];
					result_val = (void*)NclMalloc(to_type->type_class.size*limit);
					if(result_val != NULL) {
						for(i = 0; i < limit; i++) {
							if(_Nclcoerce(
								to_type,
								(void*)&((NclQuark*)result_val)[i],
								(void*)&((char*)self_md->multidval.val)[from],
								self_md->multidval.totalelements,
								(self_md->multidval.missing_value.has_missing?&self_md->multidval.missing_value.value:NULL),
								new_missing,
								from_type) == NhlFATAL) {
								
								NclFree(result_val);
							}
							from += step;
						}
						output_md = _NclCreateVal(
							NULL,
							NULL,
							self_md->obj.obj_type,
							self_md->obj.obj_type_mask,
							result_val,
							new_missing,
							n_dims,
							(int*)dimsizes,
							TEMPORARY,
							NULL,
							(NclObjClass)to_type
							);
						return((NclData)output_md);
					}
				} else {
					result_val = (void*)NclMalloc(to_type->type_class.size*self_md->multidval.totalelements);
					if(result_val != NULL) {
						if(_Nclcoerce(
							to_type,
							result_val,
							self_md->multidval.val,
							self_md->multidval.totalelements,
							(self_md->multidval.missing_value.has_missing?&self_md->multidval.missing_value.value:NULL),
							new_missing,
							from_type) != NhlFATAL) {
							
							output_md = _NclCreateVal(
								NULL,
								NULL,
								self_md->obj.obj_type,
								self_md->obj.obj_type_mask,
								result_val,
								new_missing,
								self_md->multidval.n_dims,
								self_md->multidval.dim_sizes,
								TEMPORARY,
								NULL,
								(NclObjClass)to_type
								);
							return((NclData)output_md);
						} else {
							NclFree(result_val);
						}
					}
				}
			}
		} 
	}
	return(NULL);
}

static void MultiDValResetMissing
#if	NhlNeedProto
(NclData self, NclScalar * missing)
#else
(self, missing)
NclData self;
NclScalar * missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	
	if((self_md == NULL)||(missing == NULL)) return;

	if(!self_md->multidval.missing_value.has_missing) {
		self_md->multidval.missing_value.has_missing = 1;
		self_md->multidval.missing_value.value = *missing;
		return;
	}

	if(self_md->multidval.type == NULL) return;

	if(_Nclreset_mis(self_md->multidval.type,self_md->multidval.val,&(self_md->multidval.missing_value.value),missing,self_md->multidval.totalelements) != NhlFATAL) {
		self_md->multidval.missing_value.value = *missing;
	}
	return;
}

static void MultiDValPrint
#if     NhlNeedProto
(NclObj self, FILE *fp)
#else
(self,fp)
NclObj self;
FILE *fp;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
        int i[NCL_MAX_DIMENSIONS];
        int j[NCL_MAX_DIMENSIONS];
        int k,where,done = 0;
        int ndims = self_md->multidval.n_dims;
	int el_size;


	el_size = self_md->multidval.type->type_class.size;
        for(k = 0; k < self_md->multidval.n_dims; k++) {
                i[k] = 0;
                j[k] = self_md->multidval.dim_sizes[k];
        }
        while(!done) {
                where = 0;
                nclfprintf(fp,"(");
                for(k = 0; k < ndims - 1; k++) {
                        nclfprintf(fp,"%d,",i[k]);
                        where = (where + i[k]) * j[k+1];
                }
                nclfprintf(fp,"%d)\t",i[ndims-1]);
                where = where + i[ndims - 1];

	
		_Nclprint(self_md->multidval.type,fp,(void*)((char*)self_md->multidval.val + (where * el_size)));
		nclfprintf(fp,"\n");
                i[ndims - 1]++;
                if(i[ndims - 1] == j[ndims - 1]) {
                        for(k=ndims - 1;k >0;k--) {
                                if(i[k] == j[k]) {
                                        i[k] = 0;
                                        i[k-1]++;
                                }
                        }
                        if(i[0] == j[0]) done = 1;
                }
        }
        return;
}

static void MultiDValDestroy
#if     NhlNeedProto
(NclObj self)
#else
(self)
        NclObj  self;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;

        _NclUnRegisterObj(self);

        if(self_md->multidval.sel_rec != NULL) {
                NclFree(self_md->multidval.sel_rec);
        }

        if((self_md->obj.status != STATIC)&&(self_md->multidval.val != NULL)) {
                NclFree(self_md->multidval.val);
        }
        NclFree(self);
        return;
}


NclMultiDValDataClassRec nclMultiDValDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	MultiDValDestroy,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	NULL,
		(NclAddParentFunction)MultiDValAddParent,
                (NclDelParentFunction)MultiDValDelParent,
	/* NclPrintFunction print; 	*/	MultiDValPrint	
	},
	{
/* NclGenericFunction dup; 	*/	MultiDValDup,
/* NclResetMissingValueFuction dup;	*/	MultiDValResetMissing,
/* NclReadSubSecFunction r_subsection */ 	MultiDValReadSection,
/* NclReadSubSecFunction w_subsection */ {	
						MultiDVal_md_WriteSection,
						MultiDVal_s_WriteSection
					},
/* NclReadThenWriteSubFunc w_subsection */ MultiDVal_ReadWriteSection,
/* NclDataFunction coerce; 	*/	{
						MultiDVal_md_Coerce,
						MultiDVal_md_Coerce
					},
/* NclDataFunction multiply; 	*/	{
						MultiDVal_mdmd_Mul,
						MultiDVal_ss_Mul,
						MultiDVal_ss_Mul,
						MultiDVal_ss_Mul
					},
/* NclDataFunction plus; 	*/	{
						MultiDVal_mdmd_Plus,
						MultiDVal_ss_Plus,
						MultiDVal_ss_Plus,
						MultiDVal_ss_Plus
					},
/* NclDataFunction minus; 	*/	{	
						MultiDVal_mdmd_Minus,	
						MultiDVal_ss_Minus,	
						MultiDVal_ss_Minus,	
						MultiDVal_ss_Minus
					},
/* NclDataFunction divide; 	*/	{
						MultiDVal_mdmd_Div,
						MultiDVal_ss_Div,
						MultiDVal_ss_Div,
						MultiDVal_ss_Div
					},
/* NclDataFunction exponent; 	*/	{
                                                MultiDVal_mdmd_Exp,
                                                MultiDVal_ss_Exp,
                                                MultiDVal_ss_Exp,
                                                MultiDVal_ss_Exp
					},
/* NclDataFunction mod; 	*/	{
                                                MultiDVal_mdmd_Mod,
                                                MultiDVal_ss_Mod,
                                                MultiDVal_ss_Mod,
                                                MultiDVal_ss_Mod
					},
/* NclDataFunction mat; 	*/	{
                                                NULL,
                                                NULL,
                                                NULL,
                                                NULL 
					},
/* NclDataFunction sel_lt; 	*/	{
                                                MultiDVal_mdmd_SelLt,
                                                MultiDVal_ss_SelLt,
                                                MultiDVal_ss_SelLt,
                                                MultiDVal_ss_SelLt
					},
/* NclDataFunction sel_gt; 	*/	{
                                                MultiDVal_mdmd_SelGt,
                                                MultiDVal_ss_SelGt,
                                                MultiDVal_ss_SelGt,
                                                MultiDVal_ss_SelGt
					},
/* NclDataFunction not; 	*/	{
						MultiDVal_md_Not,
						MultiDVal_md_Not
					},
/* NclDataFunction neg; 	*/	{	
						MultiDVal_md_Neg,
						MultiDVal_md_Neg
					},
/* NclDataFunction gt; 		*/	{
                                                MultiDVal_mdmd_Gt,
                                                MultiDVal_ss_Gt,
                                                MultiDVal_ss_Gt,
                                                MultiDVal_ss_Gt
					},
/* NclDataFunction lt; 		*/	{
                                                MultiDVal_mdmd_Lt,
                                                MultiDVal_ss_Lt,
                                                MultiDVal_ss_Lt,
                                                MultiDVal_ss_Lt
					},
/* NclDataFunction ge; 		*/	{
                                                MultiDVal_mdmd_Ge,
                                                MultiDVal_ss_Ge,
                                                MultiDVal_ss_Ge,
                                                MultiDVal_ss_Ge
					},
/* NclDataFunction le; 		*/	{
                                                MultiDVal_mdmd_Le,
                                                MultiDVal_ss_Le,
                                                MultiDVal_ss_Le,
                                                MultiDVal_ss_Le
					},
/* NclDataFunction ne; 		*/	{
                                                MultiDVal_mdmd_Ne,
                                                MultiDVal_ss_Ne,
                                                MultiDVal_ss_Ne,
                                                MultiDVal_ss_Ne
					},
/* NclDataFunction eq; 		*/	{
                                                MultiDVal_mdmd_Eq,
                                                MultiDVal_ss_Eq,
                                                MultiDVal_ss_Eq,
                                                MultiDVal_ss_Eq
					},
/* NclDataFunction and;	 	*/	{
                                                MultiDVal_mdmd_And,
                                                MultiDVal_ss_And,
                                                MultiDVal_ss_And,
                                                MultiDVal_ss_And
					},
/* NclDataFunction or; 		*/	{
                                                MultiDVal_mdmd_Or,
                                                MultiDVal_ss_Or,
                                                MultiDVal_ss_Or,
                                                MultiDVal_ss_Or
					},
/* NclDataFunction xor;		*/	{
                                                MultiDVal_mdmd_Xor,
                                                MultiDVal_ss_Xor,
                                                MultiDVal_ss_Xor,
                                                MultiDVal_ss_Xor
					},
/* NclIsMissingFuncion is_mis   */	MultiDVal_IsMis
	},
	{
		NULL
	}
};

NclObjClass nclMultiDValDataClass = (NclObjClass)&nclMultiDValDataClassRec;


struct _NclMultiDValDataRec * _NclCreateMultiDVal
#if     NhlNeedProto
(NclObj inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, void *val, NclScalar *missing_value, int n_dims, int *dim_sizes, NclStatus status, NclSelectionRecord *sel_rec, NclTypeClass type)
#else
(inst, theclass, obj_type, obj_type_mask, val, missing_value, n_dims, dim_sizes, status, sel_rec, type)
NclObj inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
int *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
NclTypeClass type;
#endif
{

	NclMultiDValData thevalobj;
	NclObjClass class_ptr= nclMultiDValDataClass;
	int i;
	NhlErrorTypes ret1= NhlNOERROR;
	int nelem;

	ret1 = _NclInitClass(nclMultiDValDataClass);
	if(ret1 < NhlWARNING) {
		return(NULL);
	}
	if(inst == NULL ) {
		thevalobj = (NclMultiDValData)NclMalloc(
				(unsigned)nclMultiDValDataClassRec.obj_class.obj_size);
	} else {
		thevalobj = (NclMultiDValData)inst;
	}
	if(theclass != NULL) {
		class_ptr = theclass;
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/
	_NclDataCreate((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_MultiDValData),status);


	thevalobj->multidval.data_type = type->type_class.data_type;
	thevalobj->multidval.val = val;
	thevalobj->multidval.type= type;
	thevalobj->multidval.hlu_type_rep[0] = type->type_class.hlu_type_rep[0];
	thevalobj->multidval.hlu_type_rep[1] = type->type_class.hlu_type_rep[1];

	if(missing_value != NULL ) {
		thevalobj->multidval.missing_value.has_missing = 1;
			thevalobj->multidval.missing_value.value = *missing_value;
	} else {
		thevalobj->multidval.missing_value.has_missing = 0;
	}



	thevalobj->multidval.n_dims = n_dims;
	nelem = 1;
	if((n_dims == 1) &&(dim_sizes[0]== 1)) {
		thevalobj->multidval.kind = SCALAR;
		thevalobj->multidval.dim_sizes[0] = 1;
	} else {
		thevalobj->multidval.kind = MULTID;
		for(i = 0; i<n_dims; i++) {
			thevalobj->multidval.dim_sizes[i] = dim_sizes[i];
			nelem *= dim_sizes[i];
		}
	}
	thevalobj->multidval.totalelements = nelem;
	thevalobj->multidval.totalsize = nelem * type->type_class.size;
	if(sel_rec != NULL) {
		thevalobj->multidval.sel_rec = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
		memcpy((char*)thevalobj->multidval.sel_rec,(char*)sel_rec,sizeof(NclSelectionRecord));
	} else {
		thevalobj->multidval.sel_rec = NULL;
	}
	return((NclMultiDValData)thevalobj);
}


/*
 *      $Id: NclMultiDValHLUObjData.c,v 1.2 1994-08-25 18:00:50 ethan Exp $
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
#include "defs.h"
#include <errno.h>
#include "NclHLUObj.h"
#include "NclMultiDValHLUObjData.h"
#include "DataSupport.h"


static struct _NclDataRec *MultiDVal_HluObj_ReadSection
#if  __STDC__
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
	int *val;
	int i,k,from,to;
	NclScalar *tmp = NULL;

	long current_index[NCL_MAX_DIMENSIONS];
	long multiplier[NCL_MAX_DIMENSIONS];
	long compare_sel[NCL_MAX_DIMENSIONS];
	long strider[NCL_MAX_DIMENSIONS];
	int output_dim_sizes[NCL_MAX_DIMENSIONS];

	int total_elements = 1;
	int n_dims_input = self_md->multidval.n_dims;
	int n_elem=0;
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

	val = (int*)NclMalloc(total_elements * sizeof(int));
	to = 0;
	while(!done) {
		from = 0;
		for(i = 0; i < n_dims_input;i++) {
			from = from + (current_index[i] * multiplier[i]);
		}
		if(!chckmiss) {
			val[to] = ((int*)self_md->multidval.val)[from];
		} else {
			val[to] = (((int*)self_md->multidval.val)[from] == missing->intval) ? missing->intval:((int*)self_md->multidval.val)[from];
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
#if  __STDC__
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
	int *val;
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
		if(target_md->multidval.missing_value.value.intval ==
			value_md->multidval.missing_value.value.intval) {
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
			if(chckmiss) {
				val = (int*)value_md->multidval.val;
				for(i = 0; i< target_md->multidval.totalelements; i++) {
					((int*)target_md->multidval.val)[i] = 
						((val[i] == value_md->multidval.missing_value.value.intval) ? 
						target_md->multidval.missing_value.value.intval 
						: val[i]);
				}
			} else {
				memcpy(target_md->multidval.val,value_md->multidval.val,value_md->multidval.totalsize);
				return(NhlNOERROR);
			}
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

	val = (int*)value_md->multidval.val;
	from = 0;
	while(!done) {
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		if(chckmiss) {
			((int*)target_md->multidval.val)[to] = 
				((val[from] == value_md->multidval.missing_value.value.intval) ? 
				target_md->multidval.missing_value.value.intval 
				: val[from]);
		} else {
			((int*)target_md->multidval.val)[to] = val[from];
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
	return(NhlNOERROR);
}
static NhlErrorTypes MultiDVal_HLUObj_s_WriteSection
#if  __STDC__
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
	NclSelection *sel_ptr = sel->selection;
	int *val;
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
		if(target_md->multidval.missing_value.value.intval ==
			value_md->multidval.missing_value.value.intval) {
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

	val = (int*)value_md->multidval.val;
	if(chckmiss) {
		if(*val == value_md->multidval.missing_value.value.intval){
			*val = target_md->multidval.missing_value.value.intval;
		}
	}
	while(!done) {
		to = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (current_index[i] * multiplier[i]);
		}
		((int*)target_md->multidval.val)[to] = *val;
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



static void MultiDVal_HLUObj_ResetMissing
#if  __STDC__
(NclData self, NclScalar *missing)
#else
(self,missing)
	NclData self;
	NclScalar *missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData)self;
	int theval;
	int oldval;
	int *step;
	int i;

	if(missing == NULL) {	
		return;
	} else {
		if(self_md->multidval.missing_value.has_missing) {
			oldval = self_md->multidval.missing_value.value.intval;
			theval = missing->intval;
			step = (int*)self_md->multidval.val;
			for(i = 0; i< self_md->multidval.totalelements; i++) {
				if(*step == oldval) {
					*step = theval;
				}
				step++;
			}
			self_md->multidval.missing_value.value.intval = theval;
			self_md->multidval.missing_value.has_missing = 1;
		} else {
			self_md->multidval.missing_value.value.intval = missing->intval;
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
#if  __STDC__
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
	int *to_val;
	NclSelection *from_sel_ptr = NULL;
	int *from_val;

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
		if(target_md->multidval.missing_value.value.intval ==
			value_md->multidval.missing_value.value.intval) {
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
	to_val = (int*)target_md->multidval.val;
	from_val = (int*)value_md->multidval.val;
	while(!done) {
		to = 0;
		from = 0;
		for(i = 0; i < n_dims_target;i++) {
			to = to + (to_current_index[i] * to_multiplier[i]);
		}
		for(i = 0; i < n_dims_value;i++) {
			from = from + (from_current_index[i] * from_multiplier[i]);
		}
		if(chckmiss) {
			to_val[to] = 
				((from_val[from] == value_md->multidval.missing_value.value.intval) ? 
				target_md->multidval.missing_value.value.intval 
				: from_val[from]);
		} else {
			to_val[to] = from_val[from];
		}
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
	return(NhlNOERROR);
}

static void MultiDVal_HLUObj_Destroy
#if  __STDC__
(NclObj self)
#else
(self)
        NclObj  self;
#endif
{
        NclMultiDValData self_md = (NclMultiDValData)self;
	NclHLUObj tmp_ho;
	int *obj_ids,i;

	_NclUnRegisterObj(self);
	
	if(self_md->multidval.sel_rec != NULL) {
		NclFree(self_md->multidval.sel_rec);
	}

	obj_ids = (int*)self_md->multidval.val;	
	for(i = 0; i< self_md->multidval.totalelements; i++) {
		tmp_ho = (NclHLUObj)_NclGetObj(obj_ids[i]);
		(void)_NclDelParent((NclObj)tmp_ho,self);
	}
	
	if((self_md->obj.status != STATIC)&&(self_md->multidval.val != NULL)) {
		NclFree(self_md->multidval.val);
	}
	NclFree(self);
	return;
}

static NclData NclMultiDValhluDup
#if  __STDC__
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData) self;
	int *toval;
	int *frval;
	int missing;
	NclScalar themissing;
	int i;
	toval = (int*)NclMalloc(self_md->multidval.totalsize);
	frval = (int*)self_md->multidval.val;
	if(toval == NULL) {
		return(NULL);
        }
	if((new_missing == NULL)||(!self_md->multidval.missing_value.has_missing)) {
		memcpy((char*)toval,(char*)frval,self_md->multidval.totalsize);
		themissing = self_md->multidval.missing_value.value;
	} else {
		missing = self_md->multidval.missing_value.value.intval;
		for(i = 0; i < self_md->multidval.totalelements; i++) {
			toval[i] = (frval[i] == missing ?
				new_missing->intval :
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



NclMultiDValHLUObjDataClassRec nclMultiDValHLUObjDataClassRec = {
	{
/* char *class_name; 		*/	"MultiDValHLUObjData",
/* unsigned int obj_size;	*/	sizeof(NclMultiDValHLUObjDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclMultiDValDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	MultiDVal_HLUObj_Destroy,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	NULL,
	(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
	/* NclPrintFunction print; 	*/	NULL
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
/* NclDataFunction xor;		*/	{NULL,NULL,NULL,NULL}
	},
	{
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL,
		NULL
	}
	
};

NclObjClass nclMultiDValHLUObjDataClass = (NclObjClass)&nclMultiDValHLUObjDataClassRec;


struct _NclMultiDValDataRec * _NclMultiDValHLUObjDataCreate
#if __STDC__
(NclObj inst,NclObjClass theclass,NclObjTypes obj_type,unsigned int obj_type_mask,void *val,NclScalar *missing_value,int n_dims, int *dim_sizes,NclStatus status,NclSelectionRecord *sel_rec)
#else
(inst,theclass,obj_type,obj_type_mask, val,missing_value,n_dims,dim_sizes,status,sel_rec)
NclObj inst ;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
int *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
#endif
{
	NclMultiDValHLUObjData thevalobj;
	NclObjClass class_ptr= nclMultiDValHLUObjDataClass;
	int i;
	NhlErrorTypes ret1= NhlNOERROR;
	int nelem;
	int *obj_ids;
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
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/
	_NclMultiDValDataCreate((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_MultiDValHLUObjData),status);


	thevalobj->multidval.data_type = NCL_obj;
	thevalobj->multidval.val = val;

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
	thevalobj->multidval.totalsize = nelem * _NclSizeOf(NCL_int);
	if(sel_rec != NULL) {
		thevalobj->multidval.sel_rec = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
		memcpy((char*)thevalobj->multidval.sel_rec,(char*)sel_rec,sizeof(NclSelectionRecord));
	} else {
		thevalobj->multidval.sel_rec = NULL;
	}
	thevalobj->multidval.hlu_type_rep[0] = NULL;
	thevalobj->multidval.hlu_type_rep[1] = NULL;
	obj_ids = (int*)thevalobj->multidval.val;
	for(i = 0; i<thevalobj->multidval.totalelements; i++) {
		tmp_ho = (NclHLUObj)_NclGetObj(obj_ids[i]);
		if((tmp_ho != NULL)&&(tmp_ho->obj.obj_type_mask & Ncl_HLUObj)){
			(void)_NclAddParent((NclObj)tmp_ho,(NclObj)thevalobj);
		}
	}
	
	return((NclMultiDValData)thevalobj);
}

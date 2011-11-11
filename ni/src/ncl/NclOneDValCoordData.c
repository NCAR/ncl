/*
 *      $Id: NclOneDValCoordData.c,v 1.14 2008-12-10 20:12:17 dbrown Exp $
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
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include <errno.h>
#include "NclFile.h"
#include "NclOneDValCoordData.h"
#include "DataSupport.h"
#include <math.h>
#include "NclTypeobj.h"
#include "TypeSupport.h"


static NhlErrorTypes NclOneDValGetClosestIndex
#if	NhlNeedProto
(NclMultiDValData self_md,void *ind_val, long * ind)
#else
(self_md,ind_val, ind)
NclMultiDValData self_md;
void *ind_val;
long * ind;
#endif
{
	void *ind_ptr;
	void *coord_ptr;
	NclOneDValCoordData the_coord;
	NclTypeClass type_coord;
	double cmp_val;
	ng_size_t i;
	logical lres;
	NclScalar m1_res;
	NclScalar m2_res;

	if((self_md == NULL)||(ind_val == NULL)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetClosestIndex: Null values passed in");
		return(NhlFATAL);
	}
	the_coord = (NclOneDValCoordData)self_md;
	ind_ptr = ind_val;
	coord_ptr = self_md->multidval.val;
	type_coord = self_md->multidval.type;
	switch(the_coord->onedval.mono_type) {
	case NclINCREASING:
		i = 0;
		while(i < the_coord->multidval.totalelements) {
			_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),ind_ptr,NULL,NULL,16,&cmp_val);
			if(cmp_val >= 0 ){
				*ind= i;
				break;
			} else {
				i++;
			}
		}
		if(i == the_coord->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetClosestIndex: finish coordinate index out of range, can't continue");
			return(NhlFATAL);
		}
		if(cmp_val == 0) {
			return(NhlNOERROR);
		} else {
			if(i == 0) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetClosestIndex: finish coordinate index out of range, can't continue");
				return(NhlFATAL);
			}
			_Nclminus(type_coord,(void*)&m1_res,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),ind_ptr,NULL,NULL,1,1);
			_Nclminus(type_coord,(void*)&m2_res,ind_ptr,(void*)((char*)coord_ptr + ((i - 1) * type_coord->type_class.size)),NULL,NULL,1,1);
			_Ncllt(type_coord,(void*)&lres,(void*)&m1_res,(void*)&m2_res,NULL,NULL,1,1);
			if(lres) {
				*ind = i;
			} else {
				*ind = i-1;
			}
			return(NhlNOERROR);
		}
	case NclDECREASING:
		i = 0;
		while(i < the_coord->multidval.totalelements) {
			_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),ind_ptr,NULL,NULL,16,&cmp_val);
			if(cmp_val <= 0 ){
				*ind= i;
				break;
			} else {
				i++;
			}
		}
		if(i == the_coord->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetClosestIndex: finish coordinate index out of range, can't continue");
			return(NhlFATAL);
		}
		if(cmp_val == 0) {
			return(NhlNOERROR);
		} else {
			if(i == 0) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetClosestIndex: finish coordinate index out of range, can't continue");
				return(NhlFATAL);
			}
			_Nclminus(type_coord,(void*)&m1_res,ind_ptr,(void*)((char*)coord_ptr + ((i-1) * type_coord->type_class.size)),NULL,NULL,1,1);
			_Nclminus(type_coord,(void*)&m2_res,(void*)((char*)coord_ptr + ((i) * type_coord->type_class.size)),ind_ptr,NULL,NULL,1,1);
			_Nclge(type_coord,(void*)&lres,(void*)&m1_res,(void*)&m2_res,NULL,NULL,1,1);
			if(lres) {
				*ind = i-1;
			} else {
				*ind = i;
			}
			return(NhlNOERROR);
		}
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetClosestIndex: Non-monotonic coordinate value being used, can't complete coordinate subscript");
		return(NhlFATAL);
		
	}
	
}

static NhlErrorTypes NclOneDValGetRangeIndex
#if	NhlNeedProto
(NclMultiDValData coord_md, void *start_val, void *finish_val, long *start, long *finish)
#else
(coord_md, start_md, finish_md, start, finish)
NclMultiDValData coord_md;
void *start_val;
void *finish_val;
long *start;
long *finish;
#endif
{
	void* start_ptr;
	void* finish_ptr;
	void* coord_ptr;
	NclOneDValCoordData the_coord;
	NclTypeClass type_coord;
	logical result;
	double cmp_val;
	ng_size_t i=0;

	if((coord_md == NULL)||!(coord_md->obj.obj_type_mask & Ncl_OneDValCoordData)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: Non-Coordinate type value object passed, can't continue"); 
		return(NhlFATAL);
	} else {
		the_coord = (NclOneDValCoordData)coord_md;
		coord_ptr = coord_md->multidval.val;
	}
	if((start_val == NULL)&&(finish_val == NULL)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: Missing either both start and end subscript values"); 
		return(NhlFATAL);
	} else if(start_val == NULL) {
		finish_ptr = finish_val;
		type_coord = coord_md->multidval.type;

	
		*start = 0;
		switch(the_coord->onedval.mono_type) {
		case NclINCREASING:
			i = 0;
			while(i < the_coord->multidval.totalelements) {
				result = 0;
				_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),finish_ptr,NULL,NULL,16,&cmp_val);
				if(cmp_val > 0 ){
					*finish = i - 1;
					if((cmp_val > 0)&&(i==0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: finish coordinate index out of range, can't continue");
						return(NhlFATAL);
					}
					break;
				} else {
					i++;
				}
			}
			if(i == the_coord->multidval.totalelements) {
				*finish = i - 1;
			}
			return(NhlNOERROR);
		case NclDECREASING:
			i = 0;
			while(i < the_coord->multidval.totalelements) {
				result = 0;
				_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),finish_ptr,NULL,NULL,16,&cmp_val);
				if(cmp_val < 0 ){
					*finish= i - 1;
					if((cmp_val < 0)&&(i==0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: finish coordinate index out of range, can't continue");
						return(NhlFATAL);
					}
					break;
				} else {
					i++;
				}
			}
			if(i == the_coord->multidval.totalelements) {
				*finish = i - 1;
			}
			return(NhlNOERROR);
		default:
		 	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: Non-monotonic coordinate value being used, can't complete coordinate subscript");
			return(NhlFATAL);
		}


	} else if(finish_val == NULL) {
		start_ptr= start_val;
		type_coord = coord_md->multidval.type;

		*finish = the_coord->multidval.totalelements - 1;
		switch(the_coord->onedval.mono_type) {
		case NclINCREASING:
			i = 0;
			while(i < the_coord->multidval.totalelements) {
				result = 0;
				_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),start_ptr,NULL,NULL,16,&cmp_val);
				if(cmp_val >= 0 ){
					*start = i;
					break;
				} else {
					i++;
				}
			}
			if(i == the_coord->multidval.totalelements) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: start coordinate index out of range, can't continue");
				return(NhlFATAL);
			}
			return(NhlNOERROR);
		case NclDECREASING:
			i = 0;
			while(i < the_coord->multidval.totalelements) {
				result = 0;
				_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),start_ptr,NULL,NULL,16,&cmp_val);
				if(cmp_val <= 0 ){
					*start = i;
					break;
				} else {
					i++;
				}
			}
			if(i == the_coord->multidval.totalelements) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: start coordinate index out of range, can't continue");
				return(NhlFATAL);
			}
			return(NhlNOERROR);
		default:
		 	NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: Non-monotonic coordinate value being used, can't complete coordinate subscript");
			return(NhlFATAL);
		}
	} else {
		finish_ptr = finish_val;
		start_ptr= start_val;
		type_coord = coord_md->multidval.type;
	
		_Ncleq(type_coord,(void*)&result,start_ptr,finish_ptr,NULL,NULL,1,1);
		if(result) {
/*
* Single Subscript Case
*/
			if(NclOneDValGetClosestIndex(coord_md,finish_val,finish) == NhlFATAL) {
				return(NhlFATAL);
			}
			*start = *finish;
			return(NhlNOERROR);
		} else {
/*
* Actual range specified
*/
			_Ncllt(type_coord,(void*)&result,start_ptr,finish_ptr,NULL,NULL,1,1);
			switch(the_coord->onedval.mono_type) {
			case NclINCREASING:
				if(result) {
					i = 0;
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + (i * type_coord->type_class.size)),start_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val >= 0 ){
							*start = i;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: start coordinate index out of range, can't continue");
						return(NhlFATAL);
					}
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),finish_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val > 0 ){
							*finish = i - 1;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements) {
						*finish = i - 1;
					}
					return(NhlNOERROR);
				} else {
					i = 0;
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),finish_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val >= 0 ){
							*finish = i;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: finish coordinate index out of range, can't continue");
						return(NhlFATAL);
					}
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),start_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val > 0 ){
							*start = i - 1;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements) {
						*start = i - 1;
					}
					return(NhlNOERROR);
				} 
			case NclDECREASING:
				if(result) {
					i = 0;
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),finish_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val <= 0 ){
							*finish= i;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: finish coordinate index out of range, can't continue");
						return(NhlFATAL);
					}
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),start_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val < 0 ){
							*start = i - 1;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements){
						*start = i -1;
					}
					return(NhlNOERROR);
				} else {
					i = 0;
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),start_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val <= 0 ){
							*start= i;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: start coordinate index out of range, can't continue");
						return(NhlFATAL);
					}
					while(i < the_coord->multidval.totalelements) {
						result = 0;
						_Nclcmpf(type_coord,(void*)((char*)coord_ptr + i * type_coord->type_class.size),finish_ptr,NULL,NULL,16,&cmp_val);
						if(cmp_val < 0 ){
							*finish= i - 1;
							break;
						} else {
							i++;
						}
					}
					if(i == the_coord->multidval.totalelements){
						*finish = i - 1;
					}
					return(NhlNOERROR);
				} 
			default:
				NhlPError(NhlFATAL,NhlEUNKNOWN,"NclOneDValGetRangeIndex: Non-monotonic coordinate value being used, can't complete coordinate subscript");
				return(NhlFATAL);
			}
		}
	}
}


static NclData NclOneDValCoordDup
#if	NhlNeedProto
(NclData self,NclScalar *new_missing)
#else
(self,new_missing)
NclData self;
NclScalar *new_missing;
#endif
{
	NclMultiDValData self_md = (NclMultiDValData) self;
	ng_size_t *toval;
	ng_size_t *frval;
	NclScalar themissing;
	NclMultiDValData output_md = NULL;
	toval = (ng_size_t *)NclMalloc(self_md->multidval.totalsize);
	frval = (ng_size_t *)self_md->multidval.val;
	if(toval == NULL) {
		return(NULL);
        }
	memcpy((char*)toval,(char*)frval,self_md->multidval.totalsize);
	themissing = self_md->multidval.missing_value.value;
        output_md = _NclOneDValCoordDataCreate(
                NULL,
                NULL,
                Ncl_OneDValCoordData,
                0,
                (void*)toval,
                (self_md->multidval.missing_value.has_missing ? &themissing : NULL),
                self_md->multidval.n_dims,
                self_md->multidval.dim_sizes,
                TEMPORARY,
                NULL,
		self_md->multidval.type);
	if(output_md != NULL ) {
		if((new_missing != NULL)&&(self_md->multidval.missing_value.has_missing)) {
			_Nclreset_mis(output_md->multidval.type,output_md->multidval.val,&(self_md->multidval.missing_value.value),new_missing,self_md->multidval.totalelements);
			output_md->multidval.missing_value.has_missing = 1;
			output_md->multidval.missing_value.value = *new_missing;
		}
	}
	return((NclData)output_md);
}
static NhlErrorTypes OneDValCoordWrite_s
#if     NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
        NclData target;
        NclSelectionRecord *sel;
        NclData value;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclOneDValCoordData thevalobj = (NclOneDValCoordData) target;


	if((target->obj.class_ptr != NULL)
		&&(target->obj.class_ptr->obj_class.super_class !=NULL)
		&&(((NclDataClass)target->obj.class_ptr->obj_class.super_class)->data_class.w_subsection[1] !=NULL)) {
	
		ret = (*((NclDataClass)target->obj.class_ptr->obj_class.super_class)->data_class.w_subsection[1])(target,sel,value);
		
		thevalobj->onedval.mono_type = _Nclis_mono(thevalobj->multidval.type,thevalobj->multidval.val,
			(thevalobj->multidval.missing_value.has_missing?&thevalobj->multidval.missing_value.value:NULL),
			thevalobj->multidval.totalelements);

		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"OneDValCoordWrite_s: an error occurred while trying to write to a coordinate value");
		return(NhlFATAL);
	}
}
static NhlErrorTypes OneDValCoordWrite_md
#if     NhlNeedProto
(NclData target, NclSelectionRecord * sel, struct _NclDataRec* value)
#else
(target,sel,value)
        NclData target;
        NclSelectionRecord *sel;
        NclData value;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclOneDValCoordData thevalobj = (NclOneDValCoordData) target;


	if((target->obj.class_ptr != NULL)
		&&(target->obj.class_ptr->obj_class.super_class !=NULL)
		&&(((NclDataClass)target->obj.class_ptr->obj_class.super_class)->data_class.w_subsection[0] !=NULL)) {
	
		ret = (*((NclDataClass)target->obj.class_ptr->obj_class.super_class)->data_class.w_subsection[0])(target,sel,value);
		
		thevalobj->onedval.mono_type = _Nclis_mono(thevalobj->multidval.type,thevalobj->multidval.val,
			(thevalobj->multidval.missing_value.has_missing?&thevalobj->multidval.missing_value.value:NULL),
			thevalobj->multidval.totalelements);

		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"OneDValCoordWrite_md: an error occurred while trying to write to a coordinate value");
		return(NhlFATAL);
	}
}
static NhlErrorTypes OneDValCoordReadWrite
#if     NhlNeedProto
(NclData to_data, NclSelectionRecord * to_selection, NclData from_data, NclSelectionRecord *from_selection)
#else
(to_data,to_selection,from_data,from_selection)
NclData to_data;
NclSelectionRecord * to_selection;
NclData from_data;
NclSelectionRecord *from_selection;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclOneDValCoordData thevalobj = (NclOneDValCoordData) to_data;


	if((to_data->obj.class_ptr != NULL)
		&&(to_data->obj.class_ptr->obj_class.super_class !=NULL)
		&&(((NclDataClass)to_data->obj.class_ptr->obj_class.super_class)->data_class.r_then_w_subsection !=NULL)) {
	
		ret = (*((NclDataClass)to_data->obj.class_ptr->obj_class.super_class)->data_class.r_then_w_subsection)(to_data,to_selection,from_data,from_selection);
		
		thevalobj->onedval.mono_type = _Nclis_mono(thevalobj->multidval.type,thevalobj->multidval.val,
			(thevalobj->multidval.missing_value.has_missing?&thevalobj->multidval.missing_value.value:NULL),
			thevalobj->multidval.totalelements);

		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"OneDValCoordReadWrite: an error occurred while trying to write to a coordinate value");
		return(NhlFATAL);
	}
}

static NhlErrorTypes InitializeOneDClass(
#if NhlNeedProto
void
#endif
);

NclOneDValCoordDataClassRec nclOneDValCoordDataClassRec = {
	{
/* char *class_name; 		*/	"OneDValCoordData",
/* unsigned int obj_size;	*/	sizeof(NclOneDValCoordDataRec),
/* NclObjClass 			*/	(NclObjClass)&nclMultiDValDataClassRec,
/* int inited			*/	0,
/* NclGenericFunction destroy; 	*/	NULL,
/* NclSetStatusFunction set_status; 	*/	NULL,
/* NclInitPartFunction initialize_part; 	*/	NULL,
/* NclInitClassFunction initialize_class; 	*/	InitializeOneDClass,
	(NclAddParentFunction)NULL,
	(NclDelParentFunction)NULL,
/* NclPrintSummaryFunction print_summary */ NULL,
/* NclPrintFunction print; 	*/	NULL,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   NULL
	},
	{
/* NclGenericFunction dup; 	*/	NclOneDValCoordDup,
/* NclResetMissingValueFuction dup;	*/	NULL,
/* NclReadSubSecFunction r_subsection */ NULL,
/* NclReadSubSecFunction w_subsection */{ OneDValCoordWrite_md, OneDValCoordWrite_s},
/* NclReadThenWriteSubFunc w_subsection */ OneDValCoordReadWrite,
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
/* NclCoordRangeFunction get_range_ind */	NclOneDValGetRangeIndex,
/* NclCoordClosestFunction get_closest_ind */	 NclOneDValGetClosestIndex
	}
};

NclObjClass nclOneDValCoordDataClass = (NclObjClass)&nclOneDValCoordDataClassRec;

static NhlErrorTypes InitializeOneDClass
#if NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_OneDValCoordData,
		(NclObjClass)&nclOneDValCoordDataClassRec
	);
	return(NhlNOERROR);
}

struct _NclMultiDValDataRec * _NclOneDValCoordDataCreate
#if	NhlNeedProto
(NclObj inst,NclObjClass theclass,NclObjTypes obj_type,unsigned int obj_type_mask,void *val,NclScalar *missing_value,int n_dims, ng_size_t *dim_sizes,NclStatus status,NclSelectionRecord *sel_rec,NclTypeClass type)
#else
(inst,theclass,obj_type,obj_type_mask, val,missing_value,n_dims,dim_sizes,status,sel_rec,type)
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
NclTypeClass type;
#endif
{
	NclOneDValCoordData thevalobj;
	NclObjClass class_ptr= nclOneDValCoordDataClass;
	NhlErrorTypes ret1= NhlNOERROR;

	ret1 = _NclInitClass(nclOneDValCoordDataClass);
	if(ret1 < NhlWARNING) {
		return(NULL);
	}
	if(inst == NULL ) {
		thevalobj = (NclOneDValCoordData)NclMalloc(
				(unsigned)nclOneDValCoordDataClassRec.obj_class.obj_size);
	} else {
		thevalobj = (NclOneDValCoordData)inst;
	}
	if(theclass != NULL) {
		class_ptr = theclass;
	}
/*
* Since no initialize functions exist for Obj and Data (meaningless because
* data has not instance record) fields must be assign manually here
*/
	if(n_dims > 1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclOneDValCoordDataCreate: Number of dimensions is greater than one, could not create coordinate data object");
		return(NULL);
	}
	if (dim_sizes[0] == 0)
		thevalobj->onedval.mono_type = NclNONMONO;
	else
		thevalobj->onedval.mono_type = _Nclis_mono(type,val,missing_value,*dim_sizes);

	if(!(thevalobj->onedval.mono_type & (NclINCREASING | NclDECREASING) )) {
		NhlPError(NhlINFO,NhlEUNKNOWN,"_NclOneDValCoordDataCreate: A non-monotonic value was passed in. Coordinate subscripting will not work on the coordinate object");
	}

	_NclCreateMultiDVal((NclObj)thevalobj,class_ptr,obj_type,(obj_type_mask | Ncl_OneDValCoordData),val,missing_value,n_dims,dim_sizes,status,sel_rec,(NclTypeClass)type);


	
	return((NclMultiDValData)thevalobj);
}

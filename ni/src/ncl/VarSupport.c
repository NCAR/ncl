/*
 *      $Id: VarSupport.c,v 1.14 1996-05-02 23:31:02 ethan Exp $
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
 *	Date:		Thu Jan 13 14:52:04 MST 1994
 *
 *	Description:	
 */

#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMultiDValData.h"
#include "Machine.h"
#include "NclVar.h"
#include "NclHLUVar.h"
#include "NclCoordVar.h"
#include "NclFileVar.h"
#include "VarSupport.h"
#include "DataSupport.h"


struct _NclVarRec* _NclVarNclCreate
#if	NhlNeedProto
(struct _NclVarRec * inst, struct _NclObjClassRec * theclass, NclObjTypes obj_type, unsigned int obj_type_mask, struct _NclSymbol  *thesym, struct _NclMultiDValDataRec *value, struct _NclDimRec *dim_info, int att_id, int* coords, NclVarTypes var_type, char *var_name,NclStatus status)
#else
(inst, theclass, obj_type, obj_type_mask, thesym, value, dim_info, att_id, coords, var_type, var_name,status)
struct _NclVarRec * inst;
struct _NclObjClassRec * theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
struct _NclSymbol  *thesym;
struct _NclMultiDValDataRec *value;
struct _NclDimRec *dim_info;
int att_id;
int* coords;
NclVarTypes var_type;
char *var_name;
NclStatus status;
#endif
{
	if(obj_type == Ncl_FileVar) {
		return(_NclFileVarCreate(inst, theclass, obj_type, obj_type_mask, thesym, value, dim_info, att_id, coords, var_type, var_name,status));
	} else if(obj_type == Ncl_CoordVar) {
		return(_NclCoordVarCreate(inst, theclass, obj_type, obj_type_mask, thesym, value, dim_info, att_id, coords, var_type, var_name,status));
	} else if(obj_type == Ncl_HLUVar) {
		return(_NclHLUVarCreate(inst, theclass, obj_type, obj_type_mask, thesym, value, dim_info, att_id, coords, var_type, var_name,status));
	} else {
		return(_NclVarCreate(inst, theclass, obj_type, obj_type_mask, thesym, value, dim_info, att_id, coords, var_type, var_name,status));
	}
}

NclObjTypes _NclGetVarRepValue
#if	NhlNeedProto
(struct _NclVarRec * self)
#else
(self)
struct _NclVarRec * self;
#endif
{
	NclVarClass vc;

	if(self == NULL) {
		return(Ncl_None);
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.rep_val != NULL) {
			return((*vc->var_class.rep_val)(self));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	} 
	
	return(Ncl_None);
}

struct _NclMultiDValDataRec* _NclCoerceVar
#if	NhlNeedProto
(struct _NclVarRec * self, NclObjTypes coerce_obj_to,NclScalar *new_missing)
#else
(self, coerce_obj_to,new_missing)
struct _NclVarRec * self; 
NclObjTypes coerce_obj_to;
NclScalar *new_missing;
#endif
{	
	NclVarClass vc;
	if(self  == NULL) {
		return(NULL);
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.var_coerce != NULL) {
			return((NclMultiDValData)(*vc->var_class.var_coerce)(self,coerce_obj_to,new_missing));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return((NclMultiDValData)NULL);
}

struct _NclVarRec *_NclVarRead
#if	NhlNeedProto
(struct _NclVarRec *var,struct _NclSelectionRecord * sel_ptr)
#else
(var,sel_ptr)
struct _NclVarRec *var;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclVarClass vc;
	if(var == NULL) {
		return(NULL);
	} else {
		vc = (NclVarClass)var->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.read_func != NULL) {
			return((*vc->var_class.read_func)(var,sel_ptr));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return(NULL);
}
struct _NclMultiDValDataRec *_NclVarValueRead
#if	NhlNeedProto
(struct _NclVarRec *var,struct _NclSelectionRecord * sel_ptr,NclScalar *new_missing)
#else
(var,sel_ptr,new_missing)
struct _NclVarRec *var;
struct _NclSelectionRecord *sel_ptr;
NclScalar *new_missing;
#endif
{
	NclVarClass vc;
	if(var == NULL) {
		return(NULL);
	} else {
		vc = (NclVarClass)var->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.read_val_func != NULL) {
			return((NclMultiDValData)(*vc->var_class.read_val_func)(var,sel_ptr,new_missing));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes  _NclBuildCoordVSelection
#if	NhlNeedProto
(struct _NclVarRec *var,struct _NclVecRec * vec, struct _NclSelection* sel,int  dim_num,char* dim_name)
#else
(var,vec,sel,dim_num,dim_name)
	struct _NclVarRec *var;
	struct _NclVecRec* vec;
	struct _NclSelection* sel;
	int dim_num;
	char * dim_name;
#endif
{	
	NclMultiDValData vect_md;
	long *thevector;
	int i;
	char * v_name;
	int index = -1;
	NclQuark cname;
	NclMultiDValData name_md = NULL,result_md = NULL,tmp_md = NULL,coord_md = NULL;
	long start = 0,finish = 0;
	NclCoordVar cvar = NULL;
	NclObjTypes the_type;
/*
* Preconditions: subscripts are SCALAR guarenteed!!!!
*/
	if(var->var.var_quark != -1) {
		v_name = NrmQuarkToString(var->var.var_quark);
	} else if(var->var.thesym != NULL) {
		v_name = var->var.thesym->name;
	} else {
		v_name = "unnamed";
	}

/*
* vec is guarenteed to be one dimensional, and of an integer type
*/
	vect_md = vec->vec;

	if(vect_md != NULL) {
		if(dim_name != NULL) {
			index = _NclIsDim(var,dim_name);
			if((index >= 0)&&(index < var->var.n_dims)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s), could not determine dimension number",dim_name,v_name);
				return(NhlFATAL);
			}
			cname = NrmStringToQuark(dim_name);
		} else {
			name_md = _NclReadDim(var,NULL,dim_num);
                        if(name_md != NULL) {
                                if(name_md->multidval.type->type_class.type & Ncl_Typestring) {
                                        cname = *(string*)name_md->multidval.val;
                                        _NclDestroyObj((NclObj)name_md);
                                } else {
                                        return(NhlFATAL);
                                }
                        } else {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) of (%s) is not named and therfore doesn't have an associated coordinate variable",dim_num,v_name);
                                return(NhlFATAL);
                        }
			sel->dim_num = dim_num;
		}
/*
* I don;t think there is anyway to get arround having to allocate the 
* vector again. Since I don't want to make any assumptions about how
* to free the objects without freeing the val field which I need to keep
* arround untill the actual ValueRead happens
*/
		if(!_NclIsCoord(var,NrmQuarkToString(cname))) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) of (%s) does not have an associated coordinate variable",NrmQuarkToString(cname),v_name);
                        return(NhlFATAL);
                }
		cvar = (NclCoordVar)_NclReadCoordVar(var,NrmQuarkToString(cname),NULL);
		coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
		the_type = _NclGetVarRepValue((NclVar)cvar);
		if(!(the_type & vect_md->multidval.type->type_class.type)){
			tmp_md = _NclCoerceData(vect_md,the_type,NULL);
			if(tmp_md == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable",dim_num);
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				return(NhlFATAL);
			} 
		} else {
			tmp_md = vect_md;
		}



		thevector = (long*)NclMalloc((unsigned)tmp_md->multidval.totalelements * sizeof(long));
		sel->sel_type = Ncl_VECSUBSCR;
		sel->u.vec.n_ind = tmp_md->multidval.totalelements;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(_NclGetCoordClosestIndex(coord_md,(void*)((char*)tmp_md->multidval.val + i * tmp_md->multidval.type->type_class.size),&(thevector[i])) == NhlFATAL) {
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				return(NhlFATAL);
			}
		}
		sel->u.vec.min = thevector[0];
		sel->u.vec.max = thevector[0];
		sel->u.vec.ind = thevector;
		for(i = 0; i < sel->u.vec.n_ind; i++) {
			if(thevector[i] > sel->u.vec.max) {
				sel->u.vec.max = thevector[i];
			}
			if(thevector[i] < sel->u.vec.min) {
				sel->u.vec.min = thevector[i];
			}
		}
		if((tmp_md != vect_md)&&(tmp_md->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)tmp_md);
		}
		if(coord_md->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)coord_md);
		}
		if(cvar->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)cvar);
		}
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}
}
NhlErrorTypes _NclBuildCoordRSelection
#if	NhlNeedProto
(struct _NclVarRec *var,struct _NclRangeRec * range, struct _NclSelection* sel,int  dim_num, char * dim_name)
#else
(var,range,sel,dim_num,dim_name)
	struct _NclVarRec *var;
	struct _NclRangeRec* range;
	struct _NclSelection* sel;
	int dim_num;
	char * dim_name;
#endif
{
	char * v_name = NULL;
	int index = -1;
	NclQuark cname;
	NclMultiDValData name_md = NULL,result_md = NULL,tmp_md = NULL,coord_md = NULL;
	long start = 0,finish = 0;
	NclCoordVar cvar = NULL;
	NclObjTypes the_type;
/*
* Preconditions: subscripts are SCALAR guarenteed!!!!
*/
	if(var->var.var_quark != -1) {
		v_name = NrmQuarkToString(var->var.var_quark);
	} else if(var->var.thesym != NULL) {
		v_name = var->var.thesym->name;
	} else {
		v_name = "unnamed";
	}

	if(range != NULL) {
		if(dim_name != NULL) {
			index = _NclIsDim(var,dim_name);
			if((index >= 0)&&(index < var->var.n_dims)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s), could not determine dimension number",dim_name,v_name);
				return(NhlFATAL);
			}
			cname = NrmStringToQuark(dim_name);
		} else {
			name_md = _NclReadDim(var,NULL,dim_num);
			if(name_md != NULL) {
				if(name_md->multidval.type->type_class.type & Ncl_Typestring) {
					cname = *(string*)name_md->multidval.val;
					_NclDestroyObj((NclObj)name_md);
				} else {
					return(NhlFATAL);
				}	
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) of (%s) is not named and therfore doesn't have an associated coordinate variable",dim_num,v_name);
				return(NhlFATAL);
			}
			sel->dim_num = dim_num;
		}
		if(!_NclIsCoord(var,NrmQuarkToString(cname))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) of (%s) does not have an associated coordinate variable",NrmQuarkToString(cname),v_name);
			return(NhlFATAL);
		}
		if((range->start == NULL)&&(range->finish == NULL)) {

			sel->sel_type = Ncl_SUB_ALL;
			sel->u.sub.start = 0;
			sel->u.sub.finish = 0;
			sel->u.sub.stride = 1;

		} else if(range->start == NULL) {

			sel->sel_type = Ncl_SUB_DEF_VAL;
			sel->u.sub.start = 0;
			sel->u.sub.stride = 1;
/*
* Compute sel->u.sub.finish from coordinate info
*/
			cvar = (NclCoordVar)_NclReadCoordVar(var,NrmQuarkToString(cname),NULL);
			coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
			the_type = _NclGetVarRepValue((NclVar)cvar);

			if(!(the_type & range->finish->multidval.type->type_class.type)){
				tmp_md = _NclCoerceData(range->finish,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coorced to type of coordinate variable",dim_num);
					if(coord_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)coord_md);
					}
					if(cvar->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar);
					}
					return(NhlFATAL);
				} else {
					if(range->finish->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)range->finish);
					}
					range->finish = tmp_md;
				}
			}
			if(_NclGetCoordRange(coord_md,NULL,range->finish->multidval.val,&sel->u.sub.start,&sel->u.sub.finish) == NhlFATAL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not obtain coordinate indexes, unable to perform subscript");
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				return(NhlFATAL);
			}

		} else if(range->finish == NULL) {
/*
* Compute sel->u.sub.start from coordinate info
*/

			sel->sel_type = Ncl_SUB_VAL_DEF;

			sel->u.sub.finish = 0;
			sel->u.sub.stride = 1;


			cvar = (NclCoordVar)_NclReadCoordVar(var,NrmQuarkToString(cname),NULL);
			coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
			the_type = _NclGetVarRepValue((NclVar)cvar);

			if(!(the_type & range->start->multidval.type->type_class.type)){
				tmp_md = _NclCoerceData(range->start,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coorced to type of coordinate variable",dim_num);
					if(coord_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)coord_md);
					}
					if(cvar->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar);
					}
					return(NhlFATAL);
				} else {
					if(range->start->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)range->start);
					}
					range->start = tmp_md;
				}
			}
			if(_NclGetCoordRange(coord_md,range->start->multidval.val,NULL,&sel->u.sub.start,&sel->u.sub.finish) == NhlFATAL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not obtain coordinate indexes, unable to perform subscript");
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				return(NhlFATAL);
			}

		} else {

/*
* Compute sel->u.sub.start and sel->u.sub.finish from coordinate info
* Have to check when they are equal so only one value is selected
*/
			sel->sel_type = Ncl_SUBSCR;
			cvar = (NclCoordVar)_NclReadCoordVar(var,NrmQuarkToString(cname),NULL);
			coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
			the_type = _NclGetVarRepValue((NclVar)cvar);
			if(!(the_type & range->start->multidval.type->type_class.type)){
				tmp_md = _NclCoerceData(range->start,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coorced to type of coordinate variable",dim_num);
					if(coord_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)coord_md);
					}
					if(cvar->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar);
					}
					return(NhlFATAL);
				} else {
					if(range->start->obj.status != PERMANENT) {
						if(range->start == range->finish) {	
							range->finish = tmp_md;
						}
						_NclDestroyObj((NclObj)range->start);
					}
					range->start = tmp_md;
				}
			}
			tmp_md = NULL;
			if(!(the_type & range->finish->multidval.type->type_class.type)){
				tmp_md = _NclCoerceData(range->finish,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coorced to type of coordinate variable",dim_num);
					if(coord_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)coord_md);
					}
					if(cvar->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar);
					}
					return(NhlFATAL);
				} else {
					if(range->finish->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)range->finish);
					}
					range->finish = tmp_md;
				}
			}

			if(_NclGetCoordRange(coord_md,range->start->multidval.val,range->finish->multidval.val,&sel->u.sub.start,&sel->u.sub.finish) == NhlFATAL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not obtain coordinate indexes, unable to perform subscript");
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				return(NhlFATAL);
			}

			if(sel->u.sub.start <= sel->u.sub.finish) {
				sel->u.sub.stride = 1;
			} else {
				sel->u.sub.stride = -1;
			}

		}
		if(range->stride != NULL) {
			if(!_NclScalarCoerce(
				range->stride->multidval.val,
				range->stride->multidval.data_type,
				&(sel->u.sub.stride),NCL_long)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				return(NhlFATAL);
			}

		} 
	} 
	if(coord_md->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)coord_md);
	}
	if(cvar->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)cvar);
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclBuildRSelection
#if	NhlNeedProto
(struct _NclVarRec *var,struct _NclRangeRec * range, struct _NclSelection* sel,int  dim_num, char * dim_name)
#else
(var,range,sel,dim_num,dim_name)
	struct _NclVarRec *var;
	struct _NclRangeRec* range;
	struct _NclSelection* sel;
	int dim_num;
	char * dim_name;
#endif
{
	char * v_name;
	int index = -1;
/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	if(var->var.var_quark != -1) {
		v_name = NrmQuarkToString(var->var.var_quark);
	} else if(var->var.thesym != NULL) {
		v_name = var->var.thesym->name;
	} else {
		v_name = "unnamed";
	}

	if(range != NULL) {
		if(dim_name != NULL) {
/*
*---------> code needed here <-------------------
*/
			index = _NclIsDim(var,dim_name);
			if((index >= 0)&&(index < var->var.n_dims)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s), could not determine dimension number",dim_name,v_name);
				return(NhlFATAL);
			}
		} else {
			sel->dim_num = dim_num;
		}
		if((range->start == NULL)&&(range->finish == NULL)) {

			sel->sel_type = Ncl_SUB_ALL;
			sel->u.sub.start = 0;
			sel->u.sub.finish = 0;
			sel->u.sub.stride = 1;

		} else if(range->start == NULL) {

			sel->sel_type = Ncl_SUB_DEF_VAL;
			sel->u.sub.start = 0;

			if(!_NclScalarCoerce(
				range->finish->multidval.val,
				range->finish->multidval.data_type,
				&(sel->u.sub.finish),NCL_long)) {
/*
* This shouldn't happen but it can't hurt to have an extra check here
*/
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
				return(NhlFATAL);
			}

			sel->u.sub.stride = 1;

		} else if(range->finish == NULL) {

			sel->sel_type = Ncl_SUB_VAL_DEF;

			if(!_NclScalarCoerce(
				range->start->multidval.val,
				range->start->multidval.data_type,
				&(sel->u.sub.start),NCL_long)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
				return(NhlFATAL);
				
			}

			sel->u.sub.finish = 0;
			sel->u.sub.stride = 1;

		} else {

			sel->sel_type = Ncl_SUBSCR;

			if(!_NclScalarCoerce(
				range->start->multidval.val,
				range->start->multidval.data_type,
				&(sel->u.sub.start),NCL_long)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
				return(NhlFATAL);
			}

			if(!_NclScalarCoerce(
				range->finish->multidval.val,
				range->finish->multidval.data_type,
				&(sel->u.sub.finish),NCL_long)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
				return(NhlFATAL);
			}

			if(sel->u.sub.start <= sel->u.sub.finish) {
				sel->u.sub.stride = 1;
			} else {
				sel->u.sub.stride = -1;
			}

		}
		if(range->stride != NULL) {
			if(!_NclScalarCoerce(
				range->stride->multidval.val,
				range->stride->multidval.data_type,
				&(sel->u.sub.stride),NCL_long)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
				return(NhlFATAL);
			}

		} 
	} 
	return(NhlNOERROR);
}


int _NclVarIsAtt
#if	NhlNeedProto
(struct _NclVarRec *self, char * attname)
#else
(self, attname)
struct _NclVarRec *self;
char * attname;
#endif
{
	NclVarClass vc;

	if(self == NULL) {
		return(0);
	} else {
		vc = (NclVarClass) self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.is_att_func != NULL) {
			return((*vc->var_class.is_att_func)(self,attname));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class; 
		}
	}
	return(0);
}


NhlErrorTypes _NclWriteAtt
#if	NhlNeedProto
(struct _NclVarRec * self, char    * attname, struct  _NclMultiDValDataRec * value, struct _NclSelectionRecord *sel_ptr)
#else
( self, attname,value, sel_ptr)
struct _NclVarRec * self;
char    * attname;
struct  _NclMultiDValDataRec * value;
struct  _NclSelectionRecord * sel_ptr;
#endif
{
	NclVarClass vc;

	if(self == NULL) {
		return(NhlFATAL);
	} else {
		vc = (NclVarClass) self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.write_att_func != NULL) {
			return((*vc->var_class.write_att_func)(self,attname,value,sel_ptr));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

struct _NclMultiDValDataRec *_NclReadAtt
#if	NhlNeedProto
(struct _NclVarRec *self, char    *attname, struct  _NclSelectionRecord * sel_ptr)
#else
(self, attname, sel_ptr)
struct _NclVarRec *self;
char    *attname;
struct  _NclSelectionRecord* sel_ptr;
#endif
{
	NclVarClass vc;
	
	if(self == NULL) {
		return(NULL) ;
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.write_att_func != NULL) {
			return((NclMultiDValData)(*vc->var_class.read_att_func)(self,attname,sel_ptr));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return(NULL);
}

int _NclIsDim
#if	NhlNeedProto
(struct _NclVarRec *self,char * dimname)
#else
(self,dimname)
struct _NclVarRec *self;
char * dimname;
#endif
{
	NclVarClass vc;

	if(self == NULL) {
		return(0);
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.is_dim_func != NULL) {
			return((*vc->var_class.is_dim_func)(self,dimname));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return(0);
}

struct _NclMultiDValDataRec *_NclReadDim
#if	NhlNeedProto
(struct _NclVarRec *self, char *dim_name, long dim_num)
#else
(self,dim_name,dim_num)
struct _NclVarRec *self;
char *dim_name;
long dim_num;
#endif
{
	NclVarClass vc;
	if(self == NULL) {
		return(NULL);
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc ->var_class.read_dim_func != NULL) {
			return((NclMultiDValData)(*vc->var_class.read_dim_func)(self,dim_name,dim_num));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclWriteDim
#if     NhlNeedProto
(struct _NclVarRec *self, long dim_num, char *dim_name)
#else
(self, dim_num, dim_name)
struct _NclVarRec *self;
long dim_num;
char *dim_name;
#endif
{
	NclVarClass vc ;
	
	if(self == NULL) {
		return(NhlFATAL);
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.write_dim_func != NULL) {
			return((*vc->var_class.write_dim_func)(self,dim_num,dim_name));
		} else {
			vc = (NclVarClass) vc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
	
}

int _NclIsCoord
#if	NhlNeedProto
(struct _NclVarRec * self ,char *coordname)
#else
(self,coordname)
struct _NclVarRec *self;
char *coordname;
#endif
{
	NclVarClass vc;

	if(self == NULL) {
                return(0);
        } else {
                vc = (NclVarClass)self->obj.class_ptr;
        }
        while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.is_coord_func != NULL) {
			return((*vc->var_class.is_coord_func)(self,coordname));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(0);
}

struct _NclVarRec *_NclReadCoordVar
#if	NhlNeedProto
(struct  _NclVarRec *self, char *coord_name, struct  _NclSelectionRecord *sel_ptr)
#else
(self, coord_name, sel_ptr)
struct  _NclVarRec *self;
char *coord_name;
struct  _NclSelectionRecord *sel_ptr;
#endif
{
	NclVarClass vc;

        if(self == NULL) {
                return(NULL);
        } else {
                vc = (NclVarClass)self->obj.class_ptr;
        }
        while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.read_coordinate != NULL) {
			return((NclVar)(*vc->var_class.read_coordinate)(self,coord_name,sel_ptr));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclDeleteCoordVar
#if	NhlNeedProto
(struct  _NclVarRec *self, char *coord_name)
#else
(self, value, coord_name, sel_ptr)
struct  _NclVarRec *self;
struct  _NclMultiDValDataRec  *value;
char *coord_name;
struct  _NclSelectionRecord *sel_ptr;
#endif
{
	NclVarClass vc;

        if(self == NULL) {
                return(NhlFATAL);
        } else {
                vc = (NclVarClass)self->obj.class_ptr;
        }
        while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.delete_coordinate != NULL) {
			return((*vc->var_class.delete_coordinate)(self,coord_name));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

NhlErrorTypes _NclWriteCoordVar
#if	NhlNeedProto
(struct  _NclVarRec *self, struct  _NclMultiDValDataRec  *value, char *coord_name, struct  _NclSelectionRecord *sel_ptr)
#else
(self, value, coord_name, sel_ptr)
struct  _NclVarRec *self;
struct  _NclMultiDValDataRec  *value;
char *coord_name;
struct  _NclSelectionRecord *sel_ptr;
#endif
{
	NclVarClass vc;

        if(self == NULL) {
                return(NhlFATAL);
        } else {
                vc = (NclVarClass)self->obj.class_ptr;
        }
        while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.write_coordinate != NULL) {
			return((*vc->var_class.write_coordinate)(self,value,coord_name,sel_ptr));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

NhlErrorTypes _NclAssignToVar
#if	NhlNeedProto
(struct _NclVarRec *self, struct _NclMultiDValDataRec *value, struct _NclSelectionRecord* sel_ptr)
#else 
(self, value, sel_ptr)
struct _NclVarRec *self;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclVarClass vc;

        if(self == NULL) {
                return(NhlFATAL);
        } else {
                vc = (NclVarClass)self->obj.class_ptr;
        }
        while((NclObjClass)vc != nclObjClass) {

		if(vc->var_class.write_func != NULL) {
			return((*vc->var_class.write_func)(self,value,sel_ptr));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

struct _NclDimRec *_NclGetDimInfo
#if	NhlNeedProto
(struct _NclVarRec *self, char *dim_name, long dim_num)
#else
(self,dim_name,dim_num)
struct _NclVarRec *self;
char *dim_name;
long dim_num;
#endif
{
	NclVarClass vc;
	if(self == NULL) {
		return(NULL);
	} else {
		vc = (NclVarClass)self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.get_dim_info != NULL) {
			return((NclDimRec*)(*vc->var_class.get_dim_info)(self,dim_name,dim_num));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	} 
	return(NULL);
}
NhlErrorTypes  _NclBuildVSelection
#if	NhlNeedProto
(struct _NclVarRec *var,struct _NclVecRec * vec, struct _NclSelection* sel,int  dim_num,char* dim_name)
#else
(var,vec,sel,dim_num,dim_name)
	struct _NclVarRec *var;
	struct _NclVecRec* vec;
	struct _NclSelection* sel;
	int dim_num;
	char * dim_name;
#endif
{	
	NclMultiDValData vect_md;
	NclMultiDValData tmp_md;
	long *thevector;
	int i;
	char * v_name;
	int index = -1;
/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	if(var->var.var_quark != -1) {
		v_name = NrmQuarkToString(var->var.var_quark);
	} else if(var->var.thesym != NULL) {
		v_name = var->var.thesym->name;
	} else {
		v_name = "unnamed";
	}

/*
* vec is guarenteed to be one dimensional, and of an integer type
*/
	vect_md = vec->vec;

	if(vect_md != NULL) {
		if(dim_name != NULL) {
			index = _NclIsDim(var,dim_name);
			if((index >= 0)&&(index < var->var.n_dims)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s), could not determine dimension number",dim_name,v_name);
				return(NhlFATAL);
			}
		} else {
			sel->dim_num = dim_num;
		}
/*
* I don;t think there is anyway to get arround having to allocate the 
* vector again. Since I don't want to make any assumptions about how
* to free the objects without freeing the val field which I need to keep
* arround untill the actual ValueRead happens
*/
		if(!(vect_md->multidval.type->type_class.type & Ncl_Typelong)) {
			tmp_md = _NclCoerceData(vect_md,Ncl_Typelong,NULL);
			if(tmp_md == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce vector to long type can't perform subscripting");
				return(NhlFATAL);
			}
			
		}  else {
			tmp_md = vect_md;
		}
		thevector = (long*)NclMalloc((unsigned)vect_md->multidval.totalelements * sizeof(long));
	
		memcpy((char*)thevector,(char*)tmp_md->multidval.val,tmp_md->multidval.totalelements * sizeof(long));
		sel->sel_type = Ncl_VECSUBSCR;
		sel->u.vec.n_ind = vect_md->multidval.totalelements;
		sel->u.vec.min = thevector[0];
		sel->u.vec.max = thevector[0];
		sel->u.vec.ind = thevector;
		for(i = 0; i < sel->u.vec.n_ind; i++) {
			if(thevector[i] > sel->u.vec.max) {
				sel->u.vec.max = thevector[i];
			}
			if(thevector[i] < sel->u.vec.min) {
				sel->u.vec.min = thevector[i];
			}
		}
		if((tmp_md != vect_md)&&(tmp_md->obj.status != PERMANENT)) {
			_NclDestroyObj((NclObj)tmp_md);
		}
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}
}


NhlErrorTypes _NclAssignVarToVar
#if	NhlNeedProto
(struct _NclVarRec* lhs,NclSelectionRecord *lhs_sel_ptr,struct _NclVarRec *rhs,NclSelectionRecord *rhs_sel_ptr)
#else
(lhs,lhs_sel_ptr,rhs,rhs_sel_ptr)
struct _NclVarRec* lhs;
NclSelectionRecord *lhs_sel_ptr;
struct _NclVarRec *rhs;
NclSelectionRecord *rhs_sel_ptr;
#endif
{
	NclVarClass vc;

	if(lhs == NULL) {
		return(NhlFATAL);
	} else {
		vc = (NclVarClass)lhs->obj.class_ptr;
	}
	while((NclObjClass) vc != nclObjClass) {
		if(vc->var_class.write_vv_func != NULL) {
                	return((*vc->var_class.write_vv_func)(lhs,lhs_sel_ptr,rhs,rhs_sel_ptr));
        	} else {
			vc = (NclVarClass) vc->obj_class.super_class;
       		}
	}
        return(NhlFATAL);
	 
}

struct _NclVarRec* _NclCopyVar
#if	NhlNeedProto
(struct _NclVarRec * var,char *new_name, struct _NclVarRec * storage)
#else 
(var,new_name,storage)
	struct _NclVarRec * var;
	char *new_name;
	struct _NclVarRec * storage;
#endif
{
	NclVarClass vc;

	if(var == NULL) {
		return(NULL);
	} else {
		vc = (NclVarClass)var->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.copy_var != NULL) {
			return((NclVar)((*vc->var_class.copy_var)(var,new_name,storage)));
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NULL);
}



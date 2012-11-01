/*
 *      $Id: VarSupport.c,v 1.32 2010/04/14 21:29:48 huangwei Exp $
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

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "Symbol.h"
#include "NclMultiDValData.h"
#include "NclVar.h"
#include "NclHLUVar.h"
#include "NclCoordVar.h"
#include "NclFileVar.h"
#include "NclList.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclAtt.h"
#include "AttSupport.h"
#include "TypeSupport.h"
#include "ApiRecords.h"

#include <math.h>

NclSelectionRecord* _NclGetVarSelRec
#if	NhlNeedProto
( struct _NclVarRec * inst)
#else
( inst)
struct _NclVarRec * inst;
#endif
{
	int i;

	if(inst->var.sel_rec == NULL) {
		inst->var.sel_rec = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
		inst->var.sel_rec->selected_from_sym = NULL;
		inst->var.sel_rec->selected_from_var = NULL;
		inst->var.sel_rec->n_entries = inst->var.n_dims;
		for(i = 0; i < inst->var.n_dims; i++ ) {
			inst->var.sel_rec->selection[i].sel_type = Ncl_SUB_ALL;
			inst->var.sel_rec->selection[i].dim_num = i;
		}	
		return(inst->var.sel_rec);
	} else {
		for(i = 0; i < inst->var.n_dims; i++) {	
			if((inst->var.sel_rec->selection[i].sel_type == Ncl_VECSUBSCR)&&(inst->var.sel_rec->selection[i].u.vec.ind != NULL)) {

				NclFree(inst->var.sel_rec->selection[i].u.vec.ind);	
				inst->var.sel_rec->selection[i].u.vec.ind = NULL;
				inst->var.sel_rec->selection[i].sel_type = Ncl_SUB_ALL;
			}
		}
		return(inst->var.sel_rec);
	}
}


struct _NclMultiDValDataRec* _NclStripVarData
#if	NhlNeedProto
(struct _NclVarRec * inst)
#else
(struct _NclVarRec * inst)
#endif
{
	NclMultiDValData tmp;
	if(inst->obj.status == TEMPORARY) {
		NclRefList *tref;
		tmp = (NclMultiDValData)_NclGetObj(inst->var.thevalue_id);	
		tmp->obj.status = TEMPORARY;
		tref = tmp->obj.parents;
		while (tref != NULL && tref->pid == inst->obj.id) {
			tmp->obj.parents = tref->next;
			NclFree(tref);
			tref = tmp->obj.parents;
			tmp->obj.ref_count--;
		}
		inst->var.thevalue_id = -1;
		return(tmp);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclStripVarData: Trying to strip data from PERMANENT variable! This is a bug please report it, thank you");
		return(NULL);
	}
}

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
	ng_size_t i;
	char * v_name;
	int index = -1;
	NclQuark cname;
	NclMultiDValData name_md = NULL,tmp_md = NULL,coord_md = NULL;
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
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) of (%s) is not named and therefore doesn't have an associated coordinate variable",dim_num,v_name);
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
	NclMultiDValData name_md = NULL,tmp_md = NULL,coord_md = NULL;
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
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) of (%s) is not named and therefore doesn't have an associated coordinate variable",dim_num,v_name);
				return(NhlFATAL);
			}
			sel->dim_num = dim_num;
		}
		if(!_NclIsCoord(var,NrmQuarkToString(cname))) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) of (%s) does not have an associated coordinate variable",NrmQuarkToString(cname),v_name);
			return(NhlFATAL);
		}
		sel->u.sub.is_single = range->is_single;
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable",dim_num);
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable",dim_num);
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable",dim_num);
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable",dim_num);
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

			sel->u.sub.stride = 1;

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
	if(coord_md && coord_md->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)coord_md);
	}
	if(cvar && cvar->obj.status != PERMANENT) {
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
	if(var != NULL) {
		if(var->var.var_quark != -1) {
			v_name = NrmQuarkToString(var->var.var_quark);
		} else if(var->var.thesym != NULL) {
			v_name = var->var.thesym->name;
		} else {
			v_name = "unnamed";
		}
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
		} else {
			sel->dim_num = dim_num;
		}
		sel->u.sub.is_single = range->is_single;
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

			sel->u.sub.stride = 1;

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
	NhlErrorTypes subret, ret;

	if(self == NULL) {
		return(NhlFATAL);
	} else {
		vc = (NclVarClass) self->obj.class_ptr;
	}
	while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.write_att_func != NULL) {
			ret = (*vc->var_class.write_att_func)(self,attname,value,sel_ptr);
			if (ret > NhlFATAL && self->var.n_dims == 1 && self->var.coord_vars[0] != -1) {
				NclVar cv = (NclVar)_NclGetObj(self->var.coord_vars[0]);
				if (self->var.var_quark == cv->var.var_quark) {
					subret = _NclWriteAtt(cv,attname,value,sel_ptr);
					return (MIN(ret,subret));
				}
			}
			return ret;
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
	ng_size_t i;
	char * v_name;
	int index = -1;
/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	if(var != NULL) {
		if(var->var.var_quark != -1) {
			v_name = NrmQuarkToString(var->var.var_quark);
		} else if(var->var.thesym != NULL) {
			v_name = var->var.thesym->name;
		} else {
			v_name = "unnamed";
		}
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
(struct _NclVarRec * var,struct _NclSymbol* new_name, struct _NclVarRec * storage)
#else 
(var,new_name,storage)
	struct _NclVarRec * var;
	struct _NclSymbol * new_name;
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


NhlErrorTypes _NclAttCopyWrite
#if	NhlNeedProto
(struct _NclVarRec *to,struct _NclVarRec* from)
#else
(to,from)
struct _NclVarRec *to;
struct _NclVarRec* from;
#endif
{
	NclAtt tmp_att;
	NclAttList *att_list;
	int i;
	NhlErrorTypes ret = NhlNOERROR;
	if((to == NULL) || (from==NULL)) return(NhlFATAL);

	if((to->var.att_id != -1)&&(from->var.att_id != -1)&&(to->var.att_id != from->var.att_id)) {
/*
* Need to merge lists
*/
		tmp_att = (NclAtt)_NclGetObj(from->var.att_id);
		att_list = tmp_att->att.att_list;
		for(i = 0; i < tmp_att->att.n_atts; i++) {
			if(_NclIsAtt(to->var.att_id,att_list->attname)) {
				if(NrmStringToQuark(att_list->attname) != NrmStringToQuark(NCL_MISSING_VALUE_ATT))
					_NclDeleteAtt(to->var.att_id,att_list->attname);
				_NclAddAtt(to->var.att_id,att_list->attname,att_list->attvalue,NULL);
				att_list = att_list->next;
			} else {
				_NclAddAtt(to->var.att_id,att_list->attname,att_list->attvalue,NULL);
				att_list = att_list->next;
			}
		}
	}else if((from->var.att_id != -1)&&(to->var.att_id == -1)) {
/*
* Simple case just copy atts
*/
		to->var.att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)to);
		to->var.att_cb = _NclAddCallback((NclObj)_NclGetObj(to->var.att_id),(NclObj)to,_NclVarMissingNotify,MISSINGNOTIFY,NULL);
		
		tmp_att = (NclAtt)_NclGetObj(from->var.att_id);
		att_list = tmp_att->att.att_list;		
		for(i = 0; i < tmp_att->att.n_atts; i++) {
			_NclAddAtt(to->var.att_id,att_list->attname,att_list->attvalue,NULL);
			att_list = att_list->next;
		}
		
	}
	return(ret);
}
NhlErrorTypes _NclPrintVarSummary
#if	NhlNeedProto
(struct _NclVarRec *self)
#else
(thevar)
struct _NclVarRec* self;
#endif
{

	NclVar cvar= NULL;
	char *v_name;
	int i;
	NclMultiDValData thevalue = NULL;
	NclMultiDValData tmp_md= NULL;
	int ret;
	NhlErrorTypes ret0 = NhlNOERROR;

	FILE *fp = _NclGetOutputStream();

	if((self!= NULL)&&(self->obj.obj_type_mask & Ncl_Var)) {
		thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
		if(self->var.thesym != NULL) {
			v_name = self->var.thesym->name;
		} else if(self->var.var_quark != -1) {
			v_name = NrmQuarkToString(self->var.var_quark);
		} else {
			v_name = "unnamed";
		}

		ret = nclfprintf(fp,"\n");
		if(ret < 0) {
			return(NhlWARNING);
		}
		switch(self->var.var_type) {
		case NORMAL:
		case HLUOBJ:
			ret = nclfprintf(fp,"Variable: %s\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case VARSUBSEL:
			ret = nclfprintf(fp,"Variable: %s (subsection)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case COORD:
			ret = nclfprintf(fp,"Variable: %s (coordinate)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case COORDSUBSEL:
			ret = nclfprintf(fp,"Variable: %s (coordinate subsection)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case PARAM:
			ret = nclfprintf(fp,"Variable: %s (parameter)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case RETURNVAR:
			ret = nclfprintf(fp,"Variable: %s (return)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case FILEVAR:
			ret = nclfprintf(fp,"Variable: %s (file variable)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		case FILEVARSUBSEL:
			ret = nclfprintf(fp,"Variable: %s (file variable)\n",v_name);
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		default:
			ret = nclfprintf(fp,"Variable: %s\n","unnamed");
			if(ret < 0) {
				return(NhlWARNING);
			}
			break;
		}
		if(thevalue->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
			ret = nclfprintf(fp,"Type: file\n");
			if (thevalue->multidval.missing_value.has_missing && 
			    *(obj*)thevalue->multidval.val == thevalue->multidval.missing_value.value.objval) {
				nclfprintf(fp,"(0) File Missing Value : %d\n",*(obj*)thevalue->multidval.val);
			}	
			else {
				NclObj file = _NclGetObj(*(int*)thevalue->multidval.val);
				_NclPrintFileSummary(file,fp);
			}
		} 
		else if(thevalue->obj.obj_type_mask & Ncl_MultiDVallistData) {
			ret0 = _PrintListVarSummary((NclObj)thevalue,fp);
		} else {
			if(thevalue != NULL) 
			{
				ret = nclfprintf(fp,"Type: %s\n",_NclBasicDataTypeToName(thevalue->multidval.data_type));
				if(ret < 0) {
					return(NhlWARNING);
				}

				ret = nclfprintf(fp,"Total Size: %lld bytes\n",(long long)thevalue->multidval.totalsize);
				if(ret < 0) {
					return(NhlWARNING);
				}
				ret = nclfprintf(fp,"            %lld values\n",(long long)thevalue->multidval.totalelements);
				if(ret < 0) {
					return(NhlWARNING);
				}
			}
			ret = nclfprintf(fp,"Number of Dimensions: %d\n",self->var.n_dims);
			if(ret < 0) {
				return(NhlWARNING);
			}
			ret = nclfprintf(fp,"Dimensions and sizes:\t");
			if(ret < 0) {
				return(NhlWARNING);
			}
			for(i = 0; i< self->var.n_dims; i++) {
				ret = nclfprintf(fp,"[");
				if(ret < 0) {
					return(NhlWARNING);
				}
				if((self->var.dim_info[i].dim_quark != -1)) {
					ret = nclfprintf(fp,"%s | ",NrmQuarkToString(self->var.dim_info[i].dim_quark));
					if(ret < 0) {
						return(NhlWARNING);
					}
				}
				ret = nclfprintf(fp,"%lld]",(long long)self->var.dim_info[i].dim_size);
				if(ret < 0) {
					return(NhlWARNING);
				}
				if(i !=  self->var.n_dims - 1) {
					ret = nclfprintf(fp," x ");
					if(ret < 0) {
						return(NhlWARNING);
					}
				}
			}
			ret = nclfprintf(fp,"\n");
			if(ret < 0) {
				return(NhlWARNING);
			}
			ret = nclfprintf(fp,"Coordinates: \n");
			if(ret < 0) {
				return(NhlWARNING);
			}
			for(i =0 ; i< self->var.n_dims; i++) {
				if((self->var.coord_vars[i] != -1)&&(_NclGetObj(self->var.coord_vars[i])!=NULL)) {
					cvar = (NclVar)_NclGetObj(self->var.coord_vars[i]);
					tmp_md = _NclVarValueRead(cvar,NULL,NULL);
					ret = nclfprintf(fp,"            ");
					if(ret < 0) {
						return(NhlWARNING);
					}
					ret = nclfprintf(fp,"%s: [", NrmQuarkToString(self->var.dim_info[i].dim_quark));
					if(ret < 0) {
						return(NhlWARNING);
					}
					if (tmp_md->multidval.totalelements == 0) {
						ret = nclfprintf(fp,"no elements]\n");
						if(ret < 0) {
							return(NhlWARNING);
						}
						continue;
					}
					ret0 =_Nclprint(tmp_md->multidval.type,fp,tmp_md->multidval.val);
					if(ret0 < NhlWARNING) {
						return(NhlWARNING);
					}
					ret = nclfprintf(fp,"..");
					if(ret < 0) {
						return(NhlWARNING);
					}
					ret0 = _Nclprint(tmp_md->multidval.type,fp,
							 &(((char*)tmp_md->multidval.val)
							   [(tmp_md->multidval.totalelements -1)*tmp_md->multidval.type->type_class.size]));
					if(ret0 < NhlWARNING) {
						return(NhlWARNING);
					}
					ret = nclfprintf(fp,"]\n");
					if(ret < 0) {
						return(NhlWARNING);
					}
				}
			}
			ret0 = _NclPrint(_NclGetObj(self->var.att_id),fp);
		}
	
	} else { 
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclPrintVarSummary: Non-Variable passed to printVarSummary, can't print summary");
		return(NhlFATAL);
	}
	return MIN(ret,ret0);
}



NclApiDataList *_NclGetVarInfo2
#if	NhlNeedProto
(NclVar the_var)
#else
(the_var)
NclVar;
#endif
{
	NclApiDataList *tmp = NULL;
	NclAtt tmp_att = NULL;
	NclAttList *att_list = NULL;
	NclMultiDValData the_value = NULL;
	int j;


	tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
	tmp->kind = VARIABLE_LIST;
	tmp->u.var = (NclApiVarInfoRec*) NclMalloc(sizeof(NclApiVarInfoRec));
	tmp->u.var->name = the_var->var.var_quark;
	the_value = (NclMultiDValData)_NclGetObj(the_var->var.thevalue_id);
	tmp->u.var->data_type= the_value->multidval.data_type;
	tmp->u.var->type = the_var->var.var_type;
	tmp->u.var->n_dims = the_var->var.n_dims;
	tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*the_var->var.n_dims);
	for(j = 0; j < the_var->var.n_dims; j++) {
		tmp->u.var->dim_info[j]= the_var->var.dim_info[j];
			if(the_var->var.coord_vars[j] != -1) {
				tmp->u.var->coordnames[j]= the_var->var.dim_info[j].dim_quark;
			} else {
				tmp->u.var->coordnames[j]= -1;
			}
	}
	if(the_var->var.att_id != -1) {

		tmp_att= (NclAtt)_NclGetObj(the_var->var.att_id);
		att_list = tmp_att->att.att_list;
		tmp->u.var->n_atts = tmp_att->att.n_atts;
		tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*tmp_att->att.n_atts);
			
		j = 0;	
		while(att_list != NULL) {
			tmp->u.var->attnames[j] = att_list->quark;
			att_list = att_list->next;
			j++;
		}
	} else {
		tmp->u.var->n_atts = 0;
		tmp->u.var->attnames = NULL;
	}
	tmp->next = NULL;	
	return(tmp);
}

NhlErrorTypes _PrintListVarSummary
#if     NhlNeedProto
(NclObj self, FILE *fp)
#else
(self,fp)
NclObj self;
FILE *fp;
#endif
{
	NclObjClass oc;

	if(self == NULL)
	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"_PrintListVarSummary: Cannot print info of NULL list."));
		return(NhlWARNING);
	}
	else
	{
		oc = self->obj.class_ptr;
	}

	while(oc != NULL)
	{
		if(oc->obj_class.print_summary != NULL)
		{
			return((*(oc->obj_class.print_summary))(self,fp));
		}
		else
		{
			oc = oc->obj_class.super_class;
		}
	} 

	return(NhlWARNING);
}

unsigned int _closest_prime(unsigned int prime_in)
{
    int i, number, k;
    unsigned int primes[MAX_ALLOWED_NUMBER];
    unsigned int prime_out;
    unsigned int bound;
    int check_this;

    prime_out = 2*prime_in + 1;

    primes[0] = 2U;
    primes[1] = 3U;
    for(i = 2; i < MAX_ALLOWED_NUMBER; i++)
    {
         number = primes[i-1];
         check_this = 1;
         while(check_this)
         {
             number += 2U;
             check_this = 0;
             bound = (unsigned int) sqrt((double) number);
             for(k = 1; k < i; k++)
             {
                 if(primes[k] > bound)
                     break; /*Not a viable shortcut for small quantities*/

                 if(!(number % primes[k]))
                 {
                     check_this = 1;
                     break;
                 }
             }
         }
         primes[i] = number;
         if(number == prime_in)
         {
             prime_out = number;
             break;
         }
         else if(number > prime_in)
         {
             if((number - prime_in) > (prime_in - number))
                 prime_out = number;
             else
                 prime_out = primes[i-1];
             break;
         }
    }

    return prime_out;
}


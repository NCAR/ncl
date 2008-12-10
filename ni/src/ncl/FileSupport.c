
/*
 *      $Id: FileSupport.c,v 1.28 2008-12-10 20:12:16 dbrown Exp $
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
#include "NclMultiDValData.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "Symbol.h"
#include "NclCoordVar.h"
#include "FileSupport.h"
#include "VarSupport.h"
#include "ApiRecords.h"
#include "NclAtt.h"


NhlErrorTypes _NclBuildFileCoordRSelection
#if	NhlNeedProto
(struct _NclFileRec *file,NclQuark var,struct _NclRangeRec * range, struct _NclSelection* sel,int  dim_num, char * dim_name)
#else
(file,var,range,sel,dim_num,dim_name)
	struct _NclFileRec *file;
	NclQuark var;
	struct _NclRangeRec* range;
	struct _NclSelection* sel;
	int dim_num;
	char * dim_name;
#endif
{
	NclQuark cname;
        NclMultiDValData name_md = NULL,result_md = NULL,tmp_md = NULL,coord_md = NULL;
        long start = 0,finish = 0;
        NclCoordVar cvar = NULL;
        NclObjTypes the_type;
	char * v_name;
	char * f_name;
	
	int index = -1;
	int vindex = -1;
/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	v_name = NrmQuarkToString(var);
	f_name = NrmQuarkToString(file->file.fname);
	vindex = _NclFileIsVar(file,var);

	if(range != NULL) {
		if(dim_name != NULL) {
			cname = NrmStringToQuark(dim_name);
			index = _NclFileVarIsDim(file,var,cname);
			if((index >= 0)&&(index < file->file.var_info[vindex]->num_dimensions)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s->%s), could not determine dimension number",dim_name,f_name,v_name);
				return(NhlFATAL);
			}
		} else {
			name_md = _NclFileVarReadDim(file,var,-1,(long)dim_num);
			if(name_md != NULL) {
				if(name_md->multidval.type->type_class.type & Ncl_Typestring) {
					cname = *(string*)name_md->multidval.val;
					_NclDestroyObj((NclObj)name_md);
				} else {
					return(NhlFATAL);
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%d) of file (%s) is not named and therefore doesn't have an associated coordinate variable",dim_num,f_name);
                                return(NhlFATAL);

			}
			sel->dim_num = dim_num;
		}
		if(_NclFileVarIsCoord(file,cname) == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) of file (%s) does not have an associated coordinate variable",NrmQuarkToString(cname),f_name);
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
		
			cvar = (NclCoordVar)_NclFileReadCoord(file,cname,NULL);
			coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
			the_type = _NclGetVarRepValue((NclVar)cvar);
			if(!(the_type & range->finish->multidval.type->type_class.type)) {
				tmp_md = _NclCoerceData(range->finish,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable, subscript (%d)",dim_num);
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
                                return(NhlFATAL);
                        }
		} else if(range->finish == NULL) {

			sel->sel_type = Ncl_SUB_VAL_DEF;
			sel->u.sub.finish = 0;
			sel->u.sub.stride = 1;

			cvar = (NclCoordVar)_NclFileReadCoord(file,cname,NULL);
			coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
			the_type = _NclGetVarRepValue((NclVar)cvar);
			if(!(the_type & range->start->multidval.type->type_class.type)) {
				tmp_md = _NclCoerceData(range->start,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable, subscript (%d)",dim_num);
					if(cvar->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar);
					}
					if(coord_md->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)coord_md);
					}
                                        return(NhlFATAL);

				} else {
					if(range->start->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)range->start);
					}
					range->start= tmp_md;
				}
			}
			if(_NclGetCoordRange(coord_md,range->start->multidval.val,NULL,&sel->u.sub.start,&sel->u.sub.finish) == NhlFATAL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not obtain coordinate indexes, unable to perform subscript (%d)",dim_num);
				if(cvar->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)cvar);
				}
				if(coord_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)coord_md);
				}
                                return(NhlFATAL);
                        }
		} else if(range->start == range->finish) {
			sel->sel_type = Ncl_SUBSCR;
                        cvar = (NclCoordVar)_NclFileReadCoord(file,cname,NULL);
                        coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
                        the_type = _NclGetVarRepValue((NclVar)cvar);

                        if(!(the_type & range->start->multidval.type->type_class.type)) {
                                tmp_md = _NclCoerceData(range->start,the_type,NULL);
                                if(tmp_md == NULL) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable, subscript (%d)",dim_num);
                                        if(cvar->obj.status != PERMANENT) {
                                                _NclDestroyObj((NclObj)cvar);
                                        }
                                        if(coord_md->obj.status != PERMANENT) {
                                                _NclDestroyObj((NclObj)coord_md);
                                        }
                                        return(NhlFATAL);

                                } else {
                                        if(range->start->obj.status != PERMANENT) {
                                                _NclDestroyObj((NclObj)range->start);
                                        }
                                        range->finish = range->start= tmp_md;
                                }
                        }
			if(_NclGetCoordRange(coord_md,range->start->multidval.val,range->finish->multidval.val,&sel->u.sub.start,&sel->u.sub.finish) == NhlFATAL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not obtain coordinate indexes, unable to perform subscript");
                                if(cvar->obj.status != PERMANENT) {
                                        _NclDestroyObj((NclObj)cvar);
                                }
                                if(coord_md->obj.status != PERMANENT) {
                                        _NclDestroyObj((NclObj)coord_md);
                                }
                                return(NhlFATAL);
                        }

                        sel->u.sub.stride = 1;


		} else {

			sel->sel_type = Ncl_SUBSCR;
			cvar = (NclCoordVar)_NclFileReadCoord(file,cname,NULL);
			coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
			the_type = _NclGetVarRepValue((NclVar)cvar);

			if(!(the_type & range->start->multidval.type->type_class.type)) {
				tmp_md = _NclCoerceData(range->start,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable, subscript (%d)",dim_num);
					if(cvar->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar);
					}
                                        return(NhlFATAL);

				} else {
					if(range->start->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)range->start);
					}
					range->start= tmp_md;
				}
			}

			tmp_md = NULL;
			if(!(the_type & range->finish->multidval.type->type_class.type)) {
				tmp_md = _NclCoerceData(range->finish,the_type,NULL);
				if(tmp_md == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate subscript type mismatch. Subscript (%d) can not be coerced to type of coordinate variable, subscript (%d)",dim_num);
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
				return(NhlFATAL);
			}

		} 
	} 
	if(cvar->obj.status != PERMANENT) {
		_NclDestroyObj((NclObj)cvar);
	}
	return(NhlNOERROR);
}


NhlErrorTypes  _NclBuildFileCoordVSelection
#if	NhlNeedProto
(struct _NclFileRec *file , NclQuark var,struct _NclVecRec * vec, struct _NclSelection* sel,int  dim_num,char* dim_name)
#else
(file,var,vec,sel,dim_num,dim_name)
	struct _NclFileRec *file;
	NclQuark var;
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
	char * f_name;
	int index = -1;
	int vindex = -1;
	NclQuark cname;
	NclMultiDValData name_md = NULL,tmp_md = NULL,coord_md = NULL;
        NclCoordVar cvar = NULL;
        NclObjTypes the_type;

/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	v_name = NrmQuarkToString(var);
	f_name = NrmQuarkToString(file->file.fname);
	vindex = _NclFileIsVar(file,var);

/*
* vec is guarenteed to be one dimensional, and of an integer type
*/
	vect_md = vec->vec;

	if(vect_md != NULL) {
		if(dim_name != NULL) {
			index = _NclFileVarIsDim(file,var,NrmStringToQuark(dim_name));
			if((index >= 0)&&(index < file->file.var_info[vindex]->num_dimensions)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s->%s), could not determine dimension number",dim_name,f_name,v_name);
				return(NhlFATAL);
			}
		} else {
			name_md = _NclFileVarReadDim(file,var,-1,(long)dim_num);
			if(name_md != NULL) {
				if(name_md->multidval.type->type_class.type & Ncl_Typestring) {
					cname = *(string*)name_md->multidval.val;
					_NclDestroyObj((NclObj)name_md);
				} else {
					return(NhlFATAL);
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN, "Dimension (%d) of file (%s) is not named and therefore doesn't have an associated coordinate variable",dim_num,f_name);
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
		if(_NclFileVarIsCoord(file,cname) == -1) {
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) of file (%s) does not have an associated coordinate variable",NrmQuarkToString(cname),f_name);
                        return(NhlFATAL);

                }
		cvar = (NclCoordVar)_NclFileReadCoord(file,cname,NULL);
		coord_md = _NclVarValueRead((NclVar)cvar,NULL,NULL);
		the_type = _NclGetVarRepValue((NclVar)cvar);
		if(!(the_type & vect_md->multidval.type->type_class.type)) {
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
		thevector = (long*)NclMalloc((unsigned)tmp_md->multidval.totalelements*sizeof(long));
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
NhlErrorTypes _NclBuildFileRSelection
#if	NhlNeedProto
(struct _NclFileRec *file,NclQuark var,struct _NclRangeRec * range, struct _NclSelection* sel,int  dim_num, char * dim_name)
#else
(file,var,range,sel,dim_num,dim_name)
	struct _NclFileRec *file;
	NclQuark var;
	struct _NclRangeRec* range;
	struct _NclSelection* sel;
	int dim_num;
	char * dim_name;
#endif
{
	char * v_name;
	char * f_name;
	
	int index = -1;
	int vindex = -1;
/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	v_name = NrmQuarkToString(var);
	f_name = NrmQuarkToString(file->file.fname);
	vindex = _NclFileIsVar(file,var);

	if(range != NULL) {
		if(dim_name != NULL) {
			index = _NclFileVarIsDim(file,var,NrmStringToQuark(dim_name));
			if((index >= 0)&&(index < file->file.var_info[vindex]->num_dimensions)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s->%s), could not determine dimension number",dim_name,f_name,v_name);
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


NhlErrorTypes  _NclBuildFileVSelection
#if	NhlNeedProto
(struct _NclFileRec *file , NclQuark var,struct _NclVecRec * vec, struct _NclSelection* sel,int  dim_num,char* dim_name)
#else
(file,var,vec,sel,dim_num,dim_name)
	struct _NclFileRec *file;
	NclQuark var;
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
	char * f_name;
	int index = -1;
	int vindex = -1;
/*
* Preconditions: subscripts are SCALAR and integer guarenteed!!!!
*/
	v_name = NrmQuarkToString(var);
	f_name = NrmQuarkToString(file->file.fname);
	vindex = _NclFileIsVar(file,var);

/*
* vec is guarenteed to be one dimensional, and of an integer type
*/
	vect_md = vec->vec;

	if(vect_md != NULL) {
		if(dim_name != NULL) {
			index = _NclFileVarIsDim(file,var,NrmStringToQuark(dim_name));
			if((index >= 0)&&(index < file->file.var_info[vindex]->num_dimensions)){
				sel->dim_num = index;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a dimension name in variable (%s->%s), could not determine dimension number",dim_name,f_name,v_name);
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
			
		}  else {
			tmp_md = vect_md;
		}
		thevector = (long*)NclMalloc((unsigned)vect_md->multidval.totalelements * sizeof(long));
	
		memcpy((char*)thevector,(char*)tmp_md->multidval.val,vect_md->multidval.totalelements * sizeof(long));
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
NclObjTypes _NclFileVarRepValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var)
#else 
(thefile, var)
NclFile thefile;
NclQuark var;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(Ncl_None);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.rep_val != NULL) {
			return((*fc->file_class.rep_val)(thefile,var));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(Ncl_None);
}


int _NclFileIsVar
#if	NhlNeedProto
(NclFile thefile,NclQuark var)
#else 
(thefile,var)
	NclFile thefile;
	NclQuark var;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_var!= NULL) {
			return((*fc->file_class.is_var)(thefile,var));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(-1);
}

NhlErrorTypes _NclFileWriteVarVar
#if	NhlNeedProto
(NclFile thefile, NclQuark lhs_var, struct _NclSelectionRecord * lhs_sel_ptr, struct _NclVarRec* rhs_var, struct _NclSelectionRecord *rhs_sel_ptr)
#else 
(thefile, lhs_var, lhs_sel_ptr, rhs_var, rhs_sel_ptr)
NclFile thefile;
NclQuark lhs_var;
struct _NclSelectionRecord * lhs_sel_ptr;
struct _NclVarRec* rhs_var;
struct _NclSelectionRecord *rhs_sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_var != NULL) {
			return((*fc->file_class.write_var_var)(thefile, lhs_var, lhs_sel_ptr,rhs_var, rhs_sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

NhlErrorTypes _NclFileWriteVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var, struct _NclMultiDValDataRec *value,struct _NclSelectionRecord * sel_ptr)
#else 
(thefile, var, value, sel_ptr)
NclFile thefile;
NclQuark var;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord * sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_var != NULL) {
			return((*fc->file_class.write_var)(thefile, var, value, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

struct _NclVarRec *_NclFileReadVar
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_var_func != NULL) {
			return((*fc->file_class.read_var_func)(thefile, var_name, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}

struct _NclMultiDValDataRec* _NclFileReadVarValue
#if	NhlNeedProto
(NclFile thefile, NclQuark var_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, var_name, sel_ptr)
NclFile thefile;
NclQuark var_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_var_val_func != NULL) {
			return((*fc->file_class.read_var_val_func)(thefile, var_name, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}

int _NclFileVarIsAtt
#if	NhlNeedProto
(NclFile thefile,NclQuark thevar,NclQuark theatt)
#else 
(thefile,thevar,theatt)
NclFile thefile;
NclQuark thevar;
NclQuark theatt;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_att != NULL) {
			return((*fc->file_class.is_var_att)(thefile,thevar ,theatt));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(-1);
}

struct _NclMultiDValDataRec *_NclFileReadVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, var, attname, sel_ptr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_var_att_func != NULL) {
			return((*fc->file_class.read_var_att_func)(thefile, var, attname, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}


NhlErrorTypes _NclFileDeleteVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname)
#else 
(thefile, var, attname)
NclFile thefile;
NclQuark var;
NclQuark attname;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.del_var_att_func != NULL) {
			return((*fc->file_class.del_var_att_func)(thefile, var, attname));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
NhlErrorTypes _NclFileWriteVarAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark attname,struct _NclMultiDValDataRec* value, struct _NclSelectionRecord * sel_pr)
#else 
(thefile, var, attname,value, sel_pr)
NclFile thefile;
NclQuark var;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord * sel_pr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_var_att_func != NULL) {
			return((*fc->file_class.write_var_att_func)(thefile, var, attname,value, sel_pr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
int _NclFileIsAtt
#if	NhlNeedProto
(NclFile thefile,NclQuark theatt)
#else 
(thefile,theatt)
NclFile thefile;
NclQuark theatt;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_att != NULL) {
			return((*fc->file_class.is_att)(thefile,theatt));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(-1);
}

struct _NclMultiDValDataRec *_NclFileReadAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname,  sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_att_func != NULL) {
			return((*fc->file_class.read_att_func)(thefile, attname, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclFileDeleteAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname)
#else 
(thefile, attname)
NclFile thefile;
NclQuark attname;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.del_att_func != NULL) {
			return((*fc->file_class.del_att_func)(thefile, attname));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
NhlErrorTypes _NclFileWriteAtt
#if	NhlNeedProto
(NclFile thefile, NclQuark attname, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord *sel_ptr)
#else 
(thefile, attname, value, sel_ptr)
NclFile thefile;
NclQuark attname;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_att_func != NULL) {
			return((*fc->file_class.write_att_func)(thefile, attname, value, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

int _NclFileVarIsDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name)
#else 
(thefile, var, dim_name)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_var_dim != NULL) {
			return((*fc->file_class.is_var_dim)(thefile,var, dim_name));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return (-1);
}
int _NclFileIsDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name)
#else 
(thefile, dim_name)
NclFile thefile;
NclQuark dim_name;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_dim != NULL) {
			return((*fc->file_class.is_dim)(thefile, dim_name));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return (-1);
}

struct _NclMultiDValDataRec* _NclFileReadDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_dim_func != NULL) {
			return((*fc->file_class.read_dim_func)(thefile, dim_name, dim_num));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclFileVarWriteDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile, var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_dim_func != NULL) {
			return((*fc->file_class.write_var_dim_func)(thefile,var, dim_name, dim_num));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
struct _NclMultiDValDataRec* _NclFileVarReadDim
#if	NhlNeedProto
(NclFile thefile, NclQuark var, NclQuark dim_name, long dim_num)
#else 
(thefile, var, dim_name, dim_num)
NclFile thefile;
NclQuark var;
NclQuark dim_name;
long dim_num;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_dim_func != NULL) {
			return((*fc->file_class.read_var_dim_func)(thefile,var, dim_name, dim_num));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclFileWriteDim
#if	NhlNeedProto
(NclFile thefile, NclQuark dim_name, long dim_num)
#else 
(thefile, dim_name, dim_num)
NclFile thefile;
NclQuark dim_name;
long dim_num;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_dim_func != NULL) {
			return((*fc->file_class.write_dim_func)(thefile, dim_name, dim_num));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

int _NclFileVarIsCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name)
#else 
(thefile,coord_name)
NclFile thefile;
NclQuark coord_name;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_coord != NULL) {
			return((*fc->file_class.is_coord)(thefile,coord_name));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(-1);
}

struct _NclVarRec* _NclFileReadCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_coord_func != NULL) {
			return((*fc->file_class.read_coord_func)(thefile, coord_name, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclFileWriteCoord
#if	NhlNeedProto
(NclFile thefile, NclQuark coord_name, struct _NclMultiDValDataRec* value, struct _NclSelectionRecord* sel_ptr)
#else 
(thefile, coord_name, value, sel_ptr)
NclFile thefile;
NclQuark coord_name;
struct _NclMultiDValDataRec* value;
struct _NclSelectionRecord* sel_ptr;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.write_coord_func != NULL) {
			return((*fc->file_class.write_coord_func)(thefile, coord_name, value, sel_ptr));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
extern NhlErrorTypes _NclFileAddVar
#if     NhlNeedProto
(NclFile thefile, NclQuark varname, NclQuark type, int n_dims, NclQuark *dimnames)
#else
(thefile, varname, type, n_dims, dimnames)
NclFile thefile;
NclQuark varname;
NclQuark type;
int n_dims;
NclQuark *dimnames;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.add_var_func != NULL) {
			return((*fc->file_class.add_var_func)(thefile, varname, type, n_dims, dimnames));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
extern NhlErrorTypes _NclFileAddDim
#if     NhlNeedProto
(NclFile thefile, NclQuark dimname, int dimsize, int is_unlimited)
#else
(thefile, dimname, dimsize, is_unlimited)
NclFile thefile;
NclQuark dimname;
int dimsize;
int is_unlimited;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.add_dim_func != NULL) {
			return((*fc->file_class.add_dim_func)(thefile, dimname, dimsize, is_unlimited));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

NhlErrorTypes _NclPrintFileVarSummary
#if NhlNeedProto
(NclFile  thefile , NclQuark  varname )
#else
(thefile ,  varname )
NclFile thefile; 
NclQuark  varname; 
#endif
{
	FILE *fp = _NclGetOutputStream();
	NclFileAttInfoList* step;
	int i,j;
	int ret;
	long long total;
	char *dimnames[100];
	NclMultiDValData tmp_md;
	NclVar tmp_var;
	int vindex = -1;

	vindex = _NclFileIsVar(thefile,varname);
	if(vindex > -1) {
		for(i = 0;i < thefile->file.n_vars;i++) {
			if((thefile->file.var_info[i] != NULL)&&(thefile->file.var_info[i]->var_name_quark == varname)) {
							

				ret = nclfprintf(fp,"\n\n");
               		 	if(ret < 0) {
                       			return(NhlWARNING);
                		}
				ret = nclfprintf(fp,"Variable: %s\n",NrmQuarkToString(varname));
               		 	if(ret < 0) {
                       			return(NhlWARNING);
                		}
				total = 1;	
				for(j = 0; j < thefile->file.var_info[i]->num_dimensions;j++) {
					total *= thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
				}
				ret = nclfprintf(fp,"Type: %s\n",_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type));
               		 	if(ret < 0) {
                       			return(NhlWARNING);
                		}
				ret = nclfprintf(fp,"Total Size: %lld bytes\n",total * _NclSizeOf(thefile->file.var_info[i]->data_type));
                		if(ret < 0) {
                        		return(NhlWARNING);
                		}
				ret = nclfprintf(fp,"            %lld values\n",total);
                		if(ret < 0) {
                        		return(NhlWARNING);
                		}
				ret = nclfprintf(fp,"Number of Dimensions: %d\n",thefile->file.var_info[i]->num_dimensions);
                		if(ret < 0) {
                        		return(NhlWARNING);
                		}
				ret = nclfprintf(fp,"Dimensions and sizes:\t");
				if(ret < 0) {
                                        return(NhlWARNING);
                                }
                                for(j = 0; j < thefile->file.var_info[i]->num_dimensions;j++) {
					ret = nclfprintf(fp,"[");
					if(ret < 0) {
		                                return(NhlWARNING);
               		        	}
					ret = nclfprintf(fp,"%s | ",NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark));
					if(ret < 0) {
						return(NhlWARNING);
					}
					
					ret = nclfprintf(fp,"%lld]",(long long) thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size);
					if(ret < 0) {
                                                return(NhlWARNING);
                                        }
					if(j != thefile->file.var_info[i]->num_dimensions-1) {
						ret = nclfprintf(fp," x ");
		                                if(ret < 0) {
							return(NhlWARNING);
                                		}

					}
				}
				ret = nclfprintf(fp,"\nCoordinates: \n");
				for(j = 0; j < thefile->file.var_info[i]->num_dimensions;j++) {
					if(_NclFileVarIsCoord(thefile,thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark)!= -1) {
						int size = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
						ret = nclfprintf(fp,"            ");
						if(ret < 0) {
							return(NhlWARNING);
						}
						ret = nclfprintf(fp,"%s: [", NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark));
						if(ret < 0) {
							return(NhlWARNING);
						}
						if (size == 0) {
							ret = nclfprintf(fp,"no elements]\n");
							if(ret < 0) {
								return(NhlWARNING);
							}
							continue;
						}
						tmp_var = _NclFileReadCoord(thefile,thefile->file.file_dim_info
									    [thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark,NULL);
						if(tmp_var != NULL) {
							tmp_md = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
						}
						ret =_Nclprint(tmp_md->multidval.type,fp,tmp_md->multidval.val);
						if(ret < NhlWARNING) {
							return(NhlWARNING);
						}
						ret = nclfprintf(fp,"..");
						if(ret < 0) {
							return(NhlWARNING);
						}
						ret = _Nclprint(tmp_md->multidval.type,fp,
								&(((char*)tmp_md->multidval.val)
								  [(tmp_md->multidval.totalelements -1)*tmp_md->multidval.type->type_class.size]));
						if(ret < NhlWARNING) {
							return(NhlWARNING);
						}
                                		ret = nclfprintf(fp,"]\n");
                                		if(ret < 0) {
                                        		return(NhlWARNING);
                                		}
						if(tmp_var->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)tmp_var);
						}
					} else {
						ret = nclfprintf(fp,"            ");
						if(ret < 0) {
               		                                 return(NhlWARNING);
		                                }
						ret = nclfprintf(fp,"%s: not a coordinate variable\n",
								 NrmQuarkToString(thefile->file.file_dim_info
										  [thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark));
						if(ret < 0) {
               		                                 return(NhlWARNING);
		                                }
					}
				}	
				step = thefile->file.var_att_info[i];
				j = 0;
				while(step != NULL) {
					step = step->next;
					j++;
				}
			 	step = thefile->file.var_att_info[i];

				ret = nclfprintf(fp,"Number of Attributes: %d\n",j);
				if(ret < 0) {
					return(NhlWARNING);
				}

				while(step != NULL) {
					ret = nclfprintf(fp,"  %s :\t", NrmQuarkToString(step->the_att->att_name_quark));
					if(ret < 0) {
						return(NhlWARNING);
					 }
					if(step->the_att->num_elements == 1) {
						tmp_md = _NclFileReadVarAtt(thefile,thefile->file.var_info[i]->var_name_quark,step->the_att->att_name_quark,NULL);
						ret = _Nclprint(tmp_md->multidval.type, fp,tmp_md->multidval.val);
						if(ret < NhlINFO) {
							return(NhlWARNING);
						}
						ret = nclfprintf(fp,"\n");
						if(ret < 0) {
							return(NhlWARNING);
						}
					} else {
						ret = nclfprintf(fp,"<ARRAY>\n");
						if(ret < 0) {
							return(NhlWARNING);
						}
					}
					step = step->next;
				}
				ret = nclfprintf(fp,"\n");
				if(ret < 0) {
					return(NhlWARNING);
                        	}
				return(NhlNOERROR);
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"printFileVarSummary: (%s) is not a variable in the file (%s)",NrmQuarkToString(varname),NrmQuarkToString(thefile->file.fname));
		return(NhlFATAL);	
	}
}

NclApiDataList *_NclGetFileVarInfoList2
#if	NhlNeedProto
(struct _NclFileRec *thefile)
#else
(thefile)
	struct _NclFileRec *thefile;
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclSymbol *s = NULL;
	int i,j;
	NclStackEntry *thevar = NULL;
	NclMultiDValData theid = NULL;
	NclFileAttInfoList *step;
	if(thefile!=NULL) {
					for(i = 0; i < thefile->file.n_vars; i++) {
						tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
						tmp->kind = VARIABLE_LIST;
						tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
						tmp->u.var->name = thefile->file.var_info[i]->var_name_quark;
						tmp->u.var->data_type = thefile->file.var_info[i]->data_type;
						tmp->u.var->type = FILEVAR;
						tmp->u.var->n_dims = thefile->file.var_info[i]->num_dimensions;
						tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.var->n_dims);

						for(j = 0 ; j < tmp->u.var->n_dims ; j++) {
							tmp->u.var->dim_info[j].dim_quark =thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
							tmp->u.var->dim_info[j].dim_num = thefile->file.var_info[i]->file_dim_num[j];
							tmp->u.var->dim_info[j].dim_size = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
							if(thefile->file.coord_vars[thefile->file.var_info[i]->file_dim_num[j]] != NULL) {
								tmp->u.var->coordnames[j] = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;

							} else {
								tmp->u.var->coordnames[j] = -1;
							}
						}
						if(thefile->file.var_att_info[i] != NULL) {
							j = 0;
							step = thefile->file.var_att_info[i];
							while(step != NULL) {
								step = step->next;
								j++;
							}
							tmp->u.var->n_atts = j;
							tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*j);
							step = thefile->file.var_att_info[i];
							j = 0;
							while(step != NULL) {
								tmp->u.var->attnames[j]= step->the_att->att_name_quark;
								j++;
								step = step->next;
							}
						} else {
							tmp->u.var->n_atts = 0;
							tmp->u.var->attnames = NULL;
						}
						tmp->next = thelist;
						thelist = tmp;
						tmp = NULL;
					}
	} else {
		return(NULL);
	}
}

NclApiDataList *_NclGetFileVarInfo2
#if	NhlNeedProto
(struct _NclFileRec *thefile,NclQuark file_var_name)
#else
(thefile,file_var_name)
struct _NclFileRec *thefile;
NclQuark file_var_name;
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclSymbol *s = NULL;
	int i,j;
	NclStackEntry *thevar = NULL;
	NclMultiDValData theid = NULL;
	NclFileAttInfoList *step;



	if(thefile != NULL) {
		for(i = 0; i < thefile->file.n_vars; i++) {
			if(thefile->file.var_info[i]->var_name_quark == file_var_name) {
				tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
				tmp->kind = VARIABLE_LIST;
				tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
				tmp->u.var->name = thefile->file.var_info[i]->var_name_quark;
				tmp->u.var->data_type= thefile->file.var_info[i]->data_type;
				tmp->u.var->type = FILEVAR;
				tmp->u.var->n_dims = thefile->file.var_info[i]->num_dimensions;
				tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.var->n_dims);
				for(j = 0 ; j < tmp->u.var->n_dims ; j++) {
					tmp->u.var->dim_info[j].dim_quark =thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
					tmp->u.var->dim_info[j].dim_num = thefile->file.var_info[i]->file_dim_num[j];
					tmp->u.var->dim_info[j].dim_size = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
					if(thefile->file.coord_vars[thefile->file.var_info[i]->file_dim_num[j]] != NULL) {	
						tmp->u.var->coordnames[j] =  thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
					} else {
						tmp->u.var->coordnames[j] = -1;
					}
				}
				if(thefile->file.var_att_info[i] != NULL) {
					j = 0;
					step = thefile->file.var_att_info[i];
					while(step != NULL) {
						step = step->next;
						j++;
					}
					tmp->u.var->n_atts = j;
					tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*j);
					step = thefile->file.var_att_info[i];
					j = 0;
					while(step != NULL) {
						tmp->u.var->attnames[j]= step->the_att->att_name_quark;
						j++;
						step = step->next;
					}
				} else {
					tmp->u.var->n_atts = 0;
					tmp->u.var->attnames = NULL;
				}
				tmp->next = NULL;
				return(tmp);
			}
		}
	}
	return(NULL);
}

NclApiDataList *_NclGetFileInfo2
#if	NhlNeedProto
(NclFile thefile)
#else
(thefile)
NclFile thefile;
#endif
{
	NclApiDataList *tmp = NULL;
	NclSymTableListNode *st;
	NclSymbol *s;
	int i,j;
	NclMultiDValData theid;

	tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
	tmp->kind = FILE_LIST;
	tmp->u.file = (NclApiFileInfoRec*)NclMalloc(sizeof(NclApiFileInfoRec));
	if(thefile != NULL) {
		tmp->u.file->name = thefile->file.fname;
		tmp->u.file->path = thefile->file.fpath;
		tmp->u.file->wr_status = thefile->file.wr_status;
		tmp->u.file->file_format = (int)thefile->file.file_format;
		tmp->u.file->n_dims = thefile->file.n_file_dims;
		tmp->u.file->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.file->n_dims);
		for(j = 0; j < tmp->u.file->n_dims; j++) {
			tmp->u.file->dim_info[j].dim_num = j;
			tmp->u.file->dim_info[j].dim_quark = thefile->file.file_dim_info[j]->dim_name_quark;
			tmp->u.file->dim_info[j].dim_size = thefile->file.file_dim_info[j]->dim_size;
		}
		if(thefile->file.n_vars > 0) {
			tmp->u.file->n_vars = thefile->file.n_vars;
			tmp->u.file->var_names = (NclQuark*)NclMalloc(sizeof(NclQuark)*thefile->file.n_vars);
			for(j = 0; j < thefile->file.n_vars; j++) {
				tmp->u.file->var_names[j] = thefile->file.var_info[j]->var_name_quark;
			}
		} else {
			tmp->u.file->n_vars = 0;
			tmp->u.file->var_names = NULL;
		}
		if(thefile->file.n_file_atts > 0) {
			tmp->u.file->n_atts = thefile->file.n_file_atts;
			tmp->u.file->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*thefile->file.n_file_atts);
			for(j = 0; j < thefile->file.n_file_atts; j++) {
				tmp->u.file->attnames[j] = thefile->file.file_atts[j]->att_name_quark;
			}
						
		} else {
			tmp->u.file->n_atts = 0;
			tmp->u.file->attnames = NULL;
		}
		tmp->next = NULL;
		return(tmp);
	}
}


NhlErrorTypes _NclFileSetOption
#if	NhlNeedProto
(NclFile thefile, 
 NclQuark format, 
 NclQuark option, 
 struct _NclMultiDValDataRec *value
	)
#else 
(thefile, format, option, value)
NclFile thefile;
NclQuark format;
NclQuark option;
struct _NclMultiDValDataRec *value;
#endif
{
	NclFileClass fc = NULL;

	fc = &nclFileClassRec;
	if(fc && fc->file_class.set_file_option != NULL) {
		return((*fc->file_class.set_file_option)(thefile, format, option, value));
	}
	return(NhlFATAL);
}


int _NclFileIsOption
#if	NhlNeedProto
( NclQuark format, 
 NclQuark option 
)
#else 
(format,option)
NclQuark format;
NclQuark option;
#endif
{
	NclFileClass fc = NULL;
	int i;

	fc = &nclFileClassRec;
	if(fc) {
		for (i = 0; i < fc->file_class.num_options; i++) {
			NclFileOption *opt = &(fc->file_class.options[i]);
			if (opt->name != _NclGetLower(option))
				continue;
			/* if format not specified then just report that the option is defined */
			if (format == NrmNULLQUARK)
				return 1;
			if (! (_NclGetFormatFuncs(format) &&
			       _NclGetFormatFuncs(format) == _NclGetFormatFuncs(opt->format)) )
				continue;
			return 1;
		}
	}
	return 0;
}

NhlErrorTypes _NclFileSetOptionDefaults
#if	NhlNeedProto
(
 NclQuark format, 
 NclQuark option
)
#else 
(format, option)
NclQuark format;
NclQuark option;
#endif
{
	NclFileClass fc = NULL;
	int i;

	fc = &nclFileClassRec;
	if( !fc) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Error referencing file class");
		return(NhlFATAL);
	}
	if (format && option) {
		if (! _NclFileIsOption(format,option)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s is not a valid option for format %s",
				  NrmQuarkToString(option),
				  NrmQuarkToString(format));
			return(NhlWARNING);
		}
		_NclFileSetOption(NULL,format,option,NULL);
	}
	else if (format) {
		if (! _NclGetFormatFuncs(format)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"%s is not a valid format",
				  NrmQuarkToString(format));
			return(NhlWARNING);
		}
		for (i = 0; i < fc->file_class.num_options; i++) {
			NclFileOption *opt = &(fc->file_class.options[i]);
			if (!(_NclGetFormatFuncs(format) == _NclGetFormatFuncs(opt->format)))
				continue;
			_NclFileSetOption(NULL,format,opt->name,NULL);
		}
	}
	else if (option) {
		int found = False;
		for (i = 0; i < fc->file_class.num_options; i++) {
			NclFileOption *opt = &(fc->file_class.options[i]);
			if (option != opt->name)
				continue;
			_NclFileSetOption(NULL,opt->format,option,NULL);
			found = True;
		}
		if (! found) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"no such option: %s",
				  NrmQuarkToString(option));
			return(NhlWARNING);
		}
	}
	else {
		for (i = 0; i < fc->file_class.num_options; i++) {
			NclFileOption *opt = &(fc->file_class.options[i]);
			_NclFileSetOption(NULL,opt->format,opt->name,NULL);
		}
	}
	return NhlNOERROR;
			
}

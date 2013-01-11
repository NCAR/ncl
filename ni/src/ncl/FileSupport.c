
/*
 *      $Id$
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
#include "nioError.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "ncarg/hlu/Error.h"
#endif

#include <netcdf.h>

#ifdef BuildHDF5
#include <hdf5.h>
#endif

#ifdef BuildHDF4
#include <hdf/mfhdf.h>
#endif

#ifdef BuildHDFEOS5
#include <HE5_HdfEosDef.h>
#endif

#ifdef BuildHDFEOS
#include <HdfEosDef.h>
#endif

#include "defs.h"
#include "NclMultiDValData.h"
#include "NclFile.h"
#include "NclList.h"
#include "NclNewFile.h"
#include "NclGroup.h"
#include "NclNewGroup.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "TypeSupport.h"
#include "Symbol.h"
#include "NclCoordVar.h"
#include "FileSupport.h"
#include "VarSupport.h"
#include "ApiRecords.h"
#include "NclAtt.h"

#include <sys/stat.h>

NclQuark _NclVerifyFile(NclQuark the_path, NclQuark pre_file_ext_q);

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
        NclMultiDValData name_md = NULL,tmp_md = NULL,coord_md = NULL;
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
	ng_size_t i;
	char * v_name;
	char * f_name;
	int index = -1;
	int vindex = -1;
	NclQuark cname = NrmNULLQUARK;
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
	ng_size_t i;
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

      /*
       *fprintf(stdout, "\n\n\nhit _NclFileIsVar. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tvar: %s\n", NrmQuarkToString(var));
       */

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

int _NclFileIsGroup
#if	NhlNeedProto
(NclFile thefile,NclQuark group)
#else 
(thefile,group)
	NclFile thefile;
	NclQuark group;
#endif
{
	NclFileClass fc = NULL;

      /*
       *fprintf(stdout, "\n\n\nhit _NclFileIsGroup. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tgroup: %s\n", NrmQuarkToString(group));
       */

	if(thefile == NULL) {
		return(-1);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.is_group!= NULL) {
      		      /*
        		fprintf(stdout, "\n\n\nend _NclFileIsGroup. file: %s, line: %d\n", __FILE__, __LINE__);
        		fprintf(stdout, "\tgroup: %s\n", NrmQuarkToString(group));
       		       */
			return((*fc->file_class.is_group)(thefile,group));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
      /*
        fprintf(stdout, "\n\n\nend _NclFileIsGroup. file: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stdout, "\tgroup: %s\n", NrmQuarkToString(group));
       */
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
		if(fc->file_class.write_var_var != NULL) {
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

      /*
       *fprintf(stdout, "\nHit _NclFileReadVar, file: %s, line:%d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tvar_name: <%s>\n", NrmQuarkToString(var_name));
       */

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

NclQuark *_NclFileReadVarNames(NclFile thefile, int *num_vars)
{
	char *class_name;

      /*
       *fprintf(stdout, "\nHit _NclFileReadVarNames, file: %s, line:%d\n", __FILE__, __LINE__);
       */

	if(thefile == NULL)
	{
		return(NULL);
	}

	class_name = thefile->obj.class_ptr->obj_class.class_name;

	if(0 == strcmp("NclFileClass", class_name))
	{
		if(thefile->file.format_funcs->get_var_names != NULL)
			return((*thefile->file.format_funcs->get_var_names)
				((void *)thefile->file.private_rec, num_vars));
	}
	else if(0 == strcmp("NclNewFileClass", class_name))
	{
		NclNewFile newfile = (NclNewFile) thefile;
		if(newfile->newfile.format_funcs->get_var_names != NULL)
			return((*newfile->newfile.format_funcs->get_var_names)
				(newfile->newfile.grpnode, num_vars));
	}
	else
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"_NclFileReadVarNames: Unknown Class <%s>\n", class_name));
		return (NULL);
	}
	return (NULL);
}

NclQuark *_NclFileReadGrpNames(NclFile thefile, int *num_grps)
{
	char *class_name;

      /*
       *fprintf(stdout, "\nHit _NclFileReadGrpNames, file: %s, line:%d\n", __FILE__, __LINE__);
       */

	if(thefile == NULL)
	{
		return(NULL);
	}

	class_name = thefile->obj.class_ptr->obj_class.class_name;

	if(0 == strcmp("NclFileClass", class_name))
	{
		if(thefile->file.format_funcs->get_grp_names != NULL)
			return((*thefile->file.format_funcs->get_grp_names)
				((void *)thefile->file.private_rec, num_grps));
	}
	else if(0 == strcmp("NclNewFileClass", class_name))
	{
		NclNewFile newfile = (NclNewFile) thefile;
		if(newfile->newfile.format_funcs->get_grp_names != NULL)
			return((*newfile->newfile.format_funcs->get_grp_names)
				(newfile->newfile.grpnode, num_grps));
	}
	else
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,
			"_NclFileReadGrpNames: Unknown Class <%s>\n", class_name));
		return (NULL);
	}
}

struct _NclFileRec *_NclFileReadGroup
#if	NhlNeedProto
(NclFile thefile, NclQuark group_name)
#else 
(thefile, group_name)
NclFile thefile;
NclQuark group_name;
#endif
{
	NclFileClass fc = NULL;

      /*
       *fprintf(stdout, "\n\nfile: %s, line:%d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
       */

	if(thefile == NULL) {
		return(NULL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.read_group_func != NULL) {
			return((*fc->file_class.read_group_func)(thefile, group_name));
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

extern NhlErrorTypes _NclFileAddVlen(NclFile infile, NclQuark vlen_name, NclQuark var_name,
                                     NclQuark type, NclQuark dim_name)
{
	NclNewFile thefile = (NclNewFile) infile;
	NclNewFileClass fc = NULL;

      /*
       *fprintf(stderr, "\nHit _NclFileAddVlen, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tvlen name: <%s>, var name: <%s>, base type: <%s>, dim_name: <%s>\n",
       *                 NrmQuarkToString(vlen_name), NrmQuarkToString(var_name),
       *                 NrmQuarkToString(type), NrmQuarkToString(dim_name));
       */

	if(infile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddVlen: CANNOT add vlen to empty file.\n"));
		return(NhlFATAL);
	}

	if(! thefile->file.use_new_hlfs)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddVlen: Old File Structure DO NOT Support vlen.\n"));
		return(NhlFATAL);
	}

	fc = (NclNewFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass)
	{
		if(fc->newfile_class.create_vlen_type != NULL)
		{
			return((*fc->newfile_class.create_vlen_type)
                               (infile, vlen_name, var_name, type, dim_name));
		}
		else
		{
			fc = (NclNewFileClass)fc->obj_class.super_class;
		}
	}

	return(NhlFATAL);
}

extern NhlErrorTypes _NclFileAddEnum(NclFile infile, NclQuark enum_name, NclQuark var_name,
                                     NclQuark dim_name, NclQuark *mem_name, void *mem_value,
                                     ng_size_t n_mems, NclBasicDataTypes val_type)
{
	NclNewFile thefile = (NclNewFile) infile;
	NclNewFileClass fc = NULL;

      /*
       *fprintf(stderr, "\nHit _NclFileAddEnum, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tenum name: <%s>, var name: <%s>, dim_name: <%s>\n",
       *                 NrmQuarkToString(enum_name), NrmQuarkToString(var_name),
       *                 NrmQuarkToString(dim_name));
       */

	if(infile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddEnum: CANNOT add enum to empty file.\n"));
		return(NhlFATAL);
	}

	if(! thefile->file.use_new_hlfs)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddEnum: Old File Structure DO NOT Support enum.\n"));
		return(NhlFATAL);
	}

	fc = (NclNewFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass)
	{
		if(fc->newfile_class.create_enum_type != NULL)
		{
			return((*fc->newfile_class.create_enum_type)
                               (infile, enum_name, var_name, dim_name,
                                mem_name, mem_value, n_mems, val_type));
		}
		else
		{
			fc = (NclNewFileClass)fc->obj_class.super_class;
		}
	}

	return(NhlFATAL);
}

extern NhlErrorTypes _NclFileAddCompound(NclFile infile, NclQuark compound_name, NclQuark var_name,
                                         ng_size_t n_dims, NclQuark *dim_name, ng_size_t n_mems,
                                         NclQuark *mem_name, NclQuark *mem_type, int *mem_size)
{
	NclNewFile thefile = (NclNewFile) infile;
	NclNewFileClass fc = NULL;

      /*
       *fprintf(stderr, "\nHit _NclFileAddCompound, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tcompound name: <%s>, var name: <%s>, n_dims = %d, dim_name: <%s>\n",
       *                 NrmQuarkToString(compound_name), NrmQuarkToString(var_name),
       *                 n_dims, NrmQuarkToString(dim_name[0]));
       */

	if(infile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddCompound: CANNOT add compound to empty file.\n"));
		return(NhlFATAL);
	}

	if(! thefile->file.use_new_hlfs)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddCompound: Old File Structure DO NOT Support compound.\n"));
		return(NhlFATAL);
	}

	fc = (NclNewFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass)
	{
		if(fc->newfile_class.create_compound_type != NULL)
		{
			return((*fc->newfile_class.create_compound_type)
                               (infile, compound_name, var_name,
                                n_dims, dim_name,
                                n_mems, mem_name, mem_type, mem_size));
		}
		else
		{
			fc = (NclNewFileClass)fc->obj_class.super_class;
		}
	}

	return(NhlFATAL);
}

extern NhlErrorTypes _NclFileWriteCompound(NclFile infile, NclQuark compound_name, NclQuark var_name,
                                           ng_size_t n_mems, NclQuark *mem_name, NclObj listobj)
{
	NclNewFile thefile = (NclNewFile) infile;
	NclList thelist = (NclList) listobj;
	NclNewFileClass fc = NULL;

      /*
       *fprintf(stderr, "\nHit _NclFileWriteCompound, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tcompound name: <%s>, var name: <%s>, n_mems = %d, mem_name: <%s>\n",
       *                 NrmQuarkToString(compound_name), NrmQuarkToString(var_name),
       *                 n_mems, NrmQuarkToString(mem_name[0]));
       */

	if(thefile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileWriteCompound: CANNOT add compound to empty file.\n"));
		return(NhlFATAL);
	}

	if(! thefile->file.use_new_hlfs)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileWriteCompound: Old File Structure DO NOT Support compound.\n"));
		return(NhlFATAL);
	}

	fc = (NclNewFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass)
	{
		if(fc->newfile_class.create_compound_type != NULL)
		{
			return((*fc->newfile_class.write_compound)
                               (infile, compound_name, var_name,
                                n_mems, mem_name, thelist));
		}
		else
		{
			fc = (NclNewFileClass)fc->obj_class.super_class;
		}
	}

	return(NhlFATAL);
}

extern NhlErrorTypes _NclFileAddOpaque(NclFile infile, NclQuark opaque_name, NclQuark var_name,
                                       int var_size, NclQuark dim_name)
{
	NclNewFile thefile = (NclNewFile) infile;
	NclNewFileClass fc = NULL;

      /*
       *fprintf(stderr, "\nHit _NclFileAddOpaque, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\topaque name: <%s>, var name: <%s>, size: %d, dim_name: <%s>\n",
       *                 NrmQuarkToString(opaque_name), NrmQuarkToString(var_name),
       *                 var_size, NrmQuarkToString(dim_name));
       */

	if(infile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddOpaque: CANNOT add opaque to empty file.\n"));
		return(NhlFATAL);
	}

	if(! thefile->file.use_new_hlfs)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddOpaque: Old File Structure DO NOT Support opaque.\n"));
		return(NhlFATAL);
	}

	fc = (NclNewFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass)
	{
		if(fc->newfile_class.create_opaque_type != NULL)
		{
			return((*fc->newfile_class.create_opaque_type)
                               (infile, opaque_name, var_name, var_size, dim_name));
		}
		else
		{
			fc = (NclNewFileClass)fc->obj_class.super_class;
		}
	}

	return(NhlFATAL);
}

extern NhlErrorTypes _NclFileAddGrp(NclFile infile, NclQuark grpname)
{
	NclNewFile thefile = (NclNewFile) infile;
	NclNewFileClass fc = NULL;

	if(infile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddGrp: CANNOT add group to empty file.\n"));
		return(NhlFATAL);
	}

	if(! thefile->file.use_new_hlfs)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclFileAddGrp: Old File Structure DO NOT Support Group.\n"));
		return(NhlFATAL);
	}

	fc = (NclNewFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass)
	{
		if(fc->newfile_class.write_grp != NULL)
		{
			return((*fc->newfile_class.write_grp)(infile, grpname));
		}
		else
		{
			fc = (NclNewFileClass)fc->obj_class.super_class;
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
extern NhlErrorTypes _NclFileAddVarChunk
#if     NhlNeedProto
(NclFile thefile, NclQuark varname, int n_dims, ng_size_t *dims)
#else
(thefile, varname, n_dims, dims)
NclFile thefile;
NclQuark varname;
int n_dims;
ng_size_t *dims;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.add_var_chunk_func != NULL) {
			return((*fc->file_class.add_var_chunk_func)(thefile, varname, n_dims, dims));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
extern NhlErrorTypes _NclFileAddVarChunkCache
#if     NhlNeedProto
(NclFile thefile, NclQuark varname, ng_size_t cache_size, ng_size_t cache_nelems, float cache_preemption)
#else
(thefile, varname, cache_size, cache_nelems, cache_preemption)
NclFile thefile;
NclQuark varname;
ng_size_t cache_size;
ng_size_t cache_nelems;
float cache_preemption;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.add_var_chunk_cache_func != NULL) {
			return((*fc->file_class.add_var_chunk_cache_func)
				(thefile, varname, cache_size, cache_nelems, cache_preemption));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
extern NhlErrorTypes _NclFileSetVarCompressLevel
#if     NhlNeedProto
(NclFile thefile, NclQuark varname, int compress_level)
#else
(thefile, varname, compress_level)
NclFile thefile;
NclQuark varname;
int compress_level;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.set_var_compress_level_func != NULL) {
			return((*fc->file_class.set_var_compress_level_func)(thefile, varname, compress_level));
		} else {
			fc = (NclFileClass)fc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
extern NhlErrorTypes _NclFileAddDim
#if     NhlNeedProto
(NclFile thefile, NclQuark dimname, ng_size_t dimsize, int is_unlimited)
#else
(thefile, dimname, dimsize, is_unlimited)
NclFile thefile;
NclQuark dimname;
ng_size_t dimsize;
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

extern NhlErrorTypes _NclFileAddChunkDim
#if     NhlNeedProto
(NclFile thefile, NclQuark dimname, ng_size_t dimsize, int is_unlimited)
#else
(thefile, dimname, dimsize, is_unlimited)
NclFile thefile;
NclQuark dimname;
ng_size_t dimsize;
int is_unlimited;
#endif
{
	NclFileClass fc = NULL;

	if(thefile == NULL) {
		return(NhlFATAL);
	}
	fc = (NclFileClass)thefile->obj.class_ptr;
	while((NclObjClass)fc != nclObjClass) {
		if(fc->file_class.add_chunk_dim_func != NULL) {
			return((*fc->file_class.add_chunk_dim_func)(thefile, dimname, dimsize, is_unlimited));
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
				ret = nclfprintf(fp,"Total Size: %lld bytes\n",(long long)total * _NclSizeOf(thefile->file.var_info[i]->data_type));
                		if(ret < 0) {
                        		return(NhlWARNING);
                		}
				ret = nclfprintf(fp,"            %lld values\n",(long long)total);
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
						ng_size_t size = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
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
						if (! tmp_var) {
							return NhlFATAL;
						}
						tmp_md = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
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
					if (step->the_att->num_elements == 0) {
						ret = nclfprintf(fp,"<NULL>\n");
						if(ret < 0) {
							return(NhlWARNING);
						}
					}						
					else if(step->the_att->num_elements == 1) {
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
	}
	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"in _NclPrintFileVarSummary"));
	NhlPError(NhlFATAL,NhlEUNKNOWN,"printFileVarSummary: (%s) is not a variable in the file (%s)",NrmQuarkToString(varname),NrmQuarkToString(thefile->file.fname));
	return(NhlFATAL);	
}

NclQuark *_NclSplitGroupPath
#if NhlNeedProto
(NclQuark group_name, int *n_lvls)
#else
(group_name, n_lvls)
NclQuark group_name;
int *n_lvls;
#endif
{
	NclQuark *splited_group_names = NULL;
	char tmp_str[1024];
	char tmp_delim[32];
	char *result = NULL;
	int n = 0;
	int max_lvl = 0;

      /*
       *fprintf(stdout, "\n\n\nhit _NclSplitGroupPath. file: %s, line: %d\n", __FILE__, __LINE__);
       */
	
	strcpy(tmp_str, (char *) NrmQuarkToString(group_name));

      /*
       *fprintf(stdout, "\tgroup_name: <%s>\n", tmp_str);
       */

	if('/' != tmp_str[0])
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"in _NclSplitGroupPath, group name started with <%s>", tmp_str[0]));
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclSplitGroupPath: group name (%s) did not start with '/'(slash).",
			NrmQuarkToString(group_name), tmp_str[0]);
		return (NULL);
	}

	strcpy(tmp_delim, "/");
	for(n = 0; n < strlen(tmp_str); n++)
		if('/' == tmp_str[n])
			max_lvl++;

	splited_group_names = (NrmQuark *) NclMalloc(sizeof(NclQuark) * (max_lvl));

	result = strtok(tmp_str, tmp_delim);
	n = 0;
	while(result != NULL)
	{
		splited_group_names[n] = NrmStringToQuark(result);
              /*
	       *fprintf(stdout, "\tsplited_group_names[%d]: <%s>\n", n, result);
               */
		n++;
		result = strtok(NULL, tmp_delim);
	}
	*n_lvls = n;
	return (splited_group_names);
}

NclQuark *_NclGetFileGroupsList
#if NhlNeedProto
(NclFile thefile, NclQuark base_group_name, int depth, int *n_grps)
#else
(thefile, base_group_name, depth, n_grps)
NclFile thefile; 
NclQuark base_group_name; 
int depth; 
int *n_grps; 
#endif
{
	int i, j;
	NclQuark *selected_group_names = NULL;
	NclQuark *splited_base = NULL;
	NclQuark *splited_name = NULL;
	int need_save = 0;
	int num_grps = 0;
	int max_depth = INT_MAX;
	int bas_depth = 0;
	int cur_depth = 0;
	int dif_depth = 0;

      /*
       *fprintf(stdout, "\n\n\nhit _NclGetFileGroupsList. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tbase_group_name: <%s>\n", NrmQuarkToString(base_group_name));
       *fprintf(stdout, "\tdepth: %d\n", depth);
       *fprintf(stdout, "\tthefile->file.n_grps: %d\n", thefile->file.n_grps);
       */

	if(thefile->file.n_grps < 1)
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"in _NclGetFileGroupsList"));
		NhlPError(NhlFATAL,NhlEUNKNOWN,"getfilegroups: no groups under (%s)",
			NrmQuarkToString(base_group_name));
		*n_grps = 0;
		return (NULL);
	}

	splited_base = _NclSplitGroupPath(base_group_name, &bas_depth);

	if(depth)
		max_depth = depth;

      /*
       *fprintf(stdout, "\tmax_depth = %d\n", max_depth);
       *fprintf(stdout, "\tbas_depth = %d\n", bas_depth);
       */

	selected_group_names = (NclQuark *) NclMalloc(sizeof(NclQuark) * thefile->file.n_grps);

      /*
       *fprintf(stdout, "\n\n\nhit _NclGetFileGroupsList. file: %s, line: %d\n", __FILE__, __LINE__);
       *for(i = 0; i < thefile->file.n_grps; i++)
       *{
       *	fprintf(stdout, "\tthefile->file.grp_info[%d]->grp_name_quark = <%s>\n",
       *		i, NrmQuarkToString(thefile->file.grp_info[i]->grp_name_quark));
       *}
       */

	for(i = 0; i < thefile->file.n_grps; i++)
	{
		need_save = 1;

	      /*
	       *fprintf(stdout, "\tthefile->file.grp_info[%d]->grp_name_quark = <%s>\n",
	       *	i, NrmQuarkToString(thefile->file.grp_info[i]->grp_name_quark));
	       */

		splited_name = _NclSplitGroupPath(thefile->file.grp_info[i]->grp_name_quark, &cur_depth);
		dif_depth = cur_depth - bas_depth;
	      /*
	       *fprintf(stdout, "\tbas_depth = %d\n", bas_depth);
	       *fprintf(stdout, "\tcur_depth = %d\n", cur_depth);
	       *fprintf(stdout, "\tdif_depth = %d\n", dif_depth);
	       *fprintf(stdout, "\tmax_depth = %d\n", max_depth);
	       */

		if(dif_depth < 1)
			continue;
		else if(dif_depth > max_depth)
			continue;

		for(j = 0; j < bas_depth; j++)
		{
	              /*
		       *fprintf(stdout, "\tsplited_base[%d] = <%s>\n", j, NrmQuarkToString(splited_base[j]));
		       *fprintf(stdout, "\tsplited_name[%d] = <%s>\n", j, NrmQuarkToString(splited_name[j]));
	               */
			if(splited_base[j] != splited_name[j])
			{
				need_save = 0;
				break;
			}
		}

		if(need_save)
		{
			selected_group_names[num_grps] = thefile->file.grp_info[i]->grp_name_quark;
	              /*
		       *fprintf(stdout, "\tselected_group_names[%d]: <%s>\n",
		       *	num_grps, NrmQuarkToString(selected_group_names[num_grps]));
	               */
		    num_grps++;
		}
	}

	if(num_grps < 1)
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"in _NclGetFileGroupsList"));
		NhlPError(NhlFATAL,NhlEUNKNOWN,"getfilegroups: can not find any group under (%s)",
			NrmQuarkToString(base_group_name));
		NclFree(selected_group_names);
		return (NULL);
	}

	if(num_grps < thefile->file.n_grps)
	{
		selected_group_names = (NclQuark *) NclRealloc(selected_group_names, sizeof(NclQuark) * num_grps);
	}
      /*
       *fprintf(stdout, "\tnum_grps = %d\n", num_grps);
       */
	*n_grps = num_grps;
	return (selected_group_names);	
}

NclQuark *_NclGetGroupVarsList
#if NhlNeedProto
(NclFile thefile, NclQuark base_group_name, int depth, int *n_vars)
#else
(thefile, base_group_name, depth, n_vars)
NclFile thefile; 
NclQuark base_group_name; 
int depth; 
int *n_vars; 
#endif
{
	int i, j;
	NclQuark *selected_var_names = NULL;
	NclQuark *splited_base = NULL;
	NclQuark *splited_name = NULL;
	NclQuark *final_names = NULL;
	int need_save = 0;
	int num_vars = 0;
	int max_depth = INT_MAX;
	int bas_depth = 0;
	int cur_depth = 0;
	int dif_depth = 0;

      /*
       *fprintf(stdout, "\n\n\nhit _NclGetGroupVarsList. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tbase_group_name: <%s>\n", NrmQuarkToString(base_group_name));
       *fprintf(stdout, "\tdepth: %d\n", depth);
       *fprintf(stdout, "\tthefile->file.n_vars: %d\n", thefile->file.n_vars);
       */

        if(thefile->file.n_vars < 1)
        {
        	NHLPERROR((NhlFATAL,NhlEUNKNOWN,"in _NclGetGroupVarsList"));
        	NhlPError(NhlFATAL,NhlEUNKNOWN,"getgroupvars: no vars under (%s)",
        		NrmQuarkToString(base_group_name));
        	*n_vars = 0;
        	return (NULL);
        }

	splited_base = _NclSplitGroupPath(base_group_name, &bas_depth);

	if(depth)
		max_depth = depth;

      /*
       *fprintf(stdout, "\tmax_depth = %d\n", max_depth);
       *fprintf(stdout, "\tbas_depth = %d\n", bas_depth);
       */

	selected_var_names = (NclQuark *) NclMalloc(sizeof(NclQuark) * thefile->file.n_vars);
	final_names = (NclQuark *) NclMalloc(sizeof(NclQuark) * thefile->file.n_vars);

      /*
       *fprintf(stdout, "\n\n\nhit _NclGetGroupVarsList. file: %s, line: %d\n", __FILE__, __LINE__);
       */

	for(i = 0; i < thefile->file.n_vars; i++)
	{
		need_save = 1;

	      /*
	       *fprintf(stdout, "\tthefile->file.var_info[%d]->var_full_name_quark = <%s>\n",
	       *	i, NrmQuarkToString(thefile->file.var_info[i]->var_full_name_quark));
	       */

		splited_name = _NclSplitGroupPath(thefile->file.var_info[i]->var_full_name_quark, &cur_depth);
		dif_depth = cur_depth - bas_depth;
	      /*
	       *fprintf(stdout, "\tbas_depth = %d\n", bas_depth);
	       *fprintf(stdout, "\tcur_depth = %d\n", cur_depth);
	       *fprintf(stdout, "\tdif_depth = %d\n", dif_depth);
	       *fprintf(stdout, "\tmax_depth = %d\n", max_depth);
	       */

		if(dif_depth < 1)
			continue;
		else if(dif_depth > max_depth)
			continue;

		for(j = 0; j < bas_depth; j++)
		{
	              /*
		       *fprintf(stdout, "\tsplited_base[%d] = <%s>\n", j, NrmQuarkToString(splited_base[j]));
		       *fprintf(stdout, "\tsplited_name[%d] = <%s>\n", j, NrmQuarkToString(splited_name[j]));
	               */
			if(splited_base[j] != splited_name[j])
			{
				need_save = 0;
				break;
			}
		}

		if(need_save)
		{
			for(j = 0; j < thefile->file.n_grps; j++)
			{
				if(thefile->file.grp_info[j]->grp_name_quark == thefile->file.var_info[i]->var_full_name_quark)
				need_save = 0;
                                break;
			}

			if(need_save)
			{
				for(j = 0; j < num_vars; j++)
				{
					if(selected_var_names[j] == thefile->file.var_info[i]->var_full_name_quark)
					{
						need_save = 0;
                                		break;
					}
				}

				if(need_save)
				{
					selected_var_names[num_vars] = thefile->file.var_info[i]->var_full_name_quark;
					num_vars++;
				}
			}
		}
	}

	if(num_vars < 1)
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"in _NclGetGroupVarsList"));
		NhlPError(NhlFATAL,NhlEUNKNOWN,"getgroupvars: can not find any group under (%s)",
			NrmQuarkToString(base_group_name));
		NclFree(selected_var_names);
		return (NULL);
	}

	if(num_vars < thefile->file.n_vars)
	{
		selected_var_names = (NclQuark *) NclRealloc(selected_var_names, sizeof(NclQuark) * num_vars);
	}
      /*
       *fprintf(stdout, "\tnum_vars = %d\n", num_vars);
       */
	*n_vars = num_vars;
	return (selected_var_names);	
}

NclApiDataList *_NclGetFileVarInfoList2
#if	NhlNeedProto
(struct _NclFileRec *thefile)
#else
(thefile)
	struct _NclFileRec *thefile;
#endif
{
	NclApiDataList *tmp = NULL, *thelist = NULL;
	int i,j;
	NclFileAttInfoList *step;

	if(thefile ==NULL) {
		return NULL;
	}
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
	return thelist;
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
	NclApiDataList *tmp = NULL;
	int i,j;
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
	int j;

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
	return NULL;
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

#ifdef USE_NETCDF4_FEATURES
	if(NCLnewfs)
		fc = (NclFileClass) &nclNewFileClassRec;
	else
#endif
		fc = &nclFileClassRec;

	while(fc)
	{
		if(NULL != fc->file_class.set_file_option)
			return((*fc->file_class.set_file_option)(thefile, format, option, value));
		else
			fc = (NclFileClass)fc->obj_class.super_class;
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
	int i = 5;

#ifdef USE_NETCDF4_FEATURES
	if(NCLnewfs)
		fc = (NclFileClass) &nclNewFileClassRec;
	else
#endif
		fc = &nclFileClassRec;

        while((! fc) && i)
	{
		if(fc->file_class.num_options)
			break;
		fc = (NclFileClass)fc->obj_class.super_class;
		i--;
        }

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

NclQuark _NclFindFileExt(NclQuark path, NclQuark *fname_q, NhlBoolean *is_http,
			char **end_of_name, int *len_path, int rw_status)
{
	NclQuark file_ext_q = -1;

	char *the_path = NrmQuarkToString(path);
	char *last_slash = NULL;
	char buffer[NCL_MAX_STRING];
	struct stat buf;

	int i;

	if(strncmp(the_path,"http://",7))
		*is_http = False;
	else
		*is_http = True;

	last_slash = strrchr(the_path,'/');
	if(last_slash == NULL) {
		last_slash = the_path;
		*len_path = 0;
	} else {
		last_slash++;
	}

	*end_of_name = strrchr(last_slash,'.');
	if (*is_http) {
		if (*end_of_name == NULL) {
			*end_of_name = &last_slash[strlen(last_slash)];
		}
		*len_path = *end_of_name - the_path;
		i = 0;
		while(last_slash != *end_of_name) {
			buffer[i] = *last_slash;
			i++;
			last_slash++;
		}
		buffer[i] = '\0';
		*fname_q = NrmStringToQuark(buffer);
		(*end_of_name)++;

	        file_ext_q = NrmStringToQuark("nc");
		return file_ext_q;
	}
	else if(*end_of_name == NULL) {
		NclQuark the_real_path = NrmStringToQuark(_NGResolvePath(NrmQuarkToString(path)));
		NclQuark old_file_ext_q = NrmStringToQuark("nc");
		struct stat file_stat;

		if(0 == stat(NrmQuarkToString(the_real_path), &file_stat))
		{
			if(file_stat.st_size)
				file_ext_q = _NclVerifyFile(the_real_path, old_file_ext_q);
		}

	} else {
		if (1 == rw_status)
		{
			if(stat(_NGResolvePath(the_path),&buf) == -1)
			{
				char tmp_path[NCL_MAX_STRING];
				char tmp_name[NCL_MAX_STRING];
				strcpy(tmp_path, the_path);
				strcpy(tmp_name, *end_of_name);
				tmp_path[strlen(the_path) - strlen(tmp_name)] = '\0';

				if(stat(_NGResolvePath(tmp_path),&buf) == -1)
				{
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,
						"_NclFindFileExt: Requested file <%s> or <%s> does not exist\n", the_path, tmp_path));
                                		return(-1);
				}
			}
		}

		*len_path = *end_of_name - the_path;
		i = 0;
		while(last_slash != *end_of_name) {
			buffer[i] = *last_slash;
			i++;
			last_slash++;
		}
		buffer[i] = '\0';
		*fname_q = NrmStringToQuark(buffer);
		(*end_of_name)++;

		strcpy(buffer, *end_of_name);
                for(i = 0; i < strlen(buffer); ++i)
			buffer[i] = tolower(buffer[i]);

		file_ext_q = NrmStringToQuark(buffer);
	}

	return file_ext_q;
}

NclQuark _NclVerifyFile(NclQuark the_path, NclQuark pre_file_ext_q)
{
	NclQuark cur_ext_q;
	NclQuark ori_file_ext_q = pre_file_ext_q;
	NclQuark file_ext_q = pre_file_ext_q;

	char *fext = NrmQuarkToString(pre_file_ext_q);

	char *ext_list[] = {"nc"
#ifdef BuildHDF5
			   , "h5"
#endif
#ifdef BuildHDFEOS5
			   , "he5"
#endif
#ifdef BuildHDFEOS
			   , "he2"
#endif
#ifdef BuildHDF4
			   , "hdf"
#endif
			   };

	int found = 0;
	int n = -1;
	int sizeofextlist = sizeof(ext_list) / sizeof(ext_list[0]);

	char filename[NCL_MAX_STRING];
	struct stat buf;

	for(n = 0; n < strlen(fext); ++n)
 		fext[n] = tolower(fext[n]);

	if(0 == strncmp(fext, "gr", 2))
		return file_ext_q;
#ifdef BuildGDAL
	else if(0 == strncmp(fext, "shp", 3))
	{
       		return file_ext_q;
	}
#endif
	else if((0 == strcmp(fext, "cdf")) || (0 == strcmp(fext, "nc3")) ||
           (0 == strcmp(fext, "nc4")) || (0 == strcmp(fext, "netcdf")))
		ori_file_ext_q = NrmStringToQuark("nc");
#ifdef BuildHDF4
	else if((0 == strcmp(fext, "hd")) || (0 == strcmp(fext, "h4")))
		ori_file_ext_q = NrmStringToQuark("hdf");
#endif
#ifdef BuildHDF5
	else if(0 == strcmp(fext, "hdf5"))
		ori_file_ext_q = NrmStringToQuark("h5");
#endif
#ifdef BuildHDFEOS
	else if((0 == strcmp(fext, "hdfeos")) || (0 == strcmp(fext, "he")) || (0 == strcmp(fext, "he4")))
		ori_file_ext_q = NrmStringToQuark("he2");
#endif
#ifdef BuildHDFEOS5
	else if(0 == strcmp(fext, "hdfeos5"))
		ori_file_ext_q = NrmStringToQuark("he5");
#endif

	strcpy(filename, NrmQuarkToString(the_path));

	if(stat(_NGResolvePath(filename),&buf) == -1)
	{
		char tmp_path[NCL_MAX_STRING];
		char tmp_name[NCL_MAX_STRING];
		strcpy(tmp_path, filename);
		strcpy(tmp_name, NrmQuarkToString(pre_file_ext_q));
		tmp_path[strlen(filename) - strlen(tmp_name) - 1] = '\0';

		if(stat(_NGResolvePath(tmp_path),&buf) == -1)
		{
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"_NclVerifyFile: Requested file <%s> or <%s> does not exist\n", filename, tmp_path));
			return(-1);
		}
		strcpy(filename, tmp_path);
	}


	for(n = -1; n < sizeofextlist; n++)
	{
		if(0 > n)
			cur_ext_q = ori_file_ext_q;
		else
		{
			cur_ext_q = NrmStringToQuark(ext_list[n]);
			if(ori_file_ext_q == cur_ext_q)
				continue;
		}

		if(NrmStringToQuark("nc") == cur_ext_q)
		{
			int cdfid;
			int format;
			int nc_ret = NC_NOERR;
			ng_usize_t ChunkSizeHint = 64 * getpagesize();
			nc_ret = nc__open(filename,NC_NOWRITE,&ChunkSizeHint,&cdfid);

			if(NC_NOERR != nc_ret)
			{
				continue;
			}

			nc_inq_format(cdfid, &format);

        	       /**format
         		*Pointer to location for returned format version, one of
			*	NC_FORMAT_CLASSIC,
         		*       NC_FORMAT_64BIT,
         		*       NC_FORMAT_NETCDF4,
         		*       NC_FORMAT_NETCDF4_CLASSIC.
         		*/
			switch(format)
			{
#ifdef USE_NETCDF4_FEATURES
              			case NC_FORMAT_NETCDF4:
					file_ext_q = cur_ext_q;
					found = 1;
                   			NCLnewfs = 1;
                   			break;
#endif
              			case NC_FORMAT_NETCDF4_CLASSIC:
              			case NC_FORMAT_64BIT:
              			case NC_FORMAT_CLASSIC:
					file_ext_q = cur_ext_q;
					found = 1;
                   			break;
              			default:
					found = 0;
                   			break;
			}

			ncclose(cdfid);

			if(found)
				break;
		}
#ifdef BuildHDF5
		else if(NrmStringToQuark("h5") == cur_ext_q)
		{
			htri_t status = H5Fis_hdf5(filename);

			if(status)
			{
        			file_ext_q = cur_ext_q;
				found = 1;
				break;
			}
			else
				found = 0;
		}
#endif
#ifdef BuildHDFEOS5
		else if(NrmStringToQuark("he5") == cur_ext_q)
		{
			long str_buf_size = 0;
			long nsw = 0;
			long ngd = 0;
			long npt = 0;
			long nza = 0;

			/*HDFEOS5 file should be first a HDF5 file.*/
			htri_t status = H5Fis_hdf5(filename);

			if(! status)
			{
				found = 0;
				continue;
			}

			nsw = HE5_SWinqswath(filename, NULL, &str_buf_size);
			ngd = HE5_GDinqgrid (filename, NULL, &str_buf_size);
			npt = HE5_PTinqpoint(filename, NULL, &str_buf_size);
			nza = HE5_ZAinqza   (filename, NULL, &str_buf_size);

			if((npt <= 0) && (nsw <= 0) && (ngd <= 0) && (nza <= 0))
				found = 0;
			else
			{
        			file_ext_q = cur_ext_q;
        			found = 1;
				break;
			}
		}
#endif
#ifdef BuildHDFEOS
		else if(NrmStringToQuark("he2") == cur_ext_q)
		{
			int32 nsw = 0;
			int32 ngd = 0;
			int32 npt = 0;
			int32 bsize;

			/*A HDFEOS(2) file must be a hdf(4) file first*/
			intn status = Hishdf(filename);

			if(! status)
			{
        			found = 0;
				continue;
			}

			nsw = SWinqswath(filename, NULL, &bsize);
			ngd = GDinqgrid (filename, NULL, &bsize);
			npt = PTinqpoint(filename, NULL, &bsize);

			if((npt <= 0) && (nsw <= 0) && (ngd <=0))
				found = 0;
			else
			{
        			file_ext_q = cur_ext_q;
        			found = 1;
				break;
			}
		}
#endif
#ifdef BuildHDF4
		else if(NrmStringToQuark("hdf") == cur_ext_q)
		{
			intn status = Hishdf(filename);

			if(status)
			{
        			file_ext_q = cur_ext_q;
        			found = 1;
				break;
			}
			else
				found = 0;
		}
#endif
		else
		{
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
					"NCL does not know anything about file suffix <%s>. \n%s\n",
				 	NrmQuarkToString(cur_ext_q),
					"But NCL will try its best to figure out the file format."));
		}
	}

	return file_ext_q;
}


NclFile _NclCreateFile(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
			unsigned int obj_type_mask, NclStatus status,
			NclQuark path, int rw_status)
{
	NclFile file_out = NULL;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);

	NclQuark file_ext_q = -1;
	NclQuark fname_q;
	NhlBoolean is_http;
	char *end_of_name = NULL;
	int len_path;

	static int first = 1;

        struct stat file_stat;

      /*Save the previous NCLnewfs.*/
	short preNCLnewfs = NCLnewfs;

	file_ext_q = _NclFindFileExt(path, &fname_q, &is_http, &end_of_name, &len_path, rw_status);

	if(! is_http)
	{
		if(0 > file_ext_q)
		{
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"(%s) has no file extension, can't determine type of file to open",NrmQuarkToString(path)));
			return(NULL);
		}
		else if (rw_status > -1)
		{
			NclQuark the_real_path = NrmStringToQuark(_NGResolvePath(NrmQuarkToString(path)));
			NclQuark old_file_ext_q = file_ext_q;

			file_ext_q = -1;

			if((0 == stat(NrmQuarkToString(the_real_path), &file_stat)) &&
					(file_stat.st_size))
				file_ext_q = _NclVerifyFile(the_real_path, old_file_ext_q);
			else
			{
				char tmp_path[NCL_MAX_STRING];
				char *ext_name;
				strcpy(tmp_path, NrmQuarkToString(the_real_path));

				ext_name = strrchr(tmp_path, '.');
				/*Use while loop will allow user to append multiple extensions.
				*But it will be not consistent to addfile.
				*So we comment out the while loop for NOW.
				*Wei Huang, 05/21/2012
				*/
				/*while(NULL != ext_name)*/
				if(NULL != ext_name)
				{
					tmp_path[strlen(tmp_path) - strlen(ext_name)] = '\0'; 
				
					if(! stat(_NGResolvePath(tmp_path), &file_stat))
					{
						file_ext_q = _NclVerifyFile(NrmStringToQuark(tmp_path), old_file_ext_q);
						/*break;*/
					}
					ext_name = strrchr(tmp_path, '.');
				}
			}

			if(0 > file_ext_q)
			{
				fprintf(stderr, "\tfile_ext_q: <%s>\n", "Undefined");
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"_NclCreateFile: Can not open file: <%s> properly.\n",
				 	NrmQuarkToString(the_real_path)));
				return file_out;
			}
		}

		if(first)
		{
			first = 0;
			/* Check if new file-strucuture */
			if(NULL != fcp->options[Ncl_USE_NEW_HLFS].value)
			{
				int newfs = 0;
				newfs = *(int *)(fcp->options[Ncl_USE_NEW_HLFS].value->multidval.val);
				if(newfs)
				{
				      /*Only certain data format can use new file-structure. Wei 01/11/2013*/
					if((NrmStringToQuark("nc") == file_ext_q) ||
					   (NrmStringToQuark("nc4") == file_ext_q) ||
					   (NrmStringToQuark("nc3") == file_ext_q) ||
					   (NrmStringToQuark("cdf") == file_ext_q) ||
					   (NrmStringToQuark("netcdf") == file_ext_q))
						NCLnewfs = 1;
					else
						NCLnewfs = 0;
				}
				else
					NCLnewfs = 0;
			}
		}
	}

#ifdef USE_NETCDF4_FEATURES
	if(NCLnewfs)
	{
		file_out = _NclNewFileCreate(inst, theclass, obj_type, obj_type_mask, status,
				path, rw_status, file_ext_q, fname_q, is_http, end_of_name, len_path);
	}					
	else
#endif
	{
		file_out = _NclFileCreate(inst, theclass, obj_type, obj_type_mask, status,
				path, rw_status, file_ext_q, fname_q, is_http, end_of_name, len_path);
	}					

      /*Set back to previous NCLnewfs.*/
	NCLnewfs = preNCLnewfs;

	return file_out;
}

NhlErrorTypes _NclPrintFileSummary(NclObj self, FILE *fp)
{
	NclFile file = (NclFile) self;
#ifdef USE_NETCDF4_FEATURES
	if(file->file.use_new_hlfs)
	{
		return (_NclNewFilePrintSummary(self, fp));
	}
	else
#endif
	{
		return (_NclFilePrintSummary(self, fp));
	}
}

NclGroup *_NclCreateGroup(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                         unsigned int obj_type_mask, NclStatus status,
                         NclFile file_in, NclQuark group_name)
{
    NclGroup *group_out = NULL;

  /*
   *fprintf(stderr, "\nEnter _NclCreateGroup, file: %s, line: %d\n", __FILE__, __LINE__);
   */

#ifdef USE_NETCDF4_FEATURES
    if(file_in->file.use_new_hlfs)
    {
        group_out = _NclNewGroupCreate(inst, theclass, obj_type, obj_type_mask,
                                       status, file_in, group_name);
    }                    
    else
#endif
    {
        group_out = _NclGroupCreate(inst, theclass, obj_type, obj_type_mask,
                                    status, file_in, group_name);
    }                    

  /*
   *fprintf(stderr, "Leave _NclCreateGroup, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
    return group_out;
}

ng_size_t *_NclFileReadChunkSizes(NclFile thefile, int *nchunks)
{
	ng_size_t *chunksize = NULL;

	char *class_name;

	if(thefile == NULL)
	{
		return(NULL);
	}

	class_name = thefile->obj.class_ptr->obj_class.class_name;

	if (thefile->file.use_new_hlfs) 
	{
		NclNewFile newfile = (NclNewFile) thefile;
		NclFileDimRecord *chunkdimrec = newfile->newfile.grpnode->chunk_dim_rec;
		int n;
		if(NULL != chunkdimrec)
		{
			*nchunks = chunkdimrec->n_dims;
			chunksize = (ng_size_t *)NclMalloc(chunkdimrec->n_dims * sizeof(ng_size_t));
			if(NULL == chunksize)
			{
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,
					"_NclFileReadChunkSizes: Can not allocate memory for chunksize\n"));
				return (NULL);
			}
			for(n = 0; n < chunkdimrec->n_dims; n++)
				chunksize[n] = chunkdimrec->dim_node[n].size;

			return chunksize;
		}
		else
		{
			*nchunks = 0;
			return NULL;
		}
	}

	*nchunks = 0;
	NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		"_NclFileReadChunkSizes: Unknown Class <%s>\n", class_name));
	return (NULL);
}

int _NclFileReadCompressionLevel(NclFile thefile)
{
	int cl = 0;
	char *class_name;

	if(thefile == NULL)
	{
		return(0);
	}

	class_name = thefile->obj.class_ptr->obj_class.class_name;

	if (thefile->file.use_new_hlfs)
	{
		NclNewFile newfile = (NclNewFile) thefile;
		cl = newfile->newfile.grpnode->compress_level;
		return cl;
	}

	NHLPERROR((NhlFATAL,NhlEUNKNOWN,
		"_NclFileReadCompressionLevel: Unknown Class <%s>\n", class_name));
	return (0);
}

NclQuark _NclFileReadVersion(NclFile thefile)
{
	NclQuark version = NrmStringToQuark("unknown");
	char *class_name;

	if(thefile == NULL)
	{
		return version;
	}

	class_name = thefile->obj.class_ptr->obj_class.class_name;

	if(0 == strcmp("NclNewFileClass", class_name))
	{
		NclNewFile newfile = (NclNewFile) thefile;
		version = newfile->newfile.grpnode->kind;
	}
	else
	{
		if(thefile->file.file_ext_q == NrmStringToQuark("nc"))
		{
			NHLPERROR((NhlWARNING,NhlEUNKNOWN,
				"_NclFileReadVersion: \n%s%s%s%s%s\n",
				"\t\t\t add line: <setfileoption(\"nc\", \"usenewhlfs\", True)>\n",
				"\t\t\t before open a NetCDF file(in your script)\n",
				"\t\t\t or add '-f' option to run ncl\n",
				"\t\t\t to use the new-file-structure\n",
				"\t\t\t to get the version/kind info.\n"));
		}
		else
		{
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,
				"_NclFileReadVersion: Unknown Class <%s>\n", class_name));
		}
	}

	return version;
}


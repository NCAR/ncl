
/*
 *      $Id: FileSupport.c,v 1.1 1994-07-14 20:45:50 ethan Exp $
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
#include "NclMultiDValData.h"
#include "NclFile.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "Symbol.h"
#include "Machine.h"


NhlErrorTypes _NclBuildFileRSelection
#if  __STDC__
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
/*
*---------> code needed here <-------------------
*/
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


NhlErrorTypes  _NclBuildFileVSelection
#if  __STDC__
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
		if(!(vect_md->obj.obj_type_mask & Ncl_MultiDVallongData)) {
			tmp_md = _NclCoerceData(vect_md,Ncl_MultiDVallongData,NULL);
			
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
				sel->u.vec.max = thevector[i];
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
#if  __STDC__
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
#if  __STDC__
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
	return(0);
}

NhlErrorTypes _NclFileWriteVarVar
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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

NhlErrorTypes _NclFileWriteVarAtt
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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

NhlErrorTypes _NclFileWriteAtt
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
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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
#if  __STDC__
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

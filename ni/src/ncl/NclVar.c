
/*
 *      $Id: NclVar.c,v 1.12 1995-03-09 20:45:08 haley Exp $
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
 *	Date:		Thu Jan 13 15:04:25 MST 1994
 *
 *	Description:	
 */
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "Machine.h"
#include "NclAtt.h"
#include "NclVar.h"
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCoordVar.h"
static NhlErrorTypes VarWrite(
#if 	NhlNeedProto
	struct _NclVarRec * /* self */,
	struct _NclMultiDValDataRec * /* value */,
	struct _NclSelectionRecord * /*sel_ptr*/
#endif
);
	
static struct _NclVarRec *VarRead(
#if	NhlNeedProto
	struct _NclVarRec * /* self*/,
	struct _NclSelectionRecord * /*sel_ptr*/
#endif
);
static struct _NclDataRec *VarValRead(
#if	NhlNeedProto
	struct _NclVarRec * /* self*/,
	struct _NclSelectionRecord * /*sel_ptr*/,
	NclScalar * /*new_missing*/
#endif
);

static void VarPrint(
#if	NhlNeedProto
struct	_NclObjRec*	/*self*/,
FILE	*		/*fp*/
#endif
);

static void	VarDestroy(
#if	NhlNeedProto
struct  _NclObjRec*	/*self*/
#endif
);

static struct _NclDataRec * VarCoerce(
#if	NhlNeedProto
struct  _NclVarRec*	/*self*/,
NclObjTypes	/* coerce_to_obj */,
NclScalar *	/* coerce_to_obj */
#endif
);


static NclObjTypes VarValRep(
#if	NhlNeedProto
struct _NclVarRec*	/*self*/
#endif
);

static struct _NclMultiDValDataRec  *VarReadAtt(
#if	NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*attname */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static NhlErrorTypes VarWriteAtt(
#if	NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*attname */,
struct _NclMultiDValDataRec * /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int VarIsAAtt (
#if  	NhlNeedProto
struct _NclVarRec * /*self*/,
char* /*attname*/
#endif
);
static int VarIsADim(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char* /*attname*/
#endif
);

static struct _NclMultiDValDataRec *VarReadDim(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
char    *               /*dim_name*/,
long                     /*dim_num*/
#endif
);

static struct _NclDimRec *VarGetDimInfo(
#if     NhlNeedProto
struct _NclVarRec       * /*self*/,
char *                  /*dim_name*/,
long                     /*dim_num*/
#endif
);

static NhlErrorTypes VarWriteDim(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
long                     /*dim_num*/,
char    *               /*dim_name*/
#endif
);


static int VarIsACoord(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char* /*attname*/
#endif
);

static struct _NclVarRec *VarReadCoord(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
char    *               /* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

static NhlErrorTypes VarWriteCoord(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
struct  _NclMultiDValDataRec    * /*value*/,
char    *               /* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

static NhlErrorTypes VarDelParent(
#if	NhlNeedProto
struct _NclObjRec *     /*theobj*/,
struct _NclObjRec *     /*parent*/
#endif
);

static NhlErrorTypes VarAddParent(
#if	NhlNeedProto
struct _NclObjRec *     /*theobj*/,
struct _NclObjRec *     /*parent*/
#endif
);
static NhlErrorTypes VarVarWrite(
#if NhlNeedProto
struct _NclVarRec * /*lhs*/,
struct _NclSelectionRecord * /*lhs_sel_ptr*/,
struct _NclVarRec * /*rhs*/,
struct _NclSelectionRecord * /*rhs_sel_ptr*/
#endif 
);

static struct _NclVarRec * VarCopy(
#if NhlNeedProto
struct _NclVarRec * /*thevar*/, 
NclScalar * /*new_missing*/, 
struct _NclVarRec * /*storage*/
#endif
);


NclVarClassRec nclVarClassRec = {
	{
		"NclVarClass",
		sizeof(NclVarRec),
		(NclObjClass)&nclObjClassRec,
		0,
		(NclGenericFunction)VarDestroy,
		(NclSetStatusFunction)NULL /*VarSetStatus*/,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)NULL,
		(NclAddParentFunction)VarAddParent,
                (NclDelParentFunction)VarDelParent,
/* NclPrintFunction print */	VarPrint
	},
	{
/* NclRepValueFunc rep_val */		VarValRep,
/* NclGetValFunc get_val*/		NULL,
/* NclVarCoerceFunc */			VarCoerce,
/* NclCopyVarFunc */			VarCopy,

/* NclAssignFunction write_func */	VarWrite,
/* NclAssignVarToVarFunction write_vv_func */	VarVarWrite,
/* NclReadFunction read_func */		VarRead,
/* NclReadValueFunction read_func */	VarValRead,

/* NclReadAttribute read_att_func*/	VarReadAtt,
/* NclIsA is_att_func*/			VarIsAAtt,
/* NclWriteAttribute write_att_func*/	VarWriteAtt,

/* NclIsAFunc is_dim_func */		VarIsADim,
/* NclReadDimension read_dim_func*/	VarReadDim,
/* NclGetDimInfo read_dim_func*/	VarGetDimInfo,
/* NclWriteDimension write_dim_func*/	VarWriteDim,

/* NclIsAFunc is_coord_func */		VarIsACoord,
/* NclReadCoordinate read_coordinate*/	VarReadCoord,
/* NclWriteCoordinate write_coordinate*/ VarWriteCoord
	}
};

NclObjClass nclVarClass = (NclObjClass)&nclVarClassRec;



static struct _NclDataRec *	VarValRead
#if	NhlNeedProto
(NclVar	self,NclSelectionRecord *sel_ptr,NclScalar *new_missing)
#else
(self,sel_ptr,new_missing)
NclVar	self;
NclSelectionRecord *sel_ptr;
NclScalar *new_missing;
#endif
{
	int i;
	int	dims_ref[NCL_MAX_DIMENSIONS];
	NclSelection *thesel;
	NclMultiDValData thevalue;

	for(i=0; i< NCL_MAX_DIMENSIONS; i++) {
		dims_ref[i] = 0;
	}
	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
		
	
	if(thevalue != NULL) {
		if(sel_ptr != NULL) {
			thesel = sel_ptr->selection;
			if(sel_ptr->n_entries != self->var.n_dims) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript has %d dimensions and %d dimensions were needed to reference variable %s",sel_ptr->n_entries,self->var.n_dims,self->var.thesym->name);
				return(NULL);
			}
			sel_ptr->selected_from_sym = self->var.thesym;
			sel_ptr->selected_from_var = self;
			for(i = 0; i<sel_ptr->n_entries; i++) {
				if(!dims_ref[thesel[i].dim_num]) {
					dims_ref[thesel[i].dim_num] = 1;
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension referenced more than once in subscript");
					return(NULL);
				}
			}
			
			return(_NclReadSubSection((NclData)thevalue,sel_ptr,new_missing));
		} else {
			return((NclData)thevalue);
		}
	} else {
		if(self->var.thesym != NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable %s is still undefined, can not continue",self->var.thesym->name);
			return(NULL);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable <unnamed> is still undefined, can not continue");
			return(NULL);
		}
	}
}
/*
static int VarSetStatus
#if	NhlNeedProto
(NclObj  self, NclStatus requested)
#else
(NclObj  self, NclStatus requested)
NclObj  self;
NclStatus requested;
#endif
{
	NclVar var = (NclVar) self;
	if((var->obj.status == TEMPORARY)||(((var->var.var_type == PARAM)||(var->var.var_type == RETURNVAR))&&(var->obj.ref_count == 0))){
* Should var status set children status? 
                var->obj.status = requested;
                return(1);
        } else {
		return(0);
	}
}
*/

static void	VarPrint
#if	NhlNeedProto
(NclObj theobj,FILE *fp)
#else
(theobj,fp)
NclObj theobj;
FILE *fp;
#endif
{
	NclVar self = (NclVar) theobj;
	char *v_name;
	int i;
	NclMultiDValData thevalue;

	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	

	if(self->var.thesym != NULL) {
		v_name = self->var.thesym->name;
	} else if(self->var.var_quark != -1) {
		v_name = NrmQuarkToString(self->var.var_quark);
	} else {
		v_name = "unnamed";
	}

	nclfprintf(fp,"\n\n");
	switch(self->var.var_type) {
	case NORMAL:
	case HLUOBJ:
		nclfprintf(fp,"Variable: %s\n",v_name);
		break;
	case VARSUBSEL:
		nclfprintf(fp,"Variable: %s (subsection)\n",v_name);
		break;
	case COORD:
		nclfprintf(fp,"Variable: %s (coordinate)\n",v_name);
		break;
	case COORDSUBSEL:
		nclfprintf(fp,"Variable: %s (coordinate subsection)\n",v_name);
		break;
	case PARAM:
		nclfprintf(fp,"Variable: %s (parameter)\n",v_name);
		break;
	case RETURNVAR:
		nclfprintf(fp,"Variable: %s (return)\n",v_name);
		break;
/*
		nclfprintf(fp,"Variable: %s (HLU object)\n",v_name);
		break;
*/
	case FILEVAR:
		nclfprintf(fp,"Variable: %s (file variable)\n",v_name);
		break;
	case FILEVARSUBSEL:
		nclfprintf(fp,"Variable: %s (file variable subsection)\n",v_name);
		break;
	default:
		nclfprintf(fp,"Variable: %s\n","unnamed");
		break;
	}
	if(thevalue == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The value associated with variable (%s) has been freed, can't print it",v_name);
		return;
	}
	nclfprintf(fp,"Type: %s\n",_NclBasicDataTypeToName(thevalue->multidval.data_type));
	nclfprintf(fp,"Total Size: %d bytes\n",thevalue->multidval.totalsize);
	nclfprintf(fp,"            %d values\n",thevalue->multidval.totalelements);
	nclfprintf(fp,"Number of Dimensions: %d\n",self->var.n_dims);
	nclfprintf(fp,"Dimensions and sizes:\t");
	for(i = 0; i< self->var.n_dims; i++) {
		nclfprintf(fp,"[");
		if((self->var.dim_info[i].dim_quark != -1)) {
			nclfprintf(fp,"%s | ",NrmQuarkToString(self->var.dim_info[i].dim_quark));
		}
		nclfprintf(fp,"%d]",self->var.dim_info[i].dim_size);
		if(i !=  self->var.n_dims - 1) {
			nclfprintf(fp," x ");
		}
	}
	nclfprintf(fp,"\n");
	nclfprintf(fp,"Coordinates: \n");
	for(i =0 ; i< self->var.n_dims; i++) {
		if((self->var.coord_vars[i] != -1)&&(_NclGetObj(self->var.coord_vars[i])!=NULL)) {
			nclfprintf(fp,"            ");
			nclfprintf(fp,"%s: [xx..xx]\n",NrmQuarkToString(self->var.dim_info[i].dim_quark));
		}
	}
	_NclPrint(_NclGetObj(self->var.att_id),fp);
	if((self != NULL) && (thevalue != NULL)) {
		_NclPrint((NclObj)thevalue,fp);
	}
	return;
}

struct _NclVarRec *_NclVarCreate
#if	NhlNeedProto
(NclVar inst,
NclObjClass theclass,
NclObjTypes obj_type,
unsigned int obj_type_mask,
struct _NclSymbol *thesym,
struct _NclMultiDValDataRec * value,
NclDimRec *dim_info,
int att_id,
int *coords,
NclVarTypes var_type,
char *var_name,
NclStatus status)
#else
(inst,theclass,obj_type,obj_type_mask,thesym,value,dim_info,att_id,coords,var_type,var_name,status)
	NclVar inst;
	NclObjClass theclass;
	NclObjTypes obj_type;
	unsigned int obj_type_mask;
	struct _NclSymbol *thesym;
	struct _NclMultiDValDataRec *value;
	NclDimRec *dim_info;
	int att_id;
	int *coords;
	NclVarTypes var_type;
	char *var_name;
	NclStatus status;
#endif
{
	NclVar var_out,tmp_var;
	int var_out_free = 0;
	int i;
	char *v_name;
	NclObjClass class_ptr;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp = NULL,tmp_md = NULL;
	NclScalar *tmp_s = NULL;
	int tmp_dim_size =1;


	ret = _NclInitClass(nclVarClass);
	if(ret < NhlWARNING)
		return(NULL);
	if(theclass == NULL) {
		class_ptr = nclVarClass;
	} else {
		class_ptr = theclass;
	}
	if(thesym != NULL) {
		v_name = thesym->name;
	} else if(var_name != NULL) {
		v_name = var_name;
	} else {
		v_name = "unnamed";
	}

	if(inst ==NULL) {	
		var_out = (NclVar)NclMalloc(sizeof(NclVarRec));
		var_out_free = 1;
	} else {
		var_out = inst;
	}
/*
 	switch(var_type) {
	case PARAM:
	case NORMAL:
	case COORD:
	case HLUOBJ:
		status = PERMANENT;
		break;
	case FILEVAR:
	case VARSUBSEL:
	case COORDSUBSEL:
	case FILEVARSUBSEL:
	case RETURNVAR:
	default:
		status = TEMPORARY;	
		break;
	}
*/
	(void)_NclObjCreate((NclObj)var_out,class_ptr,obj_type ,(obj_type_mask | Ncl_Var),status);

	var_out->var.thesym = thesym;
	if(var_name != NULL) {
		var_out->var.var_quark = NrmStringToQuark(var_name);
	} else {
		var_out->var.var_quark = -1;
	}
	
	if((value != NULL)&&(value->obj.obj_type_mask & Ncl_MultiDValData)) {
		if((_NclSetStatus((NclObj)value,PERMANENT))||((var_type == PARAM)&&(value->obj.status != STATIC))||(var_type == RETURNVAR)) {
			var_out->var.thevalue_id = value->obj.id;
		} else {
			tmp = _NclCopyVal(value,NULL);
			if(tmp != NULL) {
				var_out->var.thevalue_id = tmp->obj.id;
				_NclSetStatus(_NclGetObj(var_out->var.thevalue_id),PERMANENT);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Internal error copying value for variable (%s)",v_name);
				if(var_out_free) 
					NclFree(var_out);
			}
		}
		var_out->var.n_dims = value->multidval.n_dims;
		for(i = 0; i< value->multidval.n_dims; i++) {
			var_out->var.dim_info[i].dim_size = value->multidval.dim_sizes[i];
			var_out->var.dim_info[i].dim_num = i;
			if(dim_info != NULL){
				var_out->var.dim_info[i].dim_quark = dim_info[i].dim_quark;
			} else {
				var_out->var.dim_info[i].dim_quark = -1;
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to assign non-value data type to variable (%s)",v_name);
		if(var_out_free)
			NclFree(var_out);
		return(NULL);
	}
	var_out->var.var_type = var_type;

	for(i = 0; i< NCL_MAX_DIMENSIONS ; i++) {
		var_out->var.coord_vars[i] = -1;
	}
	if(value->multidval.missing_value.has_missing) {
		if(att_id == -1){
			att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,NULL);
			tmp_s = (NclScalar*)NclMalloc(sizeof(NclScalar));
			*tmp_s = value->multidval.missing_value.value;
			tmp_md = _NclCreateMultiDVal(
					NULL,
					NULL,
					value->obj.obj_type,
					0,
					(void*)tmp_s,
					NULL,
					1,
					&tmp_dim_size,
					TEMPORARY,
					NULL,
					value->multidval.type);
			_NclAddAtt(att_id,NCL_MISSING_VALUE_ATT,tmp_md,NULL);
		} else if(att_id != -1) {
			tmp_s = (NclScalar*)NclMalloc(sizeof(NclScalar));
			*tmp_s = value->multidval.missing_value.value;
			tmp_md = _NclCreateMultiDVal(
					NULL,
					NULL,
					value->obj.obj_type,
					0,
					(void*)tmp_s,
					NULL,
					1,
					&tmp_dim_size,
					TEMPORARY,
					NULL,
					value->multidval.type);
			_NclAddAtt(att_id,NCL_MISSING_VALUE_ATT,tmp_md,NULL);
		}
	}
	var_out->var.att_id = att_id;
	if(att_id != -1) {
		_NclAddParent(_NclGetObj(att_id),(NclObj)var_out);
	} 
	if(coords != NULL) {
		for(i = 0; i<var_out->var.n_dims; i++) {
			if(coords[i] != -1) {
				tmp_var = (NclVar)_NclGetObj(coords[i]);
				if(!_NclSetStatus((NclObj)tmp_var,PERMANENT)) {
					tmp_var = _NclCopyVar(tmp_var,NULL,NULL);
					_NclSetStatus((NclObj)tmp_var,PERMANENT);
					var_out->var.coord_vars[i] = tmp_var->obj.id;
					_NclAddParent((NclObj)tmp_var,(NclObj)var_out);
				} else {
					_NclAddParent((NclObj)tmp_var,(NclObj)var_out);
					var_out->var.coord_vars[i] = coords[i];
				}
			} else {
				var_out->var.coord_vars[i] = coords[i];
			}
		}
	}
	_NclAddParent(_NclGetObj(var_out->var.thevalue_id),(NclObj)var_out);
	return(var_out);
}

static void VarDestroy
#if	NhlNeedProto
(struct _NclObjRec*	self)
#else
(self)
struct _NclObjRec*	self;
#endif
{
	int i;
	NclVar self_var = (NclVar)self;
	NclObj value = (NclObj)_NclGetObj(self_var->var.thevalue_id);


	_NclUnRegisterObj((NclObj)self_var);
	for(i = 0; i< self_var->var.n_dims; i++ ) {
		if(self_var->var.coord_vars[i] != -1) {
			_NclDelParent(_NclGetObj(self_var->var.coord_vars[i]),self);
		}
	}
	if(self_var->var.att_id != -1)
	_NclDelParent(_NclGetObj(self_var->var.att_id),(NclObj)self_var);

	
	if(value != NULL) {
		_NclDelParent(value,self);
	}
	NclFree(self_var);
	return;
}


static struct _NclDataRec * VarCoerce
#if	NhlNeedProto
(struct _NclVarRec* self, NclObjTypes coerce_to_obj, NclScalar *new_missing)
#else
(self,coerce_to_obj,new_missing)
struct _NclVarRec* self;
NclObjTypes coerce_to_obj;
NclScalar *new_missing;
#endif
{
	NclMultiDValData val_md = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);

	if(val_md != NULL) {
		return(
		(*((NclDataClass)(val_md->obj.class_ptr))->data_class.coerce)(
			(NclData)val_md,coerce_to_obj,new_missing));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to coerce NULL value");
		return(NULL);
	}
}


static NclObjTypes VarValRep
#if	NhlNeedProto
(struct _NclVarRec* self)
#else
(self)
	struct _NclVarRec* self;
#endif
{
	NclMultiDValData thevalue  = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(thevalue != NULL) {
		return((NclObjTypes)(thevalue->multidval.type->type_class.type ));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"VarValRep: NULL variable passed");
		return(Ncl_Obj);
	}
}

static NhlErrorTypes VarWriteAtt
#if	NhlNeedProto
(struct _NclVarRec *self, char *attname, struct _NclMultiDValDataRec *value, struct _NclSelectionRecord *sel_ptr)
#else
(self, attname, value, sel_ptr)
struct _NclVarRec *self;
char *attname;
struct _NclMultiDValDataRec *value;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int att_quark;
	NclMultiDValData value_md,thevalue;
	NclScalar tmp_mis;
	char *v_name;
/*
* Preconditions:
*
*	value is MultiDValData type of object
*
*/
	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(self->var.var_quark != -1) {
		v_name = NrmQuarkToString(self->var.var_quark);
	} else if(self->var.thesym != NULL) {
		v_name = self->var.thesym->name;
	} else {
		v_name = "unnamed";
	}
	att_quark = NrmStringToQuark(attname);	
	if(att_quark == NrmStringToQuark(NCL_MISSING_VALUE_ATT)) {
		if(value->multidval.kind == SCALAR) {
/*
* This function is convenient for providing a type independent way of
* copying the value into the missing_value structure.
*/
			if(_NclScalarCoerce((void*)(value->multidval.val),value->multidval.data_type,(void*)&tmp_mis,thevalue->multidval.data_type)) {

/*
* Need to do this cast in addition to above because value still needs to be
* inserted into the att_list
*/
				value_md =  _NclCoerceData(value,thevalue->multidval.type->type_class.type ,NULL);
				if(value_md == NULL) {

/*
* ALthough if _NclScalarCoerce succeds you shouldn't get this condition
* but putting error message here anyways
*/

					NhlPError(NhlFATAL,NhlEUNKNOWN,"Type Mismatch: The type of missing value could not be converted to type of variable (%s)",v_name);
					return(NhlFATAL);
				}
				_NclResetMissingValue(thevalue,&tmp_mis);	
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Type Mismatch: The type of missing value could not be converted to type of variable (%s)",v_name);
				return(NhlFATAL);
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Missing values must be scalar");
			return(NhlFATAL);
		}
	} else {
		value_md = value;
	}
	if(self->var.att_id == -1) {
		self->var.att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)self);
	}
	return(_NclAddAtt(self->var.att_id,attname,value_md,sel_ptr));
}

static struct _NclMultiDValDataRec *VarReadAtt
#if	NhlNeedProto
(struct _NclVarRec *self, char *attname, struct _NclSelectionRecord *sel_ptr)
#else
(self, attname, sel_ptr)
struct _NclVarRec *self;
char *attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	return(_NclGetAtt(self->var.att_id,attname,sel_ptr));
}

static int VarIsAAtt
#if	NhlNeedProto
(struct _NclVarRec * self, char* attname)
#else
(self, attname)
struct _NclVarRec *self;
char* attname;
#endif
{
	return(_NclIsAtt(self->var.att_id,attname));
	
}
static int VarIsADim
#if	NhlNeedProto
(struct _NclVarRec *self, char* dimname)
#else
(self, dimname)
struct _NclVarRec *self;
char* dimname;
#endif
{
	int i;
	int dim_quark = NrmStringToQuark(dimname);

	for(i = 0; i< self->var.n_dims; i++) {
		if(self->var.dim_info[i].dim_quark == dim_quark) {
			return(i);
		}
	}
	return(-1);
}

static struct _NclMultiDValDataRec *VarReadDim
#if	NhlNeedProto
(struct  _NclVarRec* self, char* dim_name, long dim_num)
#else
(self, dim_name, dim_num)
struct  _NclVarRec* self;
char* dim_name;
long dim_num;
#endif
{
	int index;
	int dim_size = 1;
	string* nameptr;
	int *numptr;
	char *v_name= NULL;
	if(self->var.var_quark != -1) {
		v_name = NrmQuarkToString(self->var.var_quark);
	} else if(self->var.thesym != NULL) {
		v_name = self->var.thesym->name;
	} else {
		v_name = "unnamed";
	}
	if(dim_name != NULL) {
/*
* When dim_name provided the output is dim_num
*/
		index = VarIsADim(self,dim_name);
		if((index < self->var.n_dims)&&(dim_num >= 0)) {
			numptr = (int*)malloc((unsigned)sizeof(int));
			*numptr = index;
			return(
				_NclCreateMultiDVal(
              			  NULL,NULL,Ncl_MultiDValData,0,(void*)numptr,
                		  NULL,
                		  1,
                		  &(dim_size),
                		  TEMPORARY,NULL,(NclTypeClass)nclTypeintClass)
			);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension name (%s) is not defined for variable (%s)",dim_name,v_name);
			return(NULL);
		}
	} else if((dim_num < self->var.n_dims)&&(dim_num >= 0)) {
/*
* When dim_num provided the output is dim_name
*/
		if(self->var.dim_info[dim_num].dim_quark != -1) {
			nameptr = (string*)NclMalloc(sizeof(string));

			*nameptr = self->var.dim_info[dim_num].dim_quark;
	
			return(
				_NclCreateMultiDVal(
              			  NULL,NULL,Ncl_MultiDValData,0,(void*)nameptr,
               	 		  NULL,
               	 		  1,
               	 		  &(dim_size),
               		 	  TEMPORARY,NULL,(NclTypeClass)nclTypestringClass)
			);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension number (%d) does not have a defined name",dim_num);
			return(NULL);
		}
	} else if(dim_num >= self->var.n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) only has (%d) dimensions , cannot access dimension (%d)",v_name,self->var.n_dims,dim_num);
		return(NULL);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to access  illegal dimension number of name");
		return(NULL); 
	}
}


static struct _NclDimRec *VarGetDimInfo
#if	NhlNeedProto
(struct _NclVarRec* self, char *dim_name, long dim_num)
#else
(self,dim_name,dim_num)
struct _NclVarRec* self;
char *dim_name;
long dim_num;
#endif
{
	int index;

	if(dim_name != NULL) {
		index = VarIsADim(self,dim_name);
		if((index < self->var.n_dims)&&(index >=0)) {
			return(&(self->var.dim_info[index]));
		}
	} else if((dim_num < self->var.n_dims)&&( dim_num >= 0)) {
			return(&(self->var.dim_info[dim_num]));
	} else {
		return(NULL);	
	}
}

static NhlErrorTypes VarWriteDim
#if	NhlNeedProto
(struct  _NclVarRec *self, long dim_num, char *dim_name)
#else
(self,dim_num,dim_name)
struct _NclVarRec *self;
long dim_num;
char *dim_name;
#endif
{
	int dim_quark;
	char *v_name;
	NclVar tmp_var;

	if(self->var.thesym != NULL) {
		v_name = self->var.thesym->name;
	} else if(self->var.var_quark!= -1) {
		v_name = NrmQuarkToString(self->var.var_quark);
	} else {
		v_name = "unnamed";
	}

	if((dim_name != NULL)&&((dim_num < self->var.n_dims)&&(dim_num>=0))) {
		dim_quark = NrmStringToQuark(dim_name);
		if(self->var.dim_info[dim_num].dim_quark == -1) {
			self->var.dim_info[dim_num].dim_quark = dim_quark;
			return(NhlNOERROR);
		} else {
			if(self->var.dim_info[dim_num].dim_quark != dim_quark) {
				self->var.dim_info[dim_num].dim_quark = dim_quark;
				if(self->var.coord_vars[dim_num] != -1) {
					tmp_var = (NclVar)_NclGetObj(self->var.coord_vars[dim_num]);
					if(tmp_var != NULL) {
						tmp_var->var.var_quark = dim_quark;
					}
				}
					
			} 
			return(NhlNOERROR);
		}
	}  else if(dim_num >= self->var.n_dims){
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) has (%d) dimensions can not write to dimension (%ld)",v_name,self->var.n_dims,dim_num);
		return(NhlFATAL);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Either attempt to write NULL dimension name or illegal dimension number used");
		return(NhlFATAL);
	}
}


static int VarIsACoord
#if	NhlNeedProto
(struct _NclVarRec *self, char* coordname)
#else
(self, coordname)
struct _NclVarRec *self;
char* coordname;
#endif
{
	int index = -1;

	index = VarIsADim(self,coordname);
	if(index < 0) {
		return(0);
	} else {
		if(self->var.coord_vars[index] != -1) {
			return(1);
		} else {
			return(0);
		}
	}
}




static NhlErrorTypes VarWrite
#if	NhlNeedProto
(NclVar self,struct _NclMultiDValDataRec *value, NclSelectionRecord *sel_ptr)
#else
(self,value,sel_ptr)
NclVar self;
struct _NclMultiDValDataRec *value;
NclSelectionRecord *sel_ptr;
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData thevalue;
	NclMultiDValData attvalue;
	NclMultiDValData tmp_md;
	NclMultiDValData tmp_md1;
	int i;
	NclScalar *missing_ptr;
	int miss_dim_sizes[NCL_MAX_DIMENSIONS];
	NclVar self_var = (NclVar)self;

/*
* Preconditions value is a NclMultiDValData
* self is an NclVar
* if sel_ptr is set n_entries == n_dims of thevalue field
*/
	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);

	if(sel_ptr == NULL) {
		if(value->multidval.n_dims == thevalue->multidval.n_dims){
			for(i = 0; i< thevalue->multidval.n_dims;i++) {
				if(value->multidval.dim_sizes[i] != 
					thevalue->multidval.dim_sizes[i]) {
					
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension sizes of left hand side and right hand side of assignment do not match");
					return(NhlFATAL);
				}
			}
			if(thevalue->multidval.missing_value.has_missing ) {
/*
* _NclCoerceData is used regardless of the type since the missing value must 
* also be set Therefore is the types are different two things get done at once.
* If the types are the same then only the missing value is set. The Coerce 
* functions are be smart enough to compare missing values to see if they are 
* equal if they are and the types are equal then value is returned unchanged. 
* if value is TEMPORARY and the types are equal then _NclResetMissingValue
* is used by the coerce functions.
*/
					tmp_md = _NclCoerceData(value,
						thevalue->multidval.type->type_class.type,
						&thevalue->multidval.missing_value.value); 
					if(tmp_md==NULL) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Assignment type mismatch, right hand side can't be coerced to type of left hand side");
						return(NhlFATAL);
					}
			} else if(value->multidval.missing_value.has_missing) {
/*
* Only input value has missing values. In this situation a missing value 
* attribute must be created and inserted into the attlist.
*/
				if(value->multidval.type->type_class.type != thevalue->multidval.type->type_class.type) {
					tmp_md = _NclCoerceData(value,
						thevalue->multidval.type->type_class.type,
						NULL);
					if(tmp_md == NULL) {	
						return(NhlFATAL);
					}
				} else {
					tmp_md = value;
				}
				
/*
* Need to create permanent storage hence the malloc
*/
				missing_ptr = (NclScalar*)NclMalloc((unsigned)	
						sizeof(NclScalar));
				*missing_ptr = tmp_md->multidval.missing_value.value;
				miss_dim_sizes[0] = 1;
				
				attvalue = _NclCreateMultiDVal(
					NULL,
					thevalue->obj.class_ptr,
					thevalue->obj.obj_type,
					0,
					(void*)missing_ptr,
					NULL,
					1,
					miss_dim_sizes,
					PERMANENT,
					NULL,
					thevalue->multidval.type);
				if(self_var->var.att_id == -1) {
					self_var->var.att_id =
						_NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)self_var);
				}
				_NclAddAtt(self_var->var.att_id,NCL_MISSING_VALUE_ATT,attvalue,NULL);
			} else  {
/*
* Case where neither have missing values
*/
				if(value->multidval.type->type_class.type != thevalue->multidval.type->type_class.type) {

					tmp_md = _NclCoerceData(value,
						thevalue->multidval.type->type_class.type,
						NULL);
					if(tmp_md == NULL) {	
						return(NhlFATAL);
					}
				} else {
					tmp_md = value;
				}
			} 
/*
* By changing this field to permanent the calling env will not destroy it
*/
			if(!_NclSetStatus((NclObj)tmp_md,PERMANENT) ) {
/*
* this is ok since value is destroyed by calling env when value is STATIC.
* Note that if tmp_md came from _NclCoerceData it will not hit this branch
* so no memory is lost. The only way tmp_md can be PERMANENT or STATIC is
* if it came in from the calling env. Therefore no pointer is actually lost. 
*/
				tmp_md1 = _NclCopyVal(tmp_md,NULL);
				_NclSetStatus((NclObj)tmp_md1,PERMANENT);
				tmp_md = tmp_md1;
			}
/*
* Since whole sale replacement of thevalue occurs, it is the responsiblity
* of this function to free the old storage
*
* BOGUS CODE ALERT----> switching to id's instead of pointers makes this a
* no-no
*/

			_NclDelParent((NclObj)thevalue,(NclObj)self);
			_NclAddParent((NclObj)tmp_md,(NclObj)self);
			self->var.thevalue_id = tmp_md->obj.id;
			return(NhlNOERROR);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions on right hand side do not match number of dimension in left hand side");
			return(NhlFATAL);
		}

	} else {
		if(sel_ptr->n_entries == thevalue->multidval.n_dims) {
			if(value->multidval.type->type_class.type != thevalue->multidval.type->type_class.type) {
/*
* Don't care about the missing values here because the _subsection function 
* handles differences in missing values
*/
				tmp_md = _NclCoerceData(value,
					thevalue->multidval.type->type_class.type,
					NULL);
				if(tmp_md==NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Assignment type mismatch, right hand side can't be coerced to type of left hand side");
					return(NhlFATAL);
				}
			} else {
/*
* Handle missing value diffs here
*/
				tmp_md = value;
			}
/*
* Number of subscripts is already guarenteed to be the same in Execute
* _subsection function handle the differences in missing values so no need 
* to have Coerce convert it before this call
*/
			ret = _NclWriteSubSection((NclData)thevalue,sel_ptr,(NclData)tmp_md);
			if((tmp_md != value)&&(tmp_md->obj.status != PERMANENT)) {
				_NclDestroyObj((NclObj)tmp_md);
			}

			return(ret);
		} else if( value->multidval.kind == SCALAR) {
			if(value->multidval.type->type_class.type != thevalue->multidval.type->type_class.type) {
/*
* Don't care about the missing values here because the _subsection function 
* handles differences in missing values
*/
				tmp_md = _NclCoerceData(value,
					thevalue->multidval.type->type_class.type,
					NULL);
				if(tmp_md==NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Assignment type mismatch, right hand side can't be coerced to type of left hand side");
					return(NhlFATAL);
				}
			} else {
/*
* Handle missing value diffs here
*/
				tmp_md = value;
			}
			_NclWriteSubSection((NclData)thevalue, sel_ptr, (NclData)tmp_md);
			return(ret);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions on right hand side do not match number of dimension in left hand side");
			return(NhlFATAL);
		} 
	}
}
static struct _NclVarRec *VarReadCoord
#if	NhlNeedProto
(struct  _NclVarRec* self, char *coord_name, struct _NclSelectionRecord *sel_ptr)
#else
(self,coord_name,sel_ptr)
struct  _NclVarRec* self;
char *coord_name;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	int index;
	char *v_name;
	NclVar tmp;

	if(self->var.thesym != NULL) {
		v_name = self->var.thesym->name;
	} else if(self->var.var_quark!= -1) {
		v_name = NrmQuarkToString(self->var.var_quark);
	} else {
		v_name = "unnamed";
	}

	index = VarIsADim(self,coord_name);
	if((index>=0)&&(index < self->var.n_dims))  {
		if(self->var.coord_vars[index] != -1) {
			tmp = _NclVarRead((NclVar)_NclGetObj(self->var.coord_vars[index]),sel_ptr);
			return(tmp);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"No coordinate variable exists for dimension (%s) in variable (%s)",coord_name,v_name);
			return(NULL);
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension of variable (%s), can't access coordinate variable for named dimension",coord_name,v_name);
		return(NULL);
	}
}
static NhlErrorTypes VarWriteCoord
#if	NhlNeedProto
(struct  _NclVarRec* self, struct  _NclMultiDValDataRec* value, char* coord_name, struct  _NclSelectionRecord *sel_ptr)
#else
(self, value, coord_name, sel_ptr)
struct  _NclVarRec* self;
struct  _NclMultiDValDataRec* value;
char* coord_name;
struct  _NclSelectionRecord *sel_ptr;
#endif
{
	int index;
	NclDimRec tmp;
	NclObj tmp_obj;

	
	index = VarIsADim(self,coord_name);
	if((index>=0)&&(index < self->var.n_dims))  {
		if(value->multidval.n_dims != 1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables must have one dimension only");
			return(NhlFATAL);
		}
		if((self->var.coord_vars[index] != -1)&&(_NclGetObj(self->var.coord_vars[index]) != NULL)) {
			return(_NclAssignToVar((NclVar)_NclGetObj(self->var.coord_vars[index]),value,sel_ptr));
		} else {
			tmp.dim_quark = NrmStringToQuark(coord_name);
			tmp_obj = (NclObj)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,value,&tmp,-1,NULL,COORD,coord_name,PERMANENT);
			_NclAddParent(tmp_obj,(NclObj)self);
			self->var.coord_vars[index] = tmp_obj->obj.id;
			if(self->var.coord_vars[index] == -1) {
				return(NhlFATAL);
			} else {
				return(NhlNOERROR);
			}
		}
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a named dimension. Can not assign coordinate variable",coord_name);
		return(NhlFATAL);
	}
}
static struct _NclVarRec *	VarRead
#if	NhlNeedProto
(NclVar	self,NclSelectionRecord *sel_ptr)
#else
(self,sel_ptr,new_missing)
NclVar	self;
NclSelectionRecord *sel_ptr;
#endif
{
	NclMultiDValData val;
	int i,j;
	NclSelectionRecord tmp_sel;
	NclVar coord_var;
	int coords[NCL_MAX_DIMENSIONS];
	NclMultiDValData thevalue;
	NclObj tmp_obj;
	NclAtt tmp_att = NULL;
	NclDimRec dim_info[NCL_MAX_DIMENSIONS];
	int single = 0;

	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);

	
	
	if(thevalue != NULL) {
		if(sel_ptr != NULL) {
/*
* First read value. Then get dimension info. Then 
* get attributes followed by reading coordinate
* vars
*/
			val = (NclMultiDValData)_NclReadSubSection((NclData)thevalue,sel_ptr,NULL);
			if(val == NULL) {
				return(NULL);
			}
			tmp_sel.n_entries = 1;
			tmp_sel.selected_from_sym = NULL;
			tmp_sel.selected_from_var = NULL;
			tmp_sel.selection[0].dim_num = 0;
			j = 0;
			tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(self->var.att_id),NULL);
			for(i = 0; i< sel_ptr->n_entries; i++) {
				if((self->var.coord_vars[sel_ptr->selection[i].dim_num] != -1)||(_NclGetObj(self->var.coord_vars[sel_ptr->selection[i].dim_num]) != NULL)) {
					if(sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR) {
						tmp_sel.selection[0].sel_type = sel_ptr->selection[i].sel_type;
						tmp_sel.selection[0].u.vec= sel_ptr->selection[i].u.vec;
						tmp_sel.selection[0].u.vec.ind = NclMalloc((unsigned)sizeof(long)*sel_ptr->selection[i].u.vec.n_ind);
						memcpy((void*)tmp_sel.selection[0].u.vec.ind,(void*)sel_ptr->selection[i].u.vec.ind,sizeof(long)*sel_ptr->selection[i].u.vec.n_ind);
						if(sel_ptr->selection[i].u.vec.n_ind == 1) {
							single = 1;
						}
					} else {
						tmp_sel.selection[0].sel_type = sel_ptr->selection[i].sel_type;
						tmp_sel.selection[0].u.sub = sel_ptr->selection[i].u.sub;
						if(sel_ptr->selection[i].u.sub.start == sel_ptr->selection[i].u.sub.finish) {
							single = 1;
						}
					}
/*
* Since tmp_sel is not null the _NclVarRead function will create a new
* coordvar so the situation of coord_var equalling the one pointed to in
* the coordinate array is not possible
*/
					coord_var = _NclVarRead((NclVar)_NclGetObj(self->var.coord_vars[sel_ptr->selection[i].dim_num]),&tmp_sel);
					coords[j] = coord_var->obj.id; 
				} else {
					coords[j] = -1;
				}
				if(single){ 
					if(coords[j] != -1) {
						if(tmp_att == NULL) {
							tmp_att = (NclAtt)_NclGetObj(_NclAttCreate(NULL,NULL,Ncl_Att,0,NULL));
						}
						_NclAddAtt(tmp_att->obj.id,NrmQuarkToString(self->var.dim_info[sel_ptr->selection[i].dim_num].dim_quark),_NclVarValueRead(coord_var,NULL,NULL),NULL);
					}
					single = 0;
				} else {
					dim_info[j].dim_num = 0;
					dim_info[j].dim_quark = self->var.dim_info[sel_ptr->selection[i].dim_num].dim_quark;
					dim_info[j].dim_size = val->multidval.dim_sizes[j];
					j++;
				}
			}
			if(j == 0) {
				dim_info[j].dim_num = 0;
				dim_info[j].dim_quark = -1;
				dim_info[j].dim_size = val->multidval.dim_sizes[j];
				coords[j] = -1;
			}
			tmp_obj = (NclObj)_NclVarNclCreate(NULL,NULL,self->obj.obj_type,self->obj.obj_type_mask,self->var.thesym,val,dim_info,((tmp_att == NULL) ? -1:tmp_att->obj.id),coords,(self->obj.obj_type_mask & Ncl_CoordVar ?COORDSUBSEL:VARSUBSEL),NrmQuarkToString(self->var.var_quark),TEMPORARY);
			for(i = 0; i< sel_ptr->n_entries;i++) {
				if(((NclVar)tmp_obj)->var.coord_vars[i] != -1){
					_NclAddParent(
						_NclGetObj(((NclVar)tmp_obj)->var.coord_vars[i]),
						tmp_obj );
				}
			}
			return((NclVar)tmp_obj);
		} else {
			return(self);
		}
	} else {
		if(self->var.thesym != NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable %s is still undefined, can not continue",self->var.thesym->name);
			return(NULL);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable <unnamed> is still undefined, can not continue");
			return(NULL);
		}
	}
}
static NhlErrorTypes VarDelParent
#if	NhlNeedProto
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
                NhlPError(NhlFATAL,NhlEUNKNOWN,"MultiDValDelParent: Attempt to delete parent from empty list");
                return(NhlFATAL);
        }

        tmp = theobj->obj.parents;
        while((tmp!=NULL)&&(tmp->pptr->obj.id == parent->obj.id)) {
                theobj->obj.parents = theobj->obj.parents->next;
                NclFree(tmp);
                tmp = theobj->obj.parents;
		theobj->obj.ref_count--;
                found = 1;
        }
        if((tmp == NULL)&&(found)) {
		_NclDestroyObj(theobj);
                return(NhlNOERROR);
        }
        while(tmp->next != NULL) {
                if(tmp->next->pptr->obj.id == parent->obj.id) {
                        found = 1;
			theobj->obj.ref_count--;
                        tmp1 = tmp->next;
                        tmp->next = tmp->next->next;
                        NclFree(tmp1);
                } else {
                        tmp = tmp->next;
                }
        }
        if(found) {
		if(theobj->obj.ref_count <=0) 
			_NclDestroyObj(theobj);
                return(NhlNOERROR);
        } else {
                return(NhlWARNING);
        }
}

static NhlErrorTypes VarAddParent
#if	NhlNeedProto
(struct _NclObjRec *theobj, struct _NclObjRec *parent)
#else
(theobj, parent)
struct _NclObjRec *theobj;
struct _NclObjRec *parent;
#endif
{
	NclRefList *tmp = NULL;
	if(parent->obj.obj_type_mask & ( Ncl_Var)) {

		theobj->obj.ref_count++;
	        tmp = theobj->obj.parents;
       		theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
       		theobj->obj.parents->next = tmp;
        	theobj->obj.parents->pptr = parent;
		
		return(NhlNOERROR);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "Only files and variables can be parents of variables\n");
		return(NhlFATAL);
	}
}

static struct _NclVarRec * VarCopy
#if	NhlNeedProto
(struct _NclVarRec *thevar, NclScalar *new_missing, struct _NclVarRec *storage)
#else
(thevar,new_missing, storage)
struct _NclVarRec *thevar;
NclScalar *new_missing;
struct _NclVarRec *storage;
#endif
{
	struct _NclVarRec *tmp_var = NULL;
	NclObj tmp_obj = NULL;
	int i;
/*
	if(thevar == NULL)
		return(NULL);

	if(storage != NULL) {
		tmp_var = storage;
	} else {
		tmp_var = (NclVar)NclMalloc(thevar->obj.class_ptr->obj_class.obj_size);
	}
	if(tmp_var == NULL) {
		return(NULL);
	}
	(void)_NclObjCreate((NclObj)tmp_var,thevar->obj.class_ptr,thevar->obj.obj_type ,thevar->obj.obj_type_mask,TEMPORARY);
	
	tmp_var->var = thevar->var;


	tmp_obj = (NclObj)_NclCopyVal((NclMultiDValData)_NclGetObj(thevar->var.thevalue_id),new_missing);
	_NclSetStatus((NclObj)tmp_obj,PERMANENT);
	_NclAddParent(tmp_obj,(NclObj)tmp_var);
	if(tmp_obj != NULL) {
		tmp_var->var.thevalue_id = tmp_obj->obj.id;
	} else {
		tmp_var->var.thevalue_id = -1;
	}
	tmp_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(thevar->var.att_id),NULL);
	if(tmp_obj != NULL) {
		_NclAddParent(tmp_obj,(NclObj)tmp_var);
		tmp_var->var.att_id = tmp_obj->obj.id;
	} else {
		tmp_var->var.att_id = -1;
	}
	for(i = 0; i < tmp_var->var.n_dims; i++) {
		if((tmp_var->var.coord_vars[i] == -1)
			||(_NclGetObj(tmp_var->var.coord_vars[i]) == NULL)) {
			tmp_var->var.coord_vars[i] = -1;
		} else {
			tmp_obj = (NclObj)_NclCopyVar((NclVar)_NclGetObj(tmp_var->var.coord_vars[i]),NULL,NULL);
			if(tmp_obj == NULL) {
				tmp_var->var.coord_vars[i] = -1;
			} else {
				_NclAddParent(tmp_obj,(NclObj)tmp_var);
				tmp_var->var.coord_vars[i] = tmp_obj->obj.id;
			}
		}
	}
*/
	tmp_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(thevar->var.att_id),NULL);
	tmp_var = _NclVarNclCreate(NULL,thevar->obj.class_ptr,thevar->obj.obj_type,thevar->obj.obj_type_mask,NULL,(NclMultiDValData)_NclGetObj(thevar->var.thevalue_id),thevar->var.dim_info,((tmp_obj != NULL)?tmp_obj->obj.id:-1),thevar->var.coord_vars,thevar->var.var_type,NULL,TEMPORARY);
	

	return(tmp_var);
}

static NhlErrorTypes VarVarWrite
#if	NhlNeedProto
(struct _NclVarRec * lhs, struct _NclSelectionRecord * lhs_sel_ptr, struct _NclVarRec * rhs, struct _NclSelectionRecord * rhs_sel_ptr)
#else
(lhs, lhs_sel_ptr, rhs, rhs_sel_ptr)
struct _NclVarRec * lhs;
struct _NclSelectionRecord * lhs_sel_ptr;
struct _NclVarRec * rhs;
struct _NclSelectionRecord * rhs_sel_ptr;
#endif
{
	NclMultiDValData lhs_md = NULL;
	NclMultiDValData rhs_md = NULL;
	NclObjTypes lhs_type,rhs_type;
	NhlErrorTypes ret = NhlNOERROR;


	rhs_type = _NclGetVarRepValue(rhs);
	lhs_type = _NclGetVarRepValue(lhs);
	
	lhs_md = (NclMultiDValData)_NclGetObj(lhs->var.thevalue_id);
	if((rhs_type != lhs_type)||((rhs_type == lhs_type)&&(rhs_type == Ncl_Obj))){
		if(rhs_type == Ncl_Obj) {
			return(NhlFATAL);
		}
		rhs_md = _NclVarValueRead(rhs,rhs_sel_ptr,NULL);

		ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
		if(rhs_md->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)rhs_md);
		}
		if(ret < NhlWARNING) {
			return(ret);
		}
		return(ret);
			
	} else {
	
		rhs_md = (NclMultiDValData)_NclGetObj(rhs->var.thevalue_id);
	}

	if((lhs_md == NULL)||(rhs_md == NULL) ) {
		return(NhlFATAL);
	}

	if((lhs->obj.id == rhs->obj.id)||(lhs->var.thevalue_id == rhs->var.thevalue_id)) {
/*
* Situation where left and right hand side share same value or are identical. A situation where
* they could have different id's but share the same value is when a global and a function parameter
* reference the same data value.
*/
		rhs_md = _NclVarValueRead(rhs,rhs_sel_ptr,NULL);
		if(rhs_md == NULL) {
			return(NhlFATAL);
		}

	
		ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);

		if(rhs_md->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)rhs_md);
		}
		if(ret < NhlWARNING) {
			return(ret);
		}
		return(ret);
	} else if((lhs_sel_ptr != NULL)&&(rhs_sel_ptr != NULL)) {
		_NclReadThenWriteSubSection((NclData)lhs_md, lhs_sel_ptr, (NclData)rhs_md, rhs_sel_ptr);

	} else if(rhs_sel_ptr == NULL) {
		ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
		if(ret < NhlWARNING){
			return(ret);
		} 
		return(ret);
	} else {
/*
* lhs_sel_ptr == NULL &&  rhs_sel_ptr != NULL
*/
		rhs_md = _NclVarValueRead(rhs,rhs_sel_ptr,NULL);

		ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
		if(ret < NhlWARNING) {
			return(ret);
		}
		return(ret);
	}

	
	return(NhlNOERROR);
}

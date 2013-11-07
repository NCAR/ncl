
/*
 *      $Id: NclVar.c,v 1.81 2010-04-14 21:29:48 huangwei Exp $
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
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "NclAtt.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "NclList.h"
#include "DataSupport.h"
#include "TypeSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"
#include "NclCallBacksI.h"
#include <math.h>
#ifdef NIO_LIB_ONLY
#define UNDEF 255
#else
#include "parser.h"
#include "ListSupport.h"
#endif

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

static NhlErrorTypes VarDeleteCoord(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
char    *               /* coord_name */
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
static NhlErrorTypes VarReplaceCoord(
struct  _NclVarRec      * /*self*/,
struct  _NclMultiDValDataRec    * /*value*/,
char    *               /* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
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
struct _NclSymbol * /*new_name*/, 
struct _NclVarRec * /*storage*/
#endif
);

static NhlErrorTypes InitializeVarClass(
#if NhlNeedProto
void
#endif
);

static void * VarObtainCallData(
#if NhlNeedProto
NclObj /*obj*/,
unsigned int /* type */
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
		(NclInitClassFunction)InitializeVarClass,
		(NclAddParentFunction)VarAddParent,
                (NclDelParentFunction)VarDelParent,
/* NclPrintSummaryFunction print_summary */ NULL,
/* NclPrintFunction print */	VarPrint,
/* NclCallBackList* create_callback*/   NULL,
/* NclCallBackList* delete_callback*/   NULL,
/* NclCallBackList* modify_callback*/   NULL,
/* NclObtainCall obtain_calldata*/   VarObtainCallData
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
/* NclWriteCoordinate write_coordinate*/ VarWriteCoord,
/* NclWriteCoordinate write_coordinate*/ VarDeleteCoord
	}
};

NclObjClass nclVarClass = (NclObjClass)&nclVarClassRec;

void _NclVarMissingNotify
#if     NhlNeedProto
(NhlArgVal cbdata, NhlArgVal udata)
#else
(cbdata,udata)
NhlArgVal cbdata;
NhlArgVal udata;
#endif
{
	NclVar self = NULL;
	NclMultiDValData theval = NULL;

	self = (NclVar)_NclGetObj(udata.intval);
	if(self != NULL) {
		theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
		if(theval != NULL) {
			_NclResetMissingValue(theval,(NclScalar*)cbdata.ptrval);
		}
	}
}

static void *VarObtainCallData
#if NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
	NclVarClassInfo  *tmp = NclMalloc(sizeof(NclVarClassInfo));
	NclVar var = (NclVar)obj;
	int i;
	
	tmp->obj.obj_id = obj->obj.id;
	tmp->obj.obj_type = NCLVar;
	if((var->var.thesym != NULL)&&(var->var.thesym->level != 1)) {
		if(var->var.var_type == NORMAL) {
			tmp->var.var_type = (NclApiVarTypes)NclAPIFUNCNORMAL;
		} else {
			tmp->var.var_type = (NclApiVarTypes)var->var.var_type;
		} 
	} else {
		tmp->var.var_type = (NclApiVarTypes)var->var.var_type;
	}
	tmp->var.var_quark = var->var.var_quark;
	tmp->var.n_dims = var->var.n_dims;
	for ( i = 0; i < var->var.n_dims; i++) {
		tmp->var.dim_sizes[i] = var->var.dim_info[i].dim_size;
		tmp->var.dim_quarks[i] = var->var.dim_info[i].dim_quark;
	}
	return((void*)tmp);
}
static NhlErrorTypes InitializeVarClass
#if NhlNeedProto
(void)
#else
()
#endif
{
	_NclRegisterClassPointer(
		Ncl_Var,
		(NclObjClass)&nclVarClassRec
	);
	return(NhlNOERROR);
}


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

	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
		
	
	if(thevalue != NULL) {
		if(sel_ptr != NULL) {
			for(i=0; i< sel_ptr->n_entries; i++) {
				dims_ref[i] = 0;
			}
			thesel = sel_ptr->selection;
			if(sel_ptr->n_entries != self->var.n_dims) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Subscript has %d dimensions and %d dimensions were needed to reference variable %s",sel_ptr->n_entries,self->var.n_dims,self->var.thesym->name);
				return(NULL);
			}
			sel_ptr->selected_from_sym = self->var.thesym;
			sel_ptr->selected_from_var = self;
			for(i = sel_ptr->n_entries-1; i >= 0; i--) {
					dims_ref[thesel[i].dim_num]++;
			}
			for(i = 0; i<sel_ptr->n_entries; i++) {
				if(dims_ref[i] != 1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension referenced more than once in subscript or not at all");
					return(NULL);
				}
			}
			thevalue = (NclMultiDValData)_NclReadSubSection((NclData)thevalue,sel_ptr,new_missing);
			if(thevalue == NULL) {	
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred reading %s",((self->var.thesym != NULL)?self->var.thesym->name:"unknown"));
			}
			return((NclData)thevalue);
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

NhlErrorTypes VarPrintVarSummary
#if	NhlNeedProto
(NclObj theobj,FILE *fp)
#else
(theobj,fp)
NclObj theobj;
FILE *fp;
#endif
{
	NclVar self = (NclVar) theobj;
	NclVar cvar= NULL;
	NclMultiDValData tmp_md = NULL;
	char *v_name;
	int i;
	NclMultiDValData thevalue = NULL;
	int ret;
	NhlErrorTypes ret0 = NhlNOERROR;

	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	

	if(self->var.thesym != NULL) {
		v_name = self->var.thesym->name;
	} else if(self->var.var_quark != -1) {
		v_name = NrmQuarkToString(self->var.var_quark);
	} else {
		v_name = "unnamed";
	}

	ret = nclfprintf(fp,"\n\n");
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
/*
		ret = nclfprintf(fp,"Variable: %s (HLU object)\n",v_name);
		if(ret < 0) {
			return(NhlWARNING);
		}
		break;
*/
	case FILEVAR:
		ret = nclfprintf(fp,"Variable: %s (file variable)\n",v_name);
		if(ret < 0) {
			return(NhlWARNING);
		}
		break;
	case FILEVARSUBSEL:
		ret = nclfprintf(fp,"Variable: %s (file variable subsection)\n",v_name);
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
	if(thevalue == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The value associated with variable (%s) has been freed, can't print it",v_name);
		return(NhlWARNING);
	}
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
			ret0 = _Nclprint(tmp_md->multidval.type,fp,&(((char*)tmp_md->multidval.val)
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
	
	return(ret0);
}

NhlErrorTypes VarPrint
#if	NhlNeedProto
(NclObj theobj,FILE *fp)
#else
(theobj,fp)
NclObj theobj;
FILE *fp;
#endif
{
	NclVar self = (NclVar) theobj;
	NclMultiDValData thevalue = NULL;
	NhlErrorTypes ret0 = NhlNOERROR;

	VarPrintVarSummary(theobj, fp);

	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	
	if((self != NULL) && (thevalue != NULL)) {
		if(thevalue->multidval.data_type)
			ret0 = _NclPrint((NclObj)thevalue,fp);
	}
	return(ret0);
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
	ng_size_t tmp_dim_size =1;
	NclAtt tmp_att;


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
		if((_NclSetStatus((NclObj)value,PERMANENT))||
		   ((var_type == PARAM)&&(value->obj.status != STATIC))
		   ||(var_type == RETURNVAR) 
		   ||(var_type == ATTVALLINK)) {
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
		for(;i< NCL_MAX_DIMENSIONS; i++) {
			var_out->var.dim_info[i].dim_size = -1;
			var_out->var.dim_info[i].dim_num = i;
			var_out->var.dim_info[i].dim_quark = -1;
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
	if(var_type == PARAM) {
		var_out->var.att_id = att_id;
		if(att_id != -1) {
			_NclAddParent(_NclGetObj(att_id),(NclObj)var_out);
			var_out->var.att_cb = _NclAddCallback(_NclGetObj(att_id),(NclObj)var_out,_NclVarMissingNotify,MISSINGNOTIFY,NULL);
		} 
		if(coords != NULL) {
			for(i = 0; i<var_out->var.n_dims; i++) {
				tmp_var = (NclVar)_NclGetObj(coords[i]);
				if(coords[i] != -1) {
					tmp_var->var.var_type = COORD;
					_NclAddParent((NclObj)tmp_var,(NclObj)var_out);
					var_out->var.coord_vars[i] = coords[i];
				} else {
					var_out->var.coord_vars[i] = coords[i];

				}
			}
		}
		_NclAddParent(_NclGetObj(var_out->var.thevalue_id),(NclObj)var_out);
		if(class_ptr == nclVarClass) {
			_NclCallCallBacks((NclObj)var_out,CREATED);
		}
	} else {
		if(att_id != -1) {
/*
* Unfortunately atts are created as permanent always so I have to check ref count instead
			if(!_NclSetStatus((NclObj)_NclGetObj(att_id),PERMANENT)) {
*/
			tmp_att = (NclAtt)_NclGetObj(att_id);
			if(tmp_att->obj.ref_count != 0) {
				tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(att_id),NULL);
				_NclSetStatus((NclObj)tmp_att,PERMANENT);
				_NclAddParent((NclObj)tmp_att,(NclObj)var_out);
				var_out->var.att_cb = _NclAddCallback((NclObj)tmp_att,(NclObj)var_out,_NclVarMissingNotify,MISSINGNOTIFY,NULL);
				var_out->var.att_id = tmp_att->obj.id;
			} else {
				var_out->var.att_id = tmp_att->obj.id;
				_NclAddParent(_NclGetObj(att_id),(NclObj)var_out);
				var_out->var.att_cb = _NclAddCallback(_NclGetObj(att_id),(NclObj)var_out,_NclVarMissingNotify,MISSINGNOTIFY,NULL);
			}
		} else {
			var_out->var.att_id = att_id;
		}
		if(coords != NULL) {
			for(i = 0; i<var_out->var.n_dims; i++) {
				if(coords[i] != -1) {
					tmp_var = (NclVar)_NclGetObj(coords[i]);
					if(!_NclSetStatus((NclObj)tmp_var,PERMANENT)) {
						tmp_var = _NclCopyVar(tmp_var,NULL,NULL);
						tmp_var->var.var_type = COORD;
						_NclSetStatus((NclObj)tmp_var,PERMANENT);
						var_out->var.coord_vars[i] = tmp_var->obj.id;
						_NclAddParent((NclObj)tmp_var,(NclObj)var_out);
					} else {
						tmp_var->var.var_type = COORD;
						_NclAddParent((NclObj)tmp_var,(NclObj)var_out);
						var_out->var.coord_vars[i] = coords[i];
					}
				} else {
					var_out->var.coord_vars[i] = coords[i];
				}
			}
		}
		_NclAddParent(_NclGetObj(var_out->var.thevalue_id),(NclObj)var_out);
		if(class_ptr == nclVarClass) {
			_NclCallCallBacks((NclObj)var_out,CREATED);
		}
	}
	var_out->var.sel_rec = NULL;
	var_out->var.ref_var = NULL;
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
	if(self_var->var.att_id != -1) {
		_NhlCBDelete(self_var->var.att_cb);
		_NclDelParent(_NclGetObj(self_var->var.att_id),(NclObj)self_var);
	}

	
	if(value != NULL) {
		_NclDelParent(value,self);
	}
	if(self_var->obj.cblist != NULL) {
		_NhlCBDestroy(self_var->obj.cblist);
	}
	if(self_var->var.sel_rec != NULL) {
		for(i = 0; i <  self_var->var.sel_rec->n_entries; i++) {
			if(self_var->var.sel_rec->selection[i].sel_type == Ncl_VECSUBSCR){
				NclFree(self_var->var.sel_rec->selection[i].u.vec.ind);
			}
		}
		NclFree(self_var->var.sel_rec);
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
	NhlErrorTypes ret = NhlNOERROR;
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
			if(value->multidval.type != thevalue->multidval.type) {
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
#if 0
					if (thevalue->multidval.type->type_class.type == Ncl_Typelogical) {
						if (*(int*)value_md->multidval.val != -1) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Logical type variable (%s) missing value can only be set to Missing: setting to Missing",v_name);
							if(value_md != value && value_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)value_md);
							}
							value_md = _NclCreateLMissing();
						}
					}
#endif
/*
				_NclResetMissingValue(thevalue,&tmp_mis);	
*/
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Type Mismatch: The type of missing value could not be converted to type of variable (%s)",v_name);
					return(NhlFATAL);
				}
			} else {
				value_md = value;
#if 0
				if (thevalue->multidval.type->type_class.type == Ncl_Typelogical) {
					if (*(int*)value_md->multidval.val != -1) {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Logical type variable (%s) missing value can only be set to Missing: setting to Missing",v_name);
						value_md = _NclCreateLMissing();
					}
				}
				memcpy((void*)&tmp_mis,value->multidval.val,value->multidval.type->type_class.size);
#endif
/*
				_NclResetMissingValue(thevalue,&tmp_mis);	
*/
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
		self->var.att_cb = _NclAddCallback((NclObj)_NclGetObj(self->var.att_id),(NclObj)self,_NclVarMissingNotify,MISSINGNOTIFY,NULL);
	}
	ret = _NclAddAtt(self->var.att_id,attname,value_md,sel_ptr);
	if((value_md != value)&&(value_md->obj.status != PERMANENT) ) {
		_NclDestroyObj((NclObj)value_md);
	}
	
	return(ret);
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
	ng_size_t dim_size = 1;
	NclQuark* nameptr;
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
			return(NULL);
		}
	} else if((dim_num < self->var.n_dims)&&(dim_num >= 0)) {
/*
* When dim_num provided the output is dim_name
*/
		if(self->var.dim_info[dim_num].dim_quark != -1) {
			nameptr = (NclQuark*)NclMalloc(sizeof(NclQuark));

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
			return(NULL);
		}
	} else if(dim_num >= self->var.n_dims) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) only has (%d) dimensions , cannot access dimension (%d)",v_name,self->var.n_dims,dim_num);
		return(NULL);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to access  illegal dimension number or name");
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
	} 
	return(NULL);	
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
		} else {
			if(self->var.dim_info[dim_num].dim_quark != dim_quark) {
				self->var.dim_info[dim_num].dim_quark = dim_quark;
				if(self->var.coord_vars[dim_num] != -1) {
					tmp_var = (NclVar)_NclGetObj(self->var.coord_vars[dim_num]);
					if(tmp_var != NULL) {
						tmp_var->var.var_quark = dim_quark;
						tmp_var->var.dim_info[0].dim_quark = dim_quark;
					}
				}
					
			} 
		}
		if (self->var.ref_var != NULL) {
			NclVar rvar = (NclVar) self->var.ref_var;
			while (rvar) {
				rvar->var.dim_info[dim_num].dim_quark = dim_quark;
				if (rvar->var.coord_vars[dim_num] != -1) {
					tmp_var = (NclVar)_NclGetObj(rvar->var.coord_vars[dim_num]);
					if(tmp_var != NULL) {
						tmp_var->var.var_quark = dim_quark;
						tmp_var->var.dim_info[0].dim_quark = dim_quark;
					}
				}
				rvar = (NclVar) rvar->var.ref_var;
			}
		}
		return(NhlNOERROR);
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
		if((self->var.coord_vars[index] != -1)&&(_NclGetObj(self->var.coord_vars[index]) != NULL)) {
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
	int i,j;
	NclScalar *missing_ptr;
	ng_size_t miss_dim_sizes[NCL_MAX_DIMENSIONS];
	NclVar self_var = (NclVar)self;
	NclSelectionRecord mysel;
	int theval_ndims, tmpmd_ndims;


/*
* Preconditions value is a NclMultiDValData
* self is an NclVar
* if sel_ptr is set n_entries == n_dims of thevalue field
*/
	thevalue = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
/*
* When id's are equal a write screws things up 
* dib -- except it's okay if it references a scalar logical variable - 2011-07-12
*/
	if(thevalue->obj.id == value->obj.id && thevalue->multidval.data_type == NCL_logical &&
		thevalue->multidval.kind == SCALAR) {
		return NhlNOERROR;
	}
	else if (thevalue->obj.id != value->obj.id) {
#ifdef NIO_LIB_ONLY
		if (sel_ptr == NULL) {
#else
		/*Handle NCL_list*/
		if(NCL_list == thevalue->multidval.data_type)
		{
			NclObj theobj = (NclObj)_NclGetObj(*(int *)thevalue->multidval.val);
			NclObj rhsobj = (NclObj)_NclGetObj(*(int *)value->multidval.val);
			NclObj tmp = NULL;
			NclList thelist = (NclList) theobj;
			NclList rhslist = (NclList) rhsobj;
			NclListObjList *step;

			if(NULL != thelist)
			{
				while(0 < thelist->list.nelem)
				{
					tmp = (NclObj)_NclListPop((NclObj)thelist);
				}
			}

		      /*Comment out this paragraph (as this removed items from rhslist),
		       *and use the paragraph below (which copy items for rhslist).
		       *Wei, 7/12/2011.
		       *
		       *while(0 < rhslist->list.nelem)
		       *{
		       *	tmp = (NclObj)_NclListPop((NclObj)rhslist);
		       *	_NclListPush((NclObj)thelist, tmp);
		       *}
		       */

			step = rhslist->list.last;
			while(step != NULL)
			{
				tmp = _NclGetObj(step->obj_id);
	
				_NclListPush((NclObj)thelist, tmp);

				step = step->prev;
			}

			return (NhlNOERROR);
		}
		else if(sel_ptr == NULL) {
#endif
			if(value->multidval.type->type_class.type != thevalue->multidval.type->type_class.type) {
				tmp_md = _NclCoerceData(value,
						thevalue->multidval.type->type_class.type,
						NULL);
				if(tmp_md == NULL) {
					NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Assignment type mismatch, right hand side can't be coerced to type of left hand side"));
					return(NhlFATAL);
				}
			} else {
				tmp_md = value;
			}
			/* 
			 * ignore singleton dims in the output and input
			 */

			for (i = 0, theval_ndims = 0; i < thevalue->multidval.n_dims; i++) {
				if (thevalue->multidval.dim_sizes[i] > 1) {
					theval_ndims++;
				}
			}
			for (i = 0, tmpmd_ndims = 0; i < tmp_md->multidval.n_dims; i++) {
				if (tmp_md->multidval.dim_sizes[i] > 1) {
					tmpmd_ndims++;
				}
			}

			if((tmpmd_ndims == theval_ndims)|| (tmp_md->multidval.kind == SCALAR)){
				if (tmp_md->multidval.kind != SCALAR) {
					for(i = 0, j = 0; i< thevalue->multidval.n_dims;i++) {
						if (thevalue->multidval.dim_sizes[i] == 1)
							continue;
						while (tmp_md->multidval.dim_sizes[j] == 1)
							j++;
						
						if(tmp_md->multidval.dim_sizes[j] != thevalue->multidval.dim_sizes[i]) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,
								  "Dimension sizes of left hand side and right hand side of assignment do not match");
							if((tmp_md!=value) &&(tmp_md->obj.status == TEMPORARY)) {
								_NclDestroyObj((NclObj)tmp_md);
							}
							return(NhlFATAL);
						}
						j++;
					}
				}
				if (! thevalue->multidval.missing_value.has_missing &&
				    tmp_md->multidval.missing_value.has_missing) {
				        /*
					 * A missing value attribute must be added to the variable
					 */
					missing_ptr = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
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
						TEMPORARY,
						NULL,
						thevalue->multidval.type);
					if(self_var->var.att_id == -1) {
						self_var->var.att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)self_var);
						self->var.att_cb = _NclAddCallback((NclObj)_NclGetObj(self->var.att_id),
										   (NclObj)self,
										   _NclVarMissingNotify,
										   MISSINGNOTIFY,
										   NULL);
					}
					_NclAddAtt(self_var->var.att_id,NCL_MISSING_VALUE_ATT,attvalue,NULL);
				}

				mysel.n_entries = thevalue->multidval.n_dims;
				for( i = 0; i < thevalue->multidval.n_dims; i++) {
					mysel.selection[i].sel_type = Ncl_SUB_ALL;
					mysel.selection[i].dim_num = i;
					mysel.selection[i].u.sub.stride = 1;
				}
				ret = _NclWriteSubSection((NclData)thevalue,&mysel,(NclData)tmp_md);
				if((tmp_md != value)&&(tmp_md->obj.status != PERMANENT))	
					_NclDestroyObj((NclObj)tmp_md);
				return ret;
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
	return(NhlFATAL);
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

static NhlErrorTypes VarDeleteCoord
#if	NhlNeedProto
(struct  _NclVarRec* self, char* coord_name)
#else
(self, coord_name )
struct  _NclVarRec* self;
char* coord_name;
#endif
{
	int index;
	
	index = VarIsADim(self,coord_name);
	if((index>=0)&&(index < self->var.n_dims))  {
		if((self->var.coord_vars[index] != -1)&&(_NclGetObj(self->var.coord_vars[index]) != NULL)) {
			_NclDelParent(_NclGetObj(self->var.coord_vars[index]),(NclObj)self);
			self->var.coord_vars[index] = -1;
		} else {
			self->var.coord_vars[index] = -1;
		}
		return(NhlNOERROR);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) is not a named dimension. Can not assign coordinate variable",coord_name);
		return(NhlFATAL);
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
			if(value->multidval.dim_sizes[0] == self->var.dim_info[index].dim_size) {
				tmp.dim_quark = NrmStringToQuark(coord_name);
				tmp_obj = (NclObj)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,value,&tmp,-1,NULL,COORD,coord_name,PERMANENT);
				_NclAddParent(tmp_obj,(NclObj)self);
				self->var.coord_vars[index] = tmp_obj->obj.id;
				if(self->var.coord_vars[index] == -1) {
					return(NhlFATAL);
				} else {
					return(NhlNOERROR);
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables must be the same dimension as their dimension");
				return(NhlFATAL);
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
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred reading %s",((self->var.thesym != NULL)?self->var.thesym->name:"unknown"));
				return(NULL);
			}
			tmp_sel.n_entries = 1;
			tmp_sel.selected_from_sym = NULL;
			tmp_sel.selected_from_var = NULL;
			tmp_sel.selection[0].dim_num = 0;
			j = 0;
			tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(self->var.att_id),NULL);
			for(i = 0; i< sel_ptr->n_entries; i++) {
				coord_var = NULL;
				if((self->var.coord_vars[sel_ptr->selection[i].dim_num] != -1)&&(_NclGetObj(self->var.coord_vars[sel_ptr->selection[i].dim_num]) != NULL)) {
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
							single = sel_ptr->selection[i].u.sub.is_single;
						}
					}
/*
* Since tmp_sel is not null the _NclVarRead function will create a new
* coordvar so the situation of coord_var equalling the one pointed to in
* the coordinate array is not possible
*/
					coord_var = _NclVarRead((NclVar)_NclGetObj(self->var.coord_vars[sel_ptr->selection[i].dim_num]),&tmp_sel);
					if(tmp_sel.selection[0].sel_type == Ncl_VECSUBSCR) {
						NclFree(tmp_sel.selection[0].u.vec.ind);
					}
					coords[j] = coord_var->obj.id; 
				} else {
                                        switch(sel_ptr->selection[i].sel_type) {
                                        case Ncl_VECSUBSCR:
                                                if(sel_ptr->selection[i].u.vec.n_ind == 1) {
                                                        single = 1;
                                                }
                                                break;
                                        case Ncl_SUB_ALL:
                                                if(self->var.dim_info[sel_ptr->selection[i].dim_num].dim_size == 1) {
                                                        single = 0;
                                                }
                                                break;
                                        case Ncl_SUB_VAL_DEF:
                                                if(sel_ptr->selection[i].u.sub.start == self->var.dim_info[sel_ptr->selection[i].dim_num].dim_size -1) {
                                                        single = 0;
                                                }
                                                break;
                                        case Ncl_SUB_DEF_VAL:
                                                if(sel_ptr->selection[i].u.sub.finish== 0) {
                                                        single = 0;
                                                }
                                                break;
                                        case Ncl_SUBSCR:
                                                if(sel_ptr->selection[i].u.sub.start == sel_ptr->selection[i].u.sub.finish) {
                                                        single = sel_ptr->selection[i].u.sub.is_single;
                                                }
                                                break;
                                        }

					coords[j] = -1;
						
				}
				if(single){ 
					if(coords[j] != -1) {
						if(tmp_att == NULL) {
							tmp_att = (NclAtt)_NclGetObj(_NclAttCreate(NULL,NULL,Ncl_Att,0,NULL));
						}
						_NclAddAtt(tmp_att->obj.id,NrmQuarkToString(self->var.dim_info[sel_ptr->selection[i].dim_num].dim_quark),_NclVarValueRead(coord_var,NULL,NULL),NULL);
						if(coord_var && coord_var->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)coord_var);
						}
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
	NclVar thevar = (NclVar)theobj;

        if(theobj->obj.parents == NULL) {
                NhlPError(NhlFATAL,NhlEUNKNOWN,"VarDelParent: Attempt to delete parent from empty list");
                return(NhlFATAL);
        }

        tmp = theobj->obj.parents;
        while((tmp!=NULL)&&(tmp->pid == parent->obj.id)) {
                theobj->obj.parents = theobj->obj.parents->next;
                NclFree(tmp);
                tmp = theobj->obj.parents;
		theobj->obj.ref_count--;
                found = 1;
        }
        if((tmp == NULL)&&(found)) {
		if((theobj->obj.ref_count <=0)&&((thevar->var.thesym == NULL)||(thevar->var.thesym->type == UNDEF)))
			_NclDestroyObj(theobj);
                return(NhlNOERROR);
        }
        while(tmp->next != NULL) {
                if(tmp->next->pid == parent->obj.id) {
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
		if((theobj->obj.ref_count <=0)&&((thevar->var.thesym == NULL)||(thevar->var.thesym->type == UNDEF)))
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
/*
	if(parent->obj.obj_type_mask & ( Ncl_Var)) {
*/

		theobj->obj.ref_count++;
	        tmp = theobj->obj.parents;
       		theobj->obj.parents = NclMalloc((unsigned)sizeof(NclRefList));
       		theobj->obj.parents->next = tmp;
        	theobj->obj.parents->pid = parent->obj.id;
		
		return(NhlNOERROR);
/*
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN, "Only files and variables can be parents of variables\n");
		return(NhlFATAL);
	}
*/
}

static struct _NclVarRec * VarCopy
#if	NhlNeedProto
(struct _NclVarRec *thevar, struct _NclSymbol *new_name, struct _NclVarRec *storage)
#else
(thevar,new_name, storage)
struct _NclVarRec *thevar;
struct _NclSymbol *new_name;
struct _NclVarRec *storage;
#endif
{
	struct _NclVarRec *tmp_var = NULL;
	NclObj tmp_obj = NULL;

	tmp_obj = (NclObj)_NclCopyAtt((NclAtt)_NclGetObj(thevar->var.att_id),NULL);
	tmp_var = _NclVarNclCreate(NULL,
				   thevar->obj.class_ptr,thevar->obj.obj_type,
				   thevar->obj.obj_type_mask,
				   new_name,
				   (NclMultiDValData)_NclGetObj(thevar->var.thevalue_id),
				   thevar->var.dim_info,
				   ((tmp_obj != NULL)?tmp_obj->obj.id:-1),
				   thevar->var.coord_vars,
				   (thevar->var.var_type == PARAM)?NORMAL:thevar->var.var_type,
				   (new_name == NULL)?NrmQuarkToString(thevar->var.var_quark):new_name->name,
				   TEMPORARY);

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
	NclSelectionRecord tmp_sel;
	NclMultiDValData val_md = NULL;
	int i,j,done,m;
	ng_size_t lhs_n_elem,rhs_n_elem;
	void *tmp_coord;
	char *tmp_ptr;
	NclMultiDValData *tmp_coord_array;
	NclVar *tmp_coord_vars,cvar = NULL;
	NclQuark *tmp_name_array;
	NclAtt tmp_att;
	NclAttList *att_list;


	rhs_type = _NclGetVarRepValue(rhs);
	lhs_type = _NclGetVarRepValue(lhs);
	tmp_sel.selected_from_sym=NULL;
	tmp_sel.selected_from_var = NULL;
	tmp_sel.n_entries = 1;
	
	if(rhs_type == Ncl_Obj) {
		return(NhlFATAL);
	}
	lhs_md = (NclMultiDValData)_NclGetObj(lhs->var.thevalue_id);
	rhs_md = (NclMultiDValData)_NclGetObj(rhs->var.thevalue_id);
	if(lhs_md == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred reading %s",((lhs->var.thesym != NULL)?lhs->var.thesym->name:"unknown"));
		return(NhlFATAL);
	}
	if(rhs_md == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred reading %s",((rhs->var.thesym != NULL)?rhs->var.thesym->name:"unknown"));
		return(NhlFATAL);
	}
/*
	if((rhs_sel_ptr == NULL)&&(lhs_sel_ptr == NULL)&&(lhs->obj.id == rhs->obj.id)&&(lhs->var.thevalue_id == rhs->var.thevalue_id)&&(lhs->var.att_id == rhs->var.att_id)) { 
		return(NhlNOERROR);
	}
*/
	
	if(lhs->obj.id == rhs->obj.id) { 
/*
* Same Variable
*/
		if(rhs_sel_ptr != NULL) {
			rhs_md = _NclVarValueRead(rhs,rhs_sel_ptr,NULL);
			if(rhs_md == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred reading %s",((rhs->var.thesym != NULL)?rhs->var.thesym->name:"unknown"));
				return(NhlFATAL);
			}
		}


		if(lhs_md->obj.id != rhs_md->obj.id) {
			ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			if(rhs_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
		} else if((lhs_sel_ptr != NULL)&&(rhs_sel_ptr == NULL)) {
			rhs_md = _NclCopyVal(rhs_md,NULL); 
			ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			if(rhs_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
		}
/*
* When var is the same special care must be taken to not overwrite coordinate information
* This would happend in named subscripting case. Essentially have to loop through and read
* coord values out the loop again and write them.
*/
		if((lhs_sel_ptr !=NULL) &&(rhs_sel_ptr == NULL)) {
			tmp_coord_array = (NclMultiDValData*)NclMalloc(lhs->var.n_dims * sizeof(NclMultiDValData));
			tmp_coord_vars = (NclVar*)NclMalloc(lhs->var.n_dims * sizeof(NclVar));
			tmp_name_array = (NclQuark*)NclMalloc(lhs->var.n_dims * sizeof(NclQuark));
			for(i = 0; i< lhs->var.n_dims; i++) {
				tmp_name_array[i] = rhs->var.dim_info[i].dim_quark;
				if(rhs->var.coord_vars[i] != -1) {
/*
* Have to copy it since _NclWriteCoord uses WriteVar which would destroy value pointer causing
* free memory reads when dimension reordering occurs.
*/
					tmp_coord_vars[i] = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[i].dim_quark),NULL);
					tmp_coord_array[i] = _NclCopyVal(_NclVarValueRead(tmp_coord_vars[i],NULL,NULL),NULL);
					
				} else {
					tmp_coord_array[i] = NULL;
					tmp_coord_vars[i] = NULL;
				}
			} 
			for(i = 0; i < lhs->var.n_dims; i++) {
				_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(tmp_name_array[i]));
			}
			for(i = 0; i < lhs->var.n_dims; i++) {
				tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
				tmp_sel.selection[0].dim_num = 0;
				if(tmp_coord_array[i] != NULL) {
                                	_NclWriteCoordVar(lhs,tmp_coord_array[i],NrmQuarkToString(tmp_name_array[i]),&tmp_sel);
					_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(tmp_name_array[i]),NULL),tmp_coord_vars[i]);
				}
			}
				
			NclFree(tmp_coord_array);
			NclFree(tmp_coord_vars);
			NclFree(tmp_name_array);
		} else if((lhs_sel_ptr == NULL)&&(rhs_sel_ptr != NULL)) {
			tmp_coord_array = (NclMultiDValData*)NclMalloc(lhs->var.n_dims * sizeof(NclMultiDValData));
			tmp_coord_vars = (NclVar*)NclMalloc(lhs->var.n_dims * sizeof(NclVar));
			tmp_name_array = (NclQuark*)NclMalloc(lhs->var.n_dims * sizeof(NclQuark));
			for(i = 0; i< lhs->var.n_dims; i++) {
				tmp_name_array[i] = rhs->var.dim_info[rhs_sel_ptr->selection[i].dim_num].dim_quark;
				if(rhs->var.coord_vars[rhs_sel_ptr->selection[i].dim_num] != -1) {
/*
* Have to copy it since _NclWriteCoord uses WriteVar which would destroy value pointer causing
* free memory reads when dimension reordering occurs.
*/
					tmp_sel.selection[0] = rhs_sel_ptr->selection[i];
					tmp_sel.selection[0].dim_num = 0;
					tmp_coord_vars[i] = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[i].dim_num].dim_quark),NULL);
					tmp_coord_array[i] = _NclVarValueRead(tmp_coord_vars[i],&tmp_sel,NULL);
					if((tmp_coord_array[i] != NULL)&&(tmp_coord_array[i]->obj.status == PERMANENT)) {
						tmp_coord_array[i] = _NclCopyVal(tmp_coord_array[i],NULL);
					}
					
				} else {
					tmp_coord_array[i] = NULL;
					tmp_coord_vars[i] = NULL;
				}
			} 
			for(i = 0; i < lhs->var.n_dims; i++) {
				if(tmp_name_array[i] != -1)
					_NclWriteDim(lhs,i,NrmQuarkToString(tmp_name_array[i]));
			}
			for(i = 0; i < lhs->var.n_dims; i++) {
				if(tmp_coord_array[i] != NULL) {
                                	_NclWriteCoordVar(lhs,tmp_coord_array[i],NrmQuarkToString(tmp_name_array[i]),NULL);
					_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(tmp_name_array[i]),NULL),tmp_coord_vars[i]);
				}
			}
			NclFree(tmp_name_array);
			NclFree(tmp_coord_array);
			NclFree(tmp_coord_vars);

		} else if((lhs_sel_ptr != NULL) &&(rhs_sel_ptr != NULL)) {
			tmp_coord_array = (NclMultiDValData*)NclMalloc(lhs->var.n_dims * sizeof(NclMultiDValData));
			tmp_coord_vars = (NclVar*)NclMalloc(lhs->var.n_dims * sizeof(NclVar));
			tmp_name_array = (NclQuark*)NclMalloc(lhs->var.n_dims * sizeof(NclQuark));
			for(i = 0; i< lhs->var.n_dims; i++) {
				tmp_name_array[i] = rhs->var.dim_info[rhs_sel_ptr->selection[i].dim_num].dim_quark;
				if(rhs->var.coord_vars[rhs_sel_ptr->selection[i].dim_num] != -1) {
/*
* Have to copy it since _NclWriteCoord uses WriteVar which would destroy value pointer causing
* free memory reads when dimension reordering occurs.
*/
					tmp_sel.selection[0] = rhs_sel_ptr->selection[i];
					tmp_sel.selection[0].dim_num = 0;
					tmp_coord_vars[i] = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[i].dim_num].dim_quark),&tmp_sel);
					tmp_coord_array[i] = _NclCopyVal(_NclVarValueRead(tmp_coord_vars[i],NULL,NULL),NULL);
					
				} else {
					tmp_coord_array[i] = NULL;
				}
			} 
			for(i = 0; i < lhs->var.n_dims; i++) {
				if(tmp_name_array[i] != -1) 
					_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(tmp_name_array[i]));
			}
			for(i = 0; i < lhs->var.n_dims; i++) {
				if(tmp_coord_array[i] != NULL) {
                                        tmp_sel.selection[0] = lhs_sel_ptr->selection[i];
                                        tmp_sel.selection[0].dim_num = 0;

                        		_NclWriteCoordVar(lhs,tmp_coord_array[i],NrmQuarkToString(tmp_name_array[i]),&tmp_sel);
					_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(tmp_name_array[i]),NULL),tmp_coord_vars[i]);
				}
			}
			NclFree(tmp_name_array);
			NclFree(tmp_coord_array);
			NclFree(tmp_coord_vars);

		}
/*
* lhs_sel_ptr == NULL and rhs_sel_ptr == NULL is a don't care case
*/
		return(ret);
	} else if((lhs_sel_ptr != NULL)&&(rhs_sel_ptr != NULL)) {
#ifdef NCLVARDEBUG
		fprintf(stdout,"(rhs_sel_ptr != NULL) &&(lhs_sel_ptr != NULL)\n");
#endif
/*
* Taking advatage that the following call fills in the extents for each selection
*/
		j = 0;
		for(i=0; i < lhs_sel_ptr->n_entries ; i++) {
			if(((lhs_sel_ptr->selection[i].sel_type != Ncl_VECSUBSCR) && (!lhs_sel_ptr->selection[i].u.sub.is_single))||
				((lhs_sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR) && (lhs_sel_ptr->selection[i].u.vec.n_ind != 1) )) {
					j++;
				}
		}
		lhs_n_elem = j;
		j = 0;
		for(i=0; i < rhs_sel_ptr->n_entries ; i++) {
			if(((rhs_sel_ptr->selection[i].sel_type != Ncl_VECSUBSCR) && (!rhs_sel_ptr->selection[i].u.sub.is_single))||
				((rhs_sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR) && (rhs_sel_ptr->selection[i].u.vec.n_ind != 1) )) {
					j++;
				}
		}
		rhs_n_elem = j;

		if(rhs_n_elem > 0 ){
			if(lhs_n_elem != rhs_n_elem) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"VarVarWrite: Number of dimensions on left hand side does not match right hand side");
				return(NhlFATAL);
			}
		}


		if(lhs_md->obj.id != rhs_md->obj.id) {
			if(rhs_type != lhs_type) {
				rhs_md = (NclMultiDValData)_NclReadSubSection((NclData)rhs_md,rhs_sel_ptr,NULL); 
				ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			} else {
				ret = _NclReadThenWriteSubSection((NclData)lhs_md, lhs_sel_ptr, (NclData)rhs_md, rhs_sel_ptr);
			}
			if(rhs_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_md);
			}
			if(ret < NhlINFO) {
				return(ret);
			}
		} else {
			rhs_md = (NclMultiDValData)_NclReadSubSection((NclData)rhs_md,rhs_sel_ptr,NULL); 
			ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			if(rhs_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
		}
		j = 0;
		i = 0;
		done = 0;
		while(!done) {
			switch(rhs_sel_ptr->selection[j].sel_type) {
			case Ncl_VECSUBSCR:
				rhs_n_elem = rhs_sel_ptr->selection[j].u.vec.n_ind;
				break;
			default:
				rhs_n_elem =  (ng_size_t) labs((rhs_sel_ptr->selection[j].u.sub.finish
								- rhs_sel_ptr->selection[j].u.sub.start)
							       / rhs_sel_ptr->selection[j].u.sub.stride) + 1;
				break;
			}
			switch(lhs_sel_ptr->selection[i].sel_type) {
			case Ncl_VECSUBSCR:
				lhs_n_elem = lhs_sel_ptr->selection[i].u.vec.n_ind;
				break;
			default:
				lhs_n_elem =  (ng_size_t) labs((lhs_sel_ptr->selection[i].u.sub.finish
								- lhs_sel_ptr->selection[i].u.sub.start)
							       / lhs_sel_ptr->selection[i].u.sub.stride) + 1;
				break;
			}
			if(((lhs_n_elem != 1)&&(rhs_n_elem != 1))||((lhs_n_elem == 1)&&(rhs_n_elem == 1))) {
/*
* Nothing needs to be done unless right hand side has defined dimension 
*/
				if(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark > 0) {

/*
* If names don't match
*/
					if((lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark != rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark)){
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 0\n");
#endif
						if(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark == -1) {
							_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark));
						} else {
/*
* This is a warning condition since names don't match.
*/
							if(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1){
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 1\n");
#endif
								NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs and overwriting coordinate variable, use \"(/../)\" if this change is not desired",lhs_sel_ptr->selection[i].dim_num);
								ret = NhlWARNING;
								_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark));
								tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]);
								if(tmp_coord != NULL) {
									_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
								}
								lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] = -1;
							} else {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 2\n");
#endif
								NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs, use \"(/../)\" if this change is not desired",lhs_sel_ptr->selection[i].dim_num);
								ret = NhlWARNING;
								_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark));
							}
						}
/*
* lhs side has either been freed or never had a coordinate var and right side has one
*/
						if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] == -1)&&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] != -1)) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 3\n");
#endif
							tmp_sel.selection[0] = rhs_sel_ptr->selection[j];
							tmp_sel.selection[0].dim_num = 0;
							cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
							val_md = _NclVarValueRead(cvar,NULL,NULL);
							tmp_sel.selection[0]= lhs_sel_ptr->selection[i];
							tmp_sel.selection[0].dim_num = 0;
							if((val_md != NULL) &&(lhs_n_elem == lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size)) {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 4\n");
#endif
								_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
							} else if(val_md != NULL) {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 5\n");
#endif
/*
* Missing values need to be filled in
*/
								tmp_coord = NclMalloc(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size*val_md->multidval.type->type_class.size);
								tmp_ptr = (char*)tmp_coord;
								for(m = 0; m < lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size; m++) {
									memcpy((void*)tmp_ptr,(void*)&(val_md->multidval.type->type_class.default_mis),val_md->multidval.type->type_class.size);
									tmp_ptr = tmp_ptr + val_md->multidval.type->type_class.size;

								}
								_NclWriteCoordVar(lhs,
									_NclCreateMultiDVal( NULL, NULL, Ncl_MultiDValData, 0, tmp_coord, &val_md->multidval.type->type_class.default_mis, 1, &lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size, TEMPORARY, NULL, val_md->multidval.type),
									NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),
									NULL);
								_NclWriteCoordVar(lhs,
									val_md,
									NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),
									&tmp_sel);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
								
							}
							if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
								_NclDestroyObj((NclObj)cvar);
							}
						}  
/*
* dimension names match and both have coord var
*/
					} else if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1)&&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] != -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 6\n");
#endif
						tmp_sel.selection[0] = rhs_sel_ptr->selection[j];
						tmp_sel.selection[0].dim_num = 0;
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						tmp_sel.selection[0]= lhs_sel_ptr->selection[i];
						tmp_sel.selection[0].dim_num = 0;
						if(val_md != NULL) {
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
						}
/*
* dimension names match and only rhs has coord var
*/
							if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
								_NclDestroyObj((NclObj)cvar);
							}

					} else if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] == -1) &&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] != -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 7\n");
#endif
						tmp_sel.selection[0] = rhs_sel_ptr->selection[j];
						tmp_sel.selection[0].dim_num = 0;
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						tmp_sel.selection[0]= lhs_sel_ptr->selection[i];
						tmp_sel.selection[0].dim_num = 0;
						if((val_md != NULL)&&(lhs_n_elem == lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size)) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 8\n");
#endif
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
						} else if(val_md != NULL){
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 9\n");
#endif
	/*
	* Missing values need to be filled in
	*/
							tmp_coord = NclMalloc(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size*val_md->multidval.type->type_class.size);
							tmp_ptr = (char*)tmp_coord;
							for(m = 0; m < lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size; m++) {
								memcpy((void*)tmp_ptr,(void*)&(val_md->multidval.type->type_class.default_mis),val_md->multidval.type->type_class.size);
								tmp_ptr = tmp_ptr + val_md->multidval.type->type_class.size;

							}
							_NclWriteCoordVar(
								lhs,
								_NclCreateMultiDVal( 
									NULL, 
									NULL, 
									Ncl_MultiDValData, 
									0, 
									tmp_coord, 
									&val_md->multidval.type->type_class.default_mis, 
									1, 
									&lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size, 
									TEMPORARY, 
									NULL, 
									val_md->multidval.type),
								NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),
								NULL);
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
						}
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
                                                }

					} else  if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1) &&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] == -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 10\n");
#endif
						NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no coordinate variable for dimension number (%d), destroying coordinate var,  use \"(/../)\" if this is not desired outcome",lhs_sel_ptr->selection[i].dim_num);
						ret = NhlWARNING;
						tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]);
						if(tmp_coord != NULL) {
							_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
						}
						lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] = -1;
					}
				} else {
					if(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark >0) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 11\n");
#endif
						if(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1){
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 12\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no dimension name or coordinate variable, deleting name of lhs dimension number (%d) and destroying coordinate var,  use \"(/../)\" if this is not desired outcome",lhs_sel_ptr->selection[i].dim_num);
							ret = NhlWARNING;
							tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]);
							if(tmp_coord != NULL) {
								_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
							}
							lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] = -1;
						} else {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 13\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: lhs has dimension name and rhs doesn't, deleting name of lhs dimension number(%d), use \"(/../)\" if this is not desired outcome",lhs_sel_ptr->selection[i].dim_num);
							ret = NhlWARNING;
							
						}
						lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark = -1;
					}
					
				}
				i++;
				j++;	
			} else {
				if(lhs_n_elem == 1){
					i++;
				}
				if(rhs_n_elem == 1){
					j++;
				}
			}
			if((i == lhs->var.n_dims)||(j == rhs->var.n_dims)) {
				done = 1;
			}
		}
	} else if((rhs_sel_ptr == NULL) &&(lhs_sel_ptr == NULL)) {
#ifdef NCLVARDEBUG
		fprintf(stdout,"(rhs_sel_ptr == NULL) &&(lhs_sel_ptr == NULL)\n");
#endif
/*
* This could be the case during parameter remapping
*/
		if(lhs_md->obj.id != rhs_md->obj.id) {
			ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			if(ret < NhlWARNING){
				return(ret);
			} 
		}
/*
* Loop through all dimensions check dimension name. if unequal change name
* then if coord_var exists write to that coordinate variable
* When lhs_sel_ptr is null all dims are same size this simplifies things alot
* Note (2003-08-08): we are now skipping singleton dimensions that do not match in either the rhs or the lhs. 
*/
		for(i = 0,j = 0; i < lhs->var.n_dims; i++) {
			if (lhs->var.dim_info[i].dim_size == 1 && rhs->var.dim_info[j].dim_size != 1) 
				continue;
			else if (lhs->var.dim_info[i].dim_size != 1 && rhs->var.dim_info[j].dim_size == 1) {
				while (rhs->var.dim_info[j].dim_size == 1)
					j++;
			}
				
			if(rhs->var.dim_info[j].dim_quark>0) {
#ifdef NCLVARDEBUG
				fprintf(stdout,"case 14\n");
#endif
				if(lhs->var.dim_info[i].dim_quark != rhs->var.dim_info[j].dim_quark) {
#ifdef NCLVARDEBUG
					fprintf(stdout,"case 15\n");
#endif
					if(lhs->var.dim_info[i].dim_quark == -1) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 16\n");
#endif
						_NclWriteDim(lhs,i,NrmQuarkToString(rhs->var.dim_info[j].dim_quark));
						if(rhs->var.coord_vars[j] != -1) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 17\n");
#endif
							cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
							val_md = _NclVarValueRead(cvar,NULL,NULL);
							if(val_md != NULL) {
								_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
							}
							if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
								_NclDestroyObj((NclObj)cvar);
							}
						} 	 
					} else {
						if(lhs->var.coord_vars[i] != -1){
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 18\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs and overwriting coordinate variable, use \"(/../)\" if this change is not desired",i);
							ret = NhlWARNING;
							_NclWriteDim(lhs,i,NrmQuarkToString(rhs->var.dim_info[j].dim_quark));
							tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[i]);
							if(tmp_coord != NULL) {
								_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
							}
							lhs->var.coord_vars[i] = -1;
						} else {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 19\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs, use \"(/../)\" if this change is not desired",i);
							ret = NhlWARNING;
							_NclWriteDim(lhs,i,NrmQuarkToString(rhs->var.dim_info[j].dim_quark));
						}
						if(rhs->var.coord_vars[j] != -1) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 19.5\n");
#endif
							cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
							val_md = _NclVarValueRead(cvar,NULL,NULL);
							if(val_md != NULL) {
								_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
							}
							if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
								_NclDestroyObj((NclObj)cvar);
							}
						} 	 
					} 
				} else {
					if(rhs->var.coord_vars[j] != -1) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 20\n");
#endif
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						if(val_md != NULL) {
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
						}
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
						}
					} else if(lhs->var.coord_vars[i] != -1) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 21\n");
#endif
						NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no coordinate variable for dimension number (%d), destroying coordinate var,  use \"(/../)\" if this is not desired outcome",i);
						ret = NhlWARNING;
						tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[i]);
						if(tmp_coord != NULL) {
							_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
						}
						lhs->var.coord_vars[i] = -1;
					} 
				}
			} else if(lhs->var.dim_info[i].dim_quark > 0) {
#ifdef NCLVARDEBUG
					fprintf(stdout,"case 22\n");
#endif
				if(lhs->var.coord_vars[i] != -1){
#ifdef NCLVARDEBUG
					fprintf(stdout,"case 23\n");
#endif
					NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs and overwriting coordinate variable, use \"(/../)\" if this change is not desired",i);
					ret = NhlWARNING;
					lhs->var.dim_info[i].dim_quark = -1;
					tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[i]);
					if(tmp_coord != NULL) {
						_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
					}
					lhs->var.coord_vars[i] = -1;
				} else {
#ifdef NCLVARDEBUG
					fprintf(stdout,"case 24\n");
#endif
					NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs, use \"(/../)\" if this change is not desired",i);
					ret = NhlWARNING;
					lhs->var.dim_info[i].dim_quark = -1;
				}
			}
			j++;
		}
	} else if(rhs_sel_ptr == NULL) {
#ifdef NCLVARDEBUG
		fprintf(stdout,"(rhs_sel_ptr == NULL)\n");
#endif
		if(lhs_md->obj.id != rhs_md->obj.id) {
			j = 0;
			if(rhs_md->multidval.kind != SCALAR && lhs_md->multidval.n_dims != rhs_md->multidval.n_dims) {
				for(i=0; i < lhs_sel_ptr->n_entries ; i++) {
					if(((lhs_sel_ptr->selection[i].sel_type != Ncl_VECSUBSCR) && (!lhs_sel_ptr->selection[i].u.sub.is_single))||
						((lhs_sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR) && (lhs_sel_ptr->selection[i].u.vec.n_ind != 1) )) {
							j++;
						}
				}
				if(j != rhs_md->multidval.n_dims) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"VarVarWrite: Number of dimensions on left hand side does not match right hand side");
					return(NhlFATAL);
				}
			}
			ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			if(ret < NhlWARNING){
				return(ret);
			} 
		} else {
			rhs_md = _NclCopyVal(rhs_md,NULL); 
			ret = _NclAssignToVar(lhs,rhs_md,lhs_sel_ptr);
			if(rhs_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
		}
		j = 0;
		i = 0;
		done = 0;
		while(!done) {
			switch(lhs_sel_ptr->selection[i].sel_type) {
			case Ncl_VECSUBSCR:
				lhs_n_elem = lhs_sel_ptr->selection[i].u.vec.n_ind;
				break;
			default:
				lhs_n_elem =  (ng_size_t) labs((lhs_sel_ptr->selection[i].u.sub.finish
								- lhs_sel_ptr->selection[i].u.sub.start)
							       / lhs_sel_ptr->selection[i].u.sub.stride) + 1;
				break;
			}
			if((lhs_n_elem != 1)||((lhs_n_elem == 1)&&(rhs_md->multidval.totalelements ==1))||((lhs_n_elem == 1)&&(rhs_md->multidval.n_dims > j )&&(rhs_md->multidval.dim_sizes[j] == 1))){

if(rhs_md->multidval.totalelements !=1) {
				if(rhs->var.dim_info[j].dim_quark > 0) {
#ifdef NCLVARDEBUG
					fprintf(stdout,"case 25\n");
#endif
					if((lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark != rhs->var.dim_info[j].dim_quark)){
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 26\n");
#endif
						if(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark == -1) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 27\n");
#endif
							_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(rhs->var.dim_info[j].dim_quark));
						} else {
							if(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1){
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 28\n");
#endif
								NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs and overwriting coordinate variable, use \"(/../)\" if this change is not desired",lhs_sel_ptr->selection[i].dim_num);
								ret = NhlWARNING;
								_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(rhs->var.dim_info[j].dim_quark));
								tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]);
								if(tmp_coord != NULL) {
									_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
								}
								lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] = -1;
							} else {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 29\n");
#endif
								NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs, use \"(/../)\" if this change is not desired",lhs_sel_ptr->selection[i].dim_num);
								ret = NhlWARNING;
								_NclWriteDim(lhs,lhs_sel_ptr->selection[i].dim_num,NrmQuarkToString(rhs->var.dim_info[j].dim_quark));
							}
						}
				
						if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] == -1)&&(rhs->var.coord_vars[j] != -1)) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 30\n");
#endif
							cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
							val_md = _NclVarValueRead(cvar,NULL,NULL);
							tmp_sel.selection[0]= lhs_sel_ptr->selection[i];
							tmp_sel.selection[0].dim_num = 0;
							if((val_md != NULL) &&(lhs_n_elem == lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size)) {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 31\n");
#endif
								_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),&tmp_sel);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
							} else if(val_md != NULL)  {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 32\n");
#endif
								tmp_coord = NclMalloc(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size*val_md->multidval.type->type_class.size);
								tmp_ptr = (char*)tmp_coord;
								for(m = 0; m < lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size; m++) {
									memcpy((void*)tmp_ptr,(void*)&(val_md->multidval.type->type_class.default_mis),val_md->multidval.type->type_class.size);
									tmp_ptr = tmp_ptr + val_md->multidval.type->type_class.size;
								}
								_NclWriteCoordVar(
									lhs,
									_NclCreateMultiDVal( 
										NULL, 
										NULL, 
										Ncl_MultiDValData, 
										0, 
										tmp_coord, 
										&val_md->multidval.type->type_class.default_mis, 
										1, 
										&lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size, 
										TEMPORARY, 
										NULL, 
										val_md->multidval.type),
									NrmQuarkToString(rhs->var.dim_info[j].dim_quark),
									NULL);
								_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),&tmp_sel);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
							}
						} 
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
						}
					} else if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1)&&(rhs->var.coord_vars[j] != -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 33\n");
#endif
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						tmp_sel.selection[0]= lhs_sel_ptr->selection[i];
						tmp_sel.selection[0].dim_num = 0;
						if(val_md != NULL) {
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),&tmp_sel);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
						}
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
						}
					} else if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] == -1)&&(rhs->var.coord_vars[j] != -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 34\n");
#endif
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						tmp_sel.selection[0]= lhs_sel_ptr->selection[i];
						tmp_sel.selection[0].dim_num = 0;
						if((val_md != NULL) &&(lhs_n_elem == lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size)) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 35\n");
#endif
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),&tmp_sel);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
						} else if(val_md != NULL)  {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 36\n");
#endif
	/*
	* Missing values need to be filled in
	*/
							tmp_coord = NclMalloc(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size*val_md->multidval.type->type_class.size);
							tmp_ptr = (char*)tmp_coord;
							for(m = 0; m < lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size; m++) {
								memcpy((void*)tmp_ptr,(void*)&(val_md->multidval.type->type_class.default_mis),val_md->multidval.type->type_class.size);
								tmp_ptr = tmp_ptr + val_md->multidval.type->type_class.size;

							}
							_NclWriteCoordVar(
								lhs,
								_NclCreateMultiDVal( 
									NULL, 
									NULL, 
									Ncl_MultiDValData, 
									0, 
									tmp_coord, 
									&val_md->multidval.type->type_class.default_mis, 
									1, 
									&lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_size, 
									TEMPORARY, 
									NULL, 
									val_md->multidval.type),
								NrmQuarkToString(rhs->var.dim_info[j].dim_quark),
								NULL);
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),&tmp_sel);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[j].dim_quark),NULL),cvar);
						}
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
						}
					} else if((lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]!= -1)&&(rhs->var.coord_vars[j] == -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 37\n");
#endif
						NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no coordinate variable for dimension number (%d), destroying coordinate var,  use \"(/../)\" if this is not desired outcome",lhs_sel_ptr->selection[i].dim_num);
						ret = NhlWARNING;
						tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]);
						if(tmp_coord != NULL) {
							_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
						}
						lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] = -1;
					}
				} else {
					if(lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark >0) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 38\n");
#endif
						if(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] != -1){
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 39\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no dimension name or coordinate variable, deleting name of lhs dimension number (%d) and destroying coordinate var,  use \"(/../)\" if this is not desired outcome",lhs_sel_ptr->selection[i].dim_num);
							ret = NhlWARNING;
							tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num]);
							if(tmp_coord != NULL) {
								_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
							}
							lhs->var.coord_vars[lhs_sel_ptr->selection[i].dim_num] = -1;
						} else {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 40\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: lhs has dimension name and rhs doesn't, deleting name of lhs dimension number(%d), use \"(/../)\" if this is not desired outcome",lhs_sel_ptr->selection[i].dim_num);
							ret = NhlWARNING;
							
						}
						lhs->var.dim_info[lhs_sel_ptr->selection[i].dim_num].dim_quark = -1;
					}
				}
}
				i++;
				j++;
			} else {
				i++;
			}
			if(i==lhs->var.n_dims) {
				done =1;
			}
		}
	} else {
/*
* lhs_sel_ptr == NULL &&  rhs_sel_ptr != NULL
*/
#ifdef NCLVARDEBUG
		fprintf(stdout,"(lhs_sel_ptr == NULL)\n");
#endif
		rhs_md = _NclVarValueRead(rhs,rhs_sel_ptr,NULL);
		if(rhs_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred reading %s",((rhs->var.thesym != NULL)?rhs->var.thesym->name:"unknown"));
			return(NhlFATAL);
		}

		if(lhs_md->obj.id != rhs_md->obj.id) {
			ret = _NclAssignToVar(lhs,rhs_md,NULL);
			if(rhs_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)rhs_md);
			}
			if(ret < NhlWARNING) {
				return(ret);
			}
		}
		j=0;
		i= 0;
		done = 0;
		while(!done) {
			switch(rhs_sel_ptr->selection[j].sel_type) {
			case Ncl_VECSUBSCR:
				rhs_n_elem = rhs_sel_ptr->selection[j].u.vec.n_ind;
				break;
			default:
				rhs_n_elem =  (ng_size_t) labs((rhs_sel_ptr->selection[j].u.sub.finish
								- rhs_sel_ptr->selection[j].u.sub.start)
							       / rhs_sel_ptr->selection[j].u.sub.stride) + 1;
				break;
			}

			if((rhs_n_elem != 1)||((rhs_n_elem==1)&&(lhs->var.dim_info[i].dim_size == 1))){
				if(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark > 0) {
#ifdef NCLVARDEBUG
					fprintf(stdout,"case 41\n");
#endif
					if((lhs->var.dim_info[i].dim_quark != rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark)){
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 42\n");
#endif
						if(lhs->var.dim_info[i].dim_quark == -1) {
							_NclWriteDim(lhs,i,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark));
						} else {
/*
* This is a warning condition since names don't match.
*/
							if(lhs->var.coord_vars[i] != -1){
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 43\n");
#endif
								NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs and overwriting coordinate variable, use \"(/../)\" if this change is not desired",i);
								ret = NhlWARNING;
								_NclWriteDim(lhs,i,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark));
								tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[i]);
								if(tmp_coord != NULL) {
									_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
								}
								lhs->var.coord_vars[i] = -1;
							} else {
#ifdef NCLVARDEBUG
								fprintf(stdout,"case 43.5\n");
#endif
								NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: Dimension names for dimension number (%d) don't match, assigning name of rhs dimension to lhs, use \"(/../)\" if this change is not desired",i);
								ret = NhlWARNING;
								_NclWriteDim(lhs,i,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark));
							}
						}
/*
* lhs side has either been freed or never had a coordinate var and right side has one
*/
						if((lhs->var.coord_vars[i] == -1)&&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] != -1)) {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 44\n");
#endif
							tmp_sel.selection[0] = rhs_sel_ptr->selection[j];
							tmp_sel.selection[0].dim_num = 0;
							cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
							val_md = _NclVarValueRead(cvar,NULL,NULL);
							if(val_md != NULL) {
								_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL);
								_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
							}
							if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
								_NclDestroyObj((NclObj)cvar);
							}

						}  
						
					} else if((lhs->var.coord_vars[i] != -1)&&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] != -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 45\n");
#endif
						tmp_sel.selection[0] = rhs_sel_ptr->selection[j];
						tmp_sel.selection[0].dim_num = 0;
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						if(val_md != NULL ) {
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
						}
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
						}
					} else if((lhs->var.coord_vars[i] == -1)&&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] != -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 46\n");
#endif
						tmp_sel.selection[0] = rhs_sel_ptr->selection[j];
						tmp_sel.selection[0].dim_num = 0;
						cvar = _NclReadCoordVar(rhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),&tmp_sel);
						val_md = _NclVarValueRead(cvar,NULL,NULL);
						if(val_md != NULL ) {
							_NclWriteCoordVar(lhs,val_md,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL);
							_NclAttCopyWrite(_NclReadCoordVar(lhs,NrmQuarkToString(rhs->var.dim_info[rhs_sel_ptr->selection[j].dim_num].dim_quark),NULL),cvar);
						}
						if((cvar !=NULL)&&(cvar->obj.status == TEMPORARY)) {
							_NclDestroyObj((NclObj)cvar);
						}
					} else if((lhs->var.coord_vars[i] != -1)&&(rhs->var.coord_vars[rhs_sel_ptr->selection[j].dim_num] == -1)) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 47\n");
#endif
						NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no coordinate variable for dimension number (%d), destroying coordinate var,  use \"(/../)\" if this is not desired outcome",rhs_sel_ptr->selection[j].dim_num);
						ret = NhlWARNING;
						tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[i]);
						if(tmp_coord != NULL) {
							_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
						}
						lhs->var.coord_vars[i] = -1;
					}
					i++;
					j++;
				} else {
					if(lhs->var.dim_info[i].dim_quark >0) {
#ifdef NCLVARDEBUG
						fprintf(stdout,"case 48\n");
#endif
						if(lhs->var.coord_vars[i] != -1){
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 49\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: rhs has no dimension name or coordinate variable, deleting name of lhs dimension number (%d) and destroying coordinate var,  use \"(/../)\" if this is not desired outcome",i);
							ret = NhlWARNING;
							tmp_coord = (void*)_NclGetObj(lhs->var.coord_vars[i]);
							if(tmp_coord != NULL) {
								_NclDelParent((NclObj)tmp_coord,(NclObj)lhs);
							}
							lhs->var.coord_vars[i] = -1;
						} else {
#ifdef NCLVARDEBUG
							fprintf(stdout,"case 50\n");
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"VarVarWrite: lhs has dimension name and rhs doesn't, deleting name of lhs dimension number(%d), use \"(/../)\" if this is not desired outcome",i);
							ret = NhlWARNING;
							
						}
						lhs->var.dim_info[i].dim_quark = -1;
					}
					i++;
					j++;
				}
			} else {
				j++;
			}
			if(j == rhs->var.n_dims) {
				done = 1;
			}
		}
	}

	if((rhs->var.att_id != -1)&&(lhs->var.att_id != -1)&&(lhs->var.att_id != rhs->var.att_id)) {
/*
* Need to merge lists
*/
		tmp_att = (NclAtt)_NclGetObj(rhs->var.att_id);
		att_list = tmp_att->att.att_list;
		for(i = 0; i < tmp_att->att.n_atts; i++) {
			if(_NclIsAtt(lhs->var.att_id,att_list->attname)) {
				if(NrmStringToQuark(att_list->attname) != NrmStringToQuark(NCL_MISSING_VALUE_ATT)) {
					_NclDeleteAtt(lhs->var.att_id,att_list->attname);
					_NclAddAtt(lhs->var.att_id,att_list->attname,att_list->attvalue,NULL);
				}
				else {
					/*
					 * Don't allow the _FillValue of other types to be converted to the logical type.
					 */
					if (lhs_type != Ncl_Typelogical) {
						_NclAddAtt(lhs->var.att_id,att_list->attname,att_list->attvalue,NULL);
					}
				}
				att_list = att_list->next;
			} else {
				_NclAddAtt(lhs->var.att_id,att_list->attname,att_list->attvalue,NULL);
				att_list = att_list->next;
			}
		}
	}else if((rhs->var.att_id != -1)&&(lhs->var.att_id == -1)) {
/*
* Simple case just copy atts
*/
		lhs->var.att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)lhs);
		lhs->var.att_cb = _NclAddCallback((NclObj)_NclGetObj(lhs->var.att_id),(NclObj)lhs,_NclVarMissingNotify,MISSINGNOTIFY,NULL);
		
		tmp_att = (NclAtt)_NclGetObj(rhs->var.att_id);
		att_list = tmp_att->att.att_list;		
		for(i = 0; i < tmp_att->att.n_atts; i++) {
			_NclAddAtt(lhs->var.att_id,att_list->attname,att_list->attvalue,NULL);
			att_list = att_list->next;
		}
		
	}
	return(ret);
}

static NhlErrorTypes VarReplaceCoord(struct _NclVarRec* self,
				     struct _NclMultiDValDataRec* value,
				     char* coord_name,
				     struct _NclSelectionRecord *sel_ptr)
{
	int index;

	index = VarIsADim(self,coord_name);
	if((index>=0)&&(index < self->var.n_dims))
	{
		VarDeleteCoord(self, coord_name);
	}

	return (VarWriteCoord(self, value, coord_name, sel_ptr));
}

NhlErrorTypes _NclReplaceCoordVar(struct _NclVarRec *self,
				  struct _NclMultiDValDataRec *value,
				  char *coord_name,
				  struct _NclSelectionRecord *sel_ptr)
{
	NclVarClass vc;

        if(self == NULL) {
                return(NhlFATAL);
        } else {
                vc = (NclVarClass)self->obj.class_ptr;
        }

        while((NclObjClass)vc != nclObjClass) {
		if(vc->var_class.write_coordinate != NULL) {
			return VarReplaceCoord(self,value,coord_name,sel_ptr);
		} else {
			vc = (NclVarClass)vc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}



/*
 *      $Id: NclVar.h,v 1.11 2010-04-14 21:29:48 huangwei Exp $
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
 *	Date:		Thu Jan 13 15:04:41 MST 1994
 *
 *	Description:	
 */
#ifndef NclVar_h
#define NclVar_h
#include "NclData.h"

typedef NhlErrorTypes (*NclAssignFunction)(
#if	NhlNeedProto
	struct _NclVarRec * /*self*/,
	struct _NclMultiDValDataRec * /* value */,
	struct _NclSelectionRecord * /* sel_ptr */
#endif
);

typedef NhlErrorTypes (*NclAssignVarToVarFunc)(
#if	NhlNeedProto
	struct _NclVarRec * /*lhs*/,
	NclSelectionRecord * /*lhs_sel_ptr*/,
	struct _NclVarRec * /* rhs */,
	NclSelectionRecord * /*rhs_sel_ptr*/
#endif
);

typedef struct _NclVarRec *(*NclReadFunction)(
#if	NhlNeedProto
	struct _NclVarRec * /*self*/,
	struct _NclSelectionRecord * /* sel_ptr */
#endif
);

typedef struct _NclDataRec *(*NclReadValueFunction)(
#if	NhlNeedProto
	struct _NclVarRec * /*self*/,
	struct _NclSelectionRecord * /* sel_ptr */,
	NclScalar * /*new_missing*/
#endif
);

typedef void (*NclVarPrintFunction)(
#if	NhlNeedProto
struct 	_NclObjRec	* /*self*/,
FILE	*		  /*fp*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadAttribute)(
#if	NhlNeedProto
struct  _NclVarRec	* /*self*/,
char	*		/* attname */,
struct 	_NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclWriteAttribute)(
#if	NhlNeedProto
struct  _NclVarRec	* /*self*/,
char	*		/* attname */,
struct  _NclMultiDValDataRec	* /*value */,
struct 	_NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadDimension)(
#if	NhlNeedProto
struct  _NclVarRec	* /*self*/,
char	*		/*dim_name*/,
long			/*dim_num*/
#endif
);

typedef NhlErrorTypes  (*NclWriteDimension)(
#if	NhlNeedProto
struct  _NclVarRec	* /*self*/,
long			/*dim_num*/,
char	*		/*dim_name*/
#endif
);

typedef struct _NclVarRec * (*NclReadCoordinate)(
#if     NhlNeedProto
struct  _NclVarRec 	* /*self*/,
char	*		/* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef NclObjTypes (*NclRepValueFunc) (
#if	NhlNeedProto
struct  _NclVarRec	* /*self */
#endif
);

typedef struct  _NclDataRec * (*NclGetValFunc)(
#if	NhlNeedProto
struct _NclVarRec 	* /*self*/
#endif
);

typedef struct _NclDataRec *(*NclVarCoerceFunc)(
#if     NhlNeedProto
        struct _NclVarRec*     /*self*/,
        NclObjTypes             /*coerce_to_obj*/,
        NclScalar *		/*coerce_to_obj*/
#endif
);

typedef NhlErrorTypes (*NclDeleteCoordinate)(
#if     NhlNeedProto
struct  _NclVarRec 	* /*self*/,
char	*		/* coord_name */
#endif
);

typedef NhlErrorTypes (*NclWriteCoordinate)(
#if     NhlNeedProto
struct  _NclVarRec 	* /*self*/,
struct  _NclMultiDValDataRec 	* /*value*/,
char	*		/* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef int (*NclIsAFunc) (
#if	NhlNeedProto
	struct _NclVarRec * /*self*/,
	char * /*name*/
#endif
);

typedef struct  _NclDimRec * (*NclGetDimInfo)(
#if	NhlNeedProto
struct _NclVarRec 	* /*self*/,
char *			/*dim_name*/,
long			/*dim_num*/
#endif
);

typedef struct _NclVarRec * (*NclCopyVarFunction)(
#if	NhlNeedProto
struct 	_NclVarRec * /*thevar*/,
struct _NclSymbol*	/*new_name*/,
struct 	_NclVarRec*	/* storage */
#endif
);

struct _NclVarRec *_NclVarCreate(
#if	NhlNeedProto
	struct _NclVarRec *	/* inst */,
	struct _NclObjClassRec *	/* theclass */,
	NclObjTypes 	/* obj_type */,
	unsigned int 	/* obj_type_mask */,
	struct _NclSymbol  * /* thesym */,
	struct _NclMultiDValDataRec * /* value */,
	struct _NclDimRec * /*dim_info*/,
	int		/*att_id*/,
	int* 		/*coords*/,
	NclVarTypes /* var_type */,
	char * /*var_name*/,
	NclStatus /*status*/
#endif
);


typedef struct _NclVarClassPart {
	NclRepValueFunc		rep_val;
	NclGetValFunc		get_val;
	NclVarCoerceFunc	var_coerce;
	NclCopyVarFunction	copy_var;
	

	NclAssignFunction	write_func;
	NclAssignVarToVarFunc	write_vv_func;
	NclReadFunction		read_func;
	NclReadValueFunction	read_val_func;
	
	NclReadAttribute	read_att_func;	
	NclIsAFunc		is_att_func;	
	NclWriteAttribute	write_att_func;	

	NclIsAFunc		is_dim_func;	
	NclReadDimension 	read_dim_func;	
	NclGetDimInfo   	get_dim_info;	
	NclWriteDimension 	write_dim_func;	

	NclIsAFunc		is_coord_func;	
	NclReadCoordinate	read_coordinate;
	NclWriteCoordinate	write_coordinate;
	NclDeleteCoordinate	delete_coordinate;
} NclVarClassPart;

typedef struct _NclVarPart {
	NclVarTypes	var_type;
	int	var_quark;
	struct _NclSymbol* thesym;
	int thevalue_id;
	int n_dims;
	NclDimRec  dim_info[NCL_MAX_DIMENSIONS];
	int				att_id;
	_NhlCB				att_cb;
	int coord_vars[NCL_MAX_DIMENSIONS];
	NclSelectionRecord *sel_rec;
	NclObj ref_var;  /* the referenced variable: used only inside func_proc calls */
}NclVarPart;
 
typedef struct _NclVarClassRec{
	NclObjClassPart	obj_class;
	NclVarClassPart var_class;
}NclVarClassRec;

typedef struct _NclVarRec {
	NclObjPart      obj;
	NclVarPart	var;
}NclVarRec;

typedef NclVarRec *NclVar;
typedef NclVarClassRec *NclVarClass;

extern NclObjClass nclVarClass;

extern NclVarClassRec nclVarClassRec;

extern NhlErrorTypes VarPrint(NclObj theobj,FILE *fp);
extern NhlErrorTypes VarPrintVarSummary(NclObj theobj,FILE *fp);

void _NclVarMissingNotify(
#if     NhlNeedProto
NhlArgVal /*cbdata*/,
NhlArgVal /*udata*/
#endif
);

NhlErrorTypes _NclReplaceCoordVar(struct _NclVarRec *self,
                                  struct _NclMultiDValDataRec *value,
                                  char *coord_name,
                                  struct _NclSelectionRecord *sel_ptr);
#endif /* NclVar_h */

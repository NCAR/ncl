
/*
 *      $Id: NclFile.h,v 1.1 1994-07-14 20:46:31 ethan Exp $
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
 *	Date:		Thu Jan 13 14:55:29 MST 1994
 *
 *	Description:	
 */
#ifndef NclFile_h
#define NclFile_h

#include "NclData.h"

typedef struct _NclFileRec NclFileRec;
typedef struct _NclFileClassRec NclFileClassRec;
typedef NclFileRec *NclFile;
typedef NclFileClassRec *NclFileClass;

typedef NclObjTypes (*NclFileVarRepValueFunc)(
#ifdef NhlNeedProto
NclFile	/* thefile */,
NclQuark /* var */
#endif
);

typedef struct _NclMultiDValDataRec* (*NclFileVarCoerceFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclObjTypes             /*coerce_to_obj*/,
NclScalar *             /*new_missing*/
#endif
);

typedef int (*NclFileVarIsAFunc)(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* var */,
NclQuark /* name */
#endif
);

typedef int (*NclFileIsAFunc)(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

typedef NhlErrorTypes (*NclAssignFileVarFunc)(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

typedef struct _NclVarRec* (*NclGetFileVarFunc)(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec* (*NclGetFileVarValFunc)(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadVarAttributeFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */, 
NclQuark /* var */,
NclQuark /* attname */, 
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclWriteVarAttributeFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadAttributeFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

typedef NhlErrorTypes (*NclWriteAttributeFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadVarDimensionFunc)(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

typedef NhlErrorTypes (*NclWriteVarDimensionFunc)(
#ifdef NhlNeedProto
NclFile /*thefile*/, 
NclQuark /* var */,
NclQuark /*dim_name*/, 
long /*dim_num */
#endif
);

typedef struct _NclMultiDValDataRec * (*NclReadDimensionFunc)(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

typedef NhlErrorTypes (*NclWriteDimensionFunc)(
#ifdef NhlNeedProto
NclFile /*thefile*/, 
NclQuark /*dim_name*/, 
long /*dim_num */
#endif
);

typedef struct _NclVarRec * (*NclReadFileCoordFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

typedef NhlErrorTypes (*NclWriteFileCoordFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);


typedef NhlErrorTypes (*NclAssignFileVarVarFunc)(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /*lhs_var*/,
struct _NclSelectionRecord* /*lhs_sel_ptr*/,
struct _NclVarRec* /* rhs_var*/,
struct _NclSelectionRecord* /*rhs_sel_ptr*/
#endif
);

typedef struct _NclFileClassPart {

	NclFileVarRepValueFunc	rep_val;
	NclFileIsAFunc		is_var;
	NclAssignFileVarFunc	write_var;
	NclAssignFileVarVarFunc	write_var_var;
	NclGetFileVarFunc 	read_var_func;
	NclGetFileVarValFunc    read_var_val_func;
	NclFileIsAFunc		is_att;
	NclReadAttributeFunc	read_att_func;	
	NclWriteAttributeFunc	write_att_func;	
	NclFileVarIsAFunc		is_var_att;
	NclReadVarAttributeFunc	read_var_att_func;	
	NclWriteVarAttributeFunc	write_var_att_func;	
	NclFileIsAFunc		is_dim;
	NclFileVarIsAFunc		is_var_dim;
	NclReadVarDimensionFunc 	read_var_dim_func;	
	NclWriteVarDimensionFunc 	write_var_dim_func;	
	NclReadDimensionFunc 	read_dim_func;	
	NclWriteDimensionFunc 	write_dim_func;	
	NclFileIsAFunc		is_coord;
	NclReadFileCoordFunc	read_coord_func;
	NclWriteFileCoordFunc	write_coord_func;
} NclFileClassPart;

typedef struct _NclFileAttInfoList {
	struct _NclFAttRec*	the_att;
	struct _NclFileAttInfoList * next;
} NclFileAttInfoList;
typedef struct _NclFilePart {
	NclQuark	fname;
	NclQuark	fpath;
	int		wr_status;
	int		file_type;
	int		n_vars;
	struct _NclFVarRec 	*var_info[NCL_MAX_FVARS];
	NclFileAttInfoList *var_att_info[NCL_MAX_FVARS];
	int 		var_att_ids[NCL_MAX_FVARS];

	int 	   	n_file_dims;
	struct _NclFDimRec  	*file_dim_info[NCL_MAX_DIMENSIONS];
	struct _NclFVarRec	*coord_vars[NCL_MAX_DIMENSIONS];

	int		n_file_atts;
	struct _NclFAttRec	*file_atts[NCL_MAX_FVARS];
	int 		file_atts_id;
	struct _NclFormatFunctionRecord *format_funcs;
	void	*private_rec;
}NclFilePart;
 
struct _NclFileClassRec{
	NclObjClassPart	obj_class;
	NclFileClassPart file_class;
};

struct _NclFileRec {
	NclObjPart      obj;
	NclFilePart	file;
};


extern NclObjClass nclFileClass;

extern NclFileClassRec nclFileClassRec;

extern NclFile _NclCreateFile(
#ifdef NhlNeedProto 
	NclObj	/* inst */,
	NclObjClass /*theclass*/,
	NclObjTypes /* obj_type */,
	unsigned int /*obj_type_mask*/,
	NclStatus /*status*/,
	NclQuark /*path */,
	int 	/*rw_status*/
#endif
);

#endif /* NclVar_h */

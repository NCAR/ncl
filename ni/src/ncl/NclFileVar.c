

/*
 *      $Id: NclFileVar.c,v 1.4 1994-12-23 01:18:23 ethan Exp $
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
#include "NclFile.h"
#include "NclFileInterfaces.h"
#include "NclFileVar.h"
#include "FileSupport.h"
#include "DataSupport.h"
#include "AttSupport.h"
#include "VarSupport.h"

static int FileVarIsACoord(
#if NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*coordname*/
#endif
);

static struct _NclVarRec *FileVarReadCoord(
#if	NhlNeedProto
struct _NclVarRec * /*self*/,
char *  /*coord_name*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static NhlErrorTypes FileVarWriteCoord(
#if	NhlNeedProto
struct _NclVarRec * /*self*/,
struct _NclMultiDValDataRec * /*value*/,
char * 	/*coord_name*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);


static void	FileVarDestroy(
#if	NhlNeedProto
struct  _NclObjRec*	/*self*/
#endif
);

static struct _NclMultiDValDataRec  *FileVarReadAtt(
#if	NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*attname */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static NhlErrorTypes FileVarWriteAtt(
#if	NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*attname */,
struct _NclMultiDValDataRec * /*value*/,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

static int FileVarIsAAtt (
#if  	NhlNeedProto
struct _NclVarRec * /*self*/,
char* /*attname*/
#endif
);

static int FileVarIsADim(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*attname */
#endif
);

static struct _NclMultiDValDataRec *FileVarReadDim(
#if	NhlNeedProto
struct _NclVarRec * /* self */,
char * /*dim_name*/,
long /*dim_num */
#endif
);

static struct _NclDimRec *FileVarGetDimInfo(
#if 	NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*dim_name*/,
long /* dim_num */
#endif
);

static NhlErrorTypes FileVarWriteDim(
#if	NhlNeedProto
struct _NclVarRec *  /*self*/,
long 		/*dim_num*/,
char * 		/*dim_name */
#endif
);

static void	FileVarPrint
#if	NhlNeedProto
(NclObj theobj,FILE *fp)
#else
(theobj,fp)
NclObj theobj;
FILE *fp;
#endif
{
	NclVar thevar = (NclVar) theobj;
	NclFile thefile;
	NclMultiDValData theval;

	theval = (NclMultiDValData)_NclGetObj(thevar->var.thevalue_id);
	if(theval != NULL) {
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
		if(thefile != NULL) {
			_NclPrint((NclObj)thefile,fp);
		}
	}
	return;
}

NclFileVarClassRec nclFileVarClassRec = {
	{
		"NclVarClass",
		sizeof(NclVarRec),
		(NclObjClass)&nclVarClassRec,
		0,
		(NclGenericFunction)FileVarDestroy,
		(NclSetStatusFunction)NULL /*VarSetStatus*/,
		(NclInitPartFunction)NULL,
		(NclInitClassFunction)NULL,
		(NclAddParentFunction)NULL,
                (NclDelParentFunction)NULL,
/* NclPrintFunction print */	FileVarPrint
	},
	{
/* NclRepValueFunc rep_val */		NULL,
/* NclGetValFunc get_val*/		NULL,
/* NclVarCoerceFunc */			NULL,
/* NclCopyVarFunc */			NULL,

/* NclAssignFunction write_func */	NULL,
/* NclAssignVarToVarFunction write_vv_func */	NULL,
/* NclReadFunction read_func */		NULL,
/* NclReadValueFunction read_func */	NULL,

/* NclReadAttribute read_att_func*/	FileVarReadAtt,
/* NclIsA is_att_func*/			FileVarIsAAtt,
/* NclWriteAttribute write_att_func*/	FileVarWriteAtt,

/* NclIsAFunc is_dim_func */		FileVarIsADim,
/* NclReadDimension read_dim_func*/	FileVarReadDim,
/* NclGetDimInfo read_dim_func*/	FileVarGetDimInfo,
/* NclWriteDimension write_dim_func*/	FileVarWriteDim,

/* NclIsAFunc is_coord_func */		FileVarIsACoord,
/* NclReadCoordinate read_coordinate*/	FileVarReadCoord,
/* NclWriteCoordinate write_coordinate*/ FileVarWriteCoord
	},
	{
		NULL
	}
};

NclObjClass nclFileVarClass = (NclObjClass)&nclFileVarClassRec;





struct _NclVarRec *_NclFileVarCreate
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
char *var_name)
#else
(inst,theclass,obj_type,obj_type_mask,thesym,value,dim_info,att_id,coords,var_type,var_name)
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
#endif
{
	NclFileVar fvar = NULL;
	NclFile thefile;

	thefile = (NclFile)_NclGetObj(*(int*)value->multidval.val);
	if(inst != NULL) {
		fvar = (NclFileVar) inst;
	} else {
		fvar = (NclFileVar) NclMalloc(sizeof(NclFileVarRec));
	}
	if(theclass != NULL) {
		_NclVarCreate((NclVar)fvar,theclass,obj_type,obj_type_mask | Ncl_FileVar,thesym,value,dim_info,att_id,coords,var_type,var_name);
	} else {
		_NclVarCreate((NclVar)fvar,(NclObjClass)&nclFileVarClassRec,obj_type,obj_type_mask | Ncl_FileVar,thesym,value,dim_info,att_id,coords,var_type,var_name);
	}
	_NclAddParent((NclObj)thefile,(NclObj)fvar);
	return((NclVar)fvar);
}

static void FileVarDestroy
#if	NhlNeedProto
(struct _NclObjRec*	self)
#else
(self)
struct _NclObjRec*	self;
#endif
{
	int i;
	NclVar self_var = (NclVar)self;
	NclMultiDValData value = (NclMultiDValData)_NclGetObj(self_var->var.thevalue_id);
	NclFile thefile = (NclFile)_NclGetObj(*(int*)value->multidval.val);


	_NclUnRegisterObj((NclObj)self_var);
	_NclDelParent((NclObj)thefile,self);
	for(i = 0; i< self_var->var.n_dims; i++ ) {
		if(self_var->var.coord_vars[i] != -1) {
			_NclDelParent(_NclGetObj(self_var->var.coord_vars[i]),self);
		}
	}

	
	if((value != NULL)&&(value->obj.class_ptr->obj_class.destroy != NULL)) {
		_NclDelParent((NclObj)value,self);
	}
	NclFree(self_var);
	return;
}




static NhlErrorTypes FileVarWriteAtt
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
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(attname != NULL)) {
		return(_NclFileWriteAtt(thefile,NrmStringToQuark(attname),value,sel_ptr));
	} else {
		return(NhlFATAL);
	}
	
	
	
}

static struct _NclMultiDValDataRec *FileVarReadAtt
#if	NhlNeedProto
(struct _NclVarRec *self, char *attname, struct _NclSelectionRecord *sel_ptr)
#else
(self, attname, sel_ptr)
struct _NclVarRec *self;
char *attname;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(attname != NULL)) {
		return(_NclFileReadAtt(thefile,NrmStringToQuark(attname),sel_ptr));
	} else {
		return(NULL);
	}
}

static int FileVarIsAAtt
#if	NhlNeedProto
(struct _NclVarRec * self, char* attname)
#else
(self, attname)
struct _NclVarRec *self;
char* attname;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(attname != NULL)) {
		if(_NclFileIsAtt(thefile,NrmStringToQuark(attname)) != -1) {
			return(1);
		} else {
			return(0);
		}
	} else {
		return(0);
	}
}

static int FileVarIsADim
#if	NhlNeedProto
(struct _NclVarRec * self, char * dimname)
#else
(self, dimname)
struct _NclVarRec * self;
char * dimname;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;
	int index;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(dimname != NULL)) {
		index = _NclFileIsDim(thefile,NrmStringToQuark(dimname));
		if(index != -1) {
			return(1);
		}
	}
	return(0);
}

static struct _NclMultiDValDataRec *FileVarReadDim
#if	NhlNeedProto
(struct _NclVarRec *self, char *dim_name, long dim_num)
#else
(self, dim_name, dim_num)
struct _NclVarRec *self;
char *dim_name;
long dim_num;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if(thefile != NULL) {
		return(_NclFileReadDim(thefile,(dim_name == NULL ? -1 : NrmStringToQuark(dim_name)),dim_num));
	}
	return(NULL);
}

static struct _NclDimRec *FileVarGetDimInfo
#if 	NhlNeedProto
(struct _NclVarRec *self, char *dim_name, long dim_num)
#else
(self, dim_name, dim_num)
struct _NclVarRec *self;
char *dim_name;
long dim_num;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;
	struct _NclDimRec * thedim = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if(thefile != NULL) {
		thedim = (NclDimRec*)NclMalloc(sizeof(NclDimRec));
		if(dim_name != NULL) {
			thedim->dim_num = _NclFileIsDim(thefile,NrmStringToQuark(dim_name));
			if(thedim->dim_num == -1) {	
				NclFree(thedim);
				return(NULL);
			}
			thedim->dim_size = thefile->file.file_dim_info[(int)thedim->dim_num]->dim_size;
			thedim->dim_quark = NrmStringToQuark(dim_name);
			return(thedim);
		} else {
			if((dim_num > 0)&&(dim_num < thefile->file.n_file_dims)) {
				thedim->dim_num = dim_num;
				thedim->dim_quark = thefile->file.file_dim_info[dim_num]->dim_name_quark;	
				thedim->dim_size = thefile->file.file_dim_info[dim_num]->dim_size;
				return(thedim);
			} else {
				NclFree(thedim);
				return(NULL);
			}
		}
	}
}

static NhlErrorTypes FileVarWriteDim
#if	NhlNeedProto
(struct _NclVarRec *self, long dim_num, char *dim_name)
#else
(self, dim_num, dim_name)
struct _NclVarRec *self;
long dim_num;
char *dim_name;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if(thefile != NULL) {
		return(_NclFileWriteDim(thefile,(dim_name == NULL ? -1 : NrmStringToQuark(dim_name)),dim_num));
	}
	return(NULL);
}

static int FileVarIsACoord
#if	NhlNeedProto
(struct _NclVarRec *self, char *coordname)
#else
(self, coordname)
struct _NclVarRec *self;
char *coordname;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;
	int index ;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(coordname != NULL)) {
		index = _NclFileVarIsCoord(thefile,NrmStringToQuark(coordname));
		if(index != -1) {
			return(1);
		} else {
			return(0);
		}
	}
	return(0);
}

static struct _NclVarRec *FileVarReadCoord
#if	NhlNeedProto
(struct _NclVarRec *self, char *coord_name, struct _NclSelectionRecord *sel_ptr)
#else
(self, coord_name, sel_ptr)
struct _NclVarRec *self;
char *coord_name;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(coord_name != NULL)) {
		return(_NclFileReadCoord(thefile,NrmStringToQuark(coord_name),sel_ptr));
	}
	return(NULL);
		
}

static NhlErrorTypes FileVarWriteCoord
#if	NhlNeedProto
(struct _NclVarRec *self, struct _NclMultiDValDataRec *value, char *coord_name, struct _NclSelectionRecord *sel_ptr)
#else
(self, value, coord_name, sel_ptr)
struct _NclVarRec *self;
struct _NclMultiDValDataRec *value;
char *coord_name;
struct _NclSelectionRecord *sel_ptr;
#endif
{
	NclFile thefile = NULL;
	NclMultiDValData theval = NULL;

	theval = (NclMultiDValData)_NclGetObj(self->var.thevalue_id);
	if(theval != NULL) 
		thefile = (NclFile)_NclGetObj(*(int*)theval->multidval.val);
	if((thefile != NULL)&&(coord_name != NULL)) {
		return(_NclFileWriteCoord(thefile,NrmStringToQuark(coord_name),value,sel_ptr));
	}
	return(NhlFATAL);
}

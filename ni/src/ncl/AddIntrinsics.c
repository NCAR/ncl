

/*
 *      $Id: AddIntrinsics.c,v 1.10 1994-12-23 01:17:15 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		AddIntrinsics.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Feb 7 11:07:41 MST 1994
 *
 *	Description:	
 */
#ifdef __cpluplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "parser.h"

extern NhlErrorTypes _NclINhlDataToNDC(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclINhlNDCToData(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIAddFile(
#if	NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclIClear(
#if	NhlNeedProto
void
#endif 
);
extern NhlErrorTypes  _NclIUpdate(
#if	NhlNeedProto
void
#endif 
);
extern NhlErrorTypes  _NclIFrame(
#if	NhlNeedProto
void
#endif 
);
extern NhlErrorTypes  _NclIPrint(
#if	NhlNeedProto
void
#endif 
);
extern NhlErrorTypes  _NclIDelete(
#if	NhlNeedProto
void
#endif
);
extern NhlErrorTypes  _NclIDraw(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes  _NclIDestroy(
#if	NhlNeedProto
void
#endif
);
extern NhlErrorTypes  _NclIDumpStk(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes  _NclISizeOf(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes  _NclIDimSizes(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes  _NclIAny(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes  _NclIAll(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIAddToOverlay(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIIsMissing(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIListVariables(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIListFiles(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIListFuncs(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIListFileVariables(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIListHLUObjs(
#if	NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIPSetRL(
#if	NhlNeedProto
void
#endif
);

void _NclAddIntrinsics
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	NclArgTemplate *args;

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIPrint,args,"print",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDelete,args,"delete",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDraw,args,"draw",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDestroy,args,"destroy",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIUpdate,args,"update",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIFrame,args,"frame",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIClear,args,"clear",1,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIDumpStk,args,"dump",1,PIPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("integer");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIPSetRL,args,"setrl",1,PIPROC);
	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("string");
	args[1].is_dimsizes = 1;
	args[1].dim_sizes[0] = 1;
	args[1].n_dims = 1;
	_NclRegisterProc(_NclIAddFile,args,"addfile",2,IFUNC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterFunc(_NclISizeOf,args,"sizeof",1,IFUNC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	_NclRegisterFunc(_NclIDimSizes,args,"dimsizes",1,IFUNC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("logical");
	args[0].is_dimsizes = 0;
	_NclRegisterFunc(_NclIAny,args,"any",1,IFUNC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("logical");
	args[0].is_dimsizes = 0;
	_NclRegisterFunc(_NclIAll,args,"all",1,IFUNC);
	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
	args[1].arg_data_type = NULL;
	args[1].is_dimsizes = 0;
	_NclRegisterProc(_NclIAddToOverlay,args,"overlay",2,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIIsMissing,args,"ismissing",1,IFUNC);
	args = NclCalloc(5,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("float");
	args[1].is_dimsizes = 0;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("float");
	args[2].is_dimsizes = 0;
	args[2].n_dims = 1;
	args[3].arg_data_type = _NclLookUp("float");
	args[3].is_dimsizes = 0;
	args[3].n_dims = 1;
	args[4].arg_data_type = _NclLookUp("float");
	args[4].is_dimsizes = 0;
	args[4].n_dims = 1;
	_NclRegisterProc(_NclINhlDataToNDC,args,"datatondc",5,IPROC);
	args = NclCalloc(5,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	args[1].arg_data_type = _NclLookUp("float");
	args[1].is_dimsizes = 0;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("float");
	args[2].is_dimsizes = 0;
	args[2].n_dims = 1;
	args[3].arg_data_type = _NclLookUp("float");
	args[3].is_dimsizes = 0;
	args[3].n_dims = 1;
	args[4].arg_data_type = _NclLookUp("float");
	args[4].is_dimsizes = 0;
	args[4].n_dims = 1;
	_NclRegisterProc(_NclINhlNDCToData,args,"ndctodata",5,IPROC);
	_NclRegisterProc(_NclIListVariables,NULL,"list_vars",0,IPROC);
	_NclRegisterProc(_NclIListFiles,NULL,"list_files",0,IPROC);
	_NclRegisterProc(_NclIListFuncs,NULL,"list_procfuncs",0,IPROC);
	_NclRegisterProc(_NclIListHLUObjs,NULL,"list_hlus",0,IPROC);
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("file");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIListFileVariables,args,"list_filevars",1,IPROC);
	return;
}


#ifdef __cpluplus
}
#endif

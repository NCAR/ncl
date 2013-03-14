
/*
 *      $Id: AddIntrinsics.c,v 1.35 2010-04-14 21:29:47 huangwei Exp $
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

extern NhlErrorTypes _NclIAddToOverlayAfter(
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

extern NhlErrorTypes _NclIGetFileVarNames(
#if     NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclIGetFileGrpNames(void);

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

NhlErrorTypes _NclIqsort(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIbsearch(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbindirwrite(
#if     NhlNeedProto
void
#endif
);

NhlErrorTypes _NclIcbinread(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbinread(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbinrecwrite(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbindirread(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbinrecread(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclICrayBinRecRead(
#if     NhlNeedProto
void
#endif
);
extern NhlErrorTypes _NclICrayBinNumRec(
#if NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbinnumrec(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIasciiwrite(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIasciiread(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIchngdir(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIcbinwrite(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIfbinwrite(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIsleep(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIprompt(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIrand(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIsrand(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIabs(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIgetenv(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIncargpath(
#if     NhlNeedProto
void
#endif
);
NhlErrorTypes _NclIncargversion(
#if     NhlNeedProto
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
	_NclRegisterProc(_NclIDraw,args,"NhlDraw",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDraw,args,"draw",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDestroy,args,"NhlDestroy",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIDestroy,args,"destroy",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIUpdate,args,"NhlUpdateWorkstation",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIUpdate,args,"update",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIFrame,args,"NhlFrame",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIFrame,args,"frame",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 0;
	_NclRegisterProc(_NclIClear,args,"NhlClearWorkstation",1,IPROC);

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
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("graphic");
	args[1].is_dimsizes = 1;
	args[1].dim_sizes[0] = 1;
	args[1].n_dims = 1;
	_NclRegisterProc(_NclIAddToOverlay,args,"overlay",2,IPROC);

	args = NclCalloc(3,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("graphic");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("graphic");
	args[1].is_dimsizes = 1;
	args[1].dim_sizes[0] = 1;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("graphic");
	args[2].is_dimsizes = 1;
	args[2].dim_sizes[0] = 1;
	args[2].n_dims = 1;
	_NclRegisterProc(_NclIAddToOverlayAfter,args,"NhlAddOverlay",3,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = NULL;
	args[0].is_dimsizes = 0;
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
	_NclRegisterProc(_NclINhlDataToNDC,args,"NhlDataToNDC",5,IPROC);

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
	_NclRegisterProc(_NclINhlNDCToData,args,"NhlNDCToData",5,IPROC);

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

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("file");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIGetFileVarNames,args,"getfilevarnames",1,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("file");
	args[0].is_dimsizes = 1;
	args[0].dim_sizes[0] = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIGetFileGrpNames,args,"getfilegrpnames",1,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("numeric");
	args[0].is_dimsizes = 0;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIqsort,args,"qsort",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].is_dimsizes = 0;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIqsort,args,"sqsort",1,IPROC);

	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("numeric");
	args[0].is_dimsizes = 0;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].dim_sizes[0] = 1;
	args[1].is_dimsizes = 1;
	args[1].n_dims = 1;
	_NclRegisterFunc(_NclIbsearch,args,"bsearch",2,IFUNC);

	args = NclCalloc(3,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].is_dimsizes = 0;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("string");
	args[2].is_dimsizes = 1;
	args[2].dim_sizes[0] = 1;
	args[2].n_dims = 1;
	_NclRegisterFunc(_NclIcbinread,args,"cbinread",3,IFUNC);

	args = NclCalloc(4,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("integer");
	args[1].dim_sizes[0] = 1;
	args[1].is_dimsizes = 1;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("numeric");
	args[2].is_dimsizes = 1;
	args[2].n_dims = 1;
	args[2].dim_sizes[0] = -1;
	args[3].arg_data_type = _NclLookUp("string");
	args[3].is_dimsizes = 1;
	args[3].dim_sizes[0] = 1;
	args[3].n_dims = 1;
	_NclRegisterFunc(_NclIfbindirread,args,"fbindirread",4,IFUNC);

	args = NclCalloc(4,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("integer");
	args[1].dim_sizes[0] = 1;
	args[1].is_dimsizes = 1;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("numeric");
	args[2].is_dimsizes = 1;
	args[2].n_dims = 1;
	args[2].dim_sizes[0] = -1;
	args[3].arg_data_type = _NclLookUp("string");
	args[3].is_dimsizes = 1;
	args[3].dim_sizes[0] = 1;
	args[3].n_dims = 1;
	_NclRegisterFunc(_NclIfbinrecread,args,"fbinrecread",4,IFUNC);

	args = NclCalloc(3,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("integer");
	args[1].dim_sizes[0] = 1;
	args[1].is_dimsizes = 1;
	args[1].n_dims = 1;
	args[2].arg_data_type = NULL;
	args[2].is_dimsizes = 0;
	_NclRegisterFunc(_NclIfbinrecwrite,args,"fbinrecwrite",3,IPROC);
	
	args = NclCalloc(4,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("integer");
	args[1].dim_sizes[0] = 1;
	args[1].is_dimsizes = 1;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("numeric");
	args[2].is_dimsizes = 0;
	args[2].n_dims = 1;
	args[3].arg_data_type = _NclLookUp("string");
	args[3].is_dimsizes = 1;
	args[3].dim_sizes[0] = 1;
	args[3].n_dims = 1;
	_NclRegisterFunc(_NclICrayBinRecRead,args,"craybinrecread",4,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclICrayBinNumRec,args,"craybinnumrec",1,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIfbinnumrec,args,"fbinnumrec",1,IFUNC);


	args = NclCalloc(3,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].is_dimsizes = 0;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("string");
	args[2].is_dimsizes = 0;
	args[2].dim_sizes[0] = 1;
	args[2].n_dims = 1;
	_NclRegisterFunc(_NclIfbinread,args,"fbinread",3,IFUNC);

	args = NclCalloc(3,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].is_dimsizes = 0;
	args[1].n_dims = 1;
	args[2].arg_data_type = _NclLookUp("string");
	args[2].dim_sizes[0] = 1;
	args[2].is_dimsizes = 1;
	args[2].n_dims = 1;
	_NclRegisterFunc(_NclIasciiread,args,"asciiread",3,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIchngdir,args,"chngdir",1,IPROC);

	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].is_dimsizes = 0;
	_NclRegisterProc(_NclIfbindirwrite,args,"fbindirwrite",2,IPROC);

	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].is_dimsizes = 0;
	_NclRegisterProc(_NclIcbinwrite,args,"cbinwrite",2,IPROC);

	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = NULL;
	args[1].is_dimsizes = 0;
	_NclRegisterProc(_NclIasciiwrite,args,"asciiwrite",2,IPROC);

	args = NclCalloc(2,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	args[1].arg_data_type = _NclLookUp("numeric");
	args[1].is_dimsizes = 0;
	_NclRegisterProc(_NclIfbinwrite,args,"fbinwrite",2,IPROC);

	_NclRegisterFunc(_NclIrand,NULL,"rand",0,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("integer");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIsrand,args,"srand",1,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("integer");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterProc(_NclIsleep,args,"sleep",1,IPROC);
	
	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIprompt,args,"prompt",1,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIncargpath,args,"ncargpath",1,IFUNC);

	_NclRegisterProc(_NclIncargversion,NULL,"ncargversion",0,IPROC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("string");
	args[0].dim_sizes[0] = 1;
	args[0].is_dimsizes = 1;
	args[0].n_dims = 1;
	_NclRegisterFunc(_NclIgetenv,args,"getenv",1,IFUNC);

	args = NclCalloc(1,sizeof(NclArgTemplate));
	args[0].arg_data_type = _NclLookUp("snumeric");
	args[0].is_dimsizes = 0;
	_NclRegisterFunc(_NclIabs,args,"abs",1,IFUNC);
	
	return;
}


#ifdef __cpluplus
}
#endif

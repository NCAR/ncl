
/*
 *      $Id: NclApi.h,v 1.6 1994-12-23 01:18:06 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		NclApi.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Aug 17 13:43:10 MDT 1993
 *
 *	Description:	
 */
#ifndef _NCNclApi_h
#define _NCNclApi_h

#ifdef __cplusplus
extern "C" {
#endif

#define NCLAPI_TMP_VAR "__N_C_L__T_M_P__"
#define NCLAPI_DEL_TMP_VAR "delete(__N_C_L__T_M_P__)"

extern int NclInitServer(
#if	NhlNeedProto
NhlErrorTypes /*error_level*/
#endif
);

extern void NclCloseServer(
#if	NhlNeedProto
void
#endif
);

extern int NclSubmitBlock1(
#if	NhlNeedProto
	char *	/*script*/,
	int	/*script_size*/
#endif
);

extern int NclSubmitBlock2(
#if	NhlNeedProto
	char *script[]
#endif
);

extern int NclSubmitCommand(
#if	NhlNeedProto
	char * /*command*/
#endif
);

extern void NclPrintErrorMsgs(
#if	NhlNeedProto
	void
#endif
);

extern int NclGetErrorId(
#if	NhlNeedProto
void
#endif
);

extern struct _NclApiDataList* NclGetProcFuncList(
#if NhlNeedProto
void
#endif
);

extern struct _NclApiDataList* NclGetFileList(
#if NhlNeedProto
void
#endif
);

extern struct _NclApiDataList* NclGetFileVarsList(
#if NhlNeedProto
NclQuark /*filevar*/
#endif
);

extern struct _NclApiDataList* NclGetHLUObjsList(
#if NhlNeedProto
void
#endif
);

extern struct _NclApiDataList* NclGetNewHLUObjsList(
#if NhlNeedProto
void
#endif
);

extern struct _NclApiDataList* NclGetDelHLUObjsList(
#if NhlNeedProto
void
#endif
);

extern int NclGetHLUObjId(
#if NhlNeedProto
char * /*var_name*/
#endif
);

extern struct _NclApiDataList* NclGetVarList(
#if NhlNeedProto
void
#endif
);

extern void NclFreeDataList(
#if NhlNeedProto
NclApiDataList* /*tmp;*/
#endif
);

struct _NclExtValueRec {
	int type;
	int constant;
	void *value;
	int elem_size;
	int totalelements;
	int n_dims;
	int dim_sizes[NCL_MAX_DIMENSIONS];
};

typedef struct _NclExtValueRec NclExtValueRec;

extern NclExtValueRec *NclGetVarValue(
#if NhlNeedProto
char * /*var_name*/,
int /*copy_data*/
#endif
);
extern NclExtValueRec *NclGetExprValue(
#if NhlNeedProto
char * /*expression*/
#endif
);

extern void NclFreeExtValue(
#if NhlNeedProto
NclExtValueRec * /*val*/
#endif
);

#ifdef __cplusplus
}
#endif
#endif  /*_NCNclApi_h */

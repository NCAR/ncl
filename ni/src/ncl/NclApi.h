
/*
 *      $Id: NclApi.h,v 1.4 1994-11-07 03:02:16 ethan Exp $
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
#ifdef NhlNeedProto
NhlErrorTypes /*error_level*/
#endif
);

extern void NclCloseServer(
#ifdef NhlNeedProto
void
#endif
);

extern int NclSubmitBlock1(
#ifdef NhlNeedProto
	char *	/*script*/,
	int	/*script_size*/
#endif
);

extern int NclSubmitBlock2(
#ifdef NhlNeedProto
	char *script[]
#endif
);

extern int NclSubmitCommand(
#ifdef NhlNeedProto
	char * /*command*/
#endif
);

extern void NclPrintErrorMsgs(
#ifdef NhlNeedProto
	void
#endif
);

extern int NclGetErrorId(
#ifdef NhlNeedProto
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

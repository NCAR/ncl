
/*
 *      $Id: NclApi.h,v 1.18 1997-02-27 20:18:45 boote Exp $
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

extern void NclResetServer(
#if NhlNeedProto
void
#endif
);


extern int NclInitServer(
#if	NhlNeedProto
void
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

extern struct _NclExtValueRec *NclGetHLUObjId(
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

typedef union _NclApiScalar {
        double  doubleval;
        float   floatval;
        int     intval;
        long    longval;
        short   shortval;
        char    charval;
        string  stringval;
        byte    byteval;
        logical logicalval;
        obj     objval;
}NclApiScalar;

#define NCLAPI_none	0
#define NCLAPI_short 	01
#define NCLAPI_int 	02
#define NCLAPI_long 	04
#define NCLAPI_float 	010
#define NCLAPI_double 	020
#define NCLAPI_char 	040
#define NCLAPI_byte 	0100
#define NCLAPI_string 	0200
#define NCLAPI_numeric 	0400
#define NCLAPI_logical 	01000
#define NCLAPI_obj 	02000


struct _NclExtValueRec {
	int type;
	int constant;
	void *value;
	int 	has_missing;
	NclApiScalar missing;
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


extern NclQuark *NclGetFileSymNames(
#if NhlNeedProto
int * /*num_names*/
#endif
);

extern NclQuark *NclGetVarSymNames(
#if NhlNeedProto
int * /*num_names*/
#endif
);

extern NclQuark *NclGetHLUVarSymNames(
#if NhlNeedProto
int * /*num_names*/
#endif
);

extern NclQuark *NclGetProcFuncSymNames(
#if NhlNeedProto
int * /*num_names*/
#endif
);

extern struct _NclApiDataList* NclGetFileInfo(
#if NhlNeedProto
NclQuark  /*file_sym_name*/
#endif
);

extern struct _NclExtValueRec *NclReadFileAtt(
#if NhlNeedProto
NclQuark /* file_sym_name */,
NclQuark /* attname */
#endif
);

extern NclQuark* NclGetFileVarNames(
#if NhlNeedProto
NclQuark /*file_sym_name*/,
int * /*num_names*/
#endif
);

extern struct _NclApiDataList* NclGetFileVarInfo(
#if NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/
#endif
);

extern struct _NclApiDataList* NclGetFileVarCoordInfo(
#if NhlNeedProto
NclQuark /*file_sym_name*/,
NclQuark /*file_var_name*/,
NclQuark /*coordname*/
#endif
);

extern struct _NclExtValueRec *NclReadFileVar(
#if NhlNeedProto
NclQuark /*file_sym_name */,
NclQuark /*file_var_name */,
long	* /*start*/,
long	* /*finish*/,
long	* /*stride*/
#endif
);

extern struct _NclExtValueRec *NclReadFileVarAtt(
#if NhlNeedProto
NclQuark /*file_sym_name */,
NclQuark /*file_var_name */,
NclQuark /*attname */
#endif
);

extern struct _NclExtValueRec *NclReadFileVarCoord(
#if NhlNeedProto
NclQuark /*file_sym_name */,
NclQuark /*file_var_name */,
NclQuark /*coordname*/,
long	* /*start*/,
long	* /*finish*/,
long	* /*stride*/
#endif
);

extern struct _NclApiDataList* NclGetVarInfo(
#if NhlNeedProto
NclQuark /*var_sym_name */
#endif
);
extern struct _NclApiDataList* NclGetVarCoordInfo(
#if NhlNeedProto
NclQuark /*var_sym_name */,
NclQuark /*coordname*/
#endif
);

extern struct _NclExtValueRec* NclReadVar(
#if NhlNeedProto
NclQuark /*var_sym_name */,
long	* /*start*/,
long	* /*finish*/,
long	* /*stride*/
#endif
);

extern struct _NclExtValueRec* NclReadVarAtt(
#if NhlNeedProto
NclQuark /*var_sym_name */,
NclQuark /*attname*/
#endif
);

extern struct _NclExtValueRec* NclReadVarCoord(
#if NhlNeedProto
NclQuark /*var_sym_name */,
NclQuark /*coordname*/,
long	* /*start*/,
long	* /*finish*/,
long	* /*stride*/
#endif
);

extern struct _NclExtValueRec* NclReadVarCoordAtt(
#if NhlNeedProto
NclQuark /*var_sym_name */,
NclQuark /*coordname*/,
NclQuark /*attname*/
#endif
);



extern char *NclTypeToString(
#if     NhlNeedProto
void * /*val*/, 
int /*data_type*/
#endif
);

extern void NclSetPromptFunc(
#if	NhlNeedProto
NclPromptFunc /*prmf*/, 
void * /*user_data */
#endif
);

extern void NclSetOutputStream(
#if	NhlNeedProto
	FILE	*out
#endif
);

extern NhlBoolean NclSymbolDefined(
#if	NhlNeedProto
	char	*sym
#endif
);

#ifdef __cplusplus
}
#endif

#endif  /*_NCNclApi_h */

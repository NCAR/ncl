/*
 *      $Id: NclApi.c,v 1.16 1995-04-01 00:54:45 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		ncl_api.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Aug 17 11:44:15 MDT 1993
 *
 *	Description:	Contains functions for initializing and using
 *			ncl from with in an application.
 */
#ifdef __cplusplus
extern "C" {
#endif

#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/App.h>
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "NclVar.h"
#include "ApiRecords.h"
#include "NclApi.h"
#include <errno.h>
#include <netcdf.h>

FILE *the_err_file;
#if     defined(SunOS) && (MAJOR == 4)
extern FILE *nclin;
extern int nclparse(int);
#else
extern FILE *yyin;
extern int yyparse(int);
#endif
int fd[2];

extern char *the_input_buffer;
extern char *the_input_buffer_ptr;
extern int the_input_buffer_size;

FILE *thefptr;
FILE *theoptr;
int cmd_line;
extern int cur_line_number;

extern FILE* error_fp;
extern FILE* stdout_fp;

int NclInitServer
#if	NhlNeedProto
(NhlErrorTypes error_level)
#else
(error_level)
	NhlErrorTypes error_level;
#endif
{
	int appid;
#ifdef YYDEBUG
#if     defined(SunOS) && (MAJOR == 4)
        extern int ncldebug;
        ncldebug = 1;
#else
        yydebug = 1;
        extern int yydebug;
#endif
#endif
	ncopts = NC_VERBOSE;
/*
	thefptr = fopen("ncl.tree","w");
        theoptr = fopen("ncl.seq","w");
*/
	NhlInitialize();
	NhlVACreate(&appid,"ncl",NhlappLayerClass,NhlDEFAULT_APP,
                NhlNappDefaultParent,1,NULL);


	
	NhlVASetValues(NhlErrGetID(),
/*
		NhlNerrBuffer,True,
*/
		NhlNerrLevel,error_level,
/*
		NhlNerrPrint,False,
		NhlNerrFilePtr,the_err_file,
*/
		NULL);
	_NclInitMachine();
	_NclInitSymbol();
	_NclInitTypeClasses();
	_NclInitDataClasses();
	_NhlRegSymConv(NhlTGenArray,NhlTNclData,NhlTGenArray,NhlTGenArray);


	return(1);	
	
}

void NclCloseServer
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlClose();
}

int NclSubmitBlock1
#if	NhlNeedProto
(char *script,int script_size)
#else
(script,script_size)
	char *script;
	int script_size;
#endif
{
	static first = 1;
	int i;
	char *tmp = script;
	if(the_input_buffer != NULL) {
		NclFree(the_input_buffer);
	}
	the_input_buffer = (char*)NclMalloc((unsigned)sizeof(char)*script_size +1);
	the_input_buffer_ptr = the_input_buffer;
	for(i = 0; i< script_size; i++) *the_input_buffer_ptr++ = *tmp++;
	*the_input_buffer_ptr = '\n';
	the_input_buffer_ptr = the_input_buffer;
        the_input_buffer_size = script_size+1;
#if     defined(SunOS) && (MAJOR == 4)
        if(nclparse((first? 1: 0))==1) {
#else
        if(yyparse((first? 1: 0))==1) {
#endif
		first = 0;
                return(0);
        } else {
		first = 0;
                return(1);
        }
}

int NclSubmitBlock2
#if	NhlNeedProto
(char *script[])
#else
(script)
	char *script[];
#endif
{
	int i = 0;
	int size = 0;
	char *tmp;
	static int first = 1;

	for(i =0; script[i] != NULL; i++)  
		size += strlen(script[i]) + 1; 

	if(the_input_buffer != NULL) {
		NclFree(the_input_buffer);
	}
	the_input_buffer = (char*)NclMalloc((unsigned)sizeof(char)*size +1);
	the_input_buffer_ptr = the_input_buffer;
	for(i=0;script[i] != NULL;i++) {
		tmp = script[i];
		while((*the_input_buffer_ptr = *tmp) != '\0') {
			the_input_buffer_ptr++;
			tmp++;
		}
		*(the_input_buffer_ptr) = '\n';
		the_input_buffer_ptr++;
	}
	*the_input_buffer_ptr = '\n';
	the_input_buffer_size = size +1;
	the_input_buffer_ptr = the_input_buffer;
#if     defined(SunOS) && (MAJOR == 4)
        if(nclparse((first?1:0))==1) {
#else
        if(yyparse((first?1:0))==1) {
#endif
		first = 0;
                return(0);
        } else {
		first = 0;
                return(1);
        }
}

int NclSubmitCommand
#if	NhlNeedProto
(char * command)
#else
(command)
	char *command;
#endif
{
	static int first = 1;
	the_input_buffer_ptr = the_input_buffer = command;
	the_input_buffer_size = strlen(command);
#if     defined(SunOS) && (MAJOR == 4)
	if(nclparse((first?1:0))==1) {
#else
	if(yyparse((first?1:0))==1) {
#endif
		first = 0;
                return(0);
        } else {
		first = 0;
                return(1);
        }
}

void NclPrintErrorMsgs
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int num,i;
	Const NhlErrMsg *msptr;

	num = NhlErrNumMsgs();
	for(i=0; i< num; i++) {
		NhlErrGetMsg(i,&msptr);
		NhlErrFPrintMsg(the_err_file,msptr);
	}
	return;
}

int NclGetErrorId 
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	return(NhlErrGetID());
}


struct _NclApiDataList* NclGetProcFuncList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(_NclGetDefinedProcFuncInfo());
} 

struct _NclApiDataList* NclGetVarList
#if	NhlNeedProto
(void)
#else
()
#endif
{
        return(_NclGetDefinedVarInfo());
}

struct _NclApiDataList* NclGetFileList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(_NclGetDefinedFileInfo());
}

struct _NclApiDataList* NclGetFileVarsList
#if	NhlNeedProto
(NclQuark filevar)
#else
(filevar)
	NclQuark filevar;
#endif
{
	return(_NclGetFileVarInfo(filevar));
}

void NclFreeDataList
#if	NhlNeedProto
(NclApiDataList *dlist)
#else
(dlist)
NclApiDataList *dlist;
#endif
{
	if(dlist != NULL) 
		_NclFreeApiDataList((void*)dlist);
}
int NclGetHLUObjId
#if	NhlNeedProto
(char *varname)
#else
(varname)
	char *varname;
#endif
{
	return(_NclGetHLUObjId(varname));
}

struct _NclApiDataList* NclGetHLUObjsList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(_NclGetDefinedHLUInfo());
}



struct _NclExtValueRec *NclGetVarValue
#if	NhlNeedProto
(char *var_name,int copy_data)
#else
(var_name,copy_data)
	char * var_name;
	int copy_data;
#endif
{
	NclSymbol *s;

	s = _NclLookUp(var_name);
	return(_NclGetVarValue(s,copy_data));
}

NclExtValueRec *NclGetExprValue
#if	NhlNeedProto
(char * expression)
#else
(expression)
	char* expression;
#endif
{
	char *tmp= NULL,*ptr = NULL;
	NclSymbol *s;
	NclExtValueRec *tmp_val = NULL;
	int size;
/*
	s = _NclLookUp(NCLAPI_TMP_VAR);

	if((s != NULL)&&(s->type != UNDEF)) {
		
	}
*/
	size = strlen(expression);
	size += strlen(NCLAPI_TMP_VAR);
	size += 2; /* one for equals and one for '\0' */

	ptr = tmp = NclMalloc(size);
	memcpy(ptr,NCLAPI_TMP_VAR,strlen(NCLAPI_TMP_VAR));
	ptr += strlen(NCLAPI_TMP_VAR) + 1;
	*ptr++ = '=';
	memcpy(ptr,expression,strlen(expression));
	ptr += strlen(expression) + 1;
	*ptr = '\0';
	NclSubmitCommand(tmp);
	tmp_val = NclGetVarValue(NCLAPI_TMP_VAR,1);
	NclSubmitCommand(NCLAPI_DEL_TMP_VAR);
	
	return(tmp_val);
}

void NclFreeExtValue
#if	NhlNeedProto
(NclExtValueRec* val)
#else
(val)
NclExtValueRec* val;
#endif
{
	if(val != NULL) {
		if(!val->constant)
			NclFree(val->value);
		NclFree(val);
		return;
	}
}
static NclApiDataList *new_list = NULL;
static NclApiDataList *del_list = NULL;

void _NclAddToNewList
#if	NhlNeedProto
(int id,NclQuark name,NhlLayerClass cl_ptr)
#else
(id,name,cl_ptr)
int id;
NclQuark name;
NhlLayerClass cl_ptr;
#endif
{
	NclApiDataList* tmp;

	tmp = NclMalloc(sizeof(NclApiDataList));
	tmp->u.hlu_obj =(NclApiHLUObjInfoRec*)NclMalloc(sizeof(NclApiHLUObjInfoRec));
	tmp->u.hlu_obj->name = name;
	tmp->u.hlu_obj->obj_id = id;
	tmp->u.hlu_obj->obj_class = cl_ptr;
	tmp->next = new_list;
	new_list = tmp;
	return;
}

void _NclAddToDelList
#if	NhlNeedProto
(int id,NclQuark name,NhlLayerClass cl_ptr)
#else
(id,name,cl_ptr)
int id;
NclQuark name;
NhlLayerClass cl_ptr;
#endif
{
	NclApiDataList* tmp;

	tmp = NclMalloc(sizeof(NclApiDataList));
	tmp->u.hlu_obj =(NclApiHLUObjInfoRec*)NclMalloc(sizeof(NclApiHLUObjInfoRec));
	tmp->u.hlu_obj->name = name;
	tmp->u.hlu_obj->obj_id = id;
	tmp->u.hlu_obj->obj_class = cl_ptr;
	tmp->next = del_list;
	del_list = tmp;
	return;
}

struct _NclApiDataList* NclGetNewHLUObjsList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp;

	tmp = new_list;
	new_list = NULL;
	return(tmp);
}

struct _NclApiDataList* NclGetDelHLUObjsList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp;

	tmp = del_list;
	del_list = NULL;
	return(tmp);
}



#ifdef __cplusplus
}
#endif

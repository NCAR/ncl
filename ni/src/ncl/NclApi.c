/*
 *      $Id: NclApi.c,v 1.32 1996-07-25 20:28:13 ethan Exp $
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
#include "DataSupport.h"
#include "NclCallBacksI.h"
#include "NclMdInc.h"

int force_reset = 0;
int start_state = 0;

NhlErrorTypes NclApiRegisterCallback
#if	NhlNeedProto
(NclApiObjTypes obj_type,unsigned int type, void* callback_function, void* user_data)
#else
(obj_type,type, callback_function, user_data)
NclApiObjTypes obj_type;
unsigned int type;
void* callback_function;
void* user_data;
#endif
{
	return(_NclRegisterCallback((NclObjTypes)obj_type,type,callback_function,user_data));
}

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
extern char *cur_line_text;
extern int cur_line_maxsize;
extern char *cur_line_text_pos;


extern FILE* error_fp;
extern FILE* stdout_fp;

void NclResetServer
#if 	NhlNeedProto
(void)
#else
()
#endif
{
	force_reset = 1;
	return;
}

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
        extern int yydebug;
        yydebug = 1;
#endif
#endif
	ncopts = NC_VERBOSE;
/*
	thefptr = fopen("ncl.tree","w");
        theoptr = fopen("ncl.seq","w");
*/
	cur_line_text = NclMalloc((unsigned)512);
        cur_line_maxsize = 512;
        cur_line_text_pos = &(cur_line_text[0]);

	NhlInitialize();
	NhlVACreate(&appid,"ncl",NhlappClass,NhlDEFAULT_APP,
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
	_NhlRegSymConv(NULL,NhlTGenArray,NhlTNclData,NhlTGenArray,NhlTGenArray);


	the_input_buffer = "begin\nend\n";
	the_input_buffer_ptr = the_input_buffer;
	the_input_buffer_size = strlen("begin\nend\n");
#if     defined(SunOS) && (MAJOR == 4)
        start_state = nclparse(1);
#else
        start_state = yyparse(1);
#endif
	the_input_buffer = NULL;

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
	int state;
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
	if(force_reset) {
		_NclDeleteNewSymStack();
/*
		(void)_NclPopScope();
*/

	}
#if     defined(SunOS) && (MAJOR == 4)
        state = nclparse((first||force_reset? 1: 0));
#else
        state = yyparse((first||force_reset? 1: 0));
#endif
	force_reset = 0;
	first = 0;
        return(state==start_state);
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
	int state;

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
	if(force_reset) {
                _NclDeleteNewSymStack();
/*
		(void)_NclPopScope();
*/

        }

#if     defined(SunOS) && (MAJOR == 4)
        state = nclparse((first||force_reset? 1: 0));
#else
        state = yyparse((first||force_reset? 1: 0));
#endif
	first = 0;
	force_reset = 0;
        return(state==start_state);
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
	int state;
	the_input_buffer_ptr = the_input_buffer = command;
	the_input_buffer_size = strlen(command);

	if(force_reset) {
                _NclDeleteNewSymStack();
/*
		(void)_NclPopScope();
*/

        }

#if     defined(SunOS) && (MAJOR == 4)
        state = nclparse((first||force_reset? 1: 0));
#else
        state = yyparse((first||force_reset? 1: 0));
#endif
	first = 0;
	force_reset = 0;
        return(state==start_state);
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
NclQuark* NclGetVarSymNames
#if 	NhlNeedProto
(int	*num_names)
#else
(num_names)
int	*num_names;
#endif
{
	return(_NclGetVarSymNames(num_names));	
}

NclQuark* NclGetFileSymNames
#if 	NhlNeedProto
(int	*num_names)
#else
(num_names)
int	*num_names;
#endif
{
	return(_NclGetFileSymNames(num_names));	
}

NclQuark* NclGetHLUVarSymNames
#if 	NhlNeedProto
(int *num_names)
#else
(num_names)
int *num_names;
#endif
{
	return(_NclGetHLUVarSymNames(num_names));
}

NclQuark *NclGetProcFuncSymNames
#if     NhlNeedProto
(int *num_names)
#else
(num_names)
int *num_names;
#endif
{
        return(_NclGetProcFuncSymNames(num_names));
}


struct _NclApiDataList *NclGetFileInfo
#if	NhlNeedProto
(NclQuark file_sym_name)
#else
(file_sym_name)
NclQuark file_sym_name
#endif
{
	return(_NclGetFileInfo(file_sym_name));
}

struct _NclExtValueRec * NclReadFileAtt
#if  	NhlNeedProto
(NclQuark file_sym_name,NclQuark attname) 
#else
(file_sym_name,attname) 
NclQuark file_sym_name;
NclQuark attname;
#endif
{
	return(_NclReadFileAtt(file_sym_name,attname));
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
	return(_NclGetFileVarInfoList(filevar));
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
NclExtValueRec *NclGetHLUObjId
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
struct _NclExtValueRec *NclReadVar
#if	NhlNeedProto
(NclQuark var_name,long *start, long *finish, long* stride)
#else
(var_name,start,finish,stride)
	NclQuark var_name;
	long *start;
	long *finish;
	long *stride;
#endif
{
	NclSymbol *s;

	s = _NclLookUp(NrmQuarkToString(var_name));
	return(_NclReadVarValue(s,start,finish,stride));
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
(int id,NclQuark name,NhlClass cl_ptr)
#else
(id,name,cl_ptr)
int id;
NclQuark name;
NhlClass cl_ptr;
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
(int id,NclQuark name,NhlClass cl_ptr)
#else
(id,name,cl_ptr)
int id;
NclQuark name;
NhlClass cl_ptr;
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


NclQuark *NclGetFileVarNames
#if	NhlNeedProto
(NclQuark file_sym_name,int *num_names)
#else
(file_sym_name,num_names)
NclQuark file_sym_name;
int *num_names;
#endif
{
	return(_NclGetFileVarNames(file_sym_name,num_names));
}

struct _NclApiDataList *NclGetFileVarInfo
#if     NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name)
#else
(file_sym_name,file_var_name)
NclQuark file_sym_name;
NclQuark file_var_name;
#endif
{
	return(_NclGetFileVarInfo(file_sym_name,file_var_name));
}

struct _NclApiDataList * NclGetFileVarCoordInfo
#if     NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name, NclQuark coord_name)
#else
(file_sym_name,file_var_name, coord_name)
NclQuark file_sym_name;
NclQuark file_var_name;
NclQuark coord_name;
#endif
{
	return(_NclGetFileVarCoordInfo(file_sym_name,file_var_name,coord_name));
}

struct _NclExtValueRec *NclReadFileVar
#if     NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name, long *start, long *finish, long *stride)
#else
(file_sym_name,file_var_name, start,finish, stride)
NclQuark file_sym_name;
NclQuark file_var_name;
long * start;
long * finish;
long * stride;
#endif
{
	return(_NclReadFileVar(file_sym_name,file_var_name,start,finish,stride));
}

struct _NclExtValueRec *NclReadFileVarAtt
#if     NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name, NclQuark attname )
#else
(file_sym_name,file_var_name,attname )
NclQuark file_sym_name;
NclQuark file_var_name;
NclQuark attname;
#endif
{
	return(_NclReadFileVarAtt(file_sym_name,file_var_name,attname));
}

struct _NclExtValueRec *NclReadFileVarCoord
#if     NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name, NclQuark coordname, long *start, long* finish, long * stride)
#else
(file_sym_name,file_var_name,coordname,start,finish,stride)
NclQuark file_sym_name;
NclQuark file_var_name;
NclQuark coordname;
long * start;
long * finish;
long * stride;
#endif
{
	return(_NclReadFileVarCoord(file_sym_name,file_var_name,coordname, start,finish,stride));
}

struct _NclApiDataList *NclGetVarInfo
#if     NhlNeedProto
(NclQuark var_sym_name)
#else
(var_sym_name)
NclQuark var_sym_name;
#endif
{
	return(_NclGetVarInfo(var_sym_name));
}


struct _NclApiDataList *NclGetVarCoordInfo
#if     NhlNeedProto
(NclQuark var_sym_name,NclQuark coordname)
#else
(var_sym_name,coordname)
NclQuark var_sym_name;
NclQuark coordname;
#endif
{
	return(_NclGetVarCoordInfo(var_sym_name,coordname));
}

struct _NclExtValueRec *NclReadVarAtt
#if     NhlNeedProto
(NclQuark var_sym_name,NclQuark attname )
#else
(var_sym_name,attname)
NclQuark var_sym_name;
NclQuark attname;
#endif
{
	return(_NclReadVarAtt(var_sym_name,attname));
}

struct _NclExtValueRec *NclReadVarCoord
#if     NhlNeedProto
(NclQuark var_sym_name,NclQuark coordname, long *start, long *finish, long *stride)
#else
(var_sym_name,coordname,start, finish, stride)
NclQuark var_sym_name;
NclQuark coordname;
long *start;
long *finish;
long *stride;
#endif
{
	return(_NclReadVarCoord(var_sym_name,coordname,start,finish,stride));
}

struct _NclExtValueRec *NclReadVarCoordAtt
#if     NhlNeedProto
(NclQuark var_sym_name,NclQuark coordname, NclQuark attname)
#else
(var_sym_name,coordname,attname)
NclQuark var_sym_name;
NclQuark coordname;
NclQuark attname
#endif
{
	return(_NclReadVarCoordAtt(var_sym_name,coordname,attname));
}

char *NclTypeToString
#if 	NhlNeedProto
(void *val, int data_type) 
#else
(val, data_type) 
void *val;
int data_type;
#endif
{
	char buffer[256];
	char *out;


	switch(data_type) {
	case NCLAPI_short:
		sprintf(buffer,nclTypeshortClassRec.type_class.format,*(short*)val);
		break;
	case NCLAPI_int:
		sprintf(buffer,nclTypeintClassRec.type_class.format,*(int*)val);
		break;
	case NCLAPI_long:
		sprintf(buffer,nclTypelongClassRec.type_class.format,*(long*)val);
		break;
	case NCLAPI_float:
		sprintf(buffer,nclTypefloatClassRec.type_class.format,*(float*)val);
		break;
	case NCLAPI_double:
		sprintf(buffer,nclTypedoubleClassRec.type_class.format,*(double*)val);
		break;
	case NCLAPI_char:
		sprintf(buffer,nclTypecharClassRec.type_class.format,*(char*)val);
		break;
	case NCLAPI_byte:
		sprintf(buffer,nclTypebyteClassRec.type_class.format,*(byte*)val);
		break;
	case NCLAPI_string:
		sprintf(buffer,nclTypestringClassRec.type_class.format,NrmQuarkToString(*(NclQuark*)val));
		break;
	case NCLAPI_logical:
		if(*(int*)val)  {
			sprintf(buffer,"True");
		} else {
			sprintf(buffer,"False");
		}
		break;
	default:
		return(NULL);
	}
	out = (char*)NclMalloc(strlen(buffer)+1);
	strcpy(out,buffer);
	return(out);
}

#ifdef __cplusplus
}
#endif

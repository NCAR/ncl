/*
 *      $Id: NclApi.c,v 1.6 1994-10-29 00:57:30 ethan Exp $
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
#include "defs.h"
#include "NclData.h"
#include "Symbol.h"
#include "NclVar.h"
#include "ApiRecords.h"
#include "NclApi.h"
#include <errno.h>

FILE *the_err_file;
extern FILE *yyin;
extern int yyparse(int);
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
#if __STDC__
(FILE *error_file,FILE* stdout_file,NhlErrorTypes error_level)
#else
(error_file,stdout_file,error_level)
	FILE *error_file;
	FILE *stdout_file;
	NhlErrorTypes error_level;
#endif
{
#ifdef YYDEBUG
        extern int yydebug;
        yydebug = 1;
#endif
	NhlOpen();
	error_fp = the_err_file = error_file;
	stdout_fp = stdout_file;
	thefptr = fopen("ncl.tree","w");
        theoptr = fopen("ncl.seq","w");


	
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
	_NclInitDataClasses();
	_NhlRegSymConv(NhlTGenArray,NhlTNclData,NhlTGenArray,NhlTGenArray);


	return(1);	
	
}

void NclCloseServer
#if __STDC__
(void)
#else
()
#endif
{
	NhlClose();
}

int NclSubmitBlock1
#if __STDC__
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
        if(yyparse((first? 1: 0))==1) {
		first = 0;
                return(0);
        } else {
		first = 0;
                return(1);
        }
}

int NclSubmitBlock2
#if __STDC__
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
        if(yyparse((first?1:0))==1) {
		first = 0;
                return(0);
        } else {
		first = 0;
                return(1);
        }
}

int NclSubmitCommand
#if __STDC__
(char * command)
#else
(command)
	char *command;
#endif
{
	static int first = 1;
	the_input_buffer_ptr = the_input_buffer = command;
	the_input_buffer_size = strlen(command);
	if(yyparse((first?1:0))==1) {
		first = 0;
                return(0);
        } else {
		first = 0;
                return(1);
        }
}

void NclPrintErrorMsgs
#if __STDC__
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
#if __STDC__
(void)
#else 
()
#endif
{
	return(NhlErrGetID());
}


struct _NclApiDataList* NclGetProcFuncList
#if __STDC__
(void)
#else
()
#endif
{
	return(_NclGetDefinedProcFuncInfo());
} 

struct _NclApiDataList* NclGetVarList
#if __STDC__
(void)
#else
()
#endif
{
        return(_NclGetDefinedVarInfo());
}

struct _NclApiDataList* NclGetFileList
#if __STDC__
(void)
#else
()
#endif
{
	return(_NclGetDefinedFileInfo());
}

struct _NclApiDataList* NclGetFileVarsList
#if __STDC__
(NclQuark filevar)
#else
(filevar)
	NclQuark filevar;
#endif
{
	return(_NclGetFileVarInfo(filevar));
}

void NclFreeDataList
#if __STDC__
(NclApiDataList *dlist)
#else
(dlist)
NclApiDataList *dlist;
#endif
{
	if(dlist != NULL) 
		_NclFreeApiDataList((void*)dlist);
}

struct _NclApiDataList* NclGetHLUObjsList
#if __STDC__
(void)
#else
()
#endif
{
	return(_NclGetDefinedHLUInfo());
}



struct _NclExtValueRec *NclGetVarValue
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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



#ifdef __cplusplus
}
#endif

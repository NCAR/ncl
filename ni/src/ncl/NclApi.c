
/*
 *      $Id: NclApi.c,v 1.2 1993-12-21 19:17:49 ethan Exp $
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
#include <data_objs/NclData.h>
#include <defs.h>
#include <Symbol.h>
#include <errno.h>

FILE *the_err_file;
extern FILE *yyin;
int fd[2];

extern char *the_input_buffer;
extern char *the_input_buffer_ptr;
extern int the_input_buffer_size;



int NclInitServer
#if __STDC__
(FILE *error_file,NhlErrorTypes error_level)
#else
(error_file,error_level)
	FILE *error_file;
	NhlErrorTypes error_level;
#endif
{
	NhlOpen();
	the_err_file = error_file;
	
	NhlSetValues(NhlErrGetID(),
		NhlNbufferErrors,True,
		NhlNerrorLevel,error_level,
		NhlNprintErrors,False,NULL);
	
	return(_NclInitSymbol());
	
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
        if(yyparse()==1) {
                return(0);
        } else {
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
        if(yyparse()==1) {
                return(0);
        } else {
                return(1);
        }
}

int NclSubmitCommand
#if __STDC__
(char * command);
#else
(command)
	char *command;
#endif
{
	the_input_buffer_ptr = the_input_buffer = command;
	the_input_buffer_size = strlen(command);
	if(yyparse()==1) {
                return(0);
        } else {
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
	NhlErrMsg *msptr;

	NhlErrGetMsg(&num,&msptr);
	for(i=0; i< num; i++) {
		NhlErrFPrintMsg(the_err_file,&(msptr[i]));
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
#ifdef __cplusplus
}
#endif

/*
 *      $Id: NclApi.c,v 1.61 2008-07-11 23:05:47 dbrown Exp $
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
#include "Keywords.h"
#include "NclVar.h"
#include "ApiRecords.h"
#include "NclApi.h"
#include <errno.h>
#include <netcdf.h>
#include "DataSupport.h"
#include "NclCallBacksI.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "NclHLUVar.h"
#include "NclVar.h"
#include "NclFile.h"
#include "NclFileVar.h"
#include "HLUSupport.h"
#include <dirent.h>
#if defined(HPUX)
#include <dl.h>
#else
#include <dlfcn.h>
#endif

#if 1
#include "NclDriver.h"
#else
short    NCLverbose = 0;
short    NCLecho = 0;
short   NCLoverrideEcho = 0;    /* override echo; non-advertised option */
short   NCLnoPrintElem = 0;     /* don't enumerate values in print() */
short   NCLnoSysPager = 0;
char   *nclf = NULL;
#endif

int force_reset = 0;
int start_state = 0;

extern int _NclParseString(
#if NhlNeedProto
char * /*str*/,
int  /*reset*/
#endif
);

NhlErrorTypes NclHLUStringRef
#if 	NhlNeedProto
(int id , NclHLUStruct *ptr)
#else
( id , ptr)
int id;
NclHLUStruct *ptr;
#endif
{
	NclHLULookUpTable* tmp;

	tmp = _NclGetHLURefInfo(id);

	if(tmp == NULL) {
		ptr->hlu_id = -1;
		ptr->var_quark = -1;
		ptr->att_quark = -1;
		ptr->offset = -1;
		ptr->n_offsets = 0;
		ptr->n_refs = 0;
		ptr->myprivate = NULL;
		return(NhlFATAL);
	} else {
		ptr->hlu_id = tmp->hlu_id;
		if(tmp->n_entries > 0) {
			ptr->var_quark = tmp->ref_list[0].vq;
			ptr->att_quark = tmp->ref_list[0].aq;
			ptr->n_refs = tmp->n_entries;
			ptr->n_offsets = tmp->ref_list[0].n_refs;
			ptr->offset = tmp->ref_list[0].refs[0];
			ptr->myprivate = (void*)tmp;
		} else {
			ptr->var_quark = -1;
			ptr->offset = -1;
			ptr->n_offsets = 0;
			ptr->n_refs = 0;
			ptr->offset = -1;
			ptr->myprivate = NULL;
			return(NhlWARNING);
		}
	}
	return(NhlNOERROR);	
}


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

extern void InitStdStreams(FILE* in, FILE* out, FILE* err);

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
(void)
#else
()
#endif
{
        DIR *d;
        struct dirent *ent;
        void *so_handle;
        char buffer[4*NCL_MAX_STRING];
        void (*init_function)(void);
        char *libpath;
        char *scriptpath;
        char *pt;
	int reset = 1;

#ifdef YYDEBUG
/*
#if     defined(SunOS) && (MAJOR == 4)
        extern int ncldebug;
        ncldebug = 1;
#else
*/
        extern int yydebug;
        yydebug = 1;
/*
#endif
*/
#endif
	ncopts = NC_VERBOSE;
	cur_line_text = NclMalloc((unsigned)512);
        cur_line_maxsize = 512;
        cur_line_text_pos = &(cur_line_text[0]);
	/* init to -3 so begin/end determination of start_state isn't counted */
	cur_line_number = -3;

        InitStdStreams(stdin, stdout, stderr);
        
	NhlOpen();

	_NclInitMachine();
	_NclInitSymbol();
	_NclInitTypeClasses();
	_NclInitDataClasses();

/*
* Now handle default directories
*/
	if((libpath = getenv("NCL_DEF_LIB_DIR"))!=NULL) {
		d = opendir(_NGResolvePath(libpath));
		if(d != NULL) {
			while((ent = readdir(d)) != NULL) {
				if(*ent->d_name != '.') {
					sprintf(buffer,"%s/%s",_NGResolvePath(libpath),ent->d_name);
#if defined(HPUX)
					so_handle = shl_load(buffer,BIND_IMMEDIATE,0L);
#else
					so_handle = dlopen(buffer,RTLD_NOW);
					if(so_handle == NULL) {
						NhlPError(NhlFATAL,NhlEUNKNOWN," Could not open (%s): %s",buffer,dlerror());
					}
#endif
					if(so_handle != NULL) {
#if defined(HPUX)
						init_function = NULL;
						(void)shl_findsym(so_handle, "Init",TYPE_UNDEFINED,(void*)&init_function);
#else
						init_function = dlsym(so_handle, "Init");
#endif
						if(init_function != NULL) {
							(*init_function)();
						} else {
#if defined(HPUX)
							shl_unload(so_handle);
#else
							dlclose(so_handle);
#endif
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not find Init() in external file %s, file not loaded",buffer);
						}
					} 
				}
			}
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN," Could not open (%s), no libraries loaded.",libpath);
		}
	}

	_NclInitClass(nclHLUObjClass);
	_NclInitClass(nclHLUVarClass);
	_NclInitClass(nclVarClass);
	_NclInitClass(nclFileClass);
	_NclInitClass(nclFileVarClass);

	the_input_buffer = "begin\nend\n\177";
	the_input_buffer_ptr = the_input_buffer;
	the_input_buffer_size = strlen("begin\nend\n");
	start_state = _NclParseString(the_input_buffer,1);
	the_input_buffer = NULL;

	if((scriptpath = getenv("NCL_DEF_SCRIPTS_DIR"))!=NULL) {
		d = opendir(_NGResolvePath(scriptpath));
		if(d!= NULL) {
			while((ent = readdir(d)) != NULL) {
				if(*ent->d_name != '.') {
					sprintf(buffer,"%s/%s",_NGResolvePath(scriptpath),ent->d_name);
					pt = strrchr(buffer,'.');
					if(pt != NULL) {
						pt++;
						if(strncmp(pt,"ncl",3)==0) {
							if(_NclPreLoadScript(buffer,1) == NhlFATAL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Error loading default script");
							} else {
								yyparse(reset);
/*
								if(reset)
									reset = 0;
*/
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Scripts must have the \".ncl\" file extension");
						}
					} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Scripts must have the \".ncl\" file extension");
					}
				}
			}
		} else {
            NhlPError(NhlWARNING,NhlEUNKNOWN," Could not open (%s), no scripts loaded.",scriptpath);
		}

	}

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
	static int first = 1;
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
	}
	state = _NclParseString(the_input_buffer,(first||force_reset? 1: 0));
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
        }
	state = _NclParseString(the_input_buffer,(first||force_reset? 1: 0));
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

	if(the_input_buffer != NULL) {
		NclFree(the_input_buffer);
	}
	if(command[strlen(command)-2] == '\n') {
		the_input_buffer = (char*)NclMalloc((unsigned)strlen(command)+2);
		strcpy(the_input_buffer,command);
		the_input_buffer[strlen(command)+1] = '\0';
		the_input_buffer[strlen(command)] = '\177';
	
		the_input_buffer_size = strlen(command) + 2;
	} else {
/*
* All lines must be terminated by '\n'
* Since sometimes the calling environment appends a '\n'
* this if statement takes care of when the caller doesn't
* append a '\n'
*/
		the_input_buffer = (char*)NclMalloc((unsigned)strlen(command)+3);
		strcpy(the_input_buffer,command);
		the_input_buffer[strlen(command)] = '\n';
		the_input_buffer[strlen(command)+2] = '\0';
		the_input_buffer[strlen(command)+1] = '\177';
	
		the_input_buffer_size = strlen(command) + 3;
	}

	if(force_reset) {
                _NclDeleteNewSymStack();
/*
		(void)_NclPopScope();
*/

        }

	state = _NclParseString(the_input_buffer,(first||force_reset? 1: 0));
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
NhlClass *NclGetHLUClassPtrs
#if     NhlNeedProto
(int    *num_names)
#else
(num_names)
int     *num_names;
#endif
{
        return(_NclGetHLUClassPtrs(num_names));
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
	NclExtValueRec *tmp_val = NULL;
	ng_usize_t size;
/*
	s = _NclLookUp(NCLAPI_TMP_VAR);

	if((s != NULL)&&(s->type != UNDEF)) {
		
	}
*/
	size = strlen(expression);
	size += strlen(NCLAPI_TMP_VAR);
	size += 2; /* one for equals and one for '\n' */

	ptr = tmp = NclMalloc(size);
	memcpy(ptr,NCLAPI_TMP_VAR,strlen(NCLAPI_TMP_VAR));
	ptr += strlen(NCLAPI_TMP_VAR);
	*ptr++ = '=';
	memcpy(ptr,expression,strlen(expression));
	ptr += strlen(expression);
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
		out = (char*)NclMalloc(strlen(NrmQuarkToString(*(NclQuark*)val))+1);
		strcpy(out,NrmQuarkToString(*(NclQuark*)val));
		return(out);
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

NhlBoolean
NclSymbolDefined
#if	NhlNeedProto
(
	char	*sym
)
#else
(sym)
	char	*sym;
#endif
{
	NclSymbol	*s;

	s = _NclLookUp(sym);

	if(!s || (s->type == UNDEF))
		return False;
	else
		return True;
}

#ifdef __cplusplus
}
#endif

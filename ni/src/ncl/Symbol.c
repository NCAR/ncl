/*
 *      $Id: Symbol.c,v 1.25 1995-11-04 00:49:30 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Symbol.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun 28 14:49:28 MDT 1993
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <errno.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include "defs.h"
#include "Symbol.h"
#include "NclBuiltIns.h"
#include "Keywords.h"
#include "NclVar.h"
#include "NclAtt.h"
#include "NclMultiDValData.h"
#include "ApiRecords.h"
#include "Machine.h"
#include "NclFile.h"
#include "VarSupport.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "NclHLUObj.h"
#include "NclApi.h"

extern void NclAddUserFileFormats(
#if	NhlNeedProto
void
#endif
);
extern void _NclAddFileFormats(
#if	NhlNeedProto
void
#endif
);
/*
* The following is a pointer to the top of the symbol table stack.
* It holds a list of pointers to the various scope levels.
*/
static NclSymTableListNode *thetablelist;

/*
* This is used to avoid fragmentation in the event syntax errors are
* detected in the current statement.
*/
static NclSymbol *new_sym_stack[NCL_MAX_SYMS_PER_STMNT];
static int new_sym_i = 0;

/*
 * Function:	_NclInitSymbol
 *
 * Description:	initializes the symbol table list
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Returns:	1 on succes, 0 on failer
 * Side Effect:	NONE
 */
extern void NclAddUserHLUObjs(
#if NhlNeedProto
void
#endif
);

extern void NclAddUserFuncs(
#if NhlNeedProto
void
#endif
);
int _NclInitSymbol
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int i;
	NclSymbol *tmp;

	thetablelist = (NclSymTableListNode *) NclMalloc((unsigned)
				sizeof(NclSymTableListNode));
	if(thetablelist == NULL) {
		NhlPError(NhlFATAL,errno,"InitSymbol: Can't create symbol table list");
		return(0);
	}

	thetablelist->level = 0;
	thetablelist->cur_offset = 0;
	thetablelist->this_scope = (NclSymTableElem*) NclMalloc((unsigned) 
		sizeof(NclSymTableElem) * NCL_SYM_TAB_SIZE);
	thetablelist->previous = NULL;

	if(thetablelist->this_scope == NULL) {
		NhlPError(NhlFATAL,errno,"InitSymbol: Can't create symbol table");
		return(0);
	}
/*
* Clear first symbol table
*/
	for(i = 0; i< NCL_SYM_TAB_SIZE; i++) {
		thetablelist->this_scope[i].nelem = 0;
		thetablelist->this_scope[i].thelist = NULL;
	}

/*
* This is where all the keywords get defined. See keywords.h for the
* data structure and list.
*/
	i = 0;
	while(keytab[i].keyword != NULL) {
		tmp = _NclAddSym(keytab[i].keyword,keytab[i].token);
		i++;
		if(tmp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"InitSymbol: An error occurred while adding keywords, can't continue");
			return(0);
			
		}
	}
	_NclAddIntrinsics();
	_NclAddBuiltIns();
	NclAddUserFuncs();
	_NclAddHLUObjs();
	NclAddUserHLUObjs();
	_NclAddFileFormats();
	NclAddUserFileFormats();
/*
* After keywords are defined a new scope must be created. The Zero
* level scope is just for keywords and does not need any memory on the
* stack.
*/
	return(_NclNewScope());
}


void _NclRegisterFunc
#if	NhlNeedProto
(NclBuiltInFuncWrapper thefuncptr,NclArgTemplate *args,char* fname,int nargs,int ftype)
#else 
(thefuncptr,args, fname,nargs,ftype)
	NclBuiltInFuncWrapper thefuncptr;
	NclArgTemplate	*args;
	char *fname;
	int nargs;
	int ftype;
#endif
{
	NclSymbol *s;

	s = _NclLookUp(fname);
	if(s != NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterFunc: %s is already a defined symbol can't add it as built-in ",fname);
		return;
	} else {
		s = _NclAddSym(fname,ftype);
		s->u.bfunc = (NclBuiltInFuncInfo*)NclMalloc((unsigned)sizeof(NclBuiltInFuncInfo));
		if(s->u.bfunc == NULL)  {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterFunc: Memory allocation error can't add %s",fname);
			return;
		}
		s->u.bfunc->nargs = nargs;
		s->u.bfunc->theargs = args;
		s->u.bfunc->thesym = s;
		s->u.bfunc->thefunc = thefuncptr;
		s->u.bfunc->thescope = NULL;
		return;
	}
}

void NclRegisterFunc
#if	NhlNeedProto
(NclPubBuiltInFuncWrapper thefuncptr, void * args, char * fname, int nargs)
#else 
(thefuncptr, args, fname, nargs)
NclPubBuiltInFuncWrapper thefuncptr;
void * args;
char * fname;
int nargs;
#endif
{
	_NclRegisterFunc((NclBuiltInFuncWrapper)thefuncptr,(NclArgTemplate*)args,fname,nargs,IFUNC);
}
void _NclRegisterProc
#if	NhlNeedProto
(NclBuiltInProcWrapper theprocptr,NclArgTemplate *args,char* fname,int nargs,int ftype)
#else 
(theprocptr,args, fname,nargs,ftype)
	NclBuiltInProcWrapper theprocptr;
	NclArgTemplate	*args;
	char *fname;
	int nargs;
	int ftype;
#endif
{
	NclSymbol *s;

	s = _NclLookUp(fname);
	if(s != NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterProc: %s is already a defined symbol can't add it as built-in ",fname);
		return;
	} else {
		s = _NclAddSym(fname,ftype);
		s->u.bproc = (NclBuiltInProcInfo*)NclMalloc((unsigned)sizeof(NclBuiltInProcInfo));
		if(s->u.bproc == NULL)  {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterProc: Memory allocation error can't add %s",fname);
			return;
		}
		s->u.bproc->nargs = nargs;
		s->u.bproc->theargs = args;
		s->u.bproc->thesym = s;
		s->u.bproc->theproc = theprocptr;
		s->u.bproc->thescope = NULL;
		return;
	}
}
void NclRegisterProc
#if NhlNeedProto
(NclBuiltInProcWrapper theprocptr, void* args, char* fname, int n_args)
#else
(theprocptr, args, fname, n_args)
NclBuiltInProcWrapper theprocptr;
void * args;
char* fname;
int n_args;
#endif
{
	_NclRegisterProc((NclBuiltInProcWrapper)theprocptr,(NclArgTemplate*)args,fname,n_args,IPROC);
}
void *NewArgs
#if NhlNeedProto
(int n)
#else
(n)
int n;
#endif
{
	return((void*)NclMalloc(n * sizeof(NclArgTemplate)));
}
void SetArgTemplate
#if NhlNeedProto
(void *args, int arg_num, char *type_name, int n_dims, int *dimsizes)
#else
(args, arg_num, type_name, n_dims, dimsizes)
void *args;
int arg_num;
char *type_name;
int n_dims;
int *dimsizes;
#endif
{
	NclArgTemplate* the_args = (NclArgTemplate*) &(((NclArgTemplate*)args)[arg_num]);
	int i;
	if(the_args == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Error adding argument template for intrinisic function NULL arg record passed");
		return;
	}
	the_args->n_dims = n_dims;
	if(dimsizes != NULL) {
		the_args->is_dimsizes = 1;
		memcpy((void*)the_args->dim_sizes,(void*)dimsizes,sizeof(int)*n_dims);
	} else {
		the_args->is_dimsizes = 0;
		for(i = 0; i < NCL_MAX_DIMENSIONS; i++) {
			the_args->dim_sizes[i] = -1;
		}
	}

	if(type_name != NULL) {	
		the_args->arg_data_type = _NclLookUp(type_name);
		if(the_args->arg_data_type == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error adding argument template for intrinisic function");
		}
	} else {
		the_args->arg_data_type = NULL;
	}
	the_args->arg_sym = NULL;
	return;

}




/*
 * Function:	_NclNewScope
 *
 * Description:	allocates and pushes a new symbol table on to the symbol
 *		table list. This new scope is usually a function but can
 *		be a separate block too. All symbols added to this symbol
 *		table will are local to this block , function or procedure.
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Returns:	
 * Side Effect:	
 */
int _NclNewScope
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int i;
	NclSymTableListNode *news = (NclSymTableListNode*) NclMalloc((unsigned)
					sizeof(NclSymTableListNode));
	
	if(news == NULL) {
		NhlPError(NhlFATAL, errno, "NewScope: Can't create a new symbol table node");
		return(0);
	}

	news->this_scope = (NclSymTableElem *) NclMalloc((unsigned)
				sizeof(NclSymTableElem) * NCL_SYM_TAB_SIZE);


	if(news->this_scope == NULL) {
		NhlPError(NhlFATAL, errno, "NewScope: Can't create a new symbol table");
		return(0);
	}

/* 
* Everytime a new symbol is added to this scope this value is incremented
* It will be a multiplier to use to figure out the location of the identifiers
* value with respect to the frames base pointer.
*/
	news->cur_offset = 0;

	news->level = thetablelist->level + 1;
	news->previous = thetablelist;
	thetablelist = news;
	
	for(i = 0; i< NCL_SYM_TAB_SIZE; i++) {
		thetablelist->this_scope[i].nelem = 0;
		thetablelist->this_scope[i].thelist = NULL;
	}
	return(1);

}


/*
 * Function:	_NclPopScope
 *
 * Description:	Returns a pointer to the symbol table. This will be save
 *		with the block, function or procedure data for use in pushing
 *		arguments and local variables on the stack. 
 *
 * In Args:	NONE
 *
 *
 * Returns:	Returns pointer to symbol table on top of stack
 *
 * Side Effect:	
 */
NclSymTableListNode * _NclPopScope 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclSymTableListNode *tmp;
	NclSymTableListNode *tmp1;
	
	if(thetablelist == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"PopScope: Symbol table stack underflow");
		return(NULL);
	}

	tmp = thetablelist;
	tmp1 = thetablelist->previous;
	tmp->previous = NULL;
	thetablelist = tmp1;
	return(tmp);
}


/*
 * Function:	hash_pjw
 *
 * Description:	hash function used for hashing symbol table entries.
 *		This function was recommended by the dragon book.
 *
 * In Args:	name 	string to be hashed
 *
 * Out Args:	NONE
 *
 * Scope:	
 * Returns:	integer between 0 and NCL_SYM_TAB_SIZE
 * Side Effect:	NONE
 */
static unsigned int hash_pjw
#if	NhlNeedProto
(char *name)
#else
( name )
char *name;
#endif
{
        char *p;
        unsigned h =0, g;

        for(p = name; *p != '\0'; p = p +1) {
                h = (h<<4) + (*p);
                if(g = h & 0xf0000000) {
                        h = h^ (g >> 24);
                        h = h ^ g;
                }
        }

        return (h % NCL_SYM_TAB_SIZE);
}

NclSymbol *_NclAddUniqueSym
#if	NhlNeedProto
(char *name,int type)
#else
(name,type)
	char *name;
	int  type;
#endif
{
	static int unique_id = 0;
	char buffer[80];

	strcpy(buffer,name);
	sprintf(&(buffer[strlen(buffer)]),"%d",unique_id);
	unique_id++;
	return(_NclAddSym(buffer,type));
	
}


/*
 * Function:	_NclAddSym
 *
 * Description:	Adds a symbol to the top symbol table
 *
 * In Args:	name	name of symbol.
 *		type    integer type indentifier which corresponds to token
 *			list in ncl.y
 *
 * Out Args:	NONE
 *
 * Returns:	Returns pointer to new symbol table entry.
 * Side Effect:	Changes symbol table data.
 */
NclSymbol *_NclAddSym
#if	NhlNeedProto
(char *name,int type)
#else
(name,type)
	char *name;
	int  type;
#endif
{
	NclSymbol *s;
	int 	index;
	

	index = hash_pjw(name);
	s = (NclSymbol*)NclMalloc((unsigned)sizeof(NclSymbol));
	if(s == NULL) {
		NhlPError(NhlFATAL,errno,"NclAddSym: Unable to create new symbol table entry");
		return(s);
	}
	
	strncpy(s->name,name,NCL_MAX_STRING);
	s->level = thetablelist->level;
	s->type = type;
	s->ind = index;
	thetablelist->this_scope[index].nelem++;
	if(thetablelist->this_scope[index].thelist != NULL) {
		thetablelist->this_scope[index].thelist->sympre = s;
	}
	s->symnext = thetablelist->this_scope[index].thelist;
	s->sympre = NULL;
	thetablelist->this_scope[index].thelist = s;
	s->u.var = NULL;
	s->offset = thetablelist->cur_offset++;

	if(s->level > 0) {
		new_sym_stack[new_sym_i] = s;
		new_sym_i++;
	}
	return(s);
}


void _NclResetNewSymStack
#if	NhlNeedProto
(void)
#else
()
#endif
{
	new_sym_i = 0;
}

void _NclDeleteNewSymStack
#if	NhlNeedProto
(void)
#else
()
#endif
{	
	int i;
	NclSymTableListNode *step;



	for( i= new_sym_i-1 ; i>= 0; i--) {
		step = thetablelist;
		while(step != NULL) {
               	 if(step->level == new_sym_stack[i]->level)
			break;
               	 else
               	         step = step->previous;
        	}
		if(step != NULL) {
			step->cur_offset = new_sym_stack[i]->offset;
			_NclDeleteSym(new_sym_stack[i]);
		}
	}
	new_sym_i = 0;
	return;
}



/*
 * Function:	_NclGetCurrentScopeLevel
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
int _NclGetCurrentScopeLevel
#if	NhlNeedProto
(void)
#else
()
#endif
{
	return(thetablelist->level);
}


/*
 * Function:	_NclDeleteSym
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NclDeleteSym
#if	NhlNeedProto
(NclSymbol *sym)
#else
(sym)
	NclSymbol *sym;
#endif
{
	NclSymTableListNode *step;
	NclSymbol *tmp,*tmp2;
	int i;

	step = thetablelist;
	while(step != NULL) {
		if(step->level == sym->level) 	
			break;
		else 
			step = step->previous;
	}
	step->this_scope[sym->ind].nelem--;
	if((sym->sympre == NULL) &&(sym->symnext == NULL)) {
		step->this_scope[sym->ind].thelist = NULL;
		if(step->this_scope[sym->ind].nelem != 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlDeleteSym: Ack!! a big problem has just occured in the symbol table");
		}
	} else if(sym->sympre == NULL) {
		sym->symnext->sympre = NULL;
	} else if(sym->symnext == NULL) {
		sym->sympre->symnext = NULL;
	} else {
		sym->sympre->symnext = sym->symnext;
		sym->symnext->sympre = sym->sympre;
	}




	switch(sym->type) {
	case UNDEF:
		break;
	case VAR:
/*
*
* ------> Need checks to see if actual strorage needs to be freed<--
*
*/
		if(sym->u.var != NULL) {
			NclFree(sym->u.var);
		}
		break;
	case DFILE:
	if(sym->u.file != NULL) {
		for(i = 0; i< NCL_SYM_TAB_SIZE; i++) {
			if(sym->u.file->filescope->this_scope[i].nelem > 0) {
				tmp = sym->u.file->filescope->this_scope[i].thelist;
				while(tmp != NULL) {
					tmp2 = tmp->symnext;
					NclFree((void*)tmp);
					tmp = tmp2;
				}
			}
		}
	}
	break;
	case PROC:
	case UNDEFFILEVAR:
	case FILEVAR:
	case EPROC:
	case NPROC:
	case FUNC:
	case EFUNC:
	case NFUNC:
	case VBLKNAME:
	default:
                break;

	}
	NclFree((void*)sym);
}

/*
 * Function:	_NclLookUp
 *
 * Description:	 searches entire list of symbol tables for name. If not
 *		found returns NULL otherwise it returns a pointer to 
 *		the symbol table entry.
 *
 * In Args:	name	name of symbol desired
 *
 * Returns:	Returns either NULL if not found or the symbol table
 *		entry pointer.
 *
 * Side Effect:	NONE
 */
NclSymbol *_NclLookUp
#if	NhlNeedProto
(char *name)
#else
(name)
char *name;
#endif
{
	NclSymbol *s;
	NclSymTableListNode *st;
	int index;

	index = hash_pjw(name);
	st = thetablelist;
/*
* Searches all scopes in current symbol table list.
*/
	while(st != NULL) {	
		s = st->this_scope[index].thelist;
		while(s != NULL) {
			if(strcmp(s->name,name) == 0)
				return(s);
			s = s->symnext;
		}
		st = st->previous;
	}
	return(NULL);
}


/*
 * Function:	_NclLookUpInScope
 *
 * Description:	
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
NclSymbol *_NclLookUpInScope
#if	NhlNeedProto
(NclSymTableListNode * thetable, char *name)
#else
(thetable, name)
NclSymTableListNode *thetable;
char *name;
#endif
{
	NclSymbol *s;
	int index;

	index = hash_pjw(name);
/*
* Searches all scopes in current symbol table list.
*/
	s = thetable->this_scope[index].thelist;
	while(s != NULL) {
		if(strcmp(s->name,name) == 0)
			return(s);
		s = s->symnext;
	}
	return(NULL);
}

NclSymbol *_NclAddInScope
#if	NhlNeedProto
(NclSymTableListNode *thetable, char* name, int type)
#else
(thetable,name,type)
	NclSymTableListNode 	*thetable;
	char			*name;
	int			type;
#endif
{

	NclSymbol *s;
	int 	index;
	

	index = hash_pjw(name);
	s = (NclSymbol*)NclMalloc((unsigned)sizeof(NclSymbol));
	if(s == NULL) {
		NhlPError(NhlFATAL,errno,"NclAddSymInScope: Unable to create new symbol table entry");
		return(s);
	}
	
	strncpy(s->name,name,NCL_MAX_STRING);
	s->level = -1;
	s->type = type;
	s->ind = index;
	thetable->this_scope[index].nelem++;
	if(thetable->this_scope[index].thelist != NULL) {
		thetable->this_scope[index].thelist->sympre = s;
	}
	s->symnext = thetable->this_scope[index].thelist;
	s->sympre = NULL;
	thetable->this_scope[index].thelist = s;
	s->u.var = NULL;
	s->offset = thetable->cur_offset++;
	return(s);
}

void _NclDeleteSymInScope
#if	NhlNeedProto
(NclSymTableListNode *thetable, NclSymbol *sym)
#else
(thetable, sym)
NclSymTableListNode *thetable;
NclSymbol *sym;
#endif
{

	thetable->this_scope[sym->ind].nelem--;
	sym->sympre->symnext = sym->symnext;
	switch(sym->type) {
	case UNDEFFILEVAR:
	case FILEVAR:
/*
*
* ------> Need checks to see if actual strorage needs to be freed<------
*
*/
		if(sym->u.fvar != NULL) {
			NclFree((void*)sym->u.fvar);
		}
		break;
	default:
                break;

	}
	NclFree((void*)sym);
	return;
}


NclSymbol *_NclChangeSymbolType 
#if	NhlNeedProto
(NclSymbol *thesym,int type)
#else
(thesym,type)
NclSymbol *thesym;
int type;
#endif
{
	thesym->type = type;
	return(thesym);
}


void _NclPrintSym
#if	NhlNeedProto
(FILE *fp) 
#else
(fp)
	FILE *fp;
#endif
{
	NclSymTableListNode *st;
	NclSymbol *s;
	int i;
	if(fp == NULL) return;

	st = thetablelist;

	while(st != NULL) {
		fprintf(fp,"Level: %d\n",st->level);
		fprintf(fp,"Current Offset: %d\n",st->cur_offset);
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->this_scope[i].nelem != 0) {
				fprintf(fp,"\tIndex: %d\n",i);
				s = st->this_scope[i].thelist;
				while(s != NULL) {
					fprintf(fp,"\t\t: %d) %s\n",s->offset,s->name);
					s = s->symnext;
				}
			}
			
		}
		st = st->previous;
	}
}

void _NclAddSingleObj
#if	NhlNeedProto
(char *name,struct _NhlClassRec *the_ptr)
#else
(name,the_ptr)
	char *name;
	struct _NhlClassRec *the_ptr;
#endif
{
	NclSymbol *s;

	s = _NclAddSym(name,OBJTYPE);
	s->u.obj_class_ptr = the_ptr;
	return;
}

NclApiDataList *_NclGetFileVarInfo
#if	NhlNeedProto
(NclQuark file_var)
#else
(file_var)
	NclQuark file_var;
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclSymbol *s = NULL;
	int i,j;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid = NULL;
	NclFileAttInfoList *step;



	s = _NclLookUp(NrmQuarkToString(file_var));
	if(s != NULL) {
		thevar = _NclRetrieveRec(s,DONT_CARE);
		theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
		thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
		if(thefile != NULL) {
			for(i = 0; i < thefile->file.n_vars; i++) {
				tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
				tmp->kind = VARIABLE_LIST;
				tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
				tmp->u.var->name = thefile->file.var_info[i]->var_name_quark;
				tmp->u.var->data_type_quark = NrmStringToQuark(_NclBasicDataTypeToName(thefile->file.var_info[i]->data_type));
				tmp->u.var->type = FILEVAR;
				tmp->u.var->n_dims = thefile->file.var_info[i]->num_dimensions;
				tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.var->n_dims);
				for(j = 0 ; j < tmp->u.var->n_dims ; j++) {
					tmp->u.var->dim_info[j].dim_quark =thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
					tmp->u.var->dim_info[j].dim_num = thefile->file.var_info[i]->file_dim_num[j];
					tmp->u.var->dim_info[j].dim_size = thefile->file.var_info[i]->dim_sizes[j];
				}
				if(thefile->file.var_att_info[i] != NULL) {
					j = 0;
					step = thefile->file.var_att_info[i];
					while(step != NULL) {
						step = step->next;
						j++;
					}
					tmp->u.var->n_atts = j;
					tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*j);
					step = thefile->file.var_att_info[i];
					j = 0;
					while(step != NULL) {
						tmp->u.var->attnames[j]= step->the_att->att_name_quark;
						j++;
						step = step->next;
					}
				} else {
					tmp->u.var->n_atts = 0;
					tmp->u.var->attnames = NULL;
				}
				tmp->next = thelist;
				thelist = tmp;
				tmp = NULL;
			}
		}
	}
	return(thelist);
}

NclApiDataList *_NclGetDefinedFileInfo
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclSymTableListNode *st;
	NclSymbol *s;
	int i,j;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid;

	st = thetablelist;
	while(st != NULL) {
                for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
                        if(st->this_scope[i].nelem != 0) {
				s = st->this_scope[i].thelist;
                                while(s != NULL) {
					if(s->type == VAR) {
						thevar = _NclRetrieveRec(s,DONT_CARE);
						if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
							tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
							tmp->kind = FILE_LIST;
							tmp->u.file = (NclApiFileInfoRec*)NclMalloc(sizeof(NclApiFileInfoRec));
							theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
							thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
							tmp->u.file->name = thevar->u.data_var->var.var_quark;
							tmp->u.file->path = thefile->file.fpath;
							tmp->u.file->wr_status = thefile->file.wr_status;
							tmp->u.file->file_type = thefile->file.file_type;
							tmp->u.file->n_dims = thefile->file.n_file_dims;
							tmp->u.file->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.file->n_dims);
							for(j = 0; j < tmp->u.file->n_dims; j++) {
								tmp->u.file->dim_info[j].dim_num = j;
								tmp->u.file->dim_info[j].dim_quark = thefile->file.file_dim_info[j]->dim_name_quark;
								tmp->u.file->dim_info[j].dim_size = thefile->file.file_dim_info[j]->dim_size;
							}
							if(thefile->file.n_vars > 0) {
								tmp->u.file->n_vars = thefile->file.n_vars;
								tmp->u.file->var_names = (NclQuark*)NclMalloc(sizeof(NclQuark)*thefile->file.n_vars);
								for(j = 0; j < thefile->file.n_vars; j++) {
									tmp->u.file->var_names[j] = thefile->file.var_info[j]->var_name_quark;
								}
							}
							if(thefile->file.n_file_atts > 0) {
								tmp->u.file->n_atts = thefile->file.n_file_atts;
								tmp->u.file->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*thefile->file.n_file_atts);
								for(j = 0; j < thefile->file.n_file_atts; j++) {
									tmp->u.file->attnames[j] = thefile->file.file_atts[j]->att_name_quark;
								}
								
							} else {
								tmp->u.file->n_atts = 0;
								tmp->u.file->attnames = NULL;
							}
							tmp->next = thelist;
							thelist = tmp;
							tmp = NULL;
						}
					}
					s = s->symnext;
                                }
                        }
                }
                st = st->previous;
        }
        return(thelist);


}

NclApiDataList *_NclGetDefinedProcFuncInfo
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclSymTableListNode *st;
	NclSymbol *s;
	NclArgTemplate *tmpargs = NULL;
	int i,j;

	st = thetablelist;
	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->this_scope[i].nelem != 0) {
				s = st->this_scope[i].thelist;
				while(s != NULL) {
					switch(s->type) {
					case FUNC:
					case NPROC: 
					case PROC:
					case NFUNC:{
					
						tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
						tmp->kind = PROCFUNC_LIST;
						tmp->u.func = (NclApiFuncInfoRec*)NclMalloc(sizeof(NclApiFuncInfoRec));
						tmp->u.func->name = NrmStringToQuark(s->name);
						tmp->u.func->nparams = s->u.procfunc->nargs;
						if((s->type == FUNC)||(s->type == NFUNC)) {
							tmp->u.func->kind = 1;
						} else {
							tmp->u.func->kind = 0;
						}
						tmpargs = s->u.procfunc->theargs;
						break;
					}
					case IPROC: {
						tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
						tmp->kind = PROCFUNC_LIST;
						tmp->u.func = (NclApiFuncInfoRec*)NclMalloc(sizeof(NclApiFuncInfoRec));
						tmp->u.func->name = NrmStringToQuark(s->name);
						tmp->u.func->nparams = s->u.bproc->nargs;
						tmp->u.func->kind = 0;
						tmpargs = s->u.bproc->theargs;
						break;
					}
					case IFUNC: {
						tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
						tmp->kind = PROCFUNC_LIST;
						tmp->u.func = (NclApiFuncInfoRec*)NclMalloc(sizeof(NclApiFuncInfoRec));
						tmp->u.func->name = NrmStringToQuark(s->name);
						tmp->u.func->nparams = s->u.bfunc->nargs;
						tmp->u.func->kind = 1;
						tmpargs = s->u.bfunc->theargs;
						break;
					}
					case EPROC:
					case EFUNC:
					default:

						break;
					}
					if(tmp != NULL) {
						if(tmpargs != NULL) {
							tmp->u.func->theargs = (NclArgTemplate*)NclMalloc(sizeof(NclArgTemplate)*tmp->u.func->nparams);
							for(j = 0; j < tmp->u.func->nparams; j++) {
								tmp->u.func->theargs[j] = tmpargs[j];
							}
						} else {
							tmp->u.func->theargs= NULL;
						}
						
						tmp->next = thelist;
						thelist = tmp;
						tmp = NULL;
					}
					s = s->symnext;
				}
			}
		}
		st = st->previous;
	}	
	return(thelist);
}

NclExtValueRec *_NclGetHLUObjId
#if	NhlNeedProto
(char *varname)
#else
(varname)
        char *varname;
#endif
{
	NclSymbol *thesym = _NclLookUp(varname);
	NclStackEntry *the_var;
	NclMultiDValData the_hlu = NULL;
	NclHLUObj hlu = NULL;
	NclExtValueRec *tmp = NULL;
	int i;
	int *value;
	


	if(thesym != NULL) {
		the_var = _NclRetrieveRec(thesym,DONT_CARE);	
		tmp = (NclExtValueRec*)NclMalloc((unsigned)sizeof(NclExtValueRec));
		tmp->constant = 0;
		if((the_var->kind == NclStk_VAR)&&((the_var->u.data_var->obj.obj_type_mask & Ncl_HLUVar))) {
			the_hlu = _NclVarValueRead(the_var->u.data_var,NULL,NULL);
			tmp->type = the_hlu->multidval.data_type;
			tmp->totalelements = the_hlu->multidval.totalelements;
			tmp->elem_size = the_hlu->multidval.type->type_class.size;
			tmp->n_dims = the_hlu->multidval.n_dims;
			value = NULL;
			if(the_hlu != NULL) {
				value = (int*)NclMalloc(sizeof(int)* the_hlu->multidval.totalelements);
				if(the_hlu->multidval.kind == SCALAR) {
					if((!the_hlu->multidval.missing_value.has_missing)||(*((int*)the_hlu->multidval.val)!= the_hlu->multidval.missing_value.value.objval)) {
						hlu = (NclHLUObj)_NclGetObj(*((int*)the_hlu->multidval.val));
						*value = hlu->hlu.hlu_id;
					} else {
						*value = the_hlu->multidval.missing_value.value.objval;
					}
					tmp->dim_sizes[0] = 1;
					tmp->value = (void*)value;
					
					if(hlu != NULL) {
						return(tmp);
					} 
				} else {
					for(i = 0; i<the_hlu->multidval.n_dims; i++) {
						tmp->dim_sizes[i] = the_hlu->multidval.dim_sizes[i];

						if((!the_hlu->multidval.missing_value.has_missing)||(((int*)the_hlu->multidval.val)[i] != the_hlu->multidval.missing_value.value.objval)) {
							hlu = (NclHLUObj)_NclGetObj(((int*)the_hlu->multidval.val)[i]);
							value[i] = hlu->hlu.hlu_id;
						} else {
							value[i] = the_hlu->multidval.missing_value.value.objval;
						}
					}
					tmp->value = (void*)value;
				}
			}
			if(tmp->value != NULL) {
				return(tmp);
			} else {
				NclFree(tmp);
			}
		} 
	} 
       	return(NULL);
}

NclApiDataList * _NclGetDefinedHLUInfo
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclMultiDValData the_hlu = NULL;
	NclHLUObj hlu = NULL;
	NclSymTableListNode *st;
	NclSymbol *s;
	int i,j,not_ok = 0;
	NclStackEntry *the_var;

	st = thetablelist;

	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->this_scope[i].nelem != 0) {
				s = st->this_scope[i].thelist;
				while(s != NULL) {
					if(s->type == VAR) {
						the_var = _NclRetrieveRec(s,DONT_CARE);	
						if((the_var->kind == NclStk_VAR)&&((the_var->u.data_var->obj.obj_type_mask & Ncl_HLUVar))) {
							the_hlu = _NclVarValueRead(the_var->u.data_var,NULL,NULL);
							if(the_hlu != NULL) {
								tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
								tmp->kind = HLU_LIST;
								tmp->u.hlu = (NclApiHLUVarInfoRec*)NclMalloc(sizeof(NclApiHLUVarInfoRec));
								tmp->u.hlu->n_objs = the_hlu->multidval.totalelements;
								tmp->u.hlu->objs = (NclApiHLUInfoRec*)NclMalloc(sizeof(NclApiHLUInfoRec)*tmp->u.hlu->n_objs);
								tmp->u.hlu->name = the_var->u.data_var->var.var_quark;
								for(j = 0 ;  j < the_hlu->multidval.totalelements; j++) {
									if(!(the_hlu->multidval.missing_value.has_missing)||(((int*)the_hlu->multidval.val)[j]!= the_hlu->multidval.missing_value.value.objval)) {
										hlu = (NclHLUObj)_NclGetObj(((int*)the_hlu->multidval.val)[j]);
										if(hlu != NULL) {
											tmp->u.hlu->objs[j].obj_name = NrmStringToQuark(NhlName(hlu->hlu.hlu_id));
											tmp->u.hlu->objs[j].obj_class= NrmStringToQuark(NhlClassName(hlu->hlu.hlu_id));
											tmp->u.hlu->objs[j].obj_id = hlu->hlu.hlu_id;
										} else {
											not_ok = 1;
										} 
									} else {
										tmp->u.hlu->objs[j].obj_name = 0;
										tmp->u.hlu->objs[j].obj_class= 0;
										tmp->u.hlu->objs[j].obj_id = the_hlu->multidval.missing_value.value.objval;
									}
								}
								if(not_ok) {
									NclFree(tmp->u.hlu->objs);
									NclFree(tmp->u.hlu);
									NclFree(tmp);
									not_ok = 0;
									tmp = NULL;
								} else {
									tmp->next = thelist;
									thelist = tmp;
									tmp = NULL;
								}
							}
						}
					}
					s = s->symnext;
				}
			}
			
		}
		st = st->previous;
	}
	return(thelist);
}
NclApiDataList *_NclGetDefinedVarInfo
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp = NULL,*thelist = NULL;
	NclAtt tmp_att = NULL;
	NclAttList *att_list = NULL;
	NclMultiDValData the_value = NULL;
	NclSymTableListNode *st;
	NclSymbol *s;
	int i,j;
	NclStackEntry *the_var;

	st = thetablelist;

	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->this_scope[i].nelem != 0) {
				s = st->this_scope[i].thelist;
				while(s != NULL) {
					if(s->type == VAR) {
						the_var = _NclRetrieveRec(s,DONT_CARE);	
						if((the_var->kind == NclStk_VAR)&&(!(the_var->u.data_var->obj.obj_type_mask & (Ncl_FileVar|Ncl_HLUVar)))) {
/*&& (the_var->u.data_var->var.var_type == NORMAL)) */

							tmp = (NclApiDataList*)NclMalloc
								(sizeof(NclApiDataList));
							tmp->kind = VARIABLE_LIST;
							tmp->u.var = (NclApiVarInfoRec*)
								NclMalloc(sizeof(NclApiVarInfoRec));
							tmp->u.var->name = the_var->u.data_var->var.var_quark;
							the_value = (NclMultiDValData)_NclGetObj(the_var->u.data_var->var.thevalue_id);
							tmp->u.var->data_type_quark = NrmStringToQuark(_NclBasicDataTypeToName(the_value->multidval.data_type));
							tmp->u.var->type = the_var->u.data_var->var.var_type;
							tmp->u.var->n_dims = the_var->u.data_var->var.n_dims;
							tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*the_var->u.data_var->var.n_dims);
							for(j = 0; j < the_var->u.data_var->var.n_dims; j++) {
								tmp->u.var->dim_info[j]= the_var->u.data_var->var.dim_info[j];
							}
							if(the_var->u.data_var->var.att_id != -1) {
	
								tmp_att= (NclAtt)_NclGetObj(the_var->u.data_var->var.att_id);
								att_list = tmp_att->att.att_list;
								tmp->u.var->n_atts = tmp_att->att.n_atts;
								tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*tmp_att->att.n_atts);
							
								j = 0;	
								while(att_list != NULL) {
									tmp->u.var->attnames[j] = att_list->quark;
									att_list = att_list->next;
									j++;
								}
							} else {
								tmp->u.var->n_atts = 0;
								tmp->u.var->attnames = NULL;
							}
							tmp->next = thelist;
							thelist = tmp;
							tmp = NULL;
						}
					}
					s = s->symnext;
				}
			}
			
		}
		st = st->previous;
	}
	return(thelist);
}
void _NclFreeApiDataList
#if	NhlNeedProto
(void* l)
#else
(l)
	void *l;
#endif
{
	NclApiDataList *tmp,*list = (NclApiDataList*)l;

	while(list != NULL) {
		switch(list->kind) {
		case VARIABLE_LIST: {
			if(list->u.var->dim_info != NULL) 
				NclFree(list->u.var->dim_info);
			if(list->u.var->attnames != NULL)
				NclFree(list->u.var->attnames);
			NclFree(list->u.var);
		}
		break;
		case PROCFUNC_LIST:{
			if(list->u.func->theargs != NULL)	
				NclFree(list->u.func->theargs); 
			NclFree(list->u.func);
		}
		break;
		case FILE_LIST: {
			if(list->u.file->dim_info != NULL)
				NclFree(list->u.file->dim_info);
			if(list->u.file->attnames != NULL)
				NclFree(list->u.file->attnames);
			if(list->u.file->var_names != NULL)
				NclFree(list->u.file->var_names);
				NclFree(list->u.file);
		}
		case HLU_LIST: {
			if(list->u.hlu->objs != NULL) 
				NclFree(list->u.hlu->objs);
			NclFree(list->u.hlu);
	
		}
		break;
		default:
			break;
		}
		tmp = list;
		list = list->next;
		NclFree(tmp);
	}
}

extern struct _NclExtValueRec *_NclGetVarValue
#if	NhlNeedProto
(NclSymbol *the_sym, int copy_data)
#else
(the_sym, copy_data)
NclSymbol *the_sym;
int copy_data;
#endif
{
	NclExtValueRec* tmp;
	NclStackEntry *the_var;
	NclMultiDValData the_val; 
	int i;

	tmp = (NclExtValueRec*)NclMalloc((unsigned)sizeof(NclExtValueRec));
	the_var = _NclRetrieveRec(the_sym,DONT_CARE);
	if(the_var != NULL) {
		the_val = _NclVarValueRead(the_var->u.data_var,NULL,NULL);
		tmp->constant = !copy_data;
		if(copy_data) {
			tmp->value = (void*)NclMalloc(the_val->multidval.totalsize);
			memcpy(tmp->value,the_val->multidval.val,the_val->multidval.totalsize);
		} else {
			tmp->value = the_val->multidval.val;
		}
		tmp->totalelements = the_val->multidval.totalelements;
		tmp->elem_size = the_val->multidval.totalsize/the_val->multidval.totalelements;
		tmp->type = (int)the_val->multidval.data_type;
		for(i = 0; i < the_val->multidval.n_dims; i++) {
			tmp->dim_sizes[i] = the_val->multidval.dim_sizes[i];
		}
		return(tmp);
	} 
	return(NULL);
}


#ifdef __cplusplus
}
#endif

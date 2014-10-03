/*
 *      $Id: Symbol.c,v 1.74 2010-04-14 21:29:48 huangwei Exp $
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
#include <ncarg/hlu/Callbacks.h>
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
#ifdef USE_NETCDF4_FEATURES
#include "NclAdvancedFile.h"
#endif
#include "FileSupport.h"
#include "VarSupport.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "NclHLUObj.h"
#include "NclApi.h"
#include "FileSupport.h"
#include "NclTypeobj.h"
#include "NclProf.h"
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

extern void NclAddUserBuiltInFuncs(
#if NhlNeedProto
void
#endif
);

extern void NclAddJavaBuiltInFuncs(
#if NhlNeedProto
void
#endif
);

extern void NclAddUserFuncs(
#if NhlNeedProto
void
#endif
);

#ifdef BuildOpenCL
extern void NclAddOpenCLBuiltInFuncs(void);
#endif

void _NclFreeProcFuncInfo
#if	NhlNeedProto
(NclSymbol *sym)
#else
(sym)
NclSymbol *sym;
#endif
{
	switch(sym->type) {
		case IFUNC:
			NclFree(sym->u.bfunc->theargs);
			NclFree(sym->u.bfunc->thescope);
			NclFree(sym->u.bfunc);
			sym->u.bfunc = NULL;
			break;
		case IPROC:
		case PIPROC:
			NclFree(sym->u.bproc->theargs);
			NclFree(sym->u.bproc->thescope);
			NclFree(sym->u.bproc);
			sym->u.bproc = NULL;
			break;
		case NPROC:
		case NFUNC:
/*
			for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
				if(sym->u.procfunc->thescope->this_scope[i].nelem != 0) {
					s = sym->u.procfunc->thescope->this_scope[i].thelist;
					while(s != NULL) {
						tmps = s;
						switch(s->type) {
						case IPROC:
						case PIPROC:
						case IFUNC:
						case NPROC:
						case NFUNC:
							_NclFreeProcFuncInfo(s);
						}
						s = s->symnext;
						NclFree(tmps);
					}
				}
				
			}
			NclFree(sym->u.procfunc->thescope->this_scope);
			tmp = (_NclMachineRec*)sym->u.procfunc->mach_rec_ptr;
			NclFree(tmp->themachine);
			NclFree(tmp->thefiles);
			NclFree(tmp->thelines);
			NclFree(sym->u.procfunc->mach_rec_ptr);
			NclFree(sym->u.procfunc->theargs);
			NclFree(sym->u.procfunc->thescope);
			NclFree(sym->u.procfunc);
*/
			sym->u.procfunc = NULL;
			break;

	}
	sym->type = UNDEF;
}

NhlErrorTypes _NclWalkSymTable 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclSymTableListNode *st;
	NclSymbol *s;
	int i;

	st = thetablelist;
	while(st != NULL) {
                for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
                        if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
                                while(s != NULL) {
					switch(s->type) {
					case VAR:
						fprintf(stdout,"VAR: %s\n",s->name);
						break;
					case UNDEF:
						fprintf(stdout,"UNDEF: %s\n",s->name);
						break;
					case NPROC: 
						fprintf(stdout,"NPROC: %s\n",s->name);
						if(s->u.procfunc->mach_rec_ptr != NULL) {
							_NclPushMachine(s->u.procfunc->mach_rec_ptr);
							_NclPrintMachine(-1,-1,stdout);
							(void) _NclPopMachine();
						}
						break;
					case NFUNC:
						fprintf(stdout,"NFUNC: %s\n",s->name);
						if(s->u.procfunc->mach_rec_ptr != NULL) {
							_NclPushMachine(s->u.procfunc->mach_rec_ptr);
							_NclPrintMachine(-1,-1,stdout);
							(void) _NclPopMachine();
						}
						break;
					case IPROC: 
						fprintf(stdout,"IPROC: %s\n",s->name);
						break;
					case IFUNC: 
						fprintf(stdout,"IFUNC: %s\n",s->name);
						break;
					}
					s = s->symnext;
                                }
                        }
                }
                st = st->previous;
        }
        return(NhlNOERROR);
}
static NclSelectionRecord *BuildSel
#if     NhlNeedProto
(int n_dims, ng_size_t *dimsizes,long* start, long* finish, long* stride)
#else
(n_dims, *dimsizes, start, finish, stride)
int n_dims;
ng_size_t *dimsizes;
long* start;
long* finish;
long* stride;
#endif
{
	NclSelectionRecord *sel_ptr = NULL;
	int i;


	if((start == NULL)&&(finish == NULL)&&(stride == NULL)) {
/*
* Whole selection
*/
		return(NULL);
	} else if((finish == NULL)&&(start != NULL)&&(stride == NULL)) {
/*
* Scalar selection
*/
		sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
		sel_ptr->n_entries = n_dims;
		for(i = 0; i < n_dims; i++) {
			sel_ptr->selection[i].sel_type = Ncl_SUBSCR;
			sel_ptr->selection[i].dim_num = i;
			sel_ptr->selection[i].u.sub.start = start[i];
			sel_ptr->selection[i].u.sub.finish= start[i];
			sel_ptr->selection[i].u.sub.stride = 1;
			sel_ptr->selection[i].u.sub.is_single = 1;
		}
	} else if((start == NULL)&&(finish == NULL)&&(stride != NULL)) {
/*
* Whole selection strided
*/
		sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
		sel_ptr->n_entries = n_dims;
		for(i = 0; i < n_dims; i++) {
			sel_ptr->selection[i].sel_type = Ncl_SUBSCR;
			sel_ptr->selection[i].dim_num = i;
			sel_ptr->selection[i].u.sub.start = 0;
			sel_ptr->selection[i].u.sub.finish= dimsizes[i] - 1;
			sel_ptr->selection[i].u.sub.stride = stride[i];
			sel_ptr->selection[i].u.sub.is_single =
				dimsizes[i]/stride[i] > 1 ? 0 : 1;

		}

	} else if((stride == NULL)&&(start != NULL)&&(finish!= NULL)) {
/*
* Subselection no stride
*/
		sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
		sel_ptr->n_entries = n_dims;
		for(i = 0; i < n_dims; i++) {
			sel_ptr->selection[i].sel_type = Ncl_SUBSCR;
			sel_ptr->selection[i].dim_num = i;
			sel_ptr->selection[i].u.sub.start = start[i];
			sel_ptr->selection[i].u.sub.finish= finish[i];
			sel_ptr->selection[i].u.sub.stride = 1;
			sel_ptr->selection[i].u.sub.is_single =
				abs(finish[i] - start[i]) > 0 ? 0 : 1;
		}

	} else if((stride != NULL)&&(start != NULL)&&(finish!= NULL)) {

/*
* Subselection with stride
*/
		sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
		sel_ptr->n_entries = n_dims;
		for(i = 0; i < n_dims; i++) {
			sel_ptr->selection[i].sel_type = Ncl_SUBSCR;
			sel_ptr->selection[i].dim_num = i;
			sel_ptr->selection[i].u.sub.start = start[i];
			sel_ptr->selection[i].u.sub.finish= finish[i];
			sel_ptr->selection[i].u.sub.stride = stride[i];
			sel_ptr->selection[i].u.sub.is_single =
				abs((finish[i] - start[i])/stride[i]) > 0 ? 0 : 1;
		}

	}
	return(sel_ptr);
}


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
	thetablelist->sr = (NclScopeRec*) NclMalloc((unsigned)sizeof(NclScopeRec));
	if(thetablelist->sr == NULL) {
		NhlPError(NhlFATAL,errno,"InitSymbol: Can't create symbol table list");
		return(0);
	}

	thetablelist->sr->level = 0;
	thetablelist->sr->cur_offset = 0;
	thetablelist->sr->this_scope = (NclSymTableElem*) NclMalloc((unsigned) 
		sizeof(NclSymTableElem) * NCL_SYM_TAB_SIZE);
	thetablelist->previous = NULL;

	if(thetablelist->sr->this_scope == NULL) {
		NhlPError(NhlFATAL,errno,"InitSymbol: Can't create symbol table");
		return(0);
	}
/*
* Clear first symbol table
*/
	for(i = 0; i< NCL_SYM_TAB_SIZE; i++) {
		thetablelist->sr->this_scope[i].nelem = 0;
		thetablelist->sr->this_scope[i].thelist = NULL;
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
	NclAddUserBuiltInFuncs();
	NclAddJavaBuiltInFuncs();
#ifdef BuildOpenCL
	NclAddOpenCLBuiltInFuncs();
#endif
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

int _NclFinalizeSymbol()
{
	_NclDeleteNewSymStack();
	return 0;
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
	if((s == NULL)||((s->type == UNDEF)||(_NclGetCurrentScopeLevel() > s->level))) {
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
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterFunc: %s is already a defined symbol can't add it as built-in ",fname);
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
	if((s == NULL)||((s->type == UNDEF)||(_NclGetCurrentScopeLevel() > s->level))) {
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
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterProc: %s is already a defined symbol can't add it as built-in ",fname);
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
	return((void*)NclCalloc(n, sizeof(NclArgTemplate)));
}
void SetArgTemplate
#if NhlNeedProto
(void *args, int arg_num, char *type_name, int n_dims, ng_size_t *dimsizes)
#else
(args, arg_num, type_name, n_dims, dimsizes)
void *args;
int arg_num;
char *type_name;
int n_dims;
ng_size_t *dimsizes;
#endif
{
	NclArgTemplate* the_args = (NclArgTemplate*) &(((NclArgTemplate*)args)[arg_num]);
	int i;
	if(the_args == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Error adding argument template for intrinsic function NULL arg record passed");
		return;
	}
	the_args->n_dims = n_dims;
	if(dimsizes != NULL) {
		the_args->is_dimsizes = 1;
		memcpy((void*)the_args->dim_sizes,(void*)dimsizes,sizeof(ng_size_t)*n_dims);
	} else {
		the_args->is_dimsizes = 0;
		for(i = 0; i < NCL_MAX_DIMENSIONS; i++) {
			the_args->dim_sizes[i] = -1;
		}
	}

	if(type_name != NULL) {	
		the_args->arg_data_type = _NclLookUp(type_name);
		if(the_args->arg_data_type == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Error adding argument template for intrinsic function");
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
	news->sr = (NclScopeRec*)NclMalloc((unsigned)sizeof(NclScopeRec));
	if(news->sr == NULL) {
		NhlPError(NhlFATAL, errno, "NewScope: Can't create a new symbol table node");
		return(0);
	}

	news->sr->this_scope = (NclSymTableElem *) NclMalloc((unsigned)
				sizeof(NclSymTableElem) * NCL_SYM_TAB_SIZE);


	if(news->sr->this_scope == NULL) {
		NhlPError(NhlFATAL, errno, "NewScope: Can't create a new symbol table");
		return(0);
	}

/* 
* Everytime a new symbol is added to this scope this value is incremented
* It will be a multiplier to use to figure out the location of the identifiers
* value with respect to the frames base pointer.
*/
	news->sr->cur_offset = 0;

	news->sr->level = thetablelist->sr->level + 1;
	news->previous = thetablelist;
	thetablelist = news;
	
	for(i = 0; i< NCL_SYM_TAB_SIZE; i++) {
		thetablelist->sr->this_scope[i].nelem = 0;
		thetablelist->sr->this_scope[i].thelist = NULL;
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
NclScopeRec * _NclPopScope 
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScopeRec *tmp;
	NclSymTableListNode *tmp1;
	
	if(thetablelist == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"PopScope: Symbol table stack underflow");
		return(NULL);
	}

	tmp = thetablelist->sr;
	tmp1 = thetablelist->previous;
	NclFree(thetablelist);
	thetablelist = tmp1;
	return(tmp);
}

void _NclPushScope 
#if	NhlNeedProto
(NclScopeRec * the_scope)
#else
(the_scope)
NclScopeRec * the_scope;
#endif
{
	NclSymTableListNode *tmp;
	

	tmp = (NclSymTableListNode*)NclMalloc(sizeof(NclSymTableListNode));
	tmp->sr = the_scope;
	tmp->previous = thetablelist;
	thetablelist = tmp;
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
        unsigned h = 0, g;

        for(p = name; *p != '\0'; p = p +1) {
                h = (h<<4) + (*p);
                if((g = h) & 0xf0000000) {
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
 *		type    integer type identifier which corresponds to token
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
	s->level = thetablelist->sr->level;
	s->type = type;
	s->ind = index;
	thetablelist->sr->this_scope[index].nelem++;
	if(thetablelist->sr->this_scope[index].thelist != NULL) {
		thetablelist->sr->this_scope[index].thelist->sympre = s;
	}
	s->symnext = thetablelist->sr->this_scope[index].thelist;
	s->sympre = NULL;
	thetablelist->sr->this_scope[index].thelist = s;
	s->u.var = NULL;
	s->offset = thetablelist->sr->cur_offset++;

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
               	 if(step->sr->level == new_sym_stack[i]->level)
			break;
               	 else
               	         step = step->previous;
        	}
		if(step != NULL) {
			step->sr->cur_offset = new_sym_stack[i]->offset;
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
	return(thetablelist->sr->level);
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
		if(step->sr->level == sym->level) 	
			break;
		else 
			step = step->previous;
	}
	step->sr->this_scope[sym->ind].nelem--;
	if((sym->sympre == NULL) &&(sym->symnext == NULL)) {
		step->sr->this_scope[sym->ind].thelist = NULL;
		if(step->sr->this_scope[sym->ind].nelem != 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NhlDeleteSym: Ack!! a big problem has just occurred in the symbol table");
		}
	} else if(sym->sympre == NULL) {
/*
* New top of list
*/
		step->sr->this_scope[sym->ind].thelist = sym->symnext;
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
	case UNDEFFILEVAR:
	case FILEVAR:
	case NPROC:
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
		s = st->sr->this_scope[index].thelist;
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
(NclScopeRec * thetable, char *name)
#else
(thetable, name)
NclScopeRec *thetable;
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

void _NclUndefSymbolsInScope
#if	NhlNeedProto
(NclProcFuncInfo *procfunc_info)
#else
(procfunc_info)
NclProcFuncInfo *procfunc_info;
#endif
{
	NclScopeRec *sr = procfunc_info->thescope;
	NclSymbol *s;
        int i,j;

	for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
		if(sr->this_scope[i].nelem != 0) {
			s = sr->this_scope[i].thelist;
			while (s != NULL) {
				for (j = 0; j < procfunc_info->nargs; j++) {
					if (s == procfunc_info->theargs[j].arg_sym) {
						break;
					}
				}
				if (j == procfunc_info->nargs)
					s->type = UNDEF;
				s = s->symnext;
			}
		}
	}
}

NclSymbol *_NclAddInScope
#if	NhlNeedProto
(NclScopeRec *thetable, char* name, int type)
#else
(thetable,name,type)
	NclScopeRec *thetable;
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
(NclScopeRec *thetable, NclSymbol *sym)
#else
(thetable, sym)
NclScopeRec *thetable;
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
	FILE *ffp = stdout;
	if(fp != NULL)  {
		ffp = fp;
	}

	st = thetablelist;

	while(st != NULL) {
		fprintf(ffp,"Level: %d\n",st->sr->level);
		fprintf(ffp,"Current Offset: %d\n",st->sr->cur_offset);
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->sr->this_scope[i].nelem != 0) {
				fprintf(ffp,"\tIndex: %d\n",i);
				s = st->sr->this_scope[i].thelist;
				while(s != NULL) {
					fprintf(ffp,"\t\t: %d) %s\n",s->offset,s->name);
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

	s = _NclLookUp(name);
	if(s == NULL) {
		s = _NclAddSym(name,OBJTYPE);
		s->u.obj_class_ptr = the_ptr;
	} else if(s->type == OBJTYPE) {
		s->u.obj_class_ptr = the_ptr;
	} else {
		_NclChangeSymbolType(s,OBJTYPE);
		s->u.obj_class_ptr = the_ptr;
	}
	return;
}

#ifdef USE_NETCDF4_FEATURES
static NclApiDataList *getAdvancedFileVarInfoList(NclFile thefile)
{
    NclApiDataList *tmp = NULL;
    NclApiDataList *thelist = NULL;
    NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
    NclFileVarNode *varnode = NULL;
    int i, j;

    if(NULL != theadvancedfile->advancedfile.grpnode->var_rec)
    {
        for(i = 0; i < theadvancedfile->advancedfile.grpnode->var_rec->n_vars; ++i)
        {
            varnode = &(theadvancedfile->advancedfile.grpnode->var_rec->var_node[i]);

            tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
            tmp->kind = VARIABLE_LIST;
            tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
            tmp->u.var->name = varnode->name;
            tmp->u.var->data_type = varnode->type;
            tmp->u.var->type = FILEVAR;
            if(NULL != varnode->dim_rec)
            {
                tmp->u.var->n_dims = varnode->dim_rec->n_dims;
                if(tmp->u.var->n_dims)
                    tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.var->n_dims);
                else
                    tmp->u.var->dim_info = NULL;

                for(j = 0 ; j < tmp->u.var->n_dims ; ++j)
                {
                    tmp->u.var->dim_info[j].dim_quark = varnode->dim_rec->dim_node[j].name;
                    tmp->u.var->dim_info[j].dim_num   = j;
                    tmp->u.var->dim_info[j].dim_size  = varnode->dim_rec->dim_node[j].size;
                    tmp->u.var->coordnames[j] = varnode->dim_rec->dim_node[j].name;
                }
            }
            else
            {
                tmp->u.var->n_dims = 0;
                tmp->u.var->dim_info = NULL;
            }
 
            if(NULL != varnode->att_rec)
            {
                tmp->u.var->n_atts = varnode->att_rec->n_atts;
                tmp->u.var->attnames = (NclQuark*)NclMalloc(tmp->u.var->n_atts * sizeof(NclQuark));
                for(j = 0; j < tmp->u.var->n_atts; ++j)
                {
                    tmp->u.var->attnames[j] = varnode->att_rec->att_node[j].name;
                }
            }
            else
            {
                tmp->u.var->n_atts = 0;
                tmp->u.var->attnames = NULL;
            }
            tmp->next = thelist;
            thelist = tmp;
            tmp = NULL;
        }
    }
    return (thelist);
}
#endif

NclApiDataList *_NclGetFileVarInfoList
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
	if((s != NULL)&&(s->type != UNDEF))  {
		thevar = _NclRetrieveRec(s,DONT_CARE);
		if(thevar->kind == NclStk_VAR) {
			theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
			if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
				if(thefile != NULL)
				{
#ifdef USE_NETCDF4_FEATURES
				if(thefile->file.advanced_file_structure)
				{
					fprintf(stderr, "\nHit _NclGetFileVarInfoList in file: %s, line: %d\n", __FILE__, __LINE__);
					thelist = getAdvancedFileVarInfoList(thefile);
				}
				else
#endif
				{
					for(i = 0; i < thefile->file.n_vars; i++) {
						tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
						tmp->kind = VARIABLE_LIST;
						tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
						tmp->u.var->name = thefile->file.var_info[i]->var_name_quark;
						tmp->u.var->data_type = thefile->file.var_info[i]->data_type;
						tmp->u.var->type = FILEVAR;
						tmp->u.var->n_dims = thefile->file.var_info[i]->num_dimensions;
						if(tmp->u.var->n_dims)
							tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.var->n_dims);
						else
							tmp->u.var->dim_info = NULL;

						for(j = 0 ; j < tmp->u.var->n_dims ; j++) {
							tmp->u.var->dim_info[j].dim_quark =thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
							tmp->u.var->dim_info[j].dim_num = thefile->file.var_info[i]->file_dim_num[j];
							tmp->u.var->dim_info[j].dim_size = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
							if(thefile->file.coord_vars[thefile->file.var_info[i]->file_dim_num[j]] != NULL) {
								tmp->u.var->coordnames[j] = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;

							} else {
								tmp->u.var->coordnames[j] = -1;
							}
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
			}
		}
	}
	return(thelist);
}
NclApiDataList *_NclGetFileVarInfo
#if	NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name)
#else
(file_sym_name,file_var_name)
NclQuark file_sym_name;
NclQuark file_var_name;
#endif
{
	NclApiDataList *tmp = NULL;
	NclSymbol *s = NULL;
	int i,j;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid = NULL;
	NclFileAttInfoList *step;



	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		thevar = _NclRetrieveRec(s,DONT_CARE);
		if(thevar->kind == NclStk_VAR ){
			theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
			if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

				if(thefile != NULL)
				{
#ifdef USE_NETCDF4_FEATURES
				if(thefile->file.advanced_file_structure)
				{
					NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
					NclFileVarNode *varnode = _getVarNodeFromNclFileGrpNode(theadvancedfile->advancedfile.grpnode, file_var_name);

					if(NULL != varnode)
					{
						tmp = (NclApiDataList*)NclCalloc(1, sizeof(NclApiDataList));
						tmp->kind = VARIABLE_LIST;
						tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
						tmp->u.var->name = varnode->name;
						tmp->u.var->data_type= varnode->type;
						tmp->u.var->type = FILEVAR;
						tmp->u.var->n_dims = varnode->dim_rec->n_dims;
						tmp->u.var->dim_info = (NclDimRec*)NclCalloc(tmp->u.var->n_dims, sizeof(NclDimRec));
						for(j = 0 ; j < tmp->u.var->n_dims ; ++j)
						{
							tmp->u.var->dim_info[j].dim_quark = varnode->dim_rec->dim_node[j].name;
							tmp->u.var->dim_info[j].dim_num   = j;
							tmp->u.var->dim_info[j].dim_size  = varnode->dim_rec->dim_node[j].size;

							tmp->u.var->coordnames[j] = varnode->dim_rec->dim_node[j].name;
						}

						tmp->u.var->n_atts = 0;
						tmp->u.var->attnames = NULL;
						if(NULL != varnode->att_rec)
						{
							if(0 < varnode->att_rec->n_atts)
							{
								tmp->u.var->n_atts = varnode->att_rec->n_atts;
								tmp->u.var->attnames = (NclQuark*)NclCalloc(varnode->att_rec->n_atts, sizeof(NclQuark));
								for(j = 0; j < varnode->att_rec->n_atts; ++j)
									tmp->u.var->attnames[j] = varnode->att_rec->att_node[j].name;
							}
						}
						tmp->next = NULL;
						return(tmp);
					}
				}
				else
#endif
				{
					for(i = 0; i < thefile->file.n_vars; i++) {
						if(thefile->file.var_info[i]->var_name_quark == file_var_name) {
							tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
							tmp->kind = VARIABLE_LIST;
							tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
							tmp->u.var->name = thefile->file.var_info[i]->var_name_quark;
							tmp->u.var->data_type= thefile->file.var_info[i]->data_type;
							tmp->u.var->type = FILEVAR;
							tmp->u.var->n_dims = thefile->file.var_info[i]->num_dimensions;
							tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.var->n_dims);
							for(j = 0 ; j < tmp->u.var->n_dims ; j++) {
								tmp->u.var->dim_info[j].dim_quark =thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
								tmp->u.var->dim_info[j].dim_num = thefile->file.var_info[i]->file_dim_num[j];
								tmp->u.var->dim_info[j].dim_size = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;
								if(thefile->file.coord_vars[thefile->file.var_info[i]->file_dim_num[j]] != NULL) {	
									tmp->u.var->coordnames[j] =  thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
								} else {
									tmp->u.var->coordnames[j] = -1;
								}
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
							tmp->next = NULL;
							return(tmp);
						}
					}
				}
				}
			}
		}
	}
	return(NULL);
}

extern NclQuark *_NclGetFileCompoundVarComponentInfo(NclQuark file_sym_name, NclQuark file_var_name, ng_size_t* num_components)
{
	NclQuark *component_names = NULL;

#ifdef USE_NETCDF4_FEATURES
	NclSymbol *s = NULL;
	int n;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid = NULL;
	NclFileAttInfoList *step;

	*num_components = 0;

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((NULL == s) || (UNDEF == s->type))
		return component_names;

	thevar = _NclRetrieveRec(s,DONT_CARE);
	if(NclStk_VAR != thevar->kind)
		return component_names;

	theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
	if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData)
	{
		thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

		if((NULL == thefile) || (0 == thefile->file.advanced_file_structure))
			return component_names;
		else
		{
			NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
			NclFileVarNode *varnode = _getVarNodeFromNclFileGrpNode(theadvancedfile->advancedfile.grpnode, file_var_name);

			if((NULL == varnode) || ( 0 == varnode->is_compound))
				return component_names;

			if((NULL == varnode->comprec) || (1 > varnode->comprec->n_comps))
				return component_names;

			component_names = (NclQuark*)NclCalloc(varnode->comprec->n_comps, sizeof(NclQuark));
			*num_components = varnode->comprec->n_comps;
			for(n = 0; n < varnode->comprec->n_comps; ++n)
				component_names[n] = varnode->comprec->compnode[n].name;
		}
	}
#else
	*num_components = 0;
#endif

	return component_names;
}

#ifdef USE_NETCDF4_FEATURES
static NclApiDataList *getAdvancedFileVarCoordInfo(NclFile thefile,
                                  NclQuark coordname)
{
    int i,k;
    NclApiDataList *tmp = NULL;
    NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
    NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;
    NclFileVarNode *varnode = NULL;
    NclFileDimNode *dimnode = NULL;

    if(NULL != grpnode->dim_rec)
    {
        for(i = 0; i < grpnode->dim_rec->n_dims; ++i)
        {
            dimnode = &(grpnode->dim_rec->dim_node[i]);
            if(coordname == dimnode->name)
            {
                tmp = (NclApiDataList*)NclCalloc(1, sizeof(NclApiDataList));
                tmp->next = NULL;
                tmp->kind = VARIABLE_LIST;
                tmp->u.var->n_atts = 0;
                tmp->u.var->attnames = NULL;
                tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
                tmp->u.var->name = coordname;
                tmp->u.var->data_type = NCL_none;
                tmp->u.var->type = COORD;
                tmp->u.var->n_dims = 1;
                tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec));
                tmp->u.var->dim_info->dim_quark = coordname;
                tmp->u.var->dim_info->dim_num = i;
                tmp->u.var->dim_info->dim_size = dimnode->size;
                tmp->u.var->coordnames[0] = -1;    

                varnode = _getVarNodeFromNclFileGrpNode(grpnode, coordname);
                if(NULL != varnode)
                {
                    if(NULL != varnode->att_rec)
                    {
                        tmp->u.var->n_atts = varnode->att_rec->n_atts;
                        tmp->u.var->attnames = (NclQuark*)NclMalloc(tmp->u.var->n_atts * sizeof(NclQuark));
                        for(k = 0; k < varnode->att_rec->n_atts; ++k)
                            tmp->u.var->attnames[k]= varnode->att_rec->att_node[k].name;
                    }
                    tmp->u.var->data_type = varnode->type;
                }
                break;
            }
        }
    }
    return tmp;
}
#endif

NclApiDataList *_NclGetFileVarCoordInfo
#if	NhlNeedProto
(NclQuark file_sym_name,NclQuark file_var_name,NclQuark coordname)
#else
(file_sym_name,file_var_name,coordname)
NclQuark file_sym_name;
NclQuark file_var_name;
NclQuark coordname;
#endif
{
	NclApiDataList *tmp = NULL;
	NclSymbol *s = NULL;
	int i,j,k;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid = NULL;
	NclFileAttInfoList *step;

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		thevar = _NclRetrieveRec(s,DONT_CARE);
		if(thevar->kind == NclStk_VAR ){
			theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
			if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

				if(NULL != thefile)
				{
#ifdef USE_NETCDF4_FEATURES
				if(thefile->file.advanced_file_structure)
				{
					return (getAdvancedFileVarCoordInfo(thefile, coordname));
				}
#endif

				if(_NclFileVarIsCoord(thefile,coordname) != -1)
				{
					for(i = 0; i < thefile->file.n_file_dims; i++) {
						if((thefile->file.coord_vars[i] != NULL)&&(thefile->file.coord_vars[i]->var_name_quark == coordname)) {
							tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
							tmp->kind = VARIABLE_LIST;
							tmp->u.var = (NclApiVarInfoRec*)NclMalloc(sizeof(NclApiVarInfoRec));
							tmp->u.var->name = thefile->file.coord_vars[i]->var_name_quark;
							tmp->u.var->data_type= thefile->file.coord_vars[i]->data_type;
							tmp->u.var->type = COORD;
							tmp->u.var->n_dims = 1;
							tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec));
							tmp->u.var->dim_info->dim_quark = coordname;
							tmp->u.var->dim_info->dim_num = thefile->file.coord_vars[i]->file_dim_num[0];
							tmp->u.var->dim_info->dim_size = thefile->file.file_dim_info[thefile->file.coord_vars[i]->file_dim_num[0]]->dim_size;	
							tmp->u.var->coordnames[0] = -1;	
							for(j = 0; j < thefile->file.n_vars; j++) {
								if(thefile->file.var_info[j]->var_name_quark == coordname) {
									if(thefile->file.var_att_info[j] != NULL) {
										k = 0;
										step = thefile->file.var_att_info[j];
										while(step != NULL) {
											step = step->next;
											k++;
										}
										tmp->u.var->n_atts = k;
										tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*k);
										step = thefile->file.var_att_info[j];
										k = 0;
										while(step != NULL) {
											tmp->u.var->attnames[k]= step->the_att->att_name_quark;
											k++;
											step = step->next;
										}
									} else {
										tmp->u.var->n_atts = 0;
										tmp->u.var->attnames = NULL;
									}
									break;
								} else {
										tmp->u.var->n_atts = 0;
										tmp->u.var->attnames = NULL;
								}
							}
							tmp->next = NULL;
							return(tmp);
						}
					}
				}
				}
			}
		}
	}
	return(NULL);
}
NclQuark *_NclGetFileVarNames
#if	NhlNeedProto
(NclQuark file_sym_name,int *num_names)
#else
(file_sym_name,num_names)
NclQuark file_sym_name;
int *num_names;
#endif
{
	NclSymbol *s = NULL;
	int i;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid = NULL;
	NclQuark *names_out = NULL;
	*num_names = 0;

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		thevar = _NclRetrieveRec(s,DONT_CARE);
		if(thevar->kind == NclStk_VAR ){
			theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
			if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

				if(thefile != NULL)
				{
#ifdef USE_NETCDF4_FEATURES
				if(thefile->file.advanced_file_structure)
				{
					NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
					NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;
					if(NULL != grpnode->var_rec)
					{
						*num_names = grpnode->var_rec->n_vars;
						names_out = (NclQuark*)NclMalloc(grpnode->var_rec->n_vars * sizeof(NclQuark));
						for(i = 0; i < grpnode->var_rec->n_vars; ++i)
							names_out[i] = grpnode->var_rec->var_node[i].name;
					}
				}
				else
#endif
				{
					*num_names = thefile->file.n_vars;
					if(thefile->file.n_vars > 0) {
						names_out = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*thefile->file.n_vars);
						for(i = 0; i < thefile->file.n_vars; i++)
							names_out[i] = thefile->file.var_info[i]->var_name_quark;
					}
				}
				}
			}
		}
	}
	return (names_out);
}
NclQuark *_NclGetFileSymNames
#if	NhlNeedProto
(int    *num_names)
#else
(num_names)
int     *num_names;
#endif
{
	NclSymTableListNode *st;
	int i = 0;
	NclQuark *tmp_out;
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	int current_size = NCL_SYM_TAB_SIZE;

	*num_names = 0;
/*
* Start out with a guess
*/
	tmp_out = (NclQuark*)NclMalloc(sizeof(NclQuark)*current_size);
	st = thetablelist;
	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
                        if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
                                while(s != NULL) {
					if(s->type == VAR) {
						thevar = _NclRetrieveRec(s,DONT_CARE);
						if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
							if(*num_names < current_size) {
								tmp_out[*num_names] = thevar->u.data_var->var.var_quark;
							} else {
								tmp_out = NclRealloc(tmp_out,current_size * 2);
								current_size *= 2;
								tmp_out[*num_names] = thevar->u.data_var->var.var_quark;
							}
							(*num_names)++;
						}
					}
					s = s->symnext;
				}
			}
		}
                st = st->previous;
	}
	if(*num_names == 0) {
		NclFree(tmp_out);
		return(NULL);
	}
	return(tmp_out);
}
NhlClass *_NclGetHLUClassPtrs
#if	NhlNeedProto
(int    *num_names)
#else
(num_names)
int     *num_names;
#endif
{
	NclSymTableListNode *st;
	int i = 0;
	NhlClass  *tmp_out;
	NclSymbol *s;
	int current_size = NCL_SYM_TAB_SIZE;

	*num_names = 0;
/*
* Start out with a guess
*/
	tmp_out = (NhlClass*)NclMalloc(sizeof(NhlClass)*current_size);
	st = thetablelist;
	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
                        if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
                                while(s != NULL) {
					switch(s->type) {
					case OBJTYPE:
						if(*num_names < current_size) {
							tmp_out[*num_names] = s->u.obj_class_ptr;
						} else {
							tmp_out = NclRealloc(tmp_out,current_size * 2);
							current_size *= 2;
							tmp_out[*num_names] = s->u.obj_class_ptr;
						}
						(*num_names)++;
						break;
					default:

						break;
					}
					s = s->symnext;
				}
			}
		}
                st = st->previous;
	}
	if(*num_names == 0) {
		NclFree(tmp_out);
		return(NULL);
	}
	return(tmp_out);
}
NclQuark *_NclGetProcFuncSymNames
#if	NhlNeedProto
(int    *num_names)
#else
(num_names)
int     *num_names;
#endif
{
	NclSymTableListNode *st;
	int i = 0;
	NclQuark *tmp_out;
	NclSymbol *s;
	int current_size = NCL_SYM_TAB_SIZE;

	*num_names = 0;
/*
* Start out with a guess
*/
	tmp_out = (NclQuark*)NclMalloc(sizeof(NclQuark)*current_size);
	st = thetablelist;
	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
                        if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
                                while(s != NULL) {
					switch(s->type) {
					case NPROC: 
					case NFUNC:
					case IPROC: 
					case IFUNC: {
						if(*num_names < current_size) {
							tmp_out[*num_names] = NrmStringToQuark(s->name);
						} else {
							tmp_out = NclRealloc(tmp_out,sizeof(NclQuark) * current_size * 2);
							current_size *= 2;
							tmp_out[*num_names] = NrmStringToQuark(s->name);
						}
						(*num_names)++;
						break;
					}
					default:

						break;
					}
					s = s->symnext;
				}
			}
		}
                st = st->previous;
	}
	if(*num_names == 0) {
		NclFree(tmp_out);
		return(NULL);
	}
	return(tmp_out);
}

struct _NclExtValueRec *_NclReadFileVar
#if	NhlNeedProto
(NclQuark file_sym_name, NclQuark file_var_name , long *start, long *finish, long *stride)
#else
(file_sym_name, file_var_name, start, finish, stride)
NclQuark file_sym_name;
NclQuark file_var_name;
long    * start;
long    * finish;
long    * stride;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	NclMultiDValData theid;
	NclSelectionRecord *sel_ptr=NULL;
	int i,index = 0,k;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	
	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

				if(thefile != NULL)
				{
#ifdef USE_NETCDF4_FEATURES
					if(thefile->file.advanced_file_structure)
					{
						NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
						NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;
						NclFileVarNode *varnode = _getVarNodeFromNclFileGrpNode(grpnode, file_var_name);

						if(NULL != varnode->dim_rec)
						{
							for(k = 0; k < varnode->dim_rec->n_dims; ++k)
								dim_sizes[k] = varnode->dim_rec->dim_node[k].size;

							sel_ptr = BuildSel(varnode->dim_rec->n_dims,dim_sizes,start,finish,stride);
						}
					}
					else
#endif
					{
						index = _NclFileIsVar(thefile,file_var_name);
						for(k = 0; k < thefile->file.var_info[index]->num_dimensions; k++) {
							dim_sizes[k] = thefile->file.file_dim_info[thefile->file.var_info[index]->file_dim_num[k]]->dim_size;
						}
						sel_ptr = BuildSel(thefile->file.var_info[index]->num_dimensions,dim_sizes,start,finish,stride);
					}

					tmp_md = _NclFileReadVarValue(thefile,file_var_name,sel_ptr);
					if(sel_ptr != NULL) {
						NclFree(sel_ptr);
					}
					if(tmp_md != NULL) {
						if(tmp_md->obj.status == TEMPORARY) {
							_NclSetStatus((NclObj)tmp_md,STATIC);
						} else {
							tmp_md = _NclCopyVal(tmp_md,NULL);
							_NclSetStatus((NclObj)tmp_md,STATIC);
						}
						out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
						out_data->constant = 0;
						out_data->value = tmp_md->multidval.val;
						out_data->totalelements = tmp_md->multidval.totalelements;
						out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
						out_data->type = (int)tmp_md->multidval.data_type;
						out_data->n_dims = tmp_md->multidval.n_dims;
						for(i = 0; i < tmp_md->multidval.n_dims; i++) {
							out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
						}
						out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
						out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
						_NclDestroyObj((NclObj)tmp_md);
						return(out_data);
					}
				}
			}
		}	
	}
	return(NULL);
}

struct _NclExtValueRec *_NclReadFileVarCoord
#if     NhlNeedProto
(NclQuark file_sym_name, NclQuark file_var_name, NclQuark coordname, long * start, long* finish, long* stride)
#else
(file_sym_name, file_var_name, coordname,start, finish,stride )
NclQuark file_sym_name;
NclQuark file_var_name;
NclQuark coordname;
long * start;
long* finish;
long* stride;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclVar tmp_var = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	NclMultiDValData theid;
	NclSelectionRecord *sel_ptr=NULL;
	int i,index = 0,k;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

				if(thefile != NULL)
				{
#ifdef USE_NETCDF4_FEATURES
					if(thefile->file.advanced_file_structure)
					{
						NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
						NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;
						NclFileVarNode *varnode = _getVarNodeFromNclFileGrpNode(grpnode, coordname);

						if(NULL != varnode->dim_rec)
						{
							for(k = 0; k < varnode->dim_rec->n_dims; ++k)
								dim_sizes[k] = varnode->dim_rec->dim_node[k].size;

							sel_ptr = BuildSel(varnode->dim_rec->n_dims,dim_sizes,start,finish,stride);
						}
					}
					else
#endif
					{
						index = _NclFileVarIsCoord(thefile,coordname);
						for(k = 0; k < thefile->file.coord_vars[index]->num_dimensions; k++)
							dim_sizes[k] = thefile->file.file_dim_info[thefile->file.coord_vars[index]->file_dim_num[k]]->dim_size;
						sel_ptr = BuildSel(thefile->file.coord_vars[index]->num_dimensions,dim_sizes,start,finish,stride);
					}
					tmp_var = _NclFileReadCoord(thefile,coordname,sel_ptr);
					if(sel_ptr != NULL) {
						NclFree(sel_ptr);
					}
					if(tmp_var != NULL) {
						tmp_md = _NclVarValueRead(tmp_var,NULL,NULL);
					} else {
						return(NULL);
					}
					if(tmp_md != NULL) {
						if(tmp_md->obj.status == TEMPORARY) {
							_NclSetStatus((NclObj)tmp_md,STATIC);
						} else {
							tmp_md = _NclCopyVal(tmp_md,NULL);
							_NclSetStatus((NclObj)tmp_md,STATIC);
						}
						out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
						out_data->constant = 0;
						out_data->value = tmp_md->multidval.val;
						out_data->totalelements = tmp_md->multidval.totalelements;
						out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
						out_data->type = (int)tmp_md->multidval.data_type;
						out_data->n_dims = tmp_md->multidval.n_dims;
						for(i = 0; i < tmp_md->multidval.n_dims; i++) {
							out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
						}
						if(tmp_var->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)tmp_var);
						}
						out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
						out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
						_NclDestroyObj((NclObj)tmp_md);
						return(out_data);
					}
				}
			} 
		}
	}
	return(NULL);
}


struct _NclExtValueRec *_NclReadFileVarAtt
#if     NhlNeedProto
(NclQuark file_sym_name, NclQuark file_var_name, NclQuark attname)
#else
(file_sym_name, file_var_name, attname )
NclQuark file_sym_name;
NclQuark file_var_name;
NclQuark attname;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	NclMultiDValData theid;
	int i;

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
				if(thefile != NULL) {
					tmp_md = _NclFileReadVarAtt(thefile,file_var_name,attname,NULL);
					if(tmp_md != NULL) {
						if(tmp_md->obj.status == TEMPORARY) {
							_NclSetStatus((NclObj)tmp_md,STATIC);
						} else {
							tmp_md = _NclCopyVal(tmp_md,NULL);
							_NclSetStatus((NclObj)tmp_md,STATIC);
						}
						out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
						out_data->constant = 0;
						out_data->value = tmp_md->multidval.val;
						out_data->totalelements = tmp_md->multidval.totalelements;
						out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
						out_data->type = (int)tmp_md->multidval.data_type;
						out_data->n_dims= tmp_md->multidval.n_dims;
						for(i = 0; i < tmp_md->multidval.n_dims; i++) {
							out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
						}
						out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
						out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
						_NclDestroyObj((NclObj)tmp_md);
						return(out_data);
					}
				}
			} 
		}
	}
	return(NULL);
}

struct _NclExtValueRec *_NclReadFileAtt
#if	NhlNeedProto
(NclQuark file_sym_name, NclQuark attname)
#else
(file_sym_name, attname)
NclQuark file_sym_name;
NclQuark attname;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	NclMultiDValData theid;
	int i;

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
				if(thefile != NULL) {
					tmp_md = _NclFileReadAtt(thefile,attname,NULL);
					if(tmp_md != NULL) {
						if(tmp_md->obj.status == TEMPORARY) {
							_NclSetStatus((NclObj)tmp_md,STATIC);
						} else {
							tmp_md = _NclCopyVal(tmp_md,NULL);
							_NclSetStatus((NclObj)tmp_md,STATIC);
						}
						out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
						out_data->constant = 0;
						out_data->value = tmp_md->multidval.val;
						out_data->totalelements = tmp_md->multidval.totalelements;
						out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
						out_data->type = (int)tmp_md->multidval.data_type;
						out_data->n_dims= tmp_md->multidval.n_dims;
						for(i = 0; i < tmp_md->multidval.n_dims; i++) {
							out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
						}
						out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
						out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
						_NclDestroyObj((NclObj)tmp_md);
						return(out_data);
					}
				}
			} 
		}
	}
	return(NULL);
}

#ifdef USE_NETCDF4_FEATURES
static NclApiDataList *getAdvancedFileInfo(NclFile thefile)
{
    NclApiDataList     *tmp = NULL;
    NclAdvancedFile   theadvancedfile = (NclAdvancedFile) thefile;
    NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;
    int j;

    tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));

    tmp->next = NULL;
    tmp->kind = FILE_LIST;
    tmp->u.file = (NclApiFileInfoRec*)NclMalloc(sizeof(NclApiFileInfoRec));

    tmp->u.file->name = grpnode->name;
    tmp->u.file->path = grpnode->path;
    tmp->u.file->wr_status = grpnode->status;
    tmp->u.file->file_format = grpnode->file_format;

    tmp->u.file->n_dims = 0;
    tmp->u.file->dim_info = NULL;

    if(NULL != grpnode->dim_rec)
    {
        tmp->u.file->n_dims = grpnode->dim_rec->n_dims;
        tmp->u.file->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp->u.file->n_dims);
        for(j = 0; j < tmp->u.file->n_dims; ++j)
        {
            tmp->u.file->dim_info[j].dim_num   = j;
            tmp->u.file->dim_info[j].dim_quark = grpnode->dim_rec->dim_node[j].name;
            tmp->u.file->dim_info[j].dim_size  = grpnode->dim_rec->dim_node[j].size;
        }
    }

    tmp->u.file->n_vars = 0;
    tmp->u.file->var_names = NULL;

    if(NULL != grpnode->var_rec)
    {
        if(0 < grpnode->var_rec->n_vars)
        {
            tmp->u.file->n_vars = grpnode->var_rec->n_vars;
            tmp->u.file->var_names = (NclQuark*)NclMalloc(sizeof(NclQuark) * grpnode->var_rec->n_vars);
            for(j = 0; j < grpnode->var_rec->n_vars; ++j)
                tmp->u.file->var_names[j] = grpnode->var_rec->var_node[j].name;
        }
    }

    tmp->u.file->n_atts = 0;
    tmp->u.file->attnames = NULL;

    if(NULL != grpnode->att_rec)
    {
        if(0 < grpnode->att_rec->n_atts)
        {
            tmp->u.file->n_atts = grpnode->att_rec->n_atts;
            tmp->u.file->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark) * grpnode->att_rec->n_atts);
            for(j = 0; j < grpnode->att_rec->n_atts; ++j)
                tmp->u.file->attnames[j] = grpnode->att_rec->att_node[j].name;
        }
    }

    return(tmp);
}
#endif

NclApiDataList *_NclGetFileInfo
#if	NhlNeedProto
(NclQuark file_sym_name)
#else
(file_sym_name)
NclQuark file_sym_name;
#endif
{
	NclApiDataList *tmp = NULL;
	NclSymbol *s;
	int j;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid;

	s = _NclLookUp(NrmQuarkToString(file_sym_name));
	if((s != NULL)&&(s->type!= UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

				if(thefile != NULL)
				{
#ifdef USE_NETCDF4_FEATURES
				if(thefile->file.advanced_file_structure)
				{
					return (getAdvancedFileInfo(thefile));
				}
				else
#endif
				{
					tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
					tmp->kind = FILE_LIST;
					tmp->u.file = (NclApiFileInfoRec*)NclMalloc(sizeof(NclApiFileInfoRec));
					tmp->u.file->name = thevar->u.data_var->var.var_quark;
					tmp->u.file->path = thefile->file.fpath;
					tmp->u.file->wr_status = thefile->file.wr_status;
					tmp->u.file->file_format = (int) thefile->file.file_format;
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
					} else {
						tmp->u.file->n_vars = 0;
						tmp->u.file->var_names = NULL;
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
					tmp->next = NULL;
					return(tmp);
				}
				}
			}
		}
	}
	return(NULL);
}

NclApiDataList *_NclGetDefinedFileInfo
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclApiDataList *tmp = NULL;
	NclSymTableListNode *st;
	NclSymbol *s;
	int i,j;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid;

	st = thetablelist;
	while(st != NULL) {
                for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
                        if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
                                while(s != NULL) {
					if(s->type == VAR) {
						thevar = _NclRetrieveRec(s,DONT_CARE);
						if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
							theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
							thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);

							if(thefile != NULL)
							{
#ifdef USE_NETCDF4_FEATURES
							if(thefile->file.advanced_file_structure)
							{
								tmp = getAdvancedFileInfo(thefile);
							}
							else
#endif
							{
								tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
								tmp->next = NULL;
								tmp->kind = FILE_LIST;
								tmp->u.file = (NclApiFileInfoRec*)NclMalloc(sizeof(NclApiFileInfoRec));
								tmp->u.file->name = thevar->u.data_var->var.var_quark;
								tmp->u.file->path = thefile->file.fpath;
								tmp->u.file->wr_status = thefile->file.wr_status;
								tmp->u.file->file_format = (int)thefile->file.file_format;
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
								} else {
									tmp->u.file->n_vars = 0;
									tmp->u.file->var_names = NULL;
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
        return (tmp);
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
	int i,j,k;

	st = thetablelist;
	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
				while(s != NULL) {
					switch(s->type) {
					case NPROC: 
					case NFUNC:{
					
						tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
						tmp->kind = PROCFUNC_LIST;
						tmp->u.func = (NclApiFuncInfoRec*)NclMalloc(sizeof(NclApiFuncInfoRec));
						tmp->u.func->name = NrmStringToQuark(s->name);
						tmp->u.func->nparams = s->u.procfunc->nargs;
						if(s->type == NFUNC) {
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
					default:

						break;
					}
					if(tmp != NULL) {
						if(tmpargs != NULL) {
							tmp->u.func->theargs = (NclApiArgTemplate*)NclMalloc(sizeof(NclApiArgTemplate)*tmp->u.func->nparams);
							for(j = 0; j < tmp->u.func->nparams; j++) {
								tmp->u.func->theargs[j].n_dims = tmpargs[j].n_dims;
								for(k = 0; k < tmpargs[j].n_dims; k++) {
		
									tmp->u.func->theargs[j].dim_sizes[k]= tmpargs[j].dim_sizes[k];
								}
								tmp->u.func->theargs[j].arg_data_type = NrmNULLQUARK;
								tmp->u.func->theargs[j].arg_sym = NrmNULLQUARK;
								if (tmpargs[j].arg_data_type && tmpargs[j].arg_data_type->name)
									tmp->u.func->theargs[j].arg_data_type = NrmStringToQuark(tmpargs[j].arg_data_type->name);
								if (tmpargs[j].arg_sym && tmpargs[j].arg_sym->name)
									tmp->u.func->theargs[j].arg_sym= NrmStringToQuark(tmpargs[j].arg_sym->name);
								tmp->u.func->theargs[j].is_dimsizes= tmpargs[j].is_dimsizes;
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
	


	if((thesym != NULL)&&(thesym->type!= UNDEF)) {
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
					tmp->has_missing = the_hlu->multidval.missing_value.has_missing;
					tmp->missing = *(NclApiScalar*)&(the_hlu->multidval.missing_value.value);
					
					if(hlu != NULL) {
						return(tmp);
					} 
				} else {
					for(i = 0; i<the_hlu->multidval.n_dims; i++) {
						tmp->dim_sizes[i] = the_hlu->multidval.dim_sizes[i];
					}
					for(i = 0; i < the_hlu->multidval.totalelements; i++) {
						if((!the_hlu->multidval.missing_value.has_missing)||(((int*)the_hlu->multidval.val)[i] != the_hlu->multidval.missing_value.value.objval)) {
							hlu = (NclHLUObj)_NclGetObj(((int*)the_hlu->multidval.val)[i]);
							value[i] = hlu->hlu.hlu_id;
						} else {
							value[i] = the_hlu->multidval.missing_value.value.objval;
						}
					}
					tmp->value = (void*)value;
					tmp->has_missing = the_hlu->multidval.missing_value.has_missing;
					tmp->missing = *(NclApiScalar*)&(the_hlu->multidval.missing_value.value);
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
			if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
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
NclQuark *_NclGetHLUVarSymNames
#if	NhlNeedProto
(int *num_names)
#else
(num_names)
int *num_names;
#endif
{
	NclSymTableListNode *st;
	NclSymbol *s;
	NclStackEntry *the_var;
	int i;
	int current_size = NCL_SYM_TAB_SIZE;
	NclQuark *tmp_out;
	
	*num_names = 0;

	tmp_out = (NclQuark*)NclMalloc(sizeof(NclQuark)*current_size);

	st = thetablelist;

	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
				while(s != NULL) {
					if(s->type == VAR) {
						the_var = _NclRetrieveRec(s,DONT_CARE);	
						if((the_var->kind == NclStk_VAR)&&(the_var->u.data_var->obj.obj_type_mask & (Ncl_HLUVar))) {
							if(*num_names < current_size) {
								tmp_out[*num_names] = the_var->u.data_var->var.var_quark;
							} else {
								tmp_out = NclRealloc(tmp_out,current_size * 2);
								current_size *= 2;
								tmp_out[*num_names] = the_var->u.data_var->var.var_quark;
							}
							(*num_names)++;
						}
					}
					s = s->symnext;
				}
			}
		}
		st = st->previous;
	}
	if(*num_names == 0) {
		NclFree(tmp_out);
		return(NULL);
	}
	return(tmp_out);
	
}
NclQuark *_NclGetVarSymNames
#if	NhlNeedProto
(int *num_names)
#else
(num_names)
int *num_names;
#endif
{
	NclSymTableListNode *st;
	NclSymbol *s;
	NclStackEntry *the_var;
	int i;
	int current_size = NCL_SYM_TAB_SIZE;
	NclQuark *tmp_out;
	
	*num_names = 0;

	tmp_out = (NclQuark*)NclMalloc(sizeof(NclQuark)*current_size);

	st = thetablelist;

	while(st != NULL) {
		for(i = 0; i < NCL_SYM_TAB_SIZE; i++) {
			if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
				while(s != NULL) {
					if(s->type == VAR) {
						the_var = _NclRetrieveRec(s,DONT_CARE);	
						if((the_var->kind == NclStk_VAR)&&(!(the_var->u.data_var->obj.obj_type_mask & (Ncl_FileVar|Ncl_HLUVar)))) {
							if(*num_names < current_size) {
								tmp_out[*num_names] = the_var->u.data_var->var.var_quark;
							} else {
								tmp_out = NclRealloc(tmp_out,current_size * 2);
								current_size *= 2;
								tmp_out[*num_names] = the_var->u.data_var->var.var_quark;
							}
							(*num_names)++;
						}
					}
					s = s->symnext;
				}
			}
		}
		st = st->previous;
	}
	if(*num_names == 0) {
		NclFree(tmp_out);
		return(NULL);
	}
	return(tmp_out);
	
}

NclApiDataList *_NclGetVarCoordInfo
#if	NhlNeedProto
(NclQuark var_sym_name,NclQuark coordname)
#else
(var_sym_name,coordname)
NclQuark var_sym_name;
NclQuark coordname;
#endif
{
	NclApiDataList *tmp = NULL;
	NclAtt tmp_att = NULL;
	NclAttList *att_list = NULL;
	NclMultiDValData the_value = NULL;
	NclSymbol *s;
	int j;
	NclStackEntry *the_var;
	NclVar tmp_var;

	s = _NclLookUp(NrmQuarkToString(var_sym_name));
	if((s->type == VAR)&&(s->type!= UNDEF)) {
		the_var = _NclRetrieveRec(s,DONT_CARE);	
		if((the_var->kind == NclStk_VAR) &&(!(the_var->u.data_var->obj.obj_type_mask & (Ncl_FileVar|Ncl_HLUVar)))) {
			
			tmp_var = _NclReadCoordVar(the_var->u.data_var,NrmQuarkToString(coordname),NULL);	

			tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
			tmp->kind = VARIABLE_LIST;
			tmp->u.var = (NclApiVarInfoRec*) NclMalloc(sizeof(NclApiVarInfoRec));
			tmp->u.var->name = tmp_var->var.var_quark;
			the_value = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
			tmp->u.var->data_type= the_value->multidval.data_type;
			tmp->u.var->type = tmp_var->var.var_type;
			tmp->u.var->n_dims = tmp_var->var.n_dims;
			tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*tmp_var->var.n_dims);
			for(j = 0; j < tmp_var->var.n_dims; j++) {
				tmp->u.var->dim_info[j]= tmp_var->var.dim_info[j];
					if(tmp_var->var.coord_vars[j] != -1) {
						tmp->u.var->coordnames[j]= tmp_var->var.dim_info[j].dim_quark;
					} else {
						tmp->u.var->coordnames[j]= -1;
					}
			}
			if(tmp_var->var.att_id != -1) {
	
				tmp_att= (NclAtt)_NclGetObj(tmp_var->var.att_id);
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
			tmp->next = NULL;
			if(tmp_var->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)tmp_var);
			}
			return(tmp);
		}
	}
	return(NULL);
}

NclApiDataList *_NclGetVarInfo
#if	NhlNeedProto
(NclQuark var_sym_name)
#else
(var_sym_name)
NclQuark var_sym_name;
#endif
{
	NclApiDataList *tmp = NULL;
	NclAtt tmp_att = NULL;
	NclAttList *att_list = NULL;
	NclMultiDValData the_value = NULL;
	NclSymbol *s;
	int j;
	NclStackEntry *the_var;

	s = _NclLookUp(NrmQuarkToString(var_sym_name));
	if(s && (s->type == VAR)&&(s->type!= UNDEF)) {
		the_var = _NclRetrieveRec(s,DONT_CARE);	
		if((the_var->kind == NclStk_VAR) &&(!(the_var->u.data_var->obj.obj_type_mask & (Ncl_FileVar)))) {

			tmp = (NclApiDataList*)NclMalloc(sizeof(NclApiDataList));
			tmp->kind = VARIABLE_LIST;
			tmp->u.var = (NclApiVarInfoRec*) NclMalloc(sizeof(NclApiVarInfoRec));
			tmp->u.var->name = the_var->u.data_var->var.var_quark;
			the_value = (NclMultiDValData)_NclGetObj(the_var->u.data_var->var.thevalue_id);
			tmp->u.var->data_type= the_value->multidval.data_type;
			tmp->u.var->type = the_var->u.data_var->var.var_type;
			tmp->u.var->n_dims = the_var->u.data_var->var.n_dims;
			tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*the_var->u.data_var->var.n_dims);
			for(j = 0; j < the_var->u.data_var->var.n_dims; j++) {
				tmp->u.var->dim_info[j]= the_var->u.data_var->var.dim_info[j];
					if(the_var->u.data_var->var.coord_vars[j] != -1) {
						tmp->u.var->coordnames[j]= the_var->u.data_var->var.dim_info[j].dim_quark;
					} else {
						tmp->u.var->coordnames[j]= -1;
					}
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
			tmp->next = NULL;	
			return(tmp);
		}
	}
	return(NULL);
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
			if(st->sr->this_scope[i].nelem != 0) {
				s = st->sr->this_scope[i].thelist;
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
							tmp->u.var->data_type= the_value->multidval.data_type;
							tmp->u.var->type = the_var->u.data_var->var.var_type;
							tmp->u.var->n_dims = the_var->u.data_var->var.n_dims;
							tmp->u.var->dim_info = (NclDimRec*)NclMalloc(sizeof(NclDimRec)*the_var->u.data_var->var.n_dims);
							for(j = 0; j < the_var->u.data_var->var.n_dims; j++) {
								tmp->u.var->dim_info[j]= the_var->u.data_var->var.dim_info[j];
								if(the_var->u.data_var->var.coord_vars[j] != -1) {
									tmp->u.var->coordnames[j]= the_var->u.data_var->var.dim_info[j].dim_quark;
								} else {
									tmp->u.var->coordnames[j]= -1;
								}
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
		break;
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
extern struct _NclExtValueRec *_NclReadVarValue
#if	NhlNeedProto
(NclSymbol *the_sym, long* start, long*finish, long*stride)
#else
(the_sym, start,finish,stride)
NclSymbol *the_sym;
long* start;
long*finish;
long*stride;
#endif
{
	NclExtValueRec* tmp;
	NclStackEntry *the_var;
	NclMultiDValData the_val; 
	NclSelectionRecord *sel_ptr = NULL;
	NclHLUObj tmp_ho;
	int i;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];

	tmp = (NclExtValueRec*)NclMalloc((unsigned)sizeof(NclExtValueRec));
	the_var = _NclRetrieveRec(the_sym,DONT_CARE);
	if((the_var != NULL)&&(the_var->kind == NclStk_VAR)) {
		for(i = 0; i < the_var->u.data_var->var.n_dims; i++) {
			dim_sizes[i]  = the_var->u.data_var->var.dim_info[i].dim_size;
		}
		sel_ptr = BuildSel(the_var->u.data_var->var.n_dims,dim_sizes,start,finish,stride);
		the_val = _NclVarValueRead(the_var->u.data_var,sel_ptr,NULL);
		if(sel_ptr != NULL) {
			NclFree(sel_ptr);
		}
		tmp->constant = 0;
		tmp->totalelements = the_val->multidval.totalelements;
		tmp->type = (int)the_val->multidval.data_type;
		tmp->has_missing = the_val->multidval.missing_value.has_missing;
		tmp->missing = *(NclApiScalar*)&(the_val->multidval.missing_value.value);
		if((the_val->multidval.data_type != NCL_obj)&&!(the_val->obj.obj_type_mask & Ncl_MultiDValHLUObjData)){
			tmp->elem_size = the_val->multidval.totalsize/the_val->multidval.totalelements;
			if(the_val->obj.status == TEMPORARY) {
				_NclSetStatus((NclObj)the_val,STATIC);
			} else {
				the_val = _NclCopyVal(the_val,NULL);
				_NclSetStatus((NclObj)the_val,STATIC);
			}
			tmp->value = the_val->multidval.val;
			tmp->n_dims= the_val->multidval.n_dims;
			for(i = 0; i < the_val->multidval.n_dims; i++) {
				tmp->dim_sizes[i] = the_val->multidval.dim_sizes[i];
			}
			_NclDestroyObj((NclObj)the_val);
		} else {
			tmp->elem_size = sizeof(int);
			tmp->value = (void*)NclMalloc(sizeof(int)*the_val->multidval.totalelements);
			for(i = 0; i < the_val->multidval.totalelements; i++) {
				if((the_val->multidval.missing_value.has_missing)&&(((obj*)the_val->multidval.val)[i] == the_val->multidval.missing_value.value.objval)) {
					((int*)tmp->value)[i] = the_val->multidval.missing_value.value.objval;
				} else {
					tmp_ho = (NclHLUObj)_NclGetObj(((obj*)the_val->multidval.val)[i]);
					if(tmp_ho != NULL) {
						((int*)tmp->value)[i] = tmp_ho->hlu.hlu_id;
					} else if(the_val->multidval.missing_value.has_missing) {
						((int*)tmp->value)[i] = the_val->multidval.missing_value.value.objval;
					} else {
						((int*)tmp->value)[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
						tmp->has_missing = 1;
						tmp->missing.objval = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
					}
				}
				
			}
			if(the_val->obj.status == TEMPORARY) {
				_NclDestroyObj((NclObj)the_val);
			}
		}
		return(tmp);
	} 
	return(NULL);
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
	if((the_var != NULL)&&(the_var->kind == NclStk_VAR)) {
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
/* added by dib */
		tmp->n_dims = the_val->multidval.n_dims;
/* end added by dib */
		tmp->has_missing = the_val->multidval.missing_value.has_missing;
		tmp->missing = *(NclApiScalar*)&(the_val->multidval.missing_value.value);
		return(tmp);
	} 
	NclFree(tmp);
	return(NULL);
}



extern struct _NclExtValueRec *_NclReadVarAtt
#if	NhlNeedProto
(NclQuark var_sym_name, NclQuark attname)
#else
(the_sym, attname )
NclQuark var_sym_name;
NclQuark attname;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	NclHLUObj tmp_ho;
	int i;

	s = _NclLookUp(NrmQuarkToString(var_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if(thevar->kind == NclStk_VAR) {
				tmp_md= _NclReadAtt(thevar->u.data_var,NrmQuarkToString(attname),NULL);
				if(tmp_md != NULL) {
					out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
					out_data->constant = 0;
					out_data->totalelements = tmp_md->multidval.totalelements;
					out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
					out_data->type = (int)tmp_md->multidval.data_type;
					out_data->n_dims= tmp_md->multidval.n_dims;
					out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
					out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
					if((tmp_md->multidval.data_type != NCL_obj)&&!(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData)){
						out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
						if(tmp_md->obj.status == TEMPORARY) {
							_NclSetStatus((NclObj)tmp_md,STATIC);
						} else {
							tmp_md = _NclCopyVal(tmp_md,NULL);
							_NclSetStatus((NclObj)tmp_md,STATIC);
						}
						out_data->value = tmp_md->multidval.val;
						for(i = 0; i < tmp_md->multidval.n_dims; i++) {
							out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
						}
						_NclDestroyObj((NclObj)tmp_md);
					} else {
						 out_data->elem_size = sizeof(int);
						out_data->value = (void*)NclMalloc(sizeof(int)*tmp_md->multidval.totalelements);
						for(i = 0; i < tmp_md->multidval.totalelements; i++) {
							if((tmp_md->multidval.missing_value.has_missing)&&(((obj*)tmp_md->multidval.val)[i] == tmp_md->multidval.missing_value.value.objval)) {
								((int*)out_data->value)[i] = tmp_md->multidval.missing_value.value.objval;
							} else {
								tmp_ho = (NclHLUObj)_NclGetObj(((obj*)tmp_md->multidval.val)[i]);
								if(tmp_ho != NULL) {
									((int*)out_data->value)[i] = tmp_ho->hlu.hlu_id;
								} else if(tmp_md->multidval.missing_value.has_missing) {
									((int*)out_data->value)[i] = tmp_md->multidval.missing_value.value.objval;
								} else {
									((int*)out_data->value)[i] = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
									out_data->has_missing = 1;
									out_data->missing.objval = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
								}
							}
						}
						for(i = 0; i < tmp_md->multidval.n_dims; i++) {
							out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
						}
						if(tmp_md->obj.status == TEMPORARY) {
							_NclDestroyObj((NclObj)tmp_md);
						}
					}
					return(out_data);
				}
			}
		} 
	}
	return(NULL);
}

extern struct _NclExtValueRec *_NclReadVarCoord
#if	NhlNeedProto
(NclQuark var_sym_name, NclQuark coordname, long *start, long* finish, long* stride)
#else
(the_sym, coordname,start,finish,stride)
NclQuark var_sym_name;
NclQuark coordname;
long *start;
long* finish;
long* stride;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	NclSelectionRecord *sel_ptr=NULL;
	int i;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];

	s = _NclLookUp(NrmQuarkToString(var_sym_name));
	if((s != NULL)&&(s->type != UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if(thevar->kind == NclStk_VAR) {
				for(i = 0; i < thevar->u.data_var->var.n_dims;i++) {
				
					if(thevar->u.data_var->var.dim_info[i].dim_quark == coordname) {	
						dim_sizes[0] = thevar->u.data_var->var.dim_info[i].dim_size;
						break;
					}
				}
				sel_ptr = BuildSel(1,dim_sizes,start,finish,stride);
		
				tmp_md= _NclVarValueRead(_NclReadCoordVar(thevar->u.data_var,NrmQuarkToString(coordname),NULL),sel_ptr,NULL);
				if(sel_ptr != NULL) {
					NclFree(sel_ptr);
				}
				if(tmp_md != NULL) {
					if(tmp_md->obj.status == TEMPORARY) {
						_NclSetStatus((NclObj)tmp_md,STATIC);
					} else {
						tmp_md = _NclCopyVal(tmp_md,NULL);
						_NclSetStatus((NclObj)tmp_md,STATIC);
					}
					out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
					out_data->constant = 0;
					out_data->value = tmp_md->multidval.val;
					out_data->totalelements = tmp_md->multidval.totalelements;
					out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
					out_data->type = (int)tmp_md->multidval.data_type;
					out_data->n_dims= tmp_md->multidval.n_dims;
					for(i = 0; i < tmp_md->multidval.n_dims; i++) {
						out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
					}
					out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
					out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
					_NclDestroyObj((NclObj)tmp_md);
					return(out_data);
				}
			}
		} 
	}
	return(NULL);
}

extern struct _NclExtValueRec *_NclReadVarCoordAtt
#if	NhlNeedProto
(NclQuark var_sym_name, NclQuark coordname, NclQuark attname)
#else
(the_sym, coordname, attname)
NclQuark var_sym_name;
NclQuark coordname;
NclQuark attname;
#endif
{
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclMultiDValData tmp_md;
	NclExtValueRec *out_data = NULL;
	int i;

	s = _NclLookUp(NrmQuarkToString(var_sym_name));
	if((s != NULL)&&(s->type!= UNDEF)) {
		if(s->type == VAR) {
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if(thevar->kind == NclStk_VAR) {
				tmp_md= _NclReadAtt(_NclReadCoordVar(thevar->u.data_var,NrmQuarkToString(coordname),NULL),NrmQuarkToString(attname),NULL);
				if(tmp_md != NULL) {
					if(tmp_md->obj.status == TEMPORARY) {
						_NclSetStatus((NclObj)tmp_md,STATIC);
					} else {
						tmp_md = _NclCopyVal(tmp_md,NULL);
						_NclSetStatus((NclObj)tmp_md,STATIC);
					}
					out_data = (NclExtValueRec*)NclMalloc(sizeof(NclExtValueRec));
					out_data->constant = 0;
					out_data->value = tmp_md->multidval.val;
					out_data->totalelements = tmp_md->multidval.totalelements;
					out_data->elem_size = tmp_md->multidval.totalsize/tmp_md->multidval.totalelements;
					out_data->type = (int)tmp_md->multidval.data_type;
					out_data->n_dims= tmp_md->multidval.n_dims;
					for(i = 0; i < tmp_md->multidval.n_dims; i++) {
						out_data->dim_sizes[i] = tmp_md->multidval.dim_sizes[i];
					}
					out_data->has_missing = tmp_md->multidval.missing_value.has_missing;
					out_data->missing = *(NclApiScalar*)&(tmp_md->multidval.missing_value.value);
					_NclDestroyObj((NclObj)tmp_md);
					return(out_data);
				}
			}
		} 
	}
	return(NULL);
}

void _NclFileCleanUp (
#if NhlNeedProto
void
#endif
)

{
	NrmQuark *flist;
	int i,num_names;
	NclSymbol *s;
	NclStackEntry *thevar = NULL;
	NclFile thefile = NULL;
	NclMultiDValData theid;

	flist = _NclGetFileSymNames(&num_names);
	for (i = 0; i < num_names; i++) {
		s = _NclLookUp(NrmQuarkToString(flist[i]));
		if((s != NULL)&&(s->type != UNDEF)) {
			if(s->type == VAR) {
				thevar = _NclRetrieveRec(s,DONT_CARE);
				if((thevar->kind == NclStk_VAR)&&(thevar->u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
					theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
					thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
					if (thefile != NULL) {
					        _NclDestroyObj((NclObj)thefile);
 					}
				}
			}
		}
	}
        if (flist)
                NclFree(flist);
	return;
}

void _NclExit(int status) {
	_NclFileCleanUp();

        NhlClose();

	NCL_PROF_FINALIZE();

	_NclFinalizeSymbol();

	_NclFinalizeMachine();

#ifdef NCLDEBUG
	if(NCLdebug_on)
		_finalizeNclMemoryRecord();
#endif

	exit(status);
}

extern long _NclGetFileVarChunkInfo(NclFile thefile, NclQuark file_var_name, long *chunkdimsizes)
{
	int j;
	long ncds = 0;

	chunkdimsizes[0] = -4294967296;

	if(thefile != NULL)
	{
#ifdef USE_NETCDF4_FEATURES
		if(thefile->file.advanced_file_structure)
		{
			NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
			NclFileVarNode *varnode = _getVarNodeFromNclFileGrpNode(theadvancedfile->advancedfile.grpnode, file_var_name);

			if(NULL != varnode->chunk_dim_rec)
			{
				ncds = varnode->chunk_dim_rec->n_dims;
				for(j = 0 ; j < varnode->chunk_dim_rec->n_dims ; ++j)
				{
					chunkdimsizes[j] = varnode->chunk_dim_rec->dim_node[j].size;
				}
			}
		}
#endif
	}

	return ncds;
}


#ifdef __cplusplus
}
#endif

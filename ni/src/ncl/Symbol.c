/*
 *      $Id: Symbol.c,v 1.10 1994-07-08 21:31:54 ethan Exp $
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
#include <defs.h>
#include <data_objs/NclData.h>
#include <Keywords.h>
#include <Symbol.h>

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

int _NclInitSymbol
#if __STDC__ 
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
	_NclAddHLUObjs();
	_NclAddFileFormats();
/*
* After keywords are defined a new scope must be created. The Zero
* level scope is just for keywords and does not need any memory on the
* stack.
*/
	return(_NclNewScope());
}


void _NclRegisterFunc
#if  __STDC__
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
void _NclRegisterProc
#if  __STDC__
(NclBuiltInProcWrapper theprocptr,NclArgTemplate *args,char* fname,int nargs,int ftype)
#else 
(theprocptr,args, fname,nargs,ftype)
	NclIntrinsicProcWrapper theprocptr;
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
(void)
#else
()
#endif
{
	new_sym_i = 0;
}

void _NclDeleteNewSymStack
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if  __STDC__
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
#if __STDC__
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
#if __STDC__
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
#if __STDC__
(FILE *fp) 
#else
(fp)
	FILE *fp;
#endif
{
	NclSymTableListNode *st;
	NclSymbol *s;
	int i;

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
#if __STDC__
(char *name,struct _NhlLayerClassRec *the_ptr)
#else
(name,the_ptr)
	char *name;
	struct _NhlLayerClassRec *the_ptr;
#endif
{
	NclSymbol *s;

	s = _NclAddSym(name,OBJTYPE);
	s->u.obj_class_ptr = the_ptr;
	return;
}
#ifdef __cplusplus
}
#endif

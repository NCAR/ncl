/*
 *      $Id: Symbol.c,v 1.1 1998-03-27 23:37:34 ethan Exp $
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
#include <ncarg/ncl/defs.h>
#include "WSymbol.h"

/*
* This is used to avoid fragmentation in the event syntax errors are
* detected in the current statement.
*/
static NclSymbol *new_sym_stack[NCL_MAX_SYMS_PER_STMNT];
static int new_sym_i = 0;



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
	s = (NclSymbol*)malloc((unsigned)sizeof(NclSymbol));
	if(s == NULL) {
		fprintf(stderr,"NclAddSymInScope: Unable to create new symbol table entry");
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
	s->u.farg = NULL;
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
	free((void*)sym);
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


#ifdef __cplusplus
}
#endif

/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SrcTree.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jul 12 12:11:51 MDT 1993
 *
 *	Description:	Code for constructing individual source tree nodes
 */
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include "defs.h"
#include "Symbol.h"
#include "SrcTree.h"
#include "parser.h"
extern int cur_line_number;
extern char *cur_load_file;

NclGenericNode **node_list = NULL;
int node_list_index = 0;
int cur_node_list_size = NCL_SRC_TREE_NODE_LIST_SIZE;

/*
* These are the string name equivalents of the typedef found in
* SrcTree.h
*/

char *src_tree_names[] = {"Ncl_BLOCK",
			"Ncl_RETURN",
			"Ncl_IFTHEN",
			"Ncl_IFTHENELSE",
			"Ncl_VISBLKSET",
			"Ncl_VISBLKGET",
			"Ncl_VISBLKCREATE",
			"Ncl_DOFROMTO",
			"Ncl_DOFROMTOSTRIDE",
			"Ncl_INTRINSICPROCCALL",
			"Ncl_EXTERNALPROCCALL",
			"Ncl_PROCCALL",
			"Ncl_FUNCDEF",
			"Ncl_EXTERNFUNCDEF",
			"Ncl_LOCALVARDEC",
			"Ncl_DIMSIZELISTNODE",
			"Ncl_PROCDEF",
			"Ncl_EXTERNPROCDEF",
			"Ncl_ASSIGN",
			"Ncl_IDNREF",
			"Ncl_INTSUBSCRIPT",
			"Ncl_COORDSUBSCRIPT",
			"Ncl_SINGLEINDEX",
			"Ncl_RANGEINDEX",
			"Ncl_NEGEXPR",
			"Ncl_NOTEXPR",
			"Ncl_MODEXPR",
			"Ncl_OREXPR",
			"Ncl_ANDEXPR",
			"Ncl_XOREXPR",
			"Ncl_LTSELECTEXPR",
			"Ncl_GTSELECTEXPR",
			"Ncl_PLUSEXPR",
			"Ncl_MINUSEXPR",
			"Ncl_MULEXPR",
			"Ncl_MATMULEXPR",
			"Ncl_DIVEXPR",
			"Ncl_EXPEXPR",
			"Ncl_LEEXPR",
			"Ncl_GEEXPR",
			"Ncl_GTEXPR",
			"Ncl_LTEXPR",
			"Ncl_EQEXPR",
			"Ncl_NEEXPR",
			"Ncl_REAL",
			"Ncl_INT",
			"Ncl_STRING",
			"Ncl_INTRINSICFUNCCALL",
			"Ncl_EXTERNFUNCCALL",
			"Ncl_FUNCCALL",
			"Ncl_ARRAY",
			"Ncl_ROWLIST",
			"Ncl_ROWCOLUMNNODE",
			"Ncl_DOWHILE",
			"Ncl_VAR",
			"Ncl_VARDIM",
			"Ncl_VARATT",
			"Ncl_VARCOORD",
			"Ncl_FILEVAR",
			"Ncl_IDNEXPR",
			"Ncl_RESOURCE",
			"Ncl_GETRESOURCE",
			"Ncl_OBJ",
			"Ncl_BREAK",
			"Ncl_CONTINUE",
			"Ncl_FILEVARATT",
			"Ncl_FILEVARDIM",
			"Ncl_FILEVARCOORD",
			"Ncl_NEW",
			"Ncl_LOGICAL",
			"Ncl_VARCOORDATT",
			"Ncl_FILEVARCOORDATT",
			"Ncl_WILDCARDINDEX",
			"Ncl_NULLNODE",
			"Ncl_LIST",
			"Ncl_EXPRNEW",
			"Ncl_FILEVARLIST",
			"Ncl_GROUP",
			"Ncl_FILEGROUP",
			"Ncl_FILEGROUPATT",
			"Ncl_FILEGROUPDIM",
			"Ncl_FILEGROUPCOORD",
			"Ncl_FILEGROUPCOORDATT",
			"Ncl_FILEGROUPLIST",
			"Ncl_LISTVAR",
			"Ncl_REASSIGN",
			"Ncl_WILLNOTBEUSED"
			};
/*
* These are the string equivalents of the attribute tags assigned to 
* identifier references to determine the context of the reference
*/

char *ref_node_names[] = { "Ncl_READIT", "Ncl_WRITEIT", "Ncl_PARAMIT", "Ncl_VALONLY" };


/*
 * Function:	_NclRegisterNode
 *
 * Description:	 This function is called from all of the "_Make" functions.
 *		it inserts a pointer to the srctree node into a list. Each 
 * 		node contains a destroy_it field that is a pointer to a
 *		function that will free the tree node. All this is necessary
 *		because the parser can sometimes get into a state where 
 *		tree nodes are lost. This mechansim allows the entire source 
 *		tree to be freed regardless of the error status.
 *
 * In Args:	thenode Must be a source tree node defined in SrcTree.h
 *
 * Out Args:	NONE
 *
 * Returns:	NONE
 * Side Effect:	Possible memory allocation.
 */
void _NclRegisterNode
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode) 
	struct ncl_genericnode *thenode;
#endif
{
	if(node_list == NULL) {
		node_list = (NclGenericNode**)NclMalloc((unsigned)
				sizeof(NclGenericNode*) * cur_node_list_size);
		if(node_list == NULL) {		
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterNode: Error while trying to allocate memory for source tree, can't continue");
			return;
		}
	}
	if(node_list_index >= cur_node_list_size) {	
		cur_node_list_size *= 2;
		node_list = (NclGenericNode**)NclRealloc(node_list,(unsigned)
				sizeof(NclGenericNode*) * cur_node_list_size);
		if(node_list == NULL) {		
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclRegisterNode: Error while trying to allocate memory for source tree, can't continue");
			return;
		}
	}
	node_list[node_list_index] = thenode;
	node_list_index++;
	return;
	
}

/*
 * Function:	_NclResetNodeList
 *
 * Description:	Just sets the global varaible node_list_index back to 0
 *
 * In Args:	NONE
 *
 * Out Args:	NONE
 *
 * Scope:	
 * Returns:	NONE
 * Side Effect:	If called before freeing the tree memory leaks will occur!
 */
void _NclResetNodeList
#if	NhlNeedProto
(void)
#else 
()
#endif
{
	node_list_index = 0;
	return;
	
}



void _NclGenericDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFree((void*)thenode);
}


void *_NclMakeBreakCont
#if	NhlNeedProto
(NclSymbol *thesym)
#else
(thesym)
	NclSymbol *thesym;
#endif
{
	NclBreak *tmp = (NclBreak*)NclMalloc((unsigned)sizeof(NclBreak));

	if(thesym->type == BREAK) {
		tmp->kind = Ncl_BREAK;
		tmp->name = src_tree_names[Ncl_BREAK];
	} else {
		tmp->kind = Ncl_CONTINUE;
		tmp->name = src_tree_names[Ncl_CONTINUE];
	}
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	_NclRegisterNode((NclGenericNode*)tmp);
	return(tmp);
}


void *_NclMakeNULLNode
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclGenericNode *tmp = (NclGenericNode*)NclMalloc((unsigned)sizeof(NclGenericNode));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_NULLNODE;
	tmp->name = src_tree_names[Ncl_NULLNODE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
/*
 * Function:	_NclMakeReturn
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
void *_NclMakeReturn
#if	NhlNeedProto
(void * return_expr)
#else
(return_expr) 
	void * return_expr;
#endif
{
	NclReturn *tmp = (NclReturn*)NclMalloc((unsigned)sizeof(NclReturn));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_RETURN;
	tmp->name = src_tree_names[Ncl_RETURN];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->expr = return_expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void _NclIfThenDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclIfThen*tmp = (NclIfThen*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->block_stmnt_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}


/*
 * Function:	_NclMakeIfThen
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
void *_NclMakeIfThen
#if	NhlNeedProto
(void * conditional_expr, NclSrcListNode * block_stmnt_list)
#else
(conditional_expr, block_stmnt_list)
void * conditional_expr;
NclSrcListNode * block_stmnt_list;
#endif
{
	NclIfThen* tmp = (NclIfThen*)NclMalloc((unsigned)sizeof(NclIfThen));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_IFTHEN;
	tmp->name = src_tree_names[Ncl_IFTHEN];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclIfThenDestroy;
	tmp->cond_expr = conditional_expr;
	tmp->block_stmnt_list = block_stmnt_list;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclIfThenElseDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclIfThenElse*tmp = (NclIfThenElse*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->block_stmnt_list1;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        step = tmp->block_stmnt_list2;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeIfThenElse
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
void *_NclMakeIfThenElse
#if	NhlNeedProto
(void * conditional_expr, NclSrcListNode * block_stmnt_list1, NclSrcListNode * block_stmnt_list2)
#else
(conditional_expr, block_stmnt_list1, block_stmnt_list2)
void * conditional_expr;
NclSrcListNode * block_stmnt_list1;
NclSrcListNode * block_stmnt_list2;
#endif
{
	NclIfThenElse *tmp = (NclIfThenElse*)NclMalloc(
					(unsigned)sizeof(NclIfThenElse));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_IFTHENELSE;
	tmp->name = src_tree_names[Ncl_IFTHENELSE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclIfThenElseDestroy;
	tmp->cond_expr = conditional_expr;
	tmp->block_stmnt_list1 = block_stmnt_list1;
	tmp->block_stmnt_list2 = block_stmnt_list2;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


void _NclGResDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclGetResource *tmp = (NclGetResource*)thenode;

	NclFree(tmp);
}

/*
 * Function:	_NclMakeGetResource
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
void *_NclMakeGetResource
#if	NhlNeedProto
(void *resexpr, void *target_idn)
#else
(resexpr, target_idn)
	void *resexpr;
	void *target_idn;
#endif
{
	NclGetResource *tmp = (NclGetResource*)NclMalloc((unsigned)sizeof(NclGetResource));

	tmp->kind = Ncl_GETRESOURCE;
	tmp->name = src_tree_names[Ncl_GETRESOURCE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGResDestroy;
	tmp->resexpr = resexpr;
	tmp->target_idn = target_idn;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
	
}

void _NclResDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclResource *tmp = (NclResource*)thenode;

	NclFree(tmp);
}
/*
 * Function:	_NclMakeResource
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
void *_NclMakeResource
#if	NhlNeedProto
(void *resexpr, void *expr)
#else
(resexpr , expr)
	void *resexpr;
	void *expr;
#endif
{
	NclResource *tmp = (NclResource*)NclMalloc((unsigned)sizeof(NclResource));

	tmp->kind = Ncl_RESOURCE;
	tmp->name = src_tree_names[Ncl_RESOURCE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->resexpr = resexpr;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclResDestroy;
	tmp->expr = expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeObjRef
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
/*
extern void *_NclMakeObjRef
#if	NhlNeedProto
(NclSymbol* obj)
#else
(obj)
	NclSymbol *obj;
#endif
{
	NclObj *tmp = (NclObj*)NclMalloc((unsigned)sizeof(NclObj));
	
	tmp->kind = Ncl_OBJ;
	tmp->name = src_tree_names[Ncl_OBJ];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;

	tmp->obj = obj;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp); 
}
*/
void _NclVisblkDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclVisblk*tmp = (NclVisblk*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->resource_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeVis
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
void *_NclMakeVis
#if	NhlNeedProto
(void *obj_name_expr,NclSymbol *objtype,void *objparent,NclSrcListNode* resource_list,NclSrcTreeTypes nodetype)
#else
(obj_name_expr,objtype,objparent,resource_list,nodetype)
void * obj_name_expr;
NclSymbol* objtype;
void * objparent;
NclSrcListNode * resource_list;
NclSrcTreeTypes nodetype;
#endif
{
	NclVisblk *tmp = (NclVisblk*)NclMalloc((unsigned)sizeof(NclVisblk));

	tmp->kind = nodetype;
	tmp->name = src_tree_names[nodetype];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclVisblkDestroy;

	tmp->obj_name_expr = obj_name_expr;
	tmp->objparent = objparent;
	tmp->resource_list = resource_list;
	tmp->objtype = objtype;
	
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void _NclSGVisblkDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclSGVisblk*tmp = (NclSGVisblk*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->resource_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

void *_NclMakeSGVis
#if	NhlNeedProto
(void *objname,NclSrcListNode* resource_list,NclSrcTreeTypes nodetype)
#else
(objname,resource_list,nodetype)
void * objname;
NclSrcListNode * resource_list;
NclSrcTreeTypes nodetype;
#endif
{
	NclSGVisblk *tmp = (NclSGVisblk*)NclMalloc((unsigned)sizeof(NclSGVisblk));

	tmp->kind = nodetype;
	tmp->name = src_tree_names[nodetype];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclSGVisblkDestroy;

	tmp->objname = objname;
	tmp->resource_list = resource_list;
	
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
/*
 * Function:	_NclMakeNewListNode
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
NclSrcListNode *_NclMakeNewListNode
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclSrcListNode *tmp = (NclSrcListNode*)NclMalloc(
					(unsigned)sizeof(NclSrcListNode));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->node = NULL;
	tmp->next = NULL;
	return(tmp);

}

void _NclDoFromToDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclDoFromTo *tmp = (NclDoFromTo*)thenode;
	NclSrcListNode *step,*temp;
	step = tmp->block_stmnt_list;
	while(step != NULL) {
		temp = step;
		step = step->next;
		NclFree(temp);
	}
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeDoFromTo
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
void *_NclMakeDoFromTo
#if	NhlNeedProto
(void* var, void *start_expr, void *end_expr, NclSrcListNode *block_stmnt_list)
#else
(var,start_expr,end_expr,block_stmnt_list)
void *var;
void *	start_expr;
void *  end_expr;
NclSrcListNode * block_stmnt_list;
#endif
{
	NclDoFromTo *tmp = (NclDoFromTo*)NclMalloc((unsigned)sizeof(NclDoFromTo));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_DOFROMTO;
	tmp->name = src_tree_names[Ncl_DOFROMTO];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclDoFromToDestroy;
	tmp->inc_var = var;
	tmp->start_expr = start_expr;
	tmp->end_expr = end_expr;
	tmp->block_stmnt_list = block_stmnt_list;
	tmp->inc_sym = _NclAddUniqueSym("L_INC_",VAR);
	tmp->dir_sym = _NclAddUniqueSym("L_DIR_",VAR);
	tmp->end_sym = _NclAddUniqueSym("L_END_",VAR);
	_NclRegisterNode((NclGenericNode*)tmp);

	return((void*)tmp);
}

void _NclDoFromToStrideDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclDoFromToStride *tmp = (NclDoFromToStride*)thenode;
	NclSrcListNode *step,*temp;
	step = tmp->block_stmnt_list;
	while(step != NULL) {
		temp = step;
		step = step->next;
		NclFree(temp);
	}
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeDoFromToStride
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
void *_NclMakeDoFromToStride
#if	NhlNeedProto
(void *var, void *start_expr, void *end_expr, void *stride_expr, NclSrcListNode *block_stmnt_list)
#else
(var,start_expr,end_expr,stride_expr,block_stmnt_list)
void *var;
void *start_expr;
void *end_expr;
void *stride_expr;
NclSrcListNode *block_stmnt_list;
#endif
{
	NclDoFromToStride *tmp = (NclDoFromToStride*)NclMalloc(
					(unsigned)sizeof(NclDoFromToStride));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_DOFROMTOSTRIDE;
	tmp->name = src_tree_names[Ncl_DOFROMTOSTRIDE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclDoFromToStrideDestroy;
	tmp->inc_var = var;
	tmp->start_expr = start_expr;
	tmp->end_expr = end_expr;
	tmp->stride_expr = stride_expr;
	tmp->block_stmnt_list = block_stmnt_list;
	tmp->inc_sym = _NclAddUniqueSym("L_INC_",VAR);
	tmp->dir_sym = _NclAddUniqueSym("L_DIR_",VAR);
	tmp->end_sym = _NclAddUniqueSym("L_END_",VAR);
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclBuiltinProcCallDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclProcCall *tmp = (NclProcCall*)thenode;
	NclSrcListNode *step,*temp;

	step = tmp->arg_list;
	while(step != NULL) {
		temp = step;
		step = step->next;
		NclFree(temp);
	}
        NclFree((void*)tmp);
}


/*
 * Function:	_NclMakeBuiltinProcCall
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
void *_NclMakeProcCall
#if	NhlNeedProto
(NclSymbol * proc, NclSrcListNode * arg_list,NclSrcTreeTypes type)
#else
(proc, arg_list,type)
NclSymbol * proc;
NclSrcListNode * arg_list;
NclSrcTreeTypes type;
#endif
{
	NclProcCall *tmp = (NclProcCall*)NclMalloc(
					(unsigned)sizeof(NclProcCall));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = type;
	tmp->name = src_tree_names[type];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclBuiltinProcCallDestroy;
	tmp->proc = proc;
	tmp->arg_list = arg_list;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclAddProcFuncInfoToSym
#if	NhlNeedProto
(NclSymbol * pf_sym,NclSrcListNode *dec_list)
#else
(pf_sym,dec_list)
NclSymbol * pf_sym;
NclSrcListNode *dec_list;
#endif
{
	NclProcFuncInfo  *tmp1 = (NclProcFuncInfo*)NclMalloc((unsigned)
				sizeof(NclProcFuncInfo));
	NclSrcListNode *step, *step1;
	NclLocalVarDec *var_dec;
	int i,j;
	NclDimSizeListNode *dim_size;

	if(tmp1 == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return;
	}
	if(dec_list != NULL) {
		step = dec_list;
		i = 0;
		while(step != NULL) {
			i++;
			step = step->next;
		}
		step = dec_list;
		tmp1->nargs = i;
		tmp1->theargs = (NclArgTemplate*)
				NclMalloc((unsigned)sizeof(NclArgTemplate)*i);
		i = 0;
		while( step != NULL ) {
			var_dec = (NclLocalVarDec*)step->node;
			if(var_dec->dim_size_list != NULL) {
				tmp1->theargs[i].is_dimsizes = 1;
				step1 = var_dec->dim_size_list;
				j = 0;
				while(step1 != NULL) {
					dim_size = (NclDimSizeListNode*)step1->node;
					tmp1->theargs[i].dim_sizes[j] = dim_size->size;
					step1 = step1->next;
					j++;
				}
				tmp1->theargs[i].n_dims= j;
			} else {
				tmp1->theargs[i].is_dimsizes = 0;
				tmp1->theargs[i].n_dims = -1;
			}
			tmp1->theargs[i].arg_data_type = var_dec->data_type;
			tmp1->theargs[i].arg_sym = var_dec->var;
			
			step = step->next;		
			i++;
		}
		tmp1->thesym = pf_sym;
		tmp1->mach_rec_ptr= NULL;
	} else {
		tmp1->nargs = 0;
		tmp1->theargs = NULL;
		tmp1->thesym = pf_sym;
		tmp1->mach_rec_ptr = NULL;
	}
	tmp1->thescope = NULL;
	pf_sym->u.procfunc = tmp1;
}

void _NclNFunctionDefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else 
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFuncDef *tmp = (NclFuncDef*)thenode;
	NclSrcListNode *tmp_dec,*dec = tmp->dec_list;
	while(dec != NULL) {
		tmp_dec = dec->next;
		NclFree((void*)dec);
		dec = tmp_dec;
	}

	NclFree((void*)tmp);
	
}

/*
 * Function:	_NclMakeNFunctionDef
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
void *_NclMakeNFunctionDef
#if	NhlNeedProto
(NclSymbol *func, NclSrcListNode * dec_list,  void *block, NclScopeRec* thescope)
#else 
(func,dec_list,block,thescope)
NclSymbol *func;
NclSrcListNode * dec_list;
void* block;
NclScopeRec* thescope; 
#endif
{
	NclFuncDef *tmp = (NclFuncDef*)NclMalloc(
				(unsigned)sizeof(NclFuncDef));


	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FUNCDEF;
	tmp->name = src_tree_names[Ncl_FUNCDEF];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->func = func;
	tmp->dec_list = dec_list;
	tmp->block = block;
	tmp->scope = thescope;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclNFunctionDefDestroy;

	if(func->u.procfunc != NULL) {
		func->u.procfunc->thescope = thescope;
	} else {
/*
		_NclAddProcFuncInfoToSym(func,dec_list);
*/
		if(func->u.procfunc ==NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not create procedure or function parameter info");
			return(NULL);
		} else {
			func->u.procfunc->thescope = thescope;
		}
	}
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclEFunctionDefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else 
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclExternFuncDef *tmp = (NclExternFuncDef*)thenode;

	NclFree((void*)tmp);
	
}

/*
 * Function:	_NclMakeEFunctionDef
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
void* _NclMakeEFunctionDef
#if	NhlNeedProto
(NclSymbol *func, NclSrcListNode *dec_list, char *path_info_string,NclScopeRec* thescope)
#else
(func, dec_list, path_info_string,thescope)
NclSymbol *func;
NclSrcListNode *dec_list;
char *path_info_string;
NclScopeRec *thescope;
#endif
{
	NclExternFuncDef *tmp = (NclExternFuncDef*)NclMalloc(
					(unsigned)sizeof(NclExternFuncDef));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_EXTERNFUNCDEF;
	tmp->name = src_tree_names[Ncl_EXTERNFUNCDEF];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclEFunctionDefDestroy;
	tmp->func = func;
	tmp->dec_list = dec_list;
	tmp->path_info_string_q = NrmStringToQuark(path_info_string);
	tmp->scope = thescope;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


void _NclLocalVarDecDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclLocalVarDec*tmp = (NclLocalVarDec*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->dim_size_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeLocalVarDec
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
void* _NclMakeLocalVarDec
#if	NhlNeedProto
(NclSymbol* var, NclSrcListNode *dim_size_list, NclSymbol *param_type)
#else 
(var,dim_size_list,param_type)
NclSymbol* var;
NclSrcListNode *dim_size_list;
NclSymbol* param_type;
#endif
{
	NclLocalVarDec *tmp = (NclLocalVarDec*)NclMalloc((unsigned)
					sizeof(NclLocalVarDec));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_LOCALVARDEC;
	tmp->name = src_tree_names[Ncl_LOCALVARDEC];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclLocalVarDecDestroy;
	tmp->var = var;
	tmp->dim_size_list = dim_size_list;
	tmp->data_type = param_type;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeDimSizeNode
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
void * _NclMakeDimSizeNode
#if	NhlNeedProto
(long long size)
#else
(size)
	long long size;
#endif
{
	NclDimSizeListNode *tmp = (NclDimSizeListNode*)NclMalloc((unsigned)
					sizeof(NclDimSizeListNode));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_DIMSIZELISTNODE;
	tmp->name = src_tree_names[Ncl_DIMSIZELISTNODE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	if(size == -1) {
		tmp->any = 1;
	} else {
		tmp->any = 0;
	}
	tmp->size = (ng_size_t)size;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclProcDefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else 
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclProcDef *tmp = (NclProcDef*)thenode;
	NclSrcListNode *tmp_dec,*dec = tmp->dec_list;
	while(dec != NULL) {
		tmp_dec = dec->next;
		NclFree((void*)dec);
		dec = tmp_dec;
		
	}

	NclFree((void*)tmp);
	
}

/*
 * Function:	_NclMakeProcDef
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
void * _NclMakeProcDef
#if	NhlNeedProto
(NclSymbol *var, NclSrcListNode *arg_list, void* block,NclScopeRec *thescope)
#else 
(var, arg_list, block,thescope)
NclSymbol *var;
NclSrcListNode *arg_list;
void* block;
NclScopeRec *thescope;
#endif
{
	NclProcDef *tmp = (NclProcDef*)NclMalloc((unsigned)sizeof(NclProcDef));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_PROCDEF;
	tmp->name = src_tree_names[Ncl_PROCDEF];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclProcDefDestroy;
	tmp->proc = var;	
	tmp->dec_list = arg_list;
	tmp->block = block;
	tmp->scope = thescope;
	if(var->u.procfunc != NULL) {
		var->u.procfunc->thescope = thescope;
	} else {
/*
		_NclAddProcFuncInfoToSym(func,dec_list);
*/
		if(var->u.procfunc ==NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not create procedure or function parameter info");
			return(NULL);
		} else {
			var->u.procfunc->thescope = thescope;
		}
	}


	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclEProcDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclExternProcDef *tmp = (NclExternProcDef*)thenode;

	NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeExternalProcDef
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
void* _NclMakeExternalProcDef
#if	NhlNeedProto
(NclSymbol *var,NclSrcListNode *dec_list, char* path_info_string,NclScopeRec *thescope)
#else
(var,dec_list,path_info_string,thescope)
NclSymbol *var;
NclSrcListNode *dec_list;
char* path_info_string;
NclScopeRec *thescope;
#endif
{
	NclExternProcDef *tmp = (NclExternProcDef*)NclMalloc(
					(unsigned)sizeof(NclExternProcDef));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	
	tmp->kind = Ncl_EXTERNPROCDEF;
	tmp->name = src_tree_names[Ncl_EXTERNPROCDEF];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclEProcDestroy;
	tmp->proc = var;
	tmp->dec_list = dec_list;
	tmp->scope = thescope;
	tmp->path_info_string_q = NrmStringToQuark(path_info_string);
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeAssignment
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
void* _NclMakeAssignment
#if	NhlNeedProto
(void *name_ref, void *expr)
#else
(name_ref,expr)
void *name_ref;
void *expr;
#endif
{
	NclAssign *tmp = (NclAssign*)NclMalloc((unsigned)sizeof(NclAssign));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	
	tmp->kind = Ncl_ASSIGN;
	tmp->name = src_tree_names[Ncl_ASSIGN];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->left_side = name_ref;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->right_side = expr;

	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

/*
 * Function:	_NclMakeReassignment
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
void *_NclMakeReassignment(void *name_ref, void *expr)
{
	NclAssign *tmp = (NclAssign*)NclMalloc((unsigned)sizeof(NclAssign));

	if(NULL == tmp)
	{
		NHLPERROR((NhlFATAL,errno,"Not enough memory for source tree construction"));
		return(NULL);
	}

	tmp->kind = Ncl_REASSIGN;
	tmp->name = src_tree_names[Ncl_REASSIGN];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->new_left = 0;
	tmp->left_side = name_ref;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->right_side = expr;
	
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclIdnRefDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclIdnRef*tmp = (NclIdnRef*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeIdnRef
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
void* _NclMakeIdnRef
#if	NhlNeedProto
(void *name, NclSrcListNode *subscript_list)
#else
(name, subscript_list)
void *name;
NclSrcListNode *subscript_list;
#endif
{
	NclIdnRef *tmp = (NclIdnRef*)NclMalloc((unsigned)sizeof(NclIdnRef));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_IDNREF;
	tmp->name = src_tree_names[Ncl_IDNREF];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclIdnRefDestroy;
	tmp->thename = name;
	tmp->subscript_list = subscript_list;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeIntSubscript
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
void* _NclMakeIntSubscript
#if	NhlNeedProto
(void * subexpr, void  * dimname )
#else  
(subexpr,dimname)
void * subexpr; 
void  * dimname;
#endif
{
	NclSubscript *tmp = (NclSubscript*)NclMalloc(
					(unsigned)sizeof(NclSubscript));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_INTSUBSCRIPT;
	tmp->name = src_tree_names[Ncl_INTSUBSCRIPT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->subexpr = subexpr;
/*
	if(dimname != NULL) {
		tmp->dimname_q = NrmStringToQuark(dimname);
	} else {
		tmp->dimname_q = -1;
	}	
*/
	tmp->dimname_expr = dimname;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclCoordSubscriptDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclSubscript *tmp = (NclSubscript*)thenode;

	NclFree((void*)tmp);
}
/*
 * Function:	_NclMakeCoordSubscript
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
void* _NclMakeCoordSubscript
#if	NhlNeedProto
(void *subexpr, void *dimname)
#else
(subexpr, dimname)
void *subexpr;
void *dimname;
#endif
{
	NclSubscript *tmp = (NclSubscript*)NclMalloc(
					(unsigned)sizeof(NclSubscript));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_COORDSUBSCRIPT;
	tmp->name = src_tree_names[Ncl_COORDSUBSCRIPT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclCoordSubscriptDestroy;
	tmp->subexpr =subexpr;
/*
	if(dimname != NULL) {
                tmp->dimname_q = NrmStringToQuark(dimname);
        } else {
                tmp->dimname_q = -1;
        }       
*/
	tmp->dimname_expr = dimname;
	_NclRegisterNode((NclGenericNode*)tmp);
        return((void*)tmp);
}


/*
 * Function:	_NclMakeSingleIndex
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
void* _NclMakeSingleIndex
#if	NhlNeedProto
(void *expr)
#else
(expr)
	void *expr;
#endif
{
	NclSingleIndex *tmp = (NclSingleIndex*)NclMalloc((unsigned)
					sizeof(NclSingleIndex));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_SINGLEINDEX;
	tmp->name = src_tree_names[Ncl_SINGLEINDEX];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->expr = expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void *_NclMakeWildCardIndex 
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclWildCardIndex *tmp = (NclWildCardIndex*)NclMalloc((unsigned)
					sizeof(NclWildCardIndex));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_WILDCARDINDEX;
	tmp->name = src_tree_names[Ncl_WILDCARDINDEX];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
/*
 * Function:	_NclMakeRangeIndex
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
void* _NclMakeRangeIndex
#if	NhlNeedProto
(void * start_expr, void * end_expr, void* stride)
#else
(start_expr,end_expr,stride)
void * start_expr;
void * end_expr;
void * stride;
#endif
{
	NclRangeIndex *tmp = (NclRangeIndex*)NclMalloc((unsigned)
					sizeof(NclRangeIndex));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_RANGEINDEX;
	tmp->name = src_tree_names[Ncl_RANGEINDEX];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->start_expr = start_expr;
	tmp->end_expr = end_expr;
	tmp->stride = stride;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeUnaryExpr
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
void * _NclMakeUnaryExpr
#if	NhlNeedProto
(void * expr, NclSrcTreeTypes type)
#else
(expr,type)
void * expr;
NclSrcTreeTypes type;
#endif
{
	NclMonoExpr *tmp = (NclMonoExpr*)NclMalloc((unsigned)
					sizeof(NclMonoExpr));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = type;
	tmp->name = src_tree_names[type];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->expr = expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

/*
 * Function:	_NclMakeIdnExpr
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
void *_NclMakeIdnExpr
#if	NhlNeedProto
(void *idn_ref_node)
#else 
(idn_ref_node)
	void *idn_ref_node;
#endif
{
	NclIdnExpr *tmp = (NclIdnExpr*)NclMalloc((unsigned)sizeof(NclIdnExpr));
	tmp->kind = Ncl_IDNEXPR;
	tmp->name = src_tree_names[Ncl_IDNEXPR];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->idn_ref_node = idn_ref_node;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

/*
 * Function:	_NclMakeExpr
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
void * _NclMakeExpr
#if	NhlNeedProto
(void * left_expr, void * right_expr, NclSrcTreeTypes type)
#else
(left_expr,right_expr,type)
void * left_expr;
void * right_expr;
NclSrcTreeTypes type;
#endif
{
	NclDualExpr *tmp = (NclDualExpr*)NclMalloc((unsigned)
					sizeof(NclDualExpr));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = type;
	tmp->name = src_tree_names[type];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->left_expr = left_expr;
	tmp->right_expr = right_expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeRealExpr
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
void * _NclMakeRealExpr
#if	NhlNeedProto
(double real,char *string_rep)
#else
(real,string_rep)
double real;
char *string_rep;
#endif
{	
	NclReal *tmp = (NclReal*)NclMalloc((unsigned)sizeof(NclReal));
	char *ts;
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_REAL;	
	tmp->name = src_tree_names[Ncl_REAL];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->real = real;
	tmp->ref_type = Ncl_READIT;
	tmp->total_len = -1;
	tmp->len_after_dec = -1;
	tmp->is_double = 0;
	if(string_rep != NULL) {
		tmp->total_len = strlen(string_rep);
		ts = strchr(string_rep,'.');
		if(ts != NULL) {
			tmp->len_after_dec = strlen(ts);
		}
		ts = strchr(string_rep,'d');
		if (ts != NULL) {
			tmp->is_double = 1;
		}
	}
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


/*
 * Function:	_NclMakeIntExpr
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
void * _NclMakeIntExpr
#if	NhlNeedProto
(long long integer,char* string_rep)
#else
(integer,string_rep)
long long integer;
char* string_rep;
#endif
{
	NclInt *tmp = (NclInt*)NclMalloc((unsigned)sizeof(NclInt));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_INT;	
	tmp->name = src_tree_names[Ncl_INT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->integer= integer;
	tmp->ref_type = Ncl_READIT;
	tmp->len = -1;
	tmp->int_type = 'i';
	if (string_rep != NULL) { 
		char *type = strpbrk(string_rep,"bBChHiIlLqQ");
		if (type) {
			tmp->int_type = *type;
			tmp->len = strlen(string_rep) - 1;
		}
		tmp->len = strlen(string_rep);
	}
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void * _NclMakeLogicalExpr
#if	NhlNeedProto
(int integer,char* string_rep)
#else
(integer,string_rep)
int integer;
char* string_rep;
#endif
{
	NclInt *tmp = (NclInt*)NclMalloc((unsigned)sizeof(NclInt));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_LOGICAL;	
	tmp->name = src_tree_names[Ncl_LOGICAL];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->integer= integer;
	tmp->ref_type = Ncl_READIT;
	tmp->len = -1;
	if(string_rep != NULL) 
		tmp->len = strlen(string_rep);
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclStringExprDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclString *tmp = (NclString*)thenode;

	NclFree((void*)tmp);
}
/*
 * Function:	_NclMakeStringExpr
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
void * _NclMakeStringExpr
#if	NhlNeedProto
(char * str)
#else
(str)
char * str;
#endif
{
	NclString *tmp = (NclString*)NclMalloc((unsigned)sizeof(NclString));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_STRING;	
	tmp->name = src_tree_names[Ncl_STRING];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclStringExprDestroy;
	if(str != NULL) {
		tmp->string_q = NrmStringToQuark(str);
	}
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);

	return((void*)tmp);
}

void _NclFuncCallDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclFuncCall*tmp = (NclFuncCall*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->arg_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeFuncCall
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
void * _NclMakeFuncCall
#if	NhlNeedProto
(NclSymbol * fname, NclSrcListNode * argument_list,NclSrcTreeTypes type)
#else
(fname, argument_list,type)
NclSymbol * fname; 
NclSrcListNode * argument_list;
NclSrcTreeTypes type;
#endif
{
	NclFuncCall * tmp = (NclFuncCall*)NclMalloc((unsigned)sizeof(NclFuncCall));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = type;
	tmp->name = src_tree_names[type];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFuncCallDestroy;
	tmp->func = fname;
	tmp->arg_list = argument_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


void _NclArrayNodeDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{	
	NclArray *tmp = (NclArray*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->rcl->list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
	NclFree((void*)tmp->rcl);
        NclFree((void*)tmp);
}

void _NclListNodeDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{	
	NclArray *tmp = (NclArray*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->rcl->list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
	NclFree((void*)tmp->rcl);
        NclFree((void*)tmp);
}

/*
 * Function:	
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
void *_NclMakeArrayNode
#if	NhlNeedProto
(NclRclList* rc_list)
#else
(rc_list)
NclRclList* rc_list;
#endif
{
	NclArray *tmp = (NclArray*)NclMalloc((unsigned)sizeof(NclArray));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_ARRAY;
	tmp->name = src_tree_names[Ncl_ARRAY];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclArrayNodeDestroy;
	tmp->rcl = rc_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

/*
 * Function:	
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
void *_NclMakeListVarNode
#if	NhlNeedProto
(NclRclList* rc_list)
#else
(rc_list)
NclRclList* rc_list;
#endif
{
	NclListVar *tmp = (NclListVar*)NclMalloc((unsigned)sizeof(NclListVar));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_LISTVAR;
	tmp->name = src_tree_names[Ncl_LISTVAR];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclListNodeDestroy;
	tmp->rcl = rc_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);

	return((void*)tmp);
}



/*
 * Function:	_NclMakeRowList
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
NclRclList *_NclMakeRowList
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclRclList *tmp = (NclRclList*)NclMalloc((unsigned)sizeof(NclRclList));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	return(tmp);
}

void _NclDoWhileDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclDoWhile*tmp = (NclDoWhile*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->stmnts;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeWhile
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
void *_NclMakeWhile
#if	NhlNeedProto
(void * cond_expr, NclSrcListNode * statements)
#else
(cond_expr, statements)
void * cond_expr; 
NclSrcListNode * statements;
#endif
{
	NclDoWhile *tmp = (NclDoWhile*)NclMalloc((unsigned)sizeof(NclDoWhile));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_DOWHILE;
	tmp->name = src_tree_names[Ncl_DOWHILE];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclDoWhileDestroy;
	tmp->cond_expr = cond_expr;
	tmp->stmnts = statements;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclBlockDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclBlock*tmp = (NclBlock*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->stmnts;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

/*
 * Function:	_NclMakeBlock
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
void *_NclMakeBlock
#if	NhlNeedProto
(NclSrcListNode *statements)
#else
(statements)
NclSrcListNode *statements;
#endif
{
	NclBlock *tmp = (NclBlock*)NclMalloc((unsigned)sizeof(NclBlock));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_BLOCK;
	tmp->name = src_tree_names[Ncl_BLOCK];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclBlockDestroy;
	tmp->stmnts = statements;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void putspace
#if	NhlNeedProto
(int i,FILE *fp)
#else
(i,fp)
	int i;
	FILE *fp;
#endif
{	
	int j;

	for(j = 0;  j < 3*i; j++) {
		putc(' ',fp);
	}
}

void _NclPrintSymbol
#if	NhlNeedProto
(NclSymbol *sym,FILE *fp)
#else
(sym,fp)
	NclSymbol *sym;
	FILE *fp;
#endif
{
	if(fp == NULL) return;
	if(sym == NULL) return;
	switch(sym->type) {
	case INTEGER:
		fprintf(fp,"%s\t","INTEGER");
		break;
	case UINT:
		fprintf(fp,"%s\t","UINT");
		break;
	case FLOAT:
		fprintf(fp,"%s\t","FLOAT");
		break;
	case LONG:
		fprintf(fp,"%s\t","LONG");
		break;
	case ULONG:
		fprintf(fp,"%s\t","ULONG");
		break;
	case INT64:
		fprintf(fp,"%s\t","INT64");
		break;
	case UINT64:
		fprintf(fp,"%s\t","UINT64");
		break;
	case DOUBLE:
		fprintf(fp,"%s\t","DOUBLE");
		break;
	case BYTE:
		fprintf(fp,"%s\t","BYTE");
		break;
	case CHARACTER:
		fprintf(fp,"%s\t","CHARACTER");
		break;
	case GROUP:
		fprintf(fp,"%s\t","GROUP");
		break;
	case NUMERIC:
		fprintf(fp,"%s\t","NUMERIC");
		break;
	case ENUMERIC:
		fprintf(fp,"%s\t","ENUMERIC");
		break;
	case SNUMERIC:
		fprintf(fp,"%s\t","SNUMERIC");
		break;
	case FILETYPE:
		fprintf(fp,"%s\t","FILETYPE");
		break;
	case SHORT:
		fprintf(fp,"%s\t","SHORT");
		break;
	case USHORT:
		fprintf(fp,"%s\t","USHORT");
		break;
	case UNDEF:
		fprintf(fp,"%s\t","UNDEF");
		break;
	case VAR:
		fprintf(fp,"%s\t","VAR");
		break;
	case DO:
		fprintf(fp,"%s\t","DO");
		break;
	case QUIT:
		fprintf(fp,"%s\t","QUIT");
		break;
	case NPROC:
		fprintf(fp,"%s\t","NPROC");
		break;
	case BGIN:
		fprintf(fp,"%s\t","BGIN");
		break;
	case END:
		fprintf(fp,"%s\t","END");
		break;
	case DLIB:
		fprintf(fp,"%s\t","DLIB");
		break;
	case NFUNC:
		fprintf(fp,"%s\t","NFUNC");
		break;
	case FDIM:
		fprintf(fp,"%s\t","FDIM");
		break;
	case IF:
		fprintf(fp,"%s\t","IF");
		break;
	case THEN:
		fprintf(fp,"%s\t","THEN");
		break;
	case OBJVAR:
		fprintf(fp,"%s\t","OBJVAR");
		break;
	case OBJTYPE:
		fprintf(fp,"%s\t","OBJTYPE");
		break;
	case DFILE:
		fprintf(fp,"%s\t","DFILE");
		break;
	case KEYFUNC:
		fprintf(fp,"%s\t","KEYFUNC");
		break;
	case KEYPROC:
		fprintf(fp,"%s\t","KEYPROC");
		break;
	case VSBLKCREATE:
		fprintf(fp,"%s\t","VSBLKCREATE");
		break;
	case VSBLKSET:
		fprintf(fp,"%s\t","VSBLKSET");
		break;
	case VSBLKGET:
		fprintf(fp,"%s\t","VSBLKGET");
		break;
	case ELSE:
		fprintf(fp,"%s\t","ELSE");
		break;
	case NCLEXTERNAL:
		fprintf(fp,"%s\t","NCLEXTERNAL");
		break;
	case EXTERNAL:
		fprintf(fp,"%s\t","EXTERNAL");
		break;
	case RETURN:
		fprintf(fp,"%s\t","RETURN");
		break;
	case BREAK:
		fprintf(fp,"%s\t","BREAK");
		break;
	case CONTINUE:
		fprintf(fp,"%s\t","CONTINUE");
		break;
	case LIST:
		fprintf(fp,"%s\t","LIST");
		break;
	default:
		return;
		break;
	}
	fprintf(fp,"S: %s\tL: %d\tOf: %d\n",sym->name,sym->level,sym->offset);
}

void _NclPrintTree
#if	NhlNeedProto
(void *root,FILE *fp)
#else
(root,fp)
	void *root;
	FILE *fp;
#endif
{
	NclGenericNode *groot = (NclGenericNode*)root;
	NclSrcListNode *step;
	static int i = 0;;

	if(fp == NULL) return;
if(groot != NULL) {
/*
	if(groot->file != NULL) {
*/
		fprintf(fp,"%d) ",groot->line);		
/*
	}
*/
	switch(groot->kind) {
		case Ncl_BLOCK:
		{	
			NclBlock *block = (NclBlock*)root;

			putspace(i,fp);
			fprintf(fp,"%s\n",block->name);
			i++;
			step = block->stmnts;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
		break;
		case Ncl_NULLNODE:
		{
			NclGenericNode *ret = (NclGenericNode*)root;

			putspace(i,fp);	
			fprintf(fp,"%s\n",ret->name);
		}
			break;
		case Ncl_RETURN:
		{
			NclReturn *ret = (NclReturn*)root;

			putspace(i,fp);	
			fprintf(fp,"%s\n",ret->name);
			i++;
			_NclPrintTree(ret->expr,fp);
			i--;
		}
			break;
		case Ncl_IFTHEN:
		{
			NclIfThen *ifthen = (NclIfThen*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",ifthen->name);
			i++;
			_NclPrintTree(ifthen->cond_expr,fp);
			step = ifthen->block_stmnt_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
			
		}
			break;
		case Ncl_IFTHENELSE:
		{
			NclIfThenElse *ifthenelse = (NclIfThenElse*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",ifthenelse->name);
			i++;
			_NclPrintTree(ifthenelse->cond_expr,fp);
			step = ifthenelse->block_stmnt_list1;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			step = ifthenelse->block_stmnt_list2;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
/*
		case Ncl_OBJ:
		{
			NclObj *obj = (NclObj*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",obj->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(obj->obj,fp);
			i--;
		}
			break;
*/
		case Ncl_VISBLKCREATE:
		{
			NclVisblk *vblk = (NclVisblk*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",vblk->name);
			i++;
			putspace(i,fp);
			_NclPrintTree(vblk->obj_name_expr,fp);
			_NclPrintSymbol(vblk->objtype,fp);
			if(vblk->objparent != NULL) {
				_NclPrintTree(vblk->objparent,fp);
			}
			step = vblk->resource_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
		break;
		case Ncl_VISBLKSET:
		case Ncl_VISBLKGET:
		{
			NclSGVisblk *vblk = (NclSGVisblk*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",vblk->name);
			i++;
			_NclPrintTree(vblk->objname,fp);
			step = vblk->resource_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_RESOURCE:
		{
			NclResource *resource = (NclResource*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",resource->name);
			i++;
			putspace(i,fp);
			_NclPrintTree(resource->resexpr,fp);
			putspace(i,fp);
			_NclPrintTree(resource->expr,fp);
			i--;
		}
			break;
		case Ncl_GETRESOURCE:
		{
			NclGetResource *resource = (NclGetResource*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",resource->name);
			i++;
			putspace(i,fp);
			_NclPrintTree(resource->resexpr,fp);
			putspace(i,fp);
			_NclPrintTree(resource->target_idn,fp);
			i--;
		}
			break;
		case Ncl_DOFROMTO:
		{
			NclDoFromTo *dofromto = (NclDoFromTo*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",dofromto->name);
			i++;
			_NclPrintTree(dofromto->inc_var,fp);
			_NclPrintTree(dofromto->start_expr,fp);
			_NclPrintTree(dofromto->end_expr,fp);
			step = dofromto->block_stmnt_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step= step->next;
			}
			i--;	
		}
			break;
		case Ncl_DOFROMTOSTRIDE:
		{
			NclDoFromToStride *dofromtostride = 
						(NclDoFromToStride*) root;	

			putspace(i,fp);
			fprintf(fp,"%s\n",dofromtostride->name);
			i++;
			_NclPrintTree(dofromtostride->inc_var,fp);
			_NclPrintTree(dofromtostride->start_expr,fp);
			_NclPrintTree(dofromtostride->end_expr,fp);
			_NclPrintTree(dofromtostride->stride_expr,fp);
			step = dofromtostride->block_stmnt_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step= step->next;
			}
			i--;
		}
			break;
		case Ncl_INTRINSICPROCCALL:
		case Ncl_EXTERNALPROCCALL:
		case Ncl_PROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",proccall->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(proccall->proc,fp);
			step = proccall->arg_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_FUNCDEF:
		{
			NclFuncDef *funcdef = (NclFuncDef*)root;
	
			putspace(i,fp);
			fprintf(fp,"%s\n",funcdef->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(funcdef->func,fp);
			step = funcdef->dec_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}	
/*
			step = funcdef->local_dec_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}	
*/
			_NclPrintTree(funcdef->block,fp);
			i--;
		}	
			break;
		case Ncl_EXTERNFUNCDEF:
		{
			NclExternFuncDef *externfuncdef = (NclExternFuncDef*)
							 root;

			putspace(i,fp);
			fprintf(fp,"%s\n",externfuncdef->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(externfuncdef->func,fp);
			step = externfuncdef->dec_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			putspace(i,fp);
			fprintf(fp,"%s",NrmQuarkToString(externfuncdef->path_info_string_q));
			i--;
		}
			break;
		case Ncl_LOCALVARDEC:
		{
			NclLocalVarDec *localvardec = (NclLocalVarDec*)root;
	
			putspace(i,fp);
			fprintf(fp,"%s\n",localvardec->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(localvardec->var,fp);	
			step = localvardec->dim_size_list;
			while(step!= NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			if(localvardec->data_type != NULL) {
				putspace(i,fp);
				_NclPrintSymbol(localvardec->data_type,fp);
			}
			i--;
		}
			break;
		case Ncl_DIMSIZELISTNODE:
		{
			NclDimSizeListNode *dimsizelistnode = 
						(NclDimSizeListNode*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",dimsizelistnode->name);
			i++;
			if(dimsizelistnode->any) {
				fprintf(fp,"ANYSIZE\n");
			} else {
				putspace(i,fp);
				fprintf(fp,"%ld\n",(long)dimsizelistnode->size);
			}
			i--;
		}
			break;
		case Ncl_PROCDEF:
		{
			NclProcDef * procdef = (NclProcDef*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",procdef->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(procdef->proc,fp);
			step = procdef->dec_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
/*
			step = procdef->local_dec_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
*/
			_NclPrintTree(procdef->block,fp);
			i--;
		}
			break;
		case Ncl_EXTERNPROCDEF:
		{
			NclExternProcDef *externprocdef = (NclExternProcDef*)
						root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",externprocdef->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(externprocdef->proc,fp);
			step = externprocdef->dec_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			putspace(i,fp);
			fprintf(fp,"%s\n", NrmQuarkToString(externprocdef->path_info_string_q));
			i--;
		}
			break;
		case Ncl_ASSIGN:
		{
			NclAssign *assign = (NclAssign*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",assign->name);
			i++;
			_NclPrintTree(assign->left_side,fp);
			_NclPrintTree(assign->right_side,fp);
			i--;
		}
			break;
		case Ncl_IDNREF:
		{
			NclIdnRef *nameref = (NclIdnRef*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",nameref->name);
			i++;
			_NclPrintTree(nameref->thename,fp);
			step = nameref->subscript_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_INTSUBSCRIPT:	
		case Ncl_COORDSUBSCRIPT:	
		{
			NclSubscript *subscript = (NclSubscript*)
					root;

			putspace(i,fp);
			fprintf(fp,"%s\n",subscript->name);
			i++;
/*
			if(subscript->dimname_q != -1) {
				putspace(i,fp);
				fprintf(fp,"%s\n",NrmQuarkToString(subscript->dimname_q));
			}
*/
			if(subscript->dimname_expr != NULL) {
				_NclPrintTree(subscript->dimname_expr,fp);
			}
			if(subscript->subexpr != NULL) {
				_NclPrintTree(subscript->subexpr,fp);
			} else {
				putspace(i,fp);
				fprintf(fp,"ALL\n");
			}
			i--;
		}
			break;
		case Ncl_SINGLEINDEX:
		{
			NclSingleIndex *singleindex = (NclSingleIndex*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",singleindex->name);
			i++;
			_NclPrintTree(singleindex->expr,fp);
			i--;
		}
			break;
		case Ncl_RANGEINDEX:
		{
			NclRangeIndex *rangeindex = (NclRangeIndex*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",rangeindex->name);
			i++;
			if(rangeindex->start_expr != NULL) {
				_NclPrintTree(rangeindex->start_expr,fp);
			} else {
				putspace(i,fp);
				fprintf(fp,"START\n");
			}
			if(rangeindex->end_expr != NULL) {
				_NclPrintTree(rangeindex->end_expr,fp);
			} else {
				putspace(i,fp);
				fprintf(fp,"END\n");
			}
			if(rangeindex->stride!= NULL) {
				_NclPrintTree(rangeindex->stride ,fp);
			} else {
				putspace(i,fp);
				fprintf(fp,"DEFAULT STRIDE\n");
			}
			i--;
		}
			break;
		case Ncl_IDNEXPR:
		{
			NclIdnExpr *idnexpr = (NclIdnExpr*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",idnexpr->name);
			i++;
			_NclPrintTree(idnexpr->idn_ref_node,fp);
			i--;
		}
			break;
		case Ncl_NEGEXPR:
		case Ncl_NOTEXPR:
		{
			NclMonoExpr *monoexpr = (NclMonoExpr*) root;
			putspace(i,fp);
			fprintf(fp,"%s\n",monoexpr->name);
			i++;
			_NclPrintTree(monoexpr->expr,fp);
			i--;
		}
			break;
		case Ncl_MODEXPR:
		case Ncl_OREXPR:
		case Ncl_ANDEXPR:
		case Ncl_XOREXPR:
		case Ncl_LTSELECTEXPR:
		case Ncl_GTSELECTEXPR:
		case Ncl_PLUSEXPR:
		case Ncl_MINUSEXPR:
		case Ncl_MULEXPR:
		case Ncl_MATMULEXPR:
		case Ncl_DIVEXPR:
		case Ncl_EXPEXPR:
		case Ncl_LEEXPR:
		case Ncl_GEEXPR:
		case Ncl_GTEXPR:
		case Ncl_LTEXPR:
		case Ncl_EQEXPR:
		case Ncl_NEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",dualexpr->name);
			i++;
			_NclPrintTree(dualexpr->left_expr,fp);
			_NclPrintTree(dualexpr->right_expr,fp);
			i--;
		}
			break;
		case Ncl_REAL:
		{
			NclReal *real = (NclReal*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",real->name);
			putspace(i+1,fp);
			fprintf(fp,"%s\t",ref_node_names[real->ref_type]);
			fprintf(fp,"%g\n",real->real);
		}
			break;
		case Ncl_LOGICAL:
		case Ncl_INT:
		{
			NclInt *integer = (NclInt*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",integer->name);
			putspace(i+1,fp);
			fprintf(fp,"%s\t",ref_node_names[integer->ref_type]);
			fprintf(fp,"%lld\n",integer->integer);
		}
			break;
		case Ncl_STRING:
		{
			NclString *str= (NclString*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",str->name);
			putspace(i+1,fp);
			fprintf(fp,"%s\t",ref_node_names[str->ref_type]);
			fprintf(fp,"%s\n",NrmQuarkToString(str->string_q));
		}
			break;
		case Ncl_INTRINSICFUNCCALL:
		case Ncl_EXTERNFUNCCALL:
		case Ncl_FUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;
			
			putspace(i,fp);
			fprintf(fp,"%s\n",funccall->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[funccall->ref_type]);
			_NclPrintSymbol(funccall->func,fp);
			step = funccall->arg_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_ARRAY:
		{
			NclArray *array = (NclArray*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",array->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[array->ref_type]);
			fprintf(fp,"nelem:%d\n",array->rcl->nelem);
			step = array->rcl->list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_DOWHILE:
		{
			NclDoWhile *dowhilel = (NclDoWhile*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",dowhilel->name);
			i++;
			_NclPrintTree(dowhilel->cond_expr,fp);
			step = dowhilel->stmnts;
			while(step != NULL) {	
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_LIST:
		{
			NclList *list = (NclList*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",list->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[list->ref_type]);
			_NclPrintSymbol(list->sym,fp);
			
			i++;
			putspace(i,fp);
			_NclPrintTree(list->ref_node,fp);
			_NclPrintTree(list->subscript_list,fp);
/*
			step = list->subscript_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
*/
			i--;
			i--;
		}
			break;
		case Ncl_VAR:
		{
			NclVar *var = (NclVar*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",var->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[var->ref_type]);
			_NclPrintSymbol(var->sym,fp);
			step = var->subscript_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_FILEVARDIM:
		{
			NclFileVarDim *filevardim = (NclFileVarDim*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",filevardim->name);
			i++;
			putspace(i,fp);
/*
			fprintf(fp,"%s\n",NrmQuarkToString(filevardim->filevar_q));
*/
			_NclPrintTree(filevardim->filevarnode,fp);
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[filevardim->ref_type]);
			_NclPrintSymbol(filevardim->filesym,fp);
			_NclPrintTree(filevardim->dim_expr,fp);
			i--;
		}
			break;
		case Ncl_VARDIM:
		{
			NclVarDim *vardim = (NclVarDim*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",vardim->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[vardim->ref_type]);
			_NclPrintSymbol(vardim->sym,fp);
			_NclPrintTree(vardim->dim_expr,fp);
			i--;
		}
			break;
		case Ncl_FILEVARATT:
		{
			NclFileVarAtt *filevaratt = (NclFileVarAtt*)root;
			putspace(i,fp);
			i++;
			fprintf(fp,"%s\n",filevaratt->name);
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[filevaratt->ref_type]);
			_NclPrintSymbol(filevaratt->filesym,fp);
			putspace(i,fp);
			_NclPrintTree(filevaratt->filevarnode,fp);
			putspace(i,fp);
/*
			fprintf(fp,"attname: %s\n",NrmQuarkToString(filevaratt->attname_q));
*/
			_NclPrintTree(filevaratt->attnamenode,fp);
			step = filevaratt->subscript_list ;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_VARATT:
		{
			NclVarAtt *varatt = (NclVarAtt*)root;
			putspace(i,fp);
			i++;
			fprintf(fp,"%s\n",varatt->name);
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[varatt->ref_type]);
			_NclPrintSymbol(varatt->sym,fp);
			putspace(i,fp);
/*
			fprintf(fp,"attname: %s",NrmQuarkToString(varatt->attname_q));
*/
			_NclPrintTree(varatt->attnamenode,fp);
			step = varatt->subscript_list ;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_FILEVARCOORDATT:
		{
			NclFileCoordAtt *filecoordatt = (NclFileCoordAtt*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",filecoordatt->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[filecoordatt->ref_type]);
			_NclPrintSymbol(filecoordatt->filesym,fp);
			putspace(i,fp);
			_NclPrintTree(filecoordatt->filevarnode,fp);
			putspace(i,fp);
/*
			fprintf(fp,"coordname: %s\n",NrmQuarkToString(filecoordatt->coord_name_q));
*/
			_NclPrintTree(filecoordatt->coordnamenode,fp);
			putspace(i,fp);
/*
			fprintf(fp,"attname: %s\n",NrmQuarkToString(filecoordatt->attname_q));
*/
			_NclPrintTree(filecoordatt->attnamenode,fp);
			i--;
		}
			break;
		case Ncl_FILEVARCOORD:
		{
			NclFileCoord *filecoord = (NclFileCoord*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",filecoord->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[filecoord->ref_type]);
			_NclPrintSymbol(filecoord->filesym,fp);
			putspace(i,fp);
			_NclPrintTree(filecoord->filevarnode,fp);
			putspace(i,fp);
/*
			fprintf(fp,"coordname: %s\n",NrmQuarkToString(filecoord->coord_name_q));
*/
			_NclPrintTree(filecoord->coordnamenode,fp);
			step = filecoord->subscript_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_VARCOORDATT:
		{
			NclCoordAtt *coordatt = (NclCoordAtt*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",coordatt->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[coordatt->ref_type]);
			_NclPrintSymbol(coordatt->sym,fp);
			putspace(i,fp);
/*
			fprintf(fp,"coordname: %s\n",NrmQuarkToString(coordatt->coord_name_q));
*/
			_NclPrintTree(coordatt->coordnamenode,fp);
			putspace(i,fp);
/*
			fprintf(fp,"attname: %s\n",NrmQuarkToString(coordatt->attname_q));
*/
			_NclPrintTree(coordatt->attnamenode,fp);
			i--;
		}
			break;
		case Ncl_VARCOORD:
		{
			NclCoord *coord = (NclCoord*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",coord->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[coord->ref_type]);
			_NclPrintSymbol(coord->sym,fp);
			putspace(i,fp);
/*
			fprintf(fp,"coordname: %s\n",NrmQuarkToString(coord->coord_name_q));
*/
			_NclPrintTree(coord->coordnamenode,fp);

			step = coord->subscript_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_FILEVAR:
		{
			NclFileVar *filevar = (NclFileVar*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",filevar->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[filevar->ref_type]);
			_NclPrintSymbol(filevar->dfile,fp);
			putspace(i,fp);
			_NclPrintTree(filevar->filevarnode,fp);
			step = filevar->subscript_list;
			while(step != NULL) {
				_NclPrintTree(step->node,fp);
				step = step->next;
			}
			i--;
		}
			break;
		case Ncl_BREAK:
		case Ncl_CONTINUE:
		{
			putspace(i,fp);
			fprintf(fp,"%s\n",groot->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%d\n",groot->line);
			i--;
		}
		break;
		case Ncl_NEW:
		{
			NclNew *new = (NclNew*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",new->name);
			i++;
			putspace(i,fp);
			_NclPrintSymbol(new->data_sym,fp);	
			_NclPrintTree(new->size_expr,fp);
			if(new->missing_expr != NULL) {
				_NclPrintTree(new->missing_expr,fp);
			}
			i--;	
		}
		break;
		case Ncl_REASSIGN:
		{
                        fprintf(stderr, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
                        fprintf(stderr, "\tNcl_REASSIGN, groot->kind: %d\n", groot->kind);

			NclAssign *assign = (NclAssign*)root;
			putspace(i,fp);
			fprintf(fp,"%s\n",assign->name);
			i++;
			_NclPrintTree(assign->left_side,fp);
			_NclPrintTree(assign->right_side,fp);
			i--;
		}
		break;
		case Ncl_FILEGROUP:
		{
			NclFileGroup *filegroup = (NclFileGroup*)root;

			putspace(i,fp);
			fprintf(fp,"%s\n",filegroup->name);
			i++;
			putspace(i,fp);
			fprintf(fp,"%s\t",ref_node_names[filegroup->ref_type]);
			_NclPrintSymbol(filegroup->dfile,fp);
			putspace(i,fp);
			_NclPrintTree(filegroup->filegroupnode,fp);
			i--;
		}
		break;
		
		default:
			fprintf(stdout, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
			fprintf(stdout, "\tUNRECOGNIZED ENUM VALUE, groot->kind: %d\n", groot->kind);
			fprintf(fp,"UNRECOGNIZED ENUM VALUE!\n");
			break;
	}
	return;
} else {
	fprintf(fp,"ERROR NULL NODE FOUND!\n");
}
}


void _NclFileVarDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFileVar* tmp = (NclFileVar*)thenode;
	 NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void _NclFileGroupDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFileGroup* tmp = (NclFileGroup*)thenode;

	NclFree((void*)tmp);
}
void *_NclMakeFileVarRef
#if	NhlNeedProto
(NclSymbol *dfile,void * filevar, NclSrcListNode * subscript_list, int type )
#else
(dfile ,filevar, subscript_list ,type)
NclSymbol * dfile;
void* filevar;
NclSrcListNode * subscript_list;
int type;
#endif
{
        NclFileVar *tmp = (NclFileVar*)NclMalloc((unsigned)sizeof(NclFileVar));
        if(tmp == NULL) {
                NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
                return(NULL);
        }
        tmp->kind = type;
        tmp->name = src_tree_names[type];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileVarDestroy;
	tmp->dfile = dfile;
        tmp->filevarnode = filevar;
	tmp->subscript_list = subscript_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
        return((void*)tmp);
}
void *_NclMakeFileGroupRef
#if	NhlNeedProto
(NclSymbol *dfile, void * filegroup, int type )
#else
(dfile, filegroup, type)
NclSymbol * dfile;
void* filegroup;
int type;
#endif
{
        NclFileGroup *tmp = (NclFileGroup*)NclMalloc((unsigned)sizeof(NclFileGroup));
        if(tmp == NULL) {
                NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
                return(NULL);
        }
        tmp->kind = type;
        tmp->name = src_tree_names[type];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileGroupDestroy;
	tmp->dfile = dfile;
        tmp->filegroupnode = filegroup;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
        return((void*)tmp);
}

void _NclVarDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclVar*tmp = (NclVar*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}
/*
void _NclListDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclVar*tmp = (NclVar*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}
*/

void *_NclMakeListRef
#if	NhlNeedProto
(void *ref_node,NclSymbol *equiv_sym,NclSymbol *list,void *subscript_list,NclSymbol *tmp_var)
#else
(ref_node,equiv_sym,list,subscript_list,tmp_var)
void *ref_node;
NclSymbol *equiv_sym;
NclSymbol *list;
void *subscript_list;
NclSymbol *tmp_var;
#endif
{
	NclList * tmp = (NclList*)NclMalloc((unsigned)sizeof(NclList));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_LIST;
	tmp->name = src_tree_names[Ncl_LIST];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
/*
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclListDestroy;
*/
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->sym = list;
	tmp->tmp= equiv_sym;
	tmp->tmp_var= tmp_var;
	tmp->agg_subscript = NULL;
	tmp->ref_node= ref_node;
	tmp->subscript_list = subscript_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclFileVarListDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclFileVarList*tmp = (NclFileVarList*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->filevar_subscript;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

void _NclFileGroupListDestroy
#if     NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
        struct ncl_genericnode *thenode;
#endif
{
        NclFileGroupList*tmp = (NclFileGroupList*)thenode;
        NclSrcListNode *step,*temp;
        step = tmp->filegroup_subscript;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }
        NclFree((void*)tmp);
}

void *_NclMakeFileVarListRef
#if	NhlNeedProto
(NclSymbol *list, void *list_subscript, void * filevar, NclSrcListNode *filevar_subscript)
#else
(list, list_subscript, filevar, filevar_subscript)
NclSymbol *list;
void *list_subscript;
void *filevar;
NclSrcListNode *filevar_subscript;
#endif
{
	NclFileVarList * tmp = (NclFileVarList*)NclMalloc((unsigned)sizeof(NclFileVarList));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FILEVARLIST;
	tmp->name = src_tree_names[Ncl_FILEVARLIST];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileVarListDestroy;
	tmp->list = list;
	tmp->list_subscript = list_subscript;
	tmp->filevar = filevar;
	tmp->filevar_subscript = filevar_subscript;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void *_NclMakeFileGroupListRef
#if	NhlNeedProto
(NclSymbol *list, void *list_subscript, void * filegroup, NclSrcListNode *filegroup_subscript)
#else
(list, list_subscript, filegroup, filegroup_subscript)
NclSymbol *list;
void *list_subscript;
void *filegroup;
NclSrcListNode *filegroup_subscript;
#endif
{
	NclFileGroupList * tmp = (NclFileGroupList*)NclMalloc((unsigned)sizeof(NclFileGroupList));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FILEGROUPLIST;
	tmp->name = src_tree_names[Ncl_FILEVARLIST];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileGroupListDestroy;
	tmp->list = list;
	tmp->list_subscript = list_subscript;
	tmp->filegroup = filegroup;
	tmp->filegroup_subscript = filegroup_subscript;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}


void *_NclMakeVarRef
#if	NhlNeedProto
(NclSymbol *var,NclSrcListNode *subscript_list)
#else
(var,subscript_list)
NclSymbol *var;
NclSrcListNode *subscript_list;
#endif
{
	NclVar * tmp = (NclVar*)NclMalloc((unsigned)sizeof(NclVar));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_VAR;
	tmp->name = src_tree_names[Ncl_VAR];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclVarDestroy;
	tmp->sym = var;
	tmp->subscript_list = subscript_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void *_NclMakeVarDimRef
#if	NhlNeedProto
(NclSymbol *var,void*  dim_expr)
#else
(var,dim_expr)
NclSymbol *var;
void *dim_expr;
#endif
{
	NclVarDim *tmp = (NclVarDim*)NclMalloc((unsigned)sizeof(NclVarDim));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_VARDIM;
	tmp->name = src_tree_names[Ncl_VARDIM];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->sym = var;
	tmp->dim_expr = dim_expr;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void _NclFileVarDimNumRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFileVarDim *tmp = (NclFileVarDim*)thenode;
	
	NclFree((void*)tmp);
}
void *_NclMakeFileVarDimRef
#if	NhlNeedProto
(NclSymbol *var,void *filevar,void *dim_expr)
#else
(var,filevar,dim_expr)
NclSymbol *var;
void *filevar;
void *dim_expr;
#endif
{
	NclFileVarDim *tmp = (NclFileVarDim*)NclMalloc((unsigned)sizeof(NclFileVarDim));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FILEVARDIM;
	tmp->name = src_tree_names[Ncl_FILEVARDIM];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileVarDimNumRefDestroy;
	tmp->filesym = var;
	tmp->dim_expr = dim_expr;
	tmp->filevarnode = filevar;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclFileVarAttRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else 
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFileVarAtt *tmp =(NclFileVarAtt*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void *_NclMakeFileVarAttRef
#if	NhlNeedProto
(NclSymbol *file,void* filevar, void *attname,NclSrcListNode *subscript_list)
#else
(file,filevar,attname,subscript_list)
NclSymbol *file;
void *filevar;
char *attname;
NclSrcListNode *subscript_list;
#endif
{
	NclFileVarAtt *tmp =(NclFileVarAtt*)NclMalloc((unsigned)sizeof(NclFileVarAtt));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FILEVARATT;
	tmp->name = src_tree_names[Ncl_FILEVARATT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileVarAttRefDestroy;
	tmp->filesym = file;
	tmp->attnamenode = attname;
	tmp->filevarnode = filevar;
	tmp->subscript_list = subscript_list;

	tmp->ref_type = Ncl_READIT;	
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void _NclVarAttRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else 
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclVarAtt *tmp =(NclVarAtt*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void *_NclMakeVarAttRef
#if	NhlNeedProto
(NclSymbol *var,void *attname,NclSrcListNode *subscript_list)
#else
(var,attname,subscript_list)
NclSymbol *var;
char *attname;
NclSrcListNode *subscript_list;
#endif
{
	NclVarAtt *tmp =(NclVarAtt*)NclMalloc((unsigned)sizeof(NclVarAtt));
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_VARATT;
	tmp->name = src_tree_names[Ncl_VARATT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclVarAttRefDestroy;
	tmp->sym = var;
	tmp->attnamenode = attname;
	tmp->subscript_list = subscript_list;

	tmp->ref_type = Ncl_READIT;	
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void _NclFileVarCoordRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFileCoord *tmp= (NclFileCoord*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void *_NclMakeFileVarCoordRef
#if	NhlNeedProto
(NclSymbol *var,void* filevar, void *coord,NclSrcListNode *subscript_list)
#else
(var,coord,filevar,subscript_list)
NclSymbol *var;
char *filevar;
char *coord;
NclSrcListNode *subscript_list;
#endif
{
	NclFileCoord *tmp= (NclFileCoord*)NclMalloc((unsigned)sizeof(NclFileCoord));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FILEVARCOORD;
	tmp->name = src_tree_names[Ncl_FILEVARCOORD];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileVarCoordRefDestroy;
	tmp->filesym = var;
	tmp->filevarnode = filevar;
	tmp->coordnamenode = coord;
	tmp->subscript_list = subscript_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void _NclFileVarCoordAttRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclFileCoordAtt *tmp= (NclFileCoordAtt*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void *_NclMakeFileVarCoordAttRef
#if	NhlNeedProto
(NclSymbol *var,void* filevar, void *coord,void *attname,NclSrcListNode *subscript_list)
#else
(var,coord,filevar,attname,subscript_list)
NclSymbol *var;
char *filevar;
char *coord;
char *attname;
NclSrcListNode *subscript_list
#endif
{
	NclFileCoordAtt *tmp= (NclFileCoordAtt*)NclMalloc((unsigned)sizeof(NclFileCoordAtt));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_FILEVARCOORDATT;
	tmp->name = src_tree_names[Ncl_FILEVARCOORDATT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclFileVarCoordAttRefDestroy;
	tmp->filesym = var;
	tmp->filevarnode = filevar;
	tmp->coordnamenode = coord;
	tmp->attnamenode = attname;
	tmp->subscript_list = subscript_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclVarCoordRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclCoord *tmp= (NclCoord*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void *_NclMakeVarCoordRef
#if	NhlNeedProto
(NclSymbol *var,void *coord,NclSrcListNode *subscript_list)
#else
(var,coord,subscript_list)
NclSymbol *var;
char *coord;
NclSrcListNode *subscript_list;
#endif
{
	NclCoord *tmp= (NclCoord*)NclMalloc((unsigned)sizeof(NclCoord));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_VARCOORD;
	tmp->name = src_tree_names[Ncl_VARCOORD];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclVarCoordRefDestroy;
	tmp->sym = var;
	tmp->coordnamenode = coord;
	tmp->subscript_list= subscript_list;
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void _NclVarCoordAttRefDestroy
#if	NhlNeedProto
(struct ncl_genericnode *thenode)
#else
(thenode)
	struct ncl_genericnode *thenode;
#endif
{
	NclCoordAtt *tmp= (NclCoordAtt*)thenode;
	NclSrcListNode *step,*temp;
        step = tmp->subscript_list;
        while(step != NULL) {
                temp = step;
                step = step->next;
                NclFree(temp);
        }

	NclFree((void*)tmp);
}
void *_NclMakeVarCoordAttRef
#if	NhlNeedProto
(NclSymbol *var,void *coord,void *attname,NclSrcListNode *subscript_list)
#else
(var,coord,attname,subscript_list)
NclSymbol *var;
char *coord;
char *attname;
NclSrcListNode *subscript_list;
#endif
{
	NclCoordAtt *tmp= (NclCoordAtt*)NclMalloc((unsigned)sizeof(NclCoordAtt));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_VARCOORDATT;
	tmp->name = src_tree_names[Ncl_VARCOORDATT];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclVarCoordAttRefDestroy;
	tmp->sym = var;
	tmp->coordnamenode = coord;
	tmp->attnamenode = attname;
	tmp->subscript_list= subscript_list; 
	tmp->ref_type = Ncl_READIT;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}

void *_NclMakeExprNewOp
#if	NhlNeedProto
(void * size_expr, void * datatype,void *missing_expr)
#else
(size_expr,datatype,missing_expr)
void * size_expr;
void *datatype;
void *missing_expr;
#endif
{
	NclExprNew *tmp= (NclExprNew*)NclMalloc((unsigned)sizeof(NclExprNew));
	
	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_EXPRNEW;
	tmp->name = src_tree_names[Ncl_EXPRNEW];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->size_expr = size_expr;
	tmp->data_type_expr = datatype;
	tmp->missing_expr = missing_expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}
void *_NclMakeNewOp
#if	NhlNeedProto
(void * size_expr, struct _NclSymbol * datatype,void *missing_expr)
#else
(size_expr,datatype,missing_expr)
void * size_expr;
struct _NclSymbol *datatype;
void *missing_expr;
#endif
{
	NclNew *tmp= (NclNew*)NclMalloc((unsigned)sizeof(NclNew));

	if(tmp == NULL) {
		NhlPError(NhlFATAL,errno,"Not enough memory for source tree construction");
		return(NULL);
	}
	tmp->kind = Ncl_NEW;
	tmp->name = src_tree_names[Ncl_NEW];
	tmp->line = cur_line_number;
	tmp->file = cur_load_file;
	tmp->destroy_it = (NclSrcTreeDestroyProc)_NclGenericDestroy;
	tmp->size_expr = size_expr;
	tmp->data_sym = datatype;
	tmp->missing_expr = missing_expr;
	_NclRegisterNode((NclGenericNode*)tmp);
	return((void*)tmp);
}



void _NclFreeTree
#if	NhlNeedProto
(void )
#else
()
#endif
{
	int i;

	for(i = 0 ; i< node_list_index; i++) {
		(*node_list[i]->destroy_it)((struct ncl_genericnode*)node_list[i]);
	}
	_NclResetNodeList();
}

void _NclValOnly
#if	NhlNeedProto
(void *root)
#else
(root)
void *root;
#endif
{
	NclGenericNode *groot = (NclGenericNode*)root;
	NclIdnExpr *idnexpr = NULL;
	
	if(groot->kind == Ncl_IDNEXPR) {
		idnexpr = (NclIdnExpr*)((NclIdnExpr*)root)->idn_ref_node;
		if (idnexpr != NULL) {
			switch(((NclGenericRefNode*)idnexpr)->kind) {
				case Ncl_VAR: 
				case Ncl_GROUP: 
				case Ncl_VARCOORD: 
				case Ncl_FILEVAR: 
				case Ncl_FILEVARCOORD: 
				case Ncl_FILEGROUP: 
					((NclGenericRefNode*)idnexpr)->ref_type = Ncl_VALONLY;
					break;
				default:
					break;
			}
		}
/*
	} else if(groot->kind == Ncl_VAR) {
		((NclGenericRefNode*)groot)->ref_type = Ncl_VALONLY;
*/
	
	} else {
		return;
	}
}
#ifdef __cplusplus
}
#endif

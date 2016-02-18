#ifdef __cplusplus
extern  "C" {
#endif
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include "defs.h"
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "parser.h"
#include "OpsList.h"
#include "SrcTree.h"
#include "NclMdInc.h"
#include "DataSupport.h"
#include <errno.h>

typedef struct _NclLoopCoBrRec {
	NclExecuteReturnStatus kind; 
	int off;
	int line;
	char *file;
	struct _NclLoopCoBrRec *next;
}NclLoopCoBrRec;
typedef struct _NclLoopRec {
	struct _NclLoopCoBrRec *cobr_list;
	struct _NclLoopRec* next;
}NclLoopRec;

extern char *cur_load_file;
extern int loading;
extern int cmd_line;

static struct _NclLoopRec *current_loop = NULL;

static void _NclNewLoop
#if	NhlNeedProto
(void)
#else
()
#endif
{
	struct _NclLoopRec *tmp;

	tmp = current_loop;

	current_loop = NclMalloc(sizeof(NclLoopRec));
	current_loop->cobr_list = NULL;
	current_loop->next = tmp;
	return;
}

static void _NclEndLoop
#if	NhlNeedProto
(int cont_off, int break_off)
#else
(cont_off,break_off)
int cont_off;
int break_off;
#endif
{
	struct _NclLoopRec *tmp;
	struct _NclLoopCoBrRec *tmp2,*tmp3;
	

	tmp = current_loop;
	current_loop = current_loop->next;
	tmp2 = tmp->cobr_list;
	while(tmp2 != NULL) {
		switch(tmp2->kind) {
		case Ncl_BREAKS:
			_NclPutInstrAt(tmp2->off,break_off,tmp2->line,tmp2->file);
		break;
		case Ncl_CONTINUES:
			_NclPutInstrAt(tmp2->off,cont_off,tmp2->line,tmp2->file);
		break;
		default:
			NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal Error"));
			return;
		}
		tmp3= tmp2;
		tmp2 = tmp2->next;
		NclFree(tmp3);
	}
	NclFree(tmp);
}

static void _NclPushContinue
#if 	NhlNeedProto
(int cont_off,int line,char *file)
#else
(cont_off)
int cont_off;
#endif
{
	struct _NclLoopCoBrRec *tmp = NULL;
	if(current_loop == NULL) {
		_NclPutInstrAt(cont_off,_NclGetCurrentOffset(),line,file);
	} else {
		tmp = NclMalloc(sizeof(NclLoopCoBrRec));
		tmp->kind = Ncl_CONTINUES;
		tmp->off = cont_off;
		tmp->line = line;
		tmp->file = file;
		tmp->next = current_loop->cobr_list;
		current_loop->cobr_list = tmp;
	}
	return;
}

static void _NclPushBreak
#if 	NhlNeedProto
(int break_off,int line,char *file)
#else
(break_off)
int break_off;
#endif
{
	struct _NclLoopCoBrRec *tmp = NULL;
	if(current_loop == NULL) {
		_NclPutInstrAt(break_off,_NclGetCurrentOffset(),line,file);
	} else {
		tmp = NclMalloc(sizeof(NclLoopCoBrRec));
		tmp->kind = Ncl_BREAKS;
		tmp->off = break_off;
		tmp->line = line;
		tmp->file = file;
		tmp->next = current_loop->cobr_list;
		current_loop->cobr_list = tmp;
	}
	return;
}



/*
 * Function:	_NclTransTerminate
 *
 * Description:	 Called by the parser when Translating is finished
 *		to guarentee that the last instruction exits the current 
 *		Execute instance is a STOPSEQ.
 *
 * In Args:	
 *
 * Out Args:	
 *
 * Scope:	
 * Returns:	
 * Side Effect:	
 */
void _NclTransTerminate
#if	NhlNeedProto
(void)
#else
()
#endif
{
			_NclPutInstr(STOPSEQ,0,NULL);
}
int number_of_constants = 0;

static NclObj CreateConst
#if     NhlNeedProto
(NclObj inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, void *val, NclScalar *missing_value, int n_dims, ng_size_t *dim_sizes, NclStatus status, NclSelectionRecord *sel_rec, NclTypeClass type)
#else
(inst, theclass, obj_type, obj_type_mask, val, missing_value, n_dims, dim_sizes, status, sel_rec, type)
NclObj inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
ng_size_t *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
NclTypeClass type;
#endif
{
	NclObj tmp_obj;
	number_of_constants++;
	tmp_obj = (NclObj) _NclCreateMultiDVal(inst, theclass, obj_type, obj_type_mask, val, missing_value, n_dims, dim_sizes, status, sel_rec, type);
	tmp_obj->obj.is_constant = tmp_obj->obj.id+1;
	return((NclObj)tmp_obj);

}

static NclObj CreateTrueConst
#if     NhlNeedProto
(void)
#else
()
#endif
{
	static int first = 1;
	NclObj tmp_obj;
	
	if(first) {
		number_of_constants++;
		first = 0;
	}
	tmp_obj = (NclObj) _NclCreateTrue();
	tmp_obj->obj.is_constant = tmp_obj->obj.id+1;
	return(tmp_obj);
}
static NclObj CreateFalseConst
#if     NhlNeedProto
(void)
#else
()
#endif
{
	static int first = 1;
	NclObj tmp_obj;
	
	if(first) {
		number_of_constants++;
		first = 0;
	}
	tmp_obj = (NclObj) _NclCreateFalse();
	tmp_obj->obj.is_constant = tmp_obj->obj.id+1;
	return(tmp_obj);
}

static NclObj CreateLMissingConst
#if     NhlNeedProto
(void)
#else
()
#endif
{
	static int first = 1;
	NclObj tmp_obj;
	
	if(first) {
		number_of_constants++;
		first = 0;
	}
	tmp_obj = (NclObj) _NclCreateLMissing();
	tmp_obj->obj.is_constant = tmp_obj->obj.id+1;
	return(tmp_obj);
}




/*
 * Function:	_NclTranslate
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
int _NclTranslate
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
	int off1 = -1 ,off2 = -1 ,off3 = -1, off4 = -1 ,off5 = -1;
	static int nesting = 0;
	NclObj tmp_md = NULL;
	void *tmp_val = NULL;
	ng_size_t dim_size = 1;

	nesting++;

/*
* off1 must be used for offset that is returned to calling env.!!!
*/

if(groot != NULL) {
	switch(groot->kind) {
		case Ncl_BLOCK:
		{
			NclBlock *blk = (NclBlock*)root;

			step = blk->stmnts;	
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);
				step=step->next;
				while(step != NULL) {
					(void)_NclTranslate(step->node,fp);
					step = step->next;
				}	
			} else {
				off1 = _NclGetCurrentOffset();
			}
		break;
		}
		case Ncl_RETURN:
		{
			NclReturn *ret = (NclReturn*)root;
			if(ret->expr != NULL) {	
				off1 = _NclTranslate(ret->expr,fp);
				_NclPutInstr(RETURN_OP,ret->line,ret->file);
			} else {
				off1 = _NclPutInstr(CRETURN_OP,ret->line,ret->file);
			}
/*
* All that is needed is the return op to tell machine that top of
* stack should be placed in frames return_value field
*/
		break;
		}
		case Ncl_IFTHEN:
		{
			NclIfThen *ifthen = (NclIfThen*)root;

			off1 = _NclTranslate(ifthen->cond_expr,fp);
			_NclPutInstr(JMPFALSE,ifthen->line,ifthen->file);
			off3 = _NclPutInstr(NOOP,ifthen->line,ifthen->file);
			step = ifthen->block_stmnt_list;
			while(step != NULL) {
                                (void)_NclTranslate(step->node,fp);
                                step = step->next;
                        }
			_NclPutInstrAt(off3,_NclGetCurrentOffset(),ifthen->line,ifthen->file);

			break;
		}
		case Ncl_IFTHENELSE:
		{
			NclIfThenElse *ifthenelse = (NclIfThenElse*)root;

			off1 = _NclTranslate(ifthenelse->cond_expr,fp);
			_NclPutInstr(JMPFALSE,ifthenelse->line,ifthenelse->file);
			off2 = _NclPutInstr(NOOP,ifthenelse->line,ifthenelse->file);
			step = ifthenelse->block_stmnt_list1;
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstr(JMP,ifthenelse->line,ifthenelse->file);
			off3 = _NclPutInstr(NOOP,ifthenelse->line,ifthenelse->file);
			step = ifthenelse->block_stmnt_list2;
			if(step != NULL) {
				off4 = _NclTranslate(step->node,fp);
				step = step->next;
			} 
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;
			}
			if(off4 != -1)  {
				_NclPutInstrAt(off2,off4,ifthenelse->line,ifthenelse->file);
			} else {
				(void)_NclPutInstrAt(off2,_NclGetCurrentOffset(),ifthenelse->line,ifthenelse->file);
			}
			(void)_NclPutInstrAt(off3,_NclGetCurrentOffset(),ifthenelse->line,ifthenelse->file);
			break;
		}
		case Ncl_VISBLKCREATE:
		{
			NclVisblk *vblk = (NclVisblk*)root;
			int nres = 0;

			step = vblk->resource_list;
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);	
				step = step->next;
				nres++;
			}
			while(step != NULL) {
				_NclTranslate(step->node,fp);	
				step = step->next;
				nres++;
			}
			off2 = _NclTranslate(vblk->obj_name_expr,fp);
			if(vblk->objparent != NULL) {
				_NclTranslate(vblk->objparent,fp);	
				_NclPutInstr(CREATE_OBJ_WP_OP,vblk->line,vblk->file);
			} else {
				 _NclPutInstr(CREATE_OBJ_OP,vblk->line,vblk->file);
			}
			if(off1 == -1) 
				off1 = off2;
			_NclPutIntInstr(nres,vblk->line,vblk->file);
			_NclPutInstr((NclValue)vblk->objtype,vblk->line,vblk->file);
			break;
		}
		case Ncl_VISBLKSET:
		{
			NclSGVisblk *vblk = (NclSGVisblk*)root;
			int nres = 0;
			step = vblk->resource_list;
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);
				step = step->next;
				nres++;
			}
			while(step != NULL) {
				_NclTranslate(step->node,fp);
				step = step->next;
				nres++;
			}
			off2 = _NclTranslate(vblk->objname,fp);
			if(off1 == -1) 
				off1 = off2;
			_NclPutInstr(SET_OBJ_OP,vblk->line,vblk->file);
			_NclPutIntInstr(nres,vblk->line,vblk->file);
			break;
		}
		case Ncl_VISBLKGET:
		{
			NclSGVisblk *vblk = (NclSGVisblk*)root;
			step = vblk->resource_list;
/* 
* Translating objname each time produces a lot of extra calls but is
* probably better than duplicating it on the top of the stack each
* iteration. Need to come up with POP_OP before any alternatives 
* can be investigated. With a POP_OP GET_OBJ_OP could always 
* push the object referenc back on the stack when done and then
* when the visblock is over it could pop it off the stack.
*/
			if(step != NULL) {
				off1 = _NclTranslate(vblk->objname,fp);
				_NclTranslate(step->node,fp);
				step = step->next;
			} else {
				off1 = _NclGetCurrentOffset(); 
			}
			while(step != NULL) {
				_NclTranslate(vblk->objname,fp);
				_NclTranslate(step->node,fp);
				step = step->next;
			}
			break;
		}
		case Ncl_RESOURCE:
		{
			NclResource *resource = (NclResource*)root;
			off1 = _NclTranslate(resource->resexpr,fp);
			_NclTranslate(resource->expr,fp);
			break;
		}
		case Ncl_GETRESOURCE:
		{
/*
* obj refernce is on top of stack when this 
* block starts.
*/
			NclGetResource *resource = (NclGetResource*)root;
			off1 = _NclTranslate(resource->resexpr,fp);
			_NclPutInstr((NclValue)GET_OBJ_OP,resource->line,resource->file);
/*
* This allows full range of assignment possibilities.
*/
			_NclTranslate(resource->target_idn,fp);
			break;
		}
		case Ncl_DOFROMTO:
                {
                        NclDoFromTo *dofromto = (NclDoFromTo*)root;

			if(dofromto->block_stmnt_list != NULL) {
				_NclNewLoop();
			
/*
* Two values of start expr pushed on stack
*/
                        	off1 = _NclPutInstr(PUSH_INT_LIT_OP,dofromto->line,dofromto->file);
				tmp_val = NclMalloc(sizeof(int));
				*(int*)tmp_val = 1;
				tmp_md = CreateConst(NULL,
                                                NULL,Ncl_MultiDValData,0,
                                                (void*)tmp_val,NULL,1,&dim_size,
                                                PERMANENT,NULL,(NclTypeClass)nclTypeintClass);
                        	_NclPutIntInstr(tmp_md->obj.id,dofromto->line,dofromto->file);
/*
* assigns increment value to inc_sym uncovers direction logical on top of stack
*/
				_NclPutInstr(ASSIGN_VAR_OP,dofromto->line,dofromto->file);
				_NclPutInstr((NclValue)dofromto->inc_sym,dofromto->line,dofromto->file);
				_NclPutIntInstr(0,dofromto->line,dofromto->file);
/*
* assigns direction sym uncovers start_expr value
*/
				_NclPutInstr(PUSH_LOGICAL_LIT_OP,dofromto->line,dofromto->file);
                        	tmp_md = CreateFalseConst();
				_NclPutIntInstr(tmp_md->obj.id,dofromto->line,dofromto->file);

				_NclPutInstr(ASSIGN_VAR_OP,dofromto->line,dofromto->file);
				_NclPutInstr((NclValue)dofromto->dir_sym,dofromto->line,dofromto->file);
				_NclPutIntInstr(0,dofromto->line,dofromto->file);

			
/*
* assigns start_expr to the loop counter variable
*/
				_NclTranslate(dofromto->start_expr,fp);
                        	((NclGenericRefNode*)dofromto->inc_var)->ref_type = Ncl_WRITEIT;
                        	_NclTranslate(dofromto->inc_var,fp);
		
				_NclTranslate(dofromto->end_expr,fp);
				((NclGenericRefNode*)dofromto->inc_var)->ref_type = Ncl_READIT;
                        	_NclTranslate(dofromto->inc_var,fp);
	
				_NclPutInstr(LOOP_VALIDATE_OP,dofromto->line,dofromto->file);
				_NclPutInstr((NclValue)dofromto->inc_sym,dofromto->line,dofromto->file);
				_NclPutInstr((NclValue)dofromto->dir_sym,dofromto->line,dofromto->file);
				
	
				_NclPutInstr(JMP,dofromto->line,dofromto->file);
				off2 = _NclPutInstr(NOOP,dofromto->line,dofromto->file);
				off3 = _NclTranslate(dofromto->end_expr,fp);
				((NclGenericRefNode*)dofromto->inc_var)->ref_type = Ncl_READIT;
                        	_NclTranslate(dofromto->inc_var,fp);
				_NclPutInstr(LOOP_INC_OP,dofromto->line,dofromto->file);
				_NclPutInstr((NclValue)dofromto->inc_sym,dofromto->line,dofromto->file);
				_NclPutInstr((NclValue)dofromto->dir_sym,dofromto->line,dofromto->file);

				off5 = _NclPutInstr(JMPFALSE,dofromto->line,dofromto->file);
                        	_NclPutInstrAt(off2, off5,dofromto->line,dofromto->file);
				off4 = _NclPutInstr(NOOP,dofromto->line,dofromto->file);
/*
* START LOOP BODY
*/
                        	step = dofromto->block_stmnt_list;
                        	while(step != NULL) {
                                	(void) _NclTranslate(step->node,fp);
                                	step = step->next;
                        	}
/*
* END LOOP BODY
*/
				_NclPutInstr(JMP,dofromto->line,dofromto->file);
				_NclPutInstr(off3,dofromto->line,dofromto->file);
                        	_NclPutInstrAt(off4, _NclGetCurrentOffset(),dofromto->line,dofromto->file);
				_NclEndLoop(off3,_NclGetCurrentOffset());
			} else {
				off1 = _NclPutInstr(NOOP,dofromto->line,dofromto->file);
				if(dofromto->file == NULL) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) is being ignored",(cmd_line ? dofromto->line - 1 : dofromto->line));
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) in file (%s) is being ignored",dofromto->line, dofromto->file);
				}
			}
                        break;
		}

		case Ncl_DOFROMTOSTRIDE:
		{
			NclDoFromToStride *dofromtostride = (NclDoFromToStride*)root;
		
			if(dofromtostride->block_stmnt_list != NULL) {	
				_NclNewLoop();	
				off1 = _NclTranslate(dofromtostride->stride_expr,fp);
				_NclPutInstr(ASSIGN_VAR_OP,dofromtostride->line,dofromtostride->file);
				_NclPutInstr((NclValue)dofromtostride->inc_sym,dofromtostride->line,dofromtostride->file);
				_NclPutIntInstr(0,dofromtostride->line,dofromtostride->file);
		



	
				_NclTranslate(dofromtostride->end_expr,fp);
				_NclTranslate(dofromtostride->start_expr,fp);
				_NclPutInstr(LE_OP,dofromtostride->line,dofromtostride->file);
				_NclPutInstr(ASSIGN_VAR_OP,dofromtostride->line,dofromtostride->file);
                                _NclPutInstr((NclValue)dofromtostride->dir_sym,dofromtostride->line,dofromtostride->file);
                                _NclPutIntInstr(0,dofromtostride->line,dofromtostride->file);


				_NclTranslate(dofromtostride->start_expr,fp);
				((NclGenericRefNode*)dofromtostride->inc_var)->ref_type = Ncl_WRITEIT;
				_NclTranslate(dofromtostride->inc_var,fp);

				_NclTranslate(dofromtostride->end_expr,fp);
                                ((NclGenericRefNode*)dofromtostride->inc_var)->ref_type = Ncl_READIT;
                                _NclTranslate(dofromtostride->inc_var,fp);

				_NclPutInstr(LOOP_VALIDATE_OP,dofromtostride->line,dofromtostride->file);
                                _NclPutInstr((NclValue)dofromtostride->inc_sym,dofromtostride->line,dofromtostride->file);
                                _NclPutInstr((NclValue)dofromtostride->dir_sym,dofromtostride->line,dofromtostride->file);



				_NclPutInstr(JMP,dofromtostride->line,dofromtostride->file);
                                off2 = _NclPutInstr(NOOP,dofromtostride->line,dofromtostride->file);
                                off3 = _NclTranslate(dofromtostride->end_expr,fp);
                                ((NclGenericRefNode*)dofromtostride->inc_var)->ref_type = Ncl_READIT;
                                _NclTranslate(dofromtostride->inc_var,fp);
                                _NclPutInstr(LOOP_INC_OP,dofromtostride->line,dofromtostride->file);
                                _NclPutInstr((NclValue)dofromtostride->inc_sym,dofromtostride->line,dofromtostride->file);
                                _NclPutInstr((NclValue)dofromtostride->dir_sym,dofromtostride->line,dofromtostride->file);

                                off5 = _NclPutInstr(JMPFALSE,dofromtostride->line,dofromtostride->file);
                                _NclPutInstrAt(off2, off5,dofromtostride->line,dofromtostride->file);
                                off4 = _NclPutInstr(NOOP,dofromtostride->line,dofromtostride->file);

				step = dofromtostride->block_stmnt_list;
                                while(step != NULL) {
                                        (void) _NclTranslate(step->node,fp);
                                        step = step->next;
                                }
                                _NclPutInstr(JMP,dofromtostride->line,dofromtostride->file);
                                _NclPutInstr(off3,dofromtostride->line,dofromtostride->file);
                                _NclPutInstrAt(off4, _NclGetCurrentOffset(),dofromtostride->line,dofromtostride->file);
				_NclEndLoop(off3,_NclGetCurrentOffset());
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) being ignored",dofromtostride->line);
				off1 = _NclPutInstr(NOOP,dofromtostride->line,dofromtostride->file);
				if(dofromtostride->file == NULL) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) is being ignored",(cmd_line ? dofromtostride->line - 1 : dofromtostride->line));
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) in file (%s) is being ignored",dofromtostride->line ,dofromtostride->file);
				}

			}
			break;

		}			
		case Ncl_ASSIGN:
		{
			NclAssign *assign = (NclAssign*)root;
			NclIdnExpr *idnexpr = NULL;
			int nsubs = 0,nsubs_lhs = 0;

/*
* OK why is all this need? Assigment from one variable to another is
* different than any other identifier references. When one variable
* is being assigned to another variable all of the attributes, 
* coordinate vars, and dimension names need to be assigned in 
* addition to the value. Either major mods were need to the
* parser or the following type of code needed to be added.
* All other references just get the value of the variable and
* not the rest of the info.
*/
			if(((NclGenericNode*)assign->right_side)->kind == Ncl_IDNEXPR) {
				off1 = -1;
				off2 = -1;
				off3 = -1;
				idnexpr = (NclIdnExpr*)assign->right_side;
				switch(((NclGenericNode*)idnexpr->idn_ref_node)->kind) {
				case Ncl_VAR: {
				if(((NclGenericNode*)assign->left_side)->kind == Ncl_VAR) {
					NclVar *var = (NclVar*)(idnexpr->idn_ref_node);
					NclVar *lhs_var = (NclVar*)(assign->left_side);
					
					if(lhs_var->subscript_list != NULL) {
						off1 = _NclPutInstr(ISDEFINED_OP,lhs_var->line,lhs_var->file);
						_NclPutInstr((NclValue)lhs_var->sym,lhs_var->line,lhs_var->file);
						step = lhs_var->subscript_list;
						nsubs_lhs = 0;
						while(step != NULL) {
							(void)_NclTranslate(step->node,fp);
							step = step->next;
							nsubs_lhs++;
						}
					}
					if(var->subscript_list != NULL) {
						off2 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
						_NclPutInstr((NclValue)var->sym,var->line,var->file);
						step = var->subscript_list;
						nsubs = 0;
						while(step != NULL) {
							(void)_NclTranslate(step->node,fp);
							step = step->next;
							nsubs++;
						}
					}
					off3 = _NclPutInstr((NclValue)ASSIGN_VAR_VAR_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					_NclPutIntInstr(nsubs,var->line,var->file);
					_NclPutInstr((NclValue)lhs_var->sym,lhs_var->line,lhs_var->file);
					_NclPutIntInstr(nsubs_lhs,var->line,var->file);
					if(off1 == -1) {
						if(off2 == -1) {
							off1 = off3;
						} else {
							off1 = off2;
						}
					} 
				} else {
					off1 = _NclTranslate(assign->right_side,fp);
					_NclTranslate(assign->left_side,fp);
				}
				}
				break;
				case Ncl_VARCOORD: {
					NclCoord *coord = (NclCoord*)(idnexpr->idn_ref_node);
					if(coord->subscript_list != NULL) {
						step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
						step = step->next;
						nsubs = 1;
						while(step != NULL) {
							(void)_NclTranslate(step->node,fp);
							nsubs++;
							step = step->next;
						}
						_NclTranslate(coord->coordnamenode,fp);
						_NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
					} else {
						off1 = _NclTranslate(coord->coordnamenode,fp);
						_NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
					}
					_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
/*
					_NclPutInstr((NclValue)coord->coord_name_q,coord->line,coord->file);
*/
					_NclPutIntInstr(nsubs,coord->line,coord->file);
					_NclTranslate(assign->left_side,fp);
				}
				break;
#if 0
				/*
				 * if assigning to a double variable a real literal should be treated as a double.
				 * unfortunately I cannot yet see how one knows the type of a variable here. 
                                 * so leave this out for now.
                                 */
				case Ncl_REAL: {
					NclVar *lhs_var = (NclVar*)(assign->left_side);
					NclReal *real = (NclReal*)idnexpr->idn_ref_node);
					off1 = _NclTranslate(assign->right_side,fp);
					_NclTranslate(assign->left_side,fp);
				}
				break;
#endif
				case Ncl_FILEVARCOORD: 
				case Ncl_FILEVAR: 
				default:
					off1 = _NclTranslate(assign->right_side,fp);
					_NclTranslate(assign->left_side,fp);
					break;
				}
			} else {
				off1 = _NclTranslate(assign->right_side,fp);
				_NclTranslate(assign->left_side,fp);
			}
			break;
		}
		case Ncl_REASSIGN:
		{
			NclAssign *assign = (NclAssign*)root;
			NclIdnExpr *idnexpr = NULL;
			int nsubs = 0,nsubs_lhs = 0;
		
			/*
			* Ressigment from one variable to another is
			* different than any other identifier references. When one variable
			* is being assigned to another variable all of the attributes, 
			* coordinate vars, and dimension names need to be assigned in 
			* addition to the value. Either major mods were need to the
			* parser or the following type of code needed to be added.
			* All other references just get the value of the variable and
			* not the rest of the info.
			*/

			if(((NclGenericNode*)assign->right_side)->kind == Ncl_IDNEXPR)
			{
				off1 = -1;
				off2 = -1;
				off3 = -1;
				idnexpr = (NclIdnExpr*)assign->right_side;
				switch(((NclGenericNode*)idnexpr->idn_ref_node)->kind)
				{
				case Ncl_VAR:
				{
					if(((NclGenericNode*)assign->left_side)->kind == Ncl_VAR)
					{
						NclVar *var = (NclVar*)(idnexpr->idn_ref_node);
						NclVar *lhs_var = (NclVar*)(assign->left_side);
					
						if(lhs_var->subscript_list != NULL)
						{
							off1 = _NclPutInstr(ISDEFINED_OP,lhs_var->line,lhs_var->file);
							_NclPutInstr((NclValue)lhs_var->sym,lhs_var->line,lhs_var->file);
							step = lhs_var->subscript_list;
							nsubs_lhs = 0;
							while(step != NULL)
							{
								(void)_NclTranslate(step->node,fp);
								step = step->next;
								nsubs_lhs++;
							}
						}

						if(var->subscript_list != NULL)
						{
							off2 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
							_NclPutInstr((NclValue)var->sym,var->line,var->file);
							step = var->subscript_list;
							nsubs = 0;
							while(step != NULL)
							{
								(void)_NclTranslate(step->node,fp);
								step = step->next;
								nsubs++;
							}
						}

						off3 = _NclPutInstr((NclValue)REASSIGN_VAR_VAR_OP,var->line,var->file);
						_NclPutInstr((NclValue)var->sym,var->line,var->file);
						_NclPutIntInstr(nsubs,var->line,var->file);
						_NclPutInstr((NclValue)lhs_var->sym,lhs_var->line,lhs_var->file);
						_NclPutIntInstr(nsubs_lhs,var->line,var->file);

						if(off1 == -1)
						{
							if(off2 == -1)
								off1 = off3;
							else
								off1 = off2;
						} 
					}
					else
					{
						off1 = _NclTranslate(assign->right_side,fp);
						_NclTranslate(assign->left_side,fp);
					}
				}
				break;
				case Ncl_VARCOORD:
				{
					NclCoord *coord = (NclCoord*)(idnexpr->idn_ref_node);
					if(coord->subscript_list != NULL)
					{
						step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
						step = step->next;
						nsubs = 1;
						while(step != NULL)
						{
							(void)_NclTranslate(step->node,fp);
							nsubs++;
							step = step->next;
						}
						_NclTranslate(coord->coordnamenode,fp);
						_NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
					}
					else
					{
						off1 = _NclTranslate(coord->coordnamenode,fp);
						_NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
					}
					_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
					_NclPutIntInstr(nsubs,coord->line,coord->file);
					_NclTranslate(assign->left_side,fp);
				}
				break;
				case Ncl_FILEVARCOORD: 
				case Ncl_FILEVAR: 
				default:
					off1 = _NclTranslate(assign->right_side,fp);
					_NclTranslate(assign->left_side,fp);
					break;
				}
			}
			else
			{
				off1 = _NclTranslate(assign->right_side,fp);
				_NclTranslate(assign->left_side,fp);
			}
			break;
		}
		case Ncl_INTSUBSCRIPT:	
		{
			NclSubscript *subscript = (NclSubscript*)
					root;
			if(subscript->dimname_expr != NULL) {
/*
				off1 = _NclPutInstr(PUSH_STRING_LIT_OP,subscript->line,subscript->file);
				tmp_val = NclMalloc(sizeof(NclQuark));
				*(NclQuark*)tmp_val = subscript->dimname_q;
				tmp_md = CreateConst(NULL, NULL,Ncl_MultiDValData,0,
					(void*)tmp_val,NULL,1,&dim_size, PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
				_NclPutIntInstr(tmp_md->obj.id,subscript->line,subscript->file);
*/
				off1 = _NclTranslate(subscript->dimname_expr,fp);
				_NclTranslate(subscript->subexpr,fp);
				_NclPutInstr(NAMED_INT_SUBSCRIPT_OP,subscript->line,subscript->file);
			} else {
				off1 = _NclTranslate(subscript->subexpr,fp);
				_NclPutInstr(INT_SUBSCRIPT_OP,subscript->line,subscript->file);
			}
			break;
		}
		case Ncl_COORDSUBSCRIPT:	
		{
			NclSubscript *subscript = (NclSubscript*)
					root;
			if(subscript->dimname_expr != NULL) {
/*
				off1 = _NclPutInstr(PUSH_STRING_LIT_OP,subscript->line,subscript->file);
				tmp_val = NclMalloc(sizeof(NclQuark));
				*(NclQuark*)tmp_val = subscript->dimname_q;
				tmp_md = CreateConst(NULL, NULL,Ncl_MultiDValData,0,
					(void*)tmp_val,NULL,1,&dim_size, PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
				_NclPutIntInstr(tmp_md->obj.id,subscript->line,subscript->file);
*/
				off1 = _NclTranslate(subscript->dimname_expr,fp);
				_NclTranslate(subscript->subexpr,fp);
				_NclPutInstr(NAMED_COORD_SUBSCRIPT_OP,subscript->line,subscript->file);
			} else {
				off1 = _NclTranslate(subscript->subexpr,fp);
				_NclPutInstr(COORD_SUBSCRIPT_OP,subscript->line,subscript->file);
			}
			break;
		}
		case Ncl_SINGLEINDEX:
		{
			NclSingleIndex *singleindex = (NclSingleIndex*)root;
			off1 = _NclTranslate(singleindex->expr,fp);
			_NclPutInstr(SINGLE_INDEX_OP,singleindex->line,singleindex->file);
			break;
		}
		case Ncl_RANGEINDEX:
		{
			NclRangeIndex *rangeindex = (NclRangeIndex*)root;	
			if(rangeindex->start_expr != NULL) {
				off1 = _NclTranslate(rangeindex->start_expr,fp);
			} else {
				off1 = _NclPutInstr(DEFAULT_RANGE_OP,rangeindex->line,rangeindex->file);
			}
			if(rangeindex->end_expr != NULL) {
				_NclTranslate(rangeindex->end_expr,fp);
			} else {
				_NclPutInstr(DEFAULT_RANGE_OP,rangeindex->line,rangeindex->file);
			}
			if(rangeindex->stride != NULL) {
				_NclTranslate(rangeindex->stride,fp);
			} else {
				_NclPutInstr(DEFAULT_RANGE_OP,rangeindex->line,rangeindex->file);
			}
			_NclPutInstr(RANGE_INDEX_OP,rangeindex->line,rangeindex->file);
			break;
		}
		case Ncl_IDNEXPR:
		{
			NclIdnExpr *idnexpr = (NclIdnExpr*)root;
			off1 = _NclTranslate(idnexpr->idn_ref_node,fp);
			break;
		}
		case Ncl_NEGEXPR:
		{
			NclMonoExpr *monoexpr = (NclMonoExpr*) root;
			
			off1 = _NclTranslate(monoexpr->expr,fp);
			_NclPutInstr(NEG_OP,monoexpr->line,monoexpr->file);
			break;
		}
		case Ncl_NOTEXPR:
		{
			NclMonoExpr *monoexpr = (NclMonoExpr*) root;
			off1 = _NclTranslate(monoexpr->expr,fp);
			_NclPutInstr(NOT_OP,monoexpr->line,monoexpr->file);
			break;
		}
		case Ncl_MODEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(MOD_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_OREXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(JMP_SCALAR_TRUE_OP,dualexpr->line,dualexpr->file);
			off2 = _NclPutInstr(NOOP,dualexpr->line,dualexpr->file);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(OR_OP,dualexpr->line,dualexpr->file);
			_NclPutInstrAt(off2,_NclGetCurrentOffset(),dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_ANDEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(JMP_SCALAR_FALSE_OP,dualexpr->line,dualexpr->file);
			off2 = _NclPutInstr(NOOP,dualexpr->line,dualexpr->file);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(AND_OP,dualexpr->line,dualexpr->file);
			_NclPutInstrAt(off2,_NclGetCurrentOffset(),dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_XOREXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(XOR_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_LTSELECTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(LTSEL_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_GTSELECTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(GTSEL_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_PLUSEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(PLUS_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_MINUSEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(MINUS_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_MULEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(MUL_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_MATMULEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(MAT_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_DIVEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(DIV_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_EXPEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(EXP_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_LEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(LE_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_GEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(GE_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_GTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(GT_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_LTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(LT_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_EQEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(EQ_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_NEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->left_expr,fp);
			_NclTranslate(dualexpr->right_expr,fp);
			_NclPutInstr(NE_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_REAL:
		{
			NclReal *real = (NclReal*)root;
			
			off1 = _NclPutInstr(PUSH_REAL_LIT_OP,real->line,real->file);
			if (real->is_double) {
				tmp_val = NclMalloc(sizeof(double));
				*(double*)tmp_val = real->real;
				tmp_md = CreateConst(NULL, NULL,Ncl_MultiDValData,0,
						     (void*)tmp_val,NULL,1,&dim_size, PERMANENT,NULL,
						     (NclTypeClass)nclTypedoubleClass);
			}
			else {
				tmp_val = NclMalloc(sizeof(float));
				*(float*)tmp_val = real->real;
				tmp_md = CreateConst(NULL, NULL,Ncl_MultiDValData,0,
						     (void*)tmp_val,NULL,1,&dim_size, PERMANENT,NULL,
						     (NclTypeClass)nclTypefloatClass);
			}
			_NclPutIntInstr(tmp_md->obj.id,real->line,real->file);
			break;
		}
		case Ncl_LOGICAL:
		{
			NclInt *integer = (NclInt*)root;
			off1 = _NclPutInstr(PUSH_LOGICAL_LIT_OP,integer->line,integer->file);
			if(integer->integer == -1) {
				tmp_md = CreateLMissingConst();
			}
			else if (integer->integer) {
				tmp_md = CreateTrueConst();
			} else {
				tmp_md = CreateFalseConst();
			}

			_NclPutIntInstr(tmp_md->obj.id,integer->line,integer->file);
			break;
		}
		case Ncl_INT:
		{
			NclInt *integer = (NclInt*)root;
			NclTypeClass tclass;

			off1 = _NclPutInstr(PUSH_INT_LIT_OP,integer->line,integer->file);
			switch (integer->int_type) {
			case 'b':
				tmp_val = NclMalloc(sizeof(char));
				*(char*)tmp_val = (char) integer->integer;
				tclass = (NclTypeClass) nclTypebyteClass;
				break;
			case 'h':
				tmp_val = NclMalloc(sizeof(short));
				*(short*)tmp_val = (short) integer->integer;
				tclass = (NclTypeClass) nclTypeshortClass;
				break;
			case 'i':
				tmp_val = NclMalloc(sizeof(int));
				*(int*)tmp_val = (int) integer->integer;
				tclass = (NclTypeClass) nclTypeintClass;
				break;
			case 'l':
				tmp_val = NclMalloc(sizeof(long));
				*(long*)tmp_val = (long) integer->integer;
				tclass = (NclTypeClass) nclTypelongClass;
				break;
			case 'q':
				tmp_val = NclMalloc(sizeof(long long));
				*(long long*)tmp_val = integer->integer;
				tclass = (NclTypeClass) nclTypeint64Class;
				break;
			case 'C':
				tmp_val = NclMalloc(sizeof(byte));
				*(unsigned char*)tmp_val = (unsigned char) integer->integer;
				tclass = (NclTypeClass) nclTypecharClass;
				break;
			case 'H':
				tmp_val = NclMalloc(sizeof(unsigned short));
				*(unsigned short*)tmp_val = (unsigned short) integer->integer;
				tclass = (NclTypeClass) nclTypeushortClass;
				break;
			case 'I':
				tmp_val = NclMalloc(sizeof(unsigned int));
				*(unsigned int*)tmp_val = (unsigned int) integer->integer;
				tclass = (NclTypeClass) nclTypeuintClass;
				break;
			case 'L':
				tmp_val = NclMalloc(sizeof(unsigned long));
				*(unsigned long*)tmp_val = (unsigned long) integer->integer;
				tclass = (NclTypeClass) nclTypeulongClass;
				break;
			case 'Q':
				tmp_val = NclMalloc(sizeof(unsigned long long));
				*(unsigned long long*)tmp_val = (unsigned long long)integer->integer;
				tclass = (NclTypeClass) nclTypeuint64Class;
				break;
			case 'B':
				tmp_val = NclMalloc(sizeof(unsigned char));
				*(unsigned char*)tmp_val = (unsigned char)integer->integer;
				tclass = (NclTypeClass) nclTypeubyteClass;
				break;
			}

			tmp_md = CreateConst(NULL, NULL,Ncl_MultiDValData,0, 
					     (void*)tmp_val,NULL,1,&dim_size, PERMANENT,NULL,tclass);
			_NclPutIntInstr(tmp_md->obj.id,integer->line,integer->file);
			break;
		}
		case Ncl_STRING:
		{
			NclString *string= (NclString*)root;

			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,string->line,string->file);
			tmp_val = NclMalloc(sizeof(NclQuark));
			*(NclQuark*)tmp_val = string->string_q;
			tmp_md = CreateConst(NULL, NULL,Ncl_MultiDValData,0,
				(void*)tmp_val,NULL,1,&dim_size, PERMANENT,NULL,(NclTypeClass)nclTypestringClass);
			_NclPutIntInstr(tmp_md->obj.id,string->line,string->file);
			break;
		}
/*

Unneeded translations
		case Ncl_LOCALVARDEC:
		{
			NclLocalVarDec *localvardec = (NclLocalVarDec*)root;
			off1 = -1;
			break;
		}
		case Ncl_DIMSIZELISTNODE:
		{
			NclDimSizeListNode *dimsizelistnode = 
						(NclDimSizeListNode*)root;
			off1 = -1;
			break;
		}
*/
		case Ncl_PROCDEF:
		{
			NclProcDef * procdef = (NclProcDef*)root;
			_NclNewMachine();	
			(void)_NclTranslate(procdef->block,fp);
			_NclPutInstr(STOPSEQ,procdef->line,procdef->file);
			procdef->proc->u.procfunc->mach_rec_ptr = _NclPopMachine();

			off1 = _NclPutInstr(FPDEF,procdef->line,procdef->file);
			_NclPutInstr((NclValue)procdef->proc,procdef->line,procdef->file);
			_NclPutInstr((NclValue)procdef->proc->u.procfunc,procdef->line,procdef->file);
			_NclPutInstr((NclValue)procdef->proc->type,procdef->line,procdef->file);
			break;
		}
		case Ncl_EXTERNPROCDEF:
		{
			NclExternProcDef *externprocdef = (NclExternProcDef*)
						root;
			off1 = _NclPutInstr(NOOP,externprocdef->line,externprocdef->file);
			break;
		}
		case Ncl_INTRINSICPROCCALL:
		{
			NclProcCall *proccall = (NclProcCall*)root;	
			int i = 0;


			off1 = _NclPutInstr(NEW_FRAME_OP,proccall->line,proccall->file);
			_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
			off2 = _NclPutInstr(NOOP,proccall->line,proccall->file);
			step = proccall->arg_list;
			if(step != NULL) {
				_NclTranslate(step->node,fp);
				step= step->next;
				_NclPutInstr((NclValue)CONVERT_TO_LOCAL,proccall->line,proccall->file);
				_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
				_NclPutIntInstr(i,proccall->line,proccall->file);
				i = 1;
				while(step != NULL) {
					_NclTranslate(step->node,fp);
					_NclPutInstr((NclValue)CONVERT_TO_LOCAL,proccall->line,proccall->file);
					_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
					_NclPutIntInstr(i,proccall->line,proccall->file);
					step= step->next;
					i++;
				}
				_NclPutInstr(INTRINSIC_PROC_CALL,proccall->line,proccall->file);
			} else {
				_NclPutInstr(INTRINSIC_PROC_CALL,proccall->line,proccall->file);
			}
			_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
			_NclPutIntInstr(i,proccall->line,proccall->file);
			_NclPutInstrAt(off2,_NclGetCurrentOffset(),proccall->line,proccall->file);
			break;
		}
		case Ncl_EXTERNALPROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			off1 = _NclPutInstr(NOOP,proccall->line,proccall->file);
			break;
		}
		case Ncl_PROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			int i=0;
			

/*
* The reason the PROC_CALL_OP doesn't handle this is because it must
* be located underneath the argument list and the argument list expressions
* are most conviently just translated directly here rather than by the
* PROC_CALL_OP or before this and moved to correct locations
*/
			off1 = _NclPutInstr(NEW_FRAME_OP,proccall->line,proccall->file);
			_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
/*
* Save spot for next instruction offset
*/

			off2 = _NclPutInstr(NOOP,proccall->line,proccall->file);

/*
* Now translate argument_list each of which will will be left on the stack 
* in the appropriate order above the frame record
*/

			step = proccall->arg_list;
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step=step->next;
/*
* The following takes the value left on the stack from the above translation
* and the argument template provided by the symbol table and converts the
* argument to the local variable it is mapped to. Also if the value is a 
* reference to an identifier the selection record created, by the subscript
* operator and stored in the value object, is put in the frames parameter
* map so that the arguments can be remapped once execution of the function
* or procedure has completed. Type and dimension checks are done at this time
* also.
*/
				_NclPutInstr(CONVERT_TO_LOCAL,proccall->line,proccall->file);

				_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
				_NclPutIntInstr(i,proccall->line,proccall->file);
				i++;
			}
/*
*
* Then PROC_CALL_OP reserves stack locations for the variables defined
* in the local statment as well as any instantiated with in the function
* block.
* 
* It then sets the new pc to the first statment of the funciton block
* and calls execute 
* 
* when execute is done the PROC_CALL_OP  pops off local vars,parameters
* and the stack frame , frees space and resets pc to appropriate next 
* instruction. While poping off the parameters and before poping the stack
* frame the PROC_CALL_OP remaps the parameters back to their variables.
* It does this by using the parameter map available in the stack frame.
*
*/
			_NclPutInstr(PROC_CALL_OP,proccall->line,proccall->file);
			_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);

			_NclPutInstrAt(off2,_NclGetCurrentOffset(),proccall->line,proccall->file);
			
			break;
		}
		case Ncl_INTRINSICFUNCCALL:
		{
			NclFuncCall *funccall = (NclFuncCall*)root;	
			int i = 0;


			off1 = _NclPutInstr(NEW_FRAME_OP,funccall->line,funccall->file);
			_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
			off2 = _NclPutInstr(NOOP,funccall->line,funccall->file);

			step = funccall->arg_list;
			if(step != NULL) {
				_NclTranslate(step->node,fp);
				step= step->next;
				_NclPutInstr((NclValue)CONVERT_TO_LOCAL,funccall->line,funccall->file);
				_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
				_NclPutIntInstr(i,funccall->line,funccall->file);
				i = 1;
				while(step != NULL) {
					_NclTranslate(step->node,fp);
					step= step->next;
					_NclPutInstr((NclValue)CONVERT_TO_LOCAL,funccall->line,funccall->file);
					_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
					_NclPutIntInstr(i,funccall->line,funccall->file);
					i++;
				}
				_NclPutInstr(INTRINSIC_FUNC_CALL,funccall->line,funccall->file);
			} else {
				_NclPutInstr(INTRINSIC_FUNC_CALL,funccall->line,funccall->file);
			}
			_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
			_NclPutIntInstr(i,funccall->line,funccall->file);
			_NclPutInstrAt(off2,_NclGetCurrentOffset(),funccall->line,funccall->file);
			break;
		}
		case Ncl_EXTERNFUNCDEF:
		{
			NclExternFuncDef *externfuncdef = (NclExternFuncDef*)
							 root;
			off1 = _NclPutInstr(NOOP,externfuncdef->line,externfuncdef->file);
			break;
		}
		case Ncl_EXTERNFUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;

			off1 = _NclPutInstr(NOOP,funccall->line,funccall->file);
			break;
		}
		case Ncl_FUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;
			int i = 0;
/*
* The reason the FUNC_CALL_OP doesn't handle this is because it must
* be located underneath the argument list and the argument list expressions
* are most conviently just translated directly here rather than by the
* FUNC_CALL_OP or before this and moved to correct locations
*/
			off1 = _NclPutInstr(NEW_FRAME_OP,funccall->line,funccall->file);
			_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
/*
* Save spot for next instruction offset
*/
			off2 = _NclPutInstr(NOOP,funccall->line,funccall->file);
/*
* Now translate argument_list each of which will will be left on the stack 
* in the appropriate order above the frame record
*/

			step = funccall->arg_list;
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step=step->next;
				_NclPutInstr(CONVERT_TO_LOCAL,funccall->line,funccall->file);
				_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
				_NclPutIntInstr(i,funccall->line,funccall->file);
				i++;
			}
/*
* The FUNC_CALL_OP first visits each argument on the stack and 
* performs type and dimension checks based on template located
* in the symbol table
*
* Then FUNC_CALL_OP reserves stack locations for the variables defined
* in the local statment as well as any instantiated with in the function
* block.
* 
* It then sets the new pc to the first statment of the funciton block
* and calls execute 
* 
* when execute is done the FUNC_CALL_OP  pops off local vars,parameters
* and the stack frame , frees space and resets pc to appropriate next 
* instruction.
*
* Finally FUNC_CALL_OP leave function return value on top of stack for
* calling environment to use.
*
*/
			_NclPutInstr(FUNC_CALL_OP,funccall->line,funccall->file);
			_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
			_NclPutInstrAt(off2,_NclGetCurrentOffset(),funccall->line,funccall->file);
			break;
		}
		case Ncl_FUNCDEF:
		{
			NclFuncDef *funcdef = (NclFuncDef*)root;
		
			_NclNewMachine();
/*
* May need to get this starting offset in addtion to the machine
* record
*/	
			(void)_NclTranslate(funcdef->block,fp);
			_NclPutInstr(STOPSEQ,funcdef->line,funcdef->file);
			funcdef->func->u.procfunc->mach_rec_ptr = _NclPopMachine();
/*
* Since the function definition really doesn't have any runtime execution there
* is no correct starting offset to run. Therefore the FDEF is needed to guarentee
* compliance with how the rest of the constructs behave.  Also need to be able
* to get att symbol table entry for functions therefore spot after FDEF gets
* symbol table pointer 
*/
			
			off1 = _NclPutInstr(FPDEF,funcdef->line,funcdef->file);
			_NclPutInstr((NclValue)funcdef->func,funcdef->line,funcdef->file);
			_NclPutInstr((NclValue)funcdef->func->u.procfunc,funcdef->line,funcdef->file);
			_NclPutInstr((NclValue)funcdef->func->type,funcdef->line,funcdef->file);
			break;
		}	
		case Ncl_ARRAY:
		{
			NclArray *array = (NclArray*)root;
			
			step = array->rcl->list;
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);
				step = step->next;
			}
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;	
			}
			_NclPutInstr(ARRAY_LIT_OP,array->line,array->file);
			_NclPutInstr((NclValue)array->rcl->nelem,array->line,array->file);
			break;
		}
		case Ncl_LISTVAR:
		{
			NclListVar *listvar = (NclListVar*)root;
			
			step = listvar->rcl->list;
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);
				step = step->next;
			}
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;	
			}
			_NclPutInstr(LISTVAR_LIT_OP,listvar->line,listvar->file);
			_NclPutInstr((NclValue)listvar->rcl->nelem,listvar->line,listvar->file);
			break;
		}
		case Ncl_DOWHILE:
		{
			NclDoWhile *dowhilel = (NclDoWhile*)root;
			if(dowhilel->stmnts != NULL) {
				_NclNewLoop();
				off1 =  _NclTranslate(dowhilel->cond_expr,fp);
				_NclPutInstr(JMPFALSE,dowhilel->line,dowhilel->file);
				off2 = _NclPutInstr(NOOP,dowhilel->line,dowhilel->file);
				step = dowhilel->stmnts;
				while(step != NULL) {
					(void)_NclTranslate(step->node,fp);
					step = step->next;
				}
				_NclPutInstr(JMP,dowhilel->line,dowhilel->file);
				_NclPutInstr(off1,dowhilel->line,dowhilel->file);
				_NclPutInstrAt(off2,_NclGetCurrentOffset(),dowhilel->line,dowhilel->file);
				_NclEndLoop(off1,_NclGetCurrentOffset());
			} else {
				off1 = _NclPutInstr(NOOP,dowhilel->line,dowhilel->file);
				if(dowhilel->file== NULL) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) is being ignored",(cmd_line ? dowhilel->line - 1 : dowhilel->line));
				} else {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty loop body, statement ending at line (%d) in file (%s) is being ignored",dowhilel->line, dowhilel->file);
				}
			}
			break;
		}
		case Ncl_VAR:
		{
			NclVar *var = (NclVar*)root;
			int nsubs = 0;
	
			switch(var->ref_type) {	
			case Ncl_VALONLY:
				if(var->subscript_list != NULL) {
					off1 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					step = var->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(VARVAL_READ_OP,var->line,var->file);
				} else {

					off1= _NclPutInstr(VARVAL_READ_OP,var->line,var->file);
				}
				_NclPutInstr((NclValue)var->sym,var->line,var->file);
				_NclPutIntInstr(nsubs,var->line,var->file);
				break;
			case Ncl_READIT:
				if(var->subscript_list != NULL) {
					off1 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					step = var->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(VAR_READ_OP,var->line,var->file);
				} else {

					off1= _NclPutInstr(VAR_READ_OP,var->line,var->file);
				}
				_NclPutInstr((NclValue)var->sym,var->line,var->file);
				_NclPutIntInstr(nsubs,var->line,var->file);
				break;
			case Ncl_WRITEIT:
				if(var->subscript_list != NULL) {
					off1 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					step = var->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(ASSIGN_VAR_OP,var->line,var->file);
				} else {
					off1= _NclPutInstr(ASSIGN_VAR_OP,var->line,var->file);
				}
				_NclPutInstr((NclValue)var->sym,var->line,var->file);
				_NclPutIntInstr(nsubs,var->line,var->file);
				break;
			case Ncl_REWRITEIT:
				if(var->subscript_list != NULL)
				{
					off1 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					step = var->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL)
					{
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(REASSIGN_VAR_OP,var->line,var->file);
				}
				else
				{
					off1= _NclPutInstr(REASSIGN_VAR_OP,var->line,var->file);
				}
				_NclPutInstr((NclValue)var->sym,var->line,var->file);
				_NclPutIntInstr(nsubs,var->line,var->file);
				break;
			case Ncl_PARAMIT:	
				if(var->subscript_list != NULL) {
					off1 = _NclPutInstr(ISDEFINED_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					step = var->subscript_list;
					 _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(PARAM_VAR_OP,var->line,var->file);
				} else {
					off1= _NclPutInstr(PARAM_VAR_OP,var->line,var->file);
				}
				_NclPutInstr((NclValue)var->sym,var->line,var->file);
				_NclPutIntInstr(nsubs,var->line,var->file);
				break;
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Unknown var->ref_type %d\n", var->ref_type));
				return (NhlFATAL);
			}
			break;
		}
		case Ncl_FILEVARDIM:
		{
			NclFileVarDim *filevardim = (NclFileVarDim*)root;

			off1 = _NclTranslate(filevardim->dim_expr,fp);
			_NclTranslate(filevardim->filevarnode,fp);
			_NclPutInstr(ISDEFINED_OP,filevardim->line,filevardim->file);
			_NclPutInstr((NclValue)filevardim->filesym,filevardim->line,filevardim->file);
			switch(filevardim->ref_type) {
			case Ncl_READIT:
				_NclPutInstr(FILEVAR_DIM_OP,filevardim->line,filevardim->file);
				_NclPutInstr((NclValue)filevardim->filesym,filevardim->line,filevardim->file);
/*
				_NclPutInstr((NclValue)filevardim->filevar_q,filevardim->line,filevardim->file);
*/
				break;
			case Ncl_WRITEIT:
				_NclPutInstr(ASSIGN_FILEVAR_DIM_OP,filevardim->line,filevardim->file);
				_NclPutInstr((NclValue)filevardim->filesym,filevardim->line,filevardim->file);
/*
				_NclPutInstr((NclValue)filevardim->filevar_q,filevardim->line,filevardim->file);
*/
				break;
			case Ncl_PARAMIT:
				_NclPutInstr(PARAM_FILEVAR_DIM_OP,filevardim->line,filevardim->file);
				_NclPutInstr((NclValue)filevardim->filesym,filevardim->line,filevardim->file);
/*
				_NclPutInstr((NclValue)filevardim->filevar_q,filevardim->line,filevardim->file);
*/
				break;
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
				return (NhlFATAL);
			}
			break;
		}
		case Ncl_VARDIM:
		{
			NclVarDim *vardim = (NclVarDim*)root;

			off1 = _NclTranslate(vardim->dim_expr,fp);
			switch(vardim->ref_type) {
			case Ncl_READIT:
				_NclPutInstr(VAR_DIM_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				break;
			case Ncl_WRITEIT:
				_NclPutInstr(ASSIGN_VAR_DIM_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				break;
			case Ncl_PARAMIT:
				_NclPutInstr(PARAM_VAR_DIM_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				break;
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
				return (NhlFATAL);
			}
			break;
		}
		case Ncl_FILEVARATT:
		{
			NclFileVarAtt *filevaratt = (NclFileVarAtt*)root;
			int nsubs = 0;
			off1 = _NclPutInstr(ISDEFINED_OP,filevaratt->line,filevaratt->file);
			_NclPutInstr((NclValue)filevaratt->filesym,filevaratt->line,filevaratt->file);
			switch(filevaratt->ref_type) {
			case Ncl_READIT:
				if(filevaratt->subscript_list != NULL) {
					step = filevaratt->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevaratt->filevarnode,fp);
					_NclTranslate(filevaratt->attnamenode,fp);
					_NclPutInstr(FILEVARATT_OP,filevaratt->line,filevaratt->file);
				} else {
					_NclTranslate(filevaratt->filevarnode,fp);
					_NclTranslate(filevaratt->attnamenode,fp);
					_NclPutInstr(FILEVARATT_OP,filevaratt->line,filevaratt->file);
				}
				_NclPutInstr((NclValue)filevaratt->filesym,filevaratt->line,filevaratt->file);
/*
				_NclPutInstr((NclValue)filevaratt->filevar_q,filevaratt->line,filevaratt->file);
				_NclPutInstr((NclValue)filevaratt->attname_q,filevaratt->line,filevaratt->file);
*/
				_NclPutIntInstr(nsubs,filevaratt->line,filevaratt->file);
				break;
			case Ncl_WRITEIT:
				if(filevaratt->subscript_list != NULL) {
					step = filevaratt->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevaratt->filevarnode,fp);
					_NclTranslate(filevaratt->attnamenode,fp);
					_NclPutInstr(ASSIGN_FILEVARATT_OP,filevaratt->line,filevaratt->file);
				} else {
					_NclTranslate(filevaratt->filevarnode,fp);
					_NclTranslate(filevaratt->attnamenode,fp);
					_NclPutInstr(ASSIGN_FILEVARATT_OP,filevaratt->line,filevaratt->file);
				}
				_NclPutInstr((NclValue)filevaratt->filesym,filevaratt->line,filevaratt->file);
/*
				_NclPutInstr((NclValue)filevaratt->filevar_q,filevaratt->line,filevaratt->file);
				_NclPutInstr((NclValue)filevaratt->attname_q,filevaratt->line,filevaratt->file);
*/
				_NclPutIntInstr(nsubs,filevaratt->line,filevaratt->file);
				break;	
			case Ncl_PARAMIT:
				if(filevaratt->subscript_list != NULL) {
					step = filevaratt->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevaratt->filevarnode,fp);
					_NclTranslate(filevaratt->attnamenode,fp);
					_NclPutInstr(PARAM_FILEVARATT_OP,filevaratt->line,filevaratt->file);
				} else {
					_NclTranslate(filevaratt->filevarnode,fp);
					_NclTranslate(filevaratt->attnamenode,fp);
					_NclPutInstr(PARAM_FILEVARATT_OP,filevaratt->line,filevaratt->file);
				}
				_NclPutInstr((NclValue)filevaratt->filesym,filevaratt->line,filevaratt->file);
/*
				_NclPutInstr((NclValue)filevaratt->filevar_q,filevaratt->line,filevaratt->file);
				_NclPutInstr((NclValue)filevaratt->attname_q,filevaratt->line,filevaratt->file);
*/
				_NclPutIntInstr(nsubs,filevaratt->line,filevaratt->file);
				break;	
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}	
			break;
		}
		case Ncl_VARATT:
		{
			NclVarAtt *varatt = (NclVarAtt*)root;
			int nsubs = 0;
			switch(varatt->ref_type) {
			case Ncl_READIT:
				if(varatt->subscript_list != NULL) {
					step = varatt->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(VARATT_OP,varatt->line,varatt->file);
				} else {
					off1 = _NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(VARATT_OP,varatt->line,varatt->file);
				}
				_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
/*
				_NclPutInstr((NclValue)varatt->attname_q,varatt->line,varatt->file);
*/
				_NclPutIntInstr(nsubs,varatt->line,varatt->file);
				break;
			case Ncl_WRITEIT:
				if(varatt->subscript_list != NULL) {
					step = varatt->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(ASSIGN_VARATT_OP,varatt->line,varatt->file);
				} else {
					off1 = _NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(ASSIGN_VARATT_OP,varatt->line,varatt->file);
				}
				_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
/*
				_NclPutInstr((NclValue)varatt->attname_q,varatt->line,varatt->file);
*/
				_NclPutIntInstr(nsubs,varatt->line,varatt->file);
				break;	
			case Ncl_REWRITEIT:
				/*
				 *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
				 */

				if(varatt->subscript_list != NULL) {
					step = varatt->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(REASSIGN_VARATT_OP,varatt->line,varatt->file);
				} else {
					off1 = _NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(REASSIGN_VARATT_OP,varatt->line,varatt->file);
				}
				_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
				_NclPutIntInstr(nsubs,varatt->line,varatt->file);
				break;	
			case Ncl_PARAMIT:
				if(varatt->subscript_list != NULL) {
					step = varatt->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;		
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(PARAM_VARATT_OP,varatt->line,varatt->file);
				} else {
					off1 = _NclTranslate(varatt->attnamenode,fp);
					_NclPutInstr(PARAM_VARATT_OP,varatt->line,varatt->file);
				}
				_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
/*
				_NclPutInstr((NclValue)varatt->attname_q,varatt->line,varatt->file);
*/
				_NclPutIntInstr(nsubs,varatt->line,varatt->file);
				break;	
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}
			break;
		}
		case Ncl_FILEVARCOORDATT:
		{
			NclFileCoordAtt *filecoordatt = (NclFileCoordAtt*)root;
			int nsubs = 0;
			off1 = _NclPutInstr(ISDEFINED_OP,filecoordatt->line,filecoordatt->file);
			_NclPutInstr((NclValue)filecoordatt->filesym,filecoordatt->line,filecoordatt->file);
			switch(filecoordatt->ref_type) {
			case Ncl_READIT:
				if(filecoordatt->subscript_list != NULL) {
					step = filecoordatt->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoordatt->filevarnode,fp);
					_NclTranslate(filecoordatt->coordnamenode,fp);
					_NclTranslate(filecoordatt->attnamenode,fp);
					_NclPutInstr(FILEVAR_COORD_ATT_OP,filecoordatt->line,filecoordatt->file);
				} else {
					_NclTranslate(filecoordatt->filevarnode,fp);
					_NclTranslate(filecoordatt->coordnamenode,fp);
					_NclTranslate(filecoordatt->attnamenode,fp);
					_NclPutInstr(FILEVAR_COORD_ATT_OP,filecoordatt->line,filecoordatt->file);
				}
				_NclPutInstr((NclValue)filecoordatt->filesym,filecoordatt->line,filecoordatt->file);
				_NclPutIntInstr(nsubs,filecoordatt->line,filecoordatt->file);
				break;
			case Ncl_WRITEIT:
				if(filecoordatt->subscript_list != NULL) {
					step = filecoordatt->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoordatt->filevarnode,fp);
					_NclTranslate(filecoordatt->coordnamenode,fp);
					_NclTranslate(filecoordatt->attnamenode,fp);
					_NclPutInstr(ASSIGN_FILEVAR_COORD_ATT_OP,filecoordatt->line,filecoordatt->file);
				} else {
					_NclTranslate(filecoordatt->filevarnode,fp);
					_NclTranslate(filecoordatt->coordnamenode,fp);
					_NclTranslate(filecoordatt->attnamenode,fp);
					_NclPutInstr(ASSIGN_FILEVAR_COORD_ATT_OP,filecoordatt->line,filecoordatt->file);
				}
				_NclPutInstr((NclValue)filecoordatt->filesym,filecoordatt->line,filecoordatt->file);
/*
				_NclPutInstr((NclValue)filecoordatt->coord_name_q,filecoordatt->line,filecoordatt->file);
				_NclPutInstr((NclValue)filecoordatt->attname_q,filecoordatt->line,filecoordatt->file);
*/
				_NclPutIntInstr(nsubs,filecoordatt->line,filecoordatt->file);
				break;
			case Ncl_PARAMIT:
				if(filecoordatt->subscript_list != NULL) {
					step = filecoordatt->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoordatt->filevarnode,fp);
					_NclTranslate(filecoordatt->coordnamenode,fp);
					_NclTranslate(filecoordatt->attnamenode,fp);
					_NclPutInstr(PARAM_FILEVAR_COORD_ATT_OP,filecoordatt->line,filecoordatt->file);
				} else {
					_NclTranslate(filecoordatt->filevarnode,fp);
					_NclTranslate(filecoordatt->coordnamenode,fp);
					_NclTranslate(filecoordatt->attnamenode,fp);
					_NclPutInstr(PARAM_FILEVAR_COORD_ATT_OP,filecoordatt->line,filecoordatt->file);
				}
				_NclPutInstr((NclValue)filecoordatt->filesym,filecoordatt->line,filecoordatt->file);
/*
				_NclPutInstr((NclValue)filecoordatt->coord_name_q,filecoordatt->line,filecoordatt->file);
				_NclPutInstr((NclValue)filecoordatt->attname_q,filecoordatt->line,filecoordatt->file);
*/
				_NclPutIntInstr(nsubs,filecoordatt->line,filecoordatt->file);
				break;
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}
			break;
		}
		case Ncl_FILEVARCOORD:
		{
			NclFileCoord *filecoord = (NclFileCoord*)root;
			int nsubs = 0;
			off1 = _NclPutInstr(ISDEFINED_OP,filecoord->line,filecoord->file);
			_NclPutInstr((NclValue)filecoord->filesym,filecoord->line,filecoord->file);
			switch(filecoord->ref_type) {
			case Ncl_VALONLY:
				if(filecoord->subscript_list != NULL) {
					step = filecoord->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(FILEVARVAL_COORD_OP,filecoord->line,filecoord->file);
				} else {
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(FILEVARVAL_COORD_OP,filecoord->line,filecoord->file);
				}
				_NclPutInstr((NclValue)filecoord->filesym,filecoord->line,filecoord->file);
				_NclPutIntInstr(nsubs,filecoord->line,filecoord->file);
				break;
			case Ncl_READIT:
				if(filecoord->subscript_list != NULL) {
					step = filecoord->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(FILEVAR_COORD_OP,filecoord->line,filecoord->file);
				} else {
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(FILEVAR_COORD_OP,filecoord->line,filecoord->file);
				}
				_NclPutInstr((NclValue)filecoord->filesym,filecoord->line,filecoord->file);
				_NclPutIntInstr(nsubs,filecoord->line,filecoord->file);
				break;
			case Ncl_WRITEIT:
				if(filecoord->subscript_list != NULL) {
					step = filecoord->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(ASSIGN_FILEVAR_COORD_OP,filecoord->line,filecoord->file);
				} else {
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(ASSIGN_FILEVAR_COORD_OP,filecoord->line,filecoord->file);
				}
				_NclPutInstr((NclValue)filecoord->filesym,filecoord->line,filecoord->file);
/*
				_NclPutInstr((NclValue)filecoord->filevar_q,filecoord->line,filecoord->file);
				_NclPutInstr((NclValue)filecoord->coord_name_q,filecoord->line,filecoord->file);
*/
				_NclPutIntInstr(nsubs,filecoord->line,filecoord->file);
				break;
			case Ncl_PARAMIT:
				if(filecoord->subscript_list != NULL) {
					step = filecoord->subscript_list; 
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(PARAM_FILEVAR_COORD_OP,filecoord->line,filecoord->file);
				} else {
					_NclTranslate(filecoord->filevarnode,fp);
					_NclTranslate(filecoord->coordnamenode,fp);
					_NclPutInstr(PARAM_FILEVAR_COORD_OP,filecoord->line,filecoord->file);
				}
				_NclPutInstr((NclValue)filecoord->filesym,filecoord->line,filecoord->file);
/*
				_NclPutInstr((NclValue)filecoord->filevar_q,filecoord->line,filecoord->file);
				_NclPutInstr((NclValue)filecoord->coord_name_q,filecoord->line,filecoord->file);
*/
				_NclPutIntInstr(nsubs,filecoord->line,filecoord->file);
				break;
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}
			break;
		}
		case Ncl_VARCOORDATT:
		{
			NclCoordAtt *coordatt = (NclCoordAtt*)root;
			int nsubs = 0;
			switch(coordatt->ref_type) {
			case Ncl_READIT:
				if(coordatt->subscript_list != NULL) {
					step = coordatt->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coordatt->coordnamenode,fp);
					_NclTranslate(coordatt->attnamenode,fp);
					_NclPutInstr(VAR_COORD_ATT_OP,coordatt->line,coordatt->file);
				} else {
					off1 = _NclTranslate(coordatt->coordnamenode,fp);
					_NclTranslate(coordatt->attnamenode,fp);
					_NclPutInstr(VAR_COORD_ATT_OP,coordatt->line,coordatt->file);
				}
				_NclPutInstr((NclValue)coordatt->sym,coordatt->line,coordatt->file);
/*
				_NclPutInstr((NclValue)coordatt->coord_name_q,coordatt->line,coordatt->file);
				_NclPutInstr((NclValue)coordatt->attname_q,coordatt->line,coordatt->file);
*/
				_NclPutIntInstr(nsubs,coordatt->line,coordatt->file);
				break;
			case Ncl_WRITEIT:
				if(coordatt->subscript_list != NULL) {
					step = coordatt->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coordatt->coordnamenode,fp);
					_NclTranslate(coordatt->attnamenode,fp);
					_NclPutInstr(ASSIGN_VAR_COORD_ATT_OP,coordatt->line,coordatt->file);
				} else {
					off1 = _NclTranslate(coordatt->coordnamenode,fp);
					_NclTranslate(coordatt->attnamenode,fp);
					_NclPutInstr(ASSIGN_VAR_COORD_ATT_OP,coordatt->line,coordatt->file);
				}
				_NclPutInstr((NclValue)coordatt->sym,coordatt->line,coordatt->file);
/*
				_NclPutInstr((NclValue)coordatt->coord_name_q,coordatt->line,coordatt->file);
				_NclPutInstr((NclValue)coordatt->attname_q,coordatt->line,coordatt->file);
*/
				_NclPutIntInstr(nsubs,coordatt->line,coordatt->file);
				break;
			case Ncl_PARAMIT:
				if(coordatt->subscript_list != NULL) {
					step = coordatt->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclPutInstr(PARAM_VAR_COORD_ATT_OP,coordatt->line,coordatt->file);
				} else {
					off1 = _NclTranslate(coordatt->coordnamenode,fp);
					_NclTranslate(coordatt->attnamenode,fp);
					_NclPutInstr(PARAM_VAR_COORD_ATT_OP,coordatt->line,coordatt->file);
				}
				_NclPutInstr((NclValue)coordatt->sym,coordatt->line,coordatt->file);
/*
				_NclPutInstr((NclValue)coordatt->coord_name_q,coordatt->line,coordatt->file);
				_NclPutInstr((NclValue)coordatt->attname_q,coordatt->line,coordatt->file);
*/
				_NclPutIntInstr(nsubs,coordatt->line,coordatt->file);
				break;
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}
			break;
		}
		case Ncl_VARCOORD:
		{
			NclCoord *coord = (NclCoord*)root;
			int nsubs = 0;
			switch(coord->ref_type) {
			case Ncl_VALONLY:
				if(coord->subscript_list != NULL) {
					step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(VARVAL_COORD_OP,coord->line,coord->file);
				} else {
					off1 = _NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(VARVAL_COORD_OP,coord->line,coord->file);
				}
				_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
				_NclPutIntInstr(nsubs,coord->line,coord->file);
				break;
			case Ncl_READIT:
				if(coord->subscript_list != NULL) {
					step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
				} else {
					off1 = _NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
				}
				_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
				_NclPutIntInstr(nsubs,coord->line,coord->file);
				break;
			case Ncl_WRITEIT:
				if(coord->subscript_list != NULL) {
					step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(ASSIGN_VAR_COORD_OP,coord->line,coord->file);
				} else {
					off1 = _NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(ASSIGN_VAR_COORD_OP,coord->line,coord->file);
				}
				_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
/*
				_NclPutInstr((NclValue)coord->coord_name_q,coord->line,coord->file);
*/
				_NclPutIntInstr(nsubs,coord->line,coord->file);
				break;
			case Ncl_PARAMIT:
				if(coord->subscript_list != NULL) {
					step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(PARAM_VAR_COORD_OP,coord->line,coord->file);
				} else {
					off1 = _NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(PARAM_VAR_COORD_OP,coord->line,coord->file);
				}
				_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
/*
				_NclPutInstr((NclValue)coord->coord_name_q,coord->line,coord->file);
*/
				_NclPutIntInstr(nsubs,coord->line,coord->file);
				break;
			case Ncl_REWRITEIT:
				if(coord->subscript_list != NULL) {
					step = coord->subscript_list; off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						nsubs++;
						step = step->next;
					}
					_NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(REASSIGN_VAR_COORD_OP,coord->line,coord->file);
				} else {
					off1 = _NclTranslate(coord->coordnamenode,fp);
					_NclPutInstr(REASSIGN_VAR_COORD_OP,coord->line,coord->file);
				}

				_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
				_NclPutIntInstr(nsubs,coord->line,coord->file);

				break;
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}
			break;
		}
		case Ncl_FILEVAR:
		{
			NclFileVar *filevar = (NclFileVar*)root;
			int nsubs = 0;
		
			off1 = _NclPutInstr(ISDEFINED_OP,filevar->line,filevar->file);
			_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
			switch(filevar->ref_type) {
			case Ncl_VALONLY:
				if(filevar->subscript_list != NULL) {
					step = filevar->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(FILE_VARVAL_OP,filevar->line,filevar->file);
				} else {
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(FILE_VARVAL_OP,filevar->line,filevar->file);
				}
				_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
				_NclPutIntInstr(nsubs,filevar->line,filevar->file);
				break;
			case Ncl_READIT:	
				if(filevar->subscript_list != NULL) {
					step = filevar->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(FILE_VAR_OP,filevar->line,filevar->file);
				} else {
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(FILE_VAR_OP,filevar->line,filevar->file);
				}
				_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
				_NclPutIntInstr(nsubs,filevar->line,filevar->file);
				break;
			case Ncl_WRITEIT:
				if(filevar->subscript_list != NULL) {
					step = filevar->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(ASSIGN_FILE_VAR_OP,filevar->line,filevar->file);
				} else {
					_NclTranslate(filevar->filevarnode,fp);
					 _NclPutInstr(ASSIGN_FILE_VAR_OP,filevar->line,filevar->file);
				}
				_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
/*
				_NclPutInstr((NclValue)filevar->filevar_q,filevar->line,filevar->file);
*/
				_NclPutIntInstr(nsubs,filevar->line,filevar->file);
				break;
			case Ncl_PARAMIT:
				if(filevar->subscript_list != NULL) {
					step = filevar->subscript_list;
					_NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(PARAM_FILE_VAR_OP,filevar->line,filevar->file);
				} else {
					_NclTranslate(filevar->filevarnode,fp);
					_NclPutInstr(PARAM_FILE_VAR_OP,filevar->line,filevar->file);
				}
				_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
/*
				_NclPutInstr((NclValue)filevar->filevar_q,filevar->line,filevar->file);
*/
					_NclPutIntInstr(nsubs,filevar->line,filevar->file);
				break;
                        default:
                                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                                return (NhlFATAL);
			}
			break;
		}
		case Ncl_BREAK:
		{
			off1 = _NclPutInstr(JMP,groot->line,groot->file);
			off2 = _NclPutInstr(NOOP,groot->line,groot->file);
			_NclPushBreak(off2,groot->line,groot->file);
			break;
		}
		case Ncl_CONTINUE:
		{
			off1 = _NclPutInstr(JMP,groot->line,groot->file);
			off2 = _NclPutInstr(NOOP,groot->line,groot->file);
			_NclPushContinue(off2,groot->line,groot->file);
			break;
		}
		case Ncl_NULLNODE:
		{
			off1 = _NclPutInstr(PUSHNULL,groot->line,groot->file);
			break;
		}
		case Ncl_EXPRNEW:
		{
			NclExprNew *new_op = (NclExprNew*)groot;
			off1 =_NclTranslate(new_op->data_type_expr,fp);
			_NclTranslate(new_op->size_expr,fp);
			if(new_op->missing_expr != NULL) {
				_NclTranslate(new_op->missing_expr,fp);
				_NclPutInstr(NEW_WM_OP,new_op->line,new_op->file);
				_NclPutInstr((NclValue)NULL,new_op->line,new_op->file);
			}  else {
				_NclPutInstr(NEW_OP,new_op->line,new_op->file);
				_NclPutInstr((NclValue)NULL,new_op->line,new_op->file);
			}
			break;
		}
		case Ncl_NEW:
		{
			NclNew *new_op = (NclNew*)groot;
			off1 = _NclTranslate(new_op->size_expr,fp);
			if(new_op->missing_expr != NULL) {
				_NclTranslate(new_op->missing_expr,fp);
				_NclPutInstr(NEW_WM_OP,new_op->line,new_op->file);
				_NclPutInstr((NclValue)new_op->data_sym,new_op->line,new_op->file);
			}  else {
				_NclPutInstr(NEW_OP,new_op->line,new_op->file);
				_NclPutInstr((NclValue)new_op->data_sym,new_op->line,new_op->file);
			}
			break;
		}
		case Ncl_LIST:
		{
			NclList *list_op = (NclList*)groot;

			off1 = _NclPutInstr(ISDEFINED_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->sym,list_op->line,list_op->file);
			if(list_op->subscript_list != NULL) {
				(void)_NclTranslate(list_op->subscript_list,fp);
				if(list_op->ref_type == Ncl_WRITEIT) {
					((NclGenericRefNode*)list_op->ref_node)->ref_type = Ncl_WRITEIT;
					_NclPutInstr(LIST_ASSIGN_VERIFY_SUB,list_op->line,list_op->file);
				}
			}

/*
* List object hold
* Creates a tmp List variable to be used to sequence through list object
* Separation of list variable is need to support multiple list reference on same
* line. Another reason is lists can be pushed on to stack because that would
* cause problems with array operations.
*/

			_NclPutInstr(LIST_READ_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->sym,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->tmp,list_op->line,list_op->file);
			_NclPutIntInstr(list_op->subscript_list?1:0,list_op->line,list_op->file);
/*
* Starts sequesting of list through tmp_var reference. retrieves next element and then
* creates equivalence symbol table entry with the symbol list_op->tmp. This enables
* anything int the list_op->ref_node to work without changes. The retrieve record
* will return whatever is equivalent with to "tmp". Stack is NOT affected.
*/
			off2 = _NclPutInstr(SET_NEXT_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->tmp,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->tmp_var,list_op->line,list_op->file);
			off3 = _NclPutInstr(NOOP,list_op->line,list_op->file);
			
/*
* Actual operation to perform on each list element. Idn's in ref_node are symbols pointed
* to by "tmp"
*/
			(void)_NclTranslate(list_op->ref_node,fp);
			_NclPutInstr(LIST_CLEAR_TMP_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->tmp_var,list_op->line,list_op->file);
			

			_NclPutInstr(JMP,list_op->line,list_op->file);
			_NclPutInstr(off2,list_op->line,list_op->file);
/*
* terminates list operation. All elements on stack are popped and joined either
* using ARRAY_LIST_OP or concatenated. Temporary is freed
*/
			off5 =_NclPutInstr(TERM_LIST_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->tmp,list_op->line,list_op->file);
			_NclPutInstrAt(off3,off5,list_op->line,list_op->file);
			break;
		}
		case Ncl_FILEVARLIST:
		{
			NclFileVarList *list_op = (NclFileVarList*)groot;
			int nsubs = 0;

			off1 = _NclPutInstr(ISDEFINED_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->list,list_op->line,list_op->file);
			if(list_op->filevar_subscript != NULL) {
				step = list_op->filevar_subscript;
				_NclTranslate(step->node,fp);
				step = step->next;
				nsubs = 1;
				while(step != NULL) {
					(void)_NclTranslate(step->node,fp);
					step = step->next;
					nsubs++;
				}
			}
			_NclTranslate(list_op->filevar,fp);
			if(list_op->list_subscript != NULL) {
				(void)_NclTranslate(list_op->list_subscript,fp);
			}
			_NclPutInstr(LIST_READ_FILEVAR_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->list,list_op->line,list_op->file);
			_NclPutIntInstr(list_op->list_subscript?1:0,list_op->line,list_op->file);
			_NclPutIntInstr(nsubs,list_op->line,list_op->file);
			break;
		}
		case Ncl_FILEGROUP:
		{
			NclFileGroup *filegroup = (NclFileGroup *)root;
			int nsubs = 0;
		
			off1 = _NclPutInstr(ISDEFINED_OP,filegroup->line,filegroup->file);
			_NclPutInstr((NclValue)filegroup->dfile,filegroup->line,filegroup->file);

			switch(filegroup->ref_type) {
			case Ncl_VALONLY:
				_NclTranslate(filegroup->filegroupnode,fp);
				_NclPutInstr(FILE_GROUPVAL_OP,filegroup->line,filegroup->file);
				_NclPutInstr((NclValue)filegroup->dfile,filegroup->line,filegroup->file);
				_NclPutIntInstr(nsubs,filegroup->line,filegroup->file);
				break;
			case Ncl_READIT:	
				_NclTranslate(filegroup->filegroupnode,fp);
				_NclPutInstr(FILE_GROUP_OP,filegroup->line,filegroup->file);
				_NclPutInstr((NclValue)filegroup->dfile,filegroup->line,filegroup->file);
				_NclPutIntInstr(nsubs,filegroup->line,filegroup->file);
				break;
			case Ncl_WRITEIT:
				_NclTranslate(filegroup->filegroupnode,fp);
				_NclPutInstr(ASSIGN_FILE_GROUP_OP,filegroup->line,filegroup->file);
				_NclPutInstr((NclValue)filegroup->dfile,filegroup->line,filegroup->file);
				_NclPutIntInstr(nsubs,filegroup->line,filegroup->file);
				break;
			case Ncl_REWRITEIT:
#if 1
                                fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__);
                                fprintf(stderr, "\tReassigning to group is not supported in NCL.\n");
				exit (-1);
#else
				_NclTranslate(filegroup->filegroupnode,fp);
				_NclPutInstr(ASSIGN_FILE_GROUP_OP,filegroup->line,filegroup->file);
				_NclPutInstr((NclValue)filegroup->dfile,filegroup->line,filegroup->file);
				_NclPutIntInstr(nsubs,filegroup->line,filegroup->file);
#endif
				break;
			case Ncl_PARAMIT:
				_NclTranslate(filegroup->filegroupnode,fp);
				_NclPutInstr(PARAM_FILE_GROUP_OP,filegroup->line,filegroup->file);
				_NclPutInstr((NclValue)filegroup->dfile,filegroup->line,filegroup->file);
				_NclPutIntInstr(nsubs,filegroup->line,filegroup->file);
				break;
			}
			break;
		}
		case Ncl_FILEGROUPLIST:
		{
			NclFileVarList *list_op = (NclFileVarList*)groot;
			int nsubs = 0;

			off1 = _NclPutInstr(ISDEFINED_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->list,list_op->line,list_op->file);
			if(list_op->filevar_subscript != NULL) {
				step = list_op->filevar_subscript;
				_NclTranslate(step->node,fp);
				step = step->next;
				nsubs = 1;
				while(step != NULL) {
					(void)_NclTranslate(step->node,fp);
					step = step->next;
					nsubs++;
				}
			}
			_NclTranslate(list_op->filevar,fp);
			if(list_op->list_subscript != NULL) {
				(void)_NclTranslate(list_op->list_subscript,fp);
			}
			_NclPutInstr(LIST_READ_FILEVAR_OP,list_op->line,list_op->file);
			_NclPutInstr((NclValue)list_op->list,list_op->line,list_op->file);
			_NclPutIntInstr(list_op->list_subscript?1:0,list_op->line,list_op->file);
			_NclPutIntInstr(nsubs,list_op->line,list_op->file);
			break;
		}
		default:
			fprintf(stdout, "\n\nfile: %s, line: %d\n", __FILE__, __LINE__);
			fprintf(stdout,"\tgroot->name = %s\n", groot->name);
			fprintf(stdout,"\tgroot->file = %s\n", groot->file);
			fprintf(stdout,"\tgroot->line = %d\n", groot->line);
			fprintf(stdout,"\tgroot->kind = %d\n", groot->kind);
			break;
	}
	nesting--;
	if(nesting == 0) {
		_NclPutInstr(ENDSTMNT_OP,groot->line,groot->file);
	}
/*
* -----------> need to do something when off1 isn't set. This probably 
* can happen with empty blocks
*/
	return(off1);
} else {
	fprintf(fp,"ERROR NULL NODE FOUND!\n");
	return(-1);
}
}
#ifdef __cplusplus
}
#endif

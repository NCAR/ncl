
#include <ncarg/hlu/hlu.h>
#include <defs.h>
#include <Symbol.h>
#include <SrcTree.h>
#include <errno.h>
#include <y.tab.h>
#include <OpsList.h>
#include <Machine.h>

extern char *cur_load_file;
extern int loading;
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
#if __STDC__
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
			}
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;
			}	
		break;
		}
		case Ncl_RETURN:
		{
			NclReturn *ret = (NclReturn*)root;
			
			(void)_NclTranslate(ret->expr,fp);
/*
* All that is needed is the return op to tell machine that top of
* stack should be placed in frames return_value field
*/
			off1 = _NclPutInstr(RETURN_OP,ret->line,ret->file);
		break;
		}
		case Ncl_IFTHEN:
		{
			NclIfThen *ifthen = (NclIfThen*)root;

			off1 = _NclTranslate(ifthen->cond_expr,fp);
			_NclPutInstr(IF_OP,ifthen->line,ifthen->file);
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
			_NclPutInstr(IF_OP,ifthenelse->line,ifthenelse->file);
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
			_NclPutInstrAt(off2,off4,ifthenelse->line,ifthenelse->file);
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
			_NclTranslate(vblk->objname,fp);
			_NclTranslate(vblk->objtype,fp);
			_NclPutInstr(CREATE_OBJ_OP,vblk->line,vblk->file);
			_NclPutInstr((NclValue)nres,vblk->line,vblk->file);
			break;
		}
		case Ncl_VISBLKSET:
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
			_NclTranslate(vblk->objname,fp);
			_NclPutInstr(SET_OBJ_OP,vblk->line,vblk->file);
			_NclPutInstr((NclValue)nres,vblk->line,vblk->file);
			break;
		}
		case Ncl_VISBLKGET:
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
			_NclTranslate(vblk->objname,fp);
			_NclPutInstr(GET_OBJ_OP,vblk->line,vblk->file);
			_NclPutInstr((NclValue)nres,vblk->line,vblk->file);
			break;
		}
		case Ncl_RESOURCE:
		{
			NclResource *resource = (NclResource*)root;
			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,resource->line,resource->file);
			_NclPutInstr((NclValue)resource->res_name,resource->line,resource->file);
			_NclTranslate(resource->expr,fp);
			break;
		}
		case Ncl_GETRESOURCE:
		{
			NclGetResource *resource = (NclGetResource*)root;
			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,resource->line,resource->file);
			_NclPutInstr((NclValue)resource->res_name,resource->line,resource->file);
			_NclPutInstr(PUSH_VAR_OP,resource->line,resource->file);
			_NclPutInstr((NclValue)resource->var,resource->line,resource->file);
			break;
		}
		case Ncl_DOFROMTO:
		{
			NclDoFromTo *dofromto = (NclDoFromTo*)root;

			off1 = _NclTranslate(dofromto->start_expr,fp);
			_NclTranslate(dofromto->end_expr,fp);
			_NclTranslate(dofromto->inc_var,fp);
			_NclPutInstr(DO_FROM_TO_OP,dofromto->line,dofromto->file);
			off2 = _NclPutInstr(NOOP,dofromto->line,dofromto->file);
			_NclPutInstr(JMP,dofromto->line,dofromto->file);
			off3 = _NclPutInstr(NOOP,dofromto->line,dofromto->file);
			step = dofromto->block_stmnt_list;
			if(step != NULL) {
				off4 = _NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstrAt(off2,off4,dofromto->line,dofromto->file);
			while(step != NULL) {
				(void) _NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstr(STOPSEQ,dofromto->line,dofromto->file);
			_NclPutInstrAt(off3, _NclGetCurrentOffset(),dofromto->line,dofromto->file);
			break;
		}
		case Ncl_DOFROMTOSTRIDE:
		{
			NclDoFromToStride *dofromtostride = 
						(NclDoFromToStride*) root;	
			off1 = _NclTranslate(dofromtostride->start_expr,fp);
			_NclTranslate(dofromtostride->end_expr,fp);
			_NclTranslate(dofromtostride->stride_expr,fp);
			_NclTranslate(dofromtostride->inc_var,fp);
			_NclPutInstr(DO_FROM_TO_STRIDE_OP,dofromtostride->line,dofromtostride->file);
			off2 = _NclPutInstr(NOOP,dofromtostride->line,dofromtostride->file);
			_NclPutInstr(JMP,dofromtostride->line,dofromtostride->file);
			off3 = _NclPutInstr(NOOP,dofromtostride->line,dofromtostride->file);
			step = dofromtostride->block_stmnt_list;
			if(step != NULL) {
				off4 = _NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstrAt(off2,off4,dofromtostride->line,dofromtostride->file);
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstr(STOPSEQ,dofromtostride->line,dofromtostride->file);
			_NclPutInstrAt(off3,_NclGetCurrentOffset(),dofromtostride->line,dofromtostride->file);
			break;
		}
		case Ncl_ASSIGN:
		{
			NclAssign *assign = (NclAssign*)root;

			off1 = _NclTranslate(assign->right_side,fp);
			_NclTranslate(assign->left_side,fp);
			_NclPutInstr(ASSIGN_OP,assign->line,assign->file);
			break;
		}
		case Ncl_INTSUBSCRIPT:	
		{
			NclSubscript *subscript = (NclSubscript*)
					root;
			if(subscript->dimname != NULL) {
				off1 = _NclPutInstr(PUSH_STRING_LIT_OP,subscript->line,subscript->file);
				_NclPutInstr((NclValue)subscript->dimname,subscript->line,subscript->file);
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
			if(subscript->dimname != NULL) {
				off1 = _NclPutInstr(PUSH_STRING_LIT_OP,subscript->line,subscript->file);
				_NclPutInstr((NclValue)subscript->dimname,subscript->line,subscript->file);
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
			
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MOD_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_OREXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(OR_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_ANDEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(AND_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_XOREXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(XOR_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_LTSELECTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(LTSEL_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_GTSELECTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(GTSEL_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_PLUSEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(PLUS_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_MINUSEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MINUS_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_MULEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MUL_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_MATMULEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MAT_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_DIVEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(DIV_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_EXPEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(EXP_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_LEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(LE_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_GEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(GE_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_GTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(GT_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_LTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(LT_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_EQEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(EQ_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_NEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(NE_OP,dualexpr->line,dualexpr->file);
			break;
		}
		case Ncl_REAL:
		{
			NclReal *real = (NclReal*)root;
			
			off1 = _NclPutInstr(PUSH_REAL_LIT_OP,real->line,real->file);
			_NclPutRealInstr(real->real,real->line,real->file);
			break;
		}
		case Ncl_INT:
		{
			NclInt *integer = (NclInt*)root;
			off1 = _NclPutInstr(PUSH_INT_LIT_OP,integer->line,integer->file);
			_NclPutInstr((NclValue)integer->integer,integer->line,integer->file);
			break;
		}
		case Ncl_STRING:
		{
			NclString *string= (NclString*)root;
			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,string->line,string->file);
			_NclPutInstr((NclValue)string->string,string->line,string->file);
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
			break;
		}
		case Ncl_EXTERNPROCDEF:
		{
			NclExternProcDef *externprocdef = (NclExternProcDef*)
						root;
			off1 = _NclPutInstr(NOOP,externprocdef->line,externprocdef->file);
			break;
		}
		case Ncl_BUILTINPROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			step = proccall->arg_list;
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);
				step=step->next;
			}
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step= step->next;
			}
/*
* Checks types of arguments and takes care of coercion if needed
*/
			_NclPutInstr(BPROC_CALL_OP,proccall->line,proccall->file);
			_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);
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
			}
/*
* The PROC_CALL_OP first visits each argument on the stack and 
* performs type and dimension checks based on template located
* in the symbol table
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
* instruction.
*
*/
			_NclPutInstr(PROC_CALL_OP,proccall->line,proccall->file);
			_NclPutInstr((NclValue)proccall->proc,proccall->line,proccall->file);

			_NclPutInstrAt(off2,_NclGetCurrentOffset(),proccall->line,proccall->file);
			
			break;
		}
		case Ncl_BUILTINFUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;
			step = funccall->arg_list;
			if(step != NULL) {
				off1 = _NclTranslate(step->node,fp);
				step=step->next;
			}
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step= step->next;
			}
/*
* Checks types of arguments and takes care of coercion if needed
*/
			_NclPutInstr(BFUNC_CALL_OP,funccall->line,funccall->file);
			_NclPutInstr((NclValue)funccall->func,funccall->line,funccall->file);
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
		case Ncl_DOWHILE:
		{
			NclDoWhile *dowhilel = (NclDoWhile*)root;
			off1 = _NclPutInstr(DO_WHILE_OP,dowhilel->line,dowhilel->file);
			off2 = _NclPutInstr(NOOP,dowhilel->line,dowhilel->file);
			off3 = _NclPutInstr(NOOP,dowhilel->line,dowhilel->file);
			_NclPutInstr(JMP,dowhilel->line,dowhilel->file);
			off4 = _NclPutInstr(NOOP,dowhilel->line,dowhilel->file);	
			_NclPutInstrAt(off2,_NclTranslate(dowhilel->cond_expr,fp),dowhilel->line,dowhilel->file);
			_NclPutInstr(STOPSEQ,dowhilel->line,dowhilel->file);
			step = dowhilel->stmnts;
			if(step != NULL) {
				off5 = _NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstrAt(off3,off5,dowhilel->line,dowhilel->file);
			while(step != NULL) {
				(void)_NclTranslate(step->node,fp);
				step = step->next;
			}
			_NclPutInstr(STOPSEQ,dowhilel->line,dowhilel->file);
			_NclPutInstrAt(off4,_NclGetCurrentOffset(),dowhilel->line,dowhilel->file);
			break;
		}
		case Ncl_VAR:
		{
			NclVar *var = (NclVar*)root;
			int nsubs = 0;
	
			switch(var->ref_type) {	
			case Ncl_READIT:
				if(var->subscript_list != NULL) {
					step = var->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(SUBSCRIPTED_VAR_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					_NclPutInstr((NclValue)nsubs,var->line,var->file);
				} else {
					off1= _NclPutInstr(PUSH_VAR_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
				}
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:	
				if(var->subscript_list != NULL) {
					step = var->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(WRITE_SUBSCRIPTED_VAR_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
					_NclPutInstr((NclValue)nsubs,var->line,var->file);
				} else {
					off1= _NclPutInstr(WRITE_PUSH_VAR_OP,var->line,var->file);
					_NclPutInstr((NclValue)var->sym,var->line,var->file);
				}
				break;
			}
			break;
		}
		case Ncl_VARDIMNUM:
		{
			NclVarDim *vardim = (NclVarDim*)root;

			switch(vardim->ref_type) {
			case Ncl_READIT:
				off1 = _NclPutInstr(VAR_DIMNUM_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->u.dimnum,vardim->line,vardim->file);
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				off1 = _NclPutInstr(WRITE_VAR_DIMNUM_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->u.dimnum,vardim->line,vardim->file);
				break;
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
					_NclPutInstr(SUBSCRIPTED_VARATT_OP,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->attname,varatt->line,varatt->file);
					_NclPutInstr((NclValue)nsubs,varatt->line,varatt->file);
				} else {
					off1 = _NclPutInstr(VARATT_OP,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->attname,varatt->line,varatt->file);
				}
				break;
			case Ncl_WRITEIT:
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
					_NclPutInstr(WRITE_SUBSCRIPTED_VARATT_OP,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->attname,varatt->line,varatt->file);
					_NclPutInstr((NclValue)nsubs,varatt->line,varatt->file);
				} else {
					off1 = _NclPutInstr(WRITE_VARATT_OP,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->sym,varatt->line,varatt->file);
					_NclPutInstr((NclValue)varatt->attname,varatt->line,varatt->file);
				}
				break;	
			}
			break;
		}
		case Ncl_VARDIMNAME:
		{
			NclVarDim *vardim = (NclVarDim*)root;

			switch(vardim->ref_type) {
			case Ncl_READIT:
				off1 = _NclPutInstr(VAR_DIMNAME_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->u.dimname,vardim->line,vardim->file);
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				off1 = _NclPutInstr(WRITE_VAR_DIMNAME_OP,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
				_NclPutInstr((NclValue)vardim->u.dimname,vardim->line,vardim->file);
				break;
			}
			break;
		}
		case Ncl_VARCOORD:
		{
			NclCoord *coord = (NclCoord*)root;
			int nsubs = 0;
			switch(coord->ref_type) {
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
					_NclPutInstr(SUBSCRIPTED_VAR_COORD_OP,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->coord_name,coord->line,coord->file);
					_NclPutInstr((NclValue)nsubs,coord->line,coord->file);
				} else {
					off1 = _NclPutInstr(VAR_COORD_OP,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->coord_name,coord->line,coord->file);
			
				}
				break;
			case Ncl_WRITEIT:
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
					_NclPutInstr(WRITE_SUBSCRIPTED_VAR_COORD_OP,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->coord_name,coord->line,coord->file);
					_NclPutInstr((NclValue)nsubs,coord->line,coord->file);
				} else {
					off1 = _NclPutInstr(WRITE_VAR_COORD_OP,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->sym,coord->line,coord->file);
					_NclPutInstr((NclValue)coord->coord_name,coord->line,coord->file);
			
				}
				break;
			}
			break;
		}
		case Ncl_FILE:
		{
			NclFile *file = (NclFile*)root;
		
			switch(file->ref_type) {
			case Ncl_READIT:	
				off1 = _NclPutInstr(PUSH_FILE_OP,file->line,file->file);
				_NclPutInstr((NclValue)file->dfile,file->line,file->file);
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				off1 = _NclPutInstr(WRITE_PUSH_FILE_OP,file->line,file->file);
				_NclPutInstr((NclValue)file->dfile,file->line,file->file);
				break;
			}
			break;
		}
		case Ncl_FILEVAR:
		{
			NclFileVar *filevar = (NclFileVar*)root;
			int nsubs = 0;
		
			switch(filevar->ref_type) {
			case Ncl_READIT:	
				if(filevar->subscript_list != NULL) {
					step = filevar->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(SUBSCRIPTED_FILE_VAR_OP,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->filevar,filevar->line,filevar->file);
					_NclPutInstr((NclValue)nsubs,filevar->line,filevar->file);
				} else {
					off1= _NclPutInstr(PUSH_FILE_VAR_OP,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->filevar,filevar->line,filevar->file);
				}
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				if(filevar->subscript_list != NULL) {
					step = filevar->subscript_list;
					off1 = _NclTranslate(step->node,fp);
					step = step->next;
					nsubs = 1;
					while(step != NULL) {
						(void)_NclTranslate(step->node,fp);
						step = step->next;
						nsubs++;
					}
					_NclPutInstr(WRITE_SUBSCRIPTED_FILE_VAR_OP,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->filevar,filevar->line,filevar->file);
					_NclPutInstr((NclValue)nsubs,filevar->line,filevar->file);
				} else {
					off1= _NclPutInstr(WRITE_PUSH_FILE_VAR_OP,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->dfile,filevar->line,filevar->file);
					_NclPutInstr((NclValue)filevar->filevar,filevar->line,filevar->file);
				}
				break;
			}
			break;
		}
		case Ncl_FILEDIMNUM:
		{
			NclFileDim *filedim = (NclFileDim*)root;
			
			switch(filedim->ref_type) {
			case Ncl_READIT:	
				off1 = _NclPutInstr(FILE_DIMNUM_OP,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->dfile,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->u.dimnum,filedim->line,filedim->file);
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				off1 = _NclPutInstr(WRITE_FILE_DIMNUM_OP,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->dfile,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->u.dimnum,filedim->line,filedim->file);
				break;
			}
			break;
		}
		case Ncl_FILEDIMNAME:
		{
			NclFileDim *filedim = (NclFileDim*)root;
			switch(filedim->ref_type) {
			case Ncl_READIT:	
				off1 = _NclPutInstr(FILE_DIMNAME_OP,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->dfile,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->u.dimname,filedim->line,filedim->file);
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				off1 = _NclPutInstr(WRITE_FILE_DIMNAME_OP,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->dfile,filedim->line,filedim->file);
				_NclPutInstr((NclValue)filedim->u.dimname,filedim->line,filedim->file);
				break;
			}
			break;
		}
		case Ncl_FILEATT:
		{
			NclFileAtt *fileatt = (NclFileAtt*) root;
			int nsubs = 0;
			switch(fileatt->ref_type) {
			case Ncl_READIT:	
				if(fileatt->subscript_list == NULL) {
					off1 = _NclPutInstr(FILE_ATT_OP,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->dfile,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->attname,fileatt->line,fileatt->file);
				} else {
					off1 = _NclPutInstr(SUBSCRIPTED_FILE_ATT_OP,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->dfile,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->attname,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)nsubs,fileatt->line,fileatt->file);
				}
				break;
			case Ncl_WRITEIT:
			case Ncl_PARAMIT:
				if(fileatt->subscript_list == NULL) {
					off1 = _NclPutInstr(WRITE_FILE_ATT_OP,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->dfile,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->attname,fileatt->line,fileatt->file);
				} else {
					off1 = _NclPutInstr(WRITE_SUBSCRIPTED_FILE_ATT_OP,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->dfile,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)fileatt->attname,fileatt->line,fileatt->file);
					_NclPutInstr((NclValue)nsubs,fileatt->line,fileatt->file);
				}
				break;
			}
			break;
		}
		case Ncl_BREAK:
		{
			off1 = _NclPutInstr(BREAK_OP,groot->line,NULL);
			break;
		}
		case Ncl_CONTINUE:
		{
			off1 = _NclPutInstr(CONTINUE_OP,groot->line,NULL);
			break;
		}
		default:
		
		fprintf(fp,"UNRECOGNIZED ENUM VALUE!\n");
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

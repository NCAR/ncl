
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
	NclStackEntry	data;
	static int i = 0;
	int off1 = -1 ,off2 = -1 ,off3 = -1, off4 = -1 ,off5 = -1;
	int tmpline;
	char* tmpfile;

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
			return(off1);
		}
		break;
		case Ncl_RETURN:
		{
			NclReturn *ret = (NclReturn*)root;
			
			(void)_NclTranslate(ret->expr,fp);
/*
* All that is needed is the return op to tell machine that top of
* stack should be placed in frames return_value field
*/
			off1 = _NclPutInstr(RETURN_OP,ret->line,ret->file);
			return(off1);
		}
			break;
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

			return(off1);
		}
			break;
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
			return(off1);
	
		}
			break;
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
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
		case Ncl_RESOURCE:
		{
			NclResource *resource = (NclResource*)root;
			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,resource->line,resource->file);
			_NclPutInstr((NclValue)resource->res_name,resource->line,resource->file);
			_NclTranslate(resource->expr,fp);
			return(off1);
		}
			break;
		case Ncl_GETRESOURCE:
		{
			NclGetResource *resource = (NclGetResource*)root;
			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,resource->line,resource->file);
			_NclPutInstr((NclValue)resource->res_name,resource->line,resource->file);
			_NclPutInstr(PUSH_VAR_OP,resource->line,resource->file);
			_NclPutInstr((NclValue)resource->var,resource->line,resource->file);
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
		case Ncl_BUILTINPROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			return(-1);
		}
			break;
		case Ncl_EXTERNALPROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			return(-1);
		}
			break;
		case Ncl_FUNCDEF:
		{
			NclFuncDef *funcdef = (NclFuncDef*)root;
			return(-1);
			
		}	
			break;
		case Ncl_EXTERNFUNCDEF:
		{
			NclExternFuncDef *externfuncdef = (NclExternFuncDef*)
							 root;
			return(-1);
		}
			break;
		case Ncl_LOCALVARDEC:
		{
			NclLocalVarDec *localvardec = (NclLocalVarDec*)root;
			return(-1);
		}
			break;
		case Ncl_DIMSIZELISTNODE:
		{
			NclDimSizeListNode *dimsizelistnode = 
						(NclDimSizeListNode*)root;
			return(-1);
		}
			break;
		case Ncl_PROCDEF:
		{
			NclProcDef * procdef = (NclProcDef*)root;
			return(-1);
		}
			break;
		case Ncl_EXTERNPROCDEF:
		{
			NclExternProcDef *externprocdef = (NclExternProcDef*)
						root;
			return(-1);
		}
			break;
		case Ncl_ASSIGN:
		{
			NclAssign *assign = (NclAssign*)root;

			off1 = _NclTranslate(assign->right_side,fp);
			_NclTranslate(assign->left_side,fp);
			_NclPutInstr(ASSIGN_OP,assign->line,assign->file);
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
		case Ncl_SINGLEINDEX:
		{
			NclSingleIndex *singleindex = (NclSingleIndex*)root;
			off1 = _NclTranslate(singleindex->expr,fp);
			_NclPutInstr(SINGLE_INDEX_OP,singleindex->line,singleindex->file);
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
		case Ncl_IDNEXPR:
		{
			NclIdnExpr *idnexpr = (NclIdnExpr*)root;
			int nsubs = 0;
			off1 = _NclTranslate(idnexpr->idn_ref_node,fp);
			return(off1);
		}
		case Ncl_NEGEXPR:
		{
			NclMonoExpr *monoexpr = (NclMonoExpr*) root;
			
			off1 = _NclTranslate(monoexpr->expr,fp);
			_NclPutInstr(NEG_OP,monoexpr->line,monoexpr->file);
			return(off1);
		}
			break;
		case Ncl_NOTEXPR:
		{
			NclMonoExpr *monoexpr = (NclMonoExpr*) root;
			off1 = _NclTranslate(monoexpr->expr,fp);
			_NclPutInstr(NOT_OP,monoexpr->line,monoexpr->file);
			return(off1);
		}
			break;
		case Ncl_MODEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MOD_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_OREXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(OR_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_ANDEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(AND_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_XOREXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(XOR_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_LTSELECTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(LTSEL_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_GTSELECTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(GTSEL_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_PLUSEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(PLUS_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_MINUSEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MINUS_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_MULEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MUL_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_MATMULEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(MAT_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_DIVEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(DIV_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_EXPEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(EXP_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_LEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(LE_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_GEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(GE_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_GTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(GT_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_LTEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(LT_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_EQEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(EQ_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_NEEXPR:
		{
			NclDualExpr *dualexpr = (NclDualExpr*)root;
			off1 = _NclTranslate(dualexpr->right_expr,fp);
			_NclTranslate(dualexpr->left_expr,fp);
			_NclPutInstr(NE_OP,dualexpr->line,dualexpr->file);
			return(off1);
		}
			break;
		case Ncl_REAL:
		{
			NclReal *real = (NclReal*)root;
			
			off1 = _NclPutInstr(PUSH_REAL_LIT_OP,real->line,real->file);
			_NclPutRealInstr(real->real,real->line,real->file);
			return(off1);
		}
			break;
		case Ncl_INT:
		{
			NclInt *integer = (NclInt*)root;
			off1 = _NclPutInstr(PUSH_INT_LIT_OP,integer->line,integer->file);
			_NclPutInstr((NclValue)integer->integer,integer->line,integer->file);
			return(off1);
		}
			break;
		case Ncl_STRING:
		{
			NclString *string= (NclString*)root;
			off1 = _NclPutInstr(PUSH_STRING_LIT_OP,string->line,string->file);
			_NclPutInstr((NclValue)string->string,string->line,string->file);
			return(off1);
		}
			break;
		case Ncl_BUILTINFUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;
			
			return(-1);
		}
			break;
		case Ncl_EXTERNFUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;
			
			return(-1);
		}
			break;
		case Ncl_FUNCCALL:
		{	
			NclFuncCall *funccall = (NclFuncCall*)root;
			
			return(off1);
		}
			break;
		case Ncl_PROCCALL:
		{	
			NclProcCall *proccall = (NclProcCall*)root;
			

			return(-1);
		}
			break;
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
			return(off1);
		}
			break;
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
			return(off1);
		}
			break;
		case Ncl_VAR:
		{
			NclVar *var = (NclVar*)root;
			int nsubs = 0;
		
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
			return(off1);
		}
			break;
		case Ncl_VARDIMNUM:
		{
			NclVarDim *vardim = (NclVarDim*)root;

			off1 = _NclPutInstr(VAR_DIMNUM_OP,vardim->line,vardim->file);
			_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
			_NclPutInstr((NclValue)vardim->u.dimnum,vardim->line,vardim->file);
			
			return(off1);
		}
			break;
		case Ncl_VARATT:
		{
			NclVarAtt *varatt = (NclVarAtt*)root;
			int nsubs = 0;
			if(varatt->subscript_list != NULL) {
				step == varatt->subscript_list;
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
			
			return(off1);
		}
			break;
		case Ncl_VARDIMNAME:
		{
			NclVarDim *vardim = (NclVarDim*)root;

			off1 = _NclPutInstr(VAR_DIMNAME_OP,vardim->line,vardim->file);
			_NclPutInstr((NclValue)vardim->sym,vardim->line,vardim->file);
			_NclPutInstr((NclValue)vardim->u.dimname,vardim->line,vardim->file);
			return(off1);
		}
			break;
		case Ncl_VARCOORD:
		{
			NclCoord *coord = (NclCoord*)root;
			int nsubs = 0;
			if(coord->subscript_list != NULL) {
				step = coord->subscript_list;
				off1 = _NclTranslate(step->node,fp);
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
				_NclPutInstr((NclValue)coord->coord_name,coord->line,coord->file);
			
			}
			return(off1);
		}
			break;
		case Ncl_FILE:
		{
			NclFile *file = (NclFile*)root;
			
			off1 = _NclPutInstr(PUSH_FILE_OP,file->line,file->file);
			_NclPutInstr((NclValue)file->dfile,file->line,file->file);
			return(off1);
		}
			break;
		case Ncl_FILEVAR:
		{
			NclFileVar *filevar = (NclFileVar*)root;
			int nsubs = 0;
		
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
			return(off1);
		}
			break;
		case Ncl_FILEDIMNUM:
		{
			NclFileDim *filedim = (NclFileDim*)root;
			
			off1 = _NclPutInstr(FILE_DIMNUM_OP,filedim->line,filedim->file);
			_NclPutInstr((NclValue)filedim->dfile,filedim->line,filedim->file);
			_NclPutInstr((NclValue)filedim->u.dimnum,filedim->line,filedim->file);
			return(off1);
		}
			break;
		case Ncl_FILEDIMNAME:
		{
			NclFileDim *filedim = (NclFileDim*)root;
			off1 = _NclPutInstr(FILE_DIMNAME_OP,filedim->line,filedim->file);
			_NclPutInstr((NclValue)filedim->dfile,filedim->line,filedim->file);
			_NclPutInstr((NclValue)filedim->u.dimname,filedim->line,filedim->file);
			return(off1);
		}
			break;
		case Ncl_FILEATT:
		{
			NclFileAtt *fileatt = (NclFileAtt*) root;
			int nsubs = 0;
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
			return(off1);
			
		}
			break;
		case Ncl_EOLN:
		{
			off1 = _NclPutInstr(LINE,groot->line,NULL);
			_NclPutInstr(groot->line,groot->line,NULL);
			return(off1);
		}
			break;
		default:
		
		fprintf(fp,"UNRECOGNIZED ENUM VALUE!\n");
			return(-1);
			break;
	}
} else {
	fprintf(fp,"ERROR NULL NODE FOUND!\n");
}
}

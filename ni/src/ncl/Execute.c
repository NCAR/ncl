

/*
 *      $Id: Execute.c,v 1.13 1994-04-18 17:10:48 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Execute.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 14 12:35:16 MDT 1993
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>

#include <data_objs/NclVar.h>
#include <data_objs/NclMultiDValdoubleData.h>
#include <data_objs/NclMultiDValfloatData.h>
#include <data_objs/NclMultiDValintData.h>
#include <data_objs/NclMultiDValshortData.h>
#include <data_objs/NclMultiDVallongData.h>
#include <data_objs/NclMultiDValstringData.h>
#include <defs.h>
#include <Symbol.h>
#include <errno.h>
#include <OpsList.h>
#include <Machine.h>
#include <Execute.h>
#include <OpsFuncs.h>
#include <y.tab.h>
#include <data_objs/DataSupport.h>

extern int cmd_line;


NclExecuteReturnStatus _NclExecute
#if __STDC__
(unsigned long start_offset)
#else 
(start_offset) 
	unsigned long start_offset;
#endif
{
	NclValue *ptr;
	int *lptr;
	char **fptr;
	NclValue *machine;
	NhlErrorTypes status = NhlNOERROR;
	static int level = 0;

	machine = _NclGetCurrentMachine();
	ptr = machine + start_offset;
	lptr = _NclGetCurrentLineRec() + start_offset;
	fptr = _NclGetCurrentFileNameRec() + start_offset;
	level++;

	while(1) {
		switch(*ptr) {
/****************************
* Zero Operand Instructions *
****************************/
			case STOPSEQ:
				level--;
				return(Ncl_STOPS);
			case CONTINUE_OP:
				level--;
				return(Ncl_CONTINUES);
			case BREAK_OP:
				level--;
				return(Ncl_BREAKS);
			case ENDSTMNT_OP:
			case NOOP :
				break;
			case NAMED_INT_SUBSCRIPT_OP :
			case INT_SUBSCRIPT_OP : {
				NclStackEntry data;
				NclStackEntry data1;
				int mask = (int)(Ncl_MultiDVallongData | Ncl_MultiDValintData | Ncl_MultiDValshortData); 

/*
* This is the first place that type checks on the vectors and range values can
* be done since it isn't until here that it is determined that normal integer
* subscripting is going on
*/
				data1.kind = NclStk_SUBREC;
				data1.u.sub_rec = (NclSubRec*)NclMalloc(
					sizeof(NclSubRec));
				data = _NclPop();
				if(data.kind == NclStk_VECREC) {
					if(data.u.vec_rec->vec->obj.obj_type_mask & mask ) {
						data1.u.sub_rec->sub_type = INT_VECT;
						data1.u.sub_rec->u.vec = data.u.vec_rec;
					} else{
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Vector subscripts must be integer");
						status = NhlFATAL;
					}
				} else if(data.kind == NclStk_RANGEREC) {
					if(((data.u.range_rec->start == NULL)
						|| (data.u.range_rec->start->obj.obj_type_mask & mask)) &&
					((data.u.range_rec->finish == NULL)
						||(data.u.range_rec->finish->obj.obj_type_mask & mask)) &&
					((data.u.range_rec->stride == NULL)
						||(data.u.range_rec->stride->obj.obj_type_mask & mask))) {
						data1.u.sub_rec->sub_type = INT_RANGE;
						data1.u.sub_rec->u.range = data.u.range_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Subscripts must be integer when not using coordinate indexing");
						status = NhlFATAL;
					}
				}
				if(*ptr == INT_SUBSCRIPT_OP) {
					data1.u.sub_rec->name = NULL;
				} else {
					data = _NclPop();
					switch(data.kind) {
					case NclStk_VAL: {
/*
* Taking for granted that syntax only allows string litterals here
*/
						data1.u.sub_rec->name = (char*)NclMalloc(strlen(*((char**) data.u.data_obj->multidval.val))+1);
						strcpy(data1.u.sub_rec->name,*(char**)data.u.data_obj->multidval.val);			
						_NclDestroyObj((NclObj)data.u.data_obj);
						
						break;
					}
					default:	
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Illegal type for coordinate name in coordinate subscript ignoring value");
						data1.u.sub_rec->name = NULL;
						break;
					}
				}
				_NclPush(data1);
				break;
			}
			case DEFAULT_RANGE_OP : {
				NclStackEntry data;
				data.kind = NclStk_NOVAL;
				data.u.offset = 0;
				_NclPush(data);
				break;
			}
			case RANGE_INDEX_OP : {
				NclStackEntry start;
				NclStackEntry finish;
				NclStackEntry stride;
				NclStackEntry data;

				stride = _NclPop();
				finish = _NclPop();
				start  = _NclPop();
				data.kind = NclStk_RANGEREC;
				data.u.range_rec = (NclRangeRec*)NclMalloc(
					sizeof(NclRangeRec));
				if(start.kind == NclStk_NOVAL) {
					data.u.range_rec->start = NULL;
				} else {
					switch(start.kind) {
					case NclStk_VAL:
						if(start.u.data_obj !=NULL) {
						data.u.range_rec->start = start.u.data_obj;
						} else {
							status = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						data.u.range_rec->start = 
								_NclVarValueRead(start.u.data_var,NULL,NULL);
						if(data.u.range_rec->start == NULL) {
							status = NhlFATAL;
						}
						break;
					default:
						status = NhlFATAL;
						break;
					}
				}
				if(finish.kind == NclStk_NOVAL) {
					data.u.range_rec->finish = NULL;
				} else {
					switch(finish.kind) {
					case NclStk_VAL:
						if(finish.u.data_obj !=NULL) {
						data.u.range_rec->finish= finish.u.data_obj;
						} else {
							status = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						data.u.range_rec->finish= _NclVarValueRead(finish.u.data_var,NULL,NULL);
						if(data.u.range_rec->finish == NULL) {
							status = NhlFATAL;
						}
						break;
					default:
						status = NhlFATAL;
						break;
					}
				}
				if(stride.kind == NclStk_NOVAL) {
					data.u.range_rec->stride= NULL;
				} else {
					switch(stride.kind) {
					case NclStk_VAL:
						if(stride.u.data_obj !=NULL) {
						data.u.range_rec->stride= stride.u.data_obj;
						} else {
							status = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						data.u.range_rec->stride= _NclVarValueRead(stride.u.data_var,NULL,NULL);
						if(data.u.range_rec->stride == NULL){
							status = NhlFATAL;
						}
						break;
					default:
						status = NhlFATAL;
						break;
					}
				}
				if((data.u.range_rec->start != NULL) &&
					(data.u.range_rec->start->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					status = NhlFATAL;
				}
				if((data.u.range_rec->finish != NULL) &&
					(data.u.range_rec->finish->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					status = NhlFATAL;
				}
				if((data.u.range_rec->stride != NULL) &&
					(data.u.range_rec->stride->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					status = NhlFATAL;
				}
				_NclPush(data);
				break;
			}
			case SINGLE_INDEX_OP : {
				NclStackEntry data;
				NclStackEntry data1;
				NclMultiDValData val;

				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAR: 
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
					if(val == NULL){
						status = NhlFATAL;
					}
					break;
				case NclStk_VAL:
					if(data.u.data_obj != NULL) {
						val = data.u.data_obj;
					} else {
						status = NhlFATAL;
					}
					break;
				default:
					status = NhlFATAL;
				}
				if(status != NhlFATAL) {
					if(val->multidval.kind == SCALAR) {
						data1.kind = NclStk_RANGEREC;
						data1.u.range_rec = 
							(NclRangeRec*)NclMalloc(
							sizeof(NclRangeRec));
						data1.u.range_rec->start = val;
						data1.u.range_rec->finish = val;
						data1.u.range_rec->stride=NULL;
						_NclPush(data1);
					} else if(val->multidval.n_dims == 1) {
						data1.kind = NclStk_VECREC;
						data1.u.vec_rec =
							(NclVecRec*)NclMalloc(
							sizeof(NclVecRec));
						data1.u.vec_rec->vec = val;
						_NclPush(data1);
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Subscripts must be scalar or one dimensional vectors\n");
						status = NhlFATAL;
					}
				}
				break;
			}
			case RETURN_OP : {
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;
				data = _NclPop();
				
				ret = _NclPlaceReturn(data);
				if(ret< NhlWARNING) {
					return(Ncl_ERRORS);
				} else {
					return(Ncl_STOPS);
				}
			}
			case IF_OP :
				break;
			case NAMED_COORD_SUBSCRIPT_OP : 
			case COORD_SUBSCRIPT_OP : {
				NclStackEntry data;
				NclStackEntry data1;
				int mask = (int)(Ncl_MultiDVallongData | Ncl_MultiDValintData | Ncl_MultiDValshortData); 

/*
* This is the first place that type checks on the vectors and range values can
* be done since it isn't until here that it is determined that normal integer
* subscripting is going on
*/
				data = _NclPop();
	
				data1.kind = NclStk_SUBREC;
				data1.u.sub_rec = (NclSubRec*)NclMalloc(
					sizeof(NclSubRec));
				if(data.kind == NclStk_VECREC) {
					data1.u.sub_rec->sub_type = COORD_VECT;
					data1.u.sub_rec->u.vec = data.u.vec_rec;
				} else if(data.kind == NclStk_RANGEREC) {
					if(((data.u.range_rec->stride == NULL)
						||(data.u.range_rec->stride->obj.obj_type_mask & mask))) {
						data1.u.sub_rec->sub_type = COORD_RANGE;
						data1.u.sub_rec->u.range = data.u.range_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. stride must always be integer regardless of whether coordinate or integer subscripting is being used\n");
						status = NhlFATAL;
					}
				}
				if(*ptr == COORD_SUBSCRIPT_OP) {
					data1.u.sub_rec->name = NULL;
				} else {
					data = _NclPop();
					switch(data.kind) {
					case NclStk_VAL: {
/*
* Taking for granted that syntax only allows string litterals here
*/
						data1.u.sub_rec->name = NclMalloc(strlen(*(char**) data.u.data_obj->multidval.val)+1);
						strcpy(data1.u.sub_rec->name,*(char**)data.u.data_obj->multidval.val);			
						
						_NclDestroyObj((NclObj)data.u.data_obj);
						break;
					}
					default:	
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Illegal type for coordinate name in coordinate subscript ignoring value");
						data1.u.sub_rec->name = NULL;
						break;
					}
				}
				_NclPush(data1);
				break;
			} 
			case NEG_OP : {
				NclStackEntry data;
				NclStackEntry operand;
				operand = _NclPop();
				status = _NclMonoOp(operand,&data,NEG_OP);
				_NclPush(data);
			}
			break;
			case NOT_OP : {
				NclStackEntry data;
				NclStackEntry operand;
				operand = _NclPop();
				status = _NclMonoOp(operand,&data,NOT_OP);
				_NclPush(data);
			}
			break;
			case MOD_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,MOD_OP);
				_NclPush(data);
			}
			break;
			case OR_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,OR_OP);
				_NclPush(data);
			}
			break;
			case AND_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,AND_OP);
				_NclPush(data);
			}
			break;
			case XOR_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,XOR_OP);
				_NclPush(data);
			}
			break;
			case LTSEL_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,LTSEL_OP);
				_NclPush(data);
			}
			break;
			case GTSEL_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,GTSEL_OP);
				_NclPush(data);
			}
			break;
			case PLUS_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status =  _NclDualOp(lhs,rhs,&data,PLUS_OP);
				_NclPush(data);
			}
				break;
			case MINUS_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,MINUS_OP);
				_NclPush(data);
			}
				break;
			case MUL_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,MUL_OP);
				_NclPush(data);
			}
				break;
			case MAT_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,MAT_OP);
				_NclPush(data);
			}
				break;
			case DIV_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,DIV_OP);
				_NclPush(data);
			}
				break;
			case EXP_OP :{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,EXP_OP);
				_NclPush(data);
			}
			break;
			case LE_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,LE_OP);
				_NclPush(data);
			}
			break;
			case GE_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,GE_OP);
				_NclPush(data);
			}
			break;
			case GT_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,GT_OP);
				_NclPush(data);
			}
			break;
			case LT_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,LT_OP);
				_NclPush(data);
			}
			break;
			case EQ_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,EQ_OP);
				_NclPush(data);
			}
			break;
			case NE_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				lhs = _NclPop();
				rhs = _NclPop();
				status = _NclDualOp(lhs,rhs,&data,NE_OP);
				_NclPush(data);
			}
			break;
/***************************
* One Operand Instructions *
***************************/
			case FUNC_CALL_OP: {
				NclSymbol *func = NULL;

				ptr++;lptr++;fptr++;
				func = (NclSymbol*)(*ptr);

				_NclFinishFrame();
				
				status = _NclFuncCallOp(func);
				break;
			}
			case FPDEF:
				ptr++;lptr++;fptr++;
				break;
			case JMP:
			{
				ptr++;lptr++;fptr++;
/*
* Needs to be substracted by one so ptr++ is valid
*/
				ptr = machine + (*ptr - 1);		
				break;
			}
			case ARRAY_LIT_OP :
			{
				NclStackEntry data;
				ptr++;lptr++;fptr++;
				status = _NclBuildArray((int)*ptr,&data);
				_NclPush(data);
				break;
			}
			case PUSH_STRING_LIT_OP :
			{
				NclStackEntry data;
				int *thestr;
				int dim_size = 1;
			
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				thestr = (int*)NclMalloc((unsigned)sizeof(int));
				*thestr = *ptr;
				data.u.data_obj = _NclMultiDValstringCreate(NULL,
						NULL,Ncl_MultiDValstringData,0,
						(void*)thestr,NULL,1,&dim_size,
						TEMPORARY,NULL);
				_NclPush(data);
				break;
			}
			case PUSH_REAL_LIT_OP : 
			{
				NclStackEntry data;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclMultiDValfloatCreate(NULL,
						NULL,Ncl_MultiDValfloatData,0,
						(void*)ptr,NULL,1,&dim_size,
						STATIC,NULL);
				_NclPush(data);
				break;
			}
			case PUSH_INT_LIT_OP :
			{
				NclStackEntry data;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclMultiDValintCreate(NULL,
						NULL,Ncl_MultiDValintData,0,
						(void*)ptr,NULL,1,&dim_size,
						STATIC,NULL);
				_NclPush(data);
				break;
			}
			case JMPFALSE :
				ptr++;lptr++;fptr++;
				break;
			case SET_OBJ_OP :
				ptr++;lptr++;fptr++;
				break;
			case GET_OBJ_OP :
				ptr++;lptr++;fptr++;
				break;
			case PROC_CALL_OP:{
				NclSymbol *proc = NULL;

				ptr++;lptr++;fptr++;
				proc = (NclSymbol*)(*ptr);
			
				_NclFinishFrame();	
				status = _NclProcCallOp(proc);
			}
				break;
			case BPROC_CALL_OP:
				ptr++;lptr++;fptr++;
				break;
			case INTRINSIC_PROC_CALL:
			{
				int i;
				NclStackEntry data;
				ptr++;lptr++;fptr++;
/*
* This is not going to work because nothing is done to unpack the
* arguments they are just popped now!!!!!!
*/
				if(((NclSymbol*)*ptr)->u.bproc != NULL) {
					(*((NclSymbol*)*ptr)->u.bproc->theproc)();
/*
* should actually map values back
*/
					for(i = 0;i<((NclSymbol*)*ptr)->u.bproc->nargs; i++) {
						data = _NclPop();
						switch(data.kind) {
						case NclStk_VAR:
							if((data.u.data_var != NULL)&&(data.u.data_var->obj.status != PERMANENT)){
								_NclDestroyObj((NclObj)data.u.data_obj);
							}
							break;
						case NclStk_VAL:
							if((data.u.data_obj != NULL)&&(data.u.data_obj->obj.status != PERMANENT)){
								_NclDestroyObj((NclObj)data.u.data_obj);
							}
							break;
						default:
							break;
						}
	
					}
				}
				ptr++;lptr++;fptr++;
			}
				break;
			case BFUNC_CALL_OP:
				ptr++;lptr++;fptr++;
				break;
			case DO_FROM_TO_OP :
				ptr++;lptr++;fptr++;
				break;
			case DO_FROM_TO_STRIDE_OP :
				ptr++;lptr++;fptr++;
				break;
			case PARAM_VAR_DIM_OP:
			case VAR_DIM_OP: {
				NclSymbol *thesym;
				long dim_num;
				NclStackEntry data;
				NclMultiDValData data_md = NULL;
				NclStackEntry *var;
				unsigned int valid_dims = ((int)Ncl_MultiDVallongData 
					| (int)Ncl_MultiDValintData 
					| (int)Ncl_MultiDValshortData);

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
		
				var = _NclRetrieveRec(thesym);

				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAL:	
					data_md = data.u.data_obj;
					break;
				case NclStk_VAR:
					data_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
					break;
				default:
					data_md = NULL;
/* ---------> Error message here < +++++++++ */				
					status = NhlFATAL;
					break;
				}
				if((data_md != NULL)&&(data_md->obj.obj_type_mask & valid_dims)&&(data_md->multidval.kind == SCALAR)&&(var!= NULL)&&(var->u.data_var != NULL)) {	
					if(!(data_md->obj.obj_type_mask & Ncl_MultiDVallongData)) {
						_NclScalarCoerce(
							(void*)data_md->multidval.val,
							data_md->multidval.data_type,
							(void*)&dim_num,
							NCL_long);
					} else {
						dim_num = *(long*)
							data_md->multidval.val;
					}
					if((data_md->obj.status != PERMANENT)&&(data_md->obj.ref_count ==0)) {
						_NclDestroyObj((NclObj)data_md);
					}


					data.u.data_obj = _NclReadDim(
						var->u.data_var,
						NULL,
						dim_num
						);
					if(data.u.data_obj == NULL) {
						status = NhlFATAL;
					} else {
						data.kind = NclStk_VAL;
						_NclPush(data);
					}
				} else {
					status = NhlFATAL;
				}
			}
			break;
			case ASSIGN_VAR_DIM_OP: {
				NclSymbol *thesym = NULL;
				long	dim_num;
				char	*dim_name = NULL;
				NclStackEntry dim_ref;
				NclStackEntry dim_expr;
				NclMultiDValData dim_ref_md = NULL;
				NclMultiDValData dim_expr_md = NULL;
				NclStackEntry *data_var = NULL;
				unsigned int valid_dims = (unsigned int)(Ncl_MultiDVallongData 
					| Ncl_MultiDValintData 
					| Ncl_MultiDValshortData);
				unsigned int valid_expr = (unsigned int)(Ncl_MultiDValstringData 
					| Ncl_MultiDValcharData);

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
				data_var =  _NclRetrieveRec(thesym);
				dim_ref = _NclPop();
				dim_expr = _NclPop();
				
				switch(dim_ref.kind) {
				case NclStk_VAL: 
					dim_ref_md = dim_ref.u.data_obj;
				break;
				case NclStk_VAR:	
					dim_ref_md = _NclVarValueRead(dim_ref.u.data_var,NULL,NULL);
					break;
				default:
					break;
				}
				switch(dim_expr.kind) {
				case NclStk_VAL:
					dim_expr_md = dim_expr.u.data_obj;
					break;
				case NclStk_VAR:	
					dim_expr_md = _NclVarValueRead(dim_expr.u.data_var,NULL,NULL);
					break;
				default:
					break;
				}
				if((data_var != NULL )&&(data_var->u.data_var != NULL)
					&&(dim_expr_md->obj.obj_type_mask & valid_expr)
					&&(dim_ref_md->obj.obj_type_mask & valid_dims)
					&&(dim_expr_md->multidval.kind == SCALAR)
					&&(dim_ref_md->multidval.kind == SCALAR)) {
					if(!(dim_expr_md->multidval.data_type != Ncl_MultiDValstringData)) {
						_NclScalarCoerce(
							(void*)dim_expr_md->multidval.val,
							dim_expr_md->multidval.data_type,
							(void*)&dim_name,
							NCL_long);
							
					} else {
						dim_name = *(char**)dim_expr_md->multidval.val;
					}
					if((dim_ref_md->multidval.data_type != NCL_long)) {
						_NclScalarCoerce(
							(void*)dim_ref_md->multidval.val,
							dim_ref_md->multidval.data_type,
							(void*)&dim_num,
							NCL_long);
							
					} else {
						dim_num= *(long*)dim_ref_md->multidval.val;
					}

					if(status != NhlFATAL) {
					status = _NclWriteDim(
						data_var->u.data_var,
						dim_num,
						dim_name);
					}
					if((dim_expr_md->obj.status != PERMANENT)&&(dim_expr_md->obj.ref_count == 0)) {
						_NclDestroyObj((NclObj)dim_expr_md);
					}
					if((dim_ref_md->obj.status != PERMANENT)&&(dim_ref_md->obj.ref_count == 0)) {
						_NclDestroyObj((NclObj)dim_ref_md);
					}
				} else {
					status = NhlFATAL;
				}
			}
			break;
/***************************
* Two Operand Instructions *
***************************/			
			case PARAM_VAR_OP:
			case VAR_READ_OP: {
				NhlErrorTypes ret = NhlNOERROR;
				int i;
				int nsubs;
				NclStackEntry data;
				NclStackEntry data1;
				NclStackEntry* var;
				NclSymbol *sym;
				NclSelectionRecord *sel_ptr=NULL;
				int dim_is_ref[NCL_MAX_DIMENSIONS];

				ptr++;lptr++;fptr++;
				sym = (NclSymbol*)*ptr;
				var = _NclRetrieveRec(sym);
				ptr++;lptr++;fptr++;
				nsubs = *ptr;
				if(var->u.data_var == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined",sym->name);
					status = NhlFATAL;
				} else if(nsubs == 0) {
					if(var != NULL) {
						_NclPush(*var);
					}
				} else if(nsubs != var->u.data_var->var.n_dims) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimesions of variable,(%d) Subscripts used, (%d) Subscripts expected",nsubs,var->u.data_var->var.n_dims);
					status = NhlFATAL;
				} else {
					sel_ptr = (NclSelectionRecord*)NclMalloc
						(sizeof(NclSelectionRecord));
					sel_ptr->n_entries = nsubs;
					for(i=0;i<nsubs;i++) {
						dim_is_ref[i] = 0;
					}
					for(i=0;i<nsubs;i++) {
						data =_NclPop();
						switch(data.u.sub_rec->sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildVSelection(var->u.data_var,data.u.sub_rec->u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
							break;
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,data.u.sub_rec->u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
							break;
						case COORD_VECT:
						case COORD_RANGE:
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING) {
							status = NhlFATAL;
						}
						if(!dim_is_ref[(sel_ptr->selection[nsubs - i - 1]).dim_num]) {
							dim_is_ref[(sel_ptr->selection[nsubs - i - 1]).dim_num] = 1;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in subscript # %d,dimension is referenced more than once",i);
							status = NhlFATAL;
						}
					} 
					if(status != NhlFATAL) {
						data1.kind = NclStk_VAR;
						data1.u.data_var = _NclVarRead(var->u.data_var,sel_ptr);
						if(data1.u.data_var != NULL) {
							_NclPush(data1);
						} else {
							status = NhlFATAL;
						}
					}
				}
			}
			break;
			case ASSIGN_VAR_OP :{
				NclStackEntry rhs;
				NclStackEntry data;
				NclStackEntry *lhs_var = NULL;
				NclMultiDValData rhs_md = NULL;
				NclMultiDValData tmp_md = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				int i,nsubs;	
				NclSymbol *sym = NULL;
				NhlErrorTypes ret = NhlNOERROR;
			

			ptr++;lptr++;fptr++;
			sym = (NclSymbol*)(*ptr);

			ptr++;lptr++;fptr++;
			nsubs = *ptr;

			lhs_var = _NclRetrieveRec(sym);
			if((status != NhlFATAL)&&(lhs_var != NULL)) {
				if(lhs_var->kind == NclStk_NOVAL) {
					if(nsubs != 0) {
						status = NhlFATAL;
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Assign: %s is undefined, can not subscript an undefined variable",sym->name);
						status = NhlFATAL;
						_NclCleanUpStack(nsubs+1);
					} else {
						rhs = _NclPop();	
						if(rhs.kind == NclStk_VAL) {
							rhs_md = rhs.u.data_obj;
							if(rhs_md != NULL) {
								if(rhs_md->obj.status != TEMPORARY) {
/*
* This is ok no ponters are lost since rhs_md was permanent which means that
* some NclVar object has a reference to it
*/
									tmp_md = rhs_md;	
									rhs_md= _NclCopyVal(rhs_md,NULL);
									if(tmp_md->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)tmp_md);
									}
				
								}
								lhs_var->u.data_var= _NclVarCreate(NULL,NULL,Ncl_Var,0,sym,rhs_md,NULL,-1,NULL,NORMAL,sym->name);
								if(lhs_var->u.data_var != NULL) {
									(void)_NclChangeSymbolType(sym,VAR);
									lhs_var->kind = NclStk_VAR;
								} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
									status = NhlWARNING;
									lhs_var->kind = NclStk_NOVAL;
								}
							} 
						} else if(rhs.kind == NclStk_VAR) {
/*
* -----> need some modification here. Only time this happens is when a funcion
* returns a variable. Otherwise ASSIGN_VAR_VAR_OP is used by the translator.
* This should be changed to call possibly the _NclAssignVarToVar  function in
* this situation as well as destroy the return variable if
*/
							rhs_md = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
							if(rhs_md != NULL) {
								if(rhs_md->obj.status != TEMPORARY) {
/*
* This is ok no ponters are lost since rhs_md was permanent which means that
* some NclVar object has a reference to it
*/
									tmp_md = rhs_md;
									rhs_md= _NclCopyVal(rhs_md,NULL);
									if(tmp_md->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)tmp_md);
									}
								}
								lhs_var->u.data_var= _NclVarCreate(NULL,NULL,Ncl_Var,0,sym,rhs_md,rhs.u.data_var->var.dim_info,rhs.u.data_var->var.att_id,rhs.u.data_var->var.coord_vars,NORMAL,sym->name);
								if(lhs_var->u.data_var != NULL) {
									(void)_NclChangeSymbolType(sym,VAR);
									lhs_var->kind = NclStk_VAR;
								} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
									status = NhlWARNING;
									lhs_var->kind = NclStk_NOVAL;
								}
							} else {
								status = NhlFATAL;
							} 
							if(rhs.u.data_var->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)rhs.u.data_var);
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal right-hand side type for assignment");
							status = NhlFATAL;
						}
					}
				} else if(lhs_var->kind == NclStk_VAR) {
					if((nsubs != lhs_var->u.data_var->var.n_dims)&&(nsubs != 0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts (%d) and number of dimensions (%d) do not match for variable (%s)",nsubs,lhs_var->u.data_var->var.n_dims,sym->name);
						status = NhlFATAL;
						_NclCleanUpStack(nsubs+1);
					}
					if(nsubs != 0) {
						sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
						sel_ptr->n_entries = nsubs;
					} else {
						sel_ptr = NULL;
					}

					for(i=0;i<nsubs;i++) {
						data =_NclPop();
						switch(data.u.sub_rec->sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildVSelection(lhs_var->u.data_var,data.u.sub_rec->u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
							break;
						case INT_RANGE:
/*
* Need to free some stuff here
*/								
							ret = _NclBuildRSelection(lhs_var->u.data_var,data.u.sub_rec->u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
							break;
						case COORD_VECT:
						case COORD_RANGE:
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING) {
							status = NhlFATAL;
						}
					}
					rhs = _NclPop();	
					if(status != NhlFATAL) {
						if(rhs.kind == NclStk_VAL) {
							rhs_md = rhs.u.data_obj;
							if(rhs_md != NULL) {
								ret = _NclAssignToVar(lhs_var->u.data_var,rhs_md,sel_ptr);
								if(rhs_md->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)rhs_md);
								}
								if(ret <= NhlWARNING) {
									status = ret;
								}
							} else {
								status = NhlFATAL;
							}
						} else if(rhs.kind == NclStk_VAR) {
/*
* I don't pass in a new missing in this situation because
* _NclAssignToVar checks the missing values and it has
* to visit each element anyways
*/
							status = _NclAssignVarToVar(rhs.u.data_var,sel_ptr,rhs.u.data_var,NULL);
							if(rhs.u.data_var->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)rhs.u.data_var);
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal right-hand side type for assignment");
							status = NhlFATAL;
						}
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Assignment not supported for left-hand type");
					status = NhlFATAL;
				}

			} else {
				_NclCleanUpStack(nsubs);
			}
			break;
			}
			case NEW_FRAME_OP: {
				NclSymbol *proc;
				int offset;
				ptr++;lptr++;fptr++;
				proc = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				offset = (int)(*ptr);
				if((proc->u.procfunc != NULL)&&(proc->u.procfunc->thescope != NULL)&&(offset >= 0)) {
					_NclPushFrame(proc->u.procfunc->thescope,offset,proc->u.procfunc->nargs);
				} else {
					status = NhlFATAL;
				}
				break;
			}
			case CONVERT_TO_LOCAL: {
				NclSymbol *thesym = NULL;
				NclGenProcFuncInfo *pfinfo = NULL;
				NclSymbol *argsym = NULL;
				NclStackEntry data;
				unsigned int obj_type_param;
				unsigned int obj_type_arg;
				NclMultiDValData tmp_md = NULL;
				NclVar tmp_var = NULL;
				int i;
				int arg_num = -1;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				arg_num = (int)(*ptr);

				switch(thesym->type) {
/*
*				case IPROC:
*				case IFUNC:
* Intrinsic functions don't have local variables.
*/
				case FUNC:
				case PROC:
					pfinfo = (NclGenProcFuncInfo*)thesym->u.bproc;
					break;
				case NFUNC:
				case NPROC:
					pfinfo = (NclGenProcFuncInfo*)thesym->u.procfunc;
					break;
				case EFUNC:
				case EPROC:
				default:
					pfinfo = NULL;
					break;
				}
				if(pfinfo == NULL) {
					status = NhlFATAL;
				} else if(arg_num >= pfinfo->nargs) {
					status = NhlFATAL;
				} else {
					argsym = pfinfo->theargs[arg_num].arg_sym;
/*
*---> Need to look into allowing HLU objects to be used as parameters
* in which case this will not be enough also files <----
*/
					obj_type_arg = _NclKeywordToObjType(pfinfo->theargs[arg_num].arg_data_type);
					if(obj_type_arg == Ncl_Obj) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal type for argument in argument (%d) of (%s)",arg_num,thesym->name);
						status = NhlFATAL;
					}
/*
* Check dimensions first since it isn't expensive
*/		
					data = _NclPop();
					switch(data.kind) {
					case NclStk_VAR: {

						if(pfinfo->theargs[arg_num].is_dimsizes) {
							if(pfinfo->theargs->n_dims != data.u.data_var->var.n_dims) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) does not match specification",arg_num,thesym->name);
								status = NhlFATAL;

							} else {
								for(i = 0; i< pfinfo->theargs->n_dims; i++) {
									if(pfinfo->theargs->dim_sizes[i] != -1) {
										if(pfinfo->theargs->dim_sizes[i] != data.u.data_var->var.dim_info[i].dim_size) {
											NhlPError(NhlFATAL,NhlEUNKNOWN,"Size of dimension (%d) of argument (%d) does not match specification in (%s) function definition",i,arg_num,thesym->name);
											status = NhlFATAL;
										}
									}
									if(status == NhlFATAL) {
										break;
									} else {
										i++;
									}
								}
							}
						}
/*
* Variable subsections also point to the symbol of the main variable so the AddObjToParamList just
* stores the symbol rather than the pointer to variable record
*/
						_NclAddObjToParamList((NclObj)data.u.data_var,arg_num);
						if(status != NhlFATAL) {
							obj_type_param = _NclGetVarRepValue(data.u.data_var);
							if(!(obj_type_param & obj_type_arg)){
								tmp_md = _NclCoerceVar(data.u.data_var,obj_type_arg,NULL);
								if(tmp_var == NULL) {
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
									status = NhlFATAL;
								}
/*
* Attention: missing value may be different type than variable data until code is put here to fix it
*/
								tmp_var = _NclVarCreate(NULL,data.u.data_var->obj.class_ptr,
										Ncl_Var,
										0,
										argsym,
										tmp_md,
										data.u.data_var->var.dim_info,
										data.u.data_var->var.att_id,
										data.u.data_var->var.coord_vars,
										PARAM,
										argsym->name);
										
							} else {
								tmp_var = _NclVarCreate(NULL,data.u.data_var->obj.class_ptr,
										data.u.data_var->obj.obj_type,
										data.u.data_var->obj.obj_type_mask,
										argsym,
										(NclMultiDValData)_NclGetObj(data.u.data_var->var.thevalue_id),
										data.u.data_var->var.dim_info,
										data.u.data_var->var.att_id,
										data.u.data_var->var.coord_vars,
										PARAM,
										argsym->name);
							}
/*
* Need to put ancestor of local variable in the parmeter list so it can be unpacked later
*/

						}
						if(status != NhlFATAL) {
							data.kind = NclStk_VAR;
							data.u.data_var = tmp_var;
							_NclPush(data);
						}
					}
					break;
					case NclStk_VAL: {
						if(pfinfo->theargs[arg_num].is_dimsizes) {
							if(pfinfo->theargs->n_dims != data.u.data_obj->multidval.n_dims) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) does not match specification",arg_num,thesym->name);
								status = NhlFATAL;

							} else {
								for(i = 0; i< pfinfo->theargs->n_dims; i++) {
									if(pfinfo->theargs->dim_sizes[i] != -1) {
										if(pfinfo->theargs->dim_sizes[i] != data.u.data_obj->multidval.dim_sizes[i]) {
											NhlPError(NhlFATAL,NhlEUNKNOWN,"Size of dimension (%d) of argument (%d) does not match specification in (%s) function definition",i,arg_num,thesym->name);
											status = NhlFATAL;
										}
									}
									if(status == NhlFATAL) {
										break;
									} else {
										i++;
									}
								}
							}
						}
						_NclAddObjToParamList((NclObj)data.u.data_obj,arg_num);
                                                if(status != NhlFATAL) {
                                                        obj_type_param =((NclMultiDValData)data.u.data_obj)->obj.obj_type;
                                                        if(!(obj_type_param & obj_type_arg)){
                                                                tmp_md = _NclCoerceData(data.u.data_obj,obj_type_arg,NULL);
                                                                if(tmp_var == NULL) {
                                                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
                                                                        status = NhlFATAL;
                                                                }
/*
* Attention: missing value may be different type than variable data until code is put here to fix it
*/
                                                                tmp_var = _NclVarCreate(
										NULL,NULL,
                                                                                Ncl_Var,
                                                                                0,
                                                                                argsym,
                                                                                tmp_md,
                                                                                NULL,
                                                                                -1,
                                                                                NULL,
                                                                                PARAM,
                                                                                argsym->name);

                                                        } else {
                                                                tmp_var = _NclVarCreate(
										NULL,NULL,
                                                                                Ncl_Var,
                                                                                0,
                                                                                argsym,
                                                                                data.u.data_obj,
                                                                                NULL,
                                                                                -1,
                                                                                NULL,
                                                                                PARAM,
                                                                                argsym->name);
                                                        }
/*
* Need to put ancestor of local variable in the parmeter list so it can be unpacked later
*/

                                                }
                                                if(status != NhlFATAL) {
                                                        data.kind = NclStk_VAR;
                                                        data.u.data_var = tmp_var;
                                                        _NclPush(data);
                                                }
					}
					break;
					case NclStk_FILE:
					case NclStk_GRAPHIC:
					default:
						break;
					}
/*
					_NclChangeSymbolType(argsym,VAR);
*/
				}
				break;
			}
			case DO_WHILE_OP :
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
			case FILEVAR_DIM_OP:	
			case ASSIGN_FILEVAR_DIM_OP:
			case PARAM_FILEVAR_DIM_OP:
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
/*****************************
* Three Operand Instructions *
*****************************/
			case CREATE_OBJ_WP_OP : 
			case CREATE_OBJ_OP : {
				int nres;
				NclSymbol *objname;
				NclSymbol *objtype;
				NclStackEntry parent;
				int parent_id = -1;
				if(*ptr == CREATE_OBJ_WP_OP) {
				/*--->Code to retrieve parent<---*/
					parent_id = -1;
				}
				ptr++;lptr++;fptr++;
				nres = (int)*ptr;
				ptr++;lptr++;fptr++;
				objname =(NclSymbol*)*ptr ;
				ptr++;lptr++;fptr++;
				objtype =(NclSymbol*)*ptr ;

				status = _NclCreateHLUObjOp(nres,objname,objtype,parent_id);
			}
			break;
			case PARAM_VARATT_OP:
			case VARATT_OP: {
				NclSymbol *thesym = NULL;
				char*	attname = NULL;
				int	nsubs;
				NclStackEntry *var = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (int)(*ptr);

				var = _NclRetrieveRec(thesym);
				if(var->u.data_var != NULL) {
					if(_NclVarIsAtt(var->u.data_var,attname)) {
						if(nsubs == 1) {
							sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
							sel_ptr->n_entries = 1;
							data =_NclPop();
							if(data.u.sub_rec->name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
								status = NhlWARNING;
							}
							switch(data.u.sub_rec->sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/						
								ret = _NclBuildVSelection(NULL,data.u.sub_rec->u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
							case INT_RANGE:
/*
* Need to free some stuff here
*/								
								ret = _NclBuildRSelection(NULL,data.u.sub_rec->u.range,&(sel_ptr->selection[0]),0,NULL);
								break;
							case COORD_VECT:
							case COORD_RANGE:
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
								status = NhlFATAL;
								break;
							}
						_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING)
								status = ret;
						} else if(nsubs != 0) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attributes only have one dimension, %d subscripts used",nsubs);		
							status = NhlFATAL;
						}
					} else {
						status = NhlFATAL;
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference attribute (%s) which is undefined",attname);
					}
					if(status != NhlFATAL) {
						data.u.data_obj = _NclReadAtt(var->u.data_var,attname,sel_ptr);
						if(data.u.data_obj == NULL) {
							data.kind = NclStk_NOVAL;
							status = NhlFATAL;
						} else {
							data.kind = NclStk_VAL;
						}
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is still undefined, unable to reference attribute %s",thesym->name,attname);
					status = NhlFATAL;
				}
				_NclPush(data);
			}
			break;
			case ASSIGN_VAR_COORD_OP: {
				NclStackEntry *var = NULL;
				NclStackEntry data;
				NclSymbol* thesym = NULL;
				char *coord_name = NULL;
				int nsubs = 0;
				NhlErrorTypes ret = NhlNOERROR;
				NclSelectionRecord *sel_ptr = NULL;
				NclMultiDValData thevalue = NULL;
				

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (int)*ptr;

				var = _NclRetrieveRec(thesym);
				if((var == NULL)||(var->u.data_var == NULL)) {
					status = NhlFATAL;
				} else if(_NclIsDim(var->u.data_var,coord_name) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension in variable (%s).",coord_name,thesym->name);
					status = NhlFATAL;
				} else {
					if(nsubs == 0) {
						sel_ptr = NULL;
					} else if(nsubs == 1){
						sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec->name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							status = NhlWARNING;
						}
						switch(data.u.sub_rec->sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret = _NclBuildVSelection(var->u.data_var,data.u.sub_rec->u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,data.u.sub_rec->u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
							NclFree(sel_ptr);
							sel_ptr = NULL;
							status = NhlFATAL;
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING)
							status = NhlFATAL;

					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts on left hand side of assignement",nsubs);
						_NclCleanUpStack(nsubs);
						status = NhlFATAL;
					}
					if(status != NhlFATAL) {
						data = _NclPop();
						switch(data.kind) {
						case NclStk_VAL: 
							thevalue = data.u.data_obj;
							break;
						case NclStk_VAR:
							thevalue = _NclVarValueRead(data.u.data_var,NULL,NULL);
							break;
						default:
							thevalue = NULL;
							status = NhlFATAL;
						break;
						}
					
						if(thevalue != NULL) {
							ret = _NclWriteCoordVar(var->u.data_var,thevalue,coord_name,sel_ptr);
							if(status < ret){
								status = ret;
							}
						} else {
							status = NhlFATAL;
						}
					} else {	
						_NclCleanUpStack(1);
					}
				}
			}
			break;
			case PARAM_VAR_COORD_OP:
			case VAR_READ_COORD_OP:
			case VAR_COORD_OP: {
				NclStackEntry *var = NULL;
				NclStackEntry data;
				NclSymbol* thesym = NULL;
				char *coord_name = NULL;
				int nsubs = 0;
				NclSelectionRecord *sel_ptr = NULL;
				NhlErrorTypes ret = NhlNOERROR;
				

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (int)*ptr;

				var = _NclRetrieveRec(thesym);
				if((var == NULL)||(var->u.data_var == NULL)) {
					status = NhlFATAL;
				} else if(_NclIsDim(var->u.data_var,coord_name) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension in variable (%s).",coord_name,thesym->name);
					status = NhlFATAL;
				} else {
					if(nsubs == 0) {
						sel_ptr = NULL;
					} else if(nsubs == 1){
						sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec->name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							status = NhlWARNING;
						}
						switch(data.u.sub_rec->sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret = _NclBuildVSelection(var->u.data_var,data.u.sub_rec->u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,data.u.sub_rec->u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
							NclFree(sel_ptr);
							sel_ptr = NULL;
							status = NhlFATAL;
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING)
							status = NhlFATAL;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts used on coordinate variable reference",nsubs);
						_NclCleanUpStack(nsubs);
						status = NhlFATAL;
					}
					if(status != NhlFATAL) {
						data.u.data_var = _NclReadCoordVar(var->u.data_var,coord_name,sel_ptr);
						if(data.u.data_var != NULL) {
							data.kind = NclStk_VAR;
							_NclPush(data);
						} else {
							status = NhlFATAL;
						}
					} 
				}
			}
			break;
			case PARAM_FILE_VAR_OP:
			case FILE_VAR_OP :
			case ASSIGN_FILE_VAR_OP :
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
			case ASSIGN_VARATT_OP: {
				NclSymbol *thesym = NULL;
				char *attname = NULL;
				int nsubs;
				NhlErrorTypes ret = NhlNOERROR;
				NclStackEntry *var = NULL;
				NclStackEntry value;
				NclMultiDValData value_md = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry data1;
				
				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (int)(*ptr);
	
				var = _NclRetrieveRec(thesym);
				if((var->u.data_var != NULL)&&!(status < NhlINFO)) {
					if(nsubs == 1) {
						sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
						sel_ptr->n_entries = 1;
						data1 =_NclPop();
						if(data1.u.sub_rec->name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
							status = NhlWARNING;
						}
						switch(data1.u.sub_rec->sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret =_NclBuildVSelection(NULL,data1.u.sub_rec->u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_RANGE:
/*
* Need to free some stuff here
*/								
							ret =_NclBuildRSelection(NULL,data1.u.sub_rec->u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
							status = NhlFATAL;
							break;
						}
						_NclFreeSubRec(data1.u.sub_rec);
						if(ret < NhlWARNING) 
							status = NhlFATAL;
					} else if(nsubs != 0){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to subscript attribute with more than one dimension");
						status = NhlFATAL;
					}
					if(!(status < NhlINFO)) {
						value = _NclPop();
						if(value.kind == NclStk_VAR) {
							value_md = _NclVarValueRead(value.u.data_var,NULL,NULL);
							if(value_md == NULL) {
								status = NhlFATAL;
							}
						} else if(value.kind == NclStk_VAL){
							value_md = value.u.data_obj;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to assign illegal type or value to variable attribute");
							status = NhlFATAL;
						}
						ret = _NclWriteAtt(var->u.data_var,attname,value_md,sel_ptr);
						if(value_md->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)value_md);
						}
						if( ret < NhlINFO) {
							status = ret;
						}
					} else {
						_NclCleanUpStack(1);
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined, can not assign attribute (%s)",thesym->name,attname);
					status = NhlFATAL;
				}
			}
			break;
/*****************************
* Four Operand Instructions  *
*****************************/
			case FILEVARATT_OP:
			case ASSIGN_FILEVARATT_OP:
			case PARAM_FILEVARATT_OP:
			case FILEVAR_COORD_OP:
			case ASSIGN_FILEVAR_COORD_OP:
			case PARAM_FILEVAR_COORD_OP:
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
			break;
			case ASSIGN_VAR_VAR_OP: {
				NhlErrorTypes ret = NhlNOERROR;
				int i;
				int rhs_nsubs=0,lhs_nsubs=0;
				NclStackEntry data;
				NclStackEntry *rhs_var,*lhs_var;
				NclSymbol *rhs_sym,*lhs_sym;
				NclSelectionRecord *lhs_sel_ptr = NULL;
				NclSelectionRecord *rhs_sel_ptr = NULL;
				struct _NclVarRec *tmp_var;

	
				ptr++;lptr++;fptr++;
				rhs_sym = (NclSymbol*)*ptr;
				rhs_var = _NclRetrieveRec(rhs_sym);
				ptr++;lptr++;fptr++;
				rhs_nsubs = *ptr;
				ptr++;lptr++;fptr++;
				lhs_sym = (NclSymbol*)*ptr;
				lhs_var = _NclRetrieveRec(lhs_sym);
				ptr++;lptr++;fptr++;
				lhs_nsubs = *ptr;

				if((rhs_var == NULL)||(rhs_var->kind == NclStk_NOVAL)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN," Assign: %s is undefined",rhs_sym->name);
					status = NhlFATAL;
				}

				if((status!=NhlFATAL)&&(lhs_var != NULL)&&(lhs_var->kind == NclStk_NOVAL)) {
					if(lhs_nsubs != 0) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Assign: %s is undefined, can not subscript an undefined variable",lhs_sym->name);
						status = NhlFATAL;
						_NclCleanUpStack(lhs_nsubs);
					} else if(rhs_nsubs != 0) {
/*
* This branch is where wholesale assigment of rhs to lhs occurs. including coords,atts and values
*/
					rhs_sel_ptr = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
					rhs_sel_ptr->n_entries = rhs_nsubs;
					for(i=0;i<rhs_nsubs;i++) {
						data =_NclPop();
						switch(data.u.sub_rec->sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildVSelection(rhs_var->u.data_var,data.u.sub_rec->u.vec,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
							break;
						case INT_RANGE:
/*
* Need to free some stuff here
*/								
							ret = _NclBuildRSelection(rhs_var->u.data_var,data.u.sub_rec->u.range,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
							break;
						case COORD_VECT:
						case COORD_RANGE:
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING) {
							status = NhlFATAL;
						}
					} 
					lhs_var->kind = NclStk_VAR;
					lhs_var->u.data_var = _NclVarRead(rhs_var->u.data_var,rhs_sel_ptr);
					if(!_NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT)) {	
						tmp_var = lhs_var->u.data_var;
						lhs_var->u.data_var = _NclCopyVar(lhs_var->u.data_var,NULL,NULL);
						_NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT);	
						if(lhs_var->u.data_var->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)tmp_var);
						}
					}
/*
* ----> May want to encapsulate the following into the NclVar object
* 	A likely function interface would be: _NclChangeVar(int quark,NclSymbol *thesym, NclVarTypes var_type); 
* 	which would be a method.
*/
					lhs_var->u.data_var->var.var_quark = NrmStringToQuark(lhs_sym->name);
					lhs_var->u.data_var->var.thesym = lhs_sym;
					lhs_var->u.data_var->var.var_type = NORMAL;
/*
*-----> end of questionable code
*/
					} else {
						lhs_var->kind = NclStk_VAR;
						lhs_var->u.data_var = _NclCopyVar(rhs_var->u.data_var,NULL,NULL);
					}
				} else if((status !=NhlFATAL)&&(lhs_var->kind == NclStk_VAR)&&(lhs_var->u.data_var != NULL)) {
/*
* When the target variable is already defined just normal assignment occurs if it is not subscripted
* if it is then the _NclAssignVarToVar is used which is different then the normal assignment provided
* by the ASSIGN_VAR_OP operator.
*/
					if(rhs_nsubs!=0) {
						rhs_sel_ptr = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
						rhs_sel_ptr->n_entries = rhs_nsubs;
				
						for(i=0;i<rhs_nsubs;i++) {
							data =_NclPop();
							switch(data.u.sub_rec->sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/							
								ret = _NclBuildVSelection(rhs_var->u.data_var,data.u.sub_rec->u.vec,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							case INT_RANGE:
/*
* Need to free some stuff here
*/								
								ret = _NclBuildRSelection(rhs_var->u.data_var,data.u.sub_rec->u.range,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							case COORD_VECT:
							case COORD_RANGE:
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING) {
								status = NhlFATAL;
							}
						} 
					} else {
						rhs_sel_ptr = NULL;
					}
					if(lhs_nsubs !=0) {
						lhs_sel_ptr = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
						lhs_sel_ptr->n_entries = lhs_nsubs;
						for(i=0;i<lhs_nsubs;i++) {
							data =_NclPop();
							switch(data.u.sub_rec->sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/							
								ret = _NclBuildVSelection(lhs_var->u.data_var,data.u.sub_rec->u.vec,&(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),lhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							case INT_RANGE:
/*
* Need to free some stuff here
*/									
								ret = _NclBuildRSelection(lhs_var->u.data_var,data.u.sub_rec->u.range,&(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),lhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							case COORD_VECT:
							case COORD_RANGE:
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING) {
								status = NhlFATAL;
							}
						} 
					} else {
						lhs_sel_ptr = NULL;
					}
					ret = _NclAssignVarToVar(lhs_var->u.data_var,lhs_sel_ptr,rhs_var->u.data_var,rhs_sel_ptr);
					if(ret < NhlINFO) {
						status = ret;
					}
				}
				break;
			}
			default:
				break;
		}
		if(status < NhlINFO) {
			if(*fptr == NULL) {
				NhlPError(status,NhlEUNKNOWN,"Execute: Error occured at or near line %d\n",(cmd_line ? (*lptr)-1: *lptr));
			} else {
				NhlPError(status,NhlEUNKNOWN,"Execute: Error occured at or near line %d in file %s\n", *lptr, *fptr);
			}
			if(status < NhlWARNING) {
/*
* need to clean up stack !!! for current level
*/
				level--;
				return(Ncl_ERRORS);
			}
		}	
		status = NhlNOERROR;	
		ptr++;lptr++;fptr++;
	}
}

#ifdef __cplusplus
}
#endif



/*
 *      $Id: Execute.c,v 1.56 1996-04-17 23:56:55 ethan Exp $
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

#include "defs.h"
#include "Symbol.h"
#include "NclVar.h"
#include "Machine.h"
#include "NclFileInterfaces.h"
#include "NclFile.h"
#include "NclFileVar.h"
#include "NclHLUVar.h"
#include "FileSupport.h"
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclMdInc.h"
#include "OpsList.h"
#include "OpsFuncs.h"
#include "parser.h"
#include "NclAtt.h"
#include <errno.h>

extern int cmd_line;


NclExecuteReturnStatus _NclExecute
#if	NhlNeedProto
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
	NhlErrorTypes estatus = NhlNOERROR;
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
				int mask = (int)(Ncl_Typelong | Ncl_Typeint | Ncl_Typeshort); 

/*
* This is the first place that type checks on the vectors and range values can
* be done since it isn't until here that it is determined that normal integer
* subscripting is going on
*/
				data1.kind = NclStk_SUBREC;
				data1.u.sub_rec = (NclSubRec*)NclMalloc(
					sizeof(NclSubRec));
				data1.u.sub_rec->tolerence = -1;
				data = _NclPop();
				if(data.kind == NclStk_VECREC) {
					if(data.u.vec_rec->vec->multidval.type->type_class.type & mask ) {
						data1.u.sub_rec->sub_type = INT_VECT;
						data1.u.sub_rec->u.vec = data.u.vec_rec;
					} else{
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Vector subscripts must be integer");
						estatus = NhlFATAL;
					}
				} else if(data.kind == NclStk_RANGEREC) {
					if(((data.u.range_rec->start == NULL)
						|| (data.u.range_rec->start->multidval.type->type_class.type & mask)) &&
					((data.u.range_rec->finish == NULL)
						||(data.u.range_rec->finish->multidval.type->type_class.type & mask)) &&
					((data.u.range_rec->stride == NULL)
						||(data.u.range_rec->stride->multidval.type->type_class.type & mask))) {
						data1.u.sub_rec->sub_type = INT_RANGE;
						data1.u.sub_rec->u.range = data.u.range_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Subscripts must be integer when not using coordinate indexing");
						estatus = NhlFATAL;
					}
				}
				if(estatus != NhlFATAL) {
					if(*ptr == INT_SUBSCRIPT_OP) {
						data1.u.sub_rec->name = NULL;
					} else {
						data = _NclPop();
						switch(data.kind) {
						case NclStk_VAL: {
/*
* Taking for granted that syntax only allows string litterals here
*/
							data1.u.sub_rec->name = NrmQuarkToString(*((NclQuark*) data.u.data_obj->multidval.val));
							_NclDestroyObj((NclObj)data.u.data_obj);
							
							break;
						}
						default:	
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Illegal type for coordinate name in coordinate subscript ignoring value");
							data1.u.sub_rec->name = NULL;
							break;
						}
					}
					if(_NclPush(data1) == NhlFATAL)  {
						estatus = NhlFATAL;
					} else {
						if(estatus == NhlFATAL) 
							_NclCleanUpStack(1);
					}
				}
				break;
			}
			case DEFAULT_RANGE_OP : {
				NclStackEntry data;
				data.kind = NclStk_NOVAL;
				data.u.offset = 0;
				estatus = _NclPush(data);
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
/*
* These three values are destroyed later when processing the 
* subscripts. This however presents a problem when a variable has
* been used
*/
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
							estatus = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						data.u.range_rec->start = 
								_NclVarValueRead(start.u.data_var,NULL,NULL);
						if(data.u.range_rec->start == NULL) {
							estatus = NhlFATAL;
						}
						break;
					default:
						estatus = NhlFATAL;
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
							estatus = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						data.u.range_rec->finish= _NclVarValueRead(finish.u.data_var,NULL,NULL);
						if(data.u.range_rec->finish == NULL) {
							estatus = NhlFATAL;
						}
						break;
					default:
						estatus = NhlFATAL;
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
							estatus = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						data.u.range_rec->stride= _NclVarValueRead(stride.u.data_var,NULL,NULL);
						if(data.u.range_rec->stride == NULL){
							estatus = NhlFATAL;
						}
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
				}
				if((data.u.range_rec->start != NULL) &&
					(data.u.range_rec->start->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					estatus = NhlFATAL;
				}
				if((data.u.range_rec->finish != NULL) &&
					(data.u.range_rec->finish->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					estatus = NhlFATAL;
				}
				if((data.u.range_rec->stride != NULL) &&
					(data.u.range_rec->stride->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					estatus = NhlFATAL;
				}
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL)  {
					_NclCleanUpStack(1);
				}
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
						estatus = NhlFATAL;
					}
					break;
				case NclStk_VAL:
					if(data.u.data_obj != NULL) {
						val = data.u.data_obj;
					} else {
						estatus = NhlFATAL;
					}
					break;
				default:
					estatus = NhlFATAL;
				}
				if(estatus != NhlFATAL) {
					if(val->multidval.kind == SCALAR) {
						data1.kind = NclStk_RANGEREC;
						data1.u.range_rec = 
							(NclRangeRec*)NclMalloc(
							sizeof(NclRangeRec));
						data1.u.range_rec->start = val;
						data1.u.range_rec->finish = val;
						data1.u.range_rec->stride=NULL;
						estatus = _NclPush(data1);
					} else if(val->multidval.n_dims == 1) {
						data1.kind = NclStk_VECREC;
						data1.u.vec_rec =
							(NclVecRec*)NclMalloc(
							sizeof(NclVecRec));
						data1.u.vec_rec->vec = val;
						estatus = _NclPush(data1);
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Subscripts must be scalar or one dimensional vectors\n");
						estatus = NhlFATAL;
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
					level--;
					return(Ncl_ERRORS);
				} else {
					level--;
					return(Ncl_STOPS);
				}
			}
			case NAMED_COORD_SUBSCRIPT_OP : 
			case COORD_SUBSCRIPT_OP : {
				NclStackEntry data;
				NclStackEntry data1;
				int mask = (int)(Ncl_Typelong | Ncl_Typeint | Ncl_Typeshort); 

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
						||(data.u.range_rec->stride->multidval.type->type_class.type & mask))) {
						data1.u.sub_rec->sub_type = COORD_RANGE;
						data1.u.sub_rec->u.range = data.u.range_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. stride must always be integer regardless of whether coordinate or integer subscripting is being used\n");
						estatus = NhlFATAL;
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
						data1.u.sub_rec->name = NrmQuarkToString(*(NclQuark*) data.u.data_obj->multidval.val);
						
						_NclDestroyObj((NclObj)data.u.data_obj);
						break;
					}
					default:	
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Illegal type for coordinate name in coordinate subscript ignoring value");
						data1.u.sub_rec->name = NULL;
						break;
					}
				}
				if(_NclPush(data1) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
				break;
			} 
			case NEG_OP : {
				NclStackEntry data;
				NclStackEntry operand;
				operand = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclMonoOp(operand,&data,NEG_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL)  {
					_NclCleanUpStack(1);
				}
			}
			break;
			case NOT_OP : {
				NclStackEntry data;
				NclStackEntry operand;
				operand = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclMonoOp(operand,&data,NOT_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case MOD_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,MOD_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case OR_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,OR_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case AND_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,AND_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case XOR_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,XOR_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case LTSEL_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,LTSEL_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case GTSEL_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,GTSEL_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case PLUS_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus =  _NclDualOp(lhs,rhs,&data,PLUS_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
				break;
			case MINUS_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,MINUS_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
				break;
			case MUL_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,MUL_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
				break;
			case MAT_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,MAT_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
				break;
			case DIV_OP :
			{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,DIV_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
				break;
			case EXP_OP :{
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,EXP_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case LE_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,LE_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case GE_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,GE_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case GT_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,GT_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case LT_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,LT_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case EQ_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,EQ_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case NE_OP : {
				NclStackEntry data;
				NclStackEntry lhs;
				NclStackEntry rhs;
				rhs = _NclPop();
				lhs = _NclPop();
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclDualOp(lhs,rhs,&data,NE_OP);
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}
			break;
			case GET_OBJ_OP :
			{
				NclStackEntry obj_name;
				NclStackEntry res_name;
				NclStackEntry data_out;
				NclMultiDValData name;
				NclMultiDValData res;

				res_name = _NclPop();
				if(res_name.kind == NclStk_VAL) {
					res = res_name.u.data_obj;
				} else if(res_name.kind == NclStk_VAR) {
					res = _NclVarValueRead(res_name.u.data_var,NULL,NULL);
				}
				

				obj_name = _NclPop();
				if(obj_name.kind == NclStk_VAL) {
					name = obj_name.u.data_obj;
				} else if(obj_name.kind == NclStk_VAR) {
					name = _NclVarValueRead(obj_name.u.data_var,NULL,NULL);
				}

/*
* Guarenteed by grammar that res is reference to string object
*/
				data_out = _NclGetHLUObjOp(name,*(NclQuark*)res->multidval.val);
				if((data_out.kind != NclStk_NOVAL)&&(data_out.u.data_obj != NULL)){
					estatus =_NclPush(data_out);	
				} else {
					estatus = NhlFATAL;
				}

				if((res_name.kind == NclStk_VAL)&&(res_name.u.data_obj->obj.status != PERMANENT)) {
					_NclDestroyObj((NclObj)res_name.u.data_obj);
				} else if((res_name.kind == NclStk_VAR)&&(res_name.u.data_var->obj.status != PERMANENT)) {
					_NclDestroyObj((NclObj)res_name.u.data_var);
				}
				if((obj_name.kind == NclStk_VAL)&&(obj_name.u.data_var->obj.status != PERMANENT)) { 
					_NclDestroyObj((NclObj)obj_name.u.data_obj);
				} else if((obj_name.kind == NclStk_VAR)&&(obj_name.u.data_var->obj.status != PERMANENT)) {
					_NclDestroyObj((NclObj)obj_name.u.data_var);
				}
				
			}
			break;
/***************************
* One Operand Instructions *
***************************/
			case FUNC_CALL_OP: {
				NclSymbol *func = NULL;
				int caller_level;

				ptr++;lptr++;fptr++;
				func = (NclSymbol*)(*ptr);

				caller_level = _NclFinishFrame();
			/*
			* Doesn't leave anything on the stack if an error has occured
			*/	
				estatus = _NclFuncCallOp(func,caller_level);
				break;
			}
			case FPDEF:
				ptr++;lptr++;fptr++;
				break;
			case JMP:
			{
				unsigned long offset;
				ptr++;lptr++;fptr++;
				offset = *ptr;	
				ptr = machine + offset - 1;
				lptr = _NclGetCurrentLineRec() + offset - 1;
				fptr = _NclGetCurrentFileNameRec() + offset - 1;
				break;
			}
			case ARRAY_LIT_OP :
			{
				NclStackEntry data;
				ptr++;lptr++;fptr++;
				estatus = _NclBuildArray((int)*ptr,&data);
				if(estatus != NhlFATAL)
					estatus = _NclPush(data);
				break;
			}
			case PUSH_STRING_LIT_OP :
			{
				NclStackEntry data;
				NclQuark *thestr;
				int dim_size = 1;
			
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				thestr = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark));
				*thestr = *ptr;
				data.u.data_obj = _NclCreateMultiDVal(NULL,
						NULL,Ncl_MultiDValData,0,
						(void*)thestr,NULL,1,&dim_size,
						TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
				estatus  = _NclPush(data);
				break;
			}
			case PUSH_REAL_LIT_OP : 
			{
				NclStackEntry data;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclCreateMultiDVal(NULL,
						NULL,Ncl_MultiDValData,0,
						(void*)ptr,NULL,1,&dim_size,
						STATIC,NULL,(NclTypeClass)nclTypefloatClass);
				estatus = _NclPush(data);
				break;
			}
			case PUSH_LOGICAL_LIT_OP: {
				NclStackEntry data;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclCreateMultiDVal(NULL,
						NULL,Ncl_MultiDValData,0,
						(void*)ptr,NULL,1,&dim_size,
						STATIC,NULL,(NclTypeClass)nclTypelogicalClass);
				estatus = _NclPush(data);
				break;
			}
			case PUSH_INT_LIT_OP :
			{
				NclStackEntry data;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclCreateMultiDVal(NULL,
						NULL,Ncl_MultiDValData,0,
						(void*)ptr,NULL,1,&dim_size,
						STATIC,NULL,(NclTypeClass)nclTypeintClass);
				estatus = _NclPush(data);
				break;
			}
			case PUSH_LOG_LIT_OP :
			{
				NclStackEntry data;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclCreateMultiDVal(NULL,
						NULL,Ncl_MultiDValData,0,
						(void*)ptr,NULL,1,&dim_size,
						STATIC,NULL,(NclTypeClass)nclTypelogicalClass);
				estatus = _NclPush(data);
				break;
			}
			case JMP_SCALAR_TRUE_OP: {
				NclStackEntry data;
				NclMultiDValData val;
				unsigned long offset;

				ptr++;lptr++;fptr++;
				offset = *ptr;
				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAL:
					val = data.u.data_obj;	
					break;
				case NclStk_VAR:
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
					break;
				default:
					estatus = NhlFATAL;
					break;
				}
				
				if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
					if(!_NclIsMissing(val,val->multidval.val)) {
						if((*(logical*)val->multidval.val)) {
							machine = _NclGetCurrentMachine();
							ptr = machine + offset - 1;
							lptr = _NclGetCurrentLineRec() + offset - 1;
							fptr = _NclGetCurrentFileNameRec() + offset - 1;
						}
					} 
				} 
				if(estatus != NhlFATAL) {
					estatus =  _NclPush(data);
				}
				break;
			}
			case JMP_SCALAR_FALSE_OP: {
				NclStackEntry data;
				NclMultiDValData val;
				unsigned long offset;

				ptr++;lptr++;fptr++;
				offset = *ptr;
				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAL:
					val = data.u.data_obj;	
					break;
				case NclStk_VAR:
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
					break;
				default:
					estatus = NhlFATAL;
					break;
				}
				
				if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
					if(!_NclIsMissing(val,val->multidval.val)) {
						if(!(*(logical*)val->multidval.val)) {
							machine = _NclGetCurrentMachine();
							ptr = machine + offset - 1;
							lptr = _NclGetCurrentLineRec() + offset - 1;
							fptr = _NclGetCurrentFileNameRec() + offset - 1;
						}
					} 
				}  
				if(estatus != NhlFATAL) {
					estatus = _NclPush(data);
				}
				break;
			}
			case JMPFALSE : {
				NclStackEntry data;
				NclMultiDValData val;
				unsigned long offset;

				ptr++;lptr++;fptr++;
				offset = *ptr;
				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAL:
					val = data.u.data_obj;	
					break;
				case NclStk_VAR:
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
					break;
				default:
					estatus = NhlFATAL;
					break;
				}
				
				if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
					if(!_NclIsMissing(val,val->multidval.val)) {
						if(!(*(logical*)val->multidval.val)) {
							machine = _NclGetCurrentMachine();
							ptr = machine + offset - 1;
							lptr = _NclGetCurrentLineRec() + offset - 1;
							fptr = _NclGetCurrentFileNameRec() + offset - 1;
						}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"If: the result of the conditional expression yields a missing value can not determine branch, see ismissing and clearmissing functions");
						estatus = NhlFATAL;
					}
					if(val->obj.status != PERMANENT) 
						_NclDestroyObj((NclObj)val);
				} else {
					if(val->obj.status != PERMANENT) 
						_NclDestroyObj((NclObj)val);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"If: if statments require SCALAR logical values");
					estatus = NhlFATAL;
				}
				break;
			}
			case SET_OBJ_OP : {
				NclStackEntry data;
				NclMultiDValData val;
				int nres;

				data = _NclPop();
				if(data.kind == NclStk_VAL) {
					val = data.u.data_obj;
				} else if(data.kind == NclStk_VAR) {
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
				}
				ptr++;lptr++;fptr++;
				nres = (int)*ptr;
				if(val == NULL) {
					_NclCleanUpStack(2*nres);
					estatus = NhlFATAL;
				} else {
					estatus = _NclSetHLUObjOp(val,nres);
				}
			}
				break;
			case PROC_CALL_OP:{
				NclSymbol *proc = NULL;
				int caller_level;

				ptr++;lptr++;fptr++;
				proc = (NclSymbol*)(*ptr);
			
				caller_level = _NclFinishFrame();	
				estatus = _NclProcCallOp(proc,caller_level);
			}
				break;
			case BPROC_CALL_OP:
				ptr++;lptr++;fptr++;
				break;
			case INTRINSIC_FUNC_CALL:
			{
				int i;
				NclFrame *previous_fp;
				int caller_level;
				NhlErrorTypes ret = NhlNOERROR;
				ptr++;lptr++;fptr++;
				
	
/*
* This is not going to work because nothing is done to unpack the
* arguments they are just popped now!!!!!!
* 5/20 this was taken care of by modifiying the CONVERT_TO_LOCAL operator.
*/
				caller_level = _NclFinishFrame();	
				if(((NclSymbol*)*ptr)->u.bfunc != NULL) {
					ret = (*((NclSymbol*)*ptr)->u.bfunc->thefunc)();
/*
* should actually map values back
*/
					if(ret < NhlWARNING) {
						estatus = ret;
					}
					if(((NclSymbol*)*ptr)->u.bfunc->thescope != NULL) {
						_NclPopScope();
					}
					previous_fp = _NclLeaveFrame(caller_level);
					_NclRemapIntrParameters(((NclSymbol*)*ptr)->u.bfunc->nargs,
							previous_fp,INTRINSIC_FUNC_CALL);
					 _NclPopFrame(INTRINSIC_FUNC_CALL);


/*
					for(i = 0;i<((NclSymbol*)*ptr)->u.bfunc->nargs; i++) {
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
*/
				} else {
					(void)_NclLeaveFrame(caller_level);
				}
				ptr++;lptr++;fptr++;
			}
				break;
			case INTRINSIC_PROC_CALL:
			{
				int i;
				NclFrame *previous_fp;
				int caller_level;
				NhlErrorTypes ret = NhlNOERROR;
				ptr++;lptr++;fptr++;
/*
* This is not going to work because nothing is done to unpack the
* arguments they are just popped now!!!!!!
* 5/20 this was taken care of by modifiying the CONVERT_TO_LOCAL operator.
*/
				caller_level = _NclFinishFrame();	
				if(((NclSymbol*)*ptr)->u.bproc != NULL) {
					ret = (*((NclSymbol*)*ptr)->u.bproc->theproc)();
					if(ret < NhlWARNING) {
						estatus = ret;
					}
/*
* should actually map values back
*/
					if(((NclSymbol*)*ptr)->u.bproc->thescope != NULL) {
						_NclPopScope();
					}
					previous_fp = _NclLeaveFrame(caller_level);
					_NclRemapIntrParameters(((NclSymbol*)*ptr)->u.bfunc->nargs,
							previous_fp,INTRINSIC_PROC_CALL);
					_NclPopFrame(INTRINSIC_PROC_CALL);

/*
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
*/
				} else {
					(void)_NclLeaveFrame(caller_level);
				}
				ptr++;lptr++;fptr++;
			}
				break;
			case BFUNC_CALL_OP:
				ptr++;lptr++;fptr++;
				break;
			case DUP_TOFS: {
				NclStackEntry data;
				NclStackEntry data_dup;
				
				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAL:
					data_dup.kind = data.kind;
					data_dup.u.data_obj = _NclCopyVal(data.u.data_obj,NULL);
					estatus = _NclPush(data);
					if(estatus != NhlFATAL)
						estatus = _NclPush(data_dup);
					break;
				case NclStk_VAR:
					data_dup.kind = data.kind;
					data_dup.u.data_var = _NclCopyVar(data.u.data_var,NULL,NULL);
					estatus = _NclPush(data);
					if(estatus != NhlFATAL)
						estatus = _NclPush(data_dup);
					break;
				default:
					estatus = NhlFATAL;
					break;
				}
				break;
			}
			case DO_FROM_TO_OP : {
				NclStackEntry data;
				NclStackEntry* data_ptr;
				unsigned long jmp_off;
				NclExecuteReturnStatus rtst;
				int done = 0;
				NclMultiDValData val;
				NclSymbol *end_sym;
				NclSymbol *inc_sym;
				NclSymbol *dir_sym;
				NclMultiDValData tmp_md = NULL;
				NclMultiDValData tmp2_md = NULL;

				

				ptr++;lptr++;fptr++;
				jmp_off = *ptr;
                                ptr++,lptr++,fptr++;
                                end_sym = (NclSymbol*)*ptr;
                                ptr++,lptr++,fptr++;
                                dir_sym = (NclSymbol*)*ptr;
                                ptr++,lptr++,fptr++;
                                inc_sym = (NclSymbol*)*ptr;
                                data_ptr = _NclRetrieveRec(inc_sym,DONT_CARE);
				tmp2_md = _NclVarValueRead(data_ptr->u.data_var,NULL,NULL);
				if(!(tmp2_md->multidval.type->type_class.type & Ncl_Typelong)) {
                                	tmp_md = _NclCoerceData(tmp2_md,Ncl_Typelong,NULL);
				} else {
					tmp_md = tmp2_md;
				}
				if(tmp_md != NULL) {
					if(!((*(long*)tmp_md->multidval.val) > 0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"DO: Stride value is less than 1, stide must be positive");
						estatus = NhlFATAL;
					}
					if(tmp_md != tmp2_md) {
						_NclDestroyObj((NclObj)tmp_md);
					}
				}
				if(estatus != NhlFATAL) {	
					data = _NclPop();
					if(data.kind == NclStk_VAL) {
						switch(data.kind) {
						case NclStk_VAL:
							val = data.u.data_obj;
							break;
						case NclStk_VAR:
							val = _NclVarValueRead(data.u.data_var,NULL,NULL);
							break;
						default:
							estatus = NhlFATAL;
							break;
						}
						if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
							if(!*(logical*)val->multidval.val) {
								done = 1;
							}
						} 
						switch(data.kind) {
						case NclStk_VAL:
							if(data.u.data_obj->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)data.u.data_obj);
							}
							break;
						case NclStk_VAR:
							if(data.u.data_var->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)data.u.data_var);
							}
							break;
						default:
							break;
						}
						while(!done) {
							rtst = _NclExecute(jmp_off);
							if((rtst != Ncl_ERRORS)&&(rtst != Ncl_BREAKS)) {
								data = _NclPop();
								switch(data.kind) {
								case NclStk_VAL:
									val = data.u.data_obj;
								break;
								case NclStk_VAR:
									val = _NclVarValueRead(data.u.data_var,NULL,NULL);
								break;
								default:
									estatus = NhlFATAL;
								break;
								}
								if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
									if(!*(logical*)val->multidval.val) {
										done = 1;
									}
								} 
								switch(data.kind) {
								case NclStk_VAL:
									if(data.u.data_obj->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)data.u.data_obj);
									}
									break;
								case NclStk_VAR:
									if(data.u.data_var->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)data.u.data_var);
									}
									break;
								default:
									break;
								}
							} else if(rtst == Ncl_BREAKS){
								done = 1;
								estatus = NhlNOERROR;
							} else {
								done = 1;
								estatus = NhlFATAL;
							}
						}
					}
				}

                                data_ptr = _NclRetrieveRec(end_sym,DONT_CARE);
                                (void)_NclChangeSymbolType(end_sym,UNDEF);
                                _NclDestroyObj((NclObj)data_ptr->u.data_var);
                                data_ptr->kind = NclStk_NOVAL;
                                data_ptr->u.data_var = NULL;

                                data_ptr = _NclRetrieveRec(dir_sym,DONT_CARE);
                                (void)_NclChangeSymbolType(dir_sym,UNDEF);
                                _NclDestroyObj((NclObj)data_ptr->u.data_var);
                                data_ptr->kind = NclStk_NOVAL;
                                data_ptr->u.data_var = NULL;
                                data_ptr = _NclRetrieveRec(inc_sym,DONT_CARE);
                                (void)_NclChangeSymbolType(inc_sym,UNDEF);
                                _NclDestroyObj((NclObj)data_ptr->u.data_var);
                                data_ptr->kind = NclStk_NOVAL;
                                data_ptr->u.data_var = NULL;

			}
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
				unsigned int valid_dims = ((int)Ncl_Typelong 
					| (int)Ncl_Typeint 
					| (int)Ncl_Typeshort);

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
		
				var = _NclRetrieveRec(thesym,READ_IT);

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
					estatus = NhlFATAL;
					break;
				}
				if((data_md != NULL)&&(data_md->multidval.type->type_class.type & valid_dims)&&(data_md->multidval.kind == SCALAR)&&(var!= NULL)&&(var->u.data_var != NULL)) {	
					if(!(data_md->multidval.type->type_class.type & Ncl_Typelong)) {
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
						estatus = NhlFATAL;
					} else {
						data.kind = NclStk_VAL;
						estatus = _NclPush(data);
					}
				} else {
					if((data_md != NULL)&&!(data_md->multidval.type->type_class.type & valid_dims)){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce ref dim to long");
					} else if((data_md != NULL)&&(data_md->multidval.kind != SCALAR)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Reference dims must be scalar");
					} else if(var == NULL) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined",thesym->name);
					}
			
					if((data_md != NULL)&&(data_md->obj.status != PERMANENT)) {
						_NclDestroyObj((NclObj)data_md);
					}	
					estatus = NhlFATAL;
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
				unsigned int valid_dims = (unsigned int)(Ncl_Typelong 
					| Ncl_Typeint 
					| Ncl_Typeshort);
				unsigned int valid_expr = (unsigned int)(Ncl_Typestring 
					| Ncl_Typechar);

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
				data_var =  _NclRetrieveRec(thesym,WRITE_IT);
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
					&&(dim_expr_md->multidval.type->type_class.type & valid_expr)
					&&(dim_ref_md->multidval.type->type_class.type & valid_dims)
					&&(dim_expr_md->multidval.kind == SCALAR)
					&&(dim_ref_md->multidval.kind == SCALAR)) {
					if((dim_expr_md->multidval.data_type != NCL_string)) {
						_NclScalarCoerce(
							(void*)dim_expr_md->multidval.val,
							dim_expr_md->multidval.data_type,
							(void*)&dim_name,
							NCL_long);
							
					} else {
						dim_name = NrmQuarkToString(*(string*)dim_expr_md->multidval.val);
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

					if(estatus != NhlFATAL) {
					estatus = _NclWriteDim(
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
					if((data_var != NULL)&&(data_var->u.data_var != NULL)) {
						if((dim_expr_md != NULL) &&!(dim_expr_md->multidval.type->type_class.type & valid_expr)) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Right hand side of dimension expression must be a string or character result");
						} else if((dim_ref_md != NULL) &&!(dim_ref_md->multidval.type->type_class.type & valid_dims)) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce ref dim to long");
						} else if((dim_expr_md != NULL) &&dim_expr_md->multidval.kind != SCALAR) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Right hand side of dimension expression must be SCALAR");
						} else if((dim_ref_md != NULL) &&dim_ref_md->multidval.kind != SCALAR) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Reference dimension expression must be SCALAR");
						}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined",thesym->name);
					}
					estatus = NhlFATAL;
				}
			}
			break;
			case NEW_WM_OP:
			case NEW_OP:
			{	
				NclStackEntry size_expr;
				NclStackEntry missing_expr;
				NclSymbol *data_type;

				if(*ptr == NEW_WM_OP) {
					missing_expr = _NclPop();
				} else {
					missing_expr.kind = NclStk_NOVAL;
					missing_expr.u.data_obj = NULL;
				}
				size_expr = _NclPop();
				ptr++; lptr++; fptr++;
				data_type = (NclSymbol*)*ptr;
				estatus = _NclNewOp(data_type,size_expr,missing_expr);
				switch(size_expr.kind) {
				case NclStk_VAL:
					if(size_expr.u.data_obj->obj.status != PERMANENT) {	
						_NclDestroyObj((NclObj)size_expr.u.data_obj);
					}
					break;
				case NclStk_VAR:
					if(size_expr.u.data_var->obj.status != PERMANENT) {	
						_NclDestroyObj((NclObj)size_expr.u.data_var);
					}
					break;
				default:
					break;
				}
				
				break;
			}
			case ISDEFINED_OP: {
				NclStackEntry* var;
				NclSymbol *var_sym;


				ptr++;lptr++;fptr++;
				var_sym = (NclSymbol*)*ptr;
				var = _NclRetrieveRec(var_sym,DONT_CARE);
				if((var== NULL) || (var->kind == NclStk_NOVAL)|| (var->u.data_var == NULL)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Undefined indentifier: (%s) is undefined, can't continue",var_sym->name);
					estatus = NhlFATAL;
				}
				break;
			}
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
				var = _NclRetrieveRec(sym,READ_IT);
				ptr++;lptr++;fptr++;
				nsubs = *ptr;
				if(var->u.data_var == NULL) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined",sym->name);
					_NclCleanUpStack(nsubs);
					estatus = NhlFATAL;
				} else if(nsubs == 0) {
					if(var != NULL) {
						estatus = _NclPush(*var);
					} else {
						estatus = NhlFATAL;
					}
				} else if(nsubs != var->u.data_var->var.n_dims) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimesions of variable,(%d) Subscripts used, (%d) Subscripts expected",nsubs,var->u.data_var->var.n_dims);
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
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
							ret = _NclBuildCoordVSelection(var->u.data_var,data.u.sub_rec->u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
							break;
						case COORD_RANGE:
							ret = _NclBuildCoordRSelection(var->u.data_var,data.u.sub_rec->u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING) {
							estatus = NhlFATAL;
							break;
						}
						if(!dim_is_ref[(sel_ptr->selection[nsubs - i - 1]).dim_num]) {
							dim_is_ref[(sel_ptr->selection[nsubs - i - 1]).dim_num] = 1;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in subscript # %d,dimension is referenced more than once",i);
							estatus = NhlFATAL;
						}
					} 
					if(estatus != NhlFATAL) {
						data1.kind = NclStk_VAR;
						data1.u.data_var = _NclVarRead(var->u.data_var,sel_ptr);
						if(sel_ptr != NULL) {
							NclFree(sel_ptr);
						}
						if(data1.u.data_var != NULL) {
							estatus = _NclPush(data1);
						} else {
							estatus = NhlFATAL;
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

			lhs_var = _NclRetrieveRec(sym,WRITE_IT);
			if((estatus != NhlFATAL)&&(lhs_var != NULL)) {
				if(lhs_var->kind == NclStk_NOVAL) {
					if(nsubs != 0) {
						estatus = NhlFATAL;
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Assign: %s is undefined, can not subscript an undefined variable",sym->name);
						estatus = NhlFATAL;
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
								if(rhs_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
									lhs_var->u.data_var= _NclFileVarCreate(NULL,NULL,Ncl_FileVar,0,sym,rhs_md,NULL,-1,NULL,NORMAL,sym->name,PERMANENT);
								} else if(rhs_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData ) {
									lhs_var->u.data_var= _NclHLUVarCreate(NULL,NULL,Ncl_HLUVar,0,sym,rhs_md,NULL,-1,NULL,NORMAL,sym->name,PERMANENT);
								} else {
									lhs_var->u.data_var= _NclVarCreate(NULL,NULL,Ncl_Var,0,sym,rhs_md,NULL,-1,NULL,NORMAL,sym->name,PERMANENT);
								}
								if(lhs_var->u.data_var != NULL) {
									(void)_NclChangeSymbolType(sym,VAR);
									lhs_var->kind = NclStk_VAR;
								} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
									estatus = NhlWARNING;
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
								if(rhs_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) { 
									lhs_var->u.data_var= _NclFileVarCreate(NULL,NULL,Ncl_FileVar,0,sym,rhs_md,rhs.u.data_var->var.dim_info,rhs.u.data_var->var.att_id,rhs.u.data_var->var.coord_vars,NORMAL,sym->name,PERMANENT);
								} else if(rhs_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData ) {
									lhs_var->u.data_var= _NclHLUVarCreate(NULL,NULL,Ncl_HLUVar,0,sym,rhs_md,rhs.u.data_var->var.dim_info,rhs.u.data_var->var.att_id,rhs.u.data_var->var.coord_vars,NORMAL,sym->name,PERMANENT);
								} else {
									lhs_var->u.data_var= _NclVarCreate(NULL,NULL,Ncl_Var,0,sym,rhs_md,rhs.u.data_var->var.dim_info,rhs.u.data_var->var.att_id,rhs.u.data_var->var.coord_vars,NORMAL,sym->name,PERMANENT);
								}
								if(lhs_var->u.data_var != NULL) {
									(void)_NclChangeSymbolType(sym,VAR);
									lhs_var->kind = NclStk_VAR;
								} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
									estatus = NhlWARNING;
									lhs_var->kind = NclStk_NOVAL;
								}
							} else {
								estatus = NhlFATAL;
							} 
							if(rhs.u.data_var->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)rhs.u.data_var);
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal right-hand side type for assignment");
							estatus = NhlFATAL;
						}
					}
				} else if(lhs_var->kind == NclStk_VAR) {
					if((nsubs != lhs_var->u.data_var->var.n_dims)&&(nsubs != 0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts (%d) and number of dimensions (%d) do not match for variable (%s)",nsubs,lhs_var->u.data_var->var.n_dims,sym->name);
						estatus = NhlFATAL;
						_NclCleanUpStack(nsubs+1);
					}
					if(nsubs != 0) {
						sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
						sel_ptr->n_entries = nsubs;
					} else {
						sel_ptr = NULL;
					}
					if(estatus != NhlFATAL) {
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
								ret = _NclBuildCoordVSelection(lhs_var->u.data_var,data.u.sub_rec->u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
								break;
							case COORD_RANGE:
								ret = _NclBuildCoordRSelection(lhs_var->u.data_var,data.u.sub_rec->u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING) {
								estatus = NhlFATAL;
								break;
							}
						}
					}
					rhs = _NclPop();	
					if(estatus != NhlFATAL) {
						if(rhs.kind == NclStk_VAL) {
							rhs_md = rhs.u.data_obj;
							if(rhs_md != NULL) {
								ret = _NclAssignToVar(lhs_var->u.data_var,rhs_md,sel_ptr);
								if(sel_ptr != NULL) {
									NclFree(sel_ptr);
								}
								if(rhs_md->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)rhs_md);
								}
								if(ret <= NhlWARNING) {
									estatus = ret;
								}
							} else {
								estatus = NhlFATAL;
							}
						} else if(rhs.kind == NclStk_VAR) {
/*
* I don't pass in a new missing in this situation because
* _NclAssignToVar checks the missing values and it has
* to visit each element anyways
*/
							estatus = _NclAssignVarToVar(lhs_var->u.data_var,sel_ptr,rhs.u.data_var,NULL);
							if(sel_ptr != NULL) {
								NclFree(sel_ptr);
							}
							if(rhs.u.data_var->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)rhs.u.data_var);
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal right-hand side type for assignment");
							estatus = NhlFATAL;
						}
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Assignment not supported for left-hand type");
					estatus = NhlFATAL;
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
				if((proc->u.procfunc != NULL)&&(offset >= 0)) {
					if(proc->u.procfunc->thescope != NULL) {	
						_NclPushScope(proc->u.procfunc->thescope);
					}
					estatus = _NclPushFrame(proc,offset);
				} else {
					estatus = NhlFATAL;
				}
				break;
			}
			case CONVERT_TO_LOCAL: {
				NclSymbol *thesym = NULL;
				NclGenProcFuncInfo *pfinfo = NULL;
				NclSymbol *argsym = NULL;
				NclStackEntry data,tmp_data;
				unsigned int obj_type_param;
				unsigned int obj_type_arg;
				NclMultiDValData tmp_md = NULL;
				NclVar tmp_var = NULL;
				int i;
				int arg_num = -1;
				unsigned int except_mds = ((int)Ncl_MultiDValHLUObjData | (int)Ncl_MultiDValnclfileData);

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				arg_num = (int)(*ptr);

				switch(thesym->type) {
				case IPROC:
				case PIPROC:
				case IFUNC:
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
					estatus = NhlFATAL;
				} else if(arg_num >= pfinfo->nargs) {
					estatus = NhlFATAL;
				} else {
/*
*---> Need to look into allowing HLU objects to be used as parameters
* in which case this will not be enough also files <----
*/
/*
* Check dimensions first since it isn't expensive
*/		
					data = _NclPop();
					switch(data.kind) {
					case NclStk_VAR: {
						if(pfinfo->theargs[arg_num].arg_data_type != NULL) {
							obj_type_arg = _NclKeywordToObjType(pfinfo->theargs[arg_num].arg_data_type);
							if(obj_type_arg == Ncl_Obj) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal type for argument in argument (%d) of (%s)",arg_num,thesym->name);
								estatus = NhlFATAL;
							}
							if(obj_type_arg & except_mds) {
								if((obj_type_arg & Ncl_MultiDValHLUObjData)&&!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
                                                                	NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
                                                                	estatus = NhlFATAL;
								}
								if((obj_type_arg & Ncl_MultiDValnclfileData)&&!(data.u.data_var->obj.obj_type_mask & Ncl_FileVar)) {
                                                                	NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
                                                                	estatus = NhlFATAL;
								}
								obj_type_param = obj_type_arg = Ncl_Typeobj;
							} else {
                                               		 	obj_type_param = _NclGetVarRepValue(data.u.data_var);
							}
						} else {
                                                	obj_type_arg = obj_type_param = _NclGetVarRepValue(data.u.data_var);
						}

						if(pfinfo->theargs[arg_num].is_dimsizes) {
							if(pfinfo->theargs[arg_num].n_dims != data.u.data_var->var.n_dims) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) does not match specification",arg_num,thesym->name);
								estatus = NhlFATAL;

							} else {
								for(i = 0; i< pfinfo->theargs[arg_num].n_dims; i++) {
									if(pfinfo->theargs[arg_num].dim_sizes[i] != -1) {
										if(pfinfo->theargs[arg_num].dim_sizes[i] != data.u.data_var->var.dim_info[i].dim_size) {
											NhlPError(NhlFATAL,NhlEUNKNOWN,"Size of dimension (%d) of argument (%d) does not match specification in (%s) function definition",i,arg_num,thesym->name);
											estatus = NhlFATAL;
										}
									}
									if(estatus == NhlFATAL) {
										break;
									} else {
										i++;
									}
								}
							}
						}
						if(estatus != NhlFATAL) {
                                                	if(!(obj_type_param & obj_type_arg)){
                                                        	tmp_md = _NclCoerceVar(data.u.data_var,obj_type_arg,NULL);
                                                        	if(tmp_md == NULL) {
                                                                	NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
                                                                	estatus = NhlFATAL;
                                                        	} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Argument %d of the current function or procedure was coerced to the appropriate type and thus will not change if the function or procedure modifies its value",arg_num);
									estatus = NhlWARNING;
								}
/*
* Attention: missing value may be different type than variable data until code is put here to fix it
*/
								if(data.u.data_var->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)data.u.data_var);
								}
							} else {
								tmp_md = NULL;
							}
							if(estatus != NhlFATAL) 
                                                		_NclAddObjToParamList((NclObj)data.u.data_var,arg_num);
							if((thesym->type != IPROC)&&(thesym->type != PIPROC) && (thesym->type != IFUNC)&&(estatus != NhlFATAL)) {
/*
* Variable subsections also point to the symbol of the main variable so the AddObjToParamList just
* stores the symbol rather than the pointer to variable record
*/
                                                		argsym = pfinfo->theargs[arg_num].arg_sym;
								if(tmp_md != NULL) {
									if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
										tmp_var = _NclFileVarCreate(NULL,data.u.data_var->obj.class_ptr,
                                                                                	Ncl_FileVar,
                                                                                	0,
                                                                                	argsym,
                                                                                	tmp_md,
                                                                                	data.u.data_var->var.dim_info,
                                                                                	data.u.data_var->var.att_id,
                                                                                	data.u.data_var->var.coord_vars,
                                                                                	PARAM,
                                                                                	argsym->name,PERMANENT);
									} else if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData ) {
										tmp_var = _NclHLUVarCreate(NULL,data.u.data_var->obj.class_ptr,
                                                                                	Ncl_HLUVar,
                                                                                	0,
                                                                                	argsym,
                                                                                	tmp_md,
                                                                                	data.u.data_var->var.dim_info,
                                                                                	data.u.data_var->var.att_id,
                                                                                	data.u.data_var->var.coord_vars,
                                                                                	PARAM,
                                                                                	argsym->name,PERMANENT);
									} else {
										tmp_var = _NclVarCreate(NULL,data.u.data_var->obj.class_ptr,
                                                                                	Ncl_Var,
                                                                                	0,
                                                                                	argsym,
                                                                                	tmp_md,
                                                                                	data.u.data_var->var.dim_info,
                                                                                	data.u.data_var->var.att_id,
                                                                                	data.u.data_var->var.coord_vars,
                                                                                	PARAM,
                                                                                	argsym->name,PERMANENT);
									}
                                                        	} else {
									tmp_md = (NclMultiDValData)_NclGetObj(data.u.data_var->var.thevalue_id);
									if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
                                                                		tmp_var = _NclFileVarCreate(NULL,data.u.data_var->obj.class_ptr,
                                                                                	data.u.data_var->obj.obj_type,
                                                                                	data.u.data_var->obj.obj_type_mask,
                                                                                	argsym,
                                                                                	tmp_md,
                                                                                	data.u.data_var->var.dim_info,
                                                                                	data.u.data_var->var.att_id,
                                                                                	data.u.data_var->var.coord_vars,
                                                                                	PARAM,
                                                                                	argsym->name,PERMANENT);
									} else if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
                                                                		tmp_var = _NclHLUVarCreate(NULL,data.u.data_var->obj.class_ptr,
                                                                                	data.u.data_var->obj.obj_type,
                                                                                	data.u.data_var->obj.obj_type_mask,
                                                                                	argsym,
                                                                                	tmp_md,
                                                                                	data.u.data_var->var.dim_info,
                                                                                	data.u.data_var->var.att_id,
                                                                                	data.u.data_var->var.coord_vars,
                                                                                	PARAM,
                                                                                	argsym->name,PERMANENT);
									} else {
                                                                		tmp_var = _NclVarCreate(NULL,data.u.data_var->obj.class_ptr,
                                                                                	data.u.data_var->obj.obj_type,
                                                                                	data.u.data_var->obj.obj_type_mask,
                                                                                	argsym,
                                                                                	tmp_md,
                                                                                	data.u.data_var->var.dim_info,
                                                                                	data.u.data_var->var.att_id,
                                                                                	data.u.data_var->var.coord_vars,
                                                                                	PARAM,
                                                                                	argsym->name,PERMANENT);
									}
                                                        	}
                                                		if(estatus != NhlFATAL) {
                                                        		data.kind = NclStk_VAR;
                                                        		data.u.data_var = tmp_var;
                                                        		estatus = _NclPush(data);
                                                		}
							} else if(estatus != NhlFATAL){
								if(tmp_md != NULL){
									tmp_data.kind = NclStk_VAL;
									tmp_data.u.data_obj = tmp_md;
									estatus = _NclPush(tmp_data);
								} else {
									estatus = _NclPush(data);
								}
							}
						}
					}
					break;
					case NclStk_VAL: {
						if(pfinfo->theargs[arg_num].arg_data_type != NULL) {
							obj_type_arg = _NclKeywordToObjType(pfinfo->theargs[arg_num].arg_data_type);
							if(obj_type_arg == Ncl_Obj) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal type for argument in argument (%d) of (%s)",arg_num,thesym->name);
								estatus = NhlFATAL;
							}
							if(obj_type_arg & except_mds) {
								if(!(data.u.data_obj->obj.obj_type_mask & obj_type_arg)) {
                                                        		NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
									estatus = NhlFATAL;
								}
                                                		obj_type_arg = obj_type_param = Ncl_Typeobj;
							} else {
                                                		obj_type_param =((NclMultiDValData)data.u.data_obj)->multidval.type->type_class.type;
							}
						} else {
                                                	obj_type_arg = obj_type_param =((NclMultiDValData)data.u.data_obj)->multidval.type->type_class.type;
						}
						if(estatus != NhlFATAL) {
							if(pfinfo->theargs[arg_num].is_dimsizes) {
								if(pfinfo->theargs[arg_num].n_dims != data.u.data_obj->multidval.n_dims) {
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) does not match specification",arg_num,thesym->name);
									estatus = NhlFATAL;
	
								} else {
									for(i = 0; i< pfinfo->theargs->n_dims; i++) {
										if(pfinfo->theargs->dim_sizes[i] != -1) {
											if(pfinfo->theargs->dim_sizes[i] != data.u.data_obj->multidval.dim_sizes[i]) {
												NhlPError(NhlFATAL,NhlEUNKNOWN,"Size of dimension (%d) of argument (%d) does not match specification in (%s) function definition",i,arg_num,thesym->name);
												estatus = NhlFATAL;
											}
										}
										if(estatus == NhlFATAL) {
											break;
										} else {
											i++;
										}
									}
								}
							}
                                                	if(!(obj_type_param & obj_type_arg)){
                                                		tmp_md = _NclCoerceData(data.u.data_obj,obj_type_arg,NULL);
                                                        	if(tmp_md == NULL) {
                                                        		NhlPError(NhlFATAL,NhlEUNKNOWN,"Argument type mismatch on argument (%d) of (%s) can not coerce",arg_num,thesym->name);
                                                        		estatus = NhlFATAL;
									if(data.u.data_obj->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)data.u.data_obj);
									}
                                                        	} else {
									if(data.u.data_obj->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)data.u.data_obj);
									}
									data.u.data_obj = tmp_md;
									data.kind = NclStk_VAL;
								}
	/*
	* Attention: missing value may be different type than variable data until code is put here to fix it
	*/
							} else {
								tmp_md = data.u.data_obj;
							}
							if((thesym->type != IPROC)&&(thesym->type != PIPROC)&&(thesym->type != IFUNC)&&(estatus != NhlFATAL)) {
								argsym = pfinfo->theargs[arg_num].arg_sym;
								_NclAddObjToParamList((NclObj)data.u.data_obj,arg_num);
                                                		if(estatus != NhlFATAL) {
	
									if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
                                                        			tmp_var = _NclFileVarCreate(
											NULL,NULL,
                                                                			Ncl_FileVar,
                                                                			0,
                                                                			argsym,
                                                                			tmp_md,
                                                                			NULL,
                                                                			-1,
                                                                			NULL,
                                                                			PARAM,
                                                                			argsym->name,PERMANENT);
									} else if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
                                                        			tmp_var = _NclHLUVarCreate(
											NULL,NULL,
                                                                			Ncl_HLUVar,
                                                                			0,
                                                                			argsym,
                                                                			tmp_md,
                                                                			NULL,
                                                                			-1,
                                                                			NULL,
                                                                			PARAM,
                                                                			argsym->name,PERMANENT);
									} else {
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
                                                                			argsym->name,PERMANENT);
									}
									if(tmp_md->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)tmp_md);
									}
	/*
	* Need to put ancestor of local variable in the parmeter list so it can be unpacked later
	*/
	
                                                		}
                                                		if(estatus != NhlFATAL) {
                                                        		data.kind = NclStk_VAR;
                                                        		data.u.data_var = tmp_var;
                                                        		estatus = _NclPush(data);
                                                		}
							} else if(estatus != NhlFATAL) {
								argsym = pfinfo->theargs[arg_num].arg_sym;
								_NclAddObjToParamList((NclObj)data.u.data_obj,arg_num);

                                                       		estatus = _NclPush(data);
							}
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
			case DO_WHILE_OP : {
				unsigned long cond_off;
				unsigned long block_off;
				int done = 0;
				NclStackEntry data;
				NclMultiDValData val,tmp_md;
				NclExecuteReturnStatus rtst;

				ptr++;lptr++;fptr++;
				cond_off = *ptr;
				ptr++;lptr++;fptr++;
				block_off = *ptr;
				while( !done) {
					rtst = _NclExecute(cond_off);
					if(rtst != Ncl_ERRORS) {
						data = _NclPop();
						switch(data.kind) {
						case NclStk_VAL:
							val = data.u.data_obj;
						break;
						case NclStk_VAR:
							val = _NclVarValueRead(data.u.data_var,NULL,NULL);
						break;
						default:
							estatus = NhlFATAL;
						break;
						}
						if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
							if(!*(logical*)val->multidval.val) {
								done = 1;
							}
							switch(data.kind) {
							case NclStk_VAL:
								if(data.u.data_obj->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)data.u.data_obj);
								}
								break;
							case NclStk_VAR:
								if(data.u.data_var->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)data.u.data_var);
								}
								break;
							default:
								break;
							}
						}  else if(val->multidval.kind == SCALAR){
							tmp_md = _NclCoerceData(val,Ncl_Typelogical,NULL);
							if(tmp_md == NULL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce expression to logical type");
								estatus = NhlFATAL;
							} else {
								if(!*(logical*)tmp_md->multidval.val) {
									done = 1;
								}
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"While requires a SCALAR conditional expression");
							estatus = NhlFATAL;
						}
						if((estatus != NhlFATAL)&&(!done)){
							rtst = _NclExecute(block_off);
							switch(rtst) {
							case Ncl_ERRORS: 
								done = 1;
								estatus = NhlFATAL;
								break;
							case Ncl_BREAKS:
								done = 1;
								break;
							default:
								break;
							}
						}  else {
							done = 1;
						}
					} else {
						done = 1;
						estatus = NhlFATAL;
					}
				}
			}
				break;
			case ASSIGN_FILEVAR_DIM_OP: {
				NclFile file;
				NclStackEntry* file_ptr,data,rhs_data,fvar;
				NclMultiDValData file_md,rhs_md,dim_expr_md,thevalue;
				NclSymbol* file_sym;
				NclQuark var_name;
				long dim_num;
				NclQuark dim_name;


				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var_name = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)(*ptr);	
/*
				ptr++;lptr++;fptr++;
				var_name = (NclQuark)*ptr;
*/
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL)&&(file_ptr->u.data_var != NULL)) {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if((file_md != NULL)&&(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData)) {
						file = (NclFile)_NclGetObj((int)*(obj*)file_md->multidval.val);
						if(file != NULL) {
							data = _NclPop();
							switch(data.kind) {
							case NclStk_VAL:
								dim_expr_md = data.u.data_obj;
								break;
							case NclStk_VAR:
								dim_expr_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
								break;
							default:
								dim_expr_md = NULL;
								estatus = NhlFATAL;
								break;
							}
							if(dim_expr_md->multidval.kind == SCALAR) {
								if(dim_expr_md->multidval.type->type_class.type & Ncl_Typelong) {
									dim_num = *(long*)dim_expr_md->multidval.val;
								} else {
									if(!(_NclScalarCoerce(dim_expr_md->multidval.val,dim_expr_md->multidval.data_type,(void*)&dim_num,NCL_long))) {                            
										estatus = NhlFATAL;

									}
		
								}
								rhs_data = _NclPop();
								switch(rhs_data.kind) {
								case NclStk_VAR:
									rhs_md = _NclVarValueRead(rhs_data.u.data_var,NULL,NULL);
									break;
								case NclStk_VAL:
									rhs_md = rhs_data.u.data_obj;
									break;
								default:
									break;
								}
								if((rhs_md != NULL)&&(rhs_md->multidval.kind == SCALAR)) {
									if(rhs_md->multidval.type->type_class.type & Ncl_Typestring) {
										dim_name  = *(NclQuark*)(rhs_md->multidval.val);
									} else {
										if(!(_NclScalarCoerce(rhs_md->multidval.val,rhs_md->multidval.data_type,&dim_name,NCL_string))) {                            
											estatus = NhlFATAL;
										}
									}
								}
								if(estatus != NhlFATAL) {
									estatus = _NclFileVarWriteDim(file,var_name,dim_name,dim_num);
								}
							}
						}
					}
				} else {
					_NclCleanUpStack(2);
				} 
			break;
			}
			case FILEVAR_DIM_OP:	
			case PARAM_FILEVAR_DIM_OP: {
				NclSymbol *file;
				NclQuark var_name;
				NclStackEntry dim_expr;
				NclMultiDValData tmp_md,tmp1_md,file_md,thevalue;
				NclStackEntry *file_ptr,data,fvar;
				NclFile file_obj;

				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var_name = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				file = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				var_name = (NclQuark)(*ptr);
*/
				dim_expr = _NclPop();
				file_ptr = _NclRetrieveRec(file,READ_IT);
				if((estatus != NhlFATAL)&&((file_ptr == NULL) || (file_ptr->u.data_var == NULL))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File (%s) is undefined",file->name);
					estatus = NhlFATAL;
				} else if(estatus != NhlFATAL){
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						file_obj = (NclFile)_NclGetObj(*(int*)file_md->multidval.val);
						if(file_obj == NULL) {
							estatus = NhlFATAL;
						}
					}
					switch(dim_expr.kind) {
					case NclStk_VAR:
						tmp_md = _NclVarValueRead(dim_expr.u.data_var,NULL,NULL);
						break;
					case NclStk_VAL:
						tmp_md = dim_expr.u.data_obj;
						break;
					default:
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Internal errror: An incorrect type of object was placed on the stack");
						estatus = NhlFATAL;
						break;
					}
					if((tmp_md != NULL)) {
						if(!(tmp_md->multidval.type->type_class.type & Ncl_Typelong)) {
							tmp1_md = _NclCoerceData(tmp_md,Ncl_Typelong,NULL);
							if(tmp1_md == NULL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not corece dimension ref into long");
								estatus = NhlFATAL;
							} else if((tmp1_md != NULL)&&(tmp_md->obj.status != PERMANENT)) {
								_NclDestroyObj((NclObj)tmp_md);
								tmp_md =tmp1_md;
							} 	
						}
						if(estatus != NhlFATAL) {
							if(tmp_md->multidval.kind == SCALAR) {
								tmp1_md = _NclFileVarReadDim(file_obj,var_name,(NclQuark)-1,*(long*)tmp_md->multidval.val);
								if(tmp1_md != NULL) {
									data.kind = NclStk_VAL;
									data.u.data_obj = tmp1_md;
									estatus = _NclPush(data);
								}
							} else 	{
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension references must be scalar");
							}
						}
					}
				}
			break;
			}
			case CREATE_OBJ_WP_OP : 
			case CREATE_OBJ_OP : {
				int nres;
				NclSymbol *objtype;
				NclStackEntry parent,data;
				NclStackEntry obj_name_expr;
				NclMultiDValData obj_name_md;
				NclMultiDValData tmp_md;
				NclMultiDValData tmp1_md;
				char * objname = NULL;
				if(*ptr == CREATE_OBJ_WP_OP) {
					parent = _NclPop();
					if(parent.kind == NclStk_VAR) {
						tmp_md = _NclVarValueRead(parent.u.data_var,NULL,NULL);
					} else if(parent.kind == NclStk_VAL) {
						tmp_md = parent.u.data_obj;
						parent.u.data_obj = NULL;
					} else {
						estatus = NhlFATAL;
					}
					if(tmp_md->multidval.kind != SCALAR) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"HLU Object can only have one parent, Parent objects must be scalar");
						estatus = NhlFATAL;
					}
				} else {
					tmp_md = NULL;
					parent.u.data_var = NULL;
				}
				obj_name_expr = _NclPop();
				if(obj_name_expr.kind == NclStk_VAL) {
					obj_name_md = obj_name_expr.u.data_obj;
				} else if(obj_name_expr.kind == NclStk_VAR){
					obj_name_md = _NclVarValueRead(
						obj_name_expr.u.data_var,
						NULL,
						NULL
						);
				} else {
					estatus = NhlFATAL;
					obj_name_md = NULL;
				}
				if(obj_name_md == NULL) {
					estatus = NhlFATAL;
				} else {
					if(obj_name_md->multidval.kind != SCALAR) {
						NhlPError(NhlFATAL,
							NhlEUNKNOWN,
							"create: The object name expression must result in a single scalar string value."	
						);
						estatus = NhlFATAL;
					} else
					if(!(obj_name_md->multidval.type->type_class.type & Ncl_Typestring)) {

						tmp1_md = _NclCoerceData(
						obj_name_md,
						Ncl_Typestring,
						NULL
						);
						if(tmp1_md == NULL) {
						NhlPError(NhlFATAL,
							NhlEUNKNOWN,
							"create: The object name expression must result in a string value or a value that can be coerced to a string."	
						);
							estatus = NhlFATAL;
							if(obj_name_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)obj_name_md);
							}
						} else {
							if(obj_name_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)obj_name_md);
							}
							obj_name_md = tmp1_md;
							tmp1_md = NULL;
						}
					}
				}
				objname = NrmQuarkToString(*(string*)obj_name_md->multidval.val);
				if(obj_name_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)obj_name_md);
				}
				
			
				ptr++;lptr++;fptr++;
				nres = (int)*ptr;
				ptr++;lptr++;fptr++;
				objtype =(NclSymbol*)*ptr ;
	
				if(estatus != NhlFATAL) {
					data = _NclCreateHLUObjOp(nres,objname,objtype,tmp_md);
					if(data.kind != NclStk_NOVAL) {
						estatus = _NclPush(data);
					} else {
						estatus = NhlFATAL;
					}
				} else {
					_NclCleanUpStack(2*nres);
				}
		
				if((tmp_md != NULL)&&(parent.u.data_var != NULL)&&(parent.u.data_var->obj.status != PERMANENT))  {
					_NclDestroyObj((NclObj)parent.u.data_var);
				} else if((tmp_md != NULL)&&(tmp_md->obj.status != PERMANENT)) {
					_NclDestroyObj((NclObj)tmp_md);
				}
			}
			break;
/*****************************
* Three Operand Instructions *
*****************************/
			case PARAM_VARATT_OP:
			case VARATT_OP: {
				NclSymbol *thesym = NULL;
				char*	attname = NULL;
				int	nsubs;
				NclStackEntry *var = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;
				NclMultiDValData tmpmis = NULL;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (int)(*ptr);

				var = _NclRetrieveRec(thesym,READ_IT);
				if(var->u.data_var != NULL) {
					if(_NclVarIsAtt(var->u.data_var,attname)) {
						if(nsubs == 1) {
							sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
							sel_ptr->n_entries = 1;
							data =_NclPop();
							if(data.u.sub_rec->name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
								estatus = NhlWARNING;
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
								estatus = NhlFATAL;
								break;
							}
						_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING) {
								estatus = ret;
								break;
							}
						} else if(nsubs != 0) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attributes only have one dimension, %d subscripts used",nsubs);		
							estatus = NhlFATAL;
						}
						if(estatus != NhlFATAL) {
							data.u.data_obj = _NclReadAtt(var->u.data_var,attname,sel_ptr);
							if(data.u.data_obj == NULL) {
								data.kind = NclStk_NOVAL;
								estatus = NhlFATAL;
							} else {
								data.kind = NclStk_VAL;
							}
						}
					} else {
						estatus = NhlWARNING;
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Attempt to reference attribute (%s) which is undefined",attname);
						data.kind = NclStk_VAL;
						tmpmis = _NclCreateMissing();
						data.u.data_obj = tmpmis;
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is still undefined, unable to reference attribute %s",thesym->name,attname);
					estatus = NhlFATAL;
				}
				if(estatus != NhlFATAL) 
					estatus = _NclPush(data);
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

				var = _NclRetrieveRec(thesym,WRITE_IT);
				if((var == NULL)||(var->u.data_var == NULL)) {
					estatus = NhlFATAL;
				} else if(_NclIsDim(var->u.data_var,coord_name) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension in variable (%s).",coord_name,thesym->name);
					estatus = NhlFATAL;
				} else {
					if(nsubs == 0) {
						sel_ptr = NULL;
					} else if(nsubs == 1){
						sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec->name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							estatus = NhlWARNING;
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
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING)
							estatus = NhlFATAL;

					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts on left hand side of assignement",nsubs);
						_NclCleanUpStack(nsubs);
						estatus = NhlFATAL;
					}
					if(estatus != NhlFATAL) {
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
							estatus = NhlFATAL;
						break;
						}
					
						if(thevalue != NULL) {
							ret = _NclWriteCoordVar(var->u.data_var,thevalue,coord_name,sel_ptr);
							if(ret<estatus){
								estatus = ret;
							}
						} else {
							estatus = NhlFATAL;
						}
						switch(data.kind) {
						case NclStk_VAL: 
							if(data.u.data_obj->obj.status != PERMANENT) 
								_NclDestroyObj((NclObj)data.u.data_obj);
							break;
						case NclStk_VAR:
							if(data.u.data_obj->obj.status != PERMANENT) 
								_NclDestroyObj((NclObj)data.u.data_var);
							break;
						default:
							break;
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

				var = _NclRetrieveRec(thesym,READ_IT);
				if((var == NULL)||(var->u.data_var == NULL)) {
					estatus = NhlFATAL;
				} else if(_NclIsDim(var->u.data_var,coord_name) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension in variable (%s).",coord_name,thesym->name);
					estatus = NhlFATAL;
				} else {
					if(nsubs == 0) {
						sel_ptr = NULL;
					} else if(nsubs == 1){
						sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec->name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							estatus = NhlWARNING;
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
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING)
							estatus = NhlFATAL;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts used on coordinate variable reference",nsubs);
						_NclCleanUpStack(nsubs);
						estatus = NhlFATAL;
					}
					if(estatus != NhlFATAL) {
						data.u.data_var = _NclReadCoordVar(var->u.data_var,coord_name,sel_ptr);
						if(data.u.data_var != NULL) {
							data.kind = NclStk_VAR;
							estatus = _NclPush(data);
						} else {
							estatus = NhlFATAL;
						}
					} 
				}
			}
			break;
			case ASSIGN_FILE_VAR_OP :{
/*
* Changed to a two operand function 1/30
*/
				NclSymbol *file_sym;
				NclQuark var;
				NclStackEntry *file_ptr = NULL;
				NclStackEntry rhs;
				NclStackEntry data;
				NclStackEntry fvar;
				NclMultiDValData thevalue;
				int nsubs = 0;
				NclFile file = NULL;
				NclSelectionRecord* sel_ptr = NULL;
				int i,index;
				NclMultiDValData rhs_md = NULL,value = NULL; 
				int subs_expected;




				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)*ptr;
				ptr++;lptr++;fptr++;
/*
				var = (NclQuark)*ptr;
				ptr++;lptr++;fptr++;
*/
				nsubs = *ptr;
				file_ptr = _NclRetrieveRec(file_sym,WRITE_IT);
				if((file_ptr != NULL)&&(file_ptr->kind == NclStk_VAR)&&(estatus != NhlFATAL)) {
					value = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(value->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						if(value != NULL)
							file = (NclFile)_NclGetObj((int)*(obj*)value->multidval.val);
						if((file != NULL)&&((index = _NclFileIsVar(file,var)) != -1)) {
							if((nsubs != file->file.var_info[index]->num_dimensions)&&(nsubs != 0)){
								subs_expected = 0;
								for(i = 0; i < file->file.var_info[index]->num_dimensions; i++) {
									if(file->file.file_dim_info[file->file.var_info[index]->file_dim_num[i]]->dim_size != 1) {
										subs_expected += 1;
									}
								}
								if(subs_expected != nsubs) {
									NhlPError(NhlFATAL,NhlEUNKNOWN,
										"Number of subscripts (%d) and number of dimensions (%d) do not match for variable (%s->%s)",
										nsubs,
										file->file.var_info[index]->num_dimensions,
										file_sym->name,
										NrmQuarkToString(var));
										estatus = NhlFATAL;
										_NclCleanUpStack(nsubs +1);
								}
							} 
	
							if((nsubs != 0)&&(nsubs == file->file.var_info[index]->num_dimensions))  {
								sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
								sel_ptr->n_entries = nsubs;
								for(i = 0 ; i < nsubs; i++) {
									data = _NclPop();
									switch(data.u.sub_rec->sub_type) {
									case INT_VECT:
										estatus = _NclBuildFileVSelection(file,var,data.u.sub_rec->u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
										break;
									case INT_RANGE:
										estatus = _NclBuildFileRSelection(file,var,data.u.sub_rec->u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
										break;
									case COORD_VECT:
										estatus = _NclBuildFileCoordVSelection(file,var,data.u.sub_rec->u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
										break;
									case COORD_RANGE:
										estatus = _NclBuildFileCoordRSelection(file,var,data.u.sub_rec->u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec->name);
										break;
									}
									_NclFreeSubRec(data.u.sub_rec);
								}
							} else if((nsubs != 0)&&(nsubs == subs_expected)) { 
								sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
								sel_ptr->n_entries = file->file.var_info[index]->num_dimensions;
		
								for(i = 0 ; i < sel_ptr->n_entries; i++) {
									if(file->file.file_dim_info[file->file.var_info[index]->file_dim_num[sel_ptr->n_entries - i - 1]]->dim_size != 1){
										data = _NclPop();
										switch(data.u.sub_rec->sub_type) {
										case INT_VECT:
											estatus = _NclBuildFileVSelection(file,var,data.u.sub_rec->u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
											break;
										case INT_RANGE:
											estatus = _NclBuildFileRSelection(file,var,data.u.sub_rec->u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
											break;
										case COORD_VECT:
											estatus = _NclBuildFileCoordVSelection(file,var,data.u.sub_rec->u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
											break;
										case COORD_RANGE:
											estatus = _NclBuildFileCoordRSelection(file,var,data.u.sub_rec->u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
											break;
										}
										_NclFreeSubRec(data.u.sub_rec);
									} else {
			
										sel_ptr->selection[sel_ptr->n_entries - i - 1].sel_type = Ncl_SUBSCR;
										sel_ptr->selection[sel_ptr->n_entries - i - 1].u.sub.start = 0;
										sel_ptr->selection[sel_ptr->n_entries - i - 1].u.sub.finish= 0;
										sel_ptr->selection[sel_ptr->n_entries - i - 1].u.sub.stride = 1;
										sel_ptr->selection[sel_ptr->n_entries - i - 1].dim_num = sel_ptr->n_entries - i - 1;
										

									}
								}
							} else {
								sel_ptr = NULL;
							}
/*
* Coercion must wait until inside of File method
*/
							rhs = _NclPop();
							if(estatus != NhlFATAL) {
								if(rhs.kind == NclStk_VAL) {
									rhs_md = rhs.u.data_obj;
									if(rhs_md != NULL) {
										estatus = _NclFileWriteVar(file, var, rhs_md, sel_ptr);

										if(rhs_md->obj.status != PERMANENT) {
											_NclDestroyObj((NclObj)rhs_md);
										}
									} else {
										estatus = NhlFATAL;
									}
				
								} else if(rhs.kind == NclStk_VAR) {
									estatus = _NclFileWriteVarVar(file,var,sel_ptr,rhs.u.data_var,NULL);
									if(rhs.u.data_var->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)rhs.u.data_var);
									}
								} else {	

									estatus = NhlFATAL;
								}
							}
							
						} else {
							rhs = _NclPop();
							if(estatus != NhlFATAL) {
								if(rhs.kind == NclStk_VAL) {
									rhs_md = rhs.u.data_obj;
									if(rhs_md != NULL) {
										estatus = _NclFileWriteVar(file, var, rhs_md, NULL);

										if(rhs_md->obj.status != PERMANENT) {
											_NclDestroyObj((NclObj)rhs_md);
										}
									} else {
										estatus = NhlFATAL;
									}
				
								} else if(rhs.kind == NclStk_VAR) {
									estatus = _NclFileWriteVarVar(file,var,NULL,rhs.u.data_var,NULL);
									if(rhs.u.data_var->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)rhs.u.data_var);
									}
								} else {	
									estatus = NhlFATAL;
								}
	
							}
							
						}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) does not reference a file",file_sym->name);
						_NclCleanUpStack(nsubs+1);
						estatus = NhlFATAL;
					}
				} else {
					if((file_ptr == NULL)||(file_ptr->kind != NclStk_VAR)) { 
						NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is undefined or does not reference a file",file_sym->name);
						estatus = NhlFATAL;
					}
					_NclCleanUpStack(nsubs +1);
				}
				break;
			}
			case PARAM_FILE_VAR_OP:
			case FILE_VAR_OP : {
/*
* Changed to a two operand function 1/31/96
*/
				NclSymbol *dfile = NULL;
				NclQuark var;
				int nsubs,subs_expected;
				NclStackEntry *file_ptr = NULL;
				NclStackEntry out_var,data;
				NclStackEntry fvar;
				NclMultiDValData value,thevalue;
				NclFile file = NULL;
				int i;
				int dim_is_ref[NCL_MAX_DIMENSIONS];
				int index = -1;
/*
				int kind;
*/
				NclSelectionRecord* sel_ptr = NULL;
				NhlErrorTypes ret = NhlNOERROR;
/*
				kind = *ptr;
*/
				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				dfile = (NclSymbol*)*ptr;
				ptr++;lptr++;fptr++;
/*
				var = (NclQuark)*ptr;
				ptr++;lptr++;fptr++;
*/
				nsubs = (NclQuark)*ptr;
				file_ptr =  _NclRetrieveRec(dfile,READ_IT);
				if((file_ptr != NULL)&&(file_ptr->kind == NclStk_VAR)&&(estatus != NhlFATAL)) {
					value = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(value->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						if(value != NULL) 
							file = (NclFile)_NclGetObj((int)*(obj*)value->multidval.val);
						if((file != NULL)&&((index = _NclFileIsVar(file,var)) != -1)) {
							for(i = 0 ; i < file->file.var_info[index]->num_dimensions; i++) {
								dim_is_ref[i] = 0;
							}
							subs_expected = 0;
							for(i = 0; i < file->file.var_info[index]->num_dimensions; i++) {
                                                        	if(file->file.file_dim_info[file->file.var_info[index]->file_dim_num[i]]->dim_size != 1) {
                                                               		subs_expected += 1;
                                                                }
                                                        }

							if((nsubs != 0)&&(nsubs ==  file->file.var_info[index]->num_dimensions)){
								sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
								sel_ptr->n_entries = nsubs;
							} else if(nsubs == subs_expected) {
								sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
								sel_ptr->n_entries = file->file.var_info[index]->num_dimensions;
									
							} else if(nsubs==0){
								sel_ptr = NULL;
							} else {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimensions of variable, (%d) subscripts used, (%d) subscripts expected",nsubs,file->file.var_info[index]->num_dimensions);
								estatus = NhlFATAL;
								_NclCleanUpStack(nsubs);
							}
							if(estatus != NhlFATAL) {
								if(sel_ptr != NULL) {
									for(i=0;i<sel_ptr->n_entries;i++) {
										if(file->file.file_dim_info[file->file.var_info[index]->file_dim_num[sel_ptr->n_entries - i - 1]]->dim_size != 1){
											data =_NclPop();
											switch(data.u.sub_rec->sub_type) {
											case INT_VECT:
												ret = _NclBuildFileVSelection(file,var,data.u.sub_rec->u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
												break;
											case INT_RANGE:
												ret = _NclBuildFileRSelection(file,var,data.u.sub_rec->u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
												break;
											case COORD_VECT:
												estatus = _NclBuildFileCoordVSelection(file,var,data.u.sub_rec->u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
												break;
											case COORD_RANGE:
												estatus = _NclBuildFileCoordRSelection(file,var,data.u.sub_rec->u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec->name);
												break;
											}
											_NclFreeSubRec(data.u.sub_rec);
											if(ret < NhlWARNING) {
												estatus = NhlFATAL;
											}
											if(estatus < NhlWARNING) 
												break;
										} else {
											sel_ptr->selection[sel_ptr->n_entries - i - 1].sel_type = Ncl_SUBSCR;
											sel_ptr->selection[sel_ptr->n_entries - i - 1].u.sub.start = 0;
											sel_ptr->selection[sel_ptr->n_entries - i - 1].u.sub.finish= 0;
											sel_ptr->selection[sel_ptr->n_entries - i - 1].u.sub.stride = 1;
											sel_ptr->selection[sel_ptr->n_entries - i - 1].dim_num = sel_ptr->n_entries - i - 1;

										}
										if(!dim_is_ref[(sel_ptr->selection[sel_ptr->n_entries - i - 1]).dim_num]) {
											dim_is_ref[(sel_ptr->selection[sel_ptr->n_entries - i - 1]).dim_num] = 1;
										} else {
											NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in subscript # %d, dimension is referenced more that once",i);
											estatus = NhlFATAL;
										}
									}
								}
								if(estatus != NhlFATAL) {
									out_var.kind = NclStk_VAR;
									out_var.u.data_var = _NclFileReadVar(file,var,sel_ptr);
									if(sel_ptr != NULL)
										NclFree(sel_ptr);
									if((estatus != NhlFATAL)&&(out_var.u.data_var != NULL)) {
										estatus = _NclPush(out_var);
									} else 	{
										estatus = NhlFATAL;
									}
								}	
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Either file (%s) isn't defined or variable (%s) is not a variable in the file",dfile->name,NrmQuarkToString(var));
							_NclCleanUpStack(nsubs);
							estatus = NhlFATAL;
							out_var.kind = NclStk_NOVAL;	
							out_var.u.data_obj = NULL;
						}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) does not reference a file",dfile->name);
						_NclCleanUpStack(nsubs);
						estatus = NhlFATAL;
					}
				} else {
					if((file_ptr == NULL)||(file_ptr->kind != NclStk_VAR)) { 
						NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is undefined or does not reference a file",dfile->name);
						estatus = NhlFATAL;
					}
					_NclCleanUpStack(nsubs);
				}
				
				break;
			}
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
	
				var = _NclRetrieveRec(thesym,WRITE_IT);
				if(var->u.data_var != NULL) {
					if(nsubs == 1) {
						sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
						sel_ptr->n_entries = 1;
						data1 =_NclPop();
						if(data1.u.sub_rec->name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
							estatus = NhlWARNING;
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
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(data1.u.sub_rec);
						if(ret < NhlWARNING) 
							estatus = NhlFATAL;
					} else if(nsubs != 0){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to subscript attribute with more than one dimension");
						estatus = NhlFATAL;
					}
					if(!(estatus < NhlINFO)) {
						value = _NclPop();
						if(value.kind == NclStk_VAR) {
							value_md = _NclVarValueRead(value.u.data_var,NULL,NULL);
							if(value_md == NULL) {
								estatus = NhlFATAL;
							}
						} else if(value.kind == NclStk_VAL){
							value_md = value.u.data_obj;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to assign illegal type or value to variable attribute");
							estatus = NhlFATAL;
						}
						ret = _NclWriteAtt(var->u.data_var,attname,value_md,sel_ptr);
						if((value.kind == NclStk_VAR)&&(value.u.data_var->obj.status != PERMANENT)) {
							 _NclDestroyObj((NclObj)value.u.data_var);
						} else if((value.kind == NclStk_VAL)&&(value.u.data_obj->obj.status != PERMANENT)){
							 _NclDestroyObj((NclObj)value.u.data_obj);
						} 
						if( ret < NhlINFO) {
							estatus = ret;
						}
					} else {
						_NclCleanUpStack(1);
					}
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined, can not assign attribute (%s)",thesym->name,attname);
					estatus = NhlFATAL;
				}
			}
			break;
/*****************************
* Four Operand Instructions  *
*****************************/
			case ASSIGN_FILEVARATT_OP: {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,data1,rhs,fvar;
				NclMultiDValData file_md,rhs_md;
				NclSelectionRecord *sel_ptr;
				NclFile		file;
				NclQuark 	var;
				NclQuark	att;
				int nsubs;
				NhlErrorTypes ret = NhlNOERROR;
				NclMultiDValData thevalue;

				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)(*ptr);
/*
				ptr++;lptr++;fptr++;
				var = (NclQuark)(*ptr);
*/
				ptr++;lptr++;fptr++;
				att = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = *ptr;
				file_ptr = _NclRetrieveRec(file_sym,WRITE_IT);
				if((estatus!=NhlFATAL)&&(file_ptr != NULL)&&(file_ptr->u.data_var != NULL)) {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if((file_md != NULL)&&(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData)) {
						file = (NclFile)_NclGetObj((int)*(obj*)file_md->multidval.val);
						if((file != NULL)&&((_NclFileIsVar(file,var)) != -1)) {
							if(nsubs == 1) {
								sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
								sel_ptr->n_entries = 1;
								data1 =_NclPop();
								if(data1.u.sub_rec->name != NULL) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
									estatus = NhlWARNING;
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
									estatus = NhlFATAL;
									break;
								}
								_NclFreeSubRec(data1.u.sub_rec);
								if(ret < NhlWARNING) 
									estatus = NhlFATAL;
							} else if(nsubs != 0){
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Attribute must only be single dimensional objects");
								estatus = NhlFATAL;
							}
							rhs = _NclPop();
							if((rhs.kind == NclStk_VAL)&&(rhs.u.data_obj != NULL)) {
								rhs_md = rhs.u.data_obj;
							} else if((rhs.kind == NclStk_VAR)&&(rhs.u.data_var != NULL)) {
								rhs_md = _NclVarValueRead(rhs.u.data_var,NULL,NULL);
							} else {
								estatus = NhlFATAL;
							}
	
							if(estatus != NhlFATAL) {
								estatus = _NclFileWriteVarAtt(file,var,att,rhs_md,NULL);
							}
							if(estatus != NhlFATAL) {
								if(rhs_md->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)rhs_md);
								}
							}
							
						}
					}
				} else {
					_NclCleanUpStack(nsubs);
				}
				break;
			}
			case ASSIGN_FILEVAR_COORD_OP:{
				NclFile file;
				NclStackEntry *file_ptr,rhs_data,data,fvar;
				NclMultiDValData file_md;
				NclSymbol *file_sym;
				NclQuark coord_name;
				NclQuark var_name;
				int nsubs;
				NclSelectionRecord *sel_ptr;
				NclMultiDValData rhs_md,thevalue;
				NclAtt theatt;
				NclAttList *step;
				NhlErrorTypes ret = NhlNOERROR;
	
				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var_name = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
/*
				var_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
*/
				coord_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (int)(*ptr);
/*
* This is really are read because the actual variable holding
* the file object id doesn't change
*/
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL) &&(file_ptr->u.data_var != NULL)) {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if((file_md != NULL)&&(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData)) {
						file = (NclFile)_NclGetObj((int)*(obj*)file_md->multidval.val);
						if((file!=NULL)&&(_NclFileVarIsDim(file,var_name,coord_name)!=-1)) {
							if(nsubs == 0) {
								sel_ptr = NULL;
							} else if(nsubs == 1){
								sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
								sel_ptr->n_entries = 1;
								data =_NclPop();
								if(data.u.sub_rec->name != NULL) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
									estatus = NhlWARNING;
								}
								switch(data.u.sub_rec->sub_type) {
								case INT_VECT:
/*
* Need to free some stuff here
*/						
									estatus = _NclBuildFileVSelection(file,coord_name,data.u.sub_rec->u.vec,&(sel_ptr->selection[0]),0,NULL);
									break;
								case INT_RANGE:
/*
* Need to free some stuff here
*/							
									estatus = _NclBuildFileRSelection(file,coord_name,data.u.sub_rec->u.range,&(sel_ptr->selection[0]),0,NULL);
									break;
								case COORD_VECT:
								case COORD_RANGE:
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
									NclFree(sel_ptr);
									sel_ptr = NULL;
									estatus = NhlFATAL;
									break;
								}
								_NclFreeSubRec(data.u.sub_rec);
							} else {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts used on coordinate variable reference",nsubs);
								_NclCleanUpStack(nsubs +1);
								estatus = NhlFATAL;
							}
							if(estatus != NhlFATAL) {
								rhs_data = _NclPop();
								switch(rhs_data.kind) {
								case NclStk_VAL:
									rhs_md = rhs_data.u.data_obj;
									break;
								case NclStk_VAR:
									rhs_md = _NclVarValueRead(rhs_data.u.data_var,NULL,NULL);
									break;
								default:
									rhs_md = NULL;
									estatus = NhlFATAL;
									break;
								}
								if(rhs_md != NULL) {
									estatus = _NclFileWriteCoord(file,coord_name,rhs_md,sel_ptr);
									if(rhs_data.kind == NclStk_VAR) {
										if(rhs_data.u.data_var->var.att_id != -1) {
											theatt = (NclAtt)_NclGetObj(rhs_data.u.data_var->var.att_id);
											step = theatt->att.att_list;
											while(step != NULL){
												ret = _NclFileWriteVarAtt(file,coord_name,step->quark,step->attvalue,NULL);
												if(ret < NhlWARNING) {
													estatus = ret;
													break;
												}
											}
										}
									}
									if(rhs_data.u.data_obj->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)rhs_data.u.data_obj);
								
									}
								}
							}
						} else {
							estatus = NhlFATAL;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) does not exist in file (%s), can not assign coordinate variable",coord_name,NrmQuarkToString(file->file.fname));
						}
					}
				} else {
					_NclCleanUpStack(nsubs +1);
				}
				break;
			}
			case FILEVARATT_OP:
			case PARAM_FILEVARATT_OP: {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,fvar;
				NclMultiDValData file_md,thevalue;
				NclFile	file;
				NclQuark var_name,att_name;
				int nsubs = 0;
				NclSelectionRecord* sel_ptr = NULL;
				NclStackEntry out_data;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;
			
				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var_name = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				var_name = (NclQuark)(*ptr);
*/
				ptr++;lptr++;fptr++;
				att_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = *ptr;
	
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL)&&(file_ptr->u.data_var != NULL))  {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						file = (NclFile)_NclGetObj(*((int*)file_md->multidval.val));
						if(nsubs == 1) {
							sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
							sel_ptr->n_entries = 1;
							data =_NclPop();
							if(data.u.sub_rec->name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
								estatus = NhlWARNING;
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
								estatus = NhlFATAL;
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING)
								estatus = ret;
						} else if(nsubs != 0) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attributes only have one dimension, %d subscripts used",nsubs);		
							estatus = NhlFATAL;
						}
						out_data.u.data_obj = _NclFileReadVarAtt(file,var_name,att_name,sel_ptr);
						if(out_data.u.data_obj != NULL) {
							out_data.kind = NclStk_VAL;
							estatus = _NclPush(out_data);
						} else {
							estatus = NhlFATAL;
						}
						
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference a file variable attribute from a non-file");
						estatus = NhlFATAL;
					}
				} else {
					_NclCleanUpStack(nsubs);
				}
				break;
			}
			case FILEVAR_COORD_OP:
			case PARAM_FILEVAR_COORD_OP: {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,fvar;
				NclMultiDValData file_md,thevalue;
				NclFile	file;
				NclQuark var_name,coord_name;
				int nsubs = 0;
				NclSelectionRecord* sel_ptr = NULL;
				NclStackEntry out_data;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;


				fvar = _NclPop();
				switch(fvar.kind) {
				case NclStk_VAL: 
					thevalue = fvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(fvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||(thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					var_name = *(NclQuark*)thevalue->multidval.val;
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
/*
				var_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
*/
				coord_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
				nsubs = (*ptr);
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL)&&(file_ptr->u.data_var != NULL)) {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						file = (NclFile)_NclGetObj(*(int*)file_md->multidval.val);
						if(nsubs == 0) {
							sel_ptr = NULL;
						} else if(nsubs == 1){
							sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
							sel_ptr->n_entries = 1;
							data =_NclPop();
							if(data.u.sub_rec->name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
								estatus = NhlWARNING;
							}
							switch(data.u.sub_rec->sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/						
								ret = _NclBuildFileVSelection(file,var_name,data.u.sub_rec->u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
							case INT_RANGE:
/*
* Need to free some stuff here
*/							
								ret = _NclBuildFileRSelection(file,var_name,data.u.sub_rec->u.range,&(sel_ptr->selection[0]),0,NULL);
								break;
							case COORD_VECT:
							case COORD_RANGE:
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
								NclFree(sel_ptr);
								sel_ptr = NULL;
								estatus = NhlFATAL;
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING)
								estatus = NhlFATAL;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts used on coordinate variable reference",nsubs);
							_NclCleanUpStack(nsubs);
							estatus = NhlFATAL;
						}
						out_data.u.data_var =_NclFileReadCoord (file, coord_name,sel_ptr);
						if(out_data.u.data_var != NULL) {
							out_data.kind = NclStk_VAR;
							estatus = _NclPush(out_data);
						} else {
							estatus = NhlFATAL;
						}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference a file variable coordinate with a non file ");
						estatus = NhlFATAL;
					}
				} else {
					_NclCleanUpStack(nsubs);
				}
				break;
			}
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
				rhs_var = _NclRetrieveRec(rhs_sym,READ_IT);
				ptr++;lptr++;fptr++;
				rhs_nsubs = *ptr;
				ptr++;lptr++;fptr++;
				lhs_sym = (NclSymbol*)*ptr;
				lhs_var = _NclRetrieveRec(lhs_sym,WRITE_IT);
				ptr++;lptr++;fptr++;
				lhs_nsubs = *ptr;

				if((rhs_var == NULL)||(rhs_var->kind == NclStk_NOVAL)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN," Assign: %s is undefined",rhs_sym->name);
					estatus = NhlFATAL;
				}

				if((estatus!=NhlFATAL)&&(lhs_var != NULL)&&(lhs_var->kind == NclStk_NOVAL)) {
					if(lhs_nsubs != 0) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Assign: %s is undefined, can not subscript an undefined variable",lhs_sym->name);
						estatus = NhlFATAL;
						_NclCleanUpStack(lhs_nsubs);
					} else if(rhs_nsubs == 0) {
						lhs_var->kind = NclStk_VAR;
						lhs_var->u.data_var = _NclCopyVar(rhs_var->u.data_var,lhs_sym->name,NULL);
						_NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT);	
						lhs_var->u.data_var->var.thesym = lhs_sym;
						(void)_NclChangeSymbolType(lhs_sym,VAR);
						lhs_var->u.data_var->var.var_type = NORMAL;
					} else if((rhs_nsubs != 0)&&(rhs_nsubs == rhs_var->u.data_var->var.n_dims)) {
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
							ret = _NclBuildCoordVSelection(rhs_var->u.data_var,data.u.sub_rec->u.vec,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
							break;
						case COORD_RANGE:
							ret = _NclBuildCoordRSelection(rhs_var->u.data_var,data.u.sub_rec->u.range,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
							break;
						}
						_NclFreeSubRec(data.u.sub_rec);
						if(ret < NhlWARNING) {
							estatus = NhlFATAL;
							break;
						}
					} 
					if(estatus != NhlFATAL) {
						lhs_var->kind = NclStk_VAR;
						lhs_var->u.data_var = _NclVarRead(rhs_var->u.data_var,rhs_sel_ptr);
						if(rhs_sel_ptr != NULL) {
							NclFree(rhs_sel_ptr);
						}
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
						(void)_NclChangeSymbolType(lhs_sym,VAR);
						lhs_var->u.data_var->var.var_type = NORMAL;
/*
*-----> end of questionable code
*/
					}
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts on rhs do not match number of dimesions of variable,(%d) Subscripts used, (%d) Subscripts expected",rhs_nsubs,rhs_var->u.data_var->var.n_dims);
						estatus = NhlFATAL;
						_NclCleanUpStack(rhs_nsubs);
						
					}
				} else if((estatus !=NhlFATAL)&&(lhs_var->kind == NclStk_VAR)&&(lhs_var->u.data_var != NULL)) {
/*
* When the target variable is already defined just normal assignment occurs if it is not subscripted
* if it is then the _NclAssignVarToVar is used which is different then the normal assignment provided
* by the ASSIGN_VAR_OP operator.
*/
					if(rhs_nsubs == 0) {
						rhs_sel_ptr = NULL;
					} else if((estatus != NhlFATAL)&&(rhs_nsubs != rhs_var->u.data_var->var.n_dims)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts on rhs do not match number of dimesions of variable,(%d) Subscripts used, (%d) Subscripts expected",rhs_nsubs,rhs_var->u.data_var->var.n_dims);
						estatus = NhlFATAL;
						_NclCleanUpStack(rhs_nsubs);
					} else {
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
								ret = _NclBuildCoordVSelection(rhs_var->u.data_var,data.u.sub_rec->u.vec,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							case COORD_RANGE:
								ret = _NclBuildCoordRSelection(rhs_var->u.data_var,data.u.sub_rec->u.range,&(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),rhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING) {
								estatus = NhlFATAL;
								break;
							}
						} 
					} 
					if((lhs_nsubs ==0)&&(estatus != NhlFATAL)){
						lhs_sel_ptr = NULL;
					} else if((estatus != NhlFATAL)&&(lhs_nsubs != lhs_var->u.data_var->var.n_dims)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts on lhs do not match number of dimesions of variable,(%d) Subscripts used, (%d) Subscripts expected",lhs_nsubs,lhs_var->u.data_var->var.n_dims);
						estatus = NhlFATAL;
						_NclCleanUpStack(lhs_nsubs);
					} else if (estatus != NhlFATAL) {
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
								ret = _NclBuildCoordVSelection(lhs_var->u.data_var,data.u.sub_rec->u.vec,&(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),lhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							case COORD_RANGE:
								ret = _NclBuildCoordRSelection(lhs_var->u.data_var,data.u.sub_rec->u.range,&(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),lhs_nsubs - i - 1,data.u.sub_rec->name);
								break;
							}
							_NclFreeSubRec(data.u.sub_rec);
							if(ret < NhlWARNING) {
								estatus = NhlFATAL;
								break;
							}
						} 
					} 
					if(estatus != NhlFATAL) {
						ret = _NclAssignVarToVar(lhs_var->u.data_var,lhs_sel_ptr,rhs_var->u.data_var,rhs_sel_ptr);
						if(lhs_sel_ptr != NULL) {
							NclFree(lhs_sel_ptr);
						}
						if(rhs_sel_ptr != NULL) {
							NclFree(rhs_sel_ptr);
						}
						if(ret < NhlINFO) {
							estatus = ret;
						}
					} 
				} else {
					_NclCleanUpStack(rhs_nsubs);
					_NclCleanUpStack(lhs_nsubs);
				}
				break;
			}
			default:
				break;
		}
		if(estatus < NhlINFO) {
			if(*fptr == NULL) {
				NhlPError(estatus,NhlEUNKNOWN,"Execute: Error occurred at or near line %d\n",(cmd_line ? (*lptr)-1: *lptr));
			} else {
				NhlPError(estatus,NhlEUNKNOWN,"Execute: Error occurred at or near line %d in file %s\n", *lptr, *fptr);
			}
			if(estatus < NhlWARNING) {
/*
* need to clean up stack !!! for current level
*/
				_NclAbortFrame();
/*
* Probably still need more stack freeing for other types of errors 
* this really only handles left overs from failed function and 
* procedure calls
*/
				_NclClearToStackBase();

				level--;
				return(Ncl_ERRORS);
			}
		}	
		estatus = NhlNOERROR;	
		ptr++;lptr++;fptr++;
	}
}

#ifdef __cplusplus
}
#endif

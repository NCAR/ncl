

/*
 *      $Id: Execute.c,v 1.1 1993-12-21 19:17:28 ethan Exp $
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
	NhlErrorTypes status = NOERROR;

	machine = _NclGetCurrentMachine();
	ptr = machine + start_offset;
	lptr = _NclGetCurrentLineRec() + start_offset;
	fptr = _NclGetCurrentFileNameRec() + start_offset;

	while(1) {
		switch(*ptr) {
/****************************
* Zero Operand Instructions *
****************************/
			case STOPSEQ:
				return(Ncl_STOPS);
			case CONTINUE_OP:
				return(Ncl_CONTINUES);
			case BREAK_OP:
				return(Ncl_BREAKS);
			case ENDSTMNT_OP:
			case NOOP :
				break;
			case INT_SUBSCRIPT_OP :
				break;
			case DEFAULT_RANGE_OP :
				break;
			case RANGE_INDEX_OP :
				break;
			case SINGLE_INDEX_OP :
				break;
			case RETURN_OP :
				break;
			case IF_OP :
				break;
			case NAMED_COORD_SUBSCRIPT_OP :
				break;
			case NAMED_INT_SUBSCRIPT_OP :
				break;
			case COORD_SUBSCRIPT_OP :
				break;
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
				char **thestr;
				int dim_size = 1;
			
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				thestr = (char**)NclMalloc((unsigned)sizeof(char*));
				*thestr = (char*)NclMalloc((unsigned)strlen((char*)*ptr + 1));
				strcpy(*thestr,(char*)ptr);
				data.u.data_obj = _NclMultiDValstringCreate(NULL,
						(void*)thestr,NULL,1,&dim_size,
						NULL,TEMPORARY,NULL);
				_NclPush(data);
				break;
			}
			case PUSH_REAL_LIT_OP : 
			{
				NclStackEntry data;
				float *theval;
				int dim_size = 1;
				ptr++;lptr++;fptr++;
				data.kind = NclStk_VAL;
				data.u.data_obj = _NclMultiDValfloatCreate(NULL,
						(void*)ptr,NULL,1,&dim_size,
						NULL,STATIC,NULL);
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
						(void*)ptr,NULL,1,&dim_size,
						NULL,STATIC,NULL);
				_NclPush(data);
				break;
			}
			case JMPFALSE :
				ptr++;lptr++;fptr++;
				break;
			case CREATE_OBJ_OP :
				ptr++;lptr++;fptr++;
				break;
			case SET_OBJ_OP :
				ptr++;lptr++;fptr++;
				break;
			case GET_OBJ_OP :
				ptr++;lptr++;fptr++;
				break;
			case PROC_CALL_OP:
				ptr++;lptr++;fptr++;
				break;
			case BPROC_CALL_OP:
			{
				int i;
				ptr++;lptr++;fptr++;
/*
* This is not going to work because nothing is done to unpack the
* arguments they are just popped now!!!!!!
*/
				if(((NclSymbol*)*ptr)->u.bproc != NULL) {
					(*((NclSymbol*)*ptr)->u.bproc->theproc)();
					for(i = 0;i<((NclSymbol*)*ptr)->u.bproc->nargs; i++) {
						(void)_NclPop();
					}
				}
			}
				break;
			case FUNC_CALL_OP:
				ptr++;lptr++;fptr++;
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
/***************************
* Two Operand Instructions *
***************************/			
			case VAR_DIMNUM_OP:
			case ASSIGN_VAR_DIMNUM_OP:
			case PARAM_VAR_DIMNUM_OP:
			case VAR_DIMNAME_OP:
			case ASSIGN_VAR_DIMNAME_OP:
			case PARAM_VAR_DIMNAME_OP:
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
			case PARAM_VAR_OP:
			case ASSIGN_VAR_OP :
			case VAR_OP :
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
			case NEW_FRAME_OP:
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
			case CONVERT_TO_LOCAL:
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
			case DO_WHILE_OP :
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				break;
/*****************************
* Three Operand Instructions *
*****************************/
			case PARAM_FILE_VAR_OP:
			case PARAM_VARATT_OP:
			case PARAM_VAR_COORD_OP:
			case VAR_COORD_OP:
			case ASSIGN_VAR_COORD_OP:
			case VARATT_OP:
			case ASSIGN_VARATT_OP:
			case FILE_VAR_OP :
			case ASSIGN_FILE_VAR_OP :
			case FILEVAR_DIMNAME_OP:	
			case ASSIGN_FILEVAR_DIMNAME_OP:
			case PARAM_FILEVAR_DIMNAME_OP:
			case FILEVAR_DIMNUM_OP:	
			case ASSIGN_FILEVAR_DIMNUM_OP:
			case PARAM_FILEVAR_DIMNUM_OP:
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
				ptr++;lptr++;fptr++;
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
			default:
				break;
		}

		if(status < INFO) {
			if(*fptr == NULL) {
				NhlPError(status,E_UNKNOWN,"Execute: Error occured at or near line %d\n",(cmd_line ? (*lptr)-1: *lptr));
			} else {
				NhlPError(status,E_UNKNOWN,"Execute: Error occured at or near line %d in file %s\n", *lptr, *fptr);
			}
			if(status < WARNING) {
/*
* need to clean up stack !!! for current level
*/
				return(Ncl_ERRORS);
			}
		}	
		status = NOERROR;	
		ptr++;lptr++;fptr++;
	}
}

#ifdef __cplusplus
}
#endif


/*
 *      $Id: Machine.c,v 1.12 1994-04-07 16:48:13 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Machine.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Wed Jul 7 09:08:34 MDT 1993
 *
 *	Description:	Contains functions for manipulating stacks, frames
 *			and the instruction list.
 */
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <data_objs/NclData.h>
#include <data_objs/NclMultiDValData.h>
#include <data_objs/NclVar.h>
#include <data_objs/NclAtt.h>
#include <data_objs/DataSupport.h>
#include <defs.h>
#include <Symbol.h>
#include <errno.h>
#include <OpsList.h>
#include <Machine.h>

/*
* This is done so stack size and machine size can be configured at 
* compile time
*/
#ifndef NCL_LEVEL_1_SIZE
#define NCL_LEVEL_1_SIZE 512
#endif

#ifndef NCL_STACK_SIZE
#define NCL_STACK_SIZE 2048
#endif

#ifndef NCL_MACHINE_SIZE
#define NCL_MACHINE_SIZE 4096
#endif

/*
* Making this smaller because functions will generally be smaller than 
* whole programs.
*/
#ifndef NCL_FUNC_MACHINE_SIZE
#define NCL_FUNC_MACHINE_SIZE 512
#endif

NclStackEntry thestack[NCL_STACK_SIZE];

char *ops_strings[NUM_OPERATORS];

_NclMachineStack *mstk;

NclStackEntry  *level_1_vars;
int	current_level_1_size;

NclFrame *fp;
NclStackEntry *sb;
unsigned int current_scope_level = 1;

static void SetUpOpsStrings() {
	ops_strings[0] = "NOOP";
	ops_strings[NOOP] = "NOOP";
	ops_strings[STOPSEQ] = "STOPSEQ";
	ops_strings[ENDSTMNT_OP] = "ENDSTMNT_OP";
	ops_strings[RETURN_OP] = "RETURN_OP";
	ops_strings[FPDEF] = "FPDEF";
	ops_strings[NEW_FRAME_OP] = "NEW_FRAME_OP";
	ops_strings[BPROC_CALL_OP] = "BPROC_CALL_OP";
	ops_strings[FUNC_CALL_OP] = "FUNC_CALL_OP";
	ops_strings[INTRINSIC_FUNC_CALL] = "INTRINSIC_FUNC_CALL";
	ops_strings[INTRINSIC_PROC_CALL] = "INTRINSIC_PROC_CALL";
	ops_strings[PROC_CALL_OP] = "PROC_CALL_OP";
	ops_strings[JMP] = "JMP";
	ops_strings[JMPFALSE] = "JMPFALSE";
	ops_strings[IF_OP] = "IF_OP";
	ops_strings[DO_FROM_TO_OP] = "DO_FROM_TO_OP";
	ops_strings[DO_FROM_TO_STRIDE_OP] = "DO_FROM_TO_STRIDE_OP";
	ops_strings[DO_WHILE_OP] = "DO_WHILE_OP";
	ops_strings[BREAK_OP] = "BREAK_OP";
	ops_strings[CONTINUE_OP] = "CONTINUE_OP";
	ops_strings[CREATE_OBJ_OP] = "CREATE_OBJ_OP";
	ops_strings[SET_OBJ_OP] = "SET_OBJ_OP";
	ops_strings[GET_OBJ_OP] = "GET_OBJ_OP";
	ops_strings[CONVERT_TO_LOCAL] = "CONVERT_TO_LOCAL";
	ops_strings[PUSH_STRING_LIT_OP] = "PUSH_STRING_LIT_OP";
	ops_strings[PUSH_REAL_LIT_OP] = "PUSH_REAL_LIT_OP";
	ops_strings[PUSH_INT_LIT_OP] = "PUSH_INT_LIT_OP";
	ops_strings[ARRAY_LIT_OP] = "ARRAY_LIT_OP";
	ops_strings[NAMED_INT_SUBSCRIPT_OP] = "NAMED_INT_SUBSCRIPT_OP";
	ops_strings[INT_SUBSCRIPT_OP] = "INT_SUBSCRIPT_OP";
	ops_strings[NAMED_COORD_SUBSCRIPT_OP] = "NAMED_COORD_SUBSCRIPT_OP";
	ops_strings[COORD_SUBSCRIPT_OP] = "COORD_SUBSCRIPT_OP";
	ops_strings[SINGLE_INDEX_OP] = "SINGLE_INDEX_OP";
	ops_strings[DEFAULT_RANGE_OP] = "DEFAULT_RANGE_OP";
	ops_strings[BFUNC_CALL_OP] = "BFUNC_CALL_OP";
	ops_strings[RANGE_INDEX_OP] = "RANGE_INDEX_OP";
	ops_strings[NEG_OP] = "NEG_OP";
	ops_strings[NOT_OP] = "NOT_OP";
	ops_strings[MOD_OP] = "MOD_OP";
	ops_strings[OR_OP] = "OR_OP";
	ops_strings[AND_OP] = "AND_OP";
	ops_strings[XOR_OP] = "XOR_OP";
	ops_strings[LTSEL_OP] = "LTSEL_OP";
	ops_strings[GTSEL_OP] = "GTSEL_OP";
	ops_strings[PLUS_OP] = "PLUS_OP";
	ops_strings[MINUS_OP] = "MINUS_OP";
	ops_strings[MUL_OP] = "MUL_OP";
	ops_strings[MAT_OP] = "MAT_OP";
	ops_strings[DIV_OP] = "DIV_OP";
	ops_strings[EXP_OP] = "EXP_OP";
	ops_strings[LE_OP] = "LE_OP";
	ops_strings[GE_OP] = "GE_OP";
	ops_strings[GT_OP] = "GT_OP";
	ops_strings[LT_OP] = "LT_OP";
	ops_strings[EQ_OP] = "EQ_OP";
	ops_strings[NE_OP] = "NE_OP";
	ops_strings[FILE_VAR_OP] = "FILE_VAR_OP";
	ops_strings[ASSIGN_FILE_VAR_OP] = "ASSIGN_FILE_VAR_OP";
	ops_strings[PARAM_FILE_VAR_OP] = "PARAM_FILE_VAR_OP";
	ops_strings[FILEVAR_DIM_OP] = "FILEVAR_DIM_OP";
	ops_strings[ASSIGN_FILEVAR_DIM_OP] = "ASSIGN_FILEVAR_DIM_OP";
	ops_strings[PARAM_FILEVAR_DIM_OP] = "PARAM_FILEVAR_DIM_OP";
	ops_strings[FILEVAR_COORD_OP] = "FILEVAR_COORD_OP";
	ops_strings[ASSIGN_FILEVAR_COORD_OP] = "ASSIGN_FILEVAR_COORD_OP";
	ops_strings[PARAM_FILEVAR_COORD_OP] = "PARAM_FILEVAR_COORD_OP";
	ops_strings[FILEVARATT_OP] = "FILEVARATT_OP";
	ops_strings[ASSIGN_FILEVARATT_OP] = "ASSIGN_FILEVARATT_OP";
	ops_strings[PARAM_FILEVARATT_OP] = "PARAM_FILEVARATT_OP";
	ops_strings[VAR_READ_OP] = "VAR_READ_OP";
	ops_strings[ASSIGN_VAR_OP] = "ASSIGN_VAR_OP";
	ops_strings[PARAM_VAR_OP] = "PARAM_VAR_OP";
	ops_strings[VARATT_OP] = "VARATT_OP";
	ops_strings[ASSIGN_VARATT_OP] = "ASSIGN_VARATT_OP";
	ops_strings[PARAM_VARATT_OP] = "PARAM_VARATT_OP";
	ops_strings[VAR_COORD_OP] = "VAR_COORD_OP";
	ops_strings[VAR_READ_COORD_OP] = "VAR_READ_COORD_OP";
	ops_strings[ASSIGN_VAR_COORD_OP] = "ASSIGN_VAR_COORD_OP";
	ops_strings[PARAM_VAR_COORD_OP] = "PARAM_VAR_COORD_OP";
	ops_strings[VAR_DIM_OP]= "VAR_DIM_OP";
	ops_strings[ASSIGN_VAR_DIM_OP]= "ASSIGN_VAR_DIM_OP";
	ops_strings[PARAM_VAR_DIM_OP]= "PARAM_VAR_DIM_OP";
	ops_strings[ASSIGN_VAR_VAR_OP]= "ASSIGN_VAR_VAR_OP";
}

NclValue *_NclGetCurrentMachine
#if __STDC__
(void)
#else
()
#endif
{
	return(mstk->themachine);
}

NclStackEntry *_NclPeek
#if  __STDC__
(int offset)
#else
(offset)
	int offset;
#endif
{
	return((NclStackEntry*)(sb - (offset + 1)));
}

void _NclPutArg
#if  __STDC__
(NclStackEntry data, int arg_num,int total_args)
#else
(data,arg_num,total_args)
NclStackEntry data;
int arg_num;
int total_args;
#endif
{
	NclStackEntry *ptr;

	ptr = ((NclStackEntry*)(sb - total_args)) + arg_num;
	*ptr = data;
 
	return;
}

NclStackEntry _NclGetArg
#if  __STDC__
(int arg_num,int total_args)
#else
(arg_num,total_args)
int arg_num;
int total_args;
#endif
{
	NclStackEntry *ptr;

	ptr = ((NclStackEntry*)(sb - total_args)) + arg_num;

	return(*ptr);
}

int *_NclGetCurrentLineRec
#if __STDC__
(void)
#else
()
#endif
{
	return(mstk->thelines);
}

char **_NclGetCurrentFileNameRec
#if __STDC__
(void)
#else
()
#endif
{
	return(mstk->thefiles);
}

void _NclNewMachine
#if __STDC__
(void)
#else
()
#endif
{
	_NclMachineStack* tmp;
	tmp = (_NclMachineStack*)NclMalloc((unsigned)sizeof(_NclMachineStack));
	tmp->themachine = (NclValue*)NclCalloc(NCL_FUNC_MACHINE_SIZE,sizeof(NclValue));
	tmp->thefiles = (char**)NclCalloc(NCL_FUNC_MACHINE_SIZE,sizeof(char*));
	tmp->thelines = (int*)NclCalloc(NCL_FUNC_MACHINE_SIZE,sizeof(int));
	if(tmp->themachine == NULL ){
		NhlPError(NhlFATAL,errno,"_NhlNewMachine: Can't allocate space for new machine");
		return;
	}
	tmp->pc = tmp->themachine;
	tmp->lc = tmp->thelines;
	tmp->fn = tmp->thefiles;
	tmp->pcoffset = 0;
	tmp->current_machine_size = NCL_FUNC_MACHINE_SIZE;
	tmp->next = mstk;
	mstk = tmp;
}

void *_NclPopMachine
#if __STDC__
(void)
#else
()
#endif
{
	_NclMachineStack* tmp;

	tmp = mstk;
	mstk = mstk->next;
	tmp->next = NULL;
	return((void*)tmp);
}
void _NclPushMachine
#if __STDC__
(void * the_mach_rec)
#else
(the_mach_rec)
	void * the_mach_rec;
#endif
{
	_NclMachineStack* tmp;

	tmp = mstk;
	mstk = (_NclMachineStack*)the_mach_rec;
	mstk->next = tmp;
	return;
}

void _NclResetMachine
#if __STDC__
(void)
#else
()
#endif
{
	fp = NULL;
	if(sb != thestack) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"ResetMachine: reseting non-empty stack, memory may leak!");
	}
	sb = thestack;
	mstk->pcoffset = 0;
	mstk->pc = mstk->themachine;
	mstk->lc = mstk->thelines;
	mstk->fn = mstk->thefiles;
	return;
}

static NhlErrorTypes IncreaseMachineSize
#if __STDC__
(void)
#else 
()
#endif
{
	mstk->themachine = (NclValue*)NclRealloc(mstk->themachine,mstk->current_machine_size*2);
	mstk->thefiles = (char**)NclRealloc(mstk->themachine,mstk->current_machine_size*2);
	mstk->thelines = (int*)NclRealloc(mstk->themachine,mstk->current_machine_size*2);
	mstk->current_machine_size *=2;
	if(mstk->themachine == NULL) {
		NhlPError(NhlFATAL,errno,"IncreaseMachineSize: Unable to increase the size of the machine");
		return(NhlFATAL);

	}
/*
* Since a new pointer is possible here a new value of pc needs to be computed
* from the current pcoffset value
*/
	mstk->pc = &(mstk->themachine[mstk->pcoffset]);
	return(NhlNOERROR);
}
	

NhlErrorTypes _NclInitMachine
#if __STDC__
(void)
#else
()
#endif
{
	fp = (NclFrame*)thestack;
	sb = thestack;
	mstk = (_NclMachineStack*)NclMalloc((unsigned)sizeof(_NclMachineStack));
	mstk->themachine = (NclValue*)NclCalloc(NCL_MACHINE_SIZE,sizeof(NclValue));
	mstk->thefiles = (char**)NclCalloc(NCL_MACHINE_SIZE,sizeof(char*));
	mstk->thelines = (int*)NclCalloc(NCL_MACHINE_SIZE,sizeof(int));
	if(mstk->themachine == NULL ){
		NhlPError(NhlFATAL,errno,"_NhlInitMachine: Can't allocate space for machine");
		return(NhlFATAL);
	}
	mstk->pc = mstk->themachine;
	mstk->lc = mstk->thelines;
	mstk->fn = mstk->thefiles;
	mstk->pcoffset = 0;
	mstk->current_machine_size = NCL_MACHINE_SIZE;
	mstk->next = NULL;
	SetUpOpsStrings();
/*
* Now set up level 1 variable storage locations
*/
	
	level_1_vars = (NclStackEntry*)NclCalloc(NCL_LEVEL_1_SIZE,
		sizeof(NclStackEntry));
	current_level_1_size = NCL_LEVEL_1_SIZE;
	if(level_1_vars == NULL) {
		NhlPError(NhlFATAL,errno,"_NhlInitMachine: Can't allocate space for machine");
		return(NhlFATAL);
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclPutLevel1Var
#if  __STDC__
(int offset,NclStackEntry *therec) 
#else
(offset,therec)
	int offset;
	NclStackEntry *therec;
#endif
{ 	
	if((offset >= current_level_1_size)||(offset < 0)){
		return(NhlWARNING);
	} else {
		
		level_1_vars[offset] = *therec;
		return(NhlNOERROR);
	}
}
NclStackEntry *_NclGetLevel1Var
#if  __STDC__
(int offset) 
#else
(offset)
	int offset;
#endif
{ 	
	if((offset >= current_level_1_size)||(offset < 0)){
		return(NULL);
	} else {
		return(&(level_1_vars[offset]));
	}
}

NhlErrorTypes _NclPutRec
#if  __STDC__
(NclSymbol* the_sym,NclStackEntry *therec)
#else
(the_sym,therec)
NclSymbol* the_sym;
NclStackEntry *therec;
#endif
{
	int i;
	NclFrame *previous;

	i = current_scope_level;
	

	if(the_sym->level == 1) {
		return(_NclPutLevel1Var(the_sym->offset,therec));
	} else {
		previous = (NclFrame*)fp;
		while(i != the_sym->level) {
			i--;
			previous = (NclFrame*)((NclStackEntry*)thestack + ((NclStackEntry*)previous)->u.offset);
		}
/*
* increment over stack frame stuff to base of actual scope
*/
		previous++;
		*((NclStackEntry*)((NclStackEntry*)previous + the_sym->offset)) = *therec;
		return(NhlNOERROR);
	}
}

NclStackEntry *_NclRetrieveRec
#if  __STDC__
(NclSymbol* the_sym)
#else
(the_sym)
NclSymbol* the_sym;
#endif
{
	int i;
	NclFrame *previous;

	i = current_scope_level;
	

	if(the_sym->level == 1) {
		return(_NclGetLevel1Var(the_sym->offset));
	} else if(the_sym->level != 0){
		previous = (NclFrame*)fp;
		while(i != the_sym->level) {
			i--;
			previous = (NclFrame*)((NclStackEntry*)thestack + ((NclStackEntry*)previous)->u.offset);
		}
/*
* increment over stack frame stuff to base of actual scope
*/
		previous++;
		return((NclStackEntry*)((NclStackEntry*)previous + the_sym->offset));
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference keyword (%s) as variable",the_sym->name);
		return(NULL);
	}
}

static struct _NclFrameList flist ;

static void SaveFramePtrNLevel
#if __STDC__
(struct _NclFrame *tmp_fp,int level)
#else
(tmp_fp,level)
	struct _NclFrame *tmp_fp;
	int level;
#endif
{
	static inited = 0;

	struct _NclFrameList *tmp;

	if(!inited) {
		flist.next = NULL;
		flist.fp = NULL;
		flist.level = -1;
		inited = 1;
	}
	tmp = flist.next;

	flist.next = (NclFrameList*)NclMalloc((unsigned)sizeof(NclFrameList));
	flist.next->fp = tmp_fp;
	flist.next->level = level;
	flist.next->next = tmp;
}

static void SetNextFramePtrNLevel
#if __STDC__
(void)
#else
()
#endif
{
	struct _NclFrameList *tmp;

	tmp = flist.next;
	if(tmp != NULL) {
		flist.next = flist.next->next;
		fp = tmp->fp;
		current_scope_level = tmp->level;
		NclFree(tmp);
	}
	
}


void _NclPushFrame
#if __STDC__
(NclSymTableListNode *new_scope,unsigned long next_instr_offset,int nargs)
#else
(new_scope,next_instr_offset,nargs)
	NclSymTableListNode *new_scope;
	unsigned long next_instr_offset;
	int nargs;
#endif
{
	NclFrame *tmp,*tmp_fp; 
	NclFrame *previous; 
	int i;

	previous = (NclFrame*)fp;	

	tmp = (NclFrame*)(sb);
	tmp->func_ret_value.kind = NclStk_RETURNVAL;
	tmp->func_ret_value.u.data_obj= NULL;
	if(new_scope->level == current_scope_level+1) {
		tmp->static_link.u.offset  = (unsigned long)((NclStackEntry*)previous - (NclStackEntry*)thestack);
		tmp->static_link.kind = NclStk_STATIC_LINK;
	} else if(new_scope->level == current_scope_level) {
		tmp->static_link = previous->static_link;
	} else  {
		i = current_scope_level - new_scope->level;
		while(i-- >= 0) {
			previous = (NclFrame*)((NclStackEntry*)thestack + previous->static_link.u.offset);
		}
		tmp->static_link.u.offset = (unsigned long)((NclStackEntry*)previous - (NclStackEntry*)thestack);
		tmp->static_link.kind = NclStk_STATIC_LINK;
	}
	tmp->dynamic_link.u.offset  = (unsigned long)((NclStackEntry*)previous - (NclStackEntry*)thestack);
	tmp->dynamic_link.kind = NclStk_DYNAMIC_LINK;

/*
* Maybe should be pcoffset + 1???
*/
	tmp->return_pcoffset.u.offset = next_instr_offset ;
	tmp->return_pcoffset.kind = NclStk_RET_OFFSET;
	if(nargs > 0) {
		tmp->parameter_map.kind = NclStk_PARAMLIST;
		tmp->parameter_map.u.the_list = (NclParamRecList*)NclMalloc(
					(unsigned) sizeof(NclParamRecList) * nargs);
		for(i = 0 ; i< nargs; i++) {
			tmp->parameter_map.u.the_list[i].p_type = NONE_P;
			tmp->parameter_map.u.the_list[i].var_sym = NULL;
			tmp->parameter_map.u.the_list[i].rec = NULL;
		}
	} else {
		tmp->parameter_map.u.the_list = NULL;
	}
	tmp_fp = tmp;
	
	tmp++;
	sb = (NclStackEntry*)tmp;
/*
	current_scope_level = new_scope->level;
*/

/* 
* the frame pointer needs to be returned so it can be set after all of the
* arguments have been pushed on to the stack. If this isn't done then the
*  the _NclRetrieveRec function won't be able to find the correct parameters
* referenced by the instruction sequence. Same goes for the current scope
* level.
*/
	SaveFramePtrNLevel(tmp_fp,new_scope->level);
	return;
}

void _NclFinishFrame
#if __STDC__
(void)
#else
()
#endif
{
	SetNextFramePtrNLevel();
	return;
}

void *_NclLeaveFrame
#if __STDC__
(void)
#else
()
#endif
{
	NclFrame * prev;
/*
	sb = &(fp->func_ret_value);
*/
	prev = fp;
	fp = (NclFrame*)(thestack + fp->dynamic_link.u.offset);
	return((void*)prev);
}

void _NclPush
#if __STDC__
(NclStackEntry data)
#else
(data)
	NclStackEntry data;
#endif
{
	*(sb) = data;
	sb++;
	if((sb) >= &(thestack[NCL_STACK_SIZE -1]) ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Push: Stack overflow");
	}
	return;
}

NclStackEntry _NclPop
#if __STDC__
(void)
#else
()
#endif
{
	NclStackEntry tmp;
	if(sb <= thestack) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Pop: Stack underflow");
		tmp.kind = NclStk_NOVAL;
		tmp.u.offset = 0;
		return(tmp);
	} else {
		sb--;
		tmp = (*(sb));
		sb->kind = NclStk_NOVAL;
		sb->u.offset = 0;
		return(tmp);
	}
}
int _NclPutIntInstr
#if __STDC__
(int val,int line,char* file)
#else
(val,line,file)
        int val;
        int line;
        char *file;
#endif
{
/* 
* Sometimes the parser needs to know the offset of the instruction it put
* into the instruction list (i.e. function return addresses, loops and 
* conditionals. Therefore it is necessary to return the offset of the instruct
* being placed in the list.
*/
	int old_offset = (int)(mstk->pc - mstk->themachine);

/*
* Check for overflow
*/
	if(mstk->pc >= &(mstk->themachine[mstk->current_machine_size -1])) {
/*
* Will take care of updating mstk->pc
*/
		IncreaseMachineSize();
	}
	*((int*)mstk->pc++) = val;
	*(mstk->lc++) = line;
	*(mstk->fn++) = file;
	mstk->pcoffset = (int)(mstk->pc - mstk->themachine);

	return(old_offset);
}

int _NclPutRealInstr
#if __STDC__
(float val,int line,char* file)
#else
(val,line,file)
	float val;
	int line;
	char *file;
#endif
{
/* 
* Sometimes the parser needs to know the offset of the instruction it put
* into the instruction list (i.e. function return addresses, loops and 
* conditionals. Therefore it is necessary to return the offset of the instruct
* being placed in the list.
*/
	int old_offset = (int)(mstk->pc - mstk->themachine);

/*
* Check for overflow
*/
	if(mstk->pc >= &(mstk->themachine[mstk->current_machine_size -1])) {
/*
* Will take care of updating mstk->pc
*/
		IncreaseMachineSize();
	}
	*((float*)mstk->pc++) = val;
	*(mstk->lc++) = line;
	*(mstk->fn++) = file;
	mstk->pcoffset = (int)(mstk->pc - mstk->themachine);

	return(old_offset);
}
int _NclPutInstr
#if __STDC__
(NclValue val,int line, char* file)
#else
(val,line,file)
	NclValue val;
	int line;
	char *file;
#endif
{
/* 
* Sometimes the parser needs to know the offset of the instruction it put
* into the instruction list (i.e. function return addresses, loops and 
* conditionals. Therefore it is necessary to return the offset of the instruct
* being placed in the list.
*/
	int old_offset = (int)(mstk->pc - mstk->themachine);

/*
* Check for overflow
*/
	if(mstk->pc >= &(mstk->themachine[mstk->current_machine_size -1])) {
/*
* Will take care of updating mstk->pc
*/
		IncreaseMachineSize();
	}
	*(mstk->pc++) = val;
	*(mstk->lc++) = line;
	*(mstk->fn++) = file;
	mstk->pcoffset = (int)(mstk->pc - mstk->themachine);

	return(old_offset);
}

int _NclGetCurrentOffset
#if __STDC__
(void)
#else
()
#endif
{
	return(mstk->pcoffset);
}

int _NclPutInstrAt
#if __STDC__
(int offset,NclValue val,int line, char* file)
#else 
(offset, val, line, file)
	int offset;
	NclValue val;
	int line;
	char *file;
#endif
{
	NclValue *ptr;
	int *lptr;
	char **fptr;

	ptr = (NclValue*)(mstk->themachine + offset);
	lptr = (int*)(mstk->thelines + offset);
	fptr = (char**)(mstk->thefiles+ offset);

	*ptr = val;
	*lptr = line;
	*fptr = file;

	return(offset);
}



void _NclPrintMachine
#if  __STDC__
(int from, int to,FILE* fp)
#else
(from,to,fp)
	int from;
	int to;
	FILE *fp;
#endif
{
	NclValue *ptr;
	NclValue *eptr;
	int	*lptr;
	char **fptr;

	if(from == -1){
		from = 0;
	}
	if(to == -1) {
		to = mstk->pcoffset;
	}

	ptr = (NclValue*)(mstk->themachine + from);
	eptr = (NclValue*)(mstk->themachine + to);
	lptr = (int*)(mstk->thelines+from);		
	fptr = (char**)(mstk->thefiles+from);
	
	while(ptr != eptr) {
		if(*fptr != NULL) {
			fprintf(fp,"(%d,%d,%s)\t",(int)(ptr-mstk->themachine),*lptr,*fptr);
		} else {
			fprintf(fp,"(%d,%d)\t",(int)(ptr-mstk->themachine),*lptr);
		}
		switch(*ptr) {
			case NOOP :
			case STOPSEQ:
			case RETURN_OP :
			case IF_OP :
			case NAMED_COORD_SUBSCRIPT_OP :
			case INT_SUBSCRIPT_OP :
			case NAMED_INT_SUBSCRIPT_OP :
			case COORD_SUBSCRIPT_OP :
			case SINGLE_INDEX_OP :
			case DEFAULT_RANGE_OP :
			case RANGE_INDEX_OP :
			case NEG_OP :
			case NOT_OP :
			case MOD_OP :
			case OR_OP :
			case AND_OP :
			case XOR_OP :
			case LTSEL_OP :
			case GTSEL_OP :
			case PLUS_OP :
			case MINUS_OP :
			case MUL_OP :
			case MAT_OP :
			case DIV_OP :
			case EXP_OP :
			case LE_OP :
			case GE_OP :
			case GT_OP :
			case LT_OP :
			case EQ_OP :
			case NE_OP :
			case BREAK_OP:
			case CONTINUE_OP:
			case ENDSTMNT_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				break;
			case JMP :
			case JMPFALSE :
			case CREATE_OBJ_OP :
			case SET_OBJ_OP :
			case GET_OBJ_OP :
			case PUSH_INT_LIT_OP :
			case ARRAY_LIT_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case PUSH_STRING_LIT_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				break;
			case NEW_FRAME_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case PROC_CALL_OP:
			case BPROC_CALL_OP:
			case FUNC_CALL_OP:
			case BFUNC_CALL_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				break;
			case INTRINSIC_FUNC_CALL:
			case INTRINSIC_PROC_CALL:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d",(int)*ptr);
				break;
			case DO_FROM_TO_OP :
			case DO_FROM_TO_STRIDE_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",*ptr);
				break;
			case PUSH_REAL_LIT_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%g\n",*(float*)ptr);
				break;
			case DO_WHILE_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case FILEVAR_COORD_OP:
			case ASSIGN_FILEVAR_COORD_OP:
			case PARAM_FILEVAR_COORD_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case VAR_COORD_OP:
			case VAR_READ_COORD_OP:
			case ASSIGN_VAR_COORD_OP:
			case PARAM_VAR_COORD_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case FILEVARATT_OP:
			case ASSIGN_FILEVARATT_OP:
			case PARAM_FILEVARATT_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case VARATT_OP:
			case ASSIGN_VARATT_OP:
			case PARAM_VARATT_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%s\n",NrmQuarkToString(*ptr));
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",(int)*ptr);
				break;
			case FILE_VAR_OP :
			case ASSIGN_FILE_VAR_OP :
			case PARAM_FILE_VAR_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				fprintf(fp,"%s\n",NrmQuarkToString(*ptr),fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d",*ptr);
				break;
			case VAR_READ_OP :
			case ASSIGN_VAR_OP :
			case PARAM_VAR_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",*ptr);
				break;
			case ASSIGN_VAR_VAR_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",*ptr);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t%d\n",*ptr);
				break;
			case FPDEF:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				_NclPushMachine(((NclSymbol*)*ptr)->u.procfunc->mach_rec_ptr);
				_NclPrintMachine(-1,-1,fp);
				(void)_NclPopMachine();
				break;
			case CONVERT_TO_LOCAL:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++;fptr++;
				fprintf(fp,"\tArg #:");
				fprintf(fp,"%d\n",(int)*ptr,fp);
				break;
			case VAR_DIM_OP:
			case ASSIGN_VAR_DIM_OP:
			case PARAM_VAR_DIM_OP:
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++,fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				break;
			case FILEVAR_DIM_OP :
			case ASSIGN_FILEVAR_DIM_OP :
			case PARAM_FILEVAR_DIM_OP :
				fprintf(fp,"%s\n",ops_strings[*ptr]);
				ptr++;lptr++,fptr++;
				fprintf(fp,"\t");
				_NclPrintSymbol((NclSymbol*)*ptr,fp);
				ptr++;lptr++,fptr++;
				fprintf(fp,"\t");
				fprintf(fp,"%s\n",NrmQuarkToString(*ptr));
				break;
			default:
				break;
		}
		ptr++;lptr++;fptr++;
	}
	return;
}


extern void _NclAddObjToParamList
#if  __STDC__
(struct _NclObjRec *obj, int arg_num)
#else
(obj,arg_num)
	struct _NclObjRec *obj;
	int arg_num;
#endif 
{
	NclMultiDValData tmp_md;
	NclMultiDValData tmp_var;
	NclFrame *tmp_fp;
/*
* guarenteed to have next frame pointer in the flist stack.
*/
	tmp_fp = flist.next->fp;
	if(obj->obj.obj_type_mask & NCL_VAR_TYPE_MASK) {
		tmp_fp->parameter_map.u.the_list[arg_num].p_type = VAR_P;
		tmp_fp->parameter_map.u.the_list[arg_num].var_sym = ((NclVar)obj)->var.thesym;
		tmp_fp->parameter_map.u.the_list[arg_num].var_ptr = (NclVar)obj;
		if((((NclVar)obj)->var.var_type == VARSUBSEL)
			||(((NclVar)obj)->var.var_type == COORDSUBSEL)
			||(((NclVar)obj)->var.var_type == FILEVARSUBSEL)) {
		
			tmp_md = (NclMultiDValData)_NclGetObj(((NclVar)obj)->var.thevalue_id);
			if(tmp_md->multidval.sel_rec != NULL) {
				tmp_fp->parameter_map.u.the_list[arg_num].rec = (NclSelectionRecord*)NclMalloc((unsigned)sizeof(NclSelectionRecord));
            			memcpy((char*)tmp_fp->parameter_map.u.the_list[arg_num].rec,(char*)tmp_md->multidval.sel_rec,sizeof(NclSelectionRecord));

			} else {
				tmp_fp->parameter_map.u.the_list[arg_num].rec = NULL;
			}
		}
	} else if(obj->obj.obj_type_mask & NCL_VAL_TYPE_MASK) {
		tmp_fp->parameter_map.u.the_list[arg_num].p_type = VALUE_P;
		tmp_fp->parameter_map.u.the_list[arg_num].var_sym = NULL;
		tmp_fp->parameter_map.u.the_list[arg_num].var_ptr = NULL;
		tmp_fp->parameter_map.u.the_list[arg_num].rec = NULL;
	} else {
		tmp_fp->parameter_map.u.the_list[arg_num].p_type = NONE_P;
		tmp_fp->parameter_map.u.the_list[arg_num].var_sym = NULL;
		tmp_fp->parameter_map.u.the_list[arg_num].var_ptr = NULL;
		tmp_fp->parameter_map.u.the_list[arg_num].rec = NULL;
/*
* handle files 
*/
	}
	
}

void _NclRemapParameters
#if __STDC__
(int nargs,void *previous_fp,int from)
#else
(nargs,previous_fp,from)
	int nargs;
	void *previous_fp;
	int from;
#endif
{
	NclParamRecList *the_list;
	NclStackEntry data,*data_ptr;
	NclObjTypes param_rep_type,var_rep_type;
	NclVar anst_var = NULL, tmp_var = NULL,tmp_var1 = NULL;
	int i = 0,j = 0,contains_vec = 0;
	NclFrame* tmp_fp = (NclFrame*)previous_fp;
	int check_ret_status = 0;
	int value_ref_count = 0;
	int coord_ids[NCL_MAX_DIMENSIONS];
	NclAtt tmp_att;
	NclVar tmp_coord_var;
/*
* Some kind of check is need to assure top of stack and arguments are 
* aligned before the following loop starts up
*/
	switch(from) {
	case FUNC_CALL_OP:
		check_ret_status = 1;
		break;
	default:
		check_ret_status = 0;
		break;
	}

	the_list = tmp_fp->parameter_map.u.the_list;
	for (i = 0; i < nargs; i++) {
		data = _NclPop();
		if(the_list[i].p_type == VAR_P) {
			if((the_list[i].var_sym != NULL)&&(the_list[i].rec != NULL)) {

/* 
* Key thing to think about here: if there is a subscript record or no variable 
* symbol then the value associated with a parameter variable is an array subsection
* or the result of an expression which can be converted to a temporary value if it has 
* been placed in the return value spot of the frame. Otherwise, if there is a sym and
* no selection record then the variable must be copied into a temporary variable.
*/ 



/*
* Variables regardless of subsectioning or type will share att id with parameter
* variable. So only coordinate variables, type and actual data array values 
* must bew remapped.
*/
				for(j = 0; j< the_list[i].rec->n_entries; j ++) {
					if(the_list[i].rec->selection[j].sel_type == Ncl_VECSUBSCR) {
						contains_vec = 1;
					}
				}
				data_ptr = _NclRetrieveRec(the_list[i].var_sym);
				anst_var = data_ptr->u.data_var;
				if(data.u.data_var != NULL ) {
					var_rep_type = _NclGetVarRepValue(data.u.data_var);
					param_rep_type = _NclGetVarRepValue(anst_var);
					if((var_rep_type != param_rep_type)&&(!contains_vec)) {
/*
* Try reverse coercion which in many cases will
* just fail.
*/
					} else {
						if(!contains_vec) 
							_NclAssignVarToVar(anst_var,the_list[i].rec,data.u.data_var,NULL);

					}
/* 
* ------->
* Need to deal with mapping coordinates, attributes and dim_info. THis is
* not trivial since remapping can cause coord arrays to expand to the
* size of target dimension and be partially filled in with missing values 
* <-------
*/
				
					if((the_list[i].var_ptr != NULL)&&(anst_var->obj.id != the_list[i].var_ptr->obj.id)) {
						_NclDestroyObj((NclObj)the_list[i].var_ptr);
					}

					value_ref_count = _NclGetObjRefCount(data.u.data_var->var.thevalue_id);

					if((check_ret_status)&&(tmp_fp->func_ret_value.kind == NclStk_VAR)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)&&(value_ref_count == 1)) {
/* data better be a variable otherwise something is majorly hosed */
/* Here the object is available to be forced into a temporary variable */

							tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
							tmp_var = _NclVarCreate(
								NULL,
								tmp_var1->obj.class_ptr,
								tmp_var1->obj.obj_type,
								tmp_var1->obj.obj_type_mask,
								NULL,
								(NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),
								tmp_var1->var.dim_info,
								tmp_var1->var.att_id,
								tmp_var1->var.coord_vars,
								RETURNVAR,
								NULL
								);
							_NclDestroyObj((NclObj)tmp_var1);
							tmp_fp->func_ret_value.u.data_var = tmp_var;
							check_ret_status = 0;
						
					} else if((check_ret_status)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)){
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						if(tmp_var1->var.att_id != -1) 
							tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(tmp_var1->var.att_id),NULL);
						for(j = 0; j< tmp_var1->var.n_dims; j++) {
							if(tmp_var1->var.coord_vars[j] != -1) {
								tmp_coord_var = _NclCopyVar((NclVar)_NclGetObj(tmp_var1->var.coord_vars[j]),NULL,NULL);
								coord_ids[j] = tmp_coord_var->obj.id;
							} else {
								coord_ids[j] = -1;
							}
						}
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							_NclCopyVal((NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),NULL),
							tmp_var1->var.dim_info,
							(tmp_att == NULL ? -1:tmp_att->obj.id),
							coord_ids,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
					} else {
						_NclDestroyObj((NclObj)data.u.data_var);
					}
				} else {
					if((the_list[i].var_ptr != NULL)&&(anst_var->obj.id != the_list[i].var_ptr->obj.id)) {
						_NclDestroyObj((NclObj)the_list[i].var_ptr);
					}
				}
			} else if(the_list[i].var_sym != NULL){
				data_ptr = _NclRetrieveRec(the_list[i].var_sym);
				anst_var = data_ptr->u.data_var;
				if((anst_var->var.att_id == -1)&&(data.u.data_var->var.att_id != -1)) {
					anst_var->var.att_id = data.u.data_var->var.att_id;
					_NclAddParent(_NclGetObj(anst_var->var.att_id),(NclObj)anst_var);
				} else if((anst_var->var.att_id != -1)&&(data.u.data_var->var.att_id == -1)){

					_NclDelParent(_NclGetObj(anst_var->var.att_id),(NclObj)anst_var);
					anst_var->var.att_id = -1;
				} else if(anst_var->var.att_id != data.u.data_var->var.att_id) {
					_NclDelParent(_NclGetObj(anst_var->var.att_id),(NclObj)anst_var);
					anst_var->var.att_id = data.u.data_var->var.att_id;
					_NclAddParent(_NclGetObj(anst_var->var.att_id),(NclObj)anst_var);
				}
				for(j = 0 ; j < data.u.data_var->var.n_dims; j++) {
					if((anst_var->var.coord_vars[j] != -1)&&(data.u.data_var->var.coord_vars[j] == -1)) {
						_NclDelParent(_NclGetObj(anst_var->var.coord_vars[j]),(NclObj)anst_var);
						anst_var->var.coord_vars[j] = -1;
						
						
					} else if((anst_var->var.coord_vars[j] == -1)&&(data.u.data_var->var.coord_vars[j] == -1)) {
						anst_var->var.coord_vars[j] = data.u.data_var->var.coord_vars[j];
						_NclAddParent(_NclGetObj(anst_var->var.coord_vars[j]),(NclObj)anst_var);
				
					} else if(anst_var->var.coord_vars[j] != data.u.data_var->var.coord_vars[j]) {
						_NclDelParent(_NclGetObj(anst_var->var.coord_vars[j]),(NclObj)anst_var);
						anst_var->var.coord_vars[j] = data.u.data_var->var.coord_vars[j];
						_NclAddParent(_NclGetObj(anst_var->var.coord_vars[j]),(NclObj)anst_var);
					} 
					anst_var->var.dim_info[j] = data.u.data_var->var.dim_info[j];
				}
				value_ref_count = _NclGetObjRefCount(data.u.data_var->var.thevalue_id);

				if((check_ret_status)&&(tmp_fp->func_ret_value.kind == NclStk_VAR)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)&&(value_ref_count == 1)) {
/* data better be a variable otherwise something is majorly hosed */
/* Here the object is available to be forced into a temporary variable */
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							(NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),
							tmp_var1->var.dim_info,
							tmp_var1->var.att_id,
							tmp_var1->var.coord_vars,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
						
				} else if((check_ret_status)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)){
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						if(tmp_var1->var.att_id != -1) 
							tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(tmp_var1->var.att_id),NULL);
						for(i = 0; i< tmp_var1->var.n_dims; i++) {
							if(tmp_var1->var.coord_vars[i] != -1) {
								tmp_coord_var = _NclCopyVar((NclVar)_NclGetObj(tmp_var1->var.coord_vars[i]),NULL,NULL);
								coord_ids[i] = tmp_coord_var->obj.id;
							} else {
								coord_ids[i] = -1;
							}
						}
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							_NclCopyVal((NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),NULL),
							tmp_var1->var.dim_info,
							(tmp_att == NULL ? -1:tmp_att->obj.id),
							coord_ids,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
				} else {
					_NclDestroyObj((NclObj)data.u.data_var);
				}
			} else {
/*
* In this situation the variable is a return value which isn't attached to a symbol
*/
				value_ref_count = _NclGetObjRefCount(data.u.data_var->var.thevalue_id);

				if((check_ret_status)&&(tmp_fp->func_ret_value.kind == NclStk_VAR)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)&&(value_ref_count == 1)) {
/* data better be a variable otherwise something is majorly hosed */
/* Here the object is available to be forced into a temporary variable */
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							(NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),
							tmp_var1->var.dim_info,
							tmp_var1->var.att_id,
							tmp_var1->var.coord_vars,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
						
				} else if((check_ret_status)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)){
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						if(tmp_var1->var.att_id != -1) 
							tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(tmp_var1->var.att_id),NULL);
						for(i = 0; i< tmp_var1->var.n_dims; i++) {
							if(tmp_var1->var.coord_vars[i] != -1) {
								tmp_coord_var = _NclCopyVar((NclVar)_NclGetObj(tmp_var1->var.coord_vars[i]),NULL,NULL);
								coord_ids[i] = tmp_coord_var->obj.id;
							} else {
								coord_ids[i] = -1;
							}
						}
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							_NclCopyVal((NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),NULL),
							tmp_var1->var.dim_info,
							(tmp_att == NULL ? -1:tmp_att->obj.id),
							coord_ids,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
				} else {
					_NclDestroyObj((NclObj)data.u.data_var);
				}
			}
		} else if(the_list[i].p_type == VALUE_P) {
			if(data.kind == NclStk_VAR) { 
/*
* Need to turn data part into a temporary variable and still destroy variable
*   >Does not currently do this for now it does the same thing as if it were
*    a variable parameter<
*/
				value_ref_count = _NclGetObjRefCount(data.u.data_var->var.thevalue_id);

				if((check_ret_status)&&(tmp_fp->func_ret_value.kind == NclStk_VAR)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)&&(value_ref_count == 1)) {
/* data better be a variable otherwise something is majorly hosed */
/* Here the object is available to be forced into a temporary variable */
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							(NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),
							tmp_var1->var.dim_info,
							tmp_var1->var.att_id,
							tmp_var1->var.coord_vars,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
						
				} else if((check_ret_status)&&(tmp_fp->func_ret_value.u.data_var->obj.id == data.u.data_var->obj.id)){
						tmp_var1 = (NclVar)_NclGetObj(data.u.data_var->obj.id);
						if(tmp_var1->var.att_id != -1) 
							tmp_att = _NclCopyAtt((NclAtt)_NclGetObj(tmp_var1->var.att_id),NULL);
						for(i = 0; i< tmp_var1->var.n_dims; i++) {
							if(tmp_var1->var.coord_vars[i] != -1) {
								tmp_coord_var = _NclCopyVar((NclVar)_NclGetObj(tmp_var1->var.coord_vars[i]),NULL,NULL);
								coord_ids[i] = tmp_coord_var->obj.id;
							} else {
								coord_ids[i] = -1;
							}
						}
						tmp_var = _NclVarCreate(
							NULL,
							tmp_var1->obj.class_ptr,
							tmp_var1->obj.obj_type,
							tmp_var1->obj.obj_type_mask,
							NULL,
							_NclCopyVal((NclMultiDValData)_NclGetObj(tmp_var1->var.thevalue_id),NULL),
							tmp_var1->var.dim_info,
							(tmp_att == NULL ? -1:tmp_att->obj.id),
							coord_ids,
							RETURNVAR,
							NULL
							);
						_NclDestroyObj((NclObj)tmp_var1);
						tmp_fp->func_ret_value.u.data_var = tmp_var;
						check_ret_status = 0;
				} else {
					_NclDestroyObj((NclObj)data.u.data_var);
				}
			} else {
				_NclDestroyObj((NclObj)data.u.data_var);
			}
		}
	}
}

void _NclDumpStack
#if __STDC__
(FILE *fp)
#else 
(fp) 
	FILE *fp;
#endif
{
	NclStackEntry *tmp_ptr = sb;

	fprintf(fp,"\n");
	tmp_ptr = sb - 1;
	while(tmp_ptr >= thestack) {
		fprintf(fp,"%d)\t",(int)(tmp_ptr - thestack));
		switch(tmp_ptr->kind) {
		case	NclStk_NOVAL:
			fprintf(fp,"NclStk_NOVAL\n");
			break;
		case	NclStk_OFFSET:
			fprintf(fp,"NclStk_OFFSET\n");
			break;
		case 	NclStk_VAL:
			fprintf(fp,"NclStk_VAL\n");
			break;
		case 	NclStk_VAR:
			fprintf(fp,"NclStk_VAR\n");
			break;
		case 	NclStk_SUBREC:
			fprintf(fp,"NclStk_SUBREC\n");
			break;
		case	NclStk_PARAMLIST:
			fprintf(fp,"NclStk_PARAMLIST\n");
			break;
		case	NclStk_RANGEREC:
			fprintf(fp,"NclStk_RANGEREC\n");
			break;
		case	NclStk_VECREC:
			fprintf(fp,"NclStk_VECREC\n");
			break;
		case	NclStk_FILE:	
			fprintf(fp,"NclStk_FILE\n");
			break;
		case 	NclStk_GRAPHIC:
			fprintf(fp,"NclStk_GRAPHIC\n");
			break;
        	case	NclStk_STATIC_LINK:
			fprintf(fp,"NclStk_STATIC_LINK\t%d\n",(int)(tmp_ptr->u.offset ));
			break;
        	case	NclStk_DYNAMIC_LINK:
			fprintf(fp,"NclStk_DYNAMIC_LINK\t%d\n",(int)(tmp_ptr->u.offset));
			break;
        	case	NclStk_RET_OFFSET:
			fprintf(fp,"NclStk_RET_OFFSET\t%d\n",(int)(tmp_ptr->u.offset));
			break;
		case 	NclStk_RETURNVAL:
			fprintf(fp,"------------FRAME--------------\n");
			break;
		default:
			fprintf(fp,"\n");
			break;
		}
		tmp_ptr--;
	}
	fprintf(fp,"\n");
}

NhlErrorTypes _NclPlaceReturn
#if __STDC__
(struct _NclStackEntry data)
#else
(data)
	struct NclStackEntry data;
#endif
{
	fp->func_ret_value = data;
	return(NhlNOERROR);
}


#ifdef __cplusplus
}
#endif

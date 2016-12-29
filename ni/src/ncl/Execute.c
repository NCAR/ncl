/*
 *      $Id: Execute.c,v 1.145 2010/05/04 00:35:44 dbrown Exp $
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
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/nfp/nctime.h>

#include "defs.h"
#include "Symbol.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "Machine.h"
#include "NclFileInterfaces.h"
#include "NclFile.h"
#ifdef USE_NETCDF4_FEATURES
#include "NclAdvancedFile.h"
#endif
#include "NclGroup.h"
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
#include "NclList.h"
#include "NclHLUObj.h"
#include "TypeSupport.h"
#include "HLUSupport.h"
#include "ListSupport.h"
#include "NclProf.h"
#include <errno.h>

extern int cmd_line;

extern void _NclHLUVarValChange(
#if     NhlNeedProto
NhlArgVal cbdata, NhlArgVal udata
#endif
);
typedef struct exe_stack_node {
        NclValue *ptr;
        int *lptr;
        char **fptr;
        NclValue *machine;
        struct exe_stack_node *next;
} ExeStackNode;
ExeStackNode handle = { NULL,NULL,NULL,NULL,NULL};
NclValue *ptr;
int *lptr;
char **fptr;
NclValue *machine;
NhlErrorTypes estatus = NhlNOERROR;
static int level = 0;
static short _ItIsNclReassign = 0;

static void _NclPushExecute
#if     NhlNeedProto
(void)
#else
()
#endif
{
        ExeStackNode *tmp = handle.next;

        handle.next = (ExeStackNode*)NclMalloc(sizeof(ExeStackNode));
        handle.next->ptr = ptr;
        handle.next->lptr = lptr;
        handle.next->fptr= fptr;
        handle.next->machine= machine;
        handle.next->next= tmp;
}
static void _NclPopExecute
#if     NhlNeedProto
(void)
#else
()
#endif
{
        ExeStackNode *tmp = handle.next;

        if(handle.next != NULL) {
                ptr = handle.next->ptr;
                lptr = handle.next->lptr;
                fptr =  handle.next->fptr;
                machine = handle.next->machine;
                handle.next= handle.next->next;
                NclFree(tmp);
        }
}

#ifdef USE_NETCDF4_FEATURES
static NclSelectionRecord* _NclAllocateAdvancedFileSelPointer(NclFile file, NclQuark var,
                                                              int nsubs, NhlErrorTypes* estatus)
{
    NclAdvancedFile advancedfile = (NclAdvancedFile)file;
    NclFileVarNode *varnode = NULL;
    NclSelectionRecord* sel_ptr = NULL;

    *estatus = NhlNOERROR;

    varnode = _getVarNodeFromNclFileGrpNode(advancedfile->advancedfile.grpnode, var);
    if(NULL == varnode)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"variable (%s) is not in file (%s)",
                   NrmQuarkToString(var),NrmQuarkToString(advancedfile->advancedfile.grpnode->path)));
        _NclCleanUpStack(nsubs);
        *estatus = NhlFATAL;
        return sel_ptr;
    }

    if(0 == nsubs)
    {
        sel_ptr = NULL;
    }
    else if(nsubs == varnode->dim_rec->n_dims)
    {
        sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
        sel_ptr->n_entries = nsubs;
    }
    else
    {
        NhlPError(NhlFATAL,NhlEUNKNOWN,
                 "Number of subscripts do not match number of dimensions of variable, (%d) subscripts used, (%d) subscripts expected",
                  nsubs,varnode->dim_rec->n_dims);
        _NclCleanUpStack(nsubs);
        *estatus = NhlFATAL;
     }

     return sel_ptr;
}
#endif

void CallLIST_ASSIGN_VERIFY_SUB (void) {
	NclStackEntry *data_ptr;

	data_ptr = _NclPeek(0);
	if(data_ptr->kind == NclStk_SUBREC) {
		switch(data_ptr->u.sub_rec.sub_type) {
		case COORD_VECT:
		case COORD_RANGE:
		case COORD_SINGLE:
		case INT_VECT:
		case INT_RANGE:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Only single elements from lists can be used on the left-hand-side of and assignment statement");
			estatus = NhlFATAL;
			break;
		case INT_SINGLE:
			break;
		}
	} else {
		estatus = NhlFATAL;
	}

}
void CallLIST_CLEAR_TMP_OP(void) {
	NclSymbol *temporary;
	NclStackEntry *temporary_ptr;

	ptr++;lptr++;fptr++;
	temporary = (NclSymbol*)(*ptr);
	temporary_ptr = _NclRetrieveRec(temporary,DONT_CARE);
/*
	if((temporary_ptr->kind == NclStk_VAR)&&(temporary_ptr->u.data_var->obj.status == TEMPORARY)) {
		_NclDestroyObj((NclObj)temporary_ptr->u.data_var);
	}
*/
	temporary_ptr->kind = NclStk_NOVAL;
	temporary_ptr->u.data_var = NULL;
	(void)_NclChangeSymbolType(temporary,UNDEF);
	
	return;
}
void CallTERM_LIST_OP(void) {
	NclSymbol *temporary;
	NclStackEntry *temporary_list_ptr;
	NclStackEntry output;
	ng_size_t n_elements =0;

	ptr++;lptr++;fptr++;
	temporary = (NclSymbol*)(*ptr);
	temporary_list_ptr = _NclRetrieveRec(temporary,DONT_CARE);

	if(temporary_list_ptr != NULL) {
		n_elements = temporary_list_ptr->u.data_list->list.nelem;

		if(1 != n_elements)
		{
			if(temporary_list_ptr->u.data_list->list.list_type & NCL_JOIN)
			{
				estatus = _NclBuildArray(n_elements,&output);
			}
			else if(temporary_list_ptr->u.data_list->list.list_type & NCL_CONCAT)
			{
				estatus = _NclBuildConcatArray(n_elements,&output);
			}
			else
			{
				/*This code will make the output a new list.*/
				estatus = _NclBuildListVar(n_elements,&output);
			}

       			if(estatus != NhlFATAL)
       				estatus = _NclPush(output);
		}

		_NclDestroyObj((NclObj)temporary_list_ptr->u.data_list);
	}

	return;
}

void CallLIST_READ_OP(void) {
	NclSymbol *listsym;
	NclSymbol *temporary;
	NclStackEntry *list_ptr;
	NclStackEntry *temporary_list_ptr;
	NclStackEntry data;
	NclList list;
	NclList newlist;
	int subs;
	ng_size_t i;
	NclSelection *sel_ptr=NULL;
	NclSelection sel;
	NclMultiDValData vect_md,tmp_md;
	long *thevector;
	
	ptr++;lptr++;fptr++;
	listsym = (NclSymbol*)(*ptr);
	ptr++;lptr++;fptr++;
	temporary = (NclSymbol*)(*ptr);
	ptr++;lptr++;fptr++;
	subs = *(int*)ptr;

	int number_of_item = 0;

	list_ptr = _NclRetrieveRec(listsym,DONT_CARE);
	temporary_list_ptr = _NclRetrieveRec(temporary,DONT_CARE);
	if(list_ptr != NULL) {
		tmp_md = (NclMultiDValData)_NclGetObj(list_ptr->u.data_var->var.thevalue_id);
		if((tmp_md != NULL)&&(tmp_md->multidval.data_type == NCL_list)){
			list = (NclList)_NclGetObj(*(obj*)tmp_md->multidval.val);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"List subscripting used on non-list variable, can't continue");
			estatus = NhlFATAL;
                	return;
		}
	} else {
		estatus = NhlFATAL;
                return;
	}


	if(subs) {
		data = _NclPop();
		switch(data.u.sub_rec.sub_type) {
			case INT_VECT:
				vect_md = data.u.sub_rec.u.vec.vec;
				if(!(vect_md->multidval.type->type_class.type & Ncl_Typelong)) {
                        		tmp_md = _NclCoerceData(vect_md,Ncl_Typelong,NULL);
                        		if(tmp_md == NULL) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce vector to long type can't perform subscripting");
						estatus = NhlFATAL;
                                		return;
                        		}
 
                		}  else {
                        		tmp_md = vect_md;
               			}
		                thevector = (long*)NclMalloc((unsigned)vect_md->multidval.totalelements * sizeof(long));
				memcpy((char*)thevector,(char*)tmp_md->multidval.val,tmp_md->multidval.totalelements * sizeof(long));
				sel.sel_type = Ncl_VECSUBSCR;
				sel.u.vec.n_ind = vect_md->multidval.totalelements;
				sel.u.vec.min = thevector[0];
				sel.u.vec.max = thevector[0];
				sel.u.vec.ind = thevector;
                		for(i = 0; i < sel.u.vec.n_ind; i++) {
                        		if(thevector[i] > sel.u.vec.max) {
                                		sel.u.vec.max = thevector[i];
                        		}
                        		if(thevector[i] < sel.u.vec.min) {
                                		sel.u.vec.min = thevector[i];
                        		}
                		}
                		if((tmp_md != vect_md)&&(tmp_md->obj.status != PERMANENT)) {
               			         _NclDestroyObj((NclObj)tmp_md);
		                }
				break;
			case INT_SINGLE:
			case INT_RANGE:
				sel.u.sub.is_single = data.u.sub_rec.u.range.is_single;
				if(( data.u.sub_rec.u.range.start == NULL)&&( data.u.sub_rec.u.range.finish == NULL)) {
					sel.sel_type = Ncl_SUB_ALL;
					sel.u.sub.start = 0;
					sel.u.sub.finish = 0;
					sel.u.sub.stride = 1;
				} else if(data.u.sub_rec.u.range.start == NULL) {
					sel.sel_type = Ncl_SUB_DEF_VAL;
					sel.u.sub.start = 0;
		                        if(!_NclScalarCoerce(
               			                 data.u.sub_rec.u.range.finish->multidval.val,
                               			 data.u.sub_rec.u.range.finish->multidval.data_type,
                                		&(sel.u.sub.finish),NCL_long)) {
/*
* This shouldn't happen but it can't hurt to have an extra check here
*/
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                        		sel.u.sub.stride = 1;

				} else if(data.u.sub_rec.u.range.finish == NULL) {
                        		sel.sel_type = Ncl_SUB_VAL_DEF;
 
                        		if(!_NclScalarCoerce(
                                		data.u.sub_rec.u.range.start->multidval.val,
                                		data.u.sub_rec.u.range.start->multidval.data_type,
                                		&(sel.u.sub.start),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
		 
                        		}
		 
                        		sel.u.sub.finish = 0;
                        		sel.u.sub.stride = 1;

				} else {
		                        sel.sel_type = Ncl_SUBSCR;
 
					if(!_NclScalarCoerce(
						data.u.sub_rec.u.range.start->multidval.val,
						data.u.sub_rec.u.range.start->multidval.data_type,
						&(sel.u.sub.start),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                        		if(!_NclScalarCoerce(
                                		data.u.sub_rec.u.range.finish->multidval.val,
                                		data.u.sub_rec.u.range.finish->multidval.data_type,
                                		&(sel.u.sub.finish),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                        		sel.u.sub.stride = 1;

				}
				if(data.u.sub_rec.u.range.stride != NULL) {
                        		if(!_NclScalarCoerce(
                                		data.u.sub_rec.u.range.stride->multidval.val,
                                		data.u.sub_rec.u.range.stride->multidval.data_type,
                                		&(sel.u.sub.stride),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                		}

				break;
			default:
				break;
		}

		number_of_item = 1 + sel.u.sub.finish - sel.u.sub.start;

		sel_ptr = &sel;
		_NclFreeSubRec(&data.u.sub_rec);
	} else {
		sel_ptr = NULL;
	}

	if(number_of_item)
	{
		newlist = _NclListSelect(list,sel_ptr);
		if(NULL != newlist)
		{
			temporary_list_ptr->kind = NclStk_LIST;
			temporary_list_ptr->u.data_list = newlist;
		}
		else
		{
			temporary_list_ptr->kind = NclStk_NOVAL;
			temporary_list_ptr->u.data_list = NULL;
			estatus = NhlFATAL;
		}
	} else {
		temporary_list_ptr->kind = NclStk_NOVAL;
		temporary_list_ptr->u.data_list = NULL;
		estatus = NhlFATAL;
	}
	
	return;
}
int HasTimeUnits(NrmQuark units)
{
	char *str_units = NrmQuarkToString(units);
	int ok_so_far = 0;

	/* make sure the string has a minimum length of 6 (I don't think it is possible to have time units with less than
	   6 total characters -- this prevents a short string from causing overrun with these tests 
	if (strlen(str_units) < 6)
	return 0; */

	if (! str_units) 
		return 0;
	if(!strncasecmp(str_units,"sec",3) || !strcasecmp(str_units,"s")){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"min",3) || !strcasecmp(str_units,"mn")){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"hour",4) || !strcasecmp(str_units,"hr")){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"day",3) || !strcasecmp(str_units,"dy")){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"week",4) || !strcasecmp(str_units,"wk")){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"month",5) || !strcasecmp(str_units,"mo")){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"season",6)){
		ok_so_far = 1;
        }
        else if(!strncasecmp(str_units,"year",4) || !strcasecmp(str_units,"yr")){
		ok_so_far = 1;
        }
	if (! ok_so_far) {
		return 0;
	}
	/* Since some of the unit abbreviations such as "s" cannot really be considered a very good test of whether the
	   unit string represents time, also check for the presence of an equivalent to "since" */
	
	if (! strcasestr(str_units,"since") || 
	    !  strcasestr(str_units,"after") ||
	    ! strcasestr(str_units,"ref")  ||
	    ! strcasestr(str_units,"from")) {
		return 1;
	}
	return 0;
}

extern int cuErrorOccurred;

NhlErrorTypes FixAggCoord(NclOneDValCoordData agg_coord_md, long *agg_dim_count, NrmQuark *units, NrmQuark *calendar, int nfiles)
{
	int i;
	int first = 1;
	cdCalenType ctype, base_ctype;
	cdUnitTime base_cdunit, cdunit;
	cdCompTime base_cdcomptime, cdcomptime;
	ng_size_t agg_ix = 0;
	NrmQuark qbase_unit;
	/*NclBasicDataTypes coord_type;*/
	void *val;
	double dx,dx_out,dx_diff;
	ng_size_t tsize;
	void *diff;

	cuErrorOccurred = 0;
	if (nfiles <= 0) 
		return NhlNOERROR;

	val = agg_coord_md->multidval.val;
	tsize = agg_coord_md->multidval.type->type_class.size;
	/*coord_type = agg_coord_md->multidval.data_type;*/
	diff = NclMalloc(tsize);

	for (i = 0; i < nfiles; i++) {
		if (agg_dim_count[i] == 0) /* indicates a bad file */
			continue;
		if (calendar[i] == NrmNULLQUARK)
			ctype = cdStandard;
		else
			ctype = calendar_type(NrmQuarkToString(_NclGetLower(calendar[i])));
		if (first) {
			first = 0;
			base_ctype = ctype;
			if ((units[i] == NrmNULLQUARK) || cdParseRelunits(ctype,NrmQuarkToString(units[i]),&base_cdunit,&base_cdcomptime)) {
				NhlPError(NhlWARNING, NhlEUNKNOWN,"non-monotonic aggregation variable -- not enough information to fix");
				return NhlWARNING;
			}
			qbase_unit = units[i];
			agg_ix = agg_dim_count[i];
			continue;
		}
		if (units[i] == qbase_unit) /* nothing to do */
			continue;
		if ((units[i] == NrmNULLQUARK) || cdParseRelunits(ctype,NrmQuarkToString(units[i]),&cdunit,&cdcomptime)) {
			NhlPError(NhlWARNING, NhlEUNKNOWN,"non-monotonic aggregation variable -- not enough information to fix");
			return NhlWARNING;
		}
		
		_Nclcoerce((NclTypeClass)nclTypedoubleClass,
			   (void*)(&dx),
			   (char *)val + agg_ix * tsize,
			   1,
			   NULL,
			   NULL,
			   agg_coord_md->multidval.type);

		cdRel2Comp(ctype,NrmQuarkToString(units[i]),dx,&cdcomptime);
		cdComp2Rel(base_ctype,cdcomptime,NrmQuarkToString(qbase_unit),&dx_out);
		dx_diff = dx_out - dx;
		_NclScalarForcedCoerce((void*)&dx_diff,NCL_double,diff, agg_coord_md->multidval.data_type);
		_Nclplus(agg_coord_md->multidval.type,(char *)val + agg_ix * tsize,(char *)val + agg_ix * tsize,diff,NULL,NULL,agg_dim_count[i],1);

		agg_ix += agg_dim_count[i];
	}

	NclFree(diff);
	/* check that array is now monotonic */
	if (! _Nclis_mono(agg_coord_md->multidval.type, agg_coord_md->multidval.val, NULL, agg_coord_md->multidval.totalelements)) {
		NhlPError(NhlWARNING, NhlEUNKNOWN,"error attempting to fix non-monotonic aggregation variable");
		return NhlWARNING;
	}

	return NhlNOERROR;
}

NhlErrorTypes FixAggCoordValue(NclOneDValCoordData agg_coord_md, ng_size_t ref_ix, ng_size_t ix, NrmQuark *units, NrmQuark *calendar,ng_size_t offset, ng_size_t n_sub_elements)
{
	cdCalenType ctype, base_ctype;
	cdUnitTime base_cdunit;
	cdCompTime base_cdcomptime, cdcomptime;
	NrmQuark qbase_unit;
	/*NclBasicDataTypes coord_type;*/
	void *val;
	double dx,dx_out,dx_diff;
	ng_size_t tsize;
	void *diff;
	
	ref_ix = 0;

	cuErrorOccurred = 0;
	val = agg_coord_md->multidval.val;
	
	tsize = agg_coord_md->multidval.type->type_class.size;
	diff = NclMalloc(tsize);

	if (calendar[ref_ix] == NrmNULLQUARK)
		ctype = cdStandard;
	else
		ctype = calendar_type(NrmQuarkToString(_NclGetLower(calendar[ref_ix])));

	qbase_unit = units[ref_ix];
	base_ctype = ctype;
	if ((units[ix] == NrmNULLQUARK) || cdParseRelunits(ctype,NrmQuarkToString(units[ix]),&base_cdunit,&base_cdcomptime)) {
		return NhlWARNING;
	}
	
	_Nclcoerce((NclTypeClass)nclTypedoubleClass,
		   (void*)(&dx),
		   (char *)val + offset,
		   1,
		   NULL,
		   NULL,
		   agg_coord_md->multidval.type);

	cdRel2Comp(ctype,NrmQuarkToString(units[ix]),dx,&cdcomptime);
	cdComp2Rel(base_ctype,cdcomptime,NrmQuarkToString(qbase_unit),&dx_out);
	dx_diff = dx_out - dx;
	_NclScalarForcedCoerce((void*)&dx_diff,NCL_double,diff, agg_coord_md->multidval.data_type);
	_Nclplus(agg_coord_md->multidval.type,(char *)val + offset,(char *)val + offset,diff,NULL,NULL,n_sub_elements,1);
	NclFree(diff);

	return NhlNOERROR;
}
	
void CallLIST_READ_FILEVAR_OP(void) {
	NclSymbol *listsym;
	NclStackEntry *list_ptr;
	NclStackEntry data;
	NclStackEntry fvar;
	NclQuark var;
 	NclList list;
	NclList newlist = NULL;
	int filevar_subs,subs;
	NclSelection *sel_ptr=NULL, *fsel;
	NclSelection sel;
	NclSelectionRecord *filevar_sel_ptr = NULL;
	NclMultiDValData vect_md,tmp_md,agg_coord_var_md,sub_agg_md,thevalue;
	long *thevector;
	int the_obj_id;
	NclObj the_obj;
	long *agg_dim_count = NULL;
 	NrmQuark *units = NULL;
 	NrmQuark *calendar = NULL;
	int list_index;
	long total_agg_dim_size, agg_start_index;
	long agg_end_index;
	long agg_sel_count;
	long agg_stride_index;
	NrmQuark agg_dim_name = NrmNULLQUARK;
	NclFile *files = NULL;
	NclVar var1 = NULL, agg_coord_var;
	NclOneDValCoordData agg_coord_md = NULL;
	NclMultiDValData var_md;
	NclDimRec dim_info;
	int first;
	int dir;
	ng_size_t i,ix_start, ix_end;
	ng_size_t var_offset;
	ng_size_t var_dim_sizes[32];
	ng_size_t agg_chunk_size;
        int var_ndims; /* non_aggregated natural var dim count */
	int good_file_count, first_good_ix = 0;
	long long max_var_size;
	ng_size_t total_coord_offset = 0;


	ptr++;lptr++;fptr++;
	listsym = (NclSymbol*)(*ptr);
	ptr++;lptr++;fptr++;
	subs = *(int*)ptr;
	ptr++;lptr++;fptr++;
	filevar_subs = *(int*)ptr;

	list_ptr = _NclRetrieveRec(listsym,DONT_CARE);
	if(list_ptr != NULL) {
		tmp_md = (NclMultiDValData)_NclGetObj(list_ptr->u.data_var->var.thevalue_id);
		if((tmp_md != NULL)&&(tmp_md->multidval.data_type == NCL_list)){
			list = (NclList)_NclGetObj(*(obj*)tmp_md->multidval.val);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"List subscripting used on non-list variable, can't continue");
			estatus = NhlFATAL;
                	return;
		}
	} else {
		estatus = NhlFATAL;
                return;
	}


	/* this is the single subscript for the file list variable */
	sel.dim_num = 0;
	if(subs) {
		data = _NclPop();
		switch(data.u.sub_rec.sub_type) {
			case INT_VECT:
				vect_md = data.u.sub_rec.u.vec.vec;
				if(!(vect_md->multidval.type->type_class.type & Ncl_Typelong)) {
                        		tmp_md = _NclCoerceData(vect_md,Ncl_Typelong,NULL);
                        		if(tmp_md == NULL) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce vector to long type can't perform subscripting");
						estatus = NhlFATAL;
                                		return;
                        		}
 
                		}  else {
                        		tmp_md = vect_md;
               			}
		                thevector = (long*)NclMalloc((unsigned)vect_md->multidval.totalelements * sizeof(long));
				if (thevector == NULL) {
					NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
					estatus = NhlFATAL;
					return;
				}
				memcpy((char*)thevector,(char*)tmp_md->multidval.val,tmp_md->multidval.totalelements * sizeof(long));
				sel.sel_type = Ncl_VECSUBSCR;
				sel.u.vec.n_ind = vect_md->multidval.totalelements;
				sel.u.vec.min = thevector[0];
				sel.u.vec.max = thevector[0];
				sel.u.vec.ind = thevector;
                		for(i = 0; i < sel.u.vec.n_ind; i++) {
                        		if(thevector[i] > sel.u.vec.max) {
                                		sel.u.vec.max = thevector[i];
                        		}
                        		if(thevector[i] < sel.u.vec.min) {
                                		sel.u.vec.min = thevector[i];
                        		}
                		}
                		if((tmp_md != vect_md)&&(tmp_md->obj.status != PERMANENT)) {
               			         _NclDestroyObj((NclObj)tmp_md);
		                }
				break;
			case INT_SINGLE:
			case INT_RANGE:
				sel.u.sub.is_single = data.u.sub_rec.u.range.is_single;
				if(( data.u.sub_rec.u.range.start == NULL)&&( data.u.sub_rec.u.range.finish == NULL)) {
					sel.sel_type = Ncl_SUB_ALL;
					sel.u.sub.start = 0;
					sel.u.sub.finish = 0;
					sel.u.sub.stride = 1;
				} else if(data.u.sub_rec.u.range.start == NULL) {
					sel.sel_type = Ncl_SUB_DEF_VAL;
					sel.u.sub.start = 0;
		                        if(!_NclScalarCoerce(
               			                 data.u.sub_rec.u.range.finish->multidval.val,
                               			 data.u.sub_rec.u.range.finish->multidval.data_type,
                                		&(sel.u.sub.finish),NCL_long)) {
/*
* This shouldn't happen but it can't hurt to have an extra check here
*/
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                        		sel.u.sub.stride = 1;

				} else if(data.u.sub_rec.u.range.finish == NULL) {
                        		sel.sel_type = Ncl_SUB_VAL_DEF;
 
                        		if(!_NclScalarCoerce(
                                		data.u.sub_rec.u.range.start->multidval.val,
                                		data.u.sub_rec.u.range.start->multidval.data_type,
                                		&(sel.u.sub.start),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
		 
                        		}
		 
                        		sel.u.sub.finish = 0;
                        		sel.u.sub.stride = 1;

				} else {
		                        sel.sel_type = Ncl_SUBSCR;
 
					if(!_NclScalarCoerce(
						data.u.sub_rec.u.range.start->multidval.val,
						data.u.sub_rec.u.range.start->multidval.data_type,
						&(sel.u.sub.start),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                        		if(!_NclScalarCoerce(
                                		data.u.sub_rec.u.range.finish->multidval.val,
                                		data.u.sub_rec.u.range.finish->multidval.data_type,
                                		&(sel.u.sub.finish),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                        		sel.u.sub.stride = 1;

				}
				if(data.u.sub_rec.u.range.stride != NULL) {
                        		if(!_NclScalarCoerce(
                                		data.u.sub_rec.u.range.stride->multidval.val,
                                		data.u.sub_rec.u.range.stride->multidval.data_type,
                                		&(sel.u.sub.stride),NCL_long)) {
                                		NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce subscript value to long data type");
						estatus = NhlFATAL;
                               	 		return;
                        		}
 
                		}

				break;
			default:
				break;
		}
		sel_ptr = &sel;
		_NclFreeSubRec(&data.u.sub_rec);
	} else {
		sel_ptr = NULL;
	}

	/* get the selected files from the file list */
	newlist =_NclListSelect(list,sel_ptr);
	if (! newlist) {
		estatus = NhlFATAL;
                return;
	}

	if (sel.sel_type == Ncl_VECSUBSCR) {
		NclFree(sel.u.vec.ind);
	}

	fvar = _NclPop();
	var = NrmNULLQUARK;
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
	if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
		estatus = NhlFATAL;
	} else {
		var = *(NclQuark*)thevalue->multidval.val;
		if(fvar.u.data_obj->obj.status != PERMANENT) {
			_NclDestroyObj((NclObj)fvar.u.data_obj);
		}
	}

	/* Now create an array of files, with a NULL value for any file that does not have a conforming variable for the aggregation:
         * the first file determines the dimensionality of the non-aggregated dimension that must be conformed to. For now the 
	 * only the dimension size is checked, but maybe dimension names should be as well.
	 * JOIN and CAT types are handled separately.
         */
	agg_coord_var = NULL; /* use to test for presence of coordinate variable */
	agg_coord_var_md = NULL;
	agg_dim_count = NclCalloc(newlist->list.nelem, sizeof(long));
	units = NclCalloc(newlist->list.nelem, sizeof(NrmQuark));
	calendar = NclCalloc(newlist->list.nelem, sizeof(NrmQuark));
	files = NclCalloc(newlist->list.nelem, sizeof(NclFile));
	total_agg_dim_size = newlist->list.nelem;
	if (! (agg_dim_count && files && units && calendar)) {
		NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
		estatus = NhlFATAL;
	}

	if (estatus == NhlFATAL) {
		goto fatal_err;
	}

	first = 1;
	var_ndims = 0;
	good_file_count = 0;
	for (i = 0; i < total_agg_dim_size; i++) {
		units[i] = NrmNULLQUARK;
		calendar[i] = NrmNULLQUARK;
	}
	if (newlist->list.list_type & NCL_JOIN) {
		total_agg_dim_size = newlist->list.nelem;
		agg_dim_name = NrmStringToQuark("ncl_join");
		list_index = newlist->list.nelem - 1;
		while ((the_obj_id = _NclListGetNext((NclObj)newlist)) != -1) {
			NclMultiDValData file_md = NULL;
			NclFile thefile = NULL;
			int index;
	
			the_obj = _NclGetObj(the_obj_id);
			if (the_obj && the_obj->obj.obj_type == Ncl_FileVar) {
				file_md= (NclMultiDValData)_NclVarValueRead((NclVar)the_obj,NULL,NULL);
				if (! file_md) {
					NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
					estatus = NhlFATAL;
					goto fatal_err;
				}
				thefile = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
				if (thefile && var != NrmNULLQUARK && ((index = _NclFileIsVar(thefile, var)) > -1)) {
					int bad = 0;
#ifdef USE_NETCDF4_FEATURES
					if(thefile->file.advanced_file_structure)
					{
						NclAdvancedFile advancedfile = (NclAdvancedFile)thefile;
						NclFileVarNode *varnode = NULL;
						varnode = _getVarNodeFromNclFileGrpNode(advancedfile->advancedfile.grpnode, var);
						if(NULL == varnode)
						{
							NHLPERROR((NhlFATAL,NhlEUNKNOWN,"variable (%s) is not in file (%s)",
								NrmQuarkToString(var),
								NrmQuarkToString(advancedfile->advancedfile.grpnode->path)));
						}

						if(first && (NULL != varnode->dim_rec))
						{
							var_ndims = varnode->dim_rec->n_dims;
							for (i = 0; i < var_ndims; ++i)
							{
								var_dim_sizes[i] = varnode->dim_rec->dim_node[i].size;
							}
							first = 0;
						}
						else
						{
							if(NULL != varnode->dim_rec)
							{
								if(varnode->dim_rec->n_dims != var_ndims)
								{
									NHLPERROR((NhlWARNING,NhlEUNKNOWN,
										"File %s dimension count for variable does not conform to others in list; skipping file",
								  		NrmQuarkToString(advancedfile->advancedfile.fpath)));
									bad = 1;
								}
								else
								{
									for (i = 1; i < var_ndims; ++i) /* dim 0 does not need to match */
									{
										if(varnode->dim_rec->dim_node[i].size != var_dim_sizes[i])
										{
											NHLPERROR((NhlWARNING,NhlEUNKNOWN,
												"File %s dimension sizes do not conform to others in list; skipping file",
										  		NrmQuarkToString(advancedfile->advancedfile.fpath)));
											bad = 1;
											break;
										}
									}
								}
							}
							else
							{
								bad = 1;
							}
						}

						if (bad)
						{
							files[list_index] = NULL;
							agg_dim_count[list_index] = 0;
							total_agg_dim_size--;
							list_index--;
						}
						else
						{
							agg_dim_count[list_index] = 1;
							files[list_index] = thefile;
							good_file_count++;
							list_index--;
						}
					}
					else
#endif
					{
						struct _NclFVarRec *var_info = thefile->file.var_info[index];
						if (first) { /* save the dimension sizes */
							var_ndims = var_info->num_dimensions;
							for (i = 0; i < var_info->num_dimensions; i++) {
								var_dim_sizes[i] = thefile->file.file_dim_info[var_info->file_dim_num[i]]->dim_size;
							}
							first = 0;
						}
						else {
							if (var_info->num_dimensions != var_ndims) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"File %s dimension count for variable  does not conform to others in list; skipping file",
								  	NrmQuarkToString(thefile->file.fpath));
								bad = 1;
							}
							else {
								for (i = 0; i < var_info->num_dimensions; i++) {
									if (thefile->file.file_dim_info[var_info->file_dim_num[i]]->dim_size != var_dim_sizes[i]) {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"File %s dimension sizes do not conform to others in list; skipping file",
										  	NrmQuarkToString(thefile->file.fpath));
										bad = 1;
										break;
									}
								}
							}
						}
						if (bad) {
							files[list_index] = NULL;
							agg_dim_count[list_index] = 0;
							total_agg_dim_size--;
							list_index--;
						}
						else {
							files[list_index] = thefile;
							agg_dim_count[list_index] = 1;
							good_file_count++;
							list_index--;
						}
					}
				}
				else {
					files[list_index] = NULL;
					agg_dim_count[list_index] = 0;
					total_agg_dim_size--;
					list_index--;
				}
			}
		}
	}
	else {
		total_agg_dim_size = 0;
		list_index = newlist->list.nelem - 1;
		while ((the_obj_id = _NclListGetNext((NclObj)newlist)) != -1) {
			NclMultiDValData file_md = NULL;
			NclFile thefile = NULL;
			int index;
			int agg_dim;

			the_obj = _NclGetObj(the_obj_id);
			if (the_obj && the_obj->obj.obj_type == Ncl_FileVar) {
				file_md= (NclMultiDValData)_NclVarValueRead((NclVar)the_obj,NULL,NULL);
				if (! file_md) {
					NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
					estatus = NhlFATAL;
					goto fatal_err;
				}
				thefile = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
				if (thefile && var != NrmNULLQUARK && ((index = _NclFileIsVar(thefile, var)) > -1)) {
					int bad = 0;
#ifdef USE_NETCDF4_FEATURES
					if(thefile->file.advanced_file_structure)
					{
						NclAdvancedFile advancedfile = (NclAdvancedFile)thefile;
						NclFileVarNode *varnode = NULL;
						varnode = _getVarNodeFromNclFileGrpNode(advancedfile->advancedfile.grpnode, var);
						if(NULL == varnode)
						{
							NHLPERROR((NhlFATAL,NhlEUNKNOWN,"variable (%s) is not in file (%s)",
								   NrmQuarkToString(var),
								   NrmQuarkToString(advancedfile->advancedfile.grpnode->path)));
						}
						if(first && (NULL != varnode->dim_rec))
						{
							var_ndims = varnode->dim_rec->n_dims;
							for (i = 0; i < var_ndims; ++i)
							{
								var_dim_sizes[i] = varnode->dim_rec->dim_node[i].size;
							}
							first = 0;
						}
						if(NULL != varnode->dim_rec)
						{
							if(varnode->dim_rec->n_dims != var_ndims)
							{
								NHLPERROR((NhlWARNING,NhlEUNKNOWN,
									   "File %s dimension count for variable does not conform to others in list; skipping file",
									   NrmQuarkToString(advancedfile->advancedfile.fpath)));
								bad = 1;
							}
							else
							{
								for (i = 1; i < var_ndims; ++i) /* Wei 05/24/2013, Guess the first dimension does not need to match. */
								{
									if(varnode->dim_rec->dim_node[i].size != var_dim_sizes[i])
									{
										NHLPERROR((NhlWARNING,NhlEUNKNOWN,
											   "File %s dimension sizes do not conform to others in list; skipping file",
											   NrmQuarkToString(advancedfile->advancedfile.fpath)));
										bad = 1;
										break;
									}
								}
							}
						}
						else
						{
							bad = 1;
						}

						if (bad) {
							files[list_index] = NULL;
							agg_dim_count[list_index] = 0;
							units[list_index] = NrmNULLQUARK;
							calendar[list_index] = NrmNULLQUARK;
							list_index--;
						}
						else {
							NclMultiDValData tmd;
							agg_dim_name = varnode->dim_rec->dim_node[0].name;
							total_agg_dim_size += varnode->dim_rec->dim_node[0].size;
							agg_dim_count[list_index] = varnode->dim_rec->dim_node[0].size;
							if (_NclFileVarIsAtt(thefile,agg_dim_name,NrmStringToQuark("units")) != -1) {
								tmd = _NclFileReadVarAtt(thefile,agg_dim_name,NrmStringToQuark("units"),NULL);
								units[list_index] = *(NrmQuark *) tmd->multidval.val;
							}
							if (_NclFileVarIsAtt(thefile,agg_dim_name,NrmStringToQuark("calendar")) != -1) {
								tmd = _NclFileReadVarAtt(thefile,agg_dim_name,NrmStringToQuark("calendar"),NULL);
								calendar[list_index] = *(NrmQuark *) tmd->multidval.val;
							}
							files[list_index] = thefile;
							good_file_count++;
							list_index--;

						}
					}
					else 
#endif
					{
						struct _NclFVarRec *var_info = thefile->file.var_info[index];
						if (first) { /* save the dimension sizes */
							var_ndims = var_info->num_dimensions;
							for (i = 0; i < var_info->num_dimensions; i++) {
								var_dim_sizes[i] = thefile->file.file_dim_info[var_info->file_dim_num[i]]->dim_size;
							}
							first = 0;
						}
						else {
							if (var_info->num_dimensions != var_ndims) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"File %s dimension count for variable  does not conform to others in list; skipping file",
									  NrmQuarkToString(thefile->file.fpath));
								bad = 1;
							}
							else {
								for (i = 1; i < var_info->num_dimensions; i++) { /* dim 0 does not need to match */
									if (thefile->file.file_dim_info[var_info->file_dim_num[i]]->dim_size != var_dim_sizes[i]) {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"File %s dimension sizes do not conform to others in list; skipping file",
											  NrmQuarkToString(thefile->file.fpath));
										bad = 1;
										break;
									}
								}
							}
						}

						if (bad) {
							files[list_index] = NULL;
							agg_dim_count[list_index] = 0;
							units[list_index] = NrmNULLQUARK;
							calendar[list_index] = NrmNULLQUARK;
							list_index--;
						}
						else {
							NclMultiDValData tmd;
							agg_dim = thefile->file.var_info[index]->file_dim_num[0];
							agg_dim_name = thefile->file.file_dim_info[agg_dim]->dim_name_quark;
							total_agg_dim_size += thefile->file.file_dim_info[agg_dim]->dim_size;
							agg_dim_count[list_index] = thefile->file.file_dim_info[agg_dim]->dim_size;
							if (_NclFileVarIsAtt(thefile,agg_dim_name,NrmStringToQuark("units")) != -1) {
								tmd = _NclFileReadVarAtt(thefile,agg_dim_name,NrmStringToQuark("units"),NULL);
								units[list_index] = *(NrmQuark *) tmd->multidval.val;
							}
							if (_NclFileVarIsAtt(thefile,agg_dim_name,NrmStringToQuark("calendar")) != -1) {
								tmd = _NclFileReadVarAtt(thefile,agg_dim_name,NrmStringToQuark("calendar"),NULL);
								calendar[list_index] = *(NrmQuark *) tmd->multidval.val;
							}
							files[list_index] = thefile;
							good_file_count++;
							list_index--;

						}
					}					
				}
				else {
					files[list_index] = NULL;
					agg_dim_count[list_index] = 0;
					list_index--;
				}
			}
		}
		if (good_file_count == 0 || agg_dim_name == NrmNULLQUARK) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"No valid instance of variable %s found in file list", NrmQuarkToString(var));
			estatus = NhlFATAL;
			goto fatal_err;
		}
		if (_NclFileVarIsCoord(files[0],agg_dim_name) > -1) {
			long offset;
			NclCoordVar cvar;
			cvar = (NclCoordVar)_NclFileReadCoord(files[0],agg_dim_name,NULL);
			agg_coord_md = (NclOneDValCoordData) _NclGetObj(cvar->var.thevalue_id);
			if (!agg_coord_md) {
				NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
				estatus = NhlFATAL;
				goto fatal_err;
			}
			if (agg_coord_md->multidval.n_dims != 1) {
				_NclDestroyObj((NclObj)agg_coord_md);
				agg_coord_md = NULL;
			}
			else {
				/* sort of cheating -- expand the multidval */
				int type_size = agg_coord_md->multidval.type->type_class.size;
				agg_coord_md->multidval.val = NclRealloc(agg_coord_md->multidval.val,total_agg_dim_size * type_size);
				if (! agg_coord_md->multidval.val) {
					NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
					estatus = NhlFATAL;
					goto fatal_err;
				}
				agg_coord_md->multidval.dim_sizes[0] = total_agg_dim_size;
				agg_coord_md->multidval.totalsize = total_agg_dim_size * agg_coord_md->multidval.type->type_class.size;
				agg_coord_md->multidval.totalelements = total_agg_dim_size;
				offset = agg_dim_count[0] * type_size;
				for (i = 1; i < newlist->list.nelem; i++) {
					NclMultiDValData tmp_md;
					if (files[i] && _NclFileIsVar(files[i],agg_dim_name) > -1) {
						tmp_md  = _NclFileReadVarValue(files[i],agg_dim_name,NULL);
						if (! tmp_md) {
							NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
							estatus = NhlFATAL;
							goto fatal_err;
						}
						memcpy((char *)agg_coord_md->multidval.val + offset, 
						       tmp_md->multidval.val, tmp_md->multidval.totalsize);
						_NclDestroyObj((NclObj)tmp_md);
						offset += (agg_dim_count[i] * type_size);
					}				
				}
				/* create a coordinate variable -- I mean a variable with a coordinate variable */
				dim_info.dim_quark = agg_dim_name;
				dim_info.dim_num = 0;
				dim_info.dim_size = total_agg_dim_size;
				agg_coord_md->obj.status = PERMANENT;
				/*tvar = _NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,agg_coord_md,&dim_info,-1,NULL,COORD,NrmQuarkToString(agg_dim_name),TEMPORARY);*/
				agg_coord_md->obj.status = TEMPORARY;
				agg_coord_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,(NclMultiDValData)agg_coord_md,&dim_info,-1,&cvar->obj.id,VAR,NrmQuarkToString(agg_dim_name),TEMPORARY);
			}
		}	
	}	
	if (good_file_count == 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"No valid instance of variable %s found in file list", NrmQuarkToString(var));
		estatus = NhlFATAL;
		goto fatal_err;
	}
	else if (good_file_count < newlist->list.nelem) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"A valid instance of variable %s was not found in one or more elements of the file list", NrmQuarkToString(var));
		for (i = 0; i < newlist->list.nelem; i++) {
			if (! files[i])
				continue;
			first_good_ix = i;
			break;
		}
	}
	

	/* Reuse the list sel_ptr for the aggregated dimensions selection, which occurs next */
	
	sel_ptr = &sel;
	filevar_sel_ptr = NULL;
	if (! filevar_subs) {
		agg_sel_count = total_agg_dim_size;
		sel.sel_type = Ncl_SUB_ALL;
		sel.dim_num = 0;
		sel.u.sub.start = 0;
		sel.u.sub.finish = total_agg_dim_size - 1;
		sel.u.sub.stride = 1;
		sel.u.sub.is_single = total_agg_dim_size > 1 ? 0 : 1;
	}
	else {
		long start = 0, finish = total_agg_dim_size - 1, stride = 1;
		long end_ix;
		filevar_sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
		if (! filevar_sel_ptr) {
			NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
			estatus = NhlFATAL;
			goto fatal_err;
		}
		filevar_sel_ptr->n_entries = 0;

		if (newlist->list.list_type & NCL_JOIN) {
			if (newlist->list.nelem == 1) {
				if (filevar_subs < var_ndims || filevar_subs > var_ndims + 1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
                                          "Number of subscripts on rhs do not match number of dimensions of aggregated join type variable, (%d) subscripts used, (%d or %d) subscripts expected",
						  filevar_subs, var_ndims, var_ndims + 1);
					estatus = NhlFATAL;
					goto fatal_err;
				}
			}
			else if (filevar_subs != var_ndims + 1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "Number of subscripts on rhs do not match number of dimensions of aggregated join type variable, (%d) subscripts used, (%d) subscripts expected",
					  filevar_subs, var_ndims + 1);
				estatus = NhlFATAL;
				goto fatal_err;
			}
			if (filevar_subs > var_ndims) {
				filevar_sel_ptr->n_entries = filevar_subs - 1; 
			}
			else {
				filevar_sel_ptr->n_entries = filevar_subs; 
			}
			end_ix = 0;
		}
		else {
			if (filevar_subs != var_ndims) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "Number of subscripts on rhs do not match number of dimensions of aggregated cat type variable, (%d) Subscripts used, (%d) Subscripts expected",
					  filevar_subs, var_ndims);
				estatus = NhlFATAL;
				goto fatal_err;
			}
			filevar_sel_ptr->n_entries = filevar_subs;
			end_ix = 1;
		}
			
		
		for(i = filevar_sel_ptr->n_entries - 1; i >= end_ix; i--) {
			data =_NclPop();
			switch(data.u.sub_rec.sub_type) {
			case INT_VECT:
				_NclBuildFileVSelection(files[first_good_ix],var,&data.u.sub_rec.u.vec,&(filevar_sel_ptr->selection[i]),i,data.u.sub_rec.name);
				break;
			case INT_SINGLE:
			case INT_RANGE:
				_NclBuildFileRSelection(files[first_good_ix],var,&data.u.sub_rec.u.range,&(filevar_sel_ptr->selection[i]),i,data.u.sub_rec.name);
				break;
			case COORD_VECT:
				_NclBuildFileCoordVSelection(files[first_good_ix],var,&data.u.sub_rec.u.vec,&(filevar_sel_ptr->selection[i]),i,data.u.sub_rec.name);
				break;
			case COORD_SINGLE:
			case COORD_RANGE:
				 _NclBuildFileCoordRSelection(files[first_good_ix],var,&data.u.sub_rec.u.range,&(filevar_sel_ptr->selection[i]),i,data.u.sub_rec.name);
				break;
			}
			_NclFreeSubRec(&data.u.sub_rec);
		}
		if ((newlist->list.list_type & NCL_CONCAT) || (filevar_subs > var_ndims)) {
			data =_NclPop();
			switch(data.u.sub_rec.sub_type) {
			case INT_VECT:
				_NclBuildVSelection(agg_coord_var,&data.u.sub_rec.u.vec,sel_ptr,0,data.u.sub_rec.name);
				break;
			case INT_SINGLE:
			case INT_RANGE:
				_NclBuildRSelection(agg_coord_var,&data.u.sub_rec.u.range,sel_ptr,0,data.u.sub_rec.name);
				break;
			case COORD_VECT:
				_NclBuildCoordVSelection(agg_coord_var,&data.u.sub_rec.u.vec,sel_ptr,0,data.u.sub_rec.name);
				break;
			case COORD_SINGLE:
			case COORD_RANGE:
				_NclBuildCoordRSelection(agg_coord_var,&data.u.sub_rec.u.range,sel_ptr,0,data.u.sub_rec.name);
				break;
			}
			_NclFreeSubRec(&data.u.sub_rec);
		}
		/*  Now adjust the start and finish such that start is always less than finish and the stride accounts for the direction of the subselection
		 *  Note that NCL always ensures that the start element is included in any subselection, for any stride. This means making an adjustment to 
		 *  one of the endpoints in some cases.
                 *  Then get the count of the selected elements of the first dimension
		 */
		sel.dim_num = 0;
		agg_sel_count = -1;
		if (sel.sel_type == Ncl_VECSUBSCR) {
			agg_sel_count = sel.u.vec.n_ind;
		}
		else {
			if(sel.u.sub.stride == 0 ) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Invalid stride: stride must be positive non-zero integer");
				sel.u.sub.stride = 1;
			}
			switch (sel.sel_type) {
			case Ncl_SUB_ALL:
				finish = total_agg_dim_size - 1;
				start  = 0;
				stride = sel.u.sub.stride;
				if (stride != 1) 
					sel.sel_type = Ncl_SUBSCR;
				break;
			case Ncl_SUB_VAL_DEF:
				finish = total_agg_dim_size - 1;
				start = sel.u.sub.start;
				stride = sel.u.sub.stride;
				sel.sel_type = Ncl_SUBSCR;
				break;
			case Ncl_SUB_DEF_VAL:
				finish = sel.u.sub.finish;
				start = 0;
				stride = sel.u.sub.stride;
				sel.sel_type = Ncl_SUBSCR;
				break;
			case Ncl_SUBSCR:
				if (sel.u.sub.finish < sel.u.sub.start) {
					start  = sel.u.sub.finish + (sel.u.sub.start - sel.u.sub.finish) % labs(sel.u.sub.stride);
					finish = sel.u.sub.start;
					stride = -sel.u.sub.stride;
				}
				else {
					finish = sel.u.sub.finish;
					start = sel.u.sub.start;
					stride = sel.u.sub.stride;
				}
				break;
			default:
				break;
			}
			if (agg_sel_count < 0) {
				agg_sel_count = (long) ((finish - start) / labs(stride)) + 1L;
			}
			sel.u.sub.start = start;
			sel.u.sub.stride = stride;
			sel.u.sub.finish =  finish - (finish - start) % labs(stride);
		}
	}

	/* step through the files -- reading the correct elements of the first dimension; this handles both JOIN and CAT 
	 * in sub-branches. It would probably be wise to split them apart into separate routines. In fact this whole function
	 * should be broken up into more tractable pieces.
	 */

	first = 1;
	var_offset = 0;
	agg_stride_index = -1;
	max_var_size = 	sizeof(ng_size_t) == 8 ? LONG_MAX : INT_MAX;
	if (sel.sel_type == Ncl_SUBSCR) {
		if (sel.u.sub.stride > 0) {
			dir = 1;
			ix_start = 0;
			ix_end = newlist->list.nelem;
			agg_end_index = -1;
			agg_stride_index = sel.u.sub.start;
		}
		else {
			dir = -1;
			ix_end = -1;
			ix_start = newlist->list.nelem -1;
			agg_end_index = total_agg_dim_size;
			agg_stride_index = sel.u.sub.finish;
		}
	}
	else if (sel.sel_type == Ncl_VECSUBSCR) {
		long *ind = sel.u.vec.ind;
		if (ind[0] > ind[sel.u.vec.n_ind -1]) { /* reversed */
			for (i = 1; i < sel.u.vec.n_ind; i++) {
				if (ind[i] >= ind[i-1]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "Vector selection for the aggregated dimension of a list file variable must be monotonically increasing or decreasing"); 
					estatus = NhlFATAL;
					goto fatal_err;
				}
			}
			dir = -1;
			ix_end = -1;
			ix_start = newlist->list.nelem -1;
			agg_end_index = total_agg_dim_size;
		}
		else {
			for (i = 1; i < sel.u.vec.n_ind; i++) {
				if (ind[i] <= ind[i-1]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "Vector selection for the aggregated dimension of a list file variable must be monotonically increasing or decreasing"); 
					estatus = NhlFATAL;
					goto fatal_err;
				}
			}
			dir = 1;
			ix_start = 0;
			ix_end = newlist->list.nelem;
			agg_end_index = -1;
		}
	}
	else {
		dir = 1;
		ix_start = 0;
		ix_end = newlist->list.nelem;
		agg_end_index = -1;
		agg_stride_index = sel.u.sub.start;
	}
		
	for (i = ix_start; i != ix_end; i+= dir) {
		int j;
		long *vec = NULL;
		long vcount,vstart;
		int do_file = 0;
		ng_size_t n_sub_elements;
		ng_size_t sub_coord_elements;

		agg_start_index = agg_end_index + dir;
		agg_end_index += dir * agg_dim_count[i];
		if (! files[i]) 
			continue;

		if (! filevar_subs)
			do_file = 1;
		else if (newlist->list.list_type & NCL_JOIN) {
			switch (sel.sel_type) {
			case Ncl_SUB_ALL:
				do_file = 1;
				break;
			case Ncl_SUBSCR:
				if (dir == 1) {
					if (agg_stride_index > agg_end_index) {
						break;
					}
					if (agg_stride_index < agg_start_index) {
						break;
					}
					if (agg_stride_index < sel.u.sub.finish)
						agg_stride_index += sel.u.sub.stride;
				}
				else {
					if (agg_stride_index < agg_end_index) {
						break;
					}
					if (agg_stride_index > agg_start_index) {
						break;
					}
					if (agg_stride_index > sel.u.sub.start)
						agg_stride_index += sel.u.sub.stride;
				}					
				do_file = 1;
				break;
			case Ncl_VECSUBSCR:
				if (dir == 1) {
					if (sel.u.vec.min > agg_end_index) {
						break;
					}
					if (sel.u.vec.max < agg_start_index) {
						break;
					}
					for (j = 0; j < sel.u.vec.n_ind; j++) {
						if (sel.u.vec.ind[j] == agg_end_index) {
							do_file = 1;
							break;
						}
					}
					break;
				}
				else {
					if (sel.u.vec.max <  agg_end_index) {
						break;
					}
					if (sel.u.vec.min > agg_start_index) {
						break;
					}
					for (j = 0; j < sel.u.vec.n_ind; j++) {
						if (sel.u.vec.ind[j] == agg_end_index) {
							do_file = 1;
							break;
						}
					}
					break;

				}
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
				estatus = NhlFATAL;
				goto fatal_err;
			}

		}
		else {
			fsel = &(filevar_sel_ptr->selection[0]);
			fsel->dim_num = 0;
			switch (sel.sel_type) {
			case Ncl_SUB_ALL:
				do_file = 1;
				fsel->sel_type = Ncl_SUB_ALL;
				fsel->u.sub.stride = 1;
				fsel->u.sub.start = 0;
				fsel->u.sub.finish = agg_end_index - agg_start_index;
				fsel->u.sub.is_single = sel.u.sub.is_single;
				break;
			case Ncl_SUBSCR:
				if (dir == 1) {
					if (sel.u.sub.start > agg_end_index) {
						break;
					}
					if (sel.u.sub.finish < agg_start_index) {
						break;
					}
					while (agg_stride_index < agg_start_index)
						agg_stride_index += sel.u.sub.stride;
					fsel->u.sub.start = MAX(agg_stride_index,sel.u.sub.start) - agg_start_index;
					fsel->u.sub.finish = MIN(agg_end_index,sel.u.sub.finish) - agg_start_index;
				}
				else {
					if (sel.u.sub.finish < agg_end_index) {
						break;
					}
					if (sel.u.sub.start > agg_start_index) {
						break;
					}
					while (agg_stride_index > agg_start_index)
						agg_stride_index += sel.u.sub.stride;
					fsel->u.sub.start = MAX(agg_end_index,sel.u.sub.start) - agg_end_index;
					fsel->u.sub.finish = MIN(agg_stride_index,sel.u.sub.finish) - agg_end_index;
					fsel->u.sub.start += (fsel->u.sub.finish - fsel->u.sub.start) % abs(sel.u.sub.stride);
				}					
				fsel->u.sub.stride = sel.u.sub.stride; 
				fsel->u.sub.is_single = sel.u.sub.is_single;
				fsel->sel_type = Ncl_SUBSCR;
				do_file = 1;
				break;
			case Ncl_VECSUBSCR:
				if (dir == 1) {
					if (sel.u.vec.min > agg_end_index) {
						break;
					}
					if (sel.u.vec.max < agg_start_index) {
						break;
					}
					vcount = 0;
					vstart = -1;
					for (j = 0; j < sel.u.vec.n_ind; j++) {
						if (sel.u.vec.ind[j] > agg_end_index)
							break;
						if (sel.u.vec.ind[j] >= agg_start_index) {
							if (vstart < 0) 
								vstart = j;
							vcount++;
						}
					}
					if (vcount == 0) {
						break;
					}
					else if (vcount == 1 && sel.u.vec.n_ind > 1) {
						/* 
						 * In this case we have to switch to normal indexed subscripting because it is not possible to
						 *  preserve single element dimensions using vector subscripting.
						 */
						fsel->u.sub.start = sel.u.vec.ind[vstart] - agg_start_index;
						fsel->u.sub.finish = fsel->u.sub.start;
						fsel->u.sub.stride = 1;
						fsel->u.sub.is_single = agg_sel_count > 1 ? 0 : 1;
						fsel->sel_type = Ncl_SUBSCR;
						do_file = 1;
						break;
					}

					vec = NclMalloc(sizeof(long) * vcount);
					if (! vec) {
						NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
						estatus = NhlFATAL;
						goto fatal_err;
					}
					for (j = vstart; j < sel.u.vec.n_ind; j++) {
						if (sel.u.vec.ind[j] > agg_end_index)
							break;
						vec[j - vstart] = sel.u.vec.ind[j] - agg_start_index;
					}
					fsel->u.vec.n_ind = vcount;
					fsel->u.vec.ind = vec;
					fsel->u.vec.min = vec[0];
					fsel->u.vec.max = vec[vcount - 1];
					fsel->sel_type = Ncl_VECSUBSCR;
					do_file = 1;
					break;
				}
				else {
					if (sel.u.vec.max < agg_end_index) {
						break;
					}
					if (sel.u.vec.min > agg_start_index) {
						break;
					}
					vcount = 0;
					vstart = -1;
					for (j = 0; j < sel.u.vec.n_ind; j++) {
						if (sel.u.vec.ind[j] < agg_end_index)
							break;
						if (sel.u.vec.ind[j] <= agg_start_index) {
							if (vstart < 0) 
								vstart = j;
							vcount++;
						}
					}
					if (vcount == 0) {
						break;
					}
					else if (vcount == 1 && sel.u.vec.n_ind > 1) {
						/* 
						 * In this case we have to switch to normal indexed subscripting because it is not possible to
						 *  preserve single element dimensions using vector subscripting.
						 */
						fsel->u.sub.start = sel.u.vec.ind[vstart] - agg_end_index;
						fsel->u.sub.finish = fsel->u.sub.start;
						fsel->u.sub.stride = 1;
						fsel->u.sub.is_single = agg_sel_count > 1 ? 0 : 1;
						fsel->sel_type = Ncl_SUBSCR;
						do_file = 1;
						break;
					}

					vec = NclMalloc(sizeof(long) * vcount);
					if (! vec) {
						NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
						estatus = NhlFATAL;
						goto fatal_err;
					}
					for (j = vstart; j < sel.u.vec.n_ind; j++) {
						if (sel.u.vec.ind[j] < agg_end_index)
							break;
						vec[j - vstart] = sel.u.vec.ind[j] - agg_end_index;
					}
					fsel->u.vec.n_ind = vcount;
					fsel->u.vec.ind = vec;
					fsel->u.vec.min = vec[vcount -1];
					fsel->u.vec.max = vec[0];
					fsel->sel_type = Ncl_VECSUBSCR;
					do_file = 1;
					break;
				}
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
				estatus = NhlFATAL;
				goto fatal_err;
			}
		}
		if (! do_file)
			continue;
		if (first) {
			long long tsize;
			NclVar sub_agg_coord_var;
			NclSelectionRecord sel_rec;
			agg_chunk_size = 0;
			var1 = _NclFileReadVar(files[i],var,filevar_sel_ptr);
			if (! var1) {
				NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
				estatus = NhlFATAL;
				goto fatal_err;
			}
				
			first = False;
			/* expand md_array */
			tmp_md = (NclMultiDValData)_NclGetObj(var1->var.thevalue_id);
			n_sub_elements = tmp_md->multidval.totalelements;
			if (newlist->list.list_type & NCL_JOIN) {
				/* need to add a dimension to the variable */
				agg_chunk_size = tmp_md->multidval.totalsize;
				var_offset = tmp_md->multidval.totalsize;
				tsize = agg_chunk_size * (long long) agg_sel_count;
				if (tsize > max_var_size) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "Aggregating variable %s from file list variable %s as specified would exceed maximum NCL variable size",
						  NrmQuarkToString(var),listsym->name);
					estatus = NhlFATAL;
					goto fatal_err;
				}
				if (tsize > tmp_md->multidval.totalsize) {
					tmp_md->multidval.val = NclRealloc(tmp_md->multidval.val,tsize);
					if (! tmp_md->multidval.val) {
						NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
						estatus = NhlFATAL;
						goto fatal_err;
					}
					tmp_md->multidval.totalelements = tmp_md->multidval.totalelements * agg_sel_count;
					tmp_md->multidval.totalsize = tsize;
					if (tmp_md->multidval.kind == SCALAR) { 
						if (agg_sel_count > 1) {  /* selection produced a scalar */
							tmp_md->multidval.kind = MULTID;
							tmp_md->multidval.dim_sizes[0] = agg_sel_count;
							var1->var.dim_info[0].dim_size = agg_sel_count;
							var1->var.dim_info[0].dim_num = 0;
							var1->var.dim_info[0].dim_quark = agg_dim_name;
							var1->var.coord_vars[0] = -1;
						}
					}
					else {
						for (j = tmp_md->multidval.n_dims; j > 0; j--) {
							tmp_md->multidval.dim_sizes[j] = tmp_md->multidval.dim_sizes[j-1];
							var1->var.dim_info[j].dim_quark = var1->var.dim_info[j-1].dim_quark;
							var1->var.dim_info[j].dim_size = var1->var.dim_info[j-1].dim_size;
							var1->var.coord_vars[j] = var1->var.coord_vars[j-1];
						}
						tmp_md->multidval.n_dims++;
						var1->var.n_dims++;
						tmp_md->multidval.dim_sizes[0] = agg_sel_count;
						var1->var.dim_info[0].dim_size = agg_sel_count;
						var1->var.dim_info[0].dim_num = 0;
						var1->var.dim_info[0].dim_quark = agg_dim_name;
						var1->var.coord_vars[0] = -1;
					}
				}
				else if (sel.sel_type != Ncl_VECSUBSCR && ! sel.u.sub.is_single) {
					if (tmp_md->multidval.kind != SCALAR) { 
						for (j = tmp_md->multidval.n_dims; j > 0; j--) {
							tmp_md->multidval.dim_sizes[j] = tmp_md->multidval.dim_sizes[j-1];
							var1->var.dim_info[j].dim_quark = var1->var.dim_info[j-1].dim_quark;
							var1->var.dim_info[j].dim_size = var1->var.dim_info[j-1].dim_size;
							var1->var.coord_vars[j] = var1->var.coord_vars[j-1];
						}
						tmp_md->multidval.n_dims++;
						var1->var.n_dims++;
					}
					tmp_md->multidval.dim_sizes[0] = agg_sel_count;
					var1->var.dim_info[0].dim_size = agg_sel_count;
					var1->var.dim_info[0].dim_num = 0;
					var1->var.dim_info[0].dim_quark = agg_dim_name;
					var1->var.coord_vars[0] = -1;
					break;
				}
				
			}
			else if (sel.sel_type != Ncl_VECSUBSCR && sel.u.sub.is_single) {
				if (agg_coord_var) {
					_NclDestroyObj((NclObj)agg_coord_var);
				}
				break;
			}
			else {
				agg_chunk_size = tmp_md->multidval.totalsize / tmp_md->multidval.dim_sizes[0];
				var_offset = tmp_md->multidval.totalsize;
				tsize = agg_chunk_size * (long long) agg_sel_count;
				if (tsize > max_var_size) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "Aggregating variable %s from file list variable %s as specified would exceed maximum NCL variable size",
						  NrmQuarkToString(var),listsym->name);
					estatus = NhlFATAL;
					goto fatal_err;
				}
				if (tsize > tmp_md->multidval.totalsize) {
					tmp_md->multidval.val = NclRealloc(tmp_md->multidval.val,tsize);
					if (! tmp_md->multidval.val) {
						NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
						estatus = NhlFATAL;
						goto fatal_err;
					}
					tmp_md->multidval.totalelements = (tmp_md->multidval.totalelements / tmp_md->multidval.dim_sizes[0]) * agg_sel_count;
					tmp_md->multidval.kind = MULTID;
					tmp_md->multidval.dim_sizes[0] = agg_sel_count;
					tmp_md->multidval.totalsize = tsize;
					var1->var.dim_info[0].dim_size = agg_sel_count;
				}
			}
			memcpy(&(sel_rec.selection[0]),&sel,sizeof(NclSelection));
			sel_rec.n_entries = 1;
			/* also need to expand the aggregated coordinate variable if there is one */
			if (agg_coord_var) {
				sub_agg_coord_var = _NclReadCoordVar(agg_coord_var,NrmQuarkToString(agg_dim_name),NULL);
				if (!sub_agg_coord_var) {
					NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
					estatus = NhlFATAL;
					goto fatal_err;
				}
				sub_agg_md = _NclVarValueRead(sub_agg_coord_var,&sel_rec,NULL);
				_NclDestroyObj((NclObj)agg_coord_var);
				agg_coord_var = (NclVar)_NclGetObj(var1->var.coord_vars[0]);
				agg_coord_var_md = (NclMultiDValData)_NclGetObj(agg_coord_var->var.thevalue_id);
				sub_coord_elements = agg_coord_var_md->multidval.totalelements;
				if (sub_agg_md->multidval.totalelements > agg_coord_var_md->multidval.totalelements) {
					agg_coord_var_md->multidval.val = NclRealloc(agg_coord_var_md->multidval.val,sub_agg_md->multidval.totalsize);
					if (! agg_coord_var_md->multidval.val) {
						NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
						estatus = NhlFATAL;
						goto fatal_err;
					}
				}
				agg_coord_var_md->multidval.kind = MULTID;
				agg_coord_var_md->multidval.totalelements = sub_agg_md->multidval.totalelements;
				agg_coord_var_md->multidval.dim_sizes[0] = agg_sel_count;
				agg_coord_var_md->multidval.totalsize = sub_agg_md->multidval.totalsize;
				memcpy(agg_coord_var_md->multidval.val,sub_agg_md->multidval.val,sub_agg_md->multidval.totalsize);
				/* make the units consistent */
				if (HasTimeUnits(units[0]) && units[i] != units[0]) {
					NclMultiDValData tmp_md = NULL,tmp_md2;
					NrmQuark qval = units[0];
					tmp_md = _NclReadAtt(var1,"units",NULL);
					tmp_md2 = _NclCopyVal(tmp_md,NULL);
					memcpy(tmp_md2->multidval.val,(void*)&qval,sizeof(NrmQuark));
					_NclReplaceAtt(var1,"units",tmp_md2,NULL);
					FixAggCoordValue((NclOneDValCoordData)agg_coord_var_md,0,i,units,calendar,total_coord_offset,sub_coord_elements);
					_NclDestroyObj((NclObj)tmp_md2);
				}
				total_coord_offset += sub_coord_elements * agg_coord_var_md->multidval.type->type_class.size;
				agg_coord_var->var.dim_info[0].dim_size = agg_sel_count;
				_NclDestroyObj((NclObj)sub_agg_md);
			}
		}
		else {
			int bad = 0;
			var_md = _NclFileReadVarValue(files[i],var,filevar_sel_ptr);
			if (!var_md) {
				NhlPError(NhlFATAL,ENOMEM,"Memory allocation failure");
				estatus = NhlFATAL;
				goto fatal_err;
			}
			n_sub_elements = var_md->multidval.totalelements;
			if (newlist->list.list_type & NCL_JOIN) {
				if (var_md->multidval.totalsize != agg_chunk_size)
					bad = 1;
			}
			else {
				if ((var_md->multidval.totalsize /  var_md->multidval.dim_sizes[0]) != agg_chunk_size)
					bad = 1;
			}
			if (bad) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"incorrect size for list filevar subselection");
				estatus = NhlFATAL;
				goto fatal_err;
			}
			memcpy((char *)tmp_md->multidval.val + var_offset,var_md->multidval.val,var_md->multidval.totalsize);
			var_offset += var_md->multidval.totalsize;
			_NclDestroyObj((NclObj)var_md);
			if (agg_coord_var && agg_coord_var_md) {
				/* note fewer elements in the coordinate var than in the full variable */
				sub_coord_elements = n_sub_elements;
				for (j = 1; j < var_md->multidval.n_dims; j++) {
					sub_coord_elements /= var_md->multidval.dim_sizes[j];
				}
				if (HasTimeUnits(units[0]) && units[i] != units[0]) {
					FixAggCoordValue((NclOneDValCoordData)agg_coord_var_md,0,i,units,calendar,total_coord_offset,sub_coord_elements);
				}
				total_coord_offset += sub_coord_elements * agg_coord_var_md->multidval.type->type_class.size;
			}
		}
		if (vec)  /* this is the aggregated dimension vector value passed to each file so it must be freed at each iteration */
			NclFree(vec);
	}
	
	if (agg_coord_var && agg_coord_var_md) {
		if (! _Nclis_mono(agg_coord_var_md->multidval.type, agg_coord_var_md->multidval.val, NULL, agg_coord_var_md->multidval.totalelements)) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Aggregated dimension coordinate values are non-monotonic; check aggregated file ordering");
		}
		if (agg_coord_var->var.var_quark == var1->var.var_quark && var1->var.n_dims == 1) {  
			NclMultiDValData tmd = (NclMultiDValData)_NclGetObj(var1->var.thevalue_id);
			if (!tmd || tmd->multidval.data_type != agg_coord_var_md->multidval.data_type || tmd->multidval.totalsize != agg_coord_var_md->multidval.totalsize) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"Error attempting to conform coordinate variable data to initial file's time units");
			}
			else {
				memcpy(tmd->multidval.val,agg_coord_var_md->multidval.val,agg_coord_var_md->multidval.totalsize);
			}
		}
	}

	if (var1) {
		data.kind = NclStk_VAR;
		data.u.data_var = var1;
		if(data.u.data_var != NULL) {
			estatus = _NclPush(data);
		} else {
			estatus = NhlFATAL;
		}
	}
	else {
               estatus = NhlFATAL;
        }

 fatal_err:  /* could also be totally benign */

	if (filevar_sel_ptr) {
		for (i = 1; i < filevar_sel_ptr->n_entries; i++) {
			if (filevar_sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR) {
				NclFree(filevar_sel_ptr->selection[i].u.vec.ind);
			}
		}
		NclFree(filevar_sel_ptr);
	}
	if (sel.sel_type == Ncl_VECSUBSCR) {
		NclFree(sel.u.vec.ind);
	}
	if (files)
		NclFree(files);
	if (agg_dim_count)
		NclFree(agg_dim_count);
	if (units)
		NclFree(units);
	if (calendar)
		NclFree(calendar);
	if (newlist)
		_NclDestroyObj((NclObj)newlist);
	
	return;
}

void CallSET_NEXT_OP(void) 
{
	NclSymbol *listsym;
	NclSymbol *temporary;
	NclStackEntry *list_ptr;
	NclStackEntry *tmp_ptr;
	NclList list;
	NclObj the_obj;
 	unsigned long offset;
	int the_obj_id;
	
	

	ptr++;lptr++;fptr++;
	listsym = (NclSymbol*)(*ptr);
	ptr++;lptr++;fptr++;
	temporary= (NclSymbol*)(*ptr);
	ptr++;lptr++;fptr++;
	offset = *ptr;
	
	tmp_ptr = _NclRetrieveRec(temporary,DONT_CARE);
	list_ptr = _NclRetrieveRec(listsym,DONT_CARE);
	list = list_ptr->u.data_list;
	

	the_obj_id = _NclListGetNext((NclObj)list);
	if(the_obj_id != -1 ) {
		the_obj = _NclGetObj(the_obj_id);
		if(the_obj == NULL) {
        		ptr = machine + offset - 1;
			lptr = _NclGetCurrentLineRec() + offset - 1;
			fptr = _NclGetCurrentFileNameRec() + offset - 1;
/*
* Need to destroy/free stuff here.
*/
			return;
		} else {
			switch(the_obj->obj.obj_type) {
			case Ncl_Var:
			case Ncl_HLUVar:
			case Ncl_CoordVar:
			case Ncl_MultiDValData:
			case Ncl_MultiDVallistData:
				(void)_NclChangeSymbolType(temporary,VAR);
				tmp_ptr->kind = NclStk_VAR;
				tmp_ptr->u.data_var = (NclVar)the_obj;
				break;
			case Ncl_FileVar:
				(void)_NclChangeSymbolType(temporary,FVAR);
				tmp_ptr->kind = NclStk_VAR;
				tmp_ptr->u.data_var = (NclVar)the_obj;
				break;
			case Ncl_FileGroup:
				(void)_NclChangeSymbolType(temporary,FVAR);
				tmp_ptr->kind = NclStk_VAL;
				tmp_ptr->u.data_var = (NclVar)the_obj;
				break;
			default:
				NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
				estatus = NhlFATAL;
				return;
			}
		}
	} else {
        	ptr = machine + offset - 1;
		lptr = _NclGetCurrentLineRec() + offset - 1;
		fptr = _NclGetCurrentFileNameRec() + offset - 1;
	}
	return;
}
void CallINT_SUBSCRIPT_OP(void) {
				NclStackEntry data;
				NclStackEntry data1;
				int mask = (int)(Ncl_Typelong | Ncl_Typeint | Ncl_Typeshort | Ncl_Typebyte); 

/*
* This is the first place that type checks on the vectors and range values can
* be done since it isn't until here that it is determined that normal integer
* subscripting is going on
*/
				data1.kind = NclStk_SUBREC;
				data1.u.sub_rec.tolerence = -1;
				data = _NclPop();
				if(data.kind == NclStk_VECREC) {
					if(data.u.vec_rec.vec->multidval.type->type_class.type & mask ) {
						data1.u.sub_rec.sub_type = INT_VECT;
						data1.u.sub_rec.u.vec = data.u.vec_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Vector subscripts must be integer");
						estatus = NhlFATAL;
					}
				} else if(data.kind == NclStk_RANGEREC) {
					if(((data.u.range_rec.start == NULL)
						|| (data.u.range_rec.start->multidval.type->type_class.type & mask)) &&
					((data.u.range_rec.finish == NULL)
						||(data.u.range_rec.finish->multidval.type->type_class.type & mask)) &&
					((data.u.range_rec.stride == NULL)
						||(data.u.range_rec.stride->multidval.type->type_class.type & mask))) {
						data1.u.sub_rec.sub_type = data.u.range_rec.is_single ? INT_SINGLE : INT_RANGE;
						data1.u.sub_rec.u.range = data.u.range_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Subscripts must be integer when not using coordinate indexing");
						estatus = NhlFATAL;
					}
				}
				if(estatus != NhlFATAL) {
					if(*ptr == INT_SUBSCRIPT_OP) {
						data1.u.sub_rec.name = NULL;
					} else {
						data = _NclPop();
						switch(data.kind) {
						case NclStk_VAL: {
/*
* Taking for granted that syntax only allows string litterals here
*/
							data1.u.sub_rec.name = NrmQuarkToString(*((NclQuark*) data.u.data_obj->multidval.val));
							if(data.u.data_obj->obj.status != PERMANENT)
								_NclDestroyObj((NclObj)data.u.data_obj);
							
							break;
						}
						default:	
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Illegal type for coordinate name in coordinate subscript ignoring value");
							data1.u.sub_rec.name = NULL;
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
			}

void CallDEFAULT_RANGE_OP(void) {
				NclStackEntry data;
				data.kind = NclStk_NOVAL;
				data.u.offset = 0;
				estatus = _NclPush(data);
			}

void CallRANGE_INDEX_OP(void) {
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
				data.u.range_rec.is_single = 0;
				if(start.kind == NclStk_NOVAL) {
					data.u.range_rec.start = NULL;
				} else {
					switch(start.kind) {
					case NclStk_VAL:
						if(start.u.data_obj !=NULL) {
							data.u.range_rec.start = start.u.data_obj;
						} else {
							estatus = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						if(start.u.data_var->obj.status == TEMPORARY) {
							data.u.range_rec.start = _NclStripVarData(start.u.data_var);
							_NclDestroyObj((NclObj)start.u.data_var);
						} else {
							data.u.range_rec.start = _NclVarValueRead(start.u.data_var,NULL,NULL);
						}
						if(data.u.range_rec.start == NULL) {
							estatus = NhlFATAL;
						}
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
				}
				if(finish.kind == NclStk_NOVAL) {
					data.u.range_rec.finish = NULL;
				} else {
					switch(finish.kind) {
					case NclStk_VAL:
						if(finish.u.data_obj !=NULL) {
							data.u.range_rec.finish= finish.u.data_obj;
						} else {
							estatus = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						if(finish.u.data_var->obj.status == TEMPORARY) {
							data.u.range_rec.finish = _NclStripVarData(finish.u.data_var);
							_NclDestroyObj((NclObj)finish.u.data_var);
						} else {
							data.u.range_rec.finish = _NclVarValueRead(finish.u.data_var,NULL,NULL);
						}
						if(data.u.range_rec.finish == NULL) {
							estatus = NhlFATAL;
						}
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
				}
				if(stride.kind == NclStk_NOVAL) {
					data.u.range_rec.stride= NULL;
				} else {
					switch(stride.kind) {
					case NclStk_VAL:
						if(stride.u.data_obj !=NULL) {
							data.u.range_rec.stride= stride.u.data_obj;
						} else {
							estatus = NhlFATAL;
						}
						break;
					case NclStk_VAR:
						if(stride.u.data_var->obj.status == TEMPORARY) {
							data.u.range_rec.stride = _NclStripVarData(stride.u.data_var);
							_NclDestroyObj((NclObj)stride.u.data_var);
						} else {
							data.u.range_rec.stride = _NclVarValueRead(stride.u.data_var,NULL,NULL);
						}
						if(data.u.range_rec.stride == NULL){
							estatus = NhlFATAL;
						}
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
				}
				if((data.u.range_rec.start != NULL) &&
					(data.u.range_rec.start->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					estatus = NhlFATAL;
				}
				if((data.u.range_rec.finish != NULL) &&
					(data.u.range_rec.finish->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					estatus = NhlFATAL;
				}
				if((data.u.range_rec.stride != NULL) &&
					(data.u.range_rec.stride->multidval.kind != SCALAR)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal Subscript. Only scalar values are allowed in subscript ranges.\n");
					estatus = NhlFATAL;
				}
				if(_NclPush(data) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL)  {
					_NclCleanUpStack(1);
				}
			}

void CallSINGLE_INDEX_OP(void) {
				NclStackEntry data;
				NclStackEntry data1;
				NclMultiDValData val = NULL;

				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAR: 
					if(data.u.data_var->obj.status == TEMPORARY) {
						val = _NclStripVarData(data.u.data_var);
					} else {
						val = _NclVarValueRead(data.u.data_var,NULL,NULL);
					}
					break;
				case NclStk_VAL:
						val = data.u.data_obj;
					break;
				default:
					estatus = NhlFATAL;
				}
				if(val != NULL){
					if(val->multidval.kind == SCALAR) {
						data1.kind = NclStk_RANGEREC;
							data1.u.range_rec.start = val;
							data1.u.range_rec.finish = val;
							data1.u.range_rec.stride=NULL;
							data1.u.range_rec.is_single = 1;
						if((data.kind == NclStk_VAR)&&(data.u.data_var->obj.status != PERMANENT)) {
							_NclDestroyObj((NclObj)data.u.data_var);
						}
						estatus = _NclPush(data1);
					} else if(val->multidval.n_dims == 1) {
						data1.kind = NclStk_VECREC;
							data1.u.vec_rec.vec = val;
							if((data.kind == NclStk_VAR)&&(data.u.data_var->obj.status != PERMANENT)) {
								_NclDestroyObj((NclObj)data.u.data_var);
							}
						estatus = _NclPush(data1);
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. Subscripts must be scalar or one dimensional vectors\n");
						estatus = NhlFATAL;
					}
				} else {
					estatus = NhlFATAL;
				}
			}

void CallCOORD_SUBSCRIPT_OP(void) {
				NclStackEntry data;
				NclStackEntry data1;
				int mask = (int)(Ncl_Typelong | Ncl_Typeint | Ncl_Typeshort |Ncl_Typebyte); 

/*
* This is the first place that type checks on the vectors and range values can
* be done since it isn't until here that it is determined that normal integer
* subscripting is going on
*/
				data = _NclPop();
	
				data1.kind = NclStk_SUBREC;
				if(data.kind == NclStk_VECREC) {
					data1.u.sub_rec.sub_type = COORD_VECT;
					data1.u.sub_rec.u.vec = data.u.vec_rec;
				} else if(data.kind == NclStk_RANGEREC) {
					if(((data.u.range_rec.stride == NULL)
						||(data.u.range_rec.stride->multidval.type->type_class.type & mask))) {
						data1.u.sub_rec.sub_type = data.u.range_rec.is_single ? COORD_SINGLE: COORD_RANGE;
						data1.u.sub_rec.u.range = data.u.range_rec;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Illegal subscript. stride must always be integer regardless of whether coordinate or integer subscripting is being used\n");
						estatus = NhlFATAL;
					}
				}
				if(*ptr == COORD_SUBSCRIPT_OP) {
					data1.u.sub_rec.name = NULL;
				} else {
					data = _NclPop();
					switch(data.kind) {
					case NclStk_VAL: {
/*
* Taking for granted that syntax only allows string litterals here
*/
						data1.u.sub_rec.name = NrmQuarkToString(*(NclQuark*) data.u.data_obj->multidval.val);
							
						if(data.u.data_obj->obj.status != PERMANENT)
							_NclDestroyObj((NclObj)data.u.data_obj);
						break;
					}
					default:	
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Illegal type for coordinate name in coordinate subscript ignoring value");
						data1.u.sub_rec.name = NULL;
						break;
					}
				}
				if(_NclPush(data1) == NhlFATAL) {
					estatus = NhlFATAL;
				} else if(estatus == NhlFATAL) {
					_NclCleanUpStack(1);
				}
			}

void CallNEG_OP(void) {
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

void CallNOT_OP(void) {
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

void CallMOD_OP(void) {
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

void CallOR_OP(void) {
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

void CallAND_OP(void) {
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

void CallXOR_OP(void) {
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

void CallLTSEL_OP(void) {
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

void CallGTSEL_OP(void) {
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

void CallPLUS_OP(void) {
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

void CallMINUS_OP(void) {
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

void CallMUL_OP(void) {
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

void CallMAT_OP(void) {
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

void CallDIV_OP(void) {
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

void CallEXP_OP(void) {
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

void CallLE_OP(void) {
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

void CallGE_OP(void) {
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

void CallGT_OP(void) {
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

void CallLT_OP(void) {
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

void CallEQ_OP(void) {
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

void CallNE_OP(void) {
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

void CallGET_OBJ_OP(void) {
				NclStackEntry obj_name;
				NclStackEntry res_name;
				NclStackEntry data_out;
				NclMultiDValData name = NULL;
				NclMultiDValData res = NULL;

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
				if (! (res && name)) {
					NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
					estatus = NhlFATAL;
					return;
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
void CallFPDEF(void) {
			NclSymbol *func = NULL;
			ptr++;lptr++;fptr++;
			func = (NclSymbol*)(*ptr);
			ptr++;lptr++;fptr++;
			if(func->u.procfunc == NULL) {
				func->u.procfunc = (NclProcFuncInfo*)(*ptr);
			}
			ptr++;lptr++;fptr++;
			_NclChangeSymbolType(func,*ptr);
			return;
		}

void CallFUNC_CALL_OP(void) {
				NclSymbol *func = NULL;
				int caller_level;

				ptr++;lptr++;fptr++;
				func = (NclSymbol*)(*ptr);

				caller_level = _NclFinishFrame();
			/*
			* Doesn't leave anything on the stack if an error has occurred
			*/	
				_NclPushExecute();
				NCL_PROF_PFENTER(func->name);
				estatus = _NclFuncCallOp(func,caller_level);
				NCL_PROF_PFEXIT(func->name);
				_NclPopExecute();
			}

void CallJMP(void) {
				unsigned long offset;
				ptr++;lptr++;fptr++;
				offset = *ptr;	
				ptr = machine + offset - 1;
				lptr = _NclGetCurrentLineRec() + offset - 1;
				fptr = _NclGetCurrentFileNameRec() + offset - 1;
			}

void CallARRAY_LIT_OP(void) {
				NclStackEntry data;
				ptr++;lptr++;fptr++;
				estatus = _NclBuildArray(*ptr,&data);
				if(estatus != NhlFATAL)
					estatus = _NclPush(data);
			}

void CallLISTVAR_LIT_OP(void) {
				NclStackEntry data;
				ptr++;lptr++;fptr++;
				estatus = _NclBuildListVar(*ptr,&data);
				if(estatus != NhlFATAL)
					estatus = _NclPush(data);
			}

void CallPUSH_STRING_LIT_OP(void) {
				NclStackEntry data;
			
				NhlINITVAR(data);

				ptr++;lptr++;fptr++;
				data.u.data_obj = (NclMultiDValData)_NclGetObj(*(int*)ptr);
				data.kind = NclStk_VAL;
				if(data.u.data_obj != NULL) {
					estatus  = _NclPush(data);
				} else {
					estatus  = NhlFATAL;
				}
			}

void CallJMP_SCALAR_TRUE_OP(void) {
	                        NclStackEntry data,data1;;
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
					NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
					estatus = NhlFATAL;
					return;
				}
				
				if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
					if(!_NclIsMissing(val,val->multidval.val)) {
						if((*(logical*)val->multidval.val)) {
							machine = _NclGetCurrentMachine();
							ptr = machine + offset - 1;
							lptr = _NclGetCurrentLineRec() + offset - 1;
							fptr = _NclGetCurrentFileNameRec() + offset - 1;
							if (data.kind == NclStk_VAL) {
								data1.u.data_obj = _NclCopyVal(val,NULL);
								data1.kind = NclStk_VAL;
								if(estatus != NhlFATAL) {
									estatus =  _NclPush(data1);
								}
								if (data.u.data_obj->obj.status != PERMANENT)
									_NclDestroyObj((NclObj)data.u.data_obj);
								return;
							}
						}
					} 
				} 
				if(estatus != NhlFATAL) {
					estatus =  _NclPush(data);
				}
			}

void CallJMP_SCALAR_FALSE_OP(void) {
	                        NclStackEntry data,data1;
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
					NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
					estatus = NhlFATAL;
					return;
				}
				
				if((val->multidval.type->type_class.type & Ncl_Typelogical)&&(val->multidval.kind == SCALAR)) {
					if(!_NclIsMissing(val,val->multidval.val)) {
						if(!(*(logical*)val->multidval.val)) {
							machine = _NclGetCurrentMachine();
							ptr = machine + offset - 1;
							lptr = _NclGetCurrentLineRec() + offset - 1;
							fptr = _NclGetCurrentFileNameRec() + offset - 1;
							if (data.kind == NclStk_VAL) {
								data1.u.data_obj = _NclCopyVal(val,NULL);
								data1.kind = NclStk_VAL;
								if(estatus != NhlFATAL) {
									estatus =  _NclPush(data1);
								}
								if (data.u.data_obj->obj.status != PERMANENT)
									_NclDestroyObj((NclObj)data.u.data_obj);
								return;
							}
						}
					} 
				}  
				if(estatus != NhlFATAL) {
					estatus = _NclPush(data);
				}
			}

void CallJMPFALSE(void) {
				NclStackEntry data;
				NclMultiDValData val;
				unsigned long offset;
				NclObj free_obj = NULL;

				ptr++;lptr++;fptr++;
				offset = *ptr;
				data = _NclPop();
				switch(data.kind) {
				case NclStk_VAL:
					val = data.u.data_obj;	
					if(val->obj.status != PERMANENT) 
						free_obj = (NclObj)val;
					break;
				case NclStk_VAR:
					if(data.u.data_var->obj.status == TEMPORARY)
						free_obj = (NclObj)data.u.data_var;
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
					break;
				default:
					NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error"));
					estatus = NhlFATAL;
                                        return;
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
						NhlPError(NhlFATAL,NhlEUNKNOWN,"The result of the conditional expression yields a missing value. NCL can not determine branch, see ismissing function");
						estatus = NhlFATAL;
					}

					if (free_obj)
						_NclDestroyObj(free_obj);
				} else {
					if (free_obj)
						_NclDestroyObj(free_obj);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Conditional statements (if and do while) require SCALAR logical values, see all and any functions");
					estatus = NhlFATAL;
				}
			}

void CallSET_OBJ_OP(void) {
				NclStackEntry data;
				NclMultiDValData val = NULL;
				int nres;

				data = _NclPop();
				if(data.kind == NclStk_VAL) {
					val = data.u.data_obj;
				} else if(data.kind == NclStk_VAR) {
					val = _NclVarValueRead(data.u.data_var,NULL,NULL);
				}
				ptr++;lptr++;fptr++;
				nres = *(int*)ptr;
				if(val == NULL) {
					_NclCleanUpStack(2*nres);
					estatus = NhlFATAL;
				} else {
					estatus = _NclSetHLUObjOp(val,nres);
					if(data.kind == NclStk_VAL) {
						if(val->obj.status != PERMANENT)
							_NclDestroyObj((NclObj)val);
					} else {
						if(data.u.data_var->obj.status != PERMANENT)
							_NclDestroyObj((NclObj)data.u.data_var);
					}

		
				}
			}

void CallPROC_CALL_OP(void) {
				NclSymbol *proc = NULL;
				int caller_level;

				ptr++;lptr++;fptr++;
				proc = (NclSymbol*)(*ptr);
			
				caller_level = _NclFinishFrame();	
				_NclPushExecute();
				NCL_PROF_PFENTER(proc->name);
				estatus = _NclProcCallOp(proc,caller_level);
				NCL_PROF_PFEXIT(proc->name);
				_NclPopExecute();
			}

void CallINTRINSIC_FUNC_CALL(void) {
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
#ifdef ENABLE_PROFILING
					NclSymbol *func = (NclSymbol *)(*ptr);
					NCL_PROF_PFENTER(func->name);
#endif
					ret = (*((NclSymbol*)*ptr)->u.bfunc->thefunc)();
					NCL_PROF_PFEXIT(func->name);
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

void CallINTRINSIC_PROC_CALL(void) {
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
#ifdef ENABLE_PROFILING
					NclSymbol *proc = (NclSymbol *)(*ptr);
					NCL_PROF_PFENTER(proc->name);
#endif
					ret = (*((NclSymbol*)*ptr)->u.bproc->theproc)();
					NCL_PROF_PFEXIT(proc->name);
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

void CallDUP_TOFS(void) {
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
			}

void CallLOOP_VALIDATE_OP(void) {
					NclStackEntry end_val;
					NclStackEntry inc_var;
					NclStackEntry *tmp_ptr;
					NclMultiDValData tmp_md = NULL;
					NclMultiDValData tmp2_md;
					NclSymbol *l_inc;
					NclSymbol *l_dir;
					NclMultiDValData end_md = NULL;
					NclMultiDValData inc_md = NULL;
					logical dir = False;
					logical result;
					NclStackEntry data;
					double zero = 0;

					ptr++;lptr++;fptr++;
					l_inc = (NclSymbol*)*ptr;
					ptr++,lptr++,fptr++;
					l_dir = (NclSymbol*)*ptr;
					inc_var= _NclPop();
					switch(inc_var.kind) {
					case NclStk_VAL:
						inc_md= inc_var.u.data_obj;
						break;
					case NclStk_VAR:
						inc_md= _NclVarValueRead(inc_var.u.data_var,NULL,NULL);
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
					end_val= _NclPop();
					switch(end_val.kind) {
					case NclStk_VAL:
						end_md = end_val.u.data_obj;
						break;
					case NclStk_VAR:
						end_md = _NclVarValueRead(end_val.u.data_var,NULL,NULL);
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
					tmp_ptr = _NclRetrieveRec(l_dir,DONT_CARE);
					if(tmp_ptr->u.data_var != NULL) {
						tmp_md = _NclVarValueRead(tmp_ptr->u.data_var,NULL,NULL);
						if (tmp_md)
							dir = *(logical*)tmp_md->multidval.val;
						else
							estatus = NhlFATAL;
					} else {
						estatus = NhlFATAL;
					}
					tmp_ptr = _NclRetrieveRec(l_inc,DONT_CARE);
					if(tmp_ptr->u.data_var != NULL) {
						tmp_md = _NclVarValueRead(tmp_ptr->u.data_var,NULL,NULL);
					} else {
						estatus = NhlFATAL;
					}
					if(estatus != NhlFATAL) {	
						char buffer[32];
						if(tmp_md->multidval.kind != SCALAR) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop strides must be scalar, can't execute loop");
							estatus = NhlFATAL;
						} else if(tmp_md->multidval.type->type_class.type & NCL_SNUMERIC_TYPE_MASK) {
							tmp2_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
							_Nclle(tmp2_md->multidval.type,&result,tmp2_md->multidval.val,&zero,NULL,NULL,1,1);
							if(result) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop strides must be positive, can't execute loop");
								estatus = NhlFATAL;
							}
							if(tmp2_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)tmp2_md);
							}
							if (! _NclScalarCoerce(tmp_md->multidval.val,tmp_md->multidval.data_type,(void*)buffer,inc_md->multidval.type->type_class.data_type)) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop stride type must be coercible to loop variable type, can't execute loop");
								estatus = NhlFATAL;
							}
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop strides must be numeric values, can't execute loop");
							estatus = NhlFATAL;
						} 
						if(end_md->multidval.kind != SCALAR) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop end must be scalar, can't execute loop");
							estatus = NhlFATAL;
						} else if(!(end_md->multidval.type->type_class.type & NCL_SNUMERIC_TYPE_MASK)) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop end must be numeric value, can't execute loop");
							estatus = NhlFATAL;
						}
						if(inc_md->multidval.kind != SCALAR) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop variable must be scalar, can't execute loop");
							estatus = NhlFATAL;
						} else if(!(inc_md->multidval.type->type_class.type & NCL_SNUMERIC_TYPE_MASK)) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Loop variable must be numeric value, can't execute loop");
							estatus = NhlFATAL;
						} 
					}
					if(estatus != NhlFATAL) {
						if(inc_md->multidval.type->type_class.type != end_md->multidval.type->type_class.type) {
							tmp_md = _NclCoerceData(inc_md,Ncl_Typedouble,NULL) ;
							tmp2_md = _NclCoerceData(end_md,Ncl_Typedouble,NULL) ;
		
							if(dir) {
								_Ncllt(tmp_md->multidval.type,&result,tmp_md->multidval.val,tmp2_md->multidval.val,NULL,NULL,1,1);
							} else {
								_Nclgt(tmp_md->multidval.type,&result,tmp_md->multidval.val,tmp2_md->multidval.val,NULL,NULL,1,1);
							}
							if(tmp2_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)tmp2_md);
							}
							if(tmp_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)tmp_md);
							}
						} else {
							if(dir) {
								_Ncllt(inc_md->multidval.type,&result,inc_md->multidval.val,end_md->multidval.val,NULL,NULL,1,1);
							} else {
								_Nclgt(inc_md->multidval.type,&result,inc_md->multidval.val,end_md->multidval.val,NULL,NULL,1,1);
							}
						}
						data.kind = NclStk_VAL;
						if(result) {
							data.u.data_obj = _NclCreateFalse();
						} else {
							data.u.data_obj = _NclCreateTrue();
						}
						estatus = _NclPush(data);
						if(inc_var.u.data_obj->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)inc_var.u.data_obj);
						}
						if(end_val.u.data_obj->obj.status != PERMANENT) {
							_NclDestroyObj((NclObj)end_val.u.data_obj);
						}
					}
				}

void CallLOOP_INC_OP(void) {
					NclStackEntry end_val;
					NclStackEntry inc_var;
					NclStackEntry *tmp_ptr;
					NclStackEntry *data_ptr;
					NclMultiDValData tmp_md;
					NclSymbol *l_inc;
					NclSymbol *l_dir;
					NclMultiDValData end_md = NULL;;
					NclMultiDValData inc_md = NULL;
					char *buffer[32],*inc_buf[32],*end_buf[32];
					logical dir = False;
					logical result,result2;
					NclStackEntry data;
					int tmp_estatus;
					NrmQuark inc_varname = NrmNULLQUARK;

					ptr++;lptr++;fptr++;
					l_inc = (NclSymbol*)*ptr;
					ptr++,lptr++,fptr++;
					l_dir = (NclSymbol*)*ptr;
					inc_var= _NclPop();
					switch(inc_var.kind) {
					case NclStk_VAL:
						inc_md= inc_var.u.data_obj;
						break;
					case NclStk_VAR:
						inc_md= _NclVarValueRead(inc_var.u.data_var,NULL,NULL);
						inc_varname = inc_var.u.data_var->var.var_quark;
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
					end_val= _NclPop();
					switch(end_val.kind) {
					case NclStk_VAL:
						end_md = end_val.u.data_obj;
						break;
					case NclStk_VAR:
						end_md = _NclVarValueRead(end_val.u.data_var,NULL,NULL);
						break;
					default:
						estatus = NhlFATAL;
						break;
					}
					tmp_ptr = _NclRetrieveRec(l_dir,DONT_CARE);
					tmp_md = _NclVarValueRead(tmp_ptr->u.data_var,NULL,NULL);
					if (! tmp_md)
						estatus = NhlFATAL;
					else
						dir = *(logical*)tmp_md->multidval.val;
					tmp_ptr = _NclRetrieveRec(l_inc,DONT_CARE);
					tmp_md = _NclVarValueRead(tmp_ptr->u.data_var,NULL,NULL);
					if (! tmp_md)
						estatus = NhlFATAL;
					if (estatus == NhlFATAL) {
						NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error"));
						return;
					}
					result = False;

					/*
					 * Assuming an incrementing situation (dir == False) if the increment variable is incremented prior to testing,
					 * it can possibly overflow before testing positively
					 * for a loop end. Therefore the test decrements the end variable and tests the current value of the increment variable
					 * prior to incrementing it. This could be made faster if the decremented loop variable was saved separately, so we do
					 * not have to do this calculation at each step. 
					 */ 

					if (tmp_md->multidval.type->type_class.type == inc_md->multidval.type->type_class.type &&
					    inc_md->multidval.type->type_class.type == end_md->multidval.type->type_class.type) {
						if (dir) {
							_Nclplus(end_md->multidval.type,buffer,end_md->multidval.val,tmp_md->multidval.val,NULL,NULL,1,1);
							_Ncllt(inc_md->multidval.type,&result,inc_md->multidval.val,buffer,NULL,NULL,1,1);
							_Nclminus(inc_md->multidval.type,inc_md->multidval.val,inc_md->multidval.val,tmp_md->multidval.val,NULL,NULL,1,1);
						}
						else {
							_Nclminus(end_md->multidval.type,buffer,end_md->multidval.val,tmp_md->multidval.val,NULL,NULL,1,1);
							_Nclgt(inc_md->multidval.type,&result,inc_md->multidval.val,buffer,NULL,NULL,1,1);
							_Nclplus(inc_md->multidval.type,inc_md->multidval.val,inc_md->multidval.val,tmp_md->multidval.val,NULL,NULL,1,1);
						}
					}
					else if (inc_md->multidval.type->type_class.type == end_md->multidval.type->type_class.type) {
						_NclScalarCoerce(tmp_md->multidval.val,tmp_md->multidval.data_type,(void*)buffer,inc_md->multidval.type->type_class.data_type);
						if (dir) {
							_Nclplus(end_md->multidval.type,end_buf,end_md->multidval.val,buffer,NULL,NULL,1,1);
							_Ncllt(inc_md->multidval.type,&result,inc_md->multidval.val,end_buf,NULL,NULL,1,1);
							_Nclminus(inc_md->multidval.type,inc_md->multidval.val,inc_md->multidval.val,buffer,NULL,NULL,1,1);
						}
						else {
							_Nclminus(end_md->multidval.type,end_buf,end_md->multidval.val,buffer,NULL,NULL,1,1);
							_Nclgt(inc_md->multidval.type,&result,inc_md->multidval.val,end_buf,NULL,NULL,1,1);
							_Nclplus(inc_md->multidval.type,inc_md->multidval.val,inc_md->multidval.val,buffer,NULL,NULL,1,1);
						}
					}
					else {
						_NclScalarCoerce(tmp_md->multidval.val,tmp_md->multidval.data_type,(void*)buffer,NCL_double);
						_NclScalarCoerce(end_md->multidval.val,end_md->multidval.type->type_class.data_type,end_buf,NCL_double);
						_NclScalarCoerce(inc_md->multidval.val,inc_md->multidval.data_type,inc_buf,NCL_double);
						if (dir) {
							_Nclplus((NclTypeClass)nclTypedoubleClass,buffer,end_buf,buffer,NULL,NULL,1,1);
							_Ncllt((NclTypeClass)nclTypedoubleClass,&result,inc_buf,buffer,NULL,NULL,1,1);
							_NclScalarCoerce(tmp_md->multidval.val,tmp_md->multidval.data_type,(void*)buffer,inc_md->multidval.type->type_class.data_type);
							_Nclminus(inc_md->multidval.type,buffer,inc_md->multidval.val,buffer,NULL,NULL,1,1);
						        _Nclgt(inc_md->multidval.type,&result2,buffer,inc_md->multidval.val,NULL,NULL,1,1);
							memcpy(inc_md->multidval.val,buffer,inc_md->multidval.type->type_class.size);
						}
						else {
							_Nclminus((NclTypeClass)nclTypedoubleClass,buffer,end_buf,buffer,NULL,NULL,1,1);
							_Nclgt((NclTypeClass)nclTypedoubleClass,&result,inc_buf,buffer,NULL,NULL,1,1);
							_NclScalarCoerce(tmp_md->multidval.val,tmp_md->multidval.data_type,(void*)buffer,inc_md->multidval.type->type_class.data_type);
							_Nclplus(inc_md->multidval.type,buffer,inc_md->multidval.val,buffer,NULL,NULL,1,1);
						        _Ncllt(inc_md->multidval.type,&result2,buffer,inc_md->multidval.val,NULL,NULL,1,1);
							memcpy(inc_md->multidval.val,buffer,inc_md->multidval.type->type_class.size);
						}
						if (result2 && ! result) {
							if (inc_varname != NrmNULLQUARK) 
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Prematurely ending loop due to overflow in loop variable '%s'",NrmQuarkToString(inc_varname));
							else 
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Prematurely ending loop due to overflow in loop variable");
							estatus = MIN(estatus,NhlWARNING);
							result = True;
						}
					}

					data.kind = NclStk_VAL;
					if(result) {
/*
* End of loop! Free loop labels
*/
						data_ptr = _NclRetrieveRec(l_dir,DONT_CARE);
						(void)_NclChangeSymbolType(l_dir,UNDEF);
						_NclDestroyObj((NclObj)data_ptr->u.data_var);
						data_ptr->kind = NclStk_NOVAL;
						data_ptr->u.data_var = NULL;
						data_ptr = _NclRetrieveRec(l_inc,DONT_CARE);
						(void)_NclChangeSymbolType(l_inc,UNDEF);
						_NclDestroyObj((NclObj)data_ptr->u.data_var);
						data_ptr->kind = NclStk_NOVAL;
						data_ptr->u.data_var = NULL;
						data.u.data_obj = _NclCreateFalse();
					} else {
						data.u.data_obj = _NclCreateTrue();
					}
					tmp_estatus = _NclPush(data);
					estatus = MIN(estatus,tmp_estatus);
					if(inc_var.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)inc_var.u.data_obj);
					}
					if(end_val.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)end_val.u.data_obj);
					}
				}

void CallVAR_DIM_OP(void) {
				NclSymbol *thesym;
				long dim_num;
				NclStackEntry data;
				NclMultiDValData data_md = NULL,tmpmis;
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
                                                NhlPError(NhlWARNING,NhlEUNKNOWN,"Dimension (%d) has not been defined",dim_num);
                                                data.kind = NclStk_VAL;
                                                tmpmis = _NclCreateMissing();
                                                data.u.data_obj = tmpmis;
						estatus = _NclPush(data);
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

void CallASSIGN_VAR_DIM_OP(void) {
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
						dim_name = NrmQuarkToString(*(NclQuark *)dim_expr_md->multidval.val);
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
					switch (dim_expr.kind) {
					case NclStk_VAL:
						if((dim_expr_md->obj.status != PERMANENT)&&(dim_expr_md->obj.ref_count == 0)) {
							_NclDestroyObj((NclObj)dim_expr_md);
						}
						break;
					case NclStk_VAR:	
						if((dim_expr.u.data_var->var.var_type == VARSUBSEL) &&
						   (dim_expr.u.data_var->obj.status != PERMANENT)&&(dim_expr.u.data_var->obj.ref_count == 0)) {
							_NclDestroyObj((NclObj)dim_expr.u.data_var);
						}
						break;
					default:
						break;
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

void CallNEW_OP(void) {
				NclStackEntry size_expr;
				NclStackEntry missing_expr;
				NclSymbol *data_type = NULL;
				NclStackEntry data_type_expr;
				NclMultiDValData tmp_md = NULL;

				if(*ptr == NEW_WM_OP) {
					missing_expr = _NclPop();
				} else {
					missing_expr.kind = NclStk_NOVAL;
					missing_expr.u.data_obj = NULL;
				}
				size_expr = _NclPop();
				ptr++; lptr++; fptr++;
				if((NclSymbol*)*ptr ==NULL) {
					data_type_expr = _NclPop();
					switch(data_type_expr.kind) {
					case NclStk_VAL:
						tmp_md = (NclMultiDValData)data_type_expr.u.data_obj;
						break;
					case NclStk_VAR:
						tmp_md = (NclMultiDValData)_NclGetObj(data_type_expr.u.data_var->var.thevalue_id);
						break;
					default:
						NHLPERROR((NhlFATAL,NhlEUNKNOWN, "Internal error")); 
						estatus = NhlFATAL;
						break;
					}
					if(tmp_md && tmp_md->multidval.data_type != NCL_string) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"new: data type must either be a keyword or string");
						estatus = NhlFATAL;
					} else {
						data_type = _NclLookUp(NrmQuarkToString(*(NclQuark *)tmp_md->multidval.val));
					}	
				} else {
					data_type = (NclSymbol*)*ptr;
				}
				if(estatus != NhlFATAL) {
					estatus = _NclNewOp(data_type,size_expr,missing_expr);
					switch(missing_expr.kind) {
					case NclStk_VAL:
						if(missing_expr.u.data_obj->obj.status != PERMANENT) {	
							_NclDestroyObj((NclObj)missing_expr.u.data_obj);
						}
						break;
					case NclStk_VAR:
						if(missing_expr.u.data_var->obj.status != PERMANENT) {	
							_NclDestroyObj((NclObj)missing_expr.u.data_var);
						}
						break;
					default:
						break;
					}
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
					if((NclSymbol*)*ptr ==NULL) {
						switch(data_type_expr.kind) {
						case NclStk_VAL:
							if(data_type_expr.u.data_obj->obj.status != PERMANENT) {	
								_NclDestroyObj((NclObj)data_type_expr.u.data_obj);
							}
							break;
						case NclStk_VAR:
							if(data_type_expr.u.data_var->obj.status != PERMANENT) {	
								_NclDestroyObj((NclObj)data_type_expr.u.data_var);
							}
							break;
						default:
							break;
						}
					}
				}
				
			}
void CallISDEFINED_OP(void) {
				NclStackEntry* var;
				NclSymbol *var_sym;


				ptr++;lptr++;fptr++;
				var_sym = (NclSymbol*)*ptr;
				var = _NclRetrieveRec(var_sym,DONT_CARE);
				if((var== NULL) || (var->kind == NclStk_NOVAL)|| (var->u.data_var == NULL)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Undefined identifier: (%s) is undefined, can't continue",var_sym->name);
					estatus = NhlFATAL;
				}
			}

void CallVARVAL_READ_OP(void) {
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
				nsubs = *(int*)ptr;
				if((var == NULL)||(var->u.data_var == NULL)) {
					sym = _NclLookUp(sym->name);
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined",sym->name);
					_NclCleanUpStack(nsubs);
					estatus = NhlFATAL;
				} else if(nsubs == 0) {
					data1.kind = NclStk_VAL;
					data1.u.data_obj= _NclVarValueRead(var->u.data_var,NULL,NULL);
					if(data1.u.data_obj != NULL) {
						estatus = _NclPush(data1);
					} else {
						estatus = NhlFATAL;
					}
				} else if(nsubs != var->u.data_var->var.n_dims) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimensions of variable,(%d) Subscripts used, (%d) Subscripts expected",nsubs,var->u.data_var->var.n_dims);
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				} else {
					sel_ptr = _NclGetVarSelRec(var->u.data_var);
					sel_ptr->n_entries = nsubs;
					for(i=0;i<nsubs;i++) {
						dim_is_ref[i] = 0;
					}
					for(i=0;i<nsubs;i++) {
						data =_NclPop();
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						case COORD_VECT:
							ret = _NclBuildCoordVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						case COORD_RANGE:
						case COORD_SINGLE:
							ret = _NclBuildCoordRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
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
						data1.kind = NclStk_VAL;
						data1.u.data_obj= _NclVarValueRead(var->u.data_var,sel_ptr,NULL);
						if(data1.u.data_obj != NULL) {
							estatus = _NclPush(data1);
						} else {
							estatus = NhlFATAL;
						}
					}
				}
			}

void CallVAR_READ_OP(void) {
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
				nsubs = *(int*)ptr;
				if((var == NULL)||(var->u.data_var == NULL)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined",sym->name);
					_NclCleanUpStack(nsubs);
					estatus = NhlFATAL;
				} else if(nsubs == 0) {
					estatus = _NclPush(*var);
				} else if(nsubs != var->u.data_var->var.n_dims) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimensions of variable,(%d) Subscripts used, (%d) Subscripts expected",nsubs,var->u.data_var->var.n_dims);
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				} else {
					sel_ptr = _NclGetVarSelRec(var->u.data_var);
					sel_ptr->n_entries = nsubs;
					for(i=0;i<nsubs;i++) {
						dim_is_ref[i] = 0;
					}
					for(i=0;i<nsubs;i++) {
						data =_NclPop();
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						case COORD_VECT:
							ret = _NclBuildCoordVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						case COORD_SINGLE:
						case COORD_RANGE:
							ret = _NclBuildCoordRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
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
						if(data1.u.data_var != NULL) {
							estatus = _NclPush(data1);
						} else {
							estatus = NhlFATAL;
						}
					}
				}
			}

static void performASSIGN_VAR(NclSymbol *sym, int nsubs, NclStackEntry *lhs_var)
{
	NclStackEntry rhs;
	NclStackEntry data;
	NclMultiDValData rhs_md = NULL;
	NclMultiDValData tmp_md = NULL;
	NclSelectionRecord *sel_ptr = NULL;
	ng_size_t i;
	NhlErrorTypes ret = NhlNOERROR;
	NhlArgVal udata;
			
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
						lhs_var->kind = NclStk_NOVAL;
						if(lhs_var->u.data_var != NULL) {
							(void)_NclChangeSymbolType(sym,VAR);
							lhs_var->kind = NclStk_VAR;
						} else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
							estatus = NhlWARNING;
						}
					} 
				} else if((rhs.kind == NclStk_VAR)&&(rhs.u.data_var->obj.status != TEMPORARY)) {
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
				} else if((rhs.kind == NclStk_VAR)&&(rhs.u.data_var->obj.status == TEMPORARY)) {
/*
* Since lhs is not defined and this is ASSIGN_VAR_OP the change var_type to VAR
*/
					lhs_var->u.data_var = rhs.u.data_var;
					lhs_var->u.data_var->var.thesym = sym;
					lhs_var->u.data_var->var.var_type = NORMAL;
					lhs_var->u.data_var->var.var_quark = NrmStringToQuark(sym->name);
					if(lhs_var->u.data_var != NULL) {
						(void)_NclChangeSymbolType(sym,VAR);
						lhs_var->kind = NclStk_VAR;
					} else {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
						estatus = NhlWARNING;
						lhs_var->kind = NclStk_NOVAL;
					}
					_NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT);
					_NclCallCallBacks((NclObj)lhs_var->u.data_var,CREATED);
					if(lhs_var->u.data_var->obj.obj_type_mask & Ncl_HLUVar) {
						udata.ptrval = NclMalloc(sizeof(NclHLUUData));
						((NclHLUUData*)udata.ptrval)->vq = lhs_var->u.data_var->var.var_quark;
						((NclHLUUData*)udata.ptrval)->aq = -1;
						tmp_md = (NclMultiDValData)_NclGetObj(lhs_var->u.data_var->var.thevalue_id);
						((NclHLUVar)lhs_var->u.data_var)->hvar.cb = _NclAddCallback((NclObj)tmp_md,NULL,_NclHLUVarValChange,HLUVALCHANGE,&udata);
						((NclHLUVar)lhs_var->u.data_var)->hvar.udata = udata.ptrval;

						for(i = 0; i < tmp_md->multidval.totalelements;i++) {
							if(lhs_var->u.data_var->var.thesym != NULL) {
								_NclAddHLURef(((obj*)tmp_md->multidval.val)[i],lhs_var->u.data_var->var.var_quark,-1,i,lhs_var->u.data_var->var.thesym->level);
							} else {
								_NclAddHLURef(((obj*)tmp_md->multidval.val)[i],lhs_var->u.data_var->var.var_quark,-1,i,-1);
							}
						}
					}
				} else if(rhs.kind == NclStk_LIST) {
					int n;
					NclDimRec dim_info[NCL_MAX_DIMENSIONS];
					NclVar tmpvar;
					NclList tmplist = (NclList)rhs.u.data_list;

					tmpvar = (NclVar)_NclGetObj(tmplist->list.first->obj_id);

					rhs_md = _NclVarValueRead(tmpvar,NULL,NULL);
					if(rhs_md != NULL)
					{
						if(rhs_md->obj.status != TEMPORARY)
						{
							tmp_md = rhs_md;
							rhs_md= _NclCopyVal(rhs_md,NULL);
							if(tmp_md->obj.status != PERMANENT)
							{
								_NclDestroyObj((NclObj)tmp_md);
							}
						}

						for(n = 0; n < rhs_md->multidval.n_dims; n++)
						{
							dim_info[n].dim_size = rhs_md->multidval.dim_sizes[n];
							dim_info[n].dim_num = n;
							dim_info[n].dim_quark = -1;
						}

						lhs_var->u.data_var= _NclVarCreate(NULL,NULL,
							Ncl_Var,0,sym,
							rhs_md,
							dim_info,
							-1,
							NULL,
							NORMAL,sym->name,PERMANENT);
						if(lhs_var->u.data_var != NULL)
						{
							(void)_NclChangeSymbolType(sym,VAR);
							lhs_var->kind = NclStk_VAR;
						}
						else
						{
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Could not create variable (%s)",sym->name);
							estatus = NhlWARNING;
							lhs_var->kind = NclStk_NOVAL;
						}
					} else {
						estatus = NhlFATAL;
					} 
				/*
					if(rhs.u.data_var->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)rhs.u.data_var);
					}
				*/
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
				sel_ptr = _NclGetVarSelRec(lhs_var->u.data_var);
				sel_ptr->n_entries = nsubs;
			} else {
				sel_ptr = NULL;
			}
			if(estatus != NhlFATAL) {
				for(i=0;i<nsubs;i++) {
					data =_NclPop();
					switch(data.u.sub_rec.sub_type) {
					case INT_VECT:
/*
* Need to free some stuff here
*/							
						ret = _NclBuildVSelection(lhs_var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
						break;
					case INT_SINGLE:
					case INT_RANGE:
/*
* Need to free some stuff here
*/								
						ret = _NclBuildRSelection(lhs_var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
						break;
					case COORD_VECT:
						ret = _NclBuildCoordVSelection(lhs_var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
						break;
					case COORD_SINGLE:
					case COORD_RANGE:
						ret = _NclBuildCoordRSelection(lhs_var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
						break;
					}
					_NclFreeSubRec(&data.u.sub_rec);
					if(ret < NhlWARNING) {
						estatus = NhlFATAL;
						break;
					}
				}
			}
			if(estatus != NhlFATAL) {
				rhs = _NclPop();	
				if(rhs.kind == NclStk_VAL) {
					rhs_md = rhs.u.data_obj;
					if(rhs_md != NULL) {
						ret = _NclAssignToVar(lhs_var->u.data_var,rhs_md,sel_ptr);
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
}

void CallASSIGN_VAR_OP(void)
{
	NclStackEntry *lhs_var = NULL;
	int nsubs;	
	NclSymbol *sym = NULL;

	ptr++;lptr++;fptr++;
	sym = (NclSymbol*)(*ptr);

	ptr++;lptr++;fptr++;
	nsubs = *(int*)ptr;

	lhs_var = _NclRetrieveRec(sym,WRITE_IT);

	performASSIGN_VAR(sym, nsubs, lhs_var);
}

NhlErrorTypes ClearDataBeforeReassign(NclStackEntry *data)
{
    NclStackEntry* var;
    NclSymbol *thesym;
    int sub_sel = 0;
    NclObj tmp,pobj;
    NclRefList *rlist = NULL;
    NhlErrorTypes ret = NhlNOERROR;

    switch(data->kind)
    {
    case NclStk_VAL:
        if (data->u.data_obj->obj.obj_type == Ncl_MultiDVallistData)
        {
            NclObj list_obj = _NclGetObj(*(int*) data->u.data_obj->multidval.val);
            while ((tmp = _NclListPop(list_obj)))
            {
                if (tmp->obj.obj_type == Ncl_Var)
                {
                    switch(((NclVar)tmp)->var.var_type)
                    {
                        case VARSUBSEL:
                        case COORDSUBSEL:
                        case FILEVARSUBSEL:
                            sub_sel = 1;
                            break;
                        case PARAM:
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "Deletion of parameters to functions and procedures is not allowed in NCL");
                            return(NhlFATAL);
                        case NORMAL:
                        case COORD:
                        case FILEVAR: 
                        case RETURNVAR:
                        case HLUOBJ :
                        default:
                            sub_sel = 0;
                            break;
                    }

                    if(((NclVar)tmp)->var.thesym != NULL && !sub_sel)
                    {
                        var = _NclRetrieveRec(((NclVar)tmp)->var.thesym,DONT_CARE);
                        thesym = ((NclVar)tmp)->var.thesym;
                        if(((NclVar)tmp)->var.var_type == NORMAL)
                        {
                                                       /*
                            * Can't destroy symbol since it may be referenced from the instruction
                            * sequence. Changing it to UNDEF should do the trick though
                            */
                            _NclChangeSymbolType(thesym,UNDEF);
                        }
                        _NclDestroyObj((NclObj)tmp);
                        if(var != NULL)
                        {
                            var->u.data_var = NULL;
                            var->kind = NclStk_NOVAL;
                        }
                    }
                }
            }
        }
        _NclDestroyObj((NclObj)data->u.data_obj);
        break;
    case NclStk_VAR:
        if(data->u.data_var != NULL)
        {
            switch(data->u.data_var->var.var_type)
            {
                case VARSUBSEL:
                case COORDSUBSEL:
                case FILEVARSUBSEL:
                    sub_sel = 1;
                    break;
                case PARAM:
                    NhlPError(NhlFATAL,NhlEUNKNOWN,"Deletion of parameters to functions and procedures is not allowed in NCL");
                    return(NhlFATAL);
                case NORMAL:
                case COORD:
                case FILEVAR: 
                case RETURNVAR:
                case HLUOBJ :
                default:
                    sub_sel = 0;
                    break;
            }

        }

        if((data->u.data_var != NULL)&&(data->u.data_var->var.thesym != NULL)&&(!sub_sel))
        {
            var = _NclRetrieveRec(data->u.data_var->var.thesym,DONT_CARE);
            thesym = data->u.data_var->var.thesym;
            tmp = (NclObj)data->u.data_var;
            if(data->u.data_var->var.var_type == NORMAL)
            {
		/*
		* Can't destroy symbol since it may be referenced from the instruction
		* sequence. Changing it to UNDEF should do the trick though
		*/
                _NclChangeSymbolType(thesym,UNDEF);
                data->kind = NclStk_NOVAL;
                data->u.data_obj = NULL;
                _NclPutRec(thesym, data);
            }
            _NclDestroyObj((NclObj)tmp);
            if(var != NULL)
            {
                var->u.data_var = NULL;
                var->kind = NclStk_NOVAL;
            }
        }
        else
        {
            if((data->u.data_obj->obj.ref_count != 0)&&(!sub_sel))
            {
                int id = data->u.data_obj->obj.id;
                switch(data->u.data_obj->obj.obj_type)
                {
                case Ncl_CoordVar:
                    rlist = data->u.data_obj->obj.parents;
                    while(rlist != NULL)
                    {
                        pobj = _NclGetObj(rlist->pid);
                        if(pobj->obj.obj_type == Ncl_Var)
                        {
                            _NclDeleteCoordVar((NclVar)pobj,NrmQuarkToString(data->u.data_var->var.var_quark));
                        }
                        else
                        {
                            _NclDelParent((NclObj)data->u.data_obj,(NclObj)pobj);
                        }
                        if (_NclGetObj(id) != NULL)
                            rlist = data->u.data_obj->obj.parents;
                        else
                            rlist = NULL;
                    }
                    break;
                default:
                    rlist = data->u.data_obj->obj.parents;
                    while(rlist != NULL)
                    {
                        pobj = _NclGetObj(rlist->pid);
                        _NclDelParent((NclObj)data->u.data_obj,(NclObj)pobj);
                        rlist = rlist->next;
                    }
                    break;
                }
            }
            else
            {
                var = NULL;
                tmp = (NclObj)data->u.data_var;
                _NclDestroyObj((NclObj)tmp);
                if(var != NULL)
                {
                    var->u.data_var = NULL;
                    var->kind = NclStk_NOVAL;
                }
            }
        }
        break;
    default:
        break;
    }

    data->kind = NclStk_NOVAL;
    data->u.data_obj = NULL;

    return ret;
}


void CallREASSIGN_VAR_OP(void)
{
    NclStackEntry *lhs_var = NULL;
    int nsubs;    
    NclSymbol *sym = NULL;

    ptr++;lptr++;fptr++;
    sym = (NclSymbol*)(*ptr);

    ptr++;lptr++;fptr++;
    nsubs = *(int*)ptr;

    lhs_var = _NclRetrieveRec(sym,WRITE_IT);

    ClearDataBeforeReassign(lhs_var);

    performASSIGN_VAR(sym, nsubs, lhs_var);
}

void CallNEW_FRAME_OP(void) {
				NclSymbol *proc;
				int offset;
				ptr++;lptr++;fptr++;
				proc = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				offset = (*(int*)ptr);
				if((proc->u.procfunc != NULL)&&(offset >= 0)) {
					if(proc->u.procfunc->thescope != NULL) {	
						_NclPushScope(proc->u.procfunc->thescope);
					}
					estatus = _NclPushFrame(proc,offset);
				} else {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Procedure or Function (%s) is no longer defined, check for undef's",proc->name);
					estatus = NhlFATAL;
				}
			}

void CallCONVERT_TO_LOCAL(void) {
				NclSymbol *thesym = NULL;
				NclGenProcFuncInfo *pfinfo = NULL;
				NclSymbol *argsym = NULL;
				NclStackEntry data,tmp_data;
				NclObjTypes obj_type_param;
				NclObjTypes obj_type_arg;
				NclMultiDValData tmp_md = NULL;
				NclVar tmp_var = NULL;
				int i;
				int arg_num = -1;
				unsigned int except_mds = ((int)Ncl_MultiDValHLUObjData | (int)Ncl_MultiDValnclfileData);

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
				ptr++;lptr++;fptr++;
				arg_num = (*(int*)ptr);

				switch(thesym->type) {
				case IPROC:
				case PIPROC:
				case IFUNC:
					pfinfo = (NclGenProcFuncInfo*)thesym->u.bproc;
					break;
				case NFUNC:
				case NPROC:
					pfinfo = (NclGenProcFuncInfo*)thesym->u.procfunc;
					break;
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
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) is (%d), (%d) dimensions were expected ",arg_num,thesym->name,data.u.data_var->var.n_dims,pfinfo->theargs[arg_num].n_dims);
								estatus = NhlFATAL;

							} else if(Ncl_Typelist == obj_type_arg) {
								/*
								Skip dimension check for list.
								Wei 2/4/2011
								*/
								estatus = NhlNOERROR;
							} else {
								for(i = 0; i< pfinfo->theargs[arg_num].n_dims; i++) {
									if(pfinfo->theargs[arg_num].dim_sizes[i] != -1) {
										if(pfinfo->theargs[arg_num].dim_sizes[i] != data.u.data_var->var.dim_info[i].dim_size) {
											NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of elements of dimension (%d) of argument (%d) is (%zd) in function (%s), expected (%zd) elements",i,arg_num,data.u.data_var->var.dim_info[i].dim_size,thesym->name,pfinfo->theargs[arg_num].dim_sizes[i]);
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
						} else if(pfinfo->theargs[arg_num].n_dims > 0) {
							if(pfinfo->theargs[arg_num].n_dims != data.u.data_var->var.n_dims) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) is (%d), (%d) dimensions were expected ",arg_num,thesym->name,data.u.data_var->var.n_dims,pfinfo->theargs[arg_num].n_dims);
								estatus = NhlFATAL;
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
							} else {
								tmp_md = NULL;
							}
							if ((estatus != NhlWARNING)&&(estatus!=NhlFATAL)) {
                                                		_NclAddObjToParamList((NclObj)data.u.data_var,arg_num);
							} else if((estatus == NhlWARNING)&&(tmp_md != NULL)) {
                                                		_NclAddObjToParamList((NclObj)tmp_md,arg_num);
							} else {
								estatus = NhlFATAL;
							}
							if((thesym->type != IPROC)&&(thesym->type != PIPROC) && (thesym->type != IFUNC)&&(estatus != NhlFATAL)) {
/*
* Variable subsections also point to the symbol of the main variable so the AddObjToParamList just
* stores the symbol rather than the pointer to variable record
*/
                                                		argsym = pfinfo->theargs[arg_num].arg_sym;
								if(tmp_md != NULL) {
									if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
										argsym->type = VAR;
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
										argsym->type = VAR;
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
										argsym->type = VAR;
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
									if(data.u.data_var->obj.status != PERMANENT) {
										_NclDestroyObj((NclObj)data.u.data_var);
									} 
                               	  } else {
									tmp_md = (NclMultiDValData)_NclGetObj(data.u.data_var->var.thevalue_id);
									if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
										argsym->type = VAR;
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
										argsym->type = VAR;
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
										argsym->type = VAR;
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
										tmp_var->var.ref_var = (NclObj) data.u.data_var;
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
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) is (%d), (%d) dimensions were expected ",arg_num,thesym->name,data.u.data_obj->multidval.n_dims,pfinfo->theargs[arg_num].n_dims);
									estatus = NhlFATAL;
	
								} else {
									for(i = 0; i< pfinfo->theargs[arg_num].n_dims; i++) {
										if(pfinfo->theargs[arg_num].dim_sizes[i] != -1) {
											if(pfinfo->theargs[arg_num].dim_sizes[i] != data.u.data_obj->multidval.dim_sizes[i]) {
												NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of elements of dimension (%d) of argument (%d) is (%zd) in function (%s), expected (%zd) elements",i,arg_num,data.u.data_obj->multidval.dim_sizes[i],thesym->name,pfinfo->theargs[arg_num].dim_sizes[i]);
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
							} else if(pfinfo->theargs[arg_num].n_dims > 0) {
                                                        	if(pfinfo->theargs[arg_num].n_dims != data.u.data_obj->multidval.n_dims) {
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of dimensions in parameter (%d) of (%s) is (%d), (%d) dimensions were expected ",arg_num,thesym->name,data.u.data_obj->multidval.n_dims,pfinfo->theargs[arg_num].n_dims);
                                                                	estatus = NhlFATAL;
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
								_NclAddObjToParamList((NclObj)tmp_md,arg_num);
                                                		if(estatus != NhlFATAL) {
	
									if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
										argsym->type = VAR;
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
										argsym->type = VAR;
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
										argsym->type = VAR;
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
                                                        	data.kind = NclStk_VAL;
                                                        	data.u.data_obj = tmp_md;
								argsym = pfinfo->theargs[arg_num].arg_sym;
								_NclAddObjToParamList((NclObj)tmp_md,arg_num);

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
			}

void CallASSIGN_FILEVAR_DIM_OP(void) {
				NclFile file;
				NclStackEntry* file_ptr,data,rhs_data,fvar;
				NclMultiDValData file_md,rhs_md = NULL;
				NclMultiDValData dim_expr_md,thevalue;
				NclSymbol* file_sym;
				NclQuark var_name = NrmNULLQUARK;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
					estatus = NhlFATAL;
					_NclCleanUpStack(2);
				} 
			}

void CallPARAM_FILEVAR_DIM_OP(void) {
				NclSymbol *file;
				NclQuark var_name;
				NclStackEntry dim_expr;
				NclMultiDValData tmp_md,tmp1_md,file_md,thevalue,tmpmis;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					var_name = NrmNULLQUARK;
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
				if((estatus != NhlFATAL)&&((file_ptr != NULL)&&(file_ptr->u.data_var != NULL))) {
					file_obj = NULL;
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
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Internal error: An incorrect type of object was placed on the stack");
						estatus = NhlFATAL;
						tmp_md = NULL;
						break;
					}
					if((tmp_md != NULL)) {
						if(!(tmp_md->multidval.type->type_class.type & Ncl_Typelong)) {
							tmp1_md = _NclCoerceData(tmp_md,Ncl_Typelong,NULL);
							if(tmp1_md == NULL) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Could not coerce dimension ref into long");
								estatus = NhlFATAL;
							} else if(tmp_md->obj.status != PERMANENT) {
								_NclDestroyObj((NclObj)tmp_md);
								tmp_md =tmp1_md;
							} else {
								tmp_md = tmp1_md;
							}	
						}
						if(estatus != NhlFATAL) {
							if(tmp_md->multidval.kind == SCALAR) {
								tmp1_md = _NclFileVarReadDim(file_obj,var_name,(NclQuark)-1,*(long*)tmp_md->multidval.val);
								if(tmp_md->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)tmp_md);
								}
								if(tmp1_md != NULL) {
									data.kind = NclStk_VAL;
									data.u.data_obj = tmp1_md;
									estatus = _NclPush(data);
								} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Dimension (%d) has not been defined",*(long*)tmp_md->multidval.val);
                                                			data.kind = NclStk_VAL;
                                                			tmpmis = _NclCreateMissing();
                                                			data.u.data_obj = tmpmis;
                                                			estatus = _NclPush(data);
								}
							} else 	{
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension references must be scalar");
							}
						}
					}
				} else {
					estatus = NhlFATAL;
					_NclCleanUpStack(2);
				}
			}

void CallCREATE_OBJ_OP(void) {
				int nres;
				NclSymbol *objtype;
				NclStackEntry parent,data;
				NclStackEntry obj_name_expr;
				NclMultiDValData obj_name_md;
				NclMultiDValData tmp_md = NULL;
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
				objname = NrmQuarkToString(*(NclQuark *)obj_name_md->multidval.val);
				if(obj_name_md->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)obj_name_md);
				}
				
			
				ptr++;lptr++;fptr++;
				nres = *(int*)ptr;
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

void CallVARATT_OP(void) {
				NclSymbol *thesym = NULL;
				char*	attname = NULL;
				int	nsubs;
				NclStackEntry *var = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry data,avar;
				NhlErrorTypes ret = NhlNOERROR;
				NclMultiDValData tmpmis = NULL,thevalue = NULL;

				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					attname = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
/*
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = (*(int*)ptr);

				var = _NclRetrieveRec(thesym,READ_IT);
				if(var->u.data_var != NULL) {
					if(_NclVarIsAtt(var->u.data_var,attname)) {
						if(nsubs == 1) {
							sel_ptr = _NclGetVarSelRec(var->u.data_var);
							sel_ptr->n_entries = 1;
							data =_NclPop();
							if(data.u.sub_rec.name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
								estatus = NhlWARNING;
							}
							switch(data.u.sub_rec.sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/						
								ret = _NclBuildVSelection(NULL,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
							case INT_SINGLE:
							case INT_RANGE:
/*
* Need to free some stuff here
*/								
								ret = _NclBuildRSelection(NULL,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
								break;
							case COORD_VECT:
							case COORD_SINGLE:
							case COORD_RANGE:
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
								estatus = NhlFATAL;
								break;
							}
							_NclFreeSubRec(&data.u.sub_rec);
							if(ret < NhlWARNING) {
								estatus = ret;
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

void CallVAR_COORD_ATT_OP(void) {
				NclStackEntry *var = NULL;
				NclVar coord_var;
				NclStackEntry data,cvar,avar;
				NclSymbol* thesym = NULL;
				char *coord_name = NULL;
				char *attname = NULL;
				NhlErrorTypes ret = NhlNOERROR;
				NclSelectionRecord *sel_ptr = NULL;
				NclMultiDValData tmpmis = NULL,thevalue = NULL;

				int nsubs = 0;

				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					attname = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				thevalue = NULL;
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;

				var = _NclRetrieveRec(thesym,WRITE_IT);
				if(var->u.data_var != NULL) {
					if(_NclIsDim(var->u.data_var,coord_name) == -1) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension in variable (%s).",coord_name, thesym->name);
						estatus = NhlFATAL;
					} else {
						coord_var = _NclReadCoordVar(var->u.data_var,coord_name,NULL);
						if(coord_var != NULL) {
							if(_NclVarIsAtt(coord_var,attname)) {
								if(nsubs == 1) {
									sel_ptr = _NclGetVarSelRec(coord_var);
									sel_ptr->n_entries = 1;
									data =_NclPop();
									if(data.u.sub_rec.name != NULL) {
										NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
										estatus = NhlWARNING;
									}
									switch(data.u.sub_rec.sub_type) {
									case INT_VECT:
										ret = _NclBuildVSelection(NULL,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
										break;
									case INT_SINGLE:
									case INT_RANGE:
										ret = _NclBuildRSelection(NULL,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
										break;
									case COORD_VECT:
									case COORD_SINGLE:
									case COORD_RANGE:
										NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
										estatus = NhlFATAL;
										break;
									}
									_NclFreeSubRec(&data.u.sub_rec);
									if(ret < NhlWARNING) {
										estatus = ret;
									}
								} else if(nsubs != 0) {
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Attributes only have one dimension, %d subscripts used",nsubs);
									estatus = NhlFATAL;
								}
								if(estatus != NhlFATAL) {
									data.u.data_obj = _NclReadAtt(coord_var,attname,sel_ptr);
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
							NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not coordinate variable in variable (%s).",coord_name, thesym->name);
							estatus = NhlFATAL;
						}
						if(estatus != NhlFATAL) 
							estatus = _NclPush(data);

					}
				} else {
					estatus = NhlFATAL;
				}
			}

void CallASSIGN_VAR_COORD_OP(void) {
				NclStackEntry *var = NULL,cvar;
				NclStackEntry data;
				NclSymbol* thesym = NULL;
				char *coord_name = NULL;
				int nsubs = 0;
				NhlErrorTypes ret = NhlNOERROR;
				NclSelectionRecord *sel_ptr = NULL;
				NclMultiDValData thevalue = NULL;
				int id;
				
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;

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
						sel_ptr = _NclGetVarSelRec(var->u.data_var);
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec.name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							estatus = NhlWARNING;
						}
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret = _NclBuildVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_SINGLE:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
							NclFree(sel_ptr);
							sel_ptr = NULL;
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
						if(ret < NhlWARNING)
							estatus = NhlFATAL;

					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts on left hand side of assignment",nsubs);
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
/*
* DAMN write coord is al F'ed up The followin is  a kludge so I can know if WRiteCoordVar actually destroyed tehvaleu
*/
							id = thevalue->obj.id;
							ret = _NclWriteCoordVar(var->u.data_var,thevalue,coord_name,sel_ptr);
							if(data.kind == NclStk_VAR) {
								_NclAttCopyWrite(_NclReadCoordVar(var->u.data_var,coord_name,NULL),data.u.data_var);
							}
							if(ret<estatus){
								estatus = ret;
							}
						} else {
							id = -1;
							estatus = NhlFATAL;
						}
/* _NclWriteCoordVar destroys non-permanent input so the following is not needed
* Rather than fix _NclWriteCoordVar to not free it was just easier to 
* comment the followign out.
*/
						switch(data.kind) {
						case NclStk_VAL: 
							if( (_NclGetObj(id)!= NULL)&&(data.u.data_obj->obj.status != PERMANENT))
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

void CallASSIGN_VAR_COORD_ATT_OP(void) {
				NclStackEntry *var = NULL,avar,cvar;
				NclVar coord_var;
				NclStackEntry data1;
				NclSymbol* thesym = NULL;
				char *coord_name = NULL;
				char *attname = NULL;
				NhlErrorTypes ret = NhlNOERROR;
				int nsubs;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry value;
				NclMultiDValData value_md = NULL,thevalue = NULL;


	
				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					attname = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				thevalue = NULL;
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;
				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;

				var = _NclRetrieveRec(thesym,WRITE_IT);
				if((var == NULL)||(var->u.data_var == NULL)) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable (%s) is undefined, can not assign attribute (%s)",thesym->name,attname);
					estatus = NhlFATAL;
				} else if(_NclIsDim(var->u.data_var,coord_name) == -1) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a named dimension in variable (%s).",coord_name,thesym->name);
					estatus = NhlFATAL;
				} else {
					coord_var = _NclReadCoordVar(var->u.data_var,coord_name,NULL);
					if(coord_var != NULL) {
						if((!_NclVarIsAtt(coord_var,attname))&&(nsubs >0)) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to subscript undefined coordinate variable attribute");
							estatus = NhlFATAL;
						} else if(nsubs == 1) {
							sel_ptr = _NclGetVarSelRec(coord_var);
							sel_ptr->n_entries = 1;
							data1 =_NclPop();
							if(data1.u.sub_rec.name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
								estatus = NhlWARNING;
							}
							switch(data1.u.sub_rec.sub_type) {
							case INT_VECT:
	/*
	* Need to free some stuff here
	*/						
								ret =_NclBuildVSelection(NULL,&data1.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
							case INT_SINGLE:
							case INT_RANGE:
	/*
	* Need to free some stuff here
	*/								
								ret =_NclBuildRSelection(NULL,&data1.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
								break;
							case COORD_VECT:
							case COORD_SINGLE:
							case COORD_RANGE:
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
								estatus = NhlFATAL;
								break;
							}
							_NclFreeSubRec(&data1.u.sub_rec);
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
							ret = _NclWriteAtt(coord_var,attname,value_md,sel_ptr);
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
						NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not coordinate variable in variable(%s).",coord_name, thesym->name);
                                                estatus = NhlFATAL;

					}
				}
			}

void CallVARVAL_COORD_OP(void) {
				NclStackEntry *var = NULL,cvar;
                                NclStackEntry data;
                                NclSymbol* thesym = NULL;
                                char *coord_name = NULL;
                                int nsubs = 0;
                                NclSelectionRecord *sel_ptr = NULL;
                                NhlErrorTypes ret = NhlNOERROR;
				NclMultiDValData thevalue = NULL;

				
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;

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
						sel_ptr = _NclGetVarSelRec(var->u.data_var);
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec.name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							estatus = NhlWARNING;
						}
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret = _NclBuildVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_SINGLE:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
							sel_ptr = NULL;
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
						if(ret < NhlWARNING)
							estatus = NhlFATAL;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts used on coordinate variable reference",nsubs);
						_NclCleanUpStack(nsubs);
						estatus = NhlFATAL;
					}
					if(estatus != NhlFATAL) {
						data.u.data_obj = _NclVarValueRead(_NclReadCoordVar(var->u.data_var,coord_name,NULL),sel_ptr,NULL);
						if(data.u.data_obj != NULL) {
							data.kind = NclStk_VAL;
							estatus = _NclPush(data);
						} else {
							estatus = NhlFATAL;
						}
					} 
				}
			}

void CallVAR_COORD_OP(void) {
				NclStackEntry *var = NULL,cvar;
                                NclStackEntry data;
                                NclSymbol* thesym = NULL;
                                char *coord_name = NULL;
                                int nsubs = 0;
                                NclSelectionRecord *sel_ptr = NULL;
                                NhlErrorTypes ret = NhlNOERROR;
				NclMultiDValData thevalue = NULL;

				
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;

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
						sel_ptr = _NclGetVarSelRec(var->u.data_var);
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec.name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							estatus = NhlWARNING;
						}
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret = _NclBuildVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_SINGLE:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
							sel_ptr = NULL;
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
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

void CallREASSIGN_VAR_COORD_OP(void) {
				NclStackEntry *var = NULL,cvar;
				NclStackEntry data;
				NclSymbol* thesym = NULL;
				char *coord_name = NULL;
				int nsubs = 0;
				NhlErrorTypes ret = NhlNOERROR;
				NclSelectionRecord *sel_ptr = NULL;
				NclMultiDValData thevalue = NULL;
				int id;
				
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}

				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;

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
						sel_ptr = _NclGetVarSelRec(var->u.data_var);
						sel_ptr->n_entries = 1;
						data =_NclPop();
						if(data.u.sub_rec.name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
							estatus = NhlWARNING;
						}
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret = _NclBuildVSelection(var->u.data_var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/							
							ret = _NclBuildRSelection(var->u.data_var,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_SINGLE:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
							NclFree(sel_ptr);
							sel_ptr = NULL;
							estatus = NhlFATAL;
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
						if(ret < NhlWARNING)
							estatus = NhlFATAL;

					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts on left hand side of assignment",nsubs);
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
							id = thevalue->obj.id;
							ret = _NclReplaceCoordVar(var->u.data_var,thevalue,coord_name,sel_ptr);
							if(data.kind == NclStk_VAR) {
								_NclAttCopyWrite(_NclReadCoordVar(var->u.data_var,coord_name,NULL),data.u.data_var);
							}
							if(ret<estatus){
								estatus = ret;
							}
						} else {
							id = -1;
							estatus = NhlFATAL;
						}
/* _NclWriteCoordVar destroys non-permanent input so the following is not needed
* Rather than fix _NclWriteCoordVar to not free it was just easier to 
* comment the followign out.
*/
						switch(data.kind) {
						case NclStk_VAL: 
							if( (_NclGetObj(id)!= NULL)&&(data.u.data_obj->obj.status != PERMANENT))
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

void CallASSIGN_FILE_VAR_OP(void) {
/*
* Changed to a two operand function 1/30
*/
				NclSymbol *file_sym;
				NclQuark var = NrmNULLQUARK;
				NclStackEntry *file_ptr = NULL;
				NclStackEntry rhs;
				NclStackEntry data;
				NclStackEntry fvar;
				NclMultiDValData thevalue;
				int nsubs = 0;
				NclFile file = NULL;
				NclSelectionRecord* sel_ptr = NULL;
				int i, index;
				NclMultiDValData rhs_md = NULL,value = NULL; 


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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				nsubs = *(int*)ptr;
				file_ptr = _NclRetrieveRec(file_sym,WRITE_IT);
				if((file_ptr != NULL)&&(file_ptr->kind == NclStk_VAR)&&(estatus != NhlFATAL)) {
					value = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(value->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						if(value != NULL)
							file = (NclFile)_NclGetObj((int)*(obj*)value->multidval.val);
						if((file != NULL)&&((index = _NclFileIsVar(file,var)) != -1)) {
							int ndims = 0;

#ifdef USE_NETCDF4_FEATURES
							if(file->file.advanced_file_structure)
							{
								NclAdvancedFile advancedfile = (NclAdvancedFile) file;
								NclFileVarNode *varnode;
								varnode = _getVarNodeFromNclFileGrpNode(advancedfile->advancedfile.grpnode, var);
								ndims = varnode->dim_rec->n_dims;
							}
							else
#endif
								ndims = file->file.var_info[index]->num_dimensions;

							if((nsubs != ndims) && (nsubs != 0)){
									NhlPError(NhlFATAL,NhlEUNKNOWN,
										"Number of subscripts (%d) and number of dimensions (%d) do not match for variable (%s->%s)",
										nsubs,
										ndims,
										file_sym->name,
										NrmQuarkToString(var));
										estatus = NhlFATAL;
										_NclCleanUpStack(nsubs +1);
							} 
							if(estatus != NhlFATAL)  {
								if((nsubs != 0)&&(nsubs == ndims))  {
									sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
									sel_ptr->n_entries = nsubs;
									for(i = 0 ; i < nsubs; i++) {
										data = _NclPop();
										switch(data.u.sub_rec.sub_type) {
										case INT_VECT:
											estatus = _NclBuildFileVSelection(file,var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
											break;
										case INT_SINGLE:
										case INT_RANGE:
											estatus = _NclBuildFileRSelection(file,var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
											break;
										case COORD_VECT:
											estatus = _NclBuildFileCoordVSelection(file,var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
											break;
										case COORD_SINGLE:
										case COORD_RANGE:
											estatus = _NclBuildFileCoordRSelection(file,var,&data.u.sub_rec.u.range,&(sel_ptr->selection[nsubs - i - 1]),nsubs - i - 1,data.u.sub_rec.name);
											break;
										}
										_NclFreeSubRec(&data.u.sub_rec);
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
									if(sel_ptr != NULL) {
										for(i = 0; i <  sel_ptr->n_entries; i++) { 
											if(sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR){
												NclFree(sel_ptr->selection[i].u.vec.ind);
											}
										}
										NclFree(sel_ptr);
									}
								}
							}
						} else if(file!=NULL){
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
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs +1);
				}
			}

void CallFILE_VARVAL_OP(void) {
/*
* Changed to a two operand function 1/31/96
*/
				NclSymbol *dfile = NULL;
				NclQuark var = NrmNULLQUARK;
				int nsubs;
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
				kind = *(int*)ptr;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				var = *(NclQuark*)ptr;
				ptr++;lptr++;fptr++;
*/
				nsubs = *(int*)ptr;
				file_ptr =  _NclRetrieveRec(dfile,READ_IT);
				if((file_ptr != NULL)&&(file_ptr->kind == NclStk_VAR)&&(estatus != NhlFATAL)) {
					value = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(value->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						if(value != NULL) 
							file = (NclFile)_NclGetObj((int)*(obj*)value->multidval.val);
						if((file != NULL)&&((index = _NclFileIsVar(file,var)) != -1)) {
							memset(dim_is_ref, 0, NCL_MAX_DIMENSIONS*sizeof(int));
							/*
#ifdef USE_NETCDF4_FEATURES
							if(1 == file->file.advanced_file_structure)
							{
								NclAdvancedFile advancedfile = (NclAdvancedFile)file;
								NclFileVarNode *varnode = NULL;
								varnode = _getVarNodeFromNclFileGrpNode(advancedfile->advancedfile.grpnode, var);
								if(NULL == varnode)
								{
									NHLPERROR((NhlFATAL,NhlEUNKNOWN,"variable (%s) is not in file (%s)",
										NrmQuarkToString(var),
										NrmQuarkToString(advancedfile->advancedfile.grpnode->path)));
								}

								if(NULL != varnode->dim_rec)
								{
									for (i = 0; i < varnode->dim_rec->n_dims; ++i)
										dim_is_ref[i] = 0;
								}
							}
							else
#endif
							{
								for(i = 0 ; i < file->file.var_info[index]->num_dimensions; i++) {
									dim_is_ref[i] = 0;
								}
							}
							*/

							if(nsubs==0)
							{
								sel_ptr = NULL;
							}
							else
							{
#ifdef USE_NETCDF4_FEATURES
								if(1 == file->file.advanced_file_structure)
								{
									sel_ptr = _NclAllocateAdvancedFileSelPointer(file, var, nsubs, &estatus);
								}
								else
#endif
								{
									if(nsubs == file->file.var_info[index]->num_dimensions)
									{
										sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
										sel_ptr->n_entries = nsubs;
									}
									else
									{
										NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimensions of variable, (%d) subscripts used, (%d) subscripts expected",nsubs,file->file.var_info[index]->num_dimensions);
										estatus = NhlFATAL;
										_NclCleanUpStack(nsubs);
									}
								}
							}

							if(estatus != NhlFATAL) {
								if(sel_ptr != NULL) {
									for(i=0;i<sel_ptr->n_entries;i++) {
										data =_NclPop();
										switch(data.u.sub_rec.sub_type) {
										case INT_VECT:
											ret = _NclBuildFileVSelection(file,var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
											break;
										case INT_SINGLE:
										case INT_RANGE:
											ret = _NclBuildFileRSelection(file,var,&data.u.sub_rec.u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
											break;
										case COORD_VECT:
											estatus = _NclBuildFileCoordVSelection(file,var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
											break;
										case COORD_SINGLE:
										case COORD_RANGE:
											estatus = _NclBuildFileCoordRSelection(file,var,&data.u.sub_rec.u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
											break;
										}
										_NclFreeSubRec(&data.u.sub_rec);
										if(ret < NhlWARNING) {
											estatus = NhlFATAL;
										}
										if(estatus < NhlWARNING) 
											break;
										if(!dim_is_ref[(sel_ptr->selection[sel_ptr->n_entries - i - 1]).dim_num]) {
											dim_is_ref[(sel_ptr->selection[sel_ptr->n_entries - i - 1]).dim_num] = 1;
										} else {
											NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in subscript # %d, dimension is referenced more that once",i);
											estatus = NhlFATAL;
										}
									}
								}
								if(estatus != NhlFATAL) {
									out_var.kind = NclStk_VAL;
									out_var.u.data_obj = _NclFileReadVarValue(file,var,sel_ptr);
									if(sel_ptr != NULL) {
										
										for(i = 0; i <  sel_ptr->n_entries; i++) { 
											if(sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR){
												NclFree(sel_ptr->selection[i].u.vec.ind);
											}
										}
										NclFree(sel_ptr);
									}
									if((estatus != NhlFATAL)&&(out_var.u.data_obj != NULL)) {
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
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				}
				
			}

void CallFILE_VAR_OP(void) {
/*
* Changed to a two operand function 1/31/96
*/
				NclSymbol *dfile = NULL;
				NclQuark var = NrmNULLQUARK;
				int nsubs;
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
				kind = *(int*)ptr;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				var = *(NclQuark*)ptr;
				ptr++;lptr++;fptr++;
*/
				nsubs = *(int*)ptr;
				file_ptr =  _NclRetrieveRec(dfile,READ_IT);
				if((file_ptr == NULL) || (file_ptr->kind != NclStk_VAR) || (estatus == NhlFATAL))
				{
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
					return;
				}

				value = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
				if(NULL == value) 
				{
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) does not reference a file",dfile->name);
					_NclCleanUpStack(nsubs);
					estatus = NhlFATAL;
					return;
				}

				if(value->obj.obj_type_mask & Ncl_MultiDValnclfileData)
					file = (NclFile)_NclGetObj((int)*(obj*)value->multidval.val);
				else
				{
					NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) not reference to a valid file",dfile->name);
					_NclCleanUpStack(nsubs);
					estatus = NhlFATAL;
					return;
				}

				if(NULL == file)
				{
					NhlPError(NhlFATAL,NhlEUNKNOWN,"file (%s) isn't defined",dfile->name);
					_NclCleanUpStack(nsubs);
					estatus = NhlFATAL;
					out_var.kind = NclStk_NOVAL;	
					out_var.u.data_obj = NULL;
					return;
				}

#ifdef USE_NETCDF4_FEATURES
				if(file->file.advanced_file_structure)
				{
					sel_ptr = _NclAllocateAdvancedFileSelPointer(file, var, nsubs, &estatus);
					if(NhlFATAL == estatus)
					{
						NHLPERROR((NhlFATAL,NhlEUNKNOWN,"variable (%s) is not in file (%s)",
							NrmQuarkToString(var),dfile->name));
						out_var.kind = NclStk_NOVAL;	
						out_var.u.data_obj = NULL;
						return;
					}

					memset(dim_is_ref, 0, NCL_MAX_DIMENSIONS * sizeof(int));
				}
				else
#endif
				{
					index = _NclFileIsVar(file,var);

					if(index < 0)
					{
						NHLPERROR((NhlFATAL,NhlEUNKNOWN,"variable (%s) is not in file (%s)",
							NrmQuarkToString(var),dfile->name));
						_NclCleanUpStack(nsubs);
						estatus = NhlFATAL;
						out_var.kind = NclStk_NOVAL;	
						out_var.u.data_obj = NULL;
						return;
					}

					for(i = 0 ; i < file->file.var_info[index]->num_dimensions; i++) {
						dim_is_ref[i] = 0;
					}

					if((nsubs != 0)&&(nsubs ==  file->file.var_info[index]->num_dimensions)){
						sel_ptr = (NclSelectionRecord*)NclMalloc (sizeof(NclSelectionRecord));
						sel_ptr->n_entries = nsubs;
					} else if(nsubs==0){
						sel_ptr = NULL;
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Number of subscripts do not match number of dimensions of variable, (%d) subscripts used, (%d) subscripts expected",nsubs,file->file.var_info[index]->num_dimensions);
						estatus = NhlFATAL;
						_NclCleanUpStack(nsubs);
						return;
					}
				}

				if(sel_ptr != NULL) {
					for(i=0;i<sel_ptr->n_entries;i++) {
						data =_NclPop();
						switch(data.u.sub_rec.sub_type) {
						case INT_VECT:
							ret = _NclBuildFileVSelection(file,var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
							break;
						case INT_SINGLE:
						case INT_RANGE:
							ret = _NclBuildFileRSelection(file,var,&data.u.sub_rec.u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
							break;
						case COORD_VECT:
							estatus = _NclBuildFileCoordVSelection(file,var,&data.u.sub_rec.u.vec,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
							break;
						case COORD_SINGLE:
						case COORD_RANGE:
							estatus = _NclBuildFileCoordRSelection(file,var,&data.u.sub_rec.u.range,&(sel_ptr->selection[sel_ptr->n_entries - i - 1]),sel_ptr->n_entries - i - 1,data.u.sub_rec.name);
							break;
						}
						_NclFreeSubRec(&data.u.sub_rec);
						if(ret < NhlWARNING) {
							estatus = NhlFATAL;
							return;
						}
						if(estatus < NhlWARNING) 
							break;
						if(!dim_is_ref[(sel_ptr->selection[sel_ptr->n_entries - i - 1]).dim_num]) {
							dim_is_ref[(sel_ptr->selection[sel_ptr->n_entries - i - 1]).dim_num] = 1;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Error in subscript # %d, dimension is referenced more that once",i);
							estatus = NhlFATAL;
							return;
						}
					}
				}

				if(estatus != NhlFATAL) {
					out_var.kind = NclStk_VAR;
					out_var.u.data_var = _NclFileReadVar(file,var,sel_ptr);
					if(sel_ptr != NULL) {
						for(i = 0; i <  sel_ptr->n_entries; i++) { 
							if(sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR){
								NclFree(sel_ptr->selection[i].u.vec.ind);
							}
						}
						NclFree(sel_ptr);
					}
					if((estatus != NhlFATAL)&&(out_var.u.data_var != NULL)) {
						estatus = _NclPush(out_var);
					} else 	{
						estatus = NhlFATAL;
					}
				}	
			}

void CallFILE_GROUP_OP(void) {
				NclSymbol *dfile = NULL;
				NclFile file = NULL;
				NclFile group = NULL;
				NclQuark group_name;
				NclStackEntry *file_ptr = NULL;
				NclStackEntry out_group;
				NclStackEntry gvar;
				NclMultiDValData value,thevalue;
				NclMultiDValData out_md = NULL;
				int *id = (int*)NclMalloc((unsigned)sizeof(int));
				ng_size_t dim_size = 1;

				gvar = _NclPop();

				switch(gvar.kind)
				{
					case NclStk_VAL: 
						thevalue = gvar.u.data_obj;
						break;
					case NclStk_VAR:
						thevalue = (NclMultiDValData) (gvar.u.data_var);
						break;
					default:
						thevalue = NULL;
						estatus = NhlFATAL;
						break;
				}

				estatus = NhlFATAL;
				out_group.kind = NclStk_NOVAL;	
				out_group.u.data_obj = NULL;

				if(thevalue == NULL)
				{
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Group names must be scalar string values can't continue");
					return;
				}

				group_name = *(NclQuark*)thevalue->multidval.val;
				
				if(gvar.u.data_obj->obj.status != PERMANENT)
				{
					_NclDestroyObj((NclObj)gvar.u.data_obj);
				}

				ptr++;lptr++;fptr++;
				dfile = (NclSymbol*)*ptr;

				file_ptr =  _NclRetrieveRec(dfile,READ_IT);
				if(file_ptr == NULL)
					return;

				value = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
				if(value == NULL)
					return;

				file = (NclFile)_NclGetObj((int)*(obj*)value->multidval.val);
				group = _NclCreateGroup(NULL,NULL,Ncl_File,0,TEMPORARY,file,group_name);

				if(group != NULL) {
					*id = group->obj.id;
					out_md = _NclMultiDValnclfileDataCreate(NULL,NULL,Ncl_MultiDValnclfileData,0,id,NULL,1,&dim_size,TEMPORARY,NULL);
					if(out_md != NULL) {
						out_group.kind = NclStk_VAL;
						out_group.u.data_obj = out_md;
						estatus = _NclPlaceReturn(out_group);
					} else {
						NclFree(id);
						_NclDestroyObj((NclObj)group);
					}
				} else {
					obj *tmp_obj = NULL; 
					tmp_obj =(obj*) NclMalloc(((NclTypeClass)nclTypeobjClass)->type_class.size);
					*tmp_obj = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
					out_md = _NclMultiDValnclfileDataCreate(
							NULL,
							NULL,
							Ncl_MultiDValnclfileData,
							0,
							(void*)tmp_obj,
							(void*)&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,
							1,
							&dim_size,
							TEMPORARY,
							NULL);
					if(out_md != NULL) {
						out_group.kind = NclStk_VAL;
						out_group.u.data_obj = out_md;
						estatus = _NclPlaceReturn(out_group);
						NclFree(id);
					} else {
						NclFree(id);
						_NclDestroyObj((NclObj)group);
					}
				}

				estatus = _NclPush(out_group);
			}

void CallASSIGN_VARATT_OP(void) {
				NclSymbol *thesym = NULL;
				char *attname = NULL;
				int nsubs;
				NhlErrorTypes ret = NhlNOERROR;
				NclStackEntry *var = NULL,avar;
				NclStackEntry value;
				NclMultiDValData value_md = NULL,thevalue = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry data1;
				
				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					attname = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);
/*
				ptr++;lptr++;fptr++;
				attname = NrmQuarkToString(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = (*(int*)ptr);
	
				var = _NclRetrieveRec(thesym,WRITE_IT);
				if(var->u.data_var != NULL) {
					if(nsubs == 1) {
						sel_ptr = _NclGetVarSelRec(var->u.data_var);
						sel_ptr->n_entries = 1;
						data1 =_NclPop();
						if(data1.u.sub_rec.name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
							estatus = NhlWARNING;
						}
						switch(data1.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret =_NclBuildVSelection(NULL,&data1.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/								
							ret =_NclBuildRSelection(NULL,&data1.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_SINGLE:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
							estatus = NhlFATAL;
							break;
						}
						 _NclFreeSubRec(&data1.u.sub_rec);
						if(ret < NhlWARNING) 
							estatus = NhlFATAL;
					} else if(nsubs != 0){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to subscript attribute with more than one dimension");
						estatus = NhlFATAL;
					}
					if(!(estatus < NhlINFO)) {
						int id;
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
						id = value_md->obj.id;
						ret = _NclWriteAtt(var->u.data_var,attname,value_md,sel_ptr);
						if((value.kind == NclStk_VAR)&&(value.u.data_var->obj.status != PERMANENT)) {
							 _NclDestroyObj((NclObj)value.u.data_var);
						} else if((value.kind == NclStk_VAL)&& _NclGetObj(id) && (value.u.data_obj->obj.status != PERMANENT)){
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

void CallREASSIGN_VARATT_OP(void) {
				NclSymbol *thesym = NULL;
				char *attname = NULL;
				int nsubs;
				NhlErrorTypes ret = NhlNOERROR;
				NclStackEntry *var = NULL,avar;
				NclStackEntry value;
				NclMultiDValData value_md = NULL,thevalue = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclStackEntry data1;
				
				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}

				if((thevalue == NULL) || ((thevalue->multidval.kind != SCALAR) &&
							  (thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					attname = NrmQuarkToString(*(NclQuark*)thevalue->multidval.val);
					if(PERMANENT != avar.u.data_obj->obj.status) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}

				ptr++;lptr++;fptr++;
				thesym = (NclSymbol*)(*ptr);

				ptr++;lptr++;fptr++;
				nsubs = (*(int*)ptr);
	
				var = _NclRetrieveRec(thesym,WRITE_IT);
				if(var->u.data_var != NULL) {
					if(nsubs == 1) {
						sel_ptr = _NclGetVarSelRec(var->u.data_var);
						sel_ptr->n_entries = 1;
						data1 =_NclPop();
						if(data1.u.sub_rec.name != NULL) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
							estatus = NhlWARNING;
						}
						switch(data1.u.sub_rec.sub_type) {
						case INT_VECT:
/*
* Need to free some stuff here
*/						
							ret =_NclBuildVSelection(NULL,&data1.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
							break;
						case INT_SINGLE:
						case INT_RANGE:
/*
* Need to free some stuff here
*/								
							ret =_NclBuildRSelection(NULL,&data1.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
							break;
						case COORD_VECT:
						case COORD_SINGLE:
						case COORD_RANGE:
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
							estatus = NhlFATAL;
							break;
						}
						 _NclFreeSubRec(&data1.u.sub_rec);
						if(ret < NhlWARNING) 
							estatus = NhlFATAL;
					} else if(nsubs != 0){
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to subscript attribute with more than one dimension");
						estatus = NhlFATAL;
					}
					if(!(estatus < NhlINFO)) {
						int id;
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
						id = value_md->obj.id;

						ret = _NclReplaceAtt(var->u.data_var,attname,value_md,sel_ptr);

						if((value.kind == NclStk_VAR)&&(value.u.data_var->obj.status != PERMANENT)) {
							 _NclDestroyObj((NclObj)value.u.data_var);
						} else if((value.kind == NclStk_VAL)&& _NclGetObj(id) && (value.u.data_obj->obj.status != PERMANENT)){
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

void CallASSIGN_FILEVAR_COORD_ATT_OP(void) {
				NclFile file;
				NclStackEntry *file_ptr,value,data1,fvar,avar,cvar;
				NclMultiDValData file_md;
				NclSymbol *file_sym;
				NclQuark coord_name = NrmNULLQUARK;
				/*NclQuark var_name = NrmNULLQUARK;*/
				NclQuark att_name = NrmNULLQUARK;
				int nsubs = 0;
				NclSelectionRecord *sel_ptr = NULL;
				NclMultiDValData value_md,thevalue;
				NhlErrorTypes ret = NhlNOERROR;
				

				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					att_name = *(NclQuark*)thevalue->multidval.val;
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				thevalue = NULL;
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = *(NclQuark*)thevalue->multidval.val;
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					/*var_name = *(NclQuark*)thevalue->multidval.val;*/
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)(*ptr);
/*
				ptr++;lptr++;fptr++;
				coord_name = (NclQuark)*ptr;
				ptr++;lptr++;fptr++;
				att_name = (NclQuark)*ptr;
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL) &&(file_ptr->u.data_var != NULL)) {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						file = (NclFile)_NclGetObj((int)*(obj*)file_md->multidval.val);
						if(file == NULL) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Undefined file reference");
							estatus = NhlFATAL;
						} else if(_NclFileVarIsCoord(file,coord_name) == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a coordinate variable, can not assign attribute",NrmQuarkToString(coord_name));
							estatus = NhlFATAL;
						} else if((_NclFileVarIsAtt(file,coord_name,att_name != -1))||(nsubs == 0)) {
							if(nsubs == 1) {
								sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
								sel_ptr->n_entries = 1;
								data1 =_NclPop();
								if(data1.u.sub_rec.name != NULL) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
									estatus = NhlWARNING;
								}
								switch(data1.u.sub_rec.sub_type) {
								case INT_VECT:
									ret =_NclBuildVSelection(NULL,&data1.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
								case INT_SINGLE:
								case INT_RANGE:
									ret =_NclBuildRSelection(NULL,&data1.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
									break;
								case COORD_VECT:
								case COORD_SINGLE:
								case COORD_RANGE:
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
									estatus = NhlFATAL;
									break;
								}
								_NclFreeSubRec(&data1.u.sub_rec);
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
									value_md = NULL;
                                                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to assign illegal type or value to variable attribute");
                                                                        estatus = NhlFATAL;
                                                                }
								if (estatus != NhlFATAL) {
									ret = _NclFileWriteVarAtt(file,coord_name,att_name,value_md,sel_ptr);
								} 
								estatus = MIN(ret,estatus);
                                                                if((value.kind == NclStk_VAR)&&(value.u.data_var->obj.status != PERMANENT)) {
                                                                        _NclDestroyObj((NclObj)value.u.data_var);
                                                                } else if((value.kind == NclStk_VAL)&&(value.u.data_obj->obj.status != PERMANENT)){
                                                                        _NclDestroyObj((NclObj)value.u.data_obj);
                                                                }
                                                                if(sel_ptr != NULL) {
                                                                        if(sel_ptr->selection[0].sel_type == Ncl_VECSUBSCR) {
                                                                                NclFree(sel_ptr->selection[0].u.vec.ind);
                                                                        }
                                                                        NclFree(sel_ptr);
                                                                }

							} else {
								if(sel_ptr !=  NULL) {
									NclFree(sel_ptr);
								}
								_NclCleanUpStack(1);
							 }
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to subscript undefined coordinate variable attribute");
                                                        estatus = NhlFATAL;

						} 
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference a file coordinate variable attribute from a non-file");
						estatus = NhlFATAL;
					}
				} else {
					_NclCleanUpStack(nsubs +1);
					estatus = NhlFATAL;
				}
			}

void CallASSIGN_FILEVARATT_OP(void) {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,data1,rhs,fvar,avar;
				NclMultiDValData file_md,rhs_md = NULL;
				NclSelectionRecord *sel_ptr = NULL;
				NclFile		file;
				NclQuark 	var = NrmNULLQUARK;
				NclQuark	att = NrmNULLQUARK;
				int nsubs;
				NhlErrorTypes ret = NhlNOERROR;
				NclMultiDValData thevalue;
				int id;

				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					att = *(NclQuark*)thevalue->multidval.val;
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				thevalue = NULL;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				ptr++;lptr++;fptr++;
				att = (NclQuark)(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;
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
								if(data1.u.sub_rec.name != NULL) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
									estatus = NhlWARNING;
								}
								switch(data1.u.sub_rec.sub_type) {
								case INT_VECT:
/*
* Need to free some stuff here
*/						
									ret =_NclBuildVSelection(NULL,&data1.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
									break;
								case INT_SINGLE:
								case INT_RANGE:
/*
* Need to free some stuff here
*/								
									ret =_NclBuildRSelection(NULL,&data1.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
									break;
								case COORD_VECT:
								case COORD_SINGLE:
								case COORD_RANGE:
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
									estatus = NhlFATAL;
									break;
								}
								_NclFreeSubRec(&data1.u.sub_rec);
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
								rhs_md = NULL;
								estatus = NhlFATAL;
							}
	
							if(estatus != NhlFATAL) {
/*
 * if the att is "virtual", the rhs_md gets destroyed. Therefore capture the obj id now so we can checke whether it
 * exists after this call.
 */
								id = rhs_md->obj.id;
								estatus = _NclFileWriteVarAtt(file,var,att,rhs_md,NULL);
							}
							if(estatus != NhlFATAL) {
								if(_NclGetObj(id) && rhs_md->obj.status != PERMANENT) {
									_NclDestroyObj((NclObj)rhs_md);
								}
							}
							if(sel_ptr != NULL) {
								if(sel_ptr->selection[0].sel_type == Ncl_VECSUBSCR ) {
									NclFree(sel_ptr->selection[0].u.vec.ind);
								}
								NclFree(sel_ptr);
							}
							
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Error writing file variable attribute, either thefile or the variable (%s) are undefined",NrmQuarkToString(var));
							estatus = NhlFATAL;
						}
					}
				} else {
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				}
			}

void CallASSIGN_FILEVAR_COORD_OP(void) {
				NclFile file;
				NclStackEntry *file_ptr,rhs_data,data,fvar,cvar;
				NclMultiDValData file_md;
				NclSymbol *file_sym;
				NclQuark coord_name;
				NclQuark var_name = NrmNULLQUARK;
				int nsubs;
				NclSelectionRecord *sel_ptr = NULL;
				NclMultiDValData rhs_md,thevalue;
				NclAtt theatt;
				NclAttList *step;
				NhlErrorTypes ret = NhlNOERROR;
	
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = *(NclQuark*)thevalue->multidval.val;
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				coord_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
*/
				nsubs = (*(int*)ptr);
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
								if(data.u.sub_rec.name != NULL) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
									estatus = NhlWARNING;
								}
								switch(data.u.sub_rec.sub_type) {
								case INT_VECT:
/*
* Need to free some stuff here
*/						
									estatus = _NclBuildFileVSelection(file,coord_name,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
									break;
								case INT_SINGLE:
								case INT_RANGE:
/*
* Need to free some stuff here
*/							
									estatus = _NclBuildFileRSelection(file,coord_name,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
									break;
								case COORD_VECT:
								case COORD_SINGLE:
								case COORD_RANGE:
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
									NclFree(sel_ptr);
									sel_ptr = NULL;
									estatus = NhlFATAL;
									break;
								}
								_NclFreeSubRec(&data.u.sub_rec);
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
									if(sel_ptr != NULL) {
										if(sel_ptr->selection[0].sel_type == Ncl_VECSUBSCR) {
											NclFree(sel_ptr->selection[0].u.vec.ind);
										}
										NclFree(sel_ptr);
									}
								}
							}
						} else {
							estatus = NhlFATAL;
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Dimension (%s) does not exist in file (%s), can not assign coordinate variable",NrmQuarkToString(coord_name),NrmQuarkToString(file->file.fname));
						}
					}
				} else {
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs +1);
				}
			}

void CallPARAM_FILEVAR_COORD_ATT_OP(void) {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,fvar,avar,cvar;
				NclMultiDValData file_md,thevalue = NULL;
				NclFile	file;
				NclQuark coord_name = NrmNULLQUARK,att_name = NrmNULLQUARK; /*var_name = NrmNULLQUARK;*/
				int nsubs = 0;
				NclSelectionRecord* sel_ptr = NULL;
				NclStackEntry out_data;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;
			
				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					att_name = *(NclQuark*)thevalue->multidval.val;
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				thevalue = NULL;
				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = *(NclQuark*)thevalue->multidval.val;
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					/*var_name = *(NclQuark*)thevalue->multidval.val;*/
					if(fvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)fvar.u.data_obj);
					}
				}
				ptr++;lptr++;fptr++;
				file_sym = (NclSymbol*)*ptr;
/*
				ptr++;lptr++;fptr++;
				coord_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
				att_name = (NclQuark)(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;
	
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL)&&(file_ptr->u.data_var != NULL))  {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						file = (NclFile)_NclGetObj(*((int*)file_md->multidval.val));
						if(file == NULL) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Undefined file reference");
							estatus = NhlFATAL;
						} else if(_NclFileVarIsCoord(file,coord_name) == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"(%s) is not a coordinate variable, can not read attribute",NrmQuarkToString(coord_name));

							estatus = NhlFATAL;
						} else if(_NclFileVarIsAtt(file,coord_name,att_name) == -1) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference undefined coordinate variable attribute");
							estatus = NhlFATAL;
						} else {
							if(nsubs == 1) {
								sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
								sel_ptr->n_entries = 1;
								data =_NclPop();
								if(data.u.sub_rec.name != NULL) {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
									estatus = NhlWARNING;
								}
								switch(data.u.sub_rec.sub_type) {
								case INT_VECT:
	/*
	* Need to free some stuff here
	*/						
									ret = _NclBuildVSelection(NULL,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
									break;
								case INT_SINGLE:
								case INT_RANGE:
	/*
	* Need to free some stuff here
	*/								
									ret = _NclBuildRSelection(NULL,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
									break;
								case COORD_VECT:
								case COORD_SINGLE:
								case COORD_RANGE:
									NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
									estatus = NhlFATAL;
									break;
								}
								_NclFreeSubRec(&data.u.sub_rec);
								if(ret < NhlWARNING)
									estatus = ret;
							} else if(nsubs != 0) {
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Attributes only have one dimension, %d subscripts used",nsubs);		
								estatus = NhlFATAL;
							}
							out_data.u.data_obj = _NclFileReadVarAtt(file,coord_name,att_name,sel_ptr);
							if(sel_ptr != NULL) {
								if(sel_ptr->selection[0].sel_type == Ncl_VECSUBSCR) {
									NclFree(sel_ptr->selection[0].u.vec.ind);
								}
								NclFree(sel_ptr);
							}
							if(out_data.u.data_obj != NULL) {
								out_data.kind = NclStk_VAL;
								estatus = _NclPush(out_data);
							} else {
								estatus = NhlFATAL;
							}
						}	
					} else {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Attempt to reference a file variable attribute from a non-file");
						estatus = NhlFATAL;
					}
				} else {
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				}
			}

void CallPARAM_FILEVARATT_OP(void) {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,fvar,avar;
				NclMultiDValData file_md,thevalue;
				NclFile	file;
				NclQuark var_name = NrmNULLQUARK,att_name = NrmNULLQUARK;
				int nsubs = 0;
				NclSelectionRecord* sel_ptr = NULL;
				NclStackEntry out_data;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;
			
				avar = _NclPop();
				switch(avar.kind) {
				case NclStk_VAL: 
					thevalue = avar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(avar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"File Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					att_name = *(NclQuark*)thevalue->multidval.val;
					if(avar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)avar.u.data_obj);
					}
				}
				thevalue = NULL;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				ptr++;lptr++;fptr++;
				att_name = (NclQuark)(*ptr);
*/
				ptr++;lptr++;fptr++;
				nsubs = *(int*)ptr;
	
				file_ptr = _NclRetrieveRec(file_sym,READ_IT);
				if((estatus != NhlFATAL)&&(file_ptr != NULL)&&(file_ptr->u.data_var != NULL))  {
					file_md = _NclVarValueRead(file_ptr->u.data_var,NULL,NULL);
					if(file_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
						file = (NclFile)_NclGetObj(*((int*)file_md->multidval.val));
						if(nsubs == 1) {
							sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
							sel_ptr->n_entries = 1;
							data =_NclPop();
							if(data.u.sub_rec.name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with variable attributes");
								estatus = NhlWARNING;
							}
							switch(data.u.sub_rec.sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/						
								ret = _NclBuildVSelection(NULL,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
							case INT_SINGLE:
							case INT_RANGE:
/*
* Need to free some stuff here
*/								
								ret = _NclBuildRSelection(NULL,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
								break;
							case COORD_VECT:
							case COORD_SINGLE:
							case COORD_RANGE:
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with variable attributes");
								estatus = NhlFATAL;
								break;
							}
							_NclFreeSubRec(&data.u.sub_rec);
							if(ret < NhlWARNING)
								estatus = ret;
						} else if(nsubs != 0) {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Attributes only have one dimension, %d subscripts used",nsubs);		
							estatus = NhlFATAL;
						}
						out_data.u.data_obj = _NclFileReadVarAtt(file,var_name,att_name,sel_ptr);
						if(sel_ptr != NULL) {
							if(sel_ptr->selection[0].sel_type == Ncl_VECSUBSCR) {
								NclFree(sel_ptr->selection[0].u.vec.ind);
							}
							NclFree(sel_ptr);
						}
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
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				}
			}

void CallPARAM_FILEVAR_COORD_OP(void) {
				NclSymbol *file_sym;
				NclStackEntry *file_ptr,fvar,cvar;
				NclMultiDValData file_md,thevalue;
				NclFile	file;
				NclQuark var_name = NrmNULLQUARK,coord_name = NrmNULLQUARK;
				int nsubs = 0;
				NclSelectionRecord* sel_ptr = NULL;
				NclStackEntry out_data;
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;


				cvar = _NclPop();
				switch(cvar.kind) {
				case NclStk_VAL: 
					thevalue = cvar.u.data_obj;
					break;
				case NclStk_VAR:
					thevalue = _NclVarValueRead(cvar.u.data_var,NULL,NULL);
					break;
				default:
					thevalue = NULL;
					estatus = NhlFATAL;
					break;
				}
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Variable Attribute names must be scalar string values can't continue");
					estatus = NhlFATAL;
				} else {
					coord_name = *(NclQuark*)thevalue->multidval.val;
					if(cvar.u.data_obj->obj.status != PERMANENT) {
						_NclDestroyObj((NclObj)cvar.u.data_obj);
					}
				}
				thevalue = NULL;
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
				if((thevalue == NULL)||((thevalue->multidval.kind != SCALAR)&&(thevalue->multidval.type != (NclTypeClass)nclTypestringClass))) {
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
				coord_name = (NclQuark)(*ptr);
				ptr++;lptr++;fptr++;
*/
				nsubs = (*(int*)ptr);
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
							if(data.u.sub_rec.name != NULL) {
								NhlPError(NhlWARNING,NhlEUNKNOWN,"Named dimensions can not be used with coordinate variables since only one dimension applies");
								estatus = NhlWARNING;
							}
							switch(data.u.sub_rec.sub_type) {
							case INT_VECT:
/*
* Need to free some stuff here
*/						
								ret = _NclBuildFileVSelection(file,var_name,&data.u.sub_rec.u.vec,&(sel_ptr->selection[0]),0,NULL);
								break;
							case INT_SINGLE:
							case INT_RANGE:
/*
* Need to free some stuff here
*/							
								ret = _NclBuildFileRSelection(file,var_name,&data.u.sub_rec.u.range,&(sel_ptr->selection[0]),0,NULL);
								break;
							case COORD_VECT:
							case COORD_SINGLE:
							case COORD_RANGE:
								NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate indexing can not be used with coordinate variables ");
								NclFree(sel_ptr);
								sel_ptr = NULL;
								estatus = NhlFATAL;
								break;
							}
							_NclFreeSubRec(&data.u.sub_rec);
							if(ret < NhlWARNING)
								estatus = NhlFATAL;
						} else {
							NhlPError(NhlFATAL,NhlEUNKNOWN,"Coordinate variables have only one dimension, %d subscripts used on coordinate variable reference",nsubs);
							_NclCleanUpStack(nsubs);
							estatus = NhlFATAL;
						}
						out_data.u.data_var =_NclFileReadCoord (file, coord_name,sel_ptr);
						if(sel_ptr != NULL) {
							if(sel_ptr->selection[0].sel_type == Ncl_VECSUBSCR) {
								NclFree(sel_ptr->selection[0].u.vec.ind);
							}	
							NclFree(sel_ptr);
						}
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
					estatus = NhlFATAL;
					_NclCleanUpStack(nsubs);
				}
			}

void performASSIGN_VAR_VAR_OP(NclStackEntry *lhs_var, NclStackEntry *rhs_var,
                              int lhs_nsubs, int rhs_nsubs,
                              NclSymbol *lhs_sym, NclSymbol *rhs_sym)
{
    NhlErrorTypes ret = NhlNOERROR;
    ng_size_t i;
    NclStackEntry data;
    NclSelectionRecord *lhs_sel_ptr = NULL;
    NclSelectionRecord *rhs_sel_ptr = NULL;
    NclSelectionRecord rhs_sel;
    struct _NclVarRec *tmp_var;
    NclMultiDValData tmp_md;
    NhlArgVal udata;

    if((rhs_var == NULL)||(rhs_var->kind == NclStk_NOVAL))
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,"%s is undefined",rhs_sym->name));
        estatus = NhlFATAL;
    }

    /*if((estatus!=NhlFATAL)&&(lhs_var != NULL)&&(lhs_var->kind == NclStk_NOVAL))*/
    if(((estatus!=NhlFATAL)&&(lhs_var != NULL)&&(lhs_var->kind == NclStk_NOVAL)) || _ItIsNclReassign)
    {
        if(lhs_nsubs != 0)
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                      "%s is undefined, can not subscript an undefined variable",
                       lhs_sym->name));
            estatus = NhlFATAL;
            _NclCleanUpStack(lhs_nsubs);
        }
        else if(rhs_nsubs == 0)
        {
            lhs_var->kind = NclStk_VAR;
            lhs_var->u.data_var = _NclCopyVar(rhs_var->u.data_var,lhs_sym,NULL);
            _NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT);    
            lhs_var->u.data_var->var.thesym = lhs_sym;
            (void)_NclChangeSymbolType(lhs_sym,VAR);
            lhs_var->u.data_var->var.var_type = NORMAL;
        }
        else if((rhs_nsubs != 0)&&(rhs_nsubs == rhs_var->u.data_var->var.n_dims))
        {
            /*
             * This branch is where wholesale assigment of rhs to lhs occurs. including coords,atts and values
             */
            rhs_sel_ptr = &rhs_sel;
            rhs_sel_ptr->n_entries = rhs_nsubs;
            for(i=0;i<rhs_nsubs;i++)
            {
                data =_NclPop();
                switch(data.u.sub_rec.sub_type)
                {
                    case INT_VECT:
                         /*
                         * Need to free some stuff here
                         */                
                         ret = _NclBuildVSelection(rhs_var->u.data_var,&data.u.sub_rec.u.vec,
                                                   &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                   rhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case INT_SINGLE:
                    case INT_RANGE:
                         /*
                         * Need to free some stuff here
                         */                    
                         ret = _NclBuildRSelection(rhs_var->u.data_var,
                                                   &data.u.sub_rec.u.range,
                                                   &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                   rhs_nsubs - i - 1,
                                                   data.u.sub_rec.name);
                         break;
                    case COORD_VECT:
                         ret = _NclBuildCoordVSelection(rhs_var->u.data_var,
                                                        &data.u.sub_rec.u.vec,
                                                        &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                        rhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case COORD_SINGLE:
                    case COORD_RANGE:
                        ret = _NclBuildCoordRSelection(rhs_var->u.data_var,
                                                        &data.u.sub_rec.u.range,
                                                        &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                        rhs_nsubs - i - 1,data.u.sub_rec.name);
                        break;
                }

                _NclFreeSubRec(&data.u.sub_rec);
                if(ret < NhlWARNING)
                {
                    estatus = NhlFATAL;
                    break;
                }
            } 

            if(estatus != NhlFATAL)
            {
                lhs_var->kind = NclStk_VAR;
                lhs_var->u.data_var = _NclVarRead(rhs_var->u.data_var,rhs_sel_ptr);
                if(lhs_var->u.data_var == NULL)
                {
                    estatus = NhlFATAL;
                    lhs_var->kind = NclStk_NOVAL;
                }
                else
                {
                    if(!_NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT))
                    {    
                        tmp_var = lhs_var->u.data_var;
                        lhs_var->u.data_var = _NclCopyVar(lhs_var->u.data_var,NULL,NULL);
                        _NclSetStatus((NclObj)lhs_var->u.data_var,PERMANENT);    
                        if(lhs_var->u.data_var->obj.status != PERMANENT)
                        {
                            _NclDestroyObj((NclObj)tmp_var);
                        }
                        lhs_var->u.data_var->var.var_quark = NrmStringToQuark(lhs_sym->name);
                        lhs_var->u.data_var->var.thesym = lhs_sym;
                        (void)_NclChangeSymbolType(lhs_sym,VAR);
                        lhs_var->u.data_var->var.var_type = NORMAL;
                        _NclCallCallBacks((NclObj)lhs_var->u.data_var,CREATED);
                    }
                    else
                    {
                        /*
                        * ----> May want to encapsulate the following into the NclVar object
                        *     A likely function interface would be: _NclChangeVar(int quark,NclSymbol *thesym, NclVarTypes var_type); 
                        *     which would be a method.
                        */

                        lhs_var->u.data_var->var.var_quark = NrmStringToQuark(lhs_sym->name);
                        lhs_var->u.data_var->var.thesym = lhs_sym;
                        (void)_NclChangeSymbolType(lhs_sym,VAR);
                        lhs_var->u.data_var->var.var_type = NORMAL;
                        _NclCallCallBacks((NclObj)lhs_var->u.data_var,CREATED);

                        if(lhs_var->u.data_var->obj.obj_type_mask & Ncl_HLUVar)
                        {
                            udata.ptrval = NclMalloc(sizeof(NclHLUUData));
                            ((NclHLUUData*)udata.ptrval)->vq = lhs_var->u.data_var->var.var_quark;
                            ((NclHLUUData*)udata.ptrval)->aq = -1;
                            tmp_md = (NclMultiDValData)_NclGetObj(lhs_var->u.data_var->var.thevalue_id);
                            ((NclHLUVar)lhs_var->u.data_var)->hvar.cb = _NclAddCallback((NclObj)tmp_md,NULL,_NclHLUVarValChange,HLUVALCHANGE,&udata);
                            ((NclHLUVar)lhs_var->u.data_var)->hvar.udata = udata.ptrval;
                            for(i = 0; i < tmp_md->multidval.totalelements;i++)
                            {
                                if(lhs_var->u.data_var->var.thesym != NULL)
                                {
                                    _NclAddHLURef(((obj*)tmp_md->multidval.val)[i],
                                                  lhs_var->u.data_var->var.var_quark,-1,i,
                                                  lhs_var->u.data_var->var.thesym->level);
                                }
                                else
                                {
                                    _NclAddHLURef(((obj*)tmp_md->multidval.val)[i],lhs_var->u.data_var->var.var_quark,-1,i,-1);
                                }
                            }
                        }
                    }
                }

                if(rhs_sel_ptr != NULL)
                {
                    for(i = 0; i <  rhs_sel_ptr->n_entries; i++)
                    {
                        if(rhs_sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR)
                        {
                            NclFree(rhs_sel_ptr->selection[i].u.vec.ind);
                            rhs_sel_ptr->selection[i].u.vec.ind = NULL;
                        }
                    }
                }
                /*
                *-----> end of questionable code
                */
            }
        }
        else
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                      "Number of subscripts on right-hand-side do not match\n\t\t\t%s: (%d), %s: (%d)\n",
                      "number of dimensions of variable", rhs_nsubs, "Subscripts used", rhs_var->u.data_var->var.n_dims));
            estatus = NhlFATAL;
            _NclCleanUpStack(rhs_nsubs);
            
        }
    }
    else if((estatus !=NhlFATAL)&&(lhs_var->kind == NclStk_VAR)&&(lhs_var->u.data_var != NULL))
    {
       /*
       * When the target variable is already defined just normal assignment occurs if it is not subscripted
       * if it is then the _NclAssignVarToVar is used which is different then the normal assignment provided
       * by the ASSIGN_VAR_OP operator.
       */

        if(rhs_nsubs == 0)
        {
            rhs_sel_ptr = NULL;
        }
        else if((estatus != NhlFATAL)&&(rhs_nsubs != rhs_var->u.data_var->var.n_dims))
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                      "Number of subscripts on right-hand-side do not match\n\t\t\t%s: (%d), %s: (%d)\n",
                      "number of dimensions of variable", rhs_nsubs, "Subscripts used", rhs_var->u.data_var->var.n_dims));
            estatus = NhlFATAL;
            _NclCleanUpStack(rhs_nsubs);
        }
        else
        {
            rhs_sel_ptr = &rhs_sel;
            rhs_sel_ptr->n_entries = rhs_nsubs;
    
            for(i=0;i<rhs_nsubs;i++)
            {
                data =_NclPop();
                switch(data.u.sub_rec.sub_type)
                {
                    case INT_VECT:
                         /*
                         * Need to free some stuff here
                         */                
                         ret = _NclBuildVSelection(rhs_var->u.data_var,
                                                   &data.u.sub_rec.u.vec,
                                                   &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                   rhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case INT_SINGLE:
                    case INT_RANGE:
                         /*
                         * Need to free some stuff here
                         */                    
                         ret = _NclBuildRSelection(rhs_var->u.data_var,
                                                   &data.u.sub_rec.u.range,
                                                   &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                   rhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case COORD_VECT:
                         ret = _NclBuildCoordVSelection(rhs_var->u.data_var,
                                                   &data.u.sub_rec.u.vec,
                                                   &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                   rhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case COORD_SINGLE:
                    case COORD_RANGE:
                         ret = _NclBuildCoordRSelection(rhs_var->u.data_var,
                                                   &data.u.sub_rec.u.range,
                                                   &(rhs_sel_ptr->selection[rhs_nsubs - i - 1]),
                                                   rhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                }

                _NclFreeSubRec(&data.u.sub_rec);

                if(ret < NhlWARNING)
                {
                    estatus = NhlFATAL;
                    break;
                }
            } 
        } 

        if((lhs_nsubs ==0)&&(estatus != NhlFATAL))
        {
            lhs_sel_ptr = NULL;
        }
        else if((estatus != NhlFATAL)&&(lhs_nsubs != lhs_var->u.data_var->var.n_dims))
        {
            NHLPERROR((NhlFATAL,NhlEUNKNOWN,
                      "Number of subscripts on left-hand-side do not match\n\t\t\t%s: (%d),  %s: (%d)\n",
                      "number of dimensions of variable", lhs_nsubs, "Subscripts used", lhs_var->u.data_var->var.n_dims));
            estatus = NhlFATAL;
            _NclCleanUpStack(lhs_nsubs);
        }
        else if (estatus != NhlFATAL)
        {
            lhs_sel_ptr = _NclGetVarSelRec(lhs_var->u.data_var); 
            lhs_sel_ptr->n_entries = lhs_nsubs;

            for(i=0;i<lhs_nsubs;i++)
            {
                data =_NclPop();
                switch(data.u.sub_rec.sub_type)
                {
                    case INT_VECT:
                         /*
                         * Need to free some stuff here
                         */                
                         ret = _NclBuildVSelection(lhs_var->u.data_var,
                                                   &data.u.sub_rec.u.vec,
                                                   &(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),
                                                   lhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case INT_SINGLE:
                    case INT_RANGE:
                         /*
                         * Need to free some stuff here
                         */                        
                         ret = _NclBuildRSelection(lhs_var->u.data_var,
                                                   &data.u.sub_rec.u.range,
                                                   &(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),
                                                   lhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case COORD_VECT:
                         ret = _NclBuildCoordVSelection(lhs_var->u.data_var,
                                                   &data.u.sub_rec.u.vec,
                                                   &(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),
                                                   lhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                    case COORD_SINGLE:
                    case COORD_RANGE:
                         ret = _NclBuildCoordRSelection(lhs_var->u.data_var,
                                                   &data.u.sub_rec.u.range,
                                                   &(lhs_sel_ptr->selection[lhs_nsubs - i - 1]),
                                                   lhs_nsubs - i - 1,data.u.sub_rec.name);
                         break;
                }

                _NclFreeSubRec(&data.u.sub_rec);

                if(ret < NhlWARNING)
                {
                    estatus = NhlFATAL;
                    break;
                }
            } 
        } 

        if(estatus != NhlFATAL)
        {
            ret = _NclAssignVarToVar(lhs_var->u.data_var,lhs_sel_ptr,rhs_var->u.data_var,rhs_sel_ptr);
            if(ret < NhlINFO)
            {
                estatus = ret;
            }
        } 

        if(rhs_sel_ptr != NULL)
        {
            for(i = 0; i <  rhs_sel_ptr->n_entries; i++)
            {
                if(rhs_sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR)
                {
                    NclFree(rhs_sel_ptr->selection[i].u.vec.ind);
                    rhs_sel_ptr->selection[i].u.vec.ind = NULL;
                }
            }
        }
    }
    else
    {
        estatus = NhlFATAL;
        _NclCleanUpStack(rhs_nsubs);
        _NclCleanUpStack(lhs_nsubs);
    }
}

void CallASSIGN_VAR_VAR_OP(void)
{
    int lhs_nsubs=0;
    int rhs_nsubs=0;
    NclSymbol *lhs_sym = NULL;
    NclSymbol *rhs_sym = NULL;
    NclStackEntry *lhs_var = NULL;
    NclStackEntry *rhs_var = NULL;

    ptr++;lptr++;fptr++;
    rhs_sym = (NclSymbol*)*ptr;
    rhs_var = _NclRetrieveRec(rhs_sym,READ_IT);

    ptr++;lptr++;fptr++;
    rhs_nsubs = *(int*)ptr;

    ptr++;lptr++;fptr++;
    lhs_sym = (NclSymbol*)*ptr;
    lhs_var = _NclRetrieveRec(lhs_sym,WRITE_IT);

    ptr++;lptr++;fptr++;
    lhs_nsubs = *(int*)ptr;

    performASSIGN_VAR_VAR_OP(lhs_var, rhs_var, lhs_nsubs, rhs_nsubs,
                             lhs_sym, rhs_sym);
}

void CallREASSIGN_VAR_VAR_OP(void)
{
    int lhs_nsubs=0;
    int rhs_nsubs=0;
    NclSymbol *lhs_sym = NULL;
    NclSymbol *rhs_sym = NULL;
    NclStackEntry *lhs_var = NULL;
    NclStackEntry *rhs_var = NULL;

    ptr++;lptr++;fptr++;
    rhs_sym = (NclSymbol*)*ptr;
    rhs_var = _NclRetrieveRec(rhs_sym,READ_IT);

    ptr++;lptr++;fptr++;
    rhs_nsubs = *(int*)ptr;

    ptr++;lptr++;fptr++;
    lhs_sym = (NclSymbol*)*ptr;
    lhs_var = _NclRetrieveRec(lhs_sym,WRITE_IT);

    ptr++;lptr++;fptr++;
    lhs_nsubs = *(int*)ptr;

    if(0 == strcmp(lhs_sym->name, rhs_sym->name))
    {
        _ItIsNclReassign = 1;

        performASSIGN_VAR_VAR_OP(lhs_var, rhs_var, lhs_nsubs, rhs_nsubs,
                                   lhs_sym, rhs_sym);
        _ItIsNclReassign = 0;
    }
    else
    {
        ClearDataBeforeReassign(lhs_var);

        performASSIGN_VAR_VAR_OP(lhs_var, rhs_var, lhs_nsubs, rhs_nsubs,
                                 lhs_sym, rhs_sym);
    }
}

void CallPUSHNULL(void) {
				NclStackEntry data;

				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				estatus = _NclPush(data);
			}




NclExecuteReturnStatus _NclExecute
#if	NhlNeedProto
(unsigned long start_offset)
#else 
(start_offset) 
	unsigned long start_offset;
#endif
{
	int cline;
	char *cfile = NULL;
	NclQuark cfileq = -1, nxt_fileq;

	estatus = NhlNOERROR;
	machine = _NclGetCurrentMachine();
	ptr = machine + start_offset;
	lptr = _NclGetCurrentLineRec() + start_offset;
	fptr = _NclGetCurrentFileNameRec() + start_offset;
	level++;

	cline = *lptr;
	if(fptr){
		/* FIXME: We currently don't profile cmd lines */
		cfile = *fptr;
		cfileq = NrmStringToQuark(cfile);
		NCL_PROF_LENTER(cfile, cline);	
	}
	
	while(1) {
		switch(*ptr) {
/****************************
* Zero Operand Instructions *
****************************/
			case LIST_ASSIGN_VERIFY_SUB: {
				CallLIST_ASSIGN_VERIFY_SUB();
			}
			break;
			case STOPSEQ:
				if(cfile){
					NCL_PROF_LEXIT(cfile, cline);	
				}
				level--;
				return(Ncl_STOPS);
			case ENDSTMNT_OP:
			case NOOP :
				break;
			case NAMED_INT_SUBSCRIPT_OP :
			case INT_SUBSCRIPT_OP : {
				CallINT_SUBSCRIPT_OP();
			}
			break;
			case DEFAULT_RANGE_OP : {
				CallDEFAULT_RANGE_OP();
			}
			break;
			case RANGE_INDEX_OP : {
				CallRANGE_INDEX_OP();
			}
			break;
			case SINGLE_INDEX_OP : {
				CallSINGLE_INDEX_OP();
			}
			break;
			case CRETURN_OP : 
			{
				if(cfile){
					NCL_PROF_LEXIT(cfile, cline);	
				}
				level--;
				return(Ncl_STOPS);
			}
			case RETURN_OP : 
			{
				NclStackEntry data;
				NhlErrorTypes ret = NhlNOERROR;
				data = _NclPop();
				
				ret = _NclPlaceReturn(data);
				if(ret< NhlWARNING) {
					estatus = NhlFATAL;
				} else {
					if(cfile){
						NCL_PROF_LEXIT(cfile, cline);	
					}
					level--;
					return(Ncl_STOPS);
				}
			}
			break;
			case NAMED_COORD_SUBSCRIPT_OP : 
			case COORD_SUBSCRIPT_OP : {
				CallCOORD_SUBSCRIPT_OP();
			} 
			break;
			case NEG_OP : {
				CallNEG_OP();
			}
			break;
			case NOT_OP : {
				CallNOT_OP();
			}
			break;
			case MOD_OP : {
				CallMOD_OP();
			}
			break;
			case OR_OP : {
				CallOR_OP();
			}
			break;
			case AND_OP : {
				CallAND_OP();
			}
			break;
			case XOR_OP : {
				CallXOR_OP();
			}
			break;
			case LTSEL_OP : {
				CallLTSEL_OP();
			}
			break;
			case GTSEL_OP : {
				CallGTSEL_OP();
			}
			break;
			case PLUS_OP : {
				CallPLUS_OP();
			}
				break;
			case MINUS_OP : {
				CallMINUS_OP();
			}
				break;
			case MUL_OP : {
				CallMUL_OP();
			}
				break;
			case MAT_OP : {
				CallMAT_OP();
			}
				break;
			case DIV_OP : {
				CallDIV_OP();
			}
				break;
			case EXP_OP : {
				CallEXP_OP();
			}
			break;
			case LE_OP : {
				CallLE_OP();
			}
			break;
			case GE_OP : {
				CallGE_OP();
			}
			break;
			case GT_OP : {
				CallGT_OP();
			}
			break;
			case LT_OP : {
				CallLT_OP();
			}
			break;
			case EQ_OP : {
				CallEQ_OP();
			}
			break;
			case NE_OP : {
				CallNE_OP();
			}
			break;
			case GET_OBJ_OP : {
				CallGET_OBJ_OP();
			}
			break;
/***************************
* One Operand Instructions *
***************************/
			case LIST_CLEAR_TMP_OP: {
				CallLIST_CLEAR_TMP_OP();
			}
			break;
			case TERM_LIST_OP : {
				CallTERM_LIST_OP();
			}
			break;
			case FUNC_CALL_OP : {
				CallFUNC_CALL_OP();
			}
			break;
			case FPDEF: 
				CallFPDEF();
				break;
			case JMP : {
				CallJMP();
			}
			break;
			case ARRAY_LIT_OP : {
				CallARRAY_LIT_OP();
			}
			break;
			case LISTVAR_LIT_OP : {
				CallLISTVAR_LIT_OP();
			}
			break;
			case PUSH_REAL_LIT_OP : 
			case PUSH_LOGICAL_LIT_OP: 
			case PUSH_INT_LIT_OP :
			case PUSH_STRING_LIT_OP : {
				CallPUSH_STRING_LIT_OP();
			}
			break;
			case JMP_SCALAR_TRUE_OP : {
				CallJMP_SCALAR_TRUE_OP();
			}
			break;
			case JMP_SCALAR_FALSE_OP : {
				CallJMP_SCALAR_FALSE_OP();
			}
			break;
			case JMPFALSE : {
				CallJMPFALSE();
			}
			break;
			case SET_OBJ_OP : {
				CallSET_OBJ_OP();
			}
				break;
			case PROC_CALL_OP : {
				CallPROC_CALL_OP();
			}
				break;
			case INTRINSIC_FUNC_CALL : {
				CallINTRINSIC_FUNC_CALL();
			}
				break;
			case INTRINSIC_PROC_CALL : {
				CallINTRINSIC_PROC_CALL();
			}
				break;
			case DUP_TOFS : {
				CallDUP_TOFS();
			}
			break;
			case LOOP_VALIDATE_OP : {
				CallLOOP_VALIDATE_OP();
			}
				break;
			case LOOP_INC_OP : {
				CallLOOP_INC_OP();
			}
				break;
			case PARAM_VAR_DIM_OP:
			case VAR_DIM_OP : {
				CallVAR_DIM_OP();
			}
			break;
			case ASSIGN_VAR_DIM_OP : {
				CallASSIGN_VAR_DIM_OP();
			}
			break;
			case NEW_WM_OP:
			case NEW_OP : {	
				CallNEW_OP();
			}
			break;
			case ISDEFINED_OP : {
				CallISDEFINED_OP();
			}
			break;
/***************************
* Two Operand Instructions *
***************************/			
			case LIST_READ_OP: {
				CallLIST_READ_OP();
			}
			break;
			case LIST_READ_FILEVAR_OP: {
				CallLIST_READ_FILEVAR_OP();
			}
			break;
			case VARVAL_READ_OP : {
				CallVARVAL_READ_OP();
			}
			break;
			case PARAM_VAR_OP:
			case VAR_READ_OP : {
				CallVAR_READ_OP();
			}
			break;
			case ASSIGN_VAR_OP : {
				CallASSIGN_VAR_OP();
			}
			break;
			case REASSIGN_VAR_OP : {
				CallREASSIGN_VAR_OP();
			}
			break;
			case NEW_FRAME_OP : {
				CallNEW_FRAME_OP();
			}
			break;
			case CONVERT_TO_LOCAL : {
				CallCONVERT_TO_LOCAL();
			}
			break;
			case ASSIGN_FILEVAR_DIM_OP : {
				CallASSIGN_FILEVAR_DIM_OP();
			}
			break;
			case FILEVAR_DIM_OP:	
			case PARAM_FILEVAR_DIM_OP : {
				CallPARAM_FILEVAR_DIM_OP();
			}
			break;
			case CREATE_OBJ_WP_OP : 
			case CREATE_OBJ_OP : {
				CallCREATE_OBJ_OP();
			}
			break;
/*****************************
* Three Operand Instructions *
*****************************/
			case SET_NEXT_OP: {
				CallSET_NEXT_OP();
			}	
			break;
			case PARAM_VARATT_OP:
			case VARATT_OP : {
				CallVARATT_OP();
			}
			break;
			case PARAM_VAR_COORD_ATT_OP:
			case VAR_COORD_ATT_OP : {
				CallVAR_COORD_ATT_OP();
			}
			break;
			case ASSIGN_VAR_COORD_OP : {
				CallASSIGN_VAR_COORD_OP();
			}
			break;
			case ASSIGN_VAR_COORD_ATT_OP : {
				CallASSIGN_VAR_COORD_ATT_OP();
			}
			break;
			case VARVAL_COORD_OP : {
				CallVARVAL_COORD_OP();
			}
			break;
			case PARAM_VAR_COORD_OP:
			case VAR_COORD_OP : {
				CallVAR_COORD_OP();
			}
			break;
			case REASSIGN_VAR_COORD_OP : {
				CallREASSIGN_VAR_COORD_OP();
			}
			break;
			case ASSIGN_FILE_VAR_OP : {
				CallASSIGN_FILE_VAR_OP();
			}
			break;
			case FILE_VARVAL_OP : {
				CallFILE_VARVAL_OP();
			}
			break;
			case PARAM_FILE_VAR_OP:
			case FILE_VAR_OP : {
				CallFILE_VAR_OP();
			}
			break;
			case ASSIGN_VARATT_OP : {
				CallASSIGN_VARATT_OP();
			}
			break;
			case REASSIGN_VARATT_OP : {
				CallREASSIGN_VARATT_OP();
			}
			break;
/*****************************
* Four Operand Instructions  *
*****************************/
			case ASSIGN_FILEVAR_COORD_ATT_OP : {
				CallASSIGN_FILEVAR_COORD_ATT_OP();
			}
			break;
			case ASSIGN_FILEVARATT_OP : {
				CallASSIGN_FILEVARATT_OP();
			}
			break;
			case ASSIGN_FILEVAR_COORD_OP : {
				CallASSIGN_FILEVAR_COORD_OP();
			}
			break;
			case FILEVAR_COORD_ATT_OP: 
			case PARAM_FILEVAR_COORD_ATT_OP : {
				CallPARAM_FILEVAR_COORD_ATT_OP();
			}
			break;
			case FILEVARATT_OP:
			case PARAM_FILEVARATT_OP : {
				CallPARAM_FILEVARATT_OP();
			}
			break;
			case FILEVARVAL_COORD_OP:
			case FILEVAR_COORD_OP:
			case PARAM_FILEVAR_COORD_OP : {
				CallPARAM_FILEVAR_COORD_OP();
			}
			break;
			case ASSIGN_VAR_VAR_OP : {
				CallASSIGN_VAR_VAR_OP();
			}
			break;
			case REASSIGN_VAR_VAR_OP : {
				CallREASSIGN_VAR_VAR_OP();
			}
			break;
			case PUSHNULL : {
				CallPUSHNULL();
			}
			break;
			case FILE_GROUP_OP :
				{
					CallFILE_GROUP_OP();
				}
				break;
			case FILE_GROUPVAL_OP :
				/*We do not suport group operator (other than read group/variable yet).
				{
					CallFILE_GROUPVAL_OP();
				}
				*/
				fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
				fprintf(stderr, "\tExecute: Error occurred at or near line %d\n",(cmd_line ? (*lptr): *lptr));
				fprintf(stderr, "\tSTOP at FILE_GROUPVAL_OP: %d\n", FILE_GROUPVAL_OP);
				fprintf(stderr, "\tWe are not surpose to reach here.\n");
				fprintf(stderr, "\tThe reason could be that the group we are trying to access have special characters.\n");
				fprintf(stderr, "\tCheck the script, maybe try to change the script to something like:\n");
				fprintf(stderr, "\tgn = \"the-group-name\"\n");
				fprintf(stderr, "\tg = f=>$gn$\n\n");
				NHLPERROR((estatus,NhlEUNKNOWN,"Execute: Error occurred at or near line %d\n",(cmd_line ? (*lptr): *lptr)));
				break;
			case PARAM_FILE_GROUP_OP:
				CallFILE_GROUP_OP();
				break;
			default:
			      /*
			       *fprintf(stderr, "\nfile: %s, line: %d\n", __FILE__, __LINE__)
			       *fprintf(stderr, "\tUNKNOWN Operator *ptr: %ld\n", (long)*ptr);
			       */
				break;
		}
		if(estatus < NhlINFO) {
			if(*fptr == NULL) {
				NHLPERROR((estatus,NhlEUNKNOWN,"Execute: Error occurred at or near line %d\n",(cmd_line ? (*lptr): *lptr)));
			} else {
				NHLPERROR((estatus,NhlEUNKNOWN,"Execute: Error occurred at or near line %d in file %s\n", *lptr, *fptr));
			}
			if(estatus < NhlWARNING) {
/*
* need to clean up stack !!! for current level
*/
/*
				if(level > 1) {
					_NclAbortFrame();
				} else {
*/
				if(!(level>1)) {
					_NclCleanUpStack(-1);
				}
				level--;
				return(Ncl_ERRORS);
			}
		}	
		estatus = NhlNOERROR;	
		ptr++;lptr++;fptr++;

		if (*fptr == cfile) {
			nxt_fileq = cfileq;
		}
		else {
			nxt_fileq = (*fptr) ? (NrmStringToQuark(*fptr)) : -1;
		}
		if(cfile && ((cline != *lptr) || (cfileq != nxt_fileq))){
			NCL_PROF_LEXIT(cfile, cline);	
			cline = *lptr;
			cfile = *fptr;
			if(cfile){
				cfileq = NrmStringToQuark(cfile);
				NCL_PROF_LENTER(cfile, cline);	
			}
		}
	}
}

#ifdef __cplusplus
}
#endif

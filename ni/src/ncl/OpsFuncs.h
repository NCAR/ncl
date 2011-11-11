
/*
 *      $Id: OpsFuncs.h,v 1.10 2010-04-14 21:29:48 huangwei Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		OpsFuncs.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Oct 14 15:35:00 MDT 1993
 *
 *	Description:	Contains function declarations for operator 
 *			source. included in the Execute source file.
 */



NhlErrorTypes _NclDualOp(
#if	NhlNeedProto
NclStackEntry /*lhs*/,
NclStackEntry /*rhs*/,
NclStackEntry */*result*/,
int	/* operation */
#endif
);

NhlErrorTypes _NclMonoOp(
#if	NhlNeedProto
NclStackEntry /*operand*/,
NclStackEntry * /*result */,
int  /* operation*/
#endif
);

NhlErrorTypes _NclBuildConcatArray(
#if	NhlNeedProto
int	/*n_items*/,
NclStackEntry */*result*/
#endif
);
NhlErrorTypes _NclBuildArray(
#if	NhlNeedProto
int	/*n_items*/,
NclStackEntry */*result*/
#endif
);
NhlErrorTypes _NclBuildListVar(
#if	NhlNeedProto
int	/*n_items*/,
NclStackEntry */*result*/
#endif
);

NhlErrorTypes _NclBuildNewListVar(int n_items,
                                  NclStackEntry *result);

NhlErrorTypes _NclFuncCallOp(
#if	NhlNeedProto
NclSymbol * /*func*/,
int /*caller_level*/
#endif
);
NhlErrorTypes _NclProcCallOp(
#if	NhlNeedProto
NclSymbol *  /* proc*/,
int /*caller_level*/
#endif
);

NclStackEntry _NclCreateHLUObjOp(
#if	NhlNeedProto
int /*nres*/,
char * /*the_hlu_obj*/,
NclSymbol * /*the_hlu_obj_class*/,
NclMultiDValData /*the_hlu_obj_parent*/
#endif
);
NclStackEntry _NclGetHLUObjOp(
#if	NhlNeedProto
NclMultiDValData /*the_hlu_data_obj*/,
NclQuark /*res_name*/
#endif
);

NhlErrorTypes _NclSetHLUObjOp(
#if	NhlNeedProto
NclMultiDValData /*the_hlu_data_obj*/,
int /*nres*/
#endif
);

NhlErrorTypes _NclNewOp(
#if	NhlNeedProto
NclSymbol*  /*data_type*/,
NclStackEntry /*size_expr*/,
NclStackEntry /*missing_expr*/
#endif
);


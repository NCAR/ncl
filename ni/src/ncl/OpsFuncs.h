
/*
 *      $Id: OpsFuncs.h,v 1.6 1994-05-28 00:13:01 ethan Exp $
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
#ifdef NhlNeedProto
NclStackEntry /*lhs*/,
NclStackEntry /*rhs*/,
NclStackEntry */*result*/,
int	/* operation */
#endif
);

NhlErrorTypes _NclMonoOp(
#ifdef NhlNeedProto
NclStackEntry /*operand*/,
NclStackEntry * /*result */,
int  /* operation*/
#endif
);

NhlErrorTypes _NclBuildArray(
#ifdef NhlNeedProto
int	/*n_items*/,
NclStackEntry */*result*/
#endif
);

NhlErrorTypes _NclFuncCallOp(
#ifdef NhlNeedProto
NclSymbol * /*func*/,
int /*caller_level*/
#endif
);
NhlErrorTypes _NclProcCallOp(
#ifdef NhlNeedProto
NclSymbol *  /* proc*/,
int /*caller_level*/
#endif
);

NclStackEntry _NclCreateHLUObjOp(
#ifdef NhlNeedProto
int /*nres*/,
char * /*the_hlu_obj*/,
NclSymbol * /*the_hlu_obj_class*/,
NclMultiDValData /*the_hlu_obj_parent*/
#endif
);


NhlErrorTypes _NclSetHLUObjOp(
#ifdef NhlNeedProto
NclMultiDValData /*the_hlu_data_obj*/,
int /*nres*/
#endif
);

NhlErrorTypes _NclNewOp(
#ifdef NhlNeedProto
NclSymbol*  /*data_type*/,
NclStackEntry /*size_expr*/,
NclStackEntry /*missing_expr*/
#endif
);


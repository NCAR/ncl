
/*
 *      $Id: OpsFuncs.h,v 1.5 1994-05-06 23:37:24 ethan Exp $
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
NclSymbol * /*func*/
#endif
);
NhlErrorTypes _NclProcCallOp(
#ifdef NhlNeedProto
NclSymbol *  /* proc*/
#endif
);

NhlErrorTypes _NclCreateHLUObjOp(
#ifdef NhlNeedProto
int /*nres*/,
NclSymbol * /*the_hlu_obj*/,
NclSymbol * /*the_hlu_obj_class*/,
NclMultiDValData /*the_hlu_obj_parent*/
#endif
);



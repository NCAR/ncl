
/*
 *      $Id: OpsFuncs.h,v 1.2 1993-12-30 00:44:31 ethan Exp $
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




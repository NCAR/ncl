
/*
 *      $Id: TypeSupportOpTemplate.h.sed,v 1.1 1995-01-28 01:53:15 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jan 27 18:30:21 MST 1995
 *
 *	Description:	
 */
extern NhlErrorTypes _NclTFUNC(
#if	NhlNeedProto
NclTypeClass /* the_type */,
void * /* result */,
void* /* lhs */,
void* /* rhs */,
NclScalar* /* lhs_m */,
NclScalar* /* rhs_m */,
ng_size_t /* nlhs */,
ng_size_t /* nrhs */
#endif
);

NclTypeClass _NclTFUNC_type(
#if	NhlNeedProto
NclTypeClass /*the_type*/
#endif
);



/*
 *      $Id: TypeSupportMatTemplate.h.sed,v 1.1.4.1 2008-03-28 20:37:53 grubin Exp $
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
int       /* nlhs_dims */,
ng_size_t* /* lhs_dimsizes */,
int       /* nrhs_dims */,
ng_size_t* /* rhs_dimsizes */
#endif
);

NclTypeClass _NclTFUNC_type(
#if	NhlNeedProto
NclTypeClass /*the_type*/
#endif
);


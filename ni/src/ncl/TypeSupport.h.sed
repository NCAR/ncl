
/*
 *      $Id: TypeSupport.h.sed,v 1.1 1995-01-28 01:53:07 ethan Exp $
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
 *	Date:		Fri Jan 27 18:29:58 MST 1995
 *
 *	Description:	
 */
#ifndef TypeSupport_h
#define TypeSupport_h

INSERTHERE

extern NhlErrorTypes _NclInitTypeClasses(
#if	NhlNeedProto
void
#endif
);

extern NclTypeClass _NclTypeEnumToTypeClass(
#if	NhlNeedProto
NclObjTypes /* obj_type_enum */
#endif
);

extern NhlErrorTypes _Nclcmpf(
#if	NhlNeedProto
NclTypeClass	/* the_type */,
void *		/* lhs */,
void *		/* rhs */,
NclScalar *	/* lhs_m */,
NclScalar * 	/* rhs_m */,
int		/* digits */,
double*		/* result */
#endif
);

extern void _Nclprint(
#if	NhlNeedProto
NclTypeClass /* the_type */,
FILE 	* /*fp*/,
void * /*val*/
#endif
);

extern NhlErrorTypes _Nclcoerce(
#if	NhlNeedProto
NclTypeClass /* to_type */,
void * /* result */,
void* /* from */,
int /* n */,
NclScalar* /* from_m */,
NclScalar* /* to_m */,
NclTypeClass  /*from_type*/
#endif
);

extern NhlErrorTypes _Nclreset_mis(
#if	NhlNeedProto
NclTypeClass /* the_type*/,
void * /*val*/,
NclScalar* /*old_m*/,
NclScalar* /*new_m*/,
int /*nval*/
#endif
);

extern NclMonoTypes _Nclis_mono(
#if	NhlNeedProto
NclTypeClass /* the_type */,
void * /* val */,
NclScalar* /* val_m */,
int /* nval */
#endif
);

#endif /* TypeSupport_h*/

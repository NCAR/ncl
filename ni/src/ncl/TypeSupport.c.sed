
/*
 *      $Id: TypeSupport.c.sed,v 1.7 2010-04-14 21:29:48 huangwei Exp $
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
 *	Date:		Fri Jan 27 18:29:40 MST 1995
 *
 *	Description:	
 */
#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#include "defs.h"
#include "NclType.h"
#include "TypeSupport.h"
#include "NclMdInc.h"
#include "NclTypelist.h"
#include <ctype.h>

INSERTHERE

NhlErrorTypes _NclInitTypeClasses
#if	NhlNeedProto
(void)
#else
()
#endif
{
	_NclInitClass(nclTypedoubleClass);
	_NclInitClass(nclTypefloatClass);
	_NclInitClass(nclTypelongClass);
	_NclInitClass(nclTypeintClass);
	_NclInitClass(nclTypeshortClass);
	_NclInitClass(nclTypelogicalClass);
	_NclInitClass(nclTypebyteClass);
	_NclInitClass(nclTypecharClass);
	_NclInitClass(nclTypestringClass);
	_NclInitClass(nclTypeobjClass);
	_NclInitClass(nclTypeint64Class);
	_NclInitClass(nclTypeushortClass);
	_NclInitClass(nclTypeuintClass);
	_NclInitClass(nclTypeulongClass);
	_NclInitClass(nclTypeuint64Class);
	_NclInitClass(nclTypeubyteClass);
	_NclInitClass(nclTypelistClass);
	return(NhlNOERROR);
}

NhlErrorTypes _Nclcmpf
#if	NhlNeedProto
(NclTypeClass the_type, void *lhs, void* rhs, NclScalar* lhs_m, NclScalar* rhs_m,int digits,double *result)
#else
(the_type,lhs,rhs,lhs_m,rhs_m,digits,result)
NclTypeClass the_type;
void *lhs;
void* rhs;
NclScalar* lhs_m;
NclScalar* rhs_m;
int digits;
double *result;
#endif
{
	NclTypeClass tmp;

	if(the_type->type_class.cmpf!= NULL) {
		return((*(the_type->type_class.cmpf))(lhs,rhs,lhs_m,rhs_m,digits,result));
	} else {
		tmp = (NclTypeClass)the_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.cmpf != NULL) {
				return((*(tmp->type_class.cmpf))(lhs,rhs,lhs_m,rhs_m,digits,result));
			} else {
				tmp = (NclTypeClass)tmp->obj_class.super_class;
			}
		}
	}
	return NhlFATAL;
}

NclTypeClass _NclTypeEnumToTypeClass
#if	NhlNeedProto
(NclObjTypes obj_type_enum)
#else
(obj_type_enum)
NclObjTypes obj_type_enum;
#endif
{
	switch(obj_type_enum) {
	case Ncl_Typedouble:
		return((NclTypeClass)nclTypedoubleClass);
	case Ncl_Typefloat:
		return((NclTypeClass)nclTypefloatClass);
	case Ncl_Typelong:
		return((NclTypeClass)nclTypelongClass);
	case Ncl_Typeint:
		return((NclTypeClass)nclTypeintClass);
	case Ncl_Typeshort:
		return((NclTypeClass)nclTypeshortClass);
	case Ncl_Typebyte:
		return((NclTypeClass)nclTypebyteClass);
	case Ncl_Typestring:
		return((NclTypeClass)nclTypestringClass);
	case Ncl_Typechar:
		return((NclTypeClass)nclTypecharClass);
	case Ncl_Typelogical:
		return((NclTypeClass)nclTypelogicalClass);
	case Ncl_Typeint64:
		return((NclTypeClass)nclTypeint64Class);
	case Ncl_Typeushort:
		return((NclTypeClass)nclTypeushortClass);
	case Ncl_Typeuint:
		return((NclTypeClass)nclTypeuintClass);
	case Ncl_Typeulong:
		return((NclTypeClass)nclTypeulongClass);
	case Ncl_Typeuint64:
		return((NclTypeClass)nclTypeuint64Class);
	case Ncl_Typeubyte:
		return((NclTypeClass)nclTypeubyteClass);
	case Ncl_Typeobj:
		return((NclTypeClass)nclTypeobjClass);
	case Ncl_Typelist:
		return((NclTypeClass)nclTypelistClass);
	default:
		return((NclTypeClass)nclTypeClass);
	}
}

NhlErrorTypes _Nclprint
#if	NhlNeedProto
(NclTypeClass the_type,FILE *fp,void* val)
#else
(the_type,fp,val)
NclTypeClass the_type;
FILE *fp;
void* val;
#endif
{
	NclTypeClass tmp;

	if(the_type->type_class.print != NULL) {
		return((*(the_type->type_class.print))(fp,val));
	} else {
		tmp = (NclTypeClass)the_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.print!= NULL) {
				return((*(tmp->type_class.print))(fp,val));
			} else {
				tmp = (NclTypeClass)tmp->obj_class.super_class;
			}
		}
	}
	return NhlFATAL;

}

NhlErrorTypes _Nclcoerce
#if	NhlNeedProto
(NclTypeClass to_type, void * result, void* from, ng_size_t n, NclScalar* from_m, NclScalar* to_m, NclTypeClass from_type)
#else
(t0_type, result, from, n, from_m, to_m, from_type)
NclTypeClass to_type;
void * result;
void* from;
ng_size_t n;
NclScalar* from_m;
NclScalar* to_m;
NclTypeClass from_type;
#endif
{
	NclTypeClass tmp;

	if(to_type->type_class.coerce != NULL) {
		return((*(to_type->type_class.coerce))(result,from,n,from_m,to_m,from_type));
	} else {
		tmp = (NclTypeClass)to_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.coerce != NULL) {
				return((*(tmp->type_class.coerce))(result,from,n,from_m,to_m,from_type));
			} else {
				tmp =(NclTypeClass) tmp->obj_class.super_class;
			}
		}
		return(NhlFATAL);
	}
}

NhlErrorTypes _Nclreset_mis
#if	NhlNeedProto
(NclTypeClass the_type,void *val, NclScalar* old_m, NclScalar * new_m,ng_size_t nval)
#else
(the_type,val, old_m, new_m,nval)
NclTypeClass the_type;
void *val;
NclScalar* old_m;
NclScalar* new_m;
ng_size_t nval;
#endif
{
	NclTypeClass tmp;
	if(the_type->type_class.reset_mis != NULL) {
		return((*(the_type->type_class.reset_mis))(val,old_m,new_m,nval));
	} else {
		tmp = (NclTypeClass)the_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.reset_mis != NULL) {
				return((*(tmp->type_class.reset_mis))(val,old_m,new_m,nval));
			} else {
				tmp = (NclTypeClass)tmp->obj_class.super_class;
			}
		}
		return(NhlFATAL);
	}
}

NclMonoTypes _Nclis_mono
#if	NhlNeedProto
(NclTypeClass the_type, void *val, NclScalar* val_m, ng_size_t nval)
#else
(the_type, val, val_m, nval)
NclTypeClass the_type;
void *val;
NclScalar* val_m;
ng_size_t nval;
#endif
{
	NclTypeClass tmp;
	if(the_type->type_class.is_mono != NULL) {
		return((*(the_type->type_class.is_mono))(val, val_m, nval));
	} else {
		tmp = (NclTypeClass)the_type->obj_class.super_class;
		while(tmp != (NclTypeClass)nclTypeClass) {
			if(tmp->type_class.is_mono != NULL) {
				return((*(tmp->type_class.is_mono))(val, val_m, nval));
			} else {
				tmp = (NclTypeClass)tmp->obj_class.super_class;
			}
		}
		return(NhlFATAL);
	}

}

NclQuark _NclGetLower
(
	NclQuark qstr
	)
{
	char *buf;
	char buffer[256];
	char *cp;
	char *instr = NrmQuarkToString(qstr);
	int size;
	NrmQuark outq;
	
	if (! instr) {
	        return NrmNULLQUARK;
        }
        size  = strlen(instr);
	if (size < 256) {
		buf = buffer;
	}
	else {
		buf = NclMalloc(size + 1);
	}
	strncpy(buf,instr,size);
	buf[size] = '\0';
	for (cp = buf; *cp != '\0'; cp++) {
		*cp = tolower(*cp);
	}
	outq = NrmStringToQuark(buffer);

	if (buf != buffer)
		NclFree(buf);

	return outq;
}

NhlErrorTypes _NclSetDefaultFillValues
(
	int default_type
)
{
	if (default_type == NCL_5_DEFAULT_FILLVALUES) {
		((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval = -99;
		((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval = -999;
		((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval = -9999;
		((NclTypeClass)nclTypeint64Class)->type_class.default_mis.int64val = -99999999;
		((NclTypeClass)nclTypeushortClass)->type_class.default_mis.ushortval = 0;
		((NclTypeClass)nclTypeuintClass)->type_class.default_mis.uintval = 0;
		((NclTypeClass)nclTypeulongClass)->type_class.default_mis.ulongval = 0;
		((NclTypeClass)nclTypeuint64Class)->type_class.default_mis.uint64val = 0;
		((NclTypeClass)nclTypeubyteClass)->type_class.default_mis.ubyteval = 0;
		((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval = -999.0;
		((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval = -9999.0;
		((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval = 0;
		((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval = (char) 0xff;
		((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval = NrmStringToQuark("missing");
		((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval = -1;
	}
	else if (default_type == NCL_6_DEFAULT_FILLVALUES) {
		((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval = -32767;
		((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval = -2147483647;
		((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval = -2147483647;
		((NclTypeClass)nclTypeint64Class)->type_class.default_mis.int64val = (long long)-9223372036854775806LL;
		((NclTypeClass)nclTypeushortClass)->type_class.default_mis.ushortval = 65535;
		((NclTypeClass)nclTypeuintClass)->type_class.default_mis.uintval = 4294967295U;
		((NclTypeClass)nclTypeulongClass)->type_class.default_mis.ulongval = 4294967295U;
		((NclTypeClass)nclTypeuint64Class)->type_class.default_mis.uint64val = (unsigned long long)18446744073709551614ULL;
		((NclTypeClass)nclTypeubyteClass)->type_class.default_mis.ubyteval = 255;
		((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval = 9.9692099683868690e+36f;
		((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval = 9.9692099683868690e+36;
		((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval = 0;
		((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval = -127;
		((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval = NrmStringToQuark("missing");
		((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval = -1;
	}
	return NhlNOERROR;
}

NhlErrorTypes _NclGetDefaultFillValue
(NclBasicDataTypes type,
 NclScalar *def_val)
{
	char *ctype = NULL;
	NclTypeClass class = NULL;

	ctype = _NclBasicDataTypeToName(type);
	if (ctype) {
		class = _NclNameToTypeClass(NrmStringToQuark(ctype));
		if (class) {
				*def_val = class->type_class.default_mis;
				return NhlNOERROR;
                }
        }
	return NhlFATAL;
}
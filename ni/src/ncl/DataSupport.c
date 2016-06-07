/*
 *      $Id$
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
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
 *	Date:		Thu Jan 13 14:52:04 MST 1994
 *
 *	Description:	
 */

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include "defs.h"
#include "Symbol.h"
#include "NclMdInc.h"
#include "NclTypelist.h"
#include "OpsList.h"
#include "DataSupport.h"
#ifndef NIO_LIB_ONLY
#include "parser.h"
#endif


typedef struct _ClassPointerList {
	NclObjTypes obj_type;
	NclObjClass obj_class;	
	struct _ClassPointerList *next;
}ClassPointerList;

ClassPointerList *clist = NULL;

void _NclRegisterClassPointer
#if NhlNeedProto
(NclObjTypes obj_type, NclObjClass obj_class)
#else
(obj_type, obj_class)
NclObjTypes obj_type;
NclObjClass obj_class;
#endif
{
	ClassPointerList *tmp;

	tmp = NclMalloc(sizeof(ClassPointerList));

	tmp->obj_type = obj_type;
	tmp->obj_class = obj_class;
	if(clist == NULL) {
		tmp->next = NULL;
		clist = tmp;
	} else {
		tmp->next = clist;
		clist = tmp;
	}
	return;
}

NclObjClass _NclObjTypeToPointer
#if NhlNeedProto
(NclObjTypes obj_type)
#else
(obj_type)
NclObjTypes obj_type;
#endif
{
	ClassPointerList *step;

	step = clist;

	while(step != NULL) {
		if(step->obj_type == obj_type) {
			return(step->obj_class);
		} else {
			step = step->next;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclRegisterCallback
#if     NhlNeedProto
(NclObjTypes class_type, unsigned int callback_type, NclCallBack callback_function,void *call_backdata)
#else
(class_type, callback_type, callback_function,call_backdata)
NclObjTypes class_type;
unsigned int callback_type;
NclCallBack callback_function;
void *call_backdata;
#endif
{
	NclObjClass  oc = NULL;
	NclCallBackList *cl = NULL;

	oc = _NclObjTypeToPointer(class_type);
	if(oc != NULL) {
		cl = NclMalloc(sizeof(NclCallBackList));
		switch(callback_type) {
		case CREATED:
			cl->next = oc->obj_class.create_callback;
			oc->obj_class.create_callback = cl;
			break;
		case DESTROYED:
			cl->next = oc->obj_class.delete_callback;
			oc->obj_class.delete_callback = cl;
			break;
		case MODIFIED:
			cl->next = oc->obj_class.modify_callback;
			oc->obj_class.modify_callback = cl;
			break;
		default:
			return(NhlFATAL);
		}
		cl->func = (NclCallBack)callback_function;
		cl->user_data = call_backdata;
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}
}

void * _NclObtainCallData
#if     NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
	NclObjClass oc = NULL;

	oc = obj->obj.class_ptr;

	if(oc->obj_class.obtain_calldata != NULL) {
		return((*oc->obj_class.obtain_calldata)(obj,type));
	} else {
		return(NULL);
	}
}


NhlErrorTypes _NclCallCallBacks
#if     NhlNeedProto
(NclObj obj, unsigned int type)
#else
(obj, type)
NclObj obj;
unsigned int type;
#endif
{
	NclCallBackList *cl = NULL;
	NclObjClass oc = NULL;
	void *call_data = NULL;
	NhlErrorTypes ret = NhlNOERROR,ret1;
	if(obj != NULL) {
		oc = obj->obj.class_ptr;	
		switch(type) {
		case CREATED:
			cl = oc->obj_class.create_callback;
			if(cl != NULL) {
				call_data  = _NclObtainCallData(obj,CREATED);
			}
			break;
		case MODIFIED:
			cl = oc->obj_class.modify_callback;
			if(cl != NULL) {
				call_data  = _NclObtainCallData(obj,MODIFIED);
			}
			break;
		case DESTROYED:
			cl = oc->obj_class.delete_callback;
			if(cl != NULL) {
				call_data  = _NclObtainCallData(obj,DESTROYED);
			}
			break;
		}
		if(call_data != NULL) {
			while(cl != NULL) {
				ret1 = (*(NclCallBack)cl->func)(call_data,cl->user_data);
				if(ret1 < ret) {	
					ret = ret1;
				}
				cl = cl->next;
			}
			if(call_data != NULL) {
				NclFree(call_data);
			}
		}
		return(ret);
	} else {
		return(NhlFATAL);
	}
}


NhlErrorTypes _NclGetCoordClosestIndex
#if     NhlNeedProto
(NclMultiDValData coord_md, void * ind_val, long* ind)
#else
(coord_md, ind_val, ind)
NclMultiDValData coord_md;
void *ind_val;
long* ind;
#endif
{
	NclOneDValCoordDataClass dc;
	
	if(coord_md == NULL) {
		return(NhlFATAL);
	}  else {
		dc = (NclOneDValCoordDataClass)coord_md->obj.class_ptr;
	}
	if(!(coord_md->obj.obj_type_mask & NCL_COORD_MASK) ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclGetCoordRange: Non coordinate value passed, can't continue");
		return(NhlFATAL);
	}
	while((NclObjClass)dc != nclMultiDValDataClass){
		if(dc->oned_class.get_closest_ind!= NULL) {
			return((*dc->oned_class.get_closest_ind)(coord_md, ind_val, ind));
		} else {
			dc = (NclOneDValCoordDataClass)dc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}
NhlErrorTypes _NclGetCoordRange
#if     NhlNeedProto
(NclMultiDValData coord_md, void * start_val, void *finish_val, long* start, long* finish) 
#else
(coord_md, start_val, finish_val, start, finish) 
NclMultiDValData coord_md;
void *start_val;
void *finish_val;
long* start;
long* finish; 
#endif
{
	NclOneDValCoordDataClass dc;
	
	if(coord_md == NULL) {
		return(NhlFATAL);
	}  else {
		dc = (NclOneDValCoordDataClass)coord_md->obj.class_ptr;
	}
	if(!(coord_md->obj.obj_type_mask & NCL_COORD_MASK) ) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclGetCoordRange: Non coordinate value passed, can't continue");
		return(NhlFATAL);
	}
	while((NclObjClass)dc != nclMultiDValDataClass){
		if(dc->oned_class.get_range_ind != NULL) {
			return((*dc->oned_class.get_range_ind)(coord_md, start_val, finish_val, start, finish));
		} else {
			dc = (NclOneDValCoordDataClass)dc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}


void _NclInitDataClasses
#if     NhlNeedProto
(void)
#else
()
#endif
{
/*
	_NclInitClass(nclMultiDValdoubleDataClass);
	_NclInitClass(nclMultiDValfloatDataClass);
	_NclInitClass(nclMultiDValshortDataClass);
	_NclInitClass(nclMultiDValintDataClass);
	_NclInitClass(nclMultiDVallongDataClass);
	_NclInitClass(nclMultiDValint64DataClass);
	_NclInitClass(nclMultiDValushortDataClass);
	_NclInitClass(nclMultiDValuintDataClass);
	_NclInitClass(nclMultiDValulongDataClass);
	_NclInitClass(nclMultiDValuint64DataClass);
	_NclInitClass(nclMultiDValubyteDataClass);
	_NclInitClass(nclMultiDVallogicalDataClass);
	_NclInitClass(nclMultiDValbyteDataClass);
	_NclInitClass(nclMultiDValcharDataClass);
	_NclInitClass(nclMultiDValstringDataClass);
*/
}



NclMultiDValData _NclStringMdToCharMd
#if	NhlNeedProto
(NclMultiDValData str_md)
#else
(str_md)
NclMultiDValData str_md;
#endif
{
	char **buffer;
	ng_size_t i;
	int n_dims;
	NclQuark *value;
	ng_size_t max_len,tmp_len,to;
	char *val = NULL;
	ng_size_t dim_sizes[NCL_MAX_DIMENSIONS];
	NclQuark missingQ = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
	NhlBoolean has_missing = False;
	NclScalar tmp_missing;

	if (str_md->multidval.missing_value.has_missing) {
		has_missing = True;
		missingQ = str_md->multidval.missing_value.value.stringval;
		/* default missing char value is 0 */
		tmp_missing.charval = '\0';
	}

	buffer = (char**)NclMalloc((unsigned)str_md->multidval.totalelements*sizeof(char*));
	value = (NclQuark*)str_md->multidval.val;
	/* there has to at least be room for the end-of-string 0 byte */
	max_len = 1;
	for(i = 0; i < str_md->multidval.totalelements; i++) {
		if (value[i] == NrmNULLQUARK) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclStringMdToCharMd: Null string encountered, substituting missing value");
			value[i] = missingQ;
		}
		if (has_missing && value[i] == missingQ)
			continue;
		buffer[i] = NrmQuarkToString(value[i]);
		tmp_len = strlen(buffer[i]) + 1;
		if((buffer[i] != NULL) &&(tmp_len > max_len)) {
			max_len = tmp_len;
		}
	}
	if(str_md->multidval.kind == SCALAR) {
		dim_sizes[0] = max_len;
		n_dims = 1;
	} else {
		for(i = 0; i < str_md->multidval.n_dims; i++) {
			dim_sizes[i] = str_md->multidval.dim_sizes[i];
		}
		dim_sizes[str_md->multidval.n_dims] = max_len;
		n_dims = str_md->multidval.n_dims +1;
	}
	val = (char*)NclCalloc(max_len * str_md->multidval.totalelements,sizeof(char));
	for(i = 0, to = 0; i < str_md->multidval.totalelements; i++) {
		if (has_missing && value[i] == missingQ) {
			val[to] = '\0';
		}
		else {
			strcpy((char *)&(val[to]),buffer[i]);
		}
		to += max_len;
	}
	NclFree(buffer);
	return(_NclCreateMultiDVal(
		NULL,
		NULL,
		Ncl_MultiDValData,
		0,
		val,
		(has_missing ? &tmp_missing : NULL),
		n_dims,
		dim_sizes,
		TEMPORARY,
		NULL,
		(NclTypeClass)nclTypecharClass));
}
NclMultiDValData _NclCharMdToStringMd
#if	NhlNeedProto
(NclMultiDValData char_md)
#else
(char_md)
NclMultiDValData char_md;
#endif
{
	ng_size_t i;
	ng_size_t n_strings = 1;
	char *buffer = NULL;
	ng_size_t len =0;
	ng_size_t from = 0;
	NclQuark *value = NULL;
	unsigned char *val = NULL;
	NclQuark missingQ = NrmNULLQUARK;
	char missing = '\0';
	char *cp;
	NhlBoolean has_missing = False;
	NclScalar tmp_missing;
	
	len = char_md->multidval.dim_sizes[char_md->multidval.n_dims -1];

	for( i = 0; i < char_md->multidval.n_dims - 1; i++) {
		n_strings *= char_md->multidval.dim_sizes[i];
	}
	buffer = (char*)NclMalloc((unsigned)len + 1);
	buffer[len] = '\0';
	if (char_md->multidval.missing_value.has_missing) {
		has_missing = True;
		missing = char_md->multidval.missing_value.value.charval;
		buffer[0] = missing;
		buffer[1] = '\0';
		missingQ = NrmStringToQuark(buffer);
		tmp_missing.stringval = missingQ;
	}
	value = (NclQuark*) NclMalloc((unsigned)n_strings*sizeof(NclQuark));
	val = (unsigned char*)char_md->multidval.val;
	if (has_missing) {
		/*
		 * A missing char ends the string. If it is the
		 * only character in the string the string becomes
		 * the missing value. Otherwise, the missing value is
		 * replaced with a string-ending NULL character.
		 */
		for(i = 0; i < n_strings ; i++) {
			memcpy((void*)buffer,&(val[from]),len);
			if (buffer[0] == missing) {
				value[i] = missingQ;
			}
			else {
				for (cp = buffer; *cp != '\0'; cp++) {
					if (*cp != missing)
						continue;
					*cp = '\0';
					break;
				}
				value[i] = NrmStringToQuark(buffer);
			}
			from += len;
		}
	}
	else {
		for(i = 0; i < n_strings ; i++) {
			memcpy((void*)buffer,&(val[from]),len);
			value[i] = NrmStringToQuark(buffer);
			from += len;
		}
	}
	NclFree(buffer);
	if(char_md->multidval.n_dims != 1) {
		return(_NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			(void*)value,
			(has_missing ? &tmp_missing : NULL),
			char_md->multidval.n_dims -1,
			char_md->multidval.dim_sizes,
			TEMPORARY,
			NULL,
			(NclTypeClass)nclTypestringClass));
	} else {
                ng_size_t dim_size = 1;
		return(_NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			(void*)value,
			(has_missing ? &tmp_missing : NULL),
			char_md->multidval.n_dims,
			&dim_size,
			TEMPORARY,
			NULL,
			(NclTypeClass)nclTypestringClass));
	}
}

NclTypeClass _NclNameToTypeClass
#if  NhlNeedProto
(NclQuark typename)
#else
(typename)
	NclQuark typename;
#endif
{
	static int first = 1;
	static NclQuark quarks[19];
	static NclTypeClass classes[19];
	int i;

	if(first) {
		first = 0;
		quarks[0] = NrmStringToQuark("double");
		classes[0] = (NclTypeClass)nclTypedoubleClass;
		quarks[1] = NrmStringToQuark("float");
		classes[1] = (NclTypeClass)nclTypefloatClass;
		quarks[2] = NrmStringToQuark("long");
		classes[2] = (NclTypeClass)nclTypelongClass;
		quarks[3] = NrmStringToQuark("integer");
		classes[3] = (NclTypeClass)nclTypeintClass;
		quarks[4] = NrmStringToQuark("short");
		classes[4] = (NclTypeClass)nclTypeshortClass;
		quarks[5] = NrmStringToQuark("string");
		classes[5] = (NclTypeClass)nclTypestringClass;
		quarks[6] = NrmStringToQuark("character");
		classes[6] = (NclTypeClass)nclTypecharClass;
		quarks[7] = NrmStringToQuark("byte");
		classes[7] = (NclTypeClass)nclTypebyteClass;
		quarks[8] = NrmStringToQuark("logical");
		classes[8] = (NclTypeClass)nclTypelogicalClass;
		quarks[9] = NrmStringToQuark("file");
		classes[9] = (NclTypeClass)nclTypeobjClass;
		quarks[10] = NrmStringToQuark("graphic");
		classes[10] =(NclTypeClass)nclTypeobjClass;
		quarks[11] = NrmStringToQuark("obj");
		classes[11] = (NclTypeClass)nclTypeobjClass;
		quarks[12] = NrmStringToQuark("list");
		classes[12] = (NclTypeClass)nclTypelistClass;
		quarks[13] = NrmStringToQuark("int64");
		classes[13] = (NclTypeClass)nclTypeint64Class;
		quarks[14] = NrmStringToQuark("ushort");
		classes[14] = (NclTypeClass)nclTypeushortClass;
		quarks[15] = NrmStringToQuark("uint");
		classes[15] = (NclTypeClass)nclTypeuintClass;
		quarks[16] = NrmStringToQuark("ulong");
		classes[16] = (NclTypeClass)nclTypeulongClass;
		quarks[17] = NrmStringToQuark("uint64");
		classes[17] = (NclTypeClass)nclTypeuint64Class;
		quarks[18] = NrmStringToQuark("ubyte");
		classes[18] = (NclTypeClass)nclTypeubyteClass;
	}	
	for(i = 0; i < 19; i++) {
		if(quarks[i] == typename) {
			return(classes[i]);
		}
	}
	return(NULL);
}


/*
* Probably should put stuff like this in the actual data object to avoid having to
* add objects here later
*/

long _NclObjTypeToName
#if	NhlNeedProto
(NclObjTypes obj)
#else
(obj)
	NclObjTypes obj;
#endif
{
	switch(obj) {
	case Ncl_Typeint:
		return(NrmStringToQuark("Ncl_Typeint"));
	case Ncl_Typeuint:
		return(NrmStringToQuark("Ncl_Typeuint"));
	case Ncl_Typedouble:
		return(NrmStringToQuark("Ncl_Typedouble"));
	case Ncl_Typebyte:
		return(NrmStringToQuark("Ncl_Typebyte"));
	case Ncl_Typelong:
		return(NrmStringToQuark("Ncl_Typelong"));
	case Ncl_Typeulong:
		return(NrmStringToQuark("Ncl_Typeulong"));
	case Ncl_Typeint64:
		return(NrmStringToQuark("Ncl_Typeint64"));
	case Ncl_Typeuint64:
		return(NrmStringToQuark("Ncl_Typeuint64"));
	case Ncl_Typeubyte:
		return(NrmStringToQuark("Ncl_Typeubyte"));
	case Ncl_Typeshort:
		return(NrmStringToQuark("Ncl_Typeshort"));
	case Ncl_Typeushort:
		return(NrmStringToQuark("Ncl_Typeushort"));
	case Ncl_Typefloat:
		return(NrmStringToQuark("Ncl_Typefloat"));
	case Ncl_Typechar:
		return(NrmStringToQuark("Ncl_Typechar"));
	case Ncl_Typestring:
		return(NrmStringToQuark("Ncl_Typestring"));
	case Ncl_Typelogical:
		return(NrmStringToQuark("Ncl_Typelogical"));
	case Ncl_Typeobj:
		return(NrmStringToQuark("Ncl_Typeobj"));
	case Ncl_Typelist:
		return(NrmStringToQuark("Ncl_Typelist"));
	default:
		return(-1);
	}
}

#ifndef NIO_LIB_ONLY

NclObjTypes _NclKeywordToObjType
#if	NhlNeedProto
(struct _NclSymbol *keywd)
#else
(keywd)
	struct _NclSymbol *keywd;
#endif
{
	if(keywd != NULL) {
		switch(keywd->type) {
		case INTEGER:
			return(Ncl_Typeint);
		case UINT:
			return(Ncl_Typeuint);
		case DOUBLE:
			return(Ncl_Typedouble);
		case BYTE:
			return(Ncl_Typebyte);
		case LONG:
			return(Ncl_Typelong);
		case ULONG:
			return(Ncl_Typeulong);
		case INT64:
			return(Ncl_Typeint64);
		case UINT64:
			return(Ncl_Typeuint64);
		case UBYTE:
			return(Ncl_Typeubyte);
		case SHORT:
			return(Ncl_Typeshort);
		case USHORT:
			return(Ncl_Typeushort);
		case FLOAT:
			return(Ncl_Typefloat);
		case CHARACTER:
			return(Ncl_Typechar);
		case STRNG:
			return(Ncl_Typestring);
		case GROUP:
			return(Ncl_Typegroup);
		case COMPOUND:
			return(Ncl_Typecompound);
		case NUMERIC:
			return(NCL_NUMERIC_TYPE_MASK);
	       	case ENUMERIC:
	                return(NCL_ENUMERIC_TYPE_MASK);
		case SNUMERIC:
			return(NCL_SNUMERIC_TYPE_MASK);
		case GRAPHIC:
			return(Ncl_MultiDValHLUObjData);
		case LOGICAL:
			return(Ncl_Typelogical);
		case FILETYPE:
			return(Ncl_MultiDValnclfileData);
		case LIST:
			return(Ncl_Typelist);
		default:
			return(Ncl_Obj);
		}
	} else {
		return(Ncl_Obj);
	}
}

#endif

NclObjTypes _NclBasicDataTypeToObjType
#if	NhlNeedProto
(NclBasicDataTypes dt)
#else
(dt)
	NclBasicDataTypes dt;
#endif
{
	switch(dt) {
	case NCL_short:
		return(Ncl_Typeshort);
	case NCL_int:
		return(Ncl_Typeint);
	case NCL_long:
		return(Ncl_Typelong);
	case NCL_int64:
		return(Ncl_Typeint64);
        case NCL_ushort:
                return(Ncl_Typeushort);
        case NCL_uint:
                return(Ncl_Typeuint);
        case NCL_ulong:
                return(Ncl_Typeulong);
        case NCL_uint64:
                return(Ncl_Typeuint64);
        case NCL_ubyte:
                return(Ncl_Typeubyte);
	case NCL_float:
		return(Ncl_Typefloat);
	case NCL_double:
		return(Ncl_Typedouble);
	case NCL_char:
		return(Ncl_Typechar);
	case NCL_byte:
		return(Ncl_Typebyte);
	case NCL_string:
		return(Ncl_Typestring);
	case NCL_logical:
		return(Ncl_Typelogical);
	case NCL_obj:
		return(Ncl_Typeobj);
	case NCL_group:
		return(Ncl_Typegroup);
	case NCL_compound:
		return(Ncl_Typecompound);
        case NCL_reference:
		return(Ncl_Typereference);
	case NCL_list:
	case NCL_vlen:
		return(Ncl_Typelist);
	default:
                if(NCL_ubyte == (dt ^ NCL_opaque))
                    return (Ncl_Typeubyte);
                else if(dt ^ NCL_enum)
                    return _NclBasicDataTypeToObjType(dt ^ NCL_enum);
                else if(dt ^ NCL_vlen)
                    return(Ncl_Typelist);
                else if(dt ^ NCL_list)
                    return(Ncl_Typelist);
                else
		    return (Ncl_Obj);
	}
}

#ifndef NIO_LIB_ONLY
NclBasicDataTypes _NclKeywordToDataType
#if	NhlNeedProto
(struct _NclSymbol *keywd)
#else
(keywd)
	struct _NclSymbol *keywd;
#endif
{

	if(keywd != NULL) {
		switch(keywd->type) {
		case DOUBLE:
			return(NCL_double);
		case BYTE:
			return(NCL_byte);
		case SHORT:
			return(NCL_short);
		case INTEGER:
			return(NCL_int);
		case LONG:
			return(NCL_long);
		case INT64:
			return(NCL_int64);
                case USHORT:
                        return(NCL_ushort);
                case UINT:
                        return(NCL_uint);
                case ULONG:
                        return(NCL_ulong);
		case UINT64:
                        return(NCL_uint64);
		case UBYTE:
                        return(NCL_ubyte);
		case FLOAT:
			return(NCL_float);
		case CHARACTER:
			return(NCL_char);
		case STRNG:
			return(NCL_string);
		case GROUP:
			return(NCL_group);
		case COMPOUND:
			return(NCL_compound);
		case NUMERIC:
			return(NCL_numeric);
	       	case ENUMERIC:
	       	        return(NCL_enumeric);
		case SNUMERIC:
			return(NCL_snumeric);
		case LOGICAL:
			return(NCL_logical);
		case LIST:
			return(NCL_list);
		case GRAPHIC:
		case FILETYPE:
			return(NCL_obj);
		default:
			return(NCL_none);
		}
	} else {
		return(NCL_none);
	}
}
#endif

int _NclSizeOf
#if	NhlNeedProto
(NclBasicDataTypes data_type)
#else 
(data_type)
NclBasicDataTypes data_type;
#endif
{
	switch(data_type) {
		case NCL_short:
			return(sizeof(short));
		case NCL_int:
			return(sizeof(int));
		case NCL_long:
			return(sizeof(long));
		case NCL_int64:
			return(sizeof(long long));
                case NCL_ushort:
                        return(sizeof(unsigned short));
                case NCL_uint:
                        return(sizeof(unsigned int));
                case NCL_ulong:
                        return(sizeof(unsigned long));
                case NCL_uint64:
                        return(sizeof(unsigned long long));
                case NCL_ubyte:
                        return(sizeof(unsigned char));
		case NCL_float:
			return(sizeof(float));
		case NCL_double:
			return(sizeof(double));
		case NCL_char:
			return(sizeof(char));
		case NCL_byte:
			return(sizeof(char));
		case NCL_string:
			return(sizeof(NclQuark));	
		case NCL_obj:
			return(sizeof(obj));	
		case NCL_group:
			return(sizeof(group));	
		case NCL_compound:
			return(sizeof(nclcompound));
		case NCL_logical:
			return(sizeof(int));	
		case NCL_list:
			return(sizeof(int));	
		case NCL_opaque:
			return(sizeof(char));
		case NCL_enum:
			return(sizeof(int));
		case NCL_vlen:
			return(sizeof(int));
		case NCL_reference:
			return(sizeof(NclQuark));	
		default:
			return(-1);
	}
}

void _NclDestroyObj
#if	NhlNeedProto
(NclObj obj)
#else
(obj)
	NclObj obj;
#endif
{
	NclObjClass oc;
        NhlArgVal selector;
        NhlArgVal cbdata;

	_NclCallCallBacks(obj,DESTROYED);
        cbdata.intval = obj->obj.id;
        selector.lngval = DESTROYED;
        _NhlCBCallCallbacks(obj->obj.cblist,selector,cbdata);


	if(obj == NULL)  {
		return;
	} else {
		oc = obj->obj.class_ptr;
	}

	while(oc != NULL) {
		if(oc->obj_class.destroy != NULL)  {
			(*(oc->obj_class.destroy))(obj);
			return;
		} else {
			oc = oc->obj_class.super_class;
		}
	} 
	NclFree((void*)obj);
	return;
}


int _NclScalarCoerce
#if	NhlNeedProto
(void *from,NclBasicDataTypes frtype,void *to,NclBasicDataTypes totype)
#else
(from,frtype,to,totype)
void *from;
NclBasicDataTypes frtype;
void *to;
NclBasicDataTypes totype;
#endif
{
	char buffer[80];

	switch(frtype) {
	case NCL_short:
		switch(totype) {
		case NCL_short:
			*(short*)to= *(short*)from;
			return(1);
		case NCL_int:
			*(int*)to = *(short*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(short*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(short*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(short*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(short*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(short*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(short*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(short*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(short*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(short*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",(int)*(short*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_int:
		switch(totype) {
                case NCL_int:
			*(int*)to = *(int*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(int*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(int*)from;
			return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(int*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(int*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(int*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(int*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(int*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(int*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",*(int*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_long:
		switch(totype) {
		case NCL_long:
			*(long*)to = *(long*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(long*)from;
			return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(long*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(long*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(long*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(long*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%ld",(long)*(long*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
        case NCL_int64:
                switch(totype) {
                case NCL_long:
                        *(long*)to = *(long long*)from;
                        return(1);
                case NCL_int64:
                        *(long long*)to = *(long long*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(long long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(long long*)from;
                        return(1);
                case NCL_float:
                        *(float*)to = *(long long*)from;
                        return(1);
                case NCL_double:
                        *(double*)to = *(long long*)from;
                        return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(long long*)from?1:0);
                        return(1);
                case NCL_string:
                        sprintf(buffer,"%lld",(long long)*(long long*)from);
                        *(NclQuark*)to = NrmStringToQuark(buffer);
                        return(1);
                default:
                        return(0);
                }
	case NCL_ushort:
		switch(totype) {
		case NCL_short:
			*(short*)to= *(unsigned short*)from;
			return(1);
		case NCL_int:
			*(int*)to = *(unsigned short*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(unsigned short*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned short*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(unsigned short*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(unsigned short*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned short*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned short*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(unsigned short*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(unsigned short*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(unsigned short*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",(int)*(unsigned short*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_uint:
		switch(totype) {
                case NCL_int:
			*(int*)to = *(unsigned int*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(unsigned int*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned int*)from;
			return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(unsigned int*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned int*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned int*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(unsigned int*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(unsigned int*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(unsigned int*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",*(unsigned int*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_ulong:
		switch(totype) {
		case NCL_long:
			*(long*)to = *(unsigned long*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned long*)from;
			return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned long*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(unsigned long*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(unsigned long*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(unsigned long*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%ld",(unsigned long)*(unsigned long*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
        case NCL_uint64:
                switch(totype) {
                case NCL_long:
                        *(long*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_int64:
                        *(long long*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_float:
                        *(float*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_double:
                        *(double*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(unsigned long long*)from?1:0);
                        return(1);
                case NCL_string:
                        sprintf(buffer,"%lld",(unsigned long long)*(unsigned long long*)from);
                        *(NclQuark*)to = NrmStringToQuark(buffer);
                        return(1);
                default:
                        return(0);
                }
	case NCL_float:
		switch(totype) {
		case NCL_float:
			*(float*)to = *(float*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(float*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(float*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%g",*(float*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_double:
		switch(totype) {
		case NCL_double:
			*(double*)to = *(double*)from;
			return(1);
		case NCL_string:
			sprintf(buffer,"%lg",*(double*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(double*)from?1:0);
			return(1);
		default:
			return(0);
		}
	case NCL_char:
		switch(totype) {
		case NCL_byte:
			*(byte*) to = *(char*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(char*)from;
			return(1);
		case NCL_float:
			*(float*) to = *(char*)from;
			return(1);
		case NCL_double:
			*(double*) to = *(char*)from;
			return(1);
		case NCL_long:
			*(long*) to = *(char*)from;
			return(1);
		case NCL_int64:
			*(long long*) to = *(char*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(char*)from?1:0);
			return(1);
		case NCL_short:
			*(short*) to = *(char*)from;
			return(1);
		case NCL_int:
			*(int*) to = *(char*)from;
			return(1);
		case NCL_string:
			buffer[0] = *(char*)from;
			buffer[1] = '\0';
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_byte:
		switch(totype) {
		case NCL_byte:
			*(byte*) to = *(byte*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(byte*)from;
			return(1);
		case NCL_float:
			*(float*) to = *(byte*)from;
			return(1);
		case NCL_double:
			*(double*) to = *(byte*)from;
			return(1);
		case NCL_long:
			*(long*) to = *(byte*)from;
			return(1);
		case NCL_int64:
			*(long long*) to = *(byte*)from;
			return(1);
                case NCL_ulong:
                        *(unsigned long*) to = *(byte*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*) to = *(byte*)from;
                        return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(byte*)from?1:0);
			return(1);
		case NCL_short:
			*(short*) to = *(byte*)from;
			return(1);
		case NCL_int:
			*(int*) to = *(byte*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*) to = *(byte*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*) to = *(byte*)from;
                        return(1);
		case NCL_string:
			buffer[0] = *(byte*)from;
			buffer[1] = '\0';
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_ubyte:
		switch(totype) {
		case NCL_byte:
			*(byte*) to = *(ubyte*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(ubyte*)from;
			return(1);
		case NCL_float:
			*(float*) to = *(ubyte*)from;
			return(1);
		case NCL_double:
			*(double*) to = *(ubyte*)from;
			return(1);
		case NCL_long:
			*(long*) to = *(ubyte*)from;
			return(1);
		case NCL_int64:
			*(long long*) to = *(ubyte*)from;
			return(1);
                case NCL_ulong:
                        *(unsigned long*) to = *(ubyte*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*) to = *(ubyte*)from;
                        return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(ubyte*)from?1:0);
			return(1);
		case NCL_short:
			*(short*) to = *(ubyte*)from;
			return(1);
		case NCL_int:
			*(int*) to = *(ubyte*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*) to = *(ubyte*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*) to = *(ubyte*)from;
                        return(1);
		case NCL_string:
			buffer[0] = *(ubyte*)from;
			buffer[1] = '\0';
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_string:
		switch(totype) {
		case NCL_string:
			*(NclQuark*)to = *(NclQuark*)from;
			return(1);
		default:
			return(0);
		}
	case NCL_logical:
	default:
		return(0);
	}
}

int _NclScalarForcedCoerce
#if	NhlNeedProto
(void *from,NclBasicDataTypes frtype,void *to,NclBasicDataTypes totype)
#else
(from,frtype,to,totype)
void *from;
NclBasicDataTypes frtype;
void *to;
NclBasicDataTypes totype;
#endif
{
	char buffer[80];

	switch(frtype) {
	case NCL_char:
		switch(totype) {
			double tmpd;
			char *endptr;
		case NCL_byte:
			*(byte*)to= *(char*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(char*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(char*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(char*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(char*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(char*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(char*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(char*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(char*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(char*)from;
                        return(1);
                case NCL_float:
			tmpd = strtod((char*)from,&endptr);
			if (endptr != from)
				*(float *)to = (float)tmpd;
			else
				*(float*)to = *(char*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(char*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(char*)from?1:0);
			return(1);
		case NCL_string:
			buffer[0] = *(char*)from;
			buffer[1] = '\0';
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_byte:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(byte*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(byte*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(byte*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(byte*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(byte*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(byte*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(byte*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(byte*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(byte*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(byte*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(byte*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(byte*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(byte*)from?1:0);
			return(1);
		case NCL_string:
			buffer[0] = *(byte*)from;
			buffer[1] = '\0';
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_short:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(short*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(short*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(short*)from;
			return(1);
		case NCL_int:
			*(int*)to = *(short*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(short*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(short*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(short*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(short*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(short*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(short*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(short*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(short*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(short*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",(int)*(short*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_int:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(int*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(int*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(int*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(int*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(int*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(int*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to = *(int*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(int*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(int*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(int*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(int*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(int*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(int*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",*(int*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_long:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(long*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(long*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(long*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(long*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(long*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(long*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to = *(long*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(long*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(long*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(long*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(long*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(long*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%ld",(long)*(long*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
        case NCL_int64:
                switch(totype) {
		case NCL_byte:
			*(byte*)to= *(long long*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(long long*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(long long*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(long long*)from;
			return(1);
                case NCL_long:
                        *(long*)to = *(long long*)from;
                        return(1);
                case NCL_int64:
                        *(long long*)to = *(long long*)from;
                        return(1);
                case NCL_ushort:
                        *(unsigned short*)to = *(long long*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(long long*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(long long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(long long*)from;
                        return(1);
                case NCL_float:
                        *(float*)to = *(long long*)from;
                        return(1);
                case NCL_double:
                        *(double*)to = *(long long*)from;
                        return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(long long*)from?1:0);
                        return(1);
                case NCL_string:
                        sprintf(buffer,"%lld",(long long)*(long long*)from);
                        *(NclQuark*)to = NrmStringToQuark(buffer);
                        return(1);
                default:
                        return(0);
                }
	case NCL_ushort:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(unsigned short*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(unsigned short*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(unsigned short*)from;
			return(1);
		case NCL_int:
			*(int*)to = *(unsigned short*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(unsigned short*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned short*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(unsigned short*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(unsigned short*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned short*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned short*)from;
                        return(1);
		case NCL_float:
			*(float*)to = *(unsigned short*)from;
			return(1);
		case NCL_double:
			*(double*)to = *(unsigned short*)from;
			return(1);
		case NCL_logical:
			*(logical*)to = (logical)(*(unsigned short*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",(int)*(unsigned short*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_uint:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(unsigned int*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(unsigned int*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(unsigned int*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(unsigned int*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(unsigned int*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned int*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(unsigned int*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(unsigned int*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned int*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned int*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(unsigned int*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(unsigned int*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(unsigned int*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%d",*(unsigned int*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_ulong:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(unsigned long*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(unsigned long*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(unsigned long*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(unsigned long*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(unsigned long*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned long*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(unsigned long*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(unsigned long*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned long*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(unsigned long*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(unsigned long*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(unsigned long*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%ld",(unsigned long)*(unsigned long*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
        case NCL_uint64:
                switch(totype) {
		case NCL_byte:
			*(byte*)to= *(unsigned long long*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(unsigned long long*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(unsigned long long*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(unsigned long long*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(unsigned long long*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(unsigned long long*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(unsigned long long*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(unsigned long long*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(unsigned long long*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(unsigned long long*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(unsigned long long*)from?1:0);
			return(1);
                case NCL_string:
                        sprintf(buffer,"%lld",(unsigned long long)*(unsigned long long*)from);
                        *(NclQuark*)to = NrmStringToQuark(buffer);
                        return(1);
                default:
                        return(0);
                }
	case NCL_float:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(float*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(float*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(float*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(float*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(float*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(float*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(float*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(float*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(float*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(float*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(float*)from;
                        return(1);
                case NCL_ubyte:
                        *(unsigned char*)to = *(char*)from;
                        return(1);
                case NCL_double:
			*(double*)to = *(float*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(float*)from?1:0);
			return(1);
		case NCL_string:
			sprintf(buffer,"%g",*(float*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_double:
		switch(totype) {
		case NCL_byte:
			*(byte*)to= *(double*)from;
			return(1);
		case NCL_char:
			*(char*) to = *(double*)from;
			return(1);
		case NCL_short:
			*(short*)to= *(double*)from;
			return(1);
                case NCL_int:
			*(int*)to = *(double*)from;
			return(1);
		case NCL_long:
			*(long*)to = *(double*)from;
			return(1);
		case NCL_int64:
			*(long long*)to = *(double*)from;
			return(1);
                case NCL_ushort:
                        *(unsigned short*)to= *(double*)from;
                        return(1);
                case NCL_uint:
                        *(unsigned int*)to = *(double*)from;
                        return(1);
                case NCL_ulong:
                        *(unsigned long*)to = *(double*)from;
                        return(1);
                case NCL_uint64:
                        *(unsigned long long*)to = *(double*)from;
                        return(1);
                case NCL_float:
			*(float*)to = *(double*)from;
			return(1);
                case NCL_double:
			*(double*)to = *(double*)from;
			return(1);
                case NCL_logical:
			*(logical*)to = (logical)(*(double*)from?1:0);
			return(1);
                case NCL_ubyte:
                        *(unsigned char*)to = *(double*)from;
                        return(1);
		case NCL_string:
			sprintf(buffer,"%lg",*(double*)from);
			*(NclQuark*)to = NrmStringToQuark(buffer);
			return(1);
		default:
			return(0);
		}
	case NCL_string:
		{
		char *val = NrmQuarkToString(*(NrmQuark*)from);
		char *end;

		switch(totype) {
		case NCL_byte:
			*(byte*)to = (byte) _Nclstrtol(val, &end);
			return(1);
		case NCL_char:
			*(char*)to = (char) _Nclstrtoul(val, &end);
			return(1);
		case NCL_short:
			*(short*)to = (short) _Nclstrtol(val, &end);
			return(1);
                case NCL_int:
			*(int*)to = (int) _Nclstrtol(val, &end);
			return(1);
		case NCL_long:
			*(long*)to = _Nclstrtol(val, &end);
			return(1);
		case NCL_int64:
			*(long long*)to = _Nclstrtoll(val, &end);
			return(1);
                case NCL_ubyte:
                        *(unsigned char*)to = (unsigned char) _Nclstrtoul(val, &end);
                        return(1);
                case NCL_ushort:
			*(unsigned short*)to = (unsigned short) _Nclstrtoul(val, &end);
                        return(1);
                case NCL_uint:
			*(unsigned int*)to = (unsigned int) _Nclstrtoul(val, &end);
                        return(1);
                case NCL_ulong:
			*(unsigned long*)to = (unsigned long) _Nclstrtoul(val, &end);
                        return(1);
                case NCL_uint64:
			*(unsigned long long*)to = (unsigned long long) _Nclstrtoull(val, &end);
                        return(1);
                case NCL_float:
			{
			double dval = strtod(val,&end);
                	if (end == val || errno == ERANGE) {
                        	*(float*)to = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
                	} else
                        	*(float*)to = (float)dval;
			return(1);
			}
                case NCL_double:
			{
			double dval = strtod(val,&end);
                	if (end == val || errno == ERANGE) {
                        	*(double*)to = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
                	} else
                        	*(double*)to = dval;
			return(1);
			}
                case NCL_logical:
			*(logical*)to = (logical)_Nclstrtol(val, &end);
			return(1);
		case NCL_string:
			*(NclQuark*)to = *(NclQuark*)from;
			return(1);
		default:
			return(0);
		}
		}
	case NCL_logical:
	default:
		return(0);
	}
}

NhlErrorTypes _NclPrint
#if	NhlNeedProto
(NclObj obj,FILE *fp)
#else 
(obj,fp)
NclObj obj;
FILE *fp;
#endif
{
	NclObjClass oc;

	if(obj == NULL)  {
		return(NhlWARNING);
	} else {
		oc = obj->obj.class_ptr;
	}

	while(oc != NULL) {
		if(oc->obj_class.print != NULL)  {
			return((*(oc->obj_class.print))(obj,fp));
		} else {
			oc = oc->obj_class.super_class;
		}
	} 
	return(NhlWARNING);
}


NclMultiDValData _NclCoerceData
#if	NhlNeedProto
(NclMultiDValData obj, NclObjTypes coerce_to,NclScalar *new_missing)
#else
(obj, coerce_to,new_missing)
NclMultiDValData obj;
NclObjTypes coerce_to;
NclScalar *new_missing;
#endif
{
	NclDataClass oc;
        int f_selection;

	if((obj == NULL)||!(obj->obj.obj_type_mask & NCL_MD_MASK)) {
                return(NULL);
	} else {
		oc = (NclDataClass)obj->obj.class_ptr;
	}

        f_selection = (int)obj->multidval.kind;
	while((NclObjClass)oc != nclObjClass) {
        	if( oc->data_class.coerce[f_selection] != NULL) {
                	return((NclMultiDValData)((*oc->data_class.coerce[f_selection])((NclData)obj,coerce_to,new_missing)));
        	} else {
			oc = (NclDataClass)oc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclCallDualOp
#if	NhlNeedProto
(NclMultiDValData lhs_data_obj, NclMultiDValData rhs_data_obj, int operation, NclObj *result)
#else
(lhs_data_obj, rhs_data_obj, operation,result)
NclMultiDValData lhs_data_obj;
NclMultiDValData rhs_data_obj;
int operation;
NclObj *result;
#endif
{
	NclDataClass data_part;
	int f_selection;
	NclMultiDValData tmp_result = (NclMultiDValData)*result;

	data_part = (NclDataClass)
		((NclDataClass)lhs_data_obj->obj.class_ptr);

	f_selection = (int)
		((lhs_data_obj->multidval.kind<< 1)
		|(rhs_data_obj->multidval.kind));

	*result = NULL;
	while((NclObjClass) data_part != nclObjClass){
		switch(operation) {
		case MOD_OP:
		if(data_part->data_class.mod[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.mod[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case OR_OP:
		if(data_part->data_class.ncl_or[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ncl_or[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case AND_OP:
		if(data_part->data_class.ncl_and[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ncl_and[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case XOR_OP:
		if(data_part->data_class.ncl_xor[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ncl_xor[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case LTSEL_OP:
		if(data_part->data_class.sel_lt[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.sel_lt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case GTSEL_OP:
		if(data_part->data_class.sel_gt[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.sel_gt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case PLUS_OP:
		if(data_part->data_class.plus[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.plus[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case MINUS_OP:
		if(data_part->data_class.minus[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.minus[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case MUL_OP:
		if(data_part->data_class.multiply[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.multiply[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case MAT_OP:
		if(data_part->data_class.mat[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.mat[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case DIV_OP:
		if(data_part->data_class.divide[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.divide[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case EXP_OP:
		if(data_part->data_class.exponent[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.exponent[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case LE_OP:
		if(data_part->data_class.le[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.le[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case GE_OP:
		if(data_part->data_class.ge[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ge[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case GT_OP:
		if(data_part->data_class.gt[f_selection] != NULL) {
			*result =(NclObj) ((*data_part->data_class.gt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case LT_OP:
		if(data_part->data_class.lt[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.lt[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case EQ_OP:
		if(data_part->data_class.eq[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.eq[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		case NE_OP:
		if(data_part->data_class.ne[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ne[f_selection])((NclData)lhs_data_obj,(NclData)rhs_data_obj,(NclData)tmp_result));
		}
		break;
		default:
			return(NhlFATAL);
		}
		if(*result != NULL) 
			return(NhlNOERROR);
		data_part = (NclDataClass)data_part->obj_class.super_class;
	}
	if(*result != NULL) {
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}

}


NhlErrorTypes _NclCallMonoOp
#if	NhlNeedProto
(NclMultiDValData operand, NclObj *result, int operation)
#else
(operand, result,operation)
NclMultiDValData operand;
NclObj *result;
int operation;
#endif
{
	NclDataClass data_part;
	int f_selection;

	data_part = (NclDataClass)((NclDataClass)operand->obj.class_ptr);

	f_selection = (int)operand->multidval.kind;

	while(((NclObjClass)data_part != nclObjClass)&&(*result == NULL)) { 
		switch(operation) {
		case NOT_OP:
		if(data_part->data_class.ncl_not[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.ncl_not[f_selection])((NclData)operand,NULL));
		}
		break;
		case NEG_OP:
		if(data_part->data_class.neg[f_selection] != NULL) {
			*result = (NclObj)((*data_part->data_class.neg[f_selection])((NclData)operand,NULL));
		}
		break;
		}
		data_part = (NclDataClass)data_part->obj_class.super_class;
	}

	if(*result != NULL) {
		return(NhlNOERROR);
	} else {
		*result = NULL;
		return(NhlFATAL); 
	}
}



int _NclIsMissing 
#if	NhlNeedProto
(NclMultiDValData self, void* val)
#else 
(self, val)
NclMultiDValData self;
void* val;
#endif
{
	NclDataClass dc;
	
	if(self == NULL) {
		return(0);
	}  else {
		dc = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass){
		if(dc->data_class.is_mis != NULL) {
			return((*dc->data_class.is_mis)((NclData)self,val));
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return(0);
}
struct _NclMultiDValDataRec* _NclCopyVal
#if	NhlNeedProto
(struct _NclMultiDValDataRec * self,NclScalar *new_missing)
#else
(self,new_missing)
struct _NclMultiDValDataRec * self;
NclScalar *new_missing;
#endif
{
	NclDataClass dc;
	
	if(self == NULL) {
		return(NULL);
	}  else {
		dc = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass){
		if(dc->data_class.dup != NULL) {
			return((NclMultiDValData)(*dc->data_class.dup)((NclData)self,new_missing));
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return(NULL);
}

void _NclResetMissingValue
#if	NhlNeedProto
(struct _NclMultiDValDataRec * self,NclScalar *missing)
#else
(self,missing)
	struct _NclMultiDValDataRec *self;
	NclScalar *missing;
#endif
{
	NclDataClass dc ;
	
	if(self == NULL) {
		return;
	} else {
		dc  = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass) {

		if(dc->data_class.reset_mis != NULL) {
			(*dc->data_class.reset_mis)((NclData)self,missing);
			return;
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return;
}

struct _NclMultiDValDataRec *_NclCreateLMissing
#if NhlNeedProto
(void)
#else
()
#endif
{
	static int first = 1;
	static NclMultiDValData tval = NULL;
	static NclScalar missing;

	if(first) {
		int *val = (int*)NclMalloc((unsigned)sizeof(int));
		ng_size_t dim_sizes = 1;
		*val = -1;
		missing.logicalval = (logical)-1;
		tval = _NclCreateMultiDVal(
			NULL,
			nclMultiDValDataClass,
			Ncl_MultiDValData,
			Ncl_MultiDValData,
			(void*)val,
			&missing,
			1,
			&dim_sizes,
			PERMANENT,
			NULL,
			(NclTypeClass)nclTypelogicalClass);
		first = 0;
	} 
	return(tval);
}
struct _NclMultiDValDataRec *_NclCreateFalse
#if NhlNeedProto
(void)
#else
()
#endif
{
	static int first = 1;
	static NclMultiDValData tval = NULL;

	if(first) {
		int *val = (int*)NclMalloc((unsigned)sizeof(int));
		ng_size_t dim_sizes = 1;
		*val = 0;
		tval = _NclCreateMultiDVal(
			NULL,
			nclMultiDValDataClass,
			Ncl_MultiDValData,
			Ncl_MultiDValData,
			(void*)val,
			NULL,
			1,
			&dim_sizes,
			PERMANENT,
			NULL,
			(NclTypeClass)nclTypelogicalClass);
		first = 0;
	} 
	return(tval);
	

}
struct _NclMultiDValDataRec *_NclCreateTrue
#if NhlNeedProto
(void)
#else
()
#endif
{
	static int first = 1;
	static NclMultiDValData tval = NULL;

	if(first) {
		int *val = (int*)NclMalloc((unsigned)sizeof(int));
		ng_size_t dim_sizes = 1;
		*val = 1;
		tval = _NclCreateMultiDVal(
			NULL,
			nclMultiDValDataClass,
			Ncl_MultiDValData,
			Ncl_MultiDValData,
			(void*)val,
			NULL,
			1,
			&dim_sizes,
			PERMANENT,
			NULL,
			(NclTypeClass)nclTypelogicalClass);
		first = 0;
	} 
	return(tval);
	

}
struct _NclMultiDValDataRec *_NclCreateMissing
#if NhlNeedProto
(void)
#else
()
#endif
{
	NclMultiDValData tval;
	int *val = (int*)NclMalloc((unsigned)sizeof(int));
	ng_size_t dim_sizes = 1;
	*val = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;

	tval = _NclCreateMultiDVal(
		NULL,
		nclMultiDValDataClass,
		Ncl_MultiDValData,
		Ncl_MultiDValData,
		(void*)val,
		&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
		1,
		&dim_sizes,
		TEMPORARY,
		NULL,
		(NclTypeClass)nclTypeintClass);

	return(tval);
}
struct _NclMultiDValDataRec * _NclCreateVal
#if	NhlNeedProto
(NclObj inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, void *val, NclScalar *missing_value, int n_dims, ng_size_t *dim_sizes, NclStatus status, NclSelectionRecord *sel_rec,NclObjClass type)
#else 
( inst, theclass, obj_type, obj_type_mask, val, missing_value, n_dims, dim_sizes, status, sel_rec,type)
NclObj inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
void *val;
NclScalar *missing_value;
int n_dims;
ng_size_t *dim_sizes;
NclStatus status;
NclSelectionRecord *sel_rec;
NclObjClass type;
#endif
{

	switch(obj_type) {
	case Ncl_MultiDValData:
		return(_NclCreateMultiDVal(
			inst,
			nclMultiDValDataClass,
			Ncl_MultiDValData,
			Ncl_MultiDValData,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec,
			(NclTypeClass)type));
        case Ncl_OneDValCoordData:
		return(_NclOneDValCoordDataCreate(
			inst,
			nclOneDValCoordDataClass,
			Ncl_OneDValCoordData,
			Ncl_OneDValCoordData,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec,
			(NclTypeClass)type));
	case Ncl_MultiDValHLUObjData:
		return(_NclMultiDValHLUObjDataCreate(
			inst,
			nclMultiDValHLUObjDataClass,
			Ncl_MultiDValHLUObjData,
			Ncl_MultiDValHLUObjData,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec
			));
	case Ncl_MultiDValnclfileData:
		return(_NclMultiDValnclfileDataCreate(
			inst,
			nclMultiDValnclfileDataClass,
			Ncl_MultiDValnclfileData,
			Ncl_MultiDValnclfileData,
			val,
			missing_value,
			n_dims,
			dim_sizes,
			status,
			sel_rec
			));
        default:
                return(NULL);
        }
}


_NhlCB _NclAddCallback
#if 	NhlNeedProto
( struct _NclObjRec * theobj, struct _NclObjRec *parent, _NhlCBFunc cbfunc, long cbsel, NhlArgVal *udata )
#else
( theobj, parent, cbfunc, cbsel , udata)
struct _NclObjRec * theobj;
struct _NclObjRec *parent;
_NhlCBFunc cbfunc;
long cbsel; 
NhlArgVal *udata;
#endif
{
	NhlArgVal mudata;
	NhlArgVal selector;

	NhlINITVAR(mudata);
	NhlINITVAR(selector);

	if(udata != NULL) {	
		mudata = *udata;
	} else if(parent != NULL) {
		mudata.intval = parent->obj.id;
	} else {
		mudata.ptrval = NULL;
	}
	selector.lngval = cbsel;
	if(theobj->obj.cblist == NULL) {
		theobj->obj.cblist = _NhlCBCreate(1,NULL,NULL,NULL,NULL);
	}
	return(_NhlCBAdd(theobj->obj.cblist, selector, cbfunc, mudata));
}

NhlErrorTypes _NclAddParent
#if	NhlNeedProto
(struct _NclObjRec * theobj, struct _NclObjRec* parent)
#else
( theobj,  parent)
struct _NclObjRec * theobj;
struct _NclObjRec* parent;
#endif
{
	NclObjClass class_ptr;


	if((theobj == NULL)||(parent == NULL)) 
		return(NhlFATAL);

	class_ptr = theobj->obj.class_ptr;

	while((class_ptr != NULL)&&(class_ptr->obj_class.add_parent == NULL)) {
		class_ptr = class_ptr->obj_class.super_class;
	}
	if(class_ptr == NULL) {
		return(NhlFATAL);
	} else {
		return((*class_ptr->obj_class.add_parent)(theobj,parent));
	}
}

NhlErrorTypes _NclDelParent
#if	NhlNeedProto
(struct _NclObjRec * theobj, struct _NclObjRec* parent)
#else
(theobj, parent)
struct _NclObjRec * theobj;
struct _NclObjRec* parent;
#endif
{
	NclObjClass class_ptr;

	if((theobj == NULL)||(parent == NULL)) 
		return(NhlFATAL);

	class_ptr = theobj->obj.class_ptr;

	while((class_ptr != NULL)&&(class_ptr->obj_class.del_parent == NULL)) {
		class_ptr = class_ptr->obj_class.super_class;
	}
	if(class_ptr == NULL) {
		return(NhlFATAL);
	} else {
		return((*class_ptr->obj_class.del_parent)(theobj,parent));
	}
}


int _NclGetObjRefCount
#if	NhlNeedProto
(int the_id)
#else
(the_id)
	int the_id;
#endif
{
	struct _NclObjRec * the_obj;

	the_obj = _NclGetObj(the_id);
	if(the_obj != NULL) {
		return(the_obj->obj.ref_count);
	} else {
		return(-1);
	}
}


char* _NclBasicDataTypeToName
#if	NhlNeedProto
(NclBasicDataTypes dt)
#else
(dt)
NclBasicDataTypes dt;
#endif
{
	static int first = 1;
	static NclQuark quarks[26];

	if(first) {
		first = 0;
                quarks[0] = NrmStringToQuark("double");
                quarks[1] = NrmStringToQuark("float");
                quarks[2] = NrmStringToQuark("long");
                quarks[3] = NrmStringToQuark("integer");
                quarks[4] = NrmStringToQuark("short");
                quarks[5] = NrmStringToQuark("string");
                quarks[6] = NrmStringToQuark("character");
                quarks[7] = NrmStringToQuark("byte");
                quarks[8] = NrmStringToQuark("logical");
                quarks[9] = NrmStringToQuark("file");
                quarks[10] = NrmStringToQuark("graphic");
                quarks[11] = NrmStringToQuark("obj");
                quarks[12] = NrmStringToQuark("list");
                quarks[13] = NrmStringToQuark("int64");
                quarks[14] = NrmStringToQuark("ushort");
                quarks[15] = NrmStringToQuark("uint");
                quarks[16] = NrmStringToQuark("ulong");
                quarks[17] = NrmStringToQuark("uint64");
                quarks[18] = NrmStringToQuark("group");
                quarks[19] = NrmStringToQuark("compound");
                quarks[20] = NrmStringToQuark("ubyte");
                quarks[21] = NrmStringToQuark("opaque");
                quarks[22] = NrmStringToQuark("enum");
                quarks[23] = NrmStringToQuark("vlen");
                quarks[24] = NrmStringToQuark("reference");
                quarks[25] = NrmStringToQuark("none");
	}	

	switch(dt) {
	case NCL_double:
		return(NrmQuarkToString(quarks[0]));
	case NCL_float:
		return(NrmQuarkToString(quarks[1]));
	case NCL_long:
		return(NrmQuarkToString(quarks[2]));
	case NCL_int:
		return(NrmQuarkToString(quarks[3]));
	case NCL_short:
		return(NrmQuarkToString(quarks[4]));
	case NCL_string:
		return(NrmQuarkToString(quarks[5]));
	case NCL_char:
		return(NrmQuarkToString(quarks[6]));
	case NCL_byte:
		return(NrmQuarkToString(quarks[7]));
	case NCL_logical:
		return(NrmQuarkToString(quarks[8]));
	case NCL_obj:
		return(NrmQuarkToString(quarks[11]));
	case NCL_list:
		return(NrmQuarkToString(quarks[12]));
	case NCL_int64:
		return(NrmQuarkToString(quarks[13]));
	case NCL_ushort:
		return(NrmQuarkToString(quarks[14]));
	case NCL_uint:
		return(NrmQuarkToString(quarks[15]));
	case NCL_ulong:
		return(NrmQuarkToString(quarks[16]));
	case NCL_uint64:
		return(NrmQuarkToString(quarks[17]));
	case NCL_group:
		return(NrmQuarkToString(quarks[18]));
	case NCL_compound:
		return(NrmQuarkToString(quarks[19]));
	case NCL_ubyte:
		return(NrmQuarkToString(quarks[20]));
	case NCL_opaque:
		return(NrmQuarkToString(quarks[21]));
	case NCL_enum:
		return(NrmQuarkToString(quarks[22]));
	case NCL_vlen:
		return(NrmQuarkToString(quarks[23]));
	case NCL_reference:
		return(NrmQuarkToString(quarks[24]));
	case NCL_none:
        default:
		return(NrmQuarkToString(quarks[25]));
	}
}

NclBasicDataTypes _nameToNclBasicDataType(NclQuark name)
{
	static int first = 1;
	static NclQuark quarks[26];
	static NclBasicDataTypes nbd_type[26];
	int n;

	if(first) {
		first = 0;
                quarks[0] = NrmStringToQuark("double");
                quarks[1] = NrmStringToQuark("float");
                quarks[2] = NrmStringToQuark("long");
                quarks[3] = NrmStringToQuark("integer");
                quarks[4] = NrmStringToQuark("short");
                quarks[5] = NrmStringToQuark("string");
                quarks[6] = NrmStringToQuark("character");
                quarks[7] = NrmStringToQuark("byte");
                quarks[8] = NrmStringToQuark("logical");
                quarks[9] = NrmStringToQuark("file");
                quarks[10] = NrmStringToQuark("graphic");
                quarks[11] = NrmStringToQuark("obj");
                quarks[12] = NrmStringToQuark("list");
                quarks[13] = NrmStringToQuark("int64");
                quarks[14] = NrmStringToQuark("ushort");
                quarks[15] = NrmStringToQuark("uint");
                quarks[16] = NrmStringToQuark("ulong");
                quarks[17] = NrmStringToQuark("uint64");
                quarks[18] = NrmStringToQuark("group");
                quarks[19] = NrmStringToQuark("compound");
                quarks[20] = NrmStringToQuark("ubyte");
                quarks[21] = NrmStringToQuark("opaque");
                quarks[22] = NrmStringToQuark("enum");
                quarks[23] = NrmStringToQuark("vlen");
                quarks[24] = NrmStringToQuark("reference");
                quarks[25] = NrmStringToQuark("none");

		nbd_type[0] = NCL_double;
		nbd_type[1] = NCL_float;
		nbd_type[2] = NCL_long;
		nbd_type[3] = NCL_int;
		nbd_type[4] = NCL_short;
		nbd_type[5] = NCL_string;
		nbd_type[6] = NCL_char;
		nbd_type[7] = NCL_byte;
		nbd_type[8] = NCL_logical;
		nbd_type[9] = NCL_numeric;
		nbd_type[10] = NCL_snumeric;
		nbd_type[11] = NCL_obj;
		nbd_type[12] = NCL_list;
		nbd_type[13] = NCL_int64;
		nbd_type[14] = NCL_ushort;
		nbd_type[15] = NCL_uint;
		nbd_type[16] = NCL_ulong;
		nbd_type[17] = NCL_uint64;
		nbd_type[18] = NCL_group;
		nbd_type[19] = NCL_compound;
		nbd_type[20] = NCL_ubyte;
		nbd_type[21] = NCL_opaque;
		nbd_type[22] = NCL_enum;
		nbd_type[23] = NCL_vlen;
		nbd_type[24] = NCL_reference;
		nbd_type[25] = NCL_none;
	}	

	for(n = 0; n < 26; n++)
	{
		if(name == quarks[n])
			return (nbd_type[n]);
	}

	return NCL_none;
}

NclBasicDataTypes _NclPromoteType
#if	NhlNeedProto
(NclBasicDataTypes dt)
#else
(dt)
NclBasicDataTypes dt;
#endif
{
	switch(dt) {
	case NCL_double:
		return(NCL_double);
	case NCL_float:
		return(NCL_double);
      /* Change by Wei, 1/6/2009
	case NCL_long:
		return(NCL_double);
       */
	case NCL_long:
		return(NCL_int64);
	case NCL_int64:
		return(NCL_double);
	case NCL_int:
		return(NCL_long);
	case NCL_short:
		return(NCL_int);
        case NCL_uint64:
                return(NCL_double);
        case NCL_ulong:
                return(NCL_uint64);
        case NCL_uint:
                return(NCL_ulong);
        case NCL_ushort:
                return(NCL_uint);
	case NCL_string:
		return(NCL_string);
	case NCL_char:
		return(NCL_string);
	case NCL_byte:
		return(NCL_short);
        case NCL_ubyte:
                return(NCL_ushort);
	default:
		return(NCL_none);
	}
}
struct _NclDataRec* _NclReadSubSection
#if     NhlNeedProto
(struct _NclDataRec *self, struct _NclSelectionRecord *selection, NclScalar *missing)
#else
(self, selection, missing)
struct _NclDataRec *self;
struct _NclSelectionRecord *selection;
NclScalar *missing;
#endif
{
	NclDataClass dc ;
	
	if(self == NULL) {
		return(NULL);
	} else {
		dc  = (NclDataClass)self->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass) {

		if(dc->data_class.r_subsection != NULL) {
			return((*dc->data_class.r_subsection)(self, selection, missing));
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return(NULL);
}

NhlErrorTypes _NclReadThenWriteSubSection
#if     NhlNeedProto
(struct _NclDataRec *to_data, struct _NclSelectionRecord *to_selection, struct _NclDataRec *from_data, struct _NclSelectionRecord* from_selection)
#else
(to_data, to_selection, from_data, from_selection)
struct _NclDataRec *to_data;
struct _NclSelectionRecord *to_selection;
struct _NclDataRec *from_data;
struct _NclSelectionRecord* from_selection;
#endif
{
	NclDataClass dc ;
	
	if(to_data == NULL) {
		return(NhlFATAL);
	} else {
		dc  = (NclDataClass)to_data->obj.class_ptr;
	}
	while((NclObjClass)dc != nclObjClass) {

		if(dc->data_class.r_then_w_subsection != NULL) {
			return((*dc->data_class.r_then_w_subsection)(to_data, to_selection, from_data, from_selection));
		} else {
			dc = (NclDataClass)dc->obj_class.super_class;
		}
	}
	return(NhlFATAL);
}

NhlErrorTypes _NclWriteSubSection
#if     NhlNeedProto
(struct _NclDataRec *self, struct _NclSelectionRecord * selection, struct _NclDataRec *value)
#else
(self, selection, value)
struct _NclDataRec *self;
struct _NclSelectionRecord * selection;
struct _NclDataRec *value;
#endif
{
        NclDataClass oc;
        int f_selection;

        if((self == NULL)||!(self->obj.obj_type_mask & NCL_MD_MASK)) {
                return(NhlFATAL);
        } else {
                oc = (NclDataClass)self->obj.class_ptr;
        }

        f_selection = (int)((NclMultiDValData)value)->multidval.kind;
        while((NclObjClass)oc != nclObjClass) {
                if( oc->data_class.w_subsection[f_selection] != NULL) {
                        return(((*oc->data_class.w_subsection[f_selection])(self, selection, value)));
                } else {
                        oc = (NclDataClass)oc->obj_class.super_class;
                }
        }
        return(NhlFATAL);
}


NhlErrorTypes _NclSwapBytes
#if     NhlNeedProto
(
void *outdata,
void *indata,
ng_size_t  count,
int  type_size
)
#else
(outdata,indata,count,type_size)
void *outdata;
void *indata;
ng_size_t count;
int  type_size;
#endif
{

	/* Note:
	 * if outdata == indata or outdata is NULL then swapping is done in place,
	 * and a temporary character variable is needed.
	 * Otherwise, outdata is assumed to have the same size as indata.
	 */

	if (! outdata || outdata == indata) {
		char *cd = indata;
		char ctmp;

		switch (type_size) {
		case 1:
			return NhlNOERROR;
		case 2:
			while (count-- > 0) {
				ctmp = cd[0];
				cd[0] = cd[1];
				cd[1] = ctmp;
				cd += 2;
			}
			return NhlNOERROR;
		case 4:
			while (count-- > 0) {
				ctmp = cd[0];
				cd[0] = cd[3];
				cd[3] = ctmp;
				ctmp = cd[1];
				cd[1] = cd[2];
				cd[2] = ctmp;
				cd += 4;
			}
			return NhlNOERROR;
		case 8:
			while (count-- > 0) {
				ctmp = cd[0];
				cd[0] = cd[7];
				cd[7] = ctmp;
				ctmp = cd[1];
				cd[1] = cd[6];
				cd[6] = ctmp;
				ctmp = cd[2];
				cd[2] = cd[5];
				cd[5] = ctmp;
				ctmp = cd[3];
				cd[3] = cd[4];
				cd[4] = ctmp;
				cd += 8;
			}
			return NhlNOERROR;
		
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclSwapBytes: non-supported type size, can't continue");
			return(NhlFATAL);
		}
	}
	else {
		char *in = indata;
		char *out = outdata;

		switch (type_size) {
		case 1:
			return NhlNOERROR;
		case 2:
			while (count-- > 0) {
				out[0] = in[1];
				out[1] = in[0];
				out += 2;
				in += 2;
			}
			return NhlNOERROR;
		case 4:
			while (count-- > 0) {
				out[0] = in[3];
				out[1] = in[2];
				out[2] = in[1];
				out[3] = in[0];
				out += 4;
				in += 4;
			}
			return NhlNOERROR;
		case 8:
			while (count-- > 0) {
				out[0] = in[7];
				out[1] = in[6];
				out[2] = in[5];
				out[3] = in[4];
				out[4] = in[3];
				out[5] = in[2];
				out[6] = in[1];
				out[7] = in[0];
				out += 8;
				in += 8;
			}
			return NhlNOERROR;
		
		default:
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclSwapBytes: non-supported type size, can't continue");
			return(NhlFATAL);
		}
	}
		
}

long _Nclstrtol(const char *str, char **endptr)
{

	long tval;
	int i = 0;

	while (isspace(str[i]))
			i++;
	if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
		tval = strtol(str,endptr,16);
	else
		tval = strtol(str,endptr,10);
	return tval;
}

unsigned long _Nclstrtoul(const char *str, char **endptr)
{
        unsigned long tval;
        int i = 0;

        while (isspace(str[i]))
                        i++;
        if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
        {
                tval = strtoul(str,endptr,16);
        }
        else
        {
                tval = strtoul(str,endptr,10);
        }

        return tval;
}

long long _Nclstrtoll(const char *str, char **endptr)
{
        long long tval;
        int i = 0;

        errno = ERANGE;

        while (isspace(str[i]))
                        i++;
        if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
        {
                errno = 0;
                tval = local_strtoll(str,endptr,16);
        }
        else
        {
                errno = 0;
                tval = local_strtoll(str,endptr,10);
        }

        return tval;
}

unsigned long long _Nclstrtoull(const char *str, char **endptr)
{
        unsigned long long tval;
        int i = 0;

        while (isspace(str[i]))
                        i++;
        if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
        {
                tval = strtoull(str,endptr,16);
        }
        else
        {
                tval = strtoull(str,endptr,10);
        }

        return tval;
}

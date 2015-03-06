
/*
 *      $Id: DataSupport.h,v 1.15 2009-11-17 20:10:11 dbrown Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
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
 *	Date:		Tue Dec 28 10:38:48 MST 1993
 *
 *	Description:	
 */
#ifndef _DataSupport_h
#define _DataSupport_h

extern struct _NclMultiDValDataRec *_NclCreateTrue(
#if NhlNeedProto
void
#endif
);
extern struct _NclMultiDValDataRec *_NclCreateFalse(
#if NhlNeedProto
void
#endif
);
extern struct _NclMultiDValDataRec *_NclCreateLMissing(
#if NhlNeedProto
void
#endif
);
extern struct _NclMultiDValDataRec *_NclCreateMissing(
#if NhlNeedProto
void
#endif
);

extern NhlErrorTypes _NclRegisterCallback(
#if	NhlNeedProto
NclObjTypes /* class_type */, 
unsigned int  /* callback_type */,
NclCallBack /* callback_function */,
void * /* call_backdata */
#endif
);

extern NhlErrorTypes _NclCallCallBacks(
#if	NhlNeedProto
NclObj	/* obj */,
unsigned int  /* type*/
#endif
);

extern void * _NclObtainCallData(
#if	NhlNeedProto
NclObj /* obj */,
unsigned int /* type */
#endif
);


extern void _NclInitDataClasses(
#if	NhlNeedProto
void
#endif
);

extern struct _NclMultiDValDataRec *_NclCharMdToStringMd(
#if	NhlNeedProto
struct _NclMultiDValDataRec * /*char_md*/
#endif
);
extern struct _NclMultiDValDataRec *_NclStringMdToCharMd(
#if	NhlNeedProto
struct _NclMultiDValDataRec * /*string_md*/
#endif
);

extern char* _NclBasicDataTypeToName(
#if	NhlNeedProto
NclBasicDataTypes /*dt*/
#endif
);

extern NclBasicDataTypes _nameToNclBasicDataType(NclQuark name);

extern NclBasicDataTypes _NclPromoteType(
#if  	NhlNeedProto
	NclBasicDataTypes /*dt*/
#endif
); 

extern struct _NclMultiDValDataRec * _NclCoerceData(
#if 	NhlNeedProto
struct _NclMultiDValDataRec * /*self*/,
NclObjTypes /* coerce_obj_to */,
NclScalar * /* coerce_obj_to */
#endif
);

extern NhlErrorTypes _NclPrint(
#if	NhlNeedProto
struct _NclObjRec* /* self*/,
FILE * /* fp*/
#endif
);

extern struct _NclDataRec* _NclReadSubSection(
#if	NhlNeedProto
struct _NclDataRec * /*self*/,
struct _NclSelectionRecord * /*selection*/,
NclScalar * /*missing*/
#endif
);

extern NhlErrorTypes _NclReadThenWriteSubSection(
#if	NhlNeedProto
struct _NclDataRec * /*to_data*/,
struct _NclSelectionRecord * /*to_selection*/,
struct _NclDataRec * /*from_data*/,
struct _NclSelectionRecord* /* from_selection*/
#endif
);

extern NhlErrorTypes _NclWriteSubSection(
#if	NhlNeedProto
struct _NclDataRec * /*self*/,
struct _NclSelectionRecord * /*selection*/,
struct _NclDataRec * /*value*/
#endif
);

extern NhlErrorTypes _NclCallMonoOp(
#if	NhlNeedProto
struct _NclMultiDValDataRec* /* operand*/,
NclObj*			/* result*/,
int				/* op */
#endif
);

extern NhlErrorTypes _NclCallDualOp(
#if	NhlNeedProto
struct _NclMultiDValDataRec* /* lhs */,
struct _NclMultiDValDataRec* /* rhs */,
int				/* op */,
NclObj*			/* result*/
#endif
);

extern struct _NclMultiDValDataRec* _NclCopyVal(
#if	NhlNeedProto
struct _NclMultiDValDataRec * /*self*/,
NclScalar *	/* new_missing */
#endif
);

extern void _NclResetMissingValue(
#if	NhlNeedProto
struct _NclMultiDValDataRec * /*self*/,
NclScalar * /* missing*/
#endif
);

extern int _NclScalarCoerce(
#if  NhlNeedProto
void * /*from*/,
NclBasicDataTypes /*frtype*/,
void * /*to*/,
NclBasicDataTypes /*totype*/
#endif
);

extern int _NclScalarForcedCoerce(
#if  NhlNeedProto
void * /*from*/,
NclBasicDataTypes /*frtype*/,
void * /*to*/,
NclBasicDataTypes /*totype*/
#endif
);

struct _NclMultiDValDataRec * _NclCreateVal(
#if     NhlNeedProto
NclObj /*inst*/, 
NclObjClass /*theclass*/, 
NclObjTypes /*obj_type*/, 
unsigned int /*obj_type_mask*/, 
void * /*val*/, 
NclScalar * /*missing_value*/, 
int /*n_dims*/, 
ng_size_t * /*dim_sizes*/, 
NclStatus /*status*/, 
NclSelectionRecord * /*sel_rec*/,
NclObjClass  /*type*/
#endif
);


extern NhlErrorTypes _NclAddParent(
#if NhlNeedProto
NclObj /*theobj*/ ,
NclObj /*parent*/
#endif
);

extern _NhlCB _NclAddCallback(
#if NhlNeedProto
NclObj /* theobj */,
NclObj /* parent */,
_NhlCBFunc /* cbfunc */,
long /* cbsel*/,
NhlArgVal * /*udata*/
#endif
);



extern NhlErrorTypes _NclDelParent(
#if NhlNeedProto
NclObj /*theobj*/,
NclObj /*parent*/
#endif
);


extern NclBasicDataTypes _NclKeywordToDataType(
#if NhlNeedProto
struct _NclSymbol * /*keywd*/
#endif 
);

extern NclObjTypes _NclKeywordToObjType(
#if  NhlNeedProto 
struct _NclSymbol * /*keywd*/
#endif
);

extern int _NclIsMissing(
#if NhlNeedProto 
NclMultiDValData self, void* val
#endif
);

extern int _NclGetObjRefCount(
#if NhlNeedProto
int /*the_id*/
#endif
);

extern  struct _NclTypeClassRec *_NclNameToTypeClass(
#if  NhlNeedProto
NclQuark /*obj_type*/
#endif
);

extern  long _NclObjTypeToName(
#if  NhlNeedProto
NclObjTypes /*obj_type*/
#endif
);

extern void _NclRegisterClassPointer(
NclObjTypes /*obj_type*/,
NclObjClass /*obj_calss*/
);

extern NclObjClass  _NclObjTypeToPointer(
#if NhlNeedProto
NclObjTypes /*obj_type*/
#endif
);

extern NclObjTypes _NclBasicDataTypeToObjType(
#if NhlNeedProto
NclBasicDataTypes /*dt*/
#endif
);

extern NhlErrorTypes _NclGetCoordRange(
#if	NhlNeedProto
NclMultiDValData /* coord_md*/,
void *		/* start_md */,
void *		/* finish_md */,
long*		 /* start */,
long*		 /* finish */
#endif
);

extern NhlErrorTypes _NclGetCoordClosestIndex(
#if	NhlNeedProto
NclMultiDValData /* coord_md*/,
void *		/* ind_md */,
long*		 /* ind*/
#endif
);

extern NhlErrorTypes _NclSwapBytes(
#if     NhlNeedProto
void *outdata,
void *indata,
ng_size_t count,
int  type_size
#endif
);

extern long long local_strtoll(const char *nptr, char **endptr, int base);
extern long _Nclstrtol(const char *str, char **endptr);
extern unsigned long _Nclstrtoul(const char *str, char **endptr);
extern long long _Nclstrtoll(const char *str, char **endptr);
extern unsigned long long _Nclstrtoull(const char *str, char **endptr);

#endif /*_DataSupport_h */



/*
 *      $Id: WSymbol.h,v 1.1 1998-03-27 23:37:34 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		Symbol.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Mon Jun 28 10:12:49 MDT 1993
 *
 *	Description:	Type definitions for symbol table entries.
 */
#ifdef __cplusplus
extern "C" {
#endif

#ifndef _NCSymbol_h
#define _NCSymbol_h
#define MAX_WRAP_ARGS 128
#define BUFFSIZE 1024

typedef struct _NclVarInfo {
	char	varname[NCL_MAX_STRING];
	int datatype;
	unsigned int offset;
}NclVarInfo; 

#define ANYDIMSIZE = -1
typedef NhlErrorTypes (*NclBuiltInProcWrapper)(
#if	NhlNeedProto
	void
#endif
);
typedef NhlErrorTypes (*NclIntrinsicProcWrapper)(
#if	NhlNeedProto
	void	
#endif
);
typedef NhlErrorTypes (*NclBuiltInFuncWrapper)(
#if	NhlNeedProto
	void
#endif
);

typedef struct _WFargInfo  { 
	int datatype;
	int n_dims;
	int arg_num;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	struct _NclSymbol *dim_refs[NCL_MAX_DIMENSIONS];
}WFargInfo;

typedef struct _WParamInfo {
	int datatype;
	int n_dims;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	struct getargvalrec *getarg;
} WParamInfo;

typedef struct _WSubrInfo {
	int n_args;
	struct _NclSymbol **args;
} WSubrInfo;

typedef struct _WFuncInfo {
	int datatype;
	int n_args;
	struct _NclSymbol **args;
} WFuncInfo;


typedef struct _WCentry {
	int datatype;
	int order;
	char *string;
	int isptr;
	char *array_spec;
	struct srclist *additional_src;
} WCentry;

typedef struct _Wparamvalinfo {
	struct _NclSymbol *wsym;
} WParamValInfo;

typedef struct _Wtotalparamvalinfo {
	struct _NclSymbol *wsym;
} WTotalParamValInfo;

typedef struct _Wdimsparamvalinfo {
	struct _NclSymbol *wsym;
} WDimsParamValInfo;

typedef struct _Wdimindexparamvalinfo {
	struct _NclSymbol *wsym;
	int index;
} WDimIndexParamValInfo;

typedef struct _Wfargnewinfo  {
	struct _WCentry *cdef;
	struct _WDSpecList *dspec;
	int nentries;
} Wfargnewinfo;

typedef enum _types { FARGNEW ,PARAMVAL ,TOTALPARAM ,DIMSPARAM ,DIMINDEXPARAM,RETURNPARAM } WTypes;

typedef enum _dspec_types { INTDIMSIZES,PARAMVALDIMSIZES,TOTALPARAMDIMSIZES,DIMSPARAMDIMSIZES,DIMINDEXPARAMDIMSIZES } WDSpecTypes;


typedef struct _WDSpecList {
	WDSpecTypes type;
	union {
	        struct _Wparamvalinfo pval;
                struct _Wtotalparamvalinfo tpval;
                struct _Wdimsparamvalinfo  dpval;
                struct _Wdimindexparamvalinfo dipval;
		int val;
	}u;
	struct _WDSpecList *next;
} WDSpecList;

typedef struct _WParamLoc {
	WTypes type;
	struct _NclSymbol *xfarg;
	int datatype;
	int typeofdim;
	int n_dims;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	char dim_refs[NCL_MAX_DIMENSIONS][80];
	struct _WCentry *altdimsref;
	char altndimref[80];
	struct _WCentry *cdef;
	char call_string[80];
	struct _NclSymbol *wsym;
} WParamLoc;

typedef enum _modtypes { DIMSIZESOF, MISSINGOF, ATTOF, COORDOF, INONLY,OUTONLY,INOUTONLY,SETTYPE, TYPEOF } WModTypes;

typedef struct _Wdimsof{
	struct _NclSymbol *sym;
	int index;
	int kind;
} WDimsOf;

typedef struct _Wtypeof{
	int datatype;
} WTypeOf;

typedef struct _WModList{
	WModTypes type;
	union {
		struct _Wdimsof dims;
		struct _Wtypeof type;
/********
		struct _Wmissmod missing;
		struct _Wattmod att;
		struct _Wcoordof coord;
		struct _Winout inout;
		struct _Wsettype settype;
********/
	} u;
	struct _WModList *next;
} WModList;

	

typedef struct _NclSymbol {
	int type;
	int ind;
	int level;
	char name[NCL_MAX_STRING];
	unsigned int offset;
	union {
		struct _WFargInfo		*farg;
		struct _WParamInfo		*wparam;
		struct _NclSymbol		*xref;
		struct _WSubrInfo		*subr;
		struct _WFuncInfo		*func;
		struct _WCentry		*centry;
	} u;
	struct _NclSymbol *symnext;
	struct _NclSymbol *sympre;
} NclSymbol;

typedef struct _NclSymTableElem {
	int nelem;
	struct _NclSymbol *thelist;
} NclSymTableElem;

typedef struct _NclScopeRec {
	int level;
	int cur_offset;
	struct _NclSymTableElem *this_scope;
} NclScopeRec; 
typedef struct _NclSymTableListNode {
	struct _NclScopeRec *sr;
	struct _NclSymTableListNode *previous;
}NclSymTableListNode;

typedef struct vdeflist {
	struct _WCentry *def;
	struct vdeflist *next;
} WVDefList;

typedef struct srclist {
	char *src;	
	int order;
	struct srclist *next;
} WSrcList;

typedef struct getargvalrec{
	char *assign_to;
	int datatype;
	int pnum;
	int totalp;
	char *ndims;
	char *dimsizes;
	char *missing;
	char *hasmissing;
	char *type;
	int rw;
	char nd[5];
	WSrcList *additional_src;
	int additional_src_flags;
}WGetArgValue;


typedef struct tmpvallist {
	struct _NclSymbol *tmpval;
	WSrcList *tv_src;
	struct tmpvallist *next;
}WTempValList;

typedef struct argvallist {
	WGetArgValue *arec;
	struct argvallist *next;
}WArgValList;


typedef struct callrec {
	char *callstring;
	int n_args;
	int nstrs;
	struct _WCentry **arg_strings;
}WCallRec;

typedef struct _WWrapRec {
	NclScopeRec *frec;
	NclScopeRec *wrec;
	NclScopeRec *crec;
	int f_or_p;
	char *c_defstring;
	WVDefList *c_vdefs;
	WArgValList *c_argval;
	WTempValList *c_tmpval;
	WCallRec *c_callrec;
        WSrcList *cleanup_src;
	char* rtrn;
	
} WWrapRec;

typedef struct _WWrapRecList {
	WWrapRec *therec;
	struct _WWrapRecList *next;
}WWrapRecList;

extern NclSymbol *_NclLookUpInScope(
#if	NhlNeedProto
NclScopeRec* /*thetable*/,
char			* /*name*/
#endif
);
typedef struct _FTypeVal{
	int datatype;
	int size;
}FTypeVal;
typedef struct _DimVal{
	int kind;
	union {
		NclSymbol *sym;
		int val;
	} u;
}DimVal;
extern NclSymbol *_NclAddInScope(
#if	NhlNeedProto
	NclScopeRec */*thetable*/,
	char			* /* name */,
	int			  /* type */
#endif
);

extern void _NclDeleteSymInScope(
#if	NhlNeedProto
NclScopeRec* /*thetable*/,
NclSymbol * /*sym*/
#endif
);

#define ADDSRC_TOTAL 01
#define ADDSRC_DIMSOF 02

#endif /*_NCSymbol_h*/
#ifdef __cplusplus
}
#endif

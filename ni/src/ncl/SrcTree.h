
/*
 *      $Id: SrcTree.h,v 1.7 1994-03-03 21:54:35 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1993			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		SrcTree.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri Jul 9 13:15:55 MDT 1993
 *
 *	Description:	This file contains the declarations of the functions
 *			and data structures for creating the intermediate 
 *			srctree representation.
 */
#ifdef __cplusplus
extern "C" {
#endif
#ifndef _NCSrcTree_h
#define _NCSrcTree_h

typedef enum {Ncl_BLOCK, Ncl_RETURN, Ncl_IFTHEN, Ncl_IFTHENELSE,
			Ncl_VISBLKSET, Ncl_VISBLKGET, Ncl_VISBLKCREATE, 
			Ncl_DOFROMTO, Ncl_DOFROMTOSTRIDE, 
			Ncl_BUILTINPROCCALL,Ncl_INTRINSICPROCCALL, Ncl_EXTERNALPROCCALL, 
			Ncl_PROCCALL, Ncl_FUNCDEF, Ncl_EXTERNFUNCDEF,	
			Ncl_LOCALVARDEC, Ncl_DIMSIZELISTNODE, Ncl_PROCDEF,
			Ncl_EXTERNPROCDEF, Ncl_ASSIGN, Ncl_IDNREF,
			Ncl_INTSUBSCRIPT, Ncl_COORDSUBSCRIPT, Ncl_SINGLEINDEX,
			Ncl_RANGEINDEX, Ncl_NEGEXPR, Ncl_NOTEXPR, Ncl_MODEXPR,
			Ncl_OREXPR, Ncl_ANDEXPR, Ncl_XOREXPR, Ncl_LTSELECTEXPR,
			Ncl_GTSELECTEXPR, Ncl_PLUSEXPR, Ncl_MINUSEXPR,
			Ncl_MULEXPR, Ncl_MATMULEXPR, Ncl_DIVEXPR,
			Ncl_EXPEXPR, Ncl_LEEXPR, Ncl_GEEXPR, Ncl_GTEXPR,
			Ncl_LTEXPR, Ncl_EQEXPR, Ncl_NEEXPR, Ncl_REAL,
			Ncl_INT, Ncl_STRING, Ncl_BUILTINFUNCCALL, Ncl_INTRINSICFUNCCALL,
			Ncl_EXTERNFUNCCALL, Ncl_FUNCCALL, Ncl_ARRAY,
			Ncl_ROWLIST,Ncl_ROWCOLUMNNODE, Ncl_DOWHILE,
			Ncl_VAR, Ncl_VARDIM, Ncl_VARATT,
			Ncl_VARCOORD, Ncl_FILEVAR, Ncl_IDNEXPR, 
			Ncl_RESOURCE, Ncl_GETRESOURCE, Ncl_OBJ,
			Ncl_BREAK, Ncl_CONTINUE, Ncl_FILEVARATT,
			Ncl_FILEVARDIM,  Ncl_FILEVARCOORD
                        } NclSrcTreeTypes;

typedef enum { Ncl_READIT, Ncl_WRITEIT, Ncl_PARAMIT } NclReferenceTypes;

typedef struct ncl_genericnode NclGenericNode, NclBreak, NclContinue;
typedef struct ncl_genericrefnode NclGenericRefNode;

typedef void (*NclSrcTreeDestroyProc)(
#if 	NhlNeedProto
	NclGenericNode* /*thenode*/
#endif
);

struct ncl_genericnode{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
};

struct ncl_genericrefnode {
	NclSrcTreeTypes kind;
        char *name;
        int  line;
        char *file;
        NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
};

typedef struct src_node_list {
        void *node;
        struct src_node_list *next;
}NclSrcListNode;

typedef struct ncl_rcl_list {
        int nelem;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
        struct src_node_list *list;
        struct src_node_list *currentitem;
}NclRclList;


/* Start constructs that are ncl_genericrefnodes */

typedef struct ncl_funccall{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *func;
	NclSrcListNode *arg_list;
} NclFuncCall; 

typedef struct ncl_array{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	struct ncl_rcl_list *rcl;
} NclArray;

typedef struct ncl_string{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	char *string;
} NclString; 

typedef struct ncl_int {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	int integer;
} NclInt;

typedef struct ncl_real{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	float real;
} NclReal;

typedef struct ncl_filevar {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *dfile;
	char *filevar;
	NclSrcListNode *subscript_list;
}NclFileVar;

typedef struct ncl_filevardim{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *filesym;
	char *filevar;
	void *dim_expr;
}NclFileVarDim;

typedef struct ncl_vardim{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *sym;
	void *dim_expr;
}NclVarDim;

typedef struct ncl_filevaratt{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *filesym;
	char *filevar;
	char *attname;
	NclSrcListNode *subscript_list;
}NclFileVarAtt;

typedef struct ncl_varatt{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *sym;
	char *attname;
	NclSrcListNode *subscript_list;
}NclVarAtt;

typedef struct ncl_var{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *sym;
	NclSrcListNode *subscript_list;
}NclVar;

typedef struct ncl_filecoord {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *filesym;
	char *filevar;
	char *coord_name;
	NclSrcListNode *subscript_list;
}NclFileCoord;

typedef struct ncl_coord {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *sym;
	char *coord_name;
	NclSrcListNode *subscript_list;
}NclCoord;

/* End of structures that are of type ncl_genericrefnode */

typedef struct ncl_block{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSrcListNode *stmnts;
} NclBlock; 

typedef struct ncl_visblk{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *objname;
	void *objtype;
	NclSrcListNode *resource_list;
}NclVisblk;

typedef struct ncl_resource{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	NclSrcTreeDestroyProc destroy_it;
	char *file;
	char *res_name;
	void *expr;
}NclResource;

typedef struct ncl_getresource{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	char *res_name;
	NclSymbol *var;
}NclGetResource;

typedef struct ncl_return{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *expr;
} NclReturn; 

typedef struct ncl_ifthen{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *cond_expr;
	NclSrcListNode *block_stmnt_list;
} NclIfThen; 

typedef struct ncl_ifthenelse{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *cond_expr;
	NclSrcListNode *block_stmnt_list1;
	NclSrcListNode *block_stmnt_list2;
} NclIfThenElse;

typedef struct ncl_dofromto {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	int	new_inc_var;
	void *inc_var;
	void	*start_expr;
	void    *end_expr;
	NclSrcListNode *block_stmnt_list;
} NclDoFromTo;

typedef struct ncl_dofromtostride{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	int new_inc_var;
	void *inc_var;
	void	*start_expr;
	void    *end_expr;
	void    *stride_expr;
	NclSrcListNode *block_stmnt_list;
} NclDoFromToStride;

typedef struct ncl_proccall{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol *proc;
	NclSrcListNode *arg_list;
} NclProcCall;

typedef struct ncl_funcdef{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol	*func;
	NclSrcListNode  *dec_list;
	void		*block;
	NclSymTableListNode *scope;
} NclFuncDef; 

typedef struct ncl_externfuncdef {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol	*func;
	NclSrcListNode  *dec_list;
	char *path_info_string;
	NclSymTableListNode *scope;
} NclExternFuncDef;	

typedef struct ncl_localvardec{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol *var;
	NclSrcListNode *dim_size_list;
	NclSymbol *data_type;
} NclLocalVarDec;

typedef struct ncl_dimsizelistnode{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	int   any;
	int   size;
} NclDimSizeListNode; 

typedef struct ncl_procdef{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol *proc;
	NclSrcListNode *dec_list;
	void	*block;
	NclSymTableListNode *scope;
} NclProcDef;

typedef struct ncl_externprocdef{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol * proc;
	NclSrcListNode *dec_list;
	char *path_info_string;
	NclSymTableListNode *scope;
} NclExternProcDef; 

typedef struct ncl_assign{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	int  new_left;
	void *left_side;
	void *right_side;
} NclAssign; 


typedef struct ncl_idnref{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *thename;
	NclSrcListNode *subscript_list;
} NclIdnRef;

typedef struct ncl_subscript{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *subexpr;
	char *dimname;
} NclSubscript; 

typedef struct ncl_singleindex{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *expr;
} NclSingleIndex;

typedef struct ncl_rangeindex{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *start_expr;
	void *end_expr;	
	void *stride;
} NclRangeIndex; 

typedef struct ncl_monoexpr{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *expr;
} NclMonoExpr; 

typedef struct ncl_idnexpr {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *idn_ref_node;
} NclIdnExpr;

typedef struct ncl_dualexpr{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *left_expr;
	void *right_expr;
} NclDualExpr; 

typedef struct ncl_dowhile{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *cond_expr;
	NclSrcListNode *stmnts;
} NclDoWhile;
/*
typedef struct ncl_obj{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol *obj;
}NclObj;
*/

extern void *_NclMakeReturn(
#ifdef NhlNeedProto
void *	/*return_expr*/
#endif
);

extern void *_NclMakeIfThen(
#ifdef NhlNeedProto
void * /*conditional_expr*/,
NclSrcListNode * /*block_stmnt_list*/
#endif
);

extern void *_NclMakeIfThenElse(
#ifdef NhlNeedProto
void * /*conditional_expr*/,
NclSrcListNode * /*block_stmnt_list1*/,
NclSrcListNode * /*block_stmnt_list2*/
#endif
);

extern void *_NclMakeVis(
#ifdef NhlNeedProto
void * /*objname*/,
void */*objtype*/,
NclSrcListNode *  /*objtype*/,
NclSrcTreeTypes /*nodetype*/
#endif
);

extern NclSrcListNode *_NclMakeNewListNode(
#ifdef NhlNeedProto
void
#endif
);

extern void *_NclMakeDoFromTo(
#ifdef NhlNeedProto
void* /*var*/,
void *	/*start_expr*/,
void *  /*end_expr*/,
NclSrcListNode * /*block_stmnt_list */
#endif
);

extern void *_NclMakeDoFromToStride(
#ifdef NhlNeedProto
void* /*var*/,
void *	/*start_expr*/,
void *  /*end_expr*/,
void *  /*stride_expr*/,
NclSrcListNode * /*block_stmnt_list */
#endif
);

extern void *_NclMakeProcCall(
#ifdef NhlNeedProto
NclSymbol * /* proc */,
NclSrcListNode * /* arg_list */,
NclSrcTreeTypes /* type */
#endif
);

extern void *_NclMakeNFunctionDef(
#ifdef NhlNeedProto
NclSymbol * /* func */,
NclSrcListNode * /*dec_list*/,
void*		/* block */,
NclSymTableListNode * /*scope*/
#endif
);

extern void* _NclMakeEFunctionDef(
#ifdef NhlNeedProto
NclSymbol * /* func */,
NclSrcListNode * /*dec_list*/,
char *		/* path_info_string */,
NclSymTableListNode * /*scope*/
#endif
);

extern void* _NclMakeLocalVarDec(
#ifdef NhlNeedProto
NclSymbol* /* var */,
NclSrcListNode * /*dim_size_list */,
NclSymbol*	/* param_type */
#endif
);

extern void * _NclMakeDimSizeNode(
#ifdef NhlNeedProto
int /* size */
#endif
);

extern void * _NclMakeProcDef(
#ifdef NhlNeedProto
NclSymbol * /*var*/,
NclSrcListNode * /*arg_list */,
void*	/*block*/,
NclSymTableListNode * /*thescope*/
#endif
);

extern void* _NclMakeExternalProcDef(
#ifdef NhlNeedProto
NclSymbol * /* var */,
NclSrcListNode * /*dec_list */,
char* /*path_info_string*/,
NclSymTableListNode * /*thescope*/
#endif
);

extern void* _NclMakeAssignment(
#ifdef NhlNeedProto
void * /*name_ref */,
void * /*expr */
#endif
);

extern void* _NclMakeIdnRef(
#ifdef NhlNeedProto
void * /* name */,
NclSrcListNode * /* subscript_list */
#endif
);

extern void* _NclMakeIntSubscript(
#ifdef NhlNeedProto
void * /* subexpr */,
char * /* dimname */
#endif
);

extern void* _NclMakeCoordSubscript(
#ifdef NhlNeedProto
	void * /*subexpr*/,
	char * /* dimname */
#endif
);

extern void* _NclMakeSingleIndex(
#ifdef NhlNeedProto
	void * /*expr*/
#endif
);

extern void* _NclMakeRangeIndex(
#ifdef NhlNeedProto
	void * /*start_expr*/,
	void * /*end_expr */,
	void * /*stride */
#endif
);

extern void * _NclMakeUnaryExpr(
#ifdef NhlNeedProto
	void * /*expr */,
	NclSrcTreeTypes /* type */
#endif
);	

extern void *_NclMakeIdnExpr(
#ifdef NhlNeedProto
	void * /*idn_ref_node*/
#endif
);

extern void * _NclMakeExpr(
#ifdef NhlNeedProto
	void * /* left_expr */,
	void * /* right_expr */,
	NclSrcTreeTypes /* type */
#endif
);

extern void * _NclMakeRealExpr(
#ifdef NhlNeedProto
	float /*real*/
#endif
);

extern void * _NclMakeIntExpr(
#ifdef NhlNeedProto
	int /* integer */
#endif
);

extern void * _NclMakeStringExpr(
#ifdef NhlNeedProto
	char * /* string */
#endif
);

extern void * _NclMakeFuncCall(
#ifdef NhlNeedProto
	NclSymbol * /*fname*/,
	NclSrcListNode * /*argument_list*/,
	NclSrcTreeTypes  /*type*/
#endif
);

extern void *_NclMakeArrayNode(
#ifdef NhlNeedProto
	NclRclList* /*rc_list*/
#endif
);

extern NclRclList *_NclMakeRowList(
#ifdef NhlNeedProto
	void
#endif
);

extern void *_NclMakeWhile(
#ifdef NhlNeedProto
	void * /*cond_expr*/,
	NclSrcListNode * /*statements*/
#endif
);

extern void *_NclMakeBlock(
#ifdef NhlNeedProto
	NclSrcListNode * /* statements */
#endif
);

extern void *_NclMakeFileRef(
#ifdef NhlNeedProto
	NclSymbol * /* dfile */
#endif
);

extern void *_NclMakeFileVarRef(
#ifdef NhlNeedProto
	NclSymbol * /* dfile */,
	char * /* filevar */,
	NclSrcListNode * /* subscript_list */,
	int /*type*/
#endif
);



extern void *_NclMakeFileAttRef(
#ifdef NhlNeedProto
	NclSymbol * /* dfile */,	
	char *	    /* attname */,
	NclSrcListNode * /* subscript_list */
#endif
);

extern void *_NclMakeVarRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	NclSrcListNode * /* subscript_list */
#endif
);	

extern void *_NclMakeVarDimRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	void*	/* dimnumexpr */
#endif
);
extern void *_NclMakeFileVarDimRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	char *	/*filevar*/,
	void*	/* dimexpr */
#endif
);

extern void *_NclMakeFileVarAttRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	char * /* filevar */,
	char * /* attname */,
	NclSrcListNode * /*subscript_list*/
#endif
);
extern void *_NclMakeVarAttRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	char * /* attname */,
	NclSrcListNode * /*subscript_list*/
#endif
);

extern void *_NclMakeFileVarCoordRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	char * /* filevar*/,
	char * /* coord */,
	NclSrcListNode * /* subscript_list */
#endif
);
extern void *_NclMakeVarCoordRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */,
	char * /* coord */,
	NclSrcListNode * /* subscript_list */
#endif
);


extern void *_NclMakeUndefErrorRef(
#ifdef NhlNeedProto
	NclSymbol * /* var */
#endif
);

extern void *_NclMakeGetResource(
#ifdef NhlNeedProto
	char * /*resname*/,
	NclSymbol * /*var */
#endif
);
extern void *_NclMakeResource(
#ifdef NhlNeedProto
	char * /*resname*/,
	void* /*expr*/
#endif
);
/*
extern void *_NclMakeObjRef(
#ifdef NhlNeedProto
	NclSymbol * obj
#endif
);
*/
extern void *_NclMakeEoln(
#ifdef NhlNeedProto
void
#endif
);

extern void *_NclMakeBreakCont(
#ifdef NhlNeedProto
NclSymbol * /*thesym*/
#endif
);

extern void _NclPrintTree(
#ifdef NhlNeedProto
void * /*root*/,
FILE * /*fp*/
#endif
);

extern void _NclFreeTree(
#ifdef NhlNeedProto
void 
#endif
);



#endif /*_NCSrcTree_h*/
#ifdef __cplusplus
}
#endif

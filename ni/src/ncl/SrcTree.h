
/*
 *      $Id: SrcTree.h,v 1.28 2009-02-05 03:42:32 dbrown Exp $
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
			Ncl_INTRINSICPROCCALL, Ncl_EXTERNALPROCCALL, 
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
			Ncl_INT, Ncl_STRING, Ncl_INTRINSICFUNCCALL,
			Ncl_EXTERNFUNCCALL, Ncl_FUNCCALL, Ncl_ARRAY,
			Ncl_ROWLIST,Ncl_ROWCOLUMNNODE, Ncl_DOWHILE,
			Ncl_VAR, Ncl_VARDIM, Ncl_VARATT,
			Ncl_VARCOORD, Ncl_FILEVAR, Ncl_IDNEXPR, 
			Ncl_RESOURCE, Ncl_GETRESOURCE, Ncl_OBJ,
			Ncl_BREAK, Ncl_CONTINUE, Ncl_FILEVARATT,
			Ncl_FILEVARDIM,  Ncl_FILEVARCOORD, Ncl_NEW,
			Ncl_LOGICAL, Ncl_VARCOORDATT,Ncl_FILEVARCOORDATT,Ncl_WILDCARDINDEX, 
	                Ncl_NULLNODE, Ncl_LIST, Ncl_EXPRNEW, Ncl_FILEVARLIST
                        } NclSrcTreeTypes;

typedef enum { Ncl_READIT, Ncl_WRITEIT, Ncl_PARAMIT, Ncl_VALONLY } NclReferenceTypes;

typedef struct ncl_genericnode NclGenericNode, NclBreak, NclContinue,NclWildCardIndex;
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
typedef struct ncl_proccall{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol *proc;
	NclSrcListNode *arg_list;
} NclProcCall;

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
/*
	char *string;
*/
	int  string_q;
} NclString; 

typedef struct ncl_int {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	long long integer;
	char int_type;
	int len;
} NclInt;

typedef struct ncl_real{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	double real;
	int is_double;
	int total_len;
	int len_after_dec;
} NclReal;

typedef struct ncl_filevar {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *dfile;
/*
	char *filevar;
	int  filevar_q;
*/
	void *filevarnode;
	
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
/*
	char *filevar;
	int filevar_q;
*/
	void *filevarnode;
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
/*
	char *filevar;

	int filevar_q;

	char *attname;
*/
	void *filevarnode;
	void *attnamenode;
/*
	int attname_q;
*/
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
/*
	char *attname;
*/
	void *attnamenode;
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

typedef struct ncl_list{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *sym;
	NclSymbol *tmp;
	NclSymbol *tmp_var;
	void *agg_subscript;
	void *ref_node;
	void *subscript_list;
}NclList;

typedef struct ncl_filevar_list{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *list;
	void *list_subscript;
	void *filevar;
	NclSrcListNode *filevar_subscript;
}NclFileVarList;

typedef struct ncl_filecoord_att {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *filesym;
	void *filevarnode;
	void *coordnamenode;
	void *attnamenode;
	NclSrcListNode *subscript_list;
}NclFileCoordAtt;
typedef struct ncl_filecoord {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *filesym;
	void *filevarnode;
	void *coordnamenode;
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
	void *coordnamenode;
	NclSrcListNode *subscript_list;
}NclCoord;

typedef struct ncl_coord_att {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclReferenceTypes ref_type;
	NclSymbol *sym;
	void *coordnamenode;
	void *attnamenode;
	NclSrcListNode *subscript_list;
}NclCoordAtt;
/* End of structures that are of type ncl_genericrefnode */

typedef struct ncl_block{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSrcListNode *stmnts;
} NclBlock; 

typedef struct ncl_sgvisblk{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *objname;
	NclSrcListNode *resource_list;
}NclSGVisblk;

typedef struct ncl_visblk{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *obj_name_expr;
	NclSymbol *objtype;
	void *objparent;
	NclSrcListNode *resource_list;
}NclVisblk;

typedef struct ncl_resource{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
/*
	char *res_name;
*/
	void *resexpr;
	void *expr;
}NclResource;

typedef struct ncl_getresource{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
/*
	char *res_name;
*/
	void *resexpr;
	void *target_idn;
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
	NclSymbol *end_sym;
	NclSymbol *dir_sym;
	NclSymbol *inc_sym;
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
	NclSymbol *end_sym;
	NclSymbol *dir_sym;
	NclSymbol *inc_sym;
} NclDoFromToStride;


typedef struct ncl_funcdef{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol	*func;
	NclSrcListNode  *dec_list;
	void		*block;
	NclScopeRec *scope;
} NclFuncDef; 

typedef struct ncl_externfuncdef {
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol	*func;
	NclSrcListNode  *dec_list;
/*
	char *path_info_string;
*/
	int path_info_string_q;
	NclScopeRec *scope;
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
	NclScopeRec *scope;
} NclProcDef;

typedef struct ncl_externprocdef{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	NclSymbol * proc;
	NclSrcListNode *dec_list;
/*
	char *path_info_string;
*/
	int path_info_string_q;
	NclScopeRec *scope;
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
/*
	char *dimname;
	int dimname_q;
*/
	void *dimname_expr;
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
typedef struct ncl_newnode{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *size_expr;
	NclSymbol *data_sym;
	void *missing_expr;
}NclNew;
typedef struct ncl_newexrnode{
	NclSrcTreeTypes kind;
	char *name;
	int  line;
	char *file;
	NclSrcTreeDestroyProc destroy_it;
	void *size_expr;
	void *data_type_expr;
	void *missing_expr;
}NclExprNew;

extern void *_NclMakeNULLNode(
#if	NhlNeedProto
void
#endif
);

extern void *_NclMakeReturn(
#if	NhlNeedProto
void *	/*return_expr*/
#endif
);

extern void *_NclMakeIfThen(
#if	NhlNeedProto
void * /*conditional_expr*/,
NclSrcListNode * /*block_stmnt_list*/
#endif
);

extern void *_NclMakeIfThenElse(
#if	NhlNeedProto
void * /*conditional_expr*/,
NclSrcListNode * /*block_stmnt_list1*/,
NclSrcListNode * /*block_stmnt_list2*/
#endif
);

extern void *_NclMakeSGVis(
#if	NhlNeedProto
void * /*objname*/,
NclSrcListNode *  /*objtype*/,
NclSrcTreeTypes /*nodetype*/
#endif
);
extern void *_NclMakeVis(
#if	NhlNeedProto
void * /*obj_name_expr*/,
NclSymbol* /*objtype*/,
void */*objparent*/,
NclSrcListNode *  /*objtype*/,
NclSrcTreeTypes /*nodetype*/
#endif
);

extern NclSrcListNode *_NclMakeNewListNode(
#if	NhlNeedProto
void
#endif
);

extern void *_NclMakeDoFromTo(
#if	NhlNeedProto
void* /*var*/,
void *	/*start_expr*/,
void *  /*end_expr*/,
NclSrcListNode * /*block_stmnt_list */
#endif
);

extern void *_NclMakeDoFromToStride(
#if	NhlNeedProto
void* /*var*/,
void *	/*start_expr*/,
void *  /*end_expr*/,
void *  /*stride_expr*/,
NclSrcListNode * /*block_stmnt_list */
#endif
);

extern void *_NclMakeProcCall(
#if	NhlNeedProto
NclSymbol * /* proc */,
NclSrcListNode * /* arg_list */,
NclSrcTreeTypes /* type */
#endif
);

extern void *_NclMakeNFunctionDef(
#if	NhlNeedProto
NclSymbol * /* func */,
NclSrcListNode * /*dec_list*/,
void*		/* block */,
NclScopeRec * /*scope*/
#endif
);

extern void* _NclMakeEFunctionDef(
#if	NhlNeedProto
NclSymbol * /* func */,
NclSrcListNode * /*dec_list*/,
char *		/* path_info_string */,
NclScopeRec* /*scope*/
#endif
);

extern void* _NclMakeLocalVarDec(
#if	NhlNeedProto
NclSymbol* /* var */,
NclSrcListNode * /*dim_size_list */,
NclSymbol*	/* param_type */
#endif
);

extern void * _NclMakeDimSizeNode(
#if	NhlNeedProto
long long /* size */
#endif
);

extern void * _NclMakeProcDef(
#if	NhlNeedProto
NclSymbol * /*var*/,
NclSrcListNode * /*arg_list */,
void*	/*block*/,
NclScopeRec* /*thescope*/
#endif
);

extern void* _NclMakeExternalProcDef(
#if	NhlNeedProto
NclSymbol * /* var */,
NclSrcListNode * /*dec_list */,
char* /*path_info_string*/,
NclScopeRec * /*thescope*/
#endif
);

extern void* _NclMakeAssignment(
#if	NhlNeedProto
void * /*name_ref */,
void * /*expr */
#endif
);

extern void* _NclMakeIdnRef(
#if	NhlNeedProto
void * /* name */,
NclSrcListNode * /* subscript_list */
#endif
);

extern void* _NclMakeIntSubscript(
#if	NhlNeedProto
void * /* subexpr */,
void * /* dimname */
#endif
);

extern void* _NclMakeCoordSubscript(
#if	NhlNeedProto
	void * /*subexpr*/,
	void * /* dimname */
#endif
);

extern void* _NclMakeSingleIndex(
#if	NhlNeedProto
	void * /*expr*/
#endif
);

extern void* _NclMakeWildCardIndex(
#if	NhlNeedProto
void
#endif
);
extern void* _NclMakeRangeIndex(
#if	NhlNeedProto
	void * /*start_expr*/,
	void * /*end_expr */,
	void * /*stride */
#endif
);

extern void * _NclMakeUnaryExpr(
#if	NhlNeedProto
	void * /*expr */,
	NclSrcTreeTypes /* type */
#endif
);	

extern void *_NclMakeIdnExpr(
#if	NhlNeedProto
	void * /*idn_ref_node*/
#endif
);

extern void * _NclMakeExpr(
#if	NhlNeedProto
	void * /* left_expr */,
	void * /* right_expr */,
	NclSrcTreeTypes /* type */
#endif
);

extern void * _NclMakeRealExpr(
#if	NhlNeedProto
	double /*real*/,
	char * /*string_rep*/
#endif
);

extern void * _NclMakeLogicalExpr(
#if	NhlNeedProto
	int /* integer */,
	char * /*string_rep*/
#endif
);
extern void * _NclMakeIntExpr(
#if	NhlNeedProto
	long long /* integer */,
	char * /*string_rep*/
#endif
);

extern void * _NclMakeStringExpr(
#if	NhlNeedProto
	char * /* string */
#endif
);

extern void * _NclMakeFuncCall(
#if	NhlNeedProto
	NclSymbol * /*fname*/,
	NclSrcListNode * /*argument_list*/,
	NclSrcTreeTypes  /*type*/
#endif
);

extern void *_NclMakeArrayNode(
#if	NhlNeedProto
	NclRclList* /*rc_list*/
#endif
);

extern NclRclList *_NclMakeRowList(
#if	NhlNeedProto
	void
#endif
);

extern void *_NclMakeWhile(
#if	NhlNeedProto
	void * /*cond_expr*/,
	NclSrcListNode * /*statements*/
#endif
);

extern void *_NclMakeBlock(
#if	NhlNeedProto
	NclSrcListNode * /* statements */
#endif
);

extern void *_NclMakeFileRef(
#if	NhlNeedProto
	NclSymbol * /* dfile */
#endif
);

extern void *_NclMakeFileVarRef(
#if	NhlNeedProto
	NclSymbol * /* dfile */,
	void * /* filevar */,
	NclSrcListNode * /* subscript_list */,
	int /*type*/
#endif
);



extern void *_NclMakeFileAttRef(
#if	NhlNeedProto
	NclSymbol * /* dfile */,	
	char *	    /* attname */,
	NclSrcListNode * /* subscript_list */
#endif
);

extern void *_NclMakeListRef(
#if	NhlNeedProto
	void * /*src_node*/,
	NclSymbol * /* equiv_sym*/,
	NclSymbol * /* list*/,
	void * /* subscript_list */,
	NclSymbol * /* tmp_var*/
#endif
);	
extern void *_NclMakeFileVarListRef(
#if	NhlNeedProto
	NclSymbol * /* list*/,
	void * /* list_subscript */,
	void *filevar,
	NclSrcListNode *  /* filevar_subscript */
#endif
);	

extern void *_NclMakeVarRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	NclSrcListNode * /* subscript_list */
#endif
);	

extern void *_NclMakeVarDimRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void*	/* dimnumexpr */
#endif
);
extern void *_NclMakeFileVarDimRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void*	/*filevar*/,
	void*	/* dimexpr */
#endif
);

extern void *_NclMakeFileVarAttRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void * /* filevarnode */,
	void* /* attnamenode */,
	NclSrcListNode * /*subscript_list*/
#endif
);
extern void *_NclMakeVarAttRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void * /* attnamenode */,
	NclSrcListNode * /*subscript_list*/
#endif
);

extern void *_NclMakeFileVarCoordAttRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void * /* filevar*/,
	void * /* coordnamenode */,
	void * /* attnamenode*/,
	NclSrcListNode * /*subscript_list*/
#endif
);
extern void *_NclMakeFileVarCoordRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void * /* filevar*/,
	void * /* coordnamenode */,
	NclSrcListNode * /* subscript_list */
#endif
);
extern void *_NclMakeVarCoordRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void * /* coordnamenode */,
	NclSrcListNode * /* subscript_list */
#endif
);
extern void *_NclMakeVarCoordAttRef(
#if	NhlNeedProto
	NclSymbol * /* var */,
	void * /* coordnamenode */,
	void * /* attnamenode*/,
	NclSrcListNode * /*subscript_list*/
#endif
);


extern void *_NclMakeUndefErrorRef(
#if	NhlNeedProto
	NclSymbol * /* var */
#endif
);

extern void *_NclMakeGetResource(
#if	NhlNeedProto
	void* /*resname*/,
	void* /*var */
#endif
);
extern void *_NclMakeResource(
#if	NhlNeedProto
	void* /*resname*/,
	void* /*expr*/
#endif
);
/*
extern void *_NclMakeObjRef(
#if	NhlNeedProto
	NclSymbol * obj
#endif
);
*/
extern void *_NclMakeEoln(
#if	NhlNeedProto
void
#endif
);

extern void *_NclMakeBreakCont(
#if	NhlNeedProto
NclSymbol * /*thesym*/
#endif
);

extern void _NclPrintTree(
#if	NhlNeedProto
void * /*root*/,
FILE * /*fp*/
#endif
);

extern void _NclFreeTree(
#if	NhlNeedProto
void 
#endif
);

void _NclAddProcFuncInfoToSym(
#if	NhlNeedProto
struct _NclSymbol * /*pf_sym*/,
NclSrcListNode * /*dec_list*/
#endif
);

void *_NclMakeExprNewOp(
#if	NhlNeedProto
void * /*size_expr*/,
void * /*datatype*/,
void * /*size_expr*/
#endif
);

void *_NclMakeNewOp(
#if	NhlNeedProto
void * /*size_expr*/,
struct _NclSymbol * /*datatype*/,
void * /*size_expr*/
#endif
);


void _NclValOnly(
#if	NhlNeedProto
void * /*expr*/
#endif
);


#endif /*_NCSrcTree_h*/
#ifdef __cplusplus
}
#endif

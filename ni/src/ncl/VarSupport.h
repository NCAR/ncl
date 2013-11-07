/*
 *      $Id: VarSupport.h,v 1.13 2010/04/14 21:29:48 huangwei Exp $
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
 *	Date:		Wed Jul 13 13:31:49 MDT 1994
 *
 *	Description:	
 */
#ifndef _VarSupport_h
#define _VarSupport_h

#define MAX_ALLOWED_NUMBER 10001

unsigned int _closest_prime(unsigned int prime_in);

extern NclSelectionRecord* _NclGetVarSelRec(
#if     NhlNeedProto
	struct _NclVarRec * /*inst*/
#endif

);
extern struct _NclMultiDValDataRec* _NclStripVarData(
#if     NhlNeedProto
struct _NclVarRec * /*inst*/
#endif
);

extern struct _NclMultiDValDataRec * _NclCoerceVar(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
NclObjTypes /* coerce_obj_to */,
NclScalar * /* new_missing */
#endif
);

extern NclObjTypes _NclGetVarRepValue(
#if     NhlNeedProto
struct _NclVarRec* /* self*/
#endif
);

extern struct _NclVarRec* _NclVarRead(
#if     NhlNeedProto
struct _NclVarRec*      /*var*/,
NclSelectionRecord *    /*sel_ptr*/
#endif
);

extern struct _NclMultiDValDataRec* _NclVarValueRead(
#if     NhlNeedProto
struct _NclVarRec*      /*var*/,
NclSelectionRecord *    /*sel_ptr*/,
NclScalar *             /*new_missing*/
#endif
);

extern NhlErrorTypes  _NclBuildCoordRSelection(
#if     NhlNeedProto
NclVar  /*var*/,
struct _NclRangeRec* /*range*/,
NclSelection* /*sel*/,
int /*dim_num*/,
char * /*dim_name*/
#endif
);

extern NhlErrorTypes _NclBuildCoordVSelection(
#if     NhlNeedProto
struct _NclVarRec * /*var*/,
struct _NclVecRec * /*vec*/,
struct _NclSelection* /*sel*/,
int /*dim_num*/ ,
char * /*dim_name*/
#endif
);
extern NhlErrorTypes  _NclBuildRSelection(
#if     NhlNeedProto
NclVar  /*var*/,
struct _NclRangeRec* /*range*/,
NclSelection* /*sel*/,
int /*dim_num*/,
char * /*dim_name*/
#endif
);

extern NhlErrorTypes _NclBuildVSelection(
#if     NhlNeedProto
struct _NclVarRec * /*var*/,
struct _NclVecRec * /*vec*/,
struct _NclSelection* /*sel*/,
int /*dim_num*/ ,
char * /*dim_name*/
#endif
);

extern int _NclVarIsAtt(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*attname*/
#endif
);

extern NhlErrorTypes _NclAssignToVar(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
struct _NclMultiDValDataRec * /*value*/,
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

extern NhlErrorTypes _NclWriteAtt(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char    * /*attname*/,
struct  _NclMultiDValDataRec * /*value*/,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern struct _NclMultiDValDataRec *_NclReadAtt(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
const char *        /*attname*/,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern int _NclIsDim(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*dimnmae*/
#endif
);

extern struct _NclMultiDValDataRec *_NclReadDim(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char *          /*dim_name*/,
long            /*dim_num*/
#endif
);

extern NhlErrorTypes _NclWriteDim(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
long            /*dim_num*/,
char*           /*dim_name*/
#endif
);

extern int _NclIsCoord(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*coordname*/
#endif
);

extern struct _NclVarRec *_NclReadCoordVar(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
char    *               /* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern NhlErrorTypes _NclWriteCoordVar(
#if     NhlNeedProto
struct  _NclVarRec      * /*self*/,
struct  _NclMultiDValDataRec    * /*value*/,
char    *               /* coord_name */,
struct  _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern NhlErrorTypes _NclDeleteCoordVar(
#if     NhlNeedProto
	struct  _NclVarRec      * /*self*/,
	char */* coord_name */
#endif
);


extern struct _NclDimRec *_NclGetDimInfo(
#if     NhlNeedProto
struct _NclVarRec * /*self*/,
char * /*dim_name*/,
long /*dim_num*/
#endif
);

extern NhlErrorTypes _NclAssignVarToVar(
#if NhlNeedProto
struct _NclVarRec * /*lhs*/,
NclSelectionRecord * /*lhs_sel_ptr*/,
struct _NclVarRec * /* rhs */,
NclSelectionRecord* /* rhs_sel_ptr*/
#endif
);

extern struct _NclVarRec * _NclCopyVar(
#if NhlNeedProto
struct _NclVarRec * /*var */,
struct _NclSymbol */*new_name*/,
struct _NclVarRec * /*storage*/
#endif
);

extern struct _NclVarRec * _NclVarNclCreate(
#if NhlNeedProto
        struct _NclVarRec *     /* inst */,
        struct _NclObjClassRec *        /* theclass */,
        NclObjTypes     /* obj_type */,
        unsigned int    /* obj_type_mask */,
        struct _NclSymbol  * /* thesym */,
        struct _NclMultiDValDataRec * /* value */,
        struct _NclDimRec * /*dim_info*/,
        int             /*att_id*/,
        int*            /*coords*/,
        NclVarTypes /* var_type */,
        char * /*var_name*/,
	NclStatus /*status*/
#endif
);

extern NhlErrorTypes _NclAttCopyWrite(
#if	NhlNeedProto
	struct _NclVarRec * /*to*/,
	struct _NclVarRec* /*from*/
#endif
);

extern NhlErrorTypes _NclPrintVarSummary(
#if	NhlNeedProto
	struct _NclVarRec * /*thevar*/
#endif
);

extern struct _NclApiDataList *_NclGetVarInfo2(
#if     NhlNeedProto
NclVar
#endif
);

extern NhlErrorTypes _PrintListVarSummary(
#if     NhlNeedProto
NclObj self, FILE *fp
#endif
);

NhlErrorTypes _NclReplaceAtt(struct _NclVarRec *self, char* attname,
                             struct _NclMultiDValDataRec *value,
                             struct _NclSelectionRecord *sel_ptr);

#endif /* _VarSupport_h */



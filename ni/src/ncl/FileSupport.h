/*
 *      $Id: FileSupport.h,v 1.1 1994-07-14 20:45:51 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1994			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		FileSupport.h
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Thu Jul 14 14:38:17 MDT 1994
 *
 *	Description:	
 */
#ifndef _FileSupport_h
#define _FileSupport_h


extern NhlErrorTypes  _NclBuildFileRSelection(
#if	NhlNeedProto
NclFile /*file*/,
NclQuark  /*var*/,
struct _NclRangeRec * /*range*/,
struct _NclSelection* /*sel*/,
int /*dim_num*/,
char * /*dim_name*/
#endif
);
extern NhlErrorTypes _NclBuildFileVSelection(
#if	NhlNeedProto
struct _NclFileRec * /*file*/,
NclQuark  /*var*/,
struct _NclVecRec * /*vec*/,
struct _NclSelection* /*sel*/,
int /*dim_num*/	,
char * /*dim_name*/
#endif
);

extern NclObjTypes _NclFileVarRepValue(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */
#endif
);

extern struct _NclMultiDValDataRec* _NclFileVarCoerce(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclObjTypes             /*coerce_to_obj*/,
NclScalar *             /*new_missing*/
#endif
);

extern int _NclFileIsVar(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

extern NhlErrorTypes _NclFileWriteVar(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

extern NhlErrorTypes _NclFileWriteVarVar(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*lhs_var_name*/,
struct _NclSelectionRecord * /* lhs_sel_ptr */,
struct _NclVarRec * /* rhs_var */,
struct _NclSelectionRecord * /* rhs_sel_ptr */
#endif
);

extern struct _NclVarRec *_NclFileReadVar(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern struct _NclMultiDValDataRec *_NclFileReadVarValue(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern int _NclFileIsAtt(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

extern struct _NclMultiDValDataRec* _NclFileReadAtt(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

extern NhlErrorTypes _NclFileWriteAtt(
#ifdef NhlNeedProto
NclFile /*thefile;*/,
NclQuark  /*attname;*/,
struct _NclMultiDValDataRec*  /*value;*/,
struct _NclSelectionRecord * /*sel_ptr;*/
#endif
);

extern int _NclFileIsVarAtt(
#ifdef NhlNeedProto
NclFile /*file*/,
NclQuark /* var */,
NclQuark /* attname*/
#endif
);

extern struct _NclMultiDValDataRec *_NclFileReadVarAtt(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

extern NhlErrorTypes _NclFileWriteVarAtt(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

extern int _NclFileVarIsDim(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* var*/,
NclQuark /* dimname */
#endif
);

extern int _NclFileIsDim(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /* dimname */
#endif
);

extern struct _NclMultiDValDataRec *_NclFileReadDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

extern NhlErrorTypes _NclFileWriteDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

extern struct _NclMultiDValDataRec *_NclFileVarReadDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

extern NhlErrorTypes _NclFileVarWriteDim(
#ifdef NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

extern int _NclFileVarIsCoord(
#ifdef NhlNeedProto
NclFile /*thefile */,
NclQuark /*coord_name */
#endif
);

extern struct _NclVarRec *_NclFileReadCoord(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

extern NhlErrorTypes _NclFileWriteCoord(
#ifdef NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

#endif /*_FileSupport_h */


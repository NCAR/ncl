/*
 *      $Id: FileSupport.h,v 1.2 1994-12-23 01:17:29 ethan Exp $
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
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */
#endif
);

extern struct _NclMultiDValDataRec* _NclFileVarCoerce(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclObjTypes             /*coerce_to_obj*/,
NclScalar *             /*new_missing*/
#endif
);

extern int _NclFileIsVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

extern NhlErrorTypes _NclFileWriteVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*var_name*/,
struct _NclMultiDValDataRec * /* value */,
struct _NclSelectionRecord * /* sel_ptr */
#endif
);

extern NhlErrorTypes _NclFileWriteVarVar(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*lhs_var_name*/,
struct _NclSelectionRecord * /* lhs_sel_ptr */,
struct _NclVarRec * /* rhs_var */,
struct _NclSelectionRecord * /* rhs_sel_ptr */
#endif
);

extern struct _NclVarRec *_NclFileReadVar(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern struct _NclMultiDValDataRec *_NclFileReadVarValue(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
struct _NclSelectionRecord * /*sel_ptr*/
#endif
);

extern int _NclFileIsAtt(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* name */
#endif
);

extern struct _NclMultiDValDataRec* _NclFileReadAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* attname */,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

extern NhlErrorTypes _NclFileWriteAtt(
#if	NhlNeedProto
NclFile /*thefile;*/,
NclQuark  /*attname;*/,
struct _NclMultiDValDataRec*  /*value;*/,
struct _NclSelectionRecord * /*sel_ptr;*/
#endif
);

extern int _NclFileIsVarAtt(
#if	NhlNeedProto
NclFile /*file*/,
NclQuark /* var */,
NclQuark /* attname*/
#endif
);

extern struct _NclMultiDValDataRec *_NclFileReadVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclSelectionRecord* /*sel_ptr*/
#endif
);

extern NhlErrorTypes _NclFileWriteVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr*/
#endif
);

extern int _NclFileVarIsDim(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* var*/,
NclQuark /* dimname */
#endif
);

extern int _NclFileIsDim(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /* dimname */
#endif
);

extern struct _NclMultiDValDataRec *_NclFileReadDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

extern NhlErrorTypes _NclFileWriteDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

extern struct _NclMultiDValDataRec *_NclFileVarReadDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /*var_name */,
NclQuark /*dim_name*/,
long /*dim_num*/
#endif
);

extern NhlErrorTypes _NclFileVarWriteDim(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* var_name */,
NclQuark /*dim_name*/,
long /*dim_num */
#endif
);

extern int _NclFileVarIsCoord(
#if	NhlNeedProto
NclFile /*thefile */,
NclQuark /*coord_name */
#endif
);

extern struct _NclVarRec *_NclFileReadCoord(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

extern NhlErrorTypes _NclFileWriteCoord(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* coord_name */,
struct _NclMultiDValDataRec* /*value*/,
struct _NclSelectionRecord* /* sel_ptr */
#endif
);

#endif /*_FileSupport_h */


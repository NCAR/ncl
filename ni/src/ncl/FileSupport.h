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

#include "defs.h"
#include "NclMultiDValData.h"
#include "NclList.h"

extern NhlErrorTypes  _NclBuildFileCoordRSelection(
#if	NhlNeedProto
NclFile /*file*/,
NclQuark  /*var*/,
struct _NclRangeRec * /*range*/,
struct _NclSelection* /*sel*/,
int /*dim_num*/,
char * /*dim_name*/
#endif
);
extern NhlErrorTypes _NclBuildFileCoordVSelection(
#if	NhlNeedProto
struct _NclFileRec * /*file*/,
NclQuark  /*var*/,
struct _NclVecRec * /*vec*/,
struct _NclSelection* /*sel*/,
int /*dim_num*/	,
char * /*dim_name*/
#endif
);
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

extern int _NclFileIsGroup(
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

extern struct _NclFileRec *_NclFileReadGroup(
#if	NhlNeedProto
NclFile /*thefile*/,
NclQuark /* grp_name */
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
NclQuark /*grp_name */,
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

extern NhlErrorTypes _NclFileDeleteAtt(
#if	NhlNeedProto
NclFile /*thefile;*/,
NclQuark  /*attname;*/
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

extern int _NclFileVarIsAtt(
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

extern NhlErrorTypes _NclFileDeleteVarAtt(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* var */,
NclQuark /* attname */
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

extern NhlErrorTypes _NclFileAddDim(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* dimname */,
ng_size_t	/* dimsize */,
int	/* is_unlimited*/
#endif
);

extern NhlErrorTypes _NclFileAddVlen(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                     NclQuark type, NclQuark *dim_names, ng_size_t ndims);
extern NhlErrorTypes _NclFileAddEnum(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                     NclQuark dim_name, NclQuark *mem_name, void *mem_value,
                                     ng_size_t n_mems, NclBasicDataTypes val_type);
extern NhlErrorTypes _NclFileAddOpaque(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                       int var_size, NclQuark dim_name);
extern NhlErrorTypes _NclFileAddCompound(NclFile thefile, NclQuark compound_name, NclQuark var_name,
                                         ng_size_t n_dims, NclQuark *dim_name, ng_size_t n_mems,
                                         NclQuark *mem_name, NclQuark *mem_type, int *mem_size);
extern NhlErrorTypes _NclFileWriteCompound(NclFile thefile, NclQuark compound_name, NclQuark var_name,
                                           ng_size_t n_mems, NclQuark *mem_name, NclObj listobj);

extern NhlErrorTypes _NclFileAddGrp(NclFile thefile, NclQuark grpname);

extern NhlErrorTypes _NclFileAddChunkDim(
#if	NhlNeedProto
NclFile /* thefile */,
NclQuark /* dimname */,
ng_size_t	/* dimsize */,
int	/* is_unlimited*/
#endif
);
extern NhlErrorTypes _NclFileAddVar(
#if NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */,
NclQuark /* type */,
int	/* n_dims */,
NclQuark * /* dimnames */
#endif
);
extern NhlErrorTypes _NclFileAddVarChunk(
#if NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */,
int	 /* n_dims */,
ng_size_t *    /* dims */
#endif
);
extern NhlErrorTypes _NclFileAddVarChunkCache(
#if NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */,
ng_size_t	 /* cache_size */,
ng_size_t   /* cache_nelems */,
float    /* cache_preemption */
#endif
);
extern NhlErrorTypes _NclFileSetVarCompressLevel(
#if NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */,
int	 /* compress-level */
#endif
);
extern NhlErrorTypes _NclPrintFileVarSummary(
#if NhlNeedProto
NclFile /* thefile */,
NclQuark /* varname */
#endif
);
extern NclQuark *_NclGetFileGroupsList(
#if NhlNeedProto
NclFile  /* the file*/,
NclQuark /* base_group_name */,
int      /* depth */,
int *    /* n_grps */
#endif
);
extern NclQuark *_NclGetGroupVarsList(
#if NhlNeedProto
NclFile  /* the file*/,
NclQuark /* base_group_name */,
int      /* depth */,
int *    /* n_grps */
#endif
);
extern struct _NclApiDataList *_NclGetFileVarInfoList2(
#if NhlNeedProto
struct _NclFileRec * /*thefile*/
#endif
);
extern struct _NclApiDataList *_NclGetFileVarInfo1(struct _NclFileRec * /*thefile*/,
						   NclQuark /*file_var_name*/);
extern struct _NclApiDataList *_NclGetFileVarInfo2(
#if     NhlNeedProto
struct _NclFileRec * /*thefile*/,
NclQuark /*file_var_name*/
#endif
);
extern struct _NclApiDataList *_NclGetFileInfo1(struct _NclFileRec * /*thefile*/);
extern struct _NclApiDataList *_NclGetFileInfo2(
#if NhlNeedProto
struct _NclFileRec * /*thefile*/
#endif
);

extern NhlErrorTypes _NclFileSetOption(
#if NhlNeedProto
NclFile /* thefile */,  /* either a file or a format */
NclQuark /* format */,
NclQuark /* option */,
struct _NclMultiDValDataRec* /*value*/
#endif
);

extern int _NclFileIsOption(
#if NhlNeedProto
NclQuark /* format */,
NclQuark /* option */
#endif
);

extern NhlErrorTypes _NclFileSetOptionDefaults(
#if NhlNeedProto
NclQuark format, /* if NULL set defaults for all formats */
NclQuark option /* if NULL set defaults for all options */
#endif
);

extern NclFile _NclOpenFile(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                              unsigned int obj_type_mask, NclStatus status,
                              NclQuark path, int rw_status);
extern NclGroup *_NclCreateGroup(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                                 unsigned int obj_type_mask, NclStatus status,
                                 NclFile file_in, NclQuark group_name);
extern NhlErrorTypes _NclPrintFileSummary(NclObj self, FILE *fp);
extern NclQuark *_NclFileReadVarNames(NclFile thefile, int *num_vars);
extern NclQuark *_NclFileReadGrpNames(NclFile thefile, int *num_grps);
extern ng_size_t *_NclFileReadChunkSizes(NclFile thefile, int *nchunks);
extern int _NclFileReadCompressionLevel(NclFile thefile);
extern NclQuark _NclFileReadVersion(NclFile thefile);

extern void _NclInitFileClasses();

#endif /*_FileSupport_h */


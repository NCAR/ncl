/************************************************************************
*                                                                       *
*                 Copyright (C)  1994                                   *
*         University Corporation for Atmospheric Research               *
*                 All Rights Reserved                                   *
*                                                                       *
************************************************************************/
/*
 *      $Id$
 */
#ifndef NclAdvancedFile_h
#define NclAdvancedFile_h

#ifdef NIO_LIB_ONLY
#include "niohlu.h"
#include "nioNresDB.h"
#include "nioCallbacks.h"
#else
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/Callbacks.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <assert.h>
#include <math.h>
#include <alloca.h>
#include "defs.h"
#include "Symbol.h"
#include "NclVar.h"
#include "NclFile.h"
#include "NclGroup.h"
#include "NclFileInterfaces.h"
#include "DataSupport.h"
#include "VarSupport.h"
#include "NclMultiDValData.h"
#include "NclAtt.h"
#include "AttSupport.h"
#include "NclType.h"
#include "TypeSupport.h"
#include "FileSupport.h"
#include "NclMdInc.h"
#include "NclList.h"
#include "NclCoordVar.h"
#include "NclCallBacksI.h"
#include "NclData.h"
#include "NclAdvancedFileStructure.h"

extern int grib_version;

typedef struct _NclAdvancedFileRec NclAdvancedFileRec;
typedef struct _NclAdvancedFileClassRec NclAdvancedFileClassRec;
typedef NclAdvancedFileRec *NclAdvancedFile;
typedef NclAdvancedFileClassRec *NclAdvancedFileClass;

typedef NhlErrorTypes (*NclAssignFileGrpFunc)(NclFile thefile, NclQuark grp_name);
typedef NhlErrorTypes (*NclAssignFileVlenFunc)(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                               NclQuark type, NclQuark *dim_names, ng_size_t ndims);
typedef NhlErrorTypes (*NclAssignFileEnumFunc)(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                               NclQuark dim_name, NclQuark *mem_name, void *mem_value,
                                               ng_size_t n_mems, NclBasicDataTypes val_type);
typedef NhlErrorTypes (*NclAssignFileOpaqueFunc)(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                                 int var_size, NclQuark dim_name);
typedef NhlErrorTypes (*NclAssignFileCompoundFunc)(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                                   ng_size_t n_dims, NclQuark *dim_name, ng_size_t n_mems,
                                                   NclQuark *mem_name, NclQuark *mem_type, int *mem_size);
typedef NhlErrorTypes (*NclWriteFileCompoundFunc)(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                                  ng_size_t n_mems, NclQuark *mem_name, NclList thelist);

typedef struct _NclAdvancedFileClassPart
{
    NclAssignFileGrpFunc    write_grp;
    NclAssignFileVlenFunc   create_vlen_type;
    NclAssignFileEnumFunc   create_enum_type;
    NclAssignFileOpaqueFunc   create_opaque_type;
    NclAssignFileCompoundFunc create_compound_type;
    NclWriteFileCompoundFunc  write_compound;
    int new_stuff;	/* Advanced part(s) beyond _NclFileClasspart */
} NclAdvancedFileClassPart;

typedef struct _NclAdvancedFilePart
{
    NclQuark        fname;
    NclQuark        fpath;
    NclQuark        file_ext_q;
    int             wr_status;
    NclFileFormat   file_format;
    NclFileGrpNode *grpnode;

    struct _NclFormatFunctionRecord *format_funcs;

    NclObjTypes     type;
    NclQuark        gname;
} NclAdvancedFilePart;
 
struct _NclAdvancedFileClassRec
{
    NclObjClassPart     obj_class;
    NclFileClassPart    file_class;
    NclAdvancedFileClassPart advancedfile_class;
};

struct _NclAdvancedFileRec
{
    NclObjPart      obj;
    NclFilePart     file;
    NclAdvancedFilePart  advancedfile;
};

extern NclObjClass nclAdvancedFileClass;

extern NclAdvancedFileClassRec nclAdvancedFileClassRec;

extern NclFile _NclAdvancedFileCreate(NclObj       inst,
                                 NclObjClass  theclass,
                                 NclObjTypes  obj_type,
                                 unsigned int obj_type_mask,
                                 NclStatus    status,
                                 NclQuark     path,
                                 int          rw_status,
				 NclQuark     file_ext_q,
				 NclQuark     fname_q,
				 NhlBoolean   is_http, 
				 char        *end_of_name,
				 int          len_path);

void _clearNclPrintIndentation();
void _increaseNclPrintIndentation();
void _decreaseNclPrintIndentation();

void _printNclFileUDTRecord(FILE *fp, NclAdvancedFile thefile, NclFileUDTRecord *udt_rec);
void _printNclFileAttRecord(FILE *fp, NclAdvancedFile thefile, NclFileAttRecord *att_rec);
void _printNclFileDimRecord(FILE *fp, NclAdvancedFile thefile, NclFileDimRecord *dim_rec);
void _printNclFileChunkDimRecord(FILE *fp, NclAdvancedFile thefile, NclFileDimRecord *dim_rec);
void _printNclFileVarDimRecord(FILE *fp, NclFileDimRecord *dim_rec);
void _printNclFileVarNode(FILE *fp, NclAdvancedFile thefile, NclFileVarNode *varnode);
void _printNclFileVarRecord(FILE *fp, NclAdvancedFile thefile, NclFileVarRecord *var_rec);
void _printNclFileGrpRecord(FILE *fp, NclAdvancedFile thefile, NclFileGrpRecord *grp_rec);

NhlErrorTypes _NclAdvancedFilePrintSummary(NclObj self, FILE *fp);

extern void AdvancedLoadVarAtts(NclAdvancedFile thefile, NclQuark var);

extern char *_getComponentName(const char *fullname, char **structname);
extern NclFileCompoundNode *_getComponentNodeFromVarNode(NclFileVarNode *varnode,
                                                         const char *component_name);
extern NclVar _NclCreateVlenVar(char *var_name, void *val,
                                int ndims, NclQuark *dimnames,
                                ng_size_t *dimsizes, NclBasicDataTypes type);
extern NclQuark *_NclGetGrpNames(void *therec, int *num_grps);
#endif /* NclAdvancedFile_h */


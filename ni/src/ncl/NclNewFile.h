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
#ifndef NclNewFile_h
#define NclNewFile_h

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
#include "NclNewFileStructure.h"

#define NCLFILE_INC -1
#define NCLFILE_DEC -2
#define NCLFILE_VEC 0

char blank_space[MAX_BLANK_SPACE_LENGTH];
int indentation_level;
int indentation_length;
extern int grib_version;

typedef struct _NclNewFileRec NclNewFileRec;
typedef struct _NclNewFileClassRec NclNewFileClassRec;
typedef NclNewFileRec *NclNewFile;
typedef NclNewFileClassRec *NclNewFileClass;

typedef NhlErrorTypes (*NclAssignFileGrpFunc)(NclFile thefile, NclQuark grp_name);
typedef NhlErrorTypes (*NclAssignFileVlenFunc)(NclFile thefile, NclQuark vlen_name, NclQuark var_name,
                                               NclQuark type, NclQuark dim_name);
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

typedef struct _NclNewFileClassPart
{
    NclAssignFileGrpFunc    write_grp;
    NclAssignFileVlenFunc   create_vlen_type;
    NclAssignFileEnumFunc   create_enum_type;
    NclAssignFileOpaqueFunc   create_opaque_type;
    NclAssignFileCompoundFunc create_compound_type;
    NclWriteFileCompoundFunc  write_compound;
    int new_stuff;	/* New part(s) beyond _NclFileClasspart */
} NclNewFileClassPart;

typedef struct _NclNewFilePart
{
    NclQuark        fname;
    NclQuark        fpath;
    NclQuark        file_ext_q;
    int             wr_status;
    NclFileFormat   file_format;
    NclFileGrpNode *grpnode;

    struct _NclFormatFunctionRecord *format_funcs;
} NclNewFilePart;
 
struct _NclNewFileClassRec
{
    NclObjClassPart     obj_class;
    NclFileClassPart    file_class;
    NclNewFileClassPart newfile_class;
};

struct _NclNewFileRec
{
    NclObjPart      obj;
    NclFilePart     file;
    NclNewFilePart  newfile;

    int use_new_hlfs;
};

extern NclObjClass nclNewFileClass;

extern NclNewFileClassRec nclNewFileClassRec;

extern NclFile _NclNewFileCreate(NclObj       inst,
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

static NhlErrorTypes InitializeNewFileClass();

void _clearNclPrintIndentation();
void _increaseNclPrintIndentation();
void _decreaseNclPrintIndentation();

void _printNclFileUDTRecord(FILE *fp, NclNewFile thefile, NclFileUDTRecord *udt_rec);
void _printNclFileAttRecord(FILE *fp, NclNewFile thefile, NclFileAttRecord *att_rec);
void _printNclFileDimRecord(FILE *fp, NclNewFile thefile, NclFileDimRecord *dim_rec);
void _printNclFileChunkDimRecord(FILE *fp, NclNewFile thefile, NclFileDimRecord *dim_rec);
void _printNclFileVarDimRecord(FILE *fp, NclFileDimRecord *dim_rec);
void _printNclFileVarRecord(FILE *fp, NclNewFile thefile, NclFileVarRecord *var_rec);
void _printNclFileGrpRecord(FILE *fp, NclNewFile thefile, NclFileGrpRecord *grp_rec);

void NewLoadVarAtts(NclNewFile thefile, NclQuark var);

NhlErrorTypes _NclNewFilePrintSummary(NclObj self, FILE *fp);

extern char *_getComponentName(const char *fullname, char **structname);
extern NclFileCompoundNode *_getComponentNodeFromVarNode(NclFileVarNode *varnode,
                                                         const char *component_name);
extern NclVar _NclCreateVlenVar(char *var_name, void *val,
                                int ndims, NclQuark *dimnames,
                                ng_size_t *dimsizes, NclBasicDataTypes type);
extern NclQuark *_NclGetGrpNames(void *therec, int *num_grps);
/*
 * this is for 1-D variables only - basically for coordinate variables.
 */
extern void *_NclGetCachedValue(NclFileVarNode *varnode,
             long start, long finish, long stride, void *storage);
#endif /* NclNewFile_h */


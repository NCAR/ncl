#ifndef NclNewFileStructure_h
#define NclNewFileStructure_h

#undef MAX_BLANK_SPACE_LENGTH
#define MAX_BLANK_SPACE_LENGTH  1024

#define NCL_MINIMUM_ATTS    2
#define NCL_MINIMUM_DIMS    2
#define NCL_MINIMUM_COORDS    2
#define NCL_MINIMUM_VARS    4
#define NCL_MINIMUM_GRPS    2

#include <netcdf.h>
#include "NclList.h"
#include "NclOptions.h"

typedef struct _NclFileUserDefinedTypeNode
{
    ng_size_t         id;
    size_t            size;
    int               ncl_class;
    int               max_fields;
    int               n_fields;
    NclQuark          name;
    ng_size_t         type;
    NclQuark          *mem_name;
    NclBasicDataTypes *mem_type;
} NclFileUDTNode;

typedef struct _NclFileUserDefinedTypeRecord
{
    ng_size_t       gid;
    ng_size_t       uid;
    int             max_udts;
    int             n_udts;
    NclFileUDTNode *udt_node;
} NclFileUDTRecord;

typedef struct _NclFileDimNode
{
    ng_size_t id;
    NclQuark  name;
    NclQuark  description;
    NclQuark  dataset_name;
    ng_size_t size;
    int       is_unlimited;
    int       is_dataset;
} NclFileDimNode;

typedef struct _NclFileDimRecord
{
    ng_size_t       gid;
    int             max_dims;
    int             n_dims;
    NclFileDimNode *dim_node;
} NclFileDimRecord;

typedef struct _NclFileAttNode
{
    NclQuark          name;
    NclBasicDataTypes type;
    int               n_elem;
    void             *value;
    nc_type           the_nc_type;
    int               is_virtual;
    int               is_compound;
    int               is_vlen;
    int               is_opaque;
    int               is_enum;
} NclFileAttNode;

typedef struct _NclFileAttRecord
{
    ng_size_t       gid;
    ng_size_t       aid;
    ng_size_t       id;
    int             max_atts;
    int             n_atts;
    NclFileAttNode *att_node;

    _NhlCB                   cb;
    struct _FileCallBackRec *udata;
} NclFileAttRecord;

typedef struct _NclFileCompoundNode
{
    NclQuark             name;
    NclBasicDataTypes    type;
    nc_type              the_nc_type;
    size_t               offset;
    int                  rank;
    int                  nvals;
    int                 *sides;

    void                *value;
} NclFileCompoundNode;

typedef struct _NclFileCompoundRecord
{
    size_t            max_comps;
    size_t            n_comps;   /* aka nfields */
    size_t            type;
    size_t            size;
    NclQuark          name;
    nc_type           xtype;
    nc_type           base_nc_type;

    NclFileCompoundNode    *compnode;
} NclFileCompoundRecord;

typedef struct _NclFileEnumNode
{
    NclQuark          name;
    long long         value;
} NclFileEnumNode;

typedef struct _NclFileEnumRecord
{
    size_t            max_enums;
    size_t            n_enums;   /* aka num_members */
    NclQuark          name;
    size_t            type;
    size_t            size;
    nc_type           xtype;
    nc_type           base_nc_type;

    void             *values;
    NclFileEnumNode  *enum_node;
} NclFileEnumRecord;

typedef struct _NclFileOpaqueRecord
{
    NclQuark name;
    size_t   max_opaques;
    size_t   n_opaques; 
    size_t   type;
    size_t   size;
    nc_type  xtype;
    nc_type  base_nc_type;

    void    *values;
} NclFileOpaqueRecord;

typedef struct _NclFileVlenRecord
{
    NclQuark name;
    size_t   max_vlens;
    size_t   n_vlens; 
    size_t   type;
    size_t   size;
    nc_type  xtype;
    nc_type  base_nc_type;
    int      ncl_class;
    int     *vs;
    int     *ve;

    void    *values;
} NclFileVlenRecord;

typedef struct _NclFileVarNode
{
    ng_size_t         id;
    ng_size_t         gid;
    NclQuark          name;
    NclQuark          real_name;
    NclQuark          short_name;
    NclQuark          full_name;
    NclQuark          index_dim;
    NclQuark          class_name;
    NclBasicDataTypes type;
    nc_type           the_nc_type;

/*
    int               max_dims;
    int               n_dims;
    int              *dimid;
*/
    NclFileDimRecord *dim_rec;

    int               is_chunked;

/*
    int               max_chunk_dims;
    int               n_chunk_dims;
    int              *chunk_dimid;
*/
    NclFileDimRecord *chunk_dim_rec;

    int                    is_compound;
    NclFileCompoundRecord *comprec;

    NclFileAttRecord *att_rec;

    int               compress_level;
    int               use_cache;
    ng_size_t         cache_size;
    ng_size_t         cache_nelems;
    float             cache_preemption;

    void             *value;
} NclFileVarNode;

typedef struct _NclFileVarRecord
{
    ng_size_t       gid;
    int             max_vars;
    int             n_vars;
    NclFileVarNode *var_node;
} NclFileVarRecord;

typedef struct _NclFileCoordVarRecord
{
    int            max_vars;
    int            n_vars;
    NclFileVarNode        **var_node;
} NclFileCoordVarRecord;

typedef struct _NclFileGrpRecord    NclFileGrpRecord;
typedef struct _NclFileGrpNode      NclFileGrpNode;

struct _NclFileGrpNode
{
    ng_size_t            fid;
    ng_size_t            id;
    ng_size_t            pid;
    
    NclQuark             name;
    NclQuark             pname;
    NclQuark             real_name;

    NclQuark             path;
    NclQuark             extension;

    NclFileFormat        file_format;
    int                  status;
    int                  open;
    int                  format;
    int                  define_mode;
    int                  has_scalar_dim;
    int                  header_reserve_space;

    int                  compress_level;
    void                *other_src;

    ng_size_t            deflate_pass;

    int                  use_cache;
    ng_size_t            cache_size;
    ng_size_t            cache_nelems;
    float                cache_preemption;

    int                  n_options;
    NCLOptions          *options;

    int                  is_chunked;

    NclFileDimRecord    *chunk_dim_rec;
    NclFileDimRecord    *dim_rec;
    NclFileDimRecord    *unlimit_dim_rec;
    NclFileAttRecord    *att_rec;
    NclFileVarRecord    *var_rec;
    NclFileCoordVarRecord    *coord_var_rec;
    NclFileGrpRecord    *grp_rec;
    NclFileUDTRecord    *udt_rec;

    NclFileGrpNode      *parent;
};

struct _NclFileGrpRecord
{
    int        max_grps;
    int        n_grps;
    NclFileGrpNode  **grp_node;
};

NclFileCompoundRecord *_NclFileCompoundAlloc(int n_comps);
NclFileCoordVarRecord *_NclFileCoordVarAlloc(int n_vars);

NclFileGrpRecord *_NclFileGrpAlloc(int n_grps);
NclFileVarRecord *_NclFileVarAlloc(int n_vars);
NclFileAttRecord *_NclFileAttAlloc(int n_atts);
NclFileDimRecord *_NclFileDimAlloc(int n_dims);
NclFileUDTRecord *_NclFileUDTAlloc(int n_udts);

void _NclFileGrpRealloc(NclFileGrpRecord *grp_rec);
void _NclFileVarRealloc(NclFileVarRecord *var_rec);
void _NclFileAttRealloc(NclFileAttRecord **att_rec);
void _NclFileDimRealloc(NclFileDimRecord *dim_rec);
void _NclFileUDTRealloc(NclFileUDTRecord *udt_rec);

NclFileGrpNode *_getGrpNodeFromNclFileGrpNode(NclFileGrpNode *grp_rec,
                        NclQuark grp_name);
NclFileVarNode *_getVarNodeFromNclFileVarRecord(NclFileVarRecord *var_rec,
                        NclQuark var_name);
NclFileVarNode *_getVarNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark var_name);
NclFileAttNode *_getAttNodeFromNclFileGrpRecord(NclFileGrpRecord *grp_rec,
                        NclQuark att_name);
NclFileAttNode *_getAttNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark att_name);
NclFileAttNode *_getAttNodeFromNclFileVarNode(NclFileVarNode *varnode,
                        NclQuark att_name);
NclFileDimNode *_getDimNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark dim_name);
NclFileDimNode *_getDimNodeFromNclFileGrpNodeWithID(NclFileGrpNode *grpnode,
                        int dimid);
NclFileDimNode *_getChunkDimNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark dim_name);
NclFileVarNode *_getCoordVarNodeFromNclFileGrpNode(NclFileGrpNode *grpnode,
                        NclQuark var_name);

NhlErrorTypes _addNclAttNode(NclFileAttRecord **attrec,
                 NclQuark name, NclBasicDataTypes type,
                 int n_elem, void *value);
NhlErrorTypes _addNclDimNode(NclFileDimRecord **dimrec,
                 NclQuark name, int dimid, ng_size_t size,
                 int is_unlimited);
NhlErrorTypes _addNclVarNodeToGrpNode(NclFileGrpNode *grpnode,
                 NclQuark name, int varid, NclBasicDataTypes type,
                 int n_dims, NclQuark *dimnames, ng_size_t *dimsizes);
NhlErrorTypes _addNclCoordVarNode(NclFileCoordVarRecord **var_rec,
                 NclFileVarNode *var_node);
NhlErrorTypes _addNclGrpNodeToGrpNode(NclFileGrpNode *grpnode, NclQuark grpname);

void FileDestroyGrpNode(NclFileGrpNode *grpnode);

NhlErrorTypes _delNclAttNode(NclFileAttRecord **attrec, NclQuark name);

NclFileCoordVarRecord *_NclFileCoordVarRealloc(NclFileCoordVarRecord *coord_var_rec);

NclFileCompoundRecord *get_nc4_compoundrec(int ncid, nc_type xtype);

NclMultiDValData get_nc4_vlenlist(int ncid, int varid, nc_type xtype);

void _printNclTypeVal(FILE *fp, NclBasicDataTypes type, void *val, int newline);
void _justPrintTypeVal(FILE *fp, NclBasicDataTypes type, void *val, int newline);
void _justPrintTypeValAtPoint(FILE *fp, NclBasicDataTypes type, void *val,
                              size_t np, int newline);

NclFileEnumRecord *_NclFileEnumAlloc(int n_enums);
void _NclFileEnumRealloc(NclFileEnumRecord **enumrec);
NhlErrorTypes _addNclEnumNode(NclFileEnumRecord **enumrec,
                 NclQuark name, long long value);

#endif


#ifndef H5DATA_STRUCT_H
#define H5DATA_STRUCT_H

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <limits.h>
#include <math.h>
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef NIO_LIB_ONLY
#include "nioNgSizeT.h"
#include "niohlu.h"
#include "nioNresDB.h"
#else
#ifdef STANDALONE
#include "NgSizeT.h"
#else
#include <ncarg/hlu/NgSizeT.h>
#include <ncarg/hlu/hlu.h>
#include <ncarg/hlu/NresDB.h>
#endif
#endif

#include "defs.h"
#include "hdf5.h"

#ifndef FALSE
#define FALSE		0
#endif

#ifndef TRUE
#define TRUE		1
#endif

#define SUCCEED         0
#define FAILED          (-1)

#define HDF5_BUF_SIZE	1024
#define HDF5_NAME_LEN	1024
#define MAX_COMPOUND_COMPONENTS		256

#define HDF5_EXT_LINK		0
#define HDF5_GROUP		1
#define HDF5_DATASET		2
#define HDF5_NAMED_DATATYPE	3

#define NELMTS(X)           (sizeof(X)/sizeof(X[0]))

/*
typedef unsigned long nclH5size_t;
*/
typedef ng_size_t nclH5size_t;

typedef struct _NclHDF5compound_component_t
{
    char     name[HDF5_NAME_LEN];	/* Data name */
    char     type[HDF5_NAME_LEN];	/* Data type, such as integer, float, etc. */
    hid_t    type_id;
    unsigned long offset;
    unsigned int  is_str;
} NclHDF5compound_component_t;

typedef struct _NclHDF5compound_t
{
    int     nom;	/* number of members */
    hsize_t size;	/* size of compound data */
    NclHDF5compound_component_t member[MAX_COMPOUND_COMPONENTS];
} NclHDF5compound_t;

typedef struct _NclHDF5data_t
{
    hid_t    id;                                    /* Data id */
    char     name[HDF5_NAME_LEN];                   /* Data name */
    char     type[HDF5_NAME_LEN];                   /* Data type, such as integer, float, etc. */
    char     dim_name[H5S_MAX_RANK][HDF5_NAME_LEN]; /* Dimension name */

    int      ndims;                                 /* number of dimensions */
    hsize_t  dims[H5S_MAX_RANK];                    /* dimensions */

    unsigned long  nbytes;                          /* number of bytes for value */
    void          *value;                           /* Data value */
    int            is_str;                          /* is data string */
} NclHDF5data_t;

typedef struct _NclHDF5datatype_t
{
    char     type_name[HDF5_NAME_LEN];     /* Data type name, such as integer, float, etc. */
    char     endian[HDF5_NAME_LEN];        /* Data Endian, such as little-endian, big-endian, etc. */
    char     format[HDF5_NAME_LEN];        /* The data format, such as IEEE, etc. */

    unsigned bit;               /* the size of the data (how many bits). */
    unsigned usign;             /* is it unsigned? 1, yes, 0 no. */
    int      ndims;             /* number of dimensions */
    hsize_t  dims[H5S_MAX_RANK];/* dimensions */

    NclHDF5compound_t compound;
} NclHDF5datatype_t;

typedef struct _NclHDF5external_link_t NclHDF5external_link_t;

/* Struct to keep track of external link targets visited */
struct _NclHDF5external_link_t
{
    char file[HDF5_NAME_LEN];
    char path[HDF5_NAME_LEN];

    NclHDF5external_link_t *next;
};

typedef struct _NclHDF5var_list_t NclHDF5var_list_t;

/* Struct to keep track of external link targets visited */
struct _NclHDF5var_list_t
{
    int     ndims;                                 /* number of dimensions */
    int     nconf;                                 /* number of conformed dimensions */
    char    name[HDF5_NAME_LEN];                   /* Full name of this variable */
    hsize_t dims[H5S_MAX_RANK];                    /* dimensions */
    char    dim_name[H5S_MAX_RANK][HDF5_NAME_LEN]; /* Dimension name */

    NclHDF5var_list_t *next;
};

typedef struct _NclHDF5dim_list_t NclHDF5dim_list_t;

struct _NclHDF5dim_list_t
{
    char name[HDF5_NAME_LEN];
    hsize_t  size;

    NclHDF5dim_list_t *next;
};

typedef struct _NclHDF5dim_info_t NclHDF5dim_info_t;

struct _NclHDF5dim_info_t
{
    int ndims;

    NclHDF5dim_list_t *dim_list;
};

typedef struct NclHDF5attr_node_t
{
    hid_t id;            /* Attribute id */
    hid_t type;          /* Attribute type */
    hid_t p_type;        /* Attribute p_type */
    hid_t space;         /* Attribute space */

    char  name[HDF5_NAME_LEN];          /* Attribute name */
    char  dataspace[HDF5_NAME_LEN];     /* Attribute dataspace name */
    char  type_name[HDF5_NAME_LEN];     /* Attribute type name, integer, float, etc. */

    int     counter;       /* Attributes counter */
    int     nbytes;        /* Number of bytes */
    int     ndims;         /* Number of dimensions */

    hsize_t dims[H5S_MAX_RANK];        /* Attribute dimensions */
    void   *value;                     /* Attribute value */

    H5S_class_t         space_type;
} NclHDF5attr_node_t;

typedef struct _NclHDF5attr_list_t NclHDF5attr_list_t;

struct _NclHDF5attr_list_t
{
    NclHDF5attr_node_t *attr_node;

    NclHDF5attr_list_t *next;
};

typedef struct NclHDF5dataset_node_t
{
    char        name[HDF5_NAME_LEN];         /* Dataset name */
    char        type_name[HDF5_NAME_LEN];    /* Data type name, such as integer, float, etc. */
    char        short_name[HDF5_NAME_LEN];   /* Data short name */
    char        group_name[HDF5_NAME_LEN];   /* Data group name */
    char        endian[HDF5_NAME_LEN];       /* Data Endian, such as little-endian, big-endian, etc. */

    hid_t       id;            /* data id */
    hid_t       type;          /* data type */
    hid_t       space;         /* data space */
    int         ndims;         /* number of dimensions */
    int         counter;       /* Dataset counter */
    hsize_t     dims[H5S_MAX_RANK];          /* dimensions */
    char        space_name[HDF5_NAME_LEN];    /* space type name. */
    char        dim_name[H5S_MAX_RANK][HDF5_NAME_LEN]; /* Dimension name */

    int         deflate_pass;                      /* defalte pass */
    int         nchunkdims;                        /* number of chunk dimensions */
    hsize_t     chunk_dims[H5S_MAX_RANK];          /* dimensions */

    H5S_class_t space_type;    /* type of dataspace */

    int                     num_attrs;     /* number of Attributes */
    NclHDF5attr_list_t     *attr_list;     /* Attribute list */
    NclHDF5compound_t      compound;
} NclHDF5dataset_node_t;

typedef struct _NclHDF5dataset_list_t NclHDF5dataset_list_t;

struct _NclHDF5dataset_list_t
{
    NclHDF5dataset_node_t *dataset_node;

    NclHDF5dataset_list_t *next;
};

typedef struct _NclHDF5group_list_t NclHDF5group_list_t;

typedef struct NclHDF5group_node_t
{
    char file[HDF5_NAME_LEN];                 /* group in file(name) */
    char name[HDF5_NAME_LEN];                 /* group name */ 
    char parent_name[HDF5_NAME_LEN];          /* parent-group name */ 
    hid_t id;                                 /* Group id */
    int   counter;                            /* Group counter */

    H5O_type_t                   type;	                      /* Basic object type (group, dataset, etc.) */
    char                         type_name[HDF5_NAME_LEN];    /* Record type name */

    size_t                       num_attrs;       /* number of Attributes */
    NclHDF5attr_list_t          *attr_list;       /* Attribute list */

    size_t                       num_links;       /* number of external links */
    NclHDF5external_link_t      *elink_list;      /* List of visited external links */

    size_t                       num_datasets;        /* number of datasets */
    NclHDF5dataset_list_t       *dataset_list;    /* List of variables */

    size_t                       num_groups;      /* number of groups */
    NclHDF5group_list_t         *group_list;      /* List of groups */

    NclHDF5dim_info_t           *dim_info;        /* dimension info */
} NclHDF5group_node_t;

typedef struct _NclHDF5group_list_t
{
    NclHDF5group_node_t *group_node;
    NclHDF5group_list_t *next;
} _NclHDF5group_list_t;

typedef struct _string_list_t string_list_t;

typedef struct _string_list_t
{
    char *str;
    string_list_t *next;
} _string_list_t;

typedef struct string_queue_t
{
    int ns;
    string_list_t *head;
    string_list_t *tail;
} string_queue_t;

herr_t _NclHDF5search_obj(char *name, H5O_info_t *oinfo, void *_NclHDF5obj, char *already_seen);
herr_t _NclHDF5search_link(char *name, H5O_info_t *oinfo, void *_NclHDF5link);

/* Typedefs for serach functions */
typedef herr_t (*_NclHDF5search_obj_func_t) (char *path_name, H5O_info_t *oinfo, void *udata, char *already_seen);
typedef herr_t (*_NclHDF5search_link_func_t) (char *path_name, H5L_info_t *linfo, void *udata);

typedef struct
{
    _NclHDF5search_obj_func_t  _NclHDF5search_obj;        /* Callback for objects */
    _NclHDF5search_link_func_t _NclHDF5search_link;       /* Callback for links */
    void *udata;                /* User data pass to callbacks */
} NclHDF5searcher_t;

typedef struct
{
    size_t      nalloc;
    size_t      nused;

    struct
    {
        haddr_t addr;
        char    path[HDF5_NAME_LEN];
    } *objs;
} NclHDF5_addr_t;

typedef struct
{
    NclHDF5_addr_t *seen;           /* List of addresses seen already */
    NclHDF5searcher_t *searcher;    /* Information for visiting each link/object */
    hbool_t is_absolute;            /* Whether the traversal has absolute paths */
    const char *base_grp_name;      /* Name of the group that serves as the base */
} NclHDF5_ud_traverse_t;

void _NclHDF5Print_data_value(void *value, int ndims, hsize_t *dims, char *type_name);

herr_t _NclHDF5dataset_info(hid_t dset, char *name, NclHDF5dataset_node_t *dataset_node);
herr_t _NclHDF5dataset_attr(hid_t dset, char *name, NclHDF5dataset_node_t *dataset_node);
herr_t _NclHDF5search_by_name(hid_t loc_id, char *path, H5L_info_t *linfo, void *_udata);

void _NclHDF5_addr_add(NclHDF5_addr_t *visited, haddr_t addr, char *path);
void setHDF5endian(hid_t type, hsize_t size, NclHDF5datatype_t *NclHDF5datatype);

char *_NclHDF5_addr_visited(NclHDF5_addr_t *visited, haddr_t addr);

NclHDF5datatype_t *_NclHDF5get_typename(hid_t type, int ind);

herr_t _NclHDF5check_attr(hid_t obj_id, char *attr_name, const H5A_info_t *ainfo,
                          void *attr_data);

void _NclHDF5print_group(NclHDF5group_node_t *HDF5group);
void _NclHDF5free_group(NclHDF5group_node_t *HDF5group);

void _NclHDF5var_list(NclHDF5var_list_t **HDF5var_list, NclHDF5group_node_t *HDF5group);

NclHDF5data_t *_NclHDF5get_data_with_name(hid_t fid, char *dataset_name, NclHDF5group_node_t *HDF5group,
	                                  long *start, long *finish, long *stride);

NclHDF5group_node_t *_NclHDF5allocate_group(hid_t fid, const char *fname, char *gname, H5O_type_t type);
herr_t _NclHDF5recursive_check(hid_t fid, char *grp_name,
                               _NclHDF5search_obj_func_t  _NclHDF5search_obj,
                               _NclHDF5search_link_func_t _NclHDF5search_link,
                               NclHDF5group_node_t *HDF5group);
herr_t _NclHDF5check_obj(const char *filename,
                         NclHDF5group_node_t **HDF5group);

NclHDF5group_node_t *_find_HDF5Group(char *name, NclHDF5group_node_t *group_node);
NclHDF5data_t *_NclHDF5allocate_data(hid_t id);
void _NclHDF5free_data(NclHDF5data_t *);
hid_t Ncl2HDF5type(const char *type);
unsigned long NclHDF5sizeof(const char *type);

void _NclFree_HDF5dim_list(NclHDF5dim_list_t *NclHDF5dim_list);
NclHDF5data_t *_NclHDF5allocate_data(hid_t id);

NclHDF5data_t *_NclHDF5get_data_with_id(hid_t fid, hid_t did, NclHDF5group_node_t *HDF5group);
unsigned char *_NclHDF5get_dataset(hid_t fid, char *dataset_name, hid_t dset, char *type_name);
unsigned char *_NclHDF5get_simple_dataset(hid_t dset, hid_t p_type, char *type_name);
unsigned char *_NclHDF5get_native_dataset(hid_t fid, char *dataset_name, char *type_name,
                                          NclHDF5compound_t *compound,
                                          const char *component, int *is_str,
	                                  long *start, long *finish, long *stride);

char *_find_parent_group_name(char *name);
char *_get_short_name(char *name);
char *_get_group_name(char *name);

string_queue_t *_split_string2queue(char *str, char *delim);
void _free_string_queue(string_queue_t *sq);

NclHDF5group_node_t *_find_group(char *groupname, string_list_t *sl, int depth,
                                 NclHDF5group_node_t *group_node);
NclHDF5group_node_t *_find_group_with_name(char *groupname,
                                           NclHDF5group_node_t *group_node);
NclHDF5dataset_node_t *_find_dataset(char *dataname, NclHDF5group_node_t *group_node);
NclHDF5attr_node_t *_find_dataset_attribute(char *attrname,
                                            NclHDF5dataset_node_t *dataset_node);
NclHDF5attr_node_t *_find_group_attribute(char *attrname,
                                          NclHDF5group_node_t *group_node);

herr_t _write_group_attribute(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                              char *typename, char *attrname,
                              char *groupname, NclHDF5group_node_t *group_node);
herr_t _write_dataset_attribute(hid_t did, NclHDF5attr_node_t *attr_node);

herr_t _addH5dataset(hsize_t rank, hsize_t *dims,
                     char *typename, char *dataname,
                     NclHDF5group_node_t *group_node);
herr_t _addChunk2H5dataset(hsize_t rank, hsize_t *dims,
                           char *dataname,
                           NclHDF5group_node_t *group_node);

int _write_chunkedH5dataset(hid_t fid, hsize_t rank,
                            hsize_t *dims, hsize_t *chunk_dims, void *data,
                            char *typename, char *dataname,
                            NclHDF5group_node_t *group_node);

int _NclHDF5print_simple_dataset(hid_t dset, hid_t p_type, char *type_name);
hid_t _get_groupID(NclHDF5dataset_node_t *dataset_node,
                     NclHDF5group_node_t *group_node);

int _add_attr2dataset(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                      char *typename, char *attrname,
                      char *datasetname, NclHDF5group_node_t *group_node);
int _add_attr2group(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                    char *typename, char *attrname,
                    char *groupname, NclHDF5group_node_t *group_node);
int _writeH5dataset(hid_t fid, hsize_t rank, hsize_t *dims, void *data,
                    char *typename, char *dataname,
                    NclHDF5group_node_t *group_node);
#endif


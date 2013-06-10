#include "h5data_struct.h"

#define H5_BUFSIZE         (1024 * 1024)

/*
 ***********************************************************************
 * Function:	_writeH5dataset
 *
 * Purpose:	Write a dataset.
 *
 * Return:	NclHDF5dataset_node_t *
 *
 * Programmer:	Wei Huang
 * Created:	March 11, 2010
 *
 ***********************************************************************
 */

int _writeH5dataset(hid_t fid, hsize_t rank, hsize_t *dims, void *data,
                    char *typename, char *dataname,
                    NclHDF5group_node_t *group_node)
{
    NclHDF5dataset_node_t *dataset_node;

    NclHDF5attr_list_t *curAttrList;
    NclHDF5attr_node_t *curAttrNode;

    H5T_order_t h5order;

    int i;
    herr_t      status;
    hid_t groupID = 0;
    hsize_t *max_dims = NclCalloc(rank, sizeof(hsize_t));

    dataset_node = _find_dataset(dataname, group_node);

    strcpy(dataset_node->type_name, typename);

    groupID = _get_groupID(dataset_node, group_node);

    dataset_node->ndims = rank;
    for (i=0; i<dataset_node->ndims; i++)
    {
        dataset_node->dims[i] = dims[i];
        max_dims[i] = H5S_UNLIMITED;
    }

  /*
   *dataset_node->space = H5Screate_simple(rank, dims, max_dims);
   */
    dataset_node->space = H5Screate_simple(rank, dims, NULL);
    dataset_node->type  = H5Tcopy(Ncl2HDF5type(typename));
    free(max_dims);

    h5order = H5Tget_order(Ncl2HDF5type(typename));
    status = H5Tset_order(dataset_node->type, h5order);

    dataset_node->id = H5Dcreate(fid,
                                 dataset_node->name,
                                 dataset_node->type,
                                 dataset_node->space,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);;

  /*Write the data to the dataset using default transfer properties.*/
    status = H5Dwrite(dataset_node->id, Ncl2HDF5type(typename), H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

    curAttrList = dataset_node->attr_list;
    for(i = 0; i < dataset_node->num_attrs; i++)
    {
        curAttrNode = curAttrList->attr_node;
        _write_dataset_attribute(dataset_node->id, curAttrNode);
        curAttrList = curAttrList->next;
    }

    H5Sclose(dataset_node->space);
    H5Tclose(dataset_node->type);
    H5Dclose(dataset_node->id);

#if 0
    /* Data type */
    NclHDF5datatype = _NclHDF5get_typename(dataset_node->type, 15);

    strcpy(dataset_node->type_name, NclHDF5datatype->type_name);

    if(NclHDF5datatype->compound_nom)
    {
        dataset_node->compound = NclCalloc(1, sizeof(NclHDF5compound_t));
        if(! dataset_node->compound)
        {
            fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for dataset_node->compound, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            free(NclHDF5datatype);
            return FAILED;
        }
        dataset_node->compound->member = NclCalloc(NclHDF5datatype->compound_nom,
                                                sizeof(NclHDF5compound_component_list_t));
        if(! dataset_node->compound->member)
        {
            fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for dataset_node->compound->member, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            free(NclHDF5datatype);
            return FAILED;
        }

        dataset_node->compound->nom = NclHDF5datatype->compound_nom;
        dataset_node->compound->size = NclHDF5datatype->compound_size;

        for(i=0; i<NclHDF5datatype->compound_nom; i++)
        {
            strcpy(dataset_node->compound->member[i].name, NclHDF5datatype->compound_name[i]);
            strcpy(dataset_node->compound->member[i].type, NclHDF5datatype->compound_type[i]);
            dataset_node->compound->member[i].offset = NclHDF5datatype->compound_offset[i];
            dataset_node->compound->member[i].type_id = NclHDF5datatype->compound_type_id[i];
        }
    }

    free(NclHDF5datatype);
#endif

    if(groupID)
        H5Gclose(groupID);

    return SUCCEED;
}


/*
 *************************************************************************
 * Function:	_get_groupID
 *
 * Purpose:	add dataset to _NclHDF5group, and get group id
 *
 * Return:	Success: SUCCEED
 *		Failure: FAILED
 *
 * Programmer:	Wei Huang
 *              July 1, 2009
 *
 *************************************************************************
 */

hid_t _get_groupID(NclHDF5dataset_node_t *dataset_node,
                     NclHDF5group_node_t *group_node)
{
    NclHDF5group_node_t *parent_group_node = NULL;
    NclHDF5group_node_t *current_group_node = NULL;
    NclHDF5group_list_t *current_group_list;

    NclHDF5dataset_list_t *current_dataset_list;
    NclHDF5dataset_node_t *current_dataset_node;

    char *sn = _get_short_name(dataset_node->name);
    char *parent_group_name = (char *)_find_parent_group_name(dataset_node->name);

    int i;
    int new_dataset = 1;
    hid_t groupID = 0;

    strcpy(dataset_node->short_name, sn);

    if(strlen(dataset_node->group_name))
    {
        int new_group = 1;

        parent_group_node = _find_HDF5Group(parent_group_name, group_node);

        current_group_list = parent_group_node->group_list;
        for(i = 0; i < parent_group_node->num_groups; i++)
        {
            current_group_node = current_group_list->group_node;

            if(0 == strcmp(dataset_node->group_name, current_group_node->name))
            {
                groupID = H5Gopen2(current_group_node->id, dataset_node->group_name, H5P_DEFAULT);
                new_group = 0;
                break;
            }
            current_group_list = current_group_list->next;
        }

        if(new_group)
        {
            groupID = H5Gcreate2(group_node->id, dataset_node->group_name, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

            current_group_list = NclCalloc(1, sizeof(NclHDF5group_list_t));
            current_group_node = _NclHDF5allocate_group(groupID, parent_group_node->file,
                                                        dataset_node->group_name, H5O_TYPE_GROUP);
            current_group_list->group_node = current_group_node;
            current_group_list->next = parent_group_node->group_list;
            parent_group_node->group_list = current_group_list;
            parent_group_node->num_groups += 1;
        }
    }
    else
    {
        current_group_node = group_node;
    }

    current_dataset_list = current_group_node->dataset_list;
    for(i=0; i<current_group_node->num_datasets; i++)
    {
        current_dataset_node = current_dataset_list->dataset_node;
        if(0 == strcmp(dataset_node->name, current_dataset_node->name))
        {
            new_dataset = 0;
            break;
        }
        current_dataset_list = current_dataset_list->next;
    }

    if(new_dataset)
    {
        current_dataset_list = NclCalloc(1, sizeof(NclHDF5dataset_list_t));
        if(!current_dataset_list)
        {
            fprintf(stdout, "Failed to allocated memory for current_dataset_list. in file: %s, line: %d\n",
                        __FILE__, __LINE__);
            return 0;
        }

        current_dataset_list->dataset_node = dataset_node;
        current_dataset_list->next = current_group_node->dataset_list;
        current_group_node->dataset_list = current_dataset_list;
        current_group_node->num_datasets ++;
    }
  /*
   *else
   *{
   *    wei_start("_get_groupID", __FILE__, __LINE__);
   *    wei_check_str("_get_groupID", "old dataset_node name", dataset_node->name);
   *    wei_end("_get_groupID", __FILE__, __LINE__);
   *}
   */

    if(parent_group_name !=NULL)
        free(parent_group_name);

    return groupID;
}


/*
 ***********************************************************************
 * Function:	_add_attr2group
 *
 * Purpose:	Add attributes to group.
 *
 * Return:	Success: SUCCEED
 *		Failure: FAILED
 *
 * Programmer:	Wei Huang
 * Created:	March 15, 2010
 *
 ***********************************************************************
 */

int _add_attr2group(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                    char *typename, char *attrname,
                    char *groupname, NclHDF5group_node_t *group_node)
{
    NclHDF5group_node_t *current_group_node = NULL;
    NclHDF5attr_node_t *curAttrNode;
    int size = 1;
    int i;

    if(! group_node)
    {
        return 0;
    }

    current_group_node = _find_group_with_name(groupname, group_node);
    curAttrNode = _find_group_attribute(attrname, current_group_node);

    curAttrNode->ndims = rank;
    for(i = 0; i < rank; i++)
    {
        size *= dims[i];
        curAttrNode->dims[i] = dims[i];
    }

    strcpy(curAttrNode->type_name, typename);

    if(0 == strcmp("string", typename))
    {
      /*
       *Create string attribute.
       */
        size++;
        curAttrNode->nbytes = size;
    }
    else
    {
      /*
       *Create dataspace for the first attribute.
       */
        curAttrNode->nbytes = size* (int)NclHDF5sizeof(typename);
    }

    curAttrNode->value = (void *) NclMalloc(curAttrNode->nbytes);
    memcpy(curAttrNode->value, attrdata, curAttrNode->nbytes);

    _write_group_attribute(fid, rank, dims, attrdata,
                           typename, attrname, groupname, current_group_node);

    return ( 1 );
}

herr_t _write_attribute(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                        char *typename, char *attrname,
                        char *datasetname, NclHDF5attr_node_t *attr_node)
{
    hid_t   did;
    hid_t   aid;                /* Attribute dataspace identifiers */
    hid_t   atype, h5type;      /* Attribute type */
    hid_t   attr;

    herr_t  ret;                /* Return value */
 
    int size = 1;
    int i = 0;
    
    did = H5Dopen2(fid, datasetname, H5P_DEFAULT);

    attr_node->ndims = rank;
    for(i = 0; i < rank; i++)
    {
        size *= dims[i];
        attr_node->dims[i] = dims[i];
    }

    h5type = Ncl2HDF5type(typename);

  /*
   *if(0 == strcmp("string", typename))
   */
    if(h5type == H5T_NATIVE_CHAR)
    {
      /*
       *Create string attribute.
       */
        size++;
        aid   = H5Screate(H5S_SCALAR);
        atype = H5Tcopy(H5T_C_S1);
                H5Tset_size(atype, size);
                H5Tset_strpad(atype,H5T_STR_NULLTERM);
        attr_node->nbytes = size;
    }
    else
    {
      /*
       *Create dataspace for the first attribute.
       */
        aid   = H5Screate(H5S_SIMPLE);
        atype = H5Tcopy(h5type);
        ret   = H5Sset_extent_simple(aid, rank, dims, NULL);
        attr_node->nbytes = size* (int)NclHDF5sizeof(typename);
    }
  /*
    fprintf(stdout, "\tsize = <%d>\n", size);
    fprintf(stdout, "\tattr_node->nbytes = <%d>\n", attr_node->nbytes);
   */

    attr_node->value = (void *) NclMalloc(attr_node->nbytes);
    memcpy(attr_node->value, attrdata, attr_node->nbytes);

  /*
   *Create attribute.
   */
    attr = H5Acreate2(did, attrname, atype, aid, H5P_DEFAULT, H5P_DEFAULT);

  /*
   *Write attribute.
   */
    ret = H5Awrite(attr, atype, attrdata);

    strcpy(attr_node->name, attrname);
    attr_node->id = aid;
    attr_node->type = atype;
    attr_node->space = attr;

  /*
   *Close attribute and datatype.
   */
    ret = H5Sclose(aid);
    ret = H5Tclose(atype);

  /*
   *Close the attributes.
   */
    ret = H5Aclose(attr);
    ret = H5Dclose(did);
    
    return ret;
}


/*
 ***********************************************************************
 * Function:	_add_attr2dataset
 *
 * Purpose:	Add attributes to dataset.
 *
 * Return:	Success: 1
 *		Failure: 0
 *
 * Programmer:	Wei Huang
 * Created:	March 16, 2010
 *
 ***********************************************************************
 */

int _add_attr2dataset(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                      char *typename, char *attrname,
                      char *datasetname, NclHDF5group_node_t *group_node)
{
    NclHDF5dataset_node_t *current_dataset_node;
    NclHDF5attr_node_t *curAttrNode;
    hid_t did;
    int size = 1;
    int i;

    if(! group_node)
    {
        return 0;
    }

    current_dataset_node = _find_dataset(datasetname, group_node);
    curAttrNode = _find_dataset_attribute(attrname, current_dataset_node);

    curAttrNode->ndims = rank;
    for(i = 0; i < rank; i++)
    {
        size *= dims[i];
        curAttrNode->dims[i] = dims[i];
    }

    strcpy(curAttrNode->type_name, typename);

    if((0 == strcmp("string", typename)) || (0 == strcmp("char", typename)))
    {
      /*
       *Create string attribute.
       */
        size++;
        curAttrNode->nbytes = size;
    }
    else
    {
      /*
       *Create dataspace for the first attribute.
       */
        curAttrNode->nbytes = size* (int)NclHDF5sizeof(typename);
    }

    curAttrNode->value = (void *) NclMalloc(curAttrNode->nbytes);
    memcpy(curAttrNode->value, attrdata, curAttrNode->nbytes);

    group_node->id = H5Fopen(group_node->file, H5F_ACC_RDWR, H5P_DEFAULT);
    did = H5Dopen(group_node->id, datasetname, H5P_DEFAULT);
    _write_dataset_attribute(did, curAttrNode);
    H5Dclose(did);

    return SUCCEED;
}

#if 0
#define H5FILE_NAME   "SDScompound.h5"
#define DATASETNAME   "ArrayOfStructures"
#define LENGTH        10
#define RANK          1

int sample_to_write_compound_data(void)
{

    /* First structure  and dataset*/
    typedef struct s1_t {
	int    a;
	float  b;
	double c;
    } s1_t;
    s1_t       s1[LENGTH];
    hid_t      s1_tid;     /* File datatype identifier */

    /* Second structure (subset of s1_t)  and dataset*/
    typedef struct s2_t {
	double c;
	int    a;
    } s2_t;
    s2_t       s2[LENGTH];
    hid_t      s2_tid;    /* Memory datatype handle */

    /* Third "structure" ( will be used to read float field of s1) */
    hid_t      s3_tid;   /* Memory datatype handle */
    float      s3[LENGTH];

    int        i;
    hid_t      file, dataset, space; /* Handles */
    herr_t     status;
    hsize_t    dim[] = {LENGTH};   /* Dataspace dimensions */


    /*
     * Initialize the data
     */
    for (i = 0; i< LENGTH; i++) {
        s1[i].a = i;
        s1[i].b = i*i;
        s1[i].c = 1./(i+1);
    }

    /*
     * Create the data space.
     */
    space = H5Screate_simple(RANK, dim, NULL);

    /*
     * Create the file.
     */
    file = H5Fcreate(H5FILE_NAME, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Create the memory data type.
     */
    s1_tid = H5Tcreate (H5T_COMPOUND, sizeof(s1_t));
    H5Tinsert(s1_tid, "a_name", HOFFSET(s1_t, a), H5T_NATIVE_INT);
    H5Tinsert(s1_tid, "c_name", HOFFSET(s1_t, c), H5T_NATIVE_DOUBLE);
    H5Tinsert(s1_tid, "b_name", HOFFSET(s1_t, b), H5T_NATIVE_FLOAT);

    /*
     * Create the dataset.
     */
    dataset = H5Dcreate(file, DATASETNAME, s1_tid, space, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

    /*
     * Wtite data to the dataset;
     */
    status = H5Dwrite(dataset, s1_tid, H5S_ALL, H5S_ALL, H5P_DEFAULT, s1);

    /*
     * Release resources
     */
    H5Tclose(s1_tid);
    H5Sclose(space);
    H5Dclose(dataset);
    H5Fclose(file);

    return 0;
}
#endif

string_queue_t *_split_string2queue(char *str, char *delim)
{
    char *tmp_str = strdup(str);
    char *result = NULL;
    string_queue_t *sq;
    string_list_t *slist;
    
    sq = NclCalloc(1, sizeof(string_queue_t));
    sq->ns = 0;

    result = strtok(tmp_str, delim);

    while(result != NULL)
    {
        slist = NclCalloc(1, sizeof(string_list_t));
        if(slist == NULL)
        {
            fprintf(stdout, "Failed to allocated memory for slist.\n");
            fprintf(stdout, "file: %s, line: %d\n", __FILE__, __LINE__);
            return sq;
        }

        slist->str = strdup(result);
        slist->next = NULL;
        if(sq->ns)
        {
            sq->tail->next = slist;
            sq->tail = sq->tail->next;
        }
        else
        {
            sq->head = slist;
            sq->tail = sq->head;
        }
        sq->ns++;
        result = strtok(NULL, delim);
    }

    free(tmp_str);
    return sq;
}

void _free_string_queue(string_queue_t *sq)
{
    string_list_t *slist;
    int i;
    for(i = 0; i < sq->ns; i++)
    {
        slist = sq->head;
        sq->head = slist->next;
        slist->next = NULL;
        free(slist->str);
        free(slist);
    }
    free(sq);
}

void _print_string_queue(string_queue_t *sq)
{
    string_list_t *slist;
    int i;

    fprintf(stdout, "\n\nSTRING QUEUE\n\n");
    slist = sq->head;
    for(i = 0; i < sq->ns; i++)
    {
        fprintf(stdout, "\tString %d: <%s>\n", i, slist->str);
        slist = slist->next;
    }
}


/*
 *************************************************************************
 * Function:    _find_group
 *
 * Purpose:     find current group_node
 *
 * Return:      current group_node
 *
 * Programmer:  Wei Huang
 *              April, 1, 2010
 *
 *************************************************************************
 */

NclHDF5group_node_t *_find_group(char *groupname, string_list_t *sl, int depth,
                                 NclHDF5group_node_t *group_node)
{
    if(depth <= 1)
    {
        return(group_node);
    }
    else
    {
        NclHDF5group_list_t *current_group_list;
        NclHDF5group_node_t *current_group_node = NULL;
        char new_groupname[HDF5_NAME_LEN];
        hid_t groupID = 0;
        int i;

        strcpy(new_groupname, groupname);
        strcat(new_groupname, "/");
        strcat(new_groupname, sl->str);

        current_group_list = group_node->group_list;
        for(i = 0; i < group_node->num_groups; i++)
        {
            current_group_node = current_group_list->group_node;
            if(0 == strcmp(new_groupname, current_group_node->name))
            {
                return(_find_group(new_groupname, sl->next, depth-1, current_group_node));
            }
            current_group_list = current_group_list->next;
        }

        current_group_list = NclCalloc(1, sizeof(NclHDF5group_list_t));
        if(current_group_list == NULL)
        {
            fprintf(stdout, "Failed to allocated memory for current_group_list.\n");
            fprintf(stdout, "file: %s, line: %d\n", __FILE__, __LINE__);
            exit(-1);
        }

        groupID = H5Gcreate2(group_node->id, new_groupname,
                             H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

        current_group_node = _NclHDF5allocate_group(groupID, group_node->file,
                                                    new_groupname, H5O_TYPE_GROUP);
        current_group_list->group_node = current_group_node;
        current_group_list->next = group_node->group_list;
        group_node->group_list = current_group_list;
        group_node->num_groups += 1;

        return(_find_group(new_groupname, sl->next, depth-1, current_group_node));
    }
}


/*
 *************************************************************************
 * Function:    _find_group_with_name
 *
 * Purpose:     find group_node with group name
 *
 * Return:      group_node with group name
 *
 * Programmer:  Wei Huang
 *              April, 1, 2010
 *
 *************************************************************************
 */

NclHDF5group_node_t *_find_group_with_name(char *groupname,
                                           NclHDF5group_node_t *group_node)
{
    string_queue_t *sq = NULL;
    string_list_t *sl = NULL;
    NclHDF5group_list_t *current_group_list = NULL;
    NclHDF5group_node_t *current_group_node = NULL;
    NclHDF5group_node_t *parent_group_node = group_node;
    char new_groupname[HDF5_NAME_LEN];
    char tmp_groupname[HDF5_NAME_LEN];
    hid_t groupID = 0;
    int i, n, found;

    if(groupname[0] != '/')
    {
         strcpy(new_groupname, "/");
         strcat(new_groupname, groupname);
    }
    else
    {
         strcpy(new_groupname, groupname);
    }

    if(strlen(groupname) < 2)
    {
        current_group_node = group_node;
        return current_group_node;
    }

    sq = _split_string2queue(new_groupname, "/");
    tmp_groupname[0] = '\0';
    sl = sq->head;
    for(n = 0; n < sq->ns; n++)
    {
        strcat(tmp_groupname, "/");
        strcat(tmp_groupname, sl->str);
        sl = sl->next;

        found = 0;
        current_group_list = parent_group_node->group_list;
        for(i = 0; i < parent_group_node->num_groups; i++)
        {
            current_group_node = current_group_list->group_node;
            if(0 == strcmp(tmp_groupname, current_group_node->name))
            {
                found = 1;
                parent_group_node = current_group_node;
                break;
            }
            current_group_list = current_group_list->next;
        }

        if(! found)
        {
            current_group_list = NclCalloc(1, sizeof(NclHDF5group_list_t));
            if(current_group_list == NULL)
            {
                fprintf(stdout, "Failed to allocated memory for current_group_list.\n");
                fprintf(stdout, "file: %s, line: %d\n", __FILE__, __LINE__);
                exit(-1);
            }

            groupID = H5Gcreate2(group_node->id, new_groupname,
                                 H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);

            current_group_node = _NclHDF5allocate_group(groupID, group_node->file,
                                                        new_groupname, H5O_TYPE_GROUP);
            current_group_list->group_node = current_group_node;
            current_group_list->next = group_node->group_list;
            parent_group_node->group_list = current_group_list;
            parent_group_node->num_groups += 1;
        }
    }

    return current_group_node;
}

herr_t _write_group_attribute(hid_t fid, hsize_t rank, hsize_t *dims, void *attrdata,
                              char *typename, char *attrname,
                              char *groupname, NclHDF5group_node_t *group_node)
{
    hid_t   gid;
    hid_t   aid;                /* Attribute dataspace identifiers */
    hid_t   atype, h5type;      /* Attribute type */
    hid_t   attr;
    herr_t  ret;                /* Return value */
    int size = 1;
    int i = 0;
    int found_attribute = 0;
    NclHDF5attr_list_t *curAttrList;
    NclHDF5attr_node_t *attr_node;
    
    gid = H5Gopen2(fid, groupname, H5P_DEFAULT);

    curAttrList = group_node->attr_list;
    for(i = 0; i < group_node->num_attrs; i++)
    {
        attr_node = curAttrList->attr_node;
        if(0 == strcmp(attrname, attr_node->name))
        {
            found_attribute = 1;
            break;
        }
    }

    if(! found_attribute)
    {
        curAttrList = NclCalloc(1, sizeof(NclHDF5attr_list_t));
        if(!curAttrList)
        {
            fprintf(stdout, "Failed to allocated memory for curAttrList. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return -1;
        }

        curAttrList->next = group_node->attr_list;
        group_node->attr_list = curAttrList;

        attr_node = NclCalloc(1, sizeof(NclHDF5attr_node_t));
        if(!attr_node)
        {
            fprintf(stdout, "Failed to allocated memory for attr_node. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return -1;
        }

        curAttrList->attr_node = attr_node;

        group_node->num_attrs ++;
    }

    attr_node->ndims = rank;
    for(i = 0; i < rank; i++)
    {
        size *= dims[i];
        attr_node->dims[i] = dims[i];
    }

    h5type = Ncl2HDF5type(typename);

    if(0 == strcmp("string", typename))
    {
      /*
       *Create string attribute.
       */
        size++;
        aid   = H5Screate(H5S_SCALAR);
        atype = H5Tcopy(H5T_C_S1);
                H5Tset_size(atype, size);
                H5Tset_strpad(atype,H5T_STR_NULLTERM);
        attr_node->nbytes = size;
    }
    else
    {
      /*
       *Create dataspace for the first attribute.
       */
        aid   = H5Screate(H5S_SIMPLE);
        atype = H5Tcopy(h5type);
        ret   = H5Sset_extent_simple(aid, rank, dims, NULL);
        attr_node->nbytes = size* (int)NclHDF5sizeof(typename);
    }
  /*
    fprintf(stdout, "\tsize = <%d>\n", size);
    fprintf(stdout, "\tattr_node->nbytes = <%d>\n", attr_node->nbytes);
   */

    attr_node->value = (void *) NclMalloc(attr_node->nbytes);
    memcpy(attr_node->value, attrdata, attr_node->nbytes);

  /*
   *Create attribute.
   */
    attr = H5Acreate2(gid, attrname, atype, aid, H5P_DEFAULT, H5P_DEFAULT);

  /*
   *Write attribute.
   */
    ret = H5Awrite(attr, atype, attrdata);

    strcpy(attr_node->name, attrname);
    attr_node->id = aid;
    attr_node->type = atype;
    attr_node->space = attr;

  /*
   *Close attribute and datatype.
   */
    ret = H5Sclose(aid);
    ret = H5Tclose(atype);

  /*
   *Close the attributes.
   */
    ret = H5Aclose(attr);
    ret = H5Gclose(gid);
    
    return ret;
}


/*
 ***********************************************************************
 * Function:	_find_dataset
 *
 * Purpose:	find a dataset in group_node.
 *
 * Return:	NclHDF5dataset_node_t *
 *		FAILED
 *
 * Programmer:	Wei Huang
 * Created:	April 5, 2010
 *
 ***********************************************************************
 */

NclHDF5dataset_node_t *_find_dataset(char *dataname, NclHDF5group_node_t *group_node)
{
    NclHDF5group_node_t *current_group_node;
    NclHDF5dataset_list_t *current_dataset_list;
    NclHDF5dataset_node_t *current_dataset_node;

    int i;
    int new_dataset = 1;

    string_queue_t *sq;
    string_list_t *sl;

    char fullname[HDF5_NAME_LEN];
    char groupname[HDF5_NAME_LEN];
    char shortname[HDF5_NAME_LEN];

    if(dataname[0] == '/')
    {
        strcpy(fullname, dataname);
    }
    else
    {
        strcpy(fullname, "/");
        strcat(fullname, dataname);
    }

    groupname[0] = '\0';
    sq = _split_string2queue(fullname, "/");
    if(sq->ns)
    {
        sl = sq->head;
        for(i = 0; i < sq->ns - 1; i++)
        {
            strcat(groupname, "/");
            strcat(groupname, sl->str);
            sl = sl->next;
        }
        strcpy(shortname, sl->str);
    }

    current_group_node = _find_group("", sq->head, sq->ns, group_node);

    _free_string_queue(sq);

    current_dataset_list = current_group_node->dataset_list;
    for(i=0; i<current_group_node->num_datasets; i++)
    {
        current_dataset_node = current_dataset_list->dataset_node;
        if(0 == strcmp(current_dataset_node->name, fullname))
        {
            new_dataset = 0;
            break;
        }
        current_dataset_list = current_dataset_list->next;
    }

    if(new_dataset)
    {
        current_dataset_node = NclCalloc(1, sizeof(NclHDF5dataset_node_t));
        if(!current_dataset_node)
        {
            fprintf(stdout, "Failed to allocated memory for current_dataset_node. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        current_dataset_node->num_attrs = 0;
        current_dataset_node->attr_list = NULL;

        strcpy(current_dataset_node->name, fullname);
        strcpy(current_dataset_node->group_name, groupname);
        strcpy(current_dataset_node->short_name, shortname);

        current_dataset_list = NclCalloc(1, sizeof(NclHDF5dataset_list_t));
        if(!current_dataset_list)
        {
            fprintf(stdout, "Failed to allocated memory for current_dataset_list. in file: %s, line: %d\n",
                        __FILE__, __LINE__);
            return NULL;
        }

        current_dataset_list->dataset_node = current_dataset_node;
        current_dataset_list->next = current_group_node->dataset_list;
        current_group_node->dataset_list = current_dataset_list;
        current_group_node->num_datasets ++;
    }

    return current_dataset_node;
}


/*
 ***********************************************************************
 * Function:	_addH5dataset
 *
 * Purpose:	add a dataset to group_node.
 *
 * Return:	SUCCEED
 *		FAILED
 *
 * Programmer:	Wei Huang
 * Created:	March 11, 2010
 *
 ***********************************************************************
 */

herr_t _addH5dataset(hsize_t rank, hsize_t *dims,
                     char *typename, char *dataname,
                     NclHDF5group_node_t *group_node)
{
    NclHDF5dataset_node_t *dataset_node;

    int i;

    dataset_node = _find_dataset(dataname, group_node);

    strcpy(dataset_node->type_name, typename);

    dataset_node->ndims = rank;
    for (i=0; i<dataset_node->ndims; i++)
    {
        dataset_node->dims[i] = dims[i];
    }

#if 0
    /* Data type */
    NclHDF5datatype = _NclHDF5get_typename(dataset_node->type, 15);

    strcpy(dataset_node->type_name, NclHDF5datatype->type_name);

    if(NclHDF5datatype->compound_nom)
    {
        dataset_node->compound = NclCalloc(1, sizeof(NclHDF5compound_t));
        if(! dataset_node->compound)
        {
            fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for dataset_node->compound, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            free(NclHDF5datatype);
            return FAILED;
        }
        dataset_node->compound->member = NclCalloc(NclHDF5datatype->compound_nom,
                                                sizeof(NclHDF5compound_component_list_t));
        if(! dataset_node->compound->member)
        {
            fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for dataset_node->compound->member, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            free(NclHDF5datatype);
            return FAILED;
        }

        dataset_node->compound->nom = NclHDF5datatype->compound_nom;
        dataset_node->compound->size = NclHDF5datatype->compound_size;

        for(i=0; i<NclHDF5datatype->compound_nom; i++)
        {
            strcpy(dataset_node->compound->member[i].name, NclHDF5datatype->compound_name[i]);
            strcpy(dataset_node->compound->member[i].type, NclHDF5datatype->compound_type[i]);
            dataset_node->compound->member[i].offset = NclHDF5datatype->compound_offset[i];
            dataset_node->compound->member[i].type_id = NclHDF5datatype->compound_type_id[i];
        }
    }

    free(NclHDF5datatype);
#endif

    return SUCCEED;
}


/*
 ***********************************************************************
 * Function:	_find_dataset_attribute
 *
 * Purpose:	find attributes of dataset.
 *
 * Return:	NclHDF5attr_node_t *
 *
 * Programmer:	Wei Huang
 * Created:	April 5, 2010
 *
 ***********************************************************************
 */

NclHDF5attr_node_t *_find_dataset_attribute(char *attrname,
                                            NclHDF5dataset_node_t *dataset_node)
{
    NclHDF5attr_list_t *curAttrList;
    NclHDF5attr_node_t *curAttrNode;

    int n;
    int new_attr = 1;

    curAttrList = dataset_node->attr_list;
    for(n = 0; n < dataset_node->num_attrs; n++)
    {
        curAttrNode = curAttrList->attr_node;
        if(0 == strcmp(attrname, curAttrNode->name))
        {
            new_attr = 0;
            break;
        }
        curAttrList = curAttrList->next;
    }

    if(new_attr)
    {
        curAttrList = NclCalloc(1, sizeof(NclHDF5attr_list_t));
        if(!curAttrList)
        {
            fprintf(stdout, "Failed to allocated memory for curAttrList. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        curAttrList->attr_node = NclCalloc(1, sizeof(NclHDF5attr_node_t));
        if(!curAttrList->attr_node)
        {
            fprintf(stdout, "Failed to allocated memory for curAttrList->attr_node. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        curAttrList->next = dataset_node->attr_list;
        dataset_node->attr_list = curAttrList;

        dataset_node->num_attrs ++;

        curAttrNode = curAttrList->attr_node;
    }

    strcpy(curAttrNode->name, attrname);

    return curAttrNode;
}


/*
 ***********************************************************************
 * Function:	_find_group_attribute
 *
 * Purpose:	find attributes of group.
 *
 * Return:	NclHDF5attr_node_t *
 *
 * Programmer:	Wei Huang
 * Created:	April 5, 2010
 *
 ***********************************************************************
 */

NclHDF5attr_node_t *_find_group_attribute(char *attrname,
                                          NclHDF5group_node_t *group_node)
{
    NclHDF5attr_list_t *curAttrList;
    NclHDF5attr_node_t *curAttrNode;

    int n;
    int new_attr = 1;

    curAttrList = group_node->attr_list;
    for(n = 0; n < group_node->num_attrs; n++)
    {
        curAttrNode = curAttrList->attr_node;
        if(0 == strcmp(attrname, curAttrNode->name))
        {
            new_attr = 0;
            break;
        }
        curAttrList = curAttrList->next;
    }

    if(new_attr)
    {
        curAttrList = NclCalloc(1, sizeof(NclHDF5attr_list_t));
        if(!curAttrList)
        {
            fprintf(stdout, "Failed to allocated memory for curAttrList. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        curAttrList->attr_node = NclCalloc(1, sizeof(NclHDF5attr_node_t));
        if(!curAttrList->attr_node)
        {
            fprintf(stdout, "Failed to allocated memory for curAttrList->attr_node. in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            return NULL;
        }

        curAttrList->next = group_node->attr_list;
        group_node->attr_list = curAttrList;

        group_node->num_attrs ++;

        curAttrNode = curAttrList->attr_node;
    }

    strcpy(curAttrNode->name, attrname);

    return curAttrNode;
}


/*
 ***********************************************************************
 * Function:	_write_dataset_attribute
 *
 * Purpose:	find attributes of group.
 *
 * Return:	SUCCEED
 *              FAILED
 *
 * Programmer:	Wei Huang
 * Created:	April 5, 2010
 *
 ***********************************************************************
 */

herr_t _write_dataset_attribute(hid_t did, NclHDF5attr_node_t *attr_node)
{
    hid_t   aid;                /* Attribute dataspace identifiers */
    hid_t   atype;              /* Attribute type */
    hid_t   attr;
    herr_t  ret;                /* Return value */
    
    if((0 == strcmp("string", attr_node->type_name)) || (0 == strcmp("char", attr_node->type_name)))
    {
      /*
       *Create string attribute.
       */
        aid   = H5Screate(H5S_SCALAR);
        atype = H5Tcopy(H5T_C_S1);
                H5Tset_size(atype, attr_node->nbytes);
                H5Tset_strpad(atype,H5T_STR_NULLTERM);
    }
    else
    {
      /*
       *Create dataspace for the first attribute.
       */
        hid_t h5type;      /* Attribute type */
        h5type = Ncl2HDF5type(attr_node->type_name);
        aid    = H5Screate(H5S_SIMPLE);
        atype  = H5Tcopy(h5type);
        ret    = H5Sset_extent_simple(aid, attr_node->ndims, attr_node->dims, NULL);
    }

  /*
   *Create attribute.
   */
    attr = H5Acreate2(did, attr_node->name, atype, aid, H5P_DEFAULT, H5P_DEFAULT);
    
  /*
   *Write attribute.
   */
    ret = H5Awrite(attr, atype, attr_node->value);

    strcpy(attr_node->name, attr_node->name);
    attr_node->id = aid;
    attr_node->type = atype;
    attr_node->space = attr;

  /*
   *Close attribute and datatype.
   */
    ret = H5Sclose(aid);
    ret = H5Tclose(atype);

  /*
   *Close the attributes.
   */
    ret = H5Aclose(attr);
    
    return ret;
}


/*
 ***********************************************************************
 * Function:	_write_chunkedH5dataset
 *
 * Purpose:	Write a dataset in chunks.
 *
 * Return:	0, if succeed, -1, if failed.
 *
 * Programmer:	Wei Huang
 * Created:	March 11, 2010
 *
 ***********************************************************************
 */

int _write_chunkedH5dataset(hid_t fid, hsize_t rank,
                            hsize_t *dims, hsize_t *chunk_dims, void *data,
                            char *typename, char *dataname,
                            NclHDF5group_node_t *group_node)
{
    NclHDF5dataset_node_t *dataset_node;

    NclHDF5attr_list_t *curAttrList;
    NclHDF5attr_node_t *curAttrNode;

    H5T_order_t h5order;

    int i;
    herr_t status;
    hid_t groupID = 0;
    hid_t did = 0;

    hid_t plist = H5P_DEFAULT;
    hsize_t *max_dims = NclCalloc(rank, sizeof(hsize_t));

    dataset_node = _find_dataset(dataname, group_node);

    strcpy(dataset_node->type_name, typename);

    groupID = _get_groupID(dataset_node, group_node);

    dataset_node->ndims = rank;
    dataset_node->nchunkdims = rank;
    for (i=0; i<dataset_node->ndims; i++)
    {
        chunk_dims[i] = dataset_node->chunk_dims[i];
        dataset_node->dims[i] = dims[i];
        max_dims[i] = H5S_UNLIMITED;
    }
  /*
   *dataset_node->space = H5Screate_simple(rank, dims, max_dims);
   */
    dataset_node->space = H5Screate_simple(rank, dims, NULL);
    dataset_node->type  = H5Tcopy(Ncl2HDF5type(typename));
    free(max_dims);

    h5order = H5Tget_order(Ncl2HDF5type(typename));
    status = H5Tset_order(dataset_node->type, h5order);

    plist  = H5Pcreate(H5P_DATASET_CREATE);
    status = H5Pset_chunk(plist, rank, chunk_dims);
    status = H5Pset_deflate(plist, 3);

    if(fid > 0)
    {
        did = H5Dcreate(fid, dataset_node->name,
                        dataset_node->type, dataset_node->space,
                        H5P_DEFAULT, plist, H5P_DEFAULT);
    }
    else
    {
        return FAILED;
    }
    dataset_node->id = did;

  /*Write the data to the dataset using default transfer properties.*/
    if(did > 0)
    {
        status = H5Dwrite(did, Ncl2HDF5type(typename), H5S_ALL, H5S_ALL, H5P_DEFAULT, data);

        curAttrList = dataset_node->attr_list;
        for(i = 0; i < dataset_node->num_attrs; i++)
        {
            curAttrNode = curAttrList->attr_node;
            _write_dataset_attribute(did, curAttrNode);
            curAttrList = curAttrList->next;
        }
    }
    else
    {
        return FAILED;
    }

#if 0
    /* Data type */
    NclHDF5datatype = _NclHDF5get_typename(dataset_node->type, 15);

    strcpy(dataset_node->type_name, NclHDF5datatype->type_name);

    if(NclHDF5datatype->compound_nom)
    {
        dataset_node->compound = NclCalloc(1, sizeof(NclHDF5compound_t));
        if(! dataset_node->compound)
        {
            fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for dataset_node->compound, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            free(NclHDF5datatype);
            return FAILED;
        }
        dataset_node->compound->member = NclCalloc(NclHDF5datatype->compound_nom,
                                                sizeof(NclHDF5compound_component_list_t));
        if(! dataset_node->compound->member)
        {
            fprintf(stdout, "UNABLE TO ALLOCATE MEMORY for dataset_node->compound->member, in file: %s, line: %d\n",
                    __FILE__, __LINE__);
            free(NclHDF5datatype);
            return FAILED;
        }

        dataset_node->compound->nom = NclHDF5datatype->compound_nom;
        dataset_node->compound->size = NclHDF5datatype->compound_size;

        for(i=0; i<NclHDF5datatype->compound_nom; i++)
        {
            strcpy(dataset_node->compound->member[i].name, NclHDF5datatype->compound_name[i]);
            strcpy(dataset_node->compound->member[i].type, NclHDF5datatype->compound_type[i]);
            dataset_node->compound->member[i].offset = NclHDF5datatype->compound_offset[i];
            dataset_node->compound->member[i].type_id = NclHDF5datatype->compound_type_id[i];
        }
    }

    free(NclHDF5datatype);
#endif

    H5Sclose(dataset_node->space);
    H5Tclose(dataset_node->type);
    H5Dclose(dataset_node->id);

    if(groupID)
        H5Gclose(groupID);

    return SUCCEED;
}


/*
 ***********************************************************************
 * Function:	_addChunk2H5dataset
 *
 * Purpose:	add chunk size to H5 dataset.
 *
 * Return:	SUCCEED
 *		FAILED
 *
 * Programmer:	Wei Huang
 * Created:	April 8, 2010
 *
 ***********************************************************************
 */

herr_t _addChunk2H5dataset(hsize_t rank, hsize_t *dims,
                           char *dataname,
                           NclHDF5group_node_t *group_node)
{
    NclHDF5dataset_node_t *dataset_node;

    int i;

    dataset_node = _find_dataset(dataname, group_node);

    if(rank != dataset_node->ndims)
    {
      /*
       *wei_start("_addChunk2H5dataset", __FILE__, __LINE__);
       *wei_check("_addChunk2H5dataset", __FILE__, __LINE__);
       *wei_check_int("_addChunk2H5dataset", "rank", rank);
       *wei_check_int("_addChunk2H5dataset", "dataset_node->ndims", dataset_node->ndims);
       *wei_end("_addChunk2H5dataset FAILED:", __FILE__, __LINE__);
       */
        return FAILED;
    }

    dataset_node->deflate_pass = 6;
    dataset_node->nchunkdims = rank;
    for (i=0; i<dataset_node->nchunkdims; i++)
    {
        if(dims[i] > dataset_node->dims[i])
            dataset_node->chunk_dims[i] = dataset_node->dims[i];
        else
            dataset_node->chunk_dims[i] = dims[i];
    }

    return SUCCEED;
}


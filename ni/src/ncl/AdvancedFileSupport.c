/*
 *      $Id: AdvancedFileSupport.c 13395 2012-04-26 15:40:49Z huangwei $
 */
/************************************************************************
*									*
*			     Copyright (C)  2012			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
************************************************************************/
/*
 *	File:		
 *
 *	Author:		Wei Huang
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Fri May 11 08:30:53 MDT 2012
 *
 *	Description:	
 */

#include <string.h>
#include "AdvancedFileSupport.h"

NclQuark *GetGrpVarNames(void *therec, int *num_vars)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclQuark *out_quarks = NULL;
    NclQuark *tmp_quarks = NULL;
    int i, n, nv;

    *num_vars = 0;
    if(NULL != grpnode->var_rec)
    {
        if(grpnode->var_rec->n_vars)
        {
            out_quarks = (NclQuark*)NclCalloc(grpnode->var_rec->n_vars,
                                           sizeof(NclQuark));
            assert(out_quarks);

            for(i = 0; i < grpnode->var_rec->n_vars; i++)
            {
                out_quarks[i] = grpnode->var_rec->var_node[i].name;
            }

            *num_vars = grpnode->var_rec->n_vars;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmp_quarks = GetGrpVarNames((void *)grpnode->grp_rec->grp_node[n], &nv);

            if(nv)
            {
                out_quarks = (NclQuark*) NclRealloc(out_quarks,
                                            (*num_vars + nv) * sizeof(NclQuark));
                assert(out_quarks);

                for(i = 0; i < nv; i++)
                {
                    out_quarks[*num_vars + i] = tmp_quarks[i];
                }
                NclFree(tmp_quarks);
            }

            *num_vars += nv;
        }
    }

    return out_quarks;
}

NclQuark *GetGrpDimNames(void *therec, int *num_dims)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclQuark *out_quarks = NULL;
    NclQuark *tmp_quarks = NULL;
    int i, n, nv;

    *num_dims = 0;
    if(NULL != grpnode->dim_rec)
    {
        if(grpnode->dim_rec->n_dims)
        {
            out_quarks = (NclQuark*)NclCalloc(grpnode->dim_rec->n_dims,
                                           sizeof(NclQuark));
            assert(out_quarks);

            for(i = 0; i < grpnode->dim_rec->n_dims; i++)
            {
                out_quarks[i] = grpnode->dim_rec->dim_node[i].name;
            }

            *num_dims = grpnode->dim_rec->n_dims;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        if(grpnode->grp_rec->n_grps)
        {
            for(n = 0; n < grpnode->grp_rec->n_grps; n++)
            {
                tmp_quarks = GetGrpDimNames((void *)grpnode->grp_rec->grp_node[n], &nv);

                if(nv)
                {
                    out_quarks = (NclQuark*)realloc(out_quarks,
                                                (*num_dims + nv) * sizeof(NclQuark));
                    assert(out_quarks);

                    for(i = 0; i < nv; i++)
                    {
                        out_quarks[*num_dims + i] = tmp_quarks[i];
                    }
                    NclFree(tmp_quarks);
                }

                *num_dims += nv;
            }
        }
    }
    return(out_quarks);
}

NclQuark *GetGrpAttNames(void* therec, int *num_atts)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclQuark *out_list = NULL;
    int i;

    *num_atts = 0;

    if(grpnode->att_rec)
    {
        if(grpnode->att_rec->n_atts)
        {
            out_list = (NclQuark*)NclCalloc(grpnode->att_rec->n_atts, sizeof(NclQuark));
            for(i = 0; i < grpnode->att_rec->n_atts; i++)
            {
                out_list[i] = grpnode->att_rec->att_node[i].name;
            }
            *num_atts = grpnode->att_rec->n_atts;
        }
    }
    return(out_list);
}

NclQuark *GetVarAttNamesFromGrp(void *therec, NclQuark thevar, int *num_atts)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileVarNode *varnode;
    NclQuark *out_list = NULL;
    int n;

    *num_atts = 0;
    varnode = _getVarNodeFromNclFileGrpNode(grpnode, thevar);
    if(NULL != varnode)
    {
        if(varnode->att_rec->n_atts)
        {
            *num_atts = varnode->att_rec->n_atts;
            out_list = (NclQuark *)NclCalloc((*num_atts), sizeof(NclQuark));
            assert(out_list);

            for(n = 0; n < varnode->att_rec->n_atts; n++)
            {
                out_list[n] = varnode->att_rec->att_node[n].name;
            }
        }
    }

    return(out_list);
}

NclFileVarNode *GetVarNodeFromGrpNode(void *therec, NclQuark var_name)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclFileVarNode *varnode;
    NclFileVarNode *tmp = NULL;
    int n, j;

    if(NULL != grpnode->var_rec)
    {
        for(n = 0; n < grpnode->var_rec->n_vars; n++)
        {
            varnode = &(grpnode->var_rec->var_node[n]);

            if((varnode->name != var_name) && (varnode->real_name != var_name))
                continue;

            tmp = (NclFileVarNode *)NclCalloc(1, sizeof(NclFileVarNode));
            assert(tmp);

            memcpy(tmp, varnode, sizeof(NclFileVarNode));

          /*
           *if(NULL != varnode->dimid)
           *{
           *    tmp->dimid = (int *) NclCalloc(varnode->dim_rec->n_dims, sizeof(int));
           *    assert(tmp->dimid);
           *    memcpy(&(tmp->dimid), &(varnode->dimid), varnode->dim_rec->n_dims * sizeof(int));
           *}
           */

            if(NULL != varnode->dim_rec)
            {
                tmp->dim_rec = _NclFileDimAlloc(varnode->dim_rec->n_dims);
                tmp->dim_rec->gid = varnode->dim_rec->gid;
                memcpy(&(tmp->dim_rec), &(varnode->dim_rec), sizeof(NclFileDimRecord));
                for(j = 0; j < varnode->dim_rec->n_dims; j++)
                {
                    memcpy(&(tmp->dim_rec->dim_node[j]), &(varnode->dim_rec->dim_node[j]),
                             sizeof(NclFileDimNode));
                }
            }

            if(NULL != varnode->chunk_dim_rec)
            {
                tmp->chunk_dim_rec = _NclFileDimAlloc(varnode->chunk_dim_rec->n_dims);
                tmp->chunk_dim_rec->gid = varnode->chunk_dim_rec->gid;
                memcpy(&(tmp->chunk_dim_rec), &(varnode->chunk_dim_rec), sizeof(NclFileDimRecord));
                for(j = 0; j < varnode->chunk_dim_rec->n_dims; j++)
                {
                    memcpy(&(tmp->chunk_dim_rec->dim_node[j]), &(varnode->chunk_dim_rec->dim_node[j]),
                             sizeof(NclFileDimNode));
                }
            }

            if(NULL != varnode->att_rec)
            {
                tmp->att_rec = _NclFileAttAlloc(varnode->att_rec->n_atts);
                memcpy(tmp->att_rec, varnode->att_rec, sizeof(NclFileAttRecord));
                for(j = 0; j < varnode->att_rec->n_atts; j++)
                {
                    memcpy(&(tmp->att_rec->att_node[j]), &(varnode->att_rec->att_node[j]),
                             sizeof(NclFileAttNode));
                    if(varnode->att_rec->att_node[j].n_elem)
                    {
                        memcpy(tmp->att_rec->att_node[j].value,
                               varnode->att_rec->att_node[j].value,
                               varnode->att_rec->att_node[j].n_elem *
                               _NclSizeOf(varnode->att_rec->att_node[j].type));
                    }
                }
            }

            return(tmp);
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmp = GetVarNodeFromGrpNode(grpnode->grp_rec->grp_node[n], var_name);
            if(NULL != tmp)
                return tmp;
        }
    }

    return(tmp);
}

NclFVarRec *GetVarInfo(void *therec, NclQuark var_name)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    return ((NclFVarRec *)GetVarNodeFromGrpNode(grpnode, var_name));
}

NclFDimRec *GetDimInfo(void* therec, NclQuark dim_name)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    NclFileDimNode *dimnode;
    NclFDimRec *tmp = NULL;
    int n;

    if(NULL != grpnode->dim_rec)
    {
        for(n = 0; n < grpnode->dim_rec->n_dims; n++)
        {
            dimnode = &(grpnode->dim_rec->dim_node[n]);
            if(dimnode->name != dim_name)
                continue;

            tmp = (NclFDimRec *)NclCalloc(1, sizeof(NclFDimRec));
            assert(tmp);

            tmp->dim_name_quark = dimnode->name;
            tmp->dim_size = dimnode->size;
            tmp->is_unlimited  = dimnode->is_unlimited;

            return tmp;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmp = GetDimInfo(grpnode->grp_rec->grp_node[n], dim_name);
            if(NULL != tmp)
                return tmp;
        }
    }

    return tmp;
}

NclFileAttNode *GetAttInfoFromGrpNode(NclFileGrpNode *grpnode, NclQuark att_name)
{
    NclFileAttNode *attnode;
    NclFileAttNode *tmp = NULL;
    int n;

    if(NULL != grpnode->att_rec)
    {
        for(n = 0; n < grpnode->att_rec->n_atts; n++)
        {
            attnode = &(grpnode->att_rec->att_node[n]);
            if(attnode->name != att_name)
                continue;

            tmp = (NclFileAttNode *)NclCalloc(1, sizeof(NclFileAttNode));
            assert(tmp);

            memcpy(tmp, attnode, sizeof(NclFileAttNode));

            if(attnode->n_elem)
                memcpy(tmp->value, attnode->value,
                       attnode->n_elem * _NclSizeOf(attnode->type));

            return(tmp);
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmp = GetAttInfoFromGrpNode(grpnode->grp_rec->grp_node[n], att_name);
            if(NULL != tmp)
                return tmp;
        }
    }

    return(tmp);
}

NclFAttRec *GetAttInfo(void* therec, NclQuark att_name_q)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *)therec;
    return ((NclFAttRec *)GetAttInfoFromGrpNode(grpnode, att_name_q));
}

NclFileAttNode *GetVarAttInfoFromGrpNode(NclFileGrpNode *grpnode,
                                         NclQuark var_name, NclQuark att_name)
{
    NclFileVarNode *varnode;
    NclFileAttNode *tmp = NULL;
    int n;

    if(NULL != grpnode->var_rec)
    {
        for(n = 0; n < grpnode->var_rec->n_vars; n++)
        {
            varnode = &(grpnode->var_rec->var_node[n]);
            if(varnode->name != var_name)
                continue;

            tmp = GetAttInfoFromVarNode(varnode, att_name);

            return tmp;
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        for(n = 0; n < grpnode->grp_rec->n_grps; n++)
        {
            tmp = GetVarAttInfoFromGrpNode(grpnode->grp_rec->grp_node[n], var_name, att_name);
            if(NULL != tmp)
                return tmp;
        }
    }

    return NULL;
}

NclFAttRec *GetVarAttInfo(void *therec, NclQuark thevar, NclQuark theatt)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode*) therec;
    return ((NclFAttRec *)GetVarAttInfoFromGrpNode(grpnode, thevar, theatt));
}

NclFileAttNode *GetAttInfoFromVarNode(NclFileVarNode *varnode, NclQuark att_name)
{
    NclFileAttNode *attnode;
    NclFileAttNode *tmp = NULL;
    int n;

    if(NULL != varnode->att_rec)
    {
        for(n = 0; n < varnode->att_rec->n_atts; n++)
        {
            attnode = &(varnode->att_rec->att_node[n]);
            if(attnode->name != att_name)
                continue;

            tmp = (NclFileAttNode *)NclCalloc(1, sizeof(NclFileAttNode));
            assert(tmp);

            memcpy(tmp, attnode, sizeof(NclFileAttNode));

            if(attnode->n_elem)
            {
                memcpy(tmp->value, attnode->value,
                    attnode->n_elem * _NclSizeOf(attnode->type));
            }

            return(tmp);
        }
    }

    return(tmp);
}

void _NclCopyGroupOptions(NclFileGrpNode *grpnode, NclFileGrpNode *rootgrpnode)
{
    int n = 0;
    size_t typesize = 0;
    if(NULL != grpnode->options)
	return;

    grpnode->n_options = rootgrpnode->n_options;

    if(grpnode->n_options)
    {
        grpnode->options = (NCLOptions *)NclCalloc(grpnode->n_options, sizeof(NCLOptions));
        assert(grpnode->options);

        for(n = 0; n < grpnode->n_options; ++n)
        {
            grpnode->options[n].name = rootgrpnode->options[n].name;
            grpnode->options[n].size = rootgrpnode->options[n].size;
            grpnode->options[n].type = rootgrpnode->options[n].type;
            grpnode->options[n].values = NULL;
            if(rootgrpnode->options[n].size)
            {
                typesize = _NclSizeOf(rootgrpnode->options[n].type);
                grpnode->options[n].values = (void*)NclCalloc(rootgrpnode->options[n].size, typesize);
                memcpy(grpnode->options[n].values, rootgrpnode->options[n].values,
                       grpnode->options[n].size * typesize);
            }
        }
    }
}

NhlErrorTypes AddNewGrp(void *rec, NclQuark grpname, size_t id)
{
    NclFileGrpNode *rootgrpnode = (NclFileGrpNode *) rec;
    NhlErrorTypes ret = NhlNOERROR;
    NclFileGrpNode   *grpnode;
    NclFileGrpRecord *grprec;
    int n = -1;
    char buffer[2 * NC_MAX_NAME + 1];

    ret = _addNclGrpNodeToGrpNode(rootgrpnode, grpname);

    grprec = rootgrpnode->grp_rec;

    for(n = 0; n < grprec->n_grps; n++)
    {
        grpnode = grprec->grp_node[n];
        if(grpname == grpnode->name)
        {
            break;
        }
        else
            grpnode = NULL;
    }

    if(NULL == grpnode)
    {
        NHLPERROR((NhlFATAL,NhlEUNKNOWN,
            "AddNewGrp: can not find group (%s)",
             NrmQuarkToString(grpname)));
        return (NhlFATAL);
    }

    grpnode->gid = id;
    grpnode->fid = id;
    grpnode->pid = rootgrpnode->gid;
    
    grpnode->pname = rootgrpnode->name;
    grpnode->path = rootgrpnode->path;
    grpnode->extension = rootgrpnode->extension;
    grpnode->file_format = rootgrpnode->file_format;
    grpnode->status = rootgrpnode->status;
    grpnode->open = rootgrpnode->open;
    grpnode->format = rootgrpnode->format;
    grpnode->define_mode = rootgrpnode->define_mode;
    grpnode->compress_level = rootgrpnode->compress_level;
    grpnode->is_chunked = rootgrpnode->is_chunked;
    grpnode->use_cache = rootgrpnode->use_cache;
    grpnode->cache_size = rootgrpnode->cache_size;
    grpnode->cache_nelems = rootgrpnode->cache_nelems;
    grpnode->cache_preemption = rootgrpnode->cache_preemption;

    if(strcmp("/", NrmQuarkToString(grpnode->pname)))
    {
	    sprintf(buffer, "/%s", NrmQuarkToString(grpname));
    }
    else
    {
            sprintf(buffer, "%s%s", NrmQuarkToString(rootgrpnode->real_name),  NrmQuarkToString(grpname));
    }
    grpnode->real_name = NrmStringToQuark(buffer);

    grpnode->chunk_dim_rec = NULL;
    grpnode->unlimit_dim_rec = NULL;
    grpnode->dim_rec = NULL;
    grpnode->att_rec = NULL;
    grpnode->var_rec = NULL;
    grpnode->coord_var_rec = NULL;
    grpnode->grp_rec = NULL;
    grpnode->udt_rec = NULL;
    grpnode->parent = rootgrpnode;

    _NclCopyGroupOptions(grpnode, rootgrpnode);

    return ret;
}

NclQuark *splitString(NclQuark inq, int *num)
{
    NclQuark *qs = NULL;
    int i, m, n = 1;
    char *iname = NrmQuarkToString(inq);
    char buffer[NCL_MAX_STRING];
    char *instr = NULL;
    char *tmpstr;

    instr = &buffer[0];

    strcpy(instr, iname);

  /*
   *fprintf(stderr, "\nEnter splitString, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tinput str: <%s>\n", instr);
   */

    if('/' == instr[0])
        instr = instr + 1;

  /*
   *fprintf(stderr, "\tinput str: <%s>\n", instr);
   */

    for(i = 0; i < strlen(instr); ++i)
    {
        if('/' == instr[i])
            ++n;
    }

    if(n)
        qs = (NclQuark *)NclMalloc(n * sizeof(NclQuark));

    m = 0;
    tmpstr = strtok(instr, "/");
    while(tmpstr != NULL)
    {
        for(i = 0; i < strlen(tmpstr); ++i)
        {
            if(!isalnum(tmpstr[i]))
                tmpstr[i] = '_';
        }

        qs[m] = NrmStringToQuark(tmpstr);
        ++m;
        tmpstr = strtok(NULL, "/");
    }

    *num = m;

  /*
   *fprintf(stderr, "\tnum = %d\n", *num);
   *fprintf(stderr, "Leave splitString, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */

    return qs;
}

int get_sizeof(int nv, int ts)
{
    int ns = nv * ts;
    int os = 4 * (ns / 4);

    while(os < ns)
        os += 4;

    return os;
}

void _Ncl_add_udt(NclFileUDTRecord **rootudtrec,
                  int gid, int uid, NclQuark name,
                  int ncl_class, int type,
                  size_t size, size_t nfields,
                  NclQuark *mem_name, NclBasicDataTypes *mem_type)
{
    NclFileUDTRecord *udtrec = *rootudtrec;
    NclFileUDTNode   *udtnode;
    int n = 0;

  /*
   *fprintf(stderr, "\nEnter _Ncl_add_udt, file: %s, line: %d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgid: %d, uid: %d, name: <%s>\n", gid, uid, NrmQuarkToString(name));
   */

    if(NULL == udtrec)
    {
        udtrec = _NclFileUDTAlloc(1);
        assert(udtrec);
        *rootudtrec = udtrec;

        udtrec->gid = gid;
        udtrec->uid = uid;
    }

    if(udtrec->n_udts >= udtrec->max_udts)
    {
        _NclFileUDTRealloc(udtrec);
    }

    udtnode = &(udtrec->udt_node[udtrec->n_udts]);

    udtnode->id = uid;
    udtnode->name = name;
    udtnode->type = type;
    udtnode->size = size;
    udtnode->ncl_class = ncl_class;
    udtnode->max_fields = nfields;
    udtnode->n_fields = nfields;

    udtnode->mem_name = (NclQuark *)NclCalloc(nfields, sizeof(NclQuark));
    assert(udtnode->mem_name);
    udtnode->mem_type = (NclBasicDataTypes *)NclCalloc(nfields, sizeof(NclBasicDataTypes));
    assert(udtnode->mem_type);

    for(n = 0; n < nfields; n++)
    {
        udtnode->mem_name[n] = mem_name[n];
        udtnode->mem_type[n] = mem_type[n];
    }

    udtrec->n_udts ++;
  /*
   *fprintf(stderr, "Leave _Ncl_add_udt, file: %s, line: %d\n\n", __FILE__, __LINE__);
   */
}

void *GetCachedValue(NclFileVarNode *varnode,
                     long start, long finish, long stride, void *storage)
{
    long i,j;
    /*int tsize = varnode->the_nc_type < 1 ? 1 : nctypelen(varnode->the_nc_type);*/
    int tsize = _NclSizeOf(varnode->type);
    for (j = 0, i = start; i <= finish; i += stride,j++)
    {
        memcpy(((char*)storage) + j * tsize,((char *)varnode->value) + i * tsize,tsize);
    }
    return storage;
}

void _NclCopyOption(NCLOptions *option, NclQuark name,
                    NclBasicDataTypes data_type, int n_items, void *values)
{
    short need_realloc = 0;
    size_t nsz = 1;

    if(name != option->name)
    {
        fprintf(stderr, "\nWARINING: In copy_option, file: %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tsource name <%s> is not same as target name <%s>\n",
			   NrmQuarkToString(name), NrmQuarkToString(option->name));
        return;
    }

    if(n_items != option->size)
    {
        need_realloc = 1;
      /*
       *fprintf(stderr, "\nWARINING: In copy_option, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tsource size: %d is not equal to target size: %d\n", n_items, option->size);
       */
        option->size = n_items;
        NclFree(option->values);
    }

    if(data_type != option->type)
    {
        need_realloc = 1;

      /*
       *fprintf(stderr, "\nWARINING: In copy_option, file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tsource type: <%s> is not equal to target type: <%s>\n",
       *	           _NclBasicDataTypeToName(data_type), _NclBasicDataTypeToName(option->type));
       */

        option->type = data_type;
    }

    nsz = n_items * _NclSizeOf(data_type);
    if(NULL == option->values)
        option->values = (void*)NclMalloc(nsz);
    else if(need_realloc)
        option->values = (void*)NclRealloc(option->values, nsz);

    memcpy(option->values, values, nsz);
}


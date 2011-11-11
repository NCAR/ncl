
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
#include "defs.h"
#include "Symbol.h"
#include <math.h>
#include "NclVar.h"
#include "NclFile.h"
#include "NclNewFile.h"
#include "NclGroup.h"
#include "NclNewGroup.h"
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
#include "NclCoordVar.h"

#if 0
void copyAttributes(NclFileAttInfoList **out, NclFileAttInfoList *in)
{
    NclFileAttInfoList *att_list;
    NclFileAttInfoList *new_list;

    *out = NULL;
    att_list = in;
    while(att_list)
    {
        new_list = (NclFileAttInfoList *) NclMalloc(sizeof(NclFileAttInfoList));
        new_list->the_att = (struct _NclFAttRec *) NclMalloc(sizeof(struct _NclFAttRec));
        new_list->the_att->att_name_quark = att_list->the_att->att_name_quark;
        new_list->the_att->data_type      = att_list->the_att->data_type;
        new_list->the_att->num_elements   = att_list->the_att->num_elements;

      /*
        fprintf(stderr, "\tnew_list->the_att->att_name_quark: <%s>\n",
            NrmQuarkToString(new_list->the_att->att_name_quark));
       */

        new_list->next = *out;
        *out = new_list;
        att_list = att_list->next;
    }
}

struct _FileCallBackRec *getFileCallBack(struct _FileCallBackRec *in_fcb)
{
    struct _FileCallBackRec *out_fcb = NULL;

    if(in_fcb != NULL)
    {
        out_fcb = (struct _FileCallBackRec *)NclMalloc(sizeof(struct _FileCallBackRec));
        out_fcb->thefileid = in_fcb->thefileid;
        out_fcb->theattid  = in_fcb->theattid;
        out_fcb->thevar    = in_fcb->thevar;
    }
    return (out_fcb);
}

struct _NclFGrpRec *getGrpRec(struct _NclFGrpRec *in_grp_info)
{
    int n = 0;
    struct _NclFGrpRec *out_grp_info;
    out_grp_info = (struct _NclFGrpRec *)NclMalloc(sizeof(struct _NclFGrpRec));

    out_grp_info->grp_name_quark = in_grp_info->grp_name_quark;
    out_grp_info->grp_real_name_quark = in_grp_info->grp_real_name_quark;
    out_grp_info->grp_full_name_quark = in_grp_info->grp_full_name_quark;
    out_grp_info->data_type = in_grp_info->data_type;
    out_grp_info->num_dimensions = in_grp_info->num_dimensions;

    for(n = 0; n < in_grp_info->num_dimensions; n++)
        out_grp_info->newfile_dim_num[n] = in_grp_info->newfile_dim_num[n];

    return (out_grp_info);
}

struct _NclFVarRec *getVarRec(struct _NclFVarRec *in_var_info)
{
    int n = 0;
    struct _NclFVarRec *out_var_info;
    out_var_info = (struct _NclFVarRec *)NclMalloc(sizeof(struct _NclFVarRec));

    out_var_info->var_name_quark = in_var_info->var_name_quark;
    out_var_info->var_real_name_quark = in_var_info->var_real_name_quark;
    out_var_info->var_full_name_quark = in_var_info->var_full_name_quark;
    out_var_info->data_type = in_var_info->data_type;
    out_var_info->num_dimensions = in_var_info->num_dimensions;
    out_var_info->num_compounds = in_var_info->num_compounds;

    for(n = 0; n < in_var_info->num_dimensions; n++)
        out_var_info->newfile_dim_num[n] = in_var_info->newfile_dim_num[n];

    for(n = 0; n < in_var_info->num_compounds; n++)
    {
        out_var_info->component_name[n] = in_var_info->component_name[n];
        out_var_info->component_type[n] = in_var_info->component_type[n];
    }
 
    return (out_var_info);
}

void setGroupAttributes(NclFile group_out)
{
    int i, j, num_atts;
    NclQuark group_name;
    NclQuark *name_list;

    if(group_out->newfile.format_funcs->get_grp_att_names != NULL)
    {
        for(i = 0; i < group_out->newfile.n_grps; i++)
        {
            group_name = group_out->newfile.grp_info[i]->grp_full_name_quark;

            name_list = (*group_out->newfile.format_funcs->get_grp_att_names)
                        (group_out->newfile.private_rec,group_name,&num_atts);

            for(j = 0; j < num_atts; j++)
            {
                AddAttInfoToList(&(group_out->newfile.grp_att_info[i]),
                                  (*group_out->newfile.format_funcs->get_grp_att_info)
                                  (group_out->newfile.private_rec,group_name,name_list[j]));
            }

            NclFree((void*)name_list);
        }
    }
}

void readFileAtt
#if    NhlNeedProto
(NclFile thefile)
#else 
(thefile)
NclFile thefile;
#endif
{
    int att_id = -1;
    int i;
    void *val;
    NclMultiDValData tmp_md;
    NhlArgVal udata;

    if(thefile->newfile.format_funcs->read_att != NULL)
    {
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        for(i = 0; i < thefile->newfile.n_file_atts; i++)
        {
            val = NclMalloc(_NclSizeOf(thefile->newfile.file_atts[i]->data_type)*
                                       thefile->newfile.file_atts[i]->num_elements );

            (void)(*thefile->newfile.format_funcs->read_att)
                  (thefile->newfile.private_rec,
                   thefile->newfile.file_atts[i]->att_name_quark,
                   val);

            tmp_md = _NclCreateMultiDVal(
                    NULL,
                    NULL,
                    Ncl_MultiDValData,
                    0,
                    val,
                    NULL,
                    1,
                    &thefile->newfile.file_atts[i]->num_elements,
                    TEMPORARY,
                    NULL,
                    _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->newfile.file_atts[i]->data_type)));
            if(tmp_md != NULL)
            {
                _NclAddAtt(att_id,NrmQuarkToString(thefile->newfile.file_atts[i]->att_name_quark),tmp_md,NULL);
            }
        }
        udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
        ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
        ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
        ((FileCallBackRec*)udata.ptrval)->thevar = -1;
        thefile->newfile.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        thefile->newfile.file_att_udata = (FileCallBackRec*)udata.ptrval;
        thefile->newfile.file_atts_id = att_id;
    }
}

void setVarAtts
#if     NhlNeedProto
(NclFile thefile)
#else
(thefile)
NclFile thefile;
#endif
{
    int index;
    NclFileAttInfoList *step;
    int att_id = -1;
    void *val;
    NclMultiDValData tmp_md;
    NhlArgVal udata;
    NclQuark  var;
    
    for(index = 0; index < thefile->newfile.n_vars; index++)
    {
        var = thefile->newfile.var_info[index]->var_full_name_quark;
        step = thefile->newfile.var_att_info[index];
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        while(step != NULL)
        {
            if (step->the_att->data_type == NCL_none)
                val = NULL;
            else
            {
                val = NclMalloc(_NclSizeOf(step->the_att->data_type)* step->the_att->num_elements );
                (void)(*thefile->newfile.format_funcs->read_var_att)
                      (thefile->newfile.private_rec, var,
                       step->the_att->att_name_quark, val);
            }
            tmp_md = _NclCreateMultiDVal(
                    NULL,
                    NULL,
                    Ncl_MultiDValData,
                    0,
                    val,
                    NULL,
                    1,
                    &step->the_att->num_elements,
                    TEMPORARY,
                    NULL,
                    _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(step->the_att->data_type))
                    );
            if(tmp_md != NULL)
            {
                _NclAddAtt(att_id,NrmQuarkToString(step->the_att->att_name_quark),tmp_md,NULL);
            }
            step = step->next;
        }
        udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
        ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
        ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
        ((FileCallBackRec*)udata.ptrval)->thevar = var;
        thefile->newfile.var_att_cb[index] = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
					      FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        thefile->newfile.var_att_udata[index] = (FileCallBackRec*)udata.ptrval;
        thefile->newfile.var_att_ids[index] = att_id;
    }
}
#endif

static void UpdateNewGroupDims(NclNewFile group_out, NclFileGrpNode *grpnode)
{
  /*
   */
    fprintf(stderr, "\nEnter UpdateNewGroupDims, file: %s, line:%d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tgrpnode->name: <%s>\n", NrmQuarkToString(grpnode->name));

  /*
    if(NULL != grpnode->dim_rec)
    {
    }

   */
    fprintf(stderr, "Leave UpdateNewGroupDims, file: %s, line:%d\n\n", __FILE__, __LINE__);
}

NclGroup *_NclNewGroupCreate(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                             unsigned int obj_type_mask, NclStatus status,
                             NclFile file_in, NclQuark group_name)
{
    NclNewFile thefile = (NclNewFile) file_in;
    NclNewFile group_out = NULL;
    int group_out_free = 0;
    NhlErrorTypes ret= NhlNOERROR;
    NclObjClass class_ptr;
    NclFileGrpNode *grpnode = NULL;

  /*
   *fprintf(stderr, "\nEnter _NclNewGroupCreate, file: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
   *fprintf(stderr, "\tthefile->newfile.grpnode->name: <%s>\n",
   *                   NrmQuarkToString(thefile->newfile.grpnode->name));
   */

    if(NULL == thefile)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
            "_NclNewGroupCreate: Unable to create group from NULL file.\n"));

        return NULL;
    }

    if(group_name == thefile->newfile.grpnode->name)
    {
        return ((NclGroup *)thefile);
    }

    grpnode = _getGrpNodeFromNclFileGrpNode(thefile->newfile.grpnode, group_name);

    if(NULL == grpnode)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
            "_NclNewGroupCreate: Unable to find group <%s> from file <%s>.\n",
             NrmQuarkToString(group_name),
             NrmQuarkToString(thefile->newfile.fname)));

        return NULL;
    }

    ret = _NclInitClass(nclNewFileClass);
    if(ret < NhlWARNING) 
        return(NULL);

    if(theclass == NULL)
    {
        class_ptr = nclNewFileClass;
    }
    else
    {
        class_ptr = theclass;
    }

    if(inst == NULL)
    {
        group_out = (NclNewFile)NclCalloc(1, sizeof(NclNewFileRec));
        assert(group_out);
        group_out_free = 1;
    }
    else
    {
        group_out = (NclNewFile)inst;
    }

    group_out->newfile.fname = thefile->newfile.fname;
    group_out->newfile.fpath = thefile->newfile.fpath;
    group_out->newfile.file_ext_q = thefile->newfile.file_ext_q;
    group_out->newfile.wr_status = thefile->newfile.wr_status;
    group_out->newfile.file_format = thefile->newfile.file_format;

    group_out->newfile.format_funcs = _NclGetFormatFuncsWithNewHLFS(thefile->newfile.file_ext_q);
    group_out->use_new_hlfs = 1;

#if 0
    group_out->newfile.grpnode = (NclFileGrpNode *)
                                 (*group_out->newfile.format_funcs->initialize_file_rec)
                                 (&group_out->newfile.file_format);
    if(NULL == group_out->newfile.grpnode)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        if(group_out_free)
            NclFree((void*)group_out);
        return(NULL);
    }

    memcpy(group_out->newfile.grpnode, grpnode, sizeof(NclFileGrpNode));
#endif

    group_out->newfile.grpnode = grpnode;

    group_out->newfile.grpnode->fid = thefile->newfile.grpnode->fid;
    group_out->newfile.grpnode->path = thefile->newfile.fpath;
    group_out->newfile.grpnode->extension = thefile->newfile.file_ext_q;

  /*
   *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpnode->path: <%s>\n", NrmQuarkToString(group_out->newfile.grpnode->path));
   *fprintf(stderr, "\tgrpnode->extension: <%s>\n", NrmQuarkToString(group_out->newfile.grpnode->extension));
   */

    if(NULL == group_out->newfile.grpnode->options)
    {
        group_out->newfile.grpnode->n_options = thefile->newfile.grpnode->n_options;

        if(thefile->newfile.grpnode->n_options)
        {
            group_out->newfile.grpnode->options = (NCLOptions *)NclCalloc(grpnode->n_options, sizeof(NCLOptions));
            assert(group_out->newfile.grpnode->options);

            memcpy(group_out->newfile.grpnode->options, thefile->newfile.grpnode->options,
                   group_out->newfile.grpnode->n_options * sizeof(NCLOptions));
        }
    }

#if 0
    UpdateNewGroupDims(group_out, grpnode);
#endif

    (void)_NclObjCreate((NclObj)group_out,class_ptr,obj_type,
                        (obj_type_mask | Ncl_File),status);

    if(class_ptr == nclNewFileClass)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       */
        _NclCallCallBacks((NclObj)group_out,CREATED);
    }

  /*
   *fprintf(stderr, "Leave _NclNewGroupCreate, file: %s, line:%d\n\n", __FILE__, __LINE__);
   */

    return ((NclGroup *)group_out);
}


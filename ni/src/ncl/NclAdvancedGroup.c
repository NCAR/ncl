
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
#include "NclAdvancedFile.h"
#include "AdvancedFileSupport.h"
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

#define NUM_OPTIONS  (1 + Ncl_RECORD_MARKER_SIZE)

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
        out_grp_info->advancedfile_dim_num[n] = in_grp_info->advancedfile_dim_num[n];

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
        out_var_info->advancedfile_dim_num[n] = in_var_info->advancedfile_dim_num[n];

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

    if(group_out->advancedfile.format_funcs->get_grp_att_names != NULL)
    {
        for(i = 0; i < group_out->advancedfile.n_grps; i++)
        {
            group_name = group_out->advancedfile.grp_info[i]->grp_full_name_quark;

            name_list = (*group_out->advancedfile.format_funcs->get_grp_att_names)
                        (group_out->advancedfile.private_rec,group_name,&num_atts);

            for(j = 0; j < num_atts; j++)
            {
                AddAttInfoToList(&(group_out->advancedfile.grp_att_info[i]),
                                  (*group_out->advancedfile.format_funcs->get_grp_att_info)
                                  (group_out->advancedfile.private_rec,group_name,name_list[j]));
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

    if(thefile->advancedfile.format_funcs->read_att != NULL)
    {
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        for(i = 0; i < thefile->advancedfile.n_file_atts; i++)
        {
            val = NclMalloc(_NclSizeOf(thefile->advancedfile.file_atts[i]->data_type)*
                                       thefile->advancedfile.file_atts[i]->num_elements );

            (void)(*thefile->advancedfile.format_funcs->read_att)
                  (thefile->advancedfile.private_rec,
                   thefile->advancedfile.file_atts[i]->att_name_quark,
                   val);

            tmp_md = _NclCreateMultiDVal(
                    NULL,
                    NULL,
                    Ncl_MultiDValData,
                    0,
                    val,
                    NULL,
                    1,
                    &thefile->advancedfile.file_atts[i]->num_elements,
                    TEMPORARY,
                    NULL,
                    _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->advancedfile.file_atts[i]->data_type)));
            if(tmp_md != NULL)
            {
                _NclAddAtt(att_id,NrmQuarkToString(thefile->advancedfile.file_atts[i]->att_name_quark),tmp_md,NULL);
            }
        }
        udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
        ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
        ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
        ((FileCallBackRec*)udata.ptrval)->thevar = -1;
        thefile->advancedfile.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        thefile->advancedfile.file_att_udata = (FileCallBackRec*)udata.ptrval;
        thefile->advancedfile.file_atts_id = att_id;
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
    
    for(index = 0; index < thefile->advancedfile.n_vars; index++)
    {
        var = thefile->advancedfile.var_info[index]->var_full_name_quark;
        step = thefile->advancedfile.var_att_info[index];
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        while(step != NULL)
        {
            if (step->the_att->data_type == NCL_none)
                val = NULL;
            else
            {
                val = NclMalloc(_NclSizeOf(step->the_att->data_type)* step->the_att->num_elements );
                (void)(*thefile->advancedfile.format_funcs->read_var_att)
                      (thefile->advancedfile.private_rec, var,
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
        thefile->advancedfile.var_att_cb[index] = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
					      FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        thefile->advancedfile.var_att_udata[index] = (FileCallBackRec*)udata.ptrval;
        thefile->advancedfile.var_att_ids[index] = att_id;
    }
}

static void UpdateAdvancedGroupDims(NclAdvancedFile group_out, NclFileGrpNode *grpnode)
{
  /*
   */
    fprintf(stderr, "\nEnter UpdateAdvancedGroupDims, file: %s, line:%d\n", __FILE__, __LINE__);
    fprintf(stderr, "\tgrpnode->name: <%s>\n", NrmQuarkToString(grpnode->name));

  /*
    if(NULL != grpnode->dim_rec)
    {
    }

   */
    fprintf(stderr, "Leave UpdateAdvancedGroupDims, file: %s, line:%d\n\n", __FILE__, __LINE__);
}
#endif

NclAdvancedFile _NclAdvancedGroupCreate(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                                        unsigned int obj_type_mask, NclStatus status,
                                        NclFile file_in, NclQuark group_name)
{
    NclAdvancedFile thefile = (NclAdvancedFile) file_in;
    NclAdvancedFile group_out = NULL;
    NclObjClass class_ptr;
    NclFileGrpNode *grpnode = NULL, *outgrpnode;

  /*
   *fprintf(stderr, "\nEnter _NclAdvancedGroupCreate, file: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
   *fprintf(stderr, "\tthefile->advancedfile.grpnode->name: <%s>\n",
   *                   NrmQuarkToString(thefile->advancedfile.grpnode->name));
   */

    if(NULL == thefile)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
            "_NclAdvancedGroupCreate: Unable to create group from NULL file.\n"));

        return NULL;
    }

    if(group_name == thefile->advancedfile.grpnode->name)
    {
        return ((NclAdvancedFile)thefile);
    }

    grpnode = _getGrpNodeFromNclFileGrpNode(thefile->advancedfile.grpnode, group_name);
    if(NULL == grpnode)
    {
      /*
       *NHLPERROR((NhlWARNING,NhlEUNKNOWN,
       *    "_NclAdvancedGroupCreate: Unable to find group <%s> from file <%s>.\n",
       *     NrmQuarkToString(group_name),
       *     NrmQuarkToString(thefile->advancedfile.fname)));
       */

        return NULL;
    }
    outgrpnode = (NclFileGrpNode *)NclCalloc(1,sizeof(NclFileGrpNode));
    memcpy(outgrpnode,grpnode,sizeof(NclFileGrpNode));
    

    if(theclass == NULL)
    {
        class_ptr = nclAdvancedFileClass;
    }
    else
    {
        class_ptr = theclass;
    }

    if(inst == NULL)
    {
        group_out = (NclAdvancedFile)NclCalloc(1, sizeof(NclAdvancedFileRec));
        assert(group_out);
    }
    else
    {
        group_out = (NclAdvancedFile)inst;
    }

    group_out->advancedfile.fname = thefile->advancedfile.fname;
    group_out->advancedfile.fpath = thefile->advancedfile.fpath;
    group_out->advancedfile.file_ext_q = thefile->advancedfile.file_ext_q;
    group_out->advancedfile.wr_status = thefile->advancedfile.wr_status;
    group_out->advancedfile.file_format = thefile->advancedfile.file_format;

    group_out->advancedfile.format_funcs = _NclGetFormatFuncsWithAdvancedFileStructure(thefile->advancedfile.file_ext_q);
    group_out->file.advanced_file_structure = 1;

    group_out->advancedfile.grpnode = outgrpnode;

    group_out->advancedfile.grpnode->fid = thefile->advancedfile.grpnode->fid;
    group_out->advancedfile.grpnode->open = thefile->advancedfile.grpnode->open;
    group_out->advancedfile.grpnode->path = thefile->advancedfile.fpath;
    group_out->advancedfile.grpnode->extension = thefile->advancedfile.file_ext_q;

  /*
   *fprintf(stderr, "\tfile: %s, line:%d\n", __FILE__, __LINE__);
   *fprintf(stderr, "\tgrpnode->path: <%s>\n", NrmQuarkToString(group_out->advancedfile.grpnode->path));
   *fprintf(stderr, "\tgrpnode->extension: <%s>\n", NrmQuarkToString(group_out->advancedfile.grpnode->extension));
   */

    _NclCopyGroupOptions(group_out->advancedfile.grpnode, thefile->advancedfile.grpnode);

#if 0
    UpdateAdvancedGroupDims(group_out, grpnode);
#endif

    (void)_NclObjCreate((NclObj)group_out,class_ptr,obj_type,
                        (obj_type_mask | Ncl_File),status);

    if(class_ptr == nclAdvancedFileClass)
    {
      /*
       *fprintf(stderr, "\tfile: %s, line: %d\n", __FILE__, __LINE__);
       */
        _NclCallCallBacks((NclObj)group_out,CREATED);
    }

  /*
   *fprintf(stderr, "Leave _NclAdvancedGroupCreate, file: %s, line:%d\n\n", __FILE__, __LINE__);
   */

    return group_out;
}


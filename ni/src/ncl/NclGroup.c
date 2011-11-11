
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
#include "NclGroup.h"
#include "NclFile.h"
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
#include "NclCallBacksI.h"

extern int grib_version;

#define NCLGROUP_INC -1
#define NCLGROUP_DEC -2
#define NCLGROUP_VEC 0

#define GROUP_COORD_VAR_ACCESS 0
#define GROUP_VAR_ACCESS 1

NclQuark GroupGetDimName(
#if	NhlNeedProto
NclGroup /* thegroup */,
int /*num*/
#endif
);

#if 0 
/* this is not yet defined */
static int GroupIsVar(
#if	NhlNeedProto
NclGroup /*thegroup */,
NclQuark /* name */
#endif
);
#endif

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
        fprintf(stdout, "\tnew_list->the_att->att_name_quark: <%s>\n",
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
        out_grp_info->file_dim_num[n] = in_grp_info->file_dim_num[n];

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
        out_var_info->file_dim_num[n] = in_var_info->file_dim_num[n];

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

    if(group_out->file.format_funcs->get_grp_att_names != NULL)
    {
        for(i = 0; i < group_out->file.n_grps; i++)
        {
            group_name = group_out->file.grp_info[i]->grp_full_name_quark;

            name_list = (*group_out->file.format_funcs->get_grp_att_names)
                        (group_out->file.private_rec,group_name,&num_atts);

            for(j = 0; j < num_atts; j++)
            {
                AddAttInfoToList(&(group_out->file.grp_att_info[i]),
                                  (*group_out->file.format_funcs->get_grp_att_info)
                                  (group_out->file.private_rec,group_name,name_list[j]));
            }

            NclFree((void*)name_list);
        }
    }
}

void initializeGroup(NclFile group_out)
{
    _NclInitFilePart(&(group_out->file));
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

    if(thefile->file.format_funcs->read_att != NULL)
    {
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        for(i = 0; i < thefile->file.n_file_atts; i++)
        {
            val = NclMalloc(_NclSizeOf(thefile->file.file_atts[i]->data_type)*
                                       thefile->file.file_atts[i]->num_elements );

            (void)(*thefile->file.format_funcs->read_att)
                  (thefile->file.private_rec,
                   thefile->file.file_atts[i]->att_name_quark,
                   val);

            tmp_md = _NclCreateMultiDVal(
                    NULL,
                    NULL,
                    Ncl_MultiDValData,
                    0,
                    val,
                    NULL,
                    1,
                    &thefile->file.file_atts[i]->num_elements,
                    TEMPORARY,
                    NULL,
                    _NclTypeEnumToTypeClass(_NclBasicDataTypeToObjType(thefile->file.file_atts[i]->data_type)));
            if(tmp_md != NULL)
            {
                _NclAddAtt(att_id,NrmQuarkToString(thefile->file.file_atts[i]->att_name_quark),tmp_md,NULL);
            }
        }
        udata.ptrval = (void*)NclMalloc(sizeof(FileCallBackRec));
        ((FileCallBackRec*)udata.ptrval)->thefileid = thefile->obj.id;
        ((FileCallBackRec*)udata.ptrval)->theattid = att_id;
        ((FileCallBackRec*)udata.ptrval)->thevar = -1;
        thefile->file.file_att_cb = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        thefile->file.file_att_udata = (FileCallBackRec*)udata.ptrval;
        thefile->file.file_atts_id = att_id;
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
    
    for(index = 0; index < thefile->file.n_vars; index++)
    {
        var = thefile->file.var_info[index]->var_full_name_quark;
        step = thefile->file.var_att_info[index];
        att_id = _NclAttCreate(NULL,NULL,Ncl_Att,0,(NclObj)thefile);
        while(step != NULL)
        {
            if (step->the_att->data_type == NCL_none)
                val = NULL;
            else
            {
                val = NclMalloc(_NclSizeOf(step->the_att->data_type)* step->the_att->num_elements );
                (void)(*thefile->file.format_funcs->read_var_att)
                      (thefile->file.private_rec, var,
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
        thefile->file.var_att_cb[index] = _NclAddCallback((NclObj)_NclGetObj(att_id),NULL,
					      FileAttIsBeingDestroyedNotify,ATTDESTROYED,&udata);
        thefile->file.var_att_udata[index] = (FileCallBackRec*)udata.ptrval;
        thefile->file.var_att_ids[index] = att_id;
    }
}

NclGroup *_NclGroupCreate
#if    NhlNeedProto
(NclObj  inst, NclObjClass theclass, NclObjTypes obj_type, unsigned int obj_type_mask, NclStatus status, NclFile file_in, NclQuark group_name)
#else
(inst, theclass, obj_type, obj_type_mask, status, file_in, group_name)
NclObj  inst;
NclObjClass theclass;
NclObjTypes obj_type;
unsigned int obj_type_mask;
NclStatus status; 
NclFile file_in;
NclQuark group_name;
#endif
{
    NclGroup *group_out = NULL;
    int group_out_free = 0;
    NhlErrorTypes ret= NhlNOERROR;
    NclObjClass class_ptr;

    char grp_str[NCL_MAX_STRING];
    char tmp_str[NCL_MAX_STRING];
    char buffer[NCL_MAX_STRING];
    NclQuark *selected_group;
    int nsg = 0;
    int new_group = 0;
    int i, j, n;
    int gl = 0;
    int n_grps = 0;
    int n_vars = 0;


    ret = _NclInitClass(nclFileClass);
    if(ret < NhlWARNING) 
        return(NULL);
    if(theclass == NULL)
    {
        class_ptr = nclFileClass;
    }
    else
    {
        class_ptr = theclass;
    }

    if(inst == NULL)
    {
        group_out = (NclFile)NclMalloc(sizeof(NclFileRec));
        group_out_free = 1;
    }
    else
    {
        group_out = (NclFile)inst;
    }

    strcpy(grp_str, NrmQuarkToString(group_name));
    gl = strlen(grp_str);

  /*
    fprintf(stdout, "\n\nhit _NclGroupCreate, file: %s, line:%d\n", __FILE__, __LINE__);
    fprintf(stdout, "\tgroup_name: <%s>\n", NrmQuarkToString(group_name));
   */

    initializeGroup(group_out);

    group_out->file.fname = file_in->file.fname;
    group_out->file.fpath = file_in->file.fpath;
    group_out->file.file_ext_q = file_in->file.file_ext_q;
    group_out->file.wr_status = file_in->file.wr_status;
    group_out->file.file_format = file_in->file.file_format;

    group_out->file.format_funcs = _NclGetFormatFuncs(group_out->file.file_ext_q);
    group_out->file.private_rec = (*group_out->file.format_funcs->initialize_file_rec)(&group_out->file.file_format);

    if(group_out->file.private_rec == NULL)
    {
        NhlPError(NhlFATAL,ENOMEM,NULL);
        if(group_out_free) 
            NclFree((void*)group_out);
        return(NULL);
    }

    group_out->file.private_rec = (*group_out->file.format_funcs->open_file)
                                  (group_out->file.private_rec,
                                   group_out->file.fpath, group_out->file.wr_status);

    group_out->file.n_file_dims = file_in->file.n_file_dims;
  /*
    for(i = 0; i < group_out->file.n_file_dims; i++)
    {
        group_out->file.file_dim_info[i] = file_in->file.file_dim_info[i];
        group_out->file.coord_vars[i] = file_in->file.coord_vars[i];
    }
   */

    UpdateDims(group_out);

    group_out->file.n_file_atts = file_in->file.n_file_atts;

    for(j = 0; j < file_in->file.n_file_atts; j++)
    {
        group_out->file.file_atts[j] = (struct _NclFAttRec *)NclMalloc(sizeof(struct _NclFAttRec));
        group_out->file.file_atts[j]->att_name_quark = file_in->file.file_atts[j]->att_name_quark;
        group_out->file.file_atts[j]->data_type      = file_in->file.file_atts[j]->data_type;
        group_out->file.file_atts[j]->num_elements   = file_in->file.file_atts[j]->num_elements;
    }

    readFileAtt(group_out);

    selected_group = (NclQuark *)NclMalloc((1 + file_in->file.n_grps) * sizeof(NclQuark));

    nsg = 0;
    for(i = 0; i < file_in->file.n_grps; i++)
    {
        strcpy(tmp_str, NrmQuarkToString(file_in->file.grp_info[i]->grp_full_name_quark));
        if(gl > strlen(tmp_str))
            continue;

      /*
        if(0 == strcmp(tmp_str, grp_str))
        {
            att_list = file_in->file.grp_att_info[i];
            j = group_out->file.n_file_atts;
            while(att_list)
            {
                group_out->file.file_atts[j] = (struct _NclFAttRec *)NclMalloc(sizeof(struct _NclFAttRec));
                group_out->file.file_atts[j]->att_name_quark = att_list->the_att->att_name_quark;
                group_out->file.file_atts[j]->data_type      = att_list->the_att->data_type;
                group_out->file.file_atts[j]->num_elements   = att_list->the_att->num_elements;
                j++;
                att_list = att_list->next;
            }
            group_out->file.n_file_atts = j;

            continue;
        }
       */

        strcpy(buffer, tmp_str + gl);
        tmp_str[gl] = '\0';
        if(0 != strcmp(tmp_str, grp_str))
            continue;

        if('/' != buffer[0])
            continue;

        new_group = 1;
        for(n = 0; n < nsg; n++)
        {
            if(selected_group[n] == file_in->file.grp_info[i]->grp_full_name_quark)
            {
                new_group = 0;
                break;
            }
        }
        if(new_group)
        {
            selected_group[nsg] = file_in->file.grp_info[i]->grp_full_name_quark;
            nsg++;
        }
      /*
        copyAttributes(&(group_out->file.grp_att_info[n_grps]),
                       file_in->file.grp_att_info[i]);
        group_out->file.grp_att_ids[n_grps] = -1;
        LoadVarAtts(file_in, file_in->file.grp_info[i]->grp_name_quark);
      */
        group_out->file.grp_info[n_grps] = getGrpRec(file_in->file.grp_info[i]);
        n_grps++;
    }

    group_out->file.n_grps = n_grps;
    setGroupAttributes(group_out);

    for(i = 0; i < file_in->file.n_vars; i++)
    {
        strcpy(tmp_str, NrmQuarkToString(file_in->file.var_info[i]->var_full_name_quark));
        if(gl > strlen(tmp_str))
            continue;

        strcpy(buffer, tmp_str + gl);
        tmp_str[gl] = '\0';
        if(0 != strcmp(tmp_str, grp_str))
            continue;

        if('/' != buffer[0])
            continue;

        group_out->file.var_info[n_vars] = getVarRec(file_in->file.var_info[i]);
        copyAttributes(&(group_out->file.var_att_info[n_vars]),
                       file_in->file.var_att_info[i]);
        n_vars++;
    }

    group_out->file.n_vars = n_vars;

    setVarAtts(group_out);

    (void)_NclObjCreate((NclObj)group_out,class_ptr,obj_type,
                        (obj_type_mask | Ncl_File),status);

    if(class_ptr == nclFileClass)
    {
        _NclCallCallBacks((NclObj)group_out,CREATED);
    }

    NclFree(selected_group);

  /*
    fprintf(stdout, "\tgroup_out->file.n_vars = %d\n", group_out->file.n_vars);
    fprintf(stdout, "\n\nend _NclGroupCreate, file: %s, line:%d\n", __FILE__, __LINE__);
   */
    return(group_out);
}


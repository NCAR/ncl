#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>
#include <stdint.h>
#include <ncarg/c.h>
#include <ncarg/hlu/hluP.h>
#include <ncarg/hlu/NresDB.h>
#include <ncarg/hlu/PlotManager.h>
#include <ncarg/hlu/Workstation.h>
#include <ncarg/hlu/Workspace.h>
#include <ncarg/hlu/Callbacks.h>
#include <ncarg/hlu/AppI.h>
#include <ncarg/ncargC.h>
#include <ncarg/c.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <fcntl.h>
#include <unistd.h>
#include <math.h>
#include <float.h>
#include "defs.h"
#include <errno.h>
#include "Symbol.h"
#include "NclDataDefs.h"
#include "Machine.h"
#include "NclFile.h"
#include "NclAdvancedFile.h"
#include "NclVar.h"
#include "NclCoordVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
#include "HLUSupport.h"
#include "HluClasses.h"
#include "parser.h"
#include "OpsList.h"
#include "ApiRecords.h"
#include "TypeSupport.h"
#include "NclBuiltInSupport.h"
#include "FileSupport.h"
#include "NclAtt.h"
#include "NclList.h"
#include "ListSupport.h"
#include "NclFileInterfaces.h"
#include <signal.h>
#include <netcdf.h>
#include "NclProf.h"

#include "HLUFunctions.h"
#include "NclGlobalVars.h"

NclAdvancedFile _NclCreateAdvancedFile(NclObj inst, NclObjClass theclass, NclObjTypes obj_type,
                                       unsigned int obj_type_mask, NclStatus status,
                                       NclQuark path, int rw_status);

NclFile NclCreateFile(const char *path)
{
    NclQuark qpath = NrmStringToQuark(path);
    return (_NclOpenFile(NULL,NULL,Ncl_File,0,TEMPORARY,qpath,1));
}

#if 1
NclFile NclCreateAdvancedFile(const char *path)
{
    NclAdvancedFile nclfile;
    NclQuark qpath = NrmStringToQuark(path);
    nclfile = _NclCreateAdvancedFile(NULL,NULL,Ncl_File,0,TEMPORARY,qpath,-1);
    return (NclFile) nclfile;
}
#else
NclAdvancedFile NclCreateAdvancedFile(const char *path)
{
    NclQuark qpath = NrmStringToQuark(path);
    return (_NclCreateAdvancedFile(NULL,NULL,Ncl_File,0,TEMPORARY,qpath,-1));
}
#endif

void guiSetAdvancedFileStructure(const char *format)
{
    if(0 == strcmp("shp", format))
        NCLadvancedFileStructure[_Nio_Opt_OGR] = 1;
    else if(0 == strcmp("nc", format))
        NCLadvancedFileStructure[_Nio_Opt_NETCDF] = 1;
    else if(0 == strcmp("he5", format))
        NCLadvancedFileStructure[_Nio_Opt_HDFEOS5] = 1;
    else if(0 == strcmp("all", format)) {
	    int i;
	    for (i = 0; i < _NioNumberOfFileStructOptions; i++) {
		    NCLadvancedFileStructure[i] = 1;
	    }
    }
}

char **GetNclFileVarNames(NclFile thefile, int *num_vars)
{
    int n = 0;
    NclQuark *qvars = NULL;
    char *str = NULL;
    char **varnames = NULL;

    if(NULL == thefile)
        return NULL;

    qvars = _NclFileReadVarNames(thefile, num_vars);

    if(1 > *num_vars)
        return NULL;

    if(NULL == varnames)
        varnames = (char **)NclCalloc(*num_vars, sizeof(char *));
    else
    {
        fprintf(stderr, "\n\nWARNING!\n\n");
        fprintf(stderr, "file %s, line: %d\n", __FILE__, __LINE__);
        fprintf(stderr, "\tA null char ** variable is expected but input is not.\n");
        fprintf(stderr, "\tMemory leak risk is high.\n");
        fprintf(stderr, "\n\nWARNING!!\n\n");

        varnames = (char **)NclRealloc(varnames, (*num_vars) * sizeof(char *));
    }

    for(n = 0; n < *num_vars; ++n)
    {
        str = NrmQuarkToString(qvars[n]);
        varnames[n] = (char *)NclCalloc(strlen(str) + 1, sizeof(char));
        strcpy(varnames[n], str);

      /*
       *fprintf(stderr, "file %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tVar %d: <%s>\n", n, varnames[n]);
       */
    }

    if(NULL != qvars)
        NclFree(qvars);

    return varnames;
}

char **guiGetNclFileAttNames(NclFile thefile, int *num_atts)
{
    int n = 0;
    char *str = NULL;
    char **attnames = NULL;
    NclAdvancedFile newfile = NULL;
    NclFileGrpNode *grpnode = NULL;

    if(NULL == thefile)
        return NULL;

    if(thefile->file.advanced_file_structure)
    {
        newfile = (NclAdvancedFile) thefile;
        grpnode = newfile->advancedfile.grpnode;
        if(NULL == grpnode->att_rec)
            *num_atts = 0;
        else
            *num_atts = grpnode->att_rec->n_atts;
    }
    else
        *num_atts = thefile->file.n_file_atts;

    if(1 > *num_atts)
        return NULL;

    attnames = (char **)NclCalloc(*num_atts, sizeof(char *));

    if(thefile->file.advanced_file_structure)
    {
        for(n = 0; n < *num_atts; ++n)
        {
            str = NrmQuarkToString(grpnode->att_rec->att_node[n].name);
            attnames[n] = (char *)NclCalloc(strlen(str) + 1, sizeof(char));
            strcpy(attnames[n], str);
          /*
           *fprintf(stderr, "file %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tatt %d: <%s>\n", n, str);
           */
        }
    }
    else
    {
        for(n = 0; n < thefile->file.n_file_atts; ++n)
        {
            str = NrmQuarkToString(thefile->file.file_atts[n]->att_name_quark);
            attnames[n] = (char *)NclCalloc(strlen(str) + 1, sizeof(char));
            strcpy(attnames[n], str);
          /*
           *fprintf(stderr, "\tfile %s, line: %d\n", __FILE__, __LINE__);
           *fprintf(stderr, "\tatt %d: <%s>\n", n, str);
           */
        }
    }

    return attnames;
}

NclMultiDValData guiGetVarAtt(NclVar the_var, const char *attname)
{
    return _NclReadAtt(the_var, attname, NULL);
}

NclMultiDValData guiGetVarAttMV(NclFile thefile, const char* varname, const char *attname)
{
    NclQuark varquark = NrmStringToQuark(varname);
    NclQuark attquark = NrmStringToQuark(attname);

    return (_NclFileReadVarAtt(thefile,varquark,attquark,NULL));
}

void getNclFileVarInfo(NclFile thefile, int *ndims, int **dimsizes, char ***dimnames, long *type)
{
    int i,j;

    if(NULL == thefile)
        return;

    if(thefile->file.advanced_file_structure)
    {
        NclAdvancedFile thenewfile = (NclAdvancedFile) thefile;
        NclFileVarNode *varnode = NULL;

        if(NULL != thenewfile->advancedfile.grpnode->var_rec)
        {
            for(i = 0; i < thenewfile->advancedfile.grpnode->var_rec->n_vars; ++i)
            {
                varnode = &(thenewfile->advancedfile.grpnode->var_rec->var_node[i]);

                ndims[i] = 0;
                type[i] = (long) varnode->type;

                if(NULL != varnode->dim_rec)
                {
                    ndims[i] = varnode->dim_rec->n_dims;

                    for(j = 0 ; j < ndims[i]; ++j)
                    {
                        strcpy(dimnames[i][j], NrmQuarkToString(varnode->dim_rec->dim_node[j].name));
                        dimsizes[i][j] = varnode->dim_rec->dim_node[j].size;
                    }
                }
#if 0
                if(NULL != varnode->att_rec)
                {
                    tmp->u.var->n_atts = varnode->att_rec->n_atts;
                    tmp->u.var->attnames = (NclQuark*)NclMalloc(tmp->u.var->n_atts * sizeof(NclQuark));
                    for(j = 0; j < tmp->u.var->n_atts; ++j)
                    {
                        tmp->u.var->attnames[j] = varnode->att_rec->att_node[j].name;
                    }
                }
#endif
            }
        }
    }
    else
    {
      /*
       *fprintf(stderr, "file %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stderr, "\tnumber of vars = %d\n", thefile->file.n_vars);
       */

        for(i = 0; i < thefile->file.n_vars; ++i)
        {
            type[i] = (long) thefile->file.var_info[i]->data_type;
            ndims[i] = thefile->file.var_info[i]->num_dimensions;

            for(j = 0; j < ndims[i]; ++j)
            {
                strcpy(dimnames[i][j],
                    NrmQuarkToString(thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark));
                dimsizes[i][j] = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_size;

              /*
               *if(NULL != thefile->file.coord_vars[thefile->file.var_info[i]->file_dim_num[j]])
               *{
               *    tmp->u.var->coordnames[j] = thefile->file.file_dim_info[thefile->file.var_info[i]->file_dim_num[j]]->dim_name_quark;
               *}
               *else
               *{
               *    tmp->u.var->coordnames[j] = -1;
               *}
               */
            }
#if 0
            if(NULL != thefile->file.var_att_info[i])
            {
                j = 0;
                step = thefile->file.var_att_info[i];
                while(NULL != step)
                {
                    step = step->next;
                    j++;
                }
                tmp->u.var->n_atts = j;
                tmp->u.var->attnames = (NclQuark*)NclMalloc(sizeof(NclQuark)*j);
                step = thefile->file.var_att_info[i];
                j = 0;
                while(NULL != step)
                {
                    tmp->u.var->attnames[j]= step->the_att->att_name_quark;
                    j++;
                    step = step->next;
                }
            }
#endif
    	}
    }
}

char *NclQuarkToString(NclQuark quark)
{
    return ((char *)NrmQuarkToString(quark));
}

NclQuark NclStringToQuark(const char *str)
{
    return (NrmStringToQuark(str));
}

NclVar readNclFileVar(NclFile thefile, const char *var_name, NclSelectionRecord *sel_ptr)
{
    NclFileClass fc = NULL;
    NclQuark varqname = NrmStringToQuark(var_name);

    if(NULL == thefile)
        return(NULL);

    fc = (NclFileClass)thefile->obj.class_ptr;

    while((NclObjClass)fc != nclObjClass)
    {
        if(fc->file_class.read_var_func != NULL)
        {
            return((*fc->file_class.read_var_func)(thefile, varqname, sel_ptr));
        }
        else
        {
            fc = (NclFileClass)fc->obj_class.super_class;
        }
    }
    return(NULL);
}

void initializeNcl()
{
  /*
   *NhlInitialize();
   */

    _NclInitMachine();
    _NclInitSymbol();
    _NclInitTypeClasses();
    _NclInitDataClasses();
}

void finalizeNcl()
{
    _NclFinalizeSymbol();

    _NclFinalizeMachine();
}

void deleteNclObj(NclObj tmp)
{
    _NclDestroyObj(tmp);
}

void guiNhlDestroy(int plotid)
{
    NhlDestroy(plotid);
}

void guiNhlRLClear(int id)
{
    NhlRLClear(id);
}

int guiNhlRLCreate(NhlRLType type)
{
    return (NhlRLCreate(type));
}

void guiNhlInitialize()
{
    NhlInitialize();
}

void guiNhlRLSetString(int id, char *resname, char *value)
{
    NhlRLSetString(id, (NhlString)resname, (NhlString)value);
}

void guiNhlRLSetInteger(int id, char *resname, int value)
{
    NhlRLSetInteger(id, (NhlString)resname, value);
}

void guiNhlRLSetFloat(int id, char *resname, float value)
{
     NhlRLSetFloat(id, (NhlString) resname, value);
}

void guiNhlRLSetMDFloatArray(int id, char *resname, float *data, int num_dimensions, int *len_dimensions)
{
    int n;
    ng_size_t *dimsizes = (ng_size_t *)NclCalloc(num_dimensions, sizeof(ng_size_t));
    for(n = 0; n < num_dimensions; ++n)
        dimsizes[n] = (ng_size_t)len_dimensions[n];

    NhlRLSetMDFloatArray(id, (NhlString)resname, data, num_dimensions, dimsizes);

    NclFree(dimsizes);
}

void guiNhlRLSetFloatArray(int id, char *resname, float *data, int num_elements)
{
    ng_size_t ndims = (ng_size_t) num_elements;
    NhlRLSetFloatArray(id, (NhlString) resname, data, ndims);
}

void guiNhlSetValues(int plotid, int rlid)
{
    NhlSetValues(plotid, rlid);
}

void guiNhlRLGetFloat(int id,  char *resname, float *value)
{
    NhlRLGetFloat(id, (NhlString) resname, value);
}

void guiNhlRLGetFloatArray(int id, char *resname, float **data, int *num_elements)
{
    ng_size_t ndims;
    NhlRLGetFloatArray(id, (NhlString) resname, data, &ndims);
    *num_elements = (int) ndims;
}

void guiNhlGetValues(int plotid, int rlid)
{
    NhlGetValues(plotid, rlid);
}

void guiNhlDraw(int id)
{
    NhlDraw(id);
}

void guiNhlClose()
{
    NhlClose();
}

void guiNhlFrame(int fid)
{
    NhlFrame(fid);
}

void guiNhlCreate(int *plotid, const char *name, NhlClass class, int pid, int rlid)
{
    NhlCreate(plotid, name, class, pid, rlid);
}

NclObj guiGetObj(int id)
{
    return (NclObj) _NclGetObj(id);
}

void guiRLDestroy(int id)
{
    NhlRLDestroy(id);
}

float* guiGetValue(NclVar _nclvar)
{
    int i = 0;
    size_t n = 0;
    float *value;
    double *dp = NULL;
    short *sp = NULL;
    int *ip = NULL;
    size_t nelm = 1;
    NclMultiDValData tmp_md;
    int _varndims;
    int _vardimsizes[NCL_MAX_DIMENSIONS];

    _varndims = (int) (_nclvar->var.n_dims);
    for(i = 0; i < _varndims; ++i)
    {
        _vardimsizes[i] = (int)_nclvar->var.dim_info[i].dim_size;
        nelm *= _vardimsizes[i] ;
    }

    value = (float *)NclCalloc(nelm, sizeof(float));

    tmp_md = (NclMultiDValData) _NclGetObj(_nclvar->var.thevalue_id);

    switch(tmp_md->multidval.data_type)
    {
        case NCL_float:
             memcpy(value, tmp_md->multidval.val, nelm * sizeof(float));
             return value;
        case NCL_double:
             dp = tmp_md->multidval.val;
             for(n = 0; n < nelm; ++n)
                 value[n] = (float) dp[n];
             return value;
        case NCL_int:
             ip = tmp_md->multidval.val;
             for(n = 0; n < nelm; ++n)
                 value[n] = (float) ip[n];
             return value;
        case NCL_short:
             sp = tmp_md->multidval.val;
             for(n = 0; n < nelm; ++n)
                 value[n] = (float) sp[n];
             return value;
        default:
             break;
    }

    return NULL;
}

int* guiGetIntArray(NclVar _nclvar)
{
    int i = 0;
    int *value;
    size_t nelm = 1;
    NclMultiDValData tmp_md;
    int _varndims;
    int _vardimsizes[NCL_MAX_DIMENSIONS];

    _varndims = (int) (_nclvar->var.n_dims);
    for(i = 0; i < _varndims; ++i)
    {
        _vardimsizes[i] = (int)_nclvar->var.dim_info[i].dim_size;
        nelm *= _vardimsizes[i] ;
    }

    value = (int *)NclCalloc(nelm, sizeof(int));

    tmp_md = (NclMultiDValData) _NclGetObj(_nclvar->var.thevalue_id);

    switch(tmp_md->multidval.data_type)
    {
        case NCL_int:
             memcpy(value, tmp_md->multidval.val, nelm * sizeof(int));
             return value;
        default:
             break;
    }

    return NULL;
}

char* guiGetCharArray(NclVar _nclvar)
{
    int i = 0;
    char *value;
    size_t nelm = 1;
    NclMultiDValData tmp_md;
    int _varndims;
    int _vardimsizes[NCL_MAX_DIMENSIONS];

    _varndims = (int) (_nclvar->var.n_dims);
    for(i = 0; i < _varndims; ++i)
    {
        _vardimsizes[i] = (int)_nclvar->var.dim_info[i].dim_size;
        nelm *= _vardimsizes[i] ;
    }

    value = (char *)NclCalloc(nelm+1, sizeof(char));

    tmp_md = (NclMultiDValData) _NclGetObj(_nclvar->var.thevalue_id);

    switch(tmp_md->multidval.data_type)
    {
        case NCL_char:
             memcpy(value, tmp_md->multidval.val, nelm * sizeof(char));
             return value;
        default:
             break;
    }

    return NULL;
}

double* _readDoubleFromMD(NclMultiDValData tmp_md, size_t nelm)
{
    size_t n = 0;
    double* value = NULL;
    float *fp = NULL;
    short *sp = NULL;
    int *ip = NULL;

    if(nelm < 1)
	return NULL;

    value = (double*)NclCalloc(nelm, sizeof(double));
    assert(value);

    switch(tmp_md->multidval.data_type)
    {
        case NCL_double:
             memcpy(value, tmp_md->multidval.val, nelm * sizeof(double));
             return value;
        case NCL_float:
             fp = tmp_md->multidval.val;
             for(n = 0; n < nelm; ++n)
                 value[n] = (double) fp[n];
             return value;
        case NCL_short:
             sp = tmp_md->multidval.val;
             for(n = 0; n < nelm; ++n)
                 value[n] = (double) sp[n];
             return value;
        case NCL_int:
             ip = tmp_md->multidval.val;
             for(n = 0; n < nelm; ++n)
                 value[n] = (double) ip[n];
             return value;
        default:
             break;
    }

    return NULL;
}

double* guiGetDoubleArray(NclVar _nclvar)
{
    int i = 0;
    double* value = NULL;
    size_t nelm = 1;
    NclMultiDValData tmp_md;
    int _varndims;
    int _vardimsizes[NCL_MAX_DIMENSIONS];

    _varndims = (int) (_nclvar->var.n_dims);
    for(i = 0; i < _varndims; ++i)
    {
        _vardimsizes[i] = (int)_nclvar->var.dim_info[i].dim_size;
        nelm *= _vardimsizes[i] ;
    }

    tmp_md = (NclMultiDValData) _NclGetObj(_nclvar->var.thevalue_id);
    value = _readDoubleFromMD(tmp_md, nelm);

    return value;
}

static NclObj _popListObj(NclList thelist)
{
    NclListObjList *tmp_list;
    NclObj tmp, ret_obj;

    if(NULL == thelist)
        return NULL;

    if((NULL == thelist->list.first) && (NULL == thelist->list.last))
        return NULL;

    if(thelist->list.list_type & NCL_FIFO)
    {
        tmp_list = thelist->list.last;

        if(tmp_list == thelist->list.current_item)
            thelist->list.current_item = tmp_list->next;

        if(thelist->list.last->prev != NULL)
            thelist->list.last->prev->next = NULL;

        thelist->list.last = thelist->list.last->prev;

        if(thelist->list.nelem == 1) 
            thelist->list.first = thelist->list.last;
    }
    else
    {
        tmp_list = thelist->list.first;

        if(tmp_list == thelist->list.current_item)
            thelist->list.current_item = tmp_list->prev;

        if(thelist->list.first->next != NULL)
            thelist->list.first->next->prev = NULL;

        thelist->list.first = thelist->list.first->next;

        if(thelist->list.nelem == 1)
            thelist->list.last = thelist->list.first;
    }

    --thelist->list.nelem;

    tmp = _NclGetObj(tmp_list->obj_id);
    if(NULL == tmp)
        return NULL;

    _NhlCBDelete(tmp_list->cb);
#if 1
     tmp->obj.status = TEMPORARY;
     ret_obj = (NclObj)_NclStripVarData((NclVar)tmp);

    _NclDestroyObj(tmp);
#else
    if(tmp_list->orig_type & Ncl_MultiDValData)
    {
        tmp->obj.status = TEMPORARY;
        ret_obj = (NclObj)_NclStripVarData((NclVar)tmp);

        _NclDestroyObj(tmp);
    }
    else
    {
        ret_obj = _NclGetObj(tmp->obj.id);
        _NclDelParent(tmp, (NclObj) thelist);
    }
#endif

    NclFree(tmp_list);

    return ret_obj;
}

double** guiGetListArray(NclVar _nclvar, int** itemsizes)
{
    int i = 0;
    size_t n = 0;
    double** dblval = NULL;
    int *ip = NULL;
    size_t nelm = 1;
    NclMultiDValData tmp_md;
    int _varndims;
    int _vardimsizes[NCL_MAX_DIMENSIONS];
    int* length;
    NclList thelist = NULL;
    NclList newlist = NULL;
    NclListObjList *tmp_list = NULL;
    NclObj tmp, tmp_obj;

  /*
   *fprintf(stderr, "\nfile %s, line: %d\n", __FILE__, __LINE__);
   */

    tmp_md = (NclMultiDValData) _NclGetObj(_nclvar->var.thevalue_id);

    switch(tmp_md->multidval.data_type)
    {
        case NCL_list:
             thelist = (NclList) _NclGetObj(*(int*)tmp_md->multidval.val);
             nelm = thelist->list.nelem;
             ip = (int *) NclCalloc(nelm, sizeof(int));
             assert(ip);
             dblval = (double **) NclCalloc(nelm, sizeof(double *));
             assert(dblval);

           /*
            *fprintf(stderr, "\nfile %s, line: %d\n", __FILE__, __LINE__);
            *fprintf(stderr, "\tnelm = %d\n", nelm);
            */

             for(n = 0; n < nelm; ++n)
             {
                 tmp_obj = _popListObj(thelist);
                 if(NULL == tmp_obj)
                     continue;

                 tmp_md = (NclMultiDValData) _NclGetObj(tmp_obj->obj.id);
		 if(NCL_list == tmp_md->multidval.data_type)
                 {
                     newlist = (NclList) _NclGetObj(*(int*)tmp_md->multidval.val);
                     tmp_obj = _popListObj(newlist);
                     if(NULL == tmp_obj)
                         continue;

                     tmp_md = (NclMultiDValData) _NclGetObj(tmp_obj->obj.id);

                     ip[n] = tmp_md->multidval.dim_sizes[1];
                     dblval[n] = _readDoubleFromMD(tmp_md, 2*ip[n]);
                   /*
                    *fprintf(stderr, "\tip[%ld] = %d\n", n, ip[n]);
                    *fprintf(stderr, "\tdblval[%ld][0] = %f\n", n, dblval[n][0]);
                    */
                 }
		 else
                 {
                     ip[n] = tmp_md->multidval.dim_sizes[1];
                   /*
                    *fprintf(stderr, "\tip[%ld] = %d\n", n, ip[n]);
                    */
                     dblval[n] = _readDoubleFromMD(tmp_md, 2*ip[n]);
                 }
             }
             *itemsizes = ip;
             return dblval;
        default:
             break;
    }

    *itemsizes = ip;
    return NULL;
}

short* guiGetShortArray(NclVar _nclvar)
{
    int i = 0;
    size_t n = 0;
    short* value = NULL;
    size_t nelm = 1;
    NclMultiDValData tmp_md;
    int _varndims;
    int _vardimsizes[NCL_MAX_DIMENSIONS];

    _varndims = (int) (_nclvar->var.n_dims);
    for(i = 0; i < _varndims; ++i)
    {
        _vardimsizes[i] = (int)_nclvar->var.dim_info[i].dim_size;
        nelm *= _vardimsizes[i] ;
    }

    value = (short*)NclCalloc(nelm, sizeof(short));

    tmp_md = (NclMultiDValData) _NclGetObj(_nclvar->var.thevalue_id);

    switch(tmp_md->multidval.data_type)
    {
        case NCL_short:
             memcpy(value, tmp_md->multidval.val, nelm * sizeof(short));
             return value;
        default:
             break;
    }

    return NULL;
}

void guiNhlRLGetIntegerArray(int id, const char *name, int **data, int *ne)
{
    NhlString resname = (NhlString) name;
    ng_size_t *num_elements;
    NhlRLGetIntegerArray(id, resname, data, num_elements);
    *ne = (int) *num_elements;
}

int guiGetBoundary(int pid, float *left, float *rite, float *bot, float *top)
{
    NhlBoundingBox *thebox;
    NhlLayer instance;

    int *views;
    int i;
    ng_size_t count;
    int grlist;

    int set = 0;

    *left = 0.0;
    *rite = 1.0;
    *bot = 0.0;
    *top = 1.0;

    if(NhlIsWorkstation(pid))
    {
        grlist = NhlRLCreate(NhlGETRL);
        NhlRLClear(grlist);
        NhlRLGetIntegerArray(grlist,NhlNwkTopLevelViews,&views,&count);
        NhlGetValues(pid,grlist);
        for(i = 0; i < count; i++)
        {
            instance = _NhlGetLayer(views[i]);

            if(NULL != instance)
            {
                _NhlGetBB(instance,thebox);

                *left = thebox->l;
                *rite = thebox->r;
                *bot = thebox->b;
                *top = thebox->t;
                 set = thebox->set;
            }
        }

        NhlFree(views);
        NhlRLDestroy(grlist);
    }

    return set;
}

void guiNhlSetColor(int pid, int ci, float red, float green, float blue)
{
    NhlSetColor(pid, ci, red, green, blue);
}

void guiNhlRLSetStringArray(int id, char *resname, char **data, int nelem)
{
    ng_size_t num_elements = (ng_size_t) nelem;

    NhlRLSetStringArray(id, resname, data, num_elements);
}

struct _NclMultiDValDataRec *guiGetFileAtt(NclFile thefile, const char *attname)
{
    NclQuark qn = NrmStringToQuark(attname);
    return (_NclFileReadAtt(thefile, qn, NULL));
}

void guiDestroyObj(NclObj obj)
{
    _NclDestroyObj(obj);
}

char *guiQuarkToString(NclQuark qn)
{
    return (NrmQuarkToString(qn));
}

void guiNhlAddOverlay(int base_id, int transform_id, int after_id)
{
    NhlAddOverlay(base_id, transform_id, after_id);
}

void guiGetEndpointsAndStepSize(float min_in, float max_in,
                                int max_steps, int outside, 
                                float *min_out, float *max_out,
                                float *step_size)
{
    double in_min = (double) min_in;
    double in_max = (double) max_in;
    double out_min;
    double out_max;
    double space;

    _NhlGetEndpointsAndStepSize(in_min, in_max, max_steps, outside,
                                &out_min, &out_max, &space);
    *min_out = (float) out_min;
    *max_out = (float) out_max;
    *step_size = (float) space;
}

#if 0
extern void NhlRLUnSet(
#if	NhlNeedProto
	int		id,	/* RL list 		*/
	NhlString	name	/* resname to unset	*/
#endif
);

extern NhlBoolean NhlRLIsSet(
#if	NhlNeedProto
	int		id,	/* RL list		*/
	NhlString	name	/* resname to unset	*/
#endif
);

/*VARARGS3*/
extern NhlErrorTypes NhlRLSet(
#if	NhlNeedVarArgProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	type,		/* type of value		*/
	...				/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetLong(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	long		value		/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetDouble(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	double		value		/* value to set resname to	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	data,		/* array			*/
	NhlString	type,		/* type of elements of array	*/
	ng_size_t	size,		/* size of elements of array	*/
	int		num_dimensions,	/* number dimensions in array	*/
	ng_size_t	*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDIntegerArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*data,		/* array			*/
	int		num_dimensions,	/* number dimensions in array	*/
	ng_size_t	*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDLongArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	long		*data,		/* array			*/
	int		num_dimensions,	/* number dimensions in array	*/
	ng_size_t	*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetMDDoubleArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	double		*data,		/* array			*/
	int		num_dimensions,	/* number dimensions in array	*/
	ng_size_t	*len_dimensions	/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	data,		/* array			*/
	NhlString	type,		/* type of elements of array	*/
	ng_size_t	size,		/* size of elements of array	*/
	ng_size_t	num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetIntegerArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*data,		/* array			*/
	ng_size_t		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetLongArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	long		*data,		/* array			*/
	ng_size_t		num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetFloatArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		*data,		/* array			*/
	ng_size_t	num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLSetDoubleArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	double		*data,		/* array			*/
	ng_size_t	num_elements	/* number elements in array	*/
#endif
);

/*VARARGS3*/
extern NhlErrorTypes NhlRLGet(
#if	NhlNeedVarArgProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	type,		/* type of value		*/
	...				/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetInteger(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetLong(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	long		*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetDouble(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	double		*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetString(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	*value		/* addr to put value in		*/
#endif
);

extern NhlErrorTypes NhlRLGetMDArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	*data,		/* array			*/
	NhlString	*type,		/* type of elements of array	*/
	unsigned int	*size,		/* size of elements of array	*/
	int		*num_dimensions,/* number dimensions in array	*/
	ng_size_t	**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetMDIntegerArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		**data,		/* array			*/
	int		*num_dimensions,/* number dimensions in array	*/
	ng_size_t	**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetMDLongArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	long		**data,		/* array			*/
	int		*num_dimensions,/* number dimensions in array	*/
	ng_size_t	**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetMDFloatArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		**data,		/* array			*/
	int		*num_dimensions,/* number dimensions in array	*/
	ng_size_t	**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetMDDoubleArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	double		**data,		/* array			*/
	int		*num_dimensions,/* number dimensions in array	*/
	ng_size_t	**len_dimensions/* len each dimension in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlPointer	*data,		/* array			*/
	NhlString	*type,		/* type of elements of array	*/
	unsigned int	*size,		/* size of elements of array	*/
	ng_size_t	*num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetIntegerArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	int		**data,		/* array			*/
	ng_size_t	*num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetLongArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	long		**data,		/* array			*/
	ng_size_t	*num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetDoubleArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	double		**data,		/* array			*/
	ng_size_t	*num_elements	/* number elements in array	*/
#endif
);

extern NhlErrorTypes NhlRLGetStringArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	**data,		/* array			*/
	ng_size_t	*num_elements	/* number elements in array	*/
#endif
);
#endif


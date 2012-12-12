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
#include "NclNewFile.h"
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

NclFile NclCreateFile(const char *path)
{
    NclQuark qpath = NrmStringToQuark(path);
    return (_NclCreateFile(NULL,NULL,Ncl_File,0,TEMPORARY,qpath,0));
}

char **GetNclFileVarNames(NclFile thefile, int *num_vars)
{
    int n = 0;
    NclQuark *qvars = NULL;
    char *str = NULL;
    char **varnames = NULL;

    if(NULL == thefile)
        return 0;

    qvars = _NclFileReadVarNames(thefile, num_vars);

    if(1 > *num_vars)
        return 0;

    if(NULL == varnames)
        varnames = (char **)NclCalloc(*num_vars, sizeof(char *));
    else
    {
        fprintf(stderr, "\n\nWARNING!\n\n");
        fprintf(stderr, "file %s, line: %d, function: %s\n",
                         __FILE__, __LINE__, __PRETTY_FUNCTION__);
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
       *fprintf(stderr, "file %s, line: %d, function: %s\n",
       *                 __FILE__, __LINE__, __PRETTY_FUNCTION__);
       *fprintf(stderr, "\tVar %d: <%s>\n", n, varnames[n]);
       */
    }

    return varnames;
}

void getNclFileVarInfo(NclFile thefile, int *ndims, int **dimsizes, char ***dimnames, long *type)
{
    int i,j;

    if(NULL == thefile)
    return;

    if(_isNewFileStructure(thefile))
    {
        NclNewFile thenewfile = (NclNewFile) thefile;
        NclFileVarNode *varnode = NULL;

        if(NULL != thenewfile->newfile.grpnode->var_rec)
        {
            for(i = 0; i < thenewfile->newfile.grpnode->var_rec->n_vars; ++i)
            {
                varnode = &(thenewfile->newfile.grpnode->var_rec->var_node[i]);

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
       */
        fprintf(stderr, "file %s, line: %d, function: %s\n",
                         __FILE__, __LINE__, __PRETTY_FUNCTION__);
        fprintf(stderr, "\tnumber of vars = %d\n", thefile->file.n_vars);

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

void guiNhlRLSetMDFloatArray(int id, char *resname, float *data, int num_dimensions, ng_size_t *len_dimensions)
{
    NhlRLSetMDFloatArray(id, (NhlString)resname, data, num_dimensions, len_dimensions);
}

void guiNhlRLSetFloatArray(int id, char *resname, float *data, ng_size_t num_elements)
{
    NhlRLSetFloatArray(id, (NhlString) resname, data, num_elements);
}

void guiNhlSetValues(int plotid, int rlid)
{
    NhlSetValues(plotid, rlid);
}

void guiNhlRLGetFloatArray(int id, char *resname, float **data, ng_size_t *num_elements)
{
    NhlRLGetFloatArray(id, (NhlString) resname, data, num_elements);
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

#if 0
extern void NhlRLDestroy(
#if	NhlNeedProto
	int	id	/* RL list to destroy	*/
#endif
);

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

extern NhlErrorTypes NhlRLSetStringArray(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	NhlString	*data,		/* array			*/
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

extern NhlErrorTypes NhlRLGetFloat(
#if	NhlNeedProto
	int		id,		/* RL list			*/
	NhlString	resname,	/* resource to set		*/
	float		*value		/* addr to put value in		*/
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


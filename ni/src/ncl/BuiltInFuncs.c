/*
 *      $Id$
 */
/************************************************************************
*                                                                       *
*                   Copyright (C)  1995                                 *
*           University Corporation for Atmospheric Research             *
*                   All Rights Reserved                                 *
*                                                                       *
************************************************************************/
/*
 *	File:		BuiltInFuncs.c
 *
 *	Author:		Ethan Alpert
 *			National Center for Atmospheric Research
 *			PO 3000, Boulder, Colorado
 *
 *	Date:		Tue Feb 7 12:12:21 MST 1995
 *
 *	Description:	
 */
#ifdef __cplusplus
extern "C" {
#endif
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

extern int cmd_line;
extern short NCLnoSysPager;
extern char *nclf;

/* 
 * Function to coerce dimension sizes to int or long
 * Located in ../lib/nfp/wrapper.[ch].
 */
extern ng_size_t *get_dimensions(void *tmp_dimensions, int n_dimensions,
			   NclBasicDataTypes type_dimensions,
			   const char *);

NclStackEntry _NclCreateAList(const char *buffer);

/* 
 * Function to get dimension indexes via integers or dimension names. 
 * Located in ../lib/nfp/wrapper.[ch].
 */
extern int *get_dims_for_n_funcs(int arg_num,  int num_args, 
				 NclStackEntry tmpdata,
				 const char *name, int *ndims);

extern NhlErrorTypes _NclPreLoadScript(char *path, int status);

NhlErrorTypes NclGetCPUTime(float *time);
NhlErrorTypes NclGetWTime(double *time);

void IncLine(void);

NhlErrorTypes _NclIGetScriptPrefixName
#if     NhlNeedProto
(void)
#else
()
#endif
{
    ng_size_t dimsz = 1;
    int       ndims = 1;
    NclQuark  script_name;
    char    *lastdot = NULL,
            *prefix_nclf = NULL;
    NclScalar   missing;

    if (nclf == (char *) NULL) {
        /* set to "missing" */
        script_name = ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
        missing.stringval = ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    } else {
        /* find last "dot" in the filename */
        lastdot = strrchr(nclf, '.');
        if (lastdot != (char *) NULL) {
            prefix_nclf = (char *) NclCalloc(1, strlen(nclf) - strlen(lastdot) + 1);
            if (prefix_nclf == (char *) NULL) {
                NhlPError(NhlFATAL, NhlEUNKNOWN, "get_script_prefix_name: cannot allocate space");
                return NhlFATAL;
            }

            (void) strncpy(prefix_nclf, nclf, strlen(nclf) - strlen(lastdot));
            script_name = NrmStringToQuark(prefix_nclf);

            NclFree(prefix_nclf);
        } else {
            /* no "dot tag" in filename */
            script_name = NrmStringToQuark(nclf);
        }
    }
 
    /* return a _FillValue only if no script name was provided */
    return NclReturnValue(&script_name,
                            ndims,
                            &dimsz,
                            (nclf == (char *) NULL ? &missing : NULL),
                            NCL_string,
                            1);
}

NhlErrorTypes _NclIGetScriptName
#if     NhlNeedProto
(void)
#else
()
#endif
{
    ng_size_t dimsz = 1;
    int       ndims = 1;
    NclQuark  script_name;
    NclScalar   missing;

    if (nclf == (char *) NULL) {
        /* set to "missing" */
        script_name = ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
        missing.stringval = ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
    } else 
        script_name = NrmStringToQuark(nclf);

    /* return a _FillValue only if no script name was provided */
    return NclReturnValue(&script_name,
                            ndims,
                            &dimsz,
                            (nclf == (char *) NULL ? &missing : NULL),
                            NCL_string,
                            1);
}


NhlErrorTypes _NclIListHLUObjs
#if	NhlNeedProto
(void)
#else
()
#endif
{
        FILE *fp;
        NclApiDataList *tmp,*step;
        int i,ret=0;
	tmp = _NclGetDefinedHLUInfo();

	if(cmd_line == 1) {
		_NclStartCmdLinePager();
	}
	fp = _NclGetOutputStream();
	

	step = tmp;
	while(step != NULL) {
		ret = nclfprintf(fp,"\nVariable: %s\n",NrmQuarkToString(step->u.hlu->name));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0 ; i < step->u.hlu->n_objs; i++) {
			if(step->u.hlu->objs[i].obj_name !=0) {
				ret = nclfprintf(fp,"\t%s\t%s\n",NrmQuarkToString(step->u.hlu->objs[i].obj_name),NrmQuarkToString(step->u.hlu->objs[i].obj_class));
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
					return(NhlWARNING);
				}
			} else {
				ret = nclfprintf(fp,"\tmissing\n");
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
					return(NhlWARNING);
				}
			}
		}
		step = step->next;
	}
	_NclFreeApiDataList((void*)tmp);
	if(cmd_line == 1 ) {
                _NclEndCmdLinePager();
        }
	return(NhlNOERROR);
}

static long long _Ncl_llabs
#if     NhlNeedProto
(
        long long val
)
#else
(val)
long long val;
#endif
{
        if(val < 0)
		return (-val);
        else
		return (val);
}

NhlErrorTypes _NclIListVariables
#if	NhlNeedProto
(void)
#else
()
#endif
{
	FILE *fp;
	NclApiDataList *tmp,*step;
	int i,ret=0;
	

	if(cmd_line == 1) {
		_NclStartCmdLinePager();
	}
	fp = _NclGetOutputStream();
	tmp = _NclGetDefinedVarInfo();
	step = tmp;

	while(step != NULL) {
		ret = nclfprintf(fp,"\n%s\t%s ",_NclBasicDataTypeToName((NclBasicDataTypes)step->u.var->data_type),NrmQuarkToString(step->u.var->name));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0; i < step->u.var->n_dims - 1; i++) {
			ret = nclfprintf(fp,"[ ");
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
			if(step->u.var->dim_info[i].dim_quark != -1) {
				ret = nclfprintf(fp,"%s | ",NrmQuarkToString(step->u.var->dim_info[i].dim_quark));
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
					return(NhlWARNING);
				}
			}
			ret = nclfprintf(fp,"%zd ] x ",step->u.var->dim_info[i].dim_size);
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
		}
		ret = nclfprintf(fp,"[ ");
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		if(step->u.var->dim_info[step->u.var->n_dims - 1].dim_quark != -1) {
                	ret = nclfprintf(fp,"%s | ",NrmQuarkToString(step->u.var->dim_info[step->u.var->n_dims - 1].dim_quark));
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
                }
                ret = nclfprintf(fp,"%zd ]\n",step->u.var->dim_info[step->u.var->n_dims - 1].dim_size);
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0; i < step->u.var->n_atts; i++) {
			ret = nclfprintf(fp,"\t%s\n",NrmQuarkToString(step->u.var->attnames[i]));
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
		}
		step = step->next;
	}
	_NclFreeApiDataList((void*)tmp);
	if(cmd_line == 1) {
                _NclEndCmdLinePager();
        }
	return(NhlNOERROR);
}

NhlErrorTypes _NclIListFiles
#if	NhlNeedProto
(void)
#else
()
#endif
{
	FILE *fp;
	NclApiDataList *tmp,*step;
	int i;
	int ret = 0;
	

	if(cmd_line == 1) {
		_NclStartCmdLinePager();
	}
	fp = _NclGetOutputStream();
	tmp = _NclGetDefinedFileInfo();
	step = tmp;
	while(step != NULL) {
		ret = nclfprintf(fp,"\n%s\t%s\n",NrmQuarkToString(step->u.file->name),(step->u.file->wr_status > 0 ? "READ ONLY" : "READ/WRITE"));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		ret = nclfprintf(fp,"\t%s\n",NrmQuarkToString(step->u.file->path));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		ret = nclfprintf(fp,"\tDimensions:\n");
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0; i < step->u.file->n_dims; i++) {
			ret = nclfprintf(fp,"\t\t(%d) ",i);
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
			if(step->u.file->dim_info[i].dim_quark != -1) {
				ret = nclfprintf(fp,"%s ",NrmQuarkToString(step->u.file->dim_info[i].dim_quark));
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
					return(NhlWARNING);
				}
			}
			ret = nclfprintf(fp,"%zd\n",step->u.file->dim_info[i].dim_size);
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
		}
		ret = nclfprintf(fp,"\tAttributes:\n");
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0; i < step->u.file->n_atts; i++) {
			ret = nclfprintf(fp,"\t\t%s\n",NrmQuarkToString(step->u.file->attnames[i]));
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
		}
		step = step->next;
	}
	
	_NclFreeApiDataList((void*)tmp);
	if(cmd_line == 1) {
                _NclEndCmdLinePager();
        }
	return(NhlNOERROR);
}

NhlErrorTypes _NclIListFuncs
#if	NhlNeedProto
(void)
#else
()
#endif
{
	FILE *fp;
	NclApiDataList *tmp,*step;
	int i,j;
	int ret = 0;
	

	if(cmd_line == 1) {
		_NclStartCmdLinePager();
	}
	fp = _NclGetOutputStream();
	tmp = _NclGetDefinedProcFuncInfo();
	step = tmp;

	while(step != NULL) {
		ret = nclfprintf(fp,"\n%s ", (step->u.func->kind ? "function" : "procedure"));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
        		return(NhlWARNING);
		}
		ret = nclfprintf(fp,"%s (",NrmQuarkToString(step->u.func->name));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
        		return(NhlWARNING);
		}
	
		if(step->u.func->nparams > 0 ) {	
			ret = nclfprintf(fp,"\n");
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
        			return(NhlWARNING);
			}
			for(i = 0; i < step->u.func->nparams - 1 ; i++) {
/*
				ret = nclfprintf(fp,"\t%s ",step->u.func->theargs[i].arg_sym->name);
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
        				return(NhlWARNING);
				}
*/
				ret = nclfprintf(fp,"\t");
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
        				return(NhlWARNING);
				}
				if(step->u.func->theargs[i].is_dimsizes) {
					for(j = 0; j < step->u.func->theargs[i].n_dims; j++ ) {
						if(step->u.func->theargs[i].dim_sizes[j] > 0) {
							ret = nclfprintf(fp,"[%zd]",step->u.func->theargs[i].dim_sizes[j]);
							if(ret < 0) {
								_NclFreeApiDataList((void*)tmp);
        							return(NhlWARNING);
							}
						} else {
							ret = nclfprintf(fp,"[*]");
							if(ret < 0) {
								_NclFreeApiDataList((void*)tmp);
        							return(NhlWARNING);
							}
						}
					}
				}
				else if (step->u.func->theargs[i].n_dims > 0) {
					for(j = 0; j < step->u.func->theargs[i].n_dims; j++ ) {
						ret = nclfprintf(fp,"[*]");
						if(ret < 0) {
							_NclFreeApiDataList((void*)tmp);
							return(NhlWARNING);
						}
					}
				}
				if(step->u.func->theargs[i].arg_data_type > 0) {
					ret = nclfprintf(fp,": %s,\n",NrmQuarkToString(step->u.func->theargs[i].arg_data_type));
					if(ret < 0) {
						_NclFreeApiDataList((void*)tmp);
        					return(NhlWARNING);
					}
				} else {
					ret = nclfprintf(fp,": any type,\n");
					if(ret < 0) {
						_NclFreeApiDataList((void*)tmp);
        					return(NhlWARNING);
					}
				}
			}
/*
			ret = nclfprintf(fp,"\t%s ",step->u.func->theargs[step->u.func->nparams-1].arg_sym->name);
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
        			return(NhlWARNING);
			}
*/
			ret = nclfprintf(fp,"\t");
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
        			return(NhlWARNING);
			}
			if(step->u.func->theargs[step->u.func->nparams-1].is_dimsizes) {
				for(j = 0; j < step->u.func->theargs[step->u.func->nparams-1].n_dims; j++ ) {
					if(step->u.func->theargs[step->u.func->nparams-1].dim_sizes[j] > 0) {
						ret = nclfprintf(fp,"[%zd]",step->u.func->theargs[step->u.func->nparams-1].dim_sizes[j]);
						if(ret < 0) {
							_NclFreeApiDataList((void*)tmp);
        						return(NhlWARNING);
						}
					} else {
						ret = nclfprintf(fp,"[*]");
						if(ret < 0) {
							_NclFreeApiDataList((void*)tmp);
        						return(NhlWARNING);
						}
					}
				}
			}
			else if (step->u.func->theargs[step->u.func->nparams-1].n_dims > 0) {
				for(j = 0; j < step->u.func->theargs[step->u.func->nparams-1].n_dims; j++ ) {
					ret = nclfprintf(fp,"[*]");
					if(ret < 0) {
						_NclFreeApiDataList((void*)tmp);
						return(NhlWARNING);
					}
				}
			}
			if(step->u.func->theargs[step->u.func->nparams-1].arg_data_type > 0) {
				ret = nclfprintf(fp,": %s\n",NrmQuarkToString(step->u.func->theargs[step->u.func->nparams-1].arg_data_type));
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
        				return(NhlWARNING);
				}
			} else {
				ret = nclfprintf(fp,": any type\n");
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
        				return(NhlWARNING);
				}
			}
		} 
		ret = nclfprintf(fp,")\n");
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
        		return(NhlWARNING);
		}
		step = step->next;
	}
	
	_NclFreeApiDataList((void*)tmp);
	if(cmd_line == 1) {
                _NclEndCmdLinePager();
        }
        return(NhlNOERROR);
}

NclQuark *_NclGetAdvancedFileVarNames(void *therec, int *num_vars, int level)
{
    NclFileGrpNode *grpnode = (NclFileGrpNode *) therec;
    NclFileGrpNode *tmpgrpnode = NULL;
    NclQuark *out_quarks = NULL;
    NclQuark *tmp_quarks = NULL;
    int n, nv;
    int i;

    *num_vars = 0;

    if(NULL != grpnode->var_rec)
    {
        if(grpnode->var_rec->n_vars)
        {
            *num_vars = grpnode->var_rec->n_vars;

            out_quarks = (NclQuark *)NclCalloc(*num_vars, sizeof(NclQuark));
            assert(out_quarks);

            for(i = 0; i < grpnode->var_rec->n_vars; ++i)
            {
                out_quarks[i] = (level == 0) ?
                    grpnode->var_rec->var_node[i].name :
                    grpnode->var_rec->var_node[i].real_name;
            }
        }
    }

    if(NULL != grpnode->grp_rec)
    {
        if(grpnode->grp_rec->n_grps)
        {
            for(n = 0; n < grpnode->grp_rec->n_grps; ++n)
            {
                tmpgrpnode = grpnode->grp_rec->grp_node[n];

                tmp_quarks = _NclGetAdvancedFileVarNames((void *)tmpgrpnode, &nv, ++level);

                if(nv)
                {
                    out_quarks = (NclQuark *)NclRealloc(out_quarks,
                                                (*num_vars + nv) * sizeof(NclQuark));
                    assert(out_quarks);

                    for(i = 0; i < nv; ++i)
                    {
                        out_quarks[*num_vars + i] = tmp_quarks[i];
                    }
                    NclFree(tmp_quarks);

                    *num_vars += nv;
                }
            }
        }
    }

    return(out_quarks);
}

NclQuark *_NclAdvancedFileReadVarNames(NclFile thefile, int *num_vars)
{
    NclAdvancedFile advancedfile = (NclAdvancedFile) thefile;
    NclQuark *out_quarks = NULL;

    out_quarks = _NclGetAdvancedFileVarNames((void *)advancedfile->advancedfile.grpnode, num_vars, 0);

    return(out_quarks);
}

NhlErrorTypes _NclIGetFileVarNames
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclQuark *var_names = NULL;
	NclQuark file_q;
	ng_size_t dimsize = 0;
	int num_vars = 0;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md = NULL;
	
	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		file_q = data.u.data_var->var.var_quark;
		break;
	case NclStk_VAL:
		file_q = -1;
		tmp_md = data.u.data_obj;
		break;
	default:
		return(NhlFATAL);
	}
	if(file_q == -1)
	{
		if(tmp_md==NULL) 
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		thefile = (NclFile)_NclGetObj(*(int*)tmp_md->multidval.val);
	}
	else
	{
		NclSymbol *s = NULL;
		NclStackEntry *thevar = NULL;
		NclMultiDValData theid = NULL;

		s = _NclLookUp(NrmQuarkToString(file_q));
		if((s != NULL)&&(s->type != UNDEF))
		{
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if(thevar->kind == NclStk_VAR)
			{
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData)
				{
					thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
				}
			}
		}
	}
        
        if (thefile == NULL) {
		NclQuark *tmp_str =(NclQuark*) NclMalloc(((NclTypeClass)nclTypestringClass)->type_class.size);
		*tmp_str = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
		dimsize = (ng_size_t)1;
		data.kind = NclStk_VAL;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"getfilevarnames: %s is not a valid file variable",
			  NrmQuarkToString(file_q));
		data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)tmp_str,
						      &((NclTypeClass)nclTypestringClass)->type_class.default_mis,
						      1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
		_NclPlaceReturn(data);
		return(NhlWARNING);            
        }

        if(thefile->file.advanced_file_structure)
		var_names = _NclAdvancedFileReadVarNames(thefile, &num_vars);
	else
		var_names = _NclFileReadVarNames(thefile, &num_vars);

	if (NULL == var_names || num_vars == 0) {
		NclQuark *tmp_str =(NclQuark*) NclMalloc(((NclTypeClass)nclTypestringClass)->type_class.size);
		*tmp_str = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
		dimsize = (ng_size_t)1;
		data.kind = NclStk_VAL;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"getfilevarnames: %s contains no variables readable by NCL",
			  NrmQuarkToString(thefile->file.fname));
		data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)tmp_str,
						      &((NclTypeClass)nclTypestringClass)->type_class.default_mis,
						      1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
		_NclPlaceReturn(data);
		return(NhlWARNING);
	}
	dimsize = (ng_size_t)num_vars;
	data.kind = NclStk_VAL;
	data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)var_names,NULL,1,&dimsize,
					      TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
	_NclPlaceReturn(data);
       	return(NhlNOERROR);
}

NhlErrorTypes _NclIGetFileGrpNames
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclQuark *grp_names = NULL;
	NclQuark file_q;
	ng_size_t dimsize = 0;
	int num_vars = 0;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md = NULL;
	
	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		file_q = data.u.data_var->var.var_quark;
		break;
	case NclStk_VAL:
		file_q = -1;
		tmp_md = data.u.data_obj;
		break;
	default:
		return(NhlFATAL);
	}
	if(file_q == -1)
	{
		if(tmp_md==NULL) 
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		thefile = (NclFile)_NclGetObj(*(int*)tmp_md->multidval.val);
	}
	else
	{
		NclSymbol *s = NULL;
		NclStackEntry *thevar = NULL;
		NclMultiDValData theid = NULL;

		s = _NclLookUp(NrmQuarkToString(file_q));
		if((s != NULL)&&(s->type != UNDEF))
		{
			thevar = _NclRetrieveRec(s,DONT_CARE);
			if(thevar->kind == NclStk_VAR)
			{
				theid = _NclVarValueRead(thevar->u.data_var,NULL,NULL);
				if(theid->obj.obj_type_mask & Ncl_MultiDValnclfileData)
				{
					thefile = (NclFile)_NclGetObj(*(int*)theid->multidval.val);
				}
			}
		}
	}

	if(NULL != thefile)
		grp_names = _NclFileReadGrpNames(thefile, &num_vars);
		
	dimsize = (ng_size_t)num_vars;
	data.kind = NclStk_VAL;
	data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)grp_names,NULL,1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
	_NclPlaceReturn(data);
       	return(NhlNOERROR);
}

NhlErrorTypes _NclIListFileVariables
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	FILE *fp;
	NclApiDataList *tmp,*step;
	NclQuark file_q;
	int i,ret =0;
	NclMultiDValData tmp_md;
	NclFile thefile;
	

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		 file_q = data.u.data_var->var.var_quark;
		break;
	case NclStk_VAL:
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return(NhlFATAL);
	}
	if(cmd_line == 1) {
		_NclStartCmdLinePager();
	}
	fp = _NclGetOutputStream();
	if(file_q == -1) {
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		thefile = (NclFile)_NclGetObj(*(int*)tmp_md->multidval.val);
		
		tmp = _NclGetFileVarInfoList2(thefile);
	} else {
		tmp = _NclGetFileVarInfoList(file_q);
	}
	step = tmp;
	while(step != NULL) {
		ret = nclfprintf(fp,"\n%s\t%s ",_NclBasicDataTypeToName((NclBasicDataTypes)step->u.var->data_type),NrmQuarkToString(step->u.var->name));
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0; i < step->u.var->n_dims - 1; i++) {
			ret = nclfprintf(fp,"[ ");
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
			if(step->u.var->dim_info[i].dim_quark != -1) {
				ret = nclfprintf(fp,"%s | ",NrmQuarkToString(step->u.var->dim_info[i].dim_quark));
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
					return(NhlWARNING);
				}
			}
			ret = nclfprintf(fp,"%zd ] x ",step->u.var->dim_info[i].dim_size);
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
		}
		ret = nclfprintf(fp,"[ ");
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		if(step->u.var->dim_info[step->u.var->n_dims - 1].dim_quark != -1) {
                	ret = nclfprintf(fp,"%s | ",NrmQuarkToString(step->u.var->dim_info[step->u.var->n_dims - 1].dim_quark));
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
                }
                ret = nclfprintf(fp,"%zd ]\n",step->u.var->dim_info[step->u.var->n_dims - 1].dim_size);
		if(ret < 0) {
			_NclFreeApiDataList((void*)tmp);
			return(NhlWARNING);
		}
		for(i = 0; i < step->u.var->n_atts; i++) {
			ret = nclfprintf(fp,"\t%s\n",NrmQuarkToString(step->u.var->attnames[i]));
			if(ret < 0) {
				_NclFreeApiDataList((void*)tmp);
				return(NhlWARNING);
			}
		}
		step = step->next;
	}
	if(cmd_line == 1) {
                _NclEndCmdLinePager();
        }
	_NclFreeApiDataList((void*)tmp);
        return(NhlNOERROR);
}



NhlErrorTypes _NclINhlDataToNDC
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args[5];
	NclMultiDValData tmp_mds[5];
	NclVar tmp_vars[5];
	int i;
	int ncl_id;
	ng_size_t tmp_dimsizes = 1;
	NclHLUObj hlu_ptr;
	int status;
	NclScalar* missing, *missing1;
	NclScalar tmp_mis;
	NclMultiDValData tmp_miss_md3,tmp_miss_md4;
	

	for(i = 0 ; i < 5; i++) {
		if(i < 3) {
			args[i] = _NclGetArg(i,5,DONT_CARE);
		} else {
			args[i] = _NclGetArg(i,5,WRITE_IT);
		}
		switch(args[i].kind) {
		case NclStk_VAL:
			tmp_mds[i] = args[i].u.data_obj;
			tmp_vars[i] = NULL;
			break;
		case NclStk_VAR:
			tmp_mds[i] = _NclVarValueRead(args[i].u.data_var,
					NULL,NULL);
			tmp_vars[i] = args[i].u.data_var;
			break;
		default:
			return(NhlFATAL);
		}
	}
	ncl_id = *(int*)tmp_mds[0]->multidval.val;
	hlu_ptr = (NclHLUObj)_NclGetObj(ncl_id);
	if(hlu_ptr != NULL) {
		if(tmp_mds[1]->multidval.totalelements != tmp_mds[2]->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"datatondc: Arguments 2 and 3 must have identical dimension sizes");
			return(NhlFATAL);
		}
		if(tmp_mds[1]->multidval.totalelements != tmp_mds[3]->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"datatondc: Arguments 2 and 4 must have identical dimension sizes");
			return(NhlFATAL);
		}
		if(tmp_mds[2]->multidval.totalelements != tmp_mds[4]->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"datatondc: Arguments 3 and 5 must have identical dimension sizes");
			return(NhlFATAL);
		}
		if(tmp_mds[1]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[1]->multidval.missing_value.value;
		} else if(tmp_mds[2]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[2]->multidval.missing_value.value;
		} else if(tmp_mds[3]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[3]->multidval.missing_value.value;
		} else if(tmp_mds[4]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[4]->multidval.missing_value.value;
		} else {
			tmp_mis.floatval = 1e12;
			missing = &tmp_mis;
		}
		status = 0;
		NhlDataToNDC(hlu_ptr->hlu.hlu_id,
			(float*)tmp_mds[1]->multidval.val,
			(float*)tmp_mds[2]->multidval.val,
			tmp_mds[1]->multidval.totalelements,
			(float*)tmp_mds[3]->multidval.val,
			(float*)tmp_mds[4]->multidval.val,
			(float*)(tmp_mds[1]->multidval.missing_value.has_missing ?
				&tmp_mds[1]->multidval.missing_value.value :
				NULL),
			(float*)(tmp_mds[2]->multidval.missing_value.has_missing ?
				&tmp_mds[2]->multidval.missing_value.value :
				NULL),
			&status,
			(float*)missing);
		if(status) {
			if(tmp_vars[3] != NULL) {
				missing1 = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
				*missing1 = *missing;
				tmp_miss_md3 = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)missing1, NULL, 1 , &tmp_dimsizes, TEMPORARY, NULL,(NclObjClass) tmp_mds[3]->multidval.type);
				_NclWriteAtt(tmp_vars[3],NCL_MISSING_VALUE_ATT,tmp_miss_md3,NULL);
				if(tmp_miss_md3->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)tmp_miss_md3);
				}
			} else {	
				if(missing != NULL) {
					_NclResetMissingValue(tmp_mds[3],missing);
				}
			}
			if(tmp_vars[4] != NULL) {
				missing1 = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
				*missing1 = *missing;
				tmp_miss_md4 = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)missing1, NULL, 1 , &tmp_dimsizes, TEMPORARY, NULL, (NclObjClass)tmp_mds[4]->multidval.type);
				_NclWriteAtt(tmp_vars[4],NCL_MISSING_VALUE_ATT,tmp_miss_md4,NULL);
				if(tmp_miss_md4->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)tmp_miss_md4);
				}
			} else {
				if(missing != NULL) {
					_NclResetMissingValue(tmp_mds[4],missing);
				}
			}
		}
		return(NhlNOERROR);	
	} else {
		return(NhlFATAL);
	}
}

NhlErrorTypes _NclINhlNDCToData
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args[5];
	NclMultiDValData tmp_mds[5];
	NclVar tmp_vars[5];
	int i;
	int ncl_id;
	ng_size_t tmp_dimsizes = 1;
	NclHLUObj hlu_ptr;
	int status;
	NclScalar* missing, *missing1;
	NclScalar tmp_mis;
	NclMultiDValData tmp_miss_md3,tmp_miss_md4;

	for(i = 0 ; i < 5; i++) {
		if(i < 3) {
			args[i] = _NclGetArg(i,5,DONT_CARE);
		} else {
			args[i] = _NclGetArg(i,5,WRITE_IT);
		}
		switch(args[i].kind) {
		case NclStk_VAL:
			tmp_mds[i] = args[i].u.data_obj;
			break;
		case NclStk_VAR:
			tmp_mds[i] = _NclVarValueRead(args[i].u.data_var,
					NULL,NULL);
			tmp_vars[i] = args[i].u.data_var;
			break;
		default:
			return(NhlFATAL);
		}
	}
	ncl_id = *(int*)tmp_mds[0]->multidval.val;
	hlu_ptr = (NclHLUObj)_NclGetObj(ncl_id);
	if(hlu_ptr != NULL) {
		if(tmp_mds[1]->multidval.totalelements != tmp_mds[2]->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"datatondc: Arguments 2 and 3 must have identical dimension sizes");
			return(NhlFATAL);
		}
		if(tmp_mds[1]->multidval.totalelements != tmp_mds[3]->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"datatondc: Arguments 2 and 4 must have identical dimension sizes");
			return(NhlFATAL);
		}
		if(tmp_mds[2]->multidval.totalelements != tmp_mds[4]->multidval.totalelements) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"datatondc: Arguments 3 and 5 must have identical dimension sizes");
			return(NhlFATAL);
		}
		if(tmp_mds[1]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[1]->multidval.missing_value.value;
		} else if(tmp_mds[2]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[2]->multidval.missing_value.value;
		} else if(tmp_mds[3]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[3]->multidval.missing_value.value;
		} else if(tmp_mds[4]->multidval.missing_value.has_missing) {
			missing = &tmp_mds[4]->multidval.missing_value.value;
		} else {
			tmp_mis.floatval = 1e12;
			missing = &tmp_mis;
		}
		status = 0;
		NhlNDCToData(hlu_ptr->hlu.hlu_id,
			(float*)tmp_mds[1]->multidval.val,
			(float*)tmp_mds[2]->multidval.val,
			tmp_mds[1]->multidval.totalelements,
			(float*)tmp_mds[3]->multidval.val,
			(float*)tmp_mds[4]->multidval.val,
			(float*)(tmp_mds[1]->multidval.missing_value.has_missing ?
				(float*)&tmp_mds[1]->multidval.missing_value.value :
				NULL),
			(float*)(tmp_mds[2]->multidval.missing_value.has_missing ?
				(float*)&tmp_mds[2]->multidval.missing_value.value :
				NULL),
			&status,
			(float*)missing);
		if(status) {
			if(tmp_vars[3] != NULL) {
				missing1 = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
				*missing1 = *missing;
				tmp_miss_md3 = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)missing1, NULL, 1 , &tmp_dimsizes, TEMPORARY, NULL,(NclObjClass) tmp_mds[3]->multidval.type);
				_NclWriteAtt(tmp_vars[3],NCL_MISSING_VALUE_ATT,tmp_miss_md3,NULL);
				if(tmp_miss_md3->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)tmp_miss_md3);
				}
			} else {
				if(missing != NULL) {
					_NclResetMissingValue(tmp_mds[3],missing);
				}
			}
			if(tmp_vars[4] != NULL) {
				missing1 = (NclScalar*)NclMalloc((unsigned)sizeof(NclScalar));
				*missing1 = *missing;
				tmp_miss_md4 = (NclMultiDValData)_NclCreateVal( NULL, NULL, Ncl_MultiDValData, 0, (void*)missing1, NULL, 1 , &tmp_dimsizes, TEMPORARY, NULL, (NclObjClass)tmp_mds[4]->multidval.type);
				_NclWriteAtt(tmp_vars[4],NCL_MISSING_VALUE_ATT,tmp_miss_md4,NULL);
				if(tmp_miss_md4->obj.status != PERMANENT) {
					_NclDestroyObj((NclObj)tmp_miss_md4);
				}
			} else {
				if(missing != NULL) {
					_NclResetMissingValue(tmp_mds[4],missing);
				}
			}
		}
		return(NhlNOERROR);	
	} else {
		return(NhlFATAL);
	}
}

NhlErrorTypes _Nclsystemfunc
#if     NhlNeedProto
(void)
#else
()
#endif
{
        NclStackEntry val,data;
        NclMultiDValData tmp_md = NULL;
	char* command;
	int fildes[2],new_pipe_fd;
        NhlErrorTypes ret = NhlFATAL;
	int id;
	int tmp_id;
	int n;
	int status;
	int current_buf_size = 512;
	int current_qbuf_size = 512;
	char *buffer_ptr;
	char *buffer;
	NclQuark *qbuffer;
	NclQuark *qbuffer_ptr;
	long off;
	ng_size_t nelem = 0;


        val = _NclGetArg(0,1,DONT_CARE);
/*
* Should be constrained to be a SCALAR md
*/
        switch(val.kind) {
        case NclStk_VAL:
                tmp_md = val.u.data_obj;
                break;
        case NclStk_VAR:
                tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
                break;
        default:
                return(NhlFATAL);
        }
	if((tmp_md != NULL)&&(tmp_md->multidval.type->type_class.type & Ncl_Typestring)) {
		ret = pipe(fildes);
		errno = 0;
		id = fork();
		if (id < 0) {
			NhlPError(NhlFATAL,errno,"systemfunc: cannot create child process");
			return(NhlFATAL);
		}
		if(id == 0) {
			close(fildes[0]);
			close(fileno(stdout));
			new_pipe_fd = dup(fildes[1]);
			close(fildes[1]);
			command = NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val); 
                        /*
			 * Note: the child should use _exit() rather than exit() to avoid calling the
			 * registered atexit() functions prematurely
			 */
			if(!system(command)) {
				close(new_pipe_fd);
				_exit(0);
			} else {
				_exit(1);
			}
		} else {
			buffer = NclMalloc(current_buf_size);
			buffer_ptr = buffer;
			qbuffer = NclMalloc(current_qbuf_size*sizeof(NclQuark));
			qbuffer_ptr = qbuffer;
			signal(SIGPIPE,SIG_DFL);
			signal(SIGCHLD,SIG_DFL);
			close(fildes[1]);
			n = 0;
			nelem = 0;
			while(read(fildes[0],buffer_ptr,1) > 0) {
				if(*buffer_ptr == '\n') {
					*buffer_ptr++ = '\0';
					*qbuffer_ptr++ = NrmStringToQuark(buffer);
					buffer_ptr = buffer;
					nelem++;
					if((qbuffer_ptr - qbuffer) >= current_qbuf_size){
						off =  qbuffer_ptr - qbuffer;
						qbuffer = NhlRealloc(qbuffer,sizeof(NclQuark)*current_qbuf_size*2);
						qbuffer_ptr = qbuffer + off;
						current_qbuf_size *=2;
					}
				} else {
					buffer_ptr++;
				}
				if((buffer_ptr  - buffer) >= current_buf_size)  {
					off = buffer_ptr - buffer;
					buffer = NhlRealloc(buffer,current_buf_size*2);
					buffer_ptr = buffer + off;
					current_buf_size *=2;
				}
				n++;
			}
			while(( tmp_id = wait(&status)) != id);
			NclFree(buffer);
			close(fildes[0]);
		}
		if(nelem < 1) {
			NclQuark *mval = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark));
			ng_size_t dim_sizes = 1;
			*mval = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
			data.u.data_obj =_NclCreateMultiDVal(
				NULL,
				nclMultiDValDataClass,
				Ncl_MultiDValData,
				Ncl_MultiDValData,
				(void*)mval,
				&((NclTypeClass)nclTypestringClass)->type_class.default_mis,
				1,
				&dim_sizes,
				TEMPORARY,
				NULL,
				(NclTypeClass)nclTypestringClass);
			data.kind = NclStk_VAL;
			NclFree(qbuffer);
			_NclPlaceReturn(data);
		} else {
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)qbuffer,NULL,1,&nelem,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
			_NclPlaceReturn(data);
		}
		ret = NhlNOERROR;
	}

	return ret;
}

NhlErrorTypes _Nclstrlen
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclQuark* strs;
    int ndims;
    ng_size_t dimsz[NCL_MAX_DIMENSIONS];
    int has_missing,
    found_missing = 0;
    NclScalar   missing,
                ret_missing;
  
    ng_size_t i, sz = 1;
    int*    lens;
    

    strs = (NclQuark *) NclGetArgValue(
                        0,
                        1,
                        &ndims,
                        dimsz,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    for (i = 0; i < ndims; i++)
        sz *= dimsz[i];

    lens = NclMalloc((unsigned int) sizeof(int) * sz);
    if (lens == NULL) {
        NhlPError(NhlFATAL, errno, "strlen: memory allocation error.");
        return NhlFATAL;
    }

    if (has_missing) {
        for (i = 0; i < sz; i++) {
            if (strs[i] == missing.stringval) {
                lens[i] = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
                found_missing = 1;
            } else {
                lens[i] = strlen(NrmQuarkToString(strs[i]));
#if 0
                if (lens[i] > NCL_MAX_STRING)
                    NhlPError(NhlWARNING, NhlEUNKNOWN,
                        "strlen: string literals are limited to %d characters in length.",
                        NCL_MAX_STRING);
#endif
            }
        }
    } else {
        for (i = 0; i < sz; i++) {
            lens[i] = strlen(NrmQuarkToString(strs[i]));
#if 0
            if (lens[i] > NCL_MAX_STRING)
                NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "strlen: string literals are limited to %d characters in length.",
                    NCL_MAX_STRING);
#endif
        }
    }

    if (has_missing && found_missing) {
        ret_missing.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
        return NclReturnValue((void *) lens, ndims, dimsz, &ret_missing, NCL_int, 0);
    }
    else
        return NclReturnValue((void *) lens, ndims, dimsz, NULL, NCL_int, 0);

    NclFree(lens);
}


NhlErrorTypes _Nclsystem
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclStackEntry val;
    NclMultiDValData tmp_md = NULL;
    char* command;
    char *pager;
    char *no_sys_pager = NULL;

    val = _NclGetArg(0,1,DONT_CARE);

    /* Should be constrained to be a SCALAR md */
    switch(val.kind) {
    case NclStk_VAL:
        tmp_md = val.u.data_obj;
        break;

    case NclStk_VAR:
        tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
        break;

    default:
        return(NhlFATAL);
    }

    if ((tmp_md != NULL) && (tmp_md->multidval.type->type_class.type & Ncl_Typestring)) {
        if ((strlen(NrmQuarkToString(*(NclQuark *) tmp_md->multidval.val))) == 0) {
            NhlPError(NhlWARNING, NhlEUNKNOWN, "system: invalid argument (zero length)");
            return NhlWARNING;
        }
        no_sys_pager = getenv("NCL_NO_SYSTEM_PAGER");
        if (cmd_line == 1) {
            if ((no_sys_pager == NULL) && (!NCLnoSysPager)) {
                pager = getenv("PAGER");
                if (pager == NULL) {
                    command = NclMalloc(strlen(NrmQuarkToString(
                        *(NclQuark *)tmp_md->multidval.val)) + strlen(" | more") + 1);
                    strcpy(command, NrmQuarkToString(*(NclQuark *) tmp_md->multidval.val));
                    strcat(command, " | more");
                } else {
                    command = NclMalloc(strlen(NrmQuarkToString(
                        *(NclQuark *) tmp_md->multidval.val)) + strlen(" | ") + strlen(pager) + 1);
                    strcpy(command, NrmQuarkToString(*(NclQuark *) tmp_md->multidval.val));
                    strcat(command, " | ");
                    strcat(command, pager);
                }
            } else {
                /* user does not want to use a PAGER */
                command = NclMalloc(strlen(NrmQuarkToString(*(NclQuark *)tmp_md->multidval.val)));
                strcpy(command, NrmQuarkToString(*(NclQuark *) tmp_md->multidval.val));
            }
        
            if (!system(command)) {
                NhlFree(command);
                return(NhlNOERROR);
            } else {
                NhlFree(command);
                return(NhlWARNING);
            }
        } else {
            command = NrmQuarkToString(*(NclQuark *) tmp_md->multidval.val); 
            if (!system(command)) {
                return(NhlNOERROR);
            } else {
                return(NhlWARNING);
            }
        }
    } else {
        return(NhlFATAL);
    }
}

NhlErrorTypes _NclIIsMissing
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry val,data;
	NclMultiDValData tmp_md = NULL;
	logical *lval;
	ng_size_t i;
	
	val = _NclGetArg(0,1,DONT_CARE);
/*
* Should be constrained to be a SCALAR md
*/	
	switch(val.kind) {
	case NclStk_VAL:
		tmp_md = val.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}

	if(tmp_md != NULL) {
		lval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_md->multidval.totalelements);
		if(tmp_md->multidval.missing_value.has_missing) {

			_Ncleq(tmp_md->multidval.type,lval,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
				
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)lval,NULL,tmp_md->multidval.n_dims,tmp_md->multidval.dim_sizes,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data);
		} else {
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				lval[i] = 0;
			}
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)lval,NULL,tmp_md->multidval.n_dims,tmp_md->multidval.dim_sizes,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data);
		}
	}
	return(NhlNOERROR);
}


NhlErrorTypes _NclIAddToOverlayAfter
#if	NhlNeedProto
(void)
#else
()
#endif
{	
	NclStackEntry base;
	NclStackEntry over;
	NclStackEntry after;
	int baseid;
	int overid;
	int afterid;
	NclMultiDValData tmp_md = NULL;
	NclHLUObj base_hl = NULL;
	NclHLUObj over_hl = NULL;
	NclHLUObj after_hl = NULL;
	

	
	base =  _NclGetArg(0,3,DONT_CARE);
	over =  _NclGetArg(1,3,DONT_CARE);
	after =  _NclGetArg(2,3,DONT_CARE);

	switch(base.kind) {
	case NclStk_VAL:
		baseid = *(int*)base.u.data_obj->multidval.val;
		base_hl = (NclHLUObj)_NclGetObj(baseid);
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(base.u.data_var,NULL,NULL);
		baseid = *(int*)tmp_md->multidval.val;
		base_hl = (NclHLUObj)_NclGetObj(baseid);
		break;
	default:
		return(NhlFATAL);
	}
	switch(over.kind) {
	case NclStk_VAL:
		overid = *(int*)over.u.data_obj->multidval.val;
		over_hl = (NclHLUObj)_NclGetObj(overid);
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(over.u.data_var,NULL,NULL);
		overid = *(int*)tmp_md->multidval.val;
		over_hl = (NclHLUObj)_NclGetObj(overid);
		break;
	default:
		return(NhlFATAL);
	}
	switch(after.kind) {
	case NclStk_VAL:
		afterid = *(int*)after.u.data_obj->multidval.val;
		after_hl = (NclHLUObj)_NclGetObj(afterid);
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(after.u.data_var,NULL,NULL);
		afterid = *(int*)tmp_md->multidval.val;
		after_hl = (NclHLUObj)_NclGetObj(afterid);
		break;
	default:
		return(NhlFATAL);
	}
	NhlAddOverlay(base_hl->hlu.hlu_id,over_hl->hlu.hlu_id,after_hl->hlu.hlu_id);
	_NclAddHLUToExpList(base_hl,over_hl->obj.id);
	return(NhlNOERROR);
}
NhlErrorTypes _NclIAddToOverlay
#if	NhlNeedProto
(void)
#else
()
#endif
{	
	NclStackEntry base;
	NclStackEntry over;
	int baseid;
	int overid;
	NclMultiDValData tmp_md = NULL;
	NclHLUObj base_hl = NULL;
	NclHLUObj over_hl = NULL;
	

	
	base =  _NclGetArg(0,2,DONT_CARE);
	over =  _NclGetArg(1,2,DONT_CARE);

	switch(base.kind) {
	case NclStk_VAL:
		if(base.u.data_obj->multidval.missing_value.has_missing) {
			baseid = *(int*)base.u.data_obj->multidval.val;
			if(baseid == base.u.data_obj->multidval.missing_value.value.objval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"overlay: missing value as input, can't continue");
				return(NhlFATAL);
			}
			base_hl = (NclHLUObj)_NclGetObj(baseid);
		} else {
			baseid = *(int*)base.u.data_obj->multidval.val;
			base_hl = (NclHLUObj)_NclGetObj(baseid);
		}
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(base.u.data_var,NULL,NULL);
		if(tmp_md->multidval.missing_value.has_missing) {
			baseid = *(int*)tmp_md->multidval.val;
			if(baseid== tmp_md->multidval.missing_value.value.objval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"overlay: missing value as input, can't continue");
				return(NhlFATAL);
			}
			base_hl = (NclHLUObj)_NclGetObj(baseid);
		} else {
			baseid = *(int*)tmp_md->multidval.val;
			base_hl = (NclHLUObj)_NclGetObj(baseid);
		}
		break;
	default:
		return(NhlFATAL);
	}
	switch(over.kind) {
	case NclStk_VAL:
		if(over.u.data_obj->multidval.missing_value.has_missing) {
			overid = *(int*)over.u.data_obj->multidval.val;
			if(overid == over.u.data_obj->multidval.missing_value.value.objval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"overlay: missing value as input, can't continue");
				return(NhlFATAL);
			}
			over_hl = (NclHLUObj)_NclGetObj(overid);
		} else {
			overid = *(int*)over.u.data_obj->multidval.val;
			over_hl = (NclHLUObj)_NclGetObj(overid);
		}
		over_hl = (NclHLUObj)_NclGetObj(overid);
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(over.u.data_var,NULL,NULL);
		if(tmp_md->multidval.missing_value.has_missing) {
			overid = *(int*)tmp_md->multidval.val;
			if(overid== tmp_md->multidval.missing_value.value.objval) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"overlay: missing value as input, can't continue");
				return(NhlFATAL);
			} 
			over_hl = (NclHLUObj)_NclGetObj(overid);
		} else {
			overid = *(int*)tmp_md->multidval.val;
			over_hl = (NclHLUObj)_NclGetObj(overid);
		}
		break;
	default:
		return(NhlFATAL);
	}
	if((base_hl != NULL) &&(_NhlGetLayer(base_hl->hlu.hlu_id) != NULL)&&(over_hl != NULL)&&(_NhlGetLayer(over_hl->hlu.hlu_id)!= NULL)) {
		NhlAddOverlay(base_hl->hlu.hlu_id,over_hl->hlu.hlu_id,-1);
		_NclAddHLUToExpList(base_hl,over_hl->obj.id);
		return(NhlNOERROR);
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"overlay: bad HLU id passed in, ignoring");
	}
	return NhlFATAL;
}
NhlErrorTypes _NclIAddFile
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry path;
	NclStackEntry rw_status;
	NclStackEntry out_data;
	NclMultiDValData p_md = NULL;
	NclMultiDValData rw_md = NULL;
	NclFile file = NULL;
	NclMultiDValData out_md = NULL;
	char *rw;
	int rw_v;
	int *id = (int*)NclMalloc((unsigned)sizeof(int));
	ng_size_t dim_size = 1;
	obj *tmp_obj = NULL; 
/*
* Guarenteed to be scalar string
*/
	path =  _NclGetArg(0,2,DONT_CARE);
	rw_status = _NclGetArg(1,2,DONT_CARE);

	if(path.kind == NclStk_VAR) {
		if(path.u.data_var != NULL) {
			p_md = _NclVarValueRead(path.u.data_var,NULL,NULL);
		}
	} else if(path.kind == NclStk_VAL) {
		p_md = path.u.data_obj;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"addfile: incorrect type of object passed to addfile");
		NclFree(id);
		return(NhlFATAL);
	}
	if(rw_status.kind == NclStk_VAR) {
		if(rw_status.u.data_var != NULL) {
			rw_md = _NclVarValueRead(rw_status.u.data_var,NULL,NULL);
		}
	} else if(rw_status.kind == NclStk_VAL) {
		rw_md = rw_status.u.data_obj;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"addfile: incorrect type of object passed to addfile");
		NclFree(id);
		return(NhlFATAL);
	}
	rw = NrmQuarkToString(*(NclQuark*)rw_md->multidval.val);
	if((strrchr(rw,'c') != NULL)||(strrchr(rw,'C') != NULL)) {
		rw_v = -1;
	} else if((strrchr(rw,'w') == NULL)&&(strrchr(rw,'W') == NULL)) {
		rw_v = 1;
	} else {
		rw_v = 0;
	}
	file = _NclOpenFile(NULL,NULL,Ncl_File,0,TEMPORARY,*(NclQuark*)p_md->multidval.val,rw_v);
	if(file != NULL) {
		*id = file->obj.id;
		out_md = _NclMultiDValnclfileDataCreate(NULL,NULL,Ncl_MultiDValnclfileData,0,id,NULL,1,&dim_size,TEMPORARY,NULL);
		if(out_md != NULL) {
			out_data.kind = NclStk_VAL;
			out_data.u.data_obj = out_md;
			_NclPlaceReturn(out_data);
			return(NhlNOERROR);
		} else {
			NclFree(id);
			_NclDestroyObj((NclObj)file);
			return(NhlFATAL);
		}
	} else {
		tmp_obj =(obj*) NclMalloc(((NclTypeClass)nclTypeobjClass)->type_class.size);
		*tmp_obj = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
		out_md = _NclMultiDValnclfileDataCreate(
				NULL,
				NULL,
				Ncl_MultiDValnclfileData,
				0,
				(void*)tmp_obj,
				(void*)&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,
				1,
				&dim_size,
				TEMPORARY,
				NULL);
		if(out_md != NULL) {
			out_data.kind = NclStk_VAL;
			out_data.u.data_obj = out_md;
			_NclPlaceReturn(out_data);
			NclFree(id);
			return(NhlWARNING);
		} else {
			NclFree(id);
			_NclDestroyObj((NclObj)file);
			return(NhlFATAL);
		}
	}
}

NhlErrorTypes _NclIAny
#if	NhlNeedProto
(void)
#else
()
#endif
{
/*
* Guarenteed to be a logical
*/
	NclStackEntry data;	
	NclStackEntry data_out;	
	NclMultiDValData tmp_md = NULL;
	ng_size_t dim_size = 1;
	ng_size_t i;
	logical *tmp_val;
	data = _NclGetArg(0,1,DONT_CARE);
	if(data.kind == NclStk_VAR) {
		if(data.u.data_var != NULL) {	
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
	} else {
		NhlPError(NhlWARNING, NhlEUNKNOWN,"any: incorrect type of object passed to any");
		return(NhlWARNING);
	}
	if(tmp_md == NULL) {
		data_out.kind = NclStk_NOVAL;
		data_out.u.data_obj = NULL;
		_NclPlaceReturn(data_out);
		return(NhlFATAL);
	}

	if(tmp_md->multidval.kind == SCALAR) {
		_NclPlaceReturn(data);
	} else if(!tmp_md->multidval.missing_value.has_missing) {
		tmp_val = (logical*)tmp_md->multidval.val;
		i = 0;
		while(!(*tmp_val)) {
			tmp_val++;
			i++;
			if(i == tmp_md->multidval.totalelements) {
				tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
				*tmp_val = 0;
				data_out.kind = NclStk_VAL;

				data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
				_NclPlaceReturn(data_out);
				return(NhlNOERROR);
			}
		}
		tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
		*tmp_val = 1;
		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
		_NclPlaceReturn(data_out);
	} else {
		tmp_val = (logical*)tmp_md->multidval.val;
		i = 0;
		while(i<tmp_md->multidval.totalelements) {
			if((*tmp_val != tmp_md->multidval.missing_value.value.logicalval) &&(*tmp_val) ) {
				break;
			}
			tmp_val++;
			i++;
		}
		if(i >= tmp_md->multidval.totalelements) {
			tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
			*tmp_val = 0;
			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data_out);
		} else {
			tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
			*tmp_val = 1;
			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data_out);
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIAll
#if	NhlNeedProto
(void)
#else
()
#endif
{
/*
* Guarenteed to be a logical
*/
	NclStackEntry data;	
	NclStackEntry data_out;	
	NclMultiDValData tmp_md = NULL;
	ng_size_t dim_size = 1;
	ng_size_t i;
	logical *tmp_val;
	data = _NclGetArg(0,1,DONT_CARE);
	if(data.kind == NclStk_VAR) {
		if(data.u.data_var != NULL) {	
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
	} else {
		NhlPError(NhlWARNING, NhlEUNKNOWN,"any: incorrect type of object passed to any");
		return(NhlWARNING);
	}
	if(tmp_md == NULL) {
		data_out.kind = NclStk_NOVAL;
		data_out.u.data_obj = NULL;
		_NclPlaceReturn(data_out);
		return(NhlFATAL);
	}

	if(tmp_md->multidval.kind == SCALAR) {
		_NclPlaceReturn(data);
	} else if(!tmp_md->multidval.missing_value.has_missing) {
		tmp_val = (logical*)tmp_md->multidval.val;
		i = 0;
		while((*tmp_val)) {
			tmp_val++;
			i++;
			if(i >= tmp_md->multidval.totalelements) {
				tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
				*tmp_val = 1;
				data_out.kind = NclStk_VAL;
				data_out.u.data_obj =_NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
				_NclPlaceReturn(data_out);
				return(NhlNOERROR);
			}
		}
		tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
		*tmp_val = 0;
		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
		_NclPlaceReturn(data_out);
		return(NhlNOERROR);
	} else {
		tmp_val = (logical*)tmp_md->multidval.val;
		i = 0;
		while(i<tmp_md->multidval.totalelements) {
			if((*tmp_val != tmp_md->multidval.missing_value.value.logicalval) &&!(*tmp_val) ) {
				break;
			}
			tmp_val++;
			i++;
		}
		if(i >= tmp_md->multidval.totalelements) {
			tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
			*tmp_val = 1;
			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data_out);
			return(NhlNOERROR);
		} else {
			tmp_val = (logical*)NclMalloc((unsigned)_NclSizeOf(NCL_logical));
			*tmp_val = 0;
			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp_val,NULL,1,&dim_size,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data_out);
			return(NhlNOERROR);
		}
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclISizeOf
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;	
	NclStackEntry data_out;	
	NclMultiDValData tmp_md = NULL;
	long lsize;
	void *size;
	ng_size_t dim_size = 1;
	logical return_int;

	data = _NclGetArg(0,1,DONT_CARE);
	if(data.kind == NclStk_VAR) {
		if(data.u.data_var != NULL) {	
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
	} else {
		NhlPError(NhlWARNING, NhlEUNKNOWN,"sizeof: incorrect type of object passed to sizeof");
		return(NhlWARNING);
	}
	if(tmp_md != NULL) {
		data_out.kind = NclStk_VAL;
		return_int = True;
		lsize = (long)_NclSizeOf(tmp_md->multidval.data_type)*tmp_md->multidval.totalelements;
#if !defined(NG32BIT)
		if(lsize > INT_MAX) {
		  return_int = False;
		}
#endif
		if(return_int) {
		  size          = (void *) NclMalloc(sizeof(int));
		  *((int*)size) = (int) lsize;
		  data_out.u.data_obj = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			(void*)size,
			NULL,
			1,
			&dim_size,
			TEMPORARY,
			NULL,
			(NclTypeClass)nclTypeintClass
			);
		}
		else {
		  size           = (void *) NclMalloc(sizeof(long));
		  *((long*)size) = lsize;
		  data_out.u.data_obj = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			(void*)size,
			NULL,
			1,
			&dim_size,
			TEMPORARY,
			NULL,
			(NclTypeClass)nclTypelongClass
			);
		}
		if(data_out.u.data_obj != NULL) {
			_NclPlaceReturn(data_out);
			return(NhlNOERROR);
		} else {
			return(NhlFATAL);
		}
	} else {
		return(NhlFATAL);
	}
}

NhlErrorTypes _NclIDimSizes
#if	NhlNeedProto
(void)
#else
()
#endif
{
    NclStackEntry data;	
    NclStackEntry data_out;	
    NclMultiDValData tmp_md = NULL;
    ng_size_t dim_size, product_size;
    void *size;
    int i = 0;
    logical return_int;

    data = _NclGetArg(0,1,DONT_CARE);
    if (data.kind == NclStk_VAR) {
        if (data.u.data_var != NULL) {	
            tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
        }
    } else if (data.kind == NclStk_VAL) {
        tmp_md = data.u.data_obj;
    } else {
        NhlPError(NhlWARNING, NhlEUNKNOWN,"dimsizes: incorrect object type.");
        return(NhlWARNING);
    }

    if (tmp_md != NULL) {
        data_out.kind = NclStk_VAL;

/*
 * Until we have a type like "nclTypengsizetClass", we need to
 * specifically return ints or longs.
 *
 * Assume a return type of int unless one of the criteria 
 * for returning longs is met.
 *
 * The rules for when to return an int versus a long:
 *    - On a 32-bit system, return ints.
 *    - On a 64-bit system, return longs if any of the
 *      individual dimension sizes are > INT_MAX, or
 *      if the product of the dimension sizes is > INT_MAX.
 */
        dim_size   = tmp_md->multidval.n_dims;
	return_int = True;
#if !defined(NG32BIT)
	i = 0;
	product_size = 1;
        while(i < dim_size && return_int) {
	  product_size *= tmp_md->multidval.dim_sizes[i];
	  if(tmp_md->multidval.dim_sizes[i] > INT_MAX ||
	     product_size > INT_MAX) {
	    return_int = False;
	  }
	  i++;
	}
#endif
	if(return_int) {
	  size = (void *) NclMalloc(sizeof(int) * dim_size);
	  if(size == NULL) {
	    NhlPError(NhlFATAL, NhlEUNKNOWN,"dimsizes: cannot allocate memory for output");
	    return(NhlFATAL);
	  }
	  for (i = 0; i < dim_size; i++) {
	    ((int*)size)[i] = (int)tmp_md->multidval.dim_sizes[i];
	  }
	  data_out.u.data_obj = _NclCreateMultiDVal(
  	        NULL,
	        NULL,
	        Ncl_MultiDValData,
	        0,
	        size,
	        NULL,
	        1,
	        &dim_size,
	        TEMPORARY,
	        NULL,
	        (NclTypeClass) nclTypeintClass
	        );
	}
	else {
	  size = (void *) NclMalloc(sizeof(long) * dim_size);
	  if(size == NULL) {
	    NhlPError(NhlFATAL, NhlEUNKNOWN,"dimsizes: cannot allocate memory for output");
	    return(NhlFATAL);
	  }
	  for (i = 0; i < tmp_md->multidval.n_dims; i++) {
	    ((long*)size)[i] = (long)tmp_md->multidval.dim_sizes[i];
	  }
	  data_out.u.data_obj = _NclCreateMultiDVal(
  	        NULL,
                NULL,
                Ncl_MultiDValData,
                0,
                size,
                NULL,
                1,
                &dim_size,
                TEMPORARY,
                NULL,
                (NclTypeClass) nclTypelongClass
            );
        }
        if (data_out.u.data_obj != NULL ) {
            _NclPlaceReturn(data_out);
            return(NhlNOERROR);
        } else {
            return(NhlFATAL);
        }
    } else {
        return(NhlFATAL);
    }
}

NhlErrorTypes _NclIDumpStk
#if	NhlNeedProto
(void)
#else
()
#endif
{
	FILE *fp = NULL;
	NclMultiDValData tmp_md,tmp1_md;
	NclStackEntry data;
	char *fname = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	data = _NclGetArg(0,1,DONT_CARE);
	if(data.kind == NclStk_VAR) {
		if(data.u.data_var != NULL) {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
	} else {
		NhlPError(NhlWARNING, NhlEUNKNOWN,"dump: incorrect type of object, defaulting to stdout");
		fp =  _NclGetOutputStream();
		ret = NhlWARNING;
	}
	if(tmp_md->multidval.type->type_class.type & Ncl_Typestring) {
		if(tmp_md->multidval.kind != SCALAR) {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"dump: multiple file names passed to dump, using the first one");
			ret = NhlWARNING;
		}
		fname = NrmQuarkToString(*(int*)tmp_md->multidval.val);
	} else {
		tmp1_md = _NclCoerceData(tmp_md,Ncl_Typestring,NULL);
		if(tmp1_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dump: Unable to convert parameter to string representation for output filename");
			fp = NULL;
			return(NhlFATAL);
		} else {
			if(tmp_md->obj.status != PERMANENT) {
				_NclDestroyObj((NclObj)tmp_md);
			}
			tmp_md = tmp1_md;
			if(tmp_md->multidval.kind != SCALAR) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"dump: multiple file names passed to dump, using the first one");
				ret = NhlWARNING;
			}
			fname = NrmQuarkToString(*(int*)tmp_md->multidval.val); 
		}
	}
	if((fname != NULL)&&(strcmp(fname,"stdout"))) {
		fp = fopen(fname,"a");
	} else {
		fp = _NclGetOutputStream();
	}
	if(fp != NULL) {
		_NclDumpStack(fp,6);
		if(fp != stdout)
			fclose(fp);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"dump: Unable to open output stream or file");
		return(NhlFATAL);
	}
	return(ret);
}

NhlErrorTypes _NclIFrame
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md;
	NhlErrorTypes ret = NhlNOERROR;
	NclHLUObj hlu_ptr;
	int *obj_ids;
	ng_size_t i;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar) ) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"frame: non-object input; ignoring request");
			return(NhlFATAL);
		} else {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK) {
				obj_ids = (int*)tmp_md->multidval.val;
				for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
					hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
					if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
						ret = NhlFrame(hlu_ptr->hlu.hlu_id);
					}
				}
			}
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
		if(data.u.data_obj->obj.obj_type_mask & NCL_HLU_MASK) {
			obj_ids = (int*)tmp_md->multidval.val;
			for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
				hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
				if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
					ret = NhlFrame(hlu_ptr->hlu.hlu_id);
				}
			}
		}
	}
	return(ret);
}
NhlErrorTypes _NclIClear
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md;
	NclHLUObj hlu_ptr;
	int *obj_ids;
	ng_size_t i;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar) ) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"clear: non-object input; ignoring request");
			return(NhlFATAL);
		} else {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK) {
				obj_ids = (int*)tmp_md->multidval.val;
				for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
					hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
					if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
						NhlClearWorkstation(hlu_ptr->hlu.hlu_id);
					}
				}
			}
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
		if(data.u.data_obj->obj.obj_type_mask & NCL_HLU_MASK) {
			obj_ids = (int*)tmp_md->multidval.val;
			for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
				hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
				if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
					NhlClearWorkstation(hlu_ptr->hlu.hlu_id);
				}
			}
		}
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclIDestroy
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md;
	int *obj_ids;
	ng_size_t  i;

	data = _NclGetArg(0,1,WRITE_IT);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"destroy: non-object input; ignoring request");
	
			return(NhlFATAL);
		} else {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
	} else {
		return(NhlFATAL);
	}
	if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK) {
		obj_ids = (obj*)tmp_md->multidval.val;
		for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
			if((!tmp_md->multidval.missing_value.has_missing)||(obj_ids[i] != tmp_md->multidval.missing_value.value.objval)){
				_NclDestroyObj(_NclGetObj(obj_ids[i]));
			}
		}
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIUpdate
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md;
	NclHLUObj hlu_ptr;
	int *obj_ids;
	ng_size_t  i;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"update: non-object input; ignoring request");
	
			return(NhlFATAL);
		} else {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK) {
				obj_ids = (int*)tmp_md->multidval.val;
				for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
					hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
					if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
						NhlUpdateWorkstation(hlu_ptr->hlu.hlu_id);
					}
				}
			}
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
		if(data.u.data_obj->obj.obj_type_mask & NCL_HLU_MASK) {
			obj_ids = (int*)tmp_md->multidval.val;
			for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
				hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);

				if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
					NhlUpdateWorkstation(hlu_ptr->hlu.hlu_id);
				}
			}
		}
	} else {
		return(NhlFATAL);
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclIDraw
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md;
	int *obj_ids;
	ng_size_t  i;
	NclHLUObj hlu_ptr;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"draw: non-object input; ignoring request");
			return(NhlFATAL);
		} else {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			if(tmp_md->obj.obj_type_mask & NCL_HLU_MASK) {
				obj_ids = (int*)tmp_md->multidval.val;
				for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
					hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
					if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
						NhlDraw(hlu_ptr->hlu.hlu_id);
					}
				}
			}
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
		if(data.u.data_obj->obj.obj_type_mask & NCL_HLU_MASK) {
			obj_ids = (int*)tmp_md->multidval.val;
			for(i = 0; i < tmp_md->multidval.totalelements; i++ ) {
				hlu_ptr = (NclHLUObj)_NclGetObj(obj_ids[i]);
				if((hlu_ptr != NULL)&&(hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
					NhlDraw(hlu_ptr->hlu.hlu_id);
				}
			}
		}
	} else {
		return(NhlFATAL);
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclIPrint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	FILE *fp;
	NhlErrorTypes ret = NhlNOERROR;
	

	data = _NclGetArg(0,1,DONT_CARE);
	if(cmd_line == 1) {
		_NclStartCmdLinePager();
	}
	fp = _NclGetOutputStream();

	switch(data.kind) {
	case NclStk_VAL:
		ret = _NclPrint((NclObj)data.u.data_obj,fp);
		break;
	case NclStk_VAR:
		ret = _NclPrint((NclObj)data.u.data_var,fp);
		break;
	default:
		ret = NhlNOERROR;
		break;
	}
	if((cmd_line == 1)&&!(ret < NhlINFO)) {
		_NclEndCmdLinePager();
	}
	return(ret);
}

NhlErrorTypes _NclIDelete
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclStackEntry* var;
	NclSymbol *thesym;
	int sub_sel = 0;
	NclObj tmp,pobj;
	NclRefList *rlist = NULL;

	data = _NclGetArg(0,1,DONT_CARE);

	switch(data.kind) {
	case NclStk_VAL:
		if (data.u.data_obj->obj.obj_type == Ncl_MultiDVallistData) {
			NclObj list_obj = _NclGetObj(*(int*) data.u.data_obj->multidval.val);
			while ((tmp = _NclListPop(list_obj))) {
				if (tmp->obj.obj_type == Ncl_Var || tmp->obj.obj_type == Ncl_HLUVar
				    || tmp->obj.obj_type == Ncl_CoordVar || tmp->obj.obj_type == Ncl_FileVar) {
					switch(((NclVar)tmp)->var.var_type) {
					case VARSUBSEL:
					case COORDSUBSEL:
					case FILEVARSUBSEL:
						sub_sel = 1;
						break;
					case PARAM:
						NhlPError(NhlFATAL,NhlEUNKNOWN,"Deletion of parameters to functions and procedures is not allowed in NCL");
						return(NhlFATAL);
					case NORMAL:
					case COORD:
					case FILEVAR: 
					case RETURNVAR:
					case HLUOBJ :
					default:
						sub_sel = 0;
						break;
					}
					if(((NclVar)tmp)->var.thesym != NULL && !sub_sel) {
						var = _NclRetrieveRec(((NclVar)tmp)->var.thesym,DONT_CARE);
						thesym = ((NclVar)tmp)->var.thesym;
						if(((NclVar)tmp)->var.var_type == NORMAL) {
                                                       /*
							* Can't destroy symbol since it may be referenced from the instruction
							* sequence. Changing it to UNDEF should do the trick though
							*/
							_NclChangeSymbolType(thesym,UNDEF);
						}
						_NclDestroyObj((NclObj)tmp);
						if(var != NULL) {
							var->u.data_var = NULL;
							var->kind = NclStk_NOVAL;
						}
					}
					else if (!sub_sel) {
						_NclDestroyObj((NclObj)tmp);
					}
				}
				else if (tmp->obj.obj_type == Ncl_MultiDValData) {
					_NclDestroyObj((NclObj)tmp);
				}
			}
		}
		_NclDestroyObj((NclObj)data.u.data_obj);
		break;
	case NclStk_VAR:
		if(data.u.data_var != NULL) {
			switch(data.u.data_var->var.var_type) {
				case VARSUBSEL:
				case COORDSUBSEL:
				case FILEVARSUBSEL:
					sub_sel = 1;
					break;
				case PARAM:
					NhlPError(NhlFATAL,NhlEUNKNOWN,"Deletion of parameters to functions and procedures is not allowed in NCL");
					return(NhlFATAL);
				case NORMAL:
				case COORD:
				case FILEVAR: 
				case RETURNVAR:
				case HLUOBJ :
				default:
					sub_sel = 0;
					break;
			}

		}
		if((data.u.data_var != NULL)&&(data.u.data_var->var.thesym != NULL)&&(!sub_sel)) {
			var = _NclRetrieveRec(data.u.data_var->var.thesym,DONT_CARE);
			thesym = data.u.data_var->var.thesym;
			tmp = (NclObj)data.u.data_var;
			if(data.u.data_var->var.var_type == NORMAL) {
/*
* Can't destroy symbol since it may be referenced from the instruction
* sequence. Changing it to UNDEF should do the trick though
*/
				_NclChangeSymbolType(thesym,UNDEF);
				data.kind = NclStk_NOVAL;
				data.u.data_obj = NULL;
				_NclPutRec(thesym,&data);
			}
			_NclDestroyObj((NclObj)tmp);
			if(var != NULL) {
				var->u.data_var = NULL;
				var->kind = NclStk_NOVAL;
			}
		} else {
			if((data.u.data_obj->obj.ref_count != 0)&&(!sub_sel)) {
				int id = data.u.data_obj->obj.id;
				switch(data.u.data_obj->obj.obj_type) {
				case Ncl_CoordVar:
					rlist = data.u.data_obj->obj.parents;
					while(rlist != NULL) {
						pobj = _NclGetObj(rlist->pid);
						if(pobj->obj.obj_type == Ncl_Var) {
							_NclDeleteCoordVar((NclVar)pobj,NrmQuarkToString(data.u.data_var->var.var_quark));
						} else {
							_NclDelParent((NclObj)data.u.data_obj,(NclObj)pobj);
						}
						if (_NclGetObj(id) != NULL)
							rlist = data.u.data_obj->obj.parents;
						else
							rlist = NULL;
					}
					break;
				default:
					rlist = data.u.data_obj->obj.parents;
					while(rlist != NULL && _NclGetObj(id) != NULL) {
						pobj = _NclGetObj(rlist->pid);
						rlist = rlist->next;
						_NclDelParent((NclObj)data.u.data_obj,(NclObj)pobj);
					}
					break;
				}
			} else {
				var = NULL;
				tmp = (NclObj)data.u.data_var;
				_NclDestroyObj((NclObj)tmp);
				if(var != NULL) {
					var->u.data_var = NULL;
					var->kind = NclStk_NOVAL;
				}
			}
		}
		break;
	default:
		break;
	}
	data.kind = NclStk_NOVAL;
	data.u.data_obj = NULL;
	return(_NclPutArg(data,0,1));
}














NhlErrorTypes _Nclidsfft
#if	NhlNeedProto
(void)
#else
()
#endif
{
	float *arg[3];
	ng_size_t i;
	void *tmp_dims;
	ng_size_t *dims;
	ng_size_t  dimsizes,dimsizes1,dimsizes2;
	int has_missing,has_missing1,has_missing2;
	NclScalar missing,missing1,missing2;
	NclBasicDataTypes type0,type1,type2,type3;
	ng_size_t m,n;
	float *tmp;
	float *x_coord;
	float *y_coord;
	int *iwrk;
	float *fwrk;
	NclMultiDValData tmp_md;
	NclVar	tmp_var;
	NclMultiDValData x_coord_md;
	NclVar x_coord_var;
	NclMultiDValData y_coord_md;
	NclVar y_coord_var;
	int ids[2];
	NclDimRec dim_info[2];
	NclStackEntry data;
	float spacing,xmax,xmin,ymax,ymin;


	arg[0] = (float*)NclGetArgValue( 0, 4, NULL, &dimsizes, &missing, &has_missing, &type0,DONT_CARE);
	arg[1] = (float*)NclGetArgValue( 1, 4, NULL, &dimsizes1, &missing1, &has_missing1, &type1,DONT_CARE);
	arg[2] = (float*)NclGetArgValue( 2, 4, NULL, &dimsizes2, &missing2, &has_missing2, &type2,DONT_CARE);
	tmp_dims = (void*)NclGetArgValue( 3, 4, NULL, NULL, NULL, &has_missing, &type3,DONT_CARE);
	dims = get_dimensions(tmp_dims,2,type3,"idsfft");
	if(dims == NULL) 
	  return(NhlFATAL);

	if((dimsizes == dimsizes1)&&(dimsizes = dimsizes2)){
		xmax = (arg[0])[0];
		xmin = (arg[0])[0];
		ymax = (arg[1])[0];
		ymin = (arg[1])[0];
		for(i = 0; i < dimsizes; i++) {
			if(has_missing) {
				if((arg[0])[i] == missing.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"idsfft: input contains missing values, cannot continue");
					return(NhlFATAL);
				} else if((arg[0])[i] > xmax) {
					xmax = (arg[0])[i];
				} else if((arg[0])[i] < xmin) {
					xmin = (arg[0])[i];
				}
			} else {
				if((arg[0])[i] > xmax) {
                                        xmax = (arg[0])[i];
                                } else if((arg[0])[i] < xmin) {
                                        xmin = (arg[0])[i];
                                }
			}
			if(has_missing1) {
				if((arg[1])[i] == missing1.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"idsfft: input contains missing values, cannot continue");
					return(NhlFATAL);
				} else if((arg[1])[i] > ymax) {
					ymax = (arg[1])[i];
				} else if((arg[1])[i] < ymin) {
					ymin = (arg[1])[i];
				}
			} else {
				if((arg[1])[i] > ymax) {
                                        ymax = (arg[1])[i];
                                } else if((arg[1])[i] < ymin) {
                                        ymin = (arg[1])[i];
                                }
			}
			if(has_missing2) {
				if((arg[2])[i] == missing2.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"idsfft: input contains missing values, cannot continue");
					return(NhlFATAL);
				}
			}
		}
		m = dims[0];
		n = dims[1];
		tmp = (float*)NclMalloc(m * n * sizeof(float));
		for(i = 0; i < m*n; i++) tmp[i] = 0.0;
		x_coord = (float*)NclMalloc(m * sizeof(float));
		spacing = (xmax - xmin)/(m-1);
		for(i = 0; i < m; i++) {
			x_coord[i] = xmin + i * spacing;
		}
		y_coord = (float*)NclMalloc(n * sizeof(float));
		spacing = (ymax - ymin)/(n-1);
		for(i = 0; i < n; i++) {
			y_coord[i] = ymin + i * spacing;
		}
		iwrk = (int *)NclMalloc((31 * dimsizes + m * n)*sizeof(int));
		fwrk = (float*)NclMalloc(6*dimsizes*sizeof(float));
		c_idsfft(1,dimsizes,arg[1],arg[0],arg[2],n,m,n,y_coord,x_coord,tmp,iwrk,fwrk);
		NclFree(iwrk);
		NclFree(fwrk);
		dim_info[0].dim_quark = NrmStringToQuark("ncl0");
		dim_info[0].dim_num= 0 ; 
		dim_info[0].dim_size = m ; 
		dim_info[1].dim_quark = NrmStringToQuark("ncl1");
		dim_info[1].dim_size = n ; 
		dim_info[1].dim_num= 1 ; 

		tmp_md = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,tmp,NULL,2,dims,TEMPORARY,NULL,(NclTypeClass)nclTypefloatClass);
		x_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,x_coord,NULL,1,&(dims[0]),TEMPORARY,NULL,(NclObjClass)nclTypefloatClass);
		y_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,y_coord,NULL,1,&(dims[1]),TEMPORARY,NULL,(NclObjClass)nclTypefloatClass);

		x_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,x_coord_md,&(dim_info[0]),-1,NULL,COORD,"x",TEMPORARY);
		y_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,y_coord_md,&(dim_info[1]),-1,NULL,COORD,"y",TEMPORARY);
		ids[0] = x_coord_var->obj.id;
		ids[1] = y_coord_var->obj.id;
		tmp_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);
		data.kind = NclStk_VAR;
		data.u.data_var = tmp_var;
		NclFree(dims);
		_NclPlaceReturn(data);
		return(NhlNOERROR);
	} else {
/*
* Place bogus data in return
*/
		return(NhlFATAL);
	}
}

static NclTypeClass qc_nc = NULL;
static NclScalar* qc_missing = NULL;
static void * qc_val = NULL;

static int qsort_compare_func
#if	NhlNeedProto
(Const void* s1,Const void* s2)
#else
(s1,s2)
void* s1;
void* s2;
#endif
{
	logical res;
	long ind1 = *(long*)s1;
	long ind2 = *(long*)s2;

	if(qc_nc == NULL) return(0);
	
	_Nclgt(qc_nc,&res,(void*)((char*)qc_val + qc_nc->type_class.size*ind1),(void*)((char*)qc_val + qc_nc->type_class.size*ind2),NULL,NULL,1,1);

	if(res) return(1);

	res = 0;
	_Ncllt(qc_nc,&res,(void*)((char*)qc_val + qc_nc->type_class.size*ind1),(void*)((char*)qc_val + qc_nc->type_class.size*ind2),NULL,NULL,1,1);

	if(res) return(-1);

	return(0);

	
}

NhlErrorTypes _NclIqsort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL,tmp_md2 = NULL,tmp_md3 = NULL;
	NclVar tmp_var;
	ng_size_t  *index;
	ng_size_t i;
	NclSelectionRecord * sel_ptr = NULL;

	args  = _NclGetArg(0,1,WRITE_IT);
	switch(args.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	case NclStk_VAL:
		NhlPError(NhlFATAL,NhlEUNKNOWN, "qsort: A value was passed in only variables can be sorted");
		return(NhlFATAL);
	}
	qc_nc = tmp_md->multidval.type;
	if(tmp_md->multidval.missing_value.has_missing) {
		qc_missing = &(tmp_md->multidval.missing_value.value);
	}
	qc_val = tmp_md->multidval.val;

	index = NclMalloc(tmp_md->multidval.totalelements * sizeof(ng_size_t));
	for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		index[i] = i;
	}
	qsort((void*)index,tmp_md->multidval.totalelements,sizeof(ng_size_t),qsort_compare_func);
	
	sel_ptr = (NclSelectionRecord*)NclMalloc(sizeof(NclSelectionRecord));
	sel_ptr->n_entries = 1;
	sel_ptr->selection[0].sel_type = Ncl_VECSUBSCR;
	sel_ptr->selection[0].u.vec.n_ind = tmp_md->multidval.totalelements;
	sel_ptr->selection[0].u.vec.ind = index;
	sel_ptr->selection[0].u.vec.min = 0;
	sel_ptr->selection[0].u.vec.max = tmp_md->multidval.totalelements - 1;
	sel_ptr->selection[0].dim_num = 0;

	switch(args.kind) {
	case NclStk_VAR:
		tmp_md2 = (NclMultiDValData)_NclReadSubSection((NclData)tmp_md,sel_ptr,NULL);
		_NclAssignToVar(args.u.data_var,tmp_md2,NULL);
		if((args.u.data_var->var.dim_info[0].dim_quark != -1)&&(_NclIsCoord(args.u.data_var,NrmQuarkToString(args.u.data_var->var.dim_info[0].dim_quark)))) {
			tmp_var = _NclReadCoordVar(args.u.data_var,NrmQuarkToString(args.u.data_var->var.dim_info[0].dim_quark),NULL);
			tmp_md3 = _NclVarValueRead(tmp_var,sel_ptr,NULL);
			_NclWriteCoordVar(args.u.data_var,tmp_md3,NrmQuarkToString(args.u.data_var->var.dim_info[0].dim_quark),NULL);
			_NclDestroyObj((NclObj)tmp_md3);
		}
		_NclDestroyObj((NclObj)tmp_md2);
		break;
	default:
		return(NhlFATAL);
	}
	
	
	qc_nc = NULL;
	qc_missing = NULL;
	qc_val = NULL;
	if(sel_ptr != NULL) {
		for(i = 0; i <  sel_ptr->n_entries; i++) {
			if(sel_ptr->selection[i].sel_type == Ncl_VECSUBSCR){
				NclFree(sel_ptr->selection[i].u.vec.ind);
			}
		}
		NclFree(sel_ptr);
	}
	return(NhlNOERROR);
}
NhlErrorTypes _NclIbsearch
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;


	args  = _NclGetArg(0,1,DONT_CARE);
	switch(args.kind) {
	case NclStk_VAL:
		tmp_md = args.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"Function or procedure not implemented");
	return(NhlFATAL);
}
NhlErrorTypes _NclIfbindirread(void)
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry dimensions;
	NclStackEntry type;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	ng_size_t  n_dimensions = 0;
	ng_size_t *dimsizes = NULL;
	ng_size_t size = 1;
	ng_size_t i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	ng_size_t totalsize = 0;
	off_t f_off;
	ssize_t n;
	char *step = NULL;
	NclStackEntry data_out;
	int *recnum;
	NclScalar missing;
	int has_missing;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	fpath = _NclGetArg(0,4,DONT_CARE);
	recnum = (int*) NclGetArgValue(
		1,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.intval == *recnum)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: record number  is a missing value, can't continue");
		return(NhlFATAL);
	}
	dimensions = _NclGetArg(2,4,DONT_CARE);
	type = _NclGetArg(3,4,DONT_CARE);
	tmp_md = NULL;
	switch(type.kind) {
	case NclStk_VAL:
		tmp_md = type.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(type.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		thetype = _NclNameToTypeClass(*(NclQuark*)tmp_md->multidval.val);
		if(thetype == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: invalid type specified, can't continue");
			return(NhlFATAL);	
		}
	}

	tmp_md = NULL;
	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
		if(path_string == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: An error in the file path was detected could not resolve file path");
			return(NhlFATAL);
		}
		if(stat(path_string,&buf) == -1) {
			NhlPError(NhlFATAL, NhlEUNKNOWN,"fbindirread: Unable to open input file (%s)",path_string);
			return(NhlFATAL);
		}
	}
	tmp_md = NULL;
	switch(dimensions.kind){
	case NclStk_VAL:
		tmp_md = dimensions.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(dimensions.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
/*
 * Create array to hold dimension sizes, which can be int or long.
 */
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = get_dimensions(tmp_md->multidval.val,
					  tmp_md->multidval.totalelements,
					  tmp_md->multidval.data_type,"fbindirread");
		if(dimsizes == NULL) 
		  return(NhlFATAL);
/*
		if((n_dimensions == 1)&&(dimsizes[0] == -1)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: -1 is not supported for fbindirread use cbinread");
			return(NhlFATAL);
		}
*/
	}
	else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: invalid dimension sizes");
	  return(NhlFATAL);
	}
	if((n_dimensions == 1) && (dimsizes[0] == -1)) {
                size = buf.st_size/thetype->type_class.size;
                n_dimensions = 1;
                dimsizes[0] = size;
        } else {
                for(i = 0; i < n_dimensions; i++) {
                        size *= dimsizes[i];
                }
        }
	f_off = (off_t)(*recnum) * (off_t)size * (off_t)thetype->type_class.size;
	totalsize = size*thetype->type_class.size;

	if(f_off + (off_t)totalsize > buf.st_size) {
		ret = NhlFATAL;
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: The size implied by the dimension array and record number is greater that the size of the file, can't continue");
		return(NhlFATAL);
	}
	tmp_ptr = NclMalloc(size*thetype->type_class.size);
	fd = open(path_string,O_RDONLY);
	
	if((tmp_ptr != NULL)&&(fd > 0)) {
		lseek(fd,f_off,SEEK_SET);
		
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			tmp_ptr,
			NULL,
			n_dimensions,
			dimsizes,
			TEMPORARY,
			NULL,
			thetype);
		if(tmp_md == NULL) 
			return(NhlFATAL);

		NclFree(dimsizes);

		step = tmp_ptr;
		for(i = 0; i < (totalsize / buf.st_blksize); i++) {
			n = read(fd, step,buf.st_blksize);
			step = step + buf.st_blksize;
		}
		if(totalsize % buf.st_blksize != 0){
			n = read(fd,step,totalsize % buf.st_blksize);
			if(n == 0) {
				ret = NhlFATAL;
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: An error occurred while reading the file (%s). Size or dimension information is wrong.",path_string);
				return(NhlFATAL);
			}
			step = step + totalsize % buf.st_blksize;
		}
		if (swap_bytes) {
			ng_size_t count = (ng_size_t)(step - (char*)tmp_ptr) / thetype->type_class.size;
			ret = _NclSwapBytes(NULL,tmp_ptr,count,thetype->type_class.size);
			if (ret < NhlWARNING)
				return ret;
		}
		while((ng_size_t)(step - (char*)tmp_ptr) < totalsize) {
			memcpy(step,(char*)&(thetype->type_class.default_mis),thetype->type_class.size);
			step += thetype->type_class.size;
		}

		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = tmp_md;
		_NclPlaceReturn(data_out);
		close(fd);
		return(ret);
	} else if (fd == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: could not open file check permissions");
	}
	return(NhlFATAL);
}

int _MachineIsBigEndian()
{
    short int word = 0x0001;
    char *byte = (char *) &word;
    if(byte[0])
       return 0;
    else
       return 1;
}

NhlErrorTypes _NclIisbigendian
#if	NhlNeedProto
(void)
#else
()
#endif
{
	logical *out_val;
	ng_size_t dimsizes = 1;

	out_val = (logical*)NclMalloc(sizeof(logical));

	*out_val = _MachineIsBigEndian();

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}


NhlErrorTypes _NclIcbinread
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry dimensions;
	NclStackEntry type;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	ng_size_t  n_dimensions = 0;
	ng_size_t *dimsizes = NULL;
	ng_size_t size = 1;
	ng_size_t i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	ng_size_t totalsize = 0;
	char *step = NULL;
	NclStackEntry data_out;
	ng_size_t dim2;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	fpath = _NclGetArg(0,3,DONT_CARE);
	dimensions = _NclGetArg(1,3,DONT_CARE);
	type = _NclGetArg(2,3,DONT_CARE);
	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
		if(path_string == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinread: An error in the file path was detected could not resolve file path");
			return(NhlFATAL);
		}
		if(stat(path_string,&buf) == -1) {
			NhlPError(NhlFATAL, NhlEUNKNOWN,"cbinread: Unable to open input file (%s)",path_string);
			return(NhlFATAL);
		}
	}
	tmp_md = NULL;
	switch(type.kind) {
	case NclStk_VAL:
		tmp_md = type.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(type.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		thetype = _NclNameToTypeClass(*(NclQuark*)tmp_md->multidval.val);
		if(thetype == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinread: invalid type specified, can't continue");
			return(NhlFATAL);	
		}
	}
	tmp_md = NULL;
	switch(dimensions.kind){
	case NclStk_VAL:
		tmp_md = dimensions.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(dimensions.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = get_dimensions(tmp_md->multidval.val,
					  tmp_md->multidval.totalelements,
					  tmp_md->multidval.data_type,"cbinread");
		if(dimsizes == NULL) 
		  return(NhlFATAL);
	}
	else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinread: invalid dimension sizes");
	  return(NhlFATAL);
	}
	if((n_dimensions == 1) && (dimsizes[0] == -1)) {
		size = buf.st_size/thetype->type_class.size;
		n_dimensions = 1;
		dim2 = size;
		dimsizes[0] = dim2;
	} else {
		for(i = 0; i < n_dimensions; i++) {
			size *= dimsizes[i];
		}
	}
	if(size*thetype->type_class.size > buf.st_size) {
		ret = NhlWARNING;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"cbinread: The size implied by the dimension arrays is greater that the size of the file.\n The default _FillValue for the specified type will be filled in.\n Note dimensions and values may not be aligned properly");
		totalsize = buf.st_size;
	} else if(size*thetype->type_class.size < buf.st_size) {
		ret = NhlWARNING;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"cbinread: The size implied by the dimension arrays is less that the size of the file. \n Only the first %d contiguous bytes of the file will be read in.\nNote dimensions and values may not be aligned properly",size*thetype->type_class.size);
		totalsize = size*thetype->type_class.size;
	}  else {
		totalsize = size*thetype->type_class.size;
	}
	tmp_ptr = NclMalloc(size*thetype->type_class.size);
	fd = open(path_string,O_RDONLY);
	if((tmp_ptr != NULL)&&(fd > 0)) {
		
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			tmp_ptr,
			NULL,
			n_dimensions,
			dimsizes,
			TEMPORARY,
			NULL,
			thetype);
		if(tmp_md == NULL) 
			return(NhlFATAL);

		NclFree(dimsizes);

		step = tmp_ptr;
		for(i = 0; i < (ng_size_t)(totalsize / buf.st_blksize); i++) {
			ret = read(fd, step,buf.st_blksize);
			step = step + buf.st_blksize;
		}
		ret = read(fd,step,totalsize % buf.st_blksize);
		step = step + totalsize % buf.st_blksize;
		if (swap_bytes) {
		        ng_size_t count = ((ng_size_t)(step - (char*)tmp_ptr)) / thetype->type_class.size;
			ret = _NclSwapBytes(NULL,tmp_ptr,count,thetype->type_class.size);
			if (ret < NhlWARNING)
				return ret;
		}
		while((ng_size_t)(step - (char*)tmp_ptr) < totalsize) {
			memcpy(step,(char*)&(thetype->type_class.default_mis),thetype->type_class.size);
			step += thetype->type_class.size;
		}
		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = tmp_md;
		_NclPlaceReturn(data_out);
		close(fd);
		return(ret);
	} else if (fd == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinread: could not open file check permissions");
	}
	return(NhlFATAL);
}

NhlErrorTypes _NclIfbinnumrec
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclQuark *fpath;
	NclScalar missing;
	int 	has_missing = 0;
	char 	control_word[8];
	int fd = -1;
	off_t cur_off;
	int i;
	ssize_t n;
	ng_size_t dimsize = 1;
	int swap_bytes = 0;
	int marker_size = 4;
	NhlErrorTypes ret = NhlNOERROR;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif
	if (8 == *(int *)(fcp->options[Ncl_RECORD_MARKER_SIZE].value->multidval.val)) {
		marker_size = 8;
	}
	memset((void*) control_word,0,marker_size);

	fpath = (NclQuark*)NclGetArgValue(
		0,
		1,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.stringval == *fpath)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: path is a missing value, can't continue");
		return(NhlFATAL);
	}
	fd = open(_NGResolvePath(NrmQuarkToString(*fpath)),O_RDONLY);

	if(fd == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
		return(NhlFATAL);
	}
	cur_off = 0;
	i = 0;
	while(1) {	
		lseek(fd,cur_off,SEEK_SET);
		n = read(fd,(control_word),marker_size);
		if(n != marker_size) {
			break;
		}
		if (marker_size == 4) {
			int ind1, ind2;
			if (! swap_bytes)
				ind1 = *(int*)control_word;
			else
				_NclSwapBytes(&ind1,control_word,1,marker_size);
			lseek(fd,cur_off + (off_t)(ind1 + marker_size),SEEK_SET);
			n = read(fd,(control_word),marker_size);
			if(n != marker_size) {
				break;
			}
			if (! swap_bytes)
				ind2 =  *(int*)control_word;
			else
				_NclSwapBytes(&ind2,control_word,1,marker_size);
			if(ind1 ==  ind2) {
				i++;
				cur_off += (off_t)(ind1 + marker_size * 2);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
			}
		}
		else {
			long long ind1, ind2;
			if (! swap_bytes)
				ind1 = *(long long*)control_word;
			else
				_NclSwapBytes(&ind1,control_word,1,marker_size);
			lseek(fd,cur_off + (off_t)(ind1 + marker_size),SEEK_SET);
			n = read(fd,(control_word),marker_size);
			if(n != marker_size) {
				break;
			}
			if (! swap_bytes)
				ind2 =  *(long long*)control_word;
			else
				_NclSwapBytes(&ind2,control_word,1,marker_size);
			if(ind1 ==  ind2) {
				i++;
				cur_off += (off_t)(ind1 + marker_size * 2);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
			}
		}
	}
	close(fd);
	return(NclReturnValue(
		&i,
		1,
		&dimsize,
		NULL,
		NCL_int,
		1
	));
	
}


NhlErrorTypes _NclIfbinrecwrite
#if	NhlNeedProto
(void)
#else
()
#endif
{
	static int fd = -1;
	static off_t cur_off = 0;
	static int cur_recnum = 0;
	NclQuark *fpath;
	int	*recnum;
	ng_size_t	dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing;
	int 	has_missing = 0;
	NclTypeClass type;
	char 	control_word[8];
	void *value;
	ng_size_t i;
	long long ind1,ind2;
	ssize_t n;
	int n_dims;
	NhlErrorTypes ret = NhlNOERROR;
	int rsize = 0;
	NclBasicDataTypes datai_type;
	ng_size_t total;
	int itotal;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;
	int marker_size = 4;
	NhlBoolean keep_open = False;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif
	if (8 == *(int *)(fcp->options[Ncl_RECORD_MARKER_SIZE].value->multidval.val)) {
		marker_size = 8;
	}
	if ( *(NhlBoolean *)(fcp->options[Ncl_KEEP_OPEN].value->multidval.val) == True)
		keep_open = True;

	memset((void*) control_word,0,marker_size);

	fpath = (NclQuark*)NclGetArgValue(
		0,
		3,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.stringval == *fpath)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: path is a missing value, can't continue");
		return(NhlFATAL);
	}

	recnum = (int*) NclGetArgValue(
		1,
		3,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.intval == *recnum)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: record number  is a missing value, can't continue");
		return(NhlFATAL);
	}
	
	value = (void*)NclGetArgValue(
		2,
		3,
		&n_dims,
		dimsizes,
		&missing,
		&has_missing,
		&datai_type,
		0);
	
	type = (NclTypeClass)_NclNameToTypeClass(NrmStringToQuark(_NclBasicDataTypeToName(datai_type)));

	total = 1;
	for(i = 0; i < n_dims; i++) {
		total *= dimsizes[i];
	}
	total *= type->type_class.size;

#if !defined(NG32BIT)
	if(total > INT_MAX) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: cannot write more than 2 Gb values to a file.");
	  return(NhlFATAL);
	}
#endif
	itotal = (int)total;

	if (fd < 0) {
		fd = open(_NGResolvePath(NrmQuarkToString(*fpath)),(O_CREAT | O_RDWR),0644);
		if(fd == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
			return(NhlFATAL);
		}
		cur_off = 0;
	}

	if (*recnum > -1 && *recnum < cur_recnum) {
		i = 0;
		cur_off = 0;
	}
	else {
		i = cur_recnum;
	}
	if(*recnum != -1) {
		while(i != *recnum + 1) {	
			lseek(fd,cur_off,SEEK_SET);
			n = read(fd,(control_word),marker_size);
			if(n != marker_size) {
/*
* end of file reached
*/	
				rsize = -1;
				NhlPError(NhlINFO,NhlEUNKNOWN,"fbinrecwrite: end of file reached before record number, writing record as last record in file");
				break;
			}
			if (! swap_bytes)
				ind1 = marker_size == 4 ? *(int*)control_word : *(long long *) control_word;
			else if (marker_size == 4) {
				int itmp;
				_NclSwapBytes(&itmp,control_word,1,marker_size);
				ind1 = itmp;
			}
			else 
				_NclSwapBytes(&ind1,control_word,1,marker_size);
			lseek(fd,cur_off + (off_t)(ind1 + 4),SEEK_SET);
			n = read(fd,(control_word),marker_size);
			if(n != marker_size) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
				break;
			}
			if (! swap_bytes)
				ind2 = marker_size == 4 ? *(int*)control_word : *(long long *) control_word;
			else if (marker_size == 4) {
				int itmp;
				_NclSwapBytes(&itmp,control_word,1,marker_size);
				ind2 = itmp;
			}
			else
				_NclSwapBytes(&ind2,control_word,1,marker_size);
			if(ind1 == ind2) {
					i++;
					cur_off += (off_t)(ind1 + 2 * marker_size);
					rsize = ind1;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
			}
		}
	} else {
		rsize = -1;
		lseek(fd,0,SEEK_END);
	}
	if((rsize == -1)||(rsize== total)){
		long long ll_total;
		ll_total = itotal;
		if (rsize != -1) {
			/* seek to the beginning of current record */
			cur_off -= (off_t)(rsize + 2 * marker_size);
			lseek(fd,cur_off,SEEK_SET);
		}
		if (swap_bytes) {
			int l_total;
			char *outdata = NclMalloc(total);
			if (!outdata)
				return (NhlFATAL);
			_NclSwapBytes(outdata,value,itotal / type->type_class.size,type->type_class.size);
			if (marker_size == 4) {
				_NclSwapBytes(&l_total,&itotal,1,4);
				n = write(fd,&l_total,4);
				n = write(fd,outdata,itotal);
				n = write(fd,&l_total,4);
			}
			else {
				_NclSwapBytes(NULL,&ll_total,1,8);
				n = write(fd,&ll_total,8);
				n = write(fd,outdata,itotal);
				n = write(fd,&ll_total,8);
			}
			NclFree(outdata);
		}
		else if (marker_size == 4) {
			n = write(fd,&itotal,4);
			n = write(fd,value,itotal);
			n = write(fd,&itotal,4);
		}
		else {
			n = write(fd,&ll_total,8);
			n = write(fd,value,itotal);
			n = write(fd,&ll_total,8);
		}
		if (! keep_open) {
			close(fd);
			fd = -1;
			cur_off = 0;
			cur_recnum = 0;
		} 
		else {
			cur_recnum = cur_recnum + 1;
			cur_off = lseek(fd,0,SEEK_CUR);
		}
		return(NhlNOERROR);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: data variable size does not match record size");
		close(fd);
		return(NhlFATAL);
	}
	

}
NhlErrorTypes _NclIfbinrecread
#if	NhlNeedProto
(void)
#else
()
#endif
{
	static int fd = -1;
	static off_t cur_off = 0;
	static int cur_recnum = 0;
	NclQuark *fpath;
	int	*recnum;
	ng_size_t	*dimensions;
	NclBasicDataTypes type_dimensions;
	void	*tmp_dimensions;
	ng_size_t	dimsize;
	NclQuark *type;
	NclScalar missing;
	NclMultiDValData tmp_md;
	NclStackEntry data;
	int 	has_missing = 0;
	NclTypeClass thetype;
	char 	control_word[8];
	void *value;
	ng_size_t i;
	long long ind1,ind2;
	ng_size_t size = 1, tmp_size = 1;
	ssize_t n;
	NhlErrorTypes ret = NhlNOERROR;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;
	int marker_size = 4;
	NhlBoolean keep_open = False;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif
	if ( *(NhlBoolean *)(fcp->options[Ncl_KEEP_OPEN].value->multidval.val) == True)
		keep_open = True;

	if (8 == *(int *)(fcp->options[Ncl_RECORD_MARKER_SIZE].value->multidval.val)) {
		marker_size = 8;
	}

	memset((void*) control_word,0,8);

	data = _NclGetArg(0,4,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAL:
		tmp_md = data.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		if(data.u.data_var->var.att_id != -1) {
			NclAtt attobj;
			NclAttList *attlist;
                        attobj =  (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
                        if (attobj && attobj->att.n_atts > 0) {
				attlist = attobj->att.att_list;
				while (attlist != NULL) {
					if (!strcmp(attlist->attname,"keep_open")) {
						keep_open = *(NhlBoolean *) attlist->attvalue->multidval.val;
						break;
					}
					attlist = attlist->next;
				}
			}
		}
		break;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: path cannot be decoded, can't continue");
		return(NhlFATAL);
	}
	if(tmp_md->multidval.missing_value.has_missing) {
		has_missing = 1;
		missing = tmp_md->multidval.missing_value.value;
	}
	else {
		has_missing = 0;
		missing = tmp_md->multidval.missing_value.value;
	}
	fpath = (NclQuark *) tmp_md->multidval.val;

/*
	fpath = (NclQuark*)NclGetArgValue(
		0,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
*/
	if(has_missing &&(missing.stringval == *fpath)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: path is a missing value, can't continue");
		return(NhlFATAL);
	}

	recnum = (int*) NclGetArgValue(
		1,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.intval == *recnum)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: record number  is a missing value, can't continue");
		return(NhlFATAL);
	}
	
	tmp_dimensions = (void*)NclGetArgValue(
		2,
		4,
		NULL,
		&dimsize,
		&missing,
		&has_missing,
		&type_dimensions,
		0);

	dimensions = get_dimensions(tmp_dimensions,dimsize,type_dimensions,
				    "fbinrecread");
	if(dimensions == NULL) 
	  return(NhlFATAL);
	if(*dimensions!= -1) {
		for(i = 0; i < 	dimsize; i++) {
			if(has_missing&&(missing.intval == *(dimensions + i))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: dimension size contains a missing value, can't continue");
				return(NhlFATAL);
			}
			size *= dimensions[i];
		}
	} else {
        tmp_size = -1;
		size = (ng_size_t) tmp_size;
	}
	type = (NclQuark*)NclGetArgValue(
		3,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
	if(has_missing &&(missing.stringval == *type)) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: type is a missing value, can't continue");
		return(NhlFATAL);
	}
	thetype = _NclNameToTypeClass(*type);
	if(thetype == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: invalid type specified, can't continue");
		return(NhlFATAL);	
	}
	if (fd < 0) {
		fd = open(_NGResolvePath(NrmQuarkToString(*fpath)),O_RDONLY);

		if(fd == -1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
			return(NhlFATAL);
		}
		cur_off = 0;
	}

	if (*recnum < cur_recnum) {
		i = 0;
		cur_off = 0;
	}
	else {
		i = cur_recnum;
	}

	while(i != *recnum) {	
		lseek(fd,cur_off,SEEK_SET);
		n = read(fd,(control_word),marker_size);
		if(n != marker_size) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if (! swap_bytes)
			ind1 = marker_size == 4 ? *(int*)control_word : *(long long *) control_word;
		else if (marker_size == 4) {
			int itmp;
			_NclSwapBytes(&itmp,control_word,1,marker_size);
			ind1 = itmp;
		}
		else {
			_NclSwapBytes(&ind1,control_word,1,marker_size);
		}
		if(ind1 <= 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: 0 or less than zero fortran control word, FILE NOT SEQUENTIAL ACCESS!");
			close(fd);
			return(NhlFATAL);
		}
		lseek(fd,cur_off + (off_t)(ind1 + marker_size),SEEK_SET);
		n = read(fd,(control_word),marker_size);
		if(n != marker_size) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if (! swap_bytes)
			ind2 = marker_size == 4 ? *(int*)control_word : *(long long *) control_word;
		else if (marker_size == 4) {
			int itmp;
			_NclSwapBytes(&itmp,control_word,1,marker_size);
			ind2 = itmp;
		}
		else {
			_NclSwapBytes(&ind2,control_word,1,marker_size);
		}
		if(ind1 == ind2) {
			i++;
			cur_off += (off_t)(ind1 + 2 * marker_size);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
			close(fd);
			return(NhlFATAL);
		}
	}
	if(i == *recnum) {
		lseek(fd,cur_off,SEEK_SET);
		n = read(fd,(control_word),marker_size);
		if(n != marker_size) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if (! swap_bytes)
			ind1 = marker_size == 4 ? *(int*)control_word : *(long long *) control_word;
		else if (marker_size == 4) {
			int itmp;
			_NclSwapBytes(&itmp,control_word,1,marker_size);
			ind1 = itmp;
		}
		else {
			_NclSwapBytes(&ind1,control_word,1,marker_size);
		}
		if(tmp_size != -1) {
			value = (void*)NclMalloc(thetype->type_class.size*size);
			if(ind1 < size*thetype->type_class.size) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"fbinrecread: size specified is greater than record size, filling with missing values");
				ret = NhlWARNING;
			} else if(ind1 > size*thetype->type_class.size) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"fbinrecread: size specified is less than record size, some data will not be read");
				ret = NhlWARNING;
			}
			n = read(fd,value,(ind1>=size*thetype->type_class.size)?size*thetype->type_class.size:ind1);
			if(n != ((ind1>=size*thetype->type_class.size)?size*thetype->type_class.size:ind1))  {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: an error occurred reading the requested record. Something is wrong with the FORTRAN binary file.");
				NclFree(value);
				close(fd);
				return(NhlFATAL);
			}
			if (swap_bytes) {
				_NclSwapBytes(NULL,value,n / thetype->type_class.size,thetype->type_class.size);
			}
			if(ind1 < size*thetype->type_class.size) {
				for(;ind1<size*thetype->type_class.size-1;ind1+=thetype->type_class.size) {
					memcpy((void*)((char*)value + ind1),&thetype->type_class.default_mis,thetype->type_class.size);
				}
			}
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				value,
				NULL,
				dimsize,
				dimensions,
				TEMPORARY,
				NULL,
				thetype);
		} else {
			value = (void*)NclMalloc(ind1);
			n = read(fd,value,ind1);
			if(n != ind1) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: an error occurred reading the requested record. Something is wrong with the FORTRAN binary file.");
				NclFree(value);
				close(fd);
				return(NhlFATAL);
			}
			if (swap_bytes) {
				_NclSwapBytes(NULL,value,n / thetype->type_class.size,thetype->type_class.size);
			}
			dimsize = ind1/thetype->type_class.size;
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				value,
				NULL,
				1,
				&dimsize,
				TEMPORARY,
				NULL,
				thetype);
		}
		if(tmp_md == NULL) {
			NclFree(value);
			close(fd);
			return(NhlFATAL);
		}
		data.kind = NclStk_VAL;
		data.u.data_obj = tmp_md;
		_NclPlaceReturn(data);
		if (! keep_open) {
			close(fd);
			fd = -1;
			cur_off = 0;
			cur_recnum = 0;
		} 
		else {
			cur_recnum = *recnum + 1;
			n = read(fd,(control_word),marker_size);
			cur_off += (off_t)(ind1 + 2 * marker_size);
		}
		NclFree(dimensions);
		return(ret);
	} else {
		close(fd);
		return(NhlFATAL);
	}
	

}
NhlErrorTypes _NclIfbinread
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry dimensions;
	NclStackEntry type;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	ng_size_t  n_dimensions = 0;
	ng_size_t *dimsizes = NULL;
	ng_size_t size = 1, tmp_size = 1;
	ng_size_t i;
	void *tmp_ptr;
	ng_size_t totalsize = 0;
	ng_size_t n;
	NclStackEntry data_out;
	FILE *fd;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;
	char control_word[8];
	int marker_size = 4;
	ng_size_t ind1;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	if (8 == *(int *)(fcp->options[Ncl_RECORD_MARKER_SIZE].value->multidval.val)) {
		marker_size = 8;
	}

	memset((void*) control_word,0,marker_size);

	fpath = _NclGetArg(0,3,DONT_CARE);
	dimensions = _NclGetArg(1,3,DONT_CARE);
	type = _NclGetArg(2,3,DONT_CARE);
	tmp_md = NULL;
	switch(type.kind) {
	case NclStk_VAL:
		tmp_md = type.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(type.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		thetype = _NclNameToTypeClass(*(NclQuark*)tmp_md->multidval.val);
		if(thetype == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: invalid type specified, can't continue");
			return(NhlFATAL);	
		}
	}
	tmp_md = NULL;
	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: error retrieving the file path");
		return(NhlFATAL);
	}
	path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
	if(path_string == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: An error in the file path was detected could not resolve file path");
		return(NhlFATAL);
	}
	fd = fopen(path_string, "r");
	if(fd == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "fbinread: could not open (%s) check path and permissions, can't continue",path_string);
		return(NhlFATAL);
	}
	tmp_md = NULL;
	switch(dimensions.kind){
	case NclStk_VAL:
		tmp_md = dimensions.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(dimensions.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
/*
 * Create array to hold dimension sizes, which can be int or long.
 */
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = get_dimensions(tmp_md->multidval.val,
					  tmp_md->multidval.totalelements,
					  tmp_md->multidval.data_type,"fbinread");
		if(dimsizes == NULL) 
		  return(NhlFATAL);
	}
	else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: invalid dimension sizes");
	  return(NhlFATAL);
	}
	if((n_dimensions == 1) && (dimsizes[0] == -1)){
		tmp_size = -1;
		n_dimensions = 1;
	} else {
		for(i = 0; i < n_dimensions; i++) {
			size *= dimsizes[i];
		}
	}
	if(fread((void*)control_word, 1, marker_size, fd) != marker_size) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: An error occurred while reading the file (%s), check path",
			  _NGResolvePath(path_string));
		return(NhlFATAL);
	}
	if (! swap_bytes)
		ind1 = marker_size == 4 ? *(int*)control_word : *(long long *) control_word;
	else if (marker_size == 4) {
		int itmp;
		_NclSwapBytes(&itmp,control_word,1,marker_size);
		ind1 = itmp;
	}
	else {
		_NclSwapBytes(&ind1,control_word,1,marker_size);
	}
	if (ind1 <= 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: 0 or less than zero fortran control word, FILE NOT SEQUENTIAL ACCESS!");
		close(fd);
		return(NhlFATAL);
	}
	if(tmp_size == -1) {
		size = ind1;
		n_dimensions = 1;
		*dimsizes= size/thetype->type_class.size;
		totalsize = size;
	}
	else {
		totalsize = size * thetype->type_class.size;
		if (totalsize > ind1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "fbinread: requested variable size exceeds record size");
			close(fd);
			return(NhlFATAL);
		}
	}
	tmp_ptr = NclMalloc(totalsize);
	if (! tmp_ptr) {
	  NhlPError(NhlFATAL,ENOMEM,NULL);
	  return( NhlFATAL);
	}
	fseek(fd, (off_t)marker_size, SEEK_SET); /* skip the control word */
	n = (ng_size_t) fread(tmp_ptr, 1, totalsize, fd);
	if(n != totalsize)  {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: an error occurred reading the FORTRAN binary file.");
	  NclFree(tmp_ptr);
	  fclose(fd);
	  return(NhlFATAL);
	}
#if 0
	NGCALLF(nclpfortranread,NCLPFORTRANREAD)(path_string,tmp_ptr,&totalsize,&ret,strlen(path_string));
#endif
	if (swap_bytes) {
	  _NclSwapBytes(NULL,tmp_ptr,totalsize / thetype->type_class.size,thetype->type_class.size);
	}
	
	
	tmp_md = _NclCreateMultiDVal(
				     NULL,
				     NULL,
				     Ncl_MultiDValData,
				     0,
				     tmp_ptr,
				     NULL,
				     n_dimensions,
				     dimsizes,
				     TEMPORARY,
				     NULL,
				     thetype);
	if(tmp_md == NULL) 
	  return(NhlFATAL);
	data_out.kind = NclStk_VAL;
	data_out.u.data_obj = tmp_md;
	fclose(fd);
	NclFree(dimsizes);
	_NclPlaceReturn(data_out);
	return(ret);
} 
NhlErrorTypes _NclIasciiwrite
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry value;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	ng_size_t i;
	FILE *fd = NULL;
	char *step = NULL;
	int is_stdout = 0;
	NclVaPrintFunc tmp ;


	fpath = _NclGetArg(0,2,DONT_CARE);
	value = _NclGetArg(1,2,DONT_CARE);

	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		if((*(NclQuark*)tmp_md->multidval.val)==NrmStringToQuark("stdout")) {
			is_stdout = 1;
		} else {

			path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
			if(path_string == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiwrite: An error in the file path was detected could not resolve file path");
				return(NhlFATAL);
			}
		}
	}
	tmp_md = NULL;
	switch(value.kind){
	case NclStk_VAL:
		tmp_md = value.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(value.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(is_stdout) {
		fd = stdout;
	} else {
		errno = (short) 0;
		fd = fopen(path_string,"w+");
		if (fd == NULL && errno != (short) 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiwrite: Unable to open file for writing: %s", strerror(errno));
			return(NhlFATAL);
		}
		else if (fd == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiwrite: Unable to open file for writing");
			return(NhlFATAL);
		}
		tmp = _NclSetPrintFunc(vfprintf);
	}
	step = (char*)tmp_md->multidval.val;
	for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		ret = _Nclprint(tmp_md->multidval.type,fd,(void*)step);
		if(ret < NhlINFO) {
			return(NhlWARNING);
		}
		if(nclfprintf(fd,"\n")< 0) {
			return(NhlWARNING);
		}
		step = step + tmp_md->multidval.type->type_class.size;
	}
	if(!is_stdout) {
		_NclSetPrintFunc(tmp);
		fclose(fd);
	}
	return(NhlNOERROR);
}


#if 0
/* This routine has been replaced by two routines: asciifloat and asciiinteger */
int asciinumeric
#if NhlNeedProto
(FILE *fp, char* format,void *retvalue)
#else
()
#endif
{
	char buffer[256];
	char *step;
	int state = 1;
	int predot = 0, postdot = 0, postexp = 0;
	char cc;

	step = buffer;
	while(!feof(fp)&& (state != -1)) {
		cc = fgetc(fp);
		switch(state) { 
		case 0: /* nothing found yet:  0 and 1 are the same */
		case 1:
			switch(cc) {
			case '+':
			case '-':
				*step++ = cc;
				state = 2;		
				break;
			case '.':
				*step++ = cc;
				state = 5;
				break;
			case 'n':
			case 'N':
				/* possible Nan */
				state = 9;
				*step++ = cc;
				break;
			default:
				if(isdigit(cc)) {
					*step++ = cc;
					state = 3;
				} else {
					step = buffer;
					state = 0;
				}
			}
			break;
		case 2: /* initial +|- */
			switch(cc) {
			case '.':
				*step++ = cc;
				state = 5;
				break;
#if 0
/* 
 * I dont think e|E is valid after an initial + or - with no digits
 * intervening, anymore than it is valid as the initial character. 
 * It does not produce a valid number when scanned by sscanf.
 * dib
 */

 			case 'E' :
			case 'e' : 
				state = 5;
				ungetc(cc,fp);
				break;
#endif
			default:
				if(isdigit(cc)) {
					*step++ = cc;
					state = 3;
				} else {
					/* not a number */
					ungetc(cc,fp);
					step = buffer;
					state = 0;
				}
			}
			break;
		case 3: /* digit before decimal point */
			predot = 1;
			if(isdigit(cc)) {
				*step++ = cc;
			} else {
				switch(cc) {
				case '.':
					*step++ = cc;
					state = 5;
					break;
				case 'E' :
				case 'e' : 
					ungetc(cc,fp);
					state = 5;
					break;
				default:
/*
 * ACCEPT INTEGER
 */
					ungetc(cc,fp);
					state = -1;
					*step++ = '\0';
				}
			}
			break;
		case 5: 
			/* decimal point, possibly virtual if e|E */
			if(isdigit(cc)) {
				postdot = 1;
				*step++ = cc;
			} else {
				switch(cc) {
				case 'E':
				case 'e':
					*step++ = cc;
					state = 6;
					break;
				default:
					ungetc(cc,fp);
					
					if (! (predot || postdot)) {
						/*
						 * a single dot is not a number
						 */
						step = buffer;
						state = 0;
					}
					else {
						state = -1;
						*step++ = '\0';
					}
				}
			}
			break;
		case 6: /* exponent e|E */
			if(isdigit(cc)) {
				postexp = 1;
				*step++ = cc;
				state = 8;
			} else {
				switch(cc) {
				case '+':
				case '-':
					*step++ = cc;
					state = 7;
					break;
				default:
					ungetc(cc,fp);
					if (! (predot || postdot)) {
						/* not a number */
						step = buffer;
						state = 0;
					}
					else {
						step--; /* get rid of e|E */
						*step++ = '\0';
						state = -1;
					}
				}
			}
			break;
		case 7: /* + or - after exponent */
			if(isdigit(cc)) {
				postexp = 1;
				*step++=  cc;
                                state = 8;
                        } else {
				ungetc(cc,fp);
				if (! (predot || postdot)) {
					step = buffer;
					state = 0;
				}
				else {
					step -= 2; /* get rid of sign and e */
					*step++ = '\0';
					state = -1;
				}
			}
			break;
		case 8: 
			/* at least 1 digit of an exponent */
			if(isdigit(cc)) {
				*step++=  cc;
				state = 8;
			} else {
				ungetc(cc,fp);
				*step++ = '\0';
				state = -1;
			}
			break;
		case 9: 
			/* possible 'a' of Nan */
			if(cc == 'a' || cc == 'A') {
				*step++=  cc;
				state = 10;
			} else {
				step = buffer;
				state = 0;
			}
			break;
		case 10: 
			/* possible second 'n' of Nan */
			if(cc == 'n' || cc == 'N') {
				*step++=  cc;
				*step++ = '\0';
				state = -1;
			} else {
				step = buffer;
				ungetc(cc,fp);
				state = 0;
			}
			break;
		}
	}
	if((step != buffer)&&(!feof(fp))) {
		sscanf(buffer,format,retvalue);
		return(1);
	} else {
		return(0);
	}
}
#endif

/*
 * Read float types (double and float)
 * There is a system dependency for strtod in its handling of 
 * the '0x' prefix. 
 * For predictable results, force 0x to be read as a '0'. 
 * Might not be totally predictable if strtod recognizes variations
 * of inf and nan: GNU libc recognizes "infinity"; what else might there be?
 */

int asciifloat(char *buf, char **end, int type, void *retvalue,char **rem) {
	double tmpd;
	const char *initchars = "0123456789+-.nNiI";
	int i;
	char *cp;

	
	i = strcspn(buf,initchars);
	while (buf[i] != '\0') {
		*rem = NULL;
		if (buf[i] == '0' && 
		    (buf[i+1] == 'x' || buf[i+1] == 'X')) {
			tmpd = 0;
			*end = &(buf[i+1]);
		}
		else {
			tmpd = strtod(&(buf[i]),end);
		}
		switch (**end) {
			/* find entities that parsed as a number but are possibly incomplete
			   because they are at the end of a buffer */
		case '\0':
			*rem = &(buf[i]);
			break;
		case 'e':
		case 'E':
			cp = (*end) + 1;
			if ((*cp == '\0') ||
			    ((*cp == '-' || *cp == '+') && (*(cp+1) == '\0'))) {
				*rem = &(buf[i]);
			}
			break;
		default:
			break;
		}
		if (*end == &(buf[i])) {
			if (strlen(buf) - i < 4) {
				/* initial characters were found but not enough to parse as a number */
				*rem = &(buf[i]);
				return 0;
			}
			i++;
			i += strcspn(&(buf[i]),initchars);
			continue;
		}
		else if (buf[i] == 'n' || buf[i] == 'N' 
			 || buf[i] == 'i' || buf[i] == 'I') {
			/* inf or nan recognized: but is it an accident ? */
			if ((i > 0 && isalpha(buf[i-1])) || isalpha(**end)) {
				i = *end - buf;
				i += strcspn(*end,initchars);
				continue;
			}
		}
		switch (type) {
		case Ncl_Typefloat:
			*((float*)retvalue) = (float) tmpd;
			break;
		case Ncl_Typedouble:
			*((double*)retvalue) =  tmpd;
			break;
		}
		i = *end - buf;
		i += strcspn(*end,initchars);
		while (buf[i] == 'n' || buf[i] == 'N' 
		       || buf[i] == 'i' || buf[i] == 'I') {
			if (isalpha(buf[i-1])) 
				i++;
			else
				break;
		}
		*end = &(buf[i]);
		return 1;
	}
	*end = &(buf[i]);
	return 0;
}


/*
 * Read all the integer types:
 * Note that the file is scanned as a float but read as integer.
 * This may seem weird but is for compatibility with the old code.
 * Example: encountering the string 3e4, the scanner ingests this
 * as one number, but when read as an integer the 'e4' is ignored,
 * resulting in the single value '3'. 
 * A sequence of digits prefaced with '.' is treated as 0.
 * Ox is recognized as a prefacing a hex value for all integer types,
 * but 0 is recognized as a preface for an octal value only if reading
 * as Ncl_Typebyte,
 * Overflow is handled simply by bit truncation.
 * Nan is not valid for integers.
 */
int asciiinteger(char *buf, char **end, int type, void *retvalue,char **rem) {
	double tmpd;
	const char *initchars = "0123456789+-.";
	int i;
	int ishex = 0;
	long tmpi;
	char *iend;
	
	i = strcspn(buf,initchars);
	while (buf[i] != '\0') {
		*rem = NULL;
		/* don't depend on strtod understanding hex */
		if (buf[i] == '0' && 
		    (buf[i+1] == 'x' || buf[i+1] == 'X')) {
			ishex = 1;
			tmpi = strtol(&(buf[i]),end,16);
			/* 
			 * Skip fractional hex part for consistency 
			 * with decimal behavior
			 */
			if (**end == '.') {
				(*end)++;
				while (isxdigit(**end)) {
					(*end)++;
				}
			}
		}
		else {
			tmpd = strtod(&(buf[i]),end);
		}
		if (**end == '\0') {
			*rem = &(buf[i]);
		}
		if (*end == &(buf[i])) {
			if (strlen(buf) - i < 4) {
				/* initial characters were found but not enough to parse as a number */
				*rem = &(buf[i]);
				return 0;
			}
			i++;
			i += strcspn(&(buf[i]),initchars);
			ishex = 0;
			continue;
		}
		if (! ishex) {
			if (type == Ncl_Typebyte) {
				/* accept octal or decimal */
				tmpi = strtol(&(buf[i]),&iend,0);
			}
			else {
				tmpi = strtol(&(buf[i]),&iend,10);
			}
		}
		if (iend == &(buf[i])) {
			/* for compatibility with the old way
			 * values that start with '.' are treated as 0
			 */
			if (buf[i] == '.' && isdigit(buf[i+1])) {
			    tmpi = 0;
			}
			else {
				i++;
				i += strcspn(&(buf[i]),initchars);
				ishex = 0;
				continue;
			}
		}

		switch (type) {
		case Ncl_Typelong:
			*((long*)retvalue) = tmpi;
			break;
		case Ncl_Typeint:
			*((int*)retvalue) =  (int)tmpi;
			break;
		case Ncl_Typeshort:
			*((short*)retvalue) =  (short)tmpi;
			break;
		case Ncl_Typebyte:
			*((byte*)retvalue) =  (byte)tmpi;
			break;
		}
		i = *end - buf;
		i += strcspn(*end,initchars);
		*end = &(buf[i]);
		return 1;
	}
	*end = &(buf[i]);
	return 0;
}
			



NhlErrorTypes _NclIasciiread
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry dimensions;
	NclStackEntry type;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	ng_size_t  n_dimensions = 0;
	ng_size_t *dimsizes = NULL;
	ng_size_t size = 1;
	ng_size_t i;
	int j;
	void *tmp_ptr;
	struct stat statbuf;
	FILE *fp = NULL;
	ng_size_t totalsize = 0;
	NclStackEntry data_out;
	int has_unlimited = 0;
	int bufsize = 4096;
	char buf[4096];
	ng_size_t total = 0;
	char *cp;

	fpath = _NclGetArg(0,3,DONT_CARE);
	dimensions = _NclGetArg(1,3,DONT_CARE);
	type = _NclGetArg(2,3,DONT_CARE);
	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
		if(path_string == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: An error in the file path was detected could not resolve file path");
			return(NhlFATAL);
		}
		if(stat(path_string,&statbuf) == -1) {
			NhlPError(NhlFATAL, NhlEUNKNOWN,"asciiread: Unable to open input file (%s)",path_string);
			return(NhlFATAL);
		}
	}
	tmp_md = NULL;
	switch(dimensions.kind){
	case NclStk_VAL:
		tmp_md = dimensions.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(dimensions.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
/*
 * Create array to hold dimension sizes, which can be int or long.
 */
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = get_dimensions(tmp_md->multidval.val,
					  tmp_md->multidval.totalelements,
					  tmp_md->multidval.data_type,"asciiread");
		if(dimsizes == NULL) 
		  return(NhlFATAL);
	}
	else {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: invalid dimension sizes");
	  return(NhlFATAL);
	}
	if(dimsizes[0] == -1) {
	  has_unlimited = 1;
	} else {
		has_unlimited = 0;
		for(i = 0; i < n_dimensions; i++) {
			if(dimsizes[i] < 1)  {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Dimension size less than 1 specified and its not the first dimension, can't determine size");
				return(NhlFATAL);
			} else {
				size *= dimsizes[i];
			}
		}
	}
	tmp_md = NULL;
	switch(type.kind) {
	case NclStk_VAL:
		tmp_md = type.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(type.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		thetype = _NclNameToTypeClass(*(NclQuark*)tmp_md->multidval.val);
		if(thetype == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: invalid type specified, can't continue");
			return(NhlFATAL);	
		}
	}

	if(!has_unlimited) {
		totalsize = size;
		
		tmp_ptr = NclMalloc(size*thetype->type_class.size);
		errno = 0;
		fp = fopen(path_string,"r");
		if (fp == NULL && errno) {
			NhlPError(NhlFATAL,errno,"asciiread: Unable to open file for reading");
			return(NhlFATAL);
		}
		else if (fp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Unable to open file for reading: check permissions");
			return(NhlFATAL);
		}
		if(tmp_ptr != NULL) {
			
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				tmp_ptr,
				&(thetype->type_class.default_mis),
				n_dimensions,
				dimsizes,
				TEMPORARY,
				NULL,
				thetype);
			if(tmp_md == NULL) 
				return(NhlFATAL);

			if (thetype->type_class.type & NCL_SNUMERIC_TYPE_MASK) {
				char *end = "";
				int lret;
				int count;
				char *rem;
			        if (thetype->type_class.type & NCL_NUMERIC_TYPE_MASK) {
				while (total < totalsize) {
					if (*end == '\0') {
						count = fread(buf,1,bufsize-1,fp);
						if (count <= 0) {
							break;
						}
						buf[count] = '\0';
						end = buf;
						/* 
						 * this is to take care of a difference in SGI's strtod:
						 * it allows spaces between an 'e' and the exponent digits
						 * in a floating point number. So 
						 */
						cp = strchr(buf,' ');
						while (cp != NULL) {
							*cp = '\n';
							cp = strchr(cp+1,' ');
						}
					}
					rem = NULL;
					if (thetype->type_class.type == Ncl_Typefloat ||
					    thetype->type_class.type == Ncl_Typedouble) {
						lret = asciifloat(end,&end,thetype->type_class.type,tmp_ptr,&rem);
					}
					else {
						lret = asciiinteger(end,&end,thetype->type_class.type,tmp_ptr,&rem);
					}
					if (rem) {
						/* dangling characters not fully parsed:
						 *  copy to the beginning of the buffer and try again
						 */
						for (j = 0; rem[j] != '\0'; j++) {
							buf[j] = rem[j];
						}
						count = fread(&(buf[j]),1,bufsize-j-1,fp);
						if (count <= 0) {
							if (lret) {
								/* since the last return was good, add it to the total */
								tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
								total++;
							}
							break;
						}
						buf[j + count] = '\0';
						/* now we're going to try again */
						end = buf;
						cp = strchr(buf,' ');
						while (cp != NULL) {
							*cp = '\n';
							cp = strchr(cp+1,' ');
						}
						continue;
					}
					if (lret) {
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
						total++;
					} else {
						continue;
					}
				}
				}
                                else {
					fprintf(stdout, "Need to re-think of handling ENUMERICLs. file: %s, line: %d\n", __FILE__, __LINE__);
				}
			}
			else if(thetype->type_class.type==Ncl_Typechar) {
				for(i = 0; ((i<totalsize) && !feof(fp)); i++) {
					*(char*)tmp_ptr = fgetc(fp);
					tmp_ptr = (void*)((char*)tmp_ptr+1);
					total++;
				}
			}
			else if(thetype->type_class.type==Ncl_Typestring) {
				char *buffer;
				char *step;
				int c;
				int cur_size = NCL_MAX_STRING;

				buffer = NclMalloc(cur_size * sizeof(char));
				step =buffer;
				for(i = 0; ((i<totalsize) && !feof(fp)); i++) {
					for(j = 0; ; j++) {
						if (j == cur_size) {
							cur_size *= 2;
							buffer = NclRealloc(buffer,cur_size * sizeof(char));
							step = &buffer[j];
						}
						if(!feof(fp)) {
							c  = getc(fp);
							*step = (char) c;
							switch (c) {
							default:
								step++;
								continue;
							case '\r':
								/* throw away next character if it's a CRLF sequence */
								c = getc(fp);
								if (c != '\n') { 
									ungetc(c,fp);
								}
								/* fall through */
							case EOF:
								/* no characters since last EOL + not fall through of '\r' */
								if (j == 0 && *step != '\r') {
									break;
								}
								/* fall through */
							case '\n':
								*step = '\0';
								*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
								step = buffer;
								tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
								total++;
								break;
							}
							break;
						} else {
							break;
						}
					}
				}
				NclFree(buffer);
			}
			else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Attempt to read unsupported type");
				return(NhlFATAL);
			}
			if(total < totalsize) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"asciiread: End of file reached and only (%d) elements were read from the file, filling remaining elements with the default missing value for the requested type",total);
				for(i = total;i<totalsize;i++) {
					memcpy(tmp_ptr,
					       &(thetype->type_class.default_mis),thetype->type_class.size);
					tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
				}
			}
			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = tmp_md;
			_NclPlaceReturn(data_out);
			fclose(fp);
			NclFree(dimsizes);
			return(ret);
		}
	} else {
		int total = 0;
		errno = 0;
		fp = fopen(path_string,"r");
		if (fp == NULL && errno) {
			NhlPError(NhlFATAL,errno,"asciiread: Unable to open file for reading");
			return(NhlFATAL);
		}
		else if (fp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Unable to open file for reading: check permissions");
			return(NhlFATAL);
		}
		tmp_ptr = NclMalloc(thetype->type_class.size);

		if (thetype->type_class.type & NCL_SNUMERIC_TYPE_MASK) {
			char *end = "";
			int lret;
			int count;
			char *rem;
			totalsize = 0;
			if (thetype->type_class.type & NCL_NUMERIC_TYPE_MASK) {
			while (1) {
				if (*end == '\0') {
					count = fread(buf,1,bufsize-1,fp);
					if (count <= 0) {
						break;
					}
					buf[count] = '\0';
					end = buf;
					cp = strchr(buf,' ');
					while (cp != NULL) {
						*cp = '\n';
						cp = strchr(cp+1,' ');
					}
				}
				rem = NULL;
				if (thetype->type_class.type == Ncl_Typefloat ||
				    thetype->type_class.type == Ncl_Typedouble) {
					lret = asciifloat(end,&end,thetype->type_class.type,tmp_ptr,&rem);
				}
				else {
					lret = asciiinteger(end,&end,thetype->type_class.type,tmp_ptr,&rem);
				}
				if (rem) {
					/* dangling characters not fully parsed:
					 *  copy to the beginning of the buffer and try again
					 */
					for (j = 0; rem[j] != '\0'; j++) {
						buf[j] = rem[j];
					}
					count = fread(&(buf[j]),1,bufsize-j-1,fp);
					if (count <= 0) {
						if (lret) {
							/* since the last return was good, add it to the total */
							totalsize++;
						}
						break;
					}
					buf[j + count] = '\0';
					/* now we're going to try again */
					end = buf;
					cp = strchr(buf,' ');
					while (cp != NULL) {
						*cp = '\n';
						cp = strchr(cp+1,' ');
					}
					continue;
				}
				if (lret) {
					totalsize++;
				} else {
					continue;
				}
			}
			}
                        else {
				fprintf(stdout, "Need to re-think of handling ENUMERICLs. file: %s, line: %d\n", __FILE__, __LINE__);
			}
		}
		else if(thetype->type_class.type==Ncl_Typechar) {
			stat(path_string,&statbuf);
			totalsize = statbuf.st_size;
		}
		else if(thetype->type_class.type==Ncl_Typestring) {
			char c;
			totalsize = 0;
			int has_chars = 0;
			while(!feof(fp)) {
				c = getc(fp);
				if (c == '\r') {
					totalsize++;
					c = getc(fp); /* handle CRLF (\r\n) style EOL by ignoring LF after CR */
					if (c != '\n') /* otherwise put it back */
						ungetc(c,fp);
					has_chars = 0;
				}
				else if(c == '\n' ) {
					totalsize++;
					has_chars = 0;
				} 
				else if (c != EOF) {
					has_chars = 1;
				}
			}
			if (has_chars) {  /* some characters at the end without a newline */
				totalsize++;
			}
		}
		NclFree(tmp_ptr);
		if (totalsize == 0) {
			fclose(fp);
			NhlPError(NhlWARNING,NhlEUNKNOWN,
				  "asciiread: No elements read from file, returning a single element with the default missing value for the requested type");
			tmp_ptr = NclMalloc(1*thetype->type_class.size);
			dimsizes[0] = 1;
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				tmp_ptr,
				NULL,
				n_dimensions,
				dimsizes,
				TEMPORARY,
				NULL,
				thetype);
			if(tmp_md == NULL) 
				return(NhlFATAL);
			memcpy(tmp_ptr,&(thetype->type_class.default_mis),thetype->type_class.size);
			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = tmp_md;
			_NclPlaceReturn(data_out);
			NclFree(dimsizes);
			return(ret);
		}
		tmp_ptr = NclMalloc(totalsize*thetype->type_class.size);
		dimsizes[0] = totalsize;
		fclose(fp);
		fp = fopen(path_string,"r");
			
		tmp_md = _NclCreateMultiDVal(
			NULL,
			NULL,
			Ncl_MultiDValData,
			0,
			tmp_ptr,
			&(thetype->type_class.default_mis),
			n_dimensions,
			dimsizes,
			TEMPORARY,
			NULL,
			thetype);
		if(tmp_md == NULL) 
			return(NhlFATAL);

		if (thetype->type_class.type & NCL_SNUMERIC_TYPE_MASK) {
			char *end = "";
			int lret;
			int count;
			char *rem;
			if (thetype->type_class.type & NCL_NUMERIC_TYPE_MASK) {
			while (total < totalsize) {
				if (*end == '\0') {
					count = fread(buf,1,bufsize-1,fp);
					if (count <= 0) {
						break;
					}
					buf[count] = '\0';
					end = buf;
					cp = strchr(buf,' ');
					while (cp != NULL) {
						*cp = '\n';
						cp = strchr(cp+1,' ');
					}
				}
				rem = NULL;
				if (thetype->type_class.type == Ncl_Typefloat ||
				    thetype->type_class.type == Ncl_Typedouble) {
					lret = asciifloat(end,&end,thetype->type_class.type,tmp_ptr,&rem);
				}
				else {
					lret = asciiinteger(end,&end,thetype->type_class.type,tmp_ptr,&rem);
				}
				if (rem) {
					/* dangling characters not fully parsed:
					 *  copy to the beginning of the buffer and try again
					 */
					for (j = 0; rem[j] != '\0'; j++) {
						buf[j] = rem[j];
					}
					count = fread(&(buf[j]),1,bufsize-j-1,fp);
					if (count <= 0) {
						if (lret) {
							/* since the last return was good, add it to the total */
							tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
							total++;
						}
						break;
					}
					buf[j + count] = '\0';
					/* now we're going to try again */
					end = buf;
					cp = strchr(buf,' ');
					while (cp != NULL) {
						*cp = '\n';
						cp = strchr(cp+1,' ');
					}
					continue;
				}
				if (lret) {
					tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
					total++;
				} else {
					continue;
				}
			}
			}
                        else {
				fprintf(stdout, "Need to re-think of handling ENUMERICLs. file: %s, line: %d\n", __FILE__, __LINE__);
			}
		}
		else if(thetype->type_class.type==Ncl_Typechar) {
			for(i = 0; ((i<totalsize) && !feof(fp)); i++) {
				*(char*)tmp_ptr = fgetc(fp);
				tmp_ptr = (void*)((char*)tmp_ptr+1);
				total++;
			}
		}
		else if(thetype->type_class.type==Ncl_Typestring) {
			char *buffer;
			char *step;
			int c;
			int cur_size = NCL_MAX_STRING;

			buffer = NclMalloc(cur_size * sizeof(char));
			step =buffer;
			for(i = 0; ((i<totalsize) && !feof(fp)); i++) {
				for(j = 0; ; j++) {
					if (j == cur_size) {
						cur_size *= 2;
						buffer = NclRealloc(buffer,cur_size * sizeof(char));
						step = &buffer[j];
					}
					if(!feof(fp)) {
						c = getc(fp);
						*step = (char) c;
						switch (c) {
						default:
							step++;
							continue;
						case '\r':
							/* throw away next character if it's a CRLF sequence */
							c = getc(fp);
							if (c != '\n') { 
								ungetc(c,fp);
							}
							/* fall through */
						case EOF:
							/* no characters since last EOL + not fall through of '\r' */
							if (j == 0 && *step != '\r') {
								break;
							}
							/* fall through */
						case '\n':
							*step = '\0';
							*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
							step = buffer;
							tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
							total++;
							break;
						}
						break;
					} else {
						break;
					}
				}
			}
			NclFree(buffer);
		}
		else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Attempt to read unsupported type");
			return(NhlFATAL);
		}


		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = tmp_md;
		_NclPlaceReturn(data_out);
		fclose(fp);
		NclFree(dimsizes);
		return(ret);
	} 
	return(NhlFATAL);
}

NhlErrorTypes _NclIchngdir
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;


	args  = _NclGetArg(0,1,DONT_CARE);
	switch(args.kind) {
	case NclStk_VAL:
		tmp_md = args.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"Function or procedure not implemented");
	return(NhlFATAL);
}

NhlErrorTypes _NclIfbindirwrite
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry value;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	void *tmp_ptr;
        FILE *fd;
	ng_size_t totalsize = 0;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	fpath = _NclGetArg(0,2,DONT_CARE);
	value = _NclGetArg(1,2,DONT_CARE);

	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
		if(path_string == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirwrite: An error in the file path was detected could not resolve file path");
			return(NhlFATAL);
		}
	}
	tmp_md = NULL;
	switch(value.kind){
	case NclStk_VAL:
		tmp_md = value.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(value.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		tmp_ptr = tmp_md->multidval.val;
		thetype = tmp_md->multidval.type;
		totalsize = tmp_md->multidval.totalelements * thetype->type_class.size;
		fd = fopen(path_string, "a");
		if(fd != NULL) {
			char *outdata = NULL;
			if (swap_bytes) {
				outdata = NclMalloc(totalsize);
				if (!outdata)
					return (NhlFATAL);
				_NclSwapBytes(outdata,tmp_ptr,tmp_md->multidval.totalelements,thetype->type_class.size);
                                tmp_ptr = outdata;
			}
                        size_t numout = fwrite(tmp_ptr, 1, totalsize, fd);
                        fclose(fd);
                        if (outdata)
                            NclFree(outdata);
                        if (numout == totalsize)
                            return(1);
                        NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirwrite: attempted to write %ld bytes, wrote %ld", 
                                totalsize, numout, NULL);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirwrite: Could not create file");
		}
	}
	return(NhlFATAL);
}
NhlErrorTypes _NclIcbinwrite
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry value;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	void *tmp_ptr;
	int fd = -1;
	ng_size_t  totalsize = 0;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	fpath = _NclGetArg(0,2,DONT_CARE);
	value = _NclGetArg(1,2,DONT_CARE);

	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
		if(path_string == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinwrite: An error in the file path was detected could not resolve file path");
			return(NhlFATAL);
		}
	}
	tmp_md = NULL;
	switch(value.kind){
	case NclStk_VAL:
		tmp_md = value.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(value.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md != NULL) {
		tmp_ptr = tmp_md->multidval.val;
		thetype = tmp_md->multidval.type;
		totalsize = tmp_md->multidval.totalelements * thetype->type_class.size;
		if (swap_bytes) {
			char *outdata = NclMalloc(totalsize);
			if (!outdata)
				return (NhlFATAL);
			_NclSwapBytes(outdata,tmp_ptr,tmp_md->multidval.totalelements,thetype->type_class.size);
			fd = open(path_string,(O_CREAT | O_RDWR),0644);
			if(fd >= 0) {
				ret = write(fd,outdata,totalsize);
				close(fd);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinwrite: Could not create file");
				ret = NhlFATAL;
			}
			NclFree(outdata);
			return(ret);
		}
		else {
			fd = open(path_string,(O_CREAT | O_RDWR),0644);
			if(fd >= 0) {
				ret = write(fd, tmp_ptr,totalsize);
				close(fd);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinwrite: Could not create file");
				ret = NhlFATAL;
			}
			return(ret);
		}
	}
	return(NhlFATAL);
}
NhlErrorTypes _NclIfbinwrite
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry fpath;
	NclStackEntry value;
	NclTypeClass thetype;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	void *tmp_ptr;
	FILE *fd;
	ng_size_t  totalsize = 0;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;
	long long ll_total;
	int marker_size = 4;

#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(NclQuark *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif
	if (8 == *(int *)(fcp->options[Ncl_RECORD_MARKER_SIZE].value->multidval.val)) {
		marker_size = 8;
	}

	fpath = _NclGetArg(0,2,DONT_CARE);
	value = _NclGetArg(1,2,DONT_CARE);

	switch(fpath.kind) {
	case NclStk_VAL:
		tmp_md = fpath.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(fpath.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinwrite: error retrieving the file path");
		return(NhlFATAL);
	}
	path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
	if(path_string == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinwrite: An error in the file path was detected could not resolve file path");
		return(NhlFATAL);
	}
	fd = fopen(path_string,"w+");
	if(fd == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,
			  "fbinwrite: could not open (%s) check path and permissions, can't continue",path_string);
		return(NhlFATAL);
	}
	tmp_md = NULL;
	switch(value.kind){
	case NclStk_VAL:
		tmp_md = value.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(value.u.data_var,NULL,NULL);
		break;
	default:
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return(NhlFATAL);
	}
	if(! tmp_md) {
		NhlPError(NhlFATAL,ENOMEM,NULL);
		return (NhlFATAL);
	}
	tmp_ptr = tmp_md->multidval.val;
	thetype = tmp_md->multidval.type;
	totalsize = tmp_md->multidval.totalelements * thetype->type_class.size;

	if(totalsize > INT_MAX && (marker_size != 8 || sizeof(ng_size_t) != 8)) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinwrite: cannot write more than 2 Gb values to a file.");
	  return(NhlFATAL);
	}

        char *outdata = NULL;
        ng_size_t marker_data = totalsize;         
        int i_marker_data = (int) totalsize;
        void *marker_rec = (marker_size == 4) ? &i_marker_data : &marker_data;

        if (swap_bytes) {
            outdata = NclMalloc(totalsize);           
            if (!outdata) {
                NhlPError(NhlFATAL,ENOMEM,"fbinwrite");
		return(NhlFATAL);
            }
            _NclSwapBytes(outdata,tmp_ptr,tmp_md->multidval.totalelements,thetype->type_class.size);
            tmp_ptr = outdata;
            _NclSwapBytes(marker_rec, marker_rec, 1, marker_size);                
        }
        
        ng_size_t numbytes = fwrite((void*)marker_rec, 1, marker_size, fd);
        numbytes += fwrite(tmp_ptr, 1, totalsize, fd);
        numbytes += fwrite((void*)marker_rec, 1, marker_size, fd);
        fclose(fd);
        if (outdata)
            NclFree(outdata);
        
        ng_size_t expectedbytes = totalsize + 2*marker_size;
        if (numbytes != expectedbytes) {
            NhlPError(NhlFATAL, NhlEUNKNOWN, "fbinwrite: expected to write %ld bytes, wrote %ld", 
                    totalsize, expectedbytes, NULL);
            return(NhlFATAL);
        }
        
	return(1);
}
NhlErrorTypes _NclIsleep
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args ;
	NclMultiDValData tmp_md= NULL;


	args  = _NclGetArg(0,1,DONT_CARE);
	switch(args.kind) {
	case NclStk_VAL:
		tmp_md = args.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	sleep(*(int*)tmp_md->multidval.val);
	return(NhlNOERROR);
}
NhlErrorTypes _NclIprompt
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;


	args  = _NclGetArg(0,1,DONT_CARE);
	switch(args.kind) {
	case NclStk_VAL:
		tmp_md = args.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	NhlPError(NhlFATAL,NhlEUNKNOWN,"Function or procedure not implemented");
	return(NhlFATAL);
}
NhlErrorTypes _NclIrand
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int tmp ;
	ng_size_t dimsize = 1;

/*
 * Generate a random number from 0 to 32766 inclusive.
 */
	tmp = (int) (32767.0*rand()/(RAND_MAX+1.0));
	
	return(NclReturnValue(
		&tmp,
		1,
		&dimsize,
		&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
		NCL_int,
		1
	));

}
NhlErrorTypes _NclIsrand
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;


	args  = _NclGetArg(0,1,DONT_CARE);
	switch(args.kind) {
	case NclStk_VAL:
		tmp_md = args.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if (tmp_md != NULL) {
		srand(*(int*)tmp_md->multidval.val);
		return(NhlNOERROR);
	} else {
		return(NhlFATAL);
	}
	
}

NhlErrorTypes _NclIabs
# if	NhlNeedProto
(void)
# else
()
# endif
{
    NclScalar   missing;
    int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];

    void    *out_val,
            *value;
    byte    *bout_val,
            *bvalue;
    short   *sout_val,
            *svalue;
    int     *iout_val,
            *ivalue;
    long    *lout_val,
            *lvalue;
    float   *fout_val,
            *fvalue;
    double  *dout_val,
            *dvalue;
    long long *llout_val,
              *llvalue;

    NclBasicDataTypes   type;
    ng_size_t total = 1;
    ng_size_t i;


    /* get input data */
    value = (void *) NclGetArgValue(
                0,
                1,
                &n_dims,
                dimsizes,
                &missing,
                &has_missing,
                &type,
                0);

    for (i = 0; i < n_dims; i++) {
        total *= dimsizes[i];
    }

    switch (type) {
        case NCL_float:
            fvalue = (float *) value;
            out_val = (void *) NclMalloc(total * sizeof(float));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            fout_val = (float *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (fvalue[i] != missing.floatval) {
                        fout_val[i] = (float) fabs((float) fvalue[i]);
                    } else {
                        fout_val[i] = (float) missing.floatval;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    fout_val[i] = (float) fabs((float) fvalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                (has_missing ? &missing : NULL), NCL_float, 0);
            break;

        case NCL_double:
            dvalue = (double *) value;
            out_val = (void *) NclMalloc(total * sizeof(double));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            dout_val = (double *) out_val;
            if (has_missing) {
                for (i = 0 ; i < total; i++) {
                    if (dvalue[i] != missing.doubleval) {
                        dout_val[i] = (double) fabs((double) dvalue[i]);
                    } else {
                        dout_val[i] = (double) missing.doubleval;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    dout_val[i] = (double) fabs((double) dvalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_double, 0);
            break;

        case NCL_int:
            ivalue = (int *) value;
            out_val = (void *) NclMalloc(total * sizeof(int));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            iout_val = (int *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (ivalue[i] != missing.intval) {
                        iout_val[i] = (int) abs((int) ivalue[i]);
                    } else {
                        iout_val[i] = missing.intval;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    iout_val[i] = (int) abs((int) ivalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_int, 0);
            break;

        case NCL_ushort:
            {
                unsigned short *pin, *pout;

                out_val = (void *) NclMalloc(total * sizeof(unsigned short));
		if(out_val == NULL) {
		  NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "abs: cannot allocate memory for output array");
		  return(NhlFATAL);
		}

                pin = (unsigned short *) value;
                pout = (unsigned short *) out_val;

                if (has_missing)
                {
                    for (i = 0; i < total; i++)
                    {
                        if (pin[i] != missing.ushortval)
                        {
                            pout[i] = (unsigned short) pin[i];
                        }
                        else
                        {
                            pout[i] = missing.ushortval;
                        }
                    }
                }
                else
                {
                    for (i = 0; i < total; i++)
                    {
                        pout[i] = pin[i];
                    }
                }

                return NclReturnValue(out_val, n_dims, dimsizes,
                        (has_missing ? &missing : NULL), NCL_ushort, 0);
            }
            break;

        case NCL_uint:
            {
                unsigned int *pin, *pout;

                out_val = (void *) NclMalloc(total * sizeof(unsigned int));
		if(out_val == NULL) {
		  NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "abs: cannot allocate memory for output array");
		  return(NhlFATAL);
		}

                pin = (unsigned int *) value;
                pout = (unsigned int *) out_val;

                if (has_missing)
                {
                    for (i = 0; i < total; i++)
                    {
                        if (pin[i] != missing.uintval)
                        {
                            pout[i] = (unsigned int) pin[i];
                        }
                        else
                        {
                            pout[i] = missing.uintval;
                        }
                    }
                }
                else
                {
                    for (i = 0; i < total; i++)
                    {
                        pout[i] = pin[i];
                    }
                }

                return NclReturnValue(out_val, n_dims, dimsizes,
                        (has_missing ? &missing : NULL), NCL_uint, 0);
            }
            break;

        case NCL_ulong:
            {
                unsigned long *pin, *pout;

                out_val = (void *) NclMalloc(total * sizeof(unsigned long));
		if(out_val == NULL) {
		  NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "abs: cannot allocate memory for output array");
		  return(NhlFATAL);
		}

                pin = (unsigned long *) value;
                pout = (unsigned long *) out_val;

                if (has_missing)
                {
                    for (i = 0; i < total; i++)
                    {
                        if (pin[i] != missing.ulongval)
                        {
                            pout[i] = (unsigned long) pin[i];
                        }
                        else
                        {
                            pout[i] = missing.ulongval;
                        }
                    }
                }
                else
                {
                    for (i = 0; i < total; i++)
                    {
                        pout[i] = pin[i];
                    }
                }

                return NclReturnValue(out_val, n_dims, dimsizes,
                        (has_missing ? &missing : NULL), NCL_ulong, 0);
            }
            break;

        case NCL_uint64:
            {
                unsigned long long *pin, *pout;

                out_val = (void *) NclMalloc(total * sizeof(unsigned long long));
		if(out_val == NULL) {
		  NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "abs: cannot allocate memory for output array");
		  return(NhlFATAL);
		}

                pin = (unsigned long long *) value;
                pout = (unsigned long long *) out_val;

                if (has_missing)
                {
                    for (i = 0; i < total; i++)
                    {
                        if (pin[i] != missing.uint64val)
                        {
                            pout[i] = (unsigned long long) pin[i];
                        }
                        else
                        {
                            pout[i] = missing.uint64val;
                        }
                    }
                }
                else
                {
                    for (i = 0; i < total; i++)
                    {
                        pout[i] = pin[i];
                    }
                }

                return NclReturnValue(out_val, n_dims, dimsizes,
                        (has_missing ? &missing : NULL), NCL_uint64, 0);
            }
            break;

        case NCL_short:
            svalue = (short *) value;
            out_val = (void *) NclMalloc(total * sizeof(short));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            sout_val = (short *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (svalue[i] != missing.shortval) {
                        sout_val[i] = (short) abs((short) svalue[i]);
                    } else {
                        sout_val[i] = missing.shortval;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    sout_val[i] = (short) abs((short) svalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_short, 0);
            break;

        case NCL_long:
            lvalue = (long *) value;
            out_val = (void *) NclMalloc(total * sizeof(long));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            lout_val = (long *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (lvalue[i] != missing.longval) {
                        lout_val[i] = (long) labs((long) lvalue[i]);
                    } else {
                        lout_val[i] = missing.longval;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    lout_val[i] = (long) labs((long) lvalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_long, 0);
            break;

        case NCL_int64:
            llvalue = (long long *) value;
            out_val = (void *) NclMalloc(total * sizeof(long long));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            llout_val = (long long *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (llvalue[i] != missing.int64val) {
		      /*
                       *llout_val[i] = (long long) llabs((long long) llvalue[i]);
		       */
                        llout_val[i] = _Ncl_llabs(llvalue[i]);
                    } else {
                        llout_val[i] = missing.int64val;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
		      /*
                       *llout_val[i] = (long long) llabs((long long) llvalue[i]);
		       */
                        llout_val[i] = _Ncl_llabs(llvalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_int64, 0);
            break;

        case NCL_byte:
            bvalue = (byte *) value;
            out_val = (void *) NclMalloc(total * sizeof(float));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL, NhlEUNKNOWN,
			"abs: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            bout_val = (byte *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (bvalue[i] != missing.byteval) {
                        bout_val[i] = (byte) abs((byte) bvalue[i]);
                    } else {
                        bout_val[i] = missing.byteval;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    bout_val[i] = (byte) abs((byte) bvalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_byte, 0);
            break;

        case NCL_ubyte:
            {
                unsigned char *pin, *pout;

                out_val = (void *) NclMalloc(total * sizeof(unsigned char));
		if(out_val == NULL) {
		  NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "abs: cannot allocate memory for output array");
		  return(NhlFATAL);
		}

                pin = (unsigned char *) value;
                pout = (unsigned char *) out_val;

                if (has_missing)
                {
                    for (i = 0; i < total; i++)
                    {
                        if (pin[i] != missing.ubyteval)
                        {
                            pout[i] = (unsigned char) pin[i];
                        }
                        else
                        {
                            pout[i] = missing.ubyteval;
                        }
                    }
                }
                else
                {
                    for (i = 0; i < total; i++)
                    {
                        pout[i] = pin[i];
                    }
                }

                return NclReturnValue(out_val, n_dims, dimsizes,
                        (has_missing ? &missing : NULL), NCL_ubyte, 0);
            }
            break;

        case NCL_char:
            {
                unsigned char *pin, *pout;

                out_val = (void *) NclMalloc(total * sizeof(unsigned char));
		if(out_val == NULL) {
		  NhlPError(NhlFATAL, NhlEUNKNOWN,
			    "abs: cannot allocate memory for output array");
		  return(NhlFATAL);
		}

                pin = (unsigned char *) value;
                pout = (unsigned char *) out_val;

                if (has_missing)
                {
                    for (i = 0; i < total; i++)
                    {
                        if (pin[i] != missing.charval)
                        {
                            pout[i] = (unsigned char) pin[i];
                        }
                        else
                        {
                            pout[i] = missing.charval;
                        }
                    }
                }
                else
                {
                    for (i = 0; i < total; i++)
                    {
                        pout[i] = pin[i];
                    }
                }

                return NclReturnValue(out_val, n_dims, dimsizes,
                        (has_missing ? &missing : NULL), NCL_char, 0);
            }
            break;

        default:
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "abs: a non-numeric type was passed to this function, cannot continue");
            break;
    }
    return NhlFATAL;
}


NhlErrorTypes _NclIncargversion
#if	NhlNeedProto
(void)
#else
()
#endif
{
	Const char *tmp = NULL;
	char *tmp2 = NULL;

	tmp = GetNCARGPath("bin");

	tmp2 = NclMalloc(strlen(tmp) + strlen("/ncargversion") + 1);
	strcpy(tmp2,tmp);
	strcat(tmp2,"/ncargversion");
	system(tmp2);	
	NclFree(tmp2);
	return(NhlNOERROR);
}
NhlErrorTypes _NclIncargpath
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;
	char *str;
	NclQuark outval;
	ng_size_t dimsize = 1;


	args  = _NclGetArg(0,1,DONT_CARE);
	switch(args.kind) {
	case NclStk_VAL:
		tmp_md = args.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(args.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	str = NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val);
	if(str != NULL) {
		outval = NrmStringToQuark(GetNCARGPath(str));
	} else {
		outval = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
	}
	return(NclReturnValue(
		&outval,
		1,
		&dimsize,
		&((NclTypeClass)nclTypestringClass)->type_class.default_mis,
		NCL_string,
		1
	));
	
}

NhlErrorTypes _NclIgetenv
# if NhlNeedProto
(void)
# else
()
# endif
{
    /* Local Arguments */
    NclStackEntry   args;
    NclMultiDValData    tmp_md= NULL;
    char    *str;
    char    *tmp;
    NclQuark  outval;
    ng_size_t dimsize = 1;


    /* get the environment variable */
    args  = _NclGetArg(0, 1, DONT_CARE);
    switch(args.kind) {
        case NclStk_VAL:
            tmp_md = args.u.data_obj;
            break;

        case NclStk_VAR:
            tmp_md = _NclVarValueRead(args.u.data_var, NULL, NULL);
            break;

        default:
            return(NhlFATAL);
    }

    /* convert to an NCL string and verify it exists */
    str = NrmQuarkToString(*(NclQuark *) tmp_md->multidval.val);
    if(str != NULL) {
        tmp = getenv(str);

        /* environment variable not set; return default missing value */
        if(tmp == NULL) {
            outval = ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
            return NclReturnValue(&outval, 1, &dimsize,
                    &((NclTypeClass) nclTypestringClass)->type_class.default_mis, NCL_string, 1);
        } else {
            outval = NrmStringToQuark(tmp);
            return NclReturnValue(&outval, 1, &dimsize, NULL, NCL_string, 1);
        }
    } else {
        outval = ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;
        return NclReturnValue(&outval, 1, &dimsize,
                &((NclTypeClass)nclTypestringClass)->type_class.default_mis, NCL_string, 1);
    }
}


/* 
 * this version recognizes hexadecimal and decimal conventions, but not octal
 */

NhlErrorTypes _NclIshorttoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	short *value;
	ng_size_t total_elements = 1;
	int n_dims = 0;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing, missing2;
	int has_missing;
	ng_size_t i;
	int *output;
	
        value = (short*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	for (i = 0; i < n_dims; i++) {
		total_elements *= dimsizes[i];
	}
	output = (int*)NclMalloc(sizeof(int)*total_elements);
        if (output == NULL)
        {
            NHLPERROR((NhlFATAL, errno, "shorttoint: memory allocation error."));
            return NhlFATAL;
        }
	for(i = 0; i < total_elements; i++) {
		output[i] = (int)((short*)value)[i];
	}
	if(has_missing) {
		missing2.intval = (int)*((short*)&missing);
	}
	return(NclReturnValue(
		(void*)output,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}

NhlErrorTypes _NclIushorttoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	unsigned short *value;
	ng_size_t total_elements = 1;
	int n_dims = 0;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing, missing2;
	int has_missing;
	ng_size_t i;
	int *output;
	
        value = (unsigned short*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	for (i = 0; i < n_dims; i++) {
		total_elements *= dimsizes[i];
	}
	output = NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size *total_elements);
        if (output == NULL)
        {
            NHLPERROR((NhlFATAL, errno, "ushorttoint output: memory allocation error."));
            return NhlFATAL;
        }
	for(i = 0; i < total_elements; i++) {
		output[i] = (int)((unsigned short*)value)[i];
	}
	if(has_missing) {
		missing2.intval = (int)*((unsigned short*)&missing);
	}
	return(NclReturnValue(
		(void*)output,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}


NhlErrorTypes _NclIinttoshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
    int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    short *out_val;
	NclBasicDataTypes type;
    int *value;
    ng_size_t total=1;
    ng_size_t i;

        value = (int*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeshortClass)->type_class.size *total);
	if(has_missing) {
		if (missing.intval < SHRT_MIN || missing.intval > SHRT_MAX) {
		        missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		}
		else {
			missing2.shortval = (short)missing.intval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < SHRT_MIN) ||(value[i] > SHRT_MAX)||(value[i] == missing.intval)) {
				out_val[i] = (short)missing2.shortval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	} else {
		missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		for(i = 0; i < total; i++) {
			if((value[i] < SHRT_MIN )||(value[i] > SHRT_MAX)) {
				has_missing = 1;
				out_val[i] = missing2.shortval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_short,
		0
	));
}

NhlErrorTypes _NclIinttobyte
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
    int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    byte *out_val;
	NclBasicDataTypes type;
    int *value;
    ng_size_t total=1;
    ng_size_t i;

        value = (int*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypebyteClass)->type_class.size *total);
	if(has_missing) {
		if (missing.intval < SCHAR_MIN || missing.intval > SCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.intval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)||(value[i] == missing.intval)) {
				out_val[i] = (byte)missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)) {
				out_val[i] = missing2.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_byte,
		0
	));
}


NhlErrorTypes _NclIinttochar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        int *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (int*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypecharClass)->type_class.size *total);
	if(has_missing) {
		if (missing.intval < 0 || missing.intval > UCHAR_MAX) {
		        missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		}
		else {
			missing2.charval = (unsigned char)missing.intval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.intval)) {
				out_val[i] = (unsigned char)missing2.charval;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	} else {
		missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
				has_missing = 1;
				out_val[i] = missing2.charval;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_char,
		0
	));
}

NhlErrorTypes _NclIchartoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (unsigned char*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size *total);
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(value[i] == missing.charval) {
				out_val[i] = (int)missing.charval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		missing2.intval = (int)missing.charval;
	} else {
		for(i = 0; i < total; i++) {
			out_val[i] = (int)value[i];
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}


NhlErrorTypes _NclIshorttobyte
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        short *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (short*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypebyteClass)->type_class.size *total);
	if(has_missing) {
		if (missing.shortval < SCHAR_MIN || missing.shortval > SCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.shortval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)||(value[i] == missing.shortval)) {
				out_val[i] = (byte)  missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}

	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;  
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)) {
				out_val[i] = missing2.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_byte,
		0
	));
}


NhlErrorTypes _NclIshorttochar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        short *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (short*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypecharClass)->type_class.size *total);
	if(has_missing) {
		if (missing.shortval < 0 || missing.shortval > UCHAR_MAX) {
		        missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		}
		else {
			missing2.charval = (unsigned char)missing.shortval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.shortval)) {
				out_val[i] = (unsigned char)missing2.charval;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	} else {
		missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
				out_val[i] = missing2.charval;
				has_missing = 1;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_char,
		0
	));
}

NhlErrorTypes _NclIchartoshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (unsigned char*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeshortClass)->type_class.size *total);
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(value[i] == missing.charval) {
				out_val[i] = (short) missing.charval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		missing2.shortval = (short)missing.charval;

	} else {
		for(i = 0; i < total; i++) {
			out_val[i] = (short)value[i];
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2: NULL),
		NCL_short,
		0
	));
}

NhlErrorTypes _NclIlongtoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        long *value;
        ng_size_t total=1;
        ng_size_t i;


        value = (long*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size *total);
	if(has_missing) {
		if (missing.longval < INT_MIN || missing.longval > INT_MAX) {
		        missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		}
		else {
			missing2.intval = (int)missing.longval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < INT_MIN)||(value[i] > INT_MAX)||(value[i] == missing.longval)) {
					
				out_val[i] = missing2.intval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
	} else {
		missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		for(i = 0; i < total; i++) {
			if((value[i] < INT_MIN)||(value[i] > INT_MAX)) {
				out_val[i] = missing2.intval;
				has_missing = 1;
			} else {
				out_val[i] = (int)value[i];
			}
		}
	}

	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}
NhlErrorTypes _NclIlongtoshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        long *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (long*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeshortClass)->type_class.size *total);
	if(has_missing) {
		if (missing.longval < SHRT_MIN || missing.longval > SHRT_MAX) {
		        missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		}
		else {
			missing2.shortval = (short)missing.longval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < SHRT_MIN)||(value[i] > SHRT_MAX)||(value[i] == missing.longval)) {
				out_val[i] = (short)missing2.shortval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	} else {
		missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		for(i = 0; i < total; i++) {
			if((value[i] < SHRT_MIN )||(value[i] > SHRT_MAX)) {
				out_val[i] = missing2.shortval;
				has_missing = 1;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_short,
		0
	));
}
NhlErrorTypes _NclIlongtobyte
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        long *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (long*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypebyteClass)->type_class.size *total);
	if(has_missing) {
		if (missing.longval < SCHAR_MIN || missing.longval > SCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (int)missing.longval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.longval)) {
				out_val[i] = (byte)missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)) {
				out_val[i] = missing2.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_byte,
		0
	));
}


NhlErrorTypes _NclIlongtochar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        long *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (long*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypecharClass)->type_class.size *total);
	if(has_missing) {
		if (missing.longval < 0 || missing.longval > UCHAR_MAX) {
		        missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		}
		else {
			missing2.charval = (unsigned char)missing.longval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.longval)) {
				out_val[i] = (unsigned char)missing2.charval;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	} else {
		missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
				out_val[i] = missing2.charval;
				has_missing = 1;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_char,
		0
	));
}

NhlErrorTypes _NclIchartolong
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (unsigned char*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypelongClass)->type_class.size *total);
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(value[i] == missing.charval) {
				out_val[i] = (long)missing.charval;
			} else {
				out_val[i] = (long)value[i];
			}
		}
		missing2.longval = (long)missing.charval;
	} else {
		for(i = 0; i < total; i++) {
			out_val[i] = (long)value[i];
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_long,
		0
	));
}

NhlErrorTypes _NclIfloattoshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        float *value;
        ng_size_t total=1;
        ng_size_t i;


        value = (float*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeshortClass)->type_class.size *total);
	if(has_missing) {
		if (missing.floatval < (float)SHRT_MIN || missing.floatval > (float)SHRT_MAX) {
		        missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		}
		else {
			missing2.shortval = (short)missing.floatval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (float)SHRT_MIN)||(value[i] > (float)SHRT_MAX)||(value[i] == missing.floatval)) {
				out_val[i] = (short)missing2.shortval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	} else {
		missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		for(i = 0; i < total; i++) {
			if((value[i] < (float)SHRT_MIN)||(value[i] > (float)SHRT_MAX)) {
				out_val[i] = missing2.shortval;
				has_missing = 1;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_short,
		0
	));
}
NhlErrorTypes _NclIfloattoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        float *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (float*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size *total);
	if(has_missing) {
		if (missing.floatval < (float)INT_MIN || missing.floatval > (float)INT_MAX) {
		        missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		}
		else {
			missing2.intval = (int)missing.floatval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (float)INT_MIN)||(value[i] > (float)INT_MAX)||(value[i] == missing.floatval)) {
				out_val[i] = (int)missing2.intval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
	} else {
		missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		for(i = 0; i < total; i++) {
			if((value[i] < (float)INT_MIN)||(value[i] > (float)INT_MAX)) {
				out_val[i] = missing2.intval;
				has_missing = 1;
			} else {
				out_val[i] = (int)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}
NhlErrorTypes _NclIfloattolong
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        float *value;
        ng_size_t total=1;
        ng_size_t i;


        value = (float*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypelongClass)->type_class.size *total);
	if(has_missing) {
		if (missing.floatval < (float)LONG_MIN || missing.floatval > (float)LONG_MAX) {
		        missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
		}
		else {
			missing2.longval = (long)missing.floatval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (float)LONG_MIN)||(value[i] > (float)LONG_MAX)||(value[i] == missing.floatval)) {
				out_val[i] = missing2.longval;
			} else {
				out_val[i] = (long)value[i];
			}
		}
	} else {
		missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
		for(i = 0; i < total; i++) {
			if((value[i] < (float)LONG_MIN)||(value[i] > (float)LONG_MAX)) {
				out_val[i] = missing2.longval;
				has_missing = 1;
			} else {
				out_val[i] = (long)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_long,
		0
	));
}
NhlErrorTypes _NclIfloattobyte
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        float *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (float*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypebyteClass)->type_class.size *total);
	if(has_missing) {
		if (missing.floatval < (float)SCHAR_MIN || missing.floatval > (float)SCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.floatval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)||(value[i] == missing.floatval)) {
				out_val[i] = missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < SCHAR_MIN)||(value[i] > SCHAR_MAX)) {
				out_val[i] = missing2.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_byte,
		0
	));
}


NhlErrorTypes _NclIfloattochar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        float *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (float*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypecharClass)->type_class.size *total);
	if(has_missing) {
		if (missing.floatval < (float)0 || missing.floatval > (float)UCHAR_MAX) {
		        missing2.charval = 
				((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		}
		else {
			missing2.charval = (unsigned char)missing.floatval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.floatval)) {
				out_val[i] = missing2.charval;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	} else {
		missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
				out_val[i] = missing2.charval;
				has_missing = 1;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_char,
		0
	));
}

NhlErrorTypes _NclIchartofloat
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        float *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (unsigned char*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypefloatClass)->type_class.size *total);

	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(value[i] == missing.charval) {
				out_val[i] = (float)missing.charval;
			} else {
				out_val[i] = (float)value[i];
			}
		}
		missing2.floatval = (float)missing.charval;
	} else {
		for(i = 0; i < total; i++) {
			out_val[i] = (float)value[i];
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_float,
		0
	));
}
NhlErrorTypes _NclIdoubletobyte
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        double *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (double*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypebyteClass)->type_class.size *total);
	if(has_missing) {
		if (missing.doubleval < (double)SCHAR_MIN || missing.doubleval > (double)SCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (double)SCHAR_MIN)||(value[i] > (double)SCHAR_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < (double)SCHAR_MIN)||(value[i] > (double)SCHAR_MAX)) {
				out_val[i] = missing2.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_byte,
		0
	));
}


NhlErrorTypes _NclIdoubletochar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        double *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (double*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypecharClass)->type_class.size *total);
	if(has_missing) {
		if (missing.doubleval < (double)0 || missing.doubleval > (double)UCHAR_MAX) {
		        missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		}
		else {
			missing2.charval = (unsigned char)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (double)0)||(value[i] > (double)UCHAR_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = (unsigned char)missing2.charval;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	} else {
		missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
		for(i = 0; i < total; i++) {
			if((value[i] <(double)0)||(value[i] > (double)UCHAR_MAX)) {
				out_val[i] = missing2.charval;
				has_missing = 1;
			} else {
				out_val[i] = (unsigned char)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_char,
		0
	));
}

NhlErrorTypes _NclIchartodouble
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        double *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        ng_size_t total=1;
        ng_size_t i;

        value = (unsigned char*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypedoubleClass)->type_class.size *total);
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(value[i] == missing.charval) {
				out_val[i] = (double)missing.charval;
			} else {
				out_val[i] = (double)value[i];
			}
		}
		missing2.doubleval = (double)missing.charval;
	} else {
		for(i = 0; i < total; i++) {
			out_val[i] = (double)value[i];
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_double,
		0
	));
}
NhlErrorTypes _NclIdoubletoshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        double *value;
        ng_size_t total=1;
        ng_size_t i;


        value = (double*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeshortClass)->type_class.size *total);
	if(has_missing) {
		if (missing.doubleval < (double)SHRT_MIN || missing.doubleval > (double)SHRT_MAX) {
		        missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		}
		else {
			missing2.shortval = (short)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (double)SHRT_MIN)||(value[i] > (double)SHRT_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = (short)missing2.shortval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	} else {
		missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		for(i = 0; i < total; i++) {
			if((value[i] < (double)SHRT_MIN)||(value[i] > (double)SHRT_MAX)) {
				out_val[i] = missing2.shortval;
				has_missing = 1;
			} else {
				out_val[i] = (short)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_short,
		0
	));
}
NhlErrorTypes _NclIdoubletoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        double *value;
        ng_size_t total=1;
        ng_size_t i;


        value = (double*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size *total);
	if(has_missing) {
		if (missing.doubleval < (double)INT_MIN || missing.doubleval > (double)INT_MAX) {
		        missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		}
		else {
			missing2.intval = (int)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (double)INT_MIN)||(value[i] > (double)INT_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = (int)missing2.intval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
	} else {
		missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		for(i = 0; i < total; i++) {
			if((value[i] < (double)INT_MIN)||(value[i] > (double)INT_MAX)) {
				out_val[i] = missing2.intval;
				has_missing = 1;
			} else {
				out_val[i] = (int)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}
NhlErrorTypes _NclIdoubletolong
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        double *value;
        ng_size_t total=1;
        ng_size_t i;


        value = (double*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypelongClass)->type_class.size *total);
	if(has_missing) {
		if (missing.doubleval < (double)LONG_MIN || missing.doubleval > (double)LONG_MAX) {
		        missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
		}
		else {
			missing2.longval = (long)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < (double)LONG_MIN)||(value[i] > (double)LONG_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = missing2.longval;
			} else {
				out_val[i] = (long)value[i];
			}
		}
	} else {
		missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
		for(i = 0; i < total; i++) {
			if((value[i] < (double)LONG_MIN)||(value[i] > (double)LONG_MAX)) {
				out_val[i] = missing2.longval;
				has_missing = 1;
			} else {
				out_val[i] = (long)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_long,
		0
	));




}
NhlErrorTypes _NclIdoubletofloat
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
    int n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        float *out_val;
	NclBasicDataTypes type;
        double *value;
	double dtmp;
        ng_size_t total=1;
        ng_size_t i;

        value = (double*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = NclMalloc(((NclTypeClass)nclTypefloatClass)->type_class.size *total);
	if(has_missing) {
		if (fabs(missing.doubleval) < (double)FLT_MIN || fabs(missing.doubleval) > (double)FLT_MAX) {
		        missing2.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
		}
		else {
			missing2.floatval = (float)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			dtmp = fabs(value[i]);
			if((dtmp > (double)FLT_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = missing2.floatval;
			}
			else if (dtmp < (double)FLT_MIN) {
				out_val[i] = 0.0;
			}
			else {
				out_val[i] = (float)value[i];
			}
		}
	} else {
		missing2.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
		for(i = 0; i < total; i++) {
			dtmp = fabs(value[i]);
			if(dtmp > (double)FLT_MAX) {
				out_val[i] = missing2.floatval;
				has_missing = 1;
			}
			else if (dtmp < (double)FLT_MIN) {
				out_val[i] = 0.0;
			}
			else {
				out_val[i] = (float)value[i];
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_float,
		0
	));
}

NhlErrorTypes _NclIstringtolong
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        NclQuark *value;
        ng_size_t total=1;
        ng_size_t i;
	long tval;
	char *val;
	char *end;

        value = (NclQuark*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = (long*) NclMalloc(((NclTypeClass)nclTypelongClass)->type_class.size *total);
	if(has_missing) {
		errno = 0;
		val = NrmQuarkToString(missing.stringval);
		tval = _Nclstrtol(val,&end);
		if (end == val || errno == ERANGE) {
			missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
		}
		else {
			missing2.longval = tval;
		}
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.longval;
			} else {
				errno = 0;
				val = NrmQuarkToString(value[i]);
				tval = _Nclstrtol(val,&end);
				if (end == val) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,
					"stringtolong: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                        out_val[i] = missing2.longval;
				}
				else if (errno == ERANGE) {
                                        out_val[i] = missing2.longval;
				}
				else {
					out_val[i] = tval;
				}
			}
		}
	}  else {
		missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
		for(i = 0; i < total; i++) {
			errno = 0;
			val = NrmQuarkToString(value[i]);
			tval = _Nclstrtol(val,&end);
			if (end == val) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "stringtolong: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                has_missing = 1;
				out_val[i] = missing2.longval;
			}
			else if (errno == ERANGE) {
                                has_missing = 1;
				out_val[i] = missing2.longval;
			}
			else {
				out_val[i] = tval;
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_long,
		0
	));
}

NhlErrorTypes _NclIstringtoulong
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclScalar missing,missing2;
    int has_missing,n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    unsigned long *out_val;
    NclBasicDataTypes type;
    NclQuark *value;
    ng_size_t total=1;
    ng_size_t i;
    unsigned long tval;
    char *val;
    char *end;

    value = (NclQuark*)NclGetArgValue(
                    0,
                    1,
                    &n_dims,
                    dimsizes,
                    &missing,
                    &has_missing,
                    &type,
                    0);

    for(i = 0; i < n_dims; i++)
    {
        total *= dimsizes[i];
    }

    out_val = (unsigned long*) NclMalloc(((NclTypeClass)nclTypeulongClass)->type_class.size *total);

    if(has_missing)
    {
        errno = 0;
        val = NrmQuarkToString(missing.stringval);
        tval = _Nclstrtoul(val,&end);

        if (end == val || errno == ERANGE)
        {
            missing2.ulongval = ((NclTypeClass)nclTypeulongClass)->type_class.default_mis.ulongval;
        }
        else
        {
            missing2.ulongval = tval;
        }

        for(i = 0; i < total; i++)
        {
            if(missing.stringval == value[i])
            {
                out_val[i] = missing2.ulongval;
            }
            else
            {
                errno = 0;
                val = NrmQuarkToString(value[i]);
                tval = _Nclstrtoul(val,&end);
                if (end == val)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "stringtoulong: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                    out_val[i] = missing2.ulongval;
                }
                else if (errno == ERANGE)
                {
                    out_val[i] = missing2.ulongval;
                }
                else
                {
                    out_val[i] = tval;
                }
            }
        }
    }
    else
    {
        missing2.ulongval = ((NclTypeClass)nclTypeulongClass)->type_class.default_mis.ulongval;

        for(i = 0; i < total; i++)
        {
            errno = 0;
            val = NrmQuarkToString(value[i]);
            tval = _Nclstrtoul(val,&end);

            if (end == val)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                "stringtoulong: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                has_missing = 1;
                out_val[i] = missing2.ulongval;
            }
            else if (errno == ERANGE)
            {
                has_missing = 1;
                out_val[i] = missing2.ulongval;
            }
            else
            {
                out_val[i] = tval;
            }
        }
    }

    return(NclReturnValue(
            (void*)out_val,
            n_dims,
            dimsizes,
            (has_missing ? &missing2 : NULL),
            NCL_ulong,
            0
    ));
}

NhlErrorTypes _NclIstringtoint64
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclScalar missing,missing2;
    int has_missing,n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    long long *out_val;
    NclBasicDataTypes type;
    NclQuark *value;
    ng_size_t total=1;
    ng_size_t i;
    long long tval;
    char *val;
    char *end;

    value = (NclQuark*)NclGetArgValue(
                    0,
                    1,
                    &n_dims,
                    dimsizes,
                    &missing,
                    &has_missing,
                    &type,
                    0);

    for(i = 0; i < n_dims; i++)
    {
        total *= dimsizes[i];
    }

    out_val = (long long*) NclMalloc(((NclTypeClass)nclTypeint64Class)->type_class.size *total);

    if(has_missing)
    {
        val = NrmQuarkToString(missing.stringval);
        tval = _Nclstrtoll(val,&end);

        if (end == val || errno == ERANGE)
        {
            missing2.int64val = ((NclTypeClass)nclTypeint64Class)->type_class.default_mis.int64val;
        }
        else
        {
            missing2.int64val = tval;
        }

        for(i = 0; i < total; i++)
        {
            if(missing.stringval == value[i])
            {
                out_val[i] = missing2.int64val;
            }
            else
            {
                val = NrmQuarkToString(value[i]);
                tval = _Nclstrtoll(val,&end);
                if (end == val)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "stringtoint64: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                    out_val[i] = missing2.int64val;
                }
                else if (errno == ERANGE)
                {
                    out_val[i] = missing2.int64val;
                }
                else
                {
                    out_val[i] = tval;
                }
            }
        }
    }
    else
    {
        missing2.int64val = ((NclTypeClass)nclTypeint64Class)->type_class.default_mis.int64val;

        for(i = 0; i < total; i++)
        {
            val = NrmQuarkToString(value[i]);
            tval = _Nclstrtoll(val,&end);
            if (end == val)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                "stringtoint64: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                has_missing = 1;
                out_val[i] = missing2.int64val;
            }
            else if (errno == ERANGE)
            {
                has_missing = 1;
                out_val[i] = missing2.int64val;
            }
            else
            {
                out_val[i] = tval;
            }
        }
    }

    return(NclReturnValue(
            (void*)out_val,
            n_dims,
            dimsizes,
            (has_missing ? &missing2 : NULL),
            NCL_int64,
            0
    ));
}

NhlErrorTypes _NclIstringtouint64
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclScalar missing,missing2;
    int has_missing,n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    unsigned long long *out_val;
    NclBasicDataTypes type;
    NclQuark *value;
    ng_size_t total=1;
    ng_size_t i;
    unsigned long long tval;
    char *val;
    char *end;

    value = (NclQuark*)NclGetArgValue(
                    0,
                    1,
                    &n_dims,
                    dimsizes,
                    &missing,
                    &has_missing,
                    &type,
                    0);

    for(i = 0; i < n_dims; i++)
    {
        total *= dimsizes[i];
    }

    out_val = (unsigned long long*) NclMalloc(((NclTypeClass)nclTypeuint64Class)->type_class.size *total);

    if(has_missing)
    {
        errno = 0;
        val = NrmQuarkToString(missing.stringval);
        tval = _Nclstrtoull(val,&end);

        if (end == val || errno == ERANGE)
        {
            missing2.uint64val = ((NclTypeClass)nclTypeuint64Class)->type_class.default_mis.uint64val;
        }
        else
        {
            missing2.uint64val = tval;
        }

        for(i = 0; i < total; i++)
        {
            if(missing.stringval == value[i])
            {
                out_val[i] = missing2.uint64val;
            }
            else
            {
                errno = 0;
                val = NrmQuarkToString(value[i]);
                tval = _Nclstrtoull(val,&end);
                if (end == val)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "stringtouint64: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                    out_val[i] = missing2.uint64val;
                }
                else if (errno == ERANGE)
                {
                    out_val[i] = missing2.uint64val;
                }
                else
                {
                    out_val[i] = tval;
                }
            }
        }
    }
    else
    {
        missing2.uint64val = ((NclTypeClass)nclTypeuint64Class)->type_class.default_mis.uint64val;

        for(i = 0; i < total; i++)
        {
            errno = 0;
            val = NrmQuarkToString(value[i]);
            tval = _Nclstrtoull(val,&end);

            if (end == val)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                "stringtouint64: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                has_missing = 1;
                out_val[i] = missing2.uint64val;
            }
            else if (errno == ERANGE)
            {
                has_missing = 1;
                out_val[i] = missing2.uint64val;
            }
            else
            {
                out_val[i] = tval;
            }
        }
    }

    return(NclReturnValue(
            (void*)out_val,
            n_dims,
            dimsizes,
            (has_missing ? &missing2 : NULL),
            NCL_uint64,
            0
    ));
}

NhlErrorTypes _NclIstringtoshort
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        NclQuark *value;
        ng_size_t total=1;
        ng_size_t i;
	long tval;
	char *val;
	char *end;

        value = (NclQuark*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = (short*) NclMalloc(((NclTypeClass)nclTypeshortClass)->type_class.size *total);
	if(has_missing) {
		errno = 0;
		val = NrmQuarkToString(missing.stringval);
		tval = _Nclstrtol(val,&end);
		if (end == val || tval < SHRT_MIN || tval > SHRT_MAX || errno == ERANGE) {
			missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		}
		else {
			missing2.shortval = (short) tval;
		}
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.shortval;
			} else {
				val = NrmQuarkToString(value[i]);
				tval = _Nclstrtol(val,&end);
				if (end == val) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                        "stringtoshort: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                        out_val[i] = missing2.shortval;
				}
				else if (tval > SHRT_MAX || tval < SHRT_MIN) {
                                        out_val[i] = missing2.shortval;
				}
				else {
					out_val[i] = (short)tval;
				}
			}
		}
	}  else {
		missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
		for(i = 0; i < total; i++) {
			val = NrmQuarkToString(value[i]);
			tval = _Nclstrtol(val,&end);
			if (end == val) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "stringtoshort: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                has_missing = 1;
				out_val[i] = missing2.shortval;
			}
			else if (tval > SHRT_MAX || tval < SHRT_MIN) {
                                has_missing = 1;
				out_val[i] = missing2.shortval;
			}
			else {
				out_val[i] = (short)tval;
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_short,
		0
	));
}

NhlErrorTypes _NclIstringtoushort
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing,n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        unsigned short *out_val;
	NclBasicDataTypes type;
        NclQuark *value;
        ng_size_t total=1;
        ng_size_t i;
	long tval;
	char *val;
	char *end;

        value = (NclQuark*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = (unsigned short*) NclMalloc(((NclTypeClass)nclTypeushortClass)->type_class.size *total);
	if(has_missing) {
		errno = 0;
		val = NrmQuarkToString(missing.stringval);
		tval = _Nclstrtol(val,&end);
		if (end == val || tval < SHRT_MIN || tval > SHRT_MAX || errno == ERANGE) {
			missing2.ushortval = ((NclTypeClass)nclTypeushortClass)->type_class.default_mis.ushortval;
		}
		else {
			missing2.ushortval = (unsigned short) tval;
		}
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.ushortval;
			} else {
				val = NrmQuarkToString(value[i]);
				tval = _Nclstrtol(val,&end);
				if (end == val) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,
                                        "stringtoushort: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                        out_val[i] = missing2.ushortval;
				}
				else if (tval > SHRT_MAX || tval < SHRT_MIN) {
                                        out_val[i] = missing2.ushortval;
				}
				else {
					out_val[i] = (unsigned short)tval;
				}
			}
		}
	}  else {
		missing2.ushortval = ((NclTypeClass)nclTypeushortClass)->type_class.default_mis.ushortval;
		for(i = 0; i < total; i++) {
			val = NrmQuarkToString(value[i]);
			tval = _Nclstrtol(val,&end);
			if (end == val) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "stringtoushort: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                has_missing = 1;
				out_val[i] = missing2.ushortval;
			}
			else if (tval > SHRT_MAX || tval < SHRT_MIN) {
                                has_missing = 1;
				out_val[i] = missing2.ushortval;
			}
			else {
				out_val[i] = (unsigned short)tval;
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_ushort,
		0
	));
}


NhlErrorTypes _NclIstringtointeger
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        NclQuark *value;
        ng_size_t total=1;
        ng_size_t i;
	long tval;
	char *val;
	char *end;

        value = (NclQuark*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = (int*) NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size *total);
	if(has_missing) {
		errno = 0;
		val = NrmQuarkToString(missing.stringval);
		tval = _Nclstrtol(val,&end);
		if (end == val || tval < INT_MIN || tval > INT_MAX || errno == ERANGE) {
			missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		}
		else {
			missing2.intval = (int) tval;
		}
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.intval;
			} else {
				errno = 0;
				val = NrmQuarkToString(value[i]);
				tval = _Nclstrtol(val,&end);
				if (end == val) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,
                                        "stringtointeger: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                        out_val[i] = missing2.intval;
				}
				else if (tval > INT_MAX || tval < INT_MIN || errno == ERANGE) {
                                        out_val[i] = missing2.intval;
				}
				else {
					out_val[i] = (int)tval;
				}
			}
		}
	}  else {
		missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
		for(i = 0; i < total; i++) {
			errno = 0;
			val = NrmQuarkToString(value[i]);
			tval = _Nclstrtol(val,&end);
			if (end == val) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "stringtointeger: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                has_missing = 1;
				out_val[i] = missing2.intval;
			}
			else if (tval > INT_MAX || tval < INT_MIN || errno == ERANGE) {
                                has_missing = 1;
				out_val[i] = missing2.intval;
			}
			else {
				out_val[i] = (int)tval;
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_int,
		0
	));
}

NhlErrorTypes _NclIstringtouint
#if     NhlNeedProto
(void)
#else
()
#endif
{
    NclScalar missing,missing2;
    int has_missing,n_dims;
    ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
    unsigned int *out_val;
    NclBasicDataTypes type;
    NclQuark *value;
    ng_size_t total=1;
    ng_size_t i;
    unsigned int tval;
    char *val;
    char *end;

    value = (NclQuark*)NclGetArgValue(
                    0,
                    1,
                    &n_dims,
                    dimsizes,
                    &missing,
                    &has_missing,
                    &type,
                    0);

    for(i = 0; i < n_dims; i++)
    {
        total *= dimsizes[i];
    }

    out_val = (unsigned int*) NclMalloc(((NclTypeClass)nclTypeuintClass)->type_class.size *total);

    if(has_missing)
    {
        errno = 0;
        val = NrmQuarkToString(missing.stringval);
        tval = (unsigned int) _Nclstrtoul(val,&end);

        if (end == val || errno == ERANGE)
        {
            missing2.uintval = ((NclTypeClass)nclTypeuintClass)->type_class.default_mis.uintval;
        }
        else
        {
            missing2.uintval = tval;
        }

        for(i = 0; i < total; i++)
        {
            if(missing.stringval == value[i])
            {
                out_val[i] = missing2.uintval;
            }
            else
            {
                errno = 0;
                val = NrmQuarkToString(value[i]);
                tval = (unsigned int) _Nclstrtoul(val,&end);
                if (end == val)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "stringtouint: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                    out_val[i] = missing2.uintval;
                }
                else if (errno == ERANGE)
                {
                    out_val[i] = missing2.uintval;
                }
                else
                {
                    out_val[i] = tval;
                }
            }
        }
    }
    else
    {
        missing2.uintval = ((NclTypeClass)nclTypeuintClass)->type_class.default_mis.uintval;

        for(i = 0; i < total; i++)
        {
            errno = 0;
            val = NrmQuarkToString(value[i]);
            tval = (unsigned int) _Nclstrtoul(val,&end);

            if (end == val)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                "stringtouint: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                has_missing = 1;
                out_val[i] = missing2.uintval;
            }
            else if (errno == ERANGE)
            {
                has_missing = 1;
                out_val[i] = missing2.uintval;
            }
            else
            {
                out_val[i] = tval;
            }
        }
    }

    return(NclReturnValue(
            (void*)out_val,
            n_dims,
            dimsizes,
            (has_missing ? &missing2 : NULL),
            NCL_uint,
            0
    ));
}

static void TransD2E
#if     NhlNeedProto
	(char *val)
#else
(val)
char *val;
#endif
{
	char *cp;
	for (cp = val; *cp != '\0'; cp++) {
		if (! (*cp == 'd' || *cp == 'D'))
			continue;
		*cp = 'e';
		return;
	}
}


NhlErrorTypes _NclIstringtodouble
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        double *out_val;
	NclBasicDataTypes type;
        NclQuark *value;
        ng_size_t total=1;
        ng_size_t i;
	double tval;
	char tbuf[128];
	char *val;
	char *end;
	int bufsiz = 128;

        value = (NclQuark*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = (double*) NclMalloc(((NclTypeClass)nclTypedoubleClass)->type_class.size *total);
	/*
	 * stringtodouble is now enhanced to recognize 'd' or 'D' as
	 * indicators of an exponent in the double context. However
	 * note that when changing 'd' to 'e' in the context of 
	 * Quarks that you cannot simply change the string pointed to
	 * by the Quark. In the case of string variables that would cause
	 * the variable itself to change -- no good -- so the string must
	 * be copied.
	 */
	if(has_missing) {
		errno = 0;
		strncpy(tbuf,NrmQuarkToString(missing.stringval),bufsiz-1);
		val = tbuf;
		TransD2E(val);
		tval = strtod(val,&end);
		if (end == val || errno == ERANGE) {
			missing2.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
		}
		else {
			missing2.doubleval =  tval;
		}
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.doubleval;
			} else {
				errno = 0;
				strncpy(tbuf,NrmQuarkToString(value[i]),bufsiz-1);
				val = tbuf;
				/*
				val = NrmQuarkToString(value[i]);
				*/
				TransD2E(val);
				tval = strtod(val,&end);
				if (end == val) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,
					"stringtodouble: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                        out_val[i] = missing2.doubleval;
				}
				else if (errno == ERANGE) {
                                        out_val[i] = missing2.doubleval;
				}
				else {
					out_val[i] = tval;
				}
			}
		}
	}  else {
		missing2.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
		for(i = 0; i < total; i++) {
			errno = 0;
			strncpy(tbuf,NrmQuarkToString(value[i]),bufsiz-1);
			val = tbuf;
			/*
			val = NrmQuarkToString(value[i]);
			*/
			TransD2E(val);
			tval = strtod(val,&end);
			if (end == val) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "stringtodouble: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                has_missing = 1;
				out_val[i] = missing2.doubleval;
			}
			else if (errno == ERANGE) {
                                has_missing = 1;
                                out_val[i] = missing2.doubleval;

			}
			else {
				out_val[i] = tval;
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_double,
		0
	));
}

NhlErrorTypes _NclIstringtofloat
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing;
	int n_dims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        float *out_val;
	NclBasicDataTypes type;
        NclQuark *value;
        ng_size_t total=1;
        ng_size_t i;
	double tval,dtest;
	char *val;
	char *end;

        value = (NclQuark*)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);
        for(i = 0; i < n_dims; i++) {
                total *= dimsizes[i];
        }
	out_val = (float*)NclMalloc(((NclTypeClass)nclTypefloatClass)->type_class.size *total);
	if(has_missing) {
		errno = 0;
		val = NrmQuarkToString(missing.stringval);
		tval = strtod(val,&end);
		dtest = fabs(tval);
		if (end == val || errno == ERANGE || dtest > (double) FLT_MAX) {
			missing2.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
		}
		else if (dtest < (double) FLT_MIN) {
			missing2.floatval = 0.0;
		}
		else {
			missing2.floatval = (float) tval;
		}
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.floatval;
			} else {
				errno = 0;
				val = NrmQuarkToString(value[i]);
				tval = strtod(val,&end);
				dtest = fabs(tval);
				if (end == val) {
                                        NhlPError(NhlWARNING,NhlEUNKNOWN,
					"stringtofloat: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
                                        out_val[i] = missing2.floatval;
				}
				else if (errno == ERANGE || dtest > (double) FLT_MAX) {
                                        out_val[i] = missing2.floatval;
				}
				else if (dtest < (double) FLT_MIN) {
					out_val[i] = 0.0;
				}
				else {
					out_val[i] = tval;
				}
			}
		}
	}  else {
		missing2.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
		for(i = 0; i < total; i++) {
			errno = 0;
			val = NrmQuarkToString(value[i]);
			tval = strtod(val,&end);
			dtest = fabs(tval);
			if (end == val) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,
					  "stringtofloat: a bad value was passed; input strings must contain numeric digits, replacing with missing value");
				has_missing = 1;
				out_val[i] = missing2.floatval;
			}
			else if (errno == ERANGE || dtest > (double) FLT_MAX) {
				has_missing = 1;
				out_val[i] = missing2.floatval;
			}
			else if (dtest < (double) FLT_MIN) {
				out_val[i] = 0.0;
			}
			else {
				out_val[i] = tval;
			}
		}
	}
	return(NclReturnValue(
		(void*)out_val,
		n_dims,
		dimsizes,
		(has_missing ? &missing2 : NULL),
		NCL_float,
		0
	));
}
NhlErrorTypes _NclIstringtochar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclStackEntry data_out;
	NclMultiDValData tmp_md;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAL:
		tmp_md = data.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md == NULL) {
		return(NhlFATAL);
	}
	data_out.kind = NclStk_VAL;
	data_out.u.data_obj = _NclStringMdToCharMd(tmp_md);
	if(data_out.u.data_obj == NULL) {
		return(NhlFATAL);
	} 
	_NclPlaceReturn(data_out);
	return(NhlNOERROR);
}
NhlErrorTypes _NclIchartostring
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclStackEntry data_out;
	NclMultiDValData tmp_md;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAL:
		tmp_md = data.u.data_obj;
		break;
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	default:
		return(NhlFATAL);
	}
	if(tmp_md == NULL) {
		return(NhlFATAL);
	}
	data_out.kind = NclStk_VAL;
	data_out.u.data_obj = _NclCharMdToStringMd(tmp_md);
	if(data_out.u.data_obj == NULL) {
		return(NhlFATAL);
	} 
	_NclPlaceReturn(data_out);
	return(NhlNOERROR);
}

NhlErrorTypes _NclIIsProc
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg;
	NclMultiDValData tmp_md;
	ng_size_t  i;
	logical *outval;
	NclQuark *vals;
	NclSymbol* s;

	
	arg  = _NclGetArg(0,1,DONT_CARE);
	switch(arg.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
	}

	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_md->multidval.totalelements);
	vals = (NclQuark*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(vals[i] != tmp_md->multidval.missing_value.value.stringval) {
				s = _NclLookUp(NrmQuarkToString(vals[i]));
				if( s != NULL){
					switch(s->type) {
					case NPROC:
					case IPROC:
						outval[i] = 1;
						break;
					default:
						outval[i] = 0;
						break;
					}
				} else {
					outval[i] = 0;
				}
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			s = _NclLookUp(NrmQuarkToString(vals[i]));
			if( s != NULL){
				switch(s->type) {
				case NPROC:
				case IPROC:
					outval[i] = 1;
					break;
				default:
					outval[i] = 0;
					break;
				}
			} else {
				outval[i] = 0;
			}
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}

NhlErrorTypes _NclIExit
#if     NhlNeedProto
(void)
#else
()
#endif
{
	_NclExit(0);
	return(NhlNOERROR);
}

NhlErrorTypes _NclIStatusExit
#if     NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry   data;
	NclMultiDValData   tmp_md = NULL;
	int exit_status;

	data = _NclGetArg(0, 1, DONT_CARE);
	switch (data.kind) {
        case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var, NULL, NULL);
		break;

        case NclStk_VAL:
		tmp_md = (NclMultiDValData) data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	if (tmp_md == NULL)
		return NhlFATAL;

	exit_status = *(int *) tmp_md->multidval.val;

	_NclExit(exit_status);

	return NhlNOERROR;
}

NhlErrorTypes _NclIIsFunc
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg;
	NclMultiDValData tmp_md;
	ng_size_t  i;
	logical *outval;
	NclQuark *vals;
	NclSymbol* s;

	
	arg  = _NclGetArg(0,1,DONT_CARE);
	switch(arg.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_md->multidval.totalelements);
	vals = (NclQuark*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(vals[i] != tmp_md->multidval.missing_value.value.stringval) {
				s = _NclLookUp(NrmQuarkToString(vals[i]));
				if( s != NULL){
					switch(s->type) {
					case NFUNC:
					case IFUNC:
						outval[i] = 1;
						break;
					default:
						outval[i] = 0;
						break;
					}
				} else {
					outval[i] = 0;
				}
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			s = _NclLookUp(NrmQuarkToString(vals[i]));
			if( s != NULL){
				switch(s->type) {
				case NFUNC:
				case IFUNC:
					outval[i] = 1;
					break;
				default:
					outval[i] = 0;
					break;
				}
			} else {
				outval[i] = 0;
			}
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}

NhlErrorTypes _NclIUnDef
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg,*var,data;
	NclMultiDValData tmp_md;
	ng_size_t  i;
	NclQuark *vals;
	NclSymbol* s;
	NclObj tmp;

	
	arg  = _NclGetArg(0,1,DONT_CARE);
	switch(arg.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	vals = (NclQuark*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(vals[i] != tmp_md->multidval.missing_value.value.stringval) {
				s = _NclLookUp(NrmQuarkToString(vals[i]));
				if( s != NULL) {
					switch(s->type) {	
						case NPROC:
						case PIPROC:
						case IPROC:
						case NFUNC:
						case IFUNC:
							_NclFreeProcFuncInfo(s);
							break;
						case UNDEF:
							break;
						case VAR:
						case FVAR:
							var = _NclRetrieveRec(s,DONT_CARE);
							tmp = (NclObj)var->u.data_var;
							if((var != NULL)&&(var->u.data_var != NULL)) {
								if(var->u.data_var->var.var_type == NORMAL) {
/*
* Can't destroy symbol since it may be referenced from the instruction
* sequence. Changing it to UNDEF should do the trick though
*/
									_NclChangeSymbolType(s,UNDEF);
									data.kind = NclStk_NOVAL;
									data.u.data_obj = NULL;
									_NclPutRec(s,&data);
								}
								_NclDestroyObj((NclObj)tmp);
								if (var != NULL) {
									var->u.data_var = NULL;
									var->kind = NclStk_NOVAL;
								}
							}
							break;
						default:
							NhlPError(NhlWARNING,NhlEUNKNOWN,"undef: attempting to undefine a reserved word");
						break;
					}
				}
			} 
		}
	} else {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			s = _NclLookUp(NrmQuarkToString(vals[i]));
			if(s!=NULL){

				switch(s->type) {	
				case NPROC:
				case PIPROC:
				case IPROC:
				case NFUNC:
				case IFUNC:
					_NclFreeProcFuncInfo(s);
					break;
				case UNDEF:
					s->type = UNDEF;
					break;
				case VAR:
				case FVAR:
					var = _NclRetrieveRec(s,DONT_CARE);
					if((var != NULL)&&(var->u.data_var != NULL)) {
						tmp = (NclObj)var->u.data_var;
						if(var->u.data_var->var.var_type == NORMAL) {
/*
* Can't destroy symbol since it may be referenced from the instruction
* sequence. Changing it to UNDEF should do the trick though
*/
							_NclChangeSymbolType(s,UNDEF);
							data.kind = NclStk_NOVAL;
							data.u.data_obj = NULL;
							_NclPutRec(s,&data);
						}
						_NclDestroyObj((NclObj)tmp);
						var->u.data_var = NULL;
						var->kind = NclStk_NOVAL;
					}
					break;
				default:
					NhlPError(NhlWARNING,NhlEUNKNOWN,"undef: attempting to undefine a key word");
					break;
				}
			}
		}
	}
	
	return(NhlNOERROR);
}
NhlErrorTypes _NclIIsDefined
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg;
	NclMultiDValData tmp_md;
	ng_size_t  i;
	logical *outval;
	NclQuark *vals;
	NclSymbol* s;

	
	arg  = _NclGetArg(0,1,DONT_CARE);
	switch(arg.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_md->multidval.totalelements);
	vals = (NclQuark*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(vals[i] != tmp_md->multidval.missing_value.value.stringval) {
				s = _NclLookUp(NrmQuarkToString(vals[i]));
				if(( s == NULL)||(s->type == UNDEF)) {
					outval[i] = 0;
				} else {
					outval[i] = 1;
				}
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			s = _NclLookUp(NrmQuarkToString(vals[i]));
			if(( s == NULL)||(s->type == UNDEF)) {
				outval[i] = 0;
			} else {
				outval[i] = 1;
			}
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}
NhlErrorTypes _NclIIsVar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg;
	NclMultiDValData tmp_md;
	ng_size_t  i;
	logical *outval;
	NclQuark *vals;
	NclSymbol* s;

	
	arg  = _NclGetArg(0,1,DONT_CARE);
	switch(arg.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_md->multidval.totalelements);
	vals = (NclQuark*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(vals[i] != tmp_md->multidval.missing_value.value.stringval) {
				s = _NclLookUp(NrmQuarkToString(vals[i]));
				if(( s == NULL)||(s->type != VAR)) {
					outval[i] = 0;
				} else {
					outval[i] = 1;
				}
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			s = _NclLookUp(NrmQuarkToString(vals[i]));
			if(( s == NULL)||(s->type != VAR)) {
				outval[i] = 0;
			} else {
				outval[i] = 1;
			}
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}
NhlErrorTypes _NclIIsCoord
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg1,arg2;
	NclMultiDValData att_md;
	ng_size_t  i;
	logical *outval;
	NclVar tmp_var;
	NclQuark *vals;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	switch(arg2.kind) {
	case NclStk_VAR:
		att_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		att_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"iscoord: Non variable passed returning missing");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}
	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*att_md->multidval.totalelements);
	vals = (NclQuark*)att_md->multidval.val;
	if(att_md->multidval.missing_value.has_missing) {
		for(i = 0; i < att_md->multidval.totalelements; i++) {
			if(vals[i] != att_md->multidval.missing_value.value.stringval) {
				outval[i] = _NclIsCoord(tmp_var,NrmQuarkToString(vals[i]));
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < att_md->multidval.totalelements; i++) {
			outval[i] = _NclIsCoord(tmp_var,NrmQuarkToString(vals[i]));
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		att_md->multidval.n_dims,
		att_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}

NhlErrorTypes _NclIIsAtt
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg1,arg2;
	NclMultiDValData att_md;
	ng_size_t  i;
	logical *outval;
	NclVar tmp_var;
	NclQuark *vals;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg2.kind) {
	case NclStk_VAR:
		att_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		att_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isatt: Non variable passed returning missing");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}
	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*att_md->multidval.totalelements);
	vals = (NclQuark*)att_md->multidval.val;
	if(att_md->multidval.missing_value.has_missing) {
		for(i = 0; i < att_md->multidval.totalelements; i++) {
			if(vals[i] != att_md->multidval.missing_value.value.stringval) {
				outval[i] = _NclVarIsAtt(tmp_var,NrmQuarkToString(vals[i]));
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < att_md->multidval.totalelements; i++) {
			outval[i] = _NclVarIsAtt(tmp_var,NrmQuarkToString(vals[i]));
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		att_md->multidval.n_dims,
		att_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}

NhlErrorTypes _NclIIsDim
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg1,arg2;
	NclMultiDValData dim_md;
	ng_size_t  i;
	logical *outval;
	NclVar tmp_var;
	NclQuark *vals;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isdim: Non variable passed returning missing");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}
	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*dim_md->multidval.totalelements);
	vals = (NclQuark*)dim_md->multidval.val;
	if(dim_md->multidval.missing_value.has_missing) {
		for(i = 0; i < dim_md->multidval.totalelements; i++) {
			if(vals[i] != dim_md->multidval.missing_value.value.stringval) {
				outval[i] = _NclIsDim(tmp_var,NrmQuarkToString(vals[i])) == -1 ? 0:1;
			} else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < dim_md->multidval.totalelements; i++) {
			outval[i] = _NclIsDim(tmp_var,NrmQuarkToString(vals[i])) == -1 ? 0:1;
		}
	}
	
	return(NclReturnValue(
		(void*)outval,
		dim_md->multidval.n_dims,
		dim_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
}

NhlErrorTypes _NclIIsDimNamed
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg1,arg2;
	NclMultiDValData dim_md;
	int i;
	logical *outval;
	NclVar tmp_var;
	int *vals;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dimsize = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isdimnamed: Non variable passed returning missing");
		NclReturnValue(
			&miss,
			1,
			&dimsize,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}
	vals = (int*)dim_md->multidval.val;
	if (vals[0] == -1 && dim_md->multidval.totalelements == 1) {
		outval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_var->var.n_dims);
		for (i = 0; i < tmp_var->var.n_dims; i++) {
			if (tmp_var->var.dim_info[i].dim_quark < 1)
				outval[i] = 0;
			else
				outval[i] = 1;	
		}

        ng_size_t  ndims = tmp_var->var.n_dims;
		return(NclReturnValue(
			(void*)outval,
			1,
/*			(ng_size_t *)&tmp_var->var.n_dims,*/
            &ndims,
			NULL,
			NCL_logical,
			0
			));
	}
		
	outval = (logical*)NclMalloc((unsigned)sizeof(logical)*dim_md->multidval.totalelements);
	if(dim_md->multidval.missing_value.has_missing) {
		for(i = 0; i < dim_md->multidval.totalelements; i++) {
			if (vals[i] < 0 || vals[i] > tmp_var->var.n_dims - 1)
				outval[i] = 0;
			else if(vals[i] != dim_md->multidval.missing_value.value.intval) {
				if (tmp_var->var.dim_info[vals[i]].dim_quark < 1)
					outval[i] = 0;
				else
					outval[i] = 1;	
			}
			else {
				outval[i] = 0;
			}
		}
	} else {
		for(i = 0; i < dim_md->multidval.totalelements; i++) {
			if (vals[i] < 0 || vals[i] > tmp_var->var.n_dims - 1)
				outval[i] = 0;
			else if (tmp_var->var.dim_info[vals[i]].dim_quark < 1)
				outval[i] = 0;
			else
				outval[i] = 1;	

		}
	}
	return(NclReturnValue(
		(void*)outval,
		dim_md->multidval.n_dims,
		dim_md->multidval.dim_sizes,
		NULL,
		NCL_logical,
		0
	));
	
}

NhlErrorTypes _NclIIsFileVar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg0,arg1;
	NclMultiDValData tmp_md,file_md;
	ng_size_t  i;
	logical *outval;
	NclQuark *vals;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg0  = _NclGetArg(0,2,DONT_CARE);
	arg1  = _NclGetArg(1,2,DONT_CARE);
	switch(arg0.kind) {
	case NclStk_VAR:
		file_md = _NclVarValueRead(arg0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		file_md = arg0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	
	file_ptr = (NclFile)_NclGetObj(*(obj*)(file_md->multidval.val));

	if(file_ptr != NULL) {
		outval = (logical*)NclMalloc((unsigned)sizeof(logical)*tmp_md->multidval.totalelements);
		vals = (NclQuark*)tmp_md->multidval.val;
		if(tmp_md->multidval.missing_value.has_missing) {
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				if(vals[i] != tmp_md->multidval.missing_value.value.stringval) {
					outval[i] = _NclFileIsVar(file_ptr,vals[i]) == -1 ? 0 : 1;
				} else {
					outval[i] = 0;
				}
			}
		} else {
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				outval[i] = _NclFileIsVar(file_ptr,vals[i]) == -1 ? 0 : 1;
			}
		}
	
		return(NclReturnValue(
			(void*)outval,
			tmp_md->multidval.n_dims,
			tmp_md->multidval.dim_sizes,
			NULL,
			NCL_logical,
			0
		));
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isfilevar: undefined file returning missing value");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}

}

NhlErrorTypes _NclIIsFileVarAtt
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,file_md,att_md;
	ng_size_t  i;
	logical *outval;
	NclQuark var;
	NclQuark *vals;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg0  = _NclGetArg(0,3,DONT_CARE);
	arg1  = _NclGetArg(1,3,DONT_CARE);
	arg2  = _NclGetArg(2,3,DONT_CARE);
	switch(arg0.kind) {
	case NclStk_VAR:
		file_md = _NclVarValueRead(arg0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		file_md = arg0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg2.kind) {
	case NclStk_VAR:
		att_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		att_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	
	file_ptr = (NclFile)_NclGetObj(*(obj*)(file_md->multidval.val));
	var =*(NclQuark*)tmp_md->multidval.val;
	

	if(file_ptr != NULL) {
		outval = (logical*)NclMalloc((unsigned)sizeof(logical)*att_md->multidval.totalelements);
		vals = (NclQuark*)att_md->multidval.val;
		if(att_md->multidval.missing_value.has_missing) {
			for(i = 0; i < att_md->multidval.totalelements; i++) {
				if(vals[i] != att_md->multidval.missing_value.value.stringval) {
					outval[i] = _NclFileVarIsAtt(file_ptr,var,vals[i]) == -1 ? 0 : 1;
				} else {
					outval[i] = 0;
				}
			}
		} else {
			for(i = 0; i < att_md->multidval.totalelements; i++) {
				outval[i] = _NclFileVarIsAtt(file_ptr,var,vals[i]) == -1 ? 0 : 1;
			}
		}
	
		return(NclReturnValue(
			(void*)outval,
			att_md->multidval.n_dims,
			att_md->multidval.dim_sizes,
			NULL,
			NCL_logical,
			0
		));
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isfilevar: undefined file returning missing value");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}

}

NhlErrorTypes _NclIIsFileVarCoord
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,file_md,dim_md;
	ng_size_t  i;
	logical *outval;
	NclQuark var;
	NclQuark *vals;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg0  = _NclGetArg(0,3,DONT_CARE);
	arg1  = _NclGetArg(1,3,DONT_CARE);
	arg2  = _NclGetArg(2,3,DONT_CARE);
	switch(arg0.kind) {
	case NclStk_VAR:
		file_md = _NclVarValueRead(arg0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		file_md = arg0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	
	file_ptr = (NclFile)_NclGetObj(*(obj*)(file_md->multidval.val));
	var =*(NclQuark*)tmp_md->multidval.val;
	

	if(file_ptr != NULL) {
		outval = (logical*)NclMalloc((unsigned)sizeof(logical)*dim_md->multidval.totalelements);
		vals = (NclQuark*)dim_md->multidval.val;
		if(dim_md->multidval.missing_value.has_missing) {
			for(i = 0; i < dim_md->multidval.totalelements; i++) {
				if(vals[i] != dim_md->multidval.missing_value.value.stringval) {
					if(_NclFileVarIsDim(file_ptr,var,vals[i]) == -1 ? 0 : 1) {
						outval[i] = _NclFileVarIsCoord(file_ptr,vals[i]) == -1 ? 0 : 1;
					} else {
						outval[i] = 0;
					}
				} else {
					outval[i] = 0;
				}
			}
		} else {
			for(i = 0; i < dim_md->multidval.totalelements; i++) {
				if(_NclFileVarIsDim(file_ptr,var,vals[i]) == -1 ? 0 : 1) {
					outval[i] = _NclFileVarIsCoord(file_ptr,vals[i]) == -1 ? 0 : 1;
				} else {
					outval[i] = 0;
				}
			}
		}
	
		return(NclReturnValue(
			(void*)outval,
			dim_md->multidval.n_dims,
			dim_md->multidval.dim_sizes,
			NULL,
			NCL_logical,
			0
		));
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isfilevar: undefined file returning missing value");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}

}
NhlErrorTypes _NclIIsFileVarDim
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,file_md,dim_md;
	ng_size_t  i;
	logical *outval;
	NclQuark var;
	NclQuark *vals;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	ng_size_t dims = 1;

	
	arg0  = _NclGetArg(0,3,DONT_CARE);
	arg1  = _NclGetArg(1,3,DONT_CARE);
	arg2  = _NclGetArg(2,3,DONT_CARE);
	switch(arg0.kind) {
	case NclStk_VAR:
		file_md = _NclVarValueRead(arg0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		file_md = arg0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	
	file_ptr = (NclFile)_NclGetObj(*(obj*)(file_md->multidval.val));
	var =*(NclQuark*)tmp_md->multidval.val;
	

	if(file_ptr != NULL) {
		outval = (logical*)NclMalloc((unsigned)sizeof(logical)*dim_md->multidval.totalelements);
		vals = (NclQuark*)dim_md->multidval.val;
		if(dim_md->multidval.missing_value.has_missing) {
			for(i = 0; i < dim_md->multidval.totalelements; i++) {
				if(vals[i] != dim_md->multidval.missing_value.value.stringval) {
					outval[i] = _NclFileVarIsDim(file_ptr,var,vals[i]) == -1 ? 0 : 1;
				} else {
					outval[i] = 0;
				}
			}
		} else {
			for(i = 0; i < dim_md->multidval.totalelements; i++) {
				outval[i] = _NclFileVarIsDim(file_ptr,var,vals[i]) == -1 ? 0 : 1;
			}
		}
	
		return(NclReturnValue(
			(void*)outval,
			dim_md->multidval.n_dims,
			dim_md->multidval.dim_sizes,
			NULL,
			NCL_logical,
			0
		));
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"isfilevar: undefined file returning missing value");
		NclReturnValue(
			&miss,
			1,
			&dims,
			&((NclTypeClass)nclTypelogicalClass)->type_class.default_mis,
			NCL_logical,
			1
		);
		return(NhlWARNING);
	}

}

NhlErrorTypes _Ncl1dtond
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	NclStackEntry dims;
	NclMultiDValData tmp_dims = NULL;
	void *out_val;
	ng_size_t *dimsizes;
	ng_size_t sz = 1;
	ng_size_t  i, ndims;
	int ret;

	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

   	dims = _NclGetArg(1,2,DONT_CARE);
	switch(dims.kind) {
	case NclStk_VAR:
		tmp_dims = _NclVarValueRead(dims.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_dims = (NclMultiDValData)dims.u.data_obj;
		break;
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return(NhlFATAL);
	}

	if(tmp_dims == NULL)
		return(NhlFATAL);
	ndims = tmp_dims->multidval.totalelements;
	dimsizes = get_dimensions(tmp_dims->multidval.val,ndims,
				  tmp_dims->multidval.data_type,"onedtond");
	for (i = 0; i < ndims; i++) sz *= dimsizes[i];

	if((sz == tmp_md->multidval.totalelements)||(sz < tmp_md->multidval.totalelements)) {
		if(sz < tmp_md->multidval.totalelements) {
			NhlPError(NhlWARNING, NhlEUNKNOWN,"onedtond : output dimension sizes have fewer elements than input, some data not copied");
		}
		out_val = (void*)NclMalloc(sz*tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp_md->multidval.val,sz*tmp_md->multidval.type->type_class.size);
		ret = NclReturnValue(
			out_val,
			tmp_dims->multidval.totalelements,
			dimsizes,
			tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
			tmp_md->multidval.type->type_class.data_type,
			0
		);
	} else if((sz > tmp_md->multidval.totalelements)&&(sz%tmp_md->multidval.totalelements)){
		NhlPError(NhlWARNING, NhlEUNKNOWN,"onedtond : output dimension sizes not even multiples of input, check output");
		out_val = (void*)NclMalloc(sz*tmp_md->multidval.type->type_class.size);
		for(i = 0; i < (ng_size_t)sz/tmp_md->multidval.totalelements; i++) {
			memcpy(&(((char*)out_val)[i*tmp_md->multidval.totalsize]),
				tmp_md->multidval.val,
				tmp_md->multidval.totalsize);
		}
		memcpy(&(((char*)out_val)[i*tmp_md->multidval.totalsize]),
			tmp_md->multidval.val,
			(sz%tmp_md->multidval.totalelements)*tmp_md->multidval.type->type_class.size);

		ret = NclReturnValue(
			out_val,
			tmp_dims->multidval.totalelements,
			dimsizes,
			tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
			tmp_md->multidval.type->type_class.data_type,
			0
		);
	} else { /* (sz > tmp_md->multidval.totalelements)&&!(sz%tmp_md->multidval.totalelements)) */
		out_val = (void*)NclMalloc(sz*tmp_md->multidval.type->type_class.size);
		for(i = 0; i < (ng_size_t)sz/tmp_md->multidval.totalelements; i++) {
			memcpy(&(((char*)out_val)[i*tmp_md->multidval.totalsize]),
				tmp_md->multidval.val,
				tmp_md->multidval.totalsize);
		}
		ret = NclReturnValue(
			out_val,
			tmp_dims->multidval.totalelements,
			dimsizes,
			tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
			tmp_md->multidval.type->type_class.data_type,
			0
		);
	}
	NclFree(dimsizes);
	return(ret);
}
NhlErrorTypes _Nclndto1d
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t  dimsizes = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);
	
	out_val = (void*)NclMalloc(tmp_md->multidval.totalsize);

	memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.totalsize);
	dimsizes = tmp_md->multidval.totalelements;
	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
		tmp_md->multidval.type->type_class.data_type,
		0
	));
}
NhlErrorTypes _Nclproduct
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t dimsizes = 1;
	logical *tmp = NULL;
	ng_size_t  i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclCalloc(sizeof(logical),tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		i = 0;
		while((i<tmp_md->multidval.totalelements) && tmp[i]) {
			i++;
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				memcpy(out_val,&(tmp_md->multidval.missing_value.value),tmp_md->multidval.type->type_class.size);
				if (tmp)
					NclFree(tmp);
				return(NclReturnValue(
					out_val,
					1,
					&dimsizes,
					&(tmp_md->multidval.missing_value.value),
					tmp_md->multidval.type->type_class.data_type,
					0
				));
		}
		memcpy(out_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
			if(!tmp[i]) {
				_Nclmultiply(tmp_md->multidval.type,out_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),out_val,NULL,NULL,1,1);
			}
		}
	} else {
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Nclmultiply(tmp_md->multidval.type,out_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),out_val,NULL,NULL,1,1);
		}
	}
	if (tmp) 
		NclFree(tmp);
	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		tmp_md->multidval.type->type_class.data_type,
		0
	));
}

NhlErrorTypes _Ncldim_product
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	ng_size_t i,j;
	ng_size_t m,n;
	int sz;
	ng_size_t nd;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz* n);
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&(((char*)tmp_md->multidval.val)[i*m*sz]),&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			j = 0;
			while((j<m) && tmp[j]) {
				j++;
			}
			if(j==m) {
				memcpy(&(((char*)out_val)[i*sz]),&(tmp_md->multidval.missing_value.value),sz);
			} else {
				memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
				j = j+1;
				for(; j < m; j++) {
					if(!tmp[j]) {
						_Nclmultiply(tmp_md->multidval.type,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),&(((char*)out_val)[i*sz]),NULL,NULL,1,1);
					}
				}
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&tmp_md->multidval.missing_value.value,
			tmp_md->multidval.type->type_class.data_type,
			0);
	} else {
		for(i = 0; i < n ; i++) {
			memcpy(&(((char*)out_val)[i*sz]) ,&(((char*)tmp_md->multidval.val)[i*m*sz]),sz);
			for(j = 1; j < m; j++) {
				_Nclmultiply(tmp_md->multidval.type,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),&(((char*)out_val)[i*sz]),NULL,NULL,1,1);
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			tmp_md->multidval.type->type_class.data_type,
			0);
	}
	if(tmp != NULL)
		NclFree(tmp);
	NclFree(dimsizes);
	return(ret);

}

NhlErrorTypes _Ncldim_product_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
        int *dims; 
	ng_size_t ndims;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	ng_size_t i,j,k;
	ng_size_t i_in_sz,i_out_sz;
	ng_size_t m,n,nr,nl,sz;
	int nd;

/*
 * Get dimensions to do product across.
 */
	/* dims is the array of dimension numbers, ndims is the 1D size of the array (i.e. the number of dimensions) */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_product_n: Invalid dimension sizes to do product across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_product_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * "tmp" will be used to store locations where "m" chunks of the data
 * are equal to missing.
 */
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz*n);
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;
	      for(k = 0; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Ncleq(tmp_md->multidval.type,&tmp[k],
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),
		       &(tmp_md->multidval.missing_value.value),NULL,NULL,1,1);
	      }
/*
 * Loop through tmp to find the first non-missing value.
 */
	      k = 0;
	      while((k<m) && tmp[k]) {
		k++;
	      }
	      if(k==m) {
/*
 * All values were missing, so set the output to missing at this location.
 */
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(tmp_md->multidval.missing_value.value),sz);
	      } else {
/*
 * There's at least one non-missing value, so copy this value, and
 * start multiplying the rest of the non-missing values.
 */
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		k = k+1;
		for(; k < m; k++) {
		  if(!tmp[k]) {
		    i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		    _Nclmultiply(tmp_md->multidval.type,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     &(((char*)out_val)[i_out_sz]),NULL,NULL,1,1);
		  }
		}
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &tmp_md->multidval.missing_value.value,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so 
 * we don't need to look for missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;

	      /* k = 0 */
	      memcpy(&(((char*)out_val)[i_out_sz]),
		     &(((char*)tmp_md->multidval.val)[(i*(nr*m)+j)*sz]),sz);
	      for(k = 1; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Nclmultiply(tmp_md->multidval.type,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     &(((char*)out_val)[i_out_sz]),NULL,NULL,1,1);
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	}
	if(tmp != NULL)
	  NclFree(tmp);
	NclFree(dimsizes);
	return(ret);
}



NhlErrorTypes _Ncldim_sum
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	ng_size_t i,j;
	ng_size_t m,n;
	int sz;
	ng_size_t nd;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz* n);
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&(((char*)tmp_md->multidval.val)[i*m*sz]),&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			j = 0;
			while((j<m) && tmp[j]) {
				j++;
			}
			if(j==m) {
				memcpy(&(((char*)out_val)[i*sz]),&(tmp_md->multidval.missing_value.value),sz);
			} else {
				memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
				j = j+1;
				for(; j < m; j++) {
					if(!tmp[j]) {
						_Nclplus(tmp_md->multidval.type,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),&(((char*)out_val)[i*sz]),NULL,NULL,1,1);
					}
				}
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&tmp_md->multidval.missing_value.value,
			tmp_md->multidval.type->type_class.data_type,
			0);
	} else {
		for(i = 0; i < n ; i++) {
			memcpy(&(((char*)out_val)[i*sz]) ,&(((char*)tmp_md->multidval.val)[i*m*sz]),sz);
			for(j = 1; j < m; j++) {
				_Nclplus(tmp_md->multidval.type,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),&(((char*)out_val)[i*sz]),NULL,NULL,1,1);
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			tmp_md->multidval.type->type_class.data_type,
			0);
	}
	if(tmp != NULL)
		NclFree(tmp);
	NclFree(dimsizes);
	return(ret);

}


NhlErrorTypes _Ncldim_sum_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
        int  *dims;
	ng_size_t ndims;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	ng_size_t i,j,k;
	ng_size_t i_in_sz,i_out_sz;
	ng_size_t m,n,nr,nl;
	int sz;
	int nd;

/*
 * Get dimension(s) to do sum across.
 */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_sum_n: Invalid dimension sizes to do sum across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_sum_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * "tmp" will be used to store locations where "m" chunks of the data
 * are equal to missing.
 */
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz*n);
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;
	      for(k = 0; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Ncleq(tmp_md->multidval.type,&tmp[k],
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),
		       &(tmp_md->multidval.missing_value.value),NULL,NULL,1,1);
	      }
/*
 * Loop through tmp to find the first non-missing value.
 */
	      k = 0;
	      while((k<m) && tmp[k]) {
		k++;
	      }
	      if(k==m) {
/*
 * All values were missing, so set the output to missing at this location.
 */
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(tmp_md->multidval.missing_value.value),sz);
	      } else {
/*
 * There's at least one non-missing value, so copy this value, and
 * start summing the rest of the non-missing values.
 */
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		k = k+1;
		for(; k < m; k++) {
		  if(!tmp[k]) {
		    i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		    _Nclplus(tmp_md->multidval.type,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     &(((char*)out_val)[i_out_sz]),NULL,NULL,1,1);
		  }
		}
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &tmp_md->multidval.missing_value.value,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so 
 * we don't need to look for missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;

	      /* k = 0 */
	      memcpy(&(((char*)out_val)[i_out_sz]),
		     &(((char*)tmp_md->multidval.val)[(i*(nr*m)+j)*sz]),sz);
	      for(k = 1; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Nclplus(tmp_md->multidval.type,&(((char*)out_val)[i_out_sz]),
			 &(((char*)tmp_md->multidval.val)[i_in_sz]),
			 &(((char*)out_val)[i_out_sz]),NULL,NULL,1,1);
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	}
	if(tmp != NULL)
	  NclFree(tmp);
	NclFree(dimsizes);
	return(ret);
}


NhlErrorTypes _Nclsum
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t dimsizes = 1;
	logical *tmp = NULL;
	ng_size_t  i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		i = 0;
		while((i<tmp_md->multidval.totalelements) && tmp[i]) {
			i++;
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				if(tmp != NULL) 
					NclFree(tmp);
				memcpy(out_val,&(tmp_md->multidval.missing_value.value),tmp_md->multidval.type->type_class.size);
				return(NclReturnValue(
					out_val,
					1,
					&dimsizes,
					&(tmp_md->multidval.missing_value.value),
					tmp_md->multidval.type->type_class.data_type,
					0
				));
		}
		memcpy(out_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
			if(!tmp[i]) {
				_Nclplus(tmp_md->multidval.type,out_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),out_val,NULL,NULL,1,1);
			}
		}
	} else {
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Nclplus(tmp_md->multidval.type,out_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),out_val,NULL,NULL,1,1);
		}
	}
	if(tmp != NULL) 
		NclFree(tmp);

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		tmp_md->multidval.type->type_class.data_type,
		0
	));
}

NhlErrorTypes _Ncldim_cumsum
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry data0,data1;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData opt_md = NULL;
	void *out_val = NULL;
	logical *tmp = NULL;
	ng_size_t i,j;
	ng_size_t m,n;
	int sz;
	NclScalar *missing = NULL;
	int opt;


	data0 = _NclGetArg(0,2,DONT_CARE);
	switch(data0.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	data1 = _NclGetArg(1,2,DONT_CARE);
	switch(data1.kind) {
	case NclStk_VAR:
		opt_md = _NclVarValueRead(data1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		opt_md = (NclMultiDValData)data1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(opt_md == NULL)
		return(NhlFATAL);
	opt = *((int*)opt_md->multidval.val);


	n = 1;
	m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
	if(tmp_md->multidval.n_dims > 1) {
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
		}
	}
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz * tmp_md->multidval.totalelements);

	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			ng_size_t goffset;
			ng_size_t dim_offset = i * m * sz;
			int missing_flag = 0;
			_Ncleq(tmp_md->multidval.type,tmp,((char*)tmp_md->multidval.val) + dim_offset,
			       &(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			switch (opt) {
			default:
				NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_cumsum: invalid value for opt argument: defaulting to 0");
			case 0: /* all values after first missing become missing */
				memcpy((char*)out_val + dim_offset,
				       (char*)tmp_md->multidval.val + dim_offset,sz);
				if (tmp[0]) {
					missing_flag = 1;
				}
				for(j = 1; j < m; j++) {
					ng_size_t last_offset = dim_offset + (j-1) * sz;
					ng_size_t offset = dim_offset + j * sz;
					if (missing_flag || tmp[j]) {
						missing_flag = 1;
			 			memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),sz);
					}
					else {
						_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
							 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + last_offset,NULL,NULL,1,1);
					}
				}
				missing = &tmp_md->multidval.missing_value.value;
				break;
			case 1: /* missing values are skipped */
				for (j = 0; j < m && tmp[j]; j++) {
					ng_size_t offset = dim_offset + j * sz;
					memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),sz);
				}
				if (j < m) {
					goffset = dim_offset + j * sz;
					memcpy((char*)out_val + goffset,(char*)(tmp_md->multidval.val) + goffset,sz);
				}
				for(j++; j < m; j++) {
					ng_size_t offset = dim_offset + j * sz;
					if (tmp[j]) {
						memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),sz);
					}
					else {
						_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
							 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + goffset,NULL,NULL,1,1);
						goffset = offset;
					}
				}
				missing = &tmp_md->multidval.missing_value.value;
				break;
			case 2: /* missing values treated as 0 */
				for (j = 0; j < m && tmp[j]; j++) {
					ng_size_t offset = dim_offset + j * sz;
					memset((char*)out_val + offset,0,sz);
				}
				if (j == 0) {
					memcpy((char*)out_val + dim_offset,(char*)tmp_md->multidval.val + dim_offset,sz);
					j++;
				}
				for(; j < m; j++) {
					ng_size_t last_offset = dim_offset + (j-1) * sz;
					ng_size_t offset = dim_offset + j * sz;
					if (tmp[j]) {
						memcpy((char*)out_val + offset,(char*)out_val + last_offset,sz);
					}
					else {
						_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
							 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + last_offset,NULL,NULL,1,1);
					}
				}
				break;
			}
		}
	} else {
		for(i = 0; i < n ; i++) {
			ng_size_t dim_offset = i * m * sz;
			memcpy((char*)out_val + dim_offset,(char*)tmp_md->multidval.val + dim_offset,sz);
			for(j = 1; j < m; j++) {
				ng_size_t last_offset = dim_offset + (j-1) * sz;
				ng_size_t offset = dim_offset + j * sz;
				_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
					 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + last_offset,NULL,NULL,1,1);
			}
		}
	}
	if(tmp != NULL)
		NclFree(tmp);

	ret = NclReturnValue(
		out_val,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		missing,
		tmp_md->multidval.type->type_class.data_type,
		0);
	return(ret);
}

NhlErrorTypes _Ncldim_cumsum_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry data0,data1;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData opt_md = NULL;
	void *out_val = NULL;
	int *dims;
	ng_size_t ndims;
	logical *tmp = NULL;
	ng_size_t i,j,k;
	int sz;
	ng_size_t m,nl,nr;
	NclScalar *missing = NULL;
	int opt;

/*
 * Get dimension(s) to do cumulative sum across.
 *
 * This function is set up to allow multiple input dimensions,
 * but I don't think this function makes much sense in that 
 * context, so I'm going to register it to only allow one
 * input dimension.
 */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

	data0 = _NclGetArg(0,3,DONT_CARE);
	switch(data0.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	data1 = _NclGetArg(1,3,DONT_CARE);
	switch(data1.kind) {
	case NclStk_VAR:
		opt_md = _NclVarValueRead(data1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		opt_md = (NclMultiDValData)data1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(opt_md == NULL)
		return(NhlFATAL);
	opt = *((int*)opt_md->multidval.val);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_cumsum_n: Invalid dimension sizes to do cumulative sum across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_cumsum_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}

	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz * tmp_md->multidval.totalelements);

	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      ng_size_t goffset;
	      int missing_flag = 0;
	      ng_size_t dim_offset = ((i*nr*m)+j)*sz;
	      for(k = 0; k < m; k++) {
		goffset = dim_offset + (nr*k)*sz;
		_Ncleq(tmp_md->multidval.type,&tmp[k],
		       ((char*)tmp_md->multidval.val) + goffset,
		       &(tmp_md->multidval.missing_value.value),NULL,NULL,1,1);
	      }
	      switch (opt) {
	      default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,"dim_cumsum_n: invalid value for opt argument: defaulting to 0");
	      case 0: /* all values after first missing become missing */
		memcpy((char*)out_val + dim_offset,
		       (char*)tmp_md->multidval.val + dim_offset,sz);
		if (tmp[0]) {
		  missing_flag = 1;
		}
		for(k = 1; k < m; k++) {
		  ng_size_t last_offset = dim_offset + (nr*(k-1))*sz;
		  ng_size_t offset = dim_offset + (nr*k)*sz;
		  if (missing_flag || tmp[k]) {
		    missing_flag = 1;
		    memcpy((char*)out_val + offset,
			   &(tmp_md->multidval.missing_value.value),sz);
		  }
		  else {
		    _Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
			     (char*)(tmp_md->multidval.val) + offset,
			     (char*)out_val + last_offset,NULL,NULL,1,1);
		  }
		}
		missing = &tmp_md->multidval.missing_value.value;
		break;
	      case 1: /* missing values are skipped */
		for (k = 0; k < m && tmp[k]; k++) {
		  ng_size_t offset = dim_offset + (nr*k)*sz;
		  memcpy((char*)out_val + offset,
			 &(tmp_md->multidval.missing_value.value),sz);
		}
		if (k < m) {
		  goffset = dim_offset + (nr*k)*sz;
		  memcpy((char*)out_val + goffset,
			 (char*)(tmp_md->multidval.val) + goffset,sz);
		}
		for(k++; k < m; k++) {
		  ng_size_t offset = dim_offset + (nr*k)*sz;
		  if (tmp[k]) {
		    memcpy((char*)out_val + offset,
			   &(tmp_md->multidval.missing_value.value),sz);
		  }
		  else {
		    _Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
			     (char*)(tmp_md->multidval.val) + offset,
			     (char*)out_val + goffset,NULL,NULL,1,1);
		    goffset = offset;
		  }
		}
		missing = &tmp_md->multidval.missing_value.value;
		break;
	      case 2: /* missing values treated as 0 */
		for (k = 0; k < m && tmp[k]; k++) {
		  ng_size_t offset = dim_offset + (nr*k)*sz;
		  memset((char*)out_val + offset,0,sz);
		}
		if (k == 0) {
		  memcpy((char*)out_val + dim_offset,
			 (char*)tmp_md->multidval.val + dim_offset,sz);
		  k++;
		}
		for(; k < m; k++) {
		  ng_size_t last_offset = dim_offset + (nr*(k-1))*sz;
		  ng_size_t offset = dim_offset + (k*nr)*sz;
		  if (tmp[k]) {
		    memcpy((char*)out_val + offset,
			   (char*)out_val + last_offset,sz);
		  }
		  else {
		    _Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
			     (char*)(tmp_md->multidval.val) + offset,
			     (char*)out_val + last_offset,NULL,NULL,1,1);
		  }
		}
		break;
	      }
	    }
	  }
	} else {
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      ng_size_t dim_offset = ((i*nr*m)+j)*sz;
	      memcpy((char*)out_val + dim_offset,
		     (char*)tmp_md->multidval.val + dim_offset,sz);
	      for(k = 1; k < m; k++) {
		ng_size_t last_offset = dim_offset + (nr*(k-1))*sz;
		ng_size_t offset = dim_offset + (nr*k)*sz;
		_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
			 (char*)(tmp_md->multidval.val) + offset,
			 (char*)out_val + last_offset,NULL,NULL,1,1);
	      }
	    }
	  }
	}
	if(tmp != NULL)
	  NclFree(tmp);

	ret = NclReturnValue(
		out_val,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		missing,
		tmp_md->multidval.type->type_class.data_type,
		0);
	return(ret);
}

NhlErrorTypes _Nclcumsum
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data0,data1;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData opt_md = NULL;
	NclScalar *missing = NULL;
	int opt;
	void *out_val;
	logical *tmp = NULL;
	int missing_flag = 0;
	ng_size_t  i;
	ng_size_t goffset;

	data0 = _NclGetArg(0,2,DONT_CARE);
	switch(data0.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	data1 = _NclGetArg(1,2,DONT_CARE);
	switch(data1.kind) {
	case NclStk_VAR:
		opt_md = _NclVarValueRead(data1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		opt_md = (NclMultiDValData)data1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(opt_md == NULL)
		return(NhlFATAL);
	opt = *((int*)opt_md->multidval.val);


	out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size * tmp_md->multidval.totalelements);
	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,
		       &(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);

		switch (opt) {
		default:
			NhlPError(NhlWARNING,NhlEUNKNOWN,"cumsum: invalid value for opt argument: defaulting to 0");
		case 0: /* all values after first missing become missing */
			memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
			if (tmp[0]) {
				missing_flag = 1;
			}
			for(i = 1; i < tmp_md->multidval.totalelements; i++) {
				ng_size_t last_offset = (i-1) *  tmp_md->multidval.type->type_class.size;
				ng_size_t offset = i * tmp_md->multidval.type->type_class.size;
				if (missing_flag || tmp[i]) {
					missing_flag = 1;
					memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),
					       tmp_md->multidval.type->type_class.size);
				}
				else {
					_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
						 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + last_offset,NULL,NULL,1,1);
				}
			}
			missing = &tmp_md->multidval.missing_value.value;
			break;
		case 1: /* missing values are skipped */
			i = 0;
			while (tmp[i]) {
				ng_size_t offset = i * tmp_md->multidval.type->type_class.size;
				memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),
				       tmp_md->multidval.type->type_class.size);
				i++;
			}
			if (i < tmp_md->multidval.totalelements) {
				goffset = i * tmp_md->multidval.type->type_class.size;
				memcpy((char*)out_val + goffset,(char*)(tmp_md->multidval.val) + goffset,tmp_md->multidval.type->type_class.size);
			}
			for(i++; i < tmp_md->multidval.totalelements; i++) {
				ng_size_t offset = i * tmp_md->multidval.type->type_class.size;
				if (tmp[i]) {
					memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),
					       tmp_md->multidval.type->type_class.size);
				}
				else {
					_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
						 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + goffset,NULL,NULL,1,1);
					goffset = offset;
				}
			}
			missing = &tmp_md->multidval.missing_value.value;
			break;
		case 2: /* missing values treated as 0 */
			i = 0;
			while (tmp[i]) {
				ng_size_t offset = i * tmp_md->multidval.type->type_class.size;
				memset((char*)out_val + offset,0,tmp_md->multidval.type->type_class.size);
				i++;
			}
			if (i == 0) {
				memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
				i++;
			}
			for(; i < tmp_md->multidval.totalelements; i++) {
				ng_size_t last_offset = (i-1) *  tmp_md->multidval.type->type_class.size;
				ng_size_t offset = i * tmp_md->multidval.type->type_class.size;
				if (tmp[i]) {
					memcpy((char*)out_val + offset,(char*)out_val + last_offset,
					       tmp_md->multidval.type->type_class.size);
				}
				else {
					_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,
						 (char*)(tmp_md->multidval.val) + offset,(char*)out_val + last_offset,NULL,NULL,1,1);
				}
			}
			break;
		}
	} else {
		memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			ng_size_t last_offset = (i-1) *  tmp_md->multidval.type->type_class.size;
			ng_size_t offset = i * tmp_md->multidval.type->type_class.size;
			_Nclplus(tmp_md->multidval.type,(char*)out_val + offset,(char*)(tmp_md->multidval.val) + offset,(char*)out_val + last_offset,NULL,NULL,1,1);
		}
	}
	if(tmp != NULL) 
		NclFree(tmp);

	return(NclReturnValue(
		out_val,
		tmp_md->multidval.n_dims,
		tmp_md->multidval.dim_sizes,
		missing,
		tmp_md->multidval.type->type_class.data_type,
		0
	));
}

NhlErrorTypes _Ncldim_avg
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	double sum_val ;
	double *val = NULL;
	ng_size_t *dimsizes = NULL;
	ng_size_t i,j;
	ng_size_t m,n;
	int nd;
	ng_size_t count;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int did_coerce = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Calculate size of all but rightmost dimension (n).
 * The rightmost dimension is "m". The number of dimenions is "nd".
 */
	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
/*
 * Determine output type, which will either be float or double.
 */
	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double)* n);
		out_data_type = NCL_double;
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg: Could not coerce input data to double, can't continue");
			return(NhlFATAL);
		} else if(tmp_md->multidval.missing_value.has_missing) {

			missing = tmp_md->multidval.missing_value.value;
		}
		did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
		for(i = 0; i < n ; i++) {
/*
 * Check if we have at least one non-missing value in the subset
 * of rightmost values.
 */
			for(j = 0; j < m; j++) {
				if(val[i*m+j] != tmp_md->multidval.missing_value.value.doubleval) {
					break;
				}
			}
			count = 0;

			if(j==m) {
/*
 * Values were all missing for this subset, so set output to
 * missing for this location.
 */
				if(out_data_type == NCL_double) {
					((double*)out_val)[i] = missing.doubleval;
				} else {
					((float*)out_val)[i] = (float)missing.doubleval;
				}
			} else {
/*
 * Values were NOT all missing for this subset, so do the average,
 * starting with the first non-missing value (j).
 */
				count = 1;
				sum_val = val[(i*m) + j];

				j = j+1;
				for(; j<m;j++){
					if(val[(i*m) + j] != tmp_md->multidval.missing_value.value.doubleval) {
						sum_val+= val[(i*m) + j];
						count = count +1;
					}
				}
				sum_val = sum_val/(double) count;
/*
 * Set to appropriate type for output.
 */
				if(out_data_type == NCL_double) {
                                        ((double*)out_val)[i] = sum_val;
                                } else {
                                        ((float*)out_val)[i] = (float)sum_val;
                                }
			}
		}
		if(out_data_type != NCL_double) {
			missing.floatval = (float)missing.doubleval;
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&missing,
			out_data_type,
			0);
		NclFree(dimsizes);
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
		return(ret);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so we 
 * can proceed without worrying about missing values.
 */
		for(i = 0; i < n ; i++) {
			sum_val = val[i*m];
			for(j = 1; j< m; j++) {
				sum_val += val[i*m + j];
			}
			sum_val = sum_val/(double)m;
                        if(out_data_type == NCL_double) {
				((double*)out_val)[i] = sum_val;
			} else {
				((float*)out_val)[i] = (float)sum_val;
			}

		}	
	
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			out_data_type,
			0);
		NclFree(dimsizes);
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
		return(ret);
	}

}


NhlErrorTypes _Ncldim_avg_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	int *dims;
	int ndims;
	double sum_val ;
	double *val = NULL;
	ng_size_t *dimsizes = NULL;
	ng_size_t i,j,k;
	ng_size_t m,n,nr,nl;
	int nd;
	ng_size_t count;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int did_coerce = 0;
/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Get dimension(s) to do average across. These can be dimension 
 * indexes or dimension names.
 */
	dims = get_dims_for_n_funcs(1,2,data,"dim_avg_n",&ndims);
	if(dims == NULL) { 
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Invalid input dimensions specified");
	  return(NhlFATAL);
	}
/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Invalid dimension sizes to do average across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}
/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * Determine output type, which will either be float or double.
 */
	if(tmp_md->multidval.data_type == NCL_double) {
	  out_val = (void*)NclMalloc(sizeof(double)* n);
	  out_data_type = NCL_double;
	  if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	} else {
	  out_val = (void*)NclMalloc(sizeof(float)* n);
	  out_data_type = NCL_float;
	  tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp_md == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Could not coerce input data to double, can't continue");
	    return(NhlFATAL);
	  } else if(tmp_md->multidval.missing_value.has_missing) {
	    
	    missing = tmp_md->multidval.missing_value.value;
	  }
	  did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
/*
 * Check if we have at least one non-missing value in the subset
 * of rightmost values.
 */
	      for(k = 0; k < m; k++) {
		if(val[i*(nr*m)+(k*nr)+j] != tmp_md->multidval.missing_value.value.doubleval) {
		  break;
		}
	      }
	      count = 0;
	      
	      if(k==m) {
/*
 * Values were all missing for this subset, so set output to
 * missing for this location.
 */
		if(out_data_type == NCL_double) {
		  ((double*)out_val)[i*nr+j] = missing.doubleval;
		} else {
		  ((float*)out_val)[i*nr+j] = (float)missing.doubleval;
		}
	      } else {
/*
 * Values were NOT all missing for this subset, so do the average,
 * starting with the first non-missing value.
 */
		count = 1;
		sum_val = val[i*(nr*m)+(k*nr)+j];
		
		k = k+1;
		for(; k<m;k++){
		  if(val[i*(nr*m)+(k*nr)+j] != tmp_md->multidval.missing_value.value.doubleval) {
		    sum_val+= val[i*(nr*m)+(k*nr)+j];
		    count = count +1;
		  }
		}
		sum_val = sum_val/(double) count;
/*
 * Set to appropriate type for output.
 */
		if(out_data_type == NCL_double) {
		  ((double*)out_val)[i*nr+j] = sum_val;
		} else {
		  ((float*)out_val)[i*nr+j] = (float)sum_val;
		}
	      }
	    }
	  }
	  if(out_data_type != NCL_double) {
	    missing.floatval = (float)missing.doubleval;
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &missing,
			       out_data_type,
			       0);
	  NclFree(dimsizes);
	  if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
	  return(ret);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so we 
 * can proceed without worrying about missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      sum_val = val[i*(nr*m)+j];
	      for(k = 1; k< m; k++) {
		sum_val += val[i*(nr*m)+(k*nr)+j];
	      }
	      sum_val = sum_val/(double)m;
	      if(out_data_type == NCL_double) {
		((double*)out_val)[i*nr+j] = sum_val;
	      } else {
		((float*)out_val)[i*nr+j] = (float)sum_val;
	      }
	    }
	  }
	
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       out_data_type,
			       0);
	  NclFree(dims);
	  NclFree(dimsizes);
	  if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
	  return(ret);
	}
}


NhlErrorTypes _NclIdim_variance
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	double sum_val ;
	double sum_sqrd_val ;
	double *val = NULL;
	ng_size_t *dimsizes = NULL;
	ng_size_t i,j;
	ng_size_t m,n;
	ng_size_t nd;
	ng_size_t count;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int start;
	int did_coerce = 0;

	

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double)* n);
		out_data_type = NCL_double;
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_variance: Could not coerce input data to double, can't continue");
			return(NhlFATAL);
		} else if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
		did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			for(j = 0; j < m; j++) {
				if(val[i*m+j] != tmp_md->multidval.missing_value.value.doubleval) {
					break;
				}
			}
			count = 0;

			if(j==m) {
				if(out_data_type == NCL_double) {
					((double*)out_val)[i] = missing.doubleval;
				} else {
					((float*)out_val)[i] = (float)missing.doubleval;
				}
			} else {
				count = 1;
				start = j;
				sum_val = val[(i*m) + j];
				j = j+1;
				for(; j<m;j++){
					if(val[(i*m) + j] != tmp_md->multidval.missing_value.value.doubleval) {
						sum_val+= val[(i*m) + j];
						count = count +1;
					}
				}
				if(count != 1) {
					sum_val = sum_val/(double) count;
					sum_sqrd_val = 0;
					for(j = start; j < m; j++) {
						if(val[(i*m)+j]!= tmp_md->multidval.missing_value.value.doubleval) {        
							sum_sqrd_val += (val[(i*m)+j] - sum_val) * (val[(i*m)+j] - sum_val);
						}
					}
					sum_sqrd_val /= (double)(count-1);
				} else {
					sum_sqrd_val = 0.0;
				}
				if(out_data_type == NCL_double) {
                                        ((double*)out_val)[i] = sum_sqrd_val;
                                } else {
                                        ((float*)out_val)[i] = (float)sum_sqrd_val;
                                }
			}
		}
		if(out_data_type != NCL_double) {
			missing.floatval = (float)missing.doubleval;
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&missing,
			out_data_type,
			0);
		NclFree(dimsizes);
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
		return(ret);
	} else {
		for(i = 0; i < n ; i++) {
			sum_val = val[i*m];
			for(j = 1; j< m; j++) {
				sum_val += val[i*m + j];
			}
			sum_val = sum_val/(double)m;
			sum_sqrd_val = 0;
			for(j = 0; j < m; j++) {
				sum_sqrd_val += (val[(i*m)+j] - sum_val) * (val[(i*m)+j] - sum_val);
			}
			sum_sqrd_val /= (double)(m-1);
                        if(out_data_type == NCL_double) {
				((double*)out_val)[i] = sum_sqrd_val;
			} else {
				((float*)out_val)[i] = (float)sum_sqrd_val;
			}

		}	
	
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			out_data_type,
			0);
		NclFree(dimsizes);
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
		return(ret);
	}

}
NhlErrorTypes _NclIdim_variance_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	int *dims; 
	ng_size_t  ndims;
	double sum_val ;
	double sum_sqrd_val ;
	double *val = NULL;
	ng_size_t *dimsizes = NULL;
	ng_size_t i,j,k;
	ng_size_t i_in,i_out;
	ng_size_t m,n,nr,nl;
	int nd;
	ng_size_t count;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int start;
	int did_coerce = 0;

/*
 * Get dimension(s) to do average across.
 */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_variance_n: Invalid dimension sizes to do average across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_variance_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}

/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * Determine output type, which will either be float or double.
 */
	if(tmp_md->multidval.data_type == NCL_double) {
	  out_val = (void*)NclMalloc(sizeof(double)* n);
	  out_data_type = NCL_double;
	  if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	} else {
	  out_val = (void*)NclMalloc(sizeof(float)* n);
	  out_data_type = NCL_float;
	  tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp_md == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_variance_n: Could not coerce input data to double, can't continue");
	    return(NhlFATAL);
	  } else if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	  did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out = i*nr + j;
/*
 * Check if we have at least one non-missing value in the subset
 * of rightmost values.
 */
	      for(k = 0; k < m; k++) {
		i_in = i*(nr*m)+(k*nr)+j;
		if(val[i_in] != tmp_md->multidval.missing_value.value.doubleval) {
		  break;
		}
	      }
	      count = 0;
	      
	      if(k==m) {
		if(out_data_type == NCL_double) {
		  ((double*)out_val)[i_out] = missing.doubleval;
		} else {
		  ((float*)out_val)[i_out] = (float)missing.doubleval;
		}
	      } else {
/*
 * Values were NOT all missing for this subset, so do the average,
 * starting with the first non-missing value.
 */
		count = 1;
		start = k;
		sum_val = val[i_in];
	
		k = k+1;
		for(; k<m;k++){
		  i_in = i*(nr*m)+(k*nr)+j;
		  if(val[i_in] != tmp_md->multidval.missing_value.value.doubleval) {
		    sum_val+= val[i_in];
		    count = count +1;
		  }
		}
		if(count != 1) {
		  sum_val = sum_val/(double) count;
		  sum_sqrd_val = 0;
		  for(k = start; k < m; k++) {
		    i_in = i*(nr*m)+(k*nr)+j;
		    if(val[i_in] != tmp_md->multidval.missing_value.value.doubleval) {        
		      sum_sqrd_val += (val[i_in] - sum_val) * (val[i_in] - sum_val);
		    }
		  }
		  sum_sqrd_val /= (double)(count-1);
		} else {
		  sum_sqrd_val = 0.0;
		}
/*
 * Set to appropriate type for output.
 */
		if(out_data_type == NCL_double) {
		  ((double*)out_val)[i_out] = sum_sqrd_val;
		} else {
		  ((float*)out_val)[i_out] = (float)sum_sqrd_val;
		}
	      }
	    }
	  }
	  if(out_data_type != NCL_double) {
	    missing.floatval = (float)missing.doubleval;
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &missing,
			       out_data_type,
			       0);
	  NclFree(dimsizes);
	  if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
	  return(ret);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so we 
 * can proceed without worrying about missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out = i*nr + j;
	      sum_val = val[i*(nr*m)+j];   /* k = 0 */
	      for(k = 1; k< m; k++) {
		i_in = i*(nr*m)+(k*nr)+j;
		sum_val += val[i_in];
	      }
	      sum_val = sum_val/(double)m;
	      sum_sqrd_val = 0;
	      for(k = 0; k< m; k++) {
		i_in = i*(nr*m)+(k*nr)+j;
		sum_sqrd_val += (val[i_in] - sum_val) * (val[i_in] - sum_val);
	      }
	      sum_sqrd_val /= (double)(m-1);
	      if(out_data_type == NCL_double) {
		((double*)out_val)[i_out] = sum_sqrd_val;
	      } else {
		((float*)out_val)[i_out] = (float)sum_sqrd_val;
	      }
	    }	
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       out_data_type,
			       0);
	  NclFree(dimsizes);
	  if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
	  return(ret);
	}
}
NhlErrorTypes _NclIvariance
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	double sum_val;
	double sum_sqrd_val;
	double *val;
	void *out1_val;
	ng_size_t dimsizes = 1;
	ng_size_t n;
	ng_size_t  i;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int did_coerce = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	if(tmp_md->multidval.data_type == NCL_double) {
		out1_val = (void*)NclMalloc(sizeof(double));
		if((tmp_md->multidval.n_dims ==1)&&( tmp_md->multidval.dim_sizes[0] ==1)) {
			*(double*)out1_val = 0.0;
		        return(NclReturnValue(
               		 	out1_val,
                		1,
                		&dimsizes,
                		NULL,
                		NCL_double,
                		0
        		));

		}
                if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
		out_data_type = NCL_double;
	} else {
		out1_val = (void*)NclMalloc(sizeof(float));
		if((tmp_md->multidval.n_dims ==1)&&( tmp_md->multidval.dim_sizes[0] ==1)) {
			*(float*)out1_val = 0.0;
		        return(NclReturnValue(
               		 	out1_val,
                		1,
                		&dimsizes,
                		NULL,
                		NCL_float,
                		0
        		));
		}
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
                if(tmp_md == NULL) {
                	NhlPError(NhlFATAL,NhlEUNKNOWN,"variance: Could not coerce input data to double, can't continue");
                	return(NhlFATAL);
                } else if(tmp_md->multidval.missing_value.has_missing){
                	missing = tmp_md->multidval.missing_value.value;
                }
		did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(val[i]!= tmp_md->multidval.missing_value.value.doubleval) {
				break;
			}
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				NhlPError(NhlWARNING,NhlEUNKNOWN,"variance: Entire input array contains missing values, can't compute variance");
				if(out_data_type == NCL_double) {
					*(double*)out1_val = missing.doubleval;
				} else {
					*(float*)out1_val = (float)missing.doubleval;
					missing.floatval = (float)missing.doubleval;
				}
				if(did_coerce)_NclDestroyObj((NclObj)tmp_md);
				return(NclReturnValue(
					out1_val,
					1,
					&dimsizes,
					&missing,
					out_data_type,
					0
				));
		}
		n = 1;
		sum_val = val[i];	
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
	
			if(val[i]!= missing.doubleval) {	
				sum_val += val[i];
				n+=1;
	
			}
		}
/*
* Square of sum
*/
		if(n != 1) {
			sum_val /= (double)n;
			sum_sqrd_val = 0;
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				if(val[i]!= missing.doubleval) {        
					sum_sqrd_val += (val[i] - sum_val) * (val[i] - sum_val);
				}
			}
			sum_sqrd_val /= (double)(n-1);
		} else {
			sum_sqrd_val = 0.0;
		}



	} else {
		i = 0;
		n = 1;
		sum_val = val[i];	
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
			sum_val += val[i];
			n+=1;
		}
/*
* Square of sum
*/
		sum_val /= (double)n;
		sum_sqrd_val = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			sum_sqrd_val += (val[i] - sum_val) * (val[i] - sum_val);
		}
		sum_sqrd_val /= (double)(n-1);

	}

	if(out_data_type == NCL_double) {
		*((double*)out1_val) = sum_sqrd_val;
	} else {
		*((float*)out1_val) = (float)sum_sqrd_val;
	}

	if(did_coerce)_NclDestroyObj((NclObj)tmp_md);
	return(NclReturnValue(
		out1_val,
		1,
		&dimsizes,
		NULL,
		out_data_type,
		0
	));
}
NhlErrorTypes _NclIdim_stddev
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	double sum_val ;
	double sum_sqrd_val ;
	double *val = NULL;
	ng_size_t *dimsizes = NULL;
	ng_size_t i,j;
	ng_size_t m,n;
	ng_size_t nd;
	ng_size_t count;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int start;
	int did_coerce = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double)* n);
		out_data_type = NCL_double;
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev: Could not coerce input data to double, can't continue");
			return(NhlFATAL);
		} else if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
		did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			for(j = 0; j < m; j++) {
				if(val[i*m+j] != tmp_md->multidval.missing_value.value.doubleval) {
					break;
				}
			}
			count = 0;

			if(j==m) {
				if(out_data_type == NCL_double) {
					((double*)out_val)[i] = missing.doubleval;
				} else {
					((float*)out_val)[i] = (float)missing.doubleval;
				}
			} else {
				count = 1;
				start = j;
				sum_val = val[(i*m) + j];
	
				j = j+1;
				for(; j<m;j++){
					if(val[(i*m) + j] != tmp_md->multidval.missing_value.value.doubleval) {
						sum_val+= val[(i*m) + j];
						count = count +1;
					}
				}
				if(count != 1) {
					sum_val = sum_val/(double) count;
					sum_sqrd_val = 0;
					for(j = start; j < m; j++) {
						if(val[(i*m)+j]!= tmp_md->multidval.missing_value.value.doubleval) {        
							sum_sqrd_val += (val[(i*m)+j] - sum_val) * (val[(i*m)+j] - sum_val);
						}
					}
					sum_sqrd_val /= (double)(count-1);
					sum_sqrd_val = sqrt(sum_sqrd_val);
				} else {
					sum_sqrd_val = 0.0;
				}
				if(out_data_type == NCL_double) {
                                        ((double*)out_val)[i] = sum_sqrd_val;
                                } else {
                                        ((float*)out_val)[i] = (float)sum_sqrd_val;
                                }
			}
		}
		if(out_data_type != NCL_double) {
			missing.floatval = (float)missing.doubleval;
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&missing,
			out_data_type,
			0);
		NclFree(dimsizes);
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
		return(ret);
	} else {
		for(i = 0; i < n ; i++) {
			sum_val = val[i*m];
			for(j = 1; j< m; j++) {
				sum_val += val[i*m + j];
			}
			sum_val = sum_val/(double)m;
			sum_sqrd_val = 0;
			for(j = 0; j < m; j++) {
				sum_sqrd_val += (val[(i*m)+j] - sum_val) * (val[(i*m)+j] - sum_val);
			}
			sum_sqrd_val /= (double)(m-1);
			sum_sqrd_val = sqrt(sum_sqrd_val);
                        if(out_data_type == NCL_double) {
				((double*)out_val)[i] = sum_sqrd_val;
			} else {
				((float*)out_val)[i] = (float)sum_sqrd_val;
			}

		}	
	
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			out_data_type,
			0);
		NclFree(dimsizes);
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
		return(ret);
	}

}

NhlErrorTypes _NclIdim_stddev_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
        int *dims;
	ng_size_t ndims;
	double sum_val ;
	double sum_sqrd_val ;
	double *val = NULL;
	ng_size_t *dimsizes = NULL;
	ng_size_t i,j,k;
	ng_size_t i_in,i_out;
	ng_size_t m,n,nr,nl;
	int nd;
	ng_size_t count;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int start;
	int did_coerce = 0;

/*
 * Get dimension(s) to do average across.
 */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev_n: Invalid dimension sizes to do average across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}
/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * Determine output type, which will either be float or double.
 */
	if(tmp_md->multidval.data_type == NCL_double) {
	  out_val = (void*)NclMalloc(sizeof(double)* n);
	  out_data_type = NCL_double;
	  if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	} else {
	  out_val = (void*)NclMalloc(sizeof(float)* n);
	  out_data_type = NCL_float;
	  tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp_md == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev_n: Could not coerce input data to double, can't continue");
	    return(NhlFATAL);
	  } else if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	  did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out = i*nr + j;
/*
 * Check if we have at least one non-missing value in the subset
 * of rightmost values.
 */
	      for(k = 0; k < m; k++) {
		i_in = i*(nr*m)+(k*nr)+j;
		if(val[i_in] != tmp_md->multidval.missing_value.value.doubleval) {
		  break;
		}
	      }
	      count = 0;

	      if(k==m) {
		if(out_data_type == NCL_double) {
		  ((double*)out_val)[i_out] = missing.doubleval;
		} else {
		  ((float*)out_val)[i_out] = (float)missing.doubleval;
		}
	      } else {
/*
 * Values were NOT all missing for this subset, so do the variance,
 * starting with the first non-missing value.
 */
		count = 1;
		start = k;
		sum_val = val[i_in];
	
		k = k+1;
		for(; k<m;k++){
		  i_in = i*(nr*m)+(k*nr)+j;
		  if(val[i_in] != tmp_md->multidval.missing_value.value.doubleval) {
		    sum_val+= val[i_in];
		    count = count +1;
		  }
		}
		if(count != 1) {
		  sum_val = sum_val/(double) count;
		  sum_sqrd_val = 0;
		  for(k = start; k < m; k++) {
		    i_in = i*(nr*m)+(k*nr)+j;
		    if(val[i_in] != tmp_md->multidval.missing_value.value.doubleval) {        
		      sum_sqrd_val += (val[i_in] - sum_val) * (val[i_in] - sum_val);
		    }
		  }
		  sum_sqrd_val /= (double)(count-1);
		  sum_sqrd_val = sqrt(sum_sqrd_val);
		} else {
		  sum_sqrd_val = 0.0;
		}
/*
 * Set to appropriate type for output.
 */
		if(out_data_type == NCL_double) {
		  ((double*)out_val)[i_out] = sum_sqrd_val;
		} else {
		  ((float*)out_val)[i_out] = (float)sum_sqrd_val;
		}
	      }
	    }
	  }
	  if(out_data_type != NCL_double) {
	    missing.floatval = (float)missing.doubleval;
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &missing,
			       out_data_type,
			       0);
	  NclFree(dimsizes);
	  if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
	  return(ret);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so we 
 * can proceed without worrying about missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out = i*nr + j;
	      sum_val = val[i*(nr*m)+j];   /* k = 0 */
	      for(k = 1; k< m; k++) {
		i_in = i*(nr*m)+(k*nr)+j;
		sum_val += val[i_in];
	      }
	      sum_val = sum_val/(double)m;
	      sum_sqrd_val = 0;
	      for(k = 0; k< m; k++) {
		i_in = i*(nr*m)+(k*nr)+j;
		sum_sqrd_val += (val[i_in] - sum_val) * (val[i_in] - sum_val);
	      }
	      sum_sqrd_val /= (double)(m-1);
	      sum_sqrd_val = sqrt(sum_sqrd_val);
	      if(out_data_type == NCL_double) {
		((double*)out_val)[i_out] = sum_sqrd_val;
	      } else {
		((float*)out_val)[i_out] = (float)sum_sqrd_val;
	      }
	    }	
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       out_data_type,
			       0);
	  NclFree(dimsizes);
	  if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
	  return(ret);
	}
}

NhlErrorTypes _NclIstddev
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	double sum_val;
	double sum_sqrd_val;
	double *val;
	void *out1_val;
	ng_size_t dimsizes = 1;
	ng_size_t n;
	ng_size_t  i;
	NclBasicDataTypes out_data_type;
	NclScalar missing;
	int did_coerce = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	if(tmp_md->multidval.data_type == NCL_double) {
		out1_val = (void*)NclMalloc(sizeof(double));
		if((tmp_md->multidval.n_dims ==1)&&( tmp_md->multidval.dim_sizes[0] ==1)) {
			*(double*)out1_val = 0.0;
		        return(NclReturnValue(
               		 	out1_val,
                		1,
                		&dimsizes,
                		NULL,
                		NCL_double,
                		0
        		));

		}
                if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
		out_data_type = NCL_double;
	} else {
		out1_val = (void*)NclMalloc(sizeof(float));
		if((tmp_md->multidval.n_dims ==1)&&( tmp_md->multidval.dim_sizes[0] ==1)) {
			*(float*)out1_val = 0.0;
		        return(NclReturnValue(
               		 	out1_val,
                		1,
                		&dimsizes,
                		NULL,
                		NCL_float,
                		0
        		));
		}
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
                if(tmp_md == NULL) {
                	NhlPError(NhlFATAL,NhlEUNKNOWN,"stddev: Could not coerce input data to double, can't continue");
                	return(NhlFATAL);
                } else if(tmp_md->multidval.missing_value.has_missing){
                	missing = tmp_md->multidval.missing_value.value;
                }
		did_coerce = 1;
	}
	val = (double*)tmp_md->multidval.val;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(val[i]!= tmp_md->multidval.missing_value.value.doubleval) {
				break;
			}
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				NhlPError(NhlWARNING,NhlEUNKNOWN,"stddev: Entire input array contains missing values, can't compute standard deviation");
				if(out_data_type == NCL_double) {
					*(double*)out1_val = missing.doubleval;
				} else {
					*(float*)out1_val = (float)missing.doubleval;
					missing.floatval = (float)missing.doubleval;
				}
				if(did_coerce)_NclDestroyObj((NclObj)tmp_md);
				return(NclReturnValue(
					out1_val,
					1,
					&dimsizes,
					&missing,
					out_data_type,
					0
				));
		}
		n = 1;
		sum_val = val[i];	
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
	
			if(val[i]!= missing.doubleval) {	
				sum_val += val[i];
				n+=1;
	
			}
		}
		if(n != 1) {
	/*
	* Square of sum
	*/
			sum_val /= (double)n;
			sum_sqrd_val = 0;
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				if(val[i]!= missing.doubleval) {        
					sum_sqrd_val += (val[i] - sum_val) * (val[i] - sum_val);
				}
			}
			sum_sqrd_val /= (double)(n-1);
			sum_sqrd_val = sqrt(sum_sqrd_val);
		} else {
			sum_sqrd_val = 0.0;
		}


	} else {
		i = 0;
		n = 1;
		sum_val = val[i];	
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
			sum_val += val[i];
			n+=1;
		}
/*
* Square of sum
*/
		sum_val /= (double)n;
		sum_sqrd_val = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			sum_sqrd_val += (val[i] - sum_val) * (val[i] - sum_val);
		}
		sum_sqrd_val /= (double)(n-1);
		sum_sqrd_val = sqrt(sum_sqrd_val);

	}

	if(out_data_type == NCL_double) {
		*((double*)out1_val) = sum_sqrd_val;
	} else {
		*((float*)out1_val) = (float)sum_sqrd_val;
	}

	if(did_coerce)_NclDestroyObj((NclObj)tmp_md);
	return(NclReturnValue(
		out1_val,
		1,
		&dimsizes,
		NULL,
		out_data_type,
		0
	));
}
NhlErrorTypes _Nclavg
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	double sum_val;
	double *val;
	void *out_val;
	ng_size_t dimsizes = 1;
	logical *tmp = NULL;
	ng_size_t n;
	ng_size_t  i;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
	NclScalar missing;
	int did_coerce = 0;


	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
		out_data_type = NCL_double;
		out_val = (void*)NclMalloc(sizeof(double));
		if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
		val = (double*)tmp_md->multidval.val;
	} else {
		out_data_type = NCL_float;
		out_val = (void*)NclMalloc(sizeof(float));
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
                if(tmp_md == NULL) {
                	NhlPError(NhlFATAL,NhlEUNKNOWN,"avg: Could not coerce input data to float, can't continue");
                	return(NhlFATAL);
               	} else if(tmp_md->multidval.missing_value.has_missing){
              		missing = tmp_md->multidval.missing_value.value;
                }
		did_coerce = 1;
		val = (double*)tmp_md->multidval.val;
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(val[i] != tmp_md->multidval.missing_value.value.doubleval) {
				break;
			}
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				NhlPError(NhlWARNING,NhlEUNKNOWN,"avg: Entire input array contains missing values, can't compute average");
				if(out_data_type == NCL_double) {
					NclFree(tmp);
					memcpy(out_val,&missing,the_type->type_class.size);
					if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
					return(NclReturnValue(
						out_val,
						1,
						&dimsizes,
						&missing,
						out_data_type,
						0
					));
				} else {
					NclFree(tmp);
					*((float*)out_val) = (float)tmp_md->multidval.missing_value.value.doubleval;
					if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
					missing.floatval = (float)tmp_md->multidval.missing_value.value.doubleval;
					return(NclReturnValue(
						out_val,
						1,
						&dimsizes,
						&missing,
						out_data_type,
						0
					));
				}
		}
		n = 1;
		sum_val = val[i];
		
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
			if(val[i] != tmp_md->multidval.missing_value.value.doubleval) {
				sum_val += val[i];
				n = n+1;
			}
		}
		sum_val = sum_val/(double)n;
		if(out_data_type == NCL_double) {
			*(double*)out_val = sum_val;
		} else {
			*(float*)out_val = (float)sum_val;
			missing.floatval = (float)missing.doubleval;
		}
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
                return(NclReturnValue(
                   out_val,
                   1,
                   &dimsizes,
                   &missing,
		   out_data_type,
                   0
                   ));
		
	} else {
		sum_val = val[0];
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			sum_val += val[i];
		}
		sum_val = sum_val / (double)tmp_md->multidval.totalelements;
		if(out_data_type == NCL_double) {
			*(double*)out_val = sum_val;
		} else {
			*(float*)out_val = (float)sum_val;
		}
		if(did_coerce) _NclDestroyObj((NclObj)tmp_md);
                return(NclReturnValue(
                   out_val,
                   1,
                   &dimsizes,
                   NULL,
		   out_data_type,
                   0
                   ));
	}
}

NhlErrorTypes _Nclnum
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t dimsizes = 1;
	logical *tmp;
	ng_size_t count;
	ng_size_t  i;
	int icount,return_size;
	long lcount;
	NclBasicDataTypes return_type;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * The rules for when to return an int versus a long:
 *    - On a 32-bit system, return int.
 *    - On a 64-bit system, return long if the count > INT_MAX.
 */
	return_size = ((NclTypeClass)nclTypeintClass)->type_class.size;
	return_type = NCL_int;

	tmp = (logical*)tmp_md->multidval.val;
	count = 0;
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval))
				count++;
		}
	} else {
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(tmp[i])
				count++;
		}
	}
#if !defined(NG32BIT)
	if(count > INT_MAX) {
	  return_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
	  return_type = NCL_long;
	}
#endif
	out_val = (void*)NclMalloc(return_size);
	if(return_type == NCL_int) {
	  icount = (int)count;
	  memcpy(out_val,&icount,return_size);
	}
	else {
	  lcount = (long)count;
	  memcpy(out_val,&lcount,return_size);
	}

	return(NclReturnValue(
			      out_val,
			      1,
			      &dimsizes,
			      NULL,
			      return_type,
			      0
			      ));
}
NhlErrorTypes _Nclind
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t dimsizes = 1;
	logical *tmp, return_int;
	ng_size_t j, count;
	ng_size_t  i;
	int return_size;
	NclBasicDataTypes return_type;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * The rules for when to return an int versus a long:
 *    - On a 32-bit system, return ints.
 *    - On a 64-bit system, return longs if any indexes > INT_MAX.
 *
 * There are six main sections below, handling six possible ways
 * of returning indexes:
 *
 *   - The input array has a _FillValue, count>0, and returning ints.
 *   - The input array has a _FillValue, count>0, and returning longs.
 *   - The input array has a _FillValue, count=0, return int msg value
 *   - The input array doesn't have a _FillValue, count>0, and returning ints.
 *   - The input array doesn't have a _FillValue, count>0, and returning longs.
 *   - The input array doesn't have a _FillValue, count=0, return int msg value
 */
	return_int = True;
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input array has a _FillValue.
 */
		tmp = (logical*)tmp_md->multidval.val;
		count = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		  if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval)) {
		    count++;
#if !defined(NG32BIT)
		    if(return_int && i > INT_MAX) {
		      return_int = False;
		    }
#endif
		  }
		}
		if(return_int) {
		  return_size = ((NclTypeClass)nclTypeintClass)->type_class.size;
		  return_type = NCL_int;
		}
		else {
		  return_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
		  return_type = NCL_long;
		}
		if(count > 0) {
		  out_val = (void*)NclMalloc(return_size * count);
		  if(return_int) {
/*
 * Return ints because indexes <= INT_MAX.
 */
		    j = 0;
		    for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		      if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval)) {
			((int*)out_val)[j] = (int)i;
			j++;
		      }
		    }
		  }
		  else {
/*
 * Return longs b/c one or more indexes > INT_MAX.
 */
		    j = 0;
		    for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		      if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval)) {
			((long*)out_val)[j] = (long)i;
			j++;
		      }
		    }
		  }
		} else {
/*
 * No indexes found, so return missing.
 */
		  out_val = (void*)NclMalloc(return_size);
		  memcpy(out_val,&((NclTypeClass)nclTypeintClass)->type_class.default_mis,return_size);
		  return(NclReturnValue(
				out_val,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				((NclTypeClass)nclTypeintClass)->type_class.data_type,
				0
			));
		}
	} else {
/*
 * The input array doesn't have a _FillValue.
 */
		tmp = (logical*)tmp_md->multidval.val;
		count = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		  if(tmp[i]) {
		    count++;
#if !defined(NG32BIT)
		    if(return_int && i > INT_MAX) {
		      return_int = False;
		    }
#endif
		  }
		}
		if(return_int) {
		  return_size = ((NclTypeClass)nclTypeintClass)->type_class.size;
		  return_type = NCL_int;
		}
		else {
		  return_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
		  return_type = NCL_long;
		}
		if(count > 0) {
		  out_val = (void*)NclMalloc(return_size * count);
		  if(return_int) {
/*
 * Return ints because indexes <= INT_MAX.
 */
		    j = 0;
		    for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		      if(tmp[i]) {
			((int*)out_val)[j] = (int)i;
			j++;
		      }
		    }
		  }
		  else {
/*
 * Return longs because one or more indexes > INT_MAX.
 */
		    j = 0;
		    for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		      if(tmp[i]) {
			((long*)out_val)[j] = (long)i;
			j++;
		      }
		    }
		  }
		} else {
/*
 * No indexes found, so return missing.
 */
			out_val = (void*)NclMalloc(return_size);
			memcpy(out_val,&((NclTypeClass)nclTypeintClass)->type_class.default_mis,return_size);
			return(NclReturnValue(
				out_val,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				((NclTypeClass)nclTypeintClass)->type_class.data_type,
				0
			       ));
		}
	}

	return(NclReturnValue(
		out_val,
		1,
		&j,
		NULL,
		return_type,
		0
	));
}

NhlErrorTypes _Nclispan
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data0;
	NclStackEntry data1;
	NclStackEntry data2;
	NclMultiDValData tmp_md0 = NULL;
	NclMultiDValData tmp_md1 = NULL;
	NclMultiDValData tmp_md2 = NULL;
	NclBasicDataTypes   data0_type, data1_type, data2_type;
	NclBasicDataTypes ret_type = NCL_int;
	NclObjTypes obj_type = Ncl_Typeint;
	ng_size_t dimsizes = 1;

	data0 = _NclGetArg(0,3,DONT_CARE);
	switch(data0.kind) {
	case NclStk_VAR:
		tmp_md0 = _NclVarValueRead(data0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md0 = (NclMultiDValData)data0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md0 == NULL)
		return(NhlFATAL);
	data0_type = tmp_md0->multidval.data_type;

	data1 = _NclGetArg(1,3,DONT_CARE);
	switch(data1.kind) {
	case NclStk_VAR:
		tmp_md1 = _NclVarValueRead(data1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md1 = (NclMultiDValData)data1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md1 == NULL)
		return(NhlFATAL);
	data1_type = tmp_md1->multidval.data_type;

	data2 = _NclGetArg(2,3,DONT_CARE);
	switch(data2.kind) {
	case NclStk_VAR:
		tmp_md2 = _NclVarValueRead(data2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md2 = (NclMultiDValData)data2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md2 == NULL)
		return(NhlFATAL);
	data2_type = tmp_md2->multidval.data_type;

	if(_NclIsMissing(tmp_md0,tmp_md0->multidval.val)||
	   _NclIsMissing(tmp_md1,tmp_md1->multidval.val)||
	   _NclIsMissing(tmp_md2,tmp_md2->multidval.val)) {

		NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: Missing value detected in input, can't continue");
		return(NhlFATAL);
	}

/*
 * Determine the return type: NCL_int, NCL_long, or NCL_int64
 */
	if ((data0_type == NCL_int64) || (data1_type == NCL_int64) || 
	    (data2_type == NCL_int64)) {
		ret_type = NCL_int64;
		obj_type = Ncl_Typeint64;
	}
	else if ((data0_type == NCL_long) || (data1_type == NCL_long) || 
		 (data2_type == NCL_long)) {
		ret_type = NCL_long;
		obj_type = Ncl_Typelong;
	}
	else if (((data0_type == NCL_byte)  || 
		  (data0_type == NCL_short) ||
                  (data0_type == NCL_int)) &&
		 ((data1_type == NCL_byte)  || 
		  (data1_type == NCL_short) ||
                  (data1_type == NCL_int)) &&
		 ((data2_type == NCL_byte)  ||
		  (data2_type == NCL_short) ||
                  (data2_type == NCL_int))) {
		ret_type = NCL_int;
		obj_type = Ncl_Typeint;
	}
	else {
		NhlPError(NhlFATAL, NhlEUNKNOWN,
			  "ispan: arguments must be of an integral type, can't continue");
		return NhlFATAL;
	}

	tmp_md0 = _NclCoerceData(tmp_md0, obj_type, NULL);
	tmp_md1 = _NclCoerceData(tmp_md1, obj_type, NULL);
	tmp_md2 = _NclCoerceData(tmp_md2, obj_type, NULL);

/*
 * Three possible return types: NCL_int, NCL_long, or NCL_int64
 */
	switch (ret_type) {
        case NCL_int:
	{
        	int *out_val;
        	int i;
        	int strt;
        	int fnsh;
        	int spacing;

		spacing = *(int*)tmp_md2->multidval.val;
		if(spacing < 1) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: spacing parameter must be positive and non-zero");
			return(NhlFATAL);
		}
		fnsh = *(int*)tmp_md1->multidval.val;
		strt = *(int*)tmp_md0->multidval.val;

		dimsizes = (ng_size_t)abs(fnsh-strt)/spacing + 1;

		if((fnsh - strt) > 0) {
			out_val = (int*)NclMalloc(dimsizes*sizeof(int));
			for(i = 0; i < dimsizes; i++) {
				out_val[i] = strt + i * spacing;
			}
		} else if((fnsh - strt) < 0) {
			out_val = (int*)NclMalloc(dimsizes*sizeof(int));
			for(i = 0; i < dimsizes; i++) {
				out_val[i] = strt - i * spacing;
			}
		} else {
			out_val = (int*)NclMalloc(sizeof(int));
			*out_val = strt;
			dimsizes = 1;
		}
		return(NclReturnValue(
			       out_val,
			       1,
			       &dimsizes,
			       NULL,
			       ret_type,
			       0
			       ));
        }
	break;

        case NCL_long:
	{
        	long *out_val;
        	long i;
        	long strt;
        	long fnsh;
        	long spacing;

        	spacing = *(long*)tmp_md2->multidval.val;
        	if(spacing < 1) {
		        NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: spacing parameter must be positive and non-zero");
        		return(NhlFATAL);
        	}

        	fnsh = *(long*)tmp_md1->multidval.val;
        	strt = *(long*)tmp_md0->multidval.val;

        	dimsizes = (ng_size_t)labs(fnsh-strt)/spacing + 1;

        	if((fnsh - strt) > 0) {
        		out_val = (long*)NclMalloc(dimsizes*sizeof(long));
			if(out_val == NULL) {
			  NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: cannot allocate memory for output array");
			  return(NhlFATAL);
			}
        		for(i = 0; i < dimsizes; i++) {
		        	out_val[i] = strt + i * spacing;
        		}
        	} else if((fnsh - strt) < 0) {
		        out_val = (long*)NclMalloc(dimsizes*sizeof(long));
			if(out_val == NULL) {
			  NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: cannot allocate memory for output array");
			  return(NhlFATAL);
			}
        		for(i = 0; i < dimsizes; i++) {
		        	out_val[i] = strt - i * spacing;
        		}
        	} else {
		        out_val = (long*)NclMalloc(sizeof(long));
			if(out_val == NULL) {
			  NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: cannot allocate memory for output array");
			  return(NhlFATAL);
			}
        		*out_val = strt;
		        dimsizes = 1;
	        }
		return(NclReturnValue(
			       out_val,
			       1,
			       &dimsizes,
			       NULL,
			       ret_type,
			       0
			       ));
	}
        case NCL_int64:
	{
        	long long *out_val;
        	long long i;
        	long long strt;
        	long long fnsh;
        	long long spacing;

        	spacing = *(long long*)tmp_md2->multidval.val;
        	if(spacing < 1) {
		        NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: spacing parameter must be positive and non-zero");
        		return(NhlFATAL);
        	}

        	fnsh = *(long long*)tmp_md1->multidval.val;
        	strt = *(long long*)tmp_md0->multidval.val;

        	dimsizes = (ng_size_t)_Ncl_llabs(fnsh-strt)/spacing + 1;

        	if((fnsh - strt) > 0) {
        		out_val = (long long*)NclMalloc(dimsizes*sizeof(long long));
			if(out_val == NULL) {
			  NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: cannot allocate memory for output array");
			  return(NhlFATAL);
			}
        		for(i = 0; i < dimsizes; i++) {
		        	out_val[i] = strt + i * spacing;
        		}
        	} else if((fnsh - strt) < 0) {
		        out_val = (long long*)NclMalloc(dimsizes*sizeof(long long));
			if(out_val == NULL) {
			  NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: cannot allocate memory for output array");
			  return(NhlFATAL);
			}
        		for(i = 0; i < dimsizes; i++) {
		        	out_val[i] = strt - i * spacing;
        		}
        	} else {
		        out_val = (long long*)NclMalloc(sizeof(long long));
			if(out_val == NULL) {
			  NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: cannot allocate memory for output array");
			  return(NhlFATAL);
			}
        		*out_val = strt;
		        dimsizes = 1;
	        }
		return(NclReturnValue(
			       out_val,
			       1,
			       &dimsizes,
			       NULL,
			       ret_type,
			       0
			       ));
	}
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
	}
}

NhlErrorTypes _Nclfspan
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry   data0,
		data1,
		data2;

	NclMultiDValData    tmp_md0 = NULL,
		tmp_md1 = NULL,
		tmp_md2 = NULL;

	NclBasicDataTypes   data0_type,
		data1_type,
		data2_type;

	ng_size_t dimsizes = 1;
	ng_size_t i;

 /*
  * The internal calculation is now done in double precision, regardless
  * of the type of the input. This was implemented after 6.0.0 was 
  * released, due to some precision issues that users were having.
  */
	double  strt,           /* span start */
	        fnsh,           /* span finish */
              spacing;          /* span interval */

	void    *out_val;       /* may be of type float or of type double */

    /*
     * get arguments and associated data info
     */
    data0 = _NclGetArg(0, 3, DONT_CARE);
    switch (data0.kind) {
    case NclStk_VAR:
            tmp_md0 = _NclVarValueRead(data0.u.data_var, NULL, NULL);
            break;

    case NclStk_VAL:
            tmp_md0 = (NclMultiDValData) data0.u.data_obj;
            break;
    default:
	    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
	    return(NhlFATAL);
    }

    if (tmp_md0 == NULL)
        return NhlFATAL;

    data1 = _NclGetArg(1, 3, DONT_CARE);
    switch (data1.kind) {
    case NclStk_VAR:
            tmp_md1 = _NclVarValueRead(data1.u.data_var, NULL, NULL);
            break;

    case NclStk_VAL:
            tmp_md1 = (NclMultiDValData) data1.u.data_obj;
            break;
    default:
	    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
	    return(NhlFATAL);
    }

    if (tmp_md1 == NULL)
        return NhlFATAL;

    /* number of equally spaced points */
    data2 = _NclGetArg(2, 3, DONT_CARE);
    switch (data2.kind) {
    case NclStk_VAR:
            tmp_md2 = _NclVarValueRead(data2.u.data_var,NULL,NULL);
            break;

    case NclStk_VAL:
            tmp_md2 = (NclMultiDValData)data2.u.data_obj;
            break;
    default:
	    NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
	    return(NhlFATAL);
    }

    if (tmp_md2 == NULL)
        return NhlFATAL;

    if (_NclIsMissing(tmp_md0,tmp_md0->multidval.val) ||
	_NclIsMissing(tmp_md1,tmp_md1->multidval.val) ||
	_NclIsMissing(tmp_md2,tmp_md2->multidval.val)) {

        NhlPError(NhlFATAL, NhlEUNKNOWN, "fspan: Missing value detected in input, can't continue");
        return NhlFATAL;
    }

    data2_type = tmp_md2->multidval.data_type;
    if ((data2_type == NCL_float) || (data2_type == NCL_double)) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "fspan: number of elements parameter must be of an integral type, cannot continue");
        return NhlFATAL;
    }

    switch (data2_type) {
    case NCL_byte:
	    dimsizes = (ng_size_t)*(byte *) tmp_md2->multidval.val;
            break;
    case NCL_short:
            dimsizes = (ng_size_t)*(short *) tmp_md2->multidval.val;
            break;
    case NCL_int:
            dimsizes = (ng_size_t)*(int *) tmp_md2->multidval.val;
            break;
    case NCL_long:
            dimsizes = *(ng_size_t *)(long *) tmp_md2->multidval.val;
            break;
    case NCL_int64:
            dimsizes = *(ng_size_t *) (long long*)tmp_md2->multidval.val;
            break;
    default:
	    NhlPError(NhlFATAL, NhlEUNKNOWN,
		      "fspan: invalid type passed for number of elements parameter, can't continue");
	    return NhlFATAL;
    }

    if (dimsizes <= 1) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "fspan: number of elements parameter is less-than-or-equal-to one, can't continue");
        return NhlFATAL;
    }

    data0_type = tmp_md0->multidval.data_type;
    data1_type = tmp_md1->multidval.data_type;

   /*
    * promote arguments to type double
    */
    tmp_md0 = _NclCoerceData(tmp_md0, Ncl_Typedouble, NULL);
    tmp_md1 = _NclCoerceData(tmp_md1, Ncl_Typedouble, NULL);

    if ((data0_type == NCL_double)  ||  (data1_type == NCL_double)) {

        if (dimsizes > 1) {
            fnsh = *(double *) tmp_md1->multidval.val;
            strt = *(double *) tmp_md0->multidval.val;

            spacing = (fnsh - strt) / (dimsizes - 1);

            out_val = (void *) NclMalloc(dimsizes * sizeof(double));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL,NhlEUNKNOWN,"fspan: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            for (i = 0; i < dimsizes; i++) {
                ((double *) out_val)[i] = strt + (i * spacing);
            }

            ((double *) out_val)[0]            = strt;
            ((double *) out_val)[dimsizes - 1] = fnsh;
        } else {
            /* dimsizes == 1 */
	  out_val = (void *) NclMalloc(sizeof(double));
	  if(out_val == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"fspan: cannot allocate memory for output array");
	    return(NhlFATAL);
	  }
	  ((double *) out_val)[0] =  *(double *) tmp_md0->multidval.val;
        }

        return NclReturnValue(out_val, 1, &dimsizes, NULL, NCL_double, 0);
    } else {

        if (dimsizes > 1) {
            fnsh = *(double *) tmp_md1->multidval.val;
            strt = *(double *) tmp_md0->multidval.val;

            spacing = (fnsh - strt) / (dimsizes - 1);

            out_val = (void *) NclMalloc(dimsizes * sizeof(float));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL,NhlEUNKNOWN,"fspan: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            for (i = 0; i < dimsizes; i++) {
	      ((float *) out_val)[i] = (float)(strt + (i * spacing));
            }

            ((float *) out_val)[0]          = (float)strt;
            ((float *) out_val)[dimsizes-1] = (float)fnsh;
        } else {
            /* dimsizes == 1 */
            out_val = (void *) NclMalloc(sizeof(float));
	    if(out_val == NULL) {
	      NhlPError(NhlFATAL,NhlEUNKNOWN,"fspan: cannot allocate memory for output array");
	      return(NhlFATAL);
	    }
            ((float *) out_val)[0] =  *(float *) tmp_md0->multidval.val;
        }
    
        return NclReturnValue(out_val, 1, &dimsizes, NULL, NCL_float, 0);
    }
}

NhlErrorTypes _Nclmask
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data0;
	NclStackEntry data1;
	NclStackEntry data2;
	NclMultiDValData tmp_md0 = NULL;
	NclMultiDValData tmp_md1 = NULL;
	NclMultiDValData tmp_md2 = NULL;
	void **out_val;
	ng_size_t j;
	ng_size_t  nblk = 0 ;
	int diff;  /* only used for dimensions */
        ng_size_t  i, n;
	void *tmp = NULL;
	logical *tmp0 = NULL;
	void *mask_grid = NULL;
	void *mask_val = NULL;
	NclTypeClass mask_type = NULL;

	data0 = _NclGetArg(0,3,DONT_CARE);
	switch(data0.kind) {
	case NclStk_VAR:
		tmp_md0 = _NclVarValueRead(data0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md0 = (NclMultiDValData)data0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md0 == NULL)
		return(NhlFATAL);

	data1 = _NclGetArg(1,3,DONT_CARE);
	switch(data1.kind) {
	case NclStk_VAR:
		tmp_md1 = _NclVarValueRead(data1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md1 = (NclMultiDValData)data1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md1 == NULL)
		return(NhlFATAL);
	data2 = _NclGetArg(2,3,DONT_CARE);
	switch(data2.kind) {
	case NclStk_VAR:
		tmp_md2 = _NclVarValueRead(data2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md2 = (NclMultiDValData)data2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md2 == NULL)
		return(NhlFATAL);

	if(tmp_md0->multidval.n_dims  < tmp_md1->multidval.n_dims) {
	
		NhlPError(NhlFATAL,NhlEUNKNOWN,"mask: mask variable (parameter 1) has more dimensions than parameter 0, can't mask");
		return(NhlFATAL);
	} else if (tmp_md0->multidval.n_dims >  tmp_md1->multidval.n_dims) {
		diff = tmp_md0->multidval.n_dims  - tmp_md1->multidval.n_dims;
		for(i = 0; i <  tmp_md1->multidval.n_dims; i++) {
			if(tmp_md0->multidval.dim_sizes[diff + i] != tmp_md1->multidval.dim_sizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"mask: dimension sizes  of parameter 0 and parameter 1 do not match");
				return(NhlFATAL);
			}
		}
	} else {
		for(i = 0; i < tmp_md0->multidval.n_dims; i++) {
			if(tmp_md0->multidval.dim_sizes[i] != tmp_md1->multidval.dim_sizes[i]) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"mask: dimension sizes  of parameter 0 and parameter 1 do not match");
				return(NhlFATAL);
			}
		}
	}

	if(tmp_md1->multidval.data_type != tmp_md2->multidval.data_type) {
		tmp = (void*)NclMalloc(tmp_md1->multidval.type->type_class.size);
		if(_Nclcoerce((NclTypeClass)tmp_md1->multidval.type,tmp,tmp_md2->multidval.val,1,NULL,NULL,(NclTypeClass)tmp_md2->multidval.type) == NhlFATAL)  {
			NclFree(tmp);
			tmp = (void*)NclMalloc(tmp_md2->multidval.type->type_class.size * tmp_md1->multidval.totalelements);
			if(_Nclcoerce((NclTypeClass)tmp_md2->multidval.type,tmp,tmp_md1->multidval.val,tmp_md1->multidval.totalelements,NULL,NULL,(NclTypeClass)tmp_md1->multidval.type) == NhlFATAL)  {
				NclFree(tmp);

				NhlPError(NhlFATAL,NhlEUNKNOWN,"mask: parameter 1 and parameter 2 must be the same types or coercible to each other");
				return(NhlFATAL);
			} else {
				mask_val = tmp_md2->multidval.val;
				mask_grid = tmp;
				mask_type = tmp_md2->multidval.type;
			}
		} else {
			mask_grid = tmp_md1->multidval.val;
			mask_val = tmp;
			mask_type = tmp_md1->multidval.type;
		}
	} else {
		mask_grid = tmp_md1->multidval.val;
		mask_val = tmp_md2->multidval.val;
		mask_type = tmp_md1->multidval.type;
	}


	tmp0 = (logical*)NclMalloc(sizeof(logical)*tmp_md0->multidval.totalelements);
	if(tmp_md0->multidval.totalelements != tmp_md1->multidval.totalelements) {
		n = tmp_md0->multidval.totalelements/tmp_md1->multidval.totalelements;
	} else {
		n = 1;
	}
	_Nclne(mask_type,tmp0,mask_grid,mask_val,NULL,NULL,tmp_md1->multidval.totalelements,1);
	out_val = (void*)NclMalloc(tmp_md0->multidval.totalsize);


	nblk = tmp_md1->multidval.totalelements*tmp_md0->multidval.type->type_class.size;
	for(j = 0; j < n; j++ ) {


		memcpy(&((char*)out_val)[j*nblk],&((char*)tmp_md0->multidval.val)[j*nblk],nblk);

		if(tmp_md0->multidval.missing_value.has_missing) {
			for(i = 0; i < tmp_md1->multidval.totalelements; i++) {
				if(tmp0[i]) {
					memcpy(&(((char*)out_val)[j*nblk + (i*tmp_md0->multidval.type->type_class.size)]),&tmp_md0->multidval.missing_value.value,tmp_md0->multidval.type->type_class.size);
				} 
			}
		} else {
			for(i = 0; i < tmp_md1->multidval.totalelements; i++) {
				if(tmp0[i]) {
					memcpy(&(((char*)out_val)[j*nblk + (i*tmp_md0->multidval.type->type_class.size)]),&tmp_md0->multidval.type->type_class.default_mis,tmp_md0->multidval.type->type_class.size);
				} 
			}
		
		}
	}
	if(tmp != NULL) 	
		NclFree(tmp);
	NclFree(tmp0);
	return(NclReturnValue(
		out_val,
		tmp_md0->multidval.n_dims,
		tmp_md0->multidval.dim_sizes,
		(tmp_md0->multidval.missing_value.has_missing? &tmp_md0->multidval.missing_value.value:&tmp_md0->multidval.type->type_class.default_mis),
		tmp_md0->multidval.data_type,
		0
	));
}
/*
 * Simply:
 * for all elements of cond_md:
 *     if True:
 *        out_val_md = true_val_md
 *     else:
 *        out_val_md = false_val_md
 */


NhlErrorTypes _Nclwhere
#if	NhlNeedProto
(void)
#else
()
#endif
{

	NclStackEntry data0;
	NclStackEntry data1;
	NclStackEntry data2;
	NclMultiDValData cond_md = NULL;
	NclMultiDValData true_val_md = NULL;
	NclMultiDValData false_val_md = NULL;
	NclMultiDValData val_md = NULL;
	void *out_val;
	int i;
	ng_size_t  j;
	void *tmp = NULL;
	void *true_val = NULL;
	void *false_val = NULL;
	logical *cond_val = NULL;
	NclTypeClass val_type = NULL;
	int true_has_missing = 0,false_has_missing = 0,cond_has_missing = 0;
	int check_true = 0, check_false = 0;
	NclScalar missing_val, check_missing_val;
	int return_missing = False;
	

	data0 = _NclGetArg(0,3,DONT_CARE);
	switch(data0.kind) {
	case NclStk_VAR:
		cond_md = _NclVarValueRead(data0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		cond_md = (NclMultiDValData)data0.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(cond_md == NULL)
		return(NhlFATAL);
	if (cond_md->multidval.missing_value.has_missing)
		cond_has_missing = 1;
	

	data1 = _NclGetArg(1,3,DONT_CARE);
	switch(data1.kind) {
	case NclStk_VAR:
		true_val_md = _NclVarValueRead(data1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		true_val_md = (NclMultiDValData)data1.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	data2 = _NclGetArg(2,3,DONT_CARE);
	switch(data2.kind) {
	case NclStk_VAR:
		false_val_md = _NclVarValueRead(data2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		false_val_md = (NclMultiDValData)data2.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if (true_val_md == NULL || false_val_md == NULL)
		return(NhlFATAL);
	if (true_val_md->multidval.missing_value.has_missing) {
		true_has_missing = 1;
		return_missing = 1;
	}
	if (false_val_md->multidval.missing_value.has_missing) {
		false_has_missing = 1;
		return_missing = 1;
	}

	if (true_val_md->multidval.kind != SCALAR)  {
		if(cond_md->multidval.n_dims  != true_val_md->multidval.n_dims) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"where: condition variable (parameter 0) dimension mismatch with parameter 1");
			return(NhlFATAL);
		}
		else {
			for(i = 0; i < cond_md->multidval.n_dims; i++) {
				if(cond_md->multidval.dim_sizes[i] != true_val_md->multidval.dim_sizes[i]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "where: dimension sizes  of parameter 0 and parameter 1 do not match");
					return(NhlFATAL);
				}
			}
		}
	}
	if (false_val_md->multidval.kind != SCALAR) {
		if(cond_md->multidval.n_dims  != false_val_md->multidval.n_dims) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"where: condition variable (parameter 0) dimension mismatch with parameter 2");
			return(NhlFATAL);
		}
		else {
			for(i = 0; i < cond_md->multidval.n_dims; i++) {
				if(cond_md->multidval.dim_sizes[i] != false_val_md->multidval.dim_sizes[i]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "where: dimension sizes  of parameter 0 and parameter 2 do not match");
					return(NhlFATAL);
				}
			}
		}
	}
	if(true_val_md->multidval.data_type != false_val_md->multidval.data_type) {
		tmp = (void*)NclMalloc(true_val_md->multidval.type->type_class.size * false_val_md->multidval.totalelements);
		if(_Nclcoerce((NclTypeClass)true_val_md->multidval.type,tmp,
			      false_val_md->multidval.val,false_val_md->multidval.totalelements,
			      NULL,NULL,(NclTypeClass)false_val_md->multidval.type) == NhlFATAL)  {
			NclFree(tmp);
			tmp = (void*)NclMalloc(false_val_md->multidval.type->type_class.size * true_val_md->multidval.totalelements);
			if(_Nclcoerce((NclTypeClass)false_val_md->multidval.type,tmp,
				      true_val_md->multidval.val,true_val_md->multidval.totalelements,
				      NULL,NULL,(NclTypeClass)true_val_md->multidval.type) == NhlFATAL)  {
				NclFree(tmp);
				NhlPError(NhlFATAL,NhlEUNKNOWN,
					  "where: parameter 1 and parameter 2 must be the same types or one must be coercible to the other");
				return(NhlFATAL);
			} else {
				false_val = false_val_md->multidval.val;
				true_val = tmp;
				val_type = false_val_md->multidval.type;
				val_md = false_val_md;
				if (false_has_missing && true_has_missing) {
					_NclScalarCoerce(&true_val_md->multidval.missing_value.value,true_val_md->multidval.data_type,
							 &check_missing_val,val_md->multidval.data_type);
					if (memcmp(&val_md->multidval.missing_value.value,&check_missing_val,val_type->type_class.size)) {
						check_true = 1;
					}
					missing_val = val_md->multidval.missing_value.value;
				}
				else if (false_has_missing)
					missing_val = val_md->multidval.missing_value.value;
				else if (true_has_missing)
					_NclScalarCoerce(&true_val_md->multidval.missing_value.value,true_val_md->multidval.data_type,
							 &missing_val,val_md->multidval.data_type);
				else
					missing_val = val_md->multidval.type->type_class.default_mis;
			}
		} else {
			true_val = true_val_md->multidval.val;
			false_val = tmp;
			val_type = true_val_md->multidval.type;
			val_md = true_val_md;
			if (true_has_missing && false_has_missing) {
				_NclScalarCoerce(&false_val_md->multidval.missing_value.value,false_val_md->multidval.data_type,
						 &check_missing_val,val_md->multidval.data_type);
				if (memcmp(&val_md->multidval.missing_value.value,&check_missing_val,val_type->type_class.size)) {
					check_false = 1;
				}
				missing_val = val_md->multidval.missing_value.value;
			}
			else if (true_has_missing)
				missing_val = val_md->multidval.missing_value.value;
			else if (false_has_missing)
				_NclScalarCoerce(&false_val_md->multidval.missing_value.value,false_val_md->multidval.data_type,
							 &missing_val,val_md->multidval.data_type);
			else
				missing_val = val_md->multidval.type->type_class.default_mis;
		}
	} else {
		true_val = true_val_md->multidval.val;
		false_val = false_val_md->multidval.val;
		val_type = true_val_md->multidval.type;
		val_md = true_val_md;
		if (true_has_missing && false_has_missing) {
			if (memcmp(&val_md->multidval.missing_value.value,
				   &false_val_md->multidval.missing_value.value,val_type->type_class.size)) {
				check_missing_val = false_val_md->multidval.missing_value.value;
				check_false = 1;
			}
			missing_val = val_md->multidval.missing_value.value;
		}
		else if (true_has_missing)
			missing_val = val_md->multidval.missing_value.value;
		else if (false_has_missing)
			missing_val = false_val_md->multidval.missing_value.value;
		else
			missing_val = val_md->multidval.type->type_class.default_mis;
	}

	out_val = (void*)NclMalloc(cond_md->multidval.totalelements * val_type->type_class.size);

	cond_val = (logical *) cond_md->multidval.val;
	if (true_val_md->multidval.kind == SCALAR && false_val_md->multidval.kind == SCALAR) {
		if (check_true) {
			if (! memcmp(true_val,&check_missing_val,val_type->type_class.size))
				true_val = (void *) & missing_val;
		}
		else if (check_false) {
			if (! memcmp(false_val,&check_missing_val,val_type->type_class.size))
				false_val = (void *) & missing_val;
		}
		for(j = 0; j < cond_md->multidval.totalelements; j++) {
			if (cond_has_missing && _NclIsMissing(cond_md,(void *)&cond_val[j])) {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       &missing_val,val_type->type_class.size);
				return_missing = 1;
				continue;
			}
			if (cond_val[j]) {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       (char*)true_val, val_type->type_class.size);
			}
			else {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       (char*)false_val, val_type->type_class.size);
			}
		}
	}
	else if (true_val_md->multidval.kind == SCALAR) {
		if (check_true) {
			if (! memcmp(true_val,&check_missing_val,val_type->type_class.size))
				true_val = (void *) & missing_val;
		}
		for(j = 0; j < cond_md->multidval.totalelements; j++) {
			if (cond_has_missing && _NclIsMissing(cond_md,(void *)&cond_val[j])) {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       &missing_val,val_type->type_class.size);
				return_missing = 1;
				continue;
			}
			if (cond_val[j]) {
				memcpy((char*)out_val + j * val_type->type_class.size,
				       (char*)true_val, val_type->type_class.size);
			}
			else if (check_false) {
				if (! memcmp((char*)false_val + j *  val_type->type_class.size,
					     &check_missing_val,val_type->type_class.size)) {
					memcpy((char*)out_val + j *  val_type->type_class.size,
					       (char*)& missing_val, val_type->type_class.size);
				}
				else {
					memcpy((char*)out_val + j *  val_type->type_class.size,
					       (char*)false_val + j *  val_type->type_class.size,
					       val_type->type_class.size);
				}
			}
			else {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       (char*)false_val + j *  val_type->type_class.size,
				       val_type->type_class.size);
			}
		}
	}
	else if (false_val_md->multidval.kind == SCALAR) {
		if (check_false) {
			if (! memcmp(false_val,&check_missing_val,val_type->type_class.size))
				false_val = (void *) & missing_val;
		}
		for(j = 0; j < cond_md->multidval.totalelements; j++) {
			if (cond_has_missing && _NclIsMissing(cond_md,(void *)&cond_val[j])) {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       &missing_val,val_type->type_class.size);
				return_missing = 1;
				continue;
			}
			if (cond_val[j]) {
				if (check_true) {
					if (! memcmp((char*)true_val + j *  val_type->type_class.size,
						     &check_missing_val,val_type->type_class.size)) {
						memcpy((char*)out_val + j *  val_type->type_class.size,
						       (char*)& missing_val, val_type->type_class.size);
					}
					else {
						memcpy((char*)out_val + j *  val_type->type_class.size,
						       (char*)true_val + j * val_type->type_class.size,
						       val_type->type_class.size);
					}
				}
				else {
					memcpy((char*)out_val + j *  val_type->type_class.size,
					       (char*)true_val + j * val_type->type_class.size,
					       val_type->type_class.size);
				}
			}
			else {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       (char*)false_val, val_type->type_class.size);
			}
		}
	}
	else {
		for(j = 0; j < cond_md->multidval.totalelements; j++) {
			if (cond_has_missing && _NclIsMissing(cond_md,(void *)&cond_val[j])) {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       &missing_val,val_type->type_class.size);
				return_missing = 1;
				continue;
			}
			if (cond_val[j]) {
				if (check_true) {
					if (! memcmp((char*)true_val + j *  val_type->type_class.size,
						     &check_missing_val,val_type->type_class.size)) {
						memcpy((char*)out_val + j *  val_type->type_class.size,
						       (char*)& missing_val, val_type->type_class.size);
					}
					else {
						memcpy((char*)out_val + j *  val_type->type_class.size,
						       (char*)true_val + j * val_type->type_class.size,
						       val_type->type_class.size);
					}
				}
				else {
					memcpy((char*)out_val + j *  val_type->type_class.size,
					       (char*)true_val + j * val_type->type_class.size,
					       val_type->type_class.size);
				}
			}
			else if (check_false) {
				if (! memcmp((char*)false_val + j *  val_type->type_class.size,
					     &check_missing_val,val_type->type_class.size)) {
					memcpy((char*)out_val + j *  val_type->type_class.size,
					       (char*)& missing_val, val_type->type_class.size);
				}			
				else {
					memcpy((char*)out_val + j *  val_type->type_class.size,
					       (char*)false_val + j *  val_type->type_class.size,
					       val_type->type_class.size);
				}
			}
			else {
				memcpy((char*)out_val + j *  val_type->type_class.size,
				       (char*)false_val + j *  val_type->type_class.size,
				       val_type->type_class.size);
			}
		}
	}
	if(tmp != NULL) 	
		NclFree(tmp);
	if (return_missing) {
		return(NclReturnValue(
			       out_val,
			       cond_md->multidval.n_dims,
			       cond_md->multidval.dim_sizes,
			       &missing_val,
			       val_md->multidval.data_type,
			       0
			       ));
	}
	else {
		return(NclReturnValue(
			       out_val,
			       cond_md->multidval.n_dims,
			       cond_md->multidval.dim_sizes,
			       NULL,
			       val_md->multidval.data_type,
			       0
			       ));
	}
}

NhlErrorTypes _Nclmin
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t dimsizes = 1;
	void *tmp;
	logical result;
	ng_size_t  i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
		  /* The values are all missing */
			return(NclReturnValue(
			        &tmp_md->multidval.missing_value.value,
				1,
				&dimsizes,
			        &tmp_md->multidval.missing_value.value,
				tmp_md->multidval.data_type,
				1
			));
		}
		tmp= &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
		for(; i < tmp_md->multidval.totalelements; i++) {
			_Nclgt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,&(tmp_md->multidval.missing_value.value),1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				result = 0;
			}
		}
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			tmp_md->multidval.data_type,
			0
		));
	} else {
		tmp= tmp_md->multidval.val;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			_Nclgt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,NULL,1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				result = 0;
			}
		}
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			tmp_md->multidval.data_type,
			0
		));
	}
}
NhlErrorTypes _Nclmax
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	void *out_val;
	ng_size_t dimsizes = 1;
	void *tmp;
	logical result;
	ng_size_t  i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
		  /* The values are all missing */
			return(NclReturnValue(
			        &tmp_md->multidval.missing_value.value,
				1,
				&dimsizes,
			        &tmp_md->multidval.missing_value.value,
				tmp_md->multidval.data_type,
				1
			));
		}
		tmp= &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
		for(; i < tmp_md->multidval.totalelements; i++) {
			_Ncllt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,&(tmp_md->multidval.missing_value.value),1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				result = 0;
			}
		}
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			tmp_md->multidval.data_type,
			0
		));
	} else {
		tmp= tmp_md->multidval.val;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			_Ncllt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,NULL,1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				result = 0;
			}
		}
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp,tmp_md->multidval.type->type_class.size);

		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			tmp_md->multidval.data_type,
			0
		));
	}
}

NhlErrorTypes _Nclminind
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	ng_size_t dimsizes = 1;
	void *tmp, *out_val;
	logical result;
	ng_size_t i, j;
	NclBasicDataTypes ret_type;
	int ret_size;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	ret_type = NCL_int;
	ret_size = ((NclTypeClass)nclTypeintClass)->type_class.size;
/*
 * Potential missing values.
 */
	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
/*
 * Array is all missing.
 */
#if !defined(NG32BIT)
		  if(i > INT_MAX) ret_type = NCL_long;
#endif
		  if(ret_type == NCL_int) {
		    return(NclReturnValue(
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				ret_type,
				1
			  ));
		  }
		  else {
		    return(NclReturnValue(
				&((NclTypeClass)nclTypelongClass)->type_class.default_mis,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypelongClass)->type_class.default_mis,
				NCL_long,
				1
			  ));
		  }
		}
/*
 * Array contains some non-missing values.
 */
		tmp= &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
		j = i;
		i++;
		for(; i < tmp_md->multidval.totalelements; i++) {
			_Nclgt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,&(tmp_md->multidval.missing_value.value),1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				j = i;
				result = 0;
			}
		}
#if !defined(NG32BIT)
		if(j > INT_MAX) {
		  ret_type = NCL_long;
		  ret_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
		}
#endif
		out_val = (void*)NclMalloc(ret_size);
		if(ret_type == NCL_int) {
		  *(int*)out_val = (int)j;
		}
		else {
		  *(long*)out_val = (long)j;
		}
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			ret_type,
			1
		));
	}
	else {
/*
 * No missing values possible in array.
 */
/*
 * No missing values possible in array.
 */
		tmp= tmp_md->multidval.val;
		j = 0;
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Nclgt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,NULL,1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				j = i;
				result = 0;
			}
		}
#if !defined(NG32BIT)
		if(j > INT_MAX) {
		  ret_type = NCL_long;
		  ret_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
		}
#endif
		out_val = (void*)NclMalloc(ret_size);
		if(ret_type == NCL_int) {
		  *(int*)out_val = (int)j;
		}
		else {
		  *(long*)out_val = (long)j;
		}
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			ret_type,
			1
		));
	}
}

NhlErrorTypes _Nclmaxind
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	ng_size_t dimsizes = 1;
	void *tmp, *out_val;
	logical result;
	ng_size_t i, j;
	NclBasicDataTypes ret_type;
	int ret_size;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	ret_type = NCL_int;
	ret_size = ((NclTypeClass)nclTypeintClass)->type_class.size;
/*
 * Potential missing values.
 */
	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
/*
 * Array is all missing.
 */
#if !defined(NG32BIT)
		  if(i > INT_MAX) ret_type = NCL_long;
#endif
		  if(ret_type == NCL_int) {
			return(NclReturnValue(
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				ret_type,
				1
			));
		  }
		  else {
		    return(NclReturnValue(
				&((NclTypeClass)nclTypelongClass)->type_class.default_mis,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypelongClass)->type_class.default_mis,
				NCL_long,
				1
			  ));
		  }
		}
/*
 * Array contains some non-missing values.
 */
		tmp= &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
		j = i;
		i++;
		for(; i < tmp_md->multidval.totalelements; i++) {
			_Ncllt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,&(tmp_md->multidval.missing_value.value),1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				j = i;
				result = 0;
			}
		}
#if !defined(NG32BIT)
		if(j > INT_MAX) {
		  ret_type = NCL_long;
		  ret_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
		}
#endif
		out_val = (void*)NclMalloc(ret_size);
		if(ret_type == NCL_int) {
		  *(int*)out_val = (int)j;
		}
		else {
		  *(long*)out_val = (long)j;
		}
		return(NclReturnValue(
		        out_val,
			1,
			&dimsizes,
			NULL,
			ret_type,
			1
		));
	} else {
/*
 * No missing values possible in array.
 */
		tmp= tmp_md->multidval.val;
		j = 0;
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Ncllt(tmp_md->multidval.type,&result,tmp,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]),NULL,NULL,1,1);
			if(result == 1) {
				tmp = &(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]);
				j = i;
				result = 0;
			}
		}
#if !defined(NG32BIT)
		if(j > INT_MAX) {
		  ret_type = NCL_long;
		  ret_size = ((NclTypeClass)nclTypelongClass)->type_class.size;
		}
#endif
		out_val = (void*)NclMalloc(ret_size);
		if(ret_type == NCL_int) {
		  *(int*)out_val = (int)j;
		}
		else {
		  *(long*)out_val = (long)j;
		}
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			ret_type,
			1
		));
	}
}

NhlErrorTypes _Ncldim_min
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	logical result = 0;
	ng_size_t i,j;
	ng_size_t m,n;
	int sz;
	ng_size_t nd;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz* n);
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&(((char*)tmp_md->multidval.val)[i*m*sz]),&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			j = 0;
			while((j<m) && tmp[j]) {
				j++;
			}
			if(j==m) {
				memcpy(&(((char*)out_val)[i*sz]),&(tmp_md->multidval.missing_value.value),sz);
			} else {
				memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
				j = j+1;
				for(; j < m; j++) {
					if(!tmp[j]) {
						_Nclgt(tmp_md->multidval.type,&result,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i* m)+j)*sz]),NULL,NULL,1,1);
						if(result == 1) {
							memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
							result = 0;

						}
					}
				}
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&tmp_md->multidval.missing_value.value,
			tmp_md->multidval.type->type_class.data_type,
			0);
	} else {
		for(i = 0; i < n ; i++) {
			memcpy(&(((char*)out_val)[i*sz]) ,&(((char*)tmp_md->multidval.val)[i*m*sz]),sz);
			for(j = 1; j < m; j++) {
				_Nclgt(tmp_md->multidval.type,&result,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i* m)+j)*sz]),NULL,NULL,1,1);

				if(result == 1) {
					memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
					result = 0;

				}
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			tmp_md->multidval.type->type_class.data_type,
			0);
	}
	if(tmp != NULL)
		NclFree(tmp);
	NclFree(dimsizes);
	return(ret);

}

NhlErrorTypes _Ncldim_min_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
        int *dims;
	ng_size_t ndims;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	logical result = 0;
	ng_size_t i,j,k;
	ng_size_t i_in_sz,i_out_sz;
	ng_size_t m,n,nr,nl;
	int sz,nd;

/*
 * Get dimension to do minimum across.
 */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);
/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	  NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_min_n: Invalid dimension sizes to take minimum of, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_min_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}
/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * "tmp" will be used to store locations where "m" chunks of the data
 * are equal to missing.
 */
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz*n);
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;
	      for(k = 0; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Ncleq(tmp_md->multidval.type,&tmp[k],
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),
		       &(tmp_md->multidval.missing_value.value),NULL,NULL,1,1);
	      }
/*
 * Loop through tmp to find the first non-missing value.
 */
	      k = 0;
	      while((k<m) && tmp[k]) {
		k++;
	      }
	      if(k==m) {
/*
 * All values were missing, so set the output to missing at this location.
 */
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(tmp_md->multidval.missing_value.value),sz);
	      } else {
/*
 * There's at least one non-missing value, so copy this value, and
 * start getting the minimum of the rest of the non-missing values.
 */
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		k = k+1;
		for(; k < m; k++) {
		  if(!tmp[k]) {
		    i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		    _Nclgt(tmp_md->multidval.type,&result,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     NULL,NULL,1,1);
		    if(result == 1) {
		      memcpy(&(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		      result = 0;
		    }
		  }
		}
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &tmp_md->multidval.missing_value.value,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so 
 * we don't need to look for missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;

	      /* k = 0 */
	      memcpy(&(((char*)out_val)[i_out_sz]),
		     &(((char*)tmp_md->multidval.val)[(i*(nr*m)+j)*sz]),sz);
	      for(k = 1; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Nclgt(tmp_md->multidval.type,&result,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     NULL,NULL,1,1);
		if(result == 1) {
		  memcpy(&(((char*)out_val)[i_out_sz]),
			 &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		  result = 0;
		}
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	}
	if(tmp != NULL)
	  NclFree(tmp);
	NclFree(dimsizes);
	return(ret);
}


NhlErrorTypes _Ncldim_max
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	ng_size_t i,j;
	ng_size_t m,n,sz;
	ng_size_t nd;
	logical result = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(ng_size_t));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(ng_size_t));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz* n);
	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&(((char*)tmp_md->multidval.val)[i*m*sz]),&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			j = 0;
			while((j<m) && tmp[j]) {
				j++;
			}
			if(j==m) {
				memcpy(&(((char*)out_val)[i*sz]),&(tmp_md->multidval.missing_value.value),sz);
			} else {
				memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
				j = j+1;
				for(; j < m; j++) {
					if(!tmp[j]) {
						_Ncllt(tmp_md->multidval.type,&result,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i* m)+j)*sz]),NULL,NULL,1,1);
						if(result == 1) {
							memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
							result = 0;

						}
					}
				}
			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&tmp_md->multidval.missing_value.value,
			tmp_md->multidval.type->type_class.data_type,
			0);
	} else {
		for(i = 0; i < n ; i++) {
			memcpy(&(((char*)out_val)[i*sz]) ,&(((char*)tmp_md->multidval.val)[i*m*sz]),sz);
			for(j = 1; j < m; j++) {
				_Ncllt(tmp_md->multidval.type,&result,&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i* m)+j)*sz]),NULL,NULL,1,1);
				if(result == 1) {
					memcpy(&(((char*)out_val)[i*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
					result = 0;

				}

			}
		}
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			tmp_md->multidval.type->type_class.data_type,
			0);
	}
	if(tmp != NULL)
		NclFree(tmp);
	NclFree(dimsizes);
	return(ret);

}

NhlErrorTypes _Ncldim_max_n
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NhlErrorTypes ret = NhlNOERROR;
	NclMultiDValData tmp_md = NULL;
	void *out_val = NULL;
        int *dims;
	ng_size_t ndims;
	ng_size_t *dimsizes = NULL;
	logical *tmp = NULL;
	logical result = 0;
	ng_size_t i,j,k;
	ng_size_t i_in_sz,i_out_sz;
	ng_size_t m,n,nr,nl;
	int sz,nd;

/*
 * Get dimension to do maximum across.
 */
	dims = (int *)NclGetArgValue(1,2,NULL,&ndims,NULL,NULL,NULL,0);

/*
 * Read data values off stack (or not)
 */
	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Some error checking. Make sure input dimensions are valid.
 */
	for(i = 0; i < ndims; i++ ) {
	  if(dims[i] < 0 || dims[i] >= tmp_md->multidval.n_dims) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_max_n: Invalid dimension sizes to take maximum across, can't continue");
	    return(NhlFATAL);
	  }
	  if(i > 0 && dims[i] != (dims[i-1]+1)) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_max_n: Input dimension sizes must be monotonically increasing, can't continue");
	    return(NhlFATAL);
	  }
	}
/*
 * Calculate size of leftmost dimensions (nl) up to the dims[0]-th
 *   dimensions.
 * Calculate size of rightmost dimensions (nr) from the
 *   ndims[ndims-1]-th dimension
 *
 * The dimension(s) to do the average across are "dims".
 */
	nl = nr = m = 1;
	if(tmp_md->multidval.n_dims > 1) {
	  nd       = tmp_md->multidval.n_dims-ndims;
	  dimsizes = NclMalloc(nd * sizeof(ng_size_t));
	  for(i = 0; i < dims[0] ; i++) {
	    nl = nl*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i] = tmp_md->multidval.dim_sizes[i];
	  }
	  for(i = 0; i < ndims ; i++) {
	    m = m*tmp_md->multidval.dim_sizes[dims[i]];
	  }
	  for(i = dims[ndims-1]+1; i < tmp_md->multidval.n_dims; i++) {
	    nr = nr*tmp_md->multidval.dim_sizes[i];
	    dimsizes[i-ndims] = tmp_md->multidval.dim_sizes[i];
	  }
	} else {
	  dimsizes = NclMalloc(sizeof(ng_size_t));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;

/*
 * "tmp" will be used to store locations where "m" chunks of the data
 * are equal to missing.
 */
	tmp = (logical*)NclMalloc(sizeof(logical)*m);
	sz = tmp_md->multidval.type->type_class.size;
	out_val = (void*)NclMalloc(sz*n);
	if(tmp_md->multidval.missing_value.has_missing) {
/*
 * The input variable contains a _FillValue attribute, so we have
 * to assume there might be missing values present.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;
	      for(k = 0; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Ncleq(tmp_md->multidval.type,&tmp[k],
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),
		       &(tmp_md->multidval.missing_value.value),NULL,NULL,1,1);
	      }
/*
 * Loop through tmp to find the first non-missing value.
 */
	      k = 0;
	      while((k<m) && tmp[k]) {
		k++;
	      }
	      if(k==m) {
/*
 * All values were missing, so set the output to missing at this location.
 */
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(tmp_md->multidval.missing_value.value),sz);
	      } else {
/*
 * There's at least one non-missing value, so copy this value, and
 * start getting the maximum of the rest of the non-missing values.
 */
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		memcpy(&(((char*)out_val)[i_out_sz]),
		       &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		k = k+1;
		for(; k < m; k++) {
		  if(!tmp[k]) {
		    i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		    _Ncllt(tmp_md->multidval.type,&result,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     NULL,NULL,1,1);
		    if(result == 1) {
		      memcpy(&(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		      result = 0;
		    }
		  }
		}
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       &tmp_md->multidval.missing_value.value,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	} else {
/*
 * The input variable doesn't contain a _FillValue attribute, so 
 * we don't need to look for missing values.
 */
	  for(i = 0; i < nl ; i++) {
	    for(j = 0; j < nr ; j++) {
	      i_out_sz = ((i*nr)+j)*sz;

	      /* k = 0 */
	      memcpy(&(((char*)out_val)[i_out_sz]),
		     &(((char*)tmp_md->multidval.val)[(i*(nr*m)+j)*sz]),sz);
	      for(k = 1; k < m; k++) {
		i_in_sz = (i*(nr*m)+(k*nr)+j)*sz;
		_Ncllt(tmp_md->multidval.type,&result,
			     &(((char*)out_val)[i_out_sz]),
			     &(((char*)tmp_md->multidval.val)[i_in_sz]),
			     NULL,NULL,1,1);
		if(result == 1) {
		  memcpy(&(((char*)out_val)[i_out_sz]),
			 &(((char*)tmp_md->multidval.val)[i_in_sz]),sz);
		  result = 0;
		}
	      }
	    }
	  }
	  ret = NclReturnValue(
			       out_val,
			       nd,
			       dimsizes,
			       NULL,
			       tmp_md->multidval.type->type_class.data_type,
			       0);
	}
	if(tmp != NULL)
	  NclFree(tmp);
	NclFree(dimsizes);
	return(ret);
}


NhlErrorTypes _NclIIsInteger
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_int) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsUint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_uint) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsShort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_short) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsUshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_ushort) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsLong
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_long) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsUlong
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_ulong) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsInt64
#if     NhlNeedProto
(void)
#else
()
#endif
{
        NclStackEntry data;
        NclMultiDValData tmp_md = NULL;
        logical *out_val;
        ng_size_t dimsizes = 1;

        data = _NclGetArg(0,1,DONT_CARE);
        switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
	}
        if(tmp_md == NULL)
                return(NhlFATAL);

        out_val = (logical*)NclMalloc(sizeof(logical));
        if(tmp_md->multidval.data_type == NCL_int64) {
                *out_val = 1;
        } else {
                *out_val = 0;
        }

        return(NclReturnValue(
                out_val,
                1,
                &dimsizes,
                NULL,
                ((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
                0
        ));
}

NhlErrorTypes _NclIIsUint64
#if     NhlNeedProto
(void)
#else
()
#endif
{
        NclStackEntry data;
        NclMultiDValData tmp_md = NULL;
        logical *out_val;
        ng_size_t dimsizes = 1;

        data = _NclGetArg(0,1,DONT_CARE);
        switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
	}
        if(tmp_md == NULL)
                return(NhlFATAL);

        out_val = (logical*)NclMalloc(sizeof(logical));
        if(tmp_md->multidval.data_type == NCL_uint64) {
                *out_val = 1;
        } else {
                *out_val = 0;
        }

        return(NclReturnValue(
                out_val,
                1,
                &dimsizes,
                NULL,
                ((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
                0
        ));
}

NhlErrorTypes _NclIIsList
#if     NhlNeedProto
(void)
#else
()
#endif
{
        NclStackEntry data;
        NclMultiDValData tmp_md = NULL;
        logical *out_val;
        ng_size_t dimsizes = 1;

        data = _NclGetArg(0,1,DONT_CARE);
        switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return(NhlFATAL);
	}
        if(tmp_md == NULL)
                return(NhlFATAL);

        out_val = (logical*)NclMalloc(sizeof(logical));
        if(tmp_md->multidval.data_type == NCL_list) {
                *out_val = 1;
        } else {
                *out_val = 0;
        }



        return(NclReturnValue(
                out_val,
                1,
                &dimsizes,
                NULL,
                ((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
                0
        ));
}

NhlErrorTypes _NclIIsUbyte
#if     NhlNeedProto
(void)
#else
()
#endif
{
        NclStackEntry data;
        NclMultiDValData tmp_md = NULL;
        logical *out_val;
        ng_size_t dimsizes = 1;

        data = _NclGetArg(0,1,DONT_CARE);
        switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
	}
        if(tmp_md == NULL)
                return(NhlFATAL);

        out_val = (logical*)NclMalloc(sizeof(logical));
        if(tmp_md->multidval.data_type == NCL_ubyte) {
                *out_val = 1;
        } else {
                *out_val = 0;
        }



        return(NclReturnValue(
                out_val,
                1,
                &dimsizes,
                NULL,
                ((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
                0
        ));
}


NhlErrorTypes _NclIIsByte
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_byte) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIIsFloat
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_float) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIIsDouble
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_double) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIIsString
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_string) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIIsChar
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_char) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsNumeric
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.type->type_class.type & NCL_NUMERIC_TYPE_MASK) {
		switch(tmp_md->multidval.type->type_class.type)
		{
			case Ncl_Typeubyte:
			case Ncl_Typeushort:
				*out_val = 0;
				break;
			default:
				*out_val = 1;
		}
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsSNumeric
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));

	if(tmp_md->multidval.type->type_class.type & NCL_SNUMERIC_TYPE_MASK) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsENumeric
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.type->type_class.type & NCL_ENUMERIC_TYPE_MASK) {
		switch(tmp_md->multidval.type->type_class.type)
		{
			case Ncl_Typebyte:
			case Ncl_Typeshort:
			case Ncl_Typeint:
				*out_val = 0;
				break;
			default:
				*out_val = 1;
		}
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIIsFile
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIIsGraphic
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIIsLogical
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	logical *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.data_type == NCL_logical) {
		*out_val = 1;
	} else {
		*out_val = 0;
	}



	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypelogicalClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclIFileVarTypeOf
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclFile thefile;
	obj *thefile_id;
	NclQuark *out_val;
	ng_size_t dimsizes = 1;
	NclObjTypes ot;
	NclQuark* var_string;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        2,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
        var_string = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

	out_val = (NclQuark*)NclMalloc(sizeof(NclQuark));

	ot = _NclFileVarRepValue(thefile,*var_string);
	switch(ot) {
	case Ncl_Typedouble :                
		*out_val = NrmStringToQuark("double");
		break;
	case Ncl_Typefloat : 
		*out_val = NrmStringToQuark("float");
		break;
	case Ncl_Typelong :
		*out_val = NrmStringToQuark("long");
		break;
	case Ncl_Typeulong :
		*out_val = NrmStringToQuark("ulong");
		break;
	case Ncl_Typeint :
		*out_val = NrmStringToQuark("integer");
		break;
	case Ncl_Typeuint :
		*out_val = NrmStringToQuark("uint");
		break;
	case Ncl_Typeshort :
		*out_val = NrmStringToQuark("short");
		break;
	case Ncl_Typeushort :
		*out_val = NrmStringToQuark("ushort");
		break;
	case Ncl_Typebyte :
		*out_val = NrmStringToQuark("byte");
		break;
	case Ncl_Typeubyte :
		*out_val = NrmStringToQuark("ubyte");
		break;
	case Ncl_Typeint64 :
		*out_val = NrmStringToQuark("int64");
		break;
	case Ncl_Typeuint64 :
		*out_val = NrmStringToQuark("uint64");
		break;
	case Ncl_Typestring :
		*out_val = NrmStringToQuark("string");
		break;
	case Ncl_Typechar: 
		*out_val = NrmStringToQuark("character");
		break;
	case Ncl_Typeobj: 
		*out_val = NrmStringToQuark("obj");
		break;
	case Ncl_Typelogical:
		*out_val = NrmStringToQuark("logical");
		break;
	case Ncl_Typelist:
		*out_val = NrmStringToQuark("list");
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"internal error"));
                return(NhlFATAL);
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypestringClass)->type_class.data_type,
		0
	));
}
NhlErrorTypes _NclITypeOf
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	NclQuark *out_val;
	ng_size_t dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (NclQuark*)NclMalloc(sizeof(NclQuark));
	if(tmp_md->multidval.data_type == NCL_obj) {
		if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData) {
			*out_val = NrmStringToQuark("file");
		} else if(tmp_md->obj.obj_type_mask & Ncl_MultiDValHLUObjData) {
			*out_val = NrmStringToQuark("graphic");
		} else {
			*out_val = NrmStringToQuark("unknown");
		}
	} else {
		*out_val = NrmStringToQuark(_NclBasicDataTypeToName(tmp_md->multidval.data_type));
	}

	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		((NclTypeClass)nclTypestringClass)->type_class.data_type,
		0
	));
}

NhlErrorTypes _NclIgaus
#if	NhlNeedProto
(void)
#else
()
#endif
{
        void *tmp_nlat;
	ng_size_t *nlat, dsizes_nlat[1];
	NclBasicDataTypes type_nlat;
	int has_missing;
	NclScalar missing;
	ng_size_t nl_tmp, lwork_tmp, dimsizes[2];
	int nl, lwork=0;
	double *theta;
	double *wts;
	ng_size_t i;
	double *work = NULL;
	int ierror;
	double rtod = (double)180.0/(double)3.14159265358979323846;
	double *output;
	NclScalar output_missing;
	logical ret_missing = False;
	extern void NGCALLF(gaqdncl,GAQDNCL)(int *,double *,double *,double *,int *,int *);

	tmp_nlat = (void*)NclGetArgValue( 0, 1, NULL, dsizes_nlat, &missing, &has_missing, &type_nlat,DONT_CARE);

/*
 * Check the input dimensions and compute the total size of the input array.
 */
	nlat = get_dimensions(tmp_nlat,dsizes_nlat[0],type_nlat,"gaus");
	if(nlat == NULL) 
	  return(NhlFATAL);

/*
 * Calculate some array sizes so we can test them.
 */
	nl_tmp    = 2 * (*nlat);                   /* Output array size */
	lwork_tmp = 4 * nl_tmp*(nl_tmp+1)+2;       /* Work array size */

	if(has_missing && ( ((type_nlat==NCL_int)&&(*nlat==missing.intval)) ||
			    ((type_nlat==NCL_long)&&(*nlat==missing.longval)))) {
	  ret_missing = True;
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"gaus: missing value in input cannot compute gaussian vals");
	}
	else if(*nlat <= 0) {
	  ret_missing = True;
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"gaus: number of latitudes must be positive");
	} 
	else if(*nlat > INT_MAX || nl_tmp > INT_MAX || lwork_tmp > INT_MAX ) {
/*
 * The Fortran gaus only accepts integers for now, so can't have an nlat > INT_MAX.
 */
	  ret_missing = True;
	  NhlPError(NhlWARNING,NhlEUNKNOWN,"gaus: number of input/output latitudes and/or size of work array can't be > INT_MAX");
	}
	nl    = (int) nl_tmp;
	lwork = (int) lwork_tmp;
	if(ret_missing) {
	  dimsizes[0] = 1;
	  output = (double*)NclMalloc(sizeof(double));
          if (output == NULL)
          {
            NHLPERROR((NhlFATAL, errno, "gaus: output: memory allocation error."));
            return NhlFATAL;
          }
	  output_missing.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
	  *(double*)output = output_missing.doubleval;
	  NclReturnValue(
			 output,
			 1,
			 dimsizes,
			 &output_missing,
			 NCL_double,
			 1);
	  return(NhlWARNING);
	}
	theta = (double*)NclMalloc(sizeof(double)*nl);
	wts = (double*)NclMalloc(sizeof(double)*nl);
	work = (double*)NclMalloc(sizeof(double)*lwork);
	NGCALLF(gaqdncl,GAQDNCL)(&nl,theta,wts,work,&lwork,&ierror);
	NclFree(work);
	output = (double*)NclMalloc(sizeof(double)*nl*2);
        if (output == NULL)
        {
            NHLPERROR((NhlFATAL, errno, "gaus: output: memory allocation error."));
            return NhlFATAL;
        }

	for(i = 0; i < nl; i++) {
		output[2*i] = rtod*theta[i] - 90.0;
		output[2*i+1] = wts[i];
	}
	NclFree(wts);
	NclFree(theta);
	NclFree(nlat);
	dimsizes[0] = nl;
	dimsizes[1] = 2;
	return(NclReturnValue(
		output,
		2,
		dimsizes,
		NULL,
		NCL_double,
		0));

}


NhlErrorTypes _NclIGetVarDims
#if	NhlNeedProto
(void)
#else
()
#endif
{
	int i;
	ng_size_t dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry val;
	NclVar tmp_var;
	NclFile thefile = NULL;
	NclMultiDValData file_md = NULL;
	NclQuark names[2048];
	ng_size_t ndims = 0;



        val = _NclGetArg(0,1,DONT_CARE);
        switch(val.kind) {
	case NclStk_VAR:
		tmp_var = val.u.data_var;
		break;
	case NclStk_VAL:
	default:
		dimsizes = 1;
		return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
	}
	if(tmp_var != NULL ) {
		if(tmp_var->obj.obj_type == Ncl_FileVar) {
			file_md= (NclMultiDValData)_NclVarValueRead(tmp_var,NULL,NULL);
			thefile = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);

			if(thefile->file.advanced_file_structure)
			{
				NclAdvancedFile theadvancedfile = (NclAdvancedFile) thefile;
				NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;

				if(NULL != grpnode->dim_rec)
				{
					ndims = grpnode->dim_rec->n_dims;

					for(i = 0; i < ndims; ++i)
						names[i] = grpnode->dim_rec->dim_node[i].name;
				}
				else
				{
					names[0] = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
					ndims = 1;
					if (grpnode->name == NrmStringToQuark("/")) {
						if (grpnode->grp_rec && grpnode->grp_rec->n_grps > 0) {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"getvardims: root group in file %s contains no dimensions readable by NCL",
								  NrmQuarkToString(theadvancedfile->advancedfile.fname));
						}
						else {
							NhlPError(NhlWARNING,NhlEUNKNOWN,"getvardims: file %s contains no dimensions readable by NCL",
								  NrmQuarkToString(theadvancedfile->advancedfile.fname));
						}
					}
					else {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"getvardims: group <%s> in file %s contains no dimensions readable by NCL",
							  NrmQuarkToString(grpnode->name),
							  NrmQuarkToString(theadvancedfile->advancedfile.fname));
					}
				}
			}
			else
			{
    				if(thefile->file.advanced_file_structure)
					data = _NclGetFileInfo1(thefile);
    				else
					data = _NclGetFileInfo2(thefile);
				for (i=0; i < data->u.file->n_dims;i++) {
					names[i] = data->u.file->dim_info[i].dim_quark;
				}

				ndims = data->u.file->n_dims;

				if (ndims == 0) {
					names[0] = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
					ndims = 1;
					NhlPError(NhlWARNING,NhlEUNKNOWN,"getvardims: file %s contains no dimensions readable by NCL",
				  		NrmQuarkToString(thefile->file.fname));
				}
			}
			ret = NclReturnValue((void*)names, 1, &ndims, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, 
					     ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
			ret = MIN(ret,NhlWARNING);
		} else {
			data = _NclGetVarInfo2(tmp_var);
			for (i=0; i < data->u.var->n_dims;i++) {
				names[i] = data->u.var->dim_info[i].dim_quark;
				if(data->u.var->dim_info[i].dim_quark < 0) {
					names[i] = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
					
				}
			}

			ndims = data->u.var->n_dims;
			ret = NclReturnValue((void*)names, 1, &ndims, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, 
					     ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		}
	} else {
		ret  = NhlFATAL;
	}
	if (data)
		_NclFreeApiDataList((void*)data);
	return(ret);


}

NhlErrorTypes _NclIGetVarAtts
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclQuark name;
	ng_size_t dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md = NULL;

    ng_size_t num_atts;

        val = _NclGetArg(0,1,DONT_CARE);
        switch(val.kind) {
	case NclStk_VAR:
		tmp_var = val.u.data_var;
		if(tmp_var->var.var_quark > 0) {
			name = tmp_var->var.var_quark;
		} else {
			name = -1;
		}
		break;
	case NclStk_VAL:
	default:
		dimsizes = 1;
		return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
	}

	if((tmp_var->obj.obj_type == Ncl_Var)||(tmp_var->obj.obj_type == Ncl_HLUVar)||(tmp_var->obj.obj_type == Ncl_CoordVar)){
		if((name == -1)||(tmp_var->obj.obj_type == Ncl_CoordVar)) {
			data = _NclGetVarInfo2(tmp_var);
		} else {
			data = _NclGetVarInfo(name);
		}
		if((data != NULL)&&(data->u.var->n_atts != 0)) {
            num_atts = data->u.var->n_atts;
			ret = NclReturnValue((void*)data->u.var->attnames, 1, &num_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
			_NclFreeApiDataList((void*)data);
			return(ret);
		}
	} else if(tmp_var->obj.obj_type == Ncl_FileVar) {
		if(name == -1) {
			tmp_md = _NclVarValueRead(tmp_var,NULL,NULL);
			if(tmp_md != NULL) {
                		thefile = (NclFile)_NclGetObj(*(int*)tmp_md->multidval.val);
			}
			if(thefile==NULL) {
				dimsizes = 1;
				return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
			}
			if(thefile->file.advanced_file_structure)
				data = _NclGetFileInfo1(thefile);
    			else
				data = _NclGetFileInfo2(thefile);
		} else {
			data = _NclGetFileInfo(name);
		}
		if((data != NULL)&&(data->u.file->n_atts != 0)) {
            num_atts = data->u.file->n_atts;
			ret = NclReturnValue((void*)data->u.file->attnames, 1, &num_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
			_NclFreeApiDataList((void*)data);
			return(ret);
		}
	} else {
		data = NULL;
	}
	dimsizes = 1;
	ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
	if(data != NULL) 
		_NclFreeApiDataList((void*)data);
	return(ret);

}
NhlErrorTypes _NclIFileVarDimsizes
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclQuark *name;
	NclQuark fname;
	NclScalar name_missing;
	int name_has_missing;
	ng_size_t dimsizes, product_size;
	ng_size_t ndims;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	void *dim_sizes;
	int i;
	NclMultiDValData tmp_md = NULL;
	NclFile thefile;
	NclBasicDataTypes return_type;


        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
	case NclStk_VAR:
		tmp_var = val.u.data_var;
		if(tmp_var->var.var_quark > 0) {
			fname = tmp_var->var.var_quark;
		} else {
			fname = -1;
		}
		break;
	case NclStk_VAL:
		fname = -1;
		tmp_md = val.u.data_obj;
		break;
	default:
		dimsizes = 1;
		return(NclReturnValue((void*)&((NclTypeClass)nclTypeintClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypeintClass)->type_class.default_mis, ((NclTypeClass)nclTypeintClass)->type_class.data_type, 1));
	}

        name = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &name_missing,
                        &name_has_missing,
                        NULL,
                        0);
	if(name_has_missing) {
		if(*name == name_missing.stringval) {
			dimsizes = 1;
		        return(NclReturnValue(
               			name,
                		1,
                		&dimsizes,
                		&name_missing,
                		((NclTypeClass)nclTypeintClass)->type_class.data_type,
                		1
        		));
		}
	}
	if(fname ==-1) {
		if(tmp_md == NULL) 
			tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
		if(tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData){
                	thefile = (NclFile)_NclGetObj(*(int*)tmp_md->multidval.val);
    			if(thefile->file.advanced_file_structure)
				data = _NclGetFileVarInfo1(thefile,*name);
    			else
				data = _NclGetFileVarInfo2(thefile,*name);
		} else {
			dimsizes = 1;
			return(NclReturnValue((void*)&((NclTypeClass)nclTypeintClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypeintClass)->type_class.default_mis, ((NclTypeClass)nclTypeintClass)->type_class.data_type, 1));
		}
	} else {
		data = _NclGetFileVarInfo(fname,*name);
	}
	if((data != NULL)&&(data->u.var->n_dims != 0)) {
/*
 * First loop through dimension sizes to see if we need to return
 * ints or longs.
 *
 * The rules for when to return an int versus a long:
 *    - On a 32-bit system, return ints.
 *    - On a 64-bit system, return longs if any of the
 *      individual dimension sizes are > INT_MAX, or
 *      if the product of the dimension sizes is > INT_MAX.
 */
		ndims = data->u.var->n_dims;
		return_type = NCL_int;
#if !defined(NG32BIT)
		i = 0;
		product_size = 1;
		while(i < ndims && (return_type == NCL_int)) {
			product_size *= data->u.var->dim_info[i].dim_size;
			if(data->u.var->dim_info[i].dim_size > INT_MAX || 
			   product_size > INT_MAX) {
			  return_type = NCL_long;
			}
			i++;
		}
#endif
		if(return_type == NCL_int) {
		  dim_sizes = (void *) NclMalloc(sizeof(int) * ndims);
		  for (i = 0; i < ndims; i++) {
		    ((int*)dim_sizes)[i] = (int)data->u.var->dim_info[i].dim_size;
		  }
		}
		else {
		  dim_sizes = (void *) NclMalloc(sizeof(long) * ndims);
		  for (i = 0; i < ndims; i++) {
		    ((long*)dim_sizes)[i] = (long)data->u.var->dim_info[i].dim_size;
		  }
		}
		ret = NclReturnValue(dim_sizes, 1, &ndims, NULL, return_type, 1);
		free(dim_sizes);
		_NclFreeApiDataList((void*)data);
		return(ret);
	} else {
		dimsizes = 1;
		ret = NclReturnValue((void*)&((NclTypeClass)nclTypeintClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypeintClass)->type_class.default_mis, ((NclTypeClass)nclTypeintClass)->type_class.data_type, 1);
		if (data)
			_NclFreeApiDataList((void*)data);
		return(ret);
	}
}

NhlErrorTypes _NclIGetFileVarDims
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclQuark *name;
	NclQuark fname;
	NclScalar name_missing;
	int name_has_missing;
	ng_size_t dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	NclMultiDValData tmp_md = NULL;
	NclFile thefile = NULL;
	NclQuark dim_names[NCL_MAX_DIMENSIONS];
	int i;



        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
	case NclStk_VAR:
		tmp_var = val.u.data_var;
		if(tmp_var->var.var_quark > 0) {
			fname = tmp_var->var.var_quark;
		} else {
			fname = -1;
		}
		break;
	case NclStk_VAL:
	default:
		dimsizes = 1;
		return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
	}

        name = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &name_missing,
                        &name_has_missing,
                        NULL,
                        0);
	if(name_has_missing) {
		if(*name == name_missing.stringval) {
			dimsizes = 1;
		        return(NclReturnValue(
               			name,
                		1,
                		&dimsizes,
                		&name_missing,
                		((NclTypeClass)nclTypestringClass)->type_class.data_type,
                		1
        		));
		}
	}
	if(fname == -1) {
		tmp_md = _NclVarValueRead(tmp_var,NULL,NULL);
		if(tmp_md != NULL) {
			thefile = (NclFile)_NclGetObj(*(obj*)tmp_md->multidval.val);
		}
		if(thefile != NULL ) {
    			if(thefile->file.advanced_file_structure)
				data = _NclGetFileVarInfo1(thefile,*name);
    			else
				data = _NclGetFileVarInfo2(thefile,*name);
		} else {
			dimsizes = 1;
			return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
		}
	} else {
		data = _NclGetFileVarInfo(fname,*name);
	}
	if((data != NULL)&&(data->u.var->n_dims != 0)) {
		for(i = 0; i < data->u.var->n_dims; i++) {
		 	dim_names[i] = data->u.var->dim_info[i].dim_quark;
		}

		ng_size_t ndims = data->u.var->n_dims;
		ret = NclReturnValue((void*)dim_names, 1, &ndims, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		_NclFreeApiDataList((void*)data);
		return(ret);
	} else {
		dimsizes = 1;
		ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		if (data)
			_NclFreeApiDataList((void*)data);
		return(ret);
	}
}
NhlErrorTypes _NclIGetFileVarAtts
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclQuark *name;
	NclQuark fname;
	NclScalar name_missing;
	int name_has_missing;
	ng_size_t dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md = NULL;

        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
	case NclStk_VAR:
		tmp_var = val.u.data_var;
		if(tmp_var->var.var_quark > 0) {
			fname = tmp_var->var.var_quark;
		} else {
			fname = -1;
		}
		break;
	case NclStk_VAL:
	default:
		dimsizes = 1;
		return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
	}

        name = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &name_missing,
                        &name_has_missing,
                        NULL,
                        0);
	if(name_has_missing) {
		if(*name == name_missing.stringval) {
			dimsizes = 1;
		        return(NclReturnValue(
               			name,
                		1,
                		&dimsizes,
                		&name_missing,
                		((NclTypeClass)nclTypestringClass)->type_class.data_type,
                		1
        		));
		}
	}
	if(fname == -1 ) {
		tmp_md = _NclVarValueRead(tmp_var,NULL,NULL);
		if(tmp_md != NULL) {
			thefile = (NclFile)_NclGetObj(*(obj*)tmp_md->multidval.val);
		}
		if(thefile != NULL ) {
    			if(thefile->file.advanced_file_structure)
				data = _NclGetFileVarInfo1(thefile,*name);
    			else
				data = _NclGetFileVarInfo2(thefile,*name);
		} else {
			dimsizes = 1;
			ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		}
	} else {
		data = _NclGetFileVarInfo(fname,*name);
	}
	if((data != NULL)&&(data->u.var->n_atts != 0)) {
        ng_size_t natts = data->u.var->n_atts;
		ret = NclReturnValue((void*)data->u.var->attnames, 1, &natts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		_NclFreeApiDataList((void*)data);
		return(ret);
	} else {
		dimsizes = 1;
		ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		if (data)
			_NclFreeApiDataList((void*)data);
		return(ret);
	}

}

NhlErrorTypes _NclIGetFileCompoundVarComponentNames(void)
{
	NclQuark *name;
	NclQuark fname;
	NclScalar name_missing;
	int name_has_missing;
	NhlErrorTypes ret;
	NclVar tmp_var;
	NclStackEntry val;
	NclQuark *component_names = NULL;
	ng_size_t num_components = 0;

        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind)
	{
		case NclStk_VAR:
			tmp_var = val.u.data_var;
			if(tmp_var->var.var_quark > 0)
				fname = tmp_var->var.var_quark;
			else
				fname = -1;
			break;
		default:
			num_components = -1;
	}

        name = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &name_missing,
                        &name_has_missing,
                        NULL,
                        0);

	if(name_has_missing)
	{
		if(*name == name_missing.stringval)
			num_components = -1;
	}

	if((-1 == fname) || (-1 == num_components))
		num_components = 0;
	else
		component_names = _NclGetFileCompoundVarComponentInfo(fname, *name, &num_components);

	if(num_components)
	{
		ret = NclReturnValue((void*)component_names, 1, &num_components, NULL,
					((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		NclFree(component_names);
	}
	else
	{
		num_components = 1;
		ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &num_components,
					&((NclTypeClass)nclTypestringClass)->type_class.default_mis,
					((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
	}

	return(ret);
}

NhlErrorTypes _NclIFileVlenDef(void)
{
    NclScalar missing;
    int has_missing;

    obj *thefile_id;
    NclQuark *vlen_name;
    NclQuark *var_name;
    NclQuark *type;
    NclQuark *dim_names;
    ng_size_t ndims;
    NclFile thefile;
    NhlErrorTypes ret=NhlNOERROR;

    thefile_id = (obj*)NclGetArgValue(
                        0,
                        5,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
    thefile = (NclFile)_NclGetObj((int)*thefile_id);
    if(thefile == NULL)
    {
        NHLPERROR((NhlFATAL, NhlEUNKNOWN,
            "_NclIFileVlenDef: CANNOT add vlen to empty file.\n"));
        return(NhlFATAL);
    }

    vlen_name = (NclQuark*)NclGetArgValue(
                        1,
                        5,
                        NULL,
                        &ndims,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*vlen_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileVlenDef: CANNOT add vlen named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*vlen_name)));
            return(NhlFATAL);
        }
    }

    var_name = (NclQuark*)NclGetArgValue(
                        2,
                        5,
                        NULL,
                        &ndims,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*var_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileVlenDef: CANNOT add var named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*var_name)));
            return(NhlFATAL);
        }
    }

    type = (NclQuark*)NclGetArgValue(
                        3,
                        5,
                        NULL,
                        &ndims,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*type == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileVlenDef: CANNOT add vlen type <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*type)));
            return(NhlFATAL);
        }
    }

    dim_names = (NclQuark*)NclGetArgValue(
                        4,
                        5,
                        NULL,
                        &ndims,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if(dim_names[0] == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileVlenDef: CANNOT add vlen dimension named <%s>, which is same as missing-value.\n",
                NrmQuarkToString(dim_names[0])));
            return(NhlFATAL);
        }
    }

    ret = _NclFileAddVlen(thefile, *vlen_name, *var_name, *type, dim_names, ndims);

    return(ret);
}

NhlErrorTypes _NclIFileEnumDef(void)
{
    ng_size_t n_enums;
    NclScalar missing;
    int has_missing;

    obj *thefile_id;
    NclQuark *enum_name;
    NclQuark *var_name;
    NclQuark *dim_name;
    int n;
    NclFile thefile;
    NhlErrorTypes ret=NhlNOERROR;

    ng_size_t n_mems;
    NclScalar mem_missing;
    int mem_has_missing;
    NclQuark *mem_name;

    ng_size_t n_vals;
    NclScalar val_missing;
    int val_has_missing;
    void *mem_value;
    NclBasicDataTypes val_type;

    thefile_id = (obj*)NclGetArgValue(
                        0,
                        6,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
    thefile = (NclFile)_NclGetObj((int)*thefile_id);
    if(thefile == NULL)
    {
        NHLPERROR((NhlFATAL, NhlEUNKNOWN,
            "_NclIFileEnumDef: CANNOT add enum to empty file.\n"));
        return(NhlFATAL);
    }

    enum_name = (NclQuark*)NclGetArgValue(
                        1,
                        6,
                        NULL,
                        &n_enums,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*enum_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileEnumDef: CANNOT add enum named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*enum_name)));
            return(NhlFATAL);
        }
    }

    var_name = (NclQuark*)NclGetArgValue(
                        2,
                        6,
                        NULL,
                        &n_enums,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*var_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileEnumDef: CANNOT add var named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*var_name)));
            return(NhlFATAL);
        }
    }

    dim_name = (NclQuark*)NclGetArgValue(
                        3,
                        6,
                        NULL,
                        &n_enums,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*dim_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileEnumDef: CANNOT add enum dimension named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*dim_name)));
            return(NhlFATAL);
        }
    }

    mem_name = (NclQuark*)NclGetArgValue(
                        4,
                        6,
                        NULL,
                        &n_mems,
                        &mem_missing,
                        &mem_has_missing,
                        NULL,
                        0);

    if(mem_has_missing)
    {
        int num_missing = 0;

        for(n = 0; n < n_mems; n++)
        {
            if((NclQuark)mem_name[n] == missing.stringval)
                num_missing++;
        }

        if(num_missing == n_mems)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileEnumDef: Can not have all members as missing.\n"));
            return(NhlFATAL);
        }
    }

    mem_value = (void *)NclGetArgValue(
                        5,
                        6,
                        NULL,
                        &n_vals,
                        &val_missing,
                        &val_has_missing,
                        &val_type,
                        0);

    ret = _NclFileAddEnum(thefile, *enum_name, *var_name, *dim_name,
                          mem_name, mem_value, n_mems, val_type);

    return(ret);
}

NhlErrorTypes _NclIFileCompoundDef(void)
{
    ng_size_t n_compounds;
    ng_size_t n_dims;
    NclScalar missing;
    int has_missing;

    obj *thefile_id;
    NclQuark *compound_name;
    NclQuark *var_name;
    NclQuark *dim_name;
    int n;
    NclFile thefile;
    NhlErrorTypes ret=NhlNOERROR;

    ng_size_t n_mems;
    NclScalar mem_missing;
    int mem_has_missing;
    NclQuark *mem_name;

    ng_size_t n_types;
    NclScalar type_missing;
    int type_has_missing;
    NclQuark *mem_type;

    ng_size_t n_sizes;
    NclScalar size_missing;
    int size_has_missing;
    int *mem_size;

    int num_missing = 0;

    thefile_id = (obj*)NclGetArgValue(
                        0,
                        7,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
    thefile = (NclFile)_NclGetObj((int)*thefile_id);
    if(thefile == NULL)
    {
        NHLPERROR((NhlFATAL, NhlEUNKNOWN,
            "_NclIFileCompoundDef: CANNOT add compound to empty file.\n"));
        return(NhlFATAL);
    }

    compound_name = (NclQuark*)NclGetArgValue(
                        1,
                        7,
                        NULL,
                        &n_compounds,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*compound_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileCompoundDef: CANNOT add compound named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*compound_name)));
            return(NhlFATAL);
        }
    }

    var_name = (NclQuark*)NclGetArgValue(
                        2,
                        7,
                        NULL,
                        &n_compounds,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*var_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileCompoundDef: CANNOT add var named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*var_name)));
            return(NhlFATAL);
        }
    }

    dim_name = (NclQuark*)NclGetArgValue(
                        3,
                        7,
                        NULL,
                        &n_dims,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        num_missing = 0;

        for(n = 0; n < n_dims; n++)
        {
            if((NclQuark)dim_name[n] == missing.stringval)
                num_missing++;
        }

        if(num_missing == n_dims)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileCompoundDef: CANNOT add compound dimension named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*dim_name)));
            return(NhlFATAL);
        }
    }

    mem_name = (NclQuark*)NclGetArgValue(
                        4,
                        7,
                        NULL,
                        &n_mems,
                        &mem_missing,
                        &mem_has_missing,
                        NULL,
                        0);

    if(mem_has_missing)
    {
        num_missing = 0;

        for(n = 0; n < n_mems; n++)
        {
            if((NclQuark)mem_name[n] == missing.stringval)
                num_missing++;
        }

        if(num_missing == n_mems)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileCompoundDef: Can not have all members as missing.\n"));
            return(NhlFATAL);
        }
    }

    mem_type = (NclQuark *)NclGetArgValue(
                        5,
                        7,
                        NULL,
                        &n_types,
                        &type_missing,
                        &type_has_missing,
                        NULL,
                        0);

    if(type_has_missing)
    {
        num_missing = 0;

        for(n = 0; n < n_types; n++)
        {
            if((NclQuark)mem_type[n] == missing.stringval)
                num_missing++;
        }

        if(num_missing == n_types)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileCompoundDef: Can not have all members as missing.\n"));
            return(NhlFATAL);
        }
    }

    mem_size = (int *)NclGetArgValue(
                        6,
                        7,
                        NULL,
                        &n_sizes,
                        &size_missing,
                        &size_has_missing,
                        NULL,
                        0);

    if(size_has_missing)
    {
        num_missing = 0;

        for(n = 0; n < n_sizes; n++)
        {
            if((NclQuark)mem_size[n] == missing.intval)
                num_missing++;
        }

        if(num_missing == n_sizes)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileCompoundDef: Can not have all members as missing.\n"));
            return(NhlFATAL);
        }
    }

    ret = _NclFileAddCompound(thefile, *compound_name, *var_name,
                              n_dims, dim_name,
                              n_mems, mem_name, mem_type, mem_size);

    return(ret);
}

NhlErrorTypes _NclIFileWriteCompound(void)
{
    ng_size_t n_compounds;
    NclScalar missing;
    int has_missing;

    obj *thefile_id;
    NclQuark *compound_name;
    NclQuark *var_name;
    int n;
    NclFile thefile;
    NhlErrorTypes ret=NhlNOERROR;

    ng_size_t n_mems;
    NclScalar mem_missing;
    int mem_has_missing;
    NclQuark *mem_name;

    int num_missing = 0;

    obj *list_id;
    NclObj listobj;

    thefile_id = (obj*)NclGetArgValue(
                        0,
                        5,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
    thefile = (NclFile)_NclGetObj((int)*thefile_id);
    if(thefile == NULL)
    {
        NHLPERROR((NhlFATAL, NhlEUNKNOWN,
            "_NclIFileWriteCompound: CANNOT add compound to empty file.\n"));
        return(NhlFATAL);
    }

    compound_name = (NclQuark*)NclGetArgValue(
                        1,
                        5,
                        NULL,
                        &n_compounds,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*compound_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileWriteCompound: CANNOT add compound named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*compound_name)));
            return(NhlFATAL);
        }
    }

    var_name = (NclQuark*)NclGetArgValue(
                        2,
                        5,
                        NULL,
                        &n_compounds,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*var_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileWriteCompound: CANNOT add var named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*var_name)));
            return(NhlFATAL);
        }
    }

    mem_name = (NclQuark*)NclGetArgValue(
                        3,
                        5,
                        NULL,
                        &n_mems,
                        &mem_missing,
                        &mem_has_missing,
                        NULL,
                        0);

    if(mem_has_missing)
    {
        num_missing = 0;

        for(n = 0; n < n_mems; n++)
        {
            if((NclQuark)mem_name[n] == missing.stringval)
                num_missing++;
        }

        if(num_missing == n_mems)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileWriteCompound: Can not have all members as missing.\n"));
            return(NhlFATAL);
        }
    }

    list_id = (obj*)NclGetArgValue(
           4,
           5,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);

    listobj = (NclObj)_NclGetObj(*list_id);

    if(listobj == NULL)
    {
        NHLPERROR((NhlWARNING,NhlEUNKNOWN,
                  "_NclIFileWriteCompound: NULL list."));
        return(NhlWARNING);
    }

    ret = _NclFileWriteCompound(thefile, *compound_name, *var_name,
                                n_mems, mem_name, listobj);

    return(ret);
}

NhlErrorTypes _NclIFileGrpDef(void)
{
	ng_size_t n_grps;
	NclScalar missing;
	int has_missing;

	obj *thefile_id;
	NclQuark *grpnames;
	int i;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        2,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL)
	{
		NHLPERROR((NhlFATAL, NhlEUNKNOWN,
			"_NclIFileGrpDef: CANNOT add group to empty file.\n"));
		return(NhlFATAL);
	}

        grpnames = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        &n_grps,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

	if(has_missing)
	{
		for(i = 0; i < n_grps; i++)
		{
			if(grpnames[i] == missing.stringval)
			{
				NHLPERROR((NhlFATAL, NhlEUNKNOWN,
					"_NclIFileGrpDef: CANNOT add group <%s>, which is same as missing-value.\n",
					NrmQuarkToString(grpnames[i])));
				return(NhlFATAL);
			}
		}
	}

	for(i = 0; i < n_grps; i ++)
	{
		ret = _NclFileAddGrp(thefile,grpnames[i]);
	}

	return(ret);
}

NhlErrorTypes _NclIFileOpaqueDef(void)
{
    ng_size_t n_opaques;
    NclScalar missing;
    int has_missing;

    obj *thefile_id;
    NclQuark *opaque_name;
    NclQuark *var_name;
    NclQuark *dim_name;
    int    *var_size;
    NclFile thefile;
    NhlErrorTypes ret=NhlNOERROR;

    thefile_id = (obj*)NclGetArgValue(
                        0,
                        5,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
    thefile = (NclFile)_NclGetObj((int)*thefile_id);
    if(thefile == NULL)
    {
        NHLPERROR((NhlFATAL, NhlEUNKNOWN,
            "_NclIFileOpaqueDef: CANNOT add opaque to empty file.\n"));
        return(NhlFATAL);
    }

    opaque_name = (NclQuark*)NclGetArgValue(
                        1,
                        5,
                        NULL,
                        &n_opaques,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*opaque_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileOpaqueDef: CANNOT add opaque named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*opaque_name)));
            return(NhlFATAL);
        }
    }

    var_name = (NclQuark*)NclGetArgValue(
                        2,
                        5,
                        NULL,
                        &n_opaques,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*var_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileOpaqueDef: CANNOT add var named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*var_name)));
            return(NhlFATAL);
        }
    }

    var_size = (int *)NclGetArgValue(
                        3,
                        5,
                        NULL,
                        &n_opaques,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((int)*var_size == missing.intval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileOpaqueDef: CANNOT add opaque var_size <%d>, which is same as missing-value.\n",
                *var_size));
            return(NhlFATAL);
        }
    }

    dim_name = (NclQuark*)NclGetArgValue(
                        4,
                        5,
                        NULL,
                        &n_opaques,
                        &missing,
                        &has_missing,
                        NULL,
                        0);

    if(has_missing)
    {
        if((NclQuark)*dim_name == missing.stringval)
        {
            NHLPERROR((NhlFATAL, NhlEUNKNOWN,
                "_NclIFileOpaqueDef: CANNOT add opaque dimension named <%s>, which is same as missing-value.\n",
                NrmQuarkToString((NclQuark)*dim_name)));
            return(NhlFATAL);
        }
    }

    ret = _NclFileAddOpaque(thefile, *opaque_name, *var_name, *var_size, *dim_name);

    return(ret);
}

NhlErrorTypes _NclIFileVarDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	ng_size_t dimsize;
	NclScalar missing;
	int has_missing;

	ng_size_t tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	NclQuark *dimnames;
	NclQuark *types;
	NclQuark *varnames;
	int i;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        4,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        varnames = (NclQuark*)NclGetArgValue(
                        1,
                        4,
                        NULL,
                        &dimsize,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	if(has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(varnames[i] == missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

        types = (NclQuark*)NclGetArgValue(
                        2,
                        4,
                        NULL,
                        &tmp_dimsize,
                        &tmp_missing,
                        &tmp_has_missing,
                        NULL,
                        0);

	if(tmp_dimsize != dimsize) {
		return(NhlFATAL);
	} else if(tmp_has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(types[i] == tmp_missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

        dimnames = (NclQuark*)NclGetArgValue(
                        3,
                        4,
                        NULL,
                        &tmp_dimsize,
                        &tmp_missing,
                        &tmp_has_missing,
                        NULL,
                        0);

	if(tmp_has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(dimnames[i] == tmp_missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}
	for(i = 0; i < dimsize; i ++) {
		ret = _NclFileAddVar(thefile,varnames[i],types[i],tmp_dimsize,dimnames);
		if(ret < NhlINFO) {
			ret0 = ret;
		}
	}
	return(ret0);
}

NhlErrorTypes _NclIFileVarChunkDef
#if NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing;
	int has_missing;

	int n_dims;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	NclQuark *varnames;
	ng_size_t   *dimsizes;
	ng_size_t   input_dimsizes[NCL_MAX_DIMENSIONS];
	ng_size_t i;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;

	void *dims_void = NULL;
	NclBasicDataTypes dims_type;

        thefile_id = (obj *)NclGetArgValue(
                        0,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        varnames = (NclQuark *)NclGetArgValue(
                        1,
                        3,
                        &n_dims,
                        input_dimsizes,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	if(has_missing) {
		for(i = 0; i < n_dims; i++) {
			if(varnames[i] == missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

        dims_void = (void *)NclGetArgValue(
                        2,
                        3,
                        &n_dims,
                        input_dimsizes,
                        &tmp_missing,
                        &tmp_has_missing,
                        &dims_type,
                        0);

	n_dims = input_dimsizes[0];
        dimsizes = get_dimensions(dims_void, (ng_size_t)n_dims, dims_type, "Chunkdef");

	if(dimsizes == NULL)
	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,"FileVarChunkDef: dimension sizes wrong."));
		return(NhlFATAL);
        }

	if(tmp_has_missing) {
		for(i = 0; i < n_dims; i++) {
			if(dimsizes[i] == tmp_missing.intval)  {
				return(NhlFATAL);
			}
		}
	}

	ret = _NclFileAddVarChunk(thefile,varnames[0],n_dims,dimsizes);
	if(ret < NhlINFO) {
		ret0 = ret;
	}
	return(ret0);
}

NhlErrorTypes _NclIFileVarChunkCacheDef
#if NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing;
	int has_missing;
	int n_dims;
	NclScalar tmp_missing;
	int tmp_has_missing;
	NclQuark *varnames;
	ng_size_t *sizes;
	ng_size_t *elems;
	float  *pres;
	ng_size_t     input_dimsizes[NCL_MAX_DIMENSIONS];
	int i;
	obj *thefile_id;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;
	ng_size_t cache_size	= 3200000;
	ng_size_t cache_nelems	= 1009;
	float  cache_preemption = 0.5;

	void *elems_void = NULL;
	NclBasicDataTypes elems_type;

	void *sizes_void = NULL;
	NclBasicDataTypes sizes_type;

        thefile_id = (obj *)NclGetArgValue(
                        0,
                        5,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        varnames = (NclQuark *)NclGetArgValue(
                        1,
                        5,
                        &n_dims,
                        input_dimsizes,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	/* dimsize is not initialized */
	if(has_missing) {
		for(i = 0; i < n_dims; i++) {
			if(varnames[i] == missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

        sizes_void = (void *)NclGetArgValue(
                        2,
                        5,
                        &n_dims,
                        input_dimsizes,
                        &tmp_missing,
                        &tmp_has_missing,
                        &sizes_type,
                        0);

        sizes = get_dimensions(sizes_void, (ng_size_t)n_dims, sizes_type, "ChunkCachedef");

	if(sizes == NULL)
	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,"FileVarChunkCacheDef: Cache sizes wrong."));
		return(NhlFATAL);
        }

	cache_size = sizes[0];

        elems_void = (void *)NclGetArgValue(
                        3,
                        5,
                        &n_dims,
                        input_dimsizes,
                        &tmp_missing,
                        &tmp_has_missing,
                        &elems_type,
                        0);

        elems = get_dimensions(elems_void, (ng_size_t)n_dims, elems_type, "ChunkCachedef");

	if(elems == NULL)
	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,"FileVarChunkCacheDef: Cache elems wrong."));
		return(NhlFATAL);
        }

	cache_nelems = elems[0];

        pres = (float *)NclGetArgValue(
                        4,
                        5,
                        &n_dims,
                        input_dimsizes,
                        &tmp_missing,
                        &tmp_has_missing,
                        NULL,
                        0);

	cache_preemption = pres[0];

	ret = _NclFileAddVarChunkCache(thefile,varnames[0],cache_size,cache_nelems,cache_preemption);
	if(ret < NhlINFO) {
		ret0 = ret;
	}
	return(ret0);
}

NhlErrorTypes _NclIFileVarCompressLevelDef
#if NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing;
	int has_missing;
	int n_dims;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	NclQuark *varnames;
	int    *compress_level;
	ng_size_t    input_dimsizes[NCL_MAX_DIMENSIONS];
	int i;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;

        thefile_id = (obj *)NclGetArgValue(
                        0,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        varnames = (NclQuark *)NclGetArgValue(
                        1,
                        3,
                        &n_dims,
                        input_dimsizes,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	if(has_missing) {
		for(i = 0; i < n_dims; i++) {
			if(varnames[i] == missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

        compress_level = (int *)NclGetArgValue(
                        2,
                        3,
                        &n_dims,
                        input_dimsizes,
                        &tmp_missing,
                        &tmp_has_missing,
                        NULL,
                        0);

	n_dims = input_dimsizes[0];
	if(tmp_has_missing) {
		for(i = 0; i < n_dims; i++) {
			if(compress_level[i] == tmp_missing.intval)  {
				return(NhlFATAL);
			}
		}
	}

	ret = _NclFileSetVarCompressLevel(thefile,varnames[0],compress_level[0]);
	if(ret < NhlINFO) {
		ret0 = ret;
	}
	return(ret0);
}

NhlErrorTypes _NclIFileDimDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	ng_size_t dimsize;
	NclScalar missing;
	int has_missing;

	ng_size_t tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	NclQuark *dimnames;
	ng_size_t *dimsizes = NULL;
	logical *unlimited;
	int i;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;
	NclStackEntry data;
	NclMultiDValData    tmp_md = NULL;
	ng_size_t missing_val;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        4,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        dimnames = (NclQuark*)NclGetArgValue(
                        1,
                        4,
                        NULL,
                        &dimsize,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	if(has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(dimnames[i] == missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

	data = _NclGetArg(2, 4, DONT_CARE);
	switch (data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var, NULL, NULL);
		break;
		
	case NclStk_VAL:
		tmp_md = (NclMultiDValData) data.u.data_obj;
		break;
	default:
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return(NhlFATAL);
	}
	if (tmp_md == NULL) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return NhlFATAL;
	}

	dimsizes = (ng_size_t *) NclMalloc(tmp_md->multidval.dim_sizes[0] * sizeof(ng_size_t));
	if (! dimsizes) {
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
		return (NhlFATAL);
	}

	switch (tmp_md->multidval.data_type) {
	case NCL_byte:
		missing_val = (ng_size_t) tmp_md->multidval.missing_value.value.byteval;
		for (i = 0; i< tmp_md->multidval.dim_sizes[0]; i++)
			dimsizes[i]  = (ng_size_t)((byte *) tmp_md->multidval.val)[i];
		break;
	case NCL_short:
		missing_val = (ng_size_t) tmp_md->multidval.missing_value.value.shortval;
		for (i = 0; i< tmp_md->multidval.dim_sizes[0]; i++)
			dimsizes[i]  = (ng_size_t)((short *) tmp_md->multidval.val)[i];
		break;
	case NCL_int:
		missing_val = (ng_size_t) tmp_md->multidval.missing_value.value.intval;
		for (i = 0; i< tmp_md->multidval.dim_sizes[0]; i++)
			dimsizes[i]  = (ng_size_t)((int *) tmp_md->multidval.val)[i];
		break;
	case NCL_long:
		missing_val = (ng_size_t) tmp_md->multidval.missing_value.value.longval;
		for (i = 0; i< tmp_md->multidval.dim_sizes[0]; i++)
			dimsizes[i]  = (ng_size_t)((long *) tmp_md->multidval.val)[i];
		break;
	case NCL_int64:
		missing_val = (ng_size_t) tmp_md->multidval.missing_value.value.int64val;
		for (i = 0; i< tmp_md->multidval.dim_sizes[0]; i++)
			dimsizes[i]  = (ng_size_t)((long long *) tmp_md->multidval.val)[i];
		break;
	default:
		NhlPError(NhlFATAL, NhlEUNKNOWN,
			  "filedimdef: invalid type passed for number of elements parameter, can't continue");
		return NhlFATAL;
	}
	if (tmp_md->multidval.missing_value.has_missing) {
		for (i = 0; i< tmp_md->multidval.dim_sizes[0]; i++) {
			if (dimsizes[i] == missing_val) {
				NhlPError(NhlFATAL, NhlEUNKNOWN,
					  "filedimdef: dimension size array cannot contain missing values");
				return NhlFATAL;
			}
		}
	}

        unlimited = (logical*)NclGetArgValue(
                        3,
                        4,
                        NULL,
                        &tmp_dimsize,
                        &tmp_missing,
                        &tmp_has_missing,
                        NULL,
                        0);

	if(tmp_dimsize != dimsize) {
		return(NhlFATAL);
	} else if(tmp_has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(unlimited[i] == tmp_missing.logicalval)  {
				return(NhlFATAL);
			}
		}
	}
	for(i = 0; i < dimsize; i ++) {
		ret = _NclFileAddDim(thefile,dimnames[i],dimsizes[i],unlimited[i]);
		if(ret < NhlINFO) {
			ret0 = ret;
		}
	}
	NclFree(dimsizes);
	return(ret0);
}

NhlErrorTypes _NclIFileChunkDimDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	ng_size_t dimsize;
	NclScalar missing;
	int has_missing;

	ng_size_t tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	NclQuark *dimnames;
	void *tmp_dimsizes;
	ng_size_t *dimsizes;
        NclBasicDataTypes type_dimsizes;
	logical *unlimited;
	ng_size_t i;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        4,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        dimnames = (NclQuark*)NclGetArgValue(
                        1,
                        4,
                        NULL,
                        &dimsize,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	if(has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(dimnames[i] == missing.stringval)  {
				return(NhlFATAL);
			}
		}
	}

        tmp_dimsizes = (void *)NclGetArgValue(
                        2,
                        4,
                        NULL,
                        &tmp_dimsize,
                        &tmp_missing,
                        &tmp_has_missing,
                        &type_dimsizes,
                        0);

	dimsizes = get_dimensions(tmp_dimsizes,tmp_dimsize,type_dimsizes,
				  "FileChunkDimDef");

	if(tmp_dimsize != dimsize) {
		return(NhlFATAL);
	} else if(tmp_has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(dimsizes[i] == tmp_missing.intval)  {
				return(NhlFATAL);
			}
		}
	}

        unlimited = (logical*)NclGetArgValue(
                        3,
                        4,
                        NULL,
                        &tmp_dimsize,
                        &tmp_missing,
                        &tmp_has_missing,
                        NULL,
                        0);

	if(tmp_dimsize != dimsize) {
		return(NhlFATAL);
	} else if(tmp_has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(unlimited[i] == tmp_missing.logicalval)  {
				return(NhlFATAL);
			}
		}
	}
	for(i = 0; i < dimsize; i ++) {
		ret = _NclFileAddChunkDim(thefile,dimnames[i],dimsizes[i],unlimited[i]);
		if(ret < NhlINFO) {
			ret0 = ret;
		}
	}
	NclFree(dimsizes);
	return(ret0);
}

NhlErrorTypes _NclIFileAttDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	obj *thefile_id;
	int j;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;
	NclAtt tmp_attobj;
	NclAttList *the_att_list;
	NclStackEntry data;
	NclApiDataList *tmp;
	NclFile tmp_file;
	NclMultiDValData file_md;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        2,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}
        data = _NclGetArg(1,2,DONT_CARE);
	switch(data.kind) {
        case NclStk_VAR:
		switch(data.u.data_var->obj.obj_type) {
		case Ncl_FileVar:
			file_md= (NclMultiDValData)_NclVarValueRead(data.u.data_var,NULL,NULL);
			tmp_file = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
    			if(tmp_file->file.advanced_file_structure)
				tmp = _NclGetFileInfo1(tmp_file);
    			else
				tmp = _NclGetFileInfo2(tmp_file);
			if((tmp!= NULL)&&( tmp->u.file->n_atts > 0 )) {
				for(j = 0; j < tmp->u.file->n_atts; j++) {
					ret=_NclFileWriteAtt(thefile,tmp->u.file->attnames[j],_NclFileReadAtt(tmp_file,tmp->u.file->attnames[j],NULL),NULL);
					if(ret < NhlINFO) {
						ret0 = ret;
					}
				}
			} else {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAttDef: File variable (%s) has no attributes to assign",NrmQuarkToString(data.u.data_var->var.var_quark));
                                return(NhlWARNING);
                        }
			if (tmp)
				_NclFreeApiDataList((void*)tmp);

			break;
		default:
			if(data.u.data_var->var.att_id != -1) {
				tmp_attobj = (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAttDef: Variable (%s) has no attributes to assign", NrmQuarkToString(data.u.data_var->var.var_quark));
				return(NhlWARNING);
			}
			the_att_list = tmp_attobj->att.att_list;
			while(the_att_list != NULL) {
				ret = _NclFileWriteAtt(thefile,NrmStringToQuark(the_att_list->attname),the_att_list->attvalue,NULL);
				if(ret < NhlINFO) {
					ret0 = ret;
				}
				the_att_list = the_att_list->next;
			}
		}


                break;
        case NclStk_VAL:
	default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,"FileVarAttDef: A variable with attributes is expected not a value, No attributes to assign");
                return(NhlFATAL);
        }
	return(ret0);
}


NhlErrorTypes _NclIFileVarAttDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	ng_size_t dimsize;
	NclScalar missing;
	int has_missing;

	obj *thefile_id;
	NclQuark *varnames;
	int i,j;
	NclFile thefile;
	NhlErrorTypes ret=NhlNOERROR;
	NhlErrorTypes ret0 = NhlNOERROR;
	NclAtt tmp_attobj;
	NclAttList *the_att_list;
	NclStackEntry data;
	NclMultiDValData file_md;
	NclFile tmp_file;
	NclApiDataList * tmp;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
	if(thefile == NULL) {
		return(NhlFATAL);
	}

        varnames = (NclQuark*)NclGetArgValue(
                        1,
                        3,
                        NULL,
                        &dimsize,
                        &missing,
                        &has_missing,
                        NULL,
                        0);
	if(has_missing) {
		for(i = 0; i < dimsize; i++) {
			if(varnames[i] == missing.stringval)  {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"filevarattdef: missing value variable name detected, can't continue");
				return(NhlFATAL);
			}
		}
	}


        data = _NclGetArg(2,3,DONT_CARE);
        switch(data.kind) {
        case NclStk_VAR:
		switch(data.u.data_var->obj.obj_type) {
		case Ncl_FileVar:
			file_md= (NclMultiDValData)_NclVarValueRead(data.u.data_var,NULL,NULL);
			tmp_file = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
    			if(tmp_file->file.advanced_file_structure)
				tmp = _NclGetFileInfo1(tmp_file);
    			else
				tmp = _NclGetFileInfo2(tmp_file);
			if((tmp!=NULL)&&( tmp->u.file->n_atts > 0 )) {
				for(i = 0; i < dimsize; i++) {
					for(j = 0; j < tmp->u.file->n_atts; j++) {
						ret=_NclFileWriteVarAtt(thefile,varnames[i],tmp->u.file->attnames[j],_NclFileReadAtt(tmp_file,tmp->u.file->attnames[j],NULL),NULL);
					}
				}
			} else {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"FileVarAttDef:  File variable (%s) has no attributes to assign",NrmQuarkToString(data.u.data_var->var.var_quark));
                                return(NhlWARNING);
                        }
			if (tmp)
				_NclFreeApiDataList((void*)tmp);
			break;
		default:
			if(data.u.data_var->var.att_id != -1) {
				tmp_attobj = (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"FileVarAttDef:  Variable (%s) has no attributes to assign", NrmQuarkToString(data.u.data_var->var.var_quark));
				return(NhlWARNING);
			}
			for(i = 0; i < dimsize; i++) {
				the_att_list = tmp_attobj->att.att_list;
				while(the_att_list != NULL) {
					ret=_NclFileWriteVarAtt(thefile,varnames[i],NrmStringToQuark(the_att_list->attname),the_att_list->attvalue,NULL);
					if(ret < NhlINFO) {
						ret0 = ret;
					}
					the_att_list = the_att_list->next;
				}
			}
		}


                break;
        case NclStk_VAL:
	default:
		NhlPError(NhlWARNING,NhlEUNKNOWN,"FileVarAttDef: A variable with attributes is expected not a value, No attributes to assign");
                return(NhlFATAL);
        }

	return(ret0);
}

NhlErrorTypes sprinti_W( void )
{
/*
 * Input array variables
 */
  int *input_var;
  NclQuark *format_string;
  int format_len = 256;
  char format_buf[256];
  char format_tail[256];
  char *v_loc, *pc_loc;
  int ndims_input_var;
  ng_size_t dsizes_input_var[NCL_MAX_DIMENSIONS];  /* not used: nlata, nlona, igrida[2]; */
  NclScalar missing_input_var;
  int has_missing_input_var;
  ng_size_t total_elements,i;
  char buffer[1024];
  size_t bufsiz = 1023;
  NclBasicDataTypes   type;

/*
 * Output array variables
 */
  NclQuark *output_var;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */

  format_string = (NclQuark*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           0);

  if (strlen(NrmQuarkToString(*format_string)) > format_len - 32) {
	  /* need to leave room in the format string buffer for changes */
	  NhlPError(NhlFATAL, NhlEUNKNOWN, "sprinti: format string cannot be longer than %d", format_len - 32);
	  return NhlFATAL;
  }
	  
  memset(format_buf,0,format_len);
  strncpy(format_buf,NrmQuarkToString(*format_string),format_len);

#if 0
/*Make sure having leading % in numerical type. Wei, Feb 15, 2014*/
  pc_loc = strchr(format_buf,'%');
  if(NULL == pc_loc)
  {
      memset(buffer,0,1024);
      strcpy(buffer, "%");
      strcat(buffer, format_buf);
      strcpy(format_buf, buffer);
      memset(buffer,0,1024);
  }
#endif
  
  pc_loc = format_buf;
  while ((pc_loc = strchr(pc_loc,'%'))) {
	  if (*(pc_loc + 1) == '%') {
		  pc_loc += 2;
		  continue;
	  }
	  else 
		  break;
  }

  if(NULL == pc_loc)
      v_loc = NULL;
  else
      v_loc = strchr(pc_loc,'V');

  memset(format_tail,0,format_len);
  if (v_loc)
	  strcpy(format_tail,v_loc + 1);
  pc_loc = format_tail;
  while ((pc_loc = strchr(pc_loc,'%'))) {
	  if (*(pc_loc + 1) == '%') {
		  pc_loc += 2;
		  continue;
	  }
	  else {
		  /* error -- only one format substitution allowed by this function */
		  NhlPError(NhlFATAL, NhlEUNKNOWN, "sprinti: only one format substitution allowed");
		  return NhlFATAL;
	  }
  }
  	  
  input_var = (void*)NclGetArgValue(
           1,
           2,
           &ndims_input_var, 
           dsizes_input_var,
	   &missing_input_var,
	   &has_missing_input_var,
           &type,
           0);
  /*
  * compute total number of elements
  */
  total_elements = 1;
  for(i = 0; i < ndims_input_var; i++) {
	total_elements *= dsizes_input_var[i];
  }
  output_var = (NclQuark*)malloc(sizeof(NclQuark)*total_elements);
  if (output_var == NULL) {
	  NhlPError(NhlFATAL, errno, " sprinti: memory allocation error");
	  return NhlFATAL;
  }

  /*
   * If a 'V' is found in a format string it is replaced with the appropriate format character(s) for the NCL integer type 
   * if and when required. Type sizes up to int can be printed as integers with no effect on the output.
   * Otherwise the string is left alone and it is assumed the user knows how to specify for the type involved.
   */
  switch (type) {
  case NCL_double:
  case NCL_float:
  default:
	  NhlPError(NhlFATAL, NhlEUNKNOWN, " sprinti: invalid type for input data array: must be an integer type (any size signed or unsigned)");
	  return NhlFATAL;
  case NCL_byte:
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,(int)((char*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_ubyte:
	  if (v_loc)
		  *v_loc = 'u';
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,(unsigned int)((unsigned char*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_short:
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,(unsigned int)((short*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_ushort:
	  if (v_loc)
		  *v_loc = 'u';
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,(unsigned int)((unsigned short*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_int:
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,((int*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_uint:
	  if (v_loc)
		  *v_loc = 'u';
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,((unsigned int*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_long:
	  if (v_loc && ! strstr(format_buf,"ld")) {
		  strcpy(v_loc,"ld");
		  strcpy(v_loc + 2,format_tail);
	  }
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,((long*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_ulong:	
	  if (v_loc && ! strstr(format_buf,"ld")) {
		  strcpy(v_loc,"lu");
		  strcpy(v_loc + 2,format_tail);
	  }
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,((unsigned long*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_int64:
	  if (v_loc && ! strstr(format_buf,"lld")) {
		  strcpy(v_loc,"lld");
		  strcpy(v_loc + 3,format_tail);
	  }
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,((long long*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  case NCL_uint64:
	  if (v_loc && ! strstr(format_buf,"lld")) {
		  strcpy(v_loc,"llu");
		  strcpy(v_loc + 3,format_tail);
	  }
	  for(i = 0; i < total_elements; i++) {
		  snprintf(buffer,bufsiz,format_buf,((unsigned long long*)input_var)[i]);
		  output_var[i] = NrmStringToQuark(buffer);
	  }
	  break;
  }
	  
  return(NclReturnValue((void*)output_var,ndims_input_var,dsizes_input_var,NULL,NCL_string,0));
}

NhlErrorTypes sprintf_W(void)
{
    /* Input */
    NclQuark  *format_string;
    void    *input_var;

    int ndims_input_var;
    ng_size_t dsizes_input_var[NCL_MAX_DIMENSIONS];

    NclScalar   missing_input_var;
    int has_missing_input_var;
    ng_size_t total_elements;
    NclBasicDataTypes   type;

    /* Output */
    NclQuark  *output_str;

    double  *tmp_d;
    float   *tmp_f;
    NclScalar   missing_output_var;

    int i;
    char buffer[255];

    /*
     * External functions necessary to coerce data.  They are located at:
     *  ../lib/nfp/wrapper.[ch]
     */
    extern double   *coerce_input_double(void *, NclBasicDataTypes, int, int,
                        NclScalar *, NclScalar *);
    extern float    *coerce_input_float(void *, NclBasicDataTypes, int, int,
                        NclScalar *, NclScalar *);
    extern void     coerce_missing(NclBasicDataTypes, int, NclScalar *,
                        NclScalar *, NclScalar *);


    /*
     * Retrieve parameters
     *
     * Note any of the pointer parameters can be set to NULL, which
     * implies you don't care about its value.
     */
    format_string = (NclQuark *) NclGetArgValue(
        0,
        2,
        NULL, 
        NULL,
        NULL,
        NULL,
        NULL,
        0);

    /*
     * Accept args of any numeric type and coerce as necessary.
     */
    input_var = (void *) NclGetArgValue(
        1,
        2,
        &ndims_input_var, 
        dsizes_input_var,
        &missing_input_var,
        &has_missing_input_var,
        &type, 
        0);

    /*
     * Compute total number of elements based on input and allocate
     * storage for the output strings.
     */
    total_elements = 1;
    for (i = 0; i < ndims_input_var; i++)
        total_elements *= dsizes_input_var[i];

    output_str = (NclQuark *) NclMalloc((unsigned int) sizeof(NclQuark) * total_elements);
    if (output_str == (NclQuark *) NULL) {
        NhlPError(NhlFATAL, errno, " sprintf(): memory allocation error (str)");
        return NhlFATAL;
    }

    switch (type) {
        case NCL_double:
            tmp_d = coerce_input_double(input_var, type, total_elements, 0, NULL, NULL);
            coerce_missing(type, has_missing_input_var, &missing_input_var,
                    &missing_output_var, NULL);

            for (i = 0; i < total_elements; i++) {
                (void) sprintf(buffer, NrmQuarkToString(*format_string), tmp_d[i]);
                output_str[i] = NrmStringToQuark(buffer);
            }
    
            break;

        case NCL_short:
            /* fall through */
        case NCL_int:
            /* fall through */
        case NCL_long:
            /* fall through */
        case NCL_float:
            /* fall through */
        default:
            tmp_f = coerce_input_float(input_var, type, total_elements, 0, NULL, NULL);
            coerce_missing(type, has_missing_input_var, &missing_input_var,
                    &missing_output_var, NULL);

            for (i = 0; i < total_elements; i++) {
                (void) sprintf(buffer, NrmQuarkToString(*format_string), tmp_f[i]);
                output_str[i] = NrmStringToQuark(buffer);
            }
            if(type != NCL_float) NclFree(tmp_f);

            break;
    }

    return NclReturnValue((void *) output_str, ndims_input_var, dsizes_input_var,
                            NULL, NCL_string, 0);
}

NhlErrorTypes _NclIAttSetValues( void )
{
	obj* objects;
	int ndims;
	ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing;
	int has_missing;
	ng_size_t total = 1;
        NclStackEntry data;
	NclAtt tmp_attobj;
	NclAttList *att_list;
	NhlGenArray *gen_array;
	int i, k;
	ng_size_t m;
	int  j, *ids;
	NclHLUObj tmp_hlu_ptr,tmp_hlu_ptr1;
	int rl_list;



  	objects = (obj*)NclGetArgValue(
           0,
           2,
           &ndims, 
           dimsizes,
	   &missing,
	   &has_missing,
           NULL,
           DONT_CARE);

        for(i = 0; i < ndims; i++) {
                total *= dimsizes[i];
        }


	data = _NclGetArg(1,2,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		if(data.u.data_var->var.att_id != -1) {
			tmp_attobj =  (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
			if(tmp_attobj == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"attsetvalues: Bad attribute list, can't continue");
				return(NhlFATAL);
			}
			if(tmp_attobj->att.n_atts == 0) {
				return(NhlNOERROR);
			}
			rl_list = NhlRLCreate(NhlSETRL);
			att_list = tmp_attobj->att.att_list;
			gen_array = NclMalloc((unsigned)sizeof(NhlGenArray)*tmp_attobj->att.n_atts);

			i = 0;
			while(att_list != NULL) {
				if(att_list->quark!=NrmStringToQuark("_FillValue")) {
					if(att_list->attvalue->multidval.hlu_type_rep[0] != NULL) {
						gen_array[i] = _NhlCreateGenArray(
							(NhlPointer)att_list->attvalue->multidval.val,
							att_list->attvalue->multidval.hlu_type_rep[0],
							att_list->attvalue->multidval.type->type_class.size,
							att_list->attvalue->multidval.n_dims,
							att_list->attvalue->multidval.dim_sizes,
							0);
						NhlRLSet(rl_list,NrmQuarkToString(att_list->quark),NhlTGenArray,gen_array[i]);
					} else {
						ids = NclMalloc((unsigned)sizeof(int)*att_list->attvalue->multidval.totalelements);
						m = 0;
						for(j = 0; j < att_list->attvalue->multidval.totalelements;j++) {
							if(att_list->attvalue->obj.obj_type_mask & Ncl_MultiDValHLUObjData ) {
								tmp_hlu_ptr= (NclHLUObj)_NclGetObj(((int*)att_list->attvalue->multidval.val)[j]);
								if(tmp_hlu_ptr != NULL) {
									ids[m++] = tmp_hlu_ptr->hlu.hlu_id;
									for(k = 0; k < total; k++) {
										if((!has_missing)||
											(objects[k]!= missing.objval))  {
											tmp_hlu_ptr1 = (NclHLUObj)_NclGetObj(objects[k]);
											if((tmp_hlu_ptr1 != NULL) &&(tmp_hlu_ptr->obj.obj_type_mask & Ncl_HLUObj)) {
												_NclAddHLUToExpList(tmp_hlu_ptr1,tmp_hlu_ptr->obj.id);
											}
										}
									}
								} else {
									NhlPError(NhlWARNING,NhlEUNKNOWN,"setvalues: Bad HLU id passed to setvalues, ignoring it");
								}
			 
							}
						}
						if(att_list->attvalue->obj.obj_type_mask & NCL_HLU_MASK){
							gen_array[i] = _NhlCreateGenArray(
								(NhlPointer)ids,
								NhlTInteger,
								sizeof(int),
								1,
								(ng_size_t *)&m,
								1);
							NhlRLSet(rl_list,
								NrmQuarkToString(att_list->quark),
								NhlTGenArray,
								gen_array[i]);
							NclFree(ids);
						} else {
							NclFree(ids);
							NhlPError(NhlWARNING,NhlEUNKNOWN,"attsetvalues: the value associated with (%s) does not have an HLU representation", NrmQuarkToString(att_list->quark));
							gen_array[i] = NULL;
						}
		 
					}
				}
				i++;
				att_list = att_list->next;
			}
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"attsetvalues: Variable (%s) does not have an attribute list, can't continue",NrmQuarkToString(data.u.data_var->var.var_quark));
			return(NhlFATAL);
		}
		break;
	default:
		NhlPError(NhlFATAL,NhlEUNKNOWN,"attsetvalues: Parameter 1 must be a variable, can't continue");
		return(NhlFATAL);
		break;
	}
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(objects[i] != missing.objval) {
				tmp_hlu_ptr =  (NclHLUObj)_NclGetObj(objects[i]);
				if(tmp_hlu_ptr != NULL) {
					NhlSetValues(tmp_hlu_ptr->hlu.hlu_id,rl_list);
				}
			} 
		}
	} else {
                for( i = 0; i < total; i++) {
                	tmp_hlu_ptr = (NclHLUObj)_NclGetObj(objects[i]);
			if(tmp_hlu_ptr != NULL) {
				NhlSetValues(tmp_hlu_ptr->hlu.hlu_id,rl_list);
			}
                }
	}
        if (tmp_attobj && gen_array) {
		for(i = 0; i < tmp_attobj->att.n_atts; i++) {
			if(gen_array[i])
				NhlFreeGenArray(gen_array[i]);
		}
		NhlFree(gen_array);
		NhlRLDestroy(rl_list);
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclIPush(void)
{
	obj *list_id;
	NclObj thelist = NULL;
	NclObj theobj = NULL;
        NclStackEntry data;

   	list_id = (obj*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);
	data= _NclGetArg(1,2,DONT_CARE);
	theobj = (NclObj)data.u.data_obj;

	thelist = _NclGetObj(*list_id);

	return(_NclListPush(thelist,theobj));
}

NhlErrorTypes _NclIAppend(void)
{
        obj *list_id;
        NclObj thelist = NULL;
        NclObj theobj = NULL;
        NclStackEntry data;

        list_id = (obj*)NclGetArgValue(
           0,
           2,
           NULL,
           NULL,
           NULL,
           NULL,
           NULL,
           DONT_CARE);
        data= _NclGetArg(1,2,DONT_CARE);
        theobj = (NclObj)data.u.data_obj;

        thelist = _NclGetObj(*list_id);

        return(ListAppend(thelist,theobj));
}
	
NhlErrorTypes _NclIPop(void)
{
	obj *list_id;
	NclObj thelist = NULL;
	NclObj tmp = NULL;
        NclStackEntry data;

   	list_id = (obj*)NclGetArgValue(
           0,
           1,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);
	thelist = _NclGetObj(*list_id);
	if(thelist != NULL) {
		tmp = _NclListPop(thelist);
		if(tmp != NULL) {
			if(tmp->obj.obj_type & (Ncl_Var | Ncl_FileVar | Ncl_HLUVar | Ncl_CoordVar)) {
				data.kind = NclStk_VAR;
				data.u.data_var= (NclVar)tmp;
			} else if(tmp->obj.obj_type & ( Ncl_MultiDValData | Ncl_MultiDValnclfileData | Ncl_MultiDValHLUObjData | Ncl_OneDValCoordData)) {
				data.kind = NclStk_VAL;
				data.u.data_obj= (NclMultiDValData)tmp;
			}
			_NclPlaceReturn(data);
			return(NhlNOERROR);
		} else {
			NhlPError(NhlWARNING,NhlEUNKNOWN,"Empty List: Returning missing value");
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMissing();
			_NclPlaceReturn(data);
			return(NhlWARNING);
		}
		
	} else {
		return(NhlFATAL);
	}

	
}
NhlErrorTypes _NclINewList( void )
{
	NclStackEntry data;
	char *tmp;
	int i;
	NclQuark *tmp_string;
	char buffer[10];
	
   	tmp_string = (NclQuark*)NclGetArgValue(
           0,
           1,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);
	
	tmp = NrmQuarkToString(*tmp_string);
	if(tmp == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NewList: unknow list type.");
		return(NhlFATAL);
	}

	strncpy(buffer, tmp, 5);
	buffer[4] = '\0';
	buffer[3] = '\0';
	for(i = 0; i < strlen(tmp); i++) {
		buffer[i] = tolower(tmp[i]);
		if('\0' == tmp[i])
			break;
	}

	data = _NclCreateAList(buffer);
	_NclPlaceReturn(data);
	return(NhlNOERROR);
}
	
NhlErrorTypes _NclIprintVarSummary( void )
{
	NclStackEntry data;

	data = _NclGetArg(0,1,DONT_CARE);
	if(data.kind == NclStk_VAR ) {
		_NclPrintVarSummary(data.u.data_var);
		return(NhlNOERROR);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"printVarSummary: non-variable passed; can't continue");
		return(NhlFATAL);
	}
}
NhlErrorTypes _NclIprintFileVarSummary( void )
{
	NclFile thefile;
	obj *thefile_id;
	NclQuark* var_string;

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        2,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);
        var_string = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

	_NclPrintFileVarSummary(thefile,*var_string);
	return(NhlNOERROR);

}

NhlErrorTypes _NclIgetfilepath( void )
{
	NclFile thefile;
	obj *thefile_id;
	ng_size_t dimsz[1];
	int ndims = 1;
	NclQuark* filepath = (NclQuark*) NclMalloc(sizeof(NclQuark));

	if(NULL == filepath)
	{
		NHLPERROR((NhlFATAL,NhlEUNKNOWN,"problem to allocate memory for filepath.\n"));
		return(NhlFATAL);
	}

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        1,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);

        *filepath = thefile->file.fpath;

	dimsz[0] = 1;

	/*
        *NclScalar missing;
        *missing.stringval = NrmStringToQuark("missing");
        **filepath = NrmStringToQuark("missing");
        *return NclReturnValue((void *) filepath, ndims, dimsz, &missing, NCL_string, 0);
	*/
	return NclReturnValue((void *) filepath, ndims, dimsz, NULL, NCL_string, 0);
}

NhlErrorTypes _NclIGetFileGroups( void )
{
	NclFile thefile;
	obj *thefile_id;
	NclQuark *base_group_name;
	NclQuark *selected_group_names;
	int *depth;
	int n_grps = 0;
	int ndims = 1;
        ng_size_t dimsz[1];

      /*
       *fprintf(stdout, "\n\n\nhit _NclIGetFileGroups. file: %s, line: %d\n", __FILE__, __LINE__);
       */

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);

        base_group_name = (NclQuark*)NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

        depth = (int*)NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

      /*
       *fprintf(stdout, "\n\n\nhit _NclIGetFileGroups. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tbase_group_name: <%s>\n", NrmQuarkToString(base_group_name[0]));
       *fprintf(stdout, "\tdepth: %d\n", depth[0]);
       */

        selected_group_names = _NclGetFileGroupsList(thefile, *base_group_name, depth[0], &n_grps);
        selected_group_names = (NclQuark *) NclRealloc(selected_group_names, sizeof(NclQuark) * n_grps);

      /*
       *fprintf(stdout, "\tn_grps: %d\n", n_grps);
       */

	dimsz[0] = n_grps;
	return NclReturnValue((void *) selected_group_names, ndims, dimsz, NULL, NCL_string, 0);
}

NhlErrorTypes _NclIGetGroupVars( void )
{
	NclFile thefile;
	obj *thefile_id;
	NclQuark *base_group_name;
	NclQuark *selected_var_names;
	int *depth;
	int n_vars = 0;
	int ndims = 1;
        ng_size_t dimsz[1];

      /*
       *fprintf(stdout, "\n\n\nhit _NclIGetGroupVars. file: %s, line: %d\n", __FILE__, __LINE__);
       */

        thefile_id = (obj*)NclGetArgValue(
                        0,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);
	thefile = (NclFile)_NclGetObj((int)*thefile_id);

        base_group_name = (NclQuark*)NclGetArgValue(
                        1,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

        depth = (int*)NclGetArgValue(
                        2,
                        3,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        NULL,
                        0);

      /*
       *fprintf(stdout, "\n\n\nhit _NclIGetGroupVars. file: %s, line: %d\n", __FILE__, __LINE__);
       *fprintf(stdout, "\tbase_group_name: <%s>\n", NrmQuarkToString(base_group_name[0]));
       *fprintf(stdout, "\tdepth: %d\n", depth[0]);
       */

        selected_var_names = _NclGetGroupVarsList(thefile, *base_group_name, depth[0], &n_vars);
        selected_var_names = (NclQuark *) NclRealloc(selected_var_names, sizeof(NclQuark) * n_vars);

      /*
       *fprintf(stdout, "\tn_vars: %d\n", n_vars);
       */

	dimsz[0] = n_vars;
	return NclReturnValue((void *) selected_var_names, ndims, dimsz, NULL, NCL_string, 0);
}

NhlErrorTypes _NclILoadScript( void )
{
	NclStackEntry path;
	NclMultiDValData p_md = NULL;

	path =  _NclGetArg(0,1,DONT_CARE);
	if(path.kind == NclStk_VAR) {
		if(path.u.data_var != NULL) {
			p_md = _NclVarValueRead(path.u.data_var,NULL,NULL);
		}
	} else if(path.kind == NclStk_VAL) {
		p_md = path.u.data_obj;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"loadscript: arg 0 is incorrect type");
		return(NhlFATAL);
	}
	/* 
	 * Note the line is incremented for the parser of the script/session that calls
	 * the loadscript procedure -- it's not for the script that is loaded. That is
	 * why it needs to be called prior to calling _NclPreLoad..
	 */
	if (p_md && (p_md->multidval.type->type_class.type & Ncl_Typestring)) {
		IncLine();
		_NclPreLoadScript(NrmQuarkToString(*((NclQuark*)p_md->multidval.val)),0);
	}
	else {
		return(NhlFATAL);
	}

	return(NhlNOERROR);

}

NhlErrorTypes _NclIAddFiles( void )
{
	NclStackEntry path;
	NclStackEntry data;
	NclStackEntry rw_status;
	NclMultiDValData p_md = NULL;
	NclMultiDValData rw_md = NULL;
	NclFile file = NULL;
	NclMultiDValData out_md = NULL;
	char *rw;
	ng_size_t  i;
	int rw_v;
	int *id = (int*)NclMalloc((unsigned)sizeof(int));
	ng_size_t dim_size = 1,one = 1;
	obj *tmp_obj = NULL; 
	NclList tmp_list;
	
/*
* Guarenteed to be scalar string
*/
	path =  _NclGetArg(0,2,DONT_CARE);
	rw_status = _NclGetArg(1,2,DONT_CARE);

	if(path.kind == NclStk_VAR) {
		if(path.u.data_var != NULL) {
			p_md = _NclVarValueRead(path.u.data_var,NULL,NULL);
		}
	} else if(path.kind == NclStk_VAL) {
		p_md = path.u.data_obj;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"addfile: incorrect type of object passed to addfile");
		NclFree(id);
		return(NhlFATAL);
	}
	if(rw_status.kind == NclStk_VAR) {
		if(rw_status.u.data_var != NULL) {
			rw_md = _NclVarValueRead(rw_status.u.data_var,NULL,NULL);
		}
	} else if(rw_status.kind == NclStk_VAL) {
		rw_md = rw_status.u.data_obj;
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"addfile: incorrect type of object passed to addfile");
		NclFree(id);
		return(NhlFATAL);
	}
	rw = NrmQuarkToString(*(NclQuark*)rw_md->multidval.val);
	if((strrchr(rw,'c') != NULL)||(strrchr(rw,'C') != NULL)) {
		rw_v = -1;
	} else if((strrchr(rw,'w') == NULL)&&(strrchr(rw,'W') == NULL)) {
		rw_v = 1;
	} else {
		rw_v = 0;
	} 
	tmp_list = (NclList)_NclListCreate(NULL,NULL,0,0,NCL_CONCAT | NCL_FIFO);

	*id = tmp_list->obj.id;
	data.kind = NclStk_VAL;
	data.u.data_obj = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,0,id,NULL,1,&one,TEMPORARY,NULL);
        _NclListSetType((NclObj)tmp_list,NCL_FIFO);

	for (i = p_md->multidval.totalelements-1;i>=0; i--) {

		file = _NclOpenFile(NULL,NULL,Ncl_File,0,TEMPORARY,((NclQuark*)p_md->multidval.val)[i],rw_v);
		if(file != NULL) {
			id = (int*)NclMalloc((unsigned)sizeof(int));
			*id = file->obj.id;
			out_md = _NclMultiDValnclfileDataCreate(NULL,NULL,Ncl_MultiDValnclfileData,0,id,NULL,1,&dim_size,TEMPORARY,NULL);
			if((out_md == NULL)|| (_NclListPush((NclObj)tmp_list,(NclObj)out_md) == NhlFATAL)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"addfiles: an error occurred opening %s, can't continue",NrmQuarkToString(((NclQuark*)p_md->multidval.val)[i]));	
				return(NhlFATAL);	
			}
		} else {
			tmp_obj =(obj*) NclMalloc(((NclTypeClass)nclTypeobjClass)->type_class.size);
			*tmp_obj = ((NclTypeClass)nclTypeobjClass)->type_class.default_mis.objval;
			out_md = _NclMultiDValnclfileDataCreate(
					NULL,
					NULL,
					Ncl_MultiDValnclfileData,
					0,
					(void*)tmp_obj,
					(void*)&((NclTypeClass)nclTypeobjClass)->type_class.default_mis,
					1,
					&dim_size,
					TEMPORARY,
					NULL);
			if((out_md == NULL)|| (_NclListPush((NclObj)tmp_list,(NclObj)out_md) == NhlFATAL)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"addfiles: an error occurred opening %s, can't continue",NrmQuarkToString(((NclQuark*)p_md->multidval.val)[i]));	
				return(NhlFATAL);	
			}
		}
		file = NULL;
		out_md = NULL;
	}
	_NclPlaceReturn(data);
	return(NhlNOERROR);
}
NhlErrorTypes _NclIListGetType(void)
{
	obj *list_id;
	NclObj thelist = NULL;
	NclQuark *ret_val;
	ng_size_t dimsize = 2;
	int i;
	int list_type;

	

   	list_id = (obj*)NclGetArgValue(
           0,
           1,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);
	thelist = _NclGetObj(*list_id);
	list_type = _NclListGetType(thelist);
	i = 0;
	ret_val = (NclQuark*)NclMalloc(2 * sizeof(NclQuark));
	if(list_type & NCL_JOIN)  {
		ret_val[i++] = NrmStringToQuark("join");
	} else if(list_type & NCL_CONCAT) {
		ret_val[i++] = NrmStringToQuark("cat");
	}

	if(list_type & NCL_FIFO) {
		ret_val[i++] = NrmStringToQuark("fifo");
	} else if(list_type & NCL_LIFO) {
		ret_val[i++] = NrmStringToQuark("lifo");
	} else if(list_type & NCL_STRUCT) {
		ret_val[i++] = NrmStringToQuark("struct");
	}
	
	if(i == 1)
	{
		dimsize = 1;
		ret_val = (NclQuark*)NclRealloc(ret_val, sizeof(NclQuark));
	}

	return(NclReturnValue(
		ret_val,
		1,
		&dimsize,
		NULL,
		NCL_string,
		0
	));

}
NhlErrorTypes _NclIListSetType(void)
{
	obj *list_id;
	NclObj thelist = NULL;
	NclQuark *option;
	char *tmp;	
	char buffer[16];
	int i, buflen;

   	list_id = (obj*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);
   	option = (NclQuark*)NclGetArgValue(
           1,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);

	thelist = _NclGetObj(*list_id);
	tmp = NrmQuarkToString(*option);
	if(tmp == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSetType: unknown list type. Only \"join\", \"cat\", \"fifo\", and \"lifo\" supported");
		return(NhlFATAL);
	}
	buflen = strlen(tmp);
	for(i = 0; i < buflen; i++) {
		buffer[i] = tolower(tmp[i]);
	}
	buffer[buflen] = '\0';
	if(strcmp(buffer,"join") ==0) {
		_NclListSetType(thelist, NCL_JOIN);
	} else if((strcmp(buffer,"cat") == 0) || (strcmp(buffer,"concat") == 0)) {
		_NclListSetType(thelist, NCL_CONCAT);
	} else if(strcmp(buffer,"item") == 0) {
		_NclListSetType(thelist, NCL_STRUCT);
	} else if(strcmp(buffer,"compound") == 0) {
		_NclListSetType(thelist, NCL_COMPOUND);
	} else if(strcmp(buffer,"fifo") == 0) {
		_NclListSetType(thelist, NCL_FIFO);
	} else if(strcmp(buffer,"lifo") == 0) {
		_NclListSetType(thelist, NCL_LIFO);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ListSetType: unknown list type. Only \"join\", \"cat\", \"fifo\", and \"lifo\" supported");
		return(NhlFATAL);
	}
	return(NhlNOERROR);
}

NhlErrorTypes _NclIListCount(void)
{
	obj *list_id;
	ng_size_t dimsize = 1;
	int *ret_val;
	NclObj theobj;

   	list_id = (obj*)NclGetArgValue(
           0,
           1,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);

	ret_val = (int*)NclCalloc(1, sizeof(int));
	if(ret_val == NULL)
	{
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ListCount: problem to allocate memory.");
		return(NhlFATAL);
	}

	theobj = (NclObj)_NclGetObj(*list_id);

	if(theobj == NULL)
	{
		NHLPERROR((NhlWARNING,NhlEUNKNOWN,
			"_NclIListCount: Cannot get count of NULL list."));
		return(NhlWARNING);
	}
	else
	{
		NclList thelist = (NclList) theobj;
		ret_val[0] = (int)thelist->list.nelem;
	}

	return(NclReturnValue(
		ret_val,
		1,
		&dimsize,
		NULL,
		NCL_int,
		0
	));
}

NhlErrorTypes _NclIListIndex(void)
{
	obj *list_id;
	NclList thelist = NULL;
	ng_size_t dimsize = 1;
	int *ret_val;
	int nm = 0;
	int i;

	NrmQuark symbol_name;
	NclObj the_obj;
	NclVar cur_var;
	NclMultiDValData the_value;
	NclMultiDValData cur_value;

        NclStackEntry data;

	NclListObjList *step;

	int comp_val = 0;

   	list_id = (obj*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);

	data = _NclGetArg(1,2,DONT_CARE);

	thelist = (NclList)_NclGetObj(*list_id);

	if(NclStk_VAL == data.kind)
	{
		comp_val = 1;
		the_value = (NclMultiDValData)data.u.data_obj;

		if(NCL_string == the_value->multidval.data_type)
		{
			comp_val = -1;
			symbol_name = *(NrmQuark *)the_value->multidval.val;
		}
	}
	else
	{
		comp_val = 0;
		the_obj = (NclObj)data.u.data_obj;
	}

	ret_val = (int*)NclMalloc(thelist->list.nelem * sizeof(int));
	if(ret_val == NULL)
	{
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ListIndex: problem to allocate memory.");
		return(NhlFATAL);
	}

	ret_val[0] = -1;
	step = thelist->list.first;

	if(0 < comp_val)
	{
		for(i = 0; i < thelist->list.nelem; i++)
		{
			cur_var = (NclVar)_NclGetObj(step->obj_id);

			if(!(cur_var->var.thesym))
			{
				cur_value = (NclMultiDValData)_NclGetObj(cur_var->var.thevalue_id);

				if((the_value->multidval.data_type == cur_value->multidval.data_type) &&
				   (the_value->multidval.kind      == cur_value->multidval.kind) &&
				   (the_value->multidval.n_dims    == cur_value->multidval.n_dims) &&
				   (the_value->multidval.totalsize == cur_value->multidval.totalsize))
				{
					int match = memcmp(the_value->multidval.val, cur_value->multidval.val, the_value->multidval.totalsize);
					if(!match)
						ret_val[nm++] = i;
				}
			}

			step = step->next;
		}
	}
	else if(0 > comp_val)
	{
		for(i = 0; i < thelist->list.nelem; i++)
		{
			cur_var = (NclVar)_NclGetObj(step->obj_id);

			if(symbol_name == cur_var->var.var_quark)
			{
				ret_val[nm++] = i;
			}

			step = step->next;
		}
	}
	else
	{
		for(i = 0; i < thelist->list.nelem; i++)
		{
			if(the_obj->obj.id == step->obj_id)
			{
				ret_val[nm++] = i;
			}

			step = step->next;
		}
	}

	if(nm < 1)
            nm = 1;

	dimsize = nm;
	if(nm < thelist->list.nelem)
	{
		ret_val = (int *)NclRealloc(ret_val, nm*sizeof(int));
		if(ret_val == NULL)
		{
				NhlPError(NhlFATAL,NhlEUNKNOWN,"ListIndex: problem to reallocate memory.");
			return(NhlFATAL);
		}
	}

	return(NclReturnValue(
		ret_val,
		1,
		&dimsize,
		NULL,
		NCL_int,
		0
	));
}

NhlErrorTypes _NclIListIndexFromName(void)
{
	obj *list_id;
	NclList thelist = NULL;
	ng_size_t dimsize = 1;
	int *ret_val;
	int nm = 0;
	int i;

	NclQuark *var_name;
	NclVar cur_var;

	NclListObjList *step;

	int comp_val = 0;

   	list_id = (obj*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);

	thelist = (NclList)_NclGetObj(*list_id);

        var_name = (NclQuark*)NclGetArgValue(
	           1,
	           2,
		   NULL,
		   NULL,
		   NULL,
		   NULL,
		   NULL,
		   DONT_CARE);

	ret_val = (int*)NclMalloc(thelist->list.nelem * sizeof(int));
	if(ret_val == NULL)
	{
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ListIndexFromName: problem to allocate memory.");
		return(NhlFATAL);
	}

	ret_val[0] = -1;
	step = thelist->list.first;

	for(i = 0; i < thelist->list.nelem; i++)
	{
		cur_var = (NclVar)_NclGetObj(step->obj_id);

		if((*var_name) == cur_var->var.var_quark)
			ret_val[nm++] = i;

		step = step->next;
	}

	if(nm < 1)
            nm = 1;

	dimsize = nm;
	if(nm < thelist->list.nelem)
	{
		ret_val = (int *)NclRealloc(ret_val, nm*sizeof(int));
		if(ret_val == NULL)
		{
			NhlPError(NhlFATAL,NhlEUNKNOWN,"ListIndex: problem to reallocate memory.");
			return(NhlFATAL);
		}
	}

	return(NclReturnValue(
		ret_val,
		1,
		&dimsize,
		NULL,
		NCL_int,
		0));
}

NhlErrorTypes _NclIListVarNameFromIndex(void)
{
	obj *list_id;
	NclList thelist = NULL;
	ng_size_t dimsize = 1;
	NclQuark *ret_val;
	int nm = 1;
	int i;

	int *idx;
	NclQuark var_name = -1;
	NclVar cur_var;

	NclListObjList *step;

	int comp_val = 0;

   	list_id = (obj*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           DONT_CARE);

	thelist = (NclList)_NclGetObj(*list_id);

        idx = (int*)NclGetArgValue(
	           1,
	           2,
		   NULL,
		   NULL,
		   NULL,
		   NULL,
		   NULL,
		   DONT_CARE);

	ret_val = (NclQuark*)NclMalloc(sizeof(NclQuark));
	if(ret_val == NULL)
	{
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ListIndexFromName: problem to allocate memory.");
		return(NhlFATAL);
	}

	ret_val[0] = -1;

        if((idx[0] >= 0) && (idx[0] < thelist->list.nelem))
	{
		step = thelist->list.first;

		for(i = 0; i < idx[0]; i++)
			step = step->next;

		cur_var = (NclVar)_NclGetObj(step->obj_id);

		ret_val[0] = cur_var->var.var_quark;
	}

	return(NclReturnValue(
		ret_val,
		1,
		&dimsize,
		NULL,
		NCL_string,
		0));
}

static nc_type _MapType (NclBasicDataTypes data_type) {
	nc_type the_type;
		switch(data_type) {
			case NCL_short:
				the_type = NC_SHORT;
				break;
			case NCL_logical:
			case NCL_int:
			case NCL_long:
				the_type = NC_LONG;
				break;
			case NCL_float:
				the_type = NC_FLOAT;
				break;
			case NCL_double:
				the_type = NC_DOUBLE;
				break;
			case NCL_char:
				the_type = NC_CHAR;
				break;
			case NCL_byte:
				the_type = NC_BYTE;
				break;
			default:
				the_type = 0;
		}
	return(the_type);
}

NhlErrorTypes _NclICreateFile(void)
{
	NclStackEntry out_data,data;
	NclQuark *path;
	NclQuark *dimnames;
	void *tmp_dimsizes;
	ng_size_t *dimsizes;
	obj *varinfo;
	NclObj fileatts_obj;
	ng_size_t nd, nd0;
	ng_size_t n_vars;
	char filename_buffer[2048];
	NclList  varinfo_obj;
	NclListObjList *thelist;
	ng_size_t i,k;
	ng_size_t  j;
	NclVar tmp_var;
	NclMultiDValData dnames_md,tmp_md;
	nc_type the_type;
	nc_type tmp_type;
	int varids[2048];
	int dim_ids[2048];
	int ids[2048];
	int cdfid;
	NclAtt tmp_att;
	NclAttList *nclattlist;
        NclFile file = NULL;
        NclMultiDValData out_md = NULL;
        int *id = (int*)NclMalloc((unsigned)sizeof(int));
        ng_size_t dim_size = 1;
	NclBasicDataTypes ncl_var_type, type_dimsizes;
	int unlimited_id = -1;

  	path = (NclQuark*)NclGetArgValue(
           0,
           5,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   DONT_CARE);
  	dimnames = (NclQuark*)NclGetArgValue(
           1,
           5,
	   NULL,
	   &nd,
	   NULL,
	   NULL,
	   NULL,
	   DONT_CARE);
  	tmp_dimsizes = (void*)NclGetArgValue(
           2,
           5,
	   NULL,
	   &nd0,
	   NULL,
	   NULL,
	   &type_dimsizes,
	   DONT_CARE);

  	varinfo = (obj*)NclGetArgValue(
           3,
           5,
	   NULL,
	   &n_vars,
	   NULL,
	   NULL,
	   NULL,
	   DONT_CARE);
	data= _NclGetArg(4,5,DONT_CARE);
	fileatts_obj = (NclObj)data.u.data_obj;
	
	dimsizes = get_dimensions(tmp_dimsizes,nd0,type_dimsizes,
				  "createfile");

	sprintf(filename_buffer,"%s",NrmQuarkToString(*path));
	if(NrmStringToQuark(&(filename_buffer[strlen(filename_buffer)-3]))!= NrmStringToQuark(".nc")) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: Only netCDF creation is supported at this time. Use \".nc\" suffix");
		return(NhlFATAL);
	}

	cdfid = nccreate(NrmQuarkToString(*path),(NC_WRITE|NC_NOCLOBBER));

	ncendef(cdfid);
	ncclose(cdfid);

	cdfid = ncopen(NrmQuarkToString(*path),(NC_WRITE|NC_NOCLOBBER));
	ncredef(cdfid);

	if(cdfid == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: The specified netCDF file can't be created, either the file exists or the path is incorrect");
		return(NhlFATAL);
	}
/*	for(i = 0; i < n_dims; i++) {*/
	for(i = 0; i < nd; i++) {
		if(dimsizes[i] == -1) {	
			dim_ids[i] = ncdimdef(cdfid,NrmQuarkToString(dimnames[i]),NC_UNLIMITED);
			unlimited_id = dim_ids[i];
		} else {
			dim_ids[i] = ncdimdef(cdfid,NrmQuarkToString(dimnames[i]),(long)dimsizes[i]);
		}
	}
	varinfo_obj = (NclList)_NclGetObj(*varinfo);
	thelist = varinfo_obj->list.first;

	i=0;
	while(thelist != NULL) {
		tmp_var = (NclVar)_NclGetObj(thelist->obj_id);
		if(tmp_var == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: Something is wrong with the varinfo parameter");
			return(NhlFATAL);
	
		}
		tmp_md = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
		if(tmp_md->multidval.data_type != NCL_string) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: varinfo parameter must be a list of string variable names");
			return(NhlFATAL);
		}
		if(tmp_var->var.att_id != -1) {
			tmp_att= (NclAtt)_NclGetObj(tmp_var->var.att_id);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: varinfo parameter list elements must at minimum contain the attributes \"dims\" and \"type\" ");
			return(NhlFATAL);
		}
		nclattlist = tmp_att->att.att_list;
		dnames_md = NULL;
		while(nclattlist != NULL) {
			if(nclattlist->quark == NrmStringToQuark("dims")) {
				dnames_md = nclattlist->attvalue;
				break;
			} else {
				nclattlist = nclattlist->next;
			}
		}
		if(dnames_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: varinfo parameter list elements must at minimum contain the attributes \"dims\" and \"type\", attribute \"dims\" not found");
			return(NhlFATAL);
		}
		for(j = 0; j < dnames_md->multidval.totalelements; j++) {
			ids[j] = -2;
/*			for(k=0; k < n_dims; k++) {*/
			for(k=0; k < nd; k++) {
				if(((NclQuark*)(dnames_md->multidval.val))[j] == dimnames[k]) {
					ids[j] = dim_ids[k];
					if((unlimited_id == ids[j])&&(j != 0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: unlimited dimension must be first dimension");
						return(NhlFATAL);
					}
				}
			}
			if(ids[j] == -2) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: dimension named %s was not defined in dimension info list can't continue",NrmQuarkToString(((NclQuark*)(dnames_md->multidval.val))[j]));
				return(NhlFATAL);
			}
		}
		nclattlist = tmp_att->att.att_list;
		the_type = 0;
		while(nclattlist != NULL) {
			if(nclattlist->quark == NrmStringToQuark("type")){
				the_type = _MapType(_NclKeywordToDataType( _NclLookUp(NrmQuarkToString(*(NclQuark*)(nclattlist->attvalue->multidval.val)))));
				ncl_var_type = _NclKeywordToDataType( _NclLookUp(NrmQuarkToString(*(NclQuark*)(nclattlist->attvalue->multidval.val))));
				break;
			} else {
				nclattlist = nclattlist->next;
			}
		}
		if(the_type == 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: Either an unsupported type was requested or the \"type\" attribute was not supplied");
			return(NhlFATAL);
		}

		varids[i]= ncvardef(cdfid,NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val),the_type,dnames_md->multidval.totalelements,ids);
		nclattlist = tmp_att->att.att_list;
		while(nclattlist != NULL){ 
			if((nclattlist->quark == NrmStringToQuark("set_fillvalue"))||(nclattlist->quark == NrmStringToQuark("_FillValue"))) {
				tmp_type = _MapType(nclattlist->attvalue->multidval.data_type);
				if((tmp_type == the_type)&&(nclattlist->quark == NrmStringToQuark("set_fillvalue"))) {
					ncattput(cdfid,varids[i],"_FillValue",the_type,nclattlist->attvalue->multidval.totalelements,nclattlist->attvalue->multidval.val);
				} else {
					if(nclattlist->quark == NrmStringToQuark("_FillValue"))  {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"createfile: _FillValue attributes cannot be set in VarInfo parameter, use set_fillvalue, using default missing value for variable type");
					} else {
						NhlPError(NhlWARNING,NhlEUNKNOWN,"createfile: set_fillvalue attribute is a different type than the variable, using default missing value for variable type");
					}
					switch(ncl_var_type) {
					case NCL_float:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypefloatClassRec.type_class.default_mis.floatval));
						break;
					case NCL_logical:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypelogicalClassRec.type_class.default_mis.logicalval));
						break;
					case NCL_char:
					case NCL_string:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypecharClassRec.type_class.default_mis.charval));
						break;
					case NCL_double:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypedoubleClassRec.type_class.default_mis.doubleval));
						break;
					case NCL_byte:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypebyteClassRec.type_class.default_mis.byteval));
						break;
					case NCL_int:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypeintClassRec.type_class.default_mis.intval));
						break;
					case NCL_long:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypelongClassRec.type_class.default_mis.longval));
						break;
					case NCL_short:
						ncattput(cdfid,varids[i],"_FillValue",the_type,1,&(nclTypeshortClassRec.type_class.default_mis.shortval));
						break;
					default:
						NHLPERROR((NhlFATAL,NhlEUNKNOWN,"createfile: unsupported NetCDF 3 type"));
						return(NhlFATAL);
					}
				}
			} else if((nclattlist->quark != NrmStringToQuark("type"))&&(nclattlist->quark!=NrmStringToQuark("dims"))){
				if(nclattlist->attvalue->multidval.data_type != NCL_string) {		
					tmp_type = _MapType(nclattlist->attvalue->multidval.data_type);
					ncattput(cdfid,varids[i],nclattlist->attname,tmp_type,nclattlist->attvalue->multidval.totalelements,nclattlist->attvalue->multidval.val);
				} else {
					ncattput(cdfid,varids[i],nclattlist->attname,NC_CHAR,strlen(NrmQuarkToString(*(NclQuark*)nclattlist->attvalue->multidval.val)),NrmQuarkToString(*(NclQuark*)nclattlist->attvalue->multidval.val));
				}
			} 
			nclattlist = nclattlist->next;
		}


		thelist = thelist->next;
		i++;
	}

	if(fileatts_obj->obj.obj_type & Ncl_Var) {
		if(((NclVar)fileatts_obj)->var.att_id != -1) {
			tmp_att = (NclAtt)_NclGetObj(((NclVar)fileatts_obj)->var.att_id);
			nclattlist = tmp_att->att.att_list;
			while(nclattlist != NULL){ 
				if((nclattlist->quark != NrmStringToQuark("type"))&&(nclattlist->quark!=NrmStringToQuark("dims"))){
					if(nclattlist->attvalue->multidval.data_type != NCL_string) {		
						the_type = _MapType(nclattlist->attvalue->multidval.data_type);
						ncattput(cdfid,NC_GLOBAL,nclattlist->attname,the_type,nclattlist->attvalue->multidval.totalelements,nclattlist->attvalue->multidval.val);
					} else {
						ncattput(cdfid,NC_GLOBAL,nclattlist->attname,NC_CHAR,strlen(NrmQuarkToString(*(NclQuark*)nclattlist->attvalue->multidval.val)),NrmQuarkToString(*(NclQuark*)nclattlist->attvalue->multidval.val));
					}
				}
				nclattlist = nclattlist->next;
			}
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"createfile: fileatts parameter must be a variable, which optionally contains global file attributes, a value was passed in"); 
	}
	ncendef(cdfid);
	/*nc__enddef(cdfid,65536,4,0,4); */
	ncclose(cdfid);


        file = _NclOpenFile(NULL,NULL,Ncl_File,0,TEMPORARY,*(NclQuark*)path,0);
        if(file != NULL) {
                *id = file->obj.id;
                out_md = _NclMultiDValnclfileDataCreate(NULL,NULL,Ncl_MultiDValnclfileData,0,id,NULL,1,&dim_size,TEMPORARY,NULL);
                if(out_md != NULL) {
                        out_data.kind = NclStk_VAL;
                        out_data.u.data_obj = out_md;
                        _NclPlaceReturn(out_data);
                        return(NhlNOERROR);
                } else {
                        NclFree(id);
                        _NclDestroyObj((NclObj)file);
                        return(NhlFATAL);
                }
	}
	NclFree(dimsizes);
	return(NhlNOERROR);
}

NhlErrorTypes _NclISetFileOption(void)
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp_md1 = NULL;
	NclFile f = NULL;
	NclQuark filetype = NrmNULLQUARK;
	NclQuark option;
	NhlErrorTypes ret;
	int n_dims = 1;
	int n = 0;

	NrmQuark filetype_lower;
	NrmQuark option_lower;
	NrmQuark fs_quark = NrmStringToQuark("filestructure");
	NrmQuark ad_lower_quark = NrmStringToQuark("advanced");

	NrmQuark all_quark = NrmStringToQuark("all");
	NrmQuark  nc_quark = NrmStringToQuark("nc");
	NrmQuark  h5_quark = NrmStringToQuark("h5");
	NrmQuark he5_quark = NrmStringToQuark("he5");
	NrmQuark shp_quark = NrmStringToQuark("shp");

	NrmQuark fso;

	data = _NclGetArg(0,3,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md == NULL)
		return(NhlFATAL);
	if (tmp_md->multidval.data_type == NCL_string) {
		filetype = *(NclQuark*)tmp_md->multidval.val;
	}
	else if (tmp_md->multidval.data_type == NCL_obj &&
		 (tmp_md->obj.obj_type_mask & Ncl_MultiDValnclfileData)) {
		f = (NclFile)_NclGetObj(*(obj*)tmp_md->multidval.val);
	}
	else {
		NhlPError(NhlWARNING, NhlEUNKNOWN, 
			  "setfileoption: first argument must be a file object or a string representing a supported data format");
		return NhlWARNING;
	}


  	option = *(NclQuark*)NclGetArgValue(
           1,
           3,
	   NULL,
	   (ng_size_t *) &n_dims,
	   NULL,
	   NULL,
	   NULL,
	   DONT_CARE);

	data = _NclGetArg(2,3,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
		tmp_md1 = _NclVarValueRead(data.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md1 = (NclMultiDValData)data.u.data_obj;
		break;
        default:
                NHLPERROR((NhlFATAL,NhlEUNKNOWN,"Internal error"));
                return(NhlFATAL);
    	}
	if(tmp_md1 == NULL)
		return(NhlFATAL);


	ret = _NclFileSetOption(f,filetype,option,tmp_md1);

	return ret;
}	

NhlErrorTypes   _NclIGetFileVarTypes
# if    NhlNeedProto
(void)
# else
()
# endif /* NhlNeedProto */
{
    /* file variables */
    NclFile f;
    int *fid;

    /* var names, types */
    NclQuark  *varnames;
    NclQuark    *vartypes = NULL;
    NclObjTypes vartype;

    /* dimensions, sizes */
    int ndims;
    ng_size_t dimsz[NCL_MAX_DIMENSIONS];
    int sz = 1;

    NclScalar   missing;
    int has_missing = False;

    int i = 0;


    /* get file information (1st arg.) */
    fid = (int *) NclGetArgValue(
                    0,
                    2,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);
    f = (NclFile) _NclGetObj((int) *fid);

    /* get variable information (2nd arg.) */
    varnames = (NclQuark *) NclGetArgValue(
                    1,
                    2,
                    &ndims,
                    dimsz,
                    NULL,
                    NULL,
                    NULL,
                    0);

    /* calculate total number of input variable names */
    for (i = 0; i < ndims; i++)
        sz *= dimsz[i];

    /* the type string(s) to return */
    vartypes = (NclQuark *) NclMalloc((unsigned int) sizeof(NclQuark) * sz);
    if (vartypes == (NclQuark *) NULL) {
        NhlPError(NhlFATAL, errno, "getfilevartypes: memory allocation error");
        return NhlFATAL;
    }

    for (i = 0; i < sz; i++) {
	  NrmQuark fname;
	  vartype = _NclFileVarRepValue(f, varnames[i]); 
	  switch (vartype) {
	    	case Ncl_Typedouble:
		    	vartypes[i] = NrmStringToQuark("double");
			    break;

    		case Ncl_Typefloat:
	    		vartypes[i] = NrmStringToQuark("float");
		    	break;

    		case Ncl_Typeint64:
	    		vartypes[i] = NrmStringToQuark("int64");
		    	break;

    		case Ncl_Typeuint64:
	    		vartypes[i] = NrmStringToQuark("uint64");
		    	break;

    		case Ncl_Typelong:
	    		vartypes[i] = NrmStringToQuark("long");
		    	break;

    		case Ncl_Typeulong:
	    		vartypes[i] = NrmStringToQuark("ulong");
		    	break;

    		case Ncl_Typeint:
	    		vartypes[i] = NrmStringToQuark("integer");
		    	break;

    		case Ncl_Typeuint:
	    		vartypes[i] = NrmStringToQuark("uint");
		    	break;

    		case Ncl_Typeshort:
	    		vartypes[i] = NrmStringToQuark("short");
		    	break;

    		case Ncl_Typeushort:
	    		vartypes[i] = NrmStringToQuark("ushort");
		    	break;

       		case Ncl_Typebyte:
	    		vartypes[i] = NrmStringToQuark("byte");
		    	break;

    		case Ncl_Typeubyte:
	    		vartypes[i] = NrmStringToQuark("ubyte");
		    	break;

    		case Ncl_Typestring:
	    		vartypes[i] = NrmStringToQuark("string");
		    	break;

    		case Ncl_Typechar:
	    		vartypes[i] = NrmStringToQuark("character");
		    	break;

    		case Ncl_Typeobj:
	    		vartypes[i] = NrmStringToQuark("obj");
		    	break;

    		case Ncl_Typelogical:
	    		vartypes[i] = NrmStringToQuark("logical");
		    	break;

    		case Ncl_Typelist:
	    		vartypes[i] = NrmStringToQuark("list");
		    	break;
    		case Ncl_Typecompound:
	    		vartypes[i] = NrmStringToQuark("compound");
			break;
    		case Ncl_Typereference:
	    		vartypes[i] = NrmStringToQuark("reference");
		    	break;
    		case Ncl_Typegroup:
	    		vartypes[i] = NrmStringToQuark("group");
		    	break;

            default:
                has_missing = True;
                vartypes[i] = NrmStringToQuark("missing");
		fname = f->file.advanced_file_structure ? ((NclAdvancedFile) f)->advancedfile.fpath : f->file.fpath;

                NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "getfilevartypes: unable to determine type of variable (%s) in file (%s)",
                    NrmQuarkToString(varnames[i]), NrmQuarkToString(fname));

                break;
        }
    }

    if (has_missing) {
        missing.stringval = NrmStringToQuark("missing");
        return NclReturnValue((void *) vartypes, ndims, dimsz, &missing, NCL_string, 0);
    }
    else
        return NclReturnValue((void *) vartypes, ndims, dimsz, NULL, NCL_string, 0);

}

NhlErrorTypes  _NclIGetFileChunkSizes(void)
{
    /* file variables */
    NclFile f;
    int *fid;

    /* chunk dimensions */
    int nchunks, islong = 0;
    ng_size_t *chunk_sizes;
    NclBasicDataTypes return_type = NCL_int;

    NhlErrorTypes   ret;

    ng_size_t i = 0;

    void *retvalue = NULL;
    
    /* get file information */
    fid = (int *) NclGetArgValue(
                    0,
                    1,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);
    f = (NclFile) _NclGetObj((int) *fid);

    if (f != NULL)
    {
	chunk_sizes = _NclFileReadChunkSizes(f, &nchunks);

	if(NULL == chunk_sizes)
	{
            int *iptr = NULL;
            retvalue = (void *)NclMalloc(sizeof(int));
            iptr = (int *)retvalue;
            *iptr = -1;
            i = 1;
	    ret = NclReturnValue((void *)retvalue, 1, &i, NULL, return_type, 1);
	    free(chunk_sizes);
	    free(retvalue);
	    return ret;
	}

        for(i = 0; i < nchunks; i++)
            if(chunk_sizes[i] > INT_MAX)
                islong = 1;

	if(islong)
        {
            long *lptr = NULL;
            return_type = NCL_long;
            retvalue = (void *)NclMalloc(nchunks * sizeof(long));
            lptr = (long *)retvalue;
            for(i = 0; i < nchunks; i++)
                lptr[i] = (long) chunk_sizes[i];
        }
	else
        {
            int *iptr = NULL;
            return_type = NCL_int;
            retvalue = (void *)NclMalloc(nchunks * sizeof(int));
            iptr = (int *)retvalue;
            for(i = 0; i < nchunks; i++)
                iptr[i] = (int) chunk_sizes[i];
        }

	i = (ng_size_t) nchunks;
	ret = NclReturnValue((void *)retvalue, 1, &i, NULL, return_type, 1);
	free(chunk_sizes);
        free(retvalue);
	return ret;
    }

    NHLPERROR((NhlWARNING, NhlEUNKNOWN,
              "getfilechunksizes: undefined file variable or file has no chunking"));

    i = 1;
    NclReturnValue(
            (void*) &((NclTypeClass) nclTypeintClass)->type_class.default_mis, 1,
            &i, &((NclTypeClass) nclTypeintClass)->type_class.default_mis,
            ((NclTypeClass) nclTypeintClass)->type_class.data_type, 1);
	
    return NhlWARNING;
}

NhlErrorTypes _NclIGetFileCompressionLevel(void)
{
    /* file variables */
    NclFile f;
    int *fid;

    /* chunk dimensions */
    ng_size_t ns = 1;
    int compressionlevel = 0;
    NclBasicDataTypes return_type = NCL_int;

    NhlErrorTypes ret;
    
    /* get file information */
    fid = (int *) NclGetArgValue(
                    0,
                    1,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);
    f = (NclFile) _NclGetObj((int) *fid);

    if (f != NULL)
    {
	compressionlevel = _NclFileReadCompressionLevel(f);

	ret = NclReturnValue((void *)(&compressionlevel), 1, &ns, NULL, return_type, 1);
	return ret;
    }
    else
    {
        NHLPERROR((NhlWARNING, NhlEUNKNOWN,
              "getfilecompressionlevel: undefined file variable or file has no compression"));
    }

    NclReturnValue((void *)(&compressionlevel), 1, &ns, NULL, return_type, 1);
	
    return NhlWARNING;
}

NhlErrorTypes _NclIGetFileVersion(void)
{
    /* file variables */
    NclFile f;
    int *fid;

    /* chunk dimensions */
    ng_size_t ns = 1;
    NclQuark version = NrmStringToQuark("unknown");
    NclBasicDataTypes return_type = NCL_string;

    NhlErrorTypes ret;
    
    /* get file information */
    fid = (int *) NclGetArgValue(
                    0,
                    1,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);
    f = (NclFile) _NclGetObj((int) *fid);

    if (f != NULL)
    {
	version = _NclFileReadVersion(f);

	ret = NclReturnValue((void *)(&version), 1, &ns, NULL, return_type, 1);
	return ret;
    }

    NHLPERROR((NhlWARNING, NhlEUNKNOWN,
          "getfileversion: undefined file variable or file version unknown"));
    NclReturnValue((void *)(&version), 1, &ns, NULL, return_type, 1);
    return NhlWARNING;
}

NhlErrorTypes   _NclIGetFileDimsizes
# if	NhlNeedProto
(void)
# else
()
# endif /* NhlNeedProto */
{
    /* file variables */
    NclFile f;
    int *fid;

    /* dimensions */
    ng_size_t dimsizes = 1, ndims;
    void *dim_sizes;
    NclBasicDataTypes return_type;

    NhlErrorTypes   ret;

    ng_size_t i = 0;
    

    /* get file information */
    fid = (int *) NclGetArgValue(
                    0,
                    1,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);
    f = (NclFile) _NclGetObj((int) *fid);

    if (f != NULL)
    {
    if(f->file.advanced_file_structure)
    {
        NclAdvancedFile   theadvancedfile = (NclAdvancedFile) f;
        NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;

        if(NULL != grpnode)
        {
	    ndims = grpnode->dim_rec ? grpnode->dim_rec->n_dims : 0;

	    if (ndims == 0)
            {
		NhlPError(NhlWARNING, NhlEUNKNOWN, "getfiledimsizes: file contains no dimensions");
		dimsizes = 1;
		NclReturnValue(
			(void*) &((NclTypeClass) nclTypeintClass)->type_class.default_mis, 1,
			&dimsizes, &((NclTypeClass) nclTypeintClass)->type_class.default_mis,
			((NclTypeClass) nclTypeintClass)->type_class.data_type, 1);
		return NhlWARNING;
	    }
	    else
            {
	        return_type = NCL_int;
#if !defined(NG32BIT)
	        i = 0;
	        while(i < ndims)
                {
	            if(grpnode->dim_rec->dim_node[i].size > INT_MAX)
                    {
	                return_type = NCL_long;
                        break;
	            }
	            ++i;
	        }
#endif
	        if(return_type == NCL_int)
                {
	            dim_sizes = (void *) NclMalloc(sizeof(int) * ndims);
	            for(i = 0; i < ndims; ++i)
	                ((int*)dim_sizes)[i] = (int) grpnode->dim_rec->dim_node[i].size;
	        }
	        else
	        {
	            dim_sizes = (void *) NclMalloc(sizeof(long) * ndims);
	            for(i = 0; i < ndims; ++i)
	                ((long*)dim_sizes)[i] = (long) grpnode->dim_rec->dim_node[i].size;
	        }

	        ret = NclReturnValue(dim_sizes, 1, &ndims, NULL, return_type, 1);
	        free(dim_sizes);
	        return ret;
            }
        }
    }
    else
    {
/*
 * First loop through dimension sizes to see if we need to return
 * ints or longs.
 *
 * The rules for when to return an int versus a long:
 *    - On a 32-bit system, return ints.
 *    - On a 64-bit system, return longs if any of the
 *      individual dimension sizes are > INT_MAX.
 *      We used to also do so if the product of the dimension 
 *      was > INT_MAX, but this was removed before 6.0.0.
 */
	ndims = f->file.n_file_dims;
	if (ndims == 0) {
		NhlPError(NhlWARNING, NhlEUNKNOWN, "getfiledimsizes: file contains no dimensions");
		dimsizes = 1;
		NclReturnValue(
			(void*) &((NclTypeClass) nclTypeintClass)->type_class.default_mis, 1,
			&dimsizes, &((NclTypeClass) nclTypeintClass)->type_class.default_mis,
			((NclTypeClass) nclTypeintClass)->type_class.data_type, 1);
		return NhlWARNING;
	}
	else {
	  return_type = NCL_int;
#if !defined(NG32BIT)
	  i = 0;
	  while(i < ndims && (return_type == NCL_int)) {
	    if(f->file.file_dim_info[i]->dim_size > INT_MAX) {
	      return_type = NCL_long;
	    }
	    i++;
	  }
#endif
	  if(return_type == NCL_int) {
	    dim_sizes = (void *) NclMalloc(sizeof(int) * ndims);
	    for (i = 0; i < ndims; i++) {
	      ((int*)dim_sizes)[i] = (int)f->file.file_dim_info[i]->dim_size;
	    }
	  }
	  else {
	    dim_sizes = (void *) NclMalloc(sizeof(long) * ndims);
	    for (i = 0; i < ndims; i++) {
	      ((long*)dim_sizes)[i] = (long)f->file.file_dim_info[i]->dim_size;
	    }
	  }
	  ret = NclReturnValue(dim_sizes, 1, &ndims, NULL, return_type, 1);
	  free(dim_sizes);
	  return ret;
        }
    }
    }
    NhlPError(NhlWARNING, NhlEUNKNOWN, "getfiledimsizes: undefined file variable");

    dimsizes = 1;
    NclReturnValue(
            (void*) &((NclTypeClass) nclTypeintClass)->type_class.default_mis, 1,
            &dimsizes, &((NclTypeClass) nclTypeintClass)->type_class.default_mis,
            ((NclTypeClass) nclTypeintClass)->type_class.data_type, 1);
	
    return NhlWARNING;
}


NhlErrorTypes   _NclIVarIsUnlimited
# if    NhlNeedProto
(void)
# else
()
# endif /* NhlNeedProto */
{
    /* file variables */
    NclFile f;
    int *fid;

    /* dimension names, types */
    NclQuark  *dname;

    /* dimensions, sizes */
    ng_size_t dimsizes = 1;
    int ndims;
    ng_size_t dimsz[NCL_MAX_DIMENSIONS];

    logical isunlimited = 0;

    int i = 0;


    /* get file information (1st arg.) */
    fid = (int *) NclGetArgValue(
                    0,
                    2,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    NULL,
                    0);
    f = (NclFile) _NclGetObj((int) *fid);

    /* get dimension information (2nd arg.) */
    dname = (NclQuark *) NclGetArgValue(
                    1,
                    2,
                    &ndims,
                    dimsz,
                    NULL,
                    NULL,
                    NULL,
                    0);

    /* get dimension info and check for unlimited */
    if (f != NULL) {
        if (f->file.advanced_file_structure)
        {
            NclAdvancedFile theadvancedfile = (NclAdvancedFile) f;
            NclFileGrpNode *grpnode = theadvancedfile->advancedfile.grpnode;
            if (NULL != grpnode->dim_rec)
            {
                for (i = 0; i < grpnode->dim_rec->n_dims; ++i)
                {
                    if (grpnode->dim_rec->dim_node[i].name == *dname)
                    {
                        isunlimited = grpnode->dim_rec->dim_node[i].is_unlimited;
                        break;
                    }
                }
            }
        }
        else
        {
        if (f->file.n_file_dims > 0) {
            for (i = 0; i < f->file.n_file_dims; i++) {
                if (f->file.file_dim_info[i]->dim_name_quark == *dname) {
                    isunlimited = f->file.file_dim_info[i]->is_unlimited;
                    break;
                }
            }
        }
        }
    }
    else {
        NhlPError(NhlWARNING, NhlEUNKNOWN, "isunlimited: undefined file variable");
    }

    return NclReturnValue((void *) &isunlimited, 1, &dimsizes, NULL, NCL_logical, 1);
}


NhlErrorTypes   _NclIFileIsPresent
# if    NhlNeedProto
(void)
# else
()
# endif
{
    NclQuark *files;
    const char *fpath = NULL;
    struct stat st;

    int fid = -1;
    int ndims;
    ng_size_t dimsz[NCL_MAX_DIMENSIONS];
    int sz = 1;
    int retcode = 0;

    logical *filemanuable;        /* file manuable? */
    int i = 0;

    NclFile file = NULL;
    int rw_v = 1;
    int error_id;
    NhlErrorTypes err_level;

    files = (NclQuark *) NclGetArgValue(
                0, 
                1, 
                &ndims,
                dimsz,
                NULL,
                NULL,
                NULL,
                0);

    /* calculate total number of input files to check */
    for (i = 0; i < ndims; i++)
        sz *= dimsz[i];

    /* logical array to return */
    filemanuable = (logical *) NclCalloc(sz, (unsigned int) sizeof(logical));
    if (filemanuable == (logical *) NULL)
    {
        NhlPError(NhlFATAL, errno, "isfilepresent: memory allocation error");
        return NhlFATAL;
    }
    /*
     * suppress warning messages from NCL for the duration of this routine
     */
    error_id = NhlGetErrorObjectId();
    NhlVAGetValues(error_id,
		   NhlNerrLevel,&err_level,NULL);
    NhlVASetValues(error_id,
		   NhlNerrLevel,NhlFATAL,NULL);

    for(i = 0; i < sz; i++)
    {
	fpath = (char *) NrmQuarkToString(files[i]);
        if((0 == strncmp(fpath, "http://", 7)) || (0 == strncmp(fpath, "https://", 8)))
	{
            fid = ncopen(fpath, NC_NOWRITE);
            if(0 <= fid)
                filemanuable[i] = 1;     /* true */
        }
        else
	{
            retcode = stat(_NGResolvePath(fpath),&st);
	  /*
	   *fprintf(stderr, "File: %s, line: %d\n", __FILE__, __LINE__);
	   *fprintf(stderr, "File retcode: \t\t%d\n", retcode);
	   *fprintf(stderr, "File Size: \t\t%d bytes\n", st.st_size);
	   *fprintf(stderr, "Number of Links: \t%d\n", st.st_nlink);
	   *fprintf(stderr, "File inode: \t\t%d\n", st.st_ino);
	   */
            if(retcode)
            {
                char tmp_path[NCL_MAX_STRING];
                char *ext_name;
                strcpy(tmp_path, fpath);

                ext_name = strrchr(tmp_path, '.');
                if(NULL != ext_name)
		{
                    tmp_path[strlen(tmp_path) - strlen(ext_name)] = '\0'; 
                    if(! stat(_NGResolvePath(tmp_path),&st))
                    {
                        file = _NclOpenFile(NULL,NULL,Ncl_File,0,TEMPORARY,NrmStringToQuark(tmp_path),rw_v);
                        if(NULL != file)
                        {
                            filemanuable[i] = 1;     /* true */
                            _NclDestroyObj((NclObj)file);
                        }
                    }
                }
            }
            else
            {
	      /*
	       *fprintf(stderr, "File: %s, line: %d\n", __FILE__, __LINE__);
	       *fprintf(stderr, "File retcode: \t\t%d\n", retcode);
	       *fprintf(stderr, "File Size: \t\t%d bytes\n", st.st_size);
	       *fprintf(stderr, "Number of Links: \t%d\n", st.st_nlink);
	       *fprintf(stderr, "File inode: \t\t%d\n", st.st_ino);
	       *fprintf(stderr, "\nFile Permissions: \t");
	       *fprintf(stderr,  (S_ISDIR(st.st_mode)) ? "d" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IRUSR) ? "r" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IWUSR) ? "w" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IXUSR) ? "x" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IRGRP) ? "r" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IWGRP) ? "w" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IXGRP) ? "x" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IROTH) ? "r" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IWOTH) ? "w" : "-");
	       *fprintf(stderr,  (st.st_mode & S_IXOTH) ? "x" : "-");
	       *fprintf(stderr, "\n");
	       */

		if((! S_ISDIR(st.st_mode)) && st.st_size)
	        {
	            file = _NclOpenFile(NULL,NULL,Ncl_File,0,TEMPORARY,files[i],rw_v);
	            if(NULL != file)
                    {
                        filemanuable[i] = 1;     /* true */
                        _NclDestroyObj((NclObj)file);
                    }
                }
            }
        }
    }
    /* restore previous error level setting */
    NhlVASetValues(error_id,
		   NhlNerrLevel,err_level,NULL);

    return NclReturnValue((void *) filemanuable, ndims, dimsz, NULL, NCL_logical, 0);
}

NhlErrorTypes  _NclIFileExists(void)
{
    NclQuark  *files;
    const char  *fpath = NULL;
    struct stat st;

    int ndims;
    ng_size_t dimsz[NCL_MAX_DIMENSIONS];
    int sz = 1;

    logical *file_exists;        /* file exists? */
    int i = 0;

    files = (NclQuark *) NclGetArgValue(
                0, 
                1, 
                &ndims,
                dimsz,
                NULL,
                NULL,
                NULL,
                0);

    /* calculate total number of input files to check */
    for (i = 0; i < ndims; i++)
        sz *= dimsz[i];

    /* logical array to return */
    file_exists = (logical *) NclMalloc((unsigned int) sizeof(logical) * sz);
    if(NULL == file_exists)
    {
        NHLPERROR((NhlFATAL, errno, "memory allocation error"));
        return NhlFATAL;
    }

    for (i = 0; i < sz; i++)
    {
        file_exists[i] = 1;

        fpath = _NGResolvePath(NrmQuarkToString(files[i]));
        if(stat(fpath, &st))
            file_exists[i] = 0;
    }

    return NclReturnValue((void *) file_exists, ndims, dimsz, NULL, NCL_logical, 0);
}


NhlErrorTypes _NclItoint
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
	int j;
        ng_size_t i;
        int *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

        output = (int*)NclMalloc(sizeof(int)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "toint: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) INT_MIN;
                    dmax = (double) INT_MAX;
                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = (double) missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.intval = (int) val;
                        }
                        else
                        {
                            NHLPERROR((NhlINFO, NhlEUNKNOWN,
                                      "toint: the double missing value %g is out of integer range.\n", missing.doubleval));
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d double(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d double(s) less than INT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) INT_MIN;
                    fmax = (float) INT_MAX;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.intval = (int) val;
                        }
                        else
                        {
                            NHLPERROR((NhlINFO, NhlEUNKNOWN,
                                      "toint: The float missing value %f is out of integer range.\n", missing.floatval));
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val >= fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }
                  /*
                    fprintf(stderr, "file: %s, line: %d\n", __FILE__, __LINE__);
                    fprintf(stderr, "\tfmin = %f\n", fmin);
                    fprintf(stderr, "\tfmax = %f\n", fmax);
                    fprintf(stderr, "\tptr[0] = %f\n", ptr[0]);
                    fprintf(stderr, "\toutput[0] = %d\n", output[0]);
                    fprintf(stderr, "\tINT_MIN = %d\n", INT_MIN);
                    fprintf(stderr, "\tINT_MAX = %d\n", INT_MAX);
                   */
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d float(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d float(s) less than INT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            ret_missing.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;
                        }
                        else
                        { 
                            if((llval <= INT_MAX) && (llval >= INT_MIN))
                                ret_missing.intval = (int) llval;
                            else
                            {
                                NHLPERROR((NhlINFO, NhlEUNKNOWN,
                                          "toint: The string missing value %s is out of integer range.\n",
                                           NrmQuarkToString(missing.stringval)));
                            }
                        }
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            has_missing = 1;
                            NhlPError(NhlWARNING,NhlEUNKNOWN,
                                "toint: A bad value was passed (string); input strings must contain numeric digits, replacing with missing value");
                            output[i] = ret_missing.intval;
                        }
                        else if (llval > INT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else if (llval < INT_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) llval;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d string(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d string(s) less than INT_MIN, which have been forced to INT_MIN.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char val;
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (int) val;
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (int) ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (int) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.shortval;
                    }
    
                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (int) val;
                    }
                }
                break;
            case NCL_ushort:
                {
                    int ival;
                    unsigned short *ptr;
    
                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        ival = (int) ptr[i];
                        output[i] = ival;
                    }
                }
                break;
            case NCL_int:
                {
                    int *ptr;

                    ptr = (int *) in_value;
                    ret_missing = missing;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int val;
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.uintval < INT_MAX)
                            ret_missing.intval = (int) missing.uintval;
                        else
                        {
                            NHLPERROR((NhlINFO, NhlEUNKNOWN,
                                      "toint: The uint missing value %u is out of integer range.\n", missing.intval));
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > INT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d unsigned int(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    if(has_missing)
                    {
                        if((missing.longval <= INT_MAX) &&(missing.longval >= INT_MIN))
                            ret_missing.intval = (int) missing.longval;
                        else
                        {
                            NHLPERROR((NhlWARNING, NhlEUNKNOWN,
                                      "toint: the long missing value %ld is out of integer range.\n", missing.longval));
                        }
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > INT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else if(val < INT_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d long(s) larger than INT_MAX, which have been flagged missing.",
                            underflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d long(s) less than INT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ulongval <= INT_MAX)
                            ret_missing.intval = (int) missing.ulongval;
                        else
                        {
                            NHLPERROR((NhlWARNING, NhlEUNKNOWN,
                                      "toint: the ulong missing value %uld is out of integer range.\n", missing.ulongval));
                        }
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > INT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }
   
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d long(s) larger than INT_MAX, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    if(has_missing)
                    {
                        if((missing.int64val <= INT_MAX) && (missing.int64val >= INT_MIN))
                            ret_missing.intval = (int) missing.int64val;
                        else
                        {
                            NHLPERROR((NhlWARNING, NhlEUNKNOWN,
                                      "toint: the int64 missing value %lld is out of integer range.\n", missing.int64val));
                        }
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > INT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else if(val < INT_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d int64(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d int64(s) less than INT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    if(has_missing)
                    {
                        if(missing.uint64val <= INT_MAX)
                            ret_missing.intval = (int) missing.uint64val;
                        else
                        {
                            NHLPERROR((NhlWARNING, NhlEUNKNOWN,
                                      "toint: the uint64 missing value %ulld is out of integer range.\n", missing.uint64val));
                        }
                    }

                    ptr = (unsigned long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > INT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.intval;
                        }
                        else
                        {
                            output[i] = (int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint: there are %d uint64(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "toint: don't know how to convert logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "toint: don't know how to convert object to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "toint: don't know how to convert list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "toint: don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "toint: don't know how to convert unknown type to integer.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_int,
                0
        ));
}


NhlErrorTypes _NclItouint
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        unsigned int *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.uintval = (unsigned int) ((NclTypeClass) nclTypeuintClass)->type_class.default_mis.uintval;

        output = (unsigned int*)NclMalloc(sizeof(unsigned int)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "touint: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = 0.0;
                    dmax = (double) UINT_MAX;

                    if(has_missing)
                    {
                        if((missing.doubleval <= dmax) && (missing.doubleval >= dmin))
                            ret_missing.uintval = (unsigned int) missing.doubleval;
                    }

                    ptr = (double *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d double(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d double(s) less than INT_MIN, which have flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = 0.0;
                    fmax = (float) UINT_MAX;

                    if(has_missing)
                    {
                        if((missing.floatval <= fmax) && (missing.floatval >= fmin))
                            ret_missing.uintval = (unsigned int) missing.floatval;
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d float(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d float(s) less than INT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.intval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.intval;
                        }
                        else
                        {
                            if((llval <= UINT_MAX) && (llval >= 0))
                                ret_missing.intval = (int) llval;
                        }
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "touint: a bad value was passed to touint, input strings must contain numeric digits, replacing with missing value");
                            output[i] = ret_missing.uintval;
                        }
                        else if (errno == ERANGE)
                        {
                            has_missing = 1;
                            output[i] = ret_missing.uintval;
                        }
                        else if (llval > UINT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else if (llval < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) llval;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d double(s) larger than UINT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.uintval = (unsigned int) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned int) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte val;
                    byte *ptr;

                    if(has_missing)
                    {
                        ret_missing.uintval = (unsigned int) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            if(has_missing && (val != missing.byteval))
                            {
                               has_missing = 1;
                               underflowed ++;
                            }
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d char(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                            ret_missing.uintval = (unsigned int) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned int) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    if(has_missing)
                    {
                        if(missing.shortval >= 0)
                            ret_missing.uintval = (unsigned int) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned int uival;
                    unsigned short *ptr;
    
                    if(has_missing)
                    {
                        ret_missing.uintval = (unsigned int) missing.ushortval;
                    }
    
                    ptr = (unsigned short *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        uival = (unsigned int) ptr[i];
                        output[i] = uival;
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        if(missing.intval > 0)
                            ret_missing.uintval = (unsigned int) missing.intval;

                        for(i = 0; i < total_elements; i++)
                        {
                            val = ptr[i];
                            if(val < 0)
                            {
                                if(val != missing.intval)
                                {
                                    has_missing = 1;
                                    underflowed ++;
                                }
                                output[i] = ret_missing.uintval;
                            }
                            else
                            {
                                output[i] = (unsigned int) val;
                            }
                        }
                    }
                    else
                    {
                        for(i = 0; i < total_elements; i++)
                        {
                            val = ptr[i];
                            if(val < 0)
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.uintval;
                            }
                            else
                            {
                                output[i] = (unsigned int) val;
                            }
                        }
                    }
   
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.uintval = (unsigned int) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        if(missing.longval >= 0)
                            ret_missing.uintval = (unsigned int) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UINT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d long(s) larger than UINT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d long(s) less than 0, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        if(missing.ulongval <= UINT_MAX)
                            ret_missing.uintval = (unsigned int) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UINT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d unsigned long(s) larger than UINT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.int64val <= UINT_MAX)
                            ret_missing.uintval = (unsigned int) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UINT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d long(s) larger than UINT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d long(s) less than 0, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.uint64val <= UINT_MAX)
                            ret_missing.uintval = (int) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i]; 
                        if(val > UINT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uintval;
                        }
                        else
                        {
                            output[i] = (unsigned int) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint: there are %d uint64(s) larger than UINT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "touint: don't know how to convert logical to uint.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "touint: don't know how to convert object to uint.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "touint: don't know how to convert list to uint.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "touint: don't know how to convert NCL_none to uint.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "touint: don't know how to convert unknown type to uint.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_uint,
                0
        ));
}


NhlErrorTypes _NclItolong
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing = 0;
        int j;
	ng_size_t i;
        long *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.longval = (long) ((NclTypeClass) nclTypelongClass)->type_class.default_mis.longval;

        output = (long *)NclMalloc(sizeof(long)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "tolong: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) LONG_MIN;
                    dmax = (double) LONG_MAX;
                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = (double) missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.longval = (long) val;
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            output[i] = (long) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d double(s) larger than LONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d double(s) less than LONG_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) LONG_MIN;
                    fmax = (float) LONG_MAX;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.longval = (long) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            output[i] = (long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d float(s) larger than INT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d float(s) less than INT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.longval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.longval;
                        }
                        else
                        {
                            if((llval < LONG_MAX) && (llval > LONG_MIN))
                                ret_missing.longval = (long) llval;
                        }
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(has_missing && (missing.stringval == ptr[i]))
                        {
                            has_missing = 1;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            llval = _Nclstrtoll(str,&end);
                            if (strcmp(end, str) == 0 || errno == ERANGE)
                            {
                                has_missing = 1;
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "tolong: a bad value was passed to (string) tolong, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.longval;
                            }
                            else if (llval > LONG_MAX)
                            {
                                has_missing = 1;
                                overflowed ++;
                                output[i] = ret_missing.longval;
                            }
                            else if (llval < LONG_MIN)
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.longval;
                            }
                            else
                            {
                                output[i] = (long) llval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d double(s) larger than LONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d double(s) less than LONG_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long) ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (long) val;
                    }
                }
                break;
            case NCL_ushort:
                {
                    long val;
                    unsigned short *ptr;
    
                    ptr = (unsigned short *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.ushortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (long) ptr[i];
                        output[i] = val;
                    }
                }
                break;
            case NCL_int:
                {
                    long val;
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (long) ptr[i];
                        output[i] = val;
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int val, vmax;
                    unsigned int *ptr;

                    vmax = (unsigned int) LONG_MAX;

                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.uintval;
                    }

                    ptr = (unsigned int *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > vmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            output[i] = (long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d unsigned int(s) larger than LONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long *ptr;

                    if(has_missing)
                    {
                        ret_missing.longval = missing.longval;
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ulongval <= LONG_MAX)
                            ret_missing.longval = (long) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > LONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            output[i] = (long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d unsigned long(s) larger than LONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    if(has_missing)
                    {
                        if((missing.int64val <= LONG_MAX) && (missing.int64val >= LONG_MIN))
                            ret_missing.longval = (long) missing.int64val;
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > LONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else if(val < LONG_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            output[i] = (long) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d int64(s) larger than LONG_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d int64(s) less than LONG_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    if(has_missing)
                    {
                        if(missing.uint64val <= LONG_MAX)
                            ret_missing.longval = (long) missing.uint64val;
                    }

                    ptr = (unsigned long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > LONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.longval;
                        }
                        else
                        {
                            output[i] = (long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tolong: there are %d uint64(s) larger than LONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "tolong: don't know how to convert logical to long.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "tolong: don't know how to convert object to long.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "tolong: don't know how to convert list to long.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "tolong: don't know how to convert NCL_none to long.");
                return NhlFATAL;
                break;
            default:
                NHLPERROR((NhlFATAL, errno, "tolong: don't know how to convert unknown type to long."));
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_long,
                0
        ));
}


NhlErrorTypes _NclItoulong
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        unsigned long *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.ulongval = (unsigned long) ((NclTypeClass) nclTypeulongClass)->type_class.default_mis.ulongval;

        output = (unsigned long *)NclMalloc(sizeof(unsigned long)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "toulong: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) 0;
                    dmax = (double) ULONG_MAX;
                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.intval = (unsigned long) val;
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d double(s) larger than ULONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) 0;
                    fmax = (float) ULONG_MAX;
                    ptr = (float *) in_value;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.ulongval = (unsigned long) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d float(s) larger than ULONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d float(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    ptr = (NclQuark *) in_value;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "toulong: a bad value was passed to (string) toulong, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            if((llval <= ULONG_MAX) && (llval >= 0))
                                ret_missing.ulongval = (unsigned long) llval;
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "toulong: a bad value was passed to (string) toulong, input strings must contain numeric digits, replacing with missing value");
                            output[i] = ret_missing.ulongval;
                        }
                        else if (errno == ERANGE)
                        {
                            has_missing = 1;
                            output[i] = ret_missing.ulongval;
                        }
                        else if (llval > ULONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else if (llval < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) llval;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d double(s) larger than ULONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte val;
                    byte *ptr;
   
                    ptr = (byte *) in_value;
   
                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            if(has_missing && (val != missing.byteval))
                            {
                                has_missing = 1;
                                underflowed ++;
                            }
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d char(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    unsigned long ulval;
                    short sval;
                    short *ptr;
    
                    ptr = (short *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.shortval >= 0)
                            ret_missing.ulongval = (unsigned long) missing.shortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sval = ptr[i];
                        if(sval < 0)
                        {
                           has_missing = 1;
                           underflowed ++;
                           output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            ulval = (unsigned long) sval;
                            output[i] = ulval;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d short(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned long  ulval;
                    unsigned short *ptr;
    
                    ptr = (unsigned short *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.ushortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        ulval = (unsigned long) ptr[i];
                        output[i] = ulval;
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        if(missing.intval >= 0)
                            ret_missing.ulongval = (unsigned long) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            if(has_missing && (val != missing.intval))
                            {
                                has_missing = 1;
                                underflowed ++;
                            }
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }
   
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;

                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long) ptr[i];
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        if(missing.longval >= 0)
                            ret_missing.ulongval = (unsigned long) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++; 
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }
  
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        if((missing.int64val <= ULONG_MAX) && (missing.int64val >= 0))
                            ret_missing.ulongval = (unsigned long) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > ULONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d int64(s) larger than ULONG_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d int64(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.uint64val <= ULONG_MAX)
                            ret_missing.ulongval = (unsigned long) missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > ULONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ulongval;
                        }
                        else
                        {
                            output[i] = (unsigned long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toulong: there are %d uint64(s) larger than ULONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "toulong: don't know how to convert logical to ulong.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "toulong: don't know how to convert object to ulong.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "toulong: don't know how to convert list to ulong.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "toulong: don't know how to convert NCL_none to ulong.");
                return NhlFATAL;
                break;
            default:
                NHLPERROR((NhlFATAL, errno, "toulong: don't know how to convert unknown type to ulong."));
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_ulong,
                0
        ));
}


NhlErrorTypes _NclItoint64
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        long long *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.int64val = (long long) ((NclTypeClass) nclTypeint64Class)->type_class.default_mis.int64val;

        output = (long long *)NclMalloc(sizeof(long long)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "toint64: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) LLONG_MIN;
                    dmax = (double) LLONG_MAX;
                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = (double) missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.int64val = (long long) val;
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else
                        {
                            output[i] = (long long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d double(s) larger than LLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d double(s) less than LLONG_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) LLONG_MIN;
                    fmax = (float) LLONG_MAX;
                    ptr = (float *) in_value;
    
                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.int64val = (long long) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else
                        {
                            output[i] = (long long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d float(s) larger than LLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d float(s) less than LLONG_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "toint64: a bad value was passed to (string) toint64, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            ret_missing.int64val = (long long) llval;
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "toint64: a bad value was passed to (string) toint64, input strings must contain numeric digits, replacing with missing value");
                            output[i] = ret_missing.int64val;
                        }
                        else if (errno == ERANGE)
                        {
                            has_missing = 1;
                            output[i] = ret_missing.int64val;
                        }
                        else if (llval > LLONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else if (llval < LLONG_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else
                        {
                            output[i] = (long long) llval;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d string(s) larger than LLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d string(s) less than LLONG_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    long long llval;
                    short *ptr;

                    ptr = (short *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.shortval;
                    }
    
                    for(i = 0; i < total_elements; i++)
                    {
                        llval = (long long) ptr[i];
                        output[i] = llval;
                    }
                }
                break;
            case NCL_ushort:
                {
                    long long llval;
                    unsigned short *ptr;
    
                    ptr = (unsigned short *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.ushortval;
                    }
   
                    for(i = 0; i < total_elements; i++)
                    {
                        llval = (long long) ptr[i];
                        output[i] = llval;
                    }
                }
                break;
            case NCL_int:
                {
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_long:
                {
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;


                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (long long) ptr[i];
                    }
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.int64val = missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.uint64val <= LLONG_MAX)
                            ret_missing.int64val = (long long) missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > LLONG_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.int64val;
                        }
                        else
                        {
                            output[i] = (long long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toint64: there are %d uint64(s) larger than LLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "toint64: don't know how to convert logical to int64.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "toint64: don't know how to convert object to int64.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "toint64: don't know how to convert list to int64.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "toint64: don't know how to convert NCL_none to int64.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "toint64: don't know how to convert unknown type to int64.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_int64,
                0
        ));
}


NhlErrorTypes _NclItouint64
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        unsigned long long *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.uint64val = (unsigned long long) ((NclTypeClass) nclTypeuint64Class)->type_class.default_mis.uint64val;

        output = (unsigned long long *)NclMalloc(sizeof(unsigned long long)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "touint64: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = 0.0;
                    dmax = (double) ULLONG_MAX;
                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.uint64val = (unsigned long long) val;
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            output[i] = (unsigned long long) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d double(s) larger than ULLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = 0.0;
                    fmax = (float) ULLONG_MAX;
                    ptr = (float *) in_value;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.uint64val = (unsigned long long) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            output[i] = (unsigned long long) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d float(s) larger than ULLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d float(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "touint64: a bad value was passed to (string) touint64, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            if(llval >= 0)
                                ret_missing.uint64val = (unsigned long long) llval;
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "touint64: a bad value was passed to (string) touint64, input strings must contain numeric digits, replacing with missing value");
                            output[i] = ret_missing.uint64val;
                        }
                        else if (errno == ERANGE)
                        {
                            has_missing = 1;
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            if(llval < 0)
                            {
                                has_missing = 1;
                                output[i] = ret_missing.uint64val;
                                underflowed++;
                            }
                            else
                            {
                                output[i] = (unsigned long long) llval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d double(s) larger than ULLONG_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    ptr = (unsigned char *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.charval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long long) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte val;
                    byte *ptr;

                    if(has_missing)
                    {
                        if(missing.byteval >= 0)
                            ret_missing.uint64val = (unsigned long long) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            if(has_missing && (val != missing.byteval))
                            {
                                has_missing = 1;
                                underflowed ++;
                            }
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            output[i] = (unsigned long long) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d char(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long long) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    ptr = (short *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.shortval >= 0)
                            ret_missing.uint64val = (unsigned long long) missing.shortval;
                    }
   
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            underflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            output[i] = (unsigned long long) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned long long val;
                    unsigned short *ptr;
   
                    ptr = (unsigned short *) in_value;
   
                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.ushortval;
                    }
  
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (unsigned long long) ptr[i];
                        output[i] = val;
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        if(missing.intval >= 0)
                            ret_missing.uint64val = (unsigned long long) missing.intval;

                        for(i = 0; i < total_elements; i++)
                        {
                            val = ptr[i];
                            if(val < 0)
                            {
                                if(val != missing.intval)
                                {
                                    underflowed ++;
                                }
                                output[i] = ret_missing.ulongval;
                            }
                            else
                            {
                                if(missing.intval == val)
                                {
                                    output[i] = ret_missing.uint64val;
                                }
                                else
                                {
                                    output[i] = (unsigned long long) val;
                                }
                            }
                        }
                    }
                    else
                    {
                        for(i = 0; i < total_elements; i++)
                        {
                            val = ptr[i];
                            if(val < 0)
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.ulongval;
                            }
                            else
                            {
                                output[i] = (unsigned long long) val;
                            }
                        }
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long long) ptr[i];
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        if(missing.longval >= 0)
                            ret_missing.uint64val = (unsigned long long) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            output[i] = (unsigned long long) val;
                        }
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d long(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long long) ptr[i];
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.int64val >= 0)
                            ret_missing.uint64val = (unsigned long long) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.uint64val;
                        }
                        else
                        {
                            output[i] = (unsigned long long) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "touint64: there are %d int64(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.uint64val = missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long long) ptr[i];
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "touint64: don't know how to convert logical to uint64.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "touint64: don't know how to convert object to uint64.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "touint64: don't know how to convert list to uint64.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "touint64: don't know how to convert NCL_none to uint64.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "touint64: don't know how to convert unknown type to uint64.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_uint64,
                0
        ));
}


NhlErrorTypes _NclItoubyte
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        unsigned char *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.ubyteval = (unsigned char) ((NclTypeClass) nclTypeubyteClass)->type_class.default_mis.ubyteval;

        output = (unsigned char *)NclMalloc(sizeof(unsigned char)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "toubyte: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = 0.0;
                    dmax = (double) UCHAR_MAX;
                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.ubyteval = (unsigned char) val;
                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d double(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = 0.0;
                    fmax = (float) UCHAR_MAX;
                    ptr = (float *) in_value;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.ubyteval = (unsigned char) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d float(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d float(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "toubyte: a bad value was passed to (string) toubyte, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            if((llval >= 0) && (llval <= UCHAR_MAX))
                                ret_missing.ubyteval = (unsigned char) llval;
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "toubyte: a bad value was passed to (string) toubyte, input strings must contain numeric digits, replacing with missing value");
                            output[i] = ret_missing.ubyteval;
                        }
                        else if (errno == ERANGE)
                        {
                            has_missing = 1;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            if(llval < 0)
                            {
                                has_missing = 1;
                                output[i] = ret_missing.ubyteval;
                                underflowed++;
                            }
                            else if(llval > UCHAR_MAX)
                            {
                                has_missing = 1;
                                output[i] = ret_missing.ubyteval;
                                overflowed++;
                            }
                            else
                            {
                                output[i] = (unsigned char) llval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d double(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    byte val;
                    byte *ptr;

                    ptr = (byte *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.byteval >= 0)
                            ret_missing.ubyteval = (unsigned char) missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            if(has_missing && (val != missing.byteval))
                            {
                               has_missing = 1;
                               underflowed ++;
                            }
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                            output[i] = (char) ptr[i];
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d byte(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.ubyteval = (unsigned char) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.ubyteval = (unsigned char) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned char) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    ptr = (short *) in_value;
    
                    if(has_missing)
                    {
                        if((missing.shortval >= 0) && (missing.shortval <= UCHAR_MAX))
                            ret_missing.ubyteval = (unsigned char) missing.shortval;
                    }
   
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d int(s)(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d short(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short val;
                    unsigned short *ptr;
   
                    ptr = (unsigned short *) in_value;
   
                    if(has_missing)
                    {
                        if(missing.ushortval <= UCHAR_MAX) 
                            ret_missing.ubyteval = (char) missing.ushortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (unsigned short) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d ushort(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        if((missing.intval <= UCHAR_MAX) && (missing.intval >= 0))
                            ret_missing.ubyteval = (char) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (int) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else if(val < 0)
                        {
                            if(has_missing && (val != missing.intval))
                            {
                                has_missing = 1;
                                underflowed ++;
                            }
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d int(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;
                    unsigned int val;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.uintval <= UCHAR_MAX)
                            ret_missing.ubyteval = (char) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (unsigned int) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d uint(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        if((missing.longval <= UCHAR_MAX) && (missing.longval >= 0))
                            ret_missing.ubyteval = (char) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (long) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d long(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d long(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
                    unsigned long val;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        if(missing.ulongval <= UCHAR_MAX)
                            ret_missing.ubyteval = (char) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (unsigned long) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d ulong(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;
                    long long val;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        if((missing.int64val <= UCHAR_MAX) && (missing.int64val >= 0))
                            ret_missing.ubyteval = (char) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (long long) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d int64(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d int64(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;
                    unsigned long long val;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.uint64val <= UCHAR_MAX)
                            ret_missing.ubyteval = (char) missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (unsigned long long) ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ubyteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toubyte: there are %d uint64(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "toubyte: Don't know how to convert logical to ubyte.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "toubyte: don't know how to convert object to ubyte.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "toubyte: don't know how to convert list to ubyte.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "toubyte: don't know how to convert NCL_none to ubyte.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "toubyte: don't know how to convert unknown type to ubyte.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_ubyte,
                0
        ));
}



NhlErrorTypes _NclItoshort
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing = 0;
        int j;
	ng_size_t i;
        short *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.shortval = (short) ((NclTypeClass) nclTypeshortClass)->type_class.default_mis.shortval;

        output = (short *)NclMalloc(sizeof(short)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "toshort: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) SHRT_MIN;
                    dmax = (double) SHRT_MAX;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.shortval = (short) val;
                        }
                    }

                    ptr = (double *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (double) ptr[i];
                        if(val > dmax)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else if(val < dmin)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d double(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d double(s) less than SHRT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) SHRT_MIN;
                    fmax = (float) SHRT_MAX;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.shortval = (short) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            if(has_missing && (val != missing.floatval))
                                overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else if(val < fmin)
                        {
                            if(has_missing && (val != missing.floatval))
                                underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d float(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d float(s) less than SHRT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.shortval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.shortval;
                        }
                        else
                        {
                            if((llval <= SHRT_MAX) && (llval >= SHRT_MIN))
                                ret_missing.shortval = (short) llval;
                        }
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(has_missing && (missing.stringval == ptr[i]))
                        {
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            llval = _Nclstrtoll(str,&end);
                            if (end == str || errno == ERANGE)
                            {
                                has_missing = 1;
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "toshort: a bad value was passed to toshort, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.shortval;
                            }
                            else if (llval > SHRT_MAX)
                            {
                                has_missing = 1;
                                overflowed ++;
                                output[i] = ret_missing.shortval;
                            }
                            else if (llval < SHRT_MIN)
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.shortval;
                            }
                            else
                            {
                                output[i] = (short) llval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d double(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d double(s) less than SHRT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.shortval = (short) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (short) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    if(has_missing)
                    {
                        ret_missing.shortval = (short) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (short) ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    ptr = (unsigned char *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.shortval = (short) missing.ubyteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (short) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short *ptr;
    
                    if(has_missing)
                    {
                        ret_missing.shortval = missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (short) ptr[i];
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short val;
                    unsigned short *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ushortval < SHRT_MAX)
                            ret_missing.shortval = (short) missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
  
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d int(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    if(has_missing)
                    {
                        if((missing.intval <= SHRT_MAX) && (missing.intval >= SHRT_MIN))
                            ret_missing.shortval = (short) missing.intval;
                    }

                    ptr = (int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else if(val < SHRT_MIN)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
   
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d int(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d int(s) less than SHRT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int val;
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.uintval <= SHRT_MAX)
                            ret_missing.shortval = (short) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d unsigned int(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    if(has_missing)
                    {
                        if((missing.longval <= SHRT_MAX) && (missing.longval >= SHRT_MIN))
                            ret_missing.shortval = (short) missing.longval;
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else if(val < SHRT_MIN)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d long(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d long(s) less than SHRT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ulongval <= SHRT_MAX)
                            ret_missing.shortval = (short) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d unsigned long(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    if(has_missing)
                    {
                        if((missing.int64val <= SHRT_MAX) && (missing.int64val >= SHRT_MIN))
                            ret_missing.shortval = (short) missing.int64val;
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else if(val < SHRT_MIN)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d int64(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d int64(s) less than SHRT_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    if(has_missing)
                    {
                        if(missing.uint64val <= SHRT_MAX)
                            ret_missing.shortval = (short) missing.uint64val;
                    }

                    ptr = (unsigned long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            output[i] = (short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toshort: there are %d uint64(s) larger than SHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "toshort: don't know how to convert logical to short.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "toshort: don't know how to convert object to short.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "toshort: don't know how to convert list to short.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "toshort: don't know how to convert NCL_none to short.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "toshort: don't know how to convert unknown type to short.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_short,
                0
        ));
}


NhlErrorTypes _NclItoushort
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        unsigned short *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.ushortval = (unsigned short) ((NclTypeClass) nclTypeushortClass)->type_class.default_mis.ushortval;

        output = (unsigned short *)NclMalloc(sizeof(unsigned short)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "toushort: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = 0.0;
                    dmax = (double) USHRT_MAX;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                        {
                            ret_missing.ushortval = (unsigned short) val;
                        }
                    }

                    ptr = (double *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (double) ptr[i];
                        if(val > dmax)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else if(val < dmin)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d double(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = 0.0;
                    fmax = (float) USHRT_MAX;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                        {
                            ret_missing.shortval = (unsigned short) val;
                        }
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else if(val < fmin)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d float(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d float(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.intval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.intval;
                        }
                        else
                        {
                            if((llval <= USHRT_MAX) && (llval >= 0))
                                ret_missing.shortval = (int) llval;
                        }
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(has_missing && (missing.stringval == ptr[i]))
                        {
                            output[i] = ret_missing.ushortval;
                            has_missing = 1;
                        }
                        else
                        {
                            llval = _Nclstrtoll(str,&end);
                            if (strcmp(end, str) == 0)
                            {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                    "toushort: A bad value was passed to toushort, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.ushortval;
                            }
                            else if (llval > USHRT_MAX)
                            {
                                overflowed ++;
                                has_missing = 1;
                                output[i] = ret_missing.ushortval;
                            }
                            else if (llval < 0)
                            {
                                underflowed ++;
                                has_missing = 1;
                                output[i] = ret_missing.ushortval;
                            }
                            else
                            {
                                output[i] = (unsigned short) llval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d double(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.ushortval = (unsigned short) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned short) ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte val;
                    byte *ptr;

                    if(has_missing)
                    {
                        ret_missing.ushortval = (unsigned short) missing.byteval;
                    }

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d char(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                            ret_missing.ushortval = (unsigned short) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned short) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    if(has_missing)
                    {
                        if(missing.shortval >= 0)
                            ret_missing.ushortval = (unsigned short) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }

                    if(underflowed)
                    {
                        NHLPERROR((NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d short(s) less than 0, which have been flagged missing.",
                            underflowed));
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short val;
                    unsigned short *ptr;
    
                    if(has_missing)
                    {
                        ret_missing.ushortval = missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = val;
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    if(has_missing)
                    {
                        if((missing.intval <= USHRT_MAX) && (missing.intval >= 0))
                            ret_missing.ushortval = (unsigned short) missing.intval;
                    }

                    ptr = (int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > USHRT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ushortval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
   
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d int(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int val;
                    unsigned int *ptr;
    
                    if(has_missing)
                    {
                        if(missing.uintval <= USHRT_MAX)
                            ret_missing.ushortval = (unsigned short) missing.uintval;
                    }

                    ptr = (unsigned int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > USHRT_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d unsigned int(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    if(has_missing)
                    {
                        if((missing.longval <= USHRT_MAX) && (missing.longval >= 0))
                            ret_missing.ushortval = (unsigned short) missing.longval;
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > USHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else if(val < 0)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d long(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d long(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ulongval <= USHRT_MAX)
                            ret_missing.ushortval = (unsigned short) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > USHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d unsigned long(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    if(has_missing)
                    {
                        if((missing.int64val <= USHRT_MAX) && (missing.int64val >= 0))
                            ret_missing.ushortval = (unsigned short) missing.int64val;
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > USHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else if(val < 0)
                        {
                            underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d int64(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d int64(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        if(missing.uint64val <= USHRT_MAX)
                            ret_missing.ushortval = (unsigned short) missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > USHRT_MAX)
                        {
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.ushortval;
                        }
                        else
                        {
                            output[i] = (unsigned short) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "toushort: there are %d uint64(s) larger than USHRT_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "toushort: don't know how to convert logical to ushort.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "toushort: don't know how to convert object to ushort.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "toushort: don't know how to convert list to ushort.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "toushort: don't know how to convert NCL_none to ushort.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "toushort: don't know how to convert unknown type to ushort.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_ushort,
                0
        ));
}


NhlErrorTypes _NclItofloat
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        float *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.floatval = (float) ((NclTypeClass) nclTypefloatClass)->type_class.default_mis.floatval;

        output = (float *)NclMalloc(sizeof(float)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "tofloat: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax, dtiny;
                    double *ptr;

                    dmin = (double)(-FLT_MAX);
                    dmax = (double) FLT_MAX;
                    dtiny = (double) FLT_MIN;

                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if(val <= dmax && val >= dmin)
                        {
                            if(fabs(val) < dtiny)
                                ret_missing.floatval = 0.0;
                            else
                                ret_missing.floatval = (float) val;

                        }
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.floatval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.floatval;
                        }
                        else
                        {
                            if(fabs(val) < dtiny)
                                output[i] = 0.0;
                            else
                                output[i] = (float) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tofloat: there are %d double(s) larger than FLT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tofloat: there are %d double(s) less than (-FLT_MAX), which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float *ptr;

                    ptr = (float *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.floatval = missing.floatval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_string:
                {
                    double dval, dtest;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        dval = strtod(str,&end);
                        dtest = fabs(dval);
                        if (end == str || errno == ERANGE || dtest > (double) FLT_MAX)
                        {
                            ret_missing.floatval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.floatval;
                        }
                        else if (dtest < (double) FLT_MIN)
                        {
                            ret_missing.floatval = 0.0;
                        }
                        else
                        {
                            ret_missing.floatval = (float) dval;
                        }
                    }

                    ptr = (NclQuark *) in_value;

                    if(has_missing)
                    {
                        for(i = 0; i < total_elements; i++)
                        {
                            str = NrmQuarkToString(ptr[i]);
    
                            if(missing.stringval == ptr[i])
                            {
                                output[i] = ret_missing.floatval;
                            }
                            else
                            {
                                dval = strtod(str,&end);
                                if (strcmp(end, str) == 0)
                                {
                                    NhlPError(NhlWARNING,NhlEUNKNOWN,
                                        "tofloat: A bad value was passed to (string) tofloat, input strings must contain numeric digits, replacing with missing value");
                                    output[i] = ret_missing.floatval;
                                }
                                else if (dval > FLT_MAX)
                                {
                                    overflowed ++;
                                    output[i] = ret_missing.floatval;
                                }
                                else if (dval < (-FLT_MAX))
                                {
                                    underflowed ++;
                                    output[i] = ret_missing.floatval;
                                }
                                else
                                {
                                    dtest = fabs(dval);
                                    if (dtest < (double) FLT_MIN)
                                    {
                                        output[i] = 0.0;
                                    }
                                    else
                                    {
                                        output[i] = (float) dval;
                                    }
                                }
                            }
                        }
                    }
                    else
                    {
                        for(i = 0; i < total_elements; i++)
                        {
                            str = NrmQuarkToString(ptr[i]);
   
                            dval = strtod(str,&end);
                            if (strcmp(end, str) == 0)
                            {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                    "tofloat: A bad value was passed to (string) tofloat, input strings must contain numeric digits, replacing with missing value");
                                has_missing = 1;
                                output[i] = ret_missing.floatval;
                            }
                            else if (dval > FLT_MAX)
                            {
                                has_missing = 1;
                                overflowed ++;
                                output[i] = ret_missing.floatval;
                            }
                            else if (dval < (-FLT_MAX))
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.floatval;
                            }
                            else
                            {
                                dtest = fabs(dval);
                                if (dtest < (double) FLT_MIN)
                                {
                                    output[i] = 0.0;
                                }
                                else
                                {
                                    output[i] = (float) dval;
                                }
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tofloat: there are %d string(s) larger than FLT_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tofloat: there are %d string(s) less than (-FLT_MAX), which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    ptr = (byte *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    ptr = (unsigned char *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.charval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.floatval = (char) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short *ptr;

                    ptr = (short *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.shortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;

                    ptr = (unsigned short *) in_value;

                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.ushortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_int:
                {
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_long:
                {
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (float) val;
                    }
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.floatval = (float) missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (float) ptr[i];
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "tofloat: don't know how to convert logical to float.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "tofloat: don't know how to convert object to float.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "tofloat: don't know how to convert list to float.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "tofloat: don't know how to convert NCL_none to float.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "tofloat: don't know how to convert unknown type to float.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_float,
                0
        ));
}


NhlErrorTypes _NclItostring
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        NclQuark *output;

        char buffer[4*NCL_MAX_STRING];

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

        output = (NclQuark *)NclMalloc(sizeof(NclQuark)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "tostring: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_string:
                {
                    NclQuark *ptr;
                    char *strin;
                    char *strout;

                    ptr = (NclQuark *) in_value;

                    if(has_missing)
                    {
                        ret_missing.stringval = missing.stringval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        strin = (char *) NrmQuarkToString(ptr[i]);
                        strout = (char *) NclMalloc(strlen(strin) + 1);
                        sprintf(strout, "%s", strin);
                        output[i] = NrmStringToQuark(strout);
                        NclFree(strout);
                    }
                }
                break;
            case NCL_double:
                {
                    double *ptr;

                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%f", missing.doubleval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%f", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_float:
                {
                    float *ptr;

                    ptr = (float *) in_value;
    
                    if(has_missing)
                    {
                        sprintf(buffer, "%f", missing.floatval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%f", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    ptr = (byte *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%c", missing.byteval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%c", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;
                    char *str;
                    int n;
                    int cur_str_len;

                    ptr = (unsigned char *) in_value;

                    cur_str_len = dimsizes[n_dims-1];
                    str = (char *)NclMalloc(sizeof(char)*(cur_str_len + 1));
                    if (str == NULL)
                    {
                            NHLPERROR((NhlFATAL, errno, "tostring: memory allocation error."));
                            return NhlFATAL;
                    }
                    str[cur_str_len] = '\0';

                    if(has_missing)
                    {
                        str[0] = missing.charval;
                        str[1] = '\0';
                        ret_missing.stringval = NrmStringToQuark(str);
                    }

                    if(n_dims > 1)
                    {
                        n_dims --;
                        dimsizes[n_dims] = 0;
                    }
                    else
                    {
                        dimsizes[0] = 1;
                    }

                    total_elements = 1;
                    for(j = 0; j < n_dims; j++)
                    {
                        total_elements *= dimsizes[j];
                    }

                    output = (NclQuark *)NclRealloc(output, sizeof(NclQuark)*total_elements);
                    if (output == NULL)
                    {
                            NHLPERROR((NhlFATAL, errno, "tostring: memory allocation error."));
                            return NhlFATAL;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        n = i * cur_str_len;
                        for(j = 0; j < cur_str_len; j++)
                        {
                            str[j] = ptr[n++];
                            if(!str[j])
                                break;
                        }
                        output[i] = NrmStringToQuark(str);
                    }
                    free(str);
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;
    
                    ptr = (unsigned char *) in_value;
   
                    if(has_missing)
                    {
                        sprintf(buffer, "%d", missing.ubyteval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%d", (int)ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_short:
                {
                    short *ptr;

                    if(has_missing)
                    {
                        sprintf(buffer, "%hd", missing.shortval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%hd", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;
    
                    ptr = (unsigned short *) in_value;
   
                    if(has_missing)
                    {
                        sprintf(buffer, "%hu", missing.ushortval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%hu", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_int:
                {
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%d", missing.intval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%d", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
   
                    if(has_missing)
                    {
                        sprintf(buffer, "%u", missing.uintval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%u", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_long:
                {
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%ld", missing.longval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%ld", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%lu", missing.ulongval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%lu", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%lld", missing.int64val);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%lld", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%llu", missing.uint64val);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%llu", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "tostring: don't know how to convert logical to string.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "tostring: don't know how to convert object to string.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "tostring: don't know how to convert list to string.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "tostring: don't know how to convert NCL_none to string.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "tostring: don't know how to convert unknown type to string.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_string,
                0
        ));
}


NhlErrorTypes _NclItostring_with_format
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;

        int ndim_str;
	ng_size_t dimsz_str[NCL_MAX_DIMENSIONS];
        int has_missing_str = 0;
        NclScalar   missing_str;
        NclBasicDataTypes type_str;

        NclQuark *output;

        char *fmt;
        NclQuark *format;
        char buffer[4*NCL_MAX_STRING];

        in_value = (void *)NclGetArgValue(
                        0,
                        2,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.stringval = (NclQuark) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

        format = (NclQuark *) NclGetArgValue(
                        1,
                        2,
                        &ndim_str,
                        dimsz_str,
                        &missing_str,
                        &has_missing_str,
                        &type_str,
                        DONT_CARE);

        i = strlen((char *) NrmQuarkToString(format[0]));
        fmt = (char *) NclMalloc(i + 1);
        strcpy(fmt, (char *) NrmQuarkToString(format[0]));

        output = (NclQuark *)NclMalloc(sizeof(NclQuark)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "tostring_with_format: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_string:
                {
                    NclQuark *ptr;
                    char *strin;
                    char *strout;

                    ptr = (NclQuark *) in_value;

                    if(has_missing)
                    {
                        ret_missing.stringval = missing.stringval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        strin = (char *) NrmQuarkToString(ptr[i]);
                        strout = (char *) NclMalloc(strlen(strin) + 1);
                        sprintf(strout, fmt, strin);
                        output[i] = NrmStringToQuark(strout);
                        NclFree(strout);
                    }
                }
                break;
            case NCL_double:
                {
                    double *ptr;

                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.doubleval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_float:
                {
                    float *ptr;

                    ptr = (float *) in_value;
    
                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.floatval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    ptr = (byte *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.byteval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_char:
                {
                    char *ptr;

                    ptr = (char *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.charval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_short:
                {
                    short *ptr;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.shortval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;
    
                    ptr = (unsigned short *) in_value;
   
                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.ushortval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_int:
                {
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.intval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
   
                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.uintval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_long:
                {
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.longval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.ulongval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.int64val);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, fmt, missing.uint64val);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, fmt, ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "tostring_with_format: don't know how to convert logical to string.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "tostring_with_format: don't know how to convert object to string.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "tostring_with_format: don't know how to convert list to string.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "tostring_with_format: don't know how to convert NCL_none to string.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "tostring_with_format: don't know how to convert unknown type to string.");
                return NhlFATAL;
        }

        NclFree(fmt);

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_string,
                0
        ));
}




NhlErrorTypes _NclItodouble
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        double *output;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        errno = 0;

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.doubleval = (double) ((NclTypeClass) nclTypedoubleClass)->type_class.default_mis.doubleval;

        output = (double *)NclMalloc(sizeof(double)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "todouble: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double *ptr;

                    ptr = (double *) in_value;

                    if(has_missing)
                    {
                       ret_missing.doubleval = missing.doubleval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_float:
                {
                    float *ptr;

                    ptr = (float *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.floatval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_string:
                {
                    double dval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    ptr = (NclQuark *) in_value;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        dval = strtod(str,&end);
                        if((strcmp(end, str) == 0) || (errno == ERANGE))
                        {
                            ret_missing.doubleval = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis.doubleval;
                        }
                        else
                        {
                            ret_missing.doubleval = dval;
                        }

                        for(i = 0; i < total_elements; i++)
                        {
                            str = NrmQuarkToString(ptr[i]);
        
                            if(missing.stringval == ptr[i])
                            {
                                output[i] = ret_missing.doubleval;
                            }
                            else
                            {
                                dval = strtod(str,&end);
                                if((strcmp(end, str) == 0) || (errno == ERANGE))
                                {
                                    NhlPError(NhlWARNING,NhlEUNKNOWN,
                                        "todouble: A bad value was passed to (string) todouble, input strings must contain numeric digits, replacing with missing value");
                                    output[i] = ret_missing.doubleval;
                                    has_missing = 1;
                                }
                                else
                                {
                                    output[i] = dval;
                                }
                            }
                        }
                    }
                    else
                    {
                        for(i = 0; i < total_elements; i++)
                        {
                            str = NrmQuarkToString(ptr[i]);
   
                            dval = strtod(str,&end);
                            if((strcmp(end, str) == 0) || (errno == ERANGE))
                            {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                    "todouble: A bad value was passed to (string) todouble, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.doubleval;
                                has_missing = 1;
                            }
                            else
                            {
                                output[i] = dval;
                            }
                        }
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    ptr = (byte *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    ptr = (unsigned char *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.charval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (char) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short *ptr;

                    ptr = (short *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.shortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;

                    ptr = (unsigned short *) in_value;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.ushortval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_int:
                {
                    int *ptr;

                    ptr = (int *) in_value;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.intval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;

                    ptr = (unsigned int *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.uintval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_long:
                {
                    long *ptr;

                    ptr = (long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.longval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
    
                    ptr = (unsigned long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.ulongval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;

                    ptr = (long long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.int64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;

                    ptr = (unsigned long long *) in_value;

                    if(has_missing)
                    {
                        ret_missing.doubleval = (double) missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (double) ptr[i];
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "todouble: don't know how to convert logical to double.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "todouble: don't know how to convert object to double.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "todouble: don't know how to convert list to double.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "todouble: don't know how to convert NCL_none to double.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "todouble: don't know how to convert unknown type to double.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_double,
                0
        ));
}


NhlErrorTypes _NclItobyte
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        byte *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.byteval = (byte) ((NclTypeClass) nclTypebyteClass)->type_class.default_mis.byteval;

        output = (char *)NclMalloc(sizeof(byte)*total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "tobyte: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) SCHAR_MIN;
                    dmax = (double) SCHAR_MAX;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                           ret_missing.byteval = (unsigned char) val;
                    }

                    ptr = (double *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (double) ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < dmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d double(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d double(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) SCHAR_MIN;
                    fmax = (float) SCHAR_MAX;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                           ret_missing.byteval = (unsigned char) val;
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < fmin)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d float(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d float(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    NclQuark *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
                        }
                        else
                        {
                            if((llval < SCHAR_MAX) && (llval >= SCHAR_MIN))
                                ret_missing.byteval = (unsigned char) llval;
                        }
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(has_missing && (missing.stringval == ptr[i]))
                        {
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            llval = _Nclstrtoll(str,&end);
                            if (strcmp(end, str) == 0)
                            {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,
                                    "tobyte: A bad value was passed to tobyte, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.byteval;
                            }
                            else if (llval > SCHAR_MAX)
                            {
                                has_missing = 1;
                                overflowed ++;
                                output[i] = ret_missing.byteval;
                            }
                            else if (llval < SCHAR_MIN)
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.byteval;
                            }
                            else
                            {
                                output[i] = (byte) llval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d double(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d double(s)(s)(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    byte *ptr;

                    ptr = (byte *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.byteval = missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char val;
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        if(missing.charval < SCHAR_MAX)
                            ret_missing.byteval = (byte) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            if(has_missing && (val != missing.charval))
                            {
                                has_missing = 1;
                                overflowed ++;
                            }
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d char great than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char val;
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.byteval = (byte) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                            output[i] = (byte) val;
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d ubyte great than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    if(has_missing)
                    {
                        if((missing.shortval <= SCHAR_MAX) && (missing.shortval >= SCHAR_MIN))
                            ret_missing.byteval = (byte) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < SCHAR_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d short(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d int short(s)(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short val;
                    unsigned short *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ushortval <= SCHAR_MAX)
                            ret_missing.byteval = (byte) missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
  
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d ushort(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    if(has_missing)
                    {
                        if((missing.intval <= SCHAR_MAX) && (missing.intval >= SCHAR_MIN))
                            ret_missing.byteval = (byte) missing.intval;
                    }

                    ptr = (int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < SCHAR_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
   
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d int large than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d int(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int val;
                    unsigned int *ptr;

                    if(has_missing)
                    {
                        if(missing.uintval <= SCHAR_MAX)
                            ret_missing.byteval = (byte) missing.uintval;
                    }

                    ptr = (unsigned int *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d unsigned int(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    if(has_missing)
                    {
                        if((missing.longval <= SCHAR_MAX) && (missing.longval >= SCHAR_MIN))
                            ret_missing.byteval = (byte) missing.longval;
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < SCHAR_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d long(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d long(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ulongval <= SCHAR_MAX)
                            ret_missing.byteval = (byte) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d unsigned ulong(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    if(has_missing)
                    {
                        if((missing.int64val <= SCHAR_MAX) && (missing.int64val >= SCHAR_MIN))
                            ret_missing.byteval = (byte) missing.int64val;
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < SCHAR_MIN)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d int64(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d int64(s) less than SCHAR_MIN, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;

                    if(has_missing)
                    {
                        if(missing.uint64val <= SCHAR_MAX)
                            ret_missing.byteval = (byte) missing.uint64val;
                    }

                    ptr = (unsigned long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > SCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            output[i] = (byte) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tobyte: there are %d uint64(s) larger than SCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "tobyte: don't know how to convert logical to byte.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "tobyte: don't know how to convert object to byte.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "tobyte: don't know how to convert list to byte.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "tobyte: don't know how to convert NCL_none to byte.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "tobyte: don't know how to convert unknown type to byte.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_byte,
                0
        ));
}


NhlErrorTypes _NclItochar
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int j;
	ng_size_t i;
        char *output;

        int overflowed = 0;
        int underflowed = 0;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        ret_missing.charval = (unsigned char) ((NclTypeClass) nclTypecharClass)->type_class.default_mis.charval;

        output = (char *)NclMalloc(sizeof(char) * total_elements);
	if (output == NULL)
	{
        	NHLPERROR((NhlFATAL, errno, "tochar: memory allocation error."));
		return NhlFATAL;
	}

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = (double) 0.0;
                    dmax = (double) UCHAR_MAX;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val <= dmax) && (val >= dmin))
                           ret_missing.charval = (char) val;
                    }

                    ptr = (double *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > dmax)
                        {
                            if(has_missing && (val != missing.doubleval))
                                overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < dmin)
                        {
                            if(has_missing && (val != missing.doubleval))
                                underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d double(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d double(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_float:
                {
                    float val, fmin, fmax;
                    float *ptr;

                    fmin = (float) 0;
                    fmax = (float) UCHAR_MAX;

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val <= fmax) && (val >= fmin))
                           ret_missing.charval = (unsigned char) val;
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > fmax)
                        {
                            if(has_missing && (val != missing.floatval))
                                overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < fmin)
                        {
                            if(has_missing && (val != missing.floatval))
                                underflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d float(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d float(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    NclQuark *ptr;
                    char *str;
                    int   cur_str_len = 1;
                    int   max_str_len = 1;
                    int   n = 0;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        if(missing.stringval != ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval)
                            ret_missing.charval = '\0';		/* default missing char value is 0 */
                    }

                    ptr = (NclQuark *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(has_missing && (missing.stringval == ptr[i]))
                        {
                            continue;
                        }
                        else
                        {
                            cur_str_len = strlen(str);
                            if(max_str_len < cur_str_len)
                               max_str_len = cur_str_len;
                        }
                    }

                    output = (char *)NclRealloc(output, sizeof(char)*total_elements*max_str_len);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tochar: memory allocation error."));
                        return NhlFATAL;
                    }
		    memset(output, 0, total_elements*max_str_len);

                    if(total_elements > 1)
                    {
                       dimsizes[n_dims] = max_str_len;
                       n_dims ++;
                    }
                    else
                    {
                       dimsizes[0] = max_str_len;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        n = i * max_str_len;
                        str = NrmQuarkToString(ptr[i]);
    
                        if(has_missing && (missing.stringval == ptr[i]))
                        {
                            output[n++] = ret_missing.charval;
                        }
                        else
                        {
                            for(j = 0; j < strlen(str); j++)
                            {
                                output[n++] = str[j];
                            }
                        }
                    }

                    return(NclReturnValue((void*)output,
                                           n_dims,
                                           dimsizes,
                                           (has_missing ? &ret_missing : NULL),
                                           NCL_char,
                                           0));
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.charval = missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_byte:
                {
                    byte val;
                    byte *ptr;

                    ptr = (byte *) in_value;
    
                    if(has_missing)
                    {
                        if(missing.byteval >= 0)
                            ret_missing.charval = (unsigned char) missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d byte(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.charval = (char) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = ptr[i];
                    }
                }
                break;
            case NCL_short:
                {
                    short val;
                    short *ptr;
    
                    if(has_missing)
                    {
                        if((missing.shortval <= UCHAR_MAX) && (missing.shortval >= 0))
                            ret_missing.charval = (char) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d short(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d short(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ushort:
                {
                    unsigned short val;
                    unsigned short *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ushortval <= UCHAR_MAX)
                            ret_missing.charval = (char) missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }
  
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d ushort(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int:
                {
                    int val;
                    int *ptr;

                    if(has_missing)
                    {
                        if((missing.intval <= UCHAR_MAX) && (missing.intval >= 0))
                            ret_missing.charval = (char) missing.intval;
                    }

                    ptr = (int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else if(val <  0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }
   
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d int(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d int(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint:
                {
                    unsigned int val;
                    unsigned int *ptr;

                    if(has_missing)
                    {
                        if(missing.uintval <= UCHAR_MAX)
                            ret_missing.charval = (char) missing.uintval;
                    }

                    ptr = (unsigned int *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d unsigned int(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_long:
                {
                    long val;
                    long *ptr;

                    if(has_missing)
                    {
                        if((missing.longval <= UCHAR_MAX) && (missing.longval >= 0))
                            ret_missing.charval = (char) missing.longval;
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d long(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d long(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_ulong:
                {
                    unsigned long val;
                    unsigned long *ptr;
    
                    if(has_missing)
                    {
                        if(missing.ulongval <= UCHAR_MAX)
                            ret_missing.charval = (char) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d unsigned long(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_int64:
                {
                    long long val;
                    long long *ptr;

                    if(has_missing)
                    {
                        if((missing.int64val <= UCHAR_MAX) && (missing.int64val >= 0))
                            ret_missing.charval = (char) missing.int64val;
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d int64(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d int64(s) less than 0, which have been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long val;
                    unsigned long long *ptr;


                    if(has_missing)
                    {
                        if(missing.uint64val <= UCHAR_MAX)
                            ret_missing.charval = (char) missing.uint64val;
                    }

                    ptr = (unsigned long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            output[i] = (char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "tochar: there are %d uint64(s) larger than UCHAR_MAX, which have been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "tochar: don't know how to convert logical to char.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "tochar: don't know how to convert object to char.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "tochar: don't know how to convert list to char.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "tochar: don't know how to convert NCL_none to char.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "tochar: don't know how to convert unknown type to char.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                NCL_char,
                0
        ));
}

NhlErrorTypes _NclItosigned
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type, out_type;
        int has_missing;
        int j;
	ng_size_t i;
        void *output;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        if(has_missing)
        {
            ret_missing = missing;
        }

        switch(type)
        {
            case NCL_byte:
                {
                    byte *ptr;
                    char *out_ptr;

                    output = (void *)NclMalloc(sizeof(char)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    ptr = (byte *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (char) ptr[i];
                    }
                    out_type = NCL_byte;
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;
                    char *out_ptr;

                    output = (void *)NclMalloc(sizeof(char)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    if(has_missing)
                    {
                        ret_missing.byteval = (char) missing.ubyteval;
                    }

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (char) ptr[i];
                    }
                    out_type = NCL_byte;
                }
                break;
            case NCL_short:
                {
                    short *ptr;
                    short *out_ptr;

                    output = (void *)NclMalloc(sizeof(short)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_short;
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;
                    short *out_ptr;

                    output = (void *)NclMalloc(sizeof(short)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    if(has_missing)
                    {
                         ret_missing.shortval = (short) missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (short) ptr[i];
                    }
                    out_type = NCL_short;
                }
                break;
            case NCL_int:
                {
                    int *ptr;
                    int *out_ptr;

                    output = (void *)NclMalloc(sizeof(int)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    ptr = (int *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_int;
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;
                    int *out_ptr;

                    output = (void *)NclMalloc(sizeof(int)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.uintval;
                    }

                    ptr = (unsigned int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (int) ptr[i];
                    }
                    out_type = NCL_int;
                }
                break;
            case NCL_long:
                {
                    long *ptr;
                    long *out_ptr;

                    output = (void *)NclMalloc(sizeof(long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_long;
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
                    long *out_ptr;

                    output = (void *)NclMalloc(sizeof(long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (long) ptr[i];
                    }
                    out_type = NCL_long;
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;
                    long long *out_ptr;

                    output = (void *)NclMalloc(sizeof(long long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    ptr = (long long *) in_value;
   
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_int64;
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;
                    long long *out_ptr;

                    output = (void *)NclMalloc(sizeof(long long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tosigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.uint64val;
                    }

                    ptr = (unsigned long long *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (long long) ptr[i];
                    }
                    out_type = NCL_int64;
                }
                break;
            default:
                NhlPError(NhlFATAL, errno, "tosigned: don't know how to convert type to singed.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                out_type,
                0
        ));
}


NhlErrorTypes _NclItounsigned
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        ng_size_t total_elements = 1;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type, out_type;
        int has_missing;
        int j;
	ng_size_t i;
        void *output;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        for(j = 0; j < n_dims; j++)
        {
            total_elements *= dimsizes[j];
        }

        if(has_missing)
        {
            ret_missing = missing;
        }

        switch(type)
        {
            case NCL_byte:
                {
                    char *ptr;
                    unsigned char *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned char)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    if(has_missing)
                    {
                        ret_missing.ubyteval = (char) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (unsigned char)ptr[i];
                    }
                    out_type = NCL_ubyte;
                }
                break;
            case NCL_ubyte:
                {
                    unsigned char *ptr;
                    unsigned char *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned char)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    ptr = (unsigned char *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (unsigned char) ptr[i];
                    }
                    out_type = NCL_ubyte;
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;
                    unsigned short *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned short)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_ushort;
                }
                break;
            case NCL_short:
                {
                    short *ptr;
                    unsigned short *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned short)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    if(has_missing)
                    {
                        ret_missing.ushortval = (unsigned short) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (unsigned short) ptr[i];
                    }
                    out_type = NCL_ushort;
                }
                break;
            case NCL_uint:
                {
                    unsigned int *ptr;
                    unsigned int *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned int)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    ptr = (unsigned int *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_int;
                }
                break;
            case NCL_int:
                {
                    int *ptr;
                    unsigned int *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned int)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    if(has_missing)
                    {
                        ret_missing.uintval = (unsigned int) missing.intval;
                    }

                    ptr = (int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (unsigned int) ptr[i];
                    }
                    out_type = NCL_uint;
                }
                break;
            case NCL_ulong:
                {
                    unsigned long *ptr;
                    unsigned long *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_ulong;
                }
                break;
            case NCL_long:
                {
                    long *ptr;
                    unsigned long *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.longval;
                    }

                    ptr = (long *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (unsigned long) ptr[i];
                    }
                    out_type = NCL_ulong;
                }
                break;
            case NCL_uint64:
                {
                    unsigned long long *ptr;
                    unsigned long long *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned long long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
   
                    ptr = (unsigned long long *) in_value;
   
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_uint64;
                }
                break;
            case NCL_int64:
                {
                    long long *ptr;
                    unsigned long long *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned long long)*total_elements);
                    if (output == NULL)
                    {
                        NHLPERROR((NhlFATAL, errno, "tounsigned: memory allocation error."));
                        return NhlFATAL;
                    }
                    out_ptr = output;
    
                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.int64val;
                    }

                    ptr = (long long *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                       out_ptr[i] = (unsigned long long) ptr[i];
                    }
                    out_type = NCL_uint64;
                }
                break;
            default:
                NhlPError(NhlFATAL, errno, "tounsigned: don't know how to convert type to singed.");
                return NhlFATAL;
        }

        return(NclReturnValue(
                (void*)output,
                n_dims,
                dimsizes,
                (has_missing ? &ret_missing : NULL),
                out_type,
                0
        ));
}

NhlErrorTypes _NclIIsUnsigned
#if	NhlNeedProto
(void)
#else
()
#endif
{
	logical *out_val;
        void *in_value;
        int n_dims = 0;
        ng_size_t dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclBasicDataTypes type;
        int has_missing;

        in_value = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

	out_val = (logical*)NclMalloc(sizeof(logical));
	*out_val = 0;

        switch(type)
        {
            case NCL_ubyte:
            case NCL_ushort:
            case NCL_uint:
            case NCL_ulong:
            case NCL_uint64:
		*out_val = 1;
		if(NULL == in_value)
			*out_val = 0;
		break;
            default:
		*out_val = 0;
		break;
        }

	type = NCL_logical;
        dimsizes[0] = 1;
	return(NclReturnValue(
		out_val,
		1,
		dimsizes,
		NULL,
		type,
		0
	));
}

NhlErrorTypes _Ncldefault_fillvalue
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_type;
        int n_dims = 0;
        ng_size_t dimsizes;
        NclScalar missing;
        NclBasicDataTypes type, out_type;
        int has_missing;
	NclTypeClass type_class;
        void *output;

	dimsizes = 1;
        in_type = (void *)NclGetArgValue(
                        0,
                        1,
                        &n_dims,
                        &dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

	type_class = _NclNameToTypeClass(*(NrmQuark *)in_type);
	
        if (! type_class) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"default_fillvalue: invalid type string: %s", NrmQuarkToString(*(NrmQuark *)in_type));
		return NhlFATAL;
	}

	output = NclMalloc(type_class->type_class.size);
	out_type = type_class->type_class.data_type;
	switch (out_type) {
	case NCL_short:
		*(short *) output = type_class->type_class.default_mis.shortval;
		break;
	case NCL_int:
		*(int *) output = type_class->type_class.default_mis.intval;
		break;
	case NCL_long:
		*(long *) output = type_class->type_class.default_mis.longval;
		break;
	case NCL_int64:
		*(long long *) output = type_class->type_class.default_mis.int64val;
		break;
        case NCL_ushort:
		*(unsigned short *) output = type_class->type_class.default_mis.ushortval;
		break;
        case NCL_uint:
		*(unsigned int *) output = type_class->type_class.default_mis.uintval;
		break;
        case NCL_ulong:
		*(unsigned long *) output = type_class->type_class.default_mis.ulongval;
		break;
        case NCL_uint64:
		*(unsigned long long *) output = type_class->type_class.default_mis.uint64val;
		break;
        case NCL_ubyte:
		*(unsigned char *) output = type_class->type_class.default_mis.ubyteval;
		break;
	case NCL_float:
		*(float *) output = type_class->type_class.default_mis.floatval;
		break;
	case NCL_double:
		*(double *) output = type_class->type_class.default_mis.doubleval;
		break;
	case NCL_char:
		*(unsigned char *) output = type_class->type_class.default_mis.charval;
		break;
	case NCL_byte:
		*(char *) output = type_class->type_class.default_mis.byteval;
		break;
	case NCL_string:
		*(NrmQuark *) output = type_class->type_class.default_mis.stringval;
		break;
	case NCL_logical:
		*(int *) output = type_class->type_class.default_mis.logicalval;
		break;
	case NCL_obj:
		*(int *) output = type_class->type_class.default_mis.objval;
		break;
	case NCL_group:
		*(int *) output = type_class->type_class.default_mis.groupval;
		break;
	case NCL_compound:
		*(int *) output = type_class->type_class.default_mis.compoundval;
		break;
	case NCL_list:
	default:
		*(int *) output = type_class->type_class.default_mis.intval;
		break;
	}

        return(NclReturnValue(
                (void*)output,
                1,
                &dimsizes,
                NULL,
                out_type,
                0
        ));

}


NhlErrorTypes _Nclset_default_fillvalue
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_type;
        int n_dims = 0;
        NclScalar missing;
	int has_missing;
        ng_size_t dimsizes;
        NclBasicDataTypes type, set_type;
	NclTypeClass type_class;
        void *in_value;
	NclScalar in_value_coerced;
	static int first = 1;
	static NrmQuark q_all,q_nclv5,q_nclv6,q_default;

	if (first) {
		q_all = NrmStringToQuark("all");
		q_nclv5 = NrmStringToQuark("ncl_v5");
		q_nclv6 = NrmStringToQuark("ncl_v6");
		q_default = NrmStringToQuark("default");
		first = 0;
	}

	dimsizes = 1;
        in_type = (void *)NclGetArgValue(
                        0,
                        2,
                        &n_dims,
                        &dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        in_value = (void *)NclGetArgValue(
                        1,
                        2,
                        &n_dims,
                        &dimsizes,
                        &missing,
                        &has_missing,
                        &type,
                        0);

        if (_NclGetLower(*(NrmQuark*)in_type) == q_all) {
		NrmQuark q_inval;
		if (type != NCL_string) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"set_default_fillvalue: value type must be string when setting 'all'");
			return NhlFATAL;
		}
		q_inval = _NclGetLower(*(NrmQuark*)in_value);
		if  (q_inval == q_nclv5) {
			return (_NclSetDefaultFillValues(NCL_5_DEFAULT_FILLVALUES));
		}
		else if (q_inval == q_default || q_inval == q_nclv6) {
			return (_NclSetDefaultFillValues(NCL_6_DEFAULT_FILLVALUES));
		}
	        else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"set_default_fillvalue: invalid value for setting 'all'");
			return NhlFATAL;
		}
	}
		
	type_class = _NclNameToTypeClass(*(NrmQuark *)in_type);
	
        if (! type_class) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"set_default_fillvalue: invalid type string: %s", NrmQuarkToString(*(NrmQuark *)in_type));
		return NhlFATAL;
	}
	set_type = type_class->type_class.data_type;

	if (type != set_type) {
		if (!_NclScalarCoerce(in_value,type,&in_value_coerced,set_type)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"set_default_fillvalue: value cannot be coerced to type: %s", NrmQuarkToString(*(NrmQuark *)in_type));
			return NhlFATAL;
		}
		in_value = &in_value_coerced;
	}

	switch (set_type) {
	case NCL_short:
		type_class->type_class.default_mis.shortval = *(short *) in_value;
		break;
	case NCL_int:
		type_class->type_class.default_mis.intval = *(int *) in_value;
		break;
	case NCL_long:
		type_class->type_class.default_mis.longval = *(long *) in_value;
		break;
	case NCL_int64:
		type_class->type_class.default_mis.int64val = *(long long *) in_value;
		break;
        case NCL_ushort:
		type_class->type_class.default_mis.ushortval = *(unsigned short *) in_value;
		break;
        case NCL_uint:
		type_class->type_class.default_mis.uintval = *(unsigned int *) in_value;
		break;
        case NCL_ulong:
		type_class->type_class.default_mis.ulongval = *(unsigned long *) in_value;
		break;
        case NCL_uint64:
		type_class->type_class.default_mis.uint64val = *(unsigned long long *) in_value;
		break;
        case NCL_ubyte:
		type_class->type_class.default_mis.ubyteval = *(unsigned char *) in_value;
		break;
	case NCL_float:
		type_class->type_class.default_mis.floatval = *(float *) in_value;
		break;
	case NCL_double:
		type_class->type_class.default_mis.doubleval = *(double *) in_value;
		break;
	case NCL_char:
		type_class->type_class.default_mis.charval = *(unsigned char *) in_value;
		break;
	case NCL_byte:
		type_class->type_class.default_mis.byteval = *(char *) in_value;
		break;
	case NCL_string:
		type_class->type_class.default_mis.stringval = *(NrmQuark *) in_value;
		break;
	case NCL_logical:
		if (*(int*)in_value == 0 || *(int*) in_value == -1)
			type_class->type_class.default_mis.logicalval = *(int *) in_value;
		else
			type_class->type_class.default_mis.logicalval = 1;
		break;
	case NCL_obj:
		type_class->type_class.default_mis.objval = *(int*) in_value;
	case NCL_group:
		type_class->type_class.default_mis.objval = *(int*) in_value;
		break;
	case NCL_compound:
		type_class->type_class.default_mis.objval = *(int*) in_value;
		break;
	case NCL_list:
	default:
		type_class->type_class.default_mis.objval = *(int*) in_value;

	}
	return NhlNOERROR;

}

NhlErrorTypes _Nclget_wall_time(void)
{
        ng_size_t dimsize = 1;
	double wtime;
	NhlErrorTypes retval;

	retval = NclGetWTime(&wtime);
	if(retval != NhlNOERROR){
		NhlPError(NhlWARNING, NhlEUNKNOWN, "unable to get process wall time");
		return(NhlWARNING);
	}

        return(NclReturnValue(
                &wtime,
                1,
                &dimsize,
                NULL,
                NCL_double,
                1
        ));

}

NhlErrorTypes _Nclget_cpu_time(void)
{
        ng_size_t dimsize = 1;
	float time;
	NhlErrorTypes retval;

	retval = NclGetCPUTime(&time);
	if(retval != NhlNOERROR){
		NhlPError(NhlWARNING, NhlEUNKNOWN, "unable to get process cpu time");
		return(NhlWARNING);
	}

        return(NclReturnValue(
                &time,
                1,
                &dimsize,
                NULL,
                NCL_float,
                1
        ));

}

NhlErrorTypes _NclIGetFileVarChunkDimsizes(void)
{
	NclQuark *name;
	NclQuark fname;
	NclScalar name_missing;
	int name_has_missing;
	ng_size_t nchunkdims = 1;
	NclStackEntry val;
	NclVar tmp_var;
	NclMultiDValData tmp_md = NULL;
	NclFile thefile = NULL;
	long chunkdim_sizes[NCL_MAX_DIMENSIONS];
	int i;

	chunkdim_sizes[0] = -4294967296;

        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
	case NclStk_VAR:
		tmp_var = val.u.data_var;
		if(tmp_var->var.var_quark > 0) {
			fname = tmp_var->var.var_quark;
		} else {
			fname = -1;
		}
		break;
	case NclStk_VAL:
	default:
		return(NclReturnValue(&chunkdim_sizes[0], 1, &nchunkdims,
			&((NclTypeClass)nclTypelongClass)->type_class.default_mis, NCL_long, 1));
	}

        name = (NclQuark*)NclGetArgValue(
                        1,
                        2,
                        NULL,
                        NULL,
                        &name_missing,
                        &name_has_missing,
                        NULL,
                        0);

	if(name_has_missing) {
		if(*name == name_missing.stringval) {
		        return(NclReturnValue(
               			&chunkdim_sizes[0],
                		1,
                		&nchunkdims,
                		&name_missing,
                		NCL_long,
                		1
        		));
		}
	}

	tmp_md = _NclVarValueRead(tmp_var,NULL,NULL);
	if(tmp_md != NULL)
	{
		thefile = (NclFile)_NclGetObj(*(obj*)tmp_md->multidval.val);
		if(thefile != NULL )
			nchunkdims = _NclGetFileVarChunkInfo(thefile, *name, chunkdim_sizes);
	}

	return(NclReturnValue(chunkdim_sizes, 1, &nchunkdims, NULL, NCL_long, 1));
}

#ifdef __cplusplus
}
#endif

/*
 *      $Id: BuiltInFuncs.c,v 1.252.6.1 2010-05-06 18:49:44 dbrown Exp $
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
#include "NclVar.h"
#include "NclCoordVar.h"
#include "VarSupport.h"
#include "DataSupport.h"
#include "NclMdInc.h"
#include "NclHLUObj.h"
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

extern int cmd_line;
extern short NCLnoSysPager;
extern char *nclf;

long long local_strtoll(const char *nptr, char **endptr, int base);

NhlErrorTypes _NclIGetScriptPrefixName
#if     NhlNeedProto
(void)
#else
()
#endif
{
    int dimsz = 1,
        ndims = 1;
    string  script_name;
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
    int dimsz = 1,
        ndims = 1;
    string  script_name;
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
			ret = nclfprintf(fp,"%d ] x ",step->u.var->dim_info[i].dim_size);
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
                ret = nclfprintf(fp,"%d ]\n",step->u.var->dim_info[step->u.var->n_dims - 1].dim_size);
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
			ret = nclfprintf(fp,"%d\n",step->u.file->dim_info[i].dim_size);
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
							ret = nclfprintf(fp,"[%d]",step->u.func->theargs[i].dim_sizes[j]);
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
						ret = nclfprintf(fp,"[%d]",step->u.func->theargs[step->u.func->nparams-1].dim_sizes[j]);
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

NhlErrorTypes _NclIGetFileVarNames
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry data;
	NclApiDataList *tmp,*step;
	NclQuark *var_names = NULL;
	NclQuark file_q;
	int i,ret =0;
	int dimsize = 0;
	NclFile thefile;
	NclMultiDValData tmp_md = NULL;
	
	tmp = NULL;
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
	if(file_q == -1) {
		if(tmp_md==NULL) 
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		thefile = (NclFile)_NclGetObj(*(int*)tmp_md->multidval.val);
		
		var_names = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*thefile->file.n_vars);
		dimsize = thefile->file.n_vars;
	
		for(i = thefile->file.n_vars-1; i>=0;i-- 	) {
			if(thefile->file.var_info[i] != NULL) {
				var_names[i] = thefile->file.var_info[i]->var_name_quark;
			} else {
				var_names[i] = 0;
			}
		}
	} else {
		tmp = _NclGetFileVarInfoList(file_q);
		if(tmp==NULL){
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIGetFileVarNames: file does not exist");
			return(NhlFATAL);
		} 
		step = tmp;
		i = 0;
		while(step != NULL) {
			i++;
			step = step->next;
		}
		var_names = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*i);
		step = tmp;
		dimsize = i;
		i = 0;
		while(step != NULL) {
			var_names[i] = step->u.var->name;
			step = step->next;
			i++;
		}
	}
		
	data.kind = NclStk_VAL;
	data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)var_names,NULL,1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
	_NclPlaceReturn(data);
	if(tmp != NULL) 
		_NclFreeApiDataList((void*)tmp);
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
			ret = nclfprintf(fp,"%d ] x ",step->u.var->dim_info[i].dim_size);
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
                ret = nclfprintf(fp,"%d ]\n",step->u.var->dim_info[step->u.var->n_dims - 1].dim_size);
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
	int tmp_dimsizes = 1;
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
	int tmp_dimsizes = 1;
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
        logical *lval;
        int dimsize = 1;
	char* command;
	char *pager;
	int fildes[2],new_pipe_fd;
        int ret;
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
	int nelem = 0;


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
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMissing();
			NclFree(qbuffer);
			_NclPlaceReturn(data);
		} else {
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)qbuffer,NULL,1,&nelem,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
			_NclPlaceReturn(data);
		}
		return(NhlNOERROR);
	} else {
                return(NhlFATAL);
	}
}

NhlErrorTypes _Nclstrlen
#if     NhlNeedProto
(void)
#else
()
#endif
{
    string* strs;
    int ndims,
        dimsz[NCL_MAX_DIMENSIONS];
    int has_missing,
        found_missing = 0;
    NclScalar   missing,
                ret_missing;
  
    int sz = 1;
    int*    lens;
    int i;
    

    strs = (string *) NclGetArgValue(
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
    NclStackEntry val,data;
    NclMultiDValData tmp_md = NULL;
    logical *lval;
    int dimsize = 1;
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
	int dimsize = 1;
	int i;
	
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
	int dim_size = 1;
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
	file = _NclCreateFile(NULL,NULL,Ncl_File,0,TEMPORARY,*(NclQuark*)p_md->multidval.val,rw_v);
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
	int i,dim_size = 1;
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
	int i,dim_size = 1;
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
	int *size;
	int dim_size = 1;

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
		size = NclMalloc(sizeof(int));
		*size = _NclSizeOf(tmp_md->multidval.data_type)*tmp_md->multidval.totalelements;
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
	int *size;
	int dim_size,i;

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
		size = NclMalloc(sizeof(int)*tmp_md->multidval.n_dims);
		for(i = 0; i< tmp_md->multidval.n_dims; i++) {
			size[i] = tmp_md->multidval.dim_sizes[i];
		}
		dim_size = tmp_md->multidval.n_dims;
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
		if(data_out.u.data_obj != NULL ) {
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
	int *obj_ids,i;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar) ) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-object passed to frame, ignoring request");
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
	int *obj_ids,i;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar) ) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-object passed to clear, ignoring request");
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
	NclSymbol *thesym;
	NclStackEntry *var;
	int *obj_ids,i;
	NclHLUObj hlu_ptr = NULL;
	NclMultiDValData att_md;
	void *att_val;

	data = _NclGetArg(0,1,WRITE_IT);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-object passed to destroy, ignoring request");
	
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
	int *obj_ids,i;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-object passed to update, ignoring request");
	
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
	int *obj_ids,i;
	NclHLUObj hlu_ptr;

	data = _NclGetArg(0,1,DONT_CARE);

	if(data.kind == NclStk_VAR) {
		if(!(data.u.data_var->obj.obj_type_mask & Ncl_HLUVar)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-object passed to draw, ignoring request");
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
/*
		if(data.u.data_obj->obj.ref_count != 0) {
			rlist = data.u.data_obj->obj.parents;
			while(rlist != NULL) {
				switch(rlist->pptr->obj.obj_type) {
				case Ncl_Att:
					_NclDeleteAttMDID((NclObj)rlist->pptr->obj.id,(NclObj)data.u.data_obj->obj.id);
					break;
				default:
					_NclDelParent((NclObj)data.u.data_obj,(NclObj)rlist->pptr);
					break;
				}
				rlist = rlist->next;
			}
		} else {
*/
			_NclDestroyObj((NclObj)data.u.data_obj);
/*
		}
*/
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
						rlist = data.u.data_obj->obj.parents;
					}
					break;
				default:
					rlist = data.u.data_obj->obj.parents;
					while(rlist != NULL) {
						pobj = _NclGetObj(rlist->pid);
						_NclDelParent((NclObj)data.u.data_obj,(NclObj)pobj);
						rlist = rlist->next;
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
	int i;
	int *dims;
	int  dimsizes,dimsizes1,dimsizes2;
	int has_missing,has_missing1,has_missing2;
	NclScalar missing,missing1,missing2;
	NclBasicDataTypes type0,type1,type2;
	int m,n;
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
	dims = (int*)NclGetArgValue( 3, 4, NULL, NULL, NULL, &has_missing, NULL,DONT_CARE);

	if((dimsizes == dimsizes1)&&(dimsizes = dimsizes2)){
		xmax = (arg[0])[0];
		xmin = (arg[0])[0];
		ymax = (arg[1])[0];
		ymin = (arg[1])[0];
		for(i = 0; i < dimsizes; i++) {
			if(has_missing) {
				if((arg[0])[i] == missing.floatval) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclisdfft: input contains missing values, cannot continue");
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclisdfft: input contains missing values, cannot continue");
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclisdfft: input contains missing values, cannot continue");
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
		iwrk = (int*)NclMalloc((31 * dimsizes + m * n)*sizeof(int));
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

static qsort_compare_func
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
	NclMultiDValData tmp_md= NULL,tmp_md2 = NULL;
	NclVar tmp_var;
	long *index;
	int i;
	NclSelectionRecord * sel_ptr = NULL;
	NhlErrorTypes ret;


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

	index = (long*)NclMalloc(tmp_md->multidval.totalelements * sizeof(long));
	for(i = 0; i < tmp_md->multidval.totalelements; i++) {
		index[i] = i;
	}
	qsort((void*)index,tmp_md->multidval.totalelements,sizeof(long),qsort_compare_func);
	
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
			_NclWriteCoordVar(args.u.data_var,_NclVarValueRead(tmp_var,sel_ptr,NULL),NrmQuarkToString(args.u.data_var->var.dim_info[0].dim_quark),NULL);
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
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	int totalsize = 0;
	off_t f_off;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	int *recnum;
	NclScalar missing;
	int has_missing;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
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
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = (int*)tmp_md->multidval.val;
/*
		if((n_dimensions == 1)&&(dimsizes[0] == -1)) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: -1 is not supported for fbindirread use cbinread");
			return(NhlFATAL);
		}
*/
	}
	if((n_dimensions ==1)&&(dimsizes[0] == -1)) {
                size = buf.st_size/thetype->type_class.size;
                n_dimensions = 1;
                dimsizes = &size;
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
			int count = (int)(step - (char*)tmp_ptr) / thetype->type_class.size;
			ret = _NclSwapBytes(NULL,tmp_ptr,count,thetype->type_class.size);
			if (ret < NhlWARNING)
				return ret;
		}
		while((int)(step - (char*)tmp_ptr) < totalsize) {
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


NhlErrorTypes _NclIisbigendian
#if	NhlNeedProto
(void)
#else
()
#endif
{
	logical *out_val;
	int dimsizes = 1;

	out_val = (logical*)NclMalloc(sizeof(logical));
#ifdef ByteSwapped
	*out_val = 0;
#else
	*out_val = 1;
#endif

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
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	int dim2;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
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
		dimsizes = (int*)tmp_md->multidval.val;
	}
	if((n_dimensions ==1)&&(dimsizes[0] == -1)) {
		size = buf.st_size/thetype->type_class.size;
		n_dimensions = 1;
		dim2 = size;
		dimsizes = &dim2;
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

		step = tmp_ptr;
		for(i = 0; i < (int)(totalsize / buf.st_blksize); i++) {
			n = read(fd, step,buf.st_blksize);
			step = step + buf.st_blksize;
		}
		n = read(fd,step,totalsize % buf.st_blksize);
		step = step + totalsize % buf.st_blksize;
		if (swap_bytes) {
			int count = ((int)(step - (char*)tmp_ptr)) / thetype->type_class.size;
			ret = _NclSwapBytes(NULL,tmp_ptr,count,thetype->type_class.size);
			if (ret < NhlWARNING)
				return ret;
		}
		while((int)(step - (char*)tmp_ptr) < totalsize) {
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
	NclStackEntry data;
	NclMultiDValData tmp_md;
	string *fpath;
	NclScalar missing;
	int 	has_missing = 0;
	char 	control_word[4];
	int fd = -1;
	int ind1,ind2;
	off_t cur_off;
	int i,n;
	int dimsize = 1;
	int swap_bytes = 0;
	NhlErrorTypes ret = NhlNOERROR;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	fpath = (string*)NclGetArgValue(
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
		n = read(fd,(control_word),4);
		if(n != 4) {
			break;
		}
		if (! swap_bytes)
			ind1 = *(int*)control_word;
		else
			_NclSwapBytes(&ind1,control_word,1,sizeof(int));
		lseek(fd,cur_off + (off_t)(ind1 + 4),SEEK_SET);
		n = read(fd,(control_word),4);
		if(n != 4) {
			break;
		}
		if (! swap_bytes) 
			ind2 = *(int*)control_word;
		else
			_NclSwapBytes(&ind2,control_word,1,sizeof(int));
		if(ind1 ==  ind2) {
				i++;
				cur_off += (off_t)(ind1 + 8);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
			close(fd);
			return(NhlFATAL);
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
	string *fpath;
	int	*recnum;
	int	*dimensions;
	int	dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing;
	NclMultiDValData tmp_md;
	NclStackEntry data;
	int 	has_missing = 0;
	NclTypeClass type;
	char 	control_word[4];
	void *value;
	int i;
	int ind1,ind2;
	int fd = -1;
	int size = 1;
	int n;
	int n_dims;
	off_t cur_off = 0;
	NhlErrorTypes ret = NhlNOERROR;
	int rsize = 0;
	NclBasicDataTypes datai_type;
	int total;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	control_word[0] = (char)0;
	control_word[1] = (char)0;
	
	fpath = (string*)NclGetArgValue(
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


	fd = open(_NGResolvePath(NrmQuarkToString(*fpath)),(O_CREAT | O_RDWR),0666);
	if(fd == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
		return(NhlFATAL);
	}

	i = 0;
	cur_off = 0;
	if(*recnum != -1) {
		while(i != *recnum + 1) {	
			lseek(fd,cur_off,SEEK_SET);
			n = read(fd,(control_word),4);
			if(n != 4) {
/*
* end of file reached
*/	
				rsize = -1;
				NhlPError(NhlWARNING,NhlEUNKNOWN,"fbinrecwrite: end of file reached before record number, writing record as last record in file");
				break;
			}
			if (! swap_bytes)
				ind1 = *(int*)control_word;
			else
				_NclSwapBytes(&ind1,control_word,1,sizeof(int));
			lseek(fd,cur_off + (off_t)(ind1 + 4),SEEK_SET);
			n = read(fd,(control_word),4);
			if(n != 4) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
				break;
			}
			if (! swap_bytes) 
				ind2 = *(int*)control_word;
			else
				_NclSwapBytes(&ind2,control_word,1,sizeof(int));
			if(ind1 == ind2) {
					i++;
					cur_off += (off_t)(ind1 + 8);
					rsize = ind1;
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
			}
		}
	} else {
		rsize = -1;
		lseek(fd,cur_off,SEEK_END);
	}
	if((rsize == -1)||(rsize== total)){
		if (rsize != -1) {
			/* seek to the beginning of current record */
			cur_off -= (off_t)(rsize + 8);
			lseek(fd,cur_off,SEEK_SET);
		}
		if (swap_bytes) {
			int ltotal;
			char *outdata = NclMalloc(total);
			if (!outdata)
				return (NhlFATAL);
			_NclSwapBytes(outdata,value,total / type->type_class.size,type->type_class.size);
			_NclSwapBytes(&ltotal,&total,1,4);
			n = write(fd,&ltotal,4);
			n = write(fd,outdata,total);
			n = write(fd,&ltotal,4);
			close(fd);
			NclFree(outdata);
			return(NhlNOERROR);
		}
		else {
			n = write(fd,&total,4);
			n = write(fd,value,total);
			n = write(fd,&total,4);
			close(fd);
			return(NhlNOERROR);
		}
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
	string *fpath;
	int	*recnum;
	int	*dimensions;
	int	dimsize;
	string *type;
	NclScalar missing;
	NclMultiDValData tmp_md;
	NclStackEntry data;
	int 	has_missing = 0;
	NclTypeClass thetype;
	char 	control_word[4];
	void *value;
	int i;
	int ind1,ind2;
	int fd = -1;
	int size = 1;
	int n;
	off_t cur_off = 0;
	NhlErrorTypes ret = NhlNOERROR;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

	control_word[0] = (char)0;
	control_word[1] = (char)0;
	
	fpath = (string*)NclGetArgValue(
		0,
		4,
		NULL,
		NULL,
		&missing,
		&has_missing,
		NULL,
		0);
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
	
	dimensions = (int*)NclGetArgValue(
		2,
		4,
		NULL,
		&dimsize,
		&missing,
		&has_missing,
		NULL,
		0);

	if(*dimensions!= -1) {
		for(i = 0; i < 	dimsize; i++) {
			if(has_missing&&(missing.intval == *(dimensions + i))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: dimension size contains a missing value, can't continue");
				return(NhlFATAL);
			}
			size *= dimensions[i];
		}
	} else {
		size = -1;
	}
	type = (string*)NclGetArgValue(
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
	fd = open(_NGResolvePath(NrmQuarkToString(*fpath)),O_RDONLY);

	if(fd == -1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: could not open (%s) check path and permissions, can't continue",NrmQuarkToString(*fpath));
		return(NhlFATAL);
	}

	cur_off = 0;
	i = 0;
	while(i != *recnum) {	
		lseek(fd,cur_off,SEEK_SET);
		n = read(fd,(control_word),4);
		if(n != 4) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if (! swap_bytes)
			ind1 = *(int*)control_word;
		else
			_NclSwapBytes(&ind1,control_word,1,sizeof(int));
		if(ind1 <= 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIfbinrecread: 0 or less than zero fortran control word, FILE NOT SEQUENTIAL ACCESS!");
			close(fd);
			return(NhlFATAL);
		}
		lseek(fd,cur_off + (off_t)(ind1 + 4),SEEK_SET);
		n = read(fd,(control_word),4);
		if(n != 4) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if (! swap_bytes) 
			ind2 = *(int*)control_word;
		else
			_NclSwapBytes(&ind2,control_word,1,sizeof(int));
		if(ind1 == ind2) {
			i++;
			cur_off += (off_t)(ind1 + 8);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
			close(fd);
			return(NhlFATAL);
		}
	}
	if(i == *recnum) {
		lseek(fd,cur_off,SEEK_SET);
		n = read(fd,(control_word),4);
		if(n != 4) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if (! swap_bytes)
			ind1 = *(int*)control_word;
		else
			_NclSwapBytes(&ind1,control_word,1,sizeof(int));
		if(size != -1) {
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
		close(fd);
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
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	int dim2;
	int fd;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;
	int control_word;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_READ_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#endif

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
	fd = open(path_string,O_RDONLY);
	if(fd == -1) {
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
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = (int*)tmp_md->multidval.val;
	}
	if((n_dimensions ==1)&&(dimsizes[0] ==-1)){
		size = -1;
		dimsizes = &dim2;
		n_dimensions = 1;
	} else {
		for(i = 0; i < n_dimensions; i++) {
			size *= dimsizes[i];
		}
	}
	if(read(fd,(void*)&control_word,4) != 4) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIfbinread: An error occurred while reading the file (%s), check path",
			  _NGResolvePath(path_string));
		return(NhlFATAL);
	}
	if (swap_bytes) {
		_NclSwapBytes(NULL,&control_word,1,sizeof(int));
	}
	if(control_word <= 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"_NclIfbinread: 0 or less than zero fortran control word, FILE NOT SEQUENTIAL ACCESS!");
		close(fd);
		return(NhlFATAL);
	}
	if(size == -1) {
		size = control_word;
		n_dimensions = 1;
		*dimsizes= size/thetype->type_class.size;
		totalsize = size;
	}
	else {
		totalsize = size*thetype->type_class.size;
		if (totalsize > control_word) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,
				  "_NclIfbinread: requested variable size exceeds record size");
			close(fd);
			return(NhlFATAL);
		}
	}
	if(tmp_ptr != NULL) {
		tmp_ptr = NclMalloc(totalsize);
		if (! tmp_ptr) {
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return( NhlFATAL);
		}
		lseek(fd,(off_t)4,SEEK_SET); /* skip the control word */
		n = read(fd,tmp_ptr,totalsize);
		if(n != totalsize)  {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: an error occurred reading the FORTRAN binary file.");
			NclFree(tmp_ptr);
			close(fd);
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
		close(fd);
		_NclPlaceReturn(data_out);
		return(ret);
	} 
	close(fd);
	return(NhlFATAL);
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
	NclStackEntry type;
	NclTypeClass thetype;
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	FILE *fd = NULL;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
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
	if(tmp_md != NULL) {
		tmp_ptr = tmp_md->multidval.val;
		thetype = tmp_md->multidval.type;
		totalsize = tmp_md->multidval.totalelements * thetype->type_class.size;
	}
	if(is_stdout) {
		fd = stdout;
	} else {
		errno = 0;
		fd = fopen(path_string,"w+");
		if (fd == NULL && errno) {
			NhlPError(NhlFATAL,errno,"asciiwrite: Unable to open file for writing");
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
	char *cp,*iend;
	
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
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i,j;
	void *tmp_ptr;
	struct stat statbuf;
	FILE *fp = NULL;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	int has_unlimited = 0;
	int bufsize = 4096;
	char buf[4096];
	int total = 0;
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
	if(tmp_md != NULL) {
		n_dimensions = tmp_md->multidval.totalelements;
		dimsizes = (int*)tmp_md->multidval.val;
	}
	if(dimsizes[0] == -1) {
		if(n_dimensions == 1) {
			size = -1;
		} else {
			for(i = 1; i < n_dimensions; i++) {
				size *= dimsizes[i];
			}
		}
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

	if((size != -1)&&(!has_unlimited)) {
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
							*step = fgetc(fp);
							if(*step == '\n') {
								*step = '\0';
								*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
								step = buffer;
								tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
								total++;
								break;
							} else {
								step++;
							}
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
			return(ret);
		}
	} else if(size == -1) {
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
			totalsize = 0;
			while(!feof(fp)) {
				if(fgetc(fp) == '\n') {
					totalsize++;
				} 
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
			NULL,
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
						*step = fgetc(fp);
						if(*step == '\n') {
							*step = '\0';
							*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
							step = buffer;
							tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
							total++;
							break;
						} else {
							step++;
						}
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
		return(ret);
	} else {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Dimension size less than 1 specified, can't determine size");
		return(NhlFATAL);
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
	NclStackEntry type;
	NclTypeClass thetype;
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
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
		fd = open(path_string,(O_CREAT | O_RDWR),0666);
		if(fd >= 0) {
			lseek(fd,0,SEEK_END);
			if (swap_bytes) {
				char *outdata = NclMalloc(totalsize);
				if (!outdata)
					return (NhlFATAL);
				_NclSwapBytes(outdata,tmp_ptr,tmp_md->multidval.totalelements,thetype->type_class.size);
				n = write(fd, outdata,totalsize);
				NclFree(outdata);
			}
			else {
				n = write(fd, tmp_ptr,totalsize);
			}
			close(fd);
			return(ret);
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
	NclStackEntry type;
	NclTypeClass thetype;
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
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
			fd = open(path_string,(O_CREAT | O_RDWR),0666);
			if(fd >= 0) {
				n = write(fd,outdata,totalsize);
				close(fd);
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinwrite: Could not create file");
				ret = NhlFATAL;
			}
			NclFree(outdata);
			return(ret);
		}
		else {
			fd = open(path_string,(O_CREAT | O_RDWR),0666);
			if(fd >= 0) {
				n = write(fd, tmp_ptr,totalsize);
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
	NclStackEntry type;
	NclTypeClass thetype;
	char *typechar = NULL;
	NclMultiDValData tmp_md= NULL;
	Const char *path_string;
	int n_dimensions = 0;
	int *dimsizes = NULL;
	int size = 1;
	int i;
	void *tmp_ptr;
	struct stat buf;
	int fd = -1;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	NclFileClassPart *fcp = &(nclFileClassRec.file_class);
	int swap_bytes = 0;

	ret = _NclInitClass(nclFileClass);
	if (ret < NhlWARNING)
		return ret;
#ifdef ByteSwapped
	if (NrmStringToQuark("bigendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
		swap_bytes = 1;
	}
#else
	if (NrmStringToQuark("littleendian") == *(string *)(fcp->options[Ncl_WRITE_BYTE_ORDER].value->multidval.val)) {
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
	if(tmp_md == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinwrite: error retrieving the file path");
		return(NhlFATAL);
	}
	path_string = _NGResolvePath(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
	if(path_string == NULL) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinwrite: An error in the file path was detected could not resolve file path");
		return(NhlFATAL);
	}
	fd = open(path_string,(O_CREAT | O_RDWR),0666);
	if(fd == -1) {
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
	if (swap_bytes) {
		char *outdata = NclMalloc(totalsize);
		int ltotal;
		if (!outdata) {
			NhlPError(NhlFATAL,ENOMEM,NULL);
			return (NhlFATAL);
		}
		_NclSwapBytes(outdata,tmp_ptr,tmp_md->multidval.totalelements,thetype->type_class.size);
		_NclSwapBytes(&ltotal,&totalsize,1,4);
		n = write(fd,&ltotal,4);
		n = write(fd,outdata,totalsize);
		n = write(fd,&ltotal,4);
#if 0
		NGCALLF(nclpfortranwrite,NCLPFORTRANWRITE)(path_string,outdata,&totalsize,&ret,strlen(path_string));
#endif
		NclFree(outdata);
	}
	else {
#if 0
		NGCALLF(nclpfortranwrite,NCLPFORTRANWRITE)(path_string,tmp_ptr,&totalsize,&ret,strlen(path_string));
#endif
		n = write(fd,&totalsize,4);
		n = write(fd,tmp_ptr,totalsize);
		n = write(fd,&totalsize,4);
	}
	close(fd);
	return(ret);
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
	NclStackEntry data_out;
	NclMultiDValData tmp_md= NULL;
	int tmp ;
	int dimsize = 1;

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
   int has_missing,
	   n_dims,
	   dimsizes[NCL_MAX_DIMENSIONS];

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
    int total = 1;
    int i;


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
            llout_val = (long long *) out_val;
            if (has_missing) {
                for (i = 0; i < total; i++) {
                    if (llvalue[i] != missing.int64val) {
                        llout_val[i] = (long long) llabs((long long) llvalue[i]);
                    } else {
                        llout_val[i] = missing.int64val;
                    }
                }
            } else {
                for (i = 0; i < total; i++) {
                    llout_val[i] = (long long) llabs((long long) llvalue[i]);
                }
            }

            return NclReturnValue(out_val, n_dims, dimsizes,
                    (has_missing ? &missing : NULL), NCL_int64, 0);
            break;

        case NCL_byte:
            bvalue = (byte *) value;
            out_val = (void *) NclMalloc(total * sizeof(float));
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

        default:
            NhlPError(NhlFATAL, NhlEUNKNOWN,
                "abs: a non-numeric type was passed to this function, cannot continue");
            break;
    }
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
	string outval;
	int dimsize = 1;


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
	str = NrmQuarkToString(*(string*)tmp_md->multidval.val);
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
    string  outval;
    int dimsize = 1;


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
    str = NrmQuarkToString(*(string *) tmp_md->multidval.val);
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

static long _Nclstrtol 
#if     NhlNeedProto
(
	const char *str, 
	char **endptr
)
#else
(str,endptr)
const char *str;
char **endptr;
#endif
{

	long tval;
	int i = 0;

	while (isspace(str[i]))
			i++;
	if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
		tval = strtol(str,endptr,16);
	else
		tval = strtol(str,endptr,10);
	return tval;
}

static unsigned long _Nclstrtoul
#if     NhlNeedProto
(
        const char *str,
        char **endptr
)
#else
(str,endptr)
const char *str;
char **endptr;
#endif
{
        unsigned long tval;
        int i = 0;

        while (isspace(str[i]))
                        i++;
        if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
        {
                tval = strtoul(str,endptr,16);
        }
        else
        {
                tval = strtoul(str,endptr,10);
        }

        return tval;
}


static long long _Nclstrtoll
#if     NhlNeedProto
(
        const char *str,
        char **endptr
)
#else
(str,endptr)
const char *str;
char **endptr;
#endif
{
        long long tval;
        int i = 0;

        while (isspace(str[i]))
                        i++;
        if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
        {
                tval = local_strtoll(str,endptr,16);
        }
        else
        {
                tval = local_strtoll(str,endptr,10);
        }

        return tval;
}

static unsigned long long _Nclstrtoull
#if     NhlNeedProto
(
        const char *str,
        char **endptr
)
#else
(str,endptr)
const char *str;
char **endptr;
#endif
{
        unsigned long long tval;
        int i = 0;

        while (isspace(str[i]))
                        i++;
        if (strlen(&(str[i])) >= 2 && str[i] == '0' && (str[i+1] == 'x' || str[i+1] == 'X'))
        {
                tval = strtoull(str,endptr,16);
        }
        else
        {
                tval = strtoull(str,endptr,10);
        }

        return tval;
}


NhlErrorTypes _NclIushorttoint
#if	NhlNeedProto
(void)
#else
()
#endif
{
	unsigned short *value;
	int total_elements = 1;
	int n_dims = 0;
	int dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing, missing2;
	int has_missing;
	int i;
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
	output = (int*)NclMalloc(sizeof(int)*total_elements);
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        int *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        int *value;
        int total=1;
        int i;

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
		if (missing.intval < 0 || missing.intval > UCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.intval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.intval)) {
				out_val[i] = (byte)missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        int *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        short *value;
        int total=1;
        int i;

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
		if (missing.shortval < 0 || missing.shortval > UCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.shortval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.shortval)) {
				out_val[i] = (byte)  missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}

	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;  
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        short *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        long *value;
        int total=1;
        int i;


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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        long *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        long *value;
        int total=1;
        int i;

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
		if (missing.longval < 0 || missing.longval > UCHAR_MAX) {
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
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        long *value;
        int total=1;
        int i;

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
			missing2.charval = (int)missing.longval;
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;


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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;


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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;

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
		if (missing.floatval < (float)0 || missing.floatval > (float)UCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.floatval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.floatval)) {
				out_val[i] = missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        float *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        byte *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;

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
		if (missing.doubleval < (double)0 || missing.doubleval > (double)UCHAR_MAX) {
		        missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		}
		else {
			missing2.byteval = (byte)missing.doubleval;
		}
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.doubleval)) {
				out_val[i] = missing2.byteval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
	} else {
		missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > UCHAR_MAX)) {
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        unsigned char *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;

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
			if((value[i] < 0)||(value[i] > UCHAR_MAX)||(value[i] == missing.doubleval)) {
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

NhlErrorTypes _NclIchartodouble
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        double *out_val;
	NclBasicDataTypes type;
        unsigned char *value;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;


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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;


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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;


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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        float *out_val;
	NclBasicDataTypes type;
        double *value;
	double dtmp;
        int total=1;
        int i;

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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        string *value;
        int total=1;
        int i;
	long tval;
	char *val;
	char *end;

        value = (string*)NclGetArgValue(
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
					"A bad value was passed to stringtolong, input strings must contain numeric digits, replacing with missing value");
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
                                "A bad value was passed to stringtolong, input strings must contain numeric digits, replacing with missing value");
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
    int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
    unsigned long *out_val;
    NclBasicDataTypes type;
    string *value;
    int total=1;
    int i;
    unsigned long tval;
    char *val;
    char *end;

    value = (string*)NclGetArgValue(
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
                    "A bad value was passed to stringtoulong, input strings must contain numeric digits, replacing with missing value");
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
                "A bad value was passed to stringtoulong, input strings must contain numeric digits, replacing with missing value");
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
    int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
    long long *out_val;
    NclBasicDataTypes type;
    string *value;
    int total=1;
    int i;
    long long tval;
    char *val;
    char *end;

    value = (string*)NclGetArgValue(
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
        errno = 0;
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
                errno = 0;
                val = NrmQuarkToString(value[i]);
                tval = _Nclstrtoll(val,&end);
                if (end == val)
                {
                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                    "A bad value was passed to stringtoint64, input strings must contain numeric digits, replacing with missing value");
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
            errno = 0;
            val = NrmQuarkToString(value[i]);
            tval = _Nclstrtoll(val,&end);
            if (end == val)
            {
                NhlPError(NhlFATAL,NhlEUNKNOWN,
                "A bad value was passed to stringtoint64, input strings must contain numeric digits, replacing with missing value");
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
    int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
    unsigned long long *out_val;
    NclBasicDataTypes type;
    string *value;
    int total=1;
    int i;
    unsigned long long tval;
    char *val;
    char *end;

    value = (string*)NclGetArgValue(
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
                    "A bad value was passed to stringtouint64, input strings must contain numeric digits, replacing with missing value");
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
                "A bad value was passed to stringtouint64, input strings must contain numeric digits, replacing with missing value");
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        string *value;
        int total=1;
        int i;
	long tval;
	char *val;
	char *end;

        value = (string*)NclGetArgValue(
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
                                        "A bad value was passed to stringtoshort, input strings must contain numeric digits, replacing with missing value");
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
                                "A bad value was passed to stringtoshort, input strings must contain numeric digits, replacing with missing value");
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        unsigned short *out_val;
	NclBasicDataTypes type;
        string *value;
        int total=1;
        int i;
	long tval;
	char *val;
	char *end;

        value = (string*)NclGetArgValue(
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
                                        "A bad value was passed to stringtoushort, input strings must contain numeric digits, replacing with missing value");
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
                                "A bad value was passed to stringtoushort, input strings must contain numeric digits, replacing with missing value");
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        string *value;
        int total=1;
        int i;
	long tval;
	char *val;
	char *end;

        value = (string*)NclGetArgValue(
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
                                        "A bad value was passed to stringtointeger, input strings must contain numeric digits, replacing with missing value");
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
                                "A bad value was passed to stringtointeger, input strings must contain numeric digits, replacing with missing value");
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
    int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
    unsigned int *out_val;
    NclBasicDataTypes type;
    string *value;
    int total=1;
    int i;
    unsigned int tval;
    char *val;
    char *end;

    value = (string*)NclGetArgValue(
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
                    "A bad value was passed to stringtouint, input strings must contain numeric digits, replacing with missing value");
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
                "A bad value was passed to stringtouint, input strings must contain numeric digits, replacing with missing value");
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        double *out_val;
	NclBasicDataTypes type;
        string *value;
        int total=1;
        int i;
	double tval;
	char tbuf[128];
	char *val;
	char *end;
	int bufsiz = 128;

        value = (string*)NclGetArgValue(
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
					"A bad value was passed to stringtodouble, input strings must contain numeric digits, replacing with missing value");
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
                                "A bad value was passed to stringtodouble, input strings must contain numeric digits, replacing with missing value");
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
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        float *out_val;
	NclBasicDataTypes type;
        string *value;
        int total=1;
        int i;
	double tval,dtest;
	char *val;
	char *end;

        value = (string*)NclGetArgValue(
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
					"A bad value was passed to stringtofloat, input strings must contain numeric digits, replacing with missing value");
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
					  "A bad value was passed to stringtofloat, input strings must contain numeric digits, replacing with missing value");
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
	int i;
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
	int i;

	data = _NclGetArg(0, 1, DONT_CARE);
	switch (data.kind) {
        case NclStk_VAR:
		tmp_md = _NclVarValueRead(data.u.data_var, NULL, NULL);
		break;

        case NclStk_VAL:
		tmp_md = (NclMultiDValData) data.u.data_obj;
		break;
	}

	if (tmp_md == NULL)
		return NhlFATAL;

	exit_status = *(int *) tmp_md->multidval.val;

	_NclExit(exit_status);

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
	int i;
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
	int i;
	logical *outval;
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
	int i;
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
	int i;
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
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,att_md;
	int i;
	logical *outval;
	NclVar tmp_var;
	NclQuark *vals;
	NclSymbol* s;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		att_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		att_md = arg2.u.data_obj;
		break;
	}
	
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIsCoord: Non variable passed returning missing");
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
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,att_md;
	int i;
	logical *outval;
	NclVar tmp_var;
	NclQuark *vals;
	NclSymbol* s;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		att_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		att_md = arg2.u.data_obj;
		break;
	}
	
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsAtt: Non variable passed returning missing");
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
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,dim_md;
	int i;
	logical *outval;
	NclVar tmp_var;
	NclQuark *vals;
	NclSymbol* s;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
	}
	
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsDim: Non variable passed returning missing");
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
	NclStackEntry arg0,arg1,arg2;
	NclMultiDValData tmp_md,dim_md;
	int i;
	logical *outval;
	NclVar tmp_var;
	int *vals;
	NclSymbol* s;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dimsize = 1;
	int get_all = 0;

	
	arg1  = _NclGetArg(0,2,DONT_CARE);
	arg2  = _NclGetArg(1,2,DONT_CARE);

	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_var = arg1.u.data_var;
		break;
	case NclStk_VAL:
		tmp_var = NULL;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
	}
	

	if(tmp_var == NULL) {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsDimNamed: Non variable passed returning missing");
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
		return(NclReturnValue(
			(void*)outval,
			1,
			&tmp_var->var.n_dims,
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
	int i;
	logical *outval;
	NclQuark *vals;
	NclSymbol* s;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
	arg0  = _NclGetArg(0,2,DONT_CARE);
	arg1  = _NclGetArg(1,2,DONT_CARE);
	switch(arg0.kind) {
	case NclStk_VAR:
		file_md = _NclVarValueRead(arg0.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		file_md = arg0.u.data_obj;
		break;
	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
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
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsFileVar: undefined file returning missing value");
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
	int i;
	logical *outval;
	NclQuark var;
	NclQuark *vals;
	NclSymbol* s;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
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
	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		att_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		att_md = arg2.u.data_obj;
		break;
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
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsFileVar: undefined file returning missing value");
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
	int i;
	logical *outval;
	NclQuark var;
	NclQuark *vals;
	NclSymbol* s;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
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
	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
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
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsFileVar: undefined file returning missing value");
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
	int i;
	logical *outval;
	NclQuark var;
	NclQuark *vals;
	NclSymbol* s;
	NclFile file_ptr;
	logical miss = ((NclTypeClass)nclTypelogicalClass)->type_class.default_mis.logicalval;
	int dims = 1;

	
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
	}
	switch(arg1.kind) {
	case NclStk_VAR:
		tmp_md = _NclVarValueRead(arg1.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		tmp_md = arg1.u.data_obj;
		break;
	}
	switch(arg2.kind) {
	case NclStk_VAR:
		dim_md = _NclVarValueRead(arg2.u.data_var,NULL,NULL);
		break;
	case NclStk_VAL:
		dim_md = arg2.u.data_obj;
		break;
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
		NhlPError(NhlWARNING,NhlEUNKNOWN,"_NclIIsFileVar: undefined file returning missing value");
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
	NclMultiDValData tmp_dims= NULL;
	void *out_val;
	int *dimsizes;
	logical *tmp;
	int i;
	int sz;

	data = _NclGetArg(0,2,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	}
	if(tmp_dims == NULL)
		return(NhlFATAL);


	dimsizes = (int*)tmp_dims->multidval.val;	

	sz = 1;	
	for(i = 0; i < tmp_dims->multidval.totalelements; i++) {
		sz = sz * dimsizes[i];
	}

	if((sz == tmp_md->multidval.totalelements)||(sz < tmp_md->multidval.totalelements)) {
		if(sz < tmp_md->multidval.totalelements) {
			NhlPError(NhlWARNING, NhlEUNKNOWN,"onedtond : output dimension sizes have fewer elements than input, some data not copied");
		}
		out_val = (void*)NclMalloc(sz*tmp_md->multidval.type->type_class.size);
		memcpy(out_val,tmp_md->multidval.val,sz*tmp_md->multidval.type->type_class.size);
		return(NclReturnValue(
			out_val,
			tmp_dims->multidval.totalelements,
			dimsizes,
			tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
			tmp_md->multidval.type->type_class.data_type,
			0
		));
	} else if((sz > tmp_md->multidval.totalelements)&&(sz%tmp_md->multidval.totalelements)){
		NhlPError(NhlWARNING, NhlEUNKNOWN,"onedtond : output dimension sizes not even multiples of input, check output");
		out_val = (void*)NclMalloc(sz*tmp_md->multidval.type->type_class.size);
		for(i = 0; i < (int)sz/tmp_md->multidval.totalelements; i++) {
			memcpy(&(((char*)out_val)[i*tmp_md->multidval.totalsize]),
				tmp_md->multidval.val,
				tmp_md->multidval.totalsize);
		}
		memcpy(&(((char*)out_val)[i*tmp_md->multidval.totalsize]),
			tmp_md->multidval.val,
			(sz%tmp_md->multidval.totalelements)*tmp_md->multidval.type->type_class.size);

		return(NclReturnValue(
			out_val,
			tmp_dims->multidval.totalelements,
			dimsizes,
			tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
			tmp_md->multidval.type->type_class.data_type,
			0
		));
	} else { /* (sz > tmp_md->multidval.totalelements)&&!(sz%tmp_md->multidval.totalelements)) */
		out_val = (void*)NclMalloc(sz*tmp_md->multidval.type->type_class.size);
		for(i = 0; i < sz/tmp_md->multidval.totalelements; i++) {
			memcpy(&(((char*)out_val)[i*tmp_md->multidval.totalsize]),
				tmp_md->multidval.val,
				tmp_md->multidval.totalsize);
		}
		return(NclReturnValue(
			out_val,
			tmp_dims->multidval.totalelements,
			dimsizes,
			tmp_md->multidval.missing_value.has_missing ? &(tmp_md->multidval.missing_value.value):NULL,
			tmp_md->multidval.type->type_class.data_type,
			0
		));
	}
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
	int dimsizes = 0;
	logical *tmp;
	int i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;
	logical *tmp;
	int i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		i = 0;
		while((tmp[i])&&(i<tmp_md->multidval.totalelements)) {
			i++;
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j;
	int m,n,sz;
	int nd;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
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
			while((tmp[j])&&(j<m)) {
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
        int *dims, ndims;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,k;
	int i_in_sz,i_out_sz;
	int m,n,nr,nl,sz;
	int nd;
	NclScalar missing;

/*
 * Get dimensions to do product across.
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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
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
	      while((tmp[k])&&(k<m)) {
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j;
	int m,n,sz;
	int nd;
	NclScalar missing;


	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
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
			while((tmp[j])&&(j<m)) {
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
        int *dims, ndims;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,k;
	int i_in_sz,i_out_sz;
	int m,n,nr,nl,sz;
	int nd;
	NclScalar missing;

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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
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
	      while((tmp[k])&&(k<m)) {
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
	int dimsizes = 1;
	logical *tmp = NULL;
	int i;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		out_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		i = 0;
		while((tmp[i])&&(i<tmp_md->multidval.totalelements)) {
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
	int i,j;
	int m,n,sz;
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
			int goffset;
			int dim_offset = i * m * sz;
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
					int last_offset = dim_offset + (j-1) * sz;
					int offset = dim_offset + j * sz;
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
					int offset = dim_offset + j * sz;
					memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),sz);
				}
				if (j < m) {
					goffset = dim_offset + j * sz;
					memcpy((char*)out_val + goffset,(char*)(tmp_md->multidval.val) + goffset,sz);
				}
				for(j++; j < m; j++) {
					int offset = dim_offset + j * sz;
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
					int offset = dim_offset + j * sz;
					memset((char*)out_val + offset,0,sz);
				}
				if (j == 0) {
					memcpy((char*)out_val + dim_offset,(char*)tmp_md->multidval.val + dim_offset,sz);
					j++;
				}
				for(; j < m; j++) {
					int last_offset = dim_offset + (j-1) * sz;
					int offset = dim_offset + j * sz;
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
			int dim_offset = i * m * sz;
			memcpy((char*)out_val + dim_offset,(char*)tmp_md->multidval.val + dim_offset,sz);
			for(j = 1; j < m; j++) {
				int last_offset = dim_offset + (j-1) * sz;
				int offset = dim_offset + j * sz;
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
	int *dims, ndims;
	logical *tmp = NULL;
	int i,j,k;
	int sz;
	int m,n,nl,nr;
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
	n = nr * nl;

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
	      int goffset;
	      int missing_flag = 0;
	      int dim_offset = ((i*nr*m)+j)*sz;
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
		  int last_offset = dim_offset + (nr*(k-1))*sz;
		  int offset = dim_offset + (nr*k)*sz;
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
		  int offset = dim_offset + (nr*k)*sz;
		  memcpy((char*)out_val + offset,
			 &(tmp_md->multidval.missing_value.value),sz);
		}
		if (k < m) {
		  goffset = dim_offset + (nr*k)*sz;
		  memcpy((char*)out_val + goffset,
			 (char*)(tmp_md->multidval.val) + goffset,sz);
		}
		for(k++; k < m; k++) {
		  int offset = dim_offset + (nr*k)*sz;
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
		  int offset = dim_offset + (nr*k)*sz;
		  memset((char*)out_val + offset,0,sz);
		}
		if (k == 0) {
		  memcpy((char*)out_val + dim_offset,
			 (char*)tmp_md->multidval.val + dim_offset,sz);
		  k++;
		}
		for(; k < m; k++) {
		  int last_offset = dim_offset + (nr*(k-1))*sz;
		  int offset = dim_offset + (k*nr)*sz;
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
	      int dim_offset = ((i*nr*m)+j)*sz;
	      memcpy((char*)out_val + dim_offset,
		     (char*)tmp_md->multidval.val + dim_offset,sz);
	      for(k = 1; k < m; k++) {
		int last_offset = dim_offset + (nr*(k-1))*sz;
		int offset = dim_offset + (nr*k)*sz;
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
	int i,missing_flag = 0;
	int goffset;

	data0 = _NclGetArg(0,2,DONT_CARE);
	switch(data0.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data0.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data0.u.data_obj;
			break;
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
				int last_offset = (i-1) *  tmp_md->multidval.type->type_class.size;
				int offset = i * tmp_md->multidval.type->type_class.size;
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
				int offset = i * tmp_md->multidval.type->type_class.size;
				memcpy((char*)out_val + offset,&(tmp_md->multidval.missing_value.value),
				       tmp_md->multidval.type->type_class.size);
				i++;
			}
			if (i < tmp_md->multidval.totalelements) {
				goffset = i * tmp_md->multidval.type->type_class.size;
				memcpy((char*)out_val + goffset,(char*)(tmp_md->multidval.val) + goffset,tmp_md->multidval.type->type_class.size);
			}
			for(i++; i < tmp_md->multidval.totalelements; i++) {
				int offset = i * tmp_md->multidval.type->type_class.size;
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
				int offset = i * tmp_md->multidval.type->type_class.size;
				memset((char*)out_val + offset,0,tmp_md->multidval.type->type_class.size);
				i++;
			}
			if (i == 0) {
				memcpy(out_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
				i++;
			}
			for(; i < tmp_md->multidval.totalelements; i++) {
				int last_offset = (i-1) *  tmp_md->multidval.type->type_class.size;
				int offset = i * tmp_md->multidval.type->type_class.size;
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
			int last_offset = (i-1) *  tmp_md->multidval.type->type_class.size;
			int offset = i * tmp_md->multidval.type->type_class.size;
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,sf;
	int m,n,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

/*
 * Calculate size of all but rightmost dimension (n).
 * The rightmost dimension is "m". The number of dimenions is "nd".
 */
	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
/*
 * Determine output type, which will either be float or double.
 */
	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
		sz = tmp_md->multidval.type->type_class.size;
		out_val = (void*)NclMalloc(sizeof(double)* n);
		out_data_type = NCL_double;
		sf = sizeof(double);
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		sf = sizeof(float);
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg: Could not coerce input data to double, can't continue");
			return(NhlFATAL);
		} else if(tmp_md->multidval.missing_value.has_missing) {

			missing = tmp_md->multidval.missing_value.value;
		}
		did_coerce = 1;
		sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
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
	int *dims, ndims;
	double sum_val ;
	double *val = NULL;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,k,sf;
	int m,n,nr,nl,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
	NclScalar missing;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * Determine output type, which will either be float or double.
 */
	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
	  sz = tmp_md->multidval.type->type_class.size;
	  out_val = (void*)NclMalloc(sizeof(double)* n);
	  out_data_type = NCL_double;
	  sf = sizeof(double);
	  if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	} else {
	  out_val = (void*)NclMalloc(sizeof(float)* n);
	  sf = sizeof(float);
	  out_data_type = NCL_float;
	  tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp_md == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg_n: Could not coerce input data to double, can't continue");
	    return(NhlFATAL);
	  } else if(tmp_md->multidval.missing_value.has_missing) {
	    
	    missing = tmp_md->multidval.missing_value.value;
	  }
	  did_coerce = 1;
	  sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,sf;
	int m,n,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
		sz = tmp_md->multidval.type->type_class.size;
		out_val = (void*)NclMalloc(sizeof(double)* n);
		out_data_type = NCL_double;
		sf = sizeof(double);
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		sf = sizeof(float);
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_variance: Could not coerce input data to double, can't continue");
			return(NhlFATAL);
		} else if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
		did_coerce = 1;
		sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
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
	int *dims, ndims;
	double sum_val ;
	double sum_sqrd_val ;
	double *val = NULL;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,k,sf,i_in,i_out;
	int m,n,nr,nl,sz;
	int nd,count;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * Determine output type, which will either be float or double.
 */
	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
	  sz = tmp_md->multidval.type->type_class.size;
	  out_val = (void*)NclMalloc(sizeof(double)* n);
	  out_data_type = NCL_double;
	  sf = sizeof(double);
	  if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	} else {
	  out_val = (void*)NclMalloc(sizeof(float)* n);
	  sf = sizeof(float);
	  out_data_type = NCL_float;
	  tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp_md == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_variance_n: Could not coerce input data to double, can't continue");
	    return(NhlFATAL);
	  } else if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	  did_coerce = 1;
	  sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
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
	double out0_val;
	double sum_sqrd_val;
	double tmp_sqrd_val;
	double *val;
	void *out1_val;
	double div_val;
	double done = 1.0;
	float fone = 1.0;
	int dimsizes = 1;
	int i,n;
	short tmp1;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
	NhlErrorTypes r0 = NhlNOERROR;
	NhlErrorTypes r1 = NhlNOERROR;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,sf;
	int m,n,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
		*dimsizes = n; 	
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = 1;
	}
	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
		sz = tmp_md->multidval.type->type_class.size;
		out_val = (void*)NclMalloc(sizeof(double)* n);
		out_data_type = NCL_double;
		sf = sizeof(double);
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		sf = sizeof(float);
		out_data_type = NCL_float;
		tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	
		if(tmp_md == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev: Could not coerce input data to double, can't continue");
			return(NhlFATAL);
		} else if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
		sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
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
        int *dims, ndims;
	double sum_val ;
	double sum_sqrd_val ;
	double *val = NULL;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,k,sf,i_in,i_out;
	int m,n,nr,nl,sz;
	int nd,count;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
	  *dimsizes = 1;
	  nd = 1;
	  m  = tmp_md->multidval.dim_sizes[dims[0]];
	}
	n = nr * nl;
/*
 * Determine output type, which will either be float or double.
 */
	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
	if(tmp_md->multidval.data_type == NCL_double) {
	  sz = tmp_md->multidval.type->type_class.size;
	  out_val = (void*)NclMalloc(sizeof(double)* n);
	  out_data_type = NCL_double;
	  sf = sizeof(double);
	  if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	} else {
	  out_val = (void*)NclMalloc(sizeof(float)* n);
	  sf = sizeof(float);
	  out_data_type = NCL_float;
	  tmp_md = _NclCoerceData(tmp_md,Ncl_Typedouble,NULL);
	  if(tmp_md == NULL) {
	    NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev_n: Could not coerce input data to double, can't continue");
	    return(NhlFATAL);
	  } else if(tmp_md->multidval.missing_value.has_missing) {
	    missing = tmp_md->multidval.missing_value.value;
	  }
	  did_coerce = 1;
	  sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
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
	double out0_val;
	double sum_sqrd_val;
	double tmp_sqrd_val;
	double *val;
	void *out1_val;
	double div_val;
	double done = 1.0;
	float fone = 1.0;
	int dimsizes = 1;
	int i,n;
	short tmp1;
	NclBasicDataTypes data_type;
	NclBasicDataTypes out_data_type;
	NclTypeClass the_type;
	NhlErrorTypes r0 = NhlNOERROR;
	NhlErrorTypes r1 = NhlNOERROR;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	data_type = NCL_double;
	the_type = (NclTypeClass)nclTypedoubleClass;
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
	double div_val;
	int dimsizes = 1;
	logical *tmp = NULL;
	int i,n;
	short tmp1;
	NclBasicDataTypes data_type;
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
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	data_type = NCL_double;
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
	int dimsizes = 1;
	logical *tmp;
	int i,j,count;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)tmp_md->multidval.val;
		count = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval))
				count++;
		}
		out_val = (void*)NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size);
		memcpy(out_val,&count,((NclTypeClass)nclTypeintClass)->type_class.size);
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			NCL_int,
			0
		));
	} else {
		tmp = (logical*)tmp_md->multidval.val;
		count = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(tmp[i])
				count++;
		}
		out_val = (void*)NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size);
		memcpy(out_val,&count,((NclTypeClass)nclTypeintClass)->type_class.size);
		return(NclReturnValue(
			out_val,
			1,
			&dimsizes,
			NULL,
			NCL_int,
			0
		));
	}

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
	int dimsizes = 1;
	logical *tmp;
	int i,j,count;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)tmp_md->multidval.val;
		count = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval))
				count++;
		}
		if(count > 0) {
			out_val = (void*)NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size * count);
			j = 0;
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				if((tmp[i])&&(tmp[i] != tmp_md->multidval.missing_value.value.logicalval)) {
					((int*)out_val)[j] = i;
					j++;
				}
			}
		} else {
			out_val = (void*)NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size);
			memcpy(out_val,&((NclTypeClass)nclTypeintClass)->type_class.default_mis,((NclTypeClass)nclTypeintClass)->type_class.size);
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
		tmp = (logical*)tmp_md->multidval.val;
		count = 0;
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
			if(tmp[i])
				count++;
		}
		if(count > 0) {
			out_val = (void*)NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size * count);
			j = 0;
			for(i = 0; i < tmp_md->multidval.totalelements; i++) {
				if(tmp[i]) {
					((int*)out_val)[j] = i;
					j++;
				}
			}
		} else {
			out_val = (void*)NclMalloc(((NclTypeClass)nclTypeintClass)->type_class.size);
			memcpy(out_val,&((NclTypeClass)nclTypeintClass)->type_class.default_mis,((NclTypeClass)nclTypeintClass)->type_class.size);
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
		((NclTypeClass)nclTypeintClass)->type_class.data_type,
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
	int *out_val;
	int dimsizes = 1;
	int i;
	int strt;
	int fnsh;
	int spacing;

	data0 = _NclGetArg(0,3,DONT_CARE);
	switch(data0.kind) {
		case NclStk_VAR:
			tmp_md0 = _NclVarValueRead(data0.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md0 = (NclMultiDValData)data0.u.data_obj;
			break;
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
	}
	if(tmp_md2 == NULL)
		return(NhlFATAL);

	if(_NclIsMissing(tmp_md0,tmp_md0->multidval.val)||
	   _NclIsMissing(tmp_md1,tmp_md1->multidval.val)||
	   _NclIsMissing(tmp_md2,tmp_md2->multidval.val)) {

		NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: Missing value detected in input, can't continue");
		return(NhlFATAL);
	}

	
	spacing = *(int*)tmp_md2->multidval.val;
	if(spacing < 1) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: spacing parameter must be positive and non-zero");
		return(NhlFATAL);
	}
	fnsh = *(int*)tmp_md1->multidval.val;
	strt = *(int*)tmp_md0->multidval.val;

	dimsizes  = abs(fnsh-strt)/spacing + 1;

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
		NCL_int,
		0
	));
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
                        data1_type;

	int dimsizes = 1;
	int i;

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
    }

    if (tmp_md1 == NULL)
        return NhlFATAL;

    data2 = _NclGetArg(2, 3, DONT_CARE);
    switch (data2.kind) {
        case NclStk_VAR:
            tmp_md2 = _NclVarValueRead(data2.u.data_var,NULL,NULL);
            break;

        case NclStk_VAL:
            tmp_md2 = (NclMultiDValData)data2.u.data_obj;
            break;
    }

    if (tmp_md2 == NULL)
        return NhlFATAL;

    if (_NclIsMissing(tmp_md0,tmp_md0->multidval.val) ||
	_NclIsMissing(tmp_md1,tmp_md1->multidval.val) ||
	_NclIsMissing(tmp_md2,tmp_md2->multidval.val)) {

        NhlPError(NhlFATAL, NhlEUNKNOWN, "fspan: Missing value detected in input, can't continue");
        return NhlFATAL;
    }

    dimsizes = *(int *) tmp_md2->multidval.val;
    if (dimsizes <= 0) {
        NhlPError(NhlFATAL, NhlEUNKNOWN,
            "fspan: number of elements parameter is less-than-or-equal-to zero, can't continue");
        return NhlFATAL;
    }

    data0_type = tmp_md0->multidval.data_type;
    data1_type = tmp_md1->multidval.data_type;

    if ((data0_type == NCL_double)  ||  (data1_type == NCL_double)) {
        /*
         * promote arguments to type double
         */
        tmp_md0 = _NclCoerceData(tmp_md0, Ncl_Typedouble, NULL);
        tmp_md1 = _NclCoerceData(tmp_md1, Ncl_Typedouble, NULL);

        if (dimsizes > 1) {
        	double  strt,           /* span start */
                    fnsh,           /* span finish */
                    spacing;        /* span interval */

            fnsh = *(double *) tmp_md1->multidval.val;
            strt = *(double *) tmp_md0->multidval.val;

            spacing = (fnsh - strt) / (dimsizes - 1);

            out_val = (void *) NclMalloc(dimsizes * sizeof(double));
            for (i = 0; i < dimsizes; i++) {
                ((double *) out_val)[i] = strt + (i * spacing);
            }

            ((double *) out_val)[0] = strt;
            ((double *) out_val)[dimsizes - 1] = fnsh;
        } else {
            /* dimsizes == 1 */
             out_val = (void *) NclMalloc(sizeof(double));
            ((double *) out_val)[0] =  *(double *) tmp_md0->multidval.val;
        }

        return NclReturnValue(out_val, 1, &dimsizes, NULL, NCL_double, 0);
    } else {
        /*
         * arguments are, or are to be promoted to, type float
         */
        tmp_md0 = _NclCoerceData(tmp_md0, Ncl_Typefloat, NULL);
        tmp_md1 = _NclCoerceData(tmp_md1, Ncl_Typefloat, NULL);

        if (dimsizes > 1) {
        	float   strt,           /* span start */
                    fnsh,           /* span finish */
                    spacing;        /* span interval */

            fnsh = *(float *) tmp_md1->multidval.val;
            strt = *(float *) tmp_md0->multidval.val;

            spacing = (fnsh - strt) / (dimsizes - 1);

            out_val = (void *) NclMalloc(dimsizes * sizeof(float));
            for (i = 0; i < dimsizes; i++) {
                ((float *) out_val)[i] = strt + (i * spacing);
            }

            ((float *) out_val)[0] = strt;
            ((float *) out_val)[dimsizes-1] = fnsh;
        } else {
            /* dimsizes == 1 */
            out_val = (void *) NclMalloc(sizeof(float));
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
	int j,i;
	int nblk = 0 ;
	int diff, n;
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
	int j,i;
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
	}
	data2 = _NclGetArg(2,3,DONT_CARE);
	switch(data2.kind) {
		case NclStk_VAR:
			false_val_md = _NclVarValueRead(data2.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			false_val_md = (NclMultiDValData)data2.u.data_obj;
			break;
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"where: condition variable (parameter 1) dimension mismatch with parameter 2");
			return(NhlFATAL);
		}
		else {
			for(i = 0; i < cond_md->multidval.n_dims; i++) {
				if(cond_md->multidval.dim_sizes[i] != true_val_md->multidval.dim_sizes[i]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "wherefunc: dimension sizes  of parameter 0 and parameter 1 do not match");
					return(NhlFATAL);
				}
			}
		}
	}
	if (false_val_md->multidval.kind != SCALAR) {
		if(cond_md->multidval.n_dims  != false_val_md->multidval.n_dims) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"where: condition variable (parameter 1) dimension mismatch with parameter 2");
			return(NhlFATAL);
		}
		else {
			for(i = 0; i < cond_md->multidval.n_dims; i++) {
				if(cond_md->multidval.dim_sizes[i] != false_val_md->multidval.dim_sizes[i]) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,
						  "wherefunc: dimension sizes  of parameter 0 and parameter 1 do not match");
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
					  "wherefunc: parameter 2 and parameter 2 must be the same types or coercible to each other");
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
			       (val_md->multidval.missing_value.has_missing? &val_md->multidval.missing_value.value:
				&val_md->multidval.type->type_class.default_mis),
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
	int dimsizes = 1;
	void *tmp;
	logical result;
	int i,j;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
			return(NclReturnValue(
				&tmp_md->multidval.type->type_class.default_mis,
				1,
				&dimsizes,
				&tmp_md->multidval.type->type_class.default_mis,
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
	int dimsizes = 1;
	void *tmp;
	logical result;
	int i,j,count;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
			return(NclReturnValue(
				&tmp_md->multidval.type->type_class.default_mis,
				1,
				&dimsizes,
				&tmp_md->multidval.type->type_class.default_mis,
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
	void *out_val;
	int dimsizes = 1;
	void *tmp;
	logical result;
	int i,j;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		tmp= tmp_md->multidval.val;
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&(_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size]))))
			i++;
		if(i == tmp_md->multidval.totalelements) {
			return(NclReturnValue(
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				NCL_int,
				1
			));
		}
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
		return(NclReturnValue(
			&j,
			1,
			&dimsizes,
			NULL,
			NCL_int,
			1
		));
	} else {
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
		return(NclReturnValue(
			&j,
			1,
			&dimsizes,
			NULL,
			NCL_int,
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
	void *out_val;
	int dimsizes = 1;
	void *tmp;
	logical result;
	int i,j,count;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	if(tmp_md->multidval.missing_value.has_missing) {
		i = 0;
		while((i < tmp_md->multidval.totalelements)&&_NclIsMissing(tmp_md,&(((char*)tmp_md->multidval.val)[i* tmp_md->multidval.type->type_class.size])))
			i++;
		if(i == tmp_md->multidval.totalelements) {
			return(NclReturnValue(
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				1,
				&dimsizes,
				&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
				NCL_int,
				1
			));
		}
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
		return(NclReturnValue(
			&j,
			1,
			&dimsizes,
			NULL,
			NCL_int,
			1
		));
	} else {
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
		return(NclReturnValue(
			&j,
			1,
			&dimsizes,
			NULL,
			NCL_int,
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	logical result = 0;
	int i,j;
	int m,n,sz;
	int nd;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
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
			while((tmp[j])&&(j<m)) {
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
        int *dims, ndims;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	logical result = 0;
	int i,j,k;
	int i_in_sz,i_out_sz;
	int m,n,nr,nl,sz;
	int nd;

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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
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
	      while((tmp[k])&&(k<m)) {
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
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j;
	int m,n,sz;
	int nd;
	logical result = 0;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);


	n = 1;
	if(tmp_md->multidval.n_dims > 1) {
		dimsizes = NclMalloc((tmp_md->multidval.n_dims -1) * sizeof(int));
		for(i = 0; i < tmp_md->multidval.n_dims -1 ; i++) {
			n = n* tmp_md->multidval.dim_sizes[i];
			dimsizes[i] = tmp_md->multidval.dim_sizes[i];
		}
		m = tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1];
		nd = tmp_md->multidval.n_dims -1;
	} else {
		dimsizes = NclMalloc(sizeof(int));
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
			while((tmp[j])&&(j<m)) {
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
        int *dims, ndims;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	logical result = 0;
	int i,j,k;
	int i_in_sz,i_out_sz;
	int m,n,nr,nl,sz;
	int nd;

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
	  dimsizes = NclMalloc(nd * sizeof(int));
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
	  dimsizes = NclMalloc(sizeof(int));
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
	      while((tmp[k])&&(k<m)) {
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
        int dimsizes = 1;

        data = _NclGetArg(0,1,DONT_CARE);
        switch(data.kind) {
                case NclStk_VAR:
                        tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
                        break;
                case NclStk_VAL:
                        tmp_md = (NclMultiDValData)data.u.data_obj;
                        break;
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
        int dimsizes = 1;

        data = _NclGetArg(0,1,DONT_CARE);
        switch(data.kind) {
                case NclStk_VAR:
                        tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
                        break;
                case NclStk_VAL:
                        tmp_md = (NclMultiDValData)data.u.data_obj;
                        break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.type->type_class.type & NCL_NUMERIC_TYPE_MASK) {
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);

	out_val = (logical*)NclMalloc(sizeof(logical));
	if(tmp_md->multidval.type->type_class.type & NCL_ENUMERIC_TYPE_MASK) {
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int dimsizes = 1;
	NclObjTypes ot;
	string* var_string;

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
        var_string = (string*)NclGetArgValue(
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
		case Ncl_Typeint :
			*out_val = NrmStringToQuark("integer");
			break;
		case Ncl_Typeshort :
			*out_val = NrmStringToQuark("short");
			break;
		case Ncl_Typebyte :
			*out_val = NrmStringToQuark("byte");
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
	int dimsizes = 1;

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
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
	int * nlat;
	int has_missing;
	NclScalar missing;
	int dimsizes[2];
	int nl;
	double *theta;
	double *wts;
	int lwork= 0;
	double *work = NULL;
	int i,ierror,k;
	double *output;
	double rtod = (double)180.0/(double)3.14159265358979323846;



	nlat = (int*)NclGetArgValue( 0, 1, NULL, NULL, &missing, &has_missing, NULL,DONT_CARE);

	if(has_missing&&(*nlat==missing.intval)) {
		dimsizes[0]= 1;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"gaus: missing value in input cannot compute gaussian vals");
		NclReturnValue(
			nlat,
			1,
			dimsizes,
			&missing,
			NCL_int,
			1);
		return(NhlWARNING);
	}  else if(*nlat <= 0) {
		dimsizes[0]= 1;
		NhlPError(NhlWARNING,NhlEUNKNOWN,"gaus: number of latitudes must be positive");
		NclReturnValue(
			nlat,
			1,
			dimsizes,
			&missing,
			NCL_int,
			1);
		return(NhlWARNING);
	}
	
	nl= 2 * (*nlat) ;
	theta = (double*)NclMalloc(sizeof(double)*nl);
	wts = (double*)NclMalloc(sizeof(double)*nl);
	lwork = 4 * nl*(nl+1)+2;
	work = (double*)NclMalloc(sizeof(double)*lwork);
	NGCALLF(gaqdncl,GAQDNCL)(&nl,theta,wts,work,&lwork,&ierror);
	NclFree(work);
	output = (double*)NclMalloc(sizeof(double)*nl*2);

	for(i = 0; i < nl; i++) {
		output[2*i] = rtod*theta[i] - 90.0;
		output[2*i+1] = wts[i];
	}
	NclFree(wts);
	NclFree(theta);
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
	string name;
	int dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret = NhlNOERROR;
	NclStackEntry val;
	NclVar tmp_var;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData file_md = NULL;
	NclQuark names[2048];



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
	if(tmp_var != NULL ) {
		if(tmp_var->obj.obj_type == Ncl_FileVar) {
			file_md= (NclMultiDValData)_NclVarValueRead(tmp_var,NULL,NULL);
			thefile = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
			data = _NclGetFileInfo2(thefile);
			for (i=0; i < data->u.file->n_dims;i++) {
				names[i] = data->u.file->dim_info[i].dim_quark;
			}
			ret = NclReturnValue((void*)names, 1, &data->u.file->n_dims, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		} else {
			data = _NclGetVarInfo2(tmp_var);
			for (i=0; i < data->u.var->n_dims;i++) {
				names[i] = data->u.var->dim_info[i].dim_quark;
				if(data->u.var->dim_info[i].dim_quark < 0) {
					names[i] = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
					
				}
			}
			ret = NclReturnValue((void*)names, 1, &data->u.var->n_dims, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
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
	string name;
	int dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	NclFile thefile = NULL;
	NclMultiDValData tmp_md = NULL;



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
			ret = NclReturnValue((void*)data->u.var->attnames, 1, &data->u.var->n_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
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
			data = _NclGetFileInfo2(thefile);
		} else {
			data = _NclGetFileInfo(name);
		}
		if((data != NULL)&&(data->u.file->n_atts != 0)) {
			ret = NclReturnValue((void*)data->u.file->attnames, 1, &data->u.file->n_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
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
	string *name;
	string fname;
	NclScalar name_missing;
	int name_has_missing;
	int out_val = -1;
	int dimsizes;
	NclApiDataList *data = NULL;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	int i;
	NclMultiDValData tmp_md = NULL;
	NclFile thefile;



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

        name = (string*)NclGetArgValue(
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
			data = _NclGetFileVarInfo2(thefile,*name);
		} else {
			dimsizes = 1;
			return(NclReturnValue((void*)&((NclTypeClass)nclTypeintClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypeintClass)->type_class.default_mis, ((NclTypeClass)nclTypeintClass)->type_class.data_type, 1));
		}
	} else {
		data = _NclGetFileVarInfo(fname,*name);
	}
	if((data != NULL)&&(data->u.var->n_dims != 0)) {
		for(i = 0; i < data->u.var->n_dims; i++) {
		 	dim_sizes[i] = data->u.var->dim_info[i].dim_size;
		}
		ret = NclReturnValue((void*)dim_sizes, 1, &data->u.var->n_dims, NULL, ((NclTypeClass)nclTypeintClass)->type_class.data_type, 1);
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
	string *name;
	string fname;
	NclScalar name_missing;
	int name_has_missing;
	string out_val = -1;
	int dimsizes;
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

        name = (string*)NclGetArgValue(
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
		ret = NclReturnValue((void*)dim_names, 1, &data->u.var->n_dims, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
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
	string *name;
	string fname;
	NclScalar name_missing;
	int name_has_missing;
	string out_val = -1;
	int dimsizes;
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

        name = (string*)NclGetArgValue(
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
			data = _NclGetFileVarInfo2(thefile,*name);
		} else {
			dimsizes = 1;
			ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		}
	} else {
		data = _NclGetFileVarInfo(fname,*name);
	}
	if((data != NULL)&&(data->u.var->n_atts != 0)) {
		ret = NclReturnValue((void*)data->u.var->attnames, 1, &data->u.var->n_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
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

NhlErrorTypes _NclIFileVarDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	int dimsize;
	NclScalar missing;
	int has_missing;

	int tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	string *dimnames;
	string *types;
	string *varnames;
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

        varnames = (string*)NclGetArgValue(
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

        types = (string*)NclGetArgValue(
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

        dimnames = (string*)NclGetArgValue(
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
NhlErrorTypes _NclIFileDimDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	int dimsize;
	NclScalar missing;
	int has_missing;

	int tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	string *dimnames;
	int *dimsizes;
	logical *unlimited;
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

        dimnames = (string*)NclGetArgValue(
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

        dimsizes = (int*)NclGetArgValue(
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
		ret = _NclFileAddDim(thefile,dimnames[i],dimsizes[i],unlimited[i]);
		if(ret < NhlINFO) {
			ret0 = ret;
		}
	}
	return(ret0);
}

NhlErrorTypes _NclIFileAttDef
#if NhlNeedProto
(void)
#else
()
#endif
{

	int dimsize;
	NclScalar missing;
	int has_missing;

	int tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	string *dimnames;
	int *dimsizes;
	logical *unlimited;
	int i,j;
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

	int dimsize;
	NclScalar missing;
	int has_missing;

	int tmp_dimsize;
	NclScalar tmp_missing;
	int tmp_has_missing;

	obj *thefile_id;
	string *varnames;
	int *dimsizes;
	logical *unlimited;
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

        varnames = (string*)NclGetArgValue(
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
				NhlPError(NhlFATAL,NhlEUNKNOWN,"Missing value variable name detected, can't continue");
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
  string *format_string;
  int ndims_input_var, dsizes_input_var[NCL_MAX_DIMENSIONS], nlata, nlona, igrida[2];
  NclScalar missing_input_var;
  int has_missing_input_var, total_elements,i;
  char buffer[80];
/*
 * Output array variables
 */
  string *output_var;
/*
 * Retrieve parameters
 *
 * Note any of the pointer parameters can be set to NULL, which
 * implies you don't care about its value.
 */

  format_string = (string*)NclGetArgValue(
           0,
           2,
           NULL, 
           NULL,
	   NULL,
	   NULL,
           NULL,
           0);

  input_var = (int*)NclGetArgValue(
           1,
           2,
           &ndims_input_var, 
           dsizes_input_var,
	   &missing_input_var,
	   &has_missing_input_var,
           NULL,
           0);
  /*
  * compute total number of elements
  */
  total_elements = 1;
  for(i = 0; i < ndims_input_var; i++) {
	total_elements *= dsizes_input_var[i];
  }
  output_var = (string*)malloc(sizeof(string)*total_elements);

  for(i = 0; i < total_elements; i++) {
	sprintf(buffer,NrmQuarkToString(*format_string),input_var[i]);
	output_var[i] = NrmStringToQuark(buffer);
  }
  
  return(NclReturnValue((void*)output_var,ndims_input_var,dsizes_input_var,NULL,NCL_string,0));
}


NhlErrorTypes sprintf_W(void)
{
    /* Input */
    string  *format_string;
    void    *input_var;

    int ndims_input_var,
        dsizes_input_var[NCL_MAX_DIMENSIONS];

    NclScalar   missing_input_var;
    int has_missing_input_var,
        total_elements;
    NclBasicDataTypes   type;

    /* Output */
    string  *output_str;

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
    format_string = (string *) NclGetArgValue(
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

    output_str = (string *) NclMalloc((unsigned int) sizeof(string) * total_elements);
    if (output_str == (string *) NULL) {
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

            break;
    }

    return NclReturnValue((void *) output_str, ndims_input_var, dsizes_input_var,
                            NULL, NCL_string, 0);
}

NhlErrorTypes _NclIAttSetValues( void )
{
	obj* objects;
	int ndims;
	int dimsizes[NCL_MAX_DIMENSIONS];
	NclScalar missing;
	int has_missing;
	int total = 1;
        NclStackEntry data;
	NclAtt tmp_attobj;
	NclAttList *att_list;
	NhlGenArray *gen_array;
	int i,j,m,k,*ids;
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
						ids = (int*)NclMalloc((unsigned)sizeof(int)*att_list->attvalue->multidval.totalelements);
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
								&m,
								1);
							NhlRLSet(rl_list,
								NrmQuarkToString(att_list->quark),
								NhlTGenArray,
								gen_array[i]);
							NclFree(ids);
						} else {
							NclFree(ids);
							NhlPError(NhlWARNING,NhlEUNKNOWN,"The value associated with (%s) does not have an HLU representation", NrmQuarkToString(att_list->quark));
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
	obj *obj_id,*list_id;
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
	NclList tmp_list;
	obj *id;
	int one = 1;
	int i;
	string *tmp_string;
	char buffer[5];
	
   	tmp_string = (string*)NclGetArgValue(
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"NewList: unknow list type. Only \"join\" or \"cat\" supported");
		return(NhlFATAL);
	}
	buffer[4] = '\0';
	buffer[3] = '\0';
	for(i = 0; i < strlen(tmp); i++) {
		buffer[i] = tolower(tmp[i]);
		if(i == 3) 
			break;
	}
	

	data.kind = NclStk_VAL;
	tmp_list =(NclList)_NclListCreate(NULL,NULL,0,0,(strcmp("join",buffer) == 0 ? (NCL_JOIN | NCL_FIFO):(NCL_CONCAT|NCL_FIFO)));
	id = (obj*)NclMalloc(sizeof(obj));
	*id = tmp_list->obj.id;
	data.u.data_obj = _NclMultiDVallistDataCreate(NULL,NULL,Ncl_MultiDVallistData,0,id,NULL,1,&one,TEMPORARY,NULL);
	_NclListSetType((NclObj)tmp_list,NCL_FIFO);
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-Variable passed to printVarSummary, can't continue");
		return(NhlFATAL);
	}
}
NhlErrorTypes _NclIprintFileVarSummary( void )
{
	NclFile thefile;
	obj *thefile_id;
	NclQuark *out_val;
	int dimsizes = 1;
	NclObjTypes ot;
	string* var_string;

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
        var_string = (string*)NclGetArgValue(
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

NhlErrorTypes _NclILoadScript( void )
{
	NclStackEntry path;
	NclMultiDValData p_md = NULL;
	char buf[1024];

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
	NclStackEntry out_data;
	NclMultiDValData p_md = NULL;
	NclMultiDValData rw_md = NULL;
	NclFile file = NULL;
	NclMultiDValData out_md = NULL;
	char *rw;
	int i;
	int rw_v;
	int *id = (int*)NclMalloc((unsigned)sizeof(int));
	int dim_size = 1,one = 1;
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

		file = _NclCreateFile(NULL,NULL,Ncl_File,0,TEMPORARY,((NclQuark*)p_md->multidval.val)[i],rw_v);
		if(file != NULL) {
			id = (int*)NclMalloc((unsigned)sizeof(int));
			*id = file->obj.id;
			out_md = _NclMultiDValnclfileDataCreate(NULL,NULL,Ncl_MultiDValnclfileData,0,id,NULL,1,&dim_size,TEMPORARY,NULL);
			if((out_md == NULL)|| (_NclListPush((NclObj)tmp_list,(NclObj)out_md) == NhlFATAL)) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred opening %s, can't continue",NrmQuarkToString(((NclQuark*)p_md->multidval.val)[i]));	
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
				NhlPError(NhlFATAL,NhlEUNKNOWN,"An error occurred opening %s, can't continue",NrmQuarkToString(((NclQuark*)p_md->multidval.val)[i]));	
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
	obj *obj_id,*list_id;
	NclObj thelist = NULL;
	string *ret_val;
	int dimsize = 2;
        NclStackEntry data;
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
	data= _NclGetArg(1,2,DONT_CARE);
	thelist = _NclGetObj(*list_id);
	list_type = _NclListGetType(thelist);
	i = 0;
	ret_val = (string*)NclMalloc(2 * sizeof(string));
	if(list_type & NCL_JOIN)  {
		ret_val[i++] = NrmStringToQuark("join");
	} else {
		ret_val[i++] = NrmStringToQuark("cat");
	}
	if(list_type & NCL_FIFO) {
		ret_val[i++] = NrmStringToQuark("fifo");
	} else {
		ret_val[i++] = NrmStringToQuark("lifo");
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
	obj *obj_id,*list_id;
	NclObj thelist = NULL;
	string *option;
        NclStackEntry data;
	char *tmp;	
	char buffer[5];
	int i;

	

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
   	option = (string*)NclGetArgValue(
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
	buffer[3] = '\0';
	buffer[4] = '\0';
	for(i = 0; i < strlen(tmp); i++) {
		buffer[i] = tolower(tmp[i]);
		if(i == 3) 
			break;
	}
	if(strcmp(buffer,"join") ==0) {
		_NclListSetType(thelist, NCL_JOIN);
	} else if(strcmp(buffer,"cat") == 0) {
		_NclListSetType(thelist, NCL_CONCAT);
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
	string *path;
	string *filename;
	string *dimnames;
	int *dimsizes;
	string *varnames;
	obj *varinfo;
	NclObj fileatts_obj;
	int n_dims,n_dims0;
	int n_vars;
	int n_fileatts;
	char filename_buffer[2048];
	NclList  varinfo_obj, attlist_obj, attvals_obj;
	NclListObjList *thelist,*attvals,*attlist;
	int i,j,k;
	NclVar tmp_var;
	NclMultiDValData dnames_md,tmp_md,tmp_val;
	nc_type the_type;
	nc_type tmp_type;
	int varids[2048];
	int dim_ids[2048];
	int ids[2048];
	int cdfid;
	NclAtt tmp_att;
	NclAttList *nclattlist;
        NclMultiDValData p_md = NULL;
        NclFile file = NULL;
        NclMultiDValData out_md = NULL;
        int *id = (int*)NclMalloc((unsigned)sizeof(int));
        int dim_size = 1;
	NclBasicDataTypes ncl_var_type;
	int unlimited_id = -1;
	
	


  	path = (string*)NclGetArgValue(
           0,
           5,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   NULL,
	   DONT_CARE);
  	dimnames = (string*)NclGetArgValue(
           1,
           5,
	   NULL,
	   &n_dims,
	   NULL,
	   NULL,
	   NULL,
	   DONT_CARE);
  	dimsizes = (int*)NclGetArgValue(
           2,
           5,
	   NULL,
	   &n_dims0,
	   NULL,
	   NULL,
	   NULL,
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"The specified netCDF file can't be created, either the file exists or the path is incorrect");
		return(NhlFATAL);
	}
	for(i = 0; i < n_dims; i++) {
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Something is wrong with the varinfo parameter");
			return(NhlFATAL);
	
		}
		tmp_md = (NclMultiDValData)_NclGetObj(tmp_var->var.thevalue_id);
		if(tmp_md->multidval.data_type != NCL_string) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"varinfo parameter must be a list of string variable names");
			return(NhlFATAL);
		}
		if(tmp_var->var.att_id != -1) {
			tmp_att= (NclAtt)_NclGetObj(tmp_var->var.att_id);
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"varinfo parameter list elements must at minimum contain the attributes \"dims\" and \"type\" ");
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"varinfo parameter list elements must at minimum contain the attributes \"dims\" and \"type\", attribute \"dims\" not found");
			return(NhlFATAL);
		}
		for(j = 0; j < dnames_md->multidval.totalelements; j++) {
			ids[j] = -2;
			for(k=0; k < n_dims; k++) {
				if(((string*)(dnames_md->multidval.val))[j] == dimnames[k]) {
					ids[j] = dim_ids[k];
					if((unlimited_id == ids[j])&&(j != 0)) {
						NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: unlimited dimension must be first dimension");
						return(NhlFATAL);
					}
				}
			}
			if(ids[j] == -2) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"createfile: dimension named %s was not defined in dimension info list can't continue",NrmQuarkToString(((string*)(dnames_md->multidval.val))[j]));
				return(NhlFATAL);
			}
		}
		nclattlist = tmp_att->att.att_list;
		the_type = 0;
		while(nclattlist != NULL) {
			if(nclattlist->quark == NrmStringToQuark("type")){
				the_type = _MapType(_NclKeywordToDataType( _NclLookUp(NrmQuarkToString(*(string*)(nclattlist->attvalue->multidval.val)))));
				ncl_var_type = _NclKeywordToDataType( _NclLookUp(NrmQuarkToString(*(string*)(nclattlist->attvalue->multidval.val))));
				break;
			} else {
				nclattlist = nclattlist->next;
			}
		}
		if(the_type == 0) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Either and unsupported type was requested or the \"type\" attribute was not supplied");
			return(NhlFATAL);
		}

		varids[i]= ncvardef(cdfid,NrmQuarkToString(*(string*)tmp_md->multidval.val),the_type,dnames_md->multidval.totalelements,ids);
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
						
					}
				}
			} else if((nclattlist->quark != NrmStringToQuark("type"))&&(nclattlist->quark!=NrmStringToQuark("dims"))){
				if(nclattlist->attvalue->multidval.data_type != NCL_string) {		
					tmp_type = _MapType(nclattlist->attvalue->multidval.data_type);
					ncattput(cdfid,varids[i],nclattlist->attname,tmp_type,nclattlist->attvalue->multidval.totalelements,nclattlist->attvalue->multidval.val);
				} else {
					ncattput(cdfid,varids[i],nclattlist->attname,NC_CHAR,strlen(NrmQuarkToString(*(string*)nclattlist->attvalue->multidval.val)),NrmQuarkToString(*(string*)nclattlist->attvalue->multidval.val));
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
						ncattput(cdfid,NC_GLOBAL,nclattlist->attname,NC_CHAR,strlen(NrmQuarkToString(*(string*)nclattlist->attvalue->multidval.val)),NrmQuarkToString(*(string*)nclattlist->attvalue->multidval.val));
					}
				}
				nclattlist = nclattlist->next;
			}
		}
	} else {
		NhlPError(NhlWARNING,NhlEUNKNOWN,"fileatts parameter must be a variable, which optionally contains global file attributes, a value was passed in"); 
	}
	ncendef(cdfid);
	/*nc__enddef(cdfid,65536,4,0,4); */
	ncclose(cdfid);


        file = _NclCreateFile(NULL,NULL,Ncl_File,0,TEMPORARY,*(NclQuark*)path,0);
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

	return(NhlNOERROR);
}

NhlErrorTypes _NclISetFileOption(void)
{
	NclStackEntry data;
	NclMultiDValData tmp_md = NULL;
	NclMultiDValData tmp_md1 = NULL;
	NclFile f = NULL;
	string format = NrmNULLQUARK;
	string option;
	NhlErrorTypes ret;
	int n_dims = 1;

	data = _NclGetArg(0,3,DONT_CARE);
	switch(data.kind) {
		case NclStk_VAR:
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
			break;
		case NclStk_VAL:
			tmp_md = (NclMultiDValData)data.u.data_obj;
			break;
	}
	if(tmp_md == NULL)
		return(NhlFATAL);
	if (tmp_md->multidval.data_type == NCL_string) {
		format = *(string*)tmp_md->multidval.val;
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


  	option = *(string*)NclGetArgValue(
           1,
           3,
	   NULL,
	   &n_dims,
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
	}
	if(tmp_md1 == NULL)
		return(NhlFATAL);

	_NclInitClass(nclFileClass);
	ret = _NclFileSetOption(f,format,option,tmp_md1);

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
    string  *varnames;
    NclQuark    *vartypes = NULL;
    NclObjTypes vartype;

    /* dimensions, sizes */
    int ndims,
        dimsz[NCL_MAX_DIMENSIONS];
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
    varnames = (string *) NclGetArgValue(
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
	  vartype = _NclFileVarRepValue(f, varnames[i]); 
	  switch (vartype) {
	    	case Ncl_Typedouble:
		    	vartypes[i] = NrmStringToQuark("double");
			    break;

    		case Ncl_Typefloat:
	    		vartypes[i] = NrmStringToQuark("float");
		    	break;

    		case Ncl_Typelong:
	    		vartypes[i] = NrmStringToQuark("long");
		    	break;

    		case Ncl_Typeint:
	    		vartypes[i] = NrmStringToQuark("integer");
		    	break;

    		case Ncl_Typeshort:
	    		vartypes[i] = NrmStringToQuark("short");
		    	break;

       		case Ncl_Typebyte:
	    		vartypes[i] = NrmStringToQuark("byte");
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

            default:
                has_missing = True;
                vartypes[i] = NrmStringToQuark("missing");

                NhlPError(NhlWARNING, NhlEUNKNOWN,
                    "getfilevartypes: variable (%s) does not exist in file (%s)",
                    NrmQuarkToString(varnames[i]), NrmQuarkToString(f->file.fname));

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
    int dimsizes = 1;
    int *dim_sizes;

    NhlErrorTypes   ret;

    int i = 0;
    

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

    if (f != NULL) {
        dim_sizes = (int *) NclMalloc((unsigned int) sizeof(int) * f->file.n_file_dims);
        if (f->file.n_file_dims != 0) {
            for (i = 0; i < f->file.n_file_dims; i++)
                dim_sizes[i] = f->file.file_dim_info[i]->dim_size;

            ret = NclReturnValue((void *) dim_sizes, 1, &(f->file.n_file_dims), NULL, NCL_int, 1);
            free(dim_sizes);
        	return ret;
        }
    }
    else {
        NhlPError(NhlWARNING, NhlEUNKNOWN, " getfiledimsizes(): undefined file variable");

        dimsizes = 1;
        NclReturnValue(
            (void*) &((NclTypeClass) nclTypeintClass)->type_class.default_mis, 1,
            &dimsizes, &((NclTypeClass) nclTypeintClass)->type_class.default_mis,
            ((NclTypeClass) nclTypeintClass)->type_class.data_type, 1);
	
        return NhlWARNING;
    }
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
    string  *dname;

    /* dimensions, sizes */
    int dimsizes = 1;
    int ndims,
        dimsz[NCL_MAX_DIMENSIONS];

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
    dname = (string *) NclGetArgValue(
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
        if (f->file.n_file_dims > 0) {
            for (i = 0; i < f->file.n_file_dims; i++) {
                if (f->file.file_dim_info[i]->dim_name_quark == *dname) {
                    isunlimited = f->file.file_dim_info[i]->is_unlimited;
                    break;
                }
            }
        }
    }
    else {
        NhlPError(NhlWARNING, NhlEUNKNOWN, " isunlimited(): undefined file variable");
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
    string  *files;
    const char  *fpath = NULL;
    struct stat st;

    int ncid = 0;

    int dimsizes = 1;
    int ndims,
        dimsz[NCL_MAX_DIMENSIONS];
    int sz = 1;

    logical *file_exists;        /* file exists? */
    int i = 0;


    files = (string *) NclGetArgValue(
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
    if (file_exists == (logical *) NULL) {
        NhlPError(NhlFATAL, errno, " isfilepresent: memory allocation error");
        return NhlFATAL;
    }

    for (i = 0; i < sz; i++) {
        fpath = (char *) NrmQuarkToString(files[i]);
        if (!strncmp(fpath, "http", 4)) {
            ncid = ncopen(fpath, NC_NOWRITE);
            if (ncid == -1)
                file_exists[i] = 0;     /* false */
            else
                file_exists[i] = 1;     /* true */
        }
        else {
            fpath = _NGResolvePath(NrmQuarkToString(files[i]));
            file_exists[i] = stat(fpath, &st) == -1 ? 0 : 1;
        }
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.intval = (int) ((NclTypeClass) nclTypeintClass)->type_class.default_mis.intval;

        output = (int*)NclMalloc(sizeof(int)*total_elements);

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
                        if((val < dmax) && (val > dmin))
                        {
                            ret_missing.intval = (int) val;
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
                            "There are %d doubles larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d doubles less than INT_MIN, which has flagged missing.",
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
                        if((val < fmax) && (val > fmin))
                        {
                            ret_missing.intval = (int) val;
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
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d floats larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d floats less than INT_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.intval = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis.intval;
                        }
                        else
                        { 
                            if((llval <= INT_MAX) && (llval >= INT_MIN))
                                ret_missing.intval = (int) llval;
                        }
                    }

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            has_missing = 1;
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) tointeger, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d strings larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d strings less than INT_MIN, which has forced to INT_MIN.",
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
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.intval = (int) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
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
                            "There are %d unsigned int larger than INT_MAX, which has been flagged missing.",
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
                            "There are %d long larger than INT_MAX, which has been flagged missing.",
                            underflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than INT_MIN, which has been flagged missing.",
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
                            "There are %d long larger than INT_MAX, which has been flagged missing.",
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
                            "There are %d int64 larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than INT_MIN, which has been flagged missing.",
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
                            "There are %d uint64 larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.uintval = (unsigned int) ((NclTypeClass) nclTypeuintClass)->type_class.default_mis.uintval;

        output = (unsigned int*)NclMalloc(sizeof(unsigned int)*total_elements);

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
                        if(missing.doubleval < dmax)
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
                            "There are %d doubles larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d doubles less than INT_MIN, which has flagged missing.",
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
                        if(missing.floatval < fmax)
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
                            "There are %d floats larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d floats less than INT_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
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

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        errno = 0;
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to stringtointeger, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d double larger than UINT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
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
                        ret_missing.uintval = (unsigned int) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (unsigned int) val;
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.uintval = (unsigned int) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
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
                            "There are %d int less than 0, which has been flagged missing.",
                            underflowed);
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d long larger than UINT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than 0, which has been flagged missing.",
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
                            "There are %d unsigned long larger than UINT_MAX, which has been flagged missing.",
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
                            "There are %d long larger than UINT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than 0, which has been flagged missing.",
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
                            "There are %d uint64 larger than UINT_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.longval = (long) ((NclTypeClass) nclTypelongClass)->type_class.default_mis.longval;

        output = (long *)NclMalloc(sizeof(long)*total_elements);

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
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than LONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than LONG_MIN, which has been flagged missing.",
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
                        if((val < fmax) && (val > fmin))
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
                            "There are %d floats larger than INT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d floats less than INT_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
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

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(missing.stringval == ptr[i])
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
                                    "A bad value was passed to (string) tolong, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d double larger than LONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than LONG_MIN, which has been flagged missing.",
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
                        ret_missing.longval = (long) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (long) val;
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.longval = (long) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (long) val;
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
                        if(missing.uintval <= LONG_MAX)
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
                            "There are %d unsigned int larger than LONG_MAX, which has been flagged missing.",
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
                            "There are %d unsigned long larger than LONG_MAX, which has been flagged missing.",
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
                            "There are %d int64 larger than LONG_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than LONG_MIN, which has been flagged missing.",
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
                            "There are %d uint64 larger than LONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.ulongval = (unsigned long) ((NclTypeClass) nclTypeulongClass)->type_class.default_mis.ulongval;

        output = (unsigned long *)NclMalloc(sizeof(unsigned long)*total_elements);

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
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than ULONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
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
                        if((val < fmax) && (val > fmin))
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
                            "There are %d float larger than ULONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    ptr = (string *) in_value;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) toulong, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            if((llval <= ULONG_MAX) && (llval >= 0))
                                ret_missing.ulongval = (unsigned long) llval;
                    }

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        errno = 0;
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) toulong, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d double larger than ULONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
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
                    char val;
                    char *ptr;
   
                    ptr = (char *) in_value;
   
                    if(has_missing)
                    {
                        ret_missing.ulongval = (unsigned long) missing.byteval;
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
                            "There are %d int less than 0, which has been flagged missing.",
                            underflowed);
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
                            "There are %d short less than 0, which has been flagged missing.",
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d int64 larger than ULONG_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than 0, which has been flagged missing.",
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
                            "There are %d uint64 larger than ULONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.int64val = (long long) ((NclTypeClass) nclTypeint64Class)->type_class.default_mis.int64val;

        output = (long long *)NclMalloc(sizeof(long long)*total_elements);

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
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than LLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than LLONG_MIN, which has been flagged missing.",
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
                        if((val < fmax) && (val > fmin))
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
                            "There are %d float larger than LLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than LLONG_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) toint64, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            ret_missing.int64val = (long long) llval;
                    }

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        errno = 0;
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) toint64, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d string larger than LLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d string less than LLONG_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (long long) val;
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char val;
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.int64val = (long long) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = (long long) val;
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
                    unsigned long val;
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
                            "There are %d uint64 larger than LLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.uint64val = (unsigned long long) ((NclTypeClass) nclTypeuint64Class)->type_class.default_mis.uint64val;

        output = (unsigned long long *)NclMalloc(sizeof(unsigned long long)*total_elements);

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
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than ULLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
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
                        if((val < fmax) && (val > fmin))
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
                            "There are %d float larger than ULLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) touint64, input strings must contain numeric digits, replacing with missing value");
                        }
                        else if(errno != ERANGE)
                            if(llval >= 0)
                                ret_missing.uint64val = (unsigned long long) llval;
                    }

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        errno = 0;
                        str = NrmQuarkToString(ptr[i]);
    
                        llval = _Nclstrtoll(str,&end);
                        if (strcmp(end, str) == 0)
                        {
                            NhlPError(NhlFATAL,NhlEUNKNOWN,
                                "A bad value was passed to (string) touint64, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d double larger than ULLONG_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    ptr = (char *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.byteval;
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
                            output[i] = (unsigned long long) val;
                        }
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.uint64val = (unsigned long long) missing.charval;
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
                    unsigned long long ullval;
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                                underflowed ++;
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d long less than 0, which has been flagged missing.",
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
                            "There are %d int64 less than 0, which has been flagged missing.",
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
                        ret_missing.uint64val = missing.uint64val;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (unsigned long long) ptr[i];
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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


NhlErrorTypes _NclItoshort
#if     NhlNeedProto
(void)
#else
()
#endif
{
        void *in_value;
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing = 0;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.shortval = (short) ((NclTypeClass) nclTypeshortClass)->type_class.default_mis.shortval;

        output = (short *)NclMalloc(sizeof(short)*total_elements);

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
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than SHRT_MIN, which has been flagged missing.",
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
                        if((val < fmax) && (val > fmin))
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
                            overflowed ++;
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else if(val < fmin)
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
                            "There are %d float larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than SHRT_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
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

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(missing.stringval == ptr[i])
                        {
                            has_missing = 1;
                            output[i] = ret_missing.shortval;
                        }
                        else
                        {
                            llval = _Nclstrtoll(str,&end);
                            if (end == str || errno == ERANGE)
                            {
                                has_missing = 1;
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "A bad value was passed to stringtointeger, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d double larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than SHRT_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.shortval = (short) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        output[i] = (short) ptr[i];
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
                            "There are %d int larger than SHRT_MAX, which has been flagged missing.",
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
                            "There are %d int larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than SHRT_MIN, which has been flagged missing.",
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
                            "There are %d unsigned int larger than SHRT_MAX, which has been flagged missing.",
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
                            "There are %d long larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than SHRT_MIN, which has been flagged missing.",
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
                            "There are %d unsigned long larger than SHRT_MAX, which has been flagged missing.",
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
                            "There are %d int64 larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than SHRT_MIN, which has been flagged missing.",
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
                            "There are %d uint64 larger than SHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.ushortval = (unsigned short) ((NclTypeClass) nclTypeushortClass)->type_class.default_mis.ushortval;

        output = (unsigned short *)NclMalloc(sizeof(unsigned short)*total_elements);

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
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
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
                        if((val < fmax) && (val > fmin))
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
                            "There are %d float larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
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

                    ptr = (string *) in_value;
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
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "A bad value was passed to stringtointeger, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d double larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.ushortval = (unsigned short) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            underflowed ++;
                            has_missing = 1;
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
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d int larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than 0, which has been flagged missing.",
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
                            "There are %d unsigned int larger than USHRT_MAX, which has been flagged missing.",
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
                            "There are %d long larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than 0, which has been flagged missing.",
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
                            "There are %d unsigned long larger than USHRT_MAX, which has been flagged missing.",
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
                            "There are %d int64 larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than 0, which has been flagged missing.",
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
                            "There are %d uint64 larger than USHRT_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.floatval = (float) ((NclTypeClass) nclTypefloatClass)->type_class.default_mis.floatval;

        output = (float *)NclMalloc(sizeof(float)*total_elements);

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
                        if(val < dmax && val > dmin)
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
                            "There are %d double larger than FLT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than (-FLT_MAX), which has been flagged missing.",
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
                    string *ptr;
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

                    ptr = (string *) in_value;

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
                                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                                        "A bad value was passed to (string) tofloat, input strings must contain numeric digits, replacing with missing value");
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
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "A bad value was passed to (string) tofloat, input strings must contain numeric digits, replacing with missing value");
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
                            "There are %d string larger than FLT_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d string less than (-FLT_MAX), which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    ptr = (char *) in_value;
    
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
                    unsigned char val;
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
                    unsigned long long val;
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
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;

        int ndim_str, dimsz_str[NCL_MAX_DIMENSIONS];
        int has_missing_str = 0;
        NclScalar   missing_str;
        NclBasicDataTypes type_str;

        string *output;

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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.stringval = (string) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

        output = (string *)NclMalloc(sizeof(string)*total_elements);

        switch(type)
        {
            case NCL_string:
                {
                    string *ptr;
                    char *strin;
                    char *strout;

                    ptr = (string *) in_value;

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
                    char *ptr;

                    ptr = (char *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%d", missing.byteval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%d", ptr[i]);
                        output[i] = NrmStringToQuark(buffer);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    ptr = (unsigned char *) in_value;

                    if(has_missing)
                    {
                        sprintf(buffer, "%c", missing.charval);
                        ret_missing.stringval = NrmStringToQuark(buffer);
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        sprintf(buffer, "%c", ptr[i]);
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
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;

        int ndim_str, dimsz_str[NCL_MAX_DIMENSIONS];
        int has_missing_str = 0;
        NclScalar   missing_str;
        NclBasicDataTypes type_str;

        string *output;

        char *fmt;
        string *format;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.stringval = (string) ((NclTypeClass) nclTypestringClass)->type_class.default_mis.stringval;

        format = (string *) NclGetArgValue(
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

        output = (string *)NclMalloc(sizeof(string)*total_elements);

        switch(type)
        {
            case NCL_string:
                {
                    string *ptr;
                    char *strin;
                    char *strout;

                    ptr = (string *) in_value;

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
                    char *ptr;

                    ptr = (char *) in_value;

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
                    unsigned char *ptr;

                    ptr = (unsigned char *) in_value;

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
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
        double *output;

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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.doubleval = (double) ((NclTypeClass) nclTypedoubleClass)->type_class.default_mis.doubleval;

        output = (double *)NclMalloc(sizeof(double)*total_elements);

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
                    string *ptr;
                    char *str;
                    char *end;

                    ptr = (string *) in_value;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        dval = strtod(str,&end);
                        if (end == str || errno == ERANGE)
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
                                if (strcmp(end, str) == 0)
                                {
                                    NhlPError(NhlFATAL,NhlEUNKNOWN,
                                        "A bad value was passed to (string) todouble, input strings must contain numeric digits, replacing with missing value");
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
                            if (strcmp(end, str) == 0)
                            {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "A bad value was passed to (string) todouble, input strings must contain numeric digits, replacing with missing value");
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
                    char *ptr;

                    ptr = (char *) in_value;
    
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
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.byteval = (byte) ((NclTypeClass) nclTypebyteClass)->type_class.default_mis.byteval;

        output = (char *)NclMalloc(sizeof(byte)*total_elements);

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = 0.0;
                    dmax = (double) UCHAR_MAX;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val < dmax) && (val > dmin))
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
                            "There are %d double larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than CHAR_MIN, which has been flagged missing.",
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

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val < fmax) && (val > fmin))
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
                            "There are %d float larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than CHAR_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    string *ptr;
                    char *str;
                    char *end;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
                        }
                        else
                        {
                            if((llval < UCHAR_MAX) && (llval >= 0))
                                ret_missing.byteval = (unsigned char) llval;
                        }
                    }

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(missing.stringval == ptr[i])
                        {
                            output[i] = ret_missing.byteval;
                        }
                        else
                        {
                            llval = _Nclstrtoll(str,&end);
                            if (strcmp(end, str) == 0)
                            {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "A bad value was passed to tobyte, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.byteval;
                            }
                            else if (llval > UCHAR_MAX)
                            {
                                has_missing = 1;
                                overflowed ++;
                                output[i] = ret_missing.byteval;
                            }
                            else if (llval < 0)
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
                            "There are %d double larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than CHAR_MIN, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_byte:
                {
                    char val;
                    char *ptr;

                    ptr = (char *) in_value;
    
                    if(has_missing)
                    {
                        ret_missing.byteval = missing.byteval;
                    }

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        output[i] = val;
                    }
                }
                break;
            case NCL_char:
                {
                    char val;
                    char *ptr;

                    if(has_missing)
                    {
                        ret_missing.byteval = (char) missing.charval;
                    }

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val < 0)
                        {
                            has_missing = 1;
                            overflowed ++;
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
                            "There are %d int larger than CHAR_MAX, which has been flagged missing.",
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
                        if((missing.shortval <= CHAR_MAX) && (missing.shortval >= CHAR_MIN))
                            ret_missing.byteval = (char) missing.shortval;
                    }

                    ptr = (short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
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
                            "There are %d int larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than CHAR_MIN, which has been flagged missing.",
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
                        if(missing.ushortval <= CHAR_MAX)
                            ret_missing.byteval = (char) missing.ushortval;
                    }

                    ptr = (unsigned short *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
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
                            "There are %d int larger than CHAR_MAX, which has been flagged missing.",
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
                        if((missing.intval <= CHAR_MAX) && (missing.intval >= CHAR_MIN))
                            ret_missing.byteval = (char) missing.intval;
                    }

                    ptr = (int *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
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
                            "There are %d int larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than CHAR_MIN, which has been flagged missing.",
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
                            ret_missing.byteval = (char) missing.uintval;
                    }

                    ptr = (unsigned int *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
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
                            "There are %d unsigned int larger than CHAR_MAX, which has been flagged missing.",
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
                        if((missing.longval <= CHAR_MAX) && (missing.longval >= CHAR_MIN))
                            ret_missing.byteval = (char) missing.longval;
                    }

                    ptr = (long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
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
                            "There are %d long larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than CHAR_MIN, which has been flagged missing.",
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
                            ret_missing.byteval = (char) missing.ulongval;
                    }

                    ptr = (unsigned long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
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
                            "There are %d unsigned long larger than CHAR_MAX, which has been flagged missing.",
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
                        if((missing.int64val <= CHAR_MAX) && (missing.int64val >= CHAR_MIN))
                            ret_missing.byteval = (char) missing.int64val;
                    }

                    ptr = (long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.byteval;
                        }
                        else if(val < 0)
                        {
                            has_missing = 1;
                            underflowed ++;
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
                            "There are %d int64 larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than CHAR_MIN, which has been flagged missing.",
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
                            ret_missing.byteval = (char) missing.uint64val;
                    }

                    ptr = (unsigned long long *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = ptr[i];
                        if(val > UCHAR_MAX)
                        {
                            has_missing = 1;
                            overflowed ++;
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
                            "There are %d uint64 larger than CHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        ret_missing.charval = (unsigned char) ((NclTypeClass) nclTypecharClass)->type_class.default_mis.charval;

        output = (unsigned char *)NclMalloc(sizeof(unsigned char)*total_elements);

        switch(type)
        {
            case NCL_double:
                {
                    double val, dmin, dmax;
                    double *ptr;

                    dmin = 0.0;
                    dmax = (double) UCHAR_MAX;

                    if(has_missing)
                    {
                        val = missing.doubleval;
                        if((val < dmax) && (val > dmin))
                           ret_missing.charval = (char) val;
                    }

                    ptr = (double *) in_value;

                    for(i = 0; i < total_elements; i++)
                    {
                        val = (double) ptr[i];
                        if(val > dmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < dmin)
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
                            "There are %d double larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
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

                    if(has_missing)
                    {
                        val = missing.floatval;
                        if((val < fmax) && (val > fmin))
                           ret_missing.charval = (char) val;
                    }

                    ptr = (float *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        val = (float) ptr[i];
                        if(val > fmax)
                        {
                            has_missing = 1;
                            overflowed ++;
                            output[i] = ret_missing.charval;
                        }
                        else if(val < fmin)
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
                            "There are %d float larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d float less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_string:
                {
                    long long llval;
                    long lval;
                    string *ptr;
                    char *str;
                    char *end;
                    char buff[128];
                    int  buffsize = 128;

                    if(has_missing)
                    {
                        errno = 0;
                        str = NrmQuarkToString(missing.stringval);
                        llval = _Nclstrtoll(str,&end);
                        if (end == str || errno == ERANGE)
                        {
                            ret_missing.intval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.intval;
                        }
                        else
                        {
                            if((llval <= UCHAR_MAX) && (llval >= 0))
                                ret_missing.charval = (int) llval;
                        }
                    }

                    ptr = (string *) in_value;
                    for(i = 0; i < total_elements; i++)
                    {
                        str = NrmQuarkToString(ptr[i]);
    
                        if(missing.stringval == ptr[i])
                        {
                            has_missing = 1;
                            output[i] = ret_missing.charval;
                        }
                        else
                        {
                            lval = _Nclstrtol(str,&end);
                            if (end == str || errno == ERANGE)
                            {
                                has_missing = 1;
                                NhlPError(NhlFATAL,NhlEUNKNOWN,
                                    "A bad value was passed to stringtointeger, input strings must contain numeric digits, replacing with missing value");
                                output[i] = ret_missing.charval;
                            }
                            else if (lval > UCHAR_MAX)
                            {
                                has_missing = 1;
                                overflowed ++;
                                output[i] = ret_missing.charval;
                            }
                            else if (lval < 0)
                            {
                                has_missing = 1;
                                underflowed ++;
                                output[i] = ret_missing.charval;
                            }
                            else
                            {
                                output[i] = (unsigned char) lval;
                            }
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d double less than 0, which has been flagged missing.",
                            underflowed);
                    }
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;

                    if(has_missing)
                    {
                        ret_missing.charval = (unsigned char) missing.charval;
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
                    char val;
                    char *ptr;

                    ptr = (char *) in_value;
    
                    if(has_missing)
                    {
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

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d char less than, which has been flagged missing.",
                            underflowed);
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
                            ret_missing.charval = (unsigned char) missing.shortval;
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
                            "There are %d int larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than 0, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.ushortval;
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
                            output[i] = (unsigned char) val;
                        }
                    }
  
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int larger than UCHAR_MAX, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.intval;
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
                            "There are %d int larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int less than 0, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.uintval;
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
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d unsigned int larger than UCHAR_MAX, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.longval;
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
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
    
                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d long less than 0, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.ulongval;
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
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d unsigned long larger than UCHAR_MAX, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.int64val;
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
                            output[i] = (unsigned char) val;
                        }
                    }

                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }

                    if(underflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d int64 less than 0, which has been flagged missing.",
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
                            ret_missing.charval = (unsigned char) missing.uint64val;
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
                            output[i] = (unsigned char) val;
                        }
                    }
    
                    if(overflowed)
                    {
                        NhlPError(NhlWARNING, NhlEUNKNOWN,
                            "There are %d uint64 larger than UCHAR_MAX, which has been flagged missing.",
                            overflowed);
                    }
                }
                break;
            case NCL_logical:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_logical to integer.");
                return NhlFATAL;
                break;
            case NCL_obj:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_ob to integer.");
                return NhlFATAL;
                break;
            case NCL_list:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_list to integer.");
                return NhlFATAL;
                break;
            case NCL_none:
                NhlPError(NhlFATAL, errno, "Don't know how to convert NCL_none to integer.");
                return NhlFATAL;
                break;
            default:
                NhlPError(NhlFATAL, errno, "Don't know how to convert unkown type to integer.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type, out_type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
        }

        if(has_missing)
        {
            ret_missing = missing;
        }

        switch(type)
        {
            case NCL_char:
                {
                    unsigned char *ptr;
                    char *out_ptr;

                    output = (void *)NclMalloc(sizeof(char)*total_elements);
                    out_ptr = output;

                    if(has_missing)
                    {
                        ret_missing.byteval = (char) missing.charval;
                    }

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (char) ptr[i];
                    }
                    out_type = NCL_byte;
                }
                break;
            case NCL_byte:
                {
                    char *ptr;
                    char *out_ptr;

                    output = (void *)NclMalloc(sizeof(char)*total_elements);
                    out_ptr = output;

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_byte;
                }
                break;
            case NCL_short:
                {
                    short *ptr;
                    short *out_ptr;

                    output = (void *)NclMalloc(sizeof(short)*total_elements);
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
                NhlPError(NhlFATAL, errno, "Don't know how to convert type to singed.");
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
        int total_elements = 1;
        int n_dims = 0;
        int dimsizes[NCL_MAX_DIMENSIONS];
        NclScalar missing;
        NclScalar ret_missing;
        NclBasicDataTypes type, out_type;
        int has_missing;
        int i;
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

        for(i = 0; i < n_dims; i++)
        {
            total_elements *= dimsizes[i];
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
                    out_ptr = output;

                    if(has_missing)
                    {
                        ret_missing.charval = (unsigned char) missing.byteval;
                    }

                    ptr = (char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = (unsigned char) ptr[i];
                    }
                    out_type = NCL_char;
                }
                break;
            case NCL_char:
                {
                    unsigned char *ptr;
                    unsigned char *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned char)*total_elements);
                    out_ptr = output;

                    ptr = (unsigned char *) in_value;
    
                    for(i = 0; i < total_elements; i++)
                    {
                        out_ptr[i] = ptr[i];
                    }
                    out_type = NCL_char;
                }
                break;
            case NCL_ushort:
                {
                    unsigned short *ptr;
                    unsigned short *out_ptr;

                    output = (void *)NclMalloc(sizeof(unsigned short)*total_elements);
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
                NhlPError(NhlFATAL, errno, "Don't know how to convert type to singed.");
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



#ifdef __cplusplus
}
#endif


/*
 *      $Id: BuiltInFuncs.c,v 1.97 1998-04-20 21:49:28 ethan Exp $
 */
/************************************************************************
*									*
*			     Copyright (C)  1995			*
*	     University Corporation for Atmospheric Research		*
*			     All Rights Reserved			*
*									*
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
#include <signal.h>

extern int cmd_line;

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
		ret = nclfprintf(fp,"\n%s\t%s\n",NrmQuarkToString(step->u.file->name),(step->u.file->wr_status ? "READ ONLY" : "READ/WRITE"));
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
				if(step->u.func->theargs[i].arg_data_type != NULL) {
					ret = nclfprintf(fp,": %s,\n",step->u.func->theargs[i].arg_data_type->name);
					if(ret < 0) {
						_NclFreeApiDataList((void*)tmp);
        					return(NhlWARNING);
					}
				} else {
					ret = nclfprintf(fp,",\n");
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
			if(step->u.func->theargs[step->u.func->nparams-1].arg_data_type != NULL) {
				ret = nclfprintf(fp,": %s\n",step->u.func->theargs[step->u.func->nparams-1].arg_data_type->name);
				if(ret < 0) {
					_NclFreeApiDataList((void*)tmp);
        				return(NhlWARNING);
				}
			} else {
				ret = nclfprintf(fp,"\n");
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
	

	data = _NclGetArg(0,1,DONT_CARE);
	switch(data.kind) {
	case NclStk_VAR:
/*
		tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
*/
		file_q = data.u.data_var->var.var_quark;
		break;
	case NclStk_VAL:
		return(NhlFATAL);
	}
/*
	thefile = _NclGetObj(*(int*)tmp_md->val);
	
	var_names = (NclQuark*)NclMalloc((unsigned)sizeof(NclQuark)*thefile->n_vars);
	dimsize = thefile->n_vars;
	
	for(i = 0; i< thefile->file.n_vars;i++ 	) {
		if(thefile->file.var_info[i] != NULL) {
			var_names[i] = thefile->file.var_info[i]->var_quark;
		} else {
			var_names[i] = 0;
		}
	}
*/
	tmp = _NclGetFileVarInfoList(file_q);
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
		
	data.kind = NclStk_VAL;
	data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)var_names,NULL,1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypestringClass);
	_NclPlaceReturn(data);
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
	tmp = _NclGetFileVarInfoList(file_q);
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
		id = fork();
		if(id == 0) {
			close(fildes[0]);
			close(fileno(stdout));
			new_pipe_fd = dup(fildes[1]);
			close(fildes[1]);
			command = NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val); 
			if(!system(command)) {
				exit(0);
			} else {
				exit(1);
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
		if(cmd_line == 1) {
			pager = getenv("PAGER");
			if(pager == NULL) {
				command = NclMalloc(strlen(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val))+ strlen(" | more"));
				strcpy(command,NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
				strcat(command," | more");
			} else {
				command = NclMalloc(strlen(NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val))+ strlen(pager) + strlen(" | ")+1);
				strcpy(command,NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val));
				strcat(command," | ");
				strcat(command,pager);
			}
			if(!system(command)) {
				NhlFree(command);
				return(NhlNOERROR);
			} else {
				NhlFree(command);
                		return(NhlWARNING);
			}
		} else {
			command = NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val); 
			if(!system(command)) {
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
	if((_NhlGetLayer(base_hl->hlu.hlu_id) != NULL)&&(_NhlGetLayer(over_hl->hlu.hlu_id)!= NULL)) {
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"dump: Unable to covert parameter to string representation for output filename");
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
		if(data.u.data_obj->obj.obj_type_mask && NCL_HLU_MASK) {
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
		if(data.u.data_obj->obj.obj_type_mask && NCL_HLU_MASK) {
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"Non-object passed to update, ignoring request");
	
			return(NhlFATAL);
		} else {
			tmp_md = _NclVarValueRead(data.u.data_var,NULL,NULL);
		}
	} else if(data.kind == NclStk_VAL) {
		tmp_md = data.u.data_obj;
	} else {
		return(NhlFATAL);
	}
	if(data.u.data_obj->obj.obj_type_mask && NCL_HLU_MASK) {
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
		if(data.u.data_obj->obj.obj_type_mask && NCL_HLU_MASK) {
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
		if(data.u.data_obj->obj.obj_type_mask && NCL_HLU_MASK) {
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
						rlist = rlist->next;
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclisdfft: input contains missing values can not continue");
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclisdfft: input contains missing values can not continue");
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
					NhlPError(NhlFATAL,NhlEUNKNOWN,"_Nclisdfft: input contains missing values can not continue");
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
		spacing = (ymax - ymin)/(m-1);
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
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	int *recnum;
	NclScalar missing;
	int has_missing;


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
	}
	for(i = 0; i < n_dimensions; i++) {
		size *= dimsizes[i];
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
		if(thetype == NULL) 
			return(NhlFATAL);	
	}
	if((*recnum)*size*thetype->type_class.size > buf.st_size) {
		ret = NhlFATAL;
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirread: The size implied by the dimension array and record number is greater that the size of the file, can't continue");
		return(NhlFATAL);
	} else { 
		totalsize = size*thetype->type_class.size;
	}
	tmp_ptr = NclMalloc(size*thetype->type_class.size);
	fd = open(path_string,O_RDONLY);
	
	if((tmp_ptr != NULL)&&(fd > 0)) {
		lseek(fd,(*recnum)*size*thetype->type_class.size,SEEK_SET);
		
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

		step = tmp_ptr;
		for(i = 0; i < (int)(totalsize / buf.st_blksize); i++) {
			n = read(fd, step,buf.st_blksize);
			step = step + buf.st_blksize;
		}
		n = read(fd,step,totalsize % buf.st_blksize);
		step = step + totalsize % buf.st_blksize;

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
	for(i = 0; i < n_dimensions; i++) {
		size *= dimsizes[i];
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
		if(thetype == NULL) 
			return(NhlFATAL);	
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
			&(thetype->type_class.default_mis),
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
	int ind;
	int cur_off;
	int i,n;
	int dimsize = 1;

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
		ind = *(int*)control_word;
		lseek(fd,cur_off + ind + 4,SEEK_SET);
		n = read(fd,(control_word),4);
		if(n != 4) {
			break;
		}
		if(ind ==  *(int*)control_word ) {
				i++;
				cur_off += ind + 8;
		} else {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinnumrec: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
			close(fd);
			return(NhlFATAL);
		}
	}
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
	int ind;
	int fd = -1;
	int size = 1;
	int n;
	int n_dims;
	int cur_off = 0;
	NhlErrorTypes ret = NhlNOERROR;
	int rsize = 0;
	NclBasicDataTypes datai_type;
	int total;


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
			ind = *(int*)control_word;
			lseek(fd,cur_off + ind + 4,SEEK_SET);
			n = read(fd,(control_word),4);
			if(n != 4) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecwrite: an error occurred reading the record control words. Something is wrong with the FORTRAN binary file.");
				close(fd);
				return(NhlFATAL);
				break;
			}
			if(ind ==  *(int*)control_word ) {
					i++;
					cur_off += ind + 8;
					rsize = ind;
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
		n = write(fd,&total,4);
		n = write(fd,value,total);
		n = write(fd,&total,4);
		close(fd);
		return(NhlNOERROR);
	} else {
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
	int ind;
	int fd = -1;
	int size = 1;
	int n;
	int cur_off = 0;
	NhlErrorTypes ret = NhlNOERROR;

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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: path is a missing value, can't continue");
		return(NhlFATAL);
	}
	thetype = _NclNameToTypeClass(*type);
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
		ind = *(int*)control_word;
		lseek(fd,cur_off + ind + 4,SEEK_SET);
		n = read(fd,(control_word),4);
		if(n != 4) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: a read error occurred while reading (%s) , can't continue",NrmQuarkToString(*fpath));
			close(fd);
			return(NhlFATAL);
		}
		if(ind ==  *(int*)control_word ) {
				i++;
				cur_off += ind + 8;
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
		ind = *(int*)control_word;
		if(size != -1) {
			value = (void*)NclMalloc(thetype->type_class.size*size);
			if(ind < size*thetype->type_class.size) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"fbinrecread: size specified is greater than record size, filling with missing values");
				ret = NhlWARNING;
			} else if(ind > size*thetype->type_class.size) {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"fbinrecread: size specified is less than record size, some data will not be read");
				ret = NhlWARNING;
			}
			n = read(fd,value,(ind>=size*thetype->type_class.size)?size*thetype->type_class.size:ind);
			if(n != ((ind>=size*thetype->type_class.size)?size*thetype->type_class.size:ind))  {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: an error occurred reading the requested record. Something is wrong with the FORTRAN binary file.");
				NclFree(value);
				close(fd);
				return(NhlFATAL);
			}
			if(ind < size*thetype->type_class.size) {
				for(;ind<size*thetype->type_class.size-1;ind+=thetype->type_class.size) {
					memcpy((void*)((char*)value + ind * thetype->type_class.size),&thetype->type_class.default_mis,thetype->type_class.size);
				}
			}
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				value,
				&(thetype->type_class.default_mis),
				dimsize,
				dimensions,
				TEMPORARY,
				NULL,
				thetype);
		} else {
			value = (void*)NclMalloc(ind);
			n = read(fd,value,ind);
			if(n != ind) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinrecread: an error occurred reading the requested record. Something is wrong with the FORTRAN binary file.");
				NclFree(value);
				close(fd);
				return(NhlFATAL);
			}
			dimsize = ind/thetype->type_class.size;
			tmp_md = _NclCreateMultiDVal(
				NULL,
				NULL,
				Ncl_MultiDValData,
				0,
				value,
				&(thetype->type_class.default_mis),
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
			NhlPError(NhlFATAL,NhlEUNKNOWN,"fbinread: An error in the file path was detected could not resolve file path");
			return(NhlFATAL);
		}
		if(stat(path_string,&buf) == -1) {
			NhlPError(NhlFATAL, NhlEUNKNOWN,"fbinread: Unable to open input file (%s)",path_string);
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
	for(i = 0; i < n_dimensions; i++) {
		size *= dimsizes[i];
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
		if(thetype == NULL) 
			return(NhlFATAL);	
	}
	totalsize = size*thetype->type_class.size;
	tmp_ptr = NclMalloc(totalsize);
	NGCALLF(nclpfortranread,NCLPFORTRANREAD)(path_string,tmp_ptr,&totalsize,&ret,strlen(path_string));
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
		data_out.kind = NclStk_VAL;
		data_out.u.data_obj = tmp_md;
		_NclPlaceReturn(data_out);
		return(ret);
	} 
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
		fd = fopen(path_string,"w+");
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
	char cc;

	step = buffer;
	while(!feof(fp)&& (state != 9)) {
		cc = fgetc(fp);
		switch(state) {
		case 0:
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
		case 2:
			switch(cc) {
			case '.':
				*step++ = cc;
				state = 5;
				break;
			default:
				if(isdigit(cc)) {
					*step++ = cc;
					state = 3;
				} else {
					ungetc(cc,fp);
					step = buffer;
					state = 0;
				}
			}
			break;
		case 3:
			if(isdigit(cc)) {
				*step++ = cc;
			} else {
				switch(cc) {
				case '.':
					*step++ = cc;
					state = 5;
					break;
				default:
/*
* ACCEPT INTEGER
*/
					ungetc(cc,fp);
					state = 9;
					*step++ = '\0';
				}
			}
			break;
		case 5:
			if(isdigit(cc)) {
				*step++ = cc;
			} else {
				switch(cc) {
				case 'E':
				case 'e':
					*step++ = cc;
					state = 6;
					break;
				default:
/*
* ACCEPT INTEGER
*/
					ungetc(cc,fp);
					state = 9;
					*step++ = '\0';
				}
			}
			break;
		case 6:
			if(isdigit(cc)) {
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
					step = buffer;
					state = 0;
				}
			}
			break;
		case 7:
			if(isdigit(cc)) {
				*step++=  cc;
                                state = 8;
                        } else {
				ungetc(cc,fp);
				step = buffer;
				state = 0;
			}
			break;
		case 8:
			if(isdigit(cc)) {
                                *step++=  cc;
                                state = 8;
                        } else {
/*
* ACCEPT INTEGER
*/
					ungetc(cc,fp);
					state = 9;
					*step++ = '\0';
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
	struct stat buf;
	FILE *fd = NULL;
	int totalsize = 0;
	int n;
	char *step = NULL;
	NclStackEntry data_out;
	int has_unlimited = 0;



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
		if(stat(path_string,&buf) == -1) {
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
		if(thetype == NULL) 
			return(NhlFATAL);	
	}

	if((size != -1)&&(!has_unlimited)) {
		totalsize = size;
		
		tmp_ptr = NclMalloc(size*thetype->type_class.size);
		fd = fopen(path_string,"r");
		if((tmp_ptr != NULL)&&(fd != NULL)) {
			
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

			if(thetype->type_class.type & NCL_TYPE_NUMERIC_MASK) {
				for(i = 0; ((i < totalsize)&&(!feof(fd))); i++) {
					if(asciinumeric(fd,thetype->type_class.format,tmp_ptr)) {
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
					} else {
						i--;
						break;
					}
				}
				if(i < totalsize) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"asciiread: End of file reached and only (%d) elements were read from the file, filling remaining elements with the default missing value for the requested type",i+1);
					if(feof(fd)) i++;
					for(;i<totalsize;i++) {
						memcpy(tmp_ptr,&(thetype->type_class.default_mis),thetype->type_class.size);
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
					}
				}
				
			} else if(thetype->type_class.type==Ncl_Typechar) {
				for(i = 0; ((i<totalsize) && !feof(fd)); i++) {
					*(char*)tmp_ptr = fgetc(fd);
					tmp_ptr = (void*)((char*)tmp_ptr+1);
				}
				if(i < totalsize) {	
					NhlPError(NhlWARNING,NhlEUNKNOWN,"asciiread: End of file reached and only (%d) elements were read from the file, filling remaining elements with the default missing value for the requested type",i+1);
					if(feof(fd)) i++;
					for(;i<totalsize;i++) {
						*(char*)tmp_ptr = thetype->type_class.default_mis.charval;
						tmp_ptr = (void*)((char*)tmp_ptr+1);
					}
				}
			} else if(thetype->type_class.type==Ncl_Typestring) {
				char buffer[NCL_MAX_STRING+1];
				char *step;

				step =buffer;
				for(i = 0; ((i<totalsize) && !feof(fd)); i++) {
					for(j = 0; j < NCL_MAX_STRING; j++) {
						if(!feof(fd)) {
							*step = fgetc(fd);
							if(*step == '\n') {
								*step = '\0';
								*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
								step = buffer;
								tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);

								break;
							} else {
								step++;
							}
						} else {
							break;
						}
					}
					if(j >= NCL_MAX_STRING) {
						buffer[NCL_MAX_STRING] = '\0';
						while(!feof(fd)&&(fgetc(fd) != '\n'));
						*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
					} 
				}
				if( i < totalsize ) {
					NhlPError(NhlWARNING,NhlEUNKNOWN,"asciiread: End of file reached and only (%d) elements were read from the file, filling remaining elements with the default missing value for the requested type",i+1);
					if(feof(fd)) i++;
					for(;i<totalsize;i++) {
						*(NclQuark*)tmp_ptr = thetype->type_class.default_mis.stringval; 
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);

					}
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Attempt to read unsupported type");
				return(NhlFATAL);
			}


			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = tmp_md;
			_NclPlaceReturn(data_out);
			fclose(fd);
			return(ret);
		} else if (fd == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: could not open file check permissions");
		}
	} else if(size == -1) {
		fd = fopen(path_string,"r");
		if(fd != NULL) {
			tmp_ptr = NclMalloc(thetype->type_class.size);

			
			totalsize = 0;
			while(!feof(fd)) {
				if(asciinumeric(fd,thetype->type_class.format,tmp_ptr)) {
					totalsize++;
				} else {
					break;
				}
			}
			NclFree(tmp_ptr);
			tmp_ptr = NclMalloc(totalsize*thetype->type_class.size);
			dimsizes[0] = totalsize;
			fclose(fd);
			fd = fopen(path_string,"r");



			
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

			if(thetype->type_class.type & NCL_TYPE_NUMERIC_MASK) {
				for(i = 0; ((i < totalsize)&&(!feof(fd))); i++) {
					if(asciinumeric(fd,thetype->type_class.format,tmp_ptr)) {
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
					} else {
						i--;
						break;
					}
				}
			} else if(thetype->type_class.type==Ncl_Typechar) {
				for(i = 0; ((i<totalsize) && !feof(fd)); i++) {
					*(char*)tmp_ptr = fgetc(fd);
					tmp_ptr = (void*)((char*)tmp_ptr+1);
				}
			} else if(thetype->type_class.type==Ncl_Typestring) {
				char buffer[NCL_MAX_STRING+1];
				char *step;

				step =buffer;
				for(i = 0; ((i<totalsize) && !feof(fd)); i++) {
					for(j = 0; j < NCL_MAX_STRING; j++) {
						if(!feof(fd)) {
							*step = fgetc(fd);
							if(*step == '\n') {
								*step = '\0';
								*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
								step = buffer;
								tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);

								break;
							} else {
								step++;
							}
						} else {
							break;
						}
					}
					if(j >= NCL_MAX_STRING) {
						buffer[NCL_MAX_STRING] = '\0';
						while(!feof(fd)&&(fgetc(fd) != '\n'));
						*(NclQuark*)tmp_ptr = NrmStringToQuark(buffer);
						tmp_ptr = (void*)((char*)tmp_ptr + thetype->type_class.size);
					} 
				}
			} else {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: Attempt to read unsupported type");
				return(NhlFATAL);
			}


			data_out.kind = NclStk_VAL;
			data_out.u.data_obj = tmp_md;
			_NclPlaceReturn(data_out);
			fclose(fd);
			return(ret);
		} else if (fd == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"asciiread: could not open file check permissions");
		}
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
	}
	fd = open(path_string,(O_CREAT | O_RDWR),0666);
	if((tmp_ptr != NULL)&&(fd >= 0)) {
		lseek(fd,0,SEEK_END);
		n = write(fd, tmp_ptr,totalsize);
		close(fd);
		return(ret);
	} else if(fd < 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fbindirwrite: Could not create file");
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
	}
	fd = open(path_string,(O_CREAT | O_RDWR),0666);
	if((tmp_ptr != NULL)&&(fd >= 0)) {
		n = write(fd, tmp_ptr,totalsize);
		close(fd);
		return(ret);
	} else if(fd < 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"cbinwrite: Could not create file");
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
	}
	NGCALLF(nclpfortranwrite,NCLPFORTRANWRITE)(path_string,tmp_ptr,&totalsize,&ret,strlen(path_string));
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


	tmp = rand();
	
	
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
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;
	int *out_val = NULL;
	int i;


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
	if(tmp_md != NULL) {
		out_val = (int*)NclMalloc(tmp_md->multidval.totalelements);
		if(tmp_md->multidval.missing_value.has_missing) {
			for( i = 0 ; i < tmp_md->multidval.totalelements; i ++ ) {
				if(((int*)tmp_md->multidval.val)[i] != tmp_md->multidval.missing_value.value.intval) {
					out_val[i] = (int)abs(((int*)tmp_md->multidval.val)[i]);
				} else {
					out_val[i] = tmp_md->multidval.missing_value.value.intval;
				}
			}
		} else {
			for( i = 0 ; i < tmp_md->multidval.totalelements; i ++ ) {
				out_val[i] = (int)abs(((int*)tmp_md->multidval.val)[i]);
			}
		}
		return(NclReturnValue(
			out_val,
			tmp_md->multidval.n_dims,
			tmp_md->multidval.dim_sizes,
			&((NclTypeClass)nclTypeintClass)->type_class.default_mis,
			NCL_int,
			0
			));
	}
	return(NhlNOERROR);
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
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclStackEntry args;
	NclMultiDValData tmp_md= NULL;
	char *str;
	char *tmp;
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
		tmp = getenv(str);
		if(tmp == NULL) {
			NhlPError(NhlFATAL,NhlEUNKNOWN,"getenv : (%s) is not a defined environment parameter",str);
			outval = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
			return(NclReturnValue(
				&outval,
				1,
				&dimsize,
				&((NclTypeClass)nclTypestringClass)->type_class.default_mis,
				NCL_string,
				1
			));
		} else {
			outval = NrmStringToQuark(tmp);
			return(NclReturnValue(
				&outval,
				1,
				&dimsize,
				NULL,
				NCL_string,
				1
			));
		}
	} else {
		outval = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
		return(NclReturnValue(
			&outval,
			1,
			&dimsize,
			&((NclTypeClass)nclTypestringClass)->type_class.default_mis,
			NCL_string,
			1
		));
	}
	
}




NhlErrorTypes _NclIinttoshort
#if	NhlNeedProto
(void)
#else
()
#endif
{
	static int maxshort = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        int *value;
        int total=1;
        int i;

	if(maxshort == -1) {
/*
* Assuming IEEE integer representation
*/
		maxshort  = (int)pow(2.0,(double)((sizeof(short)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort)||(value[i] > maxshort)||(value[i] == missing.intval)) {
				out_val[i] = (short)missing.intval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		missing2.shortval =  (short)missing.intval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort )||(value[i] > maxshort)) {
				out_val[i] = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		if(has_missing) {
			missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.intval)) {
				out_val[i] = (byte)missing.intval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		missing2.byteval = (byte)missing.intval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		if(has_missing) { 
			missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
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
        char *out_val;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.intval)) {
				out_val[i] = (char)missing.intval;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		missing2.charval = (char)missing.intval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
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
        char *value;
        int total=1;
        int i;

        value = (char*)NclGetArgValue(
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.shortval)) {
				out_val[i] = (byte)  missing.shortval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		missing2.byteval = (byte)  missing.shortval;

	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;	
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		if(has_missing) {
			missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;  
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
        char *out_val;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.shortval)) {
				out_val[i] = (char)missing.shortval;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		missing2.charval = (char)missing.shortval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
				has_missing = 1;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		if(has_missing) {
			missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
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
        char *value;
        int total=1;
        int i;

        value = (char*)NclGetArgValue(
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
		missing2.shortval = (short) missing.charval;

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
	static int maxint = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        long *value;
        int total=1;
        int i;

	if(maxint == -1) {
/*
* Assuming IEEE integer representation
*/
		maxint  = (int)pow(2.0,(double)((sizeof(int)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxint)||(value[i] > maxint)||(value[i] == missing.longval)) {
				out_val[i] = (int)missing.longval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		missing2.intval = (int)missing.longval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxint )||(value[i] > maxint)) {
				out_val[i] = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
				has_missing = 1;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		if(has_missing) {
			missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
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
	static int maxshort = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        long *value;
        int total=1;
        int i;

	if(maxshort == -1) {
/*
* Assuming IEEE integer representation
*/
		maxshort  = (int)pow(2.0,(double)((sizeof(short)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort)||(value[i] > maxshort)||(value[i] == missing.longval)) {
				out_val[i] = (short)missing.longval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		missing2.shortval = (short)missing.longval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort )||(value[i] > maxshort)) {
				out_val[i] = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
				has_missing = 1;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		if(has_missing) {
			missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.longval)) {
				out_val[i] = (byte)missing.longval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		missing2.byteval = (byte)missing.longval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		if(has_missing) {
			missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
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
        char *out_val;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.longval)) {
				out_val[i] = (char)missing.longval;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		missing2.charval = (char)missing.longval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
				has_missing = 1;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		if(has_missing) {
			missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
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
        char *value;
        int total=1;
        int i;

        value = (char*)NclGetArgValue(
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
	static int maxshort = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;

	if(maxshort == -1) {
/*
* Assuming IEEE integer representation
*/
		maxshort  = (int)pow(2.0,(float)((sizeof(short)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort)||(value[i] > maxshort)||(value[i] == missing.floatval)) {
				out_val[i] = (short)missing.floatval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		missing2.shortval  = (short)missing.floatval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort )||(value[i] > maxshort)) {
				out_val[i] = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
				has_missing = 1;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		if(has_missing) {
			missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
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
	static int maxint = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;

	if(maxint == -1) {
/*
* Assuming IEEE integer representation
*/
		maxint  = (int)pow(2.0,(float)((sizeof(int)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxint)||(value[i] > maxint)||(value[i] == missing.floatval)) {
				out_val[i] = (int)missing.floatval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		missing2.intval = (int)missing.floatval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxint )||(value[i] > maxint)) {
				out_val[i] = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
				has_missing = 1;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		if(has_missing) {
			missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
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
	static int maxlong = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        float *value;
        int total=1;
        int i;

	if(maxlong == -1) {
/*
* Assuming IEEE integer representation
*/
		maxlong  = (long)pow(2.0,(float)((sizeof(long)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxlong)||(value[i] > maxlong)||(value[i] == missing.floatval)) {
				out_val[i] = (long)missing.floatval;
			} else {
				out_val[i] = (long)value[i];
			}
		}
		missing2.longval = (long)missing.floatval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxlong )||(value[i] > maxlong)) {
				out_val[i] = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
				has_missing = 1;
			} else {
				out_val[i] = (long)value[i];
			}
		}
		if(has_missing) {
			missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.floatval)) {
				out_val[i] = (byte)missing.floatval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		missing2.byteval = (byte)missing.floatval;

	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		if(has_missing) {
			missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
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
        char *out_val;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.floatval)) {
				out_val[i] = (char)missing.floatval;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		missing2.charval = (char)missing.floatval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
				has_missing = 1;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		if(has_missing) {
			missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
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
        char *value;
        int total=1;
        int i;

        value = (char*)NclGetArgValue(
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.doubleval)) {
				out_val[i] = (byte)missing.doubleval;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		missing2.byteval = (byte)missing.doubleval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
				has_missing = 1;
			} else {
				out_val[i] = (byte)value[i];
			}
		}
		if(has_missing) {
			missing2.byteval = ((NclTypeClass)nclTypebyteClass)->type_class.default_mis.byteval;
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
        char *out_val;
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
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.doubleval)) {
				out_val[i] = (char)missing.doubleval;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		missing2.charval = (char)missing.doubleval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < 0)||(value[i] > 255)) {
				out_val[i] = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
				has_missing = 1;
			} else {
				out_val[i] = (char)value[i];
			}
		}
		if(has_missing) {
			missing2.charval = ((NclTypeClass)nclTypecharClass)->type_class.default_mis.charval;
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
        char *value;
        int total=1;
        int i;

        value = (char*)NclGetArgValue(
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
	static int maxshort = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        short *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;

	if(maxshort == -1) {
/*
* Assuming IEEE integer representation
*/
		maxshort  = (int)pow(2.0,(double)((sizeof(short)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort)||(value[i] > maxshort)||(value[i] == missing.doubleval)) {
				out_val[i] = (short)missing.doubleval;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		missing2.shortval = (short)missing.doubleval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxshort )||(value[i] > maxshort)) {
				out_val[i] = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
				has_missing = 1;
			} else {
				out_val[i] = (short)value[i];
			}
		}
		if(has_missing) {
			missing2.shortval = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis.shortval;
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
	static int maxint = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        int *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;

	if(maxint == -1) {
/*
* Assuming IEEE integer representation
*/
		maxint  = (int)pow(2.0,(double)((sizeof(int)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxint)||(value[i] > maxint)||(value[i] == missing.doubleval)) {
				out_val[i] =  (int)missing.doubleval;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		missing2.intval = (int)missing.doubleval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxint )||(value[i] > maxint)) {
				out_val[i] = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
				has_missing = 1;
			} else {
				out_val[i] = (int)value[i];
			}
		}
		if(has_missing) {
			missing2.intval = ((NclTypeClass)nclTypeintClass)->type_class.default_mis.intval;
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
	static int maxlong = -1;
	NclScalar missing,missing2;
        int has_missing,n_dims,dimsizes[NCL_MAX_DIMENSIONS];
        long *out_val;
	NclBasicDataTypes type;
        double *value;
        int total=1;
        int i;

	if(maxlong == -1) {
/*
* Assuming IEEE integer representation
*/
		maxlong  = (long)pow(2.0,(double)((sizeof(long)*8)-1)) - 1;
	}

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
		for(i = 0; i < total; i++) {
			if((value[i] < -maxlong)||(value[i] > maxlong)||(value[i] == missing.doubleval)) {
				out_val[i] = (long)missing.doubleval;
			} else {
				out_val[i] = (long)value[i];
			}
		}
		missing2.longval = (long)missing.doubleval;
	} else {
		for(i = 0; i < total; i++) {
			if((value[i] < -maxlong )||(value[i] > maxlong)) {
				out_val[i] = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
				has_missing = 1;
			} else {
				out_val[i] = (long)value[i];
			}
		}
		if(has_missing) {	
			missing2.longval = ((NclTypeClass)nclTypelongClass)->type_class.default_mis.longval;
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
		missing2.floatval = (float)missing.doubleval;
	}  
	for(i = 0; i < total; i++) {
		out_val[i] = (float)value[i];
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
	missing2 = ((NclTypeClass)nclTypelongClass)->type_class.default_mis;
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.longval;
			} else {
				if(!sscanf(NrmQuarkToString(value[i]),"%ld",&(out_val[i]))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtolong, input strings must contains numeric digits, replacing with missing value");
					out_val[i] = missing2.longval;

				}
			}
		}
	}  else {
		for(i = 0; i < total; i++) {
			if(!sscanf(NrmQuarkToString(value[i]),"%ld",&(out_val[i]))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtolong, input strings must contains numeric digits, replacing with missing value");
				has_missing = 1;
				out_val[i] = missing2.longval;
		
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
	missing2 = ((NclTypeClass)nclTypeshortClass)->type_class.default_mis;
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.shortval;
			} else {
				if(!sscanf(NrmQuarkToString(value[i]),"%hd",&(out_val[i]))) {
                                        NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtoshort, input strings must contains numeric digits, replacing with missing value");
                                        out_val[i] = missing2.shortval;

				}
			}
		}
	}  else {
		for(i = 0; i < total; i++) {
			if(!sscanf(NrmQuarkToString(value[i]),"%hd",&(out_val[i]))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtoshort, input strings must contains numeric digits, replacing with missing value");
                                has_missing = 1;
                                out_val[i] = missing2.shortval;
 

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
	missing2 = ((NclTypeClass)nclTypeintClass)->type_class.default_mis;
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.intval;
			} else {
				if(!sscanf(NrmQuarkToString(value[i]),"%d",&(out_val[i]))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtointeger, input strings must contains numeric digits, replacing with missing value");
					out_val[i] = missing2.intval;
				}
			}
		}
	}  else {
		for(i = 0; i < total; i++) {
			if(!sscanf(NrmQuarkToString(value[i]),"%d",&(out_val[i]))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtointeger, input strings must contains numeric digits, replacing with missing value");
				has_missing = 1;
				out_val[i] = missing2.intval;
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
	missing2 = ((NclTypeClass)nclTypedoubleClass)->type_class.default_mis;
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.doubleval;
			} else {
				if(!sscanf(NrmQuarkToString(value[i]),"%lf",&(out_val[i]))) {
					NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtodouble, input strings must contains numeric digits, replacing with missing value");
					out_val[i] = missing2.doubleval;

				}
			}
		}
	}  else {
		for(i = 0; i < total; i++) {
			if(!sscanf(NrmQuarkToString(value[i]),"%lf",&(out_val[i]))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtodouble, input strings must contains numeric digits, replacing with missing value");
                                has_missing = 1;
                                out_val[i] = missing2.doubleval;

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
	missing2 = ((NclTypeClass)nclTypefloatClass)->type_class.default_mis;
	if(has_missing) {
		for(i = 0; i < total; i++) {
			if(missing.stringval == value[i]) {
				out_val[i] = missing2.floatval;
			} else {
				if(!sscanf(NrmQuarkToString(value[i]),"%f",&(out_val[i]))) {
					 NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtofloat, input strings must contains numeric digits, replacing with missing value");
					out_val[i] = missing2.floatval;

				}
			}
		}
	}  else {
		for(i = 0; i < total; i++) {
			if(!sscanf(NrmQuarkToString(value[i]),"%f",&(out_val[i]))) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"A bad value was passed to stringtolong, input strings must contains numeric digits, replacing with missing value");
				has_missing = 1;
				out_val[i] = missing2.floatval;
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
	exit(0);
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
						case UNDEF:
							s->type = UNDEF;
							break;
						case VAR:
						case FVAR:
							var = _NclRetrieveRec(s,DONT_CARE);
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
								_NclDestroyObj((NclObj)var->u.data_var);
								var->u.data_var = NULL;
								var->kind = NclStk_NOVAL;
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
						case UNDEF:
							s->type = UNDEF;
							break;
						case VAR:
						case FVAR:
							var = _NclRetrieveRec(s,DONT_CARE);
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
								_NclDestroyObj((NclObj)var->u.data_var);
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
			NhlPError(NhlWARNING, NhlEUNKNOWN,"1dtond : output dimension sizes have fewer elements than input, some data not copied");
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
		NhlPError(NhlWARNING, NhlEUNKNOWN,"1dtond : output dimension sizes not even multiples of input, check output");
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
	void *sum_val = NULL;
	void *div_val = NULL;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,sf;
	int m,n,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
	NclTypeClass the_type;
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
	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double)* n);
		div_val = (void*)NclMalloc(sizeof(double));
		sf = sizeof(double);
		data_type = NCL_double;
		the_type = (NclTypeClass)nclTypedoubleClass;
		if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
		sum_val = (void*)NclMalloc(sz);
	} else {
		out_val = (void*)NclMalloc(sizeof(float)* n);
		div_val = (void*)NclMalloc(sizeof(float));
		sf = sizeof(float);
		data_type = NCL_float;
		the_type = (NclTypeClass)nclTypefloatClass;
		if(tmp_md->multidval.data_type != NCL_float) {
			tmp_md = _NclCoerceData(tmp_md,Ncl_Typefloat,NULL);
			if(tmp_md == NULL) {
				NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg: Could not coerce input data to float,can't continue");
				return(NhlFATAL);
			} else if(tmp_md->multidval.missing_value.has_missing) {
				missing = tmp_md->multidval.missing_value.value;
			}
			sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
		} else if(tmp_md->multidval.missing_value.has_missing) {
			missing = tmp_md->multidval.missing_value.value;
		}
		sum_val = (void*)NclMalloc(sz);
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n ; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&(((char*)tmp_md->multidval.val)[i*m*sz]),&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			count = 0;
			j = 0;
			while((j<m)&&(tmp[j])) {
				j++;
			}
			if(j==m) {
				memcpy(&(((char*)out_val)[i*sz]),&missing,sz);
			} else {
				count = 1;
				memcpy(sum_val,&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),sz);
				j = j+1;
				for(; j < m; j++) {
					if(!tmp[j]) {
						_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),sum_val,NULL,NULL,1,1);
						count = count +1;
					}
				}
				if(_Nclcoerce((NclTypeClass)nclTypefloatClass, &(((char*)out_val)[i*sf]),sum_val,1,NULL,NULL,tmp_md->multidval.type)==NhlFATAL){
					if(_Nclcoerce((NclTypeClass)nclTypedoubleClass, &(((char*)out_val)[i*sf]),sum_val,1,NULL,NULL,tmp_md->multidval.type)==NhlFATAL){
/*
* ERROR
*/
					if(tmp != NULL)
						NclFree(tmp);
					NclFree(sum_val);
					NclFree(div_val);
                               		memcpy(out_val,&missing,the_type->type_class.size);
                               		ret = NclReturnValue(
                               			out_val,
                                       		1,
                                       		dimsizes,
                                       		&missing,
						data_type,
                                      		0
                               		);
					NclFree(dimsizes);
					return(ret);
					} else {
						if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&count,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL)  {
/*
* ERROR
*/
					if(tmp != NULL)
						NclFree(tmp);
					NclFree(sum_val);
					NclFree(div_val);
                               		memcpy(out_val,&missing,the_type->type_class.size);
                               		ret = (NclReturnValue(
                               			out_val,
                                       		1,
                                       		dimsizes,
                                       		&missing,
						data_type,
                                      		0
                               		));
					NclFree(dimsizes);
					return(ret);
						} 
					}
					_Ncldivide((NclTypeClass)nclTypedoubleClass,&(((char*)out_val)[i*sf]),&(((char*)out_val)[i*sf]),div_val,NULL,NULL,1,1);
					data_type = NCL_double;
				} else {
					if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&count,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
/*
* ERROR
*/
					if(tmp != NULL)
						NclFree(tmp);
					NclFree(sum_val);
					NclFree(div_val);
                               		memcpy(out_val,&missing,the_type->type_class.size);
                               		ret = NclReturnValue(
                               			out_val,
                                       		1,
                                       		dimsizes,
                                       		&missing,
						data_type,
                                      		0
                               		);
					NclFree(dimsizes);
					return(ret);
					} 
					_Ncldivide((NclTypeClass)nclTypefloatClass,&(((char*)out_val)[i*sf]),&(((char*)out_val)[i*sf]),div_val,NULL,NULL,1,1);
					data_type = NCL_float;
				}
			}
		}
		if(tmp != NULL)
			NclFree(tmp);
		NclFree(sum_val);
		NclFree(div_val);
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			&missing,
			data_type,
			0);
		NclFree(dimsizes);
		return(ret);
	} else {
		for(i = 0; i < n ; i++) {
			memcpy(sum_val,&(((char*)tmp_md->multidval.val)[i*m*sz]),sz);
			for(j = 1; j < m; j++) {
				_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),sum_val,NULL,NULL,1,1);
			}
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,&(((char*)out_val)[i*sf]),sum_val,1,NULL,NULL,tmp_md->multidval.type) == NhlFATAL) {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,&(((char*)out_val)[i*sf]),sum_val,1,NULL,NULL,tmp_md->multidval.type) == NhlFATAL) {
/*
* ERROR
*/
					if(tmp != NULL)
						NclFree(tmp);
					NclFree(sum_val);
					NclFree(div_val);
                               		memcpy(out_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		ret = (NclReturnValue(
                               			out_val,
                                       		1,
                                       		dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
					NclFree(dimsizes);
					return(ret);
				} else {
					if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&(tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1]),1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
/*
* ERROR
*/
					if(tmp != NULL)
						NclFree(tmp);
					NclFree(sum_val);
					NclFree(div_val);
                               		memcpy(out_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		ret = (NclReturnValue(
                               			out_val,
                                       		1,
                                       		dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
					NclFree(dimsizes);
					return(ret);
						
					}
				}
				_Ncldivide((NclTypeClass)nclTypedoubleClass,&(((char*)out_val)[i*sf]),&(((char*)out_val)[i*sf]),div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&(tmp_md->multidval.dim_sizes[tmp_md->multidval.n_dims -1]),1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
/*
* ERROR
*/
					if(tmp != NULL)
						NclFree(tmp);
					NclFree(sum_val);
					NclFree(div_val);
                               		memcpy(out_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		ret = (NclReturnValue(
                               			out_val,
                                       		1,
                                       		dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
					NclFree(dimsizes);
					return(ret);
				}
				_Ncldivide((NclTypeClass)nclTypefloatClass,&(((char*)out_val)[i*sf]),&(((char*)out_val)[i*sf]),div_val,NULL,NULL,1,1);
				data_type = NCL_float;
			}

		}
		if(tmp != NULL)
			NclFree(tmp);
		NclFree(sum_val);
		NclFree(div_val);
		ret = NclReturnValue(
			out_val,
			nd,
			dimsizes,
			NULL,
			data_type,
			0);
		NclFree(dimsizes);
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
	NclMultiDValData tmp_md = NULL;
	void *sum_val;
	void *out0_val;
	void *out_val;
	void *sum_sqrd_val;
	void *tmp_sqrd_val;
	void *out1_val;
	void *div_val;
	double done = 1.0;
	float fone = 1.0;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,sf,n;
	int m,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
	NclTypeClass the_type;
	NhlErrorTypes r0 = NhlNOERROR;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes r1 = NhlNOERROR;
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

	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double)*n);
                if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
		out0_val = (void*)NclMalloc(sizeof(double));
		out1_val = (void*)NclMalloc(sizeof(double));
		div_val = (void*)NclMalloc(sizeof(double));
		data_type = NCL_double;
		the_type = (NclTypeClass)nclTypedoubleClass;
		sf = sizeof(double);
		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
	} else {
		out_val = (void*)NclMalloc(sizeof(float)*n);
		out0_val = (void*)NclMalloc(sizeof(float));
		out1_val = (void*)NclMalloc(sizeof(float));
		div_val = (void*)NclMalloc(sizeof(float));
		data_type = NCL_float;
		sf = sizeof(float);
		the_type = (NclTypeClass)nclTypefloatClass;
		if(tmp_md->multidval.data_type != NCL_float) {
			tmp_md = _NclCoerceData(tmp_md,Ncl_Typefloat,NULL);
			if(tmp_md == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_avg: Could not coerce input data to float,can't continue");
                                return(NhlFATAL);
                        } else if(tmp_md->multidval.missing_value.has_missing) {
                                missing = tmp_md->multidval.missing_value.value;
                        }
			sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
		} else {
			missing = tmp_md->multidval.missing_value.value;
		}
		sum_val = (void*)NclMalloc(sz);
		sum_sqrd_val = (void*)NclMalloc(sz);
		tmp_sqrd_val = (void*)NclMalloc(sz);
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&((char*)tmp_md->multidval.val)[i*m*sz],&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			j = 0;
			while((tmp[j])&&(j<m)) {
				j++;
			}
			if(j==m) {
				memcpy(&((char*)out_val)[i*sz],&missing,the_type->type_class.size);
			} else {
				count = 1;
				memcpy(sum_val,&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),tmp_md->multidval.type->type_class.size);
				memcpy(sum_sqrd_val,&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),tmp_md->multidval.type->type_class.size);
				_Nclmultiply(tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
				j = j+1;
				for(; j < m; j++) {
				
					if(!tmp[j]) {
						_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[((m*i)+j)*sz]),sum_val,NULL,NULL,1,1);
		/*
		* Sum of squares
		*/
						_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[((m*i)+j)*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),NULL,NULL,1,1);
						_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
						count = count+1;
					}
				}
		/*
		* Square of sum
		*/

				_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);
				
				r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
				r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);	
				if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
					r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
					r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
					if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
							NclFree(div_val);
							NclFree(sum_val);
							NclFree(sum_sqrd_val);
							NclFree(out1_val);
							NclFree(tmp_sqrd_val);
							NclFree(tmp);
							memcpy(out0_val,&missing,the_type->type_class.size);
							ret= NclReturnValue(
								out0_val,
								1,
								dimsizes,
								&missing,
								data_type,
								0
							);
							NclFree(dimsizes);
							return(ret);
					} else {
						if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&count,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
			/*
			* return missing
			*/
								NclFree(div_val);
								NclFree(sum_val);
								NclFree(out1_val);
								NclFree(sum_sqrd_val);
								NclFree(tmp_sqrd_val);
								NclFree(tmp);
								memcpy(out0_val,&missing,the_type->type_class.size);
								ret = NclReturnValue(
									out0_val,
									1,
									dimsizes,
									&missing,
									data_type,
									0
								);
								NclFree(dimsizes);
						}
		/*
		* Divide square of sum by n
		*/
						_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
						_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
						_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
						_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
						memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypedoubleClass)->type_class.size);

						data_type = NCL_double;
					}
				} else  {
					if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&count,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
			/*
			* return missing
			*/
								NclFree(div_val);
								NclFree(sum_val);
								NclFree(out1_val);
								NclFree(sum_sqrd_val);
								NclFree(tmp_sqrd_val);
								NclFree(tmp);
								memcpy(out0_val,&missing,the_type->type_class.size);
								ret = NclReturnValue(
									out0_val,
									1,
									dimsizes,
									&missing,
									data_type,
									0
								);
								NclFree(dimsizes);
								return(ret);
					}
					_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
					_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
					data_type = NCL_float;
					memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypefloatClass)->type_class.size);

				}
			}
		}
	               if(tmp != NULL)
                        NclFree(tmp);
                NclFree(sum_val);
                NclFree(div_val);
                ret = NclReturnValue(
                        out_val,
                        nd,
                        dimsizes,
                        &missing,
                        data_type,
                        0);
                NclFree(dimsizes);
                return(ret);
	} else {
		for(i = 0; i < n; i++) {
			memcpy(sum_val,&(((char*)tmp_md->multidval.val)[i*m*sz]),tmp_md->multidval.type->type_class.size);
			memcpy(sum_sqrd_val,&(((char*)tmp_md->multidval.val)[i*m*sz]),tmp_md->multidval.type->type_class.size);
			_Nclmultiply((NclTypeClass)tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
			for(j = 1; j < m; j++) {
				_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),sum_val,NULL,NULL,1,1);
				_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),NULL,NULL,1,1);
				_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
			}
			_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);

			r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
			r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
			if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
				r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
				r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
				if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
						*dimsizes = 1;
						ret = NclReturnValue(
							out0_val,
							1,
							dimsizes,
							&the_type->type_class.default_mis,
							data_type,
							0
						);
						NclFree(dimsizes);
						return(ret);
				} else {
					if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&m,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
		/*
		* return missing
		*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
						ret = NclReturnValue(
							out0_val,
							1,
							dimsizes,
							&the_type->type_class.default_mis,
							data_type,
							0
						);
						NclFree(dimsizes);
						return(ret);
					}
					_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
					_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
					memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypedoubleClass)->type_class.size);
					data_type = NCL_double;
				}
			} else  {
				if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&m,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
		/*
		* return missing
		*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
						*dimsizes = 1;
						ret = NclReturnValue(
							out0_val,
							1,
							dimsizes,
							&the_type->type_class.default_mis,
							data_type,
							0
						);
						return(ret);
				}
				_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
				_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypefloatClass)->type_class.size);
				data_type = NCL_float;
			}
		}
	        if(tmp != NULL)
                        NclFree(tmp);
                NclFree(sum_val);
                NclFree(div_val);
                ret = NclReturnValue(
                        out_val,
                        nd,
                        dimsizes,
                        &the_type->type_class.default_mis,
                        data_type,
                        0);
                NclFree(dimsizes);
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
	void *sum_val;
	void *out0_val;
	void *sum_sqrd_val;
	void *tmp_sqrd_val;
	void *out1_val;
	void *div_val;
	double done = 1.0;
	float fone = 1.0;
	int dimsizes = 1;
	logical *tmp = NULL;
	int i,n;
	short tmp1;
	NclBasicDataTypes data_type;
	NclTypeClass the_type;
	NhlErrorTypes r0 = NhlNOERROR;
	NhlErrorTypes r1 = NhlNOERROR;
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

	if(tmp_md->multidval.data_type == NCL_double) {
		out0_val = (void*)NclMalloc(sizeof(double));
		out1_val = (void*)NclMalloc(sizeof(double));
		div_val = (void*)NclMalloc(sizeof(double));
		data_type = NCL_double;
		the_type = (NclTypeClass)nclTypedoubleClass;
		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
	} else {
		out0_val = (void*)NclMalloc(sizeof(float));
		out1_val = (void*)NclMalloc(sizeof(float));
		div_val = (void*)NclMalloc(sizeof(float));
		data_type = NCL_float;
		the_type = (NclTypeClass)nclTypefloatClass;
		if(tmp_md->multidval.data_type != NCL_float) {
			tmp_md = _NclCoerceData(tmp_md,Ncl_Typefloat,NULL);
			if(tmp_md == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"avg: Could not coerce input data to float,can't continue");
                                return(NhlFATAL);
                        } else if(tmp_md->multidval.missing_value.has_missing){
                                missing = tmp_md->multidval.missing_value.value;
                        }

		} else if(tmp_md->multidval.missing_value.has_missing) {
                                missing = tmp_md->multidval.missing_value.value;
                }

		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		i = 0;
		while((tmp[i])&&(i<tmp_md->multidval.totalelements)) {
			i++;
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				NclFree(div_val);
				NclFree(sum_val);
				NclFree(sum_sqrd_val);
				NclFree(tmp_sqrd_val);
				NclFree(out1_val);
				NclFree(tmp);
				memcpy(out0_val,&missing,the_type->type_class.size);
				return(NclReturnValue(
					out0_val,
					1,
					&dimsizes,
					&missing,
					data_type,
					0
				));
		}
		n = 1;
		memcpy(sum_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		memcpy(sum_sqrd_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		_Nclmultiply(tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
		
			if(!tmp[i]) {
				_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),sum_val,NULL,NULL,1,1);
/*
* Sum of squares
*/
				_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),NULL,NULL,1,1);
				_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
				n = n+1;
			}
		}
/*
* Square of sum
*/

		_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);
		
		r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
		r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);	
		if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
			r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
			r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
			if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(sum_sqrd_val);
					NclFree(out1_val);
					NclFree(tmp_sqrd_val);
					NclFree(tmp);
                               		memcpy(out0_val,&missing,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&missing,
						data_type,
                                      		0
                               		));
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&n,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						NclFree(tmp);
                                		memcpy(out0_val,&missing,the_type->type_class.size);
                               			return(NclReturnValue(
                                       			out0_val,
                                       			1,
                                       			&dimsizes,
                                       			&missing,
							data_type,
                                      			0
                               			));
				}
/*
* Divide square of sum by n
*/
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			}
		} else  {
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&n,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						NclFree(tmp);
                                		memcpy(out0_val,&missing,the_type->type_class.size);
                               			return(NclReturnValue(
                                       			out0_val,
                                       			1,
                                       			&dimsizes,
                                       			&missing,
							data_type,
                                      			0
                               			));
			}
			_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
			_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
			data_type = NCL_float;
		}
	} else {
		memcpy(sum_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		memcpy(sum_sqrd_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		_Nclmultiply((NclTypeClass)tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),sum_val,NULL,NULL,1,1);
			_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),NULL,NULL,1,1);
			_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
		}
		_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);

                r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
                r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
		if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
			r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
			r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
			if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(out1_val);
					NclFree(sum_sqrd_val);
					NclFree(tmp_sqrd_val);
                               		memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&tmp_md->multidval.totalelements,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(out1_val);
					NclFree(sum_sqrd_val);
					NclFree(tmp_sqrd_val);
                               		memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
				}
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			}
		} else  {
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&tmp_md->multidval.totalelements,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(out1_val);
					NclFree(sum_sqrd_val);
					NclFree(tmp_sqrd_val);
                               		memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
			}
			_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
			_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
			data_type = NCL_float;
		}
	}

	NclFree(div_val);
	NclFree(sum_val);
	if(tmp != NULL) 
		NclFree(tmp);
	return(NclReturnValue(
		out1_val,
		1,
		&dimsizes,
		NULL,
		data_type,
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
	NclMultiDValData tmp_md = NULL;
	void *sum_val;
	void *out0_val;
	void *out_val;
	void *sum_sqrd_val;
	void *tmp_sqrd_val;
	void *out1_val;
	void *div_val;
	double done = 1.0;
	float fone = 1.0;
	int *dimsizes = NULL;
	logical *tmp = NULL;
	int i,j,sf,n;
	int m,sz;
	int nd,count;
	short tmp1;
	NclBasicDataTypes data_type;
	NclTypeClass the_type;
	NhlErrorTypes r0 = NhlNOERROR;
	NhlErrorTypes ret = NhlNOERROR;
	NhlErrorTypes r1 = NhlNOERROR;
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




	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double)*n);

		out0_val = (void*)NclMalloc(sizeof(double));
		out1_val = (void*)NclMalloc(sizeof(double));
		div_val = (void*)NclMalloc(sizeof(double));
		data_type = NCL_double;
		the_type = (NclTypeClass)nclTypedoubleClass;
		sf = sizeof(double);
		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
	} else {
		out_val = (void*)NclMalloc(sizeof(float)*n);
		out0_val = (void*)NclMalloc(sizeof(float));
		out1_val = (void*)NclMalloc(sizeof(float));
		div_val = (void*)NclMalloc(sizeof(float));
		data_type = NCL_float;
		sf = sizeof(float);
		the_type = (NclTypeClass)nclTypefloatClass;
		if(tmp_md->multidval.data_type != NCL_float) {
			tmp_md = _NclCoerceData(tmp_md,Ncl_Typefloat,NULL);
			sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
                        if(tmp_md == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"dim_stddev: Could not coerce input data to float,can't continue");
                                return(NhlFATAL);
                        } else if(tmp_md->multidval.missing_value.has_missing) {
                                missing = tmp_md->multidval.missing_value.value;
                        }
                        sz = ((NclTypeClass)nclTypefloatClass)->type_class.size;
                } else if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }

		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		for(i = 0; i < n; i++) {
			_Ncleq(tmp_md->multidval.type,tmp,&((char*)tmp_md->multidval.val)[i*m*sz],&(tmp_md->multidval.missing_value.value),NULL,NULL,m,1);
			j = 0;
			while((tmp[j])&&(j<m)) {
				j++;
			}
			if(j==m) {
				memcpy(&((char*)out_val)[i*sz],&missing,the_type->type_class.size);
			} else {
				count = 1;
				memcpy(sum_val,&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),tmp_md->multidval.type->type_class.size);
				memcpy(sum_sqrd_val,&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),tmp_md->multidval.type->type_class.size);
				_Nclmultiply(tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
				j = j+1;
				for(; j < m; j++) {
				
					if(!tmp[j]) {
						_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[((m*i)+j)*sz]),sum_val,NULL,NULL,1,1);
	/*
	* Sum of squares
	*/
						_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[((m*i)+j)*sz]),&(((char*)tmp_md->multidval.val)[((i*m) + j)*sz]),NULL,NULL,1,1);
						_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
						count = count+1;
					}
				}
	/*
	* Square of sum
	*/

				_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);
				
				r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
				r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);	
				if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
					r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
					r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
					if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
							NclFree(div_val);
							NclFree(sum_val);
							NclFree(sum_sqrd_val);
							NclFree(out1_val);
							NclFree(tmp_sqrd_val);
							NclFree(tmp);
							memcpy(out0_val,&missing,the_type->type_class.size);
							ret= NclReturnValue(
								out0_val,
								1,
								dimsizes,
								&missing,
								data_type,
								0
							);
							NclFree(dimsizes);
							return(ret);
					} else {
						if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&count,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
			/*
			* return missing
			*/
								NclFree(div_val);
								NclFree(sum_val);
								NclFree(out1_val);
								NclFree(sum_sqrd_val);
								NclFree(tmp_sqrd_val);
								NclFree(tmp);
								memcpy(out0_val,&missing,the_type->type_class.size);
								ret = NclReturnValue(
									out0_val,
									1,
									dimsizes,
									&missing,
									data_type,
									0
								);
								NclFree(dimsizes);
						}
		/*
		* Divide square of sum by n
		*/
						_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
						_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
						_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
						_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
						*((double*)div_val) = .5;
						_Nclexponent((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
						memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypedoubleClass)->type_class.size);

						data_type = NCL_double;
					}
				} else  {
					if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&count,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
			/*
			* return missing
			*/
								NclFree(div_val);
								NclFree(sum_val);
								NclFree(out1_val);
								NclFree(sum_sqrd_val);
								NclFree(tmp_sqrd_val);
								NclFree(tmp);
								memcpy(out0_val,&missing,the_type->type_class.size);
								ret = NclReturnValue(
									out0_val,
									1,
									dimsizes,
									&missing,
									data_type,
									0
								);
								NclFree(dimsizes);
								return(ret);
					}
					_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
					_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
					*((float*)div_val) = .5;
					_Nclexponent((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
					data_type = NCL_float;
					memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypefloatClass)->type_class.size);

				}
			}
		}
	        if(tmp != NULL)
                        NclFree(tmp);
                NclFree(sum_val);
                NclFree(div_val);
                ret = NclReturnValue(
                        out_val,
                        nd,
                        dimsizes,
                        &missing,
                        data_type,
                        0);
                NclFree(dimsizes);
                return(ret);
	} else {
		for(i = 0; i < n; i++) {
			memcpy(sum_val,&(((char*)tmp_md->multidval.val)[i*m*sz]),tmp_md->multidval.type->type_class.size);
			memcpy(sum_sqrd_val,&(((char*)tmp_md->multidval.val)[i*m*sz]),tmp_md->multidval.type->type_class.size);
			_Nclmultiply((NclTypeClass)tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
			for(j = 1; j < m; j++) {
				_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),sum_val,NULL,NULL,1,1);
				_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),&(((char*)tmp_md->multidval.val)[((m * i) + j)*sz]),NULL,NULL,1,1);
				_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
			}
			_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);

			r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
			r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
			if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
				r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
				r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
				if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
						*dimsizes = 1;
						ret = NclReturnValue(
							out0_val,
							1,
							dimsizes,
							&the_type->type_class.default_mis,
							data_type,
							0
						);
						NclFree(dimsizes);
						return(ret);
				} else {
					if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&m,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
		/*
		* return missing
		*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
						ret = NclReturnValue(
							out0_val,
							1,
							dimsizes,
							&the_type->type_class.default_mis,
							data_type,
							0
						);
						NclFree(dimsizes);
						return(ret);
					}
					_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
					_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
					_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
					*((double*)div_val) = .5;
					_Nclexponent((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
					memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypedoubleClass)->type_class.size);
					data_type = NCL_double;
				}
			} else  {
				if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&m,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
		/*
		* return missing
		*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
						*dimsizes = 1;
						ret = NclReturnValue(
							out0_val,
							1,
							dimsizes,
							&the_type->type_class.default_mis,
							data_type,
							0
						);
						return(ret);
				}
				_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
				_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				*((float*)div_val) = .5;
				_Nclexponent((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				memcpy(&(((char*)out_val)[i*sf]),out1_val,((NclTypeClass)nclTypefloatClass)->type_class.size);
				data_type = NCL_float;
			}
		}
	        if(tmp != NULL)
                        NclFree(tmp);
                NclFree(sum_val);
                NclFree(div_val);
                ret = NclReturnValue(
                        out_val,
                        nd,
                        dimsizes,
                        &the_type->type_class.default_mis,
                        data_type,
                        0);
                NclFree(dimsizes);
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
	void *sum_val;
	void *out0_val;
	void *sum_sqrd_val;
	void *tmp_sqrd_val;
	void *out1_val;
	void *div_val;
	double done = 1.0;
	float fone = 1.0;
	int dimsizes = 1;
	logical *tmp = NULL;
	int i,n;
	short tmp1;
	NclBasicDataTypes data_type;
	NclTypeClass the_type;
	NhlErrorTypes r0 = NhlNOERROR;
	NhlErrorTypes r1 = NhlNOERROR;
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
		out0_val = (void*)NclMalloc(sizeof(double));
		div_val = (void*)NclMalloc(sizeof(double));
		data_type = NCL_double;
		the_type = (NclTypeClass)nclTypedoubleClass;
		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
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
		out0_val = (void*)NclMalloc(sizeof(float));
		div_val = (void*)NclMalloc(sizeof(float));
		data_type = NCL_float;
		the_type = (NclTypeClass)nclTypefloatClass;
		if(tmp_md->multidval.data_type != NCL_float) {
			tmp_md = _NclCoerceData(tmp_md,Ncl_Typefloat,NULL);
                        if(tmp_md == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"avg: Could not coerce input data to float,can't continue");
                                return(NhlFATAL);
                        } else if(tmp_md->multidval.missing_value.has_missing){
                                missing = tmp_md->multidval.missing_value.value;
                        }
                } else if(tmp_md->multidval.missing_value.has_missing) {
                         missing = tmp_md->multidval.missing_value.value;
                }
		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		sum_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		tmp_sqrd_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		i = 0;
		while((tmp[i])&&(i<tmp_md->multidval.totalelements)) {
			i++;
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				NclFree(div_val);
				NclFree(sum_val);
				NclFree(sum_sqrd_val);
				NclFree(tmp_sqrd_val);
				NclFree(out1_val);
				NclFree(tmp);
				memcpy(out0_val,&missing,the_type->type_class.size);
				return(NclReturnValue(
					out0_val,
					1,
					&dimsizes,
					&missing,
					data_type,
					0
				));
		}
		n = 1;
		memcpy(sum_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		memcpy(sum_sqrd_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		_Nclmultiply(tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
		
			if(!tmp[i]) {
				_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),sum_val,NULL,NULL,1,1);
/*
* Sum of squares
*/
				_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),NULL,NULL,1,1);
				_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
				n = n+1;
			}
		}
/*
* Square of sum
*/

		_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);
		
		r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
		r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);	
		if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
			r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
			r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
			if((r0 == NhlFATAL)&& (r1 == NhlFATAL)) {
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(sum_sqrd_val);
					NclFree(out1_val);
					NclFree(tmp_sqrd_val);
					NclFree(tmp);
                               		memcpy(out0_val,&missing,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&missing,
						data_type,
                                      		0
                               		));
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&n,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						NclFree(tmp);
                                		memcpy(out0_val,&missing,the_type->type_class.size);
                               			return(NclReturnValue(
                                       			out0_val,
                                       			1,
                                       			&dimsizes,
                                       			&missing,
							data_type,
                                      			0
                               			));
				}
/*
* Divide square of sum by n
*/
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				*((double*)div_val) = .5;
				_Nclexponent((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			}
		} else  {
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&n,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(out1_val);
						NclFree(sum_sqrd_val);
						NclFree(tmp_sqrd_val);
						NclFree(tmp);
                                		memcpy(out0_val,&missing,the_type->type_class.size);
                               			return(NclReturnValue(
                                       			out0_val,
                                       			1,
                                       			&dimsizes,
                                       			&missing,
							data_type,
                                      			0
                               			));
			}
			_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
			_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
			*((float*)div_val) = .5;
			_Nclexponent((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
			data_type = NCL_float;
		}
	} else {
		memcpy(sum_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		memcpy(sum_sqrd_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		_Nclmultiply((NclTypeClass)tmp_md->multidval.type,sum_sqrd_val,sum_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),sum_val,NULL,NULL,1,1);
			_Nclmultiply(tmp_md->multidval.type,tmp_sqrd_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),NULL,NULL,1,1);
			_Nclplus(tmp_md->multidval.type,sum_sqrd_val,tmp_sqrd_val,sum_sqrd_val,NULL,NULL,1,1);
		}
		_Nclmultiply(tmp_md->multidval.type,sum_val,sum_val,sum_val,NULL,NULL,1,1);

                r0 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
                r1 = _Nclcoerce((NclTypeClass)nclTypefloatClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
		if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
			r0 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out0_val,sum_val,1,NULL,NULL,tmp_md->multidval.type);
			r1 = _Nclcoerce((NclTypeClass)nclTypedoubleClass,out1_val,sum_sqrd_val,1,NULL,NULL,tmp_md->multidval.type);
			if((r0 == NhlFATAL)&&(r1 == NhlFATAL)) {
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(out1_val);
					NclFree(sum_sqrd_val);
					NclFree(tmp_sqrd_val);
                               		memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&tmp_md->multidval.totalelements,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(out1_val);
					NclFree(sum_sqrd_val);
					NclFree(tmp_sqrd_val);
                               		memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
				}
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
				_Nclminus((NclTypeClass)nclTypedoubleClass,div_val,div_val,&done,NULL,NULL,1,1);
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				*((double*)div_val) = .5;
				_Nclexponent((NclTypeClass)nclTypedoubleClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			}
		} else  {
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&tmp_md->multidval.totalelements,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(out1_val);
					NclFree(sum_sqrd_val);
					NclFree(tmp_sqrd_val);
                               		memcpy(out0_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out0_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
			}
			_Ncldivide((NclTypeClass)nclTypefloatClass,out0_val,out0_val,div_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,out1_val,out1_val,out0_val,NULL,NULL,1,1);
			_Nclminus((NclTypeClass)nclTypefloatClass,div_val,div_val,&fone,NULL,NULL,1,1);
			_Ncldivide((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
			*((float*)div_val) = .5;
			_Nclexponent((NclTypeClass)nclTypefloatClass,out1_val,out1_val,div_val,NULL,NULL,1,1);
			data_type = NCL_float;
		}
	}

	NclFree(div_val);
	NclFree(sum_val);
	if(tmp != NULL) 
		NclFree(tmp);
	return(NclReturnValue(
		out1_val,
		1,
		&dimsizes,
		NULL,
		data_type,
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
	void *sum_val;
	void *out_val;
	void *div_val;
	int dimsizes = 1;
	logical *tmp = NULL;
	int i,n;
	short tmp1;
	NclBasicDataTypes data_type;
	NclTypeClass the_type;
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

	if(tmp_md->multidval.data_type == NCL_double) {
		out_val = (void*)NclMalloc(sizeof(double));
		div_val = (void*)NclMalloc(sizeof(double));
		data_type = NCL_double;
		the_type = (NclTypeClass)nclTypedoubleClass;
		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
		if(tmp_md->multidval.missing_value.has_missing) {
                        missing = tmp_md->multidval.missing_value.value;
                }
	} else {
		out_val = (void*)NclMalloc(sizeof(float));
		div_val = (void*)NclMalloc(sizeof(float));
		data_type = NCL_float;
		the_type = (NclTypeClass)nclTypefloatClass;
		if(tmp_md->multidval.data_type != NCL_float) {
			tmp_md = _NclCoerceData(tmp_md,Ncl_Typefloat,NULL);
                        if(tmp_md == NULL) {
                                NhlPError(NhlFATAL,NhlEUNKNOWN,"avg: Could not coerce input data to float,can't continue");
                                return(NhlFATAL);
                        } else if(tmp_md->multidval.missing_value.has_missing){
                                missing = tmp_md->multidval.missing_value.value;
                        }
		} else if(tmp_md->multidval.missing_value.has_missing) {
                                missing = tmp_md->multidval.missing_value.value;
                }

		sum_val = (void*)NclMalloc(tmp_md->multidval.type->type_class.size);
	}

	if(tmp_md->multidval.missing_value.has_missing) {
		tmp = (logical*)NclMalloc(sizeof(logical)*tmp_md->multidval.totalelements);
		_Ncleq(tmp_md->multidval.type,tmp,tmp_md->multidval.val,&(tmp_md->multidval.missing_value.value),NULL,NULL,tmp_md->multidval.totalelements,1);
		i = 0;
		while((tmp[i])&&(i<tmp_md->multidval.totalelements)) {
			i++;
		}
		if(i==tmp_md->multidval.totalelements) {
/*
* return missing
*/
				NclFree(div_val);
				NclFree(sum_val);
				NclFree(tmp);
				memcpy(out_val,&missing,the_type->type_class.size);
				return(NclReturnValue(
					out_val,
					1,
					&dimsizes,
					&missing,
					data_type,
					0
				));
		}
		n = 1;
		memcpy(sum_val,&(((char*)tmp_md->multidval.val)[i*tmp_md->multidval.type->type_class.size]),tmp_md->multidval.type->type_class.size);
		i = i+1;
		for(; i < tmp_md->multidval.totalelements; i++) {
		
			if(!tmp[i]) {
				_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),sum_val,NULL,NULL,1,1);
				n = n+1;
			}
		}
		if(_Nclcoerce((NclTypeClass)nclTypefloatClass,out_val,sum_val,1,NULL,NULL,tmp_md->multidval.type) == NhlFATAL) {
			if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,out_val,sum_val,1,NULL,NULL,tmp_md->multidval.type) == NhlFATAL) {
					NclFree(div_val);
					NclFree(sum_val);
					NclFree(tmp);
                               		memcpy(out_val,&missing,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out_val,
                                       		1,
                                       		&dimsizes,
                                       		&missing,
						data_type,
                                      		0
                               		));
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&n,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(tmp);
                                		memcpy(out_val,&missing,the_type->type_class.size);
                               			return(NclReturnValue(
                                       			out_val,
                                       			1,
                                       			&dimsizes,
                                       			&missing,
							data_type,
                                      			0
                               			));
				}
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out_val,out_val,div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			}
		} else  {
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&n,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
						NclFree(div_val);
						NclFree(sum_val);
						NclFree(tmp);
                                		memcpy(out_val,&missing,the_type->type_class.size);
                               			return(NclReturnValue(
                                       			out_val,
                                       			1,
                                       			&dimsizes,
                                       			&missing,
							data_type,
                                      			0
                               			));
			}
			_Ncldivide((NclTypeClass)nclTypefloatClass,out_val,out_val,div_val,NULL,NULL,1,1);
			data_type = NCL_float;
		}
	} else {
		memcpy(sum_val,tmp_md->multidval.val,tmp_md->multidval.type->type_class.size);
		for(i = 1; i < tmp_md->multidval.totalelements; i++) {
			_Nclplus(tmp_md->multidval.type,sum_val,&(((char*)tmp_md->multidval.val)[tmp_md->multidval.type->type_class.size*i]),sum_val,NULL,NULL,1,1);
		}
		if(_Nclcoerce((NclTypeClass)nclTypefloatClass,out_val,sum_val,1,NULL,NULL,tmp_md->multidval.type) == NhlFATAL) {
			if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,out_val,sum_val,1,NULL,NULL,tmp_md->multidval.type) == NhlFATAL) {
					NclFree(div_val);
					NclFree(sum_val);
                               		memcpy(out_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
			} else {
				if(_Nclcoerce((NclTypeClass)nclTypedoubleClass,div_val,&tmp_md->multidval.totalelements,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
					NclFree(div_val);
					NclFree(sum_val);
                               		memcpy(out_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
				}
				_Ncldivide((NclTypeClass)nclTypedoubleClass,out_val,out_val,div_val,NULL,NULL,1,1);
				data_type = NCL_double;
			}
		} else  {
			if(_Nclcoerce((NclTypeClass)nclTypefloatClass,div_val,&tmp_md->multidval.totalelements,1,NULL,NULL,(NclTypeClass)nclTypeintClass) == NhlFATAL) {
	/*
	* return missing
	*/
					NclFree(div_val);
					NclFree(sum_val);
                               		memcpy(out_val,&the_type->type_class.default_mis,the_type->type_class.size);
                               		return(NclReturnValue(
                               			out_val,
                                       		1,
                                       		&dimsizes,
                                       		&the_type->type_class.default_mis,
						data_type,
                                      		0
                               		));
			}
			_Ncldivide((NclTypeClass)nclTypefloatClass,out_val,out_val,div_val,NULL,NULL,1,1);
			data_type = NCL_float;
		}
	}

	NclFree(div_val);
	NclFree(sum_val);
	if(tmp != NULL) 
		NclFree(tmp);
	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		data_type,
		0
	));
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
		_NclIsMissing(tmp_md0,tmp_md0->multidval.val)||
		_NclIsMissing(tmp_md0,tmp_md0->multidval.val)) {

		NhlPError(NhlFATAL,NhlEUNKNOWN,"ispan: Missing value detected in input, can't conitue");
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
		out_val = (int*)NclMalloc(dimsizes*sizeof(float));
		for(i = 0; i < dimsizes; i++) {
			out_val[i] = strt + i * spacing;
		}
	} else if((fnsh - strt) < 0) {
		out_val = (int*)NclMalloc(dimsizes*sizeof(float));
		for(i = 0; i < dimsizes; i++) {
			out_val[i] = strt - i * spacing;
		}
	} else {
		out_val = (int*)NclMalloc(sizeof(float));
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
	NclStackEntry data0;
	NclStackEntry data1;
	NclStackEntry data2;
	NclMultiDValData tmp_md0 = NULL;
	NclMultiDValData tmp_md1 = NULL;
	NclMultiDValData tmp_md2 = NULL;
	float *out_val;
	int dimsizes = 1;
	int i;
	float strt;
	float fnsh;
	float spacing;

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
		_NclIsMissing(tmp_md0,tmp_md0->multidval.val)||
		_NclIsMissing(tmp_md0,tmp_md0->multidval.val)) {

		NhlPError(NhlFATAL,NhlEUNKNOWN,"fspan: Missing value detected in input, can't conitue");
		return(NhlFATAL);
	}

	
	dimsizes = *(int*)tmp_md2->multidval.val;
	if(dimsizes <= 0) {
		NhlPError(NhlFATAL,NhlEUNKNOWN,"fspan: number of elements parameter is less-than-or-equal-to zero, can't conitue");
		return(NhlFATAL);
	} else if(dimsizes > 1) {
		fnsh = *(float*)tmp_md1->multidval.val;
		strt = *(float*)tmp_md0->multidval.val;

		spacing = (fnsh - strt)/(float)(dimsizes - 1);

		out_val = (float*)NclMalloc(dimsizes*sizeof(float));
		for(i = 0; i < dimsizes; i++) {
			out_val[i] = strt + i * spacing;
		}
	} else {
		out_val = (float*)NclMalloc(sizeof(float));
		out_val[0] =  *(float*)tmp_md0->multidval.val;
	}


	return(NclReturnValue(
		out_val,
		1,
		&dimsizes,
		NULL,
		NCL_float,
		0
	));
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
	out_val = (void*)NclMalloc(tmp_md0->multidval.totalsize);
	_Nclne(mask_type,tmp0,mask_grid,mask_val,NULL,NULL,tmp_md1->multidval.totalelements,1);
	for(j = 0; j < n; j++ ) {
		memcpy(&((char*)out_val)[j*tmp_md1->multidval.totalsize],&((char*)tmp_md0->multidval.val)[j*tmp_md1->multidval.totalsize],tmp_md1->multidval.totalsize);
		if(tmp_md0->multidval.missing_value.has_missing) {
			for(i = 0; i < tmp_md1->multidval.totalelements; i++) {
				if(tmp0[i]) {
					memcpy(&(((char*)out_val)[j*tmp_md1->multidval.totalsize+ (i*tmp_md0->multidval.type->type_class.size)]),&tmp_md0->multidval.missing_value.value,tmp_md0->multidval.type->type_class.size);
				} 
			}
		} else {
			for(i = 0; i < tmp_md1->multidval.totalelements; i++) {
				if(tmp0[i]) {
					memcpy(&(((char*)out_val)[j*tmp_md1->multidval.totalsize+ (i*tmp_md0->multidval.type->type_class.size)]),&tmp_md0->multidval.type->type_class.default_mis,tmp_md0->multidval.type->type_class.size);
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
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
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
		for(i = 0; i < tmp_md->multidval.totalelements; i++) {
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
	if(tmp_md->multidval.type->type_class.type & NCL_VAL_NUMERIC_MASK) {
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
		NhlPError(NhlWARNING,NhlEUNKNOWN,"gaus: missing value in input can not computes gaussian vals");
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

NhlErrorTypes _NclIvinth2p
#if	NhlNeedProto
(void)
#else
()
#endif
{
	NclVar	tmp_var;
	NclMultiDValData x_coord_md;
	NclVar lev_coord_var;
	NclMultiDValData lev_coord_md;
	int ids[3];
	NclDimRec dim_info[3];

	NclStackEntry data,val,plevo_val;
	NclMultiDValData tmp_md;
	float *datai = NULL,*datao;
	int datao_dimsizes[3];
	int datai_n_dims,datai_has_missing;
	NclBasicDataTypes datai_type;
	int datai_dimsizes[3];
	NclScalar datai_missing;
	NclQuark plevo_quark;

	float *hbcofa = NULL;
	int hbcofa_n_dims,hbcofa_has_missing;
	NclBasicDataTypes hbcofa_type;
	int hbcofa_dimsizes;
	NclScalar hbcofa_missing;

	float *hbcofb = NULL;
	int hbcofb_n_dims,hbcofb_has_missing;
	NclBasicDataTypes hbcofb_type;
	int hbcofb_dimsizes;
	NclScalar hbcofb_missing;

	float *plevo = NULL;
	float *plevo2 = NULL;
	int plevo_n_dims,plevo_has_missing;
	NclBasicDataTypes plevo_type;
	int plevo_dimsizes;
	NclScalar plevo_missing;
	

	int *intyp = NULL;
	int intyp_has_missing;
	NclScalar intyp_missing;

	float *psfc = NULL;
	int psfc_n_dims,psfc_has_missing;
	NclBasicDataTypes psfc_type;
	int psfc_dimsizes[2];
	NclScalar psfc_missing;

	float *p0 = NULL;
	int p0_has_missing;
	NclScalar p0_missing;

	int *ilev = NULL;
	int ilev_has_missing;
	NclScalar ilev_missing;

	logical* kxtrp = NULL;
	int kxtrp_has_missing;
	NclScalar kxtrp_missing;

	float *plevi;
	NclScalar missing;
	int was_val = 0;

        val = _NclGetArg(0,9,DONT_CARE);
/*
* Should be constrained to be a SCALAR md
*/
        switch(val.kind) {
        case NclStk_VAL:
		was_val = 1;
                tmp_md = val.u.data_obj;
		datai = (float*)tmp_md->multidval.val;
		datai_n_dims = tmp_md->multidval.n_dims;
		datai_dimsizes[0] = tmp_md->multidval.dim_sizes[0];
		datai_dimsizes[1] = tmp_md->multidval.dim_sizes[1];
		datai_dimsizes[2] = tmp_md->multidval.dim_sizes[2];
		datai_has_missing = tmp_md->multidval.missing_value.has_missing;
		datai_missing = tmp_md->multidval.missing_value.value;
		datai_type = tmp_md->multidval.data_type;
                break;
        case NclStk_VAR:
                tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
		datai = (float*)tmp_md->multidval.val;
		datai_n_dims = tmp_md->multidval.n_dims;
		datai_dimsizes[0] = tmp_md->multidval.dim_sizes[0];
		datai_dimsizes[1] = tmp_md->multidval.dim_sizes[1];
		datai_dimsizes[2] = tmp_md->multidval.dim_sizes[2];
		datai_has_missing = tmp_md->multidval.missing_value.has_missing;
		datai_missing = tmp_md->multidval.missing_value.value;
		datai_type = tmp_md->multidval.data_type;
                break;
        default:
                return(NhlFATAL);
        }
        hbcofa = (float*)NclGetArgValue(
                        1,
                        9,
                        &hbcofa_n_dims,
                        &hbcofa_dimsizes,
                        &hbcofa_missing,
                        &hbcofa_has_missing,
                        &hbcofa_type,
                        0);
        hbcofb = (float*)NclGetArgValue(
                        2,
                        9,
                        &hbcofb_n_dims,
                        &hbcofb_dimsizes,
                        &hbcofb_missing,
                        &hbcofb_has_missing,
                        &hbcofb_type,
                        0);
        plevo_val = _NclGetArg(3,9,DONT_CARE);
	switch(plevo_val.kind) {
	case NclStk_VAL:
		plevo_n_dims = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.n_dims;
		plevo_dimsizes = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.dim_sizes[0];
		plevo_missing = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.missing_value.value;
		plevo_has_missing = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.missing_value.has_missing;
		plevo_type = ((NclMultiDValData)(plevo_val.u.data_obj))->multidval.data_type;
		plevo_quark = NrmStringToQuark("lev");
		break;
	case NclStk_VAR:
		plevo_n_dims = ((NclVarRec*)(plevo_val.u.data_var))->var.n_dims;
		plevo_dimsizes = ((NclVarRec*)(plevo_val.u.data_var))->var.dim_info[0].dim_size;
                tmp_md = _NclVarValueRead(val.u.data_var,NULL,NULL);
		plevo_missing = tmp_md->multidval.missing_value.value;
		plevo_has_missing = tmp_md->multidval.missing_value.has_missing;
		plevo_type = tmp_md->multidval.data_type;
		plevo_quark = ((NclVarRec*)(plevo_val.u.data_var))->var.dim_info[0].dim_quark;
		if(plevo_quark == -1) 
			plevo_quark = NrmStringToQuark("lev");
		break;
	}

	
/*
	plevo = (float*)NclGetArgValue(
                        3,
                        9,
                        &plevo_n_dims,
                        &plevo_dimsizes,
                        &plevo_missing,
                        &plevo_has_missing,
                        &plevo_type,
                        0);
*/
	plevo2 = (float*)NclMalloc(sizeof(float)*plevo_dimsizes);
	memcpy(plevo2,plevo,sizeof(float)*plevo_dimsizes);
        psfc = (float*)NclGetArgValue(
                        4,
                        9,
                        &psfc_n_dims,
                        psfc_dimsizes,
                        &psfc_missing,
                        &psfc_has_missing,
                        &psfc_type,
                        0);
        intyp = (int*)NclGetArgValue(
                        5,
                        9,
                        NULL,
                        NULL,
                        &intyp_missing,
                        &intyp_has_missing,
                        NULL,
                        0);
        p0 = (float*)NclGetArgValue(
                        6,
                        9,
                        NULL,
                        NULL,
                        &p0_missing,
                        &p0_has_missing,
                        NULL,
                        0);
        ilev = (int*)NclGetArgValue(
                        7,
                        9,
                        NULL,
                        NULL,
                        &ilev_missing,
                        &ilev_has_missing,
                        NULL,
                        0);
        kxtrp = (logical*)NclGetArgValue(
                        8,
                        9,
                        NULL,
                        NULL,
                        &kxtrp_missing,
                        &kxtrp_has_missing,
                        NULL,
                        0);


	datao = (float*)NclMalloc(plevo_dimsizes*datai_dimsizes[1]*datai_dimsizes[2] * sizeof(float));
	plevi = (float*)NclMalloc((datai_dimsizes[0]+1)*sizeof(float));
	if(datai_has_missing) {
		missing = datai_missing;
	} else {
		missing =((NclTypeClass) nclTypefloatClass)->type_class.default_mis;
	}
	
	NGCALLF(vinth2p,VINTH2P)(datai,datao,hbcofa,hbcofb,p0,plevi,plevo,intyp,ilev,psfc,&missing,kxtrp,&(datai_dimsizes[2]),&(datai_dimsizes[1]),&(datai_dimsizes[0]),&(datai_dimsizes[0]),&(plevo_dimsizes));

	NclFree(plevi);
	if(was_val) {
		dim_info[0].dim_quark =plevo_quark; 
		dim_info[0].dim_num= 0 ; 
		dim_info[0].dim_size = plevo_dimsizes ; 
		dim_info[1].dim_quark = NrmStringToQuark("lat");
		dim_info[1].dim_size = datai_dimsizes[1] ; 
		dim_info[1].dim_num= 1 ; 
		dim_info[2].dim_quark = NrmStringToQuark("lon");
		dim_info[2].dim_size = datai_dimsizes[2] ;
		dim_info[2].dim_num= 2 ; 
		lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)nclTypefloatClass);
		lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,dim_info,-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
		ids[0] = lev_coord_var->obj.id;
		ids[1] = -1;
		ids[2] = -1;
		datao_dimsizes[0] = plevo_dimsizes;
		datao_dimsizes[1] = datai_dimsizes[1];
		datao_dimsizes[2] = datai_dimsizes[2];
		tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&missing,3,datao_dimsizes,TEMPORARY,NULL,(NclObjClass)nclTypefloatClass);
		data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);
		data.kind = NclStk_VAR;
		_NclPlaceReturn(data);
	} else {
		dim_info[0].dim_quark = plevo_quark;;
		dim_info[0].dim_num= 0 ; 
		dim_info[0].dim_size = plevo_dimsizes ; 
		dim_info[1].dim_quark = val.u.data_var->var.dim_info[1].dim_quark;
		dim_info[1].dim_size = val.u.data_var->var.dim_info[1].dim_size; 
		dim_info[1].dim_num= 1 ; 
		dim_info[2].dim_quark = val.u.data_var->var.dim_info[2].dim_quark;
		dim_info[2].dim_size = val.u.data_var->var.dim_info[2].dim_size; 
		dim_info[2].dim_num= 2 ; 
		lev_coord_md = _NclCreateVal(NULL,NULL,Ncl_OneDValCoordData,0,plevo2,NULL,1,&(plevo_dimsizes),TEMPORARY,NULL,(NclObjClass)nclTypefloatClass);
		lev_coord_var = (NclVar)_NclCoordVarCreate(NULL,NULL,Ncl_CoordVar,0,NULL,lev_coord_md,dim_info,-1,NULL,COORD,NrmQuarkToString(plevo_quark),TEMPORARY);
		ids[0] = lev_coord_var->obj.id;
		ids[1] = -1;
		ids[2] = -1;
		datao_dimsizes[0] = plevo_dimsizes;
		datao_dimsizes[1] = datai_dimsizes[1];
		datao_dimsizes[2] = datai_dimsizes[2];
		tmp_md = _NclCreateVal(NULL,NULL,Ncl_MultiDValData,0,datao,&missing,3,datao_dimsizes,TEMPORARY,NULL,(NclObjClass)nclTypefloatClass);
		data.u.data_var = _NclVarCreate(NULL,NULL,Ncl_Var,0,NULL,tmp_md,dim_info,-1,ids,RETURNVAR,NULL,TEMPORARY);

		if((val.u.data_var->var.dim_info[1].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark)))) {
			tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[1].dim_quark),NULL);
			_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[1].dim_quark),NULL);
		}

		if((val.u.data_var->var.dim_info[2].dim_quark != -1)&&(_NclIsCoord(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark)))) {
			tmp_var = _NclReadCoordVar(val.u.data_var,NrmQuarkToString(val.u.data_var->var.dim_info[2].dim_quark),NULL);
			_NclWriteCoordVar(data.u.data_var,_NclVarValueRead(tmp_var,NULL,NULL),NrmQuarkToString(data.u.data_var->var.dim_info[2].dim_quark),NULL);
		}
		data.kind = NclStk_VAR;
		_NclPlaceReturn(data);
	}
	return(NhlNOERROR);
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
	NclApiDataList *data;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;



        val = _NclGetArg(0,1,DONT_CARE);
        switch(val.kind) {
		case NclStk_VAR:
                	tmp_var = val.u.data_var;
			if(tmp_var->var.var_quark > 0) {
				name = tmp_var->var.var_quark;
			} else {
				dimsizes = 1;
				return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
			}
			break;
        	case NclStk_VAL:
		default:
			dimsizes = 1;
			return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
	}

	if((tmp_var->obj.obj_type == Ncl_Var)||(tmp_var->obj.obj_type == Ncl_HLUVar)){
		data = _NclGetVarInfo(name);
		if((data != NULL)&&(data->u.var->n_atts != 0)) {
			ret = NclReturnValue((void*)data->u.var->attnames, 1, &data->u.var->n_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
			_NclFreeApiDataList((void*)data);
			return(ret);
		}
	} else if(tmp_var->obj.obj_type == Ncl_FileVar) {
		data = _NclGetFileInfo(name);
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
	NclApiDataList *data;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	int dim_sizes[NCL_MAX_DIMENSIONS];
	int i;



        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
		case NclStk_VAR:
                	tmp_var = val.u.data_var;
			if(tmp_var->var.var_quark > 0) {
				fname = tmp_var->var.var_quark;
			} else {
				dimsizes = 1;
				return(NclReturnValue((void*)&((NclTypeClass)nclTypeintClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypeintClass)->type_class.default_mis, ((NclTypeClass)nclTypeintClass)->type_class.data_type, 1));
			}
			break;
        	case NclStk_VAL:
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
	data = _NclGetFileVarInfo(fname,*name);
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
	NclApiDataList *data;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;
	NclQuark dim_names[NCL_MAX_DIMENSIONS];
	int i;



        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
		case NclStk_VAR:
                	tmp_var = val.u.data_var;
			if(tmp_var->var.var_quark > 0) {
				fname = tmp_var->var.var_quark;
			} else {
				dimsizes = 1;
				return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
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
	data = _NclGetFileVarInfo(fname,*name);
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
	NclApiDataList *data;
	NhlErrorTypes ret;
	NclStackEntry val;
	NclVar tmp_var;



        val = _NclGetArg(0,2,DONT_CARE);
        switch(val.kind) {
		case NclStk_VAR:
                	tmp_var = val.u.data_var;
			if(tmp_var->var.var_quark > 0) {
				fname = tmp_var->var.var_quark;
			} else {
				dimsizes = 1;
				return(NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1));
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
	data = _NclGetFileVarInfo(fname,*name);
	if((data != NULL)&&(data->u.var->n_atts != 0)) {
		ret = NclReturnValue((void*)data->u.var->attnames, 1, &data->u.var->n_atts, NULL, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
		_NclFreeApiDataList((void*)data);
		return(ret);
	} else {
		dimsizes = 1;
		ret = NclReturnValue((void*)&((NclTypeClass)nclTypestringClass)->type_class.default_mis, 1, &dimsizes, &((NclTypeClass)nclTypestringClass)->type_class.default_mis, ((NclTypeClass)nclTypestringClass)->type_class.data_type, 1);
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
			tmp = _NclGetFileInfo(data.u.data_var->var.var_quark);
			file_md= (NclMultiDValData)_NclVarValueRead(data.u.data_var,NULL,NULL);
			tmp_file = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
			if( tmp->u.file->n_atts > 0 ) {
				for(j = 0; j < tmp->u.file->n_atts; j++) {
					ret=_NclFileWriteAtt(thefile,tmp->u.file->attnames[j],_NclFileReadAtt(tmp_file,tmp->u.file->attnames[j],NULL),NULL);
					if(ret < NhlINFO) {
						ret0 = ret;
					}
				}
			} else {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAttDef: No attributes to assign");
                                return(NhlWARNING);
                        }
			_NclFreeApiDataList((void*)tmp);

			break;
		default:
			if(data.u.data_var->var.att_id != -1) {
				tmp_attobj = (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAttDef: No attributes to assign");
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
			tmp = _NclGetFileInfo(data.u.data_var->var.var_quark);
			file_md= (NclMultiDValData)_NclVarValueRead(data.u.data_var,NULL,NULL);
			tmp_file = (NclFile)_NclGetObj(*(obj*)file_md->multidval.val);
			if( tmp->u.file->n_atts > 0 ) {
				for(i = 0; i < dimsize; i++) {
					for(j = 0; j < tmp->u.file->n_atts; j++) {
						ret=_NclFileWriteVarAtt(thefile,varnames[i],tmp->u.file->attnames[j],_NclFileReadAtt(tmp_file,tmp->u.file->attnames[j],NULL),NULL);
					}
				}
			} else {
                                NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAttDef: No attributes to assign");
                                return(NhlWARNING);
                        }
			_NclFreeApiDataList((void*)tmp);
			break;
		default:
			if(data.u.data_var->var.att_id != -1) {
				tmp_attobj = (NclAtt)_NclGetObj(data.u.data_var->var.att_id);
			} else {
				NhlPError(NhlWARNING,NhlEUNKNOWN,"FileAttDef: No attributes to assign");
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

NhlErrorTypes sprintf_W( void )
{
/*
 * Input array variables
 */
  float *input_var;
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
           2);

  input_var = (float*)NclGetArgValue(
           1,
           2,
           &ndims_input_var, 
           dsizes_input_var,
	   &missing_input_var,
	   &has_missing_input_var,
           NULL,
           2);
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
		NhlPError(NhlFATAL,NhlEUNKNOWN,"attsetvalues: Parameter 1 must be a variable,can't continue");
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
        for(; i > 0; i--) {
                if(gen_array[i-1])
                        NhlFreeGenArray(gen_array[i-1]);
        }

	NhlFree(gen_array);
	NhlRLDestroy(rl_list);
	return(NhlNOERROR);
}
#ifdef __cplusplus
}
#endif

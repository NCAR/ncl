
/*
 *      $Id: BuiltInFuncs.c,v 1.44 1996-10-02 22:33:54 ethan Exp $
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
		ret = nclfprintf(fp,"\n%s\t%s ",_NclBasicDataTypeToName(step->u.var->data_type),NrmQuarkToString(step->u.var->name));
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
			var_names[i] = thefile->file.var_info[i]->var_name_quark;
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
		ret = nclfprintf(fp,"\n%s\t%s ",_NclBasicDataTypeToName(step->u.var->data_type),NrmQuarkToString(step->u.var->name));
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
			if((tmp_vars[4] != NULL)&&(_NclVarIsAtt(tmp_vars[4],NCL_MISSING_VALUE_ATT))) {
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
		return(NhlEUNKNOWN);	
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
			if((tmp_vars[4] != NULL)&&(_NclVarIsAtt(tmp_vars[4],NCL_MISSING_VALUE_ATT))) {
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
		return(NhlEUNKNOWN);	
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
	Const char* command;

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
		command = NrmQuarkToString(*(NclQuark*)tmp_md->multidval.val);
		if(!system(command)) {
			return(NhlNOERROR);
		} else {
                	return(NhlFATAL);
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
		lval = (logical*)NclMalloc((unsigned)sizeof(logical));
		if(tmp_md->multidval.missing_value.has_missing) {
			*lval = _NclIsMissing(tmp_md,tmp_md->multidval.val);
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)lval,NULL,1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data);
		} else {
			*lval = 0;
			data.kind = NclStk_VAL;
			data.u.data_obj = _NclCreateMultiDVal(NULL,NULL,Ncl_MultiDValData,0,(void*)lval,NULL,1,&dimsize,TEMPORARY,NULL,(NclTypeClass)nclTypelogicalClass);
			_NclPlaceReturn(data);
		}
	}
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
	NhlAddOverlay(base_hl->hlu.hlu_id,over_hl->hlu.hlu_id,-1);
	_NclAddHLUToExpList(base_hl,over_hl->obj.id);
	return(NhlNOERROR);
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
	NGCALLF(ncl_fortranread,NCL_FORTRANREAD)(path_string,tmp_ptr,&totalsize,&ret,strlen(path_string));
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
	fd = open(path_string,(O_CREAT | O_RDWR),0777);
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
	NGCALLF(ncl_fortranwrite,NCL_FORTRANWRITE)(path_string,tmp_ptr,&totalsize,&ret,strlen(path_string));
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
			outval = ((NclTypeClass)nclTypestringClass)->type_class.default_mis.stringval;
		} else {
			outval = NrmStringToQuark(getenv(str));
		}
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
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.charval)) {
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
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.charval)) {
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
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.charval)) {
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
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.charval)) {
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
			if((value[i] < 0)||(value[i] > 255)||(value[i] == missing.charval)) {
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
#ifdef __cplusplus
}
#endif
